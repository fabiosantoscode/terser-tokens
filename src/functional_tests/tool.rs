use std::sync::Mutex;

use v8::{self, OwnedIsolate};

use crate::{compress::compress, from_ast::module_to_basic_blocks, swc_parse::swc_parse};

static INITIALIZED: Mutex<bool> = Mutex::new(false);

pub fn run_checks(s: &str) -> String {
    run_in_v8_with_timeout(|isolate| run_checks_inner(isolate, s))
}

pub fn run_checks_inner(isolate: &mut v8::Isolate, s: &str) -> String {
    println!("FUNCTIONAL TEST\n// reference:\n        {s}");

    let scope = &mut v8::HandleScope::new(isolate);

    let Some(reference) = run_code_for_logs(scope, s) else {
        panic!("ERROR: failed to run reference code {s}")
    };
    println!("// output: {reference}");

    // Interim log
    {
        let module = swc_parse(s);

        let module = module_to_basic_blocks("input.js", &module).unwrap();

        println!("/* parsed: {:?} */", module);
    }

    let comp_s = compress(s);
    println!("// compressed code:\n{}", comp_s);
    let Some(compressed) = run_code_for_logs(scope, &comp_s) else {
        panic!("ERROR: failed to run compressed code {comp_s}")
    };

    println!("// output: {compressed}");

    if reference != compressed {
        panic!(
            "ERROR: reference and compressed code differ:
            compressed output: {compressed}
            reference output: {reference}"
        );
    }

    reference // return reference for snapshot
}

fn run_code_for_logs(scope: &mut v8::HandleScope<'_, ()>, s: &str) -> Option<String> {
    let ctx = v8::Context::new(scope);
    let ctx_scope = &mut v8::ContextScope::new(scope, ctx);

    {
        let code = format!(
            r###"
                let __str = s =>
                    Array.isArray(s) ? '[' + s.map(__str).join(', ') + ']' :
                    s == null ? String(s) :
                    JSON.stringify(s);

                ;(async function () {{
                    {s}
                }})()
                    .then(result => {{
                        globalThis.RESULT = __str(result);
                    }})
                    .catch(error => {{
                        globalThis.RESULT = error ? error.message : __str(error);
                    }});
            "###
        );

        let code = v8::String::new(ctx_scope, &code)?;
        let script = v8::Script::compile(ctx_scope, code, None)?;
        script.run(ctx_scope)?;

        ctx_scope.perform_microtask_checkpoint();
    };

    // Read the results
    {
        let code = v8::String::new(ctx_scope, "globalThis.RESULT")?;
        let script = v8::Script::compile(ctx_scope, code, None)?;
        let result = script.run(ctx_scope)?;

        Some(result.to_string(ctx_scope)?.to_rust_string_lossy(ctx_scope))
    }
}

fn create_v8_isolate() -> v8::OwnedIsolate {
    {
        let mut initialized = INITIALIZED.lock().unwrap();
        if !*initialized {
            // initialize
            let platform = v8::new_default_platform(0, false).make_shared();
            v8::V8::initialize_platform(platform);
            v8::V8::initialize();
            *initialized = true;
        }
    }

    v8::Isolate::new(Default::default())
}

pub fn run_in_v8_with_timeout<Func>(func: Func) -> String
where
    Func: FnOnce(&mut OwnedIsolate) -> String + Send,
{
    use std::time::Duration;

    std::thread::scope(|scope| {
        let timeout = Duration::from_secs(10);

        let (handle_sender, handle_receiver) = std::sync::mpsc::channel();
        let (ret_sender, ret_receiver) = std::sync::mpsc::channel();

        scope.spawn(move || {
            let mut isolate = create_v8_isolate();

            handle_sender.send(isolate.thread_safe_handle()).unwrap();

            let result = func(&mut isolate);

            ret_sender.send(result).unwrap();
        });

        let handle = handle_receiver.recv_timeout(timeout).unwrap();

        match ret_receiver.recv_timeout(timeout) {
            Ok(result) => result,
            Err(timeout_error) => {
                handle.terminate_execution();
                panic!("timeout error {timeout_error:?}");
            }
        }
    })
}
