use std::sync::Mutex;

use v8::{self, OwnedIsolate};

use crate::{compress::compress, from_ast::module_to_basic_blocks, swc_parse::swc_parse};

static INITIALIZED: Mutex<bool> = Mutex::new(false);

pub fn run_checks(s: &str) -> String {
    run_in_v8_with_timeout(|isolate| run_checks_inner(isolate, s))
}

pub fn run_checks_inner(isolate: &mut v8::Isolate, s: &str) -> String {
    let scope = &mut v8::HandleScope::new(isolate);
    let context = v8::Context::new(scope);
    let scope = &mut v8::ContextScope::new(scope, context);

    let Some(reference) = run_code_for_logs(scope, s) else {
        panic!("failed to run reference code {s}")
    };

    // Interim log
    {
        let module = swc_parse(s);

        let module = module_to_basic_blocks("input.js", &module).unwrap();

        println!("instructions:\n{:?}", module);
    }

    let comp_s = compress(s);
    let Some(compressed) = run_code_for_logs(scope, &comp_s) else {
        panic!("failed to run compressed code {comp_s}")
    };

    if reference != compressed {
        println!(
            "reference and compressed code should have the same output\n\
            code: \n{s}// outputs: {reference}\n\
            compressed code:\n{comp_s}// outputs: {compressed}\n\
            "
        );
        assert_eq!(reference, compressed);
    }

    reference // return reference for snapshot
}

fn run_code_for_logs(
    scope: &mut v8::ContextScope<'_, v8::HandleScope<'_>>,
    s: &str,
) -> Option<String> {
    let code = format!(
        r###"
        {{
            let __str = s =>
                Array.isArray(s) ? '[' + s.map(__str).join(', ') + ']' :
                typeof s === 'object' && s ? JSON.stringify(s) :
                String(s)

            __str((function () {{
                {s}
            }}()))
        }}
    "###
    );
    let code = v8::String::new(scope, &code)?;
    let script = v8::Script::compile(scope, code, None)?;

    println!("running code: {}", s);
    let result = script.run(scope)?;

    Some(result.to_string(scope)?.to_rust_string_lossy(scope))
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
        let timeout = Duration::from_secs(5);

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
            Err(_) => {
                handle.terminate_execution();
                panic!("timeout");
            }
        }
    })
}
