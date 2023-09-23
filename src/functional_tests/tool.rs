use std::sync::Mutex;

use v8;

use crate::{compress::compress, from_ast::module_to_basic_blocks, swc_parse::swc_parse};

static INITIALIZED: Mutex<bool> = Mutex::new(false);

pub fn run_checks(s: &str) -> String {
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

    let isolate = &mut v8::Isolate::new(Default::default());
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

        println!("instructions: {:?}", module);
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
