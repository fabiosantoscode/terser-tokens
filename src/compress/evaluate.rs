use crate::{
    basic_blocks::{LogicalCondKind, StructuredFlow, StructuredModule},
    block_ops::normalize_module,
    interpret::{interpret_module, InterpretCtx, JsType},
};

pub fn compress_step_evaluate(module: &mut StructuredModule) {
    let mut types = InterpretCtx::from_module(&module);
    interpret_module(&mut types, &module);

    let types = types.into_all_variables();

    module.for_each_flow_mut(
        |_func_id, block| match  block {
            StructuredFlow::Cond(_brk, test, cons, alt) => {
                let is_truthy = types.get(&test).and_then(JsType::is_truthy);

                // statically-analyzable conditions
                match is_truthy {
                    Some(true) => *block = StructuredFlow::from_vec(std::mem::take(cons)),
                    Some(false) => *block = StructuredFlow::from_vec(std::mem::take(alt)),
                    None => {},
                };
            }
            StructuredFlow::LogicalCond(kind, left, cond_on, _right, _then_take) => {
                let eval_left = types.get(&cond_on);

                match kind {
                    LogicalCondKind::And | LogicalCondKind::Or => {
                        let is_truthy = eval_left.and_then(JsType::is_truthy);

                        match (kind, is_truthy) {
                            (LogicalCondKind::And, Some(false)) => *block = StructuredFlow::from_vec(std::mem::take(left)),
                            (LogicalCondKind::Or, Some(true)) => *block = StructuredFlow::from_vec(std::mem::take(left)),
                            _ => {},
                        };
                    },
                    LogicalCondKind::NullishCoalescing => {
                        let is_nullish = eval_left.and_then(JsType::is_nullish);

                        match is_nullish {
                            Some(false) => *block = StructuredFlow::from_vec(std::mem::take(left)),
                            _ => {},
                        };
                    }
                };
            }
            StructuredFlow::Instruction(varname, ins) => {
                match types.get(&varname) {
                    Some(value) => {
                        if let Some(new_ins) = value.as_small_literal_instruction(/* TODO calculate and pass desired max size here */)
                        {
                            *ins = new_ins;
                        }
                    }
                    None => {}
                }
            }
            _ => {}
        },
    );

    normalize_module(module);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::basic_blocks::FunctionId;
    use crate::testutils::*;

    #[test]
    fn test_evaluate_block() {
        let mut module = parse_test_module(vec![
            "{
                $0 = 1
                $1 = 2
                $2 = $0 + $1
                Return $2
            }",
        ])
        .into();

        compress_step_evaluate(&mut module);

        insta::assert_debug_snapshot!(module.get_function(FunctionId(0)).unwrap().blocks[0], @r###"
        {
            $0 = 1
            $1 = 2
            $2 = 3
            Return $2
        }
        "###);
    }

    #[test]
    fn test_evaluate_cond() {
        let mut module = parse_test_module(vec![
            "{
                $0 = 1
                if ($0) {
                    $1 = 1
                    Return $1
                } else {
                    $2 = 2
                    Return $2
                }
            }
            ",
        ])
        .into();

        compress_step_evaluate(&mut module);

        insta::assert_debug_snapshot!(module.get_function(FunctionId(0)).unwrap(), @r###"
        {
            $0 = 1
            $1 = 1
            Return $1
        }
        "###);
    }

    #[test]
    fn test_evaluate_cond_nested() {
        let mut module = parse_test_module(vec![
            "{
                $0 = 1
                $1 = $0
                $2 = 1
                $3 = $1 == $2
                if ($3) {
                    $4 = $0
                    $5 = 1
                    $6 = $4 == $5
                    if ($6) {
                        $7 = $0
                        $8 = 2000
                        $9 = $7 + $8
                        $10 = $9
                    } else {
                        $11 = 3
                        $12 = $11
                    }
                    $13 = either($0, $9, $11)
                    $14 = $13
                    $15 = 1000
                    $16 = $14 + $15
                    $17 = $16
                } else {
                    $18 = 3
                    $19 = $18
                }
                $20 = either($13, $16, $18)
                $21 = $20
                Return $21
            }",
        ]);

        compress_step_evaluate(&mut module);

        insta::assert_debug_snapshot!(module.get_function(FunctionId(0)).unwrap(), @r###"
        {
            $0 = 1
            $1 = 1
            $2 = 1
            $3 = true
            $4 = 1
            $5 = 1
            $6 = true
            $7 = 1
            $8 = 2000
            $9 = 2001
            $10 = 2001
            $14 = $9
            $15 = 1000
            $11 = $14 + $15
            $17 = $11
            $21 = $11
            Return $21
        }
        "###);
    }

    #[test]
    fn test_evaluate_cond_nested_2() {
        let mut module = parse_test_module(vec![
            "{
                $0 = 1
                $1 = $0
                $2 = 1
                $3 = $1 == $2
                if ($3) {
                    $4 = $0
                    $5 = true
                    $6 = $4 == $5
                    if ($6) {
                        $7 = $0
                        $8 = 2000
                        $9 = $7 + $8
                        $10 = $9
                    } else {
                        $11 = 3
                        $12 = $11
                    }
                    $13 = either($0, $9, $11)
                    $14 = $13
                    $15 = 1000
                    $16 = $14 + $15
                    $17 = $16
                } else {
                    $18 = 3
                    $19 = $18
                }
                $20 = either($13, $16, $18)
                $21 = $20
                Return $21
            }",
        ]);

        compress_step_evaluate(&mut module);

        insta::assert_debug_snapshot!(module.get_function(FunctionId(0)).unwrap(), @r###"
        {
            $0 = 1
            $1 = 1
            $2 = 1
            $3 = true
            $4 = 1
            $5 = true
            $6 = $4 == $5
            if ($6) {
                $7 = $0
                $8 = 2000
                $9 = $7 + $8
                $10 = $9
            } else {
                $11 = 3
                $12 = $11
            }
            $13 = either($0, $9, $11)
            $14 = $13
            $15 = 1000
            $16 = $14 + $15
            $17 = $16
            $21 = $16
            Return $21
        }
        "###);
    }

    #[test]
    fn evaluate_nonlocal_1() {
        let mut module = parse_test_module(vec![
            "{
                $0 = undefined
                $3 = FunctionId(1)
                $4 = 100
                $5 = write_non_local $$1 $4
                $6 = $3
                $7 = call $6()
                Return $7
            }",
            "{
                $8 = read_non_local $$1
                $9 = 1
                $10 = $8 + $9
                $11 = $10
                Return $11
            }",
        ])
        .into();

        compress_step_evaluate(&mut module);

        insta::assert_debug_snapshot!(module, @r###"
        StructuredModule {
            summary: ModuleSummary {
                filename: "",
            },
            top_level_stats: {
                $0 = undefined
                $3 = FunctionId(1)
                $4 = 100
                $5 = 100
                $6 = $3
                $7 = 101
                Return $7
            }
            ,
            functions: [
                function():
                {
                    $8 = 100
                    $9 = 1
                    $10 = 101
                    $11 = 101
                    Return $11
                }
                ,
            ],
        }
        "###);
    }

    /*
    #[test]
    fn evaluate_nonlocal_2() {
        // TODO: our code generates a duplicate write_non_local $$1 so we can't easily statically evaluate this
        let mut module = parse_structured_module(vec![
            "{
                $0 = undefined
                $2 = write_non_local $$1 $0
                $3 = FunctionId(1)
                $4 = 100
                $5 = write_non_local $$1 $4
                $6 = $3
                $7 = call $6()
                Return $7
            }",
            "{
                $8 = read_non_local $$1
                $9 = 1
                $10 = $8 + $9
                $11 = $10
                Return $11
            }",
        ]);

        compress_step_evaluate(&mut module);

        insta::assert_debug_snapshot!(module, @r###"
        BasicBlockModule {
            summary: ModuleSummary {
                filename: "",
            },
            top_level_stats: @0: {
                $0 = undefined
                $3 = FunctionId(1)
                $4 = 100
                $5 = write_non_local $$1 $4
                $6 = $3
                $7 = call $6()
                Return $7
            },
            functions: [
                function():
                @0: {
                    $8 = read_non_local $$1
                    $9 = 1
                    $10 = $8 + $9
                    $11 = $10
                    Return $11
                },
            ],
        }
        "###);
    }
    */
}
