use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

use super::{
    basic_block::{
        ArrayElement, BasicBlock, BasicBlockExit, BasicBlockInstruction, ExitType, TempExitType,
    },
    basic_block_group::BasicBlockGroup,
    basic_block_module::BasicBlockModule,
    to_basic_blocks::{
        convert_context::ConvertContext, module_to_basic_blocks, statements_to_basic_blocks,
    },
};
use crate::swc_parse::swc_parse;

use nom::IResult;
use swc_common::SourceMap;
use swc_common::{BytePos, SourceFile};
use swc_ecma_ast::{ExprStmt, Script, Stmt};
use swc_ecma_codegen::{text_writer::JsWriter, Emitter};
use swc_ecma_parser::{parse_file_as_expr, EsConfig};

pub fn parse_expression(source: &str) -> swc_ecma_ast::Expr {
    let file = SourceFile::new(
        swc_common::FileName::Anon,
        false,
        swc_common::FileName::Anon,
        source.into(),
        BytePos(1),
    );
    *parse_file_as_expr(
        &file,
        swc_ecma_parser::Syntax::Es(EsConfig {
            jsx: true,
            ..Default::default()
        }),
        Default::default(),
        None,
        &mut vec![],
    )
    .unwrap()
}

pub fn test_basic_blocks_expr(source: &str) -> BasicBlockGroup {
    let m = parse_expression(source);
    statements_to_basic_blocks(
        &mut ConvertContext::new(),
        &vec![&Stmt::Expr(ExprStmt {
            span: Default::default(),
            expr: Box::new(m),
        })],
    )
}

pub fn test_basic_blocks(source: &str) -> BasicBlockGroup {
    let m = test_basic_blocks_module(source);
    m.top_level_stats
}

pub fn test_basic_blocks_module(source: &str) -> BasicBlockModule {
    let m = swc_parse(source);
    module_to_basic_blocks("test.js", &m).unwrap()
}

pub fn stats_to_string(stats: Vec<Stmt>) -> String {
    struct Buf {
        pub buf: Rc<RefCell<String>>,
    }
    impl Write for Buf {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.buf
                .borrow_mut()
                .push_str(std::str::from_utf8(buf).unwrap());
            Ok(buf.len())
        }
        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }

    let script: Script = Script {
        span: Default::default(),
        body: stats,
        shebang: None,
    };
    let str = Rc::new(RefCell::new(String::new()));
    let buf = Buf { buf: str.clone() };
    let writer = Box::new(buf);
    let sourcemapper: Rc<SourceMap> = Default::default();
    let mut emitter = Emitter {
        cfg: Default::default(),
        comments: None,
        cm: sourcemapper,
        wr: JsWriter::new(Default::default(), "\n", writer, None),
    };

    emitter.emit_script(&script).unwrap();

    str.clone().borrow().clone()
}

macro_rules! whitespace {
    ($input:ident) => {{
        let (input, _) = nom::character::complete::multispace0($input)?;
        input
    }};
}

pub fn parse_basic_blocks(input: &str) -> BasicBlockGroup {
    let (_, bb) = parse_basic_blocks_inner(input).unwrap();
    bb
}

fn parse_basic_blocks_inner(input: &str) -> IResult<&str, BasicBlockGroup> {
    fn basic_block_instruction(input: &str) -> IResult<&str, (usize, BasicBlockInstruction)> {
        fn ins_litnumber(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // 10
            let (input, n) = nom::character::complete::digit1(input)?;
            let n_float: f64 = n.parse().unwrap();
            Ok((input, BasicBlockInstruction::LitNumber(n_float)))
        }

        fn ins_ref(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // $123
            let (input, _) = nom::bytes::complete::tag("@")(input)?;
            let (input, n) = nom::character::complete::digit1(input)?;
            let n_usize: usize = n.parse().unwrap();
            Ok((input, BasicBlockInstruction::Ref(n_usize)))
        }

        fn ins_binop(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // {ref} {operator} {ref}
            let (input, left) = parse_ref(input)?;
            let input = whitespace!(input);
            let (input, op) = nom::character::complete::one_of("+-*/")(input)?;
            let input = whitespace!(input);
            let (input, right) = parse_ref(input)?;
            Ok((
                input,
                BasicBlockInstruction::BinOp(String::from(op), left, right),
            ))
        }

        fn ins_undefined(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // undefined
            let (input, _) = nom::bytes::complete::tag("undefined")(input)?;
            Ok((input, BasicBlockInstruction::Undefined))
        }

        fn ins_this(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // this
            let (input, _) = nom::bytes::complete::tag("this")(input)?;
            Ok((input, BasicBlockInstruction::This))
        }

        fn ins_caught_error(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // caught_error
            let (input, _) = nom::bytes::complete::tag("caught_error")(input)?;
            Ok((input, BasicBlockInstruction::CaughtError))
        }

        fn ins_array(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // [ref, ref, ...]
            let (input, _) = nom::bytes::complete::tag("[")(input)?;
            let input = whitespace!(input);
            let (input, items) = nom::multi::separated_list0(
                nom::bytes::complete::tag(","),
                nom::sequence::preceded(nom::character::complete::multispace0, parse_ref),
            )(input)?;
            let input = whitespace!(input);
            let (input, _) = nom::bytes::complete::tag("]")(input)?;
            Ok((
                input,
                BasicBlockInstruction::Array(
                    items
                        .into_iter()
                        .map(|r| ArrayElement::Item(r))
                        .collect::<Vec<_>>(),
                ),
            ))
        }

        fn ins_tempexit(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // YieldStar {ref}
            // Yield {ref}
            // Await {ref}
            let (input, exit_type) = nom::branch::alt((
                nom::combinator::map(nom::bytes::complete::tag("YieldStar"), |_| {
                    TempExitType::YieldStar
                }),
                nom::combinator::map(nom::bytes::complete::tag("Yield"), |_| TempExitType::Yield),
                nom::combinator::map(nom::bytes::complete::tag("Await"), |_| TempExitType::Await),
            ))(input)?;

            let input = whitespace!(input);
            let (input, ref_) = ins_ref(input)?;

            Ok((
                input,
                BasicBlockInstruction::TempExit(exit_type, ref_.unwrap_ref()),
            ))
        }

        fn ins_phi(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // either({ref}, {ref}, {ref}, ...)
            let (input, _) = nom::bytes::complete::tag("either")(input)?;
            let (input, _paren) = nom::bytes::complete::tag("(")(input)?;
            let input = whitespace!(input);
            let (input, items) = nom::multi::separated_list0(
                nom::bytes::complete::tag(","),
                nom::sequence::preceded(nom::character::complete::multispace0, parse_ref),
            )(input)?;
            println!("here {input}");
            let (input, _paren) = nom::bytes::complete::tag(")")(input)?;

            Ok((input, BasicBlockInstruction::Phi(items)))
        }

        // $123 =
        let input = whitespace!(input);

        let (input, var_name) = parse_ref(input)?;
        let input = whitespace!(input);
        let (input, _) = nom::bytes::complete::tag("=")(input)?;
        let input = whitespace!(input);

        let (input, instruction) = nom::branch::alt((
            ins_undefined,
            ins_this,
            ins_litnumber,
            ins_ref,
            ins_binop,
            ins_caught_error,
            ins_array,
            ins_tempexit,
            ins_phi,
        ))(input)?;

        let input = whitespace!(input);

        Ok((input, (var_name, instruction)))
    }

    fn parse_basic_block_exit(input: &str) -> IResult<&str, BasicBlockExit> {
        // exit = jump @123

        let input = whitespace!(input);
        let (input, _exit) = nom::bytes::complete::tag("exit")(input)?;
        let input = whitespace!(input);
        let (input, _exit) = nom::bytes::complete::tag("=")(input)?;
        let input = whitespace!(input);

        let (input, word) = nom::branch::alt((
            nom::bytes::complete::tag("jump"),
            nom::bytes::complete::tag("cond"),
            nom::bytes::complete::tag("return"),
            nom::bytes::complete::tag("try"),
        ))(input)?;

        let input = whitespace!(input);

        let (input, ret) = match word {
            "return" => {
                let (input, ref_) = parse_ref(input)?;
                (input, BasicBlockExit::ExitFn(ExitType::Return, ref_))
            }
            "jump" => {
                let (input, ref_) = parse_blockref(input)?;
                (input, BasicBlockExit::Jump(ref_))
            }
            "cond" => {
                // cond {ref} ? jump @123 : jump @456
                let (input, condition) = parse_ref(input)?;
                let input = whitespace!(input);
                let (input, _) = nom::bytes::complete::tag("?")(input)?;
                let input = whitespace!(input);
                let (input, _) = nom::bytes::complete::tag("jump")(input)?;
                let input = whitespace!(input);
                let (input, consequent) = parse_blockref(input)?;
                let input = whitespace!(input);
                let (input, _) = nom::bytes::complete::tag(":")(input)?;
                let input = whitespace!(input);
                let (input, _) = nom::bytes::complete::tag("jump")(input)?;
                let input = whitespace!(input);
                let (input, alternate) = parse_blockref(input)?;
                (
                    input,
                    BasicBlockExit::Cond(condition, consequent, alternate),
                )
            }
            "try" => {
                // try @123 catch @456 finally @789 after @101112
                let (input, try_block) = parse_blockref(input)?;
                let input = whitespace!(input);
                let (input, _) = nom::bytes::complete::tag("catch")(input)?;
                let input = whitespace!(input);
                let (input, catch_block) = parse_blockref(input)?;
                let input = whitespace!(input);
                let (input, _) = nom::bytes::complete::tag("finally")(input)?;
                let input = whitespace!(input);
                let (input, finally_block) = parse_blockref(input)?;
                let input = whitespace!(input);
                let (input, _) = nom::bytes::complete::tag("after")(input)?;
                let input = whitespace!(input);
                let (input, after_block) = parse_blockref(input)?;
                (
                    input,
                    BasicBlockExit::SetTryAndCatch(
                        try_block,
                        catch_block,
                        finally_block,
                        after_block,
                    ),
                )
            }
            _ => unimplemented!(),
        };

        let input = whitespace!(input);

        Ok((input, ret))
    }

    fn basic_block_header(input: &str) -> IResult<&str, usize> {
        // @0: { ...instructions, exit = {basic block exit} }

        let input = whitespace!(input);
        let (input, _) = nom::bytes::complete::tag("@")(input)?;
        let (input, index) = nom::character::complete::digit1(input)?;
        let (input, _) = nom::bytes::complete::tag(":")(input)?;

        Ok((input, index.parse().unwrap()))
    }

    fn basic_block(input: &str) -> IResult<&str, BasicBlock> {
        let input = whitespace!(input);
        // whitespace
        let input = whitespace!(input);
        let (input, _) = nom::bytes::complete::tag("{")(input)?;
        // whitespace
        let input = whitespace!(input);

        let (input, instructions) = nom::multi::many0(basic_block_instruction)(input)?;

        let input = whitespace!(input);
        let (input, exit) = parse_basic_block_exit(input)?;

        let input = whitespace!(input);
        let (input, _) = nom::bytes::complete::tag("}")(input)?;

        let input = whitespace!(input);

        Ok((input, BasicBlock { instructions, exit }))
    }
    fn parse_ref(input: &str) -> IResult<&str, usize> {
        let (input, _) = nom::bytes::complete::tag("$")(input)?;
        let (input, n) = nom::character::complete::digit1(input)?;
        Ok((input, n.parse().unwrap()))
    }
    fn parse_blockref(input: &str) -> IResult<&str, usize> {
        let (input, _) = nom::bytes::complete::tag("@")(input)?;
        let (input, n) = nom::character::complete::digit1(input)?;
        Ok((input, n.parse().unwrap()))
    }

    let (input, blocks) = nom::multi::many0(nom::sequence::preceded(
        basic_block_header,
        nom::combinator::cut(basic_block),
    ))(input)?;

    let (input, _) = nom::combinator::eof(input)?;

    assert_eq!(input, "");

    Ok((input, BasicBlockGroup::from_asts(blocks)))
}

#[test]
fn test_parse_basic_blocks() {
    let blocks = parse_basic_blocks_inner(
        r###"
        @0: {
            $0 = 123
            exit = jump @1
        }
        "###,
    )
    .unwrap()
    .1;

    insta::assert_debug_snapshot!(blocks, @r###"
    @0: {
        $0 = 123
        exit = jump @1
    }
    "###);
}

#[test]
fn test_parse_basic_blocks_2() {
    let blocks = parse_basic_blocks_inner(
        r###"
    @0: {
        $0 = 777
        exit = jump @1
    }
    @1: {
        exit = cond $0 ? jump @2 : jump @7
    }
    @2: {
        $1 = 888
        exit = jump @3
    }
    @3: {
        exit = cond $1 ? jump @4 : jump @5
    }
    @4: {
        exit = jump @7
    }
    @5: {
        exit = jump @6
    }
    @6: {
        exit = jump @0
    }
    @7: {
        $2 = 999
        $3 = undefined
        exit = return $3
    }
    "###,
    )
    .unwrap()
    .1;

    insta::assert_debug_snapshot!(blocks, @r###"
    @0: {
        $0 = 777
        exit = jump @1
    }
    @1: {
        exit = cond $0 ? jump @2 : jump @7
    }
    @2: {
        $1 = 888
        exit = jump @3
    }
    @3: {
        exit = cond $1 ? jump @4 : jump @5
    }
    @4: {
        exit = jump @7
    }
    @5: {
        exit = jump @6
    }
    @6: {
        exit = jump @0
    }
    @7: {
        $2 = 999
        $3 = undefined
        exit = return $3
    }
    "###);
}
