use super::dominator_tree::Graph;
use super::ssa_ast::{ExitType, SsaAstNode, SsaExit};
use super::ssa_fn::SsaFn;
use domtree::frontier::DominanceFrontier;
use domtree::DomTree;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use swc_ecma_ast::{Expr, Ident, ReturnStmt, Stmt};

impl SsaFn {
    pub fn to_js_ast(&self) -> () {
        todo!()
    }
}

#[derive( Default, Clone)]
enum StructuredFlow {
    #[default]
    Nop,
    Straight(Vec<StructuredFlow>),
    Branch(
        Vec<StructuredFlow>,
        Vec<StructuredFlow>,
        Vec<StructuredFlow>,
    ),
    Break(usize),
    Loop(Vec<StructuredFlow>),
    Block(Vec<StructuredFlow>),
    Return(ExitType),
    SsaBlock(usize),
    SsaRef(usize)
}

impl StructuredFlow {
    fn str_head(&self) -> String {
        match self {
            StructuredFlow::Nop => "Nop".to_string(),
            StructuredFlow::Straight(_) => "Straight".to_string(),
            StructuredFlow::Branch(_, _, _) => "Branch".to_string(),
            StructuredFlow::Break(_) => "Break".to_string(),
            StructuredFlow::Loop(_) => "Loop".to_string(),
            StructuredFlow::Block(_) => "Block".to_string(),
            StructuredFlow::Return(_) => "Return".to_string(),
            StructuredFlow::SsaBlock(_) => "SsaBlock".to_string(),
            StructuredFlow::SsaRef(_) => "SsaRef".to_string(),
        }
    }
    fn children(&self) -> Vec<Vec<StructuredFlow>> {
        match self {
            StructuredFlow::Nop => vec![],
            StructuredFlow::Straight(x) => vec![x.clone()],
            StructuredFlow::Branch(x, y, z) => vec![x.clone(), y.clone(), z.clone()],
            StructuredFlow::Break(_) => vec![],
            StructuredFlow::Loop(x) => vec![x.clone()],
            StructuredFlow::Block(x) => vec![x.clone()],
            StructuredFlow::Return(_) => vec![],
            StructuredFlow::SsaBlock(_) => vec![],
            StructuredFlow::SsaRef(_) => vec![],
        }
    }
    fn index(&self) -> Option<usize> {
        match self {
            StructuredFlow::Nop => None,
            StructuredFlow::Straight(_) => None,
            StructuredFlow::Branch(_, _, _) => None,
            StructuredFlow::Break(x) => Some(*x),
            StructuredFlow::Loop(_) => None,
            StructuredFlow::Block(_) => None,
            StructuredFlow::Return(_) => None,
            StructuredFlow::SsaBlock(x) => Some(*x),
            StructuredFlow::SsaRef(x) => Some(*x),
        }
    }
}

impl Debug for StructuredFlow {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                let indent_str_lines = |s: &str| {
                    let lines = s.lines();
                    let indented_lines = lines.map(|line| format!("    {}", line));
                    indented_lines.collect::<Vec<String>>().join("\n")
                };

        if let Some(index) = self.index() {
            write!(f, "{}({})", self.str_head(), index)
        } else {
            let children = self.children();
            if children.len() == 0 {
                write!(f, "{}", self.str_head())
            } else if children.len() == 1 {
                let children = format!("{:?}", children[0]);
                let is_multiline = children.contains("\n");
                if is_multiline {
                    let lines = indent_str_lines(&children);
                    return write!(f, "{}(\n{}\n)", self.str_head(), lines);
                }
                write!(f, "{}({})", self.str_head(), children)
            } else {
                let lines = children
                    .iter()
                    .map(|child| indent_str_lines(&format!("{:?}", child)))
                    .collect::<Vec<String>>()
                    .join("\n");

                write!(f, "{}(\n{}\n)", self.str_head(), lines)
            }
        }
    }
}

// type Context = [ContainingSyntax] -- innermost form first
// data ContainingSyntax
// = IfThenElse
// | LoopHeadedBy Label -- label marks loop header
// | BlockFollowedBy Label -- label marks code right after the block
#[derive(Debug, Default, Clone)]
struct Ctx {
    containing_syntax: Vec<ContainingSyntax>,
}

#[derive(Debug, Clone)]
enum ContainingSyntax {
    IfThenElse,
    LoopHeadedBy(usize),
    BlockFollowedBy(usize),
}

// https://dl.acm.org/doi/pdf/10.1145/3547621
//
fn do_tree(func: &SsaFn)  -> StructuredFlow{
    let dom = func.get_dom_graph();

    do_tree_inner(&dom, 0, Default::default())
}

fn do_tree_inner(graph: &Graph, node: usize, context: Ctx) -> StructuredFlow {
    let code_for_node = |context: Ctx| {
        node_within(
            graph,
            node,
            graph
                .direct_subs(node)
                .iter()
                .filter(|child| is_merge_node(graph, **child, 0))
                .collect(),
            context,
        )
    };

    if
    /* node is a loop header (TODO) */
    false {
        let mut inner_context = context.clone();
        inner_context
            .containing_syntax
            .push(ContainingSyntax::LoopHeadedBy(node));
        StructuredFlow::Loop(vec![code_for_node(inner_context)])
    } else {
        code_for_node(context)
    }
}

fn node_within(graph: &Graph, node: usize, ys: Vec<&usize>, context: Ctx) -> StructuredFlow {
    match ys.split_first() {
        None => {
            let mut block_contents: Vec<StructuredFlow> = tx_block(graph, node, context.clone());
            block_contents.push(match &graph.nodes[node].ssa.exit {
                SsaExit::Jump(to) => do_branch(graph, node, *to, context),
                SsaExit::Cond(e, t, f) => {
                    let inner_context = {
                        let mut inner_context = context.clone();
                        inner_context
                            .containing_syntax
                            .push(ContainingSyntax::IfThenElse);
                        inner_context
                    };
                    StructuredFlow::Branch(
                        tx_expr(graph, *e),
                        vec![do_branch(graph, node, *t, inner_context.clone())],
                        vec![do_branch(graph, node, *f, inner_context.clone())],
                    )
                }
                SsaExit::ExitFn(exit, ret) => StructuredFlow::Return(exit.clone()),
            });
            StructuredFlow::Block(block_contents)
        }
        Some((y, ys)) => {
            let inner = vec![
                node_within(graph, node, ys.iter().copied().collect(), {
                    let mut inner_context = context.clone();
                    inner_context
                        .containing_syntax
                        .push(ContainingSyntax::BlockFollowedBy(**y));
                    inner_context
                }),
                do_tree_inner(&graph, **y, context),
            ];
            StructuredFlow::Block(inner)
        }
    }
}

fn tx_block(graph: &Graph, node: usize, context: Ctx) -> Vec<StructuredFlow> {
    vec![StructuredFlow::SsaBlock(node)]
}

fn tx_expr(graph: &Graph, var_index: usize) -> Vec<StructuredFlow> {
    vec![StructuredFlow::SsaRef(var_index)]
}

fn do_branch(graph: &Graph, source: usize, target: usize, context: Ctx) -> StructuredFlow {
    let reverse_postorder = reverse_postorder(graph);
    let index_source = reverse_postorder.iter().position(|&x| x == source).unwrap();
    let index_target = reverse_postorder.iter().position(|&x| x == target).unwrap();

    if
    /* is backwards */
    index_target > index_source {
        todo!("backwards jumps {source} -> {target} ({index_source} -> {index_target}) result in a 'continue'")
    } else if
    /* multiple nodes come here */
    graph.nodes[target].incoming_edges.len() > 1 {
        StructuredFlow::Break(index_target) // TODO "WasmBr i -- exit"
    } else {
        do_tree_inner(graph, target, context)
    }
}

fn reverse_postorder(graph: &Graph) -> Vec<usize> {
    let mut result = Vec::new();

    let recurse = fix_fn::fix_fn!(|recurse, result: &mut Vec<usize>, node: usize| -> () {
        if result.contains(&node) {
            return;
        }
        result.push(node);

        for child in graph.direct_subs(node) {
            recurse(result, child);
        }
    });

    recurse(&mut result, 0);

    result.reverse();

    result
}

fn is_merge_node(graph: &Graph, child: usize, root: usize) -> bool {
    // TODO - is this a merge node?
    return false;
}

#[cfg(test)]
mod tests {
    use crate::ssa::{
        testutils::test_ssa_block,
        to_ast::{do_tree, reverse_postorder},
    };

    #[test]
    fn mk_stats() {
        let func = test_ssa_block("123; 123; 123 ? 456 : 789");
        insta::assert_debug_snapshot!(func, @r###"
        @0: {
            $0 = 123
            $1 = 123
            exit = jump @1
        }
        @1: {
            $2 = 123
            exit = cond $2 ? jump @2 : jump @3
        }
        @2: {
            $3 = 456
            exit = jump @4
        }
        @3: {
            $4 = 789
            exit = jump @4
        }
        @4: {
            $5 = either($3, $4)
            $6 = undefined
            exit = return $6
        }
        "###);

        let g = func.get_dom_graph();
        println!("{:?}", g);
        println!("{:?}", reverse_postorder(&g));

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block(
            [SsaBlock(0), Block(
                [SsaBlock(1), Branch(
                    [SsaRef(2)]
                    [Block([SsaBlock(2), Break(0)])]
                    [Block([SsaBlock(3), Break(0)])]
                )]
            )]
        )
        "###);
    }

    #[test]
    fn mk_stats_2() {
        let func = test_ssa_block("123 ? (456 ? 7 : 8) : 9");
        insta::assert_debug_snapshot!(func, @r###"
        @0: {
            $0 = 123
            exit = cond $0 ? jump @1 : jump @6
        }
        @1: {
            $1 = 456
            exit = cond $1 ? jump @2 : jump @3
        }
        @2: {
            $2 = 7
            exit = jump @4
        }
        @3: {
            $3 = 8
            exit = jump @4
        }
        @4: {
            exit = jump @5
        }
        @5: {
            $4 = either($2, $3)
            exit = jump @7
        }
        @6: {
            $5 = 9
            exit = jump @7
        }
        @7: {
            $6 = either($4, $5)
            $7 = undefined
            exit = return $7
        }
        "###);

        let g = func.get_dom_graph();
        println!("{:?}", g);
        println!("{:?}", reverse_postorder(&g));

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block(
            [SsaBlock(0), Branch(
                [SsaRef(0)]
                [Block(
                    [SsaBlock(1), Branch(
                        [SsaRef(1)]
                        [Block([SsaBlock(2), Break(3)])]
                        [Block([SsaBlock(3), Break(3)])]
                    )]
                )]
                [Block([SsaBlock(6), Break(0)])]
            )]
        )
        "###);
    }

    #[test]
    fn mk_stats_3() {
        let func = test_ssa_block("
            while (777) {
                if (888) {
                    break;
                }
            }
        ");
        insta::assert_debug_snapshot!(func, @r###"
        @0: {
            $0 = 777
            exit = jump @1
        }
        @1: {
            exit = cond $0 ? jump @2 : jump @9
        }
        @2: {
            $1 = 888
            exit = jump @3
        }
        @3: {
            exit = cond $1 ? jump @4 : jump @7
        }
        @4: {
            exit = jump @6
        }
        @5: {
            exit = jump @6
        }
        @6: {
            exit = jump @7
        }
        @7: {
            exit = jump @8
        }
        @8: {
            exit = jump @0
        }
        @9: {
            $2 = undefined
            exit = return $2
        }
        "###);

        let g = func.get_dom_graph();
        println!("{:?}", g);
        println!("{:?}", reverse_postorder(&g));

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block(
            [SsaBlock(0), Block(
                [SsaBlock(1), Branch(
                    [SsaRef(0)]
                    [Block(
                        [SsaBlock(2), Block(
                            [SsaBlock(3), Branch(
                                [SsaRef(1)]
                                [Block([SsaBlock(4), Break(3)])]
                                [Break(2)]
                            )]
                        )]
                    )]
                    [Block([SsaBlock(9), Return])]
                )]
            )]
        )
        "###);
    }
}
