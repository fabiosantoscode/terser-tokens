use swc_ecma_ast::{BinExpr, BinaryOp, Expr};

use crate::basic_blocks::Instruction;

// from: https://github.com/terser/terser/blob/master/lib/parse.js#L1028
pub fn get_precedence(op: &BinaryOp) -> u32 {
    match op {
        BinaryOp::LogicalOr => 0,
        BinaryOp::NullishCoalescing => 1,
        BinaryOp::LogicalAnd => 2,
        BinaryOp::BitOr => 3,
        BinaryOp::BitXor => 4,
        BinaryOp::BitAnd => 5,
        BinaryOp::EqEq | BinaryOp::EqEqEq | BinaryOp::NotEq | BinaryOp::NotEqEq => 6,
        BinaryOp::Lt
        | BinaryOp::LtEq
        | BinaryOp::Gt
        | BinaryOp::GtEq
        | BinaryOp::In
        | BinaryOp::InstanceOf => 7,
        BinaryOp::LShift | BinaryOp::RShift | BinaryOp::ZeroFillRShift => 8,
        BinaryOp::Add | BinaryOp::Sub => 9,
        BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 10,
        BinaryOp::Exp => 11,
    }
}

// From: https://github.com/terser/terser/blob/master/lib/output.js#L1086-L1092
pub fn should_add_parens(parent_op: &BinaryOp, child: &Expr, is_right: bool) -> bool {
    let pp = get_precedence(parent_op);
    let sp = match child {
        Expr::Bin(BinExpr { op, .. }) => get_precedence(op),
        _ => return false,
    };

    return pp > sp || (pp == sp && (is_right || parent_op == &BinaryOp::Exp));
}
