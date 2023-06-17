
pub struct AST {
    pub node: ASTNode,
    pub children: Vec<AST>,
    pub start: usize,
    pub end: usize,
}

pub enum ASTNode {
    Declaration(Symbol)
}