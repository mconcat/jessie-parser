// TODO: the whole parser is missing recovery logic
// in case of errors. for any parser that contains a
// ? operator, we need to wrap it in a try block 

use std::ops::Index;

use crate::json::{self, data_literal};
use crate::rule::Parser;
use crate::{rule::{Rule, ParserState}, json::{DataStructure, data_structure, DataLiteral, repeated_elements}};

#[derive(Debug, PartialEq, Clone)]
pub struct JessieRule();

impl Rule for JessieRule {
    fn pure_json_rule() -> bool {
        false
    }

    fn jessie_rule() -> bool {
        true
    }

    fn tessie_rule() -> bool {
        false
    }

    fn ident(state: &mut ParserState) -> Result<String, String> {
        state.prevent(reserved_keyword)?;
        state.prevent(reserved_word)?;
        state.prevent(future_reserved_word)?;

        // copilot wrote, check and test later
        // seems like [a-zA-Z][a-zA-Z0-9]*
        let mut ident = String::new();

        match state.lookahead_1() {
            Some(x) if x.is_ascii_alphabetic() => {
                ident.push(x);
                state.proceed();
            }
            _ => return Err("Expected identifier".to_string()),
        }

        while let Some(x) = state.lookahead_1() {
            if x.is_ascii_alphanumeric() {
                ident.push(x);
                state.proceed();
            } else {
                break;
            }
        }
        Ok(ident)
    }

    type Expr = Expr<Self>;

    fn expr(state: &mut ParserState) -> Result<Self::Expr, String> {
       expr(state)
    } 

    type Element = Element<Self>;

    fn element(state: &mut ParserState) -> Result<Self::Element, String> {
        element(state)
    }

    type PurePropDef = PurePropDef<Self>;

    fn pure_prop_def(state: &mut ParserState) -> Result<Self::PurePropDef, String> {
        pure_prop_def(state)
    }

    type PropDef = PropDef<Self>;

    fn prop_def(state: &mut ParserState) -> Result<Self::PropDef, String> {
        prop_def(state)
    }

    type PropName = String;

    fn prop_name(state: &mut ParserState) -> Result<Self::PropName, String> {
        Self::ident(state) // TODO
    }

    type Variable = String;

    fn variable(state: &mut ParserState) -> Result<Self::Variable, String> {
        Self::ident(state) // TODO
    }

}

#[derive(Debug, PartialEq, Clone)]
pub struct ModuleBody<R: Rule>(pub Vec<ModuleItem<R>>);

pub fn module_body<R: Rule>(state: &mut ParserState) -> Result<ModuleBody<R>, String> {
    let mut items = Vec::new();

    while let Some(_) = state.lookahead_1() {
        items.push(module_item(state)?);
        state.consume_whitespace();
    }

    Ok(ModuleBody(items))
}

#[derive(Debug, PartialEq, Clone)]
pub enum ModuleItem<R: Rule> {
    // ImportDecl(ImportDecl<R>),
    // ExportDecl(ExportDecl<R>),
    ModuleDecl(ModuleDecl<R>)
}

pub fn module_item<R: Rule>(state: &mut ParserState) -> Result<ModuleItem<R>, String> {
    module_decl(state).map(ModuleItem::ModuleDecl) // TODO
}

#[derive(Debug, PartialEq, Clone)]
pub struct ModuleDecl<R: Rule>(Vec<ModuleBinding<R>>);

pub fn module_decl<R: Rule>(state: &mut ParserState) -> Result<ModuleDecl<R>, String> {
    state.consume("const")?;
    repeated_elements(state, ' ', ';', &module_binding, false).map(ModuleDecl)
}

#[derive(Debug, PartialEq, Clone)]
pub struct HardenedExpr<R: Rule>(pub Expr<R>); // TODO

pub fn hardened_expr<R: Rule>(state: &mut ParserState) -> Result<HardenedExpr<R>, String> {
    Ok(HardenedExpr(expr(state)?))
}

#[derive(Debug, PartialEq, Clone)]
pub enum ModuleBinding<R: Rule> {
    VariableBinding(R::Variable, Option<HardenedExpr<R>>),
    PatternBinding(BindingPattern<R>, HardenedExpr<R>),
}

pub fn module_binding<R: Rule>(state: &mut ParserState) -> Result<ModuleBinding<R>, String> {
    match state.lookahead_1() {
        Some('{') | Some('[') => {
            let pattern = binding_pattern(state)?;
            state.consume_whitespace();
            state.consume("=")?;
            state.consume_whitespace();
            let expr = hardened_expr(state)?;
            Ok(ModuleBinding::PatternBinding(pattern, expr))
        },
        _ => {
            let ident = R::variable(state)?;
            state.consume_whitespace();
            state.consume("=")?;
            state.consume_whitespace();
            let expr = state.attempt(hardened_expr).ok();
            Ok(ModuleBinding::VariableBinding(ident, expr))
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum BindingPattern<R: Rule> {
    ArrayPattern(Vec<Param<R>>),
    RecordPattern(Vec<PropParam<R>>),
}

pub fn binding_pattern<R: Rule>(state: &mut ParserState) -> Result<BindingPattern<R>, String> {
    match state.lookahead_1() {
        Some('[') => repeated_elements(state, '[', ']', &param, false).map(BindingPattern::ArrayPattern),
        Some('{') => repeated_elements(state, '{', '}', &prop_param, false).map(BindingPattern::RecordPattern),
        _ => Err("Expected binding pattern".to_string()),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern<R: Rule> {
    BindingPattern(BindingPattern<R>),
    Variable(String),
    DataLiteral(DataLiteral), // undefined is in dataliteal for jessie rule
    Hole,
}

pub fn pattern<R: Rule>(state: &mut ParserState) -> Result<Pattern<R>, String> {
    match state.lookahead_1() {
        Some('[') | Some('{') => binding_pattern(state).map(|x| Pattern::BindingPattern(x)),
        Some(',') | Some(']') => Ok(Pattern::Hole), // Not sure if its the right way...
        _ => data_literal(state).map(|x| Pattern::DataLiteral(x)).or_else(|_| {
            R::ident(state).map(|x| Pattern::Variable(x))
        }),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Param<R: Rule> {
    Rest(Pattern<R>),
    Optional(String, Expr<R>),
    Pattern(Pattern<R>),
}

pub fn param<R: Rule>(state: &mut ParserState) -> Result<Param<R>, String> {
    if state.lookahead_1() == Some('.') {
        state.consume("...")?;
        state.consume_whitespace();
        return pattern(state).map(|x| Param::Rest(x))
    }

    let pat = pattern(state)?;
    state.consume_whitespace();
    if let Pattern::Variable(ref x) = pat {
        if state.lookahead_1() == Some('=') {
            state.consume("=")?;
            state.consume_whitespace();
            let expr = expr(state)?;
            return Ok(Param::Optional(x.clone(), expr))
        }
    }

    Ok(Param::Pattern(pat))
}

#[derive(Debug, PartialEq, Clone)]
pub enum PropParam<R: Rule> {
    Rest(Pattern<R>),
    KeyValue(String, Pattern<R>),
    Optional(String, Expr<R>),
    Shorthand(String),
}

pub fn prop_param<R: Rule>(state: &mut ParserState) -> Result<PropParam<R>, String> {
    if state.lookahead_1() == Some('.') {
        state.consume("...")?;
        state.consume_whitespace();
        return pattern(state).map(|x| PropParam::Rest(x))
    }

    let key = R::ident(state)?;
    state.consume_whitespace();

    match state.lookahead_1() {
        Some(':') => {
            state.consume(":")?;
            state.consume_whitespace();
            let pat = pattern(state)?;
            Ok(PropParam::KeyValue(key, pat))
        },
        Some('=') => {
            state.consume("=")?;
            state.consume_whitespace();
            let expr = expr(state)?;
            Ok(PropParam::Optional(key, expr))
        }
        _ => Ok(PropParam::Shorthand(key)),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<R: Rule> {
    ArrowFunc(Box<ArrowFunc<R>>),
    FunctionExpr(Box<FunctionExpr<R>>),
    Assignment(Box<LValue<R>>, AssignOp, Box<Expr<R>>),
    CondExpr(Box<CondExpr<R>>),
    BinaryExpr(Box<BinaryExpr<R>>),
    UnaryExpr(Box<UnaryExpr<R>>),
    CallExpr(Box<CallExpr<R>>),
    DataStructure(Box<DataStructure<R>>),
    // QuasiExpr()
    ParenedExpr(Box<Expr<R>>),
    Variable(String),

}

impl<R: Rule> Expr<R> {
    pub fn new_literal(lit: DataLiteral) -> Self {
        Expr::DataStructure(Box::new(DataStructure::DataLiteral(lit)))
    }

    pub fn new_data_structure(ds: DataStructure<R>) -> Self {
        Expr::DataStructure(Box::new(ds))
    }

    pub fn new_number(n: i64) -> Self {
        Self::new_literal(DataLiteral::Number(n.to_string()))
    }

    pub fn new_add(l: Expr<R>, r: Expr<R>) -> Self {
        Expr::BinaryExpr(Box::new(BinaryExpr(BinaryOp::Add, l, r)))
    }
}

pub fn expr<R: Rule>(state: &mut ParserState) -> Result<Expr<R>, String> {
    state.attempt(arrow_func).map(|x| Expr::ArrowFunc(Box::new(x))).or_else(|_| 
        state.attempt(function_expr).map(|x| Expr::FunctionExpr(Box::new(x))).or_else(|_| 
            state.attempt(assignment).map(|(lv, op, rv)| Expr::Assignment(Box::new(lv), op, rv)).or_else(|_| 
                state.attempt(cond_expr).or_else(|_| 
                    primary_expr(state)
                )
            )
        )
    )
}

pub fn assignment<R: Rule>(state: &mut ParserState) -> Result<(LValue<R>, AssignOp, Box<Expr<R>>), String> {
    let lvalue = lvalue(state)?;
    state.consume_whitespace();
    let op = assign_op(state)?;
    state.consume_whitespace();
    let expr = Box::new(expr(state)?);
    Ok((lvalue, op, expr))
}

#[derive(Debug, PartialEq, Clone)]
pub enum AssignOp {
    Assign,
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignMod,
    AssignExp,
    AssignLShift,
    AssignRShift,
    AssignURShift,
    AssignBitAnd,
    AssignBitXor,
    AssignBitOr,
}

pub fn assign_op(state: &mut ParserState) -> Result<AssignOp, String> {
    match state.lookahead_1() {
        Some('=') => {
            state.proceed();
            Ok(AssignOp::Assign)
        },
        Some('+') => {
            state.consume("+=")?;
            Ok(AssignOp::AssignAdd)
        },
        Some('-') => {
            state.consume("-=")?;
            Ok(AssignOp::AssignSub)
        },
        Some('*') => {
            state.consume("*=")?;
            Ok(AssignOp::AssignMul)
        },
        Some('/') => {
            state.consume("/=")?;
            Ok(AssignOp::AssignDiv)
        },
        Some('%') => {
            state.consume("%=")?;
            Ok(AssignOp::AssignMod)
        },
        Some('^') => {
            state.consume("^=")?;
            Ok(AssignOp::AssignExp)
        },
        Some('<') => {
            state.consume("<<=")?;
            Ok(AssignOp::AssignLShift)
        },
        Some('>') => {
            match state.lookahead_3() {
                Some('=') => {
                    state.consume(">>=")?;
                    Ok(AssignOp::AssignRShift)
                },
                Some('>') => {
                    state.consume(">>>=")?;
                    Ok(AssignOp::AssignURShift)
                },
                _ => Err("Expected >> or >>>".to_string()),
            }
        },
        Some('&') => {
            state.consume("&=")?;
            Ok(AssignOp::AssignBitAnd)
        },
        Some('|') => {
            state.consume("|=")?;
            Ok(AssignOp::AssignBitOr)
        },
        Some('^') => {
            state.consume("^=")?;
            Ok(AssignOp::AssignBitXor)
        },
        _ => Err("Expected assignment operator".to_string()),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum PureExpr<R: Rule> {
    ArrowFunc(ArrowFunc<R>),
    Parent(json::PureExpr<R>),
    ParenedExpr(Box<PureExpr<R>>),
    Variable(String),
}

pub fn pure_expr<R: Rule>(state: &mut ParserState) -> Result<PureExpr<R>, String> {
    state.attempt(arrow_func).map(|x| PureExpr::ArrowFunc(x)).or_else(|_| 
        state.attempt(json::pure_expr).map(|x| PureExpr::Parent(x)).or_else(|_| 
            state.attempt(|state| {
                state.consume("(")?;
                state.consume_whitespace();
                let expr = pure_expr(state)?;
                state.consume_whitespace();
                state.consume(")")?;
                Ok(PureExpr::ParenedExpr(Box::new(expr)))
            }).map(|x| PureExpr::ParenedExpr(Box::new(x))).or_else(|_| 
                R::ident(state).map(|x| PureExpr::Variable(x))
            )
        )
    )
}

#[derive(Debug, PartialEq, Clone)]
pub enum LValue<R: Rule> {
    Index(Expr<R>, Expr<R>),
    Member(Expr<R>, String),
    Variable(String),
}

pub fn lvalue<R: Rule>(state: &mut ParserState) -> Result<LValue<R>, String> {
    let lval = primary_expr(state)?;

    match state.lookahead_1() {
        Some('[') => {
            state.consume("[")?;
            state.consume_whitespace();
            let index = expr(state)?; // Different from Jessie! (Jessie uses index_expr)
            state.consume_whitespace();
            state.consume("]")?;
            Ok(LValue::Index(lval, index))
        },
        Some('.') => {
            state.consume(".")?;
            state.consume_whitespace();
            let name = R::ident(state)?;
            Ok(LValue::Member(lval, name))
        },
        _ => {
            match lval {
                Expr::Variable(name) => Ok(LValue::Variable(name)),
                _ => Err("Expected lvalue".to_string()),
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<R: Rule> {
    Block(Block<R>),
    IfStatement(IfStatement<R>),
    BreakableStatement(BreakableStatement<R>),
    Terminator(Terminator<R>),
    // TryStatement(TryStatement<R>),
    ExprStatement(Expr<R>),
}

pub fn statement<R: Rule>(state: &mut ParserState) -> Result<Statement<R>, String> {
    state.attempt(block).map(|x| Statement::Block(x)).or_else(|_| 
        state.attempt(if_statement).map(|x| Statement::IfStatement(x)).or_else(|_| 
            state.attempt(breakable_statement).map(|x| Statement::BreakableStatement(x)).or_else(|_| 
                state.attempt(terminator).map(|x| Statement::Terminator(x)).or_else(|_| 
                    //state.attempt(try_statement).map(|x| Statement::TryStatement(x)).or_else(|_| 
                        state.attempt(expr_statement).map(|x| Statement::ExprStatement(x))
                    // )
                )
            )
        )
    )
}

pub fn expr_statement<R: Rule>(state: &mut ParserState) -> Result<Expr<R>, String> {
    // TODO: cantStartExprStatement
    let result = expr(state);
    state.consume_whitespace();
    state.consume(";")?;
    result
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block<R: Rule> {
    pub statements: Vec<StatementItem<R>>,
}

pub fn block<R: Rule>(state: &mut ParserState) -> Result<Block<R>, String> {
    state.consume("{")?;
    state.consume_whitespace();

    let mut statements = Vec::new();
    while state.lookahead_1() != Some('}') { // is it memory safe??
        statements.push(statement_item(state)?);
        state.consume_whitespace();
    }

    Ok(Block { statements })
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfStatement<R: Rule> {
    pub condition: Expr<R>,
    pub consequent: Block<R>,
    pub alternate: Option<ElseArm<R>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ElseArm<R: Rule> {
    Body(Block<R>),
    ElseIf(Box<IfStatement<R>>),
}

pub fn if_statement<R: Rule>(state: &mut ParserState) -> Result<IfStatement<R>, String> {
    state.consume("if")?;
    state.consume_whitespace();
    state.consume("(")?;
    state.consume_whitespace();
    let condition = expr(state)?;
    state.consume_whitespace();
    state.consume(")")?;
    state.consume_whitespace();
    let consequent = block(state)?;
    state.consume_whitespace();

    let alternate = if state.lookahead_1() == Some('e') {
        state.consume("else")?;
        state.lookahead_whitespace_nonident()?;
        state.consume_whitespace();
        if state.lookahead_1() == Some('i') {
            Some(ElseArm::ElseIf(if_statement(state).map(Box::new)?))
        } else {
            Some(ElseArm::Body(block(state)?))
        }
    } else {
        None
    };

    Ok(IfStatement { condition, consequent, alternate })
}

#[derive(Debug, PartialEq, Clone)]
pub enum BreakableStatement<R: Rule> {
    // TODO
    //ForStatement(ForStatement<R>),
    WhileStatement(WhileStatement<R>),
    //DoWhileStatement(DoWhileStatement<R>),
}

pub fn breakable_statement<R: Rule>(state: &mut ParserState) -> Result<BreakableStatement<R>, String> {
    state.attempt(while_statement).map(|x| BreakableStatement::WhileStatement(x))
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhileStatement<R: Rule> {
    pub condition: Expr<R>,
    pub body: Block<R>,
}

pub fn while_statement<R: Rule>(state: &mut ParserState) -> Result<WhileStatement<R>, String> {
    state.consume("while")?;
    state.consume_whitespace();
    state.consume("(")?;
    state.consume_whitespace();
    let condition = expr(state)?;
    state.consume_whitespace();
    state.consume(")")?;
    state.consume_whitespace();
    let body = block(state)?;

    Ok(WhileStatement { condition, body })
}

#[derive(Debug, PartialEq, Clone)]
pub enum Terminator<R: Rule> {
    Continue,
    Break,
    Return(Option<Expr<R>>),
    Throw(Expr<R>),
}

pub fn terminator<R: Rule>(state: &mut ParserState) -> Result<Terminator<R>, String> {
    println!("terminator");
    match state.lookahead_1() {
        Some('c') => {
            state.consume("continue")?;
            state.consume_whitespace();
            state.consume(";")?;
            Ok(Terminator::Continue)
        },
        Some('b') => {
            state.consume("break")?;
            state.consume_whitespace();
            state.consume(";")?;
            Ok(Terminator::Break)
        },
        Some('r') => {
            println!("return start");
            state.consume("return")?;
            println!("return consumed");
            let arg = state.attempt(|state| {
                state.consume_whitespace();
                state.consume(";")?;
                Ok(None)
            }).or_else(|_| {
                // TODO: no newline for return argument
                state.lookahead_whitespace_nonident()?;
                state.consume_whitespace();
                let arg = expr(state)?;
                state.consume(";")?;
                Ok::<Option<Expr<R>>, String>(Some(arg))
            })?;
            println!("return end");
            Ok(Terminator::Return(arg))
        },
        Some('t') => {
            state.consume("throw")?;
            state.lookahead_whitespace_nonident()?; // TODO: no newline
            state.consume_whitespace();
            let expr = expr(state)?;
            state.consume(";")?;
            Ok(Terminator::Throw(expr))
        },
        _ => Err("Expected terminator".to_string()),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum StatementItem<R: Rule> {
    Declaration(Declaration<R>),
    FunctionDecl(FunctionDecl<R>),
    Statement(Statement<R>),
}

pub fn statement_item<R: Rule>(state: &mut ParserState) -> Result<StatementItem<R>, String> {
    state.attempt(declaration).map( StatementItem::Declaration).or_else(|_| 
        state.attempt(function_decl).map(StatementItem::FunctionDecl).or_else(|_| 
            state.attempt(statement).map( StatementItem::Statement)
        )
    )
}

#[derive(Debug, PartialEq, Clone)]
pub enum DeclarationKind {
    Let,
    Const,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Declaration<R: Rule> {
    pub kind: DeclarationKind,
    pub bindings: Vec<Binding<R>>,
}

pub fn declaration<R: Rule>(state: &mut ParserState) -> Result<Declaration<R>, String> {
    let kind = match state.lookahead_1() {
        Some('l') => state.consume("let").map(|_| DeclarationKind::Let),
        Some('c') => state.consume("const").map(|_| DeclarationKind::Const),
        _ => Err("Expected 'let' or 'const'".to_string()),
    }?;

    let bindings = repeated_elements(state, ' ', ';', &binding, false)?; 

    Ok(Declaration { kind, bindings })
}

#[derive(Debug, PartialEq, Clone)]
pub enum Binding<R: Rule> {
    VariableBinding(String, Option<Expr<R>>),
    PatternBinding(BindingPattern<R>, Expr<R>),
}

pub fn binding<R: Rule>(state: &mut ParserState) -> Result<Binding<R>, String> {
    match state.lookahead_1() {
        Some('{') | Some('[') => {
            let pattern = binding_pattern(state)?;
            state.consume_whitespace();
            state.consume("=")?;
            state.consume_whitespace();
            let expr = expr(state)?;
            Ok(Binding::PatternBinding(pattern, expr))
        },
        _ => {
            let name = R::ident(state)?;
            let expr = state.attempt(|state| {
                state.consume_whitespace();
                state.consume("=")?;
                state.consume_whitespace();
                expr(state)
            }).ok();
            Ok(Binding::VariableBinding(name, expr))
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ReservedWord {
    Null,
    False,
    True,
    Async,
    Arguments,
    Eval,
    Get,
    Set,
}

pub fn reserved_word(state: &mut ParserState) -> Result<ReservedWord, String> {
    match state.lookahead_1() {
        Some('n') => state.consume("null").map(|_| ReservedWord::Null),
        Some('f') => state.consume("false").map(|_| ReservedWord::False),
        Some('t') => state.consume("true").map(|_| ReservedWord::True),
        Some('a') => match state.lookahead_2() {
            Some('s') => state.consume("async").map(|_| ReservedWord::Async),
            Some('r') => state.consume("arguments").map(|_| ReservedWord::Arguments),
            _ => Err("Expected 'async' or 'arguments'".to_string()), 
        },
        Some('e') => state.consume("eval").map(|_| ReservedWord::Eval),
        Some('g') => state.consume("get").map(|_| ReservedWord::Get),
        Some('s') => state.consume("set").map(|_| ReservedWord::Set),
        _ => Err("Expected reserved word'".to_string()),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ReservedKeyword {
    Class,
    Delete,
    Do,
    Extends,
    InstanceOf,
    In,
    New,
    Super,
    This,
    Var,
    With,
    Yield,
}

pub fn reserved_keyword(state: &mut ParserState) -> Result<ReservedKeyword, String> {
    let reserved_word = match state.lookahead_1() {
        Some('c') => state.consume("class").map(|_| ReservedKeyword::Class),
        Some('d') => {
            match state.lookahead_2() {
                Some('e') => state.consume("delete").map(|_| ReservedKeyword::Delete),
                Some('o') => state.consume("do").map(|_| ReservedKeyword::Do),
                _ => Err("Expected 'delete' or 'do'".to_string()),
            }
        },
        Some('e') => state.consume("extends").map(|_| ReservedKeyword::Extends),
        Some('i') => {
            match state.lookahead_2() {
                Some('n') => {
                    match state.lookahead_3() {
                        Some('s') => state.consume("instanceof").map(|_| ReservedKeyword::InstanceOf),
                        Some(' ') => state.consume("in").map(|_| ReservedKeyword::In),
                        _ => Err("Expected 'instanceof' or 'in'".to_string()),
                    }
                },
                _ => Err("Expected 'instanceof' or 'in'".to_string()),
            }
        },
        Some('n') => state.consume("new").map(|_| ReservedKeyword::New),
        Some('s') => state.consume("super").map(|_| ReservedKeyword::Super),
        Some('t') => state.consume("this").map(|_| ReservedKeyword::This),
        Some('v') => state.consume("var").map(|_| ReservedKeyword::Var),
        Some('w') => state.consume("with").map(|_| ReservedKeyword::With),
        Some('y') => state.consume("yield").map(|_| ReservedKeyword::Yield),
        _ => Err("Expected a reserved keyword".to_string()),
    }?;

    state.lookahead_whitespace_nonident()?;

    Ok(reserved_word)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Break,
    Case,
    Catch,
    Const,
    Continue,
    Debugger,
    Default,
    Else,
    Export,
    Finally,
    For,
    Function,
    If,
    Import,
    Return,
    Switch,
    Throw,
    Try,
    TypeOf,
    Void,
    While,
}

pub fn keyword(state: &mut ParserState) -> Result<Keyword, String> {
    let keyword = match state.lookahead_1() {
        Some('b') => state.consume("break").map(|_| Keyword::Break),
        Some('c') => {
            match state.lookahead_2() {
                Some('a') => match state.lookahead_3() {
                    Some('s') => state.consume("case").map(|_| Keyword::Case),
                    Some('t') => state.consume("catch").map(|_| Keyword::Catch),
                    _ => Err("Expected 'case' or 'catch'".to_string()),
                },
                Some('o') => match state.lookahead_3() {
                    Some('n') => state.consume("const").map(|_| Keyword::Const),
                    Some('n') => state.consume("continue").map(|_| Keyword::Continue),
                    _ => Err("Expected 'const' or 'continue'".to_string()),
                },
                _ => Err("Expected 'case', 'catch', 'const', or 'continue'".to_string()),
            }
        },
        Some('d') => {
            match state.lookahead_2() {
                Some('e') => state.consume("debugger").map(|_| Keyword::Debugger),
                Some('e') => state.consume("default").map(|_| Keyword::Default),
                _ => Err("Expected 'debugger' or 'default'".to_string()),
            }
        },
        Some('e') => state.consume("else").map(|_| Keyword::Else),
        Some('e') => state.consume("export").map(|_| Keyword::Export),
        Some('f') => {
            match state.lookahead_2() {
                Some('i') => state.consume("finally").map(|_| Keyword::Finally),
                Some('o') => state.consume("for").map(|_| Keyword::For),
                Some('u') => state.consume("function").map(|_| Keyword::Function),
                _ => Err("Expected 'finally', 'for', or 'function'".to_string()),
            }
        },
        Some('i') => state.consume("if").map(|_| Keyword::If),
        Some('i') => state.consume("import").map(|_| Keyword::Import),
        Some('r') => state.consume("return").map(|_| Keyword::Return),
        Some('s') => state.consume("switch").map(|_| Keyword::Switch),
        Some('t') => {
            match state.lookahead_2() {
                Some('h') => state.consume("throw").map(|_| Keyword::Throw),
                Some('r') => state.consume("try").map(|_| Keyword::Try),
                Some('y') => state.consume("typeof").map(|_| Keyword::TypeOf),
                _ => Err("Expected 'throw', 'try', or 'typeof'".to_string()),
            }
        },
        Some('v') => state.consume("void").map(|_| Keyword::Void),
        Some('w') => state.consume("while").map(|_| Keyword::While),
        _ => Err("Expected a keyword".to_string()),
    };

    if keyword.is_err() {
        return keyword
    }

    state.lookahead_whitespace_nonident()?;

    keyword
}

#[derive(Debug, PartialEq, Clone)]
pub enum FutureReservedWord {
    Await,
    Enum,
    Implements,
    Package,
    Protected,
    Interface,
    Private,
    Public,
}

pub fn future_reserved_word(state: &mut ParserState) -> Result<FutureReservedWord, String> {
    match state.lookahead_1() {
        Some('a') => state.consume("await").map(|_| FutureReservedWord::Await),
        Some('e') => state.consume("enum").map(|_| FutureReservedWord::Enum),
        Some('i') => state.consume("implements").map(|_| FutureReservedWord::Implements),
        Some('p') => {
            match state.lookahead_2() {
                Some('a') => state.consume("package").map(|_| FutureReservedWord::Package),
                Some('r') => state.consume("protected").map(|_| FutureReservedWord::Protected),
                // TODO: private
                Some('u') => state.consume("public").map(|_| FutureReservedWord::Public), 
                _ => Err("Expected 'package' or 'protected'".to_string()),
            }
        },
        Some('i') => state.consume("interface").map(|_| FutureReservedWord::Interface),
        _ => Err("Expected a future reserved word".to_string()),
    }   
}

pub fn primary_expr<R: Rule>(state: &mut ParserState) -> Result<Expr<R>, String> {
    state.attempt(data_structure).map(|data| Expr::DataStructure(Box::new(data))).or_else(|_| {
        match state.lookahead_1() {
            Some('(') => {
                state.consume("(")?;
                state.consume_whitespace();
                let expr = expr(state)?;
                state.consume(")")?;
                state.consume_whitespace();
                Ok(Expr::ParenedExpr(Box::new(expr)))
            },
            Some('`') => Err("QuasiExpr not implemented".to_string()),
            _ => Ok(R::ident(state).map(Expr::Variable)?),
        }
    })
}

#[derive(Debug, PartialEq, Clone)]
pub enum Element<R: Rule> {
    Spread(R::Expr),
    Expr(R::Expr),
}

pub fn element<R: Rule>(state: &mut ParserState) -> Result<Element<R>, String> {
    if state.lookahead_1() == Some('.') {
        state.consume(".")?;
        state.consume(".")?;
        state.consume(".")?;
        state.consume_whitespace();
        let expr = R::expr(state)?;
        Ok(Element::Spread(expr))
    } else {
        let expr = R::expr(state)?;
        Ok(Element::Expr(expr))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum PurePropDef<R: Rule> {
    // MethodDef(MethodDef<R>),//TODO
    Parent(json::PurePropDef<R>),
    Shorthand(R::PropName),
    Spread(R::Expr),
}

pub fn pure_prop_def<R: Rule>(state: &mut ParserState) -> Result<PurePropDef<R>, String> {
    if state.lookahead_1() == Some('.') {
        state.consume("...")?;
        state.consume_whitespace();
        let expr = R::expr(state)?;
        Ok(PurePropDef::Spread(expr))
    }
    // copilot generated these lines, didnt understand 
    /*  
    else if state.lookahead_1() == Some('[') {
        state.consume("[")?;
        state.consume_whitespace();
        let prop_name = R::prop_name(state)?;
        state.consume("]")?;
        state.consume_whitespace();
        if state.lookahead_1() == Some('(') {
            let method_def = method_def(state)?;
            Ok(PurePropDef::MethodDef(method_def))
        } else {
            Ok(PurePropDef::Parent(json::PurePropDef::Shorthand(prop_name)))
        }
    }
    */
    else {
        let prop_name = R::prop_name(state)?;
        state.consume_whitespace();
        if state.lookahead_1() == Some('(') {
            unimplemented!()
            /* 
            let method_def = method_def(state)?;
            Ok(PurePropDef::MethodDef(method_def))
            */
        } else {
            Ok(PurePropDef::Shorthand(prop_name))
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum PropDef<R: Rule> {
    // MethodDef(MethodDef<R>),// TODO
    KeyValue(R::PropName, R::Expr), 
    Shorthand(R::PropName),
    Spread(R::Expr),
}

pub fn prop_def<R: Rule>(state: &mut ParserState) -> Result<PropDef<R>, String> {
    if state.lookahead_1() == Some('.') {
        state.consume(".")?;
        state.consume(".")?;
        state.consume(".")?;
        state.consume_whitespace();
        let expr = R::expr(state)?;
        Ok(PropDef::Spread(expr))
    }
    // copilot generated these lines, didnt understand 
    /*  
    else if state.lookahead_1() == Some('[') {
        state.consume("[")?;
        state.consume_whitespace();
        let prop_name = R::prop_name(state)?;
        state.consume("]")?;
        state.consume_whitespace();
        state.consume(":")?;
        state.consume_whitespace();
        let expr = R::expr(state)?;
        Ok(PropDef::KeyValue(prop_name, expr))
    } else if state.lookahead_1() == Some('*') {
        state.consume("*")?;
        state.consume_whitespace();
        let prop_name = R::prop_name(state)?;
        state.consume(":")?;
        state.consume_whitespace();
        let expr = R::expr(state)?;
        Ok(PropDef::KeyValue(prop_name, expr))
    */
    else if state.lookahead_1() == Some('`') {
        Err("QuasiExpr not implemented".to_string())
    } else {
        let prop_name = R::prop_name(state)?;
        state.consume_whitespace();
        if state.lookahead_1() == Some(':') {
            state.consume(":")?;
            state.consume_whitespace();
            let expr = R::expr(state)?;
            Ok(PropDef::KeyValue(prop_name, expr))
        } else {
            Ok(PropDef::Shorthand(prop_name))
        }
    }
}


#[derive(Debug, PartialEq, Clone)]
pub enum MemberPostOp<R: Rule> {
    Index(Expr<R>),
    Member(String),
    // QuasiExpr
}

pub fn member_post_op<R: Rule>(state: &mut ParserState) -> Result<MemberPostOp<R>, String> {
    if state.lookahead_1() == Some('[') {
        state.consume("[")?;
        state.consume_whitespace();
        let expr = expr(state)?;
        state.consume("]")?;
        state.consume_whitespace();
        Ok(MemberPostOp::Index(expr))
    } else if state.lookahead_1() == Some('.') {
        state.consume(".")?;
        state.consume_whitespace();
        let ident = R::ident(state)?;
        Ok(MemberPostOp::Member(ident))
    } else {
        Err("Expected '[' or '.'".to_string())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum CallPostOp<R: Rule> {
    MemberPostOp(MemberPostOp<R>),
    Call(Vec<Arg<R>>),
}

pub fn call_post_op<R: Rule>(state: &mut ParserState) -> Result<CallPostOp<R>, String> {
    let result = if state.lookahead_1() == Some('[') || state.lookahead_1() == Some('.') {
        let member_post_op = member_post_op(state)?;
        Ok(CallPostOp::MemberPostOp(member_post_op))
    } else if state.lookahead_1() == Some('(') {
        state.consume("(")?;
        state.consume_whitespace();
        let args = args(state)?;
        state.consume(")")?;
        state.consume_whitespace();
        Ok(CallPostOp::Call(args))
    } else {
        Err("Expected '[' or '.' or '('. Got: ".to_string() + &state.lookahead_1().unwrap_or('\0').to_string())
    };

    state.consume_whitespace();

    result
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpr<R: Rule> {
    pub expr: Expr<R>,
    pub post_op: CallPostOp<R>,
}

pub fn call_expr<R: Rule>(state: &mut ParserState) -> Result<Expr<R>, String> { 
    let mut result = primary_expr(state)?;
    loop { // I don't like having an infinite loop here...
        match call_post_op(state) {
            Ok(op) => result = Expr::CallExpr(Box::new(CallExpr { expr: result, post_op: op })),
            Err(_) => break,
        }
    }
    Ok(result)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Arg<R: Rule> {
    Expr(R::Expr),
    Spread(R::Expr),
}

pub fn arg<R: Rule>(state: &mut ParserState) -> Result<Arg<R>, String> {
    if state.lookahead_1() == Some('.') {
        state.consume(".")?;
        state.consume(".")?;
        state.consume(".")?;
        state.consume_whitespace();
        let expr = R::expr(state)?;
        Ok(Arg::Spread(expr))
    } else {
        let expr = R::expr(state)?;
        Ok(Arg::Expr(expr))
    }
}

pub fn args<R: Rule>(state: &mut ParserState) -> Result<Vec<Arg<R>>, String> {
    state.consume("(")?;
    let mut args = vec![];
    if state.lookahead_1() == Some(')') {
        return Ok(args);
    }
    loop {
        args.push(arg::<R>(state)?);
        // TODO: check EOF
        if state.lookahead_1() == Some(')') {
            break;
        }
        state.consume(",")?;
        state.consume_whitespace();
    }
    Ok(args)
}

// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence
#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    // 3
    Or, // Left
    Coalesce, // Left

    // 4
    And, // Left

    // 5
    BitwiseOr, // Left

    // 6
    BitwiseXor, // Left

    // 7
    BitwiseAnd, // Left

    // 8 
    StrictEqual, // Left
    StrictNotEqual, // Left

    // 9 
    LessThan, // Left
    LessThanEqual, // Left
    GreaterThan, // Left
    GreaterThanEqual, // Left

    // 10
    BitwiseLeftShift, // Left
    BitwiseRightShift, // Left
    BitwiseUnsignedRightShift, // Left

    // 11
    Add, // Left
    Sub, // Left

    // 12
    Mul, // Left
    Div, // Left
    Mod, // Left

    // 13
    Pow, // Right
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpr<R: Rule>(pub BinaryOp, pub Expr<R>, pub Expr<R>);

#[derive(Debug, PartialEq, Clone)]
pub struct CondExpr<R: Rule>(pub Expr<R>, pub Expr<R>, pub Expr<R>);

pub fn cond_expr<R: Rule>(state: &mut ParserState) -> Result<Expr<R>, String> {
    let or_else_expr = or_else_expr(state)?;
    state.consume_whitespace();
    if state.lookahead_1() == Some('?') {
        state.consume("?")?;
        state.consume_whitespace();
        let expr1 = expr(state)?;
        state.consume(":")?;
        state.consume_whitespace();
        let expr2 = expr(state)?;
        Ok(Expr::CondExpr(Box::new(CondExpr(or_else_expr, expr1, expr2))))
    } else {
        Ok(or_else_expr)
    }
}

pub fn or_else_expr<R: Rule>(state: &mut ParserState) -> Result<Expr<R>, String> {
    let mut result = and_then_expr(state)?;
    state.consume_whitespace();
    while state.lookahead_1() == Some('|') && state.lookahead_2() == Some('|') {
        state.consume("||")?;
        state.consume_whitespace();
        let and_then_expr2 = and_then_expr(state)?;
        result = Expr::BinaryExpr(Box::new(BinaryExpr(BinaryOp::Or, result, and_then_expr2)))
    }
    Ok(result)
}

pub fn and_then_expr<R: Rule>(state: &mut ParserState) -> Result<Expr<R>, String> {
    let mut result = eager_expr(state)?;
    state.consume_whitespace();
    while state.lookahead_1() == Some('&') && state.lookahead_2() == Some('&') {
        state.consume("&&")?;
        state.consume_whitespace();
        let eager_expr2 = eager_expr(state)?;
        result = Expr::BinaryExpr(Box::new(BinaryExpr(BinaryOp::And, result, eager_expr2)))
    }
    Ok(result)
}

pub fn eager_expr<R: Rule>(state: &mut ParserState) -> Result<Expr<R>, String> {
    let mut result = shift_expr(state)?;
    state.consume_whitespace();
    while let Some(la) = state.lookahead_1() {
        match la {
            '<' => {
                if state.lookahead_2() == Some('=') {
                    state.consume("<=")?;
                    state.consume_whitespace();
                    result = Expr::BinaryExpr(Box::new(BinaryExpr(BinaryOp::LessThanEqual, result, shift_expr(state)?)));
                } else {
                    state.consume("<")?;
                    state.consume_whitespace();
                    result = Expr::BinaryExpr(Box::new(BinaryExpr(BinaryOp::LessThan, result, shift_expr(state)?)));
                }
            },
            '>' => {
                if state.lookahead_2() == Some('=') {
                    state.consume(">=")?;
                    state.consume_whitespace();
                    result = Expr::BinaryExpr(Box::new(BinaryExpr(BinaryOp::GreaterThanEqual, result, shift_expr(state)?)));
                } else {
                    state.consume(">")?;
                    state.consume_whitespace();
                    result = Expr::BinaryExpr(Box::new(BinaryExpr(BinaryOp::GreaterThan, result, shift_expr(state)?)));
                }
            },
            '=' => {

            },
            '!' => {
                
            },
            '&' => {

            },
            '|' => {

            },
            '^' => {

            },
            _ => break,
        }

        state.consume_whitespace();
    }

    Ok(result)
}

pub fn shift_expr<R: Rule>(state: &mut ParserState) -> Result<Expr<R>, String> {
    let mut result = add_expr(state)?;
    state.consume_whitespace();
    while let Some(la) = state.lookahead_1() {
        match la {
            '<' => {
                state.consume("<<")?;
                state.consume_whitespace();
                result = Expr::BinaryExpr(Box::new(BinaryExpr(BinaryOp::BitwiseLeftShift, result, add_expr(state)?)));
            },
            '>' => {
                state.consume(">>")?;
                if state.lookahead_1() == Some('>') {
                    state.consume(">")?;
                    state.consume_whitespace();
                    result = Expr::BinaryExpr(Box::new(BinaryExpr(BinaryOp::BitwiseUnsignedRightShift, result, add_expr(state)?)));
                } else {
                    state.consume_whitespace();
                    result = Expr::BinaryExpr(Box::new(BinaryExpr(BinaryOp::BitwiseRightShift, result, add_expr(state)?)));
                }
            },
            _ => break,
        }

        state.consume_whitespace();
    }

    Ok(result)
}

pub fn add_expr<R: Rule>(state: &mut ParserState) -> Result<Expr<R>, String> {
    let mut result = mult_expr(state)?;
    state.consume_whitespace();
    while let Some(la) = state.lookahead_1() {
        match la {
            '+' => {
                state.consume("+")?;
                state.consume_whitespace();
                result = Expr::BinaryExpr(Box::new(BinaryExpr(BinaryOp::Add, result, mult_expr(state)?)));
            },
            '-' => {
                state.consume("-")?;
                state.consume_whitespace();
                result = Expr::BinaryExpr(Box::new(BinaryExpr(BinaryOp::Sub, result, mult_expr(state)?)));
            },
            _ => break,
        }

        state.consume_whitespace();
    }

    Ok(result)
}

pub fn mult_expr<R: Rule>(state: &mut ParserState) -> Result<Expr<R>, String> {
    let mut result = pow_expr(state)?;
    state.consume_whitespace();
    while let Some(la) = state.lookahead_1() {
        match la {
            '*' => {
                state.consume("*")?;
                state.consume_whitespace();
                result = Expr::BinaryExpr(Box::new(BinaryExpr(BinaryOp::Mul, result, pow_expr(state)?)));
            },
            '/' => {
                state.consume("/")?;
                state.consume_whitespace();
                result = Expr::BinaryExpr(Box::new(BinaryExpr(BinaryOp::Div, result, pow_expr(state)?))); 
            },
            '%' => {
                state.consume("%")?;
                state.consume_whitespace();
                result = Expr::BinaryExpr(Box::new(BinaryExpr(BinaryOp::Mod, result, pow_expr(state)?)));
            },
            _ => break,
        }

        state.consume_whitespace();
    }

    Ok(result)
}

pub fn pow_expr<R: Rule>(state: &mut ParserState) -> Result<Expr<R>, String> {
    // TODO. for now just route to unaryexpr.
    Ok(unary_expr(state)?)
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp { // preOp
    TypeOf,
    Pos,
    Neg,
    BitNot,
    Not,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpr<R: Rule> {
    pub op: Vec<UnaryOp>,
    pub expr: Expr<R>,
}

pub fn unary_expr<R: Rule>(state: &mut ParserState) -> Result<Expr<R>, String> {
    // TODO, for now just route to callexpr.
    call_expr(state)
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDecl<R: Rule>(R::Variable, Vec<Param<R>>, Block<R>);

pub fn function_decl<R: Rule>(state: &mut ParserState) -> Result<FunctionDecl<R>, String> {
    state.consume("function")?;
    state.lookahead_whitespace_nonident();
    state.consume_whitespace();
    let name = R::variable(state)?;
    state.consume_whitespace();
    let params = repeated_elements(state, '(', ')', &param, false/*Check it*/)?;
    state.consume_whitespace();
    let body = block(state)?;
    Ok(FunctionDecl(name, params, body))
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionExpr<R: Rule>(pub Option<String>, pub Vec<Param<R>>, pub Block<R>);

pub fn function_expr<R: Rule>(state: &mut ParserState) -> Result<FunctionExpr<R>, String> {
    println!("function_expr");
    state.consume("function")?;
    println!("1");
    let name = state.attempt(|state| {
        state.lookahead_whitespace_nonident();
        state.consume_whitespace();
        R::ident(state)
    }).ok();
    println!("2");
    state.consume_whitespace();
    let params = repeated_elements(state, '(', ')', &param, false/*Check it*/)?;
    state.consume_whitespace();
    println!("3");
    let body = block(state)?;
    println!("4");
    Ok(FunctionExpr(name, params, body)) 
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrowFunc<R: Rule>(Vec<Param<R>>, Block<R>);

pub fn arrow_func<R: Rule>(state: &mut ParserState) -> Result<ArrowFunc<R>, String> {
    println!("arrow_func");
    let params = match state.lookahead_1() {
        Some('(') => repeated_elements(state, '(', ')', &param, false/*Check it*/)?,
        _ => Err("unimplemented")?,//TODO // R::ident(state).map(|x| vec![ArrowParam(x)]),
    };

    // TODO: no_newline
    state.consume_whitespace();
    state.consume("=>")?;
    state.consume_whitespace();

    let body = match state.lookahead_1() {
        Some('{') => block(state)?,
        _ => Err("unimplemented")?, // TODO: R::expr(state),
    };
    
    Ok(ArrowFunc(params, body))
}
