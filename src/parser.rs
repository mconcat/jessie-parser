use crate::jessie_types::*;
use crate::jessie_operation::*;
use core::fmt::Debug;
pub enum Never {

}
// Scope tracks the variables declared in the current scope.
// It does not record the variables declared in nested scopes.
#[derive(Debug, PartialEq, Clone)]
pub struct Scope {
    pub variables: Vec<String>,
}
/* 
pub trait ParserState: CombinatoryParser+Sized {
    pub fn get_parser(&mut self) -> &mut CombinatoryParserImpl; 

    const IS_PURE_JSON_RULE: bool;
    const IS_JESSIE_RULE: bool;
    const IS_TESSIE_RULE: bool;
/*
    pub fn ident(state: &mut Self) -> Result<String, String>; // TODO: not string maybe?

    type Expr: Debug+PartialEq+Clone+CommonExpr;
    pub fn CondExpr(condition: Self::Expr, consequent: Self::Expr, alternate: Self::Expr) -> Self::Expr;
    pub fn BinaryExpr(op: BinaryOp, left: Self::Expr, right: Self::Expr) -> Self::Expr;
    pub fn UnaryExpr(op: UnaryOp, operand: Self::Expr) -> Self::Expr;
    pub fn CallExpr(expr: Self::Expr, op: CallPostOp<Self>) -> Self::Expr;
    pub fn expr(state: &mut Self) -> Result<Self::Expr, String>;
    pub fn pure_expr(state: &mut Self) -> Result<Self::Expr, String>;
    pub fn primary_expr(state: &mut Self) -> Result<Self::Expr, String>;

    type Function: Debug+PartialEq+Clone;
    pub fn function_expr(state: &mut Self) -> Result<Self::Function, String>;
    pub fn function_decl(state: &mut Self) -> Result<Self::Function, String>;
    pub fn arrow_function(state: &mut Self) -> Result<Self::Function, String>;

    type Element: Debug+PartialEq+Clone;
    pub fn element(state: &mut Self) -> Result<Self::Element, String>;
    // pure element is always pure_expr

    type PropDef: Debug+PartialEq+Clone;
    pub fn prop_def(state: &mut Self) -> Result<Self::PropDef, String>;
    pub fn pure_prop_def(state: &mut Self) -> Result<Self::PropDef, String>;

    type PropName: Debug+PartialEq+Clone;
    pub fn prop_name(state: &mut Self) -> Result<Self::PropName, String>;

    type Variable: Debug+PartialEq+Clone;
    pub fn variable(state: &mut Self) -> Result<Self::Variable, String>;

    type TypeAnn: Debug+PartialEq+Clone;
    pub fn optional_type_ann(state: &mut Self) -> Option<Self::TypeAnn>;
    */
}
*/
/*
impl State {
    pub fn lookahead_1(&self) -> Option<char> {
        self.get_parser().lookahead_1()
    }

    pub fn lookahead_2(&self) -> Option<char> {
        self.get_parser().lookahead_2()
    }

    pub fn lookahead_3(&self) -> Option<char> {
        self.get_parser().lookahead_3()
    }

    pub fn lookahead_4(&self) -> Option<char> {
        self.get_parser().lookahead_4()
    }

    pub fn proceed(&mut self) {
        self.get_parser().proceed()
    }

    pub fn consume_1(&mut self, c: char) -> Result<(), String> {
        self.get_parser().consume_1(c)
    }

    pub fn consume(&mut self, s: &str) -> Result<(), String> {
        self.get_parser().consume(s)
    }

    pub fn consume_whitespace(&mut self) {
        self.get_parser().consume_whitespace()
    }

    pub fn lookahead_whitespace_nonident(&self) -> Result<(), String> {
        self.get_parser().lookahead_whitespace_nonident()
    }

    pub fn attempt<T>(&mut self, f: impl Fn(&mut Self) -> Result<T, String>) -> Result<T, String> {
        let state = self.get_parser();
        let pos = state.pos;
        match f(self) {
            Ok(r) => Ok(r),
            Err(err) => {
                state.pos = pos;
                Err(err)
            }
        }
    }

    pub fn prevent<T>(&mut self, f: impl Fn(&mut Self) -> Result<T, String>) -> Result<(), String> {
        let state = self.get_parser();
        let pos = state.pos;
        let result = match f(self) {
            Ok(_) => Err("Expected error, but got success".to_string()),
            Err(_) => Ok(()),
        };
        state.pos = pos;
        result
    }
}
*/

pub trait Node {
    fn parse(state: &mut ParserState) -> Result<Self, String> where Self: Sized;
}

#[derive(Debug, PartialEq, Clone)]
pub struct ParserState {
    pub input: String,
    pub pos: usize,
    pub scopes: Vec<Scope>,
}

impl ParserState {
    pub fn new(input: String) -> Self {
        Self {
            input,
            pos: 0,
            scopes: vec![Scope { variables: vec![] }],
        }
    }

    pub fn lookahead_1(&self) -> Option<char> {
        self.input.chars().nth(self.pos)
    }

    pub fn lookahead_2(&self) -> Option<char> {
        self.input.chars().nth(self.pos + 1)
    }

    pub fn lookahead_3(&self) -> Option<char> {
        self.input.chars().nth(self.pos + 2)
    }

    pub fn lookahead_4(&self) -> Option<char> {
        self.input.chars().nth(self.pos + 3)
    }

    pub fn proceed(&mut self) {
        self.pos += 1;
    }

    pub fn consume_1(&mut self, c: char) -> Result<(), String> {
        if self.lookahead_1() == Some(c) {
            self.proceed();
            Ok(())
        } else {
            Err(format!("Expected {}, but got {}", c, self.lookahead_1().unwrap_or('\0')))
        }
    }

    pub fn consume(&mut self, s: &str) -> Result<(), String> {
        for c in s.chars() {
            if self.lookahead_1() == Some(c) {
                self.proceed()
            } else {
                return Err(format!("Expected {}, but got {}", c, self.lookahead_1().unwrap()));
            }
        }
        Ok(())
    }
    /*
    # Define Javascript-style comments.
    _WS <- super._WS (EOL_COMMENT / MULTILINE_COMMENT)?   ${_ => SKIP};
    EOL_COMMENT <- "//" (~[\n\r] .)* _WS;
    MULTILINE_COMMENT <- "/*" (~"*/" .)* "* /" _WS;
    */

    pub fn consume_whitespace(&mut self) {
        while let Some(c) = self.lookahead_1() {
            match c {
                ' ' | '\t' | '\r' | '\n' => self.proceed(),
                '/' => {
                    match self.lookahead_2() {
                        Some('/') => {
                            self.proceed();
                            self.proceed();
                            while let Some(c) = self.lookahead_1() {
                                if c == '\r' || c == '\n' {
                                    break;
                                } else {
                                    self.proceed();
                                }
                            }
                        }
                        Some('*') => {
                            self.proceed();
                            self.proceed();
                            while let Some(c) = self.lookahead_1() {
                                if c == '*' {
                                    self.proceed();
                                    if let Some(c) = self.lookahead_1() {
                                        if c == '/' {
                                            self.proceed();
                                            break;
                                        }
                                    }
                                } else {
                                    self.proceed();
                                }
                            }
                        }
                        _ => break,
                    }
                },
                _ => break,
            }
        }
    }

    pub fn lookahead_whitespace_nonident(&self) -> Result<(), String> {
        if let Some(c) = self.lookahead_1() {
            match c {
                ' ' | '\t' | '\r' | '\n' => {
                    Ok(())
                }
                '/' => {
                    match self.lookahead_2() {
                        Some('/') | Some('*') => Ok(()),
                        _ => Err(format!("Expected whitespace, but got {}", c)),
                    }
                }
                'A'..='Z' | 'a'..='z' | '_' => Err(format!("Expected whitespace, but got {}", c)),
                _ => Ok(()),
            }
        } else {
            Ok(())
            //Err("Expected whitespace, but got EOF".to_string())
        }
    }

/* 
    pub fn left_paren(&mut self) -> Result<(), String> {
        self.consume_1('(')?;
        Ok(self.consume_whitespace())
    }

    pub pub fn right_paren(&mut self) -> Result<(), String> {
        self.consume_1(')')?;
        Ok(self.consume_whitespace())
    }

    pub pub fn left_brace(&mut self) -> Result<(), String> {
        self.consume_1('{')?;
        Ok(self.consume_whitespace())
    }

    pub pub fn right_brace(&mut self) -> Result<(), String> {
        self.consume_1('}')?;
        Ok(self.consume_whitespace())
    }

    pub pub fn left_bracket(&mut self) -> Result<(), String> {
        self.consume_1('[')?;
        Ok(self.consume_whitespace())
    }

    pub pub fn right_bracket(&mut self) -> Result<(), String> {
        self.consume_1(']')?;
        Ok(self.consume_whitespace())
    }
*/
    pub fn attempt<T>(&mut self, f: impl Fn(&mut Self) -> Result<T, String>) -> Result<T, String> {
        let pos = self.pos;
        match f(self) {
            Ok(r) => Ok(r),
            Err(err) => {
                self.pos = pos;
                Err(err)
            }
        }
    }

    pub fn prevent<T>(&mut self, f: impl Fn(&mut Self) -> Result<T, String>) -> Result<(), String> {
        let pos = self.pos;
        let result = match f(self) {
            Ok(_) => Err("Expected error, but got success".to_string()),
            Err(_) => Ok(()),
        };
        self.pos = pos;
        result
    }
/* 
    // used to parse variable names in context of declarations
    pub pub fn def_var(&mut self) -> Result<String, String> {

    }

    // used to parse variable names in context of reference
    pub pub fn use_var(&mut self) -> Result<String, String> {

    }

    // push one scope level
    pub pub fn push_scope(&mut self) {

    }

    // pop one scope level, returning the innermost scope constructed
    pub pub fn pop_scope(&mut self) -> Scope {

    }
    */
}

// comma seperated list of elements, with optional trailing comma
pub fn repeated_elements<Data: Debug>(state: &mut ParserState, open: char, close: char, element: &impl Fn(&mut ParserState) -> Result<Data, String>, trailing: bool) -> Result<Vec<Data>, String> {
    let mut elements = Vec::new();
    state.consume_1(open)?;
    loop { // I don't like having loop here
        println!("loop {:?}", elements);
        state.consume_whitespace();
        if state.lookahead_1() == Some(close) {
            state.proceed();
            break;
        }
        println!("element start");
        println!("{:?}{:?}", state.lookahead_1(), state.lookahead_2());
        elements.push(element(state)?);
        println!("element end");
        state.consume_whitespace();
        if let Some(c) = state.lookahead_1() {
            if c == ',' {
                if state.lookahead_2() == Some(close) {
                    if trailing {
                        state.proceed();
                        state.proceed();
                        break;
                    } else {
                        return Err(format!("Unexpected trailing comma in {}", open));
                    }
                } else {
                    state.proceed();
                }
            } else if c == close {
                continue
            } else {
                return Err(format!("Unexpected token: {}", c));
            } 
        }
    }

    Ok(elements)
}