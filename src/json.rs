use crate::rule::{Rule, ParserState, Parser};

#[derive(Debug, PartialEq, Clone)]
pub enum PureExpr<R: Rule> {
    DataLiteral(DataLiteral),
    Array(Array<R>),
    Record(Record<R>),
    Hole,

    // ParenedExpr(Box<PureExpr<R>>)
}

pub fn pure_expr<R: Rule>(state: &mut ParserState) -> Result<PureExpr<R>, String> {
    match state.lookahead_1() {
        Some('n') => {
            parse_null(state).map(PureExpr::DataLiteral)
        }
        Some('f') => {
            parse_false(state).map(PureExpr::DataLiteral)
        }
        Some('t') => {
            parse_true(state).map(PureExpr::DataLiteral)
        }
        Some('0'..='9') => {
            parse_number(state).map(PureExpr::DataLiteral)
        }
        Some('"') => {
            parse_string(state).map(PureExpr::DataLiteral)
        }
        Some('[') => {
            array(state).map(PureExpr::Array)
        }
        Some('{') => {
            record(state).map(PureExpr::Record)
        }
        Some(',') | Some(']') => Ok(PureExpr::Hole), // Not sure if this is correct
        Some(x) => Err(format!("Unexpected token: {}", x)),
        None => Err("Unexpected EOF".to_string()),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum DataStructure<R: Rule> {
    // Undefined is in Justin, but since we are not trying to 
    // make a modular parser, we will just put it here.
    Undefined, 
    DataLiteral(DataLiteral),
    Array(Array<R>),
    Record(Record<R>),
    Hole,
}

pub fn data_structure<R: Rule>(state: &mut ParserState) -> Result<DataStructure<R>, String> {
    let result = match state.lookahead_1() {
        Some('u') => {
            if R::pure_json_rule() {
                return Err("undefined in JSON parsing".to_string());
            }
            parse_undefined(state).map(|_| DataStructure::Undefined)
        }
        Some('n') | Some('f') | Some('t') | Some('0'..='9') | Some('"') => {
            data_literal(state).map(DataStructure::DataLiteral)
        }
        Some('[') => {
            array(state).map(DataStructure::Array)
        }
        Some('{') => {
            record(state).map(DataStructure::Record)
        }
        Some(x) => Err(format!("Unexpected token: {}", x)),
        None => Err("Unexpected EOF".to_string()),
    };

    state.consume_whitespace();

    result
}

#[derive(Debug, PartialEq, Clone)]
pub enum DataLiteral {
    Null,
    False,
    True,
    Number(String),
    String(String),
}

fn parse_undefined(state: &mut ParserState) -> Result<(), String> {
    state.consume("undefined")?;
    state.lookahead_whitespace_nonident()?;
    state.consume_whitespace();
    Ok(())
}

fn parse_null(state: &mut ParserState) -> Result<DataLiteral, String> {
    state.consume("null")?;
    state.lookahead_whitespace_nonident()?;
    state.consume_whitespace();
    Ok(DataLiteral::Null)
}

fn parse_false(state: &mut ParserState) -> Result<DataLiteral, String> {
    state.consume("false")?;
    state.lookahead_whitespace_nonident()?;
    state.consume_whitespace();
    Ok(DataLiteral::False)
}

fn parse_true(state: &mut ParserState) -> Result<DataLiteral, String> {
    state.consume("true")?;
    state.lookahead_whitespace_nonident()?;
    state.consume_whitespace();
    Ok(DataLiteral::True)
}

fn parse_number(state: &mut ParserState) -> Result<DataLiteral, String> {
    let mut number = String::new();
    while let Some(c) = state.lookahead_1() {
        if c.is_ascii_digit() {
            number.push(c);
            state.proceed();
        } else {
            break;
        }
    }
    state.consume_whitespace();
    Ok(DataLiteral::Number(number))
}

fn parse_string(state: &mut ParserState) -> Result<DataLiteral, String> {
    let mut string = String::new();
    let enclosing = state.lookahead_1().filter(|c| *c == '"' || *c == '\'').ok_or("Expected string".to_string())?;
    state.proceed();
    while let Some(c) = state.lookahead_1() {
        if c == enclosing {
            state.proceed();
            break;
        } else {
            string.push(c); // TODO: optimize
            state.proceed()
        }
    }
    state.consume_whitespace();
    Ok(DataLiteral::String(string))
}

pub fn data_literal(state: &mut ParserState) -> Result<DataLiteral, String> {
    match state.lookahead_1() {
        Some('n') => {
            parse_null(state)
        }
        Some('f') => {
            parse_false(state)
        }
        Some('t') => {
            parse_true(state)
        }
        Some('0'..='9') => {
            parse_number(state)
        }
        Some('"') => {
            parse_string(state)
        }
        Some(x) => Err(format!("Unexpected token: {}", x)),
        None => Err("Unexpected EOF".to_string()),
    }
}

pub fn repeated_elements<Data>(state: &mut ParserState, open: char, close: char, element: &impl Fn(&mut ParserState) -> Result<Data, String>, trailing: bool) -> Result<Vec<Data>, String> {
    let mut elements = Vec::new();
    state.consume_1(open)?;
    loop { // I don't like having loop here
        println!("loop");
        state.consume_whitespace();
        if state.lookahead_1() == Some(close) {
            state.proceed();
            break;
        }
        println!("element start");
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

#[derive(Debug, PartialEq, Clone)]
pub struct PureArray<R: Rule> {
    pub elements: Vec<PureExpr<R>>,
}

fn pure_array<R: Rule>(state: &mut ParserState) -> Result<PureArray<R>, String> {
    repeated_elements(state, '[', ']', &pure_expr, false).map(|elements| PureArray { elements })
}

#[derive(Debug, PartialEq, Clone)]
pub struct Array<R: Rule> {
    pub elements: Vec<R::Element>,
}

fn array<R: Rule>(state: &mut ParserState) -> Result<Array<R>, String> {
    repeated_elements(state, '[', ']', &R::element, !R::pure_json_rule()).map(|elements| Array { elements })
}

#[derive(Debug, PartialEq, Clone)]
pub struct PureRecord<R: Rule> {
    pub properties: Vec<PurePropDef<R>>,
}

fn pure_record<R: Rule>(state: &mut ParserState) -> Result<PureRecord<R>, String> {
    repeated_elements(state, '{', '}', &pure_prop_def, false).map(|properties| PureRecord { properties })
}

#[derive(Debug, PartialEq, Clone)]
pub struct Record<R: Rule> {
    pub properties: Vec<R::PropDef>,
}

fn record<R: Rule>(state: &mut ParserState) -> Result<Record<R>, String> {
    repeated_elements(state, '{', '}', &R::prop_def, !R::pure_json_rule()).map(|properties| Record { properties })
}

#[derive(Debug, PartialEq, Clone)]
pub struct PurePropDef<R: Rule> {
    pub key: R::PropName,
    pub value: PureExpr<R>,
}

fn pure_prop_def<R: Rule>(state: &mut ParserState) -> Result<PurePropDef<R>, String> {
    let key = R::prop_name(state)?;
    state.consume_whitespace();
    state.consume(":")?;
    state.consume_whitespace();
    let value = pure_expr(state)?;
    Ok(PurePropDef { key, value })
}

#[derive(Debug, PartialEq, Clone)]
pub struct PropDef<R: Rule> {
    pub key: R::PropName,
    pub value: R::Expr,
}

fn prop_def<R: Rule>(state: &mut ParserState) -> Result<PropDef<R>, String> {
    let key = R::prop_name(state)?;
    state.consume_whitespace();
    state.consume(":")?;
    state.consume_whitespace();
    let value = R::expr(state)?;
    Ok(PropDef { key, value })
}