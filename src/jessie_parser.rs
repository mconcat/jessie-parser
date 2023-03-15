use crate::parser::{ParserState, repeated_elements};
use crate::{jessie_types::*, json_types};
use crate::json_parser;
use crate::jessie_operation::*;

pub fn module_body(state: &mut ParserState) -> Result<ModuleBody, String> {
    let mut items = Vec::new();

    while let Some(_) = state.lookahead_1() {
        items.push(module_item(state)?);
        state.consume_whitespace();
    }

    Ok(ModuleBody(items))
}

pub fn module_item(state: &mut ParserState) -> Result<ModuleItem, String> {
    module_decl(state).map(ModuleItem::ModuleDecl) // TODO
}

pub fn module_decl(state: &mut ParserState) -> Result<ModuleDecl, String> {
    state.consume("const")?;
    repeated_elements(state, ' ', ';', &module_binding, false).map(ModuleDecl)
}

pub fn hardened_expr(state: &mut ParserState) -> Result<HardenedExpr, String> {
    Ok(HardenedExpr(expr(state)?))
}

pub fn module_binding(state: &mut ParserState) -> Result<ModuleBinding, String> {
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
            let ident = variable(state)?;
            state.consume_whitespace();
            state.consume("=")?;
            state.consume_whitespace();
            let expr = state.attempt(hardened_expr).ok();
            Ok(ModuleBinding::VariableBinding(ident, expr))
        }
    }
}

pub fn binding_pattern(state: &mut ParserState) -> Result<Pattern, String> {
    match state.lookahead_1() {
        Some('[') => repeated_elements(state, '[', ']', &param, false).map(|x| Pattern::ArrayPattern(x, optional_type_ann(state))),
        Some('{') => repeated_elements(state, '{', '}', &prop_param, false).map(|x| Pattern::RecordPattern(x, optional_type_ann(state))),
        _ => Err("Expected binding pattern".to_string()),
    }
}

// only parses original "pattern" rule from Jessica, not the entire variants of enum Pattern.
// consider changing the name to binding_or_ident_pattern or something...
pub fn pattern(state: &mut ParserState) -> Result<Pattern, String> {
    match state.lookahead_1() {
        Some('[') | Some('{') => binding_pattern(state),
        Some(',') | Some(']') => Ok(Pattern::Hole), // Not sure if its the right way...
        _ => // data_literal(state).map(|x| Pattern::DataLiteral(x)).or_else(|_| {
            ident(state).map(|x| Pattern::Variable(x, optional_type_ann(state)))
        //}),
    }
}

pub fn param(state: &mut ParserState) -> Result<Pattern, String> {
    if state.lookahead_1() == Some('.') {
        state.consume("...")?;
        state.consume_whitespace();
        return pattern(state).map(|x| Pattern::Rest(Box::new(x), optional_type_ann(state)))
    }

    let pat = pattern(state)?;
    state.consume_whitespace();
    if let Pattern::Variable(ref x, ref ann) = pat {
        if ann.is_some() {
            unimplemented!("Type annotations on parameters are not supported yet")
        }
        if state.lookahead_1() == Some('=') {
            state.consume("=")?;
            state.consume_whitespace();
            let expr = expr(state)?;
            return Ok(Pattern::Optional(x.clone(), Box::new(expr), optional_type_ann(state)))
        }
    }

    Ok(pat)
}

pub fn prop_param(state: &mut ParserState) -> Result<PropParam, String> {
    if state.lookahead_1() == Some('.') {
        state.consume("...")?;
        state.consume_whitespace();
        return pattern(state).map(|x| PropParam::Rest(x))
    }

    let key = ident(state)?;
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

pub fn expr(state: &mut ParserState) -> Result<Expr, String> {
    state.attempt(arrow_func).map(|x| Expr::ArrowFunc(Box::new(x))).or_else(|_| 
        state.attempt(function_expr).map(|x| Expr::FunctionExpr(Box::new(x))).or_else(|_| 
            state.attempt(assignment).map(|x| Expr::Assignment(Box::new(x))).or_else(|_| 
                state.attempt(cond_expr).or_else(|_| 
                    primary_expr(state)
                )
            )
        )
    )
}

pub fn assignment(state: &mut ParserState) -> Result<Assignment, String> {
    println!("assignment");
    let lvalue = lvalue(state)?;
    state.consume_whitespace();
    let op = assign_op(state)?;
    state.consume_whitespace();
    let expr = expr(state)?;
    Ok(Assignment(lvalue, op, expr))
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
            match state.lookahead_2() {
                Some('*') => {
                    state.consume("**=")?;
                    Ok(AssignOp::AssignExp)
                },
                Some('=') => {
                    state.consume("*=")?;
                    Ok(AssignOp::AssignMul)
                },
                _ => Err("sadf".to_string())
            }
        },
        Some('/') => {
            state.consume("/=")?;
            Ok(AssignOp::AssignDiv)
        },
        Some('%') => {
            state.consume("%=")?;
            Ok(AssignOp::AssignMod)
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

// TODO: combine paren expr and arrowfunc in single consume("(")?
pub fn pure_expr(state: &mut ParserState) -> Result<Expr, String> {
    state.attempt(arrow_func).map(|x| Expr::ArrowFunc(Box::new(x))).or_else(|_| 
        state.attempt(json_parser::pure_expr).map(|x| x.into()).or_else(|_| 
            state.attempt(|state| {
                state.consume("(")?;
                state.consume_whitespace();
                let expr = pure_expr(state)?;
                state.consume_whitespace();
                state.consume(")")?;
                Ok(Expr::ParenedExpr(Box::new(expr)))
            }).or_else(|_| 
                ident(state).map(|x| Expr::Variable(x))
            )
        )
    )
}

pub fn lvalue(state: &mut ParserState) -> Result<LValue, String> {
    let lval = primary_expr(state)?;

    match state.lookahead_1() {
        Some('[') => {
            state.consume("[")?;
            state.consume_whitespace();
            let index = expr(state)?;
            state.consume_whitespace();
            state.consume("]")?;
            Ok(LValue::Index(lval, index))
        },
        Some('.') => {
            state.consume(".")?;
            state.consume_whitespace();
            let name = ident(state)?;
            Ok(LValue::Member(lval, name))
        },
        _ => {
            match lval {
                // Expr::Variable(name) => Ok(LValue::Variable(name)), // TODO
                _ => Err("Expected lvalue".to_string()),
            }
        }
    }
}

pub fn statement(state: &mut ParserState) -> Result<Statement, String> {
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

pub fn expr_statement(state: &mut ParserState) -> Result<Expr, String> {
    // TODO: cantStartExprStatement
    let result = expr(state);
    state.consume_whitespace();
    state.consume(";")?;
    result
}

pub fn block(state: &mut ParserState) -> Result<Block, String> {
    state.consume("{")?;
    state.consume_whitespace();

    let mut statements = Vec::new();
    while state.lookahead_1() != Some('}') { // is it memory safe??
        statements.push(statement_item(state)?);
        state.consume_whitespace();
    }

    Ok(Block::new(statements))
}

pub fn if_statement(state: &mut ParserState) -> Result<IfStatement, String> {
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

pub fn breakable_statement(state: &mut ParserState) -> Result<BreakableStatement, String> {
    state.attempt(while_statement).map(|x| BreakableStatement::WhileStatement(x))
}

pub fn while_statement(state: &mut ParserState) -> Result<WhileStatement, String> {
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

pub fn terminator(state: &mut ParserState) -> Result<Terminator, String> {
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
                Ok::<Option<Expr>, String>(Some(arg))
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

pub fn statement_item(state: &mut ParserState) -> Result<StatementItem, String> {
    state.attempt(declaration).map( StatementItem::Declaration).or_else(|_| 
        state.attempt(function_decl).map(StatementItem::FunctionDecl).or_else(|_| 
            state.attempt(statement).map( StatementItem::Statement)
        )
    )
}

pub fn declaration(state: &mut ParserState) -> Result<Declaration, String> {
    let kind = match state.lookahead_1() {
        Some('l') => state.consume("let").map(|_| DeclarationKind::Let),
        Some('c') => state.consume("const").map(|_| DeclarationKind::Const),
        _ => Err("Expected 'let' or 'const'".to_string()),
    }?;

    let bindings = repeated_elements(state, ' ', ';', &binding, false)?; 

    Ok(Declaration { kind, bindings })
}

pub fn binding(state: &mut ParserState) -> Result<Binding, String> {
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
            let name = ident(state)?;
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
                Some('o') => match state.lookahead_4() {
                    Some('s') => state.consume("const").map(|_| Keyword::Const),
                    Some('t') => state.consume("continue").map(|_| Keyword::Continue),
                    _ => Err("Expected 'const' or 'continue'".to_string()),
                },
                _ => Err("Expected 'case', 'catch', 'const', or 'continue'".to_string()),
            }
        },
        Some('d') => {
            match state.lookahead_3() {
                Some('b') => state.consume("debugger").map(|_| Keyword::Debugger),
                Some('f') => state.consume("default").map(|_| Keyword::Default),
                _ => Err("Expected 'debugger' or 'default'".to_string()),
            }
        },
        Some('e') => {
            match state.lookahead_2() {
                Some('l') => state.consume("else").map(|_| Keyword::Else),
                Some('x') => state.consume("export").map(|_| Keyword::Export),
                _ => Err("Expected 'else' or 'export'".to_string()),
            }
        },
        Some('f') => {
            match state.lookahead_2() {
                Some('i') => state.consume("finally").map(|_| Keyword::Finally),
                Some('o') => state.consume("for").map(|_| Keyword::For),
                Some('u') => state.consume("function").map(|_| Keyword::Function),
                _ => Err("Expected 'finally', 'for', or 'function'".to_string()),
            }
        },
        Some('i') => {
            match state.lookahead_2() {
                Some('f') => state.consume("if").map(|_| Keyword::If),
                Some('m') => state.consume("import").map(|_| Keyword::Import),
                _ => Err("Expected 'if' or 'import'".to_string()),
            }
        },
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

pub fn future_reserved_word(state: &mut ParserState) -> Result<FutureReservedWord, String> {
    match state.lookahead_1() {
        Some('a') => state.consume("await").map(|_| FutureReservedWord::Await),
        Some('e') => state.consume("enum").map(|_| FutureReservedWord::Enum),
        Some('i') => {
            match state.lookahead_2() {
                Some('m') => state.consume("implements").map(|_| FutureReservedWord::Implements),
                Some('n') => state.consume("interface").map(|_| FutureReservedWord::Interface),
                _ => Err("Expected 'implements' or 'interface'".to_string()),
            }
        },
        Some('p') => {
            match state.lookahead_2() {
                Some('a') => state.consume("package").map(|_| FutureReservedWord::Package),
                Some('r') => state.consume("protected").map(|_| FutureReservedWord::Protected),
                // TODO: private
                Some('u') => state.consume("public").map(|_| FutureReservedWord::Public), 
                _ => Err("Expected 'package' or 'protected'".to_string()),
            }
        },
        _ => Err("Expected a future reserved word".to_string()),
    }   
}

fn array(state: &mut ParserState) -> Result<Array, String> {
    repeated_elements(state, '[', ']', &element, false).map(|elements| Array(elements))
}

fn record(state: &mut ParserState) -> Result<Record, String> {
    repeated_elements(state, '{', '}', &prop_def, false).map(|properties| Record(properties))
}

pub fn data_literal(state: &mut ParserState) -> Result<Expr, String> {
    let lit = json_parser::data_literal(state)?.into();
    Ok(Expr::DataLiteral(match lit {
        DataLiteral::Number(ref n) => {
            if state.lookahead_1() == Some('n') {
                state.proceed();
                DataLiteral::Bigint(n.clone())
            } else {
                lit
            }
        }
        _ => lit
    }))
}

pub fn data_structure(state: &mut ParserState) -> Result<Expr, String> {
    let result = match state.lookahead_1() {
        Some('n') | Some('f') | Some('t') | Some('0'..='9') | Some('"') => {
            data_literal(state)
        }
        Some('[') => {
            array(state).map(Expr::Array)
        }
        Some('{') => {
            record(state).map(Expr::Record)
        }
        Some('u') => {
            state.consume("undefined").map(|_| Expr::DataLiteral(DataLiteral::Undefined))
        }
        None => Err("Unexpected EOF".to_string()),
        _ => Err("Expected a data literal, array, record, or undefined".to_string()),
    };

    state.consume_whitespace();

    result
}

pub fn primary_expr(state: &mut ParserState) -> Result<Expr, String> {
    state.attempt(data_structure).map(|data| data.into()).or_else(|_| {
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
            _ => Ok(ident(state).map(|x| {
                Expr::Variable(x)
            })?),
        }
    })
}

pub fn element(state: &mut ParserState) -> Result<Element, String> {
    if state.lookahead_1() == Some('.') {
        state.consume(".")?;
        state.consume(".")?;
        state.consume(".")?;
        state.consume_whitespace();
        let expr = expr(state)?;
        Ok(Element::Spread(expr))
    } else {
        let expr = expr(state)?;
        Ok(Element::Expr(expr))
    }
}

pub fn pure_prop_def(state: &mut ParserState) -> Result<PropDef, String> {
    if state.lookahead_1() == Some('.') {
        state.consume("...")?;
        state.consume_whitespace();
        let expr = expr(state)?;
        Ok(PropDef::Spread(expr))
    }
    // copilot generated these lines, didnt understand 
    /*  
    else if state.lookahead_1() == Some('[') {
        state.consume("[")?;
        state.consume_whitespace();
        let prop_name = prop_name(state)?;
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
        let prop_name = prop_name(state)?;
        state.consume_whitespace();
        if state.lookahead_1() == Some('(') {
            unimplemented!()
            /* 
            let method_def = method_def(state)?;
            Ok(PurePropDef::MethodDef(method_def))
            */
        } else {
            Ok(PropDef::Shorthand(prop_name))
        }
    }
}

pub fn prop_def(state: &mut ParserState) -> Result<PropDef, String> {
    if state.lookahead_1() == Some('.') {
        state.consume(".")?;
        state.consume(".")?;
        state.consume(".")?;
        state.consume_whitespace();
        let expr = expr(state)?;
        Ok(PropDef::Spread(expr))
    }
    // copilot generated these lines, didnt understand 
    /*  
    else if state.lookahead_1() == Some('[') {
        state.consume("[")?;
        state.consume_whitespace();
        let prop_name = prop_name(state)?;
        state.consume("]")?;
        state.consume_whitespace();
        state.consume(":")?;
        state.consume_whitespace();
        let expr = expr(state)?;
        Ok(PropDef::KeyValue(prop_name, expr))
    } else if state.lookahead_1() == Some('*') {
        state.consume("*")?;
        state.consume_whitespace();
        let prop_name = prop_name(state)?;
        state.consume(":")?;
        state.consume_whitespace();
        let expr = expr(state)?;
        Ok(PropDef::KeyValue(prop_name, expr))
    */
    else if state.lookahead_1() == Some('`') {
        Err("QuasiExpr not implemented".to_string())
    } else {
        let prop_name = prop_name(state)?;
        state.consume_whitespace();
        if state.lookahead_1() == Some(':') {
            state.consume(":")?;
            state.consume_whitespace();
            let expr = expr(state)?;
            Ok(PropDef::KeyValue(prop_name, expr))
        } else {
            Ok(PropDef::Shorthand(prop_name))
        }
    }
}

pub fn cond_expr(state: &mut ParserState) -> Result<Expr, String> {
    println!("condexpr");
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
pub fn or_else_expr(state: &mut ParserState) -> Result<Expr, String> {
    println!("orelseexpr");
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

pub fn and_then_expr(state: &mut ParserState) -> Result<Expr, String> {
    println!("andthenexpr");
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

pub fn eager_expr(state: &mut ParserState) -> Result<Expr, String> {
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

pub fn shift_expr(state: &mut ParserState) -> Result<Expr, String> {
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

pub fn add_expr(state: &mut ParserState) -> Result<Expr, String> {
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

pub fn mult_expr(state: &mut ParserState) -> Result<Expr, String> {
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

pub fn pow_expr(state: &mut ParserState) -> Result<Expr, String> {
    // TODO. for now just route to unaryexpr.
    Ok(unary_expr(state)?)
}

pub fn unary_expr(state: &mut ParserState) -> Result<Expr, String> {
    // TODO, for now just route to callexpr.
    call_expr(state)
}

pub fn call_post_op(state: &mut ParserState) -> Result<CallPostOp, String> {
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

pub fn call_expr(state: &mut ParserState) -> Result<Expr, String> { 
    println!("callexpr");
    let mut result = primary_expr(state)?;
    loop { // I don't like having an infinite loop here...
        match call_post_op(state) {
            Ok(op) => result = Expr::CallExpr(Box::new(CallExpr{ expr: result, post_op: op })),
            Err(_) => break,
        }
    }
    Ok(result)
}

pub fn member_post_op(state: &mut ParserState) -> Result<MemberPostOp, String> {
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
        let ident = ident(state)?;
        Ok(MemberPostOp::Member(ident))
    } else {
        Err("Expected '[' or '.'".to_string())
    }
}

pub fn arg(state: &mut ParserState) -> Result<Arg, String> {
    if state.lookahead_1() == Some('.') {
        state.consume(".")?;
        state.consume(".")?;
        state.consume(".")?;
        state.consume_whitespace();
        let expr = expr(state)?;
        Ok(Arg::Spread(expr))
    } else {
        let expr = expr(state)?;
        Ok(Arg::Expr(expr))
    }
}

pub fn args(state: &mut ParserState) -> Result<Vec<Arg>, String> {
    state.consume("(")?;
    let mut args = vec![];
    if state.lookahead_1() == Some(')') {
        return Ok(args);
    }
    loop {
        args.push(arg(state)?);
        // TODO: check EOF
        if state.lookahead_1() == Some(')') {
            break;
        }
        state.consume(",")?;
        state.consume_whitespace();
    }
    Ok(args)
}

pub fn function_decl(state: &mut ParserState) -> Result<Function, String> {
    state.consume("function")?;
    state.lookahead_whitespace_nonident()?;
    state.consume_whitespace();
    let name = ident(state)?;
    state.consume_whitespace();
    let params = repeated_elements(state, '(', ')', &param, false/*Check it*/)?;
    state.consume_whitespace();
    let body = block(state)?;
    Ok(Function(Some(name), params, optional_type_ann(state), body))
}

pub fn function_expr(state: &mut ParserState) -> Result<Function, String> {
    println!("function_expr");
    state.consume("function")?;
    println!("1");
    let name = state.attempt(|state| {
        state.lookahead_whitespace_nonident()?;
        state.consume_whitespace();
        ident(state)
    }).ok();
    println!("2");
    state.consume_whitespace();
    let params = repeated_elements(state, '(', ')', &param, false/*Check it*/)?;
    state.consume_whitespace();
    println!("3");
    let body = block(state)?;
    println!("4");
    Ok(Function(name, params, optional_type_ann(state), body))
}

pub fn arrow_func(state: &mut ParserState) -> Result<Function, String> {
    println!("arrow_func");
    let params = match state.lookahead_1() {
        Some('(') => repeated_elements(state, '(', ')', &param, false/*Check it*/)?,
        _ => Err("unimplemented")?,//TODO // ident(state).map(|x| vec![ArrowParam(x)]),
    };

    // TODO: no_newline
    state.consume_whitespace();
    state.consume("=>")?;
    state.consume_whitespace();

    let body = match state.lookahead_1() {
        Some('{') => block(state)?,
        _ => Err("unimplemented")?, // TODO: expr(state),
    };
    
    Ok(Function(None, params, optional_type_ann(state), body))
}

fn optional_type_ann(state: &mut ParserState) -> Option<TypeAnn> {
    None // TODO
}

fn variable(state: &mut ParserState) -> Result<Variable, String> {
    ident(state).map(|name| Variable { name })
}

fn prop_name(state: &mut ParserState) -> Result<PropName, String> {
    // TODO: datastructure propname
    ident(state).map(|name| PropName::Ident(name ))
}

fn ident(state: &mut ParserState) -> Result<String, String> {
    // could be optimized
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