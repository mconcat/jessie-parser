pub mod json;
pub mod jessie;
pub mod rule;

#[cfg(test)]
mod tests {
    use crate::jessie::PropDef;
    use crate::json::*;
    use crate::jessie::*;
    use crate::rule::*;
    
    fn expr_test_cases() -> Vec<(&'static str, Expr<JessieRule>)> {
        vec![
        ("undefined", Expr::new_data_structure(DataStructure::Undefined)),
        ("3", Expr::new_number(3)),
        ("5+6", Expr::new_add(Expr::new_number(5), Expr::new_number(6))),
        ("function f(x) { return x; }", Expr::FunctionExpr(Box::new(FunctionExpr(
            Some("f".to_string()),
            vec![Param::Pattern(Pattern::Variable("x".to_string()))],
            Block{ statements: vec![StatementItem::Statement(Statement::Terminator(Terminator::Return(Some(Expr::Variable("x".to_string())))))] },
        )))),
        ("function f(x, y) {
            return x+y;   
        }", Expr::FunctionExpr(Box::new(FunctionExpr(
            Some("f".to_string()), 
            vec![Param::Pattern(Pattern::Variable("x".to_string())), Param::Pattern(Pattern::Variable("y".to_string()))], 
            Block{ statements: vec![StatementItem::Statement(Statement::Terminator(Terminator::Return(Some(Expr::new_add(Expr::Variable("x".to_string()), Expr::Variable("y".to_string()))))))] })))),
        ("function f(x, [y, z]) {
            let t = x+y;
            return z;
        }", Expr::FunctionExpr(Box::new(FunctionExpr(
            Some("f".to_string()), 
            vec![
                Param::Pattern(Pattern::Variable("x".to_string())),
                Param::Pattern(Pattern::BindingPattern(BindingPattern::ArrayPattern(vec![Param::Pattern(Pattern::Variable("y".to_string())), Param::Pattern(Pattern::Variable("z".to_string()))]))),    
            ], 
            Block{ statements: vec![
                StatementItem::Declaration(Declaration { kind: DeclarationKind::Let, bindings: vec![Binding::VariableBinding("t".to_string(), Some(Expr::new_add(Expr::Variable("x".to_string()), Expr::Variable("y".to_string()))))] }),
                StatementItem::Statement(Statement::Terminator(Terminator::Return(Some(Expr::Variable("z".to_string()))))),
            ] })))),
        ("[3, v, true, {}, ...g]", Expr::DataStructure(Box::new(DataStructure::Array(Array { elements: 
            vec![
                Element::Expr(Expr::new_number(3)),
                Element::Expr(Expr::Variable("v".to_string())),
                Element::Expr(Expr::new_data_structure(DataStructure::DataLiteral(DataLiteral::True))),
                Element::Expr(Expr::new_data_structure(DataStructure::Record(Record { properties: vec![] }))),
                Element::Spread(Expr::Variable("g".to_string())),
            ]
         })))),
         ("{x: y, t    : [p], ...o, short}", Expr::DataStructure(Box::new(DataStructure::Record(Record { properties: 
            vec![
                PropDef::KeyValue("x".to_string(), Expr::Variable("y".to_string())),
                PropDef::KeyValue("t".to_string(), Expr::new_data_structure(DataStructure::Array(Array { elements: vec![Element::Expr(Expr::Variable("p".to_string()))] }))),
                PropDef::Spread(Expr::Variable("o".to_string())),
                PropDef::Shorthand("short".to_string()),
            ]
         }))))
        ]
    }

    #[test]
    fn it_works() {
        expr_test_cases().iter().for_each(|case| {
            println!("===========");
            println!("test for {}", case.0);
            let mut state = ParserState::new(case.0.to_string());
            let result = expr::<JessieRule>(&mut state);
            assert_eq!(result, Ok(case.1.clone()));
        });
    }
}