use core::fmt::Debug;

pub type Parser<Expr> = fn(&mut ParserState) -> Result<Expr, String>;

pub struct ParserState {
    pub input: String,
    pub pos: usize,
}

impl ParserState {
    pub fn new(input: String) -> Self {
        Self { input, pos: 0 }
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

    pub fn lookahead_whitespace_nonident(&mut self) -> Result<(), String> {
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

    pub fn left_paren(&mut self) -> Result<(), String> {
        self.consume_1('(')?;
        Ok(self.consume_whitespace())
    }

    pub fn right_paren(&mut self) -> Result<(), String> {
        self.consume_1(')')?;
        Ok(self.consume_whitespace())
    }

    pub fn left_brace(&mut self) -> Result<(), String> {
        self.consume_1('{')?;
        Ok(self.consume_whitespace())
    }

    pub fn right_brace(&mut self) -> Result<(), String> {
        self.consume_1('}')?;
        Ok(self.consume_whitespace())
    }

    pub fn left_bracket(&mut self) -> Result<(), String> {
        self.consume_1('[')?;
        Ok(self.consume_whitespace())
    }

    pub fn right_bracket(&mut self) -> Result<(), String> {
        self.consume_1(']')?;
        Ok(self.consume_whitespace())
    }

    pub fn attempt<R>(&mut self, f: impl Fn(&mut Self) -> Result<R, String>) -> Result<R, String> {
        let pos = self.pos;
        match f(self) {
            Ok(r) => Ok(r),
            Err(err) => {
                self.pos = pos;
                Err(err)
            }
        }
    }

    pub fn prevent<R>(&mut self, f: impl Fn(&mut Self) -> Result<R, String>) -> Result<(), String> {
        let pos = self.pos;
        let result = match f(self) {
            Ok(_) => Err("Expected error, but got success".to_string()),
            Err(_) => Ok(()),
        };
        self.pos = pos;
        result
    }
}

pub trait Rule: Debug+PartialEq+Clone {
    fn pure_json_rule() -> bool;
    fn jessie_rule() -> bool;
    fn tessie_rule() -> bool;

    fn ident(state: &mut ParserState) -> Result<String, String>; // TODO: not string maybe?

    type Expr: Debug+PartialEq+Clone;
    fn expr(state: &mut ParserState) -> Result<Self::Expr, String>;

    type Element: Debug+PartialEq+Clone;
    fn element(state: &mut ParserState) -> Result<Self::Element, String>;

    type PurePropDef: Debug+PartialEq+Clone;
    type PropDef: Debug+PartialEq+Clone;
    type PropName: Debug+PartialEq+Clone;
    fn pure_prop_def(state: &mut ParserState) -> Result<Self::PurePropDef, String>;
    fn prop_def(state: &mut ParserState) -> Result<Self::PropDef, String>;
    fn prop_name(state: &mut ParserState) -> Result<Self::PropName, String>;

    type Variable: Debug+PartialEq+Clone;
    fn variable(state: &mut ParserState) -> Result<Self::Variable, String>;
}