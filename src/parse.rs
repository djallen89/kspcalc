//! Tokenizes strings.

use std::str::FromStr;
pub type MaybeToken<T, U> = (Option<Result<T, U>>, usize);
pub type ErrorKind = String;

pub struct TokenStream<T, U> {
    expr: String,
    index: usize,
    rules: Vec<fn(&str) -> MaybeToken<T, U>>,
    on_err: U,
}

impl<T, U: Clone> TokenStream<T, U> {
    pub fn new(e: String, rules: Vec<fn(&str) -> MaybeToken<T, U>>,
               on_err: U) -> TokenStream<T, U> {
        TokenStream { expr: e, index: 0, rules: rules, on_err: on_err }
    }

    pub fn peek(&self) -> Option<Result<T, U>> {
        self.peek_helper(0)
    }

    fn peek_helper(&self, j: usize) -> Option<Result<T, U>> {
        if self.index + j == self.expr.len() {
            return None
        }
        
        if self.expr[self.index ..].chars().next().unwrap().is_whitespace() {
            self.peek_helper(j + 1)
        } else {
            let (token, _) = analyze(&self.expr[self.index + j ..], &self.rules, &self.on_err);
            token
        }
    }

    pub fn rev(&mut self, i: usize) -> Result<(), ()> {
        if self.index >= i {
            self.index -= 1;
            Ok(())
        } else {
            Err(())
        }
    }

    pub fn expr(&self) -> String {
        self.expr.clone()
    }

    pub fn rules<'a>(&'a self) -> &'a [fn(&str) -> MaybeToken<T, U>] {
        self.rules.as_slice()
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn on_err(&self) -> U {
        self.on_err.clone()
    }
}

impl<T, U: Clone> Iterator for TokenStream<T, U> {
    type Item = Result<T, U>;
    fn next(&mut self) -> Option<Result<T, U>> {
        if self.index == self.expr.len() {
            return None
        } else {
            if self.expr[self.index ..].chars().next().unwrap().is_whitespace() {
                self.index += 1;
                self.next()
            } else {
                let (token, len) = analyze(&self.expr[self.index..], &self.rules, &self.on_err);                  
                self.index += len;
                token
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        if self.index == self.expr.len() {
            (0, None)
        } else {
            (1, Some(self.expr.len() - self.index))
        }
    }
}

pub fn analyze<T, U: Clone>(expr: &str, funs: &[fn(&str) -> MaybeToken<T, U>], on_err: &U) -> MaybeToken<T, U> {
    for &fun in funs.iter() {
        let (token, len) = fun(expr);
        if token.is_some() {
            return (token, len)
        }
    }
    (Some(Err(on_err.clone())), 0)
}

#[derive(Clone, Debug)]
pub enum Token {
    Literal(f64),
//    Colon,
    Operator(super::RocketScience),
    Variable(String),
}

pub fn make_word(expr: &str) -> usize {
    expr.find(|c: char| c.is_whitespace() || /*c == ':' || */ c == ',').unwrap_or(expr.len())
}
/*
pub fn is_colon(expr: &str) -> MaybeToken<Token, ErrorKind> {
    match expr.chars().next().unwrap() {
        ':' => (Some(Ok(Token::Colon)), 1),
        _   => (None, 0)
    }
}
*/
pub fn is_op(expr: &str) -> MaybeToken<Token, ErrorKind> {
    let word_len = make_word(expr);
    match super::RocketScience::from_str(&expr[0 .. word_len]) {
        Ok(op) => (Some(Ok(Token::Operator(op))), word_len),
        _      => (None, 0)
    }
}

pub fn is_var(expr: &str) -> MaybeToken<Token, ErrorKind> {
    let word_len = make_word(expr);
    if expr.chars().next().unwrap().is_alphabetic() {
        (Some(Ok(Token::Variable(expr[0 .. word_len].to_string()))), word_len)
    } else {
        (None, 0)
    }
}

pub fn is_number(expr: &str) -> MaybeToken<Token, ErrorKind> {
    let word_len = make_word(expr);
    match f64::from_str(&expr[0 .. word_len]) {
        Ok(x)  => (Some(Ok(Token::Literal(x))), word_len),
        Err(_) => (None, 0)
    }
}
