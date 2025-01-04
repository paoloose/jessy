use std::{collections::HashMap, iter::Peekable, str::Chars};

#[derive(Debug)]
#[allow(dead_code)]
enum JsonValue {
    Boolean(bool),
    String(String),
    Number(f64),
    Array(Vec<JsonValue>),
    Object(HashMap<String, JsonValue>),
    Null,
}

struct Lexer<'a> {
    pub input: Peekable<Chars<'a>>,
}

#[derive(Debug)]
#[allow(dead_code)]
enum NextValueError {
    Eof,
    ParseError(String),
}

impl<'a> Lexer<'a> {
    fn next_whitespaces(&mut self) {
        loop {
            match self.input.peek() {
                Some(c) if c.is_ascii_whitespace() => {
                    self.input.next();
                },
                _ => return,
            }
        }
    }

    pub fn next_value(&mut self) -> Result<JsonValue, NextValueError> {
        self.next_whitespaces();

        match self.input.peek().copied() {
            Some(c) => match c {
                c if c == '-' || c.is_numeric() => {
                    match self.next_number() {
                        Ok(n) => Ok(JsonValue::Number(n)),
                        Err(e) => {
                            Err(NextValueError::ParseError(format!("parse number error: {:#?}", e)))
                        },
                    }
                },
                '{' => {
                    match self.next_object() {
                        Ok(obj) => Ok(JsonValue::Object(obj)),
                        Err(err) => Err(err),
                    }
                },
                '[' => {
                    match self.next_array() {
                        Ok(arr) => Ok(JsonValue::Array(arr)),
                        Err(err) => Err(err),
                    }
                },
                '"' => {
                    match self.next_string() {
                        Ok(s) => Ok(JsonValue::String(s)),
                        Err(err) => Err(err),
                    }
                },
                'n' => {
                    if self.expect_next("null") && self.validate_residuals() {
                        return Ok(JsonValue::Null);
                    }
                    Err(NextValueError::ParseError("Got invalid identifier 1".to_string()))
                },
                'f' => {
                    if self.expect_next("false") && self.validate_residuals() {
                        return Ok(JsonValue::Boolean(false));
                    }
                    Err(NextValueError::ParseError("Got invalid identifier 2".to_string()))
                },
                't' => {
                    if self.expect_next("true") && self.validate_residuals() {
                        return Ok(JsonValue::Boolean(true));
                    }
                    Err(NextValueError::ParseError("Got invalid identifier 3".to_string()))
                },
                _ => {
                    Err(NextValueError::ParseError(format!("unexpected character: {}", c)))
                },
            },
            None => Err(NextValueError::Eof),
        }
    }

    fn validate_residuals(&mut self) -> bool {
        self.next_whitespaces();
        let residual = self.input.peek().copied();
        matches!(residual, None | Some(',') | Some('}') | Some(']'))
    }

    fn next_array(&mut self) -> Result<Vec<JsonValue>, NextValueError> {
        self.input.next(); // consumes [
        let mut accepts_closing = true;
        let mut accepts_comma = false;
        let mut accepts_value = true;
        let mut last_was_comma = false;

        let mut values = vec![];

        loop {
            self.next_whitespaces();
            match self.input.peek().copied() {
                Some(']') => {
                    if last_was_comma {
                        return Err(NextValueError::ParseError("trailing comma".to_string()));
                    }
                    if !accepts_closing {
                        return Err(NextValueError::ParseError("unexpected ']'".to_string()));
                    }
                    self.input.next();
                    return Ok(values);
                }
                Some(',') => {
                    if !accepts_comma {
                        return Err(NextValueError::ParseError("unexpected comma".to_string()));
                    }
                    self.input.next();
                    accepts_closing = false;
                    accepts_comma = false;
                    accepts_value = true;
                    last_was_comma = true;
                    continue;
                },
                None => {
                    return Err(NextValueError::ParseError("unexpected EOF while parsing array".to_string()));
                },
                _ => {
                    if !accepts_value {
                        return Err(NextValueError::ParseError("unexpected value".to_string()));
                    }
                    match self.next_value() {
                        Ok(value) => {
                            values.push(value);
                            accepts_value = false;
                            accepts_comma = true;
                            accepts_closing = true;
                            last_was_comma = false;
                            continue;
                        },
                        Err(err) => return Err(err),
                    }
                },
            }
        }
    }

    fn next_object(&mut self) -> Result<HashMap<String, JsonValue>, NextValueError> {
        self.input.next(); // consumes [
        let mut accepts_closing = true;
        let mut accepts_comma = false;
        let mut accepts_key = true;
        let mut accepts_value = false;
        let mut last_was_comma = false;
        let mut last_was_colon = false;
        let mut last_key: String = "".into();

        let mut map = HashMap::new();

        loop {
            self.next_whitespaces();
            match self.input.peek().copied() {
                Some('}') => {
                    if last_was_comma {
                        return Err(NextValueError::ParseError("trailing comma".to_string()));
                    }
                    if !accepts_closing {
                        return Err(NextValueError::ParseError("unexpected '}'".to_string()));
                    }
                    self.input.next();
                    return Ok(map);
                },
                Some(',') => {
                    if !accepts_comma {
                        return Err(NextValueError::ParseError("unexpected comma".to_string()));
                    }
                    self.input.next();
                    accepts_closing = false;
                    accepts_comma = false;
                    accepts_value = false;
                    accepts_key = true;
                    last_was_comma = true;
                    last_was_colon = false;
                    continue;
                },
                Some(':') => {
                    if last_was_comma || !accepts_value || last_was_colon {
                        return Err(NextValueError::ParseError("unexpected colon".to_string()));
                    }
                    self.input.next();
                    last_was_colon = true;
                    continue;
                },
                None => {
                    return Err(NextValueError::ParseError("unexpected EOF while parsing array".to_string()));
                },
                _ => {
                    match self.next_value() {
                        Ok(JsonValue::String(key)) => {
                            if accepts_key {
                                last_key = key;
                                accepts_value = true;
                                accepts_key = false;
                                accepts_comma = false;
                                accepts_closing = false;
                            } else {
                                map.insert(last_key.clone(), JsonValue::String(key));
                                accepts_value = false;
                                accepts_key = true;
                                accepts_comma = true;
                                accepts_closing = true;
                            }
                            last_was_comma = false;
                            last_was_colon = false;
                            continue;
                        },
                        Ok(value) => {
                            if !accepts_value {
                                return Err(NextValueError::ParseError("unexpected value".to_string()));
                            }
                            map.insert(last_key.clone(), value);
                            accepts_value = false;
                            accepts_key = true;
                            accepts_comma = true;
                            accepts_closing = true;
                            last_was_comma = false;
                            last_was_colon = false;
                            continue;
                        },
                        Err(err) => return Err(err),
                    }
                },
            }
        }
    }

    fn next_string(&mut self) -> Result<String, NextValueError> {
        self.input.next();
        let mut s = "".to_string();

        loop {
            match self.input.next() {
                Some('"') => {
                    return Ok(s);
                },
                Some(c) => {
                    s.push(c);
                },
                None => {
                    return Err(NextValueError::ParseError("unexpected EOF while parsing string".to_string()));
                },
            }
        }
    }

    fn expect_next(&mut self, expect: &str) -> bool {
        let mut expect_iter = expect.chars().peekable();

        loop {
            let e = expect_iter.peek().copied();
            let c = self.input.peek().copied();

            if e == c {
                self.input.next();
                expect_iter.next();
                continue;
            }
            if e.is_none() {
                return true;
            }
            return false;
        }
    }

    fn next_number(&mut self) -> Result<f64, NextValueError> {
        let mut is_negative = false;
        let mut is_scientific_notation = false;
        let mut holds_value = false;
        let mut is_decimal_part = false;
        let mut is_positive_scientific_exponential = true;

        let mut num: f64 = 0.0;
        let mut decimal_mult: f64 = 1.0;
        let mut scientific_exponent: u32 = 0;

        loop {
            let next_char = match self.input.peek().copied() {
                Some(c) => c,
                None => {
                    return if holds_value {
                        break;
                    } else {
                        Err(NextValueError::ParseError("expected number got EOF".to_string()))
                    }
                }
            };

            match next_char {
                '.' => {
                    self.input.next();
                    if is_decimal_part {
                        return Err(NextValueError::ParseError("unexpected punto".to_string()));
                    }
                    is_decimal_part = true;
                    continue;
                },
                '-' => {
                    self.input.next();
                    if is_negative {
                        return Err(NextValueError::ParseError("double negative".to_string()));
                    }
                    is_negative = true;
                    continue;
                },
                'e' | 'E' => {
                    self.input.next();
                    match self.input.peek().copied() {
                        Some('+') => {
                            is_positive_scientific_exponential = true;
                            is_scientific_notation = true;
                            self.input.next();
                        },
                        Some(c) if c.is_numeric() => {
                            is_positive_scientific_exponential = true;
                            is_scientific_notation = true;
                        },
                        Some('-') => {
                            is_positive_scientific_exponential = false;
                            is_scientific_notation = true;
                            self.input.next();
                        },
                        _ => {
                            return Err(NextValueError::ParseError("bad science".to_string()));
                        },
                    }
                },
                c if c.is_numeric() => {
                    self.input.next(); // HERE
                    let d = c.to_digit(10).unwrap();
                    if is_scientific_notation {
                        scientific_exponent *= 10;
                        scientific_exponent += d;
                        continue;
                    }
                    num *= 10.0;
                    num += d as f64;
                    holds_value = true;
                    if is_decimal_part {
                        decimal_mult *= 0.1;
                    }
                },
                c if c == ',' || c == ']' || c == '}' || c.is_whitespace() => {
                    if holds_value {
                        break;
                    }
                    return Err(NextValueError::ParseError("expected number got EOF".to_string()));
                },
                c => {
                    return Err(NextValueError::ParseError(format!("unexpected character: {}", c)));
                }
            }
        };

        num = if is_negative { -num } else { num };
        if !is_scientific_notation {
            return Ok(num * decimal_mult);
        }

        let exponent = f64::powi(10.0, scientific_exponent as i32);

        if is_positive_scientific_exponential {
            Ok((num * decimal_mult) * exponent)
        } else {
            Ok((num * decimal_mult) / exponent)
        }
    }
}

fn main() {
    let input = include_str!("./data.json");

    let mut lexer = Lexer {
        input: input.chars().peekable(),
    };

    let parsed_value = lexer.next_value();
    println!("{:#?}", parsed_value);
}
