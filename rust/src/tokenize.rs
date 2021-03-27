use super::codereader::union_of;
use super::codereader::CodeReader;
use super::dlogger::DiagnosticLogger;
use super::token::*;
use lsp_types::Range;
use num_bigint::{BigInt, BigUint, Sign};
use num_rational::BigRational;
use num_traits::Zero;
use peekmore::{PeekMore, PeekMoreIterator};
use std::char::from_u32;

pub struct Tokenizer<Source: Iterator<Item = u8>> {
  // we need to peek deep into the codereader in order to figure out
  // what we're looking at
  source: PeekMoreIterator<CodeReader<Source>>,
  dlogger: DiagnosticLogger,
}

pub fn tokenize<IntoSource: IntoIterator<Item = u8>>(
  source: IntoSource,
  dlogger: DiagnosticLogger,
) -> impl Iterator<Item = Token> {
  Tokenizer {
    source: CodeReader::new(source.into_iter()).peekmore(),
    dlogger,
  }
}

impl<Source: Iterator<Item = u8>> Tokenizer<Source> {
  // skips all whitespace
  fn skip_whitespace(&mut self) {
    while let Some((b' ' | b'\t' | b'\r' | b'\n', _)) = self.source.peek() {
      self.source.next();
    }
  }

  // skips indentation, but not new lines
  fn skip_indentation(&mut self) {
    while let Some((b' ' | b'\t', _)) = self.source.peek() {
      self.source.next();
    }
  }


  // requires that there is at least one valid token
  fn internal_lex_word(&mut self) -> (Vec<u8>, Range) {
    assert!(matches!(
      self.source.peek(),
      Some((b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_', _))
    ));

    let mut word = vec![];
    let mut range = None;
    while let Some((
        c @ (b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_'),
        r
    )) = self.source.peek() {
      word.push(*c);
      // handle range
      if let Some(or) = range {
        range = Some(union_of(*r, or));
      } else {
        range = Some(*r);
      }
      self.source.next();
    }

    (word, range.unwrap())
  }

  // lexes a keyword or identifier
  fn lex_identifier_or_keyword(&mut self) -> Token {
    let (word, range) = self.internal_lex_word();

    let tk = match word.as_slice() {
      b"loop" => TokenKind::Loop,
      b"true" => TokenKind::Bool(true),
      b"false" => TokenKind::Bool(false),
      b"bool" => TokenKind::BoolType,
      b"val" => TokenKind::Val,
      b"pat" => TokenKind::Pat,
      b"case" => TokenKind::Case,
      b"of" => TokenKind::Of,
      b"ret" => TokenKind::Ret,
      b"defer" => TokenKind::Defer,
      b"this" => TokenKind::This,
      b"as" => TokenKind::As,
      b"inf" => TokenKind::Inf,
      b"nan" => TokenKind::Nan,
      b"in" => TokenKind::In,
      b"dyn" => TokenKind::Dyn,
      b"impl" => TokenKind::Impl,
      b"where" => TokenKind::Where,
      b"and" => TokenKind::And,
      b"or" => TokenKind::Or,
      b"nil" => TokenKind::NilType,
      b"never" => TokenKind::NeverType,
      b"if" => TokenKind::If,
      b"then" => TokenKind::Then,
      b"else" => TokenKind::Else,
      b"async" => TokenKind::Async,
      b"await" => TokenKind::Await,
      b"import" => TokenKind::Import,
      _ => TokenKind::Identifier(String::from_utf8(word).unwrap()),
    };

    Token::new(tk, range)
  }

  // lexes a label
  fn lex_label(&mut self) -> Token {
    assert!(matches!(self.source.peek(), Some((b'\'', _))));

    // handle initial apostrophe
    let (_, ar) = self.source.next().unwrap();

    // parse label body
    let (word, range) = self.internal_lex_word();

    // return label
    Token::new(TokenKind::Label(String::from_utf8(word).unwrap()), union_of(ar, range))
  }

  // requires at least one character exists...
  fn internal_lex_base_number(&mut self, radix: u8, max_len: Option<u32>) -> (BigUint, Range, u32) {
    let mut range = self.source.peek().unwrap().1;

    let mut len = 0u32;
    let mut n = BigUint::zero();

    while let Some((c, r)) = self.source.peek().cloned() {
      let mut digit = match c {
        b'_' => {
          self.source.next();
          continue;
        }
        b'0'..=b'9' => {
          self.source.next();
          len += 1;
          c - b'0'
        }
        b'A'..=b'Z' => {
          self.source.next();
          len += 1;
          c - b'A' + 10
        }
        b'a'..=b'z' => {
          self.source.next();
          len += 1;
          c - b'a' + 10
        }
        _ => break,
      };

      // handle error
      if digit >= radix {
        self.dlogger.log_digit_exceeds_radix(r, radix, digit);
        digit = 0;
      }

      n = n * radix + digit;
      range = union_of(range, r);

      if Some(len) == max_len {
        break;
      }
    }

    (n, range, len)
  }

  // lexes a number
  fn lex_number(&mut self) -> Token {
    assert!(matches!(
      self.source.peek(),
      Some((b'0'..=b'9' | b'-' | b'+', _))
    ));

    // we grab the first range
    let first_range = self.source.peek().unwrap().1;

    let sign;

    match self.source.peek() {
      Some((b'-', _)) => {
        // drop this char
        self.source.next();
        // set negative
        sign = Sign::Minus;
      }
      Some((b'+', _)) => {
        // drop this char
        self.source.next();
        sign = Sign::Plus;
      }
      _ => {
        // if it wasn't a positive or negative sign, set to positive by default
        sign = Sign::Plus;
      }
    }

    let radix;

    if let Some((b'0', _)) = self.source.peek() {
      match self.source.peek_nth(1) {
        Some((b'b', _)) => {
          radix = 2;
          // drop 2
          self.source.next();
          self.source.next();
        }
        Some((b'o', _)) => {
          radix = 8;
          // drop 2
          self.source.next();
          self.source.next();
        }
        Some((b'd', _)) => {
          radix = 10;
          // drop 2
          self.source.next();
          self.source.next();
        }
        Some((b'x', _)) => {
          radix = 16;
          // drop 2
          self.source.next();
          self.source.next();
        }
        Some((c @ (b'a'..=b'z' | b'A'..=b'Z'), r)) => {
          radix = 10;
          // log error
          self.dlogger.log_unrecognized_radix_code(*r, *c);
          // drop 2
          self.source.next();
          self.source.next();
        }
        _ => {
          // by default radix is 10, and we don't drop
          radix = 10;
        }
      };
    } else {
      radix = 10;
    }

    // parse base component
    let (whole_raw, whole_range, _) = self.internal_lex_base_number(radix, None);

    let fractional;
    if let Some((b'.', _)) = self.source.peek() {
      fractional = true;
      self.source.next();
    } else {
      fractional = false;
    }

    // the whole number part of the number, signed
    let whole_n = BigInt::from_biguint(sign, whole_raw);

    let token;

    if !fractional {
      // return
      token = Token::new(TokenKind::Int(whole_n), union_of(first_range, whole_range));
    } else {
      let (fractional_raw, fractional_range, fractional_len) =
        self.internal_lex_base_number(radix, None);

      let fractional_n = BigRational::new(
        // numerator
        BigInt::from_biguint(sign, fractional_raw),
        // denominator
        BigInt::new(Sign::Plus, vec![radix as u32]).pow(fractional_len),
      );

      token = Token::new(
        TokenKind::Real(fractional_n + whole_n),
        union_of(first_range, fractional_range),
      );
    }

    token
  }

  // terminator must not be a digit
  fn internal_lex_terminated_string(&mut self, terminator: u8) -> (Vec<u8>, Range) {
    // get initial range
    let mut range = self.source.peek().unwrap().1;

    let mut string = vec![];

    // we'll use a state machine string parser

    enum State {
      Text,
      Backslash,
      Byte,
      Exit,
      Utf8(u32),
    }

    let mut state = State::Text;
    while let Some((c, r)) = self.source.peek() {
      range = union_of(*r, range);
      match state {
        State::Text => {
          match *c {
            c if c == terminator => state = State::Exit,
            b'\\' => state = State::Backslash,
            c => string.push(c),
          }
          self.source.next();
        }
        State::Backslash => {
          state = State::Text;
          match *c {
            c if c == terminator => string.push(c),
            b'\\' => string.push(b'\\'),
            b'r' => string.push(b'\r'),
            b'n' => string.push(b'\n'),
            b't' => string.push(b'\t'),
            b'0' => string.push(b'\0'),
            b'x' => state = State::Byte,
            b'u' => state = State::Utf8(4),
            b'U' => state = State::Utf8(8),
            c => self.dlogger.log_unrecognized_escape_code(*r, c),
          }
          self.source.next();
        }
        State::Byte => {
          // parse hex digit
          let (val, range, len) = self.internal_lex_base_number(16, Some(2));

          if len != 2 {
            self.dlogger.log_incomplete_unicode_or_byte_escape(range, 2);
          } else {
            // push byte directly
            string.push(val.to_u32_digits()[0] as u8);
          }

          // set state back to text
          state = State::Text;
        }
        State::Utf8(u) => {
          // parse hex digit
          let (val, range, len) = self.internal_lex_base_number(16, Some(u));

          //handle mismatch of characters
          if len != u {
            self.dlogger.log_incomplete_unicode_or_byte_escape(range, u);
          } else {
            // convert to char
            if let Some(c) = from_u32(val.to_u32_digits()[0]) {
              string.extend_from_slice(c.encode_utf8(&mut [0; 4]).as_bytes());
            } else {
              self.dlogger.log_invalid_unicode_code_point(range);
            }
          }

          // set state back to text
          state = State::Text;
        }
        State::Exit => {
            break;
        }
      }
    }

    (string, range)
  }

  fn internal_lex_delimited_string(&mut self, delimiter: u8) -> (Vec<u8>, Range) {
    // assert that the first character is the delimiter
    assert!(self.source.peek().unwrap().0 == delimiter);

    // drop first delimiter
    let (_, r1) = self.source.next().unwrap();
    let (string, range) = self.internal_lex_terminated_string(delimiter);

    (string, union_of(r1, range))
  }

  fn lex_strop(&mut self) -> Token {
    let (ident, range) = self.internal_lex_delimited_string(b'`');
    Token::new(TokenKind::Identifier(String::from_utf8(ident).unwrap()), range)
  }

  fn lex_string(&mut self) -> Token {
    let (value, range) = self.internal_lex_delimited_string(b'"');
    Token::new(
      TokenKind::String {
        value: String::from_utf8(value).unwrap(),
        block: false,
      },
      range,
    )
  }

  fn lex_metadata(&mut self) -> Token {
    assert!(matches!(self.source.peek(), Some((b'#', _))));

    let mut range = self.source.peek().unwrap().1;
    let mut string = vec![];
    loop {
      // drop the hash
      self.source.next();
      let (mut line_string, line_range) = self.internal_lex_terminated_string(b'\n');
      range = union_of(range, line_range);
      string.append(&mut line_string);

      // now we try to grab the next line
      self.skip_indentation();

      // check that we have another quote
      if let Some((b'#', _)) = self.source.peek() {
        string.push(b'\n');
      } else {
        break;
      }
    }

    Token::new(
      TokenKind::Metadata {
        value: String::from_utf8(string).unwrap(),
        significant: false,
      },
      range,
    )
  }

  fn lex_block_string(&mut self) -> Token {
    assert!(matches!(
      self.source.peek_amount(2),
      [Some((b'"', _)), Some((b'"', _))]
    ));

    let mut range = self.source.peek().unwrap().1;
    let mut string = vec![];
    loop {
      // drop the double quote
      self.source.next();
      self.source.next();
      let (mut line_string, line_range) = self.internal_lex_terminated_string(b'\n');
      range = union_of(range, line_range);
      string.append(&mut line_string);

      // now we try to grab the next line
      self.skip_indentation();

      // check that we have another quote
      if let [Some((b'"', _)), Some((b'"', _))] = self.source.peek_amount(2) {
        string.push(b'\n');
      } else {
        break;
      }
    }

    Token::new(
      TokenKind::String {
        value: String::from_utf8(string).unwrap(),
        block: true,
      },
      range,
    )
  }

  fn lex_simple_token(&mut self, tk:TokenKind, n:u32) -> Token {
    assert!(n >= 1);
    let mut range =self.source.peek().unwrap().1;

    // absorb the next n tokens
    for _ in 0..n {
        range = union_of(range, self.source.next().unwrap().1);
    }

    Token::new(tk, range)
  }

}


impl<Source: Iterator<Item = u8>> Iterator for Tokenizer<Source> {
  type Item = Token;

  fn next(&mut self) -> Option<Token> {
    self.skip_whitespace();

    // here, c1 represents the next char that would be pulled,
    // c2 represents the char after that, etc
    match self.source.peek().cloned(){
      Some((c1, r1)) => Some(match c1 {
        // here we match different characters
        b'a'..=b'z' | b'A'..=b'Z' => self.lex_identifier_or_keyword(),
        b'0'..=b'9' => self.lex_number(),
        b'`' => self.lex_strop(),
        b'#' => self.lex_metadata(),
        b'\'' => self.lex_label(),
        b'"' => match self.source.peek_nth(1).cloned() {
          Some((b'"', _)) => self.lex_block_string(),
          _ => self.lex_string(),
        },
        b'+' => match self.source.peek_nth(1).cloned() {
          Some((b'+', _)) => self.lex_simple_token(TokenKind::Append, 2),
          Some((b'0'..=b'9', _)) => self.lex_number(),
          _ => self.lex_simple_token(TokenKind::Add, 1),
        },
        b'-' => match self.source.peek_nth(1).cloned() {
          Some((b'>', _)) => self.lex_simple_token(TokenKind::Arrow, 2),
          Some((b'-', _)) => self.lex_simple_token(TokenKind::Difference, 2),
          Some((b'0'..=b'9', _)) => self.lex_number(),
          _ => self.lex_simple_token(TokenKind::Sub, 1),
        },
        b'$' => match self.source.peek_nth(1).cloned() {
          Some((b'_', _)) => self.lex_simple_token(TokenKind::Ignore, 2),
          Some((b'*', _)) => self.lex_simple_token(TokenKind::Splat, 2),
          _ => self.lex_simple_token(TokenKind::Bind, 1),
        },
        b';' => self.lex_simple_token(TokenKind::Sequence, 1),
        b':' => match self.source.peek_nth(1).cloned() {
          Some((b':', _)) => self.lex_simple_token(TokenKind::FieldAccess, 2),
          _ => self.lex_simple_token(TokenKind::Constrain, 1),
        },
        b'&' => self.lex_simple_token(TokenKind::Ref, 1),
        b'@' => self.lex_simple_token(TokenKind::Deref, 1),
        b'^' => self.lex_simple_token(TokenKind::Pow, 1),
        b'|' => match self.source.peek_nth(1).cloned() {
          Some((b'>', _)) => self.lex_simple_token(TokenKind::PipeForward, 2),
          Some((b'|', _)) => self.lex_simple_token(TokenKind::CaseOption, 2),
          _ => self.lex_simple_token(TokenKind::Sum, 1),
        },
        b',' => self.lex_simple_token(TokenKind::Cons, 1),
        b'!' => match self.source.peek_nth(1).cloned() {
          Some((b'=', _)) => self.lex_simple_token(TokenKind::CompNotEqual, 2),
          _ => {
            self.dlogger.log_unrecognized_character(r1, c1);
            self.lex_simple_token(TokenKind::None, 1)
          }
        },
        b'=' => match self.source.peek_nth(1).cloned() {
          Some((b'=', _)) => self.lex_simple_token(TokenKind::CompEqual, 2),
          _ => self.lex_simple_token(TokenKind::Assign, 1),
        },
        b'<' => match self.source.peek_nth(1).cloned() {
          Some((b'|', _)) => self.lex_simple_token(TokenKind::PipeBackward, 2),
          Some((b'=', _)) => self.lex_simple_token(TokenKind::CompLessEqual, 2),
          _ => self.lex_simple_token(TokenKind::CompLess, 1),
        },
        b'>' => match self.source.peek_nth(1).cloned() {
          Some((b'>', _)) => self.lex_simple_token(TokenKind::Compose, 2),
          Some((b'=', _)) => self.lex_simple_token(TokenKind::CompGreaterEqual, 2),
          _ => self.lex_simple_token(TokenKind::CompGreater, 1),
        },
        b'/' => match self.source.peek_nth(1).cloned() {
          Some((b'\\', _)) => self.lex_simple_token(TokenKind::Intersection, 2),
          _ => self.lex_simple_token(TokenKind::Div, 1),
        },
        b'\\' => match self.source.peek_nth(1).cloned() {
          Some((b'/', _)) => self.lex_simple_token(TokenKind::Union, 2),
          _ => {
            self.dlogger.log_unrecognized_character(r1, c1);
            self.lex_simple_token(TokenKind::None, 1)
          }
        },
        b'.' => match self.source.peek_nth(1).cloned() {
          Some((b'.', _)) => self.lex_simple_token(TokenKind::Range, 2),
          Some((b'=', _)) => self.lex_simple_token(TokenKind::RangeInclusive, 2),
          _ => self.lex_simple_token(TokenKind::RevApply, 1),
        },
        b'*' => self.lex_simple_token(TokenKind::Mul, 1),
        b'%' => self.lex_simple_token(TokenKind::Rem, 1),
        b'(' => self.lex_simple_token(TokenKind::ParenLeft, 1),
        b')' => self.lex_simple_token(TokenKind::ParenRight, 1),
        b'{' => self.lex_simple_token(TokenKind::BraceLeft, 1),
        b'}' => self.lex_simple_token(TokenKind::BraceRight, 1),
        b'[' => self.lex_simple_token(TokenKind::BracketLeft, 1),
        b']' => self.lex_simple_token(TokenKind::BracketRight, 1),
        _ => {
          self.dlogger.log_unrecognized_character(r1, c1);
          self.source.next();
          Token::new(TokenKind::None, r1)
        }
      }),
      None => None,
    }
  }
}
