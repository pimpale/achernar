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

  // requires that there is at least one valid token
  fn internal_lex_word(&mut self) -> (Vec<u8>, Range) {
    assert!(matches!(
      self.source.peek(),
      Some((b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_', _))
    ));

    let mut word = vec![];
    let mut range = None;
    for (c, r) in self.source {
      if let b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' = c {
        word.push(c);
        // handle range
        if let Some(or) = range {
          range = Some(union_of(r, or));
        } else {
          range = Some(r);
        }
      } else {
        break;
      }
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
      _ => TokenKind::Identifier(word),
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
    Token::new(TokenKind::Label(word), union_of(ar, range))
  }

  // requires at least one character exists...
  fn internal_lex_base_number(&mut self, radix: u8, max_len: Option<u32>) -> (BigUint, Range, u32) {
    let mut range = self.source.peek().unwrap().1;

    let mut len = 0u32;
    let n = BigUint::zero();

    while let Some((c, r)) = self.source.peek() {
      let digit = match c {
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
        self.dlogger.logDigitExceedsRadix(*r, digit, radix);
        digit = 0;
      }

      n = n * radix + digit;
      range = union_of(range, *r);

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
      Some((b'-', r)) => {
        // drop this char
        self.source.next();
        // set negative
        sign = Sign::Minus;
      }
      Some((b'+', r)) => {
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
      match self.source.peek_nth(2) {
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
          // drop 2
          self.source.next();
          self.source.next();
          // log error
          self.dlogger.logUnrecognizedRadixCode(*r, *c);
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

    if !fractional {
      return Token::new(TokenKind::Int(whole_n), union_of(first_range, whole_range));
    } else {
      let (fractional_raw, fractional_range, fractional_len) =
        self.internal_lex_base_number(radix, None);

      let fractional_n = BigRational::new(
        // numerator
        BigInt::from_biguint(sign, fractional_raw),
        // denominator
        BigInt::new(Sign::Plus, vec![radix as u32]).pow(fractional_len),
      );

      return Token::new(
        TokenKind::Real(fractional_n + whole_n),
        union_of(first_range, fractional_range),
      );
    }
  }

  fn internal_lex_terminated_string(&mut self, terminator: u8) -> (Vec<u8>, Range) {
    // get initial range
    let mut range = self.source.next().unwrap().1;

    let mut string = vec![];

    // we'll use a state machine string parser

    enum State {
      Text,
      Backslash,
      Byte,
      Utf8(u32),
    }

    let mut state = State::Text;
    while let Some((c, r)) = self.source.peek() {
      range = union_of(*r, range);
      match state {
        State::Text => {
          match *c {
            c if c == terminator => break,
            b'\\' => state = State::Backslash,
            c => string.push(c),
          }
          self.source.next();
        }
        State::Backslash => {
          state = State::Text;
          match *c {
            c if c == terminator => string.push(c),
            c @ b'\\' => string.push(c),
            c @ b'\r' => string.push(c),
            c @ b'\n' => string.push(c),
            c @ b'\t' => string.push(c),
            c @ b'\0' => string.push(c),
            c @ b'\\' => string.push(c),
            c @ b'x' => state = State::Byte,
            c @ b'u' => state = State::Utf8(4),
            c @ b'U' => state = State::Utf8(8),
            c => self.dlogger.logUnrecognizedEscapeCode(*r, c),
          }
          self.source.next();
        }
        State::Byte => {
          // parse hex digit
          let (val, range, len) = self.internal_lex_base_number(16, Some(2));

          if len != 2 {
            self.dlogger.logIncompleteUnicodeOrByteEscape(range, 2);
          }

          // push byte directly
          string.push(val.to_u32_digits()[0] as u8);

          // set state back to text
          state = State::Text;
        }
        State::Utf8(u) => {
          // parse hex digit
          let (val, range, len) = self.internal_lex_base_number(16, Some(u));

          //handle mismatch of characters
          if len != u {
            self.dlogger.logIncompleteUnicodeOrByteEscape(range, u);
          }

          // convert to char
          if let Some(c) = from_u32(val.to_u32_digits()[0]) {
            string.extend_from_slice(c.encode_utf8(&mut [0; 4]).as_bytes());
          } else {
            self.dlogger.logInvalidUnicodeCodePoint(range);
          }

          // set state back to text
          state = State::Text;
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
    Token::new(TokenKind::Identifier(ident), range)
  }

  fn lex_string(&mut self) -> Token {
    let (value, range) = self.internal_lex_delimited_string(b'"');
    Token::new(
      TokenKind::String {
        value,
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
      let (line_string, line_range) = self.internal_lex_terminated_string(b'\n');
      range = union_of(range, line_range);
      string.append(&mut line_string);

      // now we try to grab the next line
      self.skip_whitespace();

      // check that we have another quote
      if let Some((b'#', _)) = self.source.peek() {
        string.push(b'\n');
      } else {
        break;
      }
    }

    Token::new(
      TokenKind::Metadata {
        value: string,
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
      // drop the hash
      self.source.next();
      let (line_string, line_range) = self.internal_lex_terminated_string(b'\n');
      range = union_of(range, line_range);
      string.append(&mut line_string);

      // now we try to grab the next line
      self.skip_whitespace();

      // check that we have another quote
      if let [Some((b'"', _)), Some((b'"', _))] = self.source.peek_amount(2) {
        string.push(b'\n');
      } else {
        break;
      }
    }

    Token::new(
      TokenKind::String {
        value: string,
        block: true,
      },
      range,
    )
  }
}

impl<Source: Iterator<Item = u8>> Iterator for Tokenizer<Source> {
  type Item = Token;

  fn next(&mut self) -> Option<Token> {
    self.skip_whitespace();

    // here, c1 represents the next char that would be pulled,
    // c2 represents the char after that, etc
    match self.source.peek() {
      Some((c1, r1)) => Some(match *c1 {
        // here we match different characters
        b'a'..=b'z' | b'A'..=b'Z' => self.lex_identifier_or_keyword(),
        b'0'..=b'9' => self.lex_number(),
        b'`' => self.lex_strop(),
        b'#' => self.lex_metadata(),
        b'\'' => self.lex_label(),
        b'"' => match self.source.peek_nth(2) {
          Some((b'"', _)) => self.lex_block_string(),
          _ => self.lex_string(),
        },
        b'+' => match self.source.peek_nth(2) {
          Some((b'+', r2)) => Token::new(TokenKind::Append, union_of(*r1, *r2)),
          Some((b'0'..=b'9', _)) => self.lex_number(),
          _ => Token::new(TokenKind::Add, *r1),
        },
        b'-' => match self.source.peek_nth(2) {
          Some((b'>', r2)) => Token::new(TokenKind::Arrow, union_of(*r1, *r2)),
          Some((b'-', r2)) => Token::new(TokenKind::Difference, union_of(*r1, *r2)),
          Some((b'0'..=b'9', _)) => self.lex_number(),
          _ => Token::new(TokenKind::Sub, *r1),
        },
        b'$' => match self.source.peek_nth(2) {
          Some((b'_', r2)) => Token::new(TokenKind::Ignore, union_of(*r1, *r2)),
          Some((b'-', r2)) => Token::new(TokenKind::Splat, union_of(*r1, *r2)),
          _ => Token::new(TokenKind::Bind, *r1),
        },
        b';' => Token::new(TokenKind::Sequence, *r1),
        b':' => match self.source.peek_nth(2) {
          Some((b':', r2)) => Token::new(TokenKind::FieldAccess, union_of(*r1, *r2)),
          _ => Token::new(TokenKind::Constrain, *r1),
        },
        b'&' => Token::new(TokenKind::Ref, *r1),
        b'@' => Token::new(TokenKind::Deref, *r1),
        b'^' => Token::new(TokenKind::Pow, *r1),
        b'|' => match self.source.peek_nth(2) {
          Some((b'>', r2)) => Token::new(TokenKind::PipeForward, union_of(*r1, *r2)),
          Some((b'|', r2)) => Token::new(TokenKind::CaseOption, union_of(*r1, *r2)),
          _ => Token::new(TokenKind::Sum, *r1),
        },
        b',' => Token::new(TokenKind::Cons, *r1),
        b'!' => match self.source.peek_nth(2) {
          Some((b'=', r2)) => Token::new(TokenKind::CompNotEqual, union_of(*r1, *r2)),
          _ => {
            self.dlogger.logUnrecognizedCharacter(*r1, *c1);
            Token::new(TokenKind::None, *r1)
          }
        },
        b'=' => match self.source.peek_nth(2) {
          Some((b'=', r2)) => Token::new(TokenKind::CompEqual, union_of(*r1, *r2)),
          _ => Token::new(TokenKind::Assign, *r1),
        },
        b'<' => match self.source.peek_nth(2) {
          Some((b'|', r2)) => Token::new(TokenKind::PipeBackward, union_of(*r1, *r2)),
          Some((b'=', r2)) => Token::new(TokenKind::CompLessEqual, union_of(*r1, *r2)),
          _ => Token::new(TokenKind::CompLess, *r1),
        },
        b'>' => match self.source.peek_nth(2) {
          Some((b'>', r2)) => Token::new(TokenKind::Compose, union_of(*r1, *r2)),
          Some((b'=', r2)) => Token::new(TokenKind::CompGreaterEqual, union_of(*r1, *r2)),
          _ => Token::new(TokenKind::CompGreater, *r1),
        },
        b'/' => match self.source.peek_nth(2) {
          Some((b'\\', r2)) => Token::new(TokenKind::Intersection, union_of(*r1, *r2)),
          _ => Token::new(TokenKind::Div, *r1),
        },
        b'\\' => match self.source.peek_nth(2) {
          Some((b'/', r2)) => Token::new(TokenKind::Union, union_of(*r1, *r2)),
          _ => {
            self.dlogger.logUnrecognizedCharacter(*r1, *c1);
            Token::new(TokenKind::None, *r1)
          }
        },
        b'.' => match self.source.peek_nth(2) {
          Some((b'.', r2)) => Token::new(TokenKind::Range, union_of(*r1, *r2)),
          Some((b'=', r2)) => Token::new(TokenKind::RangeInclusive, union_of(*r1, *r2)),
          _ => Token::new(TokenKind::RevApply, *r1),
        },
        b'*' => Token::new(TokenKind::Mul, *r1),
        b'%' => Token::new(TokenKind::Rem, *r1),
        b'(' => Token::new(TokenKind::ParenLeft, *r1),
        b')' => Token::new(TokenKind::ParenRight, *r1),
        b'{' => Token::new(TokenKind::BraceLeft, *r1),
        b'}' => Token::new(TokenKind::BraceRight, *r1),
        b'[' => Token::new(TokenKind::BracketLeft, *r1),
        b']' => Token::new(TokenKind::BracketRight, *r1),
        c => {
          self.dlogger.logUnrecognizedCharacter(*r1, *c1);
          Token::new(TokenKind::None, *r1)
        }
      }),
      None => None,
    }
  }
}
