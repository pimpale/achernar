use super::codereader::union_of;
use super::codereader::CodeReader;
use super::dlogger::DiagnosticLogger;
use super::token::*;
use lsp_types::Position;
use lsp_types::Range;
use peekmore::PeekMore;
use peekmore::PeekMoreIterator;

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

  // requires that there is at least one valid token
  fn lex_word(&mut self) -> Token {
    // we grab the initial token as our first range
    let (_, initial_range) = *self.source.peek().unwrap();
    // we then try folding each next element
    let (identifier, range) =
      self
        .source
        .try_fold((vec![], initial_range), |acc, elem| match elem {
          (c @ (b'a'..=b'z' | b'A'..=b'Z' | b'_'), r) => {
              acc.0.push(c);
              Some((acc.0, union_of(acc.1, r)))
          },
          _ => None,
        }).unwrap();

    let tk = match identifier.as_slice() {
        b"loop" => TokenKind::Loop,
        b"of" => TokenKind::Of,
        b"true" => TokenKind::Bool(true),
        b"false" => TokenKind::Bool(false),
        b"bool" => TokenKind::BoolType,
        b"val" => TokenKind::Val,
        b"pat" => TokenKind::Pat,
        b"case" => TokenKind::Case,
        b"ret" => TokenKind::Ret,
        b"ret" => TokenKind::Ret,
        b"this" => TokenKind::This,
        b"defer" => TokenKind::Defer,
        b"as" => TokenKind::As,
        b"inf" => TokenKind::Inf,
        b"nan" => TokenKind::Nan,
        b"in" => TokenKind::In,
        b"dyn" => TokenKind::Dyn,
        b"nan" => TokenKind::Nan,
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
        _ => TokenKind::Identifier(identifier),
    };

    Token::new(tk, range)
  }

  fn lex_number(&mut self) -> Token {}
  fn lex_strop(&mut self) -> Token {}
  fn lex_metadata(&mut self) -> Token {}
  fn lex_block_string(&mut self) -> Token {}
  fn lex_string(&mut self) -> Token {}
  fn lex_label(&mut self) -> Token {}
}

impl<Source: Iterator<Item = u8>> Iterator for Tokenizer<Source> {
  type Item = Token;

  fn next(&mut self) -> Option<Token> {
    // here, c1 represents the next char that would be pulled,
    // c2 represents the char after that, etc

    match self.source.peek() {
      Some((c1, r1)) => Some(match *c1 {
        // here we match different characters
        b'a'..=b'z' | b'A'..=b'Z' => self.lex_word(),
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
