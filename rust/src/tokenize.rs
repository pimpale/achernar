use super::dlogger::DiagnosticLogger;
use super::token::Token;
use lsp_types::Diagnostic;

pub struct Tokenizer {
  pub stream: Box<dyn Iterator<Item = char>>,
  pub dlogger: DiagnosticLogger,
}

pub fn tokenize(
  stream: impl IntoIterator<Item = char> + 'static,
  dlogger: DiagnosticLogger,
) -> impl Iterator<Item = Token> {
  Tokenizer {
    stream: Box::new(stream.into_iter()),
    dlogger,
  }
}

impl Iterator for Tokenizer {
  type Item = Token;

  fn next(&mut self) -> Option<Token> {
    let c = self
      .stream
      .as_mut()
      .skip_while(|c| !c.is_whitespace())
      .next();

    // handle EOF
    if let None = c {
      return None;
    }

    match c.unwrap() {
      ch => {
          self.dlogger.logUnrecognizedCharacter(ch)},
    }
  }
}
