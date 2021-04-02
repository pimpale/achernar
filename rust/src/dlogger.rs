use super::token::TokenKind;
use super::COMPILER_NAME;
use lsp_types::Diagnostic;
use lsp_types::DiagnosticSeverity;
use lsp_types::NumberOrString;
use lsp_types::Range;
use std::string::ToString;
use std::sync::mpsc::channel;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::Sender;

pub struct DiagnosticLog {
  recv: Receiver<Diagnostic>,
  send: Sender<Diagnostic>,
}

impl DiagnosticLog {
  pub fn new() -> Self {
    let (send, recv) = channel();
    DiagnosticLog { recv, send }
  }

  pub fn get_logger(&mut self) -> DiagnosticLogger {
    DiagnosticLogger {
      sender: self.send.clone(),
    }
  }
}

pub struct DiagnosticLogger {
  sender: Sender<Diagnostic>,
}

impl DiagnosticLogger {
  pub fn log_unexpected_eof_in_string(&mut self, range: Range) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(1)),
      code_description: None,
      source: Some(COMPILER_NAME.to_owned()),
      message: "unexpected end of file in string, expected close quote".to_owned(),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_unrecognized_escape_code(&mut self, range: Range, c: u8) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(2)),
      code_description: None,
      source: Some(COMPILER_NAME.to_owned()),
      message: format!("invalid control char code `{}` in string", c as char),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_invalid_unicode_code_point(&mut self, range: Range) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(3)),
      code_description: None,
      source: Some(COMPILER_NAME.to_owned()),
      message: "invalid unicode code point following unicode control code".to_owned(),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_digit_exceeds_radix(&mut self, range: Range, radix: u8, digit: u8) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(4)),
      code_description: None,
      source: Some(COMPILER_NAME.to_owned()),
      message: format!(
        "digit ({}) is greater than or equal to the radix ({})",
        digit, radix
      ),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_unrecognized_radix_code(&mut self, range: Range, code: u8) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(5)),
      code_description: None,
      source: Some(COMPILER_NAME.to_owned()),
      message: format!("radix code 0{} is not recognized", code as char),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_unrecognized_character(&mut self, range: Range, character: u8) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(6)),
      code_description: None,
      source: Some(COMPILER_NAME.to_owned()),
      message: format!("unrecognized character: `{}`", character as char),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_incomplete_unicode_or_byte_escape(&mut self, range: Range, expected_len: u32) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(7)),
      code_description: None,
      source: Some(COMPILER_NAME.to_owned()),
      message: format!(
        "unicode or byte escape sequence requires {} hex characters immediately following",
        expected_len
      ),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  fn format_token(&self, maybe_tk: Option<TokenKind>) -> String {
    match maybe_tk {
      Some(tkk) => format!("token of kind `{}`", tkk.to_string()),
      None => String::from("EOF"),
    }
  }

  pub fn log_unexpected_token(
    &mut self,
    range: Range,
    expected: &str,
    unexpected_kind: Option<TokenKind>,
  ) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(8)),
      code_description: None,
      source: Some(COMPILER_NAME.to_owned()),
      message: format!(
        "expected {} but found unexpected {}",
        expected,
        self.format_token(unexpected_kind)
      ),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_unexpected_token_specific(
    &mut self,
    range: Range,
    expected_kind: Option<TokenKind>,
    unexpected_kind: Option<TokenKind>,
  ) {
    self.log_unexpected_token(range, &self.format_token(expected_kind), unexpected_kind);
  }

  fn log(&mut self, d: Diagnostic) {
    dbg!(d.clone());
    self.sender.send(d).unwrap()
  }
}
