use super::ast;
use super::token::TokenKind;
use lsp_types::Diagnostic;
use lsp_types::DiagnosticSeverity;
use lsp_types::NumberOrString;
use lsp_types::Range;
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

  pub fn get_logger(&mut self, source: Option<String>) -> DiagnosticLogger {
    DiagnosticLogger {
      sender: self.send.clone(),
      source,
    }
  }
}

pub struct DiagnosticLogger {
  sender: Sender<Diagnostic>,
  source: Option<String>,
}

impl DiagnosticLogger {
  pub fn log_unexpected_eof_in_string(&mut self, range: Range) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(1)),
      code_description: None,
      source: self.source.clone(),
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
      source: self.source.clone(),
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
      source: self.source.clone(),
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
      source: self.source.clone(),
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
      source: self.source.clone(),
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
      source: self.source.clone(),
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
      source: self.source.clone(),
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
      Some(tkk) => format!("token of kind `{}`", tkk.as_ref()),
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
      source: self.source.clone(),
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

  pub fn log_cannot_find_label_in_scope(&mut self, range: Range, label: Vec<u8>) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(9)),
      code_description: None,
      source: self.source.clone(),
      message: format!(
        "cannot find label `{}` in scope",
        String::from_utf8_lossy(label.as_slice())
      ),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_expected_case_option_expr(&mut self, range: Range, kind: &ast::ExprKind) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(10)),
      code_description: None,
      source: self.source.clone(),
      message: format!(
        "expected CaseOption BinaryOp, but found unexpected {}",
        kind.as_ref()
      ),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_expected_case_option_binop(&mut self, range: Range, kind: &ast::BinaryOpKind) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(11)),
      code_description: None,
      source: self.source.clone(),
      message: format!(
        "expected CaseOption, but found unexpected {}",
        kind.as_ref()
      ),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_unexpected_in_pattern(&mut self, range: Range, kind: &ast::ExprKind) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(12)),
      code_description: None,
      source: self.source.clone(),
      message: format!(
        "{} is invalid in a pattern. If you wish to use the value yielded from this expression, consider using `val`",
        kind.as_ref()
      ),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_unexpected_unop_in_pattern(&mut self, range: Range, kind: &ast::UnaryOpKind) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(12)),
      code_description: None,
      source: self.source.clone(),
      message: format!(
        "{} is an invalid unary operator in a pattern. If you wish to use the value yielded from this expression, consider using `val`",
        kind.as_ref()
      ),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_unexpected_binop_in_pattern(&mut self, range: Range, kind: &ast::BinaryOpKind) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(12)),
      code_description: None,
      source: self.source.clone(),
      message: format!(
        "{} is an invalid binary operator in a pattern. If you wish to use the value yielded from this expression, consider using `val`",
        kind.as_ref()
      ),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_unexpected_element_in_pattern_struct(&mut self, range: Range, kind: &ast::ExprKind) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(12)),
      code_description: None,
      source: self.source.clone(),
      message: format!(
        "{} is not permitted in a pattern struct literal. The body of the struct must contain only assigns seperated by semicolons.",
        kind.as_ref()
      ),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_unexpected_binop_in_pattern_struct(&mut self, range: Range, kind: &ast::BinaryOpKind) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(12)),
      code_description: None,
      source: self.source.clone(),
      message: format!(
        "{} is not permitted in a pattern struct literal. The body of the struct must contain only assigns seperated by semicolons.",
        kind.as_ref()
      ),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_unsupported_target_in_pattern_struct_assign(
    &mut self,
    range: Range,
    kind: &ast::ExprKind,
  ) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(12)),
      code_description: None,
      source: self.source.clone(),
      message: format!(
        "{} is an invalid target in for an assign in a pattern struct literal. The left hand side may either be `$*` or a single variable binding.",
        kind.as_ref()
      ),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_unsupported_bind_target_in_pattern_struct_assign(
    &mut self,
    range: Range,
    kind: &ast::ExprKind,
  ) {
    self.log(Diagnostic {
          range,
          severity: Some(DiagnosticSeverity::Error),
          code: Some(NumberOrString::Number(12)),
          code_description: None,
          source: self.source.clone(),
          message: format!(
            "$ may only be used to prefix an identifier. {} is not permitted as a target in a pattern struct literal.",
            kind.as_ref()
          ),
          related_information: None,
          tags: None,
          data: None,
        })
  }

  pub fn log_unexpected_bind_target(&mut self, range: Range, kind: &ast::ExprKind) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(12)),
      code_description: None,
      source: self.source.clone(),
      message: format!("Cannot bind variable to symbol of `{}`.", kind.as_ref()),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_only_in_pattern(&mut self, range: Range, kind: &ast::ExprKind) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(13)),
      code_description: None,
      source: self.source.clone(),
      message: format!("{} is invalid outside of a pattern.", kind.as_ref()),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_unexpected_binop_in_expr(&mut self, range: Range, kind: &ast::BinaryOpKind) {
    self.log(Diagnostic {
             range,
             severity: Some(DiagnosticSeverity::Error),
             code: Some(NumberOrString::Number(13)),
             code_description: None,
             source: self.source.clone(),
             message: format!(
               "{} is invalid outside of a pattern. If you wish to use this pattern as a function, consider using `pat`",
               kind.as_ref()
             ),
             related_information: None,
             tags: None,
             data: None,
        })
  }

  pub fn log_unexpected_unop_in_expr(&mut self, range: Range, kind: &ast::UnaryOpKind) {
    self.log(Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::Error),
        code: Some(NumberOrString::Number(14)),
        code_description: None,
        source: self.source.clone(),
        message: format!(
          "{} is invalid outside of a pattern. If you wish to use this pattern as a function, consider using `pat`",
          kind.as_ref()
        ),
        related_information: None,
        tags: None,
        data: None,
        })
  }

  pub fn log_repeat_splat_in_pattern_struct(&mut self, range: Range) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(14)),
      code_description: None,
      source: self.source.clone(),
      message: String::from("Only one splat is allowed in a pattern struct."),
      // Todo hint other splat
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_only_in_case(&mut self, range: Range) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(15)),
      code_description: None,
      source: self.source.clone(),
      message: String::from("the CaseOption operator may only be used in a case context"),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_field_not_identifier(&mut self, range: Range, kind: &ast::ExprKind) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(16)),
      code_description: None,
      source: self.source.clone(),
      message: format!(
        "expected identifier after module access operator, but found unexpected {}",
        kind.as_ref()
      ),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_unexpected_infer_arg(&mut self, range: Range) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(17)),
      code_description: None,
      source: self.source.clone(),
      message: "infer args may only be placed as the left hand side of a defun or the right side of an apply".to_owned(),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_expected_nil_loop_body(&mut self, range: Range) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(18)),
      code_description: None,
      source: self.source.clone(),
      message: "the body of a loop must evaluate to nil.".to_owned(),
      related_information: None,
      tags: None,
      data: None,
    })
  }

  pub fn log_unexpected_type(&mut self, range: Range, expected_type: &str, got_type: &str) {
    self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::Error),
      code: Some(NumberOrString::Number(19)),
      code_description: None,
      source: self.source.clone(),
      message: format!("expected type {}, but found type {}", expected_type, got_type),
      related_information: None,
      tags: None,
      data: None,
    })
  }
}
