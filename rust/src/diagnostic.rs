use super::span::Span;

pub enum DiagnosticData {
  // lexing errors
  UnexpectedEOFInString,
  InvalidControlChar { value: char },
  InvalidUnicodeCodePoint,
  DigitExceedsRadix { radix: u8, digit: u8 },
  UnrecognizedRadixCode { code: char },
  UnrecognizedCharacter { value: char },
}

pub struct Diagnostic {
  span: Span,
  data: DiagnosticData,
}

pub enum Severity {
  Error,
  Warning,
  Information,
  Hint,
}

pub fn severity(error: &DiagnosticData) -> Severity {
  match error {
    DiagnosticData::UnexpectedEOFInString => Severity::Error,
    DiagnosticData::InvalidControlChar { .. } => Severity::Error,
    DiagnosticData::InvalidUnicodeCodePoint => Severity::Error,
    DiagnosticData::DigitExceedsRadix { .. } => Severity::Error,
    DiagnosticData::UnrecognizedRadixCode { .. } => Severity::Error,
    DiagnosticData::UnrecognizedCharacter { .. } => Severity::Error,
  }
}


