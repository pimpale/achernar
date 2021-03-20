use super::diagnostic::Diagnostic;

pub struct DiagnosticLogger {
  diagnostics:Vec<Diagnostic>
}

impl DiagnosticLogger {
    pub fn new() -> DiagnosticLogger{
        return DiagnosticLogger {
          diagnostics:vec![]
        }
    }

    pub fn append(&mut self, diagnostic:Diagnostic) {
      self.diagnostics.push(diagnostic);
    }
}


