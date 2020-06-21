#include "diagnostic.h"
#include <stdlib.h>

const char *strDiagnosticSeverityKind(DiagnosticSeverityKind val) {
  switch (val) {
  case DSK_Error:
    return "Error";
  case DSK_Warning:
    return "Warning";
  case DSK_Information:
    return "Information";
  case DSK_Hint:
    return "Hint";
  }

  abort();
}

Diagnostic diagnostic_standalone(Span range, DiagnosticSeverityKind severity, char* message) {
  return (Diagnostic)  {
    .range= range,
    .severity = severity,
    .message = message,
    .children  = NULL,
    .children_len = 0,
  };
}

DiagnosticLogger dlogger_create(Allocator *a) {
  return (DiagnosticLogger) {
    .a = a,
    .diagnostics = vec_create(a)
  };
}

Diagnostic* dlogger_append(DiagnosticLogger *ptr) {
  return VEC_PUSH(&ptr->diagnostics, Diagnostic);
}

Vector dlogger_release(DiagnosticLogger* ptr) {
  return ptr->diagnostics;
}
