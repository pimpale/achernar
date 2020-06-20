#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

#include "allocator.h"
#include "lncol.h"

typedef enum {
  DSK_Error = 1,
  DSK_Warning = 2,
  DSK_Information = 3,
  DSK_Hint = 4,
} DiagnosticSeverityKind;

const char *strDiagnosticSeverityKind(DiagnosticSeverityKind val);

typedef struct Diagnostic_s {
  Span range;
  DiagnosticSeverityKind severity;
  const char* message;
  struct Diagnostic_s* children;
  size_t children_len;
} Diagnostic;

Diagnostic diagnostic_standalone(Span range, DiagnosticSeverityKind severity, const char* message) {
  return (Diagnostic)  {
    .range= range,
    .severity = severity,
    .message = message,
    .children  = NULL,
    .children_len = 0,
  };
}

#endif
