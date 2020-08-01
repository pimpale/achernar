#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

#include "lncol.h"
#include "allocator.h"
#include "vector.h"

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
   char* message;
  struct Diagnostic_s* children;
  size_t children_len;
} Diagnostic;


Diagnostic diagnostic_standalone(Span range, DiagnosticSeverityKind severity,  char* message);

typedef struct {
    Allocator *a;
    Vector diagnostics;
} DiagnosticLogger;

DiagnosticLogger dlogger_create(Allocator *a);

Diagnostic* dlogger_append(DiagnosticLogger* ptr);

Vector dlogger_release(DiagnosticLogger* ptr);

#endif
