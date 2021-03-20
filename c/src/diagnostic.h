#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

#include "com_define.h"
#include "com_loc.h"
#include "com_allocator.h"
#include "com_vec.h"

typedef enum {
  DSK_Error = 1,
  DSK_Warning = 2,
  DSK_Information = 3,
  DSK_Hint = 4,
} DiagnosticSeverityKind;

com_str strDiagnosticSeverityKind(DiagnosticSeverityKind val);

typedef struct Diagnostic_s {
  com_loc_Span span;
  DiagnosticSeverityKind severity;
  com_str message;
  struct Diagnostic_s* children;
  usize children_len;
} Diagnostic;

typedef struct {
    bool visible;
    Diagnostic diagnostic;
} DiagnosticEntry;

typedef struct {
    com_allocator *_a;
    // Vector diagnostics<DiagnosticEntry>
    com_vec _diagnostics;
} DiagnosticLogger;

DiagnosticLogger dlogger_create(com_allocator *a);

Diagnostic* dlogger_append(DiagnosticLogger* ptr, bool visible);

// returns a reference to the diagnostic logger's allocator 
// should be used to allocate material with the lifetime of DiagnosticLogger
com_allocator* dlogger_alloc(DiagnosticLogger* ptr);

// returns a const reference to the diagnostics vector
const com_vec* dlogger_diagnostics(DiagnosticLogger* ptr);

// destroys the dlogger, and frees all emmory associated with it
void dlogger_destroy(DiagnosticLogger* dlogger);

#endif
