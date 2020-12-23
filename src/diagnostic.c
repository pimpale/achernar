#include "diagnostic.h"
#include "com_assert.h"

com_str strDiagnosticSeverityKind(DiagnosticSeverityKind val) {
  switch (val) {
  case DSK_Error:
    return com_str_lit_m("Error");
  case DSK_Warning:
    return com_str_lit_m("Warning");
  case DSK_Information:
    return com_str_lit_m("Information");
  case DSK_Hint:
    return com_str_lit_m("Hint");
  }
  com_assert_unreachable_m("unreachable");
}

DiagnosticLogger dlogger_create(com_allocator *a) {
  return (DiagnosticLogger){
      ._a = a,
      ._diagnostics = com_vec_create(com_allocator_alloc(
          a, (com_allocator_HandleData){.len = 10,
                                        .flags = com_allocator_defaults(a) |
                                                 com_allocator_NOLEAK |
                                                 com_allocator_REALLOCABLE}))};
}

Diagnostic *dlogger_append(DiagnosticLogger *ptr, bool visible) {
  DiagnosticEntry *de = com_vec_push_m(&ptr->_diagnostics, DiagnosticEntry);
  de->visible = visible;
  return &de->diagnostic;
}

com_allocator* dlogger_alloc(DiagnosticLogger *ptr) {
    return ptr->_a;
}

const com_vec* dlogger_diagnostics(DiagnosticLogger *ptr) { return &ptr->_diagnostics; }
