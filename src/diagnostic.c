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

Diagnostic *dlogger_append(DiagnosticLogger *ptr) {
  return com_vec_push_m(&ptr->_diagnostics, Diagnostic);
}

com_allocator_Handle dlogger_alloc(DiagnosticLogger *ptr, usize n) {
  return com_allocator_alloc(
      ptr->_a, (com_allocator_HandleData){
                   .len = n,
                   .flags = com_allocator_defaults(ptr->_a) |
                            com_allocator_NOLEAK | com_allocator_REALLOCABLE});
}

com_vec dlogger_release(DiagnosticLogger *ptr) { return ptr->_diagnostics; }
