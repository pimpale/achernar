 // 
 // 
 // // inserts the specified identifier into the identifier table and onto the
 // // current scope
 // static IdentifierId hir_addIdentifier(hir_Constructor *constructor,
 //                                       hir_Identifier identifier) {
 //   // first append to identifier table
 //   *com_vec_push_m(&constructor->_identifier_table, hir_Identifier) = identifier;
 //   // it is known that vec len must be at least one, so no overflow is possible
 //   // here
 //   usize index =
 //       com_vec_len_m(&constructor->_identifier_table, hir_Identifier) - 1;
 //   // add index to the scope
 //   *com_vec_push_m(&constructor->_identifier_table, ScopeElem) =
 //       (ScopeElem){.kind = SEK_Identifier, .identifier = index};
 // 
 //   // return the identifier id
 //   return (IdentifierId){.index = index, .valid = true};
 // }
 // 

// #include "ast_to_hir.h"
// #include "com_assert.h"
// #include "com_mem.h"
// #include "com_str.h"
// 
// // the stuff that actually goes inside the identifier table
// typedef struct {
//   hir_Expr *expr;
//   com_str identifier;
//   // array of hir_Exprs that are deferred
//   com_vec defers;
// } LabelTableElem;
// 
// // utility method to allocate some noleak memory from the constructor
// static void *hir_alloc(hir_Constructor *constructor, usize len) {
//   return com_allocator_handle_get(com_allocator_alloc(
//       constructor->_a, (com_allocator_HandleData){
//                            .len = len,
//                            .flags = com_allocator_defaults(constructor->_a) |
//                                     com_allocator_NOLEAK}));
// }
// 
// #define hir_alloc_obj_m(constructor, type)                                     \
//   (type *)hir_alloc((constructor), sizeof(type))
// 
// static com_vec hir_alloc_vec(hir_Constructor *constructor) {
//   return com_vec_create(com_allocator_alloc(
//       constructor->_a,
//       (com_allocator_HandleData){
//           .len = 20, .flags = com_allocator_defaults(constructor->_a)}));
// }
// 
// // creates a hir_Constructor using a
// hir_Constructor hir_create(ast_Constructor *parser, com_allocator *a) {
//   hir_Constructor hc;
//   hc._a = a;
//   hc._parser = parser;
//   hc._label_table = hir_alloc_vec(&hc);
//   return hc;
// }
// 
// typedef struct {
//   LabelTableElem lte;
//   bool valid;
// } MaybeLabel;
// 
// // Looks through a scope
// static MaybeLabel hir_lookupLabel(hir_Constructor *constructor,
//                                   DiagnosticLogger *diagnostics,
//                                   ast_Label *label) {
//   switch (label->kind) {
//   case ast_LK_None: {
//     *dlogger_append(diagnostics) =
//         (Diagnostic){.span = label->span,
//                      .severity = DSK_Error,
//                      .message = com_str_lit_m("unknown label name"),
//                      .children_len = 0};
//     return (MaybeLabel){.valid = false};
//   }
//   case ast_LK_Label: {
//     com_str identifier = label->label.label;
// 
//     // start from the last valid index, and iterate towards 0
//     for (usize i_plus_one =
//              com_vec_len_m(&constructor->_label_table, LabelTableElem);
//          i_plus_one > 0; i_plus_one--) {
//       const usize i = i_plus_one - 1;
// 
//       // get entry
//       LabelTableElem entry =
//           *com_vec_get_m(&constructor->_label_table, i, LabelTableElem);
// 
//       if (com_str_equal(entry.identifier, identifier)) {
//         // if it matched our requirements, then set `result` and return true
//         return (MaybeLabel){.lte = entry, .valid = true};
//       }
//     }
//     // if we went through all entries in this scope and found nothing
//     return (MaybeLabel){.valid = false};
//   }
//   }
// }
// 
// static hir_Common hir_getCommon(const ast_Common in, bool generated,
//                                 hir_Constructor *constructor) {
//   com_vec metadata = hir_alloc_vec(constructor);
//   for (usize i = 0; i < in.metadata_len; i++) {
//     if (in.metadata[i].significant) {
//       // clone string
//       com_str src = in.metadata[i].data;
//       com_str_mut dest = (com_str_mut){.data = hir_alloc(constructor, src.len),
//                                        .len = src.len};
//       com_mem_move(dest.data, src.data, src.len);
//       *com_vec_push_m(&metadata, com_str) = com_str_demut(dest);
//     }
//   }
// 
//   usize metadata_len = com_vec_len_m(&metadata, com_str *);
//   return (hir_Common){.generated = generated,
//                       .span = in.span,
//                       .metadata = com_vec_release(&metadata),
//                       .metadata_len = metadata_len};
// }
// 
// // generates a nil expr that will be returned in the case of an instruction
// // that does not exist
// static hir_Expr *hir_generate_NilExpr(com_loc_Span span,
//                                       hir_Constructor *constructor) {
//   hir_Expr *ret = hir_alloc_obj_m(constructor, hir_Expr);
//   *ret = (hir_Expr){
//       .kind = hir_EK_Nil,
//       .common = {.span = span, .metadata_len = 0, .generated = true}};
//   return ret;
// }
// 
// static hir_Stmnt *hir_construct_Stmnt(const ast_Stmnt *in,
//                                       DiagnosticLogger *diagnostics,
//                                       hir_Constructor *constructor);
// 
// static hir_Expr *hir_construct_Expr(const ast_Expr *in,
//                                     DiagnosticLogger *diagnostics,
//                                     hir_Constructor *constructor);
// 
// static hir_Pat *hir_construct_Pat(const ast_Expr *in,
//                                   DiagnosticLogger *diagnostics,
//                                   hir_Constructor *constructor);
// 
// static hir_Stmnt *hir_construct_Stmnt(const ast_Stmnt *in,
//                                       DiagnosticLogger *diagnostics,
//                                       hir_Constructor *constructor) {
//   hir_Stmnt stmnt;
//   switch (in->kind) {
//   case ast_SK_None: {
//     // return nil stmnt
//     stmnt = (hir_Stmnt){
//         .kind = hir_SK_Expr,
//         .common = hir_getCommon(in->common, false, constructor),
//         .expr = {hir_generate_NilExpr(in->common.span, constructor)}};
//     break;
//   }
//   case ast_SK_Expr: {
//     stmnt = (hir_Stmnt){
//         .kind = hir_SK_Expr,
//         .common = hir_getCommon(in->common, false, constructor),
//         .expr = {hir_construct_Expr(in->expr.expr, diagnostics, constructor)}};
//     break;
//   }
//   case ast_SK_Assign: {
//     stmnt = (hir_Stmnt){
//         .kind = hir_SK_Assign,
//         .common = hir_getCommon(in->common, false, constructor),
//         .assign = {
//             .pat = hir_construct_Pat(in->assign.pat, diagnostics, constructor),
//             .val =
//                 hir_construct_Expr(in->assign.val, diagnostics, constructor)}};
//     break;
//   }
//   case ast_SK_Defer: {
//     // push the expr to the scopes table for the label
//     MaybeLabel ret = hir_lookupLabel(constructor, diagnostics, in->defer.label);
//     if (ret.valid) {
//       *com_vec_push_m(&ret.lte.defers, hir_Expr *) =
//           hir_construct_Expr(in->defer.val, diagnostics, constructor);
//     }
//     // return nil stmnt
//     stmnt = (hir_Stmnt){
//         .kind = hir_SK_Expr,
//         .common = hir_getCommon(in->common, false, constructor),
//         .expr = {hir_generate_NilExpr(in->common.span, constructor)}};
//     break;
//   }
//   }
// 
//   hir_Stmnt *ret = hir_alloc_obj_m(constructor, hir_Stmnt);
//   *ret = stmnt;
//   return ret;
// }
// static hir_Expr *hir_construct_Expr(const ast_Expr *in,
//                                     DiagnosticLogger *diagnostics,
//                                     hir_Constructor *constructor) {
//   hir_Expr expr;
// 
//   switch(in->kind) {
//     case ast_EK_None: {
//       expr = (hir_Expr) {
//           .kind = hir_EK_Nil,
//           .common = hir_getCommon(in->common, false, constructor)
//       };
//       break;
//     }
//     case ast_EK_Int: {
//       expr = (hir_Expr) {
//           .kind = hir_EK_Int,
//           .common = hir_getCommon(in->common, false, constructor)
//       };
//       break;
//     }
//     case ast_EK_Real: {
//       expr = (hir_Expr) {
//           .kind = hir_EK_Real,
//           .common = hir_getCommon(in->common, false, constructor)
//       };
//       break;
//     }
//     case ast_EK_String: {
//       expr = (hir_Expr) {
//           .kind = hir_EK_NeverType,
//           .common = hir_getCommon(in->common, false, constructor)
//       };
//       break;
//     }
//     case ast_EK_Loop: {
//       expr = (hir_Expr) {
//           .kind = hir_EK_NeverType,
//           .common = hir_getCommon(in->common, false, constructor)
//       };
//       break;
//     }
//     case ast_EK_Struct: {
//       expr = (hir_Expr) {
//           .kind = hir_EK_NeverType,
//           .common = hir_getCommon(in->common, false, constructor)
//       };
//       break;
//     }
//     case ast_EK_BindIgnore: {
//       expr = (hir_Expr) {
//           .kind = hir_EK_NeverType,
//           .common = hir_getCommon(in->common, false, constructor)
//       };
//       break;
//     }
//     case ast_EK_Bind: {
//       expr = (hir_Expr) {
//           .kind = hir_EK_NeverType,
//           .common = hir_getCommon(in->common, false, constructor)
//       };
//       break;
//     }
//     case ast_EK_AtBind: {
//       expr = (hir_Expr) {
//           .kind = hir_EK_NeverType,
//           .common = hir_getCommon(in->common, false, constructor)
//       };
//       break;
//     }
//     case ast_EK_Mutate: {
//       expr = (hir_Expr) {
//           .kind = hir_EK_NeverType,
//           .common = hir_getCommon(in->common, false, constructor)
//       };
//       break;
//     }
//     case ast_EK_AtMutate: {
//       expr = (hir_Expr) {
//           .kind = hir_EK_NeverType,
//           .common = hir_getCommon(in->common, false, constructor)
//       };
//       break;
//     }
//     case ast_EK_BinaryOp: {
//       expr = (hir_Expr) {
//           .kind = hir_EK_NeverType,
//           .common = hir_getCommon(in->common, false, constructor)
//       };
//       break;
//     }
//     case ast_EK_UnaryOp: {
//       expr = (hir_Expr) {
//           .kind = hir_EK_NeverType,
//           .common = hir_getCommon(in->common, false, constructor)
//       };
//       break;
//     }
//     case ast_EK_Ret: {
//       expr = (hir_Expr) {
//           .kind = hir_EK_NeverType,
//           .common = hir_getCommon(in->common, false, constructor)
//       };
//       break;
//     }
//     case ast_EK_Block: {
//       expr = (hir_Expr) {
//           .kind = hir_EK_NeverType,
//           .common = hir_getCommon(in->common, false, constructor)
//       };
//       break;
//     }
//     case ast_EK_Match: {
//       expr = (hir_Expr) {
//           .kind = hir_EK_NeverType,
//           .common = hir_getCommon(in->common, false, constructor)
//       };
//       break;
//     }
//     case ast_EK_FieldAccess: {
//       expr = (hir_Expr) {
//           .kind = hir_EK_NeverType,
//           .common = hir_getCommon(in->common, false, constructor)
//       };
//       break;
//     }
//     case ast_EK_Reference: {
//       expr = (hir_Expr) {
//           .kind = hir_EK_NeverType,
//           .common = hir_getCommon(in->common, false, constructor)
//       };
//       break;
//     }
//   }
// 
// 
//   // return
//   hir_Expr *ret = hir_alloc_obj_m(constructor, hir_Expr);
//   *ret = expr;
//   return ret;
// 
// }
