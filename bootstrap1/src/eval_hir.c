

// inserts the specified identifier into the identifier table and onto the
// current scope
static IdentifierId hir_addIdentifier(hir_Constructor *constructor,
                                      hir_Identifier identifier) {
  // first append to identifier table
  *com_vec_push_m(&constructor->_identifier_table, hir_Identifier) = identifier;
  // it is known that vec len must be at least one, so no overflow is possible
  // here
  usize index =
      com_vec_len_m(&constructor->_identifier_table, hir_Identifier) - 1;
  // add index to the scope
  *com_vec_push_m(&constructor->_identifier_table, ScopeElem) =
      (ScopeElem){.kind = SEK_Identifier, .identifier = index};

  // return the identifier id
  return (IdentifierId){.index = index, .valid = true};
}
