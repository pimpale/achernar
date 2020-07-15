#include "com_assert.h"
#include "com_json.h"
#include "com_os_exit.h"
#include "com_os_io_out.h"

bool attr_NORETURN com_assert_fail(com_str condition, com_str message,
                                   com_str file, u32 line, com_str function) {

  com_writer err = com_os_io_err_writer_create();

  com_json_Elem elem = com_json_obj_lit_m(((com_json_Prop[]){
      com_json_Prop_m(com_str_lit_m("kind"), com_json_str_m(com_str_lit_m("assert"))),
      com_json_Prop_m(com_str_lit_m("condition"), com_json_str_m(condition)),
      com_json_Prop_m(com_str_lit_m("message"), com_json_str_m(message)),
      com_json_Prop_m(com_str_lit_m("file"), com_json_str_m(file)),
      com_json_Prop_m(com_str_lit_m("function"), com_json_str_m(function)),
      com_json_Prop_m(com_str_lit_m("line"), com_json_int_m(line))
  }));

  com_json_serialize(&elem, &err);

  com_os_exit_panic(1);
}

void attr_NORETURN com_assert_unreachable(com_str message, com_str file, u32 line, com_str function) {
  com_writer err = com_os_io_err_writer_create();

  com_json_Elem elem = com_json_obj_lit_m(((com_json_Prop[]){
      com_json_Prop_m(com_str_lit_m("kind"), com_json_str_m(com_str_lit_m("unreachable"))),
      com_json_Prop_m(com_str_lit_m("message"), com_json_str_m(message)),
      com_json_Prop_m(com_str_lit_m("file"), com_json_str_m(file)),
      com_json_Prop_m(com_str_lit_m("function"), com_json_str_m(function)),
      com_json_Prop_m(com_str_lit_m("line"), com_json_int_m(line))
  }));

  com_json_serialize(&elem, &err);

  com_os_exit_panic(1);
}
