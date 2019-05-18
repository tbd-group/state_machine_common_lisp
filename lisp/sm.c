/*      Compiler: ECL 16.1.2                                          */
/*      Date: 2019/5/18 12:08 (yyyy/mm/dd)                            */
/*      Machine: Linux 4.18.0-20-generic x86_64                       */
/*      Source: sm.lisp                                               */
#include <ecl/ecl-cmp.h>
#include "sm.h"
/*      function definition for MAKE-VERTEX-T                         */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L1make_vertex_t(cl_narg narg, ...)
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
  cl_object v1;
  cl_object v2;
  cl_object v3;
  cl_object v4;
  cl_object v5;
  ecl_va_list args; ecl_va_start(args,narg,narg,0);
  {
   cl_object keyvars[10];
   cl_parse_key(args,5,L1make_vertex_tkeys,keyvars,NULL,FALSE);
   ecl_va_end(args);
   v1 = keyvars[0];
   v2 = keyvars[1];
   v3 = keyvars[2];
   v4 = keyvars[3];
   v5 = keyvars[4];
  }
  value0 = si_make_structure(6, VV[4], v1, v2, v3, v4, v5);
  return value0;
 }
}
/*      function definition for COIN                                  */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L2coin()
{
 cl_object T0;
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  T0 = cl_random(1, ecl_make_fixnum(2));
  value0 = ecl_make_bool(ecl_number_equalp(ecl_make_fixnum(0),T0));
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for TAKE                                  */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L3take(cl_object v1seq, cl_object v2n)
{
 cl_object T0, T1;
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  {
   cl_object v3;
   v3 = v1seq;
   if (ECL_CONSP(v3)) { goto L3; }
   if (v3==ECL_NIL) { goto L3; }
   if (ECL_VECTORP(v3)) { goto L3; }
   v1seq = si_do_check_type(v3, ECL_SYM("SEQUENCE",741), ECL_NIL, VV[7]);
L3:;
  }
  {
   cl_object v3;
   v3 = v2n;
   if (ECL_FIXNUMP(v3)||ECL_BIGNUMP(v3)) { goto L10; }
   v2n = si_do_check_type(v3, ECL_SYM("INTEGER",437), ECL_NIL, VV[8]);
L10:;
  }
  {
   cl_fixnum v3l;
   v3l = ecl_length(v1seq);
   if (!(ecl_number_compare(v2n,ecl_make_fixnum(0))>=0)) { goto L14; }
   T0 = (ecl_number_compare(v2n,ecl_make_fixnum(v3l))<=0?v2n:ecl_make_fixnum(v3l));
   value0 = cl_subseq(3, v1seq, ecl_make_fixnum(0), T0);
   return value0;
L14:;
   if (!(ecl_number_compare(v2n,ecl_make_fixnum(0))<0)) { goto L16; }
   T0 = ecl_plus(v2n,ecl_make_fixnum(v3l));
   T1 = (ecl_number_compare(ecl_make_fixnum(0),T0)>=0?ecl_make_fixnum(0):T0);
   value0 = cl_subseq(3, v1seq, T1, ecl_make_fixnum(v3l));
   return value0;
L16:;
   if (!(ecl_number_equalp(v2n,ecl_make_fixnum(0)))) { goto L18; }
   value0 = cl_subseq(3, v1seq, ecl_make_fixnum(0), ecl_make_fixnum(0));
   return value0;
L18:;
   value0 = ECL_NIL;
   cl_env_copy->nvalues = 1;
   return value0;
  }
 }
}
/*      function definition for DROP                                  */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L4drop(cl_object v1seq, cl_object v2n)
{
 cl_object T0, T1;
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  {
   cl_fixnum v3l;
   v3l = ecl_length(v1seq);
   {
    cl_object v4;
    v4 = v1seq;
    if (ECL_CONSP(v4)) { goto L4; }
    if (v4==ECL_NIL) { goto L4; }
    if (ECL_VECTORP(v4)) { goto L4; }
    v1seq = si_do_check_type(v4, ECL_SYM("SEQUENCE",741), ECL_NIL, VV[7]);
L4:;
   }
   {
    cl_object v4;
    v4 = v2n;
    if (ECL_FIXNUMP(v4)||ECL_BIGNUMP(v4)) { goto L11; }
    v2n = si_do_check_type(v4, ECL_SYM("INTEGER",437), ECL_NIL, VV[8]);
L11:;
   }
   if (!(ecl_number_compare(v2n,ecl_make_fixnum(0))>=0)) { goto L14; }
   T0 = (ecl_number_compare(v2n,ecl_make_fixnum(v3l))<=0?v2n:ecl_make_fixnum(v3l));
   value0 = cl_subseq(3, v1seq, T0, ecl_make_fixnum(v3l));
   return value0;
L14:;
   if (!(ecl_number_compare(v2n,ecl_make_fixnum(0))<0)) { goto L16; }
   T0 = ecl_plus(v2n,ecl_make_fixnum(v3l));
   T1 = (ecl_number_compare(ecl_make_fixnum(0),T0)>=0?ecl_make_fixnum(0):T0);
   value0 = cl_subseq(3, v1seq, ecl_make_fixnum(0), T1);
   return value0;
L16:;
   if (!(ecl_number_equalp(v2n,ecl_make_fixnum(0)))) { goto L18; }
   value0 = v1seq;
   cl_env_copy->nvalues = 1;
   return value0;
L18:;
   value0 = ECL_NIL;
   cl_env_copy->nvalues = 1;
   return value0;
  }
 }
}
/*      function definition for STR-LAST                              */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L5str_last(cl_object v1str)
{
 cl_object T0;
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  {
   cl_object v2;
   v2 = v1str;
   if (ECL_STRINGP(v2)) { goto L3; }
   v1str = si_do_check_type(v2, ECL_SYM("STRING",805), ECL_NIL, VV[11]);
L3:;
  }
  {
   cl_fixnum v2l;
   v2l = ecl_length(v1str);
   goto L9;
L8:;
   si_assert_failure(1, VV[12]);
L9:;
   if ((v2l)>(0)) { goto L12; }
   goto L8;
L12:;
   T0 = ecl_minus(ecl_make_fixnum(v2l),ecl_make_fixnum(1));
   value0 = cl_subseq(3, v1str, T0, ecl_make_fixnum(v2l));
   return value0;
  }
 }
}
/*      function definition for VERTEX-1-ENTRY                        */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L6vertex_1_entry()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = ecl_print(VV[14],ECL_NIL);
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for VERTEX-2-ENTRY                        */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L7vertex_2_entry()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = ecl_print(VV[16],ECL_NIL);
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for VERTEX-3-ENTRY                        */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L8vertex_3_entry()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = ecl_print(VV[18],ECL_NIL);
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for VERTEX-4-ENTRY                        */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L9vertex_4_entry()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = ecl_print(VV[20],ECL_NIL);
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for VERTEX-1-DO                           */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L10vertex_1_do()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = ecl_print(VV[22],ECL_NIL);
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for VERTEX-2-DO                           */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L11vertex_2_do()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = ecl_print(VV[24],ECL_NIL);
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for VERTEX-3-DO                           */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L12vertex_3_do()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = ecl_print(VV[26],ECL_NIL);
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for VERTEX-4-DO                           */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L13vertex_4_do()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = ecl_print(VV[28],ECL_NIL);
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for VERTEX-1-EXIT                         */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L14vertex_1_exit()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = ecl_print(VV[30],ECL_NIL);
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for VERTEX-2-EXIT                         */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L15vertex_2_exit()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = ecl_print(VV[32],ECL_NIL);
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for VERTEX-3-EXIT                         */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L16vertex_3_exit()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = ecl_print(VV[34],ECL_NIL);
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for VERTEX-4-EXIT                         */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L17vertex_4_exit()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = ecl_print(VV[36],ECL_NIL);
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for ACT-A                                 */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L18act_a()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = ecl_print(VV[38],ECL_NIL);
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for ACT-B                                 */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L19act_b()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = ecl_print(VV[40],ECL_NIL);
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for ACT-C                                 */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L20act_c()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = ecl_print(VV[42],ECL_NIL);
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for ACT-D                                 */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L21act_d()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = ecl_print(VV[44],ECL_NIL);
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for ACT-NA                                */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L22act_na()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = ecl_print(VV[46],ECL_NIL);
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for GUARD-X                               */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L23guard_x()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = L2coin();
  return value0;
 }
}
/*      function definition for GUARD-Y                               */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L24guard_y()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = L2coin();
  return value0;
 }
}
/*      function definition for GUARD-Z                               */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L25guard_z()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = L2coin();
  return value0;
 }
}
/*      function definition for GUARD-TRUE                            */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L26guard_true()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = ECL_T;
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for GUARD-FALSE                           */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L27guard_false()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = ECL_NIL;
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for GUARD-NA                              */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L28guard_na()
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  value0 = ECL_T;
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      local function DEFVERTEX                                      */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object LC29defvertex(cl_object v1, cl_object v2)
{
 cl_object T0, T1, T2, T3, T4, T5, T6, T7, T8, T9;
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  {
   cl_object v3;
   cl_object v4nym;
   cl_object v5evt_tbl;
   v3 = ecl_cdr(v1);
   if (!(v3==ECL_NIL)) { goto L3; }
   si_dm_too_few_arguments(v1);
L3:;
   {
    cl_object v6;
    v6 = ecl_car(v3);
    v3 = ecl_cdr(v3);
    v4nym = v6;
   }
   if (!(v3==ECL_NIL)) { goto L9; }
   si_dm_too_few_arguments(v1);
L9:;
   {
    cl_object v6;
    v6 = ecl_car(v3);
    v3 = ecl_cdr(v3);
    v5evt_tbl = v6;
   }
   if (Null(v3)) { goto L14; }
   si_dm_too_many_arguments(v1);
L14:;
   {
    cl_object v6dynvar;
    cl_object v7entry;
    cl_object v8doo;
    cl_object v9exit;
    cl_object v10vtxsym;
    v6dynvar = cl_format(3, ECL_NIL, VV[55], v4nym);
    v7entry = cl_format(3, ECL_NIL, VV[56], v4nym);
    v8doo = cl_format(3, ECL_NIL, VV[57], v4nym);
    v9exit = cl_format(3, ECL_NIL, VV[58], v4nym);
    {
     cl_object v11s;
     v11s = cl_make_string_input_stream(3, v6dynvar, ecl_make_fixnum(0), ECL_NIL);
     v10vtxsym = cl_read(1, v11s);
    }
    T0 = cl_list(4, ECL_SYM("FORMAT",387), ECL_NIL, VV[59], v4nym);
    {
     cl_object v11s;
     v11s = cl_make_string_input_stream(3, v7entry, ecl_make_fixnum(0), ECL_NIL);
     T1 = cl_read(1, v11s);
    }
    T2 = cl_list(2, ECL_SYM("FUNCTION",396), T1);
    {
     cl_object v11s;
     v11s = cl_make_string_input_stream(3, v8doo, ecl_make_fixnum(0), ECL_NIL);
     T3 = cl_read(1, v11s);
    }
    T4 = cl_list(2, ECL_SYM("FUNCTION",396), T3);
    {
     cl_object v11s;
     v11s = cl_make_string_input_stream(3, v9exit, ecl_make_fixnum(0), ECL_NIL);
     T5 = cl_read(1, v11s);
    }
    T6 = cl_list(2, ECL_SYM("FUNCTION",396), T5);
    T7 = cl_list(11, VV[3], ECL_SYM("NAME",1278), T0, VV[60], T2, VV[61], T4, VV[62], T6, VV[63], v5evt_tbl);
    T8 = cl_list(3, ECL_SYM("DEFPARAMETER",285), v10vtxsym, T7);
    T9 = cl_list(3, ECL_SYM("PUSH",677), v10vtxsym, VV[53]);
    value0 = cl_list(3, ECL_SYM("PROGN",671), T8, T9);
    return value0;
   }
  }
 }
}
/*      function definition for EVAL-FIRST-ADMISSIBLE-TRIPLE          */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L30eval_first_admissible_triple(cl_object v1triples)
{
 cl_object T0;
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  if (Null(v1triples)) { goto L2; }
  {
   cl_object v2triple;
   cl_object v3guard;
   cl_object v4action;
   cl_object v5new_vertex;
   v2triple = ecl_car(v1triples);
   v3guard = ecl_car(v2triple);
   v4action = ecl_cadr(v2triple);
   T0 = ecl_caddr(v2triple);
   v5new_vertex = cl_eval(T0);
   if (Null(ecl_function_dispatch(cl_env_copy,v3guard)(0))) { goto L8; }
   if (Null(v5new_vertex)) { goto L8; }
   T0 = ecl_function_dispatch(cl_env_copy,VV[114])(1, ecl_symbol_value(VV[68])) /*  VERTEX-T-EXIT-AC */;
   ecl_function_dispatch(cl_env_copy,T0)(0);
   ecl_function_dispatch(cl_env_copy,v4action)(0);
   cl_set(VV[68],v5new_vertex);
   T0 = ecl_function_dispatch(cl_env_copy,VV[115])(1, ecl_symbol_value(VV[68])) /*  VERTEX-T-ENTRY-AC */;
   ecl_function_dispatch(cl_env_copy,T0)(0);
   goto L1;
L8:;
   T0 = ecl_function_dispatch(cl_env_copy,VV[116])(1, ecl_symbol_value(VV[68])) /*  VERTEX-T-NAME */;
   cl_format(3, ECL_T, VV[70], T0);
   T0 = ecl_cdr(v1triples);
   L30eval_first_admissible_triple(T0);
   goto L1;
  }
L2:;
  T0 = ecl_function_dispatch(cl_env_copy,VV[116])(1, ecl_symbol_value(VV[68])) /*  VERTEX-T-NAME */;
  cl_format(3, ECL_T, VV[71], T0);
L1:;
  value0 = ecl_symbol_value(VV[68]);
  cl_env_copy->nvalues = 1;
  return value0;
 }
}
/*      function definition for SM-ENGINE                             */
/*      optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L31sm_engine(cl_object v1event_symbol)
{
 cl_object T0, T1;
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 ecl_cs_check(cl_env_copy,value0);
 {
TTL:
  {
   cl_object v2line;
   T1 = ecl_function_dispatch(cl_env_copy,VV[118])(1, ecl_symbol_value(VV[68])) /*  VERTEX-T-EVT-TBL */;
   T0 = ecl_assql(v1event_symbol,T1);
   v2line = ecl_cdr(T0);
   if (Null(v2line)) { goto L3; }
   value0 = L30eval_first_admissible_triple(v2line);
   return value0;
L3:;
   T0 = ecl_function_dispatch(cl_env_copy,VV[116])(1, ecl_symbol_value(VV[68])) /*  VERTEX-T-NAME */;
   cl_format(4, ECL_T, VV[73], T0, v1event_symbol);
   value0 = ecl_symbol_value(VV[68]);
   cl_env_copy->nvalues = 1;
   return value0;
  }
 }
}

#include "sm.data"
#ifdef __cplusplus
extern "C"
#endif
ECL_DLLEXPORT void _eclkYoiUpRPXONmP_UbVc3K41(cl_object flag)
{
 const cl_env_ptr cl_env_copy = ecl_process_env();
 cl_object value0;
 cl_object *VVtemp;
 if (flag != OBJNULL){
 Cblock = flag;
 #ifndef ECL_DYNAMIC_VV
 flag->cblock.data = VV;
 #endif
 flag->cblock.data_size = VM;
 flag->cblock.temp_data_size = VMtemp;
 flag->cblock.data_text = compiler_data_text;
 flag->cblock.cfuns_size = compiler_cfuns_size;
 flag->cblock.cfuns = compiler_cfuns;
 flag->cblock.source = make_constant_base_string("/home/brianbeckman/Documents/GitHub/state_machine_common_lisp/lisp/sm.lisp");
 return;}
 #ifdef ECL_DYNAMIC_VV
 VV = Cblock->cblock.data;
 #endif
 Cblock->cblock.data_text = (const cl_object *)"@EcLtAg:_eclkYoiUpRPXONmP_UbVc3K41@";
 VVtemp = Cblock->cblock.temp_data;
 ECL_DEFINE_SETF_FUNCTIONS
  si_define_structure(15, VV[0], VVtemp[0], ECL_NIL, ECL_NIL, VVtemp[1], VVtemp[2], VV[1], ECL_NIL, ECL_NIL, ECL_NIL, VVtemp[3], ecl_make_fixnum(5), ECL_NIL, ECL_NIL, VV[2]);
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[0], ECL_SYM("LOCATION",1777), VVtemp[4], VVtemp[5]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[3], ECL_SYM("LOCATION",1777), VVtemp[6], VVtemp[5]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[3], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, VVtemp[7]) /*  ANNOTATE */;
  VV[4]= cl_find_class(1, VV[0]);
  ecl_cmp_defun(VV[79]);                          /*  MAKE-VERTEX-T   */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[5], ECL_SYM("LOCATION",1777), VVtemp[8], VVtemp[9]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[5], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[85]);                          /*  COIN            */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[6], ECL_SYM("LOCATION",1777), VVtemp[10], VVtemp[11]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[6], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, VVtemp[12]) /*  ANNOTATE */;
  ecl_cmp_defun(VV[86]);                          /*  TAKE            */
  si_set_documentation(3, VV[6], ECL_SYM("FUNCTION",396), VVtemp[13]);
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[9], ECL_SYM("LOCATION",1777), VVtemp[14], VVtemp[15]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[9], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, VVtemp[12]) /*  ANNOTATE */;
  ecl_cmp_defun(VV[87]);                          /*  DROP            */
  si_set_documentation(3, VV[9], ECL_SYM("FUNCTION",396), VVtemp[16]);
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[10], ECL_SYM("LOCATION",1777), VVtemp[17], VVtemp[18]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[10], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, VVtemp[19]) /*  ANNOTATE */;
  ecl_cmp_defun(VV[88]);                          /*  STR-LAST        */
  si_set_documentation(3, VV[10], ECL_SYM("FUNCTION",396), VVtemp[20]);
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[13], ECL_SYM("LOCATION",1777), VVtemp[21], VVtemp[22]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[13], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[89]);                          /*  VERTEX-1-ENTRY  */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[15], ECL_SYM("LOCATION",1777), VVtemp[23], VVtemp[24]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[15], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[90]);                          /*  VERTEX-2-ENTRY  */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[17], ECL_SYM("LOCATION",1777), VVtemp[25], VVtemp[26]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[17], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[91]);                          /*  VERTEX-3-ENTRY  */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[19], ECL_SYM("LOCATION",1777), VVtemp[27], VVtemp[28]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[19], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[92]);                          /*  VERTEX-4-ENTRY  */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[21], ECL_SYM("LOCATION",1777), VVtemp[29], VVtemp[30]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[21], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[93]);                          /*  VERTEX-1-DO     */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[23], ECL_SYM("LOCATION",1777), VVtemp[31], VVtemp[32]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[23], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[94]);                          /*  VERTEX-2-DO     */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[25], ECL_SYM("LOCATION",1777), VVtemp[33], VVtemp[34]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[25], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[95]);                          /*  VERTEX-3-DO     */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[27], ECL_SYM("LOCATION",1777), VVtemp[35], VVtemp[36]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[27], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[96]);                          /*  VERTEX-4-DO     */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[29], ECL_SYM("LOCATION",1777), VVtemp[37], VVtemp[38]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[29], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[97]);                          /*  VERTEX-1-EXIT   */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[31], ECL_SYM("LOCATION",1777), VVtemp[39], VVtemp[40]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[31], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[98]);                          /*  VERTEX-2-EXIT   */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[33], ECL_SYM("LOCATION",1777), VVtemp[41], VVtemp[42]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[33], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[99]);                          /*  VERTEX-3-EXIT   */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[35], ECL_SYM("LOCATION",1777), VVtemp[43], VVtemp[44]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[35], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[100]);                         /*  VERTEX-4-EXIT   */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[37], ECL_SYM("LOCATION",1777), VVtemp[45], VVtemp[46]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[37], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[101]);                         /*  ACT-A           */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[39], ECL_SYM("LOCATION",1777), VVtemp[47], VVtemp[48]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[39], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[102]);                         /*  ACT-B           */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[41], ECL_SYM("LOCATION",1777), VVtemp[49], VVtemp[50]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[41], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[103]);                         /*  ACT-C           */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[43], ECL_SYM("LOCATION",1777), VVtemp[51], VVtemp[52]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[43], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[104]);                         /*  ACT-D           */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[45], ECL_SYM("LOCATION",1777), VVtemp[53], VVtemp[54]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[45], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[105]);                         /*  ACT-NA          */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[47], ECL_SYM("LOCATION",1777), VVtemp[55], VVtemp[56]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[47], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[106]);                         /*  GUARD-X         */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[48], ECL_SYM("LOCATION",1777), VVtemp[57], VVtemp[58]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[48], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[107]);                         /*  GUARD-Y         */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[49], ECL_SYM("LOCATION",1777), VVtemp[59], VVtemp[60]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[49], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[108]);                         /*  GUARD-Z         */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[50], ECL_SYM("LOCATION",1777), VVtemp[61], VVtemp[62]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[50], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[109]);                         /*  GUARD-TRUE      */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[51], ECL_SYM("LOCATION",1777), VVtemp[63], VVtemp[64]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[51], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[110]);                         /*  GUARD-FALSE     */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[52], ECL_SYM("LOCATION",1777), VVtemp[65], VVtemp[66]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[52], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, ECL_NIL) /*  ANNOTATE */;
  ecl_cmp_defun(VV[111]);                         /*  GUARD-NA        */
  si_Xmake_special(VV[53]);
  cl_set(VV[53],ECL_NIL);
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[53], ECL_SYM("LOCATION",1777), VVtemp[67], VVtemp[68]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[54], ECL_SYM("LOCATION",1777), VVtemp[69], VVtemp[70]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[54], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, VVtemp[71]) /*  ANNOTATE */;
  ecl_cmp_defmacro(VV[112]);                      /*  DEFVERTEX       */
 {
  cl_object T0, T1, T2, T3;
  si_Xmake_special(VV[64]);
  T0 = cl_format(3, ECL_NIL, VV[59], VVtemp[72]);
  T1 = ecl_fdefinition(VV[13]);
  T2 = ecl_fdefinition(VV[21]);
  T3 = ecl_fdefinition(VV[29]);
  cl_set(VV[64],L1make_vertex_t(10, ECL_SYM("NAME",1278), T0, VV[60], T1, VV[61], T2, VV[62], T3, VV[63], VVtemp[73]));
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[64], ECL_SYM("LOCATION",1777), VVtemp[74], VVtemp[75]) /*  ANNOTATE */;
  cl_set(VV[53],CONS(ecl_symbol_value(VV[64]),ecl_symbol_value(VV[53])));
 }
 {
  cl_object T0, T1, T2, T3;
  si_Xmake_special(VV[65]);
  T0 = cl_format(3, ECL_NIL, VV[59], VVtemp[76]);
  T1 = ecl_fdefinition(VV[15]);
  T2 = ecl_fdefinition(VV[23]);
  T3 = ecl_fdefinition(VV[31]);
  cl_set(VV[65],L1make_vertex_t(10, ECL_SYM("NAME",1278), T0, VV[60], T1, VV[61], T2, VV[62], T3, VV[63], VVtemp[77]));
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[65], ECL_SYM("LOCATION",1777), VVtemp[78], VVtemp[79]) /*  ANNOTATE */;
  cl_set(VV[53],CONS(ecl_symbol_value(VV[65]),ecl_symbol_value(VV[53])));
 }
 {
  cl_object T0, T1, T2, T3;
  si_Xmake_special(VV[66]);
  T0 = cl_format(3, ECL_NIL, VV[59], VVtemp[80]);
  T1 = ecl_fdefinition(VV[17]);
  T2 = ecl_fdefinition(VV[25]);
  T3 = ecl_fdefinition(VV[33]);
  cl_set(VV[66],L1make_vertex_t(10, ECL_SYM("NAME",1278), T0, VV[60], T1, VV[61], T2, VV[62], T3, VV[63], VVtemp[81]));
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[66], ECL_SYM("LOCATION",1777), VVtemp[82], VVtemp[83]) /*  ANNOTATE */;
  cl_set(VV[53],CONS(ecl_symbol_value(VV[66]),ecl_symbol_value(VV[53])));
 }
 {
  cl_object T0, T1, T2, T3;
  si_Xmake_special(VV[67]);
  T0 = cl_format(3, ECL_NIL, VV[59], VVtemp[84]);
  T1 = ecl_fdefinition(VV[19]);
  T2 = ecl_fdefinition(VV[27]);
  T3 = ecl_fdefinition(VV[35]);
  cl_set(VV[67],L1make_vertex_t(10, ECL_SYM("NAME",1278), T0, VV[60], T1, VV[61], T2, VV[62], T3, VV[63], VVtemp[85]));
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[67], ECL_SYM("LOCATION",1777), VVtemp[86], VVtemp[87]) /*  ANNOTATE */;
  cl_set(VV[53],CONS(ecl_symbol_value(VV[67]),ecl_symbol_value(VV[53])));
 }
  si_Xmake_special(VV[68]);
  cl_set(VV[68],ecl_symbol_value(VV[64]));
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[68], ECL_SYM("LOCATION",1777), VVtemp[88], VVtemp[89]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[69], ECL_SYM("LOCATION",1777), VVtemp[90], VVtemp[91]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[69], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, VVtemp[92]) /*  ANNOTATE */;
  ecl_cmp_defun(VV[113]);                         /*  EVAL-FIRST-ADMISSIBLE-TRIPLE */
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[72], ECL_SYM("LOCATION",1777), VVtemp[93], VVtemp[94]) /*  ANNOTATE */;
  ecl_function_dispatch(cl_env_copy,ECL_SYM("ANNOTATE",1771))(4, VV[72], ECL_SYM("LAMBDA-LIST",998), ECL_NIL, VVtemp[95]) /*  ANNOTATE */;
  ecl_cmp_defun(VV[117]);                         /*  SM-ENGINE       */
  {
   bool v1;
   v1 = ecl_equal(ecl_symbol_value(VV[68]),ecl_symbol_value(VV[64]));
   ecl_print(ecl_make_bool(v1),ECL_NIL);
  }
  {
   bool v1;
   v1 = (ecl_symbol_value(VV[68]))==(ecl_symbol_value(VV[64]));
   ecl_print(ecl_make_bool(v1),ECL_NIL);
  }
 {
  cl_object T0;
  T0 = L31sm_engine(VV[74]);
  {
   bool v1;
   v1 = (T0)==(ecl_symbol_value(VV[64]));
   ecl_print(ecl_make_bool(v1),ECL_NIL);
  }
 }
 {
  cl_object T0;
  T0 = L31sm_engine(VV[75]);
  {
   bool v1;
   v1 = (T0)==(ecl_symbol_value(VV[64]));
   ecl_print(ecl_make_bool(v1),ECL_NIL);
  }
 }
 {
  cl_object T0;
  T0 = L31sm_engine(VV[76]);
  {
   bool v1;
   v1 = (T0)==(ecl_symbol_value(VV[64]));
   ecl_print(ecl_make_bool(v1),ECL_NIL);
  }
 }
 {
  cl_object T0;
  T0 = L31sm_engine(VV[77]);
  {
   bool v1;
   v1 = (T0)==(ecl_symbol_value(VV[64]));
   ecl_print(ecl_make_bool(v1),ECL_NIL);
  }
 }
 {
  cl_object T0;
  T0 = L31sm_engine(VV[78]);
  {
   bool v1;
   v1 = (T0)==(ecl_symbol_value(VV[66]));
   ecl_print(ecl_make_bool(v1),ECL_NIL);
  }
 }
}
