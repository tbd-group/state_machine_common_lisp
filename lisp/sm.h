
#ifdef ECL_DYNAMIC_VV
static cl_object *VV;
#else
static cl_object VV[VM];
#endif

#ifdef __cplusplus
extern "C" {
#endif
#ifdef __cplusplus
extern cl_object si_define_structure(...);
#else
extern cl_object si_define_structure();
#endif
static cl_object L1make_vertex_t(cl_narg, ...);
#define L1make_vertex_tkeys (&VV[80])
static cl_object L2coin();
#ifdef __cplusplus
extern cl_object si_set_documentation(...);
#else
extern cl_object si_set_documentation();
#endif
static cl_object L3take(cl_object , cl_object );
static cl_object L4drop(cl_object , cl_object );
static cl_object L5str_last(cl_object );
static cl_object L6vertex_1_entry();
static cl_object L7vertex_2_entry();
static cl_object L8vertex_3_entry();
static cl_object L9vertex_4_entry();
static cl_object L10vertex_1_do();
static cl_object L11vertex_2_do();
static cl_object L12vertex_3_do();
static cl_object L13vertex_4_do();
static cl_object L14vertex_1_exit();
static cl_object L15vertex_2_exit();
static cl_object L16vertex_3_exit();
static cl_object L17vertex_4_exit();
static cl_object L18act_a();
static cl_object L19act_b();
static cl_object L20act_c();
static cl_object L21act_d();
static cl_object L22act_na();
static cl_object L23guard_x();
static cl_object L24guard_y();
static cl_object L25guard_z();
static cl_object L26guard_true();
static cl_object L27guard_false();
static cl_object L28guard_na();
static cl_object LC29defvertex(cl_object , cl_object );
extern cl_object si_dm_too_few_arguments(cl_object);
extern cl_object si_dm_too_many_arguments(cl_object);
static cl_object L30eval_first_admissible_triple(cl_object );
static cl_object L31sm_engine(cl_object );
static cl_object Cblock;
#define VM 119
#define VMtemp 96
#define ECL_DEFINE_SETF_FUNCTIONS 
#ifdef __cplusplus
}
#endif
/*
 * Exported Lisp functions
 */
#define compiler_cfuns_size 31
static const struct ecl_cfun compiler_cfuns[] = {
 /*t,m,narg,padding,name,block,entry*/
{0,0,-1,0,ecl_make_fixnum(79),ecl_make_fixnum(3),(cl_objectfn)L1make_vertex_t,ECL_NIL,ecl_make_fixnum(0)},
{0,0,0,0,ecl_make_fixnum(85),ecl_make_fixnum(5),(cl_objectfn)L2coin,ECL_NIL,ecl_make_fixnum(56)},
{0,0,2,0,ecl_make_fixnum(86),ecl_make_fixnum(6),(cl_objectfn)L3take,ECL_NIL,ecl_make_fixnum(90)},
{0,0,2,0,ecl_make_fixnum(87),ecl_make_fixnum(9),(cl_objectfn)L4drop,ECL_NIL,ecl_make_fixnum(470)},
{0,0,1,0,ecl_make_fixnum(88),ecl_make_fixnum(10),(cl_objectfn)L5str_last,ECL_NIL,ecl_make_fixnum(864)},
{0,0,0,0,ecl_make_fixnum(89),ecl_make_fixnum(13),(cl_objectfn)L6vertex_1_entry,ECL_NIL,ecl_make_fixnum(1074)},
{0,0,0,0,ecl_make_fixnum(90),ecl_make_fixnum(15),(cl_objectfn)L7vertex_2_entry,ECL_NIL,ecl_make_fixnum(1126)},
{0,0,0,0,ecl_make_fixnum(91),ecl_make_fixnum(17),(cl_objectfn)L8vertex_3_entry,ECL_NIL,ecl_make_fixnum(1177)},
{0,0,0,0,ecl_make_fixnum(92),ecl_make_fixnum(19),(cl_objectfn)L9vertex_4_entry,ECL_NIL,ecl_make_fixnum(1228)},
{0,0,0,0,ecl_make_fixnum(93),ecl_make_fixnum(21),(cl_objectfn)L10vertex_1_do,ECL_NIL,ecl_make_fixnum(1279)},
{0,0,0,0,ecl_make_fixnum(94),ecl_make_fixnum(23),(cl_objectfn)L11vertex_2_do,ECL_NIL,ecl_make_fixnum(1328)},
{0,0,0,0,ecl_make_fixnum(95),ecl_make_fixnum(25),(cl_objectfn)L12vertex_3_do,ECL_NIL,ecl_make_fixnum(1376)},
{0,0,0,0,ecl_make_fixnum(96),ecl_make_fixnum(27),(cl_objectfn)L13vertex_4_do,ECL_NIL,ecl_make_fixnum(1424)},
{0,0,0,0,ecl_make_fixnum(97),ecl_make_fixnum(29),(cl_objectfn)L14vertex_1_exit,ECL_NIL,ecl_make_fixnum(1472)},
{0,0,0,0,ecl_make_fixnum(98),ecl_make_fixnum(31),(cl_objectfn)L15vertex_2_exit,ECL_NIL,ecl_make_fixnum(1523)},
{0,0,0,0,ecl_make_fixnum(99),ecl_make_fixnum(33),(cl_objectfn)L16vertex_3_exit,ECL_NIL,ecl_make_fixnum(1573)},
{0,0,0,0,ecl_make_fixnum(100),ecl_make_fixnum(35),(cl_objectfn)L17vertex_4_exit,ECL_NIL,ecl_make_fixnum(1623)},
{0,0,0,0,ecl_make_fixnum(101),ecl_make_fixnum(37),(cl_objectfn)L18act_a,ECL_NIL,ecl_make_fixnum(1673)},
{0,0,0,0,ecl_make_fixnum(102),ecl_make_fixnum(39),(cl_objectfn)L19act_b,ECL_NIL,ecl_make_fixnum(1711)},
{0,0,0,0,ecl_make_fixnum(103),ecl_make_fixnum(41),(cl_objectfn)L20act_c,ECL_NIL,ecl_make_fixnum(1748)},
{0,0,0,0,ecl_make_fixnum(104),ecl_make_fixnum(43),(cl_objectfn)L21act_d,ECL_NIL,ecl_make_fixnum(1785)},
{0,0,0,0,ecl_make_fixnum(105),ecl_make_fixnum(45),(cl_objectfn)L22act_na,ECL_NIL,ecl_make_fixnum(1822)},
{0,0,0,0,ecl_make_fixnum(106),ecl_make_fixnum(47),(cl_objectfn)L23guard_x,ECL_NIL,ecl_make_fixnum(1859)},
{0,0,0,0,ecl_make_fixnum(107),ecl_make_fixnum(48),(cl_objectfn)L24guard_y,ECL_NIL,ecl_make_fixnum(1891)},
{0,0,0,0,ecl_make_fixnum(108),ecl_make_fixnum(49),(cl_objectfn)L25guard_z,ECL_NIL,ecl_make_fixnum(1922)},
{0,0,0,0,ecl_make_fixnum(109),ecl_make_fixnum(50),(cl_objectfn)L26guard_true,ECL_NIL,ecl_make_fixnum(1953)},
{0,0,0,0,ecl_make_fixnum(110),ecl_make_fixnum(51),(cl_objectfn)L27guard_false,ECL_NIL,ecl_make_fixnum(1984)},
{0,0,0,0,ecl_make_fixnum(111),ecl_make_fixnum(52),(cl_objectfn)L28guard_na,ECL_NIL,ecl_make_fixnum(2015)},
{0,0,2,0,ecl_make_fixnum(112),ecl_make_fixnum(54),(cl_objectfn)LC29defvertex,ECL_NIL,ecl_make_fixnum(2077)},
{0,0,1,0,ecl_make_fixnum(113),ecl_make_fixnum(69),(cl_objectfn)L30eval_first_admissible_triple,ECL_NIL,ecl_make_fixnum(3359)},
{0,0,1,0,ecl_make_fixnum(117),ecl_make_fixnum(72),(cl_objectfn)L31sm_engine,ECL_NIL,ecl_make_fixnum(4248)},
};
