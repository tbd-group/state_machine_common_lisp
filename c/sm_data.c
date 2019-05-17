#include <stdio.h>
#include <string.h>

#include "sm_types.h"
#include "sm_data.h"
#include "sm_funcs.h"

#define EV(e) e,
  const events_enm events_enm_tbl[] = { SM_ALL_EVENTS };
#undef  EV

#define EV(e)   xstr(e),
const char* all_events_str[] = { SM_ALL_EVENTS };
#undef EV

#define ST(s)   xstr(s),
const char* all_states_str[] = { SM_ALL_STATES };
#undef ST


// SET 1 - EVENTS
#define EV(e) e,
static const events_enm set_1_events[] = { SET_1_EVENTS };
#undef EV

// SET 1 - STATE 1 - Transitions
#define EVENTS_AND_TRANSITIONS  STATE_INIT_EVENTS_AND_TRANSITIONS
#define ASSOC(ev, guard, tr_act, next_st) ev,      // extract the first matrix column
static const events_enm tr_initializing_events[] =     { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) guard,   // extract the second matrix column
static guard_fp         tr_initializing_guards[] =     { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) xstr(guard),// convert guards name to strings
static const char*      tr_initializing_guards_str[] = { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) tr_act,  // extract the third matrix column
static trnAc_fp         tr_initializing_trActs[] =     { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) xstr(tr_act),// convert trans action name to strings
static const char*      tr_initializing_trActs_str[] = { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) next_st, // extract the fourth matrix column
static states_enm       tr_initializing_next_st[] =    { EVENTS_AND_TRANSITIONS };
#undef  ASSOC
#undef EVENTS_AND_TRANSITIONS


// SET 1 - STATE 2 - Transitions
#define EVENTS_AND_TRANSITIONS  STATE_UNCONFIG_EVENTS_AND_TRANSITIONS
#define ASSOC(ev, guard, tr_act, next_st) ev,
static const events_enm tr_unconfigured_events[] =     { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) guard,
static guard_fp         tr_unconfigured_guards[] =     { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) xstr(guard),
static const char*      tr_unconfigured_guards_str[] = { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) tr_act,
static trnAc_fp         tr_unconfigured_trActs[] =     { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) xstr(tr_act),
static const char*      tr_unconfigured_trActs_str[] = { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) next_st,
static states_enm       tr_unconfigured_next_st[] =    { EVENTS_AND_TRANSITIONS };
#undef  ASSOC
#undef EVENTS_AND_TRANSITIONS


// SET 1 - STATE 3 - Transitions
#define EVENTS_AND_TRANSITIONS  STATE_DISABLD_EVENTS_AND_TRANSITIONS
#define ASSOC(ev, guard, tr_act, next_st) ev,
static const events_enm tr_disabled_events[] =     { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) guard,
static guard_fp         tr_disabled_guards[] =     { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) xstr(guard),
static const char*      tr_disabled_guards_str[] = { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) tr_act,
static trnAc_fp         tr_disabled_trActs[] =     { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) xstr(tr_act),
static const char*      tr_disabled_trActs_str[] = { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) next_st,
static states_enm       tr_disabled_next_st[] =    { EVENTS_AND_TRANSITIONS };
#undef  ASSOC
#undef EVENTS_AND_TRANSITIONS


// SET 1 - STATE 4 - Transitions
#define EVENTS_AND_TRANSITIONS  STATE_ENGAGED_EVENTS_AND_TRANSITIONS
#define ASSOC(ev, guard, tr_act, next_st) ev,
static const events_enm tr_engaged_events[] =     { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) guard,
static guard_fp         tr_engaged_guards[] =     { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) xstr(guard),
static const char*      tr_engaged_guards_str[] = { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) tr_act,
static trnAc_fp         tr_engaged_trActs[] =     { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) xstr(tr_act),
static const char*      tr_engaged_trActs_str[] = { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) next_st,
static states_enm       tr_engaged_next_st[] =    { EVENTS_AND_TRANSITIONS };
#undef  ASSOC
#undef EVENTS_AND_TRANSITIONS


// SET 1 - STATE 5 - Transitions
#define EVENTS_AND_TRANSITIONS  STATE_CNTRL_EVENTS_AND_TRANSITIONS
#define ASSOC(ev, guard, tr_act, next_st) ev,
static const events_enm tr_control_events[] =     { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) guard,
static guard_fp         tr_control_guards[] =     { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) xstr(guard),
static const char*      tr_control_guards_str[] = { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) tr_act,
static trnAc_fp         tr_control_trActs[] =     { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) xstr(tr_act),
static const char*      tr_control_trActs_str[] = { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) next_st,
static states_enm       tr_control_next_st[] =    { EVENTS_AND_TRANSITIONS };
#undef  ASSOC
#undef EVENTS_AND_TRANSITIONS


// SET 1 - STATE 6 - Transitions
#define EVENTS_AND_TRANSITIONS  STATE_FAULT_EVENTS_AND_TRANSITIONS
#define ASSOC(ev, guard, tr_act, next_st) ev,
static const events_enm tr_fault_events[] =     { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) guard,
static guard_fp         tr_fault_guards[] =     { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) xstr(guard),
static const char*      tr_fault_guards_str[] = { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) tr_act,
static trnAc_fp         tr_fault_trActs[] =     { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) xstr(tr_act),
static const char*      tr_fault_trActs_str[] = { EVENTS_AND_TRANSITIONS };
#undef  ASSOC

#define ASSOC(ev, guard, tr_act, next_st) next_st,
static states_enm       tr_fault_next_st[] =    { EVENTS_AND_TRANSITIONS };
#undef  ASSOC
#undef EVENTS_AND_TRANSITIONS


// the following macro was copied from http://tinyurl.com/gmnmpq7
#define COUNT_OF(x) ((sizeof(x)/sizeof(0[x])) / ((size_t)(!(sizeof(x) % sizeof(0[x])))))

doAc_fp st_initializing_do_tbl[] = {                                                                                                                };
doAc_fp st_unconfigured_do_tbl[] = { do_ac_EID_handle, do_ac_TEMP_readBoard, do_ac_TEMP_readMotor, do_ac_CAN_comm         , do_ac_COMM_rxConfig     };
doAc_fp st_disabled_do_tbl    [] = { do_ac_EID_handle, do_ac_TEMP_readBoard, do_ac_TEMP_readMotor, do_ac_CAN_comm         , do_ac_COMM_rxConfig     };
doAc_fp st_engaged_do_tbl     [] = { do_ac_EID_handle, do_ac_TEMP_readBoard, do_ac_TEMP_readMotor, do_ac_CAN_comm                                   };
doAc_fp st_control_do_tbl     [] = { do_ac_EID_handle, do_ac_readTemps     , do_ac_CAN_comm      , do_ac_controlMotorSpeed, do_ac_getSpeedReference };
doAc_fp st_fault_do_tbl       [] = { do_ac_EID_handle, do_ac_TEMP_readBoard, do_ac_TEMP_readMotor, do_ac_CAN_comm                                   };

#define ADDRESS_OF(x) (&x)
#define STATES_FIELDS(name) name, ADDRESS_OF(name ## _data), name ## _entry, name ## _do_tbl, COUNT_OF(name ## _do_tbl), name ## _exit
#define EVENTS_TBL(name) COUNT_OF(name ## _events), name ## _events
#define TRANSITION_FIELDS(trans_name) trans_name ## _guards, trans_name ## _guards ## _str, trans_name ## _trActs, trans_name ## _trActs ## _str, trans_name ## _next_st

state_data_t  st_initializing_data;
state_data_t  st_unconfigured_data;
state_data_t  st_disabled_data;
state_data_t  st_engaged_data;
state_data_t  st_control_data;
state_data_t  st_fault_data;

const state_t set_1_states_tbl[] = {
 { STATES_FIELDS(st_initializing), EVENTS_TBL(tr_initializing), TRANSITION_FIELDS(tr_initializing) },
 { STATES_FIELDS(st_unconfigured), EVENTS_TBL(tr_unconfigured), TRANSITION_FIELDS(tr_unconfigured) },
 { STATES_FIELDS(st_disabled    ), EVENTS_TBL(tr_disabled    ), TRANSITION_FIELDS(tr_disabled    ) },
 { STATES_FIELDS(st_engaged     ), EVENTS_TBL(tr_engaged     ), TRANSITION_FIELDS(tr_engaged     ) },
 { STATES_FIELDS(st_control     ), EVENTS_TBL(tr_control     ), TRANSITION_FIELDS(tr_control     ) },
 { STATES_FIELDS(st_fault       ), EVENTS_TBL(tr_fault       ), TRANSITION_FIELDS(tr_fault       ) }
};

states_set_t state_sets[] = {
// set_name   init_state           num_of_states               state_tbl         events_tbl    csp - pointer to the current state
       { "ESC", SET_1_INITIAL_STATE, COUNT_OF(set_1_states_tbl), set_1_states_tbl, set_1_events, NULL },
//:ur: { "foo", SET_2_INITIAL_STATE, COUNT_OF(set_2_states_tbl), set_2_states_tbl, set_2_events, NULL }
};

int get_num_of_all_events() {
    return sizeof(events_enm_tbl) / sizeof(events_enm_tbl[0]);
}

sm_data_t sm_data = { COUNT_OF(state_sets), {0}, {0} }; // holds SM data in the highest level

events_enm str_to_eventEnum(const char *str) {
    for (int i=0; i < ev_eol; ++i) {
        if ( 0 == strcmp(str, all_events_str[i] ))
            return events_enm_tbl[i];
    }
    return ev_eol;
}
