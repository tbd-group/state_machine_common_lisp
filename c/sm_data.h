#pragma once

#include "sm_types.h"

// At least one empty line must separate between transition
// tables, it's because of the '\' macro operator.

// set 1 ////////////////////////////////////////////
#define SET_1_INITIAL_STATE     st_initializing

#define SET_1_EVENTS \
  EV(ev_initialized) EV(ev_engage)   EV(ev_start_speed_control)     \
  EV(ev_cmd_stop)    EV(ev_timeout)  EV(ev_fatal_error)             \
  EV(ev_eol)

#define STATE_INIT_EVENTS_AND_TRANSITIONS                           \
  ASSOC(ev_initialized, guard_true, act_na,  st_unconfigured)       \
  ASSOC(ev_fatal_error, guard_true, act_na,  st_fault       )       \

#define STATE_UNCONFIG_EVENTS_AND_TRANSITIONS                       \
  ASSOC(ev_timeout,     guard_true,  act_inc_cntr,  st_disabled  )  \
  ASSOC(ev_fatal_error, guard_true,  act_na, st_fault)              \

#define STATE_DISABLD_EVENTS_AND_TRANSITIONS                        \
  ASSOC(ev_engage,      guard_true,  act_na,        st_engaged   )  \
  ASSOC(ev_timeout,     guard_true,  act_inc_cntr,  st_disabled  )  \
  ASSOC(ev_fatal_error, guard_true,  act_na,        st_fault     )  \

#define STATE_ENGAGED_EVENTS_AND_TRANSITIONS                        \
  ASSOC(ev_start_speed_control, guard_true,  act_na,  st_control )  \
  ASSOC(ev_fatal_error,         guard_true,  act_na,  st_fault   )  \

#define STATE_CNTRL_EVENTS_AND_TRANSITIONS                          \
  ASSOC(ev_cmd_stop,     guard_true,  act_na,  st_engaged )         \
  ASSOC(ev_fatal_error,  guard_true,  act_na,  st_fault   )         \

#define STATE_FAULT_EVENTS_AND_TRANSITIONS                          \
  ASSOC(ev_eol,          guard_false, act_na,  st_fault   )

extern       events_enm str_to_eventEnum(const char *str);
extern       int        get_num_of_all_events();
extern const events_enm events_enm_tbl[];
extern const char*      all_events_str[];
extern const char*      all_states_str[];
