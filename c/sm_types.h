#pragma once

#include <stdbool.h>

typedef unsigned int uint;

// the two macros below were copied from
// https://gcc.gnu.org/onlinedocs/cpp/Stringification.html 
#define xstr(s) str(s)
#define str(s)  #s


#define SM_ALL_STATES \
    ST(st_initializing) ST(st_unconfigured) ST(st_disabled)    \
    ST(st_engaged)      ST(st_control)      ST(st_fault)       \
    /* general */ ST(st_internal) ST(st_eol)

#define ST(s) s,
typedef enum { SM_ALL_STATES } states_enm;
#undef  ST

#define SM_ALL_EVENTS \
    EV(ev_initialized) EV(ev_engage)   EV(ev_start_speed_control)   \
    EV(ev_cmd_stop)    EV(ev_timeout)  EV(ev_fatal_error)           \
    EV(ev_esc)         EV(ev_na)       EV(ev_reset)    EV(ev_eol)

#define EV(e) e,
typedef enum { SM_ALL_EVENTS } events_enm;
#undef EV

typedef struct {                                    // demonstrates state-related data
    uint    entry_counter;
    uint    exit_counter;
} state_data_t;

typedef struct {                                    // demonstrates SM-related data
    uint    sets_count;                             // number of sets of states
    uint    states_counter[st_eol];
    uint    events_counter[ev_eol];
} sm_data_t;

typedef state_data_t* (*entAc_fp)(state_data_t*);   // State Entry Action Function Pointer
typedef state_data_t* (*doAc_fp) (state_data_t*);   // list of state 'do' actions
typedef state_data_t* (*extAc_fp)(state_data_t*);   // State Exit Action Function Pointer
typedef sm_data_t*    (*trnAc_fp)(sm_data_t*);      // Transaction Action Function Pointer
typedef bool          (*guard_fp)();                // Guard Function Pointer

typedef struct {
    const states_enm        state_id;               // state ID, one of state_1, state_2, ...
    state_data_t*           state_data;             // state's data members
    entAc_fp                entry_ac;               // pointer to the state's entry action
    const doAc_fp*          do_ac_tbl;              // list of the state's do actions
    const uint              do_ac_count;            // number of do actions per state
    extAc_fp                exit_ac;                // pointer to the state's exit action

    const uint              transitions_count;      // number of transitions in this state
    const events_enm* const ev_tbl;                 // table of events relevant to this state 
    const guard_fp*         guard_tbl;              // guard functions
    const char* const*      guard_str_tbl;          // smae as previous line but as strings
    const trnAc_fp*         tafp_tbl;               // transition actions 
    const char* const*      trnAc_str_tbl;          // smae as previous line but as strings
    const states_enm* const next_st_tbl;            // table of target states
} state_t;

typedef struct {
    const char* const       set_name;
    const states_enm        init_state;
    const uint              states_count;
    const state_t* const    state_tbl;
    const events_enm* const events_tbl;
    const state_t*          csp;
} states_set_t;


