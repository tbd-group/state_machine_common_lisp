#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "sm_types.h"
#include "sm_engine.h"
#include "sm_data.h"


extern states_set_t   state_sets[];     // sm_data.c
extern sm_data_t      sm_data;          // sm_data.c

void                  sm_engine_init();
void                  sm_engine_start();
static const state_t* get_state_ptr(int nset, states_enm state);
static void           call_transition_action(trnAc_fp tfp);
static states_enm     process_states_set(int set, events_enm ev);


void sm_engine(events_enm ev) {
    if ( !(ev < ev_eol) ) {
        printf("sm_engine(ev), ev = %d\n", ev);
    }
    assert(ev < ev_eol);
    for (int set=0;  set < sm_data.sets_count;  ++set) {
        states_enm next_state_id = process_states_set(set, ev);
    }
}

states_enm process_states_set(int set, events_enm ev) {
    const state_t* csp = state_sets[set].csp;       // current state pointer
    states_enm next_state_id = csp->state_id;       // initialize next to current
    int tr;
    for (tr=0; tr < csp->transitions_count; tr++ ) {// iterate through current state's event table 
        if ( csp->ev_tbl[tr] == ev ) {              // check whether current state has to process the received event
            printf(">> event:  %s, tr = %d, csp = %s\n", all_events_str[ev], tr, all_states_str[csp->state_id]);
            printf(">> guard:  ");
            if ( false == (csp->guard_tbl[tr])() ) {// check guard transition 
               printf("failed\n");  
               continue;                            // transition guard failed, go and check the next entry in the event table
            }
            printf("succeeded\n");                  // if we got here in means that we need to switch to the next state
            next_state_id = csp->next_st_tbl[tr];
            ++sm_data.states_counter[next_state_id];
            ++sm_data.events_counter[ev]; 
            if ( next_state_id == st_internal ) {   // skip actions in case of internal transition, stay in the current state
                printf(">> state:  same (internal transition)\n");
                call_transition_action(csp->tafp_tbl[tr]);
            }
            else {
                printf(">> exit:   ");
                (csp->exit_ac)(csp->state_data);    // call current state's exit action
                call_transition_action(csp->tafp_tbl[tr]);

                const state_t* pnext = get_state_ptr(set, next_state_id);
                printf(">> state:  %s\n", all_states_str[pnext->state_id] );
                printf(">> entry:  ");
                (pnext->entry_ac)(pnext->state_data);
                // iterate through the do_action tbl
                printf(">> do:\n");
                for (int ndo=0; ndo < pnext->do_ac_count; ++ndo) { // 'ndo' stands for num of DOs
                    pnext->do_ac_tbl[ndo](pnext->state_data);
                }
                state_sets[set].csp = pnext;
            }
            break;                                  // transition completed, quit event table iteration
        }
    } // for transitions_count
    return next_state_id; 
} // process_states_set


static const state_t* get_state_ptr(int nset, states_enm state) {
    states_set_t* pset = &state_sets[nset];
    state_t* sp = NULL;
    //@ur. consider implementing it as two-dimensional look-up table
    for (int i=0; i < pset->states_count; ++i ) {
        if ( state == pset->state_tbl[i].state_id )
            return &pset->state_tbl[i];
    }
    assert(0);  // we should not get here!
}

static bool leagal_event(set,e) {
    int i=0;
    for (i=0; i < ev_eol; ++i) {
        if ( e == state_sets[set].events_tbl[i] ) {
            return true;
        }
    }
    printf("!!! %s, [%d]\n", all_events_str[e], i);
    return false;
}

void sm_engine_init() {

    // the next loop is just to let nicer prints
    int max_len_event_name = 0;
    for (int i=0; i < get_num_of_all_events(); ++i) {
        int len = strlen(all_events_str[i]);
        if (len > max_len_event_name) {
            max_len_event_name = len;
        }
    }

    // ev_eol must be the last event in the events list
    int last_ev_pos = get_num_of_all_events() - 1;
    assert(ev_eol == events_enm_tbl[last_ev_pos]);

    printf(">> sm_data.sets_count = %d\n", sm_data.sets_count);
    for (int nset=0;  nset < sm_data.sets_count;  ++nset) {
        states_set_t* pset = &state_sets[nset];

        pset->csp = get_state_ptr(nset, pset->init_state);   // init the first state of the set

        printf("\n>> state_sets[%d].name = \"%s\"  .states_count = %d\n>>\n",
                                  nset, pset->set_name, pset->states_count );
        for (int s=0;  s < pset->states_count;  ++s) {
            const state_t* pstate = &pset->state_tbl[s];
            printf(">>\t%s\n", all_states_str[pstate->state_id]);
            for (int t=0;  t < pstate->transitions_count;  ++t) {
                events_enm e = pstate->ev_tbl[t];
                assert(leagal_event(nset,e));
                const char* const guard_str = pstate->guard_str_tbl[t];
                const char* const trnAc_str = pstate->trnAc_str_tbl[t];
                states_enm        nxtSt_id  = pstate->next_st_tbl[t];
                const char* const nxtSt_str = all_states_str[nxtSt_id];
                printf(">>\t\t%2d. %-*s %-16s %-16s %-16s\n",
                        t+1, max_len_event_name, all_events_str[e],
                        guard_str, trnAc_str, nxtSt_str);
            }
        }
    }
    puts("");
}

void sm_engine_start() {
    for (int nset=0;  nset < sm_data.sets_count;  ++nset) {
        states_set_t* pset = &state_sets[nset];
        printf(">> state:  %s\n", all_states_str[pset->csp->state_id]); // state name
        printf(">> entry:  ");
        (pset->csp->entry_ac)(pset->csp->state_data);                   // the first transition action
    }
    puts("");
}

static void call_transition_action(trnAc_fp tfp) {
    printf(">> action: ");
    (tfp)(&sm_data);
}


