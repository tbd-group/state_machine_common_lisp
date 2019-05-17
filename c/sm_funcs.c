#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "sm_types.h"

static int  get_counters_sum(const sm_data_t* smd);

static void print_msg(const char* func, int prm) {
    int         width = strlen(func);
    const int   columns = 30;
    int         space = columns - width; // calculate the space needed for uniform indentation to the right

    printf("%s() %*d\n", func, space, prm);
}

// the printf format in the 1st line in the next block is equivalent to the other lines 
state_data_t* do_ac_EID_handle       (state_data_t* std) { printf("%*c%s()\n", 11,' ', __func__); return std; }
state_data_t* do_ac_TEMP_readBoard   (state_data_t* std) { printf("           %s()\n", __func__); return std; }
state_data_t* do_ac_TEMP_readMotor   (state_data_t* std) { printf("           %s()\n", __func__); return std; }
state_data_t* do_ac_readTemps        (state_data_t* std) { printf("           %s()\n", __func__); return std; }
state_data_t* do_ac_CAN_comm         (state_data_t* std) { printf("           %s()\n", __func__); return std; }
state_data_t* do_ac_getSpeedReference(state_data_t* std) { printf("           %s()\n", __func__); return std; }
state_data_t* do_ac_controlMotorSpeed(state_data_t* std) { printf("           %s()\n", __func__); return std; }
state_data_t* do_ac_COMM_rxConfig    (state_data_t* std) { printf("           %s()\n", __func__); return std; }


state_data_t* st_initializing_entry(state_data_t* std)  { print_msg(__func__, std->entry_counter++ ); return std; }
state_data_t* st_unconfigured_entry(state_data_t* std)  { print_msg(__func__, std->entry_counter++ ); return std; }
state_data_t* st_disabled_entry    (state_data_t* std)  { print_msg(__func__, std->entry_counter++ ); return std; }
state_data_t* st_engaged_entry     (state_data_t* std)  { print_msg(__func__, std->entry_counter++ ); return std; }
state_data_t* st_control_entry     (state_data_t* std)  { print_msg(__func__, std->entry_counter++ ); return std; }
state_data_t* st_fault_entry       (state_data_t* std)  { print_msg(__func__, std->entry_counter++ ); return std; }

state_data_t* st_initializing_exit(state_data_t* std)   { print_msg(__func__, std->exit_counter++  ); return std; }
state_data_t* st_unconfigured_exit(state_data_t* std)   { print_msg(__func__, std->exit_counter++  ); return std; }
state_data_t* st_disabled_exit    (state_data_t* std)   { print_msg(__func__, std->exit_counter++  ); return std; }
state_data_t* st_engaged_exit     (state_data_t* std)   { print_msg(__func__, std->exit_counter++  ); return std; }
state_data_t* st_control_exit     (state_data_t* std)   { print_msg(__func__, std->exit_counter++  ); return std; }
state_data_t* st_fault_exit       (state_data_t* std)   { print_msg(__func__, std->exit_counter++  ); return std; }

//:ur:state_data_t* state_a_entry(state_data_t* std)  { print_msg(__func__, std->entry_counter++ ); return std; }
//:ur:state_data_t* state_b_entry(state_data_t* std)  { print_msg(__func__, std->entry_counter++ ); return std; }
//:ur:state_data_t* state_c_entry(state_data_t* std)  { print_msg(__func__, std->entry_counter++ ); return std; }
//:ur:state_data_t* state_d_entry(state_data_t* std)  { print_msg(__func__, std->entry_counter++ ); return std; }
//:ur:state_data_t* state_e_entry(state_data_t* std)  { print_msg(__func__, std->entry_counter++ ); return std; }

//:ur:state_data_t* state_a_exit(state_data_t* std)   { print_msg(__func__, std->exit_counter++  ); return std; }
//:ur:state_data_t* state_b_exit(state_data_t* std)   { print_msg(__func__, std->exit_counter++  ); return std; }
//:ur:state_data_t* state_c_exit(state_data_t* std)   { print_msg(__func__, std->exit_counter++  ); return std; }
//:ur:state_data_t* state_d_exit(state_data_t* std)   { print_msg(__func__, std->exit_counter++  ); return std; }
//:ur:state_data_t* state_e_exit(state_data_t* std)   { print_msg(__func__, std->exit_counter++  ); return std; }

sm_data_t* act_a (sm_data_t* smd)               { print_msg(__func__, get_counters_sum(smd)); return smd; }
sm_data_t* act_b (sm_data_t* smd)               { print_msg(__func__, get_counters_sum(smd)); return smd; }
sm_data_t* act_c (sm_data_t* smd)               { print_msg(__func__, get_counters_sum(smd)); return smd; }
sm_data_t* act_d (sm_data_t* smd)               { print_msg(__func__, get_counters_sum(smd)); return smd; }
sm_data_t* act_na(sm_data_t* smd)               { print_msg(__func__, get_counters_sum(smd)); return smd; }
sm_data_t* act_inc_cntr(sm_data_t* smd)         { print_msg(__func__, get_counters_sum(smd)); return smd; }

bool guard_x()       { printf("%s(), ", __func__ ); return rand() & 0x01; }
bool guard_y()       { printf("%s(), ", __func__ ); return rand() & 0x01; }
bool guard_z()       { printf("%s(), ", __func__ ); return rand() & 0x01; }
bool guard_true()    { printf("%s(), ", __func__ ); return true ;         }
bool guard_false()   { printf("%s(), ", __func__ ); return false;         }
bool guard_na()      { printf("%s(), ", __func__ ); return true ;         }

static int get_counters_sum(const sm_data_t* smd) { 
    int cntrs_sum = 0;
    for (int i=0; i < ev_eol; ++i) {
        cntrs_sum += smd->events_counter[i]; 
    }
    return cntrs_sum;
}

