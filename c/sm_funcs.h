#pragma once

#include <stdbool.h>
#include "sm_types.h"

extern state_data_t* do_ac_EID_handle       (state_data_t* std); 
extern state_data_t* do_ac_TEMP_readBoard   (state_data_t* std);
extern state_data_t* do_ac_TEMP_readMotor   (state_data_t* std);
extern state_data_t* do_ac_CAN_comm         (state_data_t* std);
extern state_data_t* do_ac_readTemps        (state_data_t* std);
extern state_data_t* do_ac_CAN_comm         (state_data_t* std);
extern state_data_t* do_ac_getSpeedReference(state_data_t* std);
extern state_data_t* do_ac_controlMotorSpeed(state_data_t* std);
extern state_data_t* do_ac_COMM_rxConfig    (state_data_t* std);

extern state_data_t* st_initializing_entry(state_data_t* std);
extern state_data_t* st_unconfigured_entry(state_data_t* std);
extern state_data_t* st_disabled_entry    (state_data_t* std);
extern state_data_t* st_engaged_entry     (state_data_t* std);
extern state_data_t* st_control_entry     (state_data_t* std);
extern state_data_t* st_fault_entry       (state_data_t* std);

extern state_data_t* st_initializing_exit (state_data_t* std);
extern state_data_t* st_unconfigured_exit (state_data_t* std);
extern state_data_t* st_disabled_exit     (state_data_t* std);
extern state_data_t* st_engaged_exit      (state_data_t* std);
extern state_data_t* st_control_exit      (state_data_t* std);
extern state_data_t* st_fault_exit        (state_data_t* std);

extern bool guard_true();
extern bool guard_false();

extern sm_data_t* act_a (sm_data_t*);
extern sm_data_t* act_b (sm_data_t*);
extern sm_data_t* act_c (sm_data_t*);
extern sm_data_t* act_d (sm_data_t*);
extern sm_data_t* act_na(sm_data_t*);
extern sm_data_t* act_inc_cntr(sm_data_t*);

