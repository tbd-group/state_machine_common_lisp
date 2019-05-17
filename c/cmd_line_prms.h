#pragma once


typedef enum { eit_char,  eit_str   } event_input_type;
typedef enum { wmsg_hide, wmsg_show } welcome_message_cfg;

int cmd_line_prms(int argc, char *const *argv);
event_input_type    get_event_input_type();
welcome_message_cfg get_welcome_message_cfg();

