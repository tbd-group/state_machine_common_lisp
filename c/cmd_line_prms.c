
#include <unistd.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "cmd_line_prms.h"


typedef struct {
    welcome_message_cfg wmc;    // hide or display the welcome message
    event_input_type    eit;    // events are identified by characters ('1', '2', 'x', 'a'... or by strings, e.g. "xyz"
} sys_config;

static sys_config sys_cfg = {   // set default values
    .wmc = wmsg_show,           // display welcome message and instructions
    .eit = eit_char             // events represented by characters
};

welcome_message_cfg get_welcome_message_cfg() {
    return sys_cfg.wmc;
}

event_input_type get_event_input_type() {
    return sys_cfg.eit; 
}

int cmd_line_prms(int argc, char *const *argv) {
    // this function is base on the example in:
    // www.gnu.org/software/libc/manual/html_node/Example-of-Getopt.html
    int   index;
    int   chr;

    #define MAX_ARG_LEN 15
    char e_arg[MAX_ARG_LEN+1] = {0};
    char m_arg[MAX_ARG_LEN+1] = {0};
    opterr = 0;

    while ((chr = getopt(argc, argv, "he:m:")) != -1) {
        switch (chr) {
            case 'h':
                puts("\tsmEngine - an example of a tables-based state machine");
                puts("\tsmEngine [option]...");
                puts("\t\t-h displays this message");
                puts("\t\t-e str|char  - event representation type, string or character(default)");
                puts("\t\t-m hide|show - show(default) or hide the welcome message");
                puts("\tfor more details: http://adnconfluence.corp.amazon.com/display/PA/SM+C");
                exit(1);
                break;
            case 'e':
                strncpy(e_arg, optarg, MAX_ARG_LEN);
                e_arg[MAX_ARG_LEN] = 0;
                if ( 0 == strcmp("str",  e_arg) ) { sys_cfg.eit = eit_str; }
                break;
            case 'm':
                strncpy(m_arg, optarg, MAX_ARG_LEN);
                m_arg[MAX_ARG_LEN] = 0;
                if ( 0 == strcmp("hide", m_arg) ) { sys_cfg.wmc = wmsg_hide; }
                break;
            case '?':
                if (optopt == 'e' || optopt == 'm')
                    fprintf(stderr, "Option -%c requires an argument\n", optopt);
                else if (isprint(optopt))
                    fprintf(stderr, "Unknown option `-%c\n", optopt);
                else
                    fprintf(stderr, "Unknown option character `\\x%x'\n", optopt);
                abort();
        }
    }
    printf("e_arg = %s, m_arg = %s\n", e_arg, m_arg);
    for (index = optind;  index < argc;  index++) {
        printf("Non-option argument %s\n", argv[index]);
    }
    return 0;
}

