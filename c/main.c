
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "cmd_line_prms.h"
#include "sm_types.h"
#include "sm_data.h"
#include "sm_engine.h"

#include "quickcheck4c.h"

#define Esc 27


static void         print_opening_msg(const char* prog_name);
static events_enm   get_str_event();
static events_enm   get_event();

static events_enm first_event = ev_initialized;
static events_enm last_event  = ev_timeout;
QCC_GenValue * set_1_gen_event () {
    return QCC_genIntR (first_event, last_event);
}

QCC_TestStatus set_1_tranx (QCC_GenValue **vals, int len, QCC_Stamp **stamp) {
    const int idx = *QCC_getValue (vals, 0, int*);
    const events_enm ev = events_enm_tbl[first_event + idx - 1];
    sm_engine (ev);
    // now, extract information from the engine.
    
    int retval = 1;
    return retval;
}


int main(int argc, char **argv)
{
    // define that std(out|in|err) should not be buffered
    if ( 0 != setvbuf(stdout, NULL, _IONBF, 0) ) {
        printf("setvbuf(stdout, NULL, _IONBF, 0) failed\n");
    }
    if ( 0 != setvbuf(stdin,  NULL, _IONBF, 0) ) {
        printf("setvbuf(stdin,  NULL, _IONBF, 0) failed\n");
    }
    if ( 0 != setvbuf(stderr, NULL, _IONBF, 0) ) {
        printf("stderr, NULL, _IONBF, 0 failed\n");
    }

    sm_engine_init();
    sm_engine_start();

    // quickcheck
    //:ur: QCC_testForAll (100, 1000, set_1_tranx, 1, set_1_gen_event);

    if ( cmd_line_prms(argc, argv) ) {
        return 1;    // parse command line parameters, quit in case of illegal param
    }

    if ( wmsg_show == get_welcome_message_cfg() ) {
        print_opening_msg(argv[0]);
    }
    // interactive repl

    while (1) {
        events_enm ev = get_event();
        if ( ev_esc == ev ) {
            break;       // stop running
        }
        if ( ev_na  == ev ) {
            continue;    // unknown event, wait for the next event
        }
        sm_engine(ev);   // process the received event
    }
    return 0;
}

void print_opening_msg(const char* prog_name)
{
    event_input_type eit = get_event_input_type();
    printf("\n>> events input mode: ");
    if ( eit_str == eit ) {
        printf("strings, for character mode use: %s [-e char]\n\n", prog_name);
        puts("");
    } else {
        assert(eit_char == eit);
        printf("characters, for string mode use: %s -e str\n\n", prog_name);
        puts(">> \tEsc to quit\n");
    }
} // print_opening_msg

events_enm get_str_event()
{
    events_enm ret_val;
#define NBYTES 256
    size_t nbytes = NBYTES;
    int    bytes_read;
    char*  my_string;

    my_string = (char*)malloc(NBYTES);
    if ( NULL == my_string ) {
        perror("Unable to allocate buffer");
        exit(1);
    }
    bytes_read = getline(&my_string, &nbytes, stdin);

    if ( bytes_read >= 1 ) {
        my_string[bytes_read-1] = 0;
    }
    if ( 2 == bytes_read && Esc == my_string[0] ) {     // in case Esc key pressed
        ret_val = ev_esc;
    } else {
        ret_val = str_to_eventEnum(my_string);
    }
    free(my_string);
    return ret_val;

#undef NBYTES
}

events_enm get_event()
{
    event_input_type eit = get_event_input_type();
    if ( eit_str  == eit ) {
        return get_str_event();
    }

    assert(eit_char == eit );   // must be in 'character' input mode

    char c = getchar();

    events_enm ret_val = ev_na;
    if ( c == Esc ) {
        ret_val = ev_esc;
    } else if ( c >= '1'  &&  c <= '6' ) {
        ret_val = events_enm_tbl[c-'1'+first_event];
    }

    return ret_val;
}

