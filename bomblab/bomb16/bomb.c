
#include <stdio.h>
#include <stdlib.h>
#include "support.h"
#include "phases.h"

FILE *infile;

int main(int argc, char *argv[])
{
    char *input;

    /* When run with no arguments, the bomb reads its input lines 
     * from standard input. */
    if (argc == 1) {  
	infile = stdin;
    } 

    /* When run with one argument <file>, the bomb reads from <file> 
     * until EOF, and then switches to standard input. Thus, as you 
     * defuse each phase, you can add its defusing string to <file> and
     * avoid having to retype it. */
    else if (argc == 2) {
	if (!(infile = fopen(argv[1], "r"))) {
	    printf("%s: Error: Couldn't open %s\n", argv[0], argv[1]);
	    exit(8);
	}
    }

    /* You can't call the bomb with more than 1 command line argument. */
    else {
	printf("Usage: %s [<input_file>]\n", argv[0]);
	exit(8);
    }

    /* Do all sorts of secret stuff that makes the bomb harder to defuse. */
    initialize_bomb();

    printf("Welcome to my fiendish little bomb. You have 6 phases with\n");
    printf("which to blow yourself up. Have a nice day!\n");

#if !defined(STAGEFLAG) || STAGEFLAG == 1
    /* Hmm...  Six phases must be more secure than one phase! */
    input = read_line();             /* Get input                   */
    phase_1(input);                  /* Run the phase               */
    phase_defused();                 /* Drat!  They figured it out!
				      * Let me know how they did it. */
#endif
    printf("Phase 1 defused. How about the next one?\n");

    /* The second phase is harder.  No one will ever figure out
     * how to defuse this... */
#if !defined(STAGEFLAG) || STAGEFLAG == 2
    input = read_line();
    phase_2(input);
    phase_defused();
#endif
    printf("That's number 2.  Keep going!\n");

    /* I guess this is too easy so far.  Some more complex code will
     * confuse people. */
#if !defined(STAGEFLAG) || STAGEFLAG == 3
    input = read_line();
    phase_3(input);
    phase_defused();
#endif
    printf("Halfway there!\n");

    /* Oh yeah?  Well, how good is your math?  Try on this saucy problem! */
#if !defined(STAGEFLAG) || STAGEFLAG == 4
    input = read_line();
    phase_4(input);
    phase_defused();
#endif
    printf("So you got that one.  Try this one.\n");
    
    /* Round and 'round in memory we go, where we stop, the bomb blows! */
#if !defined(STAGEFLAG) || STAGEFLAG == 5
    input = read_line();
    phase_5(input);
    phase_defused();
#endif
    printf("Good work!  On to the next...\n");

    /* Last phase */
#if !defined(STAGEFLAG) || STAGEFLAG == 6
    input = read_line();
    phase_6(input);
    phase_defused();
#endif
    
    return 0;
}
