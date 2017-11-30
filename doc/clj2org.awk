#!/usr/bin/awk -f
#
# Example usage:
# $ cd doc
# $ ./clj2org.awk ../src/spectraining/core.clj > core.org
#

BEGIN {
    code = 0
    text = 1
    mode = text
    prevmode = text
    fenced = 0
}

function save_blankline () {
    # Save *all* the blank space :)
    holdline[holdnum++]=$0;
}
function dump_saved_blanklines () {
    # Push out held blank lines.
    for ( i = 0; i < holdnum; i++ )
        print holdline[i];
    holdnum = 0
}

NF {
    if ( /^;/ ) {
        mode = text
        sub(/^;+/, "")  # hulk smash!
        if ( prevmode == code ) {
            print "#+END_SRC"
            dump_saved_blanklines()
        }
        # Check for fenced code in text mode.
        if ( /^```/ ) {
            if ( fenced )
                $0 = "#+END_EXAMPLE"  # hulk smash!
            else
                $0 = "#+BEGIN_EXAMPLE"  # hulk smash!
            fenced = xor(fenced,1)
        }
    } else {
        mode = code
        if ( prevmode == text ) print "#+BEGIN_SRC clojure"
    }
}

{
    if ( !NF && mode == code )
        save_blankline()
    else {
        if ( prevmode == code )
            dump_saved_blanklines()
        print
        if ( NF ) prevmode = mode
    }
}
