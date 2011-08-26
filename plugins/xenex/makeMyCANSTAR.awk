# makeMyCANSTAR.awk - create a list of gcc-mingw-compatible declarations for the
#                     CANSTAR library of Russel Technologies Inc.
#
# Call as gawk -f makeMyCANSTAR.awk DECORATIONSFILE DECLARATIONFILE
#

BEGIN {
## read the decorations file derived by running nm
## This gives the decorated and undecorated names, separated by whitespace.

    while (0 != getline < ARGV[1])
	deco[$2]=$1;
    ARGV[1] = "";
}


## for each line in the original xenex declaration header file, write
## a new line with the decorated function name as an alias

/^PRE .*WINAPIX/{
    sub(/WINAPIX/, "");
    sub(/^PRE/, "PRE extern __stdcall __attribute__((alias (\"" deco[$3] "\" )) )");
    gsub(/\r/, "");
    print;
    next;
}

## other lines are copied over as-is
{
    gsub(/\r/, "");
    print;
}