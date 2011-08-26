# a simple test of the trivial R preprocessor
#
# run like this:
#
# ./Rpp A=yes '$B="no"' '$C=0' < Rpptest.R > /tmp/tmp.R; R --quiet --vanilla < /tmp/tmp.R
#
###.if $A
print("PASS: A")
###.if $B eq "no"
print("PASS: B")
###.else
print("FAIL: B")
###.endif
###.else
print("FAIL: A")
###.endif
###.if $C == 1
print("FAIL: C")
###.elsif $C == 2
print("FAIL: C2")
###.elsif $C == 0
print("PASS: C")
###.else
print("FAIL: C3")
###.endif
