mov @0 $1 r1
sub r0 r1 r1
mov @0 $200 r5
sub r5 r0 r6
mov @0 $10 r3
mov @0 $3 r7
sub r2 r1 r2
st r6 r2
sub r6 r1 r6
sub r3 r2 r4
jmp r4 r7 +0
halt r5 r2
