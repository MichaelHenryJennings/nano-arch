mov @0 $0 r0
mov @0 $1 r1
sub r0 r1 r31
mov @0 $18 r2
mov @0 $28 r3
mov @0 $0 r4
mov @0 $1000 r5
mov @0 $0 r6
mov @0 $9 r7
ld r2 r8
sub r0 r8 r8
sub r4 r8 r4
sub r6 r31 r6
sub r2 r31 r2
sub r3 r2 r9
jmp r9 r7
st r5 r4
halt r5 r1
data $1
data $2
data $3
data $4
data $5
data $6
data $7
data $8
data $9
data $10
