mov @0 $0 r0 ; constant 0
mov @0 $1 r1 ; constant 1
sub r0 r1 r31 ; constant -1
mov @0 $16 r2 ; pointer to start of array
mov @0 $26 r3 ; pointer to end of array
mov @0 $0 r4 ; sum accumulator
mov @0 $1000 r5 ; output pointer
mov @0 $8 r6 ; start of loop
ld r2 r7 ; load from array
sub r0 r7 r7 ; negate
sub r4 r7 r4 ; accumulate
sub r2 r31 r2 ; move pointer
sub r3 r2 r8 ; test pointer
jmp r8 r6 ; loop
st r5 r4 ; write output
halt r5 r1 ; done!
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
