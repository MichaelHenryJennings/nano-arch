lower r0 <- $0 ; constant 0
lower r1 <- $1 ; constant 1
r31 <= r0 - r1 ; constant -1
lower r2 <- $16 ; pointer to start of array
lower r3 <- $26 ; pointer to end of array
lower r4 <- $0 ; sum accumulator
lower r5 <- $1000 ; output pointer
lower r6 <- $8 ; start of loop
r7 <= @r2 ; load from array
r7 <= r0 - r7 ; negate
r4 <= r4 - r7 ; accumulate
r2 <= r2 - r31 ; move pointer
r8 <= r3 - r2 ; test pointer
jump ?r8 -> r6 ; loop
@r5 <= r4 ; write output
halt @r5 #r1 ; done!
$1
$2
$3
$4
$5
$6
$7
$8
$9
$10
