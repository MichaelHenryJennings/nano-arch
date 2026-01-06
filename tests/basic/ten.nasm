lower r1 <- $1
r1 <= r0 - r1
lower r5 <- $200
r6 <= r5 - r0
lower r3 <- $10
lower r7 <- $6
r2 <= r2 - r1
@r6 <= r2
r6 <= r6 - r1
r4 <= r3 - r2
jump ?r4 -> r7
halt @r5 #r2
