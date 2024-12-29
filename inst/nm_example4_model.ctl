$SIZES LVR=80 LNP4=40000

$PROB TEST_NONMEM_NODES
$INPUT C ID TIME AMT DV TYPE CMT
$DATA data_example4_nm.csv
IGNORE=C
$SUBROUTINES ADVAN13 TOL=9

$MODEL
COMP(CENTR)
COMP(RESPO)

$PK
lV = THETA(1)
lK = THETA(2)
lKIN = THETA(3)
lRES0 = THETA(4)
etaV = ETA(1)
etaK = ETA(2)
etaKIN = ETA(3)
etaRES0 = ETA(4)
V = lV * EXP(etaV)
K = lK * EXP(etaK)
KIN = lKIN * EXP(etaKIN)
RES0 = lRES0 * EXP(etaRES0)
KOUT = KIN/RES0

A_0(2) = RES0

$DES
DADT(1) = -K * A(1)
CONC = A(1)/V
DADT(2) = KIN * (1 - NNr(state=CONC,min_init=0.5,max_init=5)) - KOUT * A(2)

$ERROR
IF (TYPE.EQ.1) THEN
   Cc = A(1)/V
   Y=Cc*(1+EPS(1)) + EPS(2)
ENDIF

IF (TYPE.EQ.2) THEN
   RESP = A(2)
   Y=RESP*(1+EPS(3)) + EPS(4)
ENDIF

$THETA
2 ; [V]
0.1 ; [K]
40 ; [KIN]
100 ; [R0]

$OMEGA
0.1 ; [V]
0.1 ; [K]
0.1 ; [KIN]
0.1 ; [R0]

$SIGMA
  0.1
  0.1
  0.1
  0.1

$ESTIMATION METHOD=1 MAXEVAL=9999 INTER PRINT=5
$TABLE ID TIME DV IPRED=CIPRED AMT IRES=CIRES TYPE IWRE=CIWRES NOPRINT FILE=nm_example4.tab