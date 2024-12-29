$SIZES LVR=80 LNP4=40000

$PROB TEST_NONMEM_NODES
$INPUT C ID TIME AMT DV DOSE EVID
$DATA data_example1_nm.csv
IGNORE=C
$SUBROUTINES ADVAN13 TOL=9

$MODEL
COMP(CENTR)

$PK
lV = THETA(1)
etaV = ETA(1)
V = lV * EXP(etaV)

$DES
DADT(1) = NNc(state=A(1),min_init=0.5,max_init=5) + DOSE * NNt(state=T,min_init=1,max_init=5,time_nn=TRUE)

$ERROR
  Cc = A(1)/V
  Y=Cc*(1+EPS(1)) + EPS(2)

$THETA
2 ; [V]

$OMEGA
0.1 ; [V]

$SIGMA
  0.1
  0.1

$ESTIMATION METHOD=1 MAXEVAL=9999 INTER PRINT=5
$TABLE ID TIME DV IPRED=CIPRED AMT IRES=CIRES IWRE=CIWRES NOPRINT FILE=nm_example1.tab