$SIZES LVR=80 LNP4=40000

$PROB TEST_NONMEM_NODES
$INPUT C ID TIME AMT DV DOSE EVID CMT
$DATA data_example3_nm.csv
IGNORE=C
$SUBROUTINES ADVAN13 TOL=9

$MODEL
COMP(ABSORB)
COMP(CENTR)

$PK

$DES
DADT(1) = NNa(state=A(1),min_init=0.5,max_init=10) + DOSE * NNat(state=T,min_init=0.5,max_init=10,time_nn=TRUE)
DADT(2) = -NNa(state=A(1),min_init=0.5,max_init=10) - DOSE * NNat(state=T,min_init=0.5,max_init=10,time_nn=TRUE) + NNc(state=A(2),min_init=0.5,max_init=3) + DOSE * NNt(state=T,min_init=0.5,max_init=10,time_nn=TRUE)

$ERROR
  Cc = A(2)
  Y=Cc*(1+EPS(1)) + EPS(2)

$THETA

$OMEGA

$SIGMA
  0.1
  0.1

$ESTIMATION METHOD=1 MAXEVAL=9999 INTER PRINT=5
$TABLE ID TIME DV IPRED=CIPRED AMT IRES=CIRES IWRE=CIWRES NOPRINT FILE=nm_example3.tab