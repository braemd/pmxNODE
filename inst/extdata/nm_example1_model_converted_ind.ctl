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

lWc_11 = THETA(2)
lWc_12 = THETA(3)
lWc_13 = THETA(4)
lWc_14 = THETA(5)
lWc_15 = THETA(6)
lbc_11 = THETA(7)
lbc_12 = THETA(8)
lbc_13 = THETA(9)
lbc_14 = THETA(10)
lbc_15 = THETA(11)
lWc_21 = THETA(12)
lWc_22 = THETA(13)
lWc_23 = THETA(14)
lWc_24 = THETA(15)
lWc_25 = THETA(16)
lbc_21 = THETA(17)
lWt_11 = THETA(18)
lWt_12 = THETA(19)
lWt_13 = THETA(20)
lWt_14 = THETA(21)
lWt_15 = THETA(22)
lbt_11 = THETA(23)
lbt_12 = THETA(24)
lbt_13 = THETA(25)
lbt_14 = THETA(26)
lbt_15 = THETA(27)
lWt_21 = THETA(28)
lWt_22 = THETA(29)
lWt_23 = THETA(30)
lWt_24 = THETA(31)
lWt_25 = THETA(32)
 
etaWc_11 = ETA(2)
etaWc_12 = ETA(3)
etaWc_13 = ETA(4)
etaWc_14 = ETA(5)
etaWc_15 = ETA(6)
etabc_11 = ETA(7)
etabc_12 = ETA(8)
etabc_13 = ETA(9)
etabc_14 = ETA(10)
etabc_15 = ETA(11)
etaWc_21 = ETA(12)
etaWc_22 = ETA(13)
etaWc_23 = ETA(14)
etaWc_24 = ETA(15)
etaWc_25 = ETA(16)
etabc_21 = ETA(17)
etaWt_11 = ETA(18)
etaWt_12 = ETA(19)
etaWt_13 = ETA(20)
etaWt_14 = ETA(21)
etaWt_15 = ETA(22)
etabt_11 = ETA(23)
etabt_12 = ETA(24)
etabt_13 = ETA(25)
etabt_14 = ETA(26)
etabt_15 = ETA(27)
etaWt_21 = ETA(28)
etaWt_22 = ETA(29)
etaWt_23 = ETA(30)
etaWt_24 = ETA(31)
etaWt_25 = ETA(32)
 
Wc_11 = lWc_11 * EXP(etaWc_11)
Wc_12 = lWc_12 * EXP(etaWc_12)
Wc_13 = lWc_13 * EXP(etaWc_13)
Wc_14 = lWc_14 * EXP(etaWc_14)
Wc_15 = lWc_15 * EXP(etaWc_15)
bc_11 = lbc_11 * EXP(etabc_11)
bc_12 = lbc_12 * EXP(etabc_12)
bc_13 = lbc_13 * EXP(etabc_13)
bc_14 = lbc_14 * EXP(etabc_14)
bc_15 = lbc_15 * EXP(etabc_15)
Wc_21 = lWc_21 * EXP(etaWc_21)
Wc_22 = lWc_22 * EXP(etaWc_22)
Wc_23 = lWc_23 * EXP(etaWc_23)
Wc_24 = lWc_24 * EXP(etaWc_24)
Wc_25 = lWc_25 * EXP(etaWc_25)
bc_21 = lbc_21 * EXP(etabc_21)
Wt_11 = lWt_11 * EXP(etaWt_11)
Wt_12 = lWt_12 * EXP(etaWt_12)
Wt_13 = lWt_13 * EXP(etaWt_13)
Wt_14 = lWt_14 * EXP(etaWt_14)
Wt_15 = lWt_15 * EXP(etaWt_15)
bt_11 = lbt_11 * EXP(etabt_11)
bt_12 = lbt_12 * EXP(etabt_12)
bt_13 = lbt_13 * EXP(etabt_13)
bt_14 = lbt_14 * EXP(etabt_14)
bt_15 = lbt_15 * EXP(etabt_15)
Wt_21 = lWt_21 * EXP(etaWt_21)
Wt_22 = lWt_22 * EXP(etaWt_22)
Wt_23 = lWt_23 * EXP(etaWt_23)
Wt_24 = lWt_24 * EXP(etaWt_24)
Wt_25 = lWt_25 * EXP(etaWt_25)
 
$DES
A1 = A(1)
hc_1 = 0
hc_2 = 0
hc_3 = 0
hc_4 = 0
hc_5 = 0
hc_1_thres = Wc_11 * A1 + bc_11
hc_2_thres = Wc_12 * A1 + bc_12
hc_3_thres = Wc_13 * A1 + bc_13
hc_4_thres = Wc_14 * A1 + bc_14
hc_5_thres = Wc_15 * A1 + bc_15
IF (hc_1_thres.GT.hc_1) hc_1 = hc_1_thres
IF (hc_2_thres.GT.hc_2) hc_2 = hc_2_thres
IF (hc_3_thres.GT.hc_3) hc_3 = hc_3_thres
IF (hc_4_thres.GT.hc_4) hc_4 = hc_4_thres
IF (hc_5_thres.GT.hc_5) hc_5 = hc_5_thres
NNc = Wc_21 * hc_1 + Wc_22 * hc_2 + Wc_23 * hc_3 + Wc_24 * hc_4 + Wc_25 * hc_5 + bc_21
ht_1 = 0
ht_2 = 0
ht_3 = 0
ht_4 = 0
ht_5 = 0
ht_1_thres = -(Wt_11**2) * T + bt_11
ht_2_thres = -(Wt_12**2) * T + bt_12
ht_3_thres = -(Wt_13**2) * T + bt_13
ht_4_thres = -(Wt_14**2) * T + bt_14
ht_5_thres = -(Wt_15**2) * T + bt_15
IF (ht_1_thres.GT.ht_1) ht_1 = ht_1_thres
IF (ht_2_thres.GT.ht_2) ht_2 = ht_2_thres
IF (ht_3_thres.GT.ht_3) ht_3 = ht_3_thres
IF (ht_4_thres.GT.ht_4) ht_4 = ht_4_thres
IF (ht_5_thres.GT.ht_5) ht_5 = ht_5_thres
NNt = Wt_21 * ht_1 + Wt_22 * ht_2 + Wt_23 * ht_3 + Wt_24 * ht_4 + Wt_25 * ht_5
DADT(1) = NNc + DOSE * NNt

$ERROR
  Cc = A(1)/V
  Y=Cc*(1+EPS(1)) + EPS(2)

$THETA
2 ; [V]

0 FIX ; [lWc_11]
-2.17E-01 ; [lWc_12]
0 FIX ; [lWc_13]
6.40E-02 ; [lWc_14]
-2.53E+00 ; [lWc_15]
0 FIX ; [lbc_11]
2.24E+00 ; [lbc_12]
0 FIX ; [lbc_13]
-2.39E-02 ; [lbc_14]
6.16E+00 ; [lbc_15]
0 FIX ; [lWc_21]
-5.41E-02 ; [lWc_22]
0 FIX ; [lWc_23]
-1.08E+00 ; [lWc_24]
-6.80E-03 ; [lWc_25]
1.46E-01 ; [lbc_21]
0 FIX ; [lWt_11]
1.65E-01 ; [lWt_12]
0 FIX ; [lWt_13]
0 FIX ; [lWt_14]
-1.16E-01 ; [lWt_15]
0 FIX ; [lbt_11]
3.06E-01 ; [lbt_12]
0 FIX ; [lbt_13]
0 FIX ; [lbt_14]
1.63E-02 ; [lbt_15]
0 FIX ; [lWt_21]
-1.89E-01 ; [lWt_22]
0 FIX ; [lWt_23]
0 FIX ; [lWt_24]
7.24E+00 ; [lWt_25]
 
$OMEGA
0.1 ; [V]

0 FIX ; [etaWc_11]
0.1 ; [etaWc_12]
0 FIX ; [etaWc_13]
0.1 ; [etaWc_14]
0.1 ; [etaWc_15]
0 FIX ; [etabc_11]
0.1 ; [etabc_12]
0 FIX ; [etabc_13]
0.1 ; [etabc_14]
0.1 ; [etabc_15]
0 FIX ; [etaWc_21]
0.1 ; [etaWc_22]
0 FIX ; [etaWc_23]
0.1 ; [etaWc_24]
0.1 ; [etaWc_25]
0.1 ; [etabc_21]
0 FIX ; [etaWt_11]
0.1 ; [etaWt_12]
0 FIX ; [etaWt_13]
0 FIX ; [etaWt_14]
0.1 ; [etaWt_15]
0 FIX ; [etabt_11]
0.1 ; [etabt_12]
0 FIX ; [etabt_13]
0 FIX ; [etabt_14]
0.1 ; [etabt_15]
0 FIX ; [etaWt_21]
0.1 ; [etaWt_22]
0 FIX ; [etaWt_23]
0 FIX ; [etaWt_24]
0.1 ; [etaWt_25]

$SIGMA
  0.1
  0.1

$ESTIMATION METHOD=1 MAXEVAL=9999 INTER PRINT=5
$TABLE ID TIME DV IPRED=CIPRED AMT IRES=CIRES IWRE=CIWRES NOPRINT FILE=nm_example1.tab
