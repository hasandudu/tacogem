$STITLE Input file: REPLOOP.INC. Definitions of report parameters. Standard CGE modeling system, Version 1.01

*=============================================================================
* File      : 08REPLOOP.INC
* Author    : Hasan Dudu
* Version   : 1.0
* Date      : 24/02/2011 19:41:19
* Changed   :
* Changed by:
* Remarks   :
$ontext

$offtext
*=============================================================================

*==========================================================
*Variable values===========================================
*==========================================================

CPIX(%1)        = CPI.L;
DPIX(%1)        = DPI.L;
DMPSX(%1)       = DMPS.L;
DTINSX(%1)      = DTINS.L;
EGX(RN,%1)         = EG.L(RN);
EHX(RR,H,%1)       = EH.L(RR,H);
EXRX(ROW,%1)        = EXR.L(ROW);
FSAVX(ROW,%1)   = FSAV.L(ROW);
TFSAVX(%1)      = TFSAV.L;
GADJX(%1)       = GADJ.L;
GOVSHRX(%1)     = GOVSHR.L;
GSAVX(%1)       = GSAV.L;
IADJX(%1)       = IADJ.L;
INVSHRX(%1)     = INVSHR.L;
MPSX(RR,INS,%1)    = MPS.L(RR,INS);
MPSADJX(%1)     = MPSADJ.L;
PAX(RR,A,%1)       = PA.L(RR,A);
PDDX(RR,C,%1)      = PDD.L(RR,C);
PDSX(RR,C,%1)      = PDS.L(RR,C);
PEX(RR,C,ROW,%1)       = PE.L(RR,C,ROW);
PINTAX(RR,A,%1)    = PINTA.L(RR,A);
PMX(ROW,RR,C,%1)       = PM.L(ROW,RR,C);
PQX(RR,C,%1)       = PQ.L(RR,C);
PVAX(RR,A,%1)      = PVA.L(RR,A);
PWEX(RR,C,ROW,%1)      = PWE.L(RR,C,ROW);
PWMX(ROW,RR,C,%1)      = PWM.L(ROW,RR,C);
PXX(RR,C,%1)       = PX.L(RR,C);
PXACX(RR,A,RR1,C,%1)   = PXAC.L(RR,A,RR1,C);
QAX(RR,A,%1)       = QA.L(RR,A);
QDX(RR,C,%1)       = QD.L(RR,C);
QEX(RR,C,ROW,%1)       = QE.L(RR,C,ROW);

*--HD:30/04/2008
QETX(RR,C,%1)       =SUM(ROW,QE.L(RR,C,ROW));

QFX(RR,F,RR1,A,%1)     = QF.L(RR,F,RR1,A);
QFUEX(RR,FLAB,%1)     = QFUE.L(RR,FLAB);
LABAVLX(RR,H,%1)      = LABAVL.L(RR,H);
QFSX(RR,F,%1)      = QFS.L(RR,F);
QGX(RR,C,RN,%1)       = QG.L(RR,C,RN);
QHX(RR,C,RR1,H,%1)     = QH.L(RR,C,RR1,H);
QHAX(RR,A,RR1,C,RR2,H,%1)  = QHA.L(RR,A,RR1,C,RR2,H);
QINTX(RR,C,RR1,A,%1)   = QINT.L(RR,C,RR1,A);
QINTAX(RR,A,%1)    = QINTA.L(RR,A);
QINVX(RR,C,%1)     = QINV.L(RR,C);
QMX(ROW,RR,C,%1)       = QM.L(ROW,RR,C);
QMTX(RR,C,%1)       =SUM(ROW, QM.L(ROW,RR,C));

QQX(RR,C,%1)       = QQ.L(RR,C);
QTX(RR,C,%1)       = QT.L(RR,C);
QVAX(RR,A,%1)      = QVA.L(RR,A);
QXX(RR,C,%1)       = QX.L(RR,C);
QXACX(RR,A,RR1,C,%1)   = QXAC.L(RR,A,RR1,C);
TABSX(%1)       = TABS.L;
TINSX(RR,INS,RN,%1)   = TINS.L(RR,INS,RN);
TINSADJX(%1)    = TINSADJ.L;
TRIIX(RR,INS,RR1,INS1,%1) = TRII.L(RR,INS,RR1,INS1);
WALRASX(%1)     = WALRAS.L;
WFX(F,%1)       = WF.L(F);
WFRX(RR,F,%1)       = WFR.L(RR,F);
WFDISTX(RR,F,RR1,A,%1) = WFDIST.L(RR,F,RR1,A);
YFX(RR,F,%1)       = YF.L(RR,F);
YGX(RN,%1)         = YG.L(RN);
YIFX(RR,INSD,RR1,F,%1) = YIF.L(RR,INSD,RR1,F);
YIX(RR,INSDNG,%1)  = YI.L(RR,INSDNG);

*-Computed activity-specific wage
WFAX(RR,F,RR1,A,%1)    = WFR.L(RR,F)*WFDIST.L(RR,F,RR1,A);
WPAYX(RR,%1) = WPAY.L(RR) ;


$ONTEXT
*-==========================================================
*-Budgets for SAM accounts==================================
*-==========================================================

 SAMBUD('ACT',R,A,'INC',RR1,C,%1)         = PXAC.L(R,A,RR1,C)*theta(R,A,RR1,C)*QA.L(R,A);
 SAMBUD('ACT',R,A,'SPND',RR1,F,%1)        = WF.L(F)*WFDIST.L(R,F,RR1,A)*QF.L(R,F,RR1,A);
 SAMBUD('ACT',R,A,'SPND',RR1,C,%1)        = PQ.L(R,C)*QINT.L(R,C,RR1,A);
 SAMBUD('ACT',R,A,'SPND',RR1,'VATAX',%1)  = SUM(RR2,tva(R,A,RR2))*PVA.L(R,A)*QVA.L(R,A);
 SAMBUD('ACT',R,A,'SPND',RR1,'ACTAX',%1)  = SUM(RR2,ta(R,A,RR2))*PA.L(R,A)*QA.L(R,A);
 SAMBUD('ACT',R,A,'SPND',RR1,'WCH',%1)    = SUM(F,QF.L(RR1,F,R,A)*twa(RR1,F,R,A));

*-========================

 SAMBUD('COM',R,C,'INC',RR1,H,%1)              = PQ.L(R,C)*QH.L(R,C,RR1,H)+ SUM((RR2,A), PXAC.L(RR2,A,R,C)*QHA.L(RR2,A,R,C,RR1,H));
 SAMBUD('COM',R,C,'INC',RRN1,'GOV',%1)          = PQ.L(R,C)*QG.L(R,C,RR1);

*- ASSUME THAT INVESTMENT IN REGION R IS DONE BY THE GOODS OF REGION R
 SAMBUD('COM',R,C,'INC',R,'S-I',%1)           = PQ.L(R,C)*QINV.L(R,C);

*- ASSUME THAT STOCK CHANGES IN REGION R IS DONE BY THE GOODS OF REGION R
 SAMBUD('COM',R,C,'INC',R,'DSTK',%1)          = PQ.L(R,C)*qdst(R,C);

*- ASSUME THAT TRADE IN REGION R IS DONE BY THE GOODS OF REGION R (NO RE-EXPORT AMONG REGIONS)
 SAMBUD('COM',R,C,'INC',R,ROW,%1)            = EXR.L*pwe.L(R,C,ROW)*QE.L(R,C,ROW);

 SAMBUD('COM',R,C,'INC',RR1,A,%1)              = PQ.L(R,C)*QINT.L(R,C,RR1,A);
 SAMBUD('COM',R,C,'INC',R,'DUM',%1)$CT(R,C)          = PQ.L(R,C)*QT.L(R,C);
 SAMBUD('COM',R,C,'SPND',RR1,A,%1)             = PXAC.L(R,A,RR1,C)*theta(R,A,RR1,C)*QA.L(R,A);
 SAMBUD('COM',R,C,'SPND',RR1,'COMTAX',%1)      = tq(R,C,RR1)*PQ.L(R,C)*QQ.L(R,C);
 SAMBUD('COM',R,C,'SPND',ROW,'IMPTAX',%1)     = sum(RR1,tm(RR1,ROW,R,C))*EXR.L*pwm.L(ROW,R,C)*QM.L(ROW,R,C);
 SAMBUD('COM',R,C,'SPND',R,ROW,%1)           = EXR.L*pwm.L(ROW,R,C)*QM.L(ROW,R,C);
 SAMBUD('COM',R,C,'SPND',ROW,'EXPTAX',%1)     = SUM(RR1,te(R,C,RR1,ROW))*EXR.L*pwe.L(R,C,ROW)*QE.L(R,C,ROW);
 SAMBUD('COM',R,C,'SPND',RR1,'TRNCSTDOM',%1)$CT(R,C)   =  SUM(C1,PQ.L(R,C)*icd(R,C,RR1,C1)*QD.L(R,C));

*--========================

 SAMBUD('FAC',R,F,'INC',RR1,A,%1)              = WF.L(F)*WFDIST.L(R,F,RR1,A)*QF.L(R,F,RR1,A);
 SAMBUD('FAC',R,F,'SPND',RR1,INSD,%1)          = YIF.L(R,INSD,RR1,F);
 SAMBUD('FAC',R,F,'SPND',RR1,ROW,%1)         = EXR.L*trnsfr(RR1,ROW,R,F);
 SAMBUD('FAC',R,F,'SPND',RR1,'FACTAX',%1)
   = tf(R,F,RR1)*(SUM((RR2,A), WF.L(F)*WFDIST.L(R,F,RR2,A)*QF.L(R,F,RR2,A))
                                             - SUM((RR4,ROW),EXR.L*trnsfr(RR4,ROW,R,F)));
*- SAMBUD('FAC','FWATU','SPND','GOV',%1)         = YF.L('FWATU');
*--========================

 SAMBUD('INSDNG',R,INSDNG,'INC',RR1,F,%1)          = YIF.L(R,INSDNG,RR1,F);
 SAMBUD('INSDNG',R,INSDNG,'INC',RR1,INSDNG1,%1)    = TRII.L(R,INSDNG,RR1,INSDNG1);
 SAMBUD('INSDNG',R,INSDNG,'INC',RRN1,'GOV',%1)      = trnsfr(R,INSDNG,RRN1,'GOV')*CPI.L;
 SAMBUD('INSDNG',R,INSDNG,'INC',RR1,ROW,%1)        = trnsfr(R,INSDNG,RR1,ROW)*EXR.L;
 SAMBUD('INSDNG',R,INSDNG,'INC',RR1,'WCH',%1)      = WPAY.L(R)*shiw(R,INSDNG);
 SAMBUD('INSDNG',R,H,'SPND',RR1,C,%1)              = PQ.L(R,C)*QH.L(R,C,RR1,H) + SUM((RR2,A), PXAC.L(RR2,A,R,C)*QHA.L(RR2,A,R,C,RR1,H));
 SAMBUD('INSDNG',R,INSDNG1,'SPND',RR1,INSDNG,%1)   = TRII.L(R,INSDNG,RR1,INSDNG1);
 SAMBUD('INSDNG',R,INSDNG,'SPND',RR1,ROW,%1)       = EXR.L*trnsfr(R,ROW,RR1,INSDNG);
 SAMBUD('INSDNG',R,INSDNG,'SPND',RR1,'S-I',%1)      = MPS.L(R,INSDNG)*(1 - TINS.L(R,INSDNG,RR1))*YI.L(R,INSDNG);
 SAMBUD('INSDNG',R,INSDNG,'SPND',RR1,'INSTAX',%1)  = TINS.L(R,INSDNG,RR1)*YI.L(R,INSDNG);

*--========================

 SAMBUD('GOV',R,'DUM','INC',RR1,'INSTAX',%1)  = SUM((INSDNG), TINS.L(R,INSDNG,RR1)*YI.L(R,INSDNG));
 SAMBUD('GOV',R,'DUM','INC',RR1,'FACTAX',%1)  = SUM((F), tf(R,F,RR1)*YF.L(R,F));
 SAMBUD('GOV',R,'DUM','INC',RR1,'VATAX',%1)   = SUM((A), tva(R,A,RR1)*PVA.L(R,A)*QVA.L(R,A));
 SAMBUD('GOV',R,'DUM','INC',RR1,'ACTAX',%1)  = SUM((A), ta(R,A,RR1)*PA.L(R,A)*QA.L(R,A));
 SAMBUD('GOV',R,'DUM','INC',ROW,'IMPTAX',%1)  = SUM((C), SUM(RR1,tm(RR1,ROW,R,C))*pwm.L(ROW,R,C)*QM.L(ROW,R,C))*EXR.L;
 SAMBUD('GOV',R,'DUM','INC',ROW,'EXPTAX',%1)  = SUM((C), SUM(RR1,te(R,C,RR1,ROW))*pwe.L(R,C,ROW)*QE.L(R,C,ROW))*EXR.L;
 SAMBUD('GOV',R,'DUM','INC',RR1,'COMTAX',%1)  = SUM((C), tq(R,C,RN)*PQ.L(R,C)*QQ.L(R,C));
 SAMBUD('GOV',R,'DUM','INC',RR1,F,%1)         = YIF.L(RN,'GOV',RR1,F);
 SAMBUD('GOV',R,'DUM','INC',RR1,ROW,%1)       = trnsfr(RN,'GOV',RR1,ROW)*EXR.L;
 SAMBUD('GOV',R,'DUM','SPND',RR1,C,%1)        = PQ.L(R,C)*QG.L(R,C,RR1);
 SAMBUD('GOV',R,'DUM','SPND',RR1,INSD,%1)     = trnsfr(RR1,INSD,RN,'GOV')*CPI.L;
 SAMBUD('GOV',R,'DUM','SPND',R,'S-I',%1)     = GSAV.L;

*--========================
 SAMBUD('SAVINV',R,'DUM','INC',RR1,INSDNG,%1)  = MPS.L(R,INSDNG)*(1 - TINS.L(R,INSDNG,RR1))*YI.L(R,INSDNG);
 SAMBUD('SAVINV',R,'DUM','INC',RN,'GOV',%1)    = GSAV.L;
 SAMBUD('SAVINV',R,'DUM','INC',RR1,ROW,%1)     = EXR.L*FSAV.L(ROW);
 SAMBUD('SAVINV',R,'DUM','SPND',RR1,C,%1)      = PQ.L(RR1,C)*QINV.L(RR1,C);
 SAMBUD('SAVINV',R,'DUM','SPND',R,'DSTK',%1) = SUM(C, PQ.L(R,C)*qdst(R,C));

*--========================

 SAMBUD('INSTAX',R,'DUM','INC',RR1,INSDNG,%1)   = TINS.L(RR1,INSDNG,R)*YI.L(RR1,INSDNG);
 SAMBUD('INSTAX',R,'DUM','SPND',RRN1,'GOV',%1)   = SUM(INSDNG, TINS.L(R,INSDNG)*YI.L(R,INSDNG));

 SAMBUD('FACTAX',R,'DUM','INC',RR1,F,%1)        = tf(R,F,RR1)*YF.L(R,F);
 SAMBUD('FACTAX',R,'DUM','SPND',RRN1,'GOV',%1)   = SUM(F, tf(R,F,RR1)*YF.L(R,F));

 SAMBUD('VATAX',R,'DUM','INC',RR1,A,%1)         = tva(R,A,RR1)*PVA.L(A)*QVA.L(A);
 SAMBUD('VATAX',R,'DUM','SPND','GOV',%1)    = SUM(A, tva(A)*PVA.L(A)*QVA.L(A));

 SAMBUD('ACTAX',R,'DUM','INC',RR1,A,%1)        = ta(A)*PA.L(A)*QA.L(A);
 SAMBUD('ACTAX',R,'DUM','SPND',RRN1,'GOV',%1)   = SUM(A, ta(A)*PA.L(A)*QA.L(A));

 SAMBUD('COMTAX',R,'DUM','INC',RR1,C,%1)        = tq(C)*QQ.L(C);
 SAMBUD('COMTAX',R,'DUM','SPND',RRN1,'GOV',%1)   = SUM(C, tq(C)*QQ.L(C));

*-- SAMBUD('IMPTAX','DUM','INC',C,%1)        = SUM(ROW,tm(ROW,C)*pwm.L(ROW,C)*QM.L(ROW,C)*EXR.L;
*-- SAMBUD('IMPTAX','DUM','SPND','GOV',%1)   = SUM((ROW,C), tm(ROW,C)*pwm.L(ROW,C)*QM.L(ROW,C))*EXR.L;

*-- SAMBUD('EXPTAX','DUM','INC',C,%1)        = SUM(ROW,te(C,ROW)*pwe.L(C,ROW)*QE.L(C,ROW))*EXR.L;
*-- SAMBUD('EXPTAX','DUM','SPND','GOV',%1)   = SUM((C,ROW), te(C,ROW)*pwe.L(C,ROW)*QE.L(C,ROW))*EXR.L;

 SAMBUD('IMPTAXEU',R,'DUM','INC',C,%1)       = tm('E_U',C)*pwm.L('E_U',C)*QM.L('E_U',C)*EXR.L;
 SAMBUD('IMPTAXNEU',R,'DUM','INC',C,%1)      = tm('NEU',C)*pwm.L('NEU',C)*QM.L('NEU',C)*EXR.L;

 SAMBUD('IMPTAXEU',R,'DUM','SPND','GOV',%1)  = SUM(C, tm('E_U',C)*pwm.L('E_U',C)*QM.L('E_U',C))*EXR.L;
 SAMBUD('IMPTAXNEU',R,'DUM','SPND','GOV',%1) = SUM(C, tm('NEU',C)*pwm.L('NEU',C)*QM.L('NEU',C))*EXR.L;

 SAMBUD('EXPTAXEU',R,'DUM','INC',C,%1)       = te(C,'E_U')*pwe.L(C,'E_U')*QE.L(C,'E_U')*EXR.L;
 SAMBUD('EXPTAXNEU',R,'DUM','INC',C,%1)      = te(C,'NEU')*pwe.L(C,'NEU')*QE.L(C,'NEU')*EXR.L;

 SAMBUD('EXPTAXEU',R,'DUM','SPND','GOV',%1)  = SUM(C, te(C,'E_U')*pwe.L(C,'E_U')*QE.L(C,'E_U'))*EXR.L;
 SAMBUD('EXPTAXNEU',R,'DUM','SPND','GOV',%1) = SUM(C, te(C,'NEU')*pwe.L(C,'NEU')*QE.L(C,'NEU'))*EXR.L;

*--========================

 SAMBUD('ROW',R,'DUM','INC',C,%1)      = SUM(ROW,EXR.L*pwm.L(ROW,C)*QM.L(ROW,C));
 SAMBUD('ROW',R,'DUM','INC',F,%1)      = SUM(ROW,EXR.L*trnsfr(ROW,F));
 SAMBUD('ROW',R,'DUM','INC',INS,%1)    = SUM(ROW,EXR.L*trnsfr(ROW,INS));

 SAMBUD('ROW',R,'DUM','SPND',C,%1)     = SUM(ROW,EXR.L*pwe.L(C,ROW)*QE.L(C,ROW));
 SAMBUD('ROW',R,'DUM','SPND',INS,%1)   = SUM(ROW,EXR.L*trnsfr(INS,ROW));
 SAMBUD('ROW',R,'DUM','SPND','S-I',%1) = EXR.L*TFSAV.L;

*--========================

 SAMBUD(SAC,R,AC,'INC',RR1,'TOTAL',%1)  = SUM((RR1,ACNT), SAMBUD(SAC,R,AC,'INC',RR1,ACNT,%1));
 SAMBUD(SAC,R,AC,'SPND',RR1,'TOTAL',%1) = SUM((RR1,ACNT), SAMBUD(SAC,R,AC,'SPND',RR1,ACNT,%1));
$OFFTEXT

*-==========================================================
*-Budgets for SAM accounts==================================
*-==========================================================

 NSAMBUD('ACT',A,'INC', C      ,%1) = SUM((RR,RR1),PXAC.L(RR,A,RR1,C)*theta(RR,A,RR1,C)*QA.L(RR,A));
 NSAMBUD('ACT',A,'SPND',F      ,%1) = SUM((RR,RR1),WFR.L(RR,F)*WFDIST.L(RR,F,RR1,A)*QF.L(RR,F,RR1,A));
 NSAMBUD('ACT',A,'SPND',C      ,%1) = SUM((RR,RR1),PQ.L(RR,C)*QINT.L(RR,C,RR1,A));
 NSAMBUD('ACT',A,'SPND','VATAX',%1) = SUM((RR),SUM(RN,tva(RR,A,RN))*PVA.L(RR,A)*QVA.L(RR,A));
 NSAMBUD('ACT',A,'SPND','ACTAX',%1) = SUM((RR,RN),ta(RR,A,RN)*PA.L(RR,A)*QA.L(RR,A));
 NSAMBUD('ACT',A,'SPND','WCH'  ,%1) = SUM((RR,RR1),SUM(F,QF.L(RR1,F,RR,A)*twa(RR1,F,RR,A)));

*-========================

 NSAMBUD('COM',C,'INC',H     ,%1) = SUM((RR,RR1),PQ.L(RR,C)*QH.L(RR,C,RR1,H)+ SUM((RR2,A), PXAC.L(RR2,A,RR,C)*QHA.L(RR2,A,RR,C,RR1,H)));
 NSAMBUD('COM',C,'INC','GOV' ,%1) = SUM((RR,RN),PQ.L(RR,C)*QG.L(RR,C,RN));
 NSAMBUD('COM',C,'INC','S-I' ,%1) = SUM((RR),PQ.L(RR,C)*QINV.L(RR,C));
 NSAMBUD('COM',C,'INC','DSTK',%1) = SUM((RR),PQ.L(RR,C)*qdst(RR,C));
 NSAMBUD('COM',C,'INC',ROW   ,%1) = SUM((RR),EXR.L(ROW)*pwe.L(RR,C,ROW)*QE.L(RR,C,ROW));
 NSAMBUD('COM',C,'INC' ,A          ,%1)         = SUM((RR,RR1),PQ.L(RR,C)*QINT.L(RR,C,RR1,A));
 NSAMBUD('COM',C,'INC' ,'DUM'      ,%1)         = SUM((RR,RR1)$CT(RR,C),PQ.L(RR,C)*QT.L(RR,C));

 NSAMBUD('COM',C,'SPND',A          ,%1)         = SUM((RR,RR1),PXAC.L(RR,A,RR1,C)*theta(RR,A,RR1,C)*QA.L(RR,A));
 NSAMBUD('COM',C,'SPND','COMTAX'   ,%1)         = SUM((RR),SUM(RN,tq(RR,C,RN))*PQ.L(RR,C)*QQ.L(RR,C));
 NSAMBUD('COM',C,'SPND','IMPTAX'   ,%1)         = SUM((RR,ROW),SUM(RN,tm(RN,ROW,RR,C))*EXR.L(ROW)*pwm.L(ROW,RR,C)*QM.L(ROW,RR,C));
 NSAMBUD('COM',C,'SPND',ROW        ,%1)         = SUM((RR),EXR.L(ROW)*pwm.L(ROW,RR,C)*QM.L(ROW,RR,C));
 NSAMBUD('COM',C,'SPND','EXPTAX'   ,%1)         = SUM((RR,ROW),SUM(RN,te(RR,C,RN,ROW))*EXR.L(ROW)*pwe.L(RR,C,ROW)*QE.L(RR,C,ROW));
 NSAMBUD('COM',C,'SPND','TRNCSTDOM',%1)$SUM(RR,CT(RR,C)) = SUM((RR,RR1),SUM(C1,PQ.L(RR,C)*icd(RR,C,RR1,C1)*QD.L(RR,C)));

*--========================

 NSAMBUD('FAC',F,'INC' ,A       ,%1) = SUM((RR,RR1),WFR.L(RR,F)*WFDIST.L(RR,F,RR1,A)*QF.L(RR,F,RR1,A));
 NSAMBUD('FAC',F,'SPND',INSD    ,%1) = SUM((RR,RR1),YIF.L(RR,INSD,RR1,F));
 NSAMBUD('FAC',F,'SPND',ROW     ,%1) = SUM((RR,RN),EXR.L(ROW)*trnsfr(RN,ROW,RR,F));
 NSAMBUD('FAC',F,'SPND','FACTAX',%1) = SUM(RR,SUM(RN,tf(RR,F,RN))*(SUM((RR2,A), WFR.L(RR,F)*WFDIST.L(RR,F,RR2,A)*QF.L(RR,F,RR2,A))
                                             - SUM((RR4,ROW),EXR.L(ROW)*trnsfr(RR4,ROW,RR,F))));
*- NSAMBUD('FAC','FWATU','SPND','GOV',%1)         = YF.L('FWATU');
*--========================

 NSAMBUD('INSDNG',INSDNG,'INC'  ,F       ,%1)    = SUM((RR,RR1),YIF.L(RR,INSDNG,RR1,F));
 NSAMBUD('INSDNG',INSDNG,'INC'  ,INSDNG1 ,%1)    = SUM((RR,RR1),TRII.L(RR,INSDNG,RR1,INSDNG1));
 NSAMBUD('INSDNG',INSDNG,'INC'  ,'GOV'   ,%1)    = SUM((RR,RN),trnsfr(RR,INSDNG,RN,'GOV')*CPI.L);
 NSAMBUD('INSDNG',INSDNG,'INC'  ,ROW     ,%1)    = SUM((RR,RN),trnsfr(RR,INSDNG,RN,ROW)*EXR.L(ROW));
 NSAMBUD('INSDNG',INSDNG,'INC'  ,'WCH'   ,%1)    = SUM((RR),WPAY.L(RR)*shiw(RR,INSDNG));
 NSAMBUD('INSDNG',H,'SPND'      ,C       ,%1)    = SUM((RR,RR1),PQ.L(RR,C)*QH.L(RR,C,RR1,H) + SUM((RR2,A), PXAC.L(RR2,A,RR,C)*QHA.L(RR2,A,RR,C,RR1,H)));
 NSAMBUD('INSDNG',INSDNG,'SPND',INSDNG1  ,%1)    = SUM((RR,RR1),TRII.L(RR1,INSDNG1,RR,INSDNG));
 NSAMBUD('INSDNG',INSDNG,'SPND' ,ROW     ,%1)    = SUM((RN,RR1),EXR.L(ROW)*trnsfr(RN,ROW,RR1,INSDNG));
 NSAMBUD('INSDNG',INSDNG,'SPND' ,'S-I'   ,%1)    = SUM((RR,RN),MPS.L(RR,INSDNG)*(1 - TINS.L(RR,INSDNG,RN))*YI.L(RR,INSDNG));
 NSAMBUD('INSDNG',INSDNG,'SPND' ,'INSTAX',%1)    = SUM((RR,RN),TINS.L(RR,INSDNG,RN)*YI.L(RR,INSDNG));

*--========================

 NSAMBUD('GOV','DUM','INC' ,'INSTAX',%1)  = SUM((RR,INSDNG), sum(RN,TINS.L(RR,INSDNG,RN))*YI.L(RR,INSDNG));
 NSAMBUD('GOV','DUM','INC' ,'FACTAX',%1)  = SUM((RR,F), SUM(RN,tf(RR,F,RN))*YF.L(RR,F));
 NSAMBUD('GOV','DUM','INC' ,'VATAX' ,%1)  = SUM((RR,A), SUM(RN,tva(RR,A,RN))*PVA.L(RR,A)*QVA.L(RR,A));
 NSAMBUD('GOV','DUM','INC' ,'ACTAX',%1)  = SUM((RR,A), SUM(RN,ta(RR,A,RN))*PA.L(RR,A)*QA.L(RR,A));
 NSAMBUD('GOV','DUM','INC' ,'IMPTAX',%1)  = SUM((RR,ROW,C), SUM(RN,tm(RN,ROW,RR,C))*pwm.L(ROW,RR,C)*QM.L(ROW,RR,C)*EXR.L(ROW));
 NSAMBUD('GOV','DUM','INC' ,'EXPTAX',%1)  = SUM((RR,C,ROW), SUM(RN,te(RR,C,RN,ROW))*pwe.L(RR,C,ROW)*QE.L(RR,C,ROW)*EXR.L(ROW));
 NSAMBUD('GOV','DUM','INC' ,'COMTAX',%1)  = SUM((RR,C), SUM(RN,tq(RR,C,RN))*PQ.L(RR,C)*QQ.L(RR,C));
*- NSAMBUD('GOV','DUM','INC' ,F       ,%1)  = SUM((RN,RR1),YIF.L(RN,'GOV',RR1,F));
 NSAMBUD('GOV','DUM','INC' ,ROW     ,%1)  = SUM((RN,RN1),trnsfr(RN,'GOV',RN1,ROW)*EXR.L(ROW));
 NSAMBUD('GOV','DUM','SPND',C       ,%1)  = SUM((RR,RN),PQ.L(RR,C)*QG.L(RR,C,RN));
 NSAMBUD('GOV','DUM','SPND',INSD    ,%1)  = SUM((RR,RN),trnsfr(RR,INSD,RN,'GOV')*CPI.L);
 NSAMBUD('GOV','DUM','SPND','ROW'    ,%1)  = SUM((RN,RN1),trnsfr(RN,'ROW',RN1,'GOV')*CPI.L);
 NSAMBUD('GOV','DUM','SPND','S-I'   ,%1)  = GSAV.L;


*--========================
 NSAMBUD('SAVINV','DUM','INC' ,INSDNG,%1)  = SUM((RR),MPS.L(RR,INSDNG)*(1 - SUM(RN,TINS.L(RR,INSDNG,RN)))*YI.L(RR,INSDNG));
 NSAMBUD('SAVINV','DUM','INC' ,'GOV' ,%1)  = GSAV.L;
 NSAMBUD('SAVINV','DUM','INC' ,ROW   ,%1)  = EXR.L(ROW)*FSAV.L(ROW);
 NSAMBUD('SAVINV','DUM','SPND',C     ,%1)  = SUM(RR1,PQ.L(RR1,C)*QINV.L(RR1,C));
 NSAMBUD('SAVINV','DUM','SPND','DSTK',%1)  = SUM((RR,C), PQ.L(RR,C)*qdst(RR,C));

*--========================

 NSAMBUD('INSTAX','DUM','INC' ,INSDNG,%1)   = SUM(RR,SUM(RN,TINS.L(RR,INSDNG,RN))*YI.L(RR,INSDNG));
 NSAMBUD('INSTAX','DUM','SPND','GOV' ,%1)   = SUM((RR,INSDNG), SUM(RN,TINS.L(RR,INSDNG,RN))*YI.L(RR,INSDNG));

 NSAMBUD('FACTAX','DUM','INC' ,F    ,%1)   = SUM(RR,SUM(RN,tf(RR,F,RN))*YF.L(RR,F));
 NSAMBUD('FACTAX','DUM','SPND','GOV',%1)   = SUM((RR,F), SUM(RN,tf(RR,F,RN))*YF.L(RR,F));

 NSAMBUD('VATAX','DUM','INC' ,A    ,%1)    = SUM(RR,SUM(RN,tva(RR,A,RN)*PVA.L(RR,A))*QVA.L(RR,A));
 NSAMBUD('VATAX','DUM','SPND','GOV',%1)    = SUM((RR,A), SUM(RN,tva(RR,A,RN))*PVA.L(RR,A)*QVA.L(RR,A));

 NSAMBUD('ACTAX','DUM','INC' ,A    ,%1)   = SUM(RR,SUM(RN,ta(RR,A,RN))*PA.L(RR,A)*QA.L(RR,A));
 NSAMBUD('ACTAX','DUM','SPND','GOV',%1)   = SUM((RR,A), SUM(RN,ta(RR,A,RN))*PA.L(RR,A)*QA.L(RR,A));

 NSAMBUD('COMTAX','DUM','INC' ,C    ,%1)   = SUM(RR,SUM(RN,tq(RR,C,RN))*QQ.L(RR,C));
 NSAMBUD('COMTAX','DUM','SPND','GOV',%1)   = SUM((RR,C), SUM(RN,tq(RR,C,RN))*QQ.L(RR,C));

 NSAMBUD('IMPTAX','DUM','INC' ,C    ,%1)   = SUM((RR,ROW),SUM(RN,tm(RN,ROW,RR,C))*pwm.L(ROW,RR,C)*QM.L(ROW,RR,C)*EXR.L(ROW));
 NSAMBUD('IMPTAX','DUM','SPND','GOV',%1)   = SUM((ROW,RR,C), SUM(RN,tm(RN,ROW,RR,C))*pwm.L(ROW,RR,C)*QM.L(ROW,RR,C)*EXR.L(ROW));

 NSAMBUD('EXPTAX','DUM','INC' ,C    ,%1)   = SUM((RR,ROW),SUM(RN,te(RR,C,RN,ROW))*pwe.L(RR,C,ROW)*QE.L(RR,C,ROW)*EXR.L(ROW));
 NSAMBUD('EXPTAX','DUM','SPND','GOV',%1)   = SUM((RR,C,ROW), SUM(RN,te(RR,C,RN,ROW))*pwe.L(RR,C,ROW)*QE.L(RR,C,ROW)*EXR.L(ROW));

*--========================

 NSAMBUD('ROW','DUM','INC',C  ,%1)      = SUM((RR,ROW),EXR.L(ROW)*pwm.L(ROW,RR,C)*QM.L(ROW,RR,C));
 NSAMBUD('ROW','DUM','INC',F  ,%1)      = SUM((RR,ROW,RN),EXR.L(ROW)*trnsfr(RN,ROW,RR,F));
 NSAMBUD('ROW','DUM','INC',INS,%1)     = SUM((RR,RN,ROW),EXR.L(ROW)*trnsfr(RN,ROW,RR,INS));

 NSAMBUD('ROW','DUM','SPND',C    ,%1)  = SUM((RR,ROW),EXR.L(ROW)*pwe.L(RR,C,ROW)*QE.L(RR,C,ROW));
 NSAMBUD('ROW','DUM','SPND',INS  ,%1)  = SUM((R,RN,ROW),EXR.L(ROW)*trnsfr(R,INS,RN,ROW));
 NSAMBUD('ROW','DUM','SPND','S-I',%1)  = SUM(ROW,EXR.L(ROW)*FSAV.L(ROW));

*--========================

 NSAMBUD(SAC,AC,'INC','TOTAL' ,%1) = SUM((ACNT), NSAMBUD(SAC,AC,'INC',ACNT,%1));
 NSAMBUD(SAC,AC,'SPND','TOTAL',%1) = SUM((ACNT), NSAMBUD(SAC,AC,'SPND',ACNT,%1));

*--==========================================================
*--National accounts data====================================
*--==========================================================

 NGDPTAB1('NOMINAL','PRVCON',%1)
   =  SUM((RR,C,RR1,H), PQ.L(RR,C)*QH.L(RR,C,RR1,H))
      + SUM((RR2,A,RR,C,RR1,H), PXAC.L(RR2,A,RR,C)*QHA.L(RR2,A,RR,C,RR1,H));

 NGDPTAB1('REAL','PRVCON',%1)
   =  SUM((RR,C,RR1,H), PQX(RR,C,'BASE')*QH.L(RR,C,RR1,H))
      + SUM((RR2,A,RR,C,RR1,H), PXACX(RR2,A,RR,C,'BASE')*QHA.L(RR2,A,RR,C,RR1,H));

 NGDPTAB1('NOMINAL','FIXINV',%1)  = SUM((RR,C), PQ.L(RR,C)*QINV.L(RR,C));
 NGDPTAB1('REAL','FIXINV',%1)     = SUM((RR,C), PQX(RR,C,'BASE')*QINV.L(RR,C));

 NGDPTAB1('NOMINAL','DSTOCK',%1)   = SUM((RR,C), PQ.L(RR,C)*QDST(RR,C));
 NGDPTAB1('REAL','DSTOCK',%1)      = SUM((RR,C), PQX(RR,C,'BASE')*QDST(RR,C));

 NGDPTAB1('NOMINAL','GOVCON',%1)  = SUM((RR,C,RN), PQ.L(RR,C)*QG.L(RR,C,RN));
 NGDPTAB1('REAL','GOVCON',%1)     = SUM((RR,C,RN), PQX(RR,C,'BASE')*QG.L(RR,C,RN));

 NGDPTAB1('NOMINAL','EXPORTS',%1) = SUM((RR,C,ROW)$CE(RR,C), PWE.L(RR,C,ROW)*EXR.L(ROW)*QE.L(RR,C,ROW));
 NGDPTAB1('REAL','EXPORTS',%1)    = SUM((RR,C,ROW)$CE(RR,C), PWEX(RR,C,ROW,'BASE')*EXR0(ROW)*QE.L(RR,C,ROW));

 NGDPTAB1('NOMINAL','IMPORTS',%1) = -SUM((RR,ROW,C)$CM(RR,C), PWM.L(ROW,RR,C)*EXR.L(ROW)*QM.L(ROW,RR,C));
 NGDPTAB1('REAL','IMPORTS',%1)    = -SUM((RR,ROW,C)$CM(RR,C), PWMX(ROW,RR,C,'BASE')*EXR0(ROW)*QM.L(ROW,RR,C));

 NGDPTAB1('NOMINAL','GDPMP',%1) = SUM(IGDP2, NGDPTAB1('NOMINAL',IGDP2,%1));
 NGDPTAB1('REAL','GDPMP',%1)    = SUM(IGDP2, NGDPTAB1('REAL',IGDP2,%1));

 NGDPTAB1('NOMINAL','ABSORP',%1)
  = NGDPTAB1('NOMINAL','GDPMP',%1) - NGDPTAB1('NOMINAL','IMPORTS',%1)
    - NGDPTAB1('NOMINAL','EXPORTS',%1);

 NGDPTAB1('REAL','ABSORP',%1)
  = NGDPTAB1('REAL','GDPMP',%1) - NGDPTAB1('REAL','IMPORTS',%1)
    - NGDPTAB1('REAL','EXPORTS',%1);

 NGDPTAB1('NOMINAL','GDPFC2',%1) = SUM((RR2,A), PVA.L(RR2,A)*(1-SUM(RN,tva(RR2,A,RN)))*QVA.L(RR2,A));
 NGDPTAB1('REAL','GDPFC2',%1)    = SUM((RR2,A), PVAX(RR2,A,'BASE')*(1-SUM(RN,tva0(RR2,A,RN)))*QVA.L(RR2,A));

 NGDPTAB1('NOMINAL','NETITAX',%1) =
            SUM((RR2,A), SUM(RN,ta(RR2,A,RN))*PA.L(RR2,A)*QA.L(RR2,A))
          + SUM((RR2,A), SUM(RN,tva(RR2,A,RN))*PVA.L(RR2,A)*QVA.L(RR2,A))
          + SUM((RR,ROW,C)$CM(RR,C), SUM(RN,tm(RN,ROW,RR,C))*PWM.L(ROW,RR,C)*EXR.L(ROW)*QM.L(ROW,RR,C))
          + SUM((RR,C,ROW)$CE(RR,C), SUM(RN,te(RR,C,RN,ROW))*PWE.L(RR,C,ROW)*EXR.L(ROW)*QE.L(RR,C,ROW))
          + SUM((RR,C), SUM(RN,tq(RR,C,RN))*PQ.L(RR,C)*QQ.L(RR,C));

 NGDPTAB1('REAL','NETITAX',%1) =
            SUM((RR2,A), SUM(RN,ta0(RR2,A,RN))*PA0(RR2,A)*QA.L(RR2,A))
          + SUM((RR2,A), SUM(RN,tva0(RR2,A,RN))*PVA0(RR2,A)*QVA.L(RR2,A))
          + SUM((ROW,RR,C)$CM(RR,C), SUM(RN,tm0(RN,ROW,RR,C))*PWM0(ROW,RR,C)*EXR0(ROW)*QM.L(ROW,RR,C))
          + SUM((RR,C,ROW)$CE(RR,C), SUM(RN,te0(RR,C,RN,ROW))*PWE0(RR,C,ROW)*EXR0(ROW)*QE.L(RR,C,ROW))
          + SUM((RR,C), SUM(RN,tq0(RR,C,RN))*PQ0(RR,C)*QQ.L(RR,C));


 NGDPTAB1('NOMINAL','GDPMP2',%1)
  = NGDPTAB1('NOMINAL','GDPFC2',%1) + NGDPTAB1('NOMINAL','NETITAX',%1);

 NGDPTAB1('NOMSHARE',IGDP,%1)$NGDPTAB1('NOMINAL','GDPMP',%1)
  = 100*NGDPTAB1('NOMINAL',IGDP,%1)/NGDPTAB1('NOMINAL','GDPMP',%1);

 NGDPTAB1('REALSHARE',IGDP,%1)$NGDPTAB1('REAL','GDPMP',%1)
  = 100*NGDPTAB1('REAL',IGDP,%1)/NGDPTAB1('REAL','GDPMP',%1);

 NGDPTAB1('DEFLATOR',IGDP,%1)$NGDPTAB1('REAL',IGDP,%1)
  = 100*NGDPTAB1('NOMINAL',IGDP,%1)/NGDPTAB1('REAL',IGDP,%1);

*--==================

 NGDPTAB2('NOMINAL',A,%1)       = SUM(RR2,PVA.L(RR2,A)*(1-SUM(RN,tva(RR2,A,RN)))*QVA.L(RR2,A));
 NGDPTAB2('REAL',A,%1)          = SUM(RR2,PVAX(RR2,A,'BASE')*(1-SUM(RN,tva0(RR2,A,RN)))*QVA.L(RR2,A));

 NGDPTAB2('NOMINAL','TOTAL',%1) = SUM(A, NGDPTAB2('NOMINAL',A,%1));
 NGDPTAB2('REAL','TOTAL',%1)    = SUM(A, NGDPTAB2('REAL',A,%1));

 NGDPTAB2('NOMSHARE',A,%1)
  = 100*NGDPTAB2('NOMINAL',A,%1)/NGDPTAB2('NOMINAL','TOTAL',%1);

 NGDPTAB2('REALSHARE',A,%1)
  = 100*NGDPTAB2('REAL',A,%1)/NGDPTAB2('REAL','TOTAL',%1);

 NGDPTAB2('NOMSHARE','TOTAL',%1)  = SUM(A, NGDPTAB2('NOMSHARE',A,%1));
 NGDPTAB2('REALSHARE','TOTAL',%1) = SUM(A, NGDPTAB2('REALSHARE',A,%1));

 NGDPTAB2('DEFLATOR',A,%1)
  = 100*NGDPTAB2('NOMINAL',A,%1)/NGDPTAB2('REAL',A,%1);

 NGDPTAB2('DEFLATOR','TOTAL',%1)
  = 100*NGDPTAB2('NOMINAL','TOTAL',%1)/NGDPTAB2('REAL','TOTAL',%1);


*--==========================================================
*--Closure information=======================================
*--==========================================================

*--Macro closures
 MACCLOS('S-I',%1) = SICLOS(%1);
 MACCLOS('GOV',%1) = GOVCLOS(%1);
 MACCLOS(ROW,%1) = ROWCLOS(%1);

*--Factor closures
 FACCLOS(RR,F,FCLS,%1) =FCLOS(RR,F,FCLS,%1);


*--Gaps and solver reports=========================

 NSAMBUDGAP(SAC,ACNT,%1)
  = ABS(NSAMBUD(SAC,ACNT,'INC','TOTAL',%1) - NSAMBUD(SAC,ACNT,'SPND','TOTAL',%1));

 NSAMBUDGAP(SAC,'TOTAL',%1) = SUM(ACNT, NSAMBUDGAP(SAC,ACNT,%1));

 NGDPGAP(%1)
  =  ABS(NGDPTAB1('NOMINAL','GDPMP',%1) - NGDPTAB1('NOMINAL','GDPMP2',%1));


 SOLVEREP('MODEL-STATUS','MCP',%1)  = STANDCGE.MODELSTAT;
 SOLVEREP('SOLVER-STATUS','MCP',%1) = STANDCGE.SOLVESTAT;
 SOLVEREP('NUM-REDEFEQ','MCP',%1)   = STANDCGE.NUMREDEF;

 SOLVEREP('MODEL-STATUS','NLP',%1)  = NLPCGE.MODELSTAT;
 SOLVEREP('SOLVER-STATUS','NLP',%1) = NLPCGE.SOLVESTAT;


*--Values are reset to zero to avoid using results from earlier solve in
*--the definition of SOLVEREP FOR later solves.

 STANDCGE.MODELSTAT = 0;
 STANDCGE.SOLVESTAT = 0;
 STANDCGE.NUMREDEF  = 0;

 NLPCGE.MODELSTAT   = 0;
 NLPCGE.SOLVESTAT   = 0;
 NLPCGE.NUMREDEF    = 0;


ERRMPSEQ1(RR,INSDNG,%1)
 $(
  ( (DMPS.UP- DMPS.LO GT 1.0E-6) OR (MPSADJ.UP - MPSADJ.LO GT 1.0E-6) )
 AND
  (ABS(mpsbar(RR,INSDNG) - 1) LT 1.0E-6)
 AND
  (MPS01(RR,INSDNG) EQ 1)
                                                               ) = 1;

*--#*#*#*#*# THE END OF REPLOOP.INC #*#*#*#*
