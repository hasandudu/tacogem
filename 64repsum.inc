*=============================================================================
* File      : 010REPSUM.INC
* Author    : Hasan Dudu
* Version   : 1.0
* Date      : 24/02/2011 19:42:29
* Changed   : 24/02/2011 19:42:36
* Changed by: Hasan Dudu
* Remarks   :
$ontext

$offtext
*=============================================================================


PARAMETERS
*-This group of parameters are used for various price-index calculations.
 pdwt(RR,C)  weight of commodity c in PD index
 pwewt(RR,C) weight of commodity c in pwe index
 pwmwt(RR,C) weight of commodity c in pwm index
 pwwt(*)  weight of aggregate exports-imports in pw (tradables) index

*-These two parameters are used in the computation of EVTAB

 IC(RR1,H,SIM)        indirect compensation fn by hhd h and simulation sim
*-IC measure the consumption value needed at base prices to generate same welfare
*-as under prices and income under sim

 EV(RR,AC,SIM)        equivalent variation by hhd ac and simulation sim
*-EV measures the income change that, at base prices, would be
*-equivalent to the change simulated for sim.

$ONTEXT
Source for IC and EV formulas (here adapted to fit demand system
with home and market consumption):
Blonigen, Bruce RR2,A., Joseph E. Flynn, and Kenneth RR2,A. Reinert
(1997) Sector-Focused General Equilibrium Modeling, pp. 189-230
in eds. Joseph F. Francois and Kenneth RR2,A. Reinert, Applied Methods
for Trade Policy Analysis. Cambridge, MA: Cambridge University Press
The formulas are on pp. 224-225.
$OFFTEXT
 ;

 pdwt(RR,C)  = PDDX(RR,C,'BASE')*QDX(RR,C,'BASE')/SUM((RR1,C1), PDDX(RR1,C1,'BASE')*QDX(RR1,C1,'BASE'));
 pwewt(RR,C) = SUM(ROW,pweX(RR,C,ROW,'BASE')*QEX(RR,C,ROW,'BASE'))/SUM((RR1,C1,ROW), pweX(RR1,C1,ROW,'BASE')*QEX(RR1,C1,ROW,'BASE'));
 pwmwt(RR,C) = SUM(ROW,pwmX(ROW,RR,C,'BASE')*QMX(ROW,RR,C,'BASE'))/SUM((ROW,RR1,C1), pwmX(ROW,RR1,C1,'BASE')*QMX(ROW,RR1,C1,'BASE'));
 pwwt('EXP')
  = SUM((RR1,C1,ROW), pweX(RR1,C1,ROW,'BASE')*QEX(RR1,C1,ROW,'BASE'))
    / (SUM((RR1,C1,ROW), pweX(RR1,C1,ROW,'BASE')*QEX(RR1,C1,ROW,'BASE')) + SUM((ROW,RR1,C1), pwmX(ROW,RR1,C1,'BASE')*QMX(ROW,RR1,C1,'BASE')) );
 pwwt('IMP')
  = SUM((ROW,RR1,C1), pwmX(ROW,RR1,C1,'BASE')*QMX(ROW,RR1,C1,'BASE'))
    / (SUM((RR1,C1,ROW), pweX(RR1,C1,ROW,'BASE')*QEX(RR1,C1,ROW,'BASE')) + SUM((ROW,RR1,C1), pwmX(ROW,RR1,C1,'BASE')*QMX(ROW,RR1,C1,'BASE')) );

 IC(RR1,H,SIMCUR)
  = (
     ( EHX(RR1,H,SIMCUR)
     - SUM((RR,C), PQX(RR,C,SIMCUR)*gammam(RR,C,RR1,H))
     - SUM((RR2,A,RR,C), PXACX(RR2,A,RR,C,SIMCUR)*gammah(RR2,A,RR,C,RR1,H)) )
      /
     ( PROD((RR,C), PQX(RR,C,SIMCUR)**betam(RR,C,RR1,H))
                        *PROD((RR2,A,RR,C), PXACX(RR2,A,RR,C,SIMCUR)**betah(RR2,A,RR,C,RR1,H)) )
      )
   *( PROD((RR,C), PQX(RR,C,'BASE')**betam(RR,C,RR1,H))
                        *PROD((RR2,A,RR,C), PXACX(RR2,A,RR,C,'BASE')**betah(RR2,A,RR,C,RR1,H)) )
     + SUM((RR,C), PQX(RR,C,'BASE')*gammam(RR,C,RR1,H)) + SUM((RR2,A,RR,C), PXACX(RR2,A,RR,C,'BASE')*gammah(RR2,A,RR,C,RR1,H));


 EV(RR1,H,SIMCUR)       = IC(RR1,H,SIMCUR) - EHX(RR1,H,'BASE');
 EV(RR1,'TOTAL',SIMCUR) = SUM((H), EV(RR1,H,SIMCUR));


$ontext
Source for formulas (here adapted to fit demand system
with home and market consumption):

Blonigen, Bruce RR2,A., Joseph E. Flynn, and Kenneth RR2,A. Reinert
(1997) Sector-Focused General Equilibrium Modeling, pp. 189-230
in eds. Joseph F. Francois and Kenneth RR2,A. Reinert, Applied Methods
for Trade Policy Analysis. Cambridge, MA: Cambridge University Press
The formulas are on pp. 224-225.

Sadoulet, E., and RR2,A. de Janvry (1995). Quantitative Development Policy
Analysis. The Johns Hopkins University Press: Baltimore and London.
Section 1.5.1.3, pp. 13-14

Note:

CV is measured at simulated prices and incomes. It shows:
the maximum payment the consumer would be willing to make to avoid
having the simulated change undone (i.e. the payment after which the
consumer would have been just as well off as without the change).
For positive (negative) welfare change, CV > 0 (< 0).

EV is measured at base prices and incomes. It shows:
the minimum payment that the consumer would require for foregoing RR2,A
change (i.e., if the consumer receives this payment, then he/she is as
well off under base prices and income as he/she would have been if the
change had taken place.)
For positive (negative) welfare change, CV > 0 (< 0).

$OFFTEXT


PARAMETERS
 BETAAUX(RR1,H)      auxiliary term based on marginal spending shares
 PRICEIND(RR1,H,SIM) price index
 EXPSUB(RR1,H,SIM)   subsistence spending for hhd H of region R1 under sim on sim
 INDUTIL(RR1,H,SIM)  indirect utility for hhd H of region R1 under simulation sim
 EXPV0(RR1,H,SIM)    base-utility exp.e fn for hhd H of region R1 under simulation sim
 CV(RR1,AC,SIM)      compensating variation for hhd H of region R1 under sim.on sim
 CV2(RR1,AC,SIM)     concise formula for CV
 ;
*-"ALPHA"
 BETAAUX(RR1,H)
   = PROD((RR,C), betam(RR,C,RR1,H)**betam(RR,C,RR1,H))*PROD((RR2,A,RR,C), betah(RR2,A,RR,C,RR1,H)**betah(RR2,A,RR,C,RR1,H));

 PRICEIND(RR1,H,SIMCUR)
   = PROD((RR,C), PQX(RR,C,SIMCUR)**betam(RR,C,RR1,H))
        *PROD((RR2,A,RR,C), PXACX(RR2,A,RR,C,SIMCUR)**betah(RR2,A,RR,C,RR1,H));

 EXPSUB(RR1,H,SIMCUR)
 = SUM((RR,C), PQX(RR,C,SIMCUR)*gammam(RR,C,RR1,H))
   + SUM((RR2,A,RR,C), PXACX(RR2,A,RR,C,SIMCUR)*gammah(RR2,A,RR,C,RR1,H));

*-"V"
 INDUTIL(RR1,H,SIMCUR)
 = BETAAUX(RR1,H) *( EHX(RR1,H,SIMCUR) - EXPSUB(RR1,H,SIMCUR) )
    / PRICEIND(RR1,H,SIMCUR);

*-"e(p1,u0)"
 EXPV0(RR1,H,SIMCUR)
 = (INDUTIL(RR1,H,'BASE')/BETAAUX(RR1,H)) * PRICEIND(RR1,H,SIMCUR)
   + EXPSUB(RR1,H,SIMCUR);

 CV(RR1,H,SIMCUR)
  = EHX(RR1,H,SIMCUR) - EXPV0(RR1,H,SIMCUR);

 CV(RR1,'TOTAL',SIMCUR) = SUM(H, CV(RR1,H,SIMCUR));

*-Alternative definition of CV requiring less precalculations.
 CV2(RR1,H,SIMCUR)
  =   (EHX(RR1,H,SIMCUR) - EXPSUB(RR1,H,SIMCUR))
    - (EHX(RR1,H,'BASE') - EXPSUB(RR1,H,'BASE'))*PRICEIND(RR1,H,SIMCUR)/PRICEIND(RR1,H,'BASE');

 CV2(RR1,'TOTAL',SIMCUR) = SUM(H, CV2(RR1,H,SIMCUR));

$ONTEXT
Deriving CV2:

CV2 = EH - EXPV0
    = EH - ((V0/BETA) * PIND + EXPSUB);
    = EH - EXPSUB - (V0/BETA) * PIND ;
    = EH - EXPSUB - (1/BETA)*PIND*V0 ;

    = (EH - EXPSUB)
       - (1/BETA)*PIND *BETA*( EH0 - EXPSUB0 ) / PIND0;

    = (EH - EXPSUB)
       - PIND *( EH0 - EXPSUB0 ) / PIND0;

    = (EH - EXPSUB)
       - (EH0 - EXPSUB0) * PIND/PIND0;

*-In words: CV =
[current supernumerary spending]
MINUS
[base supernumerary spending adjusted for changes in price index].


*-Superfluous parameters for CV2:
 BETAAUX
 INDUTIL
 EXPV0

$OFFTEXT

SET
 SIND Macro summary indicators
 /
*-Real quantities, exchange rates and price indices
 QABSTOT real absorption (LCU at base prices)
 QHTOT   real household consumption (LCU at base prices)
 QINVTOT real investment (LCU at base prices)
 QGTOT   real government consumption (LCU at base prices)
 QETOT   total real exports (LCU at base prices)
 QMTOT   total real imports (LCU at base prices)
 REXR    PPP real exchange rate (LCUs per FCU)
 NEXR    nominal exchange rate (LCUs per FCU)
 PWEIND  exports price index (FCU -- 100 for base)
 PWMIND  imports price index (FCU -- 100 for base)
 PWIND   world (tradables) price index (FCU -- 100 for base)
 PDIND   domestic (non-tradables) price index (100 for base)
 CPI     consumer price index (100 for base)
 TOFT    terms of trade (ratio pwe index & pwm index) (100 for base)

*-Nominal GDP shares
 INVGDP     investment (% of nominal GDP)
 PRVSAVGDP  private (household + enterprise) savings (% of nominal GDP)
 FORSAVGDP  foreign savings (% of nominal GDP)
 TRDDEFGDP  trade deficit (% of nominal GDP)
 GOVSAVGDP  government savings (% of nominal GDP)
 IMPTAXGDP  tariff revenue (% of nominal GDP)
 DIRTAXGDP  direct tax revenue (% of nominal GDP)
 /

SINDDEV(SIND) summary indicators showing deviation from BASE
 /
 INVGDP     investment (% of nominal GDP)
 PRVSAVGDP  private (household + enterprise) savings (% of nominal GDP)
 FORSAVGDP  foreign savings (% of nominal GDP)
 TRDDEFGDP  trade deficit  (% of nominal GDP)
 GOVSAVGDP  government savings (% of nominal GDP)
 IMPTAXGDP  import tax revenue (% of nominal GDP)
 DIRTAXGDP  direct tax revenue (% of nominal GDP)
 /

 SIMNB(SIM) non-base simulations

 SINDP(SIND)  summary indicators showing percent change
 ;

SIMNB(SIM)    = YES;
SIMNB('BASE') = NO;
SINDP(SIND)$(NOT SINDDEV(SIND)) = YES;

DISPLAY SIMNB, SINDP;

PARAMETER
 MACROTAB(SIND,SIM)     Macro table
 QHTAB(RR,AC,SIM)       Disaggregated real household consumption in region R
 EVTAB(RR,AC,SIM)       Disaggregated EV in region R
 YFTAB(AC,SIM)          Disaggregated factor income distribution in region R
 QATAB(RR,AC,SIM)       Disaggregated activity production levels in region R
 PATAB(RR,AC,SIM)       Disaggregated activity prices in region R
 ;

*-MACROTAB=====================================

 MACROTAB('QINVTOT',SIMCUR) = SUM((RR,C), PQX(RR,C,'BASE')*QINVX(RR,C,SIMCUR));
 MACROTAB('QGTOT',SIMCUR)   = SUM((RR,C,RN), PQX(RR,C,'BASE')*QGX(RR,C,RN,SIMCUR));
 MACROTAB('QHTOT',SIMCUR)   = SUM((RR,C,RR1,H), PQX(RR,C,'BASE')*QHX(RR,C,RR1,H,SIMCUR))
                              + SUM((RR2,A,RR,C,RR1,H), PXACX(RR2,A,RR,C,'BASE')*QHAX(RR2,A,RR,C,RR1,H,SIMCUR)) ;
 MACROTAB('QABSTOT',SIMCUR)
  = MACROTAB('QINVTOT',SIMCUR) + MACROTAB('QGTOT',SIMCUR)
  + MACROTAB('QHTOT',SIMCUR);

 MACROTAB('QETOT',SIMCUR)  = SUM((RR,C,ROW), PWEX(RR,C,ROW,'BASE')*EXRX(ROW,'BASE')*QEX(RR,C,ROW,SIMCUR));
 MACROTAB('QMTOT',SIMCUR)  = SUM((ROW,RR,C), PWMX(ROW,RR,C,'BASE')*EXRX(ROW,'BASE')*QMX(ROW,RR,C,SIMCUR));
 MACROTAB('CPI',SIMCUR)    = 100*cpix(SIMCUR)/cpix('BASE');
*-hd: NOMİNAL EXHANGE RATE FOR EACH ROW SHOULD BE DEFINED SEPERATELY
 MACROTAB('NEXR',SIMCUR)   = SUM(ROW,100*EXRX(ROW,SIMCUR));
 MACROTAB('PDIND',SIMCUR)  = 100*SUM((RR,C), pdwt(RR,C)*PDDX(RR,C,SIMCUR));

 MACROTAB('PWEIND',SIMCUR)
  = 100*SUM((RR,C,ROW), pwex(RR,C,ROW,SIMCUR)*pwewt(RR,C))
    /SUM((C,RR,ROW), pwex(RR,C,ROW,'BASE')*pwewt(RR,C));
 MACROTAB('PWMIND',SIMCUR)
  = 100*SUM((ROW,RR,C), pwmx(ROW,RR,C,SIMCUR)*pwmwt(RR,C))
    /SUM((ROW,RR,C), pwmx(ROW,RR,C,'BASE')*pwmwt(RR,C));;

 MACROTAB('TOFT',SIMCUR)
  = 100*MACROTAB('PWEIND',SIMCUR)/MACROTAB('PWMIND',SIMCUR);

 MACROTAB('PWIND',SIMCUR)
  =  pwwt('EXP')*MACROTAB('PWEIND',SIMCUR)
  +  pwwt('IMP')*MACROTAB('PWMIND',SIMCUR);


 MACROTAB('REXR',SIM)$SIMCUR(SIM)
 = MACROTAB('NEXR',SIM)*MACROTAB('PWIND',SIM)/MACROTAB('PDIND',SIM)
  ;

 MACROTAB('INVGDP',SIMCUR)
  = 100*SUM((RR,C), PQX(RR,C,SIMCUR)*QINVX(RR,C,SIMCUR))
    /NGDPTAB1('NOMINAL','GDPMP',SIMCUR);

 MACROTAB('PRVSAVGDP',SIMCUR)
  = 100*SUM(INSDNG, NSAMBUD('INSDNG',INSDNG,'SPND','S-I',SIMCUR))
    /NGDPTAB1('NOMINAL','GDPMP',SIMCUR);

 MACROTAB('FORSAVGDP',SIMCUR)
   = 100*SUM(ROW,EXRX(ROW,SIMCUR)*TFSAVX(SIMCUR))
     /NGDPTAB1('NOMINAL','GDPMP',SIMCUR);

 MACROTAB('TRDDEFGDP',SIMCUR)
   = 100*(SUM((ROW,RR,C), PMX(ROW,RR,C,SIMCUR)*QMX(ROW,RR,C,SIMCUR))
     - SUM((RR,C,ROW), PEX(RR,C,ROW,SIMCUR)*QEX(RR,C,ROW,SIMCUR)))
     /NGDPTAB1('NOMINAL','GDPMP',SIMCUR);

 MACROTAB('GOVSAVGDP',SIMCUR)
   = 100*NSAMBUD('GOV','DUM','SPND','S-I',SIMCUR)
     /NGDPTAB1('NOMINAL','GDPMP',SIMCUR);

 MACROTAB('IMPTAXGDP',SIMCUR)
   = 100*NSAMBUD('GOV','DUM','INC','IMPTAX',SIMCUR)
     /NGDPTAB1('NOMINAL','GDPMP',SIMCUR);

 MACROTAB('DIRTAXGDP',SIMCUR)
   = 100*(NSAMBUD('GOV','DUM','INC','INSTAX',SIMCUR)
     + NSAMBUD('GOV','DUM','INC','FACTAX',SIMCUR))
     /NGDPTAB1('NOMINAL','GDPMP',SIMCUR);

*-TABLES OTHER THAN MACROTAB===============================

 QHTAB(RR1,H,SIMCUR)       = SUM((RR,C), PQX(RR,C,'BASE')*QHX(RR,C,RR1,H,SIMCUR))
                         + SUM((RR2,A,RR,C), PXACX(RR2,A,RR,C,'BASE')*QHAX(RR2,A,RR,C,RR1,H,SIMCUR));
 QHTAB(RR1,'TOTAL',SIMCUR) = SUM((RR,C,H), PQX(RR,C,'BASE')*QHX(RR,C,RR1,H,SIMCUR))
                         + SUM((RR2,A,RR,C,RR3,H), PXACX(RR2,A,RR,C,'BASE')*QHAX(RR2,A,RR,C,RR3,H,SIMCUR));

 EVTAB(RR1,H,'BASE')       = EHX(RR1,H,'BASE');
 EVTAB(RR1,'TOTAL','BASE') = SUM((H), EHX(RR1,H,'BASE'));
 EVTAB(RR1,H,SIMNB)        = 100*EV(RR1,H,SIMNB)/EHX(RR1,H,'BASE');
 EVTAB(RR1,'TOTAL',SIMNB)     = 100*SUM((RR3,H), EV(RR3,H,SIMNB))/SUM((RR3,H), EHX(RR3,H,'BASE'));

 YFTAB(F,SIMCUR)
   = 100*NSAMBUD('FAC',F,'INC','TOTAL',SIMCUR)
     /SUM(F1, NSAMBUD('FAC',F1,'INC','TOTAL',SIMCUR));
 YFTAB('TOTAL',SIMCUR) = SUM(F, YFTAB(F,SIMCUR));

 QATAB(RR2,A,SIMCUR)       = PAX(RR2,A,'BASE')*QAX(RR2,A,SIMCUR);
 QATAB(RR2,'TOTAL',SIMCUR) = SUM((RR3,A), PAX(RR3,A,'BASE')*QAX(RR3,A,SIMCUR));

 PATAB(RR2,A,SIMCUR)       = 100*PAX(RR2,A,SIMCUR);

*-==============================================
*-Computing %-age change from BASE for QHTAB and
*-for selected indicators of MACROTAB (in SINDP)

MACROTAB(SINDP,SIMNB)$SIMCUR(SIMNB)
  = 100*(MACROTAB(SINDP,SIMNB)/MACROTAB(SINDP,'BASE') - 1);

QHTAB(RR,AC,SIMNB)$(SIMCUR(SIMNB)$QHTAB(RR,AC,'BASE'))
  = 100*(QHTAB(RR,AC,SIMNB)/QHTAB(RR,AC,'BASE') - 1);

QATAB(RR,AC,SIMNB)$(SIMCUR(SIMNB)$QATAB(RR,AC,'BASE'))
  = 100*(QATAB(RR,AC,SIMNB)/QATAB(RR,AC,'BASE') - 1);

PATAB(RR,AC,SIMNB)$(SIMCUR(SIMNB)$PATAB(RR,AC,'BASE'))
  = 100*(PATAB(RR,AC,SIMNB)/PATAB(RR,AC,'BASE') - 1);

*-==============================================
*-Computing deviation from BASE for YFTAB and
*-for selected indicators of MACROTAB (in SINDDEV)

MACROTAB(SINDDEV,SIMNB)$SIMCUR(SIMNB)
  = MACROTAB(SINDDEV,SIMNB) - MACROTAB(SINDDEV,'BASE');

YFTAB(AC,SIMNB)$SIMCUR(SIMNB)
  = YFTAB(AC,SIMNB) - YFTAB(AC,'BASE');

*-Displaying only one decimal point
 OPTION DECIMALS=1;

DISPLAY
 MACROTAB
"In the BASE column, aggregate real indicators are measured"
"at base-year values."
"Exchange rates and price indices are set at 100."
"Items with GDP in name are expressed as % of nominal GDP."
""
"Units for non-BASE columns:"
"% changes from BASE for items not computed as GDP shares;"
"deviation from BASE for items computed as GDP shares."
""

QHTAB
"In the BASE column, quantities are measured at base-year prices."
"The non-BASE columns show % change from BASE."

EVTAB
"In the BASE column, consumption value at base-year prices."
"The non-BASE columns show EV as % of BASE consumption value."

YFTAB
"The BASE column show %-age shares of base factor incomes."
"The non-BASE columns show deviations from BASE."

QATAB
"In the BASE column, quantities are measured at base-year prices."
"The non-BASE columns show % change from BASE."

PATAB
"Base-prices are indexed to 100."
"The non-BASE columns show % change from BASE."
;

*-#*#*#*#*# THE END OF REPSUM.INC #*#*#*#*