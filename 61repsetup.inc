$STITLE Input file: REPSETUP.INC. Sets and parameters for reports. Standard CGE modeling system, Version 1.01


*\SETS AND PARAMETERS FOR REPORTS++++++++


SET

 SAC  SAM account categories
 /
 ACT     actitivities
 COM     commodities
 FAC     factors
 INSDNG  domestic non-government institutions
 GOV     government
 SAVINV  savings-investments
 INSTAX  direct taxes on institutions
 FACTAX  direct taxes on factors
 VATAX   value added tax
 ACTAX  tax on activity gross revenue
 COMTAX  tax on commodity sales in the domestic market
 IMPTAX  import tariff
 EXPTAX  export tax
 ROW     TOTAL rest of world (FCU)
 /

 IND3  third index for SAM budgets
 /
 INC
 SPND
 GAP
 /

 KGDP  first index in GDP tables
 /
 NOMINAL, REAL, NOMSHARE, REALSHARE, DEFLATOR
 /

 IGDP  Items for GDP and national accounts
  /
  ABSORP   absorption
  PRVCON   private consumption
  FIXINV   fixed investment
  DSTOCK   stock change
  GOVCON   government consumption
  EXPORTS  exports
  IMPORTS  imports
  GDPMP    GDP at market prices (alt. 1: spending)
  GDPMP2   GDP at market prices (alt. 2: value-added)
  NETITAX  net indirect taxes
  GDPFC2   GDP at factor cost
  /

 IGDP2(IGDP)  Items for GDPMP summation
  /
  PRVCON   private consumption
  FIXINV   fixed investment
  DSTOCK   stock change
  GOVCON   government consumption
  EXPORTS  exports
  IMPORTS  imports
  /


*-The sets MODTYPE and SOLVEIND are used in the parameters SOLVEMAX,
*-SOLVEREP, and SOLVEWARN.

 MODTYPE  model types
  /
 MCP mixed-complementarity programming
 NLP non-linear programming
 /

 SOLVEIND  solve indicators
 /
 MODEL-STATUS    model status
 SOLVER-STATUS   solver status
 NUM-REDEFEQ     number of redefined equations
 /
 ;



*-==========================================================
*-Parameter defining legal values for SOLVEREP==============
*-==========================================================


TABLE
 SOLVEMAX(SOLVEIND,MODTYPE) max permitted value for SOLVEREP indicators

                MCP   NLP
MODEL-STATUS     1     2
SOLVER-STATUS    1     1
NUM-REDEFEQ      0     0

$ontext
Note: See the end of SIM.GMS (or the GAMS Users' Guide) for explanation
of what different values for MODEL-STATUS or SOLVER-STATUS represent.
NUM-REDEFEQ shows the number of redefined equations.
$OFFTEXT



*-==========================================================
*-Report parameters in LOOP=================================
*-==========================================================
PARAMETERS
*-Values for all model variables============================
*-Values for all model variables (variable name + X)========
 CPIX(SIM)                                        consumer price index (PQ-based)
 DPIX(SIM)                                        index for domestic producer prices (PDS-based)
 DMPSX(SIM)                                       change in marginal propensity to save for
 DTINSX(SIM)                                      change in domestic institution tax share
 EGX(RN,SIM)                                 total current government expenditure
 EHX(RR,H,SIM)                               household consumption expenditure
 EXRX(ROW,SIM)                                        exchange rate
 FSAVX(ROW,SIM)                               foreign savings
 TFSAVX(SIM)                                       foreign savings
 GADJX(SIM)                               government demand scaling factor
 GOVSHRX(SIM)                                     govt consumption share of absorption
 GSAVX(SIM)                                       government savings
 IADJX(SIM)                                investment scaling factor (for fixed capital formation)
 INVSHRX(SIM)                                     investment share of absorption
 MPSX(RR,INS,SIM)                            marginal propensity to save for dom non-gov inst ins
 MPSADJX(SIM)                             savings rate scaling factor
 PAX(RR,A,SIM)                               output price of activity a
 PDDX(RR,C,SIM)                              demand price for com'y c produced & sold domestically
 PDSX(RR,C,SIM)                              supply price for com'y c produced & sold domestically
 PEX(RR,C,ROW,SIM)                       price of exports
 PINTAX(RR,A,SIM)                            price of intermediate aggregate
 PMX(ROW,RR,C,SIM)                       price of imports
 PQX(RR,C,SIM)                               price of composite good c
 PVAX(RR,A,SIM)                              value added price
 PWEX(RR,C,ROW,SIM)                      world price of exports
 PWMX(ROW,RR,C,SIM)                      world price of imports
 PXX(RR,C,SIM)                               average output price
 PXACX(RR,A,RR1,C,SIM)                           price of commodity c from activity a
 QAX(RR,A,SIM)                               level of domestic activity
 QDX(RR,C,SIM)                               qnty of domestic sales
 QEX(RR,C,ROW,SIM)                       qnty of exports
 QETX(RR,C,SIM)                               qnty of exports
 QFX(RR,F,RR1,A,SIM)                             qnty demanded of factor f from activity a
 QFUEX(RR,FLAB,SIM)                             qnty unemployment from household H
 LABAVLX(RR,H,SIM)                           qnty labor force participation
 QFSX(RR,F,SIM)                              qnty of factor supply
 QGX(RR,C,RN,SIM)                               qnty of government consumption
 QHX(RR,C,RR1,H,SIM)                             qnty consumed of market commodity c by household h
 QHAX(RR,A,RR1,C,RR2,H,SIM)                qnty consumed of home commodity c fr act a by hhd h
 QINTX(RR,C,RR1,A,SIM)                           qnty of intermediate demand for c from activity a
 QINTAX(RR,A,SIM)                            qnty of aggregate intermediate input
 QINVX(RR,C,SIM)                             qnty of fixed investment demand
 QMX(ROW,RR,C,SIM)                       qnty of imports
 QMTX(RR,C,SIM)                               qnty of imports
 QQX(RR,C,SIM)                               qnty of composite goods supply
 QTX(RR,C,SIM)                               qnty of trade and transport demand for commodity c
 QVAX(RR,A,SIM)                              qnty of aggregate value added
 QXX(RR,C,SIM)                               qnty of aggregate marketed commodity output
 QXACX(RR,A,RR1,C,SIM)                           qnty of ouput of commodity c from activity a
 TABSX(SIM)                                       total absorption
 TINSX(RR,INS,RN,SIM)                           rate of direct tax on domestic institutions ins paid to gov' of region RR1
 TINSADJX(SIM)                            direct tax scaling factor
 TRIIX(RR,INS,RR1,INS1,SIM)                transfers to dom. inst. insdng from insdngp
 WALRASX(SIM)                                     savings-investment imbalance (should be zero)
 WFX(F,SIM)                                       economy-wide wage (rent) for factor f
 WFRX(RR,F,SIM)                                       region-wide wage (rent) for factor f
 WFDISTX(RR,F,RR1,A,SIM)                         factor wage distortion variable
 YFX(RR,F,SIM)                               factor income
 YGX(RN,SIM)                                 total current government income
 YIFX(RR,INS,RR1,F,SIM)                          income of institution ins from factor f
 YIX(RR,INS,SIM)                             income of (domestic non-governmental) institution ins
 WFAX(RR,F,RR1,A,SIM)                            price of factor f for activity a
 WPAYX(RR,SIM)                                   water charge


*-Budgets for SAM accounts==================================
 NSAMBUD(SAC,AC,IND3,*,SIM)  national budgets for  SAM accounts
*-Note: For RoW, for exch earnings of model country defined as income;
*-for exch spending of model country defined as spending
 RSAMBUD(SAC,RALL,AC,IND3,RALL1,AC,SIM)



*-National accounts data=====================================
 NGDPTAB1(KGDP,IGDP,SIM) GDP and national accounts
 NGDPTAB2(KGDP,AC,SIM)   GDP at factor cost by activity

*-Macro and factor closures=================================
 MACCLOS( * ,SIM)              Macro closures actually used (by sim)
 FACCLOS(RR,F,*,SIM)            Factor closures actually used (by sim and f)

*-==========================================================
*-Post-calculated report parameters=========================
*-==========================================================
*-% change in LOOP parameters: above parameter name + P
*-For base, these parameters show the base value.(%ch)
 CPIXP(SIM)                                                consumer price index (PQ-based) (%ch)
 DPIXP(SIM)                                                index for dom producer prices (PDS-based) (%ch)
 EGXP(RN,SIM)                                         total current gov expenditure (%ch)
 EHXP(RR,H,SIM)                                       hhd consumption expenditure (%ch)
 EXRXP(ROW,SIM)                                                exchange rate (%ch)
 FSAVXP(ROW,SIM)                                       foreign savings (%ch)
 TFSAVXP(SIM)                                       foreign savings (%ch)
 GADJXP(SIM)                                       gov demand scaling factor (%ch)
 GOVSHRXP(SIM)                                             govt consumption share of absorption (%ch)
 GSAVXP(SIM)                                         gov savings (%ch)
 IADJXP(SIM)                                       fixed investment scaling factor (%ch)
 INVSHRXP(SIM)                                             investment share of absorption (%ch)
 MPSXP(RR,INS,SIM)                                    marg prop to save for dom non-gov inst ins (%ch)
 PAXP(RR,A,SIM)                                       output price of activity a (%ch)
 PDDXP(RR,C,SIM)                                      demand price for com'y c produced & sold dom'ly (%ch)
 PDSXP(RR,C,SIM)                                      supply price for com'y c produced & sold dom'ly (%ch)
 PEXP(RR,C,ROW,SIM)                              price of exports (%ch)
 PINTAXP(RR,A,SIM)                                    price of intermediate aggregate (%ch)
 PMXP(ROW,RR,C,SIM)                              price of imports (%ch)
 PQXP(RR,C,SIM)                                       price of composite good c (%ch)
 PVAXP(RR,A,SIM)                                      value added price (%ch)
 PWEXP(RR,C,ROW,SIM)                             world price of exports (%ch)
 PWMXP(ROW,RR,C,SIM)                             world price of imports (%ch)
 PXXP(RR,C,SIM)                                       average output price (%ch)
 PXACXP(RR,A,RR1,C,SIM)                           price of comm c from activity a (%ch)
 QAXP(RR,A,SIM)                                       level of dom activity (%ch)
 QDXP(RR,C,SIM)                                       qnty of dom sales (%ch)
 QEXP(RR,C,ROW,SIM)                              qnty of exports (%ch)
 QETXP(RR,C,SIM)                                       qnty of exports (%ch)
 QFXP(RR,F,RR1,A,SIM)                             qnty demanded of factor f from activity a (%ch)
 QFUEXP(RR,FLAB,SIM)				  qnty unemployment from household H (%ch)
 LABAVLXP(RR,H,SIM)                               qnty labor force participation (%ch)
 QFSXP(RR,F,SIM)                                      qnty of factor supply (%ch)
 QGXP(RR,C,RN,SIM)                               qnty of gov consumption (%ch)
 QHXP(RR,C,RR1,H,SIM)                             qnty consumed of market comm c by hhd h (%ch)
 QHAXP(RR,A,RR1,C,R2,H,SIM)                          Qnty consumed of home comm c fr act a by hhd h (%ch)
 QINTXP(RR,C,RR1,A,SIM)                           qnty of intermediate demand for c from act.y a (%ch)
 QINTAXP(RR,A,SIM)                                    qnty of aggregate intermediate input (%ch)
 QINVXP(RR,C,SIM)                                     qnty of fixed investment demand (%ch)
 QMXP(ROW,RR,C,SIM)                              qnty of imports (%ch)
 QMTXP(RR,C,SIM)                                       qnty of imports (%ch)
 QQXP(RR,C,SIM)                                       qnty of composite goods supply (%ch)
 QTXP(RR,C,SIM)                                       qnty of trade and transport demand for comm c (%ch)
 QVAXP(RR,A,SIM)                                      qnty of aggregate value added (%ch)
 QXXP(RR,C,SIM)                                       qnty of aggregate marketed comm output (%ch)
 QXACXP(RR,A,RR1,C,SIM)                           qnty of ouput of comm c from activity a (%ch)
 TABSXP(SIM)                                               total absorption (%ch)
 TINSXP(RR,INS,RN,SIM)                           rate of direct tax on dom institutions ins (%ch)
*-TINSADJXP(SIM)                                 zero for base
 TRIIXP(RR,INS,RR1,INS1,SIM)                         transfers to dom. inst. insdng from insdngp (%ch)
*-WALRASXP(SIM)                                  zero for base
 WFXP(F,SIM)                                               economy-wide wage (rent) for factor f (%ch)
 WFRXP(RR,F,SIM)                                       region-wide wage (rent) for factor f (%ch)
 WFDISTXP(RR,F,RR1,A,SIM)                         factor wage distortion variable (%ch)
 YFXP(RR,F,SIM)                                       factor income (%ch)
 YGXP(RN,SIM)                                         total current gov income (%ch)
 YIFXP(RR,INS,RR1,F,SIM)                          income of institution ins from factor f (%ch)
 YIXP(RR,INS,SIM)                                     income of (dom non-gov) institution ins (%ch)
 WFAXP(RR,F,RR1,A,SIM)                                  price of factor f for activity a (%ch)
 WPAYXP(RR,SIM)                                           water charge

*-Other %-age change reports
 NSAMBUDP(SAC,AC,IND3,AC,SIM)                 budgets for SAM accounts (%ch for non-base)
 NGDPTAB1P(KGDP,IGDP,SIM)                         GDP and national accounts (%ch for non-base)
 NGDPTAB2P(KGDP,AC,SIM)                           GDP at f.c. by activity (%ch for non-base)

*-Consistency and solution checks========================================
 NSAMBUDGAP(SAC,AC,SIM)                         abs value of inc-spnd gaps for SAM acc budgets
 NGDPGAP(SIM)                            gaps in GDP reports
 SOLVEREP(SOLVEIND,MODTYPE,SIM)         model+solver status & no of redef eq.ons by model type

 NEGPWARNDOM(RR,C,SIM)                                  comm c has one or more neg.e prices for sim.on sim
 NEGPWARNROW(RR,C,ROW,SIM)                          comm c has one or more neg.e prices for sim.on sim
 NEGQWARNDOM(RR,C,SIM)                                  comm c has one or more neg.e qnties for sim.on sim
 NEGQWARNROW(RR,C,ROW,SIM)                          comm c has one or more neg.e qnties for sim.on sim
 NEGWFWARN(RR,F,SIM)                                 factor f has one or more neg.e wages for sim.on sim
 NEGWFRWARN(RR,F,SIM)                                 factor f has one or more neg.e wages for sim.on sim
 NEGQFWARN(RR,F,SIM)                                 factor f has one or more neg.e qnties for sim.on sim

 NGAPWARN(SIM)                                             error if abs gap above cutoff
 WALRASWARN(SIM)                                          error if abs value of WALRAS above cutoff
 SOLVEWARN(SOLVEIND,MODTYPE,SIM)           error if values of SOLVEREP are illegal
 ;



*-#*#*#*#*# THE END OF REPSETUP.INC #*#*#*#*
