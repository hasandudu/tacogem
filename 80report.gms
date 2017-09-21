$TITLE Report File
$STITLE Turkish Agricultural CGE Model - Regional.
$STITLE Based on IFPRI Standard CGE modeling system, Version 1.01
$ontext
*=============================================================================
* File      : 80report.gms
* Author    : Hasan Dudu (hasan@dudu.gen.tr)
* Version   : 1.0
* Date      : 15/08/2013 17:13:57
* Changed   : 13/12/2013 18:08:24
* Changed by: Hasan Dudu (hasan@dudu.gen.tr)
* Remarks   :
This file prepares the tables and figures used in the paper
* Licence   : MIT Licence
Copyright (c) <2013> <Hasan Dudu>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.


$OFFTEXT
*=============================================================================

PARAMETER tablesectorsum(A,*,*,SIM) Sector Summary Tables
tablesectorsumP(A,*,*,SIM) Sector Summary Tables % change
;
*-Sector Summary Tables

tablesectorsum(A,'Dom. Mark.','Production',SIMCUR)                      =SUM(RR,QVAX(RR,A,SIMCUR));
tablesectorsum(A,'Dom. Mark.','Consumption',SIMCUR)     =SUM((RR,C,RR1,H)$MAPAC(A,C),QHX(RR,C,RR1,H,SIMCUR));
tablesectorsum(A,'Dom. Mark.','Prices',SIMCUR)$SUM(RR,QAX(RR,A,SIMCUR)) =SUM(RR,PAX(RR,A,SIMCUR)*QAX(RR,A,SIMCUR))/SUM(RR,QAX(RR,A,SIMCUR));
tablesectorsum(A,'Employment',F,SIMCUR)                                 =SUM((RR,RR1),QFX(RR,F,RR1,A,SIMCUR));
tablesectorsum(A,'Wages',F,SIMCUR)$SUM(RR,QFX(RR,F,RR,A,SIMCUR))        =SUM(RR,WFAX(RR,F,RR,A,SIMCUR)*QFX(RR,F,RR,A,SIMCUR))/SUM(RR,QFX(RR,F,RR,A,SIMCUR));
tablesectorsum(A,'Trade','Exports',SIMCUR)                              =SUM((RR,ROW),SUM(C$MAPAC(A,C),PEX(RR,C,ROW,SIMCUR)*QEX(RR,C,ROW,SIMCUR)));
tablesectorsum(A,'Trade','Imports',SIMCUR)                              =SUM((ROW,RR),SUM(C$MAPAC(A,C),PMX(ROW,RR,C,SIMCUR)*QMX(ROW,RR,C,SIMCUR)));
tablesectorsum(A,'Trade','Trade Deficit',SIMCUR)                        =tablesectorsum(A,'Trade','Exports',SIMCUR)-tablesectorsum(A,'Trade','Imports',SIMCUR);


tablesectorsumP(A,'Dom. Mark.','Production',SIMCUR)$tablesectorsum(A,'Dom. Mark.','Production','BASE')  =100*(tablesectorsum(A,'Dom. Mark.','Production',SIMCUR) /tablesectorsum(A,'Dom. Mark.','Production','BASE')-1) ;
tablesectorsumP(A,'Dom. Mark.','Consumption',SIMCUR)$tablesectorsum(A,'Dom. Mark.','Consumption','BASE')        =100*(tablesectorsum(A,'Dom. Mark.','Consumption',SIMCUR)/tablesectorsum(A,'Dom. Mark.','Consumption','BASE')-1);
tablesectorsumP(A,'Dom. Mark.','Prices',SIMCUR)  $tablesectorsum(A,'Dom. Mark.','Prices','BASE')        =100*(tablesectorsum(A,'Dom. Mark.','Prices',SIMCUR) /tablesectorsum(A,'Dom. Mark.','Prices','BASE')-1)         ;
tablesectorsumP(A,'Employment',F,SIMCUR) $tablesectorsum(A,'Employment',F,'BASE')       =100*(tablesectorsum(A,'Employment',F,SIMCUR) /tablesectorsum(A,'Employment',F,'BASE')-1)                       ;
tablesectorsumP(A,'Wages',F,SIMCUR) $tablesectorsum(A,'Wages',F,'BASE')         =100*(tablesectorsum(A,'Wages',F,SIMCUR) /tablesectorsum(A,'Wages',F,'BASE')-1)                 ;
tablesectorsumP(A,'Trade','Exports',SIMCUR) $tablesectorsum(A,'Trade','Exports','BASE')         =100*(tablesectorsum(A,'Trade','Exports',SIMCUR) /tablesectorsum(A,'Trade','Exports','BASE')-1)         ;
tablesectorsumP(A,'Trade','Imports',SIMCUR) $tablesectorsum(A,'Trade','Imports','BASE')         =100*(tablesectorsum(A,'Trade','Imports',SIMCUR) /tablesectorsum(A,'Trade','Imports','BASE')-1)         ;
tablesectorsumP(A,'Trade','Trade Deficit',SIMCUR) $tablesectorsum(A,'Trade','Trade Deficit','BASE')     =100*(tablesectorsum(A,'Trade','Trade Deficit',SIMCUR) /tablesectorsum(A,'Trade','Trade Deficit','BASE')-1)     ;

tablesectorsumP(A,'Dom. Mark.','Production','BASE')     = tablesectorsum(A,'Dom. Mark.','Production','BASE');
tablesectorsumP(A,'Dom. Mark.','Consumption','BASE')    =tablesectorsum(A,'Dom. Mark.','Consumption','BASE') ;
tablesectorsumP(A,'Dom. Mark.','Prices','BASE')         = tablesectorsum(A,'Dom. Mark.','Prices','BASE') ;
tablesectorsumP(A,'Employment',F,'BASE')        = tablesectorsum(A,'Employment',F,'BASE') ;
tablesectorsumP(A,'Wages',F,'BASE')     = tablesectorsum(A,'Wages',F,'BASE');
tablesectorsumP(A,'Trade','Exports','BASE')     = tablesectorsum(A,'Trade','Exports','BASE') ;
tablesectorsumP(A,'Trade','Imports','BASE')     = tablesectorsum(A,'Trade','Imports','BASE') ;
tablesectorsumP(A,'Trade','Trade Deficit','BASE')       = tablesectorsum(A,'Trade','Trade Deficit','BASE') ;

*-TABLE 1: SECTORAL VA and GDP
SET nomrel(KGDP)
/Nominal, Real/
;

parameter
table1(NOMREL,*,SIM) Gross Value added in Sectors
table1P(NOMREL,*,SIM) Gross Value added in Sectors % change
;

table1(NOMREL,'Total',SIMCUR)           =NGDPTAB2(NOMREL,'Total',SIMCUR);
table1(NOMREL,'Agriculture',SIMCUR)     =NGDPTAB2(NOMREL,'AAGRI',SIMCUR);
table1(NOMREL,'Non-Agriculture',SIMCUR) =SUM(A$(NOT AAGR(A)),NGDPTAB2(NOMREL,A,SIMCUR));
table1(NOMREL,'Manufacturing',SIMCUR)   =NGDPTAB2(NOMREL,'AMANU',SIMCUR);
table1(NOMREL,'Food Production',SIMCUR) =NGDPTAB2(NOMREL,'AFOOD',SIMCUR);
table1(NOMREL,'Textiles',SIMCUR)        =NGDPTAB2(NOMREL,'ATEXT',SIMCUR);
table1(NOMREL,'Energy',SIMCUR)          =NGDPTAB2(NOMREL,'AENER',SIMCUR);
table1(NOMREL,'Services',SIMCUR)        =NGDPTAB2(NOMREL,'ASERV',SIMCUR)+NGDPTAB2('Nominal','APUBL',SIMCUR);

table1P(NOMREL,'Total',SIMCUR)          =(table1(NOMREL,'Total',SIMCUR)          /table1(NOMREL,'Total','BASE')          -1)*100;
table1P(NOMREL,'Agriculture',SIMCUR)    =(table1(NOMREL,'Agriculture',SIMCUR)    /table1(NOMREL,'Agriculture','BASE')    -1)*100;
table1P(NOMREL,'Non-Agriculture',SIMCUR)=(table1(NOMREL,'Non-Agriculture',SIMCUR)/ table1(NOMREL,'Non-Agriculture','BASE')-1)*100;
table1P(NOMREL,'Manufacturing',SIMCUR)  =(table1(NOMREL,'Manufacturing',SIMCUR)  /table1(NOMREL,'Manufacturing','BASE')  -1)*100;
table1P(NOMREL,'Food Production',SIMCUR)=(table1(NOMREL,'Food Production',SIMCUR)/ table1(NOMREL,'Food Production','BASE')-1)*100;
table1P(NOMREL,'Textiles',SIMCUR)       =(table1(NOMREL,'Textiles',SIMCUR)       /table1(NOMREL,'Textiles','BASE')       -1)*100;
table1P(NOMREL,'Energy',SIMCUR)         =(table1(NOMREL,'Energy',SIMCUR)         /table1(NOMREL,'Energy','BASE')         -1)*100;
table1P(NOMREL,'Services',SIMCUR)       =(table1(NOMREL,'Services',SIMCUR)       /table1(NOMREL,'Services','BASE')       -1)*100;

table1P(NOMREL,'Total','BASE')          =table1(NOMREL,'Total','BASE')           ;
table1P(NOMREL,'Agriculture','BASE')    =table1(NOMREL,'Agriculture','BASE')     ;
table1P(NOMREL,'Non-Agriculture','BASE')= table1(NOMREL,'Non-Agriculture','BASE') ;
table1P(NOMREL,'Manufacturing','BASE')  =table1(NOMREL,'Manufacturing','BASE')   ;
table1P(NOMREL,'Food Production','BASE')= table1(NOMREL,'Food Production','BASE') ;
table1P(NOMREL,'Textiles','BASE')       =table1(NOMREL,'Textiles','BASE')        ;
table1P(NOMREL,'Energy','BASE')         =table1(NOMREL,'Energy','BASE')  ;
table1P(NOMREL,'Services','BASE')       =table1(NOMREL,'Services','BASE')        ;

parameter table2(*,*,SIM) Macro indicators % change
;

table2('Real','Absorbtion',SIMCUR)                      =MACROTAB('QABSTOT',SIMCUR);
table2('Real','Houshold Consumption',SIMCUR)            =MACROTAB('QHTOT',SIMCUR);

table2('Real','Export' ,SIMCUR)                         =MACROTAB('QETOT',SIMCUR);
table2('Real','Import',SIMCUR)                          =MACROTAB('QMTOT',SIMCUR);
table2(' ','PPP Real Exchange Rate',SIMCUR)             =MACROTAB('REXR',SIMCUR);
table2(' ','Domestic Price Index',SIMCUR)               =MACROTAB('PDIND',SIMCUR);
table2('Ratio to GDP','Investment',SIMCUR)              =MACROTAB('INVGDP',SIMCUR);
table2('Ratio to GDP','Private Saving',SIMCUR)          =MACROTAB('PRVSAVGDP',SIMCUR);
table2('Ratio to GDP','Foreign Saving',SIMCUR)          =MACROTAB('FORSAVGDP',SIMCUR);
table2('Ratio to GDP','Trade Deficit',SIMCUR)           =MACROTAB('TRDDEFGDP',SIMCUR);
table2('Ratio to GDP','Government Saving',SIMCUR)       =MACROTAB('GOVSAVGDP',SIMCUR);

Parameter Table3(R,H,SIM) Household income
Table3P(R,H,SIM) Household income % change
;

Table3(RR,H,SIMCUR)=YIX(RR,H,SIMCUR);
Table3('TOT',H,SIMCUR)=SUM(RR,Table3(RR,H,SIMCUR));


Table3P(RR,H,SIMCUR)=(Table3(RR,H,SIMCUR)/Table3(RR,H,'BASE')-1)*100;
Table3P('TOT',H,SIMCUR)=(Table3('TOT',H,SIMCUR)/Table3('TOT',H,'BASE')-1)*100;

Table3P(R,H,'BASE')=Table3(R,H,'BASE');

Parameter table4(R,FLAB,SIM) Unemployment Rates
table4P(R,FLAB,SIM) change in unemployment rates
;


table4(RR,FLAB,SIMCUR)= QFUEX(RR,FLAB,SIMCUR)/QFSX(RR,FLAB,SIMCUR)*100;
table4('TOT',FLAB,SIMCUR)= SUM(RR,QFUEX(RR,FLAB,SIMCUR))/SUM(RR,QFSX(RR,FLAB,SIMCUR))*100;

table4P(R,FLAB,SIMCUR)  = table4(R,FLAB,SIMCUR)-table4(R,FLAB,"BASE");
table4P(R,FLAB,"BASE")  = table4(R,FLAB,"BASE") ;

SET AT5(A) /
*-AAGRI
AMANU
AFOOD
ATEXT
AENER
ASERV
APUBL
/;
SET AAGG /AGR
FOOD
OTHER
ALL/
;

SET MAPAGG(A,AAGG) /
AAGRI.AGR
AMANU.OTHER
AFOOD.FOOD
ATEXT.OTHER
AENER.OTHER
ASERV.OTHER
APUBL.OTHER
/;
SET MAPCAGG(C,AAGG) /
CAGRI.AGR
CMANU.OTHER
CFOOD.FOOD
CTEXT.OTHER
CENER.OTHER
CSERV.OTHER
CPUBL.OTHER
/;

Parameter table5(AAGG,*,*,SIM) last table in presentation
table5P(AAGG,*,*,SIM);

table5(AAGG,'Dom. Mark.','Production',SIMCUR)                                          =SUM(A$MAPAGG(A,AAGG),tablesectorsum(A,'Dom. Mark.','Production',SIMCUR) );
table5(AAGG,'Dom. Mark.','Consumption',SIMCUR)                                         =SUM((RR,C,RR1,H)$MAPCAGG(C,AAGG),QHX(RR,C,RR1,H,SIMCUR));
table5(AAGG,'Dom. Mark.','Prices',SIMCUR)$SUM((A,RR)$MAPAGG(A,AAGG),QAX(RR,A,SIMCUR))  =SUM((A,RR)$MAPAGG(A,AAGG),PAX(RR,A,SIMCUR)*QAX(RR,A,SIMCUR))/SUM((A,RR)$MAPAGG(A,AAGG),QAX(RR,A,SIMCUR));
table5(AAGG,'Employment',F,SIMCUR)                                                     =SUM(A$MAPAGG(A,AAGG),tablesectorsum(A,'Employment',F,SIMCUR) ) ;
table5(AAGG,'Wages',F,SIMCUR)$SUM((A,RR)$MAPAGG(A,AAGG),QFX(RR,F,RR,A,SIMCUR))         =SUM((A,RR)$MAPAGG(A,AAGG),WFAX(RR,F,RR,A,SIMCUR)*QFX(RR,F,RR,A,SIMCUR))/SUM((A,RR)$MAPAGG(A,AAGG),QFX(RR,F,RR,A,SIMCUR));
table5(AAGG,'Trade','Exports',SIMCUR)                                                  =SUM(A$MAPAGG(A,AAGG),tablesectorsum(A,'Trade','Exports',SIMCUR) );
table5(AAGG,'Trade','Imports',SIMCUR)                                                  =SUM(A$MAPAGG(A,AAGG),tablesectorsum(A,'Trade','Imports',SIMCUR) );
table5(AAGG,'Trade','Trade Deficit',SIMCUR)                                            =SUM(A$MAPAGG(A,AAGG),tablesectorsum(A,'Trade','Trade Deficit',SIMCUR));

table5('ALL','Dom. Mark.','Production',SIMCUR)                                         =SUM(A,tablesectorsum(A,'Dom. Mark.','Production',SIMCUR) );
table5('ALL','Dom. Mark.','Consumption',SIMCUR)                                        =SUM((RR,C,RR1,H),QHX(RR,C,RR1,H,SIMCUR));
table5('ALL','Dom. Mark.','Prices',SIMCUR)$SUM((A,RR),QAX(RR,A,SIMCUR))                =SUM((A,RR),PAX(RR,A,SIMCUR)*QAX(RR,A,SIMCUR))/SUM((A,RR),QAX(RR,A,SIMCUR));
table5('ALL','Employment',F,SIMCUR)                                                    =SUM(A,tablesectorsum(A,'Employment',F,SIMCUR) ) ;
table5('ALL','Wages',F,SIMCUR)$SUM((A,RR),QFX(RR,F,RR,A,SIMCUR))                       =SUM((A,RR),WFAX(RR,F,RR,A,SIMCUR)*QFX(RR,F,RR,A,SIMCUR))/SUM((A,RR),QFX(RR,F,RR,A,SIMCUR));
table5('ALL','Trade','Exports',SIMCUR)                                                 =SUM(A,tablesectorsum(A,'Trade','Exports',SIMCUR) );
table5('ALL','Trade','Imports',SIMCUR)                                                 =SUM(A,tablesectorsum(A,'Trade','Imports',SIMCUR) );
table5('ALL','Trade','Trade Deficit',SIMCUR)                                           =SUM(A,tablesectorsum(A,'Trade','Trade Deficit',SIMCUR));



table5P(AAGG,'Dom. Mark.','Production',SIMCUR) $table5(AAGG,'Dom. Mark.','Production','BASE')       =100*(table5(AAGG,'Dom. Mark.','Production',SIMCUR)   /table5(AAGG,'Dom. Mark.','Production','BASE')-1)     ;
table5P(AAGG,'Dom. Mark.','Consumption',SIMCUR)$table5(AAGG,'Dom. Mark.','Consumption','BASE')      =100*(table5(AAGG,'Dom. Mark.','Production',SIMCUR)   /table5(AAGG,'Dom. Mark.','Production','BASE')-1)     ;
table5P(AAGG,'Dom. Mark.','Prices',SIMCUR)     $table5(AAGG,'Dom. Mark.','Prices','BASE')           =100*(table5(AAGG,'Dom. Mark.','Prices',SIMCUR)       /table5(AAGG,'Dom. Mark.','Prices','BASE')-1)         ;
table5P(AAGG,'Employment',F,SIMCUR)            $table5(AAGG,'Employment',F,'BASE')                  =100*(table5(AAGG,'Employment',F,SIMCUR)              /table5(AAGG,'Employment',F,'BASE')-1)                ;
table5P(AAGG,'Wages',F,SIMCUR)                 $table5(AAGG,'Wages',F,'BASE')                       =100*(table5(AAGG,'Wages',F,SIMCUR)                   /table5(AAGG,'Wages',F,'BASE')-1)                     ;
table5P(AAGG,'Trade','Exports',SIMCUR)         $table5(AAGG,'Trade','Exports','BASE')               =100*(table5(AAGG,'Trade','Exports',SIMCUR)           /table5(AAGG,'Trade','Exports','BASE')-1)             ;
table5P(AAGG,'Trade','Imports',SIMCUR)         $table5(AAGG,'Trade','Imports','BASE')               =100*(table5(AAGG,'Trade','Imports',SIMCUR)           /table5(AAGG,'Trade','Imports','BASE')-1)             ;
table5P(AAGG,'Trade','Trade Deficit',SIMCUR)   $table5(AAGG,'Trade','Trade Deficit','BASE')         =100*(table5(AAGG,'Trade','Trade Deficit',SIMCUR)     /table5(AAGG,'Trade','Trade Deficit','BASE')-1)       ;

table5P(AAGG,'Dom. Mark.','Production','BASE')        = table5(AAGG,'Dom. Mark.','Production','BASE');
table5P(AAGG,'Dom. Mark.','Consumption','BASE')       = table5(AAGG,'Dom. Mark.','Consumption','BASE') ;
table5P(AAGG,'Employment',F,'BASE')                   = table5(AAGG,'Employment',F,'BASE') ;
table5P(AAGG,'Wages',F,'BASE')                        = table5(AAGG,'Wages',F,'BASE');
table5P(AAGG,'Trade','Exports','BASE')                = table5(AAGG,'Trade','Exports','BASE') ;
table5P(AAGG,'Trade','Imports','BASE')                = table5(AAGG,'Trade','Imports','BASE') ;
table5P(AAGG,'Trade','Trade Deficit','BASE')          = table5(AAGG,'Trade','Trade Deficit','BASE') ;
table5P(AAGG,'Dom. Mark.','Prices','BASE')            = table5(AAGG,'Dom. Mark.','Prices','BASE') ;

SET VAVR        value added variables
/PVA, QVA, GVA/ ;

Parameter VA(R,A,VAVR,SIM)
VAP(R,A,VAVR,SIM);

VA(RR,A,'PVA',SIMCUR)=PVAX(RR,A,SIMCUR);
VA(RR,A,'QVA',SIMCUR)=QVAX(RR,A,SIMCUR);
VA(RR,A,'GVA',SIMCUR)=PVAX(RR,A,SIMCUR)*QVAX(RR,A,SIMCUR);


VA('TOT',A,'QVA',SIMCUR)=SUM(RR,QVAX(RR,A,SIMCUR));
VA('TOT',A,'GVA',SIMCUR)=SUM(RR,PVAX(RR,A,SIMCUR)*QVAX(RR,A,SIMCUR));
VA('TOT',A,'PVA',SIMCUR)=VA('TOT',A,'GVA',SIMCUR)/VA('TOT',A,'QVA',SIMCUR);



VAP(R,A,VAVR,SIMCUR)$VA(R,A,VAVR,'BASE')=VA(R,A,VAVR,SIMCUR)/VA(R,A,VAVR,'BASE')*100-100;

*dump table data
execute_unload 'gdx\80report.gdx'
execute 'gdxxrw gdx\80report.gdx o=result.xlsx index=index!A1'
