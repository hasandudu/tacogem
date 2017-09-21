$TITLE SAM balancing program. Standard CGE modeling system, Version 1.00
$ONTEXT
Version 1.00 Release date: May 02, 2001
Last update: August 17, 2011

This SAM-balancing program is intended for the removal of rounding
errors, not for estimating SAMs with significant imbalances. For an
approach to SAM estimation that is appropriate in such contexts, see:

Robinson, Sherman and Moataz El-Said. "GAMS Code for Estimating a Social
Accounting Matrix (SAM) Using Cross Entropy Methods (CE)" Discussion
Paper 64, Trade and Macroeconomics Division, IFPRI, December 2000.

Robinson, Sherman, Andrea Cattaneo, and Moataz El-Said. "Updating and
Estimating a Social Account Matrix Using Cross Entropy Methods."
Economic Systems Research, Vol. 13, No. 1, 2001, pp. 47-64.
Also available as Discussion Paper 58, Trade and Macroeconomics Division,
IFPRI, August 2000.

In this file:

--the user has the option of removing small cells (with absolute values
below a cutoff point).
--the row/column balance of the SAM is checked.
--a SAM-balancing program is activated if the account imbalances in the SAM
exceed a cut-off point.
--the objective function of the SAM-balancing program is a cross-entropy
distance from the initial SAM coefficients for the entire matrix rather
than column sums. The method approximates the RAS procedure except that
in this case the only restrictions are that column and row sums be equal.
We do not assume known column sums.
--Negative entries are fixed and excluded from the balance procedure.
--Rows/columns with negative sums are also excluded and balanced by a
simple column adjustment.

*HD: The file is now a batinclude file with the argument being the SAM parameter.
*See that the file works only for SAM's with two indices. 

$OFFTEXT

*-CHECKING SAM BALANCE AND REMOVING SMALL CELLS=========================

 SET
  SMLCELL(AC,AC1)       SAM cells with abs value < cutoff are removed
 ;

 PARAMETER
  TDIFF(AC)      column minus row total for account AC in SAM
 ;
 SCALAR
  CUTOFF   lower bound on absolute cell values  %2
 ;
*-If the user does not want to remove small absolute values, then CUTOFF
*-should be set at zero.
*-If the user wants to remove small absolute values then CUTOFF should
*-be set at a low positive value (e.g. 0.005).

 %1('TOTAL',AC1) = 0;
 %1(AC,'TOTAL')  = 0;

*-ACNT-ACNT1 is an element in the set SMLCELL if
*-(1) they have a non-zero SAM cell; and
*-(2) the absolute value of this cell is less than cutoff
 SMLCELL(ACNT,ACNT1)$(%1(ACNT,ACNT1)$(ABS(%1(ACNT,ACNT1))LT cutoff))=YES;

*-The cells corresponding to elements in SMLCELL are set at zero.
 %1(ACNT,ACNT1)$SMLCELL(ACNT,ACNT1) = 0;
 %1('TOTAL',ACNT) = SUM(ACNT1, %1(ACNT1,ACNT));
 %1(ACNT,'TOTAL') = SUM(ACNT1, %1(ACNT,ACNT1));
 TDIFF(ACNT)       = %1('TOTAL',ACNT) - %1(ACNT,'TOTAL');

*-Negative cells are handled by fixing the cell coefficient and
*-excluding them from the adjustment process. If the row/column sum is
*- negative, the adjustment is handled by a one-column RAS.

SETS
 ICOEFF(AC,AC1)   Cells to include in adjustment process
 IROW2(AC)        Row or column to include in process
 ;

*-initialize as yes, then exclude cells
 ICOEFF(ACNT,ACNT1)     = YES ;
 IROW2(ACNT)            = YES ;

*-Exclude cells for which row/column sums are zero or less

LOOP(ACNT$(%1(ACNT,'TOTAL') LE 0),

 IROW2(ACNT)            = NO ;
 ICOEFF(ACNT,ACNT1)     = NO ;
 ICOEFF(ACNT1,ACNT)     = NO ;

 %1(ACNT1,ACNT)$%1(ACNT,'TOTAL') =
      %1(ACNT1,ACNT)*%1('TOTAL',ACNT)/%1(ACNT,'TOTAL') ;

*-end loop statement
 );


*-do not include in the process if tdiff is small enough
 LOOP(ACNT$(tdiff(ACNT) LE 1.0E-8),

 IROW2(ACNT)            = NO ;
 ICOEFF(ACNT,ACNT1)     = NO ;
 ICOEFF(ACNT1,ACNT)     = NO ;

 %1(ACNT1,ACNT)$%1(ACNT,'TOTAL') =
      %1(ACNT1,ACNT)*%1('TOTAL',ACNT)/%1(ACNT,'TOTAL') ;

*-end loop statement
 );


 %1('TOTAL',ACNT) = SUM(ACNT1, %1(ACNT1,ACNT));
 %1(ACNT,'TOTAL') = SUM(ACNT1, %1(ACNT,ACNT1));
 TDIFF(ACNT)       = %1('TOTAL',ACNT) - %1(ACNT,'TOTAL');

*-SAM BALANCING PROGRAM==========================================
*-This program assures that the SAM is perfectly balanced. Put an
*-asterisk before the solve statement if you do not want to run it.
*-Currently it runs if the sum of the absolute values of the account
*-imbalances exceed an upper limit close to zero.

PARAMETER
 SUMABSDEV  sum of absolute values of column-row imbalances;
 SUMABSDEV = SUM(ACNT, ABS(tdiff(ACNT)));
DISPLAY SUMABSDEV;

PARAMETER
 COEFF0(AC,AC1)   Initial SAM coefficients
 ;
SCALAR
 TOTWGHT   total of all SAM cells
 delta1     epsilon value for cross-entropy minimand            /.00001 /
 delta2    lower bound for coefficients based on cutoff value
  ;

 TOTWGHT              = SUM((ACNT,ACNT1), %1(ACNT,ACNT1)) ;
 COEFF0(ACNT,ACNT1)   = %1(ACNT,ACNT1)/TOTWGHT ;

 delta2 = (CUTOFF/TOTWGHT) ;

*-Exclude all cells which are zero or negative.
 ICOEFF(ACNT,ACNT1)$(COEFF0(ACNT,ACNT1) LE 0) = NO ;

VARIABLES
 CENTROPY      Cross entropy measure
 COEFF(AC,AC1) SAM coefficients which sum to one over entire matrix
 ;

*-Initialize variables
 COEFF.L(ACNT, ACNT1) = COEFF0(ACNT,ACNT1);
 CENTROPY.L           = 0 ;

EQUATIONS
 ENTROPYEQ   Cross entropy objective function
 COLROWBAL   column total equals row total
 SUMCFEQ     Total sum  of coefficients
 ;

 ENTROPYEQ..
 CENTROPY =E= SUM((ACNT,ACNT1)$ICOEFF(acnt,ACNT1),
                 COEFF(acnt,ACNT1)*(log(COEFF(acnt,ACNT1) + delta1)
                 - log(COEFF0(acnt,ACNT1) + delta1))) ;

 COLROWBAL(ACNT)$IROW2(ACNT)..
  SUM(ACNT1, COEFF(ACNT1,ACNT)) =E= SUM(ACNT1, COEFF(ACNT,ACNT1));

 SUMCFEQ..
  SUM((ACNT,ACNT1), COEFF(ACNT,ACNT1)) =E= 1 ;


 MODEL
  SAMBAL SAM balancing program for removing rounding errors
  /
   ENTROPYEQ
   COLROWBAL
   SUMCFEQ
  /;


*-Fix cells not to be changed.
*-After balancing, no unfixed cell will have value less than cutoff.

 COEFF.LO(ACNT,ACNT1)$ICOEFF(ACNT,ACNT1)       = delta2 ;
 COEFF.FX(ACNT,ACNT1)$(NOT ICOEFF(ACNT,ACNT1)) = COEFF0(ACNT,ACNT1) ;

 OPTION LIMROW=0, LIMCOL=0;
 SAMBAL.HOLDFIXED = 1;
 OPTION NLP=MINOS5;
*-OPTION NLP=CONOPT2;

*-IF(SUMABSDEV GT 0.00000001,
IF(SUMABSDEV GT 1.0E-6,
  SOLVE SAMBAL MINIMIZING CENTROPY USING NLP;
);

*-Reload new SAM

 %1(ACNT,ACNT1)   = coeff.l(ACNT,ACNT1)*TOTWGHT ;

 %1('TOTAL',ACNT) = SUM(ACNT1, %1(ACNT1,ACNT));
 %1(ACNT,'TOTAL') = SUM(ACNT1, %1(ACNT,ACNT1));

 Parameter
 SUMABSDEVPOST      sum of absolute values of column-row imbalances after balancing
 TDIFFPOST(AC)    column minus row total for account AC in SAM after balancing;

 TDIFFPOST(ACNT)= %1('TOTAL',ACNT) - %1(ACNT,'TOTAL');



 SUMABSDEVPOST= SUM(ACNT, ABS(tdiffpost(ACNT)));
