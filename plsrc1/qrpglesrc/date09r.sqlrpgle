**Free

Ctl-opt option(*noDebugIo)   DftActGrp(*no);

// -------------------------------------------------------------------------
//
//  Description: Date Test 9:  Decimal 8 With SQL
//  Programmer.: Brian Rees
//  Date.......: July 2025
//
// -------------------------------------------------------------------------


// -------------------------------------------------------------------------
//  Declare Files
// -------------------------------------------------------------------------
Dcl-f dateScreen WorkStn
    sfile(GRID:rrn);
Dcl-f EmpMaster rename(EmpMaster: Rcd1);


// -------------------------------------------------------------------------
//  Global Variables
// -------------------------------------------------------------------------

dcl-s rrn Zoned(5);
dcl-s Today        date;
dcl-s annivDate   date;
dcl-s Anniversary  date;
dcl-c q '''';

dcl-s Sqlstmt Char(1000);
dcl-ds Sq_Data ExtName('EMPMASTER') End-DS;


// -------------------------------------------------------------------------
//  Mainline Program
// -------------------------------------------------------------------------
nbrDays = 30;
Today = %date();

Dou *in03 = *on;
  annivDate = %date() + %days(nbrDays);
  CLEARS1();
  heading = 'Date9r - Decimal 8 With SQL';

  SQL_Dec8();
  scrDate = uDate;
  *in71 = *on;
  if rrn > 0;
    *in70 = *off;
  endif;
  write fkey;
  exfmt MAINSCREEN;

Enddo;

*inlr = *on;

// ----------------------------------------------------------------
dcl-proc CLEARS1;

  *in71 = *off;
  *in70 = *on;
  *in72 = *on;
  Write MAINSCREEN;
  *in70= *off;
  *in72 = *off;

  rrn = 0;

End-Proc;

// ----------------------------------------------------------------
Dcl-Proc SQL_Dec8;

  dcl-s dateString varchar(60);

  dateString = 'INSERT(INSERT(LEFT(CHAR(hire_Dnum),8),5,0,'
    + q+'-'+q+'),8,0,'+q+'-'+q+')' ;

  Sqlstmt =
      'Select * from EmpMaster '
    + 'WHERE DAYOFYEAR(DATE(' + dateString + ') +'
    + ' (YEAR(CURRENT_DATE) - YEAR(' + dateString + ')) YEARS) '
    + '   BETWEEN DAYOFYEAR(CURRENT_DATE) '
    + '   AND DAYOFYEAR(CURRENT_DATE + ' + %char(nbrDays) + ' DAYS)'
    + ' OR ('
    + '  DAYOFYEAR(CURRENT_DATE) > '
    + '  DAYOFYEAR(CURRENT_DATE + ' + %char(nbrDays) + ' DAYS)'
    + ' AND ('
    + '  DAYOFYEAR(DATE(' + dateString + ') + '
    + '  (YEAR(CURRENT_DATE) - YEAR(' + dateString + ')) YEARS) '
    + '  >= DAYOFYEAR(CURRENT_DATE) '
    + '  OR DAYOFYEAR(DATE(' + dateString + ') + '
    + '  (YEAR(CURRENT_DATE) - YEAR(' + dateString + ')) YEARS) '
    + '  <= DAYOFYEAR(CURRENT_DATE + ' + %char(nbrDays) + ' DAYS))) ';

  Exec Sql Declare C4 Cursor For Sqlstmt4;
  Exec Sql Prepare Sqlstmt4 From :Sqlstmt;
  Exec Sql Open C4;

  dou SqlCod <> *Zero;
    Exec Sql Fetch Next From C4 Into :Sq_Data;

    if SqlCod <> *zero;
      leave;
    endif;


    s1EmpName = %trim(lName) + ', ' + %trim(fName);
    s1HireDt =  %Char(%date(%char(hire_dnum): *iso0));


    s1Anniv = %char(%subdt(%date(%char(hire_dnum): *iso0):*M)) + '/'
             + %char(%subdt(%date(%char(hire_dnum): *iso0):*D)) + '/'
             + %char(%subdt(Today:*Y)) ;

    // Adjust for past anniversaries this year
    if %Date(s1Anniv:*usa) < Today;
      s1Anniv = %char(%subdt(%date(%char(hire_dnum): *iso0):*M)) + '/'
              + %char(%subdt(%date(%char(hire_dnum): *iso0):*D)) + '/'
              + %char(%subdt(Today:*Y) + 1) ;
    endif;
    rrn = rrn + 1;
    write GRID;

  enddo;
  Exec Sql Close C4;


End-Proc;

