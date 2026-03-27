**Free

Ctl-opt option(*noDebugIo)   DftActGrp(*no);

// -------------------------------------------------------------------------
//
//  Description: Date Test 6:  Date Format With SQL
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
  Heading = 'Date6r - Date Format With SQL';
  SQL_DateFmt();
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
Dcl-Proc SQL_DateFmt;

  Sqlstmt =
    'Select * from EmpMaster '
    + 'WHERE DAYOFYEAR(DATE(hire_date) +'
    + ' (YEAR(CURRENT_DATE) - YEAR(hire_date)) YEARS) '
    + '   BETWEEN DAYOFYEAR(CURRENT_DATE) '
    + '   AND DAYOFYEAR(CURRENT_DATE + ' + %char(nbrDays) + ' DAYS)'
    + ' OR ('
    + '  DAYOFYEAR(CURRENT_DATE) > '
    + '  DAYOFYEAR(CURRENT_DATE + ' + %char(nbrDays) + ' DAYS)'
    + ' AND ('
    + '  DAYOFYEAR(DATE(hire_date) + '
    + '  (YEAR(CURRENT_DATE) - YEAR(hire_date)) YEARS) '
    + '  >= DAYOFYEAR(CURRENT_DATE) '
    + '  OR DAYOFYEAR(DATE(hire_date) + '
    + '  (YEAR(CURRENT_DATE) - YEAR(hire_date)) YEARS) '
    + '  <= DAYOFYEAR(CURRENT_DATE + ' + %char(nbrDays) + ' DAYS))) ';

  Exec Sql Declare C1 Cursor For Sqlstmt;
  Exec Sql Prepare Sqlstmt From :Sqlstmt;
  Exec Sql Open C1;

  dou SqlCod <> *Zero;
    Exec Sql Fetch Next From C1 Into :Sq_Data;

    if SqlCod <> *zero;
      leave;
    endif;

    s1EmpName = %trim(lName) + ', ' + %trim(fName);
    s1HireDt = %char(hire_date);

    s1Anniv = %char(%subdt(hire_date:*M)) + '/'
             + %char(%subdt(hire_date:*D)) + '/'
             + %char(%subdt(Today:*Y)) ;

    if %Date(s1Anniv:*usa) < Today;

      s1Anniv = %char(%subdt(hire_date:*M)) + '/'
              + %char(%subdt(hire_date:*D)) + '/'
              + %char(%subdt(Today:*Y) + 1) ;
    endif;
    rrn = rrn + 1;
    write GRID;

  enddo;
  Exec Sql Close C1;


End-Proc;

