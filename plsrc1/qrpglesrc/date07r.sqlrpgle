**Free

Ctl-opt option(*noDebugIo)   DftActGrp(*no);

// -------------------------------------------------------------------------
//
//  Description: Date Test 7: Timestamp Format With SQL
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
  heading = 'Date7r - Timestamp Format With SQL';

  SQL_tsFormat();
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
Dcl-Proc RPG_tsFormat;

  Setll *start EmpMaster;
  Dou %eof(EmpMaster);

    read EmpMaster;
    if %eof(EmpMaster);
      leave;
    endif;

    s1EmpName = %trim(lName) + ', ' + %trim(fName);
    s1HireDt = %char(hire_ts);


    s1Anniv = %char(%subdt(hire_ts:*M)) + '/'
            + %char(%subdt(hire_ts:*D)) + '/'
            + %char(%subdt(Today:*Y)) ;

    // Adjust for past anniversaries this year
    if %Date(s1Anniv:*usa) < Today;
      s1Anniv = %char(%subdt(hire_ts:*M)) + '/'
              + %char(%subdt(hire_ts:*D)) + '/'
              + %char(%subdt(Today:*Y) + 1) ;
    endif;

    // Check if within the next 30 days
    if %date(s1Anniv:*usa) <= annivDate;
      rrn = rrn + 1;
      write GRID;
    endif;

  enddo;

End-Proc;

// ----------------------------------------------------------------
Dcl-Proc SQL_tsFormat;

  Sqlstmt =
    'Select * from EmpMaster '
    + 'WHERE DAYOFYEAR(DATE(hire_ts) +'
    + ' (YEAR(CURRENT_DATE) - YEAR(hire_ts)) YEARS) '
    + '   BETWEEN DAYOFYEAR(CURRENT_DATE) '
    + '   AND DAYOFYEAR(CURRENT_DATE + ' + %char(nbrDays) + ' DAYS)'
    + ' OR ('
    + '  DAYOFYEAR(CURRENT_DATE) > '
    + '  DAYOFYEAR(CURRENT_DATE + ' + %char(nbrDays) + ' DAYS)'
    + ' AND ('
    + '  DAYOFYEAR(DATE(hire_ts) + '
    + '  (YEAR(CURRENT_DATE) - YEAR(hire_ts)) YEARS) '
    + '  >= DAYOFYEAR(CURRENT_DATE) '
    + '  OR DAYOFYEAR(DATE(hire_ts) + '
    + '  (YEAR(CURRENT_DATE) - YEAR(hire_ts)) YEARS) '
    + '  <= DAYOFYEAR(CURRENT_DATE + ' + %char(nbrDays) + ' DAYS))) ';

  Exec Sql Declare C2 Cursor For Sqlstmt2;
  Exec Sql Prepare Sqlstmt2 From :Sqlstmt;
  Exec Sql Open C2;

  dou SqlCod <> *Zero;
    Exec Sql Fetch Next From C2 Into :Sq_Data;

    if SqlCod <> *zero;
      leave;
    endif;

    s1EmpName = %trim(lName) + ', ' + %trim(fName);
    s1HireDt = %char(hire_ts);

    s1Anniv = %char(%subdt(hire_ts:*M)) + '/'
             + %char(%subdt(hire_ts:*D)) + '/'
             + %char(%subdt(Today:*Y)) ;

    if %Date(s1Anniv:*usa) < Today;

      s1Anniv = %char(%subdt(hire_ts:*M)) + '/'
              + %char(%subdt(hire_ts:*D)) + '/'
              + %char(%subdt(Today:*Y) + 1) ;
    endif;
    rrn = rrn + 1;
    write GRID;

  enddo;
  Exec Sql Close C2;


End-Proc;


