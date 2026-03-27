**Free

Ctl-opt option(*noDebugIo)   DftActGrp(*no);

// -------------------------------------------------------------------------
//
//  Description: Date Test 10:  Decimal 6 Format With SQL
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
  heading = 'Date10r - Decimal 6 Format With SQL';

  SQL_Dec6();
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
Dcl-Proc RPG_Dec6;

  Setll *start EmpMaster;
  Dou %eof(EmpMaster);

    read EmpMaster;
    if %eof(EmpMaster);
      leave;
    endif;

    s1EmpName = %trim(lName) + ', ' + %trim(fName);
    s1HireDt =  %Char(%date(%char(hire_d6): *ymd0));


    s1Anniv = %char(%subdt(%date(%char(hire_d6): *ymd0):*M)) + '/'
             + %char(%subdt(%date(%char(hire_d6): *ymd0):*D)) + '/'
             + %char(%subdt(Today:*Y)) ;

    // Adjust for past anniversaries this year
    if %Date(s1Anniv:*usa) < Today;
      s1Anniv = %char(%subdt(%date(%char(hire_d6): *ymd0):*M)) + '/'
              + %char(%subdt(%date(%char(hire_d6): *ymd0):*D)) + '/'
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
Dcl-Proc SQL_Dec6;

  dcl-s dateString varchar(60);

  dateString = 'INSERT(INSERT('
   + q + '20' + q + ' || CHAR(hire_D6),5,0,'
   + q + '-' + q + '),8,0,' + q + '-' + q + ')';

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

  Exec Sql Declare C5 Cursor For Sqlstmt5;
  Exec Sql Prepare Sqlstmt5 From :Sqlstmt;
  Exec Sql Open C5;

  dou SqlCod <> *Zero;
    Exec Sql Fetch Next From C5 Into :Sq_Data;

    if SqlCod <> *zero;
      leave;
    endif;


    s1EmpName = %trim(lName) + ', ' + %trim(fName);
    s1HireDt =  %Char(%date(%char(hire_d6): *ymd0));


    s1Anniv = %char(%subdt(%date(%char(hire_d6): *ymd0):*M)) + '/'
             + %char(%subdt(%date(%char(hire_d6): *ymd0):*D)) + '/'
             + %char(%subdt(Today:*Y)) ;

    // Adjust for past anniversaries this year
    if %Date(s1Anniv:*usa) < Today;
      s1Anniv = %char(%subdt(%date(%char(hire_d6): *ymd0):*M)) + '/'
              + %char(%subdt(%date(%char(hire_d6): *ymd0):*D)) + '/'
              + %char(%subdt(Today:*Y) + 1) ;
    endif;
    rrn = rrn + 1;
    write GRID;

  enddo;
  Exec Sql Close C5;


End-Proc;

