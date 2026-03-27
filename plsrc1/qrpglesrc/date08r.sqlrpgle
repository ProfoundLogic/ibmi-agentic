**Free

Ctl-opt option(*noDebugIo)   DftActGrp(*no);

// -------------------------------------------------------------------------
//
//  Description: Date Test 8:  SQL 10 Character Format
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
  heading = 'Date8r - 10 Character Format with SQL';

  SQL_Char_10();
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
Dcl-Proc RPG_Char_10;

  Setll *start EmpMaster;
  Dou %eof(EmpMaster);

    read EmpMaster;
    if %eof(EmpMaster);
      leave;
    endif;

    s1EmpName = %trim(lName) + ', ' + %trim(fName);
    s1HireDt = %char(HIRE_DSTR);


    s1Anniv = %subst(HIRE_DSTR:6:2) + '/'
            + %subst(HIRE_DSTR:9:2) + '/'
            + %char(%subdt(Today:*Y)) ;

    // Adjust for past anniversaries this year
    if %Date(s1Anniv:*usa) < Today;
      s1Anniv = %subst(HIRE_DSTR:6:2) + '/'
              + %subst(HIRE_DSTR:9:2) + '/'
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
Dcl-Proc SQL_Char_10;

  Sqlstmt =
    'Select * from EmpMaster '
    + 'WHERE DAYOFYEAR(DATE(HIRE_DSTR) +'
    + ' (YEAR(CURRENT_DATE) - YEAR(HIRE_DSTR)) YEARS) '
    + '   BETWEEN DAYOFYEAR(CURRENT_DATE) '
    + '   AND DAYOFYEAR(CURRENT_DATE + ' + %char(nbrDays) + ' DAYS)'
    + ' OR ('
    + '  DAYOFYEAR(CURRENT_DATE) > '
    + '  DAYOFYEAR(CURRENT_DATE + ' + %char(nbrDays) + ' DAYS)'
    + ' AND ('
    + '  DAYOFYEAR(DATE(HIRE_DSTR) + '
    + '  (YEAR(CURRENT_DATE) - YEAR(HIRE_DSTR)) YEARS) '
    + '  >= DAYOFYEAR(CURRENT_DATE) '
    + '  OR DAYOFYEAR(DATE(HIRE_DSTR) + '
    + '  (YEAR(CURRENT_DATE) - YEAR(HIRE_DSTR)) YEARS) '
    + '  <= DAYOFYEAR(CURRENT_DATE + ' + %char(nbrDays) + ' DAYS))) ';

  Exec Sql Declare C3 Cursor For Sqlstmt3;
  Exec Sql Prepare Sqlstmt3 From :Sqlstmt;
  Exec Sql Open C3;

  dou SqlCod <> *Zero;
    Exec Sql Fetch Next From C3 Into :Sq_Data;

    if SqlCod <> *zero;
      leave;
    endif;

    s1EmpName = %trim(lName) + ', ' + %trim(fName);
    s1HireDt = %char(HIRE_DSTR);

    s1Anniv =  %subst(HIRE_DSTR:6:2) + '/'
              + %subst(HIRE_DSTR:9:2) + '/'
              + %char(%subdt(Today:*Y)) ;

    if %Date(s1Anniv:*usa) < Today;

      s1Anniv = %subst(HIRE_DSTR:6:2) + '/'
              + %subst(HIRE_DSTR:9:2) + '/'
              + %char(%subdt(Today:*Y) + 1) ;
    endif;
    rrn = rrn + 1;
    write GRID;

  enddo;
  Exec Sql Close C3;


End-Proc;

