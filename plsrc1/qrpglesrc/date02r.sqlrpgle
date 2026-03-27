**Free

Ctl-opt option(*noDebugIo)   DftActGrp(*no);

// -------------------------------------------------------------------------
//
//  Description: Date Test 2: Timestamp Format With RPG
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
  heading = 'Date2r - Timestamp Format With RPG';

  RPG_tsFormat();
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

