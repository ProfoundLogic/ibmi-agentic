     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')
      /TITLE SR38401A - RI Short-Rate print/attach program
      //===============================================================*
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      // Created   : 10/01/14                                          *
      // Programmer: C:302                                             *
      // Project   : WC2271
      //===============================================================*
      // REVISIONS:                                                    *
      *                                                               *
      * 10/12/16 C:267 Project: SY371                                 *
      *                Scriptura phase 2                              *
      *                Clean up Program/Copybook                      *
      *                                                               *
      //===============================================================*
      // Notes:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      //                                                               *
      //  QQMODE = 1 - Called to determine if form should be attached. *
      //  QQMODE = 2 - Called to print endorsement.                    *
      //  QQMODE = 3 - Called for additional input.                    *
      //  QQMODE = 4 - Called for mid-term processing.                 *
      //===============================================================*

      /COPY SCOPYEN01F

     FWDSTL     if   e           k disk

      /COPY SCOPYEN01D

     d edds            ds
     d  emefd1
     d  emefd2
     d  emefd3
     d  emefd4
     d  emefd5
     d ed                                  OVERLAY(edds)
     d                                     LIKE(emefd1) DIM(5)

      // employer cancel dates
     d cdds            ds
     d  emcad1
     d  emcad2
     d  emcad3
     d  emcad4
     d  emcad5
     d cd                                  OVERLAY(cdds)
     d                                     LIKE(emcad1) DIM(5)

     dkkco#            s              3  0
     dkkfnd            s              3  0
     dkkst             s              2
     dkkefdt           s              7  0
     dtmpefdt          s              7  0
     dsysdat           s              7  0

     D print           s               n   inz(*off)
     D qqform          s             15A   Inz('WC380401A ')
     D addpgm          c                   const('SRXXXXXXXX')

     D  FM200          ds
     D  fm2001
     D  fm2002
     D  fm2003
     D  fm2004
     D  fm2005
     D  fm2006
     D  fm2007
     D  fm2008
     D  fm2009
     D  fm2010
     D  fm2011
     D  fm2012
     D  fm2013
     D  fm2014
     D  fm2015
     D  fm2016
     D  fm2017
     D  fm2018
     D  fm2019
     D  fm2020
     D  fm2021
     D  fm2022
     D  fm2023
     D  fm2024
     D  fm2025
     D  fm2026
     D  fm2027
     D  fm2028
     D  fm2029
     D  fm2030
     D  fm2031
     D  fm2032
     D  fm2033
     D  fm2034
     D  fm2035
     D  fm2036
     D  fm2037
     D  fm2038
     D  fm2039
     D  fm2040

     D fm2                                 OVERLAY(fm200)
     D                                     LIKE(fm2001) DIM(40)

     D  FM50           ds
     D  fm1f01
     D  fm1f02
     D  fm1f03
     D  fm1f04
     D  fm1f05
     D  fm1f06
     D  fm1f07
     D  fm1f08
     D  fm1f09
     D  fm1f10
     D  fm1f11
     D  fm1f12
     D  fm1f13
     D  fm1f14
     D  fm1f15

     D fm5                                 OVERLAY(fm50)
     D                                     LIKE(fm1f01) DIM(15)

     D  FM40           ds
     D  fmc401
     D  fmc402
     D  fmc403
     D  fmc404
     D  fmc405
     D  fmc406
     D  fmc407
     D  fmc408
     D  fmc409
     D  fmc410
     D  fmc411
     D  fmc412
     D  fmc413
     D  fmc414
     D  fmc415
     D  fmc416

     D fm4                                 OVERLAY(fm40)
     D                                     LIKE(fmc401) DIM(16)

     d x1              s              3  0
     d x2              s              3  0


     D TableData       DS                  QUALIFIED
     D  Day                    1      5A
     D  Percentage             6     11A
     D  PercentSn             12     12A
     D  Space1                13     15A
     D  Factor                16     22A
     D  Space2                23     25A

     D emp_cx          S              1A

     D x               S              3I 0

     D decTemp01       S              9P 5
     D decTemp02       S              9P 5
     D decTemp03       S              9P 5
     D CalcFactor      S              6P 4

      *---------------------------------------------------------------------
      * Prototype Declaration
      *---------------------------------------------------------------------
     d GetSrcVDat      pr                  extPgm('SRCVTDAT')
     d srDti                          8  0
     d srFfmt                         8
     d srTfmt                         8
     d srDto                         10
     d srErr                          1

     D SR38401A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1


     D SR38401A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      /COPY SCOPYEN01C

      /Free

       //=========================================================================
       //  CK_EMP_CX - Check if employer division is cancelled
       //=========================================================================
       begsr ck_emp_cx;

          emp_cx = 'Y';
          chain (ELco#:ELfnd:ELemp#:ELdiv) pem01;

          if %found;

             //  Get Employer Effective/Cancellation date

             x = 5;
             dow (ed(x) = 0) and (x > 1);
                x = x - 1;
             enddo;

             if (cd(x) = 0) or (cd(x) > sysdat);
                emp_cx = 'N';
             endif;

          endif;

       endsr;

       //=========================================================================
       //  OTHINF - Subroutine to load form specific information & write record
       //=========================================================================
       begsr othinf;

             // Short rate information records exists
             kkco# = %dec(qqco#:3:0);
             kkfnd = %dec(qqfnd:3:0);
             kkst = ENst;
             kkefdt = ELpfdt;

             setgt (kkco#:kkfnd:kkst:kkefdt) WDSTL;
             readpe (kkco#:kkfnd:kkst) WDSTL;
             if (not %EOF and STperc <> *zero);
                tmpEfDt = STefdt;
             endif;

             // Counter type of field use
             x1 = 1;

             // Counter for inside each type of field use.
             x2 = *Zeros;

             kkEfDt = tmpEfDt;
             setll (kkco#:kkfnd:kkst:kkefdt) WDSTL;
             reade (kkco#:kkfnd:kkst:kkefdt) WDSTL;

             Dow Not %EOF;

                TableData.Day = %Char(STdays);
                TableData.Percentage = %EditC((STperc):'J');
                Eval(hr) decTemp01 = STDays / 365;
                Eval(hr) decTemp02 = STPerc / 100;
                Eval(hR) decTemp03 = 1 / decTemp01;
                Eval(hr) CalcFactor = decTemp03 * decTemp02;
                TableData.Factor = %EditC((CalcFactor):'J');

                x2 = x2 + 1;

                If x1 < 41;

                   If x2 < 9;
                      If (x2 = 1 or x2 = 2) And
                          (x1 =  1 or x1 = 11 or x1 = 21
                          or x1 = 31);
                         TableData.PercentSn = '%';
                      Else;
                         TableData.PercentSn = ' ';
                      Endif;
                      fm2(x1) = %trim(fm2(x1)) + '   '  + TableData;
                   Else;
                      x1 = x1 + 1;
                      x2 = 1;
                      If (x1 = 11 or x1 = 21 or x1 = 31);
                         TableData.PercentSn = '%';
                      Endif;
                      If x1 < 41;
                         fm2(x1) = %trim(fm2(x1)) + '   ' + TableData;
                      Endif;
                   Endif;

                EndIf;

                If x1 > 40 and x1 < 56;
                      If (x2 = 1 or x2 = 2) And (x1 =  41 );
                         TableData.PercentSn = '%';
                      Else;
                         TableData.PercentSn = ' ';
                      Endif;

                   If x2 < 3;
                      fm5(x1 - 40) = %trim(fm5(x1 - 40)) + '   '  + TableData;
                   Else;
                      x1 = x1 + 1;
                      x2 = 1;
                   If x1 < 56;
                         fm5(x1 - 40) = %trim(fm5(x1 - 40)) + '   ' + TableData;
                      Endif;
                   Endif;

                EndIf;

                If x1 > 55 and x1 < 72;

                      fm4(x1 - 55) = %trim(TableData);
                      x1 = x1 + 1;

                EndIf;

                reade (kkco#:kkfnd:kkst:kkefdt) WDSTL;

             EndDo;

       endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       begsr pgmstr;
          // Retrieve the date in CMDY format.
          sysdat = %dec(%Char(%Date():*cymd0):7:0);
       endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       begsr pgmend;
       endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       begsr valid;
             // Check that employer division is not cancelled
             exsr ck_emp_cx;
             //
             if emp_cx = 'N';

                // Short rate information records exists
                kkco# = %Dec(qqco#:3:0);
                kkfnd = %Dec(qqfnd:3:0);
                kkst = RSjrst;
                kkefdt = ELpfdt;

                setgt (kkco#:kkfnd:kkst:kkefdt) pst01;
                readpe (kkco#:kkfnd:kkst) pst01;
                if (Not %EOF  and stperc <> *zero);
                   tmpefdt = STefdt;
                   crtfrm = *on;
                endif;

             endif;

       endsr;
      /End-free
