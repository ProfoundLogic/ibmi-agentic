     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     *
    H option(*nodebugio:*srcstmt)
    H dftactgrp(*no) actgrp('QILE')
    H BNDDIR('WTBNDDIR')
    H/TITLE SR04602B - Print - Misc Policy Doc's
     *===============================================================*
     * W7413RMPD                                                     *
     *                                                               *
     * Program Description: Program W7413RMPD will print the VA      *
     *  policy notice (W7413OMPD) document.                          *
     *                                                               *
     * Created   : 09/18/12                                          *
     * Programmer: C:282                                             *
     * Project   : C93WKR                                            *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 01/30/17 C:65  PROJECT: SY371                                 *
     *                Add processing to prevent output from being    *
     *                generated if spool file output is disabled.    *
     *                                                               *
     * 10/22/12 C:282 PROJECT: C93WKR PRISS 19                       *
     *                - Changes done updated the zip code detail     *
     *                  and phone detail.                            *
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: Please add comments to the top of revisions.           *
     * ------------------------------------------------------------- *
     *===============================================================*
    fwdell1    if   e           k disk
    fwmrsl     if   e           k disk
    fwmfnl     if   e           k disk
      *
     fW7413ompd o    e             printer               usropn
     *=========================================================================
     *  Definition Specs
     *=========================================================================
     dqqCo#            s              3
     dqqFnd            s              3
     dqqEmp#           s              9
     dqqDiv            s              5
     dqqFyr            s              3
     dqqRtcd           s              1
     dintCo#           s              3  0
     dintFnd           s              3  0
     dintEmp#          s              9  0
     dintFyr           s              3  0
     dcharCty          s             25
     dcharSt           s              2
     d****charZip          s              9
     dflgEmpCx         s              1
     dintCount         s              3  0
     dintCxDate        s              7  0
     d*** begin add ***  01/30/17  ******************************************
     D useSpool        s              1n   inz
     d*** end add *****  01/30/17  ******************************************
      *
     dcharLtl          c                   CONST('.')
      *
     d                 ds
     dintPhn                   1      7  0
     dintAREA                  1      3  0
     dintPhn#                  4      7  0
      *
     d                 ds
     dcharZip                  1      9
     dcharZip5                 1      5
     dcharZip4                 6      9
      *
     d/copy wPrEmpLr
     *=========================================================================
     *  PLIST definitions
     *=========================================================================
     ** Entry parameters
     c     *Entry        Plist
     c                   Parm                    qqCo#
     c                   Parm                    qqFnd
     c                   Parm                    qqEmp#
     c                   Parm                    qqDiv
     c                   Parm                    qqFyr
     c                   Parm                    qqRtcd
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
      * Key List base on file WDELP
     c     keyWDELL1     Klist
     c                   Kfld                    intCo#
     c                   Kfld                    intFnd
     c                   Kfld                    intFyr
     c                   Kfld                    intEmp#
      *
      * Key List base on file WMRSP
     c     keyWMRSL      Klist
     c                   Kfld                    intCo#
     c                   Kfld                    ELrtcd
      *
      * Key List base on file WMFNL
     c     keyWMFNL      Klist
     c                   Kfld                    intCo#
     c                   Kfld                    intFnd
      *
      * Key List base on file WMEMP
     c     keyWMEMLQ     Klist
     c                   Kfld                    intCo#
     c                   Kfld                    intFnd
     c                   Kfld                    intEmp#
     c                   Kfld                    ELdiv
     *=========================================================================
     *  Main
     *=========================================================================
      /free
       //*** begin add ***  01/30/17  ******************************************
       useSpool = *on;
       If qqrtcd = 'N';
          useSpool = *off;
       Endif;
       qqrtcd = ' ';
       //*** end add *****  01/30/17  ******************************************

       intCo#  =  %Int(qqCo#);
       intFnd  =  %Int(qqFnd);
       intEmp# =  %Int(qqEmp#);
       intFyr  =  %Int(qqFyr);

       // Read all of the divisions on the policy year for VA Coverage.

       Setll keyWDELl1 pel01;
       Reade keyWDELl1 pel01;
       Dow Not %Eof(wdell1);
          Chain keyWMRSL prs01;
          If %Found(wmrsl);

       // If a division with VA coverage does exist, leave with return code set as ' '.
             If RSjrst = 'VA';

       // Check Division has active coverage in the policy period (not flat cancelled)
                Exsr Ck_Emp_Cx;

                If flgEmpCx = 'N';
                   qqRtcd = ' ';
                   Leave;
                Else;
                   qqrtcd = '1';
                EndIf;                 // flgEmpCx = 'N';

             Else;
                qqrtcd = '1';
             EndIf;                 // RSjrst = 'VA';

          EndIf;                    // %Found(wmrsl);
          Reade keyWDELl1 pel01;
       EndDo;

       //*** begin chg ***  01/30/17  ******************************************
       //*****If qqRtcd = ' ';
       If qqRtcd = ' ' and useSpool;
       //*** end chg *****  01/30/17  ******************************************

        // Subroutine to populate record in printer file W7413ompd.
          Exsr AddRcdPrtf;

       // Open the Printer File.
          Open  W7413ompd;

          Write NoticeVA;

       // Close the Printer File.
          Close W7413ompd;

       EndIf;                    // qqRtcd = ' ';

       *InLr = *On;

      /end-free
     *=========================================================================
     *  AddRcdPrtf - Populate record in printer file
     *=========================================================================
      /free
       BegSr AddRcdPrtf;

       Chain keyWMFNL pfn01;
       If %Found(WMFNL);
          If (FNmad1 <> ' ' And FNmcty <> ' ' And
             FNmzip  <> 0);
       //***** BEG ADD ******* 10/22/12 **********************************
             addName  = FNname;
       //***** END ADD ******* 10/22/12 **********************************
             addAddr1 = FNmad1;
             addAddr2 = FNmad2;
             charCty  = FNmcty;
             charSt   = FNmst;
             charzip  = %EditC(FNmzip:'X');
          Else;
       //***** BEG ADD ******* 10/22/12 **********************************
             addName  = FNname;
       //***** END ADD ******* 10/22/12 **********************************
             addAddr1 = FNpad1;
             addAddr2 = FNpad2;
             charCty  = FNpcty;
             charSt   = FNpst;
             charzip  = %EditC(FNmzip:'X');
          EndIf;

          If addAddr2 <> *Blanks;
             *In51 = *On;
          Else;
             *In51 = *Off;
          EndIf;

       //***** BEG CHG ******* 10/22/12 **********************************
       // addCtyStZp  = %trim(charCty) + ' ' + charSt + ',' + ' ' + charzip;
       // intPhn      = FNph1;
       // addPhn      = %EditC(FNph1A:'X')  + charLtl +
       //               %EditC(intArea:'X') + charLtl +
       //               %EditC(intPhn:'X');

          addCtyStZp  = %trim(charCty) + ' ' + charSt + ',' + ' ' +
                        charzip5 + '-' + charZip4;
          intPhn      = FNph2;
          addPhn      = %EditC(FNarea:'X')  + charLtl +
                        %EditC(intArea:'X') + charLtl +
                        %EditC(intPhn#:'X');
       //***** END CHG ******* 10/22/12 **********************************
          addAttn  =    FNattn;

       EndIf;                 // %Found(WMFNL);

       EndSr;
      /end-free
     *=========================================================================
     *  CK_EMP_CX - Check if employer division is cancelled
     *=========================================================================
      /free
       BegSr Ck_Emp_Cx;

       flgEmpCx = 'Y';

       // Retrieve the Policy Cacellation Date.
       intCxDate = GetCxDate(intCo#:intFnd:intEmp#:ELDiv);

       // Check Policy is not flat cancelled.
       If intCxDate <> 0 And intCxdate <= ELPFDT;
          flgEmpCx = 'Y';
       Else;
          flgEmpCx = 'N';
       EndIf;

       EndSr;
      /end-free
