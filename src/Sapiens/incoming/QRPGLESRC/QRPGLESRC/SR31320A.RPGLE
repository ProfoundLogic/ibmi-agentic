     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
      //===============================================================*
     H/TITLE SR31320A - NY - Optional Labor Contractor Endorsement
      //===============================================================*
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      // Created   : 03/27/20  C:79  Project: WC2736                   *
      //===============================================================*
      // REVISIONS:
      //
      //   10/19/22 C:342 Project: WC2834                              *
      //                  Endorsement Recognition of Cancellation      *
      //                                                               *
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

      // Copybook F specs
     F/COPY SCOPYEN01F

      //* Statistical Reporting/Rating File
     Fwdrrl     if   e           k disk
      //=========================================================================
      //  Definition Specs
      //=========================================================================
     d intCxDate7      s              7  0

      // Copybook D specs
     D/COPY SCOPYEN01D
      //=========================================================================
      //  /COPY required constants
      //=========================================================================
      // Form ID
     D qqForm          s             15a   inz('WC310320A ')

      // Additional entry program
     D addPgm          c                   const('SRXXXXXXXX')
      //=========================================================================
      //  Prototypes
      //=========================================================================
      //Entry parameters
     D SR31320A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR31320A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      //***** Beg Del *** 10/19/22 **********************************
      *****/Copy WprEmplr
      //***** End Del *** 10/19/22 **********************************
      //=========================================================================
      //  *LIKE DEFN definitions
      //=========================================================================
     C     *like         define    rpemp#        x1Emp#
     C     *like         define    rpdiv         x1Div
      //=========================================================================
      //  KLIST definitions
      //=========================================================================
     C     rrKy          klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    svJrSt
     C                   kfld                    ELpfDt
      //
     C     rrKy1         klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    svJrSt

     C     rpKy1         KList
     C                   KFld                    xxCo#
     C                   KFld                    xxFnd
     C                   KFld                    x1Emp#
     C                   KFld                    x1Div
     C                   KFld                    xxFyr
      //=========================================================================
      //  Main
      //=========================================================================
      // Copybook C specs
     C/COPY SCOPYEN01C
      //=========================================================================
      //  OTHINF - Subroutine to load form specific information & write record
      //=========================================================================
      /FREE
       Begsr OthInf;

          //* Get PEO Policy Number

          x1Emp# = 1;
          x1Div = 0;
          open WMRPL;
          Chain rpKy1 prp01;

          If %Found(WMRPL) and (RPcap# <> *blanks);
             FMpol# = %Subst(RPcap#:1:15);

          Else;
             FMpol# = qqFnd + %Editc(x1Emp#:'X') + %Editc(xxFyr:'X');
          Endif;

          close WMRPL;

          // write endorsements

          Exsr PeoGrp;

       Endsr;
       //=========================================================================
       //  PEOGRP - Run through WDELP for NY divisions. Print those not flat cx'd
       //=========================================================================
       Begsr PeoGrp;
       Setll (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
       Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
       Dow Not %Eof(WDELL1);
          Chain (Elco#:ELrtcd) prs01;
          Select;
          When FNpeof = 'M' and ELemp# = 1;
          When FNpeof = 'C' and Elemp# = 1;

          Other;
             If RSjrst = ENst;
                Chain (ELco#:ELfnd:ELemp#:ELdiv) pem01;
                If ELpfDt <> GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
                   Exsr PeoWrt;
                Endif;
             Endif;
          Endsl;
       Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
       Enddo;
       Endsr;
      //=========================================================================
      //  PEOWRT - PEO GROUP - Write Endorsement
      //=========================================================================
       Begsr PeoWrt;

          clear FMnm01;
          clear FMnm02;
          clear FMead1;
          clear FMead2;
          clear FMead3;

          FMnm01 = EMname;
          FMnm02 = 'FEIN: ' +  %editc(EMfein:'X');

          If EMpad1 <> *blanks;
             FMead1 = EMpad1;
             FMead2 = EMpad2;
             xxZip = %EditC(EMpZip:'X');
             FMeAd3 = %Trim(EMpCty) + ', ' + %Trim(EMpSt);
          Else;
             FMead1 = EMmad1;
             FMead2 = EMmad2;
             xxZip = %EditC(EMmZip:'X');
             FMeAd3 = %Trim(EMmCty) + ', ' + %Trim(EMmSt);
          Endif;

             FMeAd3 = %Trim(FMeAd3) + ' ' + %Trim(xxZip1);
          If xxZip2 <> *blank
                 and xxZip2 <> *zero;
             FMeAd3 = %Trim(FMeAd3) + '-';
             FMeAd3 = FMeAd3 + %Trim(xxZip2);
          Endif;

          //* Write the output record
          Exsr WriteWorkFile;

       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr PgmStr;
          WriteInCopybook = *Off;
       Endsr;
      //=========================================================================
      //  PGMEND - End of program processing
      //=========================================================================
       Begsr PgmEnd;
       Endsr;
       //=========================================================================
       //  Valid - Attach Criteria
       //=========================================================================
       Begsr Valid;
          crtFrm = *On;
          Setgt  rrKy prr01;
          Readpe rrKy1 prr01;
          intCxDate7 = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If RRpgfl = *blank or ELpfDt = intCxDate7;
             crtFrm = *Off;
          Endif;
       Endsr;
      /End-Free
