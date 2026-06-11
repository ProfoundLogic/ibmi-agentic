     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE FTBILLPRXE - Perform Rx Bill Import Errors
     H DFTNAME(FTBILLPRXE)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H/DEFINE   PROFOUNDUI
     H BNDDIR('FTBNDDIR')
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     h**********************************************************************
      *===============================================================*
      *  Created   : 04/12/2016                                       *
      *  Programmer: 309                                              *
      *  Project   : 114MXO                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      *  03/29/22 C:333 Project: SY434                                *
      *                 - Update Grids for Sorting Filtering Upgrade  *
      *                                                               *
      *  06/09/20  C:212 Project : SY420                              *
      *                  Mask SSN                                     *
      *                                                               *
      *                                                               *
      *===============================================================*

     fftbillprxecf   e             workstn extfile('FTBILLPRXE')
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflBilImEr:rrnBilImEr)
     f*
     ffak1p     uf   e             disk
      *
     ffak1l2    if   e           k disk    rename(pk101:pk102)
     f                                     infds(dbfds1)
      *
     ffmcml1    if   e           k disk
     F                                     rename(pcm01:pcm02)
      *

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

    d* data structures
     d psds          esds                  extname(szq1p)
      *
     d dbfds1          ds
     d  rrnfak1p             397    400b 0

    d* standalone variables
     d ##mode          s              1    inz
     d addMode         s               n   inz(*off)
     d chgMode         s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d isValid         s               n   inz(*off)
     d isError         s               n   inz(*off)
     d mode            s              1    inz
     d process         s               n   inz(*off)
     d @@mode          s              1
     d xxCO#           s                   like(CMCO#)
     d xxFnd           s                   like(CMFND)
     d xxFyr           s                   like(CMFYR)
     d xxCase          s                   like(CMCASE)
     d varLen          s              2  0
     d varLenCty       s              2  0
     d uc              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     d lc              c                   'abcdefghijklmnopqrstuvwxyz'
     d inCall          s              1
     d inSub#          s              5  0
     d inFnd           s              3  0
     d inMember#       s             10
     d inBatchid       s              7
     d inRrnk1p        s              9  0
     d inFyr           s              3  0
     d inCase          s              7  0
     d inSS#           s              9
     d inAcdt          s              8  0
     d Outrtcd         s              1
     d xxDefDate       s               d   inz


     *---------------------------------------*
     * Procedure definitions
     *---------------------------------------*

     d RunCommand      pr                  extPgm('QCMDEXC')
     d inCmd                       2750    const
     d inCmdLen                      15  5 const

    d* Clear Subfiles
     d ClrSubFiles     pr

    d* load Bill Import Error grid
     dloadBilImpErrs   pr

    d* main Processing
     d main            pr

    d* program Start
     d programStart    pr
     d processMe                       n

    d* process transfer
     d processtrnsfr   pr
      *

    d* process transfer
     d ClmSearch       pr                  ExtPgm('F0269R')
     d inCall                         1
     d inSub#                         5  0
     d inFnd                          3  0
     d inFyr                          3  0
     d inCase                         7  0
     d inSS#                          9
     d inAcdt                         8  0
     d OutRtcd                        1
      *

    d* process other options
     d procOther       pr

    d* Validate Claim#
     d Validate        pr

    d* Update FAK1P file with new Claim#
     d Updatek1p       pr

    d* Transfer Bill
     d TransferBill    pr

      *
     d CallF6502R      pr                  ExtPgm('F6502R')
     d inMember#                     10
     d inBatchid                      7
     d inRrnk1p                       9  0

     d/copy sPrUsrPrms

     * Entry parameter
     d FTBILLPRXER     pr                  Extpgm('FTBILLPRXE')
     d  @@call                        1

     d FTBILLPRXER     pi
     d  @@call                        1
      /Free

       ProgramStart(process);

       If process;
         Main();
       Endif;

       *inlr = *on;
       Return;

      /end-free
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     P ClrSubFiles     B
     D ClrSubFiles     PI
      /Free

       scRrnBLIME = 0;
       rrnfak1p   = 0;
       rrnBilImEr = 0;
       ScRowRrn   = 0;

       clrBLSub = *On;
       dspBLSub = *Off;
       dspBLCtl = *Off;

       Write RcdBilImEr;
      /End-Free
     P ClrSubfiles     E

      //===================================================================
      // Load Import bill Error Screen
      //===================================================================
     PloadBilImpErrs   B
     dloadBilImpErrs   PI
      /free

       // Set subfile indicators
       clrBLSub = *Off;
       dspBLSub = *On;
       dspBLCtl = *On;

      // Read file and load the subfile
       Setll *Loval Pk102;
       Read Pk102;
       Dow Not %Eof(fak1l2);
        clear sflBilImEr;
       // Do not show Deleted (K1PRFL = 'X') and Processed
       // (K1PRFL = 'Y') records on Import Error screen
        If K1prfl <>'X' and K1prfl <>'Y' ;
          if K1PatId <> *Blanks;
            scClaim# = %Trim(%Subst(K1PatId:6:13));
          else;
            scClaim# = *Blanks;
          endif;

         //***** Beg Chg *** 06/09/20 ************************************* ***
         //*** scSS#    = K1pss#;
          scSS# = MaskData('SSN':'K1PSS#':K1pss#);
         //***** End Chg *** 06/09/20 ************************************* ***
          scErr    = K1prfl;

          monitor;
            scDOI = %Date(%Dec(K1doi:8:0):*iso);
          on-error;
            scDOI = xxDefDate;
          endmon;

          scRrnBLIME = rrnfak1p;
          rrnBilImEr = rrnBilImEr + 1;

          If rrnBilImEr = 1;
             scBLRowSl = 'A';
          Else;
             clear scBLRowSl;
          Endif;

          If mode = 'D';
             dspVisb = *Off;
          Else;
             dspVisb = *On;
          Endif;

          If rrnBilImEr >= 9999;
            rrnBilImEr = 1;
            Leave;
          EndIf;
          Write sflBilImEr;

        Endif;
        Read pk102;
       Enddo;

       rrnBilImEr = 1;

      /end-free
     PloadBilImpErrs   E

     //===================================================================
     // Main - Main processing
     //===================================================================
     P Main            B
     d Main            PI
      /Free

       // Process until exit button is pressed.
       Dou ExitNow = *On;

        // Clear Subfile
         ClrSubfiles();

        // Load Bill Import Error screen
         loadBilImpErrs ();

        // Process Bill Import Error screen.
         Exsr ExfmtScreens;

         Select;
          // Do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When btnRefresh = *On;
             btnRefresh =*Off;

         // Other options processing
          Other;
             procOther();
         Endsl;
       Enddo;

      //===================================================================
      // Write Screen Formats;
      //===================================================================
       BegSr ExfmtScreens;

       Select;
       When TabIndex = 0;
          TabHeight = 1050;

          Write rcdTabPan;
          Exfmt RcdBilImEr;
          Read rcdTabPan;
       EndSl;

       Endsr;
      /end-free
     P Main            E

      //===================================================================
      // ProgramStart - Initial Program tasks
      //===================================================================
     P ProgramStart    B
     d ProgramStart    PI
     d processMe                       n
      /Free

       TabIndex = 0;
       processMe = *on;

       Select;
       When mode = 'D';
          dspMode = *On;
          dspVisb = *Off;
       When mode = 'A';
          addMode = *On;
          dspVisb = *On;
       When mode = 'C';
          chgMode= *On;
          dspVisb = *On;
       Endsl;

      // get company of the user
       xxCO# = GetUserInfo(q1user);
       Return;

      /end-free
     p ProgramStart    e

      //===================================================================
      // processtrnsfr - process transfer
      //===================================================================
     p processtrnsfr   b
     d processtrnsfr   pi
      /free

       scClmSts1 = *Blanks;
       scClaim1# = *Blanks;
       scClmntNm = *Blanks;
       scDOI1    = xxDefDate;
       isValid   = *on;


      // populate transfer window fields
       chain scRowRrn sflBilImEr;
       chain scRrnBlImE pk101;

       if K1PatId <> *Blanks;
         scClaim1# = %Trim(%Subst(K1PatId:6:13));
       else;
         scClaim1# = *Blanks;
       endif;

       //***** Beg Chg *** 06/09/20 *********************************  **** ***
       //*** scss1#    = K1pss#;
        scSS1# = MaskData('SSN':'K1PSS#':K1pss#);
       //***** End Chg *** 06/09/20 ************************************* ***

       Monitor;
         scDOI1  = %Date(%Dec(K1DOI:8:0):*iso);
       On-Error;
         scDOI1  = xxDefDate;
       EndMon;

       scClmntNm = %Trim(K1MFSN) + ' ' + %Trim(K1MLSN);

       Select;
         When K1prfl = 'E';
           scClmSts1 = 'This is Not a Valid Claim# to Transfer. ' +
                       ' Please select a Valid Claim#!';
         When K1prfl = 'D';
           scClmSts1 = 'Duplicate Payment';

         When K1prfl = 'Y';
           scClmSts1 = 'This Claim# has already been Transferred';

         When K1prfl = 'X';
           scClmSts1 = 'This is a Deleted Claim#';

         Other;
           scClmSts1 = 'This is a Valid Claim# to Transfer';
         EndSl;

       Dou BtnTrCancl= *on or (BtnSaveTr = *on and isValid = *on);
          inCall   = '2';
          inSub#   = *Zeros;
          inFnd    = *Zeros;
          inFyr    = *Zeros;
          inCase   = *Zeros;
          inSS#    = *Blanks;
          inAcdt   = *Zeros;
          OutRtcd  = *Blanks;

          Exfmt RcdTrnsfr;
          isValid = *on;

          Select;
         // Search for Valid Claim#
          When ScClm#Scrh= *on;
            ClmSearch(inCall:inSub#:inFnd:inFyr:inCase:inSS#:inAcdt:OutRtcd);
            If OutRtcd ='A' Or OutRtcd ='B'  Or
              inFYR = *Zeros Or inCASE =*Zeros;
              isError = *on;
            Else;
              Chain (xxCO#:inSUB#:inFND:inFYR:inCASE) pcm02;
              If %found(fmcml1);
                ScClmntNm = %Trim(CMFSN) + ' ' + %Trim(CMLSN);
                ScClaim1#  = %editc(CMFnd:'X') +  %editc(CMfyr:'X') +
                             %editc(CMcase:'X');
       //***** Beg Chg *** 06/09/20 ************************************* ***
                //*** ScSS1#     = Cmss#;
                scSS1# = MaskData('SSN':'CMSS#':CMss#);
       //***** End Chg *** 06/09/20 ************************************* ***
                ScDoi1     = %Date(CmAcdt:*cymd);
                ScClmSts1 = 'Valid Claim#' ;
              EndIf;
            EndIf;

             // Save and Transfer bill
          When BtnSaveTr = *on;
             Validate();
             If isValid = *on;
                // Update FAK1P with selected claim# details
                Updatek1p();

               // Transfer bill, call program F6502R
                inMember# =K1Mbr#;
                inBatchid =K1Bch#;
                chain scRowRrn sflBilImEr;
                inRrnk1p  =scRrnBlImE;
                CallF6502R(inMember#:inBatchid:inRrnk1p);
             Endif;

         // Cancel button
          When BtnTrCancl= *on;
             BtnTrCancl= *off;
             Leave;

          Other;
             // Do Nothing
          Endsl;
        Enddo;
      /end-free
     P processtrnsfr   E

      //===================================================================
      // procOther - process other options
      //===================================================================
     P procOther       B
     d procOther       PI
      /free

       ReadC sflBilImEr;
       Dow Not %Eof();
         Select;

        // processing for Delete link
         When scDlt = *on;
           ScDlt = *Off;
           Chain scRrnBLIME pk101;
           If %found(fak1p);
             K1prfl = 'X' ;
             K1dlus = Q1user;
             K1dlpg = Q1pgm ;
             K1dltm = %dec(%char(%time():*iso0):6:0);
             K1dldt = %dec(%char(%date():*cymd0):7:0);
             K1dlre = 'PERFORMRX ';
             Update pk101;
           Endif;

        // processing for Transfer link
         When ScTrnsfr= *On;
           ScTrnsfr= *Off;
           clear RcdTrnsfr;
           ##mode = 'C';
           BtnTrCancl =*Off;
           BtnSaveTr  =*Off;
           processTrnsfr();

         EndSl;
         ReadC sflBilImEr;
       EndDo;
      /end-free
     P procOther       E

      //===================================================================
      // Validate - Validate Claim#
      //===================================================================
     p Validate        b
     d Validate        pi
      /free

       isValid  = *on;

      // Validate Claim#
       If ScClaim1#  = *Blanks;
         isValid  = *Off;
       EndIf;

      /end-free
     P Validate        E

      //===================================================================
      // Updatek1p  - Update FAK1P file with Claim#
      //===================================================================
     P Updatek1p       B
     d Updatek1p       PI
     c
      /free

         K1Co#  = CMCo#  ;
         K1Sub# = CMSub# ;
         K1Fnd  = CMFnd  ;
         K1Fyr  = CMFyr  ;
         K1Case = CMCase ;
         //K1Crn  = %editc(CMFnd:'X') + %editc(CMfyr:'X') +
         //         %editc(CMcase:'X');
         K1PatId= '00000' + %editc(CMFnd:'X') + %editc(CMfyr:'X') +
                          %editc(CMcase:'X');
         K1Pss# = CMSs#  ;
         K1Acdt = CMAcdt ;
         K1Mlsn = CMlsn  ;
         K1Mfsn = CMfsn  ;
         K1PRFL = ' '    ;
         K1Prdt = %Dec(%Date():*cymd);
         K1Prtm = %Dec(%Time():*iso);
         K1Prus = Q1user ;

         Update Pk101;

      /end-free
     P Updatek1p       E
       //===================================================================

