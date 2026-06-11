     H Copyright('(c) 2025 Sapiens. All Rights Reserved.')
     H dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')
     H/TITLE WTIDCTPRC  - IDC Transaction Process
     H DFTNAME(WTIDCTPRC)
     H/DEFINE   PROFOUNDUI
      *---------------------------------------------------------------*
      *  Created   : 08/02/19                                         *
      *  Programmer: C:180                                            *
      *  Project   : WC2604 - NCCI Indemnity Data Call                *
      *---------------------------------------------------------------*
      *  Description:  IDC transaction process.
      *
      *
      *---------------------------------------------------------------*
      * Revisions:
      *
      * 12/08/25    C:079  Support 150/119782
      *                    For SUNZ report the state claim number
      *
      * 06/06/25    C:344  Support 133/118331
      *                    Include maintenance fields when updating IDC file
      *---------------------------------------------------------------*
      *
     FWMIDCSC   UF   E             DISK    Infds(fdsIDCSC)
     FWMIDCTRL1 UF   E           K DISK    Infds(fdsIDCTR)
     FWXGSL     IF   E           K DISK
     FWXIDCTR   O    E             DISK    USROPN
     F*** Beg Add *** 12/08/25 *********************************************
     FWMCML1    IF   E           K DISK
     F*** End Add *** 12/08/25 *********************************************
      * *Entry
     D*** Beg Add *** 12/08/25 *********************************************
     D GetClientID     PR                  ExtPgm('RTVCLTID')
     D   outCltID                     3
     D*** End Add *** 12/08/25 *********************************************
      *
     D WTIDCTPRC       PR
     D inRRN                          9A   Const

     D WTIDCTPRC       PI
     D inRRN                          9A   Const

      //**** Beg Add **** 06/06/25 *****************************************
     D psds          esds                  ExtName(szq1p)
      //**** End Add **** 06/06/25 *****************************************

     D fdsIDCSC        DS
     D  dbSCrrn              397    400B 0

     D fdsIDCTR        DS
     D  dbTRrrn              397    400B 0

     D TRXDS         E DS                  EXTNAME(WMIDCTR)
     D SBMCTLDS      E DS                  EXTNAME(WMIDCSC)

     D intRRN          S              9  0 INZ
     D qqRRN           S              9  0 INZ
     D strBtchId       S             14A   INZ('00000000000000')
     D xxTrnDate       S              7  0 INZ
     D xxQtrFrmDt      S              7  0 inz
     D xxQtrEndDt      S              7  0 INZ
     D intRecCnt       S             11  0 INZ
     D ClrWrkFile      C                   CONST('CLRPFM FILE(WXIDCTR)')
     D CmdLen          S             15  5 INZ(20)
     D strFilTyp       s              5    inz
     D*** Beg Add *** 12/08/25 *********************************************
     D cltId           s              3
     D*** End Add *** 12/08/25 *********************************************

     D/copy WPRIDC
     D/copy SPRRUNCMD

     D WrtIdcDta       PR
     D qqRecTyp                       1A   Const

     D SndIdcDta       PR                  Extpgm('WTIDCSEND')
     D qqRRN                          9  0 Const


      /Free
       //*** Beg Add *** 12/08/25 *****************************************
       //Retrieve the Client ID
       GetClientID(cltID);
       //*** End Add *** 12/08/25 *****************************************

       //Retrieve Submission Control Record

       intRRN = %int(inRRN);
       qqRRN = intRRN;
       intRecCnt = *zeros;
       chain(n) intrrn WMIDCSC;
       If %found(WMIDCSC);
          If CCrunt = 'O';
             strBtchId = *Zeros;
          Else;
             strBtchId = CCbchId;
          Endif;


       //Process Transactional File

          Setll strBtchId PTR01;
          If %equal(WMIDCTRL1);
             reade strBtchId PTR01;
             Dow not %eof(WMIDCTRL1);
                xxTrnDate = %dec(%char(%date(TRtrnDate:*ISO0):*cymd0):7:0);
                xxQtrFrmDt = GetQtrFrmDt(CCRPTQTRCD:CCRPTYEAR);
                xxQtrEndDt = GetQtrEndDt(CCRPTQTRCD:CCRPTYEAR);
                Select;
                When CCrunt = 'O' and
                   xxTrnDate >= xxQtrFrmDt and xxTrnDate <= xxQtrEndDt;
                   chain (TRco#:TRfnd) pgs01;

                   If %found(WXGSL) and GSiefl = 'I';
                      TRbchId = CCbchId;
                      //**** Beg Add **** 06/06/25 *************************
                      TRuser  = Q1user;
                      TRpgm   = Q1pgm;
                      TRtime  = %int(%char(%time():*ISO0));
                      TRdt    = %dec(%date():*cymd);
                      //**** End Add **** 06/06/25 *************************
                      Update PTR01;
                      intRecCnt = intRecCnt + 1;
                   Endif;

                When CCrunt = 'R' or CCrunt = 'S';
                   intRecCnt = intRecCnt + 1;

                When CCrunt = 'D';
                   TRsts = 'D';
                   //**** Beg Add **** 06/06/25 *************************
                   TRuser  = Q1user;
                   TRpgm   = Q1pgm;
                   TRtime  = %int(%char(%time():*ISO0));
                   TRdt    = %dec(%date():*cymd);
                   //**** End Add **** 06/06/25 *************************
                   Update PTR01;
               Endsl;
             reade strBtchId PTR01;
             Enddo;
          Endif;

          //Original, replacement, Re-send
          Select;
          When CCrunt = 'O' or CCrunt = 'R' or CCrunt = 'S';
             If intRecCnt > *Zero;
                chain intrrn WMIDCSC;
                If %found(WMIDCSC);
                   CCrecTot = %Editc(intRecCnt:'X');
                   Update PCC01;

                   //Write Submission Control Record
                   WrtIdcDta('C');

                   //Write Transactional Records
                   WrtIdcDta('T');

                   //Close Transactional work file
                   If %Open(WXIDCTR);
                      Close WXIDCTR;
                   Endif;

                   //Send IDC data to IFS
                   SndIdcDta(qqRRN);
                Endif;
             Else;
                //Close Transactional work file
                If %Open(WXIDCTR);
                   Close WXIDCTR;
                Endif;
             Endif;

          //Delete Entire File
          When CCrunt = 'D';
             //Write Submission Control Record
             WrtIdcDta('C');

             //Close Transactional work file
             If %Open(WXIDCTR);
                Close WXIDCTR;
             Endif;

             //Send IDC data to IFS
             SndIdcDta(qqRRN);

          Other;
             //Close Transactional work file
             If %Open(WXIDCTR);
                Close WXIDCTR;
             Endif;

          Endsl;


       Endif;
       *inlr = *on;
       //-----------------------------------------------------------------------
       // Initialization
       //-----------------------------------------------------------------------
       Begsr *inzsr;
       If not %Open(WXIDCTR);
          RunCmd(ClrWrkFile:CmdLen);
          Open WXIDCTR;
       Endif;
       Endsr;
      /End-Free
       //-----------------------------------------------------------------------
      *// Write IDC data
       //-----------------------------------------------------------------------
     PWrtIdcDta        B
     DWrtIdcDta        PI
     D qqRecTyp                       1A   Const
      /Free
       Select;
       When qqRecTyp = 'T';                //Transaction record
          Setll CCbchid PTR01;
          If %equal(WMIDCTRL1);
             reade(n) CCbchId PTR01;
             Dow not %eof(WMIDCTRL1);
               //*** Beg Add *** 12/08/25 **********************************
               If cltID = '150';
                  Chain (TRco#:TRfnd:TRFYR:TRcase) pcm01;
                  If %Found(WMCML1) and CMacl# <> *blanks;
                     TRclmId = %Trim(CMacl#);
                  Endif;
               Endif;
               //*** End Add *** 12/08/25 **********************************
               TRdata = TRXDS;
               Write TDATA;
               reade(n) CCbchId PTR01;
             Enddo;
          Endif;
       When qqRecTyp = 'C';                //Control record
          TRdata = SBMCTLDS;
          Write TDATA;
       Endsl;
      /End-Free
     PWrtIdcDta        E
