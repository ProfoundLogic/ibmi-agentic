     H Copyright('(c) 2025 Sapiens. All Rights Reserved.')
     H dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')
     H/TITLE WTIDCQPRC  - IDC Quarterly Process
     H DFTNAME(WTIDCQPRC)
     H/DEFINE   PROFOUNDUI
      *---------------------------------------------------------------*
      *  Created   : 08/07/19                                         *
      *  Programmer: C:180                                            *
      *  Project   : WC2604 - NCCI Indemnity Data Call                *
      *---------------------------------------------------------------*
      *  Description:  IDC Quarterly process.
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
      *
      * 01/14/25    C:340  Project WC2995
      *                    Recompile Only;
      *
      *---------------------------------------------------------------*
      *
     FWMIDCSC   UF   E             DISK    Infds(fdsIDCSC)
     FWMIDCQTL6 UF   E           K DISK    Infds(fdsIDCTR) Rename(PQT01:PQT06)
     FWXGSL     IF   E           K DISK
     FWXIDCQT   O    E             DISK    USROPN
     F*** Beg Add *** 12/08/25 *********************************************
     FWMCML1    IF   E           K DISK
     F*** End Add *** 12/08/25 *********************************************
      * *Entry
      *
     D WTIDCQPRC       PR
     D inRRN                          9A   Const

      //**** Beg Add **** 06/06/25 *****************************************
     D psds          esds                  ExtName(szq1p)
      //**** End Add **** 06/06/25 *****************************************

     D WTIDCQPRC       PI
     D inRRN                          9A   Const

     D fdsIDCSC        DS
     D  dbSCrrn              397    400B 0

     D fdsIDCTR        DS
     D  dbTRrrn              397    400B 0

     D QTDS          E DS                  EXTNAME(WMIDCQT)
     D SBMCTLDS      E DS                  EXTNAME(WMIDCSC)

     D intRRN          S              9  0 INZ
     D qqRRN           S              9  0 INZ
     D strBtchId       S             14A   INZ('00000000000000')
     D xxTrnDate       S              7  0 INZ
     D xxQtrFrmDt      S              7  0 inz
     D xxQtrEndDt      S              7  0 INZ
     D intRecCnt       S             11  0 INZ
     D ClrWrkFile      C                   CONST('CLRPFM FILE(WXIDCQT)')
     D CmdLen          S             15  5 INZ(20)
     D strFilTyp       s              5    inz

     D/copy WPRIDC
     D/copy SPRRUNCMD

     D*** Beg Add *** 12/08/25 *********************************************
     D GetClientID     PR                  ExtPgm('RTVCLTID')
     D   outCltID                     3
     D*** End Add *** 12/08/25 *********************************************

     D WrtIdcDta       PR
     D qqRecTyp                       1A   Const

     D SndIdcDta       PR                  Extpgm('WTIDCSEND')
     D qqRRN                          9  0 Const

     D*** Beg Add *** 12/08/25 *********************************************
     D cltId           s              3
     D*** End Add *** 12/08/25 *********************************************

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
          If CCrunt = 'O' or CCrunt = 'R';
             strBtchId = *Zeros;
          Else;
             strBtchId = CCbchId;
          Endif;


       //Process Quarterly File

          Setll strBtchId PQT06;
          If %equal(WMIDCQTL6);
             reade strBtchId PQT06;
             Dow not %eof(WMIDCQTL6);
                xxTrnDate = %dec(%char(%date(QTtrnDate:*ISO0):*cymd0):7:0);
                xxQtrFrmDt = GetQtrFrmDt(CCRPTQTRCD:CCRPTYEAR);
                xxQtrEndDt = GetQtrEndDt(CCRPTQTRCD:CCRPTYEAR);
                Select;
                When (CCrunt = 'O' or CCrunt = 'R') and
                     (xxTrnDate >= xxQtrFrmDt and xxTrnDate <= xxQtrEndDt);
                   chain (QTco#:QTfnd) pgs01;

                   If %found(WXGSL) and GSiefl = 'I';
                      QTbchId = CCbchId;
                      //**** Beg Add **** 06/06/25 *************************
                      QTuser  = Q1user;
                      QTpgm   = Q1pgm;
                      QTtime  = %int(%char(%time():*ISO0));
                      QTdt    = %dec(%date():*cymd);
                      //**** End Add **** 06/06/25 *************************
                      Update PQT06;
                      intRecCnt = intRecCnt + 1;
                   Endif;

                When CCrunt = 'S';
                   intRecCnt = intRecCnt + 1;

                When CCrunt = 'D';
                   QTsts = 'D';
                   //**** Beg Add **** 06/06/25 *************************
                   QTuser  = Q1user;
                   QTpgm   = Q1pgm;
                   QTtime  = %int(%char(%time():*ISO0));
                   QTdt    = %dec(%date():*cymd);
                   //**** End Add **** 06/06/25 *************************
                   Update PQT06;
               Endsl;
             reade strBtchId PQT06;
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

                   //Write Quarterly Records
                   WrtIdcDta('Q');

                   //Close Quarterly work file
                   If %Open(WXIDCQT);
                      Close WXIDCQT;
                   Endif;

                   //Send IDC data to IFS
                   SndIdcDta(qqRRN);
                Endif;
             Else;
                //Close Quarterly work file
                If %Open(WXIDCQT);
                   Close WXIDCQT;
                Endif;
             Endif;

          //Delete Entire File
          When CCrunt = 'D';
             //Write Submission Control Record
             WrtIdcDta('C');

             //Close Quarterly work file
             If %Open(WXIDCQT);
                Close WXIDCQT;
             Endif;

             //Send IDC data to IFS
             SndIdcDta(qqRRN);

          Other;
             //Close Quarterly work file
             If %Open(WXIDCQT);
                Close WXIDCQT;
             Endif;

          Endsl;


       Endif;
       *inlr = *on;
       //-----------------------------------------------------------------------
       // Initialization
       //-----------------------------------------------------------------------
       Begsr *inzsr;
       If not %Open(WXIDCQT);
          RunCmd(ClrWrkFile:CmdLen);
          Open WXIDCQT;
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
       When qqRecTyp = 'Q';                //Quaterly record
          Setll CCbchid PQT06;
          If %equal(WMIDCQTL6);
             reade(n) CCbchId PQT06;
             Dow not %eof(WMIDCQTL6);
               //*** Beg Add *** 12/08/25 **********************************
               If cltID = '150';
                  Chain (QTco#:QTfnd:QTFYR:QTcase) pcm01;
                  If %Found(WMCML1) and CMacl# <> *blanks;
                     QTclmId = %Trim(CMacl#);
                  Endif;
               Endif;
               //*** End Add *** 12/08/25 **********************************
               QTdata = QTDS;
               Write QDATA;
               reade(n) CCbchId PQT06;
             Enddo;
          Endif;
       When qqRecTyp = 'C';                //Control record
          QTdata = SBMCTLDS;
          Write QDATA;
       Endsl;
      /End-Free
     PWrtIdcDta        E
