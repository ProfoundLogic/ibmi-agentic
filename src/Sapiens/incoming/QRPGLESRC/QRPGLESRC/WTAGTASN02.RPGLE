     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')

     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to serve Agency Quote Tracking Detail Box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTAGTASN
     h*  Calls to:
     h*
     h*==============================================================*
     h*  Created   : 06/06/22                                         *
     h*  Programmer: C:300                                            *
     h*  Project   : WC2395- Redesign of Agency Configuration program *
     h*/===============================================================*
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
      * Quote Tracking File
     fwaqtp     if   e             disk    rename(pqt01:pqt0p)
      * Quote Status File
     fwtqsl1    if   e           k disk
      * Chapter File
     fwmcpl     if   e           k disk

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy wPrEmpLr

      * *ENTRY
     DWtAgtAsn02       pr                  ExtPgm('WTAGTASN02')
     D                                9  0
     D                            32767
     D                                1
      *
     DWtAgtAsn02       pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
     d strChap         s             40
     d strXfer         s             40

     d* Load Quote Tracking Detail.
     d LoadQteTrckDtl  pr

      *
      /Free

       Chain inRRN pqt0p;
       LoadQteTrckDtl();

       outJSON = ' ';

       BuildJSON(outJSON :'OutDtlComments1' :%Trim(QTcomm));
       BuildJSON(outJSON :'OutDtlComments2' :%Trim(QTcom2));
       BuildJSON(outJSON :'OutDtlCarrInfo' :%Trim(QTcarr));
       BuildJSON(outJSON :'OutDtlChapter' :%Trim(strChap));
       BuildJSON(outJSON :'OutDtlTransType' :%Trim(strXfer));
       BuildJSON(outJSON :'OutDtlAutoLost' :%Trim(QTauls));
       If QTauls = 'Y';
          BuildJSON(outJSON :'OutDtlAutoLost' :'Yes');
       Endif;


       rtnCode = ' ';
       Return;
      /End-Free

       //===================================================================
       // Load Quote Tracking Details
       //===================================================================
     P LoadQteTrckDtl  B
     D LoadQteTrckDtl  PI
      /free

       // Transfer Type
       strXfer = *blanks;
       If QTxfer <> ' ';
          Chain (QTco#:QTxfer) pqs01;
          If %found(wtqsl1);
             strXfer = %trim(QSdesc);
          Endif;
       Endif;

       // Chapter
       strChap = *blanks;
       If QTchap <> 0;
          Chain (QTco#:QTfnd:QTchap) pcp01;
          If %found(wmcpl);
             strChap = %trim(%char(QTchap)) + ' - ' + %trim(CPdesc);
          Endif;
       Endif;


      /end-free
     P LoadQteTrckDtl  E




