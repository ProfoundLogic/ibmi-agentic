     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      *****************************************************************
     H/TITLE W9762R - AGENT PREMIUM/LOSS BY STATE REPORT
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
      * 08/26/14 C:298 Project: SY354                                 *
      *                -Implement Routing Page Changes.               *
      *                                                               *
      * 01/28/10  C:249  Project: SY215                               *
      *           C:189  Retrieve brand Name                          *
      *                                                               *
      *   04/08/08  C:190    PROJECT: 132WJR                          *
      *                      Modify the Agent Premium/Loss by State   *
      *                      Report to include processing to accumu-  *
      *                      late separate totals by state and print  *
      *                      these totals by state (for all agents)   *
      *                      just prior to Grand Total.               *
      *                                                               *
      *   09/06/07  C:33  Seq#: WA132/59962                           *
      *                   Correctly process OUTQ.                     *
      *                                                               *
      *   11/06/03  C: 132   PROJECT: WA093   SEQ: 39845              *
      *                      Expand fields for loss ratio, if over    *
      *                      then output all nines in print line      *
      *                                                               *
      *   09/16/03  C: 132   PROJECT: WA093   SEQ: 39036              *
      *                      Add Other to Select on Sort processing   *
      *                                                               *
      *   09/09/03  C: 132   PROJECT: WA093   SEQ: 38380              *
      *                      RSMLRAT field too small, increased size  *
      *                      ATMLRAT field too small, increased size  *
      *                      of all loss ratio fields, moved output   *
      *                      over two positions to allow for expanded *
      *                      fields not to overlay prior field.       *
      *                                                               *
      *   08/12/03  C: 132   PROJECT: WA093   SEQ: 38033              *
      *                      RSYLRAT field too small, increased size  *
      *                                                               *
      *****************************************************************
      *                                                               *
     FSmcol     IF   E           K Disk
     FWmupl     IF   E           K Disk
     FSmorl     IF   E           K Disk
     FWmfnl     IF   E           K Disk
     FWxl2l     IF   E           K Disk
      ***** BEG ADD ***** 04/08/08 ****************************************
     FWxl6l     UF A E           K Disk
      ***** END ADD ***** 04/08/08 ****************************************
     FWfxlp     O    E             Disk
     *
     * Rate Set Code Master File
     FWmrsl     IF   E           K Disk
      *
     FQsysprt   O    F  198        Printer OFLIND(*INOF)
     F                                     USROPN
      /eject
     D First           S              1n
      ***** BEG ADD ***** 04/08/08 ****************************************
     D First1          S              1n
      ***** END ADD ***** 04/08/08 ****************************************
     D  Kyrtcd         S             10
      ***** BEG CHG ***** 08/12/03 ****************************************
     D*****  Rsylrat        S              7  2
     D  Rsylrat        S              9  2
      ***** END CHG ***** 08/12/03 ****************************************
      ***** BEG CHG ***** 09/09/03 ****************************************
     D*****  Atmlrat        S              7  2
     D*****  Atylrat        S              7  2
     D*****  Rsmlrat        S              7  2
     D*****  Stylrat        S              7  2
     D*****  Stmlrat        S              7  2
     D*****  Mkylrat        S              7  2
     D*****  Mkmlrat        S              7  2
     D*****  Lrylrat        S              7  2
     D*****  Lrmlrat        S              7  2
     D*****  Uwylrat        S              7  2
     D*****  Uwmlrat        S              7  2
     D  Atmlrat        S              9  2
     D  Atylrat        S              9  2
     D  Rsmlrat        S              9  2
     D  Stylrat        S              9  2
     D  Stmlrat        S              9  2
     D  Mkylrat        S              9  2
     D  Mkmlrat        S              9  2
     D  Lrylrat        S              9  2
     D  Lrmlrat        S              9  2
     D  Uwylrat        S              9  2
     D  Uwmlrat        S              9  2
      ***** END CHG ***** 09/09/03 ****************************************
     D  Rsrtn          S              7  2
     D  Strtn          S              7  2
     D  Atrtn          S              7  2
     D  Uwrtn          S              7  2
     D  Mkrtn          S              7  2
     D  Lrrtn          S              7  2
      ***** BEG ADD ***** 04/08/08 ****************************************
     D  Rsrtn1         S              7  2
     D  Rsrtn2         S              7  2
     D  S2rtn1         S              7  2
     D  Strtn1         S              7  2
     D  Rsmlrat1       S              9  2
     D  Rsylrat1       S              9  2
     D  Stmlrat1       S              9  2
     D  Stylrat1       S              9  2
     D  S3ylrat        S              9  2
     D  S3mlrat        S              9  2
      ***** END ADD ***** 04/08/08 ****************************************
     D Datusa          S               D   DATFMT(*USA)
      *
     D                 DS
     D  Dsdttm                 1     12  0
     D  Dstime                 1      6  0
     D  Dsdate                 7     12  0
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
      *
     D PSDS          ESDS                  Extname(Szq1p)
      *
     C                   Eval      *Inlr = *Off
      /eject
      *************
      *
      *  Read work file input
      *
     C                   Dou       %Eof(Wxl2l)
     C                   Read      Wxl2l
      ***** BEG ADD ***** 09/16/03 ****************************************
      * If don't have an Agt Number, bypass and read another input record
     C                   If        L2agt# = *zeros
     C                   Iter
     C                   endIF                                                  End L2agt# = 0
      ***** END ADD ***** 09/16/03 ****************************************
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   Select
     C                   When      QQsort = '1'
     C                   Exsr      AG_S_R
     C                   When      QQsort = '2'
     C                   Exsr      UW_AG_S_R
     C                   When      QQsort = '3'
     C                   Exsr      MK_AG_S_R
     C                   Endsl
      *
     C                   Exsr      Add_to_Prt
      *
     C                   If        Qqpfb = 'F' or
     C                             Qqpfb = 'B'
     C                   Exsr      Move_Out
     C                   Endif
      *
     C                   Enddo
      *
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Exsr      Prt_Lr
     C                   Endif
      /Free
       // ***** Beg Add ***** 08/26/14 *****************************************************
       If qqPfb = 'P' or qqPfb = 'B';
          Except Route;
       Endif;
       // ***** End Add ***** 08/26/14 *****************************************************
      /End-free
      *
     C                   Eval      *Inlr = *On
      *
      /eject
      ********************************************************************
      * Process by Agent, State, Rate code
      ********************************************************************
      *
     C     AG_S_R        Begsr
      *
     C                   If        First
     C                   Eval      Svuwtr = L2uwtr
     C                   Eval      Svmrep = L2mrep
     C                   Eval      Svsort = L2sort
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svst   = L2st
     C                   Eval      Svrtcd = L2rtcd
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svco#  = L2co#
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svsort = L2sort
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svuwnm = L2uwnm
     C                   Eval      Svmrnm = L2mrnm
      *
     C                   Eval      Kyrtcd = L2rtcd
     C     Wmrsl_Key     Chain     Wmrsl
      *
     C                   If        %Found
     C                   Eval      Prtrtcd = Rsdesc
     C                   Endif
      *
     C                   Eval      First = *Off
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Except    Head
     C                   Endif
     C                   Endif
      *
     C                   Select
      *
     C                   When      L2sort <> Svsort
     C                   Exsr      Prt_Rs
     C                   Exsr      Prt_St
     C                   Exsr      Prt_Ag
     C                   Eval      Svsort = L2sort
     C                   Eval      Svst   = L2st
     C                   Eval      Svrtcd = L2rtcd
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svco#  = L2co#
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svsort = L2sort
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svuwnm = L2uwnm
     C                   Eval      Svmrnm = L2mrnm
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Except    Head
     C                   Endif
      *
     C                   When      L2st   <> Svst
     C                   Exsr      Prt_Rs
     C                   Exsr      Prt_St
     C                   Eval      Svst   = L2st
     C                   Eval      Svrtcd = L2rtcd
     C                   Eval      Svco#  = L2co#
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svsort = L2sort
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svuwnm = L2uwnm
     C                   Eval      Svmrnm = L2mrnm
      *
     C                   When      L2rtcd <> Svrtcd
     C                   Exsr      Prt_Rs
     C                   Eval      Svrtcd = L2rtcd
     C                   Other
     C                   Endsl
      *
     C                   Endsr
      *
      /eject
      ********************************************************************
      * Process by Underwriter, Agent, State, Rate code
      ********************************************************************
      *
     C     UW_AG_S_R     Begsr
      *
     C                   If        First
     C                   Eval      Svuwtr = L2uwtr
     C                   Eval      Svmrep = L2mrep
     C                   Eval      Svsort = L2sort
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svst   = L2st
     C                   Eval      Svrtcd = L2rtcd
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svco#  = L2co#
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svsort = L2sort
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svuwnm = L2uwnm
     C                   Eval      Svmrnm = L2mrnm
      *
     C                   Eval      Kyrtcd = L2rtcd
     C     Wmrsl_Key     Chain     Wmrsl
      *
     C                   If        %Found
     C                   Eval      Prtrtcd = Rsdesc
     C                   Endif
      *
     C                   Eval      First = *Off
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Except    Head
     C                   Endif
     C                   Endif
      *
     C                   Select
      *
     C                   When      L2uwtr <> Svuwtr
     C                   Exsr      Prt_Rs
     C                   Exsr      Prt_St
     C                   Exsr      Prt_Ag
     C                   Exsr      Prt_Uw
     C                   Eval      Svuwtr = L2uwtr
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svst   = L2st
     C                   Eval      Svrtcd = L2rtcd
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svco#  = L2co#
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svsort = L2sort
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svuwnm = L2uwnm
     C                   Eval      Svmrnm = L2mrnm
      *
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Except    Head
     C                   Endif
      *
     C                   When      L2srt2 <> Svsrt2
     C                   Exsr      Prt_Rs
     C                   Exsr      Prt_St
     C                   Exsr      Prt_Ag
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svst   = L2st
     C                   Eval      Svrtcd = L2rtcd
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svco#  = L2co#
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svsort = L2sort
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svuwnm = L2uwnm
     C                   Eval      Svmrnm = L2mrnm
      *
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Except    Head
     C                   Endif
      *
     C                   When      L2st   <> Svst
     C                   Exsr      Prt_Rs
     C                   Exsr      Prt_St
     C                   Eval      Svst   = L2st
     C                   Eval      Svrtcd = L2rtcd
     C                   Eval      Svco#  = L2co#
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svsort = L2sort
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svuwnm = L2uwnm
     C                   Eval      Svmrnm = L2mrnm
      *
     C                   When      L2rtcd <> Svrtcd
     C                   Exsr      Prt_Rs
     C                   Eval      Svrtcd = L2rtcd
     C                   Eval      Svco#  = L2co#
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svsort = L2sort
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svuwnm = L2uwnm
     C                   Eval      Svmrnm = L2mrnm
     C                   Other
     C                   Endsl
      *
     C                   Endsr
      *
      /eject
      ********************************************************************
      * Process by Marketing rep, Agent, State, Rate code
      ********************************************************************
      *
     C     MK_AG_S_R     Begsr
      *
     C                   If        First
     C                   Eval      Svuwtr = L2uwtr
     C                   Eval      Svmrep = L2mrep
     C                   Eval      Svsort = L2sort
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svst   = L2st
     C                   Eval      Svrtcd = L2rtcd
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svco#  = L2co#
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svsort = L2sort
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svuwnm = L2uwnm
     C                   Eval      Svmrnm = L2mrnm
     C                   Eval      Svco#  = L2co#
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svsort = L2sort
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svuwnm = L2uwnm
     C                   Eval      Svmrnm = L2mrnm
      *
     C                   Eval      Kyrtcd = L2rtcd
     C     Wmrsl_Key     Chain     Wmrsl
      *
     C                   If        %Found
     C                   Eval      Prtrtcd = Rsdesc
     C                   Endif
      *
     C                   Eval      First = *Off
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Except    Head
     C                   Endif
     C                   Endif
      *
     C                   Select
      *
     C                   When      L2mrep <> Svmrep
     C                   Exsr      Prt_Rs
     C                   Exsr      Prt_St
     C                   Exsr      Prt_Ag
     C                   Exsr      Prt_Mk
     C                   Eval      Svmrep = L2mrep
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svst   = L2st
     C                   Eval      Svrtcd = L2rtcd
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svco#  = L2co#
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svsort = L2sort
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svuwtr = L2uwtr
     C                   Eval      Svuwnm = L2uwnm
     C                   Eval      Svmrnm = L2mrnm
      *
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Except    Head
     C                   Endif
      *
     C                   When      L2srt2 <> Svsrt2
     C                   Exsr      Prt_Rs
     C                   Exsr      Prt_St
     C                   Exsr      Prt_Ag
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svst   = L2st
     C                   Eval      Svrtcd = L2rtcd
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svco#  = L2co#
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svsort = L2sort
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svuwnm = L2uwnm
     C                   Eval      Svuwtr = L2uwtr
     C                   Eval      Svmrnm = L2mrnm
      *
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Except    Head
     C                   Endif
      *
     C                   When      L2st   <> Svst
     C                   Exsr      Prt_Rs
     C                   Exsr      Prt_St
     C                   Eval      Svst   = L2st
     C                   Eval      Svrtcd = L2rtcd
     C                   Eval      Svco#  = L2co#
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svsort = L2sort
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svuwnm = L2uwnm
     C                   Eval      Svuwtr = L2uwtr
     C                   Eval      Svmrnm = L2mrnm
      *
     C                   When      L2rtcd <> Svrtcd
     C                   Exsr      Prt_Rs
     C                   Eval      Svrtcd = L2rtcd
     C                   Eval      Svco#  = L2co#
     C                   Eval      Svagt# = L2agt#
     C                   Eval      Svsort = L2sort
     C                   Eval      Svsrt2 = L2srt2
     C                   Eval      Svagnm = L2agnm
     C                   Eval      Svuwnm = L2uwnm
     C                   Eval      Svuwtr = L2uwtr
     C                   Eval      Svmrnm = L2mrnm
      *
     C                   Other
     C                   Endsl
      *
     C                   Endsr
      *
      /eject
      ********************************************************************
      * Move detail fields to file WFMRP
      ********************************************************************
      *
     C     Move_Out      Begsr
      *
     C                   Eval      XLCO#  = SVCO#
     C                   Eval      XLCONM = CONAME
     C                   Eval      XLAGT# = SVAGT#
     C                   Eval      XLSORT = SVSORT
     C                   Eval      XLSRT2 = SVSRT2
     C                   Eval      XLAGNM = SVAGNM
     C                   Eval      XLUWTR = SVUWTR
     C                   Eval      XLUWNM = SVUWNM
     C                   Eval      XLMREP = SVMREP
     C                   Eval      XLMRNM = SVMRNM
     C                   Eval      XLST   = SVST
     C                   Eval      XLRTCD = SVRTCD
      *
     C                   Write     Pxl01
     C                   Clear                   Pxl01
      *
     C                   Endsr
      /eject
      *****************************************************************
      *  Print Totals for Rate Code                                   *
      *****************************************************************
      *
     C     Prt_Rs        Begsr
      *
     C                   If        Rsmdea <> *zero
      ***** BEG CHG ***** 11/06/03 *********************************************
      * If Loss ratio too big for field load with all nines
      *
     C                   EVAL      XXLRATO = (Rsmdin/Rsmdea) * 100
     C                   If        XXLRATO > 9999999.00 OR XXLRATO < -9999999.00
     C                   EVAL      Rsmlrat = 9999999.99
     C                   ELSE
     C                   Eval      Rsmlrat = (Rsmdin/Rsmdea) * 100
     C                   Endif
     C**************     Eval      Rsmlrat = (Rsmdin/Rsmdea) * 100
      ***** END CHG ***** 11/06/03 *********************************************
     C                   Else
     C                   Eval      Rsmlrat = *Zero
     C                   Endif
      *
     C                   If        Rsydea <> *zero
      ***** BEG CHG ***** 11/06/03 *********************************************
      * If Loss ratio too big for field load with all nines
      *
     C                   EVAL      XXLRATO = (Rsydin/Rsydea) * 100
     C                   If        XXLRATO > 9999999.00 OR XXLRATO < -9999999.00
     C                   EVAL      Rsylrat = 9999999.99
     C                   ELSE
     C                   Eval      Rsylrat = (Rsydin/Rsydea) * 100
     C                   Endif
     C**************     Eval      Rsylrat = (Rsydin/Rsydea) * 100
      ***** END CHG ***** 11/06/03 *********************************************
     C                   Else
     C                   Eval      Rsylrat = *Zero
     C                   Endif
      *
     C                   If        (Rsrnew - Rscxrw) <> *zero
     C                   Eval      Rsrtn  = (Rsrnew - Rscxrw) / Rsrnew
     C                   Else
     C                   Eval      Rsrtn  = *Zero
     C                   Endif
      *
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
      ***** BEG ADD ***** 04/08/08 *********************************************
     C                   Exsr      BLD_SS
      ***** END ADD ***** 04/08/08 *********************************************
     C                   Except    Totrs
      *
     C                   If        *Inof = *On
     C                   Except    Head
     C                   Endif
     C                   Endif
      *
     C                   Clear                   RSMDWR
     C                   Clear                   RSMDEA
     C                   Clear                   RSMDIN
     C                   Clear                   RSMDLR
     C                   Clear                   RSYDWR
     C                   Clear                   RSYDEA
     C                   Clear                   RSYDIN
     C                   Clear                   RSYDLR
      ***** BEG ADD ***** 04/08/08 *********************************************
     C                   Clear                   RSRNEW1
     C                   Clear                   RSCXRW1
      ***** END ADD ***** 04/08/08 *********************************************
      *
     C                   Eval      Kyrtcd = L2rtcd
     C     Wmrsl_Key     Chain     Wmrsl
      *
     C                   If        %Found
     C                   Eval      Prtrtcd = Rsdesc
     C                   Endif
      *
     C                   Endsr
      *
      /eject
      *****************************************************************
      *  Print Totals for State                                       *
      *****************************************************************
      *
     C     Prt_St        Begsr
      *
     C                   If        Stmdea <> *Zero
      ***** BEG CHG ***** 11/06/03 *********************************************
      * If Loss ratio too big for field load with all nines
     C                   EVAL      XXLRATO = (Stmdin/Stmdea) * 100
     C                   If        XXLRATO > 9999999.00 OR XXLRATO < -9999999.00
     C                   EVAL      Stmlrat = 9999999.99
     C                   ELSE
     C                   Eval      Stmlrat = (Stmdin/Stmdea) * 100
     C                   Endif
     C************       Eval      Stmlrat = (Stmdin/Stmdea) * 100
      ***** END CHG ***** 11/06/03 *********************************************
     C                   Else
     C                   Eval      Stmlrat = *Zero
     C                   Endif
      *
     C                   If        Stydea <> *Zero
      ***** BEG CHG ***** 11/06/03 *********************************************
      * If Loss ratio too big for field load with all nines
     C                   EVAL      XXLRATO = (Stydin/Stydea) * 100
     C                   If        XXLRATO > 9999999.00 OR XXLRATO < -9999999.00
     C                   EVAL      Stylrat = 9999999.99
     C                   ELSE
     C                   Eval      Stylrat = (Stydin/Stydea) * 100
     C                   Endif
     C************       Eval      Stylrat = (Stydin/Stydea) * 100
      ***** END CHG ***** 11/06/03 *********************************************
     C                   Else
     C                   Eval      Stylrat = *Zero
     C                   Endif
      *
     C                   If        (Strnew - Stcxrw) <> *Zero
     C                   Eval      Strtn  = (Strnew - Stcxrw) / Strnew
     C                   Else
     C                   Eval      Strtn  = *Zero
     C                   Endif
      *
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Except    Totst
      *
     C                   If        *Inof = *On
     C                   Except    Head
     C                   Endif
     C                   Endif
      *
     C                   Clear                   STMDWR
     C                   Clear                   STMDEA
     C                   Clear                   STMDIN
     C                   Clear                   STMDLR
     C                   Clear                   STYDWR
     C                   Clear                   STYDEA
     C                   Clear                   STYDIN
     C                   Clear                   STYDLR
      *
     C                   Endsr
      *
      /eject
      *****************************************************************
      *  Print Totals for Agent                                       *
      *****************************************************************
      *
     C     Prt_Ag        Begsr
      *
     C                   If        Atmdea <> *zero
      ***** BEG CHG ***** 11/06/03 *********************************************
      * If Loss ratio too big for field load with all nines
     C                   EVAL      XXLRATO = (Atmdin/Atmdea) * 100
     C                   If        XXLRATO > 9999999.00 OR XXLRATO < -9999999.00
     C                   EVAL      Atmlrat = 9999999.99
     C                   ELSE
     C                   Eval      Atmlrat = (Atmdin/Atmdea) * 100
     C                   Endif
     C************       Eval      Atmlrat = (Atmdin/Atmdea) * 100
      ***** END CHG ***** 11/06/03 *********************************************
     C                   Else
     C                   Eval      Atmlrat = *Zero
     C                   Endif
      *
     C                   If        Atydea <> *Zero
      ***** BEG CHG ***** 11/06/03 *********************************************
      * If Loss ratio too big for field load with all nines
     C                   EVAL      XXLRATO = (Atydin/Atydea) * 100
     C                   If        XXLRATO > 9999999.00 OR XXLRATO < -9999999.00
     C                   EVAL      Atylrat = 9999999.99
     C                   ELSE
     C                   Eval      Atylrat = (Atydin/Atydea) * 100
     C                   Endif
     C************       Eval      Atylrat = (Atydin/Atydea) * 100
      ***** END CHG ***** 11/06/03 *********************************************
     C                   Else
     C                   Eval      Atylrat = *Zero
     C                   Endif
      *
     C                   If        (Atrnew - Atcxrw) <> *Zero
     C                   Eval      Atrtn  = (Atrnew - Atcxrw) / Atrnew
     C                   Else
     C                   Eval      Atrtn  = *Zero
     C                   Endif
      *
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Except    Totagt
      *
     C                   If        *Inof = *On
     C                   Except    Head
     C                   Endif
     C                   Endif
      *
     C                   Clear                   ATMDWR
     C                   Clear                   ATMDEA
     C                   Clear                   ATMDIN
     C                   Clear                   ATMDLR
     C                   Clear                   ATYDWR
     C                   Clear                   ATYDEA
     C                   Clear                   ATYDIN
     C                   Clear                   ATYDLR
      *
     C                   Endsr
      *
      /eject
      *****************************************************************
      *  Print Totals for Underwriter                                 *
      *****************************************************************
      *
     C     Prt_Uw        Begsr
      *
     C                   If        Uwydea <> *Zero
     C                   Eval      Uwylrat = (Uwydin/Uwydea) * 100
     C                   Else
     C                   Eval      Uwylrat = *Zero
     C                   Endif
      *
     C                   If        (Uwrnew - Uwcxrw) <> *Zero
     C                   Eval      Uwrtn  = (Uwrnew - Uwcxrw) / Uwrnew
     C                   Else
     C                   Eval      Uwrtn  = *Zero
     C                   Endif
      *
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Except    Totuw
      *
     C                   If        *Inof = *On
     C                   Except    Head
     C                   Endif
     C                   Endif
      *
     C                   Clear                   UWMDWR
     C                   Clear                   UWMDEA
     C                   Clear                   UWMDIN
     C                   Clear                   UWMDLR
     C                   Clear                   UWYDWR
     C                   Clear                   UWYDEA
     C                   Clear                   UWYDIN
     C                   Clear                   UWYDLR
      *
     C                   Endsr
      *
      /eject
      *****************************************************************
      *  Print Totals for Marketing Rep                               *
      *****************************************************************
      *
     C     Prt_Mk        Begsr
      *
     C                   If        Mkydea <> *Zero
     C                   Eval      Mkylrat = (Mkydin/Mkydea) * 100
     C                   Else
     C                   Eval      Mkylrat = *Zero
     C                   Endif
      *
     C                   If        (Mkrnew - Mkcxrw) <> *Zero
     C                   Eval      Mkrtn  = (Mkrnew - Mkcxrw) / Mkrnew
     C                   Else
     C                   Eval      Mkrtn  = *Zero
     C                   Endif
      *
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Except    Totmk
      *
     C                   If        *Inof = *On
     C                   Except    Head
     C                   Endif
     C                   Endif
      *
     C                   Clear                   MKMDWR
     C                   Clear                   MKMDEA
     C                   Clear                   MKMDIN
     C                   Clear                   MKMDLR
     C                   Clear                   MKYDWR
     C                   Clear                   MKYDEA
     C                   Clear                   MKYDIN
     C                   Clear                   MKYDLR
      *
     C                   Endsr
      *
      /eject
      *****************************************************************
      *  Print Totals for Last Record                                 *
      *****************************************************************
      *
     C     Prt_Lr        Begsr
      *
     C                   If        Rsydea <> *Zero
     C                   Eval      Rsylrat = (Rsydin/Rsydea) * 100
     C                   Else
     C                   Eval      Rsylrat = *Zero
     C                   Endif
      *
     C                   If        (Rsrnew - Rscxrw) <> *Zero
     C                   Eval      Rsrtn  = (Rsrnew - Rscxrw) / Rsrnew
     C                   Else
     C                   Eval      Rsrtn  = *Zero
     C                   Endif
      *
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
      ***** BEG ADD ***** 04/08/08 *********************************************
     C                   Exsr      BLD_SS
      ***** END ADD ***** 04/08/08 *********************************************
     C                   Except    Totrs
      *
     C                   If        Stydea <> *Zero
     C                   Eval      Stylrat = (Stydin/Stydea) * 100
     C                   Else
     C                   Eval      Stylrat = *Zero
     C                   Endif
      *
     C                   If        (Strnew - Stcxrw) <> *Zero
     C                   Eval      Strtn  = (Strnew - Stcxrw) / Strnew
     C                   Else
     C                   Eval      Strtn  = *Zero
     C                   Endif
      *
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Except    Totst
     C                   If        *Inof = *On
     C                   Except    Head
     C                   Endif
     C                   Endif
     C                   Endif
      *
     C                   If        Atydea <> *Zero
     C                   Eval      Atylrat = (Atydin/Atydea) * 100
     C                   Else
     C                   Eval      Atylrat = *Zero
     C                   Endif
      *
     C                   If        (Atrnew - Atcxrw) <> *Zero
     C                   Eval      Atrtn  = (Atrnew - Atcxrw) / Atrnew
     C                   Else
     C                   Eval      Atrtn  = *Zero
     C                   Endif
      *
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Except    Totagt
     C                   Endif
      *
     C                   If        *Inof = *On
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Except    Head
     C                   Endif
     C                   Endif
      *
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   If        Mkydea <> *Zero
     C                   Eval      Mkylrat = (Mkydin/Mkydea) * 100
     C                   Else
     C                   Eval      Mkylrat = *Zero
     C                   Endif
      *
     C                   If        (Mkrnew - Mkcxrw) <> *Zero
     C                   Eval      Mkrtn  = (Mkrnew - Mkcxrw) / Mkrnew
     C                   Else
     C                   Eval      Mkrtn  = *Zero
     C                   Endif
      *
     C                   If        Uwydea <> *zero
     C                   Eval      Uwylrat = (Uwydin/Uwydea) * 100
     C                   Else
     C                   Eval      Uwylrat = *Zero
     C                   Endif
      *
     C                   If        (Uwrnew - Uwcxrw) <> *Zero
     C                   Eval      Uwrtn  = (Uwrnew - Uwcxrw) / Uwrnew
     C                   Else
     C                   Eval      Uwrtn  = *Zero
     C                   Endif
      *
      ***** BEG CHG ***** 11/06/03 *********************************************
     C                   If        Lrmdea <> *Zero
     C                   Eval      Lrmlrat = (Lrmdin/Lrmdea) * 100
     C                   Else
     C                   Eval      Lrmlrat = *Zero
     C                   Endif
      ***** END CHG ***** 11/06/03 *********************************************
      *
     C                   If        Lrydea <> *Zero
     C                   Eval      Lrylrat = (Lrydin/Lrydea) * 100
     C                   Else
     C                   Eval      Lrylrat = *Zero
     C                   Endif
      *
     C                   If        (Lrrnew - Lrcxrw) <> *Zero
     C                   Eval      Lrrtn  = (Lrrnew - Lrcxrw) / Lrrnew
     C                   Else
     C                   Eval      Lrrtn  = *Zero
     C                   Endif
      *
     C                   Except    Totmk
     C                   Except    Totuw
      ***** BEG ADD ***** 04/08/08 *********************************************
     C                   Exsr      PRT_SS
      ***** END ADD ***** 04/08/08 *********************************************
     C                   Except    Lrtot
     C                   Endif
      *
     C                   Endsr
      *
      /eject
      *****************************************************************
      *  Add to Print Fields                                          *
      *****************************************************************
      *
     C     Add_to_Prt    Begsr
      *
     C                   Eval      XLMDWR = L2MDWR + XLMDWR
     C                   Eval      XLMDEA = L2MDEA + XLMDEA
     C                   Eval      XLMDIN = L2MDIN + XLMDIN
     C                   Eval      XLMDLR = L2MDLR + XLMDLR
     C                   Eval      XLYDWR = L2YDWR + XLYDWR
     C                   Eval      XLYDEA = L2YDEA + XLYDEA
     C                   Eval      XLYDIN = L2YDIN + XLYDIN
     C                   Eval      XLYDLR = L2YDLR + XLYDLR
     C                   Eval      XLRNEW = L2RNEW + XLRNEW
     C                   Eval      XLCXRW = L2CXRW + XLCXRW
      *
     C                   Eval       RSMDWR = L2MDWR + RSMDWR
     C                   Eval       RSMDEA = L2MDEA + RSMDEA
     C                   Eval       RSMDIN = L2MDIN + RSMDIN
     C                   Eval       RSMDLR = L2MDLR + RSMDLR
     C                   Eval       RSYDWR = L2YDWR + RSYDWR
     C                   Eval       RSYDEA = L2YDEA + RSYDEA
     C                   Eval       RSYDIN = L2YDIN + RSYDIN
     C                   Eval       RSYDLR = L2YDLR + RSYDLR
     C                   Eval       RSRNEW = L2RNEW + RSRNEW
     C                   Eval       RSCXRW = L2CXRW + RSCXRW
      ***** BEG ADD ***** 04/08/08 *********************************************
     C                   Eval       RSRNEW1 = L2RNEW + RSRNEW1
     C                   Eval       RSCXRW1 = L2CXRW + RSCXRW1
      ***** END ADD ***** 04/08/08 *********************************************
      *
     C                   Eval       STMDWR = L2MDWR + STMDWR
     C                   Eval       STMDEA = L2MDEA + STMDEA
     C                   Eval       STMDIN = L2MDIN + STMDIN
     C                   Eval       STMDLR = L2MDLR + STMDLR
     C                   Eval       STYDWR = L2YDWR + STYDWR
     C                   Eval       STYDEA = L2YDEA + STYDEA
     C                   Eval       STYDIN = L2YDIN + STYDIN
     C                   Eval       STYDLR = L2YDLR + STYDLR
     C                   Eval       STRNEW = L2RNEW + STRNEW
     C                   Eval       STCXRW = L2CXRW + STCXRW
      *
     C                   Eval       ATMDWR = L2MDWR + ATMDWR
     C                   Eval       ATMDEA = L2MDEA + ATMDEA
     C                   Eval       ATMDIN = L2MDIN + ATMDIN
     C                   Eval       ATMDLR = L2MDLR + ATMDLR
     C                   Eval       ATYDWR = L2YDWR + ATYDWR
     C                   Eval       ATYDEA = L2YDEA + ATYDEA
     C                   Eval       ATYDIN = L2YDIN + ATYDIN
     C                   Eval       ATYDLR = L2YDLR + ATYDLR
     C                   Eval       ATRNEW = L2RNEW + ATRNEW
     C                   Eval       ATCXRW = L2CXRW + ATCXRW
      *
     C                   Eval       UWMDWR = L2MDWR + UWMDWR
     C                   Eval       UWMDEA = L2MDEA + UWMDEA
     C                   Eval       UWMDIN = L2MDIN + UWMDIN
     C                   Eval       UWMDLR = L2MDLR + UWMDLR
     C                   Eval       UWYDWR = L2YDWR + UWYDWR
     C                   Eval       UWYDEA = L2YDEA + UWYDEA
     C                   Eval       UWYDIN = L2YDIN + UWYDIN
     C                   Eval       UWYDLR = L2YDLR + UWYDLR
     C                   Eval       UWRNEW = L2RNEW + UWRNEW
     C                   Eval       UWCXRW = L2CXRW + UWCXRW
      *
     C                   Eval       MKMDWR = L2MDWR + MKMDWR
     C                   Eval       MKMDEA = L2MDEA + MKMDEA
     C                   Eval       MKMDIN = L2MDIN + MKMDIN
     C                   Eval       MKMDLR = L2MDLR + MKMDLR
     C                   Eval       MKYDWR = L2YDWR + MKYDWR
     C                   Eval       MKYDEA = L2YDEA + MKYDEA
     C                   Eval       MKYDIN = L2YDIN + MKYDIN
     C                   Eval       MKYDLR = L2YDLR + MKYDLR
     C                   Eval       MKRNEW = L2RNEW + MKRNEW
     C                   Eval       MKCXRW = L2CXRW + MKCXRW
      *
     C                   Eval       LRMDWR = L2MDWR + LRMDWR
     C                   Eval       LRMDEA = L2MDEA + LRMDEA
     C                   Eval       LRMDIN = L2MDIN + LRMDIN
     C                   Eval       LRMDLR = L2MDLR + LRMDLR
     C                   Eval       LRYDWR = L2YDWR + LRYDWR
     C                   Eval       LRYDEA = L2YDEA + LRYDEA
     C                   Eval       LRYDIN = L2YDIN + LRYDIN
     C                   Eval       LRYDLR = L2YDLR + LRYDLR
     C                   Eval       LRRNEW = L2RNEW + LRRNEW
     C                   Eval       LRCXRW = L2CXRW + LRCXRW
      *
     C                   Endsr
      /eject
      ***** BEG ADD ***** 04/08/08 ****************************************
      *
      *****************************************************************
      *  Build State Summary                                          *
      *****************************************************************
      *
     C     BLD_SS        Begsr
      *
     C     Wxl6l_Key     Chain     Wxl6l
      *
     C                   If        %Found
     C                   EVAL      L6MDWR = L6MDWR + RSMDWR
     C                   EVAL      L6MDEA = L6MDEA + RSMDEA
     C                   EVAL      L6MDIN = L6MDIN + RSMDIN
     C                   EVAL      L6YDWR = L6YDWR + RSYDWR
     C                   EVAL      L6YDEA = L6YDEA + RSYDEA
     C                   EVAL      L6YDIN = L6YDIN + RSYDIN
     C                   Eval      L6RNEW = L6RNEW + RSRNEW1
     C                   Eval      L6CXRW = L6CXRW + RSCXRW1
     C                   UPDATE    PL601                                        Update WXL6P
     C                   ELSE
     C                   Eval       L6ST   = SVST                               Monthly writt prem.
     C                   Eval       L6RTCD = SVRTCD                             Monthly writt prem.
     C                   Eval       L6MDWR = L6MDWR + RSMDWR                    Monthly writt prem.
     C                   Eval       L6MDEA = L6MDEA + RSMDEA                    Monthly earnd prem.
     C                   Eval       L6MDIN = L6MDIN + RSMDIN                    Monthly loss incur.
     C                   Eval       L6YDWR = L6YDWR + RSYDWR                    YTD written premium.
     C                   Eval       L6YDEA = L6YDEA + RSYDEA                    YTD earned premium.
     C                   Eval       L6YDIN = L6YDIN + RSYDIN                    YTD Loss incurred.
     C                   Eval       L6RNEW = L6RNEW + RSRNEW1
     C                   Eval       L6CXRW = L6CXRW + RSCXRW1
     C                   WRITE     PL601                                        Write WXL6P
     C                   Endif
     C                   Clear                   L6MDWR
     C                   Clear                   L6MDEA
     C                   Clear                   L6MDIN
     C                   Clear                   L6YDWR
     C                   Clear                   L6YDEA
     C                   Clear                   L6YDIN
     C                   Clear                   L6RNEW
     C                   Clear                   L6CXRW
     C                   Endsr                                                  BLD_SS
      /eject
      *****************************************************************
      *  Print State Summary Subroutine                               *
      *****************************************************************
      *
     C     PRT_SS        Begsr
      *
      *  Read Summary work file input
      *
      *  Using *LOVAL, SETLL on file WXL6L.
      *
     C                   Eval      First1= *On
     C     *loval        setll     pl601
      *
     C                   Dou       %Eof(Wxl6l)
     C                   Read      Wxl6l
      *
      *  If file has no records, exit subroutine.
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
      *  Read first record jump to new page and exception output HEAD2.
      *
     C                   If        First1
     C                   Eval      First1= *Off
      *
     C                   Eval      Svst   = L6st
     C                   Eval      Svrtcd = L6rtcd
     C                   Eval      S2MDWR = 0
     C                   Eval      S2MDEA = 0
     C                   Eval      S2MDIN = 0
     C                   Eval      S2YDWR = 0
     C                   Eval      S2YDEA = 0
     C                   Eval      S2YDIN = 0
     C                   Eval      S2RNEW = 0
     C                   Eval      S2CXRW = 0
      *
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Except    Head2
     C                   Endif
     C                   Endif                                                  First1
      *
      *  Do exception output to LRSS2.
      *  CHECK FOR STATE CODE BREAK.
      *
     C                   IF        L6st   <> Svst
     C                   If        S2mdea <> *zero
      * If Loss ratio too big for field load with all nines
      *
     C                   EVAL      X@LRATO = (S2mdin/S2mdea) * 100
     C                   If        X@LRATO > 9999999.00 OR X@LRATO < -9999999.00
     C                   EVAL      Stmlrat1 = 9999999.99
     C                   ELSE
     C                   Eval      Stmlrat1 = (S2mdin/S2mdea) * 100
     C                   Endif                                                  X@LRATO
     C                   Else
     C                   Eval      Stmlrat1 = *Zero
     C                   Endif                                                  S2mdea <> *zero
      *
     C                   If        S2ydea <> *zero
      * If Loss ratio too big for field load with all nines
      *
     C                   EVAL      X@LRATO = (S2ydin/S2ydea) * 100
     C                   If        X@LRATO > 9999999.00 OR X@LRATO < -9999999.00
     C                   EVAL      Stylrat1 = 9999999.99
     C                   ELSE
     C                   Eval      Stylrat1 = (S2ydin/S2ydea) * 100
     C                   Endif
     C                   Else
     C                   Eval      Stylrat1 = *Zero
     C                   Endif
      *
     C                   If        (S2rnew - S2cxrw) <> *Zero
     C                   Eval      S2rtn1  = (S2rnew - S2cxrw) / S2rnew
     C                   Else
     C                   Eval      Strtn1  = *Zero
     C                   Endif
      *
     C* Print state total record
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Except    LRSS2
      *
     C                   If        *Inof = *On
     C                   Except    Head2
     C                   Endif                                                  *INOF=*ON
     C                   Endif                                                  QQPFB='P'or 'B'
      *
      *Re-initialize variables
      *
     C                   Eval      Svst   = L6st
     C                   Eval      Svrtcd = L6rtcd
     C                   Eval      S2MDWR = 0
     C                   Eval      S2MDEA = 0
     C                   Eval      S2MDIN = 0
     C                   Eval      S2YDWR = 0
     C                   Eval      S2YDEA = 0
     C                   Eval      S2YDIN = 0
     C                   Eval      S2RNEW = 0
     C                   Eval      S2CXRW = 0
      *
     C                   Endif                                                  L6ST<>SVL6ST
      *
      *  Calculate Loss ratio and retention rate.
      *
     C                   If        L6mdea <> *zero
      * If Loss ratio too big for field load with all nines
      *
     C                   EVAL      X@LRATO = (L6mdin/L6mdea) * 100
     C                   If        X@LRATO > 9999999.00 OR X@LRATO < -9999999.00
     C                   EVAL      Rsmlrat1 = 9999999.99
     C                   ELSE
     C                   Eval      Rsmlrat1 = (L6mdin/L6mdea) * 100
     C                   Endif
     C                   Else
     C                   Eval      Rsmlrat1 = *Zero
     C                   Endif
      *
     C                   If        L6ydea <> *zero
      * If Loss ratio too big for field load with all nines
      *
     C                   EVAL      X@LRATO = (L6ydin/L6ydea) * 100
     C                   If        X@LRATO > 9999999.00 OR X@LRATO < -9999999.00
     C                   EVAL      Rsylrat1 = 9999999.99
     C                   ELSE
     C                   Eval      Rsylrat1 = (L6ydin/L6ydea) * 100
     C                   Endif
     C                   Else
     C                   Eval      Rsylrat1 = *Zero
     C                   Endif
      *
     C                   If        (L6rnew - L6cxrw) <> *zero
     C                   Eval      Rsrtn1  = (L6rnew - L6cxrw) / L6rnew
     C                   Else
     C                   Eval      Rsrtn1  = *Zero
     C                   Endif
      *
      *  Do exception output to LRSS1.
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Except    LRSS1
      *
     C                   If        *Inof = *On
     C                   Except    Head2
     C                   Endif
     C                   Endif
      *
      *  Accumulate for state totals in new fields with prfix S2.
      *
     C                   Eval      S2MDWR = L6MDWR + S2MDWR
     C                   Eval      S2MDEA = L6MDEA + S2MDEA
     C                   Eval      S2MDIN = L6MDIN + S2MDIN
     C                   Eval      S2YDWR = L6YDWR + S2YDWR
     C                   Eval      S2YDEA = L6YDEA + S2YDEA
     C                   Eval      S2YDIN = L6YDIN + S2YDIN
     C                   Eval      S2RNEW = L6RNEW + S2RNEW
     C                   Eval      S2CXRW = L6CXRW + S2CXRW
      *
      *  Accumulate for ALL AGENTS totals in new fields with prfix S3.
      *
     C                   Eval      S3MDWR = L6MDWR + S3MDWR
     C                   Eval      S3MDEA = L6MDEA + S3MDEA
     C                   Eval      S3MDIN = L6MDIN + S3MDIN
     C                   Eval      S3YDWR = L6YDWR + S3YDWR
     C                   Eval      S3YDEA = L6YDEA + S3YDEA
     C                   Eval      S3YDIN = L6YDIN + S3YDIN
     C                   Eval      S3RNEW = L6RNEW + S3RNEW
     C                   Eval      S3CXRW = L6CXRW + S3CXRW
      *
     C                   Enddo                                                  %Eof(Wxl6l)
      *
      * Process for last record
     C                   If        not first1
     C* if atleast one record read
     C                   If        S2mdea <> *zero
      * If Loss ratio too big for field load with all nines
      *
     C                   EVAL      X@LRATO = (S2mdin/S2mdea) * 100
     C                   If        X@LRATO > 9999999.00 OR X@LRATO < -9999999.00
     C                   EVAL      Stmlrat1 = 9999999.99
     C                   ELSE
     C                   Eval      Stmlrat1 = (S2mdin/S2mdea) * 100
     C                   Endif                                                  X@LRATO
     C                   Else
     C                   Eval      Stmlrat1 = *Zero
     C                   Endif                                                  S2mdea <> *zero
      *
     C                   If        S2ydea <> *zero
      * If Loss ratio too big for field load with all nines
      *
     C                   EVAL      X@LRATO = (S2ydin/S2ydea) * 100
     C                   If        X@LRATO > 9999999.00 OR X@LRATO < -9999999.00
     C                   EVAL      Stylrat1 = 9999999.99
     C                   ELSE
     C                   Eval      Stylrat1 = (S2ydin/S2ydea) * 100
     C                   Endif
     C                   Else
     C                   Eval      Stylrat1 = *Zero
     C                   Endif
      *
     C                   If        (S2rnew - S2cxrw) <> *Zero
     C                   Eval      S2rtn1  = (S2rnew - S2cxrw) / S2rnew
     C                   Else
     C                   Eval      Strtn1  = *Zero
     C                   Endif
      *
     C* Print state total record
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Except    LRSS2
      *
     C                   If        *Inof = *On
     C                   Except    Head2
     C                   Endif                                                  *INOF=*ON
     C                   Endif                                                  QQPFB='P'or 'B'
      *
      *  At the end of file, print state totals for last state and the final totals.
      *  Do exception output to LRSS3.
      *
     C                   If        S3ydea <> *Zero
     C                   Eval      S3ylrat = (S3ydin/S3ydea) * 100
     C                   Else
     C                   Eval      S3ylrat = *Zero
     C                   Endif
      *
     C                   If        S3mdea <> *Zero
     C                   Eval      S3mlrat = (S3mdin/S3mdea) * 100
     C                   Else
     C                   Eval      S3mlrat = *Zero
     C                   Endif
      *
     C                   If        (S3rnew - S3cxrw) <> *Zero
     C                   Eval      Rsrtn1  = (S3rnew - S3cxrw) /S3rnew
     C                   Else
     C                   Eval      Rsrtn1  = *Zero
     C                   Endif
      *
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Except    LRSS3
     C                   ENDIF
     C                   Endif                                                  not first1
      *
     C                   Endsr                                                  PRT_SS
      /eject
      ***** END ADD ***** 04/08/08 ****************************************
      *
      *  INITIALIZATION                ** SUBROUTINE **   S R 1 S T   *
      *****************************************************************
      *
     C     *Inzsr        Begsr
      *
      *   KEY LIST FOR THE COMPANY FILE
      *
     C     Smcol_Key     Klist
     C                   Kfld                    Xxco#
      *
      *   KEY LIST FOR THE USER PARAMETER FILE
      *
     C     Wmupl_Key     Klist
     C                   Kfld                    Q1user
      *
      *   KEY LIST FOR THE OVERRIDE FILE
      *
     C     Smorl_Key     Klist
     C                   Kfld                    Q1pgm
     C                   Kfld                    Xxprt             1
      *
      *  FUND FILE
      *
     C     Wmfnl_Key     Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Xxfnd
      *
     C     Wmrsl_Key     Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    L2rtcd
      ***** BEG ADD ***** 04/08/08 ****************************************
      *
      *  Agent Premium/Loss by State - Summary work file
      *
     C     Wxl6l_Key     Klist
     C                   Kfld                    Svst
     C                   Kfld                    Svrtcd
      ***** END ADD ***** 04/08/08 ****************************************
     *
     C     *Like         Define    L2co#         Svco#
     C     *Like         Define    L2sort        Svsort
     C     *Like         Define    L2srt2        Svsrt2
     C     *Like         Define    L2st          Svst
     C     *Like         Define    L2rtcd        Svrtcd
     C     *Like         Define    L2uwtr        Svuwtr
     C     *Like         Define    L2mrep        Svmrep
     C     *Like         Define    L2agt#        Svagt#
     C     *Like         Define    L2agnm        Svagnm
     C     *Like         Define    Rsdesc        Prtrtcd         -13
     C     *Like         Define    L2mrnm        Svmrnm
     C     *Like         Define    L2uwnm        Svuwnm
     *
     C     *Like         Define    L2MDWR        LRMDWR
     C     *Like         Define    L2MDEA        LRMDEA
     C     *Like         Define    L2MDIN        LRMDIN
     C     *Like         Define    L2MDLR        LRMDLR
     C     *Like         Define    L2YDWR        LRYDWR
     C     *Like         Define    L2YDEA        LRYDEA
     C     *Like         Define    L2YDIN        LRYDIN
     C     *Like         Define    L2YDLR        LRYDLR
     C     *Like         Define    L2RNEW        LRRNEW
     C     *Like         Define    L2CXRW        LRCXRW
      *
     C     *Like         Define    L2YDEA        STYDEA
     C     *Like         Define    L2YDIN        STYDIN
     C     *Like         Define    L2RNEW        STRNEW
     C     *Like         Define    L2CXRW        STCXRW
     C     *Like         Define    L2YDEA        ATYDEA
     C     *Like         Define    L2YDIN        ATYDIN
     C     *Like         Define    L2RNEW        ATRNEW
     C     *Like         Define    L2CXRW        ATCXRW
     C     *Like         Define    L2YDEA        MKYDEA
     C     *Like         Define    L2YDIN        MKYDIN
     C     *Like         Define    L2RNEW        MKRNEW
     C     *Like         Define    L2CXRW        MKCXRW
     C     *Like         Define    L2YDEA        LRYDEA
     C     *Like         Define    L2YDIN        LRYDIN
     C     *Like         Define    L2RNEW        LRRNEW
     C     *Like         Define    L2CXRW        LRCXRW
      *
     C     *Like         Define    L2MDWR        RSMDWR
     C     *Like         Define    L2MDEA        RSMDEA
     C     *Like         Define    L2MDIN        RSMDIN
     C     *Like         Define    L2MDLR        RSMDLR
     C     *Like         Define    L2YDWR        RSYDWR
     C     *Like         Define    L2YDEA        RSYDEA
     C     *Like         Define    L2YDIN        RSYDIN
     C     *Like         Define    L2YDLR        RSYDLR
     C     *Like         Define    L2RNEW        RSRNEW
     C     *Like         Define    L2CXRW        RSCXRW
      ***** BEG CHG ***** 11/06/03 ****************************************
     C     *Like         Define    L2MDIN        XXLRATO
      ***** END CHG ***** 11/06/03 ****************************************
      *
     C     *Like         Define    L2MDWR        STMDWR
     C     *Like         Define    L2MDEA        STMDEA
     C     *Like         Define    L2MDIN        STMDIN
     C     *Like         Define    L2MDLR        STMDLR
     C     *Like         Define    L2YDWR        STYDWR
     C     *Like         Define    L2YDEA        STYDEA
     C     *Like         Define    L2YDIN        STYDIN
     C     *Like         Define    L2YDLR        STYDLR
     C     *Like         Define    L2RNEW        STRNEW
     C     *Like         Define    L2CXRW        STCXRW
      *
     C     *Like         Define    L2MDWR        ATMDWR
     C     *Like         Define    L2MDEA        ATMDEA
     C     *Like         Define    L2MDIN        ATMDIN
     C     *Like         Define    L2MDLR        ATMDLR
     C     *Like         Define    L2YDWR        ATYDWR
     C     *Like         Define    L2YDEA        ATYDEA
     C     *Like         Define    L2YDIN        ATYDIN
     C     *Like         Define    L2YDLR        ATYDLR
     C     *Like         Define    L2RNEW        ATRNEW
     C     *Like         Define    L2CXRW        ATCXRW
      *
     C     *Like         Define    L2MDWR        UWMDWR
     C     *Like         Define    L2MDEA        UWMDEA
     C     *Like         Define    L2MDIN        UWMDIN
     C     *Like         Define    L2MDLR        UWMDLR
     C     *Like         Define    L2YDWR        UWYDWR
     C     *Like         Define    L2YDEA        UWYDEA
     C     *Like         Define    L2YDIN        UWYDIN
     C     *Like         Define    L2YDLR        UWYDLR
     C     *Like         Define    L2RNEW        UWRNEW
     C     *Like         Define    L2CXRW        UWCXRW
      *
     C     *Like         Define    L2MDWR        MKMDWR
     C     *Like         Define    L2MDEA        MKMDEA
     C     *Like         Define    L2MDIN        MKMDIN
     C     *Like         Define    L2MDLR        MKMDLR
     C     *Like         Define    L2YDWR        MKYDWR
     C     *Like         Define    L2YDEA        MKYDEA
     C     *Like         Define    L2YDIN        MKYDIN
     C     *Like         Define    L2YDLR        MKYDLR
     C     *Like         Define    L2RNEW        MKRNEW
     C     *Like         Define    L2CXRW        MKCXRW
      ***** BEG ADD ***** 04/08/08 ****************************************
     C     *Like         Define    L2st          Svl6st
     C     *Like         Define    L2MDEA        S2MDEA
     C     *Like         Define    L2MDIN        S2MDIN
     C     *Like         Define    L2MDWR        S2MDWR
     C     *Like         Define    L2YDEA        S2YDEA
     C     *Like         Define    L2YDIN        S2YDIN
     C     *Like         Define    L2YDWR        S2YDWR
     C     *Like         Define    L2MDEA        S3MDEA
     C     *Like         Define    L2MDIN        S3MDIN
     C     *Like         Define    L2MDWR        S3MDWR
     C     *Like         Define    L2YDEA        S3YDEA
     C     *Like         Define    L2YDIN        S3YDIN
     C     *Like         Define    L2YDWR        S3YDWR
     C     *Like         Define    L2MDIN        X@LRATO
     C     *Like         Define    L2CXRW        S2CXRW
     C     *Like         Define    L2RNEW        S2RNEW
     C     *Like         Define    L2CXRW        S3CXRW
     C     *Like         Define    L2RNEW        S3RNEW
     C     *Like         Define    L2RNEW        RSRNEW1
     C     *Like         Define    L2CXRW        RSCXRW1
      ***** END ADD ***** 04/08/08 ****************************************
      *
     C     *Entry        Plist
     C                   Parm                    QQco#             3
     C                   Parm                    QQfnd             3
     C                   Parm                    QQfrdt            8
     C                   Parm                    QQtodt            8
     C                   Parm                    QQasof            6
     C                   Parm                    QQsort            1
      *                         1=Agent/State/Rate set code
      *                         2=Underwriter/Agt/State/Rate set code
      *                         3=Mkt Rep/Agt/State/Rate set code
     C                   Parm                    QQpfb             1
     C                   Parm                    QQque            10
     C                   Parm                    QQmode            1
      *
     C                   Move      QQco#         XXco#             3 0
     C                   Move      QQfnd         XXfnd             3 0
     C                   Move      QQasof        XXasof            6 0
      *
     C                   Select
     C                   When      QQsort = '1'
     C                   Eval      *In41 = *On
     C                   When      QQsort = '2'
     C                   Eval      *In42 = *On
     C                   When      QQsort = '3'
     C                   Eval      *In43 = *On
     C                   Other
     C                   Endsl
      *
     C                   If        Xxfnd <> *Zero
     C                   Eval      *In44 = *On
     C                   Endif
      *
     C                   Time                    Dsdttm
     C                   Eval      First = *On
      *
     C     Wmupl_Key     Chain     Wmupl                              60
     C     Smcol_Key     Chain     Smcol
     C     Wmfnl_Key     Chain     Wmfnl
      *
      * GET THE DISTRIBUTION INFORMATION
      *
     c*** Beg Add ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     c*** End add ***** 01/28/10 **************************************
      *
     C                   Move      '0'           XXPRT
      *
     C     Smorl_Key     Chain     Smorl
     C*
     C***  Begin Add  ***  09/06/07  **************************************
     C                   if        qqMode = 'D'
     C                   eval      OROUTQ = qqQue
     C                   endif
     C***  End   Add  ***  09/06/07  **************************************
      *
      * OPEN PRINT FILE
      *
     C                   If        Qqpfb = 'P' or
     C                             Qqpfb = 'B'
     C                   Open      Qsysprt
      ***** Beg Del ***** 08/26/14 *************************************************
     C**********         Except    Route
      ***** End Del ***** 08/26/14 *************************************************
     C                   Move      *On           *Inof
     C                   Endif
      *
      *
     C                   Endsr
      /eject
     OQSYSPRT   E            Route          1 06
      ***** Beg Del ***** 08/26/14 *************************************************
     O*********                                   8 'XXXXXXXX'
     O*********                                  20 'XXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXX     XX'
     O*********                                  77 'XXXXXX'
     O********* E            Route          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXXX    XX'
     O*********                                  78 'XXXXXXXX'
     O********* E            Route          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XXXX    XX'
     O*********                                  79 'XX      XX'
     O********* E            Route          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            Route          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            Route          1
     O*********                                   8 'XXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  71 'XX'
     O********* E            Route          1
     O*********                                   4 'XXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  79 'XX    XXXX'
     O********* E            Route          1
     O*********                                   5 'XXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX    XXXX'
     O********* E            Route          1
     O*********                                   6 'XX XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX      XX'
     O********* E            Route          1
     O*********                                   7 'XX  XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX    XXXX'
     O*********                                  79 'XX      XX'
     O********* E            Route          1
     O*********                                   8 'XX   XXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  33 'XXXXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX    XXXX'
     O*********                                  78 'XXXXXXXX'
     O********* E            Route          3
     O*********                                   9 'XX    XXX'
     O*********                                  20 'XXXXXX'
     O*********                                  32 'XXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX     XXX'
     O*********                                  77 'XXXXXX'
     O********* E            Route          1
     O*********                                  15 'Route TO:'
     O*********              ORDIS1              60
     O********* E            Route          1
     O*********              ORDIS2              60
     O********* E            Route          1
     O*********              ORDIS3              60
     O********* E            Route          2
     O*********              ORDIS4              60
     O          E            Route          2
      ***** End Del ***** 08/26/14 *************************************************
      ***** Beg Add ***** 08/05/15 ***********************************
     O          E            Route          1
     O                                           15 'Route TO:'
     O                       ORDIS1              60
     O          E            Route          1
     O                       ORDIS2              60
     O          E            Route          1
     O                       ORDIS3              60
     O          E            Route          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ***********************************
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *************************************************
     O********* E            Route          2
     O*********                                  19 'PROGRAM NAME:'
     O*********              ORPNAM              30
     O*********                                  36 'FILE:'
     O*********              ORFIL#              38
     O*********                                  46 'COPIES:'
     O*********              ORCOPY        Z     50
     O*********                                  56 'HOLD:'
     O*********              ORHOLD              61
     O********* E            Route          2
     O*********                                  19 'OUTPUT QUEUE:'
     O*********              OROUTQ              30
     O*********                                  37 'FORMS:'
     O*********              ORFORM              48
     O*********                                  56 'SAVE:'
     O*********              ORSAVE              61
      ***** End Del ***** 08/26/14 *************************************************
      *
      * CHANGED ROUTING PAGE TO COMPLY WITH VERSION 3 STANDARDS.
      *
     O          E            Route       2  2
     O                                           46 'REPORT SELECTIONS'
     O          E            Route          2
     O                                         +  6 'GROUP  . . . . . . . . . :'
     O                       QQfnd             +  1
     O          E            Route          2
     O                                         +  6 'AS OF MONTH/YEAR . . .   :'
     O                       Xxasof            +  1 '  /    '
     O*         E    49      Route          2
     O*                                        +  6 'FILES FOR MONTH REQUESTED '
     O*                                             'DOES NOT EXIST, PROGRAM '
     O*                                             'TERMINATED.'
     O          E            Route          2
     O                                         +  6 'SORT . . . . . . . . . . :'
     O               41                          58 'AGENT/STATE/RATE SET CODE'
     O               42                          58 'UNDERWRITER/AGT/STATE/RAT'
     O               42                             'E SET CODE'
     O               43                          59 'MKT REP/AGT/STATE/RATE SET'
     O               43                             ' CODE'
      *
     O          E            Route          2
     O                                         +  6 'TYPE OF OUTPUT . . . . . :'
     O                       Qqpfb             +  1
      *
     O*** Beg Chg *****  01/28/10  ************************************
     O****      E            Route          1 60
     O***                                        23 '(C) COPYRIGHT PROGRESSI'
     O***                                        46 'VE DATA SOLUTIONS. INC,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     O*** End Chg *****  01/28/10  ************************************
      *
      * MAIN PAGE HEADING
      *
     O          E            Head           1 06
     O                       Dstime                 '  :  :  '
     O                       Coname             115
     O                       Dsdate        Y    198
     O          E            Head           2
     O                       Q1pgm
     O                       Ortitl             119
     O                                          192 'PAGE:'
     O                       Page          Z    198
      *
     O          E    44      HEAD           1
     O                                              'GROUP . . . . . :'
     O                       FNFND             +  1
     O                       FNNAME            +  1
     O          E            HEAD           1
     O                                              'AS OF MONTH/YEAR:'
     O                       Xxasof            +  1 '  /    '
      *
     O          E    42      HEAD           2
     O                                              'UNDERWRITER . . :'
     O                       Svuwtr            +  1
     O                       Svuwnm            +  1
      *
     O          E    43      HEAD           2
     O                                              'MKTG REP  . . . :'
     O                       Svmrep            +  1
     O                       Svmrnm            +  1
      *
     O          E            HEAD           2
     O                                              'AGENT . . . . . :'
     O                       Svagt#        Z   +  1
     O                       Svagnm            +  1
      *
     O          E            HEAD           2
     O                                           27 '--'
     O                                           49 '----------------------'
     O                                              '------ MONTHLY -------'
     O                                              '----------------------'
     O                                              '----'
     O                                          125 '----------------------'
     O                                              '-------- YTD ---------'
     O                                              '----------------------'
     O                                              '-----'
     O          E            HEAD           2
     O                                            2 'ST'
     O                                           13 'RATE CODE'
     O                                           40 'WRITTEN PREMIUM'
     O                                           61 'EARNED PREMIUM'
     O                                           84 'INCRD LOSSES'
     O                                           97 'LOSS RATIO'
     O                                          118 'WRITTEN PREMIUM'
     O                                          139 'EARNED PREMIUM'
     O                                          160 'INCRD LOSSES'
      ***** BEG CHG ***** 08/12/03 ****************************************
     O***********                               167 'LOSS RATIO'
     O                                          174 'LOSS RATIO'
      ***** BEG END ***** 08/12/03 ****************************************
     O                                          188 'RETENTION'
      *
     O          E            TOTRS          1
     O                       Svst                 2
     O                       Svrtcd              10
     O                       RSMDWR        J     40
     O                       RSMDEA        J     61
      ***** BEG CHG ***** 09/09/03 ****************************************
     O************           RSMDIN        J     82
     O************           Rsmlrat       J     92
     O************           RSYDWR        J    113
     O************           RSYDEA        J    134
     O************           RSYDIN        J    155
     O************           Rsrtn         J    180
     O                       RSMDIN        J     84
     O                       Rsmlrat       J     97
     O                       RSYDWR        J    118
     O                       RSYDEA        J    139
     O                       RSYDIN        J    160
      ***** BEG CHG ***** 08/12/03 ****************************************
     O************           Rsylrat       J    165
      ***** BEG del ***** 09/09/03 ****************************************
     O************           Rsylrat       J    167
      ***** BEG del ***** 09/09/03 ****************************************
      ***** END CHG ***** 08/12/03 ****************************************
     O                       Rsylrat       J    174
     O                       Rsrtn         J    187
      ***** END CHG ***** 09/09/03 ****************************************
      *
     O          E            TOTST          2
     O                       Svst                 2
     O                       STMDWR        J     40
     O                       STMDEA        J     61
     O                       STMDIN        J     84
     O                       Stmlrat       J     97
     O                       STYDWR        J    118
     O                       STYDEA        J    139
     O                       STYDIN        J    160
     O                       Stylrat       J    174
     O                       Strtn         J    187
      *
     O          E            TOTAGT         2
     O                                              'TOTALS FOR AGENT:'
     O                       Svagt#        Z  +   1
     O                       Svagnm           +   1
      *
     O          E            TOTAGT         2
     O                       ATMDWR        J     40
     O                       ATMDEA        J     61
      ***** BEG CHG ***** 09/09/03 ****************************************
     O****************       ATMDIN        J     82
     O                       ATMDIN        J     84
      ***** BEG CHG ***** 09/09/03 ****************************************
     O                       Atmlrat       J     97
     O                       ATYDWR        J    118
     O                       ATYDEA        J    139
     O                       ATYDIN        J    160
     O                       Atylrat       J    174
     O                       Atrtn         J    187
      *
     O          E    42      TOTUW          1
     O                                              'TOTAL FOR UNDERWRITER:'
     O                       Svuwtr           +   1
     O                       Svuwnm           +   1
      *
     O          E    42      TOTUW          2
     O                       UWMDWR        J     40
     O                       UWMDEA        J     61
     O                       UWMDIN        J     84
     O                       Uwmlrat       J     97
     O                       UWYDWR        J    118
     O                       UWYDEA        J    139
     O                       UWYDIN        J    160
     O                       Uwylrat       J    174
     O                       Uwrtn         J    187
      *
     O          E    43      TOTMK          1
     O                                              'TOTAL FOR MARKETING REP:'
     O                       Svmrep           +   1
     O                       Svmrnm            +  1
      *
     O          E    43      TOTMK          2
     O                       MKMDWR        J     40
     O                       MKMDEA        J     61
     O                       MKMDIN        J     84
     O                       Mkmlrat       J     97
     O                       MKYDWR        J    118
     O                       MKYDEA        J    139
     O                       MKYDIN        J    160
     O                       Mkylrat       J    174
     O                       Mkrtn         J    187
      *
     O          E            LRTOT          1
     O                                              'GRAND TOTAL'
      *
     O          E            LRTOT          2
     O                       LRMDWR        J     40
     O                       LRMDEA        J     61
     O                       LRMDIN        J     84
     O                       Lrmlrat       J     97
     O                       LRYDWR        J    118
     O                       LRYDEA        J    139
     O                       LRYDIN        J    160
     O                       Lrylrat       J    174
     O                       Lrrtn         J    187
      ***** END CHG ***** 09/09/03 ****************************************
      ***** BEG ADD ***** 04/08/08 ****************************************
      *
      * HEAD2
      *
     O          E            Head2          1 06
     O                       Dstime                 '  :  :  '
     O                       Coname             115
     O                       Dsdate        Y    198
     O          E            Head2          2
     O                       Q1pgm
     O                       Ortitl             119
     O                                          192 'PAGE:'
     O                       Page          Z    198
      *
     O          E    44      HEAD2          1
     O                                              'GROUP . . . . . :'
     O                       FNFND             +  1
     O                       FNNAME            +  1
     O          E            HEAD2          2
     O                                              'AS OF MONTH/YEAR:'
     O                       Xxasof            +  1 '  /    '
      *
     O          E            HEAD2          2
     O                                           27 '--'
     O                                           49 '----------------------'
     O                                              '------ MONTHLY -------'
     O                                              '----------------------'
     O                                              '----'
     O                                          125 '----------------------'
     O                                              '-------- YTD ---------'
     O                                              '----------------------'
     O                                              '-----'
     O          E            HEAD2          2
     O                                            2 'ST'
     O                                           13 'RATE CODE'
     O                                           40 'WRITTEN PREMIUM'
     O                                           61 'EARNED PREMIUM'
     O                                           84 'INCRD LOSSES'
     O                                           97 'LOSS RATIO'
     O                                          118 'WRITTEN PREMIUM'
     O                                          139 'EARNED PREMIUM'
     O                                          160 'INCRD LOSSES'
     O                                          174 'LOSS RATIO'
     O                                          188 'RETENTION'
     O          E            LRSS1          1
     O                       SVST                 2
     O                       L6RTCD              10
     O                       L6MDWR        J     40
     O                       L6MDEA        J     61
     O                       L6MDIN        J     84
     O                       Rsmlrat1      J     97
     O                       L6YDWR        J    118
     O                       L6YDEA        J    139
     O                       L6YDIN        J    160
     O                       Rsylrat1      J    174
     O                       Rsrtn1        J    187
     O*
     O          E            LRSS2          2
     O                       SVST                 2
     O                       S2MDWR        J     40
     O                       S2MDEA        J     61
     O                       S2MDIN        J     84
     O                       Stmlrat1      J     97
     O                       S2YDWR        J    118
     O                       S2YDEA        J    139
     O                       S2YDIN        J    160
     O                       Stylrat1      J    174
     O                       Strtn1        J    187
      *
     O          E            LRSS3          2
     O                                              'TOTALS FOR ALL AGENT:'
      *
     O          E            LRSS3          2
     O                       S3MDWR        J     40
     O                       S3MDEA        J     61
     O                       S3MDIN        J     84
     O                       S3mlrat       J     97
     O                       S3YDWR        J    118
     O                       S3YDEA        J    139
     O                       S3YDIN        J    160
     O                       S3ylrat       J    174
     O                       Rsrtn1        J    187
      ***** END ADD ***** 04/08/08 ****************************************
