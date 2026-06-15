     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2018')
     H DFTACTGRP(*NO) ACTGRP('HDSOPT') BNDDIR('HYR0604')
      **************************************************************************
      * APPLICATION: O/E and Shipping                                          *
      *       TITLE: Display customer/order comments                           *
      *          BY: William Earl (MCA)                                        *
      *        DATE: September 2018                                            *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  06/17/2024  MCA  Show HTS codes and countries of origin.              *
      **************************************************************************

     FHYD0189   CF   E             WORKSTN
     F                                     SFILE(SFL01:wk_Sfl01RRN)
     FHYW0189   IF   E           K DISK
     FHDCCMT    IF   E           K DISK
     FOEOCMT    IF   E           K DISK
     FOEORDT    IF   E           K DISK    USROPN
     FHYR0189PF O    F   80        PRINTER OFLIND(*INOF) USROPN

     D/EJECT
      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D HYR0189         PR                  EXTPGM('HYR0189')
     D  ShipTo#                       7S 0 CONST
     D  ForceF11                      1N   CONST
     D  RtnCode                       2A

      /COPY QPRPSRC,HYR9930

     D QMHSNDPM        PR                  EXTPGM('QMHSNDPM')                   Send program message
     D  out_MsgID                     7A   CONST
     D  out_MsgFile                  20A   CONST
     D  out_MsgData                 512A   CONST
     D  out_MsgDtaLen                10I 0 CONST
     D  out_MsgType                  10A   CONST
     D  out_MsgStack                 10A   CONST
     D  out_MsgStkCtr                10I 0 CONST
     D  in_MsgKey                     4A
     D  io_MsgErrCode                10I 0

     D QMHRMVPM        PR                  EXTPGM('QMHRMVPM')                   Remove program msgs
     D  out_MsgStack                 10A   CONST
     D  out_MsgStkCtr                10I 0 CONST
     D  out_MsgKey                    4A   CONST
     D  out_MsgToRmv                 10A   CONST
     D  io_MsgErrCode                10I 0

     D QCMDEXC         PR                  EXTPGM('QCMDEXC')
     D  Command                     512A   CONST
     D  CommandLen                   15P 5 CONST

      **************************************************************************
      * Define *Entry parameter list.                                          *
      **************************************************************************
     D HYR0189         PI
     D  in_ShipTo#                    7S 0 CONST
     D  in_ForceF11                   1N   CONST
     D  out_RtnCode                   2A

      **************************************************************************
      * Program status data structure.                                         *
      **************************************************************************
     D ps_            SDS
     D  ps_Pgnm                1     10
     D  ps_Job               244    253
     D  ps_User              254    263
     D  ps_Job#              264    269

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_ShipTo#      S              7S 0
     D wk_ForceF11     S              1N
     D wk_RtnCode      S              2A
     D wk_Sfl01RRN     S              5P 0
     D wk_PgmMode      S              1A
     D wk_CMBLTO       S              7S 0
     D wk_CMCNA1       S             26A
     D wk_CMCCLS       S              2A
     D wk_Foreign      S              1N
     D wk_AlwF20       S              1N
     D wk_SysMDY       S              6S 0
     D wk_SysTime      S              6S 0
     D wk_RptCust      S             78A
     D wk_CmtType      S              8A
     D wk_Order#X      S              8A
     D wk_ODITEM       S             15A
     D wk_IMHTSC       S             25A
     D wk_IMCORG       S             50A
     D wk_Cmt          S             60A
     D wk_WOOUTQ       S             10A
     D wk_OutQ         S             10A
     D wk_Drawer       S              1A
     D wk_OvrCmd       S            512A
     D wk_RcdCnt       S              7P 0
     D wk_MsgID        S              7A
     D wk_MsgData      S            512A
     D wk_MsgDataLen   S              3P 0
     D in_MsgKey       S              4A
     D io_MsgErrCode   S             10I 0
     D kf_CXVCF        S              1A   INZ('C')
     D kf_CXNMBR       S              7S 0
     D kf_CXDOCT       S              3A   INZ('PIC')
     D kf_CXHT         S              1A

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D Hdg1            C                   'Customer & Order Header Comments'
     D Hdg2            C                   'Customer Comments'
     D Hdg3            C                   'Order Comments'
     D Hdg4            C                   'HTS Codes & Countries of Origin'
     D F6Txt           C                   'F6=Print'
     D F11Txt          C                   'F11=Continue'
     D F12Txt          C                   'F12=Cancel'
     D F17Txt          C                   'F17=All comments'
     D F18Txt          C                   'F18=Customer comments'
     D F19Txt          C                   'F19=Order comments'
     D F20Txt          C                   'F20=HTS and Ctry of origin'

     C/EJECT
      **************************************************************************
      * Main procedure.                                                        *
      **************************************************************************

     C                   EVAL      wk_ShipTo# = in_ShipTo#
     C                   EVAL      wk_ForceF11 = in_ForceF11
     C                   EVAL      wk_RtnCode = *Zeros
     C                   EVAL      out_RtnCode = *Zeros

     C                   IF        wk_ShipTo# = -1
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF                                                  wk_ShipTo#=-1

     C                   DO

      /Free
       wk_CMBLTO = *Zero;
       wk_CMCNA1 = *Blanks;
       wk_CMCCLS = *Blanks;
       EXEC SQL select CMBLTO, CMCNA1, CMCCLS
                 into :wk_CMBLTO, :wk_CMCNA1, :wk_CMCCLS
                from HDCUST
                where CMCUST=:wk_ShipTo#;
      /End-Free

       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from GUPTDAT
                where TDTABL='CUSTCLASS' and TDKEY1=:wk_CMCCLS and
                      TDFF02='Y';
       IF wk_RcdCnt = *Zero;
         wk_Foreign = *OFF;
         wk_AlwF20 = *OFF;
       ELSE;
         wk_Foreign = *ON;
         wk_AlwF20 = *ON;
         IF not %open(OEORDT);
           OPEN OEORDT;
         ENDIF;                                                              // %open(OEORDT)
       ENDIF;                                                                // wk_RcdCnt=*Zero

     C                   EVAL      wk_PgmMode = '1'                             Cust & order cmts
     C                   EXSR      $FillSfl01

     C                   IF        wk_Sfl01RRN = *Zero  and  wk_Foreign
     C                   EVAL      wk_PgmMode = '4'                             HTS & Ctry of origin
     C                   EXSR      $FillSfl01
     C                   ENDIF                                                  wk_Sfl01RRN=*Zero

     C                   IF        wk_Sfl01RRN = *Zero
     C                   EVAL      wk_RtnCode = '99'
     C                   LEAVE
     C                   ENDIF                                                  wk_Sfl01RRN=*Zero

     C                   IF        wk_ForceF11
     C                   EVAL      C1FKY1 = F6Txt + '   ' + F11Txt + '   ' +
     C                               F12Txt + '   ' + F17Txt
     C                   EVAL      C1FKY2 = F18Txt + '   ' + F19Txt
     C                   ELSE
     C                   EVAL      C1FKY1 = F6Txt + '   ' + F12Txt + '   ' +
     C                               F17Txt + '   ' + F18Txt
     C                   EVAL      C1FKY2 = F19Txt
     C                   ENDIF                                                  wk_ForceF11

     C                   IF        wk_AlwF20
     C                   EVAL      C1FKY2 = %trimr(C1FKY2) + '   ' + F20Txt
     C                   ENDIF                                                  wk_AlwF20

     C                   EVAL      *IN03 = *OFF
     C                   EVAL      *IN12 = *OFF

     C                   EVAL      C1HDG1 = Hdg1
     C                   EVAL      C1HDG1 = center(C1HDG1)
     C                   EVAL      C1CUST = wk_ShipTo#
     C                   EVAL      C1CNA1 = wk_CMCNA1
     C                   EVAL      *IN40 = *ON

     C                   DOW       not *IN03

     C                   EVAL      *IN11 = wk_ForceF11
     C                   EVAL      *IN20 = wk_AlwF20

      *
      * Display Subfile
      *
     C                   WRITE     MSFCTL01

     C                   IF        wk_Sfl01RRN = *Zero
     C                   EVAL      *IN26 = *OFF
     C                   ELSE
     C                   EVAL      *IN26 = *ON
     C                   ENDIF                                                  wk_Sfl01=*Zero

     C                   EVAL      *IN27 = *ON
     C                   EVAL      *IN30 = *ON

     C                   WRITE     SFLCMD01
     C                   EXFMT     SFLCTL01

     C                   EVAL      *IN26 = *OFF
     C                   EVAL      *IN27 = *OFF
     C                   EVAL      *IN40 = *OFF

     C                   EXSR      $RMVPM

      *
      * Exit?
      *
     C                   IF        *IN03
     C                   EVAL      wk_RtnCode = '03'
     C                   LEAVE
     C                   ENDIF                                                  *IN03

      *
      * Cancel?
      *
     C                   IF        *IN12
     C                   EVAL      wk_RtnCode = '12'
     C                   LEAVE
     C                   ENDIF                                                  *IN12

      *
      * Print?
      *
     C                   IF        *IN06
     C                   EXSR      $Print
     C                   ITER
     C                   ENDIF                                                  *IN06

      *
      * Continue?
      *
     C                   IF        *IN11
     C                   LEAVE
     C                   ENDIF                                                  *IN11

      *
      * All comments?
      *
     C                   IF        *IN17
     C                   EVAL      wk_PgmMode = '1'                             Cust & order cmts
     C                   EXSR      $FillSfl01
     C                   EVAL      C1HDG1 = Hdg1
     C                   EVAL      C1HDG1 = center(C1HDG1)
     C                   ITER
     C                   ENDIF                                                  *IN17

      *
      * Customer comments only?
      *
     C                   IF        *IN18
     C                   EVAL      wk_PgmMode = '2'                             Cust cmts only
     C                   EXSR      $FillSfl01
     C                   EVAL      C1HDG1 = Hdg2
     C                   EVAL      C1HDG1 = center(C1HDG1)
     C                   ITER
     C                   ENDIF                                                  *IN18

      *
      * Order comments only?
      *
     C                   IF        *IN19
     C                   EVAL      wk_PgmMode = '3'                             Order cmts only
     C                   EXSR      $FillSfl01
     C                   EVAL      C1HDG1 = Hdg3
     C                   EVAL      C1HDG1 = center(C1HDG1)
     C                   ITER
     C                   ENDIF                                                  *IN19

      *
      * HTS & country of origin?
      *
     C                   IF        *IN20
     C                   EVAL      wk_PgmMode = '4'                             HTS & Ctry of origin
     C                   EXSR      $FillSfl01
     C                   EVAL      C1HDG1 = Hdg4
     C                   EVAL      C1HDG1 = center(C1HDG1)
     C                   ITER
     C                   ENDIF                                                  *IN19

      *
      * Enter Key
      *
     C                   IF        not wk_ForceF11
     C                   LEAVE
     C                   ENDIF                                                  not wk_ForceF11

     C                   EVAL      wk_MsgID = 'GUM0031'
     C                   EVAL      wk_MsgData = 'F11'
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ITER

     C                   ENDDO                                                  not *IN03

     C                   ENDDO

     C                   EVAL      out_RtnCode = wk_RtnCode

     C                   RETURN

     C/EJECT
      **************************************************************************
      * Fill comment subfile.                                                  *
      **************************************************************************
     CSR   $FillSfl01    BEGSR

     C                   EXSR      $ClrSfl01

     C                   IF        wk_PgmMode = '1'  or  wk_PgmMode = '2'
     C                   EVAL      kf_CXNMBR = wk_ShipTo#
     C                   EVAL      kf_CXHT = 'H'
     C     key_CX        SETLL     HDCCMT
     C                   DOU       %eof(HDCCMT)
     C     key_CX        READE     HDCCMT
     C                   IF        %eof(HDCCMT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HDCCMT)
     C                   EVAL      S1CTYP = 'Ship-to'
     C                   EVAL      S1ORD# = *Blanks
     C                   EVAL      S1CMNT = CXCMNT
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1
     C                   WRITE     SFL01
     C                   ENDDO                                                  %eof(HDCCMT)
     C                   IF        wk_CMBLTO <> wk_ShipTo#
     C                   EVAL      kf_CXNMBR = wk_CMBLTO
     C                   EVAL      kf_CXHT = 'H'
     C     key_CX        SETLL     HDCCMT
     C                   DOU       %eof(HDCCMT)
     C     key_CX        READE     HDCCMT
     C                   IF        %eof(HDCCMT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HDCCMT)
     C                   EVAL      S1CTYP = 'Bill-to'
     C                   EVAL      S1ORD# = *Blanks
     C                   EVAL      S1CMNT = CXCMNT
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1
     C                   WRITE     SFL01
     C                   ENDDO                                                  %eof(HDCCMT)
     C                   ENDIF                                                  wk_CMBLTO<>wk_ShipTo
     C                   ENDIF                                                  wk_PgmMode='1' or

     C                   IF        wk_PgmMode = '1'  or  wk_PgmMode = '3'
     C     key_W1        SETLL     HYW0189
     C                   DOU       %eof(HYW0189)
     C     key_W1        READE     HYW0189
     C                   IF        %eof(HYW0189)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYW0189)
     C     key_OC        SETLL     OEOCMT
     C                   DOU       %eof(OEOCMT)
     C     key_OC        READE     OEOCMT
     C                   IF        %eof(OEOCMT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEOCMT)
     C                   IF        OCDOCT <> 'PIC'
     C                   ITER
     C                   ENDIF                                                  OCDOCT<>'PIC'
     C                   IF        OCORL# = *Zero  or  OCORL# = 999
     C                   EVAL      S1CTYP = 'Order'
     C                   ELSE
      /Free
       wk_ODITEM = *Blanks;
       EXEC SQL select ODITEM into :wk_ODITEM
                from OEORDT
                where ODORD#=:OCORD# and ODORL#=:OCORL# and ODBLN#=:OCBLN#;
      /End-Free
     C                   IF        %subst(wk_ODITEM:2:5) = 'WRNTY'
     C                   EVAL      S1CTYP = 'Order'
     C                   ELSE
     C                   IF        wk_ODITEM <> *Blanks
     C                   EVAL      S1CTYP = '#' + wk_ODITEM
     C                   ELSE
     C                   EVAL      S1CTYP = 'LN# ' + %triml(%editc(OCORL#:'Z'))
     C                   ENDIF                                                  wk_ODITEM<>*Blanks
     C                   ENDIF                                                  OCORL#=*Zero or
     C                   ENDIF                                                  %subst(wk_ODITEM:2:5
     C                   EVAL      S1ORD# = %triml(%editc(W1ORD#:'Z'))
     C                   EVAL      S1CMNT = OCCMNT
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1
     C                   WRITE     SFL01
     C                   ENDDO                                                  %eof(OEOCMT)
     C                   ENDDO                                                  %eof(HYW0189)
     C                   ENDIF                                                  wk_PgmMode='1' or

     C                   IF        wk_PgmMode = '1'  or  wk_PgmMode = '2'
     C                   EVAL      kf_CXNMBR = wk_ShipTo#
     C                   EVAL      kf_CXHT = 'T'
     C     key_CX        SETLL     HDCCMT
     C                   DOU       %eof(HDCCMT)
     C     key_CX        READE     HDCCMT
     C                   IF        %eof(HDCCMT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HDCCMT)
     C                   EVAL      S1CTYP = 'Ship-to'
     C                   EVAL      S1ORD# = *Blanks
     C                   EVAL      S1CMNT = CXCMNT
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1
     C                   WRITE     SFL01
     C                   ENDDO                                                  %eof(HDCCMT)
     C                   IF        wk_CMBLTO <> wk_ShipTo#
     C                   EVAL      kf_CXNMBR = wk_CMBLTO
     C                   EVAL      kf_CXHT = 'T'
     C     key_CX        SETLL     HDCCMT
     C                   DOU       %eof(HDCCMT)
     C     key_CX        READE     HDCCMT
     C                   IF        %eof(HDCCMT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HDCCMT)
     C                   EVAL      S1CTYP = 'Bill-to'
     C                   EVAL      S1ORD# = *Blanks
     C                   EVAL      S1CMNT = CXCMNT
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1
     C                   WRITE     SFL01
     C                   ENDDO                                                  %eof(HDCCMT)
     C                   ENDIF                                                  wk_CMBLTO<>wk_ShipTo
     C                   ENDIF                                                  wk_PgmMode='1' or

     C                   IF        wk_PgmMode = '4'                             HTS & Ctry of origin
     C     key_W1        SETLL     HYW0189
     C                   DOU       %eof(HYW0189)
     C     key_W1        READE     HYW0189
     C                   IF        %eof(HYW0189)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYW0189)
     C     key_OD        SETLL     OEORDT
     C                   DOU       %eof(OEORDT)
     C     key_OD        READE     OEORDT
     C                   IF        %eof(OEORDT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEORDT)
     C                   IF        ODBLN# = *Zero
      /Free
       wk_IMHTSC = *Blanks;
       wk_IMCORG = *Blanks;
       EXEC SQL select IMHTSC, IMCORG
                 into :wk_IMHTSC, :wk_IMCORG
                from HDIMSTCP
                where IMITEM=:ODITEM;
      /End-Free
     C                   IF        wk_IMHTSC <> *Blanks  or
     C                               wk_IMCORG <> *Blanks
     C                   IF        wk_IMHTSC = *Blanks
     C                   EVAL      wk_IMHTSC = 'N/A'
     C                   ENDIF                                                  wk_IMHTSC=*Blanks
     C                   IF        wk_IMCORG = *Blanks
     C                   EVAL      wk_IMCORG = 'N/A'
     C                   ENDIF                                                  wk_IMCORG=*Blanks
     C                   EVAL      S1CTYP = '#' + ODITEM
     C                   EVAL      S1ORD# = %triml(%editc(W1ORD#:'Z'))
     C                   EVAL      S1CMNT = 'HTS: ' + %trimr(wk_IMHTSC) +
     C                               '    Ctry of Orig: ' + wk_IMCORG
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1
     C                   WRITE     SFL01
     C                   ENDIF                                                  wk_IMHTSC<>*Blanks
     C                   ENDIF                                                  ODBLN#=*Zero
     C                   ENDDO                                                  %eof(OEORDT)
     C                   ENDDO                                                  %eof(HYW0189)
     C                   ENDIF                                                  wk_PgmMode='4' or

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Clear manual ship available lot subfile.                               *
      **************************************************************************
     CSR   $ClrSfl01     BEGSR

     C                   EVAL      *IN26 = *OFF
     C                   EVAL      *IN27 = *OFF
     C                   EVAL      *IN28 = *ON
     C                   WRITE     SFLCTL01
     C                   EVAL      *IN28 = *OFF
     C                   EVAL      wk_Sfl01RRN = *Zero

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Print comments.                                                        *
      **************************************************************************
     CSR   $Print        BEGSR

      /Free
       wk_WOOUTQ = *Blanks;
       EXEC SQL select WOOUTQ into :wk_WOOUTQ
                from HYPWSOQ
                where WOWSID=:ps_Job and WOOQTY='REPORTS';
       IF wk_WOOUTQ <> *Blanks;
         wk_OutQ = wk_WOOUTQ;
         wk_Drawer = '2';
       ELSE;
         wk_OutQ = '*JOB';
         wk_Drawer = '2';
       ENDIF;                                                                // wk_WOOUTQ<>*Blanks
       wk_OvrCmd = 'OVRPRTF HYR0189PF TOFILE(QSYSPRT) PAGESIZE(66 85) LPI(6) +
         CPI(10) OVRFLW(62) DRAWER(' + wk_Drawer + ') PAGRTT(0) OUTQ('+
         %trimr(wk_OutQ) + ') FORMTYPE(1NRW) HOLD(*NO) SAVE(*YES) +
         SPLFNAME(HYR0189PF) EXPDATE(*DAYS) DAYS(1) OVRSCOPE(*CALLLVL)';
       CALLP(E) QCMDEXC (wk_OvrCmd:%len(%trimr(wk_OvrCmd)));
      /End-Free

     C                   OPEN(E)   HYR0189PF
     C                   IF        %error
     C                   EVAL      wk_MsgID = 'ERR9999'
     C                   EVAL      wk_MsgData =
     C                               'Error opening print file.  Contact I.T.'
     C                   EXSR      $SndPM
     C                   LEAVESR
     C                   ENDIF                                                  %error

     C                   EVAL      wk_SysMDY = %dec(%date:*MDY)
     C                   EVAL      wk_SysTime = %dec(%time:*HMS)
     C                   EVAL      wk_RptCust = 'Ship to: ' +
     C                               %editc(wk_ShipTo#:'X') + ' ' + wk_CMCNA1
     C                   EVAL      *INOF = *ON

     C                   IF        wk_PgmMode = '1'  or  wk_PgmMode = '2'
     C                   EVAL      kf_CXNMBR = wk_ShipTo#
     C                   EVAL      kf_CXHT = 'H'
     C     key_CX        SETLL     HDCCMT
     C                   DOU       %eof(HDCCMT)
     C     key_CX        READE     HDCCMT
     C                   IF        %eof(HDCCMT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HDCCMT)
     C                   EVAL      wk_CmtType = 'Ship-to'
     C                   EVAL      wk_Order#X = *Blanks
     C                   EVAL      wk_Cmt = CXCMNT
     C                   EXCEPT    PRT01
     C                   ENDDO                                                  %eof(HDCCMT)
     C                   IF        wk_CMBLTO <> wk_ShipTo#
     C                   EVAL      kf_CXNMBR = wk_CMBLTO
     C                   EVAL      kf_CXHT = 'H'
     C     key_CX        SETLL     HDCCMT
     C                   DOU       %eof(HDCCMT)
     C     key_CX        READE     HDCCMT
     C                   IF        %eof(HDCCMT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HDCCMT)
     C                   EVAL      wk_CmtType = 'Bill-to'
     C                   EVAL      wk_Order#X = *Blanks
     C                   EVAL      wk_Cmt = CXCMNT
     C                   EXCEPT    PRT01
     C                   ENDDO                                                  %eof(HDCCMT)
     C                   ENDIF                                                  wk_CMBLTO<>wk_ShipTo
     C                   ENDIF                                                  wk_PgmMode='1' or

     C                   IF        wk_PgmMode = '1'  or  wk_PgmMode = '3'
     C     key_W1        SETLL     HYW0189
     C                   DOU       %eof(HYW0189)
     C     key_W1        READE     HYW0189
     C                   IF        %eof(HYW0189)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYW0189)
     C     key_OC        SETLL     OEOCMT
     C                   DOU       %eof(OEOCMT)
     C     key_OC        READE     OEOCMT
     C                   IF        %eof(OEOCMT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEOCMT)
     C                   IF        OCDOCT <> 'PIC'
     C                   ITER
     C                   ENDIF                                                  OCDOCT<>'PIC'
     C                   IF        OCORL# = *Zero  or  OCORL# = 999
     C                   EVAL      wk_CmtType = 'Order'
     C                   ELSE
      /Free
       wk_ODITEM = *Blanks;
       EXEC SQL select ODITEM into :wk_ODITEM
                from OEORDT
                where ODORD#=:OCORD# and ODORL#=:OCORL# and ODBLN#=:OCBLN#;
      /End-Free
     C                   IF        %subst(wk_ODITEM:2:5) = 'WRNTY'
     C                   EVAL      wk_CmtType = 'Order'
     C                   ELSE
     C                   IF        wk_ODITEM <> *Blanks
     C                   EVAL      wk_CmtType = '#' + wk_ODITEM
     C                   ELSE
     C                   EVAL      wk_CmtType = 'LN# ' +
     C                               %triml(%editc(OCORL#:'Z'))
     C                   ENDIF                                                  wk_ODITEM<>*Blanks
     C                   ENDIF                                                  OCORL#=*Zero or
     C                   ENDIF                                                  %subst(wk_ODITEM:2:5
     C                   EVAL      wk_Order#X = %triml(%editc(W1ORD#:'Z'))
     C                   EVAL      wk_Cmt = OCCMNT
     C                   EXCEPT    PRT01
     C                   ENDDO                                                  %eof(OEOCMT)
     C                   ENDDO                                                  %eof(HYW0189)
     C                   ENDIF                                                  wk_PgmMode='1' or

     C                   IF        wk_PgmMode = '1'  or  wk_PgmMode = '2'
     C                   EVAL      kf_CXNMBR = wk_ShipTo#
     C                   EVAL      kf_CXHT = 'T'
     C     key_CX        SETLL     HDCCMT
     C                   DOU       %eof(HDCCMT)
     C     key_CX        READE     HDCCMT
     C                   IF        %eof(HDCCMT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HDCCMT)
     C                   EVAL      wk_CmtType = 'Ship-to'
     C                   EVAL      wk_Order#X = *Blanks
     C                   EVAL      wk_Cmt = CXCMNT
     C                   EXCEPT    PRT01
     C                   ENDDO                                                  %eof(HDCCMT)
     C                   IF        wk_CMBLTO <> wk_ShipTo#
     C                   EVAL      kf_CXNMBR = wk_CMBLTO
     C                   EVAL      kf_CXHT = 'T'
     C     key_CX        SETLL     HDCCMT
     C                   DOU       %eof(HDCCMT)
     C     key_CX        READE     HDCCMT
     C                   IF        %eof(HDCCMT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HDCCMT)
     C                   EVAL      wk_CmtType = 'Bill-to'
     C                   EVAL      wk_Order#X = *Blanks
     C                   EVAL      wk_Cmt = CXCMNT
     C                   EXCEPT    PRT01
     C                   ENDDO                                                  %eof(HDCCMT)
     C                   ENDIF                                                  wk_CMBLTO<>wk_ShipTo
     C                   ENDIF                                                  wk_PgmMode='1' or

     C                   IF        wk_PgmMode = '4'                             HTS & Ctry of origin
     C     key_W1        SETLL     HYW0189
     C                   DOU       %eof(HYW0189)
     C     key_W1        READE     HYW0189
     C                   IF        %eof(HYW0189)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYW0189)
     C     key_OD        SETLL     OEORDT
     C                   DOU       %eof(OEORDT)
     C     key_OD        READE     OEORDT
     C                   IF        %eof(OEORDT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEORDT)
     C                   IF        ODBLN# = *Zero
      /Free
       wk_IMHTSC = *Blanks;
       wk_IMCORG = *Blanks;
       EXEC SQL select IMHTSC, IMCORG
                 into :wk_IMHTSC, :wk_IMCORG
                from HDIMSTCP
                where IMITEM=:ODITEM;
      /End-Free
     C                   IF        wk_IMHTSC <> *Blanks  or
     C                               wk_IMCORG <> *Blanks
     C                   IF        wk_IMHTSC = *Blanks
     C                   EVAL      wk_IMHTSC = 'N/A'
     C                   ENDIF                                                  wk_IMHTSC=*Blanks
     C                   IF        wk_IMCORG = *Blanks
     C                   EVAL      wk_IMCORG = 'N/A'
     C                   ENDIF                                                  wk_IMCORG=*Blanks
     C                   EVAL      wk_CmtType = '#' + ODITEM
     C                   EVAL      wk_Order#X = %triml(%editc(W1ORD#:'Z'))
     C                   EVAL      wk_Cmt = 'HTS: ' + %trimr(wk_IMHTSC) +
     C                               '    Ctry of Orig: ' + wk_IMCORG
     C                   EXCEPT    PRT01
     C                   ENDIF                                                  wk_IMHTSC<>*Blanks
     C                   ENDIF                                                  ODBLN#=*Zero
     C                   ENDDO                                                  %eof(OEORDT)
     C                   ENDDO                                                  %eof(HYW0189)
     C                   ENDIF                                                  wk_PgmMode='4' or

     C                   EXCEPT    EndOfRpt

     C                   CLOSE(E)  HYR0189PF

     C                   EVAL      wk_MsgID = 'MSG0380'
     C                   EVAL      wk_MsgData = %triml(%editc(wk_ShipTo#:'Z'))
     C                   EVAL      %subst(wk_MsgData:8:10) = wk_OutQ
     C                   EXSR      $SndPM

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Send message to program message queue.                                 *
      **************************************************************************
     CSR   $SndPM        BEGSR

     C                   EVAL      wk_MsgDataLen = %len(%trimr(wk_MsgData))

     C                   CALLP(E)  QMHSNDPM (wk_MsgID : 'GUPMSGF   *LIBL     ' :
     C                               wk_MsgData : wk_MsgDataLen : '*INFO' :
     C                               '*' : *Zero : in_MsgKey :
     C                               io_MsgErrCode)

     CSR                 ENDSR

     C/SPACE 3
      **************************************************************************
      * Remove messages from program message queue.                            *
      **************************************************************************
     CSR   $RmvPM        BEGSR

     C                   CALLP(E)  QMHRMVPM ('*' : *Zero : *Blanks : '*ALL' :
     C                               io_MsgErrCode)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Program initialization - first time only.                              *
      **************************************************************************
     CSR   *InzSR        BEGSR

      *
      * Define Key Lists
      *
     C     key_W1        KLIST                                                  HYW0189
     C                   KFLD                    ps_Job#
     C                   KFLD                    wk_ShipTo#

     C     key_CX        KLIST                                                  HDCCMT
     C                   KFLD                    kf_CXVCF
     C                   KFLD                    kf_CXNMBR
     C                   KFLD                    kf_CXDOCT
     C                   KFLD                    kf_CXHT

     C     key_OC        KLIST                                                  OEOCMT
     C                   KFLD                    W1ORD#

     C     key_OD        KLIST                                                  OEORD#
     C                   KFLD                    W1ORD#

      *
      * Initialize Work Fields
      *
     C                   EVAL      S@PGMQ = '*'

     CSR                 ENDSR

     O/EJECT
     OHYR0189PF H    OF                     2 03
     O                       wk_SysMDY            8 '  /  /  '
     O                       wk_SysTime          17 '0 :  :  '
     O                       C1HDG1              60
     O                                           68 'HYR0189'
     O                                           75 'PAGE'
     O                       PAGE          Z     79
     O          H    OF                     2
     O                       wk_RptCust          79
     O          H    OF                     1
     O                                            8 'Comment'
     O          H    OF                     1
     O                                            5 'Type'
     O                                           15 'Order#'
     O                                           26 'Comment'
     O          H    OF                     1
     O                                            8 '-------'
     O                                           16 '-------'
     O                                           45 '--------------------------'
     O                                           71 '--------------------------'
     O                                           79 '--------'

     O          EF           Prt01          1
     O                       wk_CmtType           9                             8A
     O                       wk_Order#X          18                             8A
     O                       wk_Cmt              79                             60A

     O          E            EndOfRpt    2  1
     O                                           51 '***  END OF REPORT  ***'

     O/EJECT
