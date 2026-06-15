     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2021')
     H DFTACTGRP(*NO) ACTGRP('HDSOPT') BNDDIR('HYR0604')
      **************************************************************************
      * APPLICATION: Shipping                                                  *
      *       TITLE: Display Sales-specified ship-via and comments             *
      *          BY: William Earl (MCA)                                        *
      *        DATE: September 2021                                            *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  09/12/2022  MCA  Show "liftgate service required" when needed.        *
      **************************************************************************

     FHYD0520   CF   E             WORKSTN
     F                                     SFILE(SFL01:wk_Sfl01RRN)
     FHYW0520   IF   E           K DISK
     FHDCCMT    IF   E           K DISK
     FOEOCMT    IF   E           K DISK
     FHYR0520PF O    F   80        PRINTER OFLIND(*INOF) USROPN

     D/EJECT
      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D HYR0520         PR                  EXTPGM('HYR0520')
     D  Caller                        2A   CONST
      ****  01 - Prior to Shipping ShipSelect (HYR0510)
      ****  02 - Normal shipping backend (HYR0110)
      ****  03 - BoxLogix shipping backend (HYR0600)
      ****  04 - Combine (BOL and Parcel) screen (HYR0140)
     D  ShipTo#                       7S 0 CONST
     D  Ord#                          8S 0 DIM(100)
     D  OrdCnt                        3P 0
     D  XtraCmt                       2A   CONST
      ****  01 - Liftgate service required
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
     D HYR0520         PI
     D  in_Caller                     2A   CONST
     D  in_ShipTo#                    7S 0 CONST
     D  in_Ord#                       8S 0 DIM(100)
     D  in_OrdCnt                     3P 0
     D  in_XtraCmt                    2A   CONST
     D  out_RtnCode                   2A

      **************************************************************************
      * Program status data structure.                                         *
      **************************************************************************
     D ps_            SDS
     D  ps_Pgnm                1     10
     D  ps_Job               244    253
     D  ps_User              254    263
     D  ps_Job#              264    269

      *****************************************************************
      * Arrays
      *****************************************************************
     D Ord#            S              8S 0 DIM(100)                             Orders
     D idx1            S              3P 0

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Caller       S              2A
     D wk_ShipTo#      S              7S 0
     D wk_OrdCnt       S              3P 0
     D wk_XtraCmt      S              2A
     D wk_RtnCode      S              2A
     D wk_XtraOrds     S              3P 0
     D wk_Sfl01RRN     S              5P 0
     D wk_AffCnt       S              3P 0
     D wk_CustCmts     S              1N
     D wk_ShpViaSpec   S              1N
     D wk_CMBLTO       S              7S 0
     D wk_CMCNA1       S             26A
     D wk_CMSV         S              2A
     D wk_SysMDY       S              6S 0
     D wk_SysTime      S              6S 0
     D wk_RptCust      S             78A
     D wk_Order#       S              8S 0
     D wk_Order#X      S              8A
     D wk_SVSVSV       S              2A
     D wk_OrdSV        S              3A
     D wk_Cmt          S             60A
     D wk_WOOUTQ       S             10A
     D wk_OutQ         S             10A
     D wk_Drawer       S              1A
     D wk_OvrCmd       S            512A
     D wk_MsgID        S              7A
     D wk_MsgData      S            512A
     D wk_MsgDataLen   S              3P 0
     D in_MsgKey       S              4A
     D io_MsgErrCode   S             10I 0
     D kf_CXVCF        S              1A   INZ('C')
     D kf_CXNMBR       S              7S 0
     D kf_CXDOCT       S              3A   INZ('SV')
     D kf_CXHT         S              1A

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D Hdg1            C                   'ShipSelect Prior To Shipping'
     D Hdg2            C                   'Order Ship'
     D Hdg3            C                   'Complete Shipment'
     D Hdg4            C                   'Combined Shipment'

     C/EJECT
      **************************************************************************
      * Main procedure.                                                        *
      **************************************************************************

     C                   EVAL      wk_Caller = in_Caller
     C                   EVAL      wk_ShipTo# = in_ShipTo#
     C                   EVAL      Ord# = in_Ord#
     C                   EVAL      wk_OrdCnt = in_OrdCnt
     C                   EVAL      wk_XtraCmt = in_XtraCmt
     C                   EVAL      wk_RtnCode = *Zeros
     C                   EVAL      out_RtnCode = *Zeros

     C                   IF        wk_ShipTo# = -1
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF                                                  wk_ShipTo#=-1

     C                   DO

     C                   IF        wk_OrdCnt = 100  and  Ord#(100) < *Zero
     C                   EVAL      wk_XtraOrds = Ord#(100) * -1
     C                   EVAL      Ord#(100) = *Zero
     C                   EVAL      wk_MsgID = 'ERR1384'
     C                   EVAL      wk_MsgData = %triml(%editc(wk_XtraOrds:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ELSE
     C                   EVAL      wk_XtraOrds = *Zero
     C                   ENDIF                                                  wk_OrdCnt=100 and

      /Free
       wk_CMBLTO = *Zero;
       wk_CMCNA1 = *Blanks;
       wk_CMSV = *Blanks;
       EXEC SQL select CMBLTO, CMCNA1, CMSV
                 into :wk_CMBLTO, :wk_CMCNA1, :wk_CMSV
                from HDCUST
                where CMCUST=:wk_ShipTo#;

       EXEC SQL delete from HYW0520 where W1JOB#=:ps_Job#;

       FOR idx1 = 1 to wk_OrdCnt;
         wk_Order# = Ord#(idx1);
         EXEC SQL insert into HYW0520
                         (W1JOB#, W1SHTO, W1ORD#)
                  values(:ps_Job#, :wk_ShipTo#, :wk_Order#);
       ENDFOR;                                                               // idx1=1 to wk_OrdCnt
      /End-Free

     C                   EVAL      wk_ShpViaSpec = *OFF

     C                   EXSR      $FillSfl01

     C                   IF        wk_Sfl01RRN = *Zero
     C                   EVAL      wk_RtnCode = '99'
     C                   LEAVE
     C                   ENDIF                                                  wk_Sfl01RRN=*Zero

     C                   EVAL       C1OINC = %triml(%editc(wk_OrdCnt:'Z'))
     C                   IF         wk_CustCmts
     C                   EVAL       C1OAFF = %triml(%editc(wk_OrdCnt:'Z'))
     C                   ELSE
     C                   EVAL       C1OAFF = %triml(%editc(wk_AffCnt:'Z'))
     C                   ENDIF                                                  wk_CustCmts

     C                   EVAL      *IN03 = *OFF
     C                   EVAL      *IN12 = *OFF

     C                   SELECT
     C                   WHEN      wk_Caller = '01'
     C                   EVAL      C1HDG1 = Hdg1
     C                   WHEN      wk_Caller = '02'
     C                   EVAL      C1HDG1 = Hdg2
     C                   WHEN      wk_Caller = '03'
     C                   EVAL      C1HDG1 = Hdg3
     C                   WHEN      wk_Caller = '04'
     C                   EVAL      C1HDG1 = Hdg4
     C                   OTHER
     C                   EVAL      C1HDG1 = 'Invalid Caller ' + wk_Caller
     C                   ENDSL
     C                   EVAL      C1HDG1 = center(C1HDG1)
     C                   EVAL      C1CUST = wk_ShipTo#
     C                   EVAL      C1CNA1 = wk_CMCNA1
     C                   EVAL      *IN40 = *ON

     C                   DOW       not *IN03

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
      * Enter Key
      *
     C                   EVAL      wk_MsgID = 'GUM0031'
     C                   EVAL      wk_MsgData = 'F11'
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ITER

     C                   ENDDO                                                  not *IN03

     C                   ENDDO

      /Free
       EXEC SQL delete from HYW0520 where W1JOB#=:ps_Job#;
      /End-Free

     C                   EVAL      out_RtnCode = wk_RtnCode

     C                   RETURN

     C/EJECT
      **************************************************************************
      * Fill comment subfile.                                                  *
      **************************************************************************
     CSR   $FillSfl01    BEGSR

     C                   EVAL      wk_CustCmts = *OFF

     C                   EXSR      $ClrSfl01

     C                   IF        wk_XtraCmt = '01'
     C                   EVAL      S1ORD# = *Blanks
     C                   EVAL      S1SHPV = *Blanks
     C                   EVAL      S1CMNT = *Blanks
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1
     C                   WRITE     SFL01
     C                   EVAL      S1CMNT = x'22' + '** NOTE -' + x'20' +
     C                               'LIFTGATE SERVICE REQUIRED.'
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1
     C                   WRITE     SFL01
     C                   EVAL      S1CMNT = *Blanks
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1
     C                   WRITE     SFL01
     C                   ENDIF                                                  wk_XtraCmt='01'

     C                   EVAL      kf_CXNMBR = wk_ShipTo#
     C                   EVAL      kf_CXHT = 'H'
     C     key_CX        SETLL     HDCCMT
     C                   DOU       %eof(HDCCMT)
     C     key_CX        READE     HDCCMT
     C                   IF        %eof(HDCCMT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HDCCMT)
     C                   EVAL      S1ORD# = *Blanks
     C                   EVAL      S1SHPV = *Blanks
     C                   EVAL      S1CMNT = CXCMNT
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1
     C                   WRITE     SFL01
     C                   EVAL      wk_CustCmts = *ON
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
     C                   EVAL      S1ORD# = *Blanks
     C                   EVAL      S1SHPV = *Blanks
     C                   EVAL      S1CMNT = CXCMNT
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1
     C                   WRITE     SFL01
     C                   EVAL      wk_CustCmts = *ON
     C                   ENDDO                                                  %eof(HDCCMT)
     C                   ENDIF                                                  wk_CMBLTO<>wk_ShipTo

     C     key_W1        SETLL     HYW0520
     C                   DOU       %eof(HYW0520)
     C     key_W1        READE     HYW0520
     C                   IF        %eof(HYW0520)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYW0520)
      /Free
       wk_SVSVSV = *Blanks;
       EXEC SQL select SVSVSV into :wk_SVSVSV
                from HYPSVCT
                where SVORD#=:W1ORD# and SVSTS='A';
       IF wk_SVSVSV = *Blanks;
         wk_SVSVSV = wk_CMSV;
       ENDIF;                                                                // wk_SVSVSV=*Blanks
      /End-Free
     C                   EVAL      wk_ShpViaSpec = *OFF
     C     key_OC        SETLL     OEOCMT
     C                   DOU       %eof(OEOCMT)
     C     key_OC        READE     OEOCMT
     C                   IF        %eof(OEOCMT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEOCMT)
     C                   IF        OCDOCT <> 'SV'
     C                   ITER
     C                   ENDIF                                                  OCDOCT<>'SV'
     C                   EVAL      S1ORD# = %triml(%editc(W1ORD#:'Z'))
     C                   IF        wk_SVSVSV = 'BW'
     C                   EVAL      S1SHPV = 'Any'
     C                   ELSE
     C                   EVAL      S1SHPV = wk_SVSVSV
     C                   ENDIF                                                  wk_SVSVSV='BW'
     C                   EVAL      S1CMNT = OCCMNT
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1
     C                   WRITE     SFL01
     C                   EVAL      wk_SVSVSV = *Blanks
     C                   EVAL      wk_ShpViaSpec = *ON
     C                   ENDDO                                                  %eof(OEOCMT)
     C                   IF        not wk_ShpViaSpec
     C                   IF        wk_SVSVSV <> 'BW'
     C                   EVAL      S1ORD# = %triml(%editc(W1ORD#:'Z'))
     C                   EVAL      S1SHPV = wk_SVSVSV
     C                   EVAL      S1CMNT = *Blanks
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1
     C                   WRITE     SFL01
     C                   EVAL      wk_AffCnt = wk_AffCnt + 1
     C                   ENDIF                                                  wk_SVSVSV<>'BW'
     C                   ELSE
     C                   EVAL      wk_AffCnt = wk_AffCnt + 1
     C                   ENDIF                                                  not wk_ShpViaSpec
     C                   ENDDO                                                  %eof(HYW0520)

     C                   EVAL      kf_CXNMBR = wk_ShipTo#
     C                   EVAL      kf_CXHT = 'T'
     C     key_CX        SETLL     HDCCMT
     C                   DOU       %eof(HDCCMT)
     C     key_CX        READE     HDCCMT
     C                   IF        %eof(HDCCMT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HDCCMT)
     C                   EVAL      S1ORD# = *Blanks
     C                   EVAL      S1SHPV = *Blanks
     C                   EVAL      S1CMNT = CXCMNT
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1
     C                   WRITE     SFL01
     C                   EVAL      wk_CustCmts = *ON
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
     C                   EVAL      S1ORD# = *Blanks
     C                   EVAL      S1SHPV = *Blanks
     C                   EVAL      S1CMNT = CXCMNT
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1
     C                   WRITE     SFL01
     C                   EVAL      wk_CustCmts = *ON
     C                   ENDDO                                                  %eof(HDCCMT)
     C                   ENDIF                                                  wk_CMBLTO<>wk_ShipTo

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
       wk_OvrCmd = 'OVRPRTF HYR0520PF TOFILE(QSYSPRT) PAGESIZE(66 85) LPI(6) +
         CPI(10) OVRFLW(62) DRAWER(' + wk_Drawer + ') PAGRTT(0) OUTQ('+
         %trimr(wk_OutQ) + ') FORMTYPE(1NRW) HOLD(*NO) SAVE(*YES) +
         SPLFNAME(HYR0520PF) EXPDATE(*DAYS) DAYS(1) OVRSCOPE(*CALLLVL)';
       CALLP(E) QCMDEXC (wk_OvrCmd:%len(%trimr(wk_OvrCmd)));
      /End-Free

     C                   OPEN(E)   HYR0520PF
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

     C                   EVAL      kf_CXNMBR = wk_ShipTo#
     C                   EVAL      kf_CXHT = 'H'
     C     key_CX        SETLL     HDCCMT
     C                   DOU       %eof(HDCCMT)
     C     key_CX        READE     HDCCMT
     C                   IF        %eof(HDCCMT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HDCCMT)
     C                   EVAL      wk_OrdSV = *Blanks
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
     C****               EVAL      wk_CmtType = 'Bill-to'
     C                   EVAL      wk_Order#X = *Blanks
     C                   EVAL      wk_Cmt = CXCMNT
     C                   EXCEPT    PRT01
     C                   ENDDO                                                  %eof(HDCCMT)
     C                   ENDIF                                                  wk_CMBLTO<>wk_ShipTo

     C     key_W1        SETLL     HYW0520
     C                   DOU       %eof(HYW0520)
     C     key_W1        READE     HYW0520
     C                   IF        %eof(HYW0520)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYW0520)
      /Free
       wk_SVSVSV = *Blanks;
       EXEC SQL select SVSVSV into :wk_SVSVSV
                from HYPSVCT
                where SVORD#=:W1ORD# and SVSTS='A';
       IF wk_SVSVSV = *Blanks;
         wk_SVSVSV = wk_CMSV;
       ENDIF;                                                                // wk_SVSVSV=*Blanks
      /End-Free
     C     key_OC        SETLL     OEOCMT
     C                   DOU       %eof(OEOCMT)
     C     key_OC        READE     OEOCMT
     C                   IF        %eof(OEOCMT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEOCMT)
     C                   IF        OCDOCT <> 'SV'
     C                   ITER
     C                   ENDIF                                                  OCDOCT<>'SV'
     C                   EVAL      wk_Order#X = %triml(%editc(W1ORD#:'Z'))
     C                   IF        wk_SVSVSV = 'BW'
     C                   EVAL      wk_OrdSV = 'Any'
     C                   ELSE
     C                   EVAL      wk_OrdSV = wk_SVSVSV
     C                   ENDIF                                                  wk_SVSVSV='BW'
     C                   EVAL      wk_Cmt = OCCMNT
     C                   EXCEPT    PRT01
     C                   EVAL      wk_SVSVSV = *Blanks
     C                   ENDDO                                                  %eof(OEOCMT)
     C                   ENDDO                                                  %eof(HYW0520)

     C                   EVAL      kf_CXNMBR = wk_ShipTo#
     C                   EVAL      kf_CXHT = 'T'
     C     key_CX        SETLL     HDCCMT
     C                   DOU       %eof(HDCCMT)
     C     key_CX        READE     HDCCMT
     C                   IF        %eof(HDCCMT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HDCCMT)
     C****               EVAL      wk_CmtType = 'Ship-to'
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
     C****               EVAL      wk_CmtType = 'Bill-to'
     C                   EVAL      wk_Order#X = *Blanks
     C                   EVAL      wk_Cmt = CXCMNT
     C                   EXCEPT    PRT01
     C                   ENDDO                                                  %eof(HDCCMT)
     C                   ENDIF                                                  wk_CMBLTO<>wk_ShipTo

     C                   EXCEPT    EndOfRpt

     C                   CLOSE(E)  HYR0520PF

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
     C     key_W1        KLIST                                                  HYW0520
     C                   KFLD                    ps_Job#
     C                   KFLD                    wk_ShipTo#

     C     key_CX        KLIST                                                  HDCCMT
     C                   KFLD                    kf_CXVCF
     C                   KFLD                    kf_CXNMBR
     C                   KFLD                    kf_CXDOCT
     C                   KFLD                    kf_CXHT

     C     key_OC        KLIST                                                  OEOCMT
     C                   KFLD                    W1ORD#

      *
      * Initialize Work Fields
      *
     C                   EVAL      S@PGMQ = '*'

     CSR                 ENDSR

     O/EJECT
     OHYR0520PF H    OF                     2 03
     O                       wk_SysMDY            8 '  /  /  '
     O                       wk_SysTime          17 '0 :  :  '
     O                       C1HDG1              60
     O                                           68 'HYR0520'
     O                                           75 'PAGE'
     O                       PAGE          Z     79
     O          H    OF                     2
     O                       wk_RptCust          79
     O          H    OF                     1
     O                                            8 'Order#'
     O                                           18 'ShipVia'
     O                                           27 'Comment'
     O          H    OF                     1
     O                                            8 '-------'
     O                                           18 '-------'
     O                                           46 '--------------------------'
     O                                           72 '--------------------------'
     O                                           80 '--------'

     O          EF           Prt01          1
     O                       wk_Order#X           9                             8A
     O                       wk_OrdSV            16                             3A
     O                       wk_Cmt              80                             60A

     O          E            EndOfRpt    2  1
     O                                           51 '***  END OF REPORT  ***'

     O/EJECT
