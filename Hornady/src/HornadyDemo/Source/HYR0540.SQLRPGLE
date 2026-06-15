     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2024')
     H DFTACTGRP(*NO) ACTGRP('HDSOPT') BNDDIR('HYR0540')
     H OPTION(*NOUNREF)
     H USRPRF(*OWNER)
      **************************************************************************
      * APPLICATION: Shipping                                                  *
      *       TITLE: Print UCC-128 carton labels for UCC-128 pallet labels     *
      *          BY: William Earl (MCA)                                        *
      *        DATE: January 2024                                              *
      *                                                                        *
      * NOTE: If a multi-carton UCC-128 serial number is passed (in the 3rd    *
      *       parameter), the program will process only that serial number.    *
      *       Otherwise, an order number and turnaround number should be       *
      *       passed, and all associated multi-carton serial numbers will be   *
      *       processed.                                                       *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  XX/XX/XXXX  XXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  *
      **************************************************************************

     FHYPPCLC   IF   E           K DISK                                         Pallet Crtn Lbl Ctrl
     FHYPSSCC   IF   E           K DISK    USROPN
     FHYLSSCC15 IF   E           K DISK    RENAME(HYRSSCC:HYRSSCC15) USROPN
     FOEORHD    IF   E           K DISK    USROPN
     FHDCUST    IF   E           K DISK    USROPN
     FHDDSHP03  IF   E           K DISK    USROPN
     FHDCTRY    IF   E           K DISK    USROPN
     FEDITPM99  IF   E           K DISK    USROPN
     FHYPSLOC   IF   E           K DISK    PREFIX(SX:2)
     FHYPWSOQ   IF   E           K DISK    USROPN
     FHYPPCLS   O    E             DISK    USROPN BLOCK(*NO)
     FHYPSLBL   UF A E           K DISK    USROPN

     D/EJECT
      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D HYR0540         PR                  EXTPGM('HYR0540')
     D  Order#                        8S 0 CONST
     D  Turn#                         9S 0 CONST
     D  MultCtnSrl#                   9S 0 CONST
     D  SSCCProc                      5P 0

     D HYR0116         PR                  EXTPROC('HYR0116')
     D  ApplID                        2    CONST
     D  Order#                        8  0 CONST
     D  Line#                         3  0 CONST
     D  Blanket#                      3  0 CONST
     D  CartonType                    1    CONST
     D  ExplosiveLbl                  1    CONST
     D  ErrorCode                     4

      /COPY QPRPSRC,EDR9900

     D QCMDEXC         PR                  EXTPGM('QCMDEXC')                    Exec system command
     D  out_Command                 512A   CONST
     D  out_CmdLen                   15P 5 CONST

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

      **************************************************************************
      * Define *Entry parameter list.                                          *
      **************************************************************************
     D HYR0540         PI
     D  in_Order#                     8S 0 CONST
     D  in_Turn#                      9S 0 CONST
     D  in_MultCtnSrl                 9S 0 CONST
     D  out_SSCCProc                  5P 0

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
     D wk_Order#       S              8S 0
     D wk_Turn#        S              9S 0
     D wk_MultCtnSrl#  S              9S 0
     D wk_SSCCProc     S              5P 0
     D wk_RtnCode      S              4A
     D wk_Alda         S              1N                                        Alda wkstn indicator
     D wk_GI281        S              1N                                        GI Hwy 281 indicator
     D wk_GIEast       S              1N                                        GI East wkstn indic
     D wk_ShpLblOutq   S             10A
     D wk_Timestamp    S               Z
     D wk_CtrlID       S                   LIKE(PCCTID)
     D wk_IWPCPC       S              9P 4
     D wk_IWPCPC0      S              5P 0
     D wk_SCUOMQ0      S              9P 0
     D wk_IMPCLS       S              4A
     D wk_IMPLTQ       S              7P 0
     D wk_BDQTYBAR     S              7P 0
     D wk_UnitPlltQty  S              7P 0
     D wk_Pct          S              5P 5
     D wk_LblQty       S              9P 0
     D wk_CurLblQty    S              9P 0
     D wk_LblCnt       S              7P 0
     D wk_LblsNeeded   S              5P 0
     D wk_Serial#      S              9P 0
     D wk_CtnSeq#      S              5P 0
     D wk_FrgnCust     S              1N   INZ(*OFF)
     D wk_StorePfxPtr  S              3P 0
     D wk_CrtnType     S              1A
     D wk_ShipLocAddr  S                   LIKE(SXSLOC)                         Shipping loc addr
     D wk_REF02        S             30A   BASED(wk_REF02Ptr)
     D wk_REF03        S             80A   BASED(wk_REF03Ptr)
     D wk_CtnLblPrtd   S              1N
     D wk_Cmd          S            256A
     D wk_RcdCnt       S              7P 0
     D wk_PgmEDR9900   S              1N   INZ(*OFF)
     D wk_Error4       S              4A
     D wk_MsgID        S              7A
     D wk_MsgData      S            512A
     D wk_MsgDataLen   S              3P 0
     D in_MsgKey       S              4A
     D io_MsgErrCode   S             10I 0
     D kf_0143         S                   LIKE(ET0143) INZ('850')
     D kf_WSID         S                   LIKE(WOWSID)
     D kf_OQTY         S                   LIKE(WOOQTY)

      **************************************************************************
      * Define data areas.                                                     *
      **************************************************************************
     D wk_DatabaseID   S              2A   DTAARA(DATABASEID)                   Database ID
     D wk_SSCCSrlRef   S              9P 0 DTAARA(SSCCSRLREF)                   SSCC serial no.

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D CtnTextPfx      C                   'CARTON #:'
     D Digits          C                   '0123456789'
     D CustNameLen     C                   %len(CMCNA1)
     D BPStoreNbrLen   C                   4
     D DSGStoreNbrLen  C                   4
     D GMStoreNbrLen   C                   4

     C/EJECT
      **************************************************************************
      * Main procedure.                                                        *
      **************************************************************************

     C                   EVAL      wk_Order# = in_Order#
     C                   EVAL      wk_Turn# = in_Turn#
     C                   EVAL      wk_MultCtnSrl# = in_MultCtnSrl
     C                   EVAL      wk_SSCCProc = *Zero
     C                   EVAL      out_SSCCProc = *Zero

     C                   DO

     C                   EVAL      wk_CtnLblPrtd = *OFF
     C                   EVAL      wk_RtnCode = *Blanks

     C                   EVAL      wk_CtrlID = '*DFT'

     C     wk_CtrlID     CHAIN     HYPPCLC

     C                   SELECT

     C                   WHEN      wk_Order# = -1
     C                   EXSR      $ShutDownPgms
     C                   EVAL      *INLR = *ON

     C                   WHEN      wk_MultCtnSrl# <> *Zero

     C                   IF        not %open(HYPSSCC)
     C                   OPEN      HYPSSCC
     C                   ENDIF                                                  not %open(HYLSSCC15)

     C     wk_MultCtnSrl#SETLL     HYPSSCC
     C                   DOU       %eof(HYPSSCC)
     C     wk_MultCtnSrl#READE     HYPSSCC
     C                   IF        %eof(HYPSSCC)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPSSCC)

     C                   EXSR      $Process

     C                   IF        not wk_CtnLblPrtd  and
     C                               wk_RtnCode <> '0001'
     C                   EVAL      wk_MsgID = 'ERR1451'
     C                   EVAL      wk_MsgData = %triml(%editc(SCSRL#:'Z'))
     C                   EVAL      %subst(wk_MsgData:10:10) = ps_Pgnm
     C                   IF        wk_Order# <> *Zero
     C                   EVAL      %subst(wk_MsgData:20:8) =
     C                               %triml(%editc(wk_Order#:'Z'))
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:20:8) = '*N'
     C                   ENDIF                                                  wk_Order#<>*Zero
     C                   IF        wk_Turn# <> *Zero
     C                   EVAL      %subst(wk_MsgData:28:9) =
     C                               %triml(%editc(wk_Turn#:'Z'))
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:28:9) = '*N'
     C                   ENDIF                                                  wk_Turn#<>*Zero
     C                   IF        wk_RtnCode <> *Blanks
     C                   EVAL      %subst(wk_MsgData:37:9) = wk_RtnCode
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:37:9) = '*N'
     C                   ENDIF                                                  wk_RtnCode<>*Blanks
     C                   EXSR      $SndPM
     C                   ENDIF                                                  not wk_CtnLblPrtd

     C                   ENDDO                                                  %eof(HYPSSCC)

     C                   OTHER

     C                   IF        not %open(HYLSSCC15)
     C                   OPEN      HYLSSCC15
     C                   ENDIF                                                  not %open(HYLSSCC15)

     C     key_SC        SETLL     HYLSSCC15
     C                   DOU       %eof(HYLSSCC15)
     C     key_SC        READE     HYLSSCC15
     C                   IF        %eof(HYLSSCC15)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSSCC15)

     C                   EXSR      $Process

     C                   ENDDO                                                  %eof(HYLSSCC15)

     C                   ENDSL

     C                   EVAL      out_SSCCProc = wk_SSCCProc

     C                   ENDDO

     C                   RETURN

     C/EJECT
      **************************************************************************
      *****                S  U  B  R  O  U  T  I  N  E  S                 *****
      **************************************************************************
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

     C/EJECT
      **************************************************************************
      * Process multi-carton UCC-128 label.                                    *
      **************************************************************************
     CSR   $Process      BEGSR

      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPPCLS
                where CSSRL#=:SCSRL#;
       IF wk_RcdCnt > *Zero;
         wk_MsgID = 'ERR1450';
         wk_MsgData = %triml(%editc(SCSRL#:'Z'));
         %subst(wk_MsgData:10:10) = ps_Pgnm;
         IF wk_Order# <> *Zero;
           %subst(wk_MsgData:20:8) =
              %triml(%editc(wk_Order#:'Z'));
         ELSE;
           %subst(wk_MsgData:20:8) = '*N';
         ENDIF;                                                              // wk_Order#<>*Zero
         IF wk_Turn# <> *Zero;
           %subst(wk_MsgData:28:9) =
              %triml(%editc(wk_Turn#:'Z'));
         ELSE;
           %subst(wk_MsgData:28:9) = '*N';
         ENDIF;                                                              // wk_Turn#<>*Zero
         IF wk_MultCtnSrl# <> *Zero;
           %subst(wk_MsgData:37:9) =
              %triml(%editc(wk_MultCtnSrl#:'Z'));
         ELSE;
           %subst(wk_MsgData:37:9) = '*N';
         ENDIF;                                                              // wk_MultCtnSrl#<>*Zer
         EXSR $SndPM;
         IF wk_MultCtnSrl# <> *Zero;
           wk_RtnCode = '0001';
         ENDIF;                                                              // wk_MultCtnSrl#<>*Zer
         LEAVESR;
       ENDIF;                                                                // wk_RcdCnt>*Zero

       wk_IWPCPC = *Zero;
       EXEC SQL select IWPCPC into :wk_IWPCPC
                from HDIWHS
                where IWITEM=:SCITEM
                fetch first row only;
       IF wk_IWPCPC = *Zero;
         IF wk_MultCtnSrl# <> *Zero;
           wk_RtnCode = '0002';
         ENDIF;                                                              // wk_MultCtnSrl#<>*Zer
         LEAVESR;
       ENDIF;                                                                // wk_IWPCPC=*Zero

       wk_IMPCLS = *Blanks;
       EXEC SQL select IMPCLS into :wk_IMPCLS
                from HDIMST
                where IMITEM=:SCITEM;

       wk_IMPLTQ = *Zero;
       EXEC SQL select IMPLTQ into :wk_IMPLTQ
                from HDIMST2
                where IMITEM=:SCITEM;

       wk_BDQTYBAR = *Zero;
       EXEC SQL select BDQTYBAR into :wk_BDQTYBAR
                from BARDATA
                where BDITEM=:SCITEM;
      /End-Free

     C                   IF        SCUOMQ <= wk_IWPCPC  or
     C                               SCUOMQ = wk_BDQTYBAR
     C                   IF        wk_MultCtnSrl# <> *Zero
     C                   EVAL      wk_RtnCode = '0003'
     C                   ENDIF                                                  wk_MultCtnSrl#<>*Zer
     C                   LEAVESR
     C                   ENDIF                                                  SCUOMQ<=wk_IWPCPC

     C                   EVAL      wk_UnitPlltQty = wk_IWPCPC * wk_IMPLTQ

     C                   SELECT
     C                   WHEN      %subst(wk_IMPCLS:1:1)='A'  or
     C                               %subst(wk_IMPCLS:1:1)='F'
     C                   EVAL      wk_Pct = PCAPQP / 100
     C                   IF        PCAPQP <> *Zero  and  wk_IMPLTQ <> *Zero  and
     C                               SCUOMQ >= wk_UnitPlltQty * wk_Pct
     C                   EXSR      $PrtCrtnLbls
     C                   LEAVESR
     C                   ENDIF                                                  PCAPQP<>*Zero and
     C                   IF        PCACQ <> *Zero  and
     C                               SCUOMQ >= wk_IWPCPC * PCACQ
     C                   EXSR      $PrtCrtnLbls
     C                   LEAVESR
     C                   ENDIF                                                  PCACQ<>*Zero and
     C                   WHEN      %subst(wk_IMPCLS:1:1)='B'
     C                   EVAL      wk_Pct = PCBPQP / 100
     C                   IF        PCBPQP <> *Zero  and  wk_IMPLTQ <> *Zero  and
     C                               SCUOMQ >= wk_UnitPlltQty * wk_Pct
     C                   EXSR      $PrtCrtnLbls
     C                   LEAVESR
     C                   ENDIF                                                  PCBPQP<>*Zero and
     C                   IF        PCBCQ <> *Zero  and
     C                               SCUOMQ >= wk_IWPCPC * PCBCQ
     C                   EXSR      $PrtCrtnLbls
     C                   LEAVESR
     C                   ENDIF                                                  PCBCQ<>*Zero and
     C                   WHEN      %subst(wk_IMPCLS:1:1)='C'
     C                   EVAL      wk_Pct = PCCPQP / 100
     C                   IF        PCCPQP <> *Zero  and  wk_IMPLTQ <> *Zero  and
     C                               SCUOMQ >= wk_UnitPlltQty * wk_Pct
     C                   EXSR      $PrtCrtnLbls
     C                   LEAVESR
     C                   ENDIF                                                  PCCPQP<>*Zero and
     C                   IF        PCCCQ <> *Zero  and
     C                               SCUOMQ >= wk_IWPCPC * PCCCQ
     C                   EXSR      $PrtCrtnLbls
     C                   LEAVESR
     C                   ENDIF                                                  PCCCQ<>*Zero and
     C                   WHEN      %subst(wk_IMPCLS:1:1)='T'
     C                   EVAL      wk_Pct = PCTPQP / 100
     C                   IF        PCTPQP <> *Zero  and  wk_IMPLTQ <> *Zero  and
     C                               SCUOMQ >= wk_UnitPlltQty * wk_Pct
     C                   EXSR      $PrtCrtnLbls
     C                   LEAVESR
     C                   ENDIF                                                  PCTPQP<>*Zero and
     C                   IF        PCTCQ <> *Zero  and
     C                               SCUOMQ >= wk_IWPCPC * PCTCQ
     C                   EXSR      $PrtCrtnLbls
     C                   LEAVESR
     C                   ENDIF                                                  PCTCQ<>*Zero and
     C                   WHEN      %subst(wk_IMPCLS:1:1)='S'
     C                   EVAL      wk_Pct = PCSPQP / 100
     C                   IF        PCSPQP <> *Zero  and  wk_IMPLTQ <> *Zero  and
     C                               SCUOMQ >= wk_UnitPlltQty * wk_Pct
     C                   EXSR      $PrtCrtnLbls
     C                   LEAVESR
     C                   ENDIF                                                  PCSPQP<>*Zero and
     C                   IF        PCSCQ <> *Zero  and
     C                               SCUOMQ >= wk_IWPCPC * PCSCQ
     C                   EXSR      $PrtCrtnLbls
     C                   LEAVESR
     C                   ENDIF                                                  PCSCQ<>*Zero and
     C                   OTHER
     C                   IF        wk_MultCtnSrl# <> *Zero
     C                   EVAL      wk_RtnCode = '0005'
     C                   ENDIF                                                  wk_MultCtnSrl#<>*Zer
     C                   LEAVESR
     C                   ENDSL

     C                   IF        wk_MultCtnSrl# <> *Zero  and
     C                               not wk_CtnLblPrtd  and
     C                               wk_RtnCode = *Blanks
     C                   EVAL      wk_RtnCode = '0004'
     C                   ENDIF                                                  wk_MultCtnSrl#<>*Zer

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Print UCC-128 carton labels.                                           *
      **************************************************************************
     CSR   $PrtCrtnLbls  BEGSR

     C                   IF        not %open(OEORHD)
     C                   OPEN      OEORHD
     C                   ENDIF                                                  not %open(OEORHD)
     C                   IF        not %open(HDCUST)
     C                   OPEN      HDCUST
     C                   ENDIF                                                  not %open(HDCUST)
     C                   IF        not %open(HDDSHP03)
     C                   OPEN      HDDSHP03
     C                   ENDIF                                                  not %open(HDDSHP03)
     C                   IF        not %open(HYPPCLS)
     C                   OPEN      HYPPCLS
     C                   ENDIF                                                  not %open(HYPPCLS)
     C                   IF        not %open(HYPSLBL)
     C                   OPEN      HYPSLBL
     C                   ENDIF                                                  not %open(HYPSLBL)

     C                   EXSR      $OvrLblPrtr

     C                   EXSR      $GrpSepLbl

     C                   EVAL      wk_LblQty = *Zero
     C                   EVAL      wk_LblCnt = *Zero

     C                   EVAL      wk_IWPCPC0 = wk_IWPCPC
     C                   EVAL      wk_SCUOMQ0 = SCUOMQ
     C                   EVAL      wk_LblsNeeded = %div(wk_SCUOMQ0:wk_IWPCPC0)
     C                   IF        %rem(wk_SCUOMQ0:wk_IWPCPC0) > *Zero
     C                   EVAL      wk_LblsNeeded = wk_LblsNeeded + 1
     C                   ENDIF                                                  %rem(wk_SCUOMQ0:wk_I

     C                   EXSR      $RtvSSCCSrl#

     C                   EVAL      wk_Timestamp = %timestamp

     C                   EVAL      wk_CtnSeq# = *Zero

     C                   DOU       wk_LblQty = SCUOMQ

     C                   IF        wk_LblQty + wk_IWPCPC <= SCUOMQ
     C                   EVAL      wk_CurLblQty = wk_IWPCPC
     C                   EVAL      wk_LblQty = wk_LblQty + wk_IWPCPC
     C                   ELSE
     C                   EVAL      wk_CurLblQty = SCUOMQ - wk_LblQty
     C                   EVAL      wk_LblQty = SCUOMQ
     C                   ENDIF                                                  wk_LblQty+wk_IWPCPC

     C                   EVAL      wk_Serial# = wk_Serial# + 1
     C                   EVAL      wk_CtnSeq# = wk_CtnSeq# + 1

     C                   EXSR      $WrtPCLS

     C                   EXSR      $WrtSLBL

     C                   IF        wk_CurLblQty = wk_IWPCPC
     C                   EVAL      wk_CrtnType = 'F'
     C                   ELSE
     C                   EVAL      wk_CrtnType = 'P'
     C                   ENDIF                                                  wk_CurLblQty=IWPCPC

     C                   CALLP(E)  HYR0116('OE':SCORD#:SCORL#:SCBLN#:
     C                               wk_CrtnType:'N':wk_Error4)
     C                   IF        %error
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0116'
     C                   EXSR      $SndPM
     C                   ENDIF                                                  %error

     C                   EVAL      wk_LblCnt = wk_LblCnt + 1

     C                   ENDDO                                                  wk_LblQty=SCUOMQ

     C     key_SL        DELETE(E) HYPSLBL

     C                   EVAL      wk_MsgID = 'MSG0475'
     C                   EVAL      wk_MsgData = %triml(%editc(SCORD#:'Z'))
     C                   EVAL      %subst(wk_MsgData:9:9) =
     C                                          %triml(%editc(SCTURN:'Z'))
     C                   EVAL      %subst(wk_MsgData:18:9) =
     C                                          %triml(%editc(wk_LblCnt:'1'))
     C                   EXSR      $SndPM

     C                   EVAL      wk_SSCCProc = wk_SSCCProc + 1

     C                   EVAL      wk_CtnLblPrtd = *ON

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Print group separator label.                                           *
      **************************************************************************
     CSR   $GrpSepLbl    BEGSR

     C                   EVAL      wk_Serial# = SCSRL#
     C                   EVAL      wk_CtnSeq# = *Zero
     C                   EVAL      wk_CurLblQty = SCUOMQ
     C                   EXSR      $WrtSLBL
     C                   EVAL      wk_CrtnType = 'G'

     C                   CALLP(E)  HYR0116('OE':SCORD#:SCORL#:SCBLN#:
     C                               wk_CrtnType:'N':wk_Error4)
     C                   IF        %error
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0116'
     C                   EXSR      $SndPM
     C                   ENDIF                                                  %error

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Write HYPPCLS record.                                                  *
      **************************************************************************
     CSR   $WrtPCLS      BEGSR

     C                   EVAL      CSSRL# = SCSRL#
     C                   EVAL      CSCSRL# = wk_Serial#
     C                   EVAL      CSORD# = SCORD#
     C                   EVAL      CSORL# = SCORL#
     C                   EVAL      CSBLN# = SCBLN#
     C                   EVAL      CSITEM = SCITEM
     C                   EVAL      CSIMDS = SCIMDS
     C                   EVAL      CSUOM = SCUOM
     C                   EVAL      CSUOMQ = wk_CurLblQty
     C                   EVAL      CSUOMW = SCUOMW
     C                   EVAL      CSUWGT = SCUWGT
     C                   EVAL(H)   CSEWGT = wk_CurLblQty * SCUWGT
     C                   EVAL      CSUPC = SCUPC
     C                   EVAL      CSCTN# = wk_CtnSeq#
     C                   EVAL      CSTURN = SCTURN
     C                   EVAL      CSPSRL# = SCPSRL#
     C                   EVAL      CSBOL# = SCBOL#
     C                   EVAL      CSTSTP1 = SCTSTP
     C                   EVAL      CSTSTP2 = wk_Timestamp

     C                   WRITE     HYRPCLS

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Write HYPSLBL record.                                                  *
      **************************************************************************
     CSR   $WrtSLBL      BEGSR

     C     key_SL        CHAIN     HYPSLBL
     C                   IF        %found(HYPSLBL)
     C                   CLEAR                   HYRSLBL
     C                   ENDIF                                                  %found(HYPSLBL)

     C     SCORD#        CHAIN     OEORHD
     C                   IF        not %found(OEORHD)
     C                   EVAL      OESVSV = *Blanks
     C                   EVAL      OESVDS = *Blanks
     C                   EVAL      OEUDF3 = *Blanks
     C                   EVAL      OEBLTO = *Zero
     C                   EVAL      OESHTO = *Zero
     C                   EVAL      OEDSHP = *Zero
     C                   ENDIF                                                  not %found(OEORHD)

     C     OEBLTO        CHAIN     HDCUST
     C                   IF        not %found(HDCUST)
     C                   EVAL      CMCCLS = *Blanks
     C                   ENDIF                                                  not %found(HDCUST)
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from GUPTDAT
                where TDTABL='CUSTCLASS' and TDKEY1=:CMCCLS and
                      TDFF02='Y';
      /End-Free
     C                   IF        wk_RcdCnt > *Zero
     C                   EVAL      wk_FrgnCust = *ON
     C                   ELSE
     C                   EVAL      wk_FrgnCust = *OFF
     C                   ENDIF                                                  wk_RcdCnt>*Zero

     C     OESHTO        CHAIN     HDCUST
     C                   IF        not %found(HDCUST)
     C                   EVAL      CMCNA1 = *Blanks
     C                   EVAL      CMCNA2 = *Blanks
     C                   EVAL      CMCNA3 = *Blanks
     C                   EVAL      CMCNA4 = *Blanks
     C                   EVAL      CMCCTY = *Blanks
     C                   EVAL      CMST = *Blanks
     C                   EVAL      CMZIP = *Blanks
     C                   EVAL      CMCTRY = *Blanks
     C                   EVAL      CMSTOR = *Blanks
     C                   ENDIF                                                  not %found(HDCUST)

     C                   IF        OEDSHP <> *Zero
     C     OEDSHP        CHAIN     HDDSHP03
     C                   IF        not %found(HDDSHP03)
     C                   EVAL      DSNAME = *Blanks
     C                   EVAL      DSADR1 = *Blanks
     C                   EVAL      DSADR2 = *Blanks
     C                   EVAL      DSADR3 = *Blanks
     C                   EVAL      DSCITY = *Blanks
     C                   EVAL      DSST = *Blanks
     C                   EVAL      DSZIP = *Blanks
     C                   EVAL      DSCTRY = *Blanks
     C                   ENDIF                                                  not %found(HDDSHP03)
     C                   ENDIF                                                  SDDSHP<>*Zero

     C                   EVAL      SLORD# = SCORD#
     C                   EVAL      SLSVSV = *Blanks
     C                   EVAL      SLSVDS = *Blanks
     C                   EVAL      SLSCAC = *Blanks
     C                   EVAL      SLBOL# = *Zero
     C                   EVAL      SLPRO# = *Blanks
     C                   EVAL      SLITEM = SCITEM
     C                   EVAL      SLIMDS = SCIMDS
     C                   EVAL      SLCITM = *Blanks
     C                   EVAL      SLUPC = SCUPC
     C                   EVAL      SLFCQT = wk_IWPCPC
     C                   EVAL      SLQTY = wk_CurLblQty
     C                   EVAL      SLUOM = SCUOM
     C                   EVAL      SLSRL# = wk_Serial#
     C                   EVAL      SLCTN# = wk_CtnSeq#
     C                   EVAL      SLTCC = wk_LblsNeeded

     C                   SELECT
     C                   WHEN      OEBLTO = 59                                  Bass Pro Shops
     C                               or  OEBLTO = 52                            Academy Stores
     C                               or  OEBLTO = 337                           Dick's Sporting Good
     C                               or  OEBLTO = 21419                         Gander Mountain
     C                               or  OEBLTO = 536527                        DSG Field & Stream
     C                   EVAL      SLCCTX = CtnTextPfx + ' ' +
     C                               %triml(%editc(SLCTN#:'4'))
     C                   OTHER
     C                   EVAL      SLCCTX = *Blanks
     C                   ENDSL

     C                   SELECT
     C                   WHEN      OEBLTO = 17                                  Sports, Inc.
     C                   IF        %check(Digits:CMCNA1:23) = *Zero
     C                   EVAL      SLSTOR = %subst(CMCNA1:23:4)
     C                   ELSE
     C                   EVAL      SLSTOR = *Blanks
     C                   ENDIF                                                  %check(Digits:%subst
     C                   WHEN      OEBLTO = 52                                  Academy Stores
     C                   EVAL      SLSTOR = '00004'
     C                   WHEN      OEBLTO = 59                                  Bass Pro Shops
     C                   EVAL      wk_StorePfxPtr = %scan(' #' : CMCNA1)
     C                   IF        wk_StorePfxPtr = *Zero  or
     C                               wk_StorePfxPtr >
     C                                 CustNameLen - BPStoreNbrLen - 1
     C                   EVAL      SLSTOR = *Blanks
     C                   ELSE
     C                   EVAL      SLSTOR = %subst(CMCNA1 :
     C                               wk_StorePfxPtr + 2 : BPStoreNbrLen)
     C                   ENDIF                                                  wk_StorePfxPtr=*Zero
     C                   WHEN      OEBLTO = 337                                 Dick's Sporting Good
     C                               or  OEBLTO = 536527                        DSG Field & Stream
     C                   IF        CMSTOR <> *Blanks
     C                   EVAL      SLSTOR = CMSTOR
     C                   ELSE
     C                   EVAL      wk_StorePfxPtr = %scan(' #' : CMCNA1)
     C                   IF        wk_StorePfxPtr = *Zero  or
     C                               wk_StorePfxPtr >
     C                                 CustNameLen - DSGStoreNbrLen - 1
     C                   EVAL      SLSTOR = *Blanks
     C                   ELSE
     C                   EVAL      SLSTOR = %subst(CMCNA1 :
     C                               wk_StorePfxPtr + 2 : DSGStoreNbrLen)
     C                   ENDIF                                                  wk_StorePfxPtr=*Zero
     C                   ENDIF                                                  CMSTOR<>*Blanks
     C                   WHEN      OEBLTO = 21419
     C                   EVAL      wk_StorePfxPtr = %scan(' #' : CMCNA1)
     C                   IF        wk_StorePfxPtr = *Zero  or
     C                               wk_StorePfxPtr >
     C                                 CustNameLen - GMStoreNbrLen - 1
     C                   EVAL      SLSTOR = *Blanks
     C                   ELSE
     C                   EVAL      SLSTOR = %subst(CMCNA1 :
     C                               wk_StorePfxPtr + 2 : GMStoreNbrLen)
     C                   ENDIF                                                  wk_StorePfxPtr=*Zero
     C                   OTHER
     C                   EVAL      SLSTOR = *Blanks
     C                   ENDSL

     C                   SELECT
     C                   WHEN      OEBLTO = 52                                  Academy Stores
     C                   EVAL      SLVEND = '01211'
     C                   OTHER
     C                   EVAL      SLVEND = *Blanks
     C                   ENDSL

     C                   IF        OEDSHP = *Zero  or
     C                               (CMCTRY <> 'US'  and  CMCTRY <> 'CA'  and
     C                                CMCTRY <> *Blanks)
     C                   EVAL      SLSTNM = CMCNA1
     C                   EVAL      SLSTA1 = CMCNA2
     C                   EVAL      SLSTA2 = CMCNA3
     C                   EVAL      SLSTA3 = CMCNA4
     C                   EVAL      SLSTCY = CMCCTY
     C                   EVAL      SLSTST = CMST
     C                   EVAL      SLSTZP = CMZIP
     C                   IF        CMCTRY = 'US'  or  CMCTRY = *Blanks
     C                   EVAL      SLSTCT = *Blanks
     C                   ELSE
     C                   IF        not %open(HDCTRY)
     C                   OPEN      HDCTRY
     C                   ENDIF                                                  not %open(HDCTRY)
     C     CMCTRY        CHAIN     HDCTRY
     C                   IF        %found(HDCTRY)
     C                   IF        CNCDES <> SLSTA1  and
     C                               CNCDES <> SLSTA2  and
     C                               CNCDES <> SLSTA3  and
     C                               CNCDES <> SLSTCY
     C                   EVAL      SLSTCT = CNCDES
     C                   ELSE
     C                   EVAL      SLSTCT = *Blanks
     C                   ENDIF                                                  CNCDES<>SLSTA1 and
     C                   ELSE
     C                   EVAL      SLSTCT = CMCTRY
     C                   ENDIF                                                  %found(HDCTRY)
     C                   ENDIF                                                  CMCTRY='US' or
     C                   ELSE
     C                   EVAL      SLSTNM = DSNAME
     C                   EVAL      SLSTA1 = DSADR1
     C                   EVAL      SLSTA2 = DSADR2
     C                   EVAL      SLSTA3 = DSADR3
     C                   EVAL      SLSTCY = DSCITY
     C                   EVAL      SLSTST = DSST
     C                   EVAL      SLSTZP = DSZIP
     C                   IF        DSCTRY = 'US'  or  DSCTRY = *Blanks
     C                   EVAL      SLSTCT = *Blanks
     C                   ELSE
     C                   IF        not %open(HDCTRY)
     C                   OPEN      HDCTRY
     C                   ENDIF                                                  not %open(HDCTRY)
     C     DSCTRY        CHAIN     HDCTRY
     C                   IF        %found(HDCTRY)
     C                   IF        CNCDES <> SLSTA1  and
     C                               CNCDES <> SLSTA2  and
     C                               CNCDES <> SLSTA3  and
     C                               CNCDES <> SLSTCY
     C                   EVAL      SLSTCT = CNCDES
     C                   ELSE
     C                   EVAL      SLSTCT = *Blanks
     C                   ENDIF                                                  CNCDES<>SLSTA1 and
     C                   ELSE
     C                   EVAL      SLSTCT = DSCTRY
     C                   ENDIF                                                  %found(HDCTRY)
     C                   ENDIF                                                  DSCTRY='US' or
     C                   ENDIF                                                  OEDSHP=*Zero

     C                   SELECT
     C                   WHEN      OEBLTO = 337  or                             Dick's Sporting Good
     C                               OEBLTO = 536527                            DSG Field & Stream
     C                   IF        not %open(EDITPM99)
     C                   OPEN      EDITPM99
     C                   ENDIF                                                  not %open(EDITPM99)
     C     key_ET        CHAIN     EDITPM99
     C                   IF        %found(EDITPM99)
     C                   EVAL      wk_Ref02Ptr =
     C                               EDI850Char(ETTPID:OEORRF:'REF02':'DP')
     C                   EVAL      SLDEPT = wk_Ref02
     C                   EVAL      wk_Ref03Ptr =
     C                               EDI850Char(ETTPID:OEORRF:'REF03':'DP')
     C                   EVAL      SLDPNM = wk_Ref03
     C                   EVAL      wk_PgmEDR9900 = *ON
     C                   ELSE
     C                   EVAL      SLDEPT = *Blanks
     C                   EVAL      SLDPNM = *Blanks
     C                   ENDIF                                                  %found(EDITPM99)
     C                   OTHER
     C                   EVAL      SLDEPT = *Blanks
     C                   EVAL      SLDPNM = *Blanks
     C                   ENDSL

     C                   SELECT
     C                   WHEN      wk_DatabaseID = 'HY'  or
     C                               wk_DatabaseID = 'XX'
     C                   SELECT
     C                   WHEN      wk_Alda  and                                 Alda workstation
     C                               not wk_FrgnCust
     C                   EVAL      wk_ShipLocAddr = 'ALDA'
     C                   WHEN      wk_GI281  and                                GI workstation
     C                               not wk_FrgnCust
     C                   EVAL      wk_ShipLocAddr = 'GI'
     C                   OTHER
     C                   EVAL      wk_ShipLocAddr = 'WEST'
     C                   ENDSL
     C                   WHEN      wk_DatabaseID = 'PV'  or
     C                               wk_DatabaseID = 'PX'
     C                   EVAL      wk_ShipLocAddr = 'PVE'
     C                   OTHER
     C                   EVAL      wk_ShipLocAddr = '****'
     C                   ENDSL
     C                   IF        wk_ShipLocAddr <> SXSLOC
     C     wk_ShipLocAddrCHAIN     HYPSLOC
     C                   IF        not %found(HYPSLOC)
     C                   CLEAR     *ALL          HYRSLOC
     C                   EVAL      SXNAME = 'HORNADY MFG COMPANY'
     C                   ENDIF                                                  not %found(HYPSLOC)
     C                   ENDIF                                                  wk_ShipLoc<>SXSLOC
     C                   EVAL      SLSFNM = SXNAME
     C                   EVAL      SLSFA1 = SXADDR1
     C                   IF        SXADDR2 = *Blanks
     C                   EVAL      SLSFA2 = %trimr(SXCITY) + ', ' + SXST + ' ' +
     C                               SXZIP
     C                   EVAL      SLSFA3 = %triml(
     C                               %editw(SXPHON:'0&(   )&   -    '))
     C                   EVAL      SLSFA4 = *Blanks
     C                   ELSE
     C                   EVAL      SLSFA2 = SXADDR2
     C                   EVAL      SLSFA3 = %trimr(SXCITY) + ', ' + SXST + ' ' +
     C                               SXZIP
     C                   EVAL      SLSFA4 = %triml(
     C                               %editw(SXPHON:'0&(   )&   -    '))
     C                   ENDIF                                                  SXADDR2=*Blanks
     C                   EVAL      SLJOBN = ps_Job#

     C                   IF        not %found(HYPSLBL)
     C                   WRITE     HYRSLBL
     C                   ELSE
     C                   UPDATE    HYRSLBL
     C                   ENDIF                                                  not %found(HYPSLBL)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Retrieve next GS1-128 SSCC serial number.                              *
      **************************************************************************
     CSR   $RtvSSCCSrl#  BEGSR

     C     *LOCK         IN        wk_SSCCSrlRef
     C                   EVAL      wk_Serial# = wk_SSCCSrlRef
     C                   EVAL      wk_SSCCSrlRef = wk_SSCCSrlRef + wk_LblsNeeded
     C                   OUT       wk_SSCCSrlRef

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Override to correct label printer.                                     *
      **************************************************************************
     CSR   $OvrLblPrtr   BEGSR

      *
      * Retrieve "extra" label output queue (if not already retrieved).
      *
     C                   IF        wk_ShpLblOutq = *Blanks
     C                   IF        not %open(HYPWSOQ)
     C                   OPEN      HYPWSOQ
     C                   ENDIF                                                  not %open(HYPWSOQ)
     C                   EVAL      kf_WSID = ps_Job
     C                   EVAL      kf_OQTY = 'SHIPXCTLBL'                       Extra carton label
     C     key_WO        CHAIN     HYPWSOQ
     C                   IF        not %found(HYPWSOQ)
     C                   EVAL      kf_WSID = '*DFT'
     C     key_WO        CHAIN     HYPWSOQ
     C                   ENDIF                                                  not %found(HYPWSOQ)
     C                   IF        not %found(HYPWSOQ)
     C                   EVAL      WOOUTQ = 'QPRINTS'
     C                   ENDIF                                                  not %found(HYPWSOQ)
     C                   EVAL      wk_ShpLblOutq = WOOUTQ
     C                   CLOSE     HYPWSOQ
     C                   ENDIF                                                  wk_ShpLblOutq=*Blank

      *
      * Override label print to "extra" label output queue.
      *
     C                   EVAL      wk_Cmd = 'OVRPRTF FILE(BCLABELS) +
     C                               OUTQ(' + %trimr(wk_ShpLblOutq) +
     C                               ') OVRSCOPE(*CALLLVL) SECURE(*YES)'
     C                   CALLP(E)  QCMDEXC (wk_Cmd:%len(%triml(wk_Cmd)))

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Shut down called programs.                                             *
      **************************************************************************
     CSR   $ShutDownPgms BEGSR

     C                   IF        wk_PgmEDR9900                                EDR9900 called
     C                   CALLP(E)  sd_EDR9900
     C                   ENDIF                                                  wk_PgmEDR9900

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Program initialization - each call.                                    *
      **************************************************************************
     CSR   $InzSR        BEGSR


     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Program initialization - first time only.                              *
      **************************************************************************
     CSR   *InzSR        BEGSR

      *
      * Define key lists.
      *
     C     key_SC        KLIST                                                  HYLSSCC15
     C                   KFLD                    wk_Order#
     C                   KFLD                    wk_Turn#

     C     key_ET        KLIST                                                  EDITPM99
     C                   KFLD                    kf_0143
     C                   KFLD                    OESHTO

     C     key_SL        KLIST                                                  HYPSLBL
     C                   KFLD                    SCORD#
     C                   KFLD                    ps_Job#

     C     key_WO        KLIST                                                  HYPWSOQ
     C                   KFLD                    kf_WSID
     C                   KFLD                    kf_OQTY

      *
      * Retrieve current database ID.
      *
     C                   IN        wk_DatabaseID

      *
      * Retrieve workstation DC location.
      *
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from GUPTDAT
                where TDTABL='ALDAWKSTN' and TDKEY1=:ps_Job;
       IF wk_RcdCnt > *Zero;
         wk_Alda = *ON;
       ELSE;
         wk_Alda = *OFF;
       ENDIF;                                                                // wk_RcdCnt>*Zero
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from GUPTDAT
                where TDTABL='GIWKSTN' and TDKEY1=:ps_Job;
       IF wk_RcdCnt > *Zero;
         wk_GI281 = *ON;
       ELSE;
         wk_GI281 = *OFF;
       ENDIF;                                                                // wk_RcdCnt>*Zero
      /End-Free

     CSR                 ENDSR

     C/EJECT
