     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2009')
      **************************************************************************
      * APPLICATION: Varsity interface                                         *
      *       TITLE: Update Varsity files for inquiry                          *
      *                (update at end of shipping)                             *
      *          BY: William Earl (MCA)                                        *
      *        DATE: April 2009                                                *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------- *
      *  03/14/2011  MCA  Change NEMERY to JELLIS for message sending.         *
      *  05/31/2013  MCA  Maintain turnaround number separation.               *
      *  10/17/2021  MCA  Change JMESSING and RSEKORA to MHAJNY and BEARL for  *
      *                   message sending.                                     *
      *                                                                        *
      **************************************************************************

     FOEORHP01  IF   E           K DISK
     FOEORHD    IF   E           K DISK
     FOEORDP    IF   E           K DISK
     FOEORDT    IF   E           K DISK
     FOEIVHH01  IF   E           K DISK
     FHDSHQT    IF   E           K DISK
     FHDIMST    IF   E           K DISK
     FHYLSSCC2  IF   E           K DISK
     FVPCONT    IF   E           K DISK
     FGUPTDAT   IF   E           K DISK
     FMFH1MH    UF   E           K DISK
     FVPDETL    UF A E           K DISK
     FVPPACK    UF A E           K DISK

     D/EJECT
      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D HYR0240         PR                  EXTPROC('HYR0240')
     D  Order#                        8P 0 CONST
     D  Turn#                         9P 0 CONST
     D  VarsityOrder#                10A   CONST
     D  ShutDown                      1N   CONST

     D QMHSNDM         PR                  EXTPGM('QMHSNDM')                    Send non-pgm message
     D  out_MsgID                     7A   CONST
     D  out_MsgFile                  20A   CONST
     D  out_MsgData                 512A   CONST
     D  out_MsgDtaLen                10I 0 CONST
     D  out_MsgType                  10A   CONST
     D  out_MsgQ                    100A   CONST
     D  out_MsgQCnt                  10I 0 CONST
     D  out_RpyMsgQ                  20A   CONST
     D  in_MsgKey                     4A
     D  io_MsgErrCode                10I 0

      **************************************************************************
      * Define *Entry paramter list.                                           *
      **************************************************************************
     D HYR0240         PI
     D  in_Order#                     8P 0 CONST
     D  in_Turn#                      9P 0 CONST
     D  in_VarOrder#                 10A   CONST
     D  in_ShutDown                   1N   CONST

      **************************************************************************
      * Define program status data structure.                                  *
      **************************************************************************
     D                SDS
     D ps_PgmName              1     10
     D ps_JobName            244    253
     D ps_UserID             254    263

      **************************************************************************
      * Define data structures.                                                *
      **************************************************************************
     D                 DS
     D td_TimeDate             1     14  0
     D td_Time                 1      6  0
     D td_TimeX                1      6
     D td_Date                 7     14  0

     D                 DS
     D ds_YYMD                 1      8  0
     D ds_YYMDX                1      8
     D ds_CYMD                 2      8  0
     D ds_MDYY                 5     12  0
     D ds_YYYY1                1      4  0
     D ds_CYY                  2      4  0
     D ds_YYYY2                9     12  0

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Order#       S                   LIKE(IHORD#)
     D wk_Turn#        S                   LIKE(IHTURN)
     D wk_VarOrder#    S                   LIKE(MHORDR)
     D wk_ShutDown     S              1N
     D wk_ExtPrc       S              9P 2
     D wk_TotPrc       S              9P 2
     D wk_Company      S                   LIKE(DTCMNO) INZ('01')
     D wk_PackCtl      S                   LIKE(DTPCKC)
     D wk_RcdType      S                   LIKE(DTTYPE)
     D wk_Index        S                   LIKE(DTINDX)
     D wk_ContainerID  S                   LIKE(PKCNID)
     D wk_BulkItem     S              1N   INZ(*OFF)
     D wk_MsgID        S              7A
     D wk_MsgData      S            512A
     D wk_MsgDataLen   S             10I 0
     D in_MsgKey       S              4A
     D io_MsgErrCode   S             10I 0 INZ(*Zero)
     D kf_Company      S                   LIKE(MHCMNO) INZ('01')
     D kf_OrdType      S                   LIKE(MHOTYP) INZ(*Blank)
     D kf_Order10      S                   LIKE(MHORDR)
     D kf_Lot#         S                   LIKE(SCLOT#)
     D kf_Table        S                   LIKE(TDTABL)
     D kf_TblKey1      S                   LIKE(TDKEY1)

     C/EJECT
      **************************************************************************
      * Mainline.                                                              *
      **************************************************************************

     C                   EVAL      wk_Order# = in_Order#
     C                   EVAL      wk_Turn# = in_Turn#
     C                   EVAL      wk_VarOrder# = in_VarOrder#
     C                   EVAL      wk_ShutDown = in_ShutDown

     C                   SELECT
     C                   WHEN      wk_ShutDown
     C                   EVAL      *INLR = *ON
     C                   OTHER
     C                   EXSR      $UpdVarsity
     C                   ENDSL

     C                   RETURN

     C/EJECT
      **************************************************************************
      * Update Varsity data.                                                   *
      **************************************************************************
     CSR   $UpdVarsity   BEGSR

     C     wk_Order#     CHAIN     OEORHD
     C                   IF        not %found(OEORHD)
     C                   LEAVESR
     C                   ENDIF                                                  not %found(OEORHD)

     C     wk_Order#     SETLL     OEORHP01
     C                   DOU       %eof(OEORHP01)  or
     C                               (IHSTAT = 'S'  and  IHTURN = wk_Turn#)
     C     wk_Order#     READE     OEORHP01
     C                   ENDDO                                                  %eof(OEORHP01)
     C                   IF        %eof(OEORHP01)
     C                   LEAVESR
     C                   ENDIF                                                  %eof(OEORHP01)

     C                   EVAL      wk_TotPrc = *Zero
     C     IHTURN        SETLL     OEORDP
     C                   DOU       %eof(OEORDP)
     C     IHTURN        READE     OEORDP
     C                   IF        %eof(OEORDP)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEORDP)
     C     key_OD        CHAIN     OEORDT
     C                   IF        not %found(OEORDT)
     C                   ITER
     C                   ENDIF                                                  not %found(OEORDT)
     C                   EVAL(H)   wk_ExtPrc = ODSLPR * IDQSTC
     C                   EVAL      wk_TotPrc = wk_TotPrc + wk_ExtPrc
     C                   ENDDO                                                  %eof(OEORDP)

     C                   IF        IHAIV# = *Zero
     C                   EVAL      HIINVA = *Zero
     C                   ELSE
     C     IHAIV#        CHAIN     OEIVHH01
     C                   IF        not %found(OEIVHH01)
     C                   EVAL      HIINVA = *Zero
     C                   ENDIF                                                  not %found(OEIVHH01)
     C                   ENDIF                                                  IHAIV#=*Zero

     C****               EVAL      kf_Order10 = %triml(%editc(wk_Order#:'Z'))
     C                   EVAL      kf_Order10 = wk_VarOrder#
     C     key_MH        CHAIN     MFH1MH
     C                   IF        %error  or  not %found(MFH1MH)
     C                   LEAVESR
     C                   ENDIF                                                  %error or

     C                   TIME                    td_TimeDate

     C                   EVAL      MHCUST = %triml(%editc(IHBLTO:'Z'))
     C                   EVAL      MHSHPT = %triml(%editc(IHSHTO:'Z'))
     C                   IF        IHDSHP <> *Zero
     C                   EVAL      MHSHPT = %trimr(MHSHPT) + '  DS'
     C                   ENDIF                                                  IHDSHP<>*Zero
     C                   EVAL      MHPNBR = IHORRF
     C                   EVAL      MHINVN = %triml(%editc(IHAIV#:'Z'))
     C                   EVAL      MHTERM = OECTRM
     C                   EVAL      MHSHPV = wk_TotPrc
     C                   EVAL      MHORDV = wk_TotPrc
     C                   EVAL      MHIAMT = HIINVA
     C                   EVAL      MHCUSC = IHOFRT
     C                   IF        MHPCKC = *Blanks
     C                   EVAL      ds_MDYY = td_Date
     C                   EVAL      ds_YYYY1 = ds_YYYY2
     C                   EVAL      MHPCKC = %trimr(MHORDR) + '-' + ds_YYMDX +
     C                               td_TimeX
     C                   ENDIF                                                  MHPCKC=*Blanks

     C                   UPDATE    MFH1MHR

     C                   EVAL      wk_PackCtl = MHPCKC
     C                   EVAL      wk_RcdType = 'SUMRY'
     C                   EVAL      wk_Index = *Zero
     C     key_DT        CHAIN     VPDETL
     C                   EVAL      ds_CYMD = OEBDTE
     C                   EVAL      ds_YYYY1 = ds_CYY + 1900
     C                   EVAL      DTORDT = ds_YYMD
     C                   EVAL      DTSHDT = MHSDAT
     C                   IF        %found(VPDETL)
     C                   UPDATE    VPDETLR
     C                   ELSE
     C                   EVAL      DTCMNO = wk_Company
     C                   EVAL      DTPCKC = wk_PackCtl
     C                   EVAL      DTTYPE = wk_RcdType
     C                   EVAL      DTINDX = wk_Index
     C                   EVAL      DTSHIP = MHSHMT
     C****               EVAL      DTORDR = %triml(%editc(wk_Order#:'Z'))
     C                   EVAL      DTORDR = wk_VarOrder#
     C                   WRITE     VPDETLR
     C                   ENDIF                                                  %found(VPDETL)

     C     key_CX        SETGT     VPCONT
     C     key_CX        READPE    VPCONT
     C                   IF        %eof(VPCONT)
     C                   EVAL      wk_ContainerID = *Blanks
     C                   ELSE
     C                   EVAL      wk_ContainerID = CXCNID
     C                   ENDIF                                                  %eof(VPCONT)

     C                   EVAL      wk_RcdType = *Blanks
     C     key_PK        SETLL     VPPACK
     C                   DOU       %eof(VPPACK)
     C     key_PK        READE     VPPACK
     C                   IF        %eof(VPPACK)
     C                   LEAVE
     C                   ENDIF                                                  %eof(VPPACK)
     C                   DELETE    VPPACKR
     C                   ENDDO                                                  %eof(VPPACK)

     C     IHTURN        SETLL     OEORDP
     C                   DOU       %eof(OEORDP)
     C     IHTURN        READE     OEORDP
     C                   IF        %eof(OEORDP)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEORDP)
     C                   IF        IDQSTC = *Zero
     C                   ITER
     C                   ENDIF                                                  IDQSTC=*Zero
     C     key_OD        CHAIN     OEORDT
     C                   IF        not %found(OEORDT)
     C                   ITER
     C                   ENDIF                                                  not %found(OEORDT)
     C     ODITEM        CHAIN     HDIMST
     C                   IF        not %found(HDIMST)
     C                   EVAL      IMIMWG = *Zero
     C                   EVAL      IMUDN4 = *Zero
     C                   EVAL      IMPCLS = ODPCLS
     C                   EVAL      IMLOT = 'N'
     C                   ENDIF                                                  not %found(HDIMST)
     C                   EVAL      kf_Table = 'BULKPRDCLS'
     C                   EVAL      kf_TblKey1 = IMPCLS
     C     key_TD        CHAIN     GUPTDAT
     C                   IF        %found(GUPTDAT)
     C                   EVAL      wk_BulkItem = *ON
     C                   ELSE
     C                   EVAL      wk_BulkItem = *OFF
     C                   ENDIF                                                  %found(GUPTDAT)
     C                   EVAL      PKCMNO = wk_Company
     C                   EVAL      PKSHIP = MHSHMT
     C                   EVAL      PKPCKC = MHPCKC
     C                   EVAL      PKTYPE = wk_RcdType
     C                   EVAL      PKCNID = wk_ContainerID
     C                   EVAL      PKPKIT = ODITEM
     C                   EVAL      PKSTYL = *Blanks
     C                   EVAL      PKCOLR = *Blanks
     C                   EVAL      PKSIZE = *Blanks
     C                   EVAL      PKSER# = *Blanks
     C                   EVAL      PKLIN# = IDORL#
     C****               EVAL      PKORDR = %triml(%editc(wk_Order#:'Z'))
     C                   EVAL      PKORDR = wk_VarOrder#
     C                   EVAL      PKPONO = IHORRF
     C                   EVAL      PKITDS = ODIMDS
     C                   EVAL      PKITUM = ODSTUM
     C                   EVAL      PKSTAT = *Blanks
     C                   EVAL      PKFCLS = *Blanks
     C                   IF        IMLOT = 'N'
     C                   EVAL      PKLOT# = *Blanks
     C                   EVAL      PKPKQT = IDQSTC
     C                   IF        wk_BulkItem
     C     IMUDN4        MULT(H)   IDQSTC        PKAWGT
     C                   ELSE
     C     IMIMWG        MULT(H)   IDQSTC        PKAWGT
     C                   ENDIF                                                  wk_BulkItem
     C     key_SC1       SETGT     HYLSSCC2
     C                   DOU       %eof(HYLSSCC2)  or  SCTURN = wk_Turn#
     C     key_SC1       READPE    HYLSSCC2
     C                   ENDDO                                                  %eof(HYLSSCC2) or
     C                   IF        %eof(HYLSSCC2)
     C                   EVAL      PKMRKS = *Blanks
     C                   ELSE
     C                   EVAL      PKMRKS = %triml(%editc(SCSRL#:'Z'))
     C                   ENDIF                                                  %eof(HYLSSCC2)
     C                   WRITE(E)  VPPACKR
     C                   IF        %error
     C                   EVAL      wk_MsgID = 'ERR9006'
     C                   EVAL      wk_MsgData = 'VPPACK    ' + ps_PgmName +
     C                               ps_JobName
     C                   EVAL      %subst(wk_MsgData:31:128) = PKCMNO + '/' +
     C                               PKSHIP + '/' + PKPCKC + '/' + PKTYPE +
     C                               '/' + PKCNID + '/' + PKPKIT + '/' +
     C                               ' ...      ... ' + '/' + PKLOT# + '/' +
     C                               '/' + PKSER# + '/' +
     C                               %editc(PKLIN#:'X')
     C                   EVAL      %subst(wk_MsgData:159:128) =
     C                               'Additional data: PKPKIT=' +
     C                               %trimr(PKPKIT) + ', PKPKQT=' +
     C                               %trim(%editc(PKPKQT:'J'))
     C                   EXSR      $SendITMsg
     C                   ENDIF                                                  %error
     C                   ELSE
     C     key_SQ        SETLL     HDSHQT
     C                   DOU       %eof(HDSHQT)
     C     key_SQ        READE     HDSHQT
     C                   IF        %eof(HDSHQT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HDSHQT)
     C                   EVAL      PKLOT# = %triml(SQLOTN)
     C                   EVAL      PKPKQT = SQLQTY
     C                   IF        wk_BulkItem
     C     IMUDN4        MULT(H)   SQLQTY        PKAWGT
     C                   ELSE
     C     IMIMWG        MULT(H)   SQLQTY        PKAWGT
     C                   ENDIF                                                  wk_BulkItem
     C                   EVAL      kf_Lot# = %triml(SQLOTN)
     C     key_SC2       SETGT     HYLSSCC2
     C                   DOU       %eof(HYLSSCC2)  or  SCTURN = wk_Turn#
     C     key_SC2       READPE    HYLSSCC2
     C                   ENDDO                                                  %eof(HYLSSCC2) or
     C                   IF        %eof(HYLSSCC2)
     C                   EVAL      PKMRKS = *Blanks
     C                   ELSE
     C                   EVAL      PKMRKS = %triml(%editc(SCSRL#:'Z'))
     C                   ENDIF                                                  %eof(HYLSSCC2)
     C                   WRITE(E)  VPPACKR
     C                   IF        %error
     C                   EVAL      wk_MsgID = 'ERR9006'
     C                   EVAL      wk_MsgData = 'VPPACK    ' + ps_PgmName +
     C                               ps_JobName
     C                   EVAL      %subst(wk_MsgData:31:128) = PKCMNO + '/' +
     C                               PKSHIP + '/' + PKPCKC + '/' + PKTYPE +
     C                               '/' + PKCNID + '/' + PKPKIT + '/' +
     C                               ' ...      ... ' + '/' + PKLOT# + '/' +
     C                               '/' + PKSER# + '/' +
     C                               %editc(PKLIN#:'X')
     C                   EVAL      %subst(wk_MsgData:159:128) =
     C                               'Additional data: PKPKIT=' +
     C                               %trimr(PKPKIT) + ', PKPKQT=' +
     C                               %trim(%editc(PKPKQT:'J')) + ', PKLOT#=' +
     C                               %trim(PKLOT#)
     C                   EXSR      $SendITMsg
     C                   ENDIF                                                  %error
     C                   ENDDO                                                  %eof(HDSHQT)
     C                   ENDIF                                                  IMLOT='N'
     C                   ENDDO                                                  %eof(OEORDP)

     CSR                 ENDSR

     C/EJECT
     C**************************************************************************
     C* Send I.T. department message.                                          *
     C**************************************************************************
     C     $SendITMsg    BEGSR

     C                   EVAL      wk_MsgDataLen = %len(%trimr(wk_MsgData))

     C                   CALLP(E)  QMHSNDM (wk_MsgID : 'GUPMSGF   *LIBL     ' :
     C                               wk_MsgData : wk_MsgDataLen : '*INFO' :
     C                               'MHAJNY    *LIBL     BEARL     *LIBL     +
     C                                JELLIS    *LIBL     *SYSOPR' :
     C                               4 : *Blanks : in_MsgKey : io_MsgErrCode)

     C                   ENDSR

     C/EJECT
      **************************************************************************
      * Program initialization.                                                *
      **************************************************************************
     CSR   *InzSR        BEGSR

     C     key_OD        KLIST                                                  OEORDT
     C                   KFLD                    wk_Order#
     C                   KFLD                    IDORL#
     C                   KFLD                    IDBLN#

     C     key_SQ        KLIST                                                  HDSHQT
     C                   KFLD                    IHTURN
     C                   KFLD                    wk_Order#
     C                   KFLD                    IDORL#
     C                   KFLD                    IDBLN#

     C     key_SC1       KLIST                                                  HYLSSCC2
     C                   KFLD                    wk_Order#
     C                   KFLD                    IDORL#
     C                   KFLD                    IDBLN#

     C     key_SC2       KLIST                                                  HYLSSCC2
     C                   KFLD                    wk_Order#
     C                   KFLD                    IDORL#
     C                   KFLD                    IDBLN#
     C                   KFLD                    kf_Lot#

     C     key_MH        KLIST                                                  MFH1MH
     C                   KFLD                    kf_Company
     C                   KFLD                    kf_OrdType
     C                   KFLD                    kf_Order10

     C     key_DT        KLIST                                                  VPDETL
     C                   KFLD                    wk_Company
     C                   KFLD                    wk_PackCtl
     C                   KFLD                    wk_RcdType
     C                   KFLD                    wk_Index

     C     key_CX        KLIST                                                  VPCONT
     C                   KFLD                    wk_Company
     C                   KFLD                    MHSHMT

     C     key_PK        KLIST                                                  VPPACK
     C                   KFLD                    wk_Company
     C                   KFLD                    MHSHMT
     C                   KFLD                    MHPCKC
     C                   KFLD                    wk_RcdType
     C                   KFLD                    wk_ContainerID

     C     key_TD        KLIST                                                  GUPTDAT
     C                   KFLD                    kf_Table
     C                   KFLD                    kf_TblKey1

     CSR                 ENDSR

     C/EJECT
