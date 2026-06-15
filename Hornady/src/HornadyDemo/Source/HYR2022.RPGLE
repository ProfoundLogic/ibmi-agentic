     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2010')
     H BNDDIR('HYR2022')
      **************************************************************************
      * APPLICATION: Shipping / EDI                                            *
      *       TITLE: Write HDS BOL data for non-truck EDI shipment             *
      *                (Paper BOL is not generated/printed)                    *
      *          BY: William Earl (MCA)                                        *
      *        DATE: July 2010                                                 *
      *                                                                        *
      * This service program will return one of the following codes:           *
      *   00 - No errors                                                       *
      *   01 - Order header not found                                          *
      *   02 - Customer master not found                                       *
      *   03 - Drop ship master not found                                      *
      *   04 - HYPBOL record locked (or encountered an error on retrieval)     *
      *   05 - Bill of lading number already assigned to shipment              *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  09/20/2010  MCA  Don't add record to EDI 211 tickler file anymore.    *
      *                   Not used.                                            *
      *  06/10/2013  MCA  Maintain turnaround number separation.               *
      *  08/23/2013  MCA  Use turn number to find correct packing list.        *
      *  09/05/2017  MCA  SnapSafe changes.                                    *
      *  07/21/2019  MCA  Handle shipment prior BOL assignment.                *
      *  07/21/2021  MCA  Allow HYPSSCC.SCBOL# update for SCBOL#=99999999.     *
      *                                                                        *
      **************************************************************************

     FOEORHD    IF   E           K DISK
     FHDCUST    IF   E           K DISK
     FHDDSHP    IF   E           K DISK
     FHDSHPV    IF   E           K DISK
     FHDFTCL    IF   E           K DISK
     FOEBOLH    IF A E           K DISK
     FOEBOLD    O    E           K DISK
     FOEPKLD03  IF   E           K DISK
     FOEPKLH    UF   E           K DISK    PREFIX(PH:2)
     FOECTRL    UF   E             DISK
     FHYPBOL    UF A E           K DISK
     FHYLSSCC2  UF   E           K DISK
     FEDOTPM    IF   E           K DISK

     D/EJECT
      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D HYR2022         PR                  EXTPROC('HYR2022')
     D  Order#                        8P 0 CONST
     D  Turn#                         9P 0 CONST
     D  ShipVia                       2A   CONST
     D  Pro#                         30A   CONST
     D  ShutDown                      1N   CONST
     D  RtnCode                       2A

     D HYR0148         PR                  EXTPROC('HYR0148')                   Add 856 tickler rcd
     D  BOL#                          8P 0 CONST
     D  ShipTo#                       7P 0 CONST
     D  PriorBOL#                     8P 0 CONST
     D  ShutDown                      1N   CONST

     D*HYR0149         PR                  EXTPROC('HYR0149')                   Add 211 tickler rcd
     D* BOL#                          8P 0 CONST
     D* ShipVia                       2A   CONST
     D* ShutDown                      1N   CONST

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

     D CSYRLO          PR                  EXTPROC('CSYRLO')                    Process record lock
     D  FileName                     10A   CONST
     D  LockCount                     1P 0

      **************************************************************************
      * Define *Entry parameter list.                                          *
      **************************************************************************
     D HYR2022         PI
     D  in_Order#                     8P 0 CONST
     D  in_Turn#                      9P 0 CONST
     D  in_ShipVia                    2A   CONST
     D  in_Pro#                      30A   CONST
     D  in_ShutDown                   1N   CONST
     D  out_RtnCode                   2A

      **************************************************************************
      * Define arrays.                                                         *
      **************************************************************************
     D                 DS
     D BIFCCQ                  1    140
     D FCCQx                               DIM(10) OVERLAY(BIFCCQ)              Frt cls case qty
     D  FCCQ_FTCL                          LIKE(BLFTCL) OVERLAY(FCCQx:1)
     D  FCCQ_CSQT                     5P 0 OVERLAY(FCCQx:6)
     D  FCCQ_WGT                      7P 0 OVERLAY(FCCQx:9)
     D  FCCQ_PLLT                     3P 0 OVERLAY(FCCQx:13)

     D idx                            3P 0

      **************************************************************************
      * Define data structures.                                                *
      **************************************************************************
     D                 DS
     D td_TimeDate             1     14  0
     D td_Time                 1      6  0
     D td_Date                 7     14  0

     D                 DS
     D ds_CYMD                 2      8  0
     D ds_MDYY                 5     12  0
     D ds_YYYY1                1      4  0
     D ds_YYYY2                9     12  0

      **************************************************************************
      * Define data areas.                                                     *
      **************************************************************************
     D wk_DatabaseID   S              2A   DTAARA(DATABASEID)                   Database ID

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Order#       S                   LIKE(OEORD#)
     D wk_Turn#        S              9P 0
     D wk_ShipVia      S                   LIKE(SVSVSV)
     D wk_Pro#         S                   LIKE(SDPRO#)
     D wk_ShutDown     S              1N
     D wk_RtnCode      S              2A
     D wk_SysCYMD      S              7P 0
     D wk_SysTime      S              6P 0
     D wk_PriorBOL#    S                   LIKE(BIBOL#)
     D wk_EDI856       S              1N
     D wk_BOL#         S                   LIKE(SDBOL#)
     D wk_SSCCFnd      S              1N
     D wk_DCWhs        S              3P 0
     D wk_Pgm01        S              1N   INZ(*OFF)
     D**** wk_Pgm02        S              1N   INZ(*OFF)
     D kf_TranSetID    S                   LIKE(EO0143) INZ('856')
     D kf_VCF          S                   LIKE(DSVCF) INZ('C')
     D kf_Invoice#     S                   LIKE(BIAIV#) INZ(*Zero)
     D RLCNT           S              1P 0
     D wk_MsgID        S              7A
     D wk_MsgData      S            512A
     D wk_MsgDataLen   S              3P 0
     D in_MsgKey       S              4A
     D io_MsgErrCode   S             10I 0

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D Yes             C                   'Y'
     D No              C                   'N'
     D CentralTime     C                   'CT'
     D Cases           C                   'CS'
     D Pounds          C                   'LB'
     D HazYes          C                   ' X'
     D HazRptQty       C                   'RQ'
     D Shipped         C                   'S'
     D DCWhs           C                   6
     D DCWhsSS         C                   955

     C/EJECT
      **************************************************************************
      * Main routine.                                                          *
      **************************************************************************

     C                   EVAL      wk_Order# = in_Order#
     C                   EVAL      wk_Turn# = in_Turn#
     C                   EVAL      wk_ShipVia = in_ShipVia
     C                   EVAL      wk_Pro# = in_Pro#
     C                   EVAL      wk_Shutdown = in_ShutDown
     C                   EVAL      wk_RtnCode = *Zeros

     C                   SELECT

     C                   WHEN      wk_ShutDown
     C                   EXSR      $ShutDownPgms
     C                   EVAL      *INLR = *ON

     C                   OTHER
     C                   EXSR      $WrtBOL

     C                   ENDSL

     C                   EVAL      out_RtnCode = wk_RtnCode
     C                   RETURN

     C/EJECT
      **************************************************************************
      * Write the bill of lading data.                                         *
      **************************************************************************
     CSR   $WrtBOL       BEGSR

      *
      * Retrieve master records.
      *
     C     wk_Order#     CHAIN     OEORHD
     C                   IF        not %found(OEORHD)
     C                   EVAL      wk_RtnCode = '01'
     C                   LEAVESR
     C                   ENDIF                                                  not %found(OEORHD)

     C     OESHTO        CHAIN     HDCUST
     C                   IF        not %found(HDCUST)
     C                   EVAL      wk_RtnCode = '02'
     C                   LEAVESR
     C                   ENDIF                                                  not %found(HDCUST)

     C                   IF        OEDSHP <>  *Zero
     C     key_DS        CHAIN     HDDSHP
     C                   IF        not %found(HDDSHP)
     C                   EVAL      wk_RtnCode = '03'
     C                   LEAVESR
     C                   ENDIF                                                  not %found(HDDSHP)
     C                   ENDIF                                                  OEDSHP<>=*Zero

     C     wk_ShipVia    CHAIN     HDSHPV
     C                   IF        not %found(HDSHPV)
     C                   EVAL      SVSVDS = *Blanks
     C                   EVAL      SVSCAC = *Blanks
     C                   EVAL      SVBOLR = No
     C                   ENDIF                                                  not %found(HDSHPV)

     C     key_BI        CHAIN(E)  HYPBOL
     C                   IF        %error
     C                   EVAL      wk_RtnCode = '04'
     C                   LEAVESR
     C                   ENDIF                                                  %error
     C                   IF        not %found(HYPBOL)
     C                   CLEAR                   HYRBOL
     C                   EVAL      FCCQ_FTCL = *Blanks
     C                   EVAL      FCCQ_CSQT = *Zero
     C                   EVAL      FCCQ_WGT = *Zero
     C                   EVAL      FCCQ_PLLT = *Zero
     C                   EVAL      BIBOL# =  *Zero
     C                   ENDIF                                                  not %found(HYPBOL)
     C                   IF        BIBOL# <> *Zero
     C                   EVAL      wk_RtnCode = '05'
     C                   ENDIF                                                  BIBOL#<>*Zero
     C                   EVAL      wk_PriorBOL# = BIBOL#

      *
      * Determine whether customer is set up for ASN in the EDI outbound
      * trading partner master file.
      *
     C     key_EO        SETLL     EDOTPM
     C                   IF        %equal(EDOTPM)
     C                   EVAL      wk_EDI856 = *ON
     C                   ELSE
     C                   EVAL      wk_EDI856 = *OFF
     C                   ENDIF                                                  %equal(EDOTPM)

      *
      * Retrieve and format system date and time.
      *
     C                   TIME                    td_TimeDate
     C                   EVAL      ds_MDYY = td_Date
     C                   EVAL      ds_YYYY1 = ds_YYYY2 - 1900
     C                   EVAL      wk_SysCYMD = ds_CYMD
     C                   EVAL      wk_SysTime = td_Time

      *
      * Retrieve next bill of lading number.
      *
     C                   EXSR      $RtvNxtBOL#

      *
      * Build Bill of Lading Header Master File (OEBOLH) record.
      *
     C                   EXSR      $BldBOLH

      *
      * Build Bill of Lading Detail Master File (OEBOLD) records.
      *
     C                   EXSR      $BldBOLD

      *
      * Write/Update HYPBOL record.
      *
     C                   EXSR      $WriteBOL

      *
      * Update the packing list header master record with the assigned
      * bill-of-lading number.
      *
     C     wk_Order#     SETGT     OEPKLD03
     C                   DOU       %eof(OEPKLD03)  or  PLTURN = wk_Turn#
     C     wk_Order#     READPE    OEPKLD03
     C                   ENDDO                                                  %eof(OEPKLD03) or
     C                   IF        not %eof(OEPKLD03)
     C     PLPKL#        CHAIN     OEPKLH
     C                   IF        %found(OEPKLH)
     C                   IF        PHBOL# <> *Zero
     C                   EVAL      wk_MsgID = 'ERR9999'
     C                   EVAL      wk_MsgData = 'Order# ' +
     C                               %triml(%editc(wk_Order#:'Z')) + ' turn# ' +
     C                               %triml(%editc(wk_Turn#:'Z')) +
     C                               ' packing list# ' +
     C                               %triml(%editc(PLPKL#:'Z')) +
     C                               ' already contained BOL# ' +
     C                               %triml(%editc(PHBOL#:'Z')) + '.'
     C                   EXSR      $SndPM
     C                   ENDIF                                                  PHBOL#<>*Zero
     C                   EVAL      PHBOL# = wk_BOL#
     C                   UPDATE    OEPKLHR
     C                   ENDIF                                                  %found(OEPKLH)
     C                   ELSE
     C                   EVAL      wk_MsgID = 'ERR9999'
     C                   EVAL      wk_MsgData = 'Order# ' +
     C                               %triml(%editc(wk_Order#:'Z')) + ' turn#' +
     C                               %triml(%editc(wk_Turn#:'Z')) +
     C                               ' packing list not found.'
     C                   EXSR      $SndPM
     C                   ENDIF                                                  not %eof(OEPKLD03)

      *
      * Update the HYPSSCC records with the assigned bill-of-lading
      * number.
      *
     C                   EVAL      wk_SSCCFnd = *OFF
     C     wk_Order#     SETLL     HYLSSCC2
     C                   DOU       %eof(HYLSSCC2)
     C     wk_Order#     READE     HYLSSCC2
     C                   IF        %eof(HYLSSCC2)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSSCC2)
     C****               IF        SCBOL# <> *Zero
     C****                           or  SCTURN <> wk_Turn#
     C                   IF        (SCBOL# <> *Zero  and
     C                               SCBOL# <> 99999999  and
     C                               SCBOL# <> wk_PriorBOL#)  or
     C                              SCTURN <> wk_Turn#
     C                   UNLOCK    HYLSSCC2
     C                   ITER
     C                   ENDIF                                                  SCBOL#<>*Zero
     C                   EVAL      SCBOL# = wk_BOL#
     C                   UPDATE    HYRSSCC
     C                   EVAL      wk_SSCCFnd = *ON
     C                   ENDDO                                                  %eof(HYLSSCC2)

      *
      * Add a record to the EDI 856 tickler file, if BOL is for EDI 856
      * customer.
      *
     C                   IF        wk_EDI856  and  wk_SSCCFnd
     C****               CALLP(E)  HYR0148 (wk_BOL#:OESHTO:*Zero:*OFF)
     C                   CALLP(E)  HYR0148 (wk_BOL#:OESHTO:wk_PriorBOL#:*OFF)
     C                   EVAL      wk_Pgm01 = *ON
     C                   ENDIF                                                  wk_EDI856 and

      *
      * Add a record to the EDI 211 tickler file, if BOL is for EDI 211
      * freight handler.
      *
     C****               CALLP(E)  HYR0149 (wk_BOL#:wk_ShipVia:*OFF)
     C****               EVAL      wk_Pgm02 = *ON

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Retrieve next bill of lading number.                                   *
      **************************************************************************
     C     $RtvNxtBOL#   BEGSR

     C                   EVAL      wk_BOL# = *Zero
     C                   DOW       wk_BOL# = *Zero
     C                   DOU       not %error
     C     1             CHAIN(E)  OECTRL
     C                   IF        %error
     C                   CALLP(E)  CSYRLO ('OECTRL':RLCNT)
     C                   ENDIF                                                  %error
     C                   ENDDO                                                  not %error
     C                   EVAL      RLCNT = *Zero
     C                   IF        %found(OECTRL)
     C                   EVAL      CELBL# = CELBL# + 1
     C                   UPDATE    OECTRLR
     C     CELBL#        SETLL     OEBOLHR
     C                   IF        not %equal(OEBOLH)
     C                   EVAL      wk_BOL# = CELBL#
     C                   ENDIF                                                  not %equal(OEBOLH)
     C                   ELSE
     C                   LEAVE
     C                   ENDIF                                                  %found(OECTRL)
     C                   ENDDO                                                  wk_BOL#=*Zero

     C                   ENDSR

     C/EJECT
      **************************************************************************
      * Build Bill of Lading Header Master File (OEBOLH) record.               *
      **************************************************************************
     C     $BldBOLH      BEGSR

     C                   EVAL      SDSTS = Shipped
     C                   EVAL      SDBOL# = wk_BOL#
     C                   EVAL      SDBOLP = No
     C****               EVAL      SDSHFM = DCWhs
     C                   EVAL      SDSHFM = wk_DCWhs
     C                   EVAL      SDSHTO = OESHTO
     C                   EVAL      SDDSHP = OEDSHP
     C                   IF        OEDSHP = *Zero
     C                   EVAL      SDNAME = CMCNA1
     C                   EVAL      SDADR1 = CMCNA2
     C                   EVAL      SDADR2 = CMCNA3
     C                   EVAL      SDADR3 = CMCNA4
     C                   EVAL      SDCITY = CMCCTY
     C                   EVAL      SDST = CMST
     C                   EVAL      SDZIP = CMZIP
     C                   EVAL      SDCTRY = CMCTRY
     C                   ELSE
     C                   EVAL      SDNAME = DSNAME
     C                   EVAL      SDADR1 = DSADR1
     C                   EVAL      SDADR2 = DSADR2
     C                   EVAL      SDADR3 = DSADR3
     C                   EVAL      SDCITY = DSCITY
     C                   EVAL      SDST = DSST
     C                   EVAL      SDZIP = DSZIP
     C                   EVAL      SDCTRY = DSCTRY
     C                   ENDIF                                                  OEDSHP=*Zero
     C                   EVAL      SDBLTO = OEBLTO
     C                   EVAL      SDSHPV = wk_ShipVia
     C                   EVAL      SDSVDS = SVSVDS
     C                   EVAL      SDCRCD = *Blanks
     C                   EVAL      SDSCAC = SVSCAC
     C                   EVAL      SDPRO# = wk_PRO#
     C                   EVAL      SDSHDT = wk_SysCYMD
     C                   EVAL      SDSTIM = wk_SysTime
     C                   EVAL      SDBOLR = SVBOLR
     C                   IF        wk_EDI856
     C                   EVAL      SDASNG = Yes
     C                   ELSE
     C                   EVAL      SDASNG = No
     C                   ENDIF                                                  wk_EDI856
     C                   EVAL      SDTPKG = %xfoot(FCCQ_CSQT)
     C                   EVAL      SDTWGT = %xfoot(FCCQ_WGT)
     C                   EVAL      SDCFPB = *Blank
     C                   EVAL      SDCODF = *Zero
     C                   EVAL      SDCODA = *Zero
     C                   EVAL      SDRMTO = *Zero
     C                   EVAL      SDEQCD = *Blanks
     C                   EVAL      SDEQIN = *Blanks
     C                   EVAL      SDEQNO = *Blanks
     C                   EVAL      SDERPH = *Zero
     C                   EVAL      SDBOLC = *Blank
     C                   EVAL      SDBHF1 = *Blank
     C                   EVAL      SDBHF2 = *Blank
     C                   EVAL      SDBHF3 = *Blank
     C                   EVAL      SDBHF4 = *Blank
     C                   EVAL      SDBHF5 = *Blank
     C                   EVAL      SDBHF6 = *Blank
     C                   EVAL      SDBHF7 = *Blank
     C                   EVAL      SDBHF8 = *Blank
     C                   EVAL      SDBCM1 = 'HYR2022'
     C                   EVAL      SDBCM2 = *Blanks
     C                   EVAL      SDBCM3 = *Blanks
     C                   EVAL      SDBCM4 = *Blanks
     C                   EVAL      SDBCM5 = *Blanks
     C                   EVAL      SDBCM6 = *Blanks
     C                   EVAL      SDBCM7 = *Blanks
     C                   EVAL      SDBCM8 = *Blanks
     C                   EVAL      SDCBTC = *Blanks
     C                   EVAL      SDSTZN = CentralTime
     C                   EVAL      SDESTD = wk_SysCYMD
     C                   EVAL      SDESTT = *Zero
     C                   EVAL      SDTRMC = *Blanks
     C                   EVAL      SDPPCD = *Blanks
     C                   EVAL      SDPPLC = *Blanks
     C                   EVAL      SDERSQ = *Blanks
     C                   EVAL      SDFOB = OEFOBC
     C                   EVAL      SDFRB# = *Blanks
     C                   EVAL      SDTRL# = *Blanks
     C                   EVAL      SDAWB# = *Blanks
     C                   EVAL      SDSEAL = *Blanks

     C                   WRITE     OEBOLHR

     C                   ENDSR
     C/EJECT
      **************************************************************************
      * Build Bill of Lading Detail Master File (OEBOLD) records.              *
      **************************************************************************
     C     $BldBOLD      BEGSR

      *
      * Retrieve freight class quantities and build detail record.
      *
     C                   FOR       idx = 1 to 10
     C                   IF        FCCQ_CSQT(idx) <> *Zero  or
     C                               FCCQ_WGT(idx) <> *Zero  or
     C                               FCCQ_PLLT(idx) <> *Zero
     C                   EVAL      BLBOL# = wk_BOL#
     C                   EVAL      BLFTCL = FCCQ_FTCL(idx)
     C     BLFTCL        CHAIN     HDFTCL
     C                   IF        %found(HDFTCL)
     C                   EVAL      BLFDS1 = FTLD01
     C                   EVAL      BLFDS2 = FTLD02
     C                   EVAL      BLFDS3 = FTLD03
     C                   ELSE
     C                   EVAL      BLFDS1 = *Blanks
     C                   EVAL      BLFDS2 = *Blanks
     C                   EVAL      BLFDS3 = *Blanks
     C                   EVAL      FTHAZM = No
     C                   ENDIF                                                  not %found(HDFTCL)
     C                   EVAL      BLPKGS = FCCQ_CSQT(idx)
     C                   EVAL      BLPKUM = Cases
     C                   EVAL      BLWGT = FCCQ_WGT(idx)
     C                   EVAL      BLGRSW = *Zero
     C                   EVAL      BLWTUM = Pounds
     C                   IF        FTHAZM = Yes
     C                   IF        FTRPQT = *Zero
     C                   EVAL      BLHAZM = HazYes
     C                   ELSE
     C                   EVAL      BLHAZM = HazRptQty
     C                   ENDIF                                                  FTRPQT=*Zero
     C                   ENDIF                                                  FTHAZM=Yes
     C                   EVAL      BLNMFC = FTNMFC
     C                   EVAL      BLUDN1 = FCCQ_PLLT(idx)
     C                   WRITE     OEBOLDR
     C                   ENDIF                                                  FCCQ_CSQT(idx)<>*Zer
     C                   ENDFOR                                                 idx=1 to 10

     C                   ENDSR

     C/EJECT
      **************************************************************************
      * Write/Update HYPBOL record.                                            *
      **************************************************************************
     CSR   $WriteBOL     BEGSR

     C                   IF        %found(HYPBOL)
     C                   EVAL      BIBOL# = wk_BOL#
     C                   UPDATE    HYRBOL
     C                   ELSE
     C                   EVAL      BIBOL# = wk_BOL#
     C                   EVAL      BIORD# = wk_Order#
     C                   EVAL      BITURN = wk_Turn#
     C                   EVAL      BIALPH = CMALPH
     C                   EVAL      BISHTO = OESHTO
     C                   EVAL      BIDSHP = OEDSHP
     C                   EVAL      BISVSV = wk_ShipVia
     C                   EVAL      BISVDS = SVSVDS
     C                   EVAL      BIPRO# = wk_Pro#
     C                   EVAL      BIPLLT = %xfoot(FCCQ_PLLT)
     C                   EVAL      BIWGT = %xfoot(FCCQ_WGT)
     C                   WRITE     HYRBOL
     C                   ENDIF                                                  %found(HYPBOL)

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

     C/EJECT
      **************************************************************************
      * Shut down called programs.                                             *
      **************************************************************************
     CSR   $ShutDownPgms BEGSR

     C                   IF        wk_Pgm01
     C                   CALLP(E)  HYR0148 (*Zero:*Zero:*Zero:*ON)
     C                   ENDIF                                                  wk_Pgm01

     C****               IF        wk_Pgm02
     C****               CALLP(E)  HYR0149 (*Zero:*Blanks:*ON)
     C****               ENDIF                                                  wk_Pgm02

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Program initialization.                                                *
      **************************************************************************
     CSR   *InzSR        BEGSR

      *
      * Define key lists.
      *
     C     key_EO        KLIST                                                  EDOTPM
     C                   KFLD                    kf_TranSetID
     C                   KFLD                    OESHTO

     C     key_DS        KLIST                                                  HDDSHP
     C                   KFLD                    kf_VCF
     C                   KFLD                    OESHTO
     C                   KFLD                    OEDSHP

     C     key_BI        KLIST                                                  HYPBOL
     C                   KFLD                    wk_Order#
     C                   KFLD                    wk_Turn#
     C                   KFLD                    kf_Invoice#

      *
      * Retrieve current database ID.
      *
     C                   IN        wk_DatabaseID

      *
      * Determine DC warehouse number.
      *
     C                   IF        wk_DatabaseID = 'SS'  or
     C                               wk_DatabaseID = 'SX'
     C                   EVAL      wk_DCWhs = DCWhsSS
     C                   ELSE
     C                   EVAL      wk_DCWhs = DCWhs
     C                   ENDIF                                                  wk_DatabaseID='SS'

     CSR                 ENDSR

     C/EJECT
