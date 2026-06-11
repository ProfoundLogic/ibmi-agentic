     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE N6181R - CALCULATE BILLING AMT IN T/M ENTRY
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*   THIS OPTION WILL CALL BILLING TYPE PROGRAM                  *
     H*                                                               *
     H*  PROGRAMMER: C:20               PROJECT: 68MAP                *
     H*  CREATED: 07/31/95                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                              **
     H*  10/05/99 C:62  PROJECT: MC212                               **
     H*                 NEED TO FIND OUT IF THERE IS ANY B AMT.      **
     H*                                                               *
     H*  03/18/99 C:94  PROJECT: 25MKL
     H*                 ADD RECON NUMBER
     H*
     H*  03/09/99 C:94  PROJECT: MC152                                *
     H*                 CHANGE FDCAL2 TO FDCALL AND CHANGE KLIST      *
     H*                                                               *
     H*  01/04/99 C:94  PROJECT: MC152                               **
     H*                 CHANGE PROGRAM TO USE FAEAP INSTEAD OF       **
     H*                 FEO1P, FAHBP, NATRP, NAMBP, AND NAPBP        **
     H*                 ALSO TO USE API(AA001R) FOR ANY UPDATE       **
     H*                                                              **
     H*  10/26/98 C:62  Project: MC145                                *
     H*                 REMOVE THE EXCPT STATEMENTS AFTER ELSES.      *
     H*                                                               *
     H*  02/18/98 C:62  Project: MC159                                *
     H*                 1) CHANGED NMPHL & NMHSL WITH FMS4L2/L3.      *
     H*                 2) IF XXTO EQ *ZEROS, THEN MAKE IT 9999999.   *
     H*                                                               *
     H*****************************************************************
     F***
     FFAEOL4    UF   E           K DISK
     F*** BEGIN CHG *** 02/18/98 **************************************
     F********** NMHSL   IF  E           K        DISK
     F********** NMPHL   IF  E           K        DISK
     FFMS4L2    IF   E           K DISK
     FFMS4L3    IF   E           K DISK
     F                                     RENAME(PS401:PS402)
     F*** END   CHG *** 02/18/98 **************************************
     FFMPDL     IF   E           K DISK
     FNMBDLB    IF   E           K DISK
     FFMFNL     IF   E           K DISK
     F*** BEG CHG *** 03/09/99 **************************************
     FFDCALL    IF   E           K DISK
     F**********FDCAL2  IF  E           K        DISK
     F*** END CHG *** 03/09/99 **************************************
     FFMCML1    UF   E           K DISK
     F*** BEG ADD *** 01/04/99 **************************************
     F                                     INFDS(DBFDS)
     F*** END ADD *** 01/04/99 **************************************
     F*** BEGIN ADD *** 10/05/99 **************************************
     FFAEAL1    IF   E           K DISK
     F*** END   ADD *** 10/05/99 **************************************
     F***

     D CMD             S              8    DIM(1) CTDATA PERRCD(1)
     D**
     D*** BEG ADD *** 01/04/99 ***************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D*  WRITE/UPDATE DATA STRUCTURE.
     D                 DS
     D  KKWRUP                 1      7
     D  XXFAEO                 1      1
     D  XXFAE1                 2      2
     D  XXFAHB                 3      3
     D  XXNAPB                 4      4
     D  XXNAMB                 5      5
     D  XXNATR                 6      6
     D  XXFAEA                 7      7
     D*
     D*** END ADD *** 01/04/99 ***************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D**
     D                 DS
     D  XXPGM                  1      8
     D  XXTYPE                 4      5
     D**  SYSTEM SUBSCRIBED TO :
     D                 DS
     D  FSY                    1     10
     D                                     DIM(10)                              GROUP SYS CODE
     D  FNSY01                 1      1
     D  FNSY02                 2      2
     D  FNSY03                 3      3
     D  FNSY04                 4      4
     D  FNSY05                 5      5
     D  FNSY06                 6      6
     D  FNSY07                 7      7
     D  FNSY08                 8      8
     D  FNSY09                 9      9
     D  FNSY10                10     10
     D** BILLING CODES FOR SYSTEM TYPES :
     D                 DS
     D  FBR                    1     30
     D                                     DIM(10)                              FILE BILL/RATE CD
     D  FNBR01                 1      3
     D  FNBR02                 4      6
     D  FNBR03                 7      9
     D  FNBR04                10     12
     D  FNBR05                13     15
     D  FNBR06                16     18
     D  FNBR07                19     21
     D  FNBR08                22     24
     D  FNBR09                25     27
     D  FNBR10                28     30
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*** BEGIN DEL *** 02/18/98 **************************************
     C*
     C*   KEY LIST FOR THE DOCTOR PROVIDER FILE
     C*
     C********** PHKEY     KLIST
     C**********           KFLD           EOFEIN
     C**********           KFLD           EOMOD
     C**********           KFLD           XXNWCD
     C**********           KFLD           EOTODT
     C*
     C*
     C*   KEY LIST FOR THE HOSPITAL PROVIDER FILE
     C*
     C********** HSKEY     KLIST
     C**********           KFLD           EOFEIN
     C**********           KFLD           EOMOD
     C**********           KFLD           XXNWCD
     C**********           KFLD           EOTODT
     C*** END   DEL *** 02/18/98 **************************************
     C*** BEGIN ADD *** 02/18/98 **************************************
     C*
     C*   KEY LIST FOR THE HOSPITAL PROVIDER FILE
     C*
     C     S4KEY         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C                   KFLD                    XXNWCD
     C                   KFLD                    EOTODT
     C*** END   ADD *** 02/18/98 **************************************
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C*
     C*  CASE DETAIL RECORDS
     C*
     C     CAKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C**********           KFLD           EOTRCD  2        DEL 03/09/99
     C                   KFLD                    EOEOB#
     C*
     C*  GROUP FILE - FMFNL
     C*
     C     FNKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C*
     C*  BILLING CODE TABLE FILE
     C*
     C     BDKEY         KLIST
     C                   KFLD                    XXCODE
     C                   KFLD                    XXDATE            7 0
     C*
     C* CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*
     C* EOMB FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    X1CO#
     C                   KFLD                    X1SUB#
     C                   KFLD                    X1FND
     C                   KFLD                    X1FYR
     C                   KFLD                    X1CASE
     C                   KFLD                    X1EOB#
     C*
     C* ENTRY PARMS
     C*** BEGIN ADD *** 10/05/99 **************************************
     C*
     C     EAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C*** END   ADD *** 10/05/99 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQEOB#            9
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *LIKE         DEFINE    BDAM01        XXAM01                          MIN. BILL AMT.
     C     *LIKE         DEFINE    EOSUB#        OLDSUB                          ADD 01/07/94
     C     *LIKE         DEFINE    EOFND         OLDFND                          ADD 01/07/94
     C     *LIKE         DEFINE    EOCO#         X1CO#
     C     *LIKE         DEFINE    EOSUB#        X1SUB#
     C     *LIKE         DEFINE    EOFND         X1FND
     C     *LIKE         DEFINE    EOFYR         X1FYR
     C     *LIKE         DEFINE    EOCASE        X1CASE
     C     *LIKE         DEFINE    EOEOB#        X1EOB#
     C*** BEGIN CHG *** 02/18/98 **************************************
     C********** *LIKE     DEFN PHNEDT    XXFROM
     C********** *LIKE     DEFN PHNXDT    XXTO
     C     *LIKE         DEFINE    S4BGDT        XXFROM
     C     *LIKE         DEFINE    S4ENDT        XXTO
     C*** END   CHG *** 02/18/98 **************************************
     C                   MOVE      *BLANKS       XXNWCD            5
     C                   Z-ADD     *ZEROS        X                 5 0
     C*
     C                   MOVE      QQCO#         X1CO#
     C                   MOVE      QQSUB#        X1SUB#
     C                   MOVE      QQFND         X1FND
     C                   MOVE      QQFYR         X1FYR
     C                   MOVE      QQCASE        X1CASE
     C                   MOVE      QQEOB#        X1EOB#
     C*
     C*  GET THE CURRENT DATE TO USE FOR KEY TO BILLING MASTER
     C*
     C                   Z-ADD     UDATE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*** BEG ADD *** 03/18/99 ***************************************
     C*
     C* FIND OUT IF PROVIDER BILLS IS INSTALLED
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C*
     C*** END ADD *** 03/18/99 ***************************************
     C*
     C     EOKEY         CHAIN     PEO01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*** CHECK IF FUND HAS CHANGED, YES MEANS BILLING CODE MAY CHANGE
     C*
     C     EOSUB#        IFNE      OLDSUB                                       ADD 01/07/94
     C     EOFND         ORNE      OLDFND
     C                   EXSR      FUND
     C                   END
     C*
     C*
     C*** CLEAR WORK AMT FIELDS
     C*
     C                   MOVE      *ZEROS        QQBAMT
     C                   MOVE      *ZEROS        XXBAMT
     C                   MOVE      *ZEROS        XXAMT
     C*** BEGIN ADD *** 10/04/99 **************************************
     C*
     C* IS PROVIDER BILLS INSTALLED
     C*
     C     ##PRVB        IFEQ      'Y'
     C*
     C* IS THERE A NETWORK DISCOUNT
     C*
     C     EONDIS        IFGT      *ZEROS
     C*
     C* DO WE PROCESS FOR THE PARTICULAR SYSTEM AS WELL
     C*
     C     BDPC25        IFEQ      2
     C*
     C                   EXSR      EAP
     C*
     C                   ENDIF
     C*
     C* CARRY OUT THE NETWORK CALCULATIONS
     C*
     C                   EXSR      NET
     C*
     C                   ELSE
     C*
     C                   EXSR      EAP
     C*
     C                   ENDIF
     C*
     C* PROVIDER BILLS IS NOT INSTALLED
     C*
     C                   ELSE
     C*** END   ADD *** 10/04/99 **************************************
     C*
     C* IS THERE A NETWORK DISCOUNT
     C*
     C     EONDIS        IFGT      *ZEROS
     C*
     C* DO WE PROCESS FOR THE PARTICULAR SYSTEM AS WELL
     C*
     C     BDPC25        IFEQ      2
     C*
     C     EOSYS         IFEQ      'P'
     C                   EXSR      PBR
     C                   END                                                    EOSYS = P
     C*
     C     EOSYS         IFEQ      'H'
     C                   EXSR      HBA
     C                   END                                                    EOSYS = H
     C*
     C     EOSYS         IFEQ      'U'
     C                   EXSR      CRT
     C                   END                                                    EOSYS = U
     C*
     C     EOSYS         IFEQ      'M'
     C                   EXSR      MED
     C                   END                                                    EOSYS = M
     C*
     C                   END                                                    BDPC25 = 2
     C**
     C                   EXSR      NET
     C**
     C                   ELSE                                                   EONDIS LT 0
     C*
     C     EOSYS         IFEQ      'P'
     C                   EXSR      PBR
     C                   END                                                    EOSYS = P
     C*
     C     EOSYS         IFEQ      'H'
     C                   EXSR      HBA
     C                   END                                                    EOSYS = H
     C*
     C     EOSYS         IFEQ      'U'
     C                   EXSR      CRT
     C                   END                                                    EOSYS = U
     C*
     C     EOSYS         IFEQ      'M'
     C                   EXSR      MED
     C                   END                                                    EOSYS = M
     C*
     C                   END                                                    EONDIS GT 0
     C*** BEGIN ADD *** 10/04/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/04/99 **************************************
     C*
     C* POPULATE HOLD FIELDS FOR GROUP AND SUBSCRIBER TO INDICATE
     C* GROUP BREAKS.
     C*
     C                   Z-ADD     EOFND         OLDFND
     C                   Z-ADD     EOSUB#        OLDSUB                         ADD 01/07/94
     C*
     C* READ NEXT EOMB
     C*
     C     EOKEY         READE     PEO01                                  60
     C                   END                                                    60 = 0
     C*
     C                   SETON                                          LR
     C*
     C*************************************************
     C*  CALCULATE INVOICE AMT FOR NETWORK TYPE EOBS  *
     C*************************************************
     C*
     C     NET           BEGSR
     C*
     C*** DETERMINE IF GROUP & PROVIDER ARE PARTICIPATING IN THE
     C*** NETWORK PROGRAM -- IF NOT, DO NOT CALL PGMS.
     C*
     C     PDKEY         CHAIN     PPD01                              63
     C     *IN63         IFEQ      '0'
     C*
 B2  C     PDPARN        IFEQ      'P'
     C*
     C*  DETERMINE IF SERVICE DATES ARE WITHIN PROVIDERS DATE RANGE ...
     C*
     C                   EXSR      SRPROV                                                   VDS
     C*
     C*                                                    ##K 12/06/89
     C     XNTYPE        IFNE      *BLANKS                                      NTWRK GRP
     C     MATCH         ANDEQ     'Y'                                          WITHIN
     C*                                                    CONTRACT DTS
     C*                                                    ##K 12/06/89
     C*** CALL THE APPROPRIATE PGM TO CALCULATE THE NETWORK BILLING
     C*** AMT BASED ON THE BILLING TYPE IN BDTYPE.
     C*
     C                   MOVE      *ZEROS        QCO#              3
     C                   MOVE      *ZEROS        QSUB#             5
     C                   MOVE      *ZEROS        QFUND             3
     C                   MOVE      *ZEROS        QFYR              3
     C                   MOVE      *ZEROS        QCASE             7
     C                   MOVE      *ZEROS        QFEIN             9
     C                   MOVE      *ZEROS        QMOD              3
     C                   MOVE      *ZEROS        QEOB#             9
     C                   MOVE      *ZEROS        QQBAMT            9
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   MOVE      *ZEROS        QRCN              3
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C                   MOVE      EOCO#         QCO#              3
     C                   MOVE      EOSUB#        QSUB#             5
     C                   MOVE      EOFND         QFUND             3
     C                   MOVE      EOFYR         QFYR              3
     C                   MOVE      EOCASE        QCASE             7
     C                   MOVE      EOFEIN        QFEIN             9
     C                   MOVE      EOMOD         QMOD              3
     C                   MOVE      EOEOB#        QEOB#
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   MOVE      EORCN         QRCN              3
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C** Call N93CL to GET PROGRAM BASED ON TYPE ENTERED.
     C                   MOVEL     CMD(1)        XXPGM
     C                   MOVE      XNTYPE        XXTYPE            2
     C*
     C                   CALL      'N93CL'
     C                   PARM                    XXPGM
     C                   PARM                    QCO#
     C                   PARM                    QSUB#
     C                   PARM                    QFUND
     C                   PARM                    QEOB#
     C                   PARM                    QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   PARM                    QFYR
     C                   PARM                    QCASE
     C                   PARM                    QRCN
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C                   MOVE      QQBAMT        XXBAMT            9 2
     C*
     C*
     C*** BEG CHG *** 01/04/99 ***************************************
     C*** UPDATE NETWORK TRANS FILE RECORD
     C     CMKEY         CHAIN(N)  PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN         ##RRN
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       KKWRUP
     C*** BEGIN ADD *** 07/26/99 **************************************
     C*
     C* INDICATES WHAT FILE IS TO BE UPDATED BY AA001R
     C* XXFAEA = 'U' WILL UPDATE FAEAP
     C* XXNATR = 'U' WILL UPDATE NATRP
     C*
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   MOVE      'U'           XXFAEA
     C*
     C                   ELSE
     C*** END   ADD *** 07/26/99 **************************************
     C                   MOVE      'U'           XXNATR
     C*** BEGIN ADD *** 07/26/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 07/26/99 **************************************
     C*
     C* IF THERE IS NO AMOUNTS, THEN THE NETWORK BILLING FLAG WILL
     C* BE SET SO THAT IT WILL NOT BE PICKED UP FOR INVOICING
     C*
     C     XXBAMT        IFEQ      *ZEROS
     C                   MOVE      'P'           ##NBLF
     C*** BEGIN ADD *** 07/26/99 **************************************
     C                   MOVEL     *BLANKS       ##BBLF
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       ##NBLF
     C                   MOVEL     *BLANKS       ##BBLF
     C*** END   ADD *** 07/26/99 **************************************
     C                   ENDIF
     C*
     C* UPDATE FAEAP OR NATRP
     C*
     C                   CALL      'AA001R'
     C                   PARM                    ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/18/99 ***************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/18/99 ***************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM                    ##BAMT            9 2          BRS AMOUNT
     C                   PARM      XXBAMT        ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM      'R'           ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C*** END CHG *** 01/04/99 ***************************************
     C*
     C*
     C*
     C                   END                                                    XNTYPE &
     C*                                                    MATCH
     C                   END                                                    PDPARN=Y
     C*
     C*** IF THE NETWORK BILLING AMOUNT (TRBAMT) IS ZERO, AND THE BILLING
     C*** FLAG IS SUCH THAT ONLY ONE BILLING AMOUNT SHOULD BE CALCULATED
     C*** THEN CALCULATE PBR OR HBA BILLING AMOUNT BASED ON SYSTEM TYPE
     C*** BEG CHG *** 01/04/99 ***************************************
     C     XXBAMT        IFEQ      *ZERO
     C********** TRBAMT    IFEQ *ZERO
     C*** END CHG *** 01/04/99 ***************************************
     C     BDPC25        ANDEQ     1
     C*** BEGIN ADD *** 07/26/99 **************************************
     C*
     C* IS PROVIDER BILLS INSTALLED
     C*
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   EXSR      EAP
     C*
     C* PROVIDER BILLS IS NOT INSTALLED
     C*
     C                   ELSE
     C*** END   ADD *** 07/26/99 **************************************
     C*
     C     EOSYS         IFEQ      'P'
     C                   EXSR      PBR
     C                   END                                                    EOSYS = P
     C*
     C     EOSYS         IFEQ      'U'
     C                   EXSR      CRT
     C                   END                                                    EOSYS = U
     C*
     C     EOSYS         IFEQ      'H'
     C                   EXSR      HBA
     C                   END                                                    EOSYS = H
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C     EOSYS         IFEQ      'M'
     C                   EXSR      MED
     C                   END                                                    EOSYS = M
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*** BEGIN ADD *** 07/26/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 07/26/99 **************************************
     C*
     C                   END                                                    ENDEQ 0/1
     C*
     C                   END                                                    63 = 0
     C*
     C                   ENDSR
     C*****************************************************************
     C*
     C*****************************************************************
     C*  CRT SUBROUTINE: CALCULATE INVOICE AMT FOR PRE/RE-CERT EOMBS  *IL.
     C*****************************************************************
     C     CRT           BEGSR
     C*
     C*                                                                89
     C                   MOVE      *ZEROS        XXCO#
     C                   MOVE      *ZEROS        XXSUB#
     C                   MOVE      *ZEROS        XXGRP#
     C                   MOVE      *ZEROS        XXEO#
     C                   MOVE      *ZEROS        QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   MOVE      *ZEROS        XFYR              3
     C                   MOVE      *ZEROS        XCASE             7
     C                   MOVE      *ZEROS        XRCN              3
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C*** DETERMINE THE PROGRAM TO CALL TO PROCESS REQUEST
     C*
     C*
     C     XUTYPE        IFNE      *BLANKS
     C*
     C** Call N93CL to GET PROGRAM BASED ON TYPE ENTERED.
     C                   MOVEL     CMD(1)        XXPGM
     C                   MOVE      XUTYPE        XXTYPE            2
     C                   MOVE      EOCO#         XXCO#             3
     C                   MOVE      EOSUB#        XXSUB#            5
     C                   MOVE      EOFND         XXGRP#            3
     C                   MOVE      EOEOB#        XXEO#             9
     C                   MOVE      *ZEROS        QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   MOVE      EOFYR         XFYR
     C                   MOVE      EOCASE        XCASE
     C                   MOVE      EORCN         XRCN
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C*
     C                   CALL      'N93CL'
     C                   PARM                    XXPGM             8
     C                   PARM                    XXCO#
     C                   PARM                    XXSUB#
     C                   PARM                    XXGRP#
     C                   PARM                    XXEO#
     C                   PARM                    QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   PARM                    XFYR
     C                   PARM                    XCASE
     C                   PARM                    XRCN
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C                   END                                                    XHTYPE
     C*
     C*** UPDATE PRE/RE-CERT TRANS FILE RECORD
     C*
     C*** BEG CHG *** 01/04/99 ***************************************
     C*
     C* GET THE RELATIVE RECORD NUMBER OF THE CASE RECORD TO PASS
     C* IT TO AA001R.
     C*
     C     CMKEY         CHAIN(N)  PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN         ##RRN
     C                   ENDIF
     C*
     C* UPDATE THE BILLING FLAG SO THAT THE EOMB WILL NOT SHOW UP
     C* FOR INVOICING UNDER THESE CONDITIONS:
     C* 1) QQBAMT IF EQUALS ZERO
     C* 2) IF THERE AREN'T ANY T/M'S, EOFLG5 NOT EQUAL TO 'Y'
     C*
     C     QQBAMT        IFEQ      *ZEROS
     C*** BEGIN ADD *** 09/23/99 **************************************
     C     EOFLG5        IFNE      'Y'
     C*** END   ADD *** 09/23/99 **************************************
     C                   MOVE      'P'           EOBLFL
     C*** BEGIN ADD *** 07/26/99 **************************************
     C*** BEGIN ADD *** 09/23/99 **************************************
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       EOBLFL
     C*
     C                   ENDIF
     C*** END   ADD *** 09/23/99 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       EOBLFL
     C*** END   ADD *** 07/26/99 **************************************
     C                   ENDIF
     C*
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           MOVE QQBAMT    ##BAMT           BRS AMOUNT
     C                   MOVE      QQBAMT        ##TMAA                         BRS AMOUNT
     C*** END   CHG *** 09/23/99 **************************************
     C                   MOVE      QQBAMT        ##BCAM                         BRS CALC AMOUNT
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXNAPB
     C*
     C* UPDATE VALUES IN NAPBP BY CALLING AA001R
     C*
     C                   CALL      'AA001R'
     C                   PARM                    ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/18/99 ***************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/18/99 ***************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           PARM           ##BAMT  92       BRS AMOUNT
     C                   PARM      *ZEROS        ##BAMT            9 2          BRS AMOUNT
     C*** END   CHG *** 09/23/99 **************************************
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C*** END CHG *** 01/04/99 ***************************************
     C*
     C* UPDATE EOSTAT WITH A 'P' SO THAT IT WILL NOT BE PROCESSED
     C* AGAIN BY THIS PROGRAM.
     C*
     C                   MOVE      'P'           EOSTAT
     C                   UPDATE    PEO01
     C*
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*  HBA SUBROUTINE: CALCULATE INVOICE AMT FOR HBA TYPE EOBS.     *IL.
     C*****************************************************************
     C     HBA           BEGSR
     C*
     C*                                                                89
     C                   MOVE      *ZEROS        XXCO#
     C                   MOVE      *ZEROS        XXSUB#
     C                   MOVE      *ZEROS        XXGRP#
     C                   MOVE      *ZEROS        XXEO#
     C                   MOVE      *ZEROS        QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   MOVE      *ZEROS        XFYR              3
     C                   MOVE      *ZEROS        XCASE             7
     C                   MOVE      *ZEROS        XRCN              3
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C*** DETERMINE THE PROGRAM TO CALL TO PROCESS REQUEST
     C*
     C*
     C     XHTYPE        IFNE      *BLANKS
     C*
     C** Call N93CL to GET PROGRAM BASED ON TYPE ENTERED.
     C                   MOVEL     CMD(1)        XXPGM
     C                   MOVE      XHTYPE        XXTYPE            2
     C                   MOVE      EOCO#         XXCO#             3
     C                   MOVE      EOSUB#        XXSUB#            5
     C                   MOVE      EOFND         XXGRP#            3
     C                   MOVE      EOEOB#        XXEO#             9
     C                   MOVE      *ZEROS        QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   MOVE      EOFYR         XFYR              3
     C                   MOVE      EOCASE        XCASE             7
     C                   MOVE      EORCN         XRCN              3
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C*
     C                   CALL      'N93CL'
     C                   PARM                    XXPGM             8
     C                   PARM                    XXCO#
     C                   PARM                    XXSUB#
     C                   PARM                    XXGRP#
     C                   PARM                    XXEO#
     C                   PARM                    QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   PARM                    XFYR
     C                   PARM                    XCASE
     C                   PARM                    XRCN
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C                   END                                                    XHTYPE
     C*
     C*** UPDATE HBA TRANS FILE RECORD
     C*
     C*** BEG CHG *** 01/04/99 ***************************************
     C*
     C* GET THE RELATIVE RECORD NUMBER OF THE CASE RECORD TO PASS
     C* IT TO AA001R.
     C*
     C     CMKEY         CHAIN(N)  PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN         ##RRN
     C                   ENDIF
     C*
     C* UPDATE THE BILLING FLAG SO THAT THE EOMB WILL NOT SHOW UP
     C* FOR INVOICING UNDER THESE CONDITIONS:
     C* 1) QQBAMT IF EQUALS ZERO
     C* 2) IF THERE AREN'T ANY T/M'S, EOFLG5 NOT EQUAL TO 'Y'
     C*
     C     QQBAMT        IFEQ      *ZEROS
     C*** BEGIN ADD *** 09/23/99 **************************************
     C     EOFLG5        IFNE      'Y'
     C*** END   ADD *** 09/23/99 **************************************
     C                   MOVE      'P'           ##BBLF
     C*** BEGIN ADD *** 07/26/99 **************************************
     C*** BEGIN ADD *** 09/23/99 **************************************
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       EOBLFL
     C*
     C                   ENDIF
     C*** END   ADD *** 09/23/99 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       EOBLFL
     C*** END   ADD *** 07/26/99 **************************************
     C                   ENDIF
     C*
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           MOVE QQBAMT    ##BAMT           BRS AMOUNT
     C                   MOVE      QQBAMT        ##TMAA                         BRS AMOUNT
     C*** END   CHG *** 09/23/99 **************************************
     C                   MOVE      QQBAMT        ##BCAM                         BRS CALC AMOUNT
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXFAHB
     C*
     C* UPDATE VALUES IN FAHBP BY CALLING AA001R
     C*
     C                   CALL      'AA001R'
     C                   PARM                    ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/18/99 **************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/18/99 **************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           PARM           ##BAMT  92       BRS AMOUNT
     C                   PARM      *ZEROS        ##BAMT            9 2          BRS AMOUNT
     C*** END   CHG *** 09/23/99 **************************************
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*** END CHG *** 01/04/99 ****************************************
     C*
     C                   ENDSR
     C*
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*****************************************************************
     C*  MED SUBROUTINE: CALCULATE INVOICE AMT FOR MED SET-UP         *
     C*****************************************************************
     C     MED           BEGSR
     C*
     C* GET THE RELATIVE RECORD NUMBER OF THE CASE RECORD TO PASS
     C* IT TO AA001R AND DETERMINE IF THIS CASE HAS ALREADY BEEN
     C* PROCESSED ONCE BEFORE.
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* IF CMSETU IS NOT EQUAL TO 'Y', THEN IT HASN'T BEEN PROCESSED
     C* YET.  ANY SUBSEQUENT EOMBS OFF OF THIS CASE WILL NOT GO THRU
     C* THIS PROCESS AGAIN('M' TYPES THAT IS).
     C*
     C     CMSETU        IFNE      'Y'
     C*                                                                89
     C                   MOVE      *ZEROS        XXCO#
     C                   MOVE      *ZEROS        XXSUB#
     C                   MOVE      *ZEROS        XXGRP#
     C                   MOVE      *ZEROS        XXEO#
     C                   MOVE      *ZEROS        QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   MOVE      *ZEROS        XFYR              3
     C                   MOVE      *ZEROS        XCASE             7
     C                   MOVE      *ZEROS        XRCN              3
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C* DETERMINE THE PROGRAM TO CALL TO PROCESS REQUEST
     C*
     C     XMTYPE        IFNE      *BLANKS
     C*
     C* Call N93CL to GET PROGRAM BASED ON TYPE ENTERED.
     C*
     C                   MOVEL     CMD(1)        XXPGM
     C                   MOVE      XMTYPE        XXTYPE            2
     C                   MOVE      EOCO#         XXCO#             3
     C                   MOVE      EOSUB#        XXSUB#            5
     C                   MOVE      EOFND         XXGRP#            3
     C                   MOVE      EOEOB#        XXEO#             9
     C                   MOVE      *ZEROS        QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   MOVE      EOFYR         XFYR              3
     C                   MOVE      EOCASE        XCASE             7
     C                   MOVE      EORCN         XRCN              3
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C                   CALL      'N93CL'
     C                   PARM                    XXPGM             8
     C                   PARM                    XXCO#
     C                   PARM                    XXSUB#
     C                   PARM                    XXGRP#
     C                   PARM                    XXEO#
     C                   PARM                    QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   PARM                    XFYR
     C                   PARM                    XCASE
     C                   PARM                    XRCN
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C                   END                                                    XMTYPE
     C*
     C* UPDATE MEDICAL MANAGEMENT RECORD
     C*
     C*** BEG CHG *** 01/04/99 ***************************************
     C*** BEGIN DEL *** 09/23/99 **************************************
     C********** CMKEY     CHAINPCM01               N99
     C********** *IN99     IFEQ *OFF
     C**********           Z-ADDDBRRN     ##RRN
     C**********           ENDIF
     C*** END   DEL *** 09/23/99 **************************************
     C*** BEGIN ADD *** 09/23/99 **************************************
     C*
     C* UPDATE THE BILLING FLAG SO THAT THE EOMB WILL NOT SHOW UP
     C* FOR INVOICING UNDER THESE CONDITIONS:
     C* 1) QQBAMT IF EQUALS ZERO
     C* 2) IF THERE AREN'T ANY T/M'S, EOFLG5 NOT EQUAL TO 'Y'
     C*
     C     QQBAMT        IFEQ      *ZEROS
     C     EOFLG5        IFNE      'Y'
     C*
     C                   MOVE      'P'           EOBLFL
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       EOBLFL
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       EOBLFL
     C*
     C                   ENDIF
     C*** END   ADD *** 09/23/99 **************************************
     C*
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           MOVE QQBAMT    ##BAMT           BRS AMOUNT
     C                   MOVE      QQBAMT        ##TMAA                         BRS AMOUNT
     C*** END   CHG *** 09/23/99 **************************************
     C                   MOVE      QQBAMT        ##BCAM                         BRS CALC AMOUNT
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXNAMB
     C*
     C* UPDATE VALUES IN NAMBP BY CALLING AA001R
     C*
     C                   CALL      'AA001R'
     C*** BEGIN ADD *** 09/23/99 **************************************
     C**********           PARM           ##RRN   90       RRN OF CASE
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C*** END   ADD *** 09/23/99 **************************************
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/18/99 **************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/18/99 **************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           PARM           ##BAMT  92       BRS AMOUNT
     C                   PARM      *ZEROS        ##BAMT            9 2          BRS AMOUNT
     C*** END   CHG *** 09/23/99 **************************************
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*** END CHG *** 01/04/99 ***************************************
     C                   MOVE      'P'           EOSTAT
     C                   UPDATE    PEO01
     C*
     C                   MOVE      'Y'           CMSETU
     C                   UPDATE    PCM01
     C                   ELSE
     C*** BEGIN DEL *** 10/26/98 **************************************
     C**********           EXCPTPCM01E
     C*** END   DEL *** 10/26/98 **************************************
     C*** BEGIN ADD *** 10/26/98 **************************************
     C     EOFLG5        IFNE      'Y'
     C*
     C                   MOVEL     'P'           EOBLFL
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       EOBLFL
     C*
     C                   ENDIF
     C*** END   ADD *** 10/26/98 **************************************
     C                   MOVE      'P'           EOSTAT
     C                   UPDATE    PEO01
     C                   END                                                    END CMSETU
     C                   END                                                    END *IN60
     C*
     C                   ENDSR
     C*
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*****************************************************************
     C*  PBR SUBROUTINE: CALCULATE INVOICE AMT FOR PBR TYPE EOBS.     *
     C*****************************************************************
     C     PBR           BEGSR
     C*
     C                   MOVE      *ZEROS        QQBAMT                         10/17/89 ADD
     C*
     C*  GET THE CASE DETAIL CLAIM RECORD.
     C*
     C     CAKEY         CHAIN     PCA01                              52
     C*
     C*  TEST FOR A HOSPTIAL PROVIDER TYPE.
     C*
     C     CAPRTY        COMP      '4'                                    91
     C*
     C*  READ ALL THE CASE DETAIL RECORDS IN ORDER TO ACCUMULATE TOTAL
     C*
     C     *IN52         DOWEQ     '0'
     C*
     C                   ADD       CATOTC        TTABIL            9 2
     C                   ADD       CATOTD        TTARED            9 2
     C                   ADD       CATOTP        TTAALW            9 2
     C*
     C                   ADD       1             #LIN              5 0
     C*
     C*  READ THE NEXT CASE DETAIL RECORD FOR THIS CLAIM.
     C*
     C     CAKEY         READE     PCA01                                  52
     C*
     C                   END                                                    END DOWEQ 52
     C*
     C*** DETERMINE BILLING METHOD
     C                   Z-ADD     *ZERO         XX#LIN            5 0
     C*
     C*** CALL THE PROGRAM TO CALCULATE THE PBR BILLING AMT
     C*
     C     XPTYPE        IFNE      *BLANKS
     C*
     C                   MOVEL     CMD(1)        XXPGM
     C                   MOVE      XPTYPE        XXTYPE
     C                   MOVE      EOCO#         XXCO#
     C                   MOVE      EOSUB#        XXSUB#
     C                   MOVE      EOFND         XXGRP#
     C                   MOVE      EOEOB#        XXEO#
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   MOVE      EOFYR         XFYR
     C                   MOVE      EOCASE        XCASE
     C                   MOVE      EORCN         XRCN
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C*
     C                   CALL      'N93CL'
     C                   PARM                    XXPGM
     C                   PARM                    XXCO#
     C                   PARM                    XXSUB#
     C                   PARM                    XXGRP#
     C                   PARM                    XXEO#
     C                   PARM                    QQBAMT
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   PARM                    XFYR
     C                   PARM                    XCASE
     C                   PARM                    XRCN
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C*
     C                   END                                                    XPTYPE
     C*
     C                   MOVE      QQBAMT        XXAMT             9 2
     C*
     C     EO#BLL        IFEQ      *ZERO
     C                   Z-ADD     1             EO#BLL
     C                   END                                                    ENDIF EO#BLL
     C*
     C     XPTYPE        IFEQ      'P1'
     C     EO#BLL        MULT      XXAMT         XXAMT
     C                   END
     C*
     C*** UPDATE EOB TRANS FILE RECORD
     C*
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           Z-ADDXXAMT     EOBAMT           ACTUAL BILL AMT
     C                   ADD       XXAMT         EOBAMT                         ACTUAL BILL AMT
     C*** END   CHG *** 09/23/99 **************************************
     C*
     C* UPDATE THE BILLING FLAG SO THAT THE EOMB WILL NOT SHOW UP
     C* FOR INVOICING UNDER THESE CONDITIONS:
     C* 1) QQBAMT IF EQUALS ZERO
     C* 2) IF THERE AREN'T ANY T/M'S, EOFLG5 NOT EQUAL TO 'Y'
     C*
     C     EOBAMT        IFEQ      *ZEROS
     C*** BEGIN ADD *** 09/23/99 **************************************
     C     EOFLG5        ANDNE     'Y'
     C*** END   ADD *** 09/23/99 **************************************
     C*
     C                   MOVE      'P'           EOBLFL                         BILLED FLAG
     C                   END                                                    ENDEQ *ZERO
     C*** BEGIN ADD *** 09/23/99 **************************************
     C                   MOVE      'P'           EOSTAT                         PROCESSED
     C*** END   ADD *** 09/23/99 **************************************
     C                   MOVE      Q1PGM         EOPGM                          PROGRAM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   Z-ADD     DSTIME        EOTIME                         TIME
     C                   UPDATE    PEO01
     C                   ENDSR
     C*
     C*
     C*****************************************************************--*
     C*  PERFORM DATE CALCULATIONS TO DETERMINE IF NETWORK BILL NEEDED.
     C*****************************************************************--*
     C*
     C     SRPROV        BEGSR
     C*                                                    ##K 12/06/89
     C*  A FLAG MATCH IS RETURNED WITH 'Y' IF SERVICE      ##
     C*  DATES ALL WITHIN PROVIDER CONTRACT EFFECTIVE      ##
     C*  DATES                                             ##
     C*                                                    ##
     C                   MOVE      'Y'           MATCH             1            ##
     C*
     C* CHAIN TO THE EOMB AUXILIARY FILE TO GET NETWORK CODE
     C*
     C                   MOVE      *BLANKS       XXNWCD
     C*** BEG CHG *** 01/04/99 ****************************************
     C*
     C                   MOVE      EONTWK        XXNWCD
     C*
     C*** END CHG *** 01/04/99 ****************************************
     C*                                                    ##K 12/06/89
     C*
     C*  GET THE EFFECTIVE AND TERMINATION DATE FOR PROVIDER
     C*
     C     PDTYPE        IFEQ      '4'
     C*** BEGIN CHG *** 02/18/98 **************************************
     C********** HSKEY     SETGTPHS01
     C**********           READPPHS01                    60
     C     S4KEY         SETGT     PS401
     C                   READP     PS401                                  60
     C     *IN60         IFEQ      '0'
     C********** HSNWCD    ANDEQXXNWCD
     C********** HSFEIN    ANDEQEOFEIN
     C********** HSFMOD    ANDEQEOMOD
     C**********           Z-ADDHSCEDT    XXFROM
     C**********           Z-ADDHSTMDT    XXTO
     C     S4NWCD        ANDEQ     XXNWCD
     C     S4FEIN        ANDEQ     EOFEIN
     C     S4MOD         ANDEQ     EOMOD
     C                   Z-ADD     S4BGDT        XXFROM
     C*
     C     S4ENDT        IFEQ      *ZEROS
     C*
     C                   Z-ADD     9999999       XXTO
     C*
     C                   ELSE
     C*
     C                   Z-ADD     S4ENDT        XXTO
     C*
     C                   ENDIF
     C*** END   CHG *** 02/18/98 **************************************
     C                   ELSE                                                   *IN60 ON
     C*
     C** IF NO ASSOCIATED RECORD, NOT IN DATE RANGE, READ NEXT RECORD.
     C*
     C                   MOVE      'N'           MATCH
     C                   END                                                    *IN60 OFF
     C                   END                                                    PDTYPE=4
     C*
     C     PDTYPE        IFEQ      '1'
     C*** BEGIN CHG *** 02/18/98 **************************************
     C********** PHKEY     SETGTPPH01
     C**********           READPPPH01                    60
     C     S4KEY         SETGT     PS402
     C                   READP     PS402                                  60
     C     *IN60         IFEQ      '0'
     C********** PHNWCD    ANDEQXXNWCD
     C********** PHFEIN    ANDEQEOFEIN
     C********** PHFMOD    ANDEQEOMOD
     C**********           Z-ADDPHNEDT    XXFROM
     C**********           Z-ADDPHNXDT    XXTO
     C     S4NWCD        ANDEQ     XXNWCD
     C     S4FEIN        ANDEQ     EOFEIN
     C     S4MOD         ANDEQ     EOMOD
     C                   Z-ADD     S4BGDT        XXFROM
     C*
     C     S4ENDT        IFEQ      *ZEROS
     C*
     C                   Z-ADD     9999999       XXTO
     C*
     C                   ELSE
     C*
     C                   Z-ADD     S4BGDT        XXTO
     C*
     C                   ENDIF
     C*
     C*** END   CHG *** 02/18/98 **************************************
     C                   ELSE                                                   *IN60 ON
     C*
     C** IF NO ASSOCIATED RECORD, NOT IN DATE RANGE, READ NEXT RECORD.
     C*
     C                   MOVE      'N'           MATCH
     C                   END                                                    *IN60 OFF
     C                   END                                                    PDTYPE=1
     C*
     C*  COMPARE DATE RANGE -- IF FROM/TO SERVICE DATES ARE BOTH BEFORE
     C*    OR BOTH AFTER THE PROVIDER CONTRACT DATES, NO NETWORK BILL
     C*    IS REQUIRED. BUT, IF ANY SERVICE DATES ARE IN RANGE, A BILL
     C*    WILL BE PRODUCED..
     C*
     C     EOFRDT        IFLT      XXFROM                                       BEFORE THE
     C     EOTODT        ANDLT     XXFROM                                       CONTRACT
     C*
     C                   MOVE      'N'           MATCH
     C                   END                                                    EOFRDT LT
     C*
     C     EOFRDT        IFGT      XXTO                                         AFTER THE
     C     EOTODT        ANDGT     XXTO                                         CONTRACT
     C*
     C                   MOVE      'N'           MATCH
     C                   END                                                    EOFRDT GT
     C*
     C                   ENDSR
     C*
     C*
     C*****************************************************************--*
     C*  WHEN FUND HAS CHANGED, MUST GET NEW BILLING CODES ...
     C*****************************************************************--*
     C*
     C     FUND          BEGSR
     C*
     C*** CLEAR BILLING TYPES
     C*
     C                   MOVE      *BLANKS       XPTYPE
     C                   MOVE      *BLANKS       XHTYPE
     C                   MOVE      *BLANKS       XUTYPE
     C                   MOVE      *BLANKS       XNTYPE
     C                   MOVE      *BLANKS       XMTYPE                         ADD 05/03/95
     C                   MOVE      *BLANKS       XXCODE
     C*
     C     FNKEY         CHAIN     PFN01                              61
     C     *IN61         IFEQ      '0'
     C*
     C*** FIRST GET PBR TYPE BILLING CODE
     C*
     C                   Z-ADD     1             X                 5 0
     C     'P'           LOOKUP    FSY(X)                                 62
     C*
     C     *IN62         IFEQ      '1'
     C                   MOVEL     FBR(X)        XXCODE            3
     C*
     C*
     C*** NOW FIND THE BILLING MASTER RECORD BASED ON THE BILLING
     C*** CODE IN XXCODE.
     C*
     C*  GET THE BILLING MASTER RECORD
     C*
     C     BDKEY         SETGT     PBD01
     C     NXTBDP        TAG
     C                   READP     PBD01                                  64
     C*
     C*** VERIFY BILLING CODE AND STATUS
     C*
     C     *IN64         IFEQ      '0'
     C     BDCODE        IFNE      XXCODE
     C     BDSTAT        OREQ      'I'
     C*
     C                   GOTO      NXTBDP
     C                   END                                                    BDCODE/BDSTAT
     C                   END                                                    *IN64 OFF
     C*
     C     *IN64         IFEQ      '0'
     C                   MOVEL     BDTYPE        XPTYPE            2
     C                   END                                                    64=0 P TYPE
     C*
     C                   END                                                    62=1 P TYPE
     C*
     C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     C*
     C*** NEXT, GET HBA TYPE BILLING CODE
     C*
     C                   Z-ADD     1             X                 5 0
     C     'H'           LOOKUP    FSY(X)                                 62
     C*
     C     *IN62         IFEQ      '1'
     C                   MOVEL     FBR(X)        XXCODE            3
     C*
     C*
     C*** NOW FIND THE BILLING MASTER RECORD BASED ON THE BILLING
     C*** CODE IN XXCODE.
     C*
     C*  GET THE BILLING MASTER RECORD
     C*
     C     BDKEY         SETGT     PBD01
     C     NXTBDH        TAG
     C                   READP     PBD01                                  64
     C*
     C*** VERIFY BILLING CODE AND STATUS
     C*
     C     *IN64         IFEQ      '0'
     C     BDCODE        IFNE      XXCODE
     C     BDSTAT        OREQ      'I'
     C*
     C                   GOTO      NXTBDH
     C                   END                                                    BDCODE/BDSTAT
     C                   END                                                    *IN64 OFF
     C*
     C     *IN64         IFEQ      '0'
     C                   MOVEL     BDTYPE        XHTYPE            2
     C                   END                                                    64=0 H TYPE
     C*
     C                   END                                                    62=1 H TYPE
     C*
     C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     C*
     C*** NEXT, GET PRE/RE-CERT TYPE BILLING CODE
     C*
     C                   Z-ADD     1             X                 5 0
     C     'U'           LOOKUP    FSY(X)                                 62
     C*
     C     *IN62         IFEQ      '1'
     C                   MOVEL     FBR(X)        XXCODE            3
     C*
     C*
     C*** NOW FIND THE BILLING MASTER RECORD BASED ON THE BILLING
     C*** CODE IN XXCODE.
     C*
     C*  GET THE BILLING MASTER RECORD
     C*
     C     BDKEY         SETGT     PBD01
     C     NXTBDU        TAG
     C                   READP     PBD01                                  64
     C*
     C*** VERIFY BILLING CODE AND STATUS
     C*
     C     *IN64         IFEQ      '0'
     C     BDCODE        IFNE      XXCODE
     C     BDSTAT        OREQ      'I'
     C*
     C                   GOTO      NXTBDU
     C                   END                                                    BDCODE/BDSTAT
     C                   END                                                    *IN64 OFF
     C*
     C     *IN64         IFEQ      '0'
     C                   MOVEL     BDTYPE        XUTYPE            2
     C                   END                                                    64=0 U TYPE
     C*
     C                   END                                                    62=1 U TYPE
     C*
     C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     C*
     C* NEXT, GET MEDICAL MANGEMENT BILLING CODE
     C*
     C                   Z-ADD     1             X                 5 0
     C     'M'           LOOKUP    FSY(X)                                 62
     C*
     C     *IN62         IFEQ      '1'
     C                   MOVEL     FBR(X)        XXCODE            3
     C*
     C*
     C* NOW FIND THE BILLING MASTER RECORD BASED ON THE BILLING
     C* CODE IN XXCODE.
     C*
     C* GET THE BILLING MASTER RECORD
     C*
     C     BDKEY         SETGT     PBD01
     C     NXTBDM        TAG
     C                   READP     PBD01                                  64
     C*
     C* VERIFY BILLING CODE AND STATUS
     C*
     C     *IN64         IFEQ      '0'
     C     BDCODE        IFNE      XXCODE
     C     BDSTAT        OREQ      'I'
     C*
     C                   GOTO      NXTBDM
     C                   END                                                    BDCODE/BDSTAT
     C                   END                                                    *IN64 OFF
     C*
     C     *IN64         IFEQ      '0'
     C                   MOVEL     BDTYPE        XMTYPE            2
     C                   END                                                    64=0 M TYPE
     C*
     C                   END                                                    62=1 M TYPE
     C*
     C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     C*
     C*** LAST- GET NETWORK TYPE BILLING CODE
     C*
     C                   Z-ADD     1             X                 5 0
     C     'N'           LOOKUP    FSY(X)                                 62
     C*
     C     *IN62         IFEQ      '1'
     C                   MOVEL     FBR(X)        XXCODE            3
     C*
     C*
     C*** NOW FIND THE BILLING MASTER RECORD BASED ON THE BILLING
     C*** CODE IN XXCODE.
     C*
     C*  GET THE BILLING MASTER RECORD
     C*
     C     BDKEY         SETGT     PBD01
     C     NXTBDN        TAG
     C                   READP     PBD01                                  64
     C*
     C*** VERIFY BILLING CODE AND STATUS
     C*
     C     *IN64         IFEQ      '0'
     C     BDCODE        IFNE      XXCODE
     C     BDSTAT        OREQ      'I'
     C*
     C                   GOTO      NXTBDN
     C                   END                                                    BDCODE/BDSTAT
     C                   END                                                    *IN64 OFF
     C*
     C                   Z-ADD     0             XXAM01
     C     *IN64         IFEQ      '0'
     C                   MOVEL     BDTYPE        XNTYPE            2
     C                   Z-ADD     BDAM01        XXAM01                         10/17/89 ADD
     C                   END                                                    64=0 N TYPE
     C*
     C                   END                                                    62=1 N TYPE
     C*
     C                   END                                                    61=0
     C                   ENDSR
     C*
     C*** BEGIN ADD *** 10/04/99 **************************************
     C*****************************************************************--*
     C*  EAP - IF PROVIDER BILLS INSTALLED UPDATE FAEAP.
     C*****************************************************************--*
     C*
     C     EAP           BEGSR
     C*
     C                   MOVE      *ZEROS        XXCO#
     C                   MOVE      *ZEROS        XXSUB#
     C                   MOVE      *ZEROS        XXGRP#
     C                   MOVE      *ZEROS        XXEO#
     C                   MOVE      *ZEROS        QQBAMT
     C                   MOVE      *ZEROS        XFYR
     C                   MOVE      *ZEROS        XCASE
     C                   MOVE      *ZEROS        XRCN
     C*
     C     EOSYS         IFEQ      'M'
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C*
     C                   ENDIF
     C*
     C*** DETERMINE THE PROGRAM TO CALL TO PROCESS REQUEST
     C*
     C*
     C     XPTYPE        IFNE      *BLANKS
     C     EOSYS         ANDEQ     'P'
     C     XHTYPE        ORNE      *BLANKS
     C     EOSYS         ANDEQ     'H'
     C     XUTYPE        ORNE      *BLANKS
     C     EOSYS         ANDEQ     'U'
     C     XMTYPE        ORNE      *BLANKS
     C     EOSYS         ANDEQ     'M'
     C     CMSETU        ANDNE     'Y'
     C     *IN60         ANDEQ     *OFF
     C*
     C** Call N93CL to GET PROGRAM BASED ON TYPE ENTERED.
     C*
     C                   MOVEL     CMD(1)        XXPGM
     C*
     C                   SELECT
     C*
     C     EOSYS         WHENEQ    'P'
     C                   MOVE      XPTYPE        XXTYPE
     C*
     C     EOSYS         WHENEQ    'H'
     C                   MOVE      XHTYPE        XXTYPE
     C*
     C     EOSYS         WHENEQ    'U'
     C                   MOVE      XUTYPE        XXTYPE
     C*
     C     EOSYS         WHENEQ    'M'
     C                   MOVE      XMTYPE        XXTYPE
     C*
     C                   ENDSL
     C*
     C                   MOVE      EOCO#         XXCO#
     C                   MOVE      EOSUB#        XXSUB#
     C                   MOVE      EOFND         XXGRP#
     C                   MOVE      EOEOB#        XXEO#
     C                   MOVE      *ZEROS        QQBAMT
     C                   MOVE      EOFYR         XFYR
     C                   MOVE      EOCASE        XCASE
     C                   MOVE      EORCN         XRCN
     C*
     C*
     C*
     C                   CALL      'N93CL'
     C                   PARM                    XXPGM
     C                   PARM                    XXCO#
     C                   PARM                    XXSUB#
     C                   PARM                    XXGRP#
     C                   PARM                    XXEO#
     C                   PARM                    QQBAMT
     C                   PARM                    XFYR
     C                   PARM                    XCASE
     C                   PARM                    XRCN
     C*
     C                   ENDIF                                                  XHTYPE
     C*
     C     EOSYS         IFNE      'M'
     C*
     C     CMKEY         CHAIN(N)  PCM01                              99
     C*
     C                   ENDIF
     C*
     C* UPDATE THE BILLING FLAG SO THAT THE EOMB WILL NOT SHOW UP
     C* FOR INVOICING UNDER THESE CONDITIONS:
     C* 1) QQBAMT IF EQUALS ZERO
     C* 2) IF THERE AREN'T ANY T/M'S, EABAMT EQUAL TO ZERO.
     C*
     C     QQBAMT        IFEQ      *ZEROS
     C*
     C     EAKY          CHAIN     PEA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     EABAMT        ANDEQ     *ZEROS
     C*
     C                   MOVE      'P'           ##BBLF
     C                   MOVEL     *BLANKS       ##NBLF
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       ##BBLF
     C                   MOVEL     *BLANKS       ##NBLF
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       ##NBLF
     C                   MOVEL     *BLANKS       ##BBLF
     C*
     C                   ENDIF
     C*
     C                   MOVE      QQBAMT        ##TMAA                         BRS AMOUNT
     C                   MOVE      QQBAMT        ##BCAM                         BRS CALC AMOUNT
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXFAEA
     C*
     C* UPDATE VALUES IN FAEAP BY CALLING AA001R
     C*
     C                   CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM      *ZEROS        ##BAMT            9 2          BRS AMOUNT
     C                   PARM      *ZEROS        ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM      *ZEROS        ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C* UPDATE EOSTAT WITH 'P' SO THAT THIS EOMB DOES NOT GET
     C* PROCESSED AGAIN.
     C*
     C                   MOVE      'P'           EOSTAT
     C                   UPDATE    PEO01
     C*
     C* UPDATE CMSETU WITH 'Y' IF THE SYSTEM IS 'M', SO THAT ANY 'M'
     C* TYPE ACTIVITY ON THIS CASE IS ONLY PROCESSED ONCE.
     C*
     C     EOSYS         IFEQ      'M'
     C*
     C                   MOVEL     'Y'           CMSETU
     C*
     C                   UPDATE    PCM01
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 10/04/99 **************************************
     O*** BEGIN DEL *** 10/26/98 **************************************
     O**********PCM01   E                PCM01E
     O*** END   DEL *** 10/26/98 **************************************
**
N93XXR
