     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H/TITLE W4081R - Deposit Recap Report - Employer Sequence
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 07/18/07 C:136 PROJECT: WC1491 (IND CASH DIST)                *
     H*                ADD PROCESSING FOR DEPOSIT REFUND AND VOID/    *
     H*                STOP PAY CODES                                 *
     H*                                                               *
     H* 03/02/06  C:035  PROJECT: SUPPORT WA133 51280                 *
     H*                  EXPANDED DATE FIELD TO ACCOMODATE 7-DIGIT    *
     H*                  CYYMMDD FORMAT.                              *
     H*                                                               *
     H* 04/19/01   C:134 PROJECT: WC577                               *
     H*            REVISED TO PRINT USER SPECIFIED PARAMETERS IN THE  *
     H*            HEADING OF THE REPORT.                             *
     H*                                                               *
     H*  10/29/98  C:61  PROJECT: WC577                               *
     H*                  Initialize accumulations prior next          *
     H*                  read from work file.                         *
     H*                                                               *
     H*  02/20/98  C:87  PROJECT: WC775                               *
     H*            ADDED CODE TO REFORMAT ROUTING PAGE. MOVED CODE    *
     H*            THAT HANDLED AGENT INFO AND OUTPUT. REMOVED DUPL   *
     H*            CODE. REMOVED AGENT ASSIGNMENT (WMAAL1) AND ITS    *
     H*            CODE - NO LONGER NEEDED. CHANGED KEY AND FILE FOR  *
     H*            AGENT  HISTORY.                                    *
     H*                                                               *
     H*  01/16/98  C:80  PROJECT: WC577                               *
     H*            MODIFIED REPORT TO READ WXEBP.  ADDED CANCELLATION *
     H*            DATE.  ADDED BILLING SCHEDULE CODE, TYPE AND       *
     H*            DESCRIPTION. ADDED PRODUCER NUMBER AND NAME.       *
     H*                                                               *
     H*  10/27/97  C:86  VERSION 3 PROJECT: WC577                     *
     H*            REVISED TO PRINT THE ALTERNATE FUND NAME IN PLACE  *
     H*            OF COMPANY NAME, IF ONE EXISTS.                    *
     H*****************************************************************
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter Master File
     FWMUPL     IF   E           K DISK
     F*
     F* Output Routing Master File
     FSMORL     IF   E           K DISK
     F*
     F* Fund Master File
     FWMFNL     IF   E           K DISK
     F*
     F*** BEGIN DEL *** 01/16/98 **************************************
     F* Employer Master File
     F* WMEML   IF  E           K        DISK
     F*** END   DEL *** 01/16/98 **************************************
     F*
     F* Employer Detail File
     FWDELL1    IF   E           K DISK
     F*
     F* Employer Accounts Receivable Master File
     FWMEAL     IF   E           K DISK
     F*
     F*** BEGIN ADD *** 01/16/98 **************************************
     F*** BEGIN DEL *** 02/20/98 **************************************
     F**** Agent Assignment Master File
     F**** WMAAL1  IF  E           K        DISK
     F*** END   DEL *** 02/20/98 **************************************
     F*
     F* Agent Producer Master File
     FWMAPL1    IF   E           K DISK
     F*
      *** BEGIN CHG *** 02/20/98 *************************************
     F**** Agent History File
     F**** WMAHL   IF  E           K        DISK
     F*
     F* Agent History File
     FWMAHL1    IF   E           K DISK
      *** END   CHG *** 02/20/98 *************************************
     F*
     FWXEBL     IF   E           K DISK
     F*
     FWTABL     IF   E           K DISK
     F*
     F*** END   ADD *** 01/16/98 **************************************
     F*
     F* Output Printer File
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F*

     D CD              S              7  0 DIM(5)
     D ED              S              7  0 DIM(5)
     D*
     D* System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*** BEGIN DEL *** 01/16/98 **************************************
     D* Cancel Date
     D*********** DS
     D*************************************** 1 0350CD
     D************************************* 001 0070EMCAD1
     D************************************* 008 0140EMCAD2
     D************************************* 015 0210EMCAD3
     D************************************* 022 0280EMCAD4
     D************************************* 029 0350EMCAD5
     D*
     D* Effective Date
     D*********** DS
     D*************************************** 1 0350ED
     D************************************* 001 0070EMEFD1
     D************************************* 008 0140EMEFD2
     D************************************* 015 0210EMEFD3
     D************************************* 022 0280EMEFD4
     D************************************* 029 0350EMEFD5
     D*** END   DEL *** 01/16/98 **************************************
     D*
     D                 DS
     D  XXPRDT                 1      8  0
     D  XXMMDD                 1      4  0
     D  XXYR                   7      8  0
     D*
     D                 DS
     D  X1PRDT                 1      6  0
     D  X1MMDD                 1      4  0
     D  X1YR                   5      6  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Key List for The Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* Key List for the Override Master File
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C* Key List for the Fund Master File
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*** BEGIN DEL *** 01/16/98 **************************************
     C* Key List for the Employer Master File
     C*
     C********** EMKEY     KLIST
     C******************** KFLD           XXCO#
     C******************** KFLD           XXFND
     C*** END   DEL *** 01/16/98 **************************************
     C*
     C* Key List for the Employer Detail File
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXFYR             3 0
     C*** BEGIN CHG *** 01/16/98 **************************************
     C******************** KFLD           EMEMP#
     C******************** KFLD           EMDIV#
     C                   KFLD                    EBEMP#
     C                   KFLD                    EBDIV
     C*** END   CHG *** 01/16/98 **************************************
     C*
     C* Key List for the Employer Accounts Receivable Master File
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** BEGIN CHG *** 01/16/98 **************************************
     C******************** KFLD           EMEMP#
     C******************** KFLD           EMDIV#
     C                   KFLD                    EBEMP#
     C                   KFLD                    EBDIV
     C*** END   CHG *** 01/16/98 **************************************
     C*
     C*** BEGIN ADD *** 01/16/98 **************************************
     C* * * BILLING SCHEDULE TABLE FILE
     C     BSKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBSCD           10
     C*
     C* Key List for the Agent Producer Master File
     C*
     C     APKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXPROD            5 0
     C*
      *** BEGIN DEL *** 02/20/98 *************************************
     C* Key List for the Agent Assignment Master File
     C**********
     C********** AAKEY     KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXFND
     C**********           KFLD           EBAGT#
      *** END   DEL *** 02/20/98 *************************************
     C*
      *** BEGIN CHG *** 02/20/98 *************************************
     C********** AHKEY     KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXFND
     C**********           KFLD           EBEMP#
     C**********           KFLD           EBDIV
     C**********           KFLD           XXFYR
     C**********           KFLD           X1DATE
     C**********
     C     AHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    EBAGT#
     C                   KFLD                    EBEMP#
     C                   KFLD                    EBDIV
      *** END   CHG *** 02/20/98 *************************************
     C*
     C*** END   ADD *** 01/16/98 **************************************
     C*** BEG ADD ***** 07/18/07 **************************************
     C     TTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKKEY            10
     C*** END ADD ***** 07/18/07 **************************************
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C*
     C*** BEGIN CHG *** 01/16/98 **************************************
     C******************** PARM           QQCO#   3
     C******************** PARM           QQFND   3
     C******************** PARM           QQFYR   3
     C******************** PARM           QQDTE   8
     C******************** PARM           QQQUE  10
     C******************** PARM           QQMODE  1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQPFDT            8
     C                   PARM                    QQPTDT            8
     C                   PARM                    QQRNDT            8
     C                   PARM                    QQRDTS            1
     C                   PARM                    QQEMTP            1
     C                   PARM                    QQACST            1
     C                   PARM                    QQPSRT            2
     C                   PARM                    QQSSRT            2
     C                   PARM                    QQEMFG            1
     C                   PARM                    QQAGFG            1
     C                   PARM                    QQPREM            1
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQRGTP            2
     C                   PARM                    QQL1RG           15
     C                   PARM                    QQH1RG           15
     C                   PARM                    QQRGT2            2
     C                   PARM                    QQL2RG           15
     C                   PARM                    QQH2RG           15
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C*
     C                   SELECT
     C*
     C     QQRGTP        WHENEQ    'EA'
     C                   MOVEL     QQL1RG        XXENLR            2
     C                   MOVEL     QQH1RG        XXENHR            2
     C                   MOVE      *ON           *IN53
     C*
     C     QQRGTP        WHENEQ    'EN'
     C                   MOVEL     QQL1RG        XXEBLR           10 0
     C                   MOVEL     QQH1RG        XXEBHR           10 0
     C                   MOVE      *ON           *IN54
     C*
     C                   ENDSL
     C*
     C                   SELECT
     C*
     C     QQRGT2        WHENEQ    'AA'
     C                   MOVEL     QQL2RG        XXANLR            2
     C                   MOVEL     QQH2RG        XXANHR            2
     C                   MOVE      *ON           *IN55
     C*
     C     QQRGT2        WHENEQ    'AN'
     C                   MOVE      QQL2RG        XXABLR            5 0
     C                   MOVE      QQH2RG        XXABHR            5 0
     C                   MOVE      *ON           *IN56
     C*
     C                   ENDSL
     C*** END   CHG *** 01/16/98 **************************************
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* Set-up and Initialize
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      EBFYR         XXFYR             3 0
     C*** BEGIN CHG *** 01/16/98 **************************************
     C******************** MOVE QQDTE     XXDATE  80
     C******************** MOVE QQDTE     XXPRDT  80
     C                   MOVE      QQRNDT        XXDATE            8 0
     C                   MOVE      QQRNDT        XXPRDT            8 0
     C*** END   CHG *** 01/16/98 **************************************
     C*** BEGIN ADD *** 01/16/98 **************************************
     C                   MOVE      QQPFDT        XXPFDT            8 0
     C                   MOVE      QQPTDT        XXPTDT            8 0
     C*
     C*** END   ADD *** 01/16/98 **************************************
     C                   MOVEL     XXMMDD        X1MMDD
     C                   MOVEL     XXYR          X1YR
     C                   MOVE      QQMODE        XXMODE            1
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1DATE            7 0
     C*
     C* Execute Subroutine to Load and Print Routing Information
     C*
     C                   EXSR      SR1ST
     C*
     C* Get the Fund Record
     C*
     C     FNKEY         CHAIN     PFN01                              40
     C*** BEGIN ADD *** 10/27/97 *******************************
     C     *IN60         IFEQ      '0'
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL(P)  FNANAM        ##DATA
     C                   ELSE
     C                   MOVEL(P)  CONAME        ##DATA
     C                   ENDIF
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XCNAME           60
     C                   ENDIF
     C*
     C*** END ADD ***** 10/27/97 ********************************
     C*
     C*** BEGIN DEL *** 01/16/98 **************************************
     C* Read the employer report selection workfile
     C*
     C********** EMKEY     CHAINPEM01                50
     C*** END   DEL *** 01/16/98 **************************************
     C*** BEGIN ADD *** 01/16/98 **************************************
     C                   READ      PEB01                                  50
     C*
     C                   MOVE      EBAGT#        X1AGT#            5 0
     C*
     C     QQPSRT        IFEQ      'AA'
     C     QQPSRT        OREQ      'AN'
     C*
     C                   MOVE      *ON           *IN25
     C*
     C                   ENDIF                                                  QQPSRT IFEQ 'AA'
     C*** END   ADD *** 01/16/98 **************************************
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C* Determine if this is a Cancelled Employer          ## LJD 10/18
     C*                                                    ##
     C*** BEGIN DEL *** 01/16/98 **************************************
      ********************
     C******************** Z-ADD*ZEROS    HLCDAT  60       ##
     C******************** MOVE '0'       *IN45            ##
     C********************                                 ##
     C******************** DO   5         A       10       ##
     C********** CD,A      IFGT HLCDAT                     ##
     C********** ED,A      ANDNE*ZERO                      ##
     C******************** Z-ADDCD,A      HLCDAT           ##
     C******************** MOVE '1'       *IN45            ##
     C******************** END                             ##
     C******************** END                             ## LJD 10/18
     C*** END   DEL *** 01/16/98 **************************************
     C*** BEGIN ADD *** 01/16/98 **************************************
     C*
     C                   MOVE      *OFF          *IN28
      *
      *** BEGIN DEL *** 02/20/98 *************************************
     C********** EBAGT#    IFNE X1AGT#
     C********** L1TOT     ANDNE*ZERO
     C**********
     C********** QQPSRT    IFEQ 'AA'
     C********** QQPSRT    OREQ 'AN'
     C**********
     C**********           EXSR SUBL2
     C**********           MOVE *ON       *IN24
     C**********           MOVE EBAGT#    X1AGT#
     C**********
     C**********           ENDIF                           QQPSRT IFEQ 'AA'
     C**********
     C**********           ENDIF                           EBAGT# IFNE X1AGT#
      *** END   DEL *** 02/20/98 *************************************
     C*
     C*
      **** BEGIN CHG **** 03/02/06 *********************************************
     C******************** Z-ADDEBCNDT    HLCDAT  60       ##
     C                   Z-ADD     EBCNDT        HLCDAT            7 0          ##
      **** END   CHG **** 03/02/06 *********************************************
     C*** END   ADD *** 01/16/98 **************************************
     C*
     C                   MOVE      *BLANKS       XXNAME           35
     C*** BEGIN CHG *** 01/16/98 **************************************
     C******************** MOVELEMNAME    XXNAME
     C                   MOVEL     EBENAM        XXENAM           40
     C*** END   CHG *** 01/16/98 **************************************
     C*
     C*** BEGIN ADD *** 01/16/98 **************************************
     C                   MOVE      EBCO#         XXCO#
     C                   MOVE      EBFND         XXFND
     C                   MOVE      EBFYR         XXFYR
     C*
     C* Get the Agent # and Producer #
     C*
     C                   MOVE      *BLANKS       PNAME            15
     C*
      *** BEGIN DEL *** 02/20/98 *************************************
     C********** AHKEY     SETGTPAH01
     C**********           READPPAH01                    60
     C**********
     C********** *IN60     DOWEQ'0'
     C**********
     C********** AHAGT#    ANDNEEBAGT#
     C**********           READPPAH01                    60
     C**********
     C**********           ENDDO                           *IN60 DOWEQ '0'
     C**********
     C********** *IN60     IFEQ '1'
     C********** AHCO#     ORNE XXCO#
     C********** AHCO#     ORNE XXCO#
     C********** AHFND     ORNE XXFND
     C********** AHEMP#    ORNE EBEMP#
     C********** AHDIV     ORNE EBDIV
     C********** AHFYR     ORNE XXFYR
     C**********
     C**********           Z-ADD0         XXPROD  50
     C**********
     C**********           ELSE
     C**********
     C**********           Z-ADDAHPROD    XXPROD
     C**********
     C**********           ENDIF                           Agnt get
      *** END   DEL *** 02/20/98 *************************************
     C*
      *** BEGIN ADD *** 02/20/98 *************************************
      *
      * Get Producer #
      *
     C     AHKEY         SETGT     PAH01
     C     AHKEY         READPE    PAH01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     AHPROD        XXPROD            5 0
      *
     C                   ELSE
      *
     C                   Z-ADD     0             XXPROD
      *
     C                   ENDIF                                                  *IN60 = *OFF
      *** END   ADD *** 02/20/98 *************************************
     C*
     C* Get Producer Name
     C*
      *** BEGIN DEL *** 02/20/98 *************************************
     C********** AAKEY     CHAINPAA01                60
     C**********           Z-ADDAAFEIN    XXFEIN  90
     C**********           Z-ADDAAMOD     XXMOD   50
      *** END   DEL *** 02/20/98 *************************************
     C     APKEY         CHAIN     PAP01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C* Call Program to Concatenate Producer Name
     C*
     C                   MOVEL     APLSN         SRLNAM           56
     C                   MOVEL     APFSN         SRFNAM           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    QQTERM
     C                   MOVEL     SRLNAM        PNAME            15
     C*
     C                   ENDIF                                                  *IN60 IFEQ '0'
     C*
     C*** END   ADD *** 01/16/98 **************************************
     C*
     C* Get the Employer Detail Record
     C*
     C     ELKEY         CHAIN     PEL01                              51
     C     *IN51         IFEQ      '0'
     C*
     C* Get the billing schedule code type and description
     C*
     C                   MOVE      *BLANKS       XXBSCD
     C                   MOVEL     ELBSCD        XXBSCD
     C*
     C     BSKY1         CHAIN     PBS01                              60
     C*
     C                   MOVEL     BSSEFL        XXSEFL            2
     C                   MOVEL     BSDESC        XXDESC           30
     C*
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELANMP        XXPREM                         ## LJD 10/18
     C                   ELSE
     C                   Z-ADD     ELENMP        XXPREM                         ## LJD 10/18
     C                   END
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             XXPREM
     C*
     C                   END
     C*
     C* Get all 'H' and 'I' Record from the A/R File
     C*
     C     EAKEY         CHAIN     PEA01                              52
     C     *IN52         DOWEQ     '0'
     C*
     C*  PROCESS RECORD ONLY IF POSTING DATE IS LESS THAN OR EQUAL
     C*    TO THE RUN DATE.
     C*
     C     EAPODT        IFLE      X1DATE
      *
      *** BEG ADD ***** 07/18/07 *************************************
     C                   MOVE(P)   EAARCD        KKKEY
     C     TTKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       TTCTYP
     C                   ENDIF
      *** END ADD ***** 07/18/07 *************************************
     C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     C* Calculate Deposit Billed
     C*
     C     EAARTY        IFEQ      'H'
      *** BEGIN DEL *** 02/20/98 *************************************
     C********** EAPODT    ANDLEX1DATE                     JJJ 2-17-89
      *** END   DEL *** 02/20/98 *************************************
     C                   ADD       EAAMT         XXDPBL
     C                   END
     C*
     C* Calculate Deposit Paid
     C*
     C     EAARTY        IFEQ      'I'
      *** BEG ADD ***** 07/18/07 *************************************
     C     EAARTY        OREQ      'T'
     C     TTCTYP        ANDEQ     'D'
      *** END ADD ***** 07/18/07 *************************************
      *** BEGIN DEL *** 02/20/98 *************************************
     C********** EAPODT    ANDLEX1DATE                     JJJ 2-17-89
      *** END   DEL *** 02/20/98 *************************************
     C                   ADD       EAAMT         XXDPPD
     C                   END
      *
      *** BEG ADD ***** 07/18/07 *************************************
     C     EAARTY        IFEQ      'R'
     C     TTCTYP        ANDEQ     'D'
     C                   SUB       EAAMT         XXDPPD
     C                   ENDIF
      *** END ADD ***** 07/18/07 *************************************
     C* * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     C                   END
     C*** BEGIN ADD *** 01/16/98 **************************************
     C*
     C* Get the Agent # and Producer #
     C*
     C*** END   ADD *** 01/16/98 **************************************
     C*
     C     EAKEY         READE     PEA01                                  52
     C                   END
     C*
     C*** BEGIN ADD *** 01/16/98 **************************************
     C* Do not print employer if deposit billed and paid amount= 0.
     C*
     C     XXDPPD        IFEQ      *ZEROS
     C     XXDPBL        ANDEQ     *ZEROS
     C*
     C                   MOVE      *ON           *IN28                    50
     C*
     C                   ENDIF                                                  XXDPPD IFEQ *ZEROS
     C*** END   ADD *** 01/16/98 **************************************
     C*
     C*                 JJJ 2-17-89 COMMENT OUT CODED ADDED BY LJD
     C* If this is a Cancelled Employer with XXDPPD = 0, do not print
     C*                                                    ## LJD 10/18
     C*** BEGIN ADD *** 01/16/98 **************************************
     C     *IN28         IFEQ      *OFF
     C*
     C     XXDPBL        SUB       XXDPPD        XXBAL
     C                   ADD       XXPREM        L1PREM           11 2
     C                   ADD       XXDPBL        L1DPBL           11 2
     C                   ADD       XXDPPD        L1DPPD           11 2
     C                   ADD       XXBAL         L1BAL            11 2
     C                   ADD       1             L1TOT             5 0
     C*
     C*** BEGIN ADD *** 01/16/98 **************************************
     C     QQPSRT        IFEQ      'EA'
     C     QQPSRT        OREQ      'EN'
     C*
     C                   EXSR      SUBL1
     C*
     C                   ENDIF                                                  QQPSRT IFEQ 'EA
     C*
     C     *IN24         IFEQ      '1'
     C     *IN25         OREQ      '1'
     C*
     C     *INOF         IFEQ      '0'
     C*
     C                   EXCEPT    HEADH
     C                   EXCEPT    PROD
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *IN24
     C                   MOVE      *OFF          *IN25
     C*
     C                   ENDIF                                                  *INOF IFEQ '0'
     C*
     C                   ENDIF                                                  *IN24 IFEQ '1'
     C*
     C*** END   ADD *** 01/16/98 **************************************
     C*
     C* Check for Page Overflow
     C*
     C     *INOF         IFEQ      '1'
     C*** BEGIN CHG *** 01/16/98 **************************************
     C******************** EXCPTHEAD
     C                   EXCEPT    HEADH
     C*** END   CHG *** 01/16/98 **************************************
     C*
     C     QQPSRT        IFEQ      'AA'
     C     QQPSRT        OREQ      'AN'
     C*
     C                   EXCEPT    PROD
     C*
     C                   ENDIF                                                  QQPSRT IFEQ 'AA
     C*
     C                   EXCEPT    HEAD
     C*
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C* Print Detail Line
     C*
     C                   Z-ADD     HLCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCDAT            6 0
     C                   EXCEPT    DETAIL
     C*
      ***** BEG ADD ******* 10/29/98 **********************************
     C                   ENDIF                                                  *IN28 IFEQ '0'
      ***** END ADD ******* 10/29/98 **********************************
     C*
     C* Clear Detail Line Fields
     C*
     C     CLRTAG        TAG                                                    ## LJD 10/18
     C                   Z-ADD     0             XXPREM           11 2
     C                   Z-ADD     0             XXDPBL           11 2
     C                   Z-ADD     0             XXDPPD           11 2
     C                   Z-ADD     0             XXBAL            11 2
     C                   Z-ADD     0             XXCDAT            6 0          ## LJD 10/18
     C*
     C* Get the Next Employer Record
     C*
     C*** BEGIN CHG *** 01/16/98 **************************************
     C********** EMKEY     READEPEM01                    50
     C*
      ***** BEG DEL ******* 10/29/98 **********************************
     C**********           ENDIF                           *IN28 IFEQ '0'
      ***** END DEL ******* 10/29/98 **********************************
     C*
     C                   READ      PEB01                                  50
     C*** END   CHG *** 01/16/98 **************************************
      *** BEGIN ADD *** 02/20/98 *************************************
     C     EBAGT#        IFNE      X1AGT#
     C     L1TOT         ANDNE     *ZERO
     C     *IN50         OREQ      *ON
     C     L1TOT         ANDNE     *ZERO
     C*
     C     QQPSRT        IFEQ      'AA'
     C     QQPSRT        OREQ      'AN'
     C*
     C                   EXSR      SUBL2
     C                   MOVE      *ON           *IN24
     C                   MOVE      EBAGT#        X1AGT#
     C*
     C                   ENDIF                                                  QQPSRT IFEQ 'AA'
     C*
     C                   ENDIF                                                  EBAGT# IFNE X1AGT#
      *** END   ADD *** 02/20/98 *************************************
     C*
     C                   ENDDO
     C*
     C     *IN50         IFEQ      '1'
     C     L2TOT         ANDEQ     0
     C*** BEGIN ADD *** 01/16/98 **************************************
     C                   EXCEPT    HEADH
     C                   EXCEPT    HEAD
     C*** END   ADD *** 01/16/98 **************************************
     C                   EXCEPT    DETL2
     C*
     C*** BEGIN ADD *** 01/16/98 **************************************
     C                   ELSE
     C*
     C                   EXSR      SUBL3
     C*
     C                   ENDIF                                                  *IN50 IFEQ '1'
     C*** END   ADD *** 01/16/98 **************************************
     C* Execute Fund Total Subroutines
     C*
     C*
     O*** BEGIN DEL *** 01/16/98 **************************************
     C********** *IN50     IFEQ '0'
     C*
     C******************** EXCPTHEAD3H
     C******************** EXCPTHEAD3
     C******************** EXCPTTOTL3
     C*
     C******************** ENDIF                           *IN50 IFEQ '0'
     O*** END   DEL *** 01/16/98 **************************************
     C*
      /Free
       // ***** Beg Add ***** 08/26/14 ********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       // ***** End Add ***** 08/26/14 ********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
     C*****************************************************************
     C*   Set-up and Initialize        ** Subroutine **   S R 1 S T   *
     C*****************************************************************
     C*
     C     SR1ST         BEGSR
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
      *
      *** BEGIN ADD *** 02/20/98 *************************************
      *
      * SET UP ROUTING PAGE FIELDS
      *
      *     ACCOUNT TYPE
     C     QQEMTP        IFEQ      'E'
     C     'ESTIMAT'     CAT       'ED':0        RSEMTP           35
      *
     C                   ELSE
      *
     C     QQEMTP        IFEQ      'S'
     C     'SELF-REP'    CAT       'ORTING':0    RSEMTP
      *
     C                   ELSE
      *
     C     QQEMTP        IFEQ      'B'
     C     'BOTH EST'    CAT       'IMATED':0    RSEMTP
     C                   CAT       'AND SE':1    RSEMTP
     C                   CAT       'LF REP':0    RSEMTP
     C                   CAT       'ORTING':0    RSEMTP
     C                   ENDIF                                                  QQEMTP='B'
      *
     C                   ENDIF                                                  QQEMTP='S'
      *
     C                   ENDIF                                                  QQEMTP='E'
      *
      *     ACCOUNT STATUS
     C     QQRDTS        IFEQ      'A'
     C                   MOVEL     'ACTIVE'      RSRDTS           35
      *
     C                   ELSE
      *
     C     QQRDTS        IFEQ      'C'
     C     'CANCELL'     CAT       'ED':0        RSRDTS
      *
     C                   ELSE
      *
     C     QQRDTS        IFEQ      'B'
     C     'BOTH ACT'    CAT       'IVE AN':0    RSRDTS
     C                   CAT       'D CANC':0    RSRDTS
     C                   CAT       'ELLED':0     RSRDTS
     C                   ENDIF                                                  QQRDTS='B'
      *
     C                   ENDIF                                                  QQRDTS='C'
      *
     C                   ENDIF                                                  QQRDTS='A'
      *
     **
     ** DISPLAY CORRECT SORT CODES
     **
      * * * * * * *
      *   SORT 1
      * * * * * * *
     C                   SELECT
      *
     C     QQPSRT        WHENEQ    'AN'
     C                   MOVEL     'AGENT #'     RSPSRT           13
      *
     C     QQPSRT        WHENEQ    'AA'
     C     'AGENT'       CAT       'NAME':1      RSPSRT
      *
     C     QQPSRT        WHENEQ    'EN'
     C     'EMPLOYER'    CAT       '/DIV':0      RSPSRT
      *
     C     QQPSRT        WHENEQ    'EA'
     C     'EMPLOYER'    CAT       'NAME':1      RSPSRT
      *
     C                   ENDSL
      * * * * * * *
      *   SORT 2
      * * * * * * *
     C                   SELECT
      *
     C     QQSSRT        WHENEQ    'EN'
     C     'EMPLOYER'    CAT       '/DIV':0      RSSSRT           13
      *
     C     QQSSRT        WHENEQ    'EA'
     C     'EMPLOYER'    CAT       'NAME':1      RSSSRT
      *
     C     QQSSRT        WHENEQ    *BLANK
     C                   MOVEL     'NONE'        RSSSRT
      *
     C                   ENDSL
     **
      *** END   ADD *** 02/20/98 *************************************
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C* Initialize Fields
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      XXFYR         XXFYR2            2 0
     C*
     C*** BEGIN ADD *** 01/16/98 **************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXDSDT            8 0
     C*
     C*** END   ADD *** 01/16/98 **************************************
     C*
     C* Get the Company Master Record
     C*
     C     COKEY         CHAIN     SMCOL                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C* Get the Distribution Information
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
      *
      *** BEGIN ADD *** 02/20/98 *************************************
      *
      *  Left Justify Report Title
      *
     C     ' '           CHECK     ORTITL        X                 2 0
      *
     C     X             IFNE      0
     C                   SUBST(P)  ORTITL:X      XXTITL           60
     C                   ENDIF                                                  X IFNE 0
      *** END   ADD *** 02/20/98 *************************************
     C*
     C* Print the Routing Page
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C*********          EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C                   Z-ADD     0             L1TOT
     C                   Z-ADD     0             L1PREM
     C                   Z-ADD     0             L1DPBL
     C                   Z-ADD     0             L1DPPD
     C                   Z-ADD     0             L1BAL
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C                   MOVE      '1'           *INOF
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C**
     C*****************************************************************
     C* Fund totals subroutine-if primary sort is EA or EN.
     C*****************************************************************
     C*
     C     SUBL1         BEGSR
     C*
     C                   ADD       L1TOT         L2TOT             5 0
     C                   ADD       L1PREM        L2PREM           11 2
     C                   ADD       L1DPBL        L2DPBL           11 2
     C                   ADD       L1DPPD        L2DPPD           11 2
     C                   ADD       L1BAL         L2BAL            11 2
     C*
     C                   Z-ADD     0             L1TOT             5 0
     C                   Z-ADD     0             L1PREM           11 2
     C                   Z-ADD     0             L1DPBL           11 2
     C                   Z-ADD     0             L1DPPD           11 2
     C                   Z-ADD     0             L1BAL            11 2
     C*
     C                   ENDSR
     C**
     C**
     C*****************************************************************
     C*   Level 2 - AGENT Totals      ** Subroutine **   S U B L 2   *
     C*****************************************************************
     C*
     C     SUBL2         BEGSR
     C*
     C                   ADD       L1TOT         L2TOT             5 0
     C                   ADD       L1PREM        L2PREM           11 2
     C                   ADD       L1DPBL        L2DPBL           11 2
     C                   ADD       L1DPPD        L2DPPD           11 2
     C                   ADD       L1BAL         L2BAL            11 2
     C*
     C   OF              EXCEPT    HEAD
     C   OF              EXCEPT    HEADH
     C                   SETOFF                                       OF
     C                   EXCEPT    TOTL2
     C*
     C* Initialize Agent Total Fields
     C*
     C*                    Z-ADD0         PAGE
     C                   Z-ADD     0             L1TOT             5 0
     C                   Z-ADD     0             L1PREM           11 2
     C                   Z-ADD     0             L1DPBL           11 2
     C                   Z-ADD     0             L1DPPD           11 2
     C                   Z-ADD     0             L1BAL            11 2
     C*
     C                   ENDSR
     C**
     C*****************************************************************
     C*   Level 3 - FUND Totals         ** Subroutine **   S U B L 3  *
     C*****************************************************************
     C*
     C     SUBL3         BEGSR
     C*
     C                   EXCEPT    TOTL3
     C*                    Z-ADD0         PAGE
     C                   Z-ADD     0             L2TOT
     C                   Z-ADD     0             L2PREM
     C                   Z-ADD     0             L2DPBL
     C                   Z-ADD     0             L2DPPD
     C                   Z-ADD     0             L2BAL
     C*
     C                   ENDSR
     C**
     O*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14  **********************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14  **********************************
      ***** Beg Add ***** 08/05/15 ***********************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ***********************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14  **********************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14  **********************************
     O*** BEGIN DEL *** 01/16/98 **************************************
     O******* E  2             ROUTE
     O********************************** 18 'POLICY YEAR:'
     O************************ XXFYR Z + 14
     O******* E  2             ROUTE
     O******************************** +  6 'RUN DATE:'
     O************************ XXDATE  + 17 '  /  /    '
     O******* E  2             ROUTE
     O******************************** +  6 'REPORT IN EMPLOYER ORDER'
     O******************************** +  0 ': Y'
     O*** END   DEL *** 01/16/98 **************************************
     O*** BEGIN ADD *** 01/16/98 **************************************
     O*** BEGIN CHG *** 02/20/98 **************************************
     O******* E  2             ROUTE
     O          E            ROUTE       1  2
     O*** END   CHG *** 02/20/98 **************************************
     O                                           28 'POLICY EFFECTIVE DATE '
     O                                           34 'RANGE:'
     O                       XXPFDT              45 '  /  /    '
     O                                           48 '-'
     O                       XXPTDT              59 '  /  /    '
     O          E            ROUTE          2
     O                                           18 'BALANCE DATE'
     O                                           34 ':'
     O                       XXDATE              45 '  /  /    '
     O          E            ROUTE          2
     O                                           18 'ACCOUNT TYPE'
     O                                           34 ':'
      *** BEGIN CHG *** 02/20/98 *************************************
     O************************ QQEMTP    36
     O                       RSEMTP              70
      *** END   CHG *** 02/20/98 *************************************
     O          E            ROUTE          2
     O                                           20 'ACCOUNT STATUS'
     O                                           34 ':'
      *** BEGIN CHG *** 02/20/98 *************************************
     O************************ QQRDTS    36
     O                       RSRDTS              70
      *** END   CHG *** 02/20/98 *************************************
     O          E            ROUTE          2
     O                                           18 'PRIMARY SORT'
     O                                           34 ':'
      *** BEGIN CHG *** 02/20/98 *************************************
     O************************ QQPSRT    37
     O                       RSPSRT              49
      *** END   CHG *** 02/20/98 *************************************
     O          E            ROUTE          2
     O                                           20 'SECONDARY SORT'
     O                                           34 ':'
      *** BEGIN CHG *** 02/20/98 *************************************
     O************************ QQSSRT    37
     O                       RSSSRT              49
      *** END   CHG *** 02/20/98 *************************************
     O          E            ROUTE          2
     O                                           30 'SELECTION ONE RANGE TYPE'
     O                                           34 ':'
     O                       QQRGTP              37
     O          E    53      ROUTE          2
     O                                           27 'EMPLOYER NAME RANGE'
     O                                           34 ':'
     O                       XXENLR              39
     O                                           41 '-'
     O                       XXENHR              44
     O          E    54      ROUTE          2
     O                                           29 'EMPLOYER NUMBER RANGE'
     O                                           34 ':'
     O                       XXEBLR        Z     46
     O                                           48 '-'
     O                       XXEBHR        Z     58
     O          E            ROUTE          2
     O                                           30 'SELECTION TWO RANGE TYPE'
     O                                           34 ':'
     O                       QQRGT2              37
     O          E    55      ROUTE          2
     O                                           24 'AGENT NAME RANGE'
     O                                           34 ':'
     O                       XXANLR              39
     O                                           41 '-'
     O                       XXANHR              44
     O          E    56      ROUTE          2
     O                                           26 'AGENT NUMBER RANGE'
     O                                           34 ':'
     O                       XXABLR        Z     42
     O                                           44 '-'
     O                       XXABHR        Z     50
     O*** END   ADD *** 01/16/98 **************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                              +000 '(C)'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEADH          1 06
     O                       DSTIME              08 '  :  :  '
     O                       XCNAME             129
     O*** BEGIN CHG *** 01/16/98 **************************************
     O************************ DSDATEY  132
     O                       XXDSDT             198 '  /  /    '
     O*** END   CHG *** 01/16/98 **************************************
     O          E            HEADH          1
     O                       Q1PGM               10
     O                       ORTITL             130
     O*** BEGIN ADD *** 04/19/01 **************************************
     O                                          163 'BALANCE DATE:'
     O                       XXDATE             174 '  /  /    '
     O*** END   ADD *** 04/19/01 **************************************
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O*** BEGIN DEL *** 01/16/98 **************************************
     O******* E  2             HEAD
     O********************************** 70 'POLICY YEAR'
     O************************ XXFYR2Z +  2
     O*** END   DEL *** 01/16/98 **************************************
     O          E            HEADH          1
     O*** BEGIN DEL *** 04/19/01 **************************************
     O********************************  186 'BALANCE DATE:'
     O*** END   DEL *** 04/19/01 **************************************
     O*** BEGIN CHG *** 01/16/98 **************************************
     O************************ X1PRDTY  132
     O*** BEGIN DEL *** 04/19/01 **************************************
     O***********************  XXDATE   198 '  /  /    '
     O*** END   DEL *** 04/19/01 **************************************
     O*** END   CHG *** 01/16/98 **************************************
     O*** BEGIN ADD *** 04/19/01 **************************************
     O                                           22 'POLICY EFFECTIVE DATE '
     O                                           28 'RANGE:'
     O                       XXPFDT              40 '  /  /    '
     O                                           42 '-'
     O                       XXPTDT              53 '  /  /    '
     O                                           74 'ACCOUNT STATUS'
     O                                           75 ':'
     O                       RSRDTS             112
     O                                          123 'ACCOUNT TYPE:'
     O                       RSEMTP             160
     O          E            HEADH          1
     O                                           13 'PRIMARY SORT:'
     O                       RSPSRT              28
     O                                           75 'SECONDARY SORT:'
     O                       RSSSRT              90
     O               53                         130 'EMPLOYER NAME RANGE:'
     O               53      XXENLR             134
     O               53                         136 '-'
     O               53      XXENHR             139
     O               54                         132 'EMPLOYER NUMBER RANGE:'
     O               54      XXEBLR        Z    142
     O               54                         144 '-'
     O               54      XXEBHR        Z    154
     O               55                         181 'AGENT NAME RANGE:'
     O               55      XXANLR             185
     O               55                         187 '-'
     O               55      XXANHR             190
     O               56                         183 'AGENT NUMBER RANGE:'
     O               56      XXABLR        Z    190
     O               56                         192 '-'
     O               56      XXABHR        Z    198
     O*** END   ADD *** 04/19/01 **************************************
     O          E            HEADH          1
     O                                              'GROUP:'
     O                       XXFND         Z   +  1
     O                       FNNAME            +  2
     O*** BEGIN CHG *** 01/16/98 **************************************
     O******* E  1             HEAD
     O********************************** 59 'CANCEL'
     O******* E  2             HEAD
     O********************************** 09 'EMPLOYER'
     O********************************** 15 'DIVISION'
     O********************************** 30 'EMPLOYER NAME'
     O********************************** 58 'DATE'
     O********************************** 76 'NORMAL PREM'
     O********************************** 94 'DEP BILLED'
     O**********************************112 'DEP RECVD'
     O**********************************130 'BALANCE'
     O          E            HEAD           1
     O                                           67 'CANCEL'
     O                                           80 'NORMAL'
     O                                           97 'DEPOSIT'
     O                                          112 'DEPOSIT'
     O                                          135 'BS'
     O                                          140 'BS'
     O                                          151 'BS CODE'
     O                                          180 'PROD'
     O                                          191 'PRODUCER'
     O          E            HEAD           2
     O                                           09 'EMPLOYER'
     O                                           15 'DIV'
     O                                           30 'EMPLOYER NAME'
     O                                           65 'DATE'
     O                                           81 'PREMIUM'
     O                                           96 'BILLED'
     O                                          113 'RECEIVED'
     O                                          129 'BALANCE'
     O                                          136 'CDE'
     O                                          141 'TYP'
     O                                          155 'DESCRIPTION'
     O                                          181 'NUMBR'
     O                                          187 'NAME'
     O*** END   CHG *** 01/16/98 **************************************
     O*** BEGIN ADD *** 01/16/98 **************************************
     O          E            PROD        1  2
     O                                            6 'AGENT:'
     O                       EBANAM              48
     O                                           63 'AGENT NUMBER:'
     O                       EBAGT#        Z     69
     O*** END   ADD *** 01/16/98 **************************************
     O*
     O*** BEGIN DEL *** 01/16/98 **************************************
     O******* E  106           HEAD3H
     O************************ DSTIME    08 '  :  :  '
     O************************ XCNAME   129 '  :  :  '
     O************************ XXDSDT   178 '  /  /  ' '
     O******* E  1             HEAD3H
     O************************ Q1PGM     10
     O************************ ORTITL   139
     O************************          172 'PAGE:'
     O************************ PAGE  Z  178
     O******* E  1             HEAD3H
     O************************          166 'RUN DATE:'
     O************************ XXDATE   178 '  /  /    '
     O******* E  2             HEAD3H
     O************************              'GROUP:'
     O************************ XXFND Z +  1
     O************************ FNNAME  +  2
     O******* E  2             HEAD3
     O************************           76 'NORMAL PREM'
     O************************           94 'DEP BILLED'
     O************************          112 'DEP RECVD'
     O************************          130 'BALANCE'
     O*** END   DEL *** 01/16/98 **************************************
     O*
     O* Detail Line
     O*
     O          E            DETL2          1
     O                                           94 '***   NO RECORDS MEET'
     O                                         +  1 'SELECTION CRITERIA   ***'
     O*
     O*  Detail Line Specs
     O*
     O          E            DETAIL         1
     O*** BEGIN CHG *** 01/16/98 **************************************
     O************************ EMDIV Z   15
     O************************ EMEMP#Z   09
     O                       EBDIV         Z     15
     O                       EBEMP#        Z     09
     O*** END   CHG *** 01/16/98 **************************************
     O*** BEGIN CHG *** 01/16/98 **************************************
     O************************ XXNAME    51
     O************************ XXCDAT    60 '  /  /  '
     O************************ XXPREMB   78
     O************************ XXDPBLB   96
     O************************ XXDPPDB  114
     O************************ XXBALEB  132
     O                       XXENAM              57
     O                       XXCDAT              67 '  /  /  '
     O                       XXPREM        B     83
     O                       XXDPBL        B     99
     O                       XXDPPD        B    115
     O                       XXBAL         B    131
     O                       ELBSCD             136
     O                       XXSEFL             141
     O                       XXDESC             174
     O                       XXPROD        Z    181
     O                       PNAME              198
     O*** END   CHG *** 01/16/98 **************************************
     O*** BEGIN ADD *** 01/16/98 **************************************
     O          E            TOTL2       1  1
     O                                           30 'AGENT TOTALS:'
     O                                           43 'COUNT:'
     O                       L1TOT         Z     49
     O                       L1PREM        B     83
     O                       L1DPBL        B     99
     O                       L1DPPD        B    115
     O                       L1BAL         B    131
     O*** END   ADD *** 01/16/98 **************************************
     O*
     O*  Fund Total Specs
     O*
     O          E            TOTL3       1  1
     O                                           30 'GROUP TOTALS:'
     O                                           43 'COUNT:'
     O                       L2TOT         Z     49
     O                       L2PREM        B     83
     O                       L2DPBL        B     99
     O                       L2DPPD        B    115
     O                       L2BAL         B    131
