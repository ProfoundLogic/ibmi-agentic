     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - FA003RC- WRITE/UPDATE WCMC CLAIM MASTER INFO BASED ON WCMS DATA
     H*                FEE SHEDULE VERSION
     H*                CONVERSION VERSION
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA003RC                                           *
     H*                                                               *
     H*    CREATED: 11/15/96  C:45                                    *
     H*    DESCRIPTION: WRITE/UPDATE WCMC CLAIM MASTER INFO BASED ON  *
     H*                 WCMS DATA                                     *
     H*                                                               *
     H*    '*' - Request code: Caller request to close files          *
     H*          No WCMC Employer record Updated or Written.          *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  11/07/24 C:333 PROJECT: 114WEX                               *
     H*                 Claims Import Process Changes                 *
     H*                                                               *
     H*  08/19/11 C:79 PROJECT: 114MYS
     H*                LOAD STATUS, ETC ON AN UPDATE
     H*                                                               *
     H*  06/12/07 C:62 PROJECT: MC144 58628
     H*                UPDATE CMSS#.
     H*                                                               *
     H*  12/12/06 C:62 PROJECT: 144MAC
     H*                -HANDLE CORPORATE SYSTEMS CLAIM#
     H*                -HANDLE STANDALONE CLAIMS
     H*                                                               *
     H*  05/05/01 C:58 PROJECT: MCXXX
     H*            ADD PARM STAT Actual claim status at conversion.
     H*
     H*  11/29/00 C:58 PROJECT: MCXXX
     H*            ADD PARM RTCD = '*' -- To close files at end of
     H*                                                               *
     H*  09/07/00 C:062 PROJECT: MC232                                *
     H*                 CHECK TO SEE IF JURISDICTION STATE IS         *
     H*                 INSTALLED IN THE WCMS/FSMS SYSTEM.            *
     H*                                                               *
     H*  08/25/00 C:062 PROJECT: MC232                                *
     H*                 ADD JURISDICTION STATE TO PARM LIST.          *
     H*                                                               *
     H*  04/03/00 C:118 PROJECT: WC854                                *
     H*                 ADD NEW ENTRY PARM FOR IMPAIRMENT DETERMINED. *
     H*                 TEST Q1PRMS.                                  *
     H*                                                               *
     H*  08/27/99 C:53 PROJECT: MCMNT                                 *
     H*                CHANGE QQSCL# LENGTH FROM 10 TO 25             *
     H*                                                               *
     H*  02/26/99 C:53 PROJECT: MC145                                 *
     H*                TRANSLATE FIELDS CMMCA, CMODIS, CMPRES         *
     H*                                                               *
     H*  02/09/99 C:53 PROJECT: MC145                                 *
     H*                ACTIVATE THE LOAD OF CMCMNU                    *
     H*                                                               *
     H*  01/20/99 C:53 PROJECT: MC178                                 *
     H*                REMOVE CODE THAT INITIALIZES FIELDS            *
     H*                                                               *
     H*  10/15/98 C:53 PROJECT: 87 CONVERSION                         *
     H*                REMOVE CODE THAT INITIALIZES FIELDS            *
     H*                REMOVE FIELDS WCMS DOES NOT HOLD               *
     H*                                                               *
     H*  09/22/98 C:53 PROJECT: MC101 # 13639                         *
     H*                DEFAULT CMPEND AND CMPREC TO "N" ON AN ADD     *
     H*                                                               *
     H*  08/10/98 C:62 PROJECT: MC155                                 *
     H*                ADDED PARM FOR CMCLFA.                         *
     H*                                                               *
     H*  07/09/98 C:62 PROJECT: MC155                                 *
     H*                MOVED CMEMP# AND CMDIV BEFORE Q1PRMS           *
     H*                                                               *
     H*  04/07/98 C:62 PROJECT: MC155                                 *
     H*                ADDED FIELDS FOR LINK W/WCMS                   *
     H*                                                               *
     H*  02/05/98 C:62 PROJECT: MC145                                 *
     H**************** 1) WRITE CLAIM HISTORY USING API FA004R.       *
     H**************** 2) ONLY WRITE NEW CLAIMS, NO UPDATES.          *
     H**************** 3) ONLY WRITE NEW CLAIMS, NO UPDATES.          *
     H**************** 4) ADDED INIT FIELD FOR TIME.                  *
     H*                                                               *
     H*  12/29/97 C:62 PROJECT: MC162                                 *
     H*                PROHIBIT WRITE/UPDATE IF FUND MASTER IS NOT    *
     H*                FOUND IN WCMC.                                 *
     H*                                                               *
     H*  06/27/97 C:56 PROJECT: 87MAZ                                 *
     H*                ADD FIELD QQRRN                                *
     H*                                                               *
     H*  06/18/97 C:56 PROJECT 87MAZ - F0270R                         *
     H*                ADD FIELD CLAD                                 *
     H*                                                               *
     H*  03/28/97 C:53 PROJECT 87MAT                                  *
     H*                ADD FIELDS CMNU,CMSU                           *
     H*                                                               *
     H*  03/21/97 C:61 PROJECT 87WBT                                  *
     H*                Revise WCMS Claims Master Maintenance          *
     H*                to accomodate all 148 EDI required             *
     H*                fields so user does not have to go over to     *
     H*                WCMC Case Management Maintenance screens to    *
     H*                fill initial 148 required fields.              *
     H*                                                               *
     H*  02/13/97 C:53 Req/Proj: MCMNT                                *
     H*                MOVED COMMON FIELDS TO ONE PLACE               *
     H*                ADD FUND FILE TO GET FUND/SUB# TRANSLATION     *
     H*                IF QQSUB# IS = *BLANKS                         *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*    60 - 61    GENERAL USAGE (CHAIN)                           *
     H*                                                               *
     H*****************************************************************
     F*
     FFMCML1    UF A E           K DISK
     F                                     INFDS(DBFDS1)
     FFMFNL1    IF   E           K DISK
     F*** BEGIN ADD *** 09/07/00 **************************************
     FSTSTL     IF   E           K DISK
     F*** END   ADD *** 09/07/00 **************************************
     F*** BEGIN ADD *** 12/12/06 **************************************
     FFMCML14   UF A E           K DISK
     F                                     RENAME(PCM01:PCM02)
     F                                     INFDS(DBFDS2)
     FFMC1L     UF   E           K DISK
     F*** END   ADD *** 12/12/06 **************************************

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D*** BEGIN ADD *** 12/12/06 **************************************
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*** END   ADD *** 12/12/06 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*** BEGIN ADD *** 12/12/06 **************************************
     C*
     C*  WCMC COMPANY AUXLIARY FILE
     C*
     C     C1KY          KLIST
     C                   KFLD                    XXCO#
     C*** END   ADD *** 12/12/06 **************************************
     C*
     C*  CLAIM MASTER KEY LIST
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*** BEGIN ADD *** 12/12/06 **************************************
     C*
     C     CMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSRC
     C*** END   ADD *** 12/12/06 **************************************
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** BEGIN ADD *** 09/07/00 **************************************
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*** END   ADD *** 09/07/00 **************************************
     C*
     C     *ENTRY        PLIST
P01  C                   PARM                    QQRTCD            1
P02  C                   PARM                    QQSS#             9
P03  C                   PARM                    QQACDT            7
P04  C                   PARM                    QQCO#             3
P05  C                   PARM                    QQSUB#            5
P06  C                   PARM                    QQFND             3
P07  C                   PARM                    QQFYR             3
P08  C                   PARM                    QQCASE            7
P09  C                   PARM                    QQEMP#            9
P10  C                   PARM                    QQDIV             5
P11  C                   PARM                    QQACTM            6
P12  C                   PARM                    QQDTRP            7
P13  C                   PARM                    QQDTLW            7
P14  C                   PARM                    QQACDS           60
P15  C                   PARM                    QQMCON            1
P16  C                   PARM                    QQENTD            7
P17  C                   PARM                    QQRCVD            7
P18  C                   PARM                    QQMMI             7
P19  C                   PARM                    QQ1A1             1
P20  C                   PARM                    QQ10A1           10
P21  C                   PARM                    QQLTFL            1            ADD 02/17/97
P22  C                   PARM                    QQSCL#           25            CHG 08/27/99
     C**** QQMCTM NOT USED
P23  C                   PARM                    QQMCTM            4            Time Rep MCO
P24  C                   PARM                    QQSTWK            2            Stop Work
P25  C                   PARM                    QQDEDT            7            Dt of Death
P26  C                   PARM                    QQDEEM            2            Emp Deceased
P27  C                   PARM                    QQCMNU            3            NRSE
P28  C                   PARM                    QQCMSU            3            *BLANKS
     C*
     C*** BEGIN ADD *** 06/18/97 **************************************
P29  C                   PARM                    QQCLAD            3            CLEX
P30  C                   PARM                    QQRRN             9 0
P31  C                   PARM                    QQLSN            30
P32  C                   PARM                    QQFSN            15
P33  C                   PARM                    QQMID             1
P34  C                   PARM                    QQDTRE            7
P35  C                   PARM                    QQCLSU            3            SUPR
P36  C                   PARM                    QQ#DEP            3
     C*** BEGIN ADD *** 04/07/98 **************************************
     C*********************************** MC SUFFIX ****** WC SUFFIX
P37  C                   PARM                    QQOCDS           40            OCDS
P38  C                   PARM                    QQHIDT            7 0          HIDT
P39  C                   PARM                    QQESTS            2            ESTS
P40  C                   PARM                    QQWKPT            2            SHFT
P41  C                   PARM                    QQPREX            1            PREX
P42  C                   PARM                    QQACD2           60            ACD2
P43  C                   PARM                    QQCVFL            1            CVFL
P44  C                   PARM                    QQMCA             1            MCO
P45  C                   PARM                    QQMMIP            5 2          IPCT
P46  C                   PARM                    QQODIS            1            ODIS
P47  C                   PARM                    QQYRLX            4 0          YRLX
P48  C                   PARM                    QQDBDT            7 0          DBDT
P49  C                   PARM                    QQHRDY            3 0          HRSD
P50  C                   PARM                    QQDYWK            1 0          DAYS
     C*** END   ADD *** 04/07/98 **************************************
     C*** BEGIN ADD *** 08/10/98 **************************************
P51  C                   PARM                    QQCLFA            3            FEAD
     C*** END   ADD *** 08/10/98 **************************************
     C*** BEGIN ADD *** 01/20/99 **************************************
P52  C                   PARM                    QQDNY             1            CLM DENIED
     C*** END   ADD *** 01/20/99 **************************************
     C***** BEGIN ADD **** 04/03/00 ***********************************
P53  C                   PARM                    QQIMP             1            IMPAIRMENT
     C***** END   ADD **** 04/03/00 ***********************************
     C*** BEGIN ADD *** 08/25/00 **************************************
P54  C                   PARM                    QQJRST            2            JURISDICTION ST
     C*** END   ADD *** 08/25/00 **************************************
     C*** BEGIN ADD *** 05/05/01 **************************************
P55  C                   PARM                    QQSTAT            1            Claim Status ST
     C*** END ADD ***** 05/05/01 **************************************
     C*** BEGIN ADD *** 12/12/06 **************************************
P56  C                   PARM                    QQSRC            20            CS Claim#
     C*** END ADD ***** 12/12/06 **************************************
     C*** Beg Add ***** 11/07/24 **************************************
P57  C                   PARM                    QQPOST            2            Policy State
     C*** End Add ***** 11/07/24 **************************************
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
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMSUB#        XXSUB#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C*** BEGIN ADD *** 09/07/00 **************************************
     C     *LIKE         DEFINE    STKEY         KKKEY
     C*** END   ADD *** 09/07/00 **************************************
     C*** BEGIN ADD *** 12/12/06 **************************************
     C     *LIKE         DEFINE    CMSRC         XXSRC
     C*
     C* RETRIEVE CLIENT ID
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C*
     C* IS THIS TML?
     C*
     C                   MOVE      *OFF          C144              1
     C     ##CLT#        IFEQ      '144'
     C                   MOVE      *ON           C144
     C                   ENDIF
     C*** END   ADD *** 12/12/06 **************************************
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C*
     C**** BEG ADD *** 11/29/00 ***************************************
     C*** IF PARM RTCD = '*' END PGM AND RETURN
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF
     C**** END ADD *** 11/29/00 ***************************************
     C*** BEGIN ADD *** 12/12/06 **************************************
     C     C144          IFEQ      *ON
     C     Q1PRMS        ANDGT     55
     C                   EXSR      CLMUPD
     C                   ELSE
     C*** END   ADD *** 12/12/06 **************************************
     C*
     C     QQSUB#        IFEQ      *BLANKS
     C     FNKY          CHAIN     PFN01                              61
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     FNSUB#        XXSUB#
     C                   Z-ADD     FNFND         XXFND
     C*** BEGIN CHG *** 12/29/97 **************************************
     C                   EXSR      CLMUPD
     C*
     C                   ELSE
     C*
     C                   MOVEL     '1'           QQRTCD
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   EXSR      CLMUPD
     C*
     C                   ENDIF
     C*** END   CHG *** 12/29/97 **************************************
     C*** BEGIN ADD *** 12/12/06 **************************************
     C                   ENDIF
     C*** END   ADD *** 12/12/06 **************************************
     C*** BEGIN ADD *** 12/29/97 **************************************
     C*
     C*  END THE PROGRAM
     C     TERM          TAG
     C                   RETURN
     C*** BEGIN END *** 12/29/97 **************************************
     C*** END   ADD *** 02/13/97 **************************
     C*** BEGIN CHG *** 12/29/97 **************************************
     C*****************************************************************
     C*    SUBROUTINE - CLMUPD - CLAIM WRITE/UPDATE
     C*****************************************************************
     C     CLMUPD        BEGSR
     C*
     C*** GET CLAIM MASTER IF EXISTS UPDATE ELSE ADD
     C*
     C*** BEGIN ADD *** 12/12/06 **************************************
     C     C144          IFEQ      *ON
     C     Q1PRMS        ANDGT     55
     C                   MOVEL(P)  QQSRC         XXSRC
     C     CMKY2         CHAIN     PCM02                              60
     C                   ELSE
     C*** END   ADD *** 12/12/06 **************************************
     C     CMKEY         CHAIN     PCM01                              60
     C*** BEGIN ADD *** 12/12/06 **************************************
     C                   ENDIF
     C*** END   ADD *** 12/12/06 **************************************
     C*
     C*** MOVE FIELDS COMMON TO ADD AND CHANGE OF RECORD
     C*
     C                   MOVE      QQACDT        CMACDT
     C                   MOVE      QQACTM        CMACTM
     C                   MOVE      QQDTRP        CMDTRP
     C                   MOVE      QQDTLW        CMDTLW
     C                   MOVE      QQACDS        CMACDS
     C                   MOVE      QQMCON        CMMCON
     C                   MOVE      QQENTD        CMENTD
     C                   MOVE      QQRCVD        CMRCVD
     C                   MOVE      QQDEDT        CMDEDT
     C                   MOVE      QQMMI         CMMMI
     C*
     C                   MOVE      QQLTFL        CMLTFL
     C                   MOVEL     QQSCL#        CMSCL#
     C                   MOVE      QQCMNU        CMCMNU                         CHG 02/09/99
     C****                 MOVE QQCMSU    CMCMSU
     C*
     C     QQDEEM        IFEQ      'Y '
     C                   MOVE      'DI'          CMDEEM                         Emp Deceased
     C                   ENDIF
     C*** BEGIN ADD *** 06/18/97 **************************************
     C                   MOVE      QQCLAD        CMCLAD
     C                   MOVE      QQLSN         CMLSN
     C                   MOVE      QQFSN         CMFSN
     C                   MOVE      QQMID         CMMID
     C                   MOVE      QQCLSU        CMCLSU
     C                   MOVE      QQDTRE        CMDTRE
     C                   MOVE      QQ#DEP        CM#DEP
     C*** BEGIN ADD *** 07/09/98 **************************************
     C                   MOVE      QQEMP#        CMEMP#
     C                   MOVE      QQDIV         CMDIV
     C*** END   ADD *** 07/09/98 **************************************
     C*** BEGIN ADD *** 04/07/98 **************************************
     C     Q1PRMS        IFGT      36
     C*** BEGIN DEL *** 07/09/98 **************************************
     C**********           MOVE QQEMP#    CMEMP#
     C**********           MOVE QQDIV     CMDIV
     C*** END   DEL *** 07/09/98 **************************************
     C                   MOVEL     QQOCDS        CMOCDS
     C                   MOVE      QQHIDT        CMHIDT
     C                   MOVEL     QQESTS        CMESTS
     C                   MOVEL     QQWKPT        CMWKPT
     C                   MOVEL     QQACD2        CMACD2
     C                   MOVEL     QQCVFL        CMCVFL
     C**********           MOVELQQPREX    CMPREX           DEL 02/26/99
     C**********           MOVELQQODIS    CMODIS           DEL 02/26/99
     C**********           MOVELQQMCA     CMMCA            DEL 02/26/99
     C*** BEG ADD *** 02/26/99 **************************************
     C*** ADD TRANSLATION
     C     QQPREX        IFEQ      ' '
     C                   MOVE      'N'           CMPREX
     C                   ELSE
     C                   MOVEL     QQPREX        CMPREX
     C                   ENDIF
     C     QQMCA         IFEQ      ' '
     C                   MOVE      'N'           CMMCA
     C                   ELSE
     C                   MOVEL     QQMCA         CMMCA
     C                   ENDIF
     C     QQODIS        IFEQ      ' '
     C                   MOVE      'N'           CMODIS
     C                   ELSE
     C                   MOVEL     QQODIS        CMODIS
     C                   ENDIF
     C*** END ADD *** 02/26/99 **************************************
     C                   MOVE      QQMMIP        CMMMIP
     C                   MOVE      QQYRLX        CMYRLX
     C                   MOVE      QQDBDT        CMDBDT
     C                   MOVE      QQHRDY        CMHRDY
     C                   MOVE      QQDYWK        CMDYWK
     C                   ENDIF
     C*** END   ADD *** 04/07/98 **************************************
     C*** BEGIN ADD *** 08/10/98 **************************************
     C     Q1PRMS        IFGT      50
     C                   MOVEL     QQCLFA        CMCLFA
     C                   ENDIF
     C*** END   ADD *** 08/10/98 **************************************
     C*** BEG ADD *** 01/20/99 **************************************
     C     Q1PRMS        IFGT      51
     C     QQDNY         IFEQ      'N'
     C                   MOVE      ' '           CMDNY
     C                   ELSE
     C                   MOVEL     QQDNY         CMDNY
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 01/20/99 **************************************
     C***** BEGIN ADD **** 04/03/00 ***********************************
     C     Q1PRMS        IFGT      52
     C                   MOVEL     QQIMP         CMIMP
     C                   ENDIF                                                  Q1PRMS>52
     C***** END   ADD **** 04/03/00 ***********************************
     C*** BEGIN ADD *** 12/12/06 **************************************
     C     C144          IFEQ      *ON
     C     Q1PRMS        ANDGT     55
     C                   MOVEL(P)  QQSRC         CMSRC
     C*** BEGIN ADD *** 06/12/07 **************************************
     C                   MOVEL     QQSS#         CMSS#
     C*** END   ADD *** 06/12/07 **************************************
     C                   ENDIF
     C*** END   ADD *** 12/12/06 **************************************
     C                   MOVE      Q1USER        CMUSER
     C                   MOVE      Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*** BEG   ADD *** 08/19/11 **************************************
     C     Q1PRMS        IFGT      54
     C                   MOVE      QQSTAT        CMSTAT
     C                   ENDIF
     C*** END   ADD *** 08/19/11 **************************************
     C*
     C     *IN60         IFEQ      '1'
     C*
     C*** ADD RECORD
     C*
     C                   MOVE      'W'           QQRTCD
     C                   MOVE      QQCO#         CMCO#
     C                   MOVE      XXSUB#        CMSUB#
     C                   MOVE      XXFND         CMFND
     C*** BEGIN ADD *** 12/12/06 **************************************
     C     C144          IFEQ      *ON
     C     Q1PRMS        ANDGT     55
     C                   MOVEL     QQACDT        CMFYR
     C     C1KY          CHAIN     PC101                              99
     C     *IN99         IFEQ      *OFF
     C                   ADD       1             C1LCS#
     C                   UPDATE    PC101
     C                   MOVEL     C1LCS#        CMCASE
     C                   ENDIF
     C                   ELSE
     C*** END   ADD *** 12/12/06 **************************************
     C                   MOVE      QQFYR         CMFYR
     C                   MOVE      QQCASE        CMCASE
     C*** BEGIN ADD *** 12/12/06 **************************************
     C                   ENDIF
     C*** END   ADD *** 12/12/06 **************************************
     C                   MOVE      QQSS#         CMSS#
     C                   MOVE      QQ1A1         CM1A1
     C                   MOVE      QQ10A1        CM10A1
     C                   MOVE      'ME'          CMRT01
     C                   MOVE      'N'           CMPREC                         CHG 09/22/98
     C                   MOVE      'N'           CMPEND                         ADD 09/22/98
     C                   MOVE      *BLANKS       CMCCL#
     C*** BEGIN DEL *** 08/25/00 **************************************
     C**********           MOVE FNAST     CMAST
     C*** END   DEL *** 08/25/00 **************************************
     C*** BEGIN CHG *** 05/05/01 **************************************
     C**********           MOVE 'O'       CMSTAT
     C                   MOVE      QQSTAT        CMSTAT
     C*** END CHANGE ** 05/05/01 **************************************
     C                   MOVE      CMDT          CMIDT
     C*** BEGIN ADD *** 02/05/98 **************************************
     C                   MOVE      CMTIME        CMITIM
     C                   MOVE      Q1USER        CMIUSR
     C*** END   ADD *** 02/05/98 **************************************
     C*** BEGIN ADD *** 08/25/00 **************************************
     C     Q1PRMS        IFGT      53
     C*** BEGIN ADD *** 09/07/00 **************************************
     C                   MOVEL(P)  QQJRST        KKKEY
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*** END   ADD *** 09/07/00 **************************************
     C                   MOVEL(P)  QQJRST        CMAST
     C*** BEGIN ADD *** 09/07/00 **************************************
     C                   ELSE
     C*
     C                   MOVEL(P)  FNAST         CMAST
     C*
     C                   ENDIF
     C*** END   ADD *** 09/07/00 **************************************
     C*
     C                   ENDIF
     C*** END   ADD *** 08/25/00 **************************************
      //*** Beg Add ***** 11/07/24 ************************************
       If Q1PRMS > 56;
          CMPOST = QQPOST;
       Endif;
      //*** End Add ***** 11/07/24 ************************************
     C*
     C*** BEGIN ADD *** 12/12/06 **************************************
     C     C144          IFEQ      *ON
     C     Q1PRMS        ANDGT     55
     C                   WRITE     PCM02
     C                   ELSE
     C*** END   ADD *** 12/12/06 **************************************
     C                   WRITE     PCM01
     C*** BEGIN ADD *** 12/12/06 **************************************
     C                   ENDIF
     C*** END   ADD *** 12/12/06 **************************************
     C*
     C*** BEGIN ADD *** 02/05/98 **************************************
     C*
     C                   EXSR      SRCHWU
     C*
     C*******************  ENDIF                           DEL 03/05/98
     C*
     C*** END   ADD *** 02/05/98 **************************************
     C*
     C                   ELSE
     C*
     C*** UPDATE RECORD
     C*** BEGIN ADD *** 12/12/06 **************************************
     C     C144          IFEQ      *ON
     C     Q1PRMS        ANDGT     55
     C                   UPDATE    PCM02
     C                   ELSE
     C*** END   ADD *** 12/12/06 **************************************
     C                   UPDATE    PCM01
     C*** BEGIN ADD *** 12/12/06 **************************************
     C                   ENDIF
     C*** END   ADD *** 12/12/06 **************************************
     C*
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 12/12/06 **************************************
     C     C144          IFEQ      *ON
     C     Q1PRMS        ANDGT     55
     C                   Z-ADD     DBRRN2        QQRRN
     C                   ELSE
     C*** END   ADD *** 12/12/06 **************************************
     C                   Z-ADD     DBRRN1        QQRRN
     C*** BEGIN ADD *** 12/12/06 **************************************
     C                   ENDIF
     C*** END   ADD *** 12/12/06 **************************************
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 02/05/98 **************************************
     C*****************************************************************
     C*       SRCHWU - WCMC CLAIM HISTORY WRITE/UPDATE API.
     C*****************************************************************
     C     SRCHWU        BEGSR
     C*
     C***********          CALL 'FA004R'
     C***********          PARM 'R'       ##RTCD  1
     C***********          PARM 'O'       ##OCST  1
     C***********          PARM DBRRN1    ##RRN   90
     C*
     C                   ENDSR
     C*** END   ADD *** 02/05/98 **************************************
     C*****************************************************************
     C**********
     C**********  END THE PROGRAM
     C**********           RETRN
     C*** END   CHG *** 12/29/97 **************************************
