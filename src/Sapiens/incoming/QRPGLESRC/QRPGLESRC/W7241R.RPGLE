     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7241R - RECOVERY REPORT - OUTPUT WORKFILE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/07/05 C:15  Support: WC129/49420                           *
     H*                Chgd pgm to list individual requests instead   *
     H*                of combining like requests.                    *
     H*                                                               *
     H*  11/04/05 C:132 PROJECT: WC129 - 48736                        *
     H*                Delete Code added on 08/30/05, so builds full  *
     H*                work file, only print pgm to check for the     *
     H*                Employer Range if Sort = "N".                  *
     H*                                                               *
     H*  08/30/05 C:132 PROJECT: WC129 - 48736                        *
     H*                If Sort = 'N', and Employer Range entered,     *
     H*                Add edit for Employer Range.                   *
     H*                                                               *
     H*  JJJ REVISED 3-3-89 ADD EMP#; COUNTY; CHAPTER; & AGENT SORT   *
     H*  JJJ REVISED 5-29-90 ALLOW SELECTION ON SDF STATUS, SDF FIELD *
     H*                      ADJUSTER, & ATTORNEY                     *
     H*                                                               *
     H*  06/13/94 C:15 PROJECT: WC322                                 *
     H*                FIXED CODE WHICH CHECKS THE SDF CATEGORY.      *
     H*                                                               *
     H* 06/26/97 C:62 PROJECT WC710                                   *
     H*               COUNTY CODE NOW IS 4A, STATE + COUNTY.          *
     H*               ALTERNATE COMPANY NAME IF NOT BLANK.            *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*       *IN60, 60, 61 - FILE INDICATORS                         *
     H*       *IN25 - SORT SELECTION CRITERIA MET                     *
     H*       *IN26 - CODE, FIELD ADJ, ATTY SELECTION CRITERIA MET    *
     H*       *IN28 - SELECT STATUS                                   *
     H*       *IN29 - SELECT FIELD ADJUSTER                           *
     H*       *IN30 - SELECT ATTORNEY                                 *
     H*****************************************************************
     FWMCML1    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMRRL1    IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWXREP     O    E             DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*** BEGIN CHG *** 06/26/97 **************************************
     D**********  DS
     D**********                              1  66 DATA
     D**********                              1   1 QQSORT
     D**********                              2  100QQFEMP
     D**********                             11  190QQTEMP
     D**********                             20  240QQFDIV
     D**********                             25  290QQTDIV
     D**********                             30  31 QQFCNT
     D**********                             32  33 QQTCNT
     D**********                             34  360QQFCHP
     D**********                             37  390QQTCHP
     D**********                             40  440QQFAGT
     D**********                             45  490QQTAGT
     D**********                             50  51 QQSTAT
     D**********                             52  54 QQFEAD
     D**********                             55  630QQATTY
     D**********                             64  660QQMOD
     D                 DS
     D  DATA                   1     70
     D  QQSORT                 1      1
     D  QQFEMP                 2     10  0
     D  QQTEMP                11     19  0
     D  QQFDIV                20     24  0
     D  QQTDIV                25     29  0
     D  QQFCNT                30     33
     D  QQTCNT                34     37
     D  QQFCHP                38     40  0
     D  QQTCHP                41     43  0
     D  QQFAGT                44     48  0
     D  QQTAGT                49     53  0
     D  QQSTAT                54     55
     D  QQFEAD                56     58
     D  QQATTY                59     67  0
     D  QQMOD                 68     70  0
     D*** END   CHG *** 06/26/97 **************************************
     D* EMPLOYER / DIVISION RANGES
     D                 DS
     D  XXFKEY                 1     14  0
     D  XXFEMP                 1      9  0
     D  XXFDIV                10     14  0
     D                 DS
     D  XXTKEY                 1     14  0
     D  XXTEMP                 1      9  0
     D  XXTDIV                10     14  0
     D                 DS
     D  XXKEY                  1     14  0
     D  EMEMP#                 1      9  0
     D  EMDIV                 10     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE CLAIMS FILE - USED TO GET EMP# & DIV
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RRFYR
     C                   KFLD                    RRCASE
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE - USED TO GET COUNTY
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMO FILE - TO GET CLAIMANT NAME
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR REQUEST REFUND FILE
     C*
     C     RRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     RRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RRFYR
     C                   KFLD                    RRCASE
     C                   KFLD                    RRFEIN
     C                   KFLD                    RRMOD
     C*
     C*   KEY LIST FOR PAYEE DEMOGRAPHIC FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    RRFEIN
     C                   KFLD                    RRMOD
     C*                                                    JJJ 3-3-89
     C*  AGENT HISTORY FILE                                |
     C*                                                    |
     C     AHKEY         KLIST                                                  |
     C                   KFLD                    XXCO#                          |
     C                   KFLD                    XXFND                          |
     C                   KFLD                    EMEMP#                         |
     C                   KFLD                    EMDIV                          |
     C                   KFLD                    RRFYR                          |
     C*
     C*   KEY LIST FOR TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKY1            10
     C*
     C*** BEGIN ADD *** 06/26/97 **************************************
     C     *LIKE         DEFINE    RECNTY        XXCNTY
     C*** END   ADD *** 06/26/97 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*** BEGIN CHG *** 06/26/97 **************************************
     C**********           PARM           QQDATA 66
     C                   PARM                    QQDATA           70
     C*** END   CHG *** 06/26/97 **************************************
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
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C*** BEGIN CHG *** 06/26/97 **************************************
     C**********           MOVE QQDATA    DATA   66        |
     C                   MOVEL     QQDATA        DATA
     C*** END   CHG *** 06/26/97 **************************************
     C                   MOVE      QQFEMP        XXFEMP                         |
     C                   MOVE      QQFDIV        XXFDIV                         |
     C                   MOVE      QQTEMP        XXTEMP                         |
     C                   MOVE      QQTDIV        XXTDIV                         |
     C*                                                    |
     C*  CHECK SELECTION ON COUNTY CODE                    |
     C*                                                    |
     C     QQSTAT        COMP      *BLANKS                                28
     C     QQFEAD        COMP      *BLANKS                                29
     C     QQATTY        COMP      *ZERO                                  30
     C                   Z-ADD     0             XXCASE            5 0
     C*
     C*  START READING REQUEST REFUND FILE - EXCLUDE INACTIVE RECORDS
     C*
     C     RRKEY         CHAIN     PRR01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  GET REFUND STATUS CATEGORY
     C*
     C                   MOVE      *BLANKS       XXKY1
     C                   MOVEL     RRSTCD        XXKY1
     C     TBKEY         CHAIN     PRS01                              60
     C*
     C     RRSTAT        IFNE      'I'
     C     RSCTGY        ANDNE     'X'
     C*
     C*  CHECK SELECTION ON CODE, FIELD ADJUSTER, & ATTORNEY
     C*
     C                   MOVE      '1'           *IN25
     C                   MOVE      '0'           *IN26
     C                   EXSR      SELECT
     C     *IN26         IFEQ      '1'
     C*
     C*  MAKE SURE PAYEE TYPE IS MED, SDF, OR OTHER.
     C*
     C     PDKEY         CHAIN     PPD01                              61
     C     *IN61         IFEQ      '0'
     C*
     C     PDTYPE        IFEQ      '2'
     C     PDTYPE        OREQ      '3'
     C     PDTYPE        OREQ      '5'
     C*
     C*  CHAIN TO CLAIM TO RETRIEVE EMP#, DIV, & SS#
     C*
     C     CMKEY         CHAIN     PCM01                              61
     C     *IN61         IFEQ      '1'
     C                   MOVE      *BLANKS       CMSS#
     C                   Z-ADD     0             CMEMP#
     C                   Z-ADD     0             CMDIV
     C                   END
     C*
     C*  IF A COUNTY WAS SELECTED, CHECK EMP FILE TO SEE IF IT MATCHES
     C*
     C*   GET EMPLOYER & AGENT RECORD & CHECK SORT SELECTION
     C*                                                    |
     C                   EXSR      SLCT                                         |
     C*                                                    |
     C*  CONTINUE IF SORT SELECTION CRITERIA IS MET, OTHERWISE GET
     C*  THE NEXT CLAIM                                    |
     C*                                                    |
     C     *IN25         IFEQ      '0'
     C*
     C*  GET CLAIMANT NAME
     C*
     C     CMCASE        IFNE      XXCASE
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       QQLAST                         LJQ 12-28-88
     C                   MOVE      *BLANKS       QQFRST                         |
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        RENAME           40
     C                   ELSE
     C                   MOVE      *BLANK        RENAME
     C                   END
     C                   END
     C*
     C*  MOVE INFORMATION TO WORKFILE
     C*
     C                   MOVE      XXCO#         RECO#
     C                   MOVE      XXFND         REFND
     C                   MOVE      RRFYR         REFYR
     C                   MOVE      RRCASE        RECASE
     C                   MOVE      RRFEIN        REFEIN
     C                   MOVE      RRMOD         REMOD
     C                   MOVE      EMEMP#        REEMP#                         JJJ 3-3-89
     C                   MOVE      EMDIV         REDIV                          |
     C*** BEGIN CHG *** 06/26/97 **************************************
     C**********           MOVE EMCNTY    RECNTY           |
     C                   MOVEL     XXCNTY        RECNTY                         |
     C*** END   CHG *** 06/26/97 **************************************
     C                   MOVE      EMCHAP        RECHAP                         |
     C                   MOVE      AHAGT#        REAGT#                         |
     C*
     C*  CHECK FOR MORE THAN 1 REQUEST FOR SAME CLAIM & PROVIDER
     C*
     C                   MOVE      '0'           *IN50
     C                   Z-ADD     0             XXAMTR            9 2
     C                   Z-ADD     0             XXAMTC            9 2
     C*** Begin dlt *** 11/07/05 **************************************
     C******     *IN50     DOWEQ'0'
     C*
     C*  GET REFUND STATUS RECORD
     C*
     C******               MOVE *BLANKS   XXKY1
     C******               MOVELRRSTCD    XXKY1
     C******     TBKEY     CHAINPRS01                60
     C*
     C******     RRSTAT    IFNE 'I'
     C******     RSCTGY    ANDNE'X'
     C*                                                    JJJ 5-30-90
     C*  CHECK SELECTION ON CODE,  & ATTORNEY              *
     C*                                                    *
     C******               MOVE '0'       *IN26            *
     C******               EXSR SELECT                     *
     C******     *IN26     IFEQ '1'                        *
     C*** End dlt ***** 11/07/05 **************************************
     C*                                                    *
     C                   ADD       RRRAMT        XXAMTR
     C                   ADD       RRRCVD        XXAMTC
     C                   MOVE      RRDTRE        REDTFI
     C                   Z-ADD     RREAMT        REEAMT
     C                   MOVE      RRSTCD        RESTCD
     C                   MOVE      *BLANKS       RECOMM
     C                   MOVEL     RRCOM1        RECOMM
     C                   Z-ADD     RRATTY        REATTY                         JJJ 5-30-90
     C                   Z-ADD     RRAMOD        REAMOD                         *
     C*** Begin dlt *** 11/07/05 **************************************
     C******               END                             *
     C******               END
     C*
     C******     RRKY2     READEPRR01                    50
     C******               END
     C*** End dlt ***** 11/07/05 **************************************
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXAMTR        RERAMT
     C                   Z-ADD     XXAMTC        RERCVD
     C                   MOVE      *BLANKS       RESTAT
     C                   MOVEL     Q1USER        REUSER
     C                   MOVEL     Q1PGM         REPGM
     C                   Z-ADD     DSTIME        RETIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         REDT
     C*
     C                   WRITE     PRE01
     C*
     C                   END                                                    *IN25 = '0'
     C*
     C*** Begin dlt *** 11/07/05 **************************************
     C******               Z-ADDCMCASE    XXCASE
     C******     RRKY2     SETGTPRR01
     C*** End dlt ***** 11/07/05 **************************************
     C*
     C                   END                                                    PDTYPE
     C                   END                                                    *IN61 PD01
     C                   END                                                    *IN26 = '1'
     C                   END                                                    RRSTAT NE I
     C     RRKEY         READE     PRR01                                  60
     C                   END                                                    *IN60 PRR01
     C*
     C                   MOVE      '1'           *INLR
     C**
     C**************************************************************
     C*       SLCT SUBROUTINE - ADDED JJJ 3-3-89
     C*       SUBROUTINE TO GET EMPLOYER MASTER FILE, AGENT HISTORY
     C*       AND CHECK SELECTION CRITERIA
     C**************************************************************
     C     SLCT          BEGSR
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     0             EMEMP#
     C                   Z-ADD     0             EMDIV
     C                   MOVE      *BLANKS       EMCNTY
     C                   Z-ADD     0             EMCHAP
     C*** BEGIN ADD *** 06/26/97 **************************************
     C                   ELSE
     C*
     C                   MOVE      EMCNTY        XXCNTY
     C*
     C     EMPST         IFNE      *BLANKS
     C*
     C                   MOVEL     EMPST         XXCNTY
     C*
     C                   ELSE
     C*
     C                   MOVEL     EMMST         XXCNTY
     C*
     C                   ENDIF                                                  EMPST
     C*** END   ADD *** 06/26/97 **************************************
     C                   END
     C*
     C     AHKEY         SETGT     PAH01
     C                   READP     PAH01                                  60
     C     *IN60         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  60
     C                   END
     C     *IN60         IFEQ      '1'
     C     AHCO#         ORNE      XXCO#
     C     AHFND         ORNE      XXFND
     C     AHEMP#        ORNE      EMEMP#
     C     AHDIV         ORNE      EMDIV
     C     AHFYR         ORNE      RRFYR
     C                   Z-ADD     0             AHAGT#
     C                   END
     C*
     C     QQSORT        IFEQ      'N'
      ***** BEG DEL ***** 11/04/05 ***************************************
      ***** BEG ADD ***** 08/30/05 ***************************************
     C*********  QQFEMP    ANDEQ*ZEROS
      ***** END ADD ***** 08/30/05 ***************************************
      ***** BEG DEL ***** 11/04/05 ***************************************
     C                   MOVE      '0'           *IN25
     C                   END
     C*
      ***** BEG DEL ***** 11/04/05 ***************************************
      ***** BEG ADD ***** 08/30/05 ***************************************
      * If Claiment Sort and Emp Range Entered, Check Employer in Range
     C*******    QQSORT    IFEQ 'N'
     C*******    QQFEMP    ANDGT*ZEROS
     C*******    EMEMP#    ANDGEQQFEMP
     C*******    EMEMP#    ANDLEQQTEMP
     C*******              MOVE '0'       *IN25
     C*******              END
      ***** END ADD ***** 08/30/05 ***************************************
      ***** BEG DEL ***** 11/04/05 ***************************************
     C*
     C     QQSORT        IFEQ      'E'
     C     XXKEY         IFGE      XXFKEY
     C     XXKEY         ANDLE     XXTKEY
     C                   MOVE      '0'           *IN25
     C                   END
     C                   END
     C*
     C     QQSORT        IFEQ      'C'
     C*** BEGIN CHG *** 06/26/97 **************************************
     C********** EMCNTY    IFGE QQFCNT
     C********** EMCNTY    ANDLEQQTCNT
     C     XXCNTY        IFGE      QQFCNT
     C     XXCNTY        ANDLE     QQTCNT
     C*
     C                   MOVE      '0'           *IN25
     C*
     C*** END   CHG *** 06/26/97 **************************************
     C                   END
     C                   END
     C*
     C     QQSORT        IFEQ      'H'
     C     EMCHAP        IFGE      QQFCHP
     C     EMCHAP        ANDLE     QQTCHP
     C                   MOVE      '0'           *IN25
     C                   END
     C                   END
     C*
     C     QQSORT        IFEQ      'A'
     C     AHAGT#        IFGE      QQFAGT
     C     AHAGT#        ANDLE     QQTAGT
     C                   MOVE      '0'           *IN25
     C                   END
     C                   END
     C*
     C                   ENDSR
     C**
     C**************************************************************
     C*       SELECT SUBROUTINE - ADDED JJJ 5-30-90
     C*       SUBROUTINE TO CHECK SELECTION ON STATUS CODE, SDF FIELD
     C*       ADJUSTER, & ATTORNEY
     C**************************************************************
     C     SELECT        BEGSR
     C*
     C*  CHECK STATUS: DON'T INCLUDE WITHDRAWN TYPES UNLESS CODE WAS
     C*                SELECTED ON PROMPT
     C*
     C  N28RRSTCD        COMP      QQSTAT                                 26
     C*** Begin chg *** 11/07/05 **************************************
     C***** 28   RSTYPE    COMP 'W'                  2626
     C   28              SETON                                            26
     C*** End chg ***** 11/07/05 **************************************
     C*
     C*  CHECK ATTORNEY SELECTION
     C*
     C     *IN26         IFEQ      '1'
     C  N30RRATTY        COMP      QQATTY                                 26
     C  N30
     CAN 26RRAMOD        COMP      QQMOD                                  26
     C   30              MOVE      '1'           *IN26
     C                   END
     C*
     C                   ENDSR
