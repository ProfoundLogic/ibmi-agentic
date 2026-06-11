     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7280R - PROMPT FOR FUND, FUND YEAR, AND DATE
     H*
     H*****************************************************************
     H* REVISIONS                                                     *
     H*                                                               *
     H* 11/24/99 C:118 PROJECT: WC577                                 *
     H*                MADE V3 CHANGES TO DISPLAY FILE.               *
     H*                                                               *
     H* 10/31/97 C:87 PROJECT WC763                                   *
     H*               SELECT ACCTS BASED UPON ACTIVE STATUS DURING THE*
     H*                YEAR, NOT ON FYR.                              *
     H*               PROVIDE SORT BY AGENT#/AGENT NAME/EMP#/EMP NAME *
     H*               ALSO PROVIDE SECONDARY SORT WITHIN AGENT. GIVE  *
     H*                RANGE SELECS FOR AGT#/AGT NAME/EMP#/EMP NAME.  *
     H*               ADD: ELPFDT(EFF DATE), ELPTDT(EXP DATE), CANX   *
     H*                    DATE, PHONE#, CONTACT1.                    *
     H*               ADD PARMS FOR PASS OUT OF PGM. ADD F10 SEARCH.  *
     H*               ADD CODE FOR NEW VARIABLES FROM SCREEN AND CODE *
     H*               FOR ERROR HANDLING OF NEW FIELDS.               *
     H*                                                               *
     H*****************************************************************
     FW7280F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     FWMEMP     IF   E             DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
      *** BEGIN ADD *** 10/31/97 **************************************
     FWMAAP     IF   E             DISK
      *** END   ADD *** 10/31/97 **************************************
      *
      *** BEGIN DEL *** 10/31/97 **************************************
     F********* WDF1L   IF  E           K        DISK
      *** END   DEL *** 10/31/97 **************************************

     D*
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*** BEGIN ADD *** 10/31/97 *****************************
     D                 DS
     D  MMCCYY                 1      6  0
     D  XXMM                   1      2  0
     D  XXCCYY                 3      6  0
     D*
     D                 DS
     D  ZZMDCY                 1      8  0
     D  ZZMM                   1      2  0
     D  ZZDD                   3      4  0
     D  ZZCCYY                 5      8  0
     D*
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D*** END   ADD *** 10/31/97 *************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C*
     C*** BEGIN DEL *** 10/31/97 ********************************
     C** FUND YEAR KEY LIST.
     C**********
     C********** F1KEY     KLIST
     C**********           KFLD           UPCO#
     C**********           KFLD           XXFND
     C**********           KFLD           XXFYR   30
     C*** END   DEL *** 10/31/97 ********************************
     C*
     C     *LIKE         DEFINE    SCFAGA        XXFAGA
     C     *LIKE         DEFINE    SCTAGA        XXTAGA
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C                   Z-ADD     0             SCRPMY
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPCO#         @@CO#
     C     *LIKE         DEFINE    EMFND         @@FND
     C     *LIKE         DEFINE    EMEMP#        @@EMP#
     C     *LIKE         DEFINE    EMDIV         @@DIV
     C*
     C                   Z-ADD     0             QQVIEW            1 0
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C***** BEGIN ADD **** 11/24/99 *****************************
     C     UPFND         IFNE      *ZEROS
     C***** END   ADD **** 11/24/99 *****************************
     C                   MOVEL(P)  UPFND         SCFND
     C***** BEGIN ADD **** 11/24/99 *****************************
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       SCFND
     C                   ENDIF                                                  *UPFND<>BLK
     C***** END   ADD **** 11/24/99 *****************************
     C*** BEGIN DEL *** 10/31/97 ********************************
     C******************** MOVE UPFYR     SCFYR
     C*** END   DEL *** 10/31/97 ********************************
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C***** BEGIN ADD **** 11/24/99 ***********************************
     C     SCFND         IFNE      *ZEROS
     C***** END   ADD **** 11/24/99 ***********************************
     C                   MOVE      SCFND         XXFND
     C                   MOVE      UPCO#         XXCO#
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C***** BEGIN ADD **** 11/24/99 ***********************************
     C                   ELSE
     C                   MOVEL     *BLANKS       SCNAME
     C                   ENDIF                                                  SCFND<>BLK
     C***** END   ADD **** 11/24/99 ***********************************
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*** BEGIN CHG *** 10/31/97 ************************************
     C*******************  MOVE *BLANKS   SCSRT            JJJ 3-1-89
     C                   MOVE      *BLANKS       SCPSRT
     C                   MOVE      *BLANKS       SCSSRT
     C                   Z-ADD     0             SCFEM#
     C                   Z-ADD     0             SCTEM#
     C*** END   CHG *** 10/31/97 ************************************
     C                   Z-ADD     0             SCFAGT
     C                   Z-ADD     0             SCTAGT
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     **
      *** BEGIN ADD *** 10/31/97 ********************************
     **  FUNCTION 10= TABLE FILE SEARCH
     **
     C     *INKJ         IFEQ      *ON
     **
      * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  TABLE SEARCH - FROM/TO EMPLOYER NAME/NUMBER
      * * * * * * * * * * * * * * * * * * * * * * * * * *
      *
     C     HFLD          IFEQ      'SCFEMN'
     C     HFLD          OREQ      'SCTEMN'
     C     HFLD          OREQ      'SCFEM#'
     C     HFLD          OREQ      'SCTEM#'
      *
     C     HFLD          IFEQ      'SCFEMN'
     C     HFLD          OREQ      'SCTEMN'
     C                   Z-ADD     1             QQVIEW
     C                   ENDIF
      *
     C     HFLD          IFEQ      'SCFEM#'
     C     HFLD          OREQ      'SCTEM#'
     C                   Z-ADD     2             QQVIEW
     C                   ENDIF
      *
     C                   CALL      'W0050R'
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM      DBRRNF        QQRRN2            9 0
     C                   PARM                    QQQTE             1
     C                   PARM                    QQVIEW
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     **
     C     *INKL         IFEQ      *ON
     C     QQRRN         OREQ      *ZERO
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCFEMN'
     C                   MOVE      *ON           *IN61
      *
     C     HFLD          WHENEQ    'SCTEMN'
     C                   MOVE      *ON           *IN46
      *
     C     HFLD          WHENEQ    'SCFEM#'
     C                   MOVE      *ON           *IN47
      *
     C     HFLD          WHENEQ    'SCTEM#'
     C                   MOVE      *ON           *IN48
      *
     C                   ENDSL
      *
     C     *INKL         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL = *ON
      *
     C                   ENDIF                                                  *INKL=*ON OR QQR
      *
      * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  LOAD EMPLOYER NAME/NUMBER
      * * * * * * * * * * * * * * * * * * * * * * * * * *
      *
     C     QQRRN         IFNE      *ZERO
     **
     C     QQRRN         CHAIN     WMEMP                              60
     **
     C     *IN60         IFEQ      *ZERO
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCFEMN'
     C                   MOVEL     EMNAME        SCFEMN
     C                   MOVE      *ON           *IN46
      *
     C     HFLD          WHENEQ    'SCTEMN'
     C                   MOVEL     EMNAME        SCTEMN
     C                   MOVE      *ON           *IN51
      *
     C     HFLD          WHENEQ    'SCFEM#'
     C                   MOVEL     EMEMP#        SCFEM#
     C                   MOVE      *ON           *IN48
      *
     C     HFLD          WHENEQ    'SCTEM#'
     C                   MOVEL     EMEMP#        SCTEM#
     C                   MOVE      *ON           *IN61
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  *IN60 IFEQ
     **
     C                   ENDIF                                                  QQRRN IFNE
     **
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  HFLD=SCFEMN
      * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  TABLE SEARCH - FROM/TO AGENT NAME/NUMBER
      * * * * * * * * * * * * * * * * * * * * * * * * * *
     C     HFLD          IFEQ      'SCFAGT'
     C     HFLD          OREQ      'SCTAGT'
     C     HFLD          OREQ      'SCFAGA'
     C     HFLD          OREQ      'SCTAGA'
     **
     C     HFLD          IFEQ      'SCFAGT'
     C     HFLD          OREQ      'SCTAGT'
     C                   Z-ADD     2             QQVIEW
     C                   ENDIF
      *
     C     HFLD          IFEQ      'SCFAGA'
     C     HFLD          OREQ      'SCTAGA'
     C                   Z-ADD     1             QQVIEW
     C                   ENDIF
      *
     C                   CALL      'W0040R'
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      DBRRNF        QQRRN             9 0
     C                   PARM                    QQVIEW
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC=*ON
     **
     C     *INKL         IFEQ      *ON
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCFAGT'
     C                   MOVE      *ON           *IN51
     C                   GOTO      STRT
      *
     C     HFLD          WHENEQ    'SCTAGT'
     C                   MOVE      *ON           *IN52
     C                   GOTO      STRT
      *
     C     HFLD          WHENEQ    'SCFAGA'
     C                   MOVE      *ON           *IN49
     C                   GOTO      STRT
      *
     C     HFLD          WHENEQ    'SCTAGA'
     C                   MOVE      *ON           *IN50
     C                   GOTO      STRT
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  *INKL=*ON
     **
     C     QQRRN         IFNE      *ZERO
      *
     C     QQRRN         CHAIN     WMAAP                              60
      *
     C     *IN60         IFEQ      *ZERO
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCFAGT'
     C                   MOVEL(P)  AAAGT#        SCFAGT
     C                   MOVE      *ON           *IN52
      *
     C     HFLD          WHENEQ    'SCTAGT'
     C                   MOVEL(P)  AAAGT#        SCTAGT
     C                   MOVE      *ON           *IN49
      *
     C     HFLD          WHENEQ    'SCFAGA'
     C                   MOVEL     AANAME        SCFAGA
     C                   MOVE      *ON           *IN50
      *
     C     HFLD          WHENEQ    'SCTAGA'
     C                   MOVEL     AANAME        SCTAGA
     C                   MOVE      *ON           *IN62
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  *IN60 EQ *ZERO
      *
     C                   ENDIF                                                  QQRRN NE *ZERO
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD IFEQ SCFAGT..
      *
      *** END   ADD *** 10/31/97 ********************************
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKJ EQ *ON
     **
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     **
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C                   GOTO      STRT01
     C                   ENDIF                                                  *INKM='1'
     C*
     C                   ENDIF                                                  *IN11='1'
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE FUND YEAR ENTERED.
     C*
     C*** BEGIN DEL *** 10/31/97 ********************************
     C**********           Z-ADDSCFYR     XXFYR
     C********** F1KEY     CHAINPF101                60
     C*** END   DEL *** 10/31/97 ********************************
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE THE DATE ENTERED
     C*
     C                   Z-ADD     SCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C                   MOVEL     SRDTO         SCRPMY
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        21
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SRERR EQ '1'
     C*
     C*** BEGIN ADD *** 10/31/97 **************************
     C* CONVERT SCRPMY TO 8 CHAR DATE CCYYMMDD
     C*
     C                   Z-ADD     SCRPMY        MMCCYY
     C                   Z-ADD     XXMM          ZZMM
     C                   Z-ADD     XXCCYY        ZZCCYY
      *
     C* LOAD START DATE
     C                   Z-ADD     1             ZZDD
     C                   MOVE      ZZMDCY        QQSTDT            8
      *
     C* LOAD END DATE
     C                   SELECT
      *
     C     XXMM          WHENEQ    2
     C                   Z-ADD     29            ZZDD
      *
      * * * * * * * VALIDATE FEBRUARY 29 AS END DATE
      *
     C                   Z-ADD     ZZMDCY        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      '1'
     C                   Z-ADD     28            ZZDD
     C                   ENDIF
      *
     C     XXMM          WHENEQ    4
     C     XXMM          OREQ      6
     C     XXMM          OREQ      9
     C     XXMM          OREQ      11
     C                   Z-ADD     30            ZZDD
     C                   OTHER
     C                   Z-ADD     31            ZZDD
     C                   ENDSL                                                  *END SELEC*
     C                   MOVE      ZZMDCY        QQENDT            8
     C*** END   ADD *** 10/31/97 **************************
     C*
     C*** BEGIN DEL *** 10/31/97 **************************
     C*** SORT SELECTION MUST BE "E" OR "A" ************** JJJ 3-1-89
     C**********                            ************** |
     C********** SCSRT     IFNE 'E'         ************** |
     C********** SCSRT     ANDNE'A'         ************** |
     C**********           MOVE '1'       *IN22 ********** |
     C**********           GOTO STRT        ************** |
     C**********           END              ************** |
     C*** END   DEL *** 10/31/97 **************************
     C*** BEGIN ADD *** 10/31/97 **************************
     C* PRIMARY SORT SELECTION MUST BE "EN", "EA", "AN", OR "AA"
     C*
     C     SCPSRT        IFNE      'EN'
     C     SCPSRT        ANDNE     'EA'
     C     SCPSRT        ANDNE     'AN'
     C     SCPSRT        ANDNE     'AA'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   ENDIF                                                  SCPSRT NE *BLAN
     C*
     C* SECONDARY SORT SELECTION MUST BE "EN" OR "EA" IF NOT BLANK
     C*
     C     SCSSRT        IFNE      *BLANK
     C     SCSSRT        ANDNE     'EN'
     C     SCSSRT        ANDNE     'EA'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   ENDIF                                                  SCSSRT NE *BLAN
     C*
     C* * * ONLY ONE OF AGT# OR AGT NAME CAN BE CHOSEN
     C*
     C     SCTAGT        IFNE      0
     C     SCFAGT        ORNE      0
     C     SCTAGA        IFNE      *BLANK
     C     SCFAGA        ORNE      *BLANK
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      '1'           *IN70
     C                   ENDIF                                                  SCTAGA
     C                   ELSE
     C                   MOVE      '0'           *IN70
     C                   ENDIF                                                  SCTAG#
     C*
     C*   FROM AGT# MUST BE LESS THAN TO AGT#
     C*
     C     *IN70         IFEQ      '1'
     C*** END   ADD *** 10/31/97 **************************
     C     SCFAGT        ANDGT     SCTAGT
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEGIN ADD *** 10/31/97 ***************************
     C*
     C* FROM AGENT NAME MUST BE LESS THAN TO AGENT NAME
     C*
     C     *IN70         IFEQ      '0'
     C     LO:UP         XLATE     SCFAGA        XXFAGA
     C     LO:UP         XLATE     SCTAGA        XXTAGA
     C     XXFAGA        IFGT      XXTAGA
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   ENDIF                                                  XXFAGA GT XXTA
     C                   ENDIF                                                  *IN70 EQ '0'
     C*
     C* * * ONLY ONE OF EMP# OR EMP NAME CAN BE CHOSEN
     C*
     C     SCTEM#        IFNE      0
     C     SCFEM#        ORNE      0
     C     SCTEMN        IFNE      *BLANK
     C     SCFEMN        ORNE      *BLANK
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      '1'           *IN71
     C                   ENDIF                                                  SCTEMN
     C                   ELSE
     C                   MOVE      '0'           *IN71
     C                   ENDIF                                                  SCTEM#
     C*
     C*
     C* FROM EMPLOYER# MUST BE LESS THAN TO EMPLOYER#
     C*
     C     *IN71         IFEQ      '1'
     C     SCFEM#        ANDGT     SCTEM#
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN71 EQ '1'
     C*
     C* FROM EMPLOYER NAME MUST BE LESS THAN TO EMP NAME
     C*
     C     *IN71         IFEQ      '0'
     C     LO:UP         XLATE(P)  SCFEMN        XXFEMN            2
     C     LO:UP         XLATE(P)  SCTEMN        XXTEMN            2
     C     XXFEMN        IFGT      XXTEMN
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   ENDIF                                                  XXFEMN GT XX
     C                   ENDIF                                                  *IN70 EQ '0'
     C*
     C* DETERMINE RANGE TYPES AND LOAD RANGE VALUES TO PASS
     C* TO CLP. NUMERIC: RIGHT JUSTIFIED, ALPHA: LEFT JUSTIFIED
      *
     C     SCTAGT        IFNE      0
     C                   MOVE      'AN'          QQSEL1            2
     C                   MOVE(P)   SCFAGT        QQS1FR           15
     C                   MOVE(P)   SCTAGT        QQS1TO           15
      *
     C                   ELSE
      *
     C     SCTAGA        IFNE      *BLANK
     C                   MOVE      'AA'          QQSEL1            2
     C                   MOVEL(P)  SCFAGA        QQS1FR           15
     C                   MOVEL(P)  SCTAGA        QQS1TO           15
     C                   ENDIF                                                  SCTAGA NE *BLANK
      *
     C                   ENDIF                                                  SCTAGT NE 0
      *
     C     SCTEM#        IFNE      0
     C                   MOVE      SCFEM#        XXFEM#           10
     C                   MOVE      SCTEM#        XXTEM#           10
      *
     C     SCTAGT        IFNE      0
     C     SCTAGA        ORNE      *BLANK
     C                   MOVE      'EN'          QQSEL2            2
     C     XXFEM#        CAT(P)    '00000'       QQS2FR           15
     C     XXTEM#        CAT(P)    '99999'       QQS2TO           15
      *
     C                   ELSE
     C                   MOVE      'EN'          QQSEL1            2
     C     XXFEM#        CAT(P)    '00000'       QQS1FR           15
     C     XXTEM#        CAT(P)    '99999'       QQS1TO           15
      *
     C                   ENDIF                                                  SCTAGT NE 0
      *
     C                   ENDIF                                                  SCTEM# NE 0
      *
     C     SCFEMN        IFNE      *BLANK
      *
     C     SCTAGT        IFNE      0
     C     SCTAGA        ORNE      *BLANK
     C                   MOVE      'EA'          QQSEL2            2
     C                   MOVEL(P)  SCFEMN        QQS2FR           15
     C                   MOVEL(P)  SCTEMN        QQS2TO           15
      *
     C                   ELSE
     C                   MOVE      'EA'          QQSEL1            2
     C                   MOVEL(P)  SCFEMN        QQS1FR           15
     C                   MOVEL(P)  SCTEMN        QQS1TO           15
     C*
     C                   ENDIF                                                  SCTAGT NE 0
     C*
     C                   ENDIF                                                  SCFEMN NE *BLANK
     C*
     C*** END   ADD *** 10/31/97 ***************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFFND            3
      *** BEGIN CHG *** 10/31/97 *********************************
     C                   MOVE      *DATE         QQRNDT            8
     C*PUIMOD
     C                   EVAL      QQRNDT = %Char(%Date():*USA0)
     C                   MOVE      'B'           QQRDST            1
     C                   MOVE      'S'           QQTYPE            1
     C                   MOVE      'B'           QQSTAT            1
     C                   MOVE      'M'           QQEMPF            1
     C                   MOVE      'M'           QQAMPF            1
     C                   MOVE      *BLANK        QQPRTY            2
     C                   MOVE      *BLANK        QQEMP#            9
     C                   MOVE      *BLANK        QQDIV             5
     C******************** MOVE SCFYR     QQFYR   3
      *** END   CHG *** 10/31/97 *********************************
     C                   MOVE      *BLANK        QQFYR             3
     C                   MOVE      SCRPMY        QQRPMY            6
     C                   MOVE      UPOUTQ        QQOUTQ           10
      *** BEGIN CHG *** 10/31/97 *********************************
     C                   MOVE      SCPSRT        QQPSRT            2
     C                   MOVE      SCSSRT        QQSSRT            2
     C******************** MOVE SCSRT     QQSRT   1
     C******************** MOVE SCFAGT    QQFAGT  5
     C******************** MOVE SCTAGT    QQTAGT  5
      *** END   CHG *** 10/31/97 *********************************
     C                   CALL      'W7280C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFFND
      *** BEGIN ADD *** 10/31/97 *********************************
     C                   PARM                    QQSTDT
     C                   PARM                    QQENDT
     C                   PARM                    QQRNDT
     C                   PARM                    QQRDST
     C                   PARM                    QQTYPE
     C                   PARM                    QQSTAT
      *** END   ADD *** 10/31/97 *********************************
     C                   PARM                    QQRPMY
     C                   PARM                    QQOUTQ
      *** BEGIN CHG *** 10/31/97 *********************************
     C                   PARM                    QQPSRT
     C                   PARM                    QQSSRT
     C******************** PARM  *******  QQSRT  ********
     C                   PARM                    QQEMPF
     C                   PARM                    QQAMPF
     C                   PARM                    QQPRTY
     C                   PARM                    QQEMP#
      *** END   CHG *** 10/31/97 *********************************
     C                   PARM                    QQDIV
     C                   PARM                    QQFYR
      *** BEGIN CHG *** 10/31/97 *********************************
     C******************** PARM           QQFAGT
     C******************** PARM           QQTAGT
     C                   PARM                    QQSEL1
     C                   PARM                    QQS1FR
     C                   PARM                    QQS1TO
     C                   PARM                    QQSEL2
     C                   PARM                    QQS2FR
     C                   PARM                    QQS2TO
     C*
      *** END   CHG *** 10/31/97 *********************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
