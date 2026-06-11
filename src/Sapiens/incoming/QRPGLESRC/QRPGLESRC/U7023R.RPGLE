     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE U7023R - EDIT EXCEPTED CLAIM WORKFILE BUILD PROGRAM
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    EXCEPTED CLAIM WORKFILE BUILD PROGRAM FOR C/R REVIEW       *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*   U7020C                                                      *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*   U70A2R                                                      *
     H*   U70A3R                                                      *
     H*                                                               *
     H*  PROGRAMMER: KEN RUTHVEN        PROJECT: 99AAF                *
     H*  CREATED: 10/10/92                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  50        READ/DOWEQ PES01                                   *
     H*  55        REPORT IS TO BE RUN AGAINST ALL EDIT CODES         *
     H* N55        REPORT IS FOR ONE EDIT CODE                        *
     H*  60        GENERAL CHAIN                                      *
     H*****************************************************************
     FUMCRL     IF   E           K DISK
     FUAESP     IF   E           K DISK
     FUAEEP     O  A E           K DISK
     FFDCALF    IF   E           K DISK

     D*
     D PGM             S              8    DIM(1) CTDATA PERRCD(1)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XZPGM                  1      8
     D  XZTYPE                 4      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE C/R REVIEW FILE
     C*
     C     CRKEY         KLIST
     C                   KFLD                    ESCO#
     C                   KFLD                    ESSUB#
     C                   KFLD                    ESFND
     C                   KFLD                    XXEDIT
     C*
     C     CRKY1         KLIST
     C                   KFLD                    ESCO#
     C                   KFLD                    ESSUB#
     C                   KFLD                    ESFND
     C*
     C*   KEY LIST FOR THE DETAIL CHARGE FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    ESCO#
     C                   KFLD                    ESSUB#
     C                   KFLD                    ESFND
     C                   KFLD                    ESFYR
     C                   KFLD                    ESCASE
     C                   KFLD                    XXDSEQ
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQEDIT            5
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C* INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    CREDIT        XXEDIT                         5A
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ                         5.0N
     C*
     C* MOVE ENTRY PARMS TO WORK FIELDS
     C*
     C                   MOVE      QQEDIT        XXEDIT
     C     XXEDIT        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN55
     C                   END                                                    END XXEDIT
     C*
     C* READ SELECTED CLAIM WORKFILE/READ C/R REVIEW FILE
     C*
     C                   READ      PES01                                  50
     C     *IN50         DOWEQ     '0'
     C  N55CRKEY         CHAIN     PCR01                              60
     C   55CRKY1         CHAIN     PCR01                              60
     C     *IN60         DOWEQ     '0'
     C     CREXT         IFEQ      *ZEROS
     C*
     C* DETERMINE WHICH INDIVIDUAL EDIT PROGRAM TO CALL
     C*
     C                   MOVEL     PGM(1)        XZPGM
     C                   MOVEL     CRETYP        XZTYPE
     C*
     C* CALL THE RPG EDIT PROGRAM TO BUILD THE EXCEPTED CLAIM WORKFILE
     C*
     C                   MOVE      ESCO#         Q1CO#             3
     C                   MOVE      ESSUB#        Q1SUB#            5
     C                   MOVE      ESFND         Q1FND             3
     C                   MOVE      ESFYR         Q1FYR             3
     C                   MOVE      ESCASE        Q1CASE            7
     C  N55              MOVE      XXEDIT        Q1EDIT            5
     C   55              MOVE      CREDIT        Q1EDIT            5
     C                   MOVE      *BLANKS       Q1EXT             3
     C                   MOVE      *BLANKS       Q1DSEQ            5
     C                   MOVE      *BLANKS       Q1RTCD            1
     C                   MOVE      QQQUE         Q1QUE            10
     C                   MOVE      QQMODE        Q1MODE            1
     C                   CALL      XZPGM
     C                   PARM                    Q1CO#
     C                   PARM                    Q1SUB#
     C                   PARM                    Q1FND
     C                   PARM                    Q1FYR
     C                   PARM                    Q1CASE
     C                   PARM                    Q1EDIT
     C                   PARM                    Q1EXT
     C                   PARM                    Q1DSEQ
     C                   PARM                    Q1RTCD
     C                   PARM                    Q1QUE
     C                   PARM                    Q1MODE
     C*
     C* UPON RETURN FROM INIVIDUAL PROGAM IF Q1RTCD EQUALS 'N' WRITE
     C* CLAIM REOCRD TO UAEEP(EXCEPTION FILE) ELSE GET NEXT RECORD
     C*
     C     Q1RTCD        IFEQ      'N'
     C                   Z-ADD     ESCO#         EECO#
     C                   Z-ADD     ESSUB#        EESUB#
     C                   Z-ADD     ESFND         EEFND
     C                   Z-ADD     ESFYR         EEFYR
     C                   Z-ADD     ESCASE        EECASE
     C                   Z-ADD     ESADT         EEADT
     C                   MOVE      Q1EDIT        EEEDIT
     C                   MOVE      Q1EXT         EEEXT
     C                   MOVE      CRETYP        EEETYP
     C                   MOVE      ESFL01        EEFL01
     C                   MOVE      ESFL02        EEFL02
     C                   MOVE      ESFL02        EEFL03
     C                   MOVE      ESFL02        EEFL04
     C                   MOVE      ESFL02        EEFL05
     C                   Z-ADD     ESDT01        EEDT01
     C                   Z-ADD     ESDT01        EEDT02
     C                   Z-ADD     ESDT01        EEDT03
     C                   Z-ADD     ESDT01        EEDT04
     C                   Z-ADD     ESDT01        EEDT05
     C                   MOVE      ESSTAT        EESTAT
     C*
     C                   MOVE      Q1DSEQ        XXDSEQ
     C*
     C* CHAIN TO DETAIL CHARGE FILE TO GET ADDITIONAL INFORMATION
     C*
     C     CAKEY         CHAIN     PCA01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CACSCD        EECSCD
     C                   Z-ADD     CAFDAT        EEFDAT
     C                   MOVE      CAIPOP        EEIPOP
     C                   Z-ADD     CAFEIN        EEFEIN
     C                   Z-ADD     CAMOD         EEMOD
     C                   MOVE      CAPRTY        EEPRTY
     C                   Z-ADD     CASVCD        EESVCD
     C                   MOVE      CAPMOD        EEPMOD
     C                   MOVE      CACPT4        EECPT4
     C                   MOVE      CAICD9        EEICD9
     C                   Z-ADD     CAANMN        EEANMN
     C                   Z-ADD     CAUNTS        EEUNTS
     C                   MOVE      CAEOMB        EEEOMB
     C                   Z-ADD     CADSEQ        EEDSEQ
     C                   MOVE      CASS#         EESS#
     C                   Z-ADD     CAACDT        EEACDT
     C                   MOVE      CARCFL        EERCFL
     C                   MOVE      CAZIPA        EEZIPA
     C                   MOVE      CAAST         EEAST
     C                   END                                                    END *IN60
     C                   MOVE      Q1USER        EEUSER
     C                   MOVE      Q1PGM         EEPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EEDT
     C*
     C                   Z-ADD     DSTIME        EETIME
     C                   MOVEL     Q1USER        EEIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EEIDT
     C*
     C                   WRITE     PEE01
     C                   END                                                    END RTCD = N
     C*
     C                   END                                                    END CREXT
     C   55CRKY1         READE     PCR01                                  60
     C  N55              MOVE      '1'           *IN60
     C                   END                                                    END *IN60
     C                   READ      PES01                                  50
     C                   END                                                    END *IN50
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
**  EDIT TYPE INDIVIDUAL PROGRAM VALUE TO CALL
U70XXR
