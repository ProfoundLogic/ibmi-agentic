     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F9515R01
     H*    DESCRITION: CLEARS CONTROL NUMBER AND SUBMISSION DATE
     H*                FOR FL TAPE SUBMISSION
     H*
     H*    CALLED FROM:
     H*       COMMAND LINE
     H*    CALLS TO:
     H*       NONE
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  06/09/20 C:295  Project : SY420
     H*                  Mask SSN
     H*
     H*****************************************************************
     FFAEOL32   UF   E           K DISK
     FFDCAL13   IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     F*
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D****                NAM        40  1
     D* CURRENT DATE
     D                 DS
     D  XXDT                   1      6  0
     D  XXMM                   1      2  0
     D  XXDD                   3      4  0
     D  XXYY                   5      6  0
     D*
     D* Year prior to current date's year; used to calc Julian date
     D                 DS
     D  ZZDT                   1      6  0
     D  ZZMM                   1      2  0
     D  ZZDD                   3      4  0
     D  ZZYY                   5      6  0
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      2  0
     D  DSDATE                 3      4  0
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
     D  SS#            S              9
      ***** End Add *** 06/09/20 *******************************************
     C*
     C* Key list for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key list for the Group Master File
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C     FNKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C* Key list for the SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C     EOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCNTL
     C                   KFLD                    XXEOB#
     C*
     C     CAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
     C*
     C*  Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQCRDT            8
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
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
     C*  INITIALIZE WORKFIELDS
     C*
     C     *LIKE         DEFINE    EOFND         XXFND
     C     *LIKE         DEFINE    FNNAME        XXCONM
     C     *LIKE         DEFINE    EOST          XXST
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOFYR         XXFYR
     C     *LIKE         DEFINE    EOCASE        XXCASE
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    CASS#         XXSS#
     C     *LIKE         DEFINE    CAACDT        XXACDT
     C*===============================================================*
     C* MAIN LOGIC                                                    *
     C*===============================================================*
     C                   EXCEPT    HEADS
     C                   Z-ADD     0             RECCT             6 0
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      FNNAME        XXCONM
     C*
     C                   MOVE      QQCRDT        SCCRDT            8 0
     C                   MOVE      QQFRDT        SCFRDT            8 0
     C                   MOVE      QQTODT        SCTODT            8 0
     C*
     C                   EXSR      ALLFND
     C*
     C                   EXSR      PGMEND
     C*
     C                   RETURN
     C*===============================================================*
     C* ALLFND - Process all funds                                    *
     C*===============================================================*
     C     ALLFND        BEGSR
     C*
     C                   Z-ADD     SCCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCRDT
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C                   MOVE      ' '           XXCNTL           20
     C                   Z-ADD     0             XXEOB#
     C*
     C     EOKY          SETLL     PEO01
     C                   READE     PEO01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     EOTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
     C                   MOVEL     EOSBDT        XXSBDT            8 0
     C*
     C     XXSBDT        IFEQ      SCCRDT
     C     SCFRDT        ANDLE     XXTRDT
     C     XXTRDT        ANDLE     SCTODT
     C                   EXSR      CLRREC
     C                   ENDIF
     C                   READ      PEO01                                  60
     C                   ENDDO
     C*
     C                   ENDSR
     C*===============================================================*
     C* CLRREC - Clears the CNTL# & SBDT from FAEOP; writes report    *
     C*===============================================================*
     C     CLRREC        BEGSR
     C*
     C                   MOVEL     EOCO#         XXCO#
     C                   MOVEL     EOSUB#        XXSUB#
     C                   MOVEL     EOFND         XXFND
     C                   MOVEL     EOFYR         XXFYR
     C                   MOVEL     EOCASE        XXCASE
     C                   MOVEL     EOEOB#        XXEOB#
     C*
     C     CAKY          CHAIN     PCA01                              62
     C                   MOVEL     CASS#         XXSS#
     C                   MOVEL     CAACDT        XXACDT
     C     CDKY          CHAIN     PCD01                              63
     C                   MOVE      CDLSN         NAM              20
     C***********          Z-ADD20        X       20
     C*
     C***********NAM,X     DOWEQ' '
     C***********          SUB  1         X
     C***********          ENDDO
     C*
     C***********          ADD  1         X
     C***********          MOVEA','       NAM,X
     C***********          ADD  2         X
     C***********          MOVEACDFSN     NAM,X
     C***********          Z-ADD40        X
     C***********NAM,X     DOWEQ' '
     C***********          SUB  1         X
     C***********          ENDDO
     C***********          ADD  2         X
     C***********          MOVEACDMID     NAM,X
     C*
     C                   MOVEL     EOCNTL        CNTL             20
     C                   MOVEL     EOEOB#        EOMB              9
     C                   MOVEL     EOSBDT        SBDT              8 0
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     CASS#         SSN               9 0
      /Free
        SS# = MaskData('SSN':'CASS#':CASS#);
      /End-free
     C                   MOVEL     SS#           SSN               9 0
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C     *INOF         IFEQ      *OFF
     C                   EXCEPT    DETAIL
     C                   ADD       1             RECCT
     C                   ADD       1             LINCT             2 0
     C     LINCT         IFGE      60
     C                   SETON                                        OF
     C                   ENDIF
     C                   ELSE
     C                   EXCEPT    HEADS
     C                   EXCEPT    DETAIL
     C                   ADD       1             RECCT
     C                   Z-ADD     6             LINCT             2 0
     C                   SETOFF                                       OF
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       EOCNTL
     C                   MOVE      *BLANKS       EOSBDT
     C*
     C                   UPDATE    PEO01
     C*
     C                   ENDSR
     C*===============================================================*
     C* KCDO - F3=Exit                                                *
     C*===============================================================*
     C     KCDO          BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*===============================================================*
     C* PGMEND - End of program processing                            *
     C*===============================================================*
     C     PGMEND        BEGSR
     C                   EXCEPT    FOOT
     C                   SETOFF                                       OF
     C                   FEOD      FAEOL32
     C                   EXSR      KCDO
     C                   ENDSR
     O*===============================================================*
     O*  REPORT SPECIFICATIONS                                        *
     O*===============================================================*
     O*
     OQSYSPRT   E            HEADS          1 02
     O***                      DSTIME    15 '  :  :  '
     O                       XXCONM              80
     O***                      DSDATE   120 '  /  /    '
     O          E            HEADS          1
     O                       Q1PGM               15
     O                                           76 'CLEARED CONTROL NUMBER'
     O          E            HEADS          1
     O                                           74 'AND SUBMISSION DATE'
     O                                          121 'PAGE:'
     O                       PAGE          Z    125
     O          E            HEADS          1
     O                                           54 'DATE'
     O          E            HEADS          1
     O                                           24 'CONTROL NUMBER'
     O                                           40 'EOMB'
     O                                           59 'SUBMITTED'
     O                                           70 'NAME'
     O                                          114 'SSN'
     O          E            HEADS          2
     O                                           30 '--------------------'
     O                                           45 '---------'
     O                                           60 '----------'
     O                                           86 '--------------------'
     O                                          106 '--------------------'
     O                                          122 '-----------'
     O          E            DETAIL         2
     O                       CNTL                30
     O                       EOMB                45
     O                       SBDT                60 '  /  /    '
     O                       NAM                106
     O                       SSN                114 '   -  -    '
     O          E            FOOT           3
     O                                           71 'TOTAL RECORDS DELETED:'
     O                       RECCT         2     79
