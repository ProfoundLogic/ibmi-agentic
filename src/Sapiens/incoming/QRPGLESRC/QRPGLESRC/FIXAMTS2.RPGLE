     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  FIXAMTS - FIX THE AMOUNT FIELDS               C:62 03/20/01  *
     H*                                                MCMNT          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  11/20/01 C:62 PROJECT MC095 29950                            *
     H*                -ADD SECTION TO CREATE WORKFILE                *
     H*                -USE WORKFILE                                  *
     H*                                                               *
     H*****************************************************************
     FNDTML     IF   E           K DISK
     FFAEAL1    UF   E           K DISK
     F*
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     TMKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C     *LIKE         DEFINE    EABAMT        XXBAMT
     C     *LIKE         DEFINE    EADT          XXDT
     C     *LIKE         DEFINE    EAEOB#        XXEOB#
     C     *LIKE         DEFINE    EABCAM        XXBCAM
     C*
     C                   Z-ADD     *ZEROS        COUNT             5 0
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*
     C                   READ      PEA01                                  99
     C     *IN99         DOWEQ     *OFF
     C     EASYS         IFEQ      'M'
     C     EASYS         OREQ      'U'
     C                   EXSR      CLCAMT
     C                   EXSR      UPDTPB
     C                   ENDIF
     C                   READ      PEA01                                  99
     C                   ENDDO
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   SETOFF                                       OA
     C                   ENDIF
     C*
     C                   EXCEPT    TOT
     C*
     C                   SETON                                        LR
     C*================================================================
     C* CLCAMT - CALCULATES BILL AMOUNT.
     C*================================================================
     C     CLCAMT        BEGSR
     C*
     C                   Z-ADD     *ZEROS        XXBAMT
     C*
     C     TMKY          CHAIN     PTM01                              99
     C     *IN99         DOWEQ     *OFF
     C     TMBFLG        IFNE      'N'
     C*
     C* CALCULATE ORIGINAL AMOUNT.
     C*
     C     TMMILE        MULT      TMMIRT        XXMIRT            9 2
     C*
     C     TMHRS         MULT      TMHRRT        XXHRRT            9 2
     C*
     C                   Z-ADD     TMMSCG        XXMSCG            9 2
     C*
     C     XXMIRT        ADD       XXHRRT        HDBAMT            9 2
     C                   ADD       XXMSCG        HDBAMT
     C*
     C                   ADD       HDBAMT        XXBAMT
     C                   ENDIF
     C*
     C     TMKY          READE     PTM01                                  99
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* DETAIL - WRITES DETAIL LINE IN REPORT
     C*================================================================
     C     DETAIL        BEGSR
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   SETOFF                                       OA
     C                   ENDIF
     C                   EXCEPT    DET
     C                   ADD       1             COUNT
     C*
     C                   ENDSR
     C*================================================================
     C* UPDTPB - Updates AMOUNTS - PROVIDER BILLS
     C*================================================================
     C     UPDTPB        BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   SELECT
     C     EASYS         WHENEQ    'U'
     C*
     C                   Z-ADD     EABAMT        XXAMTB            9 2
     C                   Z-ADD     XXBAMT        EABAMT
     C                   ADD       EABCAM        EABAMT
     C                   Z-ADD     EABAMT        XXAMTA            9 2
     C     XXAMTA        IFNE      XXAMTB
     C     XXAMTA        SUB       XXAMTB        XXDIFF            9 2
     C                   MOVEL(P)  Q1USER        EAUSER
     C                   MOVE      'F'           EAPGM
     C     XXBAMT        IFNE      *ZEROS
     C                   MOVEL     'Y'           EATMFL
     C                   ELSE
     C                   MOVEL     ' '           EATMFL
     C                   ENDIF
     C*
     C     @@CALL        IFEQ      'U'
     C                   UPDATE    PEA01
     C                   ELSE
     C                   UNLOCK    FAEAL1
     C                   ENDIF
     C                   EXSR      DETAIL
     C*
     C                   ELSE
     C     @@CALL        IFEQ      'U'
     C                   UPDATE    PEA01
     C                   ELSE
     C                   UNLOCK    FAEAL1
     C                   ENDIF
     C                   ENDIF
     C**
     C     EASYS         WHENEQ    'M'
     C                   Z-ADD     EABAMT        XXAMTB            9 2
     C                   Z-ADD     XXBAMT        EABAMT
     C                   ADD       EABCAM        EABAMT
     C                   Z-ADD     EABAMT        XXAMTA            9 2
     C*
     C     XXAMTA        IFNE      XXAMTB
     C*
     C     XXAMTA        SUB       XXAMTB        XXDIFF            9 2
     C*
     C                   MOVEL(P)  Q1USER        EAUSER
     C                   MOVE      'F'           EAPGM
     C*
     C     XXBAMT        IFNE      *ZEROS
     C                   MOVEL     'Y'           EATMFL
     C                   ELSE
     C                   MOVEL     ' '           EATMFL
     C                   ENDIF
     C*
     C     @@CALL        IFEQ      'U'
     C                   UPDATE    PEA01
     C                   ELSE
     C                   UNLOCK    FAEAL1
     C                   ENDIF
     C                   EXSR      DETAIL
     C*
     C                   ELSE
     C     @@CALL        IFEQ      'U'
     C                   UPDATE    PEA01
     C                   ELSE
     C                   UNLOCK    FAEAL1
     C                   ENDIF
     C                   ENDIF
     C*
     C                   OTHER
     C     @@CALL        IFEQ      'U'
     C                   UPDATE    PEA01
     C                   ELSE
     C                   UNLOCK    FAEAL1
     C                   ENDIF
     C                   ENDSL
     C*
     C                   ENDSR
     OQSYSPRT   E            HEAD           1 02
     O                       DSTIME               8 '  :  :  '
     O                                           49 'FIXAMTS2'
     O                       DSDATE             108 '  /  /    '
     O          E            HEAD           1
     O                                           11 'System Type'
     O                                           20 'EOMB#'
     O                                           33 'Invoice #'
     O                                           48 'Before Amount'
     O                                           62 'After Amount'
     O                                           81 'Amount Difference'
     O                                           87 'Sub#'
     O                                           94 'Grp'
     O                                          101 'Year'
     O                                          108 'Case'
     O          E            DET            1
     O                       EASYS                6
     O                       EAEOB#        4     22
     O                       EABINV        4     31
     O                       XXAMTB        4     46
     O                       XXAMTA        4     60
     O                       XXDIFF        4     74
     O                       EASUB#        4     88
     O                       EAFND         4     94
     O                       EAFYR         4    100
     O                       EACASE        4    111
     O          E            TOT            1
     O                                           22 'Total number of Bills:'
     O                       COUNT         4     29
