     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE LOADWMCMP - LOAD VALUE INTO WMCMP/CMIMP
     H*
     H*****************************************************************
     H*                                                               *
     H*  CREATED: 04/11/00  C:118 PROJECT WC854                       *
     H*                                                               *
     H*  THIS PROGRAM WILL LOAD A VALUE INTO WMCMP/CMIMP AND          *
     H*    FMCMP/CMMMIP BASED ON THE FOLLOWING TESTS:                 *
     H*
     H*  WMCMP:  IF CMIPCT <> 0, LOAD 'Y' TO CMIMP ELSE LIST ON RPT   *
     H*  FMCMP:  IF CMMMIP <> 0, LOAD 'Y' TO CMMMIP ELSE LIST ON RPT  *
     H*
     H*****************************************************************
     H*
     FWMCML     UF   E           K DISK
     FFMCMLJ    UF   E           K DISK    USROPN
     F                                     RENAME(PCM01:PCM02)
     F*
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOF)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     F*

     IPCM02
     I              CMOSCL                      C2OSCL

     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   TIME                    DSDTTM
     C*
     C*  INITIALIZE COUNTERS
     C*
     C                   Z-ADD     0             WMREC             5 0
     C                   Z-ADD     0             WMUPD             5 0
     C                   Z-ADD     0             WMPRT             5 0
     C                   Z-ADD     0             FMREC             5 0
     C                   Z-ADD     0             FMUPD             5 0
     C                   Z-ADD     0             FMPRT             5 0
     C*
     C                   EXCEPT    HEAD
     C                   SETOFF                                           LR
      *
      *     WCMC Installed determination
      *
     C                   CALL      'SA001R'
     C                   PARM                    QQWCIF            1
     C                   PARM                    QQFBIF            1
     C                   PARM                    QQFL01            1
      *
     C     QQFBIF        IFEQ      'Y'
     C     QQFL01        ANDEQ     'Y'
     C                   MOVE      *ON           XXMANC            1
     C                   OPEN      FMCMLJ
     C                   ELSE
     C                   MOVE      *OFF          XXMANC
     C                   ENDIF
     C*
     C*  WMCMP UPDATE
     C*
     C                   READ      PCM01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     CMIPCT        IFNE      0
     C                   MOVEL     'Y'           CMIMP
     C                   UPDATE    PCM01
     C                   ADD       1             WMUPD
     C*
     C                   ELSE
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       1             WMPRT
     C                   ENDIF                                                  CMIPCT<>0
     C*
     C                   ADD       1             WMREC
     C                   READ      PCM01                                  60
     C                   ENDDO
     C*
     C                   EXCEPT    HEAD
     C                   SETOFF                                         LR
     C*
     C*  FMCMP UPDATE
     C*
     C     XXMANC        IFEQ      *ON
     C*
     C                   READ      PCM02                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     CMMMIP        IFNE      0
     C                   MOVEL     'Y'           CMIMP
     C                   UPDATE    PCM02
     C                   ADD       1             WMUPD
     C*
     C                   ELSE
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       1             WMPRT
     C                   ENDIF                                                  CMIPCT<>0
     C*
     C                   ADD       1             WMREC
     C                   READ      PCM02                                  60
     C                   ENDDO
     C*
     C                   ENDIF                                                  XXMANC = *ON
     C*
     C                   MOVE      *ON           *INLR
     C   LR              EXCEPT    TOTAL
     C*
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                                           50 'CLAIM RECORDS WHERE '
     O                                              'CMIPCT = 0'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           75 'PAGE:'
     O                       PAGE          Z     80
     O          E            HEAD           1
     O                                            7 'CO#'
     O                                           14 'GROUP'
     O                                           25 'EMPLOYER#'
     O                                           32 'DIV'
     O                                           37 'FYR'
     O                                           46 'CASE'
     O          E            DET            1
     O                       CMCO#         Z      7
     O                       CMFND         Z     14
     O                       CMEMP#        Z     25
     O                       CMDIV         Z     32
     O                       CMFYR         Z     37
     O                       CMCASE        Z     46
     O          E            TOTAL       1  1
     O                                           23 '#RECORDS PRINTED = '
     O                       WMPRT         Z     30
     O          E            TOTAL          1
     O                                           23 '#RECORDS READ = '
     O                       WMREC         Z     30
     O          E            TOTAL          1
     O                                           23 '#RECORDS UPDATED = '
     O                       WMUPD         Z     30
