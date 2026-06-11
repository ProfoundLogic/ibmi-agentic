     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTFMP1P  - CONVERT PD STATE TO P1 ADJUDICATION STATE
     FFMPDP     IF   E           K DISK
     FFMP1L     UF A E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C*** PROVIDER AUXILIARY FILE
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
     C     P1KEY         KLIST
     C                   KFLD                    PDFEIN
     C                   KFLD                    PDMOD
     C*
     C                   TIME                    DSDTTM
     C*
     C*** READ PROVIDER MASTER FILE
     C                   READ      PPD01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C*** CHAIN TO PROVIDER AUXILIARY FILE
     C     P1KEY         CHAIN     PP101                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      PDFEIN        P1FEIN
     C                   MOVE      PDMOD         P1MOD
     C                   MOVEL     PDST          P1ST
     C                   MOVE      *BLANKS       P1LIC#
     C                   MOVE      'A'           P1STAT
     C                   MOVEL     'PDSPGMR'     P1USER
     C                   MOVEL     'CVTFMP1P'    P1PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P1DT
     C*
     C                   Z-ADD     DSTIME        P1TIME
     C                   MOVEL     'PDSPGMR'     P1IUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P1IDT
     C*
     C                   WRITE     PP101
     C                   END                                                    END 0 *IN50
     C*
     C*** READ NEXT PROVIDER MASTER RECORD
     C                   READ      PPD01                                  60
     C                   END                                                    END DOWEQ *IN60
     C*
     C                   MOVE      '1'           *INLR
