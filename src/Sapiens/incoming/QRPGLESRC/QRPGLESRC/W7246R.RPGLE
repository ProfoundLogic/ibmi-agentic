     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7241R - RECOVERY REPORT - OUTPUT WORKFILE
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMRRL1    IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWXR1L     O    E             DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
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
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
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
     C*
     C                   Z-ADD     0             XXCASE            5 0
     C*
     C*  START READING REQUEST REFUND FILE - EXCLUDE INACTIVE RECORDS
     C*
     C     RRKEY         CHAIN     PRR01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     RRSTAT        IFNE      'I'
     C     RRSTCD        ANDNE     '99'
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
     C*  GET CLAIMANT NAME
     C*
     C     CMCASE        IFNE      XXCASE
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        R1NAME           40
     C                   ELSE
     C                   MOVE      *BLANK        R1NAME
     C                   END
     C                   END
     C*
     C*  MOVE INFORMATION TO WORKFILE
     C*
     C                   MOVE      XXCO#         R1CO#
     C                   MOVE      XXFND         R1FND
     C                   MOVE      RRFYR         R1FYR
     C                   MOVE      RRCASE        R1CASE
     C*
     C*  CHECK FOR MORE THAN 1 REQUEST FOR SAME CLAIM & PROVIDER
     C*
     C                   MOVE      '0'           *IN50
     C                   Z-ADD     0             XXAMTR            9 2
     C                   Z-ADD     0             XXAMTC            9 2
     C     *IN50         DOWEQ     '0'
     C*
     C     RRSTAT        IFNE      'I'
     C                   MOVE      RRSTCD        R1STCD
     C                   END
     C*
     C     RRKY2         READE     PRR01                                  50
     C                   END
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *BLANKS       R1STAT
     C                   MOVEL     Q1USER        R1USER
     C                   MOVEL     Q1PGM         R1PGM
     C                   Z-ADD     DSTIME        R1TIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         R1DT
     C*
     C                   WRITE     PR101
     C*
     C                   END
     C*
     C                   Z-ADD     CMCASE        XXCASE
     C     RRKY2         SETGT     PRR01
     C*
     C                   END
     C                   END
     C     RRKEY         READE     PRR01                                  60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C**
