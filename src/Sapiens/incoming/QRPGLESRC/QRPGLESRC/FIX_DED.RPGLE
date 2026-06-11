     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*
     H* REVERSE AR RECORDS FOR TASB'S BATCH # 5883 AND 5911
     H*
     H*****************************************************************
     FWDEAL4    IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMEAP     O  A E           K DISK
     F                                     RENAME(PEA01:PEA02)

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     *LIKE         DEFINE    TTKEY         XXKEY
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBAT#
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     25            XXCO#             3 0
     C                   Z-ADD     5883          XXBAT#            9 0
     C*
     C     EAKEY         CHAIN     PEA01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C                   EXSR      ARW
     C*
     C     EAKEY         READE     PEA01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   Z-ADD     5911          XXBAT#            9 0
     C*
     C     EAKEY         CHAIN     PEA01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C                   EXSR      ARW
     C*
     C     EAKEY         READE     PEA01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   SETON                                        LR
     C*
     C*****************************************************************
     C*
     C* WRITE THE A/R RECORD INTO WMEAP
     C*
     C*****************************************************************
     C*
     C     ARW           BEGSR
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      '0'
     C     TTSTAT        ANDNE     'I'
     C                   MOVEL     TTTYPE        XXTYPE            1
     C                   ELSE
     C                   MOVE      *BLANKS       XXTYPE
     C                   END
     C*
     C                   MOVE      XXTYPE        EAARTY
     C                   MOVE      EAINV#        EANINV
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANKS       EAEDIT
     C                   MOVE      *BLANKS       EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EATIME
     C                   MOVE      EABAT#        EABUL1
     C                   Z-SUB     EAAMT         EAAMT
     C                   WRITE     PEA02
     C*
     C                   ENDSR
     C*
     C*****************************************************************
