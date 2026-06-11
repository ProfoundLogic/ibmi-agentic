     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: WRTSDAIP
     H*
     H*****************************************************************
     FSDCMP     UF   E             DISK
     FSDAIL     UF A E           K DISK
     FSTCTL1    IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     I*

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     AIKY          KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     C*
     C     CTKY          KLIST
     C                   KFLD                    CMRMCD
     C*
     C                   READ      PCM01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     AIKY          CHAIN     PAI01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PAI01
     C*
     C                   MOVEL(P)  CMKEY         AIKEY
     C                   Z-ADD     CMSEQ#        AISEQ#
     C*
     C     CTKY          CHAIN     PCT01                              99
     C     *IN99         IFEQ      *OFF
     C     CTDEPG        IFNE      *BLANKS
     C                   MOVEL(P)  CTDEPG        AIDPGM
     C                   MOVEL(P)  CMKEY         AIDKEY
     C                   MOVEL(P)  'Y'           CMFL01
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       AIDPGM
     C                   MOVEL(P)  *BLANKS       AIDKEY
     C                   MOVEL(P)  *BLANKS       CMFL01
     C                   ENDIF
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL(P)  Q1USER        AIUSER
     C                   MOVEL(P)  Q1PGM         AIPGM
     C                   MOVEL(P)  Q1USER        AIIUSR
     C                   MOVEL(P)  Q1PGM         AIIPGM
     C                   Z-ADD     DSTIME        AITIME
     C                   Z-ADD     DSTIME        AIITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AIDT
     C                   MOVEL     SRDTO         AIIDT
     C*
     C                   WRITE     PAI01
     C*
     C                   ELSE
     C*
     C     CTKY          CHAIN     PCT01                              99
     C     *IN99         IFEQ      *OFF
     C     CTDEPG        IFNE      *BLANKS
     C                   MOVEL(P)  CTDEPG        AIDPGM
     C                   MOVEL(P)  CMKEY         AIDKEY
     C                   MOVEL(P)  'Y'           CMFL01
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       AIDPGM
     C                   MOVEL(P)  *BLANKS       AIDKEY
     C                   MOVEL(P)  *BLANKS       CMFL01
     C                   ENDIF
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL(P)  Q1USER        AIUSER
     C                   MOVEL(P)  Q1PGM         AIPGM
     C                   Z-ADD     DSTIME        AITIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AIDT
     C                   UPDATE    PAI01
     C*
     C                   ENDIF
     C                   UPDATE    PCM01
     C*
     C                   READ      PCM01                                  99
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
