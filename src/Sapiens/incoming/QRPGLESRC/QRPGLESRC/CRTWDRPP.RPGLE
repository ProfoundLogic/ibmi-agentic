     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE CRTWDRPP = Create WDRPP Records
     H*****************************************************************
     H* Created on 02/17/2011 by c:62 for Support 75852               *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     FWDPAP     IF   E             DISK
     FWDRPP     O    E             DISK
     FWMPDL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWARPPML1  IF   E           K DISK
     F                                     RENAME(PRP01:PRP02)

     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C     CMKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     RPKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    PACHK#
     C                   KFLD                    PATRDT
     C*
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    RPRMDS        XXRMDS
     C     *LIKE         DEFINE    RPRMK1        XXRMK1
     C     *LIKE         DEFINE    RPRMK2        XXRMK2
     C     *LIKE         DEFINE    RPRMK3        XXRMK3
     C     *LIKE         DEFINE    RPCKCN        XXCKCN
     C*
     C                   READ      PPA01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      WRTRPP
     C*
     C                   READ      PPA01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C***************************************************************
     C* WRTRPP - WRITES WDRPP FIELDS
     C***************************************************************
     C     WRTRPP        BEGSR
     C*
     C                   CLEAR                   XXRMDS
     C                   CLEAR                   XXRMK1
     C                   CLEAR                   XXRMK2
     C                   CLEAR                   XXRMK3
     C                   CLEAR                   XXCKCN
     C     RPKY          CHAIN     PRP02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RPRMDS        XXRMDS
     C                   MOVEL(P)  RPRMK1        XXRMK1
     C                   MOVEL(P)  RPRMK2        XXRMK2
     C                   MOVEL(P)  RPRMK3        XXRMK3
     C                   MOVEL(P)  RPCKCN        XXCKCN
     C                   ENDIF
     C*
     C                   CLEAR                   PRP01
     C*
     C                   Z-ADD     PACO#         RPCO#
     C                   Z-ADD     PAFND         RPFND
     C                   Z-ADD     PAFYR         RPFYR
     C                   Z-ADD     PACASE        RPCASE
     C                   Z-ADD     PACHK#        RPCHK#
     C                   Z-ADD     PATRDT        RPTRDT
     C                   Z-ADD     PASEQ#        RPSEQ#
     C                   MOVEL     XXRMDS        RPRMDS
     C                   MOVEL     XXRMK1        RPRMK1
     C                   MOVEL     XXRMK2        RPRMK2
     C                   MOVEL     XXRMK3        RPRMK3
     C                   MOVEL     XXCKCN        RPCKCN
     C                   MOVEL     PABUL1        RPBUL1
     C*
     C                   Z-ADD     PAFEIN        KKFEIN
     C                   Z-ADD     PAMOD         KKMOD
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     C     PAFEIN        ANDNE     *ZEROS
     C*
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL     PDADR1        RPPNAM
     C                   MOVEL     PDADR2        RPADR1
     C                   ELSE
     C                   MOVEL(P)  PDNAME        RPPNAM
     C                   MOVEL(P)  PDADR1        RPADR1
     C                   MOVEL(P)  PDADR2        RPADR2
     C                   ENDIF
     C*
     C                   MOVEL(P)  PDCTY         RPCTY
     C                   MOVEL(P)  PDST          RPST
     C                   Z-ADD     PDZIP         RPZIP
     C                   MOVEL(P)  PDADR3        RPADR3
     C                   MOVEL(P)  PDCTRY        RPCTRY
     C*
     C                   ELSE
     C     CMKY          CHAIN     PCM01                              99
     C  N99CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     CDMID         IFNE      *BLANKS
     C     CDFSN         CAT(P)    CDMID:1       RPPNAM
     C                   CAT       '.':0         RPPNAM
     C                   CAT       CDLSN:1       RPPNAM
     C                   ELSE
     C     CDFSN         CAT(P)    CDLSN:1       RPPNAM
     C                   ENDIF
     C*
     C                   MOVEL(P)  CDADR1        RPADR1
     C                   MOVEL(P)  CDADR2        RPADR2
     C                   MOVEL(P)  CDCTY         RPCTY
     C                   MOVEL(P)  CDST          RPST
     C                   Z-ADD     CDZIP         RPZIP
     C                   MOVEL(P)  CDADR3        RPADR3
     C                   MOVEL(P)  CDCTRY        RPCTRY
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C********* MAINTENANCE FIELDS ************************************
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL(P)  Q1USER        RPUSER
     C                   MOVEL(P)  Q1PGM         RPPGM
     C                   Z-ADD     DSTIME        RPTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C*
     C                   WRITE     PRP01
     C*
     C                   ENDSR
