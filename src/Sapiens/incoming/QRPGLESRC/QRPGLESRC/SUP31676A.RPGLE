     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE UPDATE DQAMTR FOR RECORDS PRIOR TO DEDUC CHANGE INSTALL
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     FWDRPL     IF   E           K DISK
     FS31676L   IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C**************************************************************************
     C*
     C     PAKY          KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C                   KFLD                    RPCHK#
     C                   KFLD                    RPTRDT
     C*
     C     PDKY          KLIST
     C                   KFLD                    PAFEIN
     C                   KFLD                    PAMOD
     C*
     C     CMKY          KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C*
     C                   MOVE      *BLANKS       XXRPNM           40
     C                   Z-ADD     0             XXCNT             7 0
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*----------------------------------------------------------------
     C*  Start of main loop
     C*
     C                   READ      PRP01                                  99
     C     *IN99         DOWEQ     '0'
     C*
     C     PAKY          CHAIN     PPA01                              99
     C     *IN99         IFEQ      *OFF
     C*****
     C     PAFEIN        IFNE      *ZERO
     C*
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        XXRPNM
     C                   ELSE
     C     PDFSN         IFNE      *BLANKS
     C                   MOVEL(P)  PDFSN         XXRPNM
     C                   CAT       PDNAME:1      XXRPNM
     C                   ELSE
     C                   MOVEL(P)  PDNAME        XXRPNM
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C***
     C                   ELSE
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C     CMFSN         CAT(P)    CMMID:1       XXRPNM
     C                   CAT       CMLSN:1       XXRPNM
     C                   ENDIF
     C*
     C                   ENDIF
     C*****
     C     RPPNAM        IFNE      XXRPNM
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       1             XXCNT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   READ      PRP01                                  99
     C                   ENDDO
     C*
     C                   EXCEPT    TOT
     C                   SETON                                        LR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                                           10 'SUP31676A'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            3 'FND'
     O                                            8 'FYR'
     O                                           17 'CASE'
     O                                           24 'SEQ#'
     O                                           33 'CHECK#'
     O                                           44 'TRANS DT'
     O                                           53 'RPPNAM'
     O                                           96 'XXRPNM'
     O          E            DET
     O                       RPFND         4      3
     O                       RPFYR         4      8
     O                       RPCASE        4     17
     O                       PASEQ#        4     24
     O                       RPCHK#        4     33
     O                       RPTRDT              44
     O                       RPPNAM              87
     O                       XXRPNM             130
     O          E            TOT
     O                                           13 'TOTAL COUNT:'
     O                       XXCNT         4     21
