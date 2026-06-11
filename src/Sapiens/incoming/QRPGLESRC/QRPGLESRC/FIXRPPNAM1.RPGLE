     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE UPDATE DQAMTR FOR RECORDS PRIOR TO DEDUC CHANGE INSTALL
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     FWARPP     UF   E             DISK
     FWMPDL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C**************************************************************************
     C*
     C     PDKY          KLIST
     C                   KFLD                    RPFEIN
     C                   KFLD                    RPMOD
     C*
     C                   MOVE      *BLANKS       XXRPNM           30
     C                   MOVE      *BLANKS       XXPDNM           30
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*----------------------------------------------------------------
     C*  Start of main loop
     C*
     C                   READ      PRP01                                  99
     C     *IN99         DOWEQ     '0'
     C*
     C     RPFEIN        IFNE      *ZERO
     C     PDKY          CHAIN     PPD01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     PDDBA1        IFEQ      'N'
     C     RPPNAM        ANDEQ     PDADR1
     C     PDDBA1        OREQ      ' '
     C     RPPNAM        ANDEQ     PDADR1
     C*
     C                   MOVEL(P)  RPPNAM        XXRPNM
     C                   MOVEL(P)  PDNAME        XXPDNM
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C     PDFSN         IFNE      *BLANKS
     C                   MOVEL(P)  PDFSN         RPPNAM
     C                   CAT       PDNAME:1      RPPNAM
     C                   ELSE
     C                   MOVEL(P)  PDNAME        RPPNAM
     C                   ENDIF
     C                   MOVEL(P)  PDADR1        RPADR1
     C                   MOVEL(P)  PDADR2        RPADR2
     C                   UPDATE    PRP01
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   READ      PRP01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                                           10 'FIXRPPNAM'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            3 'FND'
     O                                            8 'FYR'
     O                                           17 'CASE'
     O                                           24 'SEQ#'
     O                                           33 'RPPNAM'
     O                                           66 'PDADR1'
     O          E            DET
     O                       RPFND         4      3
     O                       RPFYR         4      8
     O                       RPCASE        4     17
     O                       RPSEQ#        4     24
     O                       XXRPNM              57
     O                       XXPDNM              90
