     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FXARPP     IF   E             DISK
     FWMCML1    IF   E           K DISK
     FWARPP     O    E             DISK
     F                                     RENAME(PRP01:PRP02)
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DESC            C                   CONST('INTEREST EXPENSE')
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *

     C     *ENTRY        PLIST
     C                   PARM                    QQTRDT            8            MMDDCCYY
     C*
     C     CMKY          KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     *LIKE         DEFINE    RPAMT         NAMT
     C     *LIKE         DEFINE    RPAMT         OAMT
      *
     C                   MOVE      QQTRDT        XXTRDT            8 0
     C                   TIME                    DSDTTM
     C                   MOVE      *ON           *INOF
      *
     C                   READ      PRP01                                  99
      *
     C     *IN99         DOWEQ     '0'
     C                   ADD       1             OCOUNT            9 0
      *
     C                   EXSR      SELECT
      *
     C     *IN01         IFEQ      *ON
     C                   ADD       1             ICOUNT            9 0
     C                   EXSR      PRT
      *
     C                   EXSR      MOVE
     C                   EXSR      CALC
      *
     C                   MOVE      *ON           NEW
     C                   EXSR      PRT
     C                   WRITE     PRP02
     C                   ENDIF
      *
     C                   READ      PRP01                                  99
     C                   ENDDO
      *** PRINT TOTALS
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HDR
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C                   EXCEPT    TOT
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
      *
     C     SELECT        BEGSR
      *
     C                   MOVE      *ON           *IN01
      *
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVE      *OFF          *IN01
     C                   ENDIF
      *
     C     CMCTG2        IFEQ      '02'
     C     CMCTG2        OREQ      '03'
     C     RPBR7         IFEQ      '01'
     C     RPBR7         OREQ      '97'
     C     RPBR7         OREQ      '99'
     C                   MOVE      *OFF          *IN01
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR
      *
     C     MOVE          BEGSR
      *
     C                   Z-ADD     RPAMT         OAMT
      *
     C                   Z-ADD     99            RPSEQ#
      *
     C                   Z-ADD     XXTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPTRDT
      *
     C                   MOVEL(P)  DESC          RPDESC
     C                   MOVE      *BLANKS       RPBUL1
     C                   MOVE      *BLANKS       RPCHK#
     C                   MOVE      *BLANKS       RPSTAT
     C*
     C                   MOVE      Q1USER        RPUSER
     C                   MOVE      Q1PGM         RPPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   Z-ADD     DSTIME        RPTIME
      *
     C                   ENDSR
      *
     C     CALC          BEGSR
      *
     C                   ADD       RPAMT         TOTOLD           11 2
      *
     C                   Z-ADD     .001643835    RATE             11 9
     C     RPAMT         MULT      RATE          NAMT
     C                   Z-ADD     NAMT          RPAMT
      *
     C                   ADD       NAMT          TOTNEW           11 2
      *
     C                   ENDSR
      *
     C     PRT           BEGSR
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HDR
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C                   MOVEL(P)  RPPNAM        XXPNAM           30
     C                   EXCEPT    DET
     C     NEW           IFEQ      *ON
     C                   EXCEPT    DETS
     C                   MOVE      *OFF          NEW               1
     C                   ENDIF
     C                   ENDSR
     OQSYSPRT   E            HDR            1 06
     O                       DSTIME              08 '  :  :  '
     O                                           78 'INTEREST CHECK REPORT'
     O                                          122 'DATE:'
     O                       DSDATE        Y    131
     O          E            HDR            2
     O                       Q1PGM               10
     O                                          122 'PAGE:'
     O                       PAGE          Z    131
     O          E            HDR            1
     O                                         +  0 'NAME'
     O                                         + 28 'PV1'
     O                                         +  2 'PV2'
     O                                         +  3 'VOUCHER #'
     O                                         +  2 'CLAIM #'
     O                                         + 14 'AMOUNT'
     O                                         +  4 'SERVICE DATES'
     O                                         +  3 'COV TYPE'
     O          E            DET            1
     O                       XXPNAM            +  0
     O                       RPBR2             +  2
     O                       RPBR7             +  3
     O                       RPCHK#        Z   +  4
     O                       RPFND             +  4
     O                       RPFYR             +  0
     O                       RPCASE            +  0
     O                       RPAMT         J   +  2
     O                       RPFRDT        Z   +  2
     O                                         +  0 '-'
     O                       RPTODT        Z   +  0
     O                       CMCTG2            +  3
     O*****                    RPDESC  +  2
     O          E            DETS           1
     O          E            TOT            2
     O                                         +  0 'ORIGINAL AMOUNT:'
     O                       TOTOLD        J   +  2
     O          E            TOT            2
     O                                         +  0 ' ORIGINAL COUNT:'
     O                       OCOUNT        J   +  2
     O          E            TOT            2
     O                                         +  0 'INTEREST AMOUNT:'
     O                       TOTNEW        J   +  2
     O          E            TOT            2
     O                                         +  0 'NEW CHECK COUNT:'
     O                       ICOUNT        J   +  2
