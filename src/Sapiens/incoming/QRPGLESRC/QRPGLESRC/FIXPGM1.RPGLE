     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*  Program - SUP31816
     H*****************************************************************
     H* Description:  Identify and fix claims with incorrect # of     *
     H*               days paid.                                      *
     H*                                                               *
     H*    CREATED BY:  C:15   04/11/02   SEQ#: 31816                 *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/17/03 C:53 Support: WC065/38013                            *
     H*               Correct weeks/days calc.  When a neg recovery   *
     H*               is processed, the weeks/days should be added.   *
     H*****************************************************************
     H*                                                               *
     FWMCML1    IF   E           K DISK
     FWDPAL     IF   E           K DISK
     F*
     FQSYSPRT   O    F   85        PRINTER OFLIND(*INOF)

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*---------------------------------------------------------------
     C*
     C*  Key list for the Payment Detail File
     C*
     C     PAKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C                   Z-ADD     0             XXLOST            5 0
     C                   Z-ADD     0             XXWK50            5 0
     C                   Z-ADD     0             XXDIFF            5 0
     C                   Z-ADD     0             XXCNT             7 0
     C*
     C* Set-up and Initialize
     C*
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*
     C*_______________________________________________________________
     C*
     C*  Process all claims
     C*
     C                   READ      PCM01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     0             XXLOST
     C                   Z-ADD     0             XXWK50
     C*
     C*  Calculate the # of days paid.
     C*
     C     PAKY          CHAIN     PPA01                              98
     C     *IN98         DOWEQ     '0'
     C     PARETP        IFEQ      'C'
     C                   EXSR      SRDYPD
     C                   ENDIF
     C     PAKY          READE     PPA01                                  98
     C                   ENDDO
     C*
     C*  If the calculated total does not match the claim total,
     C*  print and update.
     C*
     C     CMLOST        IFNE      XXLOST
     C*
     C     CMLOST        SUB       XXLOST        XXDIFF
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       1             XXCNT
     C*
     C                   Z-ADD     XXLOST        CMLOST
     C*********************UPDATPCM01
     C*
     C                   ENDIF
     C*
     C                   READ      PCM01                                  99
     C                   ENDDO                                                  *IN99=*OFF
     C*
     C                   EXCEPT    TOT
     C*----------------------------------------------------------------
     C                   MOVE      *ON           *INLR
     C*
     C*================================================================
     C     SRDYPD        BEGSR
     C*
     C                   Z-ADD     0             XXWK50
     C*
     C*  Calculate days paid
     C*
     C     PAWKS         IFNE      *ZERO
     C     CMDAYS        IFNE      *ZERO
     C     PAWKS         MULT      CMDAYS        XXWK50
     C                   ELSE
     C     PAWKS         MULT      7             XXWK50
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ADD       PADAYS        XXWK50
     C*
     C*  Determine whether to add or subtract from total days.
     C*
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'Z'
     C     PATYPE        OREQ      'A'
     C     PAAMT         ANDGE     0
     C*** BEG ADD *** 07/17/03 **************************************
     C     PATYPE        OREQ      'R'
     C     PAAMT         ANDLT     0
     C*** END ADD *** 07/17/03 **************************************
     C*
     C                   ADD       XXWK50        XXLOST
     C*
     C                   ELSE
     C*
     C                   SUB       XXWK50        XXLOST
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C*_______________________________________________________________
     C*
     OQSYSPRT   E            HEAD           2 06
     O                       Q1PGM               10
     O                                           42 '# of Days Paid'
     O                                           56 'Discrepancies'
     O                       DSDATE              75 '  /   /    '
     O          E            HEAD           1
     O                                            4 'Fnd'
     O                                            9 'Yr'
     O                                           17 'Case'
     O                                           27 'CMLOST'
     O                                           36 'XXLOST'
     O                                           45 'Diff'
     O          E            DET         1
     O                       CMFND         4      4
     O                       CMFYR         4      9
     O                       CMCASE        4     17
     O                       CMLOST        4     27
     O                       XXLOST        4     36
     O                       XXDIFF        4     45
     O          E            TOT         2  1
     O                                           14 'Total claims:'
     O                       XXCNT               22
