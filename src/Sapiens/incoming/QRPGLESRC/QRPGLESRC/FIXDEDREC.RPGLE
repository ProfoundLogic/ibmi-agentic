     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* DESCRIPTION:  This program reads the Payment Detail file      *
     H*   and corrects the trans code and PV1 code.                   *
     H*                                                               *
     H*****************************************************************
     F*
     FWDPAL     UF   E           K DISK
     FWMFNL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*=========================================================================
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    PAFND
     C*
     C     *LIKE         DEFINE    PATRCD        XXTRCD
     C     *LIKE         DEFINE    PABR2         XXBR2
     C     *LIKE         DEFINE    FNFND         SVFND
     C                   Z-ADD     72            XXCO#             3 0
     C                   Z-ADD     0             XXCNT             5 0
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOA
     C*====================================================
     C*  Read all records
     C*
     C                   READ      PPA01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C*  Only process deductible recoveries
     C*
     C     PAPGM         IFEQ      'W8021R1'
     C*
     C*  Get the Fund record to compare trans and PV1 codes.
     C*
     C     PAFND         IFNE      SVFND
     C     FNKY          CHAIN     PFN01                              60
     C   60              GOTO      NEXT
     C                   Z-ADD     FNFND         SVFND
     C                   ENDIF
     C*
     C     PARETP        IFEQ      'C'
     C                   MOVE      FNCOTR        XXTRCD
     C                   MOVE      FNCOP1        XXBR2
     C                   ENDIF
     C     PARETP        IFEQ      'M'
     C                   MOVE      FNMETR        XXTRCD
     C                   MOVE      FNMEP1        XXBR2
     C                   ENDIF
     C     PARETP        IFEQ      'E'
     C                   MOVE      FNEXTR        XXTRCD
     C                   MOVE      FNEXP1        XXBR2
     C                   ENDIF
     C     PARETP        IFEQ      'L'
     C                   MOVE      FNLETR        XXTRCD
     C                   MOVE      FNLEP1        XXBR2
     C                   ENDIF
     C*
     C     PATRCD        IFNE      XXTRCD
     C     PABR2         ORNE      XXBR2
     C*
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       1             XXCNT
     C*
     C                   MOVE      XXTRCD        PATRCD
     C                   MOVE      XXBR2         PABR2
     C                   UPDATE    PPA01
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     NEXT          TAG
     C                   READ      PPA01                                  60
     C                   ENDDO
     C*
     C                   EXCEPT    TOTAL
     C                   MOVE      '1'           *INLR
     C*
     O*=========================================================================
     O*  EDI 148 UPLOAD DETAIL REPORT
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                                           68 'ANTHEM'
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                                            9 'FIXDEDREC'
     O                                           52 'Deductible recoveries wh'
     O                                           76 'ich had transaction and '
     O                                          100 'Pmt Val 1 codes correcte'
     O                                          102 'd.'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                           28 'Reserve'
     O                                           40 'Previous'
     O                                           54 'Correct'
     O                                           60 'Trans   PV1'
     O          E            HEAD           2
     O                                           18 'Fnd   Fyr   Claim#'
     O                                           26 'type'
     O                                           42 'Trans   PV1'
     O                                           56 'Trans   PV1'
     O                                           71 'Amount'
     O          E            DET            1
     O                       PAFND         4      3
     O                       PAFYR         4      9
     O                       PACASE        4     18
     O                       PARETP              24
     O                       PATRCD              34
     O                       PABR2               41
     O                       XXTRCD              48
     O                       XXBR2               55
     O                       PAAMT         J     71
     O          E            TOTAL       1  1
     O                                           17 'Total recoveries:'
     O                       XXCNT         4     24
