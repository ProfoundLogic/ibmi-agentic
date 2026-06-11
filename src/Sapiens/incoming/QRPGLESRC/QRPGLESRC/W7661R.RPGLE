     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H/TITLE W7661R - CALCULATED VS. BILLED PREMIUM REPORT
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 05/02/01 C:61  PROJECT WC1000                                 *
     H*                (changes not commented)                        *
     H*              - Center header title information.               *
     H*              - Format routing information                     *
     H*              - Correct paging                                 *
     H*              - Do not print CX date for Employers w/ summary  *
     H*                billing (DIV=99999)                            *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS                                                    *
     H*                                                               *
     H* 71      SUMMARY BILLING                                       *
     H*                                                               *
     H*****************************************************************
      *
      *
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWXPCL     IF   E           K DISK
     FWXPCL1    IF   E           K DISK
     F                                     RENAME(PPC01:PPC02)
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWFPCP     O    E             DISK
     F                                     RENAME(PPC01:PPC03)
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOA)
      *

     D XTITL0          C                   CONST('CALCULATED PREMIUM V-
     D                                     S BILLED PREMIUM')
      *
     D XTITL1          C                   CONST('CALCULATED PREMIUM V-
     D                                     S BILLED PREMIUM - (-
     D                                     NON-AUDITED S/R)')
     D*
     D*
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D                 DS
     D  AR                     1     14
     D                                     DIM(7) ASCEND                        A/R CODES
     D  AR1                    1      2
     D  AR2                    3      4
     D  AR3                    5      6
     D  AR4                    7      8
     D  AR5                    9     10
     D  AR6                   11     12
     D  AR7                   13     14
      *
      *
      *

     IPPC01
     I              PCPFDT                      P1PFDT
     I              PCPTDT                      P1PTDT
     I              PCCXDT                      P1CXDT
      *
     IPPC02
     I              PCPFDT                      P2PFDT
     I              PCPTDT                      P2PTDT
     I              PCCXDT                      P2CXDT
      *
      **************************************************************************
      *

     C     *LIKE         DEFINE    PCCO#         XXCO#
     C     *LIKE         DEFINE    PCFND         XXFND
     C     *LIKE         DEFINE    PCFYR         XXFYR
     C     *LIKE         DEFINE    CONAME        XXCONM
     C     *LIKE         DEFINE    PCCPRM        XXTCPR
     C     *LIKE         DEFINE    PCBPRM        XXTBPR
     C     *LIKE         DEFINE    PCDIFF        XXTDIF
     C     *LIKE         DEFINE    PCCPRM        XXTCP1
     C     *LIKE         DEFINE    PCBPRM        XXTBP1
     C     *LIKE         DEFINE    PCDIFF        XXTDF1
     C     *LIKE         DEFINE    ORTITL        XXTTL0
     C     *LIKE         DEFINE    ORTITL        XXTTL1
      *
     C     *LIKE         DEFINE    ORDIS1        X0DIS1
     C     *LIKE         DEFINE    ORDIS2        X0DIS2
     C     *LIKE         DEFINE    ORDIS3        X0DIS3
     C     *LIKE         DEFINE    ORDIS4        X0DIS4
     C     *LIKE         DEFINE    ORPNAM        X0PNAM
     C     *LIKE         DEFINE    ORFIL#        X0FIL#
     C     *LIKE         DEFINE    ORCOPY        X0COPY
     C     *LIKE         DEFINE    ORHOLD        X0HOLD
     C     *LIKE         DEFINE    OROUTQ        X0OUTQ
     C     *LIKE         DEFINE    ORFORM        X0FORM
     C     *LIKE         DEFINE    ORSAVE        X0SAVE
      *
     C     *LIKE         DEFINE    ORDIS1        X1DIS1
     C     *LIKE         DEFINE    ORDIS2        X1DIS2
     C     *LIKE         DEFINE    ORDIS3        X1DIS3
     C     *LIKE         DEFINE    ORDIS4        X1DIS4
     C     *LIKE         DEFINE    ORPNAM        X1PNAM
     C     *LIKE         DEFINE    ORFIL#        X1FIL#
     C     *LIKE         DEFINE    ORCOPY        X1COPY
     C     *LIKE         DEFINE    ORHOLD        X1HOLD
     C     *LIKE         DEFINE    OROUTQ        X1OUTQ
     C     *LIKE         DEFINE    ORFORM        X1FORM
     C     *LIKE         DEFINE    ORSAVE        X1SAVE
      *
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQSRAC            1
     C                   PARM                    QQARCD           14
     C                   PARM                    QQPRTF            1
     C                   PARM                    QQQUE            10
     C                   PARM                    QQSORT            1
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *
      *
     C                   TIME                    DSDTTM
     C                   MOVE      *BLANKS       XXNAME           30
     C                   MOVE      *BLANKS       XXBCDS           20
      *
     C                   MOVE      *OFF          *INOF
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFRDT        XXFDT             8 0
     C                   MOVE      QQTODT        XXTDT             8 0
     C                   MOVEA     QQARCD        AR
      *
     C                   Z-ADD     *ZEROS        XXTCPR
     C                   Z-ADD     *ZEROS        XXTBPR
     C                   Z-ADD     *ZEROS        XXTDIF
     C                   Z-ADD     *ZEROS        XXTCP1
     C                   Z-ADD     *ZEROS        XXTBP1
     C                   Z-ADD     *ZEROS        XXTDF1
     C                   Z-ADD     *ZEROS        XXCNT             7 0
     C                   Z-ADD     *ZEROS        XXCNT1            7 0
      *
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09
      *
     C                   ELSE
      *
     C                   SETON                                        08
     C                   ENDIF
      *
     C     QQSORT        IFEQ      'E'
     C                   MOVE      *ON           *IN30
      *
     C                   ELSE
      *
     C                   MOVE      *OFF          *IN30
     C                   ENDIF
      *
      *  USER PARAMETER
      *
     C     UPKY          CHAIN     WMUPL                              60
      *
      *   ROUTE
      *
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVEL(P)  ORTITL        XXTTL0
     C                   MOVEL(P)  ORDIS1        X0DIS1
     C                   MOVEL(P)  ORDIS2        X0DIS2
     C                   MOVEL(P)  ORDIS3        X0DIS3
     C                   MOVEL(P)  ORDIS4        X0DIS4
     C                   MOVE      ORPNAM        X0PNAM
     C                   MOVE      ORFIL#        X0FIL#
     C                   MOVE      ORCOPY        X0COPY
     C                   MOVE      ORHOLD        X0HOLD
     C                   MOVE      OROUTQ        X0OUTQ
     C                   MOVE      ORFORM        X0FORM
     C                   MOVE      ORSAVE        X0SAVE
      *
     C     ORDIS1        IFEQ      *BLANKS
     C     ORDIS2        ANDEQ     *BLANKS
     C     ORDIS3        ANDEQ     *BLANKS
     C     ORDIS4        ANDEQ     *BLANKS
     C                   MOVEL(P)  UPUSNM        X0DIS1
     C                   ENDIF
      *
     C                   ELSE
      *
     C                   MOVEL(P)  XTITL0        XXTTL0
     C                   MOVEL(P)  UPUSNM        X0DIS1
     C                   ENDIF                                                  *IN60=*OFF
      *
      *  ROUTE1
      *
     C                   MOVE      '1'           XXPRT
     C     ORKY          CHAIN     POR01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVEL(P)  ORTITL        XXTTL1
     C                   MOVEL(P)  ORDIS1        X1DIS1
     C                   MOVEL(P)  ORDIS2        X1DIS2
     C                   MOVEL(P)  ORDIS3        X1DIS3
     C                   MOVEL(P)  ORDIS4        X1DIS4
     C                   MOVE      ORPNAM        X1PNAM
     C                   MOVE      ORFIL#        X1FIL#
     C                   MOVE      ORCOPY        X1COPY
     C                   MOVE      ORHOLD        X1HOLD
     C                   MOVE      OROUTQ        X1OUTQ
     C                   MOVE      ORFORM        X1FORM
     C                   MOVE      ORSAVE        X1SAVE
      *
     C     ORDIS1        IFEQ      *BLANKS
     C     ORDIS2        ANDEQ     *BLANKS
     C     ORDIS3        ANDEQ     *BLANKS
     C     ORDIS4        ANDEQ     *BLANKS
     C                   MOVEL(P)  UPUSNM        X1DIS1
     C                   ENDIF
      *
     C                   ELSE
      *
     C                   MOVEL(P)  XTITL1        XXTTL1
     C                   MOVEL(P)  UPUSNM        X1DIS1
     C                   ENDIF                                                  *IN60=*OFF
      *
      *
      *
     C     QQSRAC        IFEQ      'N'
     C                   MOVE      *ON           *IN21
     C                   ENDIF
      *
     C     QQPRTF        IFNE      'F'
     C                   MOVE      *ON           *IN22
     C                   ENDIF
      *
     C     QQPRTF        IFNE      'P'
     C                   MOVE      *ON           *IN23
     C                   ENDIF
      *
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        XXCONM
     C                   MOVEL     CONAME        PCCNAM
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      *
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        XXFNNM           25
     C                   MOVEL     FNNAME        PCFNAM
      *
     C                   MOVE      Q1PGM         PCPGM
     C                   MOVE      Q1USER        PCUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PCDT
     C                   Z-ADD     DSTIME        PCTIME
      *
      ***** Beg Del ***** 08/26/14 ************************************
     C********   22              EXCEPT    ROUTE
     C********   21
     C********AN 22              EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 ************************************
      *
     C                   MOVEL(P)  XXTTL0        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        XXTTL0
      *
     C                   MOVEL(P)  XXTTL1        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        XXTTL1
      *
     C   22              EXCEPT    HEAD
     C   21
     CAN 22              EXCEPT    HEAD1
      *
      *  Start main loop
      *
     C   30              READ      PPC01                                  60
     C  N30              READ      PPC02                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C   30              Z-ADD     P1PFDT        SRDTI
     C  N30              Z-ADD     P2PFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT            8 0
     C                   Z-ADD     XXPFDT        PCPFDT
      *
     C   30              Z-ADD     P1PTDT        SRDTI
     C  N30              Z-ADD     P2PTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPTDT            8 0
     C                   Z-ADD     XXPTDT        PCPTDT
      *
     C   30              Z-ADD     P1CXDT        SRDTI
     C  N30              Z-ADD     P2CXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCXDT            8 0
     C                   Z-ADD     XXCXDT        PCCXDT
      *
     C     XXCXDT        IFNE      *ZERO
     C     PCDIV         ANDEQ     99999
     C                   SETON                                        71
     C                   ELSE
     C                   SETOFF                                       71
     C                   ENDIF
      *
     C                   MOVEL     PCENAM        XXNAME
     C                   MOVEL     PCBCDS        XXBCDS
      *
     C     PCBSFL        IFEQ      'S'
     C     PCAUDS        ANDNE     'A'
     C   21
     CAN 22
     CAN OA              EXCEPT    HEAD1
     C   OA              SETOFF                                       OA
     C   21
     CAN 22              EXCEPT    DET1
     C                   ADD       PCCPRM        XXTCP1
     C                   ADD       PCBPRM        XXTBP1
     C                   ADD       PCDIFF        XXTDF1
     C                   ADD       1             XXCNT1
      *
     C                   ELSE
      *
     C   22
     CAN OF              EXCEPT    HEAD
     C   OF              SETOFF                                       OF
     C   22              EXCEPT    DET
     C                   ADD       1             DSTIME
      *
     C     PCDIV         IFEQ      99999
     C                   MOVE      'Y'           PCARSM
      *
     C                   ELSE
      *
     C                   MOVE      'N'           PCARSM
     C                   END
      *
     C                   WRITE     PPC03
     C                   ADD       PCCPRM        XXTCPR
     C                   ADD       PCBPRM        XXTBPR
     C                   ADD       PCDIFF        XXTDIF
     C                   ADD       1             XXCNT
     C                   END
      *
     C   30              READ      PPC01                                  60
     C  N30              READ      PPC02                                  60
     C                   ENDDO
      *
     C     XXCNT         IFGE      1
     C   22              EXCEPT    TOT
      *
     C                   ELSE
      *
     C   22              EXCEPT    NOREC
     C                   END
      *
     C     XXCNT1        IFGE      1
     C   21
     CAN 22              EXCEPT    TOT1
      *
     C                   ELSE
      *
     C   21
     CAN 22              EXCEPT    NOREC1
     C                   END
      *
      *
      /Free
       //***** Beg Add ***** 08/26/14 ************************************
       If *in22 = '1';
          Except Route;
          If *in21 = '1';
             Except Route1;
          Endif;
       Endif;
       //***** End Add ***** 08/26/14 ************************************
      /End-free
     C                   MOVE      '1'           *INLR
      *
      *
      *
      **************************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
      **********
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             X0DIS1              56
      *
     O**********E            ROUTE          1
     O**********             X0DIS2              56
      *
     O**********E            ROUTE          1
     O**********             X0DIS3              56
      *
     O**********E            ROUTE          2
     O**********             X0DIS4              56
      ***** End Del ***** 08/26/14 *****************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       X0DIS1              56
      *
     O          E            ROUTE          1
     O                       X0DIS2              56
      *
     O          E            ROUTE          1
     O                       X0DIS3              56
      *
     O          E            ROUTE          2
     O                       X0DIS4              56
      ***** End Add ***** 08/05/15 ************************************

     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       XXTTL0            +001
      *
      ***** Beg Del ***** 08/26/14 *****************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             X0PNAM              30
     O**********                                 36 'FILE:'
     O**********             X0FIL#              38
     O**********                                 46 'COPIES:'
     O**********             X0COPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             X0HOLD              61
      *
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      X0OUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             X0FORM              48
     O**********                                 56 'SAVE:'
     O**********             X0SAVE              61
      ***** End Del ***** 08/26/14 *****************************************
      *
     O          E            ROUTE       2  2
     O                                         +  4 'REPORT SELECTIONS:'
      *
     O          E            ROUTE          2
     O                                         +  6 'COMPANY . . . . . . . :'
     O                       XXCO#         Z   +001
      *
     O          E            ROUTE          2
     O                                         +  6 'GROUP . . . . . . . . :'
     O                       XXFND             +  1
     O                       FNNAME            +  2
      *
     O          E            ROUTE          2
     O                                         +  6 'POLICY EFFECTIVE RANGE:'
     O                       XXFDT         Y   +  1
     O                                         +  1 '-'
     O                       XXTDT         Y   +  1
      *
     O          E            ROUTE          1
     O                                         +  6 'IGNORE S/R ACCOUNTS NOT'
     O          E            ROUTE          2
     O                                         +  6 'YET AUDITED . . . . . :'
     O                       QQSRAC            +  1
      *
     O          E            ROUTE          2
     O                                         +  6 'A/R CODES TO EXCLUDE  :'
     O                       AR1               +  1
     O                       AR2               +  1
     O                       AR3               +  1
     O                       AR4               +  1
     O                       AR5               +  1
     O                       AR6               +  1
     O                       AR7               +  1
      *
      *
     O          E            ROUTE          2
     O                                         +  6 'SORT ORDER  . . . . . :'
     O               30                          43 'EMPLOYER #   '
     O              N30                          43 'EMPLOYER NAME'
      *
     O          E            ROUTE          2
     O                                         +  6 'TYPE OF OUTPUT  . . . :'
     O               22N23                       35 'PRINT'
     O              N22 23                       35 'FILE '
     O               22 23                       35 'BOTH '
      *
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O****                               23 '(C) COPYRIGHT PROGRESSI'
     O****                             +  1 'VE DATA SOLUTIONS. INC.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      *
      *
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             119
     O                       DSDATE        Y    198
      *
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       XXTTL0             129
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
      *
     O          E            HEAD           2
     O                                            6 'GROUP:'
     O                       XXFND               10
     O                       FNNAME              72
      *
     O          E            HEAD           2
     O                                           21 '* A/R CODES EXCLUDED:'
     O                       AR1                 24
     O                       AR2                 27
     O                       AR3                 30
     O                       AR4                 33
     O                       AR5                 36
     O                       AR6                 39
     O                       AR7                 42
      *
     O          E            HEAD           1
     O                                           72 'EFFECTIVE'
     O                                           87 'EXPIRATION'
     O                                          100 'CANCEL'
     O                                          111 'AUDIT'
     O                                          127 'CALCULATED'
     O                                          142 'BILLED'
     O                                          173 'BILLING'
      *
     O          E            HEAD           2
     O                                            9 'EMPLR#'
     O                                           15 'DIV'
     O                                           24 'NAME'
     O                                           57 'FYR'
     O                                           70 'DATE'
     O                                           85 'DATE'
     O                                           99 'DATE'
     O                                          111 'STATUS'
     O                                          127 'PREMIUM'
     O                                          145 'PREMIUM(*)'
     O                                          161 'DIFFERENCE'
     O                                          174 'CODE/TYPE'
     O                                          187 'DESCRIPTION'
      *
     O          E            DET
     O                       PCEMP#        4      9
     O                       PCDIV         4     15
     O                       XXNAME              50
     O                       PCFYR         4     57
     O                       XXPFDT              72 '  /  /    '
     O                       XXPTDT              87 '  /  /    '
     O              N71      XXCXDT             100 '  /  /    '
     O               71                         101 '* CANCELLED *'
     O                       PCAUDS             109
     O                       PCCPRM        N    127
     O                       PCBPRM        N    142
     O                       PCDIFF        N    161
     O                       PCBSCD             170
     O                       PCBSFL             173
     O                       XXBCDS             196
      *
     O          E            TOT         1
     O                                          100 'TOTALS:'
     O                       XXTCPR        N    127
     O                       XXTBPR        N    142
     O                       XXTDIF        N    161
      *
     O          E            TOT         2
     O                                           22 'COUNT OF RECORDS WHERE'
     O                                           47 'CALC. PREMIUM DOES NOT E'
     O                                           67 'QUAL BILLED PREMIUM:'
     O                       XXCNT         4     79
      *
     O          E            NOREC
     O                                           26 '*** NO RECORDS FOUND ***'
      *
      *
      *
     OQSYSPRT1  E            ROUTE1         1 06
     O*********                                   8 'XXXXXXXX'
     O*********                                  20 'XXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXX     XX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE1         1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXXX    XX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE1         1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XXXX    XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE1         1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE1         1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE1         1
     O*********                                   8 'XXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  71 'XX'
     O********* E            ROUTE1         1
     O*********                                   4 'XXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE1         1
     O*********                                   5 'XXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE1         1
     O*********                                   6 'XX XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE1         1
     O*********                                   7 'XX  XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX    XXXX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE1         1
     O*********                                   8 'XX   XXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  33 'XXXXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX    XXXX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE1         3
     O*********                                   9 'XX    XXX'
     O*********                                  20 'XXXXXX'
     O*********                                  32 'XXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX     XXX'
     O*********                                  77 'XXXXXX'
      *
     O********* E            ROUTE1         1
     O*********                                  15 'ROUTE TO:'
     O*********              X1DIS1              56
      *
     O********* E            ROUTE1         1
     O*********              X1DIS2              56
      *
     O********* E            ROUTE1         1
     O*********              X1DIS3              56
      *
     O********* E            ROUTE1         2
     O*********              X1DIS4              56
      *
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O                       X1DIS1              56
      *
     O          E            ROUTE1         1
     O                       X1DIS2              56
      *
     O          E            ROUTE1         1
     O                       X1DIS3              56
      *
     O          E            ROUTE1         2
     O                       X1DIS4              56
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       XXTTL1            +001
      *
     O********* E            ROUTE1         2
     O*********                                  19 'PROGRAM NAME:'
     O*********              X1PNAM              30
     O*********                                  36 'FILE:'
     O*********              X1FIL#              38
     O*********                                  46 'COPIES:'
     O*********              X1COPY        Z     50
     O*********                                  56 'HOLD:'
     O*********              X1HOLD              61
      *
     O********* E            ROUTE1         2
     O*********                                  19 'OUTPUT QUEUE:'
     O*********      08      X1OUTQ              30
     O*********         09   QQQUE               30
     O*********                                  37 'FORMS:'
     O*********              X1FORM              48
     O*********                                  56 'SAVE:'
     O*********              X1SAVE              61
      *
     O          E            ROUTE1      2  2
     O                                         +  4 'REPORT SELECTIONS:'
      *
     O          E            ROUTE1         2
     O                                         +  6 'COMPANY . . . . . . . :'
     O                       XXCO#         Z   +001
      *
     O          E            ROUTE1         2
     O                                         +  6 'GROUP . . . . . . . . :'
     O                       XXFND             +  1
     O                       FNNAME            +  2
      *
     O          E            ROUTE1         2
     O                                         +  6 'POLICY EFFECTIVE RANGE:'
     O                       XXFDT         Y   +  1
     O                                         +  1 '-'
     O                       XXTDT         Y   +  1
      *
     O          E            ROUTE1         1
     O                                         +  6 'IGNORE S/R ACCOUNTS NOT'
     O          E            ROUTE1         2
     O                                         +  6 'YET AUDITED . . . . . :'
     O                       QQSRAC            +  1
      *
     O          E            ROUTE1         2
     O                                         +  6 'A/R CODES TO EXCLUDE  :'
     O                       AR1               +  1
     O                       AR2               +  1
     O                       AR3               +  1
     O                       AR4               +  1
     O                       AR5               +  1
     O                       AR6               +  1
     O                       AR7               +  1
      *
     O          E            ROUTE1         2
     O                                         +  6 'SORT ORDER  . . . . . :'
     O               30                          43 'EMPLOYER #   '
     O              N30                          43 'EMPLOYER NAME'
      *
     O          E            ROUTE1         2
     O                                         +  6 'TYPE OF OUTPUT  . . . :'
     O               22N23                       35 'PRINT'
     O              N22 23                       35 'FILE '
     O               22 23                       35 'BOTH '
      *
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE1
     O****                               23 '(C) COPYRIGHT PROGRESSI'
     O****                             +  1 'VE DATA SOLUTIONS. INC.,'
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      *
      *
     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             119
     O                       DSDATE        Y    198
      *
     O          E            HEAD1          2
     O                       Q1PGM               10
     O                       XXTTL1             129
     O                                          192 'PAGE:'
     O                       PAGE1         Z    198
      *
     O          E            HEAD1          2
     O                                            6 'GROUP:'
     O                       XXFND               10
     O                       FNNAME              72
      *
     O          E            HEAD1          2
     O                                           21 '* A/R CODES EXCLUDED:'
     O                       AR1                 24
     O                       AR2                 27
     O                       AR3                 30
     O                       AR4                 33
     O                       AR5                 36
     O                       AR6                 39
     O                       AR7                 42
      *
     O          E            HEAD1          1
     O                                           72 'EFFECTIVE'
     O                                           87 'EXPIRATION'
     O                                          100 'CANCEL'
     O                                          111 'AUDIT'
     O                                          127 'CALCULATED'
     O                                          142 'BILLED'
     O                                          173 'BILLING'
      *
     O          E            HEAD1          2
     O                                            9 'EMPLR#'
     O                                           15 'DIV'
     O                                           24 'NAME'
     O                                           57 'FYR'
     O                                           70 'DATE'
     O                                           85 'DATE'
     O                                           99 'DATE'
     O                                          111 'STATUS'
     O                                          127 'PREMIUM'
     O                                          145 'PREMIUM(*)'
     O                                          161 'DIFFERENCE'
     O                                          174 'CODE/TYPE'
     O                                          187 'DESCRIPTION'
      *
     O          E            DET1
     O                       PCEMP#        4      9
     O                       PCDIV         4     15
     O                       XXNAME              50
     O                       PCFYR         4     57
     O                       XXPFDT              72 '  /  /    '
     O                       XXPTDT              87 '  /  /    '
     O                       XXCXDT             100 '  /  /    '
     O                       PCAUDS             109
     O                       PCCPRM        N    127
     O                       PCBPRM        N    142
     O                       PCDIFF        N    161
     O                       PCBSCD             170
     O                       PCBSFL             173
     O                       XXBCDS             196
      *
     O          E            TOT1        1
     O                                          100 'TOTALS:'
     O                       XXTCP1        N    127
     O                       XXTBP1        N    142
     O                       XXTDF1        N    161
      *
     O          E            TOT1        2
     O                                           31 'COUNT OF S/R ACCOUNTS'
     O                                           53 'NOT AUDITED W/ DIFFER'
     O                                           58 'ENCE:'
     O                       XXCNT1        4     70
      *
     O          E            NOREC1
     O                                           26 '*** NO RECORDS FOUND ***'
      *
      *
