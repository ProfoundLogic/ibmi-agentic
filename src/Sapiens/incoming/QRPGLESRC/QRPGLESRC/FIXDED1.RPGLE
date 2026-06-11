     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* DESCRIPTION:                                                  *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     F*
     FWDDDL1    UF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D*
     D CD              S              2    DIM(10)
     D AMB             S              9  2 DIM(10)
     D AMR             S              9  2 DIM(10)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*=========================================================================
     C*  Key list for the Deduct Detail file
     C*
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C     DDKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    SVFYR
     C                   KFLD                    SVCASE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQUPD             1
     C*
     C     *LIKE         DEFINE    DDCO#         XXCO#
     C     *LIKE         DEFINE    DDFND         SVFND
     C     *LIKE         DEFINE    DDFYR         SVFYR
     C     *LIKE         DEFINE    DDCASE        SVCASE
     C     *LIKE         DEFINE    DDEMP#        SVEMP#
     C     *LIKE         DEFINE    DDDIV         SVDIV
     C                   MOVE      QQCO#         XXCO#
     C                   Z-ADD     0             XXCNT             5 0
     C                   Z-ADD     0             XEDIFF            9 2
     C                   Z-ADD     1             X                 3 0
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOA
     C*====================================================
     C*  Read all records
     C*
     C     DDKY          CHAIN(N)  PDD01                              61
B01  C     *IN61         DOWEQ     '0'
     C*
     C                   MOVE      DDFND         SVFND
     C                   MOVE      DDFYR         SVFYR
     C                   MOVE      DDCASE        SVCASE
     C                   MOVE      DDEMP#        SVEMP#
     C                   MOVE      DDDIV         SVDIV
     C*
     C*  Accumulate the total billed, recovered and pended for this claim
     C*    by reserve code.
     C*
     C                   CLEAR                   AMB
     C                   CLEAR                   AMR
     C                   CLEAR                   CD
     C                   Z-ADD     0             XXDIFF            9 2
     C     *IN61         DOUEQ     '1'
     C*
     C                   Z-ADD     1             X
     C     DDRECD        LOOKUP    CD(X)                                  60
     C  N60              Z-ADD     1             X
     C  N60*BLANKS       LOOKUP    CD(X)                                  60
     C                   MOVEL     DDRECD        CD(X)
     C                   ADD       DDAMTB        AMB(X)
     C                   ADD       DDAMTR        AMR(X)
     C                   ADD       DDAMTP        AMR(X)
     C*
     C     DDKY1         READE(N)  PDD01                                  61
     C                   ENDDO
     C*
     C*  Process each reserve code individually.
     C*
     C                   Z-ADD     1             X
B02  C     X             DOWLE     10
     C     CD(X)         ANDNE     *BLANKS
     C*
     C*  If the sum of the pended and recovered amounts is greater than
     C*    the sum of the billed amounts, then an adjustment needs to
     C*    be made.
     C*
     C     AMR(X)        IFGT      AMB(X)
     C*
     C     AMR(X)        SUB       AMB(X)        XXDIFF
     C                   Z-ADD     XXDIFF        SVDIFF            9 2
     C*
     C                   EXSR      SRADJ
     C*
     C                   ENDIF
     C*
     C                   ADD       1             X
E02  C                   ENDDO
     C*
     C     DDKY1         SETGT     PDD01
     C     DDKY          READE(N)  PDD01                                  61
     C*
     C     SVEMP#        IFNE      DDEMP#
     C     SVDIV         ORNE      DDDIV
     C     XEDIFF        IFNE      0
     C                   EXCEPT    EMPTOT
     C                   Z-ADD     0             XEDIFF
     C                   ENDIF
     C                   ENDIF
     C*
E01  C                   ENDDO
     C*
     C*
     C                   EXCEPT    TOTAL
     C                   MOVE      '1'           *INLR
     C*
     C*=========================================================================
     C*  SRADJ - This subroutine adjusts the Deduct Detail file.
     C*=========================================================================
     C     SRADJ         BEGSR
     C*
     C     DDKY1         CHAIN     PDD01                              60
B01  C     XXDIFF        DOWGT     0
     C*
     C                   MOVE      'N'           XXUPD             1
B02  C     DDRECD        IFEQ      CD(X)
     C*
     C     DDAMTB        IFGT      0                                            <-----
     C     DDAMTP        ANDGT     0                                                 -
     C*                                                         -
     C     DDAMTP        IFGE      XXDIFF                                       <--  -
     C                   SUB       XXDIFF        DDAMTP                           -  -
     C                   Z-ADD     0             XXDIFF                           -  -
     C                   ELSE                                                    --  -
     C                   SUB       DDAMTP        XXDIFF                           -  -
     C                   Z-ADD     0             DDAMTP                           -  -
     C                   ENDIF                                                  <--  -
     C*                                                         -
     C                   MOVE      'Y'           XXUPD                               -
     C                   ENDIF                                                  <-----
     C*
     C     DDAMTB        IFLT      0                                            <-----
     C*                                                         -
     C     DDAMTP        SUB       DDAMTB        XXAVAL            9 2               -
     C     XXAVAL        IFGE      XXDIFF                                       <--  -
     C                   SUB       XXDIFF        DDAMTP                           -  -
     C                   Z-ADD     0             XXDIFF                           -  -
     C                   ELSE                                                     -  -
     C                   SUB       XXAVAL        DDAMTP                           -  -
     C                   SUB       XXAVAL        XXDIFF                           -  -
     C                   ENDIF                                                  <--  -
     C*                                                         -
     C                   MOVE      'Y'           XXUPD                               -
     C                   ENDIF                                                  <-----
     C*
     C     XXUPD         IFEQ      'Y'
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       1             XXCNT
     C     QQUPD         IFEQ      'Y'
     C                   UPDATE    PDD01
     C                   ENDIF
     C                   ENDIF
     C*
E02  C                   ENDIF
     C*
     C     DDKY1         READE     PDD01                                  60
E01  C                   ENDDO
     C                   ADD       SVDIFF        XEDIFF
     C*
     C                   ENDSR
     O*=========================================================================
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                                            7 'FIXDED1'
     O                                           52 'Deductible Detail record'
     O                                           76 's which need to be adjus'
     O                                          100 'ted due to discrepancies'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            3 'Grp'
     O                                           14 'Employer#'
     O                                           21 'Div'
     O                                           27 'FYR'
     O                                           36 'Claim #'
     O                                           48 'Amt billed'
     O                                           61 'Amt rcvrd'
     O                                           74 'Amt pending'
     O                                           87 'Amt remaing'
     O                                          100 'Tot adj amt'
     O          E            DET            1
     O                       DDFND         4      3
     O                       DDEMP#        4     14
     O                       DDDIV         4     21
     O                       DDFYR         4     27
     O                       DDCASE        4     36
     O                       DDAMTB              49 '       .  -'
     O                       DDAMTR              62 '       .  -'
     O                       DDAMTP              75 '       .  -'
     O                       XXDIFF              88 '       .  -'
     O                       SVDIFF             101 '       .  -'
     O          E            EMPTOT      1  2
     O                                           88 'Employer total adjusted:'
     O                       XEDIFF             101 '       .  -'
     O          E            TOTAL       1  1
     O                                           18 'Total adjustments:'
     O                       XXCNT         4     25
