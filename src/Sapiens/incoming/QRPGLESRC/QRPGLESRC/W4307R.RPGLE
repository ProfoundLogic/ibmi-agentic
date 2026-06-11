     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE W4307R - CHANGE CLAIM OFFICE #
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*     Program reads the workfile produced by W0050R02 (WAOFP)   *
     H*     and chnages the claims accordingly.  If the claim office #*
     H*     eqauls the Previous Office# (OFPREV), the new office #    *
     H*     will be loaded into the claim record.  If the claim office*
     H*     # is NOT the same as the Previous Office#, no change will *
     H*     occur.  The WAOFP record will be deleted after all claims *
     H*     have been processed.                                      *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     W4307C - OVERRIDE CL                                      *
     H*  CALLS TO:                                                    *
     H*     NONE                                                      *
     H*                                                               *
     H*  CREATED: 07/14/98        PROGRAMMER: 33      PROJECT: WC577  *
     H*                                               SEQ#:WA048/12653*
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  04/10/00  C:118  PROJECT: WC987                              *
     H*                   LOAD ALTERNATE COMPANY NAME FROM FUND MASTER*
     H*                   IF AVAILABLE.                               *
     H*                                                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWAOFL     UF   E           K DISK
     FWMCML     UF   E           K DISK
     FWMEML     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOB)
     F*

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D NOREC           C                   CONST('* * * NO RECORDS LIS-
     D                                     TED FOR THIS GROUP *-
     D                                      * *')
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  OFFICE AUXILARY FILE
     C*
     C     OFKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*_______________________________________________________________
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMFND         SVFND
     C     *LIKE         DEFINE    CMDT          SYSDT
     C     *LIKE         DEFINE    ORFIL#        XXPRT
     C     *LIKE         DEFINE    ORTITL        XXTITL
     C     *LIKE         DEFINE    ORTITL        X0TITL
     C     *LIKE         DEFINE    ORTITL        X1TITL
     C     *LIKE         DEFINE    XXCNT         XXCNT1
     C     *LIKE         DEFINE    QQCALL        XXCALL
     C*_______________________________________________________________
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQCALL            1
     C*_______________________________________________________________
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   Z-ADD     *ZEROS        SVFND
     C*
     C     Q1PRMS        IFGE      2
     C                   MOVEL(P)  QQCALL        XXCALL
     C                   ENDIF                                                  Q1PRMS>=2
     C*
     C*  CHAIN TO THE COMPANY FILE
     C*
     C     XXCO#         CHAIN     PCO01                              60
     C*
     C*  CENTER COMPANY NAME
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        CONAME
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION AND PRINT
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      *OFF
     C                   EXSR      LTITLE
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   EXSR      CTITLE
     C                   MOVEL     ORTITL        X0TITL
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      *OFF
     C                   EXSR      LTITLE
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 ************************************
     C                   EXSR      CTITLE
     C                   MOVEL     ORTITL        X1TITL
     C                   ENDIF                                                  *IN60=*OFF
     C*_______________________________________________________________
     C*
     C*  START READING WORK FILE
     C*
     C     OFKY          CHAIN     POF01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C*  GET THE EMPLOYER NAME
     C*
     C     EMKY          CHAIN     PEM01                              60
     C   60              CLEAR                   EMNAME
     C*
     C*  LOAD THE WORK FIELDS
     C*
     C                   Z-ADD     OFCO#         XXCO#
     C                   Z-ADD     OFFND         XXFND
     C                   Z-ADD     OFEMP#        XXEMP#
     C                   Z-ADD     OFDIV         XXDIV
     C*
     C     SVFND         IFNE      XXFND
     C                   EXSR      SRFND
     C                   ENDIF                                                  SVFND<>XXFND
     C*
     C     CMKY          CHAIN     PCM01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     CMLSN         CAT(P)    ',':0         XXCMNM           40
     C                   CAT       CMFSN:1       XXCMNM
     C                   CAT       CMMID:1       XXCMNM
     C*
     C     CMOFF#        IFEQ      OFPREV
     C*
     C                   EXSR      CHGOFF
     C*
     C                   ELSE
     C*
     C                   ADD       1             XXCNT1
     C*
     C     *INOB         IFEQ      *ON
     C                   EXCEPT    HEAD1
     C                   MOVE      *OFF          *INOB
     C                   ENDIF                                                  *INOB=*ON
     C*
     C                   EXCEPT    DET1
     C*
     C                   ENDIF                                                  CMOFF#=OFPREV
     C*
     C     CMKY          READE     PCM01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C*   DELETE THE CURRENT RECORD
     C*
     C                   DELETE    POF01
     C*
     C     OFKY          READE     POF01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*_______________________________________________________________
     C*
     C     XXCNT         IFEQ      *ZERO
     C                   EXCEPT    MSG
     C                   ENDIF                                                  XXCNT=*ZERO
     C*
     C     XXCNT1        IFEQ      *ZERO
     C                   EXCEPT    MSG1
     C                   ENDIF                                                  XXCNT1=*ZERO
     C*_______________________________________________________________
     C*
     C     TERM          TAG
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKy POR01;
       If %found;
          Except Route;
       Endif;
       xxPrt = '1';
       Chain orKy POR01;
       If %found;
          Except Route1;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      *ON           *INLR
     C*_______________________________________________________________
     C*
     C*===============================================================
     C*  CENTER TITLE
     C*===============================================================
     C*
     C     CTITLE        BEGSR
     C*
     C                   MOVEL     ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        ORTITL
     C*
     C                   ENDSR                                                  CTITLE
     C*
     C*===============================================================
     C*  CHANGE THE CLAIM OFFICE # AND PRINT A DETAIL LINE
     C*===============================================================
     C*
     C     CHGOFF        BEGSR
     C*
     C                   MOVEL(P)  OFCURR        CMOFF#
     C*
     C                   MOVEL(P)  Q1PGM         CMPGM
     C                   MOVEL(P)  Q1USER        CMUSER
     C*
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        CMTIME
     C                   Z-ADD     SYSDT         CMDT
     C*
     C                   UPDATE    PCM01
     C*
     C                   ADD       1             XXCNT
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOA
     C                   ENDIF                                                  *INOA=*ON
     C*
     C                   EXCEPT    DET
     C*
     C                   ENDSR                                                  CHGOFF
     C*
     C*===============================================================
     C*  LEFT JUSTIFY TITLE
     C*===============================================================
     C*
     C     LTITLE        BEGSR
     C*
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C*
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C*
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF                                                  N<=60
     C*
     C                   ENDIF                                                  ORTITL<>*BLANKS
     C*
     C                   ENDSR                                                  LTITLE
     C*
     C*===============================================================
     C*  PERFORM A PAGE BREAK BETWEEN GROUPS
     C*===============================================================
     C*
     C     SRFND         BEGSR
     C*
     C     SVFND         IFNE      *ZEROS
     C*
     C     XXCNT         IFEQ      *ZERO
     C                   EXCEPT    MSG
     C                   ENDIF                                                  XXCNT=*ZERO
     C*
     C     XXCNT1        IFEQ      *ZERO
     C                   EXCEPT    MSG1
     C                   ENDIF                                                  XXCNT1=*ZERO
     C*
     C                   ENDIF                                                  SVFND<>*ZEROS
     C*
     C                   Z-ADD     *ZEROS        XXCNT             5 0
     C                   Z-ADD     *ZEROS        XXCNT1
     C*
     C     FNKY          CHAIN     PFN01                              60
     C   60              CLEAR                   FNNAME
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C*
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        CONAME
     C*
     C***** END   ADD **** 04/10/00 ***********************************
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C*
     C                   Z-ADD     XXFND         SVFND
     C*
     C                   ENDSR                                                  SRFND
     C*_______________________________________________________________
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
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
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O***                                13 '(C) COPYRIGHT'
     O***                              +001 'PROGRESSIVE DATA'
     O***                              +001 'SOLUTIONS, INC.,'
     O***                      *YEAR   +001
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ***** END ADD ****** 01/28/10 ***********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              86
     O                       DSDATE             132 '  /  /    '
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       X0TITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            6 'GROUP:'
     O                       XXFND         Z     10
     O                       FNNAME              71
     O          E            HEAD           1
     O                                          122 'OLD'
     O                                          131 'NEW'
     O          E            HEAD           2
     O                                           03 'YR'
     O                                           12 'CASE'
     O                                           27 'CLAIMANT NAME'
     O                                           65 'EMPLOYER'
     O                                           72 'DIV'
     O                                           87 'EMPLOYER NAME'
     O                                          123 'OFFICE'
     O                                          132 'OFFICE'
     O          E            DET            1
     O                       CMFYR         Z     03
     O                       CMCASE        Z     12
     O                       XXCMNM              54
     O                       XXEMP#        Z     65
     O                       XXDIV         Z     72
     O                       EMNAME             114
     O                       OFPREV             123
     O                       CMOFF#             132
     O          E            MSG         2
     O                       NOREC               65
     O*_______________________________________________________________
     O*
     OQSYSPRT1  E            ROUTE1         1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              60
     O**********E            ROUTE1         1
     O**********             ORDIS2              60
     O**********E            ROUTE1         1
     O**********             ORDIS3              60
     O**********E            ROUTE1         2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE1         1
     O                       ORDIS2              60
     O          E            ROUTE1         1
     O                       ORDIS3              60
     O          E            ROUTE1         2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE1         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE1         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE1
     O****                               13 '(C) COPYRIGHT'
     O****                             +001 'PROGRESSIVE DATA'
     O****                             +001 'SOLUTIONS, INC.,'
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
     O****                     *YEAR   +001
      ***** END ADD ****** 01/28/10 ***********************************
     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              86
     O                       DSDATE             132 '  /  /    '
     O          E            HEAD1          2
     O                       Q1PGM               10
     O                       X1TITL              96
     O                                          126 'PAGE:'
     O                       PAGE1         Z    132
     O          E            HEAD1          2
     O                                            6 'GROUP:'
     O                       XXFND         Z     10
     O                       FNNAME              71
     O          E            HEAD1          1
     O                                          122 'EMP'
     O                                          131 'CLM'
     O          E            HEAD1          2
     O                                           03 'YR'
     O                                           12 'CASE'
     O                                           27 'CLAIMANT NAME'
     O                                           65 'EMPLOYER'
     O                                           72 'DIV'
     O                                           87 'EMPLOYER NAME'
     O                                          123 'OFFICE'
     O                                          132 'OFFICE'
     O          E            DET1           1
     O                       CMFYR         Z     03
     O                       CMCASE        Z     12
     O                       XXCMNM              54
     O                       XXEMP#        Z     65
     O                       XXDIV         Z     72
     O                       EMNAME             114
     O                       OFCURR             123
     O                       CMOFF#             132
     O          E            MSG1        2
     O                       NOREC               65
