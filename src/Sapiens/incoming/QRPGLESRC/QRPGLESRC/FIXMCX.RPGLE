     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE FIXMCX - FIX - Mass Cancellations
     H*****************************************************************
     H*                                                               *
     H* Program will reinstate all employers from mass cx issue       *
     H* run by entered user on entered date.                          *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* CALLED FROM: Command Line                                     *
     H*                                                               *
     H* CALLS TO:    W7167C - REINSTATEMENT PROCESSING                *
     H*                                                               *
     H* CREATED: 02/02/04  C:61  support  wa045  41112                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*
     H* 01/28/10 C:249 Project: SY215                                 *
     H*          C:189 Retrieve brand Name                            *
     H*                                                               *
     H* 02/02/04 C:61  support wa045  41112                           *
     H*                Copy from c129.test/W7169R and change to       *
     H*                reinstate all employers from a given Mass CX   *
     H*                process (W0164R).                              *
     H*                                                               *
     H* PLEASE ADD TO TOP                                             *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  90       *off= Auto Reinstate,  *on= Don't Auto Reinstate    *
     H*  91       Read primary file                                   *
     H*  92       Read secondary file                                 *
     H*  93       No records found                                    *
     H*  99       Pgm call                                            *
     H*                                                               *
     H*****************************************************************
      *
      * System Company
     FSMCOL     IF   E           K DISK
      *
      * User Parameter
     FSMUPL     IF   E           K DISK
      *
      * System Output Routing
     FSMORL     IF   E           K DISK
      *
      * Cancellation Detail
     FWDCXL13   UF   E           K DISK
      *
      * Employer
     FWMEML     IF   E           K DISK
      *
      * Cancellation History
     FWDXHL     IF A E           K DISK
      *
      * Quote
     FWAQTL7    IF A E           K DISK
      *
      * printed output - Employer Auto Reinstated
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
      *
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
      *
      *
      *================================================================
      * CONSTANTS
      *================================================================
      *
     D XTITL           C                   CONST('FIX Mass Cancel in E-
     D                                     rror Report')
     D*
     D*
      *
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *
      *================================================================
      *   KEY LISTS
      *================================================================
      *
      *   Output Routing
      *

     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT             1
      *
      *   Company
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      *   User Parameter
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *  Cancellation Detail
      *
     C     CXKY          KLIST
     C                   KFLD                    XXDATE
     C                   KFLD                    XXCO#
      *
      *  Cancellation History
      *
     C     XHKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXFYR
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    CXREQ#
      *
      *  Employer
      *
     C     EMKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
      *
      *  Quote
      *
     C     QTKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    XXSTAT
     C                   KFLD                    CXAGT#
      *
      *
      *
      *================================================================
      *   *LIKE Define Variables
      *================================================================
     C     *LIKE         DEFINE    CXCO#         XXCO#
      *
     C     *LIKE         DEFINE    CXCO#         X2CO#
     C     *LIKE         DEFINE    CXFND         X2FND
     C     *LIKE         DEFINE    CXFYR         X2FYR
     C     *LIKE         DEFINE    CXEMP#        X2EMP#
     C     *LIKE         DEFINE    CXCXDT        X2CXDT
      *
      *
      *
      *================================================================
      *   PARAMETER LISTS INCLUDING ENTRY PARMS
      *================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@DATE            8
     C                   PARM                    @@USER           10
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
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
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *   Initialize
      *
     C                   TIME                    DSDTTM
     C                   MOVE      *ALL'_'       LINE            132
     C                   MOVE      *ON           MEETPR            1            PRINTED
      *
      *  PGMSTR executes first
      *
     C                   EXSR      PGMSTR
      *
      *  Print Routing Page
      *
     C                   EXSR      SRRTE
      *
     C                   SETON                                        OA
      *
      * Read CX Detail for all CX issued on entered day
      * zzz
     C     CXKY          CHAIN     PCX01                              92
      *
     C     *IN92         DOWEQ     *OFF
      *
     C     CXUSER        IFEQ      @@USER                                       only user
     C     CXREDT        ANDEQ     *ZERO                                        not reinstated
     C     CXPGM         ANDEQ     'W0164R'                                     only mass CX
     C                   EXSR      SRCK
     C                   ENDIF                                                  CXUSER
      *
     C     CXKY          READE     PCX01                                  92
     C                   ENDDO                                                  *IN92
      *
      *  End of Processing
      *
     C   OA
     CANN93              EXCEPT    HEAD
     C  N93              EXCEPT    NOREC
     C   93              EXCEPT    TOT
      *
     C     END           TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       kkPrt = '0';
       Chain orKy POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      *ON           *INLR
      *
      *
      *
      *================================================================
      * SRCK - Check if CX Detail record meets Auto Reinstate Criteria
      *================================================================
     CSR   SRCK          BEGSR
      *
     C     EMKY          CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      CXCO#         X2CO#
     C                   MOVE      CXFND         X2FND
     C                   MOVE      CXFYR         X2FYR
     C                   MOVE      CXCXDT        X2CXDT
     C                   MOVE      CXEMP#        X2EMP#
      *
      *  Process reinstatement
      *
     C                   EXSR      SRUPD
      *
     C                   Z-ADD     CXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1CXDT            8 0
      *
     C                   Z-ADD     CXCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1CDAT            8 0
      *
     C   OA              EXCEPT    HEAD
     C   OA              SETOFF                                       OA
     C                   EXCEPT    DET
     C                   SETON                                        93
      *
     C                   ENDIF                                                  EMSTRP
      *
     C                   ENDSR                                                  SRCK
      *
      *
      *
      *================================================================
      * SRUPD - Update files - process reinstatement
      *================================================================
     CSR   SRUPD         BEGSR
      *
      * Update Cancellation Detail with Reinstatement info
      *
     C                   Z-ADD     CXCXDT        CXREDT
     C                   MOVE      'Y'           CXREIN
     C                   MOVE      'N'           CXRCRT                         GEN CERT LETTERS
     C                   MOVE      *BLANK        CXRDRT                         DART CODE
     C                   MOVE      'Y'           CXERR
      *
     C                   MOVEL(P)  *BLANKS       CXRCM1
     C                   MOVEL(P)  *BLANKS       CXRCM2
     C                   Z-ADD     0             CXCFDT
      *
     C                   Z-ADD     SYSDAT        CXRDAT
      *
     C                   MOVE      Q1USER        CXUSER
     C                   MOVE      Q1PGM         CXPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     SYSDAT        CXDT
     C                   Z-ADD     DSTIME        CXTIME
      *
     C                   UPDATE    PCX01
      *
      *===============================================================
      *
      *  Write Cancellation History record
      *
     C     XHKY          CHAIN     PXH01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *BLANKS       XHSTAT
     C                   MOVE      Q1USER        XHUSER
     C                   MOVE      Q1PGM         XHPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     SYSDAT        XHDT
     C                   Z-ADD     DSTIME        XHTIME
     C                   MOVE      *BLANK        XHRVFL
     C                   MOVEL     CXRSN         XHRSN
     C                   Z-ADD     CXCXDT        XHCXDT
     C                   Z-ADD     CXCDAT        XHCDAT
     C                   Z-ADD     CXREDT        XHREDT
     C                   Z-ADD     CXRDAT        XHRDAT
     C                   MOVE      CXERR         XHERR
     C                   Z-ADD     SYSDAT        XHTRDT
     C                   WRITE     PXH01
     C                   ENDIF                                                   *IN60=*OFF
      *
      *===============================================================
      *
      *  Write an 'R' type quote record if the reinstatement of the
      *  most current (by week end date) quote record is a cancellation
      *  (type 'C').
      *
     C     QTKY          SETGT     PQT01                                        CO/FND/EMP/DIV/ST/AG
     C     QTKY          READPE    PQT01                                  60
      *
     C     *IN60         IFEQ      '0'
     C     QTTYPE        ANDEQ     'C'                                          CANCELLATION
     C                   MOVE      'R'           QTTYPE
     C                   TIME                    DSDTTM
     C                   Z-ADD     SYSDAT        QTDT
     C                   Z-ADD     DSTIME        QTTIME
     C                   MOVE      Q1USER        QTUSER
     C                   MOVE      Q1PGM         QTPGM
     C                   WRITE     PQT01
     C                   ENDIF                                                  QTTYPE = C
      *
      *
      *===============================================================
      *
      *  Submit reinstatement print program
      *
     C                   MOVE      CXCO#         ##CO#             3
     C                   MOVE      CXFND         ##FND             3
     C                   MOVE      CXFYR         ##FYR             3
     C                   MOVE      CXEMP#        ##EMP#            9
     C                   MOVE      CXDIV         ##DIV             5
     C                   MOVE      CXREQ#        ##REQ#            5
     C                   MOVEL(P)  OROUTQ        ##OUTQ           10
      *
     C                   CALL      'W7167C'                             99
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##EMP#            9
     C                   PARM                    ##DIV             5
     C                   PARM                    ##REQ#            5
     C                   PARM      'N'           ##RCRT            1            PRT CERT LETTR?
     C                   PARM      'N'           ##REPR            1            REPRINT? (Y/N)
     C                   PARM                    ##OUTQ           10
     C                   PARM      'R'           ##TYPE            1            LABEL R / L?
      *
     C                   ADD       1             XXCNT
      *
     C                   ENDSR                                                  SRUPD
      *
      *
      *
      *================================================================
      * PGMSTR - Set-up and Initialize
      *================================================================
     CSR   PGMSTR        BEGSR
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDAT            7 0
      *
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@DATE        X1DATE            8 0
      *
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
      *
     C                   SETOFF                                       93
      *
     C                   MOVE      'W'           XXSTAT            1            ACTIVE ENVIRONMENT
      *
     C                   Z-ADD     *ZERO         XXCNT             9 0
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *================================================================
      * SRRTE - Print routing page
      *================================================================
     CSR   SRRTE         BEGSR
      *
      *   GET COMPANY, USER, INFO.
      *
     C     COKY          CHAIN     PCO01                              60
     C     UPKY          CHAIN     PUP01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      *
      *   Output Routing - detail report 0
      *
     C                   MOVE      '0'           KKPRT
     C     ORKY          CHAIN     POR01                              60
      *
     C     *IN60         IFEQ      '0'
      *
      * Left Justify Title in Field
      *
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF
     C                   ENDIF
      *
      *  User Name if no Distribution specified in routing
      *
     C     ORDIS1        IFEQ      *BLANKS
     C     ORDIS2        ANDEQ     *BLANKS
     C     ORDIS3        ANDEQ     *BLANKS
     C     ORDIS4        ANDEQ     *BLANKS
     C                   MOVEL(P)  UPUSNM        ORDIS1
     C                   ENDIF
      *
      ***** Beg Del ***** 08/26/14 *********************************************
     C********           EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
      *
     C                   ELSE
      *
     C                   MOVEL(P)  XTITL         ORTITL
     C                   MOVEL(P)  UPOUTQ        OROUTQ
      *
     C                   ENDIF
      *
      *  Center Title in Field
      *
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        ORTITL
      *
      * Center Company name in display field
      *
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        CONAME
      *
     C                   ENDSR                                                  SRRTE
      *
      *
      *
      *================================================================
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
      *
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              56
      *
     O**********E            ROUTE          1
     O**********             ORDIS2              56
      **********
     O**********E            ROUTE          1
     O**********             ORDIS3              56
      *
     O**********E            ROUTE          1
     O**********             ORDIS4              56
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              56
      *
     O          E            ROUTE          1
     O                       ORDIS2              56
      **********
     O          E            ROUTE          1
     O                       ORDIS3              56
      *
     O          E            ROUTE          1
     O                       ORDIS4              56
      ***** End Add ***** 08/05/15 ************************************
      *
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      *
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
      *
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      *
     O          E            ROUTE       2  2
     O                                           24 'REPORT SELECTIONS:'
      *
     O          E            ROUTE          2
     O                                           26 'COMPANY . . . . :'
     O                       XXCO#         Z   +001
      *
     O          E            ROUTE          2
     O                                           26 'DATE  . . . . . :'
     O                       X1DATE            +001 '  /  /    '
      *
     O          E            ROUTE          2
     O                                           26 'USER  . . . . . :'
     O                       @@USER            +001
      *
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                25 '(C) Copyright Progressi'
     O***                              +000 've Data Solutions. Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      *
      *
      *
     O          E            HEAD           1 03
     O                       DSTIME               8 '  :  :  '
     O                       CONAME              80
     O                       DSDATE        Y    132
      *
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              90
     O                                          125 'PAGE:'
     O                       PAGE          Z    132
      *
     O          E            HEAD           1
     O                                           26 'COMPANY:'
     O                       XXCO#         Z   +001
      *
     O          E            HEAD           1
     O                                           26 'DATE   :'
     O                       X1DATE            +001 '  /  /    '
      *
     O          E            HEAD           2
     O                                           26 'USER   :'
     O                       @@USER            +001
      *
     O          E            HEAD        1  1
     O                                         +000 'Rating'
     O                                         +002 'Cancel'
     O                                         +032 'Policy'
     O                                         +004 'Cancel'
     O                                         +007 'Issue'
      *
     O          E            HEAD        0  0
     O                       LINE              +000
      *
     O          E            HEAD           2
     O                                         +000 'State'
     O                                         +003 'Reason'
     O                                         +002 'Group'
     O                                         +004 'Employer'
     O                                         +002 'Division'
     O                                         +004 'Year'
     O                                         +006 'Date'
     O                                         +009 'Date'
      *
     O          E            DET            1
     O                       CXRST             +002
     O                       CXRSN             +006
     O                       CXFND         4   +005
     O                       CXEMP#        4   +005
     O                       CXDIV         4   +002
     O                       CXFYR         4   +006
     O                       X1CXDT        Y   +005
     O                       X1CDAT        Y   +003
     O                       EMNAME            +004
      *
      *
     O          E            TOT         1  1
     O                                         +000 'Count:'
     O                       XXCNT         4   +001
      *
     O          E            NOREC       2
     O                                           27 '*** NO RECORDS MEET THE'
     O                                           50 'SELECTED CRITERIA ***'
      *
      *
