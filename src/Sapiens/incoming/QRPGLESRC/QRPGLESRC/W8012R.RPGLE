     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*
     H/TITLE W8012R - REPORT - Mass Billing Employer Exception
     H*****************************************************************
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     W8012C - OVERRIDE                                         *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*     NONE                                                      *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*  01/28/10  C:249 Project: SY215
     H*            C:189 Retrieve brand Name
     H*                                                               *
     H*                                                               *
     H*  08/21/08 C:61 PROJECT 114WRC                                 *
     H*                Created.                                       *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  91       Read main File                                      *
     H*  92       Last record processing                              *
     H*  XX - XX  XXX............................XXX                  *
     H*                                                               *
     H*****************************************************************
      *
      * System Company
     FSMCOL     IF   E           K DISK
      *
      * System Output Routing
     FSMORL     IF   E           K DISK
      *
      * User Parameter
     FWMUPL     IF   E           K DISK
      *
      * Billing Work File
     FWABLL     IF   E           K DISK
      *
      * Fund Master File
     FWMFNL     IF   E           K DISK
      *
      *  Group Carrier Information
     fwmgcl     if   e           k disk

      * Employer Master
     fwmeml     if   e           k disk
      *
      *  Employer Policy Year
     fwdell     if   e           k disk
      *
      *  Carrier/State Reporting
     fwmrpl     if   e           k disk
      *
      *  Rating State
     fwmrsl     if   e           k disk
      *
      *  Reporting Rating
     fwdrrl     if   e           k disk
      *
      *
     FQSYSPRT7  o    F  132        PRINTER OFLIND(*INOA)
      *
     D XTITL           C                   CONST('*** MASS BILLING EM-
     D                                     PLOYER EXCEPTION ***')
      *
     D Xcarr           C                   CONST('Carrier Policy number -
     D                                     is not assigned.')
      *
     D Xdbcr           C                   CONST('Debit/Credit factor ex-
     D                                     ceeds limits.')

     d xxdbcr          s             11  5
     d meetss          s              1
     d xxerr           s             40

     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     D PSDS          ESDS                  EXTNAME(SZQ1P)


     c     emky          klist
     c                   kfld                    blco#
     c                   kfld                    blfnd
     c                   kfld                    blemp#
     c                   kfld                    bldiv

     C     FNKY          KLIST
     C                   KFLD                    kkCO#
     C                   KFLD                    kkFND

     c     rrky3         KLIST
     c                   KFLD                    ELCO#
     c                   KFLD                    ELFND
     c                   KFLD                    RSJRST
     c                   KFLD                    ELPFDT

     c     rrky4         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    RSJRST

     c     rsky          klist
     c                   kfld                    elco#
     c                   kfld                    elrtcd

     c     elky          klist
     c                   kfld                    blco#
     c                   kfld                    blfnd
     c                   kfld                    blemp#
     c                   kfld                    bldiv
     c                   kfld                    blfyr

     c     rpky          klist
     c                   kfld                    blco#
     c                   kfld                    blfnd
     c                   kfld                    blemp#
     c                   kfld                    bldiv
     c                   kfld                    blfyr

     c     gcky          klist
     c                   kfld                    blco#
     c                   kfld                    blfnd
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
     C                   KFLD                    KKCO#
      *
      *   BILLING WORK FILE
      *
     c     blky          klist
     c                   kfld                    KKco#
     c                   kfld                    KKfnd
      *
      *
      *=====================================================================
      *   *LIKE Define Variables
      *=====================================================================
      *
     C     *LIKE         DEFINE    BLCO#         KKCO#
     C     *LIKE         DEFINE    BLFND         KKFND
     C     *LIKE         DEFINE    FNNAME        SVFNNM
      *
      *
      *==================================================================
      *  PLIST
      *==================================================================
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@OUTQ           10
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
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND

      * Get Group Name

     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SVFNNM
     C   60              MOVEL(P)  *BLANKS       SVFNNM
      *
      *   Print Routing Page
      *
     C                   EXSR      SRRTE
      *
      *  Read 1st record
      *
     C     BLKY          CHAIN     PBL01                              91
      *
     C     *IN91         DOWEQ     *OFF
      *
      *  If subset criteria met
      *
     C                   EXSR      SUBSET
      *
     C     MEETSS        IFEQ      *ON
      *
      *  Write detail record
      *  zzz
     C                   EXSR      SRDET
     C                   MOVE      *OFF          MEETPR                         PRINTED
     C                   ENDIF                                                  MEETSS
      *
      *  Read next record
      *
     C                   CLEAR                   PBL01
     C     BLKY          READE     PBL01                                  91
     C                   ENDDo                                                  WRK RECORD
      *
      *  Last Record processing
      *
     C     MEETPR        IFEQ      *ON
     C                   SETON                                        OA92
     C                   EXCEPT    HEAD
     C                   EXCEPT    NOREC
     C                   ENDIF                                                  MEETPR
      *
      *  End of Processing
      *
     C     END           TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       kkPrt = '7';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      *ON           *INLR
      *
      *
      *****************************************************************
      *
      *
      *****************************************************************
      * SUBSET - Check if member of records processed subset
      *****************************************************************
     CSR   SUBSET        BEGSR
      *
      *  MEETSS = *OFF, if not to be processed
      *****************************************************************
      *
      *
     c                   eval      meetss = *off
     c                   eval      xxerr  = *blanks

     c     gcky          chain     pgc01

     c                   if        %found and (gccrfl <> 'N')
     c     rpky          chain     prp01                              60

     c                   if        not %found or (RPCAP# = *blanks)
     c                   eval      meetss = *on
     c                   eval      xxerr = xcarr
     c                   endif

     c                   endif
      ****
     c     elky          chain     pel01

     c                   if        %found and (xxerr = *blanks)
     c     rsky          chain     prs01

     c                   if        %found
     C     RRKY3         setgt     PRR01
     C     RRKY4         readpe    PRR01
      *
     c                   if        not %eof and (eldbcr <> 0)
      *
     c                   if        eldbcr > 1.0

     c                   if        (eldbcr > rrdmax) and (rrdmax <> 0)
     c                   eval      meetss = *on
     c                   eval      xxerr = xdbcr
     c                   endif

     c                   endif

     c                   if        eldbcr < 1.0

     c                   if        (eldbcr < rrcmax) and (rrcmax <> 0)
     c                   eval      meetss = *on
     c                   eval      xxerr = xdbcr
     c                   endif

     c                   endif

     c                   endif

     c                   endif

     c                   endif

     c                   ENDSR                                                  SUBSET
      *
      *
      *****************************************************************
      * SRDET - Write Detail Record
      *****************************************************************
      *
     CSR   SRDET         BEGSR

     c     emky          chain     pem01                              60
     c  n60              movel     emname        xxenam           40
     c     elky          chain     pel01                              60
     c                   eval      srdti = elpfdt
     c                   call      'SRCVTDAT'    dtmdyy
     c                   Movel     srdto         xxefdt            8 0
      *
     C   OA              EXCEPT    HEAD
     C   OA              SETOFF                                       OA
     C                   EXCEPT    DETAIL
      *
     C                   ENDSR                                                  SRDET
      *
      *
      *****************************************************************
      * SRRTE - Print routing page
      *****************************************************************
      *
     CSR   SRRTE         BEGSR
      *
      *   GET COMPANY, USER, INFO.
      *
     C     COKY          CHAIN     PCO01                              60
      *
     c*** Beg Add ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     c*** End Add ***** 01/28/10 **************************************
      *
      *   Output Routing - detail report 0
      *
     C                   MOVE      '7'           KKPRT
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
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
      *
     C                   ELSE
      *
     C                   MOVEL(P)  XTITL         ORTITL
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
     C                   SETON                                        OA
      *
     C                   ENDSR                                                  SRRTE
      *
      *
      *
      *****************************************************************
      *
      *
      *
     OQSYSPRT7  E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     o**********                                  8 'XXXXXXXX'
     o**********                                 20 'XXXXXX'
     o**********                                 34 'XX      XX'
     o**********                                 45 'XXXXXXXXXX'
     o**********                                 56 'XXXXXXXXXX'
     o**********                                 67 'XXX     XX'
     o**********                                 77 'XXXXXX'
     o**********E            ROUTE          1
     o**********                                  9 'XXXXXXXXX'
     o**********
     o**********                                 34 'XX      XX'
     o**********                                 45 'XXXXXXXXXX'
     o**********                                 56 'XXXXXXXXXX'
     o**********                                 67 'XXXX    XX'
     o**********                                 78 'XXXXXXXX'
     o**********E            ROUTE          1
     o**********                                 10 'XX      XX'
     o**********                                 22 'XX      XX'
     o**********                                 34 'XX      XX'
     o**********                                 41 'XX'
     o**********                                 52 'XX'
     o**********                                 67 'XXXX    XX'
     o**********                                 79 'XX      XX'
     o**********E            ROUTE          1
     o**********                                 10 'XX      XX'
     o**********                                 22 'XX      XX'
     o**********                                 34 'XX      XX'
     o**********                                 41 'XX'
     o**********                                 52 'XX'
     o**********                                 67 'XX XX   XX'
     o**********                                 71 'XX'
     o**********E            ROUTE          1
     o**********                                  9 'XXXXXXXXX'
     o**********                                 22 'XX      XX'
     o**********                                 34 'XX      XX'
     o**********                                 41 'XX'
     o**********                                 52 'XX'
     o**********                                 67 'XX XX   XX'
     o**********                                 71 'XX'
     o**********E            ROUTE          1
     o**********                                  8 'XXXXXXXX'
     o**********                                 22 'XX      XX'
     o**********                                 34 'XX      XX'
     o**********                                 41 'XX'
     o**********                                 52 'XX'
     o**********                                 67 'XX  XX  XX'
     o**********                                 71 'XX'
     o**********E            ROUTE          1
     o**********                                  4 'XXXX'
     o**********                                 22 'XX      XX'
     o**********                                 34 'XX      XX'
     o**********                                 41 'XX'
     o**********                                 52 'XX'
     o**********                                 67 'XX  XX  XX'
     o**********                                 79 'XX    XXXX'
     o**********E            ROUTE          1
     o**********                                  5 'XXXXX'
     o**********                                 22 'XX      XX'
     o**********                                 34 'XX      XX'
     o**********                                 41 'XX'
     o**********                                 52 'XX'
     o**********                                 67 'XX   XX XX'
     o**********                                 79 'XX    XXXX'
     o**********E            ROUTE          1
     o**********                                  6 'XX XXX'
     o**********                                 22 'XX      XX'
     o**********                                 34 'XX      XX'
     o**********                                 41 'XX'
     o**********                                 52 'XX'
     o**********                                 67 'XX   XX XX'
     o**********                                 79 'XX      XX'
     o**********E            ROUTE          1
     o**********                                  7 'XX  XXX'
     o**********                                 22 'XX      XX'
     o**********                                 34 'XX      XX'
     o**********                                 41 'XX'
     o**********                                 52 'XX'
     o**********                                 67 'XX    XXXX'
     o**********                                 79 'XX      XX'
     o**********E            ROUTE          1
     o**********                                  8 'XX   XXX'
     o**********                                 21 'XXXXXXXX'
     o**********                                 33 'XXXXXXXX'
     o**********                                 41 'XX'
     o**********                                 56 'XXXXXXXXXX'
     o**********                                 67 'XX    XXXX'
     o**********                                 78 'XXXXXXXX'
     o**********E            ROUTE          3
     o**********                                  9 'XX    XXX'
     o**********                                 20 'XXXXXX'
     o**********                                 32 'XXXXXX'
     o**********                                 41 'XX'
     o**********                                 56 'XXXXXXXXXX'
     o**********                                 67 'XX     XXX'
     o**********                                 77 'XXXXXX'
      **********
     o**********E            ROUTE          1
     o**********                                 15 'ROUTE TO:'
     o**********             ORDIS1              56
      **********
     o**********E            ROUTE          1
     o**********             ORDIS2              56
      **********
     o**********E            ROUTE          1
     o**********             ORDIS3              56
      **********
     o**********E            ROUTE          1
     o**********             ORDIS4              56
      **********
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     o          E            ROUTE          1
     o                                           15 'ROUTE TO:'
     o                       ORDIS1              56
      **********
     o          E            ROUTE          1
     o                       ORDIS2              56
      **********
     o          E            ROUTE          1
     o                       ORDIS3              56
      **********
     o          E            ROUTE          1
     o                       ORDIS4              56
      ***** End Add ***** 08/05/15 ************************************
     o          E            ROUTE          2
     o                                           19 'REPORT TITLE:'
     o                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
      **********
     o**********E            ROUTE          2
     o**********                                 19 'PROGRAM NAME:'
     o**********             ORPNAM              30
     o**********                                 36 'FILE:'
     o**********             ORFIL#              38
     o**********                                 46 'COPIES:'
     o**********             ORCOPY        Z     50
     o**********                                 56 'HOLD:'
     o**********             ORHOLD              61
      **********
     o**********E            ROUTE          2
     o**********                                 19 'OUTPUT QUEUE:'
     o**********             OROUTQ              30
     o**********                                 37 'FORMS:'
     o**********             ORFORM              48
     o**********                                 56 'SAVE:'
     o**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      *
     o          E            ROUTE       2  2
     o                                           24 'REPORT SELECTIONS:'
      *
     o          E            ROUTE          2
     o                                           30 'COMPANY . . . . :'
     o                       KKCO#         Z   +001
      *
     o          E            ROUTE          2
     o                                           30 'GROUP . . . . . :'
     o                       KKFND         Z   +001
      *
     o          E            ROUTE          2
     o                                           30 'OUTPUT QUEUE    :'
     o                       @@OUTQ            +001
      *
      *
     o*** Beg Chg *****  01/28/10  ************************************
     o***       E            ROUTE          1 60
     o***                                        25 '(C) COPYRIGHT FISERV'
     o***                                      +000 ', INC.,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     o*** END CHG *****  01/28/10  ************************************
      *
      *
     o          E            HEAD           1 03
     o                       DSTIME               8 '  :  :  '
     o                       CONAME              86
     o                       DSDATE        Y    132
      *
     o          E            HEAD           1
     o                       Q1PGM               10
     o                       ORTITL              96
     o                                          126 'PAGE:'
     o                       PAGE          Z    132
      *
      *
     o          E            HEAD        1  2
     o              N92                        +000 'GROUP:'
     o              N92      kkfnd             +001
     o              N92      SVFNNM            +001
      *
     o          E            HEAD           0
     o                       LINE              +000
      *
     o          E   N92      HEAD           2
     o                                            9 'Employer '
     o                                           +2 'Div  '
     o                                           +2 'Employer Name'
     o                                          +29 'Year'
     o                                           +2 'Effective'
     o                                           +2 'Error Description'
      * zzz
     o          E            DETAIL         1
     o                       blemp#        4      9
     o                       bldiv         4     +2
     o                       xxenam         b    +2
     o                       blfyr         4     +2
     o                       xxefdt         b    +2 '  /  /    '
     o                       xxerr               +2
      *
     o          E            NOREC       2
     o                                           27 '*** No RECORDS MEET THE'
     o                                           50 'SELECTED CRITERIA ***'
      *
      *
