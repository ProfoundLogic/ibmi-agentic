     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE W0578R1 - Address History Report
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL PRINT THE ADDRESS HISTORY    *
     H*                REPORT.                                        *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W6000C                                                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*                                                               *
     H*  CREATED: 06/29/07  PROGRAMMER: C:183 PROJECT: 114WOC         *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*                  Retrieve brand Name                          *
     H*                                                               *
     H*****************************************************************
     H*
     FWDAHL1    IF   E           K DISK
      *
     FWDAHL3    IF   E           K DISK    PREFIX('X_')
     F                                     RENAME(PAH01:PAH02)
      *
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
      *
     FW0578P1   O    E             PRINTER
      *
     FQSYSPRT2  O    F  105        PRINTER OFLIND(*INOB)
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D@XXCLAIM         S             13
     DQQOUTQ           S             10
     DQQMODE           S              1
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    @CO#              3
     C                   PARM                    @DATE             8
      *
     C     COKEY         KLIST
     C                   KFLD                    XXCO#             3 0
      *
     C     BTCHK         KLIST
     C                   KFLD                    CO#
     C                   KFLD                    BTCHDT
      *
     C     AHKY          KLIST
     C                   KFLD                    AHCO#
     C                   KFLD                    AHFND
     C                   KFLD                    AHFYR
     C                   KFLD                    AHCASE
     C                   KFLD                    AHDT
     C                   KFLD                    AHTIME
     C                   KFLD                    @AHBAFL           1
      *
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
      *
      *   KEY LIST FOR THE USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDYY'       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C                   MOVE      @CO#          CO#               3 0
     C                   EVAL      XXCO# = CO#
     C                   MOVE      @DATE         @@DATE            8 0
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     C*** END ADD ***** 01/28/10 **************************************
      *
     C                   EXSR      ROUTING
      *
     C                   MOVE      @DATE         @@DATE            8 0
     C                   Z-ADD     @@DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BTCHDT            7 0
      *
     C     BTCHK         CHAIN     WDAHL1                             31
      *
     C                   DOW       *IN31 = *OFF
      *
     C                   MOVE      'N'           @NWRCFL           1
      *
     C                   IF        AHBAFL = 'A'
      *
     C                   WRITE     HEAD
      *
     C                   EVAL      @XXCLAIM = %EDITC(AHFND:'X') +
     C                             %EDITC(AHFYR:'X') + %EDITC(AHCASE:'X')
     C                   MOVE      @XXCLAIM      XXCLAIM
      *
     C                   EVAL      XXUSER = AHUSER
      *
     C                   Z-ADD     AHDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDT
      *
     C                   EVAL      XXTIME = AHTIME
      *
     C                   WRITE     MAINHEAD
     C                   WRITE     SUBHEAD
      *
     C                   IF        AHFSN = *BLANKS
     C                   EVAL      XXANAME = %trim(AHLSN)
     C                   ELSE
     C                   EVAL      XXANAME = %trim(AHLSN) + ', ' + %trim(AHFSN)
     C                             + ' ' + %trim(AHMID)
     C                   ENDIF
      *
     C                   EVAL      XXAADR1 = AHADR1
     C                   EVAL      XXAADR2 = AHADR2
     C                   EVAL      XXACTY = AHCTY
     C                   EVAL      XXAST = AHST
     C                   EVAL      XXAZIP = AHZIP
     C                   EVAL      XXACKAL = AHCKAL
     C                   EVAL      XXACTRY = AHCTRY
     C                   EVAL      XXAADR3 = AHADR3
      *
     C                   EVAL      @AHBAFL = 'B'
     C     AHKY          CHAIN     WDAHL3                             32
      *
     C                   IF        *IN32 = *OFF
      *
     C                   IF        AHFSN = *BLANKS
     C                   EVAL      XXBNAME = %trim(X_AHLSN)
     C                   ELSE
     C                   EVAL      XXBNAME = %trim(X_AHLSN) + ', ' +
     C                             %trim(X_AHFSN) + ' ' + %trim(X_AHMID)
     C                   ENDIF
      *
     C                   EVAL      XXBADR1 = X_AHADR1
     C                   EVAL      XXBADR2 = X_AHADR2
     C                   EVAL      XXBCTY = X_AHCTY
     C                   EVAL      XXBST = X_AHST
     C                   EVAL      XXBZIP = X_AHZIP
     C                   EVAL      XXBCKAL = X_AHCKAL
     C                   EVAL      XXBCTRY = X_AHCTRY
     C                   EVAL      XXBADR3 = X_AHADR3
     C                   ENDIF
      *
     C                   EXSR      CHGPRNT
      *
     C                   WRITE     DETAIL
      *
     C                   ELSEIF    AHBAFL = 'B'
      *
     C                   EVAL      @AHBAFL = 'A'
     C     AHKY          CHAIN     WDAHL3                             32
      *
     C                   IF        *IN32 = *ON
      *
     C                   WRITE     HEAD
      *
     C                   EVAL      @XXCLAIM = %EDITC(AHFND:'X') +
     C                             %EDITC(AHFYR:'X') + %EDITC(AHCASE:'X')
     C                   MOVE      @XXCLAIM      XXCLAIM
      *
     C                   EVAL      XXUSER = AHUSER
      *
     C                   Z-ADD     AHDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDT
      *
     C                   EVAL      XXTIME = AHTIME
      *
     C                   WRITE     MAINHEAD
     C                   WRITE     SUBHEAD
      *
     C                   IF        AHFSN = *BLANKS
     C                   EVAL      XXBNAME = %TRIM(AHLSN)
     C                   ELSE
     C                   EVAL      XXBNAME = %TRIM(AHLSN) + ', ' + %TRIM(AHFSN)
     C                             + ' ' + %TRIM(AHMID)
     C                   ENDIF
      *
     C                   EVAL      XXBADR1 = AHADR1
     C                   EVAL      XXBADR2 = AHADR2
     C                   EVAL      XXBCTY = AHCTY
     C                   EVAL      XXBST = AHST
     C                   EVAL      XXBZIP = AHZIP
     C                   EVAL      XXBCKAL = AHCKAL
     C                   EVAL      XXBCTRY = AHCTRY
     C                   EVAL      XXBADR3 = AHADR3
      *
     C                   EVAL      XXANAME = 'Record Added'
     C                   EVAL      XXAADR1 = *BLANKS
     C                   EVAL      XXAADR2 = *BLANKS
     C                   EVAL      XXACTY = *BLANKS
     C                   EVAL      XXAST = *BLANKS
     C                   EVAL      XXAZIP = *ZEROS
     C                   EVAL      XXACKAL = *BLANKS
     C                   EVAL      XXACTRY = *BLANKS
     C                   EVAL      XXAADR3 = *BLANKS
      *
     C                   EVAL      @NWRCFL = 'Y'
      *
     C                   EXSR      CHGPRNT
      *
     C                   WRITE     DETAIL
     C                   ENDIF
     C                   ENDIF
      *
     C     BTCHK         READE     WDAHL1                                 31
     C                   ENDDO
      *
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       write Route;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   EVAL      *INLR = *ON
      *
      *********************************************************************************************
      * CHGPRNT - Print Change fields
      *********************************************************************************************
      *
     C     CHGPRNT       BEGSR
     C                   EVAL      XXCNAME = '   '
     C                   EVAL      XXCADR1 = '   '
     C                   EVAL      XXCADR2 = '   '
     C                   EVAL      XXCCTY = '   '
     C                   EVAL      XXCST   = '   '
     C                   EVAL      XXCZIP  = '   '
     C                   EVAL      XXCCKAL = '   '
     C                   EVAL      XXCCTRY = '   '
     C                   EVAL      XXCADR3 = '   '
      *
     C                   IF        @NWRCFL = 'N'
      *
     C                   IF        XXBNAME <> XXANAME
     C                   EVAL      XXCNAME = '***'
     C                   ENDIF
      *
     C                   IF        XXBADR1 <> XXAADR1
     C                   EVAL      XXCADR1 = '***'
     C                   ENDIF
      *
     C                   IF        XXBADR2 <> XXAADR2
     C                   EVAL      XXCADR2 = '***'
     C                   ENDIF
      *
     C                   IF        XXBCTY <> XXACTY
     C                   EVAL      XXCCTY = '***'
     C                   ENDIF
      *
     C                   IF        XXBST <> XXAST
     C                   EVAL      XXCST = '***'
     C                   ENDIF
      *
     C                   IF        XXBZIP <> XXAZIP
     C                   EVAL      XXCZIP = '***'
     C                   ENDIF
      *
     C                   IF        XXBCKAL <> XXACKAL
     C                   EVAL      XXCCKAL = '***'
     C                   ENDIF
      *
     C                   IF        XXBCTRY <> XXACTRY
     C                   EVAL      XXCCTRY = '***'
     C                   ENDIF
      *
     C                   IF        XXBADR3 <> XXAADR3
     C                   EVAL      XXCADR3 = '***'
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDSR
      *
     C     ROUTING       BEGSR
      *
     C*   GET THE DISTRIBUTION INFORMATION
     C                   EVAL      QQMODE = 'B'
      *
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   ENDIF                                                  QQMODE IFEQ
      *
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C                   EVAL      QQOUTQ = OROUTQ
      *
     C*   CHAIN TO THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
      *
     C                   EVAL      RSCO# = XXCO#
     C                   EVAL      RSDATE = @@DATE
      *
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         WRITE     ROUTE
      ***** End Del ***** 08/26/14 ************************************
      *
     C                   ENDSR
      *
     OQSYSPRT2  E            ROUTE2         1 06
     O                                            8 'XXXXXXXX'
     O                                           20 'XXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXX     XX'
     O                                           77 'XXXXXX'
     O          E            ROUTE2         1
     O                                            9 'XXXXXXXXX'
     O                                           21 'XXXXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXXX    XX'
     O                                           78 'XXXXXXXX'
     O          E            ROUTE2         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XXXX    XX'
     O                                           79 'XX      XX'
     O          E            ROUTE2         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            ROUTE2         1
     O                                            9 'XXXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            ROUTE2         1
     O                                            8 'XXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           71 'XX'
     O          E            ROUTE2         1
     O                                            4 'XXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           79 'XX    XXXX'
     O          E            ROUTE2         1
     O                                            5 'XXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX    XXXX'
     O          E            ROUTE2         1
     O                                            6 'XX XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX      XX'
     O          E            ROUTE2         1
     O                                            7 'XX  XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX    XXXX'
     O                                           79 'XX      XX'
     O          E            ROUTE2         1
     O                                            8 'XX   XXX'
     O                                           21 'XXXXXXXX'
     O                                           33 'XXXXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX    XXXX'
     O                                           78 'XXXXXXXX'
     O          E            ROUTE2         3
     O                                            9 'XX    XXX'
     O                                           20 'XXXXXX'
     O                                           32 'XXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX     XXX'
     O                                           77 'XXXXXX'
     O          E            ROUTE2         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE2         1
     O               08      ORDIS2              60
     O          E            ROUTE2         1
     O               08      ORDIS3              60
     O          E            ROUTE2         2
     O               08      ORDIS4              60
     O          E            ROUTE2         2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O          E            ROUTE2         2
     O                                           19 'PROGRAM NAME:'
     O                       ORPNAM              30
     O                                           36 'FILE:'
     O                       ORFIL#              38
     O                                           46 'COPIES:'
     O                       ORCOPY        Z     50
     O                                           56 'HOLD:'
     O                       ORHOLD              61
     O          E            ROUTE2         2
     O                                           19 'OUTPUT QUEUE:'
     O               08      OROUTQ              30
     O                  09   QQOUTQ              30
     O                                           37 'FORMS:'
     O                       ORFORM              48
     O                                           56 'SAVE:'
     O                       ORSAVE              61
     O*** BEG CHG *****  01/28/10  ************************************
     O****      E            ROUTE2         1 60
     O***                                        28 '(C) COPYRIGHT FISERV,'
     O***                                        39 ' INC., 2005'
     O          e            route2         1 59
     O                       #brnd1
     O          e            route2         1
     O                       #brnd2
     O*** END CHG *****  01/28/10  ************************************
