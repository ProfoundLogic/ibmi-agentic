     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* TITLE CS700R - DAILY TRANSACTION SUMMARY REPORT - PRT PGM     *
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PRINTS ALL CLAIM SCAN SEARCH FOR THE DAY   **
     H**     BY USER, THEN CLIENT (SEARCH MASTER NUMBER)             **
     H**     INFORMATION PRINTED INCLUDES SEARCH MASTER NUMBER,      **
     H**       SEARCH MASTER NAME, TOTAL # OF SEARCHES BY MAIL,      **
     H**       PHONE, AND FAX                                        **
     H**   CALLED FROM:                                              **
     H**     CS700C - DAILY TRANSACTION SUMMARY REPORT OVERRIDE      **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: LORRI DAPPER                                  **
     H**   CREATED:   11/12/90                                       **
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 11/11/97   C:  86    PROJECT:  CS010
     H*     CHANGED DATE ROUTINE AND FIELDS FOR 8 DIGITS
     H* 12/20/90 LJD   PER TLB:
     H*                1.  CHANGED CLIENT FILE TO CMCTP/CMCTL1
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       OF     OVERFLOW FOR PRINTING FOR REPORT                 *
     H*                                                               *
     H*       50     READE/DOWEQ OF OPEN QUERY CLIENT SUMMARY WORKFILE*
     H*       60     GENERAL USAGE (CHAIN)                            *
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**    CWCLL1(PCL01): CLIDT/CLIUSR/CLCLT#                       **
     H**    CMCTL1(PCT01): CTCLT#                                    **
     H*****************************************************************
     FCWCLL1    IF   E           K DISK
     FCMCTL1    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     CLKEY1        KLIST
     C                   KFLD                    XXFRDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
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
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *LIKE         DEFINE    CLIDT         XXFRDT
     C     *LIKE         DEFINE    CLIDT         XXTODT
     C     *LIKE         DEFINE    CLIUSR        XXIUSR
     C     *LIKE         DEFINE    CLCLT#        XXCLT#
     C     *LIKE         DEFINE    CTNAME        XXNAME
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQFRDT        RSFRDT            8 0
     C                   MOVE      QQTODT        RSTODT            8 0
     C*
     C                   Z-ADD     RSFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     RSFRDT        SRDTI
     H******* BEGIN CHANGE ************ 11/11/97 **********************
     C*                    CALL 'SRCVTDAT'DTMDY1
     C*                    MOVELSRDTO     X1FRDT  60
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         X1FRDT            8 0
     H******* END   CHANGE ************ 11/11/97 **********************
     C*
     C                   Z-ADD     RSTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   Z-ADD     RSTODT        SRDTI
     H******* BEGIN CHANGE ************ 11/11/97 **********************
     C*                    CALL 'SRCVTDAT'DTMDY1
     C*                    MOVELSRDTO     X1TODT  60
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         X1TODT            8 0
     H******* END   CHANGE ************ 11/11/97 **********************
     C*
     C*** BEG ADD *** 01/28/10 ****************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C****END ADD *** 01/28/10 ****************************************
      ***** Beg Del ***** 08/26/14 ****************************************
     C***********        EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ****************************************
     C                   EXCEPT    HEAD
     C*
     C*** BEGIN READING OPEN QUERY ON CLIENT SUMMARY WORKFILE
     C     CLKEY1        SETLL     PCL01
     C                   READ      PCL01                                  50
     C*
     C*** IF NO RECORDS SELECTED, PRINT ERROR MESSAGE AND END PROGRAM
     C     *IN50         IFEQ      '1'
     C                   EXCEPT    ERRMSG
     C                   GOTO      TERM
     C                   END
     C*
     C*** ESTABLISH FIRST USER HEADER INFORMATION
     C                   MOVE      CLIUSR        XXIUSR
     C                   EXCEPT    HEAD1
     C*** ESTABLISH FIRST CLIENT INFORMATION
     C                   Z-ADD     CLCLT#        XXCLT#
     C                   EXSR      SRCLTI                                       CLIENT INFO
     C*
     C*** INITIAL USER TOTALS
     C                   Z-ADD     *ZEROS        XXUSRM
     C                   Z-ADD     *ZEROS        XXUSRP
     C                   Z-ADD     *ZEROS        XXUSRF
     C*
     C     *IN50         DOWEQ     '0'
     C     CLIDT         ANDLE     XXTODT
     C*
     C*** PERFORM TOTALS ON USER, IF USER BREAK
     C     CLIUSR        IFNE      XXIUSR
     C                   EXSR      SRCLTT                                       CLIENT TOTAL
     C                   EXSR      SRUSRT                                       USER TOTAL
     C                   EXSR      SRCLTI                                       CLIENT INFO
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    HEAD1
     C                   END
     C*
     C*** PERFORM TOTALS ON CLIENT, IF CLIENT BREAK
     C     CLCLT#        IFNE      XXCLT#
     C                   EXSR      SRCLTT                                       CLIENT TOTAL
     C                   EXSR      SRCLTI                                       CLIENT INFO
     C                   END
     C*
     C*** ACCUMULATE CLIENT TOTALS
     C     CLMTF         IFEQ      'M'
     C                   ADD       1             XXCLTM
     C                   ELSE                                                   ELSE EQ 'M'
     C     CLMTF         IFEQ      'T'
     C                   ADD       1             XXCLTP
     C                   ELSE                                                   ELSE EQ 'P'
     C     CLMTF         IFEQ      'F'
     C                   ADD       1             XXCLTF
     C                   END                                                    END EQ 'F'
     C                   END                                                    END EQ 'P'
     C                   END                                                    END EQ 'M'
     C                   ADD       1             XXCLTT
     C*
     C                   READ      PCL01                                  50
     C                   END                                                    END DOWEQ 50
     C*
     C*** ALL RECORDS READ, PRINT REPORT TOTALS
     C                   EXSR      SRCLTT
     C                   EXSR      SRUSRT
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    RPTT
     C*
     C*** END PROGRAM
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       Except Route;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*************************************************************************
     C* SRCLTI - CLIENT INFO                                                  *
     C*************************************************************************
     C     SRCLTI        BEGSR
     C*
     C*** INITIAL CLIENT TOTALS
     C                   Z-ADD     *ZEROS        XXCLTM            6 0          CLIENT MAIL SRCHS
     C                   Z-ADD     *ZEROS        XXCLTP            6 0          CLIENT PHONE SRCHS
     C                   Z-ADD     *ZEROS        XXCLTF            6 0          CLIENT FAX SRCHS
     C                   Z-ADD     *ZEROS        XXCLTT            7 0          TOTAL CLIENT SRCHS
     C*
     C*** GET CLIENT NAME
     C                   MOVE      *BLANKS       XXNAME
     C     CLCLT#        CHAIN     PCT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CTNAME        XXNAME
     C                   END
     C*
     C                   ENDSR
     C*************************************************************************
     C* SRCLTT - CLIENT TOTAL                                                 *
     C*************************************************************************
     C     SRCLTT        BEGSR
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   EXCEPT    CLTT
     C*
     C*** ROLL TOTALS INTO USER TOTALS
     C                   ADD       XXCLTM        XXUSRM
     C                   ADD       XXCLTP        XXUSRP
     C                   ADD       XXCLTF        XXUSRF
     C                   ADD       XXCLTT        XXUSRT
     C*
     C                   Z-ADD     CLCLT#        XXCLT#
     C*
     C                   ENDSR
     C*************************************************************************
     C* SRUSRT - USER TOTAL                                                   *
     C*************************************************************************
     C     SRUSRT        BEGSR
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    USRT
     C*
     C*** ROLL USER TOTALS INTO REPORT TOTALS
     C                   ADD       XXUSRM        XXRPTM            8 0          REPORT MAIL SRCHS
     C                   ADD       XXUSRP        XXRPTP            8 0          REPORT PHONE SRCHS
     C                   ADD       XXUSRF        XXRPTF            8 0          REPORT FAX SRCHS
     C                   ADD       XXUSRT        XXRPTT            9 0          TOTAL REPORT SRCHS
     C*
     C*** INITIAL USER TOTALS
     C                   Z-ADD     *ZEROS        XXUSRM            7 0          USER MAIL SRCHS
     C                   Z-ADD     *ZEROS        XXUSRP            7 0          USER PHONE SRCHS
     C                   Z-ADD     *ZEROS        XXUSRF            7 0          USER FAX SRCHS
     C                   Z-ADD     *ZEROS        XXUSRT            8 0          TOTAL USER SRCHS
     C*
     C                   MOVE      CLIUSR        XXIUSR
     C*
     C                   ENDSR
     O*************************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ********************************************
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
     O**********E            ROUTE          2
     O**********                                 15 'ROUTE TO:'
     O**********             Q1USER              30
      ***** End Del ***** 08/26/14 ********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           15 'ROUTE TO:'
     O                       Q1USER              30
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                                           54 'CLAIM SCAN TRANSAC'
     O                                           65 'TION REPORT'
      ***** Beg Del ***** 08/26/14 ********************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             Q1PGM               30
      ***** End Del ***** 08/26/14 ********************************************
     O          E            ROUTE          1
     O                                           19 'SELECTION:'
     O          E            ROUTE          1
     O                                           25 'FROM DATE:'
     O******* BEGIN CHANGE ***************** 11/11/97 ***************
     O*                        X1FRDT    35 '  /  /  '
     O                       X1FRDT              35 '  /  /    '
     O******* END   CHANGE ***************** 11/11/97 ***************
     O          E            ROUTE          2
     O                                           25 'TO DATE:'
     O******* BEGIN CHANGE ***************** 11/11/97 ***************
     O*                        X1TODT    35 '  /  /  '
     O                       X1TODT              35 '  /  /    '
     O******* END   CHANGE ***************** 11/11/97 ***************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                                  '(C) COPYRIGHT PROGRESSIV'
     O****                             +  0 'E DATA SOLUTIONS, INC.'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O                                         +000 '(C)'
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSDATE        Y      8
     O                                           47 'CLAIM SCAN'
     O                       DSTIME              85 '  :  :  '
     O          E            HEAD           2
     O                                            6 'CS700R'
     O                                           40 'TRANSACTION'
     O                                           55 'SUMMARY REPORT'
     O                                           80 'PAGE:'
     O                       PAGE                85
     O          E            HEAD           2
     O                                            5 'SRCH#'
     O                                           16 'SRCH NAME'
     O                                           53 '# MAIL'
     O                                           64 '# PHONE'
     O                                           73 '# FAX'
     O                                           84 'TOTAL'
     O          E            HEAD1          1
     O                                            8 'USER ID:'
     O                       XXIUSR              20
     O          E            CLTT        1  1
     O                       XXCLT#        Z      5
     O                       XXNAME              48
     O                       XXCLTM        Z     55
     O                       XXCLTP        Z     65
     O                       XXCLTF        Z     75
     O                       XXCLTT        Z     86
     O          E            USRT        1  1
     O                                           24 'USER'
     O                       XXIUSR              35
     O                                           43 'TOTALS:'
     O                       XXUSRM        Z     55
     O                       XXUSRP        Z     65
     O                       XXUSRF        Z     75
     O                       XXUSRT        Z     86
     O          E            RPTT        1  1
     O                                           43 'REPORT TOTALS:'
     O                       XXRPTM        Z     55
     O                       XXRPTP        Z     65
     O                       XXRPTF        Z     75
     O                       XXRPTT        Z     86
     O*
     O**** ERROR MESSAGE, IF NO RECORDS TO BE PROCESSED *************
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
