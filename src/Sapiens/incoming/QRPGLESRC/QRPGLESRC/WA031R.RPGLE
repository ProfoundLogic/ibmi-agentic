     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*                                                               *
     H* API - WA031R - UPDATE WCMS W/ WCMC: CLAIM DEMOGRAHPICS INFO   *     A
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: WA031R                                            *
     H*    CREATED: 09/05/97  C:56                                    *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     h*                                                               *
     h*  05/09/16 C:300 Project: 114WXV                               *
     h*                 Add Cell Number to Claimant Demographics.     *
     H*                                                               *
     H* 10/20/03 C:53  Project: WC909                                 *
     H*                Added country info.                            *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*    61/62 GENERAL USAGE (CHAIN)                                *
     H*                                                               *
     H*****************************************************************
     F*
     FWMCDL     UF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D DBFDS1          DS
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST                                                  WCMS
01   C                   PARM                    @@RTCD            1            -----------
02   C                   PARM                    @@SS#             9            SS#
03   C                   PARM                    @@LSN            30            LSN
04   C                   PARM                    @@FSN            15            FSN
05   C                   PARM                    @@MID             1            MID
06   C                   PARM                    @@ADR1           40            ADR1
07   C                   PARM                    @@ADR2           40            ADR2
08   C                   PARM                    @@CTY            25            CTY
09   C                   PARM                    @@ST              2            ST
10   C                   PARM                    @@ZIP1            5 0          ZIP part 1
11   C                   PARM                    @@ZIP2            4 0          ZIP part 2
12   C                   PARM                    @@AREA            3 0          AREA
13   C                   PARM                    @@PHN1            3 0          PHN# part 1
14   C                   PARM                    @@PHN2            4 0          PHN# part 2
15   C                   PARM                    @@DTOB            7 0          DTOB
16   C                   PARM                    @@SEX             1            SEX
17   C                   PARM                    @@MSTS            1            MSTS
18   C                   PARM                    @@WARA            3 0          WARA
19   C                   PARM                    @@WPH1            3 0          WPH# part 1
20   C                   PARM                    @@WPH2            4 0          WPH# part 1
     C*** BEG ADD *** 10/20/03 ****************************
21   C                   PARM                    @@CTRY            3            Country
22   C                   PARM                    @@ADR3           40            Address
     C*** END ADD *** 10/20/03 ****************************
     C*** BEG ADD *** 05/09/16 ****************************
23   C                   PARM                    @@CARA            3 0
24   C                   PARM                    @@CPN1            3 0
25   C                   PARM                    @@CPN2            4 0
     C*** END ADD *** 05/09/16 ****************************
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
     C*
     C*  CLAIMANT DEMOGRAPICS KEY LIST
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     *LIKE         DEFINE    CDSS#         KKSS#
     C*
     C                   MOVE(P)   @@SS#         KKSS#
     C*
     C** UPDATE CLAMAINT DEMOGRAPHICS FILE
     C*
     C     CDKY          CHAIN     PCD01                              99
     C*
     C     *IN99         IFEQ      '0'
     C*
     C                   MOVEL(P)  @@LSN         CDLSN
     C                   MOVEL(P)  @@FSN         CDFSN
     C                   MOVEL(P)  @@MID         CDMID
     C                   MOVEL(P)  @@ADR1        CDADR1
     C                   MOVEL(P)  @@ADR2        CDADR2
     C                   MOVEL(P)  @@CTY         CDCTY
     C                   MOVEL(P)  @@ST          CDST
     C                   MOVE      *BLANKS       CDZIP
     C                   MOVEL     @@ZIP1        CDZIP
     C                   MOVE      @@ZIP2        CDZIP
     C                   Z-ADD     @@AREA        CDAREA
     C                   MOVE      *BLANKS       CDPHN#
     C                   MOVEL     @@PHN1        CDPHN#
     C                   MOVE      @@PHN2        CDPHN#
     C                   Z-ADD     @@DTOB        CDDTOB
     C                   MOVEL     @@SEX         CDSEX
     C                   MOVEL     @@MSTS        CDMSTS
     C                   Z-ADD     @@WARA        CDWARA
     C                   MOVE      *BLANKS       CDWPH#
     C                   MOVEL     @@WPH1        CDWPH#
     C                   MOVE      @@WPH2        CDWPH#
     C*** BEG ADD *** 10/20/03 ****************************
     C     Q1PRMS        IFGT      20
     C                   MOVEL(P)  @@CTRY        CDCTRY
     C                   MOVEL(P)  @@ADR3        CDADR3
     C                   ENDIF
     C*** END ADD *** 10/20/03 ****************************
     C*** BEG ADD *** 05/09/16 ****************************
     C     Q1PRMS        IFGT      22
     C                   Z-ADD     @@CARA        CDCARA
     C                   MOVE      *BLANKS       CDCPHN
     C                   MOVEL     @@CPN1        CDCPHN
     C                   MOVE      @@CPN2        CDCPHN
     C                   ENDIF
     C*** END ADD *** 05/09/16 ****************************
     C*
     C                   MOVE(P)   Q1USER        CDUSER
     C                   MOVE(P)   Q1PGM         CDPGM
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CDTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CDDT
     C*
     C                   UPDATE    PCD01
     C*
     C                   ELSE
     C*
     C                   MOVEL     'F'           @@RTCD
     C*
     C                   ENDIF                                                  *IN99 IFEQ 0
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
