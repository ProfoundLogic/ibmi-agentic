     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** PROGRAM:   F7087R - PRINT REFUND REQUEST                    **
     H*****************************************************************
     H** DESCRIPTION:  PRINT PROGRAM - REFUND REQUEST                **
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    F0087R - RECON BREAKDOWN                                   *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  PROGRAMMER: C:20               REQ/PROJ: 12/60MAL            *
     H*  CREATED: 01/24/95                                            *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/09/20 C:295 Project: SY420                                 *
     H*                -Mask SSN                                      *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*                  Retrieve brand Name                          *
     H*                                                               *
     H*****************************************************************
     FFAERL     IF   E           K DISK
     FFAERL2    IF   E           K DISK
     F                                     RENAME(PER01:PER02)
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMC1L     IF   E           K DISK
     FFAEOL4    IF   E           K DISK
     FFDEOL160  IF   E           K DISK
     F                                     RENAME(PEO01:PDE01)
     F*
     FQSYSPRT   O    F  127        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)
     F*

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
     D XXSSN           S              9
      ***** End Add *** 06/09/20 *******************************************
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D*
     D                 DS
     D  XSZIP                  1      9  0
     D  S1ZPP1                 1      5  0
     D  S1ZPP2                 6      9  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XCNM                   1     40
     D                                     DIM(40) ASCEND
     D  XSNAME                 1     40
     D                 DS
     D  FNNM                   1     60
     D                                     DIM(60) ASCEND
     D  FNNAME                 1     60
     D                 DS
     D  XCPHN#                 1     14
     D  XXAREA                 2      4  0
     D  XXCPRN                 5      5
     D  XXPH1                  7      9  0
     D  XXHP1                 10     10
     D  XXPH2                 11     14  0
     D                 DS
     D  XF#                    1     20
     D  XXSUB#                 1      5  0
     D  XFHYP1                 6      6
     D  XXFND                  7      9  0
     D  XFHYP2                10     10
     D  XXYR                  11     12  0
     D  XFHYP3                13     13
     D  XXCASE                14     20  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D*
     D*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C* FUND MASTER
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C* CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C* USER PARMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* OUTPUT ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C* KEY LIST FOR THE WCMS CLAIM MASTER FILE
     C*
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRFND            3 0
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C* KEY LIST FOR THE REFUND REQUEST TRANSACTION FILE
     C*
     C     ERKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXREOB
     C*
     C* KEY LIST FOR THE EOMB TRANSACTION FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    EROEOB
     C*
     C     EOKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    ERREOB
     C*
     C* KEY LIST FOR THE COMPANY AUXILIARY FILE
     C*
     C     C1KEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQREOB            9
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* INITIALIZATION SECTION
     C*
     C     *LIKE         DEFINE    ERCO#         XXCO#
     C     *LIKE         DEFINE    ERFYR         XXFYR
     C     *LIKE         DEFINE    ERREOB        XXREOB
     C     *LIKE         DEFINE    ERREFA        TTREFA
     C     *LIKE         DEFINE    ERREOB        RECEOB
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQFYR         XXYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQREOB        XXREOB
     C                   MOVE      QQMODE        XXMODE            1
     C                   MOVE      *BLANKS       XCNAME           40
     C                   MOVE      *BLANKS       XCADR1           40
     C                   MOVE      *BLANKS       XCADR2           40
     C                   MOVE      *BLANKS       XXST             13
     C                   MOVE      *ZEROS        XXZP1             5
     C                   MOVE      *ZEROS        XXZP2             4
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVE      *BLANKS       XXTERM            1
     C                   MOVE      *BLANKS       XCCSZ            40
     C                   MOVE      *BLANKS       XXFNNM           35
     C                   MOVE      *BLANKS       XXPNAM           30
     C                   Z-ADD     *ZEROS        Z                 5 0
     C                   Z-ADD     *ZEROS        XXACDT            6 0
     C                   Z-ADD     *ZEROS        XXSS#             9 0
     C                   Z-ADD     *ZEROS        XOAMTC            9 2
     C                   Z-ADD     *ZEROS        XORED             9 2
     C                   Z-ADD     *ZEROS        XNBAMT            9 2
     C                   Z-ADD     *ZEROS        XNRED             9 2
     C                   Z-ADD     *ZEROS        TOAMTC            9 2
     C                   Z-ADD     *ZEROS        TORED             9 2
     C                   Z-ADD     *ZEROS        TNBAMT            9 2
     C                   Z-ADD     *ZEROS        TNRED             9 2
     C                   Z-ADD     *ZEROS        TT92N1            9 2
     C                   Z-ADD     *ZEROS        TTREFA
     C*
     C                   TIME                    DSDTTM
     C*
     C*** BEG ADD *** 01/28/10 ****************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C****END ADD *** 01/28/10 ****************************************
     C*
     C*
     C* SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C* CHAIN TO USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C* GET ROUTING INFORMATION FOR REFUNDS
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C*********          MOVE      '0'           XXPRT
     C*********     ORKEY         CHAIN     POR01                              60
     C*********      *IN60         IFEQ      '0'
     C*********          EXCEPT    ROUTE
     C*********          END                                                    END IFEQ '0'
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C* CHAIN TO THE COMPANY AUXILIARY FILE FOR NAME/ADDRESS
     C*
     C     C1KEY         CHAIN     PC101                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      C1ZIP         XSZIP
     C                   MOVE      C1NAME        XSNAME
     C                   MOVEL     '('           XCPHN#
     C                   MOVE      C1AREA        XXAREA
     C                   MOVE      ')'           XXCPRN
     C                   MOVEL     C1PHN#        XXPH1
     C                   MOVE      C1PHN#        XXPH2
     C                   MOVE      '-'           XXHP1
     C                   MOVE      '-'           XFHYP1
     C                   MOVE      '-'           XFHYP2
     C                   MOVE      '-'           XFHYP3
     C*
     C* REMOVE LEADING BLANKS FROM REMIT TO NAME
     C*
     C                   Z-ADD     1             Z
     C     *BLANK        LOOKUP    XCNM(Z)                            60
     C     *IN60         IFEQ      '1'
     C                   MOVEA     XCNM(Z)       XCNAME
     C                   ELSE
     C                   MOVE      *BLANKS       XCNAME
     C                   END
     C*
     C                   MOVEL     C1ADR1        XCADR1
     C                   MOVEL     C1ADR2        XCADR2
     C     XCADR2        COMP      *BLANKS                            9494
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     C1ST          XXST
     C                   MOVEA     XXST          SZ
     C                   MOVE      S1ZPP1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     S1ZPP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      S1ZPP2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVEL     C1CTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XCCSZ
     C                   END
     C*
     C* GET FUND(GROUP) INFORMATION.
     C*
     C     FNKEY         CHAIN     PFN01                              51
     C     *IN51         IFEQ      '0'
     C                   Z-ADD     FNWCF#        XXRFND
     C                   Z-ADD     1             Z
     C     *BLANK        LOOKUP    FNNM(Z)                            60
     C                   MOVEA     FNNM(Z)       XXFNNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C*
     C* GET CLAIM MASTER FILE
     C*
     C     CMKEY         CHAIN     PCM01                              51
     C     *IN51         IFEQ      '0'
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT
      ***** Beg Add *** 06/09/20 *******************************************
     C****               MOVE      CMSS#         XXSS#
       /Free
         XXSSN = MaskData('SSN':'CMSS#':CMSS#);
       /End-free
     C                   MOVE      XXSSN         XXSS#
      ***** End Add *** 06/09/20 *******************************************
     C*
     C     CMSS#         CHAIN     PCD01                              52
     C     *IN52         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVEL     CDLSN         XXLAST
     C     CDFSN         IFNE      *BLANK
     C                   MOVEL     CDFSN         XXFRST
     C                   MOVE      '1'           XXTERM
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END
     C                   MOVEL     XXLAST        XXPNAM
     C                   ELSE
     C                   MOVEL     *BLANKS       XXPNAM
     C                   END
     C*
     C                   ELSE
     C                   Z-ADD     0             XXACDT
     C                   MOVE      *ZERO         XXSS#
     C                   MOVEL     *BLANKS       XXPNAM
     C                   END
     C*
     C* PRINT THE HEADING
     C*
     C                   EXCEPT    HEAD
     C*
     C* CHAIN TO THE REFUND REQUEST FILE
     C*
     C     ERKEY         CHAIN     PER01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C* CHECK TO SEE IF THIS IS A DELETED EOMB
     C*
     C                   MOVE      '0'           *IN53
     C                   Z-ADD     *ZEROS        RECEOB
     C                   Z-ADD     *ZEROS        XNBAMT
     C                   Z-ADD     *ZEROS        XNRED
     C     ERFL02        IFNE      'Y'
     C*
     C* CHAIN TO EOMB FILE WITH THE OLD EOMB NUMBER TO GET THE ORG CHARGES,
     C* THE NEW INVOICED AMOUNT AND THE ORIGINAL REDUCTIONS
     C*
     C     EOKEY         CHAIN     PEO01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     EOAMTC        XOAMTC
     C     EOAMTC        SUB       EOAMT         XORED
     C                   Z-ADD     EOBAMT        XNBAMT
     C                   ELSE
     C                   Z-ADD     *ZEROS        XOAMTC
     C                   Z-ADD     *ZEROS        XORED
     C                   Z-ADD     *ZEROS        XNBAMT
     C                   END
     C*
     C                   Z-ADD     ERRECA        XNRED
     C*
     C* CALCULATE THE TRUE NEW REDUCTIONS BY SUBTRACTING THE NEW REDUCTIONS
     C* FROM THE OLD REDUCTIONS
     C*
     C     XORED         SUB       XNRED         XNRED
     C*
     C                   ELSE                                                   ELSE ERFL02 NE 'Y'
     C*
     C* CHAIN TO THE EOMB DELETE FILE TO GET THE DELETED EOMB INFORMATION
     C*
     C     EOKEY         CHAIN     PDE01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     EOAMTC        XOAMTC
     C     EOAMTC        SUB       EOAMT         XORED
     C                   Z-ADD     EOBAMT        ER92N1
     C                   ELSE                                                   ELSE *IN60 EQ '0'
     C                   Z-ADD     *ZEROS        XOAMTC
     C                   Z-ADD     *ZEROS        XORED
     C                   Z-ADD     *ZEROS        ER92N1
     C                   END                                                    END *IN60 EQ '0'
     C*
     C* CHAIN TO THE REFUND TRANSACTION FILE WITH THE OLD EOMB NUMBER TO GET
     C* THE RECON EOMB IF ANY HAS BEEN PROCESSED
     C*
     C                   MOVE      '0'           *IN53
     C     ERKEY         CHAIN     PER02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN53
     C                   Z-ADD     ERREOB        RECEOB
     C                   END                                                    END *IN60 EQ '0'
     C*
     C                   END                                                    END ERFL02 NE 'Y'
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    DETL
     C*
     C* ACCUMULATE THE TOTAL AMOUNTS
     C*
     C                   ADD       XOAMTC        TOAMTC
     C                   ADD       XORED         TORED
     C                   ADD       XNRED         TNRED
     C                   ADD       ER92N1        TT92N1
     C                   ADD       XNBAMT        TNBAMT
     C                   ADD       ERREFA        TTREFA
     C*
     C     ERKEY         READE     PER01                                  50
     C                   END                                                    END *IN50 DOWEQ '0'
     C*
     C* EXCEPT THE TOTAL LINE
     C*
     C     XXLINE        IFGE      50
     C                   EXCEPT    HEAD
     C                   END
     C                   EXCEPT    TOTL
     C*
     C* END OF PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain Orkey Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     O/EJECT
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
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
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
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                57 'INC. 1997     '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT   E            HEAD           1 04
     O*
     O          E            HEAD           1
     O                       XCNAME              95
     O                                          111 'PAGE:'
     O                       PAGE               122
     O          E            HEAD           1
     O                       XCADR1              95
     O                                          111 'DATE:'
     O                       DSDATE        Y    122
     O          E    94      HEAD           1
     O                       XCADR2              95
     O          E            HEAD           1
     O                       XCCSZ               95
     O          E            HEAD           2
     O                       XCPHN#              69
     O          E            HEAD           2
     O                                           73 '***** REFUND MEMO *****'
     O          E            HEAD           1
     O                                            9 'GROUP   :'
     O                       XXFND         Z     13
     O                                           15 '-'
     O                       XXFNNM              51
     O          E            HEAD           1
     O                                            9 'CASE#   :'
     O                       XF#                 30
     O                       XXPNAM              61
     O          E            HEAD           2
     O                                            9 'SOC SEC :'
     O                       XXSS#               21 '   -  -    '
     O                                           30 'ACC DT:'
     O                       XXACDT        Y     39
     O          E            HEAD           1
     O                                           24 'ORIGINAL'
     O                                           41 'ORIGINAL'
     O                                           55 'NEW'
     O                                           76 'ORIGINAL'
     O                                           88 'NEW'
     O          E            HEAD           2
     O                                            8 'EOMB#'
     O                                           27 'TOTAL CHARGED'
     O                                           45 'TOTAL REDUCTIONS'
     O                                           63 'TOTAL REDUCTIONS'
     O                                           79 'TOTAL INVOICED'
     O                                           95 'TOTAL INVOICED'
     O                                          110 'REFUND AMOUNT'
     O          E            DETL           1
     O                       EROEOB        Z     10
     O                       XOAMTC        J     27
     O                       XORED         J     45
     O                       XNRED         J     63
     O                       ER92N1        J     79
     O                       XNBAMT        J     95
     O                       ERREFA        J    110
     O               53                         112 '*'
     O          E            TOTL        1  1
     O                       TOAMTC        J     27
     O                       TORED         J     45
     O                       TNRED         J     63
     O                       TT92N1        J     79
     O                       TNBAMT        J     95
     O          E            TOTL           1
     O                                          110 '=============='
     O          E            TOTL           1
     O                                           90 'TOTAL TO BE REFUNDED:'
     O                       TTREFA        4B   109 '$'
     O          E    53      TOTL        2
     O               53                          24 'This EOMB was reconsi'
     O               53                          39 'dered on EOMB# '
     O               53      RECEOB        Z     48
