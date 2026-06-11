     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE F8115R - PURGE UR FILES
     H****************************************************************
     H*  PROGRAM DESCRIPTION:                                        *
     H*       THIS PROGRAM WILL PURGE ALL RECORDS FOR THE FUND       *
     H*       MASTER FILES FOR THE SELECTED FUND                     *
     H*                                                              *
     H*  CALLED FROM:                                                *
     H*    F8115C - UR PURGE OVERRIDE CL                           *
     H*                                                              *
     H*  CALLS TO:                                                   *
     H*    NONE                                                      *
     H*                                                              *
     H*  PROGRAMMER: C:53                                            *
     H*  CREATED:    04/08/96    PROJECT: MC134                      *
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                              *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 08/15/00 C:62  PROJECT: MC215                                *
     H*                REPLACE SFBPF REFERENCES WITH APIS.           *
     H*                                                              *
     H*  05/05/00 C:62 PROJECT AR001                                 *
     H*                ADD FILE NMIMP TO THE PROCESS.                *
     H*                                                              *
     H*  03/28/00 C:53 PROJECT MC228                                 *
     H*                ADD FILE UDPOP TO THE PROCESS.                *
     H*                                                              *
     H****************************************************************
     H* INDICATOR USAGE:                                             *
     H*                                                              *
     H*  08 - BATCH                                                  *
     H*  09 - DEMAND                                                 *
     H*  30 - GLOBAL DELETE OF FUND                                  *
     H*  60 - GENERAL CHAIN                                          *
     H*                                                              *
     H*                                                              *
     H****************************************************************
     H*
     F* SYSTEM FILES
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 **************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F*
     FNMARL2    UF   E           K DISK
     F*** BEGIN ADD *** 05/05/00 **************************************
     FNMIML2    UF   E           K DISK
     F*** END   ADD *** 05/05/00 **************************************
     FNXTMP     UF   E           K DISK
     FUAEEL     UF   E           K DISK
     FUAESL     UF   E           K DISK
     FUALCL     UF   E           K DISK
     FUMCHL     UF   E           K DISK
     FUMCRL     UF   E           K DISK
     FUMIQL     UF   E           K DISK
     FUMPCL     UF   E           K DISK
     FUMRTL     UF   E           K DISK
     FUXLCL     UF   E           K DISK
     F                                     RENAME(PLC01:PLC02)
     FUXPAL     UF   E           K DISK
     FUXPCL3    UF   E           K DISK
     F                                     RENAME(PPC01:PPC02)
     FUXPDL     UF   E           K DISK
     F*
     F*** BEG ADD *** 03/28/00 **************************
     FUDPOL     UF   E           K DISK
     F*** END ADD *** 03/28/00 **************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C*   KEY LIST FOR ALL FILES
     C*
     C     XXKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR ALL FILES (GLOBAL DELETE)
     C*
     C     XXKEYG        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB
     C*
     C*   KEY LIST FOR FILES WITHOUT SUB# IN KEY
     C*
     C     XXKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE REPORT OVERRIDE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FS BASE CONTROL FILE
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** FBKY      KLIST
     C**********           KFLD           XSKEY   2
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    X@CO#             3
     C                   PARM                    X@SUB             5
     C                   PARM                    X@FND             3
     C                   PARM                    X@QUE            10
     C*
     C     X@FND         COMP      '000'                                  30
     C*
     C                   MOVE      X@CO#         XXCO#             3 0
     C                   MOVE      X@SUB         XXSUB             5 0
     C                   MOVE      X@QUE         QQQUE            10
     C  N30              MOVE      X@FND         XXFND             3 0
     C   30              Z-ADD     *ZEROS        XXFND
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   CLEAR ACCUMULATION FIELDS
     C*
     C                   Z-ADD     *ZERO         XX#REC            6 0
     C                   Z-ADD     *ZERO         XXTREC            8 0
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C                   TIME                    DSDTTM
     C                   MOVE      X@CO#         XXCO#
     C                   MOVE      UDATE         XXDATE            6 0
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      *
     C*
     C*   CONTROL INFO
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           MOVE *BLANKS   XSKEY
     C**********           MOVEL'FB'      XSKEY
     C********** FBKY      CHAINSFB01                60
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   END
     C                   MOVE      *ON           *INOF
     C*
     C                   EXCEPT    HEAD
     C*** BEGIN ADD *** 05/05/00 **************************************
     C*
     C*  READ/DELETE ALL RECORDS IN THE FILE FOR THE SELECTED FUND
     C*
     C*  NMIMP - INVOICE MASTER FILE
     C*
     C  N30XXKEY         CHAIN     PIM01                              60
     C   30XXKEYG        CHAIN     PIM01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PIM01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PIM01                                  60                R
     C   30XXKEYG        READE     PIM01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'NMIMP'       XXFILE           10
     C                   EXCEPT    DET
     C*** END   ADD *** 05/05/00 **************************************
     C*
     C*  READ/DELETE ALL RECORDS IN THE FILE FOR THE SELECTED FUND
     C*
     C*  NMARP - A/R TX FILE
     C*
     C  N30XXKEY         CHAIN     PAR01                              60
     C   30XXKEYG        CHAIN     PAR01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PAR01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PAR01                                  60                R
     C   30XXKEYG        READE     PAR01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'NMARP'       XXFILE           10
     C                   EXCEPT    DET
     C*
     C*  NXTMP - T/M DETAIL CHARGE WORKFILE
     C*
     C  N30XXKEY         CHAIN     PTM01                              60
     C   30XXKEYG        CHAIN     PTM01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PTM01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PTM01                                  60                R
     C   30XXKEYG        READE     PTM01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'NXTMP'       XXFILE           10
     C                   EXCEPT    DET
     C*
     C*  UAEEP - C/R EDIT EXCEPTION CLAIMS WORKFILE
     C*
     C  N30XXKEY         CHAIN     PEE01                              60
     C   30XXKEYG        CHAIN     PEE01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEE01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PEE01                                  60                R
     C   30XXKEYG        READE     PEE01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'UAEEP'       XXFILE           10
     C                   EXCEPT    DET
     C*
     C*  UAESP C/R EDIT SELECTED CLAIMS WORKFILE
     C*
     C  N30XXKEY         CHAIN     PES01                              60
     C   30XXKEYG        CHAIN     PES01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PES01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PES01                                  60                R
     C   30XXKEYG        READE     PES01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'UAESP'       XXFILE           10
     C                   EXCEPT    DET
     C*
     C*  UALCP - PRE-CERT LETTERS WORKFILE FOR SCREEN
     C*
     C  N30XXKEY         CHAIN     PLC01                              60
     C   30XXKEYG        CHAIN     PLC01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PLC01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PLC01                                  60                R
     C   30XXKEYG        READE     PLC01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'UALCP'       XXFILE           10
     C                   EXCEPT    DET
     C*
     C*  UMCHP - PRE-CERT STATUS HISTORY FILE
     C*
     C  N30XXKEY         CHAIN     PCH01                              60
     C   30XXKEYG        CHAIN     PCH01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCH01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PCH01                                  60                R
     C   30XXKEYG        READE     PCH01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'UMCHP'       XXFILE           10
     C                   EXCEPT    DET
     C*
     C*  UMCRP - C/R EDIT MASTER FILE
     C*
     C  N30XXKEY         CHAIN     PCR01                              60
     C   30XXKEYG        CHAIN     PCR01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCR01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PCR01                                  60                R
     C   30XXKEYG        READE     PCR01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'UMCRP'       XXFILE           10
     C                   EXCEPT    DET
     C*
     C*  UMIQP - INTERQUAL CASE STATUS FILE
     C*
     C  N30XXKEY         CHAIN     PIQ01                              60
     C   30XXKEYG        CHAIN     PIQ01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PIQ01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PIQ01                                  60                R
     C   30XXKEYG        READE     PIQ01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'UMIQP'       XXFILE           10
     C                   EXCEPT    DET
     C*
     C*  UMPCP - PRE-CERT MASTER FILE
     C*
     C  N30XXKEY         CHAIN     PPC01                              60
     C   30XXKEYG        CHAIN     PPC01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PPC01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PPC01                                  60                R
     C   30XXKEYG        READE     PPC01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'UMPCP'       XXFILE           10
     C                   EXCEPT    DET
     C*
     C*  UMRTP - INTERQUAL REVIEW TRANS FILE
     C*
     C  N30XXKEY         CHAIN     PRT01                              60
     C   30XXKEYG        CHAIN     PRT01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRT01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PRT01                                  60                R
     C   30XXKEYG        READE     PRT01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'UMRTP'       XXFILE           10
     C                   EXCEPT    DET
     C*
     C*  UXLCP - PRE-CERT LETTERS WORKFILE FOR PRTDOC
     C*
     C  N30XXKEY         CHAIN     PLC02                              60
     C   30XXKEYG        CHAIN     PLC02                              60
     C*
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PLC02
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PLC02                                  60                R
     C   30XXKEYG        READE     PLC02                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'UXLCP'       XXFILE           10
     C                   EXCEPT    DET
     C*
     C*  UXPAP - PROV ANALYSIS REPORT WORKFILE
     C*
     C  N30XXKEY         CHAIN     PPA01                              60
     C   30XXKEYG        CHAIN     PPA01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PPA01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PPA01                                  60                R
     C   30XXKEYG        READE     PPA01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'UXPAP'       XXFILE           10
     C                   EXCEPT    DET
     C*
     C*  UXPCP - PRE-CERT ACTIVITY REPORT WORKFILE
     C*
     C  N30XXKEY         CHAIN     PPC02                              60
     C   30XXKEYG        CHAIN     PPC02                              60
     C*
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PPC02
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PPC02                                  60                R
     C   30XXKEYG        READE     PPC02                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'UXPCP'       XXFILE           10
     C                   EXCEPT    DET
     C*
     C*  UXPDP - PROC/DIAG ANALYSIS REPORT WORKFILE
     C*
     C  N30XXKEY         CHAIN     PPD01                              60
     C   30XXKEYG        CHAIN     PPD01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PPD01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PPD01                                  60                R
     C   30XXKEYG        READE     PPD01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'UXPDP'       XXFILE           10
     C                   EXCEPT    DET
     C*** BEG ADD *** 03/28/00 **************************
     C*
     C*  UDPOP - Pre-cert Procedure Code File
     C*
     C  N30XXKEY         CHAIN     PPO01                              60
     C   30XXKEYG        CHAIN     PPO01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PPO01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PPO01                                  60                R
     C   30XXKEYG        READE     PPO01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'UDPOP'       XXFILE           10
     C                   EXCEPT    DET
     C*** END ADD *** 03/28/00 **************************
     C*
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    TOTL
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C*
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
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
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
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE          2
     O                                           23 'REPORT SELECTION:'
     O          E            ROUTE          2
     O                                           20 'CO#:'
     O                       XXCO#               25
     O          E            ROUTE          2
     O                                           20 'DATE:'
     O                       XXDATE              30 '  /  /  '
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                                51 '.1997'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O****************************************************************
     O*     HEADING
     O****************************************************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              84
     O                       XXDATE        Y    130
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              94
     O                                          124 'PAGE:'
     O                       PAGE          Z    130
     O          E            HEAD           2
     O                                           18 'FILE NAME'
     O                                           44 '# DELETED RECORDS'
     O****************************************************************
     O*     DETAIL LINE WITH FILE NAME AND COUNT
     O****************************************************************
     O          E            DET            1
     O                       XXFILE              19
     O                       XX#REC        JB    40
     O****************************************************************
     O*     TOTAL RECORD COUNT FOR ALL FILES
     O****************************************************************
     O          E            TOTL        2  2
     O                                           18 'ALL FILES'
     O                       XXTREC        J     40
