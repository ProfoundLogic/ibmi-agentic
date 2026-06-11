     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE W7011R - DARTS AND PENDING BILLS DIARY BATCH UPDATE
     H* ***************************************************************
     H* PROGRAM DESCRIPTION:  THIS PROGRAM READS ALL PENDING CHECKS   *
     H*   IN WHICH THE 'DIARY SENT FLAG' = 'N' AND WRITES A CORRE-    *
     H*   SPONDING DARTS DIARY REMARK.                                *
     H*                                                               *
     H* CALLED FROM:  W7011C                                          *
     H* CALLS TO:  NONE                                               *
     H* PROGRAMMER:  LARRY KORZON                                     *
     H* CREATED:  10/02/92            PROJECT: AOA                    *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/29/03 C:62  Project: 129MBO                                *
     H*                Skip if Group type is "1", "2" or "3".         *
     H*                                                               *
     H* 04/29/02 C:15  Support: DR065/32234                           *
     H*                Corrected diary date calculation.              *
     H*                                                               *
     H* 03/05/01 C:15  Support: DR025/26462                           *
     H*                Added code to load the correct diary seq# into *
     H*                RPDSEQ.                                        *
     H*                                                               *
     H* 12/05/00 C:15  Project: WC577                                 *
     H*                Added code to load DSTIME to prevent error.    *
     H*                                                               *
     H*  04/26/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*                                                               *
     H* 02/25/00 C:15  Project: WC577                                 *
     H*                Added code to load the maint fields in WARPP.  *
     H*                                                               *
     H* 02/24/00 C:15  Support: WC025/21292                           *
     H*                Changed primary read file to prevent incorrect *
     H*                update of RPDSEQ.                              *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H* 04/09/99 C:15 Support: WC025/17374                            *
     H*               Changed pgm to clear setoff *IN75 when no data  *
     H*               entry pgm security option is setup for a DARTS  *
     H*               remark.                                         *
     H*                                                               *
     H* 04/30/98 C:15 Project: WC577                                  *
     H*               Changed the user authority validation routine.  *
     H*                                                               *
     H*  02/23/98 C:85 Project: WC577                                 *
     H*                CHANGED THE FIELD MOVED TO CMSMCD              *
     H*                FROM *BLANKS TO CTSMCD.                        *
     H*                                                               *
     H*  09/23/97  47  WC659.3                                        *
     H*            UPDATED TO VERSION 3 SPECS.                        *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H* ***************************************************************
     H*  INDICATOR USAGE:                                             *
     H*   *IN40 - END OF FILE (WARPP)                                 *
     H*   *IN41 - GENERAL USAGE: CHAIN & READ                         *
     H*   *IN60 - GENERAL USAGE: CHAIN & READ                         *
     H*   *IN75 - DIARIED USER NOT AUTHORIZED TO PROGRAM              *
     H* ***************************************************************
     FWARPL     UF   E           K DISK
     F*** BEGIN DLT *** 02/24/00 **************************************
     F*****WARPLF  IF  E           K        DISK
     F*****       PRP01                             KRENAMEPRP02
     F*** END DLT ***** 02/24/00 **************************************
     F*** BEGIN CHG *** 04/26/00 **************************************
     F*****SDCMLA  UF  E           K        DISK                      A
     FSDCMLA    UF   E           K DISK
     F*** END CHG *** 04/26/00 ****************************************
     F                                     RENAME(PCM01:PCM02)
     FWMCML1    IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FMMOPL     IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FWMNRL     IF   E           K DISK
      *

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D*** BEGIN ADD *** 04/26/00 **************************************
     D  DSDATE                 7     14  0
     D*** END ADD *** 04/26/00 ****************************************
     D                 DS
     D  OT                     1      6
     D                                     DIM(6)
     D  OPTY01                 1      1
     D  OPTY02                 2      2
     D  OPTY03                 3      3
     D  OPTY04                 4      4
     D  OPTY05                 5      5
     D  OPTY06                 6      6
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D                 DS
     D  QCCOM                  1     74
     D  QCCOM1                 1      2
     D  QCCOM2                 4     16
     D  QCCOM3                19     74
     D                 DS
     D  XXFNMD                 1     13
     D  XXFEIN                 1      9  0
     D  XXHYPN                10     10
     D  XXMOD                 11     13  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     IPUP02
     I              UPCO#                       U2CO#
     I              UPFND                       U2FND
     I              UPUSR                       U2USR
     I              UPTYPE                      U2TYPE
     I              UPLVL                       U2LVL

     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C*
     C*  REMARK CODE TABLE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    XXRMCD
     C*
     C*  DARTS FILE
     C*
     C     C2KY          KLIST
     C                   KFLD                    QCKEY
     C                   KFLD                    XXSEQ#
     C***
     C*** KEY LIST FOR EOMB FILE
     C***
     C***        EOKEY     KLIST
     C***                  KFLD           XXCO#
     C***                  KFLD           FNSUB#
     C***                  KFLD           FNFND
     C***                  KFLD           RPFYR
     C***                  KFLD           RPCASE
     C***                  KFLD           RPEOB#
     C***
     C*** KEY LIST FOR THE FUND
     C***
     C***        FNKEY     KLIST
     C***                  KFLD           XXCO#
     C***                  KFLD           RPFND
     C*
     C*  NETWORK REVIEW FILE KEY LIST
     C*
     C     NRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  OPTION FILE
     C*
     C     OPKY          KLIST
     C                   KFLD                    CTMNAM
     C                   KFLD                    CTOPTN
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  REQUEST PAYMENT FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C                   KFLD                    RPSEQ#
     C*
     C     RPKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    XXSTAF
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
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
     C     *LIKE         DEFINE    RPCO#         XXCO#
     C     *LIKE         DEFINE    B2KEY         XXKEY
     C     *LIKE         DEFINE    CMRMCD        XXRMCD
     C     *LIKE         DEFINE    CMSEQ#        XXSEQ#
     C     *LIKE         DEFINE    CMUSER        XXUSER
     C     *LIKE         DEFINE    UPSTAF        XXSTAF
     C     *LIKE         DEFINE    RPFRDT        XXDATE
     C*
     C     *LIKE         DEFINE    RPFND         @@GRP#
     C     *LIKE         DEFINE    RPCO#         @@CO#
     C     *LIKE         DEFINE    RPFND         @@WCMS
     C     *LIKE         DEFINE    RPFYR         @@FYR
     C     *LIKE         DEFINE    RPCASE        @@CASE
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQDATE        XXDAT8            8 0
     C                   Z-ADD     XXDAT8        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   MOVE      '-'           XXHYPN
     C*
     C** START READING REQUEST PAYMENT FILE
     C*
     C*** BEGIN CHG *** 02/24/00 **************************************
     C********** RPKY1     CHAINPRP02                40
     C     RPKY1         CHAIN     PRP01                              40
     C*** END CHG ***** 02/24/00 **************************************
     C     *IN40         DOWEQ     '0'
     C*
     C*** BEGIN ADD *** 02/24/00 **************************************
     C     RPSTAT        IFEQ      'P'
     C*** END ADD ***** 02/24/00 **************************************
     C*
     C     RPSENT        IFNE      'Y'
     C*
     C                   Z-ADD     RPFEIN        XXFEIN
     C                   Z-ADD     RPMOD         XXMOD
     C     NRKY          CHAIN     PNR01                              60
     C     *IN60         IFEQ      '0'
     C     NRGRTY        IFEQ      'N'
     C*** BEGIN ADD *** 05/29/03 **************************************
     C     NRGRTY        OREQ      '1'
     C     NRGRTY        OREQ      '2'
     C     NRGRTY        OREQ      '3'
     C*** END   ADD *** 05/29/03 **************************************
     C                   GOTO      NEXT
     C                   END
     C*
     C**  IF GROUP PROVIDER IS B (REVIEW SERVICES) AND SYSTEM TYPE
     C**  IS NOT A U (PRE/RE-CERT) OR A M (MEDICAL MGMT T/M), THEN
     C**  DO NOT CREATE DIARY.
     C*
     C     NRGRTY        IFEQ      'B'
      *
      *  WCMS / WCMC Installed flags determination
      *
     C                   CALL      'SA001R'
     C                   PARM                    QQWCIF            1
     C                   PARM                    QQFBIF            1
     C                   PARM                    QQFL01            1
      *
     C     QQFBIF        IFEQ      'Y'
     C     QQFL01        ANDEQ     'Y'
     C                   CALL      'FA016R'
     C                   PARM      RPCO#         @@CO#                          Company #
     C                   PARM      RPFND         @@WCMS                         WCMS Fund
     C                   PARM      RPFYR         @@FYR                          Policy Year
     C                   PARM      RPCASE        @@CASE                         Claim #
     C                   PARM                    @@EOB#            9 0          EOMB #
     C                   PARM                    @@RRN             9 0          EOMB RRN#
     C                   PARM                    @@SUB#            5 0          Subscriber #
     C                   PARM                    @@GRP#                         WCMC Group #
     C                   PARM                    @@RC              1            Return Code
     C*
     C     @@RC          IFNE      'U'
     C     @@RC          ANDNE     'M'
     C                   GOTO      NEXT
     C                   END
     C*
     C                   END                                                    NRGRTY = B
     C                   END                                                    QQFBIF = Y
     C*
     C                   END                                                    *IN60
     C*
     C*  INITIALIZE HOLD FIELDS
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      *BLANKS       XXRMCD
     C                   MOVE      *BLANKS       QCCOM
     C                   MOVE      *BLANKS       XXFNMD
     C                   MOVE      '-'           XXHYPN
     C                   MOVE      *BLANKS       XXUSER
     C                   MOVE      *BLANKS       XXSTAF
     C*
     C*  GET REMARK CODE FROM PMT VAL 1 FILE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     RPBR2         XXKEY
     C     TBKY          CHAIN     PB201                              60
     C  N60              MOVEL     B2RMCD        XXRMCD
     C*
     C     XXRMCD        IFEQ      *BLANK
     C                   GOTO      NEXT
     C                   END
     C*
     C*
     C*  GET DIARY USER AND CHECK AUTHORITY. IF NOT AUTHORIZED SEND
     C*  DIARY TO SUPERVISOR.
     C*
     C     CMKY          CHAIN     PCM01                              60
     C  N60              MOVE      CMCLEX        XXSTAF
     C                   EXSR      SRAUT
     C     *IN75         IFEQ      '1'
     C                   MOVE      *BLANKS       XXSTAF
     C                   MOVE      CMSUPR        XXSTAF
     C                   END
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         CABEQ     '1'           NODRT
     C  N60              MOVEL     UPUSR         XXUSER
     C*
     C*  GET PROVIDER NAME
     C*
     C                   Z-ADD     RPFEIN        XXFEIN
     C                   Z-ADD     RPMOD         XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END
     C                   MOVEL     XXLAST        QCCOM3
     C                   END
     C*
     C     RPDUE         IFLE      XXDATE
     C                   MOVE      '$$'          QCCOM1
     C                   END
     C                   MOVE      XXFNMD        QCCOM2
     C*
     C** CREATE DARTS DIARY REMARK
     C*
     C*** BEGIN DEL *** 04/26/00 **************************************
     C********             Z-ADDXXCO#     QCCO#
     C********             Z-ADDRPFND     QCFND
     C********             Z-ADDRPFYR     QCFYR
     C********             Z-ADDRPCASE    QCCASE
     C********
     C***GET*NEXT SEQUENCE NUMBER FOR DARTS RECORD
     C********
     C********             MOVE *HIVAL    XXSEQ#
     C********   C2KY      SETGTPCM02
     C********             READPPCM02                    41
     C********   *IN41     IFEQ '1'
     C********   QCKEY     ORNE CMKEY
     C********             Z-ADD0         CMSEQ#
     C********             END
     C********
     C***INITIALIZE FIELDS
     C********
     C********             MOVE *BLANKS   CMCOMM
     C********             MOVE *BLANKS   CMKEY
     C********             MOVE *BLANKS   CMDCID
     C********             MOVE *BLANKS   CMPNAM
     C********             MOVE *BLANKS   CMUSER
     C********             MOVE *BLANKS   CMPGM
     C********             MOVE *BLANKS   CMIUSR
     C********             MOVE *BLANKS   CMFOLD
     C********             MOVE *BLANKS   CMIPGM
     C********             MOVE *BLANKS   CMRMCD
     C********
     C********             MOVELQCKEY     CMKEY
     C********             MOVE *BLANKS   CMCSID
     C********             MOVELQCCOM     CMCOMM
      ********
     C********             Z-ADDRPDUE     CMCDAT
     C********   RPDUE     IFEQ *ZERO
     C********             Z-ADDXXDATE    CMCDAT
     C********             END
      ********
     C********             MOVE *BLANKS   CMCODE
     C********             MOVELCTDEPG    CMDCID
     C********             MOVEL'W0070R'  CMPNAM
     C********             Z-ADDXXDATE    CMTRDT
     C********             TIME           DSDTTM
     C********             Z-ADDDSTIME    CMTRTM
     C********             ADD  1         CMSEQ#
     C********             Z-ADD*ZEROS    CMSECL
     C********             MOVE 'N'       CMSBYL
     C********             MOVE 'A'       CMSTAT
     C********             MOVELXXUSER    CMUSER
     C********             MOVELQ1PGM     CMPGM
     C********             MOVE '0000001' CMTIME
     C********             Z-ADDXXDATE    CMDT
     C********             MOVELQ1USER    CMIUSR
     C********   CTDEPG    IFNE *BLANKS
     C********             MOVELQCKEY     CMFOLD
     C********             ELSE
     C********             MOVE *BLANKS   CMFOLD
     C********             END
     C********             Z-ADD0         CMDCDT
     C********             MOVE 'Y'       CMDIFL
     C********             MOVE *BLANKS   CMAPID
     C********             MOVE *BLANKS   CMIDOC
     C********             MOVE *BLANKS   CMIQUE
     C********             MOVE *BLANKS   CMIMGF
     C********             MOVE *BLANKS   CMIPTH
     C********             MOVE *BLANKS   CMPIDX
     C********             MOVE *BLANKS   CMQPTY
     C********             MOVE *BLANKS   CMDCTP
     C********             MOVELQ1PGM     CMIPGM
     C********                                                         *********
     C****BEGIN DEL *** 02/23/98 ***********************************************
     C**********           MOVE *BLANKS   CMSMCD
     C****END*  DEL *** 02/23/98 ***********************************************
     C********                                                         *********
     C********   CTDEPG    IFNE *BLANKS
     C********             MOVE 'Y'       CMFL01
     C********             ELSE
     C********             MOVE *BLANKS   CMFL01
     C********             END
     C********   CTDPRG    IFGT *ZERO
     C********             MOVE 'Y'       CMFL02
     C********             ELSE
     C********             MOVE *BLANKS   CMFL02
     C********             END
     C********             MOVE CTFL02    CMFL03
     C********             MOVE *BLANKS   CMFL04
     C********             MOVE *BLANKS   CMFL05
     C********             MOVELXXRMCD    CMRMCD
     C********                                                         *********
     C****BEGIN ADD *** 02/23/98 ***********************************************
     C********                                                         *********
     C********   CMRMCD    CHAINPCT01                60
     C********                                                         *********
     C********   *IN60     IFEQ *OFF
     C********             MOVE CTSMCD    CMSMCD
     C********             ENDIF                           *IN60=OFF
     C********                                                         *********
     C****END*  ADD *** 02/23/98 ***********************************************
     C********
     C********             Z-ADDXXDATE    CMIDT
     C********
     C********             WRITEPCM02
     C******BEGIN ADD **** 07/01/99 ***********************************
     C********             FEOD SDCMLA
     C******END   ADD **** 07/01/99 ***********************************
     C*** END DEL *** 04/26/00 ****************************************
     C*** BEGIN ADD *** 04/26/00 **************************************
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   Z-ADD     XXCO#         QCCO#
     C                   Z-ADD     RPFND         QCFND
     C                   Z-ADD     RPFYR         QCFYR
     C                   Z-ADD     RPCASE        QCCASE
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVEL(P)  QCCOM         Q#COMM          256
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     RPDUE         SRDTI
     C*** Begin chg *** 04/29/02 **************************************
     C******************** CALL 'SRCVTDAT'DATCHK
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** End chg ***** 04/29/02 **************************************
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C     RPDUE         IFEQ      0
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT
     C                   ENDIF                                                  DUE = 0
     C                   MOVEL(P)  XXRMCD        Q#RMCD            5
     C                   MOVEL(P)  CTDEPG        Q#DCID           12
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  XXUSER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C     CTDEPG        IFNE      *BLANKS
     C                   MOVEL(P)  QCKEY         Q#FOLD           80
     C                   ELSE                                                   DEPG <> blanks
     C                   MOVE      *BLANKS       Q#FOLD
     C                   ENDIF                                                  DEPG <> blanks
     C                   MOVEL(P)  'Y'           Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C     CTDEPG        IFNE      *BLANKS
     C                   MOVE(P)   'Y'           Q#FL01            1
     C                   ELSE                                                   DEPG <> blanks
     C                   MOVE      *BLANKS       Q#FL01
     C                   ENDIF                                                  DEPG <> blanks
     C     CTDPRG        IFGT      *ZERO
     C                   MOVE(P)   'Y'           Q#FL02            1
     C                   ELSE                                                   DPRG > blanks
     C                   MOVE      *BLANKS       Q#FL02
     C                   ENDIF                                                  DPRG > blanks
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANKS       Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *BLANKS       Q#PIDX            5
     C                   MOVEL(P)  *BLANKS       Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
     C*** END ADD *** 04/26/00 ****************************************
     C*
     C** UPDATE REQUEST PAYMENT FILE
     C*
     C*** BEGIN DLT *** 02/24/00 **************************************
     C********** RPKY      CHAINPRP01                41
     C*** END DLT ***** 02/24/00 **************************************
     C*** BEGIN CHG *** 03/05/01 **************************************
     C*******************  Z-ADDCMSEQ#    RPDSEQ
     C                   MOVE      Q#SEQ#        RPDSEQ
     C*** END CHG ***** 03/05/01 **************************************
     C                   MOVE      'Y'           RPSENT
     C*** BEGIN ADD *** 02/25/00 **************************************
     C                   Z-ADD     XXDATE        RPDT
     C*** BEGIN ADD *** 12/05/00 **************************************
     C                   TIME                    DSDTTM
     C*** END ADD ***** 12/05/00 **************************************
     C                   Z-ADD     DSTIME        RPTIME
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
     C*** END ADD ***** 02/25/00 **************************************
     C                   UPDATE    PRP01
     C*
     C     NODRT         TAG
     C**
     C                   ELSE
     C*
     C*  IF A DIARY HAS ALREADY BEEN SENT, CHECK THE DUE DATE.  IF THE
     C*  PAYMENT IS PAST DUE, UPDATE THE DIARY ITEM WITH '$$' IN THE
     C*  FIRST TWO POSITIONS OF THE COMMENT.
     C*
     C     RPDUE         IFLE      XXDATE
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   Z-ADD     XXCO#         QCCO#
     C                   Z-ADD     RPFND         QCFND
     C                   Z-ADD     RPFYR         QCFYR
     C                   Z-ADD     RPCASE        QCCASE
     C                   Z-ADD     RPDSEQ        XXSEQ#
     C     C2KY          CHAIN     PCM02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     '$$'          CMCOMM
     C                   UPDATE    PCM02
     C                   END
     C*
     C                   END                                                    *IN60
     C                   END                                                    RPSENT = N
     C*
     C     NEXT          TAG
     C*
     C*** BEGIN ADD *** 02/24/00 **************************************
     C                   ENDIF                                                  RPSTAT = 'P'
     C*** END ADD ***** 02/24/00 **************************************
     C*
     C*** BEGIN CHG *** 02/24/00 **************************************
     C********** RPKY1     READEPRP02                    40
     C     RPKY1         READE     PRP01                                  40
     C*** END CHG ***** 02/24/00 **************************************
     C                   ENDDO
     C*  ----------------------------------------------------------------
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*====================================================================*
     C* * *   SUBROUTINE FOR CHECKING USER TYPES, AUTHORITY              ***
     C*====================================================================*
     C     SRAUT         BEGSR
     C*
     C*** BEGIN ADD *** 04/09/99 **************************************
     C                   MOVE      '0'           *IN75
     C*** END ADD ***** 04/09/99 **************************************
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         CABEQ     '1'           NODRT
     C     CTDEPG        IFNE      *BLANKS
     C*
     C     UPKY          CHAIN     PUP02                              60
     C     *IN60         IFEQ      '0'
     C     OPKY          CHAIN     POP01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*** BEGIN DLT *** 04/09/99 **************************************
     C******************** MOVE '0'       *IN75
     C*** END DLT ***** 04/09/99 **************************************
     C*
     C*** BEGIN ADD *** 04/30/98 **************************************
     C*  Use SRCCSC to validate the user's authority against the
     C*    menu option
     C*
     C                   MOVE      U2LVL         SRULVL
     C                   MOVE      OPMINS        SRCLVL
     C                   CALL      'SRCCSC'
     C                   PARM      U2TYPE        SRUTYP            1            User Type
     C                   PARM                    SRULVL            3            User Level
     C                   PARM      OT(1)         SRCTYP            1            Client Type
     C                   PARM                    SRCLVL            3            Client Level
     C                   PARM      '1'           SREXEC            1            Execution Type
     C                   PARM                    SRRES             1            Result Flag
     C                   PARM      OT(2)         SRCTP2            1
     C                   PARM      OT(3)         SRCTP3            1
     C                   PARM      OT(4)         SRCTP4            1
     C                   PARM      OT(5)         SRCTP5            1
     C                   PARM      OT(6)         SRCTP6            1
     C*
     C     SRRES         IFEQ      '1'
     C                   MOVE      '1'           *IN75
     C                   END
     C*** END ADD ***** 04/30/98 **************************************
     C*
     C*** BEGIN DLT *** 04/30/98 **************************************
     C*  IF THE SECURITY TYPE FROM THE USER FILE IS NOT
     C*  "*" THEN CHECK THE TYPE, AND AUTHORITY
     C**********
     C********** U2TYPE    IFNE '*'
     C********** U2TYPE    ANDNE'#'
     C********** U2TYPE    ANDNE'$'
     C********** U2TYPE    ANDNE'&'
     C********** U2TYPE    ANDNE'%'
     C**********
     C*  LOOK FOR THE SECURITY TYPE, FROM THE OPTION
     C**********
     C********** U2TYPE    IFNE *BLANK
     C********** U2TYPE    LOKUPOT                       62
     C**N62*****           MOVE '1'       *IN75
     C**********
     C***62*****           DO
     C**********
     C*  THE VALUE FROM THE USER FILE MUST BE GREATER OR EQUAL
     C**********
     C********** U2LVL     IFLT OPMINS
     C**********           MOVE '1'       *IN75
     C**********           END
     C********** U2LVL     IFLT OPMMSC
     C**********           MOVE '1'       *IN75
     C**********           END
     C**********
     C**********           END                             END DO
     C**********
     C**********           ELSE
     C**********           MOVE '1'       *IN75
     C**********           END                             UPTYPE NE' '
     C**********
     C**********           ELSE
     C**********
     C*  CHECK THE SPECIAL TYPE CODES
     C**********
     C**********           MOVE *BLANK    XXSPEC  1
     C**********           MOVE *BLANK    OT
     C********** U2TYPE    IFNE '*'
     C**********
     C********** 1         DO   6         I2      50
     C********** OT,I2     IFNE *BLANK
     C**********
     C*  CALL SECURITY PGM AND ONLY CHECK THE SPECIAL TYPE CODES.
     C**********
     C**********           MOVE U2LVL     SRULVL
     C**********           MOVE U2LVL     SRCLVL
     C**********
     C**********           CALL 'SRCCSC'
     C**********           PARM U2TYPE    SRUTYP  1        User Type
     C**********           PARM           SRULVL  3        User Level
     C**********           PARM OT,I2     SRCTYP  1        Client Type
     C**********           PARM           SRCLVL  3        Client Level
     C**********           PARM '1'       SREXEC  1        Execution Type
     C**********           PARM           SRRES   1        Result Flag
     C**********
     C********** SRRES     IFEQ '0'
     C**********           MOVELU2TYPE    XXSPEC
     C**********           END
     C**********
     C**********           END                             OT,12 NE BLANK
     C**********           ENDDO
     C**********
     C********** XXSPEC    IFEQ ' '
     C**********           MOVE '1'       *IN75
     C**********           END
     C**********
     C********** U2LVL     IFLT OPMINS
     C**********           MOVE '1'       *IN75
     C**********           END
     C**********
     C********** U2LVL     IFLT OPMMSC
     C**********           MOVE '1'       *IN75
     C**********           END
     C**********
     C**********           END                             U2TYPE NE *
     C**********
     C**********           END                             U2TYPE NE *#$&%
     C*** END DLT ***** 04/30/98 **************************************
     C*
     C                   END                                                    *IN60 POP01
     C                   END                                                    *IN60 PUP02
     C                   END                                                    CTDEPG NE BLANK
     C*
     C                   ENDSR
