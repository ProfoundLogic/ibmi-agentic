     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H****************************************************************
     H/TITLE F8100R - PURGE WCMC MASTER FILES
     H****************************************************************
     H*  PROGRAM DESCRIPTION:                                        *
     H*       THIS PROGRAM WILL PURGE ALL RECORDS FOR THE SUB/FUND   *
     H*       MASTER FILES                                           *
     H*                                                              *
     H*  CALLED FROM:                                                *
     H*    F8100C - WCMC MASTER PURGE OVERRIDE CL                    *
     H*                                                              *
     H*  CALLS TO:                                                   *
     H*    NONE                                                      *
     H*                                                              *
     H*  PROGRAMMER: C:53                                            *
     H*  CREATED:    04/08/96    PROJECT: MC134                      *
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  08/15/00 C:62  PROJECT: MC215                                *
     H*                 REPLACE SFBPF REFERENCES WITH APIS.           *
     H*                                                              *
     H****************************************************************
     H* INDICATOR USAGE:                                             *
     H*                                                              *
     H* 08 = BATCH MODE                                              *
     H* 09 = DEMAND MODE                                             *
     H* 20 = DETERMINES KEY FOR DARTS                                *
     H* 30 = DETERMINES GLOBAL FUND DELETE                           *
     H* 60 = GENERAL CHAIN                                           *
     H*                                                              *
     H****************************************************************
     H*
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 **************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F*
     FFMFNL     UF   E           K DISK
     FFMCML1    UF   E           K DISK
     FFMEML     UF   E           K DISK
     FFMF1L     UF   E           K DISK
     FFMSML     UF   E           K DISK
     FFXLOP     UF   E           K DISK
     FSDCMLB    UF   E           K DISK
     F                                     RENAME(PCM01:PCM03)
     FSMARL1    UF   E           K DISK
     F                                     RENAME(PAR01:PAR02)
     FSMHDL     UF   E           K DISK
     FSMH1L     UF   E           K DISK
     F*
     FQSYSPRT   O    F  139        PRINTER OFLIND(*INOA)

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D* DARTS
     D                 DS
     D  QCKEY                  1     40
     D  XCCO#                  1      3
     D  XCSUB                  4      8
     D  XCFND                  9     11
     D  XCFYR                 12     14
     D  XCCASE                15     21
     D                 DS
     D  QWKEY                  1     40
     D  XWCO#                  1      3
     D  XWFND                  4      6
     D  XWFYR                  7      9
     D  XWCASE                10     16
     D*
     D*** BEGIN DEL *** 08/15/00 **************************************
     D**********  DS
     D**********                              1  10 FBAL01
     D**********                              4   4 XXAL4
     D*** END   DEL *** 08/15/00 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

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
     C*   KEY LIST FOR THE DARTS FILE
     C*
     C*  FS BASE CONTROL FILE
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** FBKY      KLIST
     C**********           KFLD           XSKEY   2
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    X@CO#             3
     C                   PARM                    X@SUB             5
     C                   PARM                    X@FND             3
     C                   PARM                    X@QUE            10
     C*** BEGIN ADD *** 08/15/00 ***********************************************
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C*** END   ADD *** 08/15/00 ***********************************************
     C*
     C     X@FND         COMP      '000'                                  30
     C*
     C                   MOVE      X@CO#         XXCO#             3 0
     C                   MOVE      X@SUB         XXSUB             5 0
     C                   MOVE      X@QUE         QQQUE            10
     C                   MOVE      X@CO#         XCCO#
     C                   MOVE      X@SUB         XCSUB
     C                   MOVE      X@CO#         XWCO#
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
     C                   Z-ADD     *ZERO         XXDREC            6 0
     C                   Z-ADD     *ZERO         XXMREC            6 0
     C                   Z-ADD     *ZERO         XXAREC            6 0
     C                   Z-ADD     *ZERO         XXBREC            6 0
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
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   CONTROL INFO
     C*
     C*** BEGIN DEL *** 08/15/00 ***********************************************
     C**********           MOVE *BLANKS   XSKEY
     C**********           MOVEL'FB'      XSKEY
     C********** FBKY      CHAINSFB01                60
     C*** END   DEL *** 08/15/00 ***********************************************
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
     C********           EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   END
     C                   MOVE      *ON           *INOF
     C*
     C                   EXCEPT    HEAD
     C*
     C*  READ/DELETE ALL RECORDS IN THE FILE FOR THE SELECTED SUB/FUND
     C*
     C*  FUND AUXILIARY FILE
     C*
     C  N30XXKEY         CHAIN     PF101                              60
     C   30XXKEYG        CHAIN     PF101                              60
     C*
     C*  DETERMINE WHICH DARTS KEY TO USE
     C*
     C  N60F1OUTA        IFEQ      'N'
     C*** BEGIN CHG *** 08/15/00 ***********************************************
     C********** XXAL4     COMP 'Y'                      20
     C     ##WCDK        COMP      'Y'                                    20
     C*** END   CHG *** 08/15/00 ***********************************************
     C                   ENDIF
     C*
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PF101
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PF101                                  60                R
     C   30XXKEYG        READE     PF101                                  60                R
     C                   ENDDO
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FMF1P'       XXFILE           10
     C                   EXCEPT    DET
     C*
     C*  FUND MASTER FILE
     C*
     C  N30XXKEY         CHAIN     PFN01                              60
     C   30XXKEYG        CHAIN     PFN01                              60
     C*
     C*  DELETE ALL THE DARTS RECORDS FOR THIS FUND
     C     *IN20         IFEQ      '1'
     C                   MOVE      FNFND         XWFND
     C                   MOVE      *BLANKS       XWFYR
     C                   MOVE      *BLANKS       XWCASE
     C                   ELSE
     C                   MOVE      FNFND         XCFND
     C                   MOVE      *BLANKS       XCFYR
     C                   MOVE      *BLANKS       XCCASE
     C                   ENDIF
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C*  DARTS MAJOR REMARKS BY FUND
     C*
     C                   EXSR      DARTS
     C*
     C* DELETE THE FUND RECORD
     C*
     C                   DELETE    PFN01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PFN01                                  60                R
     C   30XXKEYG        READE     PFN01                                  60                R
     C                   ENDDO
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FMFNP'       XXFILE           10
     C                   EXCEPT    DET
     C*
     C*  SUBSCRIBER MASTER FILE
     C*
     C     XXKEYG        CHAIN     PSM01                              60        NO FND #
     C*
     C*  DELETE ALL THE DARTS RECORDS FOR THIS SUBSCRIBER
     C     *IN20         IFEQ      '1'
     C                   MOVE      *BLANKS       XWFND
     C                   MOVE      *BLANKS       XWFYR
     C                   MOVE      *BLANKS       XWCASE
     C                   ELSE
     C                   MOVE      *BLANKS       XCFND
     C                   MOVE      *BLANKS       XCFYR
     C                   MOVE      *BLANKS       XCCASE
     C                   ENDIF
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C*  DARTS MAJOR REMARKS BY SUBSCRIBER ONLY IF GLOBAL DELETE
     C*
     C   30              EXSR      DARTS
     C*
     C* DELETE THE SUBSCRIBER RECORD
     C*
     C   30              DELETE    PSM01
     C   30              ADD       1             XX#REC
     C   30              ADD       1             XXTREC
     C     XXKEYG        READE     PSM01                                  60                R
     C                   ENDDO
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FMSMP'       XXFILE           10
     C                   EXCEPT    DET
     C*
     C*  CLAIM MASTER FILE
     C*
     C  N30XXKEY         CHAIN     PCM01                              60
     C   30XXKEYG        CHAIN     PCM01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C*  DELETE ALL THE DARTS RECORDS FOR THIS CLAIM
     C     *IN20         IFEQ      '1'
     C                   MOVE      CMFND         XWFND
     C     CMFYR         IFEQ      *ZEROS
     C                   MOVE      *BLANKS       XWFYR
     C                   ELSE
     C                   MOVE      CMFYR         XWFYR
     C                   ENDIF
     C     CMCASE        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       XWCASE
     C                   ELSE
     C                   MOVE      CMCASE        XWCASE
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVE      CMFND         XCFND
     C     CMFYR         IFEQ      *ZEROS
     C                   MOVE      *BLANKS       XCFYR
     C                   ELSE
     C                   MOVE      CMFYR         XCFYR
     C                   ENDIF
     C     CMCASE        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       XCCASE
     C                   ELSE
     C                   MOVE      CMCASE        XCCASE
     C                   ENDIF
     C                   ENDIF
     C*
     C*  DARTS MAJOR REMARKS BY CLAIM
     C*
     C                   EXSR      DARTS
     C*
     C* DELETE THE CLAIM RECORD
     C*
     C                   DELETE    PCM01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PCM01                                  60                R
     C   30XXKEYG        READE     PCM01                                  60                R
     C                   ENDDO
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FMCMP'       XXFILE           10
     C                   EXCEPT    DET
     C*
     C*  PRINT THE DART MAJOR REMARK DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XXDREC        XX#REC
     C                   MOVEL     'SDCMP'       XXFILE           10
     C                   MOVEL     'CLAIM'       XXDESC           10
     C                   EXCEPT    DET
     C*
     C*  PRINT THE DART ADDITIONAL REMARK DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XXMREC        XX#REC
     C                   MOVEL     'SMARP'       XXFILE           10
     C                   MOVEL     'CLAIM'       XXDESC           10
     C                   EXCEPT    DET
     C*
     C*  PRINT THE DARTS CUSTOM GROUP ID FORMAT A DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XXAREC        XX#REC
     C                   MOVEL     'SMHDP'       XXFILE           10
     C                   MOVEL     'CLAIM'       XXDESC           10
     C                   EXCEPT    DET
     C*
     C*  PRINT THE DARTS CUSTOM GROUP ID FORMAT B DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XXBREC        XX#REC
     C                   MOVEL     'SMH1P'       XXFILE           10
     C                   MOVEL     'CLAIM'       XXDESC           10
     C                   EXCEPT    DET
     C*
     C*  EMPLOYER MASTER
     C*
     C  N30XXKEY         CHAIN     PEM01                              60
     C   30XXKEYG        CHAIN     PEM01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEM01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PEM01                                  60                R
     C   30XXKEYG        READE     PEM01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FMEMP'       XXFILE           10
     C                   EXCEPT    DET
     C*
     C*  CASE LOCK-OUT FILE
     C*
     C  N30XXKEY         CHAIN     PLO01                              60
     C   30XXKEYG        CHAIN     PLO01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PLO01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PLO01                                  60                R
     C   30XXKEYG        READE     PLO01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FXLOP'       XXFILE           10
     C                   EXCEPT    DET
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
     C     DARTS         BEGSR
     C*
     C  N20QCKEY         CHAIN     PCM03                              62
     C   20QWKEY         CHAIN     PCM03                              62
     C*
     C     *IN62         DOWEQ     '0'
     C                   DELETE    PCM03
     C                   ADD       1             XXDREC
     C                   ADD       1             XXTREC
     C  N20QCKEY         READE     PCM03                                  62                R
     C   20QWKEY         READE     PCM03                                  62                R
     C                   END
     C*
     C*  DARTS ADDITIONAL REMARKS BY CLAIM
     C*
     C  N20QCKEY         CHAIN     PAR02                              63
     C   20QWKEY         CHAIN     PAR02                              63
     C     *IN63         DOWEQ     '0'
     C                   DELETE    PAR02
     C                   ADD       1             XXMREC
     C                   ADD       1             XXTREC
     C  N20QCKEY         READE     PAR02                                  63                R
     C   20QWKEY         READE     PAR02                                  63                R
     C                   END
     C*
     C*  CUSTOM GROUP ID DATA FILE - FORMAT A BY CLAIM
     C*
     C  N20QCKEY         CHAIN     PHD01                              64
     C   20QWKEY         CHAIN     PHD01                              64
     C     *IN64         DOWEQ     '0'
     C                   DELETE    PHD01
     C                   ADD       1             XXAREC
     C                   ADD       1             XXTREC
     C  N20QCKEY         READE     PHD01                                  64                R
     C   20QWKEY         READE     PHD01                                  64                R
     C                   END
     C*
     C*  CUSTOM GROUP ID DATA FILE - FORMAT B BY CLAIM
     C*
     C  N20QCKEY         CHAIN     PH101                              65
     C   20QWKEY         CHAIN     PH101                              65
     C     *IN65         DOWEQ     '0'
     C                   DELETE    PH101
     C                   ADD       1             XXBREC
     C                   ADD       1             XXTREC
     C  N20QCKEY         READE     PH101                                  65                R
     C   20QWKEY         READE     PH101                                  65                R
     C                   END
     C*
     C                   ENDSR
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
