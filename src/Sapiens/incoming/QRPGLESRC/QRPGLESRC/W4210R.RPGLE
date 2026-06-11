     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W4210R - CHECK RECONCILIATION DETAIL FILE DAILY UPDATE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  05/23/00 C:15 Project: WC577                                 *
     H*                Changed pgm to correctly identify the original *
     H*                check rec record when a reverse is processed.  *
     H*                                                               *
     H*  10/28/99 C:15 Support: WC025/20362                           *
     H*                Changed pgm to prevent array index error if    *
     H*                more than 9999 group checks are processed      *
     H*                and increase efficiency.                       *
     H*                                                               *
     H*  08/13/99 C:15 Support: WC033/19159                           *
     H*                Changed pgm to load the correct activity date  *
     H*                for a group check V, S or Z.                   *
     H*                                                               *
     H*  10/30/97 C:30 Project: WC415                                 *
     H*           Write 1 WDCRP record for Group Checks.              *
     H*                                                               *
     H*  04/11/97 C:47 PROJECT WC462                                  *
     H*                GROUP CHECK -STOP/VOID/REVERSAL PROCESSING     *
     H*                                                               *
     H*  09/03/97 C:30 Project WC659                                  *
     H*           Remove Fund Aux file usage.  Add Bank Acct Master   *
     H*           file.  Assign Check Rec Upd 'Y' Bank Accts into an  *
     H*           array.  While processing Processed Payments, if     *
     H*           Bank Acct is in array, write/update Check Rec.      *
     H*           Assign Fund to Check Rec field prior to WRT/UPD.    *
     H* _____________________________________________________________ *
     H*
     H*   07/11/94 C:22 PROJECT WC353
     H*     MODIFIED TO CHECK THAT THE BANK ACCOUNT NUMBER AND CHECK
     H*     NUMBER ARE THE SAME WHEN A HIT IS MADE IN THE CHECK REC
     H*     FILE (WDCRP)
     H*
     H*   09/08/94 C:22 PROJECT WC363
     H*     ADDED A VERIFICATION ROUTINE TO CHECK FOR ANY OCCURENCE
     H*     OF THE KEY FOR ALL TYPES OF CHECKS. REVERSALS AND RE-START
     H*     WERE THE CONSIDERATION.
     H*
     H*   08/16/95 C:36 PROJECT WC444                                 *
     H*                 ADD FLAG FOR "TRANSFER TO" CHECK              *
     H*                 RECONCILIATION                                *
     H* ************************************************************
     FWDCRL     UF A E           K DISK
     FWDPAL2    IF   E           K DISK
      *** BEGIN DEL *** 09/03/97 **************************************
     F***WDF2L   IF  E           K        DISK
      *** END   DEL *** 09/03/97 **************************************
     FWMPDL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     F*** BEGIN ADD *** 08/16/95 **************************************
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)
     F*** END ADD ***** 08/16/95 **************************************
      *** BEGIN ADD *** 04/11/97 **************************************
     FWAGCL     IF   E           K DISK
     FWDCRL7    UF A E           K DISK
     F                                     RENAME(PCR01:PCR07)
      *** END ADD ***** 04/11/97 **************************************
      *** BEGIN ADD *** 10/30/97 **************************************
      *  Payment Detail file
     FWDPAL9    IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
      *
      *** END   ADD *** 10/30/97 **************************************
      *** BEGIN ADD *** 09/03/97 **************************************
      *  Bank Account Master file
     FWSCKL     IF   E           K DISK
      *

     D BNK             S             20    DIM(200)                             Bank Accounts
      *** END   ADD *** 09/03/97 **************************************
      *** BEGIN ADD *** 10/30/97 **************************************
     D CKAR            S              7  0 DIM(9999) ASCEND                     Check Array
      *
      *** END   ADD *** 10/30/97 **************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     IPPA02
     I              PACO#                       PACO2
     I              PACHK#                      P2CHK#
     I              PAAMT                       P2AMT
     I              PATRDT                      P2TRDT
     I              PAPGTP                      P2PGTP

     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C*
     C*  KEY LIST FOR CHECK DETAIL FILE
     C*
     C     CRKEY         KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    XXKEY            15
     C*** BEGIN ADD *** 04/11/97 **************************************
     C*
     C     CRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    GCCHK#
     C                   KFLD                    GCTRDT
     C                   KFLD                    GCAMT
     C*
     C*  KEY LIST FOR GROUP CHECK PENDING FILE
     C*
     C     GCKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*** END ADD ***** 04/11/97 **************************************
     C*
     C*   KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C     F2KEY         KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C*
     C*   KEY LIST FOR THE PAYMENT FILE
     C*
     C     PAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C*** BEGIN ADD *** 08/16/95 **************************************
     C*** BEGIN ADD *** 10/30/97 **************************************
      *
      *   Payment Detail file
      *
     C     P2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C                   KFLD                    SVCHK#
      *
     C*** END   ADD *** 10/30/97 **************************************
     C*
     C*  PAYMENT CODE TABLE FILE
     C*
     C     PCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    PCKEY
     C*** END ADD ***** 08/16/95 **************************************
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    PAFEIN
     C                   KFLD                    PAMOD
     C*
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
      *** BEGIN ADD *** 10/30/97 **************************************
     C                   PARM                    QQTYPE            1
      *
     C     *LIKE         DEFINE    PACHK#        SVCHK#
      *** END   ADD *** 10/30/97 **************************************
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
     C     *LIKE         DEFINE    PACO#         XXCO#
     C     *LIKE         DEFINE    PADT          XXDT
     C     *LIKE         DEFINE    PADT          XXSVDT
     C     *LIKE         DEFINE    PADT          XXRVDT
     C     *LIKE         DEFINE    PADT          XXRVTR
     C     *LIKE         DEFINE    PAACCT        XXACCT
     C*
      *** BEGIN ADD *** 10/30/97 **************************************
     C                   Z-ADD     *ZEROS        SVCHK#
     C                   Z-ADD     *ZEROS        XXGPAM            9 2
     C                   Z-ADD     1             X                 7 0
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVE      *OFF          *IN45
      *** END   ADD *** 10/30/97 **************************************
      *** BEGIN ADD *** 09/03/97 **************************************
     C                   EXSR      SRBNK                                        Load Bank ACCTs
      *** END   ADD *** 09/03/97 **************************************
      *
     C                   TIME                    DSDTTM
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQDATE        XXDT8             8 0
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*** BEGIN ADD *** 04/11/97 **************************************
     C*
     C*   CALL THE SUBROUTINE TO PROCESS THE GROUP CHECK
     C*   STOP/VOID/REVERSAL
     C*
     C                   EXSR      GRPCHK
     C*
     C*** END ADD ***** 04/11/97 **************************************
     C*
     C*   START READING PAYMENT FILE
     C*
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         DOWEQ     '0'
     C*** BEGIN ADD *** 08/16/95 **************************************
     C*
     C*  CHECK TRANSFER TO CHECK REC FLAG
     C*
     C                   MOVE      *BLANKS       PCKEY
     C                   MOVEL     PATRCD        PCKEY
     C     PCKY1         CHAIN     PPC01                              60
     C     *IN60         IFEQ      '0'
     C     PCCKTR        ANDEQ     'Y'
     C*** END ADD ***** 08/16/95 **************************************
     C*
      *** BEGIN DEL *** 09/03/97 **************************************
     C****UPDATE FILE ONLY IF THERE'S A BANK ACCT# IN FUND AUXILIARY
     C*****
     C*****      F2KEY     CHAINPF201                60
     C*****      *IN60     IFEQ '0'
     C*****      F2ACCT    ANDNE*BLANKS
      *** END   DEL *** 09/03/97 **************************************
      *** BEGIN ADD *** 09/03/97 **************************************
      *
      *  Using the Payment Detail Account, determine if account is in
      *  the BNK array of "valid" Check Rec Accounts.
      *
     C                   MOVE      *OFF          *IN44
     C     PAACCT        IFNE      *BLANK
     C                   Z-ADD     1             Y                 3 0
     C     PAACCT        LOOKUP    BNK(Y)                                 44
     C                   ENDIF
      *
     C     *IN44         IFEQ      *ON
      *
      *** END   ADD *** 09/03/97 **************************************
      *** BEGIN ADD *** 10/30/97 **************************************
      * IF THE PAYMENT WAS PART OF A GROUP TYPE CHECK THE FOLLOWING
      * OCCURS:
      *    1. AN ARRAY LOOK UP WILL OCCUR TO DETERMINE IF THE CHECK
      *       HAS ALREADY BEEN PROCESSED.
      *    2. IF THE CHECK HAS NOT BEEN PROCESSED THEN USING A ACCESS
      *       OF CO#/TRDT/CHK# ALL AMOUNTS FOR THIS CHECK ARE
      *       ACCUMULATED.
      *    3. WHEN THE CHECK # BREAK OCCURS THEN THE SINGLE CHECK
      *       RECORD IS WRITTEN TO THE CHECK REC FILE, ONLY IF THE
      *       CHECK RECORD HASN'T BEEN WRITTEN BEFORE.
      *    4. THE CHECK # IS PLACED IN THE ARRAY (CKAR). THIS WILL
      *       PREVENT THE ACCUMULATION OF TOTALS FOR THIS CHECK
      *       OCCURING AGAIN FOR THE MULTIPLE CHECK ENTRIES IN THE
      *       PAYMENT DETAIL FILE.
      *
     C     QQTYPE        IFEQ      'G'
      *
     C     PAPGTP        IFEQ      'G'
     C     PACHK#        LOOKUP    CKAR                                   26
      *
     C     *IN26         IFEQ      '0'
      *
      *  Check # Not Found in Array
      *
     C                   MOVE      PACHK#        SVCHK#
     C     P2KEY         CHAIN     PPA02                              69
      *
      *  Accumulate ALL WDPAP same Check# Group Payments
      *
     C     *IN69         DOWEQ     '0'
     C     P2CHK#        IFEQ      SVCHK#
     C     P2PGTP        ANDEQ     'G'
     C                   ADD       P2AMT         XXGPAM
     C                   ENDIF                                                  *IN69
     C     P2KEY         READE     PPA02                                  69
     C                   ENDDO                                                  P2PGTP EQ 'G'
      *
     C     CRKEY         CHAIN     PCR01                              68
      *
     C     *IN68         DOWEQ     '0'
     C     CRCHK#        IFEQ      PACHK#
     C     CRCKAM        ANDEQ     XXGPAM
     C     CRCKDT        ANDEQ     PAPODT
     C                   MOVE      *ON           *IN45
     C                   ENDIF                                                  CRCHK# EQ PACHK#
     C     CRKEY         READE     PCR01                                  68
     C                   ENDDO                                                  *IN60 EQ *OFF
      *
     C*** BEGIN ADD *** 10/28/99 **************************************
     C     *IN45         IFEQ      *OFF
     C*** END ADD ***** 10/28/99 **************************************
     C                   Z-ADD     PACO#         CRCO#
     C                   Z-ADD     PAFND         CRFND
     C                   MOVEL     PAACCT        CRACCT
     C                   Z-ADD     PACHK#        CRCHK#
     C                   Z-ADD     PAPODT        CRCKDT
     C                   Z-ADD     XXGPAM        CRCKAM
     C                   MOVE      XHKEY         CRKY
      *
     C     PATYPE        IFEQ      'M'
     C     PATYPE        OREQ      'P'
     C                   MOVE      'C'           CRRTP
     C                   ELSE
     C     PATYPE        IFEQ      'R'
     C                   MOVE      'R'           CRRTP
     C                   ELSE
     C                   MOVE      'Z'           CRRTP
     C                   ENDIF                                                  PATYPE EQ 'R'
     C                   ENDIF                                                  PATYPE EQ 'M' 'P' 'C
      *
     C     PATYPE        IFNE      'Z'
     C                   Z-ADD     0             CRACDT
     C                   MOVE      *BLANKS       CRACTP
     C                   ELSE
     C                   Z-ADD     XXRVTR        CRACDT
     C                   MOVE      'X'           CRACTP
     C                   ENDIF                                                  PATYPE NE 'Z'
      *
     C                   Z-ADD     0             CRRDT1
     C                   Z-ADD     0             CRRDT2
     C                   MOVE      *BLANKS       CRDESC
     C     CMKEY         CHAIN     PCM01                              60
     C                   MOVE      PAFEIN        XXSS#             9
      *
     C     PAFEIN        IFEQ      0
     C     XXSS#         OREQ      CMSS#
      *
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDFSN         XXFRST
     C                   MOVEL     CDLSN         XXLAST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        CRDESC
     C                   ENDIF                                                  *IN60 EQ *OFF
      *
     C                   ELSE
      *
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PDNAME        CRDESC
     C                   ENDIF                                                  *IN60 EQ *OFF
     C                   ENDIF                                                  PAFEIN
      *
     C                   MOVE      'N'           CRRVFL
     C                   Z-ADD     PABUL1        CRBUL1
     C                   MOVE      *BLANKS       CRSTAT
     C                   MOVEL     Q1USER        CRUSER
     C                   MOVEL     Q1PGM         CRPGM
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        CRTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRDT
      *
     C                   MOVE      XXRVTP        CRRVTP
     C                   Z-ADD     XXRVDT        CRRVDT
     C                   MOVE      XXSVST        CRSVST
     C                   Z-ADD     XXSVDT        CRSVDT
     C                   Z-ADD     XXRVTR        CRRVTR
      *
     C*** BEGIN DLT *** 10/28/99 **************************************
     C********** *IN45     IFEQ *OFF
     C*** END DLT ***** 10/28/99 **************************************
     C                   WRITE     PCR01
     C                   ENDIF                                                  *IN45 EQ *OFF
     C*** BEGIN ADD *** 10/28/99 **************************************
     C                   MOVE      PACHK#        CKAR(X)
     C                   ADD       1             X
     C*** END ADD ***** 10/28/99 **************************************
     C                   ENDIF                                                  *IN26 EQ *OFF
      *
     C                   Z-ADD     *ZEROS        SVCHK#
     C                   Z-ADD     *ZEROS        XXGPAM
     C*** BEGIN DLT *** 10/28/99 **************************************
     C******************** MOVE PACHK#    CKAR,X
     C******************** ADD  1         X
     C*** END DLT ***** 10/28/99 **************************************
     C                   MOVE      *OFF          *IN45
     C                   ENDIF                                                  PAPGTP <> 'G'
      *
     C                   ELSE                                                   QQTYPE <> 'G'
      *
      * NORMAL CHECK REC PROCESSING FOR NON-GROUP TYPE PAYMENTS OCCURS:
      *
     C     PAPGTP        IFNE      'G'
      *
      *** END   ADD *** 10/30/97 **************************************
     C*
     C*   WRITE PAYMENTS, MANUAL PAYMENTS, REFUNDS, AND REVERSALS
     C*
     C     PATYPE        IFEQ      'M'
     C     PATYPE        OREQ      'P'
     C     PATYPE        OREQ      'R'
     C     PATYPE        OREQ      'Z'
     C*
     C*   CHECK IF RECORD ALREADY EXISTS IN DETAIL FILE - USE PASEQ#
     C*
     C                   MOVEL     PACASE        XXCSSQ           12
     C                   MOVE      PASEQ#        XXCSSQ
     C                   MOVE      XXCSSQ        XXKEY
     C                   MOVEL     PAFYR         XXKEY
     C*
     C*   HOLD THE KEY FOR LATER MOVE TO FILE
     C*
     C                   MOVE      XXKEY         XHKEY            15
     C*** BEGIN ADD 09/08/94 ***************************************
     C                   EXSR      VER
     C*
     C     XXHIT         IFEQ      'N'
     C*** END ADD 09/08/94 *****************************************
     C*
     C*   IF REVERSAL, GET THE VOIDED CHECK & MOVE ITS ACTIVITY INFO
     C*   TO SAVE FIELDS FOR LATER MOVE TO FILE. INTIALIZE EACH SAVE
     C*   FIELD TO BLANKS
     C*
     C                   Z-ADD     0             XXRVTR
     C                   MOVE      *BLANKS       XXRVTP            1
     C                   Z-ADD     0             XXRVDT
     C                   MOVE      *BLANKS       XXSVST            1
     C                   Z-ADD     0             XXSVDT
     C                   MOVE      *BLANKS       XXACCT
     C*
     C     PATYPE        IFEQ      'Z'
     C                   Z-ADD     PATRDT        XXRVTR
     C                   MOVEL     PACASE        XXCSSQ
     C                   MOVE      PABASQ        XXCSSQ
     C                   MOVE      XXCSSQ        XXKEY
     C                   MOVEL     PAFYR         XXKEY
     C*** BEGIN ADD 09/08/94 *******************************************
     C                   MOVE      *OFF          *IN25
     C*** END ADD 09/08/94 *********************************************
     C     CRKEY         CHAIN     PCR01                              60
     C*** BEGIN ADD 09/08/94 *******************************************
     C     *IN60         DOWEQ     '0'
     C     PAAMT         IFEQ      CRCKAM
     C     PACHK#        ANDEQ     CRCHK#
     C*** BEGIN CHG *** 05/23/00 ***************************************
     C********** PATYPE    ANDEQ'C'
     C     CRRTP         ANDEQ     'C'
     C*** END CHG ***** 05/23/00 ***************************************
      *** BEGIN CHG *** 09/03/97 ***************************************
     C***********CRACCT    ANDEQF2ACCT
     C     CRACCT        ANDEQ     PAACCT
      *** END   CHG *** 09/03/97 ***************************************
     C                   MOVE      *ON           *IN25
     C                   GOTO      EXLP
     C                   END
     C     CRKEY         READE     PCR01                                  60
     C                   ENDDO
      *
     C     EXLP          TAG
     C     *IN25         IFEQ      *ON
     C*** END ADD 09/08/94 *********************************************
     C                   MOVE      CRACCT        XXACCT
     C                   MOVE      CRACTP        XXRVTP
     C                   Z-ADD     CRACDT        XXRVDT
     C*  REVISED 6-29-89 - NEED BLANK STATUS IN ORDER TO FREE CHECK FOR
     C*  CLEARING OR MANUAL VOID/STOP.
     C                   MOVE      *BLANKS       CRSTAT
     C                   MOVE      CRSTAT        XXSVST
     C     CRRDT2        IFNE      0
     C                   Z-ADD     CRRDT2        XXSVDT
     C                   ELSE
     C                   Z-ADD     CRRDT1        XXSVDT
     C                   END
     C                   UPDATE    PCR01
     C                   END
     C                   END
     C*
     C*   MOVE FIELDS FOR INITIAL RECORED
     C*
     C                   Z-ADD     PACO#         CRCO#
     C     XXACCT        IFNE      *BLANKS
     C                   MOVE      XXACCT        CRACCT
     C                   ELSE
      *** BEGIN CHG *** 09/03/97 ***************************************
     C*********************MOVE F2ACCT    CRACCT
     C                   MOVEL     PAACCT        CRACCT
      *** END   CHG *** 09/03/97 ***************************************
     C                   END
     C                   Z-ADD     PACHK#        CRCHK#
     C                   Z-ADD     PAPODT        CRCKDT
     C                   Z-ADD     PAAMT         CRCKAM
     C                   MOVE      XHKEY         CRKY
     C     PATYPE        IFEQ      'M'
     C     PATYPE        OREQ      'P'
     C                   MOVE      'C'           CRRTP
     C                   ELSE
     C     PATYPE        IFEQ      'R'
     C                   MOVE      'R'           CRRTP
     C                   ELSE
     C                   MOVE      'Z'           CRRTP
     C                   END
     C                   END
     C     PATYPE        IFNE      'Z'
     C                   Z-ADD     0             CRACDT
     C                   MOVE      *BLANKS       CRACTP
     C                   ELSE
     C                   Z-ADD     XXRVTR        CRACDT
     C                   MOVE      'X'           CRACTP
     C                   END
     C                   Z-ADD     0             CRRDT1
     C                   Z-ADD     0             CRRDT2
     C                   MOVE      *BLANKS       CRDESC
     C     CMKEY         CHAIN     PCM01                              60
     C                   MOVE      PAFEIN        XXSS#
     C     PAFEIN        IFEQ      0
     C     XXSS#         OREQ      CMSS#
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDFSN         XXFRST
     C                   MOVEL     CDLSN         XXLAST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        CRDESC
     C                   END
     C*
     C                   ELSE
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PDNAME        CRDESC
     C                   END
     C*
     C                   END
     C                   MOVE      'N'           CRRVFL
     C                   Z-ADD     PABUL1        CRBUL1
     C                   MOVE      *BLANKS       CRSTAT
     C                   MOVEL     Q1USER        CRUSER
     C                   MOVEL     Q1PGM         CRPGM
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        CRTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRDT
     C                   MOVE      XXRVTP        CRRVTP
     C                   Z-ADD     XXRVDT        CRRVDT
     C                   MOVE      XXSVST        CRSVST
     C                   Z-ADD     XXSVDT        CRSVDT
     C                   Z-ADD     XXRVTR        CRRVTR
      *** BEGIN ADD *** 09/03/97 ***********************************************
     C                   Z-ADD     PAFND         CRFND                          WCMS Group
      *** END   ADD *** 09/03/97 ***********************************************
     C                   WRITE     PCR01
     C                   END
     C*
     C                   END
     C*
     C     PATYPE        IFEQ      'S'
     C     PATYPE        OREQ      'V'
     C*
     C*   UPDATE EXISTING RECORD - USE PABASQ IN KEY
     C*
     C                   MOVEL     PACASE        XXCSSQ
     C                   MOVE      PABASQ        XXCSSQ
     C                   MOVE      XXCSSQ        XXKEY
     C                   MOVEL     PAFYR         XXKEY
     C     CRKEY         CHAIN     PCR01                              60
     C*** BEGIN CHANGE 07/12/94 **********************************
     C*          *IN60     IFEQ '0'
     C     *IN60         DOWEQ     '0'
     C*** END CHANGE 07/12/94 ************************************
     C*** BEGIN ADD 07/11/94 *************************************
      *** BEGIN CHG *** 09/03/97 ***************************************
     C***********F2ACCT    IFEQ CRACCT
      *
     C     PAACCT        IFEQ      CRACCT
      *** END   CHG *** 09/03/97 ***************************************
     C     PACHK#        ANDEQ     CRCHK#
     C*** BEGIN ADD 09/08/94 *************************************
     C     PAAMT         ANDEQ     CRCKAM
     C*** END ADD 07/11/94 ***************************************
     C*
     C                   MOVE      'N'           CRRVFL
     C                   MOVE      PATYPE        CRACTP
     C                   Z-ADD     PATRDT        CRACDT
     C                   Z-ADD     PABUL1        CRBUL1
     C                   MOVEL     Q1USER        CRUSER
     C                   MOVEL     Q1PGM         CRPGM
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        CRTIME
     C                   Z-ADD     XXDT          SRDTI
      *** BEGIN ADD *** 09/03/97 ***********************************************
     C                   Z-ADD     PAFND         CRFND                          WCMS Group
      *** END   ADD *** 09/03/97 ***********************************************
     C                   UPDATE    PCR01
     C*** BEGIN ADD 07/12/94 **************************************
     C                   END
     C     CRKEY         READE     PCR01                                  60
     C*                    ENDDO
     C*** END ADD 07/12/94 ****************************************
     C*
     C                   END
     C                   END
     C*
     C                   END
      *** BEGIN ADD *** 10/30/97 ******************************************
     C                   ENDIF                                                  PAPGTP NE 'G'
     C                   ENDIF                                                  QQTYPE EQ 'G'
      *** END   ADD *** 10/30/97 ******************************************
     C                   END                                                    PCCKTR FLAG
     C*** END ADD ***** 08/16/95 **************************************
     C     PAKEY         READE     PPA01                                  60
     C                   ENDDO
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
      *** BEGIN ADD *** 09/03/97 *************************************
      ****************************************************************
      *  SRBNK - Load array BNK with Accounts with Check Rec 'Y'
      ****************************************************************
     C     SRBNK         BEGSR
      *
     C                   Z-ADD     *ZERO         Y
     C                   MOVEA     *BLANK        BNK
     C                   READ      PCK01                                  60
      *
      *  Read WSCKP recs and assign only Accounts with Check Rec Upd = Y
      *
     C     *IN60         DOWEQ     *OFF
     C     CKCRUP        IFEQ      'Y'
     C                   ADD       1             Y
     C                   MOVE      CKACCT        BNK(Y)
     C                   ENDIF
      *
     C                   READ      PCK01                                  60
      *
     C                   ENDDO
      *
     C                   ENDSR
      *** END   ADD *** 09/03/97 *************************************
     C*
     C*** BEGIN ADD 09/08/94 *****************************************
     C* **************************************************************
     C* VERIFICATION SUBROUTINE: THIS SUBROUTINE WILL READ THE CHECK
     C* RECONCILIATION FILE UNTIL THE CORRECT RECORD IS FOUND OR AN
     C* END OF FILE OCCURS. XXHIT WILL EQUAL 'Y' IF THE RECORD IS FOUND,
     C* AND THE PAYMENT DETAIL RECORD WILL NOT BE WRITTEN. XXHIT WILL
     C* EQUAL 'N' IF AN EOF OCCURS, THEN THE PAYMENT DETAIL RECORD WILL
     C* BE WRITTEN.
     C* **************************************************************
     C     VER           BEGSR
     C*
     C                   MOVE      'N'           XXHIT             1
     C     CRKEY         CHAIN     PCR01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C                   MOVE      'C'           XXCMP             1
     C                   END
     C     PATYPE        IFEQ      'R'
     C                   MOVE      'R'           XXCMP
     C                   END
     C     PATYPE        IFEQ      'Z'
     C                   MOVE      'Z'           XXCMP
     C                   END
     C*
     C     PAAMT         IFEQ      CRCKAM
     C     PACHK#        ANDEQ     CRCHK#
     C     PATRDT        ANDEQ     CRCKDT
     C     XXCMP         ANDEQ     CRRTP
     C     XXKEY         ANDEQ     CRKY
     C                   MOVE      'Y'           XXHIT
     C                   END
     C*
     C     CRKEY         READE     PCR01                                  60
     C                   ENDDO
     C                   ENDSR
     C*** END ADD 09/08/94 ********************************************
     C*** BEGIN ADD *** 04/11/97 **************************************
     C*****************************************************************
     C*  THIS SUBROUTINE WILL PROCESS THE GROUP CHECK                 *
     C*  STOP/VOID/REVERSAL                                           *
     C*****************************************************************
     C     GRPCHK        BEGSR
     C*
     C*  READ THE PENDING GROUP CHECK STOP/VOID/REVERSAL FILE AND
     C*  MAKE THE APPRPRIATE CHANGES TO THE CHECK REC FILE
     C*
     C     GCKEY         READE     PGC01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     CRKY1         CHAIN     PCR07                              60
     C     *IN60         IFEQ      *OFF
     C     GCTRTY        IFEQ      'S'
     C     GCTRTY        OREQ      'V'
     C                   MOVE      'N'           CRRVFL
     C                   MOVE      GCTRTY        CRACTP
     C*** BEGIN CHG *** 08/13/99 **************************************
     C******************** Z-ADDGCTRDT    CRACDT
     C                   Z-ADD     GCDT          CRACDT
     C*** END CHG ***** 08/13/99 **************************************
     C                   MOVEL     Q1USER        CRUSER
     C                   MOVEL     Q1PGM         CRPGM
     C                   Z-ADD     DSTIME        CRTIME
     C                   Z-ADD     XXDT          CRDT
     C                   UPDATE    PCR07
     C*
     C                   ELSE
     C*
     C     GCTRTY        IFEQ      'Z'
     C                   Z-ADD     0             XXSVDT
     C                   MOVE      *BLANKS       CRSTAT
     C                   MOVEL     Q1USER        CRUSER
     C                   MOVEL     Q1PGM         CRPGM
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        CRTIME
     C                   Z-ADD     XXDT          CRDT
     C                   UPDATE    PCR07
     C*
     C                   Z-ADD     GCTRDT        CRRVTR
     C                   MOVE      CRSTAT        XXSVST
     C*
     C     CRRDT2        IFNE      *ZERO
     C                   Z-ADD     CRRDT2        XXSVDT
     C                   ELSE
     C                   Z-ADD     CRRDT1        XXSVDT
     C                   END
     C*
     C*** BEGIN CHG *** 08/13/99 **************************************
     C******************** Z-ADDGCTRDT    CRACDT
     C                   Z-ADD     GCDT          CRACDT
     C*** END CHG ***** 08/13/99 **************************************
     C                   MOVE      'Z'           CRRTP
     C                   MOVE      'X'           CRACTP
     C                   MOVE      'N'           CRRVFL
     C                   MOVE      *BLANKS       CRSTAT
     C                   Z-ADD     *ZERO         CRRDT1
     C                   Z-ADD     *ZERO         CRRDT2
     C                   MOVEL     Q1USER        CRUSER
     C                   MOVEL     Q1PGM         CRPGM
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        CRTIME
     C                   Z-ADD     XXDT          CRDT
     C                   MOVE      CRACTP        CRRVTP
     C                   Z-ADD     CRACDT        CRRVDT
     C                   MOVE      CRSTAT        CRSVST
     C                   Z-ADD     XXSVDT        CRSVDT
     C                   WRITE     PCR07
     C                   END
     C                   END
     C                   END
     C*
     C     GCKEY         READE     PGC01                                  60
     C                   ENDDO
     C                   ENDSR
