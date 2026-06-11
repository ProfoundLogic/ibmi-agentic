     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W7000R1 - UPDATE CHECK TRANSACTIONS WITH GROUP CHK NUMBERS
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*       THIS PROGRAM WILL READ THE REQUEST FOR PAYMENT FILE     *
     H*        (WARPP) FOR ALL GROUP PAYMENTS AND UPDATE THE RECORDS  *
     H*        WITH THE GROUP CHECK NUMBER.                           *
     H* CALLED FROM: W6000C1                                          *
     H* CALLS TO:    NONE                                             *
     H* 09/08/94  C:15  PROJECT: WC361                                *
     H*****************************************************************
     H*  REVISION:                                                    *
     H*                                                               *
     H* 05/13/15 C:136 Project: WC2365                                *
     H*                Implement EFT into Group Checks                *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H* 05/10/01 C:15 Project: WC1020                                 *
     H*               Added code to use the Bank Code (CKTP) to       *
     H*               retrieve the appropriate group check record.    *
     H*                                                               *
     H*  10/30/97 C:30 Project: V3                                    *
     H*           Assign Bank Acct # to Request for Payment records.  *
     H*                                                               *
     H*****************************************************************
     FWARPL4    UF   E           K DISK
     F*** Begin chg *** 05/10/01 **************************************
     F*****WAR3L7  IF  E           K        DISK
     FWAR3L4    IF   E           K DISK
     F*** End chg ***** 05/10/01 **************************************
     F                                     RENAME(PRP01:PRP02)
      *** beg add ***** 05/13/15 **************************************
     f                                     prefix(R3:2)
      *** end add ***** 05/13/15 **************************************

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

      *** beg dlt ***** 05/13/15 ***********************************************
     I****PRP02**
     I***********   RPCO#                       R3CO#
     I***********   RPFND                       R3FND
     I***********   RPFYR                       R3FYR
     I***********   RPCASE                      R3CASE
     I***********   RPSEQ#                      R3SEQ#
     I***********   RPTRDT                      R3TRDT
     I***********   RPFEIN                      R3FEIN
     I***********   RPMOD                       R3MOD
     I***********   RPAMT                       R3AMT
     I***********   RPBR2                       R3BR2
     I***********   RPBR7                       R3BR7
     I***********   RPNCCI                      R3NCCI
     I***********   RPFRDT                      R3FRDT
     I***********   RPTODT                      R3TODT
     I***********   RPWKS                       R3WKS
     I***********   RPDAYS                      R3DAYS
     I***********   RPTRCD                      R3TRCD
     I***********   RPTYPE                      R3TYPE
     I***********   RPRETP                      R3RETP
     I***********   RPRECD                      R3RECD
     I***********   RPRMDS                      R3RMDS
     I***********   RPRMK1                      R3RMK1
     I***********   RPRMK2                      R3RMK2
     I***********   RPRMK3                      R3RMK3
     I***********   RPDESC                      R3DESC
     I***********   RPCKCN                      R3CKCN
     I***********   RPPNAM                      R3PNAM
     I***********   RPADR1                      R3ADR1
     I***********   RPADR2                      R3ADR2
     I***********   RPCTY                       R3CTY
     I***********   RPST                        R3ST
     I***********   RPZIP                       R3ZIP
     I***********   RPSTAT                      R3STAT
     I***********   RPUSER                      R3USER
     I***********   RPPGM                       R3PGM
     I***********   RPTIME                      R3TIME
     I***********   RPDT                        R3DT
     I***********   RPBUL1                      R3BUL1
     I***********   RPCHK#                      R3CHK#
     I***********   RPEOB#                      R3EOB#
     I***********   RPPGTP                      R3PGTP
     I***********   RPOFF#                      R3OFF#
     I***********   RPCKTP                      R3CKTP
     I***********   RPACCT                      R3ACCT
     I***********   RPRCVD                      R3RCVD
     I***********   RPDUE                       R3DUE
     I***********   RPSTS                       R3STS
     I***********   RPSORT                      R3SORT
     I***********   RPSENT                      R3SENT
     I***********   RPDSEQ                      R3DSEQ
     I***********   RPRQST                      R3RQST
     I***********   RPLTCD                      R3LTCD
     I***********   RPNTWK                      R3NTWK
     I***********   RPEOBS                      R3EOBS
     I***********   RPFNAL                      R3FNAL
      *** end dlt ***** 05/13/15 ***********************************************
     C****************************************************************
     C*  PROGRAM ENTRY PARAMETERS
     C****************************************************************
     C*
     C     *LIKE         DEFINE    RPTRDT        XXDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDATE            8
     C                   PARM                    QQCO#             3
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
     C                   MOVE      QQDATE        XXDATE            8 0
     C                   MOVE      QQCO#         XXCO#             3 0
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C*  REQUEST PAYMENT NORMAL FILE
     C*
     C     RPKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  REQUEST PAYMENT GROUP FILE
     C*
     C     RPKEY         KLIST
     C                   KFLD                    XXCO#
     C*** Begin add *** 05/10/01 **************************************
     C                   KFLD                    RPCKTP
     C*** End add ***** 05/10/01 **************************************
     C                   KFLD                    RPFND
     C                   KFLD                    RPFEIN
     C                   KFLD                    RPMOD
      *** beg add ***** 05/13/15 *************************************
     C                   KFLD                    RPEFTC
      *** end add ***** 05/13/15 *************************************
     C****************************************************************
     C*  PROGRAM MAINLINE PROCESS
     C****************************************************************
     C*
     C*  START READING THE GROUP REQUEST FOR PAYMENTS.
     C*
     C     RPKY1         CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C     RPTRDT        IFLE      XXDT
     C*
     C*  CHAIN TO GROUP REQUEST FILE WITH FUND/FEIN/MOD AND UPDATE
     C*  WITH GROUP CHECK NUMBER.
     C*
     C     RPKEY         CHAIN     PRP02                              61
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     R3CHK#        RPCHK#
      *** BEGIN ADD *** 10/31/97 ********************************************
     C                   MOVEL     R3ACCT        RPACCT                         Bank Account #
      *** END   ADD *** 10/31/97 ********************************************
     C                   UPDATE    PRP01
     C                   END
     C*
     C                   END
     C     RPKY1         READE     PRP01                                  60
     C                   END
     C*
     C* END PGM
     C*
     C                   MOVE      '1'           *INLR
