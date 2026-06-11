     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W7001R1 - ASSIGN BATCH NUMBERS
     H*****************************************************************
     H* DESCRIPTION:                                                  *
     H*    THIS PROGRAM ASSIGNS THE BATCH UPDATE LOG NUMBER FOR THE   *
     H*     GROUP CHECKS.                                             *
     H* CALLED FROM: W7001C                                           *
     H* CALLS TO:    NONE                                             *
     H* 09/08/94  C:15  PROJECT: WC361                                *
     H*****************************************************************
     H*  REVISION:                                                    *
     H*                                                               *
     H* 04/19/21 C:062 Project: WC2749(95409)                         *
     H*                Only apply bul# to associated check requests   *
     H*                                                               *
     H*  04/20/21 c:62 Ticket                                         *
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H* 05/10/01 C:15 Project: WC1020                                 *
     H*               Added code to assign the bul#s by Bank Code     *
     H*               (CKTP) for the multi bank acct processing.      *
     H*                                                               *
     H*****************************************************************
     FSMCOL     UF   E           K DISK
     F*** Begin dlt *** 05/10/01 **************************************
     F*******WARPL4  UF  E           K        DISK
     F*******WAC3L   IF  E           K        DISK
     F*** End dlt ***** 05/10/01 **************************************
     F*** Begin add *** 05/10/01 **************************************
     FWARPLS    UF   E           K DISK
     FWAC3L4    IF   E           K DISK
     F*** End add ***** 05/10/01 **************************************

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C****************************************************************
     C* PROGRAM ENTER PARAMETERS
     C****************************************************************
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
     C                   MOVE      QQCO#         XXCO#
     C****************************************************************
     C* PROGRAM FILE KEY LISTINGS
     C****************************************************************
     C*
     C*  REQUEST PAYMENT KEY LIST
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C*** Begin add *** 05/10/01 **************************************
     C                   KFLD                    CPCKTP
     C*** End add ***** 05/10/01 **************************************
     C                   KFLD                    CPFND
     C*
     C*  CHECK PROCESSING FILE
     C*
     C     CPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C*
     C*  COMPANY MASTER FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C****************************************************************
     C* *LIKE DEFINES AND PROGRAM VARIABLES INITIALIZATION
     C****************************************************************
     C*
     C     *LIKE         DEFINE    COCO#         XXCO#
     C     *LIKE         DEFINE    COBUL1        XXBUL1
     C     *LIKE         DEFINE    RPTRDT        XXDT
     C*
     C****************************************************************
     C*
     C*  GET THE COMPANY MASTER RECORD, GET THE BUL NUMBER
     C*
     C     COKY          CHAIN     PCO01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     COBUL1        XXBUL1
     C*
     C*  START READING THE CHECK PROCESSING FILE, WE MUST HAVE
     C*  A RECORD FOR THIS DATE
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C     CPKY          CHAIN     PCP01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  THE CHECK PROCESSING FLAGS MUST BE BLANK
     C*
     C     CPSTA1        IFEQ      ' '
     C     CPSTA0        ANDEQ     ' '
     C*
     C*  CHECK TO SEE IF WE ARE IN RESTART, IF YES THEN WAIT UNTIL
     C*  WE GET TO THE RECORD FOR RESTART, AND START UPDATING
     C*
     C*  START READING ALL PAYMENT RECORDS WHICH HAVE NOT BEEN UPDATED
     C*
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C     RPTRDT        IFLE      XXDT
       //*** Begin Add *** 04/19/21 **************************************
       If (CPeftc = RPeftc) Or
          (CPeftc = 'B' And (RPeftc = ' ' Or RPeftc = 'E')) Or
          (CPeftc = 'A');
       Else;
          Reade rpky prp01;
          If %Eof(warpls);
             *in60 = *On;
          EndIf;
          Iter;
       Endif;
       //*** End   Add *** 04/19/21 **************************************
     C*
     C     CPRSTR        IFEQ      *ZERO
     C*
     C     XXBUL1        ADD       1             XXBUL1
     C                   Z-ADD     XXBUL1        RPBUL1
     C                   UPDATE    PRP01
     C*
     C                   END
     C*
     C                   END
     C     RPKY          READE     PRP01                                  60
     C                   END
     C*
     C                   END
     C*
     C     CPKY          READE     PCP01                                  60
     C                   END
     C*
     C*  UPDATE THE COMPANY MASTER
     C*
     C                   Z-ADD     XXBUL1        COBUL1
     C                   UPDATE    PCO01
     C                   END
     C*
     C                   MOVE      '1'           *INLR
