     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W3030R - INSERT CHECK NUMBERS, BATCH NUMBERS FOR COMM.
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  12/02/97 C:30 Project:  WC500                                *
     H*           Change DATE PARM from 6 to 8 chars.                 *
     H*           Assign Check Processing Bank Account # to each      *
     H*           Request for Payment record.                         *
     H*           When the Group has been processed, WAC2L2 will      *
     H*           be used to find ALL Group records with the same     *
     H*           Bank Account.  If another Group is found, the Check *
     H*           Processing record will be updated with the Starting *
     H*           check #.                                            *
     H*                                                               *
     H*****************************************************************
     FSMCOL     UF   E           K DISK
     FWAR2L     UF   E           K DISK
     FWAC2L     IF   E           K DISK
     FWXWPL1    UF A E           K DISK
      *** BEGIN ADD *** 12/02/97 **************************************
      *
      *  Agent Commission Check Processing file
     FWAC2L2    UF   E           K DISK
     F                                     RENAME(PC201:PC202)
      *** END   ADD *** 12/02/97 **************************************
      *

     D DES             S             30    DIM(1) CTDATA PERRCD(1)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C*  REQUEST PAYMENT KEY LIST
     C*
     C     R2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  CHECK PROCESSING FILE
     C*
     C     C2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C                   KFLD                    XXFND
      *
      *** BEGIN ADD *** 12/02/97 ***********************************************
      *
     C     C2KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C                   KFLD                    XXACCT
      *
     C     *LIKE         DEFINE    C2ACCT        XXACCT
      *** END   ADD *** 12/02/97 ***********************************************
     C*
     C*  WASTE CHECK KEY LIST
     C*
     C     WPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    WPCHK#
     C*
     C*  COMPANY MASTER FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C     *LIKE         DEFINE    R2DT          XXDT
     C*
     C     *ENTRY        PLIST
      *** BEGIN CHG *** 12/02/97 ***********************************************
     C*********************PARM***********QQDATE  6
     C                   PARM                    QQDATE            8
      *** END   CHG *** 12/02/97 ***********************************************
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
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
      *** BEGIN CHG *** 12/02/97 ***********************************************
     C*********************MOVE QQDATE    XXDATE  60
      *
     C                   MOVE      QQDATE        XXDATE            8 0
      *** END   CHG *** 12/02/97 ***********************************************
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C*
     C*  GET THE COMPANY MASTER RECORD, GET THE BUL NUMBER
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     COBUL2        XXBUL2            9 0
     C*
     C*  CHAIN TO THE CHECK PROCESSING FILE, WE MUST HAVE
     C*  A RECORD FOR THIS DATE
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C     C2KEY         CHAIN     PC201                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  THE CHECK PROCESSING FLAGS MUST BE BLANK
     C*
     C     C2STA1        IFEQ      ' '
     C     C2STA0        ANDEQ     ' '
     C*
     C     C2RSTR        IFNE      *ZERO
     C                   Z-ADD     C2RSTR        XXCHK#            7 0
     C                   ELSE
     C                   Z-ADD     C2CHK#        XXCHK#
     C                   END
     C*
     C*  CHECK TO SEE IF WE ARE IN RESTART, IF YES THEN WAIT UNTIL
     C*  WE GET TO THE RECORD FOR RESTART, AND START UPDATING
     C*
     C*  START READING ALL PAYMENT RECORDS WHICH HAVE NOT BEEN UPDATED
     C*
     C     R2KEY         CHAIN     PR201                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     C2RSTR        IFNE      *ZERO
     C*
     C     R2CHCK        IFGE      C2CHK#
     C*
      *** BEGIN ADD *** 12/02/97 ***********************************************
     C                   MOVEL     C2ACCT        R2ACCT
      *** END   ADD *** 12/02/97 ***********************************************
     C                   Z-ADD     XXCHK#        R2CHCK
     C     XXCHK#        ADD       1             XXCHK#
     C                   UPDATE    PR201
     C*
     C                   END
     C*
     C                   ELSE
     C*
     C     XXBUL2        ADD       1             XXBUL2
     C                   Z-ADD     XXBUL2        R2BUL2
      *** BEGIN ADD *** 12/02/97 ***********************************************
     C                   MOVEL     C2ACCT        R2ACCT
      *** END   ADD *** 12/02/97 ***********************************************
     C                   Z-ADD     XXCHK#        R2CHCK
     C     XXCHK#        ADD       1             XXCHK#
     C                   UPDATE    PR201
     C*
     C                   END
     C*
     C     R2KEY         READE     PR201                                  60
     C                   END
     C*
     C                   END
     C*
     C*  WRITE TRANSACTIONS TO THE WASTE CHECK FILE
     C*
     C     C2WAST        IFEQ      'Y'
     C     C2RSTR        IFNE      *ZERO
     C*
     C     C2RSTR        SUB       C2CHK#        X1                3 0
     C                   Z-ADD     C2CHK#        X2                7 0
     C*
     C                   MOVE      XXCO#         WPCO#
     C                   MOVE      XXFND         WPFND
     C                   MOVE      XXDT          WPTRDT
     C                   MOVEL     DES(1)        WPDESC
     C                   MOVE      Q1USER        WPUSER
     C                   MOVE      Q1PGM         WPPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WPDT
     C*
     C                   MOVE      DSTIME        WPTIME
     C                   MOVE      *ZERO         WPBUL1
     C*
     C     1             DO        X1
     C                   Z-ADD     X2            WPCHK#
     C                   ADD       1             X2
     C     WPKEY         CHAIN     PWP01                              60
     C     *IN60         IFEQ      '1'
     C                   WRITE     PWP01
     C                   ELSE
     C                   MOVE      XXCO#         WPCO#
     C                   MOVE      XXFND         WPFND
     C                   MOVE      XXDT          WPTRDT
     C                   MOVEL     DES(1)        WPDESC
     C                   MOVE      Q1USER        WPUSER
     C                   MOVE      Q1PGM         WPPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WPDT
     C*
     C                   MOVE      DSTIME        WPTIME
     C                   MOVE      *ZERO         WPBUL1
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C                   END
     C*
     C*  UPDATE THE COMPANY MASTER
     C*
     C                   Z-ADD     XXBUL2        COBUL2
     C                   UPDATE    PCO01
     C                   END
      *
      *** BEGIN ADD *** 12/02/97 ***********************************************
      *
      *  Update the Starting Check Number in the Check Processing  file
      *  for ALL Groups with requested Processing Date.
      *
     C                   MOVEL     C2ACCT        XXACCT
      *
     C     C2KY1         CHAIN     PC202                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     C2STA1        IFEQ      *BLANK
     C     C2STA0        ANDEQ     *BLANK
     C     C2FND         ANDNE     XXFND
      *
     C                   Z-ADD     XXCHK#        C2CHK#
     C                   MOVE      Q1USER        C2USER
     C                   MOVE      Q1PGM         C2PGM
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        C2TIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C2DT
      *
     C                   UPDATE    PC202
      *
     C                   ENDIF
      *
     C     C2KY1         READE     PC202                                  60
      *
     C                   ENDDO
      *
      *** END   ADD *** 12/02/97 ***********************************************
     C*
     C                   MOVE      '1'           *INLR
**
WASTE CHECK - VOID
