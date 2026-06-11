     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7010R - MOVE CHECK TRANSACTIONS TO MONTHLY FILE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H* 01/05/11 C:257 Project: 114WXM                                *
     H*               New field added - RECOMPILE ONLY                *
     H*                                                               *
     H* 11/18/02 C:53 Project: WC909                                  *
     H*               New fields added - RECOMPILE ONLY               *
     H*                                                               *
     H* 11/11/99 C:15 Project: WC577                                  *
     H*               Changed pgm to load pmts if RPBUL1 = 0 for      *
     H*               disaster recovery purposes.                     *
     H*                                                               *
     H* 08/19/99 C:15 Support: WC025/18690                            *
     H*               Changed pgm to correctly load the deleted pmts  *
     H*               into WARPPM.                                    *
     H*                                                               *
     H* 03/04/99 C:15 Support: WC060/16413                            *
     H*               Changed pgm to delete transactions from all     *
     H*               groups regardless of whether or not there is a  *
     H*               check schedule record.                          *
     H*                                                               *
     H* 12/04/98 C:15 Project: 48MBV                                  *
     H*               Changed pgm to write deleted transactions to    *
     H*               Request Payment History file.                   *
     H*                                                               *
     H* 3/24/98 C:86 Project: WC820                                   *
     H*              Added code to complete the diary items for all   *
     H*              payments.  If the remark code matches the one    *
     H*              setup for the last reoccurring comp payment,     *
     H*              do not complete the diary.                       *
     H*****************************************************************
     FWARPL     UF   E           K DISK
     F*** BEGIN CHG *** 11/11/99 **************************************
     F*****WARPLM  IF  E           K        DISK                      A
     FWARPLM    O    E           K DISK
     F*** END CHG ***** 11/11/99 **************************************
     F                                     RENAME(PRP01:PRP02)
     F*** BEGIN DLT *** 03/04/99 **************************************
     F*******WACPL   IF  E           K        DISK
     F*** END DLT ***** 03/04/99 **************************************
     F*** BEGIN ADD *** 03/24/98 **************************************
     FSDCMLA    UF   E           K DISK
     FWMPCL     IF   E           K DISK
     F*** END ADD ***** 03/24/98 **************************************

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*** BEGIN ADD *** 03/24/98 **************************************
     D                 DS
     D  QCKEY                  1     40
     D  RPCO#                  1      3  0
     D  RPFND                  4      6  0
     D  RPFYR                  7      9  0
     D  RPCASE                10     16  0

     I*** END ADD ***** 03/24/98 **************************************

     C*
     C     *LIKE         DEFINE    RPDT          XXDT
     C*** BEGIN ADD *** 03/04/99 **************************************
     C     *LIKE         DEFINE    RPFND         SVFND
     C*** END ADD ***** 03/04/99 **************************************
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
     C*** BEGIN ADD *** 03/24/98 **************************************
     C*  PAYMENT CONTROL FILE WMPCL
     C*
     C     PCKY          KLIST
     C                   KFLD                    XXCO#
     C*** BEGIN CHG *** 03/04/99 **************************************
     C******************** KFLD           CPFND
     C                   KFLD                    RPFND
     C*** END CHG ***** 03/04/99 **************************************
     C*
     C*  DARTS FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    QCKEY
     C                   KFLD                    RPDSEQ
     C*** END ADD ***** 03/24/98 **************************************
     C*
     C*  REQUEST PAYMENT KEY LIST
     C*
     C     RPKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C*** BEGIN DLT *** 03/04/99 **************************************
     C******************** KFLD           CPFND
     C*** END DLT ***** 03/04/99 **************************************
     C*
     C     RPKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPBUL1
     C*
     C*** BEGIN DLT *** 03/04/99 **************************************
     C*  CHECK PROCESSING KEY LIST
     C**********
     C********** CPKEY     KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXDT
     C*** END DLT ***** 03/04/99 **************************************
     C*
     C*  START READING THE CHECK PROCESSING FILE, WE MUST HAVE
     C*  A RECORD FOR THIS DATE
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C*** BEGIN DLT *** 03/04/99 **************************************
     C********** CPKEY     CHAINPCP01                60
     C********** *IN60     DOWEQ'0'
     C**********
     C*  THE CHECK PROCESSING FLAG MUST BE FLIPPED
     C**********
     C********** CPSTA0    IFEQ 'Y'
     C********** CPSTA1    ANDEQ'Y'
     C**********
     C*** BEGIN ADD *** 03/24/98 **************************************
     C*  GET THE RECURRING COMP CONTROL RECORD
     C**********
     C**********           MOVE *BLANKS   PCRCLP
     C********** PCKY      CHAINPPC01                60
     C*** END ADD ***** 03/24/98 **************************************
     C*** END DLT ***** 03/04/99 **************************************
     C*
     C*  START READING ALL PAYMENT RECORDS
     C*
     C     RPKEY         CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  IF RECORD PROCESSED, WRITE TO MONTHLY FILE AND DELETE TRANS
     C*
     C     RPSTAT        IFEQ      'Y'
     C*** BEGIN ADD *** 12/04/98 **************************************
     C     RPSTAT        OREQ      'D'
     C*** END ADD ***** 12/04/98 **************************************
     C*
     C*  CHAIN TO THE MONTHLY FILE WITH THE INFO
     C*
     C*** BEGIN DLT *** 11/11/99 **************************************
     C*** BEGIN ADD *** 08/19/99 **************************************
     C********** RPSTAT    IFEQ 'Y'
     C*** END ADD ***** 08/19/99 **************************************
     C********** RPKEY1    CHAINPRP02                60
     C********** *IN60     IFEQ '1'
     C**********           WRITEPRP02
     C**********           END
     C*** BEGIN ADD *** 08/19/99 **************************************
     C**********           ELSE
     C**********           WRITEPRP02
     C**********           END
     C*** END ADD ***** 08/19/99 **************************************
     C*** END DLT ***** 11/11/99 **************************************
     C*** BEGIN ADD *** 11/11/99 **************************************
     C                   WRITE     PRP02
     C*** END ADD ***** 11/11/99 **************************************
     C*
     C                   END
     C*
     C*  DELETE THE PROCESSED OR DELETED TRANSACTION RECORDS
     C*
     C     RPSTAT        IFEQ      'Y'
     C     RPSTAT        OREQ      'D'
     C*
     C*** BEGIN ADD *** 03/04/99 **************************************
     C*  GET THE RECURRING COMP CONTROL RECORD
     C*
     C     RPFND         IFNE      SVFND
     C                   MOVE      *BLANKS       PCRCLP
     C     PCKY          CHAIN     PPC01                              60
     C                   Z-ADD     RPFND         SVFND
     C                   END
     C*** END ADD ***** 03/04/99 **************************************
     C*** BEGIN ADD *** 03/24/98 **************************************
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C     CMDCDT        ANDEQ     0
     C*
     C     CMRMCD        IFNE      PCRCLP
     C     CMRMCD        OREQ      *BLANKS
     C                   MOVE      XXDT          CMDCDT
     C                   MOVEL     Q1PGM         CMPGM
     C                   MOVE      XXDT          CMDT
     C                   MOVE      *BLANKS       CMFL04
     C                   UPDATE    PCM01
     C                   ENDIF
     C*
     C                   ELSE
     C  N60              EXCEPT    PCM01E
     C                   ENDIF
     C*** END ADD ***** 03/24/98 **************************************
     C*
     C                   DELETE    PRP01
     C                   END
     C*
     C     RPKEY         READE     PRP01                                  60
     C                   END
     C*
     C*** BEGIN DLT *** 03/04/99 **************************************
     C**********           END
     C**********
     C********** CPKEY     READEPCP01                    60
     C**********           END
     C*** END DLT ***** 03/04/99 **************************************
     C*
     C                   MOVE      '1'           *INLR
     O*
     O*** BEGIN ADD *** 03/24/98 **************************************
     OPCM01     E            PCM01E
     O*** END ADD ***** 03/24/98 **************************************
