     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - WA040R - SAME DAY RECONSIDERATION CHECK REQUEST DELETION DATA
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: WA040R                                            *
     H*    CREATED: 07/10/99  C:56  PROJECT: MC202                    *
     H*    DESCRIPTION: THIS API WILL DELETE CHECK REQUESTS FOR THE   *
     H*                 CURRENT RECONSIDERATION IF NECESSARY          *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/01/00 C:62  Support: WC044/24754                           *
     H*                MAKE SURE IT DELETES ALL CHECK REQUESTS,       *
     H*                NOT JUST THE FIRST ONE.                        *
     H*                                                               *
     H* 05/19/00 C:15  Support: WC044/22902                           *
     H*                Added code to load RPSTS.                      *
     H*                                                               *
     H* 02/25/00 C:15  Project: WC577                                 *
     H*                Changed pgm to load a 'D' to RPSTAT instead of *
     H*                deleting the pmt.  This will allow the system  *
     H*                to keep track of deleted pmts.                 *
     H*                                                               *
     H* 02/10/00 C:15  Support: WC025/21292                           *
     H*                Added code to complete any diaries associated  *
     H*                with the deleted pmts.                         *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       99      GENERAL USAGE (CHAIN)                           *
     H*                                                               *
     H*****************************************************************
     H*  CALLED FROM:  F0393R                                         *
     H*****************************************************************
     F*
     FWARPLN    UF   E           K DISK
     FWMCML1    IF   E           K DISK
     F*** BEGIN ADD *** 05/19/00 **************************************
     FWTPSL     IF   E           K DISK
     F*** END ADD ***** 05/19/00 **************************************

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXKEY                  1     40
     D  XXCO#                  1      3  0
     D  XXFND                  4      6  0
     D  XXFYR                  7      9  0
     D  XXCASE                10     16  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C/COPY SCOPYDTPL

     C*
     C*  KEY LIST FOR REQUEST PAYMENT TRANSACTION FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKEOBS
     C*
     C* KEY LIST FOR CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*** BEGIN ADD *** 05/19/00 **************************************
     C*  Key list for the Payment Status table file
     C*
     C     PSKY          KLIST
     C                   KFLD                    KKCO#
     C*** END ADD ***** 05/19/00 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@EOB#            9 0
     C                   PARM                    @@EOBS            7 0
     C*
     C     *LIKE         DEFINE    RPCO#         KKCO#
     C     *LIKE         DEFINE    RPFND         KKFND
     C     *LIKE         DEFINE    RPFYR         KKFYR
     C     *LIKE         DEFINE    RPCASE        KKCASE
     C     *LIKE         DEFINE    RPEOB#        KKEOB#
     C     *LIKE         DEFINE    RPEOBS        KKEOBS                         RECON#
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@FND         KKFND
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C                   Z-ADD     @@CO#         XXCO#
     C                   Z-ADD     @@FND         XXFND
     C                   Z-ADD     @@FYR         XXFYR
     C                   Z-ADD     @@CASE        XXCASE
     C                   Z-ADD     @@EOB#        KKEOB#
     C                   Z-ADD     @@EOBS        KKEOBS
     C*
     C* CHECK TO SEE IF THERE IS A CHECK REQUEST FOR THIS BILL.
     C* IF ONE EXISTS AND PENDED, COMPLETE THE ASSOCIATED DARTS REMARK
     C* IF ONE EXISTS AND THE CLAIM IS CLOSED AND THE REQUEST IS IN A
     C* RELEASE STATUS, DECREASE RESERVES ON THE CLAIM.
     C* FINALLY, DELTED THE CHECK REQUEST.
     C*
     C     RPKY          CHAIN     PRP01                              99
     C*** BEGIN CHG *** 12/01/00 **************************************
     C********** *IN99     IFEQ *OFF
     C     *IN99         DOWEQ     *OFF
     C*** END   CHG *** 12/01/00 **************************************
     C*
     C     CMKY          CHAIN     PCM01                              98
     C     *IN98         IFEQ      *OFF
     C*
     C     CMSTAT        IFEQ      'C'
     C     RPSTAT        ANDEQ     *BLANKS
     C                   MOVE      'D'           QQTYPE                         "Decrease" Res
     C                   CALL      'SRPRCC'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    RPRECD
     C                   PARM                    QQTYPE            1
     C                   PARM                    RPAMT
     C                   ENDIF                                                  CMSTAT = 'C'
     C*
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C*** BEGIN ADD *** 02/10/00 **************************************
     C                   EXSR      CLSDRY
     C*** END ADD ***** 02/10/00 **************************************
     C*
     C*** BEGIN ADD *** 05/19/00 **************************************
     C*  Get valid 'Deleted' status codes.
     C*
     C     PSKY          CHAIN     PPS01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     PSTYPE        IFEQ      'D'
     C     PSSTAT        ANDNE     'I'
     C                   MOVEL     PSKEY         RPSTS
     C                   LEAVE
     C                   ENDIF
     C*
     C     PSKY          READE     PPS01                                  60
     C                   ENDDO
     C*** END ADD ***** 05/19/00 **************************************
     C*
     C*** BEGIN ADD *** 02/10/00 **************************************
     C                   MOVE      'D'           RPSTAT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   Z-ADD     DSTIME        RPTIME
     C                   MOVEL     Q1PGM         RPPGM
     C                   MOVEL     Q1USER        RPUSER
     C*
     C                   UPDATE    PRP01
     C*** END ADD ***** 02/10/00 **************************************
     C*** BEGIN ADD *** 12/01/00 **************************************
     C     RPKY          READE     PRP01                                  99
     C*** END   ADD *** 12/01/00 **************************************
     C*
     C*** BEGIN DLT *** 02/10/00 **************************************
     C******************** DELETPRP01
     C*** END DLT ***** 02/10/00 **************************************
     C*** BEGIN CHG *** 12/01/00 **************************************
     C**********           ENDIF                           *IN99 = *OFF
     C                   ENDDO                                                  *IN99 = *OFF
     C*** END   CHG *** 12/01/00 **************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   RETURN
     C*
     C*** BEGIN ADD *** 02/10/00 **************************************
     C*================================================================
     C* CLSDRY - This subroutine completes the diary which is attached
     C*          to the request pmt.
     C*================================================================
     C     CLSDRY        BEGSR
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVE      RPCO#         XXCO#
     C                   MOVE      RPFND         XXFND
     C                   MOVE      RPFYR         XXFYR
     C                   MOVE      RPCASE        XXCASE
     C                   MOVEL     'D'           ##RTCD
      *
     C                   CALL      'WA018R'
     C                   PARM                    XXKEY
     C                   PARM                    RPDSEQ
     C                   PARM                    ##RTCD            1
     C*
     C                   ENDSR
     C*** END ADD ***** 02/10/00 **************************************
     C*
