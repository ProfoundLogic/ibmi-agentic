     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE W7302R - SERVICE RECORD RELEASE AND INVOICING
     H*****************************************************************
     H** PROGRAM DESCRIPTION:                                         *
     H**   This program reads FAEOL2 and updates the release flag     *
     H** (EOHRFL) with an 'R'.  The Invoicing programs are called for *
     H** each Subscriber (N7120C, N9310C).                            *
     H**                                                              *
     H**                                                              *
     H** CALLED FROM:                                                 *
     H**                                                              *
     H**   F7301C                                                     *
     H**                                                              *
     H** CALLS TO:                                                    *
     H**                                                              *
     H**   N7120C - PBR/HBA Billing Override CL                       *
     H**   N9300C - Network Invoicing/Re-Bills Override CL            *
     H**                                                              *
     H** PROGRAMMER: C:35 - CARL F. GREEN                             *
     H** CREATED:    08/28/95         PROJECT: 25MBK                  *
     H**                                                              *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  01/23/02 C:62 PROJECT: AR033
     H*                USE NEW NETWORK INVOICE PROGRAM.
     H*
     H*  03/19/99 C:94 PROJECT: 25MKL
     H*                ADD COMMENTS, CHANGE PROGRAM TO BE AT FAEAP
     H*                LEVEL, NOT FAEOP LEVEL
     H*
     H*  03/06/99 C:94 PROJECT: MC152
     H*                CHG CALL TO N9300C IF PRNTWK = I
     H*
     H*  02/11/99 C:94 PROJECT: 48MBV                                 *
     H*                ADD SELECTION OF RECORDS TO ONLY ALLOW IF      *
     H*                RPNTWK = B, Y AND RPSTAT = Y                   *
     H*                                                               *
     H*  01/29/99 C:94 PROJECT: 48MBV                                 *
     H*                CHANGE THE READ TO WARPLE AND CHAIN TO FAEOP   *
     H*                INORDER TO MAKE BATCH GO FASTER                *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS USED:                                              *
     H*                                                               *
     H* 60 - SMUPL CHAIN INDICATOR/FAEOL2 READ INDICATOR              *
     H* 61 - FMF1L CHAIN INDICATOR                                    *
     H*                                                               *
     H*****************************************************************
     H** FILE USAGE:                                                  *
     H**                                                              *
     H** DISPLAY FILES:                                               *
     H**                                                              *
     H**                                                              *
     H** DATA FILES:                                                  *
     H**    FAEOL3 (PEO01): CO#/SUB#/FND/FYR/CASE/EOB#/STAT           *
     H**    FMF1L  (PF101): CO#/SUB#/FND                              *
     H**    SMUPL  (PUP01): USR                                       *
     H**                                                              *
     H*****************************************************************
     F*** BEG CHG *** 01/29/99 ***************************************
     F*** BEG DEL *** 03/19/99  **************************************
     F**********FAEOLG  UF  E           K        DISK
     F*** END DEL *** 03/19/99  **************************************
     F**********FAEOL3  UF  E           K        DISK
     F*** END CHG *** 01/29/99 ***************************************
     FFMF1L     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     F*** BEG ADD *** 01/29/99 ***************************************
     FWARPLE    IF   E           K DISK
     F*** END ADD *** 01/29/99 ***************************************
     F*** BEG ADD *** 03/19/99 ***************************************
     FFAEAL1    UF   E           K DISK
     F*** END ADD *** 03/19/99 ***************************************
     F*****************************************************************

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C*
     C*** BEG CHG *** 03/19/99 ***************************************
     C     EAKEY         KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C                   KFLD                    RPEOB#
     C                   KFLD                    XXRCN
     C**********
     C****BEG*ADD *** 01/29/99 ***************************************
     C********** EOKEY     KLIST
     C**********           KFLD           RPCO#
     C**********           KFLD           KKSUB#
     C**********           KFLD           KKFND
     C**********           KFLD           RPFYR
     C**********           KFLD           RPCASE
     C**********           KFLD           RPEOB#
     C****END*ADD *** 01/29/99 ***************************************
     C*** END CHG *** 03/19/99 ***************************************
     C**********
     C*** BEG CHG *** 03/19/99 ***************************************
     C     *LIKE         DEFINE    EACO#         XXCO#
     C     *LIKE         DEFINE    EASUB#        XXSUB#
     C     *LIKE         DEFINE    EAFND         XXFND
     C     *LIKE         DEFINE    EACO#         XXPCO#
     C     *LIKE         DEFINE    EASUB#        XXPSUB
     C     *LIKE         DEFINE    EAFND         XXPFND
     C     *LIKE         DEFINE    EACASE        XXPCAS
     C     *LIKE         DEFINE    EARCN         XXRCN
     C********** *LIKE     DEFN EOCO#     XXCO#
     C********** *LIKE     DEFN EOSUB#    XXSUB#
     C********** *LIKE     DEFN EOFND     XXFND
     C********** *LIKE     DEFN EOCO#     XXPCO#
     C********** *LIKE     DEFN EOSUB#    XXPSUB
     C********** *LIKE     DEFN EOFND     XXPFND
     C********** *LIKE     DEFN EOCASE    XXPCAS
     C*** END CHG *** 03/19/99 ***************************************
     C     *LIKE         DEFINE    UPUSER        XXUSER
      *
      *****************************************************************
      *
      *  KEY LIST FOR USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    XXUSER
      *****************************************************************
      *
      *  KEY LIST FOR FUND AUXILIARY FILE
      *
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
      *****************************************************************
      *
      *
      *  GET THE USER PARAMETER FILE
      *
     C                   MOVEL     Q1USER        XXUSER
     C     UPKY          CHAIN     PUP01                              60
     C                   MOVEL     ' '           NTWK              1
     C*** BEGIN ADD *** 01/23/02 **************************************
     C                   MOVEL     *BLANKS       BRS               1
     C*** END   ADD *** 01/23/02 **************************************
      *
     C*** BEG CHG *** 01/29/99 ***************************************
     C*** THIS WILL READ THE CHECK REQUESTS THAT WERE ISSUED AS
     C*** CHECKS IN THIS BATCH
     C***
     C     *LOVAL        SETLL     PRP01
     C                   READ      PRP01                                  60
     C********** *LOVAL    SETLLPEO01
     C**********           READ PEO01                    60
     C     *IN60         DOWEQ     *OFF
     C*** BEG ADD *** 02/11/99 ***************************************
     C     RPNTWK        IFEQ      'B'                                          BRS CHECKS
     C*** BEGIN CHG *** 07/27/99 **************************************
     C********** RPNTWK    OREQ 'Y'                        NWS CHECKS
     C     RPNTWK        OREQ      'I'                                          NWS CHECKS
     C*** END   CHG *** 07/27/99 **************************************
     C     RPSTAT        IFEQ      'Y'                                          CHECK WRITEN
     C*** END ADD *** 02/11/99 ***************************************
     C     RPNTWK        IFEQ      'I'
     C                   MOVEL     'Y'           NTWK
     C                   ENDIF
     C*** BEGIN ADD *** 01/23/02 **************************************
     C     RPNTWK        IFEQ      'B'
     C                   MOVEL     'Y'           BRS
     C                   ENDIF
     C*** END   ADD *** 01/23/02 **************************************
     C*
     C                   MOVE      RPCO#         @@CO#
     C                   MOVE      RPFND         @@WCF#
     C*
     C*** TRANSLATE THE FUND TO GET SUBSCRIBER AND FUND
     C*
     C                   CALL      'FA700R'
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@WCF#            3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C*
     C                   MOVE      @@SUB#        KKSUB#            5 0
     C                   MOVE      @@FND         KKFND             3 0
     C*** END CHG *** 01/29/99 ***************************************
      *
     C                   Z-ADD     0             XXPCO#
     C                   Z-ADD     0             XXPFND
     C                   Z-ADD     0             XXPSUB
     C                   MOVE      '1'           *IN61
      *
     C*** BEG CHG *** 03/19/99 ***************************************
     C*** CHAIN TO FAEAP TO GET APPROPRIATE RECORD SO INVOICE
     C*** TAKES PLACE CORRECTLY FOR CHECK REQUEST'S MATCHED EOB'S
     C*
     C                   MOVE      RPEOBS        XXRCN
     C*
     C     EAKEY         CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C****BEG*ADD *** 01/29/99 ***************************************
     C********** EOKEY     CHAINPEO01                99
     C********** *IN99     IFEQ *OFF
     C****END*ADD *** 01/29/99 ***************************************
     C*** END CHG *** 03/19/99 ***************************************
     C*
     C     *IN61         IFEQ      *OFF
     C     F1OUTA        ANDEQ     'N'
     C*
     C*** BEG CHG *** 03/19/99 ***************************************
     C     EASUB#        IFNE      XXPSUB
     C********** EOSUB#    IFNE XXPSUB
     C*** END CHG *** 03/19/99 ***************************************
     C     *IN62         ANDEQ     *ON
     C*
     C* CALL THE A/R INVOICING PROGRAM FOR EACH EOB
     C*
     C*** BEGIN ADD *** 01/23/02 **************************************
     C     BRS           IFEQ      'Y'
     C*** END   ADD *** 01/23/02 **************************************
     C                   MOVE      XXPCO#        QQCO#             3
     C                   MOVE      XXPSUB        QQSUB#            5
     C                   CALL      'N7120C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    UPOUTQ
     C                   PARM                    MODE              1
     C*** BEGIN ADD *** 01/23/02 **************************************
     C                   PARM      'B'           INVT              1
     C*
     C                   MOVEL     *BLANKS       BRS
     C*
     C                   ENDIF
     C*** END   ADD *** 01/23/02 **************************************
      *
      * CALL THE A/R INVOICING PROGRAM
      *
     C*** BEG ADD *** 03/06/99 ***************************************
     C*          RPNTWK    IFEQ 'I'                        INHOUSE NETWORK
     C     NTWK          IFEQ      'Y'
     C*** END ADD *** 03/06/99 ***************************************
     C                   MOVE      XXPCO#        QQCO#
     C                   MOVE      XXPSUB        QQSUB#
     C*** BEGIN CHG *** 01/23/02 **************************************
     C**********           CALL 'N9300C'
     C**********           PARM           QQCO#
     C**********           PARM           QQSUB#
     C**********           PARM           QQTYPE  1
     C**********           PARM           UPOUTQ
     C**********           PARM           MODE
     C                   CALL      'N9310C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    UPOUTQ
     C                   PARM                    MODE
     C*
     C                   MOVEL     *BLANKS       NTWK
     C*** END   CHG *** 01/23/02 **************************************
     C*** BEG ADD *** 03/06/99 ***************************************
     C                   ENDIF
     C*** END ADD *** 03/06/99 ***************************************
      *
     C                   MOVE      '0'           *IN62
      *
     C                   ENDIF                                                  EOSUB# IFNE
      *
     C                   ENDIF                                                  *IN61 IFEQ
      *
     C*** BEG CHG *** 03/19/99 ***************************************
     C     EACO#         IFNE      XXPCO#
     C     EASUB#        ORNE      XXPSUB
     C     EAFND         ORNE      XXPFND
      *
     C                   MOVE      EACO#         XXCO#
     C                   MOVE      EASUB#        XXSUB#
     C                   MOVE      EAFND         XXFND
     C********** EOCO#     IFNE XXPCO#
     C********** EOSUB#    ORNE XXPSUB
     C********** EOFND     ORNE XXPFND
     C**********
     C**********           MOVE EOCO#     XXCO#
     C**********           MOVE EOSUB#    XXSUB#
     C**********           MOVE EOFND     XXFND
     C*** END CHG *** 03/19/99 ***************************************
      *
     C     F1KY          CHAIN     PF101                              61
      *
     C                   ENDIF                                                  EOCO# IFNE
      *
     C*** BEG CHG *** 03/19/99 ***************************************
     C                   MOVE      EACO#         XXPCO#
     C                   MOVE      EASUB#        XXPSUB
     C                   MOVE      EAFND         XXPFND
     C**********           MOVE EOCO#     XXPCO#
     C**********           MOVE EOSUB#    XXPSUB
     C**********           MOVE EOFND     XXPFND
     C*** END CHG *** 03/19/99 ***************************************
      *
     C     *IN61         IFEQ      *OFF
     C     F1OUTA        ANDEQ     'N'
      *
     C*** BEG CHG *** 03/19/99 ***************************************
     C*** WE UPDATE EABHRF AND SO THAT THE EOMB IS PICKED UP
     C*** IN INVOICING.  WE UPDATE EAAPPF SO THAT IT WILL NOT GET
     C*** PICKED UP A SECOND TIME.
     C***
     C*** BEGIN CHG *** 01/23/02 **************************************
     C**********           MOVE 'R'       EABHRF
     C     RPNTWK        IFEQ      'B'
     C                   MOVE      'R'           EABHRF
     C                   ENDIF
     C     RPNTWK        IFEQ      'I'
     C                   MOVE      'R'           EANHRF
     C                   ENDIF
     C*** END   CHG *** 01/23/02 **************************************
     C                   MOVE      'P'           EAAPPF
     C                   UPDATE    PEA01
     C**********           MOVE 'R'       EOHRFL
     C**********           MOVE 'P'       EOFLG3
     C**********           UPDATPEO01
     C*** END CHG *** 03/19/99 ***************************************
     C                   MOVE      '1'           *IN62
      *
     C                   ENDIF                                                  *IN61 IFEQ
      *
     C                   ENDIF                                                  *IN99
     C*** BEG ADD *** 02/11/99 ***************************************
     C                   ENDIF                                                  RPSTAT = Y
     C                   ENDIF                                                  RPNTWK = B,Y
     C*** END ADD *** 02/11/99 ***************************************
      *
     C*** BEG CHG *** 01/29/99 ***************************************
     C*** READ THE NEXT CHECK REQUEST
     C*
     C                   READ      PRP01                                  60
     C***********          READ PEO01                    60
     C*** END CHG *** 01/29/99 ***************************************
      *
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C     *IN62         IFEQ      *ON
      *
      * CALL THE A/R INVOICING PROGRAM FOR FINAL SUBSCRIBER
      *
     C*** BEGIN ADD *** 01/23/02 **************************************
     C     BRS           IFEQ      'Y'
     C*** END   ADD *** 01/23/02 **************************************
     C                   MOVE      XXPCO#        QQCO#             3
     C                   MOVE      XXPSUB        QQSUB#            5
     C                   CALL      'N7120C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    UPOUTQ
     C                   PARM                    MODE              1
     C*** BEGIN ADD *** 01/23/02 **************************************
     C                   PARM      'B'           INVT
     C                   ENDIF
     C*** END   ADD *** 01/23/02 **************************************
      *
      * CALL THE A/R INVOICING PROGRAM FOR FINAL CASE
      *
     C*** BEG ADD *** 03/06/99 ***************************************
     C*          RPNTWK    IFEQ 'I'                        INHOUSE NETWORK
     C     NTWK          IFEQ      'Y'
     C*** END ADD *** 03/06/99 ***************************************
     C                   MOVE      XXPCO#        QQCO#
     C                   MOVE      XXPSUB        QQSUB#
     C*** BEGIN CHG *** 01/23/02 **************************************
     C**********           CALL 'N9300C'
     C**********           PARM           QQCO#
     C**********           PARM           QQSUB#
     C**********           PARM           QQTYPE  1
     C**********           PARM           UPOUTQ
     C**********           PARM           MODE
     C                   CALL      'N9310C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    UPOUTQ
     C                   PARM                    MODE
     C*** END   CHG *** 01/23/02 **************************************
     C*** BEG ADD *** 03/06/99 ***************************************
     C                   ENDIF
     C*** END ADD *** 03/06/99 ***************************************
      *
     C                   ENDIF                                                  *IN62 IFEQ
      *
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
