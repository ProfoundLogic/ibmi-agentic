     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE FA020R - Retrieve EOMB data
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*    This program retrieves information from the EOMB file.     *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0080R, W7004R                                             *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 10/24/97  C:15  Project:                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 05/04/00 C:62 Project: WC048 22709                            *
     H*               ADD ADDITIONAL PARM FOR CHECK# AND USE THAT TO  *
     H*               DETERMINE WHAT INVOICE NUMBER IS PASSED BACK.   *
     H*                                                               *
     H* 04/06/00 C:62 Project: WC048 22467                            *
     H*               CHANGE HOW THE PARM @@RCN IS USED, INSTEAD OF   *
     H*               BEING PASSED BACK, PASS IN RCN TO ACCESS THE    *
     H*               CORRECT FAEAP RECORD.                           *
     H*                                                               *
     H* 03/03/00 C:62 Project: MCMNT
     H*               WITH PASSED IN FEIN/MOD DETERMINE IF BRS INV#
     H*               SHOULD BE PASSED BACK.
     H*
     H* 03/01/00 C:62 Project: MCMNT
     H*               CHANGE LOCATION OF EOINV# TO @@INV#
     H*
     H* 10/13/99 C:53 Project: MC212
     H*               EXPANDED INVOICE # TO 9 POS
     H*
     H* 03/15/99 C:94 PROJECT: 25MKL
     H*               ADD SA001R AND CHANGE INDICATORS
     H*               IF EOBLST = D MOVE 1 TO @@RC
     H*
     H* 03/03/99 C:94 PROJECT: 25MKL
     H*               ADD RECON NUMBER
     H*
     H* 01/21/99 C:94 PROJECT: 48MBV                                  *
     H*               ADDED PARMS FOR INV# FOR W0576R02               *
     H*                                                               *
     H* 10/30/97 C:15 Project: WC659                                  *
     H*               Chg pgm to send '1' in @@RTCD if no EOMB exists *
     H*               for the entered data.                           *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     FFMFNL1    IF   E           K DISK
     FFAEOL4    IF   E           K DISK
     F*** BEG ADD *** 01/21/99 ***************************************
     FNATRL     IF   E           K DISK
     F*** BEG ADD *** 03/03/99 ***************************************
     FFAEAL1    IF   E           K DISK
     F*** END ADD *** 03/03/99 ***************************************

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*** END ADD *** 01/21/99 ***************************************
     I*
      *

     C     *ENTRY        PLIST
1    C                   PARM                    @@CALL            1            Call code
2    C                   PARM                    @@CO#             3            Company #
3    C                   PARM                    @@WCF#            3            WCMS Fund
4    C                   PARM                    @@FYR             3            Policy Year
5    C                   PARM                    @@CASE            7            Claim #
6    C                   PARM                    @@EOB#            9            EOMB #
7    C                   PARM                    @@OTHR            9            EOMB billing amt
8    C                   PARM                    @@RC              1            Return Code
     C*** BEG ADD *** 01/21/99 ***************************************
     C*** BEG CHG *** 10/13/99 ***************************************
9    C                   PARM                    @@INV#            9            INVOICE #
10   C                   PARM                    @@NINV            9            NWS INV#
     C*** END CHG *** 10/13/99 ***************************************
     C*** END ADD *** 01/21/99 ***************************************
     C*** BEG ADD *** 03/03/99 ***************************************
11   C                   PARM                    @@RCN             3            RECON #
     C*** END ADD *** 03/03/99 ***************************************
     C*** BEGIN ADD *** 03/03/00 **************************************
12   C                   PARM                    @@FEIN            9 0          FEIN
13   C                   PARM                    @@MOD             3 0          MOD
     C*** END   ADD *** 03/03/00 **************************************
     C*** BEGIN ADD *** 05/04/00 **************************************
14   C                   PARM                    @@CHK#            9 0
     C*** END   ADD *** 05/04/00 **************************************
     C*
     C*  Key list for the WCMC Fund Master File
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWCF#
     C*
     C*  Key list for the EOMB File
     C*
     C     EOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*** BEG ADD *** 01/21/99 ***************************************
     C*
     C     TRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*** END ADD *** 01/21/99 ***************************************
     C*** BEG ADD *** 03/03/99 ***************************************
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXRCN
     C*
     C*** END ADD *** 03/03/99 ***************************************
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOFND         XXFND
     C     *LIKE         DEFINE    EOFND         XXWCF#
     C     *LIKE         DEFINE    EOFYR         XXFYR
     C     *LIKE         DEFINE    EOCASE        XXCASE
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C*** BEG ADD *** 03/03/99 ****************************************
     C     *LIKE         DEFINE    EARCN         XXRCN
     C*** END ADD *** 03/03/99 ****************************************
     C*
     C*** BEG ADD *** 03/15/99 ***************************************
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
     C*
     C*** END ADD *** 03/15/99 ***************************************
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@WCF#        XXWCF#
     C                   MOVE      @@FYR         XXFYR
     C                   MOVE      @@CASE        XXCASE
     C                   MOVE      @@EOB#        XXEOB#
     C                   MOVE      *ZEROS        XXFND
     C                   MOVE      *ZEROS        XXSUB#
     C*** BEGIN ADD *** 04/06/00 **************************************
     C     Q1PRMS        IFGT      10
     C                   MOVE      @@RCN         XXRCN
     C                   ENDIF
     C*** END   ADD *** 04/06/00 **************************************
     C*
     C*================================================================
     C*  Program Start
     C*================================================================
     C*
     C*  Translate the WCMS fund into WCMC Sub# and fund.
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVE      FNSUB#        XXSUB#
     C                   MOVE      FNFND         XXFND
     C                   ENDIF
     C*
     C*  Update the EOMB billing amount.
     C*
     C     EOKY          CHAIN     PEO01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVE      EOOTHR        @@OTHR
     C*** BEG ADD *** 03/03/99 ***************************************
     C                   MOVE      EORCN         XXRCN
     C*** END ADD *** 03/03/99 ***************************************
     C*** BEG ADD *** 03/15/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EOBLST        ANDEQ     'D'
     C                   MOVE      '2'           @@RC
     C                   ENDIF
     C*** END ADD *** 03/15/99 ***************************************
     C*** BEG ADD *** 01/21/99 ***************************************
     C     Q1PRMS        IFGT      8
     C*** BEG ADD *** 03/15/99 ***************************************
     C     ##AR          ANDEQ     'Y'
     C*** END ADD *** 03/15/99 ***************************************
     C*** BEG CHG *** 03/03/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C*** BEG CHG *** 03/15/99 ***************************************
     C     EAKY          CHAIN     PEA01                              98
     C     *IN98         IFEQ      *OFF
     C********** EAKY      CHAINPEA01                99
     C********** *IN99     IFEQ *OFF
     C*** END CHG *** 03/15/99 ***************************************
     C*** BEGIN ADD *** 03/03/00 **************************************
     C     Q1PRMS        IFGT      11
     C*
     C     @@FEIN        IFNE      EOFEIN
     C     @@MOD         ORNE      EOMOD
     C*** END   ADD *** 03/03/00 **************************************
     C*** BEGIN ADD *** 05/04/00 **************************************
     C     Q1PRMS        IFGT      13
     C*
     C                   SELECT
     C*
     C     @@CHK#        WHENEQ    EAWCBC
     C                   MOVE      *ZEROS        @@NINV
     C                   MOVE      EABINV        @@INV#
     C*
     C     @@CHK#        WHENEQ    EAWCNC
     C                   MOVE      EANINV        @@NINV
     C                   MOVE      *ZEROS        @@INV#
     C*
     C                   ENDSL
     C*
     C                   ELSE
     C*** END   ADD *** 05/04/00 **************************************
     C                   MOVE      EANINV        @@NINV
     C                   MOVE      EABINV        @@INV#
     C*** BEGIN ADD *** 05/04/00 **************************************
     C                   ENDIF
     C*** END   ADD *** 05/04/00 **************************************
     C*** BEGIN ADD *** 03/03/00 **************************************
     C                   ELSE
     C*
     C                   MOVE      *ZEROS        @@NINV
     C                   MOVE      *ZEROS        @@INV#
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVE      *ZEROS        @@NINV
     C                   MOVE      *ZEROS        @@INV#
     C*
     C                   ENDIF
     C*** END   ADD *** 03/03/00 **************************************
     C*** BEGIN DEL *** 04/06/00 **************************************
     C********** Q1PRMS    IFGT 10
     C**********           MOVE EARCN     @@RCN
     C**********           ENDIF
     C*** END   DEL *** 04/06/00 **************************************
     C*
     C                   ELSE
     C                   MOVE      *ZEROS        @@NINV
     C                   MOVE      *ZEROS        @@INV#
     C                   ENDIF
     C*
     C                   ELSE
     C*** BEG CHG *** 03/15/99 ***************************************
     C     TRKY          CHAIN     PTR01                              98
     C     *IN98         IFEQ      *OFF
     C********** TRKY      CHAINPTR01                99
     C********** *IN99     IFEQ *OFF
     C*** END CHG *** 03/15/99 ***************************************
     C                   MOVE      TRINV#        @@NINV
     C                   ELSE
     C                   MOVE      *ZEROS        @@NINV
     C                   ENDIF
     C*** BEGIN ADD *** 03/01/00 ***********************************************
     C                   MOVE      EOINV#        @@INV#
     C*** END   ADD *** 03/01/00 ***********************************************
     C*
     C                   ENDIF
     C*** END CHG *** 03/03/99 ***************************************
     C*
     C*** BEGIN DEL *** 03/01/00 ***********************************************
     C**********           MOVE EOINV#    @@INV#
     C*** END   DEL *** 03/01/00 ***********************************************
     C                   ENDIF
     C*** END ADD *** 01/21/99 ***************************************
     C                   ENDIF
     C*** BEGIN ADD *** 10/30/97 ***********************************************
     C     *IN99         IFEQ      '1'
     C                   MOVE      '1'           @@RC
     C                   ENDIF
     C*** END ADD ***** 10/30/97 ***********************************************
     C*
     C*  End of program
     C*
     C     ENDPGM        TAG
     C                   RETURN
