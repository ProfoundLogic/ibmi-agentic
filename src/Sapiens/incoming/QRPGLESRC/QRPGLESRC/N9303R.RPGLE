     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE N9303R - NETWORK VERIFY BILL AMOUNT GREATER THAN ZERO
     H*****************************************************************
     H**   PROGRAM: N7123R                                           **
     H**   DESCRIPTION: READS ALL RECORDS FOR A SUBSCRIBER THAT ARE  **
     H**                TO BE BILLED.  IF RECORD HAS A ZERO BILL     **
     H**                BILL AMOUNT UPDATES TRHRFL TO BE AN "H".     **
     H*****************************************************************
     H*  CALLED FROM:                                                 *
     H*    N9300C                                                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  PROGRAMMER: C:20               PROJECT: ARMNT                *
     H*  CREATED: 08/18/93                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  11/12/99 C:62  PROJECT: MCMNT
     H*                 ADD MODE PARM AND PASS TO AA001R.
     H*
     H*  03/18/99 C:94  PROJECT: 25MKL
     H*                 ADD RECON
     H*
     H*  01/06/99  C:94  PROJECT: MC152
     H*                  CHANGED PROGRAM TO USE API TO UPDATE
     H*                  NATRP AND FAEAP
     H*
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*****************************************************************
     F*** BEG CHG *** 01/06/99 ***************************************
     FNATRL     IF   E           K DISK
     FFAEAL1    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F*** BEG ADD *** 01/06/99 ***************************************
     F                                     INFDS(DBFDS)
     F*** END ADD *** 01/06/99 ***************************************
     F**********NATRL   UF  E           K        DISK
     F*** END CHG *** 01/06/99 ***************************************

     D*
     D*** BEG ADD *** 01/06/99 ***************************************
     D*  WRITE/UPDATE DATA STRUCTURE.
     D                 DS
     D  KKWRUP                 1      7
     D  XXFAEO                 1      1
     D  XXFAE1                 2      2
     D  XXFAHB                 3      3
     D  XXNAPB                 4      4
     D  XXNAMB                 5      5
     D  XXNATR                 6      6
     D  XXFAEA                 7      7
     D*** END ADD *** 01/06/99 ***************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** BEG ADD *** 01/06/99 ***************************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0

     I*** END ADD *** 01/06/99 ***************************************

     C***
     C*** BEGIN ADD *** 11/12/99 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@MODE            1
     C*** END   ADD *** 11/12/99 **************************************
     C*
     C*** BEG ADD *** 01/06/99 ***************************************
     C*
     C     CMKEY         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C*
     C     CMKY1         KLIST
     C                   KFLD                    TRCO#
     C                   KFLD                    TRSUB#
     C                   KFLD                    TRFND
     C                   KFLD                    TRFYR
     C                   KFLD                    TRCASE
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
     C     ##PRVB        IFEQ      'Y'
     C*
     C* READ NETWORK TRANSACTION FILE
     C*
     C                   READ      PEA01                                  60
     C**
     C     *IN60         DOWEQ     '0'
     C*
     C* IF THE BILL AMOUNT IS NOT GREATER THAN ZERO SET BACK TO A HOLD STATUS
     C*
     C     EANAMT        IFLE      *ZEROS
     C*
     C     CMKEY         CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN         ##RRN
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       KKWRUP
     C*** BEGIN CHG *** 11/12/99 **************************************
     C**********           MOVE 'U'       XXNATR
     C                   MOVE      'U'           XXFAEA
     C*** END   CHG *** 11/12/99 **************************************
     C*
     C                   CALL      'AA001R'
     C                   PARM                    ##RRN             9 0          RRN OF CASE
     C                   PARM      EAEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/18/99 ***************************************
     C                   PARM      EARCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/18/99 ***************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM                    ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM      'H'           ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM                    ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*** BEGIN ADD *** 11/12/99 **************************************
     C                   PARM      *BLANKS       ##BPFL            1                        ER
     C                   PARM      @@MODE        ##MODE            1                        ER
     C*** END   ADD *** 11/12/99 **************************************
     C*
     C                   ENDIF
     C*
     C                   READ      PTR01                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C                   ELSE                                                   ##PRVB = Y
     C*
     C*** END ADD *** 01/06/99 ***************************************
     C*
     C* READ NETWORK TRANSACTION FILE
     C*
     C                   READ      PTR01                                  60
     C**
     C     *IN60         DOWEQ     '0'
     C*
     C* IF THE BILL AMOUNT IS NOT GREATER THAN ZERO SET BACK TO A HOLD STATUS
     C*
     C     TRBAMT        IFLE      *ZEROS
     C*** BEG ADD *** 01/06/99 ***************************************
     C*
     C     CMKY1         CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN         ##RRN
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXNATR
     C*
     C                   CALL      'AA001R'
     C                   PARM                    ##RRN             9 0          RRN OF CASE
     C                   PARM      EAEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/18/99 ***************************************
     C                   PARM      EARCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/18/99 ***************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM                    ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM      'H'           ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM                    ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C                   ENDIF
     C*
     C*** END ADD *** 01/06/99 ***************************************
     C                   READ      PTR01                                  60
     C                   END                                                    END *IN60
     C*** BEG ADD *** 01/06/99 ***************************************
     C                   ENDIF                                                  ##PRVB = Y
     C*** END ADD *** 01/06/99 ***************************************
     C*********************************
     C* END OF PROGRAM                *
     C*********************************
     C                   MOVE      '1'           *INLR
