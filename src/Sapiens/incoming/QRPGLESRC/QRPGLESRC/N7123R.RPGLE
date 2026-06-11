     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE N7123R - PBR/HBA VERIFY BILL AMOUNT GREATER THAN ZERO
     H*****************************************************************
     H**   PROGRAM: N7123R                                           **
     H**   DESCRIPTION: READS ALL RECORDS FOR A SUBSCRIBER THAT ARE  **
     H**                TO BE BILLED.  IF RECORD HAS A ZERO BILL     **
     H**                BILL AMOUNT UPDATES EOHRFL TO BE AN "H".     **
     H*****************************************************************
     H*  CALLED FROM:                                                 *
     H*    N7120C                                                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  PROGRAMMER: C:20               PROJECT: ARMNT                *
     H*  CREATED: 08/18/93                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  11/11/99  C:62  PROJECT: MC216
     H*                  IF BILL RELEASED BUT NO T/M ARE RELEASED
     H*                  THEN PLACE BILL BACK ON HOLD. REHAB ONLY
     H*
     H*  07/27/99  C:62  PROJECT: MC201
     H*                  ADD LOGIC FOR NON-PROVIDER BILLS.
     H*
     H*  03/19/99  C:94  PROJECT: 25MKL
     H*                  IF PROVIDER BILLS IS INSTALLED THEN MAKE FAEAP
     H*                  THE MAIN FILE OTHERWISE MAKE FAEOP THE MAIN
     H*                  FILE
     H*
     H*  03/18/99  C:94  PROJECT: 25MKL
     H*                  ADD RECON #
     H*
     H*  01/04/99  C:94  PROJECT: MC152
     H*                  CHANGE PROGRAM TO USE API (AA001R) TO DO THE
     H*                  UPDATES TO NAMBP, NAPBP, AND FAHBP IF
     H*                  PROVIDER BILLS = N.  THIS MAY AFFECT SOME
     H*                  OF THE CHANGES MADE ON 07/05/95.
     H*
     H*  07/05/95  C:35    REQ/PROJ: NW008
     H*                    1.  ADD MEDICAL MANAGEMENT BILLING FILE
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*****************************************************************
     FFAEOL4    UF   E           K DISK
     FFAHBL     IF   E           K DISK
     FNAPBL1    IF   E           K DISK
     F*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     FNAMBL1    IF   E           K DISK
     F*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     F*** BEG ADD *** 01/04/99 ****************************************
     F*
     FFMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     FFAEAL1    UF   E           K DISK
     F*** BEGIN ADD *** 11/11/99 **************************************
     FNDTML11   IF   E           K DISK
     F*** END   ADD *** 11/11/99 **************************************
     F*
     F*** END ADD *** 01/04/99 ****************************************

     D*
     D*** BEG ADD *** 01/04/99 ***************************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
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
     D*
     D*** END ADD *** 01/04/99 ***************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C***
     C*
     C*
     C     HBKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     PBKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C     MBKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*** BEG ADD *** 01/04/99 ***************************************
     C     CMKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*
     C     EAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** BEGIN ADD *** 11/11/99 **************************************
     C*
     C     TMKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    EARCN
     C*** END   ADD *** 11/11/99 **************************************
     C*** END ADD *** 01/04/99 ***************************************
     C*** BEG ADD *** 01/04/98 ***************************************
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
     C*** END ADD *** 01/04/98 ***************************************
     C*
     C* READ EOB FILE
     C*** BEG ADD *** 03/19/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C                   READ      PEA01                                  60
     C     *IN60         DOWEQ     *OFF
     C*** BEGIN ADD *** 11/11/99 **************************************
     C     ##RHAB        IFEQ      'Y'
     C     EATMFL        ANDEQ     'Y'
     C*
     C     TMKY          CHAIN     PTM01                              98
     C*
     C                   ENDIF
     C*** END   ADD *** 11/11/99 **************************************
     C*
     C     EABAMT        IFLE      *ZEROS
     C*** BEGIN ADD *** 11/11/99 **************************************
     C     *IN98         OREQ      *ON
     C*** END   ADD *** 11/11/99 **************************************
     C                   MOVE      'H'           EABHRF
     C                   UPDATE    PEA01
     C                   ELSE
     C                   EXCEPT    RLSPEA
     C                   ENDIF
     C*
     C                   READ      PEA01                                  60
     C                   ENDDO
     C*
     C                   ELSE
     C*** END ADD *** 03/19/99 ***************************************
     C                   READ      PEO01                                  60
     C**
     C     *IN60         DOWEQ     '0'
     C*
     C* CHECK SYSTEM TYPE
     C*
     C     EOSYS         IFEQ      'P'
     C*
     C* IF THE BILL AMOUNT IS NOT GREATER THAN ZERO SET BACK TO A HOLD STATUS
     C*
     C     EOBAMT        IFLE      *ZEROS
     C                   MOVE      'H'           EOHRFL
     C                   UPDATE    PEO01
     C                   ELSE                                                   ELSE EOBAMT LE 0
     C                   EXCEPT    RLSPEO
     C                   END
     C*
     C                   ELSE                                                   ELSE EOSYS = "P"
     C*
     C     EOSYS         IFEQ      'H'
     C*
     C     HBKEY         CHAIN     PHB01                              61
     C*
     C     *IN61         IFEQ      *OFF
     C*
     C     HB1AAM        IFLE      *ZEROS
     C*
     C                   MOVE      'H'           ##BHRF
     C                   MOVE      'U'           XXFAHB
     C     CMKEY         CHAIN(N)  PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN         ##RRN
     C                   ENDIF
     C*
     C                   CALL      'AA001R'
     C                   PARM                    ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/18/99 ***************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/18/99 ***************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM                    ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C                   MOVE      'H'           EOHRFL
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVE      'H'           EOHRFL
     C                   UPDATE    PEO01
     C*
     C                   ENDIF                                                  *IN61
     C*
     C                   ELSE                                                   ELSE EOSYS = "H"
     C*
     C     EOSYS         IFEQ      'U'
     C*
     C     PBKEY         CHAIN     PPB01                              61
     C     *IN61         IFEQ      *OFF
     C*
     C* IF THE BILL AMOUNT IS NOT GREATER THAN ZERO SET BACK TO A HOLD STATUS
     C*
     C     PBBAMT        IFLE      *ZEROS
     C                   MOVE      'H'           ##BHRF
     C                   MOVE      'U'           XXNAPB
     C     CMKEY         CHAIN(N)  PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN         ##RRN
     C                   ENDIF
     C*
     C                   CALL      'AA001R'
     C                   PARM                    ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C**** BEG CHG *** 03/18/99 **************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C***********          PARM           ##RCN   30       RECON #
     C**** END CHG *** 03/18/99 **************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM                    ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C                   MOVE      'H'           EOHRFL
     C                   UPDATE    PEO01
     C                   ENDIF                                                  PBBAMT = 0
     C*
     C                   ELSE
     C*
     C                   MOVE      'H'           EOHRFL
     C                   UPDATE    PEO01
     C*
     C                   ENDIF                                                  *IN61
     C*
     C                   ELSE                                                   ELSE EOSYS = "U"
     C*
     C     EOSYS         IFEQ      'M'
     C*
     C     MBKEY         CHAIN     PMB01                              61
     C     *IN61         IFEQ      *OFF
     C*
     C* IF THE BILL AMOUNT IS NOT GREATER THAN ZERO SET BACK TO A HOLD STATUS
     C*
     C     MBBAMT        IFLE      *ZEROS
     C                   MOVE      'H'           ##BHRF
     C                   MOVE      'U'           XXNAMB
     C     CMKEY         CHAIN(N)  PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN         ##RRN
     C                   ENDIF
     C*
     C                   CALL      'AA001R'
     C                   PARM                    ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/18/99 ***************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/18/99 ***************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM                    ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C                   MOVE      'H'           EOHRFL
     C                   UPDATE    PEO01
     C                   ENDIF                                                  MBBAMT = 0
     C*
     C                   ELSE
     C*
     C                   MOVE      'H'           EOHRFL
     C                   UPDATE    PEO01
     C*
     C                   ENDIF                                                  *IN61
     C*
     C                   END                                                    END EOSYS = "M"
     C                   END                                                    END EOSYS = "U"
     C                   END                                                    END EOSYS = "H"
     C                   END                                                    END EOSYS = "P"
     C*
     C*** END ADD *** 01/04/99 ***************************************
     C                   READ      PEO01                                  60
     C                   END                                                    END *IN60
     C*** BEG ADD *** 03/19/99 ***************************************
     C                   ENDIF
     C*** END ADD *** 03/19/99 ***************************************
     C*********************************
     C* END OF PROGRAM                *
     C*********************************
     C                   MOVE      '1'           *INLR
     OPEO01     E            RLSPEO
     OPEA01     E            RLSPEA
