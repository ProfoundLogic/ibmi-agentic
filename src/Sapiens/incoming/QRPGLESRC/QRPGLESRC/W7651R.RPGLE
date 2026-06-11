     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7121R - Monthly Premium/Loss Report
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM IS RUN FROM THE MONTH END PROCESS AND LISTS      *
     H* EMPLOYER PREMIUM AND CLAIM AMOUNTS FROM THE BEGINNING OF      *
     H* THE YEAR TO THE SPECIFIED RUN DATE.                           *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W7651C                                                      *
     H* CALLS TO:                                                     *
     H*   SRPRST                                                      *
     H*   SRPRTD                                                      *
     H*                                                               *
     H* PROGRAMMER: KENNETH KENNEDY                                   *
     H* CREATED: 11/13/90               PROJECT: MDAAV                *
     H*                                                               *
     H* INDICATORS:                                                   *
     H*                                                               *
     H*   08  BATCH MODE                                              *
     H*   09  DEMAND MODE                                             *
     H*   11  CONDITIONS EXECUTION OF FIRST TIME SUBROUTINE           *
     H*   12  SET TO ONE IF ANY RECORDS WERE PROCESSED                *
     H*   60  GENERAL CHAIN INDICATOR                                 *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 05/23/11 C:136 SUPPORT: WA099 73548                           *
     H*                ADJUST A/R BALANCE CALC'S FOR NEW DEPOSIT      *
     H*                REFUND AND VOID CODES.                         *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 02/19/04 C:136 PROJECT: WC1033                                *
     H*                Add code for new A/R type of 'T' (True Void)   *
     H*                                                               *
     H*  02/13/02  C:15  Support: WC089/31261                         *
     H*                  Added code to exclude employers and claims   *
     H*                  entered after the run date.                  *
     H*                                                               *
     H*  12/14/99  C:118 PROJECT: WC577                               *
     H*                  Add spaces to Policy Year heading so 3 digit *
     H*                  date will display correctly.                 *
     H*                                                               *
     H*  11/12/98  C:90  SUPPORT: WA065/14038                         *
     H*            LOADED GROUP NAME BEFORE LOADING ROUTE INFO.       *
     H*                                                               *
     H*  10/27/97  C:86  VERSION 3 PROJECT: WC577                     *
     H*            REVISED TO PRINT THE ALTERNATE FUND NAME IN PLACE  *
     H*            OF COMPANY NAME, IF ONE EXISTS.                    *
     H*****************************************************************
     F*
     FWMCML5    IF   E           K DISK
     F* * * Claims File
     F*
     FWMEML     IF   E           K DISK
     F* * * Employer File
     F*
     FWMFNL     IF   E           K DISK
     F* * * Fund File
     F*
     FWDF1L     IF   E           K DISK
     F* * * Fund Detail File
     F*
     FWMCHL     IF   E           K DISK
     F* * * Claim History File
     F*
     FSMORL     IF   E           K DISK
     F* * * Batch Routing File
     F*
     FSMCOL     IF   E           K DISK
     F* * * Company File
     F*
     FWMUPL     IF   E           K DISK
     F* * * User Parameter File
     F*
     FWDELL1    IF   E           K DISK
     F* * * Employer Detail File
     F*
     FWMEAL8    IF   E           K DISK
     F* * * Employer A/R File
      *** BEG ADD ***** 05/23/11 **************************************
     FWTABL     IF   E           K DISK
      *** END ADD ***** 05/23/11 **************************************
     F*
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F* * * Output File
     F*
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOA)
     F* * * Output File

     D*
     D                 DS
     D  CMA                    1     90  2
     D                                     DIM(10)                              PMT AMOUNT
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  CMR                    1     90  2
     D                                     DIM(10)                              RESERVE AMOUNT
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE HISTORY FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXYMD
     C*
     C*  KEY LIST FOR FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE EMPLOYER A/R FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
      *** BEG ADD ***** 05/23/11 **************************************
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
      *** END ADD ***** 05/23/11 **************************************
     C*
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CHEFDT        XXYMD
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ORTITL        XXTITL
     C     *LIKE         DEFINE    ORTITL        X1TITL
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQDTE             8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C***** BEGIN DEL **** 12/14/99 ***********************************
     C**********           MOVE QQFYR     XXFYR2  20
     C***** END   DEL **** 12/14/99 ***********************************
     C                   MOVE      QQDTE         XXDATE            8 0
     C                   MOVE      QQMODE        XXMODE            1
     C*
     C*  FIRST CYCLE PROCESSING
     C*
     C  N11              EXSR      SR1ST
     C*
     C*  GET FUND RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C*** BEGIN ADD *** 10-27-97 *******************************
     C                   MOVEL(P)  FNNAME        XFNAME           60
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL(P)  FNANAM        XCNAME           40
     C                   MOVE      *ON           *IN39
     C                   ENDIF
     C*** END ADD ***********************************************
     C*
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  READ EMPLOYER DETAIL FILE
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*** Begin add *** 02/13/02 **************************************
     C     ELIDT         IFLE      XXYMD
     C*** End add ***** 02/13/02 **************************************
     C*
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   Z-ADD     ELDIV         XXDIV
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      *BLANKS       XXEMNM           24
     C                   MOVEL     EMNAME        XXEMNM
     C*
     C*  READ CLAIM FILE
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*** Begin add *** 02/13/02 **************************************
     C     CMIDT         IFLE      XXYMD
     C*** End add ***** 02/13/02 **************************************
     C*
     C                   MOVE      CMCASE        XXCASE
     C*
     C*  GET HISTORY FILE, DETERMINE IF OPEN OR CLOSED
     C*
     C     CHKEY         SETGT     PCH01
     C                   READP     PCH01                                  60
     C     *IN60         IFEQ      '1'
     C     CHCO#         ORNE      XXCO#
     C     CHFND         ORNE      XXFND
     C     CHFYR         ORNE      XXFYR
     C     CHCASE        ORNE      XXCASE
     C                   MOVE      CMSTAT        CHOCST
     C                   END
     C*
     C     CHOCST        IFEQ      'O'
     C                   ADD       1             L1OPEN
     C                   END
     C                   ADD       1             L1CLMS
     C*
     C*  ACCUMULATE PAYMENT AMOUNTS
     C*
     C                   XFOOT     CMA           XXPTOT
     C                   ADD       XXPTOT        L1PTOT
     C*
     C*  ACCUMULATE RESERVE AMOUNTS
     C*
     C                   XFOOT     CMR           XXRTOT
     C                   ADD       XXRTOT        L1RTOT
     C*
     C*** Begin add *** 02/13/02 **************************************
     C                   ENDIF                                                  CMIDT <= XXYMD
     C*** End add ***** 02/13/02 **************************************
     C*
     C     CMKEY         READE     PCM01                                  60
     C*
     C                   END
     C*
     C                   EXSR      SUBL1
     C*
     C                   END
     C*
     C*** Begin add *** 02/13/02 **************************************
     C                   ENDIF                                                  ELIDT <= XXYMD
     C*** End add ***** 02/13/02 **************************************
     C*
     C     ELKEY         READE     PEL01                                  60
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C     *IN12         IFEQ      '0'
     C                   EXCEPT    NORECS
     C                   GOTO      TERM
     C                   END
     C*
     C                   EXSR      SUBL2
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
      /Free
       // ***** Beg Add ***** 08/26/14 ***********************************
       Except Route;
       Except Route1;
       // ***** End Add ***** 08/26/14 ***********************************
      /End-Free
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*==================================================================*
     C*   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
     C*==================================================================*
     C*
     C     SR1ST         BEGSR
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X1DATE            6 0
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C* INITIALIZE FIELDS
     C*
     C                   TIME                    DSDTTM
     C*
     C     COKEY         CHAIN     SMCOL                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C**** BEGIN ADD **** 11/12/98 ************************************
     C     FNKEY         CHAIN     PFN01                              60
     C**** END   ADD **** 11/12/98 ************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      ORTITL        XXTITL
     C                   ELSE
     C                   MOVE      *BLANKS       XXTITL
     C                   END
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      ORTITL        X1TITL
     C                   ELSE
     C                   MOVE      *BLANKS       X1TITL
     C                   END
     C*
     C                   Z-ADD     *ZEROS        XXPTOT           11 2
     C                   Z-ADD     *ZEROS        XXRTOT           11 2
     C                   Z-ADD     *ZEROS        L1PTOT           11 2
     C                   Z-ADD     *ZEROS        L1RTOT           11 2
     C                   Z-ADD     *ZEROS        L1STPR           11 2
     C                   Z-ADD     *ZEROS        L1NMPR           11 2
     C                   Z-ADD     *ZEROS        L1ESPR           11 2
     C                   Z-ADD     *ZEROS        L1ENPR           11 2
     C                   Z-ADD     *ZEROS        L1PCOL           11 2
     C                   Z-ADD     *ZEROS        L1CLMS            5 0
     C                   Z-ADD     *ZEROS        L1OPEN            5 0
     C                   Z-ADD     *ZEROS        L2PTOT           11 2
     C                   Z-ADD     *ZEROS        L2RTOT           11 2
     C                   Z-ADD     *ZEROS        L2STPR           11 2
     C                   Z-ADD     *ZEROS        L2NMPR           11 2
     C                   Z-ADD     *ZEROS        L2ESPR           11 2
     C                   Z-ADD     *ZEROS        L2ENPR           11 2
     C                   Z-ADD     *ZEROS        L2PCOL           11 2
     C                   Z-ADD     *ZEROS        L2CLMS            5 0
     C                   Z-ADD     *ZEROS        L2OPEN            5 0
     C*
      ***** Beg Del ***** 08/26/14 ****************************************
     C************       EXCEPT    ROUTE
     C************       EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 ****************************************
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C*
     C*  NO RECORDS PROCESSED YET
     C*
     C                   MOVE      '0'           *IN12                                      IC.
     C*
     C                   ENDSR
     C*
     C*
     C*==================================================================*
     C*   Premium Calculations             ** Subroutine **   S U B P R  *
     C*==================================================================*
     C*
     C     SUBPR         BEGSR
     C*
     C*  CALL PGM TO CALCULATE EARNED STANDARD PREMIUM
     C*
     C                   MOVEL     EMCO#         SRCO#             3
     C                   MOVEL     EMFND         SRFND             3
     C                   MOVEL     ELFYR         SRFYR             3
     C                   MOVEL     EMEMP#        SREMP#            9
     C                   MOVEL     EMDIV         SRDIV             5
     C                   MOVEL     XXDATE        QQDTE             8
     C                   MOVE      *BLANK        SRAMT            11
     C                   MOVE      *BLANK        SRAMT2           11
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRPRST'
     C                   PARM                    SRCO#
     C                   PARM                    SRFND
     C                   PARM                    SRFYR
     C                   PARM                    SREMP#
     C                   PARM                    SRDIV
     C                   PARM                    QQDTE
     C                   PARM                    SRAMT
     C                   PARM                    SRAMT2
     C                   PARM                    QQTERM
     C                   MOVE      SRAMT         L1ESPR
     C*
     C*
     C*  CALL PGM TO CALCULATE EARNED NORMAL PREMIUM
     C*
     C                   MOVEL     EMCO#         SRCO#             3
     C                   MOVEL     EMFND         SRFND             3
     C                   MOVEL     ELFYR         SRFYR             3
     C                   MOVEL     EMEMP#        SREMP#            9
     C                   MOVEL     EMDIV         SRDIV             5
     C                   MOVEL     XXDATE        QQDTE             8
     C                   MOVE      *BLANK        SRAMT            11
     C                   MOVE      *BLANK        SRAMT2           11
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRPRTD'
     C                   PARM                    SRCO#
     C                   PARM                    SRFND
     C                   PARM                    SRFYR
     C                   PARM                    SREMP#
     C                   PARM                    SRDIV
     C                   PARM                    QQDTE
     C                   PARM                    SRAMT
     C                   PARM                    SRAMT2
     C                   PARM                    QQTERM
     C                   MOVE      SRAMT         L1ENPR
     C*
     C*  CALCULATE ANNUAL STANDARD PREMIUM AND ANNUAL NORMAL PREMIUM
     C*
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELASPR        L1STPR
     C                   Z-ADD     ELANMP        L1NMPR
     C                   ELSE
     C                   Z-ADD     ELSTPR        L1STPR
     C                   Z-ADD     ELENMP        L1NMPR
     C                   END
     C*
     C*  CALCULATE PREMIUM COLLECTED
     C*
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD ***** 05/23/11 **********************************
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'P'
      *** END ADD ***** 05/23/11 **********************************
     C*
     C     EAARTY        IFEQ      'P'
     C*** BEG ADD *******  02/19/04  **********************************
     C     EAARTY        OREQ      'T'
     C*** END ADD *******  02/19/04  **********************************
     C                   ADD       EAAMT         L1PCOL
     C                   ELSE
     C     EAARTY        IFEQ      'N'
     C     EAARTY        OREQ      'V'
     C     EAARTY        OREQ      'R'
     C                   SUB       EAAMT         L1PCOL
     C                   END
     C                   END
      *** BEG ADD ***** 05/23/11 **********************************
     C                   ENDIF                                                  TTCTYP = 'P'
      *** BEG ADD ***** 05/23/11 **********************************
     C*
     C     EAKEY         READE     PEA01                                  60
     C                   END
     C*
     C                   ENDSR
     C**
     C*==================================================================*
     C*   Level 1 Calculations             ** Subroutine **   S U B L 1  *
     C*==================================================================*
     C*
     C     SUBL1         BEGSR
     C*
     C                   EXSR      SUBPR
     C*
     C                   ADD       L1PTOT        L2PTOT
     C                   ADD       L1RTOT        L2RTOT
     C                   ADD       L1STPR        L2STPR
     C                   ADD       L1NMPR        L2NMPR
     C                   ADD       L1ESPR        L2ESPR
     C                   ADD       L1ENPR        L2ENPR
     C                   ADD       L1PCOL        L2PCOL
     C                   ADD       L1CLMS        L2CLMS
     C                   ADD       L1OPEN        L2OPEN
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   EXCEPT    L1TOTL
     C                   MOVE      '1'           *IN12
     C*
     C                   Z-ADD     *ZEROS        L1PTOT           11 2
     C                   Z-ADD     *ZEROS        L1RTOT           11 2
     C                   Z-ADD     *ZEROS        L1STPR           11 2
     C                   Z-ADD     *ZEROS        L1NMPR           11 2
     C                   Z-ADD     *ZEROS        L1ESPR           11 2
     C                   Z-ADD     *ZEROS        L1ENPR           11 2
     C                   Z-ADD     *ZEROS        L1PCOL           11 2
     C                   Z-ADD     *ZEROS        L1CLMS            5 0
     C                   Z-ADD     *ZEROS        L1OPEN            5 0
     C*
     C                   ENDSR
     C*
     C*==================================================================*
     C*   Level 2 Calculations  (FUND)     ** Subroutine **   S U B L 2  *
     C*==================================================================*
     C*
     C     SUBL2         BEGSR
     C*
     C                   EXSR      SUBPR
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   EXCEPT    L2TOTL
     C                   EXCEPT    L2TOT1
     C*
     C                   Z-ADD     *ZEROS        L1PTOT           11 2
     C                   Z-ADD     *ZEROS        L1RTOT           11 2
     C                   Z-ADD     *ZEROS        L1STPR           11 2
     C                   Z-ADD     *ZEROS        L1NMPR           11 2
     C                   Z-ADD     *ZEROS        L1ESPR           11 2
     C                   Z-ADD     *ZEROS        L1ENPR           11 2
     C                   Z-ADD     *ZEROS        L1PCOL           11 2
     C                   Z-ADD     *ZEROS        L1CLMS            5 0
     C                   Z-ADD     *ZEROS        L1OPEN            5 0
     C                   Z-ADD     *ZEROS        L2PTOT           11 2
     C                   Z-ADD     *ZEROS        L2RTOT           11 2
     C                   Z-ADD     *ZEROS        L2STPR           11 2
     C                   Z-ADD     *ZEROS        L2NMPR           11 2
     C                   Z-ADD     *ZEROS        L2ESPR           11 2
     C                   Z-ADD     *ZEROS        L2ENPR           11 2
     C                   Z-ADD     *ZEROS        L2PCOL           11 2
     C                   Z-ADD     *ZEROS        L2CLMS            5 0
     C                   Z-ADD     *ZEROS        L2OPEN            5 0
     C*
     C                   ENDSR
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ****************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ****************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
      ***** Beg Del ***** 08/26/14 ****************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ****************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                47 'VE DATA SOLUTIONS. INC.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O              N39      CONAME             119
     O               39      XCNAME             119
     O                       DSDATE        Y    198
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       XXTITL             129
     O                                          194 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD           2
      **** BEGIN CHG **** 07/29/98 *********************************************
     O********************************* 104 'POLICY YEAR 19'
     O************************ XXFYR2   106
     O                                          104 'POLICY YEAR '
     O***** BEGIN CHG **** 12/14/99 ***********************************
     O**********               XXFYR Z  106
     O                       XXFYR         Z    107
     O***** END   CHG **** 12/14/99 ***********************************
      **** END   CHG **** 07/29/98 *********************************************
     O                                          189 'RUN DATE:'
     O                       X1DATE        Y    198
     O          E            HEAD           2
     O                                            6 'GROUP:'
     O                       XXFND         Z     10
     O                       FNNAME              71
     O          E            HEAD           1
     O                                           64 'CLAIMS PAID'
     O                                           82 'RESERVES ON'
     O                                           98 'ANNUAL'
     O                                          116 'ANNUAL'
     O                                          134 'EARNED'
     O                                          152 'EARNED'
     O                                          171 'PREMIUM'
     O                                          184 'TOTAL'
     O                                          196 'TOTAL'
     O          E            HEAD           2
     O                                            9 'EMPLR'
     O                                           18 'DIV'
     O                                           39 'EMPLOYER NAME'
     O                                           63 'TO DATE'
     O                                           82 'OPEN CLAIMS'
     O                                           99 'STD PREM'
     O                                          117 'NORM PREM'
     O                                          135 'STD PREM'
     O                                          152 'NORMAL'
     O                                          172 'COLLECTED'
     O                                          184 'CLAIMS'
     O                                          198 'OPEN CLMS'
     O          E            L1TOTL         1
     O                       EMEMP#        Z      9
     O                       EMDIV         Z     19
     O                       XXEMNM              48
     O                       L1PTOT        J     66
     O                       L1RTOT        J     84
     O                       L1STPR        J    102
     O                       L1NMPR        J    120
     O                       L1ESPR        J    138
     O                       L1ENPR        J    156
     O                       L1PCOL        J    174
     O                       L1CLMS        4    182
     O                       L1OPEN        4    196
     O          E            L2TOTL      1  2
     O                                            5 'GROUP'
     O                       XXFND                9
     O                                           17 'TOTALS:'
     O                       L2PTOT        J     67
     O                       L2RTOT        J     85
     O                       L2STPR        J    103
     O                       L2NMPR        J    121
     O                       L2ESPR        J    139
     O                       L2ENPR        J    157
     O                       L2PCOL        J    175
     O                       L2CLMS        4    183
     O                       L2OPEN        4    197
     O          E            NORECS         1
     O                                           25 '*** NO RECORDS MEET'
     O                                           47 'SELECTED CRITERIA ***'
     O*
     OQSYSPRT1  E            ROUTE1         1 06
      ***** Beg Del ***** 08/26/14 ****************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE1         1
     O**********     08      ORDIS2              60
     O**********E            ROUTE1         1
     O**********     08      ORDIS3              60
     O**********E            ROUTE1         2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ****************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE1         1
     O               08      ORDIS2              60
     O          E            ROUTE1         1
     O               08      ORDIS3              60
     O          E            ROUTE1         2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       X1TITL              80
      ***** Beg Del ***** 08/26/14 ****************************************
     O**********E            ROUTE1         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE1         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ****************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE1
     O****                               23 '(C) COPYRIGHT PROGRESSI'
     O****                               47 'VE DATA SOLUTIONS. INC.,'
     O                                         +000 '(C)'
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O              N39      CONAME              60
     O               39      XCNAME              60
     O                       DSDATE        Y     80
     O          E            HEAD1          1
     O                       Q1PGM               10
     O                       X1TITL              70
     O                                           76 'PAGE:'
     O                       PAGE1         Z     80
     O          E            HEAD1          2
      **** BEGIN CHG **** 07/29/98 *********************************************
     O********************************** 45 'POLICY YEAR 19'
     O************************ XXFYR2    47
     O                                           45 'POLICY YEAR '
     O***** BEGIN CHG **** 12/14/99 ***********************************
     O**********               XXFYR Z   47
     O                       XXFYR         Z     48
     O***** END   CHG **** 12/14/99 ***********************************
      **** END   CHG **** 07/29/98 *********************************************
     O                                           71 'RUN DATE:'
     O                       X1DATE        Y     80
     O          E            HEAD1          3
     O                                            6 'GROUP:'
     O                       XXFND         Z     10
     O**** BEGIN CHG **** 11/12/98 ************************************
     O************************ FNNAME    70
     O                       FNNAME              71
     O**** END   CHG **** 11/12/98 ************************************
     O          E            L2TOT1         2
     O                                           17 'TOTAL CLAIMS PAID'
     O                                           26 'TO DATE:'
     O                       L2PTOT        J     60
     O          E            L2TOT1         2
     O                                           24 'RESERVES ON OPEN CLAIMS:'
     O                       L2RTOT        J     60
     O          E            L2TOT1         2
     O                                           24 'ANNUAL STANDARD PREMIUM:'
     O                       L2STPR        J     60
     O          E            L2TOT1         2
     O                                           22 'ANNUAL NORMAL PREMIUM:'
     O                       L2NMPR        J     60
     O          E            L2TOT1         2
     O                                           24 'EARNED STANDARD PREMIUM:'
     O                       L2ESPR        J     60
     O          E            L2TOT1         2
     O                                           22 'EARNED NORMAL PREMIUM:'
     O                       L2ENPR        J     60
     O          E            L2TOT1         2
     O                                           18 'PREMIUM COLLECTED:'
     O                       L2PCOL        J     60
     O          E            L2TOT1         2
     O                                           22 'TOTAL NUMBER OF CLAIMS'
     O                                           33 'REPORTED:'
     O                       L2CLMS        4     56
     O          E            L2TOT1         2
     O                                           20 'TOTAL NUMBER OF OPEN'
     O                                           28 'CLAIMS:'
     O                       L2OPEN        4     56
     O          E            NORECS         1
     O                                           25 '*** NO RECORDS MEET'
     O                                           47 'SELECTED CRITERIA ***'
