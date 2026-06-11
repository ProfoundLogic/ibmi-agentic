     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA012R - API:  Payment Edit II
     H*****************************************************************
     H*  DESCRIPTION:
     H*
     H*    This routinue will check for all scenarios listed below in
     H*    the order listed below.
     H*
     H*    If Requested Amount passes ALL edits, the Return Code will
     H*    be Blank.  Otherwise, the first edit error encountered will
     H*    assign the letter to the Return Code and the API will exit.
     H*
     H* ------
     H* Return
     H* Code      Edit Description
     H* ------
     H* 06 -Claim Master record check.
     H* 08 -Claim/DARTS Control record check.
     H*
     H* 10 -Payment Code must be valid and active.
     H* 11 -Reserve Code must be valid and active.
     H****12 -Reserve Code must be valid for the claim.
     H* 13 -Reserve Type 'C' only valid if claim Lost Time is 'I'
     H* 14 -If the claim indicates a 'Y' in the Controvert flag, then only
     H*     allow payments to be released if the reserve type is equal to
     H*     one of the values defined in the Claim/DARTS Control file.
     H* 15 -If reserve type is not 'C', FEIN is required
     H* 16 -If entered and FEIN is not equal to claimant SS#,
     H*     Provider FEIN/MOD must be active in the provider file.
     H* 17 -FEIN is equal to claimant SS#.
     H*     SS# must be active in the Claimant Demographics file.
     H****21 -If the claim is closed, don't allow check release if there is
     H****  no entry in the reserve cushion file for the reserve code and
     H****  claim type (indemnity vs medical only).
     H* 41 -If claim is record only, do not allow a pmt to be entered.
     H* 44 -Transaction Code must be a Compensation Type
     H* 48 -Payment type code security verification
     H*
     H*
     H*
     H*  CREATED: 07/02/97  C:30  Project:  WC660
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  02/06/25 C:333 Support: 118454                               *
     H*                 - Medical/Travel Reimbursement Request        *
     H*                   Commenting FEIN/MOD edit at payement release*
     H*                                                               *
     H*  12/29/09 C:234 Support 71016 WA071:  RE-INTRODUCE EDIT 15    *
     H*                                                               *
     H*  07/20/09 C:53  Support 69219                                 *
     H*                 Execute edit 16 on a negative #               *
     H*                 ZERO is allowed                               *
     H*                                                               *
     H*  10/18/07 C:180 Support 60496 - Remove edit 15                *
     H*                                                               *
     H*  07/10/03 C:15 Project: WC1135                                *
     H*                Added edit #48 to determine if a user has      *
     H*                authority to use the pmt type code.            *
     H*                                                               *
     H*  06/01/01 C:15 Project: WC577                                 *
     H*                Added edit #41 to prevent pended pmts from     *
     H*                being entered on 'record only' claims.         *
     H*                                                               *
     H*  05/04/00 C:15 Project: WC991                                 *
     H*                Removed edits #12 and #21 to allow pended pmts *
     H*                on claims with no reserve codes established.   *
     H*                                                               *
     H*  05/28/98 C:15 Project: WC659                                 *
     H*                Changed the edits on the payee to allow other  *
     H*                payees on comp pmts.                           *
     H*                                                               *
     H*  12/23/97 c:30 Project: WC659                                 *
     H*           Add PARM for pgm name ('W8080R' vs. all other paymnt*
     H*           pgms.  Add Error #44 for W8080R                     *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
      *  Claim Master file
     FWMCML1    IF   E           K DISK
      *________________________________________________________________
      *  Claim/DARTS Control file
     FWMCXL     IF   E           K DISK
      *________________________________________________________________
      *  Payee Demographics file
     FWMPDL     IF   E           K DISK
      *________________________________________________________________
     F*  Claimant Demographics file
     FWMCDL     IF   E           K DISK
      *________________________________________________________________
      *  Table file
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRC01)
      *________________________________________________________________
     F*** BEGIN DLT *** 05/04/00 **************************************
     F*  Reserve Cushion Definition -     KEY = CO#/FND/RESV/EFDT
     F******WDRCL   IF  E           K        DISK
     F*________________________________________________________________
     F*  Claim History file         -     KEY = CO#/FND/FYR/CASE/EFDT
     F******WMCHL   IF  E           K        DISK
     F*** END DLT ***** 05/04/00 **************************************
     F*** Begin add *** 07/10/03 **************************************
      *  User Parameter file
     FWMUPL     IF   E           K DISK
     F*** End add ***** 07/10/03 **************************************
      *________________________________________________________________
      *________________________________________________________________
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
      *  Claim/DARTS Control file Valid Controvert Reserve Types
      *
     D                 DS
     D  XRT                    1      4
     D                                     DIM(4)                               Controvert RTs
     D  CXRT01                 1      1
     D  CXRT02                 2      2
     D  CXRT03                 3      3
     D  CXRT04                 4      4
      *
      *  Claim Reserve Type
      *
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)                              Reserve Type
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
      *
      *  Claim Reserve Amount
      *
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)                              Remaining Reserve
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
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *________________________________________________________________
      *
      *  Table file key
      *

     C     TBKEY         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    XXKEY            10
      *
      *  Claim/DARTS Control file key
      *
     C     CXKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
      *
      *  Claim Master key
      *
     C     CMKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
      *
      *  Payee Demographics key
      *
     C     PDKY          KLIST
     C                   KFLD                    @@FEIN
     C                   KFLD                    @@MOD
      *
     C*** BEGIN DLT *** 05/04/00 **************************************
     C*   Reserve Cushion Definition file key
     C**********
     C********** RCKY      KLIST
     C**********           KFLD           @@CO#
     C**********           KFLD           @@FND
     C**********           KFLD           PCRECD
     C**********           KFLD           XXEFDT
     C**********
     C********** RCKY1     KLIST
     C**********           KFLD           @@CO#
     C**********           KFLD           @@FND
     C**********           KFLD           PCRECD
     C**********
     C*   Claim History file
     C**********
     C********** CHKY      KLIST
     C**********           KFLD           @@CO#
     C**********           KFLD           @@FND
     C**********           KFLD           @@FYR
     C**********           KFLD           @@CASE
     C*** END DLT ***** 05/04/00 **************************************
      *________________________________________________________________
     C     *LIKE         DEFINE    CMCO#         @@CO#
     C     *LIKE         DEFINE    CMFND         @@FND
     C     *LIKE         DEFINE    CMFYR         @@FYR
     C     *LIKE         DEFINE    CMCASE        @@CASE
     C     *LIKE         DEFINE    PDFEIN        @@FEIN
     C     *LIKE         DEFINE    PDMOD         @@MOD
     C*** BEGIN DLT *** 05/04/00 **************************************
     C********** *LIKE     DEFN CHEFDT    XXEFDT
     C********** *LIKE     DEFN RCMAMT    XXCUSH
     C*** END DLT ***** 05/04/00 **************************************
      *________________________________________________________________
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *________________________________________________________________
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#                          Company
     C                   PARM                    @@FND                          Group
     C                   PARM                    @@FYR                          Fund Year
     C                   PARM                    @@CASE                         Claim
     C                   PARM                    @@TRCD            2            Payment Code
     C                   PARM                    @@FEIN                         FEIN #
     C                   PARM                    @@MOD                          MOD #
     C                   PARM                    @@RC              2            Return Code
     C                   PARM                    @@RECD            2            Reserve Code
     C                   PARM                    @@RETP            1            Reserve Type
     C                   PARM                    @@TYPE            1            Trans Type
      *** BEGIN ADD *** 12/23/97 ***********************************************
     C                   PARM                    @@PGM            10            Program Name
      *
     C     Q1PRMS        IFEQ      12
     C                   MOVEL     @@PGM         XXPGM            10
     C                   ELSE
     C                   MOVE      *BLANK        XXPGM
     C                   ENDIF
      *
      *** END   ADD *** 12/23/97 ***********************************************
      *________________________________________________________________
      *
     C                   MOVE      *BLANK        @@RC
      *________________________________________________________________
      *
      *  Edit 06:  Claim Master record check
      *
      *  Access Claim Master                CO#/FND/FYR/CASE
      *
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '06'          @@RC
     C                   RETURN
     C                   ENDIF
      *
      *________________________________________________________________
     C*** Begin add *** 06/01/01 **************************************
      *  Edit 41:  If the claim is a "record only" type, no pmts are
      *            allowed.
      *
     C     CMLTFL        IFEQ      'R'
     C                   MOVE      '41'          @@RC
     C                   RETURN
     C                   ENDIF
     C*** End add ***** 06/01/01 **************************************
      *________________________________________________________________
      *
      *  Edit 08:  Claim/DARTS Control record check
      *
      *  Access Claim/DARTS Control file    CO#/FND
      *
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '08'          @@RC
     C                   RETURN
     C                   ENDIF
      *________________________________________________________________
      *
      *  Edit 10:  Payment Code must be valid and active
      *
      *  Access Table file                  CO#/KEY
      *
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     @@TRCD        XXKEY
      *
     C     TBKEY         CHAIN     PPC01                              60
      *
     C     PCSTAT        IFEQ      'I'
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C     *IN60         IFEQ      *ON
     C                   MOVE      '10'          @@RC
     C                   RETURN
     C                   ENDIF
      *________________________________________________________________
      *
      *  Edit 11:  Reserve Code must be valid and active
      *
      *  Access Table file                  CO#/KEY
      *
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     PCRECD        XXKEY
      *
     C     TBKEY         CHAIN     PRT01                              60
      *
     C     PCTRTP        IFNE      'Z'                                          REVERSAL
     C     PCTRTP        ANDNE     'S'                                          STOP
     C     PCTRTP        ANDNE     'V'                                          VOID
     C*
     C     RTSTAT        IFEQ      'I'
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C     *IN60         IFEQ      *ON
     C                   MOVE      '11'          @@RC
     C                   RETURN
     C                   ENDIF
     C*
     C                   ENDIF                                                  PCTRTP<>Z/S/V
      *________________________________________________________________
     C*** BEGIN DLT *** 05/04/00 **************************************
     C**********
     C*  Edit 12:  Reserve Code must be valid for the claim.
     C**********
     C**********           Z-ADD1         X
     C********** PCRECD    LOKUPRT,X                     60
     C**********
     C********** PCTRTP    IFNE 'Z'                        REVERSAL
     C********** PCTRTP    ANDNE'S'                        STOP
     C********** PCTRTP    ANDNE'V'                        VOID
     C**********
     C********** *IN60     IFEQ *OFF
     C**********           MOVE '12'      @@RC
     C**********           RETRN
     C**********           ENDIF
     C**********
     C**********           ENDIF                           PCTRTP<>Z/S/V
     C*** END DLT ***** 05/04/00 **************************************
      *________________________________________________________________
      *
      *  Edit 13:  Reserve Type 'C' only valid if claim Lost Time is 'I'
      *
     C     PCTRTP        IFNE      'Z'                                          REVERSAL
     C     PCTRTP        ANDNE     'S'                                          STOP
     C     PCTRTP        ANDNE     'V'                                          VOID
     C*
     C     RTTYPE        IFEQ      'C'
     C     CMLTFL        ANDNE     'I'
     C                   MOVE      '13'          @@RC
     C                   RETURN
     C                   ENDIF
     C*
     C                   ENDIF                                                  PCTRTP<>Z/S/V
      *________________________________________________________________
      *** BEGIN ADD *** 12/23/97 ***********************************************
      *
      *  Edit 44:  If Calling pgm is W8080R, then Reserve Type must be 'C'
      *
     C     XXPGM         IFEQ      'W8080R'
     C     RTTYPE        ANDNE     'C'
     C                   MOVE      '44'          @@RC
     C                   RETURN
     C                   ENDIF
     C*
      *** END   ADD *** 12/23/97 ***********************************************
      *________________________________________________________________
      *
      *  Edit 14:  If Claim Controvert Flag 'Y', only allow payments to be
      *           released if the reserve type is equal to one of the
      *           values defined in the Claim/DARTS Control file values.
      *
     C     PCTRTP        IFEQ      'M'
     C     PCTRTP        OREQ      'P'
     C*
     C     CMCVFL        IFEQ      'Y'                                          Controvert Flag
     C                   Z-ADD     1             X                 3 0
     C     RTTYPE        LOOKUP    XRT(X)                                 40     Equal
      *
     C     *IN40         IFEQ      *OFF                                          Not Found
     C                   MOVE      '14'          @@RC
     C                   RETURN
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  PCTRTP=P/M
      *________________________________________________________________
     C     Q1PRMS        IFGT      8
     C                   MOVE      PCRECD        @@RECD                         Reserve Code
     C                   MOVE      RTTYPE        @@RETP                         Reserve Type
     C                   MOVE      PCTRTP        @@TYPE                         Trans Type
     C                   ENDIF
      *________________________________________________________________
     C*** BEGIN DLT *** 05/04/00 **************************************
     C**********
     C*  Edit 21:  If the claim is closed, don't allow check release if
     C*     there is no entry in the reserve cushion file for the reserve
     C*     code and claim type (indemnity vs medical only).
     C**********
     C********** PCTRTP    IFEQ 'M'
     C********** PCTRTP    OREQ 'P'
     C**********
     C********** CMSTAT    IFEQ 'C'
     C**********
     C*  Get Effective Date of Closing
     C**********
     C********** CHKY      SETGTPCH01
     C********** CHKY      REDPEPCH01                    60
     C**********
     C********** *IN60     IFEQ '0'
     C********** 'C'       ANDEQCHOCST
     C**********
     C*   Check to see if Payment Type will allow Payment due to a
     C*   Reserve Cushion. The Reserve Code associated with the
     C*   Payment Type msut exist in the Reserve Cushion file.
     C**********
     C**********           Z-ADDCHEFDT    XXEFDT
     C**********
     C**********           Z-ADD*ZERO     XXCUSH
     C********** RCKY      SETGTPRC01
     C********** RCKY1     REDPEPRC01                    60
     C********** *IN60     IFEQ '0'
     C**********
     C**********           SELEC
     C**********
     C********** CMLTFL    WHEQ 'I'
     C**********           Z-ADDRCLAMT    XXCUSH           LOST TIME AMT
     C**********
     C********** CMLTFL    WHEQ 'M'
     C********** CMLTFL    OREQ 'R'
     C**********           Z-ADDRCMAMT    XXCUSH           MED ONLY AMT
     C**********
     C**********           ENDSL
     C**********
     C**********           ENDIF                           RCKY = '0'
     C**********
     C********** XXCUSH    IFEQ *ZERO
     C**********           MOVE '21'      @@RC
     C**********           RETRN
     C**********           ENDIF
     C**********
     C**********           ENDIF                           CHKY = '0'
     C**********           ENDIF                           CMSTAT = 'C'
     C**********
     C**********           ENDIF                           PCTRTP=M/P
     C*** END DLT ***** 05/04/00 **************************************
      *________________________________________________________________
     C*** Begin add *** 07/10/03 **************************************
      *
      *  Edit 48:  User must have adequate security to use the pmt
      *            type code.
      *
     C     Q1USER        CHAIN     PUP01                              60
      *
     C                   MOVE      UPLVL         SRULVL
     C                   MOVE      PCLVL         SRCLVL
      *
     C                   CALL      'SRCCSC'
     C                   PARM      UPTYPE        SRUTYP            1
     C                   PARM                    SRULVL            3
     C                   PARM      PCTY01        SRCTYP            1
     C                   PARM                    SRCLVL            3
     C                   PARM      '1'           SREXEC            1
     C                   PARM      *OFF          SRRES             1
     C                   PARM      PCTY02        SRCTP2            1
     C                   PARM      PCTY03        SRCTP3            1
     C                   PARM      PCTY04        SRCTP4            1
     C                   PARM      PCTY05        SRCTP5            1
     C                   PARM      ' '           SRCTP6            1
     C*
     C     SRRES         IFEQ      *ON                                          NO Access
     C                   MOVE      '48'          @@RC
     C                   RETURN
     C                   ENDIF                                                  SRRES=*ON
     C*** End add ***** 07/10/03 **************************************
      *________________________________________________________________
      *
      *  Edit 15:  If reserve type is not 'C', FEIN is required
      *
      ********* BEG DELETE ******** 10/18/07 **************************
     C****       RTTYPE    IFNE 'C'
     C****       @@FEIN    ANDLE*ZERO
     C****                 MOVE '15'      @@RC
     C****                 RETRN
     C****                 ENDIF
      ********* END DELETE ******** 10/18/07 **************************
      ********* BEG DELETE ******** 02/06/25 **************************
     C*** BEG ADD ***** 12/29/09 **************************************
     C**** RTTYPE        IFNE      'C'
     C**** @@FEIN        ANDLE     *ZERO
     C****               MOVE      '15'          @@RC
     C****               RETURN
     C****               ENDIF
     C*** END ADD ***** 12/29/09 **************************************
      ********* END DELETE ******** 02/06/25 **************************
      *________________________________________________________________
      *
      *  Edit 16:  If entered and FEIN is not equal to claimant SS#,
      *            Provider FEIN/MOD must be active in the provider file.
      *
     C*** BEG CHG *** 07/20/09 **************************************
     C     @@FEIN        IFNE      *ZERO
     C********** @@FEIN    IFGT *ZERO
     C*** BEG CHG *** 07/20/09 **************************************
     C                   MOVE      @@FEIN        CCFEIN            9
      *
     C     CCFEIN        IFNE      CMSS#
     C     PDKY          CHAIN     PPD01                              60
      *
     C  N60PDSTAT        IFEQ      'I'
     C                   MOVE      *ON           *IN60
     C                   ENDIF
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      '16'          @@RC
     C                   RETURN
     C                   ENDIF
      *
     C                   ELSE
      *
      *________________________________________________________________
      *
      *  Edit 17:  FEIN is equal to claimant SS#.
      *            SS# must be active in the Claimant Demographics file.
      *
     C     CMSS#         CHAIN     PCD01                              60
      *
     C  N60CDSTAT        IFEQ      'I'
     C                   MOVE      *ON           *IN60
     C                   ENDIF
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      '17'          @@RC
     C                   RETURN
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
      *________________________________________________________________
      *
     C*** BEGIN DLT *** 05/28/98 **************************************
      *  Edit 41:  Comp payment has an FEIN not equal to Claimant SSN.
      ***********
     C***********@@FEIN    IFGT *ZERO                      Payee
     C***********@@RETP    IFEQ 'C'                        Reserve Type
     C***********CCFEIN    ANDNECMSS#
     C***********          MOVE '41'      @@RC
     C***********          RETRN
     C***********          ENDIF
     C***********          ENDIF
     C*** END DLT ***** 05/28/98 **************************************
      *________________________________________________________________
      *
      *  All Edits Passed - Return Code assigned Blank
      *
     C                   MOVE      *BLANK        @@RC
     C                   RETURN
