     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) bnddir('STBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA014R - API:  Payment Edit IV
     H*****************************************************************
     H*  DESCRIPTION:
     H*
     H*    This routinue will check for all scenarios listed below in
     H*    the order listed below.
     H*
     H*    If ALL edits are passed, the Return Code will be Blank.
     H*    Otherwise, the first edit error encountered will assign
     H*    the number to the Return Code and the API will exit.
     H*
     H* ------
     H* Return
     H* Code      Edit Description
     H* ------
     H* 06 -Claim Master record check.
     H* 24 -Group type must be 'Y' or 'N'
     H* 25 -Posting Date must be valid date
     H* 26 -From service date must be valid date
     H* 27 -From Service date must be greater than or equal
     H*     Claim Accident date.
     H* 28 -Service TO Date must be valid date.
     H* 29 -From Service Date may not be greater than To Date
     H* 30 -Payment Value 1 must be valid
     H* 31 -Reserve Code not defined for Payment Value 1 code
     H* 32 -Payment Value 2 must be valid
     H* 33 -Lost Time Flag not compatible for Payment Value 1 code
     H* 34 -Received Date must be valid date
     H* 35 -Received Date must be greater than or equal
     H*     Claim Accident date.
     H* 36 -Invalid Payment Status code.
     H* 37 -Invalid Number of Days.  Must be less than 7
     H* 38 -Invalid Paid on Time code.
     H* 39 -Payment Amount is less than or equal zero
     H* 40 -Invalid EOMB#
     H* 42 -From Service date must be greater than or equal
     H*     Claim "Full Salary Paid Throught Date".
     H* 44 -EFT not set up for payee
     H*
     H*
     H*  CREATED: 07/02/97  C:30  Project:  WC660
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/20/19 C:62  Project: 152MAV                                *
     H*                Add Negative Fee Check Processing.             *
     H*                                                               *
     H* 04/12/16 C:309 Project: 114MXO                                *
     H*                Added check for Client# 114 for "Negative      *
     H*                fee adjust/ReConsiderations"                   *
     H*                                                               *
     H* 03/12/13 C:180 SUPPORT  RED2506                               *
     H*                Move edit 42 down so that edit 30 can be       *
     H*                detected by Manual payment entry               *
     H*                                                               *
     H* 08/02/11 C:136 SUPPORT: WA099 77591                           *
     H*                REMOVE 76236 PER JB                            *
     H*                                                               *
     H* 07/20/11 C:136 PROJECT: WC1806 PRISS 78                       *
     H*                DISABLE EDIT ADDED 12/06/10 PER JB             *
     H*                                                               *
     H* 04/06/11 C:255 SUPPORT: 76236                                 *
     H*                ADD EDIT TO PREVENT AN EFT FLAGGED PV1 PAYMENT *
     H*                CODE FROM BEING ASSIGNED TO A MANUAL CHECK     *
     H*                                                               *
     H* 12/06/10 C:136 PROJECT: WC1806 PRISS 78                       *
     H*                CHECK EFT STATUS OF PAYEE IF PAYMENT IS        *
     H*                FLAGGED FOR EFT.                               *
     H*                                                               *
     H* 10/14/10 C:220 Project: WC1953                                *
     H*                Add an edit to verify the appropriate PV1      *
     H*                code is being used.                            *
     H*                                                               *
     H* 11/09/09 C:220 Project: 136MEP                                *
     H*                Negative fee adjust/ReConsiderations           *
     H*                                                               *
     H* 04/01/08 C:191 PROJECT: 139MIV                                *
     H*                Edit routine #39 to allow negative payment amt.*
     H*                                                               *
     H*  06/12/00 C:62 PROJECT: MC230                                 *
     H*                SWAP OUT FA015R WITH FA031R AND FA032R.        *
     H*                                                               *
     H*  03/22/00 C:62 Support: MC048/22221  ** DO NOT INSTALL WITHOUT*
     H*                add recon to parm list.        FA015R **       *
     H*                                                               *
     H* 09/01/98 C:15 Support: WC021/13525                            *
     H*               Only call FA015R if WCMC is installed and       *
     H*               interfaced.                                     *
     H*                                                               *
     H* 10/13/97 C:15 PROJECT: WC659                                  *
     H*               Added code to only validate the From Date vs.   *
     H*               the Full Salary Date for comp pmts.             *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
      *  Claim Master file
     FWMCML1    IF   E           K DISK
      *________________________________________________________________
      *  Table file file
     FWTABL     IF   E           K DISK
      *________________________________________________________________
      *  Payment Status file
     FWTPSL     IF   E           K DISK
     F*** BEG   ADD *** 10/14/10 **************************************
     FWDELL     IF   E           K DISK
     F*** END   ADD *** 10/14/10 **************************************
      *________________________________________________________________
      *
      *

      //*** Begin Add *** 06/20/19 ****************************************
      /copy sPrModule

     D allowNegFees    s               n
     D*** End   Add *** 06/20/19 ****************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
      *  Payment Value 1 Reserve Code array
      *
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)                              Reserve Code
     D  B2RC01                 1      2
     D  B2RC02                 3      4
     D  B2RC03                 5      6
     D  B2RC04                 7      8
     D  B2RC05                 9     10
     D  B2RC06                11     12
     D  B2RC07                13     14
     D  B2RC08                15     16
     D  B2RC09                17     18
     D  B2RC10                19     20
      *** BEG ADD ***** 12/06/10 ************************************
     D                 DS
     D  XXEKEY                 1     31
     D  ##PTYP                 1      1
     D  ##EKEY                 2     31
      *** END ADD ***** 12/06/10 ************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *________________________________________________________________
      *
      *  TABLE FILE KEY
      *

     C     TBKEY         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    XXKEY            10
      *
      *  Claim Master key
      *
     C     CMKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C*** BEG   ADD *** 10/14/10 **************************************
      *  Employer Detail file
     C     ELKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*** END   ADD *** 10/14/10 **************************************
      *________________________________________________________________
     C     *LIKE         DEFINE    CMCO#         @@CO#
     C     *LIKE         DEFINE    CMFND         @@FND
     C     *LIKE         DEFINE    CMFYR         @@FYR
     C     *LIKE         DEFINE    CMCASE        @@CASE
     C     *LIKE         DEFINE    CMACDT        XXTEST
     C     *LIKE         DEFINE    CMACDT        XXFROM
     C     *LIKE         DEFINE    CMACDT        XXTO
      *________________________________________________________________
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *________________________________________________________________
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#                          Company
     C                   PARM                    @@FND                          Group
     C                   PARM                    @@FYR                          Fund Year
     C                   PARM                    @@CASE                         Claim
     C                   PARM                    @@GRP             1            Group Type
     C                   PARM                    @@PODT            8 0          Posting Date
     C                   PARM                    @@RCVD            8 0          Received Date
     C                   PARM                    @@FRDT            8 0          From Date
     C                   PARM                    @@TODT            8 0          To Date
     C                   PARM                    @@RETP            1            Reserve Code
     C                   PARM                    @@RECD            2            Reserve Code
     C                   PARM                    @@BR2             2            Payment Value 1
     C                   PARM                    @@BR7             2            Payment Value 2
     C                   PARM                    @@STS             2            Payment Status
     C                   PARM                    @@POTM            2            Paid on Time
     C                   PARM                    @@DYWK            1 0          Number of Days
     C                   PARM                    @@RC              2            Return Code
     C                   PARM                    @@EOB#            9 0          EOMB#
     C                   PARM                    @@AMT             9 2          Payment Amount
     C*** BEGIN ADD *** 03/29/00 **************************************
     C                   PARM                    @@RCN             3 0
     C*** END   ADD *** 03/29/00 **************************************
     C*** BEG ADD ***** 12/06/10 **************************************
     C                   PARM                    @@EKEY           31            EFT KEY
     C*** END ADD ***** 12/06/10 **************************************
      *________________________________________________________________
      *
     C                   MOVE      *BLANK        @@RC
     C*** BEG ADD ****** 04/01/08 ************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    @@CLT#            3
     C*** END ADD ****** 04/01/08 ************************************
     C*** Begin Chg *** 06/20/19 ****************************************
      /free
           // Check to see if negative fees are allowed
           allownegFees = *Off;
           If IsModEnabled('Negative Fee Checks');
              allowNegFees = *On;
           EndIf;
      /end-free
     C*** End   Chg *** 06/20/19 ****************************************
     C*
     C*** BEGIN ADD *** 09/01/98 **************************************
     C*  Determine if WCMC is installed.
     C*
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C*** BEGIN ADD *** 06/12/00 **************************************
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILLS
     C*** END   ADD *** 06/12/00 **************************************
     C*** END ADD ***** 09/01/98 **************************************
      *
     C*** BEG ADD ***** 12/06/10 **************************************
     C                   MOVE      *BLANKS       XXEKEY           31
     C     Q1PRMS        IFGT      20
     C                   MOVEL     @@EKEY        XXEKEY
     C                   ENDIF
     C*** END ADD ***** 12/06/10 **************************************
      *________________________________________________________________
      *
      *  Edit 06:  Claim Master record check
      *
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '06'          @@RC
     C                   RETURN
     C                   ENDIF
      *________________________________________________________________
      *
      *  Edit 24:  Group type must be 'Y' or 'N'
      *
     C     @@GRP         IFNE      'Y'
     C     @@GRP         ANDNE     'N'
     C                   MOVE      '24'          @@RC
     C                   RETURN
     C                   ENDIF
      *________________________________________________________________
      *
      *  Edit 25:  Posting Date must be valid date
      *
     C                   Z-ADD     @@PODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         @@PODT
      *
     C     SRERR         IFEQ      '1'
     C                   MOVE      '25'          @@RC
     C                   RETURN
     C                   ENDIF
      *________________________________________________________________
      *  Edit 39:  Payment Amount is less than or equal zero
      *
     C*** BEG ADD ****** 04/01/08 ************************************
     C     @@CLT#        IFNE      '139'
     C*** BEG ADD ****** 11/09/09 ************************************
     C*** Begin Chg *** 06/20/19 ****************************************
     C*****@@CLT#        ANDNE     '136'
     C     allowNegFees  ANDEQ     *Off
     C*** End   Chg *** 06/20/19 ****************************************
     C*** END ADD ****** 11/09/09 ************************************
     C*** END ADD ****** 04/01/08 ************************************
     C*** Beg Add ****** 04/12/16 ************************************
     C     @@CLT#        ANDNE     '114'
     C*** End Add ****** 04/12/16 ************************************
     C     @@AMT         IFLE      *ZERO
     C                   MOVE      '39'          @@RC
     C                   RETURN
     C                   ENDIF
     C*** BEG ADD ****** 04/01/08 ************************************
     C                   ELSE
     C     @@AMT         IFEQ      *ZERO
     C                   MOVE      '39'          @@RC
     C                   RETURN
     C                   ENDIF                                                  @@AMT=0
     C                   ENDIF                                                  @@CLT#<>'139'
     C*** END ADD ****** 04/01/08 ************************************
      *________________________________________________________________
      *  Edit 38:  Validate Paid on Time code (LATE CODE)
      *
     C     @@RETP        IFEQ      'C'
     C     @@POTM        ANDEQ     *BLANK
     C                   MOVE      '38'          @@RC
     C                   RETURN
     C                   ENDIF
      *
     C     @@POTM        IFNE      *BLANK
     C                   MOVEL(P)  @@POTM        XXKEY
     C     TBKEY         CHAIN     PLR01                              38
     C  N38LRSTAT        COMP      'I'                                    38
     C     *IN38         IFEQ      '1'
     C                   MOVE      '38'          @@RC
     C                   RETURN
     C                   ENDIF
     C                   ENDIF
      *________________________________________________________________
      *  Edit 26:  From service date must be valid date
      *
     C     @@FRDT        IFEQ      *ZERO
     C                   MOVE      '26'          @@RC
     C                   RETURN
     C                   ENDIF
      *
     C                   Z-ADD     @@FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         @@FRDT
      *
     C     SRERR         IFEQ      '1'
     C                   MOVE      '26'          @@RC
     C                   RETURN
     C                   ENDIF
      *
      *________________________________________________________________
      *
      *  Edit 27:  From Service date must be greater than or equal
      *     Claim Accident date.
      *
     C                   Z-ADD     @@FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTEST
      *
     C     XXTEST        IFLT      CMACDT
     C                   MOVE      '27'          @@RC
     C                   RETURN
     C                   ENDIF
      *________________________________________________________________
      *
      *** BEGIN DELETE 03/12/13 ****************************************
      ** Edit 42:  From Service date must be greater than or equal
      *     Claim "Full Salary Paid Throught Date".
      *
     C**   XXTEST        IFLT      CMFSDT
     C*** BEGIN ADD *** 10/13/97 **************************************
     C**   @@RETP        ANDEQ     'C'
     C*** END ADD ***** 10/13/97 **************************************
     C***                MOVE      '42'          @@RC
     C***                RETURN
     C***                ENDIF
      *** END   DELETE 03/12/13 ****************************************
      *
      *________________________________________________________________
      *
      *  Edit 28:  Service TO Date must be valid date.
      *
     C     @@TODT        IFEQ      *ZERO
     C                   MOVE      '28'          @@RC
     C                   RETURN
     C                   ENDIF
      *
     C                   Z-ADD     @@TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         @@TODT
      *
     C     SRERR         IFEQ      '1'
     C                   MOVE      '28'          @@RC
     C                   RETURN
     C                   ENDIF
      *________________________________________________________________
      *
      * Edit 29:  From Service Date may not be greater than To Date
      *
     C                   Z-ADD     @@FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFROM
      *
     C                   Z-ADD     @@TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTO
      *
     C     XXFROM        IFGT      XXTO
     C                   MOVE      '29'          @@RC
     C                   RETURN
     C                   ENDIF
      *
      *________________________________________________________________
      *  Edit 34:  Received date must be valid date
      *
     C     @@RETP        IFEQ      'M'
     C     @@RCVD        IFEQ      *ZERO
     C                   MOVE      '34'          @@RC
     C                   RETURN
     C                   ENDIF
      *
     C                   Z-ADD     @@RCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         @@RCVD
      *
     C     SRERR         IFEQ      '1'
     C                   MOVE      '34'          @@RC
     C                   RETURN
     C                   ENDIF
     C                   ENDIF
      *
      *________________________________________________________________
      *
      *  Edit 35:  Received Date must be greater than or equal
      *     Claim Accident date.
      *
     C     @@RCVD        IFGT      *ZERO
     C                   Z-ADD     @@RCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTEST
      *
     C     XXTEST        IFLT      CMACDT
     C                   MOVE      '35'          @@RC
     C                   RETURN
     C                   ENDIF
     C                   ENDIF
      *________________________________________________________________
      *________________________________________________________________
      *
      *  Edit 36:  Payment Status must be valid code (required)
      *
     C     @@STS         IFEQ      *BLANK
     C                   MOVE      '36'          @@RC
     C                   RETURN
     C                   ENDIF
      *
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     @@STS         XXKEY
     C     TBKEY         CHAIN     PPS01                              36
      *
     C     *IN36         IFEQ      '1'
     C     PSSTAT        OREQ      'I'
     C                   MOVE      '36'          @@RC
     C                   RETURN
     C                   ENDIF
      *________________________________________________________________
      *
      * Edit 30:  Payment Value 1 must be valid
      *
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     @@BR2         XXKEY
      *
     C     TBKEY         CHAIN     PB201                              28
      *
     C  N28B2STAT        COMP      'I'                                    28
     C     *IN28         IFEQ      '1'
     C                   MOVE      '30'          @@RC
     C                   RETURN
     C                   ENDIF
      *
      *________________________________________________________________
      *
      * Edit 31:  Payment Value 1 invalid for Payment's Reserve Code
      *
     C                   Z-ADD     1             X                 3 0
      *
     C     @@RECD        LOOKUP    RT(X)                                  40
      *
     C     *IN40         IFEQ      *OFF
     C                   MOVE      '31'          @@RC
     C                   RETURN
     C                   ENDIF
     C*** BEG   ADD *** 10/14/10 **************************************
     C     B2CCW         IFEQ      'Y'
     C     CMJRST        ANDEQ     'CA'
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
      *
     C     ELPFDT        IFLT      1100701
     C     RT(X)         ANDNE     'ME'
     C                   MOVE      '31'          @@RC
     C                   RETURN
     C                   ENDIF                                                  ELPFDT<1070110
      *
     C     ELPFDT        IFGE      1100701
     C     RT(X)         ANDNE     'EX'
     C                   MOVE      '31'          @@RC
     C                   RETURN
     C                   ENDIF                                                  ELPFDT>1070110
      *
     C                   ENDIF                                                  *IN60 = *OFF
     C                   ENDIF                                                  B2CCW = 'Y'
     C*** END   ADD *** 10/14/10 **************************************
      *________________________________________________________________
      *
      * Edit 32:  Payment Value 2 must be valid
      *
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     @@BR7         XXKEY
     C     TBKEY         CHAIN     PB701                              28
     C  N28B7STAT        COMP      'I'                                    28
     C     *IN28         IFEQ      '1'
     C                   MOVE      '32'          @@RC
     C                   RETURN
     C                   ENDIF
      *
      *________________________________________________________________
      *
      * Edit 33:  Payment Value 2 incompatible for Lost Time Flag
      *
     C     CMLTFL        IFEQ      'I'
     C     B7LTLF        ANDNE     'Y'
     C                   MOVE      '33'          @@RC
     C                   RETURN
     C                   ENDIF
      *
     C     CMLTFL        IFEQ      'M'
     C     B7LTLF        ANDNE     'N'
     C     CMLTFL        OREQ      'R'
     C     B7LTLF        ANDNE     'N'
     C                   MOVE      '33'          @@RC
     C                   RETURN
     C                   ENDIF
      *________________________________________________________________
      *  Edit 37:  Invalid Number of Days.  Must be less than 7
      *
     C     @@DYWK        IFGE      7
     C                   MOVE      '37'          @@RC
     C                   RETURN
     C                   ENDIF
      *________________________________________________________________
      *
      *  Edit 40:  If EOMB# greater than zero, verify Valid EOMB#.
      *
      *** BEGIN ADD *** 09/01/98 **************************************
      *  If WCMC is not installed or not interfaced with WCMS, do not
      *    process this edit.
      *
     C     ##MCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
      *** END ADD ***** 09/01/98 **************************************
      *
     C     @@EOB#        IFGT      *ZERO
     C*** BEGIN ADD *** 03/29/00 **************************************
     C     Q1PRMS        IFGT      19
     C*
     C*** BEGIN CHG *** 06/12/00 **************************************
     C**********           Z-ADD@@RCN     ##RCN
     C                   Z-ADD     @@RCN         #@RCN
     C*** END   CHG *** 06/12/00 **************************************
     C*
     C                   ELSE
     C*
     C*** BEGIN CHG *** 06/12/00 **************************************
     C**********           Z-ADD*ZEROS    ##RCN
     C                   Z-ADD     *ZEROS        #@RCN
     C*** END   CHG *** 06/12/00 **************************************
     C*
     C                   ENDIF
     C*** END   ADD *** 03/29/00 **************************************
     C*** BEGIN CHG *** 06/12/00 **************************************
     C**********           CALL 'FA015R'
     C**********           PARM           @@CO#            Company #
     C**********           PARM           @@FND            WCMS Fund
     C**********           PARM           @@FYR            Policy Year
     C**********           PARM           @@CASE           Claim #
     C**********           PARM           @@EOB#           EOMB #
     C**********           PARM *ZERO     ##RRN   90       EOMB RRN#
     C**********           PARM *ZERO     ##SUB#  50       Subscriber #
     C**********           PARM *ZERO     ##GRP#  30       WCMC Group #
     C**********           PARM *BLANK    ##RC    1        Return Code
     C*** BEGIN ADD *** 03/29/00 **************************************
     C**********           PARM *BLANKS   ##AST   2
     C**********           PARM *ZEROS    ##FEIN  90
     C**********           PARM *ZEROS    ##MOD   50
     C**********           PARM           ##RCN   30
     C*** END   ADD *** 03/29/00 **************************************
      *
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   CALL      'FA031R'
1    C                   PARM      @@CO#         #@CO#             3 0          Company #
2    C                   PARM      @@FND         #@WCF#            3 0          WCMS Fund
3    C                   PARM      @@FYR         #@FYR             3 0          Policy Year
4    C                   PARM      @@CASE        #@CASE            7 0          Claim #
5    C                   PARM      @@EOB#        #@EOB#            9 0          EOMB #
6    C                   PARM                    #@RCN             3 0          RECON #
7    C                   PARM      *BLANKS       ##RC              1            Return Code
     C*
     C                   ELSE
     C*
     C                   CALL      'FA032R'
     C                   PARM      @@CO#         #@CO#             3 0          Company #
     C                   PARM      @@FND         #@WCF#            3 0          WCMS Fund
     C                   PARM      @@FYR         #@FYR             3 0          Policy Year
     C                   PARM      @@CASE        #@CASE            7 0          Claim #
     C                   PARM      @@EOB#        #@EOB#            9 0          EOMB #
     C                   PARM      *BLANKS       ##RC              1            Return Code
     C*
     C                   ENDIF
     C*** END   CHG *** 06/12/00 **************************************
     C     ##RC          IFNE      *BLANK
     C                   MOVE      '40'          @@RC
     C                   RETURN
     C                   ENDIF
     C                   ENDIF
      *
      *** BEGIN ADD *** 09/01/98 **************************************
     C                   ENDIF
      *** END ADD ***** 09/01/98 **************************************
      *** BEG DEL ***** 08/02/11 **************************************
      ****BEG*ADD ***** 12/06/10 **************************************
     C***********B2EFT     IFEQ 'Y'
      ****BEG*ADD ***** 04/06/11 **************************************
     C***********          CALL 'SRCHKPGM'
     C***********          PARM 'W0120R'  OBJNAM 10
     C***********          PARM           @FLAG   1
     C**FLAG*=1*FOUND PROGRAM IN CALLSTACK OTHERWISE FLAG = 0.
     C***********@FLAG     IFEQ '1'                        FOUND W0120R
     C***********          MOVE '44'      @@RC
     C***********          RETRN
     C***********          ENDIF
      ****END*ADD ***** 04/06/11 **************************************
     C***********
      ****BEG*DEL ***** 07/20/11 **************************************
     C***********          MOVE @@CO#     ##CO#
     C***********          MOVE @@FND     ##FND
     C***********          Z-ADD@@PODT    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     ##DATE
     C***********          CALL 'WA023R3'
     C***********          PARM           ##CO#   3        Company
     C***********          PARM           ##PTYP  1        Payee Type
     C***********          PARM           ##FND   3        Group #
     C***********          PARM           ##EKEY 30        Key
     C***********          PARM           ##DATE  7        Date
     C***********          PARM           ##EFT   1        EFT flag
     C***********          PARM           ##PRE   1        PRE note flag
      ***********
     C***********##EFT     IFEQ *OFF
     C***********          MOVE '44'      @@RC
     C***********          RETRN
     C***********          ENDIF
      ****END*DEL ***** 07/20/11 **************************************
     C***********          ENDIF
      ****END*ADD ***** 12/06/10 **************************************
      *** END DEL ***** 08/02/11 **************************************
      *
      *  All Edits Passed - Return Code assigned Blank
      *
     C                   MOVE      *BLANK        @@RC
     C                   RETURN
      *** BEGIN ADD **** 03/12/13 ******************************************
      *  Edit 42:  From Service date must be greater than or equal
      *     Claim "Full Salary Paid Throught Date".
      *
     C     XXTEST        IFLT      CMFSDT
     C     @@RETP        ANDEQ     'C'
     C                   MOVE      '42'          @@RC
     C                   RETURN
     C                   ENDIF
      *** BEGIN ADD **** 03/12/13 ******************************************
