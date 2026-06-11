     Hcopyright('(c) 2023 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H Bnddir('STBNDDIR')
     H*****************************************************************
     H/TITLE WA019R - Duplicate Payment Check API program
     H*****************************************************************
     H*  DESCRIPTION:
     H*                                                               *
     H*    This pgm receives request for payment data and will        *
     H*    determine if this information is a duplicate (exact or     *
     H*    possible) of either another Request for Payment record or  *
     H*    a Processed Payment.                                       *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0080R                                                     *
     H*    W0120R                                                     *
     H*    W8080R                                                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*                                                               *
     H*  CREATED: 05/29/97  C:30  Project:  WC660                     *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     h*                                                               *
     h* 12/10/25 C:079 Support: 120209                                *
     h*                Correct Duplicate Payment Release Logic        *
     H*                                                               *
     H* 02/28/23 C:344 Support: 113791                                *
     H*              - Changed display file so that button text is    *
     H*                white.                                         *
     H* 02/28/23 C:344 Project: WC2430                                *
     H*              - Create a new window for Possible Duplicates.   *
     H*              - The new window will display if the new payment *
     H*                install flag set to blank. Otherwise the       *
     H*                legacy window will display if the New Payment  *
     H*                install flag set to N.                         *
     H*              - Display one of the warning descriptions.       *
     H*              - Pending Payment button will not show for       *
     H*                manual payments.
     H*              - Override button will be based on Type/Level    *
     H*                security.                                      *
     H*              - Allow user to enter comment for the duplicate  *
     H*                reasoning.                                     *
     H*              - Use input parameter for checking manual type   *
     H*                and whether to show 'Pend Payment' button.     *
     H* 04/08/15 C:279 Project: WC2328                                *
     H*              - Modified to condtion duplicate payment edit    *
     H*                based on flag setup at company master level.   *
     H*                                                               *
     H* 12/09/10 C:53  Project: WC1886 PRISS# 1                       *
     H*              - BASED ON THIS ISSUE THE 25WRG CODE SHOULD NOT
     H*                BE CLIENT ID QUALIFIED.
     H*                                                               *
     H* 10/06/10 C:191 Project: 133WKA PRISS# 36                      *
     H*              - FIX BUG WITH CLAIMS TASB BACK TO BASE CODE     *
     H*                                                               *
     H* 04/19/10 C:235 Project: 25WRG                                 *
     H*              - CLAIMS GENERAL RETROFIT                        *
     H*                                                               *
     H* 05/02/07 C:132 Project: 133WEB                                *
     H*              - Removed the use of Client ID, make available   *
     H*                to all clients, as per LK instructions.        *
     H*                                                               *
     H* 04/26/07 C:132 Project: 133WEB                                *
     H*              - Add Possible Duplicate Payment fields for      *
     H*                security message.                              *
     H*              - Check authority for making payments if tagged  *
     H*                as exact or possible duplicate payments.       *
     H*              - Change 114WLY for check of Q1PRMS > 16         *
     H*                                                               *
     H*  01/25/07 C:183 Project: 114WLY                               *
     H*                 Added Parm for Exact Payment Indicator.       *
     H*                                                               *
     H*  06/11/03 C:15 Support: WC065/37445                           *
     H*                Chgd code to show exact dups first.  Then if   *
     H*                there are no exact dups, show the possible     *
     H*                dups.                                          *
     H*                                                               *
     H*  07/29/02 C:15 Support: WC111/33402                           *
     H*                Fixed code to ignore adjustments, stops, and   *
     H*                voids.                                         *
     H*                                                               *
     H*  09/05/00 C:62 Project: MC055 24204                           *
     H*                ADDED PARM FOR DUPLICATE MODIFIER.             *
     H*                                                               *
     H*  05/19/00 C:15 Project: WC577                                 *
     H*                Changed pgm to get the correct pmt requester.  *
     H*                                                               *
     H*  11/02/99 C:15 Support: WC044/20179                           *
     H*                Added the EOMB# to the dup screen.             *
     H*                                                               *
     H*  04/07/99 C:15 Support: WC090/17156                           *
     H*                Removed PATRDT from key list used to retrieve  *
     H*                records from WDRPP due to incorrect date being *
     H*                loaded into RPTRDT.                            *
     H*                                                               *
     H*  03/02/99  C:33  SEQ#: WC095/13271                            *
     H*                  WHEN CHECKING FOR POSSIBLE DUPLICATES, CHECK *
     H*                  FOR RESERVE TYPE RATHER THAN RESERVE CODE.   *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
      *  Display File
     FWA019F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *________________________________________________________________
      *
      *  Payment Detail file -            KEY = CO#/FND/FYR/CASE/SEQ#
     FWDPAL     IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
      *________________________________________________________________
      *
      *  Request for Payment file -       KEY = CO#/FND/FYR/CASE/SEQ#
     FWARPL     IF   E           K DISK
      *________________________________________________________________
      *
      ***** BEG ADD ***** 04/26/07 ************************************
      *  NOI/Claim Intake Darts file -    KEY = CO#/FND
     FWMCXL     IF   E           K DISK
     F*
     F* User Parameter File
     FWMUPL     IF   E           K DISK    USROPN
      ***** END ADD ***** 04/26/07 ************************************
      ***** BEG ADD ***** 02/28/23 ***** WC2430 ***********************
     FWTRTL1    IF   E           K DISK    RENAME(PRT01:PRT00)
      ***** END ADD ***** 02/28/23 ***** WC2430 ***********************
      *________________________________________________________________
      *
      ***** BEG ADD ***** 04/19/10 ************************************
      *
      * PAYEE DEMOGRAPHICS  file-         KEY = FEIN/MOD
     FWMPDL     IF   E           K DISK
      ***** END ADD ***** 04/19/10 ************************************
      *________________________________________________________________
     F*** BEGIN DLT *** 05/19/00 **************************************
     F***WDRPL   IF  E           K        DISK
     F*********** PRP01                             KRENAMEPRP04
     F*** END DLT ***** 05/19/00 **************************************
      ******* beg add ******** 04/08/15 *******************************
     fsmcol     if   e           k disk    rename(pco01:pco00)
      ******* end add ******** 04/08/15 *******************************

     D*
     D***  BEGIN ADD  ***  03/02/99  *********************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D***  END   ADD  ***  03/02/99  *********************************
     D***  BEG ADD  ***  02/28/23  ***  WC2430  ********************
     d/copy sprModule
     d/copy sprSecure
     D***  END ADD  ***  02/28/23  ***  WC2430  ********************
      *________________________________________________________________
      *
      *   REQUEST PAYMENT FILE KEY LIST (WARPL)
      *

     C     RPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKSEQ
      *
      *  REQUEST PAYMENT FILE - PARTIAL
      *
     C     RPKEY2        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
      *
      *  PAYMENT DETAIL FILE - PARTIAL
      *
     C     PAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
      *
     C     PAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKSEQ#
      *
      ***** BEG ADD ***** 04/26/07 ************************************
      *
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      ***** END ADD ***** 04/26/07 ************************************
      *
      ***** BEG ADD ***** 04/19/10 ************************************
     C     RPKY2         KLIST
     C                   KFLD                    SVFEIN
     C                   KFLD                    RPMOD
      ***** END ADD ***** 04/19/10 ************************************
     C*** BEGIN DLT *** 05/19/00 **************************************
     C*  Request Payment Detail File
     C**********
     C********** RPDKEY    KLIST
     C**********           KFLD           PACO#
     C**********           KFLD           PAFND
     C**********           KFLD           PAFYR
     C**********           KFLD           PACASE
     C**********           KFLD           PACHK#
     C*** END DLT ***** 05/19/00 **************************************
     C*** BEGIN DLT *** 04/07/99 ***********************************************
     C******************** KFLD           PATRDT
     C*** END DLT ***** 04/07/99 ***********************************************
      *________________________________________________________________
      *
     C     *LIKE         DEFINE    RPCO#         KKCO#
     C     *LIKE         DEFINE    RPFND         KKFND
     C     *LIKE         DEFINE    RPFYR         KKFYR
     C     *LIKE         DEFINE    RPCASE        KKCASE
     C     *LIKE         DEFINE    RPSEQ#        KKSEQ
     C     *LIKE         DEFINE    PASEQ#        KKSEQ#
     C     *LIKE         DEFINE    SCDFDT        XXFROM
     C     *LIKE         DEFINE    SCDFDT        XXTO
     C     *LIKE         DEFINE    RPAMT         SCAMT
     C     *LIKE         DEFINE    RPRECD        SVRECD
     C***  BEGIN ADD  ***  03/02/99  *********************************
     C     *LIKE         DEFINE    RPRETP        SVRETP
     C***  END   ADD  ***  03/02/99  *********************************
     C     *LIKE         DEFINE    RPFEIN        SVFEIN
     C*** BEGIN ADD *** 09/05/00 **************************************
     C     *LIKE         DEFINE    RPMOD         @@DMOD
     C*** END   ADD *** 09/05/00 **************************************
     C*** BEGIN ADD *** 02/28/23 ***** WC2430 *************************
     C     *LIKE         DEFINE    RPDUPCOM      @@CMNT
     C*** END   ADD *** 02/28/23 ***** WC2430 *************************
     C*** Beg Add *** 12/10/25 ****************************************
     C     *LIKE         DEFINE    RPSTAT        inMode
     C*** End Add *** 12/10/25 ****************************************
      *________________________________________________________________
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *________________________________________________________________
      *
     C     *ENTRY        PLIST
     C                   PARM                    KKCO#                          Company #
     C                   PARM                    KKFND                          Fund
     C                   PARM                    KKFYR                          Fund Year
     C                   PARM                    KKCASE                         Case
     C                   PARM                    KKSEQ                          RP Seq #
     C                   PARM                    XXFROM                         From Date
     C                   PARM                    XXTO                           To Date
     C                   PARM                    SCAMT                          Amount
     C                   PARM                    SVRECD                         Reserve Code
     C                   PARM                    SVFEIN                         FEIN #
     C                   PARM                    SCDFDD                         Fund Name
     C                   PARM                    SCDCNM                         Claimant Name
     C                   PARM                    SCDPNM                         Provider Name
     C                   PARM                    I90               1            *IN90
     C***  BEGIN ADD  ***  03/02/99  *********************************
     C                   PARM                    SVRETP                         Reserve Type
     C***  END   ADD  ***  03/02/99  *********************************
     C*** BEGIN ADD *** 09/05/00 **************************************
     C                   PARM                    @@DMOD                         Modifier
     C*** END   ADD *** 09/05/00 **************************************
     C*** BEG ADD ***** 01/25/07 **************************************
     C                   PARM                    @@RTCD            1
     C*** END ADD ***** 01/25/07 **************************************
     C*** BEG ADD ***** 02/28/23 ***** WC2430 *************************
     C                   PARM                    @@CMNT
     C*** END ADD ***** 02/28/23 ***** WC2430 *************************
      *
      ***** BEG ADD ***** 04/26/07 ************************************
     C     Q1PRMS        IFGT      16
       //*** Beg Add *** 12/10/25 *************************************
       // If called to release a payment then show certain buttons
       If @@RtCd = 'R';
          inMode = 'R';
          @@RtCd = ' ';
       Endif;
       //*** End Add *** 12/10/25 *************************************
     C                   MOVE      @@RTCD        XXRTCD            1
     C                   ENDIF                                                  Q1PRMS > 16
      *
      ***** BEG DEL ***** 05/01/07 ************************************
     C**********           CALL 'RTVCLTID'
     C**********           PARM           CLTID   3
      ***** END DEL ***** 05/01/07 ************************************
      ***** BEG ADD ***** 04/19/10 ************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      ***** END ADD ***** 04/19/10 ************************************
      ***** END ADD ***** 04/26/07 ************************************
       //** BEG ADD ***** 02/28/23 ***** WC2430 ***********************
       If Q1PRMS > 17;
          scComment = @@cmnt;
       Else;
          Clear scComment;
       Endif;
       //** END ADD ***** 02/28/23 ***** WC2430 ***********************
      /free
       //******* beg add ********* 04/08/15 ***************************
       Chain kkCo# pco00;
       //******* end add ********* 04/08/15 ***************************
      /end-free
      *
      *------------------
      *  Check the Request Payment File for exact or possible duplicates.
      *
     C                   MOVE      *OFF          *IN66
     C                   MOVE      *OFF          *IN67
      *
      ***** BEG ADD ***** 04/19/10 ************************************
     C*** BEGIN ADD ****** 10/06/10 ************************************
     C                   MOVE      '0'           XXP25             1
     C*** END   ADD ****** 10/06/10 ************************************
     C*** BEG   DEL ****** 12/09/10 ************************************
     C*******    CLTID     IFEQ '025'
     C*** END   DEL ****** 12/09/10 ************************************
     C     RPKY2         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C     PDXDPE        ANDNE     'Y'
     C     *IN60         OREQ      '1'
     C*** BEGIN ADD ****** 10/06/10 ************************************
     C                   MOVE      '1'           XXP25
     C                   ENDIF
     C*** BEG   DEL ****** 12/09/10 ************************************
     C*******              ENDIF
     C*** END   DEL ****** 12/09/10 ************************************
     C*** END   ADD ****** 10/06/10 ************************************
      ***** END ADD ***** 04/19/10 ************************************
     C*** BEGIN ADD ****** 10/06/10 ************************************
     C*** BEG   DEL ****** 12/09/10 ************************************
     C*******    CLTID     IFEQ '025'
     C*** END   DEL ****** 12/09/10 ************************************
     C*** BEG   CHG ****** 12/09/10 ************************************
     C*******    XXP25     ANDEQ'1'
     C     XXP25         IFEQ      '1'
     C*** END   CHG ****** 12/09/10 ************************************
     C*** BEG   DEL ****** 12/09/10 ************************************
     C*******    CLTID     ORNE '025'
     C*** END   DEL ****** 12/09/10 ************************************
     C*** END   ADD ****** 10/06/10 ************************************
     C     RPKEY2        CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
      *
      *  Skip record if Payment Type is Not 'M' or 'P'.
      *
     C     RPTYPE        IFNE      'M'
     C     RPTYPE        ANDNE     'P'
     C                   GOTO      NXTRP1
     C                   ENDIF
      *
      *  Payment Request is an Exact Duplicate if a Previous Request has
      *  the Same Payment Reserve Code, Provider, Dates of Service,
      *  Payment Amount, and the Sequence Number of Both WARPP records
      *  are different.
      *
      *********** beg chg ******** 04/08/15 ***************************
     C*****SVRECD        IFEQ      RPRECD
     C*****SVFEIN        ANDEQ     RPFEIN
     C*****XXFROM        ANDEQ     RPFRDT
     C*****XXTO          ANDEQ     RPTODT
     C*****SCAMT         ANDEQ     RPAMT
     C*****RPSTAT        ANDNE     'D'
     C*****RPSEQ#        ANDNE     KKSEQ

      /free
       If ((svReCd = RPreCd and COERSD = ' ') or COERSD = 'Y') and
        svFEIN = RPfein and xxFrom = RPfrDt and xxTo = RPtoDt and
        scAmt = RPamt and RPstat <> 'D' and RPseq# <> kkSeq;
      /end-free
      *********** end chg ******** 04/08/15 ***************************
      *
     C                   MOVE      '1'           *IN65
     C                   MOVE      '1'           *IN66
      *** BEG ADD *** 02/28/23 *** WC2430 *****************************
       If isModEnabled('New Payments');
          GetPaymentTrans();

          Exfmt WDWMSG2;

          // Cancel Button Selected - Set return code
          If BtnCancel = *on;
             xxRTCD = '4';
          Endif;

          // Pend Payment Button Selected - Set return code
          If BtnPendPay = *on;
             xxRTCD = '1';
          Endif;

       Else;
      *** END ADD *** 02/28/23 *** WC2430 *****************************
     C                   EXSR      RPASGN
     C                   EXFMT     WDWMSG
      *** BEG ADD *** 02/28/23 *** WC2430 *****************************
       Endif;
      *** END ADD *** 02/28/23 *** WC2430 *****************************
      *** WINDOW
     C                   SETON                                        90
     C                   GOTO      ENDCK1
      *
     C*** Begin dlt *** 06/11/03 **************************************
     C*******              ELSE
      *******
      *  Payment Request is a Possible Duplicate if a Previous Request
      *  has the Same Reserve Code, Provider, and the Dates of Service
      *  for the Selected Request overlap Dates of Service for the
      *  Previous Request, and the Sequence Number of Both WARPP Records
      *  are Different.
      *******
     C***  BEGIN CHG  ***  03/02/99  *********************************
     C********** SVRECD    IFEQ RPRECD
     C*******              MOVE *OFF      MATCH   1
     C*******
     C*******    Q1PRMS    IFGE 15
     C*******
     C*******    SVRETP    IFEQ RPRETP
     C*******              MOVE *ON       MATCH
     C*******              ENDIF                           SVRETP=RPRETP
     C*******
     C*******              ELSE
     C*******
     C*******    SVRECD    IFEQ RPRECD
     C*******              MOVE *ON       MATCH
     C*******              ENDIF                           SVRECD=RPRECD
     C*******
     C*******              ENDIF                           Q1PRMS>=15
     C*******
     C*******    MATCH     IFEQ *ON
     C***  END   CHG  ***  03/02/99  *********************************
     C*******    SVFEIN    ANDEQRPFEIN
     C*******    RPSTAT    ANDNE'D'
     C*******    RPSEQ#    ANDNEKKSEQ
      *******
     C*******    XXFROM    IFGE RPFRDT
     C*******    XXFROM    ANDLERPTODT
      *******
     C*******    XXTO      ORGE RPFRDT
     C*******    XXTO      ANDLERPTODT
      *******
     C*******    XXFROM    ORLE RPFRDT
     C*******    XXTO      ANDGERPTODT
      *******
     C*******              MOVE '1'       *IN65
     C*******              MOVE '1'       *IN67
     C*******              EXSR RPASGN
     C*******              EXFMTWDWMSG
      *** WINDOW
     C*******              SETON                     90
     C*******              GOTO ENDCK1
     C*******              ENDIF                           Dates Overlap
     C*******              ENDIF                           Possible Dup
     C*** End dlt ***** 06/11/03 **************************************
     C                   ENDIF                                                  Exact Dup
      *
     C     NXTRP1        TAG
      *
     C     RPKEY2        READE     PRP01                                  60
     C                   ENDDO                                                  DOW *IN60 RPKEY
      *
      ***** BEG ADD ***** 04/19/10 ************************************
     C*** BEGIN DEL ****** 10/06/10 ************************************
     C*************        ENDIF
     C*** END   DEL ****** 10/06/10 ************************************
     C                   ENDIF
      ***** END ADD ***** 04/19/10 ************************************
      *--------------------
      *  Next, check the Payment Detail File for exact or possible
      *  duplicates.
      *
     C     PAKY          CHAIN     PPA02                              60
      *
     C     *IN60         DOWEQ     '0'
      *
      *  Skip record if Payment Type is Not 'M' or 'P'.
      *
     C     PATYPE        IFNE      'M'
     C     PATYPE        ANDNE     'P'
     C*** Begin add *** 07/29/02 **************************************
     C     PATYPE        ANDNE     'A'
     C     PATYPE        ANDNE     'Z'
     C*** End add ***** 07/29/02 **************************************
     C                   GOTO      NXTPA1
     C                   ENDIF
      *
      *  Payment Request is an Exact Duplicate if a Previous Payment has
      *  the Same Payment Reserve Code, Provider, Dates of Service and
      *  Payment Amount.
      *
      *********** beg chg ******** 04/08/15 ***************************
     C*****SVRECD        IFEQ      PARECD
     C*****SVFEIN        ANDEQ     PAFEIN
     C*****XXFROM        ANDEQ     PAFRDT
     C*****XXTO          ANDEQ     PATODT
     C*****SCAMT         ANDEQ     PAAMT

      /free
       If ((svReCd = PAreCd and COERSD = ' ') or COERSD = 'Y') and
        svFEIN = PAfein and xxFrom = PAfrDt and xxTo = PAtoDt and
        scAmt = PAamt;
      /end-free
      *********** end chg ******** 04/08/15 ***************************
      *
     C                   EXSR      SRVOID
     C     XXVOID        IFEQ      *OFF
      *
     C                   MOVE      '1'           *IN65
     C                   MOVE      '1'           *IN66
      *** BEG ADD *** 02/28/23 *** WC2430 *****************************
       If isModEnabled('New Payments');
          GetClaimsChecksPayments();
          Exfmt WDWMSG2;

          // Cancel Button Selected - Set return code
          If BtnCancel = *on;
             xxRTCD = '4';
          Endif;

          // Pend Payment Button Selected
          If BtnPendPay = *on;
             xxRTCD = '1';
          Endif;

       Else;
      *** END ADD *** 02/28/23 *** WC2430 *****************************
     C                   EXSR      PAASGN
     C                   EXFMT     WDWMSG
      *** BEG ADD *** 02/28/23 *** WC2430 *****************************
       Endif;
      *** END ADD *** 02/28/23 *** WC2430 *****************************
      *** WINDOW
     C                   SETON                                        90
     C                   GOTO      ENDCK1
      *
     C                   ENDIF
      *
     C*** Begin dlt *** 06/11/03 **************************************
     C*******              ELSE
      *******
      *  Payment Request is a Possible Duplicate if a Previous Payment
      *  has the Same Payment Reserve Code, Provider, and the Dates
      *  of Service for the NEW Request Overlap Dates of Service for
      *  the Previous Payment.
      *******
     C***  BEGIN CHG  ***  03/02/99  *********************************
     C********** SVRECD    IFEQ PARECD
     C*******              MOVE *OFF      MATCH
     C*******
     C*******    Q1PRMS    IFGE 15
     C*******
     C*******    SVRETP    IFEQ PARETP
     C*******              MOVE *ON       MATCH
     C*******              ENDIF                           SVRETP=PARETP
     C*******
     C*******              ELSE
     C*******
     C*******    SVRECD    IFEQ PARECD
     C*******              MOVE *ON       MATCH
     C*******              ENDIF                           SVRECD=PARECD
     C*******
     C*******              ENDIF                           Q1PRMS>=15
     C*******
     C*******    MATCH     IFEQ *ON
     C***  END   CHG  ***  03/02/99  *********************************
     C*******    SVFEIN    ANDEQPAFEIN
     C*******
     C*******    XXFROM    IFGE PAFRDT
     C*******    XXFROM    ANDLEPATODT
     C*******
     C*******    XXTO      ORGE PAFRDT
     C*******    XXTO      ANDLEPATODT
     C*******
     C*******    XXFROM    ORLE PAFRDT
     C*******    XXTO      ANDGEPATODT
     C*******
     C*******              EXSR SRVOID
     C*******    XXVOID    IFEQ *OFF
     C*******
     C*******              MOVE '1'       *IN65
     C*******              MOVE '1'       *IN67
     C*******              EXSR PAASGN
     C*******              EXFMTWDWMSG
      *** WINDOW
     C*******              SETON                     90
     C*******              GOTO ENDCK1
     C*******              ENDIF
     C*******              ENDIF                           Dates Overlap
     C*******              ENDIF                           Possible Dup
     C*** End dlt ***** 06/11/03 **************************************
     C                   ENDIF                                                  Exact Dup
      *
     C     NXTPA1        TAG
      *
     C     PAKY          READE     PPA02                                  60
     C                   ENDDO                                                  DOW *IN60 PAKY
      *
     C*** Begin add *** 06/11/03 **************************************
      *
      *  Payment Request is a Possible Duplicate if a Previous Request
      *  has the Same Reserve Code, Provider, and the Dates of Service
      *  for the Selected Request overlap Dates of Service for the
      *  Previous Request, and the Sequence Number of Both WARPP Records
      *  are Different.
      *
      ***** BEG ADD ***** 04/19/10 ************************************
     C*** BEG   DEL ****** 12/09/10 ************************************
     C*******    CLTID     IFEQ '025'
     C*** END   DEL ****** 12/09/10 ************************************
     C     RPKY2         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C     PDXDPE        ANDNE     'Y'
     C     *IN60         OREQ      '1'
      ***** END ADD ***** 04/19/10 ************************************
     C     RPKEY2        CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *OFF          MATCH             1
     C*
     C     Q1PRMS        IFGE      15
     C*
     C     SVRETP        IFEQ      RPRETP
     C                   MOVE      *ON           MATCH
     C                   ENDIF                                                  SVRETP=RPRETP
     C*
     C                   ELSE
     C*
      *********** beg chg ******** 04/08/15 ***************************
     C*****SVRECD        IFEQ      RPRECD

      /free
       If (svReCd = RPreCd and COERSD = ' ') or COERSD = 'Y';
      /end-free
      *********** end chg ******** 04/08/15 ***************************
     C                   MOVE      *ON           MATCH
     C                   ENDIF                                                  SVRECD=RPRECD
     C*
     C                   ENDIF                                                  Q1PRMS>=15
     C*
     C     MATCH         IFEQ      *ON
     C     SVFEIN        ANDEQ     RPFEIN
     C     RPSTAT        ANDNE     'D'
     C     RPSEQ#        ANDNE     KKSEQ
      *
     C     XXFROM        IFGE      RPFRDT
     C     XXFROM        ANDLE     RPTODT
      *
     C     XXTO          ORGE      RPFRDT
     C     XXTO          ANDLE     RPTODT
      *
     C     XXFROM        ORLE      RPFRDT
     C     XXTO          ANDGE     RPTODT
      *
     C                   MOVE      '1'           *IN65
     C                   MOVE      '1'           *IN67
      *** BEG ADD *** 02/28/23 *** WC2430 *****************************
       If isModenabled('New Payments');
          GetPaymentTrans();
          Exfmt WDWMSG2;

          // Cancel Button Selected - Set return code
          If BtnCancel = *on;
             xxRTCD = '4';
          Endif;

          // Pend Payment Button Selected - Process Override
          If BtnPendPay = *on;
             xxRTCD = '1';
          Endif;

       Else;
      *** END ADD *** 02/28/23 *** WC2430 *****************************
     C                   EXSR      RPASGN
     C                   EXFMT     WDWMSG
      *** BEG ADD *** 02/28/23 *** WC2430 *****************************
       Endif;
      *** END ADD *** 02/28/23 *** WC2430 *****************************
      *** WINDOW
     C                   SETON                                        90
     C                   GOTO      ENDCK1
     C                   ENDIF                                                  Dates Overlap
     C                   ENDIF                                                  Possible Dup
     C*
     C     RPKEY2        READE     PRP01                                  60
     C                   ENDDO                                                  DOW *IN60 RPKEY
     C*
      ***** BEG ADD ***** 04/19/10 ************************************
     C                   ENDIF
     C*** BEG   DEL ****** 12/09/10 ************************************
     C*******              ENDIF
     C*** END   DEL ****** 12/09/10 ************************************
      ***** END ADD ***** 04/19/10 ************************************
      *--------------------
      *  Next, check the Payment Detail File for possible duplicates.
      *
     C     PAKY          CHAIN     PPA02                              60
     C     *IN60         DOWEQ     '0'
      *
      *  Skip record if Payment Type i
      *
     C     PATYPE        IFNE      'M'
     C     PATYPE        ANDNE     'P'
     C     PATYPE        ANDNE     'A'
     C     PATYPE        ANDNE     'Z'
     C                   GOTO      NXTPA2
     C                   ENDIF
      *
      *  Payment Request is a Possible Duplicate if a Previous Payment
      *  has the Same Payment Reserve Code, Provider, and the Dates
      *  of Service for the NEW Request Overlap Dates of Service for
      *  the Previous Payment.
      *
     C                   MOVE      *OFF          MATCH
     C*
     C     Q1PRMS        IFGE      15
     C*
     C     SVRETP        IFEQ      PARETP
     C                   MOVE      *ON           MATCH
     C                   ENDIF                                                  SVRETP=PARETP
     C*
     C                   ELSE
     C*
      *********** beg chg ******** 04/08/15 ***************************
     C*****SVRECD        IFEQ      PARECD

      /free
       If (svReCd = PAreCd and COERSD = ' ') or COERSD = 'Y';
      /end-free
      *********** end chg ******** 04/08/15 ***************************
     C                   MOVE      *ON           MATCH
     C                   ENDIF                                                  SVRECD=PARECD
     C*
     C                   ENDIF                                                  Q1PRMS>=15
     C*
     C     MATCH         IFEQ      *ON
     C     SVFEIN        ANDEQ     PAFEIN
      *
     C     XXFROM        IFGE      PAFRDT
     C     XXFROM        ANDLE     PATODT
      *
     C     XXTO          ORGE      PAFRDT
     C     XXTO          ANDLE     PATODT
      *
     C     XXFROM        ORLE      PAFRDT
     C     XXTO          ANDGE     PATODT
      *
     C                   EXSR      SRVOID
     C     XXVOID        IFEQ      *OFF
      *
     C                   MOVE      '1'           *IN65
     C                   MOVE      '1'           *IN67
      *** BEG ADD *** 02/28/23 *** WC2430 *****************************
       If isModEnabled('New Payments');
          GetClaimsChecksPayments();
          Exfmt WDWMSG2;

          // Cancel Button Selected - Set return code
          If BtnCancel = *on;
             xxRTCD = '4';
          Endif;

          // Pend Payment Button Selected - Process Override
          If BtnPendPay = *on;
             xxRTCD = '1';
          Endif;

       Else;
      *** END ADD *** 02/28/23 *** WC2430 *****************************
     C                   EXSR      PAASGN
     C                   EXFMT     WDWMSG
      *** BEG ADD *** 02/28/23 *** WC2430 *****************************
       Endif;
      *** END ADD *** 02/28/23 *** WC2430 *****************************
      *** WINDOW
     C                   SETON                                        90
     C                   GOTO      ENDCK1
     C                   ENDIF
     C                   ENDIF                                                  Dates Overlap
     C                   ENDIF                                                  Possible Dup
      *
     C     NXTPA2        TAG
      *
     C     PAKY          READE     PPA02                                  60
     C                   ENDDO                                                  DOW *IN60 PAKY
     C*** End add ***** 06/11/03 **************************************
      *
      *-----------------
      *  Re-position/read original Request for Payment record
      *
     C     RPKY          CHAIN     PRP01                              60
      *
     C     ENDCK1        TAG
      *
     C                   MOVE      *IN90         I90
      *
      ***** BEG ADD ***** 04/26/07 ************************************
      *
     C     Q1PRMS        IFGT      16
     C                   MOVE      XXRTCD        @@RTCD
     C                   ENDIF                                                  Q1PRMS > 16
      ***** END ADD ***** 04/26/07 ************************************
      *
       //***** BEG ADD ***** 02/29/23 ************************************
         // Set Comments Parameter based on entry made to the Comments
         // field on the Duplicate Payment panel.
         If Q1PRMS > 17;
            @@CMNT = scComment;
         Endif;
       //***** END ADD ***** 02/29/23 ************************************
     C                   SETON                                        LR
      *
      *================================================================
      *  PAASGN - Assign Fields in Dup Payment Window using WDPAP Fields
      *================================================================
     C     PAASGN        BEGSR
      *
     C                   Z-ADD     PAFND         SCDFND
     C                   Z-ADD     PAFYR         SCDFYR
     C                   Z-ADD     PACASE        SCDCAS
     C                   MOVEL     PARECD        SCDRC
     C                   MOVEL     PARETP        SCDTY
     C                   Z-ADD     PAFEIN        SCDFEI
     C*** BEGIN ADD *** 09/05/00 **************************************
     C     Q1PRMS        IFGT      15
     C*
     C                   Z-ADD     @@DMOD        SCDMOD
     C*
     C                   ENDIF
     C*** END   ADD *** 09/05/00 **************************************
     C*** BEGIN DEL *** 09/05/00 **************************************
     C**********           Z-ADDPAMOD     SCDMOD
     C*** END   DEL *** 09/05/00 **************************************
     C* FIX                MOVELSCNAME    SCDFDD
     C* FIX                MOVELSCCNAM    SCDCNM
     C* FIX                MOVELSCPNAM    SCDPNM
      *
     C                   MOVE      *BLANK        SCDIND
     C     *IN66         IFEQ      *ON
     C                   MOVE      '*'           SCDIND
      ***** BEG CHG ***** 04/26/07 ************************************
     C*** BEG ADD ***** 01/25/07 **************************************
     C************         MOVE '1'       @@RTCD
     C*** END ADD ***** 01/25/07 **************************************
     C                   MOVE      '1'           XXRTCD
      ***** END CHG ***** 04/26/07 ************************************
     C                   ENDIF
      *
     C                   Z-ADD     PAFEIN        SCDFID
     C                   Z-ADD     PAMOD         SCDMD
     C                   Z-ADD     PAFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDFDT
     C                   Z-ADD     PATODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTDT
     C                   Z-ADD     PAAMT         SCDAMT
     C                   MOVE      *BLANK        SCDST
     C                   MOVE      '  PAID  '    SCDST
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDRDT
     C                   Z-ADD     PACHK#        SCDCHK
     C*** BEGIN ADD *** 11/02/99 **************************************
     C                   Z-ADD     PAEOB#        SCDEOB
     C*** END ADD ***** 11/02/99 **************************************
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   MOVEL     PARQST        SCDREQ
     C*** END ADD ***** 05/19/00 **************************************
      *
     C*** BEGIN DLT *** 05/19/00 **************************************
     C********** RPDKEY    CHAINPRP04                60
     C**********           MOVEL*BLANKS   SCDREQ
     C********** *IN60     IFEQ *OFF
     C**********           MOVELRPUSER    SCDREQ
     C**********           ENDIF
     C*** END DLT ***** 05/19/00 **************************************
      *
      ***** BEG ADD ***** 05/01/07 ************************************
     C                   EXSR      SRSLVL
      ***** END ADD ***** 05/01/07 ************************************
      *
     C                   ENDSR
      *
      *================================================================
      *  RPASGN - Assign Fields in Dup Payment Window using WARPP Fields
      *================================================================
     C     RPASGN        BEGSR
      *
     C                   Z-ADD     RPFND         SCDFND
     C*** FIX              MOVELSCNAME    SCDFDD
     C                   Z-ADD     RPFYR         SCDFYR
     C                   Z-ADD     RPCASE        SCDCAS
     C*** FIX              MOVELSCCNAM    SCDCNM
     C                   MOVEL     RPRECD        SCDRC
     C                   MOVEL     RPRETP        SCDTY
     C                   Z-ADD     RPFEIN        SCDFEI
     C*** BEGIN ADD *** 09/05/00 **************************************
     C     Q1PRMS        IFGT      15
     C*
     C                   Z-ADD     @@DMOD        SCDMOD
     C*
     C                   ENDIF
     C*** END   ADD *** 09/05/00 **************************************
     C*** BEGIN DEL *** 09/05/00 **************************************
     C**********           Z-ADDRPMOD     SCDMOD
     C*** END   DEL *** 09/05/00 **************************************
     C*** FIX              MOVELSCPNAM    SCDPNM
      *
     C                   Z-ADD     RPFEIN        SCDFID
     C                   Z-ADD     RPMOD         SCDMD
     C                   Z-ADD     RPFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDFDT
     C                   Z-ADD     RPTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTDT
     C                   Z-ADD     RPAMT         SCDAMT
      *
     C                   MOVE      *BLANK        SCDIND
     C     *IN66         IFEQ      *ON
     C                   MOVE      '*'           SCDIND
      *
      ***** BEG CHG ***** 04/26/07 ************************************
     C*** BEG ADD ***** 01/25/07 **************************************
     C**********           MOVE '1'       @@RTCD
     C*** END ADD ***** 01/25/07 **************************************
     C                   MOVE      '1'           XXRTCD
      ***** END CHG ***** 04/26/07 ************************************
     C                   ENDIF
      *
     C                   MOVE      *BLANK        SCDST
     C                   SELECT
     C     RPSTAT        WHENEQ    *BLANK
     C                   MOVE      'RELEASED'    SCDST
     C     RPSTAT        WHENEQ    'P'
     C                   MOVE      ' PENDED '    SCDST
     C                   ENDSL
      *
     C                   Z-ADD     RPTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDRDT
     C                   Z-ADD     RPCHK#        SCDCHK
     C*** BEGIN ADD *** 11/02/99 **************************************
     C                   Z-ADD     RPEOB#        SCDEOB
     C*** END ADD ***** 11/02/99 **************************************
      *
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   MOVEL     RPRQST        SCDREQ
     C*** END ADD ***** 05/19/00 **************************************
     C*** BEGIN DLT *** 05/19/00 **************************************
     C******************** MOVELRPUSER    SCDREQ
     C*** END DLT ***** 05/19/00 **************************************
      *
      ***** BEG DEL ***** 05/01/07 ************************************
      ***** BEG ADD ***** 04/26/07 ************************************
      * If LUBA, check user security authority level on override pymnts.
     C*********  CLTID     IFEQ '133'
     C*********            EXSR SRSLVL
     C*********            ENDIF
      ***** END ADD ***** 04/26/07 ************************************
      ***** END DEL ***** 05/01/07 ************************************
      *
      ***** BEG ADD ***** 05/01/07 ************************************
     C                   EXSR      SRSLVL
      ***** END ADD ***** 05/01/07 ************************************
      *
     C                   ENDSR
      *
      ***** BEG ADD ***** 04/26/07 ************************************
      *
      *================================================================
      *  SRSLVL - Determine if User has sufficient authority to pymnts
      *================================================================
     C     SRSLVL        BEGSR
      *
     C     CXKY          CHAIN     PCX01                              61
     C     *IN61         IFEQ      *OFF
     C*
     C                   OPEN      WMUPL
     C     Q1USER        CHAIN     PUP01                              60
      *
     C     *IN66         IFEQ      *ON
     C                   MOVE      UPLVL         SRULVL                         User Level
     C                   MOVE      CXDPLV        SRCLVL                         PyDenial Level
      *
     C                   CALL      'SRCCSC'
     C                   PARM      UPTYPE        SRUTYP            1
     C                   PARM                    SRULVL            3
     C                   PARM      CXDPT1        SRCTYP            1
     C                   PARM                    SRCLVL            3
     C                   PARM      '1'           SREXEC            1
     C                   PARM      *ON           SRRES             1
     C                   PARM      CXDPT2        SRCTP2            1
     C                   PARM      CXDPT3        SRCTP3            1
     C                   PARM      CXDPT4        SRCTP4            1
     C                   PARM      CXDPT5        SRCTP5            1
     C                   PARM      ' '           SRCTP6            1
      *
     C     SRRES         IFEQ      *ON                                          NO Access
     C                   MOVE      '2'           XXRTCD
     C                   MOVE      *ON           *IN45
     C                   ENDIF                                                  SRRES=*ON
      *
     C                   ENDIF                                                  *IN66 = *ON
      *
     C     *IN67         IFEQ      *ON
     C                   MOVE      UPLVL         SRULVL                         User Level
     C                   MOVE      CXDPL2        SRCLVL                         PyDenial Level
      *
     C                   CALL      'SRCCSC'
     C                   PARM      UPTYPE        SRUTYP            1
     C                   PARM                    SRULVL            3
     C                   PARM      CXDPT6        SRCTYP            1
     C                   PARM                    SRCLVL            3
     C                   PARM      '1'           SREXEC            1
     C                   PARM      *ON           SRRES             1
     C                   PARM      CXDPT7        SRCTP2            1
     C                   PARM      CXDPT8        SRCTP3            1
     C                   PARM      CXDPT9        SRCTP4            1
     C                   PARM      CXDPT0        SRCTP5            1
     C                   PARM      ' '           SRCTP6            1
      *
     C     SRRES         IFEQ      *ON                                          NO Access
     C                   MOVE      '3'           XXRTCD
     C                   MOVE      *ON           *IN45
     C                   ENDIF                                                  SRRES=*ON
      *
     C                   ENDIF                                                  *IN66 = *ON
      *
     C                   CLOSE     WMUPL
     C                   ENDIF                                                  *IN61 = *OFF
      *
     C                   ENDSR
      *
      ***** END ADD ***** 04/26/07 ************************************
      *
      *================================================================
      *  SRVOID - Determine if a Dup Payment has been Voided.
      *================================================================
     C     SRVOID        BEGSR
      *
     C*** Begin dlt *** 07/29/02 **************************************
     C********** *LIKE     DEFN PAAMT     XXAMTP
     C********** *LIKE     DEFN PACHK#    SVCHK#
     C********** *LIKE     DEFN PASEQ#    SVSQ#
     C**********           Z-ADDPASEQ#    SVSQ#
     C**********           Z-ADDPAAMT     XXAMTP
     C**********           Z-ADDPACHK#    SVCHK#
      *
     C********** PAKY      READEPPA02                    60
      *
      *  Scan Remaining Payment Detail recs for same Check # looking for
      *  'Void', 'Stop', 'Reverse', 'Adjustment', or 'Recovery'
      *  transactions to determine a Final Payment Amount.
      *
     C********** *IN60     DOWEQ*OFF
      *
     C********** PACHK#    IFEQ SVCHK#
      *
      *  Process Specific Transaction.  XXAMT reflects Final Payment Amount.
      *
     C**********           SELEC
      *
      *                    "VOID" or "STOP" or "RECOVERY"
     C********** PATYPE    WHEQ 'V'
     C********** PATYPE    OREQ 'S'
     C********** PATYPE    OREQ 'R'
     C********** XXAMTP    SUB  PAAMT     XXAMTP
      *
      *                    "REVERSE" or "ADJUSTMENT"
     C********** PATYPE    WHEQ 'A'
     C********** PATYPE    OREQ 'Z'
     C********** XXAMTP    ADD  PAAMT     XXAMTP
      *
     C**********           ENDSL
     C**********           ENDIF
      *
     C********** PAKY      READEPPA02                    60
     C**********           ENDDO
      *
      *  If positive, then Payment is a duplicate.
      *  If zero, then Payment is not a duplicate.
      *
     C**********           MOVE *ON       XXVOID  1
      *
     C********** XXAMTP    IFGT *ZERO
     C**********           MOVE *OFF      XXVOID
     C**********           ENDIF
      *
      *  Re-position at Orignal Payment Detail record
      *
     C**********           Z-ADDSVSQ#     KKSEQ#
     C********** PAKY1     CHAINPPA02                60
     C*** End dlt ***** 07/29/02 **************************************
     C*** Begin add *** 07/29/02 **************************************
     C     PABASQ        IFNE      *ZERO
     C     PAADST        ORNE      *BLANKS
     C                   MOVE      *ON           XXVOID            1
     C                   ELSE
     C                   MOVE      *OFF          XXVOID
     C                   ENDIF
     C*** End add ***** 07/29/02 **************************************
      *
     C                   ENDSR
      ***** BEG ADD ***** 02/28/23 ***** WC2430 ***********************
        //*************************************************************
        // Procedure:
        // Populate Duplicate Payment Window using Request For Payment
        // fields from the WARPP file.
        //*************************************************************
        dcl-proc GetPaymentTrans;

           // Populate with correct heading
           clear scDupeText;
           If *in66;
             scDupeText = 'An exact duplicate payment was detected';
           Else;
             scDupeText = 'A possible duplicate payment was detected';
           Endif;

           // Populate display fields
           scPayName = SCDpNm; // Incoming Parameter
           scFein    = RPFein;
           scFrSrvDt = %Dec(%Char(%Date(RPFrDt:*CYMD):*usa0):8:0);
           scToSrvDt = %Dec(%Char(%Date(RPToDt:*CYMD):*usa0):8:0);
           scPostDt  = %Dec(%Char(%Date(RPTrDt:*CYMD):*usa0):8:0);
           scChkNum  = RPChk#;
           scChkAmt  = RPAmt;
           scReqst   = RPRqst;
           scEob     = RPEobs;
           // Determine Reserve Code & Type Description
           Chain (RPCo#:RPreCd) prt00;
           If %found(WTRTL1);
              scRsvTypD = RPreCd + ' - ' + %trim(RTdesc) + ' / ' +
              %trim(RTtype) + ' - ' + %trim(GetReserveType(RPreTp));
           Else;
              scRsvTypD = RPrecd;
           Endif;

           // If user authorized, show  'Override' button.
           If CheckSecLevel('OvrDupPmnt') = *On;
              isVisOvr = *on;
              posCancel = 300;
              posOvride = 422;
           Else;
              posCancel = 422;
           Endif;

           // If manual payment type, don't show 'Pend Payment' button.
           If @@Rtcd <> 'M';
              isVisPndP = *on;
           Endif;

           //*** Beg Add *** 12/10/25 **************************************
           // If releasing a duplicate payment setup buttons correctly
           If inMode = 'R';
              isVisPndP = *off;
              isVisOvr  = *off;
              PosCancel = 545;
           Endif;
           //*** End Add *** 12/10/25 **************************************
        end-proc;
        //*************************************************************
        // Procedure:
        // Populate Duplicate Payment Window using Payment Detail
        // fields from the WDPAP file.
        //*************************************************************
        dcl-proc GetClaimsChecksPayments;

           // Populate with correct heading
           clear scDupeText;
           If *in66;
             scDupeText = 'An exact duplicate payment was detected';
           Else;
             scDupeText = 'A possible duplicate payment was detected';
           Endif;

           scPayName = SCDpNm; // Incoming Parameter
           scFein    = PAFein;
           scFrSrvDt = %Dec(%Char(%Date(PAFrDt:*CYMD):*usa0):8:0);
           scToSrvDt = %Dec(%Char(%Date(PAToDt:*CYMD):*usa0):8:0);
           scPostDt  = %Dec(%Char(%Date(PATrDt:*CYMD):*usa0):8:0);
           scChkNum  = PAChk#;
           scChkAmt  = PAAmt;
           scReqst   = PARqst;
           scEob     = PAEobs;

           // Determine Reserve Code & Type Description
           Chain (PACo#:PAreCd) prt00;
           If %found(WTRTL1);
              scRsvTypD = PAreCd + ' - ' + %trim(RTdesc) + ' / ' +
              %trim(RTtype) + ' - ' + %trim(GetReserveType(PAreTp));
           Else;
              scRsvTypD = PArecd;
           Endif;

           // If user is authorized then show  'Override' button.
           If CheckSecLevel('OvrDupPmnt') = *On;
              isVisOvr = *on;
              posCancel = 300;
              posOvride = 422;
           Else;
              posCancel = 422;
           Endif;

           // If manual payment type then dont show 'Pend Payment' button.
           If @@Rtcd <> 'M';
              isVisPndP = *on;
           Endif;
        end-proc;
        //*************************************************************
        // Procedure:
        // Determine Reserve Type Description
        //*************************************************************
        dcl-proc GetReserveType;
           dcl-pi *n char(20);
              strRsvTyp char(1);
           end-pi;
       Select;
       When strRsvTyp = 'M';
         return 'Medical';
       When strRsvTyp = 'E';
         return 'Expense';
       When strRsvTyp = 'L';
         return 'Legal';
       When strRsvTyp = 'C';
         return 'Compensation';
       Other;
         return ' ';
       EndSl;
        end-proc;
      ***** END ADD ***** 02/28/23 ***** WC2430 ***********************
