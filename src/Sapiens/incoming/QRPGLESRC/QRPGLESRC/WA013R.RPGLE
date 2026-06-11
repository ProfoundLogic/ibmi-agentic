     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) bnddir('STBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA013R - API:  Payment Edit III
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
     H*** Beg Add ***** 12/05/07 **************************************
     H* 09 -If the claim indicates a 'Y' in the Denied flag, then the users
     H*     security level and type must pass the security level and type
     H*     defined in the Claims/DARTS Control file.
     H*** End Add ***** 12/05/07 **************************************
     H* 18 -User Parameter record check.
     H* 19 -The transaction will cause the user's reserve authority limit,
     H*     defined in the user profile, to be exceeded.
     H* 20 -If the claim is open, calculate the Remaining Reserve for the
     H*     Reserve Type.  The amount being released cannot be greater
     H*     than the Remaining Reserve.
     H* 21 -If the claim is closed, don't allow check release if there is
     H*     no entry in the reserve cushion file for the reserve code and
     H*     claim type (indemnity vs medical only).
     H* 22 -If EOB# ne 0, accumulate the total amount of all pended requests
     H*     for the same EOB#.  The reserve amount for all of these
     H*     transactions must be valid.
     H* 23 -If the claim is closed,
     H*     Accumulate all WDPAP transactions with the same reserve code
     H*     and a transaction date > the latest claim closing date.
     H*     Add any request for payments (WARPP) which are currently
     H*     released for the same reserve code including the request you
     H*     are attempting to release.
     H*     If this amount exceeds the reserve cushion, send an error msg.
     H* 39 -Payment Amount must be greater than zero.
     H* 43 -The payment amount is greater than the payment authority
     H*     limit defined in the user profile.
     H*
     H*
     H*
     H*
     H*  CREATED: 07/02/97  C:30  Project:  WC660
     H*****************************************************************
     H*  REVISIONS:                                                   *
     h*                                                               *
     h* 10/21/19 C:079 Support: 152/84105                             *
     h*                Prevent Fee Checks from auto releasing when    *      after
     h*                provider check is being released               *      after
     H*                                                               *
     H* 06/20/19 C:62  Project: 152MAV                                *
     H*                Add Negative Fee Check Processing.             *
     H*                                                               *
     H* 11/30/16 C: 53 Support: 44425
     H*                correct code for 114MXO
     H*
     H* 04/12/16 C:309 Project: 114MXO                                *
     H*                Added check for Client# 114 for "Negative      *
     H*                fee adjust/ReConsiderations"                   *
     H*                                                               *
     H* 02/09/10 C:062 SUPPORT 69943
     H*                Don't release if group/prov is negative
     H*                                                               *
     H* 11/09/09 C:220 Project: 136MEP                                *
     H*                Negative fee adjust/ReConsiderations           *
     H*                                                               *
     H* 12/18/08 C:180 SUPPORT 66197
     H*                Fix security for  delayed claim.
     H*
     H* 04/01/08 C:191  Project: 139MIV                               *
     H*                 Patriot Fees on Bill reductions               *
     H*                                                               *
     H* 12/05/07 C:193  Project: 135WKJ                               *
     H*                 Change Denied Claim Payment Security Edit     *
     H*                                                               *
     H*  09/09/02 C:15 Project: WC577                                 *
     H*                Fixed reserve edit.  If an EOMB pmt was being  *
     H*                released and the BRS check's rsv code did not  *
     H*                exist on the claim, the pgm would have allowed *
     H*                the pmt to be released.  (This issue was       *
     H*                identified while researching the monthly       *
     H*                analysis claim pmt discrepancies at ERS.)      *
     H*                                                               *
     H*  07/02/01 C:15 Support: WC114/28061                           *
     H*                Chgd pgm to tighten edits to prevent converted *
     H*                data from giving false reserve edits.          *
     H*                                                               *
     H*  04/20/01 C:15 Support: WC113/27189                           *
     H*                Chgd pgm to use logical WARPLN instead of      *
     H*                WARPLA so that the FYR/CASE are part of the    *
     H*                key.                                           *
     H*                                                               *
     H*  11/06/00 C:15 Support: WC065/24863                           *
     H*                Changed pgm to prevent user from exceeding the *
     H*                reserve cushion on closed claims.              *
     H*                                                               *
     H*  07/10/00 C:15 Project: WC577                                 *
     H*                Changed pgm to prevent negative reserves when  *
     H*                releasing an EOMB pmt and the BRS check doesn't*
     H*                have the same reserve code as the provider pmt.*
     H*                                                               *
     H*  05/30/00 C:53 Project: WC991                                 *
     H*                Add parm and edit #12 to pgm                   *
     H*                Corrected Q1PRMS check.                        *
     H*                                                               *
     H*  03/22/00 C:62 Support: MC048/22221    ** IMPORTANT **        *
     H*                add recon to parm list.                        *
     H*                ** DO NOT INSTALL WITHOUT WARPLA **            *
     H*                                                               *
     H*  02/16/99 C:15 Support: WC025/16418                           *
     H*                Changed pgm to prevent payments on Record Only *
     H*                claims.                                        *
     H*                                                               *
     H*  12/29/97 C:30 Project: WC659                                 *
     H*           Pass W8080R accumulated amount (# future checks     *
     H*           times check amount) for edit 20.                    *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
      *  Claim Master file
     FWMCML1    IF   E           K DISK
      *________________________________________________________________
      *** Beg Add ***** 12/05/07 **************************************
      *  Claim/DARTS Control file
     FWMCXL     IF   E           K DISK
      *________________________________________________________________
      *** End Add ***** 12/05/07 **************************************
      *  User Parameter file
     FWMUPL     IF   E           K DISK
      *________________________________________________________________
      *  Request for Payment file -       KEY = CO#/FND/FYR/CASE/SEQ#
     FWARPL     IF   E           K DISK
      *________________________________________________________________
      *  Request for Payment file -       KEY = CO#/FND/FYR/CASE/EOB#
     F*** Begin chg *** 04/20/01 **************************************
     F******WARPLA  IF  E           K        DISK
     FWARPLN    IF   E           K DISK
     F*** End chg ***** 04/20/01 **************************************
     F                                     RENAME(PRP01:PRP02)
      *________________________________________________________________
      *  Reserve Cushion Definition -     KEY = CO#/FND/RESV/EFDT
     FWDRCL     IF   E           K DISK
      *________________________________________________________________
      *  Claim History file         -     KEY = CO#/FND/FYR/CASE/EFDT
     FWMCHL     IF   E           K DISK
      *________________________________________________________________
      *  Payment Detail file        -     KEY = CO#/FND/FYR/CASE/TRDT/SEQ#
     FWDPAL5    IF   E           K DISK
      *________________________________________________________________

      //*** Begin Add *** 06/20/19 ****************************************
      /copy sPrModule
      //*** End   Add *** 06/20/19 ****************************************
     D*** BEGIN ADD *** 07/10/00 **************************************
     D REM             S              9  2 DIM(10)
     D*** END ADD ***** 07/10/00 **************************************
     D*** BEGIN ADD *** 02/09/10 **************************************
     D RMG             S              9  2 DIM(10)
     D*** END   ADD *** 02/09/10 **************************************
     D*** BEGIN ADD *** 07/02/01 **************************************
     D RECD            S              2    DIM(10)
     D*** END ADD ***** 07/02/01 **************************************
     D*** BEGIN ADD *** 11/30/16 **************************************
     D RSVBAL          S              9  2
     D*** END ADD ***** 11/30/16 **************************************
     D*** Begin Add *** 06/20/19 ****************************************
     D allowNegFees    s               n
     D*** End   Add *** 06/20/19 ****************************************
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
      *  User Parameter file key
      *

     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *  Claim Master key
      *
     C     CMKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
      *
      *   Reserve Cushion Definition file key
      *
     C     RCKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C*** BEGIN CHG *** 07/10/00 **************************************
     C******************** KFLD           @@RECD
     C                   KFLD                    XXRECD
     C*** END CHG ***** 07/10/00 **************************************
     C                   KFLD                    XXEFDT
      *
     C     RCKY1         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C*** BEGIN CHG *** 07/10/00 **************************************
     C******************** KFLD           @@RECD
     C                   KFLD                    XXRECD
     C*** END CHG ***** 07/10/00 **************************************
      *
      *   Claim History file
      *
     C     CHKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
      *
      *   Payment Detail file
      *
     C     PAKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
      *
     C     PAKY2         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C                   KFLD                    XXEFDT
      *
      *  Request for Payment key   - WARPLA
      *
     C     RPKY2         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C*** Begin add *** 04/20/01 **************************************
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C*** End add ***** 04/20/01 **************************************
     C                   KFLD                    @@EOB#
     C*** BEGIN ADD *** 03/24/00 **************************************
     C                   KFLD                    KKEOBS
     C*** END   ADD *** 03/24/00 **************************************
     C*** Beg add ***** 12/05/07 **************************************
      *
      *  Keylist for Claims Admin/DARTS File
      *
     C     CXKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C*** End add ***** 12/05/07 **************************************
      *________________________________________________________________
     C     *LIKE         DEFINE    RPCO#         @@CO#
     C     *LIKE         DEFINE    RPFND         @@FND
     C     *LIKE         DEFINE    RPFYR         @@FYR
     C     *LIKE         DEFINE    RPCASE        @@CASE
     C     *LIKE         DEFINE    RPAMT         @@AMT
     C     *LIKE         DEFINE    RPAMT         XXAMT
     C     *LIKE         DEFINE    RPAMT         XXDIF
     C     *LIKE         DEFINE    RPAMT         CMREM
     C     *LIKE         DEFINE    RPAMT         XXREM
     C     *LIKE         DEFINE    RPRECD        @@RECD
     C     *LIKE         DEFINE    RPRETP        @@RETP
     C     *LIKE         DEFINE    RPDT          XXDT
     C     *LIKE         DEFINE    RPEOB#        @@EOB#
     C     *LIKE         DEFINE    RPSEQ#        @@SEQ#
     C     *LIKE         DEFINE    RCEFDT        XXEFDT
     C     *LIKE         DEFINE    RCMAMT        XXCUSH
      *** BEGIN ADD *** 12/29/97 ***********************************************
     C     *LIKE         DEFINE    RPAMT         XXTAMT                         Accumulated Amount
     C     *LIKE         DEFINE    RPAMT         @@TAMT                         Accumulated Amount
      *** END   ADD *** 12/29/97 ***********************************************
     C*** BEGIN ADD *** 03/24/00 **************************************
     C     *LIKE         DEFINE    RPEOBS        KKEOBS
     C*** END   ADD *** 03/24/00 **************************************
     C*** BEGIN ADD *** 07/10/00 **************************************
     C     *LIKE         DEFINE    RPRECD        XXRECD
     C*** END ADD ***** 07/10/00 **************************************
     C*** BEGIN ADD *** 02/09/10 **************************************
     C     *LIKE         DEFINE    RPFEIN        FEIN
     C     *LIKE         DEFINE    RPMOD         MOD
     C     *LIKE         DEFINE    RPPGTP        PGTP
     C*** END   ADD *** 02/09/10 **************************************
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
     C                   PARM                    @@EOB#                         EOMB #
     C                   PARM                    @@SEQ#                         WARPP SEQ#
     C                   PARM                    @@AMT                          Requested Amount
     C                   PARM                    @@RECD                         Reserve Code
     C                   PARM                    @@RETP                         Reserve Type
     C                   PARM                    @@RC              2            Return Code
      *** BEGIN ADD *** 12/29/97 ***********************************************
     C                   PARM                    @@TAMT                         Accumulated Amount
      *** END   ADD *** 12/29/97 ***********************************************
     C*** BEGIN ADD *** 03/22/00 **************************************
     C                   PARM                    @@RCN             3 0          RECON       Amount
     C*** END   ADD *** 03/22/00 **************************************
     C*** BEG ADD *** 05/30/00 **************************************
     C                   PARM                    @@TRCD            2            Payment Code
     C*** END ADD *** 05/30/00 **************************************
      *
     C*** BEG DEL *** 05/30/00 **************************************
     C*      @@TAMT info
     C*** This code is no longer needed as all programs will pass in
     C*** the @@TAMT parm. It will be the check amt from single check
     C*** programs or total amt of the reoccuring checks.
     C********** Q1PRMS    IFEQ 11
     C**********           Z-ADD@@TAMT    XXTAMT
     C**********           ELSE
     C**********           Z-ADD@@AMT     XXTAMT
     C**********           ENDIF
     C*** END CHG *** 05/30/00 **************************************
      *
      *** END   ADD *** 12/29/97 ***********************************************
      *________________________________________________________________
      *
     C                   MOVE      *BLANK        @@RC
      *________________________________________________________________
      *
      *  Edit 06:  Claim Master record check
      *
      *
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '06'          @@RC
     C                   RETURN
     C                   ENDIF
      *________________________________________________________________
      *
      *  Edit 18:  User Parameters record check
      *
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '18'          @@RC
     C                   RETURN
     C                   ENDIF
     C*** Beg Add ***** 12/05/07 *****************************************
      *________________________________________________________________
      *  EDIT 09 USES USER SECURITY VALUES TO CHECK AUTHORITY, SO
      *  MOVED AFTER EDIT 18 WHICH CHECKS THE AVAILABILITY OF USER
      *
      *  Edit 09:  Security Level & Type check with the Claim/
      *            DARTS Control file values.
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    @@CLT#            3
     C*** Begin Chg *** 06/20/19 ****************************************
      /free
           allowNegFees = *Off;
           If IsModEnabled('Negative Fee Checks');
              allowNegFees = *On;
           EndIf;
      /end-free
     C*** End   Chg *** 06/20/19 ****************************************
     C*
      *  Access Claim/DARTS Control file    CO#/FND
      *
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '09'          @@RC
     C                   RETURN
     C                   ENDIF
     C*
     C     @@CLT#        IFEQ      '135'
     C*
     C     CMDNY         IFEQ      'Y'                                          Denied Flag
     C***** BEGIN ADD ******* 12/18/08 ********************************
     C     CMDLY         OREQ      'Y'                                          Delayed Flag
     C***** END ADD ********* 12/18/08 ********************************
      *
      *  Security level and Type check
      *
     C                   MOVE      UPTYPE        SRUTYP            1            User Type
     C                   MOVE      UPLVL         SRULVL            3            User Level
     C                   MOVE      CXPDCT        SRCTYP            1            PyDenial Type
     C                   MOVE      CXPYOD        SRCLVL            3            PyDenial Level
     C                   MOVE      '1'           SREXEC            1            Execution Type
     C                   MOVE      *ON           SRRES             1            Result Flag
      *
     C                   CALL      'SRCCSC'
     C                   PARM                    SRUTYP
     C                   PARM                    SRULVL
     C                   PARM                    SRCTYP
     C                   PARM                    SRCLVL
     C                   PARM                    SREXEC
     C                   PARM                    SRRES
     C                   PARM      CXPDC2        SRCTP2            1
     C                   PARM      CXPDC3        SRCTP3            1
     C                   PARM      CXPDC4        SRCTP4            1
     C                   PARM      CXPDC5        SRCTP5            1
     C                   PARM      ' '           SRCTP6            1
      *
     C     SRRES         IFEQ      *ON                                          NO Access
     C                   MOVE      '09'          @@RC
     C                   RETURN
     C                   ENDIF                                                  SRRES=*ON
     C*
     C                   ENDIF                                                  CMDNY=Y
      *
      *           Checks if the user has authority to delay the claim
      *
     C***** BEGIN DELETE **** 12/18/08 ********************************
      **    NO LONGER SEPARATE SECURITY VALIDATION FOR DELAYED.
      **
      **    DELAYED AND DENIED NOW HAVE SAME SECURITY VALIDATION
      **    AS SHOWN ABOVE.
      **
     C****       CMDLY     IFEQ 'Y'                        Delayed Flag
      *
      *  Security level and Type check
     C                   MOVE      CXDSLV        SRCLVL
      *
     C****                 CALL 'SRCCSC'
     C****                 PARM           SRUTYP
     C****                 PARM           SRULVL
     C****                 PARM CXDS01    SRCTYP
     C****                 PARM           SRCLVL
     C****                 PARM '1'       SREXEC
     C****                 PARM           SRRES
     C****                 PARM CXDS02    SRCTP2  1
     C****                 PARM CXDS03    SRCTP3  1
     C****                 PARM CXDS04    SRCTP4  1
     C****                 PARM CXDS05    SRCTP5  1
     C****                 PARM ' '       SRCTP6  1
      ****
     C****       SRRES     IFEQ *ON                        NO Access
     C****                 MOVE '09'      @@RC
     C****                 RETRN
     C****                 ENDIF                           SRRES=*ON
      ****
     C****                 ENDIF                           CMDLY=Y
     C***** END DELETE  ***** 12/18/08 ********************************
      *
     C                   ENDIF                                                  CLTID=135
     C*** End Add ***** 12/05/07 *****************************************
      *________________________________________________________________
      *
      *  Edit 39:  Payment Amount must be greater than zero.
      *
     C*** BEG ADD ****** 04/01/08 ************************************
     C     @@CLT#        IFNE      '139'
     C*** BEG ADD ****** 11/09/09 ************************************
     C*** Begin Chg *** 06/20/19 ****************************************
     C*****@@CLT#        ANDNE     '136'
     C     allowNegFees  ANDEQ     *Off
     C*** End   Chg *** 06/20/19 ****************************************
     C*** END ADD ****** 11/09/09 ************************************
     C*** Beg Add ****** 04/12/16 ************************************
     C     @@CLT#        ANDNE     '114'
     C*** End Add ****** 04/12/16 ************************************
     C*** END ADD ****** 04/01/08 ************************************
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
     C*** BEGIN ADD *** 10/14/97 ***************************************
      *  Edit 43:  This transaction exceeds the user's pmt authority
      *              limit.
      *
     C     @@AMT         IFGT      UPPAL
     C                   MOVE      '43'          @@RC
     C                   RETURN
     C                   ENDIF
     C*** END ADD ***** 10/14/97 ***************************************
      *________________________________________________________________
      *
      *  Edit 19:  This transaction causes user to exceed their reserve
      *              authority limit.
      *
      *  Call SRRSVL to accumulate ALL previous Reserve Amount by User
      *  and subtract from User Authority Limit.
      *
     C*** BEGIN ADD *** 10/14/97 ***************************************
     C     CMSTAT        IFEQ      'C'
     C*** END ADD ***** 10/14/97 ***************************************
     C                   CALL      'SRRSVL'
     C                   PARM                    @@CO#
     C                   PARM                    @@FND
     C                   PARM                    @@FYR
     C                   PARM                    @@CASE
     C                   PARM                    @@RECD
     C                   PARM                    XXRAL            11 2
      *
      *** BEG CHG *** 05/30/00 **************************************  *********
     C     @@TAMT        IFGT      XXRAL                                        Authority AMT
     C********** @@AMT     IFGT XXRAL                      Authority AMT
      *** END CHG *** 05/30/00 **************************************  *********
     C                   MOVE      '19'          @@RC
     C                   RETURN
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 10/14/97 ***************************************
     C                   ENDIF
     C*** END ADD ***** 10/14/97 ***************************************
     C*** Begin add *** 07/02/01 **************************************
     C*________________________________________________________________
     C*   Load an array with the reserve code and amt of the pmt.  If
     C*     an EOMB is attached to the pmt, load the rsv codes and amt
     C*     of all associated pmts.
     C*
     C                   CLEAR                   REM
     C*** BEGIN ADD *** 02/09/10 **************************************
     C                   CLEAR                   RMG
     C*** END   ADD *** 02/09/10 **************************************
     C                   CLEAR                   RECD
     C                   Z-ADD     1             X
     C                   ADD       @@TAMT        REM(X)
     C                   MOVEL     @@RECD        RECD(X)
     C*** BEGIN ADD *** 02/09/10 **************************************
     C                   Z-ADD     *ZEROS        FEIN
     C                   Z-ADD     *ZEROS        MOD
     C                   MOVEL(P)  *BLANKS       PGTP
     C*** END   ADD *** 02/09/10 **************************************
     C*
     C     @@EOB#        IFNE      *ZERO
     C*** Beg Add *** 10/21/19 *********************************************
     C     @@clt#        Andne     '152'
     C*** End Add *** 10/21/19 *********************************************
     C*
     C                   Z-ADD     *ZEROS        KKEOBS
     C     Q1PRMS        IFGT      11
     C                   Z-ADD     @@RCN         KKEOBS
     C                   ENDIF
     C*
     C     RPKY2         CHAIN     PRP02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     RPSTAT        IFEQ      'P'
     C*** BEGIN CHG *** 02/09/10 **************************************
     C********** RPSEQ#    ANDNE@@SEQ#
     C     RPSEQ#        IFNE      @@SEQ#
     C*** END   CHG *** 02/09/10 **************************************
     C*
     C                   Z-ADD     1             X
     C     RPRECD        LOOKUP    RECD(X)                                99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    RECD(X)                                99
     C                   MOVEL     RPRECD        RECD(X)
     C                   ENDIF
     C                   ADD       RPAMT         REM(X)
     C*** BEGIN ADD *** 02/09/10 **************************************
     C                   ELSE
     C     RPPGTP        IFEQ      'G'
     C                   Z-ADD     RPFEIN        FEIN
     C                   Z-ADD     RPMOD         MOD
     C                   MOVEL(P)  RPPGTP        PGTP
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 02/09/10 **************************************
     C*
     C                   ENDIF
     C*
     C     RPKY2         READE     PRP02                                  60
     C                   ENDDO
     C*
     C                   ENDIF
     C*** End add ***** 07/02/01 **************************************
     C*** BEGIN ADD *** 02/09/10 **************************************
     C     PGTP          IFEQ      'G'
     C     @@TAMT        ANDLT     0
     C     CMKY          CHAIN     PRP02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     RPSTAT        IFEQ      'P'
     C     RPFEIN        ANDEQ     FEIN
     C     RPMOD         ANDEQ     MOD
     C     RPPGTP        ANDEQ     PGTP
     C                   ADD       RPAMT         RMG(X)
     C                   ENDIF
     C*
     C     CMKY          READE     PRP02                                  60
     C                   ENDDO
     C                   ENDIF
     C*** END   ADD *** 02/09/10 **************************************
      *________________________________________________________________
      *
      *  Accumulate ALL Released Request Amounts
      *
     C                   Z-ADD     *ZERO         XXREM                          Sum RP Amts Released
     C*** Begin dlt *** 07/02/01 **************************************
     C*** BEGIN ADD *** 07/10/00 **************************************
     C*******************  CLEARREM
     C*** END ADD ***** 07/10/00 **************************************
     C*** End dlt ***** 07/02/01 **************************************
      *
     C     CMKY          CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
      *
     C     RPSTAT        IFEQ      ' '
     C*** BEGIN DLT *** 07/10/00 **************************************
     C********** RPRECD    ANDEQ@@RECD
     C*** END DLT ***** 07/10/00 **************************************
     C     RPSEQ#        ANDNE     @@SEQ#
     C*** BEGIN DLT *** 07/10/00 **************************************
     C******************** ADD  RPAMT     XXREM
     C*** END DLT ***** 07/10/00 **************************************
     C*** BEGIN ADD *** 07/10/00 **************************************
     C                   Z-ADD     1             X
     C*** Begin chg *** 07/02/01 **************************************
     C*********  RPRECD    LOKUPRT,X                     99
     C     RPRECD        LOOKUP    RECD(X)                                99
     C*** End chg ***** 07/02/01 **************************************
     C   99              ADD       RPAMT         REM(X)
     C*** END ADD ***** 07/10/00 **************************************
     C*** BEGIN ADD *** 02/09/10 **************************************
     C     RPFEIN        IFEQ      FEIN
     C     RPMOD         ANDEQ     MOD
     C     RPPGTP        ANDEQ     PGTP
     C     @@TAMT        ANDLT     0
     C   99              ADD       RPAMT         RMG(X)
     C                   ENDIF
     C*** END   ADD *** 02/09/10 **************************************
     C                   ENDIF
      *
     C     CMKY          READE     PRP01                                  60
     C                   ENDDO
      *________________________________________________________________
      *** BEG ADD *** 05/30/00 ****************************************
      *
      *  Edit 12:  Reserve Code must be valid for the claim.
      *
     C*** Begin dlt *** 09/09/02 **************************************
     C**********           Z-ADD1         X
     C********** @@RECD    LOKUPRT,X                     60
     C*** End dlt ***** 09/09/02 **************************************
     C*** Begin add *** 09/09/02 **************************************
     C                   DO        10            X
     C*
     C     RECD(X)       IFNE      *BLANKS
     C                   Z-ADD     1             Y                 3 0
     C     RECD(X)       LOOKUP    RT(Y)                                  60
     C*** End add ***** 09/09/02 **************************************
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '12'          @@RC
     C                   RETURN
     C                   ENDIF
     C*
     C*** Begin add *** 09/09/02 **************************************
     C                   ENDIF
     C                   ENDDO
     C*** End add ***** 09/09/02 **************************************
      *** END ADD *** 05/30/00 **************************************  *********
      *________________________________________________________________
      *
      *  Edit 20:  If the Claim is Open, the Released Amount
      *           can not be greater than Remaining Reserve
      *
      *** BEGIN CHG *** 12/29/97 ***********************************************
     C*********************ADD  @@AMT     XXREM            Add Requested Amt
      *
      *** BEG CHG *** 05/30/00 **************************************  *********
     C**********           ADD  XXTAMT    XXREM            Add Requested Amt
     C*** BEGIN DLT *** 07/10/00 **************************************
     C*******************  ADD  @@TAMT    XXREM            Add Requested Amt
     C*** END DLT ***** 07/10/00 **************************************
      *** END CHG *** 05/30/00 **************************************  *********
      *** END   CHG *** 12/29/97 ***********************************************
     C*** Begin dlt *** 07/02/01 **************************************
     C*** BEGIN ADD *** 11/06/00 **************************************
     C******************** ADD  @@TAMT    REM,X
     C*** END ADD ***** 11/06/00 **************************************
     C*** End dlt ***** 07/02/01 **************************************
      *
     C     CMSTAT        IFNE      'C'
      *
     C                   Z-ADD     *ZERO         CMREM
     C                   Z-ADD     1             X                 3 0
      *
     C*** Begin dlt *** 07/02/01 **************************************
     C********** @@RECD    LOKUPRT,X                     60
     C********** *IN60     IFEQ *ON
     C**********           Z-ADDRR,X      CMREM
     C**********           ENDIF
     C*** End dlt ***** 07/02/01 **************************************
     C*** BEGIN DLT *** 11/06/00 **************************************
     C*** BEGIN ADD *** 07/10/00 **************************************
     C*******************  ADD  @@TAMT    REM,X
     C*** END ADD ***** 07/10/00 **************************************
     C*** END DLT ***** 11/06/00 **************************************
     C*** Begin add *** 07/02/01 **************************************
     C                   DO        10            X
     C*
     C     RECD(X)       IFNE      *BLANKS
     C                   Z-ADD     1             Y                 3 0
     C     RECD(X)       LOOKUP    RT(Y)                                  60
     C   60              Z-ADD     RR(Y)         CMREM
     C*** End add ***** 07/02/01 **************************************
     C*** Begin add *** 09/09/02 **************************************
     C  N60              Z-ADD     0             CMREM
     C*** End add ***** 09/09/02 **************************************
      *
     C*** BEGIN CHG *** 07/10/00 **************************************
     C********** XXREM     IFGT CMREM
     C     REM(X)        IFGT      CMREM
     C*** END CHG ***** 07/10/00 **************************************
     C                   MOVE      '20'          @@RC
     C                   RETURN
     C                   ENDIF
      *
     C*** Begin add *** 07/02/01 **************************************
     C                   ENDIF
     C                   ENDDO
     C*** End add ***** 07/02/01 **************************************
      *
     C                   ENDIF
      *
      *________________________________________________________________
      *
      *  Edit 21:  If the claim is closed, don't allow check release if
      *     there is no entry in the reserve cushion file for the reserve
      *     code and claim type (indemnity vs medical only).
      *
     C     CMSTAT        IFEQ      'C'
      *
      *  Get Effective Date of Closing
      *
     C     CHKY          SETGT     PCH01
     C     CHKY          READPE    PCH01                                  60
      *
     C     *IN60         IFEQ      '0'
     C     'C'           ANDEQ     CHOCST
      *
      *   Check to see if Payment Type will allow Payment due to a
      *   Reserve Cushion. The Reserve Code associated with the
      *   Payment Type msut exist in the Reserve Cushion file.
      *
     C*** BEGIN ADD *** 07/10/00 **************************************
     C                   MOVEL     @@RECD        XXRECD
     C*** END ADD ***** 07/10/00 **************************************
     C                   Z-ADD     *ZERO         XXCUSH
     C                   Z-ADD     CHEFDT        XXEFDT
     C     RCKY          SETGT     PRC01
     C     RCKY1         READPE    PRC01                                  60
     C     *IN60         IFEQ      '0'
      *
     C                   SELECT
      *
     C     CMLTFL        WHENEQ    'I'
     C                   Z-ADD     RCLAMT        XXCUSH                         LOST TIME AMT
      *
     C     CMLTFL        WHENEQ    'M'
     C*** BEGIN DLT *** 02/16/99 **************************************
     C********** CMLTFL    OREQ 'R'
     C*** END DLT ***** 02/16/99 **************************************
     C                   Z-ADD     RCMAMT        XXCUSH                         MED ONLY AMT
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  RCKY = '0'
      *
     C     XXCUSH        IFEQ      *ZERO
     C                   MOVE      '21'          @@RC
     C                   RETURN
     C                   ENDIF
      *
     C                   ENDIF                                                  CHKY = '0'
     C                   ENDIF                                                  CMSTAT = 'C'
      *________________________________________________________________
     C*** Begin dlt *** 07/02/01 **************************************
     C**********
      *  Edit 22:  If the EOB# not equal zero, sum ALL Pended EOB# WARPP
     C**********  records to previous Sum (Released Requests) for both
     C**********  Open and Closed claims.
     C**********
     C********** @@EOB#    IFGT *ZERO
     C**********  If the Claim is Open, then the Summed Amount can not be
     C**********  greater than Remaining Reserve.
     C**********
     C*** BEGIN ADD *** 03/24/00 **************************************
     C********** Q1PRMS    IFGT 11
     C**********           Z-ADD@@RCN     KKEOBS
     C**********           ELSE
     C**********           Z-ADD*ZEROS    KKEOBS
     C**********           ENDIF
     C*** END   ADD *** 03/24/00 **************************************
     C**********
     C********** RPKY2     CHAINPRP02                60
     C********** *IN60     DOWEQ'0'
     C**********
     C********** RPSTAT    IFEQ 'P'
     C*** BEGIN DLT *** 07/10/00 **************************************
     C********** RPRECD    ANDEQ@@RECD
     C*** END DLT ***** 07/10/00 **************************************
     C********** RPSEQ#    ANDNE@@SEQ#
     C*** BEGIN DLT *** 07/10/00 **************************************
     C******************** ADD  RPAMT     XXREM
     C*** END DLT ***** 07/10/00 **************************************
     C*** BEGIN ADD *** 07/10/00 **************************************
     C**********           Z-ADD1         X
     C********** RPRECD    LOKUPRT,X                     99
     C***99*****           ADD  RPAMT     REM,X
     C********** *IN99     IFEQ *OFF
     C**********           Z-ADD1         X
     C********** *BLANKS   LOKUPRT,X                     99
     C**********           MOVELRPRECD    RT,X
     C**********           ADD  RPAMT     REM,X
     C**********           ENDIF
     C*** END ADD ***** 07/10/00 **************************************
     C**********           ENDIF
     C**********
     C********** RPKY2     READEPRP02                    60
     C**********           ENDDO
     C**********
     C********** CMSTAT    IFNE 'C'
     C**********
     C*** BEGIN DLT *** 07/10/00 **************************************
     C********** CMREM     IFLT XXREM
     C**********           MOVE '22'      @@RC**********
     C**********           RETRN
     C**********           ENDIF
     C*** END DLT ***** 07/10/00 **************************************
     C*** BEGIN ADD *** 07/10/00 **************************************
     C**********           DO   10        X
     C**********
     C********** REM,X     IFGT 0
     C********** REM,X     ANDGTRR,X
     C**********           MOVE '22'      @@RC
     C**********           RETRN
     C**********           ENDIF
     C**********
     C**********           ENDDO
     C*** END DLT ***** 07/10/00 **************************************
     C**********
     C**********           ENDIF
     C**********
     C**********           ENDIF
     C*** End dlt ***** 07/02/01 **************************************
      *________________________________________________________________
      *
      * 23 -If the claim is closed,
      *     Accumulate all WDPAP transactions with the same reserve code
      *     and a transaction date > the latest claim closing date.
      *     Add any request for payments (WARPP) which are currently
      *     released for the same reserve code including the request you
      *     are attempting to release.
      *     If this amount exceeds the reserve cushion, send an error msg.
      *
     C     CMSTAT        IFEQ      'C'
      *
      *  Accumulate Payments made into Summed  Released Payments and
      *  EOB # Pended Paymets.
      *
     C     PAKY2         SETLL     PPA01
     C     PAKY          READE     PPA01                                  60
      *
     C     *IN60         DOWEQ     '0'
      *
     C*** BEGIN DLT *** 07/10/00 **************************************
     C********** PARECD    IFEQ @@RECD
     C*** END DLT ***** 07/10/00 **************************************
     C*** BEGIN ADD *** 07/10/00 **************************************
     C                   Z-ADD     1             X
     C*** Begin add *** 07/02/01 **************************************
     C     PARECD        LOOKUP    RECD(X)                                99
     C     *IN99         IFEQ      *ON
     C*** End add ***** 07/02/01 **************************************
     C*** Begin dlt *** 07/02/01 **************************************
     C********** PARECD    LOKUPRT,X                     99
     C********** *IN99     IFEQ *OFF
     C**********           Z-ADD1         X
     C********** *BLANKS   LOKUPRT,X                     99
     C**********           MOVELPARECD    RT,X
     C**********           ENDIF
     C*** End dlt ***** 07/02/01 **************************************
     C*** END ADD ***** 07/10/00 **************************************
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'A'
     C     PATYPE        OREQ      'Z'
     C*** BEGIN DLT *** 07/10/00 **************************************
     C******************** ADD  PAAMT     XXREM
     C******************** ELSE
     C******************** SUB  PAAMT     XXREM
     C*** END DLT ***** 07/10/00 **************************************
     C*** BEGIN ADD *** 07/10/00 **************************************
     C                   ADD       PAAMT         REM(X)
     C                   ELSE
     C                   SUB       PAAMT         REM(X)
     C*** END ADD ***** 07/10/00 **************************************
     C                   ENDIF
     C*
     C*** Begin add *** 07/02/01 **************************************
     C                   ENDIF
     C*** End add ***** 07/02/01 **************************************
     C*** BEGIN DLT *** 07/10/00 **************************************
     C******************** ENDIF
     C*** END DLT ***** 07/10/00 **************************************
     C*
     C     PAKY          READE     PPA01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ '0'
     C*
     C*** BEGIN DLT *** 07/10/00 **************************************
     C***Subtract Summed Payment from the Cushion Amount.
     C***If the Cushion is less than Zero, return with error code.
     C**********
     C**********           SUB  XXREM     XXCUSH
     C********** XXCUSH    IFLT *ZERO
     C**********           MOVE '23'      @@RC
     C**********           RETRN
     C**********           ENDIF
     C*** END DLT ***** 07/10/00 **************************************
     C*** BEGIN ADD *** 07/10/00 **************************************
     C*  Loop through the amounts and if the amount is greater than
     C*    the reserve cushion, return with error code.
     C*
     C                   DO        10            X
      *
     C     REM(X)        IFGT      0
      *
     C                   Z-ADD     0             XXCUSH
     C*** Begin chg *** 07/02/01 **************************************
     C******************** MOVELRT,X      XXRECD
     C                   MOVEL     RECD(X)       XXRECD
     C*** End chg ***** 07/02/01 **************************************
     C     RCKY          SETGT     PRC01
     C     RCKY1         READPE    PRC01                                  60
     C     *IN60         IFEQ      '0'
      *
     C                   SELECT
     C     CMLTFL        WHENEQ    'I'
     C                   Z-ADD     RCLAMT        XXCUSH                         LOST TIME AMT
     C     CMLTFL        WHENEQ    'M'
     C                   Z-ADD     RCMAMT        XXCUSH                         MED ONLY AMT
     C                   ENDSL
      *
     C                   ENDIF                                                  RCKY = '0'
      *
     C     REM(X)        IFGT      XXCUSH
     C                   MOVE      '23'          @@RC
     C                   RETURN
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDDO
     C*** END ADD ***** 07/10/00 **************************************
      *
     C                   ENDIF
      *
     C*** BEG ADD ****** 04/01/08 ************************************
      *  This edit is only for client 139 / 136
      * 50 - Allow payments to be released if the total paid, for that
      *      reserve, is adequate to prevent a negative total paid amount
     C*
     C*** Beg Add ****** 11/30/16 ************************************
     C                   Z-ADD     1             X
     C     @@RECD        LOOKUP    RT(X)                                  60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     RR(X)         RSVBAL
     C                   ADD       @@TAMT        RSVBAL
     C                   ENDIF
     C*** End Add ****** 11/30/16 ************************************
     C     @@CLT#        IFEQ      '139'
     C     @@TAMT        ANDLT     0
     C*** BEGIN CHG *** 02/09/10 **************************************
     C********** REM,1     ANDLT0
     C     RMG(1)        ANDLT     0
     C*** END   CHG *** 02/09/10 **************************************
     C*** BEG ADD ****** 11/09/09 ************************************
     C*** Begin Chg *** 06/20/19 ****************************************
     C*****@@CLT#        OREQ      '136'
     C     allowNegFees  OREQ      *On
     C*** End   Chg *** 06/20/19 ****************************************
     C*** Beg Del ****** 11/30/16 ************************************
     C*** Beg Add ****** 04/12/16 ************************************
     C***  @@CLT# ****** OREQ      '114'
     C*** End Add ****** 04/12/16 ************************************
     C***  REM(1) ****** ANDLT     0
     C*** End Del ****** 11/30/16 ************************************
     C     @@TAMT        ANDLT     0
     C*** Beg Add ****** 11/30/16 ************************************
     C     RSVBAL        ANDLT     0
     C*** END ADD ****** 11/09/09 ************************************
     C     @@CLT#        OREQ      '114'
     C     @@TAMT        ANDLT     0
     C     RSVBAL        ANDLT     0
     C*** End Add ****** 11/30/16 ************************************
     C                   MOVE      '50'          @@RC
     C                   RETURN
     C                   ENDIF                                                  @@CLT#='139'
     C*** END ADD ****** 04/01/08 ************************************
      *________________________________________________________________
      *
      *  All Edits Passed - Return Code assigned Blank
      *
     C                   MOVE      *BLANK        @@RC
     C                   RETURN
