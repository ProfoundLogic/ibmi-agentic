     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller) bnddir('STBNDDIR')
     H/define CONVERT
     H/TITLE W7001R2 - UPDATE GROUP PAYMENT FILE
     H*****************************************************************
     H* DESCRIPTION:                                                  *
     H*    THIS PROGRAM UPDATES THE GROUP PAYMENT FILE WITH THE SUM   *
     H*     OF ALL CHECKS FOR EACH PROVIDER.                          *
     H* CALLED FROM: W7001C                                           *
     H* CALLS TO:    NONE                                             *
     H* 09/08/94  C:15  PROJECT: WC361                                *
     H*****************************************************************
     H*  REVISION:                                                    *
     H*                                                               *
     H* 04/20/21 C:079 Support: 133/95580                             *
     H*                Correct Check Schedule logic for EPay          *
     H*                                                               *
     H* 07/20/20 C:079 Project: WC2749                                *
     H*                E-Pay Processing                               *
     H*                                                               *
     H* 06/20/19 C:62  Project: 152MAV                                *
     H*                Add Negative Fee Check Processing.             *
     H*                                                               *
     H* 04/12/16 C:309 Project: C114MXO                               *
     H*                Added condition for client '114' to handle     *
     H*                negative check requests.                       *
     H*                                                               *
     H* 05/06/15 C:136 Project: WC2365                                *
     H*                Add EFT processing to group checks. This pgm   *
     H*                will create separate Group Check records for   *
     H*                EFT and hard copy.                             *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H* 10/22/09 C:191 Support: 69926 WA139                           *
     H*           LF WARPL5 select only non-group checks - Remove it  *
     H*           Use SQL to reset check request in WARPP             *
     H*                                                               *
     H* 04/01/08 C:191 Project: 139MIV                                *
     H*                For client#139: Change the group check process *
     H*                to handle the negative check requests.         *
     H*                                                               *
     H* 05/10/01 C:15 Project: WC1020                                 *
     H*               Added code to load the Group checks by Bank Code*
     H*               (CKTP) for the multi bank acct processing.      *
     H*****************************************************************
     F*** Begin dlt *** 05/10/01 **************************************
     F*******WAC3L   IF  E           K        DISK
     F*******WARPL4  IF  E           K        DISK
     F*******WAR3L7  UF  E           K        DISK                      A
     F*** End dlt ***** 05/10/01 **************************************
     F*** Begin add *** 05/10/01 **************************************
     FWAC3L4    IF   E           K DISK
     FWARPLS    IF   E           K DISK
     FWAR3L4    UF A E           K DISK
     F*** End add ***** 05/10/01 **************************************
     F                                     RENAME(PRP01:PRP02)

     D*
      //*** Begin Add *** 06/20/19 ****************************************
      /copy sPrModule

     D allowNegFees    s               n
      //*** End   Add *** 06/20/19 ****************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     F*** BEG ADD ****** 04/01/08 ************************************
     F*** BEGIN DEL ****** 10/22/09 ************************************
     F**********WARPL5  UF  E           K        DISK
     F************PRP01                             KRENAMEPRP05
     F*** END   DEL ****** 10/22/09 ************************************
     F*** END ADD ****** 04/01/08 ************************************

     IPRP02
     I              RPAMT                       R2AMT
     C****************************************************************
     C* PROGRAM ENTER PARAMETERS
     C****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    QQDATE            8
     C                   PARM                    QQCO#             3
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
     C                   MOVE      QQDATE        XXDATE            8 0
     C                   MOVE      QQCO#         XXCO#
     C****************************************************************
     C* PROGRAM FILE KEY LISTINGS
     C****************************************************************
     C*
     C*  REQUEST PAYMENT KEY LIST
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C*** Begin add *** 05/10/01 **************************************
     C                   KFLD                    CPCKTP
     C*** End add ***** 05/10/01 **************************************
     C                   KFLD                    CPFND
     C*
     C*  REQUEST PAYMENT GROUP FILE
     C*
     C     RPKY2         KLIST
     C                   KFLD                    XXCO#
     C*** Begin add *** 05/10/01 **************************************
     C                   KFLD                    CPCKTP
     C*** End add ***** 05/10/01 **************************************
     C                   KFLD                    RPFND
     C                   KFLD                    RPFEIN
     C                   KFLD                    RPMOD
     C*** BEG ADD ***** 05/06/15 *************************************
     C                   KFLD                    RPEFTC
     C*** END ADD ***** 05/06/15 *************************************
      *
     C*** BEG ADD ****** 04/01/08 ************************************
     C     R3KY          KLIST
     C                   KFLD                    XXCO#
     C*
     C     RPKY3         KLIST
     C                   KFLD                    XXCO#
     C*** BEGIN ADD ****** 10/22/09 ************************************
     C                   KFLD                    RPCKTP
     C*** END   ADD ****** 10/22/09 ************************************
     C                   KFLD                    RPFND
     C                   KFLD                    RPFEIN
     C                   KFLD                    RPMOD
     C*** END ADD ****** 04/01/08 ************************************
     C*
     C*  CHECK PROCESSING FILE
     C*
     C     CPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C****************************************************************
     C* *LIKE DEFINES AND PROGRAM VARIABLES INITIALIZATION
     C****************************************************************
     C*
     C     *LIKE         DEFINE    RPCO#         XXCO#
     C     *LIKE         DEFINE    RPTRDT        XXDT
     C     *LIKE         DEFINE    RPAMT         XXAMT
     C****************************************************************
     C* PROGRAM MAINLINE
     C****************************************************************
     C*
     C*** BEG ADD ****** 04/01/08 ************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*** END ADD ****** 04/01/08 ************************************
     C*** Begin Chg *** 06/20/19 ****************************************
      /free
           // Check to see if negative fees are allowed
           allowNegFees = *Off;
           If IsModEnabled('Negative Fee Checks');
              allowNegFees = *On;
           EndIf;
      /end-free
     C*** End   Chg *** 06/20/19 ****************************************
     C*  START READING THE CHECK PROCESSING FILE, WE MUST HAVE
     C*  A RECORD FOR THIS DATE
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C     CPKY          CHAIN     PCP01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  THE CHECK PROCESSING FLAGS MUST BE BLANK
     C*
     C     CPSTA1        IFEQ      ' '
     C     CPSTA0        ANDEQ     ' '
     C*
     C*  START READING ALL PAYMENT RECORDS WHICH HAVE NOT BEEN UPDATED
     C*
      *** beg chg ***** 05/06/15 ************************
     C*****RPKY          CHAIN     PRP01                              60
     C******IN60         DOWEQ     '0'
      /free
       Setll rpky prp01;
       Reade rpky prp01;
       Dow not(%eof);
      /end-free
      *** end chg ***** 05/06/15 ************************
     C     RPTRDT        IFLE      XXDT
     C*
     C*  MOVE AMOUNT TO HOLD FIELDS TO BE ADDED TO PROVIDER TOTAL
     C*
     C                   Z-ADD     RPAMT         XXAMT
      *** beg add ***** 05/06/15 ************************
      /free
       // Skip EFT records if check schedule is flagged for checks
       //  only or skipt checks if flagged for EFT only.
       //*** Begin Chg *** 07/20/20 ****************************************
       //*** If CPeftc = RPeftc or CPeftc = 'B';
       //*** Begin Chg *** 04/20/21 ****************************************
       //***If CPeftc = RPeftc or CPeftc = 'B' or CPeftc = 'V' or CPeftc = 'A';
       If (CPeftc = RPeftc) Or
          (CPeftc = 'B' And (RPeftc = ' ' Or RPeftc = 'E')) Or
          (CPeftc = 'A');
       //*** End   Chg *** 04/20/21 ****************************************
       //*** End   Chg *** 07/20/20 ****************************************
       Else;
          Reade rpky prp01;
          Iter;
       Endif;
      /end-free
      *** end add ***** 05/06/15 ************************
     C*
     C*  WRITE/UPDATE THE GROUP BATCH FILE
     C*
     C     RPKY2         CHAIN     PRP02                              61
     C                   MOVE      *BLANK        RPPGTP
     C     *IN61         IFEQ      '0'
     C                   ADD       XXAMT         R2AMT
     C                   UPDATE    PRP02
     C                   ELSE
     C                   Z-ADD     XXAMT         R2AMT
     C                   WRITE     PRP02
     C                   END
     C*
     C                   END
     C     RPKY          READE     PRP01                                  60
     C                   END
     C*
     C                   END
     C*
     C     CPKY          READE     PCP01                                  60
     C                   END
     C*
     C*** BEG ADD ****** 04/01/08 ************************************
     C* Read through all group payment records in WAR3P.  If amount is less than
     C* or equal to zero, delete the record and load 0 into RPBUL1 for all
     C* associated WARPP records (match by CO#, CKTP, FND, FEIN, and MOD).
     C*** Begin Chg *** 06/20/19 ****************************************
     C*****XXCLT#        IFEQ      '139'
     C     allowNegFees  IFEQ      *On
     C*** End   Chg *** 06/20/19 ****************************************
     C***** Beg Add **** 04/12/16 **************************************
     C     XXCLT#        OREQ      '114'
     C***** End Add **** 04/12/16 **************************************
     C     R3KY          CHAIN     PRP02                              60
     C     *IN60         DOWEQ     '0'
     C     RPTRDT        IFLE      XXDT
     C     R2AMT         ANDLE     0
     C*
     C*** BEGIN DEL ****** 10/22/09 ************************************
     C***********RPKY3     CHAINPRP05                61
     C************IN61     DOWEQ'0'
     C**********           Z-ADD0         RPBUL1
     C**********           UPDATPRP05
     C********** RPKY3     READEPRP05                    61
     C**********           ENDDO                           *IN61='0'
     C*** END   DEL ****** 10/22/09 ************************************
     C*
     C*** BEGIN ADD ****** 10/22/09 ************************************
     C/EXEC SQL
     C+ UPDATE WARPP SET RPBUL1 = 0
     C+   WHERE
     C+   RPCO#  = :RPCO# AND
     C+   RPCKTP = :RPCKTP AND
     C+   RPFND  = :RPFND AND
     C+   RPFEIN  = :RPFEIN AND
     C+   RPMOD = :RPMOD AND
     C+   RPSTAT = ' ' AND
     C+   RPPGTP = 'G'
     C/END-EXEC
     C*** END   ADD ****** 10/22/09 ************************************
     C*
     C                   DELETE    PRP02
     C*
     C                   ENDIF                                                  RPTRDT LE XXDT
     C     R3KY          READE     PRP02                                  60
     C                   ENDDO                                                  *IN60='0'
     C                   ENDIF                                                  XXCLT# EQ '139'
     C*** END ADD ****** 04/01/08 ************************************
     C*
     C*  END PGM
     C*
     C                   MOVE      '1'           *INLR
