     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')
     H/define CONVERT
     H/TITLE W7001R3 - INSERT CHECK NUMBERS
     H*****************************************************************
     H* DESCRIPTION:                                                  *
     H*      THIS PROGRAM ASSIGNS CHECK NUMBERS FOR THE GROUP CHECK   *
     H*      PROCESS.                                                 *
     H* CALLED FROM: W7001C                                           *
     H* CALLS TO:    NONE                                             *
     H* 09/08/94  C:15  PROJECT: WC361                                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/19/21 C:062 Project: WC2749(95409)                         *
     H*                Extend key to include EFTC.                    *
     H*                                                               *
     H* 07/20/20 C:079 Project: WC2749                                *
     H*                E-Pay Processing                               *
     H*                                                               *
     H* 05/06/15 C:136 Project: WC2365                                *
     H*                Add EFT processing to Group Checks             *
     H*                                                               *
     H* 04/04/14 C:279 Project: 129WSC  Priss:30                      *
     H*                Correct Check Print sort order                 *
     H*                                                               *
     H* 11/25/13 C:282 Project: 129WSC                                *
/*   H*          Chgd the file to use WAR3L11 instead of WARPLR.      *
     H*                                                               *
     H* 11/19/01 C:15 Project: WC1020                                 *
     H*               Changed key list for WAC3L4.                    *
     H*                                                               *
     H* 05/10/01 C:15 Project: WC1020                                 *
     H*               Added code to assign the check#s by Bank Code   *
     H*               (CKTP) for the multi bank acct processing.      *
     H*                                                               *
     H*  07/14/00 C:15 Support: WC101/23523                           *
     H*           Added code to update all appropriate WACPP records  *
     H*           with the next available check#.                     *
     H*                                                               *
     H*  08/22/97 C:30 Project:  WC659                                *
     H*           Date PARM changed to 8 char.                        *
     H*           Check Processing file processed in CO#/PRDT/ACCT/GRP*
     H*           order.  The Bank Info record in effect for the      *
     H*           "Process Date" is accessed.                         *
     H*           When a given Group has been processed, WACPL2 will  *
     H*           be used to find the next Group record with the same *
     H*           Bank Account.  If another Group is found, the Check *
     H*           Processing record will be updated with the Starting *
     H*           check #.                                            *
     H*                                                               *
     H*       " B E F O R E "    V E R S I O N    3                   *
     H*                                                               *
     H* 11/27/95 C:30 PROJECT WC458                                   *
     H*               Correction WC458 per C:15                       *
     H*               A CHECK SUMMARY FILE WILL BE USED TO TRACK      *
     H*               THE LAST CHECK NUMBER USED.  THIS WILL ALLOW    *
     H*               FOR CHECK NUMBER TRACKING ACROSS SEVERAL        *
     H*               FUNDS SHARING THE SAME ACCOUNT.                 *
     H*               FIXED THE CHECK TRACKING PROCESS.  IF A CHECK   *
     H*               WASN'T PROCESSED FOR A CERTAIN FUND, THE        *
     H*               FOLLOWING FUNDS WOULDN'T GET UPDATED WITH THE   *
     H*               CORRECT STARTING CHECK #.                       *
     H*****************************************************************
     F*** Begin dlt *** 05/10/01 **************************************
     F*****WAR3L7  UF  E           K        DISK
     F*****WAC3L2  UF  E           K        DISK
     F*** End dlt ***** 05/10/01 **************************************
     F*** Begin add *** 05/10/01 **************************************
     F*** Begin add *** 11/25/13 **************************************
     F*****WAR3L4    UF   E           K DISK
     FWAR3L11   UF   E           K DISK
     F*** End add ***** 11/25/13 **************************************
     FWAC3L4    UF   E           K DISK
     F*** End add ***** 05/10/01 **************************************
     FWACPL3    UF   E           K DISK
     F                                     RENAME(PCP01:PCP02)
     F*________________________________________________________________
     FWXWCL1    UF A E           K DISK
     FWSCKL     UF   E           K DISK

     D DES             S             30    DIM(1) CTDATA PERRCD(1)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     d*** Beg Add *** 07/20/20 *********************************
      /copy wPrBank
      /copy wPrEPay
     d*** End Add *** 07/20/20 *********************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

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
     C                   MOVE      QQCO#         XXCO#             3 0
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
     C*  CHECK PROCESSING FILE
     C*
     C     CPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C*
     C     CPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C                   KFLD                    XXACCT
     C*** Begin add *** 05/10/01 **************************************
     C*** Begin dlt *** 11/19/01 **************************************
     C******************** KFLD           XXCKTP
     C*** End dlt ***** 11/19/01 **************************************
     C*** End add ***** 05/10/01 **************************************
     C                   KFLD                    XXFND
     C*** Begin add *** 11/19/01 **************************************
     C                   KFLD                    XXCKTP
     C*** End add ***** 11/19/01 **************************************
     C*
     C     CPKY2         KLIST
     C                   KFLD                    XXCO#
     C*** BEGIN DLT *** 07/14/00 **************************************
     C*******************  KFLD           XXDT
     C*******************  KFLD           XXACCT
     C*** END DLT ***** 07/14/00 **************************************
     C*** Begin Add *** 04/19/21 **************************************
     C     CPKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C                   KFLD                    XXACCT
     C                   KFLD                    XXFND
     C                   KFLD                    XXCKTP
     C                   KFLD                    XXEFTC
     C*** End   Add *** 04/19/21 **************************************
     C*
     C*  CHECK SUMMARY FILE
     C*
     C     CKKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXACCT
     C*
     C*  WASTE CHECK KEY LIST
     C*
     C     WCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CPFND
     C                   KFLD                    WCCHK#
     C*
     C*  *LIKE DEFINE SECTION
     C*
     C     *LIKE         DEFINE    CPFND         XXFND
     C     *LIKE         DEFINE    CPFND         SVFND
     C     *LIKE         DEFINE    CPACCT        KKACCT
     C     *LIKE         DEFINE    CPACCT        XXACCT
     C     *LIKE         DEFINE    CPACCT        SVACCT
     C     *LIKE         DEFINE    RPTRDT        XXDT
     C*** Begin add *** 05/10/01 **************************************
     C     *LIKE         DEFINE    CPCKTP        SVCKTP
     C     *LIKE         DEFINE    CPCKTP        XXCKTP
     C*** End add ***** 05/10/01 **************************************
     C*** Begin Add *** 04/19/21 *******************************************
     C     *LIKE         DEFINE    CPEFTC        SVEFTC
     C     *LIKE         DEFINE    CPEFTC        XXEFTC
     C*** End   Add *** 04/19/21 *******************************************
     C*
     C***** beg add ****** 04/04/14 ***********************************
     C*  Call WA023R2 to load the check printing sort field RPCSRT.
     C*
     C                   Call      'WA023R2'
     C                   Parm                    QQdate
     C                   Parm      'G'           QQtyp             1
     C***** end add ****** 04/04/14 ***********************************
     C*
     C*  START READING THE CHECK PROCESSING FILE, WE MUST HAVE
     C*  A RECORD FOR THIS DATE
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C     CPKY          CHAIN(N)  PCP01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      *OFF          *IN55
     C*
     C*  THE CHECK PROCESSING FLAGS MUST BE BLANK
     C*
     C     CPSTA1        IFEQ      ' '
     C     CPSTA0        ANDEQ     ' '
     C*
     C     CPRSTR        IFNE      *ZERO
     C                   Z-ADD     CPRSTR        XXCHK#            7 0
     C                   ELSE
     C                   Z-ADD     CPCHK#        XXCHK#
     C*** Begin Add *** 07/20/20 **************************************
     C                   Z-ADD     CPEPAY#       XXEPAY#           7 0
     C*** End   Add *** 07/20/20 **************************************
     C                   END
     C                   Z-ADD     XXCHK#        XXLCHK            7 0
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

      *** beg add ***** 05/06/15 ************************
      /free
       // Skip EFT records if check schedule is flagged for checks
       //  only or skipt checks if flagged for EFT only.
       //*** Begin Chg *** 07/20/20 ****************************************
       //*** If CPeftc = RPeftc or CPeftc = 'B';
       //*** Begin Chg *** 04/19/21 ****************************************
       //If CPeftc = RPeftc or CPeftc = 'B' or CPeftc = 'V' or CPeftc = 'A';
       If (CPeftc = RPeftc) Or
          (CPeftc = 'B' And (RPeftc = ' ' Or RPeftc = 'E')) Or
          (CPeftc = 'A');
       //*** End   Chg *** 04/19/21 ****************************************
       //*** End   Chg *** 07/20/20 ****************************************
       Else;
          Reade rpky prp01;
          Iter;
       Endif;
      /end-free
      *** end add ***** 05/06/15 ************************
     C*
     C*  CHECK TO SEE IF WE ARE IN RESTART, IF YES THEN WAIT UNTIL
     C*  WE GET TO THE RECORD FOR RESTART, AND START UPDATING
     C*
      *** beg chg ***** 05/06/15 ************************
      ***********
     C*****CPRSTR        IFNE      *ZERO
     C***********
     C*****RPCHK#        IFGE      CPCHK#
     C***********
     C***********        MOVE      *ON           *IN55
     C****Begin*dlt *** 05/10/01 **************************************
     C*******************  MOVE *BLANK    RPCKTP           Bank Account Code
     C****End*dlt ***** 05/10/01 **************************************
     C***********        MOVEL     CPACCT        RPACCT                         Bank Account #
     C***********        Z-ADD     XXCHK#        RPCHK#
     C***********        ADD       1             XXCHK#
     C***********        UPDATE    PRP01
     C***********        Z-ADD     RPCHK#        XXLCHK
     C***********
     C***********        END
     C***********
     C***********        ELSE
     C***********
     C***********        MOVE      *ON           *IN55
     C****Begin*dlt *** 05/10/01 **************************************
     C*******************  MOVE *BLANK    RPCKTP           Bank Account Code
     C****End*dlt ***** 05/10/01 **************************************
     C***********        MOVEL     CPACCT        RPACCT                         Bank Account #
     C***********        Z-ADD     XXCHK#        RPCHK#
     C***********        ADD       1             XXCHK#
     C***********        UPDATE    PRP01
     C***********        Z-ADD     RPCHK#        XXLCHK
     C***********
     C***********        END
     c                   If        RPeftc = ' '
     C     CPRSTR        IFNE      *ZERO
     C     RPCHK#        IFGE      CPCHK#
     C                   MOVE      *ON           *IN55
     C                   MOVEL     CPACCT        RPACCT                         Bank Account #
     C                   Z-ADD     XXCHK#        RPCHK#
     C                   ADD       1             XXCHK#
     C                   UPDATE    PRP01
     C                   Z-ADD     RPCHK#        XXLCHK
     C                   END
     C                   ELSE
     C                   MOVE      *ON           *IN55
     C                   MOVEL     CPACCT        RPACCT                         Bank Account #
     C                   Z-ADD     XXCHK#        RPCHK#
     C                   ADD       1             XXCHK#
     C                   UPDATE    PRP01
     C                   Z-ADD     RPCHK#        XXLCHK
     C                   END
     c                   Else
     c                   Exsr      UpDateEFTSeq#
     c                   Endif
      *** end chg ***** 05/06/15 ************************
     C*
     C     RPKY          READE     PRP01                                  60
     C                   END
     C*
     C*  WRITE TRANSACTIONS TO THE WASTE CHECK FILE
     C*
     C     CPWAST        IFEQ      'Y'
     C     CPRSTR        IFNE      *ZERO
     C*
     C     CPRSTR        SUB       CPCHK#        X1                3 0
     C                   Z-ADD     CPCHK#        X2                7 0
     C*
     C                   MOVE      XXCO#         WCCO#
     C                   MOVE      CPFND         WCFND
     C                   MOVE      XXDT          WCTRDT
     C                   MOVEL     DES(1)        WCDESC
     C                   MOVE      Q1USER        WCUSER
     C                   MOVE      Q1PGM         WCPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WCDT
     C*
     C                   MOVE      DSTIME        WCTIME
     C                   MOVE      *ZERO         WCBUL1
     C*
     C     1             DO        X1
     C                   Z-ADD     X2            WCCHK#
     C                   ADD       1             X2
     C     WCKY          CHAIN     PWC01                              60
     C     *IN60         IFEQ      '1'
     C                   WRITE     PWC01
     C                   ELSE
     C                   MOVE      XXCO#         WCCO#
     C                   MOVE      CPFND         WCFND
     C                   MOVE      XXDT          WCTRDT
     C                   MOVEL     DES(1)        WCDESC
     C                   MOVE      Q1USER        WCUSER
     C                   MOVE      Q1PGM         WCPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WCDT
     C*
     C                   MOVE      DSTIME        WCTIME
     C                   MOVE      *ZERO         WCBUL1
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C*  If a Check # was assigned, update the Bank Account file with
     C*    the next available check#.
     C*
     C     XXCHK#        IFGT      CPCHK#
     C*** Begin Add *** 07/20/20 **************************************
     C     xxEPay#       ORGT      CPEPAY#
     C*** End   Add *** 07/20/20 **************************************
     C*
     C                   Z-ADD     CPFND         SVFND
     C                   MOVEL     CPACCT        SVACCT
     C                   Z-ADD     CPFND         XXFND
     C                   MOVEL     CPACCT        XXACCT
     C*** Begin add *** 05/10/01 **************************************
     C                   MOVE(P)   CPCKTP        SVCKTP
     C                   MOVE(P)   CPCKTP        XXCKTP
     C*** End add ***** 05/10/01 **************************************
     C*** Begin Add *** 04/19/21 *******************************************
     C                   MOVE(P)   CPEFTC        SVEFTC
     C                   MOVE(P)   CPEFTC        XXEFTC
     C*** End   Add *** 04/19/21 *******************************************
     C*
     C     CKKY          CHAIN     PCK01                              60
     C     *IN60         IFEQ      *OFF
     C*** Begin Add *** 07/20/20 **************************************
     C     XXCHK#        IFGT      CPCHK#
     C*** End   Add *** 07/20/20 **************************************
     C                   Z-ADD     XXLCHK        CKLCHK
     C   55              ADD       1             CKLCHK                         ASSGN NXT CHECK #
     C*** Begin Add *** 07/20/20 **************************************
     C                   EndIf
     C*** End   Add *** 07/20/20 **************************************
     C                   UPDATE    PCK01
     C                   ENDIF
     C*
     C*  Update all subsequent Group Check Processing records with the
     C*  next available check#, if they share the same bank account.
     C*
     C     CPKY1         SETGT     PCP01                              60
     C     CPKY2         READE     PCP01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C*** BEGIN ADD *** 07/14/00 **************************************
     C     CPACCT        IFEQ      XXACCT
     C     CPSTA1        ANDEQ     ' '
     C*** END ADD ***** 07/14/00 **************************************
     C*
     C*** Begin Add *** 07/20/20 ****************************************
     C*  Update the E-Pay Number
     C                   If        CPeftC = 'V' Or CPeftC = 'A'
     C                   Z-ADD     CKePay#       CPepay#
     C                   EndIf
     C*  Update the Check Number
     C                   If        CPeftC = ' ' Or CPeftC = 'A' Or
     C                             CPeftC = 'B'
     C*** End   Add *** 07/20/20 ****************************************
     C                   Z-ADD     CKLCHK        CPCHK#
     C*** Begin Add *** 07/20/20 ****************************************
     C                   Endif
     C*** End   Add *** 07/20/20 ****************************************
     C                   MOVE      *BLANK        CPSTA1
     C                   MOVE      *BLANK        CPSTA0
     C                   MOVE      *ZERO         CPRSTR
     C                   MOVE      *BLANK        CPWAST
     C                   MOVE      Q1USER        CPUSER
     C                   MOVE      Q1PGM         CPPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CPDT
     C*
     C                   MOVE      DSTIME        CPTIME
     C                   UPDATE    PCP01
     C*
     C*** BEGIN ADD *** 07/14/00 **************************************
     C                   ENDIF
     C*** END ADD ***** 07/14/00 **************************************
     C*
     C     CPKY2         READE     PCP01                                  60
     C                   ENDDO
     C*
     C*  Access "Daily" Check Processing for this Bank Account # and
     C*  adjust Starting Check #.
     C*
     C                   EXSR      DAILY
     C*
     C*  Read Original Check Processing record
     C*
     C                   MOVE      SVFND         XXFND
     C                   MOVE      SVACCT        XXACCT
     C*** Begin add *** 05/10/01 **************************************
     C                   MOVE(P)   SVCKTP        XXCKTP
     C*** End add ***** 05/10/01 **************************************
     C*** Begin Add *** 04/19/21 *******************************************
     C                   MOVE(P)   SVEFTC        XXEFTC
     C*** End   Add *** 04/19/21 *******************************************
     C*** Begin Chg *** 04/19/21 *******************************************
     C*****CPKY1         SETGT     PCP01
     C     CPKY3         SETGT     PCP01
     C*** End   Chg *** 04/19/21 *******************************************
     C*
     C                   ENDIF                                                  XXCHK#
     C*
     C                   END
     C*
     C     CPKY          READE(N)  PCP01                                  60
     C                   END
     C*
     C*   END PGM
     C*
     C                   MOVE      '1'           *INLR
     C*
     C**************************************************************************
     C*  DAILY - sub to access All "Daily" Check Processing recs for ACCT
     C**************************************************************************
     C     DAILY         BEGSR
     C*
     C*  Process ALL WACPP records for Bank Acct #
     C*
     C     CKKY          CHAIN     PCP02                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     CKLCHK        CPCHK#
     C*** Begin Add *** 07/20/20 **************************************
     C                   Z-ADD     CKEPAY#       CPEPAY#
     C*** End   Add *** 07/20/20 **************************************
     C                   UPDATE    PCP02
     C     CKKY          READE     PCP02                                  60
     C                   ENDDO
     C*
     C                   ENDSR
      *=====================================================================
      * Update EFT Seq# - Updates EFT Seq# in Check Request and Bank Master
      *=====================================================================
      /free
       Begsr UpDateEFTSeq#;
       //*** Begin Add *** 07/20/20 ***********************************
       Select;
       When RPeftC = 'V';
          RPacct  = CPacct;
          RPchk#  = GetNextEPay#(CPco#:CPacct);
          xxEPay# = RPchk#;
          Update prp01;
       When RPeftC = 'E';
       //*** End   Add *** 07/20/20 ***********************************
          Chain (CPco#:CPacct) pck01;
          If not(%found);
             LeaveSr;
          Endif;
          RPacct = CPacct;
          RPchk# = CKeft#;
          Update prp01;
          CKeft# += 1;
          Update pck01;
       //*** Begin Add *** 07/20/20 ***********************************
       EndSl;
       //*** End   Add *** 07/20/20 ***********************************
       Endsr;
      /end-free
**
WASTE CHECK - VOID
