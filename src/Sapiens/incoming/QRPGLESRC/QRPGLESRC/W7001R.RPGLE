     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE W7001R - INSERT CHECK NUMBERS, BATCH NUMBERS
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 04/19/21 C:062 Project: WC2749(95409)                         *
     H*                Extend key to include EFTC.                    *
     H*                                                               *
     H* 07/20/20 C:079 Project: WC2749                                *
     H*                E-Pay Processing                               *
     H*                                                               *
     H* 05/23/19 C:079 Support: 129/75145                             *
     H*                For client 129 set sort order then assign chk# *
     H*                                                               *
     H* 05/06/15 C:062 Project: 114MRH                                *
     H*                Assign EFT Check #                             *
     H*                                                               *
     H* 04/04/14 C:279 Project: 129WSC  Priss:30                      *
     H*                Correct Check Print sort order                 *
     H*                                                               *
     H* 05/31/12 C:191 Project: WC2044                                *
     H*                Do not assign check# for EFT transactions      *
     H*                                                               *
     H* 05/13/10 C:61  PROJECT: 136WFD                                *
     H*                Update Maintenance fields.                     *
     H*                                                               *
     H* 09/19/08 C:197 PROJECT: 71WJE                                 *
     H*                CHANGE PRINTER ORDER OF CLAIM CHECKS           *
     H*                                                               *
     H* 11/19/01 C:15 Project: WC1020                                 *
     H*               Added code to assign the check#s by ACCT/FND/   *
     H*               CKTP.                                           *
     H*                                                               *
     H* 05/01/01 C:15 Project: WC1020                                 *
     H*               Added code to assign the check#s by Bank Code   *
     H*               (CKTP) for the multi bank acct processing.      *
     H*                                                               *
     H* 07/14/00 C:15 Support: WC101/23523                            *
     H*          Added code to update all appropriate WACPP records   *
     H*          with the next available check#.                      *
     H*                                                               *
     H* 03/12/99 C:69 Project: 48WLL                                  *
     H*          Changed program to no longer blank out the RPCKTP    *
     H*          field when updating WARPP file                       *
     H*                                                               *
     H* 10/22/97 C:30 Project: 60ANN                                  *
     H*          Eliminate WMBIP access.  Use CPACCT.                 *
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
     H*      " B E F O R E "    V E R S I O N    3                   *
     H*                                                               *
     H*  JJJ REVISED CODE ON 9-19-89 TO READ REQUESTS IN ORDER BY     *
     H*  CO# FND FEIN MOD AMT (OMIT RPSTAT NE BLANK)                  *
     H*                                                               *
     H* 10/13/93 C:15 PROJECT WC258                                   *
     H*               ONLY PROCESS REQUESTS WHICH HAVE A TRANSACTION  *
     H*               DATE LESS THAN OR EQUAL TO THE BATCH DATE.      *
     H*                                                               *
     H* 09/08/94 C:15 PROJECT WC361                                   *
     H*               CHANGED CODE TO USE WARPL6 WHICH IGNORES GROUP  *
     H*               TYPE CHECKS.                                    *
     H*                                                               *
     H* 10/16/95 C:30 PROJECT WC458                                   *
     H*               A CHECK SUMMARY FILE WILL BE USED TO TRACK      *
     H*               THE LAST CHECK NUMBER USED.  THIS WILL ALLOW    *
     H*               FOR CHECK NUMBER TRACKING ACROSS SEVERAL        *
     H*               FUNDS SHARING THE SAME ACCOUNT.                 *
     H*               DO NOT UPDATE THE CHECK PROCESSING RECORD       *
     H*               IF 'NO' REQUEST FOR PAYMENTS.                   *
     H*                                                               *
     H*  11/27/95 C:30 Correction WC458 per C:15                      *
     H*                FIXED THE CHECK TRACKING PROCESS.  IF A CHECK  *
     H*                WASN'T PROCESSED FOR A CERTAIN FUND, THE       *
     H*                FOLLOWING FUNDS WOULDN'T GET UPDATED WITH THE  *
     H*                CORRECT STARTING CHECK #.                      *
     H*                                                               *
     H*****************************************************************
     FSMCOL     UF   E           K DISK
     FWARPL10   UF   E           K DISK
      *________________________________________________________________
      *  "Group" Check Processing file
     FWAC3L3    UF   E           K DISK
     F                                     RENAME(PCP01:PCP02)
      *________________________________________________________________
      *  Check Processing file
     FWACPL4    UF   E           K DISK
      *________________________________________________________________
     FWXWCL1    UF A E           K DISK
     FWSCKL     UF   E           K DISK
      *________________________________________________________________
      *________________________________________________________________

     D DES             S             30    DIM(1) CTDATA PERRCD(1)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     d***** Standalone fields **********************************
     d ClientID        s              3
     d***** Bottom Standalone fields ***************************

      /copy wPrBank
     d*** Beg Add *** 07/20/20 *********************************
      /copy wPrEPay
     d*** End Add *** 07/20/20 *********************************
     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3
     d*
     d* Set the check printing sort order
     d CheckSortOrder  pr                  extpgm('WA023R2')
     d   inDate                       8
     d   inTyp                        1
     d   inDate        s              8
     d   inTyp         s              1

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
     C     RPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CPCKTP
     C                   KFLD                    CPFND
     C*
     C*  CHECK PROCESSING FILE
     C*
     C     CPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C*
     C     CPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C                   KFLD                    XXACCT
     C                   KFLD                    XXFND
     C                   KFLD                    XXCKTP
     C*
     C     CPKY2         KLIST
     C                   KFLD                    XXCO#
     C*
     C*** Begin Add *** 04/19/21 *******************************************
     C     CPKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C                   KFLD                    XXACCT
     C                   KFLD                    XXFND
     C                   KFLD                    XXCKTP
     C                   KFLD                    XXEFTC
     C*** End   Add *** 04/19/21 *******************************************
     C*
     C*  WASTE CHECK KEY LIST
     C*
     C     WCKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CPFND
     C                   KFLD                    WCCHK#
     C*
     C*  COMPANY MASTER FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*
     C*  FUND AUXILIARY FILE - BY FUND
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CPFND
     C*
     C*  FUND AUXILIARY FILE - BY ACCT
     C*
     C     F2KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKACCT
     C                   KFLD                    CPFND
     C*
     C     F2KY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKACCT
     C*
     C*  CHECK SUMMARY FILE
     C*
     C     CKKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXACCT
      *
      *  Bank Info file
      *
     C     BIKY1         KLIST
     C                   KFLD                    CPCO#
     C                   KFLD                    XXAPPL            2
     C                   KFLD                    CPFND
     C                   KFLD                    XXDT
      *
     C     BIKY2         KLIST
     C                   KFLD                    CPCO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    CPFND
     C*
     C*
     C*  *LIKE DEFINE SECTION
     C*
     C     *LIKE         DEFINE    CPFND         XXFND
     C     *LIKE         DEFINE    CPFND         SVFND
     C     *LIKE         DEFINE    CPACCT        KKACCT
     C     *LIKE         DEFINE    CPACCT        XXACCT
     C     *LIKE         DEFINE    CPACCT        SVACCT
     C     *LIKE         DEFINE    RPTRDT        XXDT
     C     *LIKE         DEFINE    CPCKTP        SVCKTP
     C     *LIKE         DEFINE    CPCKTP        XXCKTP
     C*** Begin Add *** 04/19/21 *******************************************
     C     *LIKE         DEFINE    CPEFTC        SVEFTC
     C     *LIKE         DEFINE    CPEFTC        XXEFTC
     C*** End   Add *** 04/19/21 *******************************************
     C*
      /Free
                         GetClientID(ClientId);

                         If ClientID = '129';
                            inDate = qqDate;
                            inTyp  = qqTyp;
                            CallP CheckSortOrder(inDate:inTyp);
                         Endif;
      /End-free
     C*  GET THE COMPANY MASTER RECORD, GET THE BUL NUMBER
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     COBUL1        XXBUL1            9 0
     C*
     C*  START READING THE CHECK PROCESSING FILE, WE MUST HAVE
     C*  A RECORD FOR THIS DATE
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C     CPKEY         CHAIN(N)  PCP01                              60
     C*
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
     C*** Beg Add *** **** 07/20/20 *********************************
     C                   Z-ADD     CPEPAY#       XXEPAY#           7 0
     C*** End Add *** **** 07/20/20 *********************************
     C                   END
     C                   Z-ADD     XXCHK#        XXLCHK            7 0
      *
     C*
     C*  CHECK TO SEE IF WE ARE IN RESTART, IF YES THEN WAIT UNTIL
     C*  WE GET TO THE RECORD FOR RESTART, AND START UPDATING
     C*
     C*  START READING ALL PAYMENT RECORDS WHICH HAVE NOT BEEN UPDATED
     C*
     C     RPKEY         CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C*** Beg Add *** **** 07/20/20 ************************************
       //*** Begin Chg *** 04/19/21 ************************************
       //***If RPtrDt <= xxDt and RPeftC = CPeftC or
       //*** RPtrDt <= xxDt and  CPeftC = 'B' or
       //*** RPtrDt <= xxDt and CPeftC = 'V' or
       //*** RPtrDt <= xxDt and CPeftC = 'A';
       If (RPtrDt <= xxDt and RPeftC = CPeftC) or
          (RPtrDt <= xxDt and  CPeftC = 'B' And
               (RPeftc = ' ' Or RPeftc = 'E')) or
          (RPtrDt <= xxDt and CPeftC = 'A');
       //*** End   Chg *** 04/19/21 ************************************
     C*** End Add *** **** 07/20/20 ************************************
     C*
     C     CPRSTR        IFNE      *ZERO
     C*
     C     RPCHK#        IFGE      CPCHK#
     C*
     C                   MOVE      *ON           *IN55
     C                   Z-ADD     XXCHK#        RPCHK#
     C     XXCHK#        ADD       1             XXCHK#
     C                   MOVE      Q1USER        RPUSER
     C                   MOVE      Q1PGM         RPPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C*
     C                   MOVE      DSTIME        RPTIME
     C                   UPDATE    PRP01
     C                   Z-ADD     RPCHK#        XXLCHK
     C*
     C                   END
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN55
     C     XXBUL1        ADD       1             XXBUL1
     C                   Z-ADD     XXBUL1        RPBUL1
     C                   MOVEL     CPACCT        RPACCT                         Bank Account #
     C                   MOVEL     CPCKTP        RPCKTP                         Bank Code
     C*** Beg Chg *** **** 07/20/20 ************************************
     C****************   IF        RPEFTC <> 'E'
       If RPeftC <> 'E' and RPeftC <> 'V';
     C*** End Chg *** **** 07/20/20 ************************************
     C                   EVAL      RPCHK# = XXCHK#
     C                   EVAL      XXLCHK = RPCHK#
     C     XXCHK#        ADD       1             XXCHK#
     C                   Else
       //*** Beg Add *** **** 07/20/20 *********************************
      /Free
                         If RPeftC = 'E';
      /End-Free
       //*** End Add *** **** 07/20/20 *********************************
     C                   Eval      RPchk# = GetNextEft#(CPco#:CPacct)
       //*** Beg Add *** **** 07/20/20 *********************************
      /Free
                         Endif;
      /End-Free
       //*** End Add *** **** 07/20/20 *********************************
       //*** Beg Add *** **** 07/20/20 *********************************
                         If RPeftC = 'V';
                            RPchk#  = GetNextEPay#(CPco#:CPacct);
                            xxEpay# = RPchk#;
                         Endif;
       //*** End Add *** **** 07/20/20 *********************************
     C                   ENDIF                                                  RPEFTC <> 'E'
     C                   MOVE      Q1USER        RPUSER
     C                   MOVE      Q1PGM         RPPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C*
     C                   MOVE      DSTIME        RPTIME
     C
     C                   UPDATE    PRP01
     C*
     C                   END
     C*
     C                   END
     C     RPKEY         READE     PRP01                                  60
     C                   END
      *
     C*        LOAD THE CHECK PRINTING SORT FIELD
     C*
      /Free
                         If ClientID <> '129';
      /End-free
     C                   CALL      'WA023R2'
     C                   PARM                    QQDATE
     C                   PARM      ' '           QQTYP             1
      /Free
                         Endif;
      /End-free
     C*
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
     C     WCKEY         CHAIN     PWC01                              60
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
      *
      *  If a Check # was assigned, adjust the "Next" record using this
      *  Bank Account #.
      *
     C     XXCHK#        IFGT      CPCHK#
     C*** Begin Add *** 07/20/20 *******************************************
     C     XXEPAY#       ORGT      CPEPAY#
     C*** End   Add *** 07/20/20 *******************************************
      *
     C                   Z-ADD     CPFND         SVFND
     C                   MOVEL     CPACCT        SVACCT
     C                   Z-ADD     CPFND         XXFND
     C                   MOVEL     CPACCT        XXACCT
     C                   MOVE(P)   CPCKTP        SVCKTP
     C                   MOVE(P)   CPCKTP        XXCKTP
     C*** Begin Add *** 04/19/21 *******************************************
     C                   MOVE(P)   CPEFTC        SVEFTC
     C                   MOVE(P)   CPEFTC        XXEFTC
     C*** End   Add *** 04/19/21 *******************************************
      *
     C     CKKY          CHAIN     PCK01                              60
     C     *IN60         IFEQ      *OFF
     C*** Begin Add *** 07/20/20 *******************************************
     C     XXCHK#        IFGT      CPCHK#
     C*** End   Add *** 07/20/20 *******************************************
     C                   Z-ADD     XXLCHK        CKLCHK
     C   55              ADD       1             CKLCHK                         ASSGN NXT CHECK #
     C*** Begin Add *** 07/20/20 *******************************************
     C                   EndIf
     C*** End   Add *** 07/20/20 *******************************************
     C                   MOVE      Q1USER        CKUSER
     C                   MOVE      Q1PGM         CKPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CKDT
     C*
     C                   MOVE      DSTIME        CKTIME
     C                   UPDATE    PCK01
     C                   ENDIF
     C*
     C*  UPDATE THE STARTING CHECK NUMBER IN THE REQUEST RECORD
     C*  FOR THE NEXT FUND SHARING THIS ACCOUNT.
     C*
     C     CPKY1         SETGT     PCP01                              60
     C     CPKY2         READE     PCP01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C     CPACCT        IFEQ      XXACCT
     C     CPSTA1        ANDEQ     ' '
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
     C                   ENDIF
     C*
     C     CPKY2         READE     PCP01                                  60
     C                   ENDDO
      *
      *  Access "Group" Check Processing for this Bank Account # and
      *  adjust Starting Check #.
      *
     C                   EXSR      GROUP
      *
      *  Read Original Check Processing record
      *
     C                   MOVE      SVFND         XXFND
     C                   MOVE      SVACCT        XXACCT
     C                   MOVE(P)   SVCKTP        XXCKTP
     C*** Begin Add *** 04/19/21 *******************************************
     C                   MOVE(P)   SVEFTC        XXEFTC
     C*** End   Add *** 04/19/21 *******************************************
      *
     C*** Begin Chg *** 04/19/21 *******************************************
     C*****CPKY1         SETGT     PCP01
     C     CPKY3         SETGT     PCP01
     C*** End   Chg *** 04/19/21 *******************************************
      *
     C*
     C                   ENDIF                                                  XXLCHK
     C*
     C                   END
     C*
     C     CPKEY         READE(N)  PCP01                                  60
     C                   END
     C*
     C*  UPDATE THE COMPANY MASTER
     C*
     C                   Z-ADD     XXBUL1        COBUL1
     C                   UPDATE    PCO01
     C                   END
     C*
     C*
     C                   MOVE      '1'           *INLR
      *
      **************************************************************************
      *  GROUP - sub to access All "Group" Check Processing recs for ACCT
      **************************************************************************
     C     GROUP         BEGSR
      *
      *  Process ALL WAC3P records for Bank Acct #
      *
     C     CKKY          CHAIN     PCP02                              60
      *
     C     *IN60         DOWEQ     *OFF
      *                    update "starting" check number
     C                   Z-ADD     CKLCHK        CPCHK#
     C*** Begin Add *** 07/20/20 **************************************
     C                   Z-ADD     CKEPAY#       CPEPAY#
     C*** End   Add *** 07/20/20 **************************************
     C                   UPDATE    PCP02
     C     CKKY          READE     PCP02                                  60
     C                   ENDDO
      *
     C                   ENDSR
      *
      **************************************************************************
      *  SRACCT - subroutine to access Fund Aux and Bank Info files for Group.
      **************************************************************************
     C     SRACCT        BEGSR
     C*
      *
      *
     C                   ENDSR
**
WASTE CHECK - VOID
