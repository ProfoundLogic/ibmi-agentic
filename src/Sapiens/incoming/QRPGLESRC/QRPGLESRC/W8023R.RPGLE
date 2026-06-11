     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W8023R - LOAD DEDUCTIBLE PAYMENT SUMMARY RECORDS
     H*****************************************************************
     H*                                                               *
     H*   PROGRAM DESCRIPTION:                                        *
     H*     THIS PROGRAM READS ALL FEE PAYMENTS, AND WRITES           *
     H*     A DEDUCTIBLE SUMMARY RECORD IF THE PAYMENT IS FOR A       *
     H*     DEDUCTIBLE AND THE EMPLOYER IS USING A NET REPORTING      *
     H*     PLAN.                                                     *
     H*                                                               *
     H*   CALLED FROM:                                                *
     H*     W8023C - LOAD DEDUCTIBLE PAYMENT SUMMARY - OVERRIDES      *
     H*                                                               *
     H*   CALLS TO:                                                   *
     H*     NONE                                                      *
     H*                                                               *
     H*   PROGRAMMER: C:24                                            *
     H*   CREATED:    03/01/95    PROJECT:WC311                       *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/09/15 C:136 Project: WC2393                                *
     H*                Exclude checks with negative amounts from      *
     H*                writing recovery records. They will process    *
     H*                as reversals in W8025R with 'Z' mode           *
     H*                                                               *
     H* 07/29/15 C:136 Support: 35573                                 *
     H*                Use maintenance date instead of transaction    *
     H*                date to pull A/R records with.                 *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H* 02/26/08 C:15 Project: WC311  WA065/62371                     *
     H*               Deleted code added on 5/09/02.                  *
     H*                                                               *
     H*  04/10/06 C:65 PROJECT SUPP#51293                             *
     H*                CHANGE DEDUCTIBLE DISCOUNT TO PULL BASED ON    *
     H*                ANNIV RATING DATE INSTEAD OF POLICY EFF DATE.  *
     H*                                                               *
     H* 04/07/05      PROJECT: 126WDK                                 *
     H*          C:33 PROJECT: 126WCD                                 *
     H*               Multiple Deductible discounts per Year.         *
     H*                                                               *
     H* 03/14/05 C:15 Support: WC091/45940                            *
     H*               Added code to initialize/load DQSTAT.           *
     H*                                                               *
     H* 05/14/03 C:15 Project: WC1080                                 *
     H*               Added code to ignore deduct tax payments.       *
     H*                                                               *
     H* 05/09/02 C:15 Project: WC311                                  *
     H*               Adding code to apply negative amounts to any    *
     H*               WADQP record for the employer to help prevent   *
     H*               negative amounts in WADQP.                      *
     H*                                                               *
     H* 02/02/99 C:15 Support: WC065/15291                            *
     H*               Adding a condition to determine whether the     *
     H*               deduct pmt has been posted to WADQP.            *
     H*                                                               *
     H* 04/09/98 C:65 PROJECT: WC812                                  *
     H*               ADD AMOUNT REMAINING TO WADQP.                  *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*   60  -  GENERAL WORK INDICATOR                               *
     H*   L1  -  LEVEL BREAK ON EMPLOYER                              *
     H*   L2  -  LEVEL BREAK ON FUND DETAIL                           *
     H*****************************************************************
     FWMEAL     IPE  E           K DISK
     FWADQL     UF A E           K DISK
     FWDELL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     FWDF1L     IF   E           K DISK
     FWTDDL2    IF   E           K DISK
     FWMRSL     IF   E           K DISK
      *** beg add ***** 12/09/15 ******************
      * co/fnd/emp/div/fyr/podt
     fwdddl8    uf   e           k disk    rename(pdd01:pdd08)
      * co/fnd/fyr/case/trdt/seq#
     fwdpal5    if   e           k disk    rename(ppa01:ppa05)
      * co/fnd
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)

     dAdjAmtToRcvr     pr                  like(EAamt)
     dSpreadRvrsal     pr                  like(EAamt)

     d ReverseRcvry    pr                  ExtPgm('W8025R')
     d ##co                                like(##co)     const
     d ##fnd                               like(##fnd)    const
     d ##fyrc                              like(##fyrc)   const
     d ##casec                             like(##casec)  const
     d ##seq#                              like(##seq#)   const
     d ##amt#                              like(##amt#)   const
     d ##rtcd                              like(##rtcd)

     d ##co            s              3
     d ##fnd           s              3
     d ##fyrc          s              3
     d ##casec         s              7
     d ##seq#          s              5
     d ##amt#          s              9
     d ##rtcd          s              1
     d psds          esds                  extname(SZQ1P)
     d xxAmtToRcvr     s                   like(EAamt)
      /copy sprdate
      *** end add ***** 12/09/15 ******************

     D                 DS
     D  EADTTM                 1     13  0
      *** BEG CHG ***** 07/29/15 ******************
     D**EATRDT***              1      7  0
     D  EADT                   1      7  0
      *** END CHG ***** 07/29/15 ******************
     D  EATIME                 8     13  0
     D                 DS
     D  DQDTTM                 1     13  0
     D  DQDT                   1      7  0
     D  DQTIME                 8     13  0

     C*
     C*   KEY LIST FOR THE FUND DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C*
     C*   KEY LIST FOR THE RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    XXCD              3
     C*
     C*   KEY LIST FOR THE TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    XXKEY            10
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C                   KFLD                    EAFYR
     C*
     C*   KEY LIST FOR THE DEDUCTIBLE DISCOUNT CODE FILE
     C*
     C     DDKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    XXST              2
     C***  BEGIN CHG  ***  04/07/05 *************************************
     C**********           KFLD           ELDCDE
     C**********           KFLD           ELPFDT
     C                   KFLD                    XXDDCD
     C                   KFLD                    XXDDEF
     C***  END   CHG  ***  04/07/05 *************************************
     C*
     C     DDKY2         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    XXST
     C***  BEGIN CHG  ***  04/07/05 *************************************
     C**********           KFLD           ELDCDE
     C                   KFLD                    XXDDCD
     C***  END   CHG  ***  04/07/05 *************************************
     C*
     C*  KEY LIST FOR THE DEDUCTIBLE PAYMENT RECOVERIES PENDING FILE
     C*
     C     DQKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C                   KFLD                    EAFYR
     C                   KFLD                    EACHK#
     C*
     C*** Begin add *** 05/09/02 **************************************
     C     DQKY1         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C*** End add ***** 05/09/02 **************************************
     C*
     C*  DEFINE LEVEL BREAK FIELDS
     C*
     C     *LIKE         DEFINE    EACO#         SVCO#
     C     *LIKE         DEFINE    EAFND         SVFND
     C     *LIKE         DEFINE    EAFYR         SVFYR
     C     *LIKE         DEFINE    EAEMP#        SVEMP#
     C     *LIKE         DEFINE    EADIV         SVDIV
     C*
     C*  GET THE FUND DETAIL RECORD FOR THE DEDUCTIBLE PAYMENT CODE
     C*
     C     EACO#         IFNE      SVCO#
     C     EAFND         ORNE      SVFND
     C     EAFYR         ORNE      SVFYR
     C*
     C     F1KY          CHAIN     PF101                              60
     C                   MOVE      *ON           XXELFL            1
     C                   MOVE      EACO#         SVCO#
     C                   MOVE      EAFND         SVFND
     C                   MOVE      EAFYR         SVFYR
     C                   MOVE      EAEMP#        SVEMP#
     C                   MOVE      EADIV         SVDIV
     C*
     C                   ENDIF
     C*
     C*  SET THE EMPLOYER CHANGE FLAG
     C*
     C     EAEMP#        IFNE      SVEMP#
     C     EADIV         ORNE      SVDIV
     C*
     C                   MOVE      *ON           XXELFL            1
     C                   MOVE      EAEMP#        SVEMP#
     C                   MOVE      EADIV         SVDIV
     C*
     C                   ENDIF
     C*
     C*  IF THE EMPLOYER IS NET REPORTING (DDTYPE = N) AND THE
     C*  A/R CODE IS FOR A DEDUCTIBLE PAYMENT (EAARCD = F1RDPY),
     C*  WRITE OR UPDATE THE DEDUCTIBLE PAYMENT SUMMARY RECORD.
     C*
     C*
     C*** Begin add *** 05/14/03 **************************************
     C     EAARCD        IFNE      F1DTXP
     C*** End add ***** 05/14/03 ***************************************
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFEQ      'R'
     C     EAARTY        ANDEQ     'Q'
     C*
     C     XXELFL        IFEQ      *ON
     C*
     C*  GET THE EMPLOYER DETAIL RECORD AND DEDUCTIBLE DISCOUNT CODE
     C*  RECORD.  IF EITHER CHAIN FAILS, SET DDTYPE TO *BLANK.
     C*
     C     ELKY          CHAIN     PEL01                              60
     C  N60              MOVE      ELRTCD        XXCD
     C  N60RSKY          CHAIN     PRS01                              60
     C  N60              MOVE      RSJRST        XXST
     C*
     C***  BEGIN ADD  ***  04/07/05 *************************************
     C     *LIKE         DEFINE    ELDCDE        XXDDCD
     C     *LIKE         DEFINE    ELDCEF        XXDDEF
     C*
     C                   MOVE      ELDCDE        XXDDCD
     C                   MOVE      ELDCEF        XXDDEF
     C*** BEGIN CHG ***  04/10/06  ************************************
     C                   MOVE      ELDCEF        @@DATE
     C                   MOVE      ELCO#         @@CO#
     C                   MOVE      ELFND         @@FND
     C                   MOVE      ELFYR         @@FYR
     C                   MOVE      ELEMP#        @@EMP#
     C                   MOVE      ELDIV         @@DIV
     C*
     C                   CALL      'SREFFD'
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@EMP#            9
     C                   PARM                    @@DIV             5
     C                   PARM                    @@DATE            7
     C*
     C                   MOVE      @@DATE        XXDDEF
     C*** END CHG *****  04/10/06  ************************************
     C*
     C     *IN60         IFEQ      *OFF
     C***  END   ADD  ***  04/07/05 *************************************
     C*
     C  N60DDKY          SETGT     PDD01
     C  N60DDKY2         READPE    PDD01                                  60
     C   60              MOVE      *BLANKS       DDTYPE
     C*
     C***  BEGIN ADD  ***  04/07/05 *************************************
     C     DDTYPE        IFEQ      'G'
     C*
     C                   MOVE      ELDCD2        XXDDCD
     C                   MOVE      EL2DEF        XXDDEF
     C*** BEGIN CHG ***  04/10/06  ************************************
     C                   MOVE      EL2DEF        @@DATE
     C                   MOVE      ELCO#         @@CO#
     C                   MOVE      ELFND         @@FND
     C                   MOVE      ELFYR         @@FYR
     C                   MOVE      ELEMP#        @@EMP#
     C                   MOVE      ELDIV         @@DIV
     C*
     C                   CALL      'SREFFD'
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@EMP#            9
     C                   PARM                    @@DIV             5
     C                   PARM                    @@DATE            7
     C*
     C                   MOVE      @@DATE        XXDDEF
     C*** END CHG *****  04/10/06  ************************************
     C*
     C     DDKY          SETGT     PDD01
     C     DDKY2         READPE    PDD01                                  60
     C   60              MOVE      *BLANKS       DDTYPE
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C***  END   ADD  ***  04/07/05 *************************************
     C*
     C                   MOVE      *OFF          XXELFL
     C                   ENDIF
     C*
     C     DDTYPE        IFEQ      'N'
      *** BEG ADD ***** 12/09/15 *************************************
     C     EAAMT         ANDGT     0

      /free
       xxAmtToRcvr = AdjAmtToRcvr();
      /end-free
      *** END ADD ***** 12/09/15 *************************************
     C                   CLEAR                   PDQ01
     C*
     C     DQKY          CHAIN     PDQ01                              60
     C*
     C*** Begin dlt *** 02/26/08 **************************************
     C*** Begin add *** 05/09/02 **************************************
     C********   EAAMT     IFLT 0
     C********   *IN60     ANDEQ*ON
     C********   DQKY1     CHAINPDQ01                60
     C********             ENDIF
     C*** End add ***** 05/09/02 **************************************
     C*** End dlt ***** 02/26/08 **************************************
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C*  IF THE DATE/TIME STAMP IN THE A/R FILE IS PRIOR TO THE
     C*  PENDING RECOVERY FILE, THIS IS A RESTART AND THE RECORD
     C*  HAS ALREADY BEEN PROCESSED.
     C*
     C     EADTTM        IFGT      DQDTTM
     C*** BEGIN ADD *** 02/02/99 **************************************
     C     EAPGM         ORNE      DQPGM
     C*** END ADD ***** 02/02/99 **************************************
     C                   ADD       EAAMT         DQAMT
     C*** BEGIN ADD ***  04/09/98  ************************************
      *** beg chg ***** 12/18/15 **************************************
     C*****************  ADD       EAAMT         DQAMTR
     C                   ADD       xxAmtToRcvr   DQAMTR
      *** end chg ***** 12/18/15 **************************************
     C*** END ADD *****  04/09/98  ************************************
     C                   MOVE      EAUSER        DQUSER
     C                   MOVE      EAPGM         DQPGM
     C                   MOVE      EATIME        DQTIME
     C*** BEG CHG ***** 07/29/15 **************************************
     C***********        MOVE      EATRDT        DQDT
     C                   MOVE      EADT          DQDT
     C*** END CHG ***** 07/29/15 **************************************
     C*** Begin add *** 03/14/05 **************************************
     C                   MOVE      *BLANKS       DQSTAT
     C*** End add ***** 03/14/05 **************************************
     C                   UPDATE    PDQ01
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVE      EACO#         DQCO#
     C                   MOVE      EAFND         DQFND
     C                   MOVE      EAFYR         DQFYR
     C                   MOVE      EAEMP#        DQEMP#
     C                   MOVE      EADIV         DQDIV
     C                   MOVE      EACHK#        DQCHK#
     C                   MOVE      EAAMT         DQAMT
     C*** BEGIN ADD ***  04/09/98  ************************************
      *** beg chg ***** 12/18/15 **************************************
     C*****************  MOVE      EAAMT         DQAMTR
     C                   eval      DQamtr = xxAmtToRcvr
      *** end chg ***** 12/18/15 **************************************
     C*** END ADD *****  04/09/98  ************************************
     C                   MOVE      EAUSER        DQUSER
     C                   MOVE      EAPGM         DQPGM
     C                   MOVE      EATIME        DQTIME
     C*** BEG CHG ***** 07/29/15 **************************************
     C***********        MOVE      EATRDT        DQDT
     C                   MOVE      EADT          DQDT
     C*** END CHG ***** 07/29/15 **************************************
     C*** Begin add *** 03/14/05 **************************************
     C                   MOVE      *BLANKS       DQSTAT
     C*** End add ***** 03/14/05 **************************************
     C                   WRITE     PDQ01
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDIF                                                  DDTYPE = 'N'
      *** BEG ADD ***** 12/09/15 **************************************
      /free
       If DDtype = 'N' and EAamt < 0;
          Exsr procReversal;
       Endif;
      /end-free
      *** END ADD ***** 12/09/15 **************************************
     C*
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*** Begin add *** 05/14/03 **************************************
     C                   ENDIF
     C*** End add ***** 05/14/03 ***************************************
     C*
     C*  INITIALIZE LEVEL BREAK FIELDS
     C*
     C     *INZSR        BEGSR
     C                   MOVE      EACO#         SVCO#
     C                   MOVE      EAFND         SVFND
     C                   MOVE      EAFYR         SVFYR
     C                   MOVE      EAEMP#        SVEMP#
     C                   MOVE      EADIV         SVDIV
     C                   ENDSR
      *** beg add ***** 12/09/15 *******************************
      *=========================================================
      * Wash check amount down to zero
      *=========================================================
      /free
       Begsr procReversal;
       Chain (EAco#:EAfnd) pfn00;
       DQco#  = EAco#;
       DQfnd  = EAfnd;
       DQfyr  = EAfyr;
       DQemp# = EAemp#;
       DQdiv  = EAdiv;
       DQchk# = EAchk#;
       DQamt  = EAamt;
       DQamtr = SpreadRvrsal;
       DQuser = EAuser;
       DQpgm  = EApgm;
       DQdt   = EAdt;
       DQtime = EAtime;
       DQstat = EAstat;
       Write pdq01;
       Endsr;
      /end-free
      *=========================================================
      * Clear negative remaining amounts from existing
      *  post net records first
      *=========================================================
     pAdjAmtToRcvr     b
     dAdjAmtToRcvr     pi                  like(EAamt)
      /free
       xxAmtToRcvr = EAamt;
       Setgt  (EAco#:EAfnd:EAemp#:EAdiv:EAfyr) pdq01;
       Readpe (EAco#:EAfnd:EAemp#:EAdiv:EAfyr) pdq01;
       Dow not(%Eof);
          If DQamtr >= 0;
             Readpe (EAco#:EAfnd:EAemp#:EAdiv:EAfyr) pdq01;
             Iter;
          Endif;
          xxAmtToRcvr = EAamt + DQamtr;
          If xxAmtToRcvr >= 0;
             DQamtr  = 0;
          Else;
             xxAmtToRcvr = 0;
             DQamtr += EAamt;
          Endif;
          Update pdq01;
          If xxAmtToRcvr = 0;
             Leave;
          Endif;
       Readpe (EAco#:EAfnd:EAemp#:EAdiv:EAfyr) pdq01;
       Enddo;
       Return xxAmtToRcvr;
      /end-free
     pAdjAmtToRcvr     e
      *=========================================================
      * First look in existing Post Net and reduce any remaining
      * amounts, then loop backwards through deductible detail
      * and reverse recoveries (if payment recovery is found)
      * until the amount in the check is exhausted.
      *=========================================================
     pSpreadRvrsal     b
     dSpreadRvrsal     pi                  like(EAamt)
     d amtToRvrs       s                   like(EAamt)
     d indPaymntFound  s               n
     d xxAmtToRvrs     s                   like(EAamt)

      /free
       xxAmtToRvrs = -EAamt;
       Setgt  (EAco#:EAfnd:EAemp#:EAdiv:EAfyr) pdq01;
       Readpe (EAco#:EAfnd:EAemp#:EAdiv:EAfyr) pdq01;
       Dow not(%Eof);
          If DQamtr <= 0;
             Readpe (EAco#:EAfnd:EAemp#:EAdiv:EAfyr) pdq01;
             Iter;
          Endif;
          If DQamtr < xxAmtToRvrs;
             amtToRvrs    = DQamtr;
             xxAmtToRvrs -= DQamtr;
             DQamtr       = 0;
          Else;
             amtToRvrs    = xxAmtToRvrs;
             xxAmtToRvrs  = 0;
             DQamtr      -= amtToRvrs;
          Endif;
          Update pdq01;
       Readpe (EAco#:EAfnd:EAemp#:EAdiv:EAfyr) pdq01;
       Enddo;

       Setgt  (EAco#:EAfnd:EAemp#:EAdiv:EAfyr) pdd08;
       Readpe (EAco#:EAfnd:EAemp#:EAdiv:EAfyr) pdd08;
       Dow not(%Eof);
          If DDamtr <= 0;
             Readpe (EAco#:EAfnd:EAemp#:EAdiv:EAfyr) pdd08;
             Iter;
          Endif;
          If DDamtr < xxAmtToRvrs;
             amtToRvrs    = DDamtr;
             xxAmtToRvrs -= DDamtr;
             DDamtr       = 0;
             DDamtp      += DDamtr;
          Else;
             amtToRvrs    = xxAmtToRvrs;
             xxAmtToRvrs  = 0;
             DDamtr      -= amtToRvrs;
             DDamtp      += amtToRvrs;
          Endif;
          indPaymntFound = *off;
          Setgt  (DDco#:DDfnd:DDfyr:DDcase) ppa05;
          Readpe (DDco#:DDfnd:DDfyr:DDcase) ppa05;
          Dow not(%Eof);
             Select;
             When DDrecd <> PArecd;
             When PAbr2  <> FNcop1 and PArecd = 'CO';
             When PAbr2  <> FNmep1 and PArecd = 'ME';
             When PAbr2  <> FNexp1 and PArecd = 'EX';
             When PAbr2  <> FNlep1 and PArecd = 'LE';
             Other;
                ##rtcd = 'Z';
                Callp ReverseRcvry(%editc(PAco#:'X'):
                                   %editc(PAfnd:'X'):
                                   %editc(PAfyr:'X'):
                                   %editc(PAcase:'X'):
                                   %editc(PAseq#:'X'):
                                   %editc(amtToRvrs:'X'):
                                   ##rtcd);
                indPaymntFound = *on;
                Leave;
             Endsl;
          Readpe (DDco#:DDfnd:DDfyr:DDcase) ppa05;
          Enddo;
          If indPaymntFound = *on;
              DDuser = Q1user;
              DDpgm  = Q1pgm;
              DDdt   = Dateto7(SystemDate());
              DDtime = Timeto6(SystemTime());
              Update pdd08;
          Endif;
          If xxAmtToRvrs = 0;
             Leave;
          Endif;
       Readpe (EAco#:EAfnd:EAemp#:EAdiv:EAfyr) pdd08;
       Enddo;
       Return -xxAmtToRvrs;
      /end-free
     pSpreadRvrsal     e
      *** end add ***** 12/09/15 *******************************
      *=========================================================
