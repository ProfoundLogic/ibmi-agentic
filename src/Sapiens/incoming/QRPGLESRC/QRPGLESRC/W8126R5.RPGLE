     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      /TITLE W8126R5 - CLARENDON - BUILD AGED RECEIVABLES
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
      *****************************************************************
      * Revisions:                                                    *
     H*                                                               *
     H* 01/10/18 C:33  Support 63341                                  *
     H*                Clear agt# before WA002R call                  *
      *                                                               *
      * 10/23/07 C:136 Support: WA109 59994                           *
      *                Make sure expense constant from non-state      *
      *                reporting divisions get added into premium.    *
      *                                                               *
      * 08/30/07 C:136 Support: WA109 58484                           *
      *                Change from 06/22/07 was causing the exp       *
      *                constant to be over-written.                   *
      *                                                               *
      * 06/22/07 C:136 Support: WA109 58484                           *
      *                Summarized A/R throws deferred balance off     *
      *                when policy is cancelled.  Total written       *
      *                premium from all summarized divisions for      *
      *                deferred balance calculation.                  *
      *                                                               *
      * 01/07/05 C:136 Support: WA121 44786                           *
      *                Add exclude A/R fields. Uses LDA.              *
      *                                                               *
      * 11/16/04 C:136 Project: 109WDY, 122WBV, 33WGT, 121WBG         *
      *                Balance cancelled accounts that aren't audited *
      *                written premium to Aging amounts.              *
      *                                                               *
      * 10/22/04 C:136 Support: WA122 44274                           *
      *                Correct the self-reporting deferred calculation*
      *                Do not add expense constant to written prem    *
      *                unless the CEU record has premium.             *
      *                                                               *
      * 09/28/04 C:136 Support: WA109 43967                           *
      *                Do not exclude expired accounts from the       *
      *                self-reporting calculation.                    *
      *                                                               *
      * 09/17/04 C:136 Support:WA109 43903                            *
      *                Correct the amount of payments received for    *
      *                the deferred calculation by running through    *
      *                A/R.                                           *
      *                                                               *
      * 07/19/04 C:136 Support: WA122 43131                           *
      *                Correct new S/R deferred calculation to        *
      *                properly account for advanced premium payments.*
      *                (The Aging Report gives a credit for current   *
      *                due in this situation.)                        *
      *                                                               *
      * 06/29/04 C:136 Support: WA122 43020                           *
      *                Add the new S/R calculated deferred amount to  *
      *                total amount due.                              *
      *                                                               *
      * 05/13/04  C:136  Support: WA121 42063                         *
      *                  Change deferred amount to match Clarendon's  *
      *                  new algorithm, WRITTEN PREMIUM minus         *
      *                  minus CASH RECEIVED minus AGED ACCOUNTS      *
      *                  RECEIVABLE equals DEFERRED PREMIUM.          *
      *                                                               *
      * 11/07/02  C:142  PROJECT: WC970                               *
      *                  Fixed problem with Geadpr.                   *
      *                  And addition problem with RSKY (RSKY1)       *
      *                                                               *
      * 10/23/02  C:142  PROJECT: WC970 Supp:34460                    *
      *                  Add Agdefr to defr so will balance for Clrdn *
      *                                                               *
      * 09/17/02  C:142  PROJECT: WC970 Supp:34182                    *
      *                  Only use SR defered calc if not audited      *
      *                                                               *
      * 08/20/02  C:142  PROJECT: WC970                               *
      *                  Change the way amount deferred is calced     *
      *                  for self-reporting policies only.            *
      *                                                               *
      * 05/17/02  C:35   PROJECT: WC1085                              *
      *                  Pass new parameters to W7291R1 to include    *
      *                  only premium and expense constant.           *
      *                                                               *
      * 04/03/02 C:142 Project: WC970                                 *
      *                Changed calculation Advance Premium            *
      *                Make sure Polmp record exists.                 *
      *                                                               *
      * 12/28/01 C:15  Project: WC970                                 *
      *                Changed calculation for 'Total amt due' and    *
      *                'Amt deferred'.                                *
      *****************************************************************
     FWXPGL     IF   E           K DISK
     FWFAGL     IF   E           K DISK
     FWMGCL     IF   E           K DISK
     FWMRPL4    IF   E           K DISK
     FWDABL7    IF   E           K DISK
     FWDELL1    IF   E           K DISK
     FWtabl     IF   E           K DISK
     F                                     Ignore(Prs01)
     *
     * Employer Detail File
     FWdell2    IF   E           K Disk
     F                                     Rename(Pel01:Wdelr2)
     F                                     Prefix(L2:2)
     *
     * Rate Set Code Master File
     FWmrsl     IF   E           K Disk
      *
     * Inforce premium file
     FInfcl     IF   E           K Disk
      *
      *** beg add ***** 05/13/04 *************************
      * Calendar Earned Unearned file
     Fwfeul2    if   e           k disk
      *** beg chg ***** 09/17/04 *************************
      *** beg add ***** 06/22/07 *************************
     Fwfeul1    if   e           k disk    rename(peu01 : peu0l1)
      *** end add ***** 06/22/07 *************************
     F************ wxx1l     if   e           k disk
     Fwmeal4    if   e           k disk
     F                                     rename(pea01:pea04)
      *** end chg ***** 09/17/04 *************************
      *** end add ***** 05/13/04 *************************
      *** beg add ***** 11/16/04 *************************
     Fwmeml     if   e           k disk
     Fwdehl14   if   e           k disk
      *** end add ***** 11/16/04 *************************
      *
     FAGEDP     O    E             DISK
      *
     D DATUSA          S               D   DATFMT(*USA)
     D Ptdt            S               D   DATFMT(*USA)
     D Medte           S               D   DATFMT(*USA)
     D Remaing         S              4  0
     D Curdays         S              2  0
     D                 DS
     D  XXPOL                  1     15
     D  X1FND                  1      3  0
     D  X1EMP#                 4     12  0
     D  X1FYR                 13     15  0
     D Hold20          S             20  2
     D Tab10           S             10
     D Futureyr        S              8
     D FirstFyr        S                   Like(ElFyr)
     D FirstEfd        S                   Like(Elpfdt)
     D Kyrtcd          S                   Like(Elrtcd)
     D Pgid            S                   Like(Gepgid)
     D First           S              1n
      *** beg add ***** 05/13/04 ************************************
     D srWrtnPrem      s             15  2
     D srDefrdAmt      s             15  2
     D srRecvdAmt      s             15  2
      *** beg add ***** 07/19/04 ***********************************************
     D srAmtDue        s             15  2
      *** end add ***** 07/19/04 ***********************************************
     D kktrdt          S                   Like(EUTRDT)
      *** end add ***** 05/13/04 ************************************
      *** beg add ***** 09/17/04 ************************************
     D kkkey           S                   Like(TTKEY)
      *** end add ***** 09/17/04 ************************************
      *** beg add ***** 11/16/04 ************************************
     d cxDate          s                   Like(EMCAD1)
     d estWrtnPrem     s             15  2
     d estRecvdAmt     s             15  2
     d cxDiff          s             15  2
     d cxFlag          s               n
     d negFlag         s               n
     d proExCon        s                   like(ELPRTD)
     d polFrDt         s              8  0
     d polToDt         s              8  0
     d numPolicyDays   s              5  0
     d numCanxDays     s              5  0
     d proRateFactor   s              5  4
     d xxtdt           s              7  0
     d x8Date          s              8  0
     d xx30dt          s              7  0
     d xx60dt          s              7  0
     d xx90dt          s              7  0
      *** beg add ***** 01/07/05 ************************************
     D lda             ds          1024
     D  exclARcodes                  10
     D  exclAR_1                      2    overlay(exclARcodes)
     D  exclAR_2                      2    overlay(exclARcodes:3)
     D  exclAR_3                      2    overlay(exclARcodes:5)
     D  exclAR_4                      2    overlay(exclARcodes:7)
     D  exclAR_5                      2    overlay(exclARcodes:9)
      *** end add ***** 01/07/05 ************************************
      *** beg add ***** 06/22/07 ************************************
     d s1div           s                   like(EUDIV)
      *** end add ***** 06/22/07 ************************************
      *** beg add ***** 08/30/07 ************************************
     d svprtd          s                   like(ELPRTD)
      *** end add ***** 08/30/07 ************************************

     D/copy SPRDATE
      *** end add ***** 11/16/04 ************************************
      *** beg add ***** 01/07/05 ************************************
     C     *dtaara       define    *lda          lda
     C                   in        lda
     c                   if        exclAR_1 <> *blank or
     c                             exclAR_2 <> *blank or
     c                             exclAR_3 <> *blank or
     c                             exclAR_4 <> *blank or
     c                             exclAR_5 <> *blank
     c                   movel     exclARcodes   qqcode
     c                   move      '@@'          qqcode
     c                   move      'E'           qqarie
     c                   endif
      *** end add ***** 01/07/05 ************************************
      *
      *  GROUP CONTROL FILE
      *
     C     PGKY          Klist
     C                   Kfld                    XXCO#             3 0
      *
      *  RATE SET CODE FILE
      *
     C     RSKY          Klist
     C                   Kfld                    XXCO#
     C                   Kfld                    ELRTCD
      *
      *  BILLING SCHEDULE CODE FILE
      *
     C     BSKY          Klist
     C                   Kfld                    XXCO#
     C                   Kfld                    Tab10
      *
      *  GROUP CARRIER INFO FILE
      *
     C     GCKYSV        Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Svfnd
      *
     C     GCKYAG        Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Agfnd
      *
      *  EMPLOYER MASTER FILE
      *
     C     abky          Klist
     C                   Kfld                    XXCO#
     C                   Kfld                    XXFND
     C                   Kfld                    XXEMP#
      *
      *  EMPLOYER DETAIL FILE
      *
     C     ELKY          Klist
     C                   Kfld                    XXCO#
     C                   Kfld                    XXFND
     C                   Kfld                    XXFYR
     C                   Kfld                    XXEMP#
     C                   Kfld                    XXDIV
      *
      *** beg add ***** 06/22/07 *******************************************************************
     C     ELKY1         Klist
     C                   Kfld                    EUCO#
     C                   Kfld                    EUFND
     C                   Kfld                    EUFYR
     C                   Kfld                    EUEMP#
     C                   Kfld                    EUDIV
      *** end add ***** 06/22/07 *******************************************************************
      *
     C     RPKY          Klist
     C                   Kfld                    XXCO#
     C                   Kfld                    XXFND
     C                   Kfld                    XXFYR
     C                   Kfld                    XXEMP#
      *
      **** BEGIN ADD **** 11/07/02 *****************************************************************
     C     RPKY1         Klist
     C                   Kfld                    XXCO#
     C                   Kfld                    ELFND
     C                   Kfld                    ELFYR
     C                   Kfld                    ELEMP#
      **** END   ADD **** 11/07/02 *****************************************************************
      *
     C     Wmrsl_Key     Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Kyrtcd
      *
     C     Wdell2_Key    Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    SvFnd
     C                   Kfld                    SvEmp#
      *
     C     Wdell1_Key    Klist
     C                   Kfld                    XXCO#
     C                   Kfld                    SVFND
     C                   Kfld                    SVFYR
     C                   Kfld                    SVEMP#
     C                   Kfld                    SVDIV
      *
     C     Infcl_Key     Klist
     C                   Kfld                    Pgid
     C                   Kfld                    Xxpol30          30
     C                   Kfld                    Agrst
     C                   Kfld                    Agfyr
     C                   Kfld                    Type              2
     C                   Move      '10'          Type
      *
      *** beg add ***** 05/13/04 ************************************
     c     euky          klist
     c                   kfld                    xxco#
     c                   kfld                    svfnd
     c                   kfld                    svrst
     c                   kfld                    svemp#
     c                   kfld                    svdiv
     c                   kfld                    svfyr
      *
     c     euky1         klist
     c                   kfld                    xxco#
     c                   kfld                    svfnd
     c                   kfld                    svrst
     c                   kfld                    svemp#
     c                   kfld                    svdiv
     c                   kfld                    svfyr
     c                   kfld                    kktrdt
      *
      *** beg add ***** 06/22/07 ************************************
     c     eukyl1        klist
     c                   kfld                    xxco#
     c                   kfld                    svfnd
     c                   kfld                    svfyr
     c                   kfld                    svemp#
      *** end add ***** 06/22/07 ************************************
      *
      *** beg chg ***** 09/17/04 ***********************************************
     c*****x1ky********  klist
     c     eaky4         klist
      *** end chg ***** 09/17/04 ***********************************************
     c                   kfld                    xxco#
     c                   kfld                    svfnd
     c                   kfld                    svemp#
     c                   kfld                    svdiv
     c                   kfld                    svfyr
      *** end add ***** 05/13/04 ************************************
      *
      *** beg add ***** 09/17/04 ***********************************************
     c     ttky          klist
     c                   kfld                    xxco#
     c                   kfld                    kkkey
      *** end add ***** 09/17/04 ***********************************************
      *
      *** beg add ***** 11/16/04 ************************************
     c     emky          klist
     c                   kfld                    xxco#
     c                   kfld                    svfnd
     c                   kfld                    svemp#
     c                   kfld                    svdiv
      *
     c     ehky1         klist
     c                   kfld                    xxco#
     c                   kfld                    svfnd
     c                   kfld                    svemp#
     c                   kfld                    svdiv
     c                   kfld                    svfyr
     c                   kfld                    cxDate
      *
     c     ehky          klist
     c                   kfld                    xxco#
     c                   kfld                    svfnd
     c                   kfld                    svemp#
     c                   kfld                    svdiv
     c                   kfld                    svfyr
      *** end add ***** 11/16/04 ************************************
      *
      *   ENTRY PARAMETERS
      *
     C     *Entry        Plist
     C                   Parm                    QQCO#             3
     C                   Parm                    QQFMDT            8
     C                   Parm                    QQTDT             8
     C                   Parm                    QQQUE            10
      *
     C     *Usa0         Move      Qqtdt         Datusa
     C     Datusa        Adddur    1:*y          Datusa
     C     *Usa0         Move      Datusa        Futureyr
      *
      *** beg del ***** 11/16/04 ***********************************************
     C*****DTMDYY***     Plist
     C**************     Parm                    SRDTI             8 0
     C**************     Parm      '*CYMD'       SRFFMT            8
     C**************     Parm      *BLANKS       SRTFMT            8
     C**************     Parm                    SRDTO            10
     C**************     Parm                    SRERR             1
      **************
     C*****DTCYMD***     Plist
     C**************     Parm                    SRDTI             8 0
     C**************     Parm      *BLANKS       SRFFMT            8
     C**************     Parm      '*CYMD'       SRTFMT            8
     C**************     Parm                    SRDTO            10
     C**************     Parm                    SRERR             1
      *** end del ***** 11/16/04 ***********************************************
      *
     C                   Move      QQCO#         XXCO#
     C                   Move      QQFMDT        XXFMDT            8 0
     C                   Sub       5             XXFMDT
     C                   Move      XXFMDT        QQFDT             8
     C                   Z-Add     0             XXEMP#            9 0
     C                   Move      XXEMP#        QQEMP#
     C                   Z-Add     0             XXMINC            9 0
     C                   Z-Add     0             XXMIND            9 0
     C                   Move      XXMINC        QQMINC
     C                   Move      XXMIND        QQMIND
      *** beg chg ***** 11/16/04 ***********************************************
     C*****************  Move      QQTDT         SRDTI
     C*****************  Call      'SRCVTDAT'    DTCYMD
     C*****************  Movel     SRDTO         XXTDT             7 0
     c                   move      qqtdt         x8Date
     c                   eval      xxtdt = dateTo7(cvtToDate(x8Date))
      *** end chg ***** 11/16/04 ***********************************************
      *
      *   CALCULATE 30, 60, & 90 DAYS IN THE PAST ACCORDING TO DATE
      *   ENTERED
      *         *** 30 DAYS ***
     C                   Z-Add     30            XXDAYS            5 0
     C                   Exsr      SRPAST
      *** beg chg ***** 11/16/04 ***********************************************
     C*****************  Move      SRDATE        SRDTI
     C*****************  Call      'SRCVTDAT'    DTCYMD
     C*****************  Movel     SRDTO         XX30DT            7 0
     c                   move      srDate        x8Date
     c                   eval      xx30dt = dateTo7(cvtToDate(x8Date))
      *** end chg ***** 11/16/04 ***********************************************
      *
      *         *** 60 DAYS ***
     C                   Z-Add     60            XXDAYS            5 0
     C                   Exsr      SRPAST
      *** beg chg ***** 11/16/04 ***********************************************
     C*****************  Move      SRDATE        SRDTI
     C*****************  Call      'SRCVTDAT'    DTCYMD
     C*****************  Movel     SRDTO         XX60DT            7 0
     c                   move      srDate        x8Date
     c                   eval      xx60dt = dateTo7(cvtToDate(x8Date))
      *** end chg ***** 11/16/04 ***********************************************
     **
      *         *** 90 DAYS ***
     C                   Z-Add     90            XXDAYS            5 0
     C                   Exsr      SRPAST
      *** beg chg ***** 11/16/04 ***********************************************
     C*****************  Move      SRDATE        SRDTI
     C*****************  Call      'SRCVTDAT'    DTCYMD
     C*****************  Movel     SRDTO         XX90DT            7 0
     C                   move      srDate        x8Date
     c                   eval      xx90dt = dateTo7(cvtToDate(x8Date))
      *** end chg ***** 11/16/04 ***********************************************
      /eject
      *
      *  RUN THE REPORT FOR EACH SELECTED GROUP.
      *
     C     PGKY          Chain     XPG01                              60
     C     *IN60         Doweq     *OFF
      *
     C     PGFLAG        Ifeq      'I'
      *
     C                   Move      PGGRP         QQFND             3
      * Employer Report Workfile Build Override
     C                   Call      'SRRPTC'
     C                   Parm                    QQCO#
     C                   Parm                    QQFND
     C                   Parm                    QQFDT
     C                   Parm                    Futureyr
     C                   Parm                    QQTDT
     C                   Parm      'B'           QQRDST            1            RUN DT STS
     C                   Parm      'B'           QQTYPE            1
     C                   Parm      'E'           QQSTAT            1
     C                   Parm      'EN'          QQSRT1            2
     C                   Parm                    QQSRT2            2
     C                   Parm                    QQEMPF            1
     C                   Parm                    QQAMPF            1
     C                   Parm                    QQPRTY            2            PREM TYPE
     C                   Parm                    QQEMP#            9
     C                   Parm      '00000'       QQDIV             5
     C                   Parm      '000'         QQFYR             3
     C                   Parm                    QQSEL1            2            SEL 1
     C                   Parm                    QQS1FR           15            LOW VAL
     C                   Parm                    QQS1TO           15            HI VAL
     C                   Parm                    QQSEL2            2            SEL2
     C                   Parm                    QQS2FR           15            LOW VAL
     C                   Parm                    QQS2TO           15            HI VAL
      * Employer aging report
     C                   Call      'W7291R1'
     C                   Parm                    QQCO#
     C                   Parm                    QQFND
     C                   Parm                    QQFDT
     C                   Parm                    Futureyr
     C                   Parm                    QQTDT
     C                   Parm                    QQQUE
     C                   Parm                    QQMODE            1
     C                   Parm      'EN'          QQSRT1            2
     C                   Parm                    QQSRT2            2
     C                   Parm                    QQFREA            2
     C                   Parm                    QQTOEA            2
     C                   Parm                    QQFREN            9
     C                   Parm                    QQTOEN            9
     C                   Parm                    QQFRAA            2
     C                   Parm                    QQTOAA            2
     C                   Parm                    QQFRAN            5
     C                   Parm                    QQTOAN            5
     C                   Parm      'B'           QQACST            1
     C                   Parm      'B'           QQESSR            1
     C                   Parm      'B'           QQCRDB            1
     C                   Parm      'B'           QQAUST            1
     C                   Parm                    QQCODE           60
     C                   Parm                    QQARIE            1
     C                   Parm                    QQMINC            9
     C                   Parm                    QQMIND            9
     C                   Parm                    QQIEFL            1
     C                   Parm                    QQASTC           10
      **** BEGIN ADD **** 05/17/02 *****************************************************************
     C                   Parm      'I'           QQCTIE            1
     C                   Parm      'PE'          QQASTC           10
      **** END   ADD **** 05/17/02 *****************************************************************
      *
      * Dltf Qtemp/Wxebp,L,L1
      *
     C                   Call      'W8126C2'
      *
     C                   Endif                                                  PGFLAG Ifeq
      *
     C     PGKY          Reade     XPG01                                  60
      *
     C                   Enddo
      *
     C                   Clear                   SVEMP#            9 0
     C                   Clear                   SVFND             3 0
     C                   Clear                   SVDIV             5 0
     C                   Clear                   SVFYR             3 0
     C                   Clear                   SVRST             2
     C                   Move      *OFF          *IN99
      *
      /eject
     c                   Eval      First = *On
     C     *Loval        Setll     Wfagl                                  60
      *
     C                   Dou       %Eof(Wfagl)
      *
     C                   Read      Wfagl                                  60
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   If        First
     C                   Z-Add     Agemp#        Svemp#
     C                   Z-Add     Agfnd         Svfnd
     C                   Z-Add     Agdiv         Svdiv
     C                   Z-Add     Agfyr         Svfyr
     C                   Move      Agrst         Svrst
     C                   Eval      First = *Off
     C                   Endif
      *
     C                   If        Svemp# <> Agemp# or
     C                             Svfnd  <> Agfnd or
     C                             Svfyr  <> Agfyr or
     C                             Svrst  <> Agrst
      *
     C     GCKYSV        Chain     PGC01                              60
     C     *IN60         Ifeq      *OFF
     C                   Move      GCPGID        PGID
     C                   Endif
      *
     C                   Exsr      Write
     C                   Z-Add     0             XX00              9 2
     C                   Z-Add     0             XX30              9 2
     C                   Z-Add     0             XX60              9 2
     C                   Z-Add     0             XX90              9 2
     C                   Z-Add     0             XXAMTD            9 2
     C                   Z-Add     0             XXCURR            9 2
     C                   Z-Add     0             XXDEFF           11 2
     C                   Z-Add     0             Defr             11 2
     C                   Z-Add     *Zero         XxAdpr            9 2
      **** BEGIN ADD **** 11/07/02 *****************************************************************
     C                   Move      *Blanks       Xxaudf            1
      **** END   ADD **** 11/07/02 *****************************************************************
     C                   Z-Add     AGEMP#        SVEMP#
     C                   Z-Add     AGFND         SVFND
     C                   Z-Add     AGdiv         SVdiv
     C                   Z-Add     AGFYR         SVFYR
     C                   Move      AGRST         SVRST
     C                   Move      *ON           *IN99
     C                   Endif
      *
     C                   ADD       AGCURR        XX00
     C                   ADD       AG30DY        XX30
     C                   ADD       AG60DY        XX60
     C                   ADD       AG90DY        XX90
     C                   ADD       AGAMTD        XXCURR
     C                   ADD       AGTLDU        XXAMTD
      **** BEGIN ADD **** 11/07/02 *****************************************************************
     C                   Move      Agaudf        XXaudf
      **** END   ADD **** 11/07/02 *****************************************************************
      *
     C     GCKYAG        Chain     PGC01                              60
     C     *IN60         Ifeq      *OFF
     C                   Move      GCPGID        PGID
     C                   Endif
      *
     C     Wdell1_Key    Chain     Wdell1
      *
     C                   If        %Found
     C                   Movel(P)  Elbscd        Tab10
     C     Bsky          Chain     Pbs01
      *
      *** beg add ***** 08/30/07 *****************************************
     c                   eval      svprtd = ELPRTD
      *** end add ***** 08/30/07 *****************************************
      *
     C                   If        %Found and
     C                             Bssefl = 'S' and
      **** Begin Add 09/17/2002 ******************************************
      *** beg chg ***** 05/13/04 *****************************************
     C*****************            Elfaf <> 'A'
      *** beg chg ***** 09/28/04 *****************************************
     C******************           ElFAF <> 'A' and
     C******************           ELPTDT > xxtdt
     C                             ELFAF <> 'A'
      *** end chg ***** 09/28/04 *****************************************
      *** end chg ***** 05/13/04 *****************************************
      **** End   Add 09/17/2002 ******************************************
     C                   Exsr      Self_Reportng
     C                   Else
     C                   Add       Agdefr        Xxdeff
     C                   Endif

      *** beg add ***** 11/16/04 ***********************************************
     C     Bsky          Chain     Pbs01
     c                   if        %found and
     c                             BSSEFL = 'E' and
     c                             ELFAF <> 'A'
     c                   exsr      srCxBal
     c                   endif
      *** end add ***** 11/16/04 ***********************************************

     C                   Endif
      *
     C                   Enddo
      *
     C                   Exsr      Write
      *
     C                   Move      *ON           *INLR
      *=========================================================================
      * Subroutine - Self-reporting deferred premium calculation
      *=========================================================================
     C     Self_Reportng Begsr
      *
      ****beg*del****** 05/13/04 *******************************************
     C*************      Eval      Defr = *Zero
      *****BEGIN*CHANGE * 11/07/02 *****************************************************************
     C*****Rpky****      Chain     Prp01
     C*****Rpky1***      Chain     Prp01
      *****END*CHANGE *** 11/07/02 *****************************************************************
      *************
     C*************      If        %Found and
     C*************                Rpcap# <> *Blanks
     C*************      Movel(P)  Rpcap#        XXpol30
     C*************      Else
     c*************      Move      Agfnd         X1fnd
     C*************      Move      Agemp#        X1emp#
     C*************      Move      Agfyr         X1fyr
     C*************      Movel(P)  Xxpol         Xxpol30
     C*************      Endif
     C*************
     C******Ymd****      Move      Elptdt        Ptdt
     C******Usa0***      Move      QQtdt         Medte
     C*************      Extrct    Medte:*D      Curdays
     C*************
     C*************      If        Ptdt <= Medte
     C*************      Else
     C*****Ptdt****      Subdur    Medte         Remaing:*D
     C*************      Eval      Remaing = Remaing - 1
     C*************
     C*************      If        Remaing <> *Zero
     C*****Infcl_Key     Chain     Infcl
     C*************
     C*************      If        %Found
     C*************      Eval      Defr = Fcue# +
     C*************                (Fcue# * (Curdays/Remaing))
     C*************      Eval      Xxamtd = Defr + Xxamtd
     C******Begin*Add  10/23/2002 ******************************************
     C*************      Eval      Defr   = Defr + Agdefr
     C******End***Add  10/23/2002 ******************************************
     C*************      Endif
     C*************
     C*************      Endif
     C*************
     C*************      Endif
     C*************
     C*************      Eval      Xxdeff = Xxdeff + Defr
     C*************      Eval      Defr = *Zero
      *** end del ***** 05/13/04 *******************************************
      *** beg add ***** 05/13/04 *******************************************

     c                   eval      srWrtnPrem  = 0
     c                   eval      srRecvdAmt  = 0
     c                   eval      srDefrdAmt  = 0
     c                   eval      kktrdt      = 0
      *
      * Get Written Premium from WFEUP
      *
      *** beg add ***** 07/19/04 ***************************************
      *** beg del ***** 10/22/04 ***************************************
     c*****************  z-add     ELPRTD        srWrtnPrem
      *** end del ***** 10/22/04 ***************************************
      *** end add ***** 07/19/04 ***************************************
      *** beg add ***** 06/22/07 ***************************************
     c                   if        ELARSM <> 'Y'
      *
      *  Roll through single division to get written premium if not summarized
      *
      *** end add ***** 06/22/07 ***************************************
     c     euky1         setll     peu01
     c     euky          reade     peu01
     c                   dow       not %eof
     c                   add       EUPYWR        srWrtnPrem
     c                   add       EUYDWR        srWrtnPrem
     c     euky          reade     peu01
     c                   enddo

      *** beg add ***** 06/22/07 ***************************************
     c                   else
      *
      *    Roll through all summarized divisions and accumuate written premium if
      *    this is the state reporting division.
      *
     c     emky          chain     pem01
     c                   if        %found(WMEML) and
     c                             EMSTRP = 'Y'

     c     eukyl1        setll     peu0l1
     c     eukyl1        reade     peu0l1

     c                   eval      s1div  = -1
     c                   dow       not(%eof(WFEUL1))

     c                   if        EUDIV  <> s1div
     c     elky1         chain     pel01
     C                   movel(p)  ELBSCD        tab10
     C     bsky          chain     pbs01
     c                   eval      s1div =  EUDIV
     c                   endif                                                  EUDIV <> S1DIV

     c                   if        ELARSM = 'Y' and BSSEFL = 'S'
     c                   add       EUPYWR        srWrtnPrem
     c                   add       EUYDWR        srWrtnPrem
      *** beg add ***** 10/23/07 ***************************************
     c                   add       ELPRTD        svprtd
      *** end add ***** 10/23/07 ***************************************
     c                   endif                                                  ELARSM = 'Y'

     c     eukyl1        reade     peu0l1
     c                   enddo

     c                   endif                                                  EMSTRP = 'Y'

     c                   endif                                                  ELARSM <> 'Y'
      *** end add ***** 06/22/07 ***************************************
      *
      *** beg add ***** 10/22/04 ***************************************
     c                   if        srWrtnPrem <> 0
      *** beg chg ***** 11/16/04 ***************************************
     c*****************  add       ELPRTD        srWrtnPrem
     c                   exsr      srCxFlg
     c                   if        cxFlag = *on
     c                   exsr      srProEx
     c                   add       proExCon      srWrtnPrem
     c                   else
      *** beg chg ***** 08/30/07 ***************************************
     c*****************  add       ELPRTD        srWrtnPrem
     c                   add       svprtd        srWrtnPrem
      *** end chg ***** 08/30/07 ***************************************
     c                   endif
      *** end chg ***** 11/16/04 ***************************************
      *** beg add ***** 11/16/04 ***************************************
     c                   endif
      *** end add ***** 11/16/04 ***************************************
      *
      * Get premium and expense constant payments from WMEAP (Refer to change for WC1085)
      *
      *** beg chg ***** 09/17/04 *****************************************
     c*****x1ky********  setll     px101
     c*****x1ky********  reade     px101
     c*****************  if        X1ARTY = 'P' and X1CTYP = 'P' or
     c*****************            X1ARTY = 'P' and X1CTYP = 'E'
     c*****************  add       X1AMT         srRecvdAmt
     c*****************  endif
      *****************
     c*****x1ky********  reade     px101

     c     eaky4         setll     pea04
     c     eaky4         reade     pea04
     c                   dow       not %eof

     c                   eval      kkkey = eaarcd
     c     ttky          chain     ptt01
     c                   if        %found

      *** beg chg ***** 11/04/04 **********************************************
     c**************     if        TTTYPE = 'P' and
     c**************               (TTCTYP = 'P' or TTCTYP = 'E')
     c**************     add       EAAMT         srRecvdAmt
     c**************     endif

     c                   if        (TTTYPE = 'P' or
     c                              TTTYPE = 'T')
     c                   add       EAAMT         srRecvdAmt
     c                   endif
     c
     c                   if        (TTTYPE = 'N' or
     c                              TTTYPE = 'R' or
     c                              TTTYPE = 'V')
     c                   sub       EAAMT         srRecvdAmt
     c                   endif
      *** end chg ***** 11/04/04 **********************************************

     c                   endif

     c     eaky4         reade     pea04
      *** end chg ***** 09/17/04 *****************************************
     c                   enddo
      *
      * Apply the equation from Clarendon for deferred amounts
      *
      *** beg chg ***** 07/19/04 ***************************************
     c*****************  eval      srDefrdAmt = (srWrtnPrem - srRecvdAmt
     c*****************                         - xxmatd)
     c                   sub       AGTLDU        xxamtd
     c                   eval      srAmtDue   = (srWrtnPrem - srRecvdAmt)
     c                   eval      srDefrdAmt = (srAmtDue - xxcurr)
     c                   add       srAmtDue      xxamtd

      *** beg add ***** 11/16/04 ***************************************
     c                   if        srDefrdAmt >= 0
      *** end add ***** 11/16/04 ***************************************
      *** end chg ***** 07/19/04 ***************************************
     c                   add       srDefrdAmt    xxdeff
      *** beg add ***** 06/29/04 *******************************************
      *** beg del ***** 07/19/04 ***************************************
     c*****************  add       srDefrdAmt    xxamtd
      *** end del ***** 07/19/04 ***************************************
      *** end add ***** 06/29/04 *******************************************
      *** end add ***** 05/13/04 *******************************************
      *** beg add ***** 11/16/04 ***************************************
     c                   else
     c                   add       srDefrdAmt    xxcurr
     c                   add       srDefrdAmt    xx00
     c                   endif
      *** end add ***** 11/16/04 ***************************************
      *
     C                   Endsr
      *=========================================================================
      *    Subroutine - Write to AGEDP
      *=========================================================================
     C     Write         Begsr
     C                   Z-Add     SVFND         XXFND             3 0
     C                   Z-Add     SVEMP#        XXEMP#            9 0
     C                   Z-Add     SVFYR         XXFYR             3 0
     C                   Z-Add     SvDiv         XXDIV             5 0
      *
     C     ELKY          Chain     PEL01                              60
      *
     C                   Z-Add     XXCO#         @@CO#
     C                   Z-Add     XXFND         @@FND
     C                   Z-Add     XXEMP#        @@EMP#
     C                   Z-Add     XXDIV         @@DIV
     C                   Z-Add     XXFYR         @@FYR
      *** beg chg ***** 11/16/04 ***********************************************
     C*****************  Z-Add     ELPTDT        SRDTI
     C*****************  Call      'SRCVTDAT'    DTMDYY
     C*****************  Movel     SRDTO         @@DATE
     c                   eval      @@Date = dateTo8(cvtToDate(ELPTDT))
      *** end chg ***** 11/16/04 ***********************************************
     C                   Movel     '*'           @@PLAN
      * API to gather Agent and Producer information
     C                   Call      'WA002R'
     C                   Parm                    @@CO#             3 0
     C                   Parm                    @@FND             3 0
     C                   Parm                    @@EMP#            9 0
     C                   Parm                    @@DIV             5 0
     C                   Parm                    @@FYR             3 0
     C***   Begin Chg  ***  01/10/18  *************************************
     C***********        PARM                    @@AGT#            5 0
     C***********        PARM                    @@ANAM           60
     C                   PARM      *zeros        @@AGT#            5 0
     C                   PARM      *blanks       @@ANAM           60
     C***   End   Chg  ***  01/10/18  *************************************
     C                   Parm                    @@PROD            5 0
     C                   Parm                    @@PNAM           60
     C                   Parm                    @@AREA            3 0
     C                   Parm                    @@PHON            7 0
     C                   Parm                    @@PLAN            1
     C                   Parm                    @@DATE            8 0
      *
     C     @@PLAN        Ifeq      'A'
     C                   Z-Add     0             XXPREM           11 2
     C     abky          Chain     PAB01                              60
     C     *IN60         DOWEQ     *OFF
     C     ABFYR         Ifeq      XXFYR
     C     ABARTY        ANDEQ     'P'
     C                   ADD       ABAMT         XXPREM
     C                   Endif
     C     abky          READE     PAB01                                  60
     C                   Enddo
      *
     C                   SUB       XXPREM        XXAMTD
      *
     C     XXPREM        IFLE      XXCURR
      *
     C     XXPREM        IFGT      XX90
     C                   Z-Add     0             XX90
     C                   SUB       XX90          XXPREM
     C                   ELSE
     C                   SUB       XXPREM        XX90
     C                   Z-Add     0             XXPREM
     C                   Endif
      *
     C     XXPREM        IFGT      XX60
     C                   Z-Add     0             XX60
     C                   SUB       XX60          XXPREM
     C                   ELSE
     C                   SUB       XXPREM        XX60
     C                   Z-Add     0             XXPREM
     C                   Endif
      *
     C     XXPREM        IFGT      XX30
     C                   Z-Add     0             XX30
     C                   SUB       XX30          XXPREM
     C                   ELSE
     C                   SUB       XXPREM        XX30
     C                   Z-Add     0             XXPREM
     C                   Endif
      *
     C     XXPREM        IFGT      XX00
     C                   Z-Add     0             XX00
     C                   SUB       XX00          XXPREM
     C                   ELSE
     C                   SUB       XXPREM        XX00
     C                   Z-Add     0             XXPREM
     C                   Endif
      *
     C                   ELSE
      *
     C     abky          Chain     PAB01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     ABFYR         Ifeq      XXFYR
     C     ABARTY        ANDEQ     'P'
      *
     C                   SELECT
     C     ABPODT        WHENLE    XX90DT
     C                   SUB       ABAMT         XX90
     C     ABPODT        WHENLE    XX60DT
     C                   SUB       ABAMT         XX60
     C     ABPODT        WHENLE    XX30DT
     C                   SUB       ABAMT         XX30
     C                   OTHER
     C                   SUB       ABAMT         XX00
     C                   ENDSL
      *
     C                   Endif
      *
     C     abky          READE     PAB01                                  60
     C                   Enddo
      *
     C                   Endif                                                  XXPREM IFLE
      *
     C                   Endif                                                  @@PLAN Ifeq
      *
     C                   Move      PGID          GEPGID
      *
     C     RPKY          Chain     PRP01                              60
     C     *IN60         Ifeq      *OFF
     C     RPCAP#        ANDNE     *BLANKS
     C                   Movel     RPCAP#        GEPOL
     C                   ELSE
     c                   Move      XXFND         X1FND
     C                   Move      XXEMP#        X1EMP#
     C                   Move      XXFYR         X1FYR
     C                   Movel     XXPOL         GEPOL
     C                   Endif
      *
     C                   Clear                   GeEfdt
      *
     C                   Exsr      Eff_Date_St
      *
     C     Firstefd      Ifne      *Zero
     C     *YMD          MOVE      Firstefd      DATUSA
     C                   MOVE      DATUSA        GeEfdt
     C                   ENDIF
      *
     C     *YMD          Move      XXTDT         DATUSA
     C                   Move      DATUSA        GEEOMD
      *
     C                   If        Xxcurr < *Zero and
      **** BEGIN CHANGE * 11/07/02 *****************************************************************
     C*                            AgAudf <> 'A'
     C                             XxAudf <> 'A'
      **** END CHANGE *** 11/07/02 *****************************************************************
     C                   Eval      Xxadpr = Xxcurr
     C                   Else
     C                   Eval      Xxadpr = *Zero
     C                   Endif
      *
     C                   Eval      Hold20 = XxAmtd
     C                   Eval      Geamtd = %editc(Hold20:'P')
     C                   Eval      GeAmtd# = Hold20
      *
     C                   Eval      Hold20 = XxAdpr
     C                   Eval      GeAdpr = %editc(Hold20:'P')
     C                   Eval      GeAdpr# = Hold20
      *
     C                   Eval      Hold20 = XxCurr
     C                   Eval      Gecurr = %editc(Hold20:'P')
     C                   Eval      Gecurr# = Hold20
      *
     C                   Eval      Hold20 = Xx00
     C                   Eval      Geam00 = %editc(Hold20:'P')
     C                   Eval      Geam00# = Hold20
      *
     C                   Eval      Hold20 = Xx30
     C                   Eval      Geam30 = %editc(Hold20:'P')
     C                   Eval      Geam30# = Hold20
      *
     C                   Eval      Hold20 = Xx60
     C                   Eval      Geam60 = %editc(Hold20:'P')
     C                   Eval      Geam60# = Hold20
      *
     C                   Eval      Hold20 = Xx90
     C                   Eval      Geam90 = %editc(Hold20:'P')
     C                   Eval      Geam90# = Hold20
      *
     C                   Eval      Hold20 = XxDeff
     C                   Eval      Gedeff = %editc(Hold20:'P')
     C                   Eval      Gedeff# = Hold20
      *
     C                   Move      SVRST         GERST
     C                   Eval      Gefyr  = Xxfyr
     C                   Eval      Gefnd  = Xxfnd
     C                   Eval      Geemp# = Xxemp#
     C                   Write     Age01
     C                   Exsr      WrPolm
     C                   CLEAR                   AGE01
      *
     C                   Endsr                                                  WRITE SUBR
     *=====================================================================
     *  Get Earliest Effective Date for State /Year
     *=====================================================================
     C     Eff_Date_St   Begsr
      *
     C                   Clear                   FirstFyr
     C                   Clear                   FirstEfd
      *
     C     Wdell2_Key    Setll     Wdell2
      *
     C                   Dou       %eof(Wdell2)
      *
     C     Wdell2_Key    Reade     Wdell2
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   If        L2Fyr <> Svfyr
     C                   Iter
     C                   Endif
      *
     C                   Eval      Kyrtcd = L2rtcd
     C     Wmrsl_Key     Chain     Wmrsl
      *
     C                   If        %Found
      *
     C                   If        RsJrst = Svrst
      *
     C                   If        L2pfdt < FirstEfd or
     C                             FirstEfd = *Zero
     C                   Eval      FirstEfd = L2Pfdt
     C                   Endif
      *
     C                   Endif
     C                   Endif
      *
     C                   Enddo
      *
     C                   Endsr
      *==============================================================
      * WRPOLM - Write to Policy Master File
      *==============================================================
     C     WRPOLM        BEGSR
      *
     C                   Movel(P)  GEFND         ##POL
     C                   Movel(P)  GEEMP#        XXTMP            10
     C                   CAT(P)    XXTMP:0       ##POL
     C                   Movel(P)  GEFYR         XXTMP            10
     C                   CAT(P)    XXTMP:0       ##POL
      *
     C                   Call      'W8126R2'
     C                   Parm      QQCO#         ##CO#             3
     C                   Parm      QQTDT         ##TODT            8
     C                   Parm                    ##POL            15
     C                   Parm      SvDiv         ##Div             5 0
      *
     C                   ENDSR                                                  WRPOLM
      *==============================================================
      *   SRPAST - SUBROUTINE TO CALCULATE A DATE IN THE PAST    *
      *==============================================================
     C     SRPAST        BEGSR
     **
     C                   Z-Add     XXDAYS        SRPST             5 0
      *
     C                   Move      QQTDT         SRDATE            8 0
      * Calculate a date in the past
     C                   Call      'SRPAST8'
     C                   Parm                    SRDATE
     C                   Parm                    SRPST
     C                   Parm      *BLANK        SRERR             1
     C                   Parm      *BLANK        SRTERM            1
      *
     C                   ENDSR
      *** beg add ***** 11/16/04 **********************************************
      *========================================================================
      *   SRCXBAL- Match Written Premium to A/R if canx'd and not audited
      *========================================================================
     c     srCxBal       begsr

     c                   exsr      srCxFlg

     c                   if        cxFlag = *on

     c                   eval      estWrtnPrem  = 0
     c                   eval      estRecvdAmt  = 0
     c                   eval      kktrdt       = 0
     c                   eval      cxDiff       = 0
      *
      * Get Written Premium from WFEUP
      *
      *** beg add ***** 06/22/07 ***************************************
     c                   if        ELARSM <> 'Y'
      *
      *  Roll through single division to get written premium if not summarized
      *
      *** end add ***** 06/22/07 ***************************************
     c     euky1         setll     peu01
     c     euky          reade     peu01
     c                   dow       not %eof
     c                   add       EUPYWR        estWrtnPrem
     c                   add       EUYDWR        estWrtnPrem
     c     euky          reade     peu01
     c                   enddo
      *** beg add ***** 06/22/07 ***************************************
     c                   else
      *
      *    Roll through all summarized divisions and accumuate written premium if
      *    this is the state reporting division.
      *
     c     emky          chain     pem01
     c                   if        %found(WMEML) and
     c                             EMSTRP = 'Y'

     c     eukyl1        setll     peu0l1
     c     eukyl1        reade     peu0l1

     c                   eval      s1div  = -1
     c                   dow       not(%eof(WFEUL1))

     c                   if        EUDIV  <> s1div
     c     elky1         chain     pel01
     C                   movel(p)  ELBSCD        tab10
     C     bsky          chain     pbs01
     c                   eval      s1div  = EUDIV
     c                   endif                                                  EUDIV <> S1DIV

     c                   if        ELARSM = 'Y'  and BSSEFL = 'E'
     c                   add       EUPYWR        estWrtnPrem
     c                   add       EUYDWR        estWrtnPrem
      *** beg add ***** 10/23/07 *******************************************************
     c                   add       ELPRTD        svprtd
      *** end add ***** 10/23/07 *******************************************************
     c                   endif                                                  ELARSM = 'Y'

     c     eukyl1        reade     peu0l1
     c                   enddo

     c                   endif                                                  EMSTRP = 'Y'

     c                   endif                                                  ELARSM <> 'Y'
      *** end add ***** 06/22/07 ***************************************

     c                   if        estWrtnPrem <> 0
     c                   exsr      srProEx
     c                   add       proExCon      estWrtnPrem
      *
      * Get Premium Cash from WMEAP
      *
     c     eaky4         setll     pea04
     c     eaky4         reade     pea04
     c                   dow       not %eof

     c                   eval      kkkey = eaarcd
     c     ttky          chain     ptt01
     c                   if        %found

     c                   if        (TTTYPE = 'P' or
     c                              TTTYPE = 'T')
     c                   add       EAAMT         estRecvdAmt
     c                   endif
     c
     c                   if        (TTTYPE = 'N' or
     c                              TTTYPE = 'R' or
     c                              TTTYPE = 'V')
     c                   sub       EAAMT         estRecvdAmt
     c                   endif

     c                   endif                                                  %found

     c     eaky4         reade     pea04
     c                   enddo
      *
      * Get the difference between written premium and the sum of cash + total amount due
      *
     c                   eval      cxDiff = estWrtnPrem - estRecvdAmt - AGTLDU

     c                   if        cxDiff < 0
     c                   eval      cxDiff = cxDiff * -1
     c                   eval      negFlag = *on
     c                   else
     c                   eval      negFlag = *off
     c                   endif
      *
      * Disperse the difference across the deferred and current amount due. The preprocessor
      * checks that total due = current due + deferred and that current due = sum of the aging
      * buckets.
      *
     c                   if        cxDiff <> 0

     c                   if        negFlag = *on

     c                   if        cxDiff > AGDEFR
     c                   sub       AGDEFR        cxDiff
     c                   sub       AGDEFR        xxdeff
     c                   sub       AGDEFR        xxamtd
     c                   else
     c                   sub       cxDiff        xxdeff
     c                   sub       cxDiff        xxamtd
     c                   eval      cxDiff = 0
     c                   endif                                                  cxDiff > AGDEFR

     c                   if        cxDiff <> 0
     c                   sub       cxDiff        xx00
     c                   sub       cxDiff        xxcurr
     c                   sub       cxDiff        xxamtd
     c                   endif                                                  cxDiff <> 0 n

     c                   else
     c                   add       cxDiff        xxdeff
     c                   add       cxDiff        xxamtd
     c                   endif                                                  negFlag = *on

     c                   endif                                                  cxDiff <> 0

     c                   endif                                                  pendCxFlg = *on

     c                   endif                                                  estWrtnPrem <> 0

     c                   endsr
      *========================================================================
      *   SRCXFLG - Determine if policy is cancelled
      *========================================================================
     c     srCxFlg       begsr

     c                   eval      cxFlag = *off
     c     emky          chain     pem01
     c                   if        %found(WMEML)
     c                   eval      cxDate = 0

     c                   select
     c                   when      EMCAD5 <> 0
     c                   eval      cxDate = EMCAD5
     c                   when      EMCAD4 <> 0
     c                   eval      cxDate = EMCAD4
     c                   when      EMCAD3 <> 0
     c                   eval      cxDate = EMCAD3
     c                   when      EMCAD2 <> 0
     c                   eval      cxDate = EMCAD2
     c                   when      EMCAD1 <> 0
     c                   eval      cxDate = EMCAD1
     c                   endsl

     c                   if        cxDate <> 0 and
     c                             cxDate <= xxtdt and
     c                             cxDate >= ELPFDT and
     c                             cxDate <= ELPTDT
     c                   eval      cxFlag = *on
     c                   else
     c                   eval      cxFlag = *off
     c                   endif

     c                   endif                                                  %found(WMEML)

     c                   endsr
      *========================================================================
      *   SRPROEX - Prorate Expense Constant
      *========================================================================
     c     srProEx       begsr

     c     ehky1         setgt     peh01
     c     ehky          readpe    peh01
     c                   if        not(%eof)
     c                   eval      polFrDt  = dateTo8(cvtToDate(EHPFDT))
     c                   eval      polToDt  = dateTo8(cvtToDate(EHPTDT))
     c                   eval      numPolicyDays = dtDiff(polFrDt:polToDt)
     c                   eval      numCanxDays   = dtDiff(polFrDt:cxDate)
     c                   eval(h)   proRateFactor = numCanxDays/numPolicyDays
      *** beg chg ***** 08/30/07 **********************************************
     c*****************  eval(h)   proExCon = ELPRTD * proRateFactor
     c                   eval(h)   proExCon = svprtd * proRateFactor
      *** end chg ***** 08/30/07 **********************************************
     c                   else
     c                   eval      proExCon = 0
     c                   endif

     c                   endsr
      *** end add ***** 11/16/04 **********************************************
