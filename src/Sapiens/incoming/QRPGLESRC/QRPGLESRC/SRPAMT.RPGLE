     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE SRPAMT - Calculate Endorsement Premium
     H*****************************************************************
     H*
     H*  Calculate premium amount to print in endorsement footer.
     H*
     H*  CALLED FROM: Endorsement Attachment programs
     H*
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 07/14/21 C:65  Project: Phw#91687 (Sunz)
     H*                Calculate premium in same manner as policy page
     H*                to return for endorsements.  If LDA has the premium
     H*                value available, use it, otherwise calculate here.
     H*
     H* 11/07/11 C:190 Project: WC2022.
     H*                Recompiled.
     H*
     H* 01/09/08 C:61  SUPPORT WA132 66328
     H*                Recompile with the following create options:
     H*                  "DFTACTGRP(*NO) ACTGRP(QILE) BNDDIR(SRPAMT)"
     H*
     H* 04/18/08 C:61  project wc1537
     H*                If state reporting division has coverage in FL,
     H*                then include expense constant in premium amount.
     H*
     H* 12/05/06 C:33  Seq#: WA135/55506
     H*                Do not accumulate premium for cx'd accounts.
     H*
     H* 09/11/06 C:61  SUPPORT WA132 54290 132WEW
     H*                Created.
     H*****************************************************************
      *
      *  Employer Policy Year
     Fwdell1    if   e           k disk    usropn
      ****** beg add ******* 04/18/08 *********************************
      *
      *  Rate Set
     Fwmrsl     if   e           k disk
      ****** end add ******* 04/18/08 *********************************
      *
     D***  Begin Add  ***  12/05/06  **************************************
     D cxDate7         s              7  0
     D endEffDt7       s              7  0
     D midTermEnd      s              1n
     D useDiv          s              1n
     D sysDt7          s              7  0
     D***  End   Add  ***  12/05/06  **************************************
      ****** beg add ******* 04/18/08 *********************************
     D xxstate         s              2
      ****** end add ******* 04/18/08 *********************************
      *
     D psds          esds                  extname(szq1p)
     D*
     D***  Begin Add  ***  12/05/06  **************************************
     D svLDA           ds          1024
     D LDEND#                         5  0
     D  LDENDA                        5    overlay(ldend#)
     D LDMYRR                         9  0
     D LDEEDT                         8  0
     D LDFYR                          3  0
     D LDPYRL                         1
     D LDPBCG                         1
     D LDOWCG                         1
     D*** begin add ***  07/14/21  ****************************************
     D ldEndrPrem                     9  0 Overlay(svlda:31)
     D*** end add *****  07/14/21  ****************************************
     D*
     D MidTermRun      pr             1n
     D*
     D/copy sPrDate
     D/copy wPrEmplr
      ****** beg add ******* 04/18/08 *********************************
     D/copy wPrRateSet
      ****** end add ******* 04/18/08 *********************************

     C     *dtaara       define    *lda          svLDA
     C***  End   Add  ***  12/05/06  **************************************
      *
      *
      *================================================================
      *  PLIST definitions
      *================================================================
     c     *entry        plist
     c                   parm                    @@co#             3 0
     c                   parm                    @@fnd             3 0
     c                   parm                    @@fyr             3 0
     c                   parm                    @@emp#            9 0
     c                   parm                    @@prem           11 2
     c                   parm                    @@clr             1
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
      ****** beg add ******* 04/18/08 *********************************
      *
      *  Employer Policy Year
      *
     c     rsky          klist
     c                   kfld                    @@co#
     c                   kfld                    xxrtcd            3
      ****** end add ******* 04/18/08 *********************************
      *
      *  Employer Policy Year
      *
     c     elky          klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@emp#
      *
      *
      *================================================================
      *  MAIN
      *================================================================
     C*
      ****** beg add ******* 04/18/08 *********************************
      *
      * Get coverage state of state reporting division
      *
     C                   eval      xxState = RatingSt(@@CO#:
     C                                         (GetRateSet(
     C                                           GetEmpYrRR(@@FYR:@@EMP#:
     C                                             GetStRptDv(@@CO#:@@FND:
     C                                                        @@EMP#):
     C                                             @@FND:@@CO#))))
      *
      ****** end add ******* 04/18/08 *********************************
     C***  Begin Add  ***  12/05/06  **************************************
     C                   eval      sysDt7 = DateTo7(SystemDate)
     C*
     C                   eval      midTermEnd = MidTermRun
     C***  End   Add  ***  12/05/06  **************************************
      *
     c                   exsr      getprem
      *
     c                   eval      *inlr = *on


      *================================================================
      *  getprem - get premium for policy
      *================================================================
     c     getprem       begsr

     c                   z-add     0             @@prem
     C*** begin add ***  07/14/21  ****************************************
      /free

        Monitor;
           In svlda;
           Out svlda;
           @@prem = ldEndrPrem;
           If @@Prem <> 0;
              Leavesr;
           Endif;
        On-error;
        Endmon;

      /end-free
     C*** end add *****  07/14/21  ****************************************
     c                   open      wdell1
     c     elky          setll     pel01
     c     elky          reade     pel01

     c                   dow       not %eof
     C***  Begin Add  ***  12/05/06  **************************************
     C                   eval      cxDate7 =
     C                              GetCxDate(ELCO#:ELFND:ELEMP#:ELDIV)
     C*
     C                   eval      useDiv = cxDate7 = *zero
     C                             or cxDate7 > sysDt7
     C                             or midTermEnd and cxDate7 > endEffDt7
     C*
     C                   if        useDiv
     C***  End   Add  ***  12/05/06  **************************************
     C*** begin chg ***  07/14/21  ****************************************
     c******             Eval      @@prem = @@prem + elenmp + eleter
     c******                                + eledtr + elearc
      ****** beg add ******* 04/18/08 *********************************

     c******             if        xxstate = 'FL'
     c******             eval      @@prem = @@prem + elprtf
     c******             endif
     C                   Eval      @@prem = @@prem + elanpr
     C*** end chg *****  07/14/21  ****************************************

      ****** end add ******* 04/18/08 *********************************
     C***  Begin Add  ***  12/05/06  **************************************
     C                   endif
     C***  End   Add  ***  12/05/06  **************************************
     c     elky          reade     pel01
     c                   enddo

     c                   close     wdell1
      *
     c                   endsr


     P***  Begin Add  ***  12/05/06  **************************************
     P*====================================================================
     P* Determine if job is being called from Mid-term Endorsements
     P*====================================================================
     PMidTermRun       b
     D MidTermRun      pi             1n
     C     *lock         in        *dtaara
     C                   out       *dtaara
    C*
     C                   if        (LDPBCG = *off or LDPBCG = ' ') and
     C                             (LDOWCG = *off or LDOWCG = ' ')
     C                   clear                   LDEEDT
     C                   endif
     C*
     C                   eval      endEffDt7 = DtCYMD(LDEEDT)
    C*
     C                   if        LDPBCG = *on or LDOWCG = *on
     C                   return    *on
     C                   endif

     C                   return    *off

     PMidTermRun       e
     P***  End   Add  ***  12/05/06  **************************************
