     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/Title - Build Return Fund Batch Detail
     H DATFMT(*USA)                                                                                *
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H optimize(*none)
      **********************************************************************************************
      *  DESCRIPTION:  Build Return Fund Batch File                                             *
      *                                                                                            *
      *  CALLS TO:                                                                                 *
      *                                                                                            *
      *  CREATED: 03/12/2003  C:136/C:142  WC1033                                                  *
      **********************************************************************************************
      *  REVISIONS:                                                                                *
     *                                                                                            *
      * 08/26/14 C:298 Project: SY354                                                              *
      *                -Implement Routing Page Changes.                                            *
      *                                                                                            *
      *  07/16/11  C:09   Support: 77219                                                           *
      *                   Add code to prevent duplicate entries from being written as part of the  *
     *                   report output.                                                           *
     *                                                                                            *
      *  04/11/11  C:180  Support 76457                                                            *
      *                   Fix error: Missing agent information                                     *
      *                                                                                            *
      *  01/28/10  C:249  Project: SY215                                                           *
      *            C:189  Retrieve brand Name                                                      *
      *                                                                                            *
     *  07/18/07  C:136  Project: WC1491                                                          *
     *                   Implement Return Deposit logic.                                          *
     *                                                                                            *
      *  03/08/06  C:132  PROJECT: WC071 - 51160                                                   *
      *                   Change *DATE to use The Current Date from DSDTTM                         *
      *                   Data Structure DSDATE.                                                   *
      *                                                                                            *
      *  11/05/04  C:33   Project: WC1033                                                          *
      *                   Include calculations for Agency Billing.                                 *
      **********************************************************************************************
     * Mass Return Premium Batch Control File
     FWxrnl     uf   e           k disk
      *** beg chg ***** 07/18/07 ******************************************
     * Return Premium Batch Detail File
     F********* Wxrtl1    IF   E           K Disk
     F********* Wxrtl     O    E           K Disk    Rename(PRT01:PRT02)
     F*********                            Prefix(R2:2)
     Fwxrtl     if a e           k disk    rename(prt01:prt02)
     F                                     prefix(R2:2)
     Fwxrtl1    uf   e           k disk
      *** end chg ***** 07/18/07 ******************************************
     FWxrtl5    if   E           K Disk    Rename(PRT01:PRT05)
     F                                     Prefix(R5:2)
     F***  Begin Add  ***  11/05/04  **************************************
     FWxrtl3    if   e           k disk    rename(prt01:PRT03) prefix(l3_)
     FWxrtl6    if   e           k disk    rename(prt01:prt06) prefix(l6_)
     F***  End   Add  ***  11/05/04  **************************************
     * Fund master file
     FWmfnl     IF   E           K Disk
     * Employer Detail File
     FWdell     IF   E           K Disk
     * Employer Accounts Receivable File
     Fwmeal20   if   e           k disk
     FWmeal4    IF   E           K Disk    Rename(PEA01:PEA02)
     F                                     Prefix(L4:2)
     F***  Begin Add  ***  11/05/04  **************************************
    F* Agency Billing Detail File
     Fwdabl12   if   e           k disk    rename(pab01:pab12)
     Fwdabl11   if   e           k disk    rename(pab01:pab11) prefix(l11_)
     F***  End   Add  ***  11/05/04  **************************************
     * Employer Master
     FWmeml     IF A E           K Disk
     * Return Premium Check Request File
     FWar4l3    IF   E           K Disk
     F***  Begin Add  ***  11/05/04  **************************************
     FWar4l10   if   e           k disk    rename(pr401:pr410)
     F***  End   Add  ***  11/05/04  **************************************
     * Premium Finance Company File
     FWmpfl     IF   E           K Disk
     * Banking Master File
     Fwsckl     if   e           k disk
      *** beg add ***** 07/18/07 ******************************************
     Fwdf1l     if   e           k disk
      *** end add ***** 07/18/07 ******************************************
     * Routing
     F***  Begin Add  ***  07/16/11  **************************************
     Fsmorl     if   e           k disk
     Fwxr8l     if   e           k disk
     FWxr8p     o    e           K Disk    Rename(pr801:pro01)
     F                                     Prefix(RO:2)
     F***  End Add ******  07/16/11  **************************************
     Fqsysprt   o    f  132        printer oflind(*inof)
     Fqsysprt1  o    f  132        printer oflind(*inoa)
     *=====================================================================
     * Stand Alone Fields - TOP
     *=====================================================================
     D Acc_Cur         s              1N
     D In_Batch        s              1N
     D***  Begin Add  ***  11/05/04  **************************************
     D agencyBill      s              1n
     D***  End   Add  ***  11/05/04  **************************************
     D Balance         s             11  2
     D svfnd           s                   like(EAFND)
     D svemp#          s                   like(EAEMP#)
     D svdiv           s                   like(EADIV)
     D svfyr           s                   like(EAFYR)
     D***  Begin Add  ***  11/05/04  **************************************
     D svfein          s                   like(ABFEIN)
     D svmod           s                   like(ABMOD)
     D***  End   Add  ***  11/05/04  **************************************
     D selTotal        s             15  2 inz(0)
     D selCount        s              5  0 inz(0)
     D notSelTotal     s             15  2 inz(0)
     D notSelCount     s              5  0 inz(0)
     D FromDt          s               D   DATFMT(*USA)
     D ThruDt          s               D   DATFMT(*USA)
     D Datymd          s               D   DATFMT(*YMD)
     D xxfil#          s                   like(ORFIL#)
     D UPUSNM          s                   like(userName)
     D xxque           s                   like(qqque)
     D ORTITL1         s                   like(ORTITL)
     D RptTitle        s                   like(ORTITL)
     D RptTitle1       s                   like(ORTITL)
     D kkacct          s                   like(CKACCT)
     D***  Begin Add  ***  11/05/04  **************************************
     D ag_rrn          s              9  0
     D aa_rrn          s              9  0
     D***  End   Add  ***  11/05/04  **************************************
     D userName        s             50
     D companyNm       s             60
      ** beg chg ***** 07/18/07 *******************************************
     D**********     title           c                   'Return Premium Batch Selected'
     D**********     title1          c                   'Return Premium Batch Not Selected'
     D title           c                   'Return Fund Batch Selected'
     D title1          c                   'Return Fund Batch Not Selected'
     d depModeFlag     s               n
      ** end chg ***** 07/18/07 *******************************************
     D first           s               n
     D first1          s               n
     D arCodeErr       s               n
     D excluded        s               n
     D sys_date        s               d   datfmt(*usa)
     D sys_time        s               t   timfmt(*hms:)
     D line            s            128    inz(*all'_')
     D                 DS
     D  Dsdttm                 1     12  0
     D  Dstime                 1      6  0
     D  Dsdate                 7     12  0
      ***** BEG CHG ***** 03/08/06 *******************************************************
     D                 DS
     D  xxdttm                       14  0
     D  xxtime                        6  0 overlay(xxdttm)
     D  xxdate                        8  0 overlay(xxdttm:7)
      ***** END CHG ***** 03/08/06 *******************************************************
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
     D Psds          esds                  extname(szq1p)
     D Fids01        e ds                  extname(szq2p)
      *=====================================================================
      * PROCEDURES
      *=====================================================================
     D/copy sprcompany
     D/copy sprstring
     D/copy wprusrprms
     D***  Begin Add  ***  11/05/04  **************************************
     D/copy wpragent
     D***  End   Add  ***  11/05/04  **************************************
     D*** beg add ***** 07/18/07 ******************************************
     D/copy wprar
     D getDepBalance   pr
     D getPrmBalance   pr
     D getDepMode      pr              n
     d  co#                                like(EACO#)
     d  fnd                                like(EAFND)
     d  fyr                                like(EAFYR)
     D*** end add ***** 07/18/07 ******************************************
     D exclude         pr              n
      *=====================================================================
      * KLIST
      *=====================================================================
     C     ckky          klist
     C                   kfld                    fnco#
     C                   kfld                    kkacct
    C*
     C     orky          klist
     C                   kfld                    q1pgm
     C                   kfld                    xxfil#
    C*
     C     pfky          klist
     C                   kfld                    eaco#
     C                   kfld                    eafnd
     c                   kfld                    elpfin
    C*
     C     pfky1         klist
     C                   kfld                    eaco#
     C                   kfld                    eafnd
     c                   kfld                    elpfin
     c                   kfld                    sysdate
    C*
     C     Wxrnl_Key     Klist
     C                   Kfld                    QQco#
     C                   Kfld                    Batch#
      *
     C     xr1key        Klist
     C                   Kfld                    Eaco#
     C                   Kfld                    Batch#
     C                   Kfld                    Eafnd
     C                   Kfld                    Eaemp#
     C                   Kfld                    Eadiv
     C                   Kfld                    Eafyr
      *
     C     xr5key        Klist
     C                   Kfld                    Eaco#
     C                   Kfld                    Eafnd
     C                   Kfld                    Eaemp#
     C                   Kfld                    Eadiv
     C                   Kfld                    Eafyr
     C                   Kfld                    kkbat#
      *
     C     xr5key2       Klist
     C                   Kfld                    Eaco#
     C                   Kfld                    Eafnd
     C                   Kfld                    Eaemp#
     C                   Kfld                    Eadiv
     C                   Kfld                    Eafyr
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C     keyWXRTL3     klist
     C                   kfld                    batch#
     C                   kfld                    ABCO#
     C                   kfld                    ABFEIN
     C                   kfld                    ABMOD
     C*
     C     keyWXRTL6     klist
     C                   kfld                    ABCO#
     C                   kfld                    ABFEIN
     C                   kfld                    ABMOD
     C                   kfld                    kkbat#
     C***  End   Add  ***  11/05/04  **************************************
      *
     C     War4l3_Key    Klist
     C                   Kfld                    Eaco#
     C                   Kfld                    Eafnd
     C                   Kfld                    Eaemp#
     C                   Kfld                    Eadiv
     C                   Kfld                    Eafyr
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C     keyWAR4L10    Klist
     C                   Kfld                    ABCO#
     C                   Kfld                    ABFEIN
     C                   Kfld                    ABMOD
     C***  End   Add  ***  11/05/04  **************************************
      *
     C     eaky          klist
     C                   kfld                    qqco#
     C                   kfld                    fnfnd
      *
     C     eaky1         klist
     C                   kfld                    qqco#
     C                   kfld                    fnfnd
     C                   kfld                    rnfaud

     C     Wmeal4_key    Klist
     C                   Kfld                    QQco#
     C                   Kfld                    Fnfnd
     C                   Kfld                    Eaemp#
     C                   Kfld                    Eadiv
     C                   Kfld                    Eafyr
     *
     C     Wmeml_Key     Klist
     C                   Kfld                    R2co#
     C                   Kfld                    R2fnd
     C                   Kfld                    R2emp#
     C                   Kfld                    R2div
     *
     *
     C***  Begin Add  ***  11/05/04  **************************************
     C     keyWDABL12    klist
     C                   kfld                    qqco#
     C                   kfld                    FNFND
      *
     C     key1WDABL12   klist
     C                   kfld                    qqco#
     C                   kfld                    FNFND
     C                   kfld                    RNFAUD
      *
     C     keyWDABL11    klist
     C                   kfld                    qqco#
     C                   kfld                    ABFEIN
     C                   kfld                    ABMOD
     C***  End   Add  ***  11/05/04  **************************************
     *
     C     fnky          klist
     C                   kfld                    fnco#
     C                   kfld                    fnfnd
     *
     C     Wmfnl_Key     Klist
     C                   Kfld                    QQco#
     C                   Kfld                    Fromfn
      *
     C     Wmfnl_Key2    Klist
     C                   Kfld                    QQco#
      *
     C     Wdell_Key     Klist
     C                   Kfld                    Eaco#
     C                   Kfld                    Eafnd
     C                   Kfld                    Eaemp#
     C                   Kfld                    Eadiv
     C                   Kfld                    Eafyr
      *
      *** beg add ***** 07/18/07 *******************************************************
     C     rtl1key       klist
     C                   kfld                    R2CO#
     C                   kfld                    R2BAT#
     C                   kfld                    R2FND
     C                   kfld                    R2EMP#
     C                   kfld                    R2DIV
      *
     C***  Begin Add  ***  07/16/11  **************************************
     C     r8ky          Klist
     C                   Kfld                    X8co#
     C                   Kfld                    X8fnd
     C                   Kfld                    X8emp#
     C                   Kfld                    X8div
     C                   Kfld                    X8fyr
     C***  End Add ******  07/16/11  **************************************
      *** beg add ***** 07/18/07 *******************************************************
      *
     C     *Like         Define    Rnco#         QQco#
     C     *Like         Define    Rnbat#        Batch#
     C     *Like         Define    FnFnd         Fromfn
     C     *Like         Define    FnFnd         Thrufn
     C     *Like         Define    Rnbat#        kkbat#
     F***  Begin Add  ***  07/16/11  **************************************
     C     *Like         Define    R8co#         X8co#
     C     *Like         Define    R8fnd         X8fnd
     C     *Like         Define    R8emp#        X8emp#
     C     *Like         Define    R8div         X8div
     C     *Like         Define    R8fyr         X8fyr
     C***  End Add ******  07/16/11  **************************************
      *
     C     *Entry        Plist
     C                   Parm                    QQCo              3
     C                   Parm                    QQBatch           9
     C                   Parm                    QQque            10
      *
     C     Dtcymd        Plist
     C                   Parm                    Srdti             8 0
     C                   Parm      *Blanks       Srffmt            8
     C                   Parm      '*CYMD'       Srtfmt            8
     C                   Parm                    Srdto            10
     C                   Parm                    Srerr             1
     *=====================================================================
     * M A I N L I N E   R O U T I N E                                    *
     *=====================================================================
     C                   Move      QQco          QQco#
     C                   Move      QQbatch       Batch#
     C     *hms          move      q1time        sys_time
      *
      ***** BEG CHG ***** 03/08/06 *******************************************************
     c**** *usa*******   move      *date         sys_date
     c                   TIME                    xxdttm
     c     *usa          move      xxDATE        sys_date                        mmddyyyy
      ***** END CHG ***** 03/08/06 *******************************************************

     C                   eval      companyNm = GetCompNam(QQCO#)
     C                   eval      companyNm = centerFld(companyNm)
      *
     C     Wxrnl_Key     Chain(n)  Wxrnl
     C                   If        %Found
     C                   Eval      FromFn = Rnfgrp
      *** beg chg ***** 07/18/07 ********************************************************
     C*****************  Eval      Thrufn = Rntgrp
     c                   if        rntgrp = *zero
     c                   eval      thrufn = *hival
     c                   else
     c                   eval      thrufn = rntgrp
     c                   endif
      *** end chg ***** 07/18/07 ********************************************************
     C     *Cymd         Move      Rnfaud        Fromdt
     C     *Cymd         Move      Rntaud        Thrudt
     C                   Exsr      Routing
     c                   else
     c                   seton                                        lr
     C                   Endif
      *
     C     Wmfnl_Key     Setll     Wmfnl
     C                   Dou       %Eof(Wmfnl)
     C     Wmfnl_Key2    Reade     Wmfnl
      *
     C                   If        %Eof or
     C                             Fnfnd > Thrufn
     C                   Leave
     C                   Endif
      *
     C                   Exsr      Get_Wmeap
     C***  Begin Add  ***  11/05/04  **************************************
     C* * Base CD Mar'05 Exsr      getWDABP
     C***  End   Add  ***  11/05/04  **************************************
      *
     C                   Enddo
      *
     C                   Exsr      UpdatWxrnp
      *
     C                   If        First  = *off
     C                   Except    Head
     C                   Except    NoRec
     C                   else
     C                   except    total
     C                   Endif
      *
     C                   If        First1 = *off
     C                   Except    Head1
     C                   Except    NoRec1
     C                   else
     C                   except    total1
     C                   Endif
      *
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxfil# = '0';
       Chain Orky Por01;
       If %found;
          Except Route;
       Endif;
       xxfil# = '1';
       Chain Orky Por01;
       If %found;
          Except Route1;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   Eval      *Inlr = *on
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     *=====================================================================
     * Detail Driver (process WDABP)
     *=====================================================================
     C
     C     getWDABP      begsr
     C*
     C                   eval      agencyBill = *on
     C                   eval      *inof = *on
     C*
     C     key1WDABL12   setll     pab12
     C                   dou       %eof(WDABL12)
     C     keyWDABL12    reade     pab12
     C*
     C                   select
     C                   when      %eof
     C                   leave
     C                   when      ABTRDT > RNTAUD
     C                   leave
     C                   when      ABARTY <> 'D'
     C                   iter
     C                   endsl
     C***  Begin Add  ***  07/16/11  **************************************
     C                   eval      x8co# = abco#
     C                   eval      x8fnd = abfnd
     C                   eval      x8emp# = abemp#
     C                   eval      x8div = abdiv
     C                   eval      x8fyr = abfyr
     C***  End Add ******  07/16/11  **************************************
     C*
     C                   exsr      Chk_Btch_Det
     C*
     C                   if        Not In_Batch
     C*
      *** beg chg ***** 07/18/07 ***********************************************
     C***********        exsr      Calc_Bal
     c                   callp     getPrmBalance
      *** end chg ***** 07/18/07 ***********************************************
     C*
     C                   select
     C*__________
     C*
     C                   when      balance < RNMIN and
     C                             balance > *zero
     C***  Begin Add  ***  07/16/11  **************************************
     C     R8ky          Chain     Wxr8l
     C                   If        not %found
     C***  End Add ******  07/16/11  **************************************
     C                   exsr      NoSelRpt
     C***  Begin Add  ***  07/16/11  **************************************
     C                   eval      ROco# = X8co#
     C                   eval      ROfnd = X8fnd
     C                   eval      ROemp# = X8emp#
     C                   eval      ROdiv = X8div
     C                   eval      ROfyr = X8fyr
     C                   Write     pRO01
     C                   Endif
     C***  End Add ******  07/16/11  **************************************
     C*__________
     C*
     C                   other
     C*
     C                   exsr      Chk_Oth_Bchs
     C                   exsr      Chk_Unissued
     C*
     C                   if        balance >= RNMIN
     C                   exsr      SelecRpt
     C                   exsr      Write_Wxrtp
     C                   endif
     C*
     C                   endsl
     C*
     C                   endif                                                  not_in_batch
     C*
     C                   enddo
     C*
     C                   eval      agencyBill = *off
     C*
     C                   endsr
     C***  End   Add  ***  11/05/04  **************************************
     C*
     *=====================================================================
     * Detail Driver (process Wmeap for fund)                             *
     *=====================================================================
     C     Get_Wmeap     Begsr
      *
     C     eaky1         setll     pea01
     C                   dou       %eof(wmeal20)
     C     eaky          reade     pea01
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   if        eatrdt > rntaud
     C                   leave
     C                   endif
      *
 ***  *** beg chg ***** 07/18/07 ***********************************************
     C***********        If        Eaarty <> 'D'
     C***********        Iter
     C***********        Endif
     c                   select
     c                   when      RNBTYP = 'P'
     C                   if        EAARTY <> 'D'
     C                   iter
     C                   endif
     c                   when      RNBTYP = 'D'
     c                   if        EAARTY <> 'H' and
     c                             GetARCAT(EAARCD) <> 'D'
     c                   iter
     c                   endif
     c                   endsl
      *** end chg ***** 07/18/07 ***********************************************
      *
      *
      *** beg del ***** 07/18/07 ***********************************************
      ***
      *** This was moved to speed up process. Only check account current if adding
      *** to batch.
      ***
     C*****************  Exsr      Chk_Acc_Curr
      *
     C*****************  If        Acc_Cur
     C*****************  Iter
     C*****************  Endif
      *** end del ***** 07/18/07 ***********************************************
      *
     C                   Exsr      Chk_Btch_Det
      *
     C                   If        Not In_Batch
      *
      *** beg add ***** 07/18/07 ***********************************************
     C                   Exsr      Chk_Acc_Curr
     C                   If        Acc_Cur
     C                   Iter
     C                   Endif
      *** end add ***** 07/18/07 ***********************************************
      *
      *** beg chg ***** 07/18/07 ***********************************************
     C**********         eval      excluded = exclude
     C**********         Clear                   Balance
     C**********         Exsr      Calc_Bal
     C**********         mult      -1            Balance
     C**********         if        balance < RNMIN or excluded
     C**********         if        balance > *zero
     C**********         exsr      NoSelRpt
     C**********         endif
     C**********         iter
     C**********         endif
     C                   clear                   balance
     C***  Begin Add  ***  07/16/11  **************************************
     C                   eval      x8co# = eaco#
     C                   eval      x8fnd = eafnd
     C                   eval      x8emp# = eaemp#
     C                   eval      x8div = eadiv
     C                   eval      x8fyr = eafyr
     C***  End Add ******  07/16/11  **************************************
     c                   select
     c                   when      rnbtyp = 'P'
     C                   eval      excluded = exclude
     C                   callp     getPrmBalance
     c                   when      rnbtyp = 'D'
     C                   callp     getDepBalance
     c                   endsl

     C                   mult      -1            balance
     c                   select
     c                   when      RNBTYP = 'P'
     C                   if        balance < RNMIN or excluded
     C                   if        balance > *zero
     C***  Begin Add  ***  07/16/11  **************************************
     C     R8ky          Chain     Wxr8l
     C                   If        Not %found
     C***  End Add ******  07/16/11  **************************************
     C                   exsr      NoSelRpt
     C***  Begin Add  ***  07/16/11  **************************************
     C                   eval      ROco# = X8co#
     C                   eval      ROfnd = X8fnd
     C                   eval      ROemp# = X8emp#
     C                   eval      ROdiv = X8div
     C                   eval      ROfyr = X8fyr
     C                   Write     pRO01
     C                   Endif
     C***  End Add ******  07/16/11  **************************************
     C                   endif
     C                   iter
     C                   endif
     c                   when      RNBTYP = 'D'
     C                   if        balance < RNMIN
     C                   if        balance > *zero
     C***  Begin Add  ***  07/16/11  **************************************
     C     R8ky          Chain     Wxr8l
     C                   If        Not %found
     C***  End Add ******  07/16/11  **************************************
     C                   exsr      NoSelRpt
     C***  Begin Add  ***  07/16/11  **************************************
     C                   eval      ROco# = X8co#
     C                   eval      ROfnd = X8fnd
     C                   eval      ROemp# = X8emp#
     C                   eval      ROdiv = X8div
     C                   eval      ROfyr = X8fyr
     C                   Write     pRO01
     C                   Endif
     C***  End Add ******  07/16/11  **************************************
     C                   endif
     C                   iter
     C                   endif
     c                   endsl
      *** end chg ***** 07/18/07 ***********************************************
      *
     C                   Exsr      Chk_Oth_Bchs
     C                   Exsr      Chk_Unissued
      *
     C                   If        Balance >= Rnmin and not excluded
     C                   Exsr      SelecRpt
     C                   Exsr      Write_Wxrtp
     C                   Endif
      *
     C                   Endif                                                  not_in_batch
      *
     C                   Enddo
      *
     C                   Endsr
     *=====================================================================
     *  Check if policy if account current                                *
     *=====================================================================
     C     Chk_Acc_Curr  Begsr
     C                   Eval      Acc_Cur = *Off
      *
      **** Beg change ***** 04/11/11 ***************************************
     c***                call      'WA002R'
     c***                parm      eaco#         ##co#             3 0
     c***                parm      eafnd         ##fnd             3 0
     c***                parm      eaemp#        ##emp             9 0
     c***                parm      eadiv         ##div             5 0
     c***                parm      eafyr         ##fyr             3 0
     c***                parm      *zero         ##agt             5 0
     c***                parm      *blank        ##anam           60
     c***                parm                    ##prod            5 0
     c***                parm                    ##pnam           60
     c***                parm                    ##area            3 0
     c***                parm                    ##phon            7 0
     c***                parm      '*'           ##plan            1
     c***                parm                    ##date            8 0
     c***                parm                    ##nwrt            5 0
     c***                parm                    ##rnrt            5 0
      ***
     c                   call      'WA002R'
     c                   parm      eaco#         ##co#             3 0
     c                   parm      eafnd         ##fnd             3 0
     c                   parm      eaemp#        ##emp             9 0
     c                   parm      eadiv         ##div             5 0
     c                   parm      eafyr         ##fyr             3 0
     c                   parm      *zero         ##agt             5 0
     c                   parm      *blank        ##anam           60
     c                   parm      *zeros        ##prod            5 0
     c                   parm      *blanks       ##pnam           60
     c                   parm      *zeros        ##area            3 0
     c                   parm      *zeros        ##phon            7 0
     c                   parm      '*'           ##plan            1
     c                   parm      *zeros        ##date            8 0
     c                   parm      *zeros        ##nwrt            5 0
     c                   parm      *zeros        ##rnrt            5 0
      **** End change ***** 04/11/11 ***************************************
     c                   if        ##plan = 'A'
     c                   eval      acc_cur = *on
     c                   endif
      *
     C                   Endsr
      *** beg del ***** 07/18/07 ******************************************
     *=====================================================================
     ***Calculate balance across all AR for given policy year             *
     *=====================================================================
     C*****Calc_Bal      Begsr
     C**********
     C*****Begin Add  ***  11/05/04  **************************************
     C**********         select
     C*__________
     C**********
     C**********         when      agencyBill
     C**********
     C**********         clear                   balance
     C**********
     C*****keyWDABL11    setll     pab11
     C**********         dou       %Eof(WDABL11)
     C*****keyWDABL11    reade     pab11
      **********
     C**********         select
     C**********         when      %Eof
     C**********         leave
     C**********         when      l11_ABARTY = 'A' or
     C**********                   l11_ABARTY = 'B' or
     C**********                   l11_ABARTY = 'D' or
     C**********                   l11_ABARTY = 'V' or
     C**********                   l11_ABARTY = 'N' or
     C**********                   l11_ABARTY = 'R'
     C**********         eval      balance = balance + l11_ABAMT
     C**********         when      l11_ABARTY = 'P' or
     C**********                   l11_ABARTY = 'T'
     C**********         eval      balance = balance - l11_ABAMT
     C**********         other
     C**********         endsl
      **********
     C**********         enddo
     C**********
     C**********         eval      balance = balance * -1
     C*__________
     C**********
     C**********         other
     C*****End** Add  ***  11/05/04  **************************************
      **********
     C*****Wmeal4_Key    Setll     Wmeal4
     C**********         Dou       %Eof(Wmeal4)
     C*****Wmeal4_Key    Reade     Wmeal4
      **********
     C**********         If        %Eof
     C**********         Leave
     C**********         Endif
      **********
     C**********         Select
     C**********         When      L4arty = 'A' or
     C**********                   L4arty = 'B' or
     C**********                   L4arty = 'D' or
     C**********                   L4arty = 'V' or
     C**********                   L4arty = 'N' or
     C**********                   L4arty = 'R'
     C**********         Eval      Balance = Balance + L4amt
     C**********         When      L4arty = 'P' or
     C**********                   L4arty = 'T'
     C**********         Eval      Balance = Balance - L4amt
     C**********         Other
     C**********         Endsl
      **********
     C**********         Enddo
     C**********
     C*****Begin Add  ***  11/05/04  **************************************
     C**********         endsl
     C*****End** Add  ***  11/05/04  **************************************
      **********
     C**********         Endsr
      *** end del ***** 07/18/07 ******************************************
     *=====================================================================
     *  Subtract amounts for policy in other batches                      *
     *=====================================================================
     C     Chk_Oth_Bchs  Begsr
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   select
     C*__________
     C*
     C                   when      agencyBill
     C*
     C                   eval      kkbat# = *loval
     C*
     C     keyWXRTL6     setll     prt06
     C*
     C                   dou       %Eof(WXRTL6)
     C*
     C     keyWXRTL6     reade     prt06
      *
     C                   select
     C                   when      %Eof
     C                   leave
     C*
     C                   when      RTBAT# = batch#
     C                   iter
     C*
     C                   other
     C                   eval      balance = balance - l6_RTAMT
     C*
     C                   endsl
     C*
     C                   enddo
     C*__________
     C*
     C                   other
     C***  End   Add  ***  11/05/04  **************************************
      *
     c                   eval      kkbat# = *loval
     C     xr5key        Setll     Wxrtl5
     C                   Dou       %Eof(Wxrtl5)
     C     xr5key2       Reade     Wxrtl5
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   If        RTBAT# = Batch#
     C                   iter
     C                   Endif
      *
     C                   Eval      Balance = Balance - R5amt
     C                   Enddo
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   endsl
     C***  End   Add  ***  11/05/04  **************************************
      *
     C                   Endsr
     *=====================================================================
     *  Write output file Wxrtp                                           *
     *=====================================================================
     C     Write_Wxrtp   begsr
      *
     C***  Begin Add  ***  11/05/04  **************************************
     C                   clear                   prt02
     C*
     C                   select
     C*__________
     C*
     C                   when      agencyBill
     C*
     C                   eval      R2CO#  = QQCO#
     C                   eval      R2BAT# = batch#
     C                   eval      R2FND  = *zero
     C                   eval      R2EMP# = *zero
     C                   eval      R2DIV  = *zero
     C                   Eval      R2EMPN = *blank
     C                   eval      R2AGT# = ABAGT#
     C                   eval      R2AGTN = GetAgtName(ABAGT#:ag_rrn:
     C                                                 aa_rrn:ABFND:ABCO#)
     C                   Eval      R2FEIN = ABFEIN
     C                   Eval      R2MOD  = ABMOD
     C                   eval      R2COM1 = RNCOM1
     C                   eval      R2COM2 = RNCOM2
     C                   eval      R2COM3 = RNCOM3
      *** beg add ***** 07/18/07 **********************************************
     C                   eval      R2BTYP = RNBTYP
      *** end add ***** 07/18/07 **********************************************
     C                   Eval      R2AMT  = balance
     C                   Eval      R2SELC = 'Y'
     C                   eval      R2ACFL = 'Y'
     C                   eval      R2PFIN = *blanks
     C                   eval      R2PTFL = 'A'
     C*__________
     C*
     C                   other
     C***  End   Add  ***  11/05/04  **************************************
     C*
     C                   eval      R2CO#  = QQCO#
     C                   eval      R2BAT# = batch#
     C                   eval      R2FND  = EAFND
     C                   eval      R2EMP# = EAEMP#
     C                   eval      R2DIV  = EADIV
      *
     C     Wmeml_Key     Chain     Wmeml
     C                   If        %Found(Wmeml)
     C                   Eval      R2EMPN = EMNAME
      *** beg add ***** 06/26/08 **********************************************
     C                   eval      R2CUST = EMCUST
      *** end add ***** 06/26/08 **********************************************
     C                   Endif
      *
     C                   eval      R2cafl = elctst
     C                   eval      R2AGT# = ##agt
     C                   eval      R2AGTN = ##anam
     C                   Eval      R2FEIN = *Zero
     C                   Eval      R2MOD  = *Zero
     C                   Eval      R2FYR  = EAFYR
     C                   eval      R2COM1 = RNCOM1
     C                   eval      R2COM2 = RNCOM2
     C                   eval      R2COM3 = RNCOM3
      *** beg add ***** 07/18/07 **********************************************
     C                   eval      R2BTYP = RNBTYP
      *** end add ***** 07/18/07 **********************************************
      *
     C                   Eval      R2AMT  = Balance
     C                   Eval      R2SELC = 'Y'
     C                   Eval      R2ACFL = 'N'
      *
      * get current system date in *cymd for following check
      *
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         sysdate           7 0
      *
      * Get Premium Finance Co from employer detail
      *
     C     wdell_Key     Chain     Wdell
      *
      * Validate the premium finance company and load file as premium financed employer or load as a
      * non-financed employer.
      *
     C     pfky1         setgt     ppf01
     C     pfky          readpe    ppf01                                  60
     C                   if        not *in60 and
     C                             sysdate >= PFEFDT
     C                   eval      R2PFIN  = ELPFIN
     C                   eval      R2PTFL  = 'P'
      *
     C                   if        PFEXDT  <> *zero and
     C                             sysdate  > PFEXDT
     C                   eval      R2PFIN  = *blank
     C                   eval      R2PTFL  = 'E'
     C                   endif
      *
     C                   else
     C                   eval      R2PFIN = *blank
     C                   eval      R2PTFL = 'E'
     C                   endif
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   endsl
     C***  End   Add  ***  11/05/04  **************************************
      *
     C                   eval      R2IPGM = Q1PGM
     C                   eval      R2PGM  = Q1PGM
     C                   eval      R2IUSR = Q1USER
     C                   eval      R2USER = Q1USER
     C                   time                    Dsdttm
     C                   eval      R2itim = dstime
     C                   eval      R2time = dstime
     C                   z-add     dsdate        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         R2IDT
     c                   movel     srdto         R2DATE
      *
      *** beg chg ***** 07/18/07 *******************************************
     C*****************  write     prt02
     c                   if        RNBTYP = 'D'
     c                   eval      depModeFlag = getDepMode(R2CO#:R2FND:R2FYR)
     c                   if        depModeFlag = *on
     C                   write     prt02
     c                   else
     c     rtl1key       chain     prt01
     c                   if        %found(WXRTL1)
     c                   eval      RTFYR = R2FYR
     c                   update    prt01
     c                   else
     c                   write     prt02
     c                   endif
     c                   endif
     c                   else
     C                   write     prt02
     c                   endif
      *** end chg ***** 07/18/07 *******************************************
      *
     C                   endsr
     *=====================================================================
     *  Check to see if any unissued checks                               *
     *=====================================================================
     C     chk_Unissued  begsr
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   select
     C*__________
     C*
     C                   when      agencyBill
     C*
     C     keyWAR4L10    setll     pr410
     C*
     C                   dou       %Eof(war4l10)
     C*
     C     keyWAR4L10    reade     pr410
      *
     C                   if        not %eof and
     C                             R4CHK# = *zero
     C                   eval      balance = balance - R4AMT
     C                   endif
      *
     C                   enddo
     C*__________
     C*
     C                   other
     C***  End   Add  ***  11/05/04  **************************************
      *
     C     War4l3_Key    setll     War4l3
     C                   dou       %Eof(War4l3)
     C     War4l3_Key    reade     War4l3
      *
     C                   if        Not %Eof and
     C                             R4chk# = *Zero
     C                   eval      Balance = Balance - R4amt
     C                   endif
      *
     C                   enddo
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   endsl
     C***  End   Add  ***  11/05/04  **************************************
      *
     C                   endsr
     *=====================================================================
     *  CHECK BATCH DETAIL FILE                                           *
     *=====================================================================
     C     Chk_Btch_Det  begsr
      *
     C                   eval      In_Batch = *Off
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   select
     C*__________
     C*
     C                   when      agencyBill
     C*
     C                   if        ABFEIN = svfein and
     C                             ABMOD  = svmod
     C                   eval      In_Batch = *on
     C                   else
     C*
     C     keyWXRTL3     chain     prt03
     C*
     C                   eval      In_Batch = %found(WXRTL3)
     C                   eval      svfein   = ABFEIN
     C                   eval      svmod    = ABMOD
     C*
     C                   endif
     C*__________
     C*
     C                   other
     C***  End   Add  ***  11/05/04  **************************************
      *
     C                   if        EAFND  = svfnd  and
     C                             EAEMP# = svemp# and
     C                             EADIV  = svdiv  and
     C                             EAFYR  = svfyr
     C                   eval      In_Batch = *on
     C                   else
      *
     C     xr1key        chain     Wxrtl1
     C                   if        %Found
     C                   eval      In_Batch = *on
     C                   endif
      *
     C                   eval      svfnd  = EAFND
     C                   eval      svemp# = EAEMP#
     C                   eval      svdiv  = EADIV
     C                   eval      svfyr  = EAFYR
     C                   endif
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   endsl
     C***  End   Add  ***  11/05/04  **************************************
      *
     C                   endsr
     *=====================================================================
     *  Subroutine - Routing information                                  *
     *=====================================================================
     c     Routing       Begsr
     C*
     C*    Routing for Select Report
     C*
     C     QQque         comp      *blank                             090908
     C                   eval      xxque    = QQque
     C                   eval      userName = getUsrName(q1user)
     C                   eval      UPUSNM   = userName
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C                   clear                   ORTITL
     c                   eval      xxfil#   = '0'
     C     orky          chain     por01                              60
     C*
     C                   if        ORTITL = *blank
     C                   eval      ORTITL = title
     C                   endif
     C*
     C                   eval      ORTITL = leftJstify(ORTITL)
     C*
     C                   if        not *in60
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         except    route
      ***** End Del ***** 08/26/14 ************************************
     C                   endif
     C*
     C                   eval      RptTitle =  centerFld(ORTITL)
     C*
     C*    Routing for Not Select Report
     C*
     C                   clear                   ORTITL
     c                   eval      xxfil#   = '1'
     C     orky          chain     por01                              60
     C*
     C                   if        ORTITL = *blank
     C                   eval      ORTITL = title1
     C                   endif
     C*
     C                   eval      ORTITL1 = leftJstify(ORTITL)
     C*
     C                   if        not *in60
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         except    route1
      ***** End Del ***** 08/26/14 ************************************
     C                   endif
     C*
     C                   eval      RptTitle1 = centerFld(ORTITL)
     C*
     c                   Endsr
     *=====================================================================
     *  Subroutine - Return premium selected to write                     *
     *=====================================================================
     C     SelecRpt      Begsr

     C                   if        first = *off
     C                   seton                                        of
     C                   eval      first = *on
     C                   endif

     C                   if        *inof
     C                   except    head
     C***  Begin Add  ***  11/05/04  **************************************
     C                   select
     C                   when      agencyBill
     C                   except    heada
     C                   other
     C                   except    heado
     C                   endsl
     C***  End   Add  ***  11/05/04  **************************************
     C                   setoff                                       of
     C                   endif

     C                   add       1             selCount
     C                   add       balance       selTotal
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   select
     C                   when      agencyBill
     C                   except    detaila
     C                   other
     C***  End   Add  ***  11/05/04  **************************************
     C                   except    detail
     C***  Begin Add  ***  11/05/04  **************************************
     C                   endsl
     C***  End   Add  ***  11/05/04  **************************************

     C                   Endsr
     *=====================================================================
     *  Subroutine - Return premium not selected to write                 *
     *=====================================================================
     C     noSelRpt      Begsr

     C                   if        first1 = *off
     C                   seton                                        oa
     C                   eval      first1 = *on
     C                   endif

     C                   if        *inoa
     C                   except    head1
     C***  Begin Add  ***  11/05/04  **************************************
     C                   select
     C                   when      agencyBill
     C                   except    head1a
     C                   other
     C                   except    head1o
     C                   endsl
     C***  End   Add  ***  11/05/04  **************************************
     C                   setoff                                       oa
     C                   endif

     C                   add       1             notSelCount
     C                   add       balance       notSelTotal
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   select
     C                   when      agencyBill
     C                   except    detail1a
     C                   other
     C***  End   Add  ***  11/05/04  **************************************
     C                   except    detail1
     C***  Begin Add  ***  11/05/04  **************************************
     C                   endsl
     C***  End   Add  ***  11/05/04  **************************************

     C                   Endsr
     *=====================================================================
     *  Subroutine - Update WXRNP - Batch Control Record                  *
     *=====================================================================
     C     updatWxrnp    Begsr
     C
     C     Wxrnl_Key     Chain     Wxrnl
     C                   If        %Found
     C                   Eval      RNBPF = 'Y'
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         RNDATE
     c                   eval      RNTIME = dstime
     c                   eval      RNPGM  = Q1PGM
     c                   eval      RNUSER = Q1PGM
     c                   update    prn01
     c                   else
     c                   seton                                        lr
     C                   Endif
     C                   Endsr
      *=====================================================================
      * Selected Report
      *=====================================================================
     O/copy scopyrout
     Oqsysprt   e            route          2
     O                                           20 'COMPANY . . . . :'
     O                       qqco#               25
    O*
     O          e            route          2
     O                                           20 'GROUP RANGE . . :'
     O                       fromFn            +  2
     O                                         +  1 '-'
     O                       thruFn            +  2
     O          e            route          2
     O                                           20 'DATE RANGE:'
     O                       fromDt            +  2
     O                                         +  1 '-'
     O                       thruDt            +  2
     O*** Beg Chg *****  01/28/10  ************************************
     O****      e            route          1 60
     o***                                           '(C) Copyright Fiserv Insu'
     O***                                     +   1 'rance Solutions, Inc.,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     O*** End Chg *****  01/28/10  ************************************
     O          e            head           1 06
     O                       sys_time
     O                       companyNm          096
     O                       sys_date           132
     O          e            head           2
     O                       q1pgm
     O                       RptTitle            96
     O                                          126 'PAGE:'
     O                       page          z    132
     O***  Begin Chg  ***  11/05/04  **************************************
     O**********e            head           1
     O          e            heado          1
     O***  End   Chg  ***  11/05/04  **************************************
     O                                          +84 'Contested'
     O***  Begin Chg  ***  11/05/04  **************************************
     O**********e            head           0
     O          e            heado          0
     O***  End   Chg  ***  11/05/04  **************************************
     O                       line                +2
     O***  Begin Chg  ***  11/05/04  **************************************
     O**********e            head           2
     O          e            heado          2
     O***  End   Chg  ***  11/05/04  **************************************
     O                                           +2 'Company #'
     O                                           +5 'Group #'
     O                                           +7 'Employer #'
     O                                           +5 'Division'
     O                                           +6 'Year'
     O                                          +11 'Balance'
     O                                           +3 'Audit Flag'
     O          e            detail         1
     O                       EACO#         3     +8
     O                       EAFND         3     +9
     O                       EAEMP#        3     +8
     O                       EADIV         3     +8
     O                       EAFYR         3     +6
     O                       Balance       1     +5
     O                       ELCTST              +8
     O***  Begin Add  ***  11/05/04  **************************************
     O          e            heada          0
     O                       line                +2
     O          e            heada          2
     O                                           +2 'Company #'
     O                                           +5 ' Agt Fein'
     O                                           +2 'Mod'
     O                                          +43 'Balance'
     O          e            detaila        1
     O                       ABCO#         3     +8
     O                       ABFEIN        3     +5
     O                       ABMOD         3     +2
     O                       balance       1    +36
     O***  End   Add  ***  11/05/04  **************************************
     O          e            total       1  1
     O                                              'Number of check request -
     O                                              records created:'
     O                       selCount      3    +17
     O          e            total       1  1
     O                                              'Total amount . . . . . . '
     O                                              '. . . . . . . :'
     O                       selTotal      1     +2
     O          e            noRec          1
     O                                          +66 '*** NO RECORDS MEET '
     O                                              'SELECTION CRITERIA ***'
      *=====================================================================
     * Not Selected Report
      *=====================================================================
     O/copy scopyrout1
     Oqsysprt1  e            route1         2
     O                                           20 'COMPANY . . . . :'
     O                       qqco#               25
    O*
     O          e            route1         2
     O                                           20 'GROUP RANGE . . :'
     O                       fromFn            +  2
     O                                         +  1 '-'
     O                       thruFn            +  2
     O          e            route1         2
     O                                           20 'DATE RANGE:'
     O                       fromDt            +  2
     O                                         +  1 '-'
     O                       thruDt            +  2
     O*** Beg Chg *****  01/28/10  ************************************
     O****      e            route1         1 60
     o***                                           '(C) Copyright Fiserv Insu'
     O***                                     +   1 'rance Solutions, Inc.,'
     O          e            route1         1 59
     O                       #brnd1
     O          e            route1         1
     O                       #brnd2
     O*** END CHG *****  01/28/10  ************************************
     O          e            head1          1 06
     O                       sys_time
     O                       companyNm          096
     O                       sys_date           132
     O          e            head1          2
     O                       q1pgm
     O                       rptTitle1           96
     O                                          126 'PAGE:'
     O                       page1         z    132
     O***  Begin Chg  ***  11/05/04  **************************************
     O**********e            head1          1
     O          e            head1o         1
     O***  End   Chg  ***  11/05/04  **************************************
     O                                          +84 'Contested'
     O***  Begin Chg  ***  11/05/04  **************************************
     O**********e            head1          0
     O          e            head1o         0
     O***  End   Chg  ***  11/05/04  **************************************
     O                       line                +2
     O***  Begin Chg  ***  11/05/04  **************************************
     O**********e            head1          2
     O          e            head1o         2
     O***  End   Chg  ***  11/05/04  **************************************
     O                                           +2 'Company #'
     O                                           +5 'Group #'
     O                                           +7 'Employer #'
     O                                           +5 'Division'
     O                                           +6 'Year'
     O                                           +8 'Return Amt'
     O                                           +3 'Audit Flag'
     O          e            detail1        1
     O                       EACO#         3     +8
     O                       EAFND         3     +9
     O                       EAEMP#        3     +8
     O                       EADIV         3     +8
     O                       EAFYR         3     +6
     O                       Balance       1     +5
     O                       ELCTST              +8
     O***  Begin Add  ***  11/05/04  **************************************
     O          e            head1a         0
     O                       line                +2
     O          e            head1a         2
     O                                           +2 'Company #'
     O                                           +5 ' Agt Fein'
     O                                           +2 'Mod'
     O                                          +41 'Balance'
     O          e            detail1a       1
     O                       ABCO#         3     +8
     O                       ABFEIN        3     +5
     O                       ABMOD         Z     +2
     O                       balance       1    +41
     O***  End   Add  ***  11/05/04  **************************************
     O          e            total1      1  1
     O                                              'Number of check request -
     O                                              records not selected:'
     O                       notSelCount   3    +17
     O          e            total1      1  1
     O                                              'Total amount  . . . . . .'
     O                                              ' . . . . . . . . . :'
     O                       notSelTotal   1     +2
     O          e            noRec1         1
     O                                          +66 '*** NO RECORDS MEET '
     O                                              'SELECTION CRITERIA ***'
    C*====================================================================
    C* exclude  - returns *on if a contested audit flag in WDELP = 'Y'
     C*            and exclude contested audit flag in  WXRNP = 'Y'
    C*====================================================================
     Pexclude          b

     D exclude         pi              n

     C     elky          klist
     C                   kfld                    eaco#
     C                   kfld                    eafnd
     C                   kfld                    eaemp#

     C                   if        rncafl = 'Y'
     C     elky          setll     pel01
     C     elky          reade     pel01
     C                   dow       not %eof
     C                   if        elctst = 'Y'
     C                   return    *on
     C                   endif
     C     elky          reade     pel01
     C                   enddo

     C                   else
     C     elky          setll     pel01
     C     elky          reade     pel01
     C                   dow       not %eof
     C                   if        elctst = 'Y'
     C                   leave
     C                   endif
     C     elky          reade     pel01
     C                   enddo

     C                   endif

     C                   return    *off

     Pexclude          e
      *** beg add ***** 07/18/07 ******************************************
    C*====================================================================
    C* getPrmBalance - populate balance with premium balance
    C*====================================================================
     PgetPrmBalance    b

     C                   select
     C*__________
     C*
     C                   when      agencyBill
     C*
     C                   clear                   balance
     C*
     C     keyWDABL11    setll     pab11
     C                   dou       %Eof(WDABL11)
     C     keyWDABL11    reade     pab11
      *
     C                   select
     C                   when      %Eof
     C                   leave
     C                   when      l11_ABARTY = 'A' or
     C                             l11_ABARTY = 'B' or
     C                             l11_ABARTY = 'D' or
     C                             l11_ABARTY = 'V' or
     C                             l11_ABARTY = 'N' or
     C                             l11_ABARTY = 'R'
     C                   eval      balance = balance + l11_ABAMT
     C                   when      l11_ABARTY = 'P' or
     C                             l11_ABARTY = 'T'
     C                   eval      balance = balance - l11_ABAMT
     C                   other
     C                   endsl
      *
     C                   enddo
     C*
     C                   eval      balance = balance * -1
     C*__________
     C*
     C                   other
      *
     C     Wmeal4_Key    Setll     Wmeal4
     C                   Dou       %Eof(Wmeal4)
     C     Wmeal4_Key    Reade     Wmeal4
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   Select
     C                   When      L4arty = 'A' or
     C                             L4arty = 'B' or
     C                             L4arty = 'D' or
     C                             L4arty = 'V' or
     C                             L4arty = 'N' or
     C                             L4arty = 'R'
     C                   Eval      Balance = Balance + L4amt
     C                   When      L4arty = 'P' or
     C                             L4arty = 'T'
     C                   Eval      Balance = Balance - L4amt
     C                   Other
     C                   Endsl
      *
     C                   Enddo
     C*
     C                   endsl
     C*
     PgetPrmBalance    e
    C*====================================================================
    C* getDepBalance - populate balance with deposit balance
    C*====================================================================
     PgetDepBalance    b

     d xxfyr           s                   like(EAFYR)
     d depByYear       s               n                                        any year
     d cyDepByYear     s               n                                        current year

     c     eaky2         klist
     c                   kfld                    EACO#
     c                   kfld                    EAFND
     c                   kfld                    EAEMP#
     c                   kfld                    EADIV

     c     eaky3         klist
     c                   kfld                    EACO#
     c                   kfld                    EAFND
     c                   kfld                    EAEMP#
     c                   kfld                    EADIV
     c                   kfld                    EAFYR

     c                   eval      cyDepByYear = getDepMode(EACO#:EAFND:EAFYR)
     c                   if        cyDepByYear
     c     eaky3         setll     pea02
     c     eaky3         reade     pea02
     c                   eval      xxfyr = EAFYR
     c                   else
     c     eaky2         setll     pea02
     c     eaky2         reade     pea02
     c                   endif

     c                   dow       not(%eof(WMEAL4))
     c                   if        L4FYR <> xxfyr
     c                   eval      xxfyr = L4FYR
     c                   eval      depByYear = getDepMode(L4CO#:L4FND:L4FYR)
     c                   endif

     c                   if        (L4FYR = EAFYR) or
     c                             (L4FYR <> EAFYR and not(depByYear))
     C                   select
     C                   when      L4ARTY = 'H' or
     C                             L4ARTY = 'R'
     C                   eval      balance = balance + L4AMT
     C                   when      L4arty = 'I' or
     C                             L4arty = 'T'
     C                   eval      balance = balance - L4AMT
     C                   other
     C                   endsl

     C                   endif

     c                   if        cyDepByYear = *on
     c     eaky3         reade     pea02
     c                   else
     c     eaky2         reade     pea02
     c                   endif

     c                   enddo

     PgetDepBalance    e
    C*====================================================================
    C* getDepMode - returns true if year is a deposit by year type of deposit calculation
    C*====================================================================
     PgetDepMode       b
     dgetDepMode       pi              n
     d  co#                                like(EACO#)
     d  fnd                                like(EAFND)
     d  fyr                                like(EAFYR)

     d  flag           s               n

     C     f1ky          klist
     C                   kfld                    co#
     C                   kfld                    fnd
     C                   kfld                    fyr

     c     f1ky          chain     pf101
     c                   eval      flag = F1DPYM = 'Y'
     c                   return    flag

     PgetDepMode       e
      *** end add ***** 07/18/07 ******************************************
