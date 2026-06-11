     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE W1266RA - EFT File Edit Report
     H dftactgrp(*no) actgrp(*caller)
     H option(*nodebugio: *srcstmt)                                                        *
     H debug(*yes)                                                                         *
    H*********************************************************************
    H*  Description: EFT File edit report                                *
    H*                                                                   *
    H*  Called from: W7945R                                              *
    H*                                                                   *
    H*  Calls to:                                                        *
    H*                                                                   *
    H*  Created: C:191  Project: WC1806  Dated: 06/29/09                 *
    H**************************************************************************************
    H*  Revisions:                                                                        *
     H*                                                                                    *
     H* 07/24/20 C:180 Support: 89562                                                      *
     H*                Process only requested type                                         *
     H*                                                                                    *
     H* 12/08/15 C:310 Support :38417                                                      *
     H*                -Modified code to prevent double heading.                           *
     H* 08/05/15 C:307 Project: SY354                                                      *
     H*                -Adding 'Route To:'                                                 *
     H*                                                                                    *
     H* 08/26/14 C:298 Project: SY354                                                      *
     H*                -Implement Routing Page Changes.                                    *
     H*                                                                                    *
     H*  01/22/15 C:283 Project: WC1991 Priss# 41                                          *
     H*             191 - Correct the condition to print all the records.                  *
     H*                                                                                    *
     H*  11/13/14 C:191 Project: W1991                                                     *
     H*                -Flag controls for Combining EFT Transactions                       *
     H*                - Bring back changes from V2R2 with following tag                   *
     H*                             05/20/13 C:189 Project: W1991                          *
     H*                - Never print multiple addenda records for a detail                 *
     H*                                                                                    *
     H*  05/28/14 C:136 Project: 129WTG                                                    *
     H*                -Group EFT transactions from different                              *
     H*                 bank accounts if they share the same                               *
     H*                 EFT information                                                    *
     H*                -Change O-Spec's to accomodate (Changes                             *
     H*                 not commented)                                                     *
     H*                -Combined edit report with transmission report                      *
     H*                                                                                    *
     H*  02/15/13 C:136 Support: WA133 81724                                               *
     H*                -Add group and bank totals                                          *
     H*                -Fix report date                                                    *
     H*                                                                                    *
     H*  07/14/10 C:136 Support: WA071 73256                                               *
     H*                 Do not combine transactions for Agent EFT Types for ECM only.      *
     H*                                                                                    *
     H*  11/19/09  C:191 Project: WC1806 - Priss: Print addenda records on report          *
    H*                                                                                    *
    H**************************************************************************************
    F*
     F*  User id file definition (for getting user param)
     Fwmupl     if   e           k disk
     Fsmorl     if   e           k disk
     Fsmcol     if   e           k disk
      *** beg chg ***** 05/28/14 *****************************
     F***********wsckl     if   e           k disk
     F***********wxckl     if   e           k disk    rename(pck01:pcx01) prefix(cx:2)
     Fwxckl1    if   e           k disk
      *** end chg ***** 05/28/14 *****************************
    F*
    F*  Logical file definition
     Fwaefl     if   e           k disk    rename(pef01:pef0l)
     Fwaefp1    if   e           k disk    rename(pef01:pef0p1)
      *** beg add ***** 05/20/13 ***********************************************
      * Bank Routing file
     Fwsrtl     if   e           k disk
      *** end add ***** 05/20/13 ***********************************************
      *** beg add ***** 11/13/14 ***********************************************
     Fwsckl     if   e           k disk    rename(pck01:pck00) prefix(c0:2)
      *** end add ***** 11/13/14 ***********************************************
     Fqsysprt   o    f  132        printer oflind(*inoa)
    D*================================================================
    D* Constants
    D*================================================================
    D*---------------------------------------------------------------------
    D* Stand Alone Fields
    D*---------------------------------------------------------------------
     D @@co#           s              3
     D @@outq          s             10
     D xxco#           s              3  0 inz
     D kkacct          s             20    inz
     D xxdesc          s             50    inz
     D xxfilnam        s             10    inz
     D xxcnt           s              2  0 inz
     D xxDetWrt        s               N
     D xxAllowed       s               N
     D svcid           s                   like(EFcid)
     D svtype          s                   like(EFtype)
     D x6amtp          s                   like(r6amtp) inz
     C*** BEGIN ADD ****** 11/19/09 ************************************
     D xxprin          s                   like(EFprin)
     C*** END   ADD ****** 11/19/09 ************************************
      *** beg add ***** 02/15/13 ************************************
     d xxGrpTotal      s           +002    like(r6amtp) inz
     d xxFilTotal      s           +002    like(r6amtp) inz
     d xxGrpWrt        s               n
     d xxBnkWrt        s               n
      *** end add ***** 02/15/13 ************************************
      *** beg add ***** 05/28/14 ************************************
     D @@EftFile       s             10
     D @@EftFileMbr    s             10
     d @@type          s              1
     d svEftf          s                   like(CKeftf)
     d svAcct          s                   like(CKacct)
     d xxAccTotal      s           +002    like(r6amtp) inz
     d rcdCount        s              9  0
     d isTrnsRpt       s               n
     d isEditRpt       s               n
     d xxAccWrt        s               n
     d xxFilWrt        s               n
     d xxIdst          s                   like(CKidst)
     d xxAcct          s                   like(CKacct)
     d xxBkNm          s                   like(CKbknm)
     d xxEftFilName    s                   like(CKeftf)
     d xxEftFileMbr    s                   like(@@EftFileMbr)
      *** end add ***** 05/28/14 ************************************
      *
     D xxday           s              2  0 inz                                  days
     D xxbdt           s              6  0 inz                                  Batch Date
     D xdbdt           s               d   inz                                  Date field
     D xdeedt          s               d   inz                                  Date field
     D xxeedt          s              8  0 inz                                  Entry Date
     D xx7asq#         s              4  0 inz
      *
     D svDet           s                   like(xxdet)
      *
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D*
      *** beg add ***** 05/20/13 ***********************************************
     D CombineWP       s                   like(*in90)
     D CombineAG       s                   like(*in90)
     D CombineWC       s                   like(*in90)
     D SupressAdden    s                   like(*in90)
     D xxrtky          s             10
     D isDetCombined   s               n
      *** end add ***** 05/20/13 ***********************************************
     C*** BEGIN ADD ****** 11/19/09 ************************************
     D*******Beg del ******** 11/13/14 *****************************
     D*************arrAdd          s             80    dim(99)
     D*******end del ******** 11/13/14 *****************************
     C*** END   ADD ****** 11/19/09 ************************************
     D arrDesc         s             60    dim(6) ctdata perrcd(1)
    D*---------------------------------------------------------------------
    D* Data Structures
    D*---------------------------------------------------------------------
     D psds          esds                  extname(szq1p)
      *
     D xxDet           DS            43
     D  EFtrcd                        2    overlay(xxDet)
     D  EFdfi                         8    overlay(xxDet:3)
     D  EFckdg                        1    overlay(xxDet:11)
     D  EFdact                       17    overlay(xxDet:12)
     D  EFid#                        15    overlay(xxDet:29)
      *
    D*
     D                 DS
     Dr6data                         94
     Dr6typ                           1    overlay(r6data)
     Dr6trcd                          2    overlay(r6data: 2)
     Dr6rdid                          8    overlay(r6data: 4)
     Dr6rdcd                          1    overlay(r6data:12)
     Dr6dact                         17    overlay(r6data:13)
     Dr6amtp                         10  2 overlay(r6data:30)
     Dr6iid#                         15    overlay(r6data:40)
     Dr6inam                         22    overlay(r6data:55)
     Dr6ddat                          2    overlay(r6data:77)
     Dr6arin                          1    overlay(r6data:79)
     Dr6trc#                         15    overlay(r6data:80)
     D                 DS
     Ddsdttm                         14  0
     Ddstime                          6  0 overlay(dsdttm)
     Ddsdate                          8  0 overlay(dsdttm:7)
      **********************************************************************************************
      * PLIST - Definition
      **********************************************************************************************
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      '*MDY'        srffmt
     C                   parm      '*YMD'        srtfmt
     C                   parm                    srdto
     C                   parm      *BLANK        srerr
      *
     C     dty1md        plist
     C                   parm                    srdti
     C                   parm      '*YYMD'       srffmt
     C                   parm      '*YMD'        srtfmt
     C                   parm                    srdto
     C                   parm      *BLANK        srerr
      *
     C     dty2md        plist
     C                   parm                    srdti
     C                   parm      '*YMD'        srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm      *BLANK        srerr
      *
      **********************************************************************************************
      * KLIST - Definition
      **********************************************************************************************
      **** beg del ***** 05/28/14 ***********************************
     C*****ckky***       klist
     C************       kfld                    xxco#
     C************       kfld                    kkacct
      **** end del ***** 05/28/14 ***********************************
      **** beg add ***** 11/13/14 ***********************************
     C     ckky          klist
     C                   kfld                    xxco#
     C                   kfld                    kkacct
      **** end add ***** 11/13/14 ***********************************
      *
     C     efky          klist
     C                   kfld                    xxco#
      **** beg chg ***** 05/28/14 ***********************************
     C************       kfld                    cxacct
     C                   kfld                    CKacct
      **** end chg ***** 05/28/14 ***********************************
      *** beg add ***** 05/20/13 ***********************************************
     C     keyWSRTL      klist
     C                   kfld                    xxco#
     C                   kfld                    xxrtky
      *** end add ***** 05/20/13 ***********************************************
      *===============================================================
      * MAINLINE
      *===============================================================
      *** beg add ***** 07/14/10 ***********************************************
     c                   call      'RTVCLTID'
     c                   parm                    clientID          3
      *** end add ***** 07/14/10 ***********************************************
      **** beg chg ***** 05/28/14 ***********************************
      **Read*thru*Bank select file for EFT processing
     C******loval*       setll     pcx01
     C************       read      pcx01
     C************       dow       not %eof(wxckl)
      **Process*only if not excluded and
      ****bank*account is same as input or input bank is blank
      ****beg*chg****** 09/01/10 ***********************************************
     C************       if        cxinex <> 'E' and
     C************                 (cxacct = xxacct or xxacct = *blanks)
     C************       if        cxinex <> 'E'
      ****end*chg****** 09/01/10 ***********************************************
      **Set*break*indicators
     C************       eval      *in41 = *on
     C************       eval      *in42 = *on
     C************       eval      *in43 = *off
     C************       exsr      srMain
     C************       endif                                                  cxinex <> 'E'
     C************       read      pcx01
     C************       enddo
     C************       if        xxDetWrt = *Off
     C************       except    detnrec
     C************       else
     C************       except    detend
     C************       endif                                                  xxDetWrt = *Off
     C************       eval      *inlr = *on
      /free
       Setgt (xxCo#:'E') pck01;
       Reade xxCo# pck01;
       Dow not(%eof);
         If isTrnsRpt and CKeftf <> @@EftFile;
            Reade xxCo# pck01;
            Iter;
         Endif;
         If CKeftf <> svEftf;
            svEftf = CKeftf;
            xxEftFilName = CKeftf;
            xxIdst = CKidst;
            xxBkNm = CKbknm;
            svAcct = *blank;
            Exsr SRgetEED;
            *in41 = *on;
            *in42 = *on;
            *in43 = *on;
            If rcdCount > 0;
               Exsr wrtGrpTot;
               Exsr wrtAccTot;
               Exsr wrtFilTot;
               Except detEnd;
               rcdCount = 0;
            Endif;
         Endif;
         If CKacct <> svAcct;
            svAcct = CKacct;
            xxAcct = CKacct;
            *in42 = *on;
            *in43 = *on;
            If xxAccTotal <> 0;
               Exsr wrtGrpTot;
               Exsr wrtAccTot;
            Endif;
         Endif;
         Exsr srMain;
       Reade xxCo# pck01;
       If rcdCount = 0;
          *in22 = *off;
          If Ckacct <> svAcct;
             Except head;
          Endif;
       Endif;
       If xxAccTotal = 0;
       //***** Beg Chg ***** 12/08/15 ***********************************
       //*****Except head2;
       //***** End Chg ***** 12/08/15 ***********************************
          Except detNRec;
       Endif;
       Enddo;

       If rcdCount > 0;
          Exsr wrtGrpTot;
          Exsr wrtAccTot;
          Exsr wrtFilTot;
          Except detEnd;
       Endif;

       //***** Beg Add ***** 08/26/14 ***********************************
         Except Route;
       //***** End Add ***** 08/26/14 ***********************************
       *inlr = *on;
      /end-free
      **** end chg ***** 05/28/14 ***********************************
      *==============================================================
      * srMain - Main Program processing routine
      *==============================================================
     C     srMain        begsr
     C**Read*WAEFL in co#/acct  order
      *** beg chg ***** 05/28/14 ************************************
     C*****efky**        setll     pef0l
     C*****efky**        reade     pef0l
     C***********        exsr      srrstsv
     C***********        dow       not %eof(waefl)
     C***41******        exsr      srwrthdr
     C***********        if        *in42 = *on
     C***********        setoff                                       42
     C***********        endif                                                  *in42 = *on
      /free
       Select;
       When isEditRpt;
          Setll efky pef0l;
          Reade efky pef0l;
       When isTrnsRpt;
          Setll efky pef0p1;
          Reade efky pef0p1;
       Endsl;
       Exsr srRstSv;
       Dow not(%eof);
          *in22 = *on;
          If *in41;
             Except head;
             *in41 = *off;
          Endif;
          If *in42;
             If *inoa;
                Except head;
             Endif;
             Except head2;
             *in42 = *off;
          Endif;
          Exsr GetWSRTP;
      /end-free
      *** end chg ***** 05/28/14 *************************************
      *
      * Verify transaction against bank service code i.e. credit, debit or both allowed
     C                   exsr      srChkTrans
     C                   if        xxAllowed
      * Load detail amount and count
     C                   eval      x6amtp = x6amtp + EFamt
     C                   eval      xx7asq# = xx7asq# + 1
     C*** BEGIN ADD ****** 11/19/09 **********************************
     C**** Beg chg ******* 11/13/14 *********************************
      * Accumulate addenda fields in array
     C************       eval      arrAdd(xx7asq#) = EFprin
     C                   eval      xxprin = EFprin
     C**** End chg ******* 11/13/14 *********************************
     C*** END   ADD ****** 11/19/09 **********************************
      *
     C                   else
      * Not allowed, Write error report for detail record
      * Check report overflow
     C                   if        *inoa = *on
     C                   except    head
      *** beg add ***** 05/28/14 **************************
     C                   except    head2
      *** end add ***** 05/28/14 **************************
     C                   endif                                                  *inoa=*on
      *
     C                   except    detail
     C                   endif                                                  xxAllowed
      *
      * read next record
      *** beg chg ***** 05/28/14 *************************************
     C*****efky**        reade     pef0l
      /free
       Select;
       When isEditRpt;
          Reade efky pef0l;
       When isTrnsRpt;
          Reade efky pef0p1;
       Endsl;
      /end-free
      *** end chg ***** 05/28/14 *************************************
      *
      * Process break indicators
     C                   exsr      srPrcBrkInd
      *
      * Process previos record based on break
      * --- Only if there is one to process
     C                   if        xx7asq# > 0
      * Write entry detail record - Type 6
     C   43              exsr      srwrtdetail
     C                   endif                                                  xx7asq#>0
      *** beg add ***** 02/15/13 ******************************
      *** beg del ***** 05/28/14 ******************************
     c***43******        exsr      wrtTotals
      *** end del ***** 05/28/14 ******************************
      *** end add ***** 02/15/13 ******************************
     C   43              exsr      srrstsv
      *
     C                   enddo                                                  not %eof
      *
     C                   endsr
      ****************************************************************
      * srrstsv - Reset Save fields
      ****************************************************************
     C     srrstsv       begsr
     C                   eval      svcid= EFcid
     C                   eval      svDet = xxDet
     C                   eval      svtype= EFtype
     C                   eval      isDetCombined = *Off
     C                   endsr
      ****************************************************************
      * srPrcBrkInd - Process Break indicators
      ****************************************************************
     C     srPrcBrkInd   begsr
     C* Set break indicators
     C                   select
      *** beg chg ***** 01/22/15 ************************************
     C**************     when      %eof(waefl)
     C                   when      (isEditRpt AND %eof(waefl)) or
     C                             (isTrnsRpt AND %eof(waefp1))
      *** end chg ***** 01/22/15 ************************************
      *** beg del ***** 05/28/14 ************************************
     C***********        eval      *in41 = *on
      *** end del ***** 05/28/14 ************************************
     C                   eval      *in42 = *on
     C                   eval      *in43 = *on
     C                   when      EFcid<> svcid
     C* Batch header break
     C                   eval      *in42 = *on
     C                   eval      *in43 = *on
     C                   when      xxDet <> svDet
     C*** beg add ***** 11/13/14 ***********************************************
     C                             or (EFtype = 'E' and not CombineWP)
     C                             or (EFtype = 'A' and not CombineAG)
     C                             or (EFtype = 'C' and not CombineWC)
     C*** end add ***** 11/13/14 ***********************************************
     C*** beg del ***** 11/13/14 ***********************************************
     C******************           or clientID = '071' and
     C******************              EFtype = 'A'
     C******************           or clientID = '045' and
     C******************              EFtype = 'E'
     C******************           or clientID = '025'
     C******************           or clientID = '114'
     C******************           or clientID = '146'
     C******************           or clientID = '102'
     C******************           or clientID = '133'
     C******************           or clientID = '129'
     C*** end del ***** 11/13/14 ***********************************************
     C* Check detail break to combine like transactions
     C                   eval      *in43 = *on
     C                   other
     C* No break
      *** beg del ***** 05/28/14 ************************************
     C***********        eval      *in41 = *off
      *** end del ***** 05/28/14 ************************************
     C                   eval      *in42 = *off
     C                   eval      *in43 = *off
     C                   endsl
     C*** beg add ***** 11/13/14 ***********************************************
     C                   If        xxDet = svDet And *In43 = *Off
     C                   eval      isDetCombined = *On
     C                   Endif
     C*** end add ***** 11/13/14 ***********************************************
      *
     C                   endsr
      *** beg add ***** 05/20/13 ***********************************************
      /free
       BegSr GetWSRTP;
        //* beg add ***** 11/13/14 ***************************************
       kkacct = ckacct;
       Chain ckky pck00;
        //* end add ***** 11/13/14 ***************************************
       Clear SupressAdden;
       Clear CombineWC;
       Clear CombineWP;
       Clear CombineAG;
       Clear RTspad;
       Clear RTcmwc;
       Clear RTcmwp;
       Clear RTcmag;

       If  C0idsa <> *blanks;
          xxrtky = C0idsa;
       Else;
          xxrtky = %Editc(C0idst : 'X');
       Endif;

       If  xxrtky = *blanks;
          xxrtky = C0TRNS + C0BNK# + %Editc(C0ckdg : 'X');
       Endif;

       Chain keyWSRTL PRT01;

       If %found;
          If  RTspad = 'Y' ;
             SupressAdden = *on;
          Endif;
          If  RTcmwc = 'Y' ;
             CombineWC = *on;
          Endif;
          If  RTcmwp = 'Y' ;
             CombineWP = *on;
          Endif;
          If  RTcmag = 'Y' ;
             CombineAG = *on;
          Endif;
       Endif;
       EndSr;
      /end-free
      *** end add ***** 05/20/13 ***********************************************
      ****************************************************************
      * srChkTrans - Check Transaction code allowed for bank
      ****************************************************************
     C     srChkTrans    begsr
      *
     C                   eval      xxAllowed = *off
      *
      * Load variables for Detail record
     C                   eval      r6trcd = EFtrcd
     C                   eval      r6inam = EFinam
     C                   eval      r6dact = EFdact
     C                   eval      r6amtp = EFamt
      *
     C                   select
     C                   when      cksvcl = 220
      * Credits only
     C                   if        EFtrcd = '27' or EFtrcd = '28' or
     C                             EFtrcd = '37' or EFtrcd = '38'
     C                   eval      xxdesc = arrDesc(3)
     C                   leavesr
     C                   endif                                                  Debit ACH codes
      *
     C                   when      cksvcl = 225
      * Debits only
     C                   if        EFtrcd = '22' or EFtrcd = '23' or
     C                             EFtrcd = '32' or EFtrcd = '33'
     C                   eval      xxdesc = arrDesc(4)
     C                   leavesr
     C                   endif                                                  Credit ACH codes
      *** Begin Add *** 07/24/20 ********************************************
      * Incoming payee type only
     C                   when      svType <> *blanks  and EFtype <> svType
     C                   leavesr
      *** End   Add *** 07/24/20 ********************************************
     C                   endsl
      *
      * Either service code is 200 (Mixed entries) or Service code matches transaction
     C                   eval      xxAllowed = *on
      *
     C                   endsr
      ****************************************************************
      * srwrtdetail - Write Entry Detail Record
      ****************************************************************
     C     srwrtdetail   begsr
      *
     C                   eval      r6amtp = x6amtp
     C                   eval      x6amtp = 0
      *** beg add ***** 02/15/13 ****************************
     c                   eval      xxGrpTotal = xxGrpTotal + r6amtp
     C                   eval      xxGrpWrt = *on
     C                   eval      xxBnkWrt = *on
      *** end add ***** 02/15/13 ****************************
      *** beg add ***** 05/28/14 ****************************
     C                   eval      xxAccWrt = *on
     C                   eval      xxFilWrt = *on
     c                   eval      xxAccTotal = xxAccTotal + r6amtp
     c                   eval      rcdCount = rcdCount + 1
      *** end add ***** 05/28/14 ****************************
      *
      * Check report overflow
     C                   if        *inoa = *on
     C                   except    head
      *** beg add ***** 05/28/14 **************************
     C                   except    head2
      *** end add ***** 05/28/14 **************************
     C                   endif                                                  *inoa=*on
      *
      * Write activity to report
      * Pre-note or EFT debit/Credit
      *** beg chg ***** 05/28/14 **************************
     C***********        if        r6amtp = 0
     C***********        eval      xxdesc = arrDesc(2)
     C***********        else
     C***********        eval      xxdesc = arrDesc(1)
     C***********        endif
      /free
       If isEditRpt;
          If r6Amtp = 0;
             xxDesc = arrDesc(2);
          Else;
             xxDesc = arrDesc(1);
          Endif;
       Endif;
       If isTrnsRpt;
          If r6Amtp = 0;
             xxDesc = arrDesc(6);
          Else;
             xxDesc = arrDesc(5);
          Endif;
       Endif;
      /end-free
      *** end chg ***** 05/28/14 **************************
     C                   except    detail
     C                   eval      xxDetWrt = *on
     C                   clear                   r6data
      *
     C*** BEGIN ADD ****** 11/19/09 ************************************
     C**** Beg del  ****** 11/13/14 *********************************
     C************       for       xxcnt = 1 to xx7asq#
     C************       eval      xxprin = *blanks
     C************       eval      xxprin = arrAdd(xxcnt)
     C**** End del  ****** 11/13/14 *********************************
      * Check report overflow
     C                   if        *inoa = *on
     C                   except    head
      *** beg add ***** 05/28/14 **************************
     C                   except    head2
      *** end add ***** 05/28/14 **************************
     C                   endif                                                  *inoa=*on
     C                   if        not SupressAdden
     C                   If        isDetCombined
     C                   eval      xxprin = 'Multiple records combined'
     C                   Endif
     C                   except    addenda
     C                   endif
     C**** Beg del  ****** 11/13/14 *********************************
     C***************    eval      arrAdd(xxcnt) = *blanks
     C***************    endfor
     C**** End del  ****** 11/13/14 *********************************
     C*** END   ADD ****** 11/19/09 ************************************
      * Set counter for addenda records
     C                   eval      xx7asq# = 0
      *
     C                   endsr
      **********************************************************************************
      * srGetEED - Get entry effective date
      **********************************************************************************
     C     srGetEED      begsr
      *
      *** beg add ***** 05/28/14 *******************************
      /free
       xxDay = CKbklt;
      /end-free
      *** end add ***** 05/28/14 *******************************
     C                   if        xxday < 0
     C                   eval      xxday = 0
     C                   endif                                                  xxday < 0
      *
      * add xxday to system date - to get entry effective date (batch date+lead time)
     C                   eval      xdbdt = %date(xxbdt:*YMD)
     C     xdbdt         adddur    xxday:*days   xdeedt
      *** beg chg ***** 02/15/13 *******************************
     C***********        move      xdeedt        dsdate
     C***********        z-add     dsdate        srdti

     c                   move      xdeedt        xxeedt
     c                   z-add     xxeedt        srdti
      *** end chg ***** 02/15/13 *******************************
     C                   call      'SRCVTDAT'    dty1md
     C                   movel     srdto         sveedt            6 0
      *
     C                   z-add     sveedt        srdti
     C                   call      'SRCVTDAT'    dty2md
     C                   movel     srdto         xxeedt
      *
     C                   endsr
      **********************************************************
      *                             Initialization Subroutine
      **********************************************************
     C     *inzsr        begsr
     C*
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@outq
      *** beg add ***** 05/28/14 *****************************
     c                   parm                    @@EftFile
     c                   parm                    @@EftFileMbr
     c                   parm                    @@type
      *** end add ***** 05/28/14 *****************************
     C*
     C     orky          klist
     C                   kfld                    q1pgm
     C                   kfld                    xxprt             1
     C*
     C                   move      @@co#         xxco#
      * Get batch date
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxbdt
     C*
     C* chain to company file
     C     xxco#         chain     pco01
     C*
     C* Chain user param file for user name
     C     q1user        chain     pup01
     C*
     C* get distribution information
     C*
      *** beg chg ***** 05/28/14 *****************************************
     C***********        move      '0'           xxprt
     C*****orky**        chain     por01
     C***********        if        not %found
     C***********        eval      ortitl = '       EFT Activity Report'
     C***********        endif                                                  not %found
      /free
       If @@EftFile <> *blanks;
          isTrnsRpt = *on;
          xxEftFileMbr = @@EftFileMbr;
          *in21 = *on;
          Chain (Q1pgm : '1') por01;
          If not(%found);
             ORtitl = '   EFT Transmission Report';
          Endif;
       Else;
          isEditRpt = *on;
          xxEftFileMbr = *blanks;
          *in21 = *off;
          Chain (Q1pgm : '0') por01;
          If not(%found);
             ORtitl = '     EFT Activity Report  ';
          Endif;
       Endif;
      /end-free
      *** end chg ***** 05/28/14 *****************************************
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         except    route
      ***** End Del ***** 08/26/14 ************************************
     C                   eval      xx7asq# = 0
     C                   endsr
      *** beg add ***** 05/28/14 ******************************
       //========================================================
       // WrtFilTot - Write File Totals
       //========================================================
      /free
       Begsr WrtFilTot;
         If xxFilWrt = *on;
           If *inoa = *on;
              Except head;
              Except head2;
           Endif;
           Except bnkTotal;
           xxFilTotal = 0;
           xxFilWrt = *off;
         Endif;
       Endsr;
       //========================================================
       // WrtGrpTot - Write Group Totals (Change in EFcid)
       //========================================================
       Begsr WrtGrpTot;
         If xxGrpWrt = *on;
            If *inoa = *on;
               Except head;
            Endif;
            Except grpTotal;
            xxFilTotal = xxFilTotal + xxGrpTotal;
            xxGrpTotal = 0;
            xxGrpWrt = *off;
         Endif;
       Endsr;
       //========================================================
       // WrtAccTot - Write Account Totals (Change in EFacct)
       //========================================================
       Begsr WrtAccTot;
          If *inoa = *on;
             Except head;
          Endif;
          Except accTotal;
          xxAccTotal = 0;
       Endsr;
      /end-free
      *** end add ***** 05/28/14 ******************************
      *===============================================================
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             UPUSNM              67
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       UPUSNM              67
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             @@OUTQ              31
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O**
     O**
     O**   SELECTION CRITERIA
     O**
     O          E            ROUTE          1
     O                                           20 'Company  . . :'
     O                       @@CO#               25
     O*
     O          E            ROUTE          1
     O                                           20 'Type . . . . :'
     O                       @@TYPE              23
     O*
     O          E            ROUTE          1 59
     O                                         +001 '(c)'
     O                       *YEAR             +001
     O                                         +001 'StoneRiver, Inc.'
     O                                         +001 'All rights reserved.'
     O          E            ROUTE          1
     O                                         +001 'StoneRiver is a'
     O                                         +001 'trademark of'
     O                                         +001 'StoneRiver, Inc.'
     O*=========================================================================
     O* REPORT DETAIL
     O*=========================================================================
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              85
     O                       DSDATE             132 '  /  /    '
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             101
     O                                          126 'Page:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                        +0001 'Immediate Dest #:'
     O                       xxIdst           +0003
     O                       xxBkNm           +0003
     O          E    21      head           1
     O                                        +0001 'Settlement date :'
     O                       xxeedt           +0003 '  /  /    '
     O          E            head           3
     O                                        +0001 'EFT File name . :'
     O                       xxEftFilName     +0003
     O               21                       +0003 'Member:'
     O                       xxEftFileMbr     +0003
      * Sub header for account/group break
     O          e            head2          1
     o                                        +0001 'Bank Account:'
     O                       xxAcct           +0003
     o          e    22      head2          2
     o                                        +0001 'Group Identifier: '
     O                       EFcid            +0001
     O                       EFbnam           +0003
     O                       EFcdta           +0003
     O          E    22      HEAD2          2
     O                                            8 'Type'
     O                                           14 'TrnCd'
     O                                           24 'Acct Name'
     O                                           50 'Detail Acct#'
     O                                           61 '    Amount'
     O                                           74 'Description'
     O          E            DETAIL         1
     O                       svtype               6
     O                       r6trcd              12
     O                       r6inam              37
     O                       r6dact              55
     O                       R6AMTP        3     61
     O                       XXDESC             113
     C*** BEGIN ADD ****** 11/19/09 ************************************
     O          E            ADDENDA        1
     O                       xxprin              90
     C*** END   ADD ****** 11/19/09 ************************************
     O          E            DETNREC     1  2
     O                                           29 '*** No EFT transactions'
     O                                           43 ' processed ***'
     O          E            DETEND      1  2
     O                                           50 '*** END OF FILE ***'
     o          e            accTotal       2
     o                                        +0001 'Bank Acct Total:'
     o                       xxAccTotal       +0003 '  ,   ,   , $0.  '
     o          e            grpTotal    1  2
     o                                        +0001 'Group ID Total :'
     o                       xxGrpTotal       +0003 '  ,   ,   , $0.  '
     o          e            bnkTotal       2
     o                                        +0001 'File Total . . :'
     o                       xxFilTotal       +0003 '  ,   ,   , $0.  '
**  TABLE DESC
EFT Transaction ready
Pre- Note ready
Debit transaction not allowed for Credit only Bank
Credit transaction not allowed for Debit only Bank
EFT Transaction processed
Pre- Note processed
