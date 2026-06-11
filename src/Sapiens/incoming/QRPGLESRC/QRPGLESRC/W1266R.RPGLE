     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H/TITLE W1266R - Build EFT Transmission File                                          *
     H option(*nodebugio: *srcstmt)                                                        *
     H debug(*yes)                                                                         *
     H DFTACTGRP(*NO) ACTGRP(*caller)
      *====================================================================================*
    H*  Description: EFT File creation and report                                         *
    H*  Called from: W1266C                                                               *
    H*  Calls to: W1266C1                                                                 *
    H*  Created: C:191  Project: WC1806  Dated: 06/29/09                                  *
      *====================================================================================*
    H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  03/17/25 C:291 Support: WC3002                                                    *
     H*                 Write remaining block of 10 records with all 9s if flag Pad block  *
     H*                 with 9s = 'Y'.                                                     *
     H*                                                                                    *
     H*  02/28/25 C:079 Support: 102/118321                                                *
     H*                 Rotate file ID in header based on day of month 1=A,  file.         *
     H*                 2=B, etc.                                            file.         *
     H*                                                                                    *
     H*  07/21/20 C:180 Support: 89562                                                     *
     H*                 Prevent writing of empty, disqualified record to EFT file.         *
     H*                                                                                    *
     H*  07/07/16 C:136 Support: 42346                                                     *
     H*                 Increase size of work hash total fields to prevent abend           *
     H*                 when large hash totals are encountered                             *
     H*                                                                                    *
     H*  06/04/15 C:062 Support: 35174 (114)                                               *
     H*                 Remove the infinite loop.                                          *
     H*                                                                                    *
     H*  04/15/15 C:136 Support: 32493 (LUBA)                                              *
     H*                 Add @@acct back to entry parms. The loss of it causes this program *
     H*                 to ignore whether it is called from batch or not and consequently  *
     H*                 process EFT transactions regardless of whether it is flagged to    *
     H*                 do so or not.                                                      *
     H*                                                                                    *
     H*  04/07/15 C:298 Project: W1991 Priss# 42                                           *
     H*                -If addenda record is included then only the Addenda Record         *
     H*                 indicator should be '1'.                                           *
     H*                                                                                    *
     H*  02/20/15 C:298 Project: W1991 Priss# 38                                           *
     H*                -Populate the addendum records field with file                      *
     H*                 field value during main program processing.                        *
     H*          C:136 -Fix xxRtKy value to be just like CKdisa if possible                *
     H*          C:136 -Fix batch hash count                                               *
     H*                                                                                    *
     H*  12/29/14 C:283 Project: W1991 Priss# 38                                           *
     H*                - Do not increment the sequencing of Addendum Records               *
     H*                  when writing Addenda as it is already being handled               *
     H*                  when writing the Detail record.                                   *
     H*                                                                                    *
     H*  11/13/14 C:191 Project: W1991                                                     *
     H*                - Flag controls for Combining EFT Transactions                       *
     H*                - Bring back changes from V2R2 with following tag                   *
     H*                             05/20/13 C:189 Project: W1991                          *
     H*                - Never print multiple addenda records for a detail                 *
     H*                                                                                    *
     H*  05/28/14 C:136 Project: 129WTG                                                    *
     H*                -Group EFT transactions from different                              *
     H*                 bank accounts if they have the same                                *
     H*                 EFT file name                                                      *
     H*                -Move report print to W1266RA (Changes                              *
     H*                 not commented)                                                     *
     H*                -Cleaned out old comments                                           *
     H*                                                                                    *
     H*  07/19/13 C:180 Support  WA102 82674
     H*                          Populate STD entry code with PPD for
     H*                          client 102.
     H*
     H*  02/15/13 C:136 Support: WA133 81724                                               *
     H*                -Add group and bank totals                                          *
     H*                -Fix report date                                                    *
     H*                                                                                    *
     H*  09/25/12 C:136 Support: WA102 80794                                               *
     H*                 Temp fix to prevent combining of EFT records for FMIC              *
     H*                                                                                    *
     H*  02/13/12 C:136 Support: WA046 79226S 15                                           *
     H*                 Temp fix to prevent combining of EFT records for SCS               *
     H*                                                                                    *
     H*  01/19/12 C:255 Project: WC2112                                                    *
     H*                 Removed code that populated blank in first position of Immed       *
     H*                 Destination field as it is being handled in WA073R.                *
     H*                                                                                    *
     H*  10/31/11 C:255 Support: WA133 78314                                               *
     H*                 Temp fix to prevent combining of EFT records for LUBA              *
     H*                                                                                    *
     H*  06/14/11 C:136 Support: 114WYJ PRISS 15                                           *
     H*                 Temp fix to prevent combining of EFT records for CSI               *
     H*                                                                                    *
     H*  04/14/11 C:136 Support: WA025 76529                                               *
     H*                -Move logic that retrieves EFT file/member name to beginning of     *
     H*                 srMain so that the first membe of the EFT file can be cleared      *
     H*                -Do not combine EFT transactions for TASB                           *
     H*                                                                                    *
     H*  01/31/11 C:136 Support: WA045                                                     *
     H*                 Temp fix to prevent LCTA's Premium EFT Batch from combining        *
     H*                 transactions                                                       *
     H*                                                                                    *
     H*  09/01/10 C:136 Project: WC1806 PRISS 139                                          *
     H*                 Remove affects of bank account parm. It was removed from the       *
     H*                 prompt.                                                            *
     H*                                                                                    *
     H*  07/14/10 C:136 Support: WA071 73256                                               *
     H*                 Do not combine transactions for Agent EFT Types for ECM only.      *
     H*                                                                                    *
     H*  06/15/10 C:136 Support: WA139 73170                                               *
     H*                 Insert blank into position 4 of the header record.                 *
     H*                                                                                    *
     H*  05/26/10 C:255 Project: WC1806                                                    *
     H*                 Uncomment the Immidiate Origin Name field as it was commented      *
     H*                                                                                    *
     H*  xx/xx/xx  C:xx  Project: xxxxx                                                    *
    H*                                                                                    *
      *====================================================================================*
    F*
     F*  User id file definition (for getting user param)
     Fwmupl     if   e           k disk
     Fsmorl     if   e           k disk
     Fsmcol     if   e           k disk
     Fsmbup     if   e             disk
      *** beg chg ***** 05/28/14 *****************************
     F***********wsckl     if   e           k disk
     F***********wxckl     if   e           k disk    rename(pck01:pcx01) prefix(cx:2)
     Fwxckl1    if   e           k disk
      *** beg chg ***** 05/28/14 *****************************
    F*
    F*  Logical file definition
     Fwaefl     uf   e           k disk
     F                                     rename(pef01:paf01)
     F                                     prefix(af:2)
    F*  Physical file definition
     Fwfefp     o    e             disk    usropn
     Fwaefp1    o    e             disk
     F                                     rename(pef01:paf011)
     F                                     prefix(af:2)
      *** beg add ***** 11/13/14 ***********************************************
     Fwsckl     if   e           k disk    rename(pck01:pck00) prefix(c0:2)
      *** end add ***** 11/13/14 ***********************************************
      *** beg add ***** 05/20/13 ***********************************************
      * Bank Routing file
     Fwsrtl     if   e           k disk
      *** end add ***** 05/20/13 ***********************************************
    D*================================================================
    D* Constants
    D*================================================================
    D*---------------------------------------------------------------------
    D* Stand Alone Fields
    D*---------------------------------------------------------------------
     D @@co#           s              3
     D @@outq          s             10
     D @@type          s              1
     D @@mode          s              1
      *
     D xxco#           s              3  0 inz
     D xxacct          s             20    inz
     D kkacct          s             20    inz
     D xxtype          s              1    inz
     D xxcnt           s              2  0 inz
     D xxdesc          s             60    inz
     D xxDetWrt        s               N
     D xxError         s               N
     D xxAllowed       s               N
     D svcid           s                   like(afcid)
     D svtype          s                   like(aftype)
     D x6amtp          s                   like(r6amtp) inz
      *
     D xxday           s              2  0 inz                                  days
     D xxpach          s              1    inz                                  ach flag
     D xxbdt           s              6  0 inz                                  Batch Date
     D xdbdt           s               d   inz                                  Date field
     D xdeedt          s               d   inz                                  Date field
     D xxeedt          s              8  0 inz                                  Entry Date
      *
     D xx7edsq         s                   like(r7edsq)
     D xx7asq#         s                   like(r7asq#)
      *
     D xx8ecnt         s                   like(r8ecnt)
      *** beg chg ***** 07/07/16 ************************
     D*xx8etot***      s                   like(r8etot)
     D xx8etot         s             +5    like(r8etot)
      *** end chg ***** 07/07/16 ************************
     D xx8tdam         s                   like(r8tdam)
     D xx8tcam         s                   like(r8tcam)
      *
     D xx9blkc         s              8  0 inz
     D xx9ecnt         s                   like(r9ecnt)
      *** beg chg ***** 07/07/16 ************************
     D*xx9etot***      s                   like(r9etot)
     D xx9etot         s             +5    like(r9etot)
      *** end chg ***** 07/07/16 ************************
     D xx9tdam         s                   like(r9tdam)
     D xx9tcam         s                   like(r9tcam)
     D xx9bchc         s                   like(r9bchc)
      *
     D svDet           s                   like(xxdet)
      *
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
      *** beg add ***** 05/20/13 ***********************************************
     D CombineWP       s                   like(*in90)
     D CombineAG       s                   like(*in90)
     D CombineWC       s                   like(*in90)
     D SupressAdden    s                   like(*in90)
     D xxrtky          s             10
     D isDetCombined   s               n
      *** end add ***** 05/20/13 ***********************************************
      *** beg add ***** 02/15/13 ************************************
     d xxGrpTotal      s                   like(r6amtp) inz
     d xxBnkTotal      s           +002    like(r6amtp) inz
     d xxGrpWrt        s               n
     d xxBnkWrt        s               n
      *** end add ***** 02/15/13 ************************************
      *** beg add ***** 05/28/14 ************************************
     d svEftf          s                   like(CKeftf)
     d svAcct          s                   like(CKacct)
     d xxAccTotal      s           +002    like(r6amtp) inz
     d firstFileRead   s               n
     d firstAcctRead   s               n
      *** end add ***** 05/28/14 ************************************
     D*
     D*******Beg del ******** 11/13/14 *****************************
     D********* arrAdd          s             94    dim(99)
     D*******end del ******** 11/13/14 *****************************
      ***** Beg Add ***** 02/20/15 *********************************
     dxxprin           s             80
      ***** End Add ***** 02/20/15 *********************************
      *** Beg Add ***** 03/17/25 *********************************
     d i               s              3  0
     d fillerRecords   s              3  0
      *** End Add ***** 03/17/25 *********************************
     d*** Beg Add *** 02/28/25 *********************************************
     d numDayOfMonth   s              2s 0
     d*** End Add *** 02/28/25 *********************************************
    D*---------------------------------------------------------------------
    D* Data Structures
    D*---------------------------------------------------------------------
      *
     D psds          esds                  extname(szq1p)
      *
     D xxDet           DS            43
     D  aftrcd                        2    overlay(xxDet)
     D  afdfi                         8    overlay(xxDet:3)
     D  afckdg                        1    overlay(xxDet:11)
     D  afdact                       17    overlay(xxDet:12)
     D  afid#                        15    overlay(xxDet:29)
      *
     D                 DS
     Dr1data                         94
     Dr1typ                           1    overlay(r1data)
     Dr1prcd                          2    overlay(r1data: 2)
     Dr1imdn                         10    overlay(r1data: 4)
     Dr1imor                         10    overlay(r1data:14)
     Dr1trdt                          6    overlay(r1data:24)
     Dr1trtm                          4    overlay(r1data:30)
     Dr1flid                          1    overlay(r1data:34)
     Dr1rcsz                          3    overlay(r1data:35)
     Dr1blfc                          2    overlay(r1data:38)
     Dr1fmcd                          1    overlay(r1data:40)
     Dr1dstn                         23    overlay(r1data:41)
     Dr1orpn                         23    overlay(r1data:64)
     Dr1rfcd                          8    overlay(r1data:87)
    D*
     D                 DS
     Dr5data                         94
     Dr5rtyp                          1    overlay(r5data)
     Dr5sccd                          3    overlay(r5data: 2)
     Dr5cnam                         16    overlay(r5data: 5)
     Dr5ddat                         20    overlay(r5data:21)
     Dr5cid                          10    overlay(r5data:41)
     Dr5sec                           3    overlay(r5data:51)
     Dr5edes                         10    overlay(r5data:54)
     Dr5cddt                          6  0 overlay(r5data:64)
     Dr5eedt                          6  0 overlay(r5data:70)
     Dr5spag                          3    overlay(r5data:76)
     Dr5oscd                          1    overlay(r5data:79)
     Dr5odfi                          8    overlay(r5data:80)
     Dr5bch#                          7  0 overlay(r5data:88)
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
     Dr7data                         94
     Dr7typ                           1    overlay(r7data)
     Dr7atyp                          2    overlay(r7data: 2)
     Dr7prin                         80    overlay(r7data: 4)
     Dr7asq#                          4  0 overlay(r7data:84)
     Dr7edsq                          7  0 overlay(r7data:88)
    D*
     D                 DS
     Dr8data                         94
     Dr8typ                           1    overlay(r8data)
     Dr8sccd                          3    overlay(r8data: 2)
     Dr8ecnt                          6  0 overlay(r8data: 5)
     Dr8etot                         10  0 overlay(r8data:11)
     Dr8tdam                         12  2 overlay(r8data:21)
     Dr8tcam                         12  2 overlay(r8data:33)
     Dr8cid#                         10    overlay(r8data:45)
     Dr8midc                         19    overlay(r8data:55)
     Dr8rsvd                          6    overlay(r8data:74)
     Dr8oidn                          8    overlay(r8data:80)
     Dr8bch#                          7  0 overlay(r8data:88)
    D*
     D                 DS
     Dr9data                         94
     Dr9typ                           1    overlay(r9data)
     Dr9bchc                          6  0 overlay(r9data: 2)
     Dr9blkc                          6  0 overlay(r9data: 8)
     Dr9ecnt                          8  0 overlay(r9data:14)
     Dr9etot                         10  0 overlay(r9data:22)
     Dr9tdam                         12  2 overlay(r9data:32)
     Dr9tcam                         12  2 overlay(r9data:44)
     Dr9rsvd                         39    overlay(r9data:56)
    D*
     D                 DS
     Ddsdttm                         14  0
     Ddstime                          6  0 overlay(dsdttm)
     Ddsdate                          8  0 overlay(dsdttm:7)
     d*** Beg Add *** 02/28/25 *********************************************
     d/copy sprstring
     d*** End Add *** 02/28/25 *********************************************
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
     C     afky          klist
     C                   kfld                    xxco#
      **** beg chg ***** 05/28/14 ***********************************
     C************       kfld                    cxacct
     C                   kfld                    CKacct
      **** beg chg ***** 05/28/14 ***********************************
      *** beg add ***** 05/20/13 ***********************************************
     C     keyWSRTL      klist
     C                   kfld                    xxco#
     C                   kfld                    xxrtky
      *** end add ***** 05/20/13 ***********************************************
      **********************************************************************************************
      * MAINLINE
      **********************************************************************************************
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
         If CKeftf <> svEftf;
            If svEftf = *blank;
               firstFileRead = *on;
            Else;
               firstFileRead = *off;
            Endif;
            svEftf = CKeftf;
            svAcct = *blank;
            *in41 = *on;
            *in42 = *on;
            *in43 = *on;
            If xx9blkc > 0;
             // Exsr srWrtBchCtrl;
               Exsr srWrtFilCtrl;
            Endif;
            Exsr srPrepFile;
         Endif;
         If CKacct <> svAcct;
            If svAcct = *blank;
               firstAcctRead = *on;
            Else;
               firstAcctRead = *off;
            Endif;
            svAcct = CKacct;
            *in42 = *on;
            *in43 = *on;
         Endif;
         Exsr srMain;
       Reade xxCo# pck01;
       Enddo;


       If xx9blkc > 0;
          Exsr srWrtFilCtrl;
       Endif;

       *inlr = *on;
      /end-free
      **** end chg ***** 05/28/14 ***********************************
      **********************************************************************************
      * srMain - Main Program processing routine
      **********************************************************************************
     C     srMain        begsr
      *** beg add ***** 04/14/11 **********************************************
      *** beg del ***** 05/28/14 **********************************************
     c***********        exsr      srPrepFile
      *** end del ***** 04/14/11 **********************************************
     C* Read WAEFL in co#/acct  order
     C     afky          setll     paf01
     C     afky          reade     paf01
      *** end chg ***** 05/28/14 **********************************************
      * Reset save fields
     C                   exsr      srrstsv
      * This is MAIN program loop
     C                   dow       not %eof(waefl)
      * Check for same TYPE and ACCT
     C                   exsr      srValidate
     C                   if        not xxError
     C* Write file header record - Type 1
     C   41              exsr      srwrthdr
      * Write company batch header record - Type 5
     C   42              exsr      srwrtcbhdr
      * Verify transaction against bank service code i.e. credit, debit or both allowed
     C                   exsr      srChkTrans
     C                   if        xxAllowed
      * Load detail fields
     C                   exsr      srLodDet
     C*** Beg Del ********* 11/13/14 *******************************
      **********Accumulate addenda fields in array
     C****************   exsr      srLodAdden
     C*** End Del ********* 11/13/14 *******************************
      ***** Beg Add ***** 02/20/15 *********************************
      /Free
       xxprin = afprin;
      /End-free
      ***** End Add ***** 02/20/15 *********************************
      * Write to WAEFP1 for archiving purpose
     C                   write     paf011
      * Delete from WAEFP
     C                   delete    paf01
     C                   endif                                                  xxAllowed
     C*** Begin Add *** 06/04/15 ************************************
     C                   endif                                                  not xxError
     C*** End   Add *** 06/04/15 ************************************
     C     afky          reade     paf01
      * Process break indicators
     C                   exsr      srPrcBrkInd
      * Process previous record based on break
      * --- Only if there is one to process
      *** Begin Add ** 07/21/20 ********************************
      /Free
       If xxAllowed  and not xxError;
      /End-Free
      *** End   Add ** 07/21/20 ********************************
     C                   if        xx7asq# > 0
      * Write entry detail record - Type 6
     C   43              exsr      srwrtdetail
     C*** Beg Add ********* 11/13/14 *******************************
     C                   If        Not SupressAdden
     C*** End Add ********* 11/13/14 *******************************
      * Write addenda records - Type 7
     C   43              exsr      srwrtaddenda
     C*** Beg Add ********* 11/13/14 *******************************
     C                   Endif
     C*** End Add ********* 11/13/14 *******************************
     C                   endif                                                  xx7asq#>0
      *** Begin Add ** 07/21/20 ********************************
      /Free
       Endif;
      /End-Free
      *** End   Add ** 07/21/20 ********************************
     C*** Begin Del *** 06/04/15 ************************************
     C**********         endif                                                  xxError
     C*** End   Del *** 06/04/15 ************************************
      *** beg chg ***** 05/28/14 ************************************
     C***********        if        xx9blkc > 0
     C**Write*batch control record
     C***42******        exsr      srwrtbchctrl
      **Write*File control record - Type 9
     C***41******        exsr      srwrtfilctrl
     C***********        endif                                                  xx9blkc>0
      ****beg*add ***** 02/15/13 ******************************
     c***43******        exsr      wrtTotals
      ****end*add ***** 02/15/13 ******************************
      /free
       If xx9blkc > 0 and *in42 = *on;
          Exsr srWrtBchCtrl;
       Endif;
      /end-free
      *** end chg ***** 05/28/14 ************************************
     C   43              exsr      srrstsv
      *
     C                   enddo                                                  not %eof
      *
     C                   endsr
      **********************************************************************************
      * srrstsv - Reset Save fields
      **********************************************************************************
     C     srrstsv       begsr
     C                   eval      svcid= afcid
     C                   eval      svDet = xxDet
     C                   eval      svtype= aftype
     C                   eval      isDetCombined = *Off
     C                   endsr
      **********************************************************************************
      * srValidate - Validate record for processing
      **********************************************************************************
     C     srValidate    begsr
      *
     C                   eval      xxError = *on
      * Type does not match
     C                   if        aftype <> xxtype and xxtype <> *blanks
     C                   leavesr
     C                   endif                                                  aftype <> xxtype
      *
      * If processing in daily batch, check batch process flags
     C                   if        @@mode = 'D'
      * Flag not set for agent EFT processing in daily batch
     C                   if        aftype = 'A' and buaeft <> 'Y'
     C                   leavesr
     C                   endif                                                  aftype = 'A'
      *
      * Flag not set for claims EFT processing in daily batch
     C                   if        aftype = 'C' and buceft <> 'Y'
     C                   leavesr
     C                   endif                                                  aftype = 'C'
      *
      * Flag not set for employers EFT processing in daily batch
     C                   if        aftype = 'E' and bueeft <> 'Y'
     C                   leavesr
     C                   endif                                                  aftype = 'E'
     C                   endif                                                  @@mode = 'D'
      *
     C                   eval      xxError = *off
      *
     C                   endsr
      **********************************************************************************
      * srPrcBrkInd - Process Break indicators
      **********************************************************************************
     C     srPrcBrkInd   begsr
     C* Set break indicators
     C                   select
      *
     C                   when      %eof(waefl)
      *** beg del ***** 05/28/14 ************************************
     C***********        eval      *in41 = *on
      *** end del ***** 05/28/14 ************************************
     C                   eval      *in42 = *on
     C                   eval      *in43 = *on
      *
     C                   when      afcid<> svcid
     C* Batch header break
     C                   eval      *in42 = *on
     C                   eval      *in43 = *on
      *
     C                   when      xxDet <> svDet
     C*** beg add ***** 11/13/14 ***********************************************
     C                             or (AFtype = 'E' and not CombineWP)
     C                             or (AFtype = 'A' and not CombineAG)
     C                             or (AFtype = 'C' and not CombineWC)
     C*** end add ***** 11/13/14 ***********************************************
     C*** beg del ***** 11/13/14 ***********************************************
     C* Remove client hard coding to control combining detail recs with flags
     C***********************      or clientID = '071' and
     C**********************          AFTYPE = 'A'
     C***********************      or clientID = '045' and
     C***********************         AFTYPE = 'E'
     C***********************      or clientID = '025'
     C***********************      or clientID = '114'
     C***********************      or clientID = '146'
     C***********************      or clientID = '102'
     C***********************      or clientID = '133'
     C***********************      or clientID = '129'
     C*** end del ***** 11/13/14 ***********************************************
     C* Check detail break to combine like transactions
     C                   eval      *in43 = *on
      *
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
      **********************************************************************************
      * srwrthdr - Write File header record                                            *
      **********************************************************************************
     C     srwrthdr      begsr
     C                   clear                   r1data
     C                   eval      r1typ = '1'
     C                   eval      r1prcd = '01'
     C*
     C* Immediate destination
     C                   eval      r1imdn = afidst
     C* Immediate Origin
     C                   eval      r1imor = afiorg
     C* Transmission date and time
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         r1trdt
     C                   movel     dstime        r1trtm
      *
       //*** Beg Chg *** 02/28/25 ******************************************
     C*************      movel     'A'           r1flid
       Select;
       When clientId = '102';
          numDayOfMonth = %Dec(%Subst(r1TrDt:5:2):2:0);
          r1Flid = DaytoLetter(numDayOfMonth);
          If r1Flid = ' ';
             r1Flid = 'A';
          Endif;
       Other;
          r1flid = 'A';
       Endsl;
       //*** End Add *** 02/28/25 ******************************************
     C                   movel     '094'         r1rcsz
     C                   movel     '10'          r1blfc
     C                   movel     '1'           r1fmcd
     C*
     C* Immediated Destination Name
     C                   eval      r1dstn = afdest
     C*
     C* Immediate Origin Name
     C*** Beg   chg ***  05/26/10  *****************************************
     C                   eval      r1orpn = afcnam
     C*** End   chg ***  05/26/10  *****************************************
     C*
     C                   eval      r1rfcd = afref
      *
     C                   clear                   efdata
     C                   eval      efdata = r1data
     C                   write     pef01
     C                   eval      xx9blkc = xx9blkc + 1
     C                   setoff                                       41
      *
     C                   endsr
      **********************************************************************************
      * srwrtcbhdr - Write Company batch header record                                 *
      **********************************************************************************
     C     srwrtcbhdr    begsr
      *
     C                   eval      xx9bchc = xx9bchc + 1
     C                   clear                   r5data
     C                   eval      r5rtyp = '5'
     C                   move      cksvcl        r5sccd
      * Batch company name
     C                   eval      r5cnam = afbnam
     C                   eval      r5ddat = afcdta
     C                   eval      r5cid = afcid
     C                   eval      r5sec = 'CCD'
      /FREE
              If ClientId = '102';
              EVAL r5sec = 'PPD';
              Endif;
      /END-FREE
     C                   eval      r5edes = afedes
     C                   eval      r5cddt = afcddt
      * Effective entry date
     C                   eval      r5eedt = sveedt
      *
     C                   eval      r5spag = *blanks
     C                   eval      r5oscd = '1'
      * Bank DFI
     C                   eval      r5odfi = afbdfi
     C                   eval      r5bch# = xx9bchc
     C                   eval      efdata = *blanks
     C                   eval      efdata = r5data
     C                   write     pef01
     C                   eval      xx9blkc = xx9blkc + 1
     C                   setoff                                       42
      *
     C                   endsr
      **********************************************************************************
      * srChkTrans - Check Transaction code allowed for bank                           *
      **********************************************************************************
     C     srChkTrans    begsr
      * Load variables for Detail record
     C                   eval      xxAllowed = *off
     C                   eval      r6trcd = aftrcd
     C                   eval      r6inam = afinam
     C                   eval      r6dact = afdact
     C                   eval      r6amtp = afamt
      *
     C                   select
     C                   when      cksvcl = 220
      * Credits only
     C                   if        aftrcd = '27' or aftrcd = '28' or
     C                             aftrcd = '37' or aftrcd = '38'
     C                   leavesr
     C                   endif                                                  Debit ACH codes
      *
     C                   when      cksvcl = 225
      * Debits only
     C                   if        aftrcd = '22' or aftrcd = '23' or
     C                             aftrcd = '32' or aftrcd = '33'
     C                   leavesr
     C                   endif                                                  Credit ACH codes
     C                   endsl
      * Either service code is 200 (Mixed entries) or Service code matches transaction
     C                   eval      xxAllowed = *on
      *
     C                   endsr
      **********************************************************************************
      * srLodDet    - Load Entry Detail Record                                         *
      **********************************************************************************
     C     srLodDet      begsr
      *
     C                   eval      r6typ = '6'
      *
      * Payee bank DFI
     C                   eval      r6rdid = afdfi
     C                   move      afdfi         xxdfi             8 0
      *** beg del ***** 02/20/15 *******************************
     C***********        eval      xx8etot = xx8etot + xxdfi
      *** end del ***** 02/20/15 *******************************
      *
     C                   eval      r6rdcd = afckdg
     C                   eval      x6amtp = x6amtp + afamt
      * Accumulate amount based on debit/credit transaction code
     C                   select
     C                   when      aftrcd = '22' or aftrcd = '32'
      * Credit
     C                   eval      xx8tcam = xx8tcam + afamt                    Credit
     C                   when      aftrcd = '27' or aftrcd = '37'
      * Debit
     C                   eval      xx8tdam = xx8tdam + afamt                    debit
     C                   endsl
      *
     C                   eval      r6iid# = afid#
     C                   eval      r6ddat = ' '
      ***** Beg Chg ***** 04/07/15 *****************************************************
     C*************      eval      r6arin = '1'
      /Free
       If SupressAdden = *off;
          r6arin = '1';
       Else;
          r6arin = '0';
       Endif;
      /End-free
      ***** End Chg ***** 04/07/15 *****************************************************
     C                   eval      xx7asq# = xx7asq# + 1
      *
     C                   endsr
      **********************************************************************************
      * srLodAdden  - Load addenda Record                                              *
      **********************************************************************************
     C**** BEG del ******* 11/13/14 *********************************
     C**************  srLodAdden    begsr
      ***************
     C***************    clear                   r7data
      ***************
     C***************    eval      r7typ = '7'
     C***************    eval      r7atyp = '05'
     C***************    eval      r7prin = afprin
     C***************    eval      xx7asq# = xx7asq# + 1
     C***************    eval      r7asq# = xx7asq#
     C***************    eval      r7edsq = xx7edsq + 1
      ***************
     C**** BEG del ******* 11/13/14 *********************************
     C***************    eval      arrAdd(r7asq#) = r7data
     C**** End del ******* 11/13/14 *********************************
      *******
     C***************    endsr
     C**** End del ******* 11/13/14 *********************************
      *
      **********************************************************************************
      * srwrtdetail - Write Entry Detail Record                                        *
      **********************************************************************************
     C     srwrtdetail   begsr
      *
      *** beg add ***** 02/20/15 *******************************
     C                   eval      xx8etot = xx8etot + xxdfi
      *** end add ***** 02/20/15 *******************************
      *** beg add ***** 07/07/16 *******************************
     C                   eval      xx9etot = xx9etot + xxdfi
      *** end add ***** 07/07/16 *******************************
     C                   eval      xx8ecnt = xx8ecnt + 1
     C                   eval      xx7edsq = xx7edsq + 1
      * Setup trace#
     C                   movel     r5odfi        r6trc#
     C                   move      xx7edsq       r6trc#
     C                   eval      r6amtp = x6amtp
     C                   eval      efdata = *blanks
     C                   eval      efdata = r6data
     C                   write     pef01
     C                   eval      x6amtp = 0
     c                   eval      xxGrpTotal = xxGrpTotal + r6amtp
     c                   eval      xxGrpWrt = *on
     c                   eval      xxBnkWrt = *on
      *** beg add ***** 05/28/14 ****************************
     c                   eval      xxAccTotal = xxAccTotal + r6amtp
      *** end add ***** 05/28/14 ****************************
     C                   eval      xxDetWrt = *on
     C                   eval      xx9blkc = xx9blkc + 1
     C                   clear                   r6data
      *
     C                   endsr
      **********************************************************************************
      * srwrtaddenda- Write addenda Record                                             *
      **********************************************************************************
     C     srwrtaddenda  begsr
      *
     C                   clear                   r7data

     C                   eval      r7typ = '7'
     C                   eval      r7atyp = '05'
     C**** Beg Add ****** 11/13/14 **************************
     C                   If        isDetCombined = *off
      ***** Beg Chg ***** 02/20/15***************************
     C********           eval      r7prin = afprin
      /Free
       r7prin = xxprin;
      /End-free
      ***** End Chg ***** 02/20/15***************************
     C                   Else
     C                   eval      r7prin = 'Multiple records combined'
     C                   Endif
     C**** End Add ****** 11/13/14 **************************
     C                   eval      r7asq# = 1
     C**** Beg Chg ****** 12/29/14 **************************
     C***************    eval      r7edsq = xx7edsq + 1
     C                   eval      r7edsq = xx7edsq
     C**** End Chg ****** 12/29/14 **************************
     C                   eval      efdata = r7data
     C                   write     pef01
     C                   eval      xx8ecnt = xx8ecnt + 1
     C                   eval      xx9blkc = xx9blkc + 1
     C**** Beg Del ****** 11/13/14 **************************
     C************       for       xxcnt = 1 to xx7asq#
      ************
     C************       eval      xx8ecnt = xx8ecnt + 1
      ************
     C************       eval      efdata = *blanks
     C************       eval      efdata = arrAdd(xxcnt)
      ************
     C************       write     pef01
     C************       eval      xx9blkc = xx9blkc + 1
     C************       eval      arrAdd(xxcnt) = *blanks
      ************
     C************       endfor
      *
      * Set counter for addenda records
     C************       eval      xx7asq# = 0
     C**** End Del ****** 11/13/14 **************************
      *
     C                   endsr
      **********************************************************************************
      * srwrtbchctrl  - Write Batch Control record
      **********************************************************************************
     C     srwrtbchctrl  begsr
      *
     C                   clear                   r8data
     C                   eval      r8typ = '8'
     C                   move      cksvcl        r8sccd
     C                   eval      r8ecnt = xx8ecnt
      *** beg chg ***** 07/07/16 ********************
     C*****************  eval      r8etot = xx8etot
     C                   move      xx8etot       r8etot
      *** end chg ***** 07/07/16 ********************
     C                   eval      r8tdam = xx8tdam
     C                   eval      r8tcam = xx8tcam
     C                   eval      r8cid# = r5cid
     C                   eval      r8midc = *blanks
     C                   eval      r8rsvd = *blanks
     C                   eval      r8oidn = r5odfi
      *
     C                   eval      r8bch# = xx9bchc
      *
     C                   eval      efdata = *blanks
     C                   eval      efdata = r8data
     C                   write     pef01
     C                   eval      xx9blkc = xx9blkc + 1
      *
      * Accumulate for file control record
     C                   eval      xx9ecnt = xx9ecnt + xx8ecnt
      *** beg del ***** 07/07/16 ********************************
     C***********        eval      xx9etot = xx9etot + xx8etot
      *** end del ***** 07/07/16 ********************************
     C                   eval      xx9tdam = xx9tdam + xx8tdam
     C                   eval      xx9tcam = xx9tcam + xx8tcam
      *
      * Reset Batch counters
     C                   eval      xx8ecnt = 0
     C                   eval      xx8etot = 0
     C                   eval      xx8tdam = 0
     C                   eval      xx8tcam = 0
     C                   eval      xx7edsq = 0
      *
     C                   endsr
      *
      **********************************************************************************
      * srwrtfilctrl  - Write File Control record
      **********************************************************************************
     C     srwrtfilctrl  begsr
      *
     C                   eval      xx9blkc = xx9blkc + 1
     C                   clear                   r9data
     C                   eval      r9typ = '9'
     C                   eval      r9bchc = xx9bchc
      *
     C                   eval      r9blkc = %div(xx9blkc:10)
     C                   if        %rem(xx9blkc:10) > 0
     C                   eval      r9blkc = r9blkc + 1
     C                   endif                                                  %rem
      *
     C                   eval      r9ecnt = xx9ecnt
      *** beg chg ***** 07/07/16 **********************
     C***********        eval      r9etot = xx9etot
     C                   move      xx9etot       r9etot
      *** end chg ***** 07/07/16 **********************
     C                   eval      r9tdam = xx9tdam
     C                   eval      r9tcam = xx9tcam
     C                   eval      r9rsvd = *blanks
      *
     C                   eval      efdata = *blanks
     C                   eval      efdata = r9data
     C                   write     pef01

       //*** Beg Add ***** 03/17/25 *****************************************
       If c0Pad9 = 'Y';
       //Write Remaining records with All 9's
          EFData = *All'9';
          fillerRecords = 10 - %rem(xx9blkc:10);
          For i = 1 to fillerRecords;
             Write pef01;
          EndFor;
       Endif;
      //*** End Add ***** 03/17/25 *****************************************
      *
     C                   close     wfefp
      *
      * Cut EFT file here, pass EFT filename and member name to CLP (Mode '2')
     C                   call      'W1266C1'
     C                   parm      '2'           xxmode
     C                   parm                    xxfilnam
     C                   parm                    xxfilmbr

      *** beg add ***** 05/28/14 *******************************
      * Print Transmission Report
      *  Feod WAEFP1 to get the write action caught up so that
      *  the transmission report includes all the records.
      *
     C                   feod      waefp1
     C                   call      'W1266CA'
     C                   parm                    @@co#
     C                   parm                    @@outq
     C                   parm                    xxfilnam
     C                   parm                    xxfilmbr
     C                   parm                    @@type
      *** end add ***** 05/28/14 *******************************

     C                   open      wfefp
      *
      * Reset file counters
     C                   eval      xx9blkc = 0
     C                   eval      xx9ecnt = 0
     C                   eval      xx9etot = 0
     C                   eval      xx9tdam = 0
     C                   eval      xx9tcam = 0
     C                   eval      xx9bchc = 0
      *
     C                   endsr
      **********************************************************************************
      * srGetEED - Get entry effective date
      **********************************************************************************
     C     srGetEED      begsr
      *
     C                   if        xxday < 0
     C                   eval      xxday = 0
     C                   endif                                                  xxday < 0
      *
      * add xxday to system date - to get entry effective date (batch date+lead time)
     C                   eval      xdbdt = %date(xxbdt:*YMD)
     C     xdbdt         adddur    xxday:*days   xdeedt

     c                   move      xdeedt        xxeedt
     c                   z-add     xxeedt        srdti
     C                   call      'SRCVTDAT'    dty1md
     C                   movel     srdto         sveedt            6 0
      *
     C                   z-add     sveedt        srdti
     C                   call      'SRCVTDAT'    dty2md
     C                   movel     srdto         xxeedt
      *
     C                   endsr
      **********************************************************************************
      *                             Initialisation Subroutine                          *
      **********************************************************************************
     C     *inzsr        begsr
     C*
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@outq
      *** beg add ***** 04/15/15 *************************************
     C                   parm                    @@acct           20
      *** end add ***** 04/15/15 *************************************
     C                   parm                    @@type
     C                   parm                    @@mode                         M-Menu call (W7945C)
     C*                                                                         D-Daily Batch
     C*
     C     orky          klist
     C                   kfld                    q1pgm
     C                   kfld                    xxprt             1
     C*
     C                   move      @@co#         xxco#
     C                   move      @@type        xxtype
      * Get batch date
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxbdt
     C*
     C* chain to company file
     C     xxco#         chain     pco01
     C*
     C* Get flags from month-end backup control file
     C     1             chain     pbu01
     C*** Begin Add *** 06/04/15 ************************************
     C                   If        @@Mode = 'D' And BUaEft <> 'Y' And
     C                             BUcEft <> 'Y' And BUeEft <> 'Y'
     C                   Seton                                        LR
     C                   Return
     C                   EndIf
     C*** End   Add *** 06/04/15 ************************************
     C*
     C* Chain user param file for user name
     C     q1user        chain     pup01
     C*
      * Set counters/totals
     C                   eval      xx9blkc = 0
     C                   eval      xx9ecnt = 0
     C                   eval      xx9etot = 0
     C                   eval      xx9tdam = 0
     C                   eval      xx9tcam = 0
     C                   eval      xx9bchc = 0
      *
     C                   eval      xx8ecnt = 0
     C                   eval      xx8etot = 0
     C                   eval      xx8tdam = 0
     C                   eval      xx8tcam = 0
      *
     C                   eval      xx7edsq = 0
     C                   eval      xx7asq# = 0
      *
     C                   open      wfefp
      *
     C                   endsr
      *=================================================================================
      *  srPrepFile - retrieves file/member from W1266C1
      *=================================================================================
     c     srPrepFile    begsr

     c                   eval      xxday = 0
      *** beg add ***** 11/13/14 ***************************************
     c                   eval      kkacct = ckacct
     c     ckky          chain     pck00
      *** end add ***** 11/13/14 ***************************************
      *** beg chg ***** 05/28/14 ***************************************
     c***********        eval      kkacct = cxacct
     c*****ckky**        chain     pck01
     c***********        if        %found
     c***********        eval      xxfilnam = ckeftf
     c                   eval      xxday = ckbklt
     c                   eval      xxFilNam = CKeftf
     c                   exsr      srGetEED
      *** end chg ***** 05/28/14 ***************************************

     c                   call      'W1266C1'
     c                   parm      '1'           xxmode            1
     c                   parm                    xxfilnam         10
     c                   parm                    xxfilmbr         10

      *** beg add ***** 05/20/13 ***********************************************
      /free
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
          xxrtky = C0TRNS + C0BNK# + %Editc(C0CKDG : 'X');
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

      /end-free
      *** end add ***** 05/20/13 ***********************************************
     c                   endsr                                                  srPrepFile
      *=================================================================================
