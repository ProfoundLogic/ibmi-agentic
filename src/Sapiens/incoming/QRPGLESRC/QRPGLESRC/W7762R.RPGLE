     hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     h/title W7762R  : Written vs. Collected Premium Report (Build Workfile)
     h*=================================================================================
     h*  Description:
     h*  Create a workfile build program to populate the workfile (WFWCP).  . Written premium
     h*
     h*  Called from: W7761C
     h*  Calls to:
     h*=================================================================================
    h*  Created on 05/18/2009 by c:62 for Project 114WRL
     h*=================================================================================
     h* Revisions:
     h*
     h*  06/21/10  C:191 Project: 114WVX
     h*                  Written vs Collected Report Changes
     h*                   Priss#26 : Use Transaction date only
     h*                              Write records even if no written premium
     h*                              Read WDELP on CO# for payment transactions
     h*            C:136  Priss#28 : Accumulate Assesments Billed if CEU reports
     h*                              premium on the policy.
     h*=================================================================================
      * Group Master (co/fnd)
     fwmfnl     if   e           k disk
      * Employer Master (co/fnd/emp#/div)
     fwmeml     if   e           k disk
      * Employer Detail (co/fnd/emp#/div/fyr)
     fwdell     if   e           k disk
      * A/R File (co/fnd/fyr/emp/div/arty)
     fwmeal     if   e           k disk
      * A/R File (co/fnd/fyr/emp/div)
     fwfeul1    if   e           k disk
      * Written Prem vs Collected Prem
     fwfwcp     o    e             disk
     *---------------------------------------------------------------------
     * Stand Alone Fields
     *---------------------------------------------------------------------
     d kkco#           s              3  0
     d kkfyr           s                   like(EUFYR)
     d kkfnd           s                   like(EUFND)
     d kkemp#          s                   like(EUEMP#)
     d kkdiv           s                   like(EUDIV)
     d fromGroup       s                   like(FNFND)
     d toGroup         s                   like(FNFND)
     d sys_date        s               d   datfmt(*usa)
     d sys_time        s               t   timfmt(*hms:)
     d arDate          s                   like(EADT)
     d asOfDate7       s                   like(EADT)
     d asod            s              8  0
     d asOfDate        s               d   datfmt(*usa)
     d dateCvt         s               d   datfmt(*usa)
     d polPremPaid     s             11  2
     d polAssmtBill    s             11  2
     d polAssmtPaid    s             11  2
     d polPremBal      s             11  2
     d polAssmtBal     s             11  2
     d*** BEGIN ADD ****** 06/21/10 ************************************
     d xxrrn           s              9  0
     d*** END   ADD ****** 06/21/10 ************************************
     *---------------------------------------------------------------------
     * Arrays and Data Structure
     *---------------------------------------------------------------------
     d psds          esds                  extname(szq1p)
     *---------------------------------------------------------------------
     * Procedure Interfaces
     *---------------------------------------------------------------------
     d*entry
     d W7762R          pr
     d  @@co#                         3
     d  @@fgrp                        3
     d  @@tgrp                        3
     d  @@asod                        8
     d W7762R          pi
     d  @@co#                         3
     d  @@fgrp                        3
     d  @@tgrp                        3
     d  @@asod                        8
    
     dCheckGroup       pr              n
     d group                          3  0

     d/copy sPrString
     d/copy sPrDate
     d/copy sPrCompany
     d/copy wPrGroup
     d/copy wPrAR
     d/copy wPrEmplr
     d/copy wPrAgent
     d/copy wPrUsrPrms
      *=========================================================================
      * klist
      *=========================================================================

     c     fnky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd

     c     emky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv

     c     eaky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkfyr

     c     euky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
      *=========================================================================
      * Main
      *=========================================================================
     c*** BEGIN CHG ****** 06/21/10 ************************************
     c************       read      pel01
     c     kkco#         setll     pel01
     c     kkco#         reade     pel01
     c*** END   CHG ****** 06/21/10 ************************************
     c                   dow       not(%eof)
     c                   eval      kkfnd  = elfnd
     c                   eval      kkfyr  = elfyr
     c                   eval      kkemp# = elemp#
     c                   eval      kkdiv  = eldiv
     c                   if        checkGroup(kkfnd)
     c                   exsr      writeWFWCP
      *** BEG ADD ***** 06/21/10 ***********************************************
     c                   else
     c     fnky          setgt     pel01
      *** END ADD ***** 06/21/10 ***********************************************
     c                   endif
     c*** BEGIN CHG ****** 06/21/10 ************************************
     c************       read      pel01
     c     kkco#         reade     pel01
     c*** END   CHG ****** 06/21/10 ************************************
     c                   enddo
     c                   seton                                        lr
     c                   return
      *=========================================================================
      *    *inzsr
      *=========================================================================
     c     *inzsr        begsr

     c                   eval      sys_Date  = SystemDate
     c                   eval      sys_Time  = SystemTime
     c                   eval      kkco#     = UserCo#(q1user)
     c                   move      @@fgrp        fromGroup
     c                   move      @@tgrp        toGroup
     c                   move      @@asod        asod
     c                   move      asod          asOfDate
     c     *cymd         move      asOfDate      asOfDate7

     c                   endsr
      *=========================================================================
      *  writeWFWCP - Write WFWCP
      *=========================================================================
     c     writeWFWCP    begsr

     C*** BEGIN DEL ****** 06/21/10 ************************************
     c**********         do
     C*** END   DEL ****** 06/21/10 ************************************
     c                   clear                   pwc01
     c*
     c                   eval      wcco#=elco#
     c                   eval      wcconm = GetCompNam(wcco#)
     c                   eval      wcfnd=elfnd
     c                   eval      wcfnnm = GetGrpName(wcfnd)
     c                   eval      wcemp#=elemp#
     c                   eval      wcdiv=eldiv
     C*** BEGIN CHG ****** 06/21/10 ************************************
     c*********          eval      wcenam = GetEmpName(wcemp#:wcdiv)
     c                   eval      wcenam = GetEmpName(wcemp#:wcdiv:xxrrn:wcfnd)
     C*** END   CHG ****** 06/21/10 ************************************
     c*
     c     emky          chain     pem01
b01  c                   if        %found
     c                   eval      wcindt=emefd1
e01  c                   endif
     c*
     c                   eval      wccxdt = GetCxDate(elco#:elfnd:elemp#:eldiv)
     c                   eval      wcfyr=elfyr
     c*
     c                   if        elpfdt<>*zeros
     c     *cymd         move      elpfdt        dateCvt
     c                   move      dateCvt       wcpfdt
     c                   endif
     c*
     c                   if        elptdt<>*zeros
     c     *cymd         move      elptdt        dateCvt
     c                   move      dateCvt       wcptdt
     c                   endif
     c*
     c                   eval      wcagt#=getEmpAgt(elco#:elfnd:elemp#:
     c                                              eldiv)
     c                   eval      wcagnm=getAgtName(wcagt#)
     c*
     c     euky          chain     peu01
     c*** BEGIN CHG ****** 06/21/10 ************************************
b02  c***********        if        %found
     c***********        eval      wcwrpr=euydwr+eupywr
     c***********        else
     c***********        leave
e02  c***********        endif
     c*
     c                   eval      wcwrpr= 0
     c* Priss 26: also need payments for policies that do not yet have any written premium
     c***********        if        not %found
     c***********        leave
     c***********        else
     c                   if        %found
     c** Loop thru the selected policy to calculate written premium
b02  c                   dou       %eof
     c                   eval      wcwrpr=wcwrpr+euydwr+eupywr
     c     euky          reade     peu01
e02  c                   enddo                                                  %eof
     c                   endif                                                  %found
     c*** END   CHG ****** 06/21/10 ************************************
     c*
     c                   eval      polPremPaid=0
     c                   eval      polAssmtBill=0
     c                   eval      polAssmtPaid=0
     c                   eval      polPremBal=0
     c                   eval      polAssmtBal=0
     c*
     c     eaky          chain     pea01
b03  c                   if        %found
db1  c                   dou       %eof
     c*
     C*** BEGIN DEL ****** 06/21/10 ************************************
     C* Priss 26: Use transaction date instead of AR as of date
     c********           eval      arDate = GetARDate(eaco#:eafnd:eafyr:
     c********                                        eaemp#:eadiv:eaarty:
     c********                                        eapodt:eatrdt)
     C*** END   DEL ****** 06/21/10 ************************************
     C*** BEGIN CHG ****** 06/21/10 ************************************
b04  c********           if        arDate<=asOfDate7
b04  c                   if        eatrdt<=asOfDate7
     C*** END   CHG ****** 06/21/10 ************************************
     c*
     c                   select
     c                   when      eactyp='P'
      *** beg del ***** 06/21/10 ***********************************************
     c***********        eval      polPremBal=polPremBal+arValue(eaamt:eaarty)
      *** beg del ***** 06/21/10 ***********************************************
     c                   if        eaarty='P' or eaarty='T' or eaarty='R' or
     c                             eaarty='N' or eaarty='V'
     c                   eval      polPremPaid=polPremPaid+arValue(eaamt:eaarty)
     c                   endif
     c*
     c*** BEGIN ADD ****** 06/21/10 ************************************
     c* Accumulate Expense Constant paid into the premium paid bucket.
     c                   when      eactyp='E'
      *** beg del ***** 06/21/10 ***********************************************
     c***********        eval      polPremBal=polPremBal+arValue(eaamt:eaarty)
      *** beg del ***** 06/21/10 ***********************************************
     c                   if        eaarty = 'P'
     c                   eval      polPremPaid=polPremPaid+arValue(eaamt:eaarty)
     c                   endif
     c*** END   ADD ****** 06/21/10 ************************************
     c                   when      eactyp='1' or eactyp='2' or eactyp='3' or
     c                             eactyp='4' or eactyp='5' or eactyp='6' or
     c                             eactyp='7'
     c*** beg del ***** 06/21/10 ************************************MLD
     c***********        eval      polAssmtBal=polAssmtBal+arValue(eaamt:eaarty)
     c*** end del ***** 06/21/10 ************************************MLD
     c                   select
     c                   when      eaarty='F'
      *** beg add ***** 06/21/10 ************************************MLD
     c                             and WCWRPR <> 0
      *** end add ***** 06/21/10 ************************************MLD
     c                   eval      polAssmtBill=
     c                                polAssmtBill+arValue(eaamt:eaarty)
     c                   when      eaarty='Q'
     c                   eval      polAssmtPaid=
     c                                polAssmtPaid+arValue(eaamt:eaarty)
     c                   endsl
     c                   endsl
     c*
e04  c                   endif
     c*
     c     eaky          reade     pea01
de1  c                   enddo
e03  c                   endif
     c*
     c                   eval      wcclpr=polPremPaid
     c                   eval      wcsurb=polAssmtBill
     c                   eval      wcsurp=polAssmtPaid
      *** beg chg ***** 06/21/10 *******************************************************
     c***********        eval      wcprmb=polPremBal
     c***********        eval      wcassb=polAssmtBal
     c***********        eval      wcbal=(wcwrpr+wcsurb)-(wcclpr+wcsurp)
     c                   eval      wcprmb = wcwrpr + wcclpr
     c                   eval      wcassb = polAssmtBill + polAssmtPaid
     c                   eval      wcbal  = wcprmb + wcassb
      *** end chg ***** 06/21/10 ***********************************************
     c*
     c                   eval      wcpgm=q1pgm
     c                   eval      wcuser=q1user
     c     *cymd         move      sys_date      wcdt
     c                   move      sys_time      wctime
     c*
     c*** BEGIN CHG ****** 06/21/10 ************************************
     c                   if        wcclpr <> 0 OR wcsurb <> 0 OR wcsurp <> 0
     c                             OR wcprmb <> 0 OR wcassb <> 0 OR wcwrpr <> 0
     c                   write     pwc01
     c                   endif
     c*** END   CHG ****** 06/21/10 ************************************
     C*** BEGIN DEL ****** 06/21/10 ************************************
     c***********        enddo
     C*** END   DEL ****** 06/21/10 ************************************

     c                   endsr
    P*====================================================================
    P* checkGroup - validate the group
    P*
    P*  ValidGrpYr - return *on if group is valid
    P*   group    - passed in for validation  (required)
    P*====================================================================
     PCheckGroup       b                   export
    P*
     DCheckGroup       PI             1n
     D group                               like(fnfnd)

     C     keyWMFNP      klist
     C                   kfld                    kkCo#
     C                   kfld                    group
     C*
     C* There are three scenarios:
     C*      1) No dates entered (must check to see if test group).
     C*      2) Dates entered and fromDate=toDate (do not check to
     C*           see if test group).
     C*      3) Dates entered and fromDate<>toDate (must check to see
     C*           if test group)
     C*
     C*
     C*                  Scenario 1
     C*
     C*
b01  c                   if        fromGroup=0 and toGroup=0
     C     keyWMFNP      chain     pfn01
b02  C                   if        %found and fntest<>'Y'
     C                   return    *on
e02  c                   endif
s01  c                   else
     C*
     C*                  Scenario 2
     C*
b03  C                   if        fromGroup=toGroup
b04  C                   if        group=fromGroup
     C                   return    *on
e04  c                   endif
s03  C                   else
     C*
     C*                  Scenario 3
     C*
b05  c                   if        group >= fromGroup and
     c                             group <= toGroup
     C     keyWMFNP      chain     pfn01
b06  C                   if        %found and fntest<>'Y'
     C                   return    *on
e06  c                   endif
e05  c                   endif
e03  c                   endif
e01  c                   endif
     C*
     C                   return    *off
     C*

     PCheckGroup       e
