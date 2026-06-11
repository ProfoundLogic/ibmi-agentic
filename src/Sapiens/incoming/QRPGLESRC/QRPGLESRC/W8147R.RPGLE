     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
    h/title W8147R - PA Security Fund Processing Program
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     *====================================================================
    h*  Created by: C136 on 07/27/09 for project 114WUK                  *
     *====================================================================
    h* Revisions                                                         *
    h*                                                               *
    h* 04/01/16 C:136 Phaseware: 40495                               *
    h*                Only process payments written on the Run Date  *
    h*                in order to speed up processing per John       *
    h*                McCallister                                    *
    h*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
    h*  09/16/10 C:079 Project: 114MXA                               *
    h*                 Prevent transactions with PABR2 = '98' or 'EQ'*
    h*                                                               *
    h*  07/02/10 C:136 Project: 114MWF                               *
    h*                 Correct implementation of 71284 and           *
    h*                 prevent transactions with PABR2 = 'PC' or 'PD'*
    h*                 from processing into XML file and printing    *
    h*                 on the error report.                          *
    h*                                                               *
    h*  02/16/10 C:235 Support 71287                                 *
    h*                 Claims that have been corrected i.e the pay-  *
    h*                 ments have been voided from the system still  *
    h*                 shows up in "PA Security Fund Error Report "  *
     *====================================================================
     fwxgsl     if   e           k disk
     fwdpal7    if   e           k disk    infds(paDS)
     fwmfnl     if   e           k disk
     fwmcml1    if   e           k disk
     fwdrpl3    if   e           k disk
     fsmorl     if   e           k disk
     f******BEGIN ADD******02/16/10***********************************
     f*** beg del ***** 07/02/10 *************************************
     f******* wdpalB    if   e           k disk    rename(ppa01:ppa02)
     f*** end del ***** 07/02/10 *************************************
     f******END ADD********02/16/10***********************************
     fqsysprt   o    f  180        printer oflind(*inof)
     fqsysprt1  o    f  180        printer oflind(*inoa) usropn
     *====================================================================
      * Data Structures
     *====================================================================
     d psds          esds                  extname(szq1p)
     *====================================================================
      * Prototype Definitions
     *====================================================================
     d/copy sprcompany
     d/copy sprdate
     d/copy sprstring
     d/copy wprusrprms
     d writeXML        pr                  extpgm('WAXMLC')
     d id                            10
     d layout                        10
     d firstRead                      1
     d getName         pr
     d    name1                      25
     d    name2                      25
     *====================================================================
      * Field Definitions
     *====================================================================
     d xxco#           s              3  0
     d xxfil#          s                   like(ORFIL#)
     d xxque           s                   like(@@que)
     d routeCo#        s              3
     d routeDate       s               d   datfmt(*usa)
     d @@co#           s              3
     d @@runDate       s              8
     d @@que           s             10
     d companyNm       s             60
     d sys_date        s               d   datfmt(*usa)
     d sys_time        s               t   timfmt(*hms:)
     d userName        s             50
     d upusnm          s                   like(userName)
     d docID           s             10
     d payeeName       s             25
     d claimantName    s             25
     d claim#          s              7  0
     d serviceDate     s               d   datfmt(*MDY)
     d covCode         s              3
     d tranCode        s              3
     d paymentAmt      s              9  2
     d paymentTotal    s             11  2
     d firstRead       s              1    inz('Y')
     d id              s             10    inz('SECURITY  ')
     d layout          s             10    inz('WZP1P     ')
     d errorRptOpen    s              1    inz('N')
     d ortitl0         s                   like(ORTITL)
     d ortitl1         s                   like(ORTITL)
     d saveFund        s                   like(GSFND)
     d fundName        s                   like(FNNAME)
     d name1           s             25
     d name2           s             25
     d reason          s             40
     d recordSelected  s               n
     d saveInEx        s                   like(GSIEFL)
      *** beg add ***** 04/01/16 *************************************
     d kkRunDate       s              7  0
      *** end add ***** 04/01/16 *************************************
     d******BEGIN ADD******02/16/10***********************************
     d*** beg del ***** 07/02/10 *************************************
     d*********** do_not_write    s              1a   inz(' ')
     d*** end del ***** 07/02/10 *************************************
     d******BEGIN ADD******02/16/10***********************************
     *====================================================================
      * Data Structure
     *====================================================================
     d paDS            ds
     d  paRRN                397    400B 0
     d parmData        ds           200
     d  runDate                       8
     d  fileRRN                       9
     *====================================================================
      * Constants
     *====================================================================
     d title0          c                   'PA Security Fund Transaction -
     d                                     Register'
     d title1          c                   'PA Security Fund Error Report'
     *====================================================================
      * Plists
     *====================================================================
     c     *entry        plist
     c                   parm                    @@co#
     c                   parm                    @@runDate
     c                   parm                    @@que
     *====================================================================
      * Klists
     *====================================================================
     c     orky          klist
     c                   kfld                    q1pgm
     c                   kfld                    xxfil#
     c     paky          klist
     c                   kfld                    GSCO#
     c                   kfld                    GSFND
      *** beg add ***** 04/01/16 *************************************
     c                   kfld                    kkRunDate

     c     fnky          klist
     c                   kfld                    GSCO#
     c                   kfld                    GSFND
      *** end add ***** 04/01/16 *************************************

     c******BEGIN ADD******02/16/10***********************************
     c*** beg del ***** 07/02/10 *************************************
     c*****paky1*        klist
     c***********        kfld                    GSCO#
     c***********        kfld                    GSFND
     c***********        kfld                    GSFYR             3 0
     c***********        kfld                    GSCASE            7 0
     c***********        kfld                    GSCHK#            7 0
     c*****paky2*        klist
     c***********        kfld                    GSCO#
     c***********        kfld                    GSFND
     c***********        kfld                    GSFYR
     c***********        kfld                    GSCASE
     c***********        kfld                    GSCHK#
     c***********        kfld                    GSSEQ#            5 0
     c*** end del ***** 07/02/10 *************************************
     c******END   ADD******02/16/10***********************************
     *====================================================================
     * Main
     *====================================================================
     c                   exsr      initialize
     c                   read      WXGSL
     c                   eval      saveFund = GSFND
     c                   eval      recordSelected = *off
     c                   eval      saveInEx  = GSIEFL
     c                   dow       not(%eof)
     c                   if        GSIEFL = 'I'
     c                   if        saveFund <> GSFND
     c                   eval      savefund = GSFND
     c                   select
     c                   when      recordSelected = *on
     c                   eval      recordSelected = *off
     c                   when      saveInEx = 'I'
     c                   except    noRecord
     c                   endsl
     c                   eval      saveInEx  = GSIEFL
     c                   exsr      printHeader
     c                   endif
     c     paky          setll     ppa01
     c     paky          reade     ppa01
     c                   dow       not(%eof)
     c*** beg del ***** 07/02/10 *************************************
     c***********        if        PASTAT = 'X'
     c******lock*        in        parmData                             60
     c***********        move      @@runDate     runDate
     c***********        move      paRRN         fileRRN
     c***********        out       parmData
     c******BEGIN ADD******02/16/10***********************************
     c***********        eval      gschk# = pachk#
     c***********        eval      gsfyr  = pafyr
     c***********        eval      gscase = pacase
     c***********        eval      gsseq# = paseq#
     c*****paky1*        setgt     ppa02
     c*****paky1*        readpe    ppa02                                  40
     c***********        dow       *in40 = *off
     c***********        if        PATYPE = 'V'
     c***********        Eval      do_not_write = 'Y'
     c***********        leave
     c***********        endif
     c*****paky1*        readpe    ppa02                                  40
     c***********        enddo
     c******loval        setll     ppa02
     c*****paky2*        chain     ppa02
     c***********        if        do_not_write = ' '
     c*** end del ***** 07/02/10 *************************************
     c******END   ADD******02/16/10***********************************
     c***********        callp     writeXML(id : layout : firstRead)
     c***********        eval      firstRead = 'N'
     c***********        in        parmData                             60
     c***********        if        runDate <> @@runDate
     c***********        exsr      printError
     c***********        else
     c***********        eval      recordSelected = *on
     c***********        exsr      printReport
     c***********        endif
     c***********        endif
     c******BEGIN ADD******02/16/10***********************************
     c*** beg del ***** 07/02/10 *************************************
     c***********        endif
     c***********        Eval      do_not_write = ' '
     c******END   ADD******02/16/10***********************************
     c*** end del ***** 07/02/10 *************************************
     c*** beg add ***** 07/02/10 *************************************
     c                   select
     c                   when      PATYPE = 'V' and PASTAT = 'X'
     c                   eval      recordSelected = *on
     c*** beg chg *** 09/16/10 *********************************************
     c**********         when      (PABR7  = 'PC' or PABR7 = 'PD')
     c**********                   and PASTAT = 'X'
     c                   when      (PABR7  = 'PC' or PABR7 = 'PD' or
     c                             PABR7  = '98' or PABR7 = 'EQ')
     c                             and PASTAT = 'X'
     c*** end chg *** 09/16/10 *********************************************
     c                   eval      recordSelected = *on
     c                   when      PASTAT = 'X'
     c                   exsr      pushToXML
     c                   endsl
     c*** end add ***** 07/02/10 *************************************
     c     paky          reade     ppa01
     c                   enddo
     c                   endif
     c                   read      WXGSL
     c                   if        (GSIEFL = 'E' and
     c                              saveInEx = 'I' and
     c                              recordSelected = *off) or
     c                             (%eof = *on and
     c                              saveInEx = 'I' and
     c                              recordSelected = *off)
     c                   eval      saveInEx = 'E'
     c                   except    noRecord
     c                   endif
     c                   enddo
     c                   if        @@que = 'TEST'
     c                   eval      docID = '1234567890'
     c                   eval      payeeName = 'payeeNamePayeeNamePayeeNa'
     c                   eval      claimantName = 'claimantNameClaimantNameC'
     c                   eval      claim# = 1234567
     c                   move      '01/01/01'    serviceDate
     c                   eval      covCode = 'XXX'
     c                   eval      tranCode = 'YYY'
     c                   eval      paymentAmt = 1234567.89
     c                   eval      paymentTotal = 123456789.12
     c                   endif
     c                   except    total
     c                   except    endOfList
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxfil# = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     c                   seton                                        lr
     *====================================================================
     * Initialize - Initialize fields
     *====================================================================
     c     initialize    begsr
     c     *hms          move      q1time        sys_time
     c     *usa          move      *date         sys_date
      *** beg add ***** 04/01/16 *************************
     c     *cymd         move      sys_date      kkRunDate
      *** end add ***** 04/01/16 *************************
     c     *dtaara       define    waxmlda       parmData
     c                   move      @@co#         xxco#
     c                   eval      routeCo# = %editc(xxco# : 'Z')
     c                   eval      companyNm = GetCompNam(xxco#)
     c                   eval      companyNm = centerFld(companyNm)
     c     @@que         comp      *blank                             090908
     c                   eval      xxque    = @@que
     c                   eval      userName = getUsrName(q1user)
     c                   eval      UPUSNM   = userName
     c                   exsr      routing
     c*
     c                   endsr
     *====================================================================
     * Routing - Set-up for routing pages
     *====================================================================
     c     routing       begsr
     c                   clear                   ORTITL
     c     *usa0         move      @@rundate     routeDate
     c                   eval      xxfil#   = '0'
     c     orky          chain     por01
     c                   if        ORTITL  = *blank
     c                   eval      ORTITL  = title0
     c                   endif
     c                   if        %found
     c                   eval      ortitl0 = %triml(ORTITL)
      ***** Beg Del ***** 08/26/14 ************************************
     c**********         except    route
      ***** End Del ***** 08/26/14 ************************************
     c                   endif
     c                   eval      ortitl0 = centerFld(ORTITL)
     c                   clear                   ORTITL
     c                   eval      xxfil#  = '1'
     c     orky          chain     por01
     c                   if        ORTITL  = *blank
     c                   eval      ORTITL  = title1
     c                   endif
     c                   eval      ortitl1 = centerFld(ORTITL)
     c                   endsr
     *====================================================================
     * printReport
     *====================================================================
     c     printReport   begsr
     c                   if        *inof = *on
     c                   eval      *inof = *off
     c                   exsr      printHeader
     c                   endif
     c                   movel     PACHK#        docID
     c                   callp     getName (name1 : name2)
     c                   eval      payeeName = name1
     c                   eval      claimantName = name2
     c                   eval      claim# = PACASE
     c                   monitor
     c     *CYMD         move      PATRDT        serviceDate
     c                   on-error
     c                   endmon
     c                   eval      covCode  = PABR2
     c                   eval      tranCode = PABR7
     c                   eval      paymentAmt = PAAMT
     c                   eval      paymentTotal = paymentTotal + paymentAmt
     c                   except    detail
     c                   endsr
     *====================================================================
     * printError
     *====================================================================
     c     printError    begsr
     c                   if        errorRptOpen = 'N'
     c                   eval      errorRptOpen = 'Y'
     c                   open      qsysprt1
     c                   eval      *inoa = *on
     c                   endif
     c                   if        *inoa = *on
     c                   eval      *inoa = *off
     c                   except    errorHead
     c                   endif
     c                   select
     c                   when      runDate = 'ERROR1'
     c                   eval      reason  = 'No record in WDPAP'
     c                   when      runDate = 'ERROR2'
     c                   eval      reason  = 'No record in WMCMP'
     c                   when      runDate = 'ERROR3'
     c                   eval      reason  = 'No record in WDRPP'
     c                   when      runDate = 'ERROR4'
     c                   eval      reason  = 'PV2 code does not match GL -
     c                             account'
     c                   when      runDate = 'ERROR5'
     c                   eval      reason  = 'Invalid date in WDPAP'
     c                   endsl
     c                   monitor
     c     *CYMD         move      PATRDT        serviceDate
     c                   on-error
     c                   endmon
     c                   except    errorDtl
     c                   endsr
     *====================================================================
     * printHeader
     *====================================================================
     c     printHeader   begsr
      *** beg chg ***** 04/01/16 ***********************
     c*****paky          chain     wmfnl
     c     fnky          chain     wmfnl
      *** beg chg ***** 04/01/16 ***********************
     c                   eval      fundName = FNNAME
     c                   except    head
     c                   endsr
      *** beg add ***** 07/02/10 ********************************
      *=================================================================================
      * pushToXML
      *=================================================================================
     c     pushToXML     begsr
     c     *lock         in        parmData                             60
     c                   move      @@runDate     runDate
     c                   move      paRRN         fileRRN
     c                   out       parmData
     c                   callp     writeXML(id : layout : firstRead)
     c                   eval      firstRead = 'N'
     c                   in        parmData                             60
     c                   if        runDate <> @@runDate
     c                   exsr      printError
     c                   else
     c                   eval      recordSelected = *on
     c                   exsr      printReport
     c                   endif
     c                   endsr
      *** end add ***** 07/02/10 ********************************
     *====================================================================
     o/copy scopyrout
     oqsysprt   e            route          1
     o                                           20 'COMPANY  . . :'
     o                       routeCo#          +  2
     o          e            route          1
     o                                           20 'RUN DATE . . :'
     o                       routeDate         +  2
     o          e            route          1 59
     o                                         +001 '(c)'
     o                       *year             +001
     o                                         +001 'StoneRiver, Inc.'
     o                                         +001 'All rights reserved.'
     o          e            route          1
     o                                         +001 'StoneRiver is a'
     o                                         +001 'trademark of'
     o                                         +001 'StoneRiver, Inc.'
     o          e            head           1 06
     o                       sys_time
     o                       companyNm           95
     o                       sys_date           132
     o          e            head           2
     o                       q1pgm
     o                       ortitl0             95
     o                                          126 'Page:'
     o                       page          z   +  2
     o          e            head        1  1
     o                                              'Group:'
     o                       saveFund          +  2
     o                       fundName          +  2
     o          e            head           1
     o                                           91 'Date of'
     o                                           96 'Cov'
     o                                          102 'Tran'
     o                                          123 'Payment'
     o          e            head           2
     o                                           11 'Document ID'
     o                                           25 'Payee'
     o                                           55 'Claimant'
     o                                           81 'Claim #'
     o                                           91 'Service'
     o                                           97 'Code'
     o                                          102 'Code'
     o                                          123 'Amount'
     o          e            detail         1
     o                       docID               10
     o                       payeeName           45
     o                       claimantName        72
     o                       claim#        Z     81
     o                       serviceDate         92
     o                       covCode             97
     o                       tranCode           102
     o                       paymentAmt    J    124
     o          e            total       1  1
     o                                          107 'Total:'
     o                       paymentTotal  J    124
     o          e            noRecord    1
     o                                           79 '*** No records selected **-
     o                                              *'
     o          e            EndofList   1
     o                                           83 '* * *   E n d   o f   L i -
     o                                              s t  * * *'
     oqsysprt1  e            errorHead      1 06
     o                       sys_time
     o                       companyNm           95
     o                       sys_date           132
     o          e            errorHead      2
     o                       q1pgm
     o                       ortitl1             95
     o                                          126 'Page:'
     o                       page1         z   +  2
     o          e            errorHead      2
     o                                            4 'Fund'
     o                                           09 'Fyr'
     o                                           18 'Case'
     o                                           25 'Seq#'
     o                                           36 'EOB#'
     o                                           45 'Recon#'
     o                                           55 'Trans Dt'
     o                                           70 'Amount'
     o                                           78 'Reason'
     o          e            errorDtl       1
     o                       PAFND         z      4
     o                       PAFYR         z     09
     o                       PACASE        z     18
     o                       PASEQ#        z     25
     o                       PAEOB#        z     36
     o                       PAEOBS        z     45
     o                       serviceDate         55
     o                       PAAMT         p     70
     o                       reason             112
      *=================================================================================
      * getName
      *=================================================================================
     pgetName          b
     d getName         pi
     d    name1                      25
     d    name2                      25
     c     cmky          klist
     c                   kfld                    PACO#
     c                   kfld                    PAFND
     c                   kfld                    PAFYR
     c                   kfld                    PACASE
     c     rpky          klist
     c                   kfld                    PACO#
     c                   kfld                    PAFND
     c                   kfld                    PAFYR
     c                   kfld                    PACASE
     c                   kfld                    PASEQ#
     c     cmky          chain     wmcml1
     c     rpky          chain     wdrpl3
     c                   eval      name1 = RPPNAM
     c                   eval      name2 = %trim(CMLSN) + ', ' + %trim(CMFSN)
     pgetName          e
