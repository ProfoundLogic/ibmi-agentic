     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*caller)
    h/title WAXMLR - write XML program
     *====================================================================
     *  Created by: C136 on 07/27/09 for project 114WUK                  *
     *====================================================================
     * Description                                                       *
      * Security Fund Processing (114WUK)                                 *
      *   Program receives 'SECURITY' in @@ID and 'WZP1P' in @@LAYOUT.    *
      *   MAIN then executes procSecFund and gets the run date and the    *
      *   rrn of WDPAP from the WMXMLDA data area. It processes the       *
      *   information from WDPAP and related files into WXXMP. If an      *
      *   error is encountered, an error code is passed back in WAXMLDA   *
      *   and W8147R prints the error log. If edits are passed,           *
      *   PASTAT is updated to 'W', the record is written to WXXMP,       *
      *   W8147 writes the transaction register, and W8147RA will be      *
      *   called to clear PASTAT if the XML creates successfully. The
      *   WZP1P spreadsheet in the workorder describes how the individual *
      *   fields are populated.                                           *
      *                                                                   *
      *   Error codes:  ERROR1 - rrn for WDPAP bad                        *
      *                 ERROR2 - no associated Claim Master record (WMCMP)*
      *                 ERROR3 - no associated Check View record (WDRRP)  *
      *                 ERROR4 - no associated GL account for PV2 code    *
      *                 ERROR5 - bad date                                 *
     *====================================================================
     * Revisions                                                         *
     *====================================================================
      *                                                                   *
      * 05/16/18 C:079 Support: 114 70969                                 *
      *                Do not Populate iDARTS key or  EOB#                *
      *                                                                   *
      * 07/18/16 C:212 Project: 114WYE                                    *
      *                Populate iDARTS key and EOB#                       *
      *                                                                   *
      * 04/09/15 C:295 Project: 114WRY                                    *
      *                Payment Process 793/794 Interface                  *
      *                Change Payment Method to 06 instead of 01 for      *
      *                non-EFT Payments                                   *
      *                                                                   *
      * 07/13/12 C:280 Project: 114WHG                                    *
      *                Change Cost Center from                            *
      *                7955320000  to 7955340000.                         *
      *                                                                   *
      * 07/10/12 C:282 Project: 114WWB                                    *
      *                Removing the project 114WWB as added on date       *
      *                10/18/10.                                          *
      *                                                                   *
      * 10/18/10 C:220 Project: 114WWB                                    *
      *                Changed to recognize out of country addresses when *
      *                populating address information                     *
      *                                                                   *
      * 12/07/09 C:136 Support: WA114 70825                               *
      *                Change logic for Treasury Batch number             *
      *                                                                   *
      * 11/13/09 c:136 Support: WA114 70715                               *
      *                Change payment method to '08' per John McCallister *
      *                                                                   *
      * 11/04/09 C:136 Support: WA114 70552                               *
      *                -Change hard coding for Cost Center Code           *
      *                -Change P1DHTRBCHN so that it populates the        *
      *                 first 3 char's to '794' for Indemnity Payments    *
      *                 or '793' for everything else.                     *
      *                -Hardcode fund value to '5006300000'               *
      *                                                                   *
      *                                                                   *
      * 10/15/09 C:136 Support: WA114 70322                               *
      *                -Make sure payee name is always loaded to the XML  *
      *
      * 09/30/09 C:136 Support: WA114 70068                               *
      *                -Do not remove leading zeroes from zip             *
      *                 code                                              *
      *                -Constuct Treasure Note field like 114WTQ          *
      *                 per John McCallister                              *
     *===================================================================*
     fwdpap     uf   e             disk
     fwmcml1    if   e           k disk
     fwdrpl3    if   e           k disk
     fwmefl     if   e           k disk    prefix(L0) rename(pef01:pef0l)
     fwmefl3    if   e           k disk    prefix(L3) rename(pef01:pef0l3)
     fwmpdl     if   e           k disk
     fwxxmp     if a e             disk
     fwmcdl     if   e           k disk    rename(pcd01:pcd00)
      *** beg add ***** 11/04/09 *****************************************
     fwtabl     if   e           k disk    ignore(ppa01)
      *** end add ***** 11/04/09 *****************************************
     *====================================================================
      * Field Definitions
     *====================================================================
     d  @@id           s             10
     d  @@layout       s             10
     d  name1          s             35
     d  name2          s             35
     d  up             c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     d  lo             c                   'abcdefghijklmnopqrstuvwxyz'
     d  payMthd        s              2
     d  routing#       s              9
     d  account#       s             17
     d  acctType       s              2
     d  errorFlag      s               n
     *====================================================================
     * Data Structures                                                   *
     *====================================================================
     d P1DATA        e ds                  extname(WZP1P)
     d parmData       uds           200
     d  runDate                       8
     d  fileRRN                       9
     d runDtArea      uds            50
     d  runJulDt                      5
     d  runSeq#                       2
     *====================================================================
     * Prototypes for PA Security Fund Processing                        *
     *====================================================================
     d procSecFund     pr
     d getJulDateTime  pr            15
     d getSysDate      pr            10
     d getSysTime      pr             8
     d getPayAmt       pr            16
     d getPayDate      pr            10
     d cvtToISO        pr            10
     d    date7n                      7  0 const
     d getName         pr
     d    name1                      35
     d    name2                      35
      *** beg add ***** 11/04/09 ***********************************************
     d getTrBchN       pr                  like(P1DHTRBCHN)
      *** end add ***** 11/04/09 ***********************************************
     d getDocID        pr            16
     d get1099Flag     pr            10
     d getPayMS        pr             2
     d getBankInfo     pr
     d    payMthd                     2
     d    routing#                    9
     d    account#                   17
     d    acctType                    2
     d procError       pr
     d    msg                         8    const
     d getGLacct       pr            10
     d getFund         pr            10
     d getNotes        pr                  like(P1DHNOTES)
     *====================================================================
      * Plists
     *====================================================================
     c     *entry        plist
     c                   parm                    @@id
     c                   parm                    @@layout
     *====================================================================
      * Main
     *====================================================================
     c     *dtaara       define    w8146dta      runDtArea
     c     *dtaara       define    waxmlda       parmData
     c                   in        parmData                             60
     c                   select
     c                   when      @@id = 'SECURITY'
     c                   if        @@layout = 'WZP1P'
     c                   callp     procSecFund
     c                   endif
     c                   endsl
     c                   seton                                        lr
     *====================================================================
      * procSecFund
     *====================================================================
     pprocSecFund      b
     dprocSecFund      pi
     d xxrrn           s              9  0

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

     c                   move      filerrn       xxrrn
     c     xxrrn         chain     wdpap
     c                   if        %found
     c*
     c     cmky          chain     wmcml1
     c                   if        %found
     c     rpky          chain     wdrpl3
     c                   if        %found
     c                   eval      P1INFCID   = 'INTF_0407'
     c                   eval      P1INFCTYP  = 'REQUEST-800'
     c                   select
     c                   when      PARETP = 'C'
     c                   eval      P1SRCNAME  = 'INS-SFI'
     c                   other
     c                   eval      P1SRCNAME  = 'INS-SFM'
     c                   endsl
     c                   in        runDtArea
      *** beg chg ***** 11/04/09 ***********************************************
     c*****************  eval      P1DHTRBCHN = '794' + runDtArea
     c                   eval      P1DHTRBCHN = getTrBchN
      *** end chg ***** 11/04/09 ***********************************************
     c                   eval      P1SRCDOCID = getDocID
     c                   eval      P1SRCDATE  = getSysDate
     c                   eval      P1SRCTIME  = getSysTime
     c                   eval      P1TRGNAME  = 'SAP'
     c                   eval      P1TRGDATE  = P1SRCDATE
     c                   eval      P1TRGTIME  = P1SRCTIME
     c                   eval      P1DHBUSARE = CMACL#
     c                   eval      P1DHDOCTYP = 'ZI'
     c                   eval      P1HDDOCNBR = P1SRCDOCID
     c                   eval      P1DHRECTYP = 'DH'
     c                   eval      P1DHTRNCDE = 'FB60'
     c                   eval      P1DHHDAMT  = getPayAmt
     c                   eval      P1DHPAYDTE = getPayDate
     c                   eval      P1DHINVDTE = cvtToIso(PATRDT)
     c                   eval      P1DHPARTNM = get1099flag
      *** beg chg ***** 11/13/09 ***********************************************
     c***********        eval      P1DHPYMTHS = getPayMS
     c                   eval      P1DHPYMTHS = '08'
      *** end chg ***** 11/13/09 ***********************************************
     c                   eval      P1DHPAYBLK = 'A'
     c                   eval      P1DHPAYTRM = 'P066'
     c                   callp     getName (name1 : name2)
      *** beg add ***** 09/30/09 ***********************************************
     c                   eval      P1DHNOTES  = getNotes
      *** end add ***** 09/30/09 ***********************************************
      *** beg chg ***** 11/04/09 ***********************************************
     c*****************  eval      P1DTFUND   = getFund
     c                   eval      P1DTFUND   = '5006300000'
      *** end chg ***** 11/04/09 ***********************************************
     c                   eval      P1DTGLACCT = getGLacct
      *** beg chg ***** 11/04/09 ***********************************************
     c***********        eval      P1DTCSTCNT = '7955310000'
      *** beg chg ***** 07/13/12 ***********************************************
     c***********        eval      P1DTCSTCNT = '7955320000'
      *** end chg ***** 11/04/09 ***********************************************
     c                   eval      P1DTCSTCNT = '7955340000'
      *** end chg ***** 07/13/12 ***********************************************
     c                   eval      P1DTRECTYP = 'DT'
     c                   eval      P1DTLNITM  = '001'
     c                   eval      P1DTAMOUNT = P1DHHDAMT
     c                   eval      P1DTSRVBDT = cvtToIso(PAFRDT)
     c                   eval      P1DTSRVEDT = cvtToIso(PATODT)
     c                   eval      P1DTASGNMT = P1SRCNAME
     c                   eval      P1VDRECTYP = 'DV'
     c                   eval      P1VDVENDOR = P1DHPARTNM
     c                   eval      P1VDNAME1  = name1
     c                   eval      P1VDNAME2  = name2
     c                   eval      P1VDSTRT1  = %xlate(lo : up : RPADR1)
     c                   eval      P1VDSTRT2  = %xlate(lo : up : RPADR2)
     c                   eval      P1VDCITY   = %xlate(lo : up : RPCTY )
     c                   eval      P1VDSTATE  = %xlate(lo : up : RPST  )
      *** beg chg ***** 09/30/09 ***********************************************
     c*****************  eval      P1VDPSTCD  = %char(RPZIP)
     c                   eval      P1VDPSTCD  = %editc(RPZIP : 'X')
      *** end chg ***** 09/30/09 ***********************************************
     c*** beg chg ***** 10/18/10 ***********************************************
     c*** beg chg ***** 07/10/12 ***********************************************
     c                   eval      P1VDCNTRY  = 'US'
     c*****              eval      P1VDCNTRY  = 'US'
     c*****
     c***** Add logic to load out of USA address and country
     c*****
     c*****                   select
     c*****
     c*****                   when      RPCTRY <> 'USA' and RPCTRY <> *blanks
     c*****                   eval      P1VDSTRT2  = RPADR3
     c*****                   eval      P1VDCNTRY  = RPCTRY
     c*****
     c*****                   other
     c*****                   eval      P1VDCNTRY  = 'US'
     c*****                   endsl
     c*****
     c*** end chg ***** 07/10/12 ***********************************************
     c*** end chg ***** 10/18/10 ***********************************************
     c                   if        PAFEIN = 0
     c                   eval      P1VDTAXCD1 = CMSS#
     c                   else
     c                   eval      P1VDTAXCD2 = %editc(PAFEIN : 'X')
     c                   endif
     c                   callp     getBankInfo(payMthd : routing# :
     c                                         account# : acctType)
     c                   eval      P1DHPAYMTH = payMthd
     c                   eval      P1VDBNKKEY = routing#
     c                   eval      P1VDBNKACT = account#
     c                   eval      P1VDBNKTYP = acctType
     c***** Beg Add *** 07/18/16 *******************************************
     c***** Beg Del *** 05/16/18 *******************************************
     c***************    eval      P1dhkey = %EditC(CMco#:'X') +
     c***************               %EditC(CMfnd:'X') + %EditC(CMfYr:'X') +
     c***************               %EditC(CMcase:'X')
     c***************    eval      P1dheob# = %EditC(PAeob#:'X')
     c***** End Del *** 05/16/18 *******************************************
     c***** End Add *** 07/18/16 *******************************************
     c                   if        errorFlag = *off
     c                   movel     P1DATA        XMLDATA
     c                   write     pxm01
     c                   eval      PASTAT   = 'W'
     c                   update    ppa01
     c                   endif
     c                   else
     c                   callp     procError( 'ERROR3' )
     c                   endif
     c                   else
     c                   callp     procError( 'ERROR2' )
     c                   endif
     c                   else
     c                   callp     procError( 'ERROR1' )
     c                   endif

     pprocSecFund      e
      *=========================================================================
      * getJulDateTime
      *=========================================================================
     pgetJulDateTime   b
     dgetJulDateTime   pi            15
     d date            s               d
     d julDate         s               d   datfmt(*jul)
     d date5a          s              5
     d time6n          s              6  0
     d time6a          s              6
     d time            s               t
     c                   eval      date = %date
     c                   move      date          julDate
     c     *jul0         move      julDate       date5A
     c                   eval      time = %time
     c                   move      time          time6n
     c                   move      time6n        time6a
     c                   return    'X800' + date5A + time6A
     pgetJulDateTime   e
      *=========================================================================
      * getSysDate
      *=========================================================================
     pgetSysDate       b
     dgetSysDate       pi            10
     d sysDate         s               d         datfmt(*iso)
     d date10a         s             10
     c                   eval      sysDate = %date
     c                   move      sysDate       date10a
     c                   return    date10a
     pgetSysDate       e
      *=========================================================================
      * getSysTime
      *=========================================================================
     pgetSysTime       b
     dgetSysTime       pi             8
     d sysTime         s               t
     d time8a          s              8
     c                   eval      sysTime = %time
     c     *hms:         move      sysTime       time8a
     c                   return    time8a
     pgetSysTime       e
      *=========================================================================
      * getPayAmt
      *=========================================================================
     pgetPayAmt        b
     d getPayAmt       pi            16
     d amt             s             16
     d amt10           s             10
     c                   eval      amt10 = %editc(PAAMT : '4')
     c                   move      amt10         amt
     c                   eval      amt = %xlate(' ' : '0' : amt)
     c                   if        PAAMT >= 0
     c                   movel     '+'           amt
     c                   else
     c                   movel     '-'           amt
     c                   endif
     c                   return    amt
     pgetPayAmt        e
      *=========================================================================
      * getPayDate
      *=========================================================================
     pgetPayDate       b
     dgetPayDate       pi            10
     d isoDate         s               d   datfmt(*iso)
     d date10a         s             10
     c     *usa0         move      runDate       isoDate
     c                   move      isoDate       date10a
     c                   return    date10a
     pgetPayDate       e
      *=========================================================================
      * cvtToISO
      *=========================================================================
     pcvtToISO         b
     d cvtToISO        pi            10
     d    date7n                      7  0 const
     d isoDate         s               d   datfmt(*iso)
     d date10a         s             10
     c                   monitor
     c     *cymd         move      date7n        isodate
     c                   on-error
     c                   callp     procError('ERROR5')
     c                   return    *blanks
     c                   endmon
     c                   move      isodate       date10a
     c                   return    date10a
     pcvtToISO         e
      *=========================================================================
      * getName
      *=========================================================================
     pgetName          b
     d getName         pi
     d    name1                      35
     d    name2                      35
      *** beg chg ***** 10/15/09 *******************************************
     c***********        if        PAFEIN <> 0
     c***********        eval      name1 = %subst(RPPNAM :  1 : 35)
     c***********        eval      name2 = %subst(RPPNAM : 36 : 5 )
     c***********        else
     c***********        eval      name1 = CMFSN + ' ' + CMMID
     c***********        eval      name2 = CMLSN
     c***********        endif
     c                   eval      name1 = %subst(RPPNAM :  1 : 35)
     c                   eval      name2 = %subst(RPPNAM : 36 : 5 )
      *** end chg ***** 10/15/09 *******************************************
     c                   eval      name1 = %xlate(lo : up : name1)
     c                   eval      name2 = %xlate(lo : up : name2)
     pgetName          e
      *=========================================================================
      * getDocID
      *=========================================================================
     pgetDocID         b
     d getDocID        pi            16
     d docID           s             16
     d IDtmp           s             16
     c                   move      *zeros        docID
     c                   eval      IDtmp = PABR7 +  %editc(PACASE : 'X')
     c                                           +  %editc(PACHK# : 'X')
     c                   move      IDtmp         docID
     c                   return    docID
     pgetDocID         e
      *=========================================================================
      * getBankInfo
      *=========================================================================
     pgetBankInfo      b
     d getBankInfo     pi
     d    payMthd                     2
     d    routing#                    9
     d    account#                   17
     d    acctType                    2
     c     lefky         klist
     c                   kfld                    PACO#
     c                   kfld                    kktype            1
     c                   kfld                    kkfnd             3 0
     c                   kfld                    PAFEIN
     c                   kfld                    PAMOD
     c     lefky1        klist
     c                   kfld                    PACO#
     c                   kfld                    kktype
     c                   kfld                    kkfnd
     c                   kfld                    PAFEIN
     c                   kfld                    PAMOD
     c                   kfld                    PAPODT
     c     l3efky        klist
     c                   kfld                    PACO#
     c                   kfld                    PAFND
     c                   kfld                    PAFYR
     c                   kfld                    PACASE
     c                   kfld                    PADSQ#
     c     l3efky1       klist
     c                   kfld                    PACO#
     c                   kfld                    PAFND
     c                   kfld                    PAFYR
     c                   kfld                    PACASE
     c                   kfld                    PADSQ#
     c                   kfld                    PAPODT

     c                   select
     c                   when      PAFEIN <> 0
     c                   eval      kktype = 'P'
     c     lefky1        setgt     pef0l
     c     lefky         readpe    pef0l
     c                   if        %eof
     c                   eval      kkfnd = 0
     c     lefky1        setgt     pef0l
     c     lefky         readpe    pef0l
     c                   endif
     c                   if        not(%eof)
     c                   movel     L0EFID#       routing#
     c                   movel     L0EFACCT      account#
     c                   movel     L0EFACTP      acctType
     c                   if        L0EFID# = *blank
     c***** Beg Chg ***** 04/09/15 ***************************************
     c*********          eval      payMthd = '01'
     c                   eval      payMthd = '06'
     c***** End Chg ***** 04/09/15 ***************************************
     c                   else
     c                   eval      payMthd = '02'
     c                   endif
     c                   else
     c***** Beg Chg ***** 04/09/15 ***************************************
     c*******            eval      payMthd = '01'
     c                   eval      payMthd = '06'
     c***** End Chg ***** 04/09/15 ***************************************
     c                   endif
     c                   other
     c     l3efky1       setgt     pef0l3
     c     l3efky        readpe    pef0l3
     c                   if        not(%eof)
     c                   movel     L3EFID#       routing#
     c                   movel     L3EFACCT      account#
     c                   movel     L3EFACTP      acctType
     c                   if        L3EFID# = *blank
     c***** Beg Chg ***** 04/09/15 ***************************************
     c*******            eval      payMthd = '01'
     c                   eval      payMthd = '06'
     c***** End Chg ***** 04/09/15 ***************************************
     c                   else
     c                   eval      payMthd = '02'
     c                   endif
     c                   else
     c***** Beg Chg ***** 04/09/15 ***************************************
     c*******            eval      payMthd = '01'
     c                   eval      payMthd = '06'
     c***** End Chg ***** 04/09/15 ***************************************
     c                   endif
     c                   endsl
     pgetBankInfo      e
      *=========================================================================
      * get1099Flag
      *=========================================================================
     pget1099Flag      b
     d get1099Flag     pi            10
     c     pdky          klist
     c                   kfld                    PAFEIN
     c                   kfld                    PAMOD
     c                   if        PAFEIN <> 0
     c     pdky          chain     ppd01
     c                   if        %found and PD1099 = 'Y'
     c                   return    '0000600000'
     c                   endif
     c                   endif
     c                   return    '0000600001'
     pget1099Flag      e
      *=========================================================================
      * getPayMS
      *=========================================================================
     pgetPayMS         b
     d getPayMS        pi             2
     c                   if        CMCTG2 = '02' or CMCTG2 = '07'
     c                   return    '02'
     c                   else
     c                   return    '01'
     c                   endif
     pgetPayMS         e
      *=========================================================================
      * procError
      *=========================================================================
     pprocError        b
     d procError       pi
     d  msg                           8    const
     c     *lock         in        parmData
     c                   eval      runDate = msg
     c                   out       parmData
     pprocError        e
      *=========================================================================
      * getFund
      *=========================================================================
     pgetFund          b
     d getFund         pi            10
     d char4           s              4
     d num4            s              4  0
     c                   move      PAPODT        char4
     c                   move      PAPODT        num4
     c                   if        num4 >= 701
     c                   return    '20193' + %subst(%char(PAPODT):2:2) + '000'
     c                   else
     c                   return    '20193'+%subst(%char(PAPODT-10000):2:2)+'000'
     c                   endif
     pgetFund          e
      *=========================================================================
      * getGLacct - codes provided by CSI. Refer to John McCallister's email in
      *             folder
      *=========================================================================
     pgetGLacct        b
     d getGLacct       pi            10
     c                   select
     c                   when      PABR7 = '60'
     c                   return    '   6602000'
     c                   when      PABR7 = '61'
     c                   return    '   6602002'
     c                   when      PABR7 = '62'
     c                   return    '   6602003'
     c                   when      PABR7 = '63'
     c                   return    '   6602004'
     c                   when      PABR7 = '64'
     c                   return    '   6602005'
     c                   when      PABR7 = '65'
     c                   return    '   6602006'
     c                   other
     c                   callp     procError('ERROR4')
     c                   eval      errorFlag = *on
     c                   return    *blanks
     c                   endsl
     pgetGLacct        e
      *** beg add ****** 09/30/09 **********************************************
      *=========================================================================
      * getNotes - populates Treasury Notes field in XML
      *=========================================================================
     pgetNotes         b
     d getNotes        pi                  like(P1DHNOTES)
     d srvFrDate       s              8
     d srvToDate       s              8
     d workDate        s               d
     d claimantName    s             35
     c     keyWMCDL      klist
     c                   kfld                    CMSS#
     c     keyWMCDL      chain     pcd00
     c                   eval      claimantName =
     c                             %trim(CDLSN) +
     c                             %trim(CDFSN) +
     c                             %trim(CDMID)
     c                   monitor
     c     *cymd         move      PAFRDT        workDate
     c     *mdy          move      workDate      srvFrDate
     c                   on-error
     c                   move      *zeros        srvFrDate
     c                   endmon
     c                   monitor
     c     *cymd         move      PATODT        workDate
     c     *mdy          move      workDate      srvToDate
     c                   on-error
     c                   move      *zeros        srvToDate
     c                   endmon
     c                   select
     c                   when      PARETP = 'C'
     c                   return    %trim(claimantName) + ' ' +
     c                             %editc(CMCASE : 'X') + ' WC Pmt ' +
     c                             srvFrDate + ' To ' + srvToDate
     c                   when      PARETP = 'M'
     c                   return    %trim(claimantName) +
     c                             ' DOS ' + srvFrDate + ' To ' +
     c                             srvToDate + ' Claim# ' + %editc(CMCASE : 'X')
     c                   other
     c                   return    %trim(claimantName) +
     c                             ' DOS ' + srvFrDate + ' To ' +
     c                             srvToDate + ' Inv# ' + %editc(PACHK# : 'X')
     c                   endsl
     pgetNotes         e
      *** end add ****** 09/30/09 **********************************************
      *** beg add ***** 11/04/09 ***********************************************
      *=========================================================================
      * getTrBchN - get Transaction Batch Number
      *=========================================================================
     pgetTrBchN        b
     d getTrBchN       pi                  like(P1DHTRBCHN)
      *** beg del ***** 12/07/09 ***********************************************
      ***********
     c*****b2ky**        klist
     c***********        kfld                    PACO#
     c***********        kfld                    tableKey         10
     c***********        movel     PABR2         tableKey
     c*****b2ky**        chain     pb201
     c***********        if        B2RC01 = 'CO' and
     c***********                  B2RC02 = *blank and
     c***********                  B2RC03 = *blank and
     c***********                  B2RC04 = *blank and
     c***********                  B2RC05 = *blank and
     c***********                  B2RC06 = *blank and
     c***********                  B2RC07 = *blank and
     c***********                  B2RC08 = *blank and
     c***********                  B2RC09 = *blank and
     c***********                  B2RC10 = *blank
     c***********        return    '794' + runDtArea
     c***********        else
     c***********        return    '793' + runDtArea
     c***********        endif
      *** end del ***** 12/07/09 ***********************************************
      *** beg add ***** 12/07/09 ***********************************************
     c                   if        PARETP = 'C'
     c                   return    '794' + runDtArea
     c                   else
     c                   return    '793' + runDtArea
     c                   endif
      *** end add ***** 12/07/09 ***********************************************
     pgetTrBchN        e
      *** end add ***** 11/04/09 ***********************************************
