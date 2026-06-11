     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
    h**************************************************************************
    h/TITLE W8321R - Check Request Import - Report
     h option(*nodebugio:*srcstmt)
    h**************************************************************************
    h*  DESCRIPTION:  This program reads WZCKP (Check Request Import) and uploa
    h*                the Check Request file, WARPP.  Errors will be detailed i
    h*                with a summary total of accepted and rejected records.
    h*
     h*  Called from:  W8321C
     h*
     h*  Created: 05/22/06 by C:136 for project: 135WGG
    h**************************************************************************
    h* Revisions:
     h*
     H* 08/26/14 C:307 Project: SY354
     H*                -Implement Routing Page Changes.
     H*
     h*  01/28/10  C:249 Project: SY215
     h*            C:189 Retrieve brand Name
     h*                                                               *
     h*  11/29/06 C:53 Support: 53925
     h*           Get the report to function.
     h*
     h*  10/02/06 C:53 Support: 53925
     h*           Make it work.
     h*
    h**************************************************************************
      * Check request import
     fwzckp     if   e             disk
      * Check request file
     fwarpp     o    e             disk
      * Check request file  (co/fnd/fyr/case)
     fwarpl     if   e           k disk    rename(prp01:prp0l) prefix(l)
      * Routing file
     fsmorl     if   e           k disk
      * Claims master (co/fnd/yr/case)
     fwmcml1    if   e           k disk
      * Payee Demographics (fein/mod)
     fwmpdl     if   e           k disk
      * Table Files (co#/key)
     fwtabl     if   e           k disk
     f*** Begin add *** 10/02/06 ***********************************************
     fwmnrl     if   e           k disk
     f*** End add ***** 10/02/06 ***********************************************
     fqsysprt   o    f  132        printer oflind(*inof)
      *=========================================================================
      * Stand alone fields
      *=========================================================================
     d QQCO#           s              3
     d QQBAT#          s              9
     d companyNm       s             60
     d accumTotal      s             17  2 inz(*zero)
     d kkco#           s              3  0
     d kkkey           s             10
     d numOfMths       s              6  0
     d userName        s             50
     d UPUSNM          s                   like(userName)
     d xxque           s                   like(@@que)
     d company         s                   like(CMCO#)
     d errCount        s              6  0 inz(*zero)
     d emptyFileFlag   s              1n   inz(*off)
     d errorFlag       s              1n   inz(*off)
     d dateErrFlg      s              1n   inz(*off)
     d sysDate         s               d   datfmt(*usa)
     d date1           s               d   datfmt(*usa)
     d date2           s               d   datfmt(*usa)
     d sysTime         s               t   timfmt(*hms:)
     d postDate        s               d   datfmt(*usa)
     d currDate        s               d   datfmt(*usa)
     d xxfil#          s                   like(ORFIL#)
     d x               s              2  0
     d line            s            132
     d yrDiff          s              3  0
     d mthDiff         s              2  0
     d rrn             s              9  0
     d date8           s              8  0
     d #days           s              7  0
     d totPrAmt        s              9  2
     d totPrcCt        s              9  0
     d TotRecCt        s              9  0
     d TotRejCt        s              9  0
     d chkType         s                   like(RPCKTP)
     d retCode         s              1
     d fieldDesc       s             15
     d fieldValue      s             20
     d fieldValue2     s             10
      *=========================================================================
      * Constants
      *=========================================================================
     d title           c                   'CHECK REQUEST IMPORT REPORT'
     d noErrorMsg      c                   'Check request import was successful'
     d emptyFileMsg    c                   'No records in file.'
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
      *=========================================================================
      * Arrays
      *=========================================================================
     d err             s             40    dim(15) ctdata perrcd(1)
     d*** beg chg *** 11/29/06 **************
     d*vb********      s             36    dim(4)  ctdata perrcd(1)
     d vb              s             39    dim(4)  ctdata perrcd(1)
     d*** end chg *** 11/29/06 **************
      *=========================================================================
      * Data structures
      *=========================================================================
     d psds          esds                  extname(szq1p)

     d                 ds
     d  CKCLM#                       13  0
     d  kkfnd                         3  0 overlay(CKCLM#)
     d  kkfyr                         3  0 overlay(CKCLM#:4)
     d  kkcase                        7  0 overlay(CKCLM#:7)

     d                 ds
     d data                          50
     d  RPTRDT
     d  RPFRDT
     d  RPTODT
     d  RPTRCD
     d  RPRECD
     d  RPBR2
     d  RPBR7
    d*=========================================================================
    d* Procedure Prototypes
    d*=========================================================================
     d*entry
     d W8321R          pr
     d  @@co#                         3
     d  @@que                        10
     d  @@upld                        1
     d  @@rndt                        7
     d  @@bat#                        9
     d W8321R          pi
     d  @@co#                         3
     d  @@que                        10
     d  @@upld                        1
     d  @@rndt                        7
     d  @@bat#                        9

     d printSuccess    pr
     d printEmptyMsg   pr
     d printTotals     pr
     d GetNextSeq#     pr             5  0
     d GetTrCd         pr             2
     d GetName         pr            40
     d GetAdr1         pr            40
     d GetAdr2         pr            40
     d GetCheckType    pr            10

     d printError      pr
     d  errorMsg#                     2  0 const

     d GetChkType      pr                  extpgm('WA023R1')
     d  mode                          1    const
     d  co#                           3    const
     d  fnd                           3    const
     d  fyr                           3    const
     d  case                          7    const
     d  appl                          2    const
     d  data                         50    const
     d  cktp                         10
     d  rtcd                          1    const
     d/copy sprdate
     d/copy sprcompany
     d/copy sprstring
     d/copy wprusrprms
      *=========================================================================
      * KLISTS
      *=========================================================================
     c     cmky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkcase

     c     pdky          klist
     c                   kfld                    CKFEIN
     c                   kfld                    CKMOD

     c     orky          klist
     c                   kfld                    q1pgm
     c                   kfld                    xxfil#

     c     tbky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkkey

     c*** Begin add *** 10/02/06 ***********************************************
     c     nrky          klist
     c                   kfld                    kkco#
     c                   kfld                    CKFEIN
     c                   kfld                    CKMOD
     c*** End add ***** 10/02/06 ***********************************************
      *=========================================================================
      * Main
      *=========================================================================
     c                   exsr      srRoute
     c                   exsr      srEdit
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
      *=========================================================================
      * srEdit
      *=========================================================================
     c     srEdit        begsr

     c                   read      pck01
     c                   if        %eof
     c                   callp     printEmptyMsg
     c                   eval      emptyFileFlag = *on
     c                   eval      errorFlag     = *on
     c                   endif

     c                   dow       not(%eof)
     c                   eval      errorFlag     = *off
      *
      * No matching claim
      *
     c     cmky          chain     pcm01
     c                   if        not(%found)
     c                   callp     printError(1)
     c                   endif
      *
      * Payee missing or doesn't exist
      *
     c                   if        CKFEIN = 0
     c                   callp     printError(2)
     c                   else
     c     pdky          chain     ppd01
     c                   if        not(%found)
     c                   callp     printError(3)
     c                   endif
     c                   endif
      *
      * Amount can't be zero
      *
     c                   if        CKAMT = 0
     c                   callp     printError(4)
     c                   endif
      *
      * Payment value 1 is missing or invalid
      *
     c                   if        CKPV1 = *blank
     c                   callp     printError(5)
     c                   else
     c                   eval      kkkey = CKPV1
     c     tbky          chain     pb201
     c                   if        not %found
     c                   callp     printError(6)
     c                   endif
     c                   endif
      *** beg add *** 10/02/06 ************************************
      *
      * Check for valid date format
      *
     c                   move      CKFRDT        xtest1            1
     c                   if        xtest1 = ' '
     c                   movel     CKFRDT        xwork7            7
     c                   move      xwork7        xwork8            8 0
     c                   move      xwork8        CKFRDT
     c                   endif
      *
     c                   move      CKTODT        xtest1
     c                   if        xtest1 = ' '
     c                   movel     CKTODT        xwork7
     c                   move      xwork7        xwork8
     c                   move      xwork8        CKTODT
     c                   endif
      *
      *** end add *** 10/02/06 ************************************
      *
      * From date is missing or invalid
      *
     c                   eval      dateErrFlg = *off
     c                   if        CKFRDT = *blanks
     c                   eval      dateErrFlg = *on
     c                   callp     printError(7)
     c                   else
     c                   move      CKFRDT        date8
     c                   move      date8         CKFRDT
     c                   if        dtCheck(date8) = 0
     c                   eval      dateErrFlg = *on
     c                   callp     printError(8)
     c                   endif
     c                   endif

      *
      * To date is missing or invalid
      *
     c                   if        CKTODT = *blanks
     c                   eval      dateErrFlg = *on
     c                   callp     printError(7)
     c                   else
     c                   move      CKTODT        date8
     c                   move      date8         CKTODT
     c                   if        dtCheck(date8) = 0
     c                   eval      dateErrFlg = *on
     c                   callp     printError(8)
     c                   endif
     c                   endif
      *
      * Ensure date sequence
      *
     c                   if        dateErrFlg = *off
     c     *usa0         move      CKFRDT        date1
     c     *usa0         move      CKTODT        date2
     c                   if        date2 < date1
     c                   callp     printError(9)
     c                   endif
     c                   endif
      *
      * Payment value 1 is missing or invalid
      *
     c                   if        CKRECD = *blank
     c                   callp     printError(10)
     c                   else
     c                   eval      kkkey = CKRECD
     c     tbky          chain     prt01
     c                   if        not %found
     c                   callp     printError(11)
     c                   endif
     c                   endif

     c                   eval      totRecCt  = totRecCt + 1
     c                   if        errorFlag = *on
     c                   eval      totRejCt  = totRejCt + 1
     c                   else
     c                   eval      totPrcCt = totPrcCt + 1
     c                   eval      totPrAmt = totPrAmt + CKAMT
     c                   exsr      srUpload
     c                   endif

     c                   read      pck01
     c                   enddo

     c                   if        totRejCt = 0
     c                   callp     printSuccess
     c                   endif

     c                   callp     printTotals

     c                   endsr
      *=========================================================================
      * srRoute
      *=========================================================================
     c     srRoute       begsr
      *
      * Get report info.
      *
     c     @@que         comp      *blank                             090908
     c                   move      *all'_'       line
     c                   eval      sysTime = SystemTime
     c                   eval      sysDate = SystemDate
     c                   move(p)   @@co#         kkco#
     c                   eval      companyNm = GetCompNam(kkco#)
     c                   eval      companyNm = CenterFld(companyNm)
     c                   eval      upusnm = GetUsrName(Q1USER)
     c                   eval      xxque = GetUsrOutq(Q1USER)
     c                   clear                   ORTITL
     c                   eval      xxfil#  = '0'
     c*** Beg Add ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     c*** End add ***** 01/28/10 **************************************
     c     orky          chain     por01                              60
     c                   if        ORTITL = *blank
     c                   eval      ORTITL = title
     c                   endif
     c                   eval      ORTITL  = LeftJstify(ORTITL)
     c                   if        not *in60
      ***** Beg Del ***** 08/26/14 ************************************
     c**********         except    route
      ***** End Del ***** 08/26/14 ************************************
     c                   endif
     c                   eval      ORTITL  = CenterFld(ORTITL)
     c                   eval      *inof = *on

     c                   endsr
      *=========================================================================
     c*    srUpload
      *=========================================================================
     c     srUpload      begsr

     c                   clear                   prp01
     c                   eval      RPCO#  = kkco#
     c                   eval      RPFND  = kkfnd
     c                   eval      RPFYR  = kkfyr
     c                   eval      RPCASE = kkcase
     c                   eval      RPSEQ# = GetNextSeq#
     c                   eval      RPTRDT = dateTo7(sysDate)
     c                   move(p)   RPTRDT        @@rndt
     c                   eval      RPFEIN = CKFEIN
     c                   eval      RPMOD  = CKMOD
     c                   eval      RPAMT  = CKAMT
     c                   eval      RPBR2  = CKPV1
     c                   if        CMLTFL = 'I'
     c                   eval      RPBR7  = 'LT'
     c                   else
     c                   eval      RPBR7  = 'NL'
     c                   endif
     c                   eval      RPNCCI = CMNCCI
     c                   eval      RPFRDT = dateTo7(date1)
     c                   eval      RPTODT = dateTo7(date2)
     c                   eval      RPRETP = RTTYPE
     c                   if        RPRETP = 'C'
     c                   eval      #days = dtDiff(dtMDYY(RPFRDT):dtMDYY(RPTODT))
     c                   if        #days <> 0
     c                   add       1             #days
     c                   eval      RPWKS  = %DIV(#days:7)
     c                   eval      RPDAYS = %REM(#days:7)
     c                   endif
     c                   endif
     c                   eval      RPTRCD = GetTrCd
     c                   eval      RPTYPE = 'P'
     c                   eval      RPDESC = PCDESC
     c                   eval      RPRECD = CKRECD
     c                   eval      RPRMDS = CKCOMM
     c                   eval      RPPNAM = GetName
     c                   eval      RPADR1 = GetAdr1
     c                   eval      RPADR2 = GetAdr2
     c                   eval      RPCTY  = PDCTY
     c                   eval      RPST   = PDST
     c                   eval      RPZIP  = PDZIP
     c                   eval      RPSTAT = 'P'
     c                   eval      RPUSER = Q1USER
     c                   eval      RPPGM  = Q1PGM
     c                   eval      RPTIME = TimeTo6(SystemTime)
     c                   eval      RPDT = DateTo7(SystemDate)
     c                   eval      RPCKTP = GetCheckType
     c                   move      CKRCVD        date8
     c                   if        dtCheck(date8) <> 0
     c                   eval      RPRCVD = dtCYMD(date8)
     c                   endif
     c                   eval      RPSTS  = 'PD'
     c                   eval      RPSENT = 'N'
     c                   eval      RPRQST = Q1USER
     c                   eval      RPIDT  = RPDT
     c                   eval      RPITM  = RPTIME
     c                   eval      RPIUSR = RPUSER
     c                   eval      RPIPGM = RPPGM
     c                   eval      RPCTRY = PDCTRY
     c                   eval      RPADR3 = PDADR3

     c*** Begin add *** 10/02/06 ***********************************************
     c     nrky          chain     pnr01                              99
     c                   if        *in99 = *off and nrstat <> 'I'
     c                   eval      rppgtp = 'G'
     c                   endif
     c*** End add ***** 10/02/06 ***********************************************

     c                   write     prp01

     c                   endsr
      *=========================================================================
      * Output files
      *=========================================================================
     o/copy scopyrout

     oqsysprt   e            route          1

     o*** Beg Chg *****  01/28/10  ************************************
     o****      e            route          1 55
     o***                                           '(C) Copyright Fiserv ISD'
     o***                                     +   0 ', Inc.,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     o*** End Chg *****  01/28/10  ************************************
    o*
    o* Report Heading
    o*
     o          e            head           1 06
     o                       sysTime
     o                       companyNm           96
     o                       sysDate            132
     o          e            head           2
     o                       q1pgm
     o                       ORTITL              96
     o                                          127 'PAGE:'
     o                       page          z    132
     o          e            head           0
     o                       line               132
     o          e            head2          2
     o          e            head1          2
     o                                           13 'Claim Number'
     o                                           33 'Payment Amount'
     o                                           50 'Field in Error'
     o                                           68 'Value of Field'
     o                                           95 'Error Description'
     o          e            det            1
     o*********************  CKCLM#              15 '   -   -       '
     o                       CKCLM#              14
     o                       CKAMT         1     33
     o                       fieldDesc           51
     o                       fieldValue          74
     o                       err(x)             118
     o          e            det1        2
     o                       noErrorMsg          80
     o          e            det2        2
     o                       emptyFileMsg        76
     o          e            total1      3  1
     o                       vb(1)               39
     o                       TotRecCt      1     51
     o          e            total2         1
     o                       vb(2)               39
     o                       TotRejCt      1     51
     o          e            total3         1
     o                       vb(3)               39
     o                       TotPrcCt      1     51
     o          e            total4         1
     o                       vb(4)               39
     o                       TotPrAmt      1     51
      *=====================================================================
     p*=====================================================================
     p* printError - prints error message
     p*=====================================================================
     pprintError       b
     dprintError       pi
     d errorMsg#                      2  0 const

     c                   if        *inof = *on
     c                   eval      *inof = *off
     c                   except    head
     c                   except    head1
     c                   endif

     c                   eval      fieldDesc = *blanks
     c                   eval      fieldValue = *blanks
     c                   eval      fieldValue2 = *blanks

     c*** beg add *** 11/29/06 *****************************
     c                   eval      x = errorMsg#
     c*** end add *** 11/29/06 *****************************

     c                   select
      * No matching claim found.
     c                   when      x = 1
     c                   eval      fieldDesc = 'Claim Number'
     c                   eval      fieldValue = %char(CKCLM#)
      * Payee ID is missing.
     c                   when      x = 2
     c                   eval      fieldDesc = 'FEIN/Mod'
     c                   eval      fieldValue = %char(CKFEIN) + '/'  +
     c                                          %char(CKMOD)
      * No matching payee found.
     c                   when      x = 3
     c                   eval      fieldDesc = 'FEIN/Mod'
     c                   eval      fieldValue = %char(CKFEIN) + '/'  +
     c                                          %char(CKMOD)
      * Check amount is zero.
     c                   when      x = 4
     c                   eval      fieldDesc = 'Check amount'
     c                   eval      fieldValue = %char(CKAMT)
      * Payment value 1 code is missing.
     c                   when      x = 5
     c                   eval      fieldDesc = 'Payment value 1'
     c                   eval      fieldValue = 'blank'
      * Payment value 1 invalid.
     c                   when      x = 6
     c                   eval      fieldDesc = 'Payment value 1'
     c                   eval      fieldValue = CKPV1
      * Dates of service missing.
     c                   when      x = 7
     c                   eval      fieldDesc = 'From - To dates'
     c                   eval      fieldValue = CKFRDT
     c                   eval      fieldValue2 = CKTODT
     c     fieldValue    cat       '-':1         fieldValue
     c     fieldValue    cat       fieldValue2:1 fieldValue
      * Invalid dates of service.
     c                   when      x = 8
     c                   eval      fieldDesc = 'From - To dates'
     c                   eval      fieldValue = CKFRDT
     c                   eval      fieldValue2 = CKTODT
     c     fieldValue    cat       '-':1         fieldValue
     c     fieldValue    cat       fieldValue2:1 fieldValue
      * Dates of service are out of sequence.
     c                   when      x = 9
     c                   eval      fieldDesc = 'From - To dates'
     c                   eval      fieldValue = CKFRDT
     c                   eval      fieldValue2 = CKTODT
     c     fieldValue    cat       '-':1         fieldValue
     c     fieldValue    cat       fieldValue2:1 fieldValue
      * Reserve code missing.
     c                   when      x = 10
     c                   eval      fieldDesc = 'Reserve code'
     c                   eval      fieldValue = CKRECD
      * Invalid reserve code.
     c                   when      x = 11
     c                   eval      fieldDesc = 'Reserve code'
     c                   eval      fieldValue = CKRECD
     c                   endsl

     c                   eval      errorFlag = *on
     c*** beg del *** 11/29/06 *****************************
     c***********        eval      x = errorMsg#
     c*** end del *** 11/29/06 *****************************
     c                   except    det

     pprintError       e
     p*=====================================================================
     p* printSuccess - prints successful upload message
     p*=====================================================================
     pprintSuccess     b
     dprintSuccess     pi

     c                   if        *inof = *on
     c                   eval      *inof = *off
     c                   except    head
     c                   endif

     c                   except    det1

     pprintSuccess     e
     p*=====================================================================
     p* printEmptyMsg - prints Empty Message
     p*=====================================================================
     pprintEmptyMsg    b
     dprintEmptyMsg    pi

     c                   if        *inof = *on
     c                   eval      *inof = *off
     c                   except    head
     c                   endif

     c                   except    det2

     pprintEmptyMsg    e
     p*=====================================================================
     p* printTotals - prints Totals
     p*=====================================================================
     pprintTotals      b
     dprintTotals      pi

     c                   eval      *inof = *on

     c                   if        *inof = *on
     c                   eval      *inof = *off
     c                   except    head
     c                   except    head2
     c                   endif
     c                   except    total1

     c                   if        *inof = *on
     c                   eval      *inof = *off
     c                   except    head
     c                   except    head2
     c                   endif
     c                   except    total2

     c                   if        *inof = *on
     c                   eval      *inof = *off
     c                   except    head
     c                   except    head2
     c                   endif
     c                   except    total3

     c                   if        *inof = *on
     c                   eval      *inof = *off
     c                   except    head
     c                   except    head2
     c                   endif
     c                   except    total4

     pprintTotals      e
     p*=====================================================================
     p* GetNextSeq# - gets next seqence number from WARPP
     p*=====================================================================
     pGetNextSeq#      b
     dGetNextSeq#      pi             5  0

     d kkseq#          s              5  0 inz(*hival)

     c     rpky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkcase

     c     rpky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkcase
     c                   kfld                    kkseq#

     c     rpky1         setgt     prp0l
     c     rpky          readpe    prp0l
     c                   if        not(%eof)
     c                   eval      kkseq# = lRPSEQ# + 1
     c                   else
     c                   eval      kkseq# = 1
     c                   endif

     c                   return    kkseq#

     pGetNextSeq#      e
     p*=====================================================================
     p* GetTrCd - gets transaction code from WTPCP
     p*=====================================================================
     pGetTrCd          b
     dGetTrCd          pi             2

     d trCd            s              2

     c*** beg add *** 10/02/06 ****************************
     c     *loval        setll     ppc01
     c*** end add *** 10/02/06 ****************************

     c                   read      ppc01
     c                   dow       not(%eof)
     c                   if        PCTRTP = 'P' and PCRECD = CKRECD
     c                   eval      trCd = PCKEY
     c                   leave
     c                   endif
     c                   read      ppc01
     c                   enddo

     c                   return    trCd

     pGetTrCd          e
     p*=====================================================================
     p* GetName - gets provider name
     p*=====================================================================
     pGetName          b
     dGetName          pi            40

     d name            s             40    inz(*blanks)

     c
     c                   if        PDDBA1 = 'Y'
     c                   eval      name = PDADR1
     c                   else
     c                   if        PDFSN <> *blanks
     c                   eval      name = PDFSN
     c                   cat       PDNAME:1      name
     c                   else
     c                   eval      name = PDNAME
     c                   endif
     c                   endif

     c                   return    name

     pGetName          e
     p*=====================================================================
     p* GetAdr1 - gets first address line
     p*=====================================================================
     pGetAdr1          b
     dGetAdr1          pi            40

     d adr1            s             40    inz(*blanks)

     c
     c                   if        PDDBA1 = 'Y'
     c                   eval      adr1 = PDADR2
     c                   else
     c                   eval      adr1 = PDADR1
     c                   endif

     c                   return    adr1

     pGetAdr1          e
     p*=====================================================================
     p* GetAdr2 - gets first address line
     p*=====================================================================
     pGetAdr2          b
     dGetAdr2          pi            40

     d adr2            s             40    inz(*blanks)

     c
     c                   if        PDDBA1 <> 'Y'
     c                   eval      adr2 = PDADR2
     c                   endif

     c                   return    adr2

     pGetAdr2          e
     p*=====================================================================
     p* GetCheckType - Gets check type
     p*=====================================================================
     pGetCheckType     b
     dGetCheckType     pi            10

     d chkType         s                   like(RPCKTP) inz(*blanks)
     d co#             s              3
     d grp             s              3
     d fyr             s              3
     d case            s              7

     c                   move      RPCO#         co#
     c                   move      RPFND         grp
     c                   move      RPFYR         fyr
     c                   move      RPCASE        case
     c                   callp     GetChkType('2':co#:grp:fyr:case:
     c                                        'WC':data:chkType:retCode)
     c                   return    chkType

     pGetCheckType     e

** err(x) Error messages
No matching claim found.
Payee ID is missing.
No matching payee found.
Check amount is zero.
Payment value 1 code is missing.
Payment value 1 invalid.
Dates of service missing.
Invalid dates of service.
Dates of service are out of sequence.
Reserve code missing.
Invalid reserve code.
Not used
Not used
Not used
Not used
** tvb(x) Totals Verbiage
Total number of records processed . . :
Total number of records rejected  . . :
Total number of check requests created:
Total amount of check requests created:
