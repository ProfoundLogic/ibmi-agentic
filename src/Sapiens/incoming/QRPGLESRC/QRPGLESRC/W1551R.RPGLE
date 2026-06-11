    H* (C) Copyright Fiserv, Inc., 2005 - 2007
    H*****************************************************************
    H/TITLE W7551R - PRINT SCHDULED RATING DOCUMENT
    H*****************************************************************
    H*                                                               *
    H*  10/17/05 C:156 PROJ#: 114WKY                                 *
    H*                 ORGINAL                                       *
    H*****************************************************************
    H*                                                               *
    H* REVISIONS:                                                    *
    H*                                                               *
     H* 08/20/07 c:65   Project: Supp# 59600 (WA114/53245)            *
     H*                 Do not write darts remark with a 'W' value    *
     H*                 for the environment.  'Q' is the only valid   *
     H*                 non-blank value.  Use a blank instead of 'W'. *
     H*                                                               *
     H* 04/13/07 C:132  PROJECT: 114WMS                               *
     H*               - Change WMRFL to be used for Update.           *
     H*               - If chain to WMRFL results in a Record Found,  *
    H*                 continue processing.                          *
    H*               - After writing to printer, move "Y" to RFPRTF  *
    H*                 and update file WMRFL.                        *
    H*                                                               *
     H* 04/04/07    C:180      WA114/53245                            *
     H*                        Ensure sched rating info remarks are   *
     H*                        written to DARTS when in quote mode.   *
    H*                                                               *
    H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
    H*    -------------------------------------------------------    *
    H*****************************************************************
    H* INDICATOR USAGE                                               *
    H*****************************************************************
     F*
      ***** BEG CHG ***** 04/13/07 ************************************
     F*******wmrfl     if   e           k disk
     Fwmrfl     UF   e           k disk
      ***** END CHG ***** 04/13/07 ************************************
     Fwmfnl     if   e           k disk
     Fwmrpl2    if   e           k disk
     Fwmupl     if   e           k disk
     Fsmorl     if   e           k disk
     Fsmcol     if   e           k disk
     Fwmeml     if   e           k disk
     Fwdell1    if   e           k disk
     FSTCTL1    if   e           k disk
     FSSYLF     if   e           k disk    ignore(spc01:sfb01:src01:sdr01:
     F                                            sbc01:scs01:sgl01:sp201)
     Fw1551o    o    e             printer oflind(*in10)
     *
     D/copy sPrDate

     DDarts            pr
     D*
     D                 ds
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0
    
     D                 ds
     D cezip                          9  0
     D  cezip1                        5  0 overlay(cezip)
     D  cezip2                        4  0 overlay(cezip:6)
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D emp             s              9
     D fund            s              3
     D qqco#           s              3
     D qqdcid          s             12
     D qqdiv           s              5
     D qqemp#          s              9
     D qqfnd           s              3
     D qqfold          s             80
     D qqfyr           s              3
      ******* Begin add ******** 04/04/07 **********************************
     D qqqte           s              1
      ******* End   add ******** 04/04/07 **********************************
     D xxacc           s              7  0
     D xxadr1          s             40
     D xxadr2          s             40
     D xxagg           s              7  0
     D xxco#           s              3  0
     D xxcty           s             25
     D xxdis           s              7  0
     D xxdiv           s                   like(emdiv)
     D xxemp#          s                   like(ememp#)
     D xxfnd           s              3  0
     D xxfrst          s             15
     D xxfyr           s              3  0
     D xxlast          s             56
     D xxprt           s              1
     D xxRemark        s              5    inz('SHRAT')
     D xxst            s              2
     D xxstzp          s             13
     D xxst10          s             10
     D xxterm          s              1
     D xxzip           s              9  0
     D xxzip4          s              4
     D xxzip5          s              5
     D xx5             s              5
     D year            s              3
     D sysDate         s               d
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
    C*   KEY LIST FOR THE OVERRIDE FILE
    C*
     C     orkey         klist
     C                   kfld                    q1pgm
     C                   kfld                    xxprt
    C*
    C*   KEY LIST FOR THE COMPANY FILE
    C*
     C     cokey         klist
     C                   kfld                    xxco#
    C*
    C*   KEY LIST FOR THE USER PARAMETER FILE
    C*
     C     upkey         klist
     C                   kfld                    q1user
    C*
    C*   KEY LIST FOR THE EMPLOYER MASTER FILE
    C*
     C     emkey         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
    C*
     C     emkey1        klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
    C*
     C     emkey2        klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
    C*
    C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
    C*
     C     elkey         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
    C*
    C*  FUND MASTER
    C*
     C     fnky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     *
     *  Carrier
     *
     C     rpky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxfyr
     *
     *  Scheduled Rating
     *
     C     rfpky1        klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxfyr
     C*
     C     keySTCTL1     klist
     C                   kfld                    xxRemark
     *
     C                   eval      sysDate = SystemDate
     C*
     C                   eval      xxacc = *zeros
     C                   eval      xxdis = *zeros
     C                   eval      xxagg = *zeros
    C*
     C     *entry        plist
     C                   parm                    qqco#
     C                   parm                    qqfnd
     C                   parm                    qqfyr
     C                   parm                    qqemp#
     C                   parm                    qqdiv
     C                   parm                    qqfold
     C                   parm                    qqdcid
      ******* BEGIN ADD  ********** 04/04/07 *******************************
     C                   parm                    qqqte
      ******* END   ADD  ********** 04/04/07 *******************************
    C*
     C                   move      qqco#         xxco#
     C                   move      qqfnd         xxfnd
     C                   move      qqfyr         xxfyr
     C                   move      qqemp#        xxemp#
     C                   move      qqdiv         xxdiv
    C*
    C*   SET-UP AND INITIALIZE
    C*
    C*
    C*   GET THE DISTRIBUTION INFORMATION
    C*
     C                   move      '0'           xxprt
     C     orkey         chain     por01                              60
    C*
    C*   GET THE COMPANY INFORMATION
    C*
     C     cokey         chain     pco01                              60
    C*
    C*   GET THE FUND NAME
    C*
     C     fnky          chain     pfn01                              60
    C*
    C*   GET THE USER PARAMETER INFORMATION
    C*
     C     upkey         chain     pup01                              60
     C                   if        *in60 = *on
     C                   clear                   upusnm
     C                   endif
    C*
    C*   CHAIN TO THE EMPLOYER FILE FOR ALL EMPLOYERS FOR FUND
    C*
     C     emkey1        chain     pem01                              50
    C*
    C*   RETRIEVE EMPLOYER DETAIL FILE
    C*
     C     elkey         chain     pel01                              51
    C*
     C                   if        *in51 <> *on
    C*
    C*   GET EMPLOYERS ADDRESS (PHYSICAL OR MAILING)
    C*
     C                   exsr      sub1
     C                   endif
    C*
    C*   GET THE SCHEDULE RATING INFORMATION
    C*
     C     rfpky1        chain     rfp01                              70
      *
      ***** BEG ADD ***** 04/13/07 ************************************
      * If no record, then bypass and terminate program.
     C     *in70         cabeq     *on           TERM
      ***** END ADD ***** 04/13/07 ************************************
     *
     * FORMAT DATES
     *
     C                   movel     '-'           hyphen
     C                   eval      RPFEFF = AlphaUSA(CvtToDate(ELPFDT))
     C                   eval      RPSREF = AlphaUSA(CvtToDate(RFSREF))
     C                   eval      RPSREX = AlphaUSA(CvtToDate(RFSREX))
     *
     C                   move      ' '           rpcmp1
     C                   move      ' '           rpcmp2
     C                   move      ' '           rpcmp3
     C                   select
     C                   when      rfcofl = '1'
     C                   move      'X'           rpcmp1
     C                   when      rfcofl = '2'
     C                   move      'X'           rpcmp2
     C                   when      rfcofl = '3'
     C                   move      'X'           rpcmp3
     C                   endsl
     *
     C                   select
     C                   when      rflscf = 'Y'
     C                   move      'X'           rplsys
     C                   move      ' '           rplsno
     C                   eval      RPLSDT = AlphaUSA(CvtToDate(RFLSDT))
     C                   other
     C                   move      ' '           rplsys
     C                   move      'X'           rplsno
     C                   eval      RPLSDT = *blanks
     C                   endsl
     *
     C                   select
     C                   when      rfpefl = 'Y'
     C                   move      'X'           rppeys
     C                   move      ' '           rppeno
     C                   eval      rppeem = rfpeem
     C                   other
     C                   move      ' '           rppeys
     C                   move      'X'           rppeno
     C                   eval      rppeem = *zero
     C                   endsl
     *
     C                   movel     rfprrs        rpprr1
     C                   move      rfprrs        rpprr2
     C                   movel     rfcprs        rpcpr1
     C                   move      rfcprs        rpcpr2
     C                   movel     rfmfrs        rpmfr1
     C                   move      rfmfrs        rpmfr2
     C                   movel     rfsdrs        rpsdr1
     C                   move      rfsdrs        rpsdr2
     C                   movel     rfemrs        rpemr1
     C                   move      rfemrs        rpemr2
     C                   movel     rfmcrs        rpmcr1
     C                   move      rfmcrs        rpmcr2
     C                   movel     rfmsrs        rpmsr1
     C                   move      rfmsrs        rpmsr2
     *
     C                   eval      totpdr = *zero
     C                   eval      totpdr = totpdr + rfprda
     C                   eval      totpdr = totpdr + rfcpda
     C                   eval      totpdr = totpdr + rfmfda
     C                   eval      totpdr = totpdr + rfsdda
     C                   eval      totpdr = totpdr + rfemda
     C                   eval      totpdr = totpdr + rfmcda
     C                   eval      totpdr = totpdr + rfmsda
     C                   eval      totpcr = *zero
     C                   eval      totpcr = totpcr + rfprca
     C                   eval      totpcr = totpcr + rfcpca
     C                   eval      totpcr = totpcr + rfmfca
     C                   eval      totpcr = totpcr + rfsdca
     C                   eval      totpcr = totpcr + rfemca
     C                   eval      totpcr = totpcr + rfmcca
     C                   eval      totpcr = totpcr + rfmsca
    C*
    C*   RETRIEVE CARRIER INFORMATION FOR POLICY NUMBER
    C*
     C     rpky          chain     prp01                              51
    C*
     C                   if        *in51 <> *on
    C*
    C*   FORMAT POLICY NUMBER
    C*
     C                   movel     rpcap#        rppol#
     C                   else
     C                   move      rffnd         fund
     C                   move      rfemp#        emp
     C                   move      rffyr         year
     C     fund          cat       emp:0         rppol#
     C     rppol#        cat       year:0        rppol#
     C                   endif
     *
     C                   write     srfdet
      *
      ***** BEG ADD ***** 04/13/07 ************************************
      * Update the print flag in WMRFP/RFPRTF
     C                   eval      RFPRTF = 'Y'
     C                   MOVEL     Q1USER        RFUSER
     C                   MOVEL     Q1PGM         RFPGM
     C                   TIME                    DSDTTM
     C                   eval      RFDT   = DateTo7(CvtToDate(DSDATE))
     C                   MOVEL     DSTIME        RFTIME
      *
     C                   update    rfp01
      ***** END ADD ***** 04/13/07 ************************************
     C*
     C     keySTCTL1     chain     pct01
     C                   if        %found and xxRemark <> *blanks
     C                   callp     Darts
     C                   endif
    C*
    C*   END PROGRAM
      *
      ***** BEG ADD ***** 04/13/07 ************************************
     C     TERM          TAG
      ***** END ADD ***** 04/13/07 ************************************
    C*
     C                   move      *on           *inlr
    C*
     C*====================================================================
    C*   SUBROUTINE FOR EMPLOYER INFORMATION AND PRINT CERTIFICATE
     C*====================================================================
    C*
     C     sub1          begsr
    C*
     C                   movel     emname        rfenam
    C*
    C*   GET EMPLOYERS ADDRESS (PHYSICAL OR MAILING)
    C*
     C                   clear                   xxadr1
     C                   clear                   xxadr2
     C                   clear                   xxcty
     C                   clear                   xxst
     C                   eval      xxzip = 0
     C                   if        emmpfl <> 'M'
     C                   movel     empad1        xxadr1
     C                   movel     empad2        xxadr2
     C                   movel     empcty        xxcty
     C                   movel     empst         xxst
     C                   eval      xxzip = empzip
     C                   else
     C                   movel     emmad1        xxadr1
     C                   movel     emmad2        xxadr2
     C                   movel     emmcty        xxcty
     C                   movel     emmst         xxst
     C                   eval      xxzip = emmzip
     C                   endif
    C*
    C*   COMBINE CITY STATE AND ZIP
    C*
     C                   clear                   xxlast
     C                   clear                   xxfrst
     C                   clear                   xxstzp
     C                   clear                   xxzip5
     C                   clear                   xxzip4
     C                   movel     xxcty         xxlast
     C                   movel     xxst          xxstzp
     C                   movel     xxzip         xxzip5
     C                   move      xxzip         xxstzp
     C                   move      xxstzp        xxzip4
    C*
     C                   if        xxzip4 = *zero
     C                   move      '    '        xxstzp
     C                   else
     C                   movel     '-'           xx5
     C                   move      xxzip4        xx5
     C                   movel     xxzip5        xxst10
     C                   move      xx5           xxst10
     C                   move      xxst10        xxstzp
     C                   endif
    C*
    C*  CONCATENATE CITY, STATE AND ZIP
    C*
     C                   movel     xxstzp        xxfrst
     C                   move      '1'           xxterm
     C                   call      'SRNAME'
     C                   parm                    xxlast
     C                   parm                    xxfrst
     C                   parm                    xxterm
     C*
     C                   eval      rpEAdr1 = xxadr1
     C                   eval      rpEAdr2 = xxadr2
     C                   eval      rpEAdr3 = xxlast
     C*
     C                   if        rpEAdr1 = *blanks
     C                   eval      rpEAdr1 = rpEAdr2
     C                   eval      rpEAdr2 = rpEAdr3
     C                   eval      rpEAdr3 = *blanks
     C                   endif
     C*
     C                   if        rpEAdr2 = *blanks
     C                   eval      rpEAdr2 = rpEAdr3
     C                   eval      rpEAdr3 = *blanks
     C                   endif
    C*
     C                   endsr





     C*====================================================================
    C* Write DARTS remark
     C*====================================================================
     PDarts            b

     DDarts            pi

     D                 ds
     D qckey                         40
     D  qcco#                         3  0 overlay(qckey)
     D  qcfnd                         3  0 overlay(qckey:4)
     D  qcemp#                        9  0 overlay(qckey:7)
     D  qcdiv                         5  0 overlay(qckey:16)
      ******   Beg Add 04/04/07 ********************************************
     D  qcqte                         1    overlay(qckey:40)
      ******   Beg Add 04/04/07 ********************************************
     D*
     D qqflr           s             80
     D qqpflr          s             80
     D qqpre           s              2
     D qqrtcd          s              1
     D qqsub#          s              5
     D*
     D q#apid          s              2
     D q#cdat          s              8
     D q#code          s              2
     D q#comm          s            256
     D q#com2          s             19
     D q#csid          s              2
     D q#dcid          s             12
     D q#dctp          s              5
     D q#difl          s              1
     D q#fl01          s              1
     D q#fl02          s              1
     D q#fold          s             80
     D q#idoc          s             12
     D q#imgf          s              1
     D q#ipgm          s             10
     D q#ipth          s             10
     D q#ique          s             10
     D q#key           s             40
     D q#pgm           s             10
     D q#pidx          s              5
     D q#pnam          s             10
     D q#qpty          s              1
     D q#rmcd          s              5
     D q#sbyl          s              1
     D q#secl          s              3
     D q#seq#          s              5
     D q#user          s             10
     D syKey           s              2    inz('SY')
    C*
     C     keySSYLF      klist
     C                   kfld                    syKey
     C*
     C                   clear                   SYOFIF
     C     keySSYLF      chain     ssy01
    C*
     C                   call      'SNXTDCID'
     C                   parm      *blanks       qqflr
     C                   parm      *blanks       qqdcid
     C                   parm      'RS'          qqpre
     C                   parm                    qqco#
     C                   parm                    qqfnd
     C                   parm      'P'           qqrtcd
     C                   parm      '00000'       qqsub#
     C                   parm      *blanks       qqpflr
    C*
     C                   if        qqpflr <> *blanks
     C                   eval      qqpflr = %trim(qqpflr) + '/'
     C                   endif
    C*
     C                   eval      qqfold = %trim(qqpflr) + %trim(qqflr)
     C*
     C                   eval      qcCo#  = xxCo#
     C                   eval      qcFnd  = xxFnd
     C                   eval      qcEmp# = xxEmp#
     C                   eval      qcDiv  = xxDiv
      ******* Beg Add 04/04/07 *********************************************
     C                   eval      qcqte  = qqqte
     c*** begin add ***  08/20/07  *****************************************
     c                   if        qcqte = 'W'
     c                   move      *blank        qcqte
     c                   endif
     c*** end add *****  08/20/07  *****************************************
      ******* Beg Add 04/04/07 *********************************************
     C*
     C                   eval      q#Key  = qcKey
     C                   eval      q#Comm = CTDESC
     C                   eval      q#CDat = %editc(DateTo8(sysDate):'Z')
     C                   eval      q#PNam = 'W0050R'
     C                   eval      q#User = q1user
     C                   eval      q#Pgm  = q1pgm
     C                   eval      q#IPgm = q1pgm
     C                   eval      q#RmCd = xxRemark
     C                   eval      q#Seq# = '00000'
     C                   eval      q#Sbyl = 'N'
     C                   eval      q#ImgF = 'O'
     C*
     C                   select
     C                   when      SYOFIF = 'Y'
     C                   eval      q#Fold = qqFlr
     C                   eval      q#DcId = qqDcId
     C                   eval      q#DiFl = 'N'
     C                   endsl
     C*
     C                   select
     C                   when      CTDPRG > 0
     C                   eval      q#Fl02 = 'Y'
     C                   endsl
     C*
     C                   clear                   q#com2
     C                   clear                   q#secl
     C                   clear                   q#fl01
     C                   clear                   q#fl02
     C                   clear                   q#apid
     C                   clear                   q#idoc
     C                   clear                   q#ique
     C                   clear                   q#ipth
     C                   clear                   q#pidx
     C                   clear                   q#qpty
     C                   clear                   q#dctp
     C                   clear                   q#csid
     C                   clear                   q#code
     *
     C                   call      'DA700R'
     C                   parm                    q#key
     C                   parm                    q#comm
     C                   parm                    q#cdat
     C                   parm                    q#dcid
     C                   parm                    q#pnam
     C                   parm                    q#secl
     C                   parm                    q#user
     C                   parm                    q#pgm
     C                   parm                    q#fold
     C                   parm                    q#difl
     C                   parm                    q#ipgm
     C                   parm                    q#fl01
     C                   parm                    q#fl02
     C                   parm                    q#rmcd
     C                   parm                    q#seq#
     C                   parm                    q#sbyl
     C                   parm                    q#com2
     C                   parm                    q#apid
     C                   parm                    q#idoc
     C                   parm                    q#ique
     C                   parm                    q#imgf
     C                   parm                    q#ipth
     C                   parm                    q#pidx
     C                   parm                    q#qpty
     C                   parm                    q#dctp
     C                   parm                    q#csid
     C                   parm                    q#code

     PDarts            e
