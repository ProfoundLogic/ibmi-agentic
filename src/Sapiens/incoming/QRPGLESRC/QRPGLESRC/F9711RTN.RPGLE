     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -                          *
     H StoneRiver is a trademark of StoneRiver, Inc.')                                     *
    H*
    H/TITLE F9711RTN - Tennessee Data Submission - Extract Program
     H DFTACTGRP(*NO) ACTGRP(*caller)
    H*****************************************************************
    H*                                                               *
    H*  Called from: F9711CTN - TN Data Submission - Override        *
    H*  Calls to   : None                                            *
    H*                                                               *
    H*****************************************************************
    H*  Created on 10/30/2006 by c:62 for Project TN004              *
    H*****************************************************************
    H*  Revisions:                                                   *
    H*                                                               *
    H*  11/25/08 C:062  PROJECT 10MLN                                *
    H*                  Default Dental Diagnosis code when blank     *
    H*                                                               *
    H*  10/02/07 C:062  PROJECT 10MKJ                                *
    H*                  Remove claim adj st logic.                   *
    H*                                                               *
    H*  05/16/07 C:188  PROJECT 10MKJ                                *
    H*                  Added logic to call FA900R/FA9001 for clt.10 *
    H*                                                               *
    H*  12/13/06 C:182  PROJECT TN005                                *
    H*                  Added logic to use the Cross Reference       *
    H*                  Specialty Code.                              *
    H*****************************************************************
     Ffaeopltn  if   e           k disk
     Ffmc1l     if   e           k disk
     Ffmfnl     if   e           k disk
     Ffmf1l     if   e           k disk
     Ffaehl     if   e           k disk
     Ffaedl     if   e           k disk
     Ffddal     if   e           k disk
     Ffdcal5    if   e           k disk
     Ffddul     if   e           k disk
     Ffddpl     if   e           k disk
     Ffdddl     if   e           k disk
     Ffddhl     if   e           k disk
    F*** BEG CHG ***** 05/16/07 ***********************************************
     F*****fmcml1    if   e           k disk
     Ffmcml1    if   e           k disk    infds(dbfds)
    F*** END CHG ***** 05/16/07 ***********************************************
     Fsmupl     if   e           k disk
    F*** BEG ADD ***** 12/13/06 ***********************************************
     F***ftspl1    if   e           k disk
    F*** END ADD ***** 12/13/06 ***********************************************
     Ffaeol4    if   e           k disk
     Ffaeal1    if   e           k disk
     Ffaeul     if   e           k disk
     Ffemeptn   o    e             disk    usropn

     D psds          esds                  extname(szq1p)

     D @@qtr           s              1
     D @@year          s              4
     D @@dcid          s             25

     D hdco#           s                   like(eoco#) inz(0)
     D hdsub#          s                   like(eosub#) inz(0)
     D hdfnd           s                   like(eofnd) inz(0)
     D hdfyr           s                   like(eofyr) inz(0)
     D hdcase          s                   like(eocase) inz(0)

     d i               s              3s 0
     d x               s              3s 0
     d length          s              3s 0
     d text            s            100a   varying
     d text2           s            100a   varying
     d xxdate          s              8a
     d claimID         s             16a
     d billID          s             17a
     d xxacdt          s              8a
     d xxfrdt          s              8a
     d xxtodt          s              8a
     d xxsvdt          s              8a
     d xxcrdt          s              8a
     d xxwcpd          s              8a
     d groupName       s                   like(fnname)
     d zipCode         s              5a
     d cptCode         s              5a
     d hcpcCode        s              5a
     d amb             s              1a
     d providerType    s              2a
     d providerName    s             25a
     d diagnosis       s              5a
     d ndcCode         s             13a
     d svrrn           s              9  0

     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8

     D submitted       s               n   inz(*off)
     D submitter       s              5  0 inz(0)
     D hospInpat       s              5s 0 inz(0)
     D hospOutpat      s              5s 0 inz(0)
     D physician       s              5s 0 inz(0)
     D pharmacy        s              5s 0 inz(0)

     d                 ds
     d docID                         25
     d  subID                        10    overlay(docID)
     d  year                          2    overlay(docID:11)
     d  quarter                       1    overlay(docID:13)
     d  currentDate                   8  0 overlay(docID:14)
     d  period                        1    overlay(docID:22) inz('.')
     d  extension                     3    overlay(docID:23) inz('XML')

     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)

     d                 ds
     d ndc                           11
     d  ndc1                          5    overlay(ndc)
     d  ndc2                          4    overlay(ndc:6)
     d  ndc3                          2    overlay(ndc:10)

     d fs              s              3    dim(120) ctdata perrcd(1)
     d ms              s              2    dim(120) alt(fs)

     d heading1        c                   const('<?xml version="1.0" encoding=-
     d                                     "UTF-8" ?>')
     d*heading2        c                   const('<MEDFEEINFO xmlns:xsi=-
     d*                                    "http://www.w3.org/2001/XMLSch-
     d*                                    ema-instance" xsi:noNamespace-
     d*                                    SchemaLocation="C:/DOCUME~1-
     d*                                    /cc10918.K31787/Desktop-
     d*                                    /medfeeschd.xsd">')
     d heading2        c                   const('<MEDFEEINFO xmlns:xsi=-
     d                                     "http://www.w3.org/2001/XMLSch-
     d                                     ema-instance">')
     d*
    d*** BEG ADD ***** 05/16/07 ***********************************************
     d dbfds           ds
     d dbrrn                 397    400B 0
    d*** BEG ADD ***** 05/16/07 ***********************************************
     d*
    C*****************************************************************
    C*   Parameter Lists
    C*****************************************************************
     C     *entry        plist
     C                   parm                    @@qtr
     C                   parm                    @@year
     C                   parm                    @@dcid
     C*
     C     dtyymd1       plist
     C                   parm                    srdti
     C                   parm      '*MDYY'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtyymd2       plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*****************************************************************
    C*   Key Lists
    C*****************************************************************
     C     caky          klist
     C                   kfld                    eaco#
     C                   kfld                    easub#
     C                   kfld                    eafnd
     C                   kfld                    eafyr
     C                   kfld                    eacase
     C                   kfld                    eaeob#
     C                   kfld                    dadseq

     C     cmky          klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase

     C     detky         klist
     C                   kfld                    eaco#
     C                   kfld                    easub#
     C                   kfld                    eafnd
     C                   kfld                    eafyr
     C                   kfld                    eacase
     C                   kfld                    eaeob#
     C                   kfld                    dadseq
     c*
     C     daky          klist
     C                   kfld                    eaco#
     C                   kfld                    easub#
     C                   kfld                    eafnd
     C                   kfld                    eafyr
     C                   kfld                    eacase
     C                   kfld                    eaeob#
     C                   kfld                    earcn
     c*
     C     eaky          klist
     C                   kfld                    tnco#
     C                   kfld                    tnsub#
     C                   kfld                    tnfnd
     C                   kfld                    tnfyr
     C                   kfld                    tncase
     C                   kfld                    tneob#
     C                   kfld                    tnrcn
     c*
     C     eoky          klist
     C                   kfld                    tnco#
     C                   kfld                    tnsub#
     C                   kfld                    tnfnd
     C                   kfld                    tnfyr
     C                   kfld                    tncase
     C                   kfld                    tneob#
     c*
     C     euky          klist
     C                   kfld                    tnco#
     C                   kfld                    tnsub#
     C                   kfld                    tnfnd
     C                   kfld                    tnfyr
     C                   kfld                    tncase
     C                   kfld                    tneob#
     c*
     c     fnky          klist
     c                   kfld                    eaco#
     c                   kfld                    easub#
     c                   kfld                    eafnd
     c*
     C     hdrky         klist
     C                   kfld                    eaco#
     C                   kfld                    easub#
     C                   kfld                    eafnd
     C                   kfld                    eafyr
     C                   kfld                    eacase
     C                   kfld                    eaeob#
     c*
    c*** BEG ADD ***** 12/13/06 ***********************************************
     c***
     c***  sppky         klist
     c***                kfld                    eaco#
     c***                kfld                    eospcd
     c***
    c*** END ADD ***** 12/13/06 ***********************************************
     c*
    C**********************************************************************
    C*  Main Processing
    C**********************************************************************
     c*
    c*** BEG ADD ***** 05/16/07 ***********************************************
     c                   call      'RTVCLTID'
     c                   parm      *blanks       ##CLTID           3
     c                   open      femeptn
     c                   exsr      getSubmission#
     c                   exsr      resetStatus
    c*** END ADD ***** 05/16/07 ***********************************************
     c*
     c                   exsr      crtHeading
     c*
     C                   read      ptn01
     c                   if        not %eof
     c                   dow       not %eof
     c     eoky          chain     peo01
     c     eaky          chain     pea01
     c     euky          chain     peu01
     c     cmky          chain     pcm01
     c*** begin chg *** 10/02/07 *******************************************
     c**********         if        %found and cmast = 'TN'
     c                   if        %found
     c*** end   chg *** 10/02/07 *******************************************
     c*
     c                   if        hdco# <> eoco# or hdsub# <> eosub# or
     c                             hdfnd <> eofnd or hdfyr <> eofyr or
     c                             hdcase <> eocase
     c*
     c                   if        hdcase <> 0
    c*** BEG ADD ***** 05/16/07 ***********************************************
     c                   if        ##CLTID='010'
     c                   close     femeptn
     c                   call      'FA901R'
     c                   parm      svrrn         ##RRN             9 0
     c     physician     parm      physician     ##CNT             5 0
     c     submitter     parm      submitter     ##sub             5 0
     c                   parm      @@qtr         ##qtr             1
     c                   parm      @@year        ##year            4
     c                   parm      *blanks       ##call            1
     c                   call      'FA900R'
     c                   parm      svrrn         ##RRN             9 0
     c     pharmacy      parm      pharmacy      ##CNT             5 0
     c     submitter     parm      submitter     ##sub             5 0
     c                   parm      @@qtr         ##qtr             1
     c                   parm      @@year        ##year            4
     c                   parm      *blanks       ##call            1
     c                   open      femeptn
     c                   endif
    c*** END ADD ***** 05/16/07 ***********************************************
     c                   exsr      endSubmitter
     c                   endif
     c*
     c                   eval      hdco# = eoco#
     c                   eval      hdsub# = eosub#
     c                   eval      hdfnd = eofnd
     c                   eval      hdfyr = eofyr
     c                   eval      hdcase = eocase
     c                   eval      svrrn = dbrrn

     c                   eval      hospInpat = 0
     c                   eval      hospOutpat = 0
     c                   eval      physician = 0
     c                   eval      pharmacy = 0

     c                   exsr      crtSubmitter
     c                   endif
     c*
     c                   select
     c                   when      eosys = 'H' and
     c                             (euicn = 'IM' or euicn = 'IP' or
     c                              euicn = 'IR' or euicn = 'IS')
     c                   exsr      crtHospInpat
     c                   when      eosys = 'H' and
     c                             (euicn = 'AO' or euicn = 'AS' or
     c                              euicn = 'HO' or euicn = 'HS')
     c                   exsr      crtHospOutpat
     c                   when      eosys = 'H'
     c                   exsr      crtHospOutpat
     c                   when      eosys = 'P' and
     c                             (eobtyp = 'D' or eobtyp = 'N')
     c                   exsr      crtPhysician
     c                   when      eosys = 'P' and eobtyp = 'X'
     c                   exsr      crtPharmacy
     c                   endsl
     c*
     c                   endif
     C                   read      ptn01
     c                   enddo
     c*
     c                   if        ##CLTID='010'
     c                   close     femeptn
     c                   call      'FA901R'
     c                   parm      svrrn         ##RRN             9 0
     c     physician     parm      physician     ##CNT             5 0
     c     submitter     parm      submitter     ##sub             5 0
     c                   parm      @@qtr         ##qtr             1
     c                   parm      @@year        ##year            4
     c                   parm      *blanks       ##call            1
     c                   call      'FA900R'
     c                   parm      svrrn         ##RRN             9 0
     c     pharmacy      parm      pharmacy      ##CNT             5 0
     c     submitter     parm      submitter     ##sub             5 0
     c                   parm      @@qtr         ##qtr             1
     c                   parm      @@year        ##year            4
     c                   parm      *blanks       ##call            1
     c                   open      femeptn
     c                   endif
     c                   exsr      endSubmitter
     c                   endif
     c*
    c*** BEG ADD ***** 05/16/07 ***********************************************
     c                   if        ##CLTID='010'
     c                   close     femeptn
     c                   call      'FA901R'
     c                   parm      *zeros        ##RRN             9 0
     c                   parm      *zeros        ##CNT             5 0
     c     submitter     parm      submitter     ##sub             5 0
     c                   parm      @@qtr         ##qtr             1
     c                   parm      @@year        ##year            4
     c                   parm      *blanks       ##call            1
     c                   call      'FA900R'
     c                   parm      *zeros        ##RRN             9 0
     c                   parm      *zeros        ##CNT             5 0
     c     submitter     parm      submitter     ##sub             5 0
     c                   parm      @@qtr         ##qtr             1
     c                   parm      @@year        ##year            4
     c                   parm      *blanks       ##call            1
     c                   open      femeptn
     c                   endif
    c*** END ADD ***** 05/16/07 ***********************************************
     c                   exsr      endHeading
     c                   exsr      genDocID
     c                   close     femeptn
     c*
     c                   seton                                        lr
     c*=====================================================================
     c* Create blank hospital inpatient section
     c*=====================================================================
     c     crtBlankHI    begsr
     c*
     c                   add       1             hospInpat
     c*
     c                   eval      medata = '<HOSPINPAT><RecNbrDtl1>' +
     c                             %trim(%editc(hospInpat:'4')) +
     c                             '</RecNbrDtl1><BillID>' +
     c                             '</BillID><LengthStayFrom>' +
     c                             '</LengthStayFrom><LengthStayTo>' +
     c                             '</LengthStayTo>' +
     c                             '<ICD9ProcedureCode>' +
     c                             '</ICD9ProcedureCode><DiagnosisCode>' +
     c                             '</DiagnosisCode>' +
     c                             '<NPICode></NPICode><DateOfService>' +
     c                             '</DateOfService>' +
     c                             '<FacilityZipCode>' +
     c                             '</FacilityZipCode><TotalAmountPaid>' +
     c                             '0.00</TotalAmountPaid><DateBilled>' +
     c                             '</DateBilled><DatePaid>' +
     c                             '</DatePaid></HOSPINPAT>'
     c                   write     pme01
     c*
     c                   endsr
     c*=====================================================================
     c* Create blank hospital outpatient section
     c*=====================================================================
     c     crtBlankHO    begsr
     c*
     c                   add       1             hospOutpat
     c*
     c                   eval      medata = '<HOSPOUTPAT><RecNbrDtl2>' +
     c                             %trim(%editc(hospOutpat:'4')) +
     c                             '</RecNbrDtl2><BillID>' +
     c                             '</BillID><CPTCode>' +
     c                             '</CPTCode><HCPCCode>' +
     c                             '</HCPCCode><DiagnosisCode>' +
     c                             '</DiagnosisCode>' +
     c                             '<NPICode></NPICode><DateOfService>' +
     c                             '</DateOfService>' +
     c                             '<AmountPaid>' +
     c                             '0.00</AmountPaid>' +
     c                             '<AmbulatoryCenter>' +
     c                             '</AmbulatoryCenter><FacilityZipCode>' +
     c                             '</FacilityZipCode>' +
     c                             '<DateBilled>' +
     c                             '</DateBilled><DatePaid>' +
     c                             '</DatePaid></HOSPOUTPAT>'
     c                   write     pme01
     c*
     c                   endsr
     c*=====================================================================
     c* Create blank physician section
     c*=====================================================================
     c     crtBlankPhys  begsr
     c*
     c                   add       1             physician
     c*
     c                   eval      medata = '<OTHTHNHIO><RecNbrDtl3>' +
     c                             %trim(%editc(physician:'4')) +
     c                             '</RecNbrDtl3><BillID>' +
     c                             '</BillID><CPTCode>' +
     c                             '</CPTCode><HCPCCode>' +
     c                             '</HCPCCode><DiagnosisCode>' +
     c                             '</DiagnosisCode>' +
     c                             '<NPICode></NPICode><DateOfService>' +
     c                             '</DateOfService>' +
     c                             '<AmountPaid>' +
     c                             '0.00</AmountPaid>' +
     c                             '<FacilityZipCode>' +
     c                             '</FacilityZipCode><TypeOfProvider>' +
     c                             '</TypeOfProvider>' +
     c                             '<FacProviderFein>' +
     c                             '</FacProviderFein><FacProviderName>' +
     c                             '</FacProviderName>' +
     c                             '<DateBilled>' +
     c                             '</DateBilled><DatePaid>' +
     c                             '</DatePaid></OTHTHNHIO>'
     c                   write     pme01
     c*
     c                   endsr
     c*=====================================================================
     c* Create blank pharmacy section
     c*=====================================================================
     c     crtBlankPhar  begsr
     c*
     c                   add       1             pharmacy
     c*
     c                   eval      medata = '<NDC><RecNbrDtl4>' +
     c                             %trim(%editc(pharmacy:'4')) +
     c                             '</RecNbrDtl4><BillID>' +
     c                             '</BillID><NDCCode>' +
     c                             '</NDCCode><DiagnosisCode>' +
     c                             '</DiagnosisCode>' +
     c                             '<NPICode></NPICode><DateOfService>' +
     c                             '</DateOfService>' +
     c                             '<AmountPaid>' +
     c                             '0.00</AmountPaid>' +
     c                             '<DateBilled>' +
     c                             '</DateBilled><DatePaid>' +
     c                             '</DatePaid></NDC>'
     c                   write     pme01
     c*
     c                   endsr
     c*=====================================================================
     c* Create heading
     c*=====================================================================
     c     crtHeading    begsr
     c*
     c                   clear                   pme01
     c                   eval      medata = heading1
     c                   write     pme01
     c                   eval      medata = heading2
     c                   write     pme01
     c*
     c                   endsr
     c*=====================================================================
     c* Create submission header section
     c*=====================================================================
     c     crtSubmitter  begsr
     c*
     c                   clear                   pme01
     c                   add       1             submitter
     c                   eval      submitted = *on

     c                   eval      text = c1name
     c                   eval      length = 60
     c                   exsr      stripBadChars
     c                   eval      c1name = text

     c                   time                    dsdttm
     c                   z-add     dsdate        srdti
     c                   call      'SRCVTDAT'    dtyymd1
     c                   movel     srdto         xxdate

     C                   eval      claimID = %trim(%editw(eoco#:'0   ')) +
     C                                  %trim(%editw(eofnd:'0   ')) +
     C                                  %trim(%editw(eofyr:'0   ')) +
     C                                  %trim(%editw(eocase:'0       '))

     c                   z-add     eoacdt        srdti
     c                   call      'SRCVTDAT'    dtyymd2
     c                   movel     srdto         xxacdt

     c     fnky          chain     pfn01
     c     fnky          chain     pf101
     c                   eval      text = fnname
     c                   eval      length = 60
     c                   exsr      stripBadChars
     c                   eval      fnname = text
     c                   movel     fnmzip        zipCode

     c                   eval      medata = '<SUBMITTER><RecNbr>' +
     c                             %trim(%editc(submitter:'4')) +
     c                             '</RecNbr><SubmitterId>' + c110a3 +
     c                             '</SubmitterId><SubmitterName>' +
     c                             %trim(c1name) + '</SubmitterName>' +
     c                             '<SubmitterFein>' +
     c                             %trim(%editw(c1fein:'0         ')) +
     c                             '</SubmitterFein><DateOfSubmission>' +
     c                             xxdate + '</DateOfSubmission>' +
     c                             '<TimeOfSubmission>' + %editc(dstime:'4') +
     c                             '</TimeOfSubmission><InsurerClaimNbr>' +
     c                             claimID + '</InsurerClaimNbr>' +
     c                             '<DateOfInjury>' + xxacdt +
     c                             '</DateOfInjury><InsurerName>' +
     c                             %trim(fnname) +
     c                             '</InsurerName><InsurerFein>' +
     c                             %trim(%editw(f1afei:'0         ')) +
     c                             '</InsurerFein><InsurerZip>' +
     c                             zipCode + '</InsurerZip><ReportQuarter>' +
     c                             @@qtr + '</ReportQuarter>' +
     c                             '<ReportYear>' + @@year + '</ReportYear>'
     c                   write     pme01
     c*
     c                   endsr
     c*=====================================================================
     c* Create hospital inpatient section
     c*=====================================================================
     c     crtHospInpat  begsr
     c*
     c                   add       1             hospInpat

     C                   eval      billID = %trim(%editw(eaeob#:'0         ')) +
     C                                  %trim(%editw(earcn:'0   '))

     c                   z-add     eofrdt        srdti
     c                   call      'SRCVTDAT'    dtyymd2
     c                   movel     srdto         xxfrdt

     c                   z-add     eotodt        srdti
     c                   call      'SRCVTDAT'    dtyymd2
     c                   movel     srdto         xxtodt

     c                   z-add     eofrdt        srdti
     c                   call      'SRCVTDAT'    dtyymd2
     c                   movel     srdto         xxsvdt

     c                   z-add     eocrdt        srdti
     c                   call      'SRCVTDAT'    dtyymd2
     c                   movel     srdto         xxcrdt

     c                   move      eawcpd        xxwcpd
     c                   movel     eazip         zipCode

     c                   eval      medata = '<HOSPINPAT><RecNbrDtl1>' +
     c                             %trim(%editc(hospInpat:'4')) +
     c                             '</RecNbrDtl1><BillID>' + %trim(billID) +
     c                             '</BillID><LengthStayFrom>' + xxfrdt +
     c                             '</LengthStayFrom><LengthStayTo>' +
     c                             xxtodt + '</LengthStayTo>' +
     c                             '<ICD9ProcedureCode>' +
     c                             '</ICD9ProcedureCode><DiagnosisCode>' +
     c                             %trim(eudia1) + '</DiagnosisCode>' +
     c                             '<NPICode></NPICode><DateOfService>' +
     c                             xxsvdt + '</DateOfService>' +
     c                             '<FacilityZipCode>' + zipCode +
     c                             '</FacilityZipCode><TotalAmountPaid>' +
     c                             %trim(%editw(eapaid:'       0 .  ')) +
     c                             '</TotalAmountPaid><DateBilled>' +
     c                             xxcrdt + '</DateBilled><DatePaid>' +
     c                             xxwcpd + '</DatePaid></HOSPINPAT>'
     c                   write     pme01
     c*
     c                   endsr
     c*=====================================================================
     c* Create hospital outpatient section
     c*=====================================================================
     c     crtHospOutpat begsr
     c*
     c*                  if        hospInpat = 0
     c*                  exsr      crtBlankHI
     c*                  endif
     c*
     c     daky          chain     pda01
     c                   if        %found
     c                   dou       %eof
     c                   if        dapaid>0
     c*
     c     caky          chain     pca01
     c     detky         chain     pdu01
     c*
     c                   add       1             hospOutpat

     C                   eval      billID = %trim(%editw(eaeob#:'0         ')) +
     C                                  %trim(%editw(earcn:'0   ')) +
     C                                  %trim(%editw(dadseq:'0     '))

     c                   if        durate <> *blanks
     c                   if        durate < '00000'
     c                   eval      cptCode = *blanks
     c                   eval      hcpcCode = durate
     c                   else
     c                   eval      cptCode = durate
     c                   eval      hcpcCode = *blanks
     c                   endif
     c                   else
     c                   eval      cptCode = '99499'
     c                   eval      hcpcCode = *blanks
     c                   endif

     c                   z-add     cafdat        srdti
     c                   call      'SRCVTDAT'    dtyymd2
     c                   movel     srdto         xxsvdt

     c                   if        dupos = '24'
     c                   eval      amb = 'Y'
     c                   else
     c                   eval      amb = 'N'
     c                   endif

     c                   z-add     eocrdt        srdti
     c                   call      'SRCVTDAT'    dtyymd2
     c                   movel     srdto         xxcrdt

     c                   move      eawcpd        xxwcpd
     c                   movel     eazip         zipCode

     c                   eval      medata = '<HOSPOUTPAT><RecNbrDtl2>' +
     c                             %trim(%editc(hospOutpat:'4')) +
     c                             '</RecNbrDtl2><BillID>' + billID +
     c                             '</BillID><CPTCode>' + %trim(cptCode) +
     c                             '</CPTCode><HCPCCode>' + %trim(hcpcCode) +
     c                             '</HCPCCode><DiagnosisCode>' +
     c                             %trim(eudia1) + '</DiagnosisCode>' +
     c                             '<NPICode></NPICode><DateOfService>' +
     c                             xxsvdt + '</DateOfService>' +
     c                             '<AmountPaid>' +
     c                             %trim(%editw(dapaid:'     0 .  ')) +
     c                             '</AmountPaid><AmbulatoryCenter>' + amb +
     c                             '</AmbulatoryCenter><FacilityZipCode>' +
     c                             zipCode + '</FacilityZipCode>' +
     c                             '<DateBilled>' + xxcrdt +
     c                             '</DateBilled><DatePaid>' + xxwcpd +
     c                             '</DatePaid></HOSPOUTPAT>'
     c                   write     pme01
     c                   endif
     c     daky          reade     pda01
     c                   enddo
     c                   endif
     c*
     c                   endsr
     c*=====================================================================
     c* Create physician section
     c*=====================================================================
     c     crtPhysician  begsr
     c*
     c*                  if        hospInpat = 0
     c*                  exsr      crtBlankHI
     c*                  endif
     c*                  if        hospOutpat = 0
     c*                  exsr      crtBlankHO
     c*                  endif
     c*
     c     daky          chain     pda01
     c                   if        %found
     c                   dou       %eof
     c                   if        dapaid>0
     c*
     c     caky          chain     pca01
     c                   select
     c                   when      eobtyp = 'D'
     c     hdrky         chain     ped01
     c     detky         chain     pdd01
     c                   when      eobtyp = 'N'
     c     hdrky         chain     peh01
     c     detky         chain     pdh01
     c                   endsl
     c*
     c*
     c                   add       1             physician
     c*
     C                   eval      billID = %trim(%editw(eaeob#:'0         ')) +
     C                                  %trim(%editw(earcn:'0   ')) +
     C                                  %trim(%editw(dadseq:'0     '))

     c                   if        dhprcd <> *blanks
     c                   if        dhprcd < '00000'
     c                   eval      cptCode = *blanks
     c                   eval      hcpcCode = dhprcd
     c                   else
     c                   eval      cptCode = dhprcd
     c                   eval      hcpcCode = *blanks
     c                   endif
     c                   else
     c                   eval      cptCode = '99499'
     c                   eval      hcpcCode = *blanks
     c                   endif

     c                   if        eobtyp = 'D'
     c*** begin chg *** 11/25/08 *******************************************
     c                   if        diagnosis <> ' '
     c*** end   chg *** 11/25/08 *******************************************
     c                   eval      diagnosis = eddia1
     c*** begin chg *** 11/25/08 *******************************************
     c                   else
     c                   eval      diagnosis = '9599'
     c                   endif
     c*** end   chg *** 11/25/08 *******************************************
     c                   else
     c                   eval      diagnosis = ehdia1
     c                   endif

     c                   z-add     cafdat        srdti
     c                   call      'SRCVTDAT'    dtyymd2
     c                   movel     srdto         xxsvdt

    c*** BEG ADD ***** 12/13/06 ***********************************************
     c***  sppky         chain     psp01
     c***                if        %found(ftspl1)
     c***                eval      providerType = spxref
     c***                else
    c*** END ADD ***** 12/13/06 ***********************************************
     c                   eval      x = 1
     c     eospcd        lookup    fs(x)                                  99
     c                   if        *in99
     c                   eval      providerType = ms(x)
     c                   else
     c                   eval      providerType = '99'
     c                   endif
    c*** BEG ADD ***** 12/13/06 ***********************************************
     c***                endif                                                  psp01
    c*** END ADD ***** 12/13/06 ***********************************************

     c                   movel     eapnam        providerName

     c                   eval      text = providerName
     c                   eval      length = 25
     c                   exsr      stripBadChars
     c                   eval      providerName = text

     c                   z-add     eocrdt        srdti
     c                   call      'SRCVTDAT'    dtyymd2
     c                   movel     srdto         xxcrdt

     c                   move      eawcpd        xxwcpd
     c                   movel     eazip         zipCode

     c                   eval      medata = '<OTHTHNHIO><RecNbrDtl3>' +
     c                             %trim(%editc(physician:'4')) +
     c                             '</RecNbrDtl3><BillID>' + billID +
     c                             '</BillID><CPTCode>' + %trim(cptCode) +
     c                             '</CPTCode><HCPCCode>' + %trim(hcpcCode) +
     c                             '</HCPCCode><DiagnosisCode>' +
     c                             %trim(diagnosis) + '</DiagnosisCode>' +
     c                             '<NPICode></NPICode><DateOfService>' +
     c                             xxsvdt + '</DateOfService>' +
     c                             '<AmountPaid>' +
     c                             %trim(%editw(dapaid:'     0 .  ')) +
     c                             '</AmountPaid><FacilityZipCode>' + zipCode +
     c                             '</FacilityZipCode><TypeOfProvider>' +
     c                             providerType + '</TypeOfProvider>' +
     c                             '<FacProviderFein>' +
     c                             %trim(%editw(eafein:'0         ')) +
     c                             '</FacProviderFein><FacProviderName>' +
     c                             %trim(providerName) + '</FacProviderName>' +
     c                             '<DateBilled>' + xxcrdt +
     c                             '</DateBilled><DatePaid>' + xxwcpd +
     c                             '</DatePaid></OTHTHNHIO>'
     c                   write     pme01
     c*
     c                   endif
     c     daky          reade     pda01
     c                   enddo
     c                   endif
     c*
     c                   endsr
     c*=====================================================================
     c* Create pharmacy section
     c*=====================================================================
     c     crtPharmacy   begsr
     c*
     c*                  if        hospInpat = 0
     c*                  exsr      crtBlankHI
     c*                  endif
     c*                  if        hospOutpat = 0
     c*                  exsr      crtBlankHO
     c*                  endif
     c*                  if        physician = 0
     c*                  exsr      crtBlankPhys
     c*                  endif
     c*
     c                   if        ##CLTID='010'
     c                   close     femeptn
     c                   call      'FA901R'
     c                   parm      dbrrn         ##RRN             9 0      rn o
     c     physician     parm      physician     ##CNT             5 0
     c     submitter     parm      submitter     ##sub             5 0
     c                   parm      @@QTR         ##QTR             1
     c                   parm      @@YEAR        ##YEAR            4
     c                   parm      *blanks       ##call            1
     c                   open      femeptn
     c                   endif
     c*
     c     daky          chain     pda01
     c                   if        %found
     c                   dou       %eof
     c                   if        dapaid>0
     c*
     c     caky          chain     pca01
     c     detky         chain     pdp01
     c*
     c*
     c                   add       1             pharmacy
     c*
     C                   eval      billID = %trim(%editw(eaeob#:'0         ')) +
     C                                  %trim(%editw(earcn:'0   ')) +
     C                                  %trim(%editw(dadseq:'0     '))

     c                   eval      ndc = dpndc
     c                   eval      ndcCode = ndc1 + '-' + ndc2 + '-' + ndc3

     c                   z-add     cafdat        srdti
     c                   call      'SRCVTDAT'    dtyymd2
     c                   movel     srdto         xxsvdt

     c                   z-add     eocrdt        srdti
     c                   call      'SRCVTDAT'    dtyymd2
     c                   movel     srdto         xxcrdt

     c                   move      eawcpd        xxwcpd

     c                   eval      medata = '<NDC><RecNbrDtl4>' +
     c                             %trim(%editc(pharmacy:'4')) +
     c                             '</RecNbrDtl4><BillID>' + billID +
     c                             '</BillID><NDCCode>' + ndcCode +
     c                             '</NDCCode><DiagnosisCode>' +
     c                             '</DiagnosisCode>' +
     c                             '<NPICode></NPICode><DateOfService>' +
     c                             xxsvdt + '</DateOfService>' +
     c                             '<AmountPaid>' +
     c                             %trim(%editw(dapaid:'     0 .  ')) +
     c                             '</AmountPaid><DateBilled>' + xxcrdt +
     c                             '</DateBilled><DatePaid>' + xxwcpd +
     c                             '</DatePaid></NDC>'
     c                   write     pme01
     c*
     c                   endif
     c     daky          reade     pda01
     c                   enddo
     c                   endif
     c*
     c                   endsr
     c*=====================================================================
     c* End submission section
     c*=====================================================================
     c     endSubmitter  begsr
     c*
     c                   if        submitted
     c                   clear                   pme01
     c                   eval      medata = '</SUBMITTER>'
     c                   write     pme01
     c                   eval      submitted = *off
     c                   endif
     c*
     c                   endsr
     c*=====================================================================
     c* End heading section
     c*=====================================================================
     c     endHeading    begsr
     c*
     c                   clear                   pme01
     c                   eval      medata = '</MEDFEEINFO>'
     c                   write     pme01
     c*
     c                   endsr
     c*=====================================================================
     c* Generate the document id
     c*=====================================================================
     c     genDocID      begsr
     c*
     c                   if        submitter <> 0
     c*
     c                   exsr      getSubmission#
     c                   eval      subID = c110a3
     c                   move      @@year        year
     c                   move      @@qtr         quarter
     c                   time                    dsdttm
     c                   z-add     dsdate        srdti
     c                   call      'SRCVTDAT'    dtyymd1
     c                   movel     srdto         currentDate
     c*
     c                   eval      @@dcid = docID
     c*
     c                   endif
     c*
     c                   endsr
     c*=====================================================================
     c* Get the submission number
     c*=====================================================================
     c     getSubmission#begsr
     c*
     c     q1user        chain     pup01
     c     upco#         chain     pc101
     c*
     c                   endsr
     c*=====================================================================
     c* Reset Status
     c*=====================================================================
     c     resetStatus   begsr
     c*
     c                   if        ##cltid = '010'
     c                   call      'FA901R'
     c                   parm      *zeros        ##RRN             9 0
     c                   parm      *zeros        ##CNT             5 0
     c                   parm      *zeros        ##sub             5 0
     c                   parm      @@qtr         ##qtr             1
     c                   parm      @@year        ##year            4
     c                   parm      'R'           ##call            1
     c                   call      'FA900R'
     c                   parm      *zeros        ##RRN             9 0
     c                   parm      *zeros        ##CNT             5 0
     c                   parm      *zeros        ##sub             5 0
     c                   parm      @@qtr         ##qtr             1
     c                   parm      @@year        ##year            4
     c                   parm      'R'           ##call            1
     c                   endif
     c*
     c                   endsr
     c*=====================================================================
     c* Strip Bad Characters
     c*=====================================================================
     c     stripBadChars begsr

     c*
     c* Replace & with amp; We first need to replace with ?amp; then change
     c* the ? back to an &
     c*

     c                   eval      i = %scan('&':text:1)
     c                   dow       i <> 0
     c                   if        i < (length - 4)
     c                   eval      text = %replace('~amp;':text:i:1)
     c                   else
     c                   eval      text = %replace(' ':text:i:1)
     c                   endif
     c                   eval      i = %scan('&':text:1)
     c                   enddo

     c                   eval      i = %scan('~':text:1)
     c                   dow       i <> 0
     c                   eval      text = %replace('&':text:i:1)
     c                   eval      i = %scan('~':text:1)
     c                   enddo

     c*
     c* Replace " with blanks
     c*

     c                   eval      i = %scan('"':text:1)
     c                   dow       i <> 0
     c                   eval      text = %replace('':text:i:1)
     c                   eval      i = %scan('"':text:1)
     c                   enddo

     c*
     c* Replace . with blanks
     c*

     c                   eval      i = %scan('.':text:1)
     c                   dow       i <> 0
     c                   eval      text = %replace(' ':text:i:1)
     c                   eval      i = %scan('.':text:1)
     c                   enddo

     c*
     c* Replace / with blanks
     c*

     c                   eval      i = %scan('/':text:1)
     c                   dow       i <> 0
     c                   eval      text = %replace(' ':text:i:1)
     c                   eval      i = %scan('/':text:1)
     c                   enddo

     c*
     c* Replace , with blanks
     c*

     c                   eval      i = %scan(',':text:1)
     c                   dow       i <> 0
     c                   eval      text = %replace(' ':text:i:1)
     c                   eval      i = %scan(',':text:1)
     c                   enddo

     c*
     c* Replace ¢ with blanks
     c*

     c                   eval      i = %scan('¢':text:1)
     c                   dow       i <> 0
     c                   eval      text = %replace(' ':text:i:1)
     c                   eval      i = %scan('¢':text:1)
     c                   enddo

     c                   eval      text = %trim(text)
     c*
     c                   endsr
** Spcd x-ref
GP 01
GS 02
AL 03
AL103
OL 04
ANE05
CAR06
DER07
DE107
DE207
FAM08
FA108
PAM09
GAS10
IM 11
IM111
IM211
IM311
IM411
IM511
IM611
IM711
IM811
IM911
IMA11
IMB11
IME11
OST12
NEU13
NE113
NES14
OBG16
OB116
OB216
OB416
OPH18
ORS19
ORT20
OR120
OR220
OR320
OR420
OR520
OR620
PAT22
PA122
PA222
PA322
PA422
PA522
PA622
PA722
PA822
PA922
PLS24
PL124
REH25
PSY26
CR 28
IMD29
RA130
THO33
URL34
CHI35
NU 36
NU136
NU236
PED37
PE137
PE237
PE337
PE437
PE537
PE637
PE737
PE937
GER38
NEP39
SU240
OPT41
INF44
END46
POD48
PO148
RHU66
MSP70
PMT72
PM172
PM272
PM372
PM472
CAS76
GVS77
CAR78
ADD79
AN181
HEM82
PE883
PM 84
PM184
PM284
PM384
PM484
MXS85
PN 86
PN186
PN286
IMC90
ONC91
RA692
EM 93
RAD94
RA294
RA394
RA494
RA594
RA794
RA894
RA994
OB398
