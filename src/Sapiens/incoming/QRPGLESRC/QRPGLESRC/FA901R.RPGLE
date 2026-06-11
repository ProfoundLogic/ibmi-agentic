     H**********************************************************************
     H* (C)COPYRIGHT PROGRESSIVE DATA SOLUTIONS INC., 2007
    H**********************************************************************
    H* PROGRAM: FA901R
    H* DESCRIPTION: API - PBM Medical Merge
     H**********************************************************************
     H* CREATED: 07/17/07 C: 188 PROJECT: 10MKJ
     H**********************************************************************
     H* REVISIONS:
     H*
     H* 11/25/08 c:62 Project 10MLN
     H*               Don't send MSC data if code not included
     H*
     H**********************************************************************
     F*
     Ffmcmp     if   e             disk
     Ffmcml1    if   e           k disk    rename(pcm01:pcm02) infds(dbfds)
     Ffmc1l     if   e           k disk
     Ffmfnl     if   e           k disk
     Ffmf1l     if   e           k disk
     Fsmupl     if   e           k disk
     Ffxxhl     uf   e           k disk
     Ffxxhp     uf   e             disk    rename(pxh01:pxh02)
     Ffemeptn   o    e             disk    usropn
     F*
    D*---------------------------------------------------------------------
     D* DS/Standalone field defs
    D*---------------------------------------------------------------------
     D psds          esds                  extname(szq1p)
     D dbfds           ds
     D dbrrn                 397    400b 0
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D @@rrn           s              9  0
     D @@cnt           s              5  0
     D @@sub           s              5  0
     D @@qtr           s              1
     D @@year          s              4
     D @@call          s              1
     D claimky         s             25
     D kkco#           s                   like(cmco#)
     D kksub#          s                   like(cmsub#)
     D kkfnd           s                   like(cmfnd)
     D kkfyr           s                   like(cmfyr)
     D kkcase          s                   like(cmcase)
     D svclm#          s                   like(xhclm#)
     D svfnd           s                   like(cmfnd) inz(0)
     D svfyr           s                   like(cmfyr) inz(0)
     D svcase          s                   like(cmcase) inz(0)
     D xxco#           s              3A
     D xxfnd           s              3A
     D xxfyr           s              3A
     D xxcase          s              7A
     D fromdate        s              8A
     D todate          s              8A
     D*
     D submitted       s               n   inz(*off)
     D submitter       s              5  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D i               s              3s 0
     D x               s              3s 0
     D length          s              3s 0
     D text            s            100a   varying
     D xxdate          s              8a
     D claimID         s             16a
     D xxacdt          s              8a
     D xxfrdt          s              8a
     D xxtodt          s              8a
     D xxsvdt          s              8a
     D xxcrdt          s              8a
     D xxwcpd          s              8a
     D groupName       s                   like(fnname)
     D zipCode         s              5a
     d cptCode         s              5a
     d hcpcCode        s              5a
     d paid            s             11  2
     d fein            s              9  0

     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

    C*---------------------------------------------------------------------
     C* Key lists
    C*---------------------------------------------------------------------
     C     clmky         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C     fnky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C*
     C     xxkey         klist
     C                   kfld                    claimky
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
     C*
     C     *entry        plist
     C                   parm                    @@rrn
     C                   parm                    @@cnt
     C     submitter     parm      submitter     @@sub
     C                   parm                    @@qtr
     C                   parm                    @@year
     C                   parm                    @@call
     C*
     C*=====================================================================
     C* MAIN PROGRAM
     C*=====================================================================
     C                   open      femeptn
     C*
     C** Set date compare fields based on Qtr/Yr from entry parms
     C*
     C                   exsr      setDateRange
     C*
     C** Get claim record using rrn passed to program, read through physician
     C** import file and generate EOBM TN claim output record for each.
     C*
     c                   if        @@call = *blanks
     C                   if        @@rrn <> 0
     C*
     C     @@rrn         chain     pcm01
     C*
     C                   if        %found
     C                   eval      kkco# = cmco#
     C                   exsr      getSubmission#
     C                   endif
     C*
     C                   if        %found
     C                   exsr      buildKey
     C     xxkey         setll     pxh01
     C     xxkey         reade     pxh01
     C                   dow       not %eof
     C*** begin chg *** 11/25/08 *******************************************
     C**********         if        xhpodt >= fromdate and
     C**********                   xhpodt <= todate
     C                   if        xhpodt >= fromdate and
     C                             xhpodt <= todate and xhcode<>' '
     C*** end   chg *** 11/25/08 *******************************************
     C                   exsr      wrtPhysician
     C                   eval      xhstat = 'Y'
     C                   update    pxh01
     C                   endif
     C                   reade     pxh01
     C                   enddo
     C                   endif                                                  claim found
     C                   endif                                                  rrn <> 0
     C*
     C* When rrn=0...
     C* read through FXXPP and process any records not processed already by
     C* finding claim record then generating a physician record for each.
     C*
     C                   if        @@rrn = 0
     C*
     C     *loval        setll     pxh01
     C                   read      pxh01
     C                   dow       not %eof
     C*** begin chg *** 11/25/08 *******************************************
     C**********         if        xhpodt >= fromdate and
     C**********                   xhpodt <= todate and
     C**********                   xhstat = ' '
     C                   if        xhpodt >= fromdate and
     C                             xhpodt <= todate and
     C                             xhstat = ' ' and xhcode<>' '
     C*** end   chg *** 11/25/08 *******************************************
     C*
     C** Chain to user file to get subscriber# for claim key
     C*
     C     q1user        chain     pup01
     C                   if        %found
     C                   exsr      parseKey
     C                   eval      kksub# = upsub#
     C     clmky         chain     pcm02
     C*
     C                   if        %found
     C                   eval      kkco# = cmco#
     C                   exsr      getSubmission#
     C                   if        svfnd <> cmfnd or svfyr <> cmfyr or
     C                             svcase <> cmcase
     C                   exsr      crtSubmitter
     C                   eval      svfnd = cmfnd
     C                   eval      svfyr = cmfyr
     C                   eval      svcase= cmcase
     C                   eval      @@cnt = 0
     c                   endif
     C                   exsr      wrtPhysician
     C                   eval      xhstat = 'Y'
     C                   update    pxh01
     C                   endif
     C                   endif
     C                   endif
     C                   read      pxh01                                  98
     C                   if        %eof  or (xhclm# <> svclm# and not %eof)
     C                   close     femeptn
     c                   call      'FA900R'
     c                   parm      dbrrn         ##RRN             9 0
     c                   parm      *zeros        ##CNT             5 0
     c                   parm      *zeros        ##sub             5 0
     c                   parm      @@qtr         ##qtr             1
     c                   parm      @@year        ##year            4
     c                   parm      *blanks       ##call            1
     C                   z-add     ##CNT         @@CNT
     C                   open      femeptn
     C                   exsr      endSubmitter
     c                   endif
     C                   enddo
     C                   endif
     c                   else
     C*
     C** Set status flag in medical import file to blank for records
     C** within date range from entry parms. Set all other records to 'Y'
     C*
     C                   exsr      setStatus
     c                   endif
     C*
     C                   close     femeptn
     C*                  eval      *inlr = *on
     C                   return
     C*
     C*=====================================================================
     C* SUBROUTINE setStatus
     C*=====================================================================
     C     setStatus     begsr
     C*
     C     1             setll     pxh02
     C                   read      pxh02
     C                   dow       not %eof
     C*** begin chg *** 11/25/08 *******************************************
     C**********         if        xhpodt >= fromdate and
     C**********                   xhpodt <= todate
     C                   if        xhpodt >= fromdate and
     C                             xhpodt <= todate and xhcode<>' '
     C*** end   chg *** 11/25/08 *******************************************
     C                   eval      xhstat = ' '
     C                   else
     C                   eval      xhstat = 'Y'
     C                   endif
     C                   update    pxh02
     C                   read      pxh02
     C                   enddo
     C*
     C                   endsr
     C*
     C*=====================================================================
     C* SUBROUTINE buildKey
     C*=====================================================================
     C     buildKey      begsr
     C*
     C* Key is 25A = 9 leading zeros,3,3,3,7
     C*
     C                   eval      claimky =   %trim(%editw(cmco#:'0   ')) +
     C                                         %trim(%editw(cmfnd:'0   ')) +
     C                                         %trim(%editw(cmfyr:'0   ')) +
     C                                         %trim(%editw(cmcase:'0       '))
     C*
     C                   endsr
     C*
     C*=====================================================================
     C* SUBROUTINE setDateRange
     C*=====================================================================
     C     setDateRange  begsr
     C*
     C                   select
     C                   when      @@qtr = '1'
     C                   eval      fromdate = @@year + '0101'
     C                   eval      todate = @@year + '0331'
     C                   when      @@qtr = '2'
     C                   eval      fromdate = @@year + '0401'
     C                   eval      todate = @@year + '0630'
     C                   when      @@qtr = '3'
     C                   eval      fromdate = @@year + '0701'
     C                   eval      todate = @@year + '0930'
     C                   when      @@qtr = '4'
     C                   eval      fromdate = @@year + '1001'
     C                   eval      todate = @@year + '1231'
     C                   other
     C                   eval      fromdate = *blanks
     C                   eval      todate = *blanks
     C                   endsl
     C*
     C                   endsr
     C*
     C*=====================================================================
     C* SUBROUTINE wrtPhysician
     C*=====================================================================
     C     wrtPhysician  begsr
     C*
     C                   add       1             @@cnt
     c*
     c                   if        xhcode < '00000'
     c                   eval      cptCode = *blanks
     c                   eval      hcpcCode = xhcode
     c                   else
     c                   eval      cptCode = xhcode
     c                   eval      hcpcCode = *blanks
     c                   endif
     C*
     c                   move      xhpaid        paid
     c                   move      xhfein        fein
     C                   eval      medata = '<OTHTHNHIO><RecNbrDtl3>' +
     C                             %trim(%editc(@@CNT:'4')) +
     C                             '</RecNbrDtl3><BillID>' + %trim(xhblid) +
     C                             '</BillID><CPTCode>' + %trim(cptCode) +
     C                             '</CPTCode><HCPCCode>' + %trim(hcpcCode) +
     C                             '</HCPCCode><DiagnosisCode>' +
     C                             %trim(xhdiag) + '</DiagnosisCode>' +
     C                             '<NPICode></NPICode><DateOfService>' +
     C                             xhfdat + '</DateOfService>' +
     C                             '<AmountPaid>' +
     c                             %trim(%editw(paid:'       0 .  ')) +
     C                             '</AmountPaid><FacilityZipCode>' + xhzip +
     C                             '</FacilityZipCode><TypeOfProvider>' +
     C                             xhspcd + '</TypeOfProvider>' +
     C                             '<FacProviderFein>' +
     C                             %trim(%editw(fein:'0         ')) +
     C                             '</FacProviderFein><FacProviderName>'+
     C                             %trim(xhpnam) + '</FacProviderName>' +
     C                             '<DateBilled>' + xhcrdt +
     C                             '</DateBilled><DatePaid>' + xhpodt +
     C                             '</DatePaid></OTHTHNHIO>'
     C*
     C                   write     pme01
     C                   endsr
     C*
     C*=====================================================================
     C* Create submission header section
     C*=====================================================================
     C     crtSubmitter  begsr
     C*
     C                   clear                   pme01
     C                   add       1             submitter
     C                   eval      submitted = *on
     C*
     C                   eval      text = c1name
     C                   eval      length = 60
     C                   exsr      stripBadChars
     C                   eval      c1name = text
     C
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtyymd1
     C                   movel     srdto         xxdate
     C
     C                   eval      claimID = %trim(%editw(kkco#:'0   ')) +
     C                                  %trim(%editw(kkfnd:'0   ')) +
     C                                  %trim(%editw(kkfyr:'0   ')) +
     C                                  %trim(%editw(kkcase:'0       '))
     C
     C     fnky          chain     pfn01
     C     fnky          chain     pf101
     C                   eval      text = fnname
     C                   eval      length = 60
     C                   exsr      stripBadChars
     C                   eval      fnname = text
     C                   movel     fnmzip        zipCode
     C
     C                   eval      medata = '<SUBMITTER><RecNbr>' +
     C                             %trim(%editc(submitter:'4')) +
     C                             '</RecNbr><SubmitterId>' + c110a3 +
     C                             '</SubmitterId><SubmitterName>' +
     C                             %trim(c1name) + '</SubmitterName>' +
     C                             '<SubmitterFein>' +
     C                             %trim(%editw(c1fein:'0         ')) +
     C                             '</SubmitterFein><DateOfSubmission>' +
     C                             xxdate + '</DateOfSubmission>' +
     C                             '<TimeOfSubmission>' + %editc(dstime:'4') +
     C                             '</TimeOfSubmission><InsurerClaimNbr>' +
     C                             claimID + '</InsurerClaimNbr>' +
     C                             '<DateOfInjury>' + xhfdat +
     C                             '</DateOfInjury><InsurerName>' +
     C                             %trim(fnname) +
     C                             '</InsurerName><InsurerFein>' +
     C                             %trim(%editw(f1afei:'0         ')) +
     C                             '</InsurerFein><InsurerZip>' +
     C                             zipCode + '</InsurerZip><ReportQuarter>' +
     C                             @@qtr + '</ReportQuarter>' +
     C                             '<ReportYear>' + @@year + '</ReportYear>'
     C                   write     pme01
     C*
     C                   endsr
     C*
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
     C*=====================================================================
     C* Get the submission number
     C*=====================================================================
     C     getSubmission#begsr
     C*
     C     kkco#         chain     pc101
     C*
     C                   endsr
     C*
     C*=====================================================================
     C* Parse key
     C*=====================================================================
     C     parseKey      begsr
     C*
     C                   eval      xxco# = %subst(xhclm#:1:3)
     C                   eval      xxfnd = %subst(xhclm#:4:3)
     C                   eval      xxfyr = %subst(xhclm#:7:3)
     C                   eval      xxcase = %subst(xhclm#:10:7)
     C*
     C                   move      xxco#         kkco#
     C                   move      xxfnd         kkfnd
     C                   move      xxfyr         kkfyr
     C                   move      xxcase        kkcase
     C                   move      xhclm#        svclm#
     C*
     C                   endsr
     C*
     C*=====================================================================
     C* Strip Bad Characters
     C*=====================================================================
     C     stripBadChars begsr

     C*
     C* Replace & with amp; We first need to replace with ?amp; then change
     C* the ? back to an &
     C*

     C                   eval      i = %scan('&':text:1)
     C                   dow       i <> 0
     C                   if        i < (length - 4)
     C                   eval      text = %replace('~amp;':text:i:1)
     C                   else
     C                   eval      text = %replace(' ':text:i:1)
     C                   endif
     C                   eval      i = %scan('&':text:1)
     C                   enddo

     C                   eval      i = %scan('~':text:1)
     C                   dow       i <> 0
     C                   eval      text = %replace('&':text:i:1)
     C                   eval      i = %scan('~':text:1)
     C                   enddo

     C*
     C* Replace " with blanks
     C*

     C                   eval      i = %scan('"':text:1)
     C                   dow       i <> 0
     C                   eval      text = %replace('':text:i:1)
     C                   eval      i = %scan('"':text:1)
     C                   enddo

     C*
     C* Replace . with blanks
     C*

     C                   eval      i = %scan('.':text:1)
     C                   dow       i <> 0
     C                   eval      text = %replace(' ':text:i:1)
     C                   eval      i = %scan('.':text:1)
     C                   enddo

     C*
     C* Replace / with blanks
     C*

     C                   eval      i = %scan('/':text:1)
     C                   dow       i <> 0
     C                   eval      text = %replace(' ':text:i:1)
     C                   eval      i = %scan('/':text:1)
     C                   enddo

     C*
     C* Replace , with blanks
     C*

     C                   eval      i = %scan(',':text:1)
     C                   dow       i <> 0
     C                   eval      text = %replace(' ':text:i:1)
     C                   eval      i = %scan(',':text:1)
     C                   enddo

     C*
     C* Replace ¢ with blanks
     C*

     C                   eval      i = %scan('¢':text:1)
     C                   dow       i <> 0
     C                   eval      text = %replace(' ':text:i:1)
     C                   eval      i = %scan('¢':text:1)
     C                   enddo

     C                   eval      text = %trim(text)
     C*
     C                   endsr
