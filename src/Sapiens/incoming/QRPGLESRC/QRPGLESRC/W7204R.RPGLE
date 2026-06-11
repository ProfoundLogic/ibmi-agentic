     h/TITLE W7204R - commercial payroll by class workfile build
     H dftactgrp(*no) actgrp(*caller)
     h* ============================================================= *
     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     h*                                                               *
     h*  10/29/02  C:132  Project: WC1093                             *
     h*                   Commercial Payroll by Class build workfile  *
     h*                                                               *
     h*****************************************************************
     h*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/22/13  C:287  Seq#: SY266_OVR                             *
     H*                   Added DFTACTGRP and ACTGRP compile options  *
     h*                                                               *
     h*  09/14/09  C:191  Support: 69863                              *
     h*                   State logic was wrong, not breaking correct *
     h*                                                               *
     h*  04/20/09  C:190  Project: WC1473                             *
     h*                   Use new logical to get class extension      *
     h*                                                               *
     h*  12/29/03  C:132  Project: WA114  Seq: 40454                  *
     h*                   Missing Payroll records for Group - ADD new *
     h*                   ELKY2 for READE loop for WDELL3 file        *
     h*                                                               *
     h* ============================================================= *
     fwmfnl     if   e           k disk
     fsmcol     if   e           k disk
     fwdell3    if   e           k disk
     f*****BEG CHG*****04/20/09*********************************************
     f**********wmepl1    if   e           k disk
     fwmepl1e   if   e           k disk
     f*****END CHG*****04/20/09*********************************************
     fwmrsl     if   e           k disk
     fwmprl1    if   e           k disk
     fwfcpl     uf a e           k disk
      ***** BEG ADD  **** 04/20/09 ************************************
     FWMPXL1    IF   E           K DISK
      ***** END ADD  **** 04/20/09 ************************************
     d*
     d qqcall          s              1
     d qqco#           s              3
     d qqpfdt          s              8
     d qqptdt          s              8
     d qqtype          s              1
     d qqrsfl          s              1
     d qqque           s             10
     d qqfnd           s              3
     d qqfyr           s              3
     d srdlen          s              3  0
     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
     d svemp#          s                   like(elemp#) inz
     d svfyr           s                   like(elfyr)  inz
     d svdiv           s                   like(eldiv)  inz
     d svefdt          s                   like(epefdt) inz
     d svclas          s                   like(epclas) inz
     d svspec          s                   like(epspec) inz
     d*****BEG ADD*****04/20/09*********************************************
     d svext           s                   like(epext) inz
     d*****END ADD*****04/20/09*********************************************
     d svrtcd          s                   like(elrtcd) inz
     d svst            s                   like(rsjrst) inz
     d*** BEGIN ADD ****** 09/14/09 ************************************
     d xxst            s                   like(rsjrst) inz
     d*** END   ADD ****** 09/14/09 ************************************
     d xxclas          s                   like(epclas) inz
     d xxco#           s              3  0 inz
     d xxrcnt          s              5  0 inz
     d xxfnd           s              3  0 inz
     d xxfyr           s              3  0 inz
     d xxpfdt          s              7  0 inz
     d xxptdt          s              7  0 inz
     d xxtype          s              1
     d xxrsfl          s              1
     d xxconm          s                   like(coname)
     d xxfnnm          s                   like(fnname)
     d xxdesc          s                   like(cpdesc)
     d xxeapr          s                   like(cpeapr)
     d xxsapr          s                   like(cpsapr)
     d xxaapr          s                   like(cpaapr)
     d xteapr          s                   like(cpeapr)
     d xtsapr          s                   like(cpsapr)
     d xtaapr          s                   like(cpaapr)
     d*
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0
     d psds          esds                  extname(szq1p)
     c*
     c** =================== **
     c*  Mainline Processing
     c** =================== **
     c*
     c*  Read the WDELL3 file for the date range
     c     elky          setll     pel01
      *
      ***** BEG CHG ***** 12/19/03 *****************************************
     c*****elky          reade     pel01                                  60
     c     elky2         reade     pel01                                  60
      ***** END CHG ***** 12/19/03 *****************************************
     c                   dow       Not *in60
     c*
     c*  If the Employer Detail is in the date range, process, else end program
     c*
     c                   if        elpfdt >  xxptdt
     c                   leave
     c                   endif
     c*
     c*  Read the Payroll file for this employer, div
     c     epky          setll     pep01
     c     epky          reade     pep01                                  61
     c                   dow       Not *in61
     c*
     c*** BEGIN ADD ****** 09/14/09 ************************************
     c*  If Want output by State, use routing code to get state
     c                   eval      xxst = *blanks
     c                   if        xxrsfl = 'Y'
     c     rsky          chain     prs01                              64
     c                   if        not *in64
     c                   eval      xxst = rsjrst
     c                   endif
     c                   endif                                                  end xxrsfl = Y
     c*** END   ADD ****** 09/14/09 ************************************
     c*  When the fields change, write or update the output file & reset the save fields
     c*
     c*****BEG CHG*****04/20/09*********************************************
     c************       if        epclas <> svclas OR epspec <> svspec
     c                   if        epclas <> svclas OR epspec <> svspec OR
     c                             epext  <> svext
     c*** BEGIN ADD ****** 09/14/09 ************************************
     c                             OR (xxst <> svst AND xxrsfl = 'Y')
     c*** END   ADD ****** 09/14/09 ************************************
     c*****END CHG*****04/20/09*********************************************
     c*
     c                   if        svclas > *zeros                              Not first time ?
     c     cpky          chain     pcp01                              62
     c                   if        *in62
     c                   exsr      newrcd
     c                   else
     c                   eval      cpeapr = (cpeapr + xxeapr)
     c                   eval      cpsapr = (cpsapr + xxsapr)
     c                   eval      cpaapr = (cpaapr + xxaapr)
     c                   eval      xxrcnt = (xxrcnt + 1 )
     c                   update    pcp01
      *
     c                   endif                                                  end *IN61
     c                   endif                                                  end svemp# = *zeros
     c*
     c*  Now reset fields
     c                   eval      svemp# = epemp#
     c                   eval      svdiv  = epdiv
     c                   eval      svfyr  = epfyr
     c                   eval      svclas = epclas
     c                   eval      svspec = epspec
     c*****BEG ADD*****04/20/09*********************************************
     c                   eval      svext  = epext
     c*****END ADD*****04/20/09*********************************************
     c                   eval      svefdt = epefdt
     c                   eval      svrtcd = elrtcd
     c*** BEGIN ADD ****** 09/14/09 ************************************
     c                   eval      svst = xxst
     c*** END   ADD ****** 09/14/09 ************************************
     c                   eval      xxeapr = *zeros
     c                   eval      xxsapr = *zeros
     c                   eval      xxaapr = *zeros
     c*
     c*  Get the Class Code Description from WMRSP
     c*
     c                   move      *blanks       xxdesc
      ***** BEG ADD  **** 04/20/09 ************************************
     c     rsky          chain     prs01                              64
     C     epext         ifne      ' '
     C     pxky          setgt     ppx01
     C     pxky1         readpe    ppx01                                  61
     C     *in61         ifeq      *off
     C                   movel(p)  pxdesc        xxdesc
     C                   endif                                                  *in61=*off
     C                   else
      ***** END ADD  **** 04/20/09 ************************************
     c     prky          setgt     ppr01
     c     prky1         readpe    ppr01                                  64
     c  n64              movel(p)  prdesc        xxdesc
      ***** BEG ADD  **** 04/20/09 ************************************
     C                   ENDIF                                                  epext<>' '
      ***** END ADD  **** 04/20/09 ************************************
     c*
     c*** BEGIN DEL ****** 09/14/09 ************************************
     c* This logic is moved up
     c***********  If Want output by State, use routing code to get state
     c************       if        xxrsfl = 'Y'
     c*****rsky          chain     prs01                              64
     c************       if        *in64
     c*************      eval      svst = *blanks
     c*************      else
     c*************      eval      svst = rsjrst
     c*************      endif
     c*************      endif                                                  end xxrsfl = Y
     c*** END   DEL ****** 09/14/09 ************************************
     c*
     c                   endif                                                  end elemp# <> svemp#
     c*
     c*  If WMEPP/EPSTAT not inactive ('I') add to payroll totals
     c                   if        epstat <> 'I' AND epefdt >= xxpfdt AND
     c                             epefdt <= xxptdt
     c                   eval      xxeapr = (xxeapr + epeapr)
     c                   eval      xxsapr = (xxsapr + epsapr)
     c                   eval      xxaapr = (xxaapr + epaapr)
     c*
     c                   if        svclas = xxclas
     c                   eval      xteapr = (xteapr + epeapr)
     c                   eval      xtsapr = (xtsapr + epsapr)
     c                   eval      xtaapr = (xtaapr + epaapr)
     c                   endif
     c*
     c                   endif                                                  end epstat <> I
     c*
     c     epky          reade     pep01                                  61
     c                   enddo                                                  *IN61=*off WMEPL
      *
      ***** BEG CHG ***** 12/19/03 *****************************************
     c***********        read      pel01                                  60
     c     elky2         reade     pel01                                  60
      ***** END CHG ***** 12/19/03 *****************************************
      *
     c                   enddo                                                  *IN60=*off WMDELL3
      *
      ** ================= **
      *  Terminate Program
      ** ================= **
     c*
     c* If have amount in work fields, write the last record
     c*
     c                   if        xxeapr <> *zeros OR xxsapr <> *zeros OR
     c                             xxaapr <> *zeros
     c     cpky          chain     pcp01                              62
     c                   if        *in62
     c                   exsr      newrcd
     c                   else
     c                   eval      cpeapr = (cpeapr + xxeapr)
     c                   eval      cpsapr = (cpsapr + xxsapr)
     c                   eval      cpaapr = (cpaapr + xxaapr)
     c                   update    pcp01
      *
     c                   endif
     c                   endif
     c*
     c                   move      *on           *INLR
      *
      ** ==================================================================== **
      ** NEWRCD - Move fields to output file, write record if have amounts
      ** ==================================================================== **
      *
     c     newrcd        begsr
     c*
     c                   clear                   pcp01
     c*
     c* If have amounts, write create and write new WFCPP record
     c*
     c                   if        xxeapr <> *zeros OR xxsapr <> *zeros OR
     c                             xxeapr <> *zeros
     c*
     c                   eval      cpco#  = xxco#
     c                   eval      cpconm = xxconm
     c                   eval      cpfnd  = xxfnd
     c                   eval      cpfnam = xxfnnm
     c                   eval      cpst   = svst
     c                   eval      cpclas = svclas
     c                   eval      cpspec = svspec
     c*****BEG ADD*****04/20/09*********************************************
     c                   eval      cpext  = svext
     c*****END ADD*****04/20/09*********************************************
     c                   eval      cpdesc = xxdesc
     c                   eval      cpeapr = xxeapr
     c                   eval      cpsapr = xxsapr
     c                   eval      cpaapr = xxaapr
     c                   eval      cprndt = dsdate
     c*
     c                   write     pcp01
      *
     c                   eval      xxrcnt = *zeros
     c                   endif                                                  End Amts <> zeros
     c*
     c                   endsr
      *
      ** ==================================================================== **
      ** *INZSR - Initalize subroutine
      ** ==================================================================== **
      *
     c     *inzsr        begsr
      *
      ** =============== **
      *  Parameter Lists  *
      ** =============== **
      *
     c     *entry        plist
     c                   parm                    qqco#                          Company Number
     c                   parm                    qqfnd                          Group Number
     c                   parm                    qqpfdt                         Policy from date
     c                   parm                    qqptdt                         Policy to date
     c                   parm                    qqtype                         Type of output P,F,B
     c                   parm                    qqrsfl                         Sort by rating state
     c                   parm                    qqque                          output queue
     c*
     c     datchk        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      *blanks       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
     c*
     c     dtcymd        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
     c*
     c     dtmdyy        plist
     c                   parm                    srdti
     c                   parm      '*CYMD'       srffmt
     c                   parm      *blanks       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
      *
      ** =============== **
      *  Key Field Lists  *
      ** =============== **
     c*
     c*  Company Master File
     c*
     c     coky          klist
     c                   kfld                    xxco#
     c*
     c*  Group Master File
     c*
     c     fnky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c*
     c*  Employer Detail File
     c*
     c     elky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxpfdt
      *
      ***** BEG ADD ***** 12/19/03 *****************************************
     c*
     c     elky2         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
      ***** END ADD ***** 12/19/03 *****************************************
     c*
     c*  Employer Payroll File
     c*
     c     epky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    elfyr
     c                   kfld                    elemp#
     c                   kfld                    eldiv
     c*
     c*  Rate Set Code Master File
     c*
     c     rsky          klist
     c                   kfld                    xxco#
     c                   kfld                    elrtcd
     c*
     c*  Payroll Classification File
     c*
     c     prky          klist
     c                   kfld                    xxco#
     c                   kfld                    elrtcd
     c                   kfld                    epclas
     c                   kfld                    epefdt
     c*
     c     prky1         klist
     c                   kfld                    xxco#
     c                   kfld                    elrtcd
     c                   kfld                    epclas
     c*
      ***** BEG ADD  **** 04/20/09 ************************************
     C*  PAYROLL DESCRIPTION EXT FILE
     C*
     C     PXKY          KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    epclas
     C                   KFLD                    epext
     C                   KFLD                    rsjrst
     C                   KFLD                    epefdt
     C*
     C     PXKY1         KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    epclas
     C                   KFLD                    epext
     C                   KFLD                    rsjrst
     C*
      ***** END ADD  **** 04/20/09 ************************************
     c*  Payroll by Class Code output file
     c*
     c     cpky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    svst
     c                   kfld                    svclas
     c                   kfld                    svspec
     c*****BEG ADD*****04/20/09*********************************************
     c                   kfld                    svext
     c*****END ADD*****04/20/09*********************************************
     c*
      ** ========================== **
     c*  Other First Time Processes
      ** ========================== **
     c*
     c*  Convert Parm Dates from 8 (MMDDCCYY) to 7 (cyymmdd) positions
     c*
     c                   movel(p)  qqpfdt        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxpfdt
     c*
     c                   movel(p)  qqptdt        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxptdt
     c*
     c                   move      qqco#         xxco#
     c                   move      qqfnd         xxfnd
     c                   eval      xxtype = qqtype
     c                   eval      xxrsfl = qqrsfl
     c*
     c*  Setup run date for work file output field
     c                   time                    dsdttm
     c                   eval      xxclas = 5
     c*
     c*  Get Company and Group Names
     c*
     c                   movel     *blanks       xxconm
     c                   movel     *blanks       xxfnnm
     c*
     c     coky          chain     pco01                              60
     c                   if        not *in60
     c                   movel(p)  coname        xxconm
     c*
     c     fnky          chain     pfn01                              60
     c                   movel(p)  fnname        xxfnnm
     c                   if        fnanam <> *blanks
     c                   movel(p)  fnanam        xxconm
     c                   endif
     c*
     c                   endif
     c*
     c                   endsr
     c** ==================================================================== **
