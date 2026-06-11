     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/TITLE POPCNLHST - Populate Cancellation History.
     H*
     Fwmeml     if   e           k disk
     Fwdell2    if   e           k disk
     Fwdxhp     o    e           k disk
     Fwdcxp     o    e           k disk    rename(pcx01:pcx02)
     Fwdcxl     if   e           k disk

     *=========================================================================
     *  Definition Specs
      *=============================================================================================

      * employer effective dates
     d edds            ds
     d  emefd1
     d  emefd2
     d  emefd3
     d  emefd4
     d  emefd5
    d ed                                  OVERLAY(edds)
     d                                     LIKE(emefd1) DIM(5)

      * employer cancel dates
     d cdds            ds
     d  emcad1
     d  emcad2
     d  emcad3
     d  emcad4
     d  emcad5
    d cd                                  OVERLAY(cdds)
     d                                     LIKE(emcad1) DIM(5)
     d psds          esds                  extname(szq1p)                       External prog info

     d xxcand          s                   like(emcad1)
     d sysDt7          s                   like(emcad1)
     d xxfyr           s                   like(elfyr)
     d xxreq#          s                   like(cxreq#)

     d/copy sPrDate
     d/copy wPrEmplr

     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    
     * Employer Master
     c     emky1         klist
     c                   KFld                    emco#
     c                   KFld                    emfnd
     c                   KFld                    ememp#
     c                   KFld                    emdiv

     * Employer Detail
     c     elky1         klist
     c                   KFld                    emco#
     c                   KFld                    emfnd
     c                   KFld                    ememp#
     c                   KFld                    emdiv
     c                   KFld                    xxcand

     c     elky          klist
     c                   KFld                    emco#
     c                   KFld                    emfnd
     c                   KFld                    ememp#
     c                   KFld                    emdiv

     C*  Cancellation Detail file
     c     cxky          klist
     c                   KFld                    elco#
     c                   KFld                    elfnd
     c                   KFld                    elfyr
     c                   KFld                    elemp#
     c                   KFld                    eldiv

     c     cxky1         klist
     c                   KFld                    elco#
     c                   KFld                    elfnd
     c                   KFld                    elfyr
     c                   KFld                    elemp#
     c                   KFld                    eldiv
     c                   KFld                    xxreq#

     *=========================================================================
      * Main Line processing
     *=========================================================================
     c                   eval      sysDt7 = DateTo7(SystemDate)

      * Read Employer Master for each record.

     c                   read      pem01                                  60

     c                   dow       *in60 = *off

     c** Reterive last cancellation date
     c                   eval      xxcand = GetCxDate(EMCO#:EMFND:EMEMP#:
     c                                                EMDIV)

     * Check that employer division is cancelled
     c                   if        xxCanD <> *zero

     * PO_CAN_HS - Populate cancellation History  & cancellation detail

     c                   call      'WA002R'
     C                   parm                    elco#
     C                   parm                    elfnd
     C                   parm                    elemp#
     C                   parm                    eldiv
     C                   parm      *zeros        ##fyr             3 0
     C                   parm      *zeros        #1agt#            5 0
     C                   parm      *blanks       #1anam           60

     c     elky1         setgt     pel01
     c     elky          readpe    pel01
      *
      *  If cancellation date is equal to the policy expiration date then
      *  retrieve effective date for next policy year.
      *  If cancellation date is equal to the new policy year effective date AND
      *  there is premium on new year then Write a cancellation history record
      *  for expiring year and new year.
      *
     c                   if        (xxcand = elptdt)
     c                   eval      xxfyr = elfyr
     c     elky          reade     pel01
     c                   if        (xxcand = elpfdt) AND (elampr > 0 or
     c                              eLmapr > 0)
     c                   exsr      wrtprvyr
     c                   exsr      wrtcuryr

     c                   else
     c                   exsr      wrtprvyr
     c                   endif                                                  AND elampr > 0 or

     c                   else
     c                   exsr      wrtcuryr
     c                   endif                                                  (xxcand = elpfdt)

     c                   endif                                                  emp_cx = 'Y'

     C                   read      pem01                                  60

     c                   enddo                                                  *in60 = *off

     c                   eval      *inlr = *on

     *=========================================================================
     *  WRTCURYR  - Write Current year cancellation record.
     *=========================================================================
     c     wrtcuryr      begsr

     C*  Write Cancellation Detail and History records

     c                   move      *hival        xxreq#

     c     cxky1         setgt     pcx01
     c     cxky          readpe    pcx01                                  60

     c                   if        *in60 = *off
     c     cxreq#        add       1             xxreq#
     c                   else
     c                   z-add     1             xxreq#
     c                   endif                                                  *in60 = *off

     C*  Load WDCXP fields

     c                   eval      cxco#  = elco#                               Company no.
     c                   eval      cxfnd  = elfnd                               Group no.
     c                   eval      cxfyr  = elfyr                               Policy year
     c                   eval      cxemp# = elemp#                              Emp number
     c                   eval      cxdiv  = eldiv                               Division
     c                   eval      cxreq# = xxreq#                              Sequence no.
     c                   eval      cxcxdt = xxcand                              Cancel date
     c                   eval      cxcdat = xxcand                              Date cancell issue.
     c                   eval      cxagt# = #1agt#                              Agent no.
     c                   eval      cxrsn  = *blanks                             Cancel reason.
     c                   eval      cxcom1 = *blanks                             Comment 1.
     c                   eval      cxcom2 = *blanks                             Comment 2.
     c                   eval      cxccrt = 'N'                                 Cancel cert
     c                   eval      cxcdrt = *blanks                             Cancel dart code
     c                   eval      cxrein = 'N'                                 Reinstated flag
     c                   eval      cxredt = *ZEROS                              Reinstatement date
     c                   eval      cxrdat = *ZEROS                              Date reinstatement
     c                   eval      cxrcrt = 'N'                                 Reinstatement cert
     c                   eval      cxrdrt = *blanks                             Reinstatement dart
     c                   eval      cxstat = *blanks                             Status
     c                   eval      cxuser = q1user                              User
     c                   eval      cxpgm  = q1pgm                               Program
     c                   eval      cxtime = TimeTo6(SystemTime)                 Time
     c                   eval      cxdt   = sysDt7                              Date Maintained
     c                   eval      cxrevl = *blanks                             NCCI Reinstate value
     c                   eval      cxiusr = q1user                              Initial user
     c                   eval      cxipgm = q1pgm                               Initial pgm
     c                   eval      cxitm  = CXTIME                              Initial time
     c                   eval      cxidt  = cxdt                                Initial date
     c                   eval      cxcffl = 'Y'                                 Confirm on CX flag
     c                   eval      cxcfdt = xxcand                              Cancel.confirm date
     c                   eval      cxcemn = *blanks                             Certified mail#
     c                   eval      cxdoi# = *zeros                              Dept of Ins#
     c                   eval      cxcnc# = *zeros                              Cancellation count
     c                   eval      cxerr  = *blanks                             Cancellation in erro
     c                   eval      cxamtd = *zeros                              Amount due
     c                   eval      cxrfee = *zeros                              Reinst. fee
     c                   eval      cxrfds = *blanks                             Reinst. fee desc
     c                   eval      cxcap# = *blanks                             Carrier policy#
     c                   eval      cxrpmy = *zeros                              Reporting month/yr
     c                   eval      cxcfpd = sysDt7                              Confirm.process date
     c                   eval      cxcarr = *blanks                             Carrier name
     c                   eval      cxenfl = 'Y'                                 Endors flag
     c                   eval      cxrcm1 = *blanks                             Reinstate comment1
     c                   eval      cxrcm2 = *blanks                             Reinstate comment 2
     c                   eval      cxrst  = *blanks                             Premium rating state
     c                   eval      cxcafl = *blanks                             Confirm Audit reques
     c                   eval      cxeafl = *blanks                             Current est.
     c                   eval      cxaflt = *blanks                             Auto flat


      *  Load WDXHP fields
      *
     c                   eval      xhco#  = elco#                               Company number
     c                   eval      xhfnd  = elfnd                               Group number
     c                   eval      xhfyr  = elfyr                               Policy year
     c                   eval      xhemp# = elemp#                              Emp number
     c                   eval      xhdiv  = eldiv                               Division
     c                   eval      xhreq# = xxreq#                              Sequence no.
     c                   eval      xhcxdt = xxcand                              Cancel date
     c                   eval      xhcdat = cxcdat                              Date cancell issue.
     c                   eval      xhagt# = #1agt#                              Agent no.
     c                   eval      xhrsn  = *blanks                             Cancel reason.
     c                   eval      xhredt = *zeros                              Reinstatement date
     c                   eval      xhrdat = *zeros                              Date Reinst issued
     c                   eval      xhtrdt = sysDt7                              Curr system date.
     c                   eval      xhrvfl = *blanks                             Revised flag.
     c                   eval      xhstat = *blanks                             Status
     c                   eval      xhuser = q1user                              User
     c                   eval      xhpgm  = q1pgm                               Program
     c                   eval      xhtime = TimeTo6(SystemTime)                 Time
     c                   eval      xhdt   = sysDt7                              Date maintained
     c                   eval      xhcnc# = *zeros                              Cancellation count
     c                   eval      xherr  = *blanks                             Cancellation in err.
     c                   eval      xhenfl = 'Y'                                 Endorsment flag.

     c                   write     pcx02                                        Cancellation History
     c                   write     pxh01

     c                   endsr                                                  wrtcuryr

     *=========================================================================
     *  WRTPRVYR  - Write Current year cancellation record.
     *=========================================================================
     c     wrtprvyr      begsr

     C*  Write Cancellation Detail and History records

     c                   move      *hival        xxreq#

     c     cxky1         setgt     pcx01
     c     cxky          readpe    pcx01                                  60

     c                   if        *in60 = *off
     c     cxreq#        add       1             xxreq#
     c                   else
     c                   z-add     1             xxreq#
     c                   endif                                                  *in60 = *off

     C*  Load WDCXP fields

     c                   eval      cxco#  = elco#                               Company no.
     c                   eval      cxfnd  = elfnd                               Group no.
     c                   eval      cxfyr  = xxfyr                               Policy year
     c                   eval      cxemp# = elemp#                              Emp number
     c                   eval      cxdiv  = eldiv                               Division
     c                   eval      cxreq# = xxreq#                              Sequence no.
     c                   eval      cxcxdt = xxcand                              Cancel date
     c                   eval      cxcdat = xxcand                              Date cancell issue.
     c                   eval      cxagt# = #1agt#                              Agent no.
     c                   eval      cxrsn  = *blanks                             Cancel reason.
     c                   eval      cxcom1 = *blanks                             Comment 1.
     c                   eval      cxcom2 = *blanks                             Comment 2.
     c                   eval      cxccrt = 'N'                                 Cancel cert
     c                   eval      cxcdrt = *blanks                             Cancel dart code
     c                   eval      cxrein = 'N'                                 Reinstated flag
     c                   eval      cxredt = *ZEROS                              Reinstatement date
     c                   eval      cxrdat = *ZEROS                              Date reinstatement
     c                   eval      cxrcrt = 'N'                                 Reinstatement cert
     c                   eval      cxrdrt = *blanks                             Reinstatement dart
     c                   eval      cxstat = *blanks                             Status
     c                   eval      cxuser = q1user                              User
     c                   eval      cxpgm  = q1pgm                               Program
     c                   eval      cxtime = TimeTo6(SystemTime)                 Time
     c                   eval      cxdt   = sysDt7                              Date Maintained
     c                   eval      cxrevl = *blanks                             NCCI Reinstate value
     c                   eval      cxiusr = q1user                              Initial user
     c                   eval      cxipgm = q1pgm                               Initial pgm
     c                   eval      cxitm  = CXTIME                              Initial time
     c                   eval      cxidt  = cxdt                                Initial date
     c                   eval      cxcffl = 'Y'                                 Confirm on CX flag
     c                   eval      cxcfdt = xxcand                              Cancel.confirm date
     c                   eval      cxcemn = *blanks                             Certified mail#
     c                   eval      cxdoi# = *zeros                              Dept of Ins#
     c                   eval      cxcnc# = *zeros                              Cancellation count
     c                   eval      cxerr  = *blanks                             Cancellation in erro
     c                   eval      cxamtd = *zeros                              Amount due
     c                   eval      cxrfee = *zeros                              Reinst. fee
     c                   eval      cxrfds = *blanks                             Reinst. fee desc
     c                   eval      cxcap# = *blanks                             Carrier policy#
     c                   eval      cxrpmy = *zeros                              Reporting month/yr
     c                   eval      cxcfpd = sysDt7                              Confirm.process date
     c                   eval      cxcarr = *blanks                             Carrier name
     c                   eval      cxenfl = 'Y'                                 Endors flag
     c                   eval      cxrcm1 = *blanks                             Reinstate comment1
     c                   eval      cxrcm2 = *blanks                             Reinstate comment 2
     c                   eval      cxrst  = *blanks                             Premium rating state
     c                   eval      cxcafl = *blanks                             Confirm Audit reques
     c                   eval      cxeafl = *blanks                             Current est.
     c                   eval      cxaflt = *blanks                             Auto flat

      *  Load WDXHP fields
      *
     c                   eval      xhco#  = elco#                               Company number
     c                   eval      xhfnd  = elfnd                               Group number
     c                   eval      xhfyr  = xxfyr                               Policy year
     c                   eval      xhemp# = elemp#                              Emp number
     c                   eval      xhdiv  = eldiv                               Division
     c                   eval      xhreq# = xxreq#                              Sequence no.
     c                   eval      xhcxdt = xxcand                              Cancel date
     c                   eval      xhcdat = cxcdat                              Date cancell issue.
     c                   eval      xhagt# = #1agt#                              Agent no.
     c                   eval      xhrsn  = *blanks                             Cancel reason.
     c                   eval      xhredt = *zeros                              Reinstatement date
     c                   eval      xhrdat = *zeros                              Date Reinst issued
     c                   eval      xhtrdt = sysDt7                              Curr system date.
     c                   eval      xhrvfl = *blanks                             Revised flag.
     c                   eval      xhstat = *blanks                             Status
     c                   eval      xhuser = q1user                              User
     c                   eval      xhpgm  = q1pgm                               Program
     c                   eval      xhtime = TimeTo6(SystemTime)                 Time
     c                   eval      xhdt   = sysDt7                              Date maintained
     c                   eval      xhcnc# = *zeros                              Cancellation count
     c                   eval      xherr  = *blanks                             Cancellation in err.
     c                   eval      xhenfl = 'Y'                                 Endorsment flag.

     c                   write     pcx02                                        Cancellation History
     c                   write     pxh01                                        Cancellation Detail

     c                   endsr                                                  wrtprvyr
     C*
     *=========================================================================
     *  CK_EMP_CX - Check if employer division is cancelled
     *=========================================================================
     c     ck_emp_cx     begsr

      *
     c                   move      'Y'           emp_cx            1
     c     emky1         chain     pem01

     c                   if        %found

      *  Get Employer Effective/Cancellation date

     C                   z-add     5             x                 3 0

     c                   dow       (ed(x) = 0) and (x > 1)
     c                   sub       1             x
     c                   enddo

     c                   if        (cd(x) = 0) or (cd(x) > sysDt7)
     c                   eval      emp_cx = 'N'
     c                   endif

     c                   endif

     c                   endsr


     C*
