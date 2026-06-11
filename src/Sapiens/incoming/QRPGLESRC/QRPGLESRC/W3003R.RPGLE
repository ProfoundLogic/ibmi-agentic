     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    h*********************************************************************
    h/TITLE - Internal Account Rep Commission Workfile Build
     h option(*nodebugio:*srcstmt)
    h*********************************************************************
    h*                                                                   *
    h*  Reads through WDELP across groups and writes record to WXRCP for *
    h*  each billing record, excluding expense constant, associated with *
    h*  the given policy/year that has a posting date equal to or less   *
    h*  than the month end date from the prompt.                         *
    h*                                                                   *
    h*  CALLED FROM:                                                     *
    h*    W3002C                                                         *
    h*                                                                   *
    h*  CALLS TO:                                                        *
    h*                                                                   *
    h*  CREATED: 08/28/06      C:180     PROJECT: 114WMF                 *
    h*********************************************************************
    h*  REVISIONS:                                                       *
    h*                                                                   *
    h* 01/16/15 C:136 PhaseWare: 32635                                   *
    h*                CSI requested chg for determing new/renew          *
    h*                status. If cross reference exists, it is           *
    h*                renewal.                                           *
    h*                                                                   *
    h* 10/28/09 C:191 Support: 69113 WA114                               *
    h*                Correct new/renew logic to use API                 *
    h*                                                                   *
    h* 10/23/07 C:136 Project: 114WPC                                    *
    h*                Exclude retro billlings, category type 'C'         *
    h*                from the workfile build                            *
    h*********************************************************************
    f*
     fWDF1L     if   e           k disk
     fWMFNL     if   e           k disk
     fWDELL1    if   e           k disk
     fWMEALC    if   e           k disk
     fWMAHL     if   e           k disk
     fWTABL     if   e           k disk    include(ptt01)
     fWMRPL     if   e           k disk
     fWMEML     if   e           k disk
     fWMAAL1    if   e           k disk
     fWMAPL     if   e           k disk
     fWXRCP     if a e           k disk
      *** beg add ***** 01/16/2015 *************************************
     fWZEXL     if   e           k disk    rename(pex01:pex0l)
      *** end add ***** 01/16/2015 *************************************
    f*
     f*** BEGIN DEL ****** 10/28/09 ************************************
     f***********WDEHL12   if   e           k disk
     f*** END   DEL ****** 10/28/09 ************************************
    f*
     d/copy sprdate
    d*
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d @@co#           s              3
     d @@from_date     s              8
     d @@to_date       s              8
     d @@mth_end_date  s              8
     d @@que           s             10
     d xxfrom_dt       s                   like(elpfdt)
     d xxto_dt         s                   like(elpfdt)
     d xxmth_end_dt    s                   like(elpfdt)
      *
     d xxTODAY         s                   like(elpfdt)
     d svfnd           s                   like(elfnd)
     d svfyr           s                   like(elfyr)
     d xxfyr           s                   like(elfyr)
      *
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    c*
    c*====================================================================
    c*  PLIST definitions
    c*====================================================================
     c     *entry        plist
     c                   parm                    @@co#             3
     c                   parm                    @@from_date       8
     c                   parm                    @@to_date         8
     c                   parm                    @@mth_end_date    8
     c                   parm                    @@que            10
    c*================================================================
    c*  KLIST definitions
    c*================================================================
     c     aaky          klist
     c                   kfld                    kkco#             3 0
     c                   kfld                    ahfnd
     c                   kfld                    ahsagt
      *
     c     apky          klist
     c                   kfld                    kkco#             3 0
     c                   kfld                    aafein
     c                   kfld                    aamod
     c                   kfld                    ahsprd
      *
     c     coky          klist
     c                   kfld                    kkco#
      *
     c     elky          klist
     c                   kfld                    kkco#
     c                   kfld                    f1fnd
     c                   kfld                    f1fyr
      *
     c     eaky          klist
     c                   kfld                    kkco#
     c                   kfld                    elfnd
     c                   kfld                    elfyr
     c                   kfld                    elemp#
     c                   kfld                    eldiv
      *
     c     fnky          klist
     c                   kfld                    kkco#
     c                   kfld                    f1fnd
      *
      *
     c     fnky1         klist
     c                   kfld                    kkco#
     c                   kfld                    svfnd
     c                   kfld                    svfyr
      *
     c*** BEGIN DEL ****** 10/28/09 ************************************
      *   KEY LIST FOR WDEHL12
     C*****EHKY          Klist
     C***********        Kfld                    ELco#
     C***********        Kfld                    ELfnd
     C***********        Kfld                    ELemp#
     C***********        Kfld                    ELdiv
     C***********        Kfld                    ELfyr
     c*** END   DEL ****** 10/28/09 ************************************
      *
     c     ahky          klist
     c                   kfld                    kkco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv
     c                   kfld                    elfyr
      *
     c     ahky1         klist
     c                   kfld                    kkco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv
     c                   kfld                    elfyr
     c                   kfld                    kkefdt            7 0
      *
     c     emky          klist
     c                   kfld                    kkco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv
      *
     c     ttky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkkey            10
    c*================================================================
    c*  Main
    c*================================================================
     c                   move      @@co#         kkco#
     c                   eval      xxfrom_dt = dtCYMD(numDate(@@from_date))
     c                   eval      xxto_dt   = dtCYMD(numDate(@@to_date))
     c                   eval      xxmth_end_dt=dtCYMD(numDate(@@mth_end_date))
      *
      *  Get today's date in 7 digit file format
     c                   time                    dsdttm
     c                   eval      xxtoday   = dtCYMD(dsdate)
      *
     c     coky          chain     pf101                              60
      *
     c                   dow       not *in60
      *
     c     fnky          chain     pfn01                              60
      *
     c                   if        FNTEST <> 'Y'
      *
      * Get the current policy year and then reset WDFIL file to continue
      *
     c                   if        fnfnd <> svfnd
     c                   eval      svfnd = fnfnd
     c                   eval      svfyr = f1fyr
      *
     c                   dow       not *in60 and not *in61
     c                   if        xxmth_end_dt >= f1stpo and
     c                             xxmth_end_dt <=  f1enpo
     c                   eval      xxfyr = f1fyr                                Current Policy Yr
     c                   leave
     c                   endif
     c     fnky          reade     pf101                                  61
     c                   enddo
      *
     c     fnky1         chain     pf101                              60
     c                   endif
      *
     c                   if        F1STPO >= xxfrom_dt and
     c                             F1STPO <= xxto_dt
      *
     c     elky          chain     pel01                              60
     c                   dow       not *in60
      *
     c                   if        ELPFDT >= xxfrom_dt and
     c                             ELPFDT <= xxto_dt
      *
     c                   eval      kkefdt = xxmth_end_dt
     c     ahky1         setgt     pah01
     c     ahky          readpe    pah01                                  60
      *
     c                   if        not *in60 and AHSAGT = 99999
      *
     c                   if        AHSPRD <> *zeros
      *
     c     eaky          chain     pea01                              60
     c                   dow       not *in60
      *
     c                   if        EAARTY = 'A' or
     c                             EAARTY = 'B' or
     c                             EAARTY = 'D'
      *
     c                   eval      kkkey = EAARCD
     c     ttky          chain     ptt01
     c                   if        %found and
     c                             TTCTYP <> 'E'
      *** beg add ***** 10/23/07 **********************************************
     c                             and TTCTYP <> 'C'
      *** end add ***** 10/23/07 **********************************************
      *
     c                   clear                   prc01
      *
     c*** BEGIN CHG ****** 10/28/09 ************************************
      * Change new/renew/expiring logic to use WA069R API
      ****
      ***Read Employer History file to determine status of work file fields
      *********
     c*****ehky          Setgt     Wdehl12
     c*****ehky          Readpe    Wdehl12                                62
     c***********        if        not *in62
     c***********        select
     c***********        when      ehnwrw = 'N' AND elfyr >= xxfyr
     c***********        eval      rcbstp = 'N'
     c***********        eval      rcbst2 = '1'
     c***********        when      ehnwrw = 'N' AND elfyr <  xxfyr
     c***********        eval      rcbstp = 'E'
     c***********        eval      rcbst2 = '2'
     c***********        when      ehnwrw = 'R'
     c***********        eval      rcbstp = 'R'
     c***********        eval      rcbst2 = '3'
     c***********        when      ehnwrw = ' ' AND elfyr >= xxfyr
     c***********        eval      rcbstp = 'N'
     c***********        eval      rcbst2 = '1'
     c***********        when      ehnwrw = ' ' AND elfyr <  xxfyr
     c***********        eval      rcbstp = 'E'
     c***********        eval      rcbst2 = '2'
     c***********        other
     c***********        eval      rcbstp = 'X'
     c***********        eval      rcbst2 = '9'
     c***********        endsl
      ***********
     c***********        else
      ***********
     c***********        select
     c***********        when      elfyr >= xxfyr
     c***********        eval      rcbstp = 'N'
     c***********        eval      rcbst2 = '1'
     c***********        when      elfyr <  xxfyr
     c***********        eval      rcbstp = 'E'
     c***********        eval      rcbst2 = '2'
     c***********        endsl
      ***********
     c***********        endif
     c*
     c* call API to determine new/renew status
     c                   call      'WA069R'
     c                   parm      elco#         ##co#             3 0
     c                   parm      elfnd         ##fnd             3 0
     c                   parm      elemp#        ##emp#            9 0
     c                   parm      elfyr         ##fyr             3 0
     c                   parm      *blanks       ##nwrw            1
      *** beg add ***** 01/16/15 **************************************
      /free
       Chain (ELco#:ELfnd:ELemp#:ELdiv) pex0l;
       If %found;
          ##nwrw = 'R';
       Endif;
      /end-free
      *** end add ***** 01/16/15 **************************************
     c                   select
     c                   when      ##nwrw = 'N' AND elfyr >= xxfyr
     c                   eval      rcbstp = 'N'
     c                   eval      rcbst2 = '1'
     c                   when      ##nwrw = 'N' AND elfyr <  xxfyr
     c                   eval      rcbstp = 'E'
     c                   eval      rcbst2 = '2'
     c                   when      ##nwrw = 'R'
     c                   eval      rcbstp = 'R'
     c                   eval      rcbst2 = '3'
     c                   when      ##nwrw = ' ' AND elfyr >= xxfyr
     c                   eval      rcbstp = 'N'
     c                   eval      rcbst2 = '1'
     c                   when      ##nwrw = ' ' AND elfyr <  xxfyr
     c                   eval      rcbstp = 'E'
     c                   eval      rcbst2 = '2'
     c                   other
     c                   eval      rcbstp = 'X'
     c                   eval      rcbst2 = '9'
     c                   endsl
      *
     c*** END   CHG ****** 10/28/09 ************************************
      *
     c                   eval      RCCO#  = ELCO#
     c                   eval      RCGRP  = ELFND
     c                   eval      RCEMP# = ELEMP#
     c                   eval      RCDIV  = ELDIV
     c                   eval      RCFYR  = ELFYR
     c                   eval      RCARCD = EAARCD
     c                   eval      RCAMT  = EAAMT
     c                   eval      RCPODT = EAPODT
     c                   eval      RCPFDT = ELPFDT
     c                   eval      RCPTDT = ELPTDT
      *
     c     aaky          chain     paa01
     c     apky          chain     pap01
     c                   eval      RCAREP = %trimr(APLSN) + ',' + ' ' + APFSN
      *
     c     ahky          chain     prp01
     c                   if        %found
     c                   eval      RCCAP# = RPCAP#
     c                   endif                                                  %found
      *
     c     emky          chain     pem01
     c                   if        %found
     c                   eval      RCENAM = EMNAME
      *
     c                   select
     c                   when      EMEFD5 <> *zero and EMCAD5 <> *zero
     c                             and EMEFD5 <= xxmth_end_dt
     c                   eval      RCCNDT = EMCAD5
     c                   when      EMEFD4 <> *zero and EMCAD4 <> *zero
     c                             and EMEFD4 <= xxmth_end_dt
     c                   eval      RCCNDT = EMCAD4
     c                   when      EMEFD3 <> *zero and EMCAD3 <> *zero
     c                             and EMEFD3 <= xxmth_end_dt
     c                   eval      RCCNDT = EMCAD3
     c                   when      EMEFD2 <> *zero and EMCAD2 <> *zero
     c                             and EMEFD2 <= xxmth_end_dt
     c                   eval      RCCNDT = EMCAD2
     c                   when      EMEFD1 <> *zero and EMCAD1 <> *zero
     c                             and EMEFD1 <= xxmth_end_dt
     c                   eval      RCCNDT = EMCAD1
     c                   endsl
      *
     c                   endif                                                  %found
      *
     c                   write     prc01
      *
     c                   endif                                                  %found
      *
     c                   endif                                                  EAARTY = A
      *
     c     eaky          reade     pea01                                  60
     c                   enddo                                                  chain pea01
      *
     c                   endif                                                  ##anam <> *blank
      *
     c                   endif                                                  not *in60 and SAGT
      *
     c                   endif                                                  ELPFDT >= xxfrom_dt
      *
     c     elky          reade     pel01                                  60
     c                   enddo                                                  chain pel01
      *
     c                   endif                                                  F1STPO >= xxfrom_dt
      *
     c                   endif                                                  FNTEST <> 'Y'
      *
     c     coky          reade     pf101                                  60
     c                   enddo                                                  chain pf101
      *
     c                   eval      *inlr = *on
