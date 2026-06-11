     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE POPQTETRK - Write Quote tracking record.
     H*
     Fwmeml     if   e           k disk
     fwaqtp     o    e             disk

     *=========================================================================
     *  Definition Specs
     *=========================================================================
    ** Date & time breakdown
     d                 ds
     d dsdttm                        12  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        6  0 overlay(dsdttm:7)

     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8

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

     d sys_date        s               d   datfmt(*usa)
     d sys_time        s               T   timfmt(*hms:)
     dkkagt#           s                   like(QTAGT#)
     d psds          esds                  extname(szq1p)

     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    
     * Employer Master
     c     emky1         klist
     c                   KFld                    kkco#
     c                   KFld                    kkfnd
     c                   KFld                    kkemp#
     c                   KFld                    kkdiv

     c     dtcymd        PList
     c                   Parm                    srdti
     c                   Parm      *blanks       srffmt
     c                   Parm      '*CYMD'       srtfmt
     c                   Parm                    srdto
     c                   Parm                    srerr

     *=========================================================================
      * Main Line processing
     *=========================================================================
      * Read Employer Master for each record.

     c                   move      *zeros        kkco#             3 0
     c                   move      *zeros        kkfnd             3 0
     c                   move      *zeros        kkemp#            9 0
     c                   move      *zeros        kkdiv             5 0

     c                   read      pem01                                  60

     c                   dow       *in60 = *off

     c                   eval      kkco# = emco#
     c                   eval      kkfnd = emfnd
     c                   eval      kkemp#= ememp#
     c                   eval      kkdiv =  emdiv

     * Check that employer division is cancelled

     c                   exsr      ck_emp_cx
     c     *hms          move      q1time        sys_time
     c     *usa          move      *date         sys_date

     c                   if        emp_cx = 'Y'


     c** If cancelled employer write "Cancellation" (QTTYPE = 'C') type quote tracking record

     c                   exsr      wr_qte_cr

     c** If not cancelled then write a "Written" (QTTYPE = 'W') type quote tracking record

     c                   else

     c                   exsr      wr_qte_wr

     c                   endif                                                  emp_cx = 'Y'

     c                   read      pem01                                  60

     c                   enddo                                                  *in60 = *off

     c                   eval      *inlr = *on

     *=========================================================================
     * WR_QTE_CR - Write "Cancellation" (QTTYPE = 'C') type quote tracking record
     *=========================================================================
     c     wr_qte_cr     begsr

     c                   call      'WA002R'
     C                   parm                    emco#
     C                   parm                    emfnd
     C                   parm                    ememp#
     C                   parm                    emdiv
     C                   parm      *zeros        ##fyr             3 0
     C     kkagt#        parm      *zeros        ##agt#            5 0
     C                   parm      *blanks       ##anam           60

     c                   if        kkAgt# = *zeros
     c                   leavesr
     c                   endif

     c                   eval      qtco#  = emco#                               Company no.
     c                   eval      qtfnd  = emfnd                               Group no.
     c                   eval      qtagt# = kkagt#                              Agent no.
     c                   eval      qtchap = emchap                              Chapter.
     c                   eval      qtname = emname                              Account name.
     c                   eval      qttype ='C'                                  Activity type.
     c     *cymd         move      sys_Date      qtwkdt                         Week ending date
     c                   eval      qtefdt = emefd1                              Est. effective date.
     c                   eval      qtcarr = *blanks                             Carrier name.
     c                   eval      qtprem = *zeros                              Premium amt.
     c                   eval      qtcomm = *blanks                             Comment.
     c                   eval      qtstat = 'W'                                 Status.
     c                   eval      qtuser = q1user                              User.
     c                   eval      qtpgm  = q1pgm                               Program
     c                   move      sys_time      qttime                         Time
     c     *cymd         move      sys_Date      qtdt                           Date maintained
     c                   eval      qtemp# = ememp#                              Employer no.
     c                   eval      qtdiv  = emdiv                               Division
     c                   eval      qtiusr = qtuser                              Initial User.
     c                   eval      qtipgm = qtpgm                               Initial Program
     c                   move      sys_time      qtitm                          Initial time
     c                   eval      qtidt  = qtdt                                Initial Date
     c                   eval      qtmod  = *zeros                              Exp Modifier.
     c                   eval      qtarap = *zeros                              Arap.
     c                   eval      qthlr  = *zeros                              Historical Loss.
     c                   eval      qtplr  = *zeros                              Projected Loss ratio
     c                   eval      qtfin  = *blanks                             Final status.
     c     *cymd         move      sys_Date      qtnbdt                         New Business date
     c                   eval      qtrvdt = *zeros                              Revised date.
     c                   eval      qtbfpr = *zeros                              Prem B-4 Revision
     c     *cymd         move      sys_Date      qtordt                         Original subm date
     c                   eval      qtfxdt = *zeros                              Fax date
     c                   eval      qtcom2 = *blanks                             Comment 2
     c                   eval      qtauls = *blanks                             Auto Loss.

     c                   write     pqt01

     c                   endsr                                                  wr_qte_cr

     *=========================================================================
     * WR_QTE_WR - Write a "Written" (QTTYPE = 'W') type quote tracking record
     *=========================================================================
     c     wr_qte_wr     begsr

     c                   call      'WA002R'
     C                   parm                    emco#
     C                   parm                    emfnd
     C                   parm                    ememp#
     C                   parm                    emdiv
     C                   parm      *zeros        ##fyr             3 0
     C     kkagt#        parm      *zeros        ##agt#            5 0
     C                   parm      *blanks       ##anam           60

     c                   if        kkAgt# = *zeros
     c                   leavesr
     c                   endif

     c                   eval      qtco#  = emco#                               Company no.
     c                   eval      qtfnd  = emfnd                               Group no.
     c                   eval      qtagt# = kkagt#                              Agent no.
     c                   eval      qtchap = emchap                              Chapter.
     c                   eval      qtname = emname                              Account name.
     c                   eval      qttype ='W'                                  Activity type.
     c     *cymd         move      sys_Date      qtwkdt                         Week ending date
     c                   eval      qtefdt = emefd1                              Est. effective date.
     c                   eval      qtcarr = *blanks                             Carrier name.
     c                   eval      qtprem = *zeros                              Premium amt.
     c                   eval      qtcomm = *blanks                             Comment.
     c                   eval      qtstat = 'W'                                 Status.
     c                   eval      qtuser = q1user                              User.
     c                   eval      qtpgm  = q1pgm                               Program
     c                   move      sys_time      qttime                         Time
     c     *cymd         move      sys_Date      qtdt                           Date maintained
     c                   eval      qtemp# = ememp#                              Employer no.
     c                   eval      qtdiv  = emdiv                               Division
     c                   eval      qtiusr = qtuser                              Initial User.
     c                   eval      qtipgm = qtpgm                               Initial Program
     c                   move      sys_time      qtitm                          Initial time
     c                   eval      qtidt  = qtdt                                Initial Date
     c                   eval      qtmod  = *zeros                              Exp Modifier.
     c                   eval      qtarap = *zeros                              Arap.
     c                   eval      qthlr  = *zeros                              Historical Loss.
     c                   eval      qtplr  = *zeros                              Projected Loss ratio
     c                   eval      qtfin  = *blanks                             Final status.
     c     *cymd         move      sys_Date      qtnbdt                         New Business date
     c                   eval      qtrvdt = *zeros                              Revised date.
     c                   eval      qtbfpr = *zeros                              Prem B-4 Revision
     c     *cymd         move      sys_Date      qtordt                         Original subm date
     c                   eval      qtfxdt = *zeros                              Fax date
     c                   eval      qtcom2 = *blanks                             Comment 2
     c                   eval      qtauls = *blanks                             Auto Loss.

     c                   write     pqt01
     c                   endsr                                                  wr_qte_wr
     *=========================================================================
     *  CK_EMP_CX - Check if employer division is cancelled
     *=========================================================================
     c     ck_emp_cx     begsr

     C                   time                    dsdttm
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         sysdat            7 0
      *
     c                   move      'Y'           emp_cx            1
     c     emky1         chain     pem01

     c                   if        %found

      *  Get Employer Effective/Cancellation date

     C                   z-add     5             x                 3 0

     c                   dow       (ed(x) = 0) and (x > 1)
     c                   sub       1             x
     c                   enddo

     c                   if        (cd(x) = 0) or (cd(x) > sysdat)
     c                   eval      emp_cx = 'N'
     c                   endif

     c                   endif

     c                   endsr


     C*
