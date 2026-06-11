     h/TITLE W7203R - commercial payroll by class workfile build
     h* ============================================================= *
     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*caller)
     h*****************************************************************
     h*  REVISIONS:                                                   *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     h* ============================================================= *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/22/13  C:287 Seq#: SY266_OVR                               *
     H*                 Added DFTACTGRP and ACTGRP compile options    *
     h*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     h*  10/29/02  C:132  Project: WC1093                             *
     h*                   Commercial Payroll by Class print Report    *
     h*                                                               *
     h* ============================================================= *
     fsmcol     if   e           k disk
     fsmorl     if   e           k disk
     fwmfnl     if   e           k disk
     fwfcpl     if   e           k disk
     fwmupl     if   e           k disk
     fststl1    if   e           k disk
     fqsysprt   o    f  132        printer oflind(*inob)
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
     d stflag          s              1
     d xxco#           s              3  0 inz
     d xxfnd           s              3  0 inz
     d svst            s                   like(cpst)   inz
     d xxscnt          s              5  0 inz
     d xxtcnt          s              5  0 inz
     d xxpfdt          s              8  0 inz
     d xxptdt          s              8  0 inz
     d xteapr          s                   like(cpeapr)
     d xtsapr          s                   like(cpsapr)
     d xtaapr          s                   like(cpaapr)
     d xxeapr          s                   like(cpeapr)
     d xxsapr          s                   like(cpsapr)
     d xxaapr          s                   like(cpaapr)
     d xxprt           s                   like(orfil#)
     d xxkey           s                   like(stkey)
     d*
     d xxcnam          S             40    inz
     d xxtitl          S             40    inz
     d xxstnm          S             20    inz
     d xstate          S             40    inz
     d xstcnt          S              6    inz
     d xxdata          S             60    inz
     d ##data          S             80    inz
     d ##leng          S              3  0
     d N               S              3  0
     d*
     d TITLE           C                   CONST('Commercial Payroll b-
     d                                     y Class Report')
     d nostat          C                   CONST('NO MATCH-STATE CODE')
     d*
     d*
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0
     d psds          esds                  extname(szq1p)
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
     c*
     c** =================== **
     c*  Mainline Processing
     c** =================== **
     c*
     c*  Read the WFCPP workfile of class codes built by W7204R
     c*
     c                   read      pcp01                                  60
     c*
     c*  If no records in workfile, print termination message and end program
     c                   if        *in60
     c                   movel(p)  xxcnam        cpfnam
     c                   except    head
     c                   goto      TERM
     c                   endif
     c*
     c                   eval      svst   = cpst
     c* If have State Flag on, get State name for heading
     c                   if        stflag =  'Y'
     c                   eval      *in69  = *on
     c                   movel(P)  svst          xxkey
     c     stky          chain     pst01                              62
     c                   if        (not *in62)
     c                   movel(p)  stname        xxstnm
     c                   else
     c                   movel(p)  nostat        xxstnm
     c                   endif
     c                   endif
     c                   except    head
     c*
     c*  Process all the WFCPP workfile records
     c*
     c                   dow       Not *in60
     c*
     c*  ==================== **
     c*  If have state flag parm = 'Y', accumulate totals by state
     c*  ==================== **
     c*
     c                   if        stflag =  'Y'
     c*
     c                   if        svst   <> cpst
     c*
     c* Create State total information text line with count of records
     c*
     c                   eval      xstcnt = %editc(xxscnt:'3')
     c                   eval      xstate = %trimR(xxstnm) + ' ' +
     c                             'State totals: ' + xstcnt
     c*
     c*  Get the state description from file
     c*
     c                   movel(P)  cpst          xxkey
     c     stky          chain     pst01                              62
     c                   if        (not *in62)
     c                   movel(p)  stname        xxstnm
     c                   else
     c                   movel(p)  nostat        xxstnm
     c                   endif
     c*
     c                   except    sttot
     c*
     c* If print state totals, then print headings for new page
     c                   except    head
     c                   eval      xxeapr = *zeros
     c                   eval      xxsapr = *zeros
     c                   eval      xxaapr = *zeros
     c                   eval      xxscnt = *zeros
     c                   eval      svst   = cpst
     c*
     c                   endif                                                  end svst <> cpst
     c*
     c                   eval      xxeapr = (xxeapr + cpeapr)
     c                   eval      xxsapr = (xxsapr + cpsapr)
     c                   eval      xxaapr = (xxaapr + cpaapr)
     c                   eval      xxscnt = xxscnt + 1
     c                   endif                                                  end stflag = 'Y'
     c
     c                   eval      xteapr = (xteapr + cpeapr)
     c                   eval      xtsapr = (xtsapr + cpsapr)
     c                   eval      xtaapr = (xtaapr + cpaapr)
     c                   eval      xxtcnt = (xxtcnt + 1 )
      *
      *  Now print next class detail line
      *
     c   ob              except    head
     c                   except    detl
     c*
     c*  Read next workfile record
     c*
     c                   read      pcp01                                  60
     c                   enddo                                                  *IN60=*off WMDELL3
      *
      ** ================= **
      *  Terminate Program
      ** ================= **
     c     TERM          tag
     c*
     c* If have amount in work fields, write the last records
     c*
     c                   if        stflag = 'Y' AND xxtcnt > *zeros
     c*
     c*  Get the state description from file
     c*
     c                   movel(P)  svst          xxkey
     c     stky          chain     pst01                              62
     c                   if        (not *in62)
     c                   movel(p)  stname        xxstnm
     c                   else
     c                   movel(p)  nostat        xxstnm
     c                   endif
     c*
     c* Create State total information text line with count of records
     c*
     c                   eval      xstcnt = %editc(xxscnt:'3')
     c                   eval      xstate = %trimR(xxstnm) + ' ' +
     c                             'State totals: ' + xstcnt
     c*
     c* Print Final totals
     c*
     c   ob              except    head
     c                   except    sttot
     c                   endif
     c*
     c                   if        xxtcnt > *zeros
     c                   except    LRTOT
     c                   else
     c                   except    LRERR
     c                   endif
      /Free
       // ***** Beg Add ***** 08/26/14 ****************************************
       xxPrt = '0';
       Chain orKy POR01;
       If %found;
          Except Route;
       Endif;
       // ***** End Add ***** 08/26/14 ****************************************
      /End-free
      *
      * Exit Program
     c                   move      *on           *INLR
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
      *
      *  Override File
     c     orky          klist
     c                   kfld                    q1pgm
     c                   kfld                    xxprt
      *
      *  State Code table file
     c     stky          klist
     c                   kfld                    xxkey
     c*
      ** ========================== **
     c*  Other First Time Processes
      ** ========================== **
     c*
     c*  Setup run date for work file output field
     c                   time                    dsdttm
     c*
     c                   if        qqrsfl = 'Y'
     c                   move      'Y'           stflag
     c                   endif
     c*
     c                   move      qqco#         xxco#
     c                   move      qqfnd         xxfnd
     c                   move      qqpfdt        xxpfdt
     c                   move      qqptdt        xxptdt
     c*
      *   Get the Disribution Information & Print it
     c                   move      '0'           xxprt
      *
     c     orky          chain     por01                              61
     c                   if        (ortitl <> *blanks)
     c                   move(p)   ortitl        xxdata                         FIELD, LENGTH
     c     ' '           check     xxdata        n
      *
     C                   if        qqque <> *blanks
     c     q1user        chain     pup01                              60
     c                   movel(P)  upusnm        ordis1
     c                   movel(P)  qqque         oroutq
     c                   endif
      *
     c                   if        (n <= 60)                                    LENGTH
     c                   eval      ortitl = %SUBST(XXDATA:N)                    FIELD
     c                   endif                                                  N<=60
      *
     c                   else
      *
     c                   movel(p)  title         ortitl
     c                   endif                                                  ORTITL<>*BLANKS
      *
     c*** Beg Add ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     c*** End add ***** 01/28/10 **************************************
     c*
      ***** Beg Del ***** 08/26/14 ************************************
     c*******            if        (not *in61)
     c*******            except    route
     c*******            endif                                                  *IN61=*OFF
      ***** End Del ***** 08/26/14 ************************************
      *
      *  Center Text in Field
     c                   movel(p)  ortitl        ##data
     c                   call      'SRCENTER'                           99
     c                   parm                    ##data
     c                   parm      60            ##leng
     c                   movel     ##data        ortitl
      *
      *  Center Company name in heading field
     c     coky          chain     pco01                              60
     c*
     c     fnky          chain     pfn01                              60
     c*  Save name if to print if do not have any records to process
     c                   movel(p)  fnname        xxcnam
     c*
     c                   if        fnanam <> *blanks
     c                   movel(p)  fnanam        coname
     c                   endif
     c*
     c                   movel(p)  coname        ##data
     c                   call      'SRCENTER'                           99
     c                   parm                    ##data
     c                   parm      40            ##leng
     c                   movel(p)  ##data        coname
      *
     c                   endsr
     o** ==================================================================== **
     o*  Report for Class Codes with routing page and detail
     o** ==================================================================== **
     o*
     o*  Routing page
     o*
     oQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ***********************************
     o*********
     o*********                                   8 'xxxxxxxx'
     o*********                                  20 'xxxxxx'
     o*********                                  34 'xx      xx'
     o*********                                  45 'xxxxxxxxxx'
     o*********                                  56 'xxxxxxxxxx'
     o*********                                  67 'xxx     xx'
     o*********                                  77 'xxxxxx'
     o********* E            ROUTE          1
     o*********                                   9 'xxxxxxxxx'
     o*********                                  21 'xxxxxxxx'
     o*********                                  34 'xx      xx'
     o*********                                  45 'xxxxxxxxxx'
     o*********                                  56 'xxxxxxxxxx'
     o*********                                  67 'xxxx    xx'
     o*********                                  78 'xxxxxxxx'
     o********* E            ROUTE          1
     o*********                                  10 'xx      xx'
     o*********                                  22 'xx      xx'
     o*********                                  34 'xx      xx'
     o*********                                  41 'xx'
     o*********                                  52 'xx'
     o*********                                  67 'xxxx    xx'
     o*********                                  79 'xx      xx'
     o********* E            ROUTE          1
     o*********                                  10 'xx      xx'
     o*********                                  22 'xx      xx'
     o*********                                  34 'xx      xx'
     o*********                                  41 'xx'
     o*********                                  52 'xx'
     o*********                                  67 'xx xx   xx'
     o*********                                  71 'xx'
     o********* E            ROUTE          1
     o*********                                   9 'xxxxxxxxx'
     o*********                                  22 'xx      xx'
     o*********                                  34 'xx      xx'
     o*********                                  41 'xx'
     o*********                                  52 'xx'
     o*********                                  67 'xx xx   xx'
     o*********                                  71 'xx'
     o********* E            ROUTE          1
     o*********                                   8 'xxxxxxxx'
     o*********                                  22 'xx      xx'
     o*********                                  34 'xx      xx'
     o*********                                  41 'xx'
     o*********                                  52 'xx'
     o*********                                  67 'xx  xx  xx'
     o*********                                  71 'xx'
     o********* E            ROUTE          1
     o*********                                   4 'xxxx'
     o*********                                  22 'xx      xx'
     o*********                                  34 'xx      xx'
     o*********                                  41 'xx'
     o*********                                  52 'xx'
     o*********                                  67 'xx  xx  xx'
     o*********                                  79 'xx    xxxx'
     o********* E            ROUTE          1
     o*********                                   5 'xxxxx'
     o*********                                  22 'xx      xx'
     o*********                                  34 'xx      xx'
     o*********                                  41 'xx'
     o*********                                  52 'xx'
     o*********                                  67 'xx   xx xx'
     o*********                                  79 'xx    xxxx'
     o********* E            ROUTE          1
     o*********                                   6 'xx xxx'
     o*********                                  22 'xx      xx'
     o*********                                  34 'xx      xx'
     o*********                                  41 'xx'
     o*********                                  52 'xx'
     o*********                                  67 'xx   xx xx'
     o*********                                  79 'xx      xx'
     o********* E            ROUTE          1
     o*********                                   7 'xx  xxx'
     o*********                                  22 'xx      xx'
     o*********                                  34 'xx      xx'
     o*********                                  41 'xx'
     o*********                                  52 'xx'
     o*********                                  67 'xx    xxxx'
     o*********                                  79 'xx      xx'
     o********* E            ROUTE          1
     o*********                                   8 'xx   xxx'
     o*********                                  21 'xxxxxxxx'
     o*********                                  33 'xxxxxxxx'
     o*********                                  41 'xx'
     o*********                                  56 'xxxxxxxxxx'
     o*********                                  67 'xx    xxxx'
     o*********                                  78 'xxxxxxxx'
     o********* E            ROUTE          3
     o*********                                   9 'xx    xxx'
     o*********                                  20 'xxxxxx'
     o*********                                  32 'xxxxxx'
     o*********                                  41 'xx'
     o*********                                  56 'xxxxxxxxxx'
     o*********                                  67 'xx     xxx'
     o*********                                  77 'xxxxxx'
     o********* E            ROUTE          1
     o*********                                  15 'Route To:'
     o*********              ORDIS1              60
     o********* E            ROUTE          1
     o*********              ORDIS2              60
     o********* E            ROUTE          1
     o*********              ORDIS3              60
     o********* E            ROUTE          2
     o*********              ORDIS4              60
      ***** End Del ****** 08/26/14 *************************************
      ***** Beg Add ***** 08/05/15 ************************************
     o          E            ROUTE          1
     o                                           15 'Route To:'
     o                       ORDIS1              60
     o          E            ROUTE          1
     o                       ORDIS2              60
     o          E            ROUTE          1
     o                       ORDIS3              60
     o          E            ROUTE          2
     o                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     o          E            ROUTE          2
     o                                           19 'Report Title:'
     o                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ***********************************
     o********* E            ROUTE          2
     o*********                                  19 'Program Name:'
     o*********              ORPNAM              30
     o*********                                  36 'File:'
     o*********              ORFIL#              38
     o*********                                  46 'Copies:'
     o*********              ORCOPY        Z     50
     o*********                                  56 'Hold:'
     o*********              ORHOLD              61
     o********* E            ROUTE          2
     o*********                                  19 'Output Queue:'
     o*********              OROUTQ              30
     o*********                                  37 'Forms:'
     o*********              ORFORM              48
     o*********                                  56 'Save:'
     o*********              ORSAVE              61
      ***** End Del ***** 08/26/14 ***********************************
     o*
     o          E            ROUTE          2
     o                                           19 'Company Nmbr:'
     o                       xxco#               23
     o                                           37 'Group:'
     o                       qqfnd               41
     o          E            ROUTE          1
     o                                           22 'Date Range . . :'
     o                       xxpfdt              34 '  /  /    '
     o                                           36 '-'
     o                       xxptdt              47 '  /  /    '
     o          E            ROUTE          1
     o                                           22 'Print/File/Both:'
     o                       qqtype              25
     o          E            ROUTE          2
     o                                           22 'Sort by State  :'
     o                       qqrsfl              25
     o          E            ROUTE          2
     o                                           15 'Run date:'
     o                       dsdttm              39 '  :  :  &  /  /    '
     O*** Beg Chg *****  01/28/10  ************************************
     o***       E            ROUTE            60
     o***                                           '(c) Copyright Progressi'
     o***                                      +000 've Data Solutions, Inc.,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     O*** End Chg *****  01/28/10  ************************************
     o*
     o*  Heading lines for Class Report
     o*
     oqsysprt   e            head           1 02
     o                       dstime               8 '  :  :  '
     o                       TITLE               70
     o                       dsdate             130 '  /  /    '
     o*
     o          e            head           1
     o                       q1pgm               10
     o                       coname              73
     o                                          125 'PAGE'
     o                       page          z    130
     o*
     o          e            head           2
     o                                           50 'Policy Effective Date Rang'
     o                                           52 'e '
     o                       xxpfdt              63 '  /  /    '
     o                                           71 'through'
     o                       xxptdt              82 '  /  /    '
     o*
     o          e            head           1
     o                                            6 'Group:'
     o                       qqfnd               10
     o                       cpfnam              51
     o          e            head           1
     o                  69                        6 'State:'
     o                  69   xxstnm              27
     o                                           64 'Estimated'
     o                                           86 'Self-Reported'
     o                                          103 'Audited'
     o          e            head           2
     o                                           12 'Class Code'
     o                                           32 'Class Description'
     o                                           65 'Payroll Amt'
     o                                           85 'Payroll Amt'
     o                                          105 'Payroll Amt'
     o*
     o*  Detail lines for Class Report
     o*
     o          e            detl           1
     o                       cpclas        3     10
     o                       cpspec              12
     o                       cpdesc              45
     o                       cpeapr        j     66
     o                       cpsapr        j     86
     o                       cpaapr        j    106
     o*
     o*  Total lines for Class Report
     o*
     o          e            sttot       1  1
     o                       xstate              42
     o                       xxeapr        j     66
     o                       xxsapr        j     86
     o                       xxaapr        j    106
     o*
     o          e            lrtot       1  1
     o                                           15 'Grand Totals:'
     o                       xteapr        j     66
     o                       xtsapr        j     86
     o                       xtaapr        j    106
     o*
     o          e            lrerr       1  1
     o                                           15 'Grand Totals:'
     o                                        +   5 '*** NO RECORDS MATCHED THE'
     o                                        +   1 'SELECTION CRITERIA ***'
     c** ==================================================================== **
