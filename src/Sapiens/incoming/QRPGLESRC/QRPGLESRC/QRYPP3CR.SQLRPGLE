     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
      *=========================================================================
      /title QRYPP3CR - Compare PP3C Info in Emp Dtl to Group
     h option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     h debug(*yes)
     h bnddir('WTBNDDIR')
      *  Program returns policies that are flagged as having different
      *  Policy Page 3C Info than what is currently in Groups (ELFFLA = 'Y')
      *  Called from: Menu or Cmd Line
      *  Output to report and file QRYPP3C in library where WDELP resides
      *  Created 08/25/11   C:136   PROJECT: WC1918
      *=========================================================================
     fwdell     if   e           k disk
     fwdf4l     if   e           k disk
     fwmshl1    if   e           k disk
     fsmorl     if   e           k disk
     fsmcol     if   e           k disk
     fqrypp3c   o    e             disk
     fqsysprt   o    f  185        printer oflind(*inof)
      *=========================================================================
      * Stand alone fields
      *=========================================================================
     d sysDate         s              7  0
     d sysTime         s              6  0
     d #brnd1          s             80    inz
     d #brnd2          s             80    inz
     d xxCo#           s                   like(ELCO#)
     d userName        s             50
     d companyName     s             40
     d titleCoName     s             40
     d titleRpt        s                   like(ORTITL)
     d                                     inz('POLICY PAGE 3C VBG: -
     d                                     POLICY VS GROUP')
     d PolExpDate      s               d   datfmt(*usa)
     d cxDate          s               d   datfmt(*usa)
     d headerLine      s            185    inz(*all'_')
     d runEnv          s              5
     d rptStatus       s              3
      ***** Beg Add ***** 08/26/14 ************************************
     d kkpgm           s                   like(ORPNAM) inz('QRYPP3CR')
     d kkfil#          s                   like(ORFIL#) inz('0')
      ***** End Add ***** 08/26/14 ************************************
      *=========================================================================
      * Prototypes
      *=========================================================================
     d/copy sprRtvBrnd
     d/copy sprString
     d/copy sprDate
     d/copy wprUsrPrms
     d/copy wprEmplr
     d initialize      pr
     d report          pr
     d writeFile       pr
      *=========================================================================
      * Plist
      *=========================================================================
     c     *entry        plist
     c                   parm                    inEnv             3
     c                   parm                    inUser           10
     c                   parm                    inOutQ           10

      ***** Beg Add ***** 08/26/14 ************************************
     c     orky          klist
     c                   kfld                    kkpgm
     c                   kfld                    kkfil#
      ***** End Add ***** 08/26/14 ************************************

      *=========================================================================
      * Main
      *=========================================================================
     c                   callp     initialize
     c                   callp     report
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       Chain orky smorl;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     c                   seton                                        lr
      *=========================================================================
      * Print
      *=========================================================================
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             USERNAME            67
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       USERNAME            67
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             inOutQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     o          e            route          1
     o                                           20 'COMPANY  . . :'
     o                       xxCo#               25
     o                       companyName         +2
     o          e            route          1
     o                                           20 'ENVIRONMENT  :'
     o                       runEnv              30
     o          e            route          1 59
     o                       #brnd1
     o          e            route          1
     o                       #brnd2
     o          e            head           1 06
     o                       sysTime                '  :  :  '
     o                       titleCoName        112
     o                       sysDate            185 '  /  /    '
     o          e            head           2
     o                                              'QRYPP3CR'
     o                       titleRpt           122
     o                                          179 'PAGE:'
     o                       page          z    185
     o          e            head           1
     o                                              'Grp'
     o                                           +3 'Employer'
     o                                           +2 'Div'
     o                                           +4 'Fyr'
     o                                           +2 'Pol Exp Dt'
     o                                           +3 'Emp Cx Dt'
     o                                           +2 'Submitted'
     o                                           +2 'Policy 3C Text 1'
     o                                          +46 'Policy 3C Text 2'
     o          e            head           0
     o                       headerLine
     o          e            head           2
     o                                          +52 'to WCPOLS'
     o                                          + 2 'Group 3C Text 1'
     o                                          +47 'Group 3C Text 2'
     o          e            detail         1
     o                       P3FND
     o                       P3EMP#        3     +2
     o                       P3DIV         3     +2
     o                       P3FYR         3     +2
     o                       P3PTDT        Y     +2
     o                       P3CXDT              +2 '  /  /   0'
     o                       rptStatus           +2
     o                       P3POL1              +8
     o                       P3POL2              +2
     o          e            detail         1
     o                       P3GRP1             +63
     o                       P3GRP2              +2
      *====================================================================
      * subprocedures
      *=====================================================================
     p initialize      b
     d initialize      pi
     d kkpgm           s                   like(ORPNAM) inz('QRYPP3CR')
     d kkfil#          s                   like(ORFIL#) inz('0')

     c     orky          klist
     c                   kfld                    kkpgm
     c                   kfld                    kkfil#

     c     coky          klist
     c                   kfld                    xxco#

     c                   if        inEnv = 'USR'
     c                   eval      inOutQ = getUsrOutq(inUser)
     c                   seton                                        lr
     c                   return
     c                   endif

     c                   eval      xxCo# = userCo#(inUser)
     c                   if        xxCo# = 0
     c                   seton                                        lr
     c                   return
     c                   endif

     c                   select
     c                   when      inEnv = 'LIV'
     c                   eval      runEnv = 'LIVE'
     c                   when      inEnv = 'QTE'
     c                   eval      runEnv = 'QUOTE'
     c                   when      inEnv = 'TST'
     c                   eval      runEnv = 'TEST'
     c                   when      inEnv = 'PDS'
     c                   eval      runEnv = 'PDS'
     c                   endsl

     c     coky          chain     pco01
     c                   eval      companyName = CONAME
     c                   eval      userName = getUsrName(inUser)
     c                   eval      sysDate = DateTo7(SystemDate)
     c                   eval      sysTime = timeTo6(SystemTime)
     c                   callp     rtvBrnd('@':#Brnd1:#brnd2)

     c     orky          chain     smorl
     c                   if        %found
     c                   eval      titleRpt = ORTITL
      ***** Beg Del ***** 08/26/14 ************************************
     c**********         except    route
      ***** End Del ***** 08/26/14 ************************************
     c                   endif

     c                   eval      titleCoName = centerFld(companyName : 40)
     c                   eval      titleRpt    = centerFld(titleRpt : 60)

     p initialize      e
      *====================================================================
     p report          b
     d report          pi
     d svFnd           s                   like(ELFND)
     c                   read      pel01
     c                   dow       not(%eof)

     c                   if        ELCO# <> xxCo#
     c                   read      pel01
     c                   iter
     c                   endif

     c                   if        svFnd <> ELFND
     c                   eval      *inof = *on
     c                   eval      svFnd = ELFND
     c                   endif

     c                   callp     writeFile

     c                   read      pel01
     c                   enddo

     p report          e
      *====================================================================
     p writeFile       b
     d writeFile       pi
     d type            s                   like(SHTYPE) inz('P')
     d key             s                   like(F4KEY)  inz('PP3C')
     d xxSbmStatus     s                   like(P3SHST)
     d xxGrp1          s                   like(P3GRP1)
     d xxGrp2          s                   like(P3GRP2)
     d cxDate          s                   like(ELPTDT)

     c     shky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELFYR
     c                   kfld                    type

     c     f4ky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    key

     c     f4ky1         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    key
     c                   kfld                    ELPFDT

     c                   eval      cxDate = getCxDate( ELCO#  :
     c                                                 ELFND  :
     c                                                 ELEMP# :
     c                                                 ELDIV  )

     c                   if        ELPTDT <= sysDate or
     c                             (cxDate <> 0 and cxDate <= sysDate)
     c                   return
     c                   endif

     c                   if        ELFFLA <> 'Y'
     c                   return
     c                   endif

     c     f4ky1         setgt     wdf4l
     c     f4ky          readpe    wdf4l
     c                   select
     c                   when      %eof
     c                   eval      xxGrp1 = *blanks
     c                   eval      xxGrp2 = *blanks
     c                   other
     c                   eval      xxGrp1 = F4TXT1
     c                   eval      xxGrp2 = F4TXT2
     c                   endsl

     c     shky          chain     wmshl1
     c                   select
     c                   when      not(%found) or SHSTAT < '4'
     c                   eval      xxSbmStatus = 'N'
     c                   when      SHSTAT = '4'
     c                   eval      xxSbmStatus = 'Y'
     c                   endsl

     c                   eval      P3ENV  = inEnv
     c                   eval      P3CO#  = ELCO#
     c                   eval      P3FND  = ELFND
     c                   eval      P3EMP# = ELEMP#
     c                   eval      P3DIV  = ELDIV
     c                   eval      P3FYR  = ELFYR
     c                   eval      P3FFLA = ELFFLA
     c                   eval      P3PTDT = dtMDYY(ELPTDT)
     c                   eval      P3CXDT = dtMDYY(getCxDate( ELCO#  :
     c                                                        ELFND  :
     c                                                        ELEMP# :
     c                                                        ELDIV  ))
     c                   eval      P3SHST = xxSbmStatus
     c                   select
     c                   when      xxSbmStatus  = 'N'
     c                   eval      rptStatus = 'No'
     c                   when      xxSbmStatus  = 'Y'
     c                   eval      rptStatus = 'Yes'
     c                   endsl
     c                   eval      P3GRP1 = xxGrp1
     c                   eval      P3GRP2 = xxGrp2
     c                   eval      P3POL1 = ELPP3C
     c                   eval      P3POL2 = ELPPC3
     c                   eval      P3USER = inUser
     c                   eval      P3PGM  = 'QRYPP3CR'
     c                   eval      P3DT   = sysDate
     c                   eval      P3TIME = sysTime
     c                   write     pp301

     c                   if        *inof
     c                   except    head
     c                   eval      *inof = *off
     c                   endif
     c                   except    detail

     p writeFile       e
