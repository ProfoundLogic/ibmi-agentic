     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H*====================================================================
     H/title Response File Error Report
     H*====================================================================
     H*  Created: 07/16/09   C:198  Project: WC1819 Phase 3.
     H*====================================================================
    H*  Revisions:
     H*
     H*  06/04/25 C:53  Support: 119086
     H*                 - Created from W7484R3 for independent run.         ermi
     H*                 - Changes are not commented.
     H*
     H*  03/06/25 C:333  Project WC2998
     H*                  Changes to CMS Settlement Reporting.
     H*                  - Recompiled.
     H*
     H* 11/01/19 C:79  Support: 84449
     H*                - Replace OPTACOMP condition with client 152         ermi
     H*
     H* 08/05/15 C:307 Project: SY354
     H*                -Adding 'Route To:'
     H*
     H* 08/26/14 C:307 Project: SY354
     H*                -Implement Routing Page Changes.
     H*
     H*  06/09/14   C:62   Support: WC114 85778
     H*                    Populate the group key.
     H*
     H*  03/31/14   C:53   Project: 114WBF
     H*                    Check the alternate claim # for OPTACOMP groups.
     H*
     H*  05/03/13   C:190  Project: 114WEI
     H*                    Add the screen extract date parameter.
     H*
     H*  11/21/11   C:062  Support: 78585
     H*                    Add condition to keep loading fields if read
     H*                    fails
     H*
     H*  04/15/11   C:198  Support: 76572
     H*                    Corrected display of disp code/comp code count to
     H*                    repetition.
     H*
     H*  01/17/11   C:198  Support: 75359
     H*                    Updated the disposition code desc compile time array
     H*
     H*  12/27/10   C:198  Project: WC1879 Priss 113
     H*                    Fixed the issue to show only those claims in the erro
     H*                    report which have received the response.
     H*
     H*
     H*====================================================================

    F*  Physical file definition (for maintaining record)
     F*** keylist starts with @@exdt/co#/d1code/fnd/fyr/case
     FWDD1L3    if   e           k disk    rename(pd101:pd102)
     F*** keylist starts with @@exdt/co#/fnd/fyr/case/d1code
     FWDD1L2    if   e           k disk
     F*** keylist starts with @exdt/co#/e5code/fnd/fyr/case
     FWDE5L2    if   e           k disk    rename(pe501:pe502)
     F*** keylist starts with @@exdt/co#/fnd/fyr/case
     FWDE5L1    if   e           k disk
     F*** keylist starts with @@exdt/co#/c5code/fnd/fyr/case
     FWDC5L2    if   e           k disk    rename(pc501:pc502)
     F*** keylist starts with @@exdt/co#/fnd/fyr/case
     FWDC5L1    if   e           k disk
     F*** keylist starts with co#/E5code
     FWTDEL     if   e           k disk
     F*** keylist starts with co#/c5code
     FWTDML     if   e           k disk
     F*** keylist starts with co#/fnd
     FWMFNL     if   e           k disk
     F*** Begin add *** 03/31/14 **************************************
     Fwmcml17   if   e           k disk    rename(pcm01:pcm17)
     F*** End   add *** 03/13/14 **************************************
     F***
     FWMCML1    if   e           k disk
     F***
     FSMORL     IF   E           K DISK
     * User Parameter
     Fwmupl     if   e           k disk
     * System Company
     Fsmcol     if   e           k disk
     F*******wdr2l1    if   e           k disk    usropn
     F*******wdr2l2    if   e           k disk    usropn
     F*******                              rename(pr201:pr202)
     Fwdf2l     if   e           k disk
     Fwxr0p     if   e             disk    usropn
     F                                     rename(wxr0p:pr001)
     F                                     prefix(r0:2)
    F*  Printer file
     Fqsysprt   o    f  132        printer oflind(*inof)

    D*================================================================
    D*  Data Structures
    D*================================================================
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0

     d                 ds
     d  grp#                          3    inz
     d  year#                         3    inz
     d  case#                         7    inz
     D*====================================================================
     D* Procedures
     D*====================================================================
     D*** Beg Add *** 11/01/19 *********************************************
     D GetClientID     pr                  extpgm('RTVCLTID')
     D CltID                          3
     D CltID           s              3
     D*** End Add *** 11/01/19 *********************************************
      *---------------------------------------------------------------------
      * Constants
      *---------------------------------------------------------------------
     D comapi          c                   const('WA001R')
      *---------------------------------------------------------------------
    D*================================================================
    D*  External Data Structure
    D*================================================================
     D PSDS          ESDS                  EXTNAME(SZQ1P)
    D*---------------------------------------------------------------------
    D*---------------------------------------------------------------------
    D* Stand Alone Fields
    D*---------------------------------------------------------------------
     D rcd_found       S               n   inz
     D xxexdt          S              7  0
     D GrYrCs          S             15
     D ClmntNm         S             25    inz
     D svcode          S                   like(d1code)
     D svc5de          S                   like(c5code)
     D svcede          S                   like(e5code)
     D svfnd           S                   like(e5fnd)
     D svfyr           S                   like(e5fyr)
     D svcase          S                   like(e5case)
     D x1fnd           S                   like(d1fnd)
     D x1fyr           S                   like(d1fyr)
     D x1case          S                   like(d1case)
     D x5fnd           S                   like(c5fnd)
     D x5fyr           S                   like(c5fyr)
     D x5case          S                   like(c5case)
     D scfnd           S                   like(d1fnd)
     D scgnam          S                   like(fnname)
     D sccocd          S                   like(c5code)
     D scercd          S                   like(e5code)
     D scfld           S             20
     D xxfile          S                   like(ORFIL#)
     D scdesc          S             40    inz
     D companynm       s             40
     D ##CO#           S              3  0
     D ##FND           S              3  0 inz
     D ##CONM          S             40    inz
     D ##FNAM          S             40    inz
     D xxcode          S                   like(D1code)
     D xxdesc          S             61
     D xxtot1          S              6  0
     D xxtot2          S              6  0
     D xxcocd          S                   like(c5code)
     D xxcods          S             61
     D xxtot3          S              6  0
     D xxtot4          S              6  0
     D xxtot5          S              6  0
     D xxtot6          S              6  0
     D dispdsc         S             61    dim(5) ctdata perrcd(1)
     D compdsc         S             61    dim(3) ctdata perrcd(1)
     D xxtitl          s                   like(ortitl)
     D x1titl          s                   like(ortitl)
     D xxdata          s             60
     D n               s              3  0
     D ##data          s             80
     D ##leng          s              3  0
     Dx2dcn#           ds            15
     Dxxfnd                    1      3  0
     Dxxfyr                    4      6  0
     Dxxcase                   7     13  0
     D x2code          s                   like(d1code)
     D SVrre#          s                   like(f2rre#)
     D xxrre#          s              9
     D kkfnd           s                   like(e5fnd)
     D*** Begin add *** 03/31/14 **************************************
     D xxpds#          S                   LIKE(CMPDS#) INZ
     D k1fnd           s                   like(CMFND)
     D*** End   add *** 03/13/14 **************************************
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@co#             3
     C                   PARM                    @@exdt            8
     C                   PARM                    @@date            8
      *
     C     datchk        plist
     C                   parm                    srdti             8 0
     C                   parm      '*YYMD'       srffmt            8
     C                   parm      *blanks       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
      *
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      '*YYMD'       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
      *
     C     COMPL         PLIST
     C                   PARM                    ##co#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM
     C                   PARM                    ##FNAM

    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*  KEY LIST FOR WDD1L1
     C*
     C     d1ky          KLIST
     C                   KFLD                    xxexdt            7 0
     C                   KFLD                    ##co#
     C*
     C     d2ky          KLIST
     C                   KFLD                    xxexdt            7 0
     C                   KFLD                    ##co#
     C**********         KFLD                    xxfnd
     C**********         KFLD                    xxfyr
     C**********         KFLD                    xxcase
     C*
     C     f2ky          KLIST
     C                   KFLD                    ##co#
     C                   KFLD                    kkfnd

     C     gnky          KLIST
     C                   KFLD                    xxexdt
     C                   KFLD                    ##co#
     C                   KFLD                    d1fnd
     C                   KFLD                    d1fyr
     C                   KFLD                    d1case

     C     fnky          KLIST
     C                   KFLD                    ##co#
     C                   kfld                    d1fnd

     C     cmky          KLIST
     C                   KFLD                    ##co#
     C                   KFLD                    d1fnd
     C                   KFLD                    d1fyr
     C                   KFLD                    d1case
      *
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXFILE
     C*
     C     deky          KLIST
     C                   KFLD                    ##co#
     C                   KFLD                    scercd
     C*
     C     dmky          KLIST
     C                   KFLD                    ##co#
     C                   KFLD                    svc5de
     C*** Beg Add *** 03/31/14 ****************************************
     C     cmky17        KLIST
     C                   KFLD                    ##co#
     C                   KFLD                    xxpds#
      *
     C     f1ky          KLIST
     C                   KFLD                    ##co#
     C                   KFLD                    k1fnd
     C*** End Add *** 03/31/14 ****************************************
     C*
    C*================================================================
    C*                  M A I N L I N E
    C*================================================================
       //*** Beg Add *** 11/01/19 ******************************************
      /Free
                         callp GetClientID(CltID);
      /End-free
       //*** End Add *** 11/01/19 ******************************************
     C*
     C                   exsr      initialize
     C*
     C                   move      @@exdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxexdt

     C                   move      @@exdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         ##exdt            8 0
     C*** Beg Add *** 05/03/13 ****************************************
     c                   If        @@date = ' '
     c                   eval      @@date = '00000000'
     c                   Endif
     c                   If        @@date <> '00000000'
     C                   move      @@date        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxexdt

     C                   move      @@date        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         ##exdt            8 0
     c                   Endif
     C*** End Add *** 05/03/13 ****************************************

      * Read through WDD1L1 and populate the report data


     C***                open      wdr2l1
     C***                read      pr201                                  90
     C***                if        not *in90
     C***                eval      x2dcn# = r2dcn#
     C*** Beg Add *** 03/31/14 ****************************************
     C***                exsr      OptaCmpNum
     C*** End Add *** 03/31/14 ****************************************

     C*
     C     d2ky          setll     pd101
     C     d2ky          reade     pd101                                  90
     C                   eval      scfnd = D1fnd

     C     fnky          chain     pfn01                              92
     C  N92              movel     fnname        scgnam

     C                   except    Header

     C                   eval      svfnd = D1fnd
     C                   eval      svfyr  = D1fyr
     C                   eval      svcase = D1case

     C                   Dow       *in90 = *off

     C   of              except    Header
     C*
     C                   if        *inof = *on
     C                   eval      *inof = *off
     C                   endif
     C*
     C*****d2ky          setll     pd101
     C*****d2ky          reade     pd101                                  91
     C*******************dow       *in91 = *off
     C*****************  if        d1type <> 'Q' and %char(r2type) = d1type
     C                   if        d1type <> 'Q'
     C                   exsr      prntdata

     C                   move      *blanks       sccocd
     C                   move      *blanks       scercd
     C                   leave
     C                   else
     C**** d2ky          reade     pd101                                  91
     C                   endif
     C*******************enddo

      * Group# Changed..
     C*
     C**************     read      pr201                                  90
     C     d2ky          reade     pd101                                  90
     C                   eval      x2dcn# = d1dcn#
     C*** Beg Add *** 06/09/14 ****************************************
     C                   exsr      OptaCmpNum
     C*** End Add *** 06/09/14 ****************************************
     C                   If        svfnd <> xxfnd
      * Group# Changed..
     C                   If        not rcd_found
     C                   except    fot2
     C                   Endif
     C                   eval      scfnd = xxfnd
     C*
     C     fnky          chain     pfn01                              92
     C                   if        *in92 = *off
     C                   eval      scgnam = %subst(fnname:1:30)
     C                   Endif
     C                   except    skip#
     C                   except    Header
     C                   eval      svfnd = xxfnd
     C                   eval      rcd_found = *off
      *
     C                   Endif

     C                   Enddo
     C******             endif
     C******             close     wdr2l1

     C**** to print summary
     C                   If        rcd_found
     C                   except    skip#
     C                   exsr      prntsummry
     C                   except    fot1
     C                   Endif

     C                   If        not rcd_found
     C                   except    fot1
     C                   Endif

      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxfile = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   Return
     *===============================================================*
     C     PrntData      Begsr
     C                   eval      rcd_found = *on

     C                   move      D1FND         grp#
     C                   move      D1FYR         year#
     C                   move      D1CASE        case#
     C                   move      d1code        x2code
     C                   eval      GrYrCs = grp# + '/' + year#
     C                                      + '/' + case#

     C     cmky          setll     pcm01
     C     cmky          reade     pcm01                                  90
     C                   dow       *in90 = *off
     C                   eval      clmntnm = %trim(%subst(cmlsn:1:15)) + ', '
     C                                        + %trim(%subst(cmfsn:1:7))
     C                                        + ' ' + cmmid
     C     cmky          reade     pcm01                                  90
     C                   enddo

     C     gnky          setll     pe501                              90
     C     gnky          reade     pe501                                  90
     C*
     C     gnky          setll     pc501                              91
     C     gnky          reade     pc501                                  91
     C*
     C                   if        *in90 = '1' and *in91 = '1'
     C                   except    prtdata
     C                   endif
     C*
     C*  case where compliance codes are more than error codes
     C                   dow       *in91 = *off
     C                   eval      sccocd = c5code
     C                   if        *in90 = *off
     C                   eval      scercd = e5code
     C     deky          setll     pde01
     C     deky          reade     pde01                                  93
     C*
     C  N93              movel     dedesc        scdesc
     C  N93              movel     defld         scfld
     C*
     C     gnky          reade     pe501                                  90
     C                   endif
     C                   except    prtdata
     C                   move      *blanks       scdesc
     C                   move      *blanks       scfld
     C                   move      *blanks       sccocd
     C                   move      *blanks       scercd
     C     gnky          reade     pc501                                  91
     C                   enddo
     C* to print left over error codes if error codes are more than compliance c
     C                   move      *blanks       sccocd
     C                   move      *blanks       scercd

     C                   dow       *in90 = *off
     C                   eval      scercd = e5code
     C*
     C     deky          setll     pde01
     C     deky          reade     pde01                                  93
     C*
     C  N93              movel     dedesc        scdesc
     C  N93              movel     defld         scfld
     C*
     C                   except    prtdata
     C                   move      *blanks       scdesc
     C                   move      *blanks       scfld
     C     gnky          reade     pe501                                  90
     C                   enddo

     C                   Endsr
     *===============================================================*
     C     PrntSummry    Begsr

     C*** # of claims for a particular disposition codes - keylist starts with d
     C*********          open      wdr2l2
     C                   exsr      totdisp
     C*********          close     wdr2l2
     C                   exsr      totcomp
     C*** total # of errors -  keylist starts with e5code
     C                   exsr      toterr
     C*** total # of claims with errors -  keylist starts with co#/fnd/fyr/case
     C                   exsr      totclm
     C*

     C                   Endsr
     *===============================================================*
     C     totdisp       Begsr

     C                   except    dispsm
     C*** # of claims for a particular disposition codes - keylist starts with d
     C*** xxtot1 = count of # of cliams for a particular disposition code
     C*** xxtot2 = count of # of claims for all disposition codes
     C***************    read      pr202                                  90
     C*
     C     d2ky          setll     pd101
     C     d2ky          reade     pd101                                  90
     C                   eval      svcode = d1code
     C                   eval      x2dcn# = d1dcn#
     C*** Beg Add *** 06/09/14 ****************************************
     C                   exsr      OptaCmpNum
     C*** End Add *** 06/09/14 ****************************************
     C                   eval      x1fnd = 0
     C                   eval      x1fyr  = 0
     C                   eval      x1case = 0
     C                   eval      xxtot1 = 0
     C                   eval      xxtot2 = 0
     C                   dow       *in90 = *off
     C   of              except    dispsm
     C                   if        *inof = *on
     C                   eval      *inof = *off
     C                   endif
     C*
     C                   if        d1code <> ' '
     C*** # of distinct claims having the same disposition code
     C                   if        x1fnd <> xxfnd or x1fyr <> xxfyr or
     C                             x1case <> xxcase
     C                   eval      xxtot1 = xxtot1 + 1
     C                   eval      x1fnd  = xxfnd
     C                   eval      x1fyr  = xxfyr
     C                   eval      x1case = xxcase
     C                   endif
     C                   endif
     C************       read      pr202                                  90
     C     d2ky          reade     pd101                                  90
     C                   eval      x2dcn# = d1dcn#
     C*** Beg Add *** 06/09/14 ****************************************
     C                   exsr      OptaCmpNum
     C*** End Add *** 06/09/14 ****************************************
     C                   if        d1code <> ' '
     C                   if        d1code <> svcode
     C                             or *in90 = *on
     C*
     C                   if        svcode <>  ' '
     C*
     C                   move      *blanks       xxdesc
     C                   select
     C                   when      svcode = '01'
     C                   movel     dispdsc(1)    xxdesc
     C                   when      svcode = '02'
     C                   movel     dispdsc(2)    xxdesc
     C                   when      svcode = '03'
     C                   movel     dispdsc(3)    xxdesc
     C                   when      svcode = '51'
     C                   movel     dispdsc(4)    xxdesc
     C                   when      svcode = 'SP'
     C                   movel     dispdsc(5)    xxdesc
     C                   endsl
     C*
     C                   except    dispdet
     C                   endif
     C*
     C                   eval      xxtot2 = xxtot2 + xxtot1
     C                   eval      xxtot1 = 0
     C                   eval      svcode = d1code
     C                   eval      x1fnd  = 0
     C                   eval      x1fyr  = 0
     C                   eval      x1case = 0
     C                   endif
     C                   endif
     C                   enddo
     C                   except    disptot
     C*
     C                   Endsr
     *===============================================================*
     C     totcomp       Begsr

     C                   except    compsm
     C*** # of claims for a particular compliance codes - keylist starts with c5
     C*** xxtot3 = count of # of cliams for a particular compliance code
     C*** xxtot4 = count of # of claims for all compliance codes
     C     d1ky          setll     pc502
     C     d1ky          reade     pc502                                  90
     C                   dow       *in90 = *off
     C                   eval      kkfnd = c5fnd
     C     f2ky          chain     pf201                              82
     C                   if        f2rre# = svrre#
     C                   leave
     C                   endif
     C     d1ky          reade     pc502                                  90
     C                   enddo
     C                   eval      svc5de = c5code
     C                   eval      x5fnd = 0
     C                   eval      x5fyr  = 0
     C                   eval      x5case = 0
     C                   eval      xxtot3 = 0
     C                   eval      xxtot4 = 0
     C                   dow       *in90 = *off
     C   of              except    compsm
     C                   if        *inof = *on
     C                   eval      *inof = *off
     C                   endif
     C*** # of distinct claims having the same compliance code
     C                   if        x5fnd <> c5fnd or x5fyr <> c5fyr or
     C                             x5case <> c5case
     C                   eval      xxtot3 = xxtot3 + 1
     C                   eval      x5fnd = c5fnd
     C                   eval      x5fyr  = c5fyr
     C                   eval      x5case = c5case
     C                   endif
     C*
     C     d1ky          reade     pc502                                  90
     C                   dow       *in90 = *off
     C                   eval      kkfnd = c5fnd
     C     f2ky          chain     pf201                              82
     C                   if        f2rre# = svrre#
     C                   leave
     C                   endif
     C     d1ky          reade     pc502                                  90
     C                   enddo
     C*
     C                   if        c5code <> svc5de
     C                             and c5code <> ' ' or *in90 =*on
     C*
     C                   move      *blanks       xxcods
     C*
     C     dmky          setll     pdm01
     C     dmky          reade     pdm01                                  94
     C*
     C  N94              movel     dmdesc        xxcods
     C*
     C                   except    compdet
     C                   eval      xxtot4 = xxtot4 + xxtot3
     C                   eval      xxtot3 = 0
     C                   eval      svc5de = c5code
     C                   eval      x5fnd  = 0
     C                   eval      x5fyr  = 0
     C                   eval      x5case = 0
     C*
     C                   endif
     C                   enddo
     C                   except    comptot
     C*
     C                   Endsr
     *===============================================================*
     C     toterr        Begsr

     C                   eval      xxtot5 = 0
     C*** # of distinct errors - keylist starts with e5code
     C*** xxtot5 = count of # of distinct errors
     C     d1ky          setll     pe502
     C     d1ky          reade     pe502                                  90
     C                   dow       *in90 = *off
     C                   eval      kkfnd = e5fnd
     C     f2ky          chain     pf201                              82
     C                   if        f2rre# = svrre#
     C                   leave
     C                   endif
     C     d1ky          reade     pc502                                  90
     C                   enddo
     C                   eval      svcede = e5code
     C                   dow       *in90 = *off
     C     d1ky          reade     pe502                                  90
     C                   dow       *in90 = *off
     C                   eval      kkfnd = e5fnd
     C     f2ky          chain     pf201                              82
     C                   if        f2rre# = svrre#
     C                   leave
     C                   endif
     C     d1ky          reade     pc502                                  90
     C                   enddo
     C                   if        e5code <> svcede and e5code <> ' '
     C                             or *in90 = *on
     C                   eval      xxtot5 = xxtot5 + 1
     C                   eval      svcede = e5code
     C                   endif
     C                   enddo
     C                   except    errtot1
     C*
     C                   Endsr
     *===============================================================*
     C     totclm        Begsr

     C                   eval      xxtot6 = 0
     C*** # of claims with at least one errors - keylist starts with CO#/FND/FYR
     C*** xxtot6 = count of # of # claims with at least one error.
     C     d1ky          setll     pe501
     C     d1ky          reade     pe501                                  90
     C                   eval      svfnd = e5fnd
     C                   eval      svfyr  = e5fyr
     C                   eval      svcase = e5case
     C                   dow       *in90 = *off
     C     d1ky          reade     pe501                                  90
     C                   if        svfnd  <> e5fnd or svfyr  <> e5fyr
     C                             or svcase  <> e5case or *in90 = '1'
     C                   eval      xxtot6 = xxtot6 + 1
     C                   eval      svfnd = e5fnd
     C                   eval      svfyr  = e5fyr
     C                   eval      svcase = e5case
     C                   endif
     C                   enddo
     C                   except    errtot2
     C*
     C                   Endsr
     *===============================================================*
     C     initialize    Begsr

     C                   movel     @@co#         ##CO#
     *
     C                   open      wxr0p
     C                   read      pr001                                  80
     C                   if        *in80 = *off
     C                   eval      xxrre# = %subst(r0r0p:5:9)
     C                   endif
     C                   move      xxrre#        svrre#
     C                   close     wxr0p
     C     ##co#         chain     pco01                              60
     *
     C                   if        *in60 = *off
     C*
     C                   movel(p)  coname        ##data
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel(p)  ##data        companyNm
     *
     C                   endif
     *
     C     q1user        chain     pup01                              60
     C*
     C* Time & date
     C                   Time                    dsdttm
     C* Printer file info
     C                   MOVE      '0'           XXFILE
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   if        ordis1 = *blanks
     C                             and ordis2 = *blanks
     C                             and ordis3 = *blanks
     C                             and ordis4 = *blanks
     C                   movel(p)  upusnm        ordis1
     C                   endif
     *
     C                   movel     ortitl        x1titl
     *
     C                   if        ortitl <> *blanks
     C                   move(p)   ortitl        xxdata
     C     ' '           check     xxdata        n
     C                   if        n <= 60
     C                   subst(p)  xxdata:n      xxtitl
     C                   endif
     C                   endif
     *
     C                   movel(p)  xxtitl        ##data
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      60            ##leng
     C                   movel(p)  ##data        xxtitl
     *
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   ENDIF

     C                   Endsr
     C*** Beg Add *** 03/31/14 ****************************************
     C     OptaCmpNum    begsr
     C                   movel(p)  d1dcn#        xxpds#
     C     cmky17        chain     pcm17

     C                   if        %found
     C*** Begin Chg *** 06/09/14 *********************************************
     C**********         move      CMfnd         X1fnd
     C                   Z-add     CMfnd         k1Fnd
     C*** End   Chg *** 06/09/14 *********************************************
     C     f1ky          chain     pfn01

     C*** Beg Chg *** 11/01/19 *********************************************
     C****************** IF        %found and FNgrpasc = 'OPTACOMP'
     C                   IF        %found and CltID = '152'
     C*** End Chg *** 11/01/19 *********************************************
      *** update x2dcn# subfields
     C                   move      *blanks       x2dcn#
     C                   move      CMfnd         xxfnd
     C                   move      CMfyr         xxfyr
     C                   move      CMcase        xxcase
     C                   endif
     C                   endif
     C                   endsr
     C*** End Add *** 03/31/14 ****************************************
     *===============================================================*
     OQsysprt   e            route          1 06
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
     O**********
     O**********E            ROUTE          1
     O**********                                 15 'Route to:'
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
     O**********
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'Route to:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'Report Title:'
     O                       x1titl              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'Program Name:'
     O**********             ORPNAM              30
     O**********                                 36 'File:'
     O**********             ORFIL#              38
     O**********                                 46 'Copies:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'Hold:'
     O**********             ORHOLD              61
     O**********
     O**********E            ROUTE          2
     O**********                                 19 'Output queue:'
     O**********             OROUTQ              30
     O**********                                 37 'Forms:'
     O**********             ORFORM              48
     O**********                                 56 'Save:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O*
     o          e            route            58
     o                                         +001 '(c)'
     o                       *year             +001
     o                                         +001 'StoneRiver, Inc.'
     o                                         +001 'All rights reserved.'
     o          e            route       1
     o                                         +001 'StoneRiver is a'
     o                                         +001 'trademark of'
     o                                         +001 'StoneRiver, Inc.'
     O          E            route          1  1
     O                       dstime              09 '  :  :  '
     O                       CompanyNm           86
     O                       dsdate             132 '  /  /    '
     O*
     O          e            Header      1  2
     o                       q1pgm               11
     O                       xxtitl              96
     O                                          126 'Page'
     O                       page          Z    132
     O          e            Header         2
     O                                           15 'Response Date:'
     O                       ##exdt              26 '  /  /    '
     O          e            Header         2
     O                                            7 'Group:'
     O                       scfnd               11
     O                       scgnam              72
     O          e            Header         1
     O                                           12 'Grp/Yr/Case'
     O                                           27 'Claimant'
     O                                           52 'Disp'
     O                                           58 'Comp'
     O                                           66 'Error'
     O                                           74 'Field'
     O                                          103 'Description'
     O          e            Header         2
     O                                           23 'Name'
     O                                           52 'Code'
     O                                           58 'Code'
     O                                           65 'Code'
     O*
     O          e            prtdata        1
     O                       GrYrCs              16
     O                       ClmntNm             44
     O                       x2CODE              51
     O                       sccocd              57
     O                       scercd              66
     O                       scfld               89
     O                       scdesc             132
     O*
     O          e            dispsm         2
     O                                           65 'Response File Summary'
     O*
     O          e            dispsm         1
     O                                           17 'Disposition Code'
     O                                           32 'Description'
     O                                          110 'Total# of Claims'
     O*
     O          e            dispdet        1
     O                       svcode               9
     O                       xxdesc              82
     O                       xxtot1        J    110
     O*
     O          e            disptot     1  2
     O                                          100 'Total:'
     O                       xxtot2        J    110
     O*
     O          e            compsm         1
     O                                           16 'Compliance Code'
     O                                           32 'Description'
     O                                          110 'Total# of Claims'
     O*
     O          e            compdet        1
     O                       svc5de               9
     O                       xxcods              82
     O                       xxtot3        J    110
     O*
     O          e            comptot     1  2
     O                                          100 'Total:'
     O                       xxtot4        J    110
     O*
     O          e            errtot1        1
     O                                           17 'Total# of Errors'
     O          e            errtot1        2
     O                       xxtot5        J      8
     O*
     O          e            errtot2        1
     O                                           26 'Total# of Claims w/Errors'
     O          e            errtot2        2
     O                       xxtot6        J      8
     O*
     O          e            Fot1           1
     O                                           61 ' *** End  Of  Report *** '
     O*
     O          e            Fot2           1
     O                                           61 ' *** No Records Found *** '
     O*
     O          e            Skip#                1
** dispdsc
Record accepted by CMS; ORM exists
Claim Input-rec accept;RRE has indicated 'NO' ongoing responsibility for medical
Claims input-rec accept;record ident as medicare beneficiary but no medicare cov
Qry-not a medicare beneficiary
Claims input-record not accepted due to errors in the data; correct & resubmit w
** compdsc
Late submission of TPOC
Invalid RRE TIN
Late submission of ORM Termination Date
