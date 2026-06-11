     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H*
     H/TITLE W7481R1  - Medicare Extract Edit Report Program
    H**************************************************************************
    H*  Description:
    H*  This program reads through the records in WXMDP to verify data is fille
     H*  required and data falls within defined constraints (that would not have
    H*  caught by screen editing) and mutually inclusive fields have correspond
    H*  as required
    H*  Generate error report listing all issues found for each claim
    H*
    H*  Called from: W7481R
    H*
    H*  Calls to: None.
    H*
    H*  Created: C:198  Project: WC1819 Phase 4  Date:  10/26/09
    H**************************************************************************
    H*  Revisions:
     H*
     H* 12/10/24 C:053 Support: 146/117910
     H*                Remove edit for future dates.
     H*
     H* 05/21/20 C:079 Support: 146/78896
     H*                Add CMS Reporting Edits.
     H*
     H* 02/08/16 C:306 Project: WC2403
     H*                CMS Reporting Edits.
     H*
     H* 10/20/15 C:53  PW: 37650
     H*                Add ICD10 Processing
     H*
     H* 08/05/15 C:307 Project: SY354
     H*                -Adding 'Route To:'
     H*
     H* 08/26/14 C:307 Project: SY354
     H*                -Implement Routing Page Changes.
     H*
/* 04H*  04/12/11  C: 58  Project: WC1975   Add parameter for report sequence: G
/*   H*                   printing of report, instead write output log file with
/*   H*                   data and adjuster staff code. Read log file at end of
/*   H*                   print report, in either group or adjuster sequence.
     H*
     H*  02/17/11   C:198  Project: WC1879 Priss 143
     H*                    -Edit for firm name first, if non blank otherwise edi
     H*                     Rep first and last name
     H*
     H*  02/07/11   C:198  Project: WC1980
     H*                    -Added edit for Rep FEIN if claimant relationship is
     H*
     H*  01/21/11   C:198  Project: WC1879 Priss 142
     H*                    -Send only CMS flag as 'I' ICD9 code to Medicare edit
     H*
     H*  01/05/11   C:198  Project: WC1879 Priss 143
     H*                    Fixed the CR14 edit to check for only lsn, not fsn.
     H*
     H*  11/18/10   C:53   Project: WC1879 Priss 79
     H*                    DN58-62 are now blanks, remove edits
     H*
     H*  11/01/10   C:251  Support: 74648
     H*                    Added additional edit to TPOC date/Settlement date
     H*
     H*  10/12/10   C:198  Project: WC1879 Priss 67
     H*                    Made gender a required field
     H*
     H*  10/05/10   C:251  Project: WC1944
     H*                    Added edits for settlement date to be greater than CM
     H*                    TPOC date greater than CMS DOI, Date of birth mandato
     H*                    and allow hyphens,apostrophes and spaces in last name
     H*
     H*  08/09/10   C:198  Project: WC1879 Priss 43
     H*                    Added a blank line between 2 records in the report
     H*                    Incorporated the funtioncaluty for error desc. to mov
     H*                    to next line if word is splitting between 2 lines.
     H*
     H*  05/18/10   C:198  Project: WC1879
     H*                    Added few edits for Alleged cause of injury, ICD-9 co
     H*                    Product libilities entries, office code/Site ID, TPOC
     H*                    Amount 1/Date2/Amount 2, Rep last/first name, Firm na
     H*                    ORM termination date.
     H*
    H**************************************************************************
    H*=======================================================================
    H*       INDICATOR USAGE
     H*   60  General Chain, Read
     H*
     H*          94   Generic Claim Sequence  Group in heading only        ' '
     H*          95    Group Sequence; print Adjustor code/name in detail  'G'
     H*          96    Adjustor sequence; print Group #/name in detail     'S'
     H*   97  Read WFMDL/L1 files
     H*
    H*=======================================================================
    F*
     Fwxmdl     if   e           k disk
     Fsmorl     if   e           k disk
     FWMCML1    if   e           k disk
     FWTDEL     if   e           k disk
     F*** Beg Del *** 05/18/10 ****************************************
     F****SMDIL     if   e           k disk
     F*** End Del *** 05/18/10 ****************************************
     F*** keylist starts with co#/fnd
     FWMFNL     if   e           k disk
     * User Parameter
     Fwmupl     if   e           k disk
     * System Company
     Fsmcol     if   e           k disk
     F*** Beg Add *** 05/18/10 ****************************************
     Fsmdil5    if   e           k disk    rename(pdi01:pdi05)
     F*** End Add *** 05/18/10 ****************************************
     F* **Beg Add *** 04/12/11 **********************************************
     Fwfmdp     o  a e             disk    usropn
     Fwfmdl     if   e           k disk    Rename(pmd01 : pmd02) usropn
     Fwfmdl1    if   e           k disk    Rename(pmd01 : pmd03) usropn
     Fsmupl1    if   e           k disk    rename(pup01 : pup02)
     F* **End Add *** 04/12/11 **********************************************
     F**** Beg Add *** 05/21/20 **********************************************
     Fwmicd10l  if   e           k disk
     F*** End Add *** 05/21/20 **********************************************
     Fqsysprt   o    f  132        printer oflind(*inof)
    D*================================================================
    D*  External Data Structure
    D*================================================================
     D PSDS          ESDS                  EXTNAME(SZQ1P)
    D*================================================================
    D* Stand Alone Fields
    D*================================================================
     Dxxfile           S                   like(orfil#)
     D ##CO#           S              3  0 inz
     D @@CO#           S              3    inz
     D ##FND           S              3  0 inz
     D ##CONM          S             40    inz
     D ##FNAM          S             40    inz
     D companynm       s             40
     D ##data          s             80
     D ##leng          s              3  0
     D scfnd           S                   like(d1fnd)
     D scgnam          S                   like(fnname)
     D svfnd           S                   like(d1fnd)
     D*** Beg Add *** 04/12/11 **********************************
     D svstaf          S                   like(mdadcd)
     D scstaf          S                   like(mdadcd)
     D adjnamp         S             30    inz
     D fndnamp         S             30    inz
     D*** End Add *** 04/12/11 **********************************
     D ClmntNm         S             25    inz
     D YrCs            S             11
     D scercd          S              5    inz
     D scdata          S             20    inz
     D scdesc          S             30    dim(3)
     D scfld           S             30
     D rcd_found       S               n   inz
     D xxdt            S              8  0
     D xxdate          S              8  0
     D i               S              2  0
     D*** Beg Add *** 08/09/10 **********************************
     D j               S              2  0
     D k               S              2  0
     D*** End Add *** 08/09/10 **********************************
     D*** Beg Add *** 10/05/10 **********************************
     D xxdtst          S              8  0
     D xxtpoc          S              8  0
     D*** End Add *** 10/05/10 **********************************
     D xxc1rf          S              2
     D xxc1rm          S              2
     D xxc1rn          S              2
     D x1c1rf          S              1
     D x1c1rm          S              1
     D x1c1rn          S              1
     D xxlsn           S              2
     D xxfdnm          S              1
     D svlsn           S                   like(d1lsn)
     D svfsn           S                   like(d1fsn)
     D svfdnm          S                   like(d1fdnm)
     D svc1rn          S                   like(d1c1rn)
     D svc1rm          S                   like(d1c1rm)
     D svc1rf          S                   like(d1c1rf)
     D xxtitl          s                   like(ortitl)
     D x1titl          s                   like(ortitl)
     D xxdata          s             60
     D n               s              3  0
     D*** Beg Add *** 05/18/10 ****************************************
     D xxormd          s              8  0
     D x1ormd          s              8  0
     D xxdt1           s               d
     Dxxexdt           s              8  0
     D xxdt2           s               d
     D xxmnth          s             15P 0
     D kkicd9          s                   like(d1i901)
     D*** End Add *** 05/18/10 ****************************************
     D*** Beg Add *** 05/18/10 ****************************************
     D kkIcd10         s                   Like(ICcode)
    D*================================================================
    D* Constants
    D*================================================================
     D comapi          c                   const('WA001R')
     D*** Beg Chg *** 10/05/10 ****************************************
     D*valchrl         c                   const('abcdefghijklmnopqrstuvwx-
     D*******                              yz ''')
     D valchrl         c                   const('abcdefghijklmnopqrstuvwx-
     D                                     yz- ''')
     D*** End Chg *** 10/05/10 ****************************************
     D valchrf         c                   const('abcdefghijklmnopqrstuvwx-
     D                                     yz ')
     D valalph         c                   const('abcdefghijklmnopqrstuvwxyz')
     D HI              c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
     D LO              c                   const('abcdefghijklmnopqrstuvwxyz')
     D valnum          c                   const('1234567890')
    D*================================================================
    D* Data Structures
    D*================================================================
     D                 ds
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0
     D*
     d                 ds
     d  year#                         3    inz
     d  case#                         7    inz
    C*================================================================
    C* Klist Definitions
    C*================================================================
     C*
     C     cmky          KLIST
     C                   KFLD                    ##co#
     C                   KFLD                    d1fnd
     C                   KFLD                    d1fyr
     C                   KFLD                    d1case
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXFILE
     C*
     C     DEKY          KLIST
     C                   KFLD                    ##co#
     C                   KFLD                    scercd
     C*
     C     DIKY          KLIST
     C*** Beg Chg *** 05/18/10 **********************************
     C******             KFLD                    D1I901
     C                   KFLD                    kkicd9
     C*** End Chg *** 05/18/10 **********************************
     C*
     C     fnky          KLIST
     C                   KFLD                    ##co#
     C                   kfld                    d1fnd
     C* **Beg Add *** 04/12/11 **********************************************
     C     fnky2         KLIST
     C                   KFLD                    ##co#
     C                   kfld                    mdfnd
     C* **End Add *** 04/12/11 **********************************************
     C*
    C*================================================================
    C* Plist Definitions
    C*================================================================
     C* **Beg Add *** 04/12/11 **********************************************
     C     *entry        PLIST
     C                   PARM                    @@SRT             1
     C* **End Add *** 04/12/11 **********************************************
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM
     C                   PARM                    ##FNAM
     C*
     C     dtyymd        plist
     C                   parm                    srdti             8 0
     C                   parm      '*CYMD'       srffmt            8
     C                   parm      '*YYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     C*
     C     dtyymd1       plist
     C                   parm                    srdti             8 0
     C                   parm      *blanks       srffmt            8
     C                   parm      '*YYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     C*
     C     dtmdyy        plist
     C                   parm                    srdti             8 0
     C                   parm      '*CYMD'       srffmt            8
     C                   parm      *blanks       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     C*
    C*================================================================
     C*
     C                   exsr      initialize
     C*
     C     ##co#         setll     pd101
     C     ##co#         reade     pd101                                  90
     C*
     C                   eval      svfnd = d1fnd
     C                   eval      scfnd = d1fnd
     C*
     C     fnky          chain     pfn01                              92
     C  N92              movel     fnname        scgnam
     C*
     C*
     C* **Beg Dlt *** 04/12/11 **********************************************
     C************       except    Header
     C* **End Dlt *** 04/12/11 **********************************************

     C*
     C                   dow       *in90 = *off
     C*
     C* **Beg Dlt *** 04/12/11 **********************************************
     C** of ***********  except    Header
     C* **End Dlt *** 04/12/11 **********************************************
     C*
     C                   if        *inof = *on
     C                   eval      *inof = *off
     C                   endif
     C*
     C                   move      D1FYR         year#
     C                   move      D1CASE        case#
     C                   eval      YrCs =  year# + '-' + case#
     C*
     C     cmky          setll     pcm01
     C     cmky          reade     pcm01                                  90
     C                   dow       *in90 = *off
     C                   eval      clmntnm = %trim(%subst(cmlsn:1:24)) + ', '
     C                                        + %trim(%subst(cmfsn:1:10))
     C                                        + ' ' + cmmid
     C     cmky          reade     pcm01                                  90
     C                   enddo
     C*
     C                   exsr      edits
     C*
     C     ##co#         reade     pd101                                  90
     C*
     C                   If        svfnd <> d1fnd
      * Group# Changed..
     C                   eval      scfnd = d1fnd
     C*
     C     fnky          chain     pfn01                              92
     C  N92              movel     fnname        scgnam
     C*
     C* **Beg Dlt *** 04/12/11 **********************************************
     C************       except    skip#
     C************       except    Header
     C* **End Dlt *** 04/12/11 **********************************************
     C*
     C                   eval      svfnd = d1fnd
     C                   Endif
     C*
     C                   enddo
     C*
     C* **Beg Add *** 04/12/11 **********************************************
     C                   exsr      printrpt
     C* ** End Add *** 04/12/11 **********************************************
     C*
     C                   If        rcd_found
     C                   except    fot1
     C                   Endif
     C*
     C                   If        not rcd_found
     C                   except    fot2
     C                   except    fot1
     C                   Endif
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxfile = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   seton                                        lr
    C*================================================================
     C     initialize    begsr
     C*
     C                   call      comapi        compl
     C*
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
    C*
     C* **Beg Add *** 04/12/11 **********************************************
    C*
    C*       Determine print mode
     C                   select
     C                   when      (@@srt = 'S')
     C                   seton                                        96
     C                   when      (@@srt = 'G')
     C                   seton                                        95
     C                   other
     C                   seton                                        94
     C                   endsl
    C*
     C                   open      wfmdp
     C* **Beg Add *** 04/12/11 **********************************************
    C*
     C     q1user        chain     pup01                              60
     C*
     C                   time                    dsdttm
     C*
     C                   Move      dsdate        srdti
     C                   call      'SRCVTDAT'    dtyymd1
     C                   movel     srdto         xxdate
     C*
     C                   Z-Add     0             srdti
     C* Printer file info
     C                   MOVE      '0'           XXFILE
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     *  User Name if no Distribution specified in routing
     *
     C                   if        ordis1 = *blanks
     C                             and ordis2 = *blanks
     C                             and ordis3 = *blanks
     C                             and ordis4 = *blanks
     C                   movel(p)  upusnm        ordis1
     C                   endif
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
     C                   movel(p)  ortitl        x1titl
     *
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   ENDIF
     C                   endsr
     *
     C* **Beg Add *** 04/12/11 **********************************************
    C*================================================================
     C*    Write WFMDP file
    C*================================================================
     C     writemdp      begsr
     C                   clear                   pmd01
     C*
     C                   move      CMCO#         MDCO#
     C                   move      scfnd         MDFND
     C*
     C*  Get adjustor data
     C                   movel(p)  CMCLEX        MDADCD
     C     CMCLEX        chain     pup02                              60
     C                   if        *in60 = *off
     C                   movel(p)  UPUSNM        MDADNM
     C                   endif
     C                   move      year#         MDFYR
     C                   move      case#         MDCASE
     C                   move      clmntnm       MDCLNM
     C                   move      scercd        MDERCD
     C                   move      scfld         MDFLD
     C                   move      scdata        MDDATA
     C                   move      scdesc(1)     MDDSC1
     C                   move      scdesc(2)     MDDSC2
     C                   move      scdesc(3)     MDDSC3
     C*
     C                   write     pmd01
     C*
     C                   endsr
    C*======================================================================
     C*    Print report
    C*======================================================================
     C     printrpt      begsr
     c*
     C                   close     wfmdp
     c*
     C* =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
     C*       Read and print report in group sequence
     C                   if        @@srt = 'G' or @@srt = ' '
     C                   open      wfmdl
     C     ##co#         setll     pmd02
     C     ##co#         reade     pmd02                                  97
     C*
     C     fnky2         chain     pfn01                              92
     C  N92              movel     fnname        scgnam
     C                   eval      *inof = *on
     C*
     C                   eval      svfnd = mdfnd
     C                   eval      scfnd = mdfnd
     C                   eval      svstaf = mdadcd
     C                   eval      scstaf = mdadcd
     C*
     C                   dow       *in97 = *off
     C   of              except    Header
     C                   eval      *inof = *off
     C*
     C                   exsr      lodprt
     C*
     C     fnky2         chain     pfn01                              92
     C  N92              movel     fnname        scgnam
     C*
     c*  Print
     C                   except    prtdata
     C*  Additional lines
     C                   if        @@srt = 'G' or scdesc(2) <> ' '
     C                   except    prtdata2
     C                   endif
     C                   if        scdesc(3) <> ' '
     C                   except    prtdata3
     C                   endif
     C                   except    space#
     C*
     C     ##co#         reade     pmd02                                  97
     C*
     C* If group# changed ...
     C                   If        svfnd <> mdfnd
     C                   eval      scfnd = mdfnd
     C     fnky2         chain     pfn01                              92
     C  N92              movel     fnname        scgnam
     C*
     C                   except    skip#
     C                   except    Header
     C*
     C                   eval      *inof = *off
     C                   eval      svfnd = mdfnd
     C                   Endif
     C*
     C                   enddo
     C*
     C* =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
     C*       Read and print report in adjustor sequence
     C                   else
     C*
     C                   open      wfmdl1
     C     ##co#         setll     pmd03
     C     ##co#         reade     pmd03                                  97
     C*
     C     fnky2         chain     pfn01                              92
     C  N92              movel     fnname        scgnam
     C                   eval      *inof = *on
     C*
     C                   eval      svfnd = mdfnd
     C                   eval      scfnd = mdfnd
     C                   eval      svstaf = mdadcd
     C                   eval      scstaf = mdadcd
     C*
     C                   dow       *in97 = *off
     C   of              except    Header
     C                   eval      *inof = *off
     C*
     C                   exsr      lodprt
     C*
     c*  Print
     C                   except    prtdata
     C*  Additional lines
     C                   except    prtdata2
     C                   if        scdesc(3) <> ' '
     C                   except    prtdata3
     C                   endif
     C                   except    space#
     C*
     C     ##co#         reade     pmd03                                  97
     C*
     C* If staff changed ...
     C                   If        svstaf <> mdadcd
     C                   eval      scstaf = mdadcd
     C*
     C     fnky2         chain     pfn01                              92
     C  N92              movel     fnname        scgnam
     C*
     C                   except    skip#
     C                   except    Header
     C*
     C                   eval      *inof = *off
     C                   eval      svstaf = mdadcd
     C                   Endif
     C*
     C                   enddo
     C*
     C                   endif
     C*
     C*
     C                   endsr
    C*======================================================================
     C*    Load fields to print
    C*======================================================================
     C     lodprt        begsr
     c*
     c*    Load print fields
     C                   move      MDFYR         year#
     C                   move      MDCASE        case#
     C                   move      MDCLNM        clmntnm
     C                   move      MDERCD        scercd
     C                   move      MDFLD         scfld
     C                   move      MDDATA        scdata
     C                   move      MDDSC1        scdesc(1)
     C                   move      MDDSC2        scdesc(2)
     C                   move      MDDSC3        scdesc(3)
     c*
     c*    Load etc fields
     C                   eval      YrCs =  year# + '-' + case#
     C*
     c*
     C                   endsr
     c*
    C*======================================================================
     C* **End Add *** 04/12/11 **********************************************
     C*
    C*======================================================================
     C     edits         begsr
     C** Injured party SSN#
     C                   if        d1ss#  = ' ' or d1ss# = '000000000'
     C                   seton                                        23
     C                   else
     C     valnum        check     d1ss#                                  21
     C                   endif
     C*
     C                   if        *in21  = '1' or  *in23 = '1'
     C                   movel     'CB05'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C***  Begin Add  ***  06/09/20  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*MASK'       qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'd1ss#'       qqname           25
     C     d1ss#         parm      d1ss#         qqval           100
     C***  End Add  ***  06/09/20  *******************************
     C                   movel     d1ss#         scdata
     C***  Begin Add  ***  06/09/20  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*UNMASK'     qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'd1ss#'       qqname           25
     C     d1ss#         parm      d1ss#         qqval           100
     C***  End Add  ***  06/09/20  *******************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*    Do not print now
     C***************    except    prtdata
     C***************
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C**************     except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C                   endif
     C*
     C** Injured party LSN
     C*** can not be all blank and
     C*** must contain 2 consecutive chars, contain an apostrophe '/spaces but n
     C                   move      *blanks       svlsn
     C                   if        d1lsn <> ' '
     C*
     C                   eval      svlsn = d1lsn
     C     hi:lo         xlate     d1lsn         d1lsn
     C     valchrl       check     d1lsn                                  22
     C*
     C                   if        *in22 = *off
     C*
     C                   eval      i = 1
     C*
     C                   dow       i <> 40
     C                   eval      xxlsn = %subst(d1lsn:i:2)
     C     valalph       check     xxlsn                                  23
     C                   if        *in23 = *off
     C                   leave
     C                   endif
     C                   eval      i = i + 1
     C                   enddo
     C*
     C                   endif
     C*
     C                   endif
     C*
     C                   if        d1lsn = ' '
     C                             or *in22 = *on or *in23 = *on
     C                   movel     'CB07'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     svlsn         scdata
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*    Do not print now
     C***************    except    prtdata
     C***************
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C                   endif
     C*
     C** Injured party FSN
     C*** must contain either alphabets or spaces but not all
     C                   move      *blanks       svfsn
     C                   if        d1fsn <> ' '
     C*
     C                   eval      svfsn = d1fsn
     C     hi:lo         xlate     d1fsn         d1fsn
     C     valchrf       check     d1fsn                                  24
     C*
     C                   endif
     C*
     C                   if        d1fsn = ' ' or *in24 = *on
     C                   movel     'CB08'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     svfsn         scdata
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C                   endif
     C** Injured party mid
     C*** not required, if non blank, then an alphabet is must.
     C                   if        d1mid <> ' '
     C*
     C     hi:lo         xlate     d1mid         d1mid
     C     valchrf       check     d1mid                                  25
     C                   if        *in25 = *on
     C                   movel     'CB09'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1mid         scdata
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C                   endif
     C*
     C                   endif
     C*
     C*** Beg Add *** 10/12/10 **********************************
     C** Injured party Sex
     C*** Required, must be either 0/1/2
     C*
     C                   if        d1sex = ' ' or
     C                             (d1sex <> '1' and d1sex <> '2'
     C                                  and d1sex <> '0')
     C                   movel     'CB10'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C*
     C                   Movel     d1sex         scdata
     C*
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C*
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*
     C                   endif
     C*
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C***************    except    space#
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C                   endif
     C*
     C*** End Add *** 10/12/10 **********************************
     C** Injured party DOB
     C*** Required, date must be valid and in CCYMMDD format and <= current date
     C                   if        d1dtob <> 0
     C*
     C                   Z-Add     0             srdti
     C*
     C                   Move      d1dtob        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   Movel     srdto         xxdt
     C*
     C                   endif
     C*
     C                   if        d1dtob = 0 or srerr = '1'
     C                             or xxdt   > xxdate
     C                   movel     'CB11'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C*
     C                   if        xxdt > xxdate
     C*
     C                   Z-Add     0             srdti
     C*
     C
     C                   Move      d1dtob        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         scdata
     C*
     C                   endif
     C*
     C                   if        srerr = '1'
     C                   Movel     d1dtob        scdata
     C                   endif
     C*
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C*
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C                   endif
     C*
     C*** Beg Add *** 05/18/10 **********************************
     C** Office Code/Site ID
     C*** Must be = ' ' or must contain 9-digit numeric code.
     C                   if        d1off# <> ' '
     C     valnum        check     d1off#                                 29
     C                   endif
     C*
     C                   if        d1off# <>  ' ' and *in29 = '1'
     C                   movel     'CP03'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1off#        scdata
     C*
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C*
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C                   endif
     C*
     C*** End Add *** 05/18/10 **********************************
     C** CMS Date of incident
     C*** Required, date must be valid and in CCYMMDD format and <= current date
     C                   if        d1cmdi <> 0
     C*
     C                   Z-Add     0             srdti
     C*
     C                   Move      d1cmdi        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxdt
     C*
     C                   endif
     C*
     C                   if        d1cmdi = 0 or srerr = '1'
     C                             or xxdt   > xxdate
     C                   movel     'CI01'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C*
     C                   if        xxdt > xxdate
     C*
     C                   Z-Add     0             srdti
     C*
     C                   move      d1cmdi        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         scdata
     C*
     C                   endif
     C*
     C                   if        srerr = '1'
     C                   Movel     d1cmdi        scdata
     C                   endif
     C*
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C*
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C                   endif
      /Free
        //*****Beg add *** 02/08/16 ********************************************
        // Check flag for Require the CMS Date of Incident is On, then
        // CMS Date of Incident to be on or after claims Date of Injury.

        If COrcdi = 'Y' and D1cmDi < D1acDt;
          Clear  scfld;
          clear  scdata;
          Clear  scdesc;

          scdata = %char(D1cmdi);
          scercd = '   ';

             scfld  =  'CMS DATE OF INCIDENT';
             scdesc(1) =  'CMS DOI TO BE GREATER OR EQUAL';
             scdesc(2) =  ' TO DATE OF INJURY.';
             scdesc(3) = ' ';
        //***** Write the log record.
         Exsr writemdp;
         rcd_found = *on;

        Endif;

        //*****End add *** 02/08/16 ********************************************
      /End-free
     C*
     C** Product liability indicator
     C*** Required, and it must be '1', '2' and '3'.
     C** Beg Chg *** 05/18/10 ***********************************
     C**********         if        d1prli = ' '
     C**********                   or (d1prli <> '1' and d1prli <> '2'
     C**********                    and d1prli <> '3')
     C** Beg Del *** 11/19/10 ***********************************
     C*******            if        d1prli <>  ' '
     C***End*Chg *** 05/18/10 ***********************************
     C*******            movel     'CI26'        scercd
     C*******            movel     *blanks       scfld
     C*******            movel     *blanks       scdata
     C*******            movel     *blanks       scdesc
     C*******            movel     d1prli        scdata
     C*****deky          chain     pde01                              91
     C*******            if        *in91 = *off
     C*******            movel     defld         scfld
     C*******            eval      scdesc(1) = %subst(dedesc:1:30)
     C****Beg Add *** 08/09/10 **********************************
     C*******            eval      j = 30
     C*******            if        %subst(scdesc(1):30:1) <> ' '
     C*******            eval      j = j-1
     C*******            dow       %subst(scdesc(1):j:1) <> ' '
     C*******            eval      j = j-1
     C*******            enddo
     C*******            endif
     C*******            eval      scdesc(1) = %subst(dedesc:1:j)
     C****End Add *** 08/09/10 **********************************
     C****Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C*******            eval      scdesc(2) = %subst(dedesc:j+1:30)
     C****End Chg *** 08/09/10 **********************************
     C****Beg Add *** 08/09/10 **********************************
     C*******            eval      k = 30
     C*******            if        %subst(scdesc(2):30:1) <> ' '
     C*******            eval      k = k-1
     C*******            dow       %subst(scdesc(2):k:1) <> ' '
     C*******            eval      k = k-1
     C*******            enddo
     C*******            endif
     C*******            eval      scdesc(2) = %subst(dedesc:j+1:k)
     C****End Add *** 08/09/10 **********************************
     C****Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C*******            eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C****End Chg *** 08/09/10 **********************************
     C*******            endif
     C*******            except    prtdata
     C*******            if        scdesc(2) <> ' '
     C*******            except    prtdata2
     C*******            if        scdesc(3) <> ' '
     C*******            except    prtdata3
     C*******            endif
     C*******            endif
     C****Beg Add *** 08/09/10 **********************************
     C*******            except    space#
     C****End Add *** 08/09/10 **********************************
     C*******
     C*******            eval      rcd_found = *on
     C*******            endif
     C*******
     C***Beg*Add *** 05/18/10 ***********************************
     C*******            if        d1prnm <>  ' '
     C*******            movel     'CI27'        scercd
     C*******            movel     *blanks       scfld
     C*******            movel     *blanks       scdata
     C*******            movel     *blanks       scdesc
     C*******            movel     d1prnm        scdata
     C*****deky          chain     pde01                              91
     C*******            if        *in91 = *off
     C*******            movel     defld         scfld
     C*******            eval      scdesc(1) = %subst(dedesc:1:30)
     C****Beg Add *** 08/09/10 **********************************
     C*******            eval      j = 30
     C*******            if        %subst(scdesc(1):30:1) <> ' '
     C*******            eval      j = j-1
     C*******            dow       %subst(scdesc(1):j:1) <> ' '
     C*******            eval      j = j-1
     C*******            enddo
     C*******            endif
     C*******            eval      scdesc(1) = %subst(dedesc:1:j)
     C****End Add *** 08/09/10 **********************************
     C****Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C*******            eval      scdesc(2) = %subst(dedesc:j+1:30)
     C****End Chg *** 08/09/10 **********************************
     C****Beg Add *** 08/09/10 **********************************
     C*******            eval      k = 30
     C*******            if        %subst(scdesc(2):30:1) <> ' '
     C*******            eval      k = k-1
     C*******            dow       %subst(scdesc(2):k:1) <> ' '
     C*******            eval      k = k-1
     C*******            enddo
     C*******            endif
     C*******            eval      scdesc(2) = %subst(dedesc:j+1:k)
     C****End Add *** 08/09/10 **********************************
     C****Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C*******            eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C****End Chg *** 08/09/10 **********************************
     C*******            endif
     C*******            except    prtdata
     C*******            if        scdesc(2) <> ' '
     C*******            except    prtdata2
     C*******            if        scdesc(3) <> ' '
     C*******            except    prtdata3
     C*******            endif
     C*******            endif
     C****Beg Add *** 08/09/10 **********************************
     C*******            except    space#
     C****End Add *** 08/09/10 **********************************
     C*******
     C*******            eval      rcd_found = *on
     C*******            endif
     C*******
     C*******            if        d1prbn <>  ' '
     C*******            movel     'CI28'        scercd
     C*******            movel     *blanks       scfld
     C*******            movel     *blanks       scdata
     C*******            movel     *blanks       scdesc
     C*******            movel     d1prbn        scdata
     C*****deky          chain     pde01                              91
     C*******            if        *in91 = *off
     C*******            movel     defld         scfld
     C*******            eval      scdesc(1) = %subst(dedesc:1:30)
     C****Beg Add *** 08/09/10 **********************************
     C*******            eval      j = 30
     C*******            if        %subst(scdesc(1):30:1) <> ' '
     C*******            eval      j = j-1
     C*******            dow       %subst(scdesc(1):j:1) <> ' '
     C*******            eval      j = j-1
     C*******            enddo
     C*******            endif
     C*******            eval      scdesc(1) = %subst(dedesc:1:j)
     C****End Add *** 08/09/10 **********************************
     C****Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C*******            eval      scdesc(2) = %subst(dedesc:j+1:30)
     C****End Chg *** 08/09/10 **********************************
     C****Beg Add *** 08/09/10 **********************************
     C*******            eval      k = 30
     C*******            if        %subst(scdesc(2):30:1) <> ' '
     C*******            eval      k = k-1
     C*******            dow       %subst(scdesc(2):k:1) <> ' '
     C*******            eval      k = k-1
     C*******            enddo
     C*******            endif
     C*******            eval      scdesc(2) = %subst(dedesc:j+1:k)
     C****End Add *** 08/09/10 **********************************
     C****Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C*******            eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C****End Chg *** 08/09/10 **********************************
     C*******            endif
     C*******            except    prtdata
     C*******            if        scdesc(2) <> ' '
     C*******            except    prtdata2
     C*******            if        scdesc(3) <> ' '
     C*******            except    prtdata3
     C*******            endif
     C*******            endif
     C****Beg Add *** 08/09/10 **********************************
     C*******            except    space#
     C****End Add *** 08/09/10 **********************************
     C*******
     C*******            eval      rcd_found = *on
     C*******            endif
     C*******
     C*******            if        d1prmn <>  ' '
     C*******            movel     'CI29'        scercd
     C*******            movel     *blanks       scfld
     C*******            movel     *blanks       scdata
     C*******            movel     *blanks       scdesc
     C*******            movel     d1prmn        scdata
     C*****deky          chain     pde01                              91
     C*******            if        *in91 = *off
     C*******            movel     defld         scfld
     C*******            eval      scdesc(1) = %subst(dedesc:1:30)
     C****Beg Add *** 08/09/10 **********************************
     C*******            eval      j = 30
     C*******            if        %subst(scdesc(1):30:1) <> ' '
     C*******            eval      j = j-1
     C*******            dow       %subst(scdesc(1):j:1) <> ' '
     C*******            eval      j = j-1
     C*******            enddo
     C*******            endif
     C*******            eval      scdesc(1) = %subst(dedesc:1:j)
     C****End Add *** 08/09/10 **********************************
     C****Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C*******            eval      scdesc(2) = %subst(dedesc:j+1:30)
     C****End Chg *** 08/09/10 **********************************
     C****Beg Add *** 08/09/10 **********************************
     C*******            eval      k = 30
     C*******            if        %subst(scdesc(2):30:1) <> ' '
     C*******            eval      k = k-1
     C*******            dow       %subst(scdesc(2):k:1) <> ' '
     C*******            eval      k = k-1
     C*******            enddo
     C*******            endif
     C*******            eval      scdesc(2) = %subst(dedesc:j+1:k)
     C****End Add *** 08/09/10 **********************************
     C****Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C*******            eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C****End Chg *** 08/09/10 **********************************
     C*******            endif
     C*******            except    prtdata
     C*******            if        scdesc(2) <> ' '
     C*******            except    prtdata2
     C*******            if        scdesc(3) <> ' '
     C*******            except    prtdata3
     C*******            endif
     C*******            endif
     C****Beg Add *** 08/09/10 **********************************
     C*******            except    space#
     C****End Add *** 08/09/10 **********************************
     C*******
     C*******            eval      rcd_found = *on
     C*******            endif
     C*******
     C*******            if        d1pahm <>  ' '
     C*******            movel     'CI30'        scercd
     C*******            movel     *blanks       scfld
     C*******            movel     *blanks       scdata
     C*******            movel     *blanks       scdesc
     C*******            movel     d1pahm        scdata
     C*****deky          chain     pde01                              91
     C*******            if        *in91 = *off
     C*******            movel     defld         scfld
     C*******            eval      scdesc(1) = %subst(dedesc:1:30)
     C****Beg Add *** 08/09/10 **********************************
     C*******            eval      j = 30
     C*******            if        %subst(scdesc(1):30:1) <> ' '
     C*******            eval      j = j-1
     C*******            dow       %subst(scdesc(1):j:1) <> ' '
     C*******            eval      j = j-1
     C*******            enddo
     C*******            endif
     C*******            eval      scdesc(1) = %subst(dedesc:1:j)
     C****End Add *** 08/09/10 **********************************
     C****Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C*******            eval      scdesc(2) = %subst(dedesc:j+1:30)
     C****End Chg *** 08/09/10 **********************************
     C****Beg Add *** 08/09/10 **********************************
     C*******            eval      k = 30
     C*******            if        %subst(scdesc(2):30:1) <> ' '
     C*******            eval      k = k-1
     C*******            dow       %subst(scdesc(2):k:1) <> ' '
     C*******            eval      k = k-1
     C*******            enddo
     C*******            endif
     C*******            eval      scdesc(2) = %subst(dedesc:j+1:k)
     C****End Add *** 08/09/10 **********************************
     C****Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C*******            eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C****End Chg *** 08/09/10 **********************************
     C*******            endif
     C*******            except    prtdata
     C*******            if        scdesc(2) <> ' '
     C*******            except    prtdata2
     C*******            if        scdesc(3) <> ' '
     C*******            except    prtdata3
     C*******            endif
     C*******            endif
     C****Beg Add *** 08/09/10 **********************************
     C*******            except    space#
     C****End Add *** 08/09/10 **********************************
     C*******
     C*******            eval      rcd_found = *on
     C*******            endif
     C** End Del *** 11/19/10 ***********************************
     C*
     C** Alleged Cause of Injury
     C*** beg add *** 10/20/15 ***************************************
     C                   move      *off          *in09
     C                   move      *off          *in10
     C                   move      *off          *in11
     C                   if        d1ain1 = ' ' and d1ainj = ' '
     C                   move      *on           *in11
     C                   endif
     C                   if        d1ain1 <> ' '
     C                   move      *on           *in10
     C                   else
     C*** end add *** 10/20/15 ***************************************
     C                   if        d1ainj <> ' '
     C     '.'           scan      d1ainj                                 27
     C                   move      *blanks       kkicd9
     C                   movel     d1ainj        kkicd9
     C     diky          chain     pdi05                              91
     C*** beg add *** 10/20/15 ***************************************
     C                   move      *on           *in09
     C*** end add *** 10/20/15 ***************************************
     C                   endif
     C*
     C*** beg add *** 10/20/15 ***************************************
     C                   endif
     C*** end add *** 10/20/15 ***************************************
     C*** beg chg *** 10/20/15 ***************************************
     C*****************  if        d1ainj = ' '
     C   09              if        d1ainj = ' '
     C*** end chg *** 10/20/15 ***************************************
     C                             or (d1ainj <> ' ' and
     C                             (%subst(d1ainj:1:1) <> 'E' or
     C                             dicms <> 'I' or *in27 = *on or *in91 = *on ))
     C*** beg add *** 10/20/15 ***************************************
     C                   move      *on           *in11
     C                   endif
     C                   if        *in11 = *on
     C*** end add *** 10/20/15 ***************************************
     C                   movel     'CI03'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C*** beg add *** 10/20/15 ***************************************
     C                   if        *in10 = *on
     C                   movel     d1ain1        scdata
     C                   else
     C*** end add *** 10/20/15 ***************************************
     C                   movel     d1ainj        scdata
     C*** beg add *** 10/20/15 ***************************************
     C                   endif
     C*** end add *** 10/20/15 ***************************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C                   endif
     C*
     C** End Add *** 05/18/10 ***********************************
     C** Self Insured Type
     C*** Required if self insured is 'Y' and valid values are 'I' or 'O'
     C                   if        (d1slfi = 'Y' and d1slft = ' ')
     C                              or (d1slft <>  'I' and d1slft <> 'O'
     C                              and d1slfi = 'Y')
     C                   movel     'CS02'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1slft        scdata
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C                   endif
     C*
     C** Legal name
     C*** Required if self insured type is 'O' and must be blanks if self insure
     C*** must contain at least 2 alpha chars. (to be done)
     C                   move      *blanks       svfdnm
     C                   if        d1fdnm <> ' '
     C*
     C                   eval      svfdnm = d1fdnm
     C     hi:lo         xlate     d1fdnm        d1fdnm
     C*
     C                   z-add     0             xxcnt             2 0
     C                   eval      i = 1
     C*
     C                   dow       i <> 70
     C                   eval      xxfdnm = %subst(d1fdnm:i:1)
     C     valalph       check     xxfdnm                                 29
     C                   if        *in29 = *off
     C                   eval      xxcnt = xxcnt + 1
     C                   if        xxcnt = 2
     C                   leave
     C                   endif
     C                   endif
     C                   eval      i = i + 1
     C                   enddo
     C*
     C                   endif
     C*
     C                   if        d1slft = 'I' and d1fdnm <> ' '
     C                             or (d1slft = 'O' and d1name = ' '
     C                             and ( d1fdnm =  ' ' or *in29 = '1'))
     C                   movel     'CS06'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     svfdnm        scdata
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C                   endif
     C*
     C** ORM indicator
     C*** Required and should be 'Y'/'N'
     C                   if        d1ormi = ' ' or (
     C                             d1ormi <> 'Y' and d1ormi <> 'N')
     C                   movel     'CJ01'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1ormi        scdata
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C                   endif
     C*
     C*** Beg Add *** 10/05/10 **********************************
     C** Settlement date > CMS date of incident
     C* Get CMS date of incident in format
     C                   Z-Add     0             srdti
     C*
     C                   Move      d1cmdi        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxdt
     C*
     C* Get settlement date  format
     C                   Z-Add     0             srdti
     C*
     C                   Move      d1t1sd        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxdtst
     C*
     C* if settlement < CMS DOI , then error
     C*
     C                   if        xxdtst < xxdt
     C*** Beg Add *** 11/01/10 **********************************
     C                             and xxdtst > 0
     C*** End Add *** 11/01/10 **********************************
     C                   movel     'CJ03'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   move      d1t1sd        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C*
     C                   if        srerr = '1'
     C                   Movel     d1t1sd        scdata
     C                   else
     C                   Movel     srdto         scdata
     C                   endif
     C*
     C*
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
     C*
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*
     C                   endif
     C*
     C                   eval      rcd_found = *on
     C*
     C                   endif
     C*
     C** TPOC date > CMS date of incident
     C* Get CMS date of incident in format
     C                   Z-Add     0             srdti
     C*
     C                   Move      d1cmdi        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxdt
     C*
     C* Get TPOC date in format
     C                   Z-Add     0             srdti
     C*
     C                   Move      d1t2sd        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxtpoc
     C*
     C* if TPOC date < CMS DOI , then error
     C*
     C                   if        xxtpoc < xxdt
     C*** Beg Add *** 11/01/10 **********************************
     C                             and xxtpoc > 0
     C*** End Add *** 11/01/10 **********************************
     C*
     C*** Beg Chg *** 11/01/10 **********************************
     C********           movel     'CJ03'        scercd
     C                   movel     'CT01'        scercd
     C*** End Chg *** 11/01/10 **********************************
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   move      d1t2sd        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C*
     C                   if        srerr = '1'
     C                   Movel     d1t2sd        scdata
     C                   else
     C                   Movel     srdto         scdata
     C                   endif
     C*
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
     C*
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C***************    except    space#
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C*
     C                   endif
     C*
     C*** End Add *** 10/05/10 **********************************
     C*** Beg Add *** 05/18/10 **********************************
     C** TPOC Date 1 is required if TPOC amount 1 <> 0 and TPOC Date = ' ' if TP
     C                   if        (d1t1am <> 0 and d1t1sd = 0)
     C                   movel     'CJ03'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   move      d1t1sd        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         scdata
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C                   endif
     C*
     C** TPOC Amt 1 is required if TPOC date 1 <> 0 and TPOC Amt 1 = ' ' if TPOC
     C                   if        (d1t1am = 0 and d1t1sd <> 0)
     C                   movel     'CJ04'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   move      d1t1am        scdata
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C                   endif
     C*
     C*** Beg Del *** 12/10/24 **********************************
     C***ORM termination date
     C***File submission date and ORM termination date gap should not be more th
     C****               if        d1ormd <> 0
     C****               Move      d1ormd        srdti
     C****               call      'SRCVTDAT'    dtmdyy
     C****               Movel     srdto         x1ormd
     C****
     C****               Move      d1ormd        srdti
     C****               call      'SRCVTDAT'    dtyymd
     C****               Movel     srdto         xxormd
     C**** *ISO          move      xxormd        xxdt1
     C****
     C****               Move      d1exdt        srdti
     C****               call      'SRCVTDAT'    dtyymd
     C****               Movel     srdto         xxexdt
     C**** *ISO          move      xxexdt        xxdt2
     C****
     C****               eval      xxmnth = %diff(xxdt1 :xxdt2 :*months)
     C****               endif
     C****               if        d1ormd <> 0 and xxmnth > 6
     C****               movel     'CJ06'        scercd
     C****               movel     *blanks       scfld
     C****               movel     *blanks       scdata
     C****               movel     *blanks       scdesc
     C****               move      x1ormd        scdata
     C**** deky          chain     pde01                              91
     C****               if        *in91 = *off
     C****               movel     defld         scfld
     C****               eval      scdesc(1) = %subst(dedesc:1:30)
     C****Beg Add *** 08/09/10 **********************************
     C****               eval      j = 30
     C****               if        %subst(scdesc(1):30:1) <> ' '
     C****               eval      j = j-1
     C****               dow       %subst(scdesc(1):j:1) <> ' '
     C****               eval      j = j-1
     C****               enddo
     C****               endif
     C****               eval      scdesc(1) = %subst(dedesc:1:j)
     C****End Add *** 08/09/10 **********************************
     C****Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C****               eval      scdesc(2) = %subst(dedesc:j+1:30)
     C****End Chg *** 08/09/10 **********************************
     C****Beg Add *** 08/09/10 **********************************
     C****               eval      k = 30
     C****               if        %subst(scdesc(2):30:1) <> ' '
     C****               eval      k = k-1
     C****               dow       %subst(scdesc(2):k:1) <> ' '
     C****               eval      k = k-1
     C****               enddo
     C****               endif
     C****               eval      scdesc(2) = %subst(dedesc:j+1:k)
     C****End Add *** 08/09/10 **********************************
     C****Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C****               eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C****End Chg *** 08/09/10 **********************************
     C****               endif
     C****Beg Chg *** 04/12/11 **********************************************
     C****
     C**** Do not print now
     C***************    except    prtdata
     C****
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C****Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C****End Add *** 08/09/10 **********************************
     C****
     C****Write the log record instead
     C****               exsr      writemdp
     C****
     C****End Chg *** 04/12/11 **********************************************
     C****
     C****               eval      rcd_found = *on
     C****               endif
     C****
     C*** End Del *** 12/10/24 **********************************
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
       // Must have TPOC Amount when ORM Indicator = N
       If D1t1Am = 0 and D1oRmI = 'N';
          scErCd = 'CJ07';
          scFld = *blanks;
          scData = *blanks;
          scDesc = *blanks;
          Chain DEky pde01;
          If %Found(WTDEL);
             scFld = DEfld;
             scDesc(1) = %Subst(DEdesc:1:30);
             j = 30;
             If %Subst(scDesc(1):30:1) <> ' ';
                j= j - 1;
                Dow %Subst(scDesc(1):j:1) <> ' ';
                   j = j - 1;
                Enddo;
             Endif;
             scDesc(1) = %Subst(DEdesc:1:j);
             scDesc(2) = %Subst(DEdesc:J+1:30);
             k = 30;
             If %Subst(scDesc(2):k:1) <> ' ';
                k = k - 1;
                Dow %Subst(scDesc(2):K:1) <> ' ';
                   k = k - 1;
                Enddo;
             Endif;
             scDesc(2) = %Subst(DEdesc:j+1:k);
             scDesc(3) = %Subst(DEdesc:j+k+1:30);
          Endif;
          Exsr writeMdp;
          rcd_Found = *on;
       Endif;
      /End-Free
       //*** End Add *** 05/21/20 **********************************************
      /Free
       //*****Beg add *** 02/08/16 *********************************************

        // Check ORM Termination Date to be on or after CMS Date of Incident.
        If D1orMi = 'Y' and D1orMd < D1cmDi and D1orMd <> *zeros ;

          clear  scfld;
          clear  scdata;
          clear  scdesc;

          scdata = %char(D1orMd);
          scercd = '  ';


             scfld  = 'ORM TERMINATION DATE';
             scdesc(1) = 'ORM TERMINATION DATE TO BE GRE';
             scdesc(2) = 'ATER OR EQUAL TO CMS DOI.';
             scdesc(3) = '  ';

        //***** Write the log record.
        Exsr writemdp;
        rcd_found = *on;

        EndIf;
       //*****End add *** 02/08/16 *********************************************
      /End-Free
     C*** Beg Add *** 02/07/11 *************************************************
     C** Rep. FEIN is required if clri <> ' '
     C                   if        d1clri <> ' ' and   d1plaa = 0
     C                   movel     'CR05'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C*** dont move anything into scdata
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C***************    except    space#
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C                   endif
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
                      // Representative Phone Number cannot be blank
                         If D1clRi <> ' ' and D1rPh# = 0;
                            scErCd = 'CR12';
                            scFld = *blanks;
                            scData = *blanks;
                            scDesc = *blanks;
                            Chain DEky pde01;
                            If %Found(WTDEL);
                               scFld = DEfld;
                               scDesc(1) = %Subst(DEdesc:1:30);
                               j = 30;
                               If %Subst(scDesc(1):30:1) <> ' ';
                                  j= j - 1;
                                  Dow %Subst(scDesc(1):j:1) <> ' ';
                                     j = j - 1;
                                  Enddo;
                               Endif;
                               scDesc(1) = %Subst(DEdesc:1:j);
                               scDesc(2) = %Subst(DEdesc:J+1:30);
                               k = 30;
                               If %Subst(scDesc(2):k:1) <> ' ';
                                  k = k - 1;
                                  Dow %Subst(scDesc(2):K:1) <> ' ';
                                     k = k - 1;
                                  Enddo;
                               Endif;
                               scDesc(2) = %Subst(DEdesc:j+1:k);
                               scDesc(3) = %Subst(DEdesc:j+k+1:30);
                            Endif;
                            Exsr writeMdp;
                            rcd_Found = *on;
                         Endif;
      /End-Free
       //*** End Add *** 05/21/20 **********************************************
     C*
     C*** End Add *** 02/07/11 *************************************************
     C** Rep. last name and first name -- OR -- Ref firm name is required if d1c
     C*** Beg Chg *** 01/05/11 *************************************************
     C******             if        d1clri <> ' ' and (((d1rpln = ' ' and
     C******                       d1rpfn = ' ')
     C******                         or (d1rpln = ' ' and d1rpfn <> ' ')
     C******                        or (d1rpln <> ' ' and d1rpfn = ' '))
     C*** Beg Del *** 02/17/11 *************************************************
     C******             if        d1clri <> ' ' and   (d1rpln = ' '
     C*** End Del *** 02/17/11 *************************************************
     C*** End Chg *** 01/05/11 *************************************************
     C*** Beg Add *** 02/17/11 *************************************************
     C                   if        d1clri <> ' ' and (((d1rpln = ' ' and
     C                             d1rpfn = ' ')
     C                               or (d1rpln = ' ' and d1rpfn <> ' ')
     C                              or (d1rpln <> ' ' and d1rpfn = ' '))
     C*** Beg Add *** 02/17/11 *************************************************
     C                             and d1rfnm = ' ')
     C                   movel     'CR14'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C*** dont move anything into scdata
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C***************    except    space#
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C                   endif
     C*
     C*** End Add *** 05/18/10 **********************************
     C*** Beg Del *** 05/18/10 **********************************
     C** Rep. last name
     C*** Required if injured party has a representative
     C****               if        d1clri <> ' ' and d1rpln = ' '
     C****               movel     'CR02'        scercd
     C****               movel     *blanks       scfld
     C****               movel     *blanks       scdata
     C****               movel     *blanks       scdesc
     C****               movel     d1rpln        scdata
     C**** deky          chain     pde01                              91
     C****               if        *in91 = *off
     C****               movel     defld         scfld
     C****               eval      scdesc(1) = %subst(dedesc:1:30)
     C****               eval      scdesc(2) = %subst(dedesc:31:30)
     C****               eval      scdesc(3) = %subst(dedesc:61:30)
     C****               endif
     C****               except    prtdata
     C****               if        scdesc(2) <> ' '
     C****               except    prtdata2
     C****               if        scdesc(3) <> ' '
     C****               except    prtdata3
     C****               endif
     C****               endif
     C*
     C****               eval      rcd_found = *on
     C****               endif
     C*
     C** Rep. first name
     C*** Required if injured party has a representative
     C****               if        d1clri <> ' ' and d1rpfn = ' '
     C****               movel     'CR03'        scercd
     C****               movel     *blanks       scfld
     C****               movel     *blanks       scdata
     C****               movel     *blanks       scdesc
     C****               movel     d1rpfn        scdata
     C**** deky          chain     pde01                              91
     C****               if        *in91 = *off
     C****               movel     defld         scfld
     C****               eval      scdesc(1) = %subst(dedesc:1:30)
     C****               eval      scdesc(2) = %subst(dedesc:31:30)
     C****               eval      scdesc(3) = %subst(dedesc:61:30)
     C****               endif
     C****               except    prtdata
     C****               if        scdesc(2) <> ' '
     C****               except    prtdata2
     C****               if        scdesc(3) <> ' '
     C****               except    prtdata3
     C****               endif
     C****               endif
     C*
     C****               eval      rcd_found = *on
     C****               endif
     C*
     C** Rep. firm name
     C*** Required if injured party has a representative
     C****               if        d1clri <> ' ' and d1rfnm = ' '
     C****               movel     'CR04'        scercd
     C****               movel     *blanks       scfld
     C****               movel     *blanks       scdata
     C****               movel     *blanks       scdesc
     C****               movel     d1rfnm        scdata
     C**** deky          chain     pde01                              91
     C****               if        *in91 = *off
     C****               movel     defld         scfld
     C****               eval      scdesc(1) = %subst(dedesc:1:30)
     C****               eval      scdesc(2) = %subst(dedesc:31:30)
     C****               eval      scdesc(3) = %subst(dedesc:61:30)
     C****               endif
     C****               except    prtdata
     C****               if        scdesc(2) <> ' '
     C****               except    prtdata2
     C****               if        scdesc(3) <> ' '
     C****               except    prtdata3
     C****               endif
     C****               endif
     C*
     C****               eval      rcd_found = *on
     C****               endif
     C*** End Del *** 05/18/10 **********************************
     C*
     C** Claimant 1 phone #
     C*** Required if claimant 1 relationship <> ' '
     C*
     C                   move      *blanks       xxc1ph
     C                   if        d1c1ph <>  0
     C                   move      d1c1ph        xxc1ph           10
     C     valnum        check     xxc1ph                                 39
     C                   endif
     C*
     C                   if        d1c1rl <> ' '
     C                             and (d1c1ph = 0 or *in39 = '1')
     C                             or (d1c1rl = ' ' and xxc1ph <> ' ')
     C                   movel     'CC12'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     xxc1ph        scdata
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C***************    except    space#
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C                   endif
     C*
     C** C1 Rep. last name
     C*** Required if claimant 1 has a representative
     C*** must begin with an alpha character + at least 2 consecutive alpha
     C                   move      *blanks       svc1rn
     C                   if        d1c1rn <> ' '
     C*
     C                   eval      svc1rn = d1c1rn
     C*
     C     hi:lo         xlate     d1c1rn        d1c1rn
     C                   eval      x1c1rn = %subst(d1c1rn:1:1)
     C     valalph       check     x1c1rn                                 30
     C*
     C                   if        *in30 = *off
     C*
     C                   eval      i = 1
     C*
     C                   dow       i <> 40
     C                   eval      xxc1rn = %subst(d1c1rn:i:2)
     C     valalph       check     xxc1rn                                 31
     C                   if        *in31 = *off
     C                   leave
     C                   endif
     C                   eval      i = i + 1
     C                   enddo
     C*
     C                   endif
     C*
     C                   endif
     C*
     C                   if        d1c1rl <> ' ' and d1c1rn = ' '
     C                             or (d1c1rl = ' ' and d1c1rn <> ' ')
     C                             or ( d1c1rl <> ' ' and *in30 = *on)
     C                              or (*in31 = *on and d1c1rl <> ' ')
     C                   movel     'CR22'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     svc1rn        scdata
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C***************    except    space#
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C                   endif
     C*
     C** C1 Rep. first name
     C*** Required if claimant 1 has a representative
     C*** must start with alpha char and must contain 2 consecutive alpha chars.
     C                   move      *blanks       svc1rf
     C                   if        d1c1rf <> ' '
     C*
     C                   eval      svc1rf = d1c1rf
     C     hi:lo         xlate     d1c1rf        d1c1rf
     C                   eval      x1c1rf = %subst(d1c1rf:1:1)
     C     valalph       check     x1c1rf                                 32
     C*
     C                   if        *in32 = *off
     C*
     C                   eval      i = 1
     C*
     C                   dow       i <> 30
     C                   eval      xxc1rf = %subst(d1c1rf:i:2)
     C     valalph       check     xxc1rf                                 33
     C                   if        *in33 = *off
     C                   leave
     C                   endif
     C                   eval      i = i + 1
     C                   enddo
     C*
     C                   endif
     C*
     C                   endif
     C*
     C                   if        d1c1rl <> ' ' and d1c1rf = ' '
     C                             or (d1c1rl = ' ' and d1c1rf <> ' ')
     C                             or ( d1c1rl <> ' ' and *in32 = *on)
     C                              or (*in33 = *on and d1c1rl <> ' ')
     C                   movel     'CR23'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     svc1rf        scdata
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C                   endif
     C*
     C*
     C** C1 Rep. firm name
     C*** Required if claimant 1 has a relationship
     C*** must start with alpha char and must contain 2 consecutive alpha chars.
     C*
     C****only edit if system date > 01/01/2011
     C*
     C                   move      *blanks       svc1rm
     C                   if        xxdate >= 20110101
     C*
     C                   if        d1c1rm <>  ' '
     C*
     C                   eval      svc1rm = d1c1rm
     C     hi:lo         xlate     d1c1rm        d1c1rm
     C                   eval      x1c1rm = %subst(d1c1rm:1:1)
     C     valalph       check     x1c1rm                                 34
     C*
     C                   if        *in34 = *off
     C*
     C                   eval      i = 1
     C*
     C                   dow       i <> 70
     C                   eval      xxc1rm = %subst(d1c1rm:i:2)
     C     valalph       check     xxc1rm                                 35
     C                   if        *in35 = *off
     C                   leave
     C                   endif
     C                   eval      i = i + 1
     C                   enddo
     C*
     C                   endif
     C*
     C                   endif
     C*
     C                   if        d1c1rl <> ' ' and d1c1rm = ' '
     C                             or (d1c1rl = ' ' and d1c1rm <> ' ')
     C                             or ( d1c1rl <> ' ' and *in34 = *on)
     C                              or (*in35 = *on and d1c1rl <> ' ')
     C                   movel     'CR24'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     svc1rm        scdata
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C                   endif
     C*
     C                   endif
     C*
     C*** ICD - edit
     C*
     C*** beg add *** 10/20/15 ***************************************
     C                   move      *off          *in09
     C                   move      *off          *in10
     C                   move      *off          *in11
     C                   if        d1I101 = ' ' and d1I901 = ' '
     C                   move      *on           *in11
     C                   endif
     C                   if        d1i101 <> ' '
     C                   move      *on           *in10
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
       // ICD-10 must be valid for CMS reporting
                         kkIcd10 = %Trim(D1i101);
                         Chain (kkIcd10) pic01;
                         If not %Found(WMICD10L);
                            *in11 = *on;
                         Endif;
      /End-free
     C*** end add *** 05/21/20 ***************************************
     C                   if        D1I901 <>  ' '
     C*** Beg Add *** 05/18/10 **********************************
     C     '.'           scan      d1i901                                 28
     C                   move      *blanks       kkicd9
     C                   movel     d1i901        kkicd9
     C     diky          chain     pdi05                              91
     C*** beg add *** 10/20/15 ***************************************
     C                   move      *on           *in09
     C*** end add *** 10/20/15 ***************************************
     C                   endif
     C*** beg add *** 10/20/15 ***************************************
     C                   endif
     C*** end add *** 10/20/15 ***************************************
     C*
     C*** beg chg *** 10/20/15 ***************************************
     C*****************  if        D1I901 = ' '
     C   09              if        D1I901 = ' '
     C*** end chg *** 10/20/15 ***************************************
     C                             or (D1I901 <>  ' ' and (%subst(d1i901:1:1)=
     C                             'E' or %subst(d1i901:1:1)='V' or *in28 = '1'
     C*** Beg Chg *** 01/21/11 **********************************
     C******                       or dicms <>'I' or *in91='1'))
     C                             or (dicms <>'I' and dicms <> ' ')
     C                                or *in91='1'))
     C*** beg add *** 10/20/15 ***************************************
     C                   move      *on           *in11
     C                   endif
     C                   if        *in11 = *on
     C*** end add *** 10/20/15 ***************************************
     C*** End Chg *** 01/21/11 **********************************
     C*** End Add *** 05/18/10 **********************************
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C**** diky          chain     pdi01                              92
     C****               if        *in92 = *on
     C*** End Del *** 05/18/10 **********************************
     C                   movel     'CI05'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C*** beg add *** 10/20/15 ***************************************
     C                   if        *in10 = *on
     C                   movel     d1I101        scdata
     C                   else
     C*** end add *** 10/20/15 ***************************************
     C                   movel     d1I901        scdata
     C*** beg add *** 10/20/15 ***************************************
     C                   endif
     C*** end add *** 10/20/15 ***************************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C********           endif
     C*** End Del *** 05/18/10 **********************************
     C*
     C                   endif
     C*
     C                   if        D1I902 <>  ' '
     C*** Beg Add *** 05/18/10 **********************************
     C                   eval      *in28 = *off
     C                   move      *blanks       kkicd9
     C                   movel     d1i902        kkicd9
     C     '.'           scan      d1i902                                 28
     C     diky          chain     pdi05                              91
     C                   endif
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
       // ICD-10 must be valid for CMS reporting
                         If D1i102 <> *blanks;
                            kkIcd10 = %Trim(D1i102);
                            Chain (kkIcd10) pic01;
                            If not %Found(WMICD10L);
                               *in11 = *on;
                            Else;
                               *in11 = *off;
                            Endif;
                         Endif;
      /End-free
     C*** end add *** 05/21/20 ***************************************
     C*
     C                   if
     C                             (D1I902 <>  ' ' and (%subst(d1i902:1:1)=
     C                             'E' or %subst(d1i902:1:1)='V' or *in28 = '1'
     C*** Beg Chg *** 01/21/11 **********************************
     C******                       or dicms   <>'I' or *in91='1' ))
     C                             or (dicms <>'I' and dicms <> ' ')
     C                                or *in91='1'))
     C*** End Chg *** 01/21/11 **********************************
     C*** Beg Add *** 05/21/20 **********************************************
     C                             or *in11 = *on
     C*** End Add *** 05/21/20 **********************************************
     C*** End Add *** 05/18/10 **********************************
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C**** diky          chain     pdi01                              92
     C****               if        *in92 = *on
     C*** End Del *** 05/18/10 **********************************
     C                   movel     'CI06'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1I902        scdata
     C*** Beg Add *** 05/21/20 ******************************************************
      /Free
                         If *in11 = *on;
                            scData = %Trim(d1I102);
                         Endif;
      /End-free
     C*** End Add *** 05/21/20 ******************************************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C********           endif
     C*** End Del *** 05/18/10 **********************************
     C*
     C                   endif
     C*
     C                   if        D1I903 <>  ' '
     C*
     C*** Beg Add *** 05/18/10 **********************************
     C                   eval      *in28 = *off
     C                   move      *blanks       kkicd9
     C                   movel     d1i903        kkicd9
     C     '.'           scan      d1i903                                 28
     C     diky          chain     pdi05                              91
     C                   endif
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
       // ICD-10 must be valid for CMS reporting
                         If D1i103 <> *blanks;
                            kkIcd10 = %Trim(D1i103);
                            Chain (kkIcd10) pic01;
                            If not %Found(WMICD10L);
                               *in11 = *on;
                            Else;
                               *in11 = *off;
                            Endif;
                         Endif;
      /End-free
     C*** end add *** 05/21/20 ***************************************
     C*
     C                   if
     C                             (D1I903 <>  ' ' and (%subst(d1i903:1:1)=
     C                             'E' or %subst(d1i903:1:1)='V' or *in28 = '1'
     C*** Beg Chg *** 01/21/11 **********************************
     C*******                      or dicms <>'I' or *in91='1'))
     C                             or (dicms <>'I' and dicms <> ' ')
     C                                or *in91='1'))
     C*** End Chg *** 01/21/11 **********************************
     C*** Beg Add *** 05/21/20 **********************************************
     C                             or *in11 = *on
     C*** End Add *** 05/21/20 **********************************************
     C*** End Add *** 05/18/10 **********************************
     C*** Beg Del *** 05/18/10 **********************************
     C**** diky          chain     pdi01                              92
     C****               if        *in92 = *on
     C*** End Del *** 05/18/10 **********************************
     C                   movel     'CI07'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1I903        scdata
     C*** Beg Add *** 05/21/20 ******************************************************
      /Free
                         If *in11 = *on;
                            scData = %Trim(d1I103);
                         Endif;
      /End-free
     C*** End Add *** 05/21/20 ******************************************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C*****              endif
     C*** End Del *** 05/18/10 **********************************
     C*
     C                   endif
     C*
     C                   if        D1I904 <>  ' '
     C*** Beg Add *** 05/18/10 **********************************
     C                   move      *blanks       kkicd9
     C                   movel     d1i904        kkicd9
     C                   eval      *in28 = *off
     C     '.'           scan      d1i904                                 28
     C     diky          chain     pdi05                              91
     C                   endif
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
       // ICD-10 must be valid for CMS reporting
                         If D1i104 <> *blanks;
                            kkIcd10 = %Trim(D1i104);
                            Chain (kkIcd10) pic01;
                            If not %Found(WMICD10L);
                               *in11 = *on;
                            Else;
                               *in11 = *off;
                            Endif;
                         Endif;
      /End-free
     C*** end add *** 05/21/20 ***************************************
     C*
     C                   if
     C                             (D1I904 <>  ' ' and (%subst(d1i904:1:1)=
     C                             'E' or %subst(d1i904:1:1)='V' or *in28 = '1'
     C*** Beg Chg *** 01/21/11 **********************************
     C*******                      or dicms <>'I' or *in91='1'))
     C                             or (dicms <>'I' and dicms <> ' ')
     C                                or *in91='1'))
     C*** End Chg *** 01/21/11 **********************************
     C*** Beg Add *** 05/21/20 **********************************************
     C                             or *in11 = *on
     C*** End Add *** 05/21/20 **********************************************


     C*** End Add *** 05/18/10 **********************************
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C**** diky          chain     pdi01                              92
     C****               if        *in92 = *on
     C*** End Del *** 05/18/10 **********************************
     C                   movel     'CI08'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1I904        scdata
     C*** Beg Add *** 05/21/20 ******************************************************
      /Free
                         If *in11 = *on;
                            scData = %Trim(d1I104);
                         Endif;
      /End-free
     C*** End Add *** 05/21/20 ******************************************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C****               endif
     C*** End Del *** 05/18/10 **********************************
     C*
     C                   endif
     C*
     C                   if        D1I905 <>  ' '
     C*
     C*** Beg Add *** 05/18/10 **********************************
     C                   move      *blanks       kkicd9
     C                   movel     d1i905        kkicd9
     C                   eval      *in28 = *off
     C     '.'           scan      d1i905                                 28
     C     diky          chain     pdi05                              91
     C                   endif
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
       // ICD-10 must be valid for CMS reporting
                         If D1i105 <> *blanks;
                            kkIcd10 = %Trim(D1i105);
                            Chain (kkIcd10) pic01;
                            If not %Found(WMICD10L);
                               *in11 = *on;
                            Else;
                               *in11 = *off;
                            Endif;
                         Endif;
      /End-free
     C*** end add *** 05/21/20 ***************************************
     C*
     C                   if
     C                             (D1I905 <>  ' ' and (%subst(d1i905:1:1)=
     C                             'E' or %subst(d1i905:1:1)='V' or *in28 = '1'
     C*** Beg Chg *** 01/21/11 **********************************
     C*******                      or dicms <>'I' or *in91='1' ))
     C                             or (dicms <>'I' and dicms <> ' ')
     C                                or *in91='1'))
     C*** End Chg *** 01/21/11 **********************************
     C*** Beg Add *** 05/21/20 **********************************************
     C                             or *in11 = *on
     C*** End Add *** 05/21/20 **********************************************
     C*** End Add *** 05/18/10 **********************************
     C*** Beg Del *** 05/18/10 **********************************
     C**** diky          chain     pdi01                              92
     C****               if        *in92 = *on
     C*** End Del *** 05/18/10 **********************************
     C                   movel     'CI09'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1I905        scdata
     C*** Beg Add *** 05/21/20 ******************************************************
      /Free
                         If *in11 = *on;
                            scData = %Trim(d1I105);
                         Endif;
      /End-free
     C*** End Add *** 05/21/20 ******************************************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C****               endif
     C*** End Del *** 05/18/10 **********************************
     C*
     C                   endif
     C*
     C                   if        D1I906 <>  ' '
     C*
     C*** Beg Add *** 05/18/10 **********************************
     C                   eval      *in28 = *off
     C                   move      *blanks       kkicd9
     C                   movel     d1i906        kkicd9
     C     '.'           scan      d1i906                                 28
     C     diky          chain     pdi05                              91
     C                   endif
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
       // ICD-10 must be valid for CMS reporting
                         If D1i106 <> *blanks;
                            kkIcd10 = %Trim(D1i106);
                            Chain (kkIcd10) pic01;
                            If not %Found(WMICD10L);
                               *in11 = *on;
                            Else;
                               *in11 = *off;
                            Endif;
                         Endif;
      /End-free
     C*** end add *** 05/21/20 ***************************************
     C*
     C                   if
     C                             (D1I906 <>  ' ' and (%subst(d1i906:1:1)=
     C                             'E' or %subst(d1i906:1:1)='V' or *in28 = '1'
     C*** Beg Chg *** 01/21/11 **********************************
     C*******                      or dicms <>'I' or *in91='1' ))
     C                             or (dicms <>'I' and dicms <> ' ')
     C                                or *in91='1'))
     C*** End Chg *** 01/21/11 **********************************
     C*** Beg Add *** 05/21/20 **********************************************
     C                             or *in11 = *on
     C*** End Add *** 05/21/20 **********************************************
     C*** End Add *** 05/18/10 **********************************
     C*** Beg Del *** 05/18/10 **********************************
     C**** diky          chain     pdi01                              92
     C****               if        *in92 = *on
     C*** End Del *** 05/18/10 **********************************
     C                   movel     'CI10'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1I906        scdata
     C*** Beg Add *** 05/21/20 ******************************************************
      /Free
                         If *in11 = *on;
                            scData = %Trim(d1I106);
                         Endif;
      /End-free
     C*** End Add *** 05/21/20 ******************************************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C****               endif
     C*** End Del *** 05/18/10 **********************************
     C*
     C                   endif
     C*
     C                   if        D1I907 <>  ' '
     C*
     C*** Beg Add *** 05/18/10 **********************************
     C                   move      *blanks       kkicd9
     C                   movel     d1i907        kkicd9
     C                   eval      *in28 = *off
     C     '.'           scan      d1i907                                 28
     C     diky          chain     pdi05                              91
     C                   endif
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
       // ICD-10 must be valid for CMS reporting
                         If D1i107 <> *blanks;
                            kkIcd10 = %Trim(D1i107);
                            Chain (kkIcd10) pic01;
                            If not %Found(WMICD10L);
                               *in11 = *on;
                            Else;
                               *in11 = *off;
                            Endif;
                         Endif;
      /End-free
     C*** end add *** 05/21/20 ***************************************
     C*
     C                   if
     C                             (D1I907 <>  ' ' and (%subst(d1i907:1:1)=
     C                             'E' or %subst(d1i907:1:1)='V' or *in28 = '1'
     C*** Beg Chg *** 01/21/11 **********************************
     C*******                      or dicms <>'I' or *in91='1' ))
     C                             or (dicms <>'I' and dicms <> ' ')
     C                                or *in91='1'))
     C*** End Chg *** 01/21/11 **********************************
     C*** Beg Add *** 05/21/20 **********************************************
     C                             or *in11 = *on
     C*** End Add *** 05/21/20 **********************************************
     C*** End Add *** 05/18/10 **********************************
     C*** Beg Del *** 05/18/10 **********************************
     C**** diky          chain     pdi01                              92
     C****               if        *in92 = *on
     C*** End Del *** 05/18/10 **********************************
     C                   movel     'CI11'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1I907        scdata
     C*** Beg Add *** 05/21/20 ******************************************************
      /Free
                         If *in11 = *on;
                            scData = %Trim(d1I107);
                         Endif;
      /End-free
     C*** End Add *** 05/21/20 ******************************************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C****               endif
     C*** End Del *** 05/18/10 **********************************
     C*
     C                   endif
     C*
     C                   if        D1I908 <>  ' '
     C*
     C*** Beg Add *** 05/18/10 **********************************
     C                   move      *blanks       kkicd9
     C                   movel     d1i908        kkicd9
     C                   eval      *in28 = *off
     C     '.'           scan      d1i908                                 28
     C     diky          chain     pdi05                              91
     C                   endif
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
       // ICD-10 must be valid for CMS reporting
                         If D1i108 <> *blanks;
                            kkIcd10 = %Trim(D1i108);
                            Chain (kkIcd10) pic01;
                            If not %Found(WMICD10L);
                               *in11 = *on;
                            Else;
                               *in11 = *off;
                            Endif;
                         Endif;
      /End-free
     C*** end add *** 05/21/20 ***************************************
     C*
     C                   if
     C                             (D1I908 <>  ' ' and (%subst(d1i908:1:1)=
     C                             'E' or %subst(d1i908:1:1)='V' or *in28 = '1'
     C*** Beg Chg *** 01/21/11 **********************************
     C*******                      or dicms <>'I' or *in91='1' ))
     C                             or (dicms <>'I' and dicms <> ' ')
     C                                or *in91='1'))
     C*** End Chg *** 01/21/11 **********************************
     C*** Beg Add *** 05/21/20 **********************************************
     C                             or *in11 = *on
     C*** End Add *** 05/21/20 **********************************************
     C*** End Add *** 05/18/10 **********************************
     C*** Beg Del *** 05/18/10 **********************************
     C**** diky          chain     pdi01                              92
     C****               if        *in92 = *on
     C*** End Del *** 05/18/10 **********************************
     C                   movel     'CI12'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1I908        scdata
     C*** Beg Add *** 05/21/20 ******************************************************
      /Free
                         If *in11 = *on;
                            scData = %Trim(d1I108);
                         Endif;
      /End-free
     C*** End Add *** 05/21/20 ******************************************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C****               endif
     C*** End Del *** 05/18/10 **********************************
     C*
     C                   endif
     C*
     C                   if        D1I909 <>  ' '
     C*
     C*** Beg Add *** 05/18/10 **********************************
     C                   move      *blanks       kkicd9
     C                   movel     d1i909        kkicd9
     C                   eval      *in28 = *off
     C     '.'           scan      d1i909                                 28
     C     diky          chain     pdi05                              91
     C                   endif
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
       // ICD-10 must be valid for CMS reporting
                         If D1i109 <> *blanks;
                            kkIcd10 = %Trim(D1i109);
                            Chain (kkIcd10) pic01;
                            If not %Found(WMICD10L);
                               *in11 = *on;
                            Else;
                               *in11 = *off;
                            Endif;
                         Endif;
      /End-free
     C*** end add *** 05/21/20 ***************************************
     C*
     C                   if
     C                             (D1I909 <>  ' ' and (%subst(d1i909:1:1)=
     C                             'E' or %subst(d1i909:1:1)='V' or *in28 = '1'
     C*** Beg Chg *** 01/21/11 **********************************
     C*******                      or dicms <>'I' or *in91='1' ))
     C                             or (dicms <>'I' and dicms <> ' ')
     C                                or *in91='1'))
     C*** End Chg *** 01/21/11 **********************************
     C*** Beg Add *** 05/21/20 **********************************************
     C                             or *in11 = *on
     C*** End Add *** 05/21/20 **********************************************
     C*** End Add *** 05/18/10 **********************************
     C*** Beg Del *** 05/18/10 **********************************
     C**** diky          chain     pdi01                              92
     C****               if        *in92 = *on
     C*** End Del *** 05/18/10 **********************************
     C                   movel     'CI13'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1I909        scdata
     C*** Beg Add *** 05/21/20 ******************************************************
      /Free
                         If *in11 = *on;
                            scData = %Trim(d1I109);
                         Endif;
      /End-free
     C*** End Add *** 05/21/20 ******************************************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C******             endif
     C*** End Del *** 05/18/10 **********************************
     C*
     C                   endif
     C*
     C                   if        D1I910 <>  ' '
     C*
     C*** Beg Add *** 05/18/10 **********************************
     C                   eval      *in28 = *off
     C                   move      *blanks       kkicd9
     C                   movel     d1i910        kkicd9
     C     '.'           scan      d1i910                                 28
     C     diky          chain     pdi05                              91
     C                   endif
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
       // ICD-10 must be valid for CMS reporting
                         If D1i110 <> *blanks;
                            kkIcd10 = %Trim(D1i110);
                            Chain (kkIcd10) pic01;
                            If not %Found(WMICD10L);
                               *in11 = *on;
                            Else;
                               *in11 = *off;
                            Endif;
                         Endif;
      /End-free
     C*** end add *** 05/21/20 ***************************************
     C*
     C                   if
     C                             (D1I910 <>  ' ' and (%subst(d1i910:1:1)=
     C                             'E' or %subst(d1i910:1:1)='V' or *in28 = '1'
     C*** Beg Chg *** 01/21/11 **********************************
     C*******                      or dicms  <>'I' or *in91='1' ))
     C                             or (dicms <>'I' and dicms <> ' ')
     C                                or *in91='1'))
     C*** End Chg *** 01/21/11 **********************************
     C*** Beg Add *** 05/21/20 **********************************************
     C                             or *in11 = *on
     C*** End Add *** 05/21/20 **********************************************
     C*** End Add *** 05/18/10 **********************************
     C*** End Del *** 05/18/10 **********************************
     C**** diky          chain     pdi01                              92
     C****               if        *in92 = *on
     C*** End Del *** 05/18/10 **********************************
     C                   movel     'CI14'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1I910        scdata
     C*** Beg Add *** 05/21/20 ******************************************************
      /Free
                         If *in11 = *on;
                            scData = %Trim(d1I110);
                         Endif;
      /End-free
     C*** End Add *** 05/21/20 ******************************************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C****               endif
     C*** End Del *** 05/18/10 **********************************
     C*
     C                   endif
     C*
     C                   if        D1I911 <>  ' '
     C*
     C*** Beg Add *** 05/18/10 **********************************
     C                   eval      *in28 = *off
     C                   move      *blanks       kkicd9
     C                   movel     d1i911        kkicd9
     C     '.'           scan      d1i911                                 28
     C     diky          chain     pdi05                              91
     C                   endif
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
       // ICD-10 must be valid for CMS reporting
                         If D1i111 <> *blanks;
                            kkIcd10 = %Trim(D1i111);
                            Chain (kkIcd10) pic01;
                            If not %Found(WMICD10L);
                               *in11 = *on;
                            Else;
                               *in11 = *off;
                            Endif;
                         Endif;
      /End-free
     C*** end add *** 05/21/20 ***************************************
     C*
     C                   if
     C                             (D1I911 <>  ' ' and (%subst(d1i911:1:1)=
     C                             'E' or %subst(d1i911:1:1)='V' or *in28 = '1'
     C*** Beg Chg *** 01/21/11 **********************************
     C********                     or dicms <>'I' or *in91='1' ))
     C                             or (dicms <>'I' and dicms <> ' ')
     C                                or *in91='1'))
     C*** End Chg *** 01/21/11 **********************************
     C*** Beg Add *** 05/21/20 **********************************************
     C                             or *in11 = *on
     C*** End Add *** 05/21/20 **********************************************
     C*** End Add *** 05/18/10 **********************************
     C*** Beg Del *** 05/18/10 **********************************
     C**** diky          chain     pdi01                              92
     C****               if        *in92 = *on
     C*** End Del *** 05/18/10 **********************************
     C                   movel     'CI15'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1I911        scdata
     C*** Beg Add *** 05/21/20 ******************************************************
      /Free
                         If *in11 = *on;
                            scData = %Trim(d1I111);
                         Endif;
      /End-free
     C*** End Add *** 05/21/20 ******************************************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C****               endif
     C*** End Del *** 05/18/10 **********************************
     C*
     C                   endif
     C*
     C                   if        D1I912 <>  ' '
     C*
     C*** Beg Add *** 05/18/10 **********************************
     C                   eval      *in28 = *off
     C     '.'           scan      d1i912                                 28
     C                   move      *blanks       kkicd9
     C                   movel     d1i912        kkicd9
     C     diky          chain     pdi05                              91
     C                   endif
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
       // ICD-10 must be valid for CMS reporting
                         If D1i112 <> *blanks;
                            kkIcd10 = %Trim(D1i112);
                            Chain (kkIcd10) pic01;
                            If not %Found(WMICD10L);
                               *in11 = *on;
                            Else;
                               *in11 = *off;
                            Endif;
                         Endif;
      /End-free
     C*** end add *** 05/21/20 ***************************************
     C*
     C                   if
     C                             (D1I912 <>  ' ' and (%subst(d1i912:1:1)=
     C                             'E' or %subst(d1i912:1:1)='V' or *in28 = '1'
     C*** Beg Chg *** 01/21/11 **********************************
     C********                     or dicms <>'I' or *in91='1'))
     C                             or (dicms <>'I' and dicms <> ' ')
     C                                or *in91='1'))
     C*** End Chg *** 01/21/11 **********************************
     C*** Beg Add *** 05/21/20 **********************************************
     C                             or *in11 = *on
     C*** End Add *** 05/21/20 **********************************************
     C*** End Add *** 05/18/10 **********************************
     C*** End Del *** 05/18/10 **********************************
     C**** diky          chain     pdi01                              92
     C****               if        *in92 = *on
     C*** End Del *** 05/18/10 **********************************
     C                   movel     'CI16'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1I912        scdata
     C*** Beg Add *** 05/21/20 ******************************************************
      /Free
                         If *in11 = *on;
                            scData = %Trim(d1I112);
                         Endif;
      /End-free
     C*** End Add *** 05/21/20 ******************************************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C****               endif
     C*** End Del *** 05/18/10 **********************************
     C*
     C                   endif
     C*
     C                   if        D1I913 <>  ' '
     C*
     C*** Beg Add *** 05/18/10 **********************************
     C                   eval      *in28 = *off
     C     '.'           scan      d1i913                                 28
     C                   move      *blanks       kkicd9
     C                   movel     d1i913        kkicd9
     C     diky          chain     pdi05                              91
     C                   endif
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
       // ICD-10 must be valid for CMS reporting
                         If D1i113 <> *blanks;
                            kkIcd10 = %Trim(D1i113);
                            Chain (kkIcd10) pic01;
                            If not %Found(WMICD10L);
                               *in11 = *on;
                            Else;
                               *in11 = *off;
                            Endif;
                         Endif;
      /End-free
     C*** end add *** 05/21/20 ***************************************
     C*
     C                   if
     C                             (D1I913 <>  ' ' and (%subst(d1i913:1:1)=
     C                             'E' or %subst(d1i913:1:1)='V' or *in28 = '1'
     C*** Beg Chg *** 01/21/11 **********************************
     C********                     or dicms <>'I' or *in91='1' ))
     C                             or (dicms <>'I' and dicms <> ' ')
     C                                or *in91='1'))
     C*** End Chg *** 01/21/11 **********************************
     C*** Beg Add *** 05/21/20 **********************************************
     C                             or *in11 = *on
     C*** End Add *** 05/21/20 **********************************************
     C*** End Add *** 05/18/10 **********************************
     C*** Beg Del *** 05/18/10 **********************************
     C**** diky          chain     pdi01                              92
     C****               if        *in92 = *on
     C*** End Del *** 05/18/10 **********************************
     C                   movel     'CI17'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1I913        scdata
     C*** Beg Add *** 05/21/20 ******************************************************
      /Free
                         If *in11 = *on;
                            scData = %Trim(d1I113);
                         Endif;
      /End-free
     C*** End Add *** 05/21/20 ******************************************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C****               endif
     C*** End Del *** 05/18/10 **********************************
     C*
     C                   endif
     C*
     C*
     C                   if        D1I914 <>  ' '
     C*
     C*** Beg Add *** 05/18/10 **********************************
     C                   eval      *in28 = *off
     C     '.'           scan      d1i914                                 28
     C                   move      *blanks       kkicd9
     C                   movel     d1i914        kkicd9
     C     diky          chain     pdi05                              91
     C                   endif
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
       // ICD-10 must be valid for CMS reporting
                         If D1i114 <> *blanks;
                            kkIcd10 = %Trim(D1i114);
                            Chain (kkIcd10) pic01;
                            If not %Found(WMICD10L);
                               *in11 = *on;
                            Else;
                               *in11 = *off;
                            Endif;
                         Endif;
      /End-free
     C*** end add *** 05/21/20 ***************************************
     C*
     C                   if
     C                             (D1I914 <>  ' ' and (%subst(d1i914:1:1)=
     C                             'E' or %subst(d1i914:1:1)='V' or *in28 = '1'
     C*** Beg Chg *** 01/21/11 **********************************
     C********                     or dicms  <>'I' or *in91='1' ))
     C                             or (dicms <>'I' and dicms <> ' ')
     C                                or *in91='1'))
     C*** End Chg *** 01/21/11 **********************************
     C*** Beg Add *** 05/21/20 **********************************************
     C                             or *in11 = *on
     C*** End Add *** 05/21/20 **********************************************
     C*** End Add *** 05/18/10 **********************************
     C*** Beg Del *** 05/18/10 **********************************
     C**** diky          chain     pdi01                              92
     C****               if        *in92 = *on
     C*** End Del *** 05/18/10 **********************************
     C                   movel     'CI18'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1I914        scdata
     C*** Beg Add *** 05/21/20 ******************************************************
      /Free
                         If *in11 = *on;
                            scData = %Trim(d1I114);
                         Endif;
      /End-free
     C*** End Add *** 05/21/20 ******************************************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C****               endif
     C*** End Del *** 05/18/10 **********************************
     C*
     C                   endif
     C*
     C*
     C                   if        D1I915 <>  ' '
     C*
     C*** Beg Add *** 05/18/10 **********************************
     C                   eval      *in28 = *off
     C     '.'           scan      d1i915                                 28
     C                   move      *blanks       kkicd9
     C                   movel     d1i915        kkicd9
     C     diky          chain     pdi05                              91
     C                   endif
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
       // ICD-10 must be valid for CMS reporting
                         If D1i115 <> *blanks;
                            kkIcd10 = %Trim(D1i115);
                            Chain (kkIcd10) pic01;
                            If not %Found(WMICD10L);
                               *in11 = *on;
                            Else;
                               *in11 = *off;
                            Endif;
                         Endif;
      /End-free
     C*** end add *** 05/21/20 ***************************************
     C*
     C                   if
     C                             (D1I915 <>  ' ' and (%subst(d1i915:1:1)=
     C                             'E' or %subst(d1i915:1:1)='V' or *in28 = '1'
     C*** Beg Chg *** 01/21/11 **********************************
     C********                     or dicms <>'I' or *in91='1'))
     C                             or (dicms <>'I' and dicms <> ' ')
     C                                or *in91='1'))
     C*** End Chg *** 01/21/11 **********************************
     C*** Beg Add *** 05/21/20 **********************************************
     C                             or *in11 = *on
     C*** End Add *** 05/21/20 **********************************************
     C*** End Add *** 05/18/10 **********************************
     C*** Beg Del *** 05/18/10 **********************************
     C**** diky          chain     pdi01                              92
     C****               if        *in92 = *on
     C*** End Del *** 05/18/10 **********************************
     C                   movel     'CI19'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1I915        scdata
     C*** Beg Add *** 05/21/20 ******************************************************
      /Free
                         If *in11 = *on;
                            scData = %Trim(d1I115);
                         Endif;
      /End-free
     C*** End Add *** 05/21/20 ******************************************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C****               endif
     C*** End Del *** 05/18/10 **********************************
     C*
     C                   endif
     C*
     C*
     C                   if        D1I916 <>  ' '
     C*
     C*** Beg Add *** 05/18/10 **********************************
     C                   eval      *in28 = *off
     C     '.'           scan      d1i916                                 28
     C                   move      *blanks       kkicd9
     C                   movel     d1i916        kkicd9
     C     diky          chain     pdi05                              91
     C                   endif
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
       // ICD-10 must be valid for CMS reporting
                         If D1i116 <> *blanks;
                            kkIcd10 = %Trim(D1i116);
                            Chain (kkIcd10) pic01;
                            If not %Found(WMICD10L);
                               *in11 = *on;
                            Else;
                               *in11 = *off;
                            Endif;
                         Endif;
      /End-free
     C*** end add *** 05/21/20 ***************************************
     C*
     C                   if
     C                             (D1I916 <>  ' ' and (%subst(d1i916:1:1)=
     C                             'E' or %subst(d1i916:1:1)='V' or *in28 = '1'
     C*** Beg Chg *** 01/21/11 **********************************
     C********                     or dicms <>'I' or *in91='1' ))
     C                             or (dicms <>'I' and dicms <> ' ')
     C                                or *in91='1'))
     C*** End Chg *** 01/21/11 **********************************
     C*** Beg Add *** 05/21/20 **********************************************
     C                             or *in11 = *on
     C*** End Add *** 05/21/20 **********************************************
     C*** End Add *** 05/18/10 **********************************
     C*** Beg Del *** 05/18/10 **********************************
     C**** diky          chain     pdi01                              92
     C****               if        *in92 = *on
     C*** End Del *** 05/18/10 **********************************
     C                   movel     'CI20'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1I916        scdata
     C*** Beg Add *** 05/21/20 ******************************************************
      /Free
                         If *in11 = *on;
                            scData = %Trim(d1I116);
                         Endif;
      /End-free
     C*** End Add *** 05/21/20 ******************************************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C****               endif
     C*** End Del *** 05/18/10 **********************************
     C*
     C                   endif
     C*
     C                   if        D1I917 <>  ' '
     C*
     C*** Beg Add *** 05/18/10 **********************************
     C                   eval      *in28 = *off
     C     '.'           scan      d1i917                                 28
     C                   move      *blanks       kkicd9
     C                   movel     d1i917        kkicd9
     C     diky          chain     pdi05                              91
     C                   endif
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
       // ICD-10 must be valid for CMS reporting
                         If D1i117 <> *blanks;
                            kkIcd10 = %Trim(D1i117);
                            Chain (kkIcd10) pic01;
                            If not %Found(WMICD10L);
                               *in11 = *on;
                            Else;
                               *in11 = *off;
                            Endif;
                         Endif;
      /End-free
     C*** end add *** 05/21/20 ***************************************
     C*
     C                   if
     C                             (D1I917 <>  ' ' and (%subst(d1i917:1:1)=
     C                             'E' or %subst(d1i917:1:1)='V' or *in28 = '1'
     C*** Beg Chg *** 01/21/11 **********************************
     C*********                    or dicms  <>'I' or *in91='1'))
     C                             or (dicms <>'I' and dicms <> ' ')
     C                                or *in91='1'))
     C*** End Chg *** 01/21/11 **********************************
     C*** Beg Add *** 05/21/20 **********************************************
     C                             or *in11 = *on
     C*** End Add *** 05/21/20 **********************************************
     C*** End Add *** 05/18/10 **********************************
     C*** Beg Del *** 05/18/10 **********************************
     C**** diky          chain     pdi01                              92
     C****               if        *in92 = *on
     C*** End Del *** 05/18/10 **********************************
     C                   movel     'CI21'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1I917        scdata
     C*** Beg Add *** 05/21/20 ******************************************************
      /Free
                         If *in11 = *on;
                            scData = %Trim(d1I117);
                         Endif;
      /End-free
     C*** End Add *** 05/21/20 ******************************************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C*******            endif
     C*** End Del *** 05/18/10 **********************************
     C*
     C                   endif
     C*
     C*
     C                   if        D1I918 <>  ' '
     C*
     C*** Beg Add *** 05/18/10 **********************************
     C                   eval      *in28 = *off
     C     '.'           scan      d1i918                                 28
     C                   move      *blanks       kkicd9
     C                   movel     d1i918        kkicd9
     C     diky          chain     pdi05                              91
     C                   endif
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
       // ICD-10 must be valid for CMS reporting
                         If D1i118 <> *blanks;
                            kkIcd10 = %Trim(D1i118);
                            Chain (kkIcd10) pic01;
                            If not %Found(WMICD10L);
                               *in11 = *on;
                            Else;
                               *in11 = *off;
                            Endif;
                         Endif;
      /End-free
     C*** end add *** 05/21/20 ***************************************
     C*
     C                   if
     C                             (D1I918 <>  ' ' and (%subst(d1i918:1:1)=
     C                             'E' or %subst(d1i918:1:1)='V' or *in28 = '1'
     C*** Beg Chg *** 01/21/11 **********************************
     C*********                    or dicms  <>'I' or *in91='1'))
     C                             or (dicms <>'I' and dicms <> ' ')
     C                                or *in91='1'))
     C*** End Chg *** 01/21/11 **********************************
     C*** Beg Add *** 05/21/20 **********************************************
     C                             or *in11 = *on
     C*** End Add *** 05/21/20 **********************************************
     C*** End Add *** 05/18/10 **********************************
     C*** Beg Del *** 05/18/10 **********************************
     C**** diky          chain     pdi01                              92
     C****               if        *in92 = *on
     C*** End Del *** 05/18/10 **********************************
     C                   movel     'CI22'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1I918        scdata
     C*** Beg Add *** 05/21/20 ******************************************************
      /Free
                         If *in11 = *on;
                            scData = %Trim(d1I118);
                         Endif;
      /End-free
     C*** End Add *** 05/21/20 ******************************************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C****               endif
     C*** End Del *** 05/18/10 **********************************
     C*
     C                   endif
     C*
     C*
     C                   if        D1I919 <>  ' '
     C*
     C*** Beg Add *** 05/18/10 **********************************
     C                   eval      *in28 = *off
     C     '.'           scan      d1i919                                 28
     C                   move      *blanks       kkicd9
     C                   movel     d1i919        kkicd9
     C     diky          chain     pdi05                              91
     C                   endif
       //*** Beg Add *** 05/21/20 **********************************************
      /Free
       // ICD-10 must be valid for CMS reporting
                         If D1i119 <> *blanks;
                            kkIcd10 = %Trim(D1i119);
                            Chain (kkIcd10) pic01;
                            If not %Found(WMICD10L);
                               *in11 = *on;
                            Else;
                               *in11 = *off;
                            Endif;
                         Endif;
      /End-free
     C*** end add *** 05/21/20 ***************************************
     C*
     C                   if
     C                             (D1I919 <>  ' ' and (%subst(d1i919:1:1)=
     C                             'E' or %subst(d1i919:1:1)='V' or *in28 = '1'
     C*** Beg Chg *** 01/21/11 **********************************
     C*********                    or dicms  <>'I' or *in91='1'))
     C                             or (dicms <>'I' and dicms <> ' ')
     C                                or *in91='1'))
     C*** End Chg *** 01/21/11 **********************************
     C*** Beg Add *** 05/21/20 **********************************************
     C                             or *in11 = *on
     C*** End Add *** 05/21/20 **********************************************
     C*** End Add *** 05/18/10 **********************************
     C*** Beg Del *** 05/18/10 **********************************
     C**** diky          chain     pdi01                              92
     C****               if        *in92 = *on
     C*** End Del *** 05/18/10 **********************************
     C                   movel     'CI23'        scercd
     C                   movel     *blanks       scfld
     C                   movel     *blanks       scdata
     C                   movel     *blanks       scdesc
     C                   movel     d1I919        scdata
     C*** Beg Add *** 05/21/20 ******************************************************
      /Free
                         If *in11 = *on;
                            scData = %Trim(d1I119);
                         Endif;
      /End-free
     C*** End Add *** 05/21/20 ******************************************************
     C     deky          chain     pde01                              91
     C                   if        *in91 = *off
     C                   movel     defld         scfld
     C                   eval      scdesc(1) = %subst(dedesc:1:30)
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      j = 30
     C                   if        %subst(scdesc(1):30:1) <> ' '
     C                   eval      j = j-1
     C                   dow       %subst(scdesc(1):j:1) <> ' '
     C                   eval      j = j-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(1) = %subst(dedesc:1:j)
      *** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(2) = %subst(dedesc:31:30)
     C                   eval      scdesc(2) = %subst(dedesc:j+1:30)
     C*** End Chg *** 08/09/10 **********************************
     C*** Beg Add *** 08/09/10 **********************************
     C                   eval      k = 30
     C                   if        %subst(scdesc(2):30:1) <> ' '
     C                   eval      k = k-1
     C                   dow       %subst(scdesc(2):k:1) <> ' '
     C                   eval      k = k-1
     C                   enddo
     C                   endif
     C                   eval      scdesc(2) = %subst(dedesc:j+1:k)
     C*** End Add *** 08/09/10 **********************************
     C*** Beg Chg *** 08/09/10 **********************************
     C*********          eval      scdesc(3) = %subst(dedesc:61:30)
     C                   eval      scdesc(3) = %subst(dedesc:j+k+1: 30)
     C*** End Chg *** 08/09/10 **********************************
     C                   endif
     C* **Beg Chg *** 04/12/11 **********************************************
     C*
     C*    Do not print now
     C***************    except    prtdata
     C*
     C***************    if        scdesc(2) <> ' '
     C***************    except    prtdata2
     C***************    if        scdesc(3) <> ' '
     C***************    except    prtdata3
     C***************    endif
     C***************    endif
     C*** Beg Add *** 08/09/10 **********************************
     C***************    except    space#
     C*** End Add *** 08/09/10 **********************************
     C*
     C*   Write the log record instead
     C                   exsr      writemdp
     C*
     C* **End Chg *** 04/12/11 **********************************************
     C*
     C                   eval      rcd_found = *on
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C*****              endif
     C*** End Del *** 05/18/10 **********************************
     C*
     C                   endif
     C*
     C                   endsr
     C*
    C*================================================================
     O*** Qsysprt
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
     O* **Beg Add *** 04/12/11 **********************************************
     O          E   N94      ROUTE          4
     O              N94                          22 'Report Sequence:'
     O              N94      @@srt               25
     O              N94 95                       35 'Group   '
     O              N94 96                       35 'Adjustor'
     O* **End Add *** 04/12/11 **********************************************
     O*
     O          E            ROUTE          1 58
     O                                         +001 '(c)'
     O                       *YEAR             +001
     O                                         +001 'StoneRiver, Inc.'
     O                                         +001 'All rights reserved.'
     O          E            ROUTE          1
     O                                         +001 'StoneRiver is a'
     O                                         +001 'trademark of'
     O                                         +001 'StoneRiver, Inc.'
     O* **Beg Chg *** 04/12/11 **********************************************
     O********  E ********   route          1  1
     O          E            Header         1  1
     O* **End Chg *** 04/12/11 **********************************************
     O                       dstime               9 '  :  :  '
     O                       CompanyNm           87
     O                       dsdate             132 '  /  /    '
     O          E            Header      1  2
     o                       q1pgm               11
     O                       xxtitl              96
     O                                          126 'Page'
     O                       page          Z    132
     O          e            Header         2
     O* **Beg Chg *** 04/12/11 **********************************************
     O              N96                          22 'Group:'
     O              N96      scfnd               26
     O              N96      scgnam              87
     O               96N94                       25 'Adjustor:'
     O               96N94   mdadcd              29
     O               96N94   mdadnm              55
     O*************                               7 'Group:'
     O*************          scfnd               11
     O*************          scgnam              72
     O* **End Chg *** 04/12/11 **********************************************
     O          e            Header         2
     O                                            8 'Yr/Case'
     O                                           27 'Claimant Name'
     O                                           45 'Code'
     O                                           53 'Field'
     O                                           94 'Incorrect Data'
     O                                          113 'Description'
     O*
     O          e            prtdata        1
     O                       YrCs                12
     O                       ClmntNm             39
     O                       scercd              46
     O                       scfld               78
     O                       scdata             100
     O                       scdesc(1)          132
     O          e            prtdata2       1
     O* **Beg Add *** 04/12/11 **********************************************
     O               96N94                       22 'Group:'
     O               96N94   mdfnd               26
     O               96N94   scgnam              87
     O               95N94                       25 'Adjustor:'
     O               95N94   mdadcd              29
     O               95N94   mdadnm              55
     O* **End Add *** 04/12/11 **********************************************
     O                       scdesc(2)          132
     O          e            prtdata3       1
     O                       scdesc(3)          132
     O*
     O          e            Space#         1
     O*
     O          e            Skip#                1
     O*
     O          e            Fot1           1
     O                                           78 ' *** End  Of  Report *** '
     O*
     O          e            Fot2           1
     O                                           78 ' *** No Records Found *** '
     O*
