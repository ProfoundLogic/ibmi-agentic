     *===============================================================
     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     *===============================================================
     * TITLE SC961R1 - print All remarks report cover page
     *===============================================================
      * DESCRIPTION:
      *  This program will print a cover page.
      *
      *========================================================================
      * Created on 06/23/2003 by c:62 for Project 129WAO
      *========================================================================
     *  REVISIONS:
     *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
      * 08/26/14 C:298 Project: SY354                                 *
      *                -Implement Routing Page Changes.               *
      *                                                               *
      * 01/28/10  C:249  Project: SY215                               *
      *           C:189  Retrieve brand Name                          *
      *                                                               *
      * 12/15/08 C: 220 SUPPORT: 63991
      *                -THE DARTS PRINT REPORT INS NOT PRINTINNG R/H
      *                 MARGIN ON REPORT.
      *
     *
     *===============================================================
      ******* BEG CHG ******* 12/15/08 ********************************
     F******* qsysprt   o    f   95        printer oflind(*inoa) usropn
     Fqsysprt   o    f  108        printer oflind(*inoa) usropn
      ******* END CHG ******* 12/15/08 ********************************
     Fwmcml     if   e           k disk
     Fwmeml     if   e           k disk
     Fsmorl     if   e           k disk
     Fsmupl     if   e           k disk

     *===============================================================
      * Definition Specifications
     *===============================================================

     D Output_proc     pr                  extpgm('SC961C1')
     D  path                        256a
     D  output_type                   1a

     D psds          esds                  extname(szq1p)

     D                 ds
     D folder                        16
     D  dCompany                      3    Overlay(folder:1)
     D  dGroup                        3    Overlay(folder:4)
     D  dPolYear                      3    Overlay(folder:7)
     D  dCase                         7    Overlay(folder:10)

     D                 ds
     D subfolder                     12
     D  dfdat                         6    Overlay(subfolder:1)
     D  dtdat                         6    Overlay(subfolder:7)

     D                 ds
     D document                      27
     D  claim#                       16    Overlay(document:1)
     D  report                       10    Overlay(document:18)
     D                                     inz('Cover1.PDF')

     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

     D eType           s              1a
     D eCompany        s              3a
     D eGroup          s              3a
     D ePolYear        s              3a
     D eCase           s              7a
     D eEmployer       s              9a
     D efdat           s              7a
     D etdat           s              7a
     D eotyp           s              1a
     D equeue          s             10a
     D fromDate        s              8s 0
     D toDate          s              8s 0
     D pPolYear        s              3a
     D pCase           s              7a
     D ppath           s            256a
     D oGroup          s              3s 0
     D oPolYear        s              3s 0
     D oCase           s              7s 0
     D company         s              3  0
     D group           s              3  0
     D employer        s              9  0
     D division        s              5  0
     D polYear         s              3  0
     D subs            s              5  0
     D case            s              7  0
     D line1           s             78a
     D line2           s             78a
     D line3           s             78a
     D ReturnCode      s              1a
     D xxprt           s              1
     D xxdata          s             60
     D n               s              3  0
     D ##data          s             80
     D ##leng          s              3  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D fdat            s              7s 0
     D tdat            s              7s 0
    
     D title           c                   const('Print All Remark Process')
     D empCover        c                   const('EMPLOYER COVER PAGE')
     D caseCover       c                   const('CASE COVER PAGE')
     D rootFolder      c                   const('/CLAIMS OUTPUT')
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
    
     *===============================================================
      * PLIST Definitions
     *===============================================================
     C     *entry        plist
     C                   parm                    eType
     C                   parm                    eCompany
     C                   parm                    eGroup
     C                   parm                    ePolYear
     C                   parm                    eCase
     C                   parm                    eEmployer
     C                   parm                    efdat
     C                   parm                    etdat
     C                   parm                    eotyp
     C                   parm                    equeue
    C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr

      *========================================================================
      * KList Definitions
      *========================================================================
      * Managed Care Case file key
     C     cmkey         klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    employer

      * Employer Master File key
     C     emkey         klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    employer
    C*
    C*   KEY LIST FOR THE OVERRIDE FILE.
    C*
     C     orkey         klist
     C                   kfld                    q1pgm
     C                   kfld                    xxprt

     *===============================================================
      * Main
     *===============================================================
     C                   call      'SA702RR'
     C                   parm      *BLANKS       returnCode
     C                   parm                    eCompany
     C                   parm                    eGroup
     C                   parm                    ePolYear
     C                   parm                    eCase
     C                   parm                    line1
     C                   parm                    line2
     C                   parm                    line3

     C                   open      qsysprt
     C                   time                    dsdttm

     C                   if        eType = 'C'
    C*
    C*   GET THE DISTRIBUTION INFORMATION.
    C*
     C                   move      '0'           xxprt
     C     orkey         chain     por01                              60
    C*
    C*   PRINT THE DISTRIBUTION INFORMATION, IF A VALID RECORD IS FOUND.
    C*
     C                   if        *in60 = *off
     C                             and orstat = 'A'
     C     q1user        chain     pup01                              60
    C*
    C*  Left Justify Title in Field
    C*
     C                   move(p)   ortitl        xxdata
     C     ' '           check     xxdata        n
     C                   if        n <= 60
     C                   subst(p)  xxdata:n      ortitl
     C                   endif

     C                   if        efdat <> '0000000'
     C                   move      efdat         fdat
     C                   z-add     fdat          srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         fromDate
     C                   else
     C                   eval      fromDate = 0
     C                   endif

     C                   if        etdat <> '0000000'
     C                   move      etdat         tdat
     C                   z-add     tdat          srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         toDate
     C                   else
     C                   eval      toDate = 99999999
     C                   endif

     C                   move      eGroup        oGroup
     C                   move      ePolYear      oPolYear
     C                   move      eCase         oCase
     c*
     c*** Beg Add ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     c*** End add ***** 01/28/10 **************************************

      ***** Beg Del ***** 08/26/14 ************************************
     C**********         except    route
      ***** End Del ***** 08/26/14 ************************************
    C*
    C*  Center Text in Field
    C*
     C                   movel     ortitl        ##data
    C*
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      60            ##leng
    C*
     C                   movel     ##data        ortitl
    C*
     C                   endif
      * print the case cover page
    
     C                   except    head

     C                   else
      * create the employer cover page
     C                   move      eCompany      company
     C                   move      eGroup        group
     C                   move      eEmployer     employer

     C     cmkey         chain     pcm01                              60

     C                   if        *in60 = *on
     C     emkey         chain     pem01                              61
      * print error message: no cases for employer
     C                   except    title1
     C                   except    errormsg

     C                   else
      * print header
     C                   except    head1
     C                   endif

      * for each case...
     C                   dow       *in60 = *off

     C                   eval      polYear = cmfyr
     C                   eval      division = cmdiv
     C                   eval      case = cmcase
     C                   move      cmcase        pCase
     C                   move      cmfyr         pPolYear

     C                   call      'SA702RR'
     C                   parm      *BLANKS       returnCode
     C                   parm                    eCompany
     C                   parm                    eGroup
     C                   parm                    pPolYear
     C                   parm                    pCase
     C                   parm                    line1
     C                   parm                    line2
     C                   parm                    line3

     C   oa              except    head1
     C   oa              setoff                                       oa
     C                   except    claims

     C     cmkey         reade     pcm01                                  60
     C                   enddo

     C                   endif

      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       If etype = 'C';
          xxPrt = '0';
          Chain orKey POR01;
          If %found and Orstat = 'A' ;
             Except Route;
          Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   close     qsysprt
     C*
     C* Call program that converts the report into a PDF
     C*

     C                   if        eotyp = 'F' or eotyp = 'B'

     C*
     C* Let's generate the zzzzzstatement
     C*

     C                   eval      dcompany = ecompany
     C                   eval      dgroup = egroup
     C                   eval      dpolyear = epolyear
     C                   eval      dcase = ecase
     C                   move      efdat         dfdat
     C                   move      etdat         dtdat
     C                   eval      claim# = folder

     C     rootFolder    cat(p)    '/':0         ppath
     C                   cat       folder:0      ppath
     C                   cat       '/':0         ppath
     C                   cat       subfolder:0   ppath
     C                   cat       '/':0         ppath
     C                   cat       document:0    ppath

     C                   callp     Output_Proc(ppath:eotyp)

     C                   endif

     C                   move      *on           *inlr
    
     *===============================================================
      * Output specs
     *===============================================================
      * Case cover page
     Oqsysprt   e            route          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********e            route          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********e            route          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********e            route          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********e            route          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********e            route          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********e            route          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********e            route          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********e            route          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********e            route          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********e            route          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********e            route          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********e            route          2
     O**********                                 15 'Route to:'
     O**********             upusnm              67
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          e            route          2
     O                                           15 'Route to:'
     O                       upusnm              67
      ***** End Add ***** 08/05/15 ************************************
     O          e            route          2
     O                                           19 'Report Title:'
     O                       ortitl              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********e            route          2
     O**********                                 19 'Program name:'
     O**********                                 30 'SC960R    '
     O**********                                 36 'File:'
     O**********             orfil#              38
     O**********                                 46 'Copies:'
     O**********             orcopy        z     50
     O**********                                 56 'Hold:'
     O**********             orhold              61
     O**********e            route          2
     O**********                                 19 'Output queue:'
     O**********             equeue              30
     O**********                                 37 'Forms:'
     O**********             orform              48
     O**********                                 56 'Save:'
     O**********             orsave              61
      ***** End Del ***** 08/26/14 ************************************

     O          e            route          1
     O                                           24 'Report Selections:'

     O          e            route          1
     O                                           19 'Group  . . :'
     O                       oGroup        4  +0001
     O          e            route          1
     O                                           19 'Year . . . :'
     O                       oPolYear      4  +0001
     O          e            route          1
     O                                           19 'Case . . . :'
     O                       oCase         4  +0001
     O          e            route          1
     O                                           19 'Date Range :'
     O                       fromDate         +0000 '0  /  /    '
     O                                        +0001 '-'
     O                       toDate           +0000 '0  /  /    '
     O          e            route          1
     O                                           19 'Output type:'
     O                       eotyp            +0001
     O          e            route          1
     O                                           19 'Queue  . . :'
     O                       equeue           +0001

     O          e            route          2
    
     O*** Beg Chg *****  01/28/10  ************************************
     O****      e            route            60
     O***                                        23 '(c) Copyright Progressi'
     O***                                           've Data Solutions, Inc.,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     o*** End Chg *****  01/28/10  ************************************
     Oqsysprt   e            head              2
     O                       dsdate              12 '  /  /    '
     O                       title               58
     O                       dstime              95 '  :  :  '
     O          e            head        1
     O                       q1pgm               12
     O                       caseCover           55
     O                                           90 'Page:'
     O                       page                95
     O          e            head        2
     O                       line1               80
     O          e            head        1
     O                       line2               80
     O          e            head        1
     O                       line3               80

      * Employer cover page header
     O          e            head1             2
     O                       title               58
     O          e            head1       1
     O                       empCover            56
     O          e            head1       2
     O                       line1               80

      * Employer cover page detail
     O          e            claims      3
     O                       line2               80
     O          e            claims      1
     O                       line3               80

      * Error message for no cases
     O          e            errormsg    2
     O                                           27 '** No cases for employer:'
     O                       EMNAME              69

     O          e            title1      2
     O                       title               58
     O          e            title1      1
     O                       empCover            56
