      *=====================================================================*
     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *=====================================================================*
     h/title W1053R1: Copy existing group's Endorsement forms to new group
      *=====================================================================*
      * Program description:                                                *
      *   This program copies an existing group's Endorsement               *
      *   form table records for a new group.                               *
      *                                                                     *
      * Called from: W1053R                                                 *
      * Calls to   : None                                                   *
      *                                                                     *
      * Programmer : C:113                                                  *
      * Created    : 05/24/99                                               *
      * Project    : CM027                                                  *
      *=====================================================================*
      * REVISIONS:                                                          *
      *                                                                     *
      * 11/10/16 C:136 Project: SY371                                       *
      *                Use field for extended descriptions                  *
      *                                                                     *
      * 08/05/15 C:307 Project: SY354                                       *
      *                -Adding 'Route To:'                                  *
      *                                                                     *
      * 08/26/14 C:298 Project: SY354                                       *
      *                -Implement Routing Page Changes.                     *
      *                                                                     *
      * XX/XX/XX C:XXX PROJECT: XXXXX                                       *
      *                XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX       *
      *                                                                     *
      * ------------------------------------------------------------------- *
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS                  *
      * ------------------------------------------------------------------- *
      *=====================================================================*

     Hdatedit(*ymd) datfmt(*ymd)

     FSMORL     if   e           k disk                                         Output Routing
     FWMFNL     if   e           k disk                                         Group Master
     FWMUPL     if   e           k disk                                         User Parameters
     FWTENP     o    e             disk    rename(PEN01:enPhysical)             Employer Endorsement
     FWTENL     if   e           k disk                                         Employer End logical
     FWTMYP     o    e             disk    rename(PMY01:myPhysical)             Midterm Endorsement
     FWTMYL1    if   e           k disk                                         Midterm End logical
     FWTMYL     if   e           k disk    rename(PMY01:PMY02)                  Midterm End logical

     FQSYSPRT   o    f   80        printer oflind(*in71)                        Output

      *=============================================================================================
      *  Data Structure definitions
      *=============================================================================================
      ** External data structure
     d psds          esds                  extname(szq1p)                       External prog info

      ** Working storage
     dWorkStor         ds                  inz                                   Work/Storage Area
     d  xxDATE                        8s 0                                         Date
     d    xxYEAR                      4s 0 overlay(xxDATE:1)                         Year
     d    xxMNTH                      2s 0 overlay(xxDATE:5)                         Month
     d    xxDAY                       2s 0 overlay(xxDATE:7)                         day

     d  xxTIME                        6s 0                                         Time
     d    xxHOUR                      2s 0 overlay(xxTIME:1)                         Hour
     d    xxMIN                       2s 0 overlay(xxTIME:3)                         Minute
     d    xxSEC                       2s 0 overlay(xxTIME:5)                         Second
      *=============================================================================================
      *  Field definitions
      *=============================================================================================
     d  xxNewGrp       s                   like(ENFNd)                          New Group
     d  xxCpyGrp       s                   like(ENFNd)                          Group to copy from
     d  xxGroup        s                   like(ENFNd)                          Group for keylist
     d  xxMid          s                   like(MYSORT)                         Midterm sort value
     d  xxENCount      s              3p 0 inz(0)                               Count of End records
     d  xxMYCount      s              3p 0 inz(0)                               Count of mid records
     d  xxAllGrps      s              1    inz(*off)                            Proc all groups
     d  xxAllFrms      s              1    inz(*off)                            proc all forms
     d  xxAllMids      s              1    inz(*off)                            proc all midterms
     d  xxQue          s                   like(OROUTQ)
     d  xxTitle        s             60    inz('ENDORSEMENT FORMS MASS COPY')

      ** Temp variables
     d  xxCompany      s                   like(ENCO#)
     d  xxForm         s                   like(ENFORM)
     d  xxDesc         s                   like(MYDESC)
     d  xxDescr        s                   like(ENDESC)
      *** beg add ***** 11/10/16 *************************
     d  xxExtDsc       s                   like(ENDSC2)
      *** end add ***** 11/10/16 *************************
     d  xxState        s                   like(ENST)
     d  xxeddt         s                   like(ENEddT)
     d  xxmnau         s                   like(ENMNAU)
     d  xxPrint        s                   like(ENPRNT)
     d  xxrpfm         s                   like(ENRPFM)
     d  xxrpdt         s                   like(ENRPdT)
     d  xxFolder       s                   like(ENFLdR)
     d  xxScf1         s                   like(ENSCF1)
     d  xxSct1         s                   like(ENSCF1)
     d  xxScf2         s                   like(ENSCF1)
     d  xxSct2         s                   like(ENSCF1)
     d  xxScf3         s                   like(ENSCF1)
     d  xxSct3         s                   like(ENSCF1)
     d  xxSingle       s                   like(ENSNGL)
     d  xxSort         s                   like(MYSORT)
     d  xxsrfl         s                   like(MYSRFL)
     d  xxPgm1         s                   like(MYPGM1)
     d  xxPgm2         s                   like(MYPGM2)
     d  xxPgm3         s                   like(MYPGM3)
     d  xxPgm4         s                   like(MYPGM4)
     d  xxPgm5         s                   like(MYPGM5)
     d  xxdsc1         s                   like(MYdSC1)
     d  xxdsc2         s                   like(MYdSC2)
     d  xxdsc3         s                   like(MYdSC3)
     d  xxStat         s                   like(MYSTAT)
     d  xxUser         s                   like(MYUSER)
     d  xxPgm          s                   like(MYPGM)
      *=============================================================================================
      *  PLIST definitions
      *=============================================================================================
      ** Entry parameters
     c     *entry        plist
     c                   parm                    pmCompany         3             Company
     c                   parm                    pmCpyGrp          3             Existing Group
     c                   parm                    pmNewGrp          3             New Group
     c                   parm                    pmPolFlag         1             Policy/Midterm Flag
     c                   parm                    pmForm           15             Form to copy
     c                   parm                    pmMid             3             Mid to copy
     c                   parm                    pmQue            10             User Parm que
     c                   parm                    pmMode            1             Batch or Demand
      *=============================================================================================
      *  KLIST definitions
      *=============================================================================================
      ** Group Master file
     c     wmfnKey       klist
     c                   kfld                    xxCompany

      ** Midterm Endorsement Table file
     c     wtmyKey       klist
     c                   kfld                    xxCompany
     c                   kfld                    xxCpyGrp

     c     wtmyKey1      klist
     c                   kfld                    xxCompany
     c                   kfld                    xxGroup
     c                   kfld                    xxDesc
     c                   kfld                    xxSort

     c     wtmyKey2      klist
     c                   kfld                    xxCompany
     c                   kfld                    xxGroup
     c                   kfld                    xxMid

      ** Endorsement Table file
     c     wtenKey       klist
     c                   kfld                    xxCompany
     c                   kfld                    xxGroup

     c     wtenKey1      klist
     c                   kfld                    xxCompany
     c                   kfld                    xxGroup
     c                   kfld                    xxForm

     c     wtenKey2      klist
     c                   kfld                    xxCompany
     c                   kfld                    xxGroup
     c                   kfld                    pmForm

     c     wmmuKey       klist
     c                   kfld                    q1user

     c     smorKey       klist
     c                   kfld                    q1pgm
     c                   kfld                    xxPrt             1
      *=============================================================================================
      *  Main
      *=============================================================================================
      ** Process all groups or one group
     c                   If        xxAllGrps = *on
     c     WMFNkey       SetLL     PFN01
     c     WMFNkey       ReadE     PFN01                                  70
     c                   Eval      xxNewGrp = FNFNd
     c                   EndIf


     c                   DoW       NOT *in70

      ** do not process for the copy from group
     c                   If        xxNewGrp <> xxCpyGrp

     c                   Select

      ** Process the Midterm Endorsement records
     c                   When      pmPolFlag = 'M'
     c                   If        *in71 = *on
     c                   Except    ##Header
     c                   Eval      *in71 = *off
     c                   EndIf

     c                   Except    ##MidHead

     c                   Eval      xxMYCount = 0

     c                   If        xxAllMids = *off

     c     WTMYkey2      Chain     PMY02                              60
     c                   Exsr      $ProcMid

     c                   Else

     c     WTMYkey       SetLL     PMY01
     c     WTMYkey       ReadE     PMY01                                  60

     c                   DoW       *in60 = *off

     c                   Exsr      $ProcMid
     c                   Eval      xxGroup = xxCpyGrp
     c     WTMYkey1      Chain     PMY01                              60
     c     WTMYkey       ReadE     PMY01                                  60

     c                   EndDo

     c                   EndIf                                                  xxAllMids = *off

     c                   If        xxMYCount = 0
     c                   Except    ##None
     c                   EndIf


      ** Process the Endorsement records
     c                   When      pmPolFlag = 'P'
     c                   If        *in71 = *on
     c                   Except    ##Header
     c                   Eval      *in71 = *off
     c                   EndIf

     c                   Except    ##EmpHead

     c                   Eval      xxGroup = xxCpyGrp
     c                   Eval      xxENCount = 0

     c                   If        xxAllFrms = *off

     c     WTENkey2      Chain     PEN01                              60
     c                   Exsr      $ProcEnd

     c                   Else

     c     WTENkey       SetLL     PEN01
     c     WTENkey       ReadE     PEN01                                  60

     c                   DoW       NOT *in60

     c                   Exsr      $ProcEnd
     c                   Eval      xxGroup = xxCpyGrp
     c     WTENkey1      Chain     PEN01                              60
     c     WTENkey       ReadE     PEN01                                  60

     c                   EndDo

     c                   EndIf                                                  xxAllFrms = *off

     c                   If        xxENCount = 0
     c                   Except    ##None
     c                   EndIf

     c                   EndSL

     c                   EndIf                                                  xxNewGrp <> xxCpyGrp

     c                   If        xxAllGrps = *on
     c     WMFNkey       ReadE     pfn01                                  70
     c                   Eval      xxNewGrp = FNFNd
     c                   Else
     c                   Eval      *in70 = *on
     c                   EndIf

     c                   EndDo

      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain smorKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     c                   Eval      *inlr = *on

     c                   Return
      *=============================================================================================
      *  Program Initialization Subroutine
      *=============================================================================================
     c     *inzsr        BEGSR

     c                   Eval      xxDATE=*date
     c                   time                    xxTIME
     c                   Move      pmCompany     xxCompany
     c                   Move      pmCpyGrp      xxCpyGrp
     c                   Move      pmQue         xxQue

      ** Demand or Batch mode?
     c                   If        pmMode = 'D'
     c                   Eval      *in09 = *on
     c                   Else
     c                   Eval      *in08 = *on
     c                   EndIf

      ** Processing for all groups?
     c                   If        pmNewGrp = *blanks
     c                   Eval      xxAllGrps = *on
     c                   Eval      *in11 = *on
     c                   Else
     c                   Move      pmNewGrp      xxNewGrp
     c                   Eval      xxAllGrps = *off
     c                   Eval      *in11 = *off
     c                   EndIf

      ** Policy Page or Midterm?
     c                   If        pmPolFlag = 'P'
     c                   Eval      *in14 = *on
     c                   Else
     c                   Eval      *in14 = *off
     c                   Endif

      ** Processing for specific form?
     c                   If        pmForm <> *blanks
     c                   Move      pmForm        xxForm
     c                   Eval      xxAllFrms = *off
     c                   Eval      *in12 = *off
     c                   Else
     c                   Eval      xxForm = *blanks
     c                   Eval      xxAllFrms = *on
     c                   Eval      *in12 = *on
     c                   EndIf

      ** Processing for specific midterm?
     c                   If        pmMid  <> *blanks
     c                   Move      pmMid         xxMid
     c                   Eval      xxAllMids = *off
     c                   Eval      *in13 = *off
     c                   Else
     c                   Eval      xxAllMids = *on
     c                   Eval      *in13 = *on
     c                   EndIf

     c                   Eval      xxGroup = xxCpyGrp
     c                   Eval      *in70 = *off

     c                   Eval      xxPrt = '0'

      ** Get the User Parameter Master Record
     c     wmmuKey       Chain     PUP01                              60

      ** Get the Routing Master Record
     c     smorKey       Chain     POR01                              60

     c                   If        *in60 = *off
      ***** Beg Del ***** 08/26/14 ************************************
     c************       Except    Route
      ***** End Del ***** 08/26/14 ************************************
     c                   EndIf

     c                   Except    ##Header

     c                   EndSR
      *=============================================================================================
      *  ProcEnd - Process Endorsement Table record
      *=============================================================================================
     c     $ProcEnd      BEGSR

      ** Move values into temp fields
     c                   Eval      xxCompany = enco#
     c                   Eval      xxGroup = enfnd
     c                   Eval      xxForm = enform
     c                   Eval      xxDescr = Endesc
      *** beg add ***** 11/10/16 ***********************
     c                   Eval      xxExtDsc = Endsc2
      *** end add ***** 11/10/16 ***********************
     c                   Eval      xxState = enst
     c                   Eval      xxeddt = eneddt
     c                   Eval      xxmnau = enmnau
     c                   Eval      xxPrint = enprnt
     c                   Eval      xxrpfm = enrpfm
     c                   Eval      xxrpdt = enrpdt
     c                   Eval      xxFolder = enfldr
     c                   Eval      xxPgm1 = enpgm1
     c                   Eval      xxPgm2 = enpgm2
     c                   Eval      xxPgm3 = enpgm3
     c                   Eval      xxScf1 = enscf1
     c                   Eval      xxSct1 = ensct1
     c                   Eval      xxScf2 = enscf2
     c                   Eval      xxSct2 = ensct2
     c                   Eval      xxScf3 = enscf3
     c                   Eval      xxSct3 = ensct3
     c                   Eval      xxStat = enstat
     c                   Eval      xxSingle = ensngl

      ** Check new group for record
     c                   Eval      xxGroup = xxNewGrp

     c     wtenKey1      Chain     pen01                              60

     c                   If        *in60
     c                   Exsr      $WriteEnd

     c                   If        *in71
     c                   Except    ##Header
     c                   Except    ##EmpHead
     c                   Eval      *in71 = *off
     c                   EndIf

     c                   Except    ##Empdet
     c                   EndIf

     c                   EndSR                                                   $ProcEnd
      *=============================================================================================
      *  ProcMid  - Process Midterm Endorsement Table record
      *=============================================================================================
     c     $ProcMid      BEGSR

      ** Move values into temp fields
     c                   Eval      xxCompany = myco#
     c                   Eval      xxGroup = myfnd
     c                   Eval      xxDesc = mydesc
     c                   Eval      xxSort = mysort
     c                   Eval      xxsrfl = mysrfl
     c                   Eval      xxPgm1 = mypgm1
     c                   Eval      xxPgm2 = mypgm2
     c                   Eval      xxPgm3 = mypgm3
     c                   Eval      xxPgm4 = mypgm4
     c                   Eval      xxPgm5 = mypgm5
     c                   Eval      xxdsc1 = mydsc1
     c                   Eval      xxdsc2 = mydsc2
     c                   Eval      xxdsc3 = mydsc3
     c                   Eval      xxStat = mystat

      ** Check new group for record
     c                   Eval      xxGroup = xxNewGrp

     c     wtmyKey1      Chain     pmy01                              60

     c                   If        *in60
     c                   Exsr      $WriteMid

     c                   If        *in71
     c                   Except    ##Header
     c                   Except    ##MidHead
     c                   Eval      *in71 = *off
     c                   EndIf

     c                   Except    ##Middet
     c                   EndIf

     c                   EndSR                                                   $ProcMid
      *=============================================================================================
      *  $WriteMid - Write new Midterm Endorsement Table record
      *=============================================================================================
     c     $WriteMid     BEGSR
     c                   Eval      myco# = xxCompany
     c                   Eval      myfnd = xxGroup
     c                   Eval      mydesc = xxDesc
     c                   Eval      mysort = xxSort
     c                   Eval      mysrfl = xxsrfl
     c                   Eval      mypgm1 = xxPgm1
     c                   Eval      mypgm2 = xxPgm2
     c                   Eval      mypgm3 = xxPgm3
     c                   Eval      mypgm4 = xxPgm4
     c                   Eval      mypgm5 = xxPgm5
     c                   Eval      mydsc1 = xxdsc1
     c                   Eval      mydsc2 = xxdsc2
     c                   Eval      mydsc3 = xxdsc3
     c                   Eval      mystat = xxStat
     c                   Eval      mydt = xxdate - 19000000
     c                   Eval      mytime = xxTime
     c                   Eval      myuser = q1user
     c                   Eval      mypgm = q1pgm

     c                   write     myPhysical
     c                   Eval      xxMYCount = xxMYCount + 1

     c                   EndSR                                                   $WriteWTMY
      *=============================================================================================
      *  $WriteEnd - Write new Endorsement Table record
      *=============================================================================================
     c     $WriteEnd     BEGSR
     c                   Eval      enco# = xxCompany
     c                   Eval      enfnd = xxGroup
     c                   Eval      enform = xxForm
     c                   Eval      Endesc = xxDescr
      *** beg add ***** 11/10/16 ***********************
     c                   Eval      Endsc2 = xxExtDsc
      *** end add ***** 11/10/16 ***********************
     c                   Eval      enst = xxState
     c                   Eval      eneddt = xxeddt
     c                   Eval      enmnau = xxmnau
     c                   Eval      enprnt = xxPrint
     c                   Eval      enrpfm = xxrpfm
     c                   Eval      enrpdt = xxrpdt
     c                   Eval      enfldr = xxFolder
     c                   Eval      enpgm1 = xxPgm1
     c                   Eval      enpgm2 = xxPgm2
     c                   Eval      enpgm3 = xxPgm3
     c                   Eval      enscf1 = xxScf1
     c                   Eval      ensct1 = xxSct1
     c                   Eval      enscf2 = xxScf2
     c                   Eval      ensct2 = xxSct2
     c                   Eval      enscf3 = xxScf3
     c                   Eval      ensct3 = xxSct3
     c                   Eval      enstat = xxStat
     c                   Eval      ensngl = xxSingle
     c                   Eval      enstat = xxStat
     c                   Eval      Endt = xxdate - 19000000
     c                   Eval      entime = xxTime
     c                   Eval      enuser = q1user
     c                   Eval      enpgm = q1pgm

     c                   write     enPhysical
     c                   Eval      xxENCount = xxENCount + 1

     c                   EndSR                                                   $WriteEnd
      *=============================================================================================
      *  Printer Output
      *=============================================================================================
      ** Routing page
     o/COPY SCOPYROUT
     o          e            ROUTE
     o                                           24 'REPORT SELECTIONS:'

     o          e            ROUTE       2
     o                                           22 'COPY FROM GROUP:'
     o                       pmCpyGrp            26

     o          e            ROUTE       2
     o                                           22 'COPY TO GROUP  :'
     o              N11      pmNewGrp            26
     o               11                          27 '*ALL'

     o          e            ROUTE       2
     o               14                          22 'FORM TO COPY   :'
     o              N14                          22 'MIDTERM TO COPY:'
     o               14 12                       27 '*ALL'
     o               14N12   pmForm              38
     o              N14 13                       27 '*ALL'
     o              N14N13   pmMid               26

      ** Page header
     oqsysprt   e            ##Header       1  2
     o                       q1pgm               10
     o               09      ortitl              80
     o               08      xxTitle             80
     o                       q1user              80

     o          e            ##Header       2
     o                       xxMNTH               2
     o                                            3 '/'
     o                       xxdAY                5
     o                                            6 '/'
     o                       xxYEAR              10
     o                                           74 'Page'
     o                       PAGE          4     80

      ** Midterm Endorsement header
     o          e            ##MidHead      2
     o                                           21 'Midterm Endorsements '
     o                                           38 'Written to Group '
     o                       xxNewGrp            41

     o          e            ##MidHead      1
     o                                            6 'Sort'
     o                                           28 'Description'

      ** Midterm Endorsement detail
     o          e            ##Middet       1
     o                       xxSort               6
     o                       xxDesc              47

      ** Policy Page Endorsement header
     o          e            ##EmpHead      2
     o                                           25 'Policy Page Endorsements '
     o                                           42 'Written to Group '
     o                       xxNewGrp            46

     o          e            ##EmpHead      1
     o                                            6 'Form'
     o                                           28 'Description'

      ** Policy Page Endorsement detail
     o          e            ##Empdet       1
     o                       xxForm              17
     o                       xxDescr             47

     o          e            ##None         1
     o                                           12 '*** None ***'

