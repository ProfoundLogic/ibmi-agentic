      ** (C) Copyright Progressive Data Solutions, Inc., 1999
     h/title FIXNEWGRP: Copy existing group's endorsement forms to new group
      ****************************************************************
      ** Program description:                                       **
      **   This program copies an existing group's endorsement      **
      **   form table records for a new group.                      **
      **                                                            **
      ** Called from: Command line                                  **
      **                                                            **
      ** Calls to:    None                                          **
      **                                                            **
      ** Programmer: C:113                                          **
      ** Created:    05/24/99  PROJECT: CM027                       **
      ****************************************************************

     Hdatedit(*ymd) datfmt(*ymd)

     FWMFNL     if   e           k disk                                         Group Master

     FWTMYP     o    e             disk    rename(PMY01:myPhysical)             Midterm Endorsement
     FWTMYL1    if   e           k disk                                         Midterm End logical
     FWTMYL     if   e           k disk    rename(PMY01:PMY02)                  Midterm End logical

     FWTENP     o    e             disk    rename(PEN01:enPhysical)             Employer Endorsement
     FWTENL     if   e           k disk                                         Employer End logical

     FQSYSPRT   o    f   80        printer oflind(*in71)                        Output

      *=============================================================================================
      *  data Structure definitions
      *=============================================================================================
      ** External data structure
     d psds          esds                  extname(szq1p)                       External prog info

      ** Working storage
     dWorkStor         ds                  inz                                   Work/Storage Area
     d  xxDATE                        8s 0                                         System date
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
     d  xxMid          s                   like(MYSORT)
     d  xxENCount      s              3p 0 inz(0)                               Count of end records
     d  xxMYCount      s              3p 0 inz(0)                               Count of mid records
     d  xxAllGrps      s              1    inz(*off)                            Proc all groups
     d  xxAllFrms      s              1    inz(*off)                            proc all forms
     d  xxAllMids      s              1    inz(*off)                            proc all midterms

      ** Temp variables
     d  xxCompany      s                   like(ENCO#)
     d  xxForm         s                   like(ENFORM)
     d  xxDesc         s                   like(MYDESC)
     d  xxDescr        s                   like(ENDESC)
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
     C     *entry        plist
     C                   parm                    pmCompany         3             Company
     C                   parm                    pmCpyGrp          3             Existing Group
     C                   parm                    pmNewGrp          3             New Group
     C                   parm                    pmPolFlag         1             Policy/Midterm Flag
     C                   parm                    pmForm           15             Form to copy
     C                   parm                    pmMid             3             Mid to copy
     C                   parm                    pmDebug           1
      *=============================================================================================
      *  KLIST definitions
      *=============================================================================================
      ** Group Master file
     C     wmfnKey       klist
     C                   kfld                    xxCompany

      ** Midterm Endorsement Table file
     C     wtmyKey       klist
     C                   kfld                    xxCompany
     C                   kfld                    xxCpyGrp

     C     wtmyKey1      klist
     C                   kfld                    xxCompany
     C                   kfld                    xxGroup
     C                   kfld                    xxDesc
     C                   kfld                    xxSort

     C     wtmyKey2      klist
     C                   kfld                    xxCompany
     C                   kfld                    xxGroup
     C                   kfld                    xxMid

      ** Endorsement Table file
     C     wtenKey       klist
     C                   kfld                    xxCompany
     C                   kfld                    xxGroup

     C     wtenKey1      klist
     C                   kfld                    xxCompany
     C                   kfld                    xxGroup
     C                   kfld                    xxForm

     C     wtenKey2      klist
     C                   kfld                    xxCompany
     C                   kfld                    xxGroup
     C                   kfld                    pmForm
      *=============================================================================================
      *  Main
      *=============================================================================================
      ** Process all groups or one group
     C                   IF        xxAllGrps = *on
     C     WMFNkey       setll     PFN01
     C     WMFNkey       reade     PFN01                                  70
     C                   eval      xxNewGrp = FNFNd
     C                   ENDIF

     C                   DOW       NOT *in70

      ** do not process for the copy from group
     c                   IF        xxNewGrp <> xxCpyGrp

     c                   SELECT

     c                   When      pmPolFlag = 'M'
      ** Process the Midterm Endorsement records
     C   20              except    ##MidHead
     C                   eval      xxMYCount = 0

     C                   IF        xxAllMids = *off

     C     WTMYkey2      chain     PMY02                              60
     C                   exsr      $ProcMid

     C                   ELSE

     C     WTMYkey       setll     PMY01
     C     WTMYkey       reade     PMY01                                  60

     C                   DOW       *in60 = *off

     C                   exsr      $ProcMid
     C                   eval      xxGroup = xxCpyGrp
     C     WTMYkey1      chain     PMY01                              60
     C     WTMYkey       reade     PMY01                                  60

     C                   ENDDO

     C                   ENDIF                                                  xxAllMids = *off

     C                   IF        xxMYCount = 0
     C   20              except    ##None
     C                   ENDIF


     c                   When      pmPolFlag = 'P'
      ** Process the Endorsement records
     C   20              except    ##Header
     C   20              except    ##EmpHead
     C                   eval      xxGroup = xxCpyGrp
     C                   eval      xxENCount = 0

     C                   IF        xxAllFrms = *off

     C     WTENkey2      chain     PEN01                              60
     C                   exsr      $ProcEnd

     C                   ELSE

     C     WTENkey       setll     PEN01
     C     WTENkey       reade     PEN01                                  60

     C                   DOW       NOT *in60

     C                   exsr      $ProcEnd
     C                   eval      xxGroup = xxCpyGrp
     C     WTENkey1      chain     PEN01                              60
     C     WTENkey       reade     PEN01                                  60

     C                   ENDDO

     C                   ENDIF                                                  xxAllFrms = *off

     C                   IF        xxENCount = 0
     C   20              except    ##None
     C                   ENDIF

     c                   ENdSL

     c                   ENDIF                                                  xxNewGrp <> xxCpyGrp

     C                   IF        xxAllGrps = *on
     C     WMFNkey       reade     pfn01                                  70
     C                   eval      xxNewGrp = FNFNd
     C                   ELSE
     C                   eval      *in70 = *on
     C                   ENDIF

     C                   ENDDO

     C                   eval      *inlr = *on

     C                   return
      *=============================================================================================
      *  Program Initialization Subroutine
      *=============================================================================================
     c     *inzsr        BEGSR

     c                   IF        pmDebug = 'Y'
     c                   eval      *in20 = *on
     c                   ELSE
     c                   eval      *in20 = *off
     c                   ENDIF

     c                   eval      xxDATE=*date
     c                   time                    xxTIME
     c                   move      pmCompany     xxCompany
     c                   move      pmCpyGrp      xxCpyGrp

      ** Processing for all groups?
     c                   IF        pmNewGrp = *blanks
     c                   eval      xxAllGrps = *on
     c                   ELSE
     c                   move      pmNewGrp      xxNewGrp
     c                   eval      xxAllGrps = *off
     c                   ENDIF

      ** Processing for specific form?
     c                   IF        pmForm <> *blanks
     c                   move      pmForm        xxForm
     c                   eval      xxAllFrms = *off
     c                   ELSE
     c                   eval      xxForm = *blanks
     c                   eval      xxAllFrms = *on
     c                   ENDIF

      ** Processing for specific midterm?
     c                   IF        pmMid  <> *blanks
     c                   move      pmMid         xxMid
     c                   eval      xxAllFrms = *off
     c                   ELSE
     c                   eval      xxAllMids = *on
     c                   ENDIF

     c                   eval      xxGroup = xxCpyGrp
     c                   eval      *in70 = *off

     c                   ENDSR
      *=============================================================================================
      *  ProcEnd - Process Endorsement Table record
      *=============================================================================================
     c     $ProcEnd      BEGSR

      ** Move values into temp fields
     c                   eval      xxCompany = enco#
     c                   eval      xxGroup = enfnd
     c                   eval      xxForm = enform
     c                   eval      xxDescr = endesc
     c                   eval      xxState = enst
     c                   eval      xxeddt = eneddt
     c                   eval      xxmnau = enmnau
     c                   eval      xxPrint = enprnt
     c                   eval      xxrpfm = enrpfm
     c                   eval      xxrpdt = enrpdt
     c                   eval      xxFolder = enfldr
     c                   eval      xxPgm1 = enpgm1
     c                   eval      xxPgm2 = enpgm2
     c                   eval      xxPgm3 = enpgm3
     c                   eval      xxScf1 = enscf1
     c                   eval      xxSct1 = ensct1
     c                   eval      xxScf2 = enscf2
     c                   eval      xxSct2 = ensct2
     c                   eval      xxScf3 = enscf3
     c                   eval      xxSct3 = ensct3
     c                   eval      xxStat = enstat
     c                   eval      xxSingle = ensngl

      ** Check new group for record
     c                   eval      xxGroup = xxNewGrp

     c     wtenKey1      chain     pen01                              60

     c                   IF        *in60
     c                   exsr      $WriteEnd

     c                   IF        *in71
     c   20              except    ##Header
     c   20              except    ##EmpHead
     c                   eval      *in71 = *off
     c                   ENDIF

     c   20              except    ##Empdet
     c                   ENDIF

     c                   ENDSR                                                   $ProcEnd
      *=============================================================================================
      *  ProcMid  - Process Midterm Endorsement Table record
      *=============================================================================================
     c     $ProcMid      BEGSR

      ** Move values into temp fields
     c                   eval      xxCompany = myco#
     c                   eval      xxGroup = myfnd
     c                   eval      xxDesc = mydesc
     c                   eval      xxSort = mysort
     c                   eval      xxsrfl = mysrfl
     c                   eval      xxPgm1 = mypgm1
     c                   eval      xxPgm2 = mypgm2
     c                   eval      xxPgm3 = mypgm3
     c                   eval      xxPgm4 = mypgm4
     c                   eval      xxPgm5 = mypgm5
     c                   eval      xxdsc1 = mydsc1
     c                   eval      xxdsc2 = mydsc2
     c                   eval      xxdsc3 = mydsc3
     c                   eval      xxStat = mystat

      ** Check new group for record
     c                   eval      xxGroup = xxNewGrp

     c     wtmyKey1      chain     pmy01                              60

     c                   IF        *in60
     c                   exsr      $WriteMid

     c                   IF        *in71
     c   20              except    ##Header
     c   20              except    ##MidHead
     c                   eval      *in71 = *off
     c                   ENDIF

     c   20              except    ##Middet
     c                   ENDIF

     c                   ENDSR                                                   $ProcMid
      *=============================================================================================
      *  $WriteMid - Write new Midterm Endorsement Table record
      *=============================================================================================
     c     $WriteMid     BEGSR
     c                   eval      myco# = xxCompany
     c                   eval      myfnd = xxGroup
     c                   eval      mydesc = xxDesc
     c                   eval      mysort = xxSort
     c                   eval      mysrfl = xxsrfl
     c                   eval      mypgm1 = xxPgm1
     c                   eval      mypgm2 = xxPgm2
     c                   eval      mypgm3 = xxPgm3
     c                   eval      mypgm4 = xxPgm4
     c                   eval      mypgm5 = xxPgm5
     c                   eval      mydsc1 = xxdsc1
     c                   eval      mydsc2 = xxdsc2
     c                   eval      mydsc3 = xxdsc3
     c                   eval      mystat = xxStat
     c                   eval      mydt = xxdate - 19000000
     c                   eval      mytime = xxTime
     c                   eval      myuser = q1user
     c                   eval      mypgm = q1pgm

     c                   write     myPhysical
     c                   eval      xxMYCount = xxMYCount + 1

     c                   ENDSR                                                   $WriteWTMY
      *=============================================================================================
      *  $WriteEnd - Write new Endorsement Table record
      *=============================================================================================
     c     $WriteEnd     BEGSR
     c                   eval      enco# = xxCompany
     c                   eval      enfnd = xxGroup
     c                   eval      enform = xxForm
     c                   eval      endesc = xxDescr
     c                   eval      enst = xxState
     c                   eval      eneddt = xxeddt
     c                   eval      enmnau = xxmnau
     c                   eval      enprnt = xxPrint
     c                   eval      enrpfm = xxrpfm
     c                   eval      enrpdt = xxrpdt
     c                   eval      enfldr = xxFolder
     c                   eval      enpgm1 = xxPgm1
     c                   eval      enpgm2 = xxPgm2
     c                   eval      enpgm3 = xxPgm3
     c                   eval      enscf1 = xxScf1
     c                   eval      ensct1 = xxSct1
     c                   eval      enscf2 = xxScf2
     c                   eval      ensct2 = xxSct2
     c                   eval      enscf3 = xxScf3
     c                   eval      ensct3 = xxSct3
     c                   eval      enstat = xxStat
     c                   eval      ensngl = xxSingle
     c                   eval      enstat = xxStat
     c                   eval      endt = xxdate - 19000000
     c                   eval      entime = xxTime
     c                   eval      enuser = q1user
     c                   eval      enpgm = q1pgm

     c                   write     enPhysical
     c                   eval      xxENCount = xxENCount + 1

     c                   ENDSR                                                   $WriteEnd
      *=============================================================================================
      *  Printer Output
      *=============================================================================================
      ** Page header
     Oqsysprt   e            ##Header       1  2
     O                       q1pgm               10
     O                                           52 'Endorsement Table Record'
     O                       q1user              80

     O          e            ##Header       2
     O                       xxMNTH               2
     O                                            3 '/'
     O                       xxdAY                5
     O                                            6 '/'
     O                       xxYEAR              10
     O                                           45 'Copy Program'
     O                                           74 'Page'
     O                       PAGE          4     80

      ** Midterm Endorsement header
     O          e            ##MidHead      2
     O                                           21 'Midterm Endorsements '
     O                                           38 'Written to Group '
     O                       xxNewGrp            41

     O          e            ##MidHead      1
     O                                            6 'Sort'
     O                                           28 'description'

      ** Midterm Endorsement detail
     O          e            ##Middet       1
     O                       xxSort               6
     O                       xxDesc              47

      ** Policy Page Endorsement header
     O          e            ##EmpHead      2
     O                                           25 'Policy Page Endorsements '
     O                                           42 'Written to Group '
     O                       xxNewGrp            46

     O          e            ##EmpHead      1
     O                                            6 'Form'
     O                                           28 'description'

      ** Policy Page Endorsement detail
     O          e            ##Empdet       1
     O                       xxForm              17
     O                       xxDescr             47

     O          e            ##None         1
     O                                           12 '*** None ***'

