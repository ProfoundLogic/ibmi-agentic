      *========================================================================
      * (C) Copyright Progressive Data Solutions, Inc., 2005
      *========================================================================
     H/TITLE SC981R - Open Enrollment report program
      *========================================================================
      * DESCRIPTION:
      *  This program will process through the cliams of a given employer and
      *  call the programs to generate the reports.
      *
      * CREATED   : 04/03/00
      * PROGRAMMER: C:113
      * PROJECT   : 87MIW
      *========================================================================
      * REVISIONS:
      *
      * 07/11/05 C:62  Proj: dr193
      *                Added 'Output type' to entry parms.
      *
      * 11/27/02 C:87  Proj: 114IEC
      *                Added 'Print Server Id' to entry parms and parms for
      *                call to ImagePrint
      *
      * 07/24/02 C:87  Proj: 114IEC
      *                In addition to processing all claims for an employer,
      *                added the functionality to process all employers for a
      *                group (when only the group is specified) and to process
      *                a single claim (when the fund year and case are passed in).
      *                No revision comments were actually added 'in' the program.
      *
      * ----------------------------------------------------------------------
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.
      * ----------------------------------------------------------------------
      *========================================================================
      * FILE USAGE:
      *
      *  File    Description                    Rcdfmt  Key
      *  FMCML1  MC Master Case File            PCM01   CO#:SUB:FND:FYR
      *
      *========================================================================
      * INDICATOR USAGE:
      *  Global Indicators
      *   60 - General chain.
      *   90 - General error.
      *
      *  Error Indicators
      *
      *
      *========================================================================
      * NOTES:
      *
      *
      *========================================================================
      *========================================================================
      * File Definitions
      *========================================================================
     F***fmcmlj    if   e           k disk
     Fwmcml     if   e           k disk
     Fwmcml1    if   e           k disk    rename(pcm01:pcm02)
     Fwmcml7    if   e           k disk    rename(pcm01:pcm03)

      *========================================================================
      * Procedure Prototypes
      *========================================================================
      /copy SC981RPR
      /copy DPRREMARK

      *========================================================================
      * Variable Definitions
      *========================================================================
     D eCompany        s              3a
     D eGroup          s              3a
     D eEmployer       s              9a
     D eYear           s              3a
     D eCase           s              7a
     d***  begin add  ***  07/11/05  ***************************************
     D eOutt           s              1a
     D pOutt           s              1a
     D eFrdt           s              7a
     D pFrdt           s              7a
     D eTodt           s              7a
     D pTodt           s              7a
     d***  end   add  ***  07/11/05  ***************************************
     D eOutq           s             10a
     D***  begin add  ***  11/27/02  ***************************************
     D ePsid           s              2a
     D***  end   add  ***  11/27/02  ***************************************
     D outq            s             10a
     D pPolYear        s              3a
     D pmcGroup        s              3a
     D pmcEmp          s              9a
     D pSubs           s              5a
     D pCase           s              7a
     D pType           s              1a
     D mcKey           s             40a
     D savkey          s             40a
     D pQueue          s             10a   inz(*blanks)
     D pMode           s              1a   inz('O')
     D pRemark         s            256a   inz(*blanks)
     D***  begin chg  ***  08/09/02  ***************************************
     D*** pRmkCode        s              5a   inz('ANOPE')
     D pRmkCode        s              5a   dtaara(SC981DTA)
     D***  end   chg  ***  08/09/02  ***************************************
     D pSeq            s              5a
     D pUser           s             10a
     D pRmkDat         s              8a
     D dateIn          s              8  0
     D pDiv            s              5a   inz('00000')

     D company         s              3  0
     D group           s              3  0
     D employer        s              9  0
     D division        s              5  0
     D polYear         s              3  0
     D subs            s              5  0
     D case            s              7  0
     D returnCode      s              1a
     D wcin            s              1a
     D wdck            s              1a
     D first_time      s              1a

      ** External data structure
     D psds          esds                  extname(szq1p)

      *========================================================================
      * KList Definitions
      *========================================================================
      * Managed Care Case file key
     C     cmkey         klist
     C                   kfld                    company
     C***************    kfld                    subs
     C                   kfld                    group
     C                   kfld                    employer

     C     cmkey2        klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    polYear
     C                   kfld                    case

     C     cmkey3        klist
     C                   kfld                    company
     C                   kfld                    group

      *========================================================================
      * PList Definitions
      *========================================================================
     C     *entry        plist
     C                   parm                    eCompany
     C                   parm                    eGroup
     C                   parm                    eEmployer
     C                   parm                    eYear
     C                   parm                    eCase
     C                   parm                    eOutq
     C***  begin add  ***  11/27/02  ***************************************
     C                   parm                    ePsid
     C***  end   add  ***  11/27/02  ***************************************
     C***  begin add  ***  07/11/05  ***************************************
     C                   parm                    eOutt
     C                   parm                    eFrdt
     C                   parm                    eTodt
     C***  end   add  ***  07/11/05  ***************************************

      *========================================================================
      * Main
      *========================================================================
     C***********        CALLP     GetSubAPI(returnCode:eCompany:eGroup:
     C***********                            pSubs:pmcGroup)

     C                   eval      pmcGroup = eGroup
     C                   eval      pmcEmp = eEmployer
     C                   eval      dateIn = *date
     C                   move      eCompany      company
     C                   move      pmcGroup      group
     C****************** move      pSubs         subs
     C                   move      eEmployer     employer
     C***  begin add  ***  07/11/05  ***************************************
     c                   eval      pOutt = eOutt
     c                   eval      pFrdt = eFrdt
     c                   eval      pTodt = eTodt
     C***  end   add  ***  07/11/05  ***************************************
     C                   move      eOutq         outq
     C                   move      eYear         polYear
     C                   move      eCase         case

      **  choose what subroutine to call to iterate through claims
     C                   select
     C                   when      employer <> 0
     C                   exsr      EmpLoop

     C                   when      polYear <> 0
     C                   exsr      ClmLoop

     C                   other
     C                   exsr      GrpLoop

     C                   endsl

     C***  begin del  ***  08/09/02  *******************************************
      **write a remark indicating that open enrollment was done
      **call api to get employer key
     C******             eval      wcin = 'Y'
     C******             eval      wdck = 'Y'
     C******             eval      returnCode = 'K'
     C******             clear                   mcKey
      ******
     ********* MUST GET EMPLOYER KEY HERE into mckey for Dart remark
      ******
      ******
     C******             callp     EmpKeyAPI(returnCode:
     C******                                 eCompany:pmcGroup:
     C******                                 pmcEmp:pDiv:mcKey)
      ******
     C******             move      dateIn        pRmkDat
      ******
     C******             callp     WriteRmk(mcKey:pSeq:pRemark:
     C******                         pRmkDat:pRmkCode:q1pgm:q1user:
     C******                         'W0050R':'N':'N':'Y':'N')
     C***  end   del  ***  08/09/02  *******************************************

     C                   eval      *inlr = *on

      *========================================================================*
      * Subroutine Definitions                                                 *
      *========================================================================*

      *------------------------------------------------------------------------*
      *  ProcClmRec - this subroutine is called to do the processing associated
      *               with each claim record iterated.
      *------------------------------------------------------------------------*
     C     ProcClmRec    begsr

     C                   eval      polYear = cmfyr
     C                   eval      division = cmdiv
     C                   eval      case = cmcase
     C                   move      cmcase        pCase
     C                   move      cmfyr         pPolYear

     C                   eval      wcin = 'Y'
     C                   eval      wdck = 'Y'
     C                   eval      returnCode = 'K'
     C                   eval      mcKey = *blanks

     *********  MUST GET mcKey filled with the Claims Key (co/fnd/fyr/case)
      * call api to get key
     C                   callp     CaseKeyAPI(returnCode:
     C                                       eCompany:pmcGroup:
     C                                       pPolYear:pCase:mcKey)

      * print claim cover page
     C                   eval      pType = 'C'
     C****************   callp     CoverPrint(pType:eCompany:pSubs:pmcGroup:
     C********************                   pPolYear:pCase:pmcEmp)
     C***  begin chg  ***  07/11/05  ***************************************
     C***********        callp     CoverPrint(pType:eCompany:pmcGroup:
     C***********                            pPolYear:pCase:pmcEmp)
     C                   callp     CoverPrint(pType:eCompany:pmcGroup:
     C                                       pPolYear:pCase:pmcEmp:pOutt:
     C                                       pFrdt:pTodt)
     C***  end   chg  ***  07/11/05  ***************************************

      * call mc report program
     C*******            callp     MCreports(company:group:polYear:case)

     C                   eval      savkey = mckey
      * call darts print program
     C***  begin chg  ***  07/11/05  ***************************************
     C***********        callp     dartPrint(mcKey:pQueue:pMode)
     C                   callp     dartPrint(mcKey:pQueue:pMode:pOutt:
     C                                       pFrdt:pTodt)
     C***  end   chg  ***  07/11/05  ***************************************

     C                   eval      mckey = savkey

      * call program to print documents on 400
     C***  begin chg  ***  07/11/05  ***************************************
     C***********        callp     docPrint(mcKey:outq:returnCode)
     C                   callp     docPrint(mcKey:outq:returnCode:pOutt:
     C                                       pFrdt:pTodt)
     C***  end   chg  ***  07/11/05  ***************************************

     *** added code to write a claim Remark ***
     C                   move      dateIn        pRmkDat

     C***  begin add ***  08/09/02  ******************************************
     C*  get the DARTS remark code from the data area and write a remark if not blank
     C                   in        pRmkCode
     C                   if        pRmkCode <> ' '
     C***  end   add ***  08/09/02  ******************************************
     C                   callp     WriteRmk(mcKey:pSeq:pRemark:
     C                               pRmkDat:pRmkCode:q1pgm:q1user:
     C                               'W0070R':'N':'N':'Y':'N')
     C***  begin add ***  08/09/02  ******************************************
     C                   endif                                                  pRmkCode<>' '
     C***  end   add ***  08/09/02  ******************************************
     *** added code to write a claim Remark *** END ***** mg 05/16/00

     C                   endsr                                                  ProcClmRec


      *------------------------------------------------------------------------*
      *  EmpLoop - this subroutine processes for a single employer
      *------------------------------------------------------------------------*
     C     EmpLoop       begsr

     C                   eval      first_time = 'Y'

     C     cmkey         chain     pcm01                              60

     C                   eval      polYear = cmfyr
     C                   eval      division = cmdiv
     C                   eval      case = cmcase
     C                   move      cmcase        pCase
     C                   move      cmfyr         pPolYear

     C                   if        first_time = 'Y'
     C                   eval      first_time = 'N'
     C                   eval      pType = ' '
      * print employer cover page
     C***************    callp     CoverPrint(pType:eCompany:pSubs:pmcGroup:
     C********************                   pPolYear:pCase:pmcEmp)
     C***  begin chg  ***  07/11/05  ***************************************
     C***********        callp     CoverPrint(pType:eCompany:pmcGroup:
     C***********                            pPolYear:pCase:pmcEmp)
     C                   callp     CoverPrint(pType:eCompany:pmcGroup:
     C                                       pPolYear:pCase:pmcEmp:pOutt:
     C                                       pFrdt:pTodt)
     C***  end   chg  ***  07/11/05  ***************************************

     C                   endif

     C                   dow       *in60 = *off

     C                   exsr      ProcClmRec

     C     cmkey         reade     pcm01                                  60
     C                   enddo


      * call program to submit requests for images
     C***  begin chg  ***  11/27/02  ***************************************
     C****************** callp     ImagePrint(eCompany:eGroup:pmcEmp:
     C******************           eYear:eCase)
     C***  begin chg  ***  07/11/05  ***************************************
     C***********        callp     ImagePrint(eCompany:eGroup:pmcEmp:
     C***********                  eYear:eCase:ePsid)
     C                   callp     ImagePrint(eCompany:eGroup:pmcEmp:
     C                             eYear:eCase:ePsid:pOutt:
     C                                       pFrdt:pTodt)
     C***  end   chg  ***  07/11/05  ***************************************
     C***  end   chg  ***  11/27/02  ***************************************

      * write a remark indicating that open enrollment was done
      * call api to get employer key
     C                   eval      wcin = 'Y'
     C                   eval      wdck = 'Y'
     C                   eval      returnCode = 'K'
     C                   clear                   mcKey

     ********* MUST GET EMPLOYER KEY HERE into mckey for Dart remark


     C                   callp     EmpKeyAPI(returnCode:
     C                                       eCompany:pmcGroup:
     C                                       pmcEmp:pDiv:mcKey)

     C                   move      dateIn        pRmkDat

     C***  begin add ***  08/09/02  ******************************************
     C*  get the DARTS remark code from the data area and write a remark if not blank
     C                   in        pRmkCode
     C                   if        pRmkCode <> ' '
     C***  end   add ***  08/09/02  ******************************************
     C                   callp     WriteRmk(mcKey:pSeq:pRemark:
     C                               pRmkDat:pRmkCode:q1pgm:q1user:
     C                               'W0050R':'N':'N':'Y':'N')
     C***  begin add ***  08/09/02  ******************************************
     C                   endif                                                  pRmkCode<>' '
     C***  end   add ***  08/09/02  ******************************************

     C                   endsr                                                  EmpLoop


      *------------------------------------------------------------------------*
      *  ClmLoop - this subroutine processes for a single claim
      *------------------------------------------------------------------------*
     C     ClmLoop       begsr

     C     cmkey2        chain     pcm02                              60

     C                   eval      polYear = cmfyr
     C                   eval      division = cmdiv
     C                   eval      case = cmcase
     C                   move      cmcase        pCase
     C                   move      cmfyr         pPolYear

     C                   dow       *in60 = *off

     C                   exsr      ProcClmRec

     C     cmkey2        reade     pcm02                                  60
     C                   enddo


      * call program to submit requests for images
     C***  begin chg  ***  11/27/02  ***************************************
     C****************** callp     ImagePrint(eCompany:eGroup:pmcEmp:
     C******************           eYear:eCase)
     C***  begin chg  ***  07/11/05  ***************************************
     C***********        callp     ImagePrint(eCompany:eGroup:pmcEmp:
     C***********                  eYear:eCase:ePsid)
     C                   callp     ImagePrint(eCompany:eGroup:pmcEmp:
     C                             eYear:eCase:ePsid:pOutt:
     C                                       pFrdt:pTodt)
     C***  end   chg  ***  07/11/05  ***************************************
     C***  end   chg  ***  11/27/02  ***************************************

      * write a remark indicating that open enrollment was done
      * call api to get employer key
     C                   eval      wcin = 'Y'
     C                   eval      wdck = 'Y'
     C                   eval      returnCode = 'K'
     C                   clear                   mcKey

     ********* MUST GET EMPLOYER KEY HERE into mckey for Dart remark


     C***  begin chg  ***  08/09/02  ***************************************
     C*****************  callp     EmpKeyAPI(returnCode:
     C*****************                      eCompany:pmcGroup:
     C*****************                      pmcEmp:pDiv:mcKey)
     C                   move      cmemp#        clmEmp#           9
     C                   move      cmdiv         clmDiv            5
     C                   callp     EmpKeyAPI(returnCode:
     C                                       eCompany:pmcGroup:
     C                                       clmEmp#:clmDiv:mcKey)
     C***  end   chg  ***  08/09/02  ***************************************

     C                   move      dateIn        pRmkDat

     C***  begin add ***  08/09/02  ******************************************
     C*  get the DARTS remark code from the data area and write a remark if not blank
     C                   in        pRmkCode
     C                   if        pRmkCode <> ' '
     C***  end   add ***  08/09/02  ******************************************
     C                   callp     WriteRmk(mcKey:pSeq:pRemark:
     C                               pRmkDat:pRmkCode:q1pgm:q1user:
     C                               'W0050R':'N':'N':'Y':'N')
     C***  begin add ***  08/09/02  ******************************************
     C                   endif                                                  pRmkCode<>' '
     C***  end   add ***  08/09/02  ******************************************

     C                   endsr                                                  ClmLoop


      *------------------------------------------------------------------------*
      *  GrpLoop - this subroutine processes for a single group
      *------------------------------------------------------------------------*
     C     GrpLoop       begsr

     C     cmkey3        chain     pcm03                              60

      **  loop to process all employers for this group
     C                   dow       *in60 = *off

     C                   move      cmemp#        pmcEmp
     C                   eval      employer = cmemp#
     C                   eval      polYear = cmfyr
     C                   eval      division = cmdiv
     C                   eval      case = cmcase
     C                   move      cmcase        pCase
     C                   move      cmfyr         pPolYear

      **    process all claims for the current employer
     C                   exsr      EmpLoop

     C     cmkey         setgt     pcm03
     C     cmkey3        reade     pcm03                                  60
     C                   enddo

     C                   endsr                                                  GrpLoop

      *========================================================================*
      * Procedure Definitions                                                  *
      *========================================================================*
