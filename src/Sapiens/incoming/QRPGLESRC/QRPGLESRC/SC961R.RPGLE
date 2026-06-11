     H*========================================================================
     H* (C) Copyright Progressive Data Solutions, Inc., 2003
     H*========================================================================
     H/TITLE SC961R - Open Enrollment report program
     H*========================================================================
     H* DESCRIPTION:
     H*  This program will process through the claims of a given employer & d
     H*  call the programs to generate the reports.
     H*
     H*========================================================================
     H* Created on 06/23/2003 by c:62 for Project 129WAO
     H*========================================================================
     H* REVISIONS:
     H*
     H*  03/13/06   C:168  PROJECT:129INM
     H*                    Added call to pgm SC966C to create a txt document
     H*                    in the "Claims Output Log" directory for each
     H*                    DARTS key processed.  This will be used by FCC
     H*                    to automate their CD creation
     H*
     H*========================================================================
     F* File Definitions
     F*========================================================================
     Fwmcml     if   e           k disk
     Fwmcml1    if   e           k disk    rename(pcm01:pcm02)
     Fwmcml7    if   e           k disk    rename(pcm01:pcm03)

      *========================================================================
      * Procedure Prototypes
      *========================================================================
      /copy SC961RPR
      /copy DPRREMARK

      *========================================================================
      * Variable Definitions
      *========================================================================
     D eCompany        s              3a
     D eGroup          s              3a
     D eEmployer       s              9a
     D eYear           s              3a
     D eCase           s              7a
     D eOutq           s             10a
     D ePsid           s              2a
     D outq            s             10a
     D pPolYear        s              3a
     D pmcGroup        s              3a
     D pmcEmp          s              9a
     D pSubs           s              5a
     D pCase           s              7a
     D pType           s              1a
     D mcKey           s             40a
     D savkey          s             40a
     D pMode           s              1a   inz('D')
     D pRemark         s            256a   inz(*blanks)
     D pRmkCode        s              5a   dtaara(SC961DTA)
     D pSeq            s              5a
     D pUser           s             10a
     D pRmkDat         s              8a
     D dateIn          s              8  0
     D pDiv            s              5a   inz('00000')
     D hdemp#          s                   like(cmemp#) inz(0)
     D hddiv           s                   like(cmdiv) inz(0)

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
     D efdat           s              7a
     D etdat           s              7a
     D eotyp           s              1a
     D eqry1           s            478a
     D esmcds          s             56a
     D ermcds          s            210a
     D eprint          s              1a
     D psmcds          s             57a
     D prmcds          s            211a

      ** External data structure
     D psds          esds                  extname(szq1p)

      *========================================================================
      * KList Definitions
      *========================================================================
      * Managed Care Case file key
     C     cmkey1        klist
     C                   kfld                    company
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

      ***** BEG  ADD  03/13/06 *********************************************

     C     WRTTXT        Plist
     C                   Parm                    sKey             40
     C                   Parm      *Blanks       sRetrn            1

      ***** END  ADD  03/13/06 *********************************************

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
     C                   parm                    ePsid
     C                   parm                    efdat
     C                   parm                    etdat
     C                   parm                    eotyp
     C                   parm                    eqry1
     C                   parm                    esmcds
     C                   parm                    ermcds
     C                   parm                    eprint

      *========================================================================
      * Main
      *========================================================================

     C                   eval      pmcGroup = eGroup
     C                   eval      pmcEmp = eEmployer
     C                   eval      dateIn = *date
     C                   move      eCompany      company
     C                   move      pmcGroup      group
     C                   move      eEmployer     employer
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

     C* MUST GET mcKey filled with the Claims Key (co/fnd/fyr/case)
     C* call api to get key

     C                   callp     CaseKeyAPI(returnCode:
     C                                       eCompany:pmcGroup:
     C                                       pPolYear:pCase:mcKey)

     C* print claim cover page

     C                   eval      pType = 'C'
     C                   callp     CoverPrint(pType:eCompany:pmcGroup:
     C                                       pPolYear:pCase:pmcEmp:efdat:
     C                                       etdat:eotyp:eOutq)

     C* call mc report program

     C                   eval      savkey = mckey

     C* call darts print program

     C                   if        eprint = 'Y'
     C                   callp     dartPrint(mcKey:eOutq:pMode:efdat:
     C                                       etdat:eotyp:eqry1)
     C                   endif

     C                   eval      mckey = savkey

     C* call program to print documents on 400

     C                   callp     docPrint(mcKey:outq:returnCode:efdat:
     C                                      etdat:eotyp:esmcds:ermcds)

      ***** BEG  ADD  03/13/06 *********************************************

      * creates a spool file which is later copied into a .txt file in the
      * "Claims Output Log" directory, for each DARTS key.

     c                   eval      sKey = mcKey
     c                   call      'SC966C'      WRTTXT

      ***** END  ADD  03/13/06 *********************************************

     C* added code to write a claim Remark ***

     C                   move      dateIn        pRmkDat

     C                   in        pRmkCode
     C                   if        pRmkCode <> ' '
     C                   callp     WriteRmk(mcKey:pSeq:pRemark:
     C                               pRmkDat:pRmkCode:q1pgm:q1user:
     C                               'W0070R':'N':'N':'Y':'N')
     C                   endif

     C                   endsr
      *------------------------------------------------------------------------*
      *  EmpLoop - this subroutine processes for a single employer
      *------------------------------------------------------------------------*
     C     EmpLoop       begsr

     C                   eval      first_time = 'Y'

     C     cmkey1        chain     pcm01                              60

     C                   eval      polYear = cmfyr
     C                   eval      division = cmdiv
     C                   eval      case = cmcase
     C                   move      cmcase        pCase
     C                   move      cmfyr         pPolYear

     C                   if        first_time = 'Y'
     C                   eval      first_time = 'N'
     C                   eval      pType = ' '
      * print employer cover page
     C                   callp     CoverPrint(pType:eCompany:pmcGroup:
     C                                       pPolYear:pCase:pmcEmp:efdat:
     C                                       etdat:eotyp:eOutq)

     C                   endif

     C                   dow       *in60 = *off

     C                   exsr      ProcClmRec

     C     cmkey1        reade     pcm01                                  60
     C                   enddo

     C                   movel(p)  esmcds        psmcds
     C                   move      '*'           psmcds
     C                   movel(p)  ermcds        prmcds
     C                   move      '*'           prmcds

      * call program to submit requests for images
     C                   callp     ImagePrint(eCompany:eGroup:pmcEmp:
     C                             eYear:eCase:ePsid:efdat:etdat:eotyp:
     C                             psmcds:prmcds)

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

     C*  get the DARTS remark code from the data area and write a remark if not
     C                   in        pRmkCode
     C                   if        pRmkCode <> ' '
     C                   callp     WriteRmk(mcKey:pSeq:pRemark:
     C                               pRmkDat:pRmkCode:q1pgm:q1user:
     C                               'W0050R':'N':'N':'Y':'N')
     C                   endif

     C                   endsr


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

     C                   movel(p)  esmcds        psmcds
     C                   move      '*'           psmcds
     C                   movel(p)  ermcds        prmcds
     C                   move      '*'           prmcds

      * call program to submit requests for images
     C                   callp     ImagePrint(eCompany:eGroup:pmcEmp:
     C                             eYear:eCase:ePsid:efdat:etdat:eotyp:
     C                             psmcds:prmcds)

      * write a remark indicating that open enrollment was done
      * call api to get employer key
     C                   eval      wcin = 'Y'
     C                   eval      wdck = 'Y'
     C                   eval      returnCode = 'K'
     C                   clear                   mcKey

     ********* MUST GET EMPLOYER KEY HERE into mckey for Dart remark


     C                   move      cmemp#        clmEmp#           9
     C                   move      cmdiv         clmDiv            5
     C                   callp     EmpKeyAPI(returnCode:
     C                                       eCompany:pmcGroup:
     C                                       clmEmp#:clmDiv:mcKey)

     C                   move      dateIn        pRmkDat

     C*  get the DARTS remark code from the data area and write a remark if not
     C                   in        pRmkCode
     C                   if        pRmkCode <> ' '
     C                   callp     WriteRmk(mcKey:pSeq:pRemark:
     C                               pRmkDat:pRmkCode:q1pgm:q1user:
     C                               'W0050R':'N':'N':'Y':'N')
     C                   endif

     C                   endsr
      *------------------------------------------------------------------------*
      *  GrpLoop - this subroutine processes for a single group
      *------------------------------------------------------------------------*
     C     GrpLoop       begsr

     C     cmkey3        chain     pcm01                              99
     C                   dow       *in99 = *off

     C                   eval      polYear = cmfyr
     C                   eval      division = cmdiv
     C                   eval      case = cmcase
     C                   move      cmcase        pCase
     C                   move      cmfyr         pPolYear

     C                   exsr      ProcClmRec

     C* write a remark indicating that open enrollment was done
     C* call api to get employer key

     C                   if        hdemp# <> cmemp# or hddiv <> cmdiv

     C                   eval      hdemp# = cmemp#
     C                   eval      hddiv  = cmdiv
     C                   eval      wcin = 'Y'
     C                   eval      wdck = 'Y'
     C                   eval      returnCode = 'K'
     C                   clear                   mcKey

     C* MUST GET EMPLOYER KEY HERE into mckey for Dart remark

     C                   move      cmemp#        clmEmp#           9
     C                   move      cmdiv         clmDiv            5
     C                   callp     EmpKeyAPI(returnCode:
     C                                       eCompany:pmcGroup:
     C                                       clmEmp#:clmDiv:mcKey)

     C                   move      dateIn        pRmkDat

     C*  get the DARTS remark code from the data area and write a remark if not

     C                   in        pRmkCode
     C                   if        pRmkCode <> ' '
     C                   callp     WriteRmk(mcKey:pSeq:pRemark:
     C                               pRmkDat:pRmkCode:q1pgm:q1user:
     C                               'W0050R':'N':'N':'Y':'N')
     C                   endif
     C                   endif

     C     cmkey3        reade     pcm01                                  99
     C                   enddo

     C                   movel(p)  esmcds        psmcds
     C                   move      '*'           psmcds
     C                   movel(p)  ermcds        prmcds
     C                   move      '*'           prmcds

     C* call program to submit requests for images

     C                   callp     ImagePrint(eCompany:eGroup:pmcEmp:
     C                             eYear:eCase:ePsid:efdat:etdat:eotyp:
     C                             psmcds:prmcds)

     C                   endsr
