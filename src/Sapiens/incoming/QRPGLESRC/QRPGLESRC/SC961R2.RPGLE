     Hcopyright('(c) Copyright StoneRiver, Inc., 2006 - 2009')
     Hoption(*NODEBUGIO:*SRCSTMT)
     H*========================================================================
     H/TITLE SC961R2 - Darts Aggregation - Policy
     H*========================================================================
     H* DESCRIPTION:
     H*  This program will process through the Policies
     H*  call the programs to generate the reports.
     H*
     H*========================================================================
     H* Created on 07/11/2006 by c:02 for Project 132IFV
     H*========================================================================
     H* REVISIONS:
     H*
     H* 03/10/09  C:33  Project: DR298
     H*                 Add group/employer security.
     H*
     H*  08/27/08 C:136 Support: WA132 63844
     H*                 Image doc's are intermittently being left out.  Fixed
     H*                 loop for this.
     H*
     H*  05/22/08   C:191  Project:132WIT
     H*                    Priss#27 - Correct keylist for WDELP records
     H*
     H*  04/07/08   C:191  Project:132WIT
     H*                    Priss#21 - Add policy exp date range
     H*
     H*  03/13/08   C:191  Project:132WIT
     H*                    Added a new "to" employer parameter.
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
     Fwmeml     if   e           k disk    rename(pem01:pem02)
     Fwdell9    if   e           k disk
     F***  Begin Add  ***  03/10/09  **************************************
     Fsmupl     if   e           k disk    rename(pup01:pup00)
     F***  End   Add  ***  03/10/09  **************************************

      *========================================================================
      * Procedure Prototypes
      *========================================================================
      /copy SC961R2PR
      /copy DPRREMARK

     D***  Begin Add  ***  03/10/09  **************************************
     DGetRestrict      pr             1n
     D inCo#                          3  0 Const
     D inUser                        10    const
     D inAppl                         1    const
     D*
     DRestricted       pr             1n
     D inCo#                          3  0 const
     D inUser                        10    const
     D inAppl                         1    const
     D inGrp                          3  0 const
     D inEmp#                         9  0 const options(*nopass)
     D inDiv                          5  0 const options(*nopass)
     D***  End   Add  ***  03/10/09  **************************************

      *========================================================================
      * Variable Definitions
      *========================================================================
     D eCompany        s              3a
     D eGroup          s              3a
     D eEmployer       s              9a
     D eDiv            s              5a
     D eOutq           s             10a
     D ePsid           s              2a
     D outq            s             10a
     D pPolYear        s              3a
     D pmcGroup        s              3a
     D pmcEmp          s              9a
     D pSubs           s              5a
     D pEmp#           s              9a
     D pType           s              1a
     D mcKey           s             40a
     D savkey          s             40a
     D pMode           s              1a   inz('$')
     D pRemark         s            256a   inz(*blanks)
     D pRmkCode        s              5a   dtaara(SC961DTA1)
     D pSeq            s              5a
     D pUser           s             10a
     D pRmkDat         s              8a
     D dateIn          s              8  0
     D pDiv            s              5a   inz('00000')
     D hdemp#          s                   like(ememp#) inz(0)
     D hddiv           s                   like(emdiv) inz(0)
     C**** Beg Add ********* 03/13/08 ****************************************
     D eToEmp          s              9a
     D ToEmp           s              9  0
     D prcSingle       s               N
     C**** End Add ********* 03/13/08 ****************************************

     D company         s              3  0
     D group           s              3  0
     D employer        s              9  0
     D emp#            s              9  0
     D division        s              5  0
     D Div             s              5  0
     C**** Beg Add ********* 04/07/08 ****************************************
     D ptdt            s              7  0
     D isSelected      s               n
     C**** End Add ********* 04/07/08 ****************************************
     D subs            s              5  0
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
     C**** Beg Add ********* 03/13/08 ****************************************
     D prvEmp#         s              9  0
     D prvDiv          s              5  0
     D prvEmpc         s              9a
     D prvDivc         s              5a
     C**** End Add ********* 03/13/08 ****************************************
     C**** Beg Add ********* 04/07/08 ****************************************
     D efedt           s              7a
     D etedt           s              7a
     D pfedt           s              7  0
     C**** End Add ********* 04/07/08 ****************************************
     D***  Begin Add  ***  03/10/09  **************************************
     D restrictW       s              1n
     D***  End   Add  ***  03/10/09  **************************************

      ** External data structure
     D psds          esds                  extname(szq1p)

      *========================================================================
      * KList Definitions
      *========================================================================

     C     emkey2        klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    employer
     C                   kfld                    Div
     C**** Beg Add ********* 03/13/08 ****************************************
     C     emkey         klist
     C                   kfld                    company
     C                   kfld                    group
     C**** End Add ********* 03/13/08 ****************************************
     C**** Beg Add ********* 04/07/08 ****************************************
     C     elky          klist
     C                   kfld                    company
     C                   kfld                    group
     C*** BEG CHG ****** 05/22/08 ************************************
     C*************      kfld                    employer
     C                   kfld                    ememp#
     C*************      kfld                    Div
     C                   kfld                    emdiv
     C                   kfld                    ptdt
     C*** END CHG ****** 05/22/08 ************************************
     C*
     C     elky1         klist
     C                   kfld                    company
     C                   kfld                    group
     C*** BEG CHG ****** 05/22/08 ************************************
     C*************      kfld                    employer
     C                   kfld                    ememp#
     C*************      kfld                    div
     C                   kfld                    emdiv
     C*** END CHG ****** 05/22/08 ************************************
     C**** End Add ********* 04/07/08 ****************************************


     C     WRTTXT        Plist
     C                   Parm                    sKey             40
     C                   Parm      *Blanks       sRetrn            1

      *========================================================================
      * PList Definitions
      *========================================================================
     C     *entry        plist
     C                   parm                    eCompany
     C                   parm                    eGroup
     C                   parm                    eEmployer
     C                   parm                    eDiv
     C                   parm                    eOutq
     C                   parm                    ePsid
     C                   parm                    efdat
     C                   parm                    etdat
     C                   parm                    eotyp
     C                   parm                    eqry1
     C                   parm                    esmcds
     C                   parm                    ermcds
     C                   parm                    eprint
     C**** Beg Add ********* 03/13/08 ****************************************
     C                   parm                    eToEmp
     C**** End Add ********* 03/13/08 ****************************************
     C**** Beg Add ********* 04/07/08 ****************************************
     C                   parm                    efedt
     C                   parm                    etedt
     C**** End Add ********* 04/07/08 ****************************************

      *========================================================================
      * Main
      *========================================================================

     C***  Begin Add  ***  03/10/09  **************************************
     C     q1user        chain     pup00                              60

     C                   eval      restrictW = GetRestrict(company
     C                                                    :q1user
     C                                                    :'W'
     C                                                    )
     C***  End   Add  ***  03/10/09  **************************************

     C                   eval      pmcGroup = eGroup
     C                   eval      pmcEmp = eEmployer
     C                   eval      dateIn = *date
     C                   move      eCompany      company
     C                   move      pmcGroup      group
     C                   move      eOutq         outq
     C                   move      eEmployer     Employer
     C                   move      eDiv          Div
     C**** Beg Add ********* 03/13/08 ****************************************
     C                   move      eToEmp        ToEmp
     C                   eval      prcSingle = *off
     C                   if        employer = ToEmp
     C                   eval      prcSingle = *on
     C                   endif                                                  employer=toEmp
     C**** End Add ********* 03/13/08 ****************************************
     C**** Beg Add ********* 04/07/08 ****************************************
     C                   move      efedt         pfedt
     C                   move      etedt         ptdt
     C                   eval      isSelected= *off
     C**** End Add ********* 04/07/08 ****************************************

      **  choose what subroutine to call to iterate through claims

     C                   exsr      EmpLoop


     C                   eval      *inlr = *on

      *========================================================================*
      * Subroutine Definitions                                                 *
      *========================================================================*


      *------------------------------------------------------------------------*
      *  EmpLoop - this subroutine processes for a single Employer
      *------------------------------------------------------------------------*
     C     EmpLoop       begsr

      *** beg del ***** 08/27/08 *********************************************
     C**** Beg Chg ********* 03/13/08 ****************************************
     C*****emkey2        chain     pem02                              60
     C*****emkey2        Setll     pem02                              60
     C*****emkey         ReadE     pem02                                  60
     C*****End*Chg ********* 03/13/08 ****************************************
      **********
     C**********         eval      Div     = emDiv
     C**********         eval      division = emdiv
     C**********         eval      Emp# = EmEmp#
     C**********         eval      employer = emEmp#
     C*****Beg*Add ********* 03/13/08 ****************************************
     C**********         eval      prvEmp# = emEmp#
     C**********         eval      prvDiv = emDiv
     C*****End*Add ********* 03/13/08 ****************************************
     C**********         move      ememp#        pEmp#
     C**********         move      emDiv         pDiv
      **********
     C**********         dow       *in60 = *off
     C*****Beg*Add ********* 03/13/08 ****************************************
     C**********
     C**********         If        emEmp# > ToEmp
     C**********         leave
     C**********         EndIf                                                  emEmp# > ToEmp
     C*****Beg*Add ********* 04/07/08 ****************************************
     C**Check*policy exp date range
     C*****elky*         Setll     pel01                              61
     C*****elky1         Readpe    pel01                                  61
     C**********         If        *in61 = *off and elptdt >= pfedt
     C*****End*Add ********* 04/07/08 ****************************************
     C**********
     C**********         eval      isSelected= *on
     C**********         exsr      ProcClmRec
     C*****Beg*Add ********* 04/07/08 ****************************************
     C**********         EndIf                                                  *in61 = *off
     C*****End*Add ********* 04/07/08 ****************************************
     C*****emkey         reade     pem02                                  60
     C**********
     C**On*employer break process Imageprint and other functions for employer
     C*****Beg*Add ********* 04/07/08 ****************************************
     C**********         If        isSelected
     C*****End*Add ********* 04/07/08 ****************************************
     C**********         If        emEmp#<>prvEmp# or emDiv<>prvDiv
     C**********                   or *in60 or not prcSingle
     C**********         eval      isSelected= *off
     C**********         Exsr      ProcEmpBrk
     C**Update*previous employer values
     C**********         eval      prvEmp# = emEmp#
     C**********         eval      prvDiv = emDiv
     C**********         EndIf                                                  emEmp# <>lstEmp#
     C**********
     C**********         if        prcSingle
     C**********         leave
     C**********         endif                                                  prcSingle
     C*****Beg*Add ********* 04/07/08 ****************************************
     C**********         EndIf                                                  isSelected
     C*****End*Add ********* 04/07/08 ****************************************
     C**********
     C**********         Enddo
      *** end del ***** 08/27/08 *********************************************
      *** beg add ***** 08/27/08 *********************************************
     c     emkey2        setll     pem02
     c     emkey         reade     pem02
     c                   dow       not(%eof) and ememp# <= toEmp
     C***  Begin Add  ***  03/10/09  **************************************
     C                   if        not Restricted(company
     C                                           :q1user
     C                                           :'W'
     C                                           :EMFND
     C                                           :EMEMP#
     C                                           :EMDIV
     C                                           )
     C***  End   Add  ***  03/10/09  **************************************
     c     elky          setgt     pel01
     c     elky1         readpe    pel01
     c                   if        not(%eof) and ELPTDT >= pfedt or
     c                             prcSingle = *on
     C                   exsr      procClmRec
     C                   exsr      procEmpBrk
     c                   endif                                                  not(%eof)
     C***  Begin Add  ***  03/10/09  **************************************
     C                   endif
     C***  End   Add  ***  03/10/09  **************************************
     C                   if        prcSingle
     C                   leave
     C                   endif                                                  prcSingle
     c     emkey         reade     pem02
     c                   enddo
      *** end add ***** 08/27/08 *********************************************
     C                   endsr

      *------------------------------------------------------------------------*
      *  ProcEmpBrk - this subroutine processes on employer break
      *------------------------------------------------------------------------*
     C     ProcEmpBrk    begsr
     C**** End Add ********* 03/13/08 ****************************************

     C**** Beg Del ********* 03/13/08 ****************************************
     C****************   exsr      ProcClmRec

     C*****emkey2        reade     pem02                                  60
     C*****************  enddo
     C**** End Del ********* 03/13/08 ****************************************

     C                   movel(p)  esmcds        psmcds
     C                   move      '*'           psmcds
     C                   movel(p)  ermcds        prmcds
     C                   move      '*'           prmcds

      * call program to submit requests for images
     C**** Beg Chg ********* 03/13/08 ****************************************
     C**************     callp     ImagePrint(eCompany:eGroup:eemployer:
     C**************               eDiv:ePsid:efdat:etdat:eotyp:
     C**************               psmcds:prmcds)
     C**** beg add ***** 08/27/08 ********************************************
     C                   move      ememp#        prvEmp#
     C                   move      emdiv         prvDiv
     C**** end add ***** 08/27/08 ********************************************
     C                   move      prvEmp#       prvEmpc
     C                   move      prvDiv        prvDivc
     C                   callp     ImagePrint(eCompany:eGroup:prvEmpc:
     C                             prvDivc:ePsid:efdat:etdat:eotyp:
     C                             psmcds:prmcds)
     C**** End Chg ********* 03/13/08 ****************************************

      * write a remark indicating that open enrollment was done
      * call api to get employer key
     C                   eval      wcin = 'Y'
     C                   eval      wdck = 'Y'
     C                   eval      returnCode = 'K'
     C                   clear                   mcKey

     ********* MUST GET EMPLOYER KEY HERE into mckey for Dart remark


     C**** Beg Chg ********* 03/13/08 ****************************************
     C*****************  move      ememp#        clmEmp#           9
     C*****************  move      emdiv         clmDiv            5
     C                   move      prvEmp#       clmEmp#           9
     C                   move      prvDiv        clmDiv            5
     C**** End Chg ********* 03/13/08 ****************************************
     C                   callp     EmpKeyAPI(returnCode:
     C                                       eCompany:eGroup:
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
      *  ProcClmRec - this subroutine is called to do the processing associated
      *               with each claim record iterated.
      *------------------------------------------------------------------------*
     C     ProcClmRec    begsr

     C                   eval      Div     = emdiv
     C                   eval      division = emdiv
     C                   eval      emp# = ememp#
     C                   eval      employer = ememp#
     C                   move      ememp#        pEmp#
     C                   move      emdiv         pDiv

     C                   eval      wcin = 'Y'
     C                   eval      wdck = 'Y'
     C                   eval      returnCode = 'K'
     C                   eval      mcKey = *blanks

     C* MUST GET mcKey filled with the Employer Key (co/fnd/emp#/Div)
     C* call api to get key

     C                   callp     EmpKeyAPI(returnCode:
     C                                       eCompany:eGroup:
     C                                       pEmp#:pDiv:mcKey)

     C* print Employer/Policy Cover Page

     C                   eval      pType = 'C'
     C                   callp     CoverPrint(pType:eCompany:eGroup:
     C                                       pemp#:pDiv:efdat:
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
      * "SB Policy Output Log" directory, for each DARTS key.

     c                   eval      sKey = mcKey
     c                   call      'SC966C2'     WRTTXT

      ***** END  ADD  03/13/06 *********************************************

     C* added code to write a policy Remark ***

     C                   move      dateIn        pRmkDat

     C                   in        pRmkCode
     C                   if        pRmkCode <> ' '
     C                   callp     WriteRmk(mcKey:pSeq:pRemark:
     C                               pRmkDat:pRmkCode:q1pgm:q1user:
     C                               'W0050R':'N':'N':'Y':'N')
     C                   endif

     C                   endsr


     C***  Begin Add  ***  03/10/09  **************************************
      *------------------------------------------------------------------------*
      * Determine if restrictions exist
      *------------------------------------------------------------------------*
     PGetRestrict      b
     DGetRestrict      pi             1n
     D inCo#                          3  0 Const
     D inUser                        10    const
     D inAppl                         1    const

     D ##Co#           s              3  0
     D ##User          s             10
     D ##Appl          s              1
     D ##RtnCd         s              1n

     C                   call      'SA008R'
     C                   parm      inCo#         ##Co#
     C                   parm      inUser        ##User
     C                   parm      inAppl        ##Appl
     C                   parm                    ##RtnCd

     C                   return    ##RtnCd

     PGetRestrict      e


      *------------------------------------------------------------------------*
      * Determine if user is authorized to record
      *------------------------------------------------------------------------*
     PRestricted       b
     DRestricted       pi             1n
     D inCo#                          3  0 const
     D inUser                        10    const
     D inAppl                         1    const
     D inGrp                          3  0 const
     D inEmp#                         9  0 const options(*nopass)
     D inDiv                          5  0 const options(*nopass)

     D ##Co#           s              3  0
     D ##User          s             10
     D ##Appl          s              1
     D ##RtnCd         s              1n
     D ##Auth          s              1
     D ##Grp           s              3  0
     D ##Emp#          s              9  0

     C     authGrp       plist
     C                   parm      inCo#         ##Co#
     C                   parm      inUser        ##User
     C                   parm      inAppl        ##Appl
     C                   parm                    ##RtnCd
     C                   parm                    ##Auth
     C                   parm      inGrp         ##Grp

     C     authEmp#      plist
     C                   parm      inCo#         ##Co#
     C                   parm      inUser        ##User
     C                   parm      inAppl        ##Appl
     C                   parm                    ##RtnCd
     C                   parm                    ##Auth
     C                   parm      inGrp         ##Grp
     C                   parm      inEmp#        ##Emp#

     C                   eval      ##RtnCd = *off
     C                   select
     C                   when      UPXFND = 'N' and
     C                             (%parms < 5 or inEmp# = 0)
     C                   eval      ##RtnCd = *off
     C                   when      UPXEMP = 'N' and
     C                             (%parms >= 5 and inEmp# <> 0)
     C                   eval      ##RtnCd = *off
     C                   when      UPXFND = 'Y' and not restrictW
     C                   if        inGrp <> group
     C                   eval      ##RtnCd = *on
     C                   endif
     C                   when      %parms >= 5 and inEmp# <> *zero and
     C                             UPXEMP = 'Y' and not restrictW
     C                   select
     C                   when      UPFL5 = 'Y' and
     C                             (inEmp# <> UPEMP# or inDiv <> UPDIV)
     C                   eval      ##RtnCd = *on
     C                   when      inEmp# <> UPEMP#
     C                   eval      ##RtnCd = *on
     C                   endsl
     C                   when      %parms >= 5 and inEmp# <> *zero
     C                   call      'SA008R'      authEmp#
     C                   other
     C                   call      'SA008R'      authGrp
     C                   endsl

     C                   return    ##RtnCd

     PRestricted       e
     C***  End   Add  ***  03/10/09  **************************************
