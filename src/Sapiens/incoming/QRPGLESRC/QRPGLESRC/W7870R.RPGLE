     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H bnddir('WTBNDDIR')
    H*********************************************************************
    H/title Agent Activity Report
    H*********************************************************************
    H*                                                                   *
    H*  CALLED FROM: Menu                                                *
    H*                                                                   *
    H*  CALLS TO: W7870C - Submitter                                     *
    H*                                                                   *
    H*  CREATED:   11/08/02      C:61      PROJECT: WC1073               *
    H*  Converted: 10/20/03      C:33      Project: WC1017               *
    H*********************************************************************
    H* REVSIONS:                                                         *
    H*                                                                   *
     H* 01/09/17 C:306 Project: WC2441                                    *
     H*                 - Added the new parameter in Calling of W7870C    *
     H*                 program.                                          *
     H*                                                                   *
    H* 06/15/16 C:136 Support: 42041                                     *
    H*                Fix group restriction                              *
    H*                                                                   *
     H* 04/26/10 C:248 SUPPORT                                            *
     H*                AGENT ACTIVITY REPORT.                             *
     H*                THE AGENT SORT Y/N NEEDS TO BE CHANGED TO READ     *
     H*                "PAGE BREAK BY AGENT" Y OR N.                      *
     H*                Recompiled for the display file change W7870F      *
     H*                                                                   *
    H* 06/01/09 C:62  Project: 133WGX                                    *
    H*                Add employer type selection.                       *
    H*                                                                   *
    H* 05/27/09 C:62  Project: 133WGW                                    *
    H*                Add primary sort.                                  *
    H*                                                                   *
    H* 01/23/08 C:33  Seq#: WA114/61899                                  *
    H*                Allow for group range to be entered if restriction *
    H*                 flag is on and no default group is available.     *
    H*                                                                   *
    H* 08/26/05 C:33  Seq#: WA133/48605                                  *
    H*                Allow for Mod to be blank.                         *
    H*                                                                   *
    H* 05/17/04 C:61  Support WA129 42544                                *
    H*                                                                   *
    H* 10/10/03 C:33  Project: WC1017                                    *
    H*                Add new quote types                                *
    H*                                                                   *
    H*  03/03/03 C:53 Support: WC065 # 36122                             *
    H*                Add group restriction to pgm.                      *
    H*                                                                   *
    H* 05/14/03 C:61  PROJECT WC577                                      *
    H*                If list all modifiers for agency if modifier       *
    H*                                                                   *
    H*********************************************************************
     Fw7870f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF

     Fwmfnp     if   e             disk    rename(pfn01:pfn0p)
     Fwmaap     if   e             disk    rename(paa01:paa0p)
     Fwmagp     if   e             disk    rename(pag01:pag0p)
      *** beg add ***** 06/15/16 *****************************
     Fsmazl     if   e           k disk    rename(paz01:paz00)
     D hasOtherGrps    s               n
      *** end add ***** 06/15/16 *****************************
    
     D*entry
     D W7870R          pr
     D  @@prds                       40
     D  @@call                        1
     D W7870R          pi
     D  @@prds                       40
     D  @@call                        1
    
     D ChgUserParms    pr                  extpgm('W0006R')

     D Co#FndApi       pr                  extpgm('WA001R')
     D  ##co#                         3  0
     D  ##fnd                         3  0
     D  ##conm                       40
     D  ##fnam                       40
     D  ##Lvl                         3  0
     D  ##Type                        1
     D  ##OutQ                       10
     D  ##XCo#                        1
     D  ##XFnd                        1

     D UpdateMbr       pr                  extpgm('UPDMBR')
     D  ##Mode                        1    const
     D  ##File                       10    const
     D  ##Mbr                        10

     D SubmitPgm       pr                  extpgm('W7870C')
     D  ##co#                         3    const
     D  ##FrFnd                       3    const
     D  ##ToFnd                       3    const
     D  ##FrDt                        8    const
     D  ##ToDt                        8    const
     D  ##DateType                         const like(scDTyp)
     D  ##PQB                         1    const
     D  ##AgtSort                          const like(scASrt)
     D  ##Fein                        9    const
     D  ##Mod                         3    const
     D  ##PFB                              const like(scPFB )
     D  ##DSB                              const like(scDSB )
     D  ##OutQ                       10    const
     D  ##Mbr                        10    const
     D*** BEGIN ADD *** 05/27/09 *******************************************
     D  ##PSRT                             const like(scPSrt)
     D*** END   ADD *** 05/27/09 *******************************************
     D*** BEGIN ADD *** 06/01/09 *******************************************
     D  ##FQFL                             const like(scfqfl)
     D*** END   ADD *** 06/01/09 *******************************************
     D*** Begin Add *** 01/09/17 *******************************************
     d  ##GrpSlct                     1    const
     D*** End Add   *** 01/09/17 *******************************************

     D GetSuffix       pr                  extpgm('SA030R')
     D  ##suffix                      3

     D ChkAgent        pr                  like(*in90)
     D ChkDates        pr                  like(*in90)
     D ChkGroups       pr                  like(*in90)
     D ChkPQB          pr                  like(*in90)
     D ChkPrtFlgs      pr                  like(*in90)
     D*** BEGIN ADD *** 05/27/09 *******************************************
     D ChkPrmSort      pr                  like(*in90)
     D*** END   ADD *** 05/27/09 *******************************************
     D*** BEGIN ADD *** 06/01/09 *******************************************
     D ChkFQFlag       pr                  like(*in90)
     D*** END   ADD *** 06/01/09 *******************************************
     D CreateMbrs      pr
     D GetQteTyps      pr                  like(*in90)
     D OverrideMbrs    pr
     D RemoveMbrs      pr

     D SearchFein      pr                  like(scFein)
     D  fein                               like(scFein)
     D***  Begin Chg  ***  08/26/05  **************************************
     D**mod*****                           like(scMod)
     D  mod                                like(AAMOD)
     D***  End   Chg  ***  08/26/05  **************************************

     D SearchGrp       pr                  like(scFFnd)
     D  group                              like(scFFnd)

     D/copy sPrDate
     D/copy sPrString
     D/copy wPrUsrPrms
     D/copy wPrGroup
     D/copy wPrAgent

     D/copy sCopyPR01d

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optName         c                   const('Agent Activity Report')
     D xxcall          s                   like(@@call)
     D ##Co#           s              3  0
     D ##Fnd           s              3  0
     D ##FndNM         s             40
     D ##Lvl           s              3  0
     D ##Mbr           s             10
     D ##OutQ          s             10
     D ##PQB           s              1
     D ##suffix        s              3
     D ##type          s              1
     D ##xco#          s              1
     D ##xfnd          s              1
     ******* beg add ******** 05/17/04 ***********************************
     D xxtmp1          s              6
     ******* end add ******** 05/17/04 ***********************************
     D***  Begin Add  ***  08/26/05  **************************************
     D xxMod           s                   like(AAMOD)
     D saveScMod       s                   like(scMod) inz(*hival)
     D***  End   Add  ***  08/26/05  **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C/copy sCopyPr01c
    C*
    C*====================================================================
    C* DSCAPI - Call API for getting descriptions (use 91 on error)
    C*====================================================================
    C*
     C     DSCAPI        begsr
    C*
     C                   callp     Co#FndApi(##co#:##Fnd:##conm:##FndNm:
     C                                       ##lvl:##type:##OutQ:##xco#:
     C                                       ##xfnd)
     C                   eval      *in91 = *off
    C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
    C*
     C     ERRCK1        begsr
     C*
     C  n90              eval      *in90 = ChkGroups
     C  n90              eval      *in90 = ChkDates
     C*** BEGIN ADD *** 05/27/09 *******************************************
     C  n90              eval      *in90 = ChkPrmSort
     C*** END   ADD *** 05/27/09 *******************************************
     C  n90              eval      *in90 = ChkAgent
     C*** BEGIN ADD *** 06/01/09 *******************************************
     C  n90              eval      *in90 = ChkFQFlag
     C*** END   ADD *** 06/01/09 *******************************************
     C  n90              eval      *in90 = ChkPrtFlgs
     C*
     C  n90              eval      *in90 = GetQteTyps
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
    C*
     C     KJDO          begsr
    C*
     C                   select
     C                   when      hfld = 'SCFFND'
     C                   eval      scFFnd = SearchGrp(scFFnd)
     C                   when      hfld = 'SCTFND'
     C                   eval      scTFnd = SearchGrp(scTFnd)
     C                   when      hfld = 'SCFEIN' or hfld = 'SCMOD'
     C***  Begin Chg  ***  08/26/05  **************************************
     C**********         eval      scFein = SearchFein(scFein:scMod)
     C                   eval      scFein = SearchFein(scFein:xxMod)
     C***  End   Chg  ***  08/26/05  **************************************
     C                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C* KMDO   - Processing for F13
    C*====================================================================
    C*
     C     KMDO          begsr
    C*
     C                   callp     ChgUserParms
     C                   exsr      getCoF
    C*
     C                   endsr
    C*
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
    C*
     C     OTHFKS        begsr
    C*
     C     *inkj         caseq     *on           kjdo                     90
     C     *inkm         caseq     *on           kmdo                     90
     C                   endcs
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
    C*
     C     PGMEND        begsr
    C*
     C                   if        (*inkc or *inkl) and ##Mbr <> *blank
     C                   callp     RemoveMbrs
     C                   endif
     C*
     C                   eval      *inlr = *on
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PGMSTR - Used for any start of program processing (one-time)
    C*====================================================================
    C*
     C     PGMSTR        begsr
    
     C                   if        %parms >= 1 and @@prds <> *blanks
     C                   eval      scPrds = CenterFld(@@prds:%size(scPrds))
     C                   else
     C                   eval      scPrds = CenterFld(optName:%size(scPrds))
     C                   endif
    
     C                   if        %parms >= 2
     C                   movel     @@call        xxcall
     C                   else
     C                   clear                   xxcall
     C                   endif

     C                   callp     GetSuffix(##suffix)
     C                   eval      scRptLine = 'PDSRPT' + %trim(##suffix) +
     C                                         ', if selected.'
     /Free
       // Initialising the checkbox value.
       scASRT = 'N';
      /End-Free
     C                   endsr
    C*
    C*====================================================================
    C* PREDSP - Pre-processing before displaying (every time)
    C*====================================================================
    C*
     C     PREDSP        begsr
    C*
    C* Restrict group?
    C*
      *** beg chg ***** 06/15/16 *********************
     C***********        if        ##xFnd = 'Y'
     C*****Begin*Add  ***  01/23/08  **************************************
     C***********                   and ##Fnd <> *zero
     C*****End***Add  ***  01/23/08  **************************************
     C***********        eval      *in12 = *on
     C***********        eval      scFFnd = ##Fnd
     C***********        eval      scTFnd = ##Fnd
     C***********        endif
      /free
       *in12 = *off;
       hasOtherGrps = *off;
       Setll (##co#:Q1user) paz00;
       Reade (##co#:Q1user) paz00;
       Dow not(%eof);
          If AZfnd <> ##Fnd;
             hasOtherGrps = *on;
             Leave;
          Endif;
       Reade (##co#:Q1user) paz00;
       Enddo;
       If ##xFnd = 'Y' and ##Fnd <> *zero and not(hasOtherGrps);
          scFFnd = ##Fnd;
          scTFnd = ##Fnd;
          *in12 = *on;
       Endif;
      /end-free
      *** end chg ***** 06/15/16 *********************
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PRSCTL - Process control after no errors found (Call Submitter)
    C*====================================================================
    C*
     C     PRSCTL        begsr
     C*
     C                   if        scTFnd = *zero
     C                   eval      scTFnd = *hival
     C                   endif
     C*
     C****BEGIN*CHG *** 05/27/09 *******************************************
     C**********         callp     SubmitPgm(%editc(##co#:'Z'):
     C**********                             %editc(scFFnd:'Z'):
     C**********                             %editc(scTFnd:'Z'):
     C**********                             %editc(scFrDt:'Z'):
     C**********                             %editc(scToDt:'Z'):
     C**********                             scDTyp:
     C**********                             ##PQB:
     C**********                             scASrt:
     C**********                             %editc(scFein:'Z'):
     C*****Begin Chg  ***  08/26/05  **************************************
     C**********                             %editc(scMod:'Z'):
     C**********                             scMod:
     C*****End** Chg  ***  08/26/05  **************************************
     C**********                             scPFB:
     C**********                             scDSB:
     C**********                             ##OutQ:
     C**********                             ##Mbr
     C**********                            )
     C*** BEGIN CHG *** 06/01/09 *******************************************
     C**********         callp     SubmitPgm(%editc(##co#:'Z'):
     C**********                             %editc(scFFnd:'Z'):
     C**********                             %editc(scTFnd:'Z'):
     C**********                             %editc(scFrDt:'Z'):
     C**********                             %editc(scToDt:'Z'):
     C**********                             scDTyp:
     C**********                             ##PQB:
     C**********                             scASrt:
     C**********                             %editc(scFein:'Z'):
     C**********                             scMod:
     C**********                             scPFB:
     C**********                             scDSB:
     C**********                             ##OutQ:
     C**********                             ##Mbr:
     C**********                             scpsrt
     C**********                            )
     C*** Begin Chg *** 01/09/17 *******************************************
     C***                callp     SubmitPgm(%editc(##co#:'Z'):
     C***                                    %editc(scFFnd:'Z'):
     C***                                    %editc(scTFnd:'Z'):
     C***                                    %editc(scFrDt:'Z'):
     C***                                    %editc(scToDt:'Z'):
     C***                                    scDTyp:
     C***                                    ##PQB:
     C***                                    scASrt:
     C***                                    %editc(scFein:'Z'):
     C***                                    scMod:
     C***                                    scPFB:
     C***                                    scDSB:
     C***                                    ##OutQ:
     C***                                    ##Mbr:
     C***                                    scpsrt:
     C***                                    scfqfl
     C***                                   )
     C
     C                   callp     SubmitPgm(%editc(##co#:'Z'):
     C                                       %editc(scFFnd:'Z'):
     C                                       %editc(scTFnd:'Z'):
     C                                       %editc(scFrDt:'Z'):
     C                                       %editc(scToDt:'Z'):
     C                                       scDTyp:
     C                                       ##PQB:
     C                                       scASrt:
     C                                       %editc(scFein:'Z'):
     C                                       scMod:
     C                                       scPFB:
     C                                       scDSB:
     C                                       ##OutQ:
     C                                       ##Mbr:
     C                                       scpsrt:
     C                                       scfqfl:'N'
     C                                      )
     C*** End   Chg *** 01/09/17 *******************************************
     C*** END   CHG *** 06/01/09 *******************************************
     C*** END   CHG *** 05/27/09 *******************************************
     C*
     C                   endsr
    C*____________________________________________________________________
    C*






    C*====================================================================
    C* ChkAgent - Check the agent fields (scFein/scMod & scASrt)
    C*====================================================================
     PChkAgent         b

     D ChkAgent        pi                  like(*in90)

     D***  Begin Add  ***  08/26/05  **************************************
     D xxMod           s                   like(AAMOD)

     C                   eval      scMod = RightJstfy(scMod)
     C                   move      scMod         xxMod
     C***  End   Add  ***  08/26/05  **************************************

     C                   select
     C                   when          scASrt <> 'Y'
     C                             and scASrt <> 'N'
     C                   eval      *in78 = *on
     C                   return    *on
     C***  Begin Chg  ***  08/26/05  **************************************
     C**********         when      scMod <> *zero
    C**********                   and not ValidFein(scFein:scMod)
     C                   when      scMod <> *blanks
    C                              and not ValidFein(scFein:xxMod)
     C***  End   Chg  ***  08/26/05  **************************************
     C                   eval      *in79 = *on
     C                   return    *on
     C                   when      scFein <> *zero
    C                             and not ValidFein(scFein)
     C                   eval      *in79 = *on
     C                   return    *on
     C***  Begin Add  ***  08/26/05  **************************************
     C                   when      scMod = *blanks and scFein <> *zero
     C                              and scMod <> saveScMod
     C                   eval      saveScMod = scMod
     C                   eval      *in41 = *on
     C                   return    *on
     C***  End   Add  ***  08/26/05  **************************************
     C                   endsl

     C                   return    *off

     PChkAgent         e






    C*====================================================================
    C* ChkDates - Check the date fields (scFrDt, scToDt & scDTyp)
    C*====================================================================
     PChkDates         b

     D ChkDates        pi                  like(*in90)

     C                   select
     C                   when      DtCheck(scFrDt) = *zero
     C                   eval      *in74 = *on
     C                   return    *on
     C                   when      DtCheck(scToDt) = *zero
     C                   eval      *in75 = *on
     C                   return    *on
     C                   when      CvtToDate(scToDt) < CvtToDate(scFrDt)
     C                   eval      *in76 = *on
     C                   return    *on
     C                   when          scDTyp <> 'T'
     C                             and scDTyp <> 'E'
     C                             and scDTyp <> 'O'
     C                             and scDTyp <> 'N'
     C                   eval      *in77 = *on
     C                   return    *on
     C                   endsl

     C                   eval      scFrDt = DtCheck(scFrDt)
     C                   eval      scToDt = DtCheck(scToDt)

     C                   return    *off

     PChkDates         e





    C*====================================================================
    C* ChkGroups - Check the groups
    C*====================================================================
     PChkGroups        b

     D ChkGroups       pi                  like(*in90)

     C                   select
      *** beg chg ***** 06/15/16 ******************************
     C***********        when      scFFnd <> *zero
     C***********                  and ValidGrp(scFFnd) = *off
     C***********        eval      *in71 = *on
     C***********        return    *on
     C***********        when      scTFnd <> *zero
     C***********                  and ValidGrp(scTFnd) = *off
     C***********        eval      *in72 = *on
     C***********        return    *on
     c                   when      scFFnd = scTFnd
     C                             and ValidGrp(scFFnd) = *off
     C                   eval      *in71 = *on
     C                   eval      *in72 = *on
     C                   return    *on
      *** end chg ***** 06/15/16 ******************************
     C                   when      scFFnd > scTFnd
     C                   eval      *in73 = *on
     C                   return    *on
     C                   endsl

     C                   return    *off

     PChkGroups        e





     C*** BEGIN ADD *** 05/27/09 *******************************************
    C*====================================================================
    C* ChkPrmSort - Check Primary Sort Values
    C*====================================================================
     PChkPrmSort       b

     D ChkPrmSort      pi                  like(*in90)

     C                   if        scpsrt<>'AA' and scpsrt<>'AN'
     C                   if        scasrt='N' and scpsrt=*blanks
     C                   eval      scpsrt='AN'
     C                   else
     C                   eval      *in68 = *on
     C                   return    *on
     C                   endif
     C                   endif

     C                   return    *off

     PChkPrmSort       e





     C*** END   ADD *** 05/27/09 *******************************************
     C*** BEGIN ADD *** 06/01/09 *******************************************
    C*====================================================================
    C* ChkFQFlag - Check FQ Flag
    C*====================================================================
     PChkFQFlag        b

     D ChkFQFlag       pi                  like(*in90)

     C                   if        scfqfl<>'F' and scfqfl<>'W'
     C                             and scfqfl<>'B'
     C                   eval      *in67 = *on
     C                   return    *on
     C                   endif

     C                   return    *off

     PChkFQFlag        e





     C*** END   ADD *** 06/01/09 *******************************************
    C*====================================================================
    C* ChkPrtFlgs - Check the print flags (scPFB & scDSB)
    C*====================================================================
     PChkPrtFlgs       b

     D ChkPrtFlgs      pi                  like(*in90)

     C                   select
     C                   when          scPFB <> 'P'
     C                             and scPFB <> 'F'
     C                             and scPFB <> 'B'
     C                   eval      *in70 = *on
     C                   return    *on
     C                   when          scDSB <> 'D'
     C                             and scDSB <> 'S'
     C                             and scDSB <> 'B'
     C                   eval      *in69 = *on
     C                   return    *on
     C                   endsl

     C                   return    *off

     PChkPrtFlgs       e





    C*====================================================================
    C* CreateMbrs - Create the members for WXQ2*
    C*====================================================================
     PCreateMbrs       b

     D CreateMbrs      pi

     D ##kc            s                   like(*inkc)
     D ##kl            s                   like(*inkl)

     ******* beg del ******** 05/17/04 ***********************************
     C***********        eval      ##Mbr = q1user
     ******* end del ******** 05/17/04 ***********************************
     ******* beg add ******** 05/17/04 ***********************************
    
     C                   movel(p)  q1user        xxtmp1
     C                   eval      ##Mbr = xxtmp1
    
     ******* end add ******** 05/17/04 ***********************************
     C                   callp     UpdateMbr('C':'WXQ2P':##Mbr)

     PCreateMbrs       e





    C*====================================================================
    C* GetQteTyps - Get the quote types from W/W
    C*====================================================================
     PGetQteTyps       b

     D GetQteTyps      pi                  like(*in90)

     D QteTypePgm      pr                  extpgm('W7870C1')
     D*QteTypePgm      pr                  extpgm('W7870R1')
     D  ##Mode                        1    const
     D  ##PQB                         1
     D  ##Mbr                        10
     D  ##kc                               like(*inkc)
     D  ##kl                               like(*inkl)

     D  ##kl           s                   like(*inkl)

     C                   if        ##Mbr = *blank
     C                   callp     CreateMbrs
     C                   endif

     C                   callp     QteTypePgm(' ':##PQB:##Mbr:*inkc:##kl)

     C                   return    *inkc or ##kl

     PGetQteTyps       e





    C*====================================================================
    C* OverrideMbrs - Override the new members for WXQ2*
    C*====================================================================
     POverrideMbrs     b

     D OverrideMbrs    pi

     C                   callp     UpdateMbr('O':'WXQ2L':##Mbr)
     C                   callp     UpdateMbr('O':'WXQ2L1':##Mbr)
     C                   callp     UpdateMbr('O':'WXQ2L2':##Mbr)
     C                   callp     UpdateMbr('O':'WXQ2P':##Mbr)

     C                   return

     POverrideMbrs     e





    C*====================================================================
    C* RemoveMbrs - Remove the new members for WXQ2*
    C*====================================================================
     PRemoveMbrs       b

     D RemoveMbrs      pi

     C                   callp     UpdateMbr('R':'WXQ2P':##Mbr)

     C                   return

     PRemoveMbrs       e





    C*====================================================================
    C* SearchFein - Process the agent FEIN search
    C*====================================================================
     PSearchFein       b

     D SearchFein      pi                  like(scFein)
     D  fein                               like(scFein)
     D***  Begin Chg  ***  08/26/05  **************************************
     D**mod*****                           like(scMod)
     D  mod                                like(AAMOD)
     D***  End   Chg  ***  08/26/05  **************************************

     D AArrn           s              9  0
     D AGrrn           s              9  0
     D error           s              1
     D inKL            s                   like(*inKL)

     C                   eval      *in90 = *on

     C                   select
     C                   when      scFFnd = scTFnd and scFFnd <> *zero
     C                   eval      AArrn = AgtSrchAA(*inkc:inkl:
     C                                               error:scFFnd)
     C                   if        AArrn <> *zero
     C     AArrn         chain     WMAAP
     C                   eval      *in80 = *on
     C***  Begin Chg  ***  08/26/05  **************************************
     C**********         eval      scMod = AAMod
     C                   eval      scMod = %editc(AAMOD:'1')
     C***  End   Chg  ***  08/26/05  **************************************
     C                   return    AAFein
     C                   endif
     C                   other
     C                   eval      AGrrn = AgtSrchAG(*inkc:inkl:error)
     C                   if        AGrrn <> *zero
     C     AGrrn         chain     WMAGP
     C                   eval      *in80 = *on
     C***  Begin Chg  ***  08/26/05  **************************************
     C**********         eval      scMod = AGMod
     C                   eval      scMod = %editc(AGMOD:'1')
     C***  End   Chg  ***  08/26/05  **************************************
     C                   return    AGFein
     C                   endif
     C                   endsl

     C                   eval      *in89 = *on
     C                   return    fein

     PSearchFein       e





    C*====================================================================
    C* SearchGrp - Process the group search
    C*====================================================================
     PSearchGrp        b

     D SearchGrp       pi                  like(scFFnd)
     D  inGroup                            like(scFFnd)

     D inKC            s                   like(*inKC)
     D inKL            s                   like(*inKL)
     D group           s                   like(scFFnd)
     D FNrrn           s              9  0

     C                   eval      *in90 = *on

     C                   eval      FNrrn = GrpSrch(*inkc:inkl:group)

     C                   if        FNrrn <> *zero
     C     FNrrn         chain     wmfnp
     C                   select
     C                   when      hfld = 'SCFFND'
     C                   eval      *in82 = *on
     C                   return    FNFND
     C                   when      hfld = 'SCTFND'
     C                   eval      *in83 = *on
     C                   return    FNFND
     C                   endsl
     C                   return    AAAGT#
     C                   endif

     C                   select
     C                   when      hfld = 'SCFFND'
     C                   eval      *in81 = *on
     C                   when      hfld = 'SCTFND'
     C                   eval      *in82 = *on
     C                   endsl

     C                   return    inGroup

     PSearchGrp        e
