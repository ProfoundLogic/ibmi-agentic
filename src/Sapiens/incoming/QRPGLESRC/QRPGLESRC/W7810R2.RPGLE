     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
    H/TITLE - Prompt - Renew Waivers
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
    H*********************************************************************
    H*  DESCRIPTION:  Runs prompt screen and passes parameters to        *
    H*  Waiver of Subrogation and Designated Excluded Workplace          *
    H*  Rollover program.                                                *
    H*                                                                   *
    H*  CALLED FROM: Menu                                                *
    H*                                                                   *
    H*  CALLS TO: W7810C2                                                *
    H*                                                                   *
    H*  CREATED: 10/22/08      C:136     PROJECT: 46WFY                  *
    H*********************************************************************
    H*  REVISIONS:                                                       *
     H*                                                                   *
     H* 02/19/19 C:180 133WQY-WC2653 : Remove client-id condition and     *
     H*                                make it base (WC2653)              *
     H*                                                                   *
     H* 11/13/18 C:180 133WQY Add forms for Manual Rollover:              *
     H*                       - New forms WC990302 and WC420601           *
     H*                       - Roll all endorsements flagged as a waiver *
     H*                         type endorsement (x-ref 133WQP).
     H* 04/27/09 C:189 133WGG                                             *
     H*                    Update the Manual Endorsement Rollover process *
     H*                    to include option to roll Alternate Employer   *
     H*                    Endorsement.                                   *
     H*                                                                   *
    H*                                                                   *
    H*********************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     FW7810F2   cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     Fwmemp     if   e             disk    rename(pem01:pem0p)
     D/COPY SCOPYPR01D
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     d optnm           c                   const('Rollover Waivers and Excluded-
     d                                      Workplaces')
     D comapi          c                   const('WA001R')
     D @@call          s              1
     D @@prds          s             40
     D xxcall          s                   like(@@call)
     D ##co#           s              3  0
     D ##fnam          s             40
     D ##fnd           s              3  0
     D error           s              1
     D EMrrn           s              9  0
     D yrDiff          s              5  0
     D frdt            s               d
     D todt            s               d
      *** Begin Add *** 11/13/18 ************************************
     d cltID           s              3a   Inz(*Blanks)
      *** Ebd   Add *** 11/13/18 ************************************
    D*---------------------------------------------------------------------
    D* Procedure Prototypes
    D*---------------------------------------------------------------------
     DS0006R           PR                  extpgm('S0006R')
     D environment                    1    const
     D/copy sprstring
     D/copy sprdate
     D/copy wpremplr
     D/copy wprgroup

      *** Begin Add *** 11/13/18 ************************************
     D GetCltId        PR                  Extpgm('RTVCLTID')
     d                                3a
      *** End   Add *** 11/13/18 ************************************
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@prds
     C                   parm                    @@call
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C/COPY SCOPYPR01C
    C*====================================================================
    C* DSCAPI - Call API for getting descriptions (use 91 on error)
    C*====================================================================
     C     DSCAPI        begsr
     C                   call      comapi        compl                  91
     C                   endsr
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
     C     ERRCK1        begsr

     C                   eval      scfrdt = dtCheck(scfrdt)
     C  n90              if        scfrdt  = *zero
     C                   seton                                        71  90
     C                   endif

     C                   eval      sctodt = dtCheck(sctodt)
     C  n90              if        sctodt = *zero
     C                   seton                                        72  90
     C                   endif

     c                   eval      frdt = cvtToDate(scfrdt)
     c                   eval      todt = cvtToDate(sctodt)
     C  n90              if        todt  < frdt
     C                   seton                                        73  90
     C                   endif

     c                   if        not *in90
     c                   eval      yrDiff = %diff(todt : frdt : *years)
     c                   if        yrDiff >= 1
     C                   seton                                        77  90
     c                   endif
     c                   endif

     c  n90              if        scemp# <> 0 and
     c                             validEmp(scEmp#) = *off
     c                   seton                                        74  90
     c                   endif

     c  n90              if        scmwsb <> 'Y' and scmwsb <> ' '
     C                   seton                                        75  90
     c                   endif

     c  n90              if        scmdew <> 'Y' and scmdew <> ' '
     C                   seton                                        76  90
     c                   endif


     c*** begin add ***  04/27/09  *******************************************
     c  n90              if        scmaee <> 'Y' and scmaee <> ' '
     C                   seton                                        78  90
     c                   endif

     c*** end  add ***   04/27/09  *******************************************
     C                   endsr
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
     C     KJDO          begsr

     c                   select
     C                   when      hfld = 'SCEMP#'
     C                   eval      EMrrn = EmpSrch(*inkc:*inkl:error:##FND)
     C     EMrrn         chain     pem0p
     C                   if        EMrrn <> *zero and %found
     C                   eval      scemp# = EMEMP#
     C                   seton                                        82  90
     C                   else
     C                   seton                                        81  90
     c                   setoff                                       kl
     C                   endif
     c                   endsl

     C                   endsr
    C*====================================================================
    C* KMDO - Processing for F13
    C*====================================================================
     C     KMDO          begsr
     C                   callp      S0006R('W')
     C                   exsr      getcof
     c                   eval      scfnd = ##fnd
     c                   eval      scfdsc = GetGrpName(##fnd)
     C                   endsr
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
     C     OTHFKS        begsr
     C     *inkj         caseq     *on           kjdo                     90
     C     *inkm         caseq     *on           kmdo                     90
     C                   endcs
     C                   endsr
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
     C     PGMEND        begsr
     c                   seton                                        lr
     C                   endsr
    C*====================================================================
    C* PGMSTR - Used for any start of program processing (one-time)
    C*====================================================================
     C     PGMSTR        begsr
    
     C                   if        %parms >= 1
     C                             and @@prds <> *blanks
     C                   movel(p)  @@prds        ##data
     C                   else
     C                   movel(p)  optnm         ##data
     C                   endif
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        scprds
     C                   if        %parms >= 2
     C                   movel     @@call        xxcall
     C                   else
     C                   clear                   xxcall
     C                   endif
    
     c                   eval      scfnd = ##fnd
     c                   eval      scfdsc = GetGrpName(##fnd)
      /Free
       //*** Begin Add *** 11/13/18 **********************************
        GetCltID(cltID);
       //*** Begin Delete  02/19/19 **********************************
       // If cltId = '133';
       //*** End   Delete  02/19/19 **********************************
           isLuba = *On;
           disTXNMC = *Off;
           disNCTP = *Off;
       //*** Begin Delete  02/19/19 **********************************
       //Else;
       //  isLuba = *Off;
       //  disTXNMC = *On;
       //  disNCTP = *On;
       // Endif;
       //*** End   Delete  02/19/19 **********************************
       //*** End   Add *** 11/13/18 **********************************
      /End-Free
    
     c                   endsr
    C*====================================================================
    C* PREDSP - Pre-processing before displaying (every time)
    C*====================================================================
     C     PREDSP        begsr
     C                   endsr
    C*====================================================================
    C* PRSCTL - Process control after no errors found (Call Submitter)
    C*====================================================================
     C     PRSCTL        begsr
    
     c                   move      ##co#         @@co#             3
     c                   move      ##fnd         @@fnd             3
     c                   move      scemp#        @@emp#            9
     c                   move      scfrdt        @@frdt            8
     c                   move      sctodt        @@todt            8
     c                   call      'W7810C2'
     c                   parm                    @@co#
     c                   parm                    @@fnd
     c                   parm                    @@emp#
     c                   parm                    @@frdt
     c                   parm                    @@todt
     c                   parm      scmwsb        @@mwsb            1
     c                   parm      scmdew        @@mdew            1
     c*** begin add ***  04/27/09  *******************************************
     c                   parm      scmaee        @@mdee            1
     c*** end   add ***  04/27/09  *******************************************
     c*** begin add ***  11/13/18  *******************************************
     c                   parm      scTXNMC       @@TXNMC           1
     c                   parm      scNCTP        @@NCTP            1
     c*** end   add ***  11/13/18  *******************************************
    
     C                   endsr
