    H*  (C) Copyright Progressive Data Solutions, Inc., 2000
    H*********************************************************************
    H/TITLE Madison Data Download Prompt
    H*********************************************************************
    H*  Description: Prompts user for info for Madison data download.    *
    H*                                                                   *
    H*  Called from: Menu                                                *
    H*                                                                   *
    H*  Calls to: W8115C1                                                *
    H*                                                                   *
    H*  Created: 01/18/00      C:87      Project: WC964/33WEV            *
    H**************************************************************************************
    H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  05/01/00  C:87  Project: WC964                                                    *
     H*                  Remove F13=Chg User Parms processing.  Remove references to       *
     H*                  fund and fund name for screen.  Add parms to *entry plist         *
     H*                  for passing back F3 & F12 values.                                 *
     H*                                                                                    *
     H*  02/11/00  C:87  Project: 33WEV                                                    *
     H*                  Base project is installed as a mod for client.                    *
    H*                                                                                    *
    H**************************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw8115f1   cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
     Fssylf     if   e           k disk
    D*
     D/COPY SCOPYPR01D
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optnm           c                   const('Madison Data Download Prompt')
     D comapi          c                   const('WA001R')
     D @@call          s              1
     D @@prds          s             40
     D***  begin add  ***  05/01/00  ***************************************
     D @@kc            s              1
     D @@kl            s              1
     D***  end   add  ***  05/01/00  ***************************************
     D xxcall          s                   like(@@call)
     D ##co#           s              3  0
     D ##fnam          s             40
     D ##fnd           s              3  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYDTPL
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
     C     *entry        plist
     C***  begin add  ***  05/01/00  ***************************************
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C***  end   add  ***  05/01/00  ***************************************
     C                   parm                    @@prds
     C                   parm                    @@call
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C/COPY SCOPYPR01C
    C*
    C*====================================================================
    C* DSCAPI - Call API for getting descriptions (use 91 on error)
    C*====================================================================
    C*
     C     DSCAPI        begsr
    C*
     C                   call      comapi        compl                  91
    C*
     C                   endsr                                                  DSCAPI
    C*
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
    C*
     C     ERRCK1        begsr
    C*
     C                   exsr      othfks
    C*
    C*  Validate 'Load to tape' value
     C  n90              if        scload <> 'Y'
     C                             and scload <> 'N'
     C                   eval      *in71 = *on
     C                   eval      *in90 = *on
     C                   endif
    C*
    C*  Only edit the following if 'Load to tape' flag is 'Y'
     C  n90              if        scload = 'Y'
    C*
    C*    Validate initialize tape flag
     C  n90              if        scinz <> 'Y'
     C                             and scinz <> 'N'
     C                   eval      *in72 = *on
     C                   eval      *in90 = *on
     C                   endif
    C*
    C*    Tape device cannot be blank
     C  n90              if        scdev = *blanks
     C                   eval      *in73 = *on
     C                   eval      *in90 = *on
     C                   endif
    C*
    C*    Validate tape device entered
     C  n90              call      'SA021C'
     C                   parm      ' '           ##rtc             1
     C                   parm                    scdev
    C*
     C  n90              if        ##rtc = 'F'
     C                   eval      *in74 = *on
     C                   eval      *in90 = *on
     C                   endif
    C*
    C*    Volume cannot be blank
     C  n90              if        scvol = *blanks
     C                   eval      *in75 = *on
     C                   eval      *in90 = *on
     C                   endif
    C*
    C*    Density cannot be blank
     C  n90              if        scden = *blanks
     C                   eval      *in76 = *on
     C                   eval      *in90 = *on
     C                   endif
    C*
     C                   endif
    C*
     C                   endsr                                                  ERRCK1
    C*
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
    C*
     C     KJDO          begsr
     C                   endsr                                                  KJDO
    C*
    C*====================================================================
    C* KMDO   - Processing for F13=Change user parms
    C*====================================================================
    C*
     C     KMDO          begsr
    C*
     C***  begin del  ***  05/01/00  ***************************************
     C****************** call      'W0006R'                             99
    C*
     C****************** exsr      getcof
     C***  end   del  ***  05/01/00  ***************************************
    C*
     C                   endsr                                                  KMDO
    C*
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
    C*
     C     OTHFKS        begsr
    C*
     C     *inkj         caseq     *on           kjdo                     90
     C***  begin del  ***  05/01/00  ***************************************
     C**** *inkm         caseq     *on           kmdo                     90
     C***  end   del  ***  05/01/00  ***************************************
     C                   endcs
    C*
     C                   endsr                                                  OTHFKS
    C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
     C     PGMEND        begsr
     C***  begin add  ***  05/01/00  ***************************************
     C                   if        *inkl <> *on
     C                   eval       *inkc=*on
     C                   endif
     C***  end   add  ***  05/01/00  ***************************************
     C                   endsr                                                  PGMEND
    C*====================================================================
    C* PGMSTR - Used for any start of program processing (one-time)
    C*====================================================================
    C*
     C     PGMSTR        begsr
    C*
     C                   if        q1prms >= 1
     C                             and @@prds <> *blanks
     C                   movel(p)  @@prds        ##data
     C                   else
     C                   movel(p)  optnm         ##data
     C                   endif
    C*
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
    C*
     C                   movel     ##data        scprds
    C*
     C                   if        q1prms >= 2
     C                   movel     @@call        xxcall
     C                   else
     C                   clear                   xxcall
     C                   endif
     C*
     C*  Default screen values
     C                   eval      scload = 'N'
     C                   eval      scinz = 'Y'
    C*
     C                   clear                   scdev
     C                   movel     'MADISON'     scvol
     C                   clear                   scden
    C*
    C*    Get the tape initialization values from SDRPF
     C     'WC'          chain     swc01                              60
    C*
     C                   if        *in60 = *off
    C*
     C                   movel     wctdnm        scdev
    C*
     C                   if        wctden <> '6250    '
     C                             and wctden <> '1600    '
     C                   clear                   scden
     C                   movel     '*QIC120 '    scden
     C                   else
     C                   movel     wctden        scden
     C                   endif
    C*
     C                   else
     C*
    C*    If there is no system control file, send message
     C                   move      *on           *in25
    C*
     C                   endif
    C*
     C                   endsr                                                  PGMSTR
    C*
    C*====================================================================
    C* PREDSP - Pre-processing before displaying (every time)
    C*====================================================================
    C*
     C     PREDSP        begsr
     C*
     C***  begin del  ***  05/01/00  ***************************************
     C****************** eval      scfnd = ##fnd
     C****************** eval      scfdnm = ##fnam
     C***  end   del  ***  05/01/00  ***************************************
     C*
     C                   endsr                                                  PREDSP
    C*
    C*====================================================================
    C* PRSCTL - Process control after no errors found (Call Submitter)
    C*====================================================================
    C*
     C     PRSCTL        begsr
    C*
     C                   move      ##co#         qqco#
     C                   move      ##fnd         qqfnd
    C*
     C                   call      'W8115C1'
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm      scload        qqload            1
     C                   parm      scinz         qqinz             1
     C                   parm      scdev         qqdev            10
     C                   parm      scvol         qqvol            10
     C                   parm      scden         qqden            10
    C*
     C                   endsr                                                  PRSCTL
    C*
