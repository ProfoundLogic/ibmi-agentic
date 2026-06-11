    Hcopyright ('(C) Copyright Fiserv, Inc., 2000 - 2005')
    H*****************************************************************
    H/TITLE W0630R90 - EXTENDED PAYROLL CLASS CODE DESCRIPTION
    H*****************************************************************
    H*                                                               *
    H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
    H*                OR ADD RECORDS IN THE PAYROLL CLASS CODE FILE. *
    H*                                                               *
    H*  CALLED FROM:                                                 *
    H*    W0630R - WORK WITH PAYROLL CLASS CODES                     *
    H*                                                               *
    H*  CALLS TO:                                                    *
    H*                                                               *
    H*****************************************************************
    H*  REVISIONS:                                                   *
    H*                                                               *
    H*  07/22/09 C:136 Project: 136WED                               *
    H*                -Client specific change for expanded           *
    H*                 description                                   *
     H*                -Base change to kill edit per GW (Priss 16)    *
    H*                -Populate status field with 'A' in add mode    *
    H*                                                               *
    H*  06/24/05  C:33  Project: 109WDZ                              *
    H*                  {109AU}                                      *
    H*                 -Move from client environment to base.        *
    H*                 -Convert to RPGIV                             *
    H*****************************************************************
     *
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw0630f90  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmprp     uf   e             disk    rename(ppr01:dbrcdf)
     Fwmprpe    uf a e           k disk    prefix(ext)
    D*
     D                 ds
     D fullDesc                            like(extPREXDS)
     D  scexd1                             overlay(fullDesc:1)
     D  scexd2                             overlay(fullDesc:76)
     D  scexd3                             overlay(fullDesc:151)
     D  scexd4                             overlay(fullDesc:226)
    D*
      *** beg add ***** 07/22/09 **************************************
     D fullDesc2                    100
     D  scex51                             overlay(fullDesc2:1)
     D  scex52                             overlay(fullDesc2:51)
      *** end add ***** 07/22/09 **************************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   const('Payroll Class -
     D                                     Information Expanded Description')
     D comapi          c                   const('WA001R')
     D #ofscr          c                   const(1)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s                   like(prco#)
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
     D/copy sprstring
     D/copy sprdate
     D/copy sprcompany
    C*
     C/copy scopymt41c
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     keyWMPRPE     klist
     C                   kfld                    PRCO#
     C                   kfld                    PRRTCD
     C                   kfld                    PRCLAS
     C                   kfld                    PREFDT
    C*
    C*================================================================
    C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
    C*================================================================
     C     chgsts        begsr
     C                   endsr
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
    C*
    C* Description must be entered on at least the first line
     *
      *** beg del ***** 07/22/09 **************************************
    C**n90*****         if        scexd1 = *blanks
    C**********         eval      *in70 = *on
    C**********         eval      *in90 = *on
    C**********         endif
      *** end del ***** 07/22/09 **************************************
    C*
    C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
    C*
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
    C*
    C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
    C*
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
    C*
    C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
    C*
     C                   endsr
    C*================================================================
    C* COPYSR - COPY FIELDS FROM SELECTED RECORD
    C*================================================================
     C     copysr        begsr
    C*
     C                   endsr
    C*                                                    COPYSR
    C*================================================================
    C* MaintFlds - Load maintenance fields
    C*================================================================
     C     MaintFlds     begsr
    C*
     C                   eval      extPRUSER = q1user
     C                   eval      extPRPGM  = q1pgm
     C                   eval      extPRDT   = DateTo7(SystemDate)
     C                   eval      extPRTIME = TimeTo6(SystemTime)
     C*
     C                   if        @@mode = 'A'
     C                   eval      extPRIUSR = extPRUSER
     C                   eval      extPRIPGM = extPRPGM
     C                   eval      extPRIDT  = extPRDT
     C                   eval      extPRITIM = extPRTIME
     C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
    C*
      *** beg chg ***** 07/22/09 **************************************
     C***********        eval      fullDesc = extPREXDS
     c                   select
     c                   when      cltID = '136'
     c                   eval      fullDesc2 = extPREXDS
     c                   other
     c                   eval      fullDesc = extPREXDS
     c                   endsl
     c                   if        @@mode = 'A'
     c                   eval      scstat = 'A'
     c                   else
     c                   eval      scstat = extPRSTAT
     c                   end
      *** end chg ***** 07/22/09 **************************************
    C*
     C                   endsr
    C*
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
    C*
     C                   eval      extPRCO#  = ##co#
     C                   eval      extPRRTCD = scrtcd
     C                   eval      extPRCLAS = scclas
     C                   eval      extPREFDT = dtCYMD(scefdt)
      *** beg chg ***** 07/22/09 **************************************
     C***********        eval      extPREXDS = fullDesc
     c                   select
     c                   when      cltID = '136'
     c                   eval      extPREXDS = fullDesc2
     c                   other
     c                   eval      extPREXDS = fullDesc
     c                   endsl
     c                   eval      extPRSTAT = scstat
      *** end chg ***** 07/22/09 **************************************
    C*
     C                   endsr
    C*
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
    C*
     C                   clear                   ppr01e
     C                   clear                   fullDesc
    C*
      *** beg add ***** 07/22/09 **************************************
     c                   call      'RTVCLTID'
     c                   parm                    cltID             3
     c                   if        cltID = '136'
     c                   eval      *in50 = *on
     C                   clear                   fullDesc2
     c                   endif
      *** end add ***** 07/22/09 **************************************
    C*
     C     @@rrn         chain     dbrcdf
    C*
     C                   if        %found
    C*
     C                   eval      scrtcd = PRRTCD
     C                   eval      scclas = PRCLAS
     C                   eval      scefdt = dtMDYY(PREFDT)
    C*
     C     keyWMPRPE     chain     ppr01e
     C*
     C                   select
     C                   when      @@mode = 'C' and not %found
     C                   eval      @@mode = 'A'
     C                   endsl
     C*
     C     @@mode        comp      'A'                                    40
    C*
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
    C*
     C     keyWMPRPE     chain     ppr01e
    C*
B01  C                   if        %found
     C                   exsr      mvscdb
     C                   exsr      maintFlds
     C                   update    ppr01e
E01  C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
    C*
     C                   clear                   ppr01e
    C*
     C                   exsr      mvscdb
     C                   exsr      maintFlds
     C                   write     ppr01e
    C*
     C                   feod      wmprpe
     C*
     C                   eval      outflg = *on
    C*
     C                   endsr
