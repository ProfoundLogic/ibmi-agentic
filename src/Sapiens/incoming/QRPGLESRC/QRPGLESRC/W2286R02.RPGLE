     Hcopyright('(c) Copyright Fiserv, Inc., 2006')
     *****************************************************************
     *    PROGRAM: W2283R02                                          *
     *                                                               *
     *    DESCRIPTION: Audit company assignment                      *
     *                                                               *
     *    CREATED BY:  C:132  08/28/06   PROJECT# WC1196             *
     *                 C:33                                          *
     *****************************************************************
      * Revised:                                                      *
      *                                                               *
      *                                                               *
     *****************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw2286f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmxhp     uf a e             disk    rename(pxh01:dbrcdf)
     F
     Fwttap     if   e             disk    rename(pta01:pta0p)
     Fwttal     if   e           k disk    rename(pta01:pta00)
     F
     Fwtacp     if   e             disk    rename(pac01:pac0p)
     Fwtacl     if   e           k disk    rename(pac01:pac00)
     F
     Fwtcnp     if   e             disk    rename(pcn01:pcn0p)
     Fwtcnl1    if   e           k disk
     F
     Fwttgp     if   e             disk    rename(ptg01:ptg0p)
     Fwttgl     if   e           k disk    rename(ptg01:ptg00)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Audit Company Assignment'
     D comapi          c                   'WA001R'
     D chgnum          c                   'SA020R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@call          s              1
     D @@rrn           s              9  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
     D/copy wPrEmplr

     DRtvCode          PR             5
     D typeOfCode                          like(XHUSFL)
    C*
     C/COPY SCOPYMT01C
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
     C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*
     C     keyWTACL      klist
     C                   kfld                    XHCO#
     C                   kfld                    XHACO
     C*
     C     keyWTTAL      klist
     C                   kfld                    XHCO#
     C                   kfld                    XHTYPA
     C*
     C     keyWTTGL      klist
     C                   kfld                    XHCO#
     C                   kfld                    XHBCTG
      *
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
     C*
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
      *===============================================================
      * KJDO - Subroutine for F10
      *===============================================================
      *
     C     kjdo          begsr
      *
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      scEmp# = XHEMP#
     C                   eval      scDiv  = XHDIV
     C                   eval      scFyr  = XHFYR
     C                   eval      scUsFl = XHUSFL
     C                   eval      scCode = RtvCode(XHUSFL)
     C                   eval      scTypA = XHTYPA
     C                   eval      scBCtg = XHBCTG
     C                   eval      scPrem = XHPREM
     C                   eval      scACo  = XHACO
     C*
     C                   eval      scEmpNm = GetEmpName(scEmp#:scDiv)
     C*
     C                   move      *blanks       TATYPD
     C     keyWTTAL      chain     pta00
     C                   eval      scAudTypDs = TATYPD
     C*
     C                   move      *blanks       TGBCDS
     C     keyWTTGL      chain     ptg00
     C                   eval      scBusCtgDs = TGBCDS
     C*
     C                   move      *blanks       ACNAME
     C     keyWTACL      chain     pac00
     C                   eval      scAudCoDs = ACNAME
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C*
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C     *inkj         caseq     *ON           kjdo                     90
     C                   endcs
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
     C*
     C     @@mode        comp      'D'                                    09
     C     @@mode        comp      'A'                                    40
     C*
     C                   eval      scFnd   = ##Fnd
     C                   eval      scFndNm = ##FNam
     C*
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
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   write     dbrcdf
     C                   feod      wmxhp
     C                   endsr





     P*=====================================================================
     P* Retrieve code
     P*=====================================================================
     PRtvCode          B                   Export
     DRtvCode          PI             5
     D typeOfCode                          like(XHUSFL)

     C                   select
     C                   when      typeOfCode = 'G'
     C                   return    %char(XHGOVN)
     C                   when      typeOfCode = 'N'
     C                   return    XHNAIC
     C                   when      typeOfCode = 'S'
     C                   return    XHSIC
     C                   endsl

     C                   return    *blanks
    
     PRtvCode          E
