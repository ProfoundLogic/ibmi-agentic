      * (C) Copyright FISERV Insurance Solutions, Inc., 2005

      /title W0010R63 - Premium Auto Cash Dissolve
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
      *****************************************************************
      * CALLED FROM: W0010R62 - Auto Cash Dissolve Hierarchy          *
      * CALLED TO:   None                                             *
      *                                                               *
      * created 03/30/05  C:136  Project: WC1179                      *
      *****************************************************************
      * Revised:                                                      *
      *                                                               *
      * 05/03/09 C:136 Project: WC1663                                *
      *                Verbiage change on column headers              *
      *                                                               *
      * 01/20/06 C:136 Support: WA129 50989                           *
      *                Make sure that New Business code aren't used   *
      *                for the premium payment codes.                 *
      *****************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     fw0010f63  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     *
     *  Physical file definition (for maintaining record)
     fwtc3p     uf a e             disk
     f                                     rename(pc301:dbrcdf)
     fwtc3l     if   e           k disk    infds(c3rrnds)
      *** beg add ***** 01/20/06 **************************************
     fwdf2l     if   e           k disk    rename(pf201 : pf20l)
      *** end add ***** 01/20/06 **************************************
    d*================================================================
    d*  /COPY required constants
    d*================================================================
     d optnm           c                   'Premium Auto Cash Dissolve'
     d comapi          c                   'WA001R'
     d #ofscr          c                   1
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d ##co#           s              3  0
     d ##conm          s             40
     d ##fnam          s             40
     d ##fnd           s              3  0
     d @@kc            s              1
     d @@kl            s              1
     d @@mode          s              1
     d @@rrn           s              9  0
     d @@fnd           s              3  0
     d sysdt           s              7  0
     d xxBadCode       s              1
     d key             s             10
     d x               s              1  0
     d z               s              2  0
     d c3rrnds         ds
     d  c3rrn                397    400b 0

     d srchArray       s             10    dim(4) ctdata perrcd(1)

     d                 ds
     d srchField               1      8
     d                                     dim(4)
     d  SCCNCD                 1      2
     d  SCCPCD                 3      4
     d  SCAPCD                 5      6
     d  SCSPCD                 7      8
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - BOTTOM
    d*---------------------------------------------------------------------
     d/copy wprar
     d/copy wprgroup
     d/copy sprstring
     d/copy sprdate
     c/copy scopymt41c
     *================================================================
     *  PLIST definitions
     *================================================================
     c     *entry        plist
     c                   parm      *inkc         @@kc
     c                   parm      *inkl         @@kl
     c                   parm                    @@mode
     c                   parm                    @@fnd
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     c     compl         plist
     c                   parm                    ##co#
     c                   parm                    ##fnd
     c                   parm                    ##conm
     c                   parm                    ##fnam
     *================================================================
     *  KLIST definitions
     *================================================================
     c     fnky          klist
     c                   kfld                    ##co#
     c                   kfld                    @@fnd
     *================================================================
     * DLTPRE - Pre-processing before delete occurs
     *================================================================
     c     dltpre        begsr
     c                   endsr
     *================================================================
     * DSPLYR - Processing after displaying information
     *================================================================
     c     dsplyr        begsr
     c                   endsr
     *================================================================
     * ERRCK1 - Error checking for srceen 1 (SCR01)
     *================================================================
     c     errck1        begsr

     c                   if        ValidCode(sccncd) = *off
     c                   seton                                        72  90
     c                   endif

     c  n90              if        GetARCat(sccncd)  <> 'P' or
     c                             GetARType(sccncd) <> 'P'
     c                   seton                                        74  90
     c                   endif

     c  n90              if        ValidCode(sccpcd) = *off
     c                   seton                                        73  90
     c                   endif

     c  n90              if        GetARCat(sccpcd)  <> 'P' or
     c                             GetARType(sccpcd) <> 'P'
     c                   seton                                        75  90
     c                   endif

     c  n90              if        ValidCode(scapcd) = *off
     c                   seton                                        77  90
     c                   endif

     c  n90              if        GetARCat(scapcd)  <> 'P' or
     c                             GetARType(scapcd) <> 'P'
     c                   seton                                        79  90
     c                   endif

     c  n90              if        ValidCode(scspcd) = *off
     c                   seton                                        81  90
     c                   endif

     c  n90              if        GetARCat(scspcd)  <> 'P' or
     c                             GetARType(scspcd) <> 'P'
     c                   seton                                        83  90
     c                   endif

     *** beg add ***** 01/20/06 **************************************
     c     fnky          chain     pf20l
     c  n90              if           sccncd = F2NPCD
     c                             or sccncd = F2NXCD
     c                   seton                                        84  90
     c                   endif
     c  n90              if           sccpcd = F2NPCD
     c                             or sccpcd = F2NXCD
     c                   seton                                        85  90
     c                   endif
     c  n90              if           scapcd = F2NPCD
     c                             or scapcd = F2NXCD
     c                   seton                                        86  90
     c                   endif
     c  n90              if           scspcd = F2NPCD
     c                             or scspcd = F2NXCD
     c                   seton                                        87  90
     c                   endif
     *** end add ***** 01/20/06 **************************************

     c                   endsr
     *================================================================
     * ERRCK2 - Error checking for srceen 2 (SCR02)
     *================================================================
     c     errck2        begsr
     c                   endsr
     *================================================================
     * ERRCK3 - Error checking for srceen 3 (SCR03)
     *================================================================
     c     errck3        begsr
     c                   endsr
     *================================================================
     * KJDO - Do this if somebody hits F10
     *================================================================
     c     kjdo          begsr

     c                   eval      x = 1
     c                   eval      z = 50
     c     hfld          lookup    srchArray(x)                           60

     c                   eval      z = z + x
     c                   eval      key = ARCodeSrch(*inkc:*inkl)

     c                   if        key <> *blanks
     c                   eval      srchField(x) = key
     c                   eval      z = z + 1
     c                   endif

     c                   eval      *in(z) = *on
     c                   eval      *in90 = *on
     c
     c                   endsr
     *================================================================
     * MVDBSC - Move data base fields to screen fields
     *================================================================
     c     mvdbsc        begsr
      *
     c                   eval      sccncd = C3CNCD
     c                   eval      sccpcd = C3CPCD
     c                   eval      scapcd = C3APCD
     c                   eval      scspcd = C3SPCD
      *
     c                   endsr
     *================================================================
     * MVSCDB - Move screen fields to data base fields
     *================================================================
     c     mvscdb        begsr
      *
     c                   eval      C3CNCD = sccncd
     c                   eval      C3CPCD = sccpcd
     c                   eval      C3APCD = scapcd
     c                   eval      C3SPCD = scspcd
      *
     c                   endsr
     *================================================================
     * OTHERR - Processing for other modes
     *================================================================
     c     otherr        begsr
     c                   endsr
     *================================================================
     * OTHFKS - Processing for other functions keys
     *================================================================
     c     othfks        begsr
     c     *inkj         caseq     *on           kjdo                 90
     C                   endcs
     c                   endsr
     *================================================================
     * PGMEND - End of program processing
     *================================================================
     c     pgmend        begsr
     c                   eval      *inlr = *on
     c                   endsr
     *================================================================
     * PGMSTR - Start of program processing
     *================================================================
     c     pgmstr        begsr
     c     fnky          chain     pc301
     c                   eval      @@rrn = c3rrn
     c     @@mode        comp      'D'                                    09
     c                   eval      scfnd  = @@fnd
     c                   eval      scname = GetGrpName(@@fnd)
     c                   endsr
     *================================================================
     * PREOTH - Pre-processing for other modes
     *================================================================
     c     preoth        begsr
     c                   endsr
     *================================================================
     * UPDATR - Update data base record and after processing
     *================================================================
     c     updatr        begsr
     c     @@rrn         chain     dbrcdf                             99
B01  c                   if        *in99 = *off
     c                   exsr      mvscdb
     c                   eval      C3USER = Q1USER
     c                   eval      C3PGM  = Q1PGM
     c                   eval      C3DT   = DateTo7(SystemDate)
     c                   eval      C3TIME = TimeTo6(SystemTime)
     c                   update    dbrcdf
E01  c                   endif
     c                   endsr
     *================================================================
     * WRITER - Write data base record and after processing
     *================================================================
     c     writer        begsr
     c                   clear                   dbrcdf
     c                   exsr      mvscdb
     c                   write     dbrcdf
     c                   feod      wtc3p
     c                   endsr
** srchArray
SCCNCD
SCCPCD
SCAPCD
SCSPCD
