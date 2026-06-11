     Hcopyright('(c) Copyright Fiserv, Inc., 2006')
    H*
     *****************************************************************
     *    PROGRAM: W2286R                                            *
     *                                                               *
     *    DESCRIPTION: W/W Audit company assignment                  *
     *                                                               *
     *    CREATED BY:  C:132  08/28/06   PROJECT# WC1196             *
     *                 C:33                                          *
     *****************************************************************
      * Revised:                                                      *
      *                                                               *
     *****************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw2286f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwmxhp     if   e             disk    rename(pxh01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwmxhl2    if   e           k disk    rename(pxh01:dbrcdf)
     F                                     infds(dbfds)
    F*
    F* Business Category File
     Fwmemp     if   e             disk    rename(pem01:pem0p)
     Fwmeml     if   e           k disk    rename(pem01:pem00)
    F*
    F* Business Category File
     Fwttgp     if   e             disk    rename(ptg01:ptg0p)
     Fwttgl     if   e           k disk    rename(ptg01:ptg00)
    F*
    F* Audit Company Table file
     Fwtacp     if   e             disk    rename(pac01:pac0p)
     Fwtacl     if   e           k disk    rename(pac01:pac00)
    F*
    F* Audit Type file
     Fwttap     if   e             disk    rename(pta01:pta0p)
     Fwttal     if   e           k disk    rename(pta01:pta00)
    F*
    F* SIC/NAIC Code table file
     Fwtgtl     if   e           k disk    rename(pgt01:pgt00)
     F*
     F* Payroll Class Code Description file
     Fwmprp     if   e             disk    rename(ppr01:ppr0p)
     Fwmprl     if   e           k disk    rename(ppr01:ppr00)
     F*
     Fwdelp     if   e             disk    rename(pel01:pel0p)
     Fwdell     if   e           k disk    rename(pel01:pel00)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Audit Co/Bus Cat Assign Hist'
     D slmsgi          c                   'WC39998'
     D deleted         c                   '*Deleted'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W2286R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D chgnum          c                   'SA020R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   11
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D @@call          s              1
     D in              s              2  0
     D kkCo#           s                   like(XHCO#)
     D kkFnd           s                   like(XHFND)
     D kkTrDt          s                   like(XHTRDT)
     D kkEmp#          s                   like(XHEMP#)
     D kkDiv           s                   like(XHDIV)
     D kkFyr           s                   like(XHFYR)
     D sysDt7          s              7  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D/copy sPrDate
     D/copy sPrMessage
     D/copy sPrRunCmd
     D/copy sPrString
     D/copy sPrWrkWth

     D/copy wPrEmplr

     DRtvCode          PR             5
     D typeOfCode                          like(XHUSFL)

     DRtvCodeDesc      PR            40
     D typeOfCode                          like(XHUSFL)

     DSearchAudCo      PR             2  0

     DSearchAudTyp     PR             2  0

     DSearchCode       PR             2  0
     D typeOfCode                          like(XHUSFL)

     DSearchEmp        PR             2  0

     DSearchFyr        PR             2  0

     DW2281R           PR                  extPgm('W2281R')
     D call                           1    const
     D inkc                           1    const
     D inkl                           1    const
     D rrn                            9  0

     DW2290R           PR                  extPgm('W2290R')
     D call                           1    const
     D inkc                           1    const
     D inkl                           1    const
     D rrn                            9  0
    
     C/copy sCopyWW41C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
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
     C*
     C     key1          klist
     C                   kfld                    kkCo#
     C                   kfld                    kkFnd
     C*
     C     key1a         klist
     C                   kfld                    kkCo#
     C                   kfld                    kkFnd
     C                   kfld                    kkTrDt
     C                   kfld                    kkEmp#
     C                   kfld                    kkDiv
     C                   kfld                    kkFyr
     C*
     C     keyWMEML      klist
     C                   kfld                    XHCO#
     C                   kfld                    XHFND
     C                   kfld                    XHEMP#
     C                   kfld                    XHDIV
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
     C*
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C                   eval      *in11 =
     C                             (ssEmp#   <> *zeros  or
     C                              ssDiv    <> *blanks or
     C                              ssFyr    <> *zeros  or
     C                              ssUsFl   <> *blanks or
     C                              ssTypA   <> *blanks or
     C                              ssBCtg   <> *blanks or
     C                              ssPremFr <> *zeros  or
     C                              ssPremTo <> *zeros  or
     C                              ssNRFl   <> *blanks or
     C                              ssACo    <> *blanks)
     C*
     C                   endsr
     C*
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C                   endsr
     C*
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C*
     C                   exsr      othFks
     C*
     C                   endsr
     C*
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C*
     C                   clear                   SSEMP#
     C                   clear                   SSDIV
     C                   clear                   SSFYR
     C                   clear                   SSTYPA
     C                   clear                   SSBCTG
     C                   clear                   SSPREMFR
     C                   clear                   SSPREMTO
     C                   clear                   SSNRFL
     C                   clear                   SSACO
     C*
     C                   endsr
     C*
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C*
     C                   eval      kkCo#  = ##Co#
     C                   eval      kkFnd  = ##Fnd
     C                   eval      kkTrDt = *loval
     C                   eval      kkEmp# = *loval
     C                   eval      kkDiv  = *loval
     C                   eval      kkFyr  = *loval
     C*
     C                   endsr
     C*
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C                   select
     C                   when      *in21
     C                   eval      kkTrDt = DtCYMD(NumDate(scPst1))
     C                   endsl
     C*
     C                   endsr
     C*
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   eval      kkCo#  = ##Co#
     C                   eval      kkFnd  = ##Fnd
     C                   eval      kkTrDt = scTrDt
     C                   eval      kkEmp# = scEmp#
     C                   eval      kkDiv  = scDiv
     C                   eval      kkFyr  = scFyr
     C*
     C                   endsr
    C*================================================================
    C* KJDO - Search Subroutine for F10
    C*================================================================
     C     KJDO          begsr
     C*
     C                   select
     C                   when      hfld = 'SSEMP#' or hfld = 'SSDIV'
     C                   eval      in = SearchEmp
     C                   when      hfld = 'SSFYR'
     C                   eval      in = SearchFyr
     C                   when      hfld = 'SSCODE'
     C                   eval      in = SearchCode(ssUsFl)
     C                   when      hfld = 'SCTYPA'
     C                   eval      in = SearchAudTyp
     C                   when      hfld = 'SSACO'
     C                   eval      in = SearchAudCo
     C                   endsl
     C*
     C                   eval      *in(in) = *on
     C*
     C                   endsr
     C*
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      scTrDt = XHTRDT
     C                   eval      scEmp# = XHEMP#
     C                   eval      scDiv  = XHDIV
     C                   eval      scFyr  = XHFYR
     C*
     C                   select
     C     info#         wheneq    1
     C                   eval      scData = AlphaUSA(CvtToDate(XHTRDT)) +
     C                                ' ' + %editc(XHEMP#:'Z') +
     C                                ' ' + %editc(XHDIV:'Z') +
     C                                ' ' + %editc(XHFYR:'Z') +
     C                                ' ' + XHUSFL +
     C                               '  ' + RtvCode(XHUSFL) +
     C                               '  ' + XHTYPA +
     C** **                           ' ' + XHATYP +
     C                              '   ' + XHBCTG +
     C                               '  ' + %editc(XHPREM:'J') +
     C                              '   ' + XHNRFL + ' ' +
     C                              '   ' + XHACO
     C*
     C                   endsl
     C*
     C                   eval      scEmpNm = GetEmpName(scEmp#:scDiv)
     C*
     C                   eval      scSNGD = RtvCodeDesc(XHUSFL)
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
     C*
    C*================================================================
    C* Opt20 - Subroutine to change the status
    C*================================================================
     C     opt20         begsr
     C*
     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl                  99
     C*
     C                   endsr                                                  opt20
     C*
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C*
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
     C*
     C                   endsr
     C*
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C*
     C     scsel         caseq     '20'          opt20
     C                   endcs
     C*
     C                   endsr
     C*
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
     C*
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C                   eval      sysDt7 = DateTo7(SystemDate)
     C*
     C                   eval      *in12 = *on
     C*
     C                   eval      scFnd   = ##Fnd
     C                   eval      scFndNm = ##FNam
     C*
     C                   endsr
     C*
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C*
     C                   select
     C                   when      *in21
     C     key1          reade     dbrcdf                                 95
     C                   endsl
     C*
     C                   endsr
     C*
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C*
     C                   select
     C                   when      *in21
     C     key1          readpe    dbrcdf                                 95
     C                   endsl
     C*
     C                   endsr
     C*
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C*
     C                   select
     C                   when      *in21
     C     key1a         setgt     dbrcdf                             96
     C                   endsl
     C*
     C                   endsr
     C*
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C*
     C                   select
     C                   when      *in21
     C     key1a         setll     dbrcdf                             96
     C                   endsl
     C*
     C                   endsr
     C*
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C*
     C                   eval      scData = deleted
     C*
     C                   endsr
     C*
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C                   endsr
     C*
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C                   select
     C                   when      ssEmp# <> *zeros and ssEmp# <> XHEMP#
     C                   eval      meetss = *off
     C                   when      ssEmp# <> *zeros and
     C                              ssDiv <> *blanks and
     C                               ssDiv <> %char(XHDIV)
     C                   eval      meetss = *off
     C                   when      ssFyr <> *zeros and ssFyr <> XHFYR
     C                   eval      meetss = *off
     C                   when      ssUsFl <> *blanks and ssUsFl <> XHUSFL
     C                   eval      meetss = *off
     C                   when      ssTypA <> *blanks and ssTypA <> XHTYPA
     C                   eval      meetss = *off
     C                   when      ssBCtg <> *blanks and ssBCtg <> XHBCTG
     C                   eval      meetss = *off
     C                   when      (ssPremFr <> *zero or ssPremTo <> *zeros)
     C                               and
     C                             (ssPremFr > XHPREM or ssPremTo < XHPREM)
     C                   eval      meetss = *off
     C                   when      ssNRFl <> *blanks and ssNRFl <> XHNRFL
     C                   eval      meetss = *off
     C                   when      ssACo <> *blanks and ssACo <> XHACO
     C                   eval      meetss = *off
     C                   endsl
     C*
     C                   endsr





     P*=====================================================================
     P* Retrieve code
     P*=====================================================================
     PRtvCode          B                   Export
     DRtvCode          PI             5
     D typeOfCode                          like(XHUSFL)

     C                   select
     C                   when      typeOfCode = 'G'
     C                   return    %editc(XHGOVN:'X')
     C                   when      typeOfCode = 'N'
     C                   return    XHNAIC
     C                   when      typeOfCode = 'S'
     C                   return    XHSIC
     C                   other
     C                   return    *blanks
     C                   endsl
    
     PRtvCode          E





     P*=====================================================================
     P* Retrieve code description
     P*=====================================================================
     PRtvCodeDesc      B                   Export
     DRtvCodeDesc      PI            40
     D typeOfCode                          like(XHUSFL)

     D kkClas          s                   like(PRCLAS)
     D kkEfDt          s                   like(PREFDT)
     D kkKey           s                   like(GTKEY)
     D kkTbID          s                   like(GTTBID)

     C     keyWMPRL      klist
     C                   kfld                    ##co#
     C                   kfld                    kkClas
     C                   kfld                    kkEfDt
     C*
     C     key1WMPRL     klist
     C                   kfld                    ##co#
     C                   kfld                    kkClas
     C*
     C     tableKey      klist
     C                   kfld                    ##Co#
     C                   kfld                    kkTbID
     C                   kfld                    kkKey

     C                   select
     C                   when      typeOfCode = 'G'
     C                   eval      kkClas = XHGOVN
     C                   eval      kkEfDt = sysDt7
     C                   clear                   PRDESC
     C     keyWMPRL      setgt     ppr00
     C     key1WMPRL     readpe    ppr00
     C                   return    PRDESC
     C                   when      typeOfCode = 'N'
     C                   eval      kkTbID = 'NAIC'
     C                   eval      kkKey  = XHNAIC
     C                   clear                   GTDESC
     C     tableKey      chain     pgt00
     C                   return    GTDESC
     C                   when      typeOfCode = 'S'
     C                   eval      kkTbID = 'SIC'
     C                   eval      kkKey  = XHSIC
     C                   clear                   GTDESC
     C     tableKey      chain     pgt00
     C                   return    GTDESC
     C                   other
     C                   return    *blanks
     C                   endsl
    
     PRtvCodeDesc      E





     P*=====================================================================
     P* Search Audit Company
     P*=====================================================================
     PSearchAudCo      B                   Export
     DSearchAudCo      PI             2  0

     D rrn             s              9  0 inz
     D inkc            s                   like(*inkc)
     D inkl            s                   like(*inkl)

     C                   callp     W2290R('2':inkc:inkl:rrn)

     C                   clear                   ACNAME
     C     rrn           chain     wtacp

     C                   select
     C                   when      %found(wtacp)
     C                   eval      SSACO = ACNAME
     C                   other
     C                   return    49
     C                   endsl
    
     PSearchAudCo      E





     P*=====================================================================
     P* Search Audit Type
     P*=====================================================================
     PSearchAudTyp     B                   Export
     DSearchAudTyp     PI             2  0

     D inkc            s                   like(*inkc)
     D inkl            s                   like(*inkl)
     D rrn             s              9  0 inz

     C                   callp     W2290R('2':inkc:inkl:rrn)

     C                   clear                   TATYPD
     C     rrn           chain     wttap

     C                   select
     C                   when      %found(wttap)
     C                   eval      SSTYPA = TATYPA
     C                   return    47
     C                   other
     C                   return    46
     C                   endsl
    
     PSearchAudTyp     E





     P*=====================================================================
     P* Search code
     P*=====================================================================
     PSearchCode       B                   Export
     DSearchCode       PI             2  0
     D typeOfCode                          like(XHUSFL)

     C                   select
     C                   when      typeOfCode = 'G'
     C                   return    46
     C                   when      typeOfCode = 'N'
     C                   return    46
     C                   when      typeOfCode = 'S'
     C                   return    46
     C                   other
     C                   return    43
     C                   endsl

     C                   return    45
    
     PSearchCode       E





     P*=====================================================================
     P* Search Employer
     P*=====================================================================
     PSearchEmp        B                   Export
     DSearchEmp        PI             2  0

     D rrn             s              9  0 inz
     D inkc            s                   like(*inkc)
     D inkl            s                   like(*inkl)

     C                   eval      rrn = EmpSrch(inkc:inkl)

     C                   clear                   EMEMP#
     C                   clear                   EMDIV
     C     rrn           chain     wmemp

     C                   select
     C                   when      %found(WMEMP)
     C                   eval      SSEMP# = EMEMP#
     C                   eval      SSDIV  = %editc(EMDIV:'X')
     C                   return    42
     C                   other
     C                   return    41
     C                   endsl
    
     PSearchEmp        E





     P*=====================================================================
     P* Search Year
     P*=====================================================================
     PSearchFyr        B                   Export
     DSearchFyr        PI             2  0

     D err             s              1    inz
     D inkc            s                   like(*inkc)
     D inkl            s                   like(*inkl)
     D rrn             s              9  0 inz
     D xxDiv           s                   like(EMDIV)

     C                   move      SSDIV         xxDiv
     C                   eval      rrn = EmpYrSrch(inkc:inkl:err:
     C                                             SSEMP#:xxDiv)

     C                   clear                   ELEMP#
     C                   clear                   ELDIV
     C                   clear                   ELFYR
     C     rrn           chain     wdelp

     C                   select
     C                   when      err = '2'
     C                   return    50
     C                   when      %found(WDELP)
     C                   eval      SSEMP# = ELEMP#
     C                   eval      SSDIV  = %editc(ELDIV:'X')
     C                   eval      SSFYR  = ELFYR
     C                   return    43
     C                   other
     C                   return    42
     C                   endsl
    
     PSearchFyr        E
3




** Column headings
Opt   Trans Dt   Employer   Div  Yr Tp Code  Type  Ctgy     Premium  Renew  Co
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
212F12=Cancel               NNY
215F15=Fold/truncate        YYY
217F17=Subset               YYY
