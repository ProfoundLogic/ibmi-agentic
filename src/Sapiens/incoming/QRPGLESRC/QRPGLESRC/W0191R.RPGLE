     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
    H*********************************************************************
    h/TITLE W0191R - W/W HCN Program Information
    H*********************************************************************
     H* Created: 11/10/10   C:33   Project: 139WPL
    H*********************************************************************
     H* Revisions:
     H*
    H*********************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw0191f    cf   e             workstn infds(fids01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwmhcp     if   e             disk    rename(phc01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwmhcl     if   e           k disk    rename(phc01:dbrcdf)
     F                                     infds(dbfds)
     Fwmfnp     if   e             disk    rename(pfn01:pfn0p)
     Fwdhcl2    if   e           k disk    rename(phc01:dhc02)
    D*
     D#ofHDs           c                   1
     D hd              s             78    dim(#ofHDs) ctdata perrcd(1)
     D#ofOFDs          c                   20
     D of              s              1    dim(#ofOFDs) ctdata perrcd(1)
     D ofd             s             35    dim(#ofOFDs) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'HCN Program Information'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0191R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   12
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
     D ##rrn2          s              9  0
     D @@kc            s              1n
     D @@kl            s              1n
     D @@call          s              1
     D @@rrn           s              9  0
     D kkCo#           s                   like(HCCO#)
     D kkFnd           s                   like(HCFND)
     D kkSt            s                   like(HCST)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW42C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@call
     C                   parm                    @@rrn
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm      @@rrn         ##rrn2
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*====================================================================
    C*  KLIST definitions
    C*====================================================================
     C     key           klist
     C                   kfld                    kkCo#
     C                   kfld                    kkFnd
     C*
     C     key1          klist
     C                   kfld                    kkCo#
     C                   kfld                    kkFnd
     C                   kfld                    kkSt
     C*
     C     keyWDHCL2     klist
     C                   kfld                    kkCo#
     C                   kfld                    kkFnd
     C                   kfld                    scSt
    C*====================================================================
    C* ChkSsM - Set indicator 11 for subset message
    C*====================================================================
     C     ChkSsM        begsr
     C                   endsr
     C*
    C*====================================================================
    C* Errck1 - Additional errors on subfile selection
    C*====================================================================
     C     Errck1        begsr
     C*
     C                   select
     C*_________
     C*
     C                   when      scSel = '4'
     C* Can not delete if employer established with HCN program
     C     keyWDHCL2     chain     wdhcl2
     C                   if        %found
     C                   seton                                        71  90
     C                   endif
     C*_________
     C*
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* Errck2 - Error checking for the subset screen
    C*====================================================================
     C     Errck2        begsr
     C                   endsr
     C*
    C*====================================================================
    C* InzSst - Initialize the subset fields
    C*====================================================================
     C     InzSst        begsr
     C                   endsr
     C*
    C*====================================================================
    C* KeyInz - Initialize key values for program start
    C*====================================================================
     C     KeyInz        begsr
     C*
     C                   eval      kkCo# = FNCO#
     C                   eval      kkFnd = FNFND
     C*
     C                   clear                   kkSt
     C*
     C                   endsr
     C*
    C*====================================================================
    C* KeySet - Set key values when doing "position to"
    C*====================================================================
     C     KeySet        begsr
     C*
     C                   eval      kkSt = scPst1
     C*
     C                   endsr
     C*
    C*====================================================================
    C* KeySfl - Set key values from subfile records for paging
    C*====================================================================
     C     KeySfl        begsr
     C*
     C                   eval      kkSt = scSt
     C*
     C                   endsr
     C*
    C*====================================================================
    C* KPDO - Process F15 = Mass Copy
    C*====================================================================
     C     KpDo          begsr
     C*
     C                   eval      ##Mode = 'M'
     C                   call      AcdPgm        AcdPl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* MvDbSc - Move data base fields to screen fields
    C*====================================================================
     C     MvDbSc        begsr
     C*
     C* Hidden screen fields
     C                   eval      scSt   = HCST
     C                   eval      scEfDt = HCEFDT
     C                   eval      scExDt = HCEXDT
     C                   eval      scFact = HCFACT
     C                   eval      scAppl = HCAPPL
     C*
     C* Data field
     C                   select
     C                   when      *in21
     C                   eval      scData      = HCST
     C                             + '  '      + AlphaUSA(CvtToDate(HCEFDT))
     C                             + '  '      + AlphaUSA(CvtToDate(HCEXDT))
     C                             + '   '     + %editc(HCFACT:'1')
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* OthFks - Process other function keys
    C*====================================================================
     C     OthFks        begsr
     C*
     C     *inkp         caseq     *on           KpDo
     C                   endcs
     C*
     C                   endsr
    C*====================================================================
    C* OthOpt - Process other options
    C*====================================================================
     C     OthOpt        begsr
     C                   endsr
     C*
    C*====================================================================
    C* PgmEnd - End of program processing
    C*====================================================================
     C     PgmEnd        begsr
     C                   endsr
     C*
    C*====================================================================
    C* PgmStr - Start of program processing
    C*====================================================================
     C     PgmStr        begsr
     C*
     C     @@rrn         chain     wmfnp
     C                   eval      scFnd   = FNFND
     C                   eval      scFndNm = FNNAME
     C*
     C                   exsr      KeyInz
     C*
     C                   endsr
     C*
    C*====================================================================
    C* Read - Read next record from file
    C*====================================================================
     C     Read          begsr
     C*
     C                   select
     C                   when      *in21 = *on
     C     key           reade     dbrcdf                                 95
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* ReadP - Read previous record from file
    C*====================================================================
     C     Readp         begsr
     C*
     C                   select
     C                   when      *in21 = *on
     C     key           readpe    dbrcdf                                 95
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SetGT  - Set greater than on file
    C*====================================================================
     C     SetGT         begsr
     C*
     C                   select
     C                   when      *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SetLL  - Set lower limits on file
    C*====================================================================
     C     SetLL         begsr
     C*
     C                   select
     C                   when      *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* Sf1Upd - Update subfile record when item is deleted
    C*====================================================================
     C     Sf1Upd        begsr
     C*
     C                   eval      scData = '* * * Deleted'
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SlctRd - Move fields for selected record to parameters
    C*====================================================================
     C     SlctRd        begsr
     C                   endsr
     C*
    C*====================================================================
    C* Subset - Determine if record should be put in subfile
    C*====================================================================
     C     Subset        begsr
     C                   endsr
** Column headings
Opt  St   Eff Date    Exp Date   Discount
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
215F15=Mass copy            YNN
217F17=Subset               NNN
3
