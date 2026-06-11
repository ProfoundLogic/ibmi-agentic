     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*********************************************************************
     H/TITLE W0063R - Work with Fee Type Information
     H*********************************************************************
     H*  Created: 02/11/11   C:33   Project: WC1851                       *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H*********************************************************************
     Fw0063f    cf   e             workstn infds(fids01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwtftp     if   e             disk    rename(pft01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwtftl     if   e           k disk    rename(pft01:dbrcdf)
     F                                     infds(dbfds)
     Fwmfnp     if   e             disk    rename(pfn01:pfn0p)
     Fwtfil     if   e           k disk    rename(pfi01:pfi00)
     Fwtgtl     if   e           k disk    rename(pgt01:pgt00)
    D*
     D#ofHD            c                   1
     D hd              s             78    dim(#ofHD) ctdata perrcd(1)
     D#ofOFD           c                   20
     D of              s              1    dim(#ofOFD) ctdata perrcd(1)
     D ofd             s             35    dim(#ofOFD) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Fee Type Information'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D AcdPgm          c                   'W0063R02'
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
     D ##call          s              1
     D @@kc            s              1
     D @@kl            s              1
     D @@call          s              1
     D @@rrn           s              9  0
     D kkCo#           s                   like(FTCO#)
     D kkFnd           s                   like(FTFND)
     D kkType          s                   like(FTTYPE)
     D kkDesc          s                   like(FTDESC)
     D sysDt7          s                   like(FTDT)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     DClmFeeColtDtl    pr             1n
    D*
     DGenTableDesc     pr                  like(GTDESC)
     D inKey                               like(GTKEY)  const
     D inTableId                           like(GTTBID) const
     D*
     D/copy sPrDate
     D/copy sPrMessage
     D/copy sPrRunCmd
     D/copy sPrString
     D/copy sPrWrkWth
    C*
     D/copy wPrEmplr
     D/copy wPrRateSet
    C*
     C/COPY SCOPYWW41C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     c                   parm      *inkc         @@kc              1
     c                   parm      *inkl         @@kl              1
     C                   parm                    @@call
     c                   parm                    @@rrn             9 0
    C*
    C*  Add/change/display program parameter list
    C*
     C     AcdPl         plist
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
     C                   kfld                    kkType
     C                   kfld                    kkDesc
     C*
    C*====================================================================
    C* ChkSSM - Set indicator 11 for subset message
    C*====================================================================
     C     ChkSSM        begsr
     C*
     C                   eval      *in11 = ssType <> *blanks
     C*
     C                   endsr
     C*
    C*====================================================================
    C* ErrCk1 - Additional errors on subfile selection
    C*====================================================================
     C     ErrCk1        begsr
     C*
     C                   select
     C* Opt 4
     C                   when      scSel = '4'
     C                   if        ClmFeeColtDtl
     C                   seton                                        72  90
     C                   endif
     C*
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* ErrCk2 - Error checking for the subset screen
    C*====================================================================
     C     ErrCk2        begsr
     C                   endsr
     C*
    C*====================================================================
    C* INZSST - Initialize the subset fields
    C*====================================================================
     C     inzsst        begsr
     C*
     C                   clear                   ssType
     C*
     C                   endsr
     C*
    C*====================================================================
    C* KEYINZ - Initialize key values for program start
    C*====================================================================
     C     keyinz        begsr
     C*
     C                   eval      kkCo# = FNCO#
     C                   eval      kkFnd = FNFND
     C                   clear                   kkType
     C                   clear                   kkDesc
     C*
     C                   endsr
     C*
    C*====================================================================
    C* KEYSET - Set key values when doing "position to"
    C*====================================================================
     C     keyset        begsr
     C*
     C                   eval      kkType = scPst1
     C                   clear                   kkDesc
     C*
     C                   endsr
     C*
    C*====================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*====================================================================
     C     keysfl        begsr
     C*
     C                   eval      kkType = scType
     C                   eval      kkDesc = scDesc
     C*
     C                   endsr
     C*
      *===============================================================*
      * KpDo - Subroutine for F15=Mass copy
      *===============================================================*
     c     KpDo          begsr
     C*
     c                   move      'M'           ##mode                         Disp Mass cpy scr
     c                   call      AcdPgm        AcdPl                  99
     c                   exsr      KeDo
     C*
     c                   endsr
     C*
    C*====================================================================
    C* MvDbSc - Move data base fields to screen fields
    C*====================================================================
     C     MvDbSc        begsr
     C*
     C                   select
     C                   when      view# = 1 and info# = 1
     C                   eval      scData = FTTYPE + '  '
     C                                    + %subst(GenTableDesc(FTTYPE:'FEES')
     C                                            :1:30) + '  '
     C                                    + FTDESC + '  '
     C                   endsl
     C*
     C                   eval      scType = FTTYPE
     C                   eval      scDesc = FTDESC
     C*
     C                   endsr
     C*
    C*====================================================================
    C* Opt7 - Process option 7=Fee detail
    C*====================================================================
     C     Opt7          begsr
     C*
     C                   call      'W0063RA'
     C     *inkc         parm      *blank        ##kc
     C                   parm      *blank        ##kl
     C                   parm      @@Call        ##Call
     C                   parm      scDbRR        ##RRN
     C*
     C                   endsr
     C*
    C*====================================================================
    C* OTHFKS - Process other function keys
    C*====================================================================
     C     othfks        begsr
     C*
     C     *inkp         caseq     *on           KpDo
     c                   endcs
     C*
     C                   endsr
     C*
    C*====================================================================
    C* OthOpt - Process other options
    C*====================================================================
     C     OthOpt        begsr
     C*
     C     scSel         caseq     '7'           Opt7
     c                   endcs
     C*
     C                   endsr
     C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
     C     pgmend        begsr
     C                   eval       *inlr = *on
     C                   endsr
     C*
    C*====================================================================
    C* PGMSTR - Start of program processing
    C*====================================================================
     C     pgmstr        begsr
     C*
     c     @@rrn         chain(n)  wmfnp
     C                   exsr      KeyInz
     C*
     c                   eval      scCo#  = FNCO#
     c                   eval      scFnd  = FNFND
     c                   eval      scfnam = FNNAME
     C*
     C                   eval      sysDt7 = DateTo7(SystemDate)
     C*
     C                   endsr
     C*
    C*====================================================================
    C* READ   - Read next record from file
    C*====================================================================
     C     read          begsr
     C*
     C                   select
     C                   when      *in21
     C     key           reade     dbrcdf                                 95
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* READP  - Read previous record from file
    C*====================================================================
     C     readp         begsr
     C*
     C                   select
     C                   when      *in21
     C     key           readpe    dbrcdf                                 95
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SETGT  - Set greater than on file
    C*====================================================================
     C     setgt         begsr
     C*
     C                   select
     C                   when      *in21
     C     key1          setgt     dbrcdf                             96
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SETLL  - Set lower limits on file
    C*====================================================================
     C     setll         begsr
     C*
     C                   select
     C                   when      *in21
     C     key1          setll     dbrcdf                             96
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*====================================================================
     C     sf1upd        begsr
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
     C*
     C                   select
     C* Type
     C                   when      ssType <> *blank
     C                              and ssType <> FTTYPE
     C                   eval      meetss = *off
     C*
     C                   endsl
     C*
     C                   endsr





    P*================================================================
    P* Check for Claim Fee or Collateral Detail
    P*================================================================
     PClmFeeColtDtl    b
     DClmFeeColtDtl    pi             1n

     C     keyWTFIL      klist
     C                   kfld                    scCo#
     C                   kfld                    scFnd
     C                   kfld                    scType
     C                   kfld                    scDesc

     C     keyWTFIL      chain     pfi00

     C                   return    %found

     PClmFeeColtDtl    e





     P*====================================================================
     P* Get generic table description
     P*====================================================================
     PGenTableDesc     b
     DGenTableDesc     pi                  like(GTDESC)
     D inKey                               like(GTKEY)  const
     D inTableId                           like(GTTBID) const

     C     keyWTGTL      klist
     C                   kfld                    ##Co#
     C                   kfld                    inTableId
     C                   kfld                    inKey

     C                   clear                   GTDESC
     C     keyWTGTL      chain     pgt00
     C                   return    GTDESC

     PGenTableDesc     e
** Column headings
Opt  Fee type    Fee name                        Fee description
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YNN
1044=Delete                 YNN
1055=Display                YYY
1077=Fee detail             YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
215F15=Mass copy            YNN
217F17=Subset               YYY
3
