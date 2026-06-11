     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE F2374R : WORK WITH PROVIDER PARISH SALES TAX.
     H*****************************************************************
     H*****************************************************************
     H*  DESCRIPTION: WORK WITH PROVIDER PARISH SALES TAX.            *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    F0032R                                                     *
     H*  CALLS TO:                                                    *
     H*    F2374R01                                                   *
     H*                                                               *
     H*  CREATED: 09/29/10  PROGRAMMER: C:257 PROJECT: C133MJN        *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*  XX/XX/XX  C:XXX  XXXXXXX: XXXXXX                             *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      /ENDIF
     Ff2374f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Ffmp3pla   if   e             disk
     F                                     rename(pp301:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Ffmp3l2la  if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pp301:dbrcdf)
     F*fmp3l1la  if   e           k disk
     Ffmpdp     if   e             disk
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Provider Parish Sales Tax'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2374R01'
     D comapi          c                   'FA007R'
     D uspapi          c                   'F0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   09
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
     D @@rrn           s              9  0
     D @@call          s              1
     D kkfein          s                   like(p3fein)
     D kkmod           s                   like(p3mod)
     D kkprsh          s                   like(p3prsh)
     D kkefdt          s                   like(p3efdt)
     D@@PST1           s              8p 0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    D*---------------------------------------------------------------------
    D* Data structure
    D*---------------------------------------------------------------------
    Ddtl              ds                                                       Detail
    DParish                   1     10
    DCity                    13     37
    DEftdate                 44     53
    Dsaletax                 60     64
    Dprcntg                  65     65
    D*
    Ddsefdt1          ds
    Ddefdt1                   1      2
    Ddefdt2                   3      4
    Ddefdt3                   5      8
    D*
    Ddsefdt           ds
    Defdt1                    1      2
    Defdt2                    3      3
    Defdt3                    4      5
     defdt4                    6      6
     defdt5                    7     10
    D*
     D                 ds
     D dsprovider                    55
     D  dfein                  1      9a
     D  dmod                  11     14a
     D  dname                 16     55a
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    ##rrn
    C*
    C     dtcymd        plist
    C                   parm                    srdti
    C                   parm      *blanks       srffmt
    C                   parm      '*CYMD'       srtfmt
    C                   parm                    srdto
    C                   parm                    srerr
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
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
     C     key1          klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkefdt
     C*
     C     key2          klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C*
     C     key3          klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkprsh
     C                   kfld                    kkefdt
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   if        ssprsh <> ' ' or ssefdt <> 0 or ssstat <> ' '
     C                   seton                                        11
     C                   endif
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C*
     C                   eval      *in57 = *off
     C                   eval      *in58 = *off
     C*
     C                   if        ssefdt <> *zeros
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C*
     C                   if        srerr = '1'
     C                   eval      *in57 = *on
     C                   eval      *in90 = *on
     C                   endif
     C                   endif
     C*
     C                   if        ssstat <> ' '
     C                             and ssstat <> 'A' and ssstat <> 'I'
     C                   eval      *in58 = *on
     C                   eval      *in90 = *on
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   movel     dfein         kkfein
     C                   movel     dmod          kkMOD
     C                   movel     *LOVAL        kkEFDT
     C
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   if        *in21 = *on
     C                   move      SCPST1        @@PST1
     C                   z-add     @@PST1        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   MOVEL     SRDTO         kkefdt
     C                   endif
     C                   endif
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   MOVE      scPST1        @@PST1
     C                   z-add     @@PST1        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C*
     C                   if        srerr = '0'
     C                   MOVEL     SRDTO         kkefdt
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   clear                   scdata
     C                   movel     p3prsh        Parish
     C                   movel     p3cty         city
     C*
     C                   z-add     p3efdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         dsefdt1
     C                   Movel     defdt1        efdt1
     C                   Movel     '/'           efdt2
     C                   Movel     defdt2        efdt3
     C                   Movel     '/'           efdt4
     C                   Movel     defdt3        efdt5
     C                   Movel     dsefdt        eftdate
     C*
     c                   eval      saletax =  %editc(p3tax : 'J')
     c                   movel     '%'           prcntg
     C                   movel     dtl           scdata
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
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
     C                   eval      @@rrn = ##rrn
     C* Load iformation of Provider/Provider type/State in header
     C     @@rrn         setll     PPD01
     C                   Read      PPD01
     C                   movel     pdfein        dfein
     C                   movel     pdmod         dmod
     C                   movel     pdfein        kkfein
     C                   movel     pdmod         kkmod
     C*                  movel     *loval        kkefdt
     C*                  movel     *blank        kkprsh
     C                   movel     pdname        dname
     C                   movel     dsprovider    scprvdr                        provider
     C                   movel     pdst          scst                           state
     C                   movel     pdtype        scprvdt                        provider type
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C     key2          reade     dbrcdf                                 95
     C                   endif
     C                   endsr
    C*
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C     key2          readpe    dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endif
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   endif
     C                   endsr
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C                   eval      scdata = '   *Deleted   '
     C                   endsr
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
      *
     C                   if        SSPRSH <> *BLANKS and SSPRSH <> p3PRSH
     C                   eval      meetss = *off
     C                   endif
      *
     C                   if        ssefdt <>  *zeros and xxefdt <> p3efdt
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   MOVEL     SRDTO         xxefdt            7 0
     C                   endif
     C                   eval      meetss = *off
     C                   endif
      *
     C                   if        ssstat <> ' ' and ssstat <> p3stat
     C                   eval      meetss = *off
     C                   endif
      *
     C                   endsr
** Column headings
Opt  Parish      City                       Effective Date   Sales Tax
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
217F17=Subset               YYY
3
