     H*  (C) Copyright StoneRiver, Inc., 2009
    H*
     H*****************************************************************
     H/TITLE W2150R - Work with Quote Status Types
     H*****************************************************************
     H* Created on 07/08/2009 by c:62 for Project 133WHN
     H*****************************************************************
     H* Revisions:
     H*
     H*
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw2150f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwtqsp     if   e             disk
     F                                     rename(pqs01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwtqsl3    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pqs01:dbrcdf)
    F*
    F*  Secondary logical file definition (for displaying items)
     Fwtqsl1    if   e           k disk
     F                                     infds(dbfds2)
     F                                     rename(pqs01:dbrcdf2)
    F*
    F*  Tertiary logical file definition (for displaying items)
     Fwtqsl2    if   e           k disk
     F                                     infds(dbfds3)
     F                                     rename(pqs01:dbrcdf3)
    D*
     D hd              s             78    dim(3) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Quote Status Types'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W2150R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   3
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
     D @@kc            s              1
     D @@kl            s              1
     D @@rrn           s              9  0
     D kkco#           s                   like(qsco#)
     D kkkey           s                   like(qskey)
     D kkqte           s                   like(qsqte)
     D kkdesc          s                   like(qsdesc)
     D kkaars          s                   like(qsaars)
    D*
     D                 ds
     D view1                         70
     D  desc1                        30    overlay(view1)
     D  key1                          1    overlay(view1:32)
     D  aars1                         3    overlay(view1:37)
     D  aarh1                        27    overlay(view1:41)
     D  qte1                          1    overlay(view1:70)
     D                 ds
     D view2                         70
     D  key2                          1    overlay(view2:2)
     D  desc2                        30    overlay(view2:5)
     D  aars2                         3    overlay(view2:37)
     D  aarh2                        27    overlay(view2:41)
     D  qte2                          1    overlay(view2:70)
     D                 ds
     D view3                         70
     D  aars3                         3    overlay(view3:3)
     D  aarh3                        27    overlay(view3:7)
     D  key3                          1    overlay(view3:36)
     D  desc3                        30    overlay(view3:41)
     D  qte3                          1    overlay(view3:70)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@rrn
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
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     qsky          klist
     C                   kfld                    ##co#
     C*
     C     qsky1         klist
     C                   kfld                    ##co#
     C                   kfld                    kkdesc
     C                   kfld                    kkkey
     C*
     C     qsky2         klist
     C                   kfld                    ##co#
     C                   kfld                    kkkey
     C                   kfld                    kkqte
     C*
     C     qsky3         klist
     C                   kfld                    ##co#
     C                   kfld                    kkaars
     C                   kfld                    kkkey
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C                   if        ssqte<>*blanks
     C                   seton                                        11
     C                   endif
     C*
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
     C                   if        ssqte<>'Q' and ssqte<>'W'
     C                   seton                                        7190
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
     C*
     C                   clear                   kkdesc
     C                   clear                   kkkey
     C                   clear                   kkqte
     C                   clear                   kkaars
     C*
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C                   select
     C                   when      *in21
     C                   movel(p)  scpst1        kkdesc
     C                   when      *in22
     C                   movel(p)  scpst2        kkkey
     C                   when      *in23
     C                   move (p)  scpst3        kkaars
     C                   endsl
     C*
     c                   clear                   kkqte
     C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   movel(p)  scdesc        kkdesc
     C                   movel(p)  sckey         kkkey
     C                   z-add     scaars        kkaars
     c                   movel(p)  scqte         kkqte
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      scdesc=qsdesc
     C                   eval      sckey=qskey
     C                   eval      scaars=qsaars
     C                   eval      scqte=qsqte
     C*
     c                   select
     c                   when      *in21
     c                   movel(p)  qsdesc        desc1
     c                   movel(p)  qskey         key1
     c                   eval      aars1 = %editw(qsaars:'0   ')
     c                   movel(p)  qsaarh        aarh1
     c                   movel(p)  qsqte         qte1
     c                   eval      scdata=view1
     c                   when      *in22
     c                   movel(p)  qsdesc        desc2
     c                   movel(p)  qskey         key2
     c                   eval      aars2 = %editw(qsaars:' 0 ')
     c                   movel(p)  qsaarh        aarh2
     c                   movel(p)  qsqte         qte2
     c                   eval      scdata=view2
     c                   when      *in23
     c                   movel(p)  qsdesc        desc3
     c                   movel(p)  qskey         key3
     c                   eval      aars3 = %editw(qsaars:' 0 ')
     c                   movel(p)  qsaarh        aarh3
     c                   movel(p)  qsqte         qte3
     c                   eval      scdata=view3
     c                   endsl
     C*
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
     c                   seton                                        lr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C*
     C                   select
     C                   when      *in21
     C     qsky          reade     dbrcdf                                 95
     C                   when      *in22
     C     qsky          reade     dbrcdf2                                95
     C                   when      *in23
     C     qsky          reade     dbrcdf3                                95
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C*
     C                   select
     C                   when      *in21
     C     qsky          readpe    dbrcdf                                 95
     C                   when      *in22
     C     qsky          readpe    dbrcdf2                                95
     C                   when      *in23
     C     qsky          readpe    dbrcdf3                                95
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C*
     C                   select
     C                   when      *in21
     C     qsky1         setgt     dbrcdf                             96
     C                   when      *in22
     C     qsky2         setgt     dbrcdf2                            96
     C                   when      *in23
     C     qsky3         setgt     dbrcdf3                            96
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C*
     C                   select
     C                   when      *in21
     C     qsky1         setll     dbrcdf                             96
     C                   when      *in22
     C     qsky2         setll     dbrcdf2                            96
     C                   when      *in23
     C     qsky3         setll     dbrcdf3                            96
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C                   endsr
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C*
     C                   eval      @@rrn=scdbrr
     C*
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C                   if        ssqte<>*blanks and ssqte<>qsqte
     C                   eval      meetss=*off
     C                   endif
     C*
     C                   endsr
** Column headings
Opt  Description                   Sts Order Report Heading              Qte
Opt  Sts Description                   Order Report Heading              Qte
Opt  Order Report Heading              Sts   Description                 Qte
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
211F11=Change View          YYY
212F12=Cancel               NNY
217F17=Subset               YYY
3
