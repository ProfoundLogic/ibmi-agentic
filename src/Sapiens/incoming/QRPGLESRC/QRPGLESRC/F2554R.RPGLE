     H**************************************************************************************
     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H**************************************************************************************
     H/TITLE F2552R                                                                        *
     H**************************************************************************************
     H*  Description:  Work With ICD10 Codes                                               *
     H*                                                                                    *
     H*  Created: 08/05/13   Programmer C:277   PROJECT: MC329 Phase II                    *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*
     H*  09/21/15 C:079 Support - PHW36861
     H*                 Subset on code range ICD-10 Codes not working
     H*
     H*  09/15/15 C:309 Support - PHW36861
     H*                 Subset/Position To on ICD-10 Codes not working
     H*
     H*  04/16/15 C:212 Project: MC395
     H*                 ICD10 TX 837 EDI Changes.
     H*                 DSPF SCPST1 changed to 7
     H*                                                                                    *
     H*                                                                                    *
     H**************************************************************************************
    H*
     Ff2554f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Maine DRG Production
     Ffmicd10p  if   e             disk
     F                                     rename(pic01:pfrcdf)
    F*
    F*  Maine DRG Production - Logical
     Ffmicd10l1 if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pic01:dbrcdf)
     F
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
     D lo              C                   'abcdefghijklmnopqrs-
     D                                     tuvwxyz'
     D up              C                   'ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ'
    D*================================================================
    D*  /COPY required constants
    D*================================================================
      /COPY SCOPYWW01D
     D optnm           c                   'ICD10 Codes'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2554R01'
     D comapi          c                   'FA007R'
     D uspapi          c                   'F0006R'
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
     D @@kc            s              1
     D @@kl            s              1
     D @@ICd10         s              7
     D xxdesc          s             50
     D kkcode          s                   like(iccode)
     D kkind           s                   like(ICind)
     D****** Beg Chg *** 09/15/15 *************************************
     D*** u1pdsc          s                   like(ICdsc1)
     D*** u2pdsc          s                   like(Icdsc2)
     D u1pdsc          s             50
     D u2pdsc          s             50
     D****** End Chg *** 09/15/15 *************************************
     D @@Lost          s                   like(ICind)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *blanks       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
    C*
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@kc
     C                   parm                    @@kl
     C                   parm                    @@ICd10
     C                   parm                    @@Lost
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
     C     fmicd10l1key  klist
     C                   kfld                    kkcode
     C                   kfld                    kkind
     C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C****************************************************************
    C*  CHKSSM - Set indicator 11 for subset message
    C****************************************************************
     C     chkssm        Begsr
     C*
     C                   if        ssdesc <> ' ' or ssind <> ' ' or
     C                             ssstat <> ' ' or sscod1 <> ' '
     C                             or sscod2 <> ' '
     C                   eval      *in11 = *on
     C                   endif
     C*
     C                   EndSr
    C*******************************************************************
    C** ERRCK1 - Additional errors on subfile selection
    C*******************************************************************
     C     errck1        BegSr
     C                   EndSr
    C*****************************************************************
    C** ERRCK2 - Error checking for the subset screen
    C*****************************************************************
     C     errck2        BegSr
     C*
     C                   eval      *in57 = *off
     C                   eval      *in58 = *off
     C*
     C                   if        ssind <> *blanks
     C                             and ssind <> 'D' and ssind <> 'O'
     C                   eval      *in57 = *on
     C                   eval      *in90 = *on
     C                   endif
     C
     C                   if        ssstat <> ' '
     C                             and ssstat <> 'A' and ssstat <> 'I'
     C                   eval      *in58 = *on
     C                   eval      *in90 = *on
     C                   endif
     C
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C                   eval      SSSTAT = 'A'
     C                   eval      SSind = *Blanks
     C                   If        @@call <> ' ' and @@LOST <> *blanks
     C                   eval      SSind = @@LOST
     C                   eval      @@LOST = *Blanks
     C                   endif
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   eval      kkcode = *loval
     C                   eval      kkind  = 'D'
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C****** Beg Add *** 09/15/15 *************************************
     C     lo:up         Xlate(p)  SCPST1        SCPST1
     C****** End Add *** 09/15/15 *************************************
     C                   if        *in21 = *on
     C                   eval      kkcode = SCPST1
     C                   eval      kkind = *LOVAL
     C                   elseif    *in22 = *on
     C                   eval      u1pdsc = SCPST2
     C                   endif
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   if        *in21 = *on
     C                   movel     scCOde        kkcode
     C                   movel     scINd         kkind
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   eval      scCOde = ICcode
     C                   eval      scDEsc = %subst(ICdsc1:1:25)
     C                   Movel     ICstat        scSTat
     C                   Movel     ICind         scINd
     C                   Movel     ICvld         scvld
     C                   Movel     ICfcode       scFCODE
     C*
     C                   eval      scdata = scCOde + ' ' + scvld + '         ' +
     C                               scdesc + '      ' + scINd + '        '
     C                               + scFCODE
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
    C*
    C*
     C                   If        Scsel = '20'
     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl                    99
     C                   endif
    C*
    C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C*
     C                   eval      @@kc = *inkc
     C                   eval      @@kl = *inkl
     C*
     C                   eval      *inlr = *on
     C                   return
     C*
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
     C                   if        *in21 = *on
     C                   read      dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C                   readp     dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   if        *in21 = *on
     C   21fmicd10l1key  setgt     dbrcdf                             96
     C                   endif
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   if        *in21 = *on
     C   21fmicd10l1key  setll     dbrcdf                             96
     C                   endif
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
     C                   if        scSEl = ' 1' or scSEl = '1 '
     C                             or scSEl = '01'
     C     scdbrr        chain     pfrcdf
     C                   eval      @@ICd10 = ICcode
     C                   eval      @@Lost  = ICind
     C                   else
     C                   eval      @@ICd10 = *blanks
     C                   eval      @@Lost  = *blanks
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
B01  C                   if        ssdesc <> *blanks
     C*
     C                   Movel     ICDSC1        xxdesc
     C     lo:up         xlate(p)  xxdesc        u1pdsc
     C     lo:up         xlate(p)  ssdesc        u2pdsc
     C*
     C                   exsr      string
     C*
E01  C                   endif                                                  ssdesc <> *blanks
      *
      *
     C                   if        ssind <> ' ' and
     C                             ssind <>  ICind
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   if        ssstat <> ' ' and ssstat <> icSTat
     C                   eval      meetss = *off
     C                   endif
     C*** Beg Add *** 09/21/15 ************************************
B01  C                   if        sscod1 <> *blanks and sscod1 >
     C                             iccode
     C                   eval      meetss = *off
     C                   endif
     C*
B01  C                   if        sscod2 <> *blanks and sscod2 <
     C                             iccode
     C                   eval      meetss = *off
     C                   endif
     C*** End Add *** 09/21/15 ************************************
      *
     C                   endsr
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     string        begsr
     C*
     C                   call      'QCLSCAN'
     C                   parm                    U1PDSC
     C                   parm      50            STRLEN            3 0
     C                   parm      1             START             3 0
     C                   parm                    U2PDSC
     C                   parm      50            PATLEN            3 0
     C                   parm      '0'           TRANS             1
     C                   parm      '1'           TRIM              1
     C                   parm      ' '           WILD              1
     C                   parm                    SCNRES            3 0
     C*
     C                   if        scnres = 0
     C                   move      *off          meetss
     C                   endif                                                  scrnes=0
     C*
     C                   endsr                                                  string
     C*
** Column headings
Opt Code    Specific  Description                    Type     Full code    Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YNY
217F17=Subset               YYY
12020=Change status         YNN
3
