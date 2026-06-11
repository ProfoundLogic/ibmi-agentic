     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE F8287R
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H**************************************************************************
    H*  F8287R - First Health EOR Financial Work with Bills with Errors
    H*
    H*  Called from: Menu
    H*  Calls to   : F8287R01 - First Health EOR Financial Bills with Errors Up
    H*
     H**************************************************************************
    H*  Created on 05/24/2006 by C:163 for Project 139MCV from client 139
    H*  Based on F8267R from Client 135
     H**************************************************************************
    H*  Revisions:
    H*
     H*  06/09/20 C:295  Project : SY420
     H*                  Mask SSN
     H*
     H**************************************************************************
    H*  Indicators used:
    H*
     H**************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff8287f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Ffax4p     if   e             disk
     F                                     rename(px401:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Ffax4l3    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(px401:dbrcdf)
    F*
    F*  Secondary logical file definition (for displaying items)
     Ffax4l4    if   e           k disk
     F                                     infds(dbfds2)
     F                                     rename(px401:dbrcdf2)
     Ffax4l7    if   e           k disk
      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
    D*
     D hd              s             79    dim(3) ctdata perrcd(1)
     D of              s              1    dim(13) ctdata perrcd(1)
     D ofd             s             35    dim(13) alt(of)
    D*
     D dbfdsa          ds
     D dbrrna                397    400b 0
    D*
    D*================================================================
    D*  User defined data structures
    D*================================================================
     D                 ds
     D ss#in                          9
     D  ss#1in                        3    overlay(ss#in)
     D  ss#2in                        2    overlay(ss#in:4)
     D  ss#3in                        4    overlay(ss#in:6)
     D                 ds
     D ss#ou                         11
     D  ss#1ou                        3    overlay(ss#ou)
     D  hyph1                         1    inz('-')
     D                                     overlay(ss#ou:4)
     D  ss#2ou                        2    overlay(ss#ou:5)
     D  hyph2                         1    inz('-')
     D                                     overlay(ss#ou:7)
     D  ss#3ou                        4    overlay(ss#ou:8)
     D                 ds
     D datin                          8
     D  dat1in                        2    overlay(datin)
     D  dat2in                        2    overlay(datin:3)
     D  dat3in                        4    overlay(datin:5)
     D                 ds
     D datou                         10
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   const('First Health Bills')
     D slmsgi          c                   const('MC49998')
     D slmsgf          c                   const('FSMESG')
     D acdpgm          c                   const('F8287R01')
     D comapi          c                   const('FA007R')
     D uspapi          c                   const('F0006R')
     D chgnum          c                   const('SA020R')
     D infomx          c                   const(1)
     D viewmx          c                   const(2)
     D sf1pag          c                   const(11)
     D delete          c                   const('*** DELETED ***-
     D                                      DELETED *** D-
     D                                     ELETED ***')
     D trnsfr          c                   const('**TRANSFERRED*T-
     D                                     RANSFERRED*TRA-
     D                                     NSFERRED**')
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##call          s              1
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##fyr           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mask          s             10
     D ##mode          s              1
     D ##num           s             80
     D ##rrn           s              9  0
     D ##rtcd          s              1
     D ##sub#          s              5  0
     D @@call          s              1
     D @@kc            s              1
     D @@kl            s              1
     D @@rrn           s              9  0
     D kkblid          s                   like(x4cntl)
     D kkdoi           s                   like(x4acdt)
     D kkssn           s                   like(x4ss#)
     D updlin          s              1
     D xxdate          s              8
     D xxdateiso       s               d   datfmt(*ISO)
     D xxssid          s             23
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW01C
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
     C                   parm                    ##call
     C                   parm      *blanks       ##rtcd
    C*
    C*  Change numeric field to character field.
    C*
     C     cnumpl        plist
     C                   parm                    ##num
     C                   parm      *blanks       ##mask
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C                   parm                    ##sub#
     C                   parm                    ##fyr
    C*
     C     dtmdy2        plist
     C                   parm                    srdti
     C                   parm      '*YYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      '*MDYY'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkblid
    C*
     C     key2          klist
     C                   kfld                    kkssn
     C                   kfld                    kkdoi
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
    C*
     C                   if        ssssn <> *blanks
     C                             or ssprfl <> *blanks
     C                             or ssprfl <> *blanks
     C                   eval      *in11 = *on
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* CNTERR - Count Errors
    C*================================================================
     C     cnterr        begsr
    C*
     C                   clear                   scecnt
     C                   clear                   scrcnt
     C                   clear                   scpcnt
    C*
     C     *loval        setll     px401
     C                   read      px401                                  99
     C                   dow       *in99 = *off
    C*
     C                   select
     C                   when      x4prfl = 'E'
     C                   eval      scecnt = scecnt + 1
     C                   when      x4prfl = 'P'
     C                   eval      scpcnt = scpcnt + 1
     C                   when      x4prfl = 'R'
     C                   eval      scrcnt = scrcnt + 1
     C                   endsl
    C*
     C                   read      px401                                  99
     C                   enddo
    C*
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
    C*
     C                   if        *in90 = *off
     C                   if        scsel = '4'
     C                   if        scprfl = 'E'
     C                             or scprfl = 'R'
     C                             or scprfl = 'P'
     C                   movel     '99'          scsel
     C                   else
     C                   eval      *in90 = *on
     C                   eval      *in71 = *on
     C                   endif
     C                   endif
     C                   endif
    C*
     C                   if        *in90 = *off
     C                   if        scsel = '6'
     C                             and scprfl <> 'E'
     C                             and scprfl <> 'R'
     C                             and scprfl <> 'P'
     C                   eval      *in90 = *on
     C                   eval      *in72 = *on
     C                   endif
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
    C*
     C                   if        ssprfl <> 'E'
     C                             and ssprfl <> 'P'
     C                             and ssprfl <> 'R'
     C                             and ssprfl <> ' '
     C                   eval      *in89 = *on
     C                   eval      *in90 = *on
     C                   endif
    C*
     C                   if        ssrcne <> '1'
     C                             and ssrcne <> '2'
     C                             and ssrcne <> ' '
     C                   eval      *in88 = *on
     C                   eval      *in90 = *on
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
    C*
    C*
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
    C*
     C                   clear                   kkblid
     C                   clear                   kkssn
     C                   clear                   kkdoi
    C*
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
    C*
     C                   select
     C                   when      *in21 = *on
     C                   movel(p)  scpst1        kkblid
     C                   when      *in22 = *on
     C                   movel(p)  scpst2        kkssn
     C                   move(p)   scpst3        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel(p)  srdto         kkdoi
     C                   endsl
    C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
    C*
     C                   movel(p)  scblid        kkblid
     C                   movel(p)  scssn         kkssn
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                movel(p)  scssn         kkssn
      /Free
       kkssn = UnMaskData('SSN':'kkssn':scssn);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   movel(p)  scdoi         kkdoi
    C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
    C*
     C                   if        updlin = *off
    C*
     C                   movel(p)  x4cntl        scblid
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                movel(p)  x4ss#         scssn
      /Free
       scssn = MaskData('SSN':'x4ss#':x4SS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   movel(p)  x4acdt        scdoi
     C                   movel(p)  x4prfl        scprfl
    C*
     C                   movel     x4ss#         ss#in
     C                   movel     ss#1in        ss#1ou
     C                   movel     ss#2in        ss#2ou
     C                   movel     ss#3in        ss#3ou
    C*
     C                   if        x4acdt <> 0
     C     *cymd         movel     x4acdt        xxdateiso
     C     *usa          move      xxdateiso     datou
     C                   endif
    C*
     C                   movel(p)  ss#ou         xxssid
     C                   move      datou         xxssid
    C*
     C                   if        *in21 = *on
     C                   movel(p)  x4cntl        scdata
     C                   move      xxssid        scdata
     C                   endif
    C*
     C                   if        *in22 = *on
     C                   movel(p)  xxssid        scdata
     C                   move      x4cntl        scdata
     C                   endif
    C*
     C                   movel     x4prfl        scerr
     C                   movel     x4rcne        scrcne
    C*
     C                   endif
    C*
     C                   movel     *off          updlin
    C*
     C                   endsr
    C*================================================================
    C* OPT6 - Transfer to Fiserv
    C*================================================================
     C     opt6          begsr
    C*
     C                   movel     *off          updlin
     C                   move      'C'           ##mode
     C                   move      ' '           ##call
     C                   call      acdpgm        acdpl                  99
     C                   if        *inkl = *off
     C                             and *inkc = *off
    C*
     C                   if        ##rtcd <> *blanks
     C                   movel     'P'           scprfl
     C                   movel     'P'           scerr
     C                   else
     C                   movel(p)  trnsfr        scdata
     C                   movel     'Y'           scprfl
     C                   endif
     C                   movel     *on           updlin
    C*
     C                   endif
     C                   clear                   ##call
     C                   exsr      cnterr
    C*
     C                   endsr
    C*================================================================
    C* OPT99 - Delete Bill
    C*================================================================
     C     opt99         begsr
    C*
     C                   movel     *off          updlin
     C                   move      'C'           ##mode
     C                   move      'D'           ##call
     C                   call      acdpgm        acdpl                  99
     C                   if        *inkl = *off
     C                             and *inkc = *off
    C*
     C                   movel(p)  delete        scdata
     C                   movel     'Y'           scprfl
     C                   movel     *on           updlin
    C*
     C                   endif
     C                   clear                   ##call
     C                   exsr      cnterr
    C*
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
     C     scsel         caseq     '6'           opt6
     C     scsel         caseq     '99'          opt99
     C                   endcs
    C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
    C*
     C                   eval      *inlr = *on
    C*
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C                   movel     *off          updlin
     C                   exsr      cnterr
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
    C*
     C                   if        *in21 = *on
     C                   read      dbrcdf                                 95
     C                   endif
     C                   if        *in22 = *on
     C                   read      dbrcdf2                                95
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
    C*
     C                   if        *in21 = *on
     C                   readp     dbrcdf                                 95
     C                   endif
     C                   if        *in22 = *on
     C                   readp     dbrcdf2                                95
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
    C*
     C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endif
     C                   if        *in22 = *on
     C     key2          setgt     dbrcdf2                            96
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
    C*
     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   endif
     C                   if        *in22 = *on
     C     key2          setll     dbrcdf2                            96
     C                   endif
    C*
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
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
    C*
     C                   if        ssssn <> *blanks
     C                             and ssssn <> x4ss#
     C                   move      *off          meetss
     C                   endif
    C*
     C                   if        ssprfl <> *blanks
     C                   if        ssprfl <> x4prfl
     C                   move      *off          meetss
     C                   endif
     C                   endif
    C*
     C                   if        ssrcne <> *blanks
     C                   if        ssrcne <> x4rcne
     C                   move      *off          meetss
     C                   endif
     C                   endif
    C*
     C                   endsr
** Column headings
Opt Bill ID                    SS#         Injury Date  Type       Type
Opt     SS#     Injury Date  Bill ID                    Type       Type
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 YNN
1055=Display                YYY
1066=Transfer to Fiserv     YNN
203F3=Exit                  YYN
205F5=Refresh               YNN
206F6=Add                   NNN
211F11=Chg View             YYY
212F12=Cancel               NNY
217F17=Subset               YYY
3
