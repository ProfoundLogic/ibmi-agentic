     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*
     H/TITLE F2402R
     H**************************************************************************************
     H*  Description:  FS Rate Table                                                       *
     H*                                                                                    *
     H*  Called from:                                                                      *
     H*  F2412R                                                                            *
     H*                                                                                    *
     H*  Calls to:                                                                         *
     H*  F2402R01                                                                          *
     H*                                                                                    *
     H*  Created: 12/09/09   Programmer C:198   Project:TX064                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     A*  08/04/16 C:079 Support: MC099 42706                                               *
     A*                 Fix paging issue                                    ied ***        *
     H*                                                                                    *
     A*  09/09/14 C:079 Support: MC099 86740                                               *
     A*                 Return inkc and inkl correctly                      ied ***        *
     H*                                                                                    *
     A*  12/06/10 C:198 Support: 75046                                                     *
     A*                 fixed display of position to type field. *** recomplied ***        *
     A*                                                                                    *
     A*  10/01/10 C:198 Project: WV009                                                     *
     A*                 Added field for Medicare release number                            *
     A*                                                                                    *
     A*  07/28/10 C:248 SUPPORT: 73333                                                     *
     A*                 Add additional screen fileds                                       *
     A*                                                                                    *
     H*  xx/xx/xx C:xxx  Project/Support: xxxxxx                                           *
     H*                                                                                    *
     H**************************************************************************************
    H*
     Ff2402f    cf   e             workstn
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fftrtp     if   e             disk
     F                                     rename(prt01:pfrcdf)
    F*
     Fststp     if   e             disk
     Fftgtl     if   e           k disk
     Fftgtp     if   e             disk    rename(pgt01:pgt02)
    F*  Primary logical file definition (for displaying items)
     Fftrtl2    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(prt01:dbrcdf)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'FS Rate Table'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2402R01'
     D comapi          c                   'FA007R'
     D uspapi          c                   'F0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D*** Beg Chg *** 08/04/16 ******************************************
     D**** sf1pag          c                   12
     D sf1pag          c                   10
     D*** End Chg *** 08/04/16 ******************************************
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
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D kkst            s                   like(rtst)
     D kkefdt          s                   like(rtefdt)
     D kktype          s                   like(rttype)
     D kkkey           s                   like(gtkey)
     D kktbid          s                   like(gttbid)
     C***  BEG ADD *** 10/01/10  *************************************
     D kkmrel          s                   like(rtmrel)
     C***  BEG ADD *** 10/01/10  *************************************
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
     C*** Beg Chg *** 09/09/14 *********************************************
     C****************   parm                    @@kc              1
     C****************   parm                    @@kl              1
     C                   parm      *inkc         @@kc              1
     C                   parm      *inkl         @@kl              1
     C*** End Chg *** 09/09/14 *********************************************
     C                   parm                    @@rrn             9 0
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm      kkst          ##st              2
     C*****  BEG ADD ***** 07/28/10  *********************************
     C                   parm                    stname
     C*****  END ADD ***** 07/28/10  *********************************
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C*
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *blanks       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkst
     C                   kfld                    kktype
     C                   kfld                    kkefdt
     C***  BEG ADD *** 10/01/10  *************************************
     C                   kfld                    kkmrel
     C***  END ADD *** 10/01/10  *************************************
     C*
     C     gtky          klist
     C                   kfld                    ##co#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   if        sstype <> *blanks or ssefdt <> 0 or
     C                             ssstat <> ' '
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
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
     C*
     C                   eval      *in57 = *off
     C                   eval      *in58 = *off
     C                   eval      *in59 = *off
     C*
     C                   if        sstype <> *blanks
     C                   movel     'FSRT'        kktbid
     C                   movel     sstype        kkkey
     C*
     C     gtky          chain     pgt01                              99
     C                   if        *in99 = *on
     C                   eval      *in59 = *on
     C                   eval      *in90 = *on
     C                   endif
     C*
     C                   endif
     C*
     C                   if        ssefdt <> *zeros
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         ssefdt
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
    C* KJDO   - F10 on type on subset screen
    C*================================================================
     C     kjdo          begsr
     C                   select
     C                   when      hfld = 'SSTYPE'
     C                   call      'F2380R'
     C                   parm      '2'           ##call            1
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm      0             xxrrn             9 0
     C                   parm      'FSRT'        ##tbid            4
     C                   if        xxrrn <> 0
     C     xxrrn         chain     pgt02
     C                   movel     gtkey         sstype
     C                   seton                                        9061
     C                   else
     C                   seton                                        9060
     C                   endif
     C                   endsl
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C                   movel     'A'           ssstat
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C     @@rrn         chain     pst01                              99
     C                   movel     stkey         kkst
     C*****  BEG ADD ***** 07/28/10  *********************************
VCM01C                   movel     stkey         stkey#
VCM01C                   movel     stname        stname#
     C*****  END ADD ***** 07/28/10  *********************************
     C                   move      *loval        kktype
     C                   move      *loval        kkefdt
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C   21              eval      kktype = scpst1
     C   21              eval      kkefdt = 0
     C                   if        *in21 = *on
     C                   z-add     scpstd        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   movel     srdto         kkefdt
     C                   endif
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C   21              movel(p)  sctype        kktype
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C   21              movel     srdto         kkefdt
     C                   endif
     C***  BEG ADD *** 10/01/10  *************************************
     C   21              move      scmrel        kkmrel
     C***  END ADD *** 10/01/10  *************************************
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   movel     rttype        sctype
     C                   z-add     rtefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C***  BEG ADD *** 10/01/10  *************************************
     C                   movel     rtmrel        scmrel
     C***  END ADD *** 10/01/10  *************************************
     C                   movel     rtstat        scstat
     C*
     C*** BEG CHG *** 10/01/10 ****************************************
     C***********        eval      scdata = '   ' +
     C***********                  sctype + '       '
     C***********                  + %editw(scefdt:'  /  /    '  ) +
     C***********                   '                            ' +
     C***********                   '                 '
     C***********                  + scstat
     C                   eval      scdata = '   ' +
     C                             sctype + '       '
     C                             + %editw(scefdt:'  /  /    '  ) +
     C                              '          ' + scmrel + '    ' +
     C                              '                              '
     C                             + scstat
     C*** END CHG *** 10/01/10 ****************************************
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
     C     scsel         caseq     '20'          opt20
     C                   endcs
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   seton                                        lr
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
     C     kkst          reade     dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C     kkst          readpe    dbrcdf                                 95
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
     C                   endsr
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C                   endsr
    C*================================================================
    C* OPT20  - Change status
    C*================================================================
     C     opt20         begsr
     C                   move      'S'           ##mode
     C                   call      acdpgm        acdpl                    99
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C                   if        sstype <> *blanks and sstype <> rttype
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   movel     srdto         xxefdt            7 0
     C                   endif
     C*
     C                   if        ssefdt <> *zeros and xxefdt <> rtefdt
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   if        ssstat <> ' ' and ssstat <> rtstat
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   endsr
    C*================================================================
** Column headings
Opt   Type      Effective date   Release                            Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YNY
217F17=Subset               YYY
3
