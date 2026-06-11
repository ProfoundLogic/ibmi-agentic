     H*Copyright('(c) 2011 StoneRiver, Inc. All rights reserved. -                         *
     H*StoneRiver is a trademark of StoneRiver, Inc.')                                     *
     H/TITLE F2366R                                                                        *
     H**************************************************************************************
     H*  Description:  Medicare RVU Table file                                             *
     H*                                                                                    *
     H*  Created: 07/03/09   Programmer C:227   Project:AR038                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  10/11/10  C:280  Project: seq78229                                                *
     H*                   Added option 20 for status change                                *
     H*                                                                                    *
     H*  10/01/10  C:198  Project: WV009                                                   *
     H*                   Added medicare release level on work with screen                 *
     H*                                                                                    *
     H*  08/12/10  C:198  Project: 114MWS                                                  *
     H*                   Added functionality to return RRN on being invoked in case       *
     H*                   select screen.                                                   *
     H*                                                                                    *
     H*  08/28/09  C:227  Project: AR038                                                   *
     H*                   Subset error check changes.                                      *
     H*                                                                                    *
     H**************************************************************************************
    H*
     H actgrp(*caller)
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2366f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fftrvp     if   e             disk
     F                                     rename(prv01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fftrvl1    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(prv01:dbrcdf)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D*** BEG ADD *** 10/11/10 ****************************************
     D*of              s              1    dim(10) ctdata perrcd(1)
     D*ofd             s             35    dim(10) alt(of)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
     D*** END ADD *** 10/11/10 ****************************************
     D lo              C                   'abcdefghijklmnopqrs-
     D                                     tuvwxyz'
     D up              C                   'ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ'
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Medicare RVU Table'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2366R01'
     D comapi          c                   'FA007R'
     D uspapi          c                   'F0006R'
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
     D @@call          s              1
     D*** BEG ADD *** 10/01/10 ****************************************
     D @@rel           s              1
     D*** END ADD *** 10/01/10 ****************************************
     D kkprcd          s                   like(rvprcd)
     D kkmod1          s                   like(rvmod1)
     D kkefdt          s                   like(rvefdt)
     D*** BEG ADD *** 08/28/09 *********************************************
     D s1efdt          s                   like(rvefdt)
     D*** END ADD *** 08/28/09 *********************************************
     D u1pdsc          s                   like(rvdesc)
     D u2pdsc          s                   like(rvdesc)
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
     C*** Beg Add *** 08/12/10 ******************************************
     C                   parm                    @@kc              1
     C                   parm                    @@kl              1
     C                   parm                    @@rrn             9 0
     C*** End Add *** 08/12/10 ******************************************
     C*** BEG ADD *** 10/01/10 ******************************************
     C                   parm                    @@rel             1
     C*** END ADD *** 10/01/10 ******************************************
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
     C     key1          klist
     C                   kfld                    kkprcd
     C                   kfld                    kkmod1
     C                   kfld                    kkefdt
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   if        ssdesc <> *blank or ssefdt <> 0 or
     C                             ssstat <> ' '
     C*** BEG ADD *** 10/01/10 ****************************************
     C                             or ssmrel <> ' '
     C*** END ADD *** 10/01/10 ****************************************
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
     C                   movel     srdto         ssefdt
     C*
     C                   if        srerr = '1'
     C                   eval      *in57 = *on
     C                   eval      *in90 = *on
     C*** BEG ADD *** 08/28/09 *********************************************
     C                   else
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         s1efdt
     C*** END ADD *** 08/28/09 *********************************************
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
     C                   eval      kkprcd = *loval
     C                   eval      kkmod1 = *loval
     C                   eval      kkefdt = *loval
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C   21              eval      kkprcd = scpst1
     C   21              eval      kkmod1 = scpstm
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
     C   21              movel(p)  scprcd        kkprcd
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C   21              movel     srdto         kkefdt
     C                   endif
     C   21              movel(p)  scmod1        kkmod1
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   movel     rvprcd        scprcd
     C                   movel     rvmod1        scmod1
     C                   z-add     rvefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C                   movel     rvdesc        scdesc
     C                   movel     rvstat        scstat
     C*** BEG ADD *** 10/01/10 ****************************************
     C                   movel     rvmrel        scmrel
     C*** END ADD *** 10/01/10 ****************************************
     C*
     C*** BEG CHG *** 10/01/10 ****************************************
     C***********        eval      scdata = scprcd  + '  ' + scmod1 +
     C***********                  '            ' + %editw(scefdt:'  /  /    ')
     C***********                  + '    ' + scdesc + '  ' + scstat
     C                   eval      scdata = scprcd  + '  ' + scmod1 +
     C                             '        ' + %editw(scefdt:'  /  /    ')
     C                             + '   ' + scmrel
     C                             + '    ' + scdesc + '  ' + scstat
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
    C*
    C*** BEG ADD *** 10/11/11 ****************************************
    C*
     C                   If        Scsel = '20'
     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl                    99
     C                   endif
    C*
    C*** END ADD *** 10/11/11 ****************************************
    C*
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
     C*** BEG ADD *** 10/01/10 ****************************************
     C                   if        %parms > 4
     C                   move      @@rel         ssmrel
     C                   endif
     C*** END ADD *** 10/01/10 ****************************************
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
     C*** Beg Add *** 08/12/10 ******************************************
     C                   eval      @@rrn = scdbrr
     C*** End Add *** 08/12/10 ******************************************
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C                   if        ssdesc <> *blanks
     C     lo:up         xlate(p)  rvdesc        u1pdsc
     C     lo:up         xlate(p)  ssdesc        u2pdsc
     C                   exsr      string
     C                   endif
     C*
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   movel     srdto         xxefdt            7 0
     C                   endif
     C*
     C                   if        ssefdt <> *zeros and xxefdt <> rvefdt
     C                   eval      meetss = *off
     C                   endif
     C*
     C*** BEG ADD *** 10/01/10 ****************************************
     C                   if        ssmrel <> *blanks and ssmrel <> rvmrel
     C                   eval      meetss = *off
     C                   endif
     C*
     C*** END ADD *** 10/01/10 ****************************************
     C                   if        ssstat <> ' '  and ssstat <> rvstat
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   endsr
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
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
Opt  HCPCS  Modifier  Eff. Date   Rel   Description                     Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YYY
12020=Change status         YNN
3
