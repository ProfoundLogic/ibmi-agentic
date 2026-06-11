     H**************************************************************************************
     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**************************************************************************************
     H actgrp(*caller)
     H/TITLE F2358R                                                                        *
     H**************************************************************************************
     H*  Description:  Medicare ASC Payment Rates                                          *
     H*                                                                                    *
     H*  Created: 11/14/08   Programmer C:212   Project:TX043                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  10/11/10  C:280  Project: seq78229                                                *
     H*                   Added option 20 for status change                                *
     H*                                                                                    *
     H*  04/08/11 C:251 Project: 114MWT                                                    *
     H*                 -Added *entry params to pass back RRN to calling program           *
     H*                                                                                    *
     H*  06/30/09 C:227 Project: TX063                                                     *
     H*                 -Added F6=Add                                                      *
     H*                 -Added 2=Change                                                    *
     H*                                                                                    *
     H*  05/15/09 C:198 Support: 68468                                                     *
     H*                 Compilation only                                                   *
     H*                                                                                    *
     H**************************************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2358f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Ffmasp     if   e             disk
     F                                     rename(pas01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Ffmasl1    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pas01:dbrcdf)
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
     D optnm           c                   'Medicare ASC Payment rates'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2358R01'
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
     D*** Beg Add *** 04/08/11 *********************************************
     D @@kc            s              1
     D @@kl            s              1
     D @@rrn           s              9  0
     D*** End Add *** 04/08/11 *********************************************
     D kkhcpc          s                   like(ashcpc)
     D kkefdt          s                   like(asefdt)
     D kkpi            s                   like(aspi)
     D u1pdsc          s                   like(asdesc)
     D u2pdsc          s                   like(asdesc)
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
     C*** Beg Add *** 04/08/11 *********************************************
     C                   parm                    @@KC
     C                   parm                    @@KL
     C                   parm                    @@RRN
     C*** End Add *** 04/08/11 *********************************************
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
     C     key1          klist
     C                   kfld                    kkhcpc
     C                   kfld                    kkefdt
     C                   kfld                    kkpi
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C                   if        ssdesc <> *blank or ssefdt <> 0 or
     C                             ssstat <> ' '
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
     C                   eval      kkhcpc = *loval
     C                   eval      kkefdt = *loval
     C                   eval      kkpi   = *loval
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C   21              eval      kkhcpc = scpst1
     C                   if        *in21 = *on
     C                   z-add     scpstd        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   MOVEL     SRDTO         kkefdt
     C                   endif
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   movel(p)  schcpc        kkhcpc
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C*
     C                   if        srerr = '0'
     C                   MOVEL     SRDTO         kkefdt
     C                   endif
     C*
     C                   movel(p)  scpi          kkpi
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   movel     ashcpc        schcpc
     C                   movel     asdesc        scdesc
     C                   z-add     asefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         scefdt
     C                   movel     aspi          scpi
     C                   Movel     asstat        scstat
     C*
     C                   eval      scdata = schcpc  + '  ' + scdesc +
     C                             '      ' + %editw(scefdt:'  /  /    ') +
     C                               '     '   + scstat
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
     C*** Beg Add *** 04/08/11 *********************************************
     C                   eval      @@rrn = scdbrr
     C*** End Add *** 04/08/11 *********************************************
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
B01  C                   if        ssdesc <> *blanks
     C*
     C     lo:up         xlate(p)  asdesc        u1pdsc
     C     lo:up         xlate(p)  ssdesc        u2pdsc
     C*
     C                   exsr      string
     C*
E01  C                   endif                                                  ssdesc <> *blanks
      *
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   MOVEL     SRDTO         xxefdt            7 0
     C                   endif
      *
     C                   if        ssefdt <>  *zeros and xxefdt <> asefdt
     C                   eval      meetss = *off
     C                   endif

     C                   if        ssstat <> ' ' and ssstat <> asstat
     C                   eval      meetss = *off
     C                   endif
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
Opt  HCPCS  Description                               Effective date  Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
217F17=Subset               YYY
12020=Change status         YNN
3
