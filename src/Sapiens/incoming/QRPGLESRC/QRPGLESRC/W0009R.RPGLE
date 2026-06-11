     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H** ======================================================================================== **
    H/TITLE W0009R - W/W TPA
     H*  Created: 06/25/09 by C:220 for project: 114WTR                                            *
    H* ========================================================================================= **
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0009f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwmtcp     if   e             disk
     F                                     rename(ptc01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwmtcl     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(ptc01:dbrcdf)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(20) ctdata perrcd(1)
     D ofd             s             35    dim(20) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D*optnm           c                   'TPA Contact Information'
     D optnm           c                   'TPA Code, Description, Status'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0009R02'
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
     D @@call          s              1
     D kkco#           s                   like(tcco#)
     D kktpac          s                   like(tctpac)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    D* DS Fields - START
    D*---------------------------------------------------------------------
    D dsdata          DS            65
    D  xtpac                  6     15
    D  xdesc                 23     63
    D  xstat                 65     65
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         ##kc
     C                   parm      *inkl         ##kl
     C                   parm                    @#co#             3 0
     C                   parm                    @@call
     C                   parm                    @@rrn             9 0
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##co#
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
     C                   kfld                    kkco#
     C                   kfld                    kktpac
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C     ssstat        ifne      *blank
     C                   move      *On           *in11
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
     C     ssstat        ifne      'A'
     C     ssstat        andne     'I'
     C     ssstat        andne     ' '
     C*
     C                   SETON                                        4390
     C*
     C                   endif                                                     Valid status
     C
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
     C                   z-add     *loval        kkco#
     C*                  movel     @#co#         kkco#
     C                   move      *loval        kktpac
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   movel     @#co#         kkco#
     C                   movel     scpst1        kktpac
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   movel     tctpac        kktpac
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   clear                   scdata
     C*
     C                   movel     tctpac        xtpac
     C                   movel     tcdesc        xdesc
     C                   movel     tcstat        xstat
     C*
     C                   movel     dsdata        scdata
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
     C                   select
     C     scsel         wheneq    '20'
     C                   exsr      opt20
     C                   endsl
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   seton                                        LR
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C                   movel     @#co#         ##co#
    C*
     C     @@call        ifeq      '1'
     c                   move      '1'           *in09                          INQUIRY
     c                   move      'D'           ##mode                         Display
     c                   endif
     C     @@call        ifeq      '2'
     c                   move      '1'           *in30                          SEARCH
     c                   move      'A'           xstat
     c                   endif
     c                   move      '0'           *in11
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
     C                   z-add     scdbrr        @@rrn
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C* If called in search mode then pre-subset status to "A"
     C*
     C                   if        @@call = '2'
     C                   eval      ssstat = 'A'
     C                   endif
     C*
     C                   if        ssstat <> ' ' and ssstat  <> tcstat
     C*
     C                   eval      meetss = *off
     C*
     C                   endif                                                 Status is A or I
     C                   endsr
    C*================================================================
    C* CHANGE STATUS SR IF RECORD SELECTED.
    C*================================================================
     C     opt20         begsr
     C*
     C* Option 20=Change status
     C*
     C                   movel     'S'           ##mode
     C*
     C                   call      acdpgm        acdpl                    99
     C*
     C                   endsr                                               Opt20
** Column headings
Opt       TPA Code         TPA Description                           Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
1066=                       NNN
1077=                       NNN
1088=                       NNN
11010=                      NNN
11111=                      NNN
11212=                      NNN
11414=                      NNN
11515=                      NNN
11818=                      NNN
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YYN
3
