     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H* PROGRAM: F2376R
    H* DESCRIPTION: Work with Medicare Hipps Codes
    H* CALLED FROM: F0601R
    H* CALLS TO   : F7099C
    H**********************************************************************
    H*  CREATED: 10/06/11  C:280  PROJECT: 114MZO
    H**********************************************************************
    H*  REVISIONS:
     H*
     H*  11/19/12 C:287 Project: MC367
     H*                 Disable function code F17 and option 5 for GS
    H**********************************************************************
    H*  INDICATORS
     H*
    H**********************************************************************
     H*
     FF2376F    CF   E             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
    F*
     FFMHPP     if   e             disk
     F                                     rename(PHP01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
    F*
     FFMHPL1    Uf   e           k disk
     F                                     infds(dbfds)
     F                                     rename(PHP01:dbrcdf)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
    D*
    D*================================================================
    D*  /COPY required constants
    D*================================================================
    D*
     D optnm           c                   'Medicare HIPPS Code'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2376R01'
     D comapi          c                   'FA007R'
     D uspapi          c                   'F0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   12
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
    D*
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D @@call          s              1
     D kkcode          s                   like(hpcode)
     D kkefdt          s                   like(hpefdt)
     D kktodt          s                   like(hptodt)
     D x1data          s             30
     D x2data          s             40
     D xxdt            s               d   datfmt(*usa)
     D xxdt1           s             10a
    D*
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
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@kc              1
     C                   parm                    @@kl              1
     C                   parm                    @@rrn             9 0
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
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *blanks       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     hpky          klist
     C                   kfld                    kkcode
    C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*
    C* none used
    C*
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
    C*
     C     chkssm        begsr
     C*
     C                   if        ssefdt <> 0 or
     C                             ssstat <> ' '
     C                   eval      *in11 = *on
     C                   endif
     C*
     C                   endsr
    C*
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
    C*
     C     errck1        begsr
     C                   endsr
    C*
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
    C*
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
     C*
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
    C*
    C*================================================================
    C* ERRCKP - Error checking printing
    C*================================================================
    C*
     C     errckp        begsr
     C*
     C                   eval      *IN90 = *OFF
     C                   eval      *IN59 = *OFF
     C*
     C                   if        slefdt <> 0
     C                   z-add     slefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         slefdt
     C                   if        srerr = '1'
     C                   eval      *IN90 = *On
     C                   eval      *IN59 = *On
     C                   endif
     C                   endif
     C*
     C                   endsr
    C*
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
    C*
     C     inzsst        begsr
     C*
     C                   if        @@call = '2'
     C                   eval      ssstat = 'A'
     C                   endif
     C*
     C                   endsr
    C*
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
    C*
     C     keyinz        begsr
     C*
     C                   eval      kkcode = *loval
     C                   eval      kkefdt = *loval
     C*
     C                   endsr
    C*
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
    C*
     C     keyset        begsr
     C*
     C   21              eval      kkcode = scpst1
     C*
     C                   endsr
    C*
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
    C*
     C     keysfl        begsr
     C*
     C                   eval      kkcode = sccode
     C                   eval      kkefdt = scefdt
     C*
     C                   endsr
    C*
    C*================================================================
    C* KVDO - F21=Print list
    C*================================================================
    C*
     C     kvdo          begsr
     C*
     C                   move      *zeros        slefdt
     C*
     C                   dou       *in90 = *off or *inkl = *on
     C*
     C                   exfmt     wdw01
     C*
     C                   if        *inkl = *off
     C*
     C                   exsr      errckp
     C*
     c                   if        *in90 = *off
     C*
     C                   if        slefdt <> 0
     C                   z-add     slefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ##efdt            7
     C                   else
     C                   movel     *blanks       ##efdt
     C                   endif
     C*
     C                   move      ##co#         #1co
     C*
     C                   call      'F7106C1'
     C                   parm                    #1co              3
     C                   parm      'QPRINT'      #1que            10
     C                   parm      'D'           #1mode            1
     C                   parm                    ##EFDT            7
     C*
     C                   endif
     C*
     C                   endif
     C                   enddo
     C*
     C                   endsr
    C*
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
    C*
     C     mvdbsc        begsr
     C*
     C                   eval      sccode = hpcode
     C                   eval      scefdt = hpefdt
     c*
     C     *cymd         move      hpefdt        xxdt
     C                   movel     xxdt          xxdt1
     c*
     C                   eval      scdata = sccode + '  '
     C                             + xxdt1 + '  '
     C                             + hpdsc1
     C*
     C                   eval      scstat = hpstat
     C*
     C                   endsr
    C*
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
    C*
     C     othfks        begsr
     C*
     C     *inkc         caseq     *on           kcdo
     C                   endcs
     C*
     C                   if        *inkv = *on
     C                   exsr      kvdo
     C                   endif
     C*
     C                   endsr
    C*
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
    C*
     C     othopt        begsr
    C*
     C                   If        Scsel = '20'
     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl                    99
     C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
    C*
     C     pgmend        begsr
     C*
     C                   eval      *inlr = *on
     C*
     C                   endsr
    C*
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
    C*
     C     pgmstr        begsr
     C*** Beg Add *** MC367 *** 11/19/12 ****************************
      /Free
      /If Not Defined(WEB)
       //Disable option 05
           XXOpfn = '1';
           XXMIS = 'NNN';
           XXNmbr = '05';
           Exsr UPDOFA;
       //Disable Function 17
           XXOpfn = '2';
           XXMIS = 'NNN';
           XXNmbr = '17';
           Exsr UPDOFA;
      /EndIf
      /End-Free
     C*** End Add *** MC367 *** 11/19/12 *****************************
     C                   endsr
    C*
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
    C*
     C     read          begsr
    C*
     C                   if        *in21 = *on
     C                   read      dbrcdf                                 95
     C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
    C*
     C     readp         begsr
    C*
     C                   if        *in21 = *on
     C                   readp     dbrcdf                                 95
     C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
    C*
     C     setgt         begsr
    C*
     C                   if        *in21 = *on
    C*
     C     hpky          setgt     dbrcdf                             96
    C*
     C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
    C*
     C     setll         begsr
    C*
     C                   if        *in21 = *on
    C*
     C     hpky          setll     dbrcdf                             96
    C*
     C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
    C*
     C     sf1upd        begsr
     C                   endsr
    C*
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
    C*
     C     slctrd        begsr
    C*
     C                   eval      @@rrn = scdbrr
    C*
     C                   endsr
    C*
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
    C*
     C     subset        begsr
     C*
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   movel     srdto         xxefdt            7 0
     C                   endif
     C*
     C                   if        ssefdt <> *zeros and xxefdt <> hpefdt
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   if        ssstat <> ' ' and ssstat <> hpstat
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   endsr
    C*
** Column headings
Opt HIPPS  Eff. Date   Description                                         Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YYY
221F21=Print list           YYN
3
