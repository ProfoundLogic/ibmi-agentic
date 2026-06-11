     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H**************************************************************************************
     H/TITLE F2384R                                                                        *
     H**************************************************************************************
     H*  Description:  Work With CMG Codes                                                 *
     H*                                                                                    *
     H*  Created: 10/17/11   Programmer C:274   Project:114MAR                             *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*
     H*  11/19/12 C:287 Project: MC367
     H*                 Disable function code F17 and option 5 for GS
     H*                                                                                    *
     H**************************************************************************************
    H*  INDICATORS                                                                        *
     H*                                                                                    *
    H**************************************************************************************
     Ff2384f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Ffmmgp     if   e             disk
     F                                     rename(pmg01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Ffmmgl1    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pmg01:dbrcdf)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'CMG Codes'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2384R01'
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
     D xxdt            s               d   datfmt(*usa)
     D xxdt1           s             10a
     D kkcode          s                   like(mgcode)
     D kktier          s                   like(mgtier)
     D kkefdt          s                   like(mgefdt)
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
     C                   parm      *inkc         @@kc              1
     C                   parm      *inkl         @@kl              1
     C                   parm                    @@rrn             9 0
    C*
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
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkcode
     C                   kfld                    kktier
     C                   kfld                    kkefdt
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   if        ssefdt <> 0 or ssstat <> ' '
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
    C* ERRCKP - Error checking For Print selection
    C*================================================================
     C     errckp        begsr
     C*
     C                   eval      *IN90 = *OFF
     C                   eval      *IN60 = *OFF
     C*
     C                   if        slefdt <> 0
     C                   z-add     slefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         slefdt
     C                   if        srerr = '1'
     C                   seton                                        6090
     C                   endif
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C*
     C                   if        @@call = '2'
     C                   eval      ssstat = 'A'
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C*
     C                   eval      kkcode = *loval
     C                   eval      kktier = *loval
     C                   eval      kkefdt = *loval
     C*
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C   21              eval      kkcode = scpst1
     C   21              eval      kktier = scpst2
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   eval      kkcode = sccode
     C                   eval      kktier = sctier
     C                   eval      kkefdt = scefdt
     C*
     C                   endsr
    C*================================================================
    C* KVDO - F21=Print list
    C*================================================================
     C     kvdo          begsr
     C*
     C                   move      *zeros        slefdt
     C*
     C                   dou       *in90 = *off or *inkl = *on
     C                   exfmt     wdw01
     C*
     C                   if        *inkl = *off
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
     C                   call      'F7108C'
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
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      sccode = mgcode
     C                   eval      sctier = mgtier
     C                   eval      scefdt = mgefdt
     c*
     C     *cymd         move      mgefdt        xxdt
     C                   movel     xxdt          xxdt1
     c*
     C                   eval      scdata = mgcode + '     '
     C                             + mgtier + '     '
     C                             + xxdt1  + '  '
     C                             + mgdesc
     C*
     C                   eval      scstat = mgstat
     C*
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C*
     C     *inkc         caseq     *on           kcdo
     C     *inkv         caseq     *on           kvdo
     C                   endcs
     C*
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
    C*
     C                   If        Scsel = '20'
     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl                    99
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C*
     C                   seton                                        LR
     C*
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
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
     C                   eval      @@rrn = scdbrr
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   movel     srdto         xxefdt            7 0
     C                   endif
     C*
     C                   if        ssefdt <> *zeros and xxefdt <> mgefdt
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   if        ssstat <> ' ' and ssstat <> mgstat
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   endsr
** Column headings
Opt  Code     Tier   Eff. Date   Description                             Sts
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
