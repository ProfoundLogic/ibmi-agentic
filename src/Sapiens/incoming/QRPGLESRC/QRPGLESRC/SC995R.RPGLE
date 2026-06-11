     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) ACTGRP(*CALLER)
    H*********************************************************************
    H/TITLE SC995R  - MULTPLE DIARY RECIPIENTS
    H*********************************************************************
    H*  DESCRIPTION:  Work with Multiple DARTS Diary Recipients          *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    DA700R                                                         *
    H*  CALLS TO:                                                        *
    H*    SC995R02                                                       *
    H*                                                                   *
    H*  CREATED: 05/15/09  PROGRAMMER: C:189  Project : 139WLJ           *
    H*********************************************************************
    H*
     Fsc995f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwmdrp     if   e             disk
     F                                     rename(pdr01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwmdrl     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pdr01:dbrcdf)
    F*  Secondary logical file definition (for subset option)
     Fwmdrl1    if   e           k disk
     F                                     infds(dbfds2)
     F                                     rename(pdr01:dbrcdf1)
     Fsmupp     if   e             disk
     F                                     rename(pup01:pup03)
     Fsmupl1    if   e           k disk
     Fsmupl     if   e           k disk
     F                                     rename(pup01:pup02)
     Fstctl1    if   e           k disk
    D*
     D hd              s             78    dim(2) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Multiple DARTS Diary Recipients'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'SC995R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   2
     D viewmx          c                   2
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
     D kkco#           s                   like(drco#)
     D kkrmcd          s                   like(drrmcd)
     D kkrcid          s                   like(drrcid)
     D lo              c                   const('abcdefghijklmnopqrstuvwxyz')
     D hi              c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
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
     C                   parm                    @@rmcd            5
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm      @@rmcd        #1rmcd            5
     C                   parm      scrcid        #1rcid           10
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
      *
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkrmcd
      *
     C     key2          klist
     C                   kfld                    kkrcid
      *
     C     uprmpl        plist
     C                   parm      '2'           ##call            1
     C                   parm      *zeros        ##rrn
     C                   parm      3             ##view            1 0
      *
     C     uprmpl1       plist
     C                   parm      '2'           ##call            1
     C                   parm      *zeros        ##rrn
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
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
     C* Check for Subset searches
     C                   exsr      othfks
     C                   if        ssrmcd <> *blanks
     C*
     C     ssrmcd        chain     pct01                              60
     C*
     C     *IN60         ifeq      *on
     C                   seton                                        6390
     C                   endif
     C                   endif
     C*
     C*
     C                   if        ssrcsc <> *blanks
     C*
     C     ssrcsc        chain     pup01                              60
     C     *IN60         ifeq      *on
     C                   seton                                        6490
     C                   endif
     C                   endif
     C*
     C*
     C                   if        ssrcid <> *blanks
     C*
     C     ssrcid        chain     pup02                              60
     C     *in60         ifeq      *on
     C                   seton                                        6690
     C                   endif
     C                   endif
     C*
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
     C                   eval      kkrmcd = *loval
     C                   eval      kkrcid = *loval
     C
     C                   if        q1prms >= 4
     C                   if        @@rmcd <> *blanks
     C                   seton                                        696768
     C                   movel     @@rmcd        ssrmcd
     C                   endif
     C                   endif
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C
     C   21              Move      scpst1        xxrmcd            5
     C                   eval      kkrmcd    =   xxrmcd
     C*
     C   22              Move      scpst2        xxrcid           10
     C                   eval      kkrcid    =   xxrcid
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C   21              movel(p)  scrmcd        kkrmcd
     C   22              movel(p)  scrcid        kkrcid
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   Movel     drrmcd        scrmcd
     C                   Movel     drrcid        scrcid
     C                   Movel     drrcsc        scrcsc
     C                   Movel     drrcds        scrcds
     C                   Movel     drstat        scstat
     C*
     C   21              eval      scdata = scrmcd +'  ' + scrcid +'  '
     C                             +scrcsc +'   ' + scrcds + '   ' + scstat
     C*
     C   22              eval      scdata =  scrcid +'  ' +scrcsc + '  '
     C                             + scrcds +'  ' + scrmcd + '   '+ scstat
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C     *inkj         caseq     *on           kjdo
     C                   endcs
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     c     scsel         caseq     '20'          opt20
     C                   endcs
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   eval      *inlr = *on
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
     C                   if        *in22 = *on
     C                   read      dbrcdf1                                95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C                   readp     dbrcdf                                 95
     C                   endif
     C                   if        *in22 = *on
     C                   readp     dbrcdf1                                95
     C                   endif
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endif
     C                   if        *in22 = *on
     C     key2          setgt     dbrcdf1                            96
     C                   endif
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   endif
     C                   if        *in22 = *on
     C     key2          setll     dbrcdf1                            96
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
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C
     C                   if        q1prms >= 4
     C                   if        @@RMCD <> ' '  and @@RMCD <> DRRMCD
     C                   eval      meetss = *off
     C                   endif
     C                   endif
      *
      *
     C                   if        SSRMCD <> ' '  and SSRMCD <> DRRMCD
     C                   eval      meetss = *off
     C                   endif
      *
      *
     C                   if        SSRCID <>  ' '    and SSRCID <> DRRCID
     C                   eval      meetss = *off
     C                   endif
      *
     C                   if        SSRCSC <> ' ' and SSRCSC <> DRRCSC
     C                   eval      meetss = *off
     C                   endif
      *
      *
     C                   if        SSRCDS <> ' ' and SSRCDS <> DRRCDS
     C                   eval      meetss = *off
     C                   endif
      *
      *
     C                   if        SSSTAT <> ' ' and SSSTAT <> DRSTAT
     C                   eval      meetss = *off
     C                   endif
     C*
B01  C     ssrcds        ifne      *blanks
     C*
     C     lo:hi         xlate     drrcds        u1pdsc
     C     lo:hi         xlate     ssrcds        u2pdsc
     C*
     C                   exsr      string
     C*
E01  C                   endif                                                      SSPDSC<>*BLANKS
     C                   endsr
      *===============================================================
      * KJDO - SUBROUTINE FOR F10
      *===============================================================
     C     kjdo          begsr
     C                   select
     C                   when      hfld = 'SSRMCD'
      *
     C  N69              call      'SC305R'
     C                   parm      '2'           ##call            1
     C     *inkc         parm                    ##kc              1
     C     *inkl         parm                    ##kl              1
     C                   parm      *blanks       ##rmcd            5
     C                   parm      *blanks       ##desc           40
     C                   parm      *blanks       ##rtcd            1
      *
     C                   seton                                        41
      *
     C                   If        ##rmcd <> ' '
     C                   Movel     ##rmcd        ssrmcd
     C                   seton                                        90
     C                   endif
      *
     C                   when      hfld = 'SSRCSC'
     C                   call      'S0005R'      uprmpl
      *
     C                   seton                                        44
      *
     C     ##rrn         ifne      *zeros
     C     ##rrn         chain     pup03                              99
     C*
     C                   movel     upstaf        ssrcsc
     C                   endif
     C*
     C                   when      hfld = 'SSRCID'
      *
     C                   call      'S0005R'      uprmpl1
      *
     C                   seton                                        42
     C     ##rrn         ifne      *zeros
     C     ##rrn         chain     pup03                              99
     C*
     C                   movel     upusr         ssrcid
     C                   endif
     C*
     C*
     C                   endsl
     C
     C                   seton                                        90
     C                   endsr
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     string        begsr
     C*
     C                   call      'QCLSCAN'
     C                   parm                    u1pdsc           50
     C                   parm      30            strlen            3 0
     C                   parm      1             start             3 0
     C                   parm                    u2pdsc           50
     C                   parm      30            patlen            3 0
     C                   parm      '0'           trans             1
     C                   parm      '1'           trim              1
     C                   parm      ' '           wild              1
     C                   parm                    scnres            3 0
     C*
     C     scnres        ifeq      0
     C                   move      *off          meetss
     C                   else
     C                   move      *on           meetss
     C                   endif                                                   SCNRES=0
     C*
     C                   endsr                                                     STRING
      *================================================================
      * OPT20 - SUBROUTINE FOR OPTION 20
      *================================================================
      *
      * OPTION 20=CHANGE STATUS
      *
     C     OPT20         Begsr

     C                   Move      'S'           ##Mode
     C                   Call      ACDPGM        acdpl                  99

     C                   Endsr

      *===============================================================
** Column headings
Opt  Code       ID     Code   Recipient Name                           Status
Opt     ID      Code  Recipient Name                            Code  Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               YYY
217F17=Subset               YYY
