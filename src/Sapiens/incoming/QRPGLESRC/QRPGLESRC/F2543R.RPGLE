     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H**********************************************************************
    H* PROGRAM: F2543R
    H* DESCRIPTION: WORK WITH INPATIENT EXEMPT UNIT RATES
    H* CALLED FROM: F0601R
    H* CALLS TO   : F2543R01
    H**********************************************************************
    H*  CREATED: 04/19/10  C:227  PROJECT: NY020
    H**********************************************************************
    H*  REVISIONS:
    H**********************************************************************
    H*  INDICATORS
     H*
    H**********************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      /ENDIF
     Ff2543f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fftempny   if   e             disk
     F                                     rename(pem01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Ffteml1ny  if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pem01:dbrcdf)
     Fststl     if   e           k disk
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(13) ctdata perrcd(1)
     D ofd             s             35    dim(13) alt(of)
     D lo              C                   'abcdefghijklmnopqrs-
     D                                     tuvwxyz'
     D up              C                   'ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ'
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Inpatient Exempt Unit Rates'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2543R01'
     D comapi          c                   'FA007R'
     D uspapi          c                   'F0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   10
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
     D kkopct          s                   like(emopct)
     D kkefdt          s                   like(emefdt)
     D x1data          s             11
     D x2data          s             54
     D xxdt            s               d   datfmt(*usa)
     D kkkey           s                   like(stkey)
     D u1pdsc          s            100
     D u2pdsc          s            100
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
     C                   parm                    @@kc              1
     C                   parm                    @@kl              1
     C                   parm                    @@st              2
     C                   parm                    @@rrn             9 0
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm      scst          ##st              2
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
     C                   kfld                    kkopct
     C                   kfld                    kkefdt
     C*
     C     stky          klist
     C                   kfld                    kkkey
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C                   if        ssdesc <> *blanks or ssefdt <> 0 or
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
     C                   eval      kkopct = *loval
     C                   eval      kkefdt = *loval
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C   21              eval      kkopct = scpst1
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
     C*
     C                   eval      kkopct = scopct
     C                   eval      kkefdt = scefdt
     C*
     C                   endsr
    C*================================================================
    C* ERRCKP
    C*================================================================
     C     errckp        begsr
     C*
     C                   if        slefdt <> 0
     C                   z-add     slefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         slefdt
     C                   if        srerr = '1'
     C                   seton                                        5990
     C                   endif
     C                   endif
     C*
     C     erre          endsr
    C*================================================================
    C* KVDO - F21=Print list
    C*================================================================
     C     kvdo          begsr
     C*
     C                   move      *zeros        slefdt
     C*
     C                   dou       *in90 = *off
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
     C                   call      'F7100C'
     C                   parm                    #1co              3
     C                   parm      'QPRINT'      #1que            10
     C                   parm      'D'           #1mode            1
     C                   parm      @@ST          ##ST              2
     C                   parm                    ##EFDT            7
     C*
     C                   endif
     C*
     C                   endif
     C                   enddo
     C*
     C                   endsr
     C*================================================================
     C* MKSTHD - Creates the state header
     C*================================================================
     C     mksthd        begsr
     C*
     C* Display state code and name in header
     C*
     C                   eval      kkkey = @@st
     C                   eval      scst  = @@st
     C     stky          chain     pst01                              99
     C                   eval      scstds = stname
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      scopct = emopct
     C                   eval      scefdt = emefdt
     C*
     C* Load the current record for display
     C*
     C                   movel(P)  emopct        x1data
     C*
     C                   movel(P)  emdesc        x2data
     C     *cymd         move      emefdt        xxdt
     C                   move      xxdt          x2data
     C*
     C                   movel(P)  x1data        scdata
     C                   move      x2data        scdata
     C*
     C                   eval      scstat = emstat
     C*
     C                   endsr
    C*================================================================
    C* OPT20 - Subroutine for option 20 = Change status
    C*================================================================
     C     opt20         begsr
     C*
     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl                  99
     C*
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C*
     C     *inkc         caseq     *on           kcdo
     C                   endcs
     C*
     C*****              eval       ##mode = 'S'
     C*****              call      acdpgm        acdpl                    99
     C*
     C                   if        *inkv = *on
     C                   exsr      kvdo
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C*
     C     scsel         caseq     '20'          opt20
     C                   endcs
     C*
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
     C*
     C                   exsr      mksthd
     C*
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
     C                   if        ssdesc <> *blanks
     C*
     C     lo:up         xlate(p)  emdesc        u1pdsc
     C     lo:up         xlate(p)  ssdesc        u2pdsc
     C*
     C                   exsr      string
     C*
     C                   endif
     C*
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   movel     srdto         xxefdt            7 0
     C                   endif
     C*
     C                   if        ssefdt <> *zeros and xxefdt <> emefdt
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   if        ssstat <> ' ' and ssstat <> emstat
     C                   eval      meetss = *off
     C                   endif
     C*
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
** Column headings
Opt  OpCert     Name                                          Eff date  Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YNY
217F17=Subset               YYY
221F21=Print list           YYN
3
