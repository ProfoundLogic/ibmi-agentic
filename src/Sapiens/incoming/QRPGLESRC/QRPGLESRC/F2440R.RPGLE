     H*******************************************************************
     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*******************************************************************
     H/TITLE F2440R - WORK WITH TAXONOMY CODES
     H*
     H*  DESCRIPTION: This program shows the W/W  Screen for FTTCP
     H*
     H*   CALLED BY: MENU
     H*              F0032R01
     H*
     H* Created: 03/04/09 C:198 Project: MC330
     H*
     H*******************************************************************
     H*  REVISIONS:
     H*
     H*  09/09/25 C:342 Project: MC453
     H*                 Provider Taxonomy Codes Update
     H*                 1. Compile time array for View1 and View2 changed
     H*                 2. Removed Effective date from scr01c and scr03 screen
     H*                    along with related coding.
    H*
     H*  01/08/10 C:198 Support: 71199
     H*                 Compilation only
    H*
     H*  05/15/09 C:198 Support: 68468
     H*                 Compilation only
    H*
     H*******************************************************************
     Ff2440f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Ffttcp     if   e             disk
     F                                     rename(ptc01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Ffttcl2    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(ptc01:dbrcdf)
    F*  Secondary logical file definition (for displaying items)
     Ffttcl3    if   e           k disk
     F                                     infds(dbfds2)
     F                                     rename(ptc01:ptc03)
    D*
     D hd              s             78    dim(2) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Taxonomy Codes'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2440R01'
     D comapi          c                   'FA007R'
     D uspapi          c                   'F0006R'
     D hi              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D lo              c                   'abcdefghijklmnopqrstuvwzyz'
     D infomx          c                   1
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
     D kkcode          s                   like(tccode)
     D kkclas          s                   like(tcclas)
     D kkspcl          s                   like(tcspcl)
     D*** Beg Del *** 09/09/25 ****************************************
     D*** kkefdt          s                   like(tcefdt)
     D*** End Del *** 09/09/25 ****************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C*** RRN parameter returns rrn# value to all the programs which calls it on F10
     C     *entry        plist
     C                   parm                    @@call
     C                   parm      *inkc         @@kc              1
     C                   parm      *inkl         @@kl              1
     C                   parm                    @@rrn             9 0
    C*
    C*  Add/change/display program parameter list
    C*
     C*** RRN parameter sends rrn# value to all programs which displays further data
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
      *
     C     dtmmdy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkcode
     C                   kfld                    kkclas
     C*** Beg Del *** 09/09/25 ****************************************
     C***                kfld                    kkefdt
     C*** End Del *** 09/09/25 ****************************************
      *
     C     key2          klist
     C                   kfld                    kkclas
     C                   kfld                    kkspcl
     C                   kfld                    kkcode
     C*** Beg Del *** 09/09/25 ****************************************
     C***                kfld                    kkefdt
     C*** End Del *** 09/09/25 ****************************************
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C*** Beg Chg *** 09/09/25 ****************************************
     C***                if        ssclas <> *blank or ssefdt <> 0 or
     C                   if        ssclas <> *blank or
     C*** End Chg *** 09/09/25 ****************************************
     C                             ssstat <> ' ' or ssspcl <> *blank
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
     C*** Beg Del *** 09/09/25 ****************************************
     C***                if        ssefdt <> *zeros
     C*
     C***                z-add     ssefdt        srdti
     C***                call      'SRCVTDAT'    datchk
     C*
     C***                if        srerr = '1'
     C***                eval      *in57 = *on
     C***                eval      *in90 = *on
     C***                endif
     C*
     C***                endif
     C*** End Del *** 09/09/25 ****************************************
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
     C                   movel     'A'           ssstat
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   eval      kkcode = *loval
     C                   eval      kkclas = *loval
     C                   eval      kkspcl = *loval
     A*** Beg Del *** 09/09/25 ****************************************
     C***                eval      kkefdt = *loval
     A*** End Del *** 09/09/25 ****************************************
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C   21              eval      kkcode = scpst1
     C                   if        *in21 = *on
     C**** Beg Del *** 09/09/25 ****************************************
     C****               z-add     sceffd        srdti
     C****               call      'SRCVTDAT'    dtyymd
     C****               if        srerr = '0'
     C****               MOVEL     SRDTO         kkefdt
     C****               endif
     C**** End Del *** 09/09/25 ****************************************
     C                   endif
     C   21              movel     sccls         kkclas
     C   22              movel     scpst2        kkclas
     C   22              movel     scspec        kkspcl
     C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   movel(p)  sccode        kkcode
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C*
     C*** Beg Del *** 09/09/25 ****************************************
     C***                if        srerr = '0'
     C***                MOVEL     SRDTO         kkefdt
     C***                endif
     C*** End Del *** 09/09/25 ****************************************
     C*
     C                   movel(p)  scclas        kkclas
     C*
     C                   if        *in22 = *on
     C*
     C                   movel(p)  scspcl        kkspcl
     C*
     C                   endif                                                  *in21 = *on
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   movel     tccode        sccode
     C                   z-add     tcefdt        srdti
     C                   call      'SRCVTDAT'    dtmmdy
     C                   Movel     srdto         scefdt
     C                   Movel     tcstat        scstat
     C                   Movel     tcclas        scclas
     C                   Movel     tcspcl        scspcl
     C*
     C   21              eval      scdata = sccode  + '     ' + scclas
     C                             + '                  '
     C*** Beg Chg *** 09/09/25 ****************************************
     C***                          +  %editw(scefdt:'  /  /    ')
     C                             +  scspcl
     C*** End Chg *** 09/09/25 ****************************************
     C
     C*
     C   22              eval      scdata = scclas  + '       ' + scspcl
     C                             + '       ' +  sccode  + '     '
     C*** Beg Del *** 09/09/25 ****************************************
     C***                          +  %editw(scefdt:'  /  /    ')
     C*** End Del *** 09/09/25 ****************************************
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
     C*** change status option
     C                   if        scsel = '20'
     C                   exsr      opt20
     C                   endif
     C                   endsr
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     opt20         begsr
     C*
     C                   MOVE      'S'           ##mode
     C                   call      acdpgm        acdpl                  99
     C*
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
     C                   read      ptc03                                  95
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
     C                   readp     ptc03                                  95
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
     C     key2          setgt     ptc03                              96
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
     C     key2          setll     ptc03                              96
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
     C                   MOVE      SCDBRR        @@RRN
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
      *
     C*** Beg Del *** 09/09/25 ****************************************
     C***                z-add     ssefdt        srdti
     C***                call      'SRCVTDAT'    dtyymd
     C***                if        srerr = '0'
     C***                MOVEL     SRDTO         xxefdt            7 0
     C***                endif
      *
     C***                if        ssefdt <>  *zeros and xxefdt <> tcefdt
     C***                eval      meetss = *off
     C***                endif
     C*** End Del *** 09/09/25 ****************************************
      *
     C                   if        ssclas <>  *blanks
      *
     C     lo:hi         xlate(p)  tcclas        x1clas           30
     C     lo:hi         xlate(p)  ssclas        x2clas           30
      *
     C                   exsr      string
      *
     C                   endif
      *
     C                   if        ssspcl <>  *blanks
      *
     C     lo:hi         xlate(p)  tcspcl        x1spcl           50
     C     lo:hi         xlate(p)  ssspcl        x2spcl           35
      *
     C                   exsr      string
      *
     C                   endif
      *
     C                   if        ssstat <> ' ' and ssstat <> tcstat
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   endsr
    C*===============================================================
    C* STRING - SUBROUTINE TO SEARCH STRINGS
    C*===============================================================
     C*
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    x1clas
     C                   PARM      50            strlen            3 0
     C                   PARM      1             start             3 0
     C                   PARM                    x2clas
     C                   PARM      50            patlen            3 0
     C                   PARM      '0'           trans             1
     C                   PARM      '1'           trim              1
     C                   PARM      ' '           wild              1
     C                   PARM                    scrnes            3 0
     C*
     C                   if        scrnes = 0
     C                   move      *off          meetss
     C                   endif                                                  scrnes = 0
     C*
     C                   ENDSR                                                  STRING
     C*
     C*================================================================
     C*** Opt Taxonomy code       Classification               Eff. Date             Sts
     C*** Opt Classification   Specialization   Taxonomy Code         Eff. Date      Sts
** Column headings
Opt Taxonomy code       Classification               Specialization        Sts    ** 09/09/25
Opt Classification   Specialization   Taxonomy Code                        Sts    ** 09/09/25
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
217F17=Subset               YYY
211F11=Change view          YYY
