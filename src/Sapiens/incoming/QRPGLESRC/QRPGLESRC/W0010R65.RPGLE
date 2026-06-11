     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H*****************************************************************
     H/TITLE W0010R65 - Verbiage information.
     H*****************************************************************
     H*  DESCRIPTION:  Work With Policy page 3c information.          *
     H*
     H*  CALLED FROM:                                                 *
     H*    W0010R   - WORK WITH GROUPS                                *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 12/31/10  PROGRAMMER: C:255 PROJECT: WC1918         *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/01/20 C:136  Project WC2757                                *
     H*                 Remove blanket waiver vbg but leave coding    *
     H*                 in place to add other verbiage types.         *
     H*                                                               *
     H* 02/02/11 Project WC1918 (Pirss# 26):                          *
     H*                         Display confirmation window once if m-*
     H*                         ore than one records are being deleted*
     H*                                                               *
     H* 01/25/11 Project WC1918 (Pirss# 25):                          *
     H*                         Added changes to work properly"Positi-*
     H*                         on to" functionality.                 *
     H*                                                               *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0010f65  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwdf4p     if   e             disk
     F                                     rename(pf401:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
      *** Begin Change 10/01/20 ************************************
     F**wdf4l     if   e           k disk
     Fwdf4l1    if   e           k disk
      *** End   Change 10/01/20 ************************************
     F                                     infds(dbfds)
     F                                     rename(pf401:dbrcdf)
     Fwmfnp     if   e             disk
     Fwmfnl     if   e           k disk    rename(pfn01:pfn0l)
     F                                     infds(dbfds2)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
      *** Begin Change *** 10/01/20 *************************************
     D**optnm           c                   'Policy Page - 3c Information'
     D optnm           c                   'Verbiage Entry'
      *** End   Change *** 10/01/20 *************************************
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0010R6502'
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
     D count           s              2  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D ##rrn2          s              9  0
     D @@call          s              1
     C*** Beg Add ***** 02/02/11 ******************************************
     D dsplWindow      s               n
     C*** End Add ***** 02/02/11 ******************************************
     D kkco#           s                   like(f4co#)
     D kkfnd           s                   like(f4fnd)
     D kkkey           s                   like(f4key)
     D kkefdt          s                   like(f4efdt)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *blanks       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
    C*
     C     *entry        plist
     C                   parm      *inkc         @@kc              1
     C                   parm      *inkl         @@kl              1
     C                   parm                    @@CALL            1
     C                   parm                    @@rrn             9 0
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm      @@rrn         ##rrn2            9 0
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
      *** Begin Change *** 10/01/20 ********************************************
     C**   key1          klist
     C**                 kfld                    kkco#
     C**                 kfld                    kkfnd
     C**                 kfld                    kkkey
     C**                 kfld                    kkefdt
      **
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkefdt
      *** End Change   *** 10/01/20 ********************************************
     C*
     C     key1b         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C*
     C*** Beg Chg ***** 02/02/11 ******************************************
     C********           if        scsel = '4'
     c********           exfmt     scr03
     C********           endif                                                  scsel= '4'
     C                   if        scsel = '4' and dsplWindow = *on
     c                   exfmt     scr03
     C                   eval      dsplWindow = *off
     C                   eval      autorf = *on
     C                   endif
     C*** End Chg ***** 02/02/11 ******************************************
     C
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
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C*** Beg Chg ***** 01/25/11 ******************************************
     C*****              eval      kkkey  = *loval
      *** Begin Delete *** 10/01/20 ***************************************
     C***                eval      kkkey  = 'PP3C'
      *** End   Delete *** 10/01/20 ***************************************
     C*** End Chg ***** 01/25/11 ******************************************
     C                   eval      kkefdt = *loval
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   if        *in21 = *on
     C                   z-add     0             srdti
     C                   move      scpst1        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   movel     srdto         kkefdt
     C                   endif
     C                   endif
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C   21              movel     srdto         kkefdt
     C                   endif
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   if        f4stat =  'A'
     C                   z-add     f4efdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C*
      ***** Begin Change *** 10/01/20 ***********************************
     C***                   eval      scdata = '    ' +
     C***                             ' ' + %editw(scefdt:'  /  /    ')
     C***                             + '         '  + %trim(f4txt1) + ' ' +
     C***                              %trim(f4txt2)
     C***                   else
     C***                   eval      scdata = *blanks
      /Free
       Select;
       When F4key = 'PP3C';
          scdata = '  ' + 'Policy Page 3C               ' +
          %editw(scEfDt:'  /  /    ') + '        ' +
          %trim(F4txt1) + ' ' + %trim(F4txt2);
       Other;
          scdata = *Blanks;
       Endsl;
      /End-Free
      ***** End Change  *** 10/01/20 ***********************************
     C                   endif
     C*** Beg Add ***** 02/02/11 ******************************************
     C                   eval      dsplWindow = *on
     C*** End Add ***** 02/02/11 ******************************************
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
     C     @@call        comp      '1'                                    09    inquiry
     C     @@call        comp      '2'                                    30    select
     C*
     C     @@rrn         chain     wmfnp                              60
     C  n60              z-add     fnfnd         scfnd
     C  n60              movel     fnname        scname
     C                   move      fnco#         kkco#
     C                   move      scfnd         kkfnd
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C     key1b         reade     dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C     key1b         readpe    dbrcdf                                 95
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
     C                   if        f4stat = 'D'
     C                   eval      meetss = *off
     C                   endif
     C                   endsr
** Column headings
Opt    Type                          Eff Date         Verbiage
** Description--------------MISN
1011=Select                 NNY
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
3
