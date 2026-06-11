     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/TITLE W0149R - Work With Employers Cross-Reference
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: Menu                                               *
     H*                                                               *
     H* CALLS TO:  W0149R02 - Maint Employers Cross-Reference         *
     H*                                                               *
     H* CREATED: 09/21/01                                             *
     H*                                                               *
     H* 09/21/01 C:136 PROJECT WC577                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/07/15 C:65  PhaseWare: 32455                               *
     H*                Check for employer master record existance and *
     H*                turn off subset meet if record not found. This *
     H*                will allow the program to only display live    *
     H*                cross reference when called from live, or only *
     H*                display quote cross reference when called from *
     H*                quote.                                         *
     H*                                                               *
     H* 04/24/02 C:136 Support: 32223                                 *
     H*                Program doesn't pull correct names for         *
     H*                divisions.                                     *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     Fw0149f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwzexp     if   e             disk
     F                                     rename(pex01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items in view 1)
     Fwzexl     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pex01:dbrcdf1)
    F*  Secondary logical file definition (for displaying items in view 2)
     Fwzexl1    if   e           k disk
     F                                     infds(dbfds2)
     F                                     rename(pex01:dbrcdf2)
     F*  Employer's master file (for looking up name)
     Fwmeml     if   e           k disk
    D*
     D hd              s             78    dim(2) ctdata perrcd(1)
     D hd1             s             78    dim(2) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
      *
     D view1           ds            74
     D   v1fnd                 1      3
     D   v1emp#                6     14
     D   v1div                17     21
     D   v1name               24     43
     D   v1ofnd               46     48
     D   v1oemp               51     59
     D   v1odiv               62     66
     D   v1car                71     71
      *
     D view2           ds            74
     D   v2ofnd                1      3
     D   v2oemp                6     14
     D   v2odiv               17     21
     D   v2name               24     43
     D   v2fnd                46     48
     D   v2emp#               51     59
     D   v2div                62     66
     D   v2car                71     71
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Employer Cross-Reference'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0149R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
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
     D @@dummy         s              1
     D @@call          s              1
     D kkco#           s                   like(exco#)
     D kkfnd           s                   like(exfnd)
     D kkemp#          s                   like(exemp#)
     D kkdiv           s                   like(exdiv)
     D kkoco#          s                   like(exco#)
     D kkofnd          s                   like(exfnd)
     D kkoemp          s                   like(exemp#)
     D kkodiv          s                   like(exdiv)
     D Up              C                   'ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ'
     D Lo              C                   'abcdefghijklmnopqrs-
     D                                     tuvwxyz'
     D u1pdsc          s                   like(emname)
     D u2pdsc          s                   like(emname)
     D strlen          s              3  0
     D start           s              3  0
     D patlen          s              3  0
     D trans           s              1
     D trim            s              1
     D wild            s              1
     D scnres          s              3  0
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
     C                   parm                    @@dummy                        to match menu call
     C                   parm                    @@call
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
     C     exky1         klist
     C                   kfld                    ##co#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
      *
     C     exky2         klist
     C                   kfld                    ##co#
     C                   kfld                    kkofnd
     C                   kfld                    kkoemp
     C                   kfld                    kkodiv
      *
     C     emky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
      *** beg add **** 04/24/02 ***************************************
     C                   kfld                    kkdiv
      *** beg add **** 04/24/02 ***************************************
      *** end add **** 04/24/02 ***************************************
      *** begin add ***  01/07/15  ************************************
     C     emky1         klist
     C                   kfld                    ##co#
     C                   kfld                    xxfnd             3 0
     C                   kfld                    xxemp#            9 0
     C                   kfld                    xxdiv             5 0
      *** end add *****  01/07/15  ************************************
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   if        ssfnd <> 0 or ssfremp <> 0 or sstoemp <> 0
     C                             or ssname <> *blanks
     C                   eval      *in11 = *on
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
     C                   eval      kkfnd = 0
     C                   eval      kkemp# = 0
     C                   eval      kkdiv = 0
     C                   eval      kkofnd = 0
     C                   eval      kkoemp = 0
     C                   eval      kkodiv = 0
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   if        *in21
     C                   move(p)   scpst1        kkfnd
     C                   eval      kkemp# = scpsta
     C                   endif
      *
     C                   if        *in22
     C                   move(p)   scpst1        kkofnd
     C                   eval      kkoemp = scpsta
     C                   endif
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   if        *in21
     C                   eval      kkfnd = scfnd
     C                   eval      kkemp# = scemp#
     C                   endif
      *
     C                   if        *in22
     C                   eval      kkofnd = scofnd
     C                   eval      kkoemp = scoemp
     C                   endif
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
      *
     C                   eval      scfnd = exfnd
     C                   eval      scemp# = exemp#
     C                   eval      scdiv = exdiv
     C                   eval      scofnd = exofnd
     C                   eval      scoemp = exoemp
     C                   eval      scodiv = exodiv
     C                   eval      sccar = excar
      *
     C                   if        *in21
      *** begin del ***  01/07/15  ************************************
     C*********          eval      kkfnd = scfnd
     C*********          eval      kkemp# = scemp#
      ****beg*add **** 04/24/02 ***************************************
     C*********          eval      kkdiv = scdiv
      ****end*add **** 04/24/02 ***************************************
     C*****emky          chain     pem01                              60
      *** end   del ***  01/07/15  ************************************
     C                   eval      v1name = %triml(emname)
     C                   movel(p)  hd1(1)        schdl2
     C                   eval      v1fnd = %editc(exfnd:'Z')
     C                   eval      v1emp# = %editc(exemp#:'Z')
     C                   eval      v1div = %editc(exdiv:'Z')
     C                   eval      v1ofnd = %editc(exofnd:'Z')
     C                   eval      v1oemp = %editc(exoemp:'Z')
     C                   eval      v1odiv = %editc(exodiv:'Z')
     C                   eval      v1car = sccar
     C                   movel(p)  view1         scdata
     C                   endif
      *
     C                   if        *in22
      *** begin del ***  01/07/15  ************************************
     C*********          eval      kkfnd = scofnd
     C*********          eval      kkemp# = scoemp
      ****beg*add **** 04/24/02 ***************************************
     C*********          eval      kkdiv = scodiv
      ****end*add **** 04/24/02 ***************************************
     C*****emky          chain     pem01                              60
      *** end   del ***  01/07/15  ************************************
     C                   eval      v2name = %triml(emname)
     C                   movel(p)  hd1(2)        schdl2
     C                   eval      v2ofnd = %editc(exofnd:'Z')
     C                   eval      v2oemp = %editc(exoemp:'Z')
     C                   eval      v2odiv = %editc(exodiv:'Z')
     C                   eval      v2fnd = %editc(exfnd:'Z')
     C                   eval      v2emp# = %editc(exemp#:'Z')
     C                   eval      v2div = %editc(exdiv:'Z')
     C                   eval      v2car = sccar
     C                   movel(p)  view2         scdata
     C                   endif
      *
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
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21
     C     ##co#         reade     dbrcdf1                                95
     C                   endif
     C                   if        *in22
     C     ##co#         reade     dbrcdf2                                95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21
     C     ##co#         readpe    dbrcdf1                                95
     C                   endif
     C                   if        *in22
     C     ##co#         readpe    dbrcdf2                                95
     C                   endif
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   if        *in21
     C     exky1         setgt     dbrcdf1                            96
     C                   endif
     C                   if        *in22
     C     exky2         setgt     dbrcdf2                            96
     C                   endif
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   if        *in21
     C     exky1         setll     dbrcdf1                            96
     C                   endif
     C                   if        *in22
     C     exky2         setll     dbrcdf2                            96
     C                   endif
     C                   endsr
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C                   eval      scdata = '*Deleted'
     C                   endsr
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C                   endsr
     C*================================================================
     C* STRING - Subroutine to search strings
     C*================================================================
     C     string        begsr
     C                   call      'QCLSCAN'
     C                   parm                    u1pdsc
     C                   parm      40            strlen
     C                   parm      1             start
     C                   parm                    u2pdsc
     C                   parm      40            patlen
     C                   parm      '0'           trans
     C                   parm      '1'           trim
     C                   parm      ' '           wild
     C                   parm                    scnres
      *
     C                   if        scnres = 0
     C                   eval      meetss = *off
     C                   endif
      *
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
      *** begin add ***  01/07/15  ************************************
     C                   If        *in21
     C                   eval      xxfnd = exfnd
     C                   eval      xxemp# = exemp#
     C                   eval      xxdiv = exdiv
     C                   Endif
     C                   If        *in22
     C                   eval      xxfnd = exofnd
     C                   eval      xxemp# = exoemp
     C                   eval      xxdiv = exodiv
     C                   Endif
     C*
     C     emky1         chain     pem01                              60
     C                   If        *in60 = *on
     C                   eval      meetss = *off
     C                   Endif
      *** end add *****  01/07/15  ************************************
      *
     C                   if        *in21
     C                   if        ssfnd <> 0 and ssfnd <> exfnd
     C                   eval      meetss = *off
     C                   endif
      *
     C                   if        ssfremp <> 0 and ssfremp > exemp#
     C                   eval      meetss = *off
     C                   endif
      *
     C                   if        sstoemp <> 0 and sstoemp < exemp#
     C                   eval      meetss = *off
     C                   endif
      *
     C                   if        ssname <> *blanks
      *** begin del ***  01/07/15  ************************************
     C*********          eval      kkfnd = exfnd
     C*********          eval      kkemp# = exemp#
      ****beg*add **** 04/24/02 ***************************************
     C*********          eval      kkdiv = exdiv
      *** end add **** 04/24/02 ***************************************
     C*****emky          chain     pem01                              60
      *** end del *****  01/07/15  ************************************
     C     Lo:Up         xlate(p)  emname        u1pdsc
     C     Lo:Up         xlate(p)  ssname        u2pdsc
     C                   exsr      string
     C                   endif
     C                   endif                                                  *in21
      *
     C                   if        *in22
     C                   if        ssfnd <> 0 and ssfnd <> exofnd
     C                   eval      meetss = *off
     C                   endif
      *
     C                   if        ssfremp <> 0 and ssfremp > exoemp
     C                   eval      meetss = *off
     C                   endif
      *
     C                   if        sstoemp <> 0 and sstoemp < exoemp
     C                   eval      meetss = *off
     C                   endif
      *
     C                   if        ssname <> *blanks
      *** begin del ***  01/07/15  ************************************
     C*********          eval      kkfnd = exofnd
     C*********          eval      kkemp# = exoemp
      ****beg*add **** 04/24/02 ***************************************
     C*********          eval      kkdiv = exodiv
      ****end*add **** 04/24/02 ***************************************
     C*****emky          chain     pem01                              60
      *** end del *****  01/07/15  ************************************
     C     Lo:Up         xlate(p)  emname        u1pdsc
     C     Lo:Up         xlate(p)  ssname        u2pdsc
     C                   exsr      string
     C                   endif
     C                   endif                                                  *in22
      *
     C                   endsr
** Column headings
Opt  Grp  Employer#    Div  Employer DBA          Grp  Employer#    Div  Cx/Rw
Opt  Grp  Employer#    Div  Employer DBA          Grp  Employer#    Div  Cx/Rw
**
     -----------------Current-------------------  --------Prior--------
     -------------------Prior-------------------  -------Current-------
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               NNY
217F17=Subset               YYY
3
