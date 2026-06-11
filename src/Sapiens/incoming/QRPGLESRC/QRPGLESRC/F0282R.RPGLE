     H**************************************************************************************
     H copyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**************************************************************************************
     H* H/TITLE - Work With Case Providers Start/End Dates                                 *
     H*                                                                                    *
     H* DESCRIPTION: This program shows the subfile containing current start/end dates of  *
     H*              the providers. User can also maintain these date.                     *
     H*                                                                                    *
     H* CALLED BY: MENU                                                                    *
     H*                                                                                    *
     H* CALLS TO : F0282R01                                                                *
     H*                                                                                    *
     H*                                                                                    *
     H*  Created: 12/12/11   Programmer C:277   Project:114MAW                             *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
    H*
     H actgrp(*caller)
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff0282f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Ffasep     if   e             disk
     F                                     rename(pse01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Ffasel1    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pse01:dbrcdf)
    F*
    F*  Claim file
     Ffmcmp     if   e             disk
    F*
    F*  Claim Phys/Prov/Spec Trans File
     Ffappl1    if   e           k disk
    F*
    F*  WCMC Provider Master
     Ffmpdl3    if   e           k disk
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Case Provider Start/End Dates'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F0282R01'
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
     D ##sub#          s              5  0
     D ##fyr           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D ##rrn2          s              9  0
     D @@call          s              1
     D kkco#           s                   like(seco#)
     D kksub#          s                   like(sesub#)
     D kkfnd           s                   like(sefnd )
     D kkfyr           s                   like(sefyr )
     D kkcase          s                   like(secase)
     D kkseq#          s                   like(seseq#)
     D ##seq#          s                   like(seseq#)
     D kkfein          s                   like(sefein)
     D kkmod           s                   like(semod )
     D kkstdt          s                   like(sestdt)
     D kkendt          s                   like(seendt)
     D ##stdt          s                   like(sestdt)
     D ##endt          s                   like(seendt)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C*   RRN parameter returns rrn# value to all program which calls it on F10
     C     *entry        plist
     C                   parm      *inkc         ##kc
     C                   parm      *inkl         ##kl
     C                   parm                    ##mode
     C                   parm                    ##rrn
     C                   parm                    ##rrn2
     C                   parm                    ##seq#
     C                   parm                    ##stdt
     C                   parm                    ##endt
    C*
    C*  Add/change/display program parameter list
    C*
     C*   RRN parameter sends rrn# value to all programs which displays further data
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm                    ##rrn2
     C                   parm                    ##seq#
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C                   parm                    ##sub#
     C                   parm                    ##fyr
     C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*
     C     seky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkseq#
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkstdt
     C*
     C     s1ky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkseq#
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C*
     C     ppky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkseq#
     C*
     C     p1ky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C     pdky          klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
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
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C*
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C* Start Date
     C                   z-add     sestdt        srdti
     C                   move      *blanks       srdto
     C                   MOVE      *blanks       srerr
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scstdt
     C* End Date
     C                   z-add     seendt        srdti
     C                   move      *blanks       srdto
     C                   MOVE      *BLANKS       srerr
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scendt
     C* Status
     C                   MOVEL     sestat        scstat
     C*
     C                   eval      scdata = %editw(scstdt:'  /  /    ')  +
     C                             '    ' + %editw(scendt:'  /  /    ') +
     C                             '                                     '
     C                             + scstat
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
     C*
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C*
     C* Return the Current Start and End Date
     C*
     C                   eval      kkstdt = *hival
     C     seky          setll     dbrcdf                             96
     C     s1ky          reade     dbrcdf
     C                   eval      ##stdt = sestdt
     C                   eval      ##endt = seendt
     C
     C                   eval      *inlr = *on
     C*
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C     ##MODE        COMP      '1'                                    09
     C     ##MODE        COMP      '2'                                    09
     C     ##MODE        COMP      '3'                                    09
     C     ##MODE        COMP      'D'                                    09
     C     ##MODE        COMP      'A'                                    40
     C*
     C* GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1             WC INST
     C                   PARM      *BLANK        ##MCIF            1             MC INST
     C                   PARM      *BLANK        ##WCIN            1             WC INTERFACE
     C                   PARM      *BLANK        ##AR              1             MC AR INST
     C                   PARM      *BLANK        ##NTWK            1             NETWORK INST
     C                   PARM      *BLANK        ##UR              1             PRECERT INST
     C                   PARM      *BLANK        ##MED             1             MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1             EDI INST
     C                   PARM      *BLANK        ##WCDK            1             WC DARTS KEY
     C
     C     ##rrn2        chain     pcm01                              99
     C*
     C                   move      cmco#         kkco#
     C                   move      cmsub#        kksub#
     C                   move      cmfnd         kkfnd
     C                   move      cmfyr         kkfyr
     C                   move      cmcase        kkcase
     C                   eval      kkseq# = ##seq#
     C                   eval      kkstdt = *hival
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   move      cmco#         #1co#             3
     C                   move      cmsub#        #1sub#            5
     C                   move      cmfnd         #1fnd             3
     C                   move      cmfyr         #1fyr             3
     C                   move      cmcase        #1case            7
     C*
     C                   CALL      'FA702R'
     C                   parm      *blank        ##rtcd            1
     C                   parm                    #1co#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
     C     scl1hd        parm      *blank        ##L1             78
     C     scl2hd        parm      *blank        ##L2             78
     C*
     C* Get provider information
     C*
     C     ppky          setgt     PPP01                              99
     C                   if        *in99 = *off
     C     p1ky          readpe    PPP01                                  99
     C                   if        *in99 = *off
     C                   move      ppfein        scfein
     C                   move      ppmod         scmod
     C* Set key list
     C                   move      ppseq#        kkseq#
     C                   move      ppfein        kkfein
     C                   move      ppmod         kkmod
     C*
     C* Get name and area information
     C*
     C     pdky          chain     ppd01                              99
     C                   movel     pdname        scname
     C                   Z-ADD     pdarea        scarea
     C                   movel     pdphn#        scphna
     C                   move      pdphn#        scphnb
     C                   endif
     C                   endif
     C
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C*
     C                   if        *in21 = *on
     C     s1ky          reade     dbrcdf                                 95
     C* save return start date
     C* save return end  date
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C     s1ky          readpe    dbrcdf                                 95
     C* save return start date
     C* save return end  date
     C                   endif
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   if        *in21 = *on
     C     seky          setgt     dbrcdf                             96
     C                   endif
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   if        *in21 = *on
     C     seky          setll     dbrcdf                             96
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
     C*
     C                   MOVE      SCDBRR        ##RRN
     C*
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C*
     C                   z-add     ssstdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   MOVEL     SRDTO         xxstdt            7 0
     C                   endif
     C*
     C                   if        ssstdt <>  *zeros and xxstdt <> sestdt
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   z-add     ssendt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   MOVEL     SRDTO         xxendt            7 0
     C                   endif
      *
     C                   if        ssendt <>  *zeros and xxendt <> seendt
     C                   eval      meetss = *off
     C                   endif
      *
     C                   if        ssstat <> ' ' and ssstat <> sestat
     C                   eval      meetss = *off
     C                   endif
      *
     C                   endsr
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                    99
     C*
     C                   endsr
    C*================================================================
** Column headings
Opt  Start date    End date                                     Status
** Description--------------MISN
1011=Select                 NNN
1022=Change                 YYN
1044=Delete                 NNN
1055=Display                YYN
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YYN
212F12=Cancel               YYY
217F17=Subset               YYY
5
