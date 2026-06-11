     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*
     *****************************************************************
     *    PROGRAM: W2284R                                            *
     *                                                               *
     *    DESCRIPTION: W/W Audit company assignment history          *
     *                                                               *
     *    CREATED BY:  C:53   03/23/05   PROJECT# WC1196             *
     *                                                               *
     *****************************************************************
      * Revised:                                                      *
      * 03/23/09 C:220 Seq#: 63608                                    *
      *                 Add record when audit company or audit type   *
      *                 field updated.                                *
      *                                                               *
     *****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw2284f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwmhap     if   e             disk
     F                                     rename(pha01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwmhal     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pha01:dbrcdf)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Audit Co Assignment History'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W2284R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D chgnum          c                   'SA020R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   11
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
     D kkco#           s                   like(haco#)
     D kkfnd           s                   like(hafnd)
     D kkemp#          s                   like(haemp#)
     D kkdiv           s                   like(hadiv)
     D kkfyr           s                   like(hafyr)
     D kkidt           s                   like(haidt)
     D kkitm           s                   like(haitm)
     D @@call          s              1
     D @@kc            s              1
     D @@kl            s              1
     D @@co#           s              3  0
     D @@fnd           s                   like(scfnd)
     D @@fnam          s             50
     D @@emp#          s                   like(scemp#)
     D @@div           s                   like(scdiv)
     D @@enam          s                   like(scenam)
     D @@fyr           s                   like(scfyr)
     D @@cxdt          s                   like(sccxdt)
     D @@ppfr          s                   like(scppfr)
     D @@ppto          s                   like(scppto)
     D @@#div          s                   like(sc#div)
     D @@agt#          s                   like(scagt#)
     D @@agta          s                   like(scagta)
     D @@pdc#          s                   like(scpdc#)
     D @@pdca          s                   like(scpdca)
     D ##num           s             80
     D ##mask          s             10
     D ##dec           s              3  0
     D ##thou          s              1
     D                 ds
     D****** Beg Chg ******** 03/23/09 *************************************
     D****** xxdata                  1     75
     D xxdata                  1     76
     D   xxpac                 4      6
     D   xxnac                14     16
     D   xxtypa               25     26
     D******   xxst                 32     33
     D******   xxcnty               37     38
     D   xxcnty               29     30
     D******   xxzip1               41     45
     D   xxzip1               34     38
     D******   xxdash               46     46
     D         xxdash         39     39
     D******   xxzip2               47     50
     D         xxzip2         40     43
     D******   xxtypb               55     55
     D   xxnwdt               45     56
     D******   xxprem               56     66
     D   xxprem               54     64
     D******   xxstat               75     75
     D   xxordt               66     76
     D****** End Chg ******** 03/23/09 *************************************
     D****** Beg Add ******** 03/23/09 *************************************
     D                 ds
     D xxdat1                  1     76
     D   xxordt1              66     76
     D****** End Add ******** 03/23/09 *************************************
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
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@fnam
     C                   parm                    @@emp#
     C                   parm                    @@div
     C                   parm                    @@enam
     C                   parm                    @@fyr
     C                   parm                    @@cxdt
     C                   parm                    @@ppfr
     C                   parm                    @@ppto
     C                   parm                    @@#div
     C                   parm                    @@agt#
     C                   parm                    @@agta
     C                   parm                    @@pdc#
     C                   parm                    @@pdca
    C*
     C     chgpl         plist
     C                   parm                    ##num
     C                   parm                    ##mask
     C                   parm                    ##dec
     C                   parm                    ##thou
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
     C     kyread        klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C*
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C                   kfld                    kkidt
     C                   kfld                    kkitm
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
     C                   move      @@co#         kkco#
     C                   move      @@fnd         kkfnd
     C                   move      @@emp#        kkemp#
     C                   move      @@div         kkdiv
     C                   move      @@fyr         kkfyr
     C                   move      *hival        kkidt
     C                   move      *hival        kkitm
     C                   move      *blanks       xxordt
     C                   move      *blanks       xxprdt           10
     C*
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   eval      kkidt = haidt
     C                   eval      kkitm = haitm
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   move      *blanks       ##dec
     C                   move      *zeros        ##thou
     C*
     C                   z-add     haprem        xxwrk             9 0
     C                   movel(p)  xxwrk         ##num
     C                   movel(p)  'XXX,XXX'     ##mask
     C                   call      chgnum        chgpl
     C                   movel(p)  ##num         xxprem
     C*
     C                   eval      *in30 = *off
     C                   eval      xxpac  = hapac
     C                   eval      xxnac  = hanac
     C                   eval      xxtypa = hatypa
     C****** Beg Del ************  03/23/09 ********************************************
     C******             eval      xxst   = hast
     C****** End Del ************  03/23/09 ********************************************
     C                   eval      xxcnty = hacnty
     C                   movel     hazip         xxzip1
     C                   move      hazip         xxzip2
     C****** Beg Chg *********** 03/23/09 **************************************
     C******             eval      xxtypb = hatypb
     C                   Z-ADD     hadt          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         xxdate            8
     C
     C                   eval      xxnwdt = %SUBST(XXDATE:1:2) + '/' +
     C                             %SUBST(XXDATE:3:2) + '/' + %SUBST(XXDATE:5:4)
     C*
     C****** End Chg *********** 03/23/09 **************************************
     C****** Beg Del *********** 03/23/09 **************************************
     C******             eval      xxstat = hastat
     C****** End Del *********** 03/23/09 **************************************
     C****** Beg Add *********** 03/23/09 **************************************
     C*
     C                   read      dbrcdf
     C*
     C                   Z-ADD     hadt          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         xxdate            8
     C*
     C                   if        xxpac <> ' '
     C*
     C                   eval      xxordt = %SUBST(XXDATE:1:2) + '/' +
     C                             %SUBST(XXDATE:3:2) + '/' + %SUBST(XXDATE:5:4)
     C*
     C                   else
     C                   move      *blanks       xxordt
     C*
     C                   endif
     C*
     C                   readp     dbrcdf
     C*
     C****** End Add *********** 03/23/09 **************************************
     C                   eval      xxdash = '-'
     C*
     C     xxzip2        ifeq      '0000'
     C                   move      *blanks       xxzip2
     C                   eval      xxdash = ' '
     C                   endif
     C*
     C*                  move      *blanks       scbtyd
     C*                  move(p)   'BUSI'        kktbid
     C*                  movel(p)  xxtypb        kkkey
     C*    gtky          chain     pgt01                              99
     C* N99              movel(p)  gtdesc        scbtyd
     C*
     C                   eval      scdata = xxdata
     C*
     C                   eval      scpac = hapac
     C                   eval      scnac = hanac
     C                   eval      sctypa = hatypa
     C                   eval      scst = hast
     C                   eval      sccnty = hacnty
     C                   eval      sczip = hazip
     C*
     C*** chain to get description
     C*
     C*                  move      *blanks       scacod
     C*    acky          chain     pac01                              99
     C* N99              movel(p)  acname        scacod
     C*
     C*                  move      *blanks       sccntd
     C*                  movel(p)  scst          kcnkey
     C*                  movel(p)  sccnty        kcnky2
     C*                  movel(p)  kcnkey        kkcnt1
     C*    cnky          chain     pcn01                              99
     C* N99              movel(p)  cndesc        sccntd
     C*
     C*                  move      *blanks       scatyd
     C*    taky          chain     pta01                              99
     C* N99              movel(p)  tatypd        scatyd
     C*
     C*                  move      *blanks       scbtyd
     C*                  move(p)   'BUSI'        kktbid
     C*                  movel(p)  xxtypb        kkkey
     C*    gtky          chain     pgt01                              99
     C* N99              movel(p)  gtdesc        scbtyd
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
     C                   move      @@fnd         scfnd
     C                   movel(p)  @@fnam        scfnam
     C                   move      @@emp#        scemp#
     C                   move      @@div         scdiv
     C                   movel(p)  @@enam        scenam
     C                   move      @@fyr         scfyr
     C                   move      @@cxdt        sccxdt
     C                   move      @@ppfr        scppfr
     C                   move      @@ppto        scppto
     C                   move      @@#div        sc#div
     C                   move      @@agt#        scagt#
     C                   move      @@agta        scagta
     C                   move      @@pdc#        scpdc#
     C                   move      @@pdca        scpdca
     C*
     C                   move      @@co#         kkco#
     C                   move      @@fnd         kkfnd
     C                   move      @@emp#        kkemp#
     C                   move      @@div         kkdiv
     C                   move      @@fyr         kkfyr
     C*
     C                   move      *on           *in12
     C*
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C     kyread        reade     dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C     kyread        readpe    dbrcdf                                 95
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
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C                   endsr
** Column headings
 Audit-Co Audit-Co of-Audit Cnty Zip-Code   New-Date     Premium Org-Date
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               YYY
3
