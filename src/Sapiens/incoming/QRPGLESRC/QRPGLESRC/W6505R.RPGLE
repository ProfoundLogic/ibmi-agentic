     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
      *================================================================
      *  Description:  Work with return fund check schedule.          *
      *                                                               *
      *  Calls to:                                                    *
      *                                                               *
      *  Created: 04/01/2003  John Stone/Matt Dawson                  *
      *================================================================
      *  Revisions:                                                   *
      *                                                               *
      *  04/20/10 C:198 Support 72046                                 *
      *                 - Re-compiled.                                *
      *                                                               *
      *  07/18/07 C:136 Project: WC1491                               *
      *                 Implement changes for Return Fund             *
      *================================================================
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw6505f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
     *  Physical file definition (for refreshing subfile record)
     Fwac6p     if   e             disk
     F                                     rename(PC601:pfrcdf)
     *  Primary logical file definition (for displaying items)
     Fwac6l     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pc601:dbrcdf)
     Fswcpf     if   e           k disk
     *
     D hd              S             78    dim(1)  ctdata perrcd(1)
     D of              S              1    dim(15) ctdata perrcd(1)
     D ofd             S             35    dim(15) alt(of)
     D optnm           C                   'Return Fund Check Schedule'
     D slmsgi          C                   'WC39998'
     D slmsgf          C                   'WMESGG'
     D acdpgm          C                   'W6505R02'
     D lo              C                   'abcdefghijklmnopqrs-
     D                                     tuvwxyz'
     D up              C                   'ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ'
     D comapi          C                   'WA001R'
     D uspapi          C                   'W0006R'
     D infomx          C                   1
     D viewmx          C                   1
     D sf1pag          C                   10
     D datusa          S               d   datfmt(*usa)
     *----------------------------------------------------------------
     * Stand Alone Fields - TOP
     *----------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##mode2         s              1
     D ##rrn           s              9  0
     D @@call          s              1
      *
     D kkco#           s                   like(c6co#)
     D kkfnd           s                   like(c6fnd)
     D kkprdt          s                   like(c6prdt)
     D kkchk#          s                   like(c6chk#)
     D kkacct          s                   like(c6acct)
      *
     D                 ds
     D  dsdttm                 1     12  0
     D  dstime                 1      6  0
     D  dsdate                 7     12  0
      *
     D                 ds
     D  view1                  1     74
     D  v1prdt                 1     10
     D  v1fnd                 13     15
      *** beg chg ***** 07/18/07 **************************************
     D**v1acct**************  18     37
     D**v1cktp**************  40     49
     D  v1acct                18     30
     D  v1cktp                33     42
     D  v1appl                46     47
      *** end chg ***** 07/18/07 **************************************
     D  v1str#                52     58
     D  v1sta0                64     64
     D  v1sta1                72     72
     *----------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *----------------------------------------------------------------
     C/copy scopyww01c
     *================================================================
     *  PLIST definitions                                            *
     *================================================================
    C     *entry        plist
    C                   parm                    @@call
     *
     *  Add/change/display program parameter list
     *
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     *
     *  Company file/fund (group) retrieve program parameter list
     *
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
     *================================================================
     *  KLIST definitions                                            *
     *================================================================
     C     key1          klist
     C                   kfld                    kkco#
     *
     C     c6ky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkprdt
     C                   kfld                    kkfnd
     C                   kfld                    kkacct
     *================================================================
     * CHKSSM - Set indicator 11 for subset message                  *
     *================================================================
     C     chkssm        begsr
      *
     C                   if        ssacct <> *blanks or
     C                             ssfnd  <> *zeros  or
     C                             ssprdt <> *zeros
     C                   seton                                        11
     C                   endif
      *
     C                   endsr
     *================================================================
     * ERRCK1 - Additional errors on subfile selection               *
     *================================================================
     C     errck1        begsr
     C                   endsr
     *================================================================
     * ERRCK2 - Error checking for the subset screen                 *
     *================================================================
     C     errck2        begsr
     C                   endsr
     *================================================================
     * INZSST - Initialize the subset fields                         *
     *================================================================
     C     inzsst        begsr
     C                   endsr
     *================================================================
     * KEYINZ - Initialize key values for program start              *
     *================================================================
     C     keyinz        begsr
     C                   eval      kkco#  = ##co#
     C                   eval      kkprdt = *hival
     C                   eval      kkfnd  = *loval
     C                   eval      kkacct = *loval
     C                   endsr
     *================================================================
     * KEYSET - Set key values when doing "position to"              *
     *================================================================
     C     keyset        begsr
     C                   move(p)   scpst1        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkprdt
     C                   eval      kkfnd  = scpsta
     C                   eval      kkacct = scpstb
     C                   endsr
     *================================================================
     * KEYSFL - Set key values from subfile records for paging       *
     *================================================================
     C     keysfl        begsr
     C                   eval      kkprdt = scprdt
     C                   eval      kkfnd  = scfnd
     C                   eval      kkacct = scacct
     C                   endsr
     *================================================================
     * MVDBSC - Move data base fields to screen fields               *
     *================================================================
     C     mvdbsc        begsr
      *
     C                   eval      scprdt = c6prdt
     C                   eval      scfnd  = c6fnd
     C                   eval      scacct = c6acct
      *
     C     *cymd         move      c6prdt        datusa
     C                   move      datusa        v1prdt
     C                   eval      v1fnd  = %editc(c6fnd:'Z')
     C                   eval      v1acct = c6acct
     C                   eval      v1cktp = c6cktp
      *** beg add ***** 07/18/07 ******************************
     C                   eval      v1appl = c6appl
      *** end add ***** 07/18/07 ******************************
     C                   eval      v1str# = %editc(C6chk#:'Z')
     C                   eval      v1sta0 = C6sta0
     C                   eval      v1sta1 = C6sta1
     C                   movel(p)  view1         scdata
      *
     C                   endsr
     *================================================================
     * OTHFKS - Process other function keys                          *
     *================================================================
     C     othfks        begsr
      *
     C                   if        *inkg
     C                   exsr      chk_prompt
     C                   endif
      *
     C                   endsr
     *================================================================
     * OTHOPT - Process other options                                *
     *================================================================
     C     othopt        begsr
     C                   endsr
     *================================================================
     * Return premium check print prompt                             *
     *================================================================
     C     chk_prompt    begsr
      *
     C                   call      'W6502R'
     C                   parm                    ##prds           40
     C                   parm                    ##call            1

     C                   endsr
     *================================================================
     * WINDOW - Process popup window                                 *
     *================================================================
     C     window        begsr
     C                   endsr
     *================================================================
     * PGMEND - End of program processing                            *
     *================================================================
     C     pgmend        begsr
     C                   seton                                        lr
     C                   endsr
     *================================================================
     * PGMSTR - Start of program processing                          *
     *================================================================
     C     pgmstr        begsr
      *
      * Display window if return premium module not installed
      *
     C                   read      swcpf
     C                   if        %eof or WCFL25 <> 'Y'
     C                   exfmt     rpinstwdw
     C                   seton                                        lr
     C                   return
     C                   endif

     C                   if        @@call = 'Z'
     C                   eval      @@call = *blank
     C                   eval      xxopfn = '2'
     C                   eval      xxnmbr = '12'
     C                   eval      xxmis = 'YYY'
     C                   exsr      updofa
     C                   endif

     C                   endsr
     *================================================================
     * READ   - Read next record from file                           *
     *================================================================
     C     read          begsr
      *
     C                   if        *in21
     C     key1          reade(N)  dbrcdf                                 95
     C                   endif
      *
     C                   endsr
     *================================================================
     * READP  - Read previous record from file                       *
     *================================================================
     C     readp         begsr
      *
     C                   if        *in21
     C     key1          readpe(N) dbrcdf                                 95
     C                   endif
      *
     C                   endsr
     *================================================================
     * SETGT  - Set greater than on file                             *
     *================================================================
     C     setgt         begsr
      *
     C                   if        *In21
     C     c6ky          setgt     dbrcdf                             96
     C                   endif
      *
     C                   endsr
     *================================================================
     * SETLL  - Set lower limits on file                             *
     *================================================================
     C     setll         begsr
      *
     C                   if        *in21
     C     c6ky          setll     dbrcdf                             96
     C                   endif
      *
     C                   endsr
     *================================================================
     * SF1UPD - Update subfile record when item is deleted           *
     *================================================================
     C     sf1upd        begsr
     C                   eval      scdata = '***Deleted***'
     C                   endsr
     *================================================================
     * SLCTRD - Move fields for selected record to parameters        *
     *================================================================
     C     slctrd        begsr
     C                   endsr
     *================================================================
     * SUBSET - Determine if record should be put in subfile         *
     *================================================================
     C     subset        begsr
      *
     C                   if        ssacct <> *blanks
     C     LO:UP         xlate(p)  c6acct        u1pdsc           40
     C     LO:UP         xlate(p)  ssacct        u2pdsc           40
     C                   exsr      string
     C                   endif
      *
     C                   if        ssfnd <> *zeros and
     C                             ssfnd <> c6fnd
     C                   eval      meetss = *off
     C                   endif
      *
     C                   if        ssprdt <> *zero
     C                   eval      srdti = ssprdt
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         ssprdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxprdt            7 0
      *
     C                   if        xxprdt <> c6prdt
     C                   eval      meetss = *off
     C                   endif
      *
     C                   endif
      *
     C                   endsr
      *================================================================
      * STRING - SUBROUTINE TO SEARCH STRINGS                         *
      *================================================================
     C     string        begsr

     C                   call      'QCLSCAN'
     C                   parm                    u1pdsc
     C                   parm      40            strlen            3 0
     C                   parm      1             start             3 0
     C                   parm                    u2pdsc
     C                   parm      40            patlen            3 0
     C                   parm      '0'           trans             1
     C                   parm      '1'           trim              1
     C                   parm      ' '           wild              1
     C                   parm                    scnres            3 0
      *
     C                   if        scnres = 0
     C                   eval      meetss = *off
     C                   endif
      *
     C                   endsr
      *================================================================
** Column headings
Opt Proc Date   Grp  Bank Account   Bank Code   Type  Check #  Printed  Updated
** Description--------------MISN
1022=Change                 YYY
1044=Delete                 YNN
1055=Display                YNN
1                           NNN
1                           NNN
1                           NNN
1                           NNN
1                           NNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YYY
207F7=Generate Checks       YYY
212F12=Cancel               NYY
217F17=Subset               YYN
3
