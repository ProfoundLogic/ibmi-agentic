     H* (c) 2013 StoneRiver, Inc. All rights Reserved.                                     *
     H* StoneRiver is a trademark of StoneRiver, Inc.                                      *
     H actgrp(*caller)                                                                     *
     H/TITLE F2366R01                                                                      *
     H**************************************************************************************
     H*  Description:  Medicare RVU Information Maintenance                                *
     H*                                                                                    *
     H*  Created: 07/03/09   Programmer C:227   Project:AR038                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  02/20/25   C:342    Project : FS709                                               *
     H*                      Phys (Medicare) FS                                            *
     H*                      Updated xxmin to round to 1 decimal                           *
     H*                                                                                    *
     H*  03/02/18   C:079    Support : TASB 63603                                          *
     H*                      Modify F7=Calculate tool to correctly get FTGPP date          *
     H*                                                                                    *
     H*  01/24/13   C:277    Project : FS430                                               *
     H*                      Modify F7=Calculate tool, for 1/1/2013 or later date          *
     H*                                                                                    *
     H*  07/26/12   C:277    Project : TN016                                               *
     H*                      Add Function key F8=Calculate Tennessee allowance             *
     H*                                                                                    *
     H*  10/11/11   C:280    Project : SEQ78229                                            *
     H*                      Added option 20 for status change                             *
     H*                                                                                    *
     H*  01/17/11   C:198    Project : FS388                                               *
     H*                      Added processing for F7=Calculate Medicare allowance          *
     H*                                                                                    *
     H*  10/01/10   C:198    Project : WV009                                               *
     H*                      Added medicare release level on WW and maintenance screen     *
     H*                                                                                    *
     H*  08/12/10   C:251    Project : 114MWS                                              *
     H*                      Add field Status code                                         *
     H*                                                                                    *
     H**************************************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2366f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fftrvp     uf a e             disk
     F                                     rename(prv01:dbrcdf)
     Fftrvl1    if   e           k disk
     Ffmacl1    if   e           k disk    rename(pac01:pac0l)
     Ffmacp     if   e             disk
     Fftgpl3    if   e           k disk    rename(pgp01:pgp0l)
     Fftgpp     if   e             disk
     F*** Beg Add *** 07/26/12 ****************************************
     Fststl     if   e           k disk
     Fftrtl2    if   e           k disk
     F*** End Add *** 07/26/12 ****************************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Medicare RVU Table'
     D comapi          c                   'FA007R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D kkefdt          s                   like(rvefdt)
     D kkmod1          s                   like(rvmod1)
     D kkprcd          s                   like(rvprcd)
     D kkmrel          s                   like(rvmrel)
     D kkst            s                   like(acst)
     D kkloc           s                   like(acloc)
     D xxefdt          s                   like(acefdt)
     D*** Beg Add *** 07/26/12 ****************************************
     D kkkey           s                   like(stkey)
     D kktype          s                   like(rttype)
     D msgsur          c                   const('Surgery')
     D msgort          c                   const('Ortho/Neuro')
     D msgmed          c                   const('Medicine')
     D msgeme          c                   const('Emergency Medicine')
     D*** End Add *** 07/26/12 ****************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C     statl         plist
     C                   parm                    @@call            1
     C                   parm                    @@kc2             1
     C                   parm                    @@kl2             1
     C                   parm                    ##rrn2            9 0
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     rvky          klist
     C                   kfld                    kkprcd
     C                   kfld                    kkmod1
     C                   kfld                    kkefdt
     C                   kfld                    kkmrel
     C     fmky          klist
     C                   kfld                    kkst
     C                   kfld                    kkloc
     C                   kfld                    xxefdt
     C     fmky1         klist
     C                   kfld                    kkst
     C                   kfld                    kkloc
     C*** Beg Add *** 07/26/12 ****************************************
     C*
     C     stky          klist
     C                   kfld                    kkkey
     C*
     C     rtky          klist
     C                   kfld                    kkst
     C                   kfld                    kktype
     C                   kfld                    kkefdt
     C     rtky1         klist
     C                   kfld                    kkst
     C                   kfld                    kktype
     C*** End Add *** 07/26/12 ****************************************
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHGSTS - Subroutine for changing status
    C*================================================================
    C*
     C     chgsts        begsr
    C*
     C     @@rrn         chain     dbrcdf                             99
    C*
     C                   if        *in99 = *off
    C*
     C                   if        rvstat = 'I'
     C                   eval      rvstat = 'A'
     C                   else
     C                   eval      rvstat = 'I'
     C                   endif
    C*
     C                   movel     q1user        rvuser
     C                   movel     q1pgm         rvpgm
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rvdt
     C                   movel     dstime        rvtime
    C*
     C                   update    dbrcdf
    C*
     C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     C* HCPC code must not be blank (If in ADD mode)
     C                   if        *in40 = *on and
     C                             scprcd = *blanks
     C                   seton                                        719089
     C                   leavesr
     C                   endif
     C* Modifier must not be blank (If in ADD mode)
     C                   if        *in40 = *on and
     C                             scmod1 = *blanks
     C                   seton                                        729088
     C                   leavesr
     C                   endif
     C* Description must not be blank (If in ADD mode)
     C                   if        *in40 = *on and
     C                             scdesc = *blanks
     C                   seton                                        749086
     C                   leavesr
     C                   endif
     C* Effective date not be blank (If in ADD mode)
     C                   if        *in40 = *on
     C                   if        scefdt = *zero
     C                   seton                                        739087
     C                   leavesr
     C                   else
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C                   if        srerr = '1'
     C                   seton                                        779087
     C                   leavesr
     C                   endif
     C                   endif
     C* Release must not be blank (If in ADD mode)
     C                   if        *in40 = *on and
     C                             scmrel = *blanks
     C                   seton                                        666790
     C                   leavesr
     C                   endif
     C* Check for duplicate record in FTRVP(If in ADD mode)
     C                   eval      kkprcd = scprcd
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkefdt
     C                   eval      kkmod1 = scmod1
     C                   eval      kkmrel = scmrel
     C     rvky          chain     prv01
     C                   if        %found
     C                   seton                                        769089
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   clear                   scprcd
     C                   clear                   scmod1
     C                   clear                   scefdt
     C                   clear                   scdesc
     C                   clear                   scwkrv
     C                   clear                   sctnpe
     C                   clear                   scfnpe
     C                   clear                   sctfpe
     C                   clear                   scffpe
     C                   clear                   scmprv
     C                   clear                   sctnft
     C                   clear                   scfnft
     C                   clear                   sctfat
     C                   clear                   scffat
     C                   clear                   scglob
     C                   clear                   scpre
     C                   clear                   scintr
     C                   clear                   scpost
     C                   clear                   scmult
     C                   clear                   scbila
     C                   clear                   scasst
     C                   clear                   sccosg
     C                   clear                   sctesg
     C                   clear                   scenba
     C                   clear                   scconv
     C                   clear                   scstcd
     C                   clear                   scmrel
     C                   clear                   scstat
     C                   if        *in40 = *on
     C                   eval      scprcd = *blanks
     C                   eval      scmod1 = *blanks
     C                   eval      scefdt = *zeros
     C                   eval      scdesc = *blanks
     C                   eval      scwkrv = *zeros
     C                   eval      sctnpe = *zeros
     C                   eval      scfnpe = *zeros
     C                   eval      sctfpe = *zeros
     C                   eval      scffpe = *zeros
     C                   eval      scmprv = *zeros
     C                   eval      sctnft = *zeros
     C                   eval      scfnft = *zeros
     C                   eval      sctfat = *zeros
     C                   eval      scffat = *zeros
     C**                 eval      scpctc = *blanks
     C                   eval      scglob = *blanks
     C                   eval      scpre  = *zeros
     C                   eval      scintr = *zeros
     C                   eval      scpost = *zeros
     C                   eval      scmult = *blanks
     C                   eval      scbila = *blanks
     C                   eval      scasst = *blanks
     C                   eval      sccosg = *blanks
     C                   eval      sctesg = *blanks
     C                   eval      scenba = *blanks
     C                   eval      scconv = *zeros
     C                   eval      scstcd = *blanks
     C                   eval      scmrel = *blanks
     C                   eval      scstat = 'A'
     C*
     C                   else
     C*
     C                   if        *in99 = *off
     C                   eval      scprcd = rvprcd
     C                   eval      *in51 = *off
     C                   if        rvefdt >= 1100101
     C                   if        scprcd >= '00100' and scprcd <= '01999'
     C                             and %subst(scprcd:5:1) <> 'T'
     C                             and %subst(scprcd:5:1) <> 'F'
     C                             or (scprcd >= '10000' and scprcd <= '99999')
     C                   eval      *in51 = *on
     C                   else
     C                   eval      *in51 = *off
     C                   endif
     C                   else
     C                   eval      *in51 = *off
     C                   endif
     C*** Beg Add *** 07/26/12 ****************************************
      /Free

         kkkey = 'TN';
         Chain stky pst01;
           If %found(ststl) and ST1A3 = 'Y';
             *in65 = *on;
           Else;
             *in65 = *off;
           Endif;

      /End-Free
     C*** End Add *** 07/26/12 ****************************************
     C                   eval      scmod1 = rvmod1
     C                   z-add     rvefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C                   movel     rvdesc        scdesc
     C                   eval      scwkrv = rvwkrv
     C                   eval      sctnpe = rvtnpe
     C                   eval      scfnpe = rvfnpe
     C                   eval      sctfpe = rvtfpe
     C                   eval      scffpe = rvffpe
     C                   eval      scmprv = rvmprv
     C                   eval      sctnft = rvtnft
     C                   eval      scfnft = rvfnft
     C                   eval      sctfat = rvtfat
     C                   eval      scffat = rvffat
     C**                 eval      scpctc = rvpctc
     C                   eval      scglob = rvglob
     C                   eval      scpre  = rvpre
     C                   eval      scintr = rvintr
     C                   eval      scpost = rvpost
     C                   eval      scmult = rvmult
     C                   eval      scbila = rvbila
     C                   eval      scasst = rvasst
     C                   eval      sccosg = rvcosg
     C                   eval      sctesg = rvtesg
     C                   eval      scenba = rvenba
     C                   eval      scconv = rvconv
     C                   eval      scstcd = rvstcd
     C                   eval      scmrel = rvmrel
     C                   eval      scstat = rvstat
     C                   endif
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   eval      rvprcd = scprcd
     C                   eval      rvmod1 = scmod1
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rvefdt
     C                   eval      rvdesc = scdesc
     C                   eval      rvwkrv = scwkrv
     C                   eval      rvtnpe = sctnpe
     C                   eval      rvfnpe = scfnpe
     C                   eval      rvtfpe = sctfpe
     C                   eval      rvffpe = scffpe
     C                   eval      rvmprv = scmprv
     C                   eval      rvtnft = sctnft
     C                   eval      rvfnft = scfnft
     C                   eval      rvtfat = sctfat
     C                   eval      rvffat = scffat
     C**                 eval      rvpctc = scpctc
     C                   eval      rvglob = scglob
     C                   eval      rvpre  = scpre
     C                   eval      rvintr = scintr
     C                   eval      rvpost = scpost
     C                   eval      rvmult = scmult
     C                   eval      rvbila = scbila
     C                   eval      rvasst = scasst
     C                   eval      rvcosg = sccosg
     C                   eval      rvtesg = sctesg
     C                   eval      rvenba = scenba
     C                   eval      rvconv = scconv
     C                   eval      rvstcd = scstcd
     C                   eval      rvmrel = scmrel
     C                   eval      rvstat = scstat
     C*
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C   51*inkg         caseq     *on           kgdo
     C                   endcs
     C*** Beg Add *** 07/26/12 ****************************************
      /Free

         If *in65 = *on and *inkh = *on;
            exsr khdo;
         Endif;

      /End-Free
     C*** End Add *** 07/26/12 ****************************************
     C                   endsr
    C*================================================================
    C* KGDO   - Processing for F7=Calculate Medicare Allowance
    C*================================================================
     C     kgdo          begsr
     C*
     C                   dou       *inkc = *on or *inkl = *on
     C                   if        scprcd >= '00100' and scprcd <= '01999'
     C                             and %subst(scprcd:5:1) <> 'T'
     C                             and %subst(scprcd:5:1) <> 'F'
     C                   eval      *in70 = *on
     C                   else
     C                   eval      *in70 = *off
     C                   endif
     C                   if        *inkg = *on
     C                   clear                   ssst
     C                   clear                   ssdesc
     C                   clear                   ssloc
     C                   clear                   ssmin
     C                   endif
     C                   exfmt     wdw01
     C                   movea     '0000'        *in(53)
     C*
     C                   if        *inkj = *on
     C*
     C                   select
     C                   when      hfld = 'SSST' or hfld = 'SSLOC'
     C*
     C                   if        scprcd >= '00100' and scprcd <= '01999'
     C                             and %subst(scprcd:5:1) <> 'T'
     C                             and %subst(scprcd:5:1) <> 'F' Or (
     C                             scprcd >= '99100' and scprcd <= '99140')
     C*** call F2373R
     C                   eval      @@call  = '2'
     C                   eval      ##rrn2 = *zeros
     C                   call      'F2373R'      statl
     C*
     C                   if        @@kc2 = '1'
     C                             or @@kl2 = '1'
     C                             or ##rrn2 = *zeros
     C                   select
     C                   when      hfld = 'SSST'
     C                   eval      *in80 = *on
     C                   when      hfld = 'SSLOC'
     C                   eval      *in78 = *on
     C                   endsl
     C                   endif                                                  ##rrn2 = 0
     C*
     C                   if        ##rrn2 <> 0
     C     ##rrn2        chain     pac01                              99
     C                   if        *in99 = *off
     C                   movel     acst          ssst
     C                   movel     acdesc        ssdesc
     C                   movel     acloc         ssloc
     C                   eval      *in79 = *on
     C                   write     wdw01
     C                   endif                                                  *in99 = '0'
     C                   endif                                                  @@rrn <> 0
     C*
     C                   endif                                                  scprcd is anes
     C*
     C                   if        scprcd >= '10000' and scprcd <= '99999'
     C                             and (scprcd < '99100' or scprcd > '99140')
     C*** call F2367R
     C                   eval      ##rrn2 = *zeros
     C                   eval      @@call  = '2'
     C                   call      'F2367R'      statl
     C*
     C                   if        @@kc2 = '1'
     C                             or @@kl2 = '1'
     C                             or ##rrn2 = *zeros
     C                   select
     C                   when      hfld = 'SSST'
     C                   eval      *in80 = *on
     C                   when      hfld = 'SSLOC'
     C                   eval      *in78 = *on
     C                   endsl
     C                   endif                                                  ##rrn2 = 0
     C*
     C                   if        ##rrn2 <> 0
     C     ##rrn2        chain     pgp01                              99
     C                   if        *in99 = *off
     C                   movel     gpst          ssst
     C                   movel     gplnam        ssdesc
     C                   movel     gploc         ssloc
     C                   eval      *in79 = *on
     C                   write     wdw01
     C                   endif                                                  *in99 = '0'
     C                   endif                                                  @@rrn = 0
     C*
     C                   endif                                                  scprcd is not anes
     C*
     C                   when      hfld = 'SSMIN'
     C                   eval      *in53 = *on
     C                   endsl
     C*
     C                   endif                                                  *inkj = *on
     C*
     C                   eval      *in90 = *off
     C*
     C                   if        *inkj  <> '1' and *inkc <> '1' and
     C                             *inkl <> '1'
     C*
     C                   movea     '0000'        *in(53)
     C*
     C                   exsr      errwdw1
     C                   if        *in90 = *off
     C                   exsr      calmed
     C                   endif
     C*
     C                   endif                                                  *inkj/*inkc/*inkl=1
     C*
     C                   enddo                                                  dou *inkc = *on
     C*
     C                   endsr
    C*================================================================
    C* ERRWDW1   - Error check on window 1
    C*================================================================
     C     errwdw1       begsr
     C*
     C*** state and loc combination not in fmacp/ftgpp
     C                   if        ssst =  ' '
     C                   eval      *in54 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C*
     C                   if        ssst =  ' ' and ssloc = 0
     C                   eval      *in54 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C*
     C                   if        ssst <> ' '
     C*
     C                   if        scprcd >= '00100' and scprcd <= '01999'
     C                             and %subst(scprcd:5:1) <> 'T'
     C                             and %subst(scprcd:5:1) <> 'F'
     C                              and ssmin = 0
     C                   eval      *in55 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C*
     C                   movel     ssst          kkst
     C                   movel     ssloc         kkloc
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxefdt
     C                   if        scprcd >= '00100' and scprcd <= '01999'
     C                             and %subst(scprcd:5:1) <> 'T'
     C                             and %subst(scprcd:5:1) <> 'F'
     C                             or (scprcd >= '99100' and scprcd <= '99140')
     C     fmky          setgt     pac0l
     C     fmky1         readpe    pac0l                                  99
     C                   if        *in99 = *on
     C                   eval      *in56 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C                   movel     acdesc        ssdesc
     C                   write     wdw01
     C                   endif
     C*
     C                   if        scprcd >= '10000' and scprcd <= '99999'
     C                             and (scprcd < '99100' or scprcd > '99140')
     C     fmky          setgt     pgp0l
     C     fmky1         readpe    pgp0l                                  99
     C                   if        *in99 = *on
     C                   eval      *in56 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C                   movel     gplnam        ssdesc
     C                   write     wdw01
     C                   endif
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* CALMED   - Processing to calculate Medicare Allowance
    C*================================================================
     C     calmed        begsr
     C*
     C                   movea     '000'         *in(52)
     C                   eval      kkst   = ssst
     C                   eval      kkloc  = ssloc
     C*
     C                   if        scprcd >= '00100' and scprcd <= '01999'
     C                             and %subst(scprcd:5:1) <> 'T'
     C                             and %subst(scprcd:5:1) <> 'F' or
     C                             (scprcd >= '99100' and scprcd <= '99140')
     C*** chain on FMACP using input state and loc
     C                   eval      *in50 = *on
     C                   eval      *in52 = *off
     C                   clear                   wdprcd
     C                   clear                   wdst
     C                   clear                   wdloc
     C                   clear                   wddesc
     C                   clear                   wdalow
     C                   eval      wdprcd = scprcd
     C                   eval      wdst   = ssst
     C                   eval      wddesc = ssdesc
     C                   eval      wdloc  = ssloc
     C     fmky          chain     pac0l                              99
     C                   if        *in99 = *off
     C*
     C********** Beg Chg *** 02/20/25 *****************************************
     C******             z-add     0             xxmin             5 2
     C                   z-add     0             xxmin             5 1
     C********** End Chg *** 02/20/25 *****************************************
     C                   if        scprcd >= '00100' and scprcd <= '01999'
     C                             and %subst(scprcd:5:1) <> 'T'
     C                             and %subst(scprcd:5:1) <> 'F'
     C                   eval(h)   xxmin = ssmin/15
     C                   eval      *in70 = *on
     C                   eval      wdmin  = ssmin
     C                   endif                                                  scprcd = 00100-01999
     C                   eval      wdconv = accf
     C                   eval      wdalow = (xxmin + rvwkrv) * accf
     C*
     C                   endif                                                  *in99 = *off
     C*
     C                   endif                                                  scprcd = anes
     C*
     C                   if        scprcd >= '10000' and scprcd <= '99999'
     C                             and (scprcd < '99100' or scprcd > '99140')
     C*
     C                   eval      *in52 = *on
     C                   eval      *in50 = *off
     C                   clear                   w2prcd
     C                   clear                   wdmod1
     C                   clear                   w2st
     C                   clear                   w2loc
     C                   clear                   wdfal
     C                   clear                   wdnfal
     C                   eval      wdmod1 = scmod1
     C                   eval      w2prcd = scprcd
     C                   eval      w2st   = ssst
     C                   eval      w2desc = ssdesc
     C                   eval      w2loc  = ssloc
     C*** chain on FTGPP using state and loc
     C*** Beg Chg *** 03/02/18 ****************************************
     C**** fmky          chain     pgp0l                              99
     C     fmky          Setgt     pgp0l
     C     fmky1         Readpe    pgp0l                                  99
     C*** End Chg *** 03/02/18 ****************************************
     C                   if        *in99 = *off
     C
     C                   eval      wdwkgp = gpwkgp
     C                   eval      wdpegp = gppegp
     C                   eval      wdmpgp = gpmpgp
     C*
     C*** Beg Add *** 01/24/13 ****************************************
     C                   if        scefdt < 01012013
     C*** End Add *** 01/24/13 ****************************************
     C                   eval      wdnfal = ((rvwkrv * gpwkgp) +
     C                                       (rvtnpe * gppegp) +
     C                                       (rvmprv * gpmpgp)) * rvconv
     C*
     C                   eval      wdfal  = ((rvwkrv * gpwkgp) +
     C                                       (rvtfpe * gppegp) +
     C                                       (rvmprv * gpmpgp)) * rvconv
     C*** Beg Add *** 01/24/13 ****************************************
     C                   else
     C                   eval      wdnfal = ((rvwkrv * gpwkgp) +
     C                                       (rvfnpe * gppegp) +
     C                                       (rvmprv * gpmpgp)) * rvconv
     C*
     C                   eval      wdfal  = ((rvwkrv * gpwkgp) +
     C                                       (rvffpe * gppegp) +
     C                                       (rvmprv * gpmpgp)) * rvconv
     C                   endif
     C*** End Add *** 01/24/13 ****************************************
     C                   endif                                                  *in99 = *off
     C                   endif                                                  scprcd = 10000-69999
     C*
     C                   dou       *inkc = *on or *inkl = *on
     C                   exfmt     wdw02
     C                   enddo
     C*
     C                   eval      *inkl = *off
     C*
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
     C     @@mode        comp      'D'                                    09
     C     @@mode        comp      'A'                                    40
     C     @@mode        comp      'C'                                    10
     C                   if        *in09 = *on
     C                   movel     moded         scmode
     C                   endif
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
    C*
    C*
     C                   select
     C     @@mode        wheneq    'S'
     C                   exsr      chgsts
     C                   return
     C                   endsl
    C*
    C*
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   movel     q1user        rvuser
     C                   movel     q1pgm         rvpgm
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rvdt
     c                   movel     dstime        rvtime
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   movel     q1user        rvuser
     C                   movel     q1pgm         rvpgm
     C                   movel     q1pgm         rvipgm
     C                   movel     q1user        rviusr
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rvdt
     C                   movel     srdto         rvidt
     c                   movel     dstime        rvtime
     c                   movel     dstime        rvitim
     C                   write     dbrcdf
     C                   feod      ftrvp
     C                   endsr
     C*** Beg Add *** 07/26/12 ****************************************
      /Free
    
       //KHDO   - Processing for F8=Calculate Tennessee Allowance
    
         Begsr khdo;

           Dou *inkc = *on or *inkl = *on;
              If scprcd >= '00100' and scprcd <= '01999'
                 and %subst(scprcd:5:1) <> 'T'
                 and %subst(scprcd:5:1) <> 'F';
                 *in70 = *on;
              Else;
                 *in70 = *off;
              Endif;

              If *inkh = *on;
                 w1st = 'TN';
                 w1desc = stname;
                 w1loc = 35;
              EndIf;

              exfmt wdw01a;

              If *inkc <> *on and *inkl <> *on;
                exsr calmedtn;
              Endif;                                                              //   *inkc/*inkl=1

           Enddo;                                                                 // dou *inkc = *on

         Endsr;

    
       //CALMEDTN - Processing to calculate Tennessee Allowance
    
         Begsr calmedtn;

            w2mod1 = scmod1;
            w2prcd = scprcd;
            w2st   = w1st;
            w2desc = w1desc;
            w2loc  = w1loc;

            If scprcd >= '10000' and scprcd <= '99999'
               and (scprcd < '99100' or scprcd > '99140');

               w2mod1 = scmod1;
               w2prcd = scprcd;
               w2st   = w1st;
               w2desc = w1desc;
               w2loc  = w1loc;

          //Chain to FTRTP for TN type = M to lookup the Medicare Conversion Factor
               kkst  = w1st;
               kkloc = w1loc;
               xxefdt = rvefdt;
               Setgt fmky pgp0l;
               Readpe fmky1 pgp0l;

               kkefdt = *hival;
               kktype = 'M';
               Setgt rtky prt01;
               Readpe rtky1 prt01;
                  If %found(ftrtl2);

                     w2wkgp = gpwkgp;
                     w2pegp = gppegp;
                     w2mpgp = gpmpgp;

                    Select;

          //Surgery and Ortho/Neuro
                    When w2prcd >= '10000' and w2prcd<= '69999';
                    w2nfal = ((rvwkrv * gpwkgp) +
                               (rvtnpe * gppegp) +
                               (rvmprv * gpmpgp)) * rtgscf;

                    w2nfrt = ((rvwkrv * gpwkgp) +
                               (rvtnpe * gppegp) +
                               (rvmprv * gpmpgp)) * rtoncf;
                    w2nfds = msgsur;
                    w2nrds = msgort;

                    w2fal  = ((rvwkrv * gpwkgp) +
                               (rvtfpe * gppegp) +
                               (rvmprv * gpmpgp)) * rtgscf;

                    w2fart = ((rvwkrv * gpwkgp) +
                               (rvtfpe * gppegp) +
                               (rvmprv * gpmpgp)) * rtoncf;
                    w2fads = msgsur;
                    w2frds = msgort;

          //Radiology
                    When w2prcd >= '70000' and w2prcd<= '79999';
                    w2nfal = ((rvwkrv * gpwkgp) +
                               (rvtnpe * gppegp) +
                               (rvmprv * gpmpgp)) * rtrdcf;

                    w2nfds = *blanks;
                    w2nrds = *blanks;

                    w2fal  = ((rvwkrv * gpwkgp) +
                               (rvtfpe * gppegp) +
                               (rvmprv * gpmpgp)) * rtrdcf;

                    w2fads = *blanks;
                    w2frds = *blanks;

          //Path/lab
                    When w2prcd >= '80000' and w2prcd<= '89999';
                    w2nfal = ((rvwkrv * gpwkgp) +
                               (rvtnpe * gppegp) +
                               (rvmprv * gpmpgp)) * rtlbcf;

                    w2nfds = *blanks;
                    w2nrds = *blanks;

                    w2fal  = ((rvwkrv * gpwkgp) +
                               (rvtfpe * gppegp) +
                               (rvmprv * gpmpgp)) * rtlbcf;

                    w2fads = *blanks;
                    w2frds = *blanks;

          //Physical/Occupational Therapy
                    When (w2prcd >= '97001' and w2prcd<= '97006') or
                         (w2prcd >= '97032' and w2prcd<= '97039') or
                         (w2prcd >= '97110' and w2prcd<= '97456') or
                         (w2prcd >= '97750' and w2prcd<= '97799');
                    w2nfal = ((rvwkrv * gpwkgp) +
                               (rvtnpe * gppegp) +
                               (rvmprv * gpmpgp)) * rtptcf;

                    w2nfds = *blanks;
                    w2nrds = *blanks;

                    w2fal  = ((rvwkrv * gpwkgp) +
                               (rvtfpe * gppegp) +
                               (rvmprv * gpmpgp)) * rtptcf;

                    w2fads = *blanks;
                    w2frds = *blanks;

          //Chiropractic
                    When w2prcd >= '98940' and w2prcd<= '98943';
                    w2nfal = ((rvwkrv * gpwkgp) +
                               (rvtnpe * gppegp) +
                               (rvmprv * gpmpgp)) * rtchcf;

                    w2nfds = *blanks;
                    w2nrds = *blanks;

                    w2fal  = ((rvwkrv * gpwkgp) +
                               (rvtfpe * gppegp) +
                               (rvmprv * gpmpgp)) * rtchcf;

                    w2fads = *blanks;
                    w2frds = *blanks;

          //Other codes (between 10000 - 99999)
                    Other;
                    w2nfal = ((rvwkrv * gpwkgp) +
                               (rvtnpe * gppegp) +
                               (rvmprv * gpmpgp)) * rtmdcf;

                    w2nfrt = ((rvwkrv * gpwkgp) +
                               (rvtnpe * gppegp) +
                               (rvmprv * gpmpgp)) * rtemcf;
                    w2nfds = msgmed;
                    w2nrds = msgeme;

                    w2fal  = ((rvwkrv * gpwkgp) +
                               (rvtfpe * gppegp) +
                               (rvmprv * gpmpgp)) * rtmdcf;

                    w2fart = ((rvwkrv * gpwkgp) +
                               (rvtfpe * gppegp) +
                               (rvmprv * gpmpgp)) * rtemcf;
                    w2fads = msgmed;
                    w2frds = msgeme;

                    Endsl;
                  Endif;                                                        //      *in99 = *off
            Endif;                                                              //prcd = 10000-69999



            Dou *inkc = *on or *inkl = *on;
               exfmt wdw02a;
            Enddo;

            *inkl = *off;

         Endsr;
      /End-Free
     C*** End Add *** 07/26/12 ****************************************
