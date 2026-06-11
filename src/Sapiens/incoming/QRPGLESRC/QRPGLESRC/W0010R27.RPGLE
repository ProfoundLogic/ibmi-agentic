     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*
     H/TITLE W0010R27 - Group Policy Export Info
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  Update/display group Policy Export Info *
     H*                                                               *
     H* CALLED BY: W0010R                                             *
     H*                                                               *
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H* CREATED: 06/26/09  C:198 PROJECT: WC1694                      *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/19/19 C:079 Project: 150WEM                                *
     H*                Default to Include in Policy Export Group      *
     H*                Select                                         *
     H*                                                               *
     H* 07/23/09 C:198 Project: WC1694-Priss 7                        *
     H*                Fixed when on display, mode on R.H.S is coming *
     H*                as change.                                     *
     H*                                                               *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0010f27  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmpep     uf a e             disk
     F                                     rename(ppe01:dbrcdf)
     Fwmfnp     if   e             disk
     Fwmpel     if   e           k disk
     F                                     infds(dbfds)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Group Policy Export'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Data structures
    D*---------------------------------------------------------------------
     Ddbfds            ds
     D dbrrn                 397    400b 0
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
     D @@rrn2          s              9  0
     d*** Beg add *** 12/19/19 *********************************************
     D cltId           s              3
     d*** End add *** 12/19/19 *********************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     d*** Beg add *** 12/19/19 *********************************************
     d GetClientId     pr                  extpgm('RTVCLTID')
     d  cltId                         3
     d** End add *** 12/19/19 *********************************************
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
     C                   parm                    @@rrn2
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
     C     peky          klist
     C                   kfld                    fnco#
     C                   kfld                    fnfnd
     C*
     C     peky1         klist
     C                   kfld                    fnco#
     C                   kfld                    fnfnd
     C                   kfld                    kkefdt
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
     c*
     c  n90              if        scefdt <> *zeros
     c*
     c                   eval      srdti = scefdt
     c                   call      'SRCVTDAT'    datchk
     c*
     c                   if        srerr = '1'
     c                   seton                                        88  90
     c                   else
     c                   movel     srdto         scefdt
     c                   endif
     c*
     c                   endif                                                  scefdt <> 0

     c  n90              if        scexdt <> *zeros
     c*
     c                   eval      srdti = scexdt
     c                   call      'SRCVTDAT'    datchk
     c*
     c                   if        srerr = '1'
     c                   seton                                        89  90
     c                   else
     c                   movel     srdto         scexdt
     c                   endif
     c*
     c                   endif                                                  scexdt <> 0

     c  n90              if        scexdt <> *zeros
     c*
     c                   eval      srdti = scefdt
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxefdt            7 0
     c                   eval      srdti = scexdt
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxexdt            7 0
     c*
     c                   if        xxexdt < xxefdt
     c                   seton                                        53  90
     c                   endif
     c*
     c                   endif                                                  scexdt <> 0
     c*
     c  n90              if        scendo <> ' ' and scendo <> 'Y'
     c                   seton                                        56  90
     c                   endif
     c*
     c  n90              if        scadda <> ' ' and scadda <> 'Y'
     c                   seton                                        57  90
     c                   endif
     c*
     c  n90              if        scbnet <> *blanks and scbnet <> 'Y'
     c                   seton                                        77  90
     c                   endif
     c*
     c  n90              if        scnet  <> *blanks  and scnet <> 'Y'
     c                   seton                                        78  90
     c                   endif
     c*
     c  n90              if        scptyc <> *blanks  and scptyc <> 'Y'
     c                   seton                                        79  90
     c                   endif
     c*
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
     c*
     C                   if        *in40 = *on
     C                   eval      scmode = modea
     C                   eval      outflg = *on
     C                   else
     C                   eval      scmode = modec
     C                   endif
     C*** BEG ADD *** 07/23/09 ****************************************
     C                   if        @@mode = 'D'
     C                   eval      scmode = moded
     C                   endif
     C*** END ADD *** 07/23/09 ****************************************
     c                   eval      srdti = peefdt
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         scefdt
     c                   eval      srdti = peexdt
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         scexdt
     c                   eval      scbnet = pebnet
     c                   eval      scnet  = penet
     c                   eval      scptyc = peptyc
     c                   eval      scendo = peendo
     c                   eval      scadda = peadda
       //*** Beg Add *** 12/19/19 ******************************************
      /Free
                         If PEdftGrpSl = *blank;
                            PEdftGrpSl = 'E';
                         Endif;
                         scDftGrpsl = PEdftGrpSl;
      /End-Free
       //*** End Add *** 12/19/19 ******************************************
     c*
     c                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     c*
     c                   eval      peco#  = fnco#
     c                   eval      pefnd  = fnfnd
     c                   eval      srdti = scefdt
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         peefdt
     c                   eval      srdti = scexdt
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         peexdt
     c                   eval      pebnet = scbnet
     c                   eval      penet  = scnet
     c                   eval      peendo = scendo
     c                   eval      peadda = scadda
     c                   eval      peptyc = scptyc
       //*** Beg Add *** 12/19/19 ******************************************
      /Free
                         PEdftGrpSl = scDftGrpSl;
      /End-Free
       //*** End Add *** 12/19/19 ******************************************
     c*
     c                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     c                   seton                                        lr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C                   eval      *in40 = *off
     C                   if        @@mode = 'D'
     C                   eval      *in09 = *on
     C                   else
     C                   eval      *in09 = *off
     C                   endif
     C*
     C     @@rrn2        chain     wmfnp                              60
     c  n60              eval      scfnd = fnfnd
     C  n60              movel     fnname        scname
     C*
     C                   move      *hival        kkefdt            7 0
     C     peky          chain     ppe01                              61
     C  n61              z-add     dbrrn         @@rrn
     C   61              z-add     0             @@rrn
     C*
     C                   if        not *in09
     C  n61              eval      @@mode  = 'C'
     C   61              eval      @@mode  = 'A'
     C                   if        @@mode = 'A'
     C                   eval      *in40 = *on
     C                   endif
     C                   endif
       //*** Beg Add *** 12/19/19 ******************************************
      /Free
                         GetClientID(cltId);
                         is150 = *off;
                         If cltId = '150';
                            is150 = *on;
                         Endif;
      /End-Free
       //*** End Add *** 12/19/19 ******************************************
     C*
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  c                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   eval      peuser = q1user
     C                   eval      pepgm = q1pgm
     C                   time                    dsdttm
     C                   eval      petime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         pedt
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   eval      peuser = q1user
     C                   eval      peiusr = q1user
     C                   eval      pepgm = q1pgm
     C                   eval      peipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      petime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         pedt
     C                   movel(p)  srdto         peidt
     C                   write     dbrcdf
     C                   feod      wmpep
     C                   endsr
