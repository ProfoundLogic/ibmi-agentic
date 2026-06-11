     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H DFTACTGRP(*NO) ACTGRP(*caller)
    H*
    H*****************************************************************
    H*  F8540R - Bill Extract Process                                *
    H*                                                               *
    H*****************************************************************
    H*                                                               *
    H*  Called from: F8540C                                          *
    H*  Calls to:    None                                            *
    H*                                                               *
    H*****************************************************************
    H*  Created on 01/04/2007 by c:62 for Project 144MAE             *
    H*****************************************************************
    H*  Revisions:                                                   *
    H*                                                               *
    H*  03/21/14 c:212 Project: 144MCO                               *
    H*                 Make revenue code and patient account code#   *
    H*                 available for check print                     *
    H*                                                               *
    H*  03/23/10 c:079 Project: 144MBT                               *
    H*                 Updating network field using FAU1P            *
    H*                                                               *
    H*  02/24/10 c:227 Project: 144MBT                               *
    H*                 Updating network field.                       *
    H*                                                               *
    H*  04/02/08 c:62 Project: MC144 62870                           *
    H*                Change how notes are loaded.                   *
    H*                                                               *
    H*  09/19/07 c:62 Project: MC144 xxxxx                           *
    H*                Keep the counters from exceeding there size    *
    H*                                                               *
    H*  07/12/07 c:62 Project: MC144 59054                           *
    H*                Populate Place of Service (CLR11)              *
    H*                                                               *
    H*****************************************************************
    F*
    F*  Physical file definition (for maintaining record)
     Ffaeal1    if   e           k disk
     Ffaeol4    if   e           k disk
     Ffaedl     if   e           k disk
     Ffaehl     if   e           k disk
     Ffaepl     if   e           k disk
     Ffaeul     if   e           k disk

     Ffddal4    if   e           k disk
     Ffdcalh    if   e           k disk
     Ffdddl     if   e           k disk
     Ffddel     if   e           k disk
     Ffddhl     if   e           k disk
     Ffddpl     if   e           k disk
     Ffddul     if   e           k disk

     Ffmcml1    if   e           k disk
     Ffmcdl     if   e           k disk
     Ffmpdl     if   e           k disk
     Ffmp1l     if   e           k disk
     Ffmrxl     if   e           k disk
     Fnmnwl     if   e           k disk
     Fftabl     if   e           k disk    usropn
     Ffmdil     if   e           k disk
     F*** BEG ADD *** 03/23/10 *********************************************
     Ffau1l2    if   e           k disk
     F*** END ADD *** 03/23/10 *********************************************

     Ffsaudb    o    e             disk    rename(fsaudb:pcv01)
     Ffsbill    o    e             disk    rename(fsbill:pcv02)
    D*================================================================
    D*  User defined data structures
    D*================================================================
     D psds          esds                  extname(szq1p)
     D qca             s             56    dim(2) ctdata perrcd(1)
     D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D*** BEGIN ADD *** 04/02/08 *******************************************
     D                 ds
     D notes                       1120
     D  clc04                        30    overlay(notes)
     D  clc05                        30    overlay(notes:31)
     D  clc06                        30    overlay(notes:61)
     D  clc07                        30    overlay(notes:91)
     D  clc08                        30    overlay(notes:121)
     D  clc09                        30    overlay(notes:151)
     D  clc10                        30    overlay(notes:181)
     D  clc11                        30    overlay(notes:211)
     D*** END   ADD *** 04/02/08 *******************************************
     D                 ds
     D qqcmd                         56
     D  qqst                          2    overlay(qqcmd:38)
     D                 ds
     D billID                        12
     D  eob#                          9  0 overlay(billID)
     D  rcn                           3  0 overlay(billID:10)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D kkkey           s                   like(pmkey)
     D kkicd9          s                   like(diicd9)
     D count           s              6  0
     D xxdate          s              8  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D length          s             15  5
     D xxblty          s              2
     D dxr1            s              1
     D dxr2            s              1
     D dxr3            s              1
     D dxr4            s              1
     D x               s              1  0
     D y               s              1  0
     D*** BEGIN ADD *** 04/02/08 *******************************************
     D notesIN         s           1140
     D*** END   ADD *** 04/02/08 *******************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtyym2        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtyym3        plist
     C                   parm                    srdti
     C                   parm      '*MDYY'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     eobky         klist
     C                   kfld                    eaco#
     C                   kfld                    easub#
     C                   kfld                    eafnd
     C                   kfld                    eafyr
     C                   kfld                    eacase
     C                   kfld                    eaeob#
    C*
     C     daky          klist
     C                   kfld                    eaco#
     C                   kfld                    easub#
     C                   kfld                    eafnd
     C                   kfld                    eafyr
     C                   kfld                    eacase
     C                   kfld                    eaeob#
     C                   kfld                    earcn
     C                   kfld                    cadseq
    C*
     C     caky          klist
     C                   kfld                    eaco#
     C                   kfld                    easub#
     C                   kfld                    eafnd
     C                   kfld                    eafyr
     C                   kfld                    eacase
     C                   kfld                    eaeob#
    C*
     C     detky         klist
     C                   kfld                    eaco#
     C                   kfld                    easub#
     C                   kfld                    eafnd
     C                   kfld                    eafyr
     C                   kfld                    eacase
     C                   kfld                    eaeob#
     C                   kfld                    dadseq
    C*
     C     cmky          klist
     C                   kfld                    eaco#
     C                   kfld                    easub#
     C                   kfld                    eafnd
     C                   kfld                    eafyr
     C                   kfld                    eacase
    C*
     C     cdky          klist
     C                   kfld                    eass#
    C*
     C     pdky          klist
     C                   kfld                    eafein
     C                   kfld                    eamod
    C*
     C     p1ky          klist
     C                   kfld                    eafein
     C                   kfld                    eamod
    C*
     C     nwky          klist
     C                   kfld                    eaco#
     C                   kfld                    eontwk
    C*
     C     pmky          klist
     C                   kfld                    eaco#
     C                   kfld                    kkkey
    C*
     C     diky          klist
     C                   kfld                    kkicd9
     C*** BEG ADD *** 03/23/10 *********************************************
     C     u1ky          klist
     C                   kfld                    eoeob#
     C*** END ADD *** 03/23/10 *********************************************
    C*****************************************************************
    C*
    C* EXECUTE MAIN SUBROUTINE
    C*
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    DTYYMD
     C                   movel     srdto         xxdate
     C*
     C                   read      pea01                                  99
     C                   dow       not *in99
     C*
     C                   exsr      wrtHeader
     C                   exsr      wrtDetail
     C*
     C                   read      pea01                                  99
     C                   enddo
    C*
     C                   seton                                        lr
    C*================================================================
    C*  clsOverride - Close and Delter Override FTABL
    C*================================================================
     C     clsOverride   begsr
     C*
     C                   close     ftabl
     C                   call      'QCMDEXC'                              99
     C                   parm                    qca(2)
     C                   parm      56            length
     C*
     C                   endsr
    C*================================================================
    C*  opnOverride - Open and Override FTABL
    C*================================================================
     C     opnOverride   begsr
     C*
     C                   eval      length = 56
     C                   movel(p)  qca(1)        qqcmd
     C                   movel(p)  eoast         qqst
     c*
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm      56            length
     C                   open      ftabl
     C*
     C                   endsr
    C*================================================================
    C*  wrtDetail - Write the detail record.
    C*================================================================
     C     wrtDetail     begsr
     C*
     C                   exsr      opnOverride
     C*
     C     caky          chain     pca01                              99
     C                   dow       not *in99
    C*
     C                   clear                   pcv02
     C*
     C     daky          chain     pda01
     C                   if        not %found
     C                   clear                   dachrg
     C                   clear                   damra
     C                   clear                   dandis
     C                   clear                   dapaid
     C                   endif
     C*
     C                   clear                   kkkey
     C*
     C                   select
     C                   when      eobtyp = 'D'
     C     detky         chain     pdd01
     C                   if        not %found
     C                   clear                   pdd01
     C                   else
     C                   eval      kkkey = ddmod1
     C                   endif
     C                   when      eobtyp = 'N'
     C     detky         chain     pdh01
     C                   if        not %found
     C                   clear                   pdh01
     C                   else
     C                   select
     C                   when      dhmod1 <> *blanks
     C                   eval      kkkey = dhmod1
     C                   when      dhmod2 <> *blanks
     C                   eval      kkkey = dhmod2
     C                   when      dhmod3 <> *blanks
     C                   eval      kkkey = dhmod3
     C                   when      dhmod4 <> *blanks
     C                   eval      kkkey = dhmod4
     C                   endsl
     C                   endif
     C                   when      eobtyp = 'H'
     C     detky         chain     pdu01
     C                   if        not %found
     C                   clear                   pdu01
     C                   else
     C                   eval      kkkey = dupmod
     C                   endif
     C                   when      eobtyp = 'X'
     C     detky         chain     pdp01
     C                   if        not %found
     C                   clear                   pdp01
     C                   endif
     C                   endsl
    C*
     C     pmky          chain     ppm01
     C                   if        not %found
     C                   clear                   ppm01
     C                   endif
    C*
     C                   eval      clr02 = billID
     C                   eval      clr03 = calin#
     C                   eval      srdti = cafdat
     C                   call      'SRCVTDAT'    DTYYM2
     C                   movel     srdto         clr04
     C*
     C                   select
     C                   when      eobtyp = 'D'
     C                   movel(p)  ddprcd        clr05
     C                   movel(p)  ddprds        clr06
     C                   movel(p)  ddunts        clr07
     C                   movel(p)  ddmod1        clr08
     C*** begin add *** 07/12/07 *******************************************
     C                   movel(p)  ddpos         clr11
     C*** end   add *** 07/12/07 *******************************************
     C                   eval      dxr1=' '
     C                   eval      dxr2=' '
     C                   eval      dxr3=' '
     C                   eval      dxr4=' '
     C                   if        dddxr1 = 'X'
     C                   eval      dxr1='1'
     C                   endif
     C                   if        dddxr2 = 'X'
     C                   eval      dxr2='2'
     C                   endif
     C                   if        dddxr3 = 'X'
     C                   eval      dxr3='3'
     C                   endif
     C                   if        dddxr4 = 'X'
     C                   eval      dxr4='4'
     C                   endif
     C                   eval      clr20a = %trim(dxr1) + %trim(dxr2) +
     C                                      %trim(dxr3) + %trim(dxr4)
     C                   when      eobtyp = 'N'
     C                   movel(p)  dhprcd        clr05
     C                   movel(p)  dhprds        clr06
     C                   movel(p)  dhunts        clr07
     C***** beg chg *** 03/21/14 *******************************************
     C*****              select
     C*****              when      dhmod1 <> *blanks
     C*****              eval      clr08 = dhmod1
     C*****              when      dhmod2 <> *blanks
     C*****              eval      clr08 = dhmod2
     C*****              when      dhmod3 <> *blanks
     C*****              eval      clr08 = dhmod3
     C*****              when      dhmod4 <> *blanks
     C*****              eval      clr08 = dhmod4
     C*****              endsl

     C                   eval      clr08 = dhmod1
     C***** end chg *** 03/21/14 *******************************************
     C*** begin add *** 07/12/07 *******************************************
     C                   move (p)  dhpos         clr11
     C*** end   add *** 07/12/07 *******************************************
     C                   eval      dxr1=' '
     C                   eval      dxr2=' '
     C                   eval      dxr3=' '
     C                   eval      dxr4=' '
     C                   if        dhdxr1 = 'X'
     C                   eval      dxr1='1'
     C                   endif
     C                   if        dhdxr2 = 'X'
     C                   eval      dxr2='2'
     C                   endif
     C                   if        dhdxr3 = 'X'
     C                   eval      dxr3='3'
     C                   endif
     C                   if        dhdxr4 = 'X'
     C                   eval      dxr4='4'
     C                   endif
     C                   eval      clr20a = %trim(dxr1) + %trim(dxr2) +
     C                                      %trim(dxr3) + %trim(dxr4)
     C                   when      eobtyp = 'H'
     C                   movel(p)  durate        clr05
     C                   movel(p)  dudesc        clr06
     C                   movel(p)  duunts        clr07
     C                   movel(p)  dupmod        clr08
     C*** begin add *** 07/12/07 *******************************************
     C                   move (p)  dupos         clr11
     C*** end   add *** 07/12/07 *******************************************
     C                   eval      clr08 = dupmod
     C                   when      eobtyp = 'X'
     C                   eval      clr16 = dpdesc
     C                   eval      clr17 = dpndc
     C                   eval      clr18 = dpunts
     C                   endsl
    C*
     C                   movel(p)  pmdesc        clr09
     C                   eval      clr12 = dachrg
     C                   eval      clr13 = damra
     C                   eval      clr14 = dandis + caothr + caoth2
     C                   eval      clr15 = dapaid
     C                   eval      clr20 = rxawp
     C*
     C                   eval      x=1
     C                   eval      y=1
     C*** BEGIN ADD *** 04/02/08 *******************************************
     C                   clear                   notesIN
     C*** END   ADD *** 04/02/08 *******************************************
     C     detky         chain     pde01                              99
     C                   dow       not *in99
     C*
     C                   if        destat <> 'I' and deprnt <> 'N'
     C                   if        deeob = *blanks
     C*** BEGIN CHG *** 04/02/08 *******************************************
     C**********         select
     C**********         when      x=1
     c**********         movel(p)  denot1        clc04
     C**********         when      x=2
     c**********         movel(p)  denot1        clc05
     C**********         when      x=3
     c**********         movel(p)  denot1        clc06
     C**********         when      x=4
     c**********         movel(p)  denot1        clc07
     C**********         when      x=5
     c**********         movel(p)  denot1        clc08
     C**********         when      x=6
     c**********         movel(p)  denot1        clc09
     C**********         when      x=7
     c**********         movel(p)  denot1        clc10
     C**********         when      x=8
     c**********         movel(p)  denot1        clc11
     C**********         endsl
     C                   if        x<=8
     C                   if        x=1
     C                   eval      notesIN = %trim(denot1) + ' ' +
     C                                       %trim(denot2)
     C                   else
     C                   eval      notesIN = %trim(notesIN) + ' ' +
     C                                 %trim(denot1) + ' ' + %trim(denot2)
     C                   endif
     C                   endif
     C*** END   CHG *** 04/02/08 *******************************************
     c*** begin add *** 09/19/07 *******************************************
     c                   if        x<9
     c*** end   add *** 09/19/07 *******************************************
     C                   eval      x=x+1
     c*** begin add *** 09/19/07 *******************************************
     c                   endif
     c*** end   add *** 09/19/07 *******************************************
     C                   else
     C                   select
     C                   when      y=1
     c                   movel(p)  denot1        qua04
     c                   movel(p)  deeob         qua12
     C                   when      y=2
     c                   movel(p)  denot1        qua05
     c                   movel(p)  deeob         qua13
     C                   when      y=3
     c                   movel(p)  denot1        qua06
     c                   movel(p)  deeob         qua14
     C                   when      y=4
     c                   movel(p)  denot1        qua07
     c                   movel(p)  deeob         qua15
     C                   when      y=5
     c                   movel(p)  denot1        qua08
     c                   movel(p)  deeob         qua16
     C                   when      y=6
     c                   movel(p)  denot1        qua09
     c                   movel(p)  deeob         qua17
     C                   endsl
     c*** begin add *** 09/19/07 *******************************************
     c                   if        y<9
     c*** end   add *** 09/19/07 *******************************************
     C                   eval      y=y+1
     c*** begin add *** 09/19/07 *******************************************
     c                   endif
     c*** end   add *** 09/19/07 *******************************************
     C                   endif
     C                   endif
     C*
     C     detky         reade     pde01                                  99
     C                   enddo
     C*** BEGIN ADD *** 04/02/08 *******************************************
     C                   movel(p)  notesIN       notes
     C*** END   ADD *** 04/02/08 *******************************************

     C***** beg add *** 03/21/14 *******************************************
      /free
       select;

       when eobtyp = 'H';
          clr21 = durvcd;
          clr22 = dumod2;
          clr23 = dumod3;
          clr24 = dumod4;

       when eobtyp = 'N';
          clr22 = dhmod2;
          clr23 = dhmod3;
          clr24 = dhmod4;

       endsl;
      /end-free
     C***** end add *** 03/21/14 *******************************************
    C*
     C                   write     pcv02
     C*
     C     caky          reade     pca01                                  99
     C                   enddo
     C*
     C                   exsr      clsOverride
    C*
     C                   endsr
    C*================================================================
    C*  wrtHeader - Write the header record.
    C*================================================================
     C     wrtHeader     begsr
    C*
     C                   clear                   pcv01
    C*
     C     eobky         chain     peo01
     C                   if        not %found
     C                   clear                   peo01
     C                   endif
     C*
     C                   select
     C                   when      eobtyp = 'D'
     C     eobky         chain     ped01
     C                   if        not %found
     C                   clear                   ped01
     C                   endif
     C                   when      eobtyp = 'N'
     C     eobky         chain     peh01
     C                   if        not %found
     C                   clear                   peh01
     C                   endif
     C                   when      eobtyp = 'H'
     C     eobky         chain     peu01
     C                   if        not %found
     C                   clear                   peu01
     C                   endif
     C                   when      eobtyp = 'X'
     C     eobky         chain     pep01
     C                   if        not %found
     C                   clear                   pep01
     C                   endif
     C                   endsl
    C*
     C     cmky          chain     pcm01
     C                   if        not %found
     C                   clear                   pcm01
     C                   endif
     C     cdky          chain     pcd01
     C                   if        not %found
     C                   clear                   pcd01
     C                   endif
     C     pdky          chain     ppd01
     C                   if        not %found
     C                   clear                   ppd01
     C                   endif
     C     p1ky          chain     pp101
     C                   if        not %found
     C                   clear                   pp101
     C                   endif
     C     nwky          chain     pnw01
     C                   if        not %found
     C                   clear                   pnw01
     C                   endif
    C*
     C                   eval      eob# = eaeob#
     C                   eval      rcn = earcn
     C                   eval      blr02 = billID
     C                   eval      srdti = eocrdt
     C                   call      'SRCVTDAT'    DTYYM2
     C                   movel     srdto         blr04
     C                   move      xxdate        blr05
     C                   eval      blr06 = cmsrc
     C                   eval      srdti = cmacdt
     C                   call      'SRCVTDAT'    DTYYM2
     C                   movel     srdto         blr09
     C*
     C                   select
     C                   when      eobtyp = 'D'
     C*
     C                   eval      kkicd9 = eddia1
     C     diky          chain     pdi01
     C                   if        %found
     C                   eval      blr10 = dids1
     C                   endif
     C*
     C                   eval      kkicd9 = eddia2
     C     diky          chain     pdi01
     C                   if        %found
     C                   eval      blr11 = dids1
     C                   endif
     C*
     C                   eval      kkicd9 = eddia3
     C     diky          chain     pdi01
     C                   if        %found
     C                   eval      blr12 = dids1
     C                   endif
     C*
     C                   eval      kkicd9 = eddia4
     C     diky          chain     pdi01
     C                   if        %found
     C                   eval      blr13 = dids1
     C                   endif
     C*
     C                   eval      blr14 = 'OTH'
     C                   when      eobtyp = 'N'
     C*
     C                   eval      kkicd9 = ehdia1
     C     diky          chain     pdi01
     C                   if        %found
     C                   eval      blr10 = dids1
     C                   endif
     C*
     C                   eval      kkicd9 = ehdia2
     C     diky          chain     pdi01
     C                   if        %found
     C                   eval      blr11 = dids1
     C                   endif
     C*
     C                   eval      kkicd9 = ehdia3
     C     diky          chain     pdi01
     C                   if        %found
     C                   eval      blr12 = dids1
     C                   endif
     C*
     C                   eval      kkicd9 = ehdia4
     C     diky          chain     pdi01
     C                   if        %found
     C                   eval      blr13 = dids1
     C                   endif
     C*
     C                   eval      blr14 = 'MED'
     C                   when      eobtyp = 'H'
     C*
     C                   eval      kkicd9 = eudia1
     C     diky          chain     pdi01
     C                   if        %found
     C                   eval      blr10 = dids1
     C                   endif
     C*
     C                   eval      kkicd9 = eudia2
     C     diky          chain     pdi01
     C                   if        %found
     C                   eval      blr11 = dids1
     C                   endif
     C*
     C                   eval      kkicd9 = eudia3
     C     diky          chain     pdi01
     C                   if        %found
     C                   eval      blr12 = dids1
     C                   endif
     C*
     C                   eval      kkicd9 = eudia4
     C     diky          chain     pdi01
     C                   if        %found
     C                   eval      blr13 = dids1
     C                   endif
     C*
     C                   eval      blr14 = 'HOS'
     C*
     C                   movel(p)  eublty        xxblty
     C                   if        xxblty ='13' or xxblty ='14' or
     C                             xxblty ='23' or xxblty ='33' or
     C                             xxblty ='74' or xxblty ='75' or
     C                             xxblty ='84' or eospcd ='REH' or
     C                             eospcd ='CAH' or
     C                             (xxblty ='83' and eospcd ='HOS')
     C                   eval      blr15 = 'O'
     C                   else
     C                   eval      blr15 = 'I'
     C                   endif
     C*
     C***** beg del *** 03/21/14 *******************************************
     C*****              eval      blr16 = euacct
     C***** end del *** 03/21/14 *******************************************
     C                   eval      blr17 = eupats
     C*
     C                   eval      srdti = euaddt
     C                   call      'SRCVTDAT'    dtyym2
     C                   movel     srdto         blr18
     C*
     C                   eval      srdti = euscpf
     C                   call      'SRCVTDAT'    dtyym2
     C                   movel     srdto         blr20
     C*
     C                   eval      srdti = euscpt
     C                   call      'SRCVTDAT'    dtyym2
     C                   movel     srdto         blr21
     C*
     C                   when      eobtyp = 'X'
     C                   eval      blr14 = 'PHA'
     C                   endsl
    C*
     C                   z-add     eabamt        blr22
     C*
     C                   eval      srdti = eofrdt
     C                   call      'SRCVTDAT'    dtyym2
     C                   movel     srdto         blr23
     C*
     C                   eval      srdti = eotodt
     C                   call      'SRCVTDAT'    dtyym2
     C                   movel     srdto         blr24
     C*
     C                   eval      count = 0
     C     caky          chain     pca01                              99
     C                   dow       not *in99
     C                   add       1             count
     C     caky          reade     pca01                                  99
     C                   enddo
     C                   move      count         blr25
    C*
     C                   eval      blr26 = eachrg
     C                   eval      blr27 = eamra
     C                   eval      blr28 = eandis + eaothr + eaoth2
     C                   eval      blr29 = eapaid
     C                   movel     xxdate        blr30
     C                   eval      ppr03 = 'T'
     C                   movel     eafein        ppr04
     C                   movel     eamod         ppr05
    C*
     C                   if        pdtype = '1'
     C                   movel(p)  'MD'          ppr06
     C                   else
     C                   movel(p)  'HOS'         ppr06
     C                   endif
    C*
     C                   eval      ppr07 = eapnam
     C                   eval      ppr08 = eaadr1
     C                   eval      ppr09 = eaadr2
     C                   eval      ppr10 = eacty
     C                   eval      ppr11 = east
     C                   movel     eazip         ppr12
     C                   movel     p1zip         ppr13
     C                   movel(p)  pdarea        ppr14
     C                   move      pdphn#        ppr14
     C                   movel(p)  nwname        ppr16
     C                   eval      ppr17 = cmss#
     C                   if        cdmid <> *blank
     C                   eval      ppr18 = %trim(cdlsn) + ', ' +
     C                                     %trim(cdfsn) + ' ' +
     C                                     %trim(cdmid) + '.'
     C                   else
     C                   eval      ppr18 = %trim(cdlsn) + ', ' +
     C                                     %trim(cdfsn)
     C                   endif
     C*
     C                   eval      srdti = cddtob
     C                   call      'SRCVTDAT'    dtyym2
     C                   movel     srdto         ppr19
    C*
     C                   movel(p)  cdadr1        ppr20
     C                   movel(p)  cdcty         ppr21
     C                   movel(p)  cdst          ppr22
     C                   movel(p)  cdzip         ppr23
    C*
     C*** BEG ADD *** 02/24/10 *********************************************
     C***************    movel(p)  eontwk        pponmb
     C*** END ADD *** 02/24/10 *********************************************
     C*** BEG ADD *** 03/23/10 *********************************************
     C     u1ky          chain     pu101                              99
     C                   if        *in99 = *off
     C                   movel(p)  u1ntwk        pponmb
     C                   endif
     C*** END ADD *** 03/23/10 *********************************************
     C***** beg add *** 03/21/14 *******************************************
      /free
       select;

       when eobtyp = 'H';
          blr31 = euacct;

       when eobtyp = 'N';
          blr31 = ehacct;

       endsl;
      /end-free
     C***** end add *** 03/21/14 *******************************************
     C                   write     pcv01
    C*
     C                   endsr
**  (QCMDEX)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
DLTOVR FILE(FTABL)
