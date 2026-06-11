      * (C) Copyright Fiserv, Inc., 2003 - 2009
      *
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      *****************************************************************
      * CREATED 02/07/2003 C:142 PROJECT: WC1100                      *
      *   CREATE FILE QFEUP1                                          *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 01/22/13 C:287 Seq#: SY266_OVR                                *
      *                   Added Activation group compile options      *
      *                                                               *
      * 05/12/09 C:136 Project: 135UMB                                *
      *                Add Carrier # to outfile                       *
      *                                                               *
      * 02/12/09 C:220 Project: 93WHY                                 *
      *                Additional field - info only out file WFEUP    *
      *                                                               *
      * 01/23/09 C:136 Project: WC1769                                *
      *                Update for new fields in CEU                   *
      *                                                               *
      * 05/21/08 C:136  Support: WA099 63605                          *
      *                 Update for new fields in CUE                  *
      *                                                               *
      * 12/06/04 C: 09    PROJECT: 93WEH/Seq # 44496                  *
      *                   Compile ONLY to add new fields in QFEUP.    *
      *                                                               *
      * 08/03/03 C: 132   PROJECT: WC970                              *
      *                   Compile ONLY to add new fields in QFEUP.    *
      *                                                               *
      *****************************************************************
      *                                                               *
     FQfeul     if   e           k disk
     FQfeup1    o  a e             disk    Rename(Peu01:Qeu01) Prefix(qeu:2)
     *
     D Datiso          S               D   DATFMT(*iso)
     D xxco#           S                   like(euco#)
     D xxrcym          S                   like(eurcym)
     D                 ds
     D xxcymd                         8  0
     D   xxcym                        6  0 Overlay(xxcymd:1)
     D   xxdd                         2  0 Overlay(xxcymd:7) Inz(01)
      *
     D PSDS          ESDS                  Extname(Szq1p)
      *
      *
      *  Read work file input
      *
     C     euky          chain     peu01                              99
     C                   Dow       *in99 = *off
      *
     C                   clear                   Qeu01
      *
      *  Convert all dates to *USA.
      *
     C     *usa0         move      @@todt        qeuasof
      *
     C                   if        eupfdt <> 0
     C     *usa          move      eupfdt        qeupfdt
     C                   Else
     C     *usa          move      '01/01/1901'  qeupfdt
     C                   Endif
      *
     C                   if        euprdt <> 0
     C     *usa          move      euprdt        qeuprdt
     C                   Else
     C     *usa          move      '01/01/1901'  qeuprdt
     C                   Endif
      *
     C                   if        eutrdt <> 0
     C     *usa          move      eutrdt        qeutrdt
     C                   Else
     C     *usa          move      '01/01/1901'  qeutrdt
     C                   Endif
      *
     C                   if        eupodt <> 0
     C     *usa          move      eupodt        qeupodt
     C                   Else
     C     *usa          move      '01/01/1901'  qeupodt
     C                   Endif
      *
     C                   if        euptdt <> 0
     C     *usa          move      euptdt        qeuptdt
     C                   Else
     C     *usa          move      '01/01/1901'  qeuptdt
     C                   Endif
      *
      *  Load remaining fields.
      *
     C                   eval      Qeuco#  = euco#
     C                   eval      Qeufnd  = eufnd
     C                   eval      Qeuemp# = euemp#
     C                   eval      Qeudiv  = eudiv
     C                   eval      Qeufyr  = eufyr
     C                   eval      Qeuname = euname
     C                   eval      Qeuinpr = euinpr
     C                   eval      Qeuydwr = euydwr
     C                   eval      Qeuydea = euydea
     C                   eval      Qeuunea = euunea
     C                   eval      Qeumdwr = eumdwr
     C                   eval      Qeumdea = eumdea
     C                   eval      Qeupmun = eupmun
     C                   eval      Qeupywr = eupywr
     C                   eval      Qeupyea = eupyea
     C                   eval      Qeupyun = eupyun
     C                   eval      Qeujrst = eujrst
     C                   eval      Qeuchap = euchap
     C                   eval      Qeucpds = eucpds
     C                   eval      Qeuas1  = euas1
     C                   eval      Qeuas2  = euas2
     C                   eval      Qeutax  = eutax
     C                   eval      Qeugnam = eugnam
     C                   eval      Qeucnam = eucnam
     C                   eval      Qeutype = eutype
     C                   eval      Qeucode = eucode
     C                   eval      Qeuagt# = euagt#
     C                   eval      Qeuagfe = euagfe
     C                   eval      Qeuagnm = euagnm
     C                   eval      Qeuprod = euprod
     C                   eval      Qeuprnm = euprnm
     C                   eval      Qeurate = eurate
     C                   eval      Qeusagt = eusagt
     C                   eval      Qeusfei = eusfei
     C                   eval      Qeusanm = eusanm
     C                   eval      Qeusprd = eusprd
     C                   eval      Qeuspnm = euspnm
     C                   eval      Qeusrat = eusrat
     C                   eval      Qeurtcd = eurtcd
     C                   eval      Qeuas3  = euas3
     C                   eval      Qeurgn# = eurgn#
     C                   eval      Qeurgds = eurgds
     C                   eval      Qeunwrw = eunwrw
     C                   eval      Qeuinex = euinex
     C                   eval      Qeuunex = euunex
     C                   eval      Qeumwex = eumwex
     C                   eval      Qeust   = eust
     C                   eval      Qeurcym = eurcym
      *** beg add ***** 05/21/08 *******************************************************
     C                   eval      QEUITER = EUITER
     C                   eval      QEUUNTR = EUUNTR
     C                   eval      QEUMDTR = EUMDTR
     C                   eval      QEUAS4  = EUAS4
     C                   eval      QEUAS5  = EUAS5
     C                   eval      QEUIMPR = EUIMPR
     C                   eval      QEUUNMN = EUUNMN
     C                   eval      QEUYDWM = EUYDWM
     C                   eval      QEUYDEM = EUYDEM
     C                   eval      QEUMDWM = EUMDWM
     C                   eval      QEUMDEM = EUMDEM
     C                   eval      QEUPMUM = EUPMUM
     C                   eval      QEUPYWM = EUPYWM
     C                   eval      QEUPYEM = EUPYEM
     C                   eval      QEUPYUM = EUPYUM
     C                   eval      QEUIVLC = EUIVLC
     C                   eval      QEUUVLC = EUUVLC
     C                   eval      QEUMVLC = EUMVLC
     C                   eval      QEUSEQ# = EUSEQ#
     C                   eval      QEUYDWT = EUYDWT
     C                   eval      QEUYDET = EUYDET
     C                   eval      QEUMDET = EUMDET
     C                   eval      QEUPMUT = EUPMUT
     C                   eval      QEUPYWT = EUPYWT
     C                   eval      QEUPYET = EUPYET
     C                   eval      QEUPYUT = EUPYUT
     C                   eval      QEUYDWD = EUYDWD
     C                   eval      QEUYDED = EUYDED
     C                   eval      QEUMDWD = EUMDWD
     C                   eval      QEUMDED = EUMDED
     C                   eval      QEUPMUD = EUPMUD
     C                   eval      QEUPYWD = EUPYWD
     C                   eval      QEUPYED = EUPYED
     C                   eval      QEUPYUD = EUPYUD
     C                   eval      QEUIDTR = EUIDTR
     C                   eval      QEUUDTR = EUUDTR
      *** beg add ***** 05/21/08 *******************************************************
      *** beg add ***** 01/23/09 *******************************************************
     C                   eval      QEUAS6  = EUAS6
     C                   eval      QEUAS7  = EUAS7
      *** end add ***** 01/23/09 *******************************************************
      *** beg add ***** 02/12/09 *******************************************************
     C                   eval      QEUMKRG = EUMKRG
      *** end add ***** 02/12/09 *******************************************************
     C*** beg add ***** 05/12/09 ******************************************
     C                   eval      QEUCAR# = EUCAR#
     C*** end add ***** 05/12/09 ******************************************
      *
     C                   Write     Qeu01
      *
     C     euky          Reade     peu01                                  99
     C                   Enddo
      *
     C                   Eval      *Inlr = *On
      *
      /eject
      *****************************************************************
      *  INITIALIZATION                ** SUBROUTINE **   S R 1 S T   *
      *****************************************************************
      *
     C     *Inzsr        Begsr
      *
     C     *Entry        Plist
     C                   Parm                    @@co#             3
     C                   Parm                    @@todt            8
      *
     C     euky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxrcym
      *
     C                   move      @@co#         xxco#
      *
      *  Calculate the reporting CYM
      *
     C     *usa0         move      @@todt        datiso
     C                   move      datiso        xxcymd
     C                   eval      xxrcym = xxcym - 190000
      *
     C                   Endsr
