     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      *
      *****************************************************************
      * CREATED 05/14/04   C:15  PROJECT: WC1100                      *
      *   CREATE FILE QFE2P1                                          *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 01/22/13 C:287 Seq#: SY266_OVR                                *
      *                   Added Activation group compile options      *
      *                                                               *
      * 12/01/10 C: 235   PROJECT: 133UKG                             *
      *                   Added new field E2ACD2(2nd line Accident    *
      *                   description) in QFE2P.                      *
      *                                                               *
      * 05/12/09 C:136 Project: 135UMB                                                             *
      *                Add Carrier # to outfile                                                    *
      *                                                                                            *
      * 08/03/03 C: 132   PROJECT: WC970                              *
      *                   Compile ONLY to add new fields in QFEUP.    *
      *                                                               *
      *****************************************************************
      *                                                               *
     FQfe2l     IF   E           k Disk
     FQfe2p1    O  A E             Disk    Rename(Pe201:Qe201) Prefix(qe2:2)
     *
     D Datiso          S               D   DATFMT(*iso)
     D xxco#           S                   like(e2co#)
     D xxrcym          S                   like(e2rcym)
     D                 ds
     D xxcymd                         8  0
     D   xxcym                        6  0 Overlay(xxcymd:1)
     D   xxdd                         2  0 Overlay(xxcymd:7) Inz(01)
      *
     D PSDS          ESDS                  Extname(Szq1p)
      *-------------------------------------------------------------------------
      *  Read work file input
      *
     C     e2ky          chain     pe201                              99
     C                   Dow       *in99 = *off
      *
     C                   clear                   Qe201
      *
      *  Convert all dates to *USA.
      *
     C     *usa0         move      @@todt        qe2asof
      *
     C                   if        e2acdt <> 0
     C     *usa          move      e2acdt        qe2acdt
     C                   Else
     C     *usa          move      '01/01/1901'  qe2acdt
     C                   Endif
     C                   if        e2cldt <> 0
     C     *usa          move      e2cldt        qe2cldt
     C                   Else
     C     *usa          move      '01/01/1901'  qe2cldt
     C                   Endif
     C                   if        e2pfdt <> 0
     C     *usa          move      e2pfdt        qe2pfdt
     C                   Else
     C     *usa          move      '01/01/1901'  qe2pfdt
     C                   Endif
     C                   if        e2ptdt <> 0
     C     *usa          move      e2ptdt        qe2ptdt
     C                   Else
     C     *usa          move      '01/01/1901'  qe2ptdt
     C                   Endif
     C                   if        e2cxdt <> 0
     C     *usa          move      e2cxdt        qe2cxdt
     C                   Else
     C     *usa          move      '01/01/1901'  qe2cxdt
     C                   Endif
     C                   if        e2rcvd <> 0
     C     *usa          move      e2rcvd        qe2rcvd
     C                   Else
     C     *usa          move      '01/01/1901'  qe2rcvd
     C                   Endif
      *
      *  Load remaining fields.
      *
     C                   eval      Qe2co#  = e2co#
     C                   eval      Qe2cnam = e2cnam
     C                   eval      Qe2fnd  = e2fnd
     C                   eval      Qe2fnam = e2fnam
     C                   eval      Qe2emp# = e2emp#
     C                   eval      Qe2div  = e2div
     C                   eval      Qe2emnm = e2emnm
     C                   eval      Qe2est  = e2est
     C                   eval      Qe2ezip = e2ezip
     C                   eval      Qe2agt# = e2agt#
     C                   eval      Qe2anam = e2anam
     C                   eval      Qe2cnty = e2cnty
     C                   eval      Qe2cnds = e2cnds
     C                   eval      Qe2rgn# = e2rgn#
     C                   eval      Qe2fyr  = e2fyr
     C                   eval      Qe2opcl = e2opcl
     C                   eval      Qe2lstm = e2lstm
     C                   eval      Qe2case = e2case
     C                   eval      Qe2clmn = e2clmn
     C                   eval      Qe2acds = e2acds
     C                   eval      Qe2clas = e2clas
     C                   eval      Qe2clds = e2clds
     C                   eval      Qe2ss#  = e2ss#
     C                   eval      Qe2natr = e2natr
     C                   eval      Qe2nads = e2nads
     C                   eval      Qe2part = e2part
     C                   eval      Qe2pads = e2pads
     C                   eval      Qe2caus = e2caus
     C                   eval      Qe2cads = e2cads
     C                   eval      Qe2acmp = e2acmp
     C                   eval      Qe2amed = e2amed
     C                   eval      Qe2aleg = e2aleg
     C                   eval      Qe2aoth = e2aoth
     C                   eval      Qe2pcmp = e2pcmp
     C                   eval      Qe2pmed = e2pmed
     C                   eval      Qe2pleg = e2pleg
     C                   eval      Qe2poth = e2poth
     C                   eval      Qe2rcmp = e2rcmp
     C                   eval      Qe2rmed = e2rmed
     C                   eval      Qe2rleg = e2rleg
     C                   eval      Qe2roth = e2roth
     C                   eval      Qe2rcmp = e2rcmp
     C                   eval      Qe2rmed = e2rmed
     C                   eval      Qe2rleg = e2rleg
     C                   eval      Qe2roth = e2roth
     C                   eval      Qe2tpip = e2tpip
     C                   eval      Qe2tptd = e2tptd
     C                   eval      Qe2tres = e2tres
     C                   eval      Qe2tinc = e2tinc
     C                   eval      Qe2zip  = e2zip
     C                   eval      Qe2ctg1 = e2ctg1
     C                   eval      Qe2prt  = e2prt
     C                   eval      Qe2dcmp = e2dcmp
     C                   eval      Qe2dmed = e2dmed
     C                   eval      Qe2dleg = e2dleg
     C                   eval      Qe2doth = e2doth
     C                   eval      Qe2ctg2 = e2ctg2
     C                   eval      Qe2c2ds = e2c2ds
     C                   eval      Qe2dept = e2dept
     C                   eval      Qe2dpds = e2dpds
     C                   eval      Qe2chap = e2chap
     C                   eval      Qe2chds = e2chds
     C                   eval      Qe2sex  = e2sex
     C                   eval      Qe2age  = e2age
     C                   eval      Qe2rst  = e2rst
     C                   eval      Qe2jrst = e2jrst
     C                   eval      Qe2crsv = e2crsv
     C                   eval      Qe2mrsv = e2mrsv
     C                   eval      Qe2lrsv = e2lrsv
     C                   eval      Qe2orsv = e2orsv
     C                   eval      Qe2cids = e2cids
     C                   eval      Qe2prem = e2prem
     C                   eval      Qe2cmp  = e2cmp
     C                   eval      Qe2agfe = e2agfe
     C                   eval      Qe2earn = e2earn
     C                   eval      Qe2nwrw = e2nwrw
     C                   eval      Qe2clm# = e2clm#
     C                   eval      Qe2rcym = e2rcym
     C*** beg add ***** 05/12/09 ******************************************
     C                   eval      QE2CAR# = E2CAR#
     C*** BEG ADD ***** 12/01/10 ******************************************
     C                   eval      QE2ACD2 = E2ACD2
     C*** END ADD ***** 12/01/10 ******************************************
     C*** end add ***** 05/12/09 ******************************************
      *
     C                   Write     Qe201
      *
     C     e2ky          Reade     pe201                                  99
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
     C     e2ky          klist
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
