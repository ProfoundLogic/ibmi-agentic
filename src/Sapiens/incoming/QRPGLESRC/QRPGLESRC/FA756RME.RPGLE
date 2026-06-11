     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE FA756RME
     H*****************************************************************
     H*  Description:  This program will create a DRG line item.      *
     H*                it is modelled on FA756R.                      *
     H*                                                               *
     H*  Created: 06/14/13   Programmer C:277   PROJECT: ME005        *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     Fftdgl1me  if   e           k disk
     Ffaeop     if   e             disk
     Ffaeul     if   e           k disk
     D psds          esds                  extname(szq1p)
     *
     D                 ds
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D@@car            s             10
     D @@drg           s                   like(dgdrg)
     D kkco#           s                   like(eoco#)
     D kksub#          s                   like(eosub#)
     D kkfnd           s                   like(eofnd)
     D kkfyr           s                   like(eofyr)
     D kkcase          s                   like(eocase)
     D kkeob#          s                   like(eoeob#)
     D kkefdt          s                   like(eudcdt)
     C*
     C     *entry        plist
     C                   parm                    @@rrn             9 0
     C                   parm                    @@rtcd            1
     C                   parm                    @@allow1          9 2
     C                   parm                    @@allow2          9 2
     *
     C     dgky          klist
     C                   kfld                    @@drg
     C                   kfld                    kkefdt
     *
     C     dgky1         klist
     C                   kfld                    @@drg
     *
     C     euky          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C*Set the pointer for the FAEOP from the RRN
     C*
     C                   Move      ' '           @@rtcd
     C                   z-add     0             @@allow1
     C                   z-add     0             @@allow2
     C     @@rrn         Chain     FAEOP                              99
     C                   if        *in99 = *on
     C                   Move      'F'           @@rtcd
     C                   seton                                        LR
     C                   return
     C                   endif                                                  *in99=*on
     C*
     C                   Z-add     eoco#         kkco#
     C                   Z-add     eosub#        kksub#
     C                   Z-add     eofnd         kkfnd
     C                   Z-add     eocase        kkcase
     C                   Z-add     eoeob#        kkeob#
     C                   Z-add     eofyr         kkfyr
     C*
     C*Set the pointer for the FAEUL from the values of FAEOP
     C*
     C     euky          Chain     faeul                              99
     C                   eval      kkefdt = eudcdt
     C                   eval      @@car = eumcar
     C                   eval      @@drg  = eudrg#
      *
      * Retrieve the Relative value
     C     dgky          setgt     pdg01
     C     dgky1         readpe(n) pdg01                                  99
     C                   if        *in99 = *off
     C                   Eval      @@allow1 = DGafee
     C                   Eval      @@allow2 = DGcfee
     C                   Endif
      *
     C                   seton                                        LR
     C                   return
