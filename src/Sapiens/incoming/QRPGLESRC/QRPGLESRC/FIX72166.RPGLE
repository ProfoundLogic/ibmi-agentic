     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*
     H**************************************************************************************
     H*  Description: This program reads
     H*               FAEOP and if EOBLST=C then read through each detail line FDCAP and   *
     H*               set CAFLAG=' '
     H*                                                                                    *
     H*  Created: 05/10/10   Programmer C:220   Support:72166                              *
     H**************************************************************************************
     H* Revisions:                                                                         *
     H*                                                                                    *
     H**************************************************************************************
     FFAEOP     if   e             DISK
     F* fdcap by CO#/SUB#/FND/FYR/CASE/EOB#/RCN/DSEQ
     Ffdcal19   uf   e           k disk
     F**************************************************************************************
     D*  Date/time data structure
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)


     DPSDS           ESDS                  Extname(SZQ1P)

     C*  Parm lists
     c     DTCYMD        PLIST
     c                   PARM                    SRDTI             8 0
     c                   PARM      *blanks       SRFFMT            8
     c                   PARM      '*CYMD'       SRTFMT            8
     c                   PARM                    SRDTO            10
     c                   PARM                    SRERR             1
     C*  Key lists
     C
     C*   agent history key list
     C*
     C     CAKY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*                  KFLD                    XXLIN#
     C*
     C*====================================================================
     C*  mainloop - process all policies
     C*====================================================================
     C*
     C                   TIME                    dsdttm
     C*
     C*  Process all recods in FAEOP
     C*
     C                   read      peo01                                  70
     C*
     C                   dow       *in70 = *off
     C*
     C                   if        EOBLST = 'C'
     C*
     C*  Process all line items
     C*
     C     caky1         chain     pca01                              72
     C*
     C                   dow       *in72 = *off
     C*
     C                   eval      caflag = *blank
     C*
     C                   eval      capgm= 'FIX72166'
     C                   eval      causer= Q1USER
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    DTCYMD
     C                   movel     srdto         cadt
     C*
     C                   update    pca01
     C*
     C     caky1         reade     pca01                                  72
     C*
     C                   enddo
     C                   endif
     C*
     C                   read      peo01                                  70
     C*
     C                   enddo
     C*
     C                   eval      *inlr = *on
     C*
