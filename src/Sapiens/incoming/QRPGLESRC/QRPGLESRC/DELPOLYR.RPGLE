      * (C) Copyright Fiserv Insurance Solutions, Inc., 2008                                       *
      *============================================================================================*
      /title DELPOLYR - Delete policy year info                                                    *
     H option(*nodebugio: *srcstmt)                                                                *
     H debug(*yes)                                                                                 *
      *============================================================================================*
      * Populate driver file DELPOLYR by pulling ELCO#/ELFND/ELEMP#/ELDIV/ELFYR from WDELP via SQL.
      * Then call W0051R01 for each record.
      *============================================================================================*
     fDELPOLYR  if   e             disk
     fWDELL     uf   e           k disk    rename(pel01 : pel01l)

     c     dltky         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR

     c                   read      delpolyr
     c                   dow       not(%EOF)
     c                   call      'W0051R01'
     c                   parm                    ELCO#
     c                   parm                    ELFND
     c                   parm                    ELEMP#
     c                   parm                    ELDIV
     c                   parm                    ELFYR
     c                   parm      *blank        QQQTE             1
     c     dltky         delete    pel01l
     c                   read      delpolyr
     c                   enddo

     c                   seton                                        lr
