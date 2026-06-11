    H*  (C) Copyright Progressive Data Solutions, Inc., 2005
    H*********************************************************************
    H/TITLE FIXWDVHP - WRITE VENDOR HISTORY FROM WDELP
    H*********************************************************************
    H*                                                                   *
    H* DESCRIPTION: PROGRAM WAS WRITTEN TO WRITE A VENDOR HISTORY        *
    H*              RECORD FOR EVERY EMPLOYER DETAIL RECORD USING        *
    H*              THE REGION NUMBER AS THE VENDOR NUMBER. THIS         *
    H*              PROGRAM WAS WRITTEN SPECIFICALLY FOR PROVIDENCE.     *
    H*                                                                   *
    H* CALLED FROM:                                                      *
    H*   COMMAND LINE                                                    *
    H* CALLS TO:                                                         *
    H*   NONE                                                            *
    H*                                                                   *
    H* CREATED:  06/23/05     C:136    SUPPORT: WC1115                   *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
     H* xx/xx/xx C:xxx xxxxxxx xxxxx xxxxx                                *
    H*********************************************************************
     Fwdelp     if   e             disk
     Fwdvhl     if a e           k disk
     Fwmeml     if   e           k disk
     Dpsds           esds                  extname(szq1p)
     Dsys_date         s               d   datfmt(*usa)

     c     emky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv

     c     vhky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv
     c                   kfld                    elfyr


     C     *usa          move      *date         sys_date
     C                   read      pel01
     C                   dow       not %eof

     C     vhky          chain     pvh01
     c                   if        not(%found)

     C     emky          chain     pem01
     c                   eval      vhco#  = elco#
     c                   eval      vhgrp  = elfnd
     c                   eval      vhemp# = elemp#
     c                   eval      vhdiv  = eldiv
     c                   eval      vhfyr  = elfyr
     c                   eval      vhvdr# = emrgn#
     c                   eval      vhefdt = elpfdt
     c                   eval      vhstat = 'A'
     C     *cymd         move      sys_date      vhdt
     c                   eval      vhidt  = vhdt
     C                   time                    vhtime
     c                   eval      vhitim = vhtime
     C                   eval      vhuser = q1user
     C                   eval      vhiusr = q1user
     C                   eval      vhpgm  = q1pgm
     C                   eval      vhipgm = q1pgm
     C                   write     pvh01

     c                   endif

     C                   read      pel01

     C                   enddo
     C*
     C                   eval      *inlr = *on
