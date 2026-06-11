    H*  (C) Copyright Progressive Data Solutions, Inc., 2004
    H*********************************************************************
    H/TITLE FIXELVDR - Populate Vendor History file from WDELP
    H*********************************************************************
    H* CALLED FROM:                                                      *
    H*   COMMAND LINE                                                    *
    H* CALLS TO:                                                         *
    H*   NONE                                                            *
    H*                                                                   *
    H* CREATED:  01/09/04     C:136    PROJECT: WC1115                   *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
     H* xx/xx/xx C:xxx xxxxxxx xxxxx xxxxx                                *
    H*********************************************************************
     Fwdelp     if   e             disk
     Fwdvhp     o    e             disk
     Fwmeml     if   e           k disk
     Dpsds           esds                  extname(szq1p)
     Dsys_date         s               d   datfmt(*usa)

     c     emky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv


     C     *usa          move      *date         sys_date
     C                   read      pel01
     C                   dow       not %eof

     C     emky          chain     pem01
     c                   eval      vhexdt = emcad1

     c                   eval      vhco#  = elco#
     c                   eval      vhgrp  = elfnd
     c                   eval      vhemp# = elemp#
     c                   eval      vhdiv  = eldiv
     c                   eval      vhfyr  = elfyr
     c                   eval      vhvdr# = elfnd
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

     C                   read      pel01
     C                   enddo
     C*
     C                   eval      *inlr = *on
