    h*  (C) Copyright Progressive Data Solutions, Inc., 2002
    h*********************************************************************
    h/TITLE FIXDBPRED - Populate prenotification date field in WMDBP
    h*********************************************************************
    h* CALLED FROM:                                                      *
    h*   COMMAND LINE                                                    *
    h* CALLS TO:                                                         *
    h*   NONE                                                            *
    h*                                                                   *
    h* CREATED:  06/11/03     C:136    PROJECT: WC1116                   *
    h*********************************************************************
    h* REVISIONS:                                                        *
    h*                                                                   *
     h* xx/xx/xx C:xxx xxxxxxx xxxxx xxxxx                                *
    h*********************************************************************

     fwmdbp     uf   e             disk
     fwmeml     if   e           k disk

     dpsds           esds                  extname(szq1p)
     dsys_date         s               d   datfmt(*usa)

     c     emky          klist
     c                   kfld                    dbco#
     c                   kfld                    dbfnd
     c                   kfld                    dbemp#
     c                   kfld                    dbdiv

     c     *usa          move      *date         sys_date
     c                   read      pdb01                                  60
     c                   dow       not *in60

     c                   if        DBPRED = 0

     c     emky          chain     pem01
     c                   eval      DBPRED = EMEFD1
     c     *cymd         move      sys_date      DBDT
     c                   time                    DBTIME
     c                   eval      DBUSER = Q1USER
     c                   eval      DBPGM  = Q1PGM

     c                   update    pdb01
     c                   endif

     c                   read      pdb01                                  60
     c                   enddo
     c*
     c                   eval      *inlr = *on
