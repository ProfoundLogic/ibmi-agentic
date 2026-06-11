    H*  (C) Copyright Progressive Data Solutions, Inc., 2004
    H*********************************************************************
    H/TITLE FIXACHDT - Populate DBEFDT from EMEFD1
    H*********************************************************************
    H* CALLED FROM:                                                      *
    H*   COMMAND LINE                                                    *
    H* CALLS TO:                                                         *
    H*   NONE                                                            *
    H*                                                                   *
    H* CREATED:  01/13/04     C:136    PROJECT: WC1116                   *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
    H*********************************************************************
     Fwmeml     if   e           k disk
     Fwmdbp     uf   e             disk
     Dpsds           esds                  extname(szq1p)
     Dsys_date         s               d   datfmt(*usa)

     c     dbky          klist
     c                   kfld                    dbco#
     c                   kfld                    dbfnd
     c                   kfld                    dbemp#
     c                   kfld                    dbdiv

     C     *usa          move      *date         sys_date
     C                   read      pdb01
     C                   dow       not %eof
     c                   eval      dbefdt = dbpred

     C     dbky          chain     pem01
     c                   eval      dbexdt = emcad1

     C     *cymd         move      sys_date      dbdt
     C                   time                    dbtime
     C                   eval      dbuser = q1user
     C                   eval      dbpgm  = q1pgm
     C                   update    pdb01

     C                   read      pdb01
     C                   enddo
     C*
     C                   eval      *inlr = *on
