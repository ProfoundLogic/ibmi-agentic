    H*  (C) Copyright Progressive Data Solutions, Inc., 2002
    H*********************************************************************
    H/TITLE FIX91WIP - Populate new percentage field in WTBSP
    H*********************************************************************
    H* CALLED FROM:                                                      *
    H*   COMMAND LINE                                                    *
    H* CALLS TO:                                                         *
    H*   NONE                                                            *
    H*                                                                   *
    H* CREATED:  11/19/02     C:136    PROJECT: 91WIP                    *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
     H* xx/xx/xx C:xxx xxxxxxx xxxxx xxxxx                                *
    H*********************************************************************

     Fwtbsp     uf   e             disk
     Dpsds           esds                  extname(szq1p)
     Dsys_date         s               d   datfmt(*usa)

     C     *usa          move      *date         sys_date
     C                   read      pbs01                                  60
     C                   dow       not *in60

     C                   if           BSFREQ = ' '

     C                   eval      BSFREQ = 'M'
     C     *cymd         move      sys_date      BSDT
     C                   time                    BSTIME
     C                   eval      BSUSER = Q1USER
     C                   eval      BSPGM  = Q1PGM

     C                   update    pbs01

     C                   endif

     C                   read      pbs01                                  60

     C                   enddo
     C*
     C                   eval      *inlr = *on
