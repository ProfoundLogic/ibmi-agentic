    H*  (C) Copyright Progressive Data Solutions, Inc., 2005
    H*********************************************************************
    H/TITLE FIX91WIP - Populate EPARAT if EPARAT = 0
    H*********************************************************************
    H*                                                                   *
    H* DESCRIPTION:  This program was designed specifically for          *
    H*               PPC. They do not use anniversary rating dates       *
    H*               at this point in time. Logic to use anniversary     *
    H*               rating dates would have be added if used for        *
    H*               anyone else.                                        *
    H*                                                                   *
    H* CALLED FROM:                                                      *
    H*   COMMAND LINE                                                    *
    H* CALLS TO:                                                         *
    H*   NONE                                                            *
    H*                                                                   *
    H* CREATED:  07/01/05     C:136    SUPPORT: WA135 47884              *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
     H* xx/xx/xx C:xxx xxxxxxx xxxxx xxxxx                                *
    H*********************************************************************

     Fwmepp     uf   e             disk
     Fwdell     if   e           k disk
     Fwmprl1    if   e           k disk
     Dpsds           esds                  extname(szq1p)
     Dsys_date         s               d   datfmt(*usa)

     c     elky          klist
     c                   kfld                    EPCO#
     c                   kfld                    EPFND
     c                   kfld                    EPEMP#
     c                   kfld                    EPDIV
     c                   kfld                    EPFYR

     c     prky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELRTCD
     c                   kfld                    EPCLAS

     c     prky1         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELRTCD
     c                   kfld                    EPCLAS
     c                   kfld                    ELPFDT

     C     *usa          move      *date         sys_date
     C                   read      pep01
     C                   dow       not(%eof)

     c                   if        EPARAT = 0

     c     elky          chain     pel01
     c                   if        %found

     c     prky1         setgt     ppr01
     c     prky          readpe    ppr01
     c                   if        not(%eof)
     c                   eval      EPARAT = PRRT
     C     *cymd         move      sys_date      EPDT
     C                   time                    EPTIME
     C                   eval      EPUSER = Q1USER
     C                   eval      EPPGM  = Q1PGM
     c                   update    pep01
     c                   endif

     c                   endif

     c                   endif

     C                   read      pep01
     C                   enddo
     C*
     C                   eval      *inlr = *on
