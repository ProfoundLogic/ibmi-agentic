     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
    H/TITLE FIXPPP - Populate rates into WDPPP from WDRRP
    H*********************************************************************
    H* CALLED FROM:                                                      *
    H*   COMMAND LINE                                                    *
    H* CALLS TO:                                                         *
    H*   NONE                                                            *
    H*                                                                   *
    H* CREATED:  07/06/10     C:190    PROJECT: WC1899                   *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
    H*********************************************************************
     Fwdrrl     if   e           k disk
     Fwdppp     uf   e             disk
     Dpsds           esds                  extname(szq1p)
     Dsys_date         s               d   datfmt(*usa)

     c     rrky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkst

     c     rrky1         klist
     c                   kfld                    kkco#             3 0
     c                   kfld                    kkfnd             3 0
     c                   kfld                    kkst              2
     c                   kfld                    kkefdt            7 0

     C     *usa          move      *date         sys_date
     C                   read      ppp01
     C                   dow       not %eof
     c                   eval      kkco#  = ppco#
     c                   eval      kkfnd  = ppfnd
     c                   eval      kkst   = ppst
     c                   eval      kkefdt = ppfrdt

     c     rrky1         setgt     prr01
     c     rrky          readpe    prr01
     c                   if        not %eof

     c                   if        ppa1rt = *zeros
     c                   eval      ppa1rt = rr1stp                              1ST ASSESSMENT %
     c                   endif
     c
     c                   if        ppa2rt = *zeros
     c                   eval      ppa2rt = rr2ndp                              2nd ASSESSMENT %
     c                   endif

     c                   if        ppa3rt = *zeros
     c                   eval      ppa3rt = rr3rdp                              3rd ASSESSMENT %
     c                   endif

     c                   if        ppa4rt = *zeros
     c                   eval      ppa4rt = rr4thp                              4th ASSESSMENT %
     c                   endif

     c                   if        ppa5rt = *zeros
     c                   eval      ppa5rt = rr5thp                              5th ASSESSMENT %
     c                   endif

     c                   if        ppa6rt = *zeros
     c                   eval      ppa6rt = rr6thp                              6th ASSESSMENT %
     c                   endif

     c                   if        ppa7rt = *zeros
     c                   eval      ppa7rt = rr7thp                              7th ASSESSMENT %
     c                   endif

     c                   if        ppdtrr = *zeros
     c                   eval      ppdtrr = rrdtrr                              Domestic terr rate
     c                   endif

     c                   if        ppterr = *zeros
     c                   eval      ppterr = rrterr                              Foreign terr rate
     c                   endif

     c                   endif                                                  not %eof

     C     *cymd         move      sys_date      ppdt
     C                   time                    pptime
     C                   eval      ppuser = q1user
     C                   eval      pppgm  = q1pgm
     C                   update    ppp01

     C                   read      ppp01
     C                   enddo
     C*
     C                   eval      *inlr = *on
