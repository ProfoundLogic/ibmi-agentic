     * (C) COPYRIGHT PROGRESSIVE DATA SOLUTIONS, INC., 2003
     *********************************************************************
     *TITLE FIXARCAT  - Populate new A/R category field
     *********************************************************************
     *  CALLED FROM:                                                     *
     *    COMMAND LINE                                                   *
     *  CALLS TO:                                                        *
     *                                                                   *
     *  CREATED: 11/04/03  PROGRAMMER: C:136  PROJ: WC1161               *
     *********************************************************************
     * REVISIONS:                                                        *
     *                                                                   *
     *********************************************************************
     Fwmeap     uf   e             disk
     Fwdeap     uf   e             disk    rename(pea01:pea02)
     Fwmxap     uf   e             disk
     Fwtabl     if   e           k disk
     *
     D psds          esds                  extname(szq1p)
     D sys_date        s               d   datfmt(*usa)
     D key             s                   like(ttkey)
     D co#             s                   like(ttco#)
     *
     C     tbky          klist
     C                   kfld                    co#
     C                   kfld                    key
     *
     **********************************************************************
     *
     C     *usa          move      *date         sys_date
     *
     *  update category field in wmeap
     *
     C                   read      wmeap
     C                   dow       not %eof
     *
     C                   eval      co# = eaco#
     C                   eval      key = eaarcd
     C     tbky          chain     ptt01
     *
     C                   if        %found
     C                   eval      eactyp = ttctyp
     C                   update    pea01
     C                   endif
     *
     C                   read      wmeap
     C                   enddo
     *
     *  update category in wdeap
     *
     C                   read      wdeap
     C                   dow       not %eof
     *
     C                   eval      co# = eaco#
     C                   eval      key = eaarcd
     C     tbky          chain     ptt01
     *
     C                   if        %found
     C                   eval      eactyp = ttctyp
     C                   update    pea02
     C                   endif
     *
     C                   read      wdeap
     C                   enddo
     *
     *  update category in wmxap
     *
     C                   read      wmxap
     C                   dow       not %eof
     *
     C                   eval      co# = xaco#
     C                   eval      key = xaarcd
     C     tbky          chain     ptt01
     *
     C                   if        %found
     C                   eval      xactyp = ttctyp
     C                   update    pxa01
     C                   endif
     *
     C                   read      wmxap
     C                   enddo
     *
    C                   seton                                        lr
