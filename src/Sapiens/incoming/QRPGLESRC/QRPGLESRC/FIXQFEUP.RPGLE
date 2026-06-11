    h*  (C) Copyright Progressive Data Solutions, Inc., 2004
    h*********************************************************************
    h/TITLE FIXQFEUP - Populate sequence number for each year/month
    h*********************************************************************
    h* CALLED FROM:                                                      *
    h*   COMMAND LINE                                                    *
    h* CALLS TO:                                                         *
    h*   NONE                                                            *
    h*                                                                   *
    h* CREATED:  11/23/04     C:136    SUPPORT: WA129 44496              *
    h*********************************************************************
    h* REVISIONS:                                                        *
    h*                                                                   *
     h* xx/xx/xx C:xxx xxxxxxx xxxxx xxxxx                                *
    h*********************************************************************
     fQFEUL     uf   e           k disk
     d count           s             12  0 inz(0)
     d svrcym          s                   like(EURCYM)

     c                   read      peu01
     c                   dow       not(%eof)

     c                   if        EURCYM <> svrcym
     c                   eval      svrcym = EURCYM
     c                   z-add     0             count
     c                   endif

     c                   add       1             count
     c                   eval      EUSEQ# = count
     c                   update    peu01

     c                   read      peu01
     c                   enddo

     c                   eval      *inlr = *on
