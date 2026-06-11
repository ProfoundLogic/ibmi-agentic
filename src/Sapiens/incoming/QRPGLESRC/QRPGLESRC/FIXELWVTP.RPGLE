    H*  (C) Copyright Progressive Data Solutions, Inc., 2005
    H*********************************************************************
    H/TITLE FIXELWVTP - Assign Waiver Type in WDELP
    H*********************************************************************
    H* CALLED FROM:                                                      *
    H*   COMMAND LINE                                                    *
    H* CALLS TO:                                                         *
    H*   NONE                                                            *
    H*                                                                   *
    H* CREATED:  07/15/05     C:136    PROJECT: 133WBS                   *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
     H* xx/xx/xx C:xxx xxxxxxx xxxxx xxxxx                                *
    H*********************************************************************
     Fwdelp     uf   e             disk
     Fwmwvl     if   e           k disk

     c     wvky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elfyr
     c                   kfld                    elemp#
     c                   kfld                    eldiv

     C                   read      pel01
     C                   dow       not %eof

     c                   if        elsfpc <> .01
     C                   eval      elwvtp = 'B'
     c                   else

     C     wvky          chain     pwv01
     C                   if        %found
     C                   eval      elwvtp = 'S'
     c                   else
     C                   eval      elwvtp = 'B'
     C                   endif

     C                   endif

     C                   update    pel01

     C                   read      pel01
     C                   enddo

     C                   eval      *inlr = *on
