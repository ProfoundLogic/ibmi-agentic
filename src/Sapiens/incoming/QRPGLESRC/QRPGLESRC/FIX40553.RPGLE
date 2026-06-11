    H*  (C) Copyright Progressive Data Solutions, Inc., 2004
    H*********************************************************************
    H/TITLE FIX40553 - Populate CMSMCD for Image
    H*********************************************************************
    H* CALLED FROM:                                                      *
    H*   COMMAND LINE                                                    *
    H* CALLS TO:                                                         *
    H*   NONE                                                            *
    H*                                                                   *
    H* CREATED:  02/02/04     C:136    SUPPORT: 40553                    *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
    H*********************************************************************
     Fsdcmp     uf   e             disk
     Fstctl1    if   e           k disk

     c     ctky          klist
     c                   kfld                    cmrmcd

     C                   read      pcm01
     C                   dow       not %eof

     C                   if        cmrmcd <> *blanks
     C     ctky          chain     pct01

     C                   if        %found and ctsmcd <> *blanks and
     C                             cmsmcd <> ctsmcd
     C                   eval      cmsmcd = ctsmcd
     C                   update    pcm01
     C                   endif                                                  %found

     C                   endif                                                  cmrmcd <> *blanks

     C                   read      pcm01

     C                   enddo
     C*
     C                   eval      *inlr = *on
