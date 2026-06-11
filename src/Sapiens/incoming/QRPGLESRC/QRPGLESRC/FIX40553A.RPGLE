    H*  (C) Copyright Progressive Data Solutions, Inc., 2004
    H*********************************************************************
    H/TITLE FIX40553 - Populate CMSMCD for Image
    H*********************************************************************
    H* CALLED FROM:                                                      *
    H*   COMMAND LINE                                                    *
    H* CALLS TO:                                                         *
    H*   NONE                                                            *
    H*                                                                   *
    H* CREATED:  02/02/04     C:136    SUPPORT: 40553A                   *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
    H*********************************************************************
     Fsdcmpa    if   e             disk    prefix(a_) rename(pcm01:pcm01a)
     Fsdcmle    uf   e           k disk
     Fstctl1    if   e           k disk

     c     ctky          klist
     c                   kfld                    a_cmrmcd

     c     cmky          klist
     c                   kfld                    a_cmkey
     c                   kfld                    a_cmseq#

     C                   read      pcm01a
     C                   dow       not %eof

     C                   if        a_cmrmcd <> *blanks
     C     ctky          chain     pct01

     C                   if        %found and ctsmcd <> *blanks and
     C                             a_cmsmcd <> ctsmcd
     C     cmky          chain     pcm01
     c                   if        %found
     C                   eval      cmuser = a_cmuser
     C                   eval      cmpgm  = a_cmpgm
     C                   eval      cmdt   = a_cmdt
     C                   eval      cmtime = a_cmtime
     C                   update    pcm01
     C                   endif                                                  %found

     C                   endif                                                  %found

     C                   endif                                                  cmrmcd <> *blanks

     C                   read      pcm01a

     C                   enddo
     C*
     C                   eval      *inlr = *on
