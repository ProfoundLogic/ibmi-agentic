     Fwmcml1    if   e           k disk
     f*
     d xxco#           s                   like(cmco#)
     d xxfnd           s                   like(cmfnd)
     d xxfyr           s                   like(cmfyr)
     d xxcase          s                   like(cmcase)
     d*
     c                   read      pcm01
     c                   if        not %eof
     c                   dou       %eof
     c                   eval      xxco#=cmco#
     c                   eval      xxfnd=cmfnd
     c                   eval      xxfyr=cmfyr
     c                   eval      xxcase=cmcase
     c                   read      pcm01
     c                   enddo
     c                   endif
     c*
     c                   seton                                        lr
