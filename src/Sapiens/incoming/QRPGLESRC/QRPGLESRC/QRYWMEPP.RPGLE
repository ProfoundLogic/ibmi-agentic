     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')

      /title - Query for payrolls written by S/R Online

     h option(*nodebugio:*srcstmt)
     h debug(*yes)
     h dftactgrp(*no) actgrp('QRYWMEPP')
      *=========================================================================
     fwmepl     if   e             disk
     fqrywmepp  o    e             disk
     c                   read      pep01
     c                   dow       not(%eof)
     c                   if        EPIPGM = 'SRONLI'
     c                   write     pepqry
     c                   endif
     c                   read      pep01
     c                   enddo
     c                   seton                                        lr
