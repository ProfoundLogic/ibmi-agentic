      * (C) Copyright Fiserv Insurance Solutions, Inc., 2008                                       *
      *============================================================================================*
      /title W4901RB - API for CEU Summary Flag
     H option(*nodebugio: *srcstmt)                                                                *
     H debug(*yes)                                                                                 *

     fswcpf     if   e           k disk

     d*entry
     d W4901RB         pr
     d  @@flag                        1
     d W4901RB         pi
     d  @@flag                        1

     c                   read      swcpf
     c                   eval      @@flag = WCFL37
     c                   eval      *inlr = *on
