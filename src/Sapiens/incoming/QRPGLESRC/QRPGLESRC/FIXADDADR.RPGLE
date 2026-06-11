      * (c) 2009 StoneRiver, Inc. All rights Reserved.                     *
      * StoneRiver is a trademark of StoneRiver, Inc.                      *
      *============================================================================================*
      /title FIXADDADR- Fix program to set type 'D' for dependent records and populate EFKEY       *
     H option(*nodebugio: *srcstmt)                                                                *
     H debug(*yes)                                                                                 *
      *============================================================================================*
     fwmefp     uf   e             disk

     dpsds           esds                  extname(szq1p)
     dwrkdate          s               d
     dcymddate         s              7  0
     dsvtype           s                   like(eftype)
     dsvkey            s                   like(efkey)

     c     *mdy          move      q1dt          wrkdate
     c     *cymd         move      wrkdate       cymddate
     c
     c                   read      pef01
     c                   dow       not(%eof)
      *
     c                   eval      svtype = eftype
     c                   eval      svkey  = efkey
      *
     c                   if        eftype = 'C' and efseq# > 0
     c                   eval      eftype = 'D'
     c                   endif                                                  efseq# > 0
      *
     c                   select
     c                   when      eftype = 'A'
     C                   movel(p)  efagt#        efkey
      *
     c                   when      eftype = 'C'
     C                   movel(p)  effyr         xxwk10           10
     C                   move      efcase        xxwk10
     C                   movel(p)  xxwk10        efkey
      *
     c                   when      eftype = 'D'
     C                   movel(p)  effyr         xxwk10
     C                   move      efcase        xxwk10
     C                   movel(p)  xxwk10        xxwk15           15
     C                   move      efseq#        xxwk15
     C                   movel(p)  xxwk15        efkey
      *
     c                   when      eftype = 'E'
     C                   movel(p)  efemp#        xxwk14           14
     C                   move      efdiv         xxwk14
     C                   movel(p)  xxwk14        efkey
      *
     c                   when      eftype = 'P'
     C                   movel(p)  effein        xxwk12           12
     C                   move      efmod         xxwk12
     C                   movel(p)  xxwk12        efkey
      *
     c                   endsl
      *
     c                   if        efkey <> svkey or eftype <> svtype
     c                   eval      efuser = q1user
     c                   eval      efpgm  = q1pgm
     c                   eval      eftime = q1time
     c                   eval      efdt   = cymddate
     c                   update    pef01
     c                   endif                                                  changed
      *
     c                   read      pef01
     c                   enddo
     c*
     c                   seton                                        lr
