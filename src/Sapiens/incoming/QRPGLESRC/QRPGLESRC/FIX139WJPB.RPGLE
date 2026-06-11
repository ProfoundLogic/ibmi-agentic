      * (C) Copyright Fiserv Insurance Solutions, Inc., 2008                                       *
      *============================================================================================*
      /title FIX139WJPB - Call W8023C for each day a deductible payment was rec'd                  *
     H option(*nodebugio: *srcstmt)                                                                *
     H debug(*yes)                                                                                 *
      *============================================================================================*
      *  This program runs through WMEAP and calls W8023C (Load Deductible Payment Summary)        *
      *  for each date a deductible payment was made.                                              *
      *============================================================================================*
     fwmealt    if   e           k disk                                         co/arty/trdt
     fwmupl     if   e           k disk                                         co/user
     fwtabl     if   e           k disk                                         co/key

     d psds          esds                  extname(szq1p)
     d kkarty          s                   like(EAARTY) inz('Q')
     d kkkey           s                   like(TTKEY)

     c     upky          klist
     c                   kfld                    Q1USER

     c     eaky          klist
     c                   kfld                    UPCO#
     c                   kfld                    kkarty

     c     eaky1         klist
     c                   kfld                    UPCO#
     c                   kfld                    kkarty
     c                   kfld                    EATRDT

     c     ttky          klist
     c                   kfld                    UPCO#
     c                   kfld                    kkkey

     c     upky          chain     pup01
     c                   if        not(%found)
     c                   seton                                        lr
     c                   return
     c                   endif

     c     eaky          setll     wmealt
     c     eaky          reade     wmealt
     c                   dow       not(%eof)

     c                   eval      kkkey = EAARCD
     c     ttky          chain     ptt01
     c                   if        %found and
     c                             TTCTYP = 'R'
     c                   move      UPCO#         QQCO#             3
     c                   move      EATRDT        QQDATE            7
     c                   call      'W8023C'
     c                   parm                    QQCO#
     c                   parm                    QQDATE
     c     eaky1         setgt     wmealt
     c                   endif

     c     eaky          reade     wmealt
     c                   enddo

     c                   seton                                        lr
