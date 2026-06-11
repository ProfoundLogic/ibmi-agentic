    H*****************************************************************
    H*  REVISIONS:                                                   *
    H*****************************************************************
     Fsxdtp     if   e             disk
     Fsdcmla    if   e           k disk
     D psds          esds                  extname(szq1p)
    
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)

     d kkkey           s             40
     d kkseq#          s              5  0
     d date            s              7  0
     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     cmky          klist
     C                   kfld                    kkkey
     C                   kfld                    kkseq#
    C*
     c     1             setll     pdt01
     C                   read      pdt01
     C                   dow       not %eof
     c*
     c                   movel     dttoky        test              1
     c                   if        test <> x'00'
     c                   eval      kkkey = dttoky
     c                   eval      kkseq# = dttos#
     c     cmky          chain     pcm01
     c                   if        (%found and cmidt < 1070601) or
     c                              not %found
     c*
     c                   eval      kkkey = dtfrky
     c                   eval      kkseq# = dtfrs#
     c     cmky          chain     pcm01
     c                   if        %found
     C                   call      'DX703R'
     C                   parm      DTFRKY        ##FKEY           40
     C                   parm      DTFRS#        ##FSEQ            5 0
     C                   parm      DTTOKY        ##TKEY           40
     C                   parm      DTTOS#        ##TSEQ            5 0
     C                   parm      'C'           ##CALL            1
     c                   else
     C                   call      'DX703R'
     C                   parm      DTFRKY        ##FKEY
     C                   parm      DTFRS#        ##FSEQ
     C                   parm      DTTOKY        ##TKEY
     C                   parm      DTTOS#        ##TSEQ
     C                   parm      ' '           ##CALL
     c                   endif
     c                   endif
     c                   endif
     c*
     C                   read      pdt01
     C                   Enddo
     c*
     C                   Eval      *Inlr = *On
