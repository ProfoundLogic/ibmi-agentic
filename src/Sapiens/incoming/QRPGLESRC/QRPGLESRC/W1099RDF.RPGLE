     Fwatnp     uf   e             disk    usropn
     F
     Dcmd              c                   const('OVRDBF FILE(WATNP) MBR(*ALL)')
     C
     C                   call      'QCMDEXC'
     C                   parm      cmd           cmdstr           30
     C                   parm      30            length           15 5
     C                   open      watnp
     C
     C                   read      watnp                                  99
     C                   dow       *in99 = *off
     C                   if        tnrtyp = 'F'
     C                   delete    ptn01
     C                   endif
     C                   read      watnp                                  99
     C                   enddo
     C
     C                   eval      *inlr = *on
