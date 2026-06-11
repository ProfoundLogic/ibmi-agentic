     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     h/title W1099RT - create "F" Record for IRSTAPE file
     H dftactgrp(*no) actgrp(*CALLER)
      ** =========================================================== **
      * REVISIONS:                                                    *
      *                                                               *
      ** =========================================================== **
     Fwatnpnec  o    e             disk    usropn
     Fwxtnp     if   e             disk    rename(ptn01 : ptn02)
     F
     Dcmdstr           s             36
     Dlength           s             15  5
     Dcount            s              8  0
     D
     Dcmd1             c                   const('CLRPFM FILE(WATNPNEC) +
     D                                     MBR(WATNPNEC)')
     Dcmd2             c                   const('OVRDBF FILE(WATNPNEC) +
     D                                     MBR(WATNPNEC)')
     D fData           ds           749
     D  aCount                        8S 0
     D  zeros                        21
     D  seqnbr               499    506                                         IRS Seq Nbr
     c*
     c     *entry        plist
     c                   parm                    qqccnt            8            Company Count
     c                   parm                    qqseq#            8            Seq # for F record
      *
     C
     C                   call      'QCMDEXC'
     C                   parm      cmd1          cmdstr
     C                   parm      36            length
     C
     C                   call      'QCMDEXC'
     C                   parm      cmd2          cmdstr
     C                   parm      36            length
     C
     C                   open      watnpnec
     C
     C                   read      wxtnp                                  99
     C                   dow       *in99 = *off
     C                   add       1             count
     C                   read      wxtnp                                  99
     C                   enddo
     C
     C                   move      count         aCount
     c                   if        qqccnt > *zeros
     c                   move      qqccnt        aCount
     c                   endif
     c                   move      qqseq#        seqnbr
      *
     C                   move      *all'0'       zeros
     C                   movel     'F'           tnrtyp
     C                   movel     fData         tndata
     C                   write     ptn01nec
     C
     C                   eval      *inlr = *on
