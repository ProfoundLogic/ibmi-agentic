     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')

      /title - Fix pgm for SRTODT vs SRRPMY fields written by SRONLI

     h option(*nodebugio:*srcstmt)
     h debug(*yes)
     h dftactgrp(*no) actgrp('FIXWASRP')

      * Program corrects dates in WASRP written by SRONLI. Monthly reporting
      * policies should populate SRRPMY, not SRTODT, and vice versa for any
      * other billing frequency (BSFREQ <> 'M' or *blank). Records are saved
      * to a member of FIXWASRP before updating.

      *=========================================================================
     fwdell1    if   e           k disk
     fwtabl     if   e           k disk
     fwasrl4e   uf   e           k disk
     ffixwasrp  o    e             disk    rename(psr01:psrfix)

     d kkkey           s             10
     d mthlyBill       s               n


     c     bsky          klist
     c                   kfld                    ELCO#
     c                   kfld                    kkkey

     c     srky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELFYR
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV

     c                   read      pel01
     c                   dow       not(%eof)

     c                   movel(p)  ELBSCD        kkkey
     c     bsky          chain     pbs01
     c                   if        not(%found) or
     c                             BSONLI <> 'Y'
     c                   read      PEL01
     c                   iter
     c                   endif

     c                   if        BSFREQ = 'M' or BSFREQ = *blank
     c                   eval      mthlyBill = *on
     c                   else
     c                   eval      mthlyBill = *off
     c                   endif

     c     srky          setll     psr01
     c     srky          reade     psr01
     c                   dow       not(%eof)

     c                   if        SRIPGM = 'SRONLI' and
     c                             SRTODT <> 0 and SRRPMY <> 0

     c                   if        mthlyBill
     c                   write     psrfix
     c                   eval      SRTODT = 0
     c                   update    psr01
     c                   else
     c                   write     psrfix
     c                   eval      SRRPMY = 0
     c                   update    psr01
     c                   endif

     c                   endif

     c     srky          reade     psr01
     c                   enddo

     c                   read      PEL01
     c                   enddo

     c                   seton                                        lr
