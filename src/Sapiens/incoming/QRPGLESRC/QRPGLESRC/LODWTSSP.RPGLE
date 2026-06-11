     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      *=================================================================================
      * Load WTSSELP (State Process Select Program)                       *
     H*       !!!!!!!!  Add table entries to fv and fv1 !!!!!!!!!!        *
      *=================================================================================
     H* Revised:                                                          *
     H*                                                                   *
    H*********************************************************************
     fwtsselp   uf a e           k disk
     d*
     d size            c                   50
     d x               s              3  0
     d fv              s             94    dim(size) ctdata perrcd(1)
     D fv1             s             10    dim(size) ctdata perrcd(1)
     D fv2             s             30    dim(size) alt(fv1)
     d flds          e ds                  extname(WTSSELP)
     d sysDt           s               d
     d sysDt7          s              7  0
     d psds          esds                  extname(szq1p)
     d index                          2  0
     d aflgs           ds
     d  defInExFlg                    1
     d  conOnWMPEP                    1

     c                   move      *date         sysDt
     c     *cymd         move      sysDt         sysDt7
     c     1             do        size          x
     c                   if        fv(x)  <> *blanks
     c                   eval      flds   = fv(x)
     C     SSID          chain     WTSSELP
     c                   eval      SSPGM  = q1pgm
     c                   eval      SSUSER = q1user
     c                   eval      SSDT   = sysDt7
     c                   eval      index = %lookup(SSID : fv1)
     c                   if        index > 0
     c                   eval      SSDIEF = %subst(fv2(index) : 1 : 1)
     c                   eval      SSCOPE = %subst(fv2(index) : 2 : 1)
     c                   eval      SSCRRE = %subst(fv2(index) : 3 : 1)
     c                   eval      SSF17  = %subst(fv2(index) : 4 : 1)
     c                   eval      SSCAR# = %subst(fv2(index) : 5 : 1)
     c                   else
     c                   eval      SSDIEF = 'I'
     c                   endif
     c                   if        %found(WTSSELP)
     c                   update    pss01
     c                   else
     c                   write     pss01
     c                   endif
     c                   else
     c                   leave
     c                   endif
     c                   enddo
     c                   seton                                        lr
      *  1         2         3         4         5         6         7         8         9
123456*890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
** fv (field values)
F6300C    NCCI Medical Data Call                  F6300C    000                            001
** fv1 alt fv2  (additional field values (keyed to pgm ID)
F6300C    E
