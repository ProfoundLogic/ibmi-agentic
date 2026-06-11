     hcopyright('(c) 2025 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h/TITLE W7353R7 - Financial Data Files Parse/Copy program                        *
     H dftactgrp(*no) actgrp(*caller)
     h*********************************************************************************
     h*  Description:  Transfer extraction data to WCSTAT file for tape transfer.     *
     h*                                                                               *
     h*  Called from: W7353C6                                                         *
     h*  Calls to:                                                                    *
     h*                                                                               *
     h*  Created: 11/23/10    Programmer C:190   Project:139WPQ                       *
     h*********************************************************************************
     h*  Revisions:                                                                   *
     H*                                                                               *
     H*  03/11/25  C:344 Support: 118509                                              *
     H*                  Stop the creation of the GIC (WCSTAG*) files.                *
     H*
     H*  01/22/13  C:287 Seq#: SY266_OVR          Edit                                *
     H*                  Added DFTACTGRP and ACTGRP compile options                   *
     h*                                                                               *
     h*********************************************************************************
     h*  Indicators:                                                                  *
     h*                                                                               *
     h*                                                                               *
     h*********************************************************************************
     f* Fund Work File
     fwcstatx   if   e           k disk
     fwcstatxg  if   e           k disk
     fwcstat01  o    e           k disk    prefix(a1)
     fwcstat02  o    e           k disk    prefix(a2)
     fwcstat03  o    e           k disk    prefix(a3)
     fwcstat04  o    e           k disk    prefix(a4)
     fwcstat05  o    e           k disk    prefix(a5)
     fwcstat06  o    e           k disk    prefix(a6)
     fwcstat07a o    e           k disk    prefix(a7a)
     fwcstat07b o    e           k disk    prefix(a7b)
     fwcstat07c o    e           k disk    prefix(a7c)
     fwcstat07d o    e           k disk    prefix(a7d)
     fwcstat07e o    e           k disk    prefix(a7e)
     fwcstat07f o    e           k disk    prefix(a7f)
     fwcstat07g o    e           k disk    prefix(a7g)
     fwcstat07h o    e           k disk    prefix(a7h)
     fwcstat07i o    e           k disk    prefix(a7i)
     fwcstat07j o    e           k disk    prefix(a7j)
     fwcstat09  o    e           k disk    prefix(a9)
      //**** Beg Del **** 03/11/25 *****************************************
     f*wcstag01  o    e           k disk    rename(pst01:pgt01) prefix(g1)
     f*wcstag02  o    e           k disk    rename(pst02:pgt02) prefix(g2)
     f*wcstag03  o    e           k disk    rename(pst03:pgt03) prefix(g3)
     f*wcstag04  o    e           k disk    rename(pst04:pgt04) prefix(g4)
     f*wcstag05  o    e           k disk    rename(pst05:pgt05) prefix(g5)
     f*wcstag06  o    e           k disk    rename(pst06:pgt06) prefix(g6)
     f*wcstag07a o    e           k disk    rename(pst7a:pgt7a) prefix(g7a)
     f*wcstag07b o    e           k disk    rename(pst7b:pgt7b) prefix(g7b)
     f*wcstag07c o    e           k disk    rename(pst7c:pgt7c) prefix(g7c)
     f*wcstag07d o    e           k disk    rename(pst7d:pgt7d) prefix(g7d)
     f*wcstag07e o    e           k disk    rename(pst7e:pgt7e) prefix(g7e)
     f*wcstag07f o    e           k disk    rename(pst7f:pgt7f) prefix(g7f)
     f*wcstag07g o    e           k disk    rename(pst7g:pgt7g) prefix(g7g)
     f*wcstag07h o    e           k disk    rename(pst7h:pgt7h) prefix(g7h)
     f*wcstag07i o    e           k disk    rename(pst7i:pgt7i) prefix(g7i)
     f*wcstag07j o    e           k disk    rename(pst7j:pgt7j) prefix(g7j)
     f*wcstag09  o    e           k disk    rename(pst09:pgt09) prefix(g9)
      //**** End Del **** 03/11/25 *****************************************

     d*----------------------------
     D                 ds
     d data                    1    250
     d rectyp                 41     41
     d rstyp                  42     42
      *
     D                 ds
     d datag                   1    265
     d rectypg                41     41
     d rstypg                 42     42
     d*----------------------------

     d a1data        e ds                  extname(wcstat01) prefix(a1)
     d a2data        e ds                  extname(wcstat02) prefix(a2)
     d a3data        e ds                  extname(wcstat03) prefix(a3)
     d a4data        e ds                  extname(wcstat04) prefix(a4)
     d a5data        e ds                  extname(wcstat05) prefix(a5)
     d a6data        e ds                  extname(wcstat06) prefix(a6)
     d a7adata       e ds                  extname(wcstat07a) prefix(a7a)
     d a7bdata       e ds                  extname(wcstat07b) prefix(a7b)
     d a7cdata       e ds                  extname(wcstat07c) prefix(a7c)
     d a7ddata       e ds                  extname(wcstat07d) prefix(a7d)
     d a7edata       e ds                  extname(wcstat07e) prefix(a7e)
     d a7fdata       e ds                  extname(wcstat07f) prefix(a7f)
     d a7gdata       e ds                  extname(wcstat07g) prefix(a7g)
     d a7hdata       e ds                  extname(wcstat07h) prefix(a7h)
     d a7idata       e ds                  extname(wcstat07i) prefix(a7i)
     d a7jdata       e ds                  extname(wcstat07j) prefix(a7j)
     d a9data        e ds                  extname(wcstat09) prefix(a9)
     d g1data        e ds                  extname(wcstag01) prefix(g1)
     d g2data        e ds                  extname(wcstag02) prefix(g2)
     d g3data        e ds                  extname(wcstag03) prefix(g3)
     d g4data        e ds                  extname(wcstag04) prefix(g4)
     d g5data        e ds                  extname(wcstag05) prefix(g5)
     d g6data        e ds                  extname(wcstag06) prefix(g6)
     d g7adata       e ds                  extname(wcstag07a) prefix(g7a)
     d g7bdata       e ds                  extname(wcstag07b) prefix(g7b)
     d g7cdata       e ds                  extname(wcstag07c) prefix(g7c)
     d g7ddata       e ds                  extname(wcstag07d) prefix(g7d)
     d g7edata       e ds                  extname(wcstag07e) prefix(g7e)
     d g7fdata       e ds                  extname(wcstag07f) prefix(g7f)
     d g7gdata       e ds                  extname(wcstag07g) prefix(g7g)
     d g7hdata       e ds                  extname(wcstag07h) prefix(g7h)
     d g7idata       e ds                  extname(wcstag07i) prefix(g7i)
     d g7jdata       e ds                  extname(wcstag07j) prefix(g7j)
     d g9data        e ds                  extname(wcstag09) prefix(g9)
     c*
     c                   read      rstat
     c                   dow       not(%eof)
     c                   movel(p)  rdata         data
     c                   exsr      Wr_wcstatx
     c                   read      rstat
     c                   enddo                                                  WCSTATX
     C*
     c                   read      rstatg
     c                   dow       not(%eof)
     c                   movel(p)  rdatag        datag
      //**** Beg Del **** 03/11/25 *****************************************
     c***                   exsr      Wr_wcstatxg
      //**** End Del **** 03/11/25 *****************************************
     c                   read      rstatg
     c                   enddo                                                  WCSTATXG

     c                   seton                                        lr
     c*
     C*==========================================================================
     C*  Wr_wcstatx - Subroutine to write WCSTATX
     C*==========================================================================
     c     Wr_wcstatx    begsr
      *
     c                   select

     c                   when      rectyp = '1'
     c                   movel(p)  rdata         a1data
     c                   write     pst01
      *
     c                   when      rectyp = '2'
     c                   movel(p)  rdata         a2data
     c                   write     pst02

     c                   when      rectyp = '3'
     c                   movel(p)  rdata         a3data
     c                   write     pst03
      *
     c                   when      rectyp = '4'
     c                   movel(p)  rdata         a4data
     c                   write     pst04

     c                   when      rectyp = '5'
     c                   movel(p)  rdata         a5data
     c                   write     pst05

     c                   when      rectyp = '6'
     c                   movel(p)  rdata         a6data
     c                   write     pst06

     c                   when      rectyp = '7'
     c                   select
     c                   when      rstyp = 'A'
     c                   movel(p)  rdata         a7adata
     c                   write     pst7a

     c                   when      rstyp = 'B'
     c                   movel(p)  rdata         a7bdata
     c                   write     pst7b

     c                   when      rstyp = 'C'
     c                   movel(p)  rdata         a7cdata
     c                   write     pst7c

     c                   when      rstyp = 'D'
     c                   movel(p)  rdata         a7ddata
     c                   write     pst7d

     c                   when      rstyp = 'E'
     c                   movel(p)  rdata         a7edata
     c                   write     pst7e

     c                   when      rstyp = 'F'
     c                   movel(p)  rdata         a7fdata
     c                   write     pst7f

     c                   when      rstyp = 'G'
     c                   movel(p)  rdata         a7gdata
     c                   write     pst7g

     c                   when      rstyp = 'H'
     c                   movel(p)  rdata         a7hdata
     c                   write     pst7h

     c                   when      rstyp = 'I'
     c                   movel(p)  rdata         a7idata
     c                   write     pst7i

     c                   when      rstyp = 'J'
     c                   movel(p)  rdata         a7jdata
     c                   write     pst7j
     c                   endsl

     c                   when      rectyp = '9'
     c                   move      rdata         a9data
     c                   write     pst09

     c                   endsl

     c                   endsr                                                  Wr_wcstatx

      //**** Beg Del **** 03/11/25 *****************************************
     C*==========================================================================
     C*  Wr_wcstatxg - Subroutine to write WCSTATXG
     C*==========================================================================
     c*    Wr_wcstatxg   begsr
      *
     c*                  select
      *
     c*                  when      rectypg = '1'
     c*                  move      rdatag        g1data
     c*                  write     pgt01
      *
     c*                  when      rectypg = '2'
     c*                  move      rdatag        g2data
     c*                  write     pgt02
      *
     c*                  when      rectypg = '3'
     c*                  move      rdatag        g3data
     c*                  write     pgt03
      *
     c*                  when      rectypg = '4'
     c*                  move      rdatag        g4data
     c*                  write     pgt04
      *
     c*                  when      rectypg = '5'
     c*                  move      rdatag        g5data
     c*                  write     pgt05
      *
     c*                  when      rectypg = '6'
     c*                  move      rdatag        g6data
     c*                  write     pgt06
      *
     c*                  when      rectypg = '7'
     c*                  select
     c*                  when      rstypg = 'A'
     c*                  move      rdatag        g7adata
     c*                  write     pst7a
      *
     c*                  when      rstypg = 'B'
     c*                  move      rdatag        g7bdata
     c*                  write     pgt7b
      *
     c*                  when      rstypg = 'C'
     c*                  move      rdatag        g7cdata
     c*                  write     pgt7c
      *
     c*                  when      rstypg = 'D'
     c*                  move      rdatag        g7ddata
     c*                  write     pgt7d
      *
     c*                  when      rstypg = 'E'
     c*                  move      rdatag        g7edata
     c*                  write     pgt7e
      *
     c*                  when      rstypg = 'F'
     c*                  move      rdatag        g7fdata
     c*                  write     pgt7f
      *
     c*                  when      rstypg = 'G'
     c*                  move      datag         g7gdata
     c*                  write     pgt7g
      *
     c*                  when      rstypg = 'H'
     c*                  move      rdatag        g7hdata
     c*                  write     pgt7h
      *
     c*                  when      rstypg = 'I'
     c*                  move      rdatag        g7idata
     c*                  write     pgt7i
      *
     c*                  when      rstypg = 'J'
     c*                  move      rdatag        g7jdata
     c*                  write     pgt7j
     c*                  endsl
      *
     c*                  when      rectypg = '9'
     c*                  move      rdatag        g9data
     c*                  write     pgt09
      *
     c*                  endsl
      *
     c*                  endsr                                                  Wr_wcstatx
      */**** End Del **** 03/11/25 *****************************************
