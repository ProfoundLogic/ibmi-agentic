    h*  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
    h*=============================================================================================
    h/TITLE FIX52009 - Combine A/R written by Premium Upload
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H optimize(*none)
    h*=============================================================================================
    h*  DESCRIPTION:  This program reads through WMEAP and combines multiple records with the     *
    h*                same transaction date, post date, and A/R code on a policy written by       *
    h*                W8381R.                                                                     *
    h*                                                                                            *
     h*  Created: 04/11/06 by C:136 for Support: WA135 52009                                       *
    h*=============================================================================================
    h* Revisions:                                                                                 *
     h*                                                                                            *
    h*=============================================================================================
     fwmeap     uf   e           k disk
      *=============================================================================================
      * Program Field definitions
      *=============================================================================================
     d svco#           s                   like(EACO# )
     d svfnd           s                   like(EAFND )
     d svemp#          s                   like(EAEMP#)
     d svdiv           s                   like(EADIV )
     d svrdiv          s                   like(EARDIV)
     d svfyr           s                   like(EAFYR )
     d svpodt          s                   like(EAPODT)
     d svtrdt          s                   like(EAPODT)
     d svarcd          s                   like(EAARCD)
     d totAmt          s             +3    like(EAAMT)
      *=============================================================================================
      * Main
      *=============================================================================================
     c                   read      pea01
     c                   dow       not(%eof)
     c                   eval      svco#  = EACO#
     c                   eval      svfnd  = EAFND
     c                   eval      svemp# = EAEMP#
     c                   eval      svdiv  = EADIV
     c                   eval      svrdiv = EARDIV
     c                   eval      svfyr  = EAFYR
     c                   eval      svtrdt = EATRDT
     c                   eval      svpodt = EAPODT
     c                   eval      svarcd = EAARCD

     c                   if        EAPGM = 'W8381R'

     c/exec SQL
     c+                  select sum(EAAMT)
     c+                    into :totAmt
     c+                    from WMEAP
     c+                    where EACO#  = :svco#  and
     c+                          EAFND  = :svfnd  and
     c+                          EAEMP# = :svemp# and
     c+                          EADIV  = :svdiv  and
     c+                          EARDIV = :svrdiv and
     c+                          EAFYR  = :svfyr  and
     c+                          EATRDT = :svtrdt and
     c+                          EAPODT = :svpodt and
     c+                          EAARCD = :svarcd and
     c+                          EAPGM  = 'W8381R'
     c/end-exec

     c                   eval      EAAMT = totAmt
     c                   update    PEA01

     c/exec SQL
     c+                  delete from WMEAP
     c+                    where EACO#  = :svco#  and
     c+                          EAFND  = :svfnd  and
     c+                          EAEMP# = :svemp# and
     c+                          EADIV  = :svdiv  and
     c+                          EARDIV = :svrdiv and
     c+                          EAFYR  = :svfyr  and
     c+                          EATRDT = :svtrdt and
     c+                          EAPODT = :svpodt and
     c+                          EAARCD = :svarcd and
     c+                          EAAMT <> :totAmt and
     c+                          EAPGM  = 'W8381R'
     c/end-exec

     c                   endif                                                  EAPGM = 'W8381R'

     c                   read      pea01
     c                   enddo

     c                   seton                                        lr
