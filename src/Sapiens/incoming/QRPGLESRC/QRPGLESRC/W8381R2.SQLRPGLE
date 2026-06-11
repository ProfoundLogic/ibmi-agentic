    h*  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
    h*=============================================================================================
    h/TITLE W8381R2 - Combine A/R in written by Premium/Payroll Upload
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H optimize(*none)
    h*=============================================================================================
    h*  DESCRIPTION:  This program reads through WMEAP and combines multiple records written      *
    h*                with the same post date for the same policy by W8381Re the same posting     *
    h*                date and A/R code.                                                          *
    h*                                                                                            *
     h*  Called from:  W8381C                                                                      *
     h*                                                                                            *
     h*  Created: 04/11/06 by C:136 for Support: WA135 52009                                       *
    h*=============================================================================================
    h* Revisions:                                                                                 *
     h*                                                                                            *
    h*=============================================================================================
     fwmeap     uf   e           k disk
     fwdeap     uf   e           k disk    rename(PEA01:DEA01)
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
     d svarcd          s                   like(EAARCD)
     d totAmt          s             +3    like(EAAMT)
      *=============================================================================================
      * Main
      *=============================================================================================
     c                   read      dea01
     c                   dow       not(%eof)
     c                   eval      svco#  = EACO#
     c                   eval      svfnd  = EAFND
     c                   eval      svemp# = EAEMP#
     c                   eval      svdiv  = EADIV
     c                   eval      svrdiv = EARDIV
     c                   eval      svfyr  = EAFYR
     c                   eval      svpodt = EAPODT
     c                   eval      svarcd = EAARCD

     c/exec SQL
     c+                  select sum(EAAMT)
     c+                    into :totAmt
     c+                    from WDEAP
     c+                    where EACO#  = :svco#  and
     c+                          EAFND  = :svfnd  and
     c+                          EAEMP# = :svemp# and
     c+                          EADIV  = :svdiv  and
     c+                          EARDIV = :svrdiv and
     c+                          EAFYR  = :svfyr  and
     c+                          EAPODT = :svpodt and
     c+                          EAARCD = :svarcd
     c/end-exec

     c                   if        EAAMT <> totAMT

     c                   eval      EAAMT  = totAmt
     c                   eval      EACKAM = totAmt
     c                   update    DEA01

     c/exec SQL
     c+                  delete from WDEAP
     c+                    where EACO#  = :svco#  and
     c+                          EAFND  = :svfnd  and
     c+                          EAEMP# = :svemp# and
     c+                          EADIV  = :svdiv  and
     c+                          EARDIV = :svrdiv and
     c+                          EAFYR  = :svfyr  and
     c+                          EAPODT = :svpodt and
     c+                          EAARCD = :svarcd and
     c+                          EAAMT <> :totAmt
     c/end-exec

     c                   endif

     c                   read      dea01
     c                   enddo

     c                   seton                                        lr
