    h*  (C) Copyright Fiserv Insurance Solutions, Inc., 2008
    h*=============================================================================================
    h/TITLE Fix65394 - Combine A/R by policy
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H optimize(*none)
    h*=============================================================================================
    h*  DESCRIPTION:  This program reads through WMEAP and combines multiple records written      *
    h*                with the same post date for the same policy with the same posting           *
    h*                date and A/R code for deposit transfer transactions written by Audit.       *
    h*                                                                                            *
     h*  Called from:  Command Line                                                                *
     h*                                                                                            *
     h*  Created: 10/01/08 by C:136 for Support: WA146 65394                                       *
    h*=============================================================================================
    h* Revisions:                                                                                 *
     h*                                                                                            *
    h*=============================================================================================
     fwmeal     uf   e           k disk
      *=============================================================================================
      * Program Field definitions
      *=============================================================================================
     d svco#           s                   like(EACO# )
     d svfnd           s                   like(EAFND )
     d svemp#          s                   like(EAEMP#)
     d svdiv           s                   like(EADIV )
     d svrdiv          s                   like(EARDIV)
     d svfyr           s                   like(EAFYR )
     d svtrdt          s                   like(EAPODT)
     d svpodt          s                   like(EAPODT)
     d svarcd          s                   like(EAARCD)
     d totAmt          s                   like(EAAMT)
      *=============================================================================================
      * Main
      *=============================================================================================
     c     *entry        plist
     c                   parm                    @@co#             3
     c                   parm                    @@fnd             3
     c                   parm                    @@emp#            9
     c                   parm                    @@div             5
     c                   parm                    @@fyr             3

     c     eaky          klist
     c                   kfld                    svco#
     c                   kfld                    svfnd
     c                   kfld                    svemp#
     c                   kfld                    svdiv
     c                   kfld                    svfyr
     c
     c                   move      @@co#         svco#
     c                   move      @@fnd         svfnd
     c                   move      @@emp#        svemp#
     c                   move      @@div         svdiv
     c                   move      @@fyr         svfyr
     c
     c     eaky          setll     pea01
     c     eaky          reade     pea01
     c                   dow       not(%eof)

     c                   if        EAPGM = 'W7720R' and
     c                             (EAARTY = 'H' or
     c                              EAARTY = 'I')

     c                   eval      svrdiv = EARDIV
     c                   eval      svtrdt = EATRDT
     c                   eval      svpodt = EAPODT
     c                   eval      svarcd = EAARCD

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
     c+                          EAPGM  = 'W7720R'
     c/end-exec

     c                   eval      EAAMT  = totAmt
     c                   eval      EAUSER = 'SQL'
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
     c+                          EAUSER <> 'SQL'  and
     c+                          EAPGM  = 'W7720R'
     c/end-exec

     c                   endif

     c     eaky          reade     pea01
     c                   enddo

     c                   seton                                        lr
