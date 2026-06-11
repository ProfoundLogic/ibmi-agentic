     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')

      /title - Query/Fix pgm for Self-Report A/R Transactions

     h option(*nodebugio:*srcstmt)
     h debug(*yes)
     h dftactgrp(*no) actgrp('FIXARSUMM')
      *=========================================================================
     fwmeal10   uf   e           k disk
     fwmupl     if   e           k disk
     fwmeml     if   e           k disk
     fwdell1    if   e           k disk
     fwtabl     if   e           k disk
     fwdf1l     if   e           k disk
     ffixarsumm o    e             disk    rename(pea01:peafix)

     d psds          esds                  extname(szq1p)

     d stRptDiv        s                   like(EADIV)
     d kkkey           s                   like(BSKEY)
     d svfnd           s                   like(EAFND)
     d svfyr           s                   like(EAFYR)

     c     emky          klist
     c                   kfld                    EACO#
     c                   kfld                    EAFND
     c                   kfld                    EAEMP#
     c                   kfld                    EADIV

     c     emky1         klist
     c                   kfld                    EACO#
     c                   kfld                    EAFND
     c                   kfld                    EAEMP#

     c     emky2         klist
     c                   kfld                    EACO#
     c                   kfld                    EAFND
     c                   kfld                    EAFYR
     c                   kfld                    EAEMP#
     c                   kfld                    EADIV

     c     emky3         klist
     c                   kfld                    EACO#
     c                   kfld                    EAFND
     c                   kfld                    EAFYR
     c                   kfld                    EAEMP#

     c     elky          klist
     c                   kfld                    EACO#
     c                   kfld                    EAFND
     c                   kfld                    EAFYR
     c                   kfld                    EAEMP#
     c                   kfld                    EADIV

     c     bsky          klist
     c                   kfld                    ELCO#
     c                   kfld                    kkkey

     c     f1ky          klist
     c                   kfld                    EACO#
     c                   kfld                    EAFND
     c                   kfld                    EAFYR

     c     Q1USER        chain     pup01
     c                   if        not(%found)
     c                   seton                                        lr
     c                   return
     c                   endif

     c     UPCO#         setll     pea01
     c     UPCO#         reade     pea01
     c                   dow       not(%eof)

      * Validate Employer Master exists. Summarized A/R is not a
      * concern for state reporting divisions.

     c     emky          chain     pem01
     c                   if        not(%found) or
     c                             EMSTRP = 'Y'
     c     emky2         setgt     pea01
     c     UPCO#         reade     pea01
     c                   iter
     c                   endif

      * Validate Employer Detail exists and that div is A/R Summary

     c     elky          chain     pel01
     c                   if        not(%found) or ELARSM <> 'Y'
     c     elky          setgt     pea01
     c     UPCO#         reade     pea01
     c                   iter
     c                   endif

      * Verify policy is set up for S/R Online

     c                   movel     ELBSCD        kkkey
     c     bsky          chain     pbs01
     c                   if        BSONLI <> 'Y'
     c     elky          setgt     pea01
     c     UPCO#         reade     pea01
     c                   iter
     c                   endif

      * Get S/R A/R Code from Group Detail

     c                   if        EAFND <> svfnd or
     c                             EAFYR <> svfyr
     c     f1ky          chain     pf101
     c                   if        not(%found) or F1SRBB = *blanks
     c     f1ky          setgt     pea01
     c     UPCO#         reade     pea01
     c                   iter
     c                   endif
     c                   eval      svfnd = EAFND
     c                   eval      svfyr = EAFYR
     c                   endif

      * Verify that record might be written by SRONLI. A/R is
      * transferred from A/R Batch.

     c                   if        EAARCD <> F1SRBB or
     c                             EAPGM <> 'W9206R'
     c     UPCO#         reade     pea01
     c                   iter
     c                   endif
      * Retrieve state reporting division

     c                   eval      stRptDiv = *hival
     c     emky1         setll     pem01
     c     emky1         reade     pem01
     c                   dow       not(%eof)
     c                   if        EMSTRP = 'Y'
     c                   eval      stRptDiv = EMDIV
     c                   leave
     c                   endif
     c     emky1         reade     pem01
     c                   enddo

     c                   if        stRptDiv = *hival
     c     emky3         setgt     pea01
     c     UPCO#         reade     pea01
     c                   iter
     c                   endif

     c                   if        EADIV <> stRptDiv
     c                   write     peafix
     c                   eval      EARDIV = EADIV
     c                   eval      EADIV = stRptDiv
     c                   update    pea01
     c                   endif

     c     UPCO#         reade     pea01
     c                   enddo
     c
     c                   seton                                        lr
