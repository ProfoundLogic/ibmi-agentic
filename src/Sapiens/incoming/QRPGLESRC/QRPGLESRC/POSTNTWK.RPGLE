     H* (C) Copyright Fiserv Insurance Solutions, 2006
     H**********************************************************************
     H*  Post Credits and fix fee amounts - Network
     H**********************************************************************
     H*  Created on 08/06/2006 by c:62 Project 135WHU                      *
     H**********************************************************************
     H*  Revisions:                                                        *
     H*                                                                    *
     H**********************************************************************
     Ffaeal1    uf   e           k disk
     Ffaeol4    if   e           k disk
     Fwmeml     if   e           k disk
     Fwmcml1    if   e           k disk
     Fwarpla    if   e           k disk
     Fwdpalg    if   e           k disk
     Fwxcrpn    o    e             disk
     Fqsysprt   o    f  198        printer oflind(*inoa)
     Fqsysprt1  o    f  198        printer oflind(*inoB)
     F
     D##co#            s                   like(eaco#)
     D##sub#           s                   like(easub#)
     D##fnd            s                   like(eafnd)
     D##fyr            s                   like(eafyr)
     D##case           s                   like(eacase)
     D##eob#           s                   like(eaeob#)
     D##rcn            s                   like(earcn)
     D##pct            s              5  2
     D##bamt           s                   like(eabamt)
     D##namt           s                   like(eanamt)
     D##call           s              1
     D##rtcd           s              1
     D##crb            s              1
     D##crn            s              1
     Dxxrtcd           s              1
     Dholdbamt         s                   like(eabamt)
     Dholdnamt         s                   like(eanamt)
     Ddiffbamt         s                   like(eabamt)
     Ddiffnamt         s                   like(eanamt)
     Dxxbamt           s             11  2
     Dxxnamt           s             11  2
     Dxxtotal          s             11  2
     Dxxfeeb           s                   like(eabamt)
     Dxxamtb           s                   like(eabamt)
     Dxxfeen           s                   like(eabamt)
     Dxxamtn           s                   like(eabamt)
     Dkkeobs           s                   like(rpeobs)
     D@@update         s              1
     D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     C*
     c     *entry        plist
     c                   parm                    @@update
     C*
     c     cmky          klist
     c                   kfld                    eaco#
     c                   kfld                    eafnd
     c                   kfld                    eafyr
     c                   kfld                    eacase
     C*
     c     emky          klist
     c                   kfld                    cmco#
     c                   kfld                    cmfnd
     c                   kfld                    cmemp#
     c                   kfld                    cmdiv
     C*
     c     eoky          klist
     c                   kfld                    eaco#
     c                   kfld                    easub#
     c                   kfld                    eafnd
     c                   kfld                    eafyr
     c                   kfld                    eacase
     c                   kfld                    eaeob#
     C*
     c     paky          klist
     c                   kfld                    eaco#
     c                   kfld                    eafnd
     c                   kfld                    eafyr
     c                   kfld                    eacase
     c                   kfld                    eaeob#
     c                   kfld                    kkeobs
     C*
     c     rpky          klist
     c                   kfld                    eaco#
     c                   kfld                    eafnd
     c                   kfld                    eaeob#
     c                   kfld                    kkeobs
     c*
     c                   eval      xxbamt = 0
     c                   eval      xxnamt = 0
     c                   eval      xxtotal = 0
     c                   time                    dsdttm
     c*
     c                   except    head
     c                   except    head1
     c                   eval      *inoa = *off
     c                   eval      *inob = *off
     c*
     c     *loval        setll     pea01
     C                   read      pea01
     c                   dow       not %eof
     c*
     c                   if        eaepfl = 'P'
     c*
     c                   eval      holdnamt = eanamt
     c*
     c*
     c     eoky          chain     peo01
     c                   if        eontwk = 'PPC'
     c*
     c                   call      'FA038R'
     C                   parm      eaco#         ##co#
     C                   parm      easub#        ##sub#
     C                   parm      eafnd         ##fnd
     C                   parm      eafyr         ##fyr
     C                   parm      eacase        ##case
     C                   parm      eaeob#        ##eob#
     C                   parm      earcn         ##rcn
     C                   parm      0.25          ##pct
     C                   parm      *zeros        ##namt
     C                   parm      'P'           ##call
     c*
     c                   else
     c                   eval      ##namt = 0
     c                   endif
     c*
     c                   if        @@update = 'Y'
     c                   eval      eanamt = ##namt
     c                   eval      eancam = ##namt
     c                   endif
     c*
     c                   update    pea01
     c*
     c                   exsr      checkDenial
     c*
     c                   exsr      checkPymnts
     c*
     c                   if        (##namt <> 0  and *in75)
     c                   eval      xxrtcd = ##rtcd
     c                   if        @@update = 'Y'
     c                   call      'W0081RNET'
     C                   parm      *blanks       ##call
     C                   parm      eaco#         ##CO#
     C                   parm      easub#        ##SUB#
     C                   parm      eafnd         ##fnd
     C                   parm      eafyr         ##FYR
     C                   parm      eacase        ##CASE
     C                   parm      eaeob#        ##EOB#
     C                   parm      earcn         ##rcn
     C                   parm                    ##namt
     C                   parm      *blanks       ##rtcd
     C                   parm                    ##crn
     c                   endif
     c*
     c                   setoff                                       70
     c                   if        ##crn = 'Y'
     c                   if        xxrtcd = 'P'
     c                   seton                                        70
     c                   else
     c                   seton                                        70
     c                   endif
     c                   endif
    C*
     c                   if        ##namt <> 0 and not *in70
     c                   setoff                                       73
     c                   add       ##namt        xxnamt
     c                   else
     c                   seton                                        73
     c                   endif
     c*
     c                   if        *inoa
     c                   except    head
     c                   endif
     c                   except    det
     C*
     C                   exsr      writeWXCRP
     c*
     c                   endif
     c                   endif
     c*
     c                   read      pea01
     c                   enddo
     C*
     C                   except    tot
     C*
     C
     C                   seton                                        lr
     c*=====================================================================
     c* checkDenial - check for Denial
     c*=====================================================================
     c     checkDenial   begsr
     c*
     c                   setoff                                       71
     c*
     c                   if        (holdnamt > 0 and eanamt = 0)
     C                   z-sub     holdnamt      ##namt
     c                   seton                                        71
     c                   else
     c                   z-add     eanamt        ##namt
     c                   endif
     c*
     c                   endsr
     c*=====================================================================
     c* checkPymnts - check to see if the payment has been issued.
     c*=====================================================================
     c     checkPymnts   begsr
     c*
     c                   eval      ##crn = 'N'
     c                   setoff                                       7577
     c                   if        ##rtcd = 'P'
     c                   exsr      checkPPymnts
     c                   else
     c                   z-add     earcn         kkeobs
     c*
     c                   setoff                                       75
     c                   if        ##namt < 0
     c     rpky          chain     prp01
     c                   if        %found
     c                   dou       %eof
     c                   if        rpfein = 751156402 and rpmod = 1 and
     c                             rpstat <> 'D'
     c                   eval      ##crn = 'Y'
     c                   eval      diffbamt = (##namt*-1)-rpamt
     c                   if        diffnamt > .01 or diffnamt < -.01
     c                   eval      xxfeen = ##bamt*-1
     c                   eval      xxamtn = rpamt
     c                   seton                                        77
     c                   endif
     c                   seton                                        75
     c                   leave
     c                   endif
     c     rpky          reade     prp01
     c                   enddo
     c                   endif
     c*
     c                   if        not *in75
     c     paky          chain     ppa01
     c                   if        %found
     c                   dou       %eof
     c                   if        pafein = 751156402 and pamod = 1
     c                   if        paadst = ' ' and (patype = 'P' or
     c                                               patype = 'M')
     c                   eval      diffbamt = (##namt*-1)-paamt
     c                   if        diffnamt > .01 or diffnamt < -.01
     c                   eval      xxfeen = ##bamt*-1
     c                   eval      xxamtn = paamt
     c                   seton                                        77
     c                   endif
     c                   seton                                        75
     c                   leave
     c                   endif
     c                   endif
     c     paky          reade     ppa01
     c                   enddo
     c                   endif
     c                   endif
     c                   endif
     c                   endif
     c*
     c                   if        (##namt < 0  and *in75)
     c                   setoff                                       78
     c                   if        *in77
     c                   eval      ##namt = 0
     c                   seton                                        78
     c                   if        *inob
     c                   except    head1
     c                   endif
     c                   except    det1
     c                   endif
     c                   endif
     c*
     c                   if        ##rtcd <> 'P' and ##namt > 0
     c                   eval      ##namt = 0
     c                   endif
     c*
     c                   endsr
     c*=====================================================================
     c* checkPPymnts - check to see if the payment has been issued.
     c*=====================================================================
     c     checkPPymnts  begsr
     c*
     c                   setoff                                       7475
     c                   setoff                                       7677
     c                   eval      kkeobs = earcn - 1
     c*
     c                   setoff                                       75
     c                   if        ##namt <> 0
     c     rpky          chain     prp01
     c                   if        %found
     c                   dou       %eof
     c                   if        rpfein = 751156402 and rpmod = 1 and
     c                             rpstat <> 'D'
     c                   seton                                        75
     c                   leave
     c                   endif
     c     rpky          reade     prp01
     c                   enddo
     c                   endif
     c*
     c                   if        not *in75
     c     paky          chain     ppa01
     c                   if        %found
     c                   dou       %eof
     c                   if        pafein = 751156402 and pamod = 1
     c                   if        paadst = ' ' and (patype = 'P' or
     c                                               patype = 'M')
     c                   seton                                        75
     c                   leave
     c                   endif
     c                   endif
     c     paky          reade     ppa01
     c                   enddo
     c                   endif
     c                   endif
     c                   endif
     c*
     c                   endsr
     c*=====================================================================
     c* writeWXCRP - write to workfile
     c*=====================================================================
     c     writeWXCRP    begsr
     c*
     c                   clear                   pcr01
     C*
     c                   eval      crco# = eaco#
     c                   eval      crsub# = easub#
     c                   eval      crfnd = eafnd
     c                   eval      crfyr = eafyr
     c                   eval      crcase = eacase
     c                   eval      creob# = eaeob#
     c                   eval      crrcn = earcn
     c                   eval      crchrg = eachrg
     c                   eval      crmra = eamra
     c                   eval      crothr = eaothr
     c                   eval      croth2 = eaoth2
     c                   eval      crndis = eandis
     c                   eval      crpaid = eapaid
     c                   eval      crnamt = ##namt
     c*
     c                   if        *in71
     c                   eval      crdeny = 'Y'
     c                   else
     c                   eval      crdeny = 'N'
     c                   endif
     C*
     c                   if        ##crn = 'Y'
     c                   if        xxrtcd = 'P'
     c                   eval      crstat = 'W'
     c                   else
     c                   eval      crstat = 'D'
     c                   endif
     c                   endif
     C*
     c     cmky          chain     pcm01
     c                   if        %found
     c     emky          chain     pem01
     c                   if        %found
     c                   eval      crrgn# = emrgn#
     c                   endif
     c                   endif
     C*
     c                   write     pcr01
     c*
     C                   endsr
     c*=====================================================================
     Oqsysprt   e            head           1 02
     O                       dstime               8 '  :  :  '
     O                                           97 ' '
     O                       dsdate             157 '0  /  /    '
     O          e            head           2
     O                                           10 'POSTCRDT  '
     O                                           74 '           Post'
     O                                           95 ' Credit Report       '
     O                                          152 'PAGE:'
     O                       page               157
     O          e            head           1
     O                                            3 'Grp'
     O                                            8 'Yr'
     O                                           19 'Case'
     O                                           31 'EOMB#'
     O                                           42 'Rcn'
     O                                           62 'Charge'
     O                                           82 'MRA'
     O                                          102 'Other'
     O                                          122 'Network'
     O                                          142 'Paid'
     O                                          162 'Network Fee'
     O          e            det            1
     O                       eafnd         4      3
     O                       eafyr         4      8
     O                       eacase        4     19
     O                       eaeob#        4     31
     O                       earcn         4     42
     O                       eachrg        n     62 '$'
     O                       eamra         n     82 '$'
     O                       eaothr        n    102 '$'
     O                       eandis        n    122 '$'
     O                       eapaid        n    142 '$'
     O              n73      ##namt        n    162 '$'
     O               73                         162 '----------'
     O               71                         195 'Denied'
     O          e            tot            1
     O                                         +002 'Total Network Credit:'
     O                       xxnamt        n   +001 '$'
     Oqsysprt1  e            head1          1 02
     O                       dstime               8 '  :  :  '
     O                                           97 ' '
     O                       dsdate             157 '0  /  /    '
     O          e            head1          2
     O                                           10 'POSTCRDT  '
     O                                           74 '   Post Credit '
     O                                           95 ' Report - Exceptions '
     O                                          152 'PAGE:'
     O                       page1              157
     O          e            head1          1
     O                                            3 'Grp'
     O                                            8 'Yr'
     O                                           19 'Case'
     O                                           31 'EOMB#'
     O                                           42 'Rcn'
     O                                           62 'Charge'
     O                                           82 'MRA'
     O                                          102 'Other'
     O                                          122 'Network'
     O                                          142 'Paid'
     O                                          162 'Fee Amount'
     O                                          182 'Paid Amount'
     O          e            det1           1
     O                       eafnd         4      3
     O                       eafyr         4      8
     O                       eacase        4     19
     O                       eaeob#        4     31
     O                       earcn         4     42
     O                       eachrg        n     62 '$'
     O                       eamra         n     82 '$'
     O                       eaothr        n    102 '$'
     O                       eandis        n    122 '$'
     O                       eapaid        n    142 '$'
     O               78      xxfeen        n    162 '$'
     O               78      xxamtn        n    182 '$'
     o               78                         188 'NWS'
