     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')

     H/title FIXLDAMG1 - Correct the LDAMG1 values.

     H*****************************************************************
     H* Created 04/14/2011 by c:62 for support SEQ76547               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     Fwmldl1    UF   E           K DISK
     Fwdell     IF   E           K DISK
     Fwmepl     IF   E           K DISK
     d firstYear       s               n
     d xxamg1          s                   like(ldamg1)
     d xxprem          s                   like(epaapr)
     d xxeapr          s                   like(epeapr)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY1         KLIST
     C                   KFLD                    ldco#
     C                   KFLD                    ldfnd
     C                   KFLD                    ldemp#
     C                   KFLD                    lddiv
     C*
     C     ELKY2         KLIST
     C                   KFLD                    ldco#
     C                   KFLD                    ldfnd
     C                   KFLD                    ldemp#
     C                   KFLD                    lddiv
     C                   KFLD                    ldfyr
     C*
     C*   KEY LIST FOR THE EMPLOYER PAYROLL FILE
     C*
     C     EPKY          KLIST
     C                   KFLD                    ldco#
     C                   KFLD                    ldfnd
     C                   KFLD                    ldemp#
     C                   KFLD                    lddiv
     C                   KFLD                    ldfyr
     C*
     C                   read      pld01
     C                   dow       not %eof
     C*
     C                   eval      firstYear=*off
     C                   clear                   ldamg1
     C                   clear                   xxprem
     C                   clear                   xxeapr
     C*
     c     elky1         chain     pel01
     c                   if        %found and elfyr=ldfyr
     c                   eval      firstYear=*on
     c                   endif
     C*
     C     elky2         chain     pel01
     C*
     C     epky          chain     pep01                              99
     C                   dow       not *in99
     C                   if        epstat<>'I'
     C*
     C                   if        epsafl='A'
     C                   eval      xxprem=xxprem+epaapr
     C                   else
     C                   if        firstYear
     C     epeapr        mult(h)   elpror        xxeapr
     C                   eval      xxprem=xxprem+xxeapr
     C                   else
     C                   eval      xxprem=xxprem+epeapr
     C                   endif
     C                   endif
     C                   endif
     C*
     C     epky          reade     pep01                                  99
     c                   enddo
     C*
     C     xxprem        mult(h)   ldlirt        ldamg1
     C                   if        ldamg1=0
     C                   eval      ldamg1=elamag
     C                   endif
     C*
     C                   update    pld01
     C                   read      pld01
     C                   enddo
     C*
     C                   seton                                        lr
