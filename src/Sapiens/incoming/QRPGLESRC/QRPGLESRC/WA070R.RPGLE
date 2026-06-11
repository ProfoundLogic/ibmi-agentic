     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/TITLE WA070R - Update the renewal flag for a set of employers
     H**********************************************************************
     H*  Description:  This program will update the new/renewal flag       *
     H*                for a given employers.                              *
     H*                                                                    *
     H*  Called from:                                                      *
     H*    W0149R02 - Employer X-ref Maintenance                           *
     H*                                                                    *
     H*  CALLS TO:                                                         *
     H*    None                                                            *
     H*                                                                    *
     H**********************************************************************
     H*  Created on 09/04/2008 by c:62 for Project WC1611                  *
     H**********************************************************************
     H* Revisions:                                                         *
     H*                                                                    *
     H* 12/10/09 c:62 Support 70983                                        *
     H*               Add Quote Processing                                 *
     H*                                                                    *
     H**********************************************************************
     FWDELL     UF   E           K DISK
     F*** Begin Add *** 12/10/09 *******************************************
     FWMEML     IF   E           K DISK
     F*** End   Add *** 12/10/09 *******************************************
     F*
     D psds          esds                  extname(szq1p)
     D @@co#           s                   like(elco#)
     D @@fnd           s                   like(elfnd)
     D @@emp#          s                   like(elemp#)
     D*** Begin Add *** 12/10/09 *******************************************
     D @@call          s              1
     D*** End   Add *** 12/10/09 *******************************************
     D ##co#           s                   like(elco#)
     D ##fnd           s                   like(elfnd)
     D ##fyr           s                   like(elfyr)
     D ##emp#          s                   like(elemp#)
     D ##nwrw          s                   like(elnwrw)
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8

     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     C*
     C     ELKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@EMP#
     C*
     C*** Begin Add *** 12/10/09 *******************************************
     C     EMKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@EMP#
     C                   KFLD                    ELDIV
     C*** End   Add *** 12/10/09 *******************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@co#
     C                   PARM                    @@fnd
     C                   PARM                    @@emp#
     C*** Begin Add *** 12/10/09 *******************************************
     C                   PARM                    @@call
     C*** End   Add *** 12/10/09 *******************************************
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C* Let's work thru WDELP and update the new/renewal flag
     C*
     C     elky          chain     pel01
     C                   if        %found
     C                   dou       %eof
     C*** Begin Add *** 12/10/09 *******************************************
     c                   if        q1prms > 3
     c                   if        @@call='Q'
     c     emky          chain     pem01
     c                   if        %found and emrqfl='Y'
     c                   eval      elnwrw='R'
     c                   else
     c                   call      'WA069R'
     c                   parm      elco#         ##co#
     c                   parm      elfnd         ##fnd
     c                   parm      elemp#        ##emp#
     c                   parm      elfyr         ##fyr
     c     elnwrw        parm      *blanks       ##nwrw
     c                   endif
     c                   else
     c                   call      'WA069R'
     c                   parm      elco#         ##co#
     c                   parm      elfnd         ##fnd
     c                   parm      elemp#        ##emp#
     c                   parm      elfyr         ##fyr
     c     elnwrw        parm      *blanks       ##nwrw
     c                   endif
     c                   else
     C*** End   Add *** 12/10/09 *******************************************
     c                   call      'WA069R'
     c                   parm      elco#         ##co#
     c                   parm      elfnd         ##fnd
     c                   parm      elemp#        ##emp#
     c                   parm      elfyr         ##fyr
     c     elnwrw        parm      *blanks       ##nwrw
     C*** Begin Add *** 12/10/09 *******************************************
     c                   endif
     C*** End   Add *** 12/10/09 *******************************************
     c*
     c                   time                    dsdttm
     c                   eval      eltime=dstime
     c                   eval      elpgm=q1pgm
     c                   eval      eluser=q1user
     c                   z-add     dsdate        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         eldt
     c                   update    pel01
     C*
     c     elky          reade     pel01
     c                   enddo
     c                   endif
     c*
     c                   seton                                        lr
