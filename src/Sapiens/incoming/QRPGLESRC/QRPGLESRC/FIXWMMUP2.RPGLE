     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')

     H/TITLE FIXWMMUP2 - REPORT OF ENDORSEMENT ROLLOVER ERRORS
      *========================================================================*
      *                                                                        *
      * PROGRAM DESCRIPTION:                                                   *
      *   Output list of Endorsement Rollover errors. Current year             *
      *   Endorsements (Mid-Year, Policy) should roll to renewal year          *
      *   Policy Endorsements:                                                 *
      *
      *      WC000313 - Generic Waiver                                         *
      *      WC040306 - CA Waiver                                              *
      *      WC990306 - clt132 CA Waiver                                       *
      *      WC430305 - UT Waiver                                              *
      *      WC000302 - Designated Excluded Workplace                          *
      *      WC000301A - Alternate Employer                                    *
      *
      *========================================================================*
      * REVISIONS:
      *
      * 04/01/11 C:061 SEQ75903
      *                Created.
      *
      *========================================================================*
      *
     FWMEML     IF   E           K Disk
     FWDELL     IF   E           K Disk
     FWMMUL5    IF   E           K Disk
     FWMWVL     IF   E           K Disk
     Fwmewl     IF   E           K Disk
     Fwdael     IF   E           K Disk
     FQSYSPRT   O    F  132        PRINTER oflind(*inOA)

     d sysdat          s              7  0
     d cxdate          s              7  0
     d kkform          s                   like(muform)
     d kkend#          s                   like(muend#)
     d xxmess          s             80
     d svemp#          s                   like(ememp#)
     d Count           s             11  0
      * zzz

     D                 DS
     D xxDate                         8s 0 inz
     D  xxYear                        4s 0 overlay(xxDate:5)                    Year
     D  xxMonth                       2s 0 overlay(xxDate:1)                    Month
     D  xxDay                         2s 0 overlay(xxDate:3)                    Day

     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)

      ** External data structure
     D psds          ESDS                  ExtName(szq1p)

     D/copy sPrDate
     D/copy wPrEmplr

     D Chk_waiv_err    pr
     D Form                                const like(muform)

     D Chk_des_wrk     pr
     D Chk_alt_emp     pr
     D Write_det1      pr


      *========================================================================*
      * KLIST definitions
      *========================================================================*

      * Employer

     C     emKY          KList
     C                   KFld                    kkCO#

      * Employer Detail

     C     ELKY          KList
     C                   KFld                    emco#
     C                   KFld                    emfnd
     C                   KFld                    ememp#
     C                   KFld                    emdiv

      * Employer Attached Endorsement Forms

     C     MUKY          KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    ELFYR
     C                   KFld                    ELEMP#
     C                   KFld                    KKForm
     C                   KFld                    KKEnd#

      *  Waiver Data

     C     wvky          KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    ELFYR
     C                   KFld                    ELEMP#

      *  Excluded Work Places

     C     ewky          KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    ELFYR
     C                   KFld                    ELEMP#

      *  Alternate Employer

     C     aeky          KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    ELFYR
     C                   KFld                    ELEMP#

      *========================================================================*
      * PLIST Definitions
      *========================================================================*



      *========================================================================*
      * Main
      *========================================================================*

      * Initialize

     c                   eval      count = 0
     c                   call      'RTVCLTID'
     c                   parm                    cltid             3

     c                   MOVE      *loval        KKCO#             3 0
     c                   Eval      xxDate = *date
     c                   time                    dsdttm
     c                   eval      sysdat = dtcymd(dsdate)
     C                   move      *ALL'_'       line            132
     c                   eval      *in71 = *off

      *  Print the page header.
     C                   Except    HEAD

      *  For each Employer
     C     emky          setll     PEm01
     C     emky          reade     PEm01

     c                   dow       %eof = *off

     c                   if        emstrp = 'Y'

      * if employer not cancelled
     c                   eval      cxdate = GetCxDate(emco#:emfnd:ememp#:emdiv)

     c                   if        (cxdate = 0) or (cxdate > sysdat)

      *  If Active or Future Policy Year
     C     ELky          setll     PEL01
     C     ELky          reade     PEL01

     c                   dow       %eof = *off

     c                   if        (sysdat < elptdt)

      *  Check for Errors
     c                   callp     chk_waiv_err('WC000313       ')

     c                   if        cltid = '132'
     c                   callp     chk_waiv_err('WC990306       ')
     c                   else
     c                   callp     chk_waiv_err('WC040306       ')
     c                   endif

     c                   callp     chk_waiv_err('WC430305       ')
     c                   callp     chk_des_wrk
     c                   callp     chk_alt_emp

      * zzz
     c                   endif

     C     ELky          reade     PEL01
     c                   enddo

     c                   endif

     c                   endif

     C     emky          reade     pem01
     c                   enddo

     c                   if        *in71 = *off
     c                   except    none
     c                   else
     c                   except    Total
     c                   endif

     C                   Eval      *inlr = *on

      *========================================================================*

     OQSYSPRT   E            HEAD           1  5
     O                       q1pgm               10
     O                                           73 'ENDORSEMENT ROLLOVER'
     O                                        +0001 'ERRORS'
     O                       q1user             132

     O          E            HEAD           1
     O                       xxMonth              2
     O                                            3 '/'
     O                       xxDay                5
     O                                            6 '/'
     O                       xxYear              10
     O                                          125 'PAGE:'
     O                       PAGE          Z    132

     O          E            HEAD           2
     O                                        +0000 'Client:'
     O                       cltid            +0001


     O          E            HEAD        1  0
     O                       line             +0000

     O          E            HEAD           2
     O                                           05 'GROUP'
     O                                           15 'EMPLOYER#'
     O                                        +0001 ' DIV '
     O                                        +0001 ' FYR '
     O                                        +0001 'ENDORSEMENT    '
     O                                        +0001 'MESSAGE'

     O          E            DET1           1
     O                       ELFND         Z      4
     O                       ELEMP#        Z     15
     O                       ELDIV         Z  +0001
     O                       ELFYR         Z  +0002
     O                       kkform           +0002
     O                       xxmess           +0001

     O          E            DOWN1          1

     O          E            None        2
     O                                        +0010 '*** NO ERRORS FOUND ***'

     O          E            TOTAL       1  1
     O                                        +0000 'Count:'
     O                       count         x  +0001


      * zzz

      *====================================================================
      * chk_waiv_err
      *====================================================================
     pChk_waiv_err     b
     DChk_waiv_err     pi
     D Form                                const like(muform)

     d wmmup_exists    s              3
     d wmwvp_exists    s              3

     c                   eval      WMMUP_exists = 'No '
     c                   eval      WMWVP_exists = 'No '
     c                   clear                   xxmess

      *  Check Employer Attached Endorsement Forms Record

     c                   eval      kkform = Form
     c                   eval      kkend# = 0
     c     muky          setll     pmu01
     c     muky          reade     pmu01

     c                   dow       %eof = *off

     c                   if        mumnau = 'M'
     c                   eval      WMMUP_exists = 'Yes'
     c                   leave
     c                   endif

     c     muky          reade     pmu01
     c                   enddo

      *  Check Waiver Data Record

     c     wvky          setll     pwv01
     c     wvky          reade     pwv01

     c                   dow       %eof = *off

     c                   if        (wvenm = Form) and (wvend# = 0)
     c                   eval      WMWVP_exists = 'Yes'
     c                   leave
     c                   endif

     c     wvky          reade     pwv01
     c                   enddo

      *  Report Result

     c                   select
     c                   when      (WMMUP_exists = 'Yes') and
     c                             (WMWVP_exists = 'No ')
     c                   eval      xxmess = 'No Data for attach record'
     c                   callp     write_det1

     c                   when      (WMMUP_exists = 'No ') and
     c                             (WMWVP_exists = 'Yes')
     c                   eval      xxmess = 'No attachment record for data'
     c                   callp     write_det1

     c                   endsl

     pChk_waiv_err     e


      *====================================================================
      * chk_des_wrk
      *====================================================================
     pChk_des_wrk      b
     DChk_des_wrk      pi

     d wmmup_exists    s              3
     d wmewp_exists    s              3

     c                   eval      WMMUP_exists = 'No '
     c                   eval      WMEWP_exists = 'No '
     c                   clear                   xxmess

      *  Check Employer Attached Endorsement Forms Record

     c                   eval      kkform = 'WC000302'
     c                   eval      kkend# = 0
     c     muky          setll     pmu01
     c     muky          reade     pmu01

     c                   dow       %eof = *off

     c                   if        mumnau = 'M'
     c                   eval      WMMUP_exists = 'Yes'
     c                   leave
     c                   endif

     c     muky          reade     pmu01
     c                   enddo

      *  Check Excluded Work Places

     c     ewky          chain     pew01

     c                   if        %found
     c                   eval      WMEWP_exists = 'Yes'
     c                   endif
      *
      *  Report Result

     c                   select
     c                   when      (WMMUP_exists = 'Yes') and
     c                             (WMEWP_exists = 'No ')
     c                   eval      xxmess = 'No Data for attach record'
     c                   callp     write_det1

     c                   when      (WMMUP_exists = 'No ') and
     c                             (WMEWP_exists = 'Yes')
     c                   eval      xxmess = 'No attachment record for data'
     c                   callp     write_det1

     c                   endsl

     pChk_des_wrk      e


      *====================================================================
      * chk_alt_emp
      *====================================================================
     pChk_alt_emp      b
     DChk_alt_emp      pi

     d wmmup_exists    s              3
     d wmaep_exists    s              3

     c                   eval      WMMUP_exists = 'No '
     c                   eval      WMAEP_exists = 'No '
     c                   clear                   xxmess

      *  Check Employer Attached Endorsement Forms Record

     c                   eval      kkform = 'WC000301A'
     c                   eval      kkend# = 0
     c     muky          setll     pmu01
     c     muky          reade     pmu01

     c                   dow       %eof = *off

     c                   if        mumnau = 'A'
     c                   return
     c                   endif

     c                   if        mumnau = 'M'
     c                   eval      WMMUP_exists = 'Yes'
     c                   leave
     c                   endif

     c     muky          reade     pmu01
     c                   enddo

      *  Check Excluded Work Places

     c     AEky          chain     pAE01

     c                   if        %found
     c                   eval      WMAEP_exists = 'Yes'
     c                   endif
      *
      *  Report Result

     c                   select
     c                   when      (WMMUP_exists = 'Yes') and
     c                             (WMAEP_exists = 'No ')
     c                   eval      xxmess = 'No Data for attach record'
     c                   callp     write_det1

     c                   when      (WMMUP_exists = 'No ') and
     c                             (WMAEP_exists = 'Yes')
     c                   eval      xxmess = 'No attachment record for data'
     c                   callp     write_det1

     c                   endsl

     pChk_alt_emp      e

                                                                                         : :

      *====================================================================
      * write_det1
      *====================================================================
     pWrite_det1       b
     DWrite_det1       pi

     c   OA              except    head
     c                   except    det1
     c                   eval      *in71 = *on
     c                   eval      count = count + 1

     pWrite_det1       e

