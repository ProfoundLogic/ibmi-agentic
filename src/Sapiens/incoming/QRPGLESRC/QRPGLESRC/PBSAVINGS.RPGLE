     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/TITLE PBSAVINGS                                                                      *
     H**************************************************************************************
     H*  Description:  Provider Bills Savings Report                                       *
     H*                                                                                    *
     H*  Created: 03/30/03   Programmer C:79    Project:MCMNT                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*  06/11/13 C: 79  Support: SY246                                                    *
     H*                  Use CCK Instead of CRW                                            *
     H*  04/26/04 C: 79  Support: 42312                                                    *
     H*                  Use FDDAP                                                         *
     H*  05/24/04 C: 79  Project: Request from Jackie for Summary Report                   *
     H*                                                                                    *
     H**************************************************************************************
     FFAEAL38   IF   E           K DISK
     FFAEOLG    IF   E           K DISK
     FFDDHL     IF   E           K DISK
     F*** BEG CHG *** 04/26/04 *********************************************
     FFDCAL5    IF   E           K DISK
     F******** FDCALXX   IF   E           K DISK
     F*** END CHG *** 04/26/04 *********************************************
     F*** BEG ADD *** 04/26/04 *********************************************
     FFDDAL2    IF   E           K DISK
     F*** END ADD *** 04/26/04 *********************************************
     FFASAP     O    E             DISK
     FSMUPL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D qqclt#          s              3
     D qqst            s              2
     D mthchrg         s             11  2
     D mthmra          s             11  2
     D mthndis         s             11  2
     D mthothr         s             11  2
     D mthcci          s             11  2
     D mthcrw          s             11  2
     D mthoth2         s             11  2
     D mthoth2a        s             11  2
     D mthpaid         s             11  2
     D mth#bills       s              7  0
     D totchrg         s             11  2
     D totmra          s             11  2
     D totndis         s             11  2
     D totothr         s             11  2
     D totcci          s             11  2
     D totcrw          s             11  2
     D tototh2         s             11  2
     D totpaid         s             11  2
     D tot#bills       s              7  0
     D sysdt           s              7  0
     D xxccis          s              5  2
     D xxcrws          s              5  2
     D xxfrdt          s             10
     D xxtodt          s             10
     D zzfrdt          s               d   datfmt(*usa)
     D                 ds
     D xxdate1                        8  0
     D  xxcurmth                      6  0 overlay(xxdate1)
     D  xxprtmth                      2  0 overlay(xxdate1:5)
     D  xxprtyr                       4  0 overlay(xxdate1:1)
     D zzprtmth                       2
     D zzprtyr                        4
     D                 ds
     D x1date                              like(eadt)
     D xxdate                         8  0
     D  xxnewmth                      6  0 overlay(xxdate)
     D xxfdat          s                   like(eaepdt)
     D xxtdat          s                   like(eaepdt)
     D srffmt          s              8
     D srtfmt          s              8
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D kkusr           s                   like(upusr)
     D kkco#           s                   like(eaco#)
     D kksub#          s                   like(easub#)
     D kkfnd           s                   like(eafnd)
     D kkfyr           s                   like(eafyr)
     D kkcase          s                   like(eacase)
     D kkeob#          s                   like(eaeob#)
     D kkrcn           s                   like(earcn)
     D*** BEG ADD *** 04/26/04 *********************************************
     D kkdseq          s                   like(dhdseq)
     D*** END ADD *** 04/26/04 *********************************************
     D kkepdt          s                   like(eaepdt)

     DPSDS           ESDS                  Extname(SZQ1P)

     C     upky          klist
     C                   kfld                    kkusr

     C     eaky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkepdt

     C     eaky1         klist
     C                   kfld                    kkco#

     C     dhky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#

     C     eoky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#

     C     caky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C*** BEG CHG *** 04/26/04 *********************************************
     C****************** kfld                    kkrcn
     C                   kfld                    kkdseq
     C*** END CHG *** 04/26/04 *********************************************
     C*
     C*** BEG ADD *** 04/26/04 *********************************************
     C     daky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkdseq
     C                   kfld                    kkrcn
     C*** END ADD *** 04/26/04 *********************************************
     C*
     C     *entry        plist
     C                   parm                    qqmode            1
     C                   parm                    qqclt#            3
     C                   parm                    qqst              2
     C                   parm                    qqfdat            8
     C                   parm                    qqtdat            8
    C*
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      '*YYMD'       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C*----------------------------------------------------------------------
     C*  Read through FAEAP starting at date that was passed in
     C*----------------------------------------------------------------------
     C*
     C* Get the current date in format CYMD, CO#, Set keys
     C*
     C                   exsr      init
     C*
     C     eaky          setll     pea01
     C                   read      pea01                                  99
     C*
     C                   exsr      cvtdate
     C*
     C                   eval      xxdate1 = eaepdt
     C*
     C                   dow       (*in99 = *off) and eaepdt >= xxfdat
     C                             and eaepdt <= xxtdat
     C*
     C                   exsr      keyset
     C*
     C     eoky          chain     peo01                              99
     C*
     C                   if        eaepfl = 'P' and *in99 = *off
     C                             and eoast = qqst
     C*** BEG ADD *** 05/24/04 *********************************************
     C                             and qqmode = 'D'
     C                             or eaepfl = 'P' and *in99 = *off
     C                             and qqmode = 'S'
     C*** END ADD *** 05/24/04 *********************************************
     C                   exsr      savings
     C                   endif
     C*
     C     eaky1         reade     pea01                                  99
     C*
     C                   eval      xxdate = eaepdt
     C*
     C                   exsr      cvtdate
     C*
     C                   enddo                                                  *in60=*off
     C*
     C* Print the last month and final totals
     C*
     C*
     C                   eval      mthoth2a = mthoth2 - mthcci - mthcrw
     C                   eval      tototh2 = tototh2 - totcci - totcrw
     C*
     C                   if        totchrg > 0
     C                   eval      xxccis  = (totcci/totchrg) *100
     C                   eval      xxcrws  = (totcrw/totchrg) *100
     C                   endif
     C*
     C                   if        *inoa = *on
     C                   except    head
     C                   endif
     C*
     C                   except    head1
     C                   except    monthly
     C                   except    totals
     C                   exsr      wrtfasapxx
     C*
     C                   move      *on           *inlr
     C*
     C                   Return
     C*
     C*=====================================================================
     C*    SUBROUTINE cvtdate
     C*=====================================================================
     C     cvtdate       begsr
     C*
     C                   eval      srdti = eaepdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         x1date
     C*
     C                   endsr
     C*
     C*=====================================================================
     C*    SUBROUTINE init
     C*=====================================================================
     C     init          begsr
     C*
     C                   eval      *inoa = *on
     C*
     C* Convert starting date
     C*
     C                   eval      xxfrdt = %subst(qqfdat:5:2) + '/' +
     C                               %subst(qqfdat:7:2) + '/'+
     C                               %subst(qqfdat:1:4)
     C*
     C                   eval      xxtodt = %subst(qqtdat:5:2) + '/' +
     C                               %subst(qqtdat:7:2) + '/'+
     C                               %subst(qqtdat:1:4)
     C*
     C* Get the current date
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C     *cymd         move      sys_date      sysdt
     C*
     C                   move      qqfdat        xxfdat
     C                   move      qqtdat        xxtdat
     C*
     C* Get the company number
     C*
     C                   eval      kkusr = q1user
     C*
     C     upky          chain     pup01                              99
     C*
     C                   eval      kkco# = upco#
     C                   eval      kkepdt = xxfdat
     C*
     C                   exsr      clrmth
     C*
     C                   clear                   totchrg
     C                   clear                   totmra
     C                   clear                   totndis
     C                   clear                   totothr
     C                   clear                   totcci
     C                   clear                   totcrw
     C                   clear                   tototh2
     C                   clear                   totpaid
     C                   eval      tot#bills = 0
     C*
     C                   endsr
     C*
     C*=====================================================================
     C*    KEYSET init
     C*=====================================================================
     C     keyset        begsr
     C                   eval      kkco#  = eaco#
     C                   eval      kksub# = easub#
     C                   eval      kkfnd  = eafnd
     C                   eval      kkfyr  = eafyr
     C                   eval      kkcase = eacase
     C                   eval      kkeob# = eaeob#
     C*
     C                   endsr
     C*
     C*=====================================================================
     C*    SUBROUTINE clrmth
     C*=====================================================================
     C     clrmth        begsr
     C*
     C                   clear                   mthchrg
     C                   clear                   mthmra
     C                   clear                   mthndis
     C                   clear                   mthothr
     C                   clear                   mthcci
     C                   clear                   mthcrw
     C                   clear                   mthoth2
     C                   clear                   mthoth2a
     C                   clear                   mthpaid
     C                   eval      mth#bills = 0
     C*
     C                   endsr
     C*
     C*=====================================================================
     C*    SUBROUTINE savings
     C*=====================================================================
     C     savings       begsr
     C*
     C* If different Year/Month then print and clear monthly totals
     C*
     C                   eval      xxdate = eaepdt
     C*
     C                   if        xxnewmth <> xxcurmth
     C                   eval      mthoth2a = mthoth2 - mthcci - mthcrw
     C*
     C* Print headings and monthly details
     C*
     C                   if        mth#bills > 0
     C                   if        *inoa = *on
     C                   except    head
     C                   eval      *inoa = *off
     C                   endif
     C*
     C                   except    head1
     C                   except    monthly
     C*
     C                   exsr      wrtfasapxx
     C*
     C                   endif
     C*
     C                   eval      xxcurmth = xxnewmth
     C                   exsr      clrmth
     C                   endif
     C*
     C* Read only physician bills
     C*
     C     dhky          chain     pdh01                              99
     C*
     C                   if        *in99 = *off
     C*
     C                   eval      mth#bills = mth#bills + 1
     C                   eval      tot#bills = tot#bills + 1
     C*
     C                   endif
     C*
     C                   dow       *in99 = *off
     C*** BEG ADD *** 04/26/04 *********************************************
     C*
     C                   eval      kkrcn = earcn
     C                   eval      kkdseq= dhdseq
     C*
     C     daky          chain     pda01                              99
     C*
     C                   if        *in99 = *off
     C*
     C* Monthly totals
     C*
     C                   add       dachrg        mthchrg
     C                   add       damra         mthmra
     C                   add       dandis        mthndis
     C                   add       daothr        mthothr
     C                   add       daoth2        mthoth2
     C                   add       dapaid        mthpaid
     C*
     C* Finals totals
     C*
     C                   add       dachrg        totchrg
     C                   add       damra         totmra
     C                   add       dandis        totndis
     C                   add       daothr        totothr
     C                   add       daoth2        tototh2
     C                   add       dapaid        totpaid
     C*
     C     caky          chain     pca01                              99
     C*
     C                   if        *in99 = *off
     C*
     C                   if        caot2c = 'CCI'
     C                   add       daoth2        mthcci
     C                   add       daoth2        totcci
     C                   endif
     C*
     C*** Beg Chg *** 06/11/13 *********************************************
     C****************** if        caot2c = 'CRW'
     C                   if        caot2c = 'CCK'
     C*** End Chg *** 06/11/13 *********************************************
     C                   add       daoth2        mthcrw
     C                   add       daoth2        totcrw
     C                   endif
     C*
     C                   endif
     C*
     C                   endif
     C*** END ADD *** 04/26/04 *********************************************
     C*** BEG DEL *** 04/26/04 *********************************************
     C****************   eval      kkrcn = earcn
     C****************
     C****************
     C*****caky*******   chain     pca01                              99
     C****************
     C****************   if        *in99 = *off
     C****************
     C**Monthly*totals
     C****************
     C****************   add       cachrg        mthchrg
     C****************   add       camra         mthmra
     C****************   add       candis        mthndis
     C****************   add       caothr        mthothr
     C****************   add       caoth2        mthoth2
     C****************   add       capaid        mthpaid
     C****************
     C****************   if        caot2c = 'CCI'
     C****************   add       caoth2        mthcci
     C****************   endif
     C****************
     C****************   if        caot2c = 'CRW'
     C****************   add       caoth2        mthcrw
     C****************   endif
     C****************
     C**Finals*totals*
     C****************
     C****************   add       cachrg        totchrg
     C****************   add       camra         totmra
     C****************   add       candis        totndis
     C****************   add       caothr        totothr
     C****************   add       caoth2        tototh2
     C****************   add       capaid        totpaid
     C****************
     C****************   if        caot2c = 'CCI'
     C****************   add       caoth2        totcci
     C****************   endif
     C****************
     C****************   if        caot2c = 'CRW'
     C****************   add       caoth2        totcrw
     C****************   endif
     C****************
     C****************   endif
     C*** END DEL *** 04/26/04 *********************************************
     C*
     C     dhky          reade     pdh01                                  99
     C                   enddo
     C*
     C                   endsr
     C*
     C*=====================================================================
     C*    SUBROUTINE wrtfasapxx
     C*=====================================================================
     C     wrtfasapxx    begsr
     C*
     C                   eval      saclt# = qqclt#
     C                   move      xxprtmth      zzprtmth
     C                   move      xxprtyr       zzprtyr
     C                   eval      sadate = zzprtmth + '-' + zzprtyr
     C                   eval      sa#bil = mth#bills
     C                   eval      sachrg = mthchrg
     C                   eval      samra  = mthmra
     C                   eval      sandis = mthndis
     C                   eval      saothr = mthothr
     C                   eval      saoth2 = mthoth2
     C                   eval      sapaid = mthpaid
     C                   write     psa01
     C*
     C                   endsr
     C*=====================================================================
     C*
     Oqsysprt   e            head           1 06
     O                                           77 'PROVIDER BILLS SAVINGS'
     O          e            head           4
     O                                           78 'REPORT FOR BILL TYPE = N'
     O          e            head           1
     O                                           15 'CHARGED'
     O                                           30 'MRA'
     O                                           45 'NETWORK'
     O                                           60 'OTHER 1'
     O                                           75 'CCI'
     O*** Beg Chg *** 06/11/13 *********************************************
     O************************************       90 'CRW'
     O                                           90 'CCK/CCI'
     O*** End Chg *** 06/11/13 *********************************************
     O                                          105 'OTHER 2'
     O                                          120 'PAID'
     O                                          132 '# OF BILLS'
     O          e            head           1
     O                                           15 '----------'
     O                                           30 '----------'
     O                                           45 '----------'
     O                                           60 '----------'
     O                                           75 '----------'
     O                                           90 '----------'
     O                                          105 '----------'
     O                                          120 '----------'
     O                                          132 '----------'
     O          e            head1       1  1
     O                                           73 '---------------'
     O          e            head1          1
     O                                           64 '|DATE:'
     O                       xxprtmth            67
     O                                           68 '/'
     O                       xxprtyr             72
     O                                           73 '|'
     O          e            head1          1
     O                                           73 '---------------'
     O          e            monthly     1  1
     O                       mthchrg       J     15
     O                       mthmra        J     30
     O                       mthndis       J     45
     O                       mthothr       J     60
     O                       mthcci        J     75
     O                       mthcrw        J     90
     O                       mthoth2a      J    105
     O                       mthpaid       J    120
     O                       mth#bills     J    132
     O          e            totals         2
     O                                           15 '------------'
     O                                           30 '------------'
     O                                           45 '------------'
     O                                           60 '------------'
     O                                           75 '------------'
     O                                           90 '------------'
     O                                          105 '------------'
     O                                          120 '------------'
     O                                          132 '------------'
     O          e            totals         2
     O                       totchrg       J     15
     O                       totmra        J     30
     O                       totndis       J     45
     O                       totothr       J     60
     O                       totcci        J     75
     O                       totcrw        J     90
     O                       tototh2       J    105
     O                       totpaid       J    120
     O                       tot#bills     J    132
     O          e            totals         1
     O                                           12 'DATE RANGE:'
     O                       xxfrdt              23
     O                                           26 'to'
     O                       xxtodt              37
     O          e            totals         1
     O                                           25 'CCI SAVINGS AS A PERCENT'
     O                                           37 'OF CHARGED:'
     O                       xxccis        J     45
     O          e            totals         1
     O*** Beg Chg *** 06/11/13 *******************************************
     O**************************************     25 'CRW SAVINGS AS A PERCENT'
     O                                           25 'CCK/CCI SAVINGS AS A PCT'
     O*** End Chg *** 06/11/13 *******************************************
     O                                           37 'OF CHARGED:'
     O                       xxcrws        J     45
