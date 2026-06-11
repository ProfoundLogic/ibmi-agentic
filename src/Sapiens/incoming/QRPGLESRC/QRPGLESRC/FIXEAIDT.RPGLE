     H/title FIXEAIDT - Fix Missing Initial date in WDEAp file
      *
     H*****************************************************************
     H*(C) Copyright Progressive Data Solutions, Inc., 2004
     H*****************************************************************
     H*  DESCRIPTION:  This Program reads the WDEAP file and checks   *
     H*                the EAIDT (Initial Date) for zeros. If is all  *
     H*                zeros, then checks EADT for zeros. If not all  *
     H*                zeros in EADT, loads it into EAIDT. If both    *
     H*                Fields are zero, Loads both with the current   *
     H*                date.                                          *
     H*                                                               *
     H*  CREATED: 04/23/04  CONSULTANT: C:132  PROJECT: WC1190        *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/17/04   C: 132  Project: WC1190                            *
     H*                   - If EAIDT is zero, update with the EADT    *
     H*                     which is the earliest EADT in the batch#. *
     H*                                                               *
     H*                   - If EAIDT is not zero, read through batch# *
     H*                     and see if any other date is earlier, if  *
     H*                     so store earlier date, Then go back and   *
     H*                     update the EAIDT for whole batch# with    *
     H*                     earlier date.                             *
     H*                                                               *
     H*                   - If there is no earlier date, but there    *
     H*                     is a later date, update the later dates   *
     H*                     with the current EAIDT.                   *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS:                                                   *
     H*   50  READ  - WMALP                                           *
     H*****************************************************************
     f*
     f* A/P Batch File
     fwdeal4    uf   e           k disk
     f*
     f* A/P Batch File
     fwdeal8    if   e           k disk    Rename(PEA01:PEA02) Prefix(EA2:2)
     f*
     f* Audit Report
     fqsysprt   o    f  132        printer oflind(*inob) usropn
     f*
      * ERMSG Field Has Nbr of ERR Messages Auto set in *INZSR Subr
     d err             s             43    dim(10) ctdata perrcd(1)             ERROR MESSAGES
     d txt             s             40    dim(10)                              PRINT MESSAGES
     d wrc             s              7  0 dim(10)                              PRINT MESSAGES
     d dgt             s              1    dim(13)                              DIGITS ARRAY
     d*
     d* Constants
     d*
     d lo              c                   const('abcdefghijklmnopqrs-
     d                                     tuvwxyz')
     d hi              c                   const('ABCDEFGHIJKLMNOPQRS-
     d                                     TUVWXYZ')
     d*
     d fids01        e ds                  extname(szq2p)
     d psds          esds                  extname(szq1p)
     d*
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0
     d ermsg           s              3  0 inz
     d bycnt           s              7  0 inz
     d dtcnt           s              7  0 inz
     d ercnt           s              7  0 inz
     d incnt           s              7  0 inz
     d ldcnt           s              7  0 inz
     d nocnt           s              7  0 inz
     d upcnt           s              7  0 inz
     d wrcnt           s              7  0 inz
     d x               s              3  0 inz
     d y               s              3  0 inz
     d z               s              3  0 inz
      *
     d prject          s              6
     d prnt            s              1
      *
     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
      *
      ***** BEG ADD ***** 05/17/04 *******************************
     d SVCO#           s                   LIKE(EACO#)
     d SVBAT#          s                   LIKE(EABAT#)
     d SVDT            s                   LIKE(EAIDT)
     d SVIDT           s                   LIKE(EAIDT)
     D DTflg           S              1n
     D DtChgd          S              1n
     D IDTflg          S              1n
     D Lesflg          S              1n
      ***** END ADD ***** 05/17/04 *******************************
     d*
     c***************************************************************
     c*
      * Read the WMEAP file
     c                   read      pea01                                  50
     c                   dow       not *in50
      *
      ***** BEG ADD ***** 05/17/04 *******************************
      * If break on CO# or BAT# or FND, check if dates need updating
      *
     C                   If        SVCO# <> EACO# OR SVBAT# <> EABAT#
      *
     C                   EXSR      GETDTS
      *
     C                   EVAL      SVCO#  = EACO#
     C                   EVAL      SVBAT# = EABAT#
      *
     C                   ENDIF
      ***** END ADD ***** 05/17/04 *******************************
     c                   eval      incnt = (incnt + 1)
      *
      ***** BEG ADD ***** 05/17/04 *******************************
      * If EAIDT all zeros or have different EAIDTs, update it.
      *
     c                   eval      DtChgd = *OFF
      *
     c                   if        (SVIDT <> *zeros)
      *
     c                   movel(p)  q1user        eaiusr
     c                   movel(p)  q1pgm         eaipgm
      *
     c                   select
     c                   when      (eaidt > *zeros AND eaidt = svidt) AND
     c                             LesFlg = *ON
     c                   eval      BYcnt = (BYcnt + 1)
      *
     c                   when      (eaidt > *zeros AND eaidt > svidt) AND
     c                             LesFlg = *on
     c                   eval      ldcnt = (ldcnt + 1)
     c                   eval      x = 4
     c                   eval      wrc(x) = (wrc(x) + 1)
     c                   if        prnt = 'Y' and *in91 = *on
     c   ob              except    head
     c                   except    det
     c                   endif                                                  End PRNT = 'Y'
     c                   eval      DtChgd = *ON
     c                   eval      eaidt = SVIDT
      *
     c                   when      (eaidt = *zeros AND LesFlg = *ON)
     c                   eval      ldcnt = (ldcnt + 1)
     c                   eval      x = 4
     c                   eval      wrc(x) = (wrc(x) + 1)
     c                   if        prnt = 'Y' and *in91 = *on
     c   ob              except    head
     c                   except    det
     c                   endif                                                  End PRNT = 'Y'
     c                   eval      DtChgd = *ON
     c                   eval      eaidt = SVIDT
      *
     c                   WHEN      (eaidt = *zeros) AND Not LesFlg
      *
     c                   if        eadt  = *zeros
     c                   eval      ercnt = ercnt + 1
     c                   eval      x = 3
     c                   eval      wrc(x) = (wrc(x) + 1)
     c                   if        prnt = 'Y' and *IN91 = *on
     c   ob              except    head
     c                   except    det
     c                   endif                                                  End PRNT = 'Y'
     c                   eval      DtChgd = *ON
     c                   eval      eadt  = SVIDT
     c                   eval      eaidt = SVIDT
     c                   movel(p)  q1user        eauser
     c                   movel(p)  q1pgm         eapgm
     c                   eval      eatime = dstime
      *
     c                   Else
      *
     c                   eval      wrcnt = (wrcnt + 1)
     c                   eval      x = 1
     c                   eval      wrc(x) = (wrc(x) + 1)
     c                   if        prnt = 'Y' and *in91 = *on
     c   ob              except    head
     c                   except    det
     c                   endif                                                  End PRNT = 'Y'
     c                   eval      DtChgd = *ON
      *
     c                   eval      eaidt = SVIDT
     c                   endif                                                  Eadt  = *zeros
      *
     c                   When      eadt  = *zeros AND Not DtChgd
     c                   eval      ercnt = ercnt + 1
     c                   eval      x = 2
     c                   eval      wrc(x) = (wrc(x) + 1)
     c                   if        prnt = 'Y' and *IN91 = *on
     c   ob              except    head
     c                   except    det
     c                   endif                                                  End PRNT = 'Y'
      *
     c                   eval      DtChgd = *ON
     c                   eval      eadt  = SVIDT
      *
     c                   movel(p)  q1user        eauser
     c                   movel(p)  q1pgm         eapgm
     c                   eval      eatime = dstime
      *
     c                   ENDsl
      *
     c                   if        UPDTE  = *ON AND  DtChgd = *ON
     c                   update    pea01
     c                   eval      upcnt = upcnt + 1
     c                   endif
      *
     c                   if        UPDTE  = *ON AND  DtChgd = *Off
     c                   eval      nocnt = nocnt + 1
     c                   eval      x = 5
     c                   eval      wrc(x) = (wrc(x) + 1)
     c                   if        prnt = 'Y' and *IN91 = *on
     c   ob              except    head
     c                   except    det
     c                   endif                                                  End PRNT = 'Y'
     c                   endif
      *
     c                   else
      *
     c                   eval      bycnt = bycnt + 1
     c                   goto      nxt1
     c                   endif
      *
      *
      * Get Next File Record
     c     nxt1          tag
     c                   read      pea01                                  50
     c                   enddo
     c*
      * ============================================================= *
      *  End the program & Print TOTAL Lines
      * ============================================================= *
     c     TERM          tag
      *
      *  Check For Errors and Print
      *
     c                   if        prnt  = 'Y'
      *
     c                   if        ercnt > 20 AND *IN91 = *ON
     c                   except    head
     c                   endif
     c                   except    totl
      *
     c                   eval      z = *zero
     c                   dou       (z = ermsg)
     c                   eval      z = (z + 1)
     c                   if        (wrc(z) > *zeros)
     c   ob              except    head
     c                   except    totz
     c                   endif
     c                   enddo
      *
     c                   close     qsysprt
     c                   endif                                                  End PRNT = 'Y'
      * TERMINATE Program
     c                   move      *on           *inlr
      *
      ***** BEG ADD ***** 05/17/04 *******************************
      * =========================================================== *
      *    GETDTs - check for different EAIDTs or Zero Dates
      * =========================================================== *
     c*
     c     getdts        begsr
      *
      * reset the save fields, then read second logical to get lowest dates.
      *
     c                   eval      svidt = *zeros
     c                   eval      svdt  = *zeros
     c                   eval      idtflg = *off
     c                   eval      dtflg  = *off
     c                   eval      lesflg = *off
      *
     c                   If        eadt <> *zeros AND svdt = *Zeros
     c                   eval      svdt  = eadt
      *
     c                   If        eaidt = *zeros
     c                   eval      svidt  = eadt
     c                   eval      dtflg  = *ON
     c                   endif
      *
     c                   endif
      *
     c                   If        eaidt <> *zeros AND svidt = *Zeros
     c                   eval      svidt  = eaidt
     c                   endif
      *
     c                   If        svidt  = *zeros AND svdt  = *zeros
      *
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         svidt
     c                   movel     srdto         svdt
     c                   eval      dtflg  = *ON
      *
     c                   endif
      *
     c     eaky          setll     pea02
     c     eaky          reade     pea02                                  61
      *
     c                   dow       not *IN61
      *
     c                   select
     c                   When      ea2idt <> *zeros
      * When EA2IDT is less than saved IDT, Save EA2IDT
     c                   if        ea2idt < svidt
     c                   eval      svidt = ea2idt
     c                   eval      idtflg = *on
     c                   eval      lesflg = *on
     c                   endif
      * When EA2IDT is greater than saved IDT, Set Flags on, Dont Save EA2IDT
     c                   if        ea2idt > svidt
     c                   eval      idtflg = *on
     c                   eval      lesflg = *on
     c                   endif
      *
     c                   When      ea2idt = *zeros AND ea2dt <> *zeros
     c                   if        ea2dt  < svidt
     c                   eval      svdt  = ea2dt
     c                   eval      dtflg  = *on
     c                   eval      lesflg = *on
     c                   endif
      *
     c                   endsl
      *
     c     eaky          reade     pea02                                  61
     c                   enddo
      *
      * After pass loop, get the lower date if either Flag is on
      *    Else zero out the svdt field
     c                   if        idtflg OR dtflg
     c                   if        svdt < svidt
     c                   eval      svidt = svdt
     c                   endif
      *
     c                   else
      *
     c                   eval      svidt = *zeros
     c                   endif
      *
     c                   endsr
      ***** BEG ADD ***** 05/17/04 *******************************
      ***** END ADD ***** 05/17/04 *******************************
      *
      * =========================================================== *
     c     *INZSR        begsr
      * =========================================================== *
     c*
     c* Open printer file if wanted. Use *IN91 to print detail lines
     c* If *IN91, only totals print
     c                   eval      prnt  = 'Y'
     c************       eval      prnt  = 'N'
     c                   eval      *in91 = *off
     c*
     c**********         move      *OFF          Updte             1
     c                   move      *ON           Updte             1
      *
      ***** BEG ADD ***** 05/17/04 *******************************
     C     EAKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EABAT#
      ***** END ADD ***** 05/17/04 *******************************
      *
     c*
     c                   if        updte = *on
     c                   eval      *in90 = *on
     c                   else
     c                   eval      *in90 = *off
     c                   endif
     c*
     c                   clear                   wrc                            WARN CNT ARRAY
     c                   clear                   txt                            WARN TEXT ARRAY
      *
      * Load ERMSG With Number of ERR MSG in ARRAY at Bottom of PGM
     c                   eval      ermsg = %ELEM(err)
     c                   eval      z = *zero
     c                   dou       (z = ermsg)
     c                   eval      z = (z + 1)
     c                   move      err(z)        txt(z)
     c                   enddo
      *
      * *LIKE Defines
     c     *like         define    eaco#         xxco#
     c     *like         define    eafnd         xxfnd
     c     *like         define    eaemp#        xxemp#
     c     *like         define    eadiv         xxdiv
     c     *like         define    eatime        xxtime
      *
     c                   movel(p)  'WC1190'      prject                         PROJECT CODE
     c                   time                    dsdttm
     c                   move      *blanks       xxfile           10
     c                   eval      xxfile = 'WDEAP'
     c*
     c                   if        prnt  = 'Y'
     c                   open      qsysprt
     c                   except    head
     c                   endif
      *
     c                   eval      x = 0
     c                   eval      xxtime = 0
      *
     c                   eval      xxemp# = *zeros
     c                   eval      xxdiv  = *zeros
      *
      * Date Field Conversion Parm Lists
     c     datchk        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      *blanks       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
      *
     c     dtcymd        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
      *
     c     dtmdyy        plist
     c                   parm                    srdti
     c                   parm      '*CYMD'       srffmt
     c                   parm      *blanks       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
      *
     c     dtcym2        plist
     c                   parm                    srdti
     c                   parm      '*YYMD'       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
      *
     c                   endsr
      ** ========================================================= **
     oqsysprt   e            head           1 06
     o                       dstime               8 '  :  :  '
     o                                           44 'Fix of Missing Initial '
     o                                           58 'Dates in WDEAP'
     o                  90                       73 ' - UPDATE File '
     o                 n90                       73 ' - PRINT Errors'
     o                       xxfile              86
     o                       dsdate             128 '  /  /    '
     o          e            head           2
     o                       q1pgm               10
     o                       prject            +  1
     o                       q1user             110
     o                                          123 'PAGE'
     o                       page1         z    128
     o          e            head           2
     o                                            7 'GROUP'
     o                                           17 'BATCH #'
     o                                           27 'EMPLOYER#'
     o                                           32 'DIVN'
     o                                           43 'NOTATION'
     o                                           83 'SVIDT'
     o                                           94 'EAIDT'
     o                                          104 'EADT'
     o                                          116 'PROGRAM'
     o                                          132 'In Cnt'
     o*
     o          e            det            1
     o                       eafnd         4      7
     o                       eaBAT#        3     17
     o                       eaemp#        3     27
     o                       eadiv         3     33
     o                       txt(x)              75
     o                       SVIDT               84 '   /  /  '
     o                       eaidt               95 '   /  /  '
     o                       eadt               106 '   /  /  '
     o                       eapgm              121
     o                       upcnt         3    132
     o*
     o          e            det2           1
     o                       eafnd         4      7
     o                       eaemp#        3     17
     o                       eadiv         3     23
     o                       txt(x)              65
     o                       eaidt               75 '   /  /  '
     o                       eadt                86 '   /  /  '
     o                       eaiusr              97
     o                       eaipgm             108
     o                       incnt         3    125
     o*
     o          e            totl        1  2
     o                                           30 '***** TOTALS ***** '
     o*
     o          e            totl           1
     o                                           22 'Total number of record'
     o                                           44 's read  . . . . . . . '
     o                       incnt         3     52
     o*
     o          e            totl           1
     o                                           22 'Total number records w'
     o                                           44 'ith EAIDT Not updated '
     o                       bycnt         3     52
     o*
     o          e            totl           1
     o                                           22 'Total number records u'
     o                                           44 'dated to WDEAP. . . . '
     o                       upcnt         3     52
     o*
     o          e            totl           1
     o                                           22 'Total number records w'
     o                                           44 'ith NO EAIDT  . . . . '
     o                       wrcnt         3     52
     o*
     o          e            totl           2
     o                                           22 'Total number records w'
     o                                           44 'ith NO EAIDT & EADT . '
     o                       ercnt         3     52
     o*
     o          e            totl           2
     o                                           22 'Total number records w'
     o                                           44 'ith Higher EAIDT  . . '
     o                       ldcnt         3     52
     o*
     o          e            totl           2
     o                                           22 'Total number recds w/S'
     o                                           44 'VIDT=EAIDT-Not Updated'
     o                       nocnt         3     52
     o*
     o          e            totz           1
     o                       wrc(z)        z     10
     o                       txt(z)            +  3
     o*
     o** ========================================================= **
     O*
** ERR                                  |
01-Missing EAIDT in WDEAP file-UPDATED
02-Missing EAIDT & EADT-Udpdte w/sys dte
03-Missing EADT, used EAIDT-UPDATED
04-Updated EAIDT with Lower Date
05-Have EAIDT=SVIDT=Lower Date-Not Updtd
06-
07-
08-
09-
10-
