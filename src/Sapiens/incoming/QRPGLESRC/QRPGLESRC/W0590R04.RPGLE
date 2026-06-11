     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE') bnddir('W0590R04':'WTBNDDIR')
     H*********************************************************************
     H/TITLE W0590R04 - Reserve Worksheet Summary
     H Bnddir('WTBNDDIR')
     H*********************************************************************
     H*  CREATED: 03/29/10  PROGRAMMER: C:190 PROJECT: 25WQD              *
     H*********************************************************************
     H* Revisions:                                                        *
     H*
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 08/15/17 C:33  Project: SY394                                 *
     H*                Update initial reserve flag                    *
     H*                                                                   *
     H* 06/25/14 C:062 Support WC025 85943                                *
     H*                Correct update.                                    *
     H*                                                                   *
     H* 12/15/11 C:279 Project: 25WQD, Priss: 77                          *
     H*                Modified to consider history reserve as well to    *
     H*                verify the authority limit of user.                *
     H*                                                                   *
     H* 12/09/11 C:279 Project: 25WQD                                     *
     H*                Additional Changes.                                *
     H*                                                                   *
     H* 08/25/11 C:255 SUPPORT: 77652  WA025                              *
     H*                Compilation for changes to W0590F04                *
     H*                                                                   *
     H* 10/21/10 C:183 Project: 25WQD, Priss: 58                          *
     H*                Compilation for changes to W0590F04                *
     H*                                                                   *
     H* 06/24/10 C:190 Project: 25WQD                                     *
     H*                Additional Changes.                                *
     H*                                                                   *
     H*********************************************************************
    H*
     fw0590f04  cf   e             workstn
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
    f*
    f*  Physical file definition (for maintaining record)
     fwmrwp     uf a e             disk
     f                                     rename(prw01:dbrcdf)
     fwmrwl     if   e           k disk
     fwarpl     if   e           k disk
     fwmcmp     uf   e             disk
     fwmfnl     if   e           k disk
     fwmupl     if   e           k disk
     fwmeml     if   e           k disk
     fwmcdl     if   e           k disk
     fwmrhl     if a e             DISK
     Fwtgtl     if   e           k disk
     fwtabl     if   e           k disk
     f                                     ignore(pat01)
     f                                     ignore(pb201)
     f                                     ignore(pb701)
     f                                     ignore(pba01)
     f                                     ignore(pbc01)
     f                                     ignore(pbs01)
     f*************                        ignore(pca01)
     f                                     ignore(pcc01)
     f                                     ignore(pcs01)
     f                                     ignore(pcn01)
     f                                     ignore(phy01)
     f                                     ignore(pid01)
     f                                     ignore(pis01)
     f                                     ignore(plr01)
     f*************                        ignore(pna01)
     f                                     ignore(pnc01)
     f*************                        ignore(ppa01)
     f                                     ignore(ppc01)
     f                                     ignore(prc01)
     f                                     ignore(prs01)
     f***********                          ignore(prt01)
     f                                     ignore(psp01)
     f                                     ignore(psy01)
     f                                     ignore(ptr01)
     f                                     ignore(ptt01)
     f                                     ignore(pvs01)
    d*================================================================
    d*  /COPY required constants
    d*================================================================
     D*
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
     D/copy sPrDate
     D/copy sPrString
      //***  Begin Add  ***  08/15/17  ************************************
      /copy wPrClmRsv
      //***  End   Add  ***  08/15/17  ************************************
     D/copy wPrGroup
     d optnm           c                   'Reserve Worksheet Summary'
     d comapi          c                   'WA001R'
     d #ofscr          c                   1
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     D                 ds
     D  cmss#                  1      9
     D  ss1                    1      3
     D  ss2                    4      5
     D  ss3                    6      9

     d xxco#           s                   like(cmco#)
     d xxfnd           s                   like(cmfnd)
     d xxcase          s                   like(cmcase)
     d xxfyr           s                   like(cmfyr)
     d xxacdt          s                   like(cmacdt)
     d ##co#           s              3  0
     d ##conm          s             40
     d ##fnam          s             40
     d ##fnd           s              3  0
     d @@kc            s              1
     d @@kl            s              1
     d @@mode          s              1
     d @@sts           s              1
     d @@rrn           s              9  0
     d @@rrn2          s              9  0
     d ##kc            s              1
     d ##kl            s              1
     d ##mode          s              1
     d ##rrn           s              9  0
     d ##rrn2          s              9  0
     d @@entd          s              7  0
     d @@dedt          s              7  0
     d Chkflg          s              1
     d Totrsvs         s             15  2
     d xxindr          s             15  2
     d xximdr          s             15  2
     d xxexdr          s             15  2
     d xxrr            s              9  2
     d xxupd           s              1
     D ##call          s              1
     D kkkey           s                   like(gtkey)
     D kktbid          s                   like(gttbid)

     d                 ds                  inz
     d  ar                     1     90  2
     d                                     dim(10)
     d  cmar01                 1      9  2
     d  cmar02                10     18  2
     d  cmar03                19     27  2
     d  cmar04                28     36  2
     d  cmar05                37     45  2
     d  cmar06                46     54  2
     d  cmar07                55     63  2
     d  cmar08                64     72  2
     d  cmar09                73     81  2
     d  cmar10                82     90  2

     D  ti             s              9  2 dim(10)
     D  rc             s              9  2 dim(10)

     D                                     INZ
     D  is             s              9  2 dim(3)

     D                 ds                  inz
     D  ir                     1      6
     D                                     DIM(3)
     D  xxRT01                 1      2
     D  xxRT02                 3      4
     D  xxRT03                 5      6

     d                 ds                  inz
     d  rt                     1     20
     d                                     DIM(10)
     d  cmrt01                 1      2
     d  cmrt02                 3      4
     d  cmrt03                 5      6
     d  cmrt04                 7      8
     d  cmrt05                 9     10
     d  cmrt06                11     12
     d  cmrt07                13     14
     d  cmrt08                15     16
     d  cmrt09                17     18
     d  cmrt10                19     20

     d                 ds                  inz
     d  rr                     1     90  2
     d                                     dim(10)
     d  cmrr01                 1      9  2
     d  cmrr02                10     18  2
     d  cmrr03                19     27  2
     d  cmrr04                28     36  2
     d  cmrr05                37     45  2
     d  cmrr06                46     54  2
     d  cmrr07                55     63  2
     d  cmrr08                64     72  2
     d  cmrr09                73     81  2
     d  cmrr10                82     90  2

     d                 ds                  inz
     d  rrt                    1     20
     d                                     dim(10)                              RH rsv types
     d  rhrt01                 1      2
     d  rhrt02                 3      4
     d  rhrt03                 5      6
     d  rhrt04                 7      8
     d  rhrt05                 9     10
     d  rhrt06                11     12
     d  rhrt07                13     14
     d  rhrt08                15     16
     d  rhrt09                17     18
     d  rhrt10                19     20

     d                 ds                  inz

     d  rsu                    1     90  2
     D                                     dim(10)                              RH rsv amt
     D  rhsu01                 1      9  2
     D  rhsu02                10     18  2
     D  rhsu03                19     27  2
     D  rhsu04                28     36  2
     D  rhsu05                37     45  2
     D  rhsu06                46     54  2
     D  rhsu07                55     63  2
     D  rhsu08                64     72  2
     D  rhsu09                73     81  2
     D  rhsu10                82     90  2

    c*
     C/copy sCopyMT41c
    c*================================================================
    c*  PLIST definitions
    c*================================================================
     c     *entry        plist
     c                   parm      *inkc         @@kc
     c                   parm      *inkl         @@kl
     c                   parm                    @@mode
     c                   parm                    @@rrn
     c                   parm                    @@rrn2
     c                   parm                    @@sts

     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      @@mode        ##mode
     C                   parm      @@rrn         ##rrn
     C                   parm      @@rrn2        ##rrn2

     C     acdpl2        plist
     C                   parm                    ##prds           40
     C                   parm      '2'           ##call
     C                   parm      'RSWS'        ##id              4
     C                   parm                    ##key            10
     C                   parm                    ##rtc             1

     C     acdpl3        plist
     C                   parm                    ##prds           40
     C                   parm      '2'           ##call
     C                   parm      'RSWT'        ##id              4
     C                   parm                    ##key            10
     C                   parm                    ##rtc             1
    c*
    c*  Company file/fund (group) retrieve program parameter list
    c*
     c     compl         plist
     c                   parm                    ##co#
     c                   parm                    ##fnd
     c                   parm                    ##conm
     c                   parm                    ##fnam

    C*--------------------------------------------------------------------
    C*  Key Lists                                                        *
    C*--------------------------------------------------------------------
     C*   Table file
     C     tbky          klist
     C                   kfld                    cmco#
     C                   kfld                    kkey             10

     c     rpky          klist
     c                   kfld                    cmco#
     c                   kfld                    cmfnd
     c                   kfld                    cmfyr
     c                   kfld                    cmcase
     c*
     c     cdky          klist
     c                   kfld                    cmss#


     c*  Key list for the fund master file
     c*
     c     fnky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd

     C*  Key list for the user parameter file
     C*
     C     upky          klist
     C                   kfld                    q1user
     c*
     c     emky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    cmemp#
     c                   kfld                    cmdiv

     c     rwky1         klist
     c                   kfld                    cmco#
     c                   kfld                    cmfnd
     c                   kfld                    cmfyr
     c                   kfld                    cmcase
     C*
     C     gtky          klist
     C                   kfld                    ##co#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
      *
    c*================================================================
    c*  KLIST definitions
    c*================================================================
    c*================================================================
    c*  *LIKE DEFN statements
    c*================================================================
    c*================================================================
    c* DLTPRE - Pre-processing before delete occurs
    c*================================================================
     c     dltpre        begsr
     c                   endsr
    c*================================================================
    c* DSPLYR - Processing after displaying information
    c*================================================================
     c     dsplyr        begsr
     c                   endsr                                                  dsplyr
    c*================================================================
    c* ERRCK1 - Error checking for srceen 1 (SCR01)
    c*================================================================
     c     errck1        begsr

     c                   eval      @@err = 'N'
     c*  Effective date must be valid.
     c                   if        DtCheck(scentd) = *zero
     c                   seton                                        83  90
     c                   move      'Y'           @@err             1
     c                   leavesr
     c                   endif                                                  (scst <> *blanks)
     c                   eval      scentd = DtCheck(scentd)

     c*  Reserve worksheet type
     c                   if        sctype <> 'I' and
     c                             sctype <> 'R' and
     c                             sctype <> 'S'
     c                   seton                                        84  90
     c                   move      'Y'           @@err             1
     c                   leavesr
     c                   endif

     c                   if        sctype =  'I'
     c                   move      *blanks       @@found           1
     c     rwky1         setll     prw01
     c     rwky1         reade     prw01                                  60
     c                   dow       *in60 = *off
     c                   if        sctype = 'I' and @@found = 'Y'
     c                   seton                                        85  90
     c                   leave
     c                   endif                                                  rwtype = 'I'
     c                   if        rwtype = 'I'
     c                   eval      @@found = 'Y'
     c                   endif                                                  rwtype = 'I'
     c     rwky1         reade     prw01                                  60
     c                   enddo                                                  dow *in60=*off
     C                   if        *in85 = *on
     c                   move      'Y'           @@err             1
     c                   leavesr
     c                   endif                                                  *in85 = *on
     c                   endif                                                  sctype = 'I'

     c* Reserve worksheet status
     c                   if        scsts  <> 'A' and
     c                             scsts  <> 'D' and
     c                             scsts  <> 'P'
     c                   seton                                        86  90
     c                   move      'Y'           @@err             1
     c                   leavesr
     c                   endif                                                  scsts <> 'A','D','P'

     c  n90              if        scsts  <> 'P' and scduid = *blanks
     c                   seton                                        89  90
     c                   move      'Y'           @@err             1
     c                   leavesr
     c                   endif

     c*  Rationale & comments
     c                   if        scraco = *blanks
     c                   seton                                        87  90
     c                   move      'Y'           @@err             1
     c                   leavesr
     c                   endif                                                  scsts <> 'P' and

     c                   endsr                                                  errck1
    c*================================================================
    c* ERRCK2 - Error checking for srceen 2 (SCR02)
    c*================================================================
     c     errck2        begsr
     c                   endsr
    c*================================================================
    c* ERRCK3 - Error checking for srceen 3 (SCR03)
    c*================================================================
     c     errck3        begsr
     c                   endsr
      *===============================================================
      * KJDO - Subroutine for F10
      *===============================================================
      *
     C     kjdo          begsr
      *
     C                   select
     C*
     C     hfld          wheneq    'SCTYPE'
     C                   call      'W2493R'      acdpl3
      *
     C     ##rtc         ifeq      'A'
     C                   exsr      pgmend
     C                   endif
      *
     C                   move(p)   'RSWT'        kktbid
     C                   movel(p)  ##key         kkkey
     C     gtky          chain     pgt01                              99
     C     *in99         ifeq      *off
     C                   movel(p)  gtkey         sctype
     C                   eval      *in82 = *on
     C                   endif
     C*
     C     hfld          wheneq    'SCSTS'
     C                   call      'W2493R'      acdpl2
      *
     C     ##rtc         ifeq      'A'
     C                   exsr      pgmend
     C                   endif
      *
     C                   move(p)   'RSWS'        kktbid
     C                   movel(p)  ##key         kkkey
     C     gtky          chain     pgt01                              99
     C     *in99         ifeq      *off
     C                   movel(p)  gtkey         scsts
     C                   eval      *in82 = *on

     c                   endif
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    c*================================================================
    c* MVDBSC - Move data base fields to screen fields
    c*================================================================
     c     mvdbsc        begsr

     c                   if        @@sts = 'A'
     c                   eval      scmode = '-- Add --'
     c                   endif

     C******** beg add ** 06/24/10 *********************************************
     c                   if        rwsts = 'A' or rwsts = 'D'
     c                   eval      scaww = rwaww                                Avg weekly wg
     C                   else
     C******** end add ** 06/24/10 *********************************************
     c                   eval      scaww = cmaww                                Avg weekly wg
     C******** beg add ** 06/24/10 *********************************************
     c                   endif
     C******** end add ** 06/24/10 *********************************************
     c                   if        @@sts <> 'A'
     c                   eval      scentd = DtMDYY(rwendt)                      Entry date
     c                   endif
     c                   eval      sctype = rwtype                              Reserve Wrksht type
     c                   eval      scsts  = rwsts                               Reserve Wrksht Statu
     c                   eval      scraco = rwraco                              Rationale & Comments
     c                   eval      sctmrs = rwtmrs                              Tot Med rsvs
     c                   eval      sctirs = rwtirs                              Tot Ind rsvs
     c                   eval      scters = rwters                              Tot Exp rsvs
     c                   eval      scgtrs = rwtmrs + rwtirs + rwters            Grand Tot rsvs

     c                   eval      scduid = rwduid                              Decided by
     c                   eval      scdedt = DtMDYY(rwdedt)                      Decision date

     c                   endsr                                                  mvdbsc
    c*================================================================
    c* MVSCDB - Move screen fields to data base fields
    c*================================================================
     c     mvscdb        begsr
     c                   eval      rwendt = DtCYMD(scentd)                      Entry date
     c                   eval      rwtype = sctype                              Reserve Wrksht type
     c                   eval      rwsts  = scsts                               Reserve Wrksht Statu
     c                   eval      rwtmrs = sctmrs                              Tot Med rsvs
     c                   eval      rwtirs = sctirs                              Tot Ind rsvs
     c                   eval      rwters = scters                              Tot Exp rsvs
     c                   eval      rwtars = sctmrs + sctirs + scters            Grand Tot rsvs

     c                   eval      rwraco = scraco                              Rational & Comments
     c                   eval      rwduid = scduid                              Decided by
     c                   eval      rwdedt = DtCYMD(scdedt)                      Decision date
     C******** beg add ** 06/24/10 *********************************************
     c                   eval      rwaww = scaww                                Avg weekly wg
     C******** end add ** 06/24/10 *********************************************

     c                   endsr                                                  mvscdb
    c*===============================================================
    c* OTHERR - Processing for other modes
    c*================================================================
     c     otherr        begsr
     c                   endsr
    c*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C*
     C     *inkg         caseq     *on           KgDO                     90
     C     *inkj         caseq     *ON           kjdo                     90
     C                   endcs

     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr


     C                   eval      *inlr = *on

     C                   endsr                                                  pgmend
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr

     C                   z-add     0             xxco#
     C     upky          chain     pup01                              60
     C                   if        *in60 = *off
     C                   z-add     upco#         xxco#
     C                   endif                                                  *in60 = 0

     C     @@rrn         chain     dbrcdf                             99
     C*** Begin Chg *** 06/25/14 ******************************************************
     C*****@@rrn2        chain     pcm01                              99
     C     @@rrn2        chain(n)  pcm01                              99
     C*** End   Chg *** 06/25/14 ******************************************************


     c                   eval      scsts = 'P'                                  Rsvs wrksht status
     c                   move      scsts         ##sts             1

     c****               eval      @@entd = DateTo7(SystemDate)                 Entry date
     c****               eval      scentd = DtMDYY(@@entd)                      Entry date
     c                   eval      rwendt = DateTo7(SystemDate)                 Entry date
     c                   eval      scentd = DtMDYY(rwendt)                      Entry date

     c                   move      cmfnd         xxfnd
     c                   move      cmfyr         xxfyr
     c                   move      cmcase        xxcase
     c*
     c     fnky          chain     pfn01                              60
     c                   z-add     xxfnd         scfnd
     c  n60              movel     fnname        scname

     c*  Retrieve the claimant information
     c*
      *  fyr-case
     c                   move(p)   xxfyr         xafyr             3
     c                   move(p)   XXCASE        xacase            7
     c     xafyr         cat(p)    '-':0         scyrcl
     c     scyrcl        cat       xacase:0      scyrcl
     c*
     c                   move(p)   cmco#         xaco#             3
     c                   move(p)   cmfnd         xafnd             3
     c     xaco#         cat       xafnd:0       qqkey
     c     qqkey         cat       xafyr:0       qqkey
     c     qqkey         cat       xacase:0      qqkey

      *  Claimant ss#
     c                   if        cmss# <> *blanks
      *****  Begin Chg  ***  06/09/20  **********************
     c*****ss1           cat(p)    '-':0         scss#
     c*****scss#         cat       ss2:0         scss#
     c*****scss#         cat       '-':0         scss#
     c*****scss#         cat       ss3:0         scss#
      /Free
       scss# = DisplaySS#(MaskData('SSN':'cmss#':cmss#));
      /End-free
      *****  End Chg  ***  06/09/20  **********************
     C                   endif                                                  cmss# <> *blanks
     c*  Retrieve the claimant name
     c*
     c     cdky          chain     pcd01                              60
     c                   move      *blanks       sccnam
     c                   movel     cdlsn         sccnam
     c                   if        cdlsn <> *blanks
     c     sccnam        cat       ',':0         sccnam
     C     sccnam        cat       cdfsn:1       sccnam
     C     sccnam        cat       cdmid:1       sccnam
     C                   else
     C                   movel     cdfsn         sccnam
     C     sccnam        cat       cdmid:1       sccnam
     C                   endif                                                  CDLSN <> ' '
      *
     c     emky          chain     pem01                              60
     c                   if        *in60 = *off
      *
      * Concatenate emp #, div, & name
      *
     c                   call      'WA005R'
     c                   parm                    cmemp#
     c                   parm                    cmdiv
     c                   parm                    emname
     c     scedn         parm                    xxedn            80
     c                   endif                                                  *IN60='0'
     c*
     c                   eval      scacdt = DtMDYY(cmacdt)
     c*
     c                   move      *blank        scscl#
     c                   movel     cmacl#        scscl#


     c                   endsr                                                  pgmstr
    c*================================================================
    c* PREOTH - Pre-processing for other modes
    c*================================================================
     c     preoth        begsr
     c                   endsr
    c*================================================================
    c* UPDATR - Update data base record and after processing
    c*================================================================
     c     updatr        begsr

     c     @@rrn         chain     dbrcdf                             99
B01  c                   if        *in99 = *off
     c                   exsr      mvscdb
     c                   update    dbrcdf
     c                   exsr      MaintFlds
E01  c                   endif                                                  *in99 = *off

     c                   endsr                                                  updatr
    c*================================================================
    c* WRITER - Write data base record and after processing
    c*================================================================
     c     writer        begsr
     c                   clear                   dbrcdf
     c                   exsr      mvscdb
     c                   exsr      MaintFldsInit
     c                   write     dbrcdf
     c                   feod      wmrwp

     c                   endsr
    c*================================================================
    c* Load Maintenance Fields
    c*================================================================
     c     MaintFlds     begsr

     c                   eval      rwdt   = DateTo7(SystemDate)
     c                   eval      rwtm   = TimeTo6(SystemTime)                 Maintaince time
     c                   eval      rwuser = q1user                              Maintaince user
     c                   eval      rwpgm  = q1pgm                               Maintaince Pgm

     c                   endsr                                                  MaintFlds

    c*================================================================
    c* Load Initial Maintenance Fields
    c*================================================================
     c     MaintFldsInit begsr

     c                   exsr      MaintFlds

     c                   eval      rwidt  = rwdt                                Maintaince date
     c                   eval      rwitm  = rwtm                                Initial Time
     c                   eval      rwiusr = rwuser                              Initial User
     c                   eval      rwipgm = rwpgm                               Initial pgm.


     c                   endsr                                                  MaintFldsInit

     c*===============================================================
     c* KGDO - SUBROUTINE FOR F7
     c*===============================================================
     c     kgdo          begsr
     c******** beg add ** 06/24/10 *********************************************
     c                   if        @@mode = 'D'
     c                   seton                                        92  90
     c                   else
     c******** end add ** 06/24/10 *********************************************

     c                   if        scsts = 'P'
     c                   seton                                        88  90

     c                   else
     c******** beg add ** 06/24/10 *********************************************
     c                   exsr      Errck1
     c                   if        @@err <> 'Y'
     c******** end add ** 06/24/10 *********************************************

     c* If the user changes the 'reserve worksheet status' from 'P' to 'A'.
     c                   if        ##sts = 'P' and scsts = 'A'
     c                   exsr      chkAuthority

     c                   if        Chkflg =   'Y'
     c                   Dou       *in90 = *off
     c                   exfmt     wdw01

     c                   eval      *in90 = *off
     c                   eval      *in73 = *off

     c                   if        sccon <> 'Y'and
     c                             sccon <> 'N'and
     c                             *inkl <> *on
     c                   seton                                        73  90
     c                   iter
     c                   endif                                                  sccon <> 'Y'and


     c                   if        sccon = 'Y'
     c                   exsr      UpdRmnRsvs
     c                   else
     c                   eval      scsts = ##sts
     c                   endif                                                  sccon = 'Y'

     c                   enddo                                                  *in90 = *off

     c                   if        sccon = 'N'
     c                   eval      *in90 = *on
     c                   eval      sccon = *blanks
     c                   endif

     c                   if        *inkl = *on
     c                   eval      *in90 = *on
     c                   endif                                                  *inkl = *on


     c                   else
     c                   exfmt     wdw02
     c                   exsr      WriteDarts
     c                   eval      scsts = ##sts
     c                   endif                                                  chkflg = 'Y'

     c                   endif                                                  scsts = 'A'

     c* If the user changes the 'reserve worksheet status' from 'P' to 'D'.
     c                   if        ##sts = 'P' and scsts = 'D'
     c                   exsr      chkAuthority

     c                   if        chkflg = 'Y'
     c                   Dou       *in90 = *off
     c                   exfmt     wdw03

     c                   eval      *in90 = *off
     c                   eval      *in73 = *off

     c                   if        sccon <> 'Y'and
     c                             sccon <> 'N'and
     c                             *inkl <> *on
     c                   seton                                        74  90
     c                   iter
     c                   endif                                                  sccon <> 'Y'and

     c                   if        sccon = 'Y'
     c                   exsr      NotUpdRmnRsvs
     c                   else
     c                   eval      scsts = ##sts
     c                   endif                                                  sccon = 'Y'

     c                   enddo                                                  *in90 = *off

     c                   if        sccon = 'N'
     c                   eval      *in90 = *on
     c                   eval      sccon = *blanks
     c                   endif

     c                   if        *inkl = *on
     c                   eval      *in90 = *on
     c                   endif                                                  *inkl = *on

     c                   else
     c                   exfmt     wdw04
     c                   exsr      WriteDarts
     c                   eval      scsts = ##sts
     c                   endif                                                  chkflg = 'Y'

     c                   endif                                                  scsts = 'D'

     c******** beg add ** 06/24/10 *********************************************
     c                   endif                                                  errck1<>'Y'
     c******** end add ** 06/24/10 *********************************************
     c                   endif

     c******** beg add ** 06/24/10 *********************************************
     c                   endif
     c******** end add ** 06/24/10 *********************************************
     c                   endsr                                                  kgdo

     c*===============================================================
     c* ChkAuthority - Check authority to approve the reserve payment
     c*                worksheet.
     c*===============================================================
     c     ChkAuthority  begsr

      *  Do not allow the user to exceed their reserve authority
      *    limit.
      *
     c*  For indemnity reserves
     c                   move      'Y'           Chkflg
      *
     c******** beg chg ** 12/09/11 *********************************************
     c******             if        rwtair <> 0
     c                   if        rwtaic <> 0
     c******** end chg ** 12/09/11 *********************************************
     c                   move      'CO'          @@type
     c                   exsr      callsrrsvl
     c     tbky          chain     prt01                              60
     c******** beg chg ** 12/09/11 *********************************************
     c******             if        qqlmt < rwtair
     c                   if        qqlmt < rwtaic
     c******** end chg ** 12/09/11 *********************************************
     c                   move      'N'           Chkflg
     c                   leavesr
     c                   endif
     c                   endif                                                  rwtair <> 0

     c*  For medical reserves
     c******** beg chg ** 12/09/11 *********************************************
     c******             if        rwtamr <> 0
     c                   if        rwtamc <> 0
     c******** end chg ** 12/09/11 *********************************************
     c                   move      'ME'          @@type            2
     c                   exsr      callsrrsvl
     c******** beg chg ** 12/09/11 *********************************************
     c******             if        qqlmt < rwtamr
     c                   if        qqlmt < rwtamc
     c******** end chg ** 12/09/11 *********************************************
     c                   move      'N'           Chkflg
     c                   leavesr
     c                   endif
     c                   endif                                                  rwtamr <> 0
      *
     c*  For expense reserves
     c******** beg chg ** 12/09/11 *********************************************
     c******             if        rwtaer <> 0
     c                   if        rwtaec <> 0
     c******** end chg ** 12/09/11 *********************************************
     c                   move      'AE'          @@type            2
     c                   exsr      callsrrsvl
     c******** beg chg ** 12/09/11 *********************************************
     c******             if        qqlmt < rwtaer
     c                   if        qqlmt < rwtaec
     c******** end chg ** 12/09/11 *********************************************
     c                   move      'N'           Chkflg
     c                   leavesr
     c                   endif
     c                   endif                                                  rwtaer <> 0
      *
      *  Total reserve change may not exceed the user's reserve
      *    authority limit.
      *
     c******** beg chg ** 12/09/11 *********************************************
     c******             eval      Totrsvs = rwtamr + rwtair + rwtaer
     c                   eval      Totrsvs = rwtamc + rwtaic + rwtaec
     c******** end chg ** 12/09/11 *********************************************
     c******** beg chg ** 12/15/11 *********************************************
     c********           if        Totrsvs > upral and
     c********                     upral <> 0
     c* if commulative authority limit is non-zero then check for total
     c* reserve limit as well. qqlmt will be same for all type codes.
     c                   if        upral <> 0
     c                   move      'ME'          @@type            2
     c                   exsr      callsrrsvl
     c                   if        qqlmt < Totrsvs
     c******** end chg ** 12/15/11 *********************************************
     c                   move      'N'           Chkflg
     c                   leavesr
     c                   endif
     c******** beg add ** 12/15/11 *********************************************
     c                   endif
     c******** end add ** 12/15/11 *********************************************

     c* Reserves cannot be less than 0
     c                   if        sctmrs < 0 or
     c                             sctirs < 0 or
     c                             scters < 0
     c                   move      'N'           Chkflg
     c                   leavesr
     c                   endif

     c                   endsr
     c*===============================================================

     c* UpdRmnRsvs - Update Remaning Reserve
     c*===============================================================
     c     UpdRmnRsvs    begsr

     C*** Begin Add *** 06/25/14 ******************************************************
     C     @@rrn2        chain     pcm01                              99
     C*** End   Add *** 06/25/14 ******************************************************
     c                   if        sctmrs <> 0
     c                   move      'ME'          ir(1)
     c                   move      sctmrs        is(1)
     c                   endif

     c                   if        sctirs <> 0
     c                   move      'CO'          ir(2)
     c                   move      sctirs        is(2)
     c                   endif

     c                   if        scters <> 0
     c                   move      'AE'          ir(3)
     c                   move      scters        is(3)
     c                   endif
      *
      *  Loop thru codes
      *
     c                   eval      xxupd = ' '
     c                   do        3             i                 2 0

     c                   if        ir(i) <> *blanks
      *
     c                   z-add     1             x
     c     ir(i)         lookup    rt(x)                                  60
      *
     c                   if        *in60
     c                   eval      ti(x) = rr(x) + ar(x)
     c                   eval      xxrr = rr(x) + (is(i) - ti(x))
     c                   eval      rc(x) = xxrr - rr(x)
     c                   else
      *
     c* if not found load reserve to file
     c                   z-add     1             x
     c     *blanks       lookup    rt(X)                                  60
     c                   if        *in60
     c                   eval      rt(x) = ir(i)
     c                   eval      rc(x) = is(i)
     c                   eval      xxrr = is(i)
     c                   eval      ar(x) = 0
     c                   endif                                                  *in60
      *
     c                   endif                                                  *in60
      *
     c                   if        rc(x) <> 0
     c                   eval      xxupd = 'Y'
     c                   eval      rr(x) = xxrr
     c                   endif
      *
     c                   endif
      *
     c                   enddo
      *
     c                   if        xxupd = 'Y'
      *
     c                   move      q1user        cmuser
     c                   move      q1pgm         cmpgm
      *
     c                   time                    dsdttm
     c                   z-add     dstime        cmtime
     c                   eval      cmdt  = DateTo7(SystemDate)
      *
     c                   update    pcm01
      *
      *  Update Reserve History
      *
     C                   move      cmco#         rhco#
     C                   move      cmfnd         rhfnd
     C                   z-add     cmfyr         rhfyr
     C                   move      cmcase        rhcase
     C                   clear                   rrt
     C                   clear                   rsu
      *
     c                   do        10            x                 3 0
     c                   if        rc(x) <> 0
     c                   move      rt(x)         rrt(x)
     c                   z-add     rc(x)         rsu(x)
     c                   endif
     c                   enddo

     c                   eval      rhefdt = DateTo7(SystemDate)
     c                   move      *blanks       rhstat
     c                   move      *blanks       rhclst
     C                   move      q1user        rhuser
     C                   move      q1pgm         rhpgm
     C                   time                    dsdttm
     C                   z-add     dstime        rhtime
      *
     c                   eval      rhdt  = DateTo7(SystemDate)
     c                   eval      rhidt = DateTo7(SystemDate)
      *
     c                   move      q1user        rhiusr
     c                   move      q1pgm         rhipgm
      *
     c                   z-add     dstime        rhitim
      //***  Begin Add  ***  08/15/17  ************************************
      /Free
       RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);
      /End-free
      //***  End   Add  ***  08/15/17  ************************************

      *  WRITE HISTORY RESERVE
      *
     C                   write     PRH01
     c                   endif                                                  xxupd = 'Y'

     c                   eval      scsts = 'A'
     c                   eval      scduid = q1user
     c                   eval      @@dedt = DateTo7(SystemDate)
     c                   eval      scdedt = DtMDYY(@@dedt)

     c                   endsr

     c*===============================================================
     c* NotUpdRmnRsvs - Do Not Update Remaning Reserve
     c*===============================================================
     c     NotUpdRmnRsvs begsr

     c                   eval      scsts = 'D'
     c                   eval      scduid = q1user
     c                   eval      @@dedt = DateTo7(SystemDate)
     c                   eval      scdedt = DtMDYY(@@dedt)

     c                   endsr

     c*===============================================================
     c* WriteDarts - Sent a Darts dairy
     c*===============================================================
     c     Writedarts    begsr
     C*

     C                   call      'SC001R'
     C                   parm      optnm         qqprds           40
     C                   parm      '2'           qqcall            1
     C                   parm                    qqkey            40
     C                   parm      'W0070R'      qqpnam           10
     C                   parm      qqkey         qqfold           80
     C                   parm      *BLANKS       qqdcid           12
     C                   parm                    qqdate            8
     C                   parm      'CLRWR'       qqrmcd            5
     C                   parm                    qqcom            60
     C                   parm      *blanks       qqdusr           10
     C                   parm      'Y'           qqrfld            1
     C                   parm      'Y'           qqrdoc            1
     C                   parm      *blanks       qqrdt             1
     C                   parm      'Y'           qqrrmc            1
     C                   parm      *blanks       qqrcom            1
     C                   parm      *blanks       qqradd            1
     C                   parm      *blanks       qqrdty            1
     C                   parm      *blanks       qqrdus            1

     c                   endsr                                                  Writedarts

     c*===============================================================
     c* CallSrrsvl -
     c*===============================================================
     C     CALLSRRSVL    begsr
     c                   z-add     *zeros        qqlmt
     c                   call      'SRRSVL'
     c                   parm                    xxco#
     c                   parm                    xxfnd
     c                   parm                    cmfyr
     c                   parm                    cmcase
     c                   parm                    @@TYPE
     c                   parm                    qqlmt            11 2
     C                   endsr
    C*====================================================================
    C*  SrReserve  Load Reserve Info
    C*====================================================================
     C     SrReserve     begsr
      *
      *

     c                   endsr                                                  SrReserve
    C*================================================================
