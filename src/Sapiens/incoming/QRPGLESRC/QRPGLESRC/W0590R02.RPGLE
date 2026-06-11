     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE') bnddir('W0590R02')
     H*********************************************************************
     H/TITLE W0590R02 - Anticipated Indemnity Payment Reserve Worksheet
     H Bnddir('WTBNDDIR')
     H*********************************************************************
     H*  CREATED: 03/29/10  PROGRAMMER: C:190 PROJECT: 25WQD              *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 12/09/11 C:279 Project: 25WQD                                     *
     H*                Modified for Total Indemnity Reserve Change.       *
     H*                                                                   *
     H* 08/05/10 C:190 Project: 25WQD                                     *
     H*                Additional Changes.                                *
     H*                                                                   *
     H* 06/24/10 C:190 Project: 25WQD                                     *
     H*                Additional Changes.                                *
     H*                                                                   *
     H*********************************************************************
    H*
     fw0590f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    f*
    f*  Physical file definition (for maintaining record)
     fwmrwp     uf a e             disk
     f                                     rename(prw01:dbrcdf)
     fwmcmp     if   e             disk
     fwmfnl     if   e           k disk
     fwmupl     if   e           k disk
     fwmeml     if   e           k disk
     fwmcdl     if   e           k disk
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
     f************                         ignore(prt01)
     f                                     ignore(psp01)
     f                                     ignore(psy01)
     f                                     ignore(ptr01)
     f                                     ignore(ptt01)
     f                                     ignore(pvs01)
    d*================================================================
    d*  /COPY required constants
    d*================================================================
     D*
     D/copy sPrDate
     D/copy sPrString
     D/copy wPrGroup
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
     d optnm           c                   'Anticipated Indemnity Reserve Works-
     d                                     heet'
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
     d ##sts           s              1
     d @@rrn           s              9  0
     d @@rrn2          s              9  0
     d xxiedr          s             15  2
     d xxiedp          s             15  2
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D ##rrn2          s              9  0
     D @@con           s              1
     D @@wtd75         s              3  0
     D @@wtd70         s              5  0
     D @@weeks         s              5  0

     d                 ds                  inz
     d rr                      1     90  2
     d                                     dim(10)                              RR REMAIN RSV
     d cmrr01                  1      9  2
     d cmrr02                 10     18  2
     d cmrr03                 19     27  2
     d cmrr04                 28     36  2
     d cmrr05                 37     45  2
     d cmrr06                 46     54  2
     d cmrr07                 55     63  2
     d cmrr08                 64     72  2
     d cmrr09                 73     81  2
     d cmrr10                 82     90  2

     d                 ds                  inz
     d ar                      1     90  2
     d                                     dim(10)                              RR REMAIN RSV
     d cmar01                  1      9  2
     d cmar02                 10     18  2
     d cmar03                 19     27  2
     d cmar04                 28     36  2
     d cmar05                 37     45  2
     d cmar06                 46     54  2
     d cmar07                 55     63  2
     d cmar08                 64     72  2
     d cmar09                 73     81  2
     d cmar10                 82     90  2

     d                 ds                  inz
     d rt                      1     20
     d                                     dim(10)                              RT RSV TYPES
     d cmrt01                  1      2
     d cmrt02                  3      4
     d cmrt03                  5      6
     d cmrt04                  7      8
     d cmrt05                  9     10
     d cmrt06                 11     12
     d cmrt07                 13     14
     d cmrt08                 15     16
     d cmrt09                 17     18
     d cmrt10                 19     20

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
     c                   parm                    @@chk             1
     c                   parm                    @@mode1           1

     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      @@mode        ##mode
     C                   parm      @@rrn         ##rrn
     C                   parm      @@rrn2        ##rrn2
     C                   parm      @@sts         ##sts
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

     c*  Key list for the claimant demographics file
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
    c*================================================================
    c*  KLIST definitions
    c*================================================================
    c*================================================================
    c*  *LIKE DEFN statements
    c*================================================================
     C*===============================================================
     C* CALLR - SUBROUTINE FOR CALLING THE NEXT PROGRAM
     C*===============================================================
     C*
     C     callr         begsr

     c                   if        @@con <> 'N'
     c                   if        @@chk =  'Y'
     c                   eval      @@mode = 'C'
     c                   endif
     c                   if        @@chk =  'Y' and @@mode1 = 'A'
     c                   eval      @@sts = 'A'
     c                   endif
     C                   call      'W0590R03'    ACDPL                  99
     C                   if        *inkl = '1'
     C                   eval      *in90 = '1'
     c                   endif
     c                   endif

     C                   endsr                                                  callr
    c*================================================================
    c* DLTPRE - Pre-processing before delete occurs
    c*================================================================
     c     dltpre        begsr
     c                   endsr
    c*================================================================
    c* DSPLYR - Processing after displaying information
    c*================================================================
     c     dsplyr        begsr

     c                   z-add     1             scrn#             1 0

     c                   if        scrn# = #ofscr
     c                   exsr      callr
     c                   endif

     c                   endsr                                                  dsplyr
    c*================================================================
    c* ERRCK1 - Error checking for srceen 1 (SCR01)
    c*================================================================
     c     errck1        begsr

     C******** beg add ** 08/05/10 *********************************************
     c  n90              if        sctb75 <  *zeros
     C                   seton                                        45  90
     c                   endif
     C******** end add ** 08/05/10 *********************************************

     c  n90              if        sctw75 <  *zeros
     C                   seton                                        31  90
     c                   endif

     c  n90              if        sctd75 <  *zeros
     C                   seton                                        32  90
     c                   endif

     C******** beg add ** 08/05/10 *********************************************
     c  n90              if        scta70 <  *zeros
     C                   seton                                        46  90
     c                   endif
     C******** end add ** 08/05/10 *********************************************
     c  n90              if        sctw70 <  *zeros
     C                   seton                                        33  90
     c                   endif

     c  n90              if        sctd70 <  *zeros
     C                   seton                                        34  90
     c                   endif

     c  n90              if        scipct <  *zeros
     C                   seton                                        35  90
     c                   endif

     c  n90              if        sciamt <  *zeros
     C                   seton                                        36  90
     c                   endif

     c  n90              if        sclamt <  *zeros
     C                   seton                                        37  90
     c                   endif

     c  n90              if        scsmth <  *zeros
     C                   seton                                        38  90
     c                   endif

     c  n90              if        scsamt <  *zeros
     C                   seton                                        39  90
     c                   endif

     c  n90              if        scdwks <  *zeros
     C                   seton                                        42  90
     c                   endif

     c  n90              if        scdamt <  *zeros
     C                   seton                                        43  90
     c                   endif

     c  n90              if        sctipd <  *zeros
     C                   seton                                        44  90
     c                   endif


     c                   eval      @@err = 'N'

     c                   if        sctw75 > 26
     c                   seton                                        52  90
     c                   move      'Y'           @@err             1
     c                   leavesr
     c                   endif                                                  TIBS weeks @ 75%

     c                   if        sctw75 >= 26 and sctd75 <> 0
     c                   seton                                        58  90
     c                   move      'Y'           @@err             1
     c                   leavesr
     c                   endif                                                  TIBS days @ 75%

     c                   if        sctd75 > 6
     c                   seton                                        53  90
     c                   move      'Y'           @@err             1
     c                   leavesr
     c                   endif                                                  TIBS days @ 75%

     c                   if        sctw70 <> *zeros
     c                   eval      @@wtd75 = (sctw75 * 7) + sctd75

     c                   eval      @@wtd70 = (sctw70 * 7) + sctd70

     c                   eval      @@weeks = @@wtd75+ @@wtd70
     c                   if        @@weeks > 735
     c                   seton                                        56  90
     c                   move      'Y'           @@err             1
     c                   leavesr
     c                   endif                                                  TIBS weeks @ 70%
     c                   endif                                                  TIBS weeks @ 70%

     c                   if        sctd70 > 6
     c                   seton                                        59  90
     c                   move      'Y'           @@err             1
     c                   leavesr
     c                   endif                                                  TIBS days @ 75%


     c                   if        sctd70 > 7
     c                   seton                                        55  90
     c                   move      'Y'           @@err             1
     c                   leavesr
     c                   endif                                                  TIBS days @ 70%

     c                   if        scipct > 100
     c                   seton                                        57  90
     c                   move      'Y'           @@err             1
     c                   leavesr
     c                   endif                                                  IIBS %

     c                   if        sctw75=  *zeros and
     c                             sctd75 = *zeros and
     c                             sctw70 = *zeros and
     c                             sctd70 = *zeros and
     c                             scipct = *zeros and
     c                             sciamt = *zeros and
     c                             sclamt = *zeros and
     c                             scsmth = *zeros and
     c                             scsamt = *zeros and
     c                             scdwks = *zeros and
     c                             scdamt = *zeros and
     c                             sctipd = *zeros and
     c                             sctb75 = *zeros and
     c                             scta70 = *zeros

     c                   if        @@scnd <> 'Y'
     c                   if        @@chk <> 'Y'
     c
     c                   Dou       *in90 = *off
     c                   exfmt     wdw01

     c                   eval      *in90 = *off
     c                   eval      *in73 = *off

     c                   if        sccon <> 'Y'and
     c                             sccon <> 'N'and
     c                             *inkl <> *on
     C                   seton                                        73  90
     c                   endif

     c                   enddo                                                  *in90 = *off

     c                   if        *inkl = *on or sccon = 'N'
     c                   eval      *in90 = *on
     c                   eval      sccon = *blanks
     c                   endif
     c                   endif
     c                   endif
     c                   endif
     c                   eval      @@scnd = *blanks
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
    c*================================================================
    c* MVDBSC - Move data base fields to screen fields
    c*================================================================
     c     mvdbsc        begsr

     c                   if        @@sts = 'A'
     c                   eval      scmode = '-- Add --'
     c                   endif

     c                   eval      sctw75 = rwtw75                              TIBS wks 75%
     c                   eval      sctd75 = rwtd75                              TIBS dys 75%
     c                   eval      sctw70 = rwtw70                              TIBS wks 70%
     c                   eval      sctd70 = rwtd70                              TIBS dys 70%
     c                   eval      scipct = rwipct                              IIBS %
     c                   eval      sciamt = rwiamt                              IIBS amt
     c                   eval      scsmth = rwsmth                              SIBS # months
     c                   eval      scsamt = rwsamt                              SIBS amt
     c                   eval      sclamt = rwlamt                              LIBS amt
     c                   eval      scdwks = rwdwks                              DIBS # wks
     c                   eval      scdamt = rwdamt                              DIBS amt
     c                   eval      sctipd = rwtipd                              Tot intrest ptd

     C******** beg add ** 06/24/10 *********************************************
     c                   if        rwsts = 'A' or rwsts = 'D'
     c                   eval      sccirs = rwcirs                              Curr Indem Rsvs
     c                   eval      sctnpd = rwtnpd                              Tot Indem PTD
     c                   eval      scaww = rwaww                                Avg weekly wg
     c                   eval      sctair = rwtair                              Tot Ant Indm rsvs
     c                   eval      sctirs = rwtirs                              Tot Indm rsvs
     c                   eval      sctb75 = rwtb75                              TIBS amt 75%
     c                   eval      scta70 = rwta70                              TIBS amt 70%
     c                   else
     C******** end add ** 06/24/10 *********************************************
     c                   eval      scaww = cmaww                                Avg weekly wg
     c                   if        cmltfl <> 'M'
     C******** beg dlt ** 08/05/10 *********************************************
     c********           eval      rwtb75  = (cmaww * 75)/100
     c********           eval      rwta70  = (cmaww * 70)/100
     C******** end dlt ** 08/05/10 *********************************************
     c                   eval      sctb75 = rwtb75                              TIBS amt 75%
     c                   eval      scta70 = rwta70                              TIBS amt 70%
     c                   endif
     c                   exsr      SrReserve
     c                   eval      sccirs = xxiedr                              Curr Indem Rsvs
     c                   eval      sctnpd = xxiedp                              Tot Indem PTD
     C******** beg add ** 06/24/10 *********************************************
     c                   endif
     C******** end add ** 06/24/10 *********************************************


     c                   eval      sctt75 = rwtt75                              TIBS 75% Total
     c                   eval      sctt70 = rwtt70                              TIBS 70% Total
     c                   eval      sciwks = rwiwks                              IIBS wks
     c                   eval      scitot = rwitot                              IIBS Total
     c                   eval      scstot = rwstot                              SIBS Total
     c                   eval      scdtot = rwdtot                              DIBS Total

     c                   endsr                                                  mvdbsc
    c*================================================================
    c* MVSCDB - Move screen fields to data base fields
    c*================================================================
     c     mvscdb        begsr

     c                   if        sccon <> 'N'

     C******** beg add ** 06/24/10 *********************************************
     c                   eval      rwtb75 = sctb75                              TIBS amt 75%
     c                   eval      rwta70 = scta70                              TIBS amt 70%
     C******** beg add ** 06/24/10 *********************************************
     c                   eval      rwtw75 = sctw75                              TIBS wks 75%
     c                   eval      rwtd75 = sctd75                              TIBS dys 75%
     c                   eval      rwtw70 = sctw70                              TIBS wks 70%
     c                   eval      rwtd70 = sctd70                              TIBS dys 70%
     c                   move      *zeros        @@ipct            9 0
     c                   eval(h)   @@ipct = scipct                              IIBS %
     c                   eval      rwipct = @@ipct                              IIBS %
     c                   eval      scipct = rwipct                              IIBS %
     c                   eval      rwiamt = sciamt                              IIBS amt
     c                   eval      rwsmth = scsmth                              SIBS # months
     c                   eval      rwsamt = scsamt                              SIBS amt
     c                   eval      rwlamt = sclamt                              LIBS amt
     c                   eval      rwdwks = scdwks                              DIBS # wks
     c                   eval      rwdamt = scdamt                              DIBS amt
     c                   eval      rwtipd = sctipd                              Tot intrest ptd

     c                   exsr      kgdo                                         Calculate flds
     C******** beg add ** 06/24/10 *********************************************
     c                   eval      rwcirs = sccirs                              Curr Indem Rsvs
     c                   eval      rwtnpd = sctnpd                              Tot Indem PTD
     c                   eval      rwaww = scaww                                Avg weekly wg
     C******** end add ** 06/24/10 *********************************************

     c                   eval      rwtt75 = sctt75                              TIBS @ 75% Total
     c                   eval      rwtt70 = sctt70                              TIBS @ 70% Total
     c                   eval      rwiwks = sciwks                              IIBS Wks
     c                   eval      rwitot = scitot                              IIBS total
     c                   eval      rwstot = scstot                              SIBS total
     c                   eval      rwdtot = scdtot                              DIBS total
     c                   eval      rwtair = sctair                              Tot Ant Indm rsvs
     c                   eval      rwtirs = sctirs                              Tot Indm rsvs
     C******** beg add ** 12/09/11 *********************************************
     c                   eval      rwtaic = sctt75 + sctt70 + scitot +          Curr Med rsvs
     c                                      scstot + sclamt + scdtot            Tot Med PTD
     C******** end add ** 12/09/11 *********************************************

     c                   endif                                                  sccon <> 'N'

     c                   eval      @@con = sccon
     c                   eval      sccon = *blanks

     c                   endsr                                                  mvscdb
    c*================================================================
    c* OTHERR - Processing for other modes
    c*================================================================
     c     otherr        begsr
     c                   endsr
    c*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr

     C     *inkg         caseq     *on           KgDO                     90
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

     C     @@rrn2        chain     pcm01                              99

     C                   move      cmfnd         xxfnd
     C                   move      cmfyr         xxfyr
     C                   move      cmcase        xxcase
     C*
     C     fnky          chain     pfn01                              60
     C                   z-add     xxfnd         scfnd
     C  n60              movel     fnname        scname

     C*  Retrieve the claimant information
     C*
      *  fyr-case
     c                   move(p)   xxfyr         xafyr             3
     c                   move(p)   XXCASE        xacase            7
     c     xafyr         cat(p)    '-':0         scyrcl
     c     scyrcl        cat       xacase:0      scyrcl
     c*
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

     C                   endsr                                                  pgmstr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
     C                   exsr      MaintFlds
E01  C                   endif

     C                   exsr      Callr

     C                   endsr                                                  updatr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   exsr      MaintFldsInit
     C                   write     dbrcdf
     C                   feod      wmrwp

     C                   exsr      Callr

     C                   endsr
    C*====================================================================
    C*  SrReserve  Load Reserve Info
    C*====================================================================
     C     SrReserve     begsr

     C                   eval      xxiedr = *zeros
     C                   eval      xxiedp = *zeros

     C                   z-add     0             i                 2 0

     C                   for       i = 1 to 10

     c                   movel     rt(i)         kkey

     c     tbky          chain     prt01                              60
     c                   if        *in60 = *off
     C                   if        rttype = 'C'
     c                   eval      xxiedr = rr(i) + xxiedr                      Curr Indemnity Rsvs
     c                   eval      xxiedp = ar(i) + xxiedp                      Tot Indem PTD

     c                   endif                                                  rttype = 'M'
     c                   endif                                                  *in60 = *off

     c                   endfor

     c                   eval      sctt75 = (sctb75 * sctw75) +
     c                                      ((sctb75/7) * sctd75)               TIBS @ 75% Total

     c                   eval      sctt70 = (scta70 * sctw70) +                 TIBS @ 70% Total
     c                                      ((scta70/7) * sctd70)

     c                   eval      sciwks = scipct * 3                          IIBS wks
     c                   eval      scitot = sciwks * sciamt                     IIBS total

     c                   eval      scstot = scsmth * scsamt                     SIBS total

     c                   eval      scdtot = scdwks * scdamt                     DIBS total

     c                   eval      sctair = sctt75 + sctt70 + scitot +          Tot Ant Indm rsvs
     c                                      scstot + scdtot + xxiedr + sclamt

     c                   eval      sctirs = sctair + sctipd + xxiedp            Tot Indm rsvs

     c                   endsr                                                  SrReserve
    C*================================================================
    c* Load Maintenance Fields
    c*================================================================
     c     MaintFlds     begsr

     c                   eval      rwdt   = DateTo7(SystemDate)
     c                   eval      rwtm   = TimeTo6(SystemTime)                 Maintaince time
     c                   eval      rwuser = q1user                              Maintaince user
     c                   eval      rwpgm  = q1pgm                               Maintaince Pgm

     c                   endsr

    c*================================================================
    c* Load Initial Maintenance Fields
    c*================================================================
     c     MaintFldsInit begsr

     c                   exsr      MaintFlds

     c                   eval      rwidt  = rwdt                                Maintaince date
     c                   eval      rwitm  = rwtm                                Initial Time
     c                   eval      rwiusr = rwuser                              Initial User
     c                   eval      rwipgm = rwpgm                               Initial pgm.

     c                   endsr
     c*===============================================================
     c* KGDO - SUBROUTINE FOR F7
     c*===============================================================
     c     kgdo          begsr
     c******** beg add ** 06/24/10 *********************************************
     c                   if        @@mode = 'D'
     c                   seton                                        88  90
     c                   else
     c******** end add ** 06/24/10 *********************************************

     c                   move      'Y'           @@scnd            1
     c                   exsr      errck1

     c                   if        @@err <> 'Y'
     c                   eval      sctt75 = (sctb75 * sctw75) +
     c                                      ((sctb75/7) * sctd75)               TIBS @ 75% Total

     c                   eval      sctt70 = (scta70 * sctw70) +                 TIBS @ 70% Total
     c                                      ((scta70/7) * sctd70)

     c                   eval      sciwks = scipct * 3                          IIBS wks
     c                   eval      scitot = sciwks * sciamt                     IIBS total

     c                   eval      scstot = scsmth * scsamt                     SIBS total

     c                   eval      scdtot = scdwks * scdamt                     DIBS total

     c                   eval      sctair = sctt75 + sctt70 + scitot +          Tot Ant Indm rsvs
     c                                      scstot + scdtot + xxiedr + sclamt

     c                   eval      sctirs = sctair + sctipd + xxiedp            Tot Indm rsvs
     c                   endif

     c******** beg add ** 06/24/10 *********************************************
     c                   endif
     c******** end add ** 06/24/10 *********************************************
     c                   endsr                                                  kgdo
