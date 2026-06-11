     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE') bnddir('W0590R01')
     H*********************************************************************
     H/TITLE W0590R01 - Anticipated Medical Payment Reserve Worksheet
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
     H*                Modified for Total Medical Reserve Change.         *
     H*                                                                   *
     H* 06/24/10 C:190 Project: 25WQD                                     *
     H*                Additional Changes.                                *
     H*                                                                   *
     H*********************************************************************
    H*
     fw0590f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    f*
    f*  Physical file definition (for maintaining record)
     fwmrwp     uf a e             disk
     f                                     rename(prw01:dbrcdf)
     f                                     infds(dbfds)
     fwmrwl     if   e           k disk
     f                                     infds(dbfds1)
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
     f***********                          ignore(prt01)
     f                                     ignore(psp01)
     f                                     ignore(psy01)
     f                                     ignore(ptr01)
     f                                     ignore(ptt01)
     f                                     ignore(pvs01)
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
    d*================================================================
    d*  /COPY required constants
    d*================================================================
     d*
     d dbfds           ds
     d  dbrrn                397    400B 0

     d dbfds1          ds
     d  dbrrn1               397    400B 0

     d/copy sPrDate
     d/copy sPrString
     d/copy wPrGroup
     d optnm           c                   'Anticipated Medical Reserve Worshee-
     d                                     t'
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
     d @@erchk         s              1
     d @@sts           s              1
     d ##sts           s              1
     d @@rrn           s              9  0
     d @@rrn2          s              9  0
     d xxmedr          s             15  2
     d xxmedp          s             15  2
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D ##rrn2          s              9  0
     D @@con           s              1

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
     c                   parm                    @@erchk

     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      @@mode        ##mode
     c                   parm      @@rrn         ##rrn
     c                   parm      @@rrn2        ##rrn2
     c                   parm      @@sts         ##sts
     c                   parm                    ##chk             1
     c                   parm                    ##mode1           1
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
     C                   kfld                    xxco#
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

     C******** beg add ** 06/24/10 *********************************************
     C                   eval      @#sts = *blanks
     C                   move      rwsts         @#sts             1
     C******** end add ** 06/24/10 *********************************************
     c     rwky1         setgt     prw01
     c     rwky1         readpe    prw01                                  60
     c                   if        *in60 = *off AND @@mode = 'A'
     C                   move      'A'           @@sts
     c                   eval      @@rrn = dbrrn1
     c                   eval      ##mode1= @@mode
     c                   eval      @@mode= 'C'
     c                   endif

     c*  COMP RESERVES CAN ONLY BE ENTERED ON AN INDEMNITY CLAIM.
     c                   if        @@mode <> 'D'
     c                   eval      ##chk = *blanks
     c                   if        cmltfl = 'M'
     c                   exfmt     wdw02
     c                   eval      @@mode= 'D'
     c                   eval      @@sts = 'D'
     c                   eval      ##chk = 'Y'
     c                   endif
     c                   endif

     C                   call      'W0590R02'    ACDPL                  99
     C                   if        *inkl = '1'
     C******** beg add ** 06/24/10 *********************************************
     c                   if        @#sts = 'P' AND @@MODE <> 'D'
     C                   EVAL      @@MODE = 'C'
     c                   endif
     C******** end add ** 06/24/10 *********************************************
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

     c  n90              if        scanh <  *zeros
     C                   seton                                        51  90
     c                   endif

     c  n90              if        scanam <  *zeros
     C                   seton                                        52  90
     c                   endif

     c  n90              if        scans  <  *zeros
     C                   seton                                        53  90
     c                   endif

     c  n90              if        scanr  <  *zeros
     C                   seton                                        54  90
     c                   endif

     c  n90              if        scanl  <  *zeros
     C                   seton                                        55  90
     c                   endif

     c  n90              if        scanpt <  *zeros
     C                   seton                                        56  90
     c                   endif

     c  n90              if        scanrx <  *zeros
     C                   seton                                        57  90
     c                   endif

     c  n90              if        scanme <  *zeros
     C                   seton                                        58  90
     c                   endif

     c  n90              if        scantp <  *zeros
     C                   seton                                        59  90
     c                   endif

     c  n90              if        scancm <  *zeros
     C                   seton                                        62  90
     c                   endif

     c  n90              if        scanbb <  *zeros
     C                   seton                                        66  90
     c                   endif

     c  n90              if        scanm1 <  *zeros
     C                   seton                                        63  90
     c                   endif

     c  n90              if        scanm2 <  *zeros
     C                   seton                                        64  90
     c                   endif

     c  n90              if        scanom <  *zeros
     C                   seton                                        65  90
     c                   endif


     c                   if        scanh =  *zeros and
     c                             scanam = *zeros and
     c                             scans  = *zeros and
     c                             scanr  = *zeros and
     c                             scanl  = *zeros and
     c                             scanpt = *zeros and
     c                             scanrx = *zeros and
     c                             scanme = *zeros and
     c                             scantp = *zeros and
     c                             scancm = *zeros and
     c                             scanm1 = *zeros and
     c                             scanm2 = *zeros and
     c                             scanbb = *zeros and
     c                             scanom = *zeros

     c                   Dou       *in90 = *off
     c                   exfmt     wdw01


     c                   eval      *in90 = *off
     c                   eval      *in73 = *off

     c                   if        sccon <> 'Y'and
     c                             sccon <> 'N' and
     c                             *inkl <> *on
     C                   seton                                        73  90
     c                   endif

     c                   enddo

     c                   if        *inkl = *on  or sccon = 'N'
     c                   eval      *in90 = *on
     c                   eval      sccon = *blanks
     c                   endif

     c                   endif

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

     c                   eval      scanh = rwanh                                Hospital
     c                   eval      scans = rwans                                Surgery
     c                   eval      scanl = rwanl                                Laboratory
     c                   eval      scanrx= rwanrx                               Rx/Supplies
     c                   eval      scantp =rwantp                               Treating Dr
     c                   eval      scanm1= rwanm1                               Oth Med rsvs 1
     c                   eval      scanm2= rwanm2                               Oth Med rsvs 2
     c                   eval      scanam= rwanam                               Ambulance/mileage
     c                   eval      scanr = rwanr                                Radiology
     c                   eval      scanpt= rwanpt                               Physical therapy
     c                   eval      scanme= rwanme                               RME/des doctor
     c                   eval      scancm= rwancm                               Med/case Mgmt
     c                   eval      scanbb= rwanbb                               Burial benefits
     c                   eval      scanom= rwanom                               Other

     C******** beg add ** 06/24/10 *********************************************
     c                   if        rwsts = 'A' or rwsts = 'D'
     c                   eval      sccmer = rwmedr                              Curr Med rsvs
     c                   eval      sctmpd = rwmedp                              Tot Med PTD
     c                   eval      sctamr= rwtamr                               Tot Ant Med Rsvs
     c                   eval      sctmrs= rwtmrs                               Tot Med Rsvs
     c                   eval      scaww = rwaww                                Avg weekly wg
     c                   else
     C******** end add ** 06/24/10 *********************************************
     c                   exsr      SrReserve
     c                   eval      sccmer = xxmedr                              Curr Med rsvs
     c                   eval      sctmpd = xxmedp                              Tot Med PTD
     c                   eval      scaww = cmaww                                Avg weekly wg
     C******** beg add ** 06/24/10 *********************************************
     c                   endif
     C******** end add ** 06/24/10 *********************************************

     c                   endsr                                                  mvdbsc
    c*================================================================
    c* MVSCDB - Move screen fields to data base fields
    c*================================================================
     c     mvscdb        begsr

     c                   if        sccon <> 'N'

     c                   eval      rwco# = cmco#                                Company
     c                   eval      rwfnd = cmfnd                                Fund
     c                   eval      rwfyr = cmfyr                                Fund year
     c                   eval      rwcase= cmcase                               Case
     c                   if        rwsts  =  ' '
     c                   eval      rwsts = 'P'                                  Rsrv  wrksht status
     c                   endif
     c                   if        rwauid =  ' '
     c                   eval      rwauid=q1user                                Adjuster userid
     c                   endif

     c                   eval      rwanh = scanh                                Ant Hospital Rsvs
     c                   eval      rwans = scans                                Ant Surgery Rsvs
     c                   eval      rwanl = scanl                                Ant Lab Rsvs
     c                   eval      rwanrx= scanrx                               Ant Pharmacy Rsvs
     c                   eval      rwantp =scantp                               Ant Treating Phys
     c                   eval      rwanm1= scanm1                               Ant Other Med 1Rsvs
     c                   eval      rwanm2= scanm2                               Ant Other Med 2Rsvs
     c                   eval      rwanam= scanam                               Ant Ambulance Rsvs
     c                   eval      rwanr = scanr                                Ant Radiology Rsvs
     c                   eval      rwanpt= scanpt                               Ant Phys Therapy Rsv
     c                   eval      rwanme= scanme                               Ant RME Rsvs
     c                   eval      rwancm= scancm                               Ant Case Mgmt Rsvs
     c                   eval      rwanbb= scanbb                               Ant Burial Ben Rsvs
     c                   eval      rwanom= scanom                               Ant Other Med Rsvs

     C                   exsr      kgdo

     C******** beg add ** 06/24/10 *********************************************
     c                   eval      rwmedr = sccmer                              Curr Med rsvs
     c                   eval      rwmedp = sctmpd                              Tot Med PTD
     c                   eval      rwaww = scaww                                Avg weekly wg
     C******** end add ** 06/24/10 *********************************************
     c                   eval      rwtamr= sctamr                               Tot Ant Med Rsvs
     c                   eval      rwtmrs= sctmrs                               Tot Med Rsvs
     C******** beg add ** 12/09/11 *********************************************
     c                   eval      rwtamc = scanh + scanam + scans +            Curr Med rsvs
     c                                      scanr + scanl + scanpt +            Tot Med PTD
     c                                      scanrx + scanme + scantp +          Tot Med PTD
     c                                      scancm + scanm1 + scanbb +          Tot Med PTD
     c                                      scanm2 + scanom                     Tot Med PTD
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
     C*
     C     *inkg         caseq     *on           KgDO                     90
     C                   endcs

     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr


     C                   eval      *inlr = *on

     C                   endsr                                                  pgmend
    c*================================================================
    c* PGMSTR - Start of program processing
    c*================================================================
     c     pgmstr        begsr

     c                   z-add     0             xxco#
     c     upky          chain     pup01                              60
     c                   if        *in60 = *off
     c                   z-add     upco#         xxco#
     c                   endif                                                  *in60 = 0

     c     @@rrn         chain     dbrcdf                             99
     c     @@rrn2        chain     pcm01                              99

      *  Do not allow an add if there is a record in pending status WMRWP/RPSTS='P'
     c                   if        @@mode= 'A'
     c     rwky1         setgt     prw01
     c     rwky1         readpe    prw01                                  60
     c                   dow       *in60 = *off
     c                   if        rwsts = 'P'
     c                   eval      @@erchk  = 'Y'
     c                   eval      *inlr=*on
     c                   return
     c                   endif                                                  rwtype = 'I'
     c     rwky1         readpe    prw01                                  60
     c                   enddo                                                  dow *in60=*off
     c                   endif                                                  @@mode = 'A'

      *  Do not allow an add if the claim is closed WMCMP/RPSTAR='C'
     c                   if        @@mode= 'A'  and
     c                             cmstat = 'C'
     c                   eval      @@erchk  = 'Y'
     C                   eval      *inlr=*on
     c                   return
     c                   endif

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

     c                   exsr      Callr

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

     c                   exsr      Callr

     c                   endsr
    c*====================================================================
    c*  SrReserve  Load Reserve Info
    c*====================================================================
     c     SrReserve     begsr

     c                   eval      xxmedr = *zeros
     c                   eval      xxmedp = *zeros

     c                   z-add     0             i                 2 0

     c                   for       i = 1 to 10

     c                   movel     rt(i)         kkey             10

     c     tbky          chain     prt01                              60
     c                   if        *in60 = *off
     C                   if        rttype = 'M'
     c                   eval      xxmedr = rr(i) + xxmedr                      Curr medical Rsvs
     c                   eval      xxmedp = ar(i) + xxmedp                      Tot med PTD

     c                   endif                                                  rttype = 'M'
     c                   endif                                                  *in60 = *off

     c                   endfor

      * Total antpt medical reserves
     c                   eval      sctamr = xxmedr + scanh + scans + scanl +
     c                             scanrx + scanm1 + scanm2 + scanam  + scanr +
     c                             scanpt + scanme + scancm + scanbb + scanom +
     c                             scantp

      * Total antpt PTD
     c                   eval      sctmrs = xxmedp + sctamr                     Tot med ptd
     c                   endsr                                                  SrReserve
    c*================================================================
    c* Load Maintenance Fields
    c*================================================================
     c     MaintFlds     begsr

     c                   if        sccon <> 'N'

     c                   eval      rwdt   = DateTo7(SystemDate)
     c                   eval      rwtm   = TimeTo6(SystemTime)                 Maintaince time
     c                   eval      rwuser = q1user                              Maintaince user
     c                   eval      rwpgm  = q1pgm                               Maintaince Pgm

     c                   endif                                                  sccon <> 'N'

     c                   endsr                                                  MaintFlds

    c*================================================================
    c* Load Initial Maintenance Fields
    c*================================================================
     c     MaintFldsInit begsr

     c                   exsr      MaintFlds

     c                   if        sccon <> 'N'
     c                   eval      rwidt  = rwdt                                Maintaince date
     c                   eval      rwitm  = rwtm                                Initial Time
     c                   eval      rwiusr = rwuser                              Initial User
     c                   eval      rwipgm = rwpgm                               Initial pgm.

     c                   endif                                                  sccon <> 'N'

     c                   endsr                                                  MaintFldsInit

     c*===============================================================
     c* KGDO - SUBROUTINE FOR F7
     c*===============================================================
     c     kgdo          begsr
     c
     c******** beg add ** 06/24/10 *********************************************
     c                   if        @@mode = 'D'
     c                   seton                                        88  90
     c                   else
     c******** end add ** 06/24/10 *********************************************
      * Total antpt medical reserves
     c                   eval      sctamr = xxmedr + scanh + scans + scanl +
     c                             scanrx + scanm1 + scanm2 + scanam  + scanr +
     c                             scanpt + scanme + scancm + scanbb + scanom +
     c                             scantp

      * Total antpt PTD
     c                   eval      sctmrs = xxmedp + sctamr                     Tot med ptd

     c******** beg add ** 06/24/10 *********************************************
     c                   endif
     c******** end add ** 06/24/10 *********************************************
     c                   endsr                                                  kjdo
