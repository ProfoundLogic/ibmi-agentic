     H* (c) Copyright Fiserv, Inc., 2006 - 2007                                *
     H**************************************************************************
    H/TITLE FROI Claim Import Process
     H**************************************************************************
     H* Program description:                                                   *
     H*     This program creates claims based on data from the FROI            *
     H*     import file.  Edits are processed on each claim.  If the edits     *
     H*     are passed, the claim is created in WMCMP (live claim file).       *
     H*     If any edit is not passed, the claim is created in WHFRP           *
     H*     (CiCenter interface file) so corrections can be made, then it      *
     H*     can be transferred into WMCMP.                                     *
     H*                                                                        *
     H* CALLED FROM:                                                           *
     H*   W0902C                                                               *
     H* CALLS TO:                                                              *
     H*   none                                                                 *
     H*                                                                        *
     H* CREATED: 10/18/06     C:15      Project: 132WDQ                        *
     H**************************************************************************
     H* INDICATORS:                                                            *
     H*   99  GENERAL CHAIN INDICATOR                                          *
     H**************************************************************************
     H* REVISIONS:                                                             *
     H*                                                                        *
     H* 04/20/07 C:15  Project: 132WDQ                                         *
     H*                Per request from client, removed code that adds claims  *
     H*                to live WCMS claim file.                                *
     H**************************************************************************
     fwxeip     if   e             disk
     fwdihp     uf   e             disk
     f                                     infds(dbfds)
     fwhfrp     uf a e             disk
     f                                     infds(dbfds1)
      *
     d dbfds           ds
     d  dbrrn                397    400b 0
     d dbfds1          ds
     d  dbrrn1               397    400b 0

     d xxcnt           s                   like(ihcnt)
     d xxerrc          s                   like(iherrc)
     d xxdate          s              7  0

     d* Import file data structure
     d eidata          ds          1500
     d  group                  4      6
     d  emplr#                 7     15
     d  div#                  16     20
     d  policy#               21     35
     d  empFein               36     44
     d  ss#                   45     53
     d  lastName              54     83
     d  firstName             84     98
     d  midInitial            99     99
     d  addr1                100    139
     d  addr2                140    179
     d  city                 180    204
     d  state                205    206
     d  zip                  207    215
     d  country              216    218
     d  postalCode           219    258
     d  class                259    263
     d  avgWklyWage          264    270
     d  accdate              271    278
     d  acctime              279    294
     d  dateRpt              285    292
     d  losttime             293    293
     d  nature               294    295
     d  part                 296    297
     d  cause                298    299
     d  empPrms              300    300
     d  hireDt               301    308
     d  jurisSt              309    310
     d  dtLastWrk            311    318
     d  birthDt              319    326
     d  homephn              327    336
     d  gender               337    337
     d  maritalSts           338    338
     d  disabilityDt         339    346
     d  deathDt              347    354
     d  rtwDt                355    362
     d  pdOnInjDt            363    363
     d  fullSalary           364    364
     d  dtPrvForm            365    372
     d  accDesc1             373    432
     d  accLoc               433    472
     d  locCnty              473    502
     d  dept                 503    542
     d  othInj               543    543
     d  equipmt              544    703
     d  spfcActvty           704    863
     d  accDesc2             864    983
     d  physName             984   1023
     d  physAdr             1024   1063
     d  physCity            1064   1088
     d  physState           1089   1090
     d  physZip             1091   1099
     d  physPhn             1100   1109
     d  hosptlzd            1110   1110
     d  hospName            1111   1150
     d  hospAdr             1151   1190
     d  hospCity            1191   1215
     d  hospState           1216   1217
     d  hospZip             1218   1226
     d  hospPhn             1227   1236
     d  emrgRm              1237   1237
     d  occupation          1238   1277
     d  hoursPday           1278   1280
     d  daysPweek           1281   1281
     d  totHours            1282   1284
     d  empmntSts           1285   1286

     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D ##conm          s             40
     D ##data          s             80
     D ##leng          s              3  0
     D f3reqd          s              1
      *=========================================================================
      *   KLIST
      *=========================================================================
      *  Entry parm list
      *
     c     *entry        plist
     c                   parm                    qqrrn             9
    c*
     c                   move      qqrrn         @@rrn             9 0
    C*
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      '*MDYY'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
      *
      *-------------------------------------------------------------------------
      *   Retrieve the Import History Record.
      *
     c     @@rrn         chain     wdihp                              99
     c                   if        %found

      *   Start reading records from the Import file.
      *
     c                   read      pei01                                  99
     c                   dow       *in99 = *off

     c                   exsr      WrtWhfrp
     c                   exsr      errchk1
     c                   if        ##mode <> '1'
     c*** Begin dlt *** 04/20/07 ************************************************
     c***************    exsr      WrtWmcmp
     c*** End dlt ***** 04/20/07 ************************************************
     c                   endif

     c                   read      pei01                                  99
     c                   enddo

      *   Update the "count" totals in the Import History Record.
      *
     c                   eval      ihcnt  = xxcnt
     c                   eval      iherrc = xxerrc
     c                   update    pih01

     c                   endif

     c                   seton                                        lr

      *=========================================================================
      * WtrWhfrp - This subroutine writes a WHFRP record.
      *=========================================================================
     c     WrtWhfrp      begsr

     c                   clear                   pfr01

     c                   eval      frstat = 'P'
     c                   eval      frco   = ihco#
     c                   move      group         frfnd
     c                   move      emplr#        fremp
     c                   move      div#          frdiv
     c                   move      empFein       frfein
     c                   move      ss#           frss
     c                   movel     lastName      frlsn
     c                   movel     firstName     frfsn
     c                   movel     midInitial    frmid
     c                   movel     addr1         fradr1
     c                   movel     addr2         fradr2
     c                   movel     city          frcty
     c                   movel     state         frst
     c                   move      zip           frzip
     c                   movel     country       frctry
     c                   movel     postalCode    fradr3
     c                   move      class         frclas
     c                   move      avgWklyWage   fraww
     c                   move      accdate       fracdt
     c                   move      acctime       fractm
     c                   move      dateRpt       frdtrp
     c                   move      lostTime      frltfl
     c                   movel     nature        frnatr
     c                   movel     part          frpart
     c                   movel     cause         frcaus
     c                   movel     empPrms       freadr
     c                   move      hireDt        frhidt
     c                   movel     jurisSt       frjrst
     c                   move      dtLastWrk     frdtlw
     c                   move      birthDt       frdtob
     c                   movel     homephn       frarea
     c                   move      homephn       frphn
     c                   movel     gender        frsex
     c                   movel     maritalSts    frmsts
     c                   move      disabilityDt  frdbdt
     c                   move      deathDt       frdod
     c                   move      rtwDt         frrtw
     c                   move      pdOnInjDt     frpoi
     c                   movel     fullsalary    frfsal
     c                   move      dtPrvForm     frdt1
     c                   movel     accDesc1      fr60a7
     c                   movel     accLoc        fracdr
     c                   movel     locCnty       fr30a3
     c                   movel     dept          fr40a4
     c                   movel     othInj        fr1a4
     c                   movel     equipmt       fr80a4
     c                   move      equipmt       fr80a5
     c                   movel     spfcActvty    fr80a6
     c                   move      spfcActvty    fr80a7
     c                   movel     accDesc2      fr60a3
     c                   move      accDesc2      fr60a4
     c                   movel     physName      frphyn
     c                   movel     physAdr       frphya
     c                   movel     physCity      frphyc
     c                   movel     physState     frphys
     c                   move      physZip       frphyz
     c                   movel     physPhn       frphyr
     c                   move      physPhn       frphyp
     c                   movel     hosptlzd      fr1a5
     c                   movel     hospName      frhosn
     c                   movel     hospAdr       frhosa
     c                   movel     hospCity      frhosc
     c                   movel     hospState     frhoss
     c                   move      hospZip       frhosz
     c                   movel     hospPhn       fr3n2
     c                   move      hospPhn       fr9n4
     c                   movel     emrgRm        fr1a6
     c                   movel     occupation    frocds
     c                   move      hoursPday     frhrsd
     c                   move      daysPweek     frdays
     c                   move      totHours      fr3n1
     c                   movel     empmntSts     frests

     c                   time                    dsdttm
     c                   z-add     dsdate        srdti
     c                   call      'SRCVTDAT'    dtyymd
     c                   movel     srdto         frdate

     c                   eval      frtime = dstime
     c                   eval      frpgm  = q1pgm
     c                   eval      fruser = q1user
     c                   eval      fridt  = frdate
     c                   eval      fritm  = dstime
     c                   eval      fripgm = q1pgm
     c                   eval      friusr = q1user

     c                   eval      frrcvd = frdate

     c                   write     pfr01

     c                   endsr
      *=========================================================================
      * Errchk1 - This subroutine writes a WHFRP record.
      *=========================================================================
     c     Errchk1       begsr

     c                   call      'W0071R11'
     c                   parm                    ##kc              1
     c                   parm                    ##kl              1
     c                   parm      'X'           ##mode            1
     c                   parm      dbrrn1        ##rrn             9 0

     c                   if        ##mode = '1'
     c                   eval      xxerrc = xxerrc + 1
     c                   endif

     c                   eval      xxcnt = xxcnt + 1

     c                   endsr
      *=========================================================================
      * WtrWmcmp - This subroutine writes a WMCMP record.
      *=========================================================================
     c     WrtWmcmp      begsr

     c                   call      'W0070R01'
     c                   parm                    ##kc
     c                   parm                    ##kl
     c                   parm      'X'           ##mode
     c                   parm                    ##rrn
     c                   parm                    ##cont            1
     c                   parm      dbrrn1        ##rrn1            9 0

     c     dbrrn1        chain     pfr01
     c                   if        %found
     c                   eval      frstat = 'T'
     c                   update    pfr01
     c                   endif

     c                   endsr
      *=================================================================================
