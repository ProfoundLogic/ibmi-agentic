     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H*====================================================================
     H/title Import Response File processor
     H*====================================================================
     H*  Created:07/16/09  C:198  Project: WC1819 Phase 3
     H*  Called by: W7484C
     H*  Calls to : None
     H*====================================================================
     H*  Revisions:
     H*                                                                                    *
     H*  03/06/25 C:333  Project WC2998                                                    *
     H*                  Changes to CMS Settlement Reporting.                              *
     H*                  - Recompiled.                                                     *
     H*                                                                                    *
     H*  11/01/19   C:79   Support: 84449
     H*                    - Replace OPTACOMP condition with client 152     ermine         *
     H*                                                                                    *
     H*  01/05/16   C:53   Support: 38816
     H*                    - Remove 36349. Cannot use incoming R2DCN# to determine         *
     H*                                    OptaComp fund as it may not be our number       *
     H*                    - Access WMCML1 to populate DARTS remark assignment fields.     *
     H*                                                                                    *
     H*  08/12/15   C:306  Support: 36349
     H*                    Fixed regarding, dairy item was assigned to wrong user.         *
     H*                                                                                    *
     H*  03/31/14   C:53   Project: 114WBF                                                 *
     H*                    Check the alternate claim # for OPTACOMP groups.                *
     H*                                                                                    *
     H*  05/03/13   C:190  Project: 114WEI                                                 *
     H*                    - Add the screen extract date parameter.                        *
     H*                    - Removed "d1rsbm= ' '" while updating WDD1P(per MF)            *
     H*                                                                                    *
     H*  11/18/11   C:062  Support: WC114 78428
     H*                    Look for type of record.                         laimant SS#
     H*                                                                                    *
     H*  04/15/11   C:198  Support: 76569
     H*                    update HICN only when Log history file for that claimant SS#
     H*                    is getting updated.
     H*                                                                                    *
     H*  04/14/11   C:53   Support: 76569
     H*                    Do not update HICN# if it has a value            SP
     H*                                                                                    *
     H*  02/24/11   C:53   Support: 75995
     H*                    Correct blind chain                              SP
     H*                                                                                    *
     H*  02/16/11   C:198  Project: WC1879 priss 163
     H*                    Send diary to claims only when error received is SP
     H*
     H*  01/19/11   C:53   Project: WC1963 Support 75560                                   *
     H*                    Use HIC# in the WMCDP per WC1963 changes                        *
     H*
     H*  12/18/10   C:198  Project: WC1879 Priss 119/120/122
     H*                    Fixed the issue where query response diary remark was
     H*                    going to the wrong user.
     H*
     H*  12/14/10   C:198  Project: WC1879 Priss 76/105/113
     H*                    Fixed the issue where claim rep TIN was getting
     H*                    updated with Group TIN.
     H*
     H*  11/19/10   C:198  Project: WC1879 Priss 76/94/96
     H*                    Added 2 fields --> Policy Number and claims number
     H*
     H*  10/29/10   C:198  Project: WC1879 Phase 3 Priss 76
     H*                    Send the recent most extract date from WDD1P file back
     H*                    so as to make it available for W7484R3.
     H*
     H*  10/19/10   C:198  Project WC1879 Priss# 71
     H*                    Fixed resubmission issue.
     H*
     H*  10/15/10   C:227  Project: WC1879 Phase 3 Priss 52/66
     H*                    Get the recent most extract date from WDD1P file.
     H*
     H*  04/16/10   C:198  Project: WC1819 Phase 3 Priss 112/114/115
     H*                    Corrected loading of folder id/de key when writing
     H*                    to DARTS file (SDCMP).
     H*                    Response file could have more than 1 record for a
     H*                    claimant.
     H*                    Response file has response in 8881011111111 format
     H*                    not in 888/101/1111111 format.
     H*
     H*  02/19/10   C:198  Project: WC1819 Phase 3 Priss 69
     H*                    Corrected loading Claims master HICN with the
     H*                    applied HICN not the submitted one.
     H*
     H*  02/17/10   C:198  Project: WC1819 Phase 3 Changes
     H*                    modified updation of WDD1P on receiving response
     H*                    so that it updates the latest record available for
     H*                    that claim in log history.
     H*
     H*  11/25/09   C:198  Project: WC1819 Phase 3 PRTSK 24
     H*                    Added WMD1P to get response date updated.
     H*
     H*====================================================================
     fwdr2p     if   e             disk
     fwdd1l     uf   e           k disk
     fwdc5l     if a e           k disk
     fwde5l     if a e           k disk
     FWMCXL     if   e           k disk
     Fstctl1    if   e           k disk
     Fwmupl1    if   e           k disk
     Fwmcml1    uf   e           k disk
     Fwmcdl     uf   e           k disk
     FWTGTL     IF   E           K DISK
     Fwmd1l     uf   e           k disk    rename(pd101:pd1m1) prefix(md:2)
     F*** Begin add *** 03/31/14 **************************************
     Fwmfnl     if   e           k disk
     Fwmcml17   if   e           k disk    rename(pcm01:pcm17)
     F*** End   add *** 03/13/14 **************************************
     D*====================================================================
    D* Data Structures
     D*====================================================================
     dr2data           ds
     dr2rin                    1      4
     dr2dcn#                   5     19
     dr2type                  20     20  0
     dr2hic#                  21     32
     dr2ss#                   33     41  0
     dr2lsn                   42     81
     dr2fsn                   82    111
     dr2mid                  112    112
     dr2sex                  113    113  0
     dr2dtob                 114    121  0
     dr2plaa                 122    130  0
     dr2off#                 131    139
     dr2cap#                 140    169
     dr2clm#                 170    199
     dr2fil1                 200    219
     dr2ihic#                220    231
     dr2iss#                 232    240
     dr2ilsn                 241    280
     dr2ifsn                 281    310
     dr2imid                 311    311
     dr2isex                 312    312  0
     dr2idtob                313    320  0
     dr2mefd                 321    328  0
     dr2mexd                 329    336  0
     dr2pity                 337    337
     dr2fil2                 338    357
     dr2code                 358    359
     dr2ecd1                 360    364
     dr2ecd2                 365    369
     dr2ecd3                 370    374
     dr2ecd4                 375    379
     dr2ecd5                 380    384
     dr2ecd6                 385    389
     dr2ecd7                 390    394
     dr2ecd8                 395    399
     dr2ecd9                 400    404
     dr2ecd10                405    409
     dr2ccd1                 410    411
     dr2ccd2                 412    413
     dr2ccd3                 414    415
     dr2ccd4                 416    417
     dr2ccd5                 418    419
     dr2ccd6                 420    421
     dr2ccd7                 422    423
     dr2ccd8                 424    425
     dr2ccd9                 426    427
     dr2ccd10                428    429
     dr2fil3                 430    460
     Dx2dcn#           ds            15
     dxxfnd                    1      3  0
     dxxfyr                    4      6  0
     dxxcase                   7     13  0
     D DSKEY           ds            40
     d  dsco#                         3  0 OVERLAY(DSKEY)
     d  DSFND                         3  0 OVERLAY(DSKEY:4)
     d  DSFYR                         3  0 OVERLAY(DSKEY:7)
     d  DSCASE                        7  0 OVERLAY(DSKEY:10)
     D                 ds
     D DSDTTM                        14  0
     D  DSTIME                        6  0 OVERLAY(DSDTTM)
     D  DSDATE                        8  0 OVERLAY(DSDTTM:7)
     D**
     D*====================================================================
    D* External Data Structures
     D*====================================================================
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*====================================================================
    D* Stand Alone Fields
     D*====================================================================
     Dxxco#            S              3  0
     D*** BEGIN ADD *** 11/27/13 **************************************
     D xxpds#          S                   LIKE(CMPDS#) INZ
     D*** END   ADD *** 11/27/13 **************************************
      ***** beg del *** 01/05/16 **************************************
      ***** beg add *** 08/12/15 **************************************
     d*isOptaComp **** s               n   inz(*off)
      ***** end add *** 08/12/15 **************************************
      ***** end del *** 01/05/16 **************************************
     D*====================================================================
     D* Procedures
     D*====================================================================
     D*** Beg Add *** 11/01/19 *********************************************
     D GetClientID     pr                  extpgm('RTVCLTID')
     D CltID                          3
     D CltID           s              3
     D*** End Add *** 11/01/19 *********************************************
     D*====================================================================
     D* Constants
     D*====================================================================
     D*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@co#             3
     C                   PARM                    @@exdt            8
     C*** Beg Add *** 05/03/13 ****************************************
     C                   PARM                    @@date            8
     C*** End Add *** 05/03/13 ****************************************
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD1       PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *blanks       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTCYMD2       PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     d1ky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxcase
     C                   kfld                    xxexdt            7 0
     C*
     C     d2ky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxcase
     C                   kfld                    xxedt1            7 0
     C*
     C     cmky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxcase
     C*
     C     cxky          KLIST
     C                   KFLD                    xxCO#
     C                   KFLD                    xxFND
     C*
     C     GTKY          KLIST
     C                   KFLD                    xxCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C     d1ky1         klist
     C                   KFLD                    xxco#
     C                   KFLD                    xxfnd
     C                   KFLD                    xxexdt
     C*
     C*** Beg Add *** 03/31/14 ****************************************
     C     cmky17        KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxpds#
      *
     C     fnky          KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxfnd
     C*** End Add *** 03/31/14 ****************************************
    C*================================================================
    C*  Mainline
    C*================================================================
       //*** Beg Add *** 11/01/19 ******************************************
      /Free
                         callp GetClientID(CltID);
      /End-free
       //*** End Add *** 11/01/19 ******************************************
     C                   move      @@exdt        srdti
     C                   call      'SRCVTDAT'    DTCYMD
     C                   movel     SRDTO         XXexdt
     C                   movel     SRDTO         xxedt1
     C*** Beg Add *** 05/03/13 ****************************************
     c                   If        @@date = ' '
     c                   eval      @@date = '00000000'
     c                   Endif
     c                   If        @@date <> '00000000'
     C                   move      @@date        srdti
     C                   call      'SRCVTDAT'    DTCYMD
     C                   movel     SRDTO         XXexdt
     C                   movel     SRDTO         xxedt1
     c                   Endif
     C*** End Add *** 05/03/13 ****************************************
     C*
     C                   movel     @@co#         xxco#
     C*
     C                   read      pr201                                  90
     C*
     C                   dow       *in90 = *off
     C                   eval      x2dcn# = r2dcn#
     C*** Beg Add *** 03/31/14 ****************************************
      ****** beg chg *** 08/12/15 ****************************************
      ***** beg chg *** 01/05/16 **************************************
     C*******            exsr      OptaCmpNum
     C                   exsr      OptaCmpNum
      ***** beg chg *** 01/05/16 **************************************
     C*** End Add *** 03/31/14 ****************************************

      ***** beg del *** 01/05/16 **************************************
      /free
       // Exsr ChkClmComp;
       // If isOptacomp = *On;
       //    Exsr OptaCmpNum;
       // Endif;
      /end-free
      ***** end del *** 01/05/16 **************************************
      ****** end chg *** 08/12/15 ****************************************
     C                   move      r2type        xxtype            1
     C                   move      xxedt1        xxexdt
     C     d2ky          setgt     pd101                              60
     C     cmky          readpe(n) pd101                                  60
     C                   dow       *in60 = *off
     C                   if        d1type <> 'Q' and d1rsdt = 0 and
     C                             xxtype = d1type
     C*
     C                   move      d1exdt        xxexdt
     C*
     C                   eval      srdti = 0
     C                   move      xxexdt        srdti
     C                   call      'SRCVTDAT'    DTCYMD2
     C                   movel     SRDTO         @@exdt
     C                   leave
     C                   else
     C     cmky          readpe(n) pd101                                  60
     C                   endif
     C                   enddo
     C     d1ky          chain     pd101                              90
     C                   dow       *in90 = *off
     C                   move      r2type        xxtype            1
     C*** Beg Chg *** 05/03/13 ****************************************
     C***********        if        xxtype = d1type and d1rsbm = ' '
     C                   if        xxtype = d1type
     C*** End Chg *** 05/03/13 ****************************************
     C                   exsr      update_WDD1P
     C     d1ky1         chain     pd1m1                              90
     C  N90              exsr      update_WMD1P
     C                   exsr      write_WDE5P
     C                   exsr      write_WDC5P
     C                   move      r2ss#         xxss#             9
     C     xxss#         chain     pcd01                              91
     C                   if        *in91 = *off
     C                   eval      cdhicn = d2hic#
     C                   update    pcd01
     C                   endif
     C                   endif
     C     d1ky          reade     pd101                                  90
     C                   enddo
     C                   if        %subst(r2data:360:50) <> ' '
     C                             and  %subst(r2data:358:2) = 'SP'
     C*** if remark code on W0069F/SCR05 is specified and either staff code/diary user is non blank
     C*** Check by chaining on WMCXP
     C     cxky          chain     pcx01                              90
     C                   if        *in90 = *off
     C                   if        cxcerd <> ' '
     C                   if        cxcedu <> ' ' or cxcesu <>  ' '
     C                   exsr      write_Diary
     C                   endif
     C                   endif
     C                   endif
     C                   endif                                                  error codes
     C                   read      pr201                                  90
     C                   enddo
     C                   return
     C*=============================================================================================
     C     Write_diary   begsr
     C*
     c                   eval      dskey  = *blanks
     C*
     C                   eval      dsco#  = xxco#
     C                   eval      dsfnd  = xxfnd
     C                   eval      dsfyr  = xxfyr
     C                   eval      dscase = xxcase
     C*
     C                   time                    DSDTTM
     C                   EVAL      SRDTI = dsdate
     C                   call      'SRCVTDAT'    DATCHK
     C                   movel     SRDTO         XXCDAT            8
     C*
     C                   if        cxcedu <> ' '
     C                   MOVEL     'AXDU'        XXTBID            4
     C                   MOVEL     CXCEDU        XXKEY            10
     C                   select
     C                   when      CXQRDU = '01'
     C     CMCLEX        chain     pup01
     C                   IF        %FOUND
     C                   MOVEL     upusr         Q#USER
     C                   ENDIF
     C                   when      CXQRDU = '02'
     C     CMSUPR        chain     pup01
     C                   IF        %FOUND
     C                   MOVEL     upusr         Q#USER
     C                   ENDIF
     C                   when      CXQRDU = '03'
     C     CMAMAN        chain     pup01
     C                   IF        %FOUND
     C                   MOVEL     upusr         Q#USER
     C                   ENDIF
     C                   when      CXQRDU = '04'
     C     CMUMAN        chain     pup01
     C                   IF        %FOUND
     C                   MOVEL     upusr         Q#USER
     C                   ENDIF
     C                   when      CXQRDU = '05'
     C     CMNRSE        chain     pup01
     C                   IF        %FOUND
     C                   MOVEL     upusr         Q#USER
     C                   ENDIF
     C                   when      CXQRDU = '06'
     C     CMFEAD        chain     pup01
     C                   IF        %FOUND
     C                   MOVEL     upusr         Q#USER
     C                   ENDIF
     C                   when      CXQRDU = '07'
     C     CMSDFA        chain     pup01
     C                   IF        %FOUND
     C                   MOVEL     upusr         Q#USER
     C                   ENDIF
     C                   when      CXQRDU = '08'
     C     CMESUP        chain     pup01
     C                   IF        %FOUND
     C                   MOVEL     upusr         Q#USER
     C                   ENDIF
     C                   endsl
     C                   else
     C                   if        cxcesu <> ' '
     C     cxcesu        chain     pup01                              90
     C                   if        *in90 = *off
     C                   movel     upusr         Q#USER
     C                   endif
     C                   endif
     C                   endif
     C*
     C     cxcerd        chain     pct01                              90
     C                   if        *in90 = *off
     C                   movel     ctdesc        Q#COMM
     C                   movel     ctdepg        Q#DCID
     C                   movel     ctfl01        Q#DIFL
     C                   movel     dskey         Q#key
     C                   if        Q#DCID <> ' '
     C                   movel     Q#KEY         Q#FOLD
     C                   movel     'Y'           Q#FL01
     C                   else
     C                   eval      Q#FOLD =  ' '
     C                   eval      Q#FL01 =  ' '
     C                   endif
     C                   if        CTDPRG <> 0
     C                   movel     'Y'           Q#FL02
     C                   else
     C                   eval      Q#FL02 =  ' '
     C                   endif
     C                   endif
     C*
     C                   move      cxcerd        Q#RMCD
     C*
     C                   CALL      'DA700R'
     C                   PARM      DSKEY         Q#KEY            40
     C                   PARM                    Q#COMM          256
     C                   PARM      XXCDAT        Q#CDAT            8
     C                   PARM                    Q#DCID           12
     C                   PARM      'W0070R'      Q#PNAM           10
     C                   PARM      *BLANKS       Q#SECL            3
     C                   PARM                    Q#USER           10
     C                   PARM      Q1PGM         Q#PGM            10
     C                   PARM                    Q#FOLD           80
     C                   PARM                    Q#DIFL            1
     C                   PARM      Q1PGM         Q#IPGM           10
     C                   PARM                    Q#FL01            1
     C                   PARM                    Q#FL02            1
     C                   PARM                    Q#RMCD            5
     C                   PARM      '00000'       Q#SEQ#            5
     C                   PARM      'N'           Q#SBYL            1
     C                   PARM      *BLANKS       Q#COM2           19
     C                   PARM      *BLANKS       Q#APID            2
     C                   PARM      *BLANKS       Q#IDOC           12
     C                   PARM      *BLANKS       Q#IQUE           10
     C                   PARM      *BLANKS       Q#IMGF            1
     C                   PARM      *BLANKS       Q#IPTH           10
     C                   PARM      *BLANKS       Q#PIDX            5
     C                   PARM      *BLANKS       Q#QPTY            1
     C                   PARM      *BLANKS       Q#DCTP            5
     C                   PARM      *BLANKS       Q#CSID            2
     C                   endsr
     C*=============================================================================================
     C     Write_wde5p   begsr
     C*
     C                   z-add     0             j                 2 0
     C                   eval      e5co#  = xxco#
     C                   eval      e5fnd  = xxfnd
     C                   eval      e5fyr  = xxfyr
     C                   eval      e5case = xxcase
     C                   eval      e5stat = 'N'
     C*
     C                   eval      e5exdt = xxexdt
     C*
     C* Maintenance information
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        SRDTI
     C                   call      'SRCVTDAT'    dtcymd1
     C                   MOVEL     SRDTO         e5dt
     C                   MOVEL     Q1PGM         e5pgm
     C                   MOVEL     DSTIME        e5time
     C                   MOVEL     Q1user        e5user
     C*
     C                   MOVEL     Q1PGM         e5ipgm
     C                   MOVEL     Q1user        e5iusr
     C                   MOVEL     SRDTO         e5idt
     C                   MOVEL     DSTIME        e5itim
     C*
     C**** loop to read positions 300-349
     C                   dow       j <= 45
     C                   if        %subst(r2data : 360 + j : 5) <> ' '
     C                   eval      e5code = %subst(r2data : 360 + j : 5)
     C                   write     pe501
     C                   endif
     C                   eval      j = j + 5
     C                   enddo
     C*
     C                   endsr
     C*=============================================================================================
     C     Write_wdc5p   begsr
     C*
     C                   z-add     0             j                 2 0
     C                   eval      c5co#  = xxco#
     C                   eval      c5fnd  = xxfnd
     C                   eval      c5fyr  = xxfyr
     C                   eval      c5case = xxcase
     C                   eval      c5stat = 'N'
     C*
     C                   EVAL      c5exdt = xxexdt
     C*
     C* Maintenance information
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        SRDTI
     C                   call      'SRCVTDAT'    dtcymd1
     C                   MOVEL     SRDTO         c5dt
     C                   MOVEL     Q1PGM         c5pgm
     C                   MOVEL     DSTIME        c5time
     C                   MOVEL     Q1user        c5user
     C*
     C                   MOVEL     Q1PGM         c5ipgm
     C                   MOVEL     Q1user        c5iusr
     C                   MOVEL     SRDTO         c5idt
     C                   MOVEL     DSTIME        c5itim
     C*
     C**** loop to read positions 300-349
     C                   dow       j <= 18
     C                   if        %subst(r2data : 410 + j : 2) <> ' '
     C                   eval      c5code = %subst(r2data : 410 + j : 2)
     C                   write     pc501
     C                   endif
     C                   eval      j = j + 2
     C                   enddo
     C*
     C                   endsr
     C*=============================================================================================
     C     Update_wdd1p  begsr
     C*
     C                   TIME                    DSDTTM
     C*
     C*
     C                   MOVE      DSDATE        SRDTI
     C                   call      'SRCVTDAT'    dtcymd1
     C                   MOVEL     SRDTO         D1RSDT
     C*
     C                   eval      d1code = r2code
     C* Submitted data
     C                   eval      d1hic# = r2hic#
     C                   move      r2ss#         d1ss#
     C                   movel     r2type        d1type
     C                   EVAL      d1lsn  = r2lsn
     C                   EVAL      d1fsn  = r2fsn
     C                   EVAL      d1mid  = r2mid
     C                   move      r2sex         d1sex
     C                   MOVE      r2dtob        SRDTI
     C                   call      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         D1DTOB
     C                   EVAL      d1fein = r2plaa
     C                   EVAL      d1off# = r2off#
     C                   EVAL      d1cap# = r2cap#
     C                   EVAL      d1clm# = r2clm#
     C* Applied data
     C                   eval      d2hic# = r2ihic#
     C** iss became a char field
     C                   movel     r2iss#        d2ss#
     C                   EVAL      d2lsn  = r2ilsn
     C                   EVAL      d2fsn  = r2ifsn
     C                   EVAL      d2mid  = r2imid
     C                   move      r2isex        d2sex
     C                   MOVE      R2iDTOB       SRDTI
     C                   call      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         D2DTOB
     C                   MOVE      r2mefd        SRDTI
     C                   call      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         D2mefd
     C                   MOVE      r2mexd        SRDTI
     C                   call      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         D2mexd
     C                   EVAL      d2pity = r2pity
     C* Maintenance information
     C                   MOVE      DSDATE        SRDTI
     C                   call      'SRCVTDAT'    dtcymd1
     C                   MOVEL     SRDTO         d1dt
     C                   MOVEL     Q1PGM         d1pgm
     C                   MOVEL     DSTIME        d1time
     C                   MOVEL     Q1user        d1user
     C*
     C                   update    pd101
     C*
     C                   endsr
     C*=============================================================================================
     C     Update_wmd1p  begsr
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      DSDATE        SRDTI
     C                   call      'SRCVTDAT'    dtcymd1
     C                   MOVEL     SRDTO         mdRSDT
     C*
     C* Maintenance information
     C                   MOVE      DSDATE        SRDTI
     C                   call      'SRCVTDAT'    dtcymd1
     C                   MOVEL     SRDTO         mddt
     C                   MOVEL     Q1PGM         mdpgm
     C                   MOVEL     DSTIME        mdtime
     C                   MOVEL     Q1user        mduser
     C*
     C                   update    pd1m1
     C*
     C                   endsr
     C*** Beg Add *** 03/31/14 ****************************************
     C     OptaCmpNum    begsr
     C                   movel(p)  r2dcn#        xxpds#
     C     cmky17        chain     pcm17

     C                   if        %found
     C                   move      CMfnd         XXfnd
     C     fnky          chain     pfn01

     C*** Beg Chg *** 11/01/19 *********************************************
     C*******************IF        %found and FNgrpasc = 'OPTACOMP'
     C                   IF        %found and CltID = '152'
     C*** End Chg *** 11/01/19 *********************************************
      *** update x2dcn# subfields
     C                   move      *blanks       x2dcn#
     C                   move      CMfnd         xxfnd
     C                   move      CMfyr         xxfyr
     C                   move      CMcase        xxcase
     C*** beg add *** 01/05/16 **************************************
     C*
     C* Get claim master record for DARTS remark.
     C                   else
     C     cmky          chain     pcm01
     C*** end add *** 01/05/16 **************************************
     C                   endif
     C*** beg add *** 01/05/16 **************************************
     C*
     C* Get claim master record for DARTS remark.
     C                   else
     C     cmky          chain     pcm01
     C*** end add *** 01/05/16 **************************************
     C                   endif
     C                   endsr
     C*** End Add *** 03/31/14 ****************************************
      /free
       //***** beg add ****** 08/12/15 ********************************
       //***** beg del *** 01/05/16 **************************************
       //--------------------------------------------------------------
       // ChkClmComp
       //--------------------------------------------------------------
       //Begsr ChkClmComp;
       //
       //isOptaComp = *Off;
       //xxFnd  = %dec(%subst(R2clm#:1:3):3:0);
       //xxFyr  = %dec(%subst(R2clm#:4:3):3:0);
       //xxCase = %dec(%subst(R2clm#:7:7):7:0);
       //
       //Chain(n) cmKy WMCML1;
       //If %found(WMCML1);
       //   xxFnd = CMfnd;
       //
          // Checking for OPTACOMP.
       //   Chain fnKy pfn01;
       //   If %found(WMFNL) and FNgrpAsc = 'OPTACOMP';
       //      IsOptaComp = *On;
       //      Leavesr;
       //   Endif;
       //
          // Populating the fields with WMCML1 fields
       //   x2dcn# = *blanks;
       //   xxFnd  = CMfnd;
       //   xxFyr  = CMfyr;
       //   xxCase = CMcase;
       //Endif;

       //Endsr;

       //***** end del *** 01/05/16 **************************************
       //***** end add ***** 08/12/15 *************************************
      /end-free
