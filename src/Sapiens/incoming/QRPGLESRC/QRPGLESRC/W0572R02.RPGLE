     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE') bnddir('W0572R':'SCOPYMT42')
     H*********************************************************************
     H*  Additional Claimaints Maintenance
     H*********************************************************************
     H* Created: 10/05/09   C:33   Project: WC1770                        *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H* 11/28/16 C:307 Support 33717                                      *
     H*                Adding Email Address for Additional Claimant       *
     H*                                                                   *
     H* 05/18/10 C:198 Project WC1879                                     *
     H*                Either Representative last name/first name - OR - *
     H*                firm name is required, other may be optional.      *
     H*                                                                   *
     H*********************************************************************
     Fw0572f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwdcap     uf a e             disk    rename(pca01:dbrcdf)
     Fwdcal     if   e           k disk    rename(pca01:pca00)
     Fwdcal2    if   e           k disk    rename(pca01:pca02)
     F                                     infds(CAfds2)
     Fwmcmp     if   e             disk    rename(pcm01:pcm0p)
     Fwtgtl     if   e           k disk    rename(pgt01:pgt00)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Additional Claimants'
     D comapi          c                   'WA001R'
     D ClmAPI          c                   'WA006R'
     D #ofscr          c                   2
     D#ofGenId         c                   10
     D genSrchId       s             10    dim(#ofGenId) ctdata perrcd(1)
     D genDetail       s             10    dim(#ofGenId) alt(genSrchId)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D addMode         s              1n
     D chgMode         s              1n
     D dspMode         s              1n
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##Case          s                   like(CMCASE)
     D ##Yr            s                   like(CMFYR)
     D ##RtCd          s              1
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D kkKey           s                   like(GTKEY)
     D kkTbId          s                   like(GTTBID)
     D fnRRN           s              9  0
     D emRRN           s              9  0
     D nextSeq#        s                   like(CASEQ#) inz
     D countWDCAL      s              3  0              inz
     D saveSCCLRL      s                   like(SCCLRL)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     DCAfds2           ds
     D CArrn2                397    400b 0
     D*
     DclmLine1         ds            79
     D clmLine1a               1      6
     D clmLine1b               8     10
     D clmLine1c              12     71
     D*
     DclmLine2         ds            79
     D SCYRCS                  8     18
     D SCSS#                  26     36
     D SCCLNM                 39     58
     D SCACDT                 69     78
     D*
     D                 ds
     D SCZIP                          9  0
     D  SCZIP5                 1      5  0
     D  SCZIP4                 6      9  0
     D*
     D                 ds
     D SCCRZP                         9  0
     D  SCCRZP5                1      5  0
     D  SCCRZP4                6      9  0
     D*
     D                 ds
     D SCPHN#                         7  0
     D  SCPHN#3                1      3  0
     D  SCPHN#4                4      7  0
     D*
     D                 ds
     D SCCRPH                         7  0
     D  SCCRPH3                1      3  0
     D  SCCRPH4                4      7  0
     D*
     DCheckSS#         pr             1n
     D inSS#                               like(CMSS#)  const
     D*
     DGetGenDesc       pr                  like(GTDESC)
     D inKey                         10    options(*varsize) const
     D inTableId                           like(GTTBID)      const
     D*
     DGenSearch        pr                  opdesc
     D inKey                         10    options(*varsize)
     D inDesc                        40    options(*varsize:*nopass)
     D*
     DScrUpdated       pr             1n
     D*
     DValidGen         pr             1n   opdesc
     D inKey                         10    options(*varsize) const
     D inTableId                           like(GTTBID)      const
     D*
     DCEEDOD           PR
     D parmNum                       10I 0 const
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               12A   options(*omit)
     D*
     C/copy wPrGroup
     C/copy wPrEmplr
    C*
     C/COPY SCOPYMT42C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@rrn2
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C*
     C*  Claim line API
     C     ClmPl         plist
     C                   parm      CMCO#         ##Co#
     C                   parm      CMFND         ##Fnd
     C                   parm      CMFYR         ##Yr
     C                   parm      CMCASE        ##Case
     C                   parm                    clmLine1
     C                   parm                    clmLine2
     C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     keyWDCAL      klist
     C                   kfld                    CMCO#
     C                   kfld                    CMFND
     C                   kfld                    CMFYR
     C                   kfld                    CMCASE
     C*
     C     gtky          klist
     C                   kfld                    CMCO#
     C                   kfld                    kkTbId
     C                   kfld                    kkKey
     C*
    C*================================================================
    C* Change the status
    C*================================================================
     C     ChgSts        begsr
     C*
     C     @@rrn         chain     dbrcdf
     C*
     C                   if        %found
     C*
     C                   select
     C                   when      CASTAT = 'I'
     C                   eval      CASTAT = 'A'
     C                   other
     C                   eval      CASTAT = 'I'
     C                   endsl
     C*
     C                   exsr      MaintFlds
     C                   update    dbrcdf
     C*
     C                   endif
     C*
     C                   endsr
     C*
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     C*
     C*  Claimant relationship
     C                   if        not ValidGen(SCCLRL:'MCLR')
     C                   seton                                        71  90
     C                   leavesr
     C                   endif
     C*
     C*  TIN/SS#
     C                   if           (SCCLRL = 'E'
     C                              or SCCLRL = 'F'
     C                              or SCCLRL = 'O')
     C                             and SCSSTI = *zero
     C                   seton                                        72  90
     C                   leavesr
     C                   endif
     C*
     C                   if        CheckSS#(%editc(SCSSTI:'X')) = *on
     C                   seton                                        73  90
     C                   leavesr
     C                   endif
     C*
     C*  Name
     C                   if           (SCCLRL = 'E'
     C                              or SCCLRL = 'F'
     C                              or SCCLRL = 'O')
     C                              and (SCFSN = *blank
     C                                or SCLSN = *blank)
     C                   seton                                        74  90
     C                   leavesr
     C                   endif
     C*
     C*  Entity/Org Name
     C                   if           (SCCLRL = 'X'
     C                              or SCCLRL = 'Y'
     C                              or SCCLRL = 'Z')
     C                              and SCNAME = *blank
     C                   seton                                        75  90
     C                   leavesr
     C                   endif
     C*
     C*  Address
     C                   if        SCADR1 =  *blanks
     C                   seton                                        76  90
     C                   leavesr
     C                   endif
     C*
     C                   if        (SCCTY = *blanks
     C                               and (SCST  <> *blanks
     C                                 or SCZIP <> *zeros))
     C                             or
     C                             (SCST  = *blanks
     C                               and (SCCTY <> *blanks
     C                                 or SCZIP <> *zeros))
     C                             or
     C                             (SCZIP = *zeros
     C                               and (SCCTY <> *blanks
     C                                 or SCST  <> *blanks))
     C                   seton                                        83  90
     C                   leavesr
     C                   endif
     C*
     C                   if        SCZIP5 = *zeros
     C                              and SCZIP4 <> *zeros
     C                   seton                                        84  90
     C                   leavesr
     C                   endif
     C*
     C*  Country code
     C                   if        SCCTRY <> *blanks
     C                             and not ValidGen(SCCTRY:'CTRY')
     C                   seton                                        78  90
     C                   leavesr
     C                   endif
     C*
     C*  Providence/Postal Code
     C                   if        SCADR3 = *blanks
     C                              and SCCTRY = *blanks
     C                              and SCCTY = *blanks
     C                   seton                                        85  90
     C                   leavesr
     C                   endif
     C*
     C                   if        (SCCTRY <> 'USA'
     C                               and SCCTRY <> *blanks)
     C                             and SCADR3 = *blanks
     C                   seton                                        85  90
     C                   leavesr
     C                   endif
     C*
     C                   if        SCADR3 <> *blanks
     C                              and SCCTY <> *blanks
     C                   seton                                        82  90
     C                   leavesr
     C                   endif
     C*
     C*  Phone
     C                   if            SCCLRL <> *blanks
     C                             and SCAREA <= *zeros
     C                   seton                                        80  90
     C                   leavesr
     C                   endif
     C*
     C                   if        SCAREA <> *zeros
     C                             and SCPHN# <= *zeros
     C                   seton                                        81  90
     C                   leavesr
     C                   endif
     C*
     C* Check for screen updates
     C                   if        ScrUpdated = *on
     C                   seton                                        93  90
     C                   leavesr
     C                   endif
     C*
     C*_________
     C*
     C                   endsr
     C*
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C*
     C* Claimant Rep Indicator
     C                   if        SCCLRI <> *blank
     C                             and not ValidGen(SCCLRI:'CLRI')
     C                   seton                                        71  90
     C                   leavesr
     C                   endif
     C*
     C* TIN
     C                   if        SCCLRI = *blanks
     C                              and SCCRTI <> *zeros
     C                             or
     C                             SCCLRI <> *blanks
     C                              and SCCRTI = *zeros
     C                   seton                                        72  90
     C                   leavesr
     C                   endif
     C*
     C* Rep Name
     C                   if        SCCLRI = *blanks
     C                              and SCCRFN <> *blanks
     C                             or
     C                             SCCLRI <> *blanks
     C                              and SCCRFN = *blanks
     C*** Beg Add *** 05/18/10 ******************************************
     C                              and SCCRFM = *blanks
     C*** End Add *** 05/18/10 ******************************************
     C                   seton                                        73  90
     C                   leavesr
     C                   endif
     C*
     C                   if        SCCLRI = *blanks
     C                              and SCCRLN <> *blanks
     C                             or
     C                             SCCLRI <> *blanks
     C                              and SCCRLN = *blanks
     C*** Beg Add *** 05/18/10 ******************************************
     C                              and SCCRFM = *blanks
     C*** End Add *** 05/18/10 ******************************************
     C                   seton                                        74  90
     C                   leavesr
     C                   endif
     C*
     C* Firm Name
     C                   if        SCCLRI = *blanks
     C                              and SCCRFM <> *blanks
     C                             or
     C                             SCCLRI <> *blanks
     C                              and SCCRFM = *blanks
     C*** Beg Add *** 05/18/10 ******************************************
     C                              and SCCRFN = *blanks
     C                              and SCCRLN = *blanks
     C*** End Add *** 05/18/10 ******************************************
     C                   seton                                        75  90
     C                   leavesr
     C                   endif
     C*
     C* Address
     C                   if        SCCLRI = *blanks
     C                              and SCCRA1 <> *blanks
     C                             or
     C                             SCCLRI <> *blanks
     C                              and SCCRA1 = *blanks
     C                   seton                                        76  90
     C                   leavesr
     C                   endif
     C* * * *
     C* * * *            if        SCCLRI = *blanks
     C* * * *                       and SCCRA2 <> *blanks
     C* * * *                      or
     C* * * *                      SCCLRI <> *blanks
     C* * * *                       and SCCRA2 = *blanks
     C* * * *            seton                                        77  90
     C* * * *            leavesr
     C* * * *            endif
     C*
     C                   if        (SCCRCT = *blanks
     C                               and (SCCRST <> *blanks
     C                                 or SCCRZP <> *zeros))
     C                             or
     C                             (SCCRST  = *blanks
     C                               and (SCCRCT <> *blanks
     C                                 or SCCRZP <> *zeros))
     C                             or
     C                             (SCCRZP = *zeros
     C                               and (SCCRCT <> *blanks
     C                                 or SCCRST <> *blanks))
     C                   seton                                        83  90
     C                   leavesr
     C                   endif
     C*
     C                   if        SCCRZP5 = *zeros
     C                              and SCCRZP4 <> *zeros
     C                   seton                                        84  90
     C                   leavesr
     C                   endif
     C*
     C* Country code
     C                   if        SCRCRY <> *blank
     C                             and not ValidGen(SCRCRY:'CTRY')
     C                   seton                                        78  90
     C                   leavesr
     C                   endif
     C*
     C* Providence/Postal code
     C                   if        SCCLRI <> *blanks
     C                              and (SCCRA3 = *blanks
     C                               and SCRCRY = *blanks
     C                               and SCCRCT = *blanks)
     C                   seton                                        79  90
     C                   leavesr
     C                   endif
     C*
     C                   if        (SCRCRY <> 'USA'
     C                               and SCRCRY <> *blanks)
     C                             and SCCRA3 = *blanks
     C                   seton                                        79  90
     C                   leavesr
     C                   endif
     C*
     C                   if        SCCRA3 <> *blanks
     C                              and SCCRCT <> *blanks
     C                   seton                                        82  90
     C                   leavesr
     C                   endif
     C*
     C* Phone
     C                   if        SCCLRI = *blanks
     C                              and SCCRAC <> *zeros
     C                             or
     C                             SCCLRI <> *blanks
     C                              and SCCRAC = *zeros
     C                   seton                                        80  90
     C                   leavesr
     C                   endif
     C*
     C                   if        SCCRAC <> *zeros
     C                              and SCCRPH <= *zeros
     C                   seton                                        85  90
     C                   leavesr
     C                   endif
     C*
     C                   if        SCCREX <> *zeros
     C                              and SCCRAC <= *zeros
     C                   seton                                        86  90
     C                   leavesr
     C                   endif
     C*
     C* Check for screen updates
     C                   if        ScrUpdated = *on
     C                   seton                                        93  90
     C                   leavesr
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*================================================================
    C* Load screen headings
    C*================================================================
     C     LoadHdgs      begsr
     C*
     C* If claim was converted from another system, display old claim #
     C                   if        CMCVT# <> *blank
     C                   eval      scCvt = CMCVT#
     C                   eval      *in64 = *on
     C                   endif
     C*
     C* Agency Claim # (or, State Claim #)
     C                   eval      scSCl# = CMACL#
     C*
     C* Claims API
     C*
     C                   call      ClmAPI        ClmPl
     C*
     C* Get employer name
     C                   eval      scEDN = EmpDivName(CMEMP#
     C                                               :CMDIV
     C                                               :emRRN
     C                                               :CMFND
     C                                               :CMCO#
     C                                               )
     C*
     C* Get group name
     C                   eval      scFnd  = CMFND
     C                   eval      scFNam = GetGrpName(CMFND
     C                                                :fnRRN
     C                                                :CMCO#
     C                                                )
     C*
     C                   endsr
     C*
    C*================================================================
    C* Load initial maintenance fields
    C*================================================================
     C     InitMaintFlds begsr
     C*
     C                   eval      CACO#  = CMCO#
     C                   eval      CAFND  = CMFND
     C                   eval      CAFYR  = CMFYR
     C                   eval      CACASE = CMCASE
     C                   eval      CASEQ# = nextSeq#
     C*
     C                   exsr      MaintFlds
     C*
     C                   eval      CASTAT = 'A'
     C                   eval      CAIUSR = CAUSER
     C                   eval      CAIPGM = CAPGM
     C                   eval      CAIDT  = CADT
     C                   eval      CAITIM = CATIME
     C*
     C                   endsr
     C*
    C*================================================================
    C* Processing for F10
    C*================================================================
     C     kjDo          begsr
     C*
     C                   select
     C*__________
     C*
     C                   when      hfld = 'SCCTRY'
     C                   callp     GenSearch(SCCTRY:SCCTRYDesc)
     C*__________
     C*
     C                   when      hfld = 'SCRCRY'
     C                   callp     GenSearch(SCRCRY:SCRCRYDesc)
     C*__________
     C*
     C                   when      hfld = 'SCCLRL'
     C                   callp     GenSearch(SCCLRL:SCCLRLDesc)
     C*__________
     C*
     C                   when      hfld = 'SCCLRI'
     C                   callp     GenSearch(SCCLRI:SCCLRIDesc)
     C*__________
     C*
     C*__________
     C*
     C                   endsl
     C*
     C                   endsr
     C*
    C*================================================================
    C* Load maintenance fields
    C*================================================================
     C     MaintFlds     begsr
     C*
     C                   eval      CAUSER = q1user
     C                   eval      CAPGM  = q1pgm
     C                   eval      CADT   = DateTo7(SystemDate)
     C                   eval      CATIME = TimeTo6(SystemTime)
     C*
     C                   endsr
     C*
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      saveSCCLRL = CACLRL
     C*
     C*  SCR01
     C                   eval      SCCLRL = CACLRL
     C                   eval      SCCLRLDesc = GetGenDesc(CACLRL:'MCLR')
     C*
     C                   eval      SCSSTI = CASSTI
     C                   eval      SCFSN  = CAFSN
     C                   eval      SCLSN  = CALSN
     C                   eval      SCMID  = CAMID
     C                   eval      SCNAME = CANAME
     C                   eval      SCADR1 = CAADR1
     C                   eval      SCADR2 = CAADR2
     C                   eval      SCCTY  = CACTY
     C                   eval      SCST   = CAST
     C                   eval      SCZIP  = CAZIP
     C*
     C                   eval      SCCTRY = CACTRY
     C                   eval      SCCTRYDesc = GetGenDesc(CACTRY:'CTRY')
     C*
     C                   eval      SCADR3 = CAADR3
     C                   eval      SCAREA = CAAREA
     C                   eval      SCPHN# = CAPHN#
     C*** Beg Add *** 11/28/16 ******************************************
     C                   eval      SCEMDC = CAEMDC
     C*** End Add *** 11/28/16 ******************************************
     C*
     C*  SCR02
     C                   eval      SCCLRI = CACLRI
     C                   eval      SCCLRIDesc = GetGenDesc(CACLRI:'CLRI')
     C*
     C                   eval      SCCRTI = CACRTI
     C                   eval      SCCRFN = CACRFN
     C                   eval      SCCRLN = CACRLN
     C                   eval      SCCRFM = CACRFM
     C                   eval      SCCRA1 = CACRA1
     C                   eval      SCCRA2 = CACRA2
     C                   eval      SCCRCT = CACRCT
     C                   eval      SCCRST = CACRST
     C                   eval      SCCRZP = CACRZP
     C*
     C                   eval      SCRCRY = CARCRY
     C                   eval      SCRCRYDesc = GetGenDesc(CARCRY:'CTRY')
     C*
     C                   eval      SCCRA3 = CACRA3
     C                   eval      SCCRAC = CACRAC
     C                   eval      SCCRPH = CACRPH
     C                   eval      SCCREX = CACREX
     C*** Beg Add *** 11/28/16 ******************************************
     C                   eval      SCEMDR = CAEMDR
     C*** End Add *** 11/28/16 ******************************************
     C*
     C                   endsr
     C*
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C*  SCR01
     C                   eval      CACLRL = SCCLRL
     C                   eval      CASSTI = SCSSTI
     C                   eval      CAFSN  = SCFSN
     C                   eval      CALSN  = SCLSN
     C                   eval      CAMID  = SCMID
     C                   eval      CANAME = SCNAME
     C                   eval      CAADR1 = SCADR1
     C                   eval      CAADR2 = SCADR2
     C                   eval      CACTY  = SCCTY
     C                   eval      CAST   = SCST
     C                   eval      CAZIP  = SCZIP
     C                   eval      CACTRY = SCCTRY
     C                   eval      CAADR3 = SCADR3
     C                   eval      CAAREA = SCAREA
     C                   eval      CAPHN# = SCPHN#
     C*** Beg Add *** 11/28/16 ******************************************
     C                   eval      CAEMDC = SCEMDC
     C*** End Add *** 11/28/16 ******************************************
     C*
     C*  SCR02
     C                   eval      CACLRI = SCCLRI
     C                   eval      CACRTI = SCCRTI
     C                   eval      CACRFN = SCCRFN
     C                   eval      CACRLN = SCCRLN
     C                   eval      CACRFM = SCCRFM
     C                   eval      CACRA1 = SCCRA1
     C                   eval      CACRA2 = SCCRA2
     C                   eval      CACRCT = SCCRCT
     C                   eval      CACRST = SCCRST
     C                   eval      CACRZP = SCCRZP
     C                   eval      CARCRY = SCRCRY
     C                   eval      CACRA3 = SCCRA3
     C                   eval      CACRAC = SCCRAC
     C                   eval      CACRPH = SCCRPH
     C                   eval      CACREX = SCCREX
     C*** Beg Add *** 11/28/16 ******************************************
     C                   eval      CAEMDR = SCEMDR
     C*** End Add *** 11/28/16 ******************************************
     C*
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C*
     C     *inkj         caseq     *on           KJDO                     90
     C                   endcs
     C*
     C                   endsr
     C*
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C*
     C                   eval      *inlr = *on
     C*
     C                   endsr
     C*
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C                   eval      *in40 = @@Mode = 'A'
     C                   eval      *in09 = @@Mode = 'D'
     C*
     C                   eval      addMode = @@Mode = 'A'
     C                   eval      dspMode = @@Mode = 'D'
     C                   eval      chgMode = @@Mode = 'C'
     C*
     C* Chain out to claim file
     C     @@rrn2        chain     wmcmp
     C*
     C* Maximum of 4 records allowed
     C                   if        addMode
     C                   clear                   nextSeq#
     C                   clear                   countWDCAL
     C                   clear                   CASEQ#
     C     keyWDCAL      setgt     wdcal
     C                   dou       %eof
     C     keyWDCAL      readpe    wdcal
     C                   if        not %eof and CASTAT = 'A'
     C                   eval      countWDCAL = countWDCAL + 1
     C                   endif
     C                   if        nextSeq# = *zero
     C                   eval      nextSeq# = CASEQ# + 1
     C                   endif
     C                   enddo
     C                   if        countWDCAL >= 4
     C                   eval      @@Mode = '4'
     C                   eval      *inlr = *on
     C                   return
     C                   endif
     C                   endif
     C*
     C                   exsr      LoadHdgs
     C*
     C                   endsr
     C*
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C*
     C                   select
     C                   when      @@Mode = 'S'
     C                   exsr      ChgSts
     C                   return
     C                   endsl
     C*
     C                   endsr
     C*
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
     C*
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   exsr      MaintFlds
     C                   update    dbrcdf
E01  C                   endif
     C*
     C                   endsr
     C*
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C*
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   exsr      InitMaintFlds
     C                   write     dbrcdf
     C                   feod      wdcap
     C*
     C                   eval      outFlg = *on
     C*
     C                   endsr





     P*====================================================================
     P* Check SS# doesn't already exist
     P*====================================================================
     PCheckSS#         b
     DCheckSS#         pi             1n
     D inSS#                               like(CMSS#)  const
     D*
     D ss#Tin          s                   like(CASSTI)
     D CAexists        s              1n
     D*
     C     keyWDCAL2     klist
     C                   kfld                    CMCO#
     C                   kfld                    CMFND
     C                   kfld                    CMFYR
     C                   kfld                    CMCASE
     C                   kfld                    ss#Tin
     C*
     C                   move      inSS#         ss#Tin
     C     keyWDCAL2     setll     wdcal2
     C                   dou       %eof
     C                              or (CArrn2 <> @@rrn and @@rrn <> *zeros)
     C     keyWDCAL2     reade     wdcal2
     C                   enddo
     C                   eval      CAexists = not %eof
     C*
     C                   select
     C                   when      inSS# = *zeros
     C                   return    *off
     C                   when      inSS# = CMSS#
     C                   return    *on
     C                   when      CAexists
     C                   return    *on
     C                   other
     C                   return    *blanks
     C                   endsl
     C*
     PCheckSS#         e





     P*====================================================================
     P* Get generic table descriptions
     P*====================================================================
     PGetGenDesc       b
     DGetGenDesc       pi                  like(GTDESC)
     D inKey                         10    options(*varsize) const
     D inTableId                           like(GTTBID)      const
     D*
     C     keyWTGTL      klist
     C                   kfld                    CMCO#
     C                   kfld                    inTableId
     C                   kfld                    inKey
     C*
     C     keyWTGTL      chain     WTGTL
     C                   select
     C                   when      %found
     C                   return    GTDESC
     C                   other
     C                   return    *blanks
     C                   endsl
     C*
     PGetGenDesc       e





     P*====================================================================
     P* Generic table searches
     P*====================================================================
     PGenSearch        b
     DGenSearch        pi                  opdesc
     D inKey                         10    options(*varsize)
     D inDesc                        40    options(*varsize:*nopass)
     D*
     D##PrDs           s                   like(SCPRDS)
     D##Call           s              1
     D##TableId        s                   like(GTTBID)
     D##Key            s                   like(GTKEY)
     D##RtnCd          s              1
     DoutKey           s             10
     DoutDesc          s             40
     Did               s              3  0 inz(1)
     D descType        s             10I 0
     D dataType        s             10I 0
     D descInfo1       s             10I 0
     D descInfo2       s             10I 0
     D inLen           s             10I 0
     D hexLen          s             10I 0
     D*
     DgenInfo          ds
     D genTable                1      5
     D notSelected             7      8  0
     D selected                9     10  0
     C*
     C                   callp     CEEDOD(1:descType:dataType:descInfo1:
     C                             descInfo2:inLen:*omit)
     C*
     C                   eval      *in89 = *on
     C*
     C     hfld          lookup    genSrchId(id)                          99
     C                   if        not %found
     C                   return
     C                   endif
     C*
     C                   eval      outKey  = inKey
     C                   if        %parms >= 2
     C                   eval      outDesc = inDesc
     C                   endif
     C*
     C                   eval      genInfo = genDetail(id)
     C                   eval      ##TableId = genTable
     C*
     C                   call      'W2493R'
     C                   parm      SCPRDS        ##PrDs
     C                   parm      *blanks       ##Call
     C                   parm                    ##TableId
     C                   parm      *blanks       ##Key
     C                   parm                    ##RtnCd
     C*
     C                   select
     C                   when      ##RtnCd = 'A'
     C                   eval      *inkc = *on
     C                   when      ##RtnCd = 'B' or ##Key = *blanks
     C                   eval      *in(notSelected) = *on
     C                   when      ##RtnCd = '*'
     C                   eval      *in(selected) = *on
     C                   eval      outKey  = ##Key
     C                   eval      outDesc = GetGenDesc(##Key:##TableId)
     C                   endsl
     C*
     C                   eval      %subst(inKey:1:inLen) = outKey
     C                   if        %parms >= 2
     C                   eval      inDesc = outDesc
     C                   endif
     C*
     C                   return
     PGenSearch        e





     P*====================================================================
     P* Determine if screen has been updated
     P*====================================================================
     PScrUpdated       b
     DScrUpdated       pi             1n
     D*
     C                   select
     C                   when      CADT > saveDt
     C                              or (CADT = saveDt and CATIME > saveTm)
     C                   return    *on
     C                   other
     C                   return    *off
     C                   endsl
     C*
     PScrUpdated       e





     P*====================================================================
     P* Validate generic table values
     P*====================================================================
     PValidGen         b
     DValidGen         pi             1n   opdesc
     D inKey                         10    options(*varsize) const
     D inTableId                           like(GTTBID)      const
     D*
     D key             s                   like(GTKEY)
     D descType        s             10I 0
     D dataType        s             10I 0
     D descInfo1       s             10I 0
     D descInfo2       s             10I 0
     D inLen           s             10I 0
     D hexLen          s             10I 0
     D*
     C     keyWTGTL      klist
     C                   kfld                    CMCO#
     C                   kfld                    inTableId
     C                   kfld                    key
     C*
     C                   callp     CEEDOD(1:descType:dataType:descInfo1:
     C                             descInfo2:inLen:*omit)
     C                   eval      key = %subst(inKey:1:inLen)
     C     keyWTGTL      chain     WTGTL
     C                   return    %found
     C*
     PValidGen         e





** Id     Key   N S
SCCLRL    MCLR  7172
SCCTRY    CTRY  7879
SCCLRI    CLRI  7172
SCRCRY    CTRY  7887
