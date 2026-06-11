     hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio: *srcstmt)
     h debug(*yes)

      /title SRmiscI02 - "Miscellaneous Endorsement Info/Maint" Add Info
      *****************************************************************
      * DESCRIPTION:
      *
      * WC990301 - Miscellaneous Change Endorsement - clt139
      *
      * "Info/Maint" - Endorsement "Addtional Information" program
      *              - Multiple, Manual with Additional Information.
      *              - Mid-term processing capable.
      *
      * CALLED FROM: SRmiscI
      * CALLS TO:    None
      *
      *****************************************************************
      * REVISED:
      *
      * 05/24/10 C:61  PROJECT 139WLR
      *                Do not allow entry in DISPLAY mode.
      *
      * 10/13/09 C:61  PROJECT 139WLR
      *                Created.
      *
      *****************************************************************

      * display file
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FSRMISCF02 cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    
     *  Physical file definition (for maintaining record)
     Fwlmsp     uf a e             disk
     F                                     rename(pms01:dbrcdf)
    
      *  Midterm Endorsement Table File
     Fwtmyp     if   e             disk
    
      * Misc Endorsement
     Fwlmsl     if   e           k disk

      * Endorsement/DARTS Information File
     Fwmdxl     if   e           K disk

      * Mid-term Endorse Cover Data File
     Fwmenl3    if a e           k disk

      * Group Auxiliary File
     Fwdf2l     if   e           k disk
    
      * group
     Fwmfnl     if   e           k disk
    
      * employer
     Fwmeml     if   e           k disk
    
      * employer policy year
     Fwdelp     if   e             disk
    
      * employer cross reference
     Fwzexl     if   e           k disk
     F                                     rename(pex01:pex02)
    

     D efDS            ds
     D  EMEFD1
     D  EMEFD2
     D  EMEFD3
     D  EMEFD4
     D  EMEFD5
     D ef                                  OVERLAY(efDS)
     D                                     LIKE(EMEFD1) DIM(5)

     D cxDS            ds
     D  EMCAD1
     D  EMCAD2
     D  EMCAD3
     D  EMCAD4
     D  EMCAD5
     D cx                                  OVERLAY(cxDS)
     D                                     LIKE(EMCAD1) DIM(5)

    ** Midterm data area
     D svlda           ds          1024
     D ldend#                         5  0
     D  ldenda                        5    overlay(ldend#)
     D ldmyrr                         9  0
     D ldeedt                         8  0
     D ldfyr                          3  0
     D ldpyrl                         1


    D*================================================================
    D*  /COPY required constants
    D*================================================================

     D optnm           c                   'MISCELLANEOUS ENDORSEMENT'
     D comapi          c                   'WA001R'
     D #ofscr          c                   3


    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------

     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D @@qte           s              1
     d ##emp#          s                   like(ememp#)
     d ##div           s                   like(emdiv)
     d ##name          s                   like(emname)
     d ##fyr           s                   like(elfyr)
     d ##agt#          s              5  0
     d ##anam          s             60
     d ##prod          s              5  0
     d ##pnam          s             60
     d x               s              7  0
     d sysdt           s              7  0

     d xxseq#          s                   like(msseq#)

     d svseq#          s                   like(msseq#)

     d svl001          s                   like(msl001)
     d svl002          s                   like(msl002)
     d svl003          s                   like(msl003)
     d svl004          s                   like(msl004)
     d svl005          s                   like(msl005)
     d svl006          s                   like(msl006)
     d svl007          s                   like(msl007)
     d svl008          s                   like(msl008)
     d svl009          s                   like(msl009)
     d svl010          s                   like(msl010)
     d svl011          s                   like(msl011)
     d svl012          s                   like(msl012)
     d svl013          s                   like(msl013)
     d svl014          s                   like(msl014)
     d svl015          s                   like(msl015)
     d svl016          s                   like(msl016)
     d svl017          s                   like(msl017)
     d svl018          s                   like(msl018)
     d svl019          s                   like(msl019)
     d svl020          s                   like(msl020)
     d svl021          s                   like(msl021)
     d svl022          s                   like(msl022)
     d svl023          s                   like(msl023)
     d svl024          s                   like(msl024)
     d svl025          s                   like(msl025)
     d svl026          s                   like(msl026)
     d svl027          s                   like(msl027)
     d svl028          s                   like(msl028)
     d svl029          s                   like(msl029)
     d svl030          s                   like(msl030)
     d svl031          s                   like(msl031)
     d svl032          s                   like(msl032)
     d svl033          s                   like(msl033)
     d svl034          s                   like(msl034)
     d svl035          s                   like(msl035)
     d svl036          s                   like(msl036)
     d svl037          s                   like(msl037)
     d svl038          s                   like(msl038)
     d svl039          s                   like(msl039)

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

     C/COPY SCOPYMT01C


     *================================================================
     *  PLIST definitions
     *================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
      *                                     endorsement specific data file
     C                   parm                    @@rrn
      *                                     wdelp
     C                   parm                    @@rrn2
     C                   parm                    @@pgm            10            Program Name
     C                   parm                    @@acd             1            Program Name
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
      *
     c     agtpl         plist
     c                   PARM                    ##CO#
     c                   PARM      elfnd         ##FND
     c                   PARM      elemp#        ##EMP#
     c                   PARM      eldiv         ##DIV
     c                   PARM      elfyr         ##FYR
     c     scagt#        PARM                    ##AGT#
     c                   PARM                    ##ANAM
     c     scprod        PARM                    ##PROD
     c                   PARM                    ##PNAM


     *================================================================
     *  KLIST definitions
     *================================================================

      *  Group Auxiliary file

     C     F2KY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND

      *  Endorsement Request workfile

     C     enky          klist
     C                   kfld                    emco#
     C                   kfld                    emfnd
     C                   kfld                    ememp#
     C                   kfld                    emdiv
     C                   kfld                    ldfyr
     C                   kfld                    q1jobn
     C                   kfld                    ldend#

      *  Miscellaneous Change Endorsement

     c     msky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elfyr
     c                   kfld                    elemp#

     c     fnky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd

     c     emky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv

     c     zexky         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv

      *  Endors/DARTS Control Definition file

     c     dxky          KLIST
     c                   kfld                    ##co#
     c                   kfld                    ##fnd

      *=========================================================================
      *  CKCHGS - SUBROUTINE TO CREATE MID-TERM ENDORSEMENT RECORDS
      *=========================================================================
     C     CKCHGS        BEGSR

      * if mid-term endorsement
     c                   if        (ldenda <> *zeros)
     c                             and (ldenda <> *blanks)

      * if add/change/delete of endorsement

     c                   if        (msseq# <> svseq#)
     c                             or (msl001 <> svl001)
     c                             or (msl002 <> svl002)
     c                             or (msl003 <> svl003)
     c                             or (msl004 <> svl004)
     c                             or (msl005 <> svl005)
     c                             or (msl006 <> svl006)
     c                             or (msl007 <> svl007)
     c                             or (msl008 <> svl008)
     c                             or (msl009 <> svl009)
     c                             or (msl010 <> svl010)
     c                             or (msl011 <> svl011)
     c                             or (msl012 <> svl012)
     c                             or (msl013 <> svl013)
     c                             or (msl014 <> svl014)
     c                             or (msl015 <> svl015)
     c                             or (msl016 <> svl016)
     c                             or (msl017 <> svl017)
     c                             or (msl018 <> svl018)
     c                             or (msl019 <> svl019)
     c                             or (msl020 <> svl020)
     c                             or (msl021 <> svl021)
     c                             or (msl022 <> svl022)
     c                             or (msl023 <> svl023)
     c                             or (msl024 <> svl024)
     c                             or (msl025 <> svl025)
     c                             or (msl026 <> svl026)
     c                             or (msl027 <> svl027)
     c                             or (msl028 <> svl028)
     c                             or (msl029 <> svl029)
     c                             or (msl030 <> svl030)
     c                             or (msl031 <> svl031)
     c                             or (msl032 <> svl032)
     c                             or (msl033 <> svl033)
     c                             or (msl034 <> svl034)
     c                             or (msl035 <> svl035)
     c                             or (msl036 <> svl036)
     c                             or (msl037 <> svl037)
     c                             or (msl038 <> svl038)
     c                             or (msl039 <> svl039)

     C*  Get Endors/DARTS information

     C     dxky          chain     pdx01

     C*  Setup for next request number

     c     enky          setgt     pen01
     c     enky          readpe    pen01                                  60
     c   60              clear                   pen01
     c                   eval      enreq# = enreq# + 1

     c                   move      EMCO#         ENCO#
     c                   move      EMFND         enfnd
     c                   MOVE      EMEMP#        ENEMP#
     c                   MOVE      EMDIV         ENDIV
     c                   MOVE      LDFYR         ENFYR
     c                   MOVE      Q1JOBN        ENJOB
     c                   MOVE      LDEND#        ENEND#

     c                   Z-ADD     LDEEDT        SRDTI
     c                   CALL      'SRCVTDAT'    DTCYMD
     c                   MOVEL     SRDTO         ENEEDT

     c                   MOVEL(p)  EMNAME        ENNAM1
     c                   MOVEL(p)  EMFDNM        ENFDN1

     C                   MOVEL     EMNAME        ENENM1
     C                   MOVEL(p)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVEl(p)  EMNAM2        ENENM2

     C                   MOVEL     EMFDNM        ENEFN1
     C                   MOVEL(p)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      EMFDN2        ENEFN2

     C                   MOVEL(p)  EMCONT        ENCON1
     C                   MOVEL(p)  EMATTN        ENATT1
     C                   MOVEL(p)  EMCNT3        ENCN31
     C                   MOVE      EMFEIN        ENFEN1
     C                   MOVEL(p)  EMTPOB        ENTOB1
     C                   MOVEL(p)  EMMAD1        ENMA11
     C                   MOVEL(p)  EMMAD2        ENMA21
     C                   MOVEL(p)  EMMCTY        ENMCT1
     C                   MOVEL(p)  EMMST         ENMST1
     C                   MOVEL(p)  EMMZIP        ENMZP1
     C                   MOVEL(p)  EMPAD1        ENPA11
     C                   MOVEL(p)  EMPAD2        ENPA21
     C                   MOVEL(p)  EMPCTY        ENPCT1
     C                   MOVEL(p)  EMPST         ENPST1
     C                   MOVEL(p)  EMPZIP        ENPZP1

     C                   MOVE      svseq#        ENOFI1                         seq#
     C                   MOVEL(p)  svl001        ENOEN1

     C                   MOVEL(p)  DXMISC        ENRMCD

     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
      *
     C     LDMYRR        CHAIN     pmy01
     C                   MOVEL(p)  MYDSC1        ENDSC1
     C                   MOVEL(p)  MYDSC2        ENDSC2
     C                   MOVEL(p)  MYDSC3        ENDSC3
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
     C*
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
     C     F2KY          CHAIN     PF201
     C                   MOVE      F2CAR#        ENCAR1
     C                   Z-ADD     ELPFDT        ENPFD1

      *  Write the record

     C                   MOVEL     *blanks       ENbLIN
      *
     c                   if        @@acd <> 'D'
     C                   eval      enalin = 'See attached forms.'
     c                   else
     c                   eval      enalin = *blanks
     c                   endif
      *
     C                   eval      enmisc = @@acd
      *
     C                   WRITE     PEN01
     C                   ENDIF
      *
     c                   movel(p)  msseq#        svseq#
     c                   movel(p)  msl001        svl001
     c                   movel(p)  msl002        svl002
     c                   movel(p)  msl003        svl003
     c                   movel(p)  msl004        svl004
     c                   movel(p)  msl005        svl005
     c                   movel(p)  msl006        svl006
     c                   movel(p)  msl007        svl007
     c                   movel(p)  msl008        svl008
     c                   movel(p)  msl009        svl009
     c                   movel(p)  msl010        svl010
     c                   movel(p)  msl011        svl011
     c                   movel(p)  msl012        svl012
     c                   movel(p)  msl013        svl013
     c                   movel(p)  msl014        svl014
     c                   movel(p)  msl015        svl015
     c                   movel(p)  msl016        svl016
     c                   movel(p)  msl017        svl017
     c                   movel(p)  msl018        svl018
     c                   movel(p)  msl019        svl019
     c                   movel(p)  msl020        svl020
     c                   movel(p)  msl021        svl021
     c                   movel(p)  msl022        svl022
     c                   movel(p)  msl023        svl023
     c                   movel(p)  msl024        svl024
     c                   movel(p)  msl025        svl025
     c                   movel(p)  msl026        svl026
     c                   movel(p)  msl027        svl027
     c                   movel(p)  msl028        svl028
     c                   movel(p)  msl029        svl029
     c                   movel(p)  msl030        svl030
     c                   movel(p)  msl031        svl031
     c                   movel(p)  msl032        svl032
     c                   movel(p)  msl033        svl033
     c                   movel(p)  msl034        svl034
     c                   movel(p)  msl035        svl035
     c                   movel(p)  msl036        svl036
     c                   movel(p)  msl037        svl037
     c                   movel(p)  msl038        svl038
     c                   movel(p)  msl039        svl039
     C                   ENDIF

     C                   ENDSR


     *================================================================
     * DLTPRE - Pre-processing before delete occurs
     *================================================================
     C     dltpre        begsr
      *
     c                   movel(p)  msseq#        svseq#
     c                   movel(p)  msl001        svl001
     c                   movel(p)  msl002        svl002
     c                   movel(p)  msl003        svl003
     c                   movel(p)  msl004        svl004
     c                   movel(p)  msl005        svl005
     c                   movel(p)  msl006        svl006
     c                   movel(p)  msl007        svl007
     c                   movel(p)  msl008        svl008
     c                   movel(p)  msl009        svl009
     c                   movel(p)  msl010        svl010
     c                   movel(p)  msl011        svl011
     c                   movel(p)  msl012        svl012
     c                   movel(p)  msl013        svl013
     c                   movel(p)  msl014        svl014
     c                   movel(p)  msl015        svl015
     c                   movel(p)  msl016        svl016
     c                   movel(p)  msl017        svl017
     c                   movel(p)  msl018        svl018
     c                   movel(p)  msl019        svl019
     c                   movel(p)  msl020        svl020
     c                   movel(p)  msl021        svl021
     c                   movel(p)  msl022        svl022
     c                   movel(p)  msl023        svl023
     c                   movel(p)  msl024        svl024
     c                   movel(p)  msl025        svl025
     c                   movel(p)  msl026        svl026
     c                   movel(p)  msl027        svl027
     c                   movel(p)  msl028        svl028
     c                   movel(p)  msl029        svl029
     c                   movel(p)  msl030        svl030
     c                   movel(p)  msl031        svl031
     c                   movel(p)  msl032        svl032
     c                   movel(p)  msl033        svl033
     c                   movel(p)  msl034        svl034
     c                   movel(p)  msl035        svl035
     c                   movel(p)  msl036        svl036
     c                   movel(p)  msl037        svl037
     c                   movel(p)  msl038        svl038
     c                   movel(p)  msl039        svl039
     c                   clear                   msseq#
     C                   clear                   msl001
     C                   clear                   msl002
     C                   clear                   msl003
     C                   clear                   msl004
     C                   clear                   msl005
     C                   clear                   msl006
     C                   clear                   msl007
     C                   clear                   msl008
     C                   clear                   msl009
     C                   clear                   msl010
     C                   clear                   msl011
     C                   clear                   msl012
     C                   clear                   msl013
     C                   clear                   msl014
     C                   clear                   msl015
     C                   clear                   msl016
     C                   clear                   msl017
     C                   clear                   msl018
     C                   clear                   msl019
     C                   clear                   msl020
     C                   clear                   msl021
     C                   clear                   msl022
     C                   clear                   msl023
     C                   clear                   msl024
     C                   clear                   msl025
     C                   clear                   msl026
     C                   clear                   msl027
     C                   clear                   msl028
     C                   clear                   msl029
     C                   clear                   msl030
     C                   clear                   msl031
     C                   clear                   msl032
     C                   clear                   msl033
     C                   clear                   msl034
     C                   clear                   msl035
     C                   clear                   msl036
     C                   clear                   msl037
     C                   clear                   msl038
     C                   clear                   msl039
     C                   exsr      ckchgs
     C                   endsr


     *================================================================
     * DSPLYR - Processing after displaying information
     *================================================================
     C     dsplyr        begsr
     C                   endsr


     *================================================================
     * ERRCK1 - Error checking for screen 1 (SCR01)
     *================================================================
     C     errck1        begsr
     C                   endsr


     *================================================================
     * ERRCK2 - Error checking for screen 2 (SCR02)
     *================================================================
     C     errck2        begsr
     C                   endsr


     *================================================================
     * ERRCK3 - Error checking for screen 3 (SCR03)
     *================================================================
     C     errck3        begsr
     C                   endsr


     *================================================================
     * MVDBSC - Move data base fields to screen fields
     *================================================================
     C     mvdbsc        begsr

     c                   movel(p)  msl001        scl001
     c                   movel(p)  msl002        scl002
     c                   movel(p)  msl003        scl003
     c                   movel(p)  msl004        scl004
     c                   movel(p)  msl005        scl005
     c                   movel(p)  msl006        scl006
     c                   movel(p)  msl007        scl007
     c                   movel(p)  msl008        scl008
     c                   movel(p)  msl009        scl009
     c                   movel(p)  msl010        scl010
     c                   movel(p)  msl011        scl011
     c                   movel(p)  msl012        scl012
     c                   movel(p)  msl013        scl013
     c                   movel(p)  msl014        scl014
     c                   movel(p)  msl015        scl015
     c                   movel(p)  msl016        scl016
     c                   movel(p)  msl017        scl017
     c                   movel(p)  msl018        scl018
     c                   movel(p)  msl019        scl019
     c                   movel(p)  msl020        scl020
     c                   movel(p)  msl021        scl021
     c                   movel(p)  msl022        scl022
     c                   movel(p)  msl023        scl023
     c                   movel(p)  msl024        scl024
     c                   movel(p)  msl025        scl025
     c                   movel(p)  msl026        scl026
     c                   movel(p)  msl027        scl027
     c                   movel(p)  msl028        scl028
     c                   movel(p)  msl029        scl029
     c                   movel(p)  msl030        scl030
     c                   movel(p)  msl031        scl031
     c                   movel(p)  msl032        scl032
     c                   movel(p)  msl033        scl033
     c                   movel(p)  msl034        scl034
     c                   movel(p)  msl035        scl035
     c                   movel(p)  msl036        scl036
     c                   movel(p)  msl037        scl037
     c                   movel(p)  msl038        scl038
     c                   movel(p)  msl039        scl039

     c                   endsr


     *================================================================
     * MVSCDB - Move screen fields to data base fields
     *================================================================
     C     mvscdb        begsr

     c                   eval      msco#  = elco#
     c                   eval      msfnd  = elfnd
     c                   eval      msfyr  = elfyr
     c                   eval      msemp# = elemp#
     c                   eval      msdiv  = eldiv

     c                   movel(p)  scl001        msl001
     c                   movel(p)  scl002        msl002
     c                   movel(p)  scl003        msl003
     c                   movel(p)  scl004        msl004
     c                   movel(p)  scl005        msl005
     c                   movel(p)  scl006        msl006
     c                   movel(p)  scl007        msl007
     c                   movel(p)  scl008        msl008
     c                   movel(p)  scl009        msl009
     c                   movel(p)  scl010        msl010
     c                   movel(p)  scl011        msl011
     c                   movel(p)  scl012        msl012
     c                   movel(p)  scl013        msl013
     c                   movel(p)  scl014        msl014
     c                   movel(p)  scl015        msl015
     c                   movel(p)  scl016        msl016
     c                   movel(p)  scl017        msl017
     c                   movel(p)  scl018        msl018
     c                   movel(p)  scl019        msl019
     c                   movel(p)  scl020        msl020
     c                   movel(p)  scl021        msl021
     c                   movel(p)  scl022        msl022
     c                   movel(p)  scl023        msl023
     c                   movel(p)  scl024        msl024
     c                   movel(p)  scl025        msl025
     c                   movel(p)  scl026        msl026
     c                   movel(p)  scl027        msl027
     c                   movel(p)  scl028        msl028
     c                   movel(p)  scl029        msl029
     c                   movel(p)  scl030        msl030
     c                   movel(p)  scl031        msl031
     c                   movel(p)  scl032        msl032
     c                   movel(p)  scl033        msl033
     c                   movel(p)  scl034        msl034
     c                   movel(p)  scl035        msl035
     c                   movel(p)  scl036        msl036
     c                   movel(p)  scl037        msl037
     c                   movel(p)  scl038        msl038
     c                   movel(p)  scl039        msl039

     C                   endsr


     *================================================================
     * OTHERR - Processing for other modes
     *================================================================
     C     otherr        begsr
     C                   endsr


     *================================================================
     * OTHFKS - Processing for other functions keys
     *================================================================
     C     othfks        begsr
     C                   endsr


     *================================================================
     * PGMEND - End of program processing
     *================================================================
     C     pgmend        begsr
     c                   eval      *inlr = *on
     C                   endsr


     *================================================================
     * PGMSTR - Start of program processing
     *================================================================
     C     pgmstr        begsr

     c                   select

     c                   when      @@mode = 'A'
     c                   eval      *in40 = *on


     c                   when      @@mode = 'D'
     c                   eval      *in09 = *on

     c                   endsl

      *  Load Screen Headings

     c     @@rrn2        chain     pel01                              60
     c     emky          chain     pem01                              60
     c                   eval      scfyr  = elfyr
     c                   eval      scfnd  = emfnd

      * get fund name

     c                   eval      scfnam = *blanks
     c     fnky          chain     pfn01
     c                   if        %found
     c                   if        fnanam <> *blanks
     c                   eval      scfnam = fnanam
     c                   else
     c                   eval      scfnam = fnname
     c                   endif
     c                   endif

     c                   if        elfaf = 'A'
     c                   eval      scpyst = 'Audited'
     c                   else
     c                   eval      scpyst = 'Estimated'
     c                   endif
      *
      *  Concat Employer/Division/Name
      *
     c                   CALL      'WA005R'
     c                   parm      ememp#        ##emp#
     c                   parm      emdiv         ##div
     c                   parm      emname        ##name
     c                   parm                    ##edn            80
      *
     c                   eval      scedn = ##edn

      *  get Cancel date

     c                   eval      x = 5
      *
     c                   dow       (ef(x) = 0) and (x > 1)
     c                   eval      x = x - 1
     c                   enddo
      *
     c                   eval      *in41 = *off
      *
     c                   if        (cx(x) <> 0)
     c                   eval      srdti = cx(x)
     c                   CALL      'SRCVTDAT'    DTMDYY
     c                   movel     srdto         sccxdt
     c                   eval      *in41 = *on
     c                   endif

     c                   time                    DSDTTM
     c                   z-add     dsdate        srdti
     c                   CALL      'SRCVTDAT'    dtcymd
     c                   movel     srdto         sysdt

      *  Get Agent/Producer Information

     c                   call      'WA002R'      AGTPL
     c                   eval      scanam = ##anam
     c                   eval      scpnam = ##pnam

      *  Get Employer Cross Reference info for header

     c     zexky         chain     pex02                              63

     c                   if        %found
     c                   eval      scofnd = exofnd
     c                   eval      scoemp = exoemp
     c                   eval      scodiv = exodiv
     c                   endif

     C     *dtaara       Define    *lda          svlda
     C     *lock         in        *dtaara
     C                   out       *dtaara

      *
      *  Save the old record values
      *
     c     @@rrn         CHAIN     dbrcdf

     c                   if        %found
     c                   clear                   svseq#
     C                   clear                   svl001
     C                   clear                   svl002
     C                   clear                   svl003
     C                   clear                   svl004
     C                   clear                   svl005
     C                   clear                   svl006
     C                   clear                   svl007
     C                   clear                   svl008
     C                   clear                   svl009
     C                   clear                   svl010
     C                   clear                   svl011
     C                   clear                   svl012
     C                   clear                   svl013
     C                   clear                   svl014
     C                   clear                   svl015
     C                   clear                   svl016
     C                   clear                   svl017
     C                   clear                   svl018
     C                   clear                   svl019
     C                   clear                   svl020
     C                   clear                   svl021
     C                   clear                   svl022
     C                   clear                   svl023
     C                   clear                   svl024
     C                   clear                   svl025
     C                   clear                   svl026
     C                   clear                   svl027
     C                   clear                   svl028
     C                   clear                   svl029
     C                   clear                   svl030
     C                   clear                   svl031
     C                   clear                   svl032
     C                   clear                   svl033
     C                   clear                   svl034
     C                   clear                   svl035
     C                   clear                   svl036
     C                   clear                   svl037
     C                   clear                   svl038
     C                   clear                   svl039

     C                   else

     c                   movel(p)  msseq#        svseq#
     c                   movel(p)  msl001        svl001
     c                   movel(p)  msl002        svl002
     c                   movel(p)  msl003        svl003
     c                   movel(p)  msl004        svl004
     c                   movel(p)  msl005        svl005
     c                   movel(p)  msl006        svl006
     c                   movel(p)  msl007        svl007
     c                   movel(p)  msl008        svl008
     c                   movel(p)  msl009        svl009
     c                   movel(p)  msl010        svl010
     c                   movel(p)  msl011        svl011
     c                   movel(p)  msl012        svl012
     c                   movel(p)  msl013        svl013
     c                   movel(p)  msl014        svl014
     c                   movel(p)  msl015        svl015
     c                   movel(p)  msl016        svl016
     c                   movel(p)  msl017        svl017
     c                   movel(p)  msl018        svl018
     c                   movel(p)  msl019        svl019
     c                   movel(p)  msl020        svl020
     c                   movel(p)  msl021        svl021
     c                   movel(p)  msl022        svl022
     c                   movel(p)  msl023        svl023
     c                   movel(p)  msl024        svl024
     c                   movel(p)  msl025        svl025
     c                   movel(p)  msl026        svl026
     c                   movel(p)  msl027        svl027
     c                   movel(p)  msl028        svl028
     c                   movel(p)  msl029        svl029
     c                   movel(p)  msl030        svl030
     c                   movel(p)  msl031        svl031
     c                   movel(p)  msl032        svl032
     c                   movel(p)  msl033        svl033
     c                   movel(p)  msl034        svl034
     c                   movel(p)  msl035        svl035
     c                   movel(p)  msl036        svl036
     c                   movel(p)  msl037        svl037
     c                   movel(p)  msl038        svl038
     c                   movel(p)  msl039        svl039
     c                   endif

     C                   endsr


     *================================================================
     * PREOTH - Pre-processing for other modes
     *================================================================
     C     preoth        begsr
     C                   endsr


     *================================================================
     * UPDATR - Update data base record and after processing
     *================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99

B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     c                   eval      msprt# = ldend#
     C                   eval      msusr  = q1user
     C                   eval      mspgm = q1pgm
     C                   time                    dsdttm
     C                   eval      mstim  = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         msdat
     C                   update    dbrcdf
     C                   exsr      ckchgs
     c                   eval      outflg = *on
E01  C                   endif

     C                   endsr


     *================================================================
     * WRITER - Write data base record and after processing
     *================================================================
     C     writer        begsr

      * get next sequence number

     c     msky          setgt     pms01
     c     msky          readpe    pms01

     c                   if        not(%eof)
     c                   eval      xxseq# = msseq# + 1
     c                   else
     c                   eval      xxseq# = 1
     c                   endif

      * write new record to file

     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     c                   eval      msseq# = xxseq#
     c                   eval      msend# = ldend#
     c                   eval      msprt# = ldend#

     C                   eval      msusr  = q1user
     C                   eval      msiusr = q1user
     C                   eval      mspgm  = q1pgm
     C                   eval      msipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      mstim  = dstime
     C                   eval      msitim = dstime
     C                   eval      srdti  = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         msdat
     C                   movel(p)  srdto         msidat
     C                   write     dbrcdf
      *
     C                   feod      wlmsp
     C                   exsr      ckchgs
     c                   eval      outflg = *on
     C                   endsr

