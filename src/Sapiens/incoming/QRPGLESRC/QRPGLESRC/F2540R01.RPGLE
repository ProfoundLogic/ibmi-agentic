     H*  (C) Copyright Progressive Data Solutions, Inc., 2004
    H**********************************************************************
    H* PROGRAM: F2540R01
    H* DESCRIPTION: HOSPITAL RATES
    H**********************************************************************
    H* PROGRAMMER NOTES:
    H**********************************************************************
    H*  CREATED: 09/07/04  C:79   PROJECT: NY009
    H**********************************************************************
    H*  REVISIONS:
    H**********************************************************************
     H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      /ENDIF
     Ff2540f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffahrpny   uf a e             disk
     F                                     rename(phr01:dbrcdf)
     Fststl     if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Hospital Rates'
     D comapi          c                   'FA007R'
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
     D kkkey           s                   like(stkey)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@st              2
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*
     C     stky          klist
     C                   kfld                    kkkey
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHGSTS - Subroutine for changing status
    C*================================================================
     C     chgsts        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
     C*
     C                   if        *in99 = *off
     C*
     C                   if        hrstat = 'I'
     C                   eval      hrstat = 'A'
     C*
     C                   else
     C                   eval      hrstat = 'I'
     C*
     C                   endif
     C*
     C                   eval      hruser = q1user
     C                   eval      hrpgm  = q1pgm
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti  = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hrdt
     C                   eval      hrtime = dstime
     C*
     C                   update    dbrcdf
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* CLRFLDS - Clear fields on add
    C*================================================================
     C     clrflds       begsr
     C                   eval      scopct = *blanks
     C                   eval      scefdt = 0
     C                   eval      scname = *blanks
     C                   eval      scc001 = 0
     C                   eval      scc002 = 0
     C                   eval      scc003 = 0
     C                   eval      scc004 = 0
     C                   eval      scc005 = 0
     C                   eval      scc006 = 0
     C                   eval      scc007 = 0
     C                   eval      scc008 = 0
     C                   eval      scc009 = 0
     C                   eval      scc010 = 0
     C                   eval      scc011 = 0
     C                   eval      scc012 = 0
     C                   eval      scc013 = 0
     C                   eval      scc014 = 0
     C                   eval      scc015 = 0
     C                   eval      scc016 = 0
     C                   eval      scc017 = 0
     C                   eval      scc018 = 0
     C                   eval      scc019 = 0
     C                   eval      scc020 = 0
     C                   eval      scc021 = 0
     C                   eval      scc022 = 0
     C                   eval      scc023 = 0
     C                   eval      scc024 = 0
     C                   eval      scc025 = 0
     C                   eval      scc026 = 0
     C                   eval      scc027 = 0
     C                   eval      scc028 = 0
     C                   eval      scc029 = 0
     C                   eval      scc030 = 0
     C                   eval      scc031 = 0
     C                   eval      scc032 = 0
     C                   eval      scc033 = 0
     C                   eval      scc034 = 0
     C                   eval      scc035 = 0
     C                   eval      scc036 = 0
     C                   eval      scc037 = 0
     C                   eval      scc038 = 0
     C                   eval      scc039 = 0
     C                   eval      scc040 = 0
     C                   eval      scc041 = 0
     C                   eval      scc042 = 0
     C                   eval      scc043 = 0
     C                   eval      scc044 = 0
     C                   eval      scc045 = 0
     C                   eval      scc046 = 0
     C                   eval      scc047 = 0
     C                   eval      scc048 = 0
     C                   eval      scc049 = 0
     C                   eval      scc050 = 0
     C                   eval      scc051 = 0
     C                   eval      scc052 = 0
     C                   eval      scc053 = 0
     C                   eval      scc054 = 0
     C                   eval      scc055 = 0
     C                   eval      scc056 = 0
     C                   eval      scc057 = 0
     C                   eval      scc058 = 0
     C                   eval      scc059 = 0
     C                   eval      scc060 = 0
     C                   eval      scc061 = 0
     C                   eval      scc062 = 0
     C                   eval      scc063 = 0
     C                   eval      scc064 = 0
     C                   eval      scc065 = 0
     C                   eval      scc066 = 0
     C                   eval      scc067 = 0
     C                   eval      scc068 = 0
     C                   eval      scc069 = 0
     C                   eval      scc070 = 0
     C                   eval      scc071 = 0
     C                   eval      scc072 = 0
     C                   eval      scc073 = 0
     C                   eval      scc074 = 0
     C                   eval      scc075 = 0
     C                   eval      scc076 = 0
     C                   eval      scc077 = 0
     C                   eval      scc078 = 0
     C                   eval      scc079 = 0
     C                   eval      scc080 = 0
     C                   eval      scc081 = 0
     C                   eval      scc082 = 0
     C                   eval      scc083 = 0
     C                   eval      scc084 = 0
     C                   eval      scc085 = 0
     C                   eval      scc086 = 0
     C                   eval      scc087 = 0
     C                   eval      scc088 = 0
     C                   eval      scc089 = 0
     C                   eval      scc090 = 0
     C                   eval      scc091 = 0
     C                   eval      scc092 = 0
     C                   eval      scc093 = 0
     C                   eval      scc094 = 0
     C                   eval      scc095 = 0
     C                   eval      scc096 = 0
     C                   eval      scc097 = 0
     C                   eval      scc098 = 0
     C                   eval      scc099 = 0
     C                   eval      scc100 = 0
     C                   endsr
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
     C*
     C*
     C                   if        *in90 = *off
     C*
     C                   endif
     C*
     C     ck1end        endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
     C*================================================================
     C* MKSTHD - Creates the state header
     C*================================================================
     C     mksthd        begsr
     C*
     C* Display state code and name in header
     C*
     C                   eval      kkkey = @@st
     C                   eval      scst  = @@st
     C     stky          chain     pst01                              99
     C                   eval      scstds = stname
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C* Add Mode
     C*
     C                   if        *in40 = *on
     C                   exsr      clrflds
     C                   endif
     C*
     C* Not add mode (change, display, etc)
     C*
     C                   if        *in40 = *off
     C*
     C                   eval      scopct = hropct
     C                   eval      scname = hrname
     C                   eval      scstat = hrstat
     C                   eval      srdti = hrefdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C*
     C                   eval      scc001 = hrc001
     C                   eval      scc002 = hrc002
     C                   eval      scc003 = hrc003
     C                   eval      scc004 = hrc004
     C                   eval      scc005 = hrc005
     C                   eval      scc006 = hrc006
     C                   eval      scc007 = hrc007
     C                   eval      scc008 = hrc008
     C                   eval      scc009 = hrc009
     C                   eval      scc010 = hrc010
     C                   eval      scc011 = hrc011
     C                   eval      scc012 = hrc012
     C                   eval      scc013 = hrc013
     C                   eval      scc014 = hrc014
     C                   eval      scc015 = hrc015
     C                   eval      scc016 = hrc016
     C                   eval      scc017 = hrc017
     C                   eval      scc018 = hrc018
     C                   eval      scc019 = hrc019
     C                   eval      scc020 = hrc020
     C                   eval      scc021 = hrc021
     C                   eval      scc022 = hrc022
     C                   eval      scc023 = hrc023
     C                   eval      scc024 = hrc024
     C                   eval      scc025 = hrc025
     C                   eval      scc026 = hrc026
     C                   eval      scc027 = hrc027
     C                   eval      scc028 = hrc028
     C                   eval      scc029 = hrc029
     C                   eval      scc030 = hrc030
     C                   eval      scc031 = hrc031
     C                   eval      scc032 = hrc032
     C                   eval      scc033 = hrc033
     C                   eval      scc034 = hrc034
     C                   eval      scc035 = hrc035
     C                   eval      scc036 = hrc036
     C                   eval      scc037 = hrc037
     C                   eval      scc038 = hrc038
     C                   eval      scc039 = hrc039
     C                   eval      scc040 = hrc040
     C                   eval      scc041 = hrc041
     C                   eval      scc042 = hrc042
     C                   eval      scc043 = hrc043
     C                   eval      scc044 = hrc044
     C                   eval      scc045 = hrc045
     C                   eval      scc046 = hrc046
     C                   eval      scc047 = hrc047
     C                   eval      scc048 = hrc048
     C                   eval      scc049 = hrc049
     C                   eval      scc050 = hrc050
     C                   eval      scc051 = hrc051
     C                   eval      scc052 = hrc052
     C                   eval      scc053 = hrc053
     C                   eval      scc054 = hrc054
     C                   eval      scc055 = hrc055
     C                   eval      scc056 = hrc056
     C                   eval      scc057 = hrc057
     C                   eval      scc058 = hrc058
     C                   eval      scc059 = hrc059
     C                   eval      scc060 = hrc060
     C                   eval      scc061 = hrc061
     C                   eval      scc062 = hrc062
     C                   eval      scc063 = hrc063
     C                   eval      scc064 = hrc064
     C                   eval      scc065 = hrc065
     C                   eval      scc066 = hrc066
     C                   eval      scc067 = hrc067
     C                   eval      scc068 = hrc068
     C                   eval      scc069 = hrc069
     C                   eval      scc070 = hrc070
     C                   eval      scc071 = hrc071
     C                   eval      scc072 = hrc072
     C                   eval      scc073 = hrc073
     C                   eval      scc074 = hrc074
     C                   eval      scc075 = hrc075
     C                   eval      scc076 = hrc076
     C                   eval      scc077 = hrc077
     C                   eval      scc078 = hrc078
     C                   eval      scc079 = hrc079
     C                   eval      scc080 = hrc080
     C                   eval      scc081 = hrc081
     C                   eval      scc082 = hrc082
     C                   eval      scc083 = hrc083
     C                   eval      scc084 = hrc084
     C                   eval      scc085 = hrc085
     C                   eval      scc086 = hrc086
     C                   eval      scc087 = hrc087
     C                   eval      scc088 = hrc088
     C                   eval      scc089 = hrc089
     C                   eval      scc090 = hrc090
     C                   eval      scc091 = hrc091
     C                   eval      scc092 = hrc092
     C                   eval      scc093 = hrc093
     C                   eval      scc094 = hrc094
     C                   eval      scc095 = hrc095
     C                   eval      scc096 = hrc096
     C                   eval      scc097 = hrc097
     C                   eval      scc098 = hrc098
     C                   eval      scc099 = hrc099
     C                   eval      scc100 = hrc100
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   eval      hropct = scopct
     C                   eval      hrname = scname
     C                   eval      srdti  = scefdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hrefdt
     C*
     C                   eval      hrc001 = scc001
     C                   eval      hrc002 = scc002
     C                   eval      hrc003 = scc003
     C                   eval      hrc004 = scc004
     C                   eval      hrc005 = scc005
     C                   eval      hrc006 = scc006
     C                   eval      hrc007 = scc007
     C                   eval      hrc008 = scc008
     C                   eval      hrc009 = scc009
     C                   eval      hrc010 = scc010
     C                   eval      hrc011 = scc011
     C                   eval      hrc012 = scc012
     C                   eval      hrc013 = scc013
     C                   eval      hrc014 = scc014
     C                   eval      hrc015 = scc015
     C                   eval      hrc016 = scc016
     C                   eval      hrc017 = scc017
     C                   eval      hrc018 = scc018
     C                   eval      hrc019 = scc019
     C                   eval      hrc020 = scc020
     C                   eval      hrc021 = scc021
     C                   eval      hrc022 = scc022
     C                   eval      hrc023 = scc023
     C                   eval      hrc024 = scc024
     C                   eval      hrc025 = scc025
     C                   eval      hrc026 = scc026
     C                   eval      hrc027 = scc027
     C                   eval      hrc028 = scc028
     C                   eval      hrc029 = scc029
     C                   eval      hrc030 = scc030
     C                   eval      hrc031 = scc031
     C                   eval      hrc032 = scc032
     C                   eval      hrc033 = scc033
     C                   eval      hrc034 = scc034
     C                   eval      hrc035 = scc035
     C                   eval      hrc036 = scc036
     C                   eval      hrc037 = scc037
     C                   eval      hrc038 = scc038
     C                   eval      hrc039 = scc039
     C                   eval      hrc040 = scc040
     C                   eval      hrc041 = scc041
     C                   eval      hrc042 = scc042
     C                   eval      hrc043 = scc043
     C                   eval      hrc044 = scc044
     C                   eval      hrc045 = scc045
     C                   eval      hrc046 = scc046
     C                   eval      hrc047 = scc047
     C                   eval      hrc048 = scc048
     C                   eval      hrc049 = scc049
     C                   eval      hrc050 = scc050
     C                   eval      hrc051 = scc051
     C                   eval      hrc052 = scc052
     C                   eval      hrc053 = scc053
     C                   eval      hrc054 = scc054
     C                   eval      hrc055 = scc055
     C                   eval      hrc056 = scc056
     C                   eval      hrc057 = scc057
     C                   eval      hrc058 = scc058
     C                   eval      hrc059 = scc059
     C                   eval      hrc060 = scc060
     C                   eval      hrc061 = scc061
     C                   eval      hrc062 = scc062
     C                   eval      hrc063 = scc063
     C                   eval      hrc064 = scc064
     C                   eval      hrc065 = scc065
     C                   eval      hrc066 = scc066
     C                   eval      hrc067 = scc067
     C                   eval      hrc068 = scc068
     C                   eval      hrc069 = scc069
     C                   eval      hrc070 = scc070
     C                   eval      hrc071 = scc071
     C                   eval      hrc072 = scc072
     C                   eval      hrc073 = scc073
     C                   eval      hrc074 = scc074
     C                   eval      hrc075 = scc075
     C                   eval      hrc076 = scc076
     C                   eval      hrc077 = scc077
     C                   eval      hrc078 = scc078
     C                   eval      hrc079 = scc079
     C                   eval      hrc080 = scc080
     C                   eval      hrc081 = scc081
     C                   eval      hrc082 = scc082
     C                   eval      hrc083 = scc083
     C                   eval      hrc084 = scc084
     C                   eval      hrc085 = scc085
     C                   eval      hrc086 = scc086
     C                   eval      hrc087 = scc087
     C                   eval      hrc088 = scc088
     C                   eval      hrc089 = scc089
     C                   eval      hrc090 = scc090
     C                   eval      hrc091 = scc091
     C                   eval      hrc092 = scc092
     C                   eval      hrc093 = scc093
     C                   eval      hrc094 = scc094
     C                   eval      hrc095 = scc095
     C                   eval      hrc096 = scc096
     C                   eval      hrc097 = scc097
     C                   eval      hrc098 = scc098
     C                   eval      hrc099 = scc099
     C                   eval      hrc100 = scc100
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
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   eval      *inlr = *on
     C                   endsr
    C*================================================================
    C* 0GMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C                   exsr      mksthd
     C*
     C                   if        @@mode = 'D'
     C                   eval      *in09 = *on
     C                   endif
     C*
     C                   if        @@mode <> 'D'
     C                   seton                                        41
     C                   endif
     C*
     C                   if        @@mode = 'A'
     C                   seton                                        40
     C                   seton                                        31
     C                   else
     C                   setoff                                       40
     C                   setoff                                       31
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C*
     C                   select
     C*
     C                   when      @@mode = 'S'
     C                   exsr      chgsts
     C                   return
     C*
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C*
     C                   eval      hrpgm  = q1pgm
     C                   eval      hruser = q1user
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hrdt
     C                   movel     dstime        hrtime
     C*
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C*
     C                   eval      hrstat = 'A'
     C                   eval      hripgm = q1pgm
     C                   eval      hriusr = q1user
     C                   eval      hrpgm  = q1pgm
     C                   eval      hruser = q1user
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hridt
     C                   movel     srdto         hrdt
     C                   movel     dstime        hritim
     C                   movel     dstime        hrtime
     C*
     C                   write     dbrcdf
     C                   feod      fahrpny
     C                   endsr
