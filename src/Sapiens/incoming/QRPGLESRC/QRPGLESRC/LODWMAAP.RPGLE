      ** (C) Copyright Progressive Data Solutions, Inc., 1998
      /TITLE LODWMAAP - Create WMAAP records for WMAGP records

     hdatedit(*ymd) datfmt(*ymd)
      *****************************************************************
      **   PROGRAM DESCRIPTION:                                      **
      **                                                             **
      **           Creates a record in WMAAP if a record in WMAGP    **
      **           does not have a corresponding WMAAP record.       **
      **                                                             **
      **   This program requires one parameter, a test fund for      **
      **   creating WMAAP records.  This parameter can be a 1 to 5   **
      **   digit numeric (packed decimal) field, or a numeric        **
      **   character string.                                         **
      **                                                             **
      *****************************************************************
     fwmagp     if   e             disk
     fwmaal     if a e           k disk
     fwmaal1    if   e           k disk    rename(paa01:paa02)
     fqsysprt   o    f  132        printer oflind(*in71)

     dWorkStor         ds                  inz                                   Work/Storage Area
     d  xxWORK                       10a                                           Fund work
     d    xxPAK1                      1p 0 overlay(xxWORK:1)                         Packed 1,0
     d    xxPAK3                      3p 0 overlay(xxWORK:1)                         Packed 3,0
     d    xxPAK5                      5p 0 overlay(xxWORK:1)                         Packed 5,0
     d    xxDEC3                      3s 0 overlay(xxWORK:1)                         Zoned 3,0
     d  xxBYTE                        1a                                           One byte
     d    xxNUMB                      1s 0 overlay(xxBYTE)                           Numeric byte
     d  xxAGT#                        5p 0                                         New agent number
     d  xxFND                         3p 0                                         Test fund
     d  xxMESG                      120a                                           Message
     d  xxDATE                        8s 0                                         System date
     d    xxYEAR                      4s 0 overlay(xxDATE:1)                         Year
     d    xxMNTH                      2s 0 overlay(xxDATE:5)                         Month
     d    xxDAY                       2s 0 overlay(xxDATE:7)                         Day
     d  xxTIME                        6s 0                                         Time
     d    xxHOUR                      2s 0 overlay(xxTIME:1)                         Hour
     d    xxMIN                       2s 0 overlay(xxTIME:3)                         Minute
     d    xxSEC                       2s 0 overlay(xxTIME:5)                         Second
     d  xxPAGE                        4s 0                                         Current page
     d  xxVALUE                       9a                                           String value
     d    xxNUMVAL                    9s 0 overlay(xxVALUE)                          Numeric value
     d  xxCTWMAA                      9p 0                                         Counter
     d  xxCTWMAG                      9p 0                                         Counter

     dSysArea         sds                                                        System Area
     d  syPARM           *parms                                                    Number of parms

     c     *entry        plist
     c                   parm                    pmFND            10

     c     wmaaKey       klist
     c                   kfld                    agCO#
     c                   kfld                    agFEIN
     c                   kfld                    agMOD

     c     wmaaKey1      klist
     c                   kfld                    agCO#
     c                   kfld                    xxFND

     c                   read      wmagp                                  LR
     c                   if        *inLR=*off                                    >---------+
     c                   eval      xxCTWMAG=xxCTWMAG+1                                     :
     c     wmaaKey       chain     wmaal                              91                   :
                                                                                           :
      **   No WMAAP record found for this WMAGP record                                     :
     c                   if        *in91=*on                                     >-------+ :
     c     wmaaKey1      setgt     wmaal1                                                : :
     c                   eval      xxAGT#=99999                                          : :
                                                                                         : :
      **   Find next available agent, ignoring end of range (ie. 9xxxx)                  : :
     c                   dou       *in90=*on                                     >-----+ : :
     c     wmaaKey1      readpe    wmaal1                                 91           : : :
     c                   eval      *in90=*in91                                         : : :
     c                   if        *in90=*off                                    >---+ : : :
     c                   if        aaAGT#<xxAGT#                                 >-+ : : : :
     c                   eval      xxAGT#=aaAGT#+1                                 : : : : :
     c                   eval      *in90=*on                                       : : : : :
     c                   else                                                    >-+ : : : :
     c                   eval      xxAGT#=aaAGT#-1                                 : : : : :
     c                   endif                                                   <-+ : : : :
     c                   endif                                                   <---+ : : :
     c                   enddo                                                   <-----+ : :
                                                                                         : :
      **   No agents for this fund yet, so start with 1                                  : :
     c                   if        *in91=*on and xxAGT#=99999                    >-+     : :
     c                   eval      *in91=*off                                      :     : :
     c                   eval      xxAGT#=1                                        :     : :
     c                   endif                                                   <-+     : :
                                                                                         : :
      **   No available agent number for this fund                                       : :
     c                   if        *in91=*on                                     >-----+ : :
     c                   eval      xxMESG='No more available agent numbers +           : : :
     c                             for test fund.'                                     : : :
     c                   exsr      $PrintErr                                           : : :
     c                   eval      xxMESG='Program execution terminated.'              : : :
     c                   exsr      $PrintErr                                           : : :
     c                   eval      *inLR=*on                                           : : :
     c                   else                                                    >-----+ : :
     c                   eval      xxCTWMAA=xxCTWMAA+1                                 : : :
     c                   eval      xxNUMVAL=xxAGT#                                     : : :
     c                   if        xxNUMVAL=*zero                                >-+   : : :
     c                   eval      IX=9                                            :   : : :
     c                   else                                                    >-+   : : :
     c     '0'           check     xxVALUE       IX                                :   : : :
     c                   endif                                                   <-+   : : :
     c                   eval      xxMESG='Added agent '+                              : : :
     c                             %subst(xxVALUE:IX:10-IX)+' to test fund.'           : : :
     c                   exsr      $PrintMsg                                           : : :
     c                   clear                   paa01                                 : : :
     c                   eval      aaCO#=agCO#                                         : : :
     c                   eval      aaFEIN=agFEIN                                       : : :
     c                   eval      aaMOD=agMOD                                         : : :
     c                   eval      aaFND=xxFND                                         : : :
     c                   eval      aaAGT#=xxAGT#                                       : : :
     c                   eval      aaSTAT='A'                                          : : :
     c                   eval      aaUSER='PDSPGMR'                                    : : :
     c                   eval      aaPGM='LODWMAAP'                                    : : :
     c                   time                    aaTIME                                : : :
     c                   eval      aaDT=xxDATE-19000000                                : : :
     c                   eval      aaCMFL='N'                                          : : :
     c                   eval      aaPRFL='N'                                          : : :
     c                   eval      aaMLFL='N'                                          : : :
     c                   eval      aaIUSR=aaUSER                                       : : :
     c                   eval      aaIPGM=aaPGM                                        : : :
     c                   eval      aaITM=aaTIME                                        : : :
     c                   eval      aaIDT=aaDT                                          : : :
     c                   eval      aaNAME=agNAME                                       : : :
     c                   write     paa01                                               : : :
     c                   endif                                                   <-----+ : :
     c                   endif                                                   <-------+ :
     c                   endif                                                   <---------+

     clr                 eval      xxMESG=*blanks
     clr                 exsr      $PrintMsg
     clr                 eval      xxNUMVAL=xxCTWMAG
     clr                 if        xxNUMVAL=*zero                                >-+
     clr                 eval      IX=9                                            :
     clr                 else                                                    >-+
     clr   '0'           check     xxVALUE       IX                                :
     clr                 endif                                                   <-+
     clr                 eval      xxMESG='Read '+%subst(xxVALUE:IX:10-IX)+
     c                             ' record'
     clr                 if        xxCTWMAG<>1                                   >-+
     clr                 eval      xxMESG=%trim(xxMESG)+'s'                        :
     clr                 endif                                                   <-+
     clr                 eval      xxNUMVAL=xxCTWMAA
     clr                 if        xxNUMVAL=*zero                                >-+
     clr                 eval      IX=9                                            :
     clr                 else                                                    >-+
     clr   '0'           check     xxVALUE       IX                                :
     clr                 endif                                                   <-+
     clr                 eval      xxMESG=%trim(xxMESG)+' from WMAGP and +
     c                             added '+%subst(xxVALUE:IX:10-IX)+' to WMAAP.'
     clr                 exsr      $PrintMsg

     csr   *inzsr        begsr                                                   >---------+
     csr                 eval      *in71=*on                                               :
     csr                 eval      xxDATE=*date                                            :
     csr                 time                    xxTIME                                    :
     csr                 if        syPARM=*zero                                  >-------+ :
     csr                 eval      xxMESG='No parameter passed on CALL.'                 : :
     csr                 exsr      $PrintErr                                             : :
     csr                 eval      *inLR=*on                                             : :
     csr                 else                                                    >-------+ :
     csr                 eval      xxWORK=pmFND                                          : :
     csr   ' '           checkr    xxWORK        IX                5 0                   : :
     csr                 if        IX>*zero and IX<4                             >-+     : :
     csr                 eval      xxBYTE=%subst(xxWORK:IX:1)                      :     : :
     csr                 else                                                    >-+     : :
     csr                 eval      xxBYTE=*blanks                                  :     : :
     csr                 endif                                                   <-+     : :
     csr                 bitoff    '0123'        xxBYTE                                  : :
     csr                 if        xxBYTE=x'0C' or xxBYTE=x'0F'                  >-----+ : :
     csr                 select                                                        : : :
     csr                 when      IX=1                                          >---+ : : :
     csr                 eval      xxFND=xxPAK1                                      : : : :
     csr                 when      IX=2                                          >---+ : : :
     csr                 eval      xxFND=xxPAK3                                      : : : :
     csr                 other                                                   >---+ : : :
     csr                 if        xxPAK5>999                                    >-+ : : : :
     csr                 eval      xxFND=xxPAK5-1000                               : : : : :
     csr                 else                                                    >-+ : : : :
     csr                 eval      xxFND=xxPAK5                                    : : : : :
     csr                 endif                                                   <-+ : : : :
     csr                 endsl                                                   <---+ : : :
     csr                 else                                                    >-----+ : :
     csr                 eval      xxFND=*zero                                         : : :
     csr                 do        10            IX                5 0           >---+ : : :
     csr                 eval      xxBYTE=%subst(xxWORK:IX:1)                        : : : :
     csr                 if        xxBYTE>='0' and xxBYTE<='9' and xxFND<100     >-+ : : : :
     csr                 eval      xxFND=(xxFND*10)+xxNUMB                         : : : : :
     csr                 endif                                                   <-+ : : : :
     csr                 enddo                                                   <---+ : : :
     csr                 endif                                                   <-----+ : :
     csr                 eval      xxDEC3=xxFND                                          : :
     csr                 if        xxDEC3=*zero                                  >-+     : :
     csr                 eval      IX=3                                            :     : :
     csr                 else                                                    >-+     : :
     csr   '0'           check     xxWORK        IX                                :     : :
     csr                 endif                                                   <-+     : :
     csr                 eval      xxMESG='Using test fund '+                            : :
     c                             %subst(xxWORK:IX:4-IX)+'.'                            : :
     csr                 exsr      $PrintMsg                                             : :
     csr                 endif                                                   <-------+ :
     csr                 endsr                                                   <---------+

     csr   $PrintErr     begsr                                                   >---+
     csr                 if        *in71=*on                                     >-+ :
     csr                 eval      xxPAGE=xxPAGE+1                                 : :
     csr                 except    ##Header                                        : :
     csr                 eval      *in71=*off                                      : :
     csr                 endif                                                   <-+ :
     csr                 except    ##Error                                           :
     csr                 eval      xxMESG=*blanks                                    :
     csr                 endsr                                                   <---+

     csr   $PrintMsg     begsr                                                   >---+
     csr                 if        *in71=*on                                     >-+ :
     csr                 eval      xxPAGE=xxPAGE+1                                 : :
     csr                 except    ##Header                                        : :
     csr                 eval      *in71=*off                                      : :
     csr                 endif                                                   <-+ :
     csr                 except    ##Info                                            :
     csr                 eval      xxMESG=*blanks                                    :
     csr                 endsr                                                   <---+

     oqsysprt   e            ##Header       1  2
     o                       xxMNTH               2
     o                                            3 '/'
     o                       xxDAY                5
     o                                            6 '/'
     o                       xxYEAR              10
     o                       xxHOUR              14
     o                                           15 ':'
     o                       xxMIN               17
     o                                           18 ':'
     o                       xxSEC               20
     o                                          128 'Page'
     o                       xxPAGE        4    132

     o          e            ##Header       2
     o                                           68 'Load Default WMAAP R'
     o                                           85 'ecords from WMAGP'

     o          e            ##Error        1
     o                                           10 '*****'
     o                       xxMESG             132

     o          e            ##Info         1
     o                                           10 '>    '
     o                       xxMESG             132
