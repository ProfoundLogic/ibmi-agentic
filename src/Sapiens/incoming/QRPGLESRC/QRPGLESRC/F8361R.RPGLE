    H*****************************************************************
    H* (C) Copyright Progressive Data Solutions, Inc. 2003
    H*****************************************************************
    H* Program: F8361R - TWCC ADL Upload - Conversion Program
    H*****************************************************************
     H* Created on 09/18/2003 by c:62 for Project TX021
    H*****************************************************************
    H* Revisions:
    H*
    H*****************************************************************
     Ffaa0p     if   e             disk
     Ffzbcl1    if a e           k disk
     Ffzbdl1    if a e           k disk
     Ffzccl1    if a e           k disk
     Ffzddl1    if a e           k disk
     Ffzdel1    if a e           k disk
     Ffzeel1    if a e           k disk
     Ffzefl1    if a e           k disk
     Ffzffl1    if a e           k disk
     Ffzfgl1    if a e           k disk

     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

     D                 ds
     D timestamp                     14
     D  date                          8    overlay(timestamp)
     D  time                          6    overlay(timestamp:9)

     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)
     D bbc           e ds                  extname(fzbcl1)
     D bbcw          e ds                  extname(fzbcl1) prefix(a1:2)
     D bbd           e ds                  extname(fzbdl1)
     D bbdw          e ds                  extname(fzbdl1) prefix(a9:2)
     D ccc           e ds                  extname(fzccl1)
     D cccw          e ds                  extname(fzccl1) prefix(a2:2)
     D ddd           e ds                  extname(fzddl1)
     D dddw          e ds                  extname(fzddl1) prefix(a3:2)
     D dde           e ds                  extname(fzdel1)
     D ddew          e ds                  extname(fzdel1) prefix(a4:2)
     D eee           e ds                  extname(fzeel1)
     D eeew          e ds                  extname(fzeel1) prefix(a5:2)
     D eef           e ds                  extname(fzefl1)
     D eefw          e ds                  extname(fzefl1) prefix(a6:2)
     D fff           e ds                  extname(fzffl1)
     D fffw          e ds                  extname(fzffl1) prefix(a7:2)
     D ffg           e ds                  extname(fzfgl1)
     D ffgw          e ds                  extname(fzfgl1) prefix(a8:2)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D xxtype          s              3
     D xxdate          s              8  0
     D dates           s               d   datfmt(*usa)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     C*
     C     bcky          klist
     C                   kfld                    a1id#
     C                   kfld                    a1cid#
     C                   kfld                    a1ctyp
     C                   kfld                    a1csts
     C                   kfld                    a1cefd
     C                   kfld                    a1cddt
     C                   kfld                    a1udat
     C*
     C     bdky          klist
     C                   kfld                    a9did#
     C                   kfld                    a9seq#
     C                   kfld                    a9id#
     C                   kfld                    a9udat
     C*
     C     ccky          klist
     C                   kfld                    a2id#
     C                   kfld                    a2lic#
     C                   kfld                    a2lctp
     C                   kfld                    a2jurs
     C                   kfld                    a2udat
     C*
     C     ddky          klist
     C                   kfld                    a3id#
     C                   kfld                    a3seq#
     C                   kfld                    a3code
     C                   kfld                    a3stdt
     C                   kfld                    a3endt
     C                   kfld                    a3udat
     C*
     C     deky          klist
     C                   kfld                    a4id#
     C                   kfld                    a4seq#
     C                   kfld                    a4ssq#
     C                   kfld                    a4rcc
     C                   kfld                    a4rscc
     C                   kfld                    a4udat
     C*
     C     eeky          klist
     C                   kfld                    a5id#
     C                   kfld                    a5seq#
     C                   kfld                    a5code
     C                   kfld                    a5stdt
     C                   kfld                    a5endt
     C                   kfld                    a5udat
     C*
     C     efky          klist
     C                   kfld                    a6id#
     C                   kfld                    a6seq#
     C                   kfld                    a6ssq#
     C                   kfld                    a6rcc
     C                   kfld                    a6rscc
     C                   kfld                    a6udat
     C*
     C     ffky          klist
     C                   kfld                    a7id#
     C                   kfld                    a7seq#
     C                   kfld                    a7code
     C                   kfld                    a7stdt
     C                   kfld                    a7endt
     C                   kfld                    a7udat
     C*
     C     fgky          klist
     C                   kfld                    a8id#
     C                   kfld                    a8seq#
     C                   kfld                    a8ssq#
     C                   kfld                    a8rcc
     C                   kfld                    a8rscc
     C                   kfld                    a8udat
    C*
     C                   read      pa001
     C                   dow       not %eof(faa0p)
     C*
     C                   movel(p)  a0data        xxtype
    C*
     C                   select
     C                   when      xxtype = 'BBC'
     C                   movel(p)  a0data        bbcw
     C     bcky          chain     pbc01
     C                   if        not %found
     C                   movel(p)  a0data        bbc
     C*
     C                   move      bccefd        xxdate
     C                   if        xxdate = *zeros
     C                   eval      bccef7 = *zeros
     C                   else
     C                   move      xxdate        dates
     C     *cymd         move      dates         bccef7
     C                   endif
     C*
     C                   move      bccddt        xxdate
     C                   if        xxdate = *zeros
     C                   eval      bccdd7 = *zeros
     C                   else
     C                   move      xxdate        dates
     C     *cymd         move      dates         bccdd7
     C                   endif
     C*
     C                   move      bcudat        timestamp
     C                   move      date          xxdate
     C                   if        xxdate = *zeros
     C                   eval      bcuda7 = *zeros
     C                   else
     C                   move      xxdate        dates
     C     *cymd         move      dates         bcuda7
     C                   endif
     C                   move      time          bcutim
     C                   write     pbc01
     C                   endif
     C*
     C                   when      xxtype = 'BBD'
     C                   movel(p)  a0data        bbdw
     C     bdky          chain     pbd01
     c                   if        not %found
     C                   movel(p)  a0data        bbd
     C                   write     pbd01
     C                   endif
     C*
     C                   when      xxtype = 'CCC'
     C                   movel(p)  a0data        cccw
     C     ccky          chain     pcc01
     c                   if        not %found
     C                   movel(p)  a0data        ccc
     C                   write     pcc01
     C                   endif
     C*
     C                   when      xxtype = 'DDD'
     C                   movel(p)  a0data        dddw
     C     ddky          chain     pdd01
     c                   if        not %found
     C                   movel(p)  a0data        ddd
     C                   write     pdd01
     C                   endif
     C*
     C                   when      xxtype = 'DDE'
     C                   movel(p)  a0data        ddew
     C     deky          chain     pde01
     c                   if        not %found
     C                   movel(p)  a0data        dde
     C                   write     pde01
     C                   endif
     C*
     C                   when      xxtype = 'EEE'
     C                   movel(p)  a0data        eeew
     C     eeky          chain     pee01
     c                   if        not %found
     C                   movel(p)  a0data        eee
     C                   write     pee01
     C                   endif
     C*
     C                   when      xxtype = 'EEF'
     C                   movel(p)  a0data        eefw
     C     efky          chain     pef01
     c                   if        not %found
     C                   movel(p)  a0data        eef
     C                   write     pef01
     C                   endif
     C*
     C                   when      xxtype = 'FFF'
     C                   movel(p)  a0data        fffw
     C     ffky          chain     pff01
     c                   if        not %found
     C                   movel(p)  a0data        fff
     C                   write     pff01
     C                   endif
     C*
     C                   when      xxtype = 'FFG'
     C                   movel(p)  a0data        ffgw
     C     fgky          chain     pfg01
     c                   if        not %found
     C                   movel(p)  a0data        ffg
     C                   write     pfg01
     C                   endif
     C*
     C                   endsl
    C*
     C                   read      pa001
     C                   enddo
    C*
     C                   seton                                        lr
