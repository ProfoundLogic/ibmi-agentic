     H* (C) Copyright Progressive Data Solutions, 2000
     H/TITLE FIXFLRESUB                                                                     *
     H**************************************************************************************
     H*  Description:  Populate EAESDT with date bill was completed at the                 *
     H*                recon level.                                                        *
     H*                                                                                    *
     H*  Created: 05/30/00   Programmer C:79    Project:FL030                              *
     H**************************************************************************************
     H*  Programmer notes:   Two reports are generated: QSYSPRT which shows                *
     H*                      EOB's where EOWCCK already was populated and                  *
     H*                      QSYSPRT1 where EOWCCK was zero and the pgm pop                *
     H*                      from ESDT where ESRCN=0.                                      *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FFAEOP     UF   E             DISK
     FFAEAL1    UF   E           K DISK
     FFAESL3    IF   E           K DISK
     FQSYSPRT   O    F  198        printer oflind(*in60)
     FQSYSPRT1  O    F  198        printer oflind(*in61)

     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0

     DPSDS           ESDS                  Extname(SZQ1P)


     C     eaky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#

     C     esky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
     C                   kfld                    kkblst

     C     *like         define    eoco#         kkco#
     C     *like         define    eosub#        kksub#
     C     *like         define    eofnd         kkfnd
     C     *like         define    eofyr         kkfyr
     C     *like         define    eocase        kkcase
     C     *like         define    eoeob#        kkeob#
     C     *like         define    eorcn         kkrcn
     C     *like         define    eoblst        kkblst

     C                   except    header
     C                   except    header1
     C*
     C*----------------------------------------------------------------------
     C*  Read through FAEOP file
     C*----------------------------------------------------------------------
     C                   read      peo01                                  99
     C*
     C                   dow       (*in99 = *off)
     C* Select all Florida eligible for data submission
     C*
     C                   if        eotape='Y' and eoast='FL'
     C                             and eoblst <> ' '
     C*
     C                   eval      kkco#=eoco#
     C                   eval      kksub#=eoco#
     C                   eval      kkfnd=eofnd
     C                   eval      kkfyr=eofyr
     C                   eval      kkcase=eocase
     C                   eval      kkeob#=eoeob#
     C*
     C                   if        eorcn=0
     C*
     C     eaky          chain     pea01                              98
     C*
     C                   if        (*in98 = *off)
     C*
     C* If completed then it will have date. If pended date is zero.
     C                   eval      eaesdt=eowcck
     C                   exsr      updea01
     C*
     C                   if        *in60=*on
     C                   except    header
     C                   move      *off          *in60
     C                   endif
     C                   except    detail
     C*
     C                   endif
     C* Recon ne 0
     C                   else
     C*
     C* Read through FAEAP for all recons and chain to FAESP for corr date
     C     eaky          chain     pea01                              97
     C*
     C                   dow       (*in97 = *off)
     C*
     C                   eval      kkrcn=earcn
     C                   eval      kkblst='C'
     C*
     C     esky          chain     pes01                              96
     C*
     C                   if        *in96=*off
     C*
     C                   eval      eaesdt=esdt
     C*
     C                   exsr      updea01
     C*
     C                   if        *in60=*on
     C                   except    header
     C                   move      *off          *in60
     C                   endif
     C                   except    detail
     C*
     C                   endif                                                  *in96=off
     C*
     C* Populate EOWCCK if its zero, use recon 0 ESDT.
     C                   if        eowcck=0
     C*
     C                   eval      kkrcn=0
     C*
     C     esky          chain     pes01                              96
     C*
     C                   if        *in96=*off
     C                   eval      eowcck=esdt
     C                   exsr      updeo01
     C*
     C                   if        *in61=*on
     C                   except    header1
     C                   move      *off          *in61
     C                   endif
     C                   except    detail1
     C*
     C                   endif
     C*
     C                   endif
     C*
     C     eaky          reade     pea01                                  97
     C*
     C                   enddo                                                  *in97=off
     C*
     C                   endif                                                  eorcn=0
     C*
     C                   endif                                                  eotape='Y'
     C*
     C                   read      peo01                                  99
     C*
     C                   enddo                                                  *in99=*off
     C*
     C                   eval      *inlr=*on
     C*
     C                   Return
     C**********************************************************************
     C*
     C     updea01       begsr
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C     *cymd         move      sys_date      eadt
     C                   eval      eapgm=q1pgm
     C*****************  eval      eauser=q1user
     C                   move      sys_time      eatime
     C*
     C                   update    pea01
     C*
     C                   endsr
     C**********************************************************************
     C*
     C     updeo01       begsr
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C     *cymd         move      sys_date      eodt
     C                   eval      eopgm=q1pgm
     C*****************  eval      eouser=q1user
     C                   move      sys_time      eotime
     C*
     C                   update    peo01
     C*
     C                   endsr
     C**********************************************************************
     Oqsysprt   e            header      5  1  2
     O                                           60 'EAESDT POP FROM ESDT'
     O          e            header      2  1
     O                                           12 'EOB#'
     O                                           30 'Recon'
     O                                           45 'Date'
     O                                           60 'Bill Status'
     O          e            detail
     O                       eaeob#        j     12
     O                       earcn         j     30
     O                       eaesdt        j     45
     O                       eoblst              60
     Oqsysprt1  e            header1     5  1
     O                                           60 'EOWCCK POPULATE FROM ESDT'
     O          e            header1     2  1
     O                                           12 'EOB#'
     O                                           30 'Recon'
     O                                           45 'Date'
     O                                           60 'Bill Status'
     O          e            detail1
     O                       eoeob#        j     12
     O                       eorcn         j     30
     O                       eowcck        j     45
     O                       eoblst              60
