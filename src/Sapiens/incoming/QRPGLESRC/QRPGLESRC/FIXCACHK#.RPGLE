     H* (C) Copyright Progressive Data Solutions, 2000
     H/TITLE FIXCACHK#                                                                      *
     H**************************************************************************************
     H*  Description:  Clear CACHK# for pended charges current recons                      *
     H*                                                                                    *
     H*  Created: 03/06/00   Programmer C:79    Project:MCMNT                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FFAEOLG    IF   E           K DISK
     FFDCALG    UF   E             DISK
     FQSYSPRT   O    F  198        printer

     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0

     DPSDS           ESDS                  Extname(SZQ1P)


     C     eoky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#

     C     *like         define    eoco#         kkco#
     C     *like         define    eosub#        kksub#
     C     *like         define    eofnd         kkfnd
     C     *like         define    eofyr         kkfyr
     C     *like         define    eocase        kkcase
     C     *like         define    eoeob#        kkeob#

     C                   except    header
     C*
     C*----------------------------------------------------------------------
     C*  Read through FAEOP file
     C*----------------------------------------------------------------------
     C                   read      pca01                                  99
     C                   dow       (*in99 = *off)
     C*
     C                   eval      kkco#=caco#
     C                   eval      kksub#=caco#
     C                   eval      kkfnd=cafnd
     C                   eval      kkfyr=cafyr
     C                   eval      kkcase=cacase
     C                   eval      kkeob#=caeob#
     C*
     C     eoky          chain     peo01                              98
     C                   if        (*in98 = *off)
     C*
     C                   if        caflag='P' and eorcn=carcn and cachk# <> 0
     C*
     C                   eval      cachk#=0
     C                   exsr      updca01
     C*
     C                   if        *inof=*on
     C                   except    header
     C                   move      *off          *inof
     C                   endif
     C                   except    detail
     C*
     C                   endif
     C*
     C                   endif                                                  *in98=*off
     C*
     C                   read      pca01                                  99
     C*
     C                   enddo                                                  *in60=*off
     C*
     C                   eval      *inlr=*on
     C*
     C                   Return
     C*
     C     updca01       begsr
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C     *cymd         move      sys_date      cadt
     C                   eval      capgm=q1pgm
     C                   eval      causer=q1user
     C                   move      sys_time      catime
     C*
     C                   update    pca01
     C*
     C                   endsr
     Oqsysprt   e            header      5  1
     O                                           12 'EOB#'
     O                                           20 'Recon'
     O                                           35 'Sequence'
     O          e            detail
     O                       caeob#        j     12
     O                       carcn         j     20
     O                       cadseq        j     35
