     H* (C) Copyright Progressive Data Solutions, 2003
     H/TITLE FIXADDRESS                                                                     *
     H**************************************************************************************
     H*  Description:  Populate prov name/address in FAEAP                                 *
     H*                                                                                    *
     H*  Created: 05/02/03   Programmer C:79    Project:FL043                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FFAEOP     IF   E             DISK
     FFAEAL1    UF   E           K DISK

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

     C     *like         define    eoco#         kkco#
     C     *like         define    eosub#        kksub#
     C     *like         define    eofnd         kkfnd
     C     *like         define    eofyr         kkfyr
     C     *like         define    eocase        kkcase
     C     *like         define    eoeob#        kkeob#

     C*
     C*----------------------------------------------------------------------
     C*  Read through FAEOP file
     C*----------------------------------------------------------------------
     C                   read      peo01                                  99
     C*
     C                   dow       (*in99 = *off)
     C*
     C                   exsr      setkey
     C*
     C     eaky          chain     pea01                              99
     C*
     C                   dow       *in99 = *off
     C*
     C                   exsr      updea01
     C*
     C     eaky          reade     pea01                                  99
     C*
     C                   enddo                                                  *in60=*off
     C*
     C                   read      peo01                                  99
     C*
     C                   enddo                                                  *in60=*off
     C*
     C                   eval      *inlr=*on
     C*
     C                   Return
     C*
     C**********************************************************************
     C     setkey        begsr
     C**********************************************************************
     C                   eval      kkco# = eoco#
     C                   eval      kksub#= eosub#
     C                   eval      kkfnd = eofnd
     C                   eval      kkfyr = eofyr
     C                   eval      kkcase= eocase
     C                   eval      kkeob#= eoeob#
     C                   endsr
     C*
     C**********************************************************************
     C     updea01       begsr
     C**********************************************************************
     C*
     C                   eval      eapnam = eopnam
     C                   eval      eaadr1 = eoadr1
     C                   eval      eaadr2 = eoadr2
     C                   eval      eacty  = eocty
     C                   eval      east   = eost
     C                   eval      eazip  = eozip
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C     *cymd         move      sys_date      eadt
     C                   eval      eapgm=q1pgm
     C                   move      sys_time      eatime
     C*
     C                   update    pea01
     C*
     C                   endsr
