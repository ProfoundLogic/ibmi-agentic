     H*  (C) Copyright Progressive Data Solutions, Inc., 2000
     H/TITLE CVTBLTY - converts generic table values for bill type
     fsmcol     if   e             disk
     fftgil     uf a e           k disk
     fftgtl     uf a e           k disk
     d psds          esds                  extname(szq1p)
    d*
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)
     d  date           s               d   datfmt(*usa) inz
     dkktbid           s                   like(gttbid) inz('BLTY')
      *************************************************
      ** Program entry processing                    **
      *************************************************

     c     giky          klist
     c                   kfld                    coco#
     c                   kfld                    kktbid

     c     gtky          klist
     c                   kfld                    coco#
     c                   kfld                    kktbid
      *************************************************
      ** Mainline processing                         **
      *************************************************

      ** Read next old record

     c                   read      pco01                                  lr
     c                   dow       *inlr = *off                                  >-------+
     c     giky          chain     pgi01                              99
     c                   if        *in99 = *off
     c     gtky          chain     pgt01                              99
     c                   dow       *in99 = *off
     c                   exsr      workit                                                :
     c     gtky          reade     pgt01                                  99
     c                   enddo
     c                   exsr      modgip                                                :
     c                   endif
     c                   read      pco01                                  lr
     c                   enddo                                                   <-----+ :

      ****************************************************
      ** modify/write FTGIP - BLTY ==> ZZBT and add FLBT *
      ****************************************************
     csr   modgip        begsr                                                   >---+
                                                                                 >-+ :
     c                   eval      gitbid = 'ZZBT'
     c                   eval      gipgm = q1pgm
     c                   eval      giuser = q1user
     c                   time                    dsdttm
     c                   move      dsdate        date
     c     *cymd         move      date          gidt
     c                   eval      gitime = dstime
     c                   update    pgi01
                                                                                 >-+ :
     c                   eval      gitbid = 'FLBT'
     c                   eval      gidesc = 'FLORIDA ' + gidesc
     c                   write     pgi01
                                                                                 <-+ :
     csr                 endsr                                                   <---+
      ****************************************************************
      ** modify FTGTP to 'BLTY' to 'FLBT' and write FTGTP for 'ZZBT' *
      ****************************************************************
     csr   workit        begsr                                                   >---+
                                                                                 >-+ :
     c                   eval      gttbid = 'FLBT'
     c                   eval      gtpgm = q1pgm
     c                   eval      gtuser = q1user
     c                   time                    dsdttm
     c                   move      dsdate        date
     c     *cymd         move      date          gtdt
     c                   eval      gttime = dstime
     c                   update    pgt01
                                                                                 >-+ :
     c                   if        gtkey = '111' or gtkey = '112' or gtkey
     c                             = '113' or gtkey = '114' or gtkey = '115'
     c                             or gtkey = '131' or gtkey = '132' or
     c                             gtkey = '133' or gtkey = '134' or gtkey
     c                             = '135'

     c                   eval      gttbid = 'ZZBT'
     c                   write     pgt01

     c                   endif
                                                                                 <-+ :
     csr                 endsr                                                   <---+
