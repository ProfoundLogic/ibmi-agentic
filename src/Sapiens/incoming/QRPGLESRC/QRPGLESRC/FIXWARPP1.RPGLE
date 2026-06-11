     H*  (C) Copyright Progressive Data Solutions, Inc., 1997
     H/TITLE FIXWARPP: V3 Change RPSTAT to *blank if RPSTS = 'PY'
     H**************************************************************************
     H* Description:  This pgm loads the correct provider for group type pmts. *
     H*                                                                        *
     H*                                                                        *
     H*                                                                        *
     H**************************************************************************
     fwarpl     uf   e           k disk
     fwmnrl1    if   e           k disk
     fwmpdl     if   e           k disk
     fqsysprt   o    f   80        printer oflind(*INOA)

      ** Initialization work area
      *************************************************
     dWork_Inz         ds                  inz                                   Initialization
     d  xxco#                         3p 0
     d  xxgrtp                        1a

      *************************************************
      ** Mainline processing                         **
      *************************************************
     c     nrky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxgrtp

     c     pdky          klist
     c                   kfld                    nrfein
     c                   kfld                    nrmod

      ** Retrieve the group provider info

     c                   eval      xxco#=60
     c                   eval      xxgrtp='B'
     c     nrky          chain     pnr01                              60
     c                   if        *in60=*off
     c     pdky          chain     ppd01                              60

     c                   except    head

      ** Read next Request Payment Transaction record

     c                   read      prp01                                  LR
     c                   DOW       *inLR=*off

     c                   IF        rppgtp='G'

     c   OA              except    head
     c                   except    det

     c                   eval      rppnam=pdname
     c                   eval      rpadr1=pdadr1
     c                   eval      rpadr2=pdadr2
     c                   eval      rpcty=pdcty
     c                   eval      rpst=pdst
     c                   eval      rpzip=pdzip
     c                   IF        rppgm='W7024R'
     c                   eval      rptrdt=0980318
     c                   ENDIF
     c                   update    prp01

     c                   ENDIF

     c                   read      prp01                                  LR
     c                   ENDDO

     c                   ENDIF
      **********************************************************************************************
     oqsysprt   e            head           2 06
     o                                            3 'FND'
     o                                            8 'FYR'
     o                                           17 'CASE'
     o                                           21 'TP'
     o                                           32 'Fed Id #'
     o                                           37 'Mod'
     o                                           48 'Zip code'
     o                                           59 'Zip Code'
     o          e            det            1
     o                       rpfnd                3
     o                       rpfyr                8
     o                       rpcase              17
     o                       rppgtp              20
     o                       rpfein              32
     o                       rpmod               37
     o                       rpzip               48
     o                       pdzip               59
     o                       pdfein              70
     o                       pdmod               75

