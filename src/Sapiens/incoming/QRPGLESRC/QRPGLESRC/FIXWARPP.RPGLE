     H*  (C) Copyright Progressive Data Solutions, Inc., 1997
     H/TITLE FIXWARPP: V3 Change RPSTAT to *blank if RPSTS = 'PY'
     fwarpp     uf   e             disk
     fqsysprt   o    f   80        printer oflind(*INOA)


      *************************************************
      ** Mainline processing                         **
      *************************************************
     c                   except    head

      ** Read next Request Payment Transaction record

     c                   read      warpp                                  LR

      ** If not EOF, update new fields

     c                   dow       *inLR=*off

     c                   if        RPSTAT='P' and RPSTS='PY'
     c                   eval      rpstat=*blanks
     c                   update    prp01
     c   OA              except    head
     c                   except    det
     c                   endif

     c                   read      warpp                                  LR
     c                   enddo
      **********************************************************************************************
     oqsysprt   e            head           2 06
     o                                            4 'FND'
     o                                            9 'FYR'
     o                                           18 'CASE'
     o                                           24 'STAT'
     o                                           29 'STS'
     o          e            det            1
     o                       rpfnd                4
     o                       rpfyr                9
     o                       rpcase              18
     o                       rpstat              22
     o                       rpsts               29

