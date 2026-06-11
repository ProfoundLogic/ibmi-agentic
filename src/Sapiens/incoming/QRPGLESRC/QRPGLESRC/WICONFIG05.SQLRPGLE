     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
      *===========================================================
     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
      *===========================================================
     h*  Description: Response program to serve Portal Role Association info
     h*
     h*  Created on 05/15/20   C:33  Project: SY406
      *===========================================================
      * Revisions:
      *
      *
      *===========================================================
      /copy sPrAtrium
      /copy sPrDate
      /copy sPrJson
      /copy sPrSql
      /copy sPrString
      /copy sPrTable
      /include yajl_h
      *Entry
     dGetAssociationInfo...
     d                 pr                  ExtPgm('WICONFIG05')
     d                              512
     d                            32767
     d                                1
     dGetAssociationInfo...
     d                 pi
     d inGenericParm                512
     d outJSON                    32767
     d rtnCode                        1

     ddsSIHUBPRCA    e ds                  extname(SIHUBPRCA)

     dintCo#           s              3  0 inz
     dstrCo#           s              3    inz
     dstrCriteria      s            250    varying inz
     dstrDisabled      s              5    varying inz('false')
     dstrProduct       s             25    varying inz
     dstrToolTip       s            100    varying inz
      /Free
       // inGenericParm format: |co#|product from javascript in onload event
       strCo# = ParseString(inGenericParm:'|':2);
       Monitor;
          intCo# = %int(strCo#);
       On-error;
       Endmon;
       strProduct = ParseString(inGenericParm:'|':3);
       strCriteria = 'SIproduct = ''' + strProduct + '''' +
        ' and SIsubProd <> '' ''';

       Exec sql
        select * into :dsSIHUBPRCA
         from SIHUBPRCA
         where SIproduct = :strProduct and SIsubProd <> ' '
         limit 1;
       If not isValidFetch(sqlstt);
          strDisabled = 'true';
          strCriteria = *blanks;
          strToolTip = 'No sub-products available for selected product.';
       EndIf;
       BuildJSON(outJSON :'criteria':strCriteria);
       BuildJSON(outJSON :'disabled':strDisabled);
       BuildJSON(outJSON :'toolTip':strToolTip);
       Return;
      /End-Free
