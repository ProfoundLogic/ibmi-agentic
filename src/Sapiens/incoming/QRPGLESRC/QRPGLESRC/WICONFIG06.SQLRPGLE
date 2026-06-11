     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
      *===========================================================
     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
      *===========================================================
     h*  Description: Response program to serve Portal Role Association info
     h*
     h*  Created on 09/11/20   C:33  Project: SY409
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
      *entry
     dGetRemarkCodeInfo...
     d                 pr                  ExtPgm('WICONFIG06')
     D                                9  0
     D                            32767
     D                                1
     dGetRemarkCodeInfo...
     d                 pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1

     ddsSIRLP        e ds                  inz extname('SIRLP')    qualified
     ddsSIRLPPRD     e ds                  inz extname('SIRLPPRD') qualified
     ddsSIRLPRMK       ds                  inz
     d rmCd                           5
     d beginW                         1

     dintCo#           s              3  0 inz
     dstrNoRemarkVisibility...
     d                 s             15    varying inz('hidden')
     dstrVisibility...
     d                 s             15    varying inz('visible')
     dstrCo#           s              3    inz
     dstrSql           s           1000    varying inz
      /Free
       Exec sql
        select * into :dsSIRLP
         from SIRLP
         where rrn(SIRLP) = :inRRN;

       Exec sql
        select * into :dsSIRLPPRD
         from SIRLPPRD
         where RLco#=:dsSIRLP.RLco# and RLkey=:dsSIRLP.RLkey
          and RLproduct='iDARTS'
         limit 1;

       Exsr BuildSql;
       Exec sql fetch next from cursorRmk into :dsSIRLPRMK;
       Select;
       When dsSIRLPPRD.RLproduct <> 'iDARTS';
          strVisibility = 'hidden';
       When not IsValidFetch(sqlstt) and dsSIRLP.RLkey <> *blank;
          Clear dsSIRLP.RLkey;
          Exsr BuildSql;
          strNoRemarkVisibility = 'visible';
       Other;
       Endsl;

       BuildJSON(outJSON :'sql':strSql);
       BuildJSON(outJSON :'visibility':strVisibility);
       BuildJSON(outJSON :'noRemarkVisibility':strNoRemarkVisibility);
       Return;

       //==============================================
       Begsr BuildSql;
        Exec sql close cursorRmk;
        strSql = 'select RLrmCd, RLbeginW ' +
         'from SIRLPRMK ' +
         'where RLco# = ' + %char(dsSIRLP.RLco#) +
         ' and RLkey = ''' + dsSIRLP.RLkey + '''';
        Exec sql prepare sqlRmk From :strSql;
        Exec sql declare cursorRmk cursor for sqlRmk;
        Exec sql open cursorRmk;
       Endsr;
      /End-Free
