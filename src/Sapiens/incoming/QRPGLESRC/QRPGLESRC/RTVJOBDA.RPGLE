     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)

     D Qwdrjobd        PR                  extpgm('QWDRJOBD')                   load jobd data
     Db                            1000a   options(*varsize)                    receiver
     D                               10i 0 const                                receiver length
     D                                8    const                                api format
     D                               20    const                                jobd and lib
     Db                                    like(dsApiErr)

      // extract library list info.
     D dsJobd0100      ds          3000    qualified
     D  dsLibOffset                  10i 0 overlay(dsJobd0100:361)
     D  dsLibCount                   10i 0 overlay(dsJobd0100:365)

      // error return code parm for APIs.
     D dsApiErr        DS                  qualified
     D  dsBytesIn                    10i 0
     D  dsBytesOut                   10i 0
     D  dsErrMsgId                    7a
     D  dsReserved                    1a
     D  dsMsgData                   112a

     DdsLibl           ds
     D aryLib                        10    dim(275) inz
     DcharLibl         s             10a   based(ptrLibl)
     DdecIndex         s              5u 0
     DdecLibCount      s             10i 0 inz(275)

      //--*ENTRY PARMS-------------------------------------------
     D p_RtvJobdA      PR                  extpgm('RTVJOBDA')
     D                               20a
     D                             2750a
     D p_RtvJobdA      PI
     D  inJobD                       20a
     D  outLibl                    2750a
      //---------------------------------------------------------
      /free

       // Call API to extract libraries
       callp QWDRJOBD(dsJobd0100:%size(dsJobd0100):'JOBD0100':
        inJobd:dsApiErr);

       ptrLibl = %addr(dsJobd0100) + dsJobd0100.dsLibOffset;
       If dsJobd0100.dsLibCount < decLibCount;
          decLibCount = dsJobd0100.dsLibCount;
       Endif;
       For decIndex  = 1 to decLibCount;
          aryLib(decIndex) = charLibl;
          ptrLibl += 11;
       Endfor;

       Monitor;
          outLibl = dsLibl;
       On-Error;
       Endmon;

       *inlr = *on;
       Return;
