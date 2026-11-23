**free

// ---------------------------------------------------------------------
// Program: whcoordsmk  (whcoord smoke test)
// Purpose: One-shot caller that exercises whcoord_open/fetch/close and
//          prints each row via SNDPGMMSG so a joblog + interactive
//          session confirms the service program is bound correctly.
//          Meant to be CALLed once from an interactive session:
//              CALL PGM(PERPDEMO/WHCOORDSMK) PARM('ACM')
// Epic:    PERP-4 (3D Warehouse Map) / PERP-27
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new) bnddir('PERP');

/copy whcoord_pr.rpgle

dcl-pr QMHSNDPM extpgm;
  msgId       char(7)   const;
  msgF        char(20)  const;
  msgData     char(256) const;
  msgDataLen  int(10)   const;
  msgType     char(10)  const;
  stackEntry  char(10)  const;
  stackCntr   int(10)   const;
  msgKey      char(4);
  errorCode   char(8)   const;
end-pr;

dcl-pi *n;
  p_company char(3);
end-pi;

dcl-s ok       ind;
dcl-s itemnbr  varchar(25);
dcl-s itemdsc  varchar(60);
dcl-s aisle    int(10);
dcl-s bay      int(10);
dcl-s shelf    int(10);
dcl-s xm       packed(9:4);
dcl-s ym       packed(9:4);
dcl-s zm       packed(9:4);
dcl-s onhand   packed(15:4);
dcl-s avail    packed(15:4);
dcl-s errmsg   varchar(80);
dcl-s numRows  int(10);
dcl-s line     char(256);
dcl-s msgkey   char(4);

numRows = 0;

ok = whcoord_open(p_company : errmsg);
if not ok;
  callMsg('whcoord_open failed for ' + %trim(p_company) + ': ' + errmsg);
else;
  dow whcoord_fetch(itemnbr : itemdsc : aisle : bay : shelf :
                     xm : ym : zm : onhand : avail : errmsg);
    numRows += 1;
    line = %trim(itemnbr) + ' (' + %trim(itemdsc) + ') aisle='
         + %char(aisle) + ' bay=' + %char(bay) + ' shelf=' + %char(shelf)
         + ' xyz=' + %char(xm) + ',' + %char(ym) + ',' + %char(zm)
         + ' onhand=' + %char(onhand) + ' avail=' + %char(avail);
    callMsg(line);
  enddo;

  if errmsg <> '';
    callMsg('whcoord_fetch error: ' + errmsg);
  endif;

  callMsg('whcoord: ' + %char(numRows) + ' row(s) for company '
        + %trim(p_company) + '.');

  whcoord_close();
endif;

*inlr = *on;
return;

dcl-proc callMsg;
  dcl-pi *n;
    text varchar(256) const;
  end-pi;
  dcl-s data char(256);
  data = text;
  QMHSNDPM(
    'CPF9897' :
    'QCPFMSG   QSYS      ' :
    data :
    %len(text) :
    '*INFO     ' :
    '*         ' :
    1 :
    msgkey :
    x'0000000000000000');
end-proc;
