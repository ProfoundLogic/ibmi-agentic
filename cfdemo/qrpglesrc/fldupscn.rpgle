**free

/////////////////////
// Duplicate detector run                                        GJA-916
//
// Scans the item master and writes candidate clusters to FLDUPC/FLDUPM.
// Safe to re-run: clusters already merged, rejected or deferred are left
// alone, so a reviewer's decisions are never re-raised.
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('FLETCHER');

/copy fldata_pr.rpgle

dcl-pr QMHSNDPM extpgm('QMHSNDPM');
  msgid char(7) const;
  msgfq char(20) const;
  msgdta char(256) const options(*varsize);
  msgdtalen int(10) const;
  msgtype char(10) const;
  callstk char(10) const;
  callstkcnt int(10) const;
  msgkey char(4);
  errc char(256) options(*varsize);
end-pr;

dcl-s run packed(9:0);
dcl-s created int(10) inz(0);
dcl-s skipped int(10) inz(0);
dcl-s err varchar(80);
dcl-s msg char(256);
dcl-s msgkey char(4);
dcl-ds errc qualified;
  bytesProv int(10) inz(0);
  bytesAvail int(10) inz(0);
end-ds;

run = %dec(%char(%date() : *iso0) : 8 : 0);

err = fl_scanDuplicates(run : created : skipped);

if err <> '';
  msg = 'Duplicate scan failed: ' + err;
else;
  msg = 'Duplicate scan complete. ' + %char(created) + ' new cluster(s), '
      + %char(skipped) + ' already decided and left alone.';
endif;

QMHSNDPM('CPF9897' : 'QCPFMSG   *LIBL     ' : msg : %len(%trimr(msg))
       : '*COMP' : '*EXT' : 0 : msgkey : errc);

*inlr = *on;
