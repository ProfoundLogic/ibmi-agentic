     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/define CONVERT
     H/TITLE FIXRSVDATE - Fix reserve history date
     H*****************************************************************
     H* Description:  Assign latest effective date.                   *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*****************************************************************
     FWMRHP     UF   E             DISK

     d  maxEffDt       s              7  0

     D PSDS          ESDS                  EXTNAME(SZQ1P)

      /free
       Read prh01;
       DoW Not %Eof(wmrhp);
          // Only process if it is a record created under fixrsvbal
          If RHpgm = 'FIXRSVBAL';
             Exec SQL Select Max(RHefDt) Into :maxEffDt From WMRHP
             Where RHfnd = :RHfnd And RHfYr = :RHfYr And RHcase = :RHcase
             And RHPGM <> 'FIXRSVBAL';
             RHefDt = maxEffDt;
             Update prh01;
          EndIf;
          Read prh01;
       EndDo;

       *inLR = *On;
      /end-free
