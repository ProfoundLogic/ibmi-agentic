     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('STBNDDIR')

     fsmdbp     uf a e           k disk    rename(pdb01:pdb0p)
     fsmdrp     uf a e           k disk    rename(pdr01:pdr0p)

     d #ofRcd          c                   20
     d db              s            100    dim(#ofRcd) ctdata perrcd(1)
     d dr              s             82    dim(#ofRcd) ctdata perrcd(1)

     d fldsDB        e ds                  extname(SMDBP)
     d fldsDR        e ds                  extname(SMDRP)
     d isClaimsOnly    s              1n   inz('0')
     d isClearRcd      s              1n   inz('0')
     d isWriteRcd      s              1n   inz('0')
     d sysDate7        s              7  0
     d sysTime6        s              6  0

     d psds          esds                  extname(szq1p)
     d index                          2  0
     d xxClt#          s              3

      *entry
     dCvtSY285         pr                  ExtPgm('CVTSY285')
     d inClearRcd                     1n   options(*nopass)
     dCvtSY285         pi
     d inClearRcd                     1n   options(*nopass)

     dRtvCltId         pr                  ExtPgm('RTVCLTID')
     d cltId                          3

     d/copy sPrDate
     d/copy sPrTable

      /Free
       If %parms >= 1;
          isClearRcd = inClearRcd;
       Endif;

       RtvCltId(xxClt#);
       sysDate7 = DateTo7(SystemDate);
       sysTime6 = TimeTo6(SystemTime);
       isClaimsOnly = ClaimsOnly;

       For index = 1 to #ofRcd;
          If db(index) <> *blanks;
             Clear pdb0p;
             fldsDB = db(index);
             Chain DBitem SMDBP;
             Select;
             When %found and isClearRcd;
                Delete pdb0p;
                isWriteRcd = *on;
             When %found;
                isWriteRcd = *off;
             Other;
                isWriteRcd = *on;
             Endsl;
             If isWriteRcd;
                fldsDB = db(index);
                DBstat = 'A';
                If DBitem = 'ChartsGrids';
                   DBstat = 'E';
                Endif;
                DBpgm  = q1Pgm;
                DBuser = q1User;
                DBdt   = sysDate7;
                DBtime = sysTime6;
                DBiPgm = DBpgm;
                DBiUsr = DBuser;
                DBiDt  = DBdt;
                DBiTim = DBtime;
                Write pdb0p;
             Endif;
          Endif;
          If dr(index) <> *blanks;
             Clear pdr0p;
             fldsDR = dr(index);
             Chain (DRrole:DRitem) SMDRP;
             Select;
             When %found and isClearRcd;
                Delete pdr0p;
                isWriteRcd = *on;
             When %found;
                isWriteRcd = *off;
             Other;
                isWriteRcd = *on;
             Endsl;
             If isWriteRcd;
                fldsDR = dr(index);
                Select;
                When isClaimsOnly and DRstat = 'Q';
              //When not isClaimsOnly and DRstat = 'P';
                Other;
                   DRstat = 'A';
                Endsl;
                DRpgm  = q1Pgm;
                DRuser = q1User;
                DRdt   = sysDate7;
                DRtime = sysTime6;
                DRiPgm = DRpgm;
                DRiUsr = DRuser;
                DRiDt  = DRdt;
                DRiTim = DRtime;
                Write pdr0p;
             Endif;
          Endif;
       Endfor;
       *inlr = *on;
      /End-free
      *  1         2         3         4         5         6         7         8         9
123456*890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
DBItem*                  HgtWthType           Procedure      Description
DRRole*                  Item                       X  YType
** db (field values)
New Claims               002002Chart          NewClaims      YNew Claims in Last
Closed Claims            002002Chart          ClosedClaims   YClosed Claims in Last
Litigation Claims        002002Chart          LitigationClaimYOpen Litigation Claims
Closed Settlements       002002Chart          ClosedSettle   YSettlements
Total Incurred           002002Chart          TotalIncurred  YTop Total Incurred - Open Claims
Large Loss Report        002002Chart          LargeLossRpt   YClaim Count - Incurred over
Cancellations            002002Chart          CxPolicies      Cancellations
Pending Audits           001002Grid           PendingAudits   Pending Audits
Policies Expiring        001002Grid           PolicyExpiring  Policies Expiring
Quoted Policies          001002Grid           Quoted          New/Renewal Quotes
Top 10 Agent Info        002002Chart          Top10AgentInfo  Top 10 Agency Information
Renewal Premium          002002ChartMS        RenewalPremium  Renewal Premium by Month
New Premium              002002Chart          NewPremium      New Premium by Month
Regional Losses          001004Grid           RegionalLosses  Regional Losses
SIC Losses               001004Grid           SICLosses       SIC Losses
ChartsGrids              004004iFrame                         Charts & Grids
** dr (field values)
Claims                   New Claims               001001Column3D
Claims                   Closed Claims            003001Column3D
Claims                   Litigation Claims        001003Column3D
Claims                   Closed Settlements       003003Line
Claims                   Total Incurred           001005Column3D
Claims                   Large Loss Report        003005Column3D
Policy                   Cancellations            003001Column3D
Policy                   Pending Audits           001002Grid
Policy                   Policies Expiring        001001Grid
Policy                   Quoted Policies          001003Grid                     Q
Policy                   Top 10 Agent Info        003003Column3D                 Q
Policy                   Top 10 Agent Info        002003Column3D                 P
Executive                Renewal Premium          001001MSColumn3D
Executive                New Premium              003001Column3D
Executive                Regional Losses          001003Grid
Executive                SIC Losses               001004Grid
