     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Claim reserve history detail box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTCMPCFG
     h*  Calls to:
     h*
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
      * TPA Contact Master Information
     fwmtcp     if   e             disk    rename(ptc01:ptc0p)
     f
     d
     d/copy sPrAtrium
     d
     dstrTPACd         s             10a   inz(' ')
     dstrTPADes        s             40a   inz(' ')
     dstrTcIp          s             15a   inz(' ')
     dstrTcFldr        s             80a   inz(' ')
     dstrTcUsId        s             12a   inz(' ')
     dstrTcSpSw        s             20a   inz(' ')
     dstrTcStat        s              1a   inz(' ')
     d
      * *ENTRY
     DTPADetail01      pr                  ExtPgm('WTCMPCFG01')
     D                                9  0
     D                            32767
     D                                1
      *
     DTPADetail01      pi
     D inRRN                          9  0
     D outJSON                    32767
     D RtnCode                        1
      *
      *
      /Free

       Chain(n) inRRN ptc0p;

        strTPACd  = TCtpac;
        strTPADes = TCdesc;
        strTcIp   = TCip;
        strTcFldr = TCfldr;
        strTcUsId = TCusId;
        strTcSpSw = TCspSw;
        strTcStat = TCstat;

       outJSON = ' ';
       BuildJSON(outJSON :'OutDtlTPACode' : %trim(strTPACd));
       BuildJSON(outJSON :'OutDtlTPADesc' : %trim(strTPADes));
       BuildJSON(outJSON :'OutDtlTPASiteIp' : %trim(strTcIp));
       BuildJSON(outJSON :'OutDtlTPASiteFlr' : %trim(strTcFldr));
       BuildJSON(outJSON :'OutDtlTPASiteUser' : %trim(strTcUsId));
       BuildJSON(outJSON :'OutDtlTPASitePwd' : %trim(strTcSpSw));

       RtnCode = *blanks;
       Return;

      /End-Free
