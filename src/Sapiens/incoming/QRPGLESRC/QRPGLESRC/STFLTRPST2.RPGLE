     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h BNDDIR('WTBNDDIR')
     h**********************************************************************
     h**********************************************************************
     h*  Description:   Generic AJAX program to serve user Filter Persistence
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from:
     h*  Calls to:
     h*
     h*  Created on 05/20/2016 by C:308 for Project-SY378
     h*             Grid Setting/Filter Persistence
     h**********************************************************************
     h* Revisions:
     h**********************************************************************
     fSMFPL     UF A E           k Disk    rename(pfp01:pfp02)
     d intStart        s              5  0 inz(2)
     d intPos          S              2P 0
     d inUserID        S             10
     d inRcdFmt        S             10
      *
     dpsds           esds                  extname(szq1p)
      /copy sPrDate
      /copy sPrAtrium
      *Entry
     dClrFltPrst       pr                  ExtPgm('STFLTRPST2')
     d                              512
     d                            32767
     d                                1
      *
     dClrFltPrst       pi
     d inGenParm                    512
     d outJSON                    32767
     d outRtnCode                     1
      *
     dExtractParm      pr                  like(inGenParm)
     d inParm                              like(inGenParm) const
     d inDelimitor                    1                 const
     d inStart                        5  0
      *
      /Free
       inUserID = ExtractParm(inGenParm:',':intStart);
       inRcdFmt = ExtractParm(inGenParm:',':intStart);

       outJSON = *Blank;
       outRtnCode = *Blank;
       Setll (inUserID:inRcdFmt) pfp02;
       ReadE (inUserID:inRcdFmt) pfp02;
       Dow not %Eof(smfpl);
          FPstat   = 'I';
          FPuser   = q1user;
          FPpgm    = q1Pgm;
          FPdt     = DateTo7(SystemDate);
          FPtime   = q1Time      ;
          Update pfp02;
          ReadE (inUserID:inRcdFmt) pfp02;
       EndDo;
       //Notify user records have been cleared
       BuildJSON(outJSON :'RtnMsg' :'Your filter defaults have been cleared.');

       return;
      /End-free

      *==================================================================
      * Extract entry parms from comma delimited
      *==================================================================
     pExtractParm      b
     dExtractParm      pi                  like(inGenParm)
     d inParm                              like(inGenParm) const
     d inDelimitor                    1                 const
     d inStart                        5  0

     d intLen          s              5  0 inz
     d intPos          s              5  0 inz
     d strParm         s                   like(inGenParm) inz

      /Free
       intPos = %Scan(',':inGenParm:inStart);
       If intPos <= 1;
          Return *blanks;
       Endif;
       intLen = intPos - inStart;
       strParm = %SubSt(inGenParm:inStart:intLen);
       inStart = intPos + 1;

       Return %trim(strParm);
      /End-free

     pExtractParm      e
