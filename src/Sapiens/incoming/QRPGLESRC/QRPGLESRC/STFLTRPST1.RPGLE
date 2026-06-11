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
     h*
     fsmfpl     uf a e           k disk    rename(pfp01:pfp02)
     d intStart        s              5  0 inz(2)
     d intPos          S              2P 0
     d inUserID        S             10
     d inRcdFmt        S             10
     d inFieldID       S            200
     d inValue         S            200
     d inSeq#          S              5P 0
     d strSeq          S              5
      *
     dpsds           esds                  extname(szq1p)
      /copy sPrDate
      /copy sPrAtrium
      *Entry
     dUpdFltPrst       pr                  ExtPgm('STFLTRPST1')
     d                              512
     d                            32767
     d                                1
      *
     dUpdFltPrst       pi
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
       inFieldID = ExtractParm(inGenParm:',':intStart);
       inValue  = ExtractParm(inGenParm:',':intStart);
       strSeq = ExtractParm(inGenParm:',':intStart);
       If strSeq  <> *Blanks;
          inSeq# = %Dec(strSeq:5:0);
       Else;
          inSeq# = 1;
       Endif;

       outJSON = *Blank;
       outRtnCode = *Blank;
       Chain (inUserID:inRcdFmt:inFieldID:inSeq#) pfp02;
       If %Found(smfpl);
          FPstat   = 'A';
          FPuser   = q1user;
          FPpgm    = q1Pgm;
          FPdt     = DateTo7(systemdate);
          FPtime   = q1Time      ;
          Update pfp02;
       Else;
          FPuserId  = inUserID;
          FPrcdFmt  = inRcdFmt;
          FPfieldId = inFieldID;
          FPseq#    = inSeq#;
          FPvalue   = inValue;
          FPstat   = 'A';
          FPIUSR   = q1user;
          FPIPGM   = q1Pgm;
          FPIDT    = DateTo7(systemdate);
          FPITIM   = q1Time      ;
          FPUSER   = q1user;
          FPPGM    = q1Pgm;
          FPDT     = DateTo7(systemdate);
          FPTIME   = q1Time      ;
          Write pfp02;
       EndIf;
       Setll (inUserID:inRcdFmt:inFieldID) pfp02;
       ReadE (inUserID:inRcdFmt:inFieldID) pfp02;
       Dow not %Eof(smfpl);
          If FPSeq# > inSeq#;
             FPstat   = 'I';
             Update pfp02;
          EndIf;
          ReadE (inUserID:inRcdFmt:inFieldID) pfp02;
       EndDo;
       If inSeq# = 1;
         BuildJSON(outJSON :'RtnMsg' :'Your filter defaults have been stored.');
       EndIf;

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
