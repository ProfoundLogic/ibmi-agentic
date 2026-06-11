     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title STRECDRY - Recurring Diary Processing
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
      **********************************************************************
      *  Description:  Process recurring diaries
      *
      *  Called from: SPITRIGGER
      *  Calls to   : None
      *
      **********************************************************************
      *  Created on 08/16/17 C:062   Project: WC2501
      **********************************************************************
      * Revisions:
      *
      **********************************************************************
     fstctl1    if   e           k disk
     fsmrdl     uf a e           k disk    rename(prd01:prd00)
     fsdcmla    if   e           k disk    rename(pcm01:pcm0a)

     d psds          esds                  extname(szq1p)

     d/copy sPrDate
     d/copy sPrString

      * Write Darts Remark
     d WrtDrtRmk       pr                  extpgm('DA700R')
     d ##key                         40
     d ##comm                       256
     d ##cDat                         8
     d ##dcId                        12
     d ##pNam                        10
     d ##secl                         3
     d ##user                        10
     d ##pgm                         10
     d ##fold                        80
     d ##diFl                         1
     d ##iPgm                        10
     d ##fl01                         1
     d ##fl02                         1
     d ##rmCd                         5
     d ##seq#                         5
     d ##sbyl                         1
     d ##com2                        19
     d ##apId                         2
     d ##iDoc                        12
     d ##iQue                        10
     d ##imgF                         1
     d ##iPth                        10
     d ##pIdx                         5
     d ##qPty                         1
     d ##dcTp                         5
     d ##csId                         2
     d ##code                         2
     d ##trDT                         7
     d ##trTm                         6
     d ##stat                         1
     d ##iUsr                        10
     d ##dcDT                         7
     d ##fl03                         1
     d ##fl04                         1
     d ##fl05                         1
     d ##iDt                          7

     d* Entry
     d STRECDY         pr                  Extpgm('STRECDRYC')
     d inTrgName                     10
     d inKey                         41
     d inSeq#                         5
     d inOccur                        5
     d
     d STRECDY         pi
     d inTrgName                     10
     d inKey                         41
     d inSeq#                         5
     d inOccur                        5

     d* Work Fields
     d i               s              5  0
     d recurDate       s               d   DatFmt(*Usa)
     d lastRecurDate   s               d   DatFmt(*Usa)
     d strTrgName      s             10
     d chrKey          s             40
     d decSeq#         s              5  0
     d decOccur        s              5  0

     d* DA700R parameters
     d ##key           s             40
     d ##comm          s            256
     d ##cDat          s              8
     d ##dcId          s             12    inz(*blanks)
     d ##pNam          s             10    inz
     d ##secl          s              3    inz
     d ##user          s             10
     d ##pgm           s             10
     d ##fold          s             80    inz(*blanks)
     d ##diFl          s              1    inz
     d ##iPgm          s             10
     d ##fl01          s              1    inz(*blanks)
     d ##fl02          s              1    inz(*blanks)
     d ##rmCd          s              5
     d ##seq#          s              5    inz('00000')
     d ##sbyl          s              1    inz('N')
     d ##com2          s             19    inz(*blanks)
     d ##apId          s              2    inz(*blanks)
     d ##iDoc          s             12    inz(*blanks)
     d ##iQue          s             10    inz(*blanks)
     d ##imgF          s              1    inz(*blanks)
     d ##iPth          s             10    inz(*blanks)
     d ##pIdx          s              5    inz('00000')
     d ##qPty          s              1    inz(*blanks)
     d ##dcTp          s              5    inz(*blanks)
     d ##csId          s              2    inz(*blanks)
     d ##code          s              2    inz(*blanks)
     d ##trDT          s              7
     d ##trTm          s              6
     d ##stat          s              1    inz('A')
     d ##iUsr          s             10
     d ##dcDT          s              7    inz(' ')
     d ##fl03          s              1
     d ##fl04          s              1    inz(' ')
     d ##fl05          s              1    inz(' ')
     d ##iDt           s              7


      /free
       strTrgName = inTrgName;
       chrKey     = %Subst(inKey:1:40);
       decSeq#    = %Dec(inSeq#:5:0);
       decOccur   = %Dec(inOccur:5:0);

       // Get Darts Record
       Chain (chrKey:decSeq#) pcm0a;
       If %Found(sdcmla);
          //  Check what kind of recurring diary trigger is processing
          Select;
          // Check for Completed Diaries
          When strTrgName = 'RECDRYCOMP';
             Chain (CMkey:CMseq#) prd00;
             If %Found(smrdl) And RDrd# <> 0;
                // Adjust the count
                RDrd# = RDrd# - 1;
                // Write the next recurrence diary
                ExSr ProcRecDarts;
                RDseq# = %Dec(##seq#:5:0);
                // Update the sequence #
                Update prd00;
             EndIf;
          // Check for Add Diaries
          When strTrgName = 'RECDRYADD';
             ExSr ProcRecDarts;
          EndSl;
       EndIf;

       *inLR = *On;
       Return;

       //===================================================================
       // ProcRecDarts -  Process writing DARTS remark for Recurring diaries
       //===================================================================
       BegSr ProcRecDarts;

          // Get the Recurring Diary Inforamtion
          Chain CMrmCd pct01;

          For i=1 to decOccur;
             // Write Diary
             ##key  = CMkey;
             ##seq# = '00000';

             // date
             If i = 1;
                lastRecurDate = CvtToDate(CMcDat);
             EndIf;

             Select;
             When CTrdPt = 'D';
                 recurDate = lastRecurDate + %Days(CTrdPl);
             When CTrdPt = 'W';
                 recurDate = lastRecurDate + %Days(CTrdPl*7);
             When CTrdPt = 'M';
                 recurDate = lastRecurDate + %Months(CTrdPl);
             EndSl;

             ##cDat = %EditC(DateTo8(recurDate):'X');

             lastRecurDate = recurDate;

             ##comm = %Subst(CMcomm:1:256);
             ##com2 = %Subst(CMcomm:257:19);

             ##pnam = CMpNam;
             ##pgm  = Q1pgm;
             ##iPgm = Q1pgm;
             ##secl = '000';
             ##sByL = 'N';

             ##diFl = CMdiFl;
             ##fl01 = CMfl01;
             ##fl02 = CMfl02;
             ##com2 = ' ';
             ##apId = CMapId;
             ##iDoc = CMiDoc;
             ##iQue = CMiQue;
             ##iPth = CMiPth;
             ##pIdx = %EditC(CMpIdx:'X');
             ##qPty = CMqPty;
             ##dcTp = CMdcTp;
             ##csId = CMcsId;
             ##code = CMcode;
             ##user = CMuser;
             ##rmcd = CMrmCd;
             ##dcid = CMdcId;
             ##fold = CMfold;
             ##imgF = CMimgF;
             ##trDt = %EditC(DateTo7(systemDate()):'X');
             ##trTm = %EditC(TimeTo6(systemTime()):'X');
             ##iUsr = CMiUsr;
             ##fl03 = CTfl02;
             ##iDt  =  %EditC(DateTo7(systemDate()):'X');

             WrtDrtRmk(##key:##comm:##cDat:##dcId:##pNam:##secl:##user:
              ##pgm:##fold:##diFl:##iPgm:##fl01:##fl02:##rmCd:##seq#:
              ##sbyl:##com2:##apId:##iDoc:##iQue:##imgF:##iPth:##pIdx:
              ##qPty:##dcTp:##csId:##code:##trDt:##trTm:##stat:##iUsr:
              ##dcDt:##fl03:##fl04:##fl05:##iDt);
          EndFor;

       Endsr;

      /end-free
