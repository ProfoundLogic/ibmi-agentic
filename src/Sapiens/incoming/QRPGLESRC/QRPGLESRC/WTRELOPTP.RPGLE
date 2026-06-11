     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h/title WTRELOPTP - Relay Messenger Opt Out Report
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h bnddir('WTBNDDIR')
      *===============================================================*
      *  Created   : 07/27/16                                         *
      *  Programmer: C:136                                            *
      *  Project   : 114WYQ- Create Relay Messenger Message Process   *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      *===============================================================*
     fwxrlyp    if   e             disk
     fwxrlyp1   uf a e           k disk
     fwmcml1    uf   e           k disk
      /copy WprUsrPrms
     d psds          esds                  extname(szq1p)
     d lastRunValues   ds                  dtaara('WTRELOPT')
     d  svFileName                  250    overlay(lastRunValues)
     d  svPathName                  250    overlay(lastRunValues:*next)
     d  svDartsRmk                    5    overlay(lastRunValues:*next)
     dintCo#           s                   like(CMco#)
     dintFnd           s                   like(CMfnd)
     dintFyr           s                   like(CMfyr)
     dintCase          s                   like(CMcase)
     dUpdateMaint      pr
     dWriteDarts       pr                  ExtPgm('DA700R')
     d outKey                        40
     d outComm                      256
     d outCDat                        8
     d outDcId                       12
     d outPNam                       10
     d outSecl                        3
     d outUser                       10
     d outPgm                        10
     d outFold                       80
     d outDiFl                        1
     d outIPgm                       10
     d outFl01                        1
     d outFl02                        1
     d outRmCd                        5
     d outSeq#                        5
     d outSbyl                        1
     d outCom2                       19
     d outApId                        2
     d outIDoc                       12
     d outIQue                       10
     d outImgF                        1
     d outIPth                       10
     d outPIdx                        5
     d outQPty                        1
     d outDcTp                        5
     d outCsId                        2
     d outCode                        2
     dWriteMaint       pr
     dWriteRemark      pr
      /Free
       In(e) lastRunValues;
       If %error;
          Return;
       Endif;
       // Read Qtemp/Wxrlyp which was loaded from CpyFrmImpF
       //  in WTRELOPTC
       DoU %Eof(wxrlyp);
          Read prly01;
          If %eof;
             Leave;
          Endif;
          // Skip header record since CPYFRMIMPF can't use
          //  RMVCOLNAM until we can compile at 7.1
          If %subst(RLYCustId:1:8) = 'CUSTOMER';
             Iter;
          Endif;
          // Parse out key values for chain to WMCML1
          intCo#  = %int(%subst(RLYCustId:1:3));
          intFnd  = %int(%subst(RLYCustId:4:3));
          intFyr  = %int(%subst(RLYCustId:7:3));
          intCase = %int(%subst(RLYCustId:10:7));
          Chain(e) (intCo#:intFnd:intFyr:intCase) pcm01;
          Select;
          When not(%found);
             RLYerror = 'WMCMP not found';
          When (%error);
             RLYerror = 'WMCMP not allocated';
             WriteRemark();
          Other;
             Clear RLYerror;
             CMOptOut = 'Y';
             Update pcm01;
             WriteRemark();
          Endsl;
          // Write/Update archive file
          Chain RLYCustId prly101;
          If %found;
             UpdateMaint();
             Update prly101;
          Else;
             WriteMaint();
             Write  prly101;
          Endif;
       Enddo;
       *Inlr = *On;
       Return;
      /End-free
      *====================================================
     pUpdateMaint      b
      /Free
       RLYUser = Q1user;
       RLYPgm  = Q1pgm;
       RLYDate = %Dec(%Char(%Date():*Cymd0):7:0);
       RLYTime = %Dec(%Char(%Time():*Hms0):6:0);
      /End-free
     pUpdateMaint      e
      *====================================================
     pWriteMaint       b
      /Free
       UpdateMaint();
       RLYIusr  = Q1user;
       RLYIpgm  = Q1pgm;
       RLYIDat  = RlyDate;
       RLYITim  = RlyTime;
      /End-free
     pWriteMaint       e
      *====================================================
     pWriteRemark      b
     d  outKey         s             40
     d  outComm        s            256
     d  outCDat        s              8
     d  outDcId        s             12
     d  outPNam        s             10
     d  outSecl        s              3
     d  outUser        s             10
     d  outPgm         s             10
     d  outFold        s             80
     d  outDiFl        s              1
     d  outIPgm        s             10
     d  outFl01        s              1
     d  outFl02        s              1
     d  outRmCd        s              5
     d  outSeq#        s              5
     d  outSbyl        s              1
     d  outCom2        s             19
     d  outApId        s              2
     d  outIDoc        s             12
     d  outIQue        s             10
     d  outImgF        s              1
     d  outIPth        s             10
     d  outPIdx        s              5
     d  outQPty        s              1
     d  outDcTp        s              5
     d  outCsId        s              2
     d  outCode        s              2
     d  strCo#         s              3
     d  strCase        s              9
     d  strFnd         s              3
     d  strDiv         s              5
     d  strFyr         s              3

      /Free
       outKey  = %subst(RLYCustId:1:16);
       outCDat = %Char(%Date():*usa0);
       outPNam = 'W0070R';
       outRmCd = svDartsRmk;
       outUser = GetUserId(CMClEx);
       outPgm  = Q1pgm;
       outIPgm = Q1pgm;
       Select;
       When RLYerror = 'WMCMP not allocated';
          outComm = 'An unsuccessful attempt was made to ' +
           'disable SMS texting on ' + %trim(RLYOptDat) +
           '. Reprocess through opt out process.';
       Other;
          outComm = 'This claim opted out of SMS texting '+
           'on ' + %trim(RLYOptDat) + '.';
       Endsl;
       outCDat = %Char(%Date():*usa0);
       outPNam = 'W0070R';
       WriteDARTS (outKey  : outComm : outCDat : outDcId :
                   outPNam : outSecl : outUser : outPgm  :
                   outFold : outDiFl : outIPgm : outFl01 :
                   outFl02 : outRmCd : outSeq# : outSbyl :
                   outCom2 : outApId : outIDoc : outIQue :
                   outImgF : outIPth : outPIdx : outQPty :
                   outDcTp : outCsId : outCode );

      /End-free
     pWriteRemark      e
