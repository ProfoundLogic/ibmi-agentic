     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Portal Configuration detail box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Created on 02/19/19   C:136 Project: SY407
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwinotip   if   e             Disk    Rename(pno01:pno0p)
     fwtgtl     if   e           k Disk    Rename(pgt01:pgt00)
      /copy sPrDate
      /copy sPrJson
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /Include yajl_h

      *entry
     DGetDetail        pr                  ExtPgm('WICONFIG04')
     D                                9  0
     D                            32767
     D                                1
     DGetDetail        pi
     D inRRN                          9  0
     D outJSON                    32767
     D RtnCode                        1
     d strYesOrNo      s              3a   Inz('No')
      /Free
       Clear outJSON;
       Chain inRRN pno0p;
       Chain (NOco#:NOctgy:NOtype) pgt00;
       If Not %Found;
          *Inlr = *On;
          Return;
       EndIf;
       Select;
       When GTfg02 = 'N';
          BuildJSON(outJSON :'LblGeneric1':%Trim(GTcf02 + GTcf03));
          BuildJSON(outJSON :'OutGeneric1':%Editc(NOnumb1:'X'));
          BuildJSON(outJSON :'LblGeneric2':' ');
          BuildJSON(outJSON :'OutGeneric2':' ');
       When GTfg02 = 'C' and GTfg04 = *Blank;
          BuildJSON(outJSON :'LblGeneric1':%trim(GTcf02 + GTcf03));
          If NOflag1 = 'Y';
             strYesOrNo = 'Yes';
          Endif;
          BuildJSON(outJSON :'OutGeneric1':strYesOrNo);
          BuildJSON(outJSON :'LblGeneric2':' ');
          BuildJSON(outJSON :'OutGeneric2':' ');
       When GTfg02 = 'C' and GTfg04 = 'C';
          BuildJSON(outJSON :'LblGeneric1':%trim(GTcf02 + GTcf03));
          If NOflag1 = 'Y';
             strYesOrNo = 'Yes';
          Endif;
          BuildJSON(outJSON :'OutGeneric1':strYesOrNo);
          BuildJSON(outJSON :'LblGeneric2':%trim(GTcf04 + GTcf05));
          If NOflag2 = 'Y';
             strYesOrNo = 'Yes';
          Endif;
          BuildJSON(outJSON :'OutGeneric2':strYesOrNo);
       Other;
          BuildJSON(outJSON :'LblGeneric1':' ');
          BuildJSON(outJSON :'OutGeneric1':' ');
          BuildJSON(outJSON :'LblGeneric2':' ');
          BuildJSON(outJSON :'OutGeneric2':' ');
       Endsl;
       BuildJSON(outJSON :'OutEmailSubject':%Trim(NOsubj));
       //BuildGiantJSON(outJSON :'OutEmailBody':%Trim(NObody));
       outJson = %trim(outJson) + ',' +
        BuildJsonNode('OutEmailBody':%Trim(NObody));
       *Inlr = *On;
       Return;
      /End-free
