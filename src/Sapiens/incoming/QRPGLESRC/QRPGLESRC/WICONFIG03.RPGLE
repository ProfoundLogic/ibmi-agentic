     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
      *===========================================================
     hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
      *===========================================================
      *
      *  Description: Response program to serve Portal Configuration parent information
      *               - Activation group should be *CALLER
      *               - Do not seton LR, just do a return
      *               - This process should not lock any file
      *               - This process should only read files, no updates/writes should be made
      *
      *  Created on 02/12/19   C:136 Project: SY407
      *===========================================================
      * Revisions:
      *
      *
      *===========================================================
     fwtgtl     if   e           k disk    Rename(pgt01:pgt00)
      /copy sPrAtrium
      /copy sPrDate
      /copy sPrString
      /copy wPrTable
      /copy wPrUsrPrms
      *Entry
     dGetSecondFields  pr                  ExtPgm('WICONFIG03')
     d                              512
     d                            32767
     d                                1
     dGetSecondFields  pi
     d inGenericParm                512
     d outJSON                    32767
     d rtnCode                        1


     d intCo#          s              3  0
     d pipe            s              1    Inz('|')
     d strCat          s              4
     d strParms        s                   like(inGenericParm)
     d strTyp          s             10
     d strHeading1     s             20    Inz('Number 1 Heading')
     d strHeading2     s             20    Inz('Flag 1 Heading')
     d strHeading3     s             20    Inz('Flag 2 Heading')
      /Free
       // inGenericParm format: co#|ctgy|type from javascript in onload event
       //  of notification tab and maintenance screen
       strParms = inGenericParm;
       intCo# = %int(ParseString(strParms:pipe:1:*on));
       strCat = ParseString(strParms:pipe:1:*on);
       strTyp = ParseString(strParms:pipe:1:*on);
       Chain (intCo#:strCat:strTyp) pgt00;
       If Not %Found;
          *Inlr = *On;
          Return;
       EndIf;
       If GTfg02 = 'N';
          BuildJSON(outJSON :'headingNumber1':%trim(GTcf02 + GTcf03));
       EndIf;
       If GTfg02 = 'C';
          BuildJSON(outJSON :'headingFlag1':%trim(GTcf02 + GTcf03));
       EndIf;
       If GTfg04 = 'C';
          BuildJSON(outJSON :'headingFlag2':%trim(GTcf04 + GTcf05));
       EndIf;
       Return;
      /End-Free
