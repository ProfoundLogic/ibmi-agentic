     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h*****************************************************************
     h/TITLE API - WA120R - WCMS API to determine if ICD10 is active  *
     h*****************************************************************
     h* Description:  This program will be used to Read Data Area     *
     h*               ICD10 and retund that is ICD10 is active or     *
     h*               not.                                            *
     h*                                                               *
     h*  Called From: WTCLMDTL, WPICLMPOP                             *
     h*                                                               *
     h*  Calls To:    None                                            *
     h*                                                               *
     h*****************************************************************
     h* Created on 04/20/2015 by C:279   for  Project: WC2043         *
     h*****************************************************************
     h* Revisions:                                                    *
     h*                                                               *
     h*                                                               *
     h*****************************************************************
      * prototype
     d checkICD        pr                  extpgm('WA120R')
     d  @@InjuryDate                  7  0
     d  @@rtcd                        1

     d checkICD        pi
     d  @@InjuryDate                  7  0
     d  @@rtcd                        1

     d ICDEfDtDA       ds             8    dtaara(ICD10EFDT)
     d strEfDt                 1      8

      /copy sPrDate

      /free

       // Program Starts here

       // Read Date from ICD10 Data Area
       In *lock ICDEfDtDA;

       Select;
       When strEfDt = *blanks;
          @@rtcd = '1';

       // If ICD10 is Active set return code '2'
       When @@InjuryDate >= dtCYMD(%dec(%subst(strEfDt:1:8):8:0));
          @@rtcd = '2';

       // If ICD10 is inactive set return code '1'
       Other;
          @@rtcd = '1';
       Endsl;

       *inlr = *on;
      /end-free
