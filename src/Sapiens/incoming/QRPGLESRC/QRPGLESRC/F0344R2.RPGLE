     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      //****************************************************************
     H/TITLE F0344R2 - EFT Eob Print Driver Program                   *
      //****************************************************************
      // Created on 05/27/2015 by c:62 for Project 114MRH              *
      //****************************************************************
      // Program Description:                                          *
      //                                                               *
      // Called From: F0344C                                           *
      // Calls to:    F0344C2                                          *
      //                                                               *
      //****************************************************************
      // Revisions:                                                    *
      //                                                               *
      //****************************************************************
     Ffyeal8    if   e           k Disk    Rename(pea01:pea08)
      //
      //
     D psds          esds                  ExtName(SZQ1P)

     d/copy sPrDate

     d EobPrint        pr                  ExtPgm('F0344C2')
     d   ##co#                        3    Const
     d   ##fein                       9    Const
     d   ##mod                        3    Const
     d   ##path                     100

     d inType          s              1
     D ##co#           s                   Like(EAco#)                          1
     D ##fein          s                   Like(EAfein)                         1
     D ##mod           s                   Like(EAmod)
     D ##path          s            100

     D isoDate         s               d   datfmt(*ISO)
     D strDate         s             10

     c* Entry Parameters
     c     *Entry        Plist
     c                   Parm                    inType
      /Free

       // Read thru the eob workfile
       Read pea08;
       DoW Not %Eof(FYEAL8);
          isoDate = systemDate();
          strDate = %Char(isoDate);
          ##path = '/EobPDF/' + 'EOB' + '_' + %EditC(EAfein:'X') + '_' +
                   %Subst(strDate:1:4) + '_' + %Subst(strDate:6:2) + '_' +
                   %Subst(strDate:9:2) + '_' + %EditC(EAmod:'X');
          If inType = 'G';
             ##path = %Trim(##path) + '_Group.PDF';
          Else;
             ##path = %Trim(##path) + '.PDF';
          EndIf;

          EobPrint(%EditC(EAco#:'X'):%EditC(EAfein:'X'):%EditC(EAmod:'X'):
                   ##Path);

          SetGt (EAco#:EAfein:EAmod) pea08;
          Read pea08;
       EndDo;

       *inLR = *On;

      /End-Free
