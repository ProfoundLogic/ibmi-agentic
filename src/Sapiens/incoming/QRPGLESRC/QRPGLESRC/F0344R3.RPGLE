     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      //****************************************************************
     H/TITLE F0344R2 - EFT Change EAEPFL TO A 'P'                     *
      //****************************************************************
      // Created on 05/27/2015 by c:62 for Project 114MRH              *
      //****************************************************************
      // Program Description:                                          *
      //                                                               *
      // Called From: F0344C                                           *
      // Calls to:    None                                             *
      //                                                               *
      //****************************************************************
      // Revisions:                                                    *
      //                                                               *
      //****************************************************************
     ffaeal1    uf   e           k Disk
     ffyeap     if   e             Disk    Rename(pea01:pea0p)
     fszdbp     if   e             Disk    Rename(pdb01:pdb0p)

     d psds          esds                  ExtName(SZQ1P)
     d printDate       s                   Like(EAepDt)                         1

     d/copy sPrDate

      /Free

       Chain 1 pdb0p;
       If %Found(SZDBP);
          printDate = dtYYMD(DBsDt);
       EndIf;

       // Read the Eob Work File
       Read pea0p;
       DoW Not %Eof(FYEAP);
          Chain (EAco#:EAsub#:EAfnd:EAfyr:EAcase:EAeob#:EArcn) pea01;
          If %Found(FAEAL1);
             EAepFl = 'P';
             EAepDt = printDate;
             EApgm  = q1Pgm;
             EAdt   = DateTo7(systemDate());
             EAtime = TimeTo6(systemTime());
             Update pea01;
          EndIf;

          Read pea0p;
       EndDo;

       *inLR = *ON;

      /End-Free
