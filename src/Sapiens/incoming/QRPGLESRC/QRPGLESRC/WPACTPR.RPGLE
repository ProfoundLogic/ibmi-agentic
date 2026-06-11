
        //     Program: launchR
        //        Type: RPGLE
        // Description: Dynamic Launch Program
        //
        // Copyright (c) Profound Logic Software, Inc.  All rights reserved.

     H DFTACTGRP(*NO)
     H/DEFINE   PROFOUNDUI

     Fwpactpf   CF   E             WORKSTN EXTFILE('wpactpf')
     D*
      /Free

         TxTitle = 'Johander, Sven';
         TxHeading1 = 'Claim Number';
         TxField1 = '777-110-0000007';

         Dou scSelOpt = 'OPT5';
           ExFmt SCR01;
           Select;
           When scSelOpt = 'OPT1';
             TxHeading1 =  'Option 1 clicked';
           When scSelOpt = 'OPT2';
             TxHeading1 =  'Option 2 clicked';
           When scSelOpt = 'OPT3';
             TxHeading1 =  'Option 3 clicked';
           When scSelOpt = 'OPT4';
             TxHeading1 =  'Option 4 clicked';
           When scSelOpt = 'OPT5';
             leave;
           EndSl;
         EndDo;

         *InLr = *On;
         return;

      /End-Free

