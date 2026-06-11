     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title W8156RDRV - Gallagher-Basset Claim Update - Driver
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program calls the process without a screen
     h*
     h*  Called from: Menu
     h*  Calls to:
     h*
     h**********************************************************************
     h*  Created on 9/20/2016 by C:062 for Project: 114WYV
     h**********************************************************************
     h*  Revisions:
     h*
     h**********************************************************************

     d mode            s              1    Inz('Y')
     d docPath         s            201    Inz('/import/gbs.txt')
     d savePath        s            111    Inz('/import/save')

     DSubmitter        pr                  extPgm('W8156C')
     D mode                           1
     D docPath                      201
     D savePath                     111

      /free
       // Start Of Program
       docpath = %Subst(docPath:1:200) + '*';
       savepath = %Subst(savePath:1:110) + '*';
       Submitter(mode:docPath:savePath);

       // End Program
       *inLR = *On;
       return;

      /end-free
