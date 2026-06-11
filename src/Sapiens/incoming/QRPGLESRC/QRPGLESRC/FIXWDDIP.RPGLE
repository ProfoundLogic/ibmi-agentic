     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title FIXWDDIP - Fix the Status in WDDIP for the Closed Claims.
     h dftname(FIXWDDIP)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*CALLER)
     h/define   profoundui
     h bnddir('WTBNDDIR')
     h**********************************************************************
      *===============================================================*
      *  Created   : 08/25/14                                         *
      *  Programmer: C:283                                            *
      *  Support   : WC133 85974                                      *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fwddip     uf   e             disk    rename(pdi01:pdi0p)
     fwmchl     if   e           k disk    rename(pch01:pch01)

     d psds          esds                  extname(szq1p)
     d arrLvl          s              2  0 dim(11) ctdata perrcd(1)
     d arrMths         s              3  0 dim(11) alt(arrLvl) ascend
     d intDate8        s              8  0
     d xxIndex         s              3  0
     d xxMths          s              5  0

     d                 ds
     d intDate7                       7  0
     d  xxcc                          1  0 overlay(intDate7:1)
     d  xxyy                          2  0 overlay(intDate7:2)
     d  xxmm                          2  0 overlay(intDate7:4)
     d  xxdd                          2  0 overlay(intDate7:6)
     d/Copy SPRDATE

     c* Key list for setting the Pointer of Claim status history file
     c     keyWMCHL      klist
     c                   kfld                    DIco#
     c                   kfld                    DIfnd
     c                   kfld                    DIfyr
     c                   kfld                    DIcase
     c                   kfld                    intDate7

     c* Key list to Read the records in the Claim History File
     c     key1WMCHL     klist
     c                   kfld                    DIco#
     c                   kfld                    DIfnd
     c                   kfld                    DIfyr
     c                   kfld                    DIcase

      /free

       // main program processing

       // process WDDIP file
       Setll *Start wddip;
       Read wddip;
       Dow not %eof(wddip);
          If DIstat = 'S' And DIrplv > 0;
             //* Get the Reported Months
             xxIndex = %lookup(DIrplv:arrLvl);
             xxMths = arrMths(xxIndex);
             //* Get the future Date by adding the Reported Months to the Date Reported
             intDate8 = dtFuture(dtMDYY(DIdtrp) : xxMths : 'M');
             intDate7 = dtCYMD(intDate8);
             //* Calculate the Last Date of the Month
             xxDD = 01;
             intDate7 = dtCYMD(dtFuture(dtMDYY(intDate7) : 1 : 'M'));
             intDate7 = dtCYMD(dtPast(dtMDYY(intDate7) : 1));

             If intDate7 <> *Zeros;
                Setgt keyWMCHL wmchl;
                Readpe(N) key1WMCHL wmchl;
                If Not %Eof(wmchl) And CHocst = 'C';
                   DIstat = CHocst;
                   // Set the Maintenance Fields
                   DIuser = Q1user;
                   DIpgm = Q1pgm;
                   DIdt = DateTo7(systemDate);
                   DItime = TimeTo6(systemTime);
                   Update pdi0p;
                Endif;
             Endif;
          Endif;
          Read wddip;
       Enddo;

       *inlr = *on;

** Reporting Level and Months
01018
02030
03042
04054
05066
06078
07090
08102
09114
10126
11138
