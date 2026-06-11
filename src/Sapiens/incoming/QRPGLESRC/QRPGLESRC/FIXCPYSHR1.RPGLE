     hcopyright('(c) 2023 Sapiens, Inc. All rights reserved.')

     h/title FIXCPYSHR1 - Copy Short Rate WMSTP & WDSTP to other groups
     h dftname(FIXCPYSHR1)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h bnddir('STBNDDIR')
      //*********************************************************************
      //==============================================================*
      //  Created   : 01/11/23                                         *
      //  Programmer: C:291                                            *
      //  Project   : 148WDY(AmWins)                                   *
      //  Description: This program will copy short rate data of group *
      //               #503 to all other groups                        *
      //===============================================================*
      //  Revisions:                                                   *
      *                                                                *
      *                                                                *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

      * Fund Master file
     fwmfnp     if   e             disk    rename(pfn01:pfn0p)
      * Short Rate Master File
     fwmstl     if a e           k disk    rename(pst01:pst00)
      * Short Rate Detail File
     fwdstl     if a e           k disk    rename(pst01:pst02)

     d psds          esds                  extname(szq1p)
     d inCo#           s              3  0
     d inFnd           s              3  0

      /copy sPrDate
      /copy sPrRunCmd

      /free
       Exec Sql Set Option Commit = *None;

       //From Group
       inFnd = 503;

       // Archive the file(s) before copying data
       RunCmd('ARCOBJ OBJ(WMSTP) OBJTYPE(*FILE)':32);
       RunCmd('ARCOBJ OBJ(WDSTP) OBJTYPE(*FILE)':32);

       // Delete all the records other than From Group in Short Rate files
       Exec sql
        Delete from WMSTP
         Where STfnd <> :inFnd;
       Exec sql
        Delete from WDSTP
         Where STfnd <> :inFnd;

       // Read Group Master file in loop
       Setll 1 pfn0p;
       Read pfn0p;
       Dow not %Eof(WMFNP);
          If FNfnd = inFnd;
             Read pfn0p;
             Iter;
          Endif;

          inCo# = FNco#;

          // Read Short Rate Master file in loop for From Group
          Setll (inCo#:inFnd) pst00;
          Reade (inCo#:inFnd) pst00;
          Dow not %Eof(WMSTL);
             STfnd = FNfnd;                 // Assign Group
             STuser = q1user;
             STpgm = q1pgm;
             STtime = Timeto6(SystemTime());
             STdt = Dateto7(SystemDate());
             STitim = Timeto6(SystemTime());
             STidt = Dateto7(SystemDate());
             STiusr = q1user;
             STipgm = q1pgm;
             Write pst00;
             Reade (inCo#:inFnd) pst00;
          Enddo;

          // Read Short Rate Details file in loop for From Group
          Setll (inCo#:inFnd) pst02;
          Reade (inCo#:inFnd) pst02;
          Dow not %Eof(WDSTL);
             STfnd = FNfnd;                 // Assign Group
             STuser = q1user;
             STpgm = q1pgm;
             STtime = Timeto6(SystemTime());
             STdt = Dateto7(SystemDate());
             STitim = Timeto6(SystemTime());
             STidt = Dateto7(SystemDate());
             STiusr = q1user;
             STipgm = q1pgm;
             Write pst02;
             Reade (inCo#:inFnd) pst02;
          Enddo;

          Read pfn0p;
       Enddo;

       *InLr = *On;
      /end-free
