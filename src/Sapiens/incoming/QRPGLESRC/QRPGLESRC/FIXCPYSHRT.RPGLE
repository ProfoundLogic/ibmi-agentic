     hcopyright('(c) 2021 Sapiens, Inc. All rights reserved.')

     h/title FIXCPYSHRT - Copy Short Rate WMSTP & WDSTP to other groups
     h dftname(FIXCPYSHRT)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h bnddir('STBNDDIR')
      //*********************************************************************
      //==============================================================*
      //  Created   : 09/17/21                                         *
      //  Programmer: C:300                                            *
      //  Support   : 97148 (AmWins) -- Group Maintenance - Option 23  *
      //  Description: This program will copy short rate data of group *
      //               #10 to all other groups in AmWins Production    *
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

      *
     c     keyWmstl      klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd


      /free
       // Archive the file(s) before coping data
       RunCmd('ARCOBJ OBJ(WMSTP) OBJTYPE(*FILE)':32);
       RunCmd('ARCOBJ OBJ(WDSTP) OBJTYPE(*FILE)':32);

       // Read Group Master file in loop
       Setll 1 pfn0p;
       Read pfn0p;
       Dow not %EOF(WMFNP);
          If FNfnd = 10;
             Read pfn0p;
             Iter;
          Endif;

          inCo# = FNco#;
          inFnd = 10;

          // Read Short Rate Master file in loop for group 10
          Setll (inCo#:inFnd) pst00;
          Reade (inCo#:inFnd) pst00;
          Dow not %EOF(WMSTL);
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

          // Read Short Rate Details file in loop for group 10
          Setll (inCo#:inFnd) pst02;
          Reade (inCo#:inFnd) pst02;
          Dow not %EOF(WDSTL);
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
