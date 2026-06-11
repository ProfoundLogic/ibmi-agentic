     h*Copyright('(c) 2023 Sapiens. All Rights Reserved.')                                 *
     h/title CVTEOBCODE                                                                    *
     h dftname(CVTEOBCODE)                                                                 *
     h option(*nodebugio: *srcstmt)                                                        *
     h/define   PROFOUNDUI                                                                 *
     h bnddir('STBNDDIR')                                                                  *
     H**************************************************************************************
     H*  Description:  - Map EOB Codes into the new field.                                 *
     H*                                                                                    *
     H*                                                                                    *
     H*  Created: 07/18/23   Programmer C: 342   Project: NY060                            *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     ffdfrp     uf a e           k disk
     ffddep     uf a e           k disk
     ffaeep     uf a e           k disk
     fftebpcc   uf a e           k disk
     d*
     d isRun           s              1    inz
     d psds          esds                  extname(szq1p)
     d CvtEobDS        ds             1    dtaara(CVTEOBCODE)
     d strRun                  1      1
     d/copy sPrUsrPrms
      /free
       // Read Data area to check if need to run program.
       In *lock CvtEobDS;
       isRun = strRun;

       If isRun = 'Y';
          Exsr ProgramEnd;
       Else;
          Exsr MapFReob;
          Exsr MapDEeob;
          Exsr MapEEeob;
          Exsr Add236ebp;
          strRun = 'Y';
          Out CvtEobDS;
          Exsr ProgramEnd;
       Endif;

       Begsr ProgramEnd;
          *inlr = *on;
       Endsr;

       //Read FDFRP file and map FReob with FReobCd1.
       Begsr MapFReob;
        Read pfr01;
        Dow not %eof(Fdfrp);
           FReobCd1 = FReob;
           FRuser  = q1user;                // User
           FRpgm   = q1pgm;                 // Program
           FRtime  = %Dec(%Time():*hms);    // Time
           FRdt    = %Dec(%Date():*cymd);   // Date Mintained
           Update pfr01;
           Read pfr01;
        Enddo;
       Endsr;

       //Read FDDEP file and map DEeob with DEeobCd.
       Begsr MapDEeob;
        Read pde01;
        Dow not %eof(Fddep);
           DEeobCd = DEeob;
           DEuser  = q1user;                // User
           DEpgm   = q1pgm;                 // Program
           DEtime  = %Dec(%Time():*hms);    // Time
           DEdt    = %Dec(%Date():*cymd);   // Date Mintained
           Update pde01;
           Read pde01;
        Enddo;
       Endsr;

       //Read FAEEP file and map EEeob with EEeobCd.
       Begsr MapEEeob;
        Read pee01;
        Dow not %eof(Faeep);
           EEeobCd = EEeob;
           EEuser  = q1user;                // User
           EEpgm   = q1pgm;                 // Program
           EEtime  = %Dec(%Time():*hms);    // Time
           EEdt    = %Dec(%Date():*cymd);   // Date Mintained
           Update pee01;
           Read pee01;
        Enddo;
       Endsr;

       //Add CARC 236 to FTEBPCC.
       Begsr Add236ebp;
        Read peb01;
        Clear peb01;
        EBco#   = GetUserInfo(q1user);
        EBkey   = '236';
        EBstat  = 'A';
        EBdesc  = 'This procedure or procedure/modifier combination is not +
                   compatible wit';
        EBdsc2  = 'h another procedure or procedure/modifier combination +
                   provided on the ';
        EBdsc3  = 'same day according to the National Correct Coding +
                   Initiative or worker';
        EBdsc4  = 's compensation state regulations/ fee schedule +
                   requirements.';
        EBuser  = q1user;                // User
        EBpgm   = q1pgm;                 // Program
        EBtime  = %Dec(%Time():*hms);    // Time
        EBdt    = %Dec(%Date():*cymd);   // Date Mintained
        EBiusr  = q1user;                // User
        EBidt   = %Dec(%Date():*cymd);   // Date
        Write peb01;
       Endsr;

      /end-free
