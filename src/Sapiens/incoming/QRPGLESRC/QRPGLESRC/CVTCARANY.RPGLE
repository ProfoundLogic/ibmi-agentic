     h*Copyright('(c) 2023 Sapiens. All Rights Reserved.')                                 *
     h/title CVTCARANY - Convert CARC and RARC Codes.                                      *
     h dftname(CVTCARANY)                                                                  *
     h option(*nodebugio: *srcstmt)                                                        *
     h/define   PROFOUNDUI                                                                 *
     h bnddir('STBNDDIR')                                                                  *
     h**************************************************************************************
     h*  Description:  Add CARC and RARC Codes to NY EOB Code Table file                   *
     h*                                                                                    *
     h*                                                                                    *
     h*  Created: 07/18/23   Programmer C: 342   Project: NY060                            *
     h**************************************************************************************
     h*  Revisions:                                                                        *
     h*                                                                                    *
     h**************************************************************************************
     fftablny   uf a e           k disk    usropn
     f                                     include(peb01)
     fcarccodes if   e             disk
     frarccodes if   e             disk
     d*
     d kkco#           s                   like(ebco#) inz
     d kkkey           s                   like(ebkey)
     d psds          esds                  extname(szq1p)
     d*
     d/copy sPrUsrPrms
     c*
     c     ebky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkkey
     c
      /free
       Open Ftablny;
       //Delete all records from FTEBPNY
       Read peb01;
       Dow not %eof(ftablny);
          delete peb01;
          Read peb01;
       Enddo;


       //Read CARC and RARC code file and update/write FTEBPNY as needed.
       kkco#  = GetUserInfo(q1user);
       Read pcc01;
       Dow not %eof(Carccodes);
          kkkey = CCeob;
          chain ebky peb01;
          If %found(Ftablny);
             exsr UpdatCarc;
          Else;
             exsr WrtCarc;
          Endif;
          Read pcc01;
       Enddo;

       Read prc01;
       Dow not %eof(Rarccodes);
          kkkey = RCeob;
          chain ebky peb01;
          If %found(Ftablny);
             exsr UpdatRarc;
          Else;
             exsr WrtRarc;
          Endif;
          Read prc01;
       Enddo;
       Close Ftablny;
       *Inlr = *on;


       Begsr WrtCarc;
        clear peb01;
        EBco#  = GetUserInfo(q1user);
        EBkey  = cceob;
        EBdesc = ccdesc;
        EBdsc2 = %subst(ccdesc:71:70);
        EBdsc3 = %subst(ccdesc:141:70);
        EBdsc4 = %subst(ccdesc:211:70);
        EBstat = 'A';
        EBuser  = q1user;                // User
        EBpgm   = q1pgm;                 // Program
        EBtime  = %Dec(%Time():*hms);    // Time
        EBdt    = %Dec(%Date():*cymd);   // Date Mintained
        EBiusr  = q1user;                // Initial User
        EBidt   = %Dec(%Date():*cymd);   // Initital Date
        write peb01;
       Endsr;

       Begsr WrtRarc;
        clear peb01;
        EBco#  = GetUserInfo(q1user);
        EBkey  = rceob;
        EBdesc = rcdesc;
        EBdsc2 = %subst(rcdesc:71:70);
        EBdsc3 = %subst(rcdesc:141:70);
        EBdsc4 = %subst(rcdesc:211:70);
        EBstat = 'A';
        EBuser  = q1user;                // User
        EBpgm   = q1pgm;                 // Program
        EBtime  = %Dec(%Time():*hms);    // Time
        EBdt    = %Dec(%Date():*cymd);   // Date Mintained
        EBiusr  = q1user;                // Initial User
        EBidt   = %Dec(%Date():*cymd);   // Initital Date
        write peb01;
       Endsr;

       Begsr UpdatCarc;
        EBdesc = ccdesc;
        EBdsc2 = %subst(ccdesc:71:70);
        EBdsc3 = %subst(ccdesc:141:70);
        EBdsc4 = %subst(ccdesc:211:70);
        EBuser  = q1user;                // User
        EBpgm   = q1pgm;                 // Program
        EBtime  = %Dec(%Time():*hms);    // Time
        EBdt    = %Dec(%Date():*cymd);   // Date Mintained
        update peb01;
       Endsr;

       Begsr UpdatRarc;
        EBdesc = rcdesc;
        EBdsc2 = %subst(rcdesc:71:70);
        EBdsc3 = %subst(rcdesc:141:70);
        EBdsc4 = %subst(rcdesc:211:70);
        EBuser  = q1user;                // User
        EBpgm   = q1pgm;                 // Program
        EBtime  = %Dec(%Time():*hms);    // Time
        EBdt    = %Dec(%Date():*cymd);   // Date Mintained
        update peb01;
       Endsr;

      /end-free
