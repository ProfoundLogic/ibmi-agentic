     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H*=====================================================================
     h dftactgrp(*no) actgrp(*caller)
     h bnddir('STBNDDIR')
     H*=====================================================================
     H* Created: 04/26/24 C: 079 Ticket: 115493
     H*=====================================================================
     H* File specifications
     H*=====================================================================
     FFXEOBCDP  if   e             Disk
     FFDDEL     uf   e           k Disk
     FFDCAL5    uf   e           k Disk
       //===================================================================
       // Standalone fields
       //===================================================================
       dcl-s  decCo# Like(CAco#);
       dcl-s  decDseq Zoned(5:0);
       dcl-s  decSub# Zoned(5:0);
       dcl-s  decEob# Zoned(9:0);
       dcl-s  strSql Char(500);
       //===================================================================
       // Data Stuctures
       //===================================================================
     d psds          esds                  extname(szq1p)
       dcl-ds dsEOBClm#;
        dcl-subf decEobClm# Zoned(13);
        dcl-subf decFnd Zoned(3:0) Pos(1);
        dcl-subf decFyr Zoned(3:0) Pos(4);
        dcl-subf decCase Zoned(7:0) Pos(7);
        end-ds;
       //===================================================================
       // Copybooks
       //===================================================================
     d/copy sprUsrPrms
     d/copy sprSql
     d/copy sprDate
       //===================================================================
       // Main
       //===================================================================
       Exec Sql
         set Option commit = *none;

       decCo# = GetUserInfo(Q1user);

       // Read through the workfile
       Read peob01;
       Dow Not %eof(FXEOBCDP);
          Exsr UpdateEOBcd;
          Read peob01;
       Enddo;

       *inlr = *on;
       //===================================================================
       // Subroutine UpdateEOBcd - Replace EOB Code(s) from workfile
       //===================================================================
       Begsr UpdateEOBcd;

         decEobClm# = EOBclm#;

         // Get the EOB number from the Bill ID
         strSql  = 'Select EAeob# from faeap where EAco#  = ' +
                   %EditC(decCo#:'X') + ' And EAfnd  = ' +
                   %EditC(decFnd:'X') + ' And EAsInv  = ''' +
                   %Trim(EOBbillID) + '''';

         Exec sql close cursorGetEob#;
         Exec sql prepare sqlGetEob# from :strSql;
         Exec sql declare cursorGetEob# cursor for sqlGetEob#;
         Exec sql open cursorGetEob#;

         Exec SQL fetch next from cursorGetEob# into :decEob#;

         // Get the Detail Sequence Number and Subscriber Number
         If IsValidFetch(sqlStt);
            strSql  = 'Select CAdseq,CAsub# from fdcap where CAco#  = ' +
                      %EditC(decCo#:'X') + ' And CAfnd  = ' +
                      %EditC(decFnd:'X') + ' And CAeob#  = ' +
                      %EditC(decEob#:'X') + ' And CAlin# = ' +
                      %EditC(EOBlin#:'X') + '';

            Exec Sql Prepare sqlFXEOBCDP from :strSql;
            Exec Sql Declare cursorFXEOBCDP Cursor For sqlFXEOBCDP;
            Exec Sql Open cursorFXEOBCDP;

            Exec SQL fetch next from cursorFXEOBCDP into :decDseq,:decSub#;
            If IsValidFetch(sqlStt);

               // Update with the replacement (new) EOB Code
               If EOBold <> *blanks;
                  Setll (decCo#:decSub#:decFnd:decFyr:decCase:decEob#:
                   decDseq) pde01;
                  Reade (decCo#:decSub#:decFnd:decFyr:decCase:decEob#:
                   decDseq) pde01;
                  Dow Not %Eof(FDDEL);
                     If DEeob = EOBold;
                        EvalR DEeob = %Trimr(EOBnew);
                        If EOBdesc <> *blanks;
                           DEnot1 = %Subst(EOBdesc:1:70);
                           DEnot2 = %Subst(EOBdesc:71:70);
                           DEnot3 = %Subst(EOBdesc:141:70);
                           DEnot4 = %Subst(EOBdesc:211:70);
                        Endif;
                        DEuser = Q1user;
                        DEpgm  = Q1pgm;
                        DEdt   = DateTo7(SystemDate);
                        DEtime = TimeTo6(Systemtime);
                        Update pde01;

                        // Now update FDCAP EOB code if this is the first EOB code
                        If DEseq# = 1;
                           Chain (decCo#:decSub#:decFnd:decFyr:decCase:decEob#:
                            decDseq) pca01;
                            If %Found(FDCAL5);
                               EvalR CAeomb = %Trimr(DeEob);
                               CAuser = Q1user;
                               CApgm  = Q1pgm;
                               CAdt   = DateTo7(SystemDate);
                               CAtime = TimeTo6(Systemtime);
                               Update pca01;
                            Endif;
                         Endif;
                     Endif;
                     Reade (decCo#:decSub#:decFnd:decFyr:decCase:decEob#:
                      decDseq) pde01;
                  Enddo;
               Endif;
            Endif;
         Endif;
       Endsr;

