     H**************************************************************************************
     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H**************************************************************************************
     H*  Description:  Add Beast records for state not installed                           *
     H*                                                                                    *
     H*  Created: 03/19/24   Programmer C:079   Ticket: 115253                             *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FFmbel5    uf a e           k Disk
       //===================================================================================
       // Data Structures
       //===================================================================================
     D psDs          esds                  Extname(SZQ1P)
       //===================================================================================
       // Copybooks
       //===================================================================================
      /copy sprDate
       //===================================================================================
       // Add 4 records to FMBEL5 with state = blanks
       //===================================================================================
       Chain ('P':'N':'  ') PBE01;
       BEsys  = 'P';
       BEbTyp = 'N';
       BEhePg = 'F0351R';
       BEheSm = 'F0352R';
       BEdePg = 'F0353R';
       If Not %Found(FMBEL5);
          Exsr WriteFmbep;
       Else;
          Exsr UpdateFmbep;
       Endif;

       Chain ('H':'H':'  ') PBE01;
       BEsys  = 'H';
       BEbTyp = 'H';
       BEhePg = 'F0361R';
       BEheSm = 'F0362R';
       BEdePg = 'F0363R';
       If Not %Found(FMBEL5);
          Exsr WriteFmbep;
       Else;
          Exsr UpdateFmbep;
       Endif;

       Chain ('P':'X':'  ') PBE01;
       BEsys  = 'P';
       BEbTyp = 'X';
       BEhePg = 'F0371R';
       BEheSm = 'F0372R';
       BEdePg = 'F0373R';
       If Not %Found(FMBEL5);
          Exsr WriteFmbep;
       Else;
          Exsr UpdateFmbep;
       Endif;

       Chain ('P':'D':'  ') PBE01;
       BEsys  = 'P';
       BEbTyp = 'D';
       BEhePg = 'F0381R';
       BEheSm = 'F0382R';
       BEdePg = 'F0383R';
       If Not %Found(FMBEL5);
          Exsr WriteFmbep;
       Else;
          Exsr UpdateFmbep;
       Endif;

       *inlr = *on ;
       //**************************************************************
       // Subroutine WriteFmbep - Write records to file FMBEP
       //**************************************************************
       Begsr WriteFmbep;
          BEast  = '  ';
          BEsTyp = ' ';
          BEedi  = 'N';
          BEpPln = ' ';
          BEstat = 'A';
          BEuser = Q1user;
          BEpgm  = Q1pgm;
          BEtime = Timeto6(SystemTime());
          BEdt   = Dateto7(SystemDate());
          BEiUsr = BEuser;
          BEiPgm = BEpgm;
          BEiTim = BEtime;
          BEiDt  = BEdt;
          BEadCd = '1  ';
          BEedHd = ' ';
          BEedDt = ' ';
          BErevI = ' ';
          Write pbe01;
       Endsr;
       //**************************************************************
       // Subroutine UpdateFmbep - Update records in file FMBEP
       //**************************************************************
       Begsr UpdateFmbep;
          BEast  = '  ';
          BEsTyp = ' ';
          BEedi  = 'N';
          BEpPln = ' ';
          BEstat = 'A';
          BEuser = Q1user;
          BEpgm  = Q1pgm;
          BEtime = Timeto6(SystemTime());
          BEdt   = Dateto7(SystemDate());
          BEadCd = '1  ';
          BEedHd = ' ';
          BEedDt = ' ';
          BErevI = ' ';
          Update pbe01;
       Endsr;
