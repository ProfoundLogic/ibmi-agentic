     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      //****************************************************************
      // PROGRAM: W2420R03                                             *
      // DESCRIPTION: Group Bank Account E-pay Information             *
      //****************************************************************
      // CREATED: 07/20/20 C:79 PROJ:WC2749                            *
      //****************************************************************
      // INDICATORS:
      //****************************************************************
      //                                                               *
      // PROGRAMMER NOTES:                                             *
      //****************************************************************
      // REVISIONS:                                                    *
     //                                                               *
     // 04/19/21 C:079 Support: 90/95537                              *
     //                Add Bank Code to API                           *
      //****************************************************************
      //
     FW2420F03  CF   E             WORKSTN
      /If DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIf
      //
     FWMBIP     If   E             DISK
     FWDB2P     UF A E             DISK       Rename(pb201:dbrcdf)
     FWMEVML1   If   E           K DISK
      // Standalone fields
     d sys_date        s               D   datfmt(*USA)
     d sys_time        s               T
     d/COPY SCOPYMT01D
      //
      //================================================================
      //  /COPY required constants
      //================================================================
     D OPTNM           C                   CONST('Bank Information Mai-
     D                                     ntenance')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(2)

      //================================================================
      //  PLIST definitions
      //================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
      //
      //  Company file/fund (group) retrieve program parameter list
      //
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      //================================================================
      //  KLIST definitions
      //================================================================
      //
      //================================================================
      //  *LIKE DEFN statements
      //
      //================================================================
      //
     C/COPY SCOPYMT01C
      //
      //================================================================
      // DltPre - Pre-processing before delete occurs
      //================================================================
       Begsr DltPre;
       Endsr;
       //================================================================
       // DsplyR - Processing after displaying information
       //================================================================
       Begsr DsplyR;
       Endsr;
       //================================================================
       // ErrCk1 - Error checking for srceen 1 (SCR01)
       //================================================================
       Begsr ErrCk1;

             // Display the payment (reserve) types based on vendor
       isEpClmInd = *off;
       isEpClmMed = *off;
       isEpClmExp = *off;
       isEpClmLeg = *off;

       Chain (scEpVend) pevm01;

       Dow Not %Eof(wmevml1);
          If EVMpTyp = 'C';
             isEpClmInd = *On;
          Endif;
          If EVMpTyp = 'M';
             isEpClmMed   = *On;
          Endif;
          If EVMpTyp = 'L';
             isEpClmLeg = *On;
          Endif;
          If EVMpTyp = 'E';
             isEpClmExp = *On;
          Endif;
          Reade (scEpVend) pevm01;
       Enddo;
       Endsr;
       //================================================================
       // ERRCK2 - Error checking for srceen 2 (SCR02)
       //================================================================
       Begsr ErrCk2;
       Endsr;
       //================================================================
       // ERRCK3 - Error checking for srceen 3 (SCR03)
       //================================================================
       Begsr ErrCk3;
       Endsr;                                                                 //END SCRND
       //================================================================
       // MvDbSc - Move data base fields to screen fields
       //================================================================
       Begsr MvDbSc;
         If *IN40 = *ON;
           scEpVend   = *blanks;
           scVpFACTbl = *blanks;
           scVpFACff  = *blanks;
           scEpClmInd = *blanks;
           scEpClmMed = *blanks;
           scEpClmExp = *blanks;
           scEpClmLeg = *blanks;
           //
         Else;
           //
           If *IN99 = *OFF;
             //
             scEpVend   = B2ePvend;
             scVpFACTbl = B2vPfacTbl;
             scVpFACff  = B2vPfacff;
             scEpClmInd = B2ePclmInd;
             scEpClmMed = B2ePclmMed;
             scEpClmExp = B2ePclmExp;
             scEpClmLeg = B2ePclmLeg;
             //
           Endif;
           //
         Endif;

         // Display the payment (reserve) types based on vendor
         isEpClmInd = *off;
         isEpClmMed = *off;
         isEpClmExp = *off;
         isEpClmLeg = *off;

         Chain (scEpVend) pevm01;

         Dow Not %Eof(wmevml1);
            If EVMpTyp = 'C';
               isEpClmInd = *On;
            Endif;
            If EVMpTyp = 'M';
               isEpClmMed   = *On;
            Endif;
            If EVMpTyp = 'L';
               isEpClmLeg = *On;
            Endif;
            If EVMpTyp = 'E';
               isEpClmExp = *On;
            Endif;
            Reade (scEpVend) pevm01;
         Enddo;
         //
       Endsr;
       //================================================================
       // MvScDb - Move screen fields to data base fields
       //================================================================
       Begsr MvScDb;
         //
         Chain @@rrn2 pbi01;
         //
         If @@mode = 'A';
           B2co#      = BIco#;
           B2appl     = BIappl;
           B2fnd      = BIfnd;
           B2acct     = BIacct;
           B2efdt     = BIefdt;
        //*** Beg Add *** 04/19/21 ***************************************
           B2ckTp     = BIckTp;
        //*** End Add *** 04/19/21 ***************************************
         Endif;
         //
         B2ePvend   = scEpVend;
         B2vPfacTbl = scVpFACTbl;
         B2vPfacff  = scVpFACff;
         B2ePclmInd = scEpClmInd;
         B2ePclmMed = scEpClmMed;
         B2ePclmExp = scEpClmExp;
         B2ePclmLeg = scEpClmLeg;
         //
       Endsr;
       //
       //================================================================
       // OthErr - Processing for other modes
       //================================================================
       Begsr OthErr;
       Endsr;
       //
       //================================================================
       // OthFks - Processing for other functions keys
       //================================================================
       Begsr OthFks;
       Endsr;
       //
       //================================================================
       // PgmEnd - End of program processing
       //================================================================
       Begsr PgmEnd;
       Endsr;
       //
       //================================================================
       // PgmStr - Start of program processing
       //================================================================
       Begsr PgmStr;

       Select;
       When @@Mode = '1' or @@Mode = '2' or @@Mode = '3'or @@Mode = 'D';
          *in09 = *on;
       When @@Mode = 'A';
          *in40 = *on;
       When @@Mode = 'P';
          *in41 = *on;
       Endsl;

       Chain(N) @@RRN DbRcdF;
       If Not %Found(WDB2P);
          *in99 = *on;
       Endif;

       Endsr;
       //================================================================
       // PREOTH - Pre-processing for other modes
       //================================================================
       Begsr PreOth;
       Endsr;
       //================================================================
       // UPDATR - Update data base record after processing
       //================================================================
       Begsr UpdatR;
         //
         Chain @@RRN DbRcdF;
         *IN99 = NOT %Found;
         If *IN99 = *OFF;
            Exsr MvScDb;

            sys_Date = %Date();
            B2user = Q1user;
            B2pgm  = Q1pgm;
            B2dt = %dec(sys_Date:*cymd);
            B2time = %dec(%time():*HMS);

            Update DbRcdF;
         Endif;
         //
       Endsr;
       //================================================================
       // Writer - Write data base record and after processing
       //================================================================
       Begsr Writer;
         Clear DbRcdF;
         Exsr MvScDb;

         sys_Date = %Date();
         B2user = Q1user;
         B2pgm  = Q1pgm;
         B2dt = %dec(sys_Date:*cymd);
         B2time = %dec(%time():*HMS);

         B2iUsr = B2user;
         B2iPgm  = B2pgm;
         B2iDt = B2dt;
         B2iTim = B2time;

         Write DbRcdF;

         FEOD WDB2P;
         outFlg = '1';
       Endsr;
