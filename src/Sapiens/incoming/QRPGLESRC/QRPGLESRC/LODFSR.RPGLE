     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/define CONVERT
      //****************************************************************
      // StoneRiver is a trademark of StoneRiver, Inc.                 *
      //****************************************************************
      // Auto-Load Fee Schedule Rules and Messages Program             *
      //                                                               *
      // CREATED: 11/14/2014    C: 274                                 *
      //                                                               *
      // Called from: LODFSC                                           *
      // Calls to:    none                                             *
      //                                                               *
      // Notes - FMFSDRVP/DRVProcess values:                           *
      // ' ' = Unprocessed FMFSDRVP record.                            *
      // 'Y' = Processed.  Records written to FDFRP or FTMIP.          *
      // 'N' = Record proccessed, but no records written to FDFRP or   *
      //       FTMIP.  This is typically a key to file issue.          *
      //       Further investigation is needed.                        *
      //       Correct and blank out DRVPPOCESS flag and rerun.        *
      // 'E' = Error.  State (DRVSt) or Release (DRVRel) blank.        *
      //       Correct and blank out DRVPPOCESS flag and rerun.        *
      //****************************************************************
      // Revisions:                                                    *
      //                                                               *
      // 3/14/19 C: 079 Support: 99/77688                              *
      //                Check file maintenance dates                   *
      //                                                               *
      // 5/19/20 C: 079 Support: 41637                                 *
      //                Check file maintenance dates                   *
      //****************************************************************

      //================================================================
      // FILES                                                         *
      //================================================================
     F*** Beg Chg *** 03/14/19 *****************************************
     F****FMFSDRVP  UF   E           K DISK
     FFMFSDRVP  UF   E           K DISK    UsrOpn
     F*** End Chg *** 03/14/19 *****************************************
     FSTSTL1    IF   E           K DISK
     FFDFRL10   UF A E           K DISK
     FFDFRL3    UF   E           K DISK    RENAME(PFR01:PFR03)
     F*** Beg Chg *** 03/14/19 *****************************************
     F****FDFRPLODL1UF A E           K DISK    RENAME(PFR01:PFR01L)
     FFDFRPLODL1UF A E           K DISK    RENAME(PFR01:PFR01L) UsrOpn
     F*** End Chg *** 03/14/19 *****************************************
     FFTMIL3    UF A E           K DISK
     FFTMIL2    UF A E           K DISK    RENAME(PMI01:PMI02)
     F*** Beg Chg *** 03/14/19 *****************************************
     F****FTMIPLODL1UF A E           K DISK    RENAME(PMI01:PMI01L)
     FFTMIPLODL1UF A E           K DISK    RENAME(PMI01:PMI01L) UsrOpn
     F*** End Chg *** 03/14/19 *****************************************

      //================================================================
      // Stand alone fields                                            *
      //================================================================
     D @@Lib           S             10
     D @@StrRls        S              6
     D @@EndRls        S              6
     D @@RtCd          S              1
     D StrRls          S              6
     D EndRls          S              6
     D StateKey1       S             10
     D RelKey          S              6
     D StateKey2       S              2
     D EffDtKey        S              7  0
     D flgSuccess      S              1
     D DTCYMD8         S              8  0
     D*** Beg Add *** 05/19/16 *******************************
     D curFrDt         S              7  0
     D newFrDt         S              7  0
     D*** End Add *** 05/19/16 *******************************

      //================================================================
      //  User defined data structures                                 *
      //================================================================
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     d*** Beg Add *** 03/14/19 *********************************************
      /Copy sprModule
     d*** End Add *** 03/14/19 *********************************************

      //================================================================
      // Procedure definitions                                         *
      //================================================================
     DDateTo7          PR             7  0
     D date                            d   Const

     DTimeTo6          pr             6  0
     D time                            T   Const

     DsystemDate       pr              D

     DsystemTime       pr              T

     DdtCYMD           PR             7  0
     DdtCYMD8                         8  0 Const

      //================================================================
      //  KLIST, PLIST definitions                                     *
      //================================================================
     C     keyFMFSDRVP   KLIST
     C                   KFLD                    STRRLS

     C     keySTSTL1     klist
     C                   kfld                    StateKey1

     C     keyFDFRL10    klist
     C                   kfld                    RelKey
     C                   kfld                    StateKey2
     C                   kfld                    EffDtKey

     C     key1FDFRL3    klist
     C                   kfld                    StateKey2
     C                   kfld                    EffDtKey

     C     key2FDFRL3    klist
     C                   kfld                    StateKey2

     C     key1FDFRL10   klist
     C                   kfld                    RelKey
     C                   kfld                    StateKey2

     C     keyFDFRPLODL1 klist
     C                   kfld                    RelKey
     C                   kfld                    StateKey2
     C                   kfld                    EffDtKey

     C     key1FDFRPLODL1klist
     C                   kfld                    RelKey
     C                   kfld                    StateKey2

     C     keyFTMIL2     klist
     C                   kfld                    DRVSt

     C     keyFTMIL3     klist
     C                   kfld                    DRVRel

     C     keyFTMIPLODL1 klist
     C                   kfld                    DRVRel

     C     *ENTRY        PLIST
     C                   PARM                    @@Lib                          Library
     C                   PARM                    @@StrRls                       Start release
     C                   PARM                    @@EndRls                       End release
     C                   PARM                    @@RtCd                         Return code

      //=========================================================================
      // Main Line processing                                                   *
      //=========================================================================
      //  Read FMFSDRVP - Fee Schedule Auto-Load Driver File
      /FREE
       //*** Beg Add *** 03/14/19 ******************************************
       if isModEnabled('Pbills Lite');
          *inlr = *on;
          Return;
       Endif;
       Monitor;
       Open fmfsdrvp;
       On-Error;
       EndMon;
       //*** End Add *** 03/14/19 ******************************************
       //*** Beg Add *** 03/14/19 ******************************************
       If %Open(fmfsdrvp);

       Monitor;
       Open fdfrplodl1;
       On-Error;
       Endmon;

       Monitor;
       Open ftmiplodl1;
       On-Error;
       Endmon;
       //*** End Add *** 03/14/19 ******************************************
       SetLL keyFMFSDRVP DR101;
       Read DR101;
       Dow not(%eof);
         flgSuccess  = 'N';
         If DRVRel >= StrRls                                                  //Start release
               and DRVRel <= EndRls                                           //End release
               and DRVProcess = *blank;                                       //Unprocessed

        //  Read STSTL1 - System-Wide State Code Table File. Make sure state is installed.
           StateKey1 = DRVSt;
           Chain keySTSTL1 PST01;
           If %FOUND;

             Select;
             //  Load FDFRP - Fee Schedule Rules file
        //*** Beg Chg *** 03/14/19 *****************************************
        //***When DRVType = 'R';                                              //Rules
             When DRVType = 'R' and %Open(fdfrplodl1);                        //Rules
        //*** End Chg *** 03/14/19 *****************************************
               EXSR LoadRules;
             //  Load FTMIP - Bill Entry Message ID Table file
        //*** Beg Chg *** 03/14/19 *****************************************
        //***When DRVType = 'M';                                              //Messages
             When DRVType = 'M' and %Open(ftmiplodl1);                        //Messages
        //*** End Chg *** 03/14/19 *****************************************
               EXSR LoadMessages;
             Endsl;
           Endif;
         Endif;

         Read DR101;
       Enddo;
       //*** Beg Add *** 03/14/19 ******************************************
       Endif;
       //*
       //* Close files (if they are open)
       //*
       If %Open(fmfsdrvp);
          Close fmfsdrvp;
       Endif;
       If %Open(fdfrplodl1);
          Close fdfrplodl1;
       Endif;
       If %Open(ftmiplodl1);
          Close ftmiplodl1;
       Endif;
       //*** End Add *** 03/14/19 ******************************************

       *INLR = *ON;
       Return;

       //*** Beg Add *** 05/19/16 *******************************
       //=============================================================================
       // CheckDates Subroutine:  Checks the current rules dates against what we are *
       // attempting to load                                                         *
       //=============================================================================
       BEGSR CheckDates;
         // Determine if the maintenance date on the rules is greater
         // than the maintenance date of the rules we are loading
         // If so then STOP
         curFrDt = 0;
         SetLL key1FDFRL3  PFR03;
         Reade key1FDFRL3  PFR03;
         Dow not(%eof);
           if frdt > curFrDt;
             curFrDt = frdt;
           Endif;
           Reade key1FDFRL3  PFR03;
         Enddo;
         newFrDt = 0;
         SetLL keyFDFRPLODL1 PFR01L;
         Reade keyFDFRPLODL1 PFR01L;
         Dow not(%eof);
           if frdt > newFrDt;
             newFrDt = frdt;
           Endif;
           Reade keyFDFRPLODL1 PFR01L;
         Enddo;
       Endsr;
       //*** End Add *** 05/19/16 *******************************
       //=========================================================================
       // LoadRules Subroutine:  Loads FDFRP - Fee Schedule Rules file           *
       //                        Replace or Add                                  *
       //=========================================================================
       Begsr LoadRules;

         //===> Action = "R" - Relpace
         Select;
         // Release = blank or State = blank  --> Error
         When DRVAction = 'R';
           If DRVRel = *BLANK                                                 //release
             or DRVSt = *BLANK;                                               //state
             DRVProcess = 'E';                                                //Error
             DRVTime  = Timeto6(SystemTime());
             DRVDt    = Dateto7(SystemDate());
             DRVUser  = Q1USER;
             DRVPgm   = Q1PGM;
             Update DR101;
           Else;

             // Release <> blank, State <> blank, Efective Date <> zero
             Select;
             When DRVRel <> *BLANK                                            //release
                   and DRVSt <> *BLANK                                        //state
                   and DRVEffdt <> *zero;
               RelKey    = DRVRel;
               StateKey2 = DRVSt;
               // Convert date from 8,0 to 7,0.  Uses dtCYMD from SPIDATE
                dtCYMD8  = DRVEffdt;
                EffDtKey = dtCYMD(dtCYMD8);
               //*** Beg Add *** 05/19/16 *******************************
               // Check current rules dates against what we are loading
               exsr CheckDates;
               if newFrDt >= curFrDt;
               //*** End Add *** 05/19/16 *******************************
               // Delete all records from FDFRL10 with the key --> State, Effective date
               SetLL key1FDFRL3  PFR03;
               Reade key1FDFRL3  PFR03;
               Dow not(%eof);
                 Delete key1FDFRL3  PFR03;
                 Reade key1FDFRL3  PFR03;
               Enddo;

               // Read FDFDRLODL1 for key --> Release, State, Effective date and write to FDFRP
               SetLL keyFDFRPLODL1 PFR01L;
               Reade keyFDFRPLODL1 PFR01L;
               Dow not(%eof);
                 FRTime  = Timeto6(SystemTime());
                 FRDt    = Dateto7(SystemDate());
                 FRUser  = Q1USER;
                 FRPgm   = Q1PGM;
                 Write PFR01;
                 flgSuccess = 'Y';
                 Reade keyFDFRPLODL1 PFR01L;
               Enddo;
               //*** Beg Add *** 05/19/16 *******************************
               Endif;
               //*** End Add *** 05/19/16 *******************************

             // Release <> blank, State <> blank, no Effective Date
             When DRVRel <> *BLANK                                            //release
                   and DRVSt <> *BLANK                                        //state
                   and DRVEffdt = *zero;
               RelKey    = DRVRel;
               StateKey2 = DRVSt;
               EffDtKey  = *zero;
               //*** Beg Add *** 05/19/16 *******************************
               // Check current rules dates against what we are loading
               exsr CheckDates;
               if newFrDt >= curFrDt;
               //*** End Add *** 05/19/16 *******************************

               // Delete all records from FDFRL10 with the key --> State
               SetLL key2FDFRL3  PFR03;
               Reade key2FDFRL3  PFR03;
               DOW not(%eof);
                 Delete key2FDFRL3  PFR03;
                 Reade key2FDFRL3  PFR03;
               Enddo;

               // Read FDFRPLODL1 for key --> Release, State and write to FDFRP
               SetLL key1FDFRPLODL1 PFR01L;
               Reade key1FDFRPLODL1 PFR01L;
               Dow not(%eof);
                 FRTime  = Timeto6(SystemTime());
                 FRDt    = Dateto7(SystemDate());
                 FRUser  = Q1USER;
                 FRPgm   = Q1PGM;
                 Write PFR01;
                 flgSuccess = 'Y';
                 Reade key1FDFRPLODL1  PFR01L;
               Enddo;
               //*** Beg Add *** 05/19/16 *******************************
               Endif;
               //*** End Add *** 05/19/16 *******************************
             Endsl;

             // Update Fee Schedule Auto-Load Driver File
             If flgSuccess = 'Y';
               DRVProcess = 'Y';                                              //Processed
             Else;
               DRVProcess = 'N';                                              //No records written
             Endif;
             DRVTime  = Timeto6(SystemTime());
             DRVDt    = Dateto7(SystemDate());
             DRVUser  = Q1USER;
             DRVPgm   = Q1PGM;
             Update DR101;

           ENDIF;

         //===> Action = "A" - Add
         When DRVAction = 'A';
           If DRVRel = *BLANK                                                 //release
                 or DRVSt = *BLANK;                                           //state
             DRVProcess = 'E';                                                //Error
             DRVTime  = Timeto6(SystemTime());
             DRVDt    = Dateto7(SystemDate());
             DRVUser  = Q1USER;
             DRVPgm   = Q1PGM;
             Update DR101;
           Else;

             //     Set up key list
             Select;
             When DRVRel <> *BLANK                                            //release
                   and DRVSt <> *BLANK                                        //state
                   and DRVEffdt <> *zero;
               RelKey    = DRVRel;
               StateKey2 = DRVSt;
               // Convert date from 8,0 to 7,0.  Uses dtCYMD from SPIDATE
                dtCYMD8  = DRVEffdt;
                EffDtKey = dtCYMD(dtCYMD8);
               // Read FDFRPLODL1 for key --> Release, State, Effective date and write to FDFRP
               SetLL keyFDFRPLODL1 PFR01L;
               Reade keyFDFRPLODL1 PFR01L;
               Dow not(%eof);
                 FRTime  = Timeto6(SystemTime());
                 FRDt    = Dateto7(SystemDate());
                 FRUser  = Q1USER;
                 FRPgm   = Q1PGM;
                 Write PFR01;
                 flgSuccess = 'Y';
                 Reade keyFDFRPLODL1 PFR01L;
               Enddo;

             When DRVRel <> *BLANK
                   and DRVSt <> *BLANK
                   and DRVEffdt = *zero;
               RelKey    = DRVRel;
               StateKey2 = DRVSt;
               EffDtKey  = *zero;

               // Read FDFRPLODL1 for key --> Release, State, Effective date and write to FDFRP
               SetLL key1FDFRPLODL1 PFR01L;
               Reade key1FDFRPLODL1 PFR01L;
               DOW not(%eof);
                 FRTime  = Timeto6(SystemTime());
                 FRDt    = Dateto7(SystemDate());
                 FRUser  = Q1USER;
                 FRPgm   = Q1PGM;
                 Write PFR01;
                 flgSuccess = 'Y';
                 Reade key1FDFRPLODL1 PFR01L;
               Enddo;
             Endsl;

             // Update Fee Schedule Auto-Load Driver File
             If flgSuccess = 'Y';
               DRVProcess = 'Y';                                              //Processed
             Else;
               DRVProcess = 'N';                                              //No records written
             Endif;
                 DRVTime  = Timeto6(SystemTime());
                 DRVDt    = Dateto7(SystemDate());
                 DRVUser  = Q1USER;
                 DRVPgm   = Q1PGM;
             Update DR101;

           Endif;
         Endsl;

       ENDSR;


       //=============================================================================
       // LoadMessages Subroutine:  Loads FTMIP - Bill Entry Message ID Table file   *
       //                           Replace or Add                                   *
       //=============================================================================
       BEGSR LoadMessages;

         //===> Action = "R" - Replace
         Select;
         When DRVAction = 'R';
           If DRVRel = *BLANK                                                 //release
                 or DRVSt = *BLANK;                                           //state
             DRVProcess = 'E';                                                //Error
             DRVTime  = Timeto6(SystemTime());
             DRVDt    = Dateto7(SystemDate());
             DRVUser  = Q1USER;
             DRVPgm   = Q1PGM;
             Update DR101;
           Else;

             If DRVRel <> *BLANK
                   and DRVSt <> *BLANK;

               // Delete all records from FTMIP with rlease, where substr(MIMSG#,1,2) = state.
               SetLL keyFTMIL2 PMI02;
               Reade keyFTMIL2 PMI02;
               Dow not(%eof);
                 If %subst(MIMSG#:01:02) = DRVSt;
                   Delete PMI02;
                 Endif;
                 Reade keyFTMIL2 PMI02;
               Enddo;

               // Read FTMIPLODL1 for key --> Release, State and write to FTMIP
               SetLL keyFTMIPLODL1 PMI01L;
               Reade keyFTMIPLODL1 PMI01L;
               Dow not(%eof);
                 If %subst(MIMSG#:01:02) = DRVSt;
                 MITime  = Timeto6(SystemTime());
                 MIDt    = Dateto7(SystemDate());
                 MIUser  = Q1USER;
                 MIPgm   = Q1PGM;
                   Write PMI01;
                   flgSuccess = 'Y';
                 Endif;
                 Reade keyFTMIPLODL1 PMI01L;
               Enddo;

               // Update Fee Schedule Auto-Load Driver File
               If flgSuccess = 'Y';
                 DRVProcess = 'Y';                                            //Processed
               Else;
                 DRVProcess = 'N';                                            //No records written
               Endif;
               DRVTime  = Timeto6(SystemTime());
               DRVDt    = Dateto7(SystemDate());
               DRVUser  = Q1USER;
               DRVPgm   = Q1PGM;
               Update DR101;

             Endif;
           Endif;

         //===> Action = "A" - Add
         When DRVAction = 'A';
           If DRVRel = *BLANK                                                 //release
                 or DRVSt = *BLANK;                                           //state
             DRVProcess = 'E';                                                //Error
             DRVTime  = Timeto6(SystemTime());
             DRVDt    = Dateto7(SystemDate());
             DRVUser  = Q1USER;
             DRVPgm   = Q1PGM;
             Update DR101;
           Else;

             If DRVRel <> *BLANK
                   and DRVSt <> *BLANK;

               // Read FTMIPLODL1 for key --> Release, State and write to FTMIP
               SetLL keyFTMIPLODL1 PMI01L;
               Reade keyFTMIPLODL1 PMI01L;
               Dow not(%eof);
                 IF %subst(MIMSG#:01:02) = DRVSt;
                 MITime  = Timeto6(SystemTime());
                 MIDt    = Dateto7(SystemDate());
                 MIUser  = Q1USER;
                 MIPgm   = Q1PGM;
                   Write PMI01;
                   flgSuccess = 'Y';
                 Endif;
                 Reade keyFTMIPLODL1 PMI01L;
               Enddo;

             Endif;
           Endif;

           // Update Fee Schedule Auto-Load Driver File
           If flgSuccess = 'Y';
             DRVProcess = 'Y';                                                //Processed
           Else;
             DRVProcess = 'N';                                                //No records written
           Endif;
             DRVTime  = Timeto6(SystemTime());
             DRVDt    = Dateto7(SystemDate());
             DRVUser  = Q1USER;
             DRVPgm   = Q1PGM;
           Update DR101;
         Endsl;

       Endsr;

       //*************************************************************************
       //   Program Initialization - *INZSR
       //*************************************************************************
       Begsr *INZSR;

         StrRls = @@StrRls;
         EndRls = @@EndRls;

       Endsr;
      /END-FREE
