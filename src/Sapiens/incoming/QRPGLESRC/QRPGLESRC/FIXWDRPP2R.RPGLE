     H******************************************************************
     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*CALLER)
     H******************************************************************
     H*/TITLE FIXWDRPP2R - Update Payee information in WDRPP           *
     H*                    Same as FIXWDRPP2 in EBF rel V14R7MR (MHCA) *
     H******************************************************************
     H*  DESCRIPTION: This program will update the payee information   *
     H*               name and address in WDRPP.                       *
     H*                                                                *
     H*  CREATED: 07/23/15  CONSULTANT: C:180  Support: 35755          *
     H******************************************************************
     H*                                                                *
     H* REVISIONS:                                                     *
     H*                                                                *
     H******************************************************************
      *
      * Claims Check payment file
     FWDPAP     IF   E             DISK
     FWDPALL    IF   E           K DISK    RENAME(PPA01:PPA01L) Prefix(L)
      *
      * Provider Demographic File
     FWMPDL     IF   E           K DISK
      *
      * Claims Demographic File
     FWMCDL     IF   E           K DISK
      *
      * Claim Master File
     FWMCML1    IF   E           K DISK
      *
      * Claims checks view file
     FWDRPL3    UF   E           K DISK
     FWDRPL     IF   E           K DISK    RENAME(PRP01:PRP01L) Prefix(L)
      *
      * Report
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
      *
      *
     D FixWdrpp2r      PR
     D    @Prt                        1A   Const
     D    @Upd                        1A   Const
      *
     D FixWdrpp2r      PI
     D    @Prt                        1A   Const
     D    @Upd                        1A   Const
      *
      * Data Structures
      *
     D PDDS          ESDS                  EXTNAME(SZQ1P)
      *
     D Case#           DS
     D  pafnd
     D    dash1                       1    inz('-')
     D  pafyr
     D    dash2                       1    inz('-')
     D  pacase
      *
      * Stand Alone
      *
     D Payee           S                   LIKE(PDNAME) INZ
     D Adr1            S                   LIKE(PDADR1) INZ
     D Adr2            S                   LIKE(PDADR2) INZ
     D City            S                   LIKE(PDCTY)  INZ
     D State           S                   LIKE(PDST)   INZ
     D Zip             S                   LIKE(PDZIP)  INZ
     D Adr3            S                   LIKE(PDADR3) INZ
     D Country         S                   LIKE(PDCTRY) INZ
     D Remarks         S                   LIKE(RPRMDS) INZ
     D XXFEIN          S                   LIKE(CMSS#)  INZ
     D @Err            S             40    INZ
     D XXZIP           S              9    INZ
     D XXCSZ           S             40    INZ
     D NADR            S             40    DIM(6) INZ
     D OADR            S             40    DIM(6) INZ
     D IW              S              3  0 INZ
     D IX              S              3  0 INZ
     D IY              S              3  0 INZ
     D IZ              S              3  0 INZ
     D Mode            S              9    INZ
     D Time6           S               T   INZ
     D LinCnt          S              4  0 INZ
     D xxtrdt          S               D   DATFMT(*USA)
     D Cnt1            S              7  0 INZ
     D Cnt2            S              7  0 INZ
     D Cnt3            S              7  0 INZ
     D Cnt4            S              7  0 INZ
      *
      * Constants
      *
     D @Err1           C                   CONST('FEIN demographic missing.')
     D @Err2           C                   CONST('SSN demographic missing. ')
     D @Err3a          C                   CONST('SEQ# ')
     D @Err3b          C                   CONST(' not found in chk view.')
     D @Err4           C                   CONST('Trans date mismatch.')
     D @Err5           C                   CONST('Check remarks not found.')
      *
      *
      *
       //==================================================================
       // MAIN
       //==================================================================
      /Free

       // Cycle through Payment File

         Read wdpap;
           dow not %EOF;

         // Identify potential records with issue

            If patype = 'A'    and pacase <> paxcas and paipgm = 'W0120R' and
               paamt   > *zero and pachk# > *zero    and pacase > *zero and
               paxcas > *zero;
               *in90= *OFF;
               Cnt1 = Cnt1 + 1;
               Chain(N) (PACo#:PAFnd:PAFYr:PACase:PASeq#) WDRPL3;
               If %found(WDRPL3);

                  If PAtrdt = RPTRDT;
                     exsr GetPayeeInfo;

                     If (RPPNAM <> Payee and Payee > *blanks and
                         RPPNAM > *blanks) or PAchk# <> RPCHK#;
                        Cnt2 = Cnt2 + 1;
                        Exsr GetCheckRemarks;
                        Exsr Print;
                        Exsr UpdWDRPP;
                        Exsr Cleardata;
                     Endif;

                  Else;
                    @err = @err4;
                    *in90 = *on;
                    Exsr Print;
                  Endif;

               Else;
                 @err = @err3a + %char(PASEQ#) + @err3b;
                 *in90= *on;
                 Exsr Print;
               Endif;

            EndIf;
             Read wdpap;
           EndDo;

           If *in77;
            *in79 = *on;
            Exsr Print;
           Endif;

           *inlr = *on;
       //===================================================================
       // GetPayeeInfo - Get payee's information
       //===================================================================
       BegSr GetPayeeInfo;

        Chain (PACO#:PAFND:PAFYR:PACASE) WMCML1;
        XXFEIN = %EDITC(PAFEIN:'X');

        If PAFEIN > *ZERO and XXFEIN <> CMSS#;
           Chain (PAFein:PAMod) WMPDL;
           If %found(WMPDL);
              If PDDBA1 = 'Y';
                 Payee = PDADR1;
                 Adr1  = pdadr2;
                 Adr2  = *blanks;
              Else;
                 Payee = PDNAME;
                 If PDFSN <> *blanks;
                    Payee = %trim(Payee) + ', ' + %trim(PDFSN);
                 EndIf;
                 Adr1 = PDADR1;
                 Adr2 = PDADR2;
              EndIf;

              City  = PDCTY;
              State = PDST;
              Zip   = PDZIP;

              If PDCTRY <> *blanks and PDCTRY <> 'USA';
                 Country = PDCTRY;
                 Adr3    = PDADR3;
              Endif;
           Else;
             @err = @err1;
             *in90 = *on;
           Endif;

        Else;
           If %found(WMCML1);
              Chain (CMSS#) WMCDL;
              If %found(WMCDL);
                 Payee = %trim(CDFSN) + ' ' + %trim(CDMID) + ' ' + %trim(CDLSN);
                 Adr1  = CDADR1;
                 Adr2  = CDADR2;
                 City  = CDCTY;
                 State = CDST;
                 Zip   = CDZIP;
                 If CDCTRY <> *blanks and CDCTRY <> 'USA';
                    Country = CDCTRY;
                    Adr3    = CDADR3;
                 Endif;
              Endif;
           Else;
             @err = @err2;
             *in90 = *on;
           Endif;

        Endif;


       EndSr;
       //===================================================================
       // Print - Print report
       //===================================================================
       Begsr Print;
        If *in77 and *in79 = *OFF;

        // Check for overflow
          If *INOF or Lincnt > 53;
              Except HDG;
              Lincnt = 3;
          Endif;


          xxtrdt = %date(PATRDT:*cymd);


         // Verify check #

          If PAchk# = RPCHK#;
             *IN67 =  *OFF;
          Else;
             *IN67 = *ON;
          Endif;

          Except DET1;
          Lincnt = Lincnt + 2;

          If *in90 = *off;

        // Old payee address information

             XXZIP = %EDITC(RPZip:'X');
             XXZIP = %SUBST(XXZIP:1:5) + '-' + %SUBST(XXZIP:6:3);
             XXCSZ = %trim(RPCTY) + ', ' + %trim(RPST) + ' ' + XXZIP;

             If RPAdr1 > *blanks;
                IY = IY + 1;
                OADR(IY)= RPAdr1;
             Endif;

             If RPAdr2 > *blanks;
                IY = IY + 1;
                OADR(IY) = RPAdr2;
             Endif;

             If XXCSZ > *blanks;
                IY = IY + 1;
                OADR(IY) = XXCSZ;
             Endif;

             If RPAdr3 > *blanks;
                IY = IY + 1;
                OADR(IY) = RPAdr3;
             Endif;

             If RPCTRY > *blanks;
                IY = IY + 1;
                OADR(IY) = RPCTRY;
             Endif;


             IY = IY + 1;
             OADR(IY) = RPRMDS;

             // New payee information

             Reset XXZIP;
             Reset XXCSZ;
             XXZIP = %EDITC(Zip:'X');
             XXZIP = %SUBST(XXZIP:1:5) + '-' + %SUBST(XXZIP:6:3);
             XXCSZ = %trim(City) + ', ' + %trim(State) + ' ' + XXZIP;

             If Adr1 > *blanks;
                IX = IX + 1;
                NADR(IX)= Adr1;
             Endif;

             If Adr2 > *blanks;
                IX = IX + 1;
                NADR(IX) = Adr2;
             Endif;

             If XXCSZ > *blanks;
                IX = IX + 1;
                NADR(IX) = XXCSZ;
             Endif;

             If Adr3 > *blanks;
                IX = IX + 1;
                NADR(IX) = Adr3;
             Endif;

             If Country > *blanks;
                IX = IX + 1;
                NADR(IX) = Country;
             Endif;

                IX = IX + 1;
                NADR(IX) = Remarks;

             If IX > IY;
                IZ = IX;
             Else;
                IZ = IY;
             Endif;

             For IW = 1 to IZ;
                 Except DET2;
                 Lincnt = Lincnt + 1;
             Endfor;



           Else;
             Except ERR1;
             Lincnt = Lincnt + 1;
             Cnt4 = Cnt4 + 1;
           Endif;


        Endif;

        // Final totals
        If *in77 and *in79;

          If *INOF or Lincnt > 53;
              Except HDG;
          Endif;

          Except TOT1;
        Endif;

       Endsr;
       //===================================================================
       // GetCheckRemarks - Get check remarks
       //===================================================================
       Begsr GetCheckRemarks;
        Chain (PAco#:PAfnd:PAxfyr:PAxcas:PAchk#:PAtrdt) WDPALL;
        If %found(WDPALL) and Lpaamt = (PAamt * -1);
            Chain (Lpaco#:Lpafnd:Lpafyr:Lpacase:Lpachk#:Lpatrdt:Lpaseq#) WDRPL;
            If %found(WDRPL);
               Remarks = Lrprmds;
            Else;
               *in90= *on;
               @err = @err5;
            Endif;
        Else;
          *in90= *on;
          @err = @err5;
        Endif;
       Endsr;
       //===================================================================
       // UpdWDRPP - Update check view file
       //===================================================================
       BegSr UpdWDRPP;
        If *IN78 and *IN90 = *OFF;
           Chain (PACo#:PAFnd:PAFYr:PACase:PASeq#) WDRPL3;
           If %found(WDRPL3) and PATRDT = RPTRDT;

              If RPPNAM <> Payee and Payee > *blanks and RPPNAM > *blanks;
                 RPPnam = Payee;
                 RPAdr1 = Adr1;
                 RPAdr2 = Adr2;
                 RPCty  = City;
                 RPSt   = State;
                 RPZip  = Zip;
                 RPCtry = Country;
                 RPAdr3 = Adr3;
              Endif;

              If PAchk# <> RPCHK#;
                 RPCHK# = PAchk#;
              Endif;

              Update PRP01;
              Cnt3 = Cnt3 + 1;
           Endif;
        Endif;
       EndSr;

       //===================================================================
       // ClearData - Clear work data
       //===================================================================
       Begsr ClearData;
          Clear Payee;
          Clear Adr1;
          Clear Adr2;
          Clear Adr3;
          Clear City;
          Clear State;
          Clear Zip;
          Clear Country;
          Clear XXZIP;
          Clear XXCSZ;
          Reset NADR;
          Reset OADR;
          Clear IX;
          Clear IY;
          Clear @err;
          Clear XXFEIN;
       Endsr;
       //===================================================================
       // Init - Initialization
       //===================================================================
        Begsr *INZSR;
        *in77 = @prt;
        *in78 = @upd;
        If *in78;
         Mode = ' - Update';
        Endif;
        Time6 = %time();
        If *in77;
          Except HDG;
          LinCnt = LinCnt + 3;
        Endif;
       Endsr;
      /End-Free

       //===================================================================
       // Report -
       //===================================================================
     OQSYSPRT   E            HDG           02 01
     O                                              'PGM:FIXWDRPP2R'
     O                       UDATE         Y  +0001
     O                       TIME6            +0001
     O                                           76 'FIX PAYEE INFORMATION'
     O               78      Mode
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HDG           01
     O                                              'CASE NUMBER'
     O                                           23 'CHECK #'
     O                                           34 'TRN DATE'
     O                                           60 'NEW PAYEE NAME/ADDRESS/'
     O                                        +0000 'REMARKS'
     O                                          102 'OLD PAYEE NAME/ADDRESS/'
     O                                        +0000 'REMARKS'
     O                                          131 'OLD CHECK #'
     O          E            DET1       01 01
     O                       CASE#
     O                       PACHK#           +0001
     O                       XXTRDT              35
     O                       Payee               77
     O                       RPPNAM             119
     O               67      RPCHK#             129
     O               67                         132 '*'
     O          E            DET2          01
     O                       NADR(IW)            77
     O                       OADR(IW)           119
     O          E            ERR1          02
     O                                              'ERROR:'
     O                       @err             +0001
     O               67                       +0001 'Pay file date:'
     O               67      PAtrdt           +0001 ' -  -  -  '
     O               67                       +0005 'Chk View file date:'
     O               67      RPTRDT           +0001 ' -  -  -  '
     O          E            TOT1       02 01
     O                                              'Total Records Processed:'
     O                       Cnt1          1  +0001
     O              N78                          52 'To Be'
     O                                           59 'Fixed:'
     O              N78      Cnt2          1     69
     O               78      Cnt3          1     69
     O                                           88 'In Error:'
     O                       Cnt4          1  +0001
