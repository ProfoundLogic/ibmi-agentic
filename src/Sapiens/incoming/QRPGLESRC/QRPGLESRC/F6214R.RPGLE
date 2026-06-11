     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      //****************************************************************
      // StoneRiver is a trademark of StoneRiver, Inc.
      //****************************************************************
      //
     H/TITLE F6214R - EDI 837 EXTRACT PROCESS 2
      //
      //****************************************************************
      //  DESCRIPTION:  This pgm processes all records from the log    *
      //    history file which are due for extraction.  First, the     *
      //    EDI data elements are edited, then they are printed to     *
      //    1 of 2 reports, the EDI 837 Excepted report or the 837     *
      //    Error Rejection report.  If the transaction passes all     *
      //    edits, it is written to the 837 extract files, then the    *
      //    log history file is updated.                               *
      //                                                               *
      //  CALLED FROM:  F6201C                                         *
      //  CALLS TO:     F6202R - DATA ELEMENT EDIT PROGRAM             *
      //                                                               *
      //  CREATED:  06/11/14     C:296     Project:  MC376             *
      //****************************************************************
      //  REVISIONS:                                                   *
      //                                                               *
      //****************************************************************
      //  INDICATOR USAGE:                                             *
      //                                                               *
      //****************************************************************
     FSMCOL     IF   E           K DISK
     FFMC1L     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FFEEXL     IF   E           K DISK
     FFELGL1    UF   E           K DISK
     FFELGL6    IF   E           K DISK
     F                                     RENAME(PLG01:PLG06)
     FFEMEL     UF   E           K DISK
     FFEE1L1    O    E           K DISK
     FFAEOLG    IF   E           K DISK
     FFAEAL1    IF   E           K DISK
     FFAEEL     IF   E           K DISK
     FFAEDL     IF   E           K DISK
     FFAEHL     IF   E           K DISK
     FFAEPL     IF   E           K DISK
     FFAEUL     IF   E           K DISK
     FFAESL2    IF   E           K DISK
     FFDCAL13   IF   E           K DISK
     FFDDDL     IF   E           K DISK
     FFDDEL3    IF   E           K DISK
     FFDDHL     IF   E           K DISK
     FFDDPL     IF   E           K DISK
     FFDDUL     IF   E           K DISK
     FFEDDL     O    E           K DISK
     F                                     RENAME(PDD01:PDD02)
     FFEDHL     O    E           K DISK
     F                                     RENAME(PDH01:PDH02)
     FFEDPL     O    E           K DISK
     F                                     RENAME(PDP01:PDP02)
     FFEDUL     O    E           K DISK
     F                                     RENAME(PDU01:PDU02)
     FFEEDL     O    E           K DISK
     F                                     RENAME(PED01:PED02)
     FFEEHL     O    E           K DISK
     F                                     RENAME(PEH01:PEH02)
     FFEEPL     O    E           K DISK
     F                                     RENAME(PEP01:PEP02)
     FFEEUL     O    E           K DISK
     F                                     RENAME(PEU01:PEU02)
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMEML     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMP1L     IF   E           K DISK
     FFMDIL     IF   E           K DISK
     FFTABL     IF   E           K DISK    USROPN
     FFTEBLTX   IF   E           K DISK
     F                                     RENAME(PEB01:PEB02)
     FFTEBLNC   IF   E           K DISK
     F                                     RENAME(PEB01:PEB03)
     FFTDNL     IF   E           K DISK
     FFTERL     IF   E           K DISK
     FFXXTL     IF A E           K DISK
     FSDCMLA    UF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FFMFNL     IF   E           K DISK
     FFDF2L     IF   E           K DISK
     FFTGTL     IF   E           K DISK
     FNMNWL     IF   E           K DISK
     FFDDAL2    IF   E           K DISK
     FFEBLP     IF A E           K DISK
     FFEBLL1    IF   E           K DISK
     F                                     RENAME(PBL01:PBL02)
     FFEPRP     IF A E           K DISK
     FFEPRL1    IF   E           K DISK
     F                                     RENAME(PPR01:PPR02)
     FFECOML1   IF A E           K DISK
     F                                     RENAME(PCO01:PCO02) Prefix(CO_)
     F*
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOB)
     FQSYSPRT2  O    F  198        PRINTER OFLIND(*INOC)

      //________________________________________________________________
      //     TABLES and ARRAYS
     D DN              S             30    DIM(999)
     D SQ              S              5  0 DIM(6)
     D QCA             S             56    DIM(1) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
      //
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  GC                     1     20
     D                                     DIM(10)
     D  LGBAG1                 1      2
     D  LGBAG2                 3      4
     D  LGBAG3                 5      6
     D  LGBAG4                 7      8
     D  LGBAG5                 9     10
     D  LGBAG6                11     12
     D  LGBAG7                13     14
     D  LGBAG8                15     16
     D  LGBAG9                17     18
     D  LGBAG0                19     20
     D                 DS
     D  RC                     1     30
     D                                     DIM(10)
     D  LGBAR1                 1      3
     D  LGBAR2                 4      6
     D  LGBAR3                 7      9
     D  LGBAR4                10     12
     D  LGBAR5                13     15
     D  LGBAR6                16     18
     D  LGBAR7                19     21
     D  LGBAR8                22     24
     D  LGBAR9                25     27
     D  LGBAR0                28     30
     D                 DS
     D  EOB                    1     30
     D                                     DIM(10)
     D  XXEOB1                 1      3
     D  XXEOB2                 4      6
     D  XXEOB3                 7      9
     D  XXEOB4                10     12
     D  XXEOB5                13     15
     D  XXEOB6                16     18
     D  XXEOB7                19     21
     D  XXEOB8                22     24
     D  XXEOB9                25     27
     D  XXEO10                28     30
     D                 DS
     D  @@CMD                  1     56
     D  XXST                  38     39
     D                 DS
     D  EOBKEY                 1     40
     D  CO#                    1      3
     D  SUB#                   4      8
     D  FND                    9     11
     D  FYR                   12     14
     D  CASE                  15     21
     D  EOB#                  22     30
      //
      //
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D xcACLC          S             21
     D xcSUB           S              5
     D xcFND           S              3
     D xcFYR           S              3
     D xcCASE          S              7
     D*
     D xxWK16          S             16
     D xxCnam          S             30
     D xxCo#           S              3  0
     D xxjSt           S              2
     D xxtSet          S              3
     D xxDt7           S              7  0
     D xxPos           S              2
     D xxWcPd          S              8
     D*
     D ##WCIF          S              1
     D ##MCIF          S              1
     D ##WCIN          S              1
     D ##AR            S              1
     D ##NTWK          S              1
     D ##UR            S              1
     D ##MED           S              1
     D ##EDI           S              1
     D ##WCDK          S              1
     D ##RTCD          S              1
     D ##KEY           S             40
     D ##PNAM          S             10
     D ##COMM          S            256
     D ##CDAT          S              8
     D ##DCID          S             12
     D ##SECL          S              3
     D ##USER          S             10
     D ##PGM           S             10
     D ##FOLD          S             80
     D ##DIFL          S              1
     D ##IPGM          S             10
     D ##FL01          S              1
     D ##FL02          S              1
     D ##RMCD          S              5
     D ##SEQ#          S              5
     D*
     D P               S              5  0
     D LGMtc           S              2
     D Length          S             15  5
     D SRDATE          S              8  0
     D SRDAYS          S              5  0
     D srErr           S              1
     D*
     D qqErrs          S             30
     D qqFnd           S              3
     D qqTrDt          S              8
     D qqCmDt          S              8
     D*
     D #BTYPE          S              1
     D #BRND1          S             80
     D #BRND2          S             80
     D #CLTID          S              3
     D #1CO#           S              3
     D #1SUB#          S              5
     D #1FND           S              3
     D #1FYR           S              3
     D #1CASE          S              7
     D x1ExDt          S              8  0

     D***********************************************************
     DDataElemEdit     pr                  ExtPgm('F6202R')
     D xxCo#                          3  0
     D xxjSt                          2
     D P                              5  0
     D xxtSet                         3
     D LGMtc                          2
     D DN                            30    DIM(999)
     D qqErrs                        30
     D***********************************************************

     D***********************************************************
     DSrCvtDAt         pr                   ExtPgm('SRCVTDAT')
     D srDti                          8  0
     D srFfmt                         8
     D srTfmt                         8
     D srDto                         10
     D srErr                          1
     D***********************************************************

     D***********************************************************
     DSrCvtDat1        pr                  ExtPgm('SRCVTDAT')
     D srDti                          8  0
     D srFfmt                         8
     D srTfmt                         8
     D srDto                         10
     D srErr                          1
     D***********************************************************

     D***********************************************************
     DSRcvtDAt2        pr                  ExtPgm('SRCVTDAT')
     D srDti                          8  0
     D srFfmt                         8
     D srTfmt                         8
     D srDto                         10
     D srErr                          1
     D***********************************************************

     D***********************************************************
     DSrCvtDat3        pr                  ExtPgm('SRCVTDAT')
     D srDti                          8  0
     D srFfmt                         8
     D srTfmt                         8
     D srDto                         10
     D srErr                          1
     D***********************************************************
     D***********************************************************
     DRtvSysVals       pr                  ExtPgm('SA001R')
     D ##WCIF                         1
     D ##MCIF                         1
     D ##WCIN                         1
     D ##AR                           1
     D ##NTWK                         1
     D ##UR                           1
     D ##MED                          1
     D ##EDI                          1
     D ##WCDK                         1
     D***********************************************************

     D***********************************************************
     DRtvCompInfo      pr                  Extpgm('RTVBRND')
     D #BTYPE                         1
     D #BRND1                        80
     D #BRND2                        80
     D***********************************************************

     D***********************************************************
     DQCmdExc          pr                  Extpgm('QCMDEXC')
     D @@CMD                         56
     D Length                        15  5
     D***********************************************************

     D***********************************************************
     DRtvCltId         pr                  Extpgm('RTVCLTID')
     D #CLTID                         3
     D***********************************************************

     D***********************************************************
     DGetAgtInfo       pr                  Extpgm('WA050R')
     D qqCo#                          3
     D qqFnd                          3
     D qqTrDt                         8
     D qqCmDt                         8
     D***********************************************************

     D***********************************************************
     DBldDartKey       pr                  Extpgm('FA703R')
     D ##RTCD                         1
     D ##WCIN                         1
     D ##WCDK                         1
     D #1CO#                          3
     D #1SUB#                         5
     D #1FND                          3
     D #1FYR                          3
     D #1CASE                         7
     D ##KEY                         40
     D ##PNAM                        10
     D***********************************************************

     D***********************************************************
     DGetFutDat        pr                  Extpgm('SRFUTR8')
     D SRDATE                         8  0
     D SRDAYS                         5  0
     D srErr                          1
     D***********************************************************

     D***********************************************************
     DWrtDartRmk       pr                  Extpgm('DA700R')
     D ##KEY                         40
     D ##COMM                       256
     D ##CDAT                         8
     D ##DCID                        12
     D ##PNAM                        10
     D ##SECL                         3
     D ##USER                        10
     D ##PGM                         10
     D ##FOLD                        80
     D ##DIFL                         1
     D ##IPGM                        10
     D ##FL01                         1
     D ##FL02                         1
     D ##RMCD                         5
     D ##SEQ#                         5
     D***********************************************************
     IPEH01
     I              EHDIA1                      EHDI1
     I              EHDIA2                      EHDI2
     I              EHDIA3                      EHDI3
     I              EHDIA4                      EHDI4
      //
     IPED01
     I              EDDIA1                      EDDI1
     I              EDDIA2                      EDDI2
     I              EDDIA3                      EDDI3
     I              EDDIA4                      EDDI4
     IPEU01
     I              EUDIA1                      EUDI1
     I              EUDIA2                      EUDI2
     I              EUDIA3                      EUDI3
     I              EUDIA4                      EUDI4
     I              EUDIA5                      EUDI5
     I              EUDIA6                      EUDI6
     I              EUDIA7                      EUDI7
     I              EUDIA8                      EUDI8
     I              EUDIA9                      EUDI9
     I              EUCTCD                      EUCTC2

      //================================================================
      //   ENTRY PARAMETERS (ALL ALPHANUMERIC)
      //
     C     *ENTRY        Plist
     C                   Parm                    qqCo#             3
     C                   Parm                    qqjSt             2
     C                   Parm                    qqExDt            8
     C                   Parm                    qqTSet            3
      /Free
        //Checking CO# field not to be blank.
        If qqCo# <> *blanks;
          xxCo# = %Dec(qqCo#:3:0);
        Else;
          xxCo# = 0;
        Endif;

        xxjSt = qqjSt;

        If qqExDt <> *blanks;
          x1ExDt = %Dec(qqExDt:8:0) ;
        Else;
          x1ExDt = 0;
        Endif;

        srDti  = x1ExDt;
        srFfmt = *blanks;
        srTfmt = '*CYMD';
          SrCvtDAt(srDti:srFfmt:srTfmt:srDto:srErr);

        If srDto <> *blanks;
          xxExdt = %Dec(srDto:8:0);
        Else;
          xxExdt = 0;
        Endif;
        //*********************************************************
        //  Process initialization subroutine
        //*********************************************************
        Exsr SrInit;

        //*********************************************************
        //  Call subroutine to load the extract workfile.
        //*********************************************************
        Exsr LdWrkF;

         //----------------------------------------------------------------
         // MAINLINE OF PROGRAM STARTS.
         //----------------------------------------------------------------
         Chain xtKY PXT01;
         Dow not %eof(FXXTL) ;

         //******************************************************
         //  GET THE EDI LOG HISTORY RECORD
         //******************************************************
         Chain lgKY PLG01;
           If %found(FELGL1) ;

             //**************************************************
             //  Load the Data Element Array
             //**************************************************
             Clear DN;
             Exsr GetDb;
             Exsr MvDbAr;

             //**************************************************
             //  Run the editor for each data element
             //**************************************************
             xxErr = 'N' ;
             Chain dnKY PDN01;
B02          Dow not %eof (FTDNL) ;
B03            IF DNSTAT = 'A';
                 P = dnDN# ;
                 Clear qqErrs;

               DataElemEdit(xxCo#:xxjSt:P:xxtSet:LGMtc:DN:qqErrs);

             //**********************************************************
             //   CHECK EACH DATA ELEMENT FOR ERRORS
             //**********************************************************
             x = 1;
             xxErrs = %Subst(qqErrs:X:3);
B04            Dow xxErrS <> *blanks;

B05               If xxErr = 'N';
                    Exsr ErrDry;
E05               Endif;

                  Exsr RpDet1;
                  Exsr ErrFil;
                     xxErr = 'Y';
                     x += 3;
                        xxErrS =%subST(qqErrs:X:3);

E04            Enddo;

E03            Endif;
                  Reade dnky PDN01;
E02          Enddo;
         //**************************************************************
         //  If there were no errors for this transaction, extract the
         //  EOMB (update the log record, load the 837 extract files,
         //  complete the extract error diary if it exists, and print
         //  the EOMB on the detail report).
         //**************************************************************
B02          If xxErr = 'N';

               Exsr UpdLog;
               Exsr LD837;
               Exsr CmpDry;
               Exsr RptDet;
               xxACPT += 1;                                              //ACCEPTED COUNT

X02          Else;

               Exsr ErrDry;
                  xxREJ += 1;                                            //REJECTED COUNT
E02          Endif;
E02        Endif;                                                        //If found FELGL6

         //******************************************************
         //  PROCESS NEXT LOG RECORD
         //******************************************************
      /END-FREE
     C     SKIP          TAG
      /FREE
            Reade xtKY PXT01;
E011     Enddo;
         //***********************************************************
         //  PRINT NO RECORDS SELECTED MESSAGES IF NO DETAILS WRITTEN
         //***********************************************************

B01     If *in91 = '0';
          Except Head;
          Except Msg;
E01     Endif;
B01     If *in92 = '0';
          Except Head1;
          Except Msg1;
E01     Endif;
       //********************************************************
       //   PRINT THE SUMMARY REPORT
       //********************************************************

       xx837T = xxACPT + xxREJ;                                         //TOTAL 837 CNT

       Except Head2;
       Except Det2;
       //********************************************************
       //   UPDATE THE SUBMISSION MANAGEMENT RECORD
       //********************************************************

       xxtSet = '837';

       Chain meKy PME01;
         If %Found(FEMEL);
           ME837T = XX837T;
           ME837S = XXACPT;
           MEstat = *BLANKS;
           MEuser = Q1USER;
           MEpgm  = Q1PGM;
           DSdate = %Dec(%Date():*usa);
           DStime = %Dec(%Time():*hms);
           MEtime = DSTIME;
           MEdt   = xxDt7;

         Update PME01;

         Endif;
        //******************
        //  END OF PROGRAM *
        //******************
      /END-FREE
     C     TERM          TAG
      /FREE
         *inLR = '1';


        //----------------------------------------------------------------
        // SrInit - This subroutine is used to define all Klists and
        //          variables, and initialize certain fields.
        //----------------------------------------------------------------
          Begsr SrInit;
           //*************************************************************
           //  SMCOL - KEY LIST FOR THE COMPANY MASTER FILE
           //*************************************************************
      /End-Free
     C     COKY          Klist
     C                   Kfld                    xxCo#

     C     C1KY          Klist
     C                   Kfld                    xxCo#

        //*********************************************************
        //  SMORL - KEY LIST FOR THE OUTPUT ROUTING FILE
        //*********************************************************
     C     ORKY          Klist
     C                   Kfld                    Q1PGM
     C                   Kfld                    XXFILE

        //*********************************************************
        //  FMFNL - KEY LIST FOR THE FUND MASTER FILE
        //*********************************************************
     C     FNKY          Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    LGSUB#
     C                   Kfld                    LGFND

        //*********************************************************
        //  FDF2L - KEY LIST FOR THE WCMS FUND AUX FILE
        //*********************************************************
     C     F2KY          Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    LGSUB#
     C                   Kfld                    LGFND
     C                   Kfld                    LGJST

        //*********************************************************
        //  FEEXL - KEY LIST FOR THE EDI CONTROL FILE
        //*********************************************************
     C     EXKY          Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    xxjSt
     C                   Kfld                    xxtSet

        //*********************************************************
        //  SDCMP - KEY LIST FOR THE DARTS REMARK FILE
        //*********************************************************
     C     SDKEY         Klist
     C                   Kfld                    ##KEY
     C                   Kfld                    XXDSEQ

        //*********************************************************
        //  FEMEL - SYSTEM MANAGEMENT RECORDS
        //*********************************************************
     C     MEKY          Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    xxjSt
     C                   Kfld                    XXEXDT
     C                   Kfld                    xxtSet

        //*********************************************************
        //  FTERL - ERROR MESSAGE TABLE FILE
        //*********************************************************
     C     ERKY          Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    xxErr#

        //*********************************************************
        //  FTDNL - DATA ELEMENT TABLE FILE
        //*********************************************************
     C     DNKY          Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    xxjSt
     C                   Kfld                    xxtSet

        //*********************************************************
        //  FELGP - KEY LIST FOR THE EDI LOG FILE
        //*********************************************************
     C     LGKY          Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    XTSUB#
     C                   Kfld                    XTFND
     C                   Kfld                    XTEOB#
     C                   Kfld                    XTLSEQ

     C     L2KY          Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    xxjSt

        //*********************************************************
        //  FMCMP - KEY LIST FOR THE CLAIM MASTER FILE
        //*********************************************************
     C     CMKY          Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    LGSUB#
     C                   Kfld                    LGFND
     C                   Kfld                    LGFYR
     C                   Kfld                    LGCASE

        //*********************************************************
        //  FMCDP - KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
        //*********************************************************
     C     CDKY          Klist
     C                   Kfld                    CMSS#

        //*********************************************************
        //  FMDIP - KEY LIST FOR THE DIAGNOSIS CODE FILE
        //*********************************************************
     C     DIKY          Klist
     C                   Kfld                    XXICD9
     C                   Kfld                    XXLOST

        //*********************************************************
        //  FMEMP - KEY LIST FOR THE EMPLOYER MASTER FILE
        //*********************************************************
     C     EMKY          Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    CMSUB#
     C                   Kfld                    CMFND
     C                   Kfld                    CMEMP#
     C                   Kfld                    CMDIV

        //*********************************************************
        //  FXXTP - KEY LIST FOR THE EDI EXTRACT WORKFILE
        //*********************************************************
     C     XTKY          Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    xxtSet

        //*********************************************************
        //  FMPDP - KEY LIST FOR THE PAYEE DEMOGRAPHICS FILE
        //*********************************************************
     C     PDKY          Klist
     C                   Kfld                    EOFEIN
     C                   Kfld                    EOMOD

        //*********************************************************
        //  FTABL - Key list for the Table files
        //*********************************************************
     C     TBKY          Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    XXKEY

        //*********************************************************
        //  FTGTL - Key list for the Generic Table file
        //*********************************************************
     C     GTKY          Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    X1TBID
     C                   Kfld                    X1KEY

        //*********************************************************
        //  Generic key to acces EOMB Header files by recon#
        //*********************************************************
     C     #HKY          Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    LGSUB#
     C                   Kfld                    LGFND
     C                   Kfld                    LGFYR
     C                   Kfld                    LGCASE
     C                   Kfld                    LGEOB#

     C     #HKY1         Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    LGSUB#
     C                   Kfld                    LGFND
     C                   Kfld                    LGFYR
     C                   Kfld                    LGCASE
     C                   Kfld                    LGEOB#
     C                   Kfld                    LGRCN

        //*********************************************************
        //  Generic key to access EOMB detail files by detail seq#
        //*********************************************************
     C     #DKY          Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    LGSUB#
     C                   Kfld                    LGFND
     C                   Kfld                    LGFYR
     C                   Kfld                    LGCASE
     C                   Kfld                    LGEOB#
     C                   Kfld                    CADSEQ

        //*********************************************************
        //  Key to access Detail EOB Code/Note File
        //*********************************************************
     C     #DEKY         Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    LGSUB#
     C                   Kfld                    LGFND
     C                   Kfld                    LGFYR
     C                   Kfld                    LGCASE
     C                   Kfld                    LGEOB#
     C                   Kfld                    LGRCN
     C                   Kfld                    CADSEQ

        //*********************************************************
        //  Key to access Detail Amount File
        //*********************************************************
     C     #DAKY         Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    LGSUB#
     C                   Kfld                    LGFND
     C                   Kfld                    LGFYR
     C                   Kfld                    LGCASE
     C                   Kfld                    LGEOB#
     C                   Kfld                    CADSEQ
     C                   Kfld                    LGRCN

        //*********************************************************
        //  Key list to access the Network Master File
        //*********************************************************
     C     NWKY          Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    KKNWCD

        //*********************************************************
        //  Parm lists for date routines
        //*********************************************************
     C     DATCHK        Plist
     C                   Parm                    srDti             8 0
     C                   Parm      *BLANKS       srFfmt            8
     C                   Parm      *BLANKS       srTfmt            8
     C                   Parm                    srDto            10
     C                   Parm                    srErr             1

     C     DTMDYY        Plist
     C                   Parm                    srDti
     C                   Parm      '*CYMD'       srFfmt
     C                   Parm      *BLANKS       srTfmt
     C                   Parm                    srDto
     C                   Parm                    srErr

     C     DTCYM2        Plist
     C                   Parm                    srDti
     C                   Parm      '*YYMD'       srFfmt
     C                   Parm      '*CYMD'       srTfmt
     C                   Parm                    srDto
     C                   Parm                    srErr
      //
     C     DTMDY2        Plist
     C                   Parm                    srDti
     C                   Parm      '*YYMD'       srFfmt
     C                   Parm      '*MDYY'       srTfmt
     C                   Parm                    srDto
     C                   Parm                    srErr

     C     DTCYMD        Plist
     C                   Parm                    srDti
     C                   Parm      *BLANKS       srFfmt
     C                   Parm      '*CYMD'       srTfmt
     C                   Parm                    srDto
     C                   Parm                    srErr

     C     DTYYMD        Plist
     C                   Parm                    srDti
     C                   Parm      '*MDYY'       srFfmt
     C                   Parm      '*YYMD'       srTfmt
     C                   Parm                    srDto
     C                   Parm                    srErr

        //*********************************************************
        //  Define pgm variables.
        //*********************************************************
     C     *LIKE         DEFINE    LGCO#         xxCo#
     C     *LIKE         DEFINE    LGJST         xxjSt
     C     *LIKE         DEFINE    LGEXDT        XXEXDT
     C     *LIKE         DEFINE    LGTSET        xxtSet
     C     *LIKE         DEFINE    ORFIL#        XXFILE
     C     *LIKE         DEFINE    ORTITL        XXTITL
     C     *LIKE         DEFINE    ORTITL        X1TITL
     C     *LIKE         DEFINE    ORTITL        X2TITL
     C     *LIKE         DEFINE    ERMSG#        xxErr#
     C     *LIKE         DEFINE    ERDESC        XXEDSC
     C     *LIKE         DEFINE    DNDN#         XXDN#
     C     *LIKE         DEFINE    CMSEQ#        XXDSEQ
     C     *LIKE         DEFINE    CMSEQ#        SVSEQ#
     C     *LIKE         DEFINE    EBKEY         XXKEY
     C     *LIKE         DEFINE    DIICD9        XXICD9
     C     *LIKE         DEFINE    DILOST        XXLOST
     C     *LIKE         DEFINE    EXSEIN        XXSEIN
     C     *LIKE         DEFINE    EXSZP         XXSZP
     C     *LIKE         DEFINE    GTTBID        X1TBID
     C     *LIKE         DEFINE    GTKEY         X1KEY
     C     *LIKE         DEFINE    EOCHRG        TOTCHG
     C     *LIKE         DEFINE    NWNWCD        KKNWCD
     C     *LIKE         DEFINE    CAPAID        XXPAID

     C                   MOVE      *ZEROS        XXACPT            5 0
     C                   MOVE      *ZEROS        XXREJ             5 0
     C                   MOVE      *ZEROS        XX837T            5 0
     C                   MOVE      *ZEROS        XXDATE            8 0
     C                   MOVE      *ZEROS        XXACDT            8 0
     C                   MOVE      *ZEROS        XXFRDT            8 0
     C                   MOVE      *ZEROS        XXTODT            8 0
     C                   MOVE      *ZEROS        P                 5 0
     C                   MOVE      *ZEROS        X                 5 0
     C                   MOVE      *BLANKS       XXWK12           12
     C                   MOVE      *BLANKS       XXWK30           30
     C                   MOVE      *BLANKS       XXDNDS           40
     C                   MOVE      *BLANKS       qqErrs           30
     C                   MOVE      *BLANKS       xxErrS            3
     C                   MOVE      *BLANKS       xxErr             1
     C                   MOVE      *BLANKS       XXEUSR           10
     C                   MOVE      *BLANKS       X1EUSR           10
     C                   MOVE      'D'           XXLOST

      /Free
        //DSDTTM = %Dec(%Date():*usa) + %Char(%Time():*hms));
        DSDATE = %Dec(%Date():*USA);
        DSTIME = %Dec(%Time():*hms);
        srDti = DSDATE;
        srFfmt = *blanks;
        srTfmt = '*CYMD';
           SrCvtDAt(srDti:srFfmt:srTfmt:srDto:srErr);
              If srDto <> *blanks;
                xxDt7 = %Dec(srDto:8:0);
              Else;
                xxDt7 = 0;
              Endif;


        //*********************************************************
        //** GET SYSTEM INFO
        //*********************************************************

          ##WCIF = *blanks;
          ##MCIF = *blanks;
          ##WCIN = *blanks;
          ##AR   = *blanks;
          ##NTWK = *blanks;
          ##UR   = *blanks;
          ##MED  = *blanks;
          ##EDI  = *blanks;
          ##WCDK = *blanks;

          RtvSysVals(##WCIF:##MCIF:##WCIN:##AR:##NTWK:##UR:##MED:
                     ##EDI:##WCDK);

        //*********************************************************
        //  GET THE COMPANY RECORD
        //*********************************************************
         Chain cOkY PCO01;

           If not %found(SMCOL);
             CONAME = *blanks;
           Endif;

           #BTYPE = '@';
           #BRND1 = *Blanks;
           #BRND2 = *Blanks;
           RtvCompInfo(#BTYPE:#BRND1:#BRND2);


         Chain C1kY PC101;
           If not %found(FMC1L);
             C1NAME = *Blanks;
           Endif;

        //*********************************************************
        //  GET THE ROUTING RECORDS AND PRINT THE ROUTING PAGES
        //*********************************************************
         xxFILE = '0';
         Chain orKY POR01;
B01        If %found(SMORL);
             xxTITL = ORTITL;
             Except ROUTE;
E01        Endif;

         xxFILE = '1';
         Chain orKY POR01;
B01        If %found(SMORL);
             x1TITL = ORTITL;
             Except ROUTE1;
E01        Endif;

         xxFILE = '2';
         Chain orKY POR01;
B01        If %found(SMORL);
             x2TITL = ORTITL;
             Except ROUTE2;
E01        Endif;

         *inOA = '1';
         *inOB = '1';
         *inOC = '1';
        //*********************************************************
        //  GET THE EDI CLAIMS CONTROL RECORD.  IF THE ELECTRONIC
        //  SUBMISSION FLAG DOESN'T EQUAL 'E' FOR THE TRANSACTION
        //  TYPE, DON'T PROCESS ANY OF THOSE TYPE RECORDS.
        //*********************************************************
         xxtSet = *Blanks;
         Chain eXkY PEX01;
           If %found(FEEXL);
             *in21 = *on;
           Endif;

         xxSEIN = EXSEIN;
         xxSZP  = EXSZP;

         xxtSet = '837';
         Chain exKY PEX01;
           If %found(FEEXL);
             *in21 = *on;
           Endif;

         //******************************************************
         //  Override file FTABL to the correct state.
         //******************************************************

         @@CMD  = QCA(1);
         xxST   = qqjSt;
         lenGTH = 56;
         QCmdExc(@@CMD:Length);

         Open FTABL;

         RtvCltId(#cltID);

       Endsr;

         //================================================================
        // GetDb - This subroutine retrieves the data base files needed
        //         to load the 837 files.
        //================================================================
       Begsr GetDb;

         Chain #hKY PEO01;
           If not %found(FAEOLG);
             *in99 = *on;
      /End-Free
     C   99              GOTO      SKIP
      /Free
           Endif;

        //*********************************************************
        //  Get the Group information
        //*********************************************************

         Chain fnKY PFN01;
           If not %found(FMFNL);
             FNNAME = *blanks;
             FNPZIP = *zeros;
             FNMZIP = *zeros;
E01        Endif;

         Chain f2KY PF201;
           If not %found(FDF2L);
             F29N01 = *zeros;
E01        Endif;

         Chain cmKY PCM01;
           If not %found(FMCML1);
             CMACDT = *zeros;
             CMSS#  = *zeros;
             CMSCL# = *blanks;
E01        Endif;

         Chain cdKY PCD01;
           If not %found(FMCDL);
             CDDTOB = *zeros;
             CDSEX  = *blanks;
             CDMSTS = *blanks;
E01        Endif;

         Chain emKY PEM01;
           If not %found(FMEML);
             EMFEIN = *Zeros;
E01        Endif;

         Chain #hkY1 PES01;
         Chain #hkY1 PEA01;
         Chain #HKY PED01;
         Chain #HKY PEH01;
         Chain #HKY PEP01;
         Chain #HKY PEU01;
         Chain #HKY PDD01;
         Chain #HKY PDH01;
         Chain #HKY PDP01;
         Chain #HKY PDU01;
           kkNWCD = EONTWK;
           //Clear PNW01;

         Chain NWKY PNW01;

       Endsr;

       //================================================================
       // MvDbAr - This subroutine loads the 837 data array used for the
       //          837 edit pgm.
       //================================================================
       Begsr MvDbAr;
         //**************************************************************
         //    - Trans set ID and MTC
         //**************************************************************
            DN(1) = LGTSET;
            DN(2) = LGMTC;
       //*****************************************************************
       //    - State Claim #
       //*****************************************************************
            DN(5) = CMSCL#;

       //*****************************************************************
       //    - Insurer(Group) FEIN, name and postal code
       //*****************************************************************
            DN(6) = %Char(F29N01);

            DN(7) = FNNAME;

B01           If FNPZIP <> *ZEROS;
                DN(616) = %Char(FNPZIP);

X01           Else;
B02             If FNMZIP <> *ZEROS;
                  DN(616) = %Char(FNMZIP);
E02             Endif;
E01           Endif;

         //******************************************************
         // Use Sender fein/zip for the insurer
         //******************************************************
B01      //If #CLTID = '025';

            DN(6)   = XXSEIN;
            DN(7)   = CONAME;
            DN(616) = XXSZP;

E01      //ENDIF;
         //****************************************************************
         //    - Claims administrator (Company) FEIN, name and postal code
         //****************************************************************

            DN(187) = %Char(COFEIN);
            DN(188) = CONAME;

B01         If COZIP <> *ZEROS;
              DN(14) = %Char(COZIP);
E01         Endif;

B01      //IF #CLTID = '025';

            DN(187) = %Char(C1FEIN);
            DN(188) = C1NAME;
B02           If COZIP <> *ZEROS;
                DN(14) = %Char(C1ZIP);
E02           Endif;

E01      //ENDIF;

         //***************************************************************
         //    - Claim Administrator Claim Number (co#/sub#/fnd/fyr/case)
         //***************************************************************
              xcACLC = %Char(xxCo#);
              xcSUB  = %Char(LGSUB#);
              xcFND  = %Char(LGFND);
              xcFYR  = %Char(LGFYR);
              xcCASE = %Char(LGCASE);

              xcACLC = xcFND + xcFYR + xcCASE;
              DN(15) = xcACLC;
        //---------------------------------------------------------
        //    - MCO info
        //---------------------------------------------------------

        //*********************************************************
        //    - Employer FEIN
        //*********************************************************
              DN(16) = %Char(EMFEIN);

        //*********************************************************
        //    - Employer Name & Address
       //*********************************************************
              If EMDBA <> *blanks;
                DN(18) = EMDBA;
              Else;

B01             If EMNAME <> *blanks;
                  DN(18) = EMNAME;
X01             Endif;
B02           Endif;

E02           If EMPAD1 = *blanks;
E01             DN(19) = EMMAD1;
                DN(20) = EMMAD2;
B01             DN(21) = EMMCTY;
                DN(22) = EMMST;

                  If EMMZIP <> *zeros;
                    DN(23) = %Char(EMMZIP);
                  Endif;

B02           Else;
                DN(19) = EMPAD1;
                DN(20) = EMPAD2;
                DN(21) = EMPCTY;
E02             DN(22) = EMPST;
              Endif;
X01
              DN(159) = *BLANKS;                                        //employer phone#

        //*********************************************************
        //    - Claimant information
        //*********************************************************

              srDti = CMACDT;
              srFfmt = '*CYMD';
              srTfmt = *blanks;
              SrCvtDat1(srDti:srFfmt:srTfmt:srDto:srErr);
              DN(31) = srDto;                                           //date of injury

              DN(42)  = CMSS#;                                          //SS#
              DN(43)  = CDLSN;                                          //last name
              DN(44)  = CDFSN;                                          //first name
              DN(45)  = CDMID;                                          //middle initial
              DN(255) = *blanks;                                        //last name suffix
              DN(46)  = CDADR1;                                         //address line 1
              DN(47)  = CDADR2;                                         //address line 2
              DN(48)  = CDCTY;                                          //city
              DN(49)  = CDST;                                           //state
              DN(50)  = %Char(CDZIP);                                   //postal code
              srDti   = CDDTOB;
              srFfmt = '*CYMD';
              srTfmt = *blanks;
              SrCvtDat1(srDti:srFfmt:srTfmt:srDto:srErr);
              DN(52)  = srDto;                                          //date of birth

         DN(51) = %Char(CDAREA + CDPHN#) ;
B01        If CDSEX <> *BLANKS;
              DN(53) = CDSEX;                                           //gender
X01        Else;
              DN(53) = 'U';
E01        Endif;

B01        If CDADR3 = *BLANKS;
              DN(155)= 'USA';                                           //employee country
X01        Else;
              DN(155)= CDCTRY;                                          //employee country
E01        Endif;

           DN(164) = 'USA';                                             //employer country

           DN(152) = *BLANKS;                                           //employment VISA
           DN(153) = *BLANKS;                                           //green card

        //*********************************************************
        //    - Bill Header info
        //*********************************************************

           DN(501) = %Char(EOCHRG);                                     //total amt charged
           DN(506) = 'N';                                               //prov sig on file
B01          If EONTWK <> *BLANKS;
               DN(507) = nwfl02;                                        //prov agree code
E01          Endif;

           DN(742) = DN(507);


B01        If EOBTYP = 'H';
             DN(622) = %Char(EUADHR);
             DN(623) = %Char(EUDCHR);
E01        Endif;

B01        If EOBTYP = 'D';
             xxPos = DDPOS;
E01        Endif;

           DN(624) = *BLANKS;                                           //initial amt paid
           DN(516) = %Char(EAPAID);                                     //total amt paid
           srDti   = EOCSDT;

B01        If EOCSDT = *ZEROS;
             srDti = EOCRDT;
E01        Endif;

           srFfmt = '*CYMD';
           srTfmt = *blanks;

           SrCvtDat1(srDti:srFfmt:srTfmt:srDto:srErr);

           DN(510) = srDto;                                             //date of bill

B01        If LGRCN <> *ZEROS;
B02           If ESRSDT <> *ZEROS;
                 srDti = ESRSDT;
X02           Else;
                 srDti = ESRRDT;
E02           Endif;

           srFfmt = '*CYMD';
           srTfmt = *blanks;

           SrCvtDat1(srDti:srFfmt:srTfmt:srDto:srErr);

           DN(510) = srDto;                                             //Dt received bill
E01        Endif;

B01        If LGRCN <> *ZEROS;
              srDti = ESRRDT;
X01        Else;
              srDti = EOCRDT;
E01        Endif;

           srFfmt = '*CYMD';
           srTfmt = *blanks;

           SrCvtDat1(srDti:srFfmt:srTfmt:srDto:srErr);

           DN(511) = srDto;                                             //Dt received bill
        //----------------------------------------------------------------
        // field EAWCPD format is mmddyycc, so no coversion is necessary
        //----------------------------------------------------------------
           srDti  = %Dec(EAWCPD:8:0);
           srFfmt = '*YYMD';
           srTfmt = '*MDYY';

             SrCvtDat2(srDti:srFfmt:srTfmt:srDto:srErr);
           xxWcPd = srDto;

           qqCmDt = *blanks;
B01           If xxWcPd <> '00000000' and xxWcPd <> *BLANKS;

B02             If ##WCIF = 'Y';
                qqFnd = xcFND;
                qqTrDt = xxWcPd;
                qqCmDt = *blanks;

                   GetAgtInfo(qqCo#:qqFnd:qqTrDt:qqCmDt);

E02             Endif;
E01           Endif;

           DN(508) = LGMTC;                                             //bill subm reason cod

B01           If EOBTYP = 'H';
                 srDti = EUADDT;
                 srFfmt = '*CYMD';
                 srTfmt = *blanks;

                   SrCvtDat1(srDti:srFfmt:srTfmt:srDto:srErr);

                   DN(513) = srDto;                                     //admission date
                   srDti = EUDCDT;
                   srFfmt = '*CYMD';
                   srTfmt = *blanks;

                     SrCvtDat1(srDti:srFfmt:srTfmt:srDto:srErr);

                        DN(514) =  srDto;                               //discharge date
                        DN(515) =  EUCTCD;                              //Contract type code
                        DN(521) =  EUDI1;                               //principle diag code
                        DN(518) =  %Char(EUDRG#);                       //DRG Code

                        DN(626) =  EUPRC1;                              //HCPCS prin proc code
                        DN(714) =  EUPRC2;                              //HCPCS proc bill code
E01           Endif;

B01           If EOBTYP = 'N';
                  DN(521) = EHDI1;                                      //principle diag code
E01           Endif;

B01           If EOBTYP = 'X';
                 DN(502) = EPBTCD;                                      //billing type code
B02                 If EPBTCD = *blanks;                                //code
                        DN(502) = 'RX';                                 //billing type code
E02                 Endif;

B02                 If EPACCT = *BLANKS;
                       EPACCT = CMSS#;
E02                 Endif;
E01           Endif;

B01           If EOBTYP = 'D';
B02              If EDACCT = *BLANKS;
                     EDACCT = CMSS#;
E02              Endif;
                   DN(521) = EDDI1;                                       //principle diag code

E01           Endif;

              srDti = EOFRDT;
              srFfmt = '*CYMD';
              srTfmt = *blanks;

                 SrCvtDat1(srDti:srFfmt:srTfmt:srDto:srErr);
              If srDto <> *blanks;
                xxFRdt = %Dec(srDto:8:0);
              Else;
                xxFRdt = 0;
              Endif;


              srDti = EOTODT;
              srFfmt = '*CYMD';
              srTfmt = *blanks;

                 SrCvtDat1(srDti:srFfmt:srTfmt:srDto:srErr);
              If srDto <> *blanks;
                xxTODT = %Dec(srDto:8:0);
              Else;
                xxTODT = 0;
              Endif;

              xxWK16 = %Char(xxFRdt) + %Char(xxTOdt);

        //*********************************************************
        //    - Billing provider info
        //*********************************************************

         Chain pdKY ppD01;
           If %found(FMPDL);

B02              If EOBTYP = 'X';
                    DN(701) = EPPLIC ;                                  // license#
E02              Endif;
E02        Endif;

         //*******************************************************************
         // CLAIM SUBMITTER IDENTIFIER - BILLING PROVIDER UNIQUE BILL
         // IDENTIFICATION NUMBER
         //*******************************************************************
         Select ;
          When EOBTYP = 'N';
           dn(523) = EHACCT ;
          When EOBTYP = 'H';
           dn(523) = EUACCT ;
          When EOBTYP = 'X';
           dn(523) = EUACCT ;
          When EOBTYP = 'D';
           dn(523) = EPACCT ;
         Endsl ;

         //*******************************************************************
         // FACILITY CODE VALUE (Facility code or Place of service bills code)
         //*******************************************************************
         Select ;
           When EOBTYP = 'H';
             dn(504) = %Subst(EUBLTY:1:2) ;
             dn(504) = %trim(dn(504));
           Other ;
             dn(555) = DHPOS ;
             dn(555) = %trim(dn(555));
         Endsl ;

         //*******************************************************************
         // FACILITY CODE QUALIFIER - BILLING FORMAT CODE
         //*******************************************************************
         Select ;
           When EOBTYP = 'H';
             dn(503) = 'A' ;
           Other;
             dn(503) = 'B' ;
         Endsl ;

         //*******************************************************************
         // CLAIM FREQUENCY TYPE CODE - BILL FREQUENCY TYPE CODE
         //*******************************************************************
         Select ;
           When EOBTYP = 'H';
             dn(505) = %Subst(EUBLTY:3:1) ;
         Endsl ;

         //*******************************************************************
         // Provider Agreement Code
         //*******************************************************************
         If EONTWK <> *BLANKS;
             dn(507) = nwfl02;
         Endif ;

         //*******************************************************************
         // DATE TIME PERIOD
         //*******************************************************************
             dn(509) = %trim(%Char(EOFRDT)) + '-' + %trim(%Char(EOTODT)) ;
         //*******************************************************************
         // Date Insurer Paid Bill
         //*******************************************************************
             dn(512) = %Char(EAWCPD);

         //*******************************************************************
         // Unique Bill ID
         //*******************************************************************
             dn(500) = %Char(LGEOB#) + %Char(LGRCN);

         //*******************************************************************
         // Transaction Tracking Number
         //*******************************************************************
             dn(500) = %Char(LGEOB#) + %Char(LGRCN);

         //*******************************************************************
         // Admitting Diagnosis 1
         //*******************************************************************
           If EOBTYP = 'H' and (EUICN = 'IP' or EUICN = 'IR' or
           EUICN = 'IM' or EUICN = 'IS');
             If EUICDIND = 'Y' ;
               dn(535) = EUADIA1001 ;
             else ;
               dn(535) = EUADDI ;
             Endif;
           Endif;

         //*******************************************************************
         // Diagnosis code 1 to 8
         //*******************************************************************
         If EOBTYP = 'H' ;
           If EUICDIND = 'Y' ;
             Select ;
             When EUICD1001 <> *blanks ;
               dn(522) = EUICD1001 ;
             When EUICD1002 <> *blanks ;
               dn(522) = EUICD1002 ;
             When EUICD1003 <> *blanks ;
               dn(522) = EUICD1003 ;
             When EUICD1004 <> *blanks ;
               dn(522) = EUICD1004 ;
             When EUICD1005 <> *blanks ;
               dn(522) = EUICD1005 ;
             When EUICD1006 <> *blanks ;
               dn(522) = EUICD1006 ;
             When EUICD1007 <> *blanks ;
               dn(522) = EUICD1007 ;
             When EUICD1008 <> *blanks ;
               dn(522) = EUICD1008 ;
             Endsl ;
           Else ;
             Select;
             When EUDIA1 <> *blanks ;
               dn(522) = EUDIA1 ;
             When EUDIA2 <> *blanks ;
               dn(522) = EUDIA2 ;
             When EUDIA3 <> *blanks ;
               dn(522) = EUDIA3 ;
             When EUDIA4 <> *blanks ;
               dn(522) = EUDIA4 ;
             When EUDIA5 <> *blanks ;
               dn(522) = EUDIA5 ;
             When EUDIA6 <> *blanks ;
               dn(522) = EUDIA6 ;
             When EUDIA7 <> *blanks ;
               dn(522) = EUDIA7 ;
             When EUDIA8 <> *blanks ;
               dn(522) = EUDIA8 ;
             Endsl ;
           Endif ;
         Endif ;

         //*******************************************************************
         // Outpatient Reason for Visit 1.
         //*******************************************************************
           If EOBTYP = 'H' and (EUICN = 'HO' or EUICN = 'HS' or
           EUICN = 'AO' or EUICN = 'AS');
             If EUICDIND = 'Y' ;
               dn(520) = EUADIA1001 ;
             else ;
               dn(520) = EUADDI ;
             Endif;
           Endif;

         //*******************************************************************
         // Procedure Codes 1 to 6.
         //*******************************************************************
           If EOBTYP = 'H' and (EUICN = 'IP' or EUICN = 'IR' or
           EUICN = 'IM' or EUICN = 'IS');
             If EUICDIND = 'Y' ;
               dn(525) = EUPRC1001 ;
               Select ;
               When EUPRC1002 <> *blanks ;
                 dn(736) = EUPRC1002 ;
               When EUPRC1003 <> *blanks ;
                 dn(736) = EUPRC1003 ;
               When EUPRC1004 <> *blanks ;
                 dn(736) = EUPRC1004 ;
               When EUPRC1005 <> *blanks ;
                 dn(736) = EUPRC1005 ;
               When EUPRC1006 <> *blanks ;
                 dn(736) = EUPRC1006 ;
               Endsl ;
             else ;
               dn(525) = EUPRC1 ;
               Select;
               When EUPRC2 <> *blanks ;
                 dn(736) = EUPRC2 ;
               When EUPRC3 <> *blanks ;
                 dn(736) = EUPRC3 ;
               When EUPRC4 <> *blanks ;
                 dn(736) = EUPRC4 ;
               When EUPRC5 <> *blanks ;
                 dn(736) = EUPRC5 ;
               When EUPRC6 <> *blanks ;
                 dn(736) = EUPRC6 ;
               Endsl ;
             Endif ;
             // Moving all the procedure code dates.
             dn(550) = %Char(EUPRD1);
             Select ;
             When EUPRD2 <> *zeros ;
               dn(524) = %Char(EUPRD2);
             When EUPRD3 <> *zeros ;
               dn(524) = %Char(EUPRD3);
             When EUPRD4 <> *zeros ;
               dn(524) = %Char(EUPRD4);
             When EUPRD5 <> *zeros ;
               dn(524) = %Char(EUPRD5);
             When EUPRD6 <> *zeros ;
               dn(524) = %Char(EUPRD6);
             Endsl ;
           Endif;

         //*******************************************************************
         // Condition Code 1 to 7.
         //*******************************************************************
         If EOBTYP = 'H' and (EUICN = 'IP' or EUICN = 'IR' or
         EUICN = 'IM' or EUICN = 'IS');
           Select ;
           When EUCOC1 <> *blanks ;
             dn(566) = EUCOC1 ;
           When EUCOC2 <> *blanks ;
             dn(566) = EUCOC2 ;
           When EUCOC3 <> *blanks ;
             dn(566) = EUCOC3 ;
           When EUCOC4 <> *blanks ;
             dn(566) = EUCOC4 ;
           When EUCOC5 <> *blanks ;
             dn(566) = EUCOC5 ;
           When EUCOC6 <> *blanks ;
             dn(566) = EUCOC6 ;
           When EUCOC7 <> *blanks ;
             dn(566) = EUCOC7 ;
           Endsl ;
         Endif ;

         //*******************************************************************
         // Bill Adjustment Amount 1.
         //*******************************************************************
           dn(545) = %Char(LGCHRG - LGPAID);

         //*******************************************************************
         // Prior Paid Amount.
         //*******************************************************************
           dn(760) = %Char(EOPAID  - EAPAID);

         //*******************************************************************
         // DRG#
         //*******************************************************************
           dn(549) = %Char(EUDRG#);

         //*******************************************************************
         // Billing Provider Last/group Name.
         //*******************************************************************
           dn(528) = LGBPLN;

         //*******************************************************************
         // Billing Provider First Name.
         //*******************************************************************
           dn(529) = LGBPFN;

         //*******************************************************************
         // Billing Provider Middle Name.
         //*******************************************************************
           dn(530) = LGBPMI;

         //*******************************************************************
         // Billing Provider NPI#.
         //*******************************************************************
           dn(634) = EOBNPI ;

         //*******************************************************************
         // Billing Provider Taxonomy Code.
         //*******************************************************************
           dn(537) = PDTAXO ;

         //*******************************************************************
         // Bill Provider Primary Address.
         //*******************************************************************
           dn(538) = LGBPA1 ;

         //*******************************************************************
         // Bill Provider Secondary Address.
         //*******************************************************************
           dn(539) = LGBPA2 ;

         //*******************************************************************
         // Bill Provider City.
         //*******************************************************************
           dn(540) = LGBPCT ;

         //*******************************************************************
         // Bill Provider State.
         //*******************************************************************
           dn(541) = LGBPST ;

         //*******************************************************************
         // Bill Provider Postal Code.
         //*******************************************************************
           dn(542) = LGBPZP ;

         //*******************************************************************
         // Bill Provider Country.
         //*******************************************************************
           dn(569) = LGBPCR ;

         //*******************************************************************
         // Bill Provider FEIN.
         //*******************************************************************
           dn(629) = LGBPFE ;

         //*******************************************************************
         // Bill Provider State License Number.
         //*******************************************************************
         Select ;
          When EOBTYP = 'N';
           dn(630) = EHFLIC ;
          When EOBTYP = 'H';
           dn(630) = EUFLIC ;
          When EOBTYP = 'X';
           dn(630) = EPFLIC ;
          When EOBTYP = 'D';
           dn(630) = EDFLIC ;
         Endsl ;

         //*******************************************************************
         // Rendering Provider Last/group Name.
         //*******************************************************************
         Select ;
          When EOBTYP = 'N';
           dn(638) = EHRLSN ;
          When EOBTYP = 'H';
           dn(638) = EURLSN ;
          When EOBTYP = 'D';
           dn(638) = EDRLSN ;
         Endsl ;

         //*******************************************************************
         // PROVIDER NAME FIRST - Rendering Bill Provider First Name.
         //*******************************************************************
         Select ;
          When EOBTYP = 'N';
           dn(639) = EHRFSN ;
          When EOBTYP = 'H';
           dn(639) = EURFSN ;
          When EOBTYP = 'D';
           dn(639) = EDRFSN ;
         Endsl ;

         //*******************************************************************
         // Rendering Provider NPI#.
         //*******************************************************************
         Select ;
          When EOBTYP = 'N';
           dn(647) = EHSNPI ;
          When EOBTYP = 'H';
           dn(647) = EUANPI ;
          When EOBTYP = 'D';
           dn(647) = EDTNPI ;
         Endsl ;

         //*******************************************************************
         // Rendering Provider Taxonomy Code.
         //*******************************************************************
           dn(651) = PDTAXO ;

         //*******************************************************************
         // Rendering Provider State License Number.
         //*******************************************************************
         Select ;
          When EOBTYP = 'N';
           dn(643) = EHRNLC ;
          When EOBTYP = 'X';
           dn(643) = EPFLIC ;
          When EOBTYP = 'D';
           dn(643) = EDRNLC ;
         Endsl ;

         //*******************************************************************
         // Facility Provider Last/group Name.
         //*******************************************************************
           dn(678) = EOFNAM ;

         //*******************************************************************
         // Facility Provider NPI#.
         //*******************************************************************
           dn(682) = *blanks ;

         //*******************************************************************
         // Facility Provider Postal Code.
         //*******************************************************************
           dn(688) = %Char(EOFZIP) ;

         //*******************************************************************
         // Referring Provider Last/group Name.
         //*******************************************************************
           dn(690) = EORLNM ;

         //*******************************************************************
         // Referring Provider Last/group Name.
         //*******************************************************************
           dn(691) = EORFNM ;

         //*******************************************************************
         // Referring Provider Middle Initial.
         //*******************************************************************
           dn(699) = *blanks ;

         //*******************************************************************
         // Referring Provider State License Number.
         //*******************************************************************
         Select ;
          When EOBTYP = 'N';
           dn(695) = EHRFLC ;
          When EOBTYP = 'H';
           dn(695) = EURLIC ;
          When EOBTYP = 'D';
           dn(695) = EDRFLC ;
         Endsl ;


         DN(531) = *blanks;                                             //last name suffix

        //*********************************************************
        //* If billing lic# different from rendering lic#
        //    - Rendering provider info
        //*********************************************************

         DN(638) = *blanks;                                             //last name
         DN(639) = *blanks;                                             //first name
         DN(641) = *blanks;                                             //last name suffix
         DN(642) = *blanks;                                             //FEIN
         DN(651) = *blanks;                                             //primary spec code
         DN(652) = *blanks;                                             //primary address
         DN(653) = *blanks;                                             //secondary address
         DN(654) = *blanks;                                             //city
         DN(655) = *blanks;                                             //state
         DN(656) = *blanks;                                             //postal code

         //******************************************************
         //    - Default to the billing provider
         //******************************************************

         DN(638) = PDNAME;                                              //last name

              DN(638) = EORNAM;                                         //rend pvdr lst/grp na
              DN(656) = %Char(EOZIP);                                   //postal code

         //******************************************************
         // Rendering Provider State for dental bill.
         //******************************************************

B01        If EOBTYP = 'D';
              DN(655) = LGJST;                                          //rend pvdr state
E01        Endif;

           DN(642) = %Char(EOFEIN);                                     //FEIN

        //---------------------------------------------------------
        // Rendering provider license numbers
        //---------------------------------------------------------
        //

         //******************************************************
         //    - Referring provider info
         //******************************************************
            DN(694) = %Char(EORFEI);                                    //FEIN

        //*********************************************************
        // DEFAULT IN ORDER TO SEND HISTORICAL
        //*********************************************************

B01      If EORLNM = *blanks;
            DN(694) = %Char(EOFEIN);                                    //FEIN
E01      Endif;

         //******************************************************
         // Referring provider license numbers
         //******************************************************

B02          Select;
S01             When EOBTYP = 'H';
                    DN(695) = EURLIC;
E02          Endsl;

         //******************************************************
         //    - Facility info
         //******************************************************
B01        If DN(503) = 'A';
              DN(679) = %Char(EOFFEI);                                  //facility FEIN

X01        Else;

B02           If DN(503) = 'B' and (EOBTYC = 'DM' or EOBTYC = 'RX'
                 or EOBTYC = 'MO');
                    DN(502) = EOBTYC;                                   //billing type code
E02           Endif;

E01        Endif;

         //******************************************************
         //    - Managed care organization info
         //******************************************************
            DN(209) = *blanks;                                          //MCO name
            DN(704) = *blanks;                                          //MCO FEIN
            DN(708) = *blanks;                                          //MCO primary address
            DN(709) = *blanks;                                          //MCO secondary addres
            DN(710) = *blanks;                                          //MCO city
            DN(711) = *blanks;                                          //MCO state
            DN(712) = *blanks;                                          //MCO postal code

B01           If NWFL02 = 'H';

                 DN(208) = NWNWCD;
                 DN(209) = NWNAME;

X01           Else;

                 DN(208) = *blanks;
                 DN(209) = *blanks;

E01           Endif;                                                    //NWFL02 ifeq

         //******************************************************
         //    - Bill adjustment reasons
         //******************************************************
            DN(543) = *blanks;                                          //group code
            DN(544) = *blanks;                                          //reason code

        //*********************************************************
        //    - Line item info
        //*********************************************************

            DN(547) = %Char(CALIN#);                                    //line #

B01            If EOBTYP = 'N';
                  DN(551) = DHPRDS;                                     //procedure desc
                  DN(552) = %Char(DHCHRG);                              //total charged

B02                  If DHBPRC >= '00100'  and DHBPRC <= '01999';
                        DN(553) = 'MA';

X02                  Else;

B03                     If DHBPRC <> *BLANKS;
                           DN(553) = 'UN';
X03                     Else;
                           DN(553) = *blanks;
E03                     Endif;

E02                  Endif;

                  DN(554) = %Char(DHUNTS);                              //units billed

B02                  If DHDXR1 = 'X';
                        DN(557) = '1';                                  //diagnosis pointer
E02                  Endif;

B02                  If DHDXR2 = 'X';
                        DN(557) = '2';                                  //diagnosis pointer
E02                  Endif;

B02                  If DHDXR3 = 'X';
                        DN(557) = '3';                                  //diagnosis pointer
E02                  Endif;

B02                  If DHDXR4 = 'X';
                        DN(557) = '4';                                  //diagnosis pointer
E02                  Endif;

                     DN(559) = *BLANKS;
                     DN(574) = %Char(DHPAID);                           //Revenue bill code
                     DN(600) = DHPOS;                                   //total paid
                     DN(714) = DHBPRC;                                  //POS line code
                     DN(726) = DHPRCD;                                  //HCPCS paid code
E01            Endif;

B01            If EOBTYP = 'H';
                  DN(551) = *BLANKS;                                    //procedure desc
                  DN(552) = %Char(DUCHRG);                              //total charged
                  DN(557) = *BLANKS;                                    //diagnosis pointer
                  DN(559) = %Char(DUBRVC);
                  DN(574) = %Char(DUPAID);                              //total paid
                  DN(576) = %Char(DURVCD);                              //Revenue bill code

B02                  If DURATE = *BLANKS;
                        DN(553) = 'DA';                                 //days units code
X02                  Else;
                        DN(553) = 'UN';
E02                  Endif;
E01            Endif;

B01            If EOBTYP = 'D';
                  DN(552) = %Char(DDCHRG);                              //total charged
                  DN(553) = 'UN';                                       //days units code
                  DN(574) = %Char(DDPAID);                              //total paid per line
E01            Endif;

B01            If EOBTYP = 'X';
                  DN(561) = DPRX#;                                      //presc line number
                  DN(562) = '1';                                        //dispensed as written
                  DN(570) = %Char(DPUNTS);                              //Quantity dispensed
                  DN(571) = %Char(DPRXDY);                              //Drugs/Supplies # day
                  DN(574) = %Char(DPPAID);                              //total paid per line
                  DN(564) = DPBCDT;                                     //Cost determ
                  DN(579) = %Char(DPDFEE);                              //Disp fee
E01            Endif;

         //******************************************************
         //    - Line item adjustment reasons
         //******************************************************
             DN(731) = *blanks;                                         //group code
             DN(732) = *blanks;                                         //reason code

       Endsr;

         //================================================================
         // UpdLog - This subroutine updates the log record.
         //================================================================
       Begsr UpdLog;
         //**************************************************************
         //   - Insurer and Claims administrator info
         //**************************************************************
             LGINNM = DN(7);
             LGINZP = DN(616);
             LGINFE = DN(6);
             LGCANM = DN(188);
             LGCAZP = DN(14);
             LGCAFE = DN(187);
             If DN(51) <> *blanks;
               LGCPHN# = %Dec(DN(51):10:0) ;
             Else;
               LGCPHN# = 0;
             Endif;


        //*****************************************************************
        //   - MCO info
        //*****************************************************************
           LGMCNM = DN(209);
           LGMCA1 = DN(708);
           LGMCA2 = DN(709);
           LGMCCT = DN(710);
           LGMCST = DN(711);
           LGMCZP = DN(712);
           LGMCCR = DN(713);
           LGMCFE = DN(704);
           LGMCID = DN(208);

        //*****************************************************************
        //   - Employer info
        //*****************************************************************
           LGEMNM = DN(18);
           LGEMFE = DN(16);
           LGEMA1 = DN(19);
           LGEMA2 = DN(20);
           LGEMCT = DN(21);
           LGEMST = DN(22);
           LGEMZP = DN(23);
           XXWK12 = DN(159);
             If XXWK12 <> *blanks;
               LGEMAR = %Dec(XXWK12:3:0);
               LGEMPH = %Dec(XXWK12:7:0);
             Else;
               LGEMAR = 0;
               LGEMPH = 0;
             Endif;
           LGERCT = DN(164);

        //*****************************************************************
        //   - Claimant info
        //*****************************************************************
           LGSCL# = DN(5);
           LGSS#  = DN(42);
           LGACDT = CMACDT;
           LGACTM = CMACTM;
           LGTYPE = *BLANKS;
           LGCLTY = *BLANKS;
           LGACLT = *BLANKS;
           LGCLLN = DN(43);
           LGCLFN = DN(44);
           LGCLMI = DN(45);
           LGCLSX = DN(255);
           LGCLGC = DN(153);
           LGCLVS = DN(152);
           LGCLA1 = DN(46);
           LGCLA2 = DN(47);
           LGCLCT = DN(48);
           LGCLST = DN(49);
           LGCLZP = DN(50);
           LGDTOB = CDDTOB;
           LGSEX  = DN(53);
           LGMSTS = CDMSTS;
           LGECTY = DN(155);

        //*********************************************************
        //   - EOMB Header info
        //*********************************************************
             If DN(510) <> *blanks;
               srDti = %Dec(DN(510):8:0);
             Else;
               srDti = 0;
             Endif;
           srFfmt = *blanks;
           srTfmt = '*CYMD';
              SrCvtDAt(srDti:srFfmt:srTfmt:srDto:srErr);
                 If srDto <> *blanks;
                    LGCSDT = %Dec(srDto:7:0);                              //date of bill
                 Else;
                    LGCSDT = 0;
                 Endif;
B01                If EORCN > 0;
                      If srDto <> *blanks;
                        LGRSDT = %dec(srDto:7:0);                         //date of bill
                      Else;
                        LGRSDT = 0;
                      Endif;
E01                Endif;
                 LGCRDT = EOCRDT;
                 LGRRDT = ESRRDT;
                 LGAPDT = *zeros;

                 srDti = %Dec(EAWCPD:8:0);

                 srFfmt = *blanks;
                 srTfmt = '*CYMD';
                    SrCvtDAt(srDti:srFfmt:srTfmt:srDto:srErr);
                       If srDto <> *blanks;
                         LGPDDT = %Dec(srDto:7:0);
                       Else;
                         LGPDDT = 0;
                       Endif;
B01                       If qqCmDt <> *BLANKS;
                             srDti = %Dec(qqCmDt:8:0);
                             srFfmt = *blanks;
                             srTfmt = '*CYMD';
                                SrCvtDAt(srDti:srFfmt:srTfmt:srDto:srErr);
                                   If srDto <> *blanks;
                                     LGPDDT = %Dec(srDto:7:0);
                                   Else;
                                     LGPDDT = 0;
                                   Endif;
                          Else;
                             srDti = 0;
E01                       Endif;

         //******************************************************
         // When no payment was made then report the EOB Print date
         //******************************************************
B01         If LGPDDT = *ZEROS;

B02            If EAEPDT <> *ZEROS;

                  srDti = EAEPDT;
                  srFfmt = '*YYMD';
                  srTfmt = '*CYMD';

                    SrCvtDat3(srDti:srFfmt:srTfmt:srDto:srErr);
                       If srDto <> *blanks;
                         LGPDDT = %Dec(srDto:7:0);
                       Else;
                         LGPDDT = 0;
                       Endif;

X02            Else;
                   srDti = EADT;
                   srFfmt = '*YYMD';
                   srTfmt = '*CYMD';

                      SrCvtDat3(srDti:srFfmt:srTfmt:srDto:srErr);
                         If srDto <> *blanks;
                           LGPDDT = %Dec(srDto:7:0);
                         Else;
                           LGPDDT = 0;
                         Endif;

E02            Endif;
E01         Endif;
            LGPDCK = EAWCPC;
            LGBLTC = DN(502);
            LGBLFC = DN(503);
            LGHSCD = *BLANKS;
            LGFREQ = DN(505);
            LGBENE = *BLANKS;
            LGPAUT = EOCERT;
            LGPAUT = %Char(EOEXT);
            LGDTRT = *BLANKS;
            LGFRDT = EOFRDT;
            LGTODT = EOTODT;
            LGCHRG = EOCHRG;
            LGMRA  = EOMRA;
            LGOTHR = EOOTHR;
            LGOTH2 = EOOTH2;
            LGNDIS = EONDIS;
            LGPAID = EAPAID;
            LGNOCV = *zeros;
            If DN(624) <> *blanks;
              LG1PD = %Dec(DN(624):11:2);
            Else;
              LG1PD  = 0;
            Endif;
            LGPVPD = *ZEROS;
            LGOHIW = EOOHIW;
            LGOHPI = EOOHPI;
            LGPOSC = DN(555);

        //*************************************************************
        //   - Retrieve the Header level EOMB codes and load the group
        //     and reason codes.
        //*************************************************************
           X = 1;
           Chain #HKY PEE01;
             If %found(FAEEL) and X <= 10;
                xxKEY = EEEOB;
                     Chain tbKY PEB03;                                  //for FTEBLNC file.
B02                  If %found(FTEBLNC);
                        GC(X) = EBXREF;
                        RC(X) = EBXRF2;
                        X += 1;
E02                  Endif;

                     Reade #HKY PEE01;
E01          Endif;
         //******************************************************
         //   - Billing provider info
         //******************************************************
             LGBPLN = DN(528);
             LGBPFN = DN(529);

             LGBPMI = Dn(530);
             LGBPSX = DN(531);
             LGBPFE = DN(629);
             LGBPMD = EOMOD;
             LGBPSC = DN(537);
             LGBPA1 = DN(538);
             LGBPA2 = DN(539);
             LGBPCT = DN(540);
             LGBPST = DN(541);
             LGBPZP = DN(542);

B01             If P1MCTR = *BLANKS;
                   LGBPCR = 'USA';
X01             Else;
                   LGBPCR = P1MCTR;
E01             Endif;

             LGPA#  = *BLANKS;
             LGNAT# = *BLANKS;
             LGSLIC = *BLANKS;
             LGPTP# = *BLANKS;
             LGBPID = DN(523);

        //*********************************************************
        //   - Rendering provider info
        //*********************************************************
             LGRPLN = DN(638);
             LGRPFN = DN(639);
             LGRPMI = *BLANKS;
             LGRPSX = DN(641);
             LGRPFE = DN(642);
             LGRPMD = *zeros;
             LGRPSC = DN(651);
             LGRPA1 = DN(652);
             LGRPA2 = DN(653);
             LGRPCT = DN(654);
             LGRPST = DN(655);
             LGRPZP = DN(656);
             LGRPCR = *BLANKS;
             LGPSIG = DN(506);

        //*********************************************************
        //   - Referring provider info
        //*********************************************************
             LGFPLN = DN(690);
             LGFPFN = DN(691);
             LGFPMI = DN(692);
             LGFPSX = DN(693);
             LGFPFE = DN(694);
             LGFLIC = DN(695);

        //*********************************************************
        //   - Facility info
        //*********************************************************
             LGFNAM = DN(678) ;

        //*********************************************************
        //   - Gate keeper info
        //*********************************************************
             LGGTKI = 'GP';
             LGGTK# = *BLANKS;
             LGSTS  = 'EX';
             LGUSER = Q1USER;
             LGPGM  = Q1PGM;
             LGEXDT = XXEXDT;
             //DSDTTM = %Dec(%Time());
             DSDATE = %Dec(%Date():*usa);
             DSTIME = %Dec(%Time():*hms);
             LGTIME = DSTIME;

             srDti  = DSDATE;
             srFfmt = *blanks;
             srTfmt = '*CYMD';
               SrCvtDAt(srDti:srFfmt:srTfmt:srDto:srErr);
             If srDto <> *blanks;
               LGDT = %Dec(srDto:7:0);
             Else;
               LGDT = 0;
             Endif;

         Update PLG01;

       Endsr;
       //================================================================
       // LD837 - This subroutine loads the 837 extract files.
       //           The Header files are loaded first, then the
       //           detail files are loaded.
       //================================================================
       Begsr LD837;
         //  Load Header files.
         //**************************************************************
         //  If this is a dental bill, load the Dental extract files.
         //**************************************************************
B01      IF EOBTYP = 'D';
           CLEAR PED02;
           Chain #HKY PED01;
              EDCO#  = xxCo# ;
              EDRCN  = LGRCN ;
              EDLSEQ = LGLSEQ;
              EDSTAT = 'A'   ;
              EDUSER = Q1USER;
              EDPGM  = Q1PGM ;
              //DSDTTM = %Dec(%Time());
              DSDATE = %Dec(%Date():*usa);
              DSTIME = %Dec(%Time():*hms);
              EDTIME = DSTIME ;
              EDDT   = xxDt7 ;
              EDIUSR = Q1USER ;
              EDIPGM = Q1PGM ;
              EDITIM = DSTIME ;
              EDIDT  = xxDt7 ;

                XXICD9 = EDDI1 ;
                Chain DIKY PDI01;
                  If %found(FMDIL) ;
                     EDDIA1 = DIDS1 ;
                  Else ;
                     EDDIA1 = *blanks ;
                  Endif ;

                XXICD9 = EDDI2 ;
                Chain DIKY PDI01;
                  If %found(FMDIL) ;
                     EDDIA2 = DIDS1 ;
                  Else ;
                     EDDIA2 = *blanks ;
                  Endif ;

                XXICD9 = EDDI3 ;
                Chain DIKY PDI01;
                  If %found(FMDIL) ;
                     EDDIA3 = DIDS1 ;
                  Else ;
                     EDDIA3 = *blanks ;
                  Endif ;

                XXICD9 = EDDI4 ;
                Chain DIKY PDI01;
                  If %found(FMDIL) ;
                     EDDIA4 = DIDS1 ;
                  Else ;
                     EDDIA4 = *blanks ;
                  Endif ;

              //*********************************************************
              // Billing provider license number
              //*********************************************************
              EDFLIC = DN(630) ;

              //*********************************************************
              // Rendering provider license number
              //*********************************************************
              EDRNLC = DN(643) ;
              EDTNPI = DN(647) ;

            WRITE PED02;

E01      ENDIF;


         //***********************************************************
         //  If this is a provider bill, load the HCFA extract files.
         //***********************************************************
B01      IF EOBTYP = 'N';
           CLEAR PEH02;
           Chain #HKY PEH01;
              EHCO#  = xxCo# ;
              EHRCN  = LGRCN ;
              EHLSEQ = LGLSEQ ;

                  XXICD9 = EHDI1 ;
                    Chain DIKY PDI01;
                      If %found(FMDIL) ;
                         EHDIA1 = DIDS1 ;
                      Else ;
                         EHDIA1 = *blanks ;
                      Endif ;

                  XXICD9 = EHDI2 ;
                    Chain DIKY PDI01;
                      If %found(FMDIL) ;
                         EHDIA2 = DIDS1 ;
                      Else ;
                         EHDIA2 = *blanks ;
                      Endif ;

                  XXICD9 = EHDI3 ;
                    Chain DIKY PDI01;
                      If %found(FMDIL) ;
                         EHDIA3 = DIDS1 ;
                      Else ;
                         EHDIA3 = *blanks ;
                      Endif ;

                  XXICD9 = EHDI4 ;
                    Chain DIKY PDI01;
                      If %found(FMDIL) ;
                         EHDIA4 = DIDS1 ;
                      Else ;
                         EHDIA4 = *blanks ;
                      Endif ;
              EHSTAT = 'A' ;
              EHUSER = Q1USER ;
              EHPGM  = Q1PGM ;
              //DSDTTM = %Dec(%Time());
              DSDATE = %Dec(%Date():*usa);
              DSTIME = %Dec(%Time():*hms);
              EHTIME = DSTIME ;
              EHDT   = xxDt7  ;
              EHIUSR = Q1USER ;
              EHIPGM = Q1PGM  ;
              EHITIM = DSTIME ;
              EHIDT  = xxDt7  ;

              //*************************************************
              // Billing provider license number
              //*************************************************
              EHFLIC = DN(630) ;

              //*************************************************
              // Rendering provider license number
              //*************************************************
              EHRNLC = DN(643) ;
              EHRNPI = DN(699) ;

              WRITE PEH02;
            Endif ;


         //******************************************************
         //  If this is a Rx bill, load the Rx extract files.
         //******************************************************
B01      IF EOBTYP = 'X';
           CLEAR PEP02;
           Chain #HKY PEP01;
               EPCO#  = xxCo# ;
               EPRCN  = LGRCN ;
               EPLSEQ = LGLSEQ ;
               EPSTAT = 'A' ;
               EPUSER = Q1USER ;
               EPPGM  = Q1PGM ;
               //DSDTTM = %Dec(%Time());
               DSDATE = %Dec(%Date():*usa);
               DSTIME = %Dec(%Time():*hms);
               EPTIME = DSTIME ;
               EPDT   = xxDt7 ;
               EPIUSR = Q1USER ;
               EPIPGM = Q1PGM ;
               EPITIM = DSTIME ;
               EPIDT  = xxDt7 ;

               //************************************************
               // Referring provider license number
               //************************************************
               EPPLIC = DN(695) ;

               //************************************************
               // Referring provider NPI#
               //************************************************
               EPRFNP = DN(699) ;
               EPPHAR = DN(647) ;
               WRITE PEP02;
E01      Endif;


         //*********************************************************
         //  If this is a hospital bill, load the UB extract files.
         //*********************************************************
B01      If EOBTYP = 'H';
           Clear PEU02;
           Chain #HKY PEU01;
               EUCO#  = xxCo# ;
               EURCN  = LGRCN ;
               EULSEQ = LGLSEQ ;

                  XXICD9 = EUDI1 ;
                    Chain DIKY PDI01;
                      If %found(FMDIL) ;
                        EUDIA1 = DIDS1 ;
                        EUADIA = DIDS1 ;
                      Else ;
                        EUDIA1 = *blanks ;
                        EUADIA = *blanks ;
                      Endif ;

                  XXICD9 = EUDI2 ;
                    Chain DIKY PDI01;
                      If %found(FMDIL) ;
                        EUDIA2 = DIDS1 ;
                      Else ;
                        EUDIA2 = *blanks ;
                      Endif ;

                  XXICD9 = EUDI3 ;
                    Chain DIKY PDI01;
                      If %found(FMDIL) ;
                        EUDIA3 = DIDS1 ;
                      Else ;
                        EUDIA3 = *blanks ;
                      Endif ;

                  XXICD9 = EUDI4 ;
                    Chain DIKY PDI01;
                      If %found(FMDIL) ;
                        EUDIA4 = DIDS1 ;
                      Else ;
                        EUDIA4 = *blanks ;
                      Endif ;

                  XXICD9 = EUDI5 ;
                    Chain DIKY PDI01;
                      If %found(FMDIL) ;
                        EUDIA5 = DIDS1 ;
                      Else ;
                        EUDIA5 = *blanks ;
                      Endif ;

                  XXICD9 = EUDI6 ;
                    Chain DIKY PDI01;
                      If %found(FMDIL) ;
                        EUDIA6 = DIDS1 ;
                      Else ;
                        EUDIA6 = *blanks ;
                      Endif ;

                  XXICD9 = EUDI7 ;
                    Chain DIKY PDI01;
                      If %found(FMDIL) ;
                        EUDIA7 = DIDS1 ;
                      Else ;
                        EUDIA7 = *blanks ;
                      Endif ;

                  XXICD9 = EUDI8 ;
                    Chain DIKY PDI01;
                      If %found(FMDIL) ;
                        EUDIA8 = DIDS1 ;
                      Else ;
                        EUDIA8 = *blanks ;
                      Endif ;

                  XXICD9 = EUDI9 ;
                    Chain DIKY PDI01;
                      If %found(FMDIL) ;
                        EUDIA9 = DIDS1 ;
                      Else ;
                        EUDIA9 = *blanks ;
                      Endif ;
               EUSTAT = 'A' ;
               EUUSER =  Q1USER ;
               EUPGM  =  Q1PGM ;
               //DSDTTM =  %Dec(%Time());
               DSDATE = %Dec(%Date():*usa);
               DSTIME = %Dec(%Time():*hms);
               EUTIME =  DSTIME ;
               EUDT   =  xxDt7 ;
               EUIUSR =  Q1USER ;
               EUIPGM =  Q1PGM ;
               EUITIM =  DSTIME ;
               EUIDT  =  xxDt7 ;
               EUCTC2 =  EUCTCD ;
               //************************************************
               // Referring provider license number
               //************************************************
               EURLIC =  DN(695) ;
               EUANPI =  DN(699) ;

               If DN(549) <> *blanks;
                  EUDRG# =  %Dec(DN(549):5:0) ;
               Else;
                  EUDRG# = 0;
               Endif;
         Endif;

         WRITE PEU02;

         //*********************************************************
         //  Load - 837 EDI Bill Data Extract File.
         //*********************************************************
           CLEAR PBL01 ;
           Chain #HKY PBL02 ;
               BLCO#  = LGCO# ;
               BLSUB# = LGSUB#;
               BLFND  = LGFND ;
               BLFYR  = LGFYR ;
               BLCASE = LGCASE ;
               BLEOB# = LGEOB# ;
               BLRCN  = LGRCN ;
               BLSS#  = LGSS# ;
               BLACDT = LGACDT ;
               BLLSEQ = LGLSEQ ;
               BLACCT = DN(523) ;
               If EOBTYP <> 'H' ;
                 BLFAC = Dn(555) ;
               Endif ;
               BLFQUAL = DN(503) ;
               BLCFREQ = DN(505) ;
               BLPAGR  = DN(507) ;
               BLSVDT  = DN(509) ;
               BLPDDT  = DN(512) ;
               BLBLID  = DN(500) ;
               BLTRK#  = DN(500) ;
               BLADDG  = DN(535) ;

               If EOBTYP = 'H';

               //************************************************
               // Loading Facility Code in file FEBLP.
               //************************************************
               BLFAC = Dn(504) ;

               //************************************************
               // Loading Diagnosis code 1 to 8 in file FEBLP.
               //************************************************
                 If EUICDIND = 'Y' ;
                   BLDIA1 = EUICD1001 ;
                   BLDIA2 = EUICD1002 ;
                   BLDIA3 = EUICD1003 ;
                   BLDIA4 = EUICD1004 ;
                   BLDIA5 = EUICD1005 ;
                   BLDIA6 = EUICD1006 ;
                   BLDIA7 = EUICD1007 ;
                   BLDIA8 = EUICD1008 ;
                 else ;
                   BLDIA1 = EUDI1 ;
                   BLDIA2 = EUDI2 ;
                   BLDIA3 = EUDI3 ;
                   BLDIA4 = EUDI4 ;
                   BLDIA5 = EUDI5 ;
                   BLDIA6 = EUDI6 ;
                   BLDIA7 = EUDI7 ;
                   BLDIA8 = EUDI8 ;
                 Endif ;

                 //******************************************************
                 // Loading Outpatient Reason for Visit 1 in file FEBLP.
                 //******************************************************
                 BLOUTR1 = DN(520) ;

                 //******************************************************
                 // Loading Procedure code 1 to 6 in file FEBLP.
                 //******************************************************
                   If EUICDIND = 'Y' ;
                     BLPRC1 = EUPRC1001 ;
                     BLPRC2 = EUPRC1002 ;
                     BLPRC3 = EUPRC1003 ;
                     BLPRC4 = EUPRC1004 ;
                     BLPRC5 = EUPRC1005 ;
                     BLPRC6 = EUPRC1006 ;
                   Else ;
                     BLPRC1 = EUPRC1 ;
                     BLPRC2 = EUPRC2 ;
                     BLPRC3 = EUPRC3 ;
                     BLPRC4 = EUPRC4 ;
                     BLPRC5 = EUPRC5 ;
                     BLPRC6 = EUPRC6 ;
                   Endif ;
                   //***********************************************
                   // Moving all the procedure code dates.
                   //***********************************************
                   BLPPDT1 = %Char(EUPRD1);
                   BLPPDT2 = %Char(EUPRD2);
                   BLPPDT3 = %Char(EUPRD3);
                   BLPPDT4 = %Char(EUPRD4);
                   BLPPDT5 = %Char(EUPRD5);
                   BLPPDT6 = %Char(EUPRD6);

                 //**********************************************
                 // Loading Condition Codes 1 to 7 in file FEBLP.
                 //**********************************************
                   BLCOC1 = EUCOC1 ;
                   BLCOC2 = EUCOC2 ;
                   BLCOC3 = EUCOC3 ;
                   BLCOC4 = EUCOC4 ;
                   BLCOC5 = EUCOC5 ;
                   BLCOC6 = EUCOC6 ;
                   BLCOC7 = EUCOC7 ;
               Endif ;                                                  //If EOBTYP = 'H'

               If EOBTYP = 'N';
               //************************************************
               // Loading Diagnosis code 1 to 4 in file FEBLP.
               //************************************************
                 If EUICDIND = 'Y' ;
                   BLDIA1 = EHICD1001;
                   BLDIA2 = EHICD1002;
                   BLDIA3 = EHICD1003;
                   BLDIA4 = EHICD1004;
                 else ;
                   BLDIA1 = EHDI1;
                   BLDIA2 = EHDI2;
                   BLDIA3 = EHDI3;
                   BLDIA4 = EHDI4;
                 Endif ;
               Endif ;

               If EOBTYP = 'D';
               //************************************************
               // Loading Diagnosis code 1 to 4 in file FEBLP.
               //************************************************
                 If EUICDIND = 'Y' ;
                   BLDIA1 = EDICD1001;
                   BLDIA2 = EDICD1002;
                   BLDIA3 = EDICD1003;
                   BLDIA4 = EDICD1004;
                 else ;
                   BLDIA1 = EDDI1;
                   BLDIA2 = EDDI2;
                   BLDIA3 = EDDI3;
                   BLDIA4 = EDDI4;
                 Endif ;
               Endif ;

               BLADJAMT1 = DN(545);
               BLPRPAID  = DN(760);
               BLUSER    = Q1USER;
               BLPGM     = Q1PGM;
               BLTIME    = %Dec(%Time():*hms);
               BLDT      = %Dec(%Date():*cymd);

               BLIUSR    = Q1USER;
               BLIPGM    = Q1PGM;
               BLITIM    = %Dec(%Time():*hms);
               BLIDT     = %Dec(%Date():*cymd);

             Write PBL01;

         //*********************************************************
         //  Load - 837 EDI Provider Data Extract File.
         //*********************************************************
           CLEAR PPR01 ;
           Chain #HKY PPR02 ;
               PRCO#  = LGCO# ;
               PRSUB# = LGSUB#;
               PRFND  = LGFND ;
               PRFYR  = LGFYR ;
               PRCASE = LGCASE ;
               PREOB# = LGEOB# ;
               PRRCN# = LGRCN ;
               PRSS#  = LGSS# ;
               PRACDT = LGACDT ;
               PRLSEQ = LGLSEQ ;
               PRBLSN = DN(528) ;
               PRBFSN = DN(529) ;
               PRBMID = DN(530) ;
               PRBNPI = DN(634) ;
               PRBTAXO = DN(537) ;
               PRBADR1 = DN(538) ;
               PRBADR2 = DN(539) ;
               PRBCTY  = DN(540) ;
               PRBST   = DN(541) ;
               PRBZIP  = DN(542) ;
               PRBCNTY = DN(569) ;
               PRBFEIN = DN(629) ;
               PRBLIC  = DN(630) ;
               PRRLSN =  DN(638) ;
               PRRFSN =  DN(639) ;
               PRRNPI =  DN(647) ;
               PRRTAXO =  DN(651) ;
               PRRLIC  =  DN(643) ;
               PRFLSN  =  dn(678) ;
               PRFZIP  =  dn(688) ;
               PRRFLSN =  dn(690) ;
               PRRFFSN =  dn(691) ;
               PRRFNPI =  dn(699) ;
               PRRFLIC =  dn(695) ;

               PRUSER    = Q1USER;
               PRPGM     = Q1PGM;
               PRTIME    = %Dec(%Time():*hms);
               PRDT      = %Dec(%Date():*cymd);

               PRIUSR    = Q1USER;
               PRIPGM    = Q1PGM;
               PRITIM    = %Dec(%Time():*hms);
               PRIDT     = %Dec(%Date():*cymd);

             Write PPR01;



           //*************************************************
         //******************************************************
         //  Load detail files.
         //******************************************************
           //*************************************************

         //******************************************************
         // Initial total charges for this recon
         //******************************************************
         TOTCHG = *ZERO ;

         //******************************************************
         //  Process all of the detail charges.
         //******************************************************
         Chain #HKY PCA01;
           If %found(FDCAL13);
B01          Dow not %eof(FDCAL13);
               TOTCHG = CACHRG ;
               //********************************************************
               // Determine if there are amounts in FDDAP for this recon
               // If so then continue else dont write this record
               //********************************************************
               Chain #DAKY PDA01;
                 If %found(FDDAL2) ;
                   XXPAID = DAPAID ;
X02              Else ;
                   XXPAID = 0 ;
E02              Endif;                                                 //IN99=OFF PDA01

               //*********************************************************
               //  Load the EOMB adjustment codes to a hold array, if any.
               //*********************************************************
               CLEAR EOB;
               X += 1 ;
               Chain #DEKY PDE01;
                 If %found(FDDEL3) ;
B02                 Dow not %eof(FDDEL3) and X<= 9 ;
B03                   If DESTAT = 'A';
                         XXKEY = DEEOB ;
                             Chain TBKY PEB03;                          //for FTEBLNC file.
B04                            If %found(FTEBLNC);

B05                               If X = 1;
                                     EOB(10) = EBXREF ;
E05                               Endif;
                 //***************************************************************
                 // KY USES 1ST 3 POS OF THE 2ND DESC FIELD TO HOLD REASON CODE
                 // ALL OTHER STATES WILL USE EBXRF2
                 //***************************************************************

                                EOB(X) = 'W1' ;
                                   Select;
B06                                When X = 1;
                                      EOB(10) = 'OA' ;
                                      CO_COSLAG1 = 'OA';
B06                                When X = 2;
                                      CO_COSLAG2 = 'OA';
B06                                When X = 3;
                                      CO_COSLAG3 = 'OA';
B06                                When X = 4;
                                      CO_COSLAG4 = 'OA';
E06                                Endsl;
                             X += 1 ;
E04                        Endif;

E03                   Endif;                                            //DESTAT IFEQ A

                    ReadE #DEKY PDE01;
E02                 Enddo;
                 Endif ;


           //*************************************************************
           //  If this is a dental bill, load the Dental extract files.
           //*************************************************************
B02        IF EOBTYP = 'D';
             CLEAR PDD02;
             Chain #DKY PDD01;
                 DDCO#  =  xxCo# ;
                 DDRCN  =  LGRCN ;
                 DDLSEQ =  LGLSEQ ;
                 DDLINE =  CALIN# ;
                 DDFRDT =  CAFDAT ;
                 DDTODT =  CATDAT ;
                 DDCHRG =  CACHRG ;
                 DDMRA  =  DAMRA ;
                 DDOTHR =  DAOTHR ;
                 DDOTH2 =  DAOTH2 ;
                 DDNTWK =  DANDIS ;
                 DDPAID =  XXPAID ;
                 DDUNTS =  CAUNTS ;
                 DDEOB1 =  XXEOB1 ;
                 DDEOB2 =  XXEOB2 ;
                 DDEOB3 =  XXEOB3 ;
                 DDEOB4 =  XXEOB4 ;
                 DDEOB5 =  XXEOB5 ;
                 DDEOB6 =  XXEOB6 ;
                 DDEOB7 =  XXEOB7 ;
                 DDEOB8 =  XXEOB8 ;
                 DDEOB9 =  XXEOB9 ;
                 DDEO10 =  XXEO10 ;
                 DDSTAT =  'A' ;
                 DDUSER =  Q1USER ;
                 DDPGM  =  Q1PGM ;
                 //DSDTTM =  %Dec(%Time());
                 DSDATE = %Dec(%Date():*usa);
                 DSTIME = %Dec(%Time():*hms);
                 DDTIME =  DSTIME ;
                 DDDT   =  xxDt7 ;
                 DDIUSR =  Q1USER ;
                 DDIPGM =  Q1PGM ;
                 DDITIM =  DSTIME ;
                 DDIDT  =  xxDt7 ;

                 WRITE PDD02;
E02        ENDIF;

           //***********************************************************
           //  If this is a provider bill, load the HCFA extract files.
           //***********************************************************
B02        IF EOBTYP = 'N';
             CLEAR PDH02;
             Chain #DKY PDH01;
                 DHCO#  = xxCo# ;
                 DHRCN  = LGRCN ;
                 DHLSEQ = LGLSEQ ;
                 DHLINE = CALIN# ;
                 DHFRDT = CAFDAT ;
                 DHTODT = CATDAT ;
                 DHCHRG = CACHRG ;
                 DHMRA  = DAMRA ;
                 DHOTHR = DAOTHR ;
                 DHOTH2 = DAOTH2 ;
                 DHNTWK = DANDIS ;
                 DHPAID = XXPAID ;
                 DHEOB1 = XXEOB1 ;
                 DHEOB2 = XXEOB2 ;
                 DHEOB3 = XXEOB3 ;
                 DHEOB4 = XXEOB4 ;
                 DHEOB5 = XXEOB5 ;
                 DHEOB6 = XXEOB6 ;
                 DHEOB7 = XXEOB7 ;
                 DHEOB8 = XXEOB8 ;
                 DHEOB9 = XXEOB9 ;
                 DHEO10 = XXEO10 ;
                 DHSTAT = 'A' ;
                 DHUSER = Q1USER ;
                 DHPGM  = Q1PGM ;
                 //DSDTTM = %Dec(%Time());
                 DSDATE = %Dec(%Date():*usa);
                 DSTIME = %Dec(%Time():*hms);
                 DHTIME = DSTIME ;
                 DHDT   = xxDt7 ;
                 DHIUSR = Q1USER ;
                 DHIPGM = Q1PGM ;
                 DHITIM = DSTIME ;
                 DHIDT  = xxDt7 ;

                 WRITE PDH02;
E02        ENDIF;

           //****************************************************
           //  If this is a Rx bill, load the Rx extract files.
           //****************************************************
B02        IF EOBTYP = 'X';
             CLEAR PDP02;
             Chain #DKY PDP01;
                 DPCO#  = xxCo# ;
                 DPRCN  = LGRCN ;
                 DPLSEQ = LGLSEQ ;
                 DPLINE = CALIN# ;
                 DPFRDT = CAFDAT ;
                 DPTODT = CATDAT ;
                 DPCHRG = CACHRG ;
                 DPMRA  = DAMRA ;
                 DPOTHR = DAOTHR ;
                 DPOTH2 = DAOTH2 ;
                 DPNTWK = DANDIS ;
                 DPPAID = XXPAID ;
                 DPEOB1 = XXEOB1 ;
                 DPEOB2 = XXEOB2 ;
                 DPEOB3 = XXEOB3 ;
                 DPEOB4 = XXEOB4 ;
                 DPEOB5 = XXEOB5 ;
                 DPEOB6 = XXEOB6 ;
                 DPEOB7 = XXEOB7 ;
                 DPEOB8 = XXEOB8 ;
                 DPEOB9 = XXEOB9 ;
                 DPEO10 = XXEO10 ;
                 DPSTAT = 'A' ;
                 DPUSER = Q1USER ;
                 DPPGM  = Q1PGM ;
                 //DSDTTM = %Dec(%Time());
                 DSDATE = %Dec(%Date():*usa);
                 DSTIME = %Dec(%Time():*hms);
                 DPTIME = DSTIME ;
                 DPDT   = xxDt7 ;
                 DPIUSR = Q1USER ;
                 DPIPGM = Q1PGM ;
                 DPITIM = DSTIME ;
                 DPIDT  = xxDt7;
                 DPTAX  = %Char(CATAX);
                 WRITE PDP02;
E02        ENDIF;

           //********************************************************
           //  If this is a hospital bill, load the UB extract files.
           //********************************************************
B02        IF EOBTYP = 'H';
             CLEAR PDU02;
             Chain #DKY PDU01;
                 DUCO#  = xxCo# ;
                 DURCN  = LGRCN ;
                 DULSEQ = LGLSEQ ;
                 DULINE = CALIN# ;
                 DUFRDT = CAFDAT ;
                 DUTODT = CATDAT ;
                 DUCHRG = CACHRG ;
                 DUMRA  = DAMRA ;
                 DUOTHR = DAOTHR ;
                 DUOTH2 = DAOTH2 ;
                 DUNTWK = DANDIS ;
                 DUPAID = XXPAID ;
                 DUBRVC = DUBPRC ;
                 DUUNTS = CAUNTS ;
                 DUEOB1 = XXEOB1 ;
                 DUEOB2 = XXEOB2 ;
                 DUEOB3 = XXEOB3 ;
                 DUEOB4 = XXEOB4 ;
                 DUEOB5 = XXEOB5 ;
                 DUEOB6 = XXEOB6 ;
                 DUEOB7 = XXEOB7 ;
                 DUEOB8 = XXEOB8 ;
                 DUEOB9 = XXEOB9 ;
                 DUEO10 = XXEO10 ;
                 DUSTAT = 'A' ;
                 DUUSER = Q1USER ;
                 DUPGM  = Q1PGM ;
                 //DSDTTM = %Dec(%Time());
                 DSDATE = %Dec(%Date():*usa);
                 DSTIME = %Dec(%Time():*hms);
                 DUTIME = DSTIME ;
                 DUDT   = xxDt7 ;
                 DUIUSR = Q1USER ;
                 DUIPGM = Q1PGM ;
                 DUITIM = DSTIME ;
                 DUIDT  = xxDt7 ;
                 DUMOD1 = DUPMOD ;
                 DUMOD2 = DUMOD2 ;
                 DUMOD3 = DUMOD3 ;
                 DUMOD4 = DUMOD4 ;
                 WRITE PDU02;
E02        ENDIF;

           //***********************************************************
           //  Load - 837 EDI Common Line Item Detail Data Extract File
           //***********************************************************
           CLEAR PCO02 ;
           Chain #HKY PCO02 ;
            If not %found(FECOML1);
             If DEEOB <> *blanks ;
             //Load initial fields then write.
            CO_COCO#  = CACO# ;
            CO_COSUB# = CASUB#;
            CO_COFND  = CAFND ;
            CO_COFYR  = CAFYR ;
            CO_COCASE = CACASE ;
            CO_COEOB# = CAEOB# ;
            CO_CORCN  = CARCN;
            CO_COSS#  = CASS#;
            CO_COACDT = CAACDT;
            CO_CODSEQ = CADSEQ;
            CO_COLIN# = CALIN#;
            CO_COLSEQ = LGLSEQ;

            CO_COUSER    = Q1USER;
            CO_COPGM     = Q1PGM;
            CO_COTIME    = %Dec(%Time():*hms);
            CO_CODT      = %Dec(%Date():*cymd);

            CO_COIUSR    = Q1USER;
            CO_COIPGM    = Q1PGM;
            CO_COITIM    = %Dec(%Time():*hms);
            CO_COIDT     = %Dec(%Date():*cymd);

               //************************************************************
               // Determine if there are amounts in FDDAP for this recon - 1.
               //************************************************************
               If Lgrcn > 0;
               LGRCN -= 1;
               Chain #DAKY PDA01;
                 if %found(FDDAL2) ;
                    CO_CopPAID = %Char(DAPAID) ;
X02              Else ;
                    CO_CopPAID = *Blanks;
E02              Endif;                                                 //IN99=OFF PDA01
X02            Else ;
                 CO_CopPAID = *Blanks;
E02            Endif;

               // COPPAID = DAPAID ;  (needs to be loded from last rcon)
               // Chain file FDDAP with Rcon as last key and then use Readpe
               // without rcon key.
             Write PCO02;
             EndIf ;
            EndIf ;


             Reade #HKY PCA01;
E01        Enddo;
         Endif;
       Endsr;



       //========================================================
       //  CmpDry - Complete the extract error diary if it exists.
       //========================================================
       Begsr CmpDry;

         //******************************************************
         //  Call API to build DARTS key
         //******************************************************
           #1CO#  = %Char(LGCO#);
           #1SUB# = %Char(LGSUB#);
           #1FND  = %Char(LGFND);
           #1FYR  = %Char(LGFYR);
           #1CASE = %Char(LGCASE);
           ##RTCD = 'K';
           ##KEY  = *blanks;
           ##PNAM = *blanks;

           BldDartKey(##RTCD:##WCIN:##WCDK:#1CO#:#1SUB#:#1FND:
                     #1FYR:#1CASE:##KEY:##PNAM);

        //*********************************************************
        //  Retrieve the extract diary record and complete it.
        //*********************************************************
B01        If LGDS01 > 0;
              xxDSEQ = LGDS01;
                 Chain sdKEY PCM02;
                    If %found(SDCMLA);
                       CMDCDT = 10000.01 * UDATE;
                          Update PCM02;
E02                 Endif;
E01        Endif;
         //
       EndSr;

         //****************************************************************
         //  ErrDry - Create the extract error diary if it does not exist.
         //****************************************************************
       Begsr ErrDry;

         //**************************************************************
         //  Determine if the error diary alReady exists.
         //**************************************************************
B01         If LGDS01 > 0;
               xxDSEQ = LGDS01;
                  Chain sdKEY PCM02;
                     If %found(SDCMLA);
B03                     If CMDCDT > 0;
                           CMDCDT = 0;

                             Update PCM02;
E03                     Endif;

                        SVSEQ# = %Dec(LGDS01:5:0);
      /End-Free
     C                   GOTO      ENDDRY
      /FREE
E02                  Endif;
E01         Endif;

         //******************************************************
         //  Call API to build DARTS key
         //******************************************************
             #1CO#  = %Char(LGCO#);
             #1SUB# = %Char(LGSUB#);
             #1FND  = %Char(LGFND);
             #1FYR  = %Char(LGFYR);
             #1CASE = %Char(LGCASE);

           BldDartKey(##RTCD:##WCIN:##WCDK:#1CO#:#1SUB#:#1FND:
                     #1FYR:#1CASE:##KEY:##PNAM);

        //*********************************************************
        //  Create the diary if the EDI control field exists.
        //*********************************************************
B01      If EXDR01 <> *BLANKS;

B02        If EX#D01 = *ZEROS;
              ##CDAT = %Char(DSDATE);
X02        Else;
              SRDATE = DSDATE;
              SRDAYS = EX#D01;
                GetFutDat(SRDATE:SRDAYS:srErr);
              ##CDAT = %Char(SRDATE);
E02        Endif;

           ##DCID = 'F0350C';
           ##SECL = *ZEROS;

           Clear EOBKEY;
             CO#    = %Char(LGCO#);
             SUB#   = %Char(LGSUB#);
             FND    = %Char(LGFND);
             FYR    = %Char(LGFYR);
             CASE   = %Char(LGCASE);
             EOB#   = %Char(LGEOB#);
             ##FOLD = EOBKEY;
             #1CASE = *blanks;

             ##COMM = *BLANKS;
             ##USER = EOIUSR;
             ##PGM  = Q1PGM;
             ##DIFL = 'Y';
             ##IPGM = Q1PGM;
             ##FL01 = 'Y';
             ##FL02 = 'Y';
             ##RMCD = EXDR01;

             WrtDartRmk(##KEY:##COMM:##CDAT:##DCID:##PNAM:##SECL:
                        ##USER:##PGM:##FOLD:##DIFL:##IPGM:##FL01:
                        ##FL02:##RMCD:##SEQ#);

             If ##SEQ# <> *blanks;
               SVSEQ# = %Dec(##SEQ#:5:0);
             Else;
               SVSEQ# = 0;
             Endif;

E01      Endif;

      /End-Free
     C     ENDDRY        ENDSR

        //****************************************************************
        //  ErrFil - This subroutine loads the extract error reason file.
        //****************************************************************
      /FREE
          Begsr ErrFil;

              E1CO#  =   xxCo#;
              E1SUB# =   LGSUB#;
              E1FND  =   LGFND;
              E1FYR  =   LGFYR;
              E1CASE =   LGCASE;
              E1EOB# =   LGEOB#;
              E1RCN  =   LGRCN;
              E1STS  =   LGSTS;
              E1TSET =   LGTSET;
              E1JST  =   LGJST;
              E1MTC  =   LGMTC;
              E1LSEQ =   LGLSEQ;
              E1ETYP =   'E';
              E1DN#  =   DNDN#;
              E1DND  =   DNDESC;
              E1MSG# =   xxErrS;
              E1MSGD =   ERDESC;
              E1STAT =   'A';

              E1USER = Q1USER;
              E1PGM  = Q1PGM;
              //DSDTTM = %Dec(%Time());
              DSDATE = %Dec(%Date():*usa);
              DSTIME = %Dec(%Time():*hms);
              E1TIME = DSTIME;
              E1DT   = xxDt7;
              E1IUSR = Q1USER;
              E1IPGM = Q1PGM;
              E1ITIM = DSTIME;
              E1IDT  = xxDt7;

            Write PE101;

          Endsr;

       //================================================================
       //  THIS SUBROUTINE LOADS ADDITIONAL INFORMATION FOR THE DETAIL
       //  REPORT AND PRINTS THE DETAIL LINES.
       //================================================================
       Begsr RptDet;

         //**************************************************************
         //  LOAD THE DETAIL INFORMATION
         //**************************************************************
           xxCNAM = %Trim(Dn(43)) + ',' + %Trim(Dn(43)) + ' ' + %Trim(DN(44));
           If DN(31) <> *blanks;
             xxACDT = %Dec(DN(31):8:0);
           Else;
             xxACDT = 0;
           Endif;

       //*****************************************************************
       //  PRINT THE DETAIL LINES
       //*****************************************************************
B01      If xtEUsr <> xxEUsr;
           Except Head;
             XXEUSR = xtEUsr;
             *inOA  = '0';
E01      Endif;

         If *inOA = *On;
           Except Head;
         Endif;

         Except DET;
         *in91 = '1';

        Endsr;
       //****************************************************************
       //  THIS SUBROUTINE LOADS ADDITIONAL INFORMATION FOR THE
       //  ERROR REJECTION REPORT AND HANDLES THE PRINTING OF THE
       //  DETAIL LINES.
       //****************************************************************
       Begsr RpDet1;

         //**************************************************************
         //  LOAD THE DETAIL INFORMATION
         //**************************************************************
           xxCNAM = %Trim(Dn(43)) + ',' + %Trim(Dn(43)) + ' ' + %Trim(DN(44));
           If DN(31) <> *blanks;
             xxACDT = %Dec(DN(31):8:0);
           Else;
             xxACDT = 0;
           Endif;

       //*********************************************************
       //  GET THE DATA ELEMENT # AND DESCRIPTION
       //*********************************************************
           xxDN#  = DNDN#;
           xxDNDS = DNDESC;

       //*********************************************************
       //  GET THE ERROR DESCRIPTION.
       //*********************************************************
           If xxErrS <> *blanks;
             xxErr# = %Dec(xxErrS:3:0);
           Else;
             xxErr# = 0;
           Endif;

           xxEDSC = *blanks;

           Chain erKY PER01;
              If %found(FTERL);
                 xxEDSC = ERDESC;
              Endif;

       //*************************************************************
       //   PRINT THE SUB HeadING IF NECESSARY, THEN PRINT THE DETAIL.
       //*************************************************************
B01      If XTEUSR <> X1EUSR;
            Except Head1;
              x1EUSR = xtEUsr;
              *inOB  = '0';
E01      Endif;

B01      If *inOB = '1';
            Except Head1;
               xxErr = 'N';
E01      Endif;

         *in92 = '1';

B01      If xxErr = 'N';
           Except SUBH1;
E01      Endif;

         Except DET1;

       Endsr;

       //******************************************************************
       //  This subroutine loads the extract workfile so the hierarchy will
       //  be in group/employer/case/EOB order.
       //******************************************************************
       Begsr LdWrkF;

         //****************************************************************
         //  Read all 837 Log records for the entered jurisdiction ST
         //****************************************************************
         Chain l2KY PLG06;
B01      Dow not %eof(FELGL6);

           //**************************************************************
           //  Check selection criteria
           //**************************************************************
               If LGTSET <> '837' or LGSTAT = 'H' or LGIDT > xxEXDT;
      /End-Free
     C                   GOTO      Skip1
      /FREE
               Endif;

B02            If LGSTS <> 'EI'                                         //MUST NOT HAVE BEEN
               and LGSTS <> 'EC';                                       // SENT ALReadY
      /END-FREE
     C                   GOTO      SKIP1
      /FREE
E02            Endif;

           //**************************************************************
           //  Retrieve the EOMB entry user's name
           //**************************************************************
           Chain #hKY PEO01;
              If not %found(FAEOLG);
                 EOIUSR = *blanks;
              Endif;

       //*******************************************************************
       //  LOAD THE RECORD INTO THE EXTRACT WORKFILE
       //*******************************************************************
              xtCO#  = xxCo#;
              xtSUB# = LGSUB#;
              xtFND  = LGFND;
              xtFYR  = LGFYR;
              xtCASE = LGCASE;
              xtEOB# = LGEOB#;
              xtLSEQ = LGLSEQ;
              xtTSET = LGTSET;
              xtSORT = EOIUSR;
              xtEUSR = EOIUSR;
              xtDUE  = LGDUE;
              xtUSER = Q1USER;
              xtPGM  = Q1PGM;
              //DSDTTM = %Dec(%Time());
              DSDATE = %Dec(%Date():*usa);
              DSTIME = %Dec(%Time():*hms);
              xtTIME = DSTIME;
              xtDT   = xxDt7;

           Write PXT01;

           //****************************************************
           //  PROCESS NEXT LOG RECORD
           //****************************************************
      /END-FREE
     C     SKIP1         TAG
      /FREE

           Reade l2KY PLG06;
E01      Enddo;

       Endsr;
       //*************************************************************************
      /END-FREE
     OQSYSPRT   E            ROUTE          1 06
     O                                            8 'XXXXXXXX'
     O                                           20 'XXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXX     XX'
     O                                           77 'XXXXXX'
     O          E            ROUTE          1
     O                                            9 'XXXXXXXXX'
     O                                           21 'XXXXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXXX    XX'
     O                                           78 'XXXXXXXX'
     O          E            ROUTE          1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XXXX    XX'
     O                                           79 'XX      XX'
     O          E            ROUTE          1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            ROUTE          1
     O                                            9 'XXXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            ROUTE          1
     O                                            8 'XXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           71 'XX'
     O          E            ROUTE          1
     O                                            4 'XXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           79 'XX    XXXX'
     O          E            ROUTE          1
     O                                            5 'XXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX    XXXX'
     O          E            ROUTE          1
     O                                            6 'XX XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX      XX'
     O          E            ROUTE          1
     O                                            7 'XX  XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX    XXXX'
     O                                           79 'XX      XX'
     O          E            ROUTE          1
     O                                            8 'XX   XXX'
     O                                           21 'XXXXXXXX'
     O                                           33 'XXXXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX    XXXX'
     O                                           78 'XXXXXXXX'
     O          E            ROUTE          3
     O                                            9 'XX    XXX'
     O                                           20 'XXXXXX'
     O                                           32 'XXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX     XXX'
     O                                           77 'XXXXXX'
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
     O          E            ROUTE          2
     O                                           19 'PROGRAM NAME:'
     O                       ORPNAM              30
     O                                           36 'FILE:'
     O                       ORFIL#              38
     O                                           46 'COPIES:'
     O                       ORCOPY        Z     50
     O                                           56 'HOLD:'
     O                       ORHOLD              61
     O          E            ROUTE          2
     O                                           19 'OUTPUT QUEUE:'
     O                       OROUTQ              30
     O                                           37 'FORMS:'
     O                       ORFORM              48
     O                                           56 'SAVE:'
     O                       ORSAVE              61
      //**     E   60           ROUTE
      //**                                23 '(C) COPYRIGHT PROGRESSI'
      //**                                46 'VE DATA SOLUTIONS. INC,'
      //**                                51 '.1999'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      //
     O          E            Head           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             118
     O                       DSDATE        Y    198
     O          E            Head           2
     O                       Q1PGM               10
     O                       XXTITL             129
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            Head           2
     O                                           19 'JURISDICTION STATE:'
     O                       qqjSt               22
     O                                           53 'EXTRACT SUBMISSION DATE:'
     O                       x1ExDt              64 '  /  /    '
     O                                           85 'EOMB ENTRY USER:'
     O                       XTEUSR              96
     O          E            Head           0
     O                                            9 'EOB #'
     O                                           27 'CARRIER CLAIM #'
     O                                           49 'CLAIMANT NAME'
     O                                           78 'SOC SEC #'
     O                                           93 'DT OF INJURY'
     O                                          109 'PROVIDER ID #'
     O                                          131 'DATES OF SERVICE'
     O                                          150 'AMOUNT BILLED'
     O                                          165 'AMOUNT PAID'
     O          E            Head           2
     O                                            9 '_________'
     O                                           33 '_____________________'
     O                                           60 '________________________'
     O                                           66 '______'
     O                                           78 '_________'
     O                                           93 '____________'
     O                                          109 '_____________'
     O                                          134 '______________________'
     O                                          150 '_____________'
     O                                          165 '___________'
     O          E            DET            1
     O                       LGEOB#               9
     O                       XCACLC              33
     O                       XXCNAM              66
     O                       CDSS#               78
     O                       XXACDT              92 '  /  /    '
     O                       EOFEIN             105
     O                       EOMOD              109
     O                       XXFRDT             122 '  /  /    '
     O                       XXTODT             134 '  /  /    '
     O                       EOCHRG        J    151
     O                       EOPAID        J    166
     O          E            MSG         1
     O                                           98 '*** NO RECORDS MEET '
     O                                          119 'SELECTED CRITERIA ***'
      //*************************************************************************
      //  EDI 837 ERROR REJECTION REPORT
     OQSYSPRT1  E            ROUTE1         1 06
     O                                            8 'XXXXXXXX'
     O                                           20 'XXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXX     XX'
     O                                           77 'XXXXXX'
     O          E            ROUTE1         1
     O                                            9 'XXXXXXXXX'
     O                                           21 'XXXXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXXX    XX'
     O                                           78 'XXXXXXXX'
     O          E            ROUTE1         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XXXX    XX'
     O                                           79 'XX      XX'
     O          E            ROUTE1         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            ROUTE1         1
     O                                            9 'XXXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            ROUTE1         1
     O                                            8 'XXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           71 'XX'
     O          E            ROUTE1         1
     O                                            4 'XXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           79 'XX    XXXX'
     O          E            ROUTE1         1
     O                                            5 'XXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX    XXXX'
     O          E            ROUTE1         1
     O                                            6 'XX XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX      XX'
     O          E            ROUTE1         1
     O                                            7 'XX  XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX    XXXX'
     O                                           79 'XX      XX'
     O          E            ROUTE1         1
     O                                            8 'XX   XXX'
     O                                           21 'XXXXXXXX'
     O                                           33 'XXXXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX    XXXX'
     O                                           78 'XXXXXXXX'
     O          E            ROUTE1         3
     O                                            9 'XX    XXX'
     O                                           20 'XXXXXX'
     O                                           32 'XXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX     XXX'
     O                                           77 'XXXXXX'
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       X1TITL              80
     O          E            ROUTE1         2
     O                                           19 'PROGRAM NAME:'
     O                       ORPNAM              30
     O                                           36 'FILE:'
     O                       ORFIL#              38
     O                                           46 'COPIES:'
     O                       ORCOPY        Z     50
     O                                           56 'HOLD:'
     O                       ORHOLD              61
     O          E            ROUTE1         2
     O                                           19 'OUTPUT QUEUE:'
     O                       OROUTQ              30
     O                                           37 'FORMS:'
     O                       ORFORM              48
     O                                           56 'SAVE:'
     O                       ORSAVE              61
      //**     E   60           ROUTE1
      //**                                23 '(C) COPYRIGHT PROGRESSI'
      //**                                46 'VE DATA SOLUTIONS. INC,'
      //**                                51 '.1999'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      //
     O          E            Head1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             118
     O                       DSDATE        Y    198
     O          E            Head1          2
     O                       Q1PGM               10
     O                       X1TITL             129
     O                                          192 'PAGE:'
     O                       PAGE1         Z    198
     O          E            Head1          2
     O                                           19 'JURISDICTION STATE:'
     O                       qqjSt               22
     O                                           53 'EXTRACT SUBMISSION DATE:'
     O                       x1ExDt              64 '  /  /    '
     O                                           85 'EOMB ENTRY USER:'
     O                       XTEUSR              96
     O          E            Head1          0
     O                                            9 'EOB #'
     O                                           27 'CARRIER CLAIM #'
     O                                           49 'CLAIMANT NAME'
     O                                           78 'SOC SEC #'
     O                                           93 'DT OF INJURY'
     O                                          109 'PROVIDER ID #'
     O                                          131 'DATES OF SERVICE'
     O                                          150 'AMOUNT BILLED'
     O                                          165 'AMOUNT PAID'
     O          E            Head1          1
     O                                            9 '_________'
     O                                           33 '_____________________'
     O                                           60 '________________________'
     O                                           66 '______'
     O                                           78 '_________'
     O                                           93 '____________'
     O                                          109 '_____________'
     O                                          134 '______________________'
     O                                          150 '_____________'
     O                                          165 '___________'
     O          E            Head1          0
     O                                            7 'DN#'
     O                                           33 'DATA ELEMENT DESCRIPTION'
     O                                           55 'ERR#'
     O                                           74 'ERROR DESCRIPTION'
     O          E            Head1          1
     O                                            7 '___'
     O                                           29 '____________________'
     O                                           49 '____________________'
     O                                           55 '____'
     O                                           77 '____________________'
     O                                           87 '__________'
     O          E            SUBH1       1  1
     O                       LGEOB#               9
     O                       XCACLC              33
     O                       XXCNAM              66
     O                       CDSS#               78
     O                       XXACDT              92 '  /  /    '
     O                       EOFEIN             105
     O                       EOMOD              109
     O                       XXFRDT             122 '  /  /    '
     O                       XXTODT             134 '  /  /    '
     O                       EOCHRG        J    151
     O                       EOPAID        J    166
      //
     O          E            DET1           1
     O                       XXDN#         4      7
     O                       XXDNDS              49
     O                       xxErr#        4     54
     O                       XXEDSC              87
      //
     O          E            MSG1        1
     O                                           98 '*** NO RECORDS MEET '
     O                                          119 'SELECTED CRITERIA ***'
      //
      //*************************************************************************
      //  EDI 837 SUMMARY REPORT
     OQSYSPRT2  E            ROUTE2         1 06
     O                                            8 'XXXXXXXX'
     O                                           20 'XXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXX     XX'
     O                                           77 'XXXXXX'
     O          E            ROUTE2         1
     O                                            9 'XXXXXXXXX'
     O                                           21 'XXXXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXXX    XX'
     O                                           78 'XXXXXXXX'
     O          E            ROUTE2         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XXXX    XX'
     O                                           79 'XX      XX'
     O          E            ROUTE2         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            ROUTE2         1
     O                                            9 'XXXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            ROUTE2         1
     O                                            8 'XXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           71 'XX'
     O          E            ROUTE2         1
     O                                            4 'XXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           79 'XX    XXXX'
     O          E            ROUTE2         1
     O                                            5 'XXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX    XXXX'
     O          E            ROUTE2         1
     O                                            6 'XX XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX      XX'
     O          E            ROUTE2         1
     O                                            7 'XX  XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX    XXXX'
     O                                           79 'XX      XX'
     O          E            ROUTE2         1
     O                                            8 'XX   XXX'
     O                                           21 'XXXXXXXX'
     O                                           33 'XXXXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX    XXXX'
     O                                           78 'XXXXXXXX'
     O          E            ROUTE2         3
     O                                            9 'XX    XXX'
     O                                           20 'XXXXXX'
     O                                           32 'XXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX     XXX'
     O                                           77 'XXXXXX'
     O          E            ROUTE2         2
     O                                           19 'REPORT TITLE:'
     O                       X2TITL              80
     O          E            ROUTE2         2
     O                                           19 'PROGRAM NAME:'
     O                       ORPNAM              30
     O                                           36 'FILE:'
     O                       ORFIL#              38
     O                                           46 'COPIES:'
     O                       ORCOPY        Z     50
     O                                           56 'HOLD:'
     O                       ORHOLD              61
     O          E            ROUTE2         2
     O                                           19 'OUTPUT QUEUE:'
     O                       OROUTQ              30
     O                                           37 'FORMS:'
     O                       ORFORM              48
     O                                           56 'SAVE:'
     O                       ORSAVE              61
      //**     E   60           ROUTE2
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      //
     O          E            Head2          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             118
     O                       DSDATE        Y    198
     O          E            Head2          2
     O                       Q1PGM               10
     O                       X2TITL             129
     O                                          192 'PAGE:'
     O                       PAGE2         Z    198
     O          E            Head2          2
     O                                           19 'JURISDICTION STATE:'
     O                       qqjSt               22
     O                                           53 'EXTRACT SUBMISSION DATE:'
     O                       x1ExDt              64 '  /  /    '
      //
     O          E            DET2        3  2
     O                                           20 'TOTAL 837 RECORDS'
     O                                           30 'ACCEPTED:'
     O                       XXACPT        3     41
     O          E            DET2           2
     O                                           20 'TOTAL 837 RECORDS'
     O                                           30 'REJECTED:'
     O                       XXREJ         3     41
     O          E            DET2           2
     O                                           21 'TOTAL 837 RECORDS:'
     O                       XX837T        3     41
**  QCMDEXC TABLE
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
