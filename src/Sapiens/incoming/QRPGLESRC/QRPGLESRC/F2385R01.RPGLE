     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE F2385R01 - BILL ENTRY ERROR MESSAGES UPDATE/DISPLAY
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program updates and displays the Bill Entry Error  **
     H**     Message file.                                           **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     F2385R - Work with Bill Entry Error Messages            **
     H**   CALLS TO:                                                 **
     H**     F2493R - Generic Table Search                           **
     H**                                                             **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   10/28/97      PROJECT: FS139                   **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  06/15/20 C:079 Support: PHW89430
     H*                 Load MIST and CK fields for every record
     H*
     H*  01/14/15 C:079 Project: SY394
     H*                 Load new field MIST
     H*
     H*  06/11/12 C:277Project: 114MER
     H*                Add Screen 2 Field for state 'CK'
     H*
     H*  08/07/00 C:79 Project: MCMNT
     H*                Allow change of source on F6=Add and 2=Change
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      09   Reserved for maintenance/inquiry conditions
     H**      40   Add mode
     H**      41   Call was for State Message update
     H**      60   Used for chaining
     H**      71   MCR0502 Error - Cannot be < or = zero (Message #)
     H**      72   MCR0503 Error - Cannot be blank (Description 1)
     H**      73   MCR0900 Error - Record already exists (Message #)
     H**      74   MCR0503 Error - Cannot be blank (Description 0)
     H**      76 - 89 are reserved for error screen error indicators
     H**      90   Used as general purpose error indicator
     H**      99   Used when chaining to file being updated
     H**
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      FTGTP  (PGT01)  Generic Table File physical            **
     H**      FTGTL  (PGT02)  Generic Table File (CO#/TBID/KEY)      **
     H**      FTMIP  (DBRCDF) Bill Entry Error Message               **
     H**      FTMIL  (DBRCD2) Bill Entry Error Message (MSG#/MSEQ)   **
     H**      STABL  (PST01)  System Wide State Code Table (KEY)     **
     H*****************************************************************
     H*
     FF2385F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     FFTMIP     UF A E             DISK
     F                                     RENAME(PMI01:DBRCDF)
     FFTMIL     UF A E           K DISK
     F                                     RENAME(PMI01:DBRCD2)
     FSTABL     IF   E           K DISK
     F*** Beg Add *** 06/11/12 *************************************************
     FFTEBLCK   IF   E           K DISK
     F*** End Add *** 06/11/12 *************************************************
      /COPY SCOPYMT01D
     D*
     D*
     D*** Beg Del *** 06/11/12 **************************************************
     D***/COPY SCOPYMT01I
     D*** End Del *** 06/11/12 **************************************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Bill Entry Error Mes-
     D                                     sages')
     D COMAPI          C                   CONST('FA007R')
     D*** Beg Chg *** 06/11/12 *************************************************
     D*** #OFSCR          C                   CONST(1)
     D #OFSCR          S              1  0
     D*** End Chg *** 06/11/12 *************************************************
     D
     D*** Beg Add *** 06/11/12 *************************************************
     D KKCO#           S                   Like(EBCO#)
     D KKKEY1          S                   Like(EBKEY)
     D HLCKPS          S                   Like(SCCKPS)
     D HLCKHS          S                   Like(SCCKHS)
     D HLCKE1          S                   Like(SCCKE1)
     D HLCKE2          S                   Like(SCCKE2)
     D HLCKCO          S                   Like(SCCKCO)
     D HLCKDM          S                   Like(SCCKDM)
     D HLCKDF          S                   Like(SCCKDF)
     D HLCKAS          S                   Like(SCCKAS)
     D HLCKAA          S                   Like(SCCKAA)
     D HLCKSX          S                   Like(SCCKSX)
     D HLCKAG          S                   Like(SCCKAG)
     D HLCKEP          S                   Like(SCCKEP)
     D HLCKOB          S                   Like(SCCKOB)
     D HLCKET          S                   Like(SCCKET)
     D HLCKCA          S                   Like(SCCKCA)
     D HLCKHA          S                   Like(SCCKHA)
     D HLEOB           S                   Like(SCEOB )
     D inTblId         S              4
     D inCall          S              2
     D inKc            S              1
     D inKl            S              1
     D outRrn          S              9  0
     D inCal           S              1
     D inSt            S              2
     D OutKey          S             15
     D*** End Add *** 06/11/12 *************************************************
     D*
     D* ---------------------------------------------------------------
     D* SCDATA For message number sequence
     D* ---------------------------------------------------------------
     D                 DS
     D  DESC                   1    350
     D                                     DIM(5)                               Text Lines
     D  SCDES1                 1     70
     D  SCDES2                71    140
     D  SCDES3               141    210
     D  SCDES4               211    280
     D  SCDES5               281    350
     D*** Beg Add *** 06/11/12 **************************************************
     D RtvTblId        pr                  extpgm('F2380R')
     D inCall                         2
     D inKc                           1
     D inKl                           1
     D outRrn                         9  0
     D inTblId                        4
     D*
     D GenTblId        pr                  extpgm('F2330R')
     D inCal                          1
     D inKc                           1
     D inKl                           1
     D inSt                           2
     D OutKey                        15
     D*** End Add *** 06/11/12 **************************************************
     C/COPY SCOPYMT01C
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    ##STCD            2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*          FTMIL  (DBRCDF)  Bill Entry Error Message (MSEQ/MSEQ)
     C     KEY1          KLIST
     C                   KFLD                    KKMSG#
     C                   KFLD                    KKMSEQ
     C*
     C*          FTGTL  (PGT02)  Generic Table File (CO#/KEY)
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C*          STABL   (PST01)  System Wide State Code Table (KEY)
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*** Beg Add *** 06/11/12 ****************************************
     C*        FTEBPCK   CK: EOMB Descriptions Table File
     C     EBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY1
     C*** End Add *** 06/11/12 ****************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C*** Beg Del *** 06/11/12 **************************************************
     C***/COPY SCOPYMT01C
     C*** End Del *** 06/11/12 **************************************************
     C     *LIKE         DEFINE    MIMSG#        KKMSG#
     C     *LIKE         DEFINE    MIMSEQ        KKMSEQ
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    STKEY         KKKEY
     C     *LIKE         DEFINE    SCTYPE        HLTYPE
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C                   DO
     C*
     C*          Validate Message number
     C*          ------------------------------------------------------
     C     SCMSG#        IFEQ      *BLANKS
     C                   SETON                                        7190
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  MSG# = Blanks
     C*
     C     *IN40         IFEQ      *ON
     C                   MOVEL     'PC'          KKMSG#
     C   41              MOVEL     ##STCD        KKMSG#
     C                   MOVE      SCMSG#        KKMSG#
     C                   Z-ADD     0             KKMSEQ
     C     KEY1          CHAIN     DBRCD2                             60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        7390
     C                   EXCEPT    FTMILE
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  *IN60 = Off s
     C                   ENDIF                                                  *IN40 = On
     C*
     C*          Validate Description 0
     C*          ------------------------------------------------------
     C     SCDESC        IFEQ      *BLANKS
     C                   SETON                                        7490
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  DESC = Blanks
     C*
     C*          Validate Description 1
     C*          ------------------------------------------------------
     C     SCDES1        IFEQ      *BLANKS
     C                   SETON                                        7290
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  DES1 = Blanks
     C*
     C                   ENDDO                                                  DO Loop
     C*          Validate source
     C*          ------------------------------------------------------
     C* Required field
     C     SCTYPE        IFEQ      *BLANK
     C                   SETON                                        7590
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C* Validate in the Generic table
     C*
     C                   MOVEL(P)  'ERMG'        KKTBID
     C                   MOVEL(P)  SCTYPE        KKKEY
     C*
     C     GTKY          CHAIN     PGT02                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        7690
     C                   GOTO      CK1END
     C                   ENDIF
     C*** Beg Add *** 06/11/12 ****************************************
      /Free
       If *in90 <> *on;
       scDes = scDesc;
       scDe1 = scDes1;
       Endif;
      /End-Free
     C*** End Add *** 06/11/12 ****************************************
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*** Beg Add *** 06/11/12 ****************************************
      /Free

        //ClaimCheck Payment Status - Validate in the Generic table
         If scCkps <> *blanks;
            kkTbid = 'CKPS';
            kkKey = scCkps;
            Chain gtky pgt02;
              If not %Found(FTGTL);
                *in45 = *on;
                *in77 = *on;
                *in90 = *on;
                leavesr;
              Endif;
         Endif;

        //ClaimCheck History Indicator - Validate in the Generic table
         If ScCkHs <> *blanks;
            kkTbid = 'CKHS';
            kkKey = scCkhs;
            Chain gtky pgt02;
              If not %Found(FTGTL);
                *in46 = *on;
                *in78 = *on;
                *in90 = *on;
                 leavesr;
              Endif;
         Endif;

        //ClaimCheck SCE Error1 - Validate in the Generic table
         If scCke1 <> *blanks;
            kkTbid = 'CKE1';
            kkKey = scCke1;
            Chain gtky pgt02;
              If not %Found(FTGTL);
                *in47 = *on;
                *in79 = *on;
                *in90 = *on;
                leavesr;
              Endif;
         Endif;

        //ClaimCheck SCE Error2 - Validate in the Generic table
         If scCke2 <> *blanks;
            kkTbid = 'CKE2';
            kkKey = scCke2;
            Chain gtky pgt02;
              If not %Found(FTGTL);
                *in48 = *on;
                *in80 = *on;
                *in90 = *on;
                leavesr;
              Endif;
         Endif;

        //ClaimCheck Code Origination - Validate in the Generic table
         If scCkco <> *blanks;
            kkTbid = 'CKCO';
            kkKey = scCkco;
            Chain gtky pgt02;
              If not %Found(FTGTL);
                 *in49 = *on;
                 *in81 = *on;
                 *in90 = *on;
                 leavesr;
              Endif;
         Endif;

        //ClaimCheck Duplicate Message Type - Validate in the Generic table
         If scCkdm <> *blanks;
            kkTbid = 'CKDM';
            kkKey = scCkdm;
            Chain gtky pgt02;
              If not %Found(FTGTL);
                *in50 = *on;
                *in82 = *on;
                *in90 = *on;
                leavesr;
              Endif;
         Endif;

        //ClaimCheck Duplicate Flag - Validate in the Generic table
         If scCkdf <> *blanks;
            kkTbid = 'CKDF';
            kkKey = scCkdf;
            Chain gtky pgt02;
              If not %Found(FTGTL);
                *in51 = *on;
                *in83 = *on;
                *in90 = *on;
                leavesr;
              Endif;
         Endif;

        //ClaimCheck Assistant Surgeon Status - Validate in the Generic table
         If scCkas <> *blanks;
            kkTbid = 'CKAS';
            kkKey = scCkas;
            Chain gtky pgt02;
              If not %Found(FTGTL);
                *in52 = *on;
                *in84 = *on;
                *in90 = *on;
                leavesr;
              Endif;
         Endif;

        //ClaimCheck Assistant at Surgery Status - Validate in the Generic table
         If scCkaa <> *blanks;
            kkTbid = 'CKAA';
            kkKey = scCkaa;
            Chain gtky pgt02;
              If not %Found(FTGTL);
                *in53 = *on;
                *in85 = *on;
                *in90 = *on;
                leavesr;
              Endif;
         Endif;

        //ClaimCheck Sex Indicator - Validate in the Generic table
         If scCksx <> *blanks;
            kkTbid = 'CKSX';
            kkKey = scCksx;
            Chain gtky pgt02;
              If not %Found(FTGTL);
                *in54 = *on;
                *in86 = *on;
                *in90 = *on;
                leavesr;
              Endif;
         Endif;

        //ClaimCheck Age Status - Validate in the Generic table
         If scCkag <> *blanks;
            kkTbid = 'CKAG';
            kkKey = scCkag;
            Chain gtky pgt02;
              If not %Found(FTGTL);
                *in55 = *on;
                *in87 = *on;
                *in90 = *on;
                leavesr;
              Endif;
         Endif;

        //ClaimCheck EXP Status - Validate in the Generic table
         If scCkep <> *blanks;
            kkTbid = 'CKEP';
            kkKey = scCkep;
            Chain gtky pgt02;
              If not %Found(FTGTL);
                *in56 = *on;
                *in88 = *on;
                *in90 = *on;
                leavesr;
              Endif;
         Endif;

        //ClaimCheck OBS Status - Validate in the Generic table
         If scCkob <> *blanks;
            kkTbid = 'CKOB';
            kkKey = scCkob;
            Chain gtky pgt02;
              If not %Found(FTGTL);
                *in57 = *on;
                *in89 = *on;
                *in90 = *on;
                leavesr;
              Endif;
         Endif;

        //ClaimCheck Edit Type - Validate in the Generic table
         If scCket <> *blanks;
            kkTbid = 'CKET';
            kkKey = scCket;
            Chain gtky pgt02;
              If not %Found(FTGTL);
                *in58 = *on;
                *in72 = *on;
                *in90 = *on;
                leavesr;
              Endif;
         Endif;

        //ClaimCheck Current Action - Validate in the Generic table
         If scCkca <> *blanks;
            kkTbid = 'CKCA';
            kkKey = scCkca;
            Chain gtky pgt02;
              If not %Found(FTGTL);
                *in59 = *on;
                *in74 = *on;
                *in90 = *on;
                leavesr;
              Endif;
         Endif;

        //ClaimCheck Review History Action - Validate in the Generic table
         If scCkha <> *blanks;
            kkTbid = 'CKHA';
            kkKey = scCkha;
            Chain gtky pgt02;
              If not %Found(FTGTL);
                *in61 = *on;
                *in68 = *on;
                *in90 = *on;
                leavesr;
              Endif;
         Endif;

        //Eob Code
         If scEob <> *blanks;
         kkCo# = ##CO#;
         kkKey1 = scEob;

         Chain  Ebky PEB01;

         If not %found(FTEBLCK);
           *in62 = *on;
           *in67 = *on;
            *in90 = *on;
            leavesr;
         Endif;
         Endif;

      /End-Free
     C*** End Add *** 06/11/12 ****************************************
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SETOFF                                       44
     C*** Beg Add *** 06/11/12 ****************************************
      /Free
         // Set off Posiction cursor indicator
         *in45 = *off;
         *in46 = *off;
         *in47 = *off;
         *in48 = *off;
         *in49 = *off;
         *in50 = *off;
         *in51 = *off;
         *in52 = *off;
         *in53 = *off;
         *in54 = *off;
         *in55 = *off;
         *in56 = *off;
         *in57 = *off;
         *in58 = *off;
         *in59 = *off;
         *in61 = *off;
         *in62 = *off;
         *in63 = *off;
         *in64 = *off;
         *in67 = *off;
         *in68 = *off;
         *in74 = *off;
         outRrn = 0;
         inKc = *blanks;
         inKl = *blanks;
         inCall = '2 ';
         inCal = '2';
         OutKey = *blanks;

      /End-Free
     C*** End Add *** 06/11/12 ****************************************
     C*
     C                   SELECT
     C*
     C** Source type
     C*
     C     HFLD          WHENEQ    'SCTYPE'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCTYPE        HLTYPE
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##RRN             9 0
     C                   PARM      'ERMG'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLTYPE        SCTYPE
     C                   SETON                                        9044
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCTYPE
     C                   MOVEL     GTDESC        SCTYPD
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** Beg Add *** 06/11/12 ****************************************
      /Free

       //ClaimCheck payment status

        When Hfld = 'SCCKPS';
        HlCkps = scCkps;
        inTblId = 'CKPS';

          callp RtvTblId(inCall:inKc:inKl:outRrn:inTblId);

            If inKl = '1' Or outRrn = 0;
              scCkps = HlCkps;
              *in45 = *on;
              *in90 = *on;
            EndIf;

            If outRrn <> 0;

              Chain outRrn PGT01;
                If %found(FTGTP) ;
                   scCkps = GtKey;
                   *in46 = *on;
                   *in90 = *on;
                Endif;

            Endif;

       //ClaimCheck history status

        When Hfld = 'SCCKHS';
        HlCkhs = scCkhs;
        inTblId = 'CKHS';

          callp RtvTblId(inCall:inKc:inKl:outRrn:inTblId);

            If inKl = '1' Or outRrn = 0;
              scCkhs = HlCkhs;
              *in46 = *on;
              *in90 = *on;
            EndIf;

            If outRrn <> 0;

              Chain outRrn PGT01;
                If %found(FTGTP) ;
                   scCkhs = GtKey;
                   *in47 = *on;
                   *in90 = *on;
                Endif;

            Endif;

       //ClaimCheck SCE error 1

        When Hfld = 'SCCKE1';
        HlCke1 = scCke1;
        inTblId = 'CKE1';

          callp RtvTblId(inCall:inKc:inKl:outRrn:inTblId);

            If inKl = '1' or outRrn = 0;
              scCke1 = HlCke1;
              *in47 = *on;
              *in90 = *on;
            EndIf;

            If outRrn <> 0;

              Chain outRrn PGT01;
                If %found(FTGTP) ;
                   scCke1 = GtKey;
                   *in48 = *on;
                   *in90 = *on;
                Endif;

            Endif;

       //ClaimCheck SCE error 2

        When Hfld = 'SCCKE2';
        HlCke2 = scCke2;
        inTblId = 'CKE2';

          callp RtvTblId(inCall:inKc:inKl:outRrn:inTblId);

            If inKl = '1' or outRrn = 0;
              scCke2 = HlCke2;
              *in48 = *on;
              *in90 = *on;
            EndIf;

            If outRrn <> 0;

              Chain outRrn PGT01;
                If %found(FTGTP) ;
                   scCke2 = GtKey;
                   *in49 = *on;
                   *in90 = *on;
                Endif;

            Endif;

       //ClaimCheck code origination

        When Hfld = 'SCCKCO';
        HlCkco = scCkco;
        inTblId = 'CKCO';

          callp RtvTblId(inCall:inKc:inKl:outRrn:inTblId);

            If inKl = '1' or outRrn = 0;
              scCkco = HlCkco;
              *in49 = *on;
              *in90 = *on;
            EndIf;

            If outRrn <> 0;

              Chain outRrn PGT01;
                If %found(FTGTP) ;
                   scCkco = GtKey;
                   *in50 = *on;
                   *in90 = *on;
                Endif;

            Endif;

       //ClaimCheck duplicate msg type

        When Hfld = 'SCCKDM';
        HlCkdm = scCkdm;
        inTblId = 'CKDM';

          callp RtvTblId(inCall:inKc:inKl:outRrn:inTblId);

            If inKl = '1' or outRrn = 0;
              scCkdm = HlCkdm;
              *in50 = *on;
              *in90 = *on;
            EndIf;

            If outRrn <> 0;

              Chain outRrn PGT01;
                If %found(FTGTP) ;
                   scCkdm = GtKey;
                   *in51 = *on;
                   *in90 = *on;
                Endif;

            Endif;

       //ClaimCheck duplicate flag

        When Hfld = 'SCCKDF';
        HlCkdf = scCkdf;
        inTblId = 'CKDF';

          callp RtvTblId(inCall:inKc:inKl:outRrn:inTblId);

            If inKl = '1' or outRrn = 0;
              scCkdf = HlCkdf;
              *in51 = *on;
              *in90 = *on;
            EndIf;

            If outRrn <> 0;

              Chain outRrn PGT01;
                If %found(FTGTP) ;
                   scCkdf = GtKey;
                   *in52 = *on;
                   *in90 = *on;
                Endif;

            Endif;

       //ClaimCheck assistant surgeon

        When Hfld = 'SCCKAS';
        HlCkas = scCkas;
        inTblId = 'CKAS';

          callp RtvTblId(inCall:inKc:inKl:outRrn:inTblId);

            If inKl = '1' Or outRrn = 0;
              scCkas = HlCkas;
              *in52 = *on;
              *in90 = *on;
            EndIf;

            If outRrn <> 0;

              Chain outRrn PGT01;
                If %found(FTGTP) ;
                   scCkas = GtKey;
                   *in53 = *on;
                   *in90 = *on;
                Endif;

            Endif;

       //ClaimCheck assistant at surgery

        When Hfld = 'SCCKAA';
        HlCkaa = scCkaa;
        inTblId = 'CKAA';

          callp RtvTblId(inCall:inKc:inKl:outRrn:inTblId);

            If inKl = '1' Or outRrn = 0;
              scCkaa = HlCkaa;
              *in53 = *on;
              *in90 = *on;
            EndIf;

            If outRrn <> 0;

              Chain outRrn PGT01;
                If %found(FTGTP) ;
                   scCkaa = GtKey;
                   *in54 = *on;
                   *in90 = *on;
                Endif;

            Endif;

       //ClaimCheck sex indicator

        When Hfld = 'SCCKSX';
        HlCksx = scCksx;
        inTblId = 'CKSX';

          callp RtvTblId(inCall:inKc:inKl:outRrn:inTblId);

            If inKl = '1' or outRrn = 0;
              scCksx = HlCksx;
              *in54 = *on;
              *in90 = *on;
            EndIf;

            If outRrn <> 0;

              Chain outRrn PGT01;
                If %found(FTGTP) ;
                   scCksx = GtKey;
                   *in55 = *on;
                   *in90 = *on;
                Endif;

            Endif;

       //ClaimCheck age indicator

        When Hfld = 'SCCKAG';
        HlCkag = scCkag;
        inTblId = 'CKAG';

          callp RtvTblId(inCall:inKc:inKl:outRrn:inTblId);

            If inKl = '1' or outRrn = 0;
              scCkag = HlCkag;
              *in55 = *on;
              *in90 = *on;
            EndIf;

            If outRrn <> 0;

              Chain outRrn PGT01;
                If %found(FTGTP) ;
                   scCkag = GtKey;
                   *in56 = *on;
                   *in90 = *on;
                Endif;

            Endif;

       //ClaimCheck experimental status

        When Hfld = 'SCCKEP';
        HlCkep = scCkep;
        inTblId = 'CKEP';

          callp RtvTblId(inCall:inKc:inKl:outRrn:inTblId);

            If inKl = '1' or outRrn = 0;
              scCkep = HlCkep;
              *in56 = *on;
              *in90 = *on;
            EndIf;

            If outRrn <> 0;

              Chain outRrn PGT01;
                If %found(FTGTP) ;
                   scCkep = GtKey;
                   *in57 = *on;
                   *in90 = *on;
                Endif;

            Endif;

       //ClaimCheck obsolete status

        When Hfld = 'SCCKOB';
        HlCkob = scCkob;
        inTblId = 'CKOB';

          callp RtvTblId(inCall:inKc:inKl:outRrn:inTblId);

            If inKl = '1' or outRrn = 0;
              scCkob = HlCkob;
              *in57 = *on;
              *in90 = *on;
            EndIf;

            If outRrn <> 0;

              Chain outRrn PGT01;
                If %found(FTGTP) ;
                   scCkob = GtKey;
                   *in58 = *on;
                   *in90 = *on;
                Endif;

            Endif;

       //Edit type

        When Hfld = 'SCCKET';
        HlCket = scCket;
        inTblId = 'CKET';

          callp RtvTblId(inCall:inKc:inKl:outRrn:inTblId);

            If inKl = '1' or outRrn = 0;
              scCket = HlCket;
              *in58 = *on;
              *in90 = *on;
            EndIf;

            If outRrn <> 0;

              Chain outRrn PGT01;
                If %found(FTGTP) ;
                   scCket = GtKey;
                   *in59 = *on;
                   *in90 = *on;
                Endif;

            Endif;

       //Current action

        When Hfld = 'SCCKCA';
        HlCkca = scCkca;
        inTblId = 'CKCA';

          callp RtvTblId(inCall:inKc:inKl:outRrn:inTblId);

            If inKl = '1' or outRrn = 0;
              scCkca = HlCkca;
              *in59 = *on;
              *in90 = *on;
            EndIf;

            If outRrn <> 0;

              Chain outRrn PGT01;
                If %found(FTGTP) ;
                   scCkca = GtKey;
                   *in61 = *on;
                   *in90 = *on;
                Endif;

            Endif;

       //History action

        When Hfld = 'SCCKHA';
        HlCkha = scCkha;
        inTblId = 'CKHA';

          callp RtvTblId(inCall:inKc:inKl:outRrn:inTblId);

            If inKl = '1' or outRrn = 0;
              scCkha = HlCkha;
              *in61 = *on;
              *in90 = *on;
            EndIf;

            If outRrn <> 0;

              Chain outRrn PGT01;
                If %found(FTGTP) ;
                   scCkha = GtKey;
                   *in62 = *on;
                   *in90 = *on;
                Endif;

            Endif;

       //EOB code

        When Hfld = 'SCEOB';
          HlEob = scEob;
          inSt = ##stcd;
          callp GenTblId(inCal:inKc:inKl:inSt:OutKey);

            If inKl = '1' or outKey = *blanks;
              scEob = HlEob;
              *in62 = *on;
              *in90 = *on;
            Else;
              scEob = OutKey;
              *in62 = *on;
            EndIf;

      /End-Free
     C*** End Add *** 06/11/12 ****************************************
     C                   ENDSL                                                              ROS
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C  N40              MOVEL     MISTAT        SCSTAT
     C                   MOVE      MIMSG#        SCMSG#
     C                   MOVEL     MIDESC        SCDESC
     C*** Beg Add *** 06/11/12 *************************************************
      /Free

         SCDES  = SCDESC;
         SCCKPS = MICKPS;
         SCCKET = MICKET;
         SCCKHS = MICKHS;
         SCCKCA = MICKCA;
         SCCKHA = MICKHA;
         SCCKE1 = MICKE1;
         SCCKE2 = MICKE2;
         SCCKCO = MICKCO;
         SCCKDM = MICKDM;
         SCCKDF = MICKDF;
         SCCKAS = MICKAS;
         SCCKAA = MICKAA;
         SCCKSX = MICKSX;
         SCCKAG = MICKAG;
         SCCKEP = MICKEP;
         SCCKOB = MICKOP;
         SCEOB  = MIEOB ;

      /End-Free
     C*** End Add *** 06/11/12 *************************************************
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     MITYPE        SCTYPE
     C                   MOVE      *BLANKS       SCTYPD
     C                   MOVEL(P)  'ERMG'        KKTBID
     C                   MOVEL(P)  SCTYPE        KKKEY
     C     GTKY          CHAIN     PGT02                              60
     C  N60              MOVEL(P)  GTDESC        SCTYPD
     C*
     C* Add Mode
     C                   ELSE
     C                   MOVE      *BLANK        SCTYPE
     C                   MOVE      *BLANKS       SCTYPD
     C*
     C                   ENDIF
     C*
     C                   CLEAR                   DESC
     C                   MOVEL     MIMSG#        KKMSG#
     C                   Z-ADD     1             KKMSEQ
     C     KEY1          CHAIN     DBRCD2                             60
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     MIMSEQ        I                 3 0
     C                   MOVEL(P)  MIDESC        DESC(I)
     C     KKMSG#        READE     DBRCD2                                 60
     C                   ENDDO                                                  *IN60 = Off
     C*
     C*** Beg Add *** 06/11/12 *************************************************
      /Free

         SCDE1 = SCDES1;

      /End-Free
     C*** End Add *** 06/11/12 *************************************************
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVEL(P)  SCSTAT        MISTAT
     C   40              MOVE      'A'           MISTAT
     C                   MOVEL(P)  SCTYPE        MITYPE
     C                   MOVEL(P)  Q1USER        MIUSER
     C                   MOVEL(P)  Q1PGM         MIPGM
     C                   Z-ADD     DSTIME        MITIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MIDT
     C*
     C*          Process extended description records
     C*          ------------------------------------------------------
     C                   MOVEL(P)  'PC'          KKMSG#
     C   41              MOVEL     ##STCD        KKMSG#
     C                   MOVE      SCMSG#        KKMSG#
     C                   DO        5             I
     C                   Z-ADD     I             KKMSEQ
     C     KEY1          CHAIN     DBRCD2                             60
     C*
     C*             If record and no description: delete record
     C*             ---------------------------------------------------
     C     *IN60         IFEQ      *OFF
     C     DESC(I)       ANDEQ     *BLANKS
     C                   DELETE    DBRCD2
     C                   ITER                                                   VVV ITER
     C                   ENDIF                                                  *IN60 = Off
     C*
     C*             If description and record: update record
     C*             ---------------------------------------------------
     C     DESC(I)       IFNE      *BLANKS
     C                   MOVEL(P)  SCSTAT        MISTAT
     C                   MOVEL(P)  Q1USER        MIUSER
     C                   MOVEL(P)  Q1PGM         MIPGM
     C                   Z-ADD     DSTIME        MITIME
     C                   MOVEL     SRDTO         MIDT
     C                   MOVEL     DESC(I)       MIDESC
     C                   MOVE      SCTYPE        MITYPE
     C     *IN60         IFEQ      *OFF
     C                   UPDATE    DBRCD2
     C                   ELSE                                                   DESC <> Blanks
     C*
     C*             If description and record: write record
     C*             ---------------------------------------------------
     C                   MOVEL(P)  'PC'          MIMSG#
     C   41              MOVEL     ##STCD        MIMSG#
     C                   MOVE      SCMSG#        MIMSG#
     C                   Z-ADD     I             MIMSEQ
     C                   MOVEL(P)  MIUSER        MIIUSR
     C                   MOVEL(P)  MIPGM         MIIPGM
     C                   Z-ADD     MITIME        MIITIM
     C                   Z-ADD     MIDT          MIIDT
     C                   MOVE      SCTYPE        MITYPE
       //** Beg Add *** 06/15/20 *******************************************
      /Free
                         MIst   = ##StCd;
                         MICKPS = SCCKPS;
                         MICKET = SCCKET;
                         MICKHS = SCCKHS;
                         MICKCA = SCCKCA;
                         MICKHA = SCCKHA;
                         MICKE1 = SCCKE1;
                         MICKE2 = SCCKE2;
                         MICKCO = SCCKCO;
                         MICKDM = SCCKDM;
                         MICKDF = SCCKDF;
                         MICKAS = SCCKAS;
                         MICKAA = SCCKAA;
                         MICKSX = SCCKSX;
                         MICKAG = SCCKAG;
                         MICKEP = SCCKEP;
                         MICKOP = SCCKOB;
                         MIEOB  = SCEOB ;
      /End-free
       //** End Add *** 06/15/20 *******************************************
     C                   WRITE     DBRCD2
     C                   ENDIF                                                  *IN60 = Off
     C                   ENDIF                                                  DESC <> Blanks
     C                   ENDDO                                                  DO 5 times
     C*
     C*          Update primary description record
     C*          ------------------------------------------------------
     C                   MOVEL(P)  'PC'          MIMSG#
     C   41              MOVEL     ##STCD        MIMSG#
     C                   MOVE      SCMSG#        MIMSG#
     C                   Z-ADD     0             MIMSEQ
     C                   MOVEL     SCDESC        MIDESC
     C                   MOVEL(P)  SCSTAT        MISTAT
     C                   MOVEL(P)  Q1USER        MIUSER
     C                   MOVEL(P)  Q1PGM         MIPGM
     C                   Z-ADD     DSTIME        MITIME
     C                   MOVEL     SRDTO         MIDT
     C                   MOVE      SCTYPE        MITYPE
     C     *IN40         IFEQ      *ON
     C                   MOVEL(P)  MIUSER        MIIUSR
     C                   MOVEL(P)  MIPGM         MIIPGM
     C                   Z-ADD     MITIME        MIITIM
     C                   Z-ADD     MIDT          MIIDT
     C                   ENDIF                                                  *IN40 = On
     C*** Beg Add *** 06/11/12 *************************************************
      /Free

         MICKPS = SCCKPS;
         MICKET = SCCKET;
         MICKHS = SCCKHS;
         MICKCA = SCCKCA;
         MICKHA = SCCKHA;
         MICKE1 = SCCKE1;
         MICKE2 = SCCKE2;
         MICKCO = SCCKCO;
         MICKDM = SCCKDM;
         MICKDF = SCCKDF;
         MICKAS = SCCKAS;
         MICKAA = SCCKAA;
         MICKSX = SCCKSX;
         MICKAG = SCCKAG;
         MICKEP = SCCKEP;
         MICKOP = SCCKOB;
         MIEOB  = SCEOB ;
       //** Beg Add *** 01/14/15 *******************************************
         MIST   = ##StCd;
       //** End Add *** 01/14/15 *******************************************
      /End-Free
     C*** End Add *** 06/11/12 *************************************************
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C     *INKJ         CASEQ     *ON           KJDO                           F10=search
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*          Return function key settings and exit
     C*          ------------------------------------------------------
     C                   MOVE      *INKL         @@KL
     C                   SETON                                        LR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C*          40 - Set on if 'Add' Mode
     C*          ------------------------------------------------------
     C     @@MODE        COMP      'A'                                    40
     C*
     C*          41 - Set on if State Messages
     C*          ------------------------------------------------------
     C     ##STCD        IFNE      *BLANKS
     C                   SETON                                        41
     C                   MOVEL(P)  ##STCD        KKKEY
     C                   MOVEL     ##STCD        SCSTCD
     C     STKY          CHAIN     PST01                              60
     C  N60              MOVEL     STNAME        SCSTNM
     C                   ENDIF                                                  STCD <> blanks
     C*
     C   40              MOVE      'A'           SCSTAT
     C*
     C*** Beg Add *** 06/11/12 *************************************************
      /Free
         If ##STCD = 'CK';
           #OFSCR = 2;
         Else;
           #OFSCR = 1;
         Endif;
      /End-Free
     C*** End Add *** 06/11/12 *************************************************
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C*          MODE 'S' - Change status
     C*          ------------------------------------------------------
     C     @@MODE        IFEQ      'S'
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C     MISTAT        IFEQ      'I'
     C                   MOVE      'A'           MISTAT
     C                   ELSE                                                   STAT = 'I'
     C                   MOVE      'I'           MISTAT
     C                   ENDIF                                                  STAT = 'I'
     C                   MOVEL     Q1USER        MIUSER
     C                   MOVEL     Q1PGM         MIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MIDT
     C                   MOVEL     DSTIME        MITIME
     C                   UPDATE    DBRCDF
     C                   ENDIF                                                  *IN99 = Off
     C*
     C                   RETURN
     C                   ENDIF                                                  MODE = 'S'
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FTMIP
     C                   ENDSR
     O*
     O*----------------------------------------------------------------
     O* Record releases
     O*----------------------------------------------------------------
     ODBRCD2    E            FTMILE
