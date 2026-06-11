     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H*
     H/TITLE F6201R - EDI 837 EXTRACT PROCESS
     H*
     H*****************************************************************
     H*  DESCRIPTION:  This pgm processes all records from the log    *
     H*    history file which are due for extraction.  First, the     *
     H*    EDI data elements are edited, then they are printed to     *
     H*    1 of 2 reports, the EDI 837 Excepted report or the 837     *
     H*    Error Rejection report.  If the transaction passes all     *
     H*    edits, it is written to the 837 extract files, then the    *
     H*    log history file is updated.                               *
     H*                                                               *
     H*  CALLED FROM:  F6201C                                         *
     H*  CALLS TO:     F6202R - DATA ELEMENT EDIT PROGRAM             *
     H*                                                               *
     H*  CREATED:  06/11/99     C:15      Project:  KY010             *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/09/20 C:295 Project: SY420                                 *
     H*                -Mask SSN                                      *
     H*                                                               *
     H* 07/08/16 C:079 Support: 42377/99                              *
     H*                Open WMCML1 correctly                          *
     H*                                                               *
     H* 05/24/16 C:309 Support: 41670                                 *
     H*                Used WMCML1 instead of WMCML and changed key   *
     H*                list.
     H*                                                               *
     H* 12/02/15 C:309 Project: 25MUD TDI-DWC/Prior Claim Number      *
     H*                Modify to pull Prior Claim#                    *
     H*                Close WMCML when exit                          *
     H*                                                               *
     H* 10/21/15 C:079 PhaseWare: 25/37996                            *
     H*                Add ICD10 code 9                               *
     H*                                                               *
     H* 07/28/15 C:079 PhaseWare: 25/36279                            *
     H*                Load xxlost key for ICD-9 lookup phy and dent  *
     H*                                                               *
     H* 05/01/15 C:079 PhaseWare: 25/34018                            *
     H*                Do not report the EOB cross reference code     *
     H*                                                               *
     H* 04/16/15 C:212 Project: MC395                                 *
     H*                ICD10 TX 837 EDI Changes                       *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 05/29/14 C:212 SUPPORT: 85670                                 *
     H*                Load Hospital proc codes                       *
     H*                                                               *
     H* 08/06/13 C:062 SUPPORT: MC025 82993                           *
     H*                Populate DN,523 for all bill types             *
     H*                                                               *
     H* 04/03/13 C:277 SUPPORT: 81748                                 *
     H*                Populate Rendering NPI#                        *
     H*                                                               *
     H* 02/20/12 C:277 SUPPORT: 78735                                 *
     H*                MOVE -VE LGPAID IN CORRECT FORMAT              *
     H*                                                               *
     H* 01/10/12 C:251 Project: 25MSO                                 *
     H*                Load Referring Provider License Number         *
     H*                                                               *
     H* 10/10/11 C:251 Support: 78091                                 *
     H*                On a reconsideration send all detail lines     *
     H*                irrespecive of any change .                    *
     H*                                                               *
     H* 08/11/11 C:079 Project: TX075                                 *
     H*                On a reconsideration only send changes         *
     H*                                                               *
     H* 01/28/10 C:249 Project: SY215                                 *
     H*           :189 Retrieve brand Name                            *
     H*                                                               *
     H* 05/18/09 C:198 Support: 67689                                 *
     H*                Referring provider NPI# processing             *
     H*                                                               *
     H* 04/07/09 C:079 Support: 67538                                 *
     H*                LOAD BILLED REVENUE CODE CORRECTLY             *
     H*                                                               *
     H* 02/25/09 C:062 Project: WA025 67178                           *
     H*                REMOVE DEA FROM DN,695                         *
     H*                                                               *
     H* 12/13/08 C:079 Project: TX048                                 *
     H*                ASSIGN LICENSE NUMBERS TO EXTRACT FILE/FIELDS  *
     H*                                                               *
     H* 10/30/08 C:198 Project: TX048                                 *
     H*                RENDERING PROVIDER LIC# PROCESSING             *
     H*                                                               *
     H* 07/03/08 C:198 Project: TX046                                 *
     H*                RENDERING PROVIDER NPI# PROCESSING             *
     H*                                                               *
     H* 05/28/08 C:62 Project: TX045                                  *
     H*               DEFAULT RENDERING STATE FOR TX AS TX
     H*                                                               *
     H* 01/30/08 C:47 Project: CA002                                  *
     H*               IF THE ADJUDICATION STATE = CA, GET THE
     H*               GENERIC TABLE ID CASC AND LOAD THE FIELD GTCF01
     H*               INTO THE DATA ELEMENT DN,537.
     H*                                                               *
     H* 03/02/07 C:79 Support: 57439                                  *
     H*               Condition Calls to WCMS pgms on Install Flag    *
     H*                                                               *
     H* 01/25/07 C:79 Support: 56053                                  *
     H*               If EOB Code is inactive use W1 for TX bills     *
     H*                                                               *
     H* 01/16/07 C:188 Project: CA002                                 *
     H*                Implement CA 837 Medical EDI Requirements      *
     H*                                                               *
     H* 11/02/06 C:79 Support: 55605                                  *
     H*               Remove subscriber from the claim admin clm#     *
     H*               Do not send Agency Claim Number DN,5)           *
     H*                                                               *
     H* 09/12/06 C:79 Project: 25MNZ                                  *
     H*               Add load of rendering provider                  *
     H*               Add load of paid date for denied bills          *
     H*                                                               *
     H* 07/08/06 C:79 Project: 25MNZ                                  *
     H*               Correct logic for Admitting Diag                *
     H*               Dont send patient account number                *
     H*               Use Claim Admin info from MC files              *
     H*                                                               *
     H* 02/18/05 C:79 Project: TX029                                  *
     H*               Add logic for Texas 837                         *
     H*                                                               *
     H* 06/30/04 C:62 Project: DR082 43042                            *
     H*               Change data entry process.                      *
     H*                                                               *
     H* 08/19/03 C:15 Project: WC1137                                 *
     H*               Added code to load the check mailing date into  *
     H*               LGMTCD for IP transactions.                     *
     H*                                                               *
     H*  02/13/02  C:53  Support: MC048 # 31240                       *
     H*                 -If Recon load the recon rcvd date.           *
     H*                                                               *
     H*  02/12/01  C:15  Project: KY010                               *
     H*                 -Load the SS# into the patient acct# for      *
     H*                  Pharmacy and Dental bills.                   *
     H*                                                               *
     H*  01/24/01  C:15  Project: KY010                               *
     H*                  There is only one revenue code entry in Prov *
     H*                  Bills, so this pgm was chgd to load both the *
     H*                  paid and billed codes from the same field.   *
     H*                                                               *
     H*  11/28/00  C:15  Project: KY010                               *
     H*                  Added code to load the correct insurer info. *
     H*                                                               *
     H*  02/07/00  C:062 PROJECT: Y2K                                 *
     H*                  CHANGE WCMS DATES TO YYMD.                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     F*** BEG ADD *** 07/08/06 ****************************************
     FFMC1L     IF   E           K DISK
     F*** END ADD *** 07/08/06 ****************************************
     FSMORL     IF   E           K DISK
     FFEEXL     IF   E           K DISK
     FFELGL1    UF   E           K DISK
     FFELGL     IF   E           K DISK
     F                                     RENAME(PLG01:PLG02)
     FFEMEL     UF   E           K DISK
     FFEE1L1    O    E           K DISK
     FFAEOLG    IF   E           K DISK
     FFAEAL1    IF   E           K DISK
     FFAEEL     IF   E           K DISK
     FFAEDL     IF   E           K DISK
     FFAEHL     IF   E           K DISK
     FFAEPL     IF   E           K DISK
     FFAEUL     IF   E           K DISK
     F*** BEG ADD *** 02/13/02 ****************************
     FFAESL2    IF   E           K DISK
     F*** END ADD *** 02/13/02 ****************************
     FFDCAL13   IF   E           K DISK
     FFDDDL     IF   E           K DISK
     F*** BEG CHG *** 08/11/11 ****************************************
     F******* FDDEL   IF  E           K        DISK
     FFDDEL3    IF   E           K DISK
     F*** END CHG *** 08/11/11 ****************************************
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
     F*** BEG ADD *** 02/18/05 ****************************************
     FFTEBLTX   IF   E           K DISK
     F                                     RENAME(PEB01:PEB02)
     F*** END ADD *** 02/18/05 ****************************************
     FFTDNL     IF   E           K DISK
     FFTERL     IF   E           K DISK
     FFXXTL     IF A E           K DISK
     FSDCMLA    UF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     F*** BEGIN ADD *** 11/28/00 **************************************
     FFMFNL     IF   E           K DISK
     FFDF2L     IF   E           K DISK
     F*** END ADD ***** 11/28/00 **************************************
     F*** BEG ADD *** 01/30/08 ****************************************
     FFTGTL     IF   E           K DISK
     F*** END ADD *** 01/30/08 ****************************************
     F*** BEG ADD *** 08/11/11 ****************************
     FNMNWL     IF   E           K DISK
     FFDDAL2    IF   E           K DISK
     F*** END ADD *** 08/11/11 ****************************
     F*** BEG ADD *** 04/16/15 *********************************************
     FFMICD10L1 IF   E           K DISK
     F*** END ADD *** 04/16/15 *********************************************
     F***** Beg Add *** 12/02/15 *******************************************
     F***** Beg Chg *** 05/24/16 *******************************************
     F***WMCML    IF   E           K DISK    UsrOpn

     FWMCML1    IF   E           K DISK    UsrOpn
     F***** End Chg *** 05/24/16 *******************************************
     F                                     Rename(PCM01:PCM03) Prefix(WC:2)
     F***** End Add *** 12/02/15 *******************************************
     F*
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOB)
     FQSYSPRT2  O    F  198        PRINTER OFLIND(*INOC)

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
     D*________________________________________________________________
     D*     TABLES and ARRAYS
     D DN              S             30    DIM(999)
     D SQ              S              5  0 DIM(6)
     D QCA             S             56    DIM(1) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*** BEG ADD *** 01/18/07 ****************************************
     D*
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
     D*
     D*** BEG ADD *** 04/16/15 *********************************************
     D kkICD10         S              7A   inz
     D kkInd           S              1A   inz
     D*** END ADD *** 04/16/15 *********************************************

     D***** Beg Add *** 05/24/16 *******************************************
     D XXFND           s                   like(eafnd)
     D XXFYR           s                   like(eafyr)
     D XXCASE          s                   like(eacase)
     D***** End Add *** 05/24/16 *******************************************
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     IPEH01
     I              EHDIA1                      EHDI1
     I              EHDIA2                      EHDI2
     I              EHDIA3                      EHDI3
     I              EHDIA4                      EHDI4
     I*** BEG ADD *** 04/16/15 *********************************************
     I              EHICD1001                   EHDI1001
     I              EHICD1002                   EHDI1002
     I              EHICD1003                   EHDI1003
     I              EHICD1004                   EHDI1004
     I*** END ADD *** 04/16/15 *********************************************
     I*** BEG ADD *** 02/18/05 ****************************************
     I*
     IPED01
     I              EDDIA1                      EDDI1
     I              EDDIA2                      EDDI2
     I              EDDIA3                      EDDI3
     I              EDDIA4                      EDDI4
     I*** END ADD *** 02/18/05 ****************************************
     I*** BEG ADD *** 04/16/15 *********************************************
     I              EDICD1001                   EDDI1001
     I              EDICD1002                   EDDI1002
     I              EDICD1003                   EDDI1003
     I              EDICD1004                   EDDI1004
     I*** END ADD *** 04/16/15 *********************************************
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
     I*** BEG ADD *** 01/18/07 ****************************************
     I              EUCTCD                      EUCTC2
     I***** Beg Add *** 05/29/14 **************************************
     I              EUPRC1                      EUPI1
     I              EUPRC2                      EUPI2
     I              EUPRC3                      EUPI3
     I              EUPRC4                      EUPI4
     I              EUPRC5                      EUPI5
     I              EUPRC6                      EUPI6
     I***** End Add *** 05/29/14 **************************************
     I*** BEG ADD *** 04/16/15 *********************************************
     I              EUICD1001                   EUDI1001
     I              EUICD1002                   EUDI1002
     I              EUICD1003                   EUDI1003
     I              EUICD1004                   EUDI1004
     I              EUICD1005                   EUDI1005
     I              EUICD1006                   EUDI1006
     I              EUICD1007                   EUDI1007
     I              EUICD1008                   EUDI1008
     I              EUICD1009                   EUDI1009
     I              EUPRC1001                   EUPR1001
     I              EUPRC1002                   EUPR1002
     I              EUPRC1003                   EUPR1003
     I              EUPRC1004                   EUPR1004
     I              EUPRC1005                   EUPR1005
     I              EUPRC1006                   EUPR1006
     I              EUADIA1001                  EUAD1001
     I*** Beg Add *** 10/21/15 ****************************************
     I              EUICD1010                   EUDI1010
     I*** End Add *** 10/21/15 ****************************************
     I*** END ADD *** 04/16/15 *********************************************

     C*================================================================
     C*   ENTRY PARAMETERS (ALL ALPHANUMERIC)
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQJST             2
     C                   PARM                    QQEXDT            8
     C                   PARM                    QQTSET            3
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQJST         XXJST
     C                   MOVE      QQEXDT        X1EXDT            8 0
     C                   Z-ADD     X1EXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
     C*
     C*  Process initialization subroutine
     C*
     C                   EXSR      SRINIT
     C*
     C*  Call subroutine to load the extract workfile.
     C*
     C                   EXSR      LDWRKF
     C*----------------------------------------------------------------
     C* MAINLINE OF PROGRAM.
     C*
     C     XTKY          CHAIN     PXT01                              99
B011 C     *IN99         DOWEQ     *OFF
     C*
     C*  GET THE EDI LOG HISTORY RECORD
     C*
     C     LGKY          CHAIN     PLG01                              99
     C   99              GOTO      SKIP
     C*
     C*  Load the Data Element Array
     C*
     C                   CLEAR                   DN
     C                   EXSR      GETDB
     C                   EXSR      MVDBAR
     C*
     C*  Run the editor for each data element
     C*
     C                   MOVE      'N'           XXERR
     C     DNKY          CHAIN     PDN01                              99
B02  C     *IN99         DOWEQ     *OFF
B03  C     DNSTAT        IFEQ      'A'
     C*
     C                   MOVE      DNDN#         P
     C                   CLEAR                   QQERRS
     C                   CALL      'F6202R'
     C                   PARM                    XXCO#
     C                   PARM                    XXJST
     C                   PARM                    P
     C                   PARM                    XXTSET
     C                   PARM                    LGMTC
     C                   PARM                    DN
     C                   PARM                    QQERRS
     C*
     C*   CHECK EACH DATA ELEMENT FOR ERRORS
     C*
     C                   Z-ADD     1             X
     C     3             SUBST     QQERRS:X      XXERRS
     C*
B04  C     XXERRS        DOWNE     *BLANKS
     C*
B05  C     XXERR         IFEQ      'N'
     C                   EXSR      ERRDRY
E05  C                   ENDIF
     C*
     C                   EXSR      RPDET1
     C                   EXSR      ERRFIL
     C                   MOVE      'Y'           XXERR
     C                   ADD       3             X
     C     3             SUBST     QQERRS:X      XXERRS
     C*
E04  C                   ENDDO
     C*
E03  C                   ENDIF
     C     DNKY          READE     PDN01                                  99
E02  C                   ENDDO
     C*
     C*  If there were no errors for this transaction, extract the
     C*  EOMB (update the log record, load the 837 extract files,
     C*  complete the extract error diary if it exists, and print
     C*  the EOMB on the detail report).
     C*
B02  C     XXERR         IFEQ      'N'
     C*
     C                   EXSR      UPDLOG
     C                   EXSR      LD837
     C                   EXSR      CMPDRY
     C                   EXSR      RPTDET
     C                   ADD       1             XXACPT                         ACCEPTED COUNT
     C*
X02  C                   ELSE
     C*
     C                   EXSR      ERRDRY
     C                   ADD       1             XXREJ                          REJECTED COUNT
     C*
E02  C                   ENDIF
     C*
     C*
     C*  PROCESS NEXT LOG RECORD
     C*
     C     SKIP          TAG
     C     XTKY          READE     PXT01                                  99
E011 C                   ENDDO
     C*
     C*  PRINT NO RECORDS SELECTED MESSAGES IF NO DETAILS WRITTEN
     C*
B01  C     *IN91         IFEQ      '0'
     C                   EXCEPT    HEAD
     C                   EXCEPT    MSG
E01  C                   ENDIF
B01  C     *IN92         IFEQ      '0'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    MSG1
E01  C                   ENDIF
     C*
     C*   PRINT THE SUMMARY REPORT
     C*
     C     XXACPT        ADD       XXREJ         XX837T                         TOTAL 837 CNT
     C                   EXCEPT    HEAD2
     C                   EXCEPT    DET2
     C*
     C*   UPDATE THE SUBMISSION MANAGEMENT RECORD
     C*
     C                   MOVE      '837'         XXTSET
     C     MEKY          CHAIN     PME01                              99
B01  C     *IN99         IFEQ      '0'
     C                   Z-ADD     XX837T        ME837T
     C                   Z-ADD     XXACPT        ME837S
     C                   MOVE      *BLANKS       MESTAT
     C                   MOVEL     Q1USER        MEUSER
     C                   MOVEL     Q1PGM         MEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        METIME
     C                   Z-ADD     XXDT7         MEDT
     C                   UPDATE    PME01
E01  C                   END
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxFile = '0';
       Chain orKy POR01;
       If %found;
          Except Route;
       Endif;
       xxFile = '1';
       Chain orKy POR01;
       If %found;
          Except Route1;
       Endif;
       xxFile = '2';
       Chain orKy POR01;
       If %found;
          Except Route2;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C*  END OF PROGRAM
     C*
       //***** Beg Add ***** 12/02/15 ***********************************
      /Free
       // Close Open Files

       //***** Beg Chg ***** 05/24/16 ***********************************
       //If %open(wmcml);
       //   Close wmcml;

       If %open(wmcml1);
          Close wmcml1;

       //***** End Chg ***** 05/24/16 ***********************************
       Endif;
       If %open(ftabl);
          Close ftabl;
       Endif;

      /End-free
       //***** End Add ***** 12/02/15 ***********************************
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*----------------------------------------------------------------
     C* SRINIT - This subroutine is used to define all KLISTs and
     C*          variables, and initialize certain fields.
     C*----------------------------------------------------------------
     C     SRINIT        BEGSR
     C*
     C*  SMCOL - KEY LIST FOR THE COMPANY MASTER FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*** BEG ADD *** 07/08/06 ****************************************
     C*
     C     C1KY          KLIST
     C                   KFLD                    XXCO#
     C*** END ADD *** 07/08/06 ****************************************
     C*
     C*  SMORL - KEY LIST FOR THE OUTPUT ROUTING FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXFILE
     C*
     C*** BEGIN ADD *** 11/28/00 **************************************
     C*  FMFNL - KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LGSUB#
     C                   KFLD                    LGFND
     C*
     C*  FDF2L - KEY LIST FOR THE WCMS FUND AUX FILE
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LGSUB#
     C                   KFLD                    LGFND
     C                   KFLD                    LGJST
     C*** END ADD ***** 11/28/00 **************************************
     C*
     C*  FEEXL - KEY LIST FOR THE EDI CONTROL FILE
     C*
     C     EXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXTSET
     C*
     C*  SDCMP - KEY LIST FOR THE DARTS REMARK FILE
     C*
     C     SDKEY         KLIST
     C                   KFLD                    ##KEY
     C                   KFLD                    XXDSEQ
     C*
     C*  FEMEL - SYSTEM MANAGEMENT RECORDS
     C*
     C     MEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXEXDT
     C                   KFLD                    XXTSET
     C*
     C*  FTERL - ERROR MESSAGE TABLE FILE
     C*
     C     ERKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXERR#
     C*
     C*  FTDNL - DATA ELEMENT TABLE FILE
     C*
     C     DNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXTSET
     C*
     C*  FELGP - KEY LIST FOR THE EDI LOG FILE
     C*
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XTSUB#
     C                   KFLD                    XTFND
     C                   KFLD                    XTEOB#
     C                   KFLD                    XTLSEQ
     C*
     C     L2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C*
     C*  FMCMP - KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LGSUB#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C*
     C*  FMCDP - KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  FMDIP - KEY LIST FOR THE DIAGNOSIS CODE FILE
     C*
     C     DIKY          KLIST
     C                   KFLD                    XXICD9
     C                   KFLD                    XXLOST
     C*
     C*  FMEMP - KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  FXXTP - KEY LIST FOR THE EDI EXTRACT WORKFILE
     C*
     C     XTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTSET
     C*
     C*  FMPDP - KEY LIST FOR THE PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C*
     C*  FTABL - Key list for the Table files
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*** BEG ADD *** 01/30/08 ****************************************
     C*
     C*  FTGTL - Key list for the Generic Table file
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    X1TBID
     C                   KFLD                    X1KEY
     C*** END ADD *** 01/30/08 ****************************************
    C***** Beg Add *** 12/02/15 **************************************
     C*  WMCML - Key list for Claim File
     C*
     C***** Beg Chg  *** 05/24/16 *************************************
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C***                KFLD                    CMFND
     C***                KFLD                    CMEMP#
     C***                KFLD                    CMDIV
     C***                KFLD                    CMFYR
     C***                KFLD                    CMCASE
     C***** End Chg  *** 05/24/16 *************************************
    C***** End Add *** 12/02/15 **************************************
     C*
     C*  Generic key to acces EOMB header files by recon#
     C*
     C     #HKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LGSUB#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C                   KFLD                    LGEOB#
     C*
     C     #HKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LGSUB#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C                   KFLD                    LGEOB#
     C                   KFLD                    LGRCN
     C*
     C*  Generic key to access EOMB detail files by detail seq#
     C*
     C     #DKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LGSUB#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C                   KFLD                    LGEOB#
     C                   KFLD                    CADSEQ
     C*** BEG ADD *** 08/11/11 ****************************************
     C*
     C*  Key to access Detail EOB Code/Note File
     C*
     C     #DEKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LGSUB#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C                   KFLD                    LGEOB#
     C                   KFLD                    LGRCN
     C                   KFLD                    CADSEQ
     C*
     C*  Key to access Detail Amount File
     C*
     C     #DAKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LGSUB#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C                   KFLD                    LGEOB#
     C                   KFLD                    CADSEQ
     C                   KFLD                    LGRCN
     C*
     C*  Key list to access the Network Master File
     C*
     C     NWKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKNWCD
     C*** END ADD *** 08/11/11 ****************************************
     C*
     C*  Parm lists for date routines
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** BEGIN ADD *** 02/07/00 **************************************
     C*
     C     DTCYM2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** END   ADD *** 02/07/00 **************************************
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  Define pgm variables.
     C*
     C     *LIKE         DEFINE    LGCO#         XXCO#
     C     *LIKE         DEFINE    LGJST         XXJST
     C     *LIKE         DEFINE    LGEXDT        XXEXDT
     C     *LIKE         DEFINE    LGTSET        XXTSET
     C     *LIKE         DEFINE    ORFIL#        XXFILE
     C     *LIKE         DEFINE    ORTITL        XXTITL
     C     *LIKE         DEFINE    ORTITL        X1TITL
     C     *LIKE         DEFINE    ORTITL        X2TITL
     C     *LIKE         DEFINE    ERMSG#        XXERR#
     C     *LIKE         DEFINE    ERDESC        XXEDSC
     C     *LIKE         DEFINE    DNDN#         XXDN#
     C     *LIKE         DEFINE    CMSEQ#        XXDSEQ
     C     *LIKE         DEFINE    CMSEQ#        SVSEQ#
     C     *LIKE         DEFINE    EBKEY         XXKEY
     C     *LIKE         DEFINE    DIICD9        XXICD9
     C     *LIKE         DEFINE    DILOST        XXLOST
     C*** BEG ADD *** 07/08/06 ****************************************
     C     *LIKE         DEFINE    EXSEIN        XXSEIN
     C     *LIKE         DEFINE    EXSZP         XXSZP
     C*** END ADD *** 07/08/06 ****************************************
     C*** BEG ADD *** 01/30/08 ****************************************
     C     *LIKE         DEFINE    GTTBID        X1TBID
     C     *LIKE         DEFINE    GTKEY         X1KEY
     C*** END ADD *** 01/30/08 ****************************************
     C*** BEG ADD *** 08/11/11 ****************************************
     C     *LIKE         DEFINE    EOCHRG        TOTCHG
     C     *LIKE         DEFINE    NWNWCD        KKNWCD
     C*** END ADD *** 08/11/11 ****************************************
     C*** BEG ADD *** 10/10/11 ****************************************
     C     *LIKE         DEFINE    CAPAID        XXPAID
     C*** END ADD *** 10/10/11 ****************************************
     C*
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
     C                   MOVE      *BLANKS       QQERRS           30
     C                   MOVE      *BLANKS       XXERRS            3
     C                   MOVE      *BLANKS       XXERR             1
     C                   MOVE      *BLANKS       XXEUSR           10
     C                   MOVE      *BLANKS       X1EUSR           10
     C                   MOVE      'D'           XXLOST
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT7             7 0
     C*
     C*** GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              99
     C   99              MOVE      *BLANKS       CONAME
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*** BEG ADD *** 07/08/06 ****************************************
     C*
     C     C1KY          CHAIN     PC101                              99
     C   99              MOVE      *BLANKS       C1NAME
     C*** END ADD *** 07/08/06 ****************************************
     C*
     C*  GET THE ROUTING RECORDS AND PRINT THE ROUTING PAGES
     C*
     C                   MOVE      '0'           XXFILE
     C     ORKY          CHAIN     POR01                              99
B01  C     *IN99         IFEQ      '0'
     C                   MOVEL     ORTITL        XXTITL
      ***** Beg Del ***** 08/26/14 *********************************************
     C***********        EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
E01  C                   END
     C*
     C                   MOVE      '1'           XXFILE
     C     ORKY          CHAIN     POR01                              99
B01  C     *IN99         IFEQ      '0'
     C                   MOVEL     ORTITL        X1TITL
      ***** Beg Del ***** 08/26/14 *********************************************
     C***********        EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 *********************************************
E01  C                   END
     C*
     C                   MOVE      '2'           XXFILE
     C     ORKY          CHAIN     POR01                              99
B01  C     *IN99         IFEQ      '0'
     C                   MOVEL     ORTITL        X2TITL
      ***** Beg Del ***** 08/26/14 *********************************************
     C***********        EXCEPT    ROUTE2
      ***** End Del ***** 08/26/14 *********************************************
E01  C                   END
     C*
     C                   MOVE      '1'           *INOA
     C                   MOVE      '1'           *INOB
     C                   MOVE      '1'           *INOC
     C*
     C*  GET THE EDI CLAIMS CONTROL RECORD.  IF THE ELECTRONIC
     C*  SUBMISSION FLAG DOESN'T EQUAL 'E' FOR THE TRANSACTION
     C*  TYPE, DON'T PROCESS ANY OF THOSE TYPE RECORDS.
     C*** BEG ADD *** 07/08/06 ****************************************
     C                   MOVE      *BLANKS       XXTSET
     C     EXKY          CHAIN     PEX01                              21
     C                   MOVEL     EXSEIN        XXSEIN
     C                   MOVEL     EXSZP         XXSZP
     C*** END ADD *** 07/08/06 ****************************************
     C*
     C                   MOVE      '837'         XXTSET
     C     EXKY          CHAIN     PEX01                              21
     C*
     C*  Override file FTABL to the correct state.
     C*
     C                   MOVEL     QCA(1)        @@CMD
     C                   MOVEL     QQJST         XXST
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   OPEN      FTABL
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    #CLTID            3
    C***** Beg Dlt ***** 05/24/16 ***********************************
     C***** Beg Add ***** 12/02/15 ***********************************
      /Free
          //If ##WCIF='Y';
          //Open WMCML;
          //  Chain CmKy1 PCM03;
          //Endif;
      /End-Free
     C***** End Add ***** 12/02/15 ***********************************
    C***** End Dlt ***** 05/24/16 ***********************************
     C***** Beg Add ***** 07/08/16 ***********************************
      /Free
          If ##WCIF='Y';
            Open WMCML1;
          Endif;
      /End-Free
     C***** End Add ***** 07/08/16 ***********************************
     C*
     C                   ENDSR
     C*================================================================
     C* GETDB - This subroutine retrieves the data base files needed
     C*         to load the 837 files.
     C*================================================================
     C     GETDB         BEGSR
     C*
     C     #HKY          CHAIN     PEO01                              99
     C   99              GOTO      SKIP
     C*
     C*** BEGIN ADD *** 11/28/00 **************************************
     C*  Get the Group information
     C*
     C     FNKY          CHAIN     PFN01                              99
B01  C     *IN99         IFEQ      '1'
     C                   MOVE      *BLANKS       FNNAME
     C                   MOVE      *ZEROS        FNPZIP
     C                   MOVE      *ZEROS        FNMZIP
E01  C                   END
     C*
     C     F2KY          CHAIN     PF201                              99
B01  C     *IN99         IFEQ      '1'
     C                   MOVE      *ZEROS        F29N01
E01  C                   END
     C*** END ADD ***** 11/28/00 **************************************
     C*
     C     CMKY          CHAIN     PCM01                              99
B01  C     *IN99         IFEQ      '1'
     C                   MOVE      *ZEROS        CMACDT
     C                   MOVE      *ZEROS        CMSS#
     C                   MOVE      *BLANKS       CMSCL#
E01  C                   END
     C*
     C     CDKY          CHAIN     PCD01                              99
B01  C     *IN99         IFEQ      '1'
     C                   MOVE      *ZEROS        CDDTOB
     C                   MOVE      *BLANKS       CDSEX
     C                   MOVE      *BLANKS       CDMSTS
E01  C                   END
     C*
     C     EMKY          CHAIN     PEM01                              99
B01  C     *IN99         IFEQ      '1'
     C                   MOVE      *ZEROS        EMFEIN
E01  C                   END
     C*
     C*** BEG ADD *** 02/13/02 ****************************
     C     #HKY1         CHAIN     PES01                              99
     C*** END ADD *** 02/13/02 ****************************
     C     #HKY1         CHAIN     PEA01                              99
     C     #HKY          CHAIN     PED01                              99
     C     #HKY          CHAIN     PEH01                              99
     C     #HKY          CHAIN     PEP01                              99
     C     #HKY          CHAIN     PEU01                              99
     C     #HKY          CHAIN     PDD01                              99
     C     #HKY          CHAIN     PDH01                              99
     C     #HKY          CHAIN     PDP01                              99
     C     #HKY          CHAIN     PDU01                              99
     C*** BEG ADD ***** 08/11/11 **************************************
     C                   MOVEL(P)  EONTWK        KKNWCD
     C                   CLEAR                   PNW01
     C*
     C     NWKY          CHAIN     PNW01                              99
     C*** END ADD ***** 08/11/11 **************************************
     C***** Beg Add ***** 05/24/16 ***********************************
      /Free
          If ##WCIF='Y';
        //*** Beg Del *** 07/08/16 ************************************
        //*** Open WMCML1;
        //*** End Del *** 07/08/16 ************************************
      /End-Free

     C* Determine if Standalone Account
     C*
     C                   move      qqco#         #1co#             3
     C                   movel     easub#        ##sub#            5
     C                   movel     eafnd         #1fnd             3
     C                   call      'FA701R'
     C                   parm      ' '           ##rtcd            1
     C                   parm                    #1co#
     C                   parm                    ##sub#
     C                   parm                    #1fnd
     C                   parm      ' '           ##outs            1
     C*
     C     ##outs        ifeq      'N'
     C*
     C* Get WCMS Fund Number
     C*
     C                   move      *blanks       ##wcf#            3
     C                   call      'FA700R'
     C                   parm      ' '           ##rtcd
     C                   parm                    #1co#
     C                   parm                    ##wcf#
     C                   parm                    ##sub#
     C                   parm                    #1fnd
     C*
     C                   movel     ##wcf#        #1fnd
     C                   movel     ##wcf#        XXFND
     C                   endif                                                  ##OUTS=N
     C*
      /Free
            XXFYR = eafyr ;
            XXCASE= eacase;
            Chain CmKy1 PCM03;
          Endif;
      /End-Free
     C***** End Add ***** 05/24/16 ***********************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBAR - This subroutine loads the 837 data array used for the
     C*          837 edit pgm.
     C*================================================================
     C     MVDBAR        BEGSR
     C*
     C*    - Trans set ID and MTC
     C*
     C                   MOVEL     LGTSET        DN(1)
     C                   MOVEL     LGMTC         DN(2)
     C*
     C*    - State Claim #
     C*
     C*** BEG DEL *** 11/02/06 ****************************************
     C******************** MOVELCMSCL#    DN,5
     C*** END DEL *** 11/02/06 ****************************************
     C*** BEG ADD *** 08/11/11 ****************************************
     C                   MOVEL     CMSCL#        DN(5)
     C*** END ADD *** 08/11/11 ****************************************
     C*
     C*    - Insurer(Group) FEIN, name and postal code
     C*
     C*** BEGIN DLT *** 11/28/00 **************************************
     C**********           MOVELCOFEIN    DN,6
     C**********           MOVELCONAME    DN,7
     C********** COZIP     IFNE *ZEROS
     C**********           MOVELCOZIP     DN,616
     C**********           ENDIF
     C*** END DLT ***** 11/28/00 **************************************
     C*** BEGIN ADD *** 11/28/00 **************************************
     C                   MOVEL     F29N01        DN(6)
     C*
     C                   MOVEL     FNNAME        DN(7)
B01  C     FNPZIP        IFNE      *ZEROS
     C                   MOVEL     FNPZIP        DN(616)
X01  C                   ELSE
B02  C     FNMZIP        IFNE      *ZEROS
     C                   MOVEL     FNMZIP        DN(616)
E02  C                   ENDIF
E01  C                   ENDIF
     C*** END ADD ***** 11/28/00 **************************************
     C*** BEG ADD *** 07/08/06 ****************************************
     C*
     C* Use Sender fein/zip for the insurer
     C*
B01  C     #CLTID        IFEQ      '025'
     C*
     C                   MOVEL(P)  XXSEIN        DN(6)
     C                   MOVEL     CONAME        DN(7)
     C                   MOVEL(P)  XXSZP         DN(616)
     C*
E01  C                   ENDIF
     C*** END ADD *** 07/08/06 ****************************************
     C*
     C*    - Claims administrator (Company) FEIN, name and postal code
     C*
     C                   MOVEL     COFEIN        DN(187)
     C                   MOVEL     CONAME        DN(188)
B01  C     COZIP         IFNE      *ZEROS
     C                   MOVEL     COZIP         DN(14)
E01  C                   ENDIF
     C*
B01  C     #CLTID        IFEQ      '025'
     C*
     C                   MOVEL     C1FEIN        DN(187)
     C                   MOVEL     C1NAME        DN(188)
B02  C     COZIP         IFNE      *ZEROS
     C                   MOVEL     C1ZIP         DN(14)
E02  C                   ENDIF
     C*
E01  C                   ENDIF
     C*
     C*    - Claim Administrator Claim Number (co#/sub#/fnd/fyr/case)
     C*
     C                   MOVEL(P)  XXCO#         XCACLC           21
     C                   MOVE      LGSUB#        XCSUB             5
     C                   MOVE      LGFND         XCFND             3
     C                   MOVE      LGFYR         XCFYR             3
     C                   MOVE      LGCASE        XCCASE            7
     C*
     C*** BEG DEL *** 11/02/06 ****************************************
     C*******************  CAT  XCSUB:0   XCACLC
     C*** END DEL *** 11/02/06 ****************************************
    C***** Beg Add *** 12/02/15 **************************************
     C     #CLTID        IFEQ      '025'
     C     WCPDS#        ANDNE     *Blanks
     C                   MOVEL     WCPDS#        DN(15)
     C                   ELSE
    C***** End Add *** 12/02/15 **************************************
     C                   CAT       XCFND:0       XCACLC
     C                   CAT       XCFYR:0       XCACLC
     C                   CAT       XCCASE:0      XCACLC
     C                   MOVEL     XCACLC        DN(15)
    C***** Beg Add *** 12/02/15 **************************************
     C                   ENDIF
    C***** End Add *** 12/02/15 **************************************
     C*
     C*    - MCO info
     C*********            MOVEL          DN,209
     C*********            MOVEL          DN,708
     C*********            MOVEL          DN,709
     C*********            MOVEL          DN,710
     C*********            MOVEL          DN,711
     C*********            MOVEL          DN,712
     C*********            MOVE *BLANKS   DN,713           MCO country code
     C*********            MOVEL          DN,704
     C*
     C*    - Employer FEIN
     C*
     C                   MOVEL     EMFEIN        DN(16)
     C*
     C*    - Employer Name & Address
     C*
     C*
B01  C     EMDBA         IFNE      *BLANKS
     C                   MOVEL     EMDBA         DN(18)
X01  C                   ELSE
B02  C     EMNAME        IFNE      *BLANKS
     C                   MOVEL     EMNAME        DN(18)
E02  C                   ENDIF
E01  C                   ENDIF
     C*
B01  C     EMPAD1        IFEQ      *BLANKS
     C                   MOVEL     EMMAD1        DN(19)
     C                   MOVEL     EMMAD2        DN(20)
     C                   MOVEL     EMMCTY        DN(21)
     C                   MOVEL     EMMST         DN(22)
B02  C     EMMZIP        IFNE      *ZEROS
     C                   MOVEL     EMMZIP        DN(23)
E02  C                   ENDIF
X01  C                   ELSE
     C                   MOVEL     EMPAD1        DN(19)
     C                   MOVEL     EMPAD2        DN(20)
     C                   MOVEL     EMPCTY        DN(21)
     C                   MOVEL     EMPST         DN(22)
B02  C     EMPZIP        IFNE      *ZEROS
     C                   MOVEL     EMPZIP        DN(23)
E02  C                   ENDIF
E01  C                   ENDIF
     C*
     C                   MOVE      *BLANKS       DN(159)                        employer phone#
     C*
     C*    - Claimant information
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(31)                         date of injury
     C*
     C                   MOVEL     CMSS#         DN(42)                         SS#
     C                   MOVEL     CDLSN         DN(43)                         last name
     C                   MOVEL     CDFSN         DN(44)                         first name
     C                   MOVEL     CDMID         DN(45)                         middle initial
     C                   MOVE      *BLANKS       DN(255)                        last name suffix
     C                   MOVEL     CDADR1        DN(46)                         address line 1
     C                   MOVEL     CDADR2        DN(47)                         address line 2
     C                   MOVEL     CDCTY         DN(48)                         city
     C                   MOVEL     CDST          DN(49)                         state
     C                   MOVEL     CDZIP         DN(50)                         postal code
     C*** BEG ADD *** 02/18/05 ****************************************
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(52)                         date of birth
     C*
B01  C     CDSEX         IFNE      *BLANKS
     C                   MOVEL     CDSEX         DN(53)                         gender
X01  C                   ELSE
     C                   MOVEL     'U'           DN(53)
E01  C                   ENDIF
     C*
B01  C     CDADR3        IFEQ      *BLANKS
     C                   MOVEL     'USA'         DN(155)                        employee country
X01  C                   ELSE
     C                   MOVEL     CDCTRY        DN(155)                        employee country
E01  C                   ENDIF
     C                   MOVEL     'USA'         DN(164)                        employer country
     C*
     C*** END ADD *** 02/18/05 ****************************************
     C                   MOVE      *BLANKS       DN(152)                        employment VISA
     C                   MOVE      *BLANKS       DN(153)                        green card
     C*
     C*    - Bill header info
     C*
     C*** BEG ADD *** 02/18/05 ****************************************
     C                   MOVEL     EOCHRG        DN(501)                        total amt charged
     C                   MOVEL     'N'           DN(506)                        prov sig on file
B01  C     EONTWK        IFNE      *BLANKS
     C                   MOVEL     'Y'           DN(507)                        prov agree code
X01  C                   ELSE
     C                   MOVEL     'N'           DN(507)                        prov agree code
E01  C                   ENDIF
     C                   MOVEL     DN(507)       DN(742)
     C                   MOVEL     EUPRD1        DN(550)                        princ proc date
B01  C     EOBTYP        IFEQ      'X'
     C                   MOVE      '99'          DN(555)
E01  C                   ENDIF
B01  C     EOBTYP        IFEQ      'H'
     C                   MOVEL     EUADHR        DN(622)
     C                   MOVEL     EUDCHR        DN(623)
     C                   MOVE      DUPOS         DN(555)
     C*** BEG ADD *** 07/08/06 ****************************************
B02  C     DUPOS         IFEQ      ' 99'
     C                   MOVE(P)   '12'          DN(555)
E02  C                   ENDIF
     C*** END ADD *** 07/08/06 ****************************************
E01  C                   ENDIF
B01  C     EOBTYP        IFEQ      'N'
     C                   MOVE      DHPOS         DN(555)
     C*** BEG ADD *** 07/08/06 ****************************************
B02  C     DHPOS         IFEQ      ' 99'
     C                   MOVE(P)   '11'          DN(555)
E02  C                   ENDIF
     C*** END ADD *** 07/08/06 ****************************************
E01  C                   ENDIF
B01  C     EOBTYP        IFEQ      'D'
     C                   MOVEL     DDPOS         XXPOS             2
     C                   MOVE      XXPOS         DN(555)
     C*** BEG ADD *** 07/08/06 ****************************************
B02  C     XXPOS         IFEQ      '99'
     C                   MOVE(P)   '11'          DN(555)
E02  C                   ENDIF
     C*** END ADD *** 07/08/06 ****************************************
E01  C                   ENDIF
     C*** END ADD *** 02/18/05 ****************************************
     C                   MOVE      *BLANKS       DN(624)                        initial amt paid
     C*** BEG CHG *** 08/11/11 ****************************************
     C******************** MOVELEOPAID    DN,516           total amt paid
     C                   MOVEL     EAPAID        DN(516)                        total amt paid
     C*** END CHG *** 08/11/11 ****************************************
     C                   Z-ADD     EOCSDT        SRDTI
     C*** BEG ADD *** 02/18/05 ****************************************
B01  C     EOCSDT        IFEQ      *ZEROS
     C     EOAST         ANDEQ     'TX'
     C                   Z-ADD     EOCRDT        SRDTI
E01  C                   ENDIF
     C*** END ADD *** 02/18/05 ****************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(510)                        date of bill
     C*** BEG ADD *** 07/08/06 ****************************************
     C*** END ADD *** 07/08/06 ****************************************
B01  C     LGRCN         IFNE      *ZEROS
     C*** BEG ADD *** 09/12/06 ****************************************
B02  C     ESRSDT        IFNE      *ZEROS
     C*** END ADD *** 09/12/06 ****************************************
     C                   Z-ADD     ESRSDT        SRDTI
     C*** BEG ADD *** 09/12/06 ****************************************
X02  C                   ELSE
     C                   Z-ADD     ESRRDT        SRDTI
E02  C                   ENDIF
     C*** END ADD *** 09/12/06 ****************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(510)                        Dt received bill
E01  C                   ENDIF
     C*** BEG ADD *** 02/13/02 ****************************
B01  C     LGRCN         IFNE      *ZEROS
     C                   Z-ADD     ESRRDT        SRDTI
X01  C                   ELSE
     C*** BEG END *** 02/13/02 ****************************
     C                   Z-ADD     EOCRDT        SRDTI
     C*** BEG ADD *** 02/13/02 ****************************
E01  C                   ENDIF
     C*** BEG END *** 02/13/02 ****************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(511)                        Dt received bill
     C*
     C*** BEGIN DLT *** 02/07/00 **************************************
     C* field EAWCPD format is mmddyycc, so no coversion is necessary
     C**********           MOVELEAWCPD    DN,512           date paid bill
     C*** END   DLT *** 02/07/00 **************************************
     C*** BEGIN ADD *** 02/07/00 **************************************
     C                   Z-ADD     EAWCPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         XXWCPD            8
     C                   MOVEL     XXWCPD        DN(512)                        date paid bill
     C*** END   ADD *** 02/07/00 **************************************
     C*** Begin add *** 08/19/03 **************************************
     C*** Begin add *** 06/30/04 **************************************
     C                   MOVE      *BLANKS       QQCMDT
B01  C     XXWCPD        IFNE      '00000000'
     C     XXWCPD        ANDNE     *BLANKS
     C*** End add ***** 06/30/04 **************************************
     C*** BEG ADD *** 03/02/07 ****************************************
B02  C     ##WCIF        IFEQ      'Y'
     C*** END ADD *** 03/02/07 ****************************************
     C                   CALL      'WA050R'
     C                   PARM                    QQCO#
     C                   PARM      XCFND         QQFND             3
     C                   PARM      XXWCPD        QQTRDT            8
     C                   PARM      *BLANKS       QQCMDT            8
     C*** BEG ADD *** 03/02/07 ****************************************
E02  C                   ENDIF
     C*** END ADD *** 03/02/07 ****************************************
     C*** Begin add *** 06/30/04 **************************************
E01  C                   ENDIF
     C*** End add ***** 06/30/04 **************************************
     C*
B01  C     QQCMDT        IFNE      *BLANKS
     C                   MOVEL     QQCMDT        DN(512)
E01  C                   ENDIF
     C*** End add ***** 08/19/03 **************************************
     C*
     C                   MOVEL     LGMTC         DN(508)                        bill subm reason cod
     C                   MOVEL     'B'           DN(503)                        billing format code
B01  C     EOBTYP        IFEQ      'H'
     C*** BEG DEL *** 07/08/06 ****************************************
     C*******************  MOVELEUACCT    DN,517           patient acct#
     C*** END DEL *** 07/08/06 ****************************************
     C     2             SUBST     EUBLTY:1      DN(504)                        facility code
     C*** BEG ADD *** 02/18/05 ****************************************
     C     1             SUBST     EUBLTY:3      DN(505)                        bill freq type code
     C*** END ADD *** 02/18/05 ****************************************
     C                   MOVEL     'A'           DN(503)                        billing format code
     C                   Z-ADD     EUADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(513)                        admission date
     C                   Z-ADD     EUDCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(514)                        discharge date
     C*** BEG ADD *** 01/16/07 ****************************************
     C                   MOVEL     EUCTCD        DN(515)                        Contract type code
     C*** END ADD *** 01/16/07 ****************************************
     C                   MOVEL     EUDI1         DN(521)                        principle diag code
     C                   MOVEL     EUDI1         DN(522)                        ICD-9 diagnosis code
     C*** BEG CHG *** 02/18/05 ****************************************
     C******************** MOVELEUADIA    DN,535           admitting diag code
     C*** BEG CHG *** 07/08/06 ****************************************
     C******************** MOVELEUDIA1    DN,535           admitting diag code
     C                   MOVEL     EUDI1         DN(535)                        admitting diag code
     C*** END CHG *** 07/08/06 ****************************************
     C*** END CHG *** 02/18/05 ****************************************
     C*** BEG ADD *** 07/03/08 ****************************************
     C                   MOVEL     EUDRG#        DN(518)                        DRG Code
     C*** END ADD *** 07/03/08 ****************************************
     C*** BEG ADD *** 02/18/05 ****************************************
B02  C     EUPCM         IFEQ      '9'
     C***** Beg Chg *** 05/29/14 **************************************
     C                   MOVEL     EUPI1         DN(525)                        ICD-9 prin proc code
     C***                MOVEL     EUPRC1        DN(525)                        ICD-9 prin proc code
     C***** End Chg *** 05/29/14 **************************************
E02  C                   ENDIF                                                              roc code
     C*** END ADD *** 02/18/05 ****************************************
     C***** Beg Chg *** 05/29/14 **************************************
     C                   MOVEL     EUPI1         DN(626)                        HCPCS prin proc code
     C                   MOVEL     EUPI2         DN(714)                        HCPCS proc bill code
     C***                MOVEL     EUPRC1        DN(626)                        HCPCS prin proc code
     C***                MOVEL     EUPRC2        DN(714)                        HCPCS proc bill code
     C***** End Chg *** 05/29/14 **************************************
E01  C                   END
B01  C     EOBTYP        IFEQ      'N'
     C*** BEG DEL *** 07/08/06 ****************************************
     C*******************  MOVELEHACCT    DN,517           patient acct#
     C*** END DEL *** 07/08/06 ****************************************
     C                   MOVEL     'B'           DN(503)                        billing format code
     C                   MOVEL     EHDI1         DN(521)                        principle diag code
     C                   MOVEL     EHDI1         DN(522)                        ICD-9 diagnosis code
     C                   MOVE      *BLANKS       DN(525)                        ICD-9 prin proc code
     C                   MOVE      *BLANKS       DN(627)                        ICD-9 proc code
E01  C                   END
B01  C     EOBTYP        IFEQ      'X'
     C                   MOVEL     EPBTCD        DN(502)                        billing type code
     C*** BEG ADD *** 09/12/06 ****************************************
B02  C     EPBTCD        IFEQ      *BLANKS                                                   code
     C                   MOVEL     'RX'          DN(502)                        billing type code
E02  C                   ENDIF
     C*** END ADD *** 09/12/06 ****************************************
     C                   MOVEL     'B'           DN(503)                        billing format code
     C*** BEGIN ADD *** 02/12/01 **************************************
B02  C     EPACCT        IFEQ      *BLANKS
     C                   MOVEL     CMSS#         EPACCT
E02  C                   ENDIF
     C*** END ADD ***** 02/12/01 **************************************
     C*** BEG DEL *** 07/08/06 ****************************************
     C*******************  MOVELEPACCT    DN,517           patient acct#
     C*** END DEL *** 07/08/06 ****************************************
E01  C                   END
B01  C     EOBTYP        IFEQ      'D'
     C*** BEGIN ADD *** 02/12/01 **************************************
B02  C     EDACCT        IFEQ      *BLANKS
     C                   MOVEL     CMSS#         EDACCT
E02  C                   ENDIF
     C*** END ADD ***** 02/12/01 **************************************
     C*** BEG ADD ***** 02/18/05 **************************************
     C                   MOVEL     EDDI1         DN(521)                        principle diag code
     C                   MOVEL     EDDI1         DN(522)                        ICD-9 diagnosis code
     C*** END ADD ***** 02/18/05 **************************************
     C*** BEG DEL *** 07/08/06 ****************************************
     C*******************  MOVELEDACCT    DN,517           patient acct#
     C*** END DEL *** 07/08/06 ****************************************
E01  C                   END
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDT
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODT
     C                   MOVEL     XXFRDT        XXWK16           16
     C                   MOVE      XXTODT        XXWK16
     C                   MOVEL     XXWK16        DN(509)                        service date range
     C                   MOVEL     EOEOB#        DN(500)                        unique bill ID#
     C*
     C*    - Billing provider info
     C*
     C                   MOVEL     EOFEIN        DN(529)                        FEIN
     C*
     C     PDKY          CHAIN     PPD01                              99
B01  C     *IN99         IFEQ      '0'
     C                   MOVEL     PDNAME        DN(528)                        last name
     C                   MOVEL     PDFSN         DN(529)                        first name
     C*** BEG ADD *** 02/18/05 ****************************************
     C                   MOVEL     PDNAME        DN(690)                        last name
     C*
B02  C     EOBTYP        IFEQ      'X'
     C                   MOVEL(P)  EPPLIC        DN(701)                        license#
     C*** Begin Add *** 08/06/13 **************************************
E02  C                   ENDIF
     C*** End   Add *** 08/06/13 **************************************
     C*** BEG ADD *** 11/13/05 ****************************************
B02  C     EOSINV        IFNE      *BLANKS
     C                   MOVEL     EOSINV        DN(523)                        unique bill id
X02  C                   ELSE
     C                   MOVEL     EOEOB#        DN(523)                        unique bill id
E02  C                   ENDIF
     C*** END ADD *** 11/13/05 ****************************************
     C*** BEG ADD *** 01/16/07 ****************************************
B02  C     LGJST         IFEQ      'CA'
     C     CACHRG        ANDGT     0
     C                   MOVEL     EOBLID        DN(523)                        unique prov bill id
E02  C                   ENDIF
     C*** END ADD *** 01/16/07 ****************************************
     C                   MOVEL     PDNAME        DN(690)                        last name
     C*** Begin Del *** 08/06/13 **************************************
     C**********         ENDIF
     C*** End   Del *** 08/06/13 **************************************
     C*** END ADD *** 02/18/05 ****************************************
     C                   MOVE      *BLANKS       DN(531)                        last name suffix
     C                   MOVEL     PDADR1        DN(538)                        primary address
     C                   MOVEL     PDADR2        DN(539)                        secondary address
     C                   MOVEL     PDCTY         DN(540)                        city
     C                   MOVEL     PDST          DN(541)                        state
     C                   MOVEL     PDZIP         DN(542)                        postal code
     C                   MOVEL     PDFEIN        DN(629)                        FEIN
     C*** BEG ADD *** 01/16/07 ****************************************
     C*
     C** If billing lic# different from rendering lic#
     C*
B02  C     EOBLIC        IFNE      EPFLIC
     C     EOAST         ANDEQ     'CA'
     C                   MOVEL(P)  EOBLIC        DN(630)
E02  C                   ENDIF
     C*** BEG ADD *** 12/13/08 ****************************************
     C*
     C* Billing provider license numbers
     C*
B02  C                   SELECT
     C*
S01  C     EOBTYP        WHENEQ    'N'
     C                   MOVEL(P)  EHFLIC        DN(630)
     C*
S02  C     EOBTYP        WHENEQ    'D'
     C                   MOVEL(P)  EDFLIC        DN(630)
     C*
E02  C                   ENDSL
     C*** END ADD *** 12/13/08 ****************************************
     C*
     C*** END ADD *** 01/16/07 ****************************************
     C*** BEG ADD *** 02/18/05 ****************************************
B02  C     P1MCTR        IFEQ      *BLANKS
     C                   MOVEL     'USA'         DN(569)
X02  C                   ELSE
     C                   MOVEL     P1MCTR        DN(569)
E02  C                   ENDIF
     C*** END CHG *** 02/18/05 ****************************************
     C*
     C*    - Billing provider specialty code.
     C*
     C*** BEG CHG *** 01/30/08 ****************************************
     C*
     C*********  PDKY      CHAINPP101                99
     C**N99****            MOVELP1SPC3    DN,537
     C*
     C                   MOVE      *BLANK        DN(537)
     C     PDKY          CHAIN     PP101                              99
B02  C     *IN99         IFEQ      *OFF
     C                   MOVEL     P1SPC3        DN(537)
     C*
     C* IF THE ADJUDICATION STATE = CA,
     C* USE THE GENERIC TABLE ID, CASC,
     C* AND THE Specialty code 3 FROM THE PROVIDER MASTER,
     C* TO LOAD THE FIELD GTCF01
     C* INTO THE DATA ELEMENT DN,537.
     C*
B03  C     P1ST          IFEQ      'CA'
     C                   MOVEL(P)  'CASC'        X1TBID
     C                   MOVEL(P)  P1SPC3        X1KEY
     C     GTKY          CHAIN     PGT01                              99
B04  C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTCF01        DN(537)
E04  C                   END
E03  C                   END
     C*
E02  C                   END
     C*
     C*** END CHG *** 01/30/08 ****************************************
E01  C                   END
     C*
     C*    - Rendering provider info
     C*
     C                   MOVE      *BLANKS       DN(638)                        last name
     C                   MOVE      *BLANKS       DN(639)                        first name
     C                   MOVE      *BLANKS       DN(641)                        last name suffix
     C                   MOVE      *BLANKS       DN(642)                        FEIN
     C                   MOVE      *BLANKS       DN(651)                        primary spec code
     C                   MOVE      *BLANKS       DN(652)                        primary address
     C                   MOVE      *BLANKS       DN(653)                        secondary address
     C                   MOVE      *BLANKS       DN(654)                        city
     C                   MOVE      *BLANKS       DN(655)                        state
     C                   MOVE      *BLANKS       DN(656)                        postal code
     C*** BEG ADD *** 07/03/08 ****************************************
     C*** BEG CHG *** 08/11/11 ****************************************
     C******************   MOVE *BLANKS   DN,647           Rendering prov. npi#
B01  C     EOBTYP        IFEQ      'X'
     C                   MOVEL     EPPHAR        DN(647)                        Rendering prov. npi#
E01  C                   ENDIF
B01  C     EOBTYP        IFEQ      'D'
     C                   MOVEL     EDTNPI        DN(647)                        Rendering prov. npi#
E01  C                   ENDIF
     C*** END CHG *** 08/11/11 ****************************************
     C*** END ADD *** 07/03/08 ****************************************
     C*** BEG ADD *** 09/12/06 ****************************************
     C*    - Default to the billing provider
     C*
     C                   MOVEL     PDNAME        DN(638)                        last name
     C*** BEG ADD *** 01/16/07 ****************************************
B01  C     LGJST         IFEQ      'CA'
     C                   MOVEL     EORNAM        DN(638)                        rend pvdr lst/grp na
     C                   MOVE      EOZIP         DN(656)                        postal code
     C                   MOVE      EOSPCD        DN(537)                        rend prov pri spec c
E01  C                   ENDIF
     C*** END ADD *** 01/16/07 ****************************************
     C*** BEG ADD *** 05/28/08 ****************************************
     C*
     C* Rendering Provider State for TX Dental
     C*
B01  C     LGJST         IFEQ      'TX'
     C     EOBTYP        ANDEQ     'D'
     C                   MOVEL     'TX'          DN(655)                        rend pvdr state
E01  C                   ENDIF
     C*** END ADD *** 05/28/08 ****************************************
     C                   MOVEL     PDFSN         DN(639)                        first name
     C                   MOVEL     EOFEIN        DN(642)                        FEIN
     C*** BEG ADD *** 01/16/07 ****************************************
     C*** BEG DEL *** 10/30/08 ****************************************
     C***********          MOVELEPFLIC    DN,643           rend pvdr st lic#
     C*** END DEL *** 10/30/08 ****************************************
     C*** END ADD *** 01/16/07 ****************************************
     C*** BEG ADD *** 12/13/08 ****************************************
     C*
     C* Rendering provider license numbers
     C*
B01  C     LGJST         IFEQ      'TX'
     C*
B02  C                   SELECT
     C*
S01  C     EOBTYP        WHENEQ    'N'
     C                   MOVEL(P)  EHRNLC        DN(643)
     C*** Beg Add *** 01/10/12 ****************************************
     C                   MOVEL(P)  EHRFLC        DN(695)
     C*** End Add *** 01/10/12 ****************************************
     C*
S02  C     EOBTYP        WHENEQ    'D'
     C                   MOVEL(P)  EDRNLC        DN(643)
     C*** Beg Add *** 01/10/12 ****************************************
     C                   MOVEL(P)  EDRFLC        DN(695)
     C*** End Add *** 01/10/12 ****************************************
     C*
E02  C                   ENDSL
     C*
E01  C                   ENDIF
     C*** END ADD *** 12/13/08 ****************************************
     C*
     C*** END ADD *** 09/12/06 ****************************************
     C*** BEG ADD *** 02/18/05 ****************************************
     C*
     C*    - Referring provider info
     C*
     C                   MOVEL     EORLNM        DN(690)                        last name
     C                   MOVEL     EORFNM        DN(691)                        first name
     C                   MOVEL     *BLANKS       DN(692)                        middle initial
     C                   MOVE      *BLANKS       DN(693)                        last name suffix
     C                   MOVEL     EORFEI        DN(694)                        FEIN
     C*** BEG DEL *** 10/30/08 ****************************************
     C**********           MOVELEORLIC    DN,695    P      state license #
     C*** END DEL *** 10/30/08 ****************************************
     C*** END ADD *** 02/18/05 ****************************************
     C*** BEG ADD *** 07/08/06 ****************************************
     C*
     C* DEFAULT IN ORDER TO SEND HISTORICAL
     C*
B01  C     EORLNM        IFEQ      *BLANKS
     C                   MOVEL     PDNAME        DN(690)                        last name
     C                   MOVEL(P)  'DR'          DN(691)                        first name
     C                   MOVEL     *BLANKS       DN(692)                        middle initial
     C                   MOVE      *BLANKS       DN(693)                        last name suffix
     C                   MOVEL     EOFEIN        DN(694)                        FEIN
     C*** BEG DEL *** 10/30/08 ****************************************
     C**********           MOVELEUFLIC    DN,695    P      state license #
     C*** END DEL *** 10/30/08 ****************************************
E01  C                   ENDIF
     C*** END ADD *** 07/08/06 ****************************************
     C*** BEG ADD *** 12/13/08 ****************************************
     C*
     C* Referring provider license numbers
     C*
B01  C     LGJST         IFEQ      'TX'
     C*
B02  C                   SELECT
     C*
S01  C     EOBTYP        WHENEQ    'H'
     C                   MOVEL(P)  EURLIC        DN(695)
     C*** BEGIN ADD *** 02/25/09 **************************************
     C***¢****** EOBTYP    WHEQ 'X'
     C******&***           MOVELEPPDEA    DN,695    P
     C*** END   ADD *** 02/25/09 **************************************
     C*
E02  C                   ENDSL
     C*
E01  C                   ENDIF
     C*** END ADD *** 12/13/08 ****************************************
     C*** BEG ADD *** 11/13/05 ****************************************
     C*
     C*    - Facility info
     C*
     C                   MOVEL(P)  EOPNAM        DN(678)                        facility name
     C*** END ADD *** 11/13/05 ****************************************
     C*
     C*** BEG ADD *** 01/16/07 ****************************************
     C*
B01  C     DN(503)       IFEQ      'A'
     C                   MOVEL(P)  EOFFEI        DN(679)                        facility FEIN
B02  C     LGJST         IFEQ      'CA'
     C                   MOVEL(P)  EOFNAM        DN(678)                        facility name
     C                   MOVEL(P)  EOFZIP        DN(688)                        facility postal code
E02  C                   ENDIF
     C*
X01  C                   ELSE
     C*
B02  C     DN(503)       IFEQ      'B'
     C     EOBTYC        ANDEQ     'DM'
     C     EOBTYC        OREQ      'RX'
     C     EOBTYC        OREQ      'MO'
     C                   MOVEL(P)  EOBTYC        DN(502)                        billing type code
E02  C                   ENDIF
E01  C                   ENDIF
     C*
     C*** END ADD *** 01/16/07 ****************************************
     C*
     C*    - Managed care organization info
     C*
     C                   MOVE      *BLANKS       DN(209)                        MCO name
     C                   MOVE      *BLANKS       DN(704)                        MCO FEIN
     C                   MOVE      *BLANKS       DN(708)                        MCO primary address
     C                   MOVE      *BLANKS       DN(709)                        MCO secondary addres
     C                   MOVE      *BLANKS       DN(710)                        MCO city
     C                   MOVE      *BLANKS       DN(711)                        MCO state
     C                   MOVE      *BLANKS       DN(712)                        MCO postal code
     C*
     C*** BEG ADD *** 08/11/11 ****************************************
B01  C     NWFL02        IFEQ      'H'
     C*
     C                   MOVEL(P)  NWNWCD        DN(208)
     C                   MOVEL(P)  NWNAME        DN(209)
     C*
X01  C                   ELSE
     C                   MOVE      *BLANKS       DN(208)
     C                   MOVE      *BLANKS       DN(209)
     C*
E01  C                   ENDIF                                                  NWFL02 ifeq
     C*** END ADD *** 08/11/11 ****************************************
     C*
     C*    - Bill adjustment reasons
     C*
     C                   MOVE      *BLANKS       DN(543)                        group code
     C                   MOVE      *BLANKS       DN(544)                        reason code
     C*
     C*    - Line item info
     C*
     C*** BEG ADD *** 02/18/05 ****************************************
     C                   MOVEL(P)  CALIN#        DN(547)                        line #
     C*** END ADD *** 02/18/05 ****************************************
B01  C     EOBTYP        IFEQ      'N'
     C*** BEG DEL *** 02/18/05 ****************************************
     C*******************  MOVELDHDSEQ    DN,547           line #
     C*** END DEL *** 02/18/05 ****************************************
     C                   MOVEL     DHPRDS        DN(551)                        procedure desc
     C*** BEG CHG *** 02/18/05 ****************************************
     C******************** MOVE *BLANKS   DN,552           total charged
     C                   MOVEL     DHCHRG        DN(552)                        total charged
     C*** END CHG *** 02/18/05 ****************************************
     C*** BEG ADD *** 02/18/05 ****************************************
B02  C     DHBPRC        IFGE      '00100'
     C     DHBPRC        ANDLE     '01999'
     C                   MOVE      'MA'          DN(553)
     C*
X02  C                   ELSE
B03  C     DHBPRC        IFNE      *BLANKS
     C                   MOVE      'UN'          DN(553)
     C*
X03  C                   ELSE
     C                   MOVE      *BLANKS       DN(553)
     C*
E03  C                   ENDIF
     C*
E02  C                   ENDIF
     C                   MOVEL     DHUNTS        DN(554)                        units billed
     C*** END ADD *** 02/18/05 ****************************************
B02  C     DHDXR1        IFEQ      'X'
     C                   MOVEL     '1'           DN(557)                        diagnosis pointer
E02  C                   END
B02  C     DHDXR2        IFEQ      'X'
     C                   MOVEL     '2'           DN(557)                        diagnosis pointer
E02  C                   END
B02  C     DHDXR3        IFEQ      'X'
     C                   MOVEL     '3'           DN(557)                        diagnosis pointer
E02  C                   END
B02  C     DHDXR4        IFEQ      'X'
     C                   MOVEL     '4'           DN(557)                        diagnosis pointer
E02  C                   END
     C                   MOVE      *BLANKS       DN(559)                        Revenue bill code
     C                   MOVEL     DHPAID        DN(574)                        total paid
     C                   MOVEL     DHPOS         DN(600)                        POS line code
     C                   MOVEL     DHBPRC        DN(714)                        HCPCS proc code
     C                   MOVEL     DHPRCD        DN(726)                        HCPCS paid code
E01  C                   END
     C*
B01  C     EOBTYP        IFEQ      'H'
     C*** BEG DEL *** 02/18/05 ****************************************
     C*******************  MOVELDUDSEQ    DN,547           line #
     C*** END DEL *** 02/18/05 ****************************************
     C                   MOVE      *BLANKS       DN(551)                        procedure desc
     C*** BEG CHG *** 02/18/05 ****************************************
     C******************** MOVE *BLANKS   DN,552           total charged
     C                   MOVEL     DUCHRG        DN(552)                        total charged
     C*** END CHG *** 02/18/05 ****************************************
     C                   MOVE      *BLANKS       DN(557)                        diagnosis pointer
     C*** BEGIN CHG *** 04/07/09 **************************************
     C**********           MOVELDURVCD    DN,559           Revenue paid code
     C                   MOVEL     DUBRVC        DN(559)
     C*** END   CHG *** 04/07/09 **************************************
     C                   MOVEL     DUPAID        DN(574)                        total paid
     C*** BEGIN CHG *** 01/24/01 **************************************
     C******************** MOVELDUBRVC    DN,576           Revenue bill code
     C                   MOVEL     DURVCD        DN(576)                        Revenue bill code
     C*** END CHG ***** 01/24/01 **************************************
     C*** BEG ADD *** 02/18/05 ****************************************
B02  C     DURATE        IFEQ      *BLANKS
     C                   MOVEL     'DA'          DN(553)                        days units code
X02  C                   ELSE
     C                   MOVEL     'UN'          DN(553)
E02  C                   ENDIF
     C*** END ADD *** 02/18/05 ****************************************
E01  C                   END
     C*** BEG ADD *** 02/18/05 ****************************************
B01  C     EOBTYP        IFEQ      'D'
     C                   MOVEL     DDCHRG        DN(552)                        total charged
     C                   MOVEL     'UN'          DN(553)                        days units code
     C*** BEG ADD *** 02/18/05 ****************************************
     C                   MOVEL(P)  DDPAID        DN(574)                        total paid per line
     C*** END ADD *** 02/18/05 ****************************************
E01  C                   ENDIF
     C*
B01  C     EOBTYP        IFEQ      'X'
     C                   MOVEL     DPRX#         DN(561)                        presc line number
     C                   MOVEL     '1'           DN(562)                        dispensed as written
     C                   MOVEL     DPUNTS        DN(570)                        Quantity dispensed
     C                   MOVEL     DPRXDY        DN(571)                        Drugs/Supplies # day
     C*** BEG ADD *** 02/18/05 ****************************************
     C                   MOVEL(P)  DPPAID        DN(574)                        total paid per line
     C*** END ADD *** 02/18/05 ****************************************
     C*** BEG ADD *** 02/18/05 ****************************************
     C                   MOVEL(P)  DPBCDT        DN(564)                        Cost determ
     C                   MOVEL(P)  DPDFEE        DN(579)                        Disp fee
     C*** END ADD *** 02/18/05 ****************************************
     C*** BEG ADD *** 07/03/08 ****************************************
     C*** BEG DEL *** 08/11/11 ****************************************
     C******************   MOVELDPRNPI    DN,647    P      Rendering Prov. NPI#
     C*** END DEL *** 08/11/11 ****************************************
     C*** END ADD *** 07/03/08 ****************************************
     C*** BEG ADD *** 05/18/09 **************************************
     C                   MOVEL(P)  EPRFNP        DN(699)                        REFERRING PROV. NPI#
     C*** END ADD *** 05/18/09 **************************************
     C*
E01  C                   ENDIF
     C*** END ADD *** 02/18/05 ****************************************
     C*** Beg Add *** 04/03/13 **************************************
B01  C                   SELECT
S01  C                   WHEN      EOBTYP = 'H'
     C                   MOVEL     EUANPI        DN(699)
     C*
S02  C                   WHEN      EOBTYP = 'N'
     C                   MOVEL     EHRNPI        DN(699)
     C*
E01  C                   ENDSL
     C*** End Add *** 04/03/13 **************************************
     C*
     C*    - Line item adjustment reasons
     C*
     C                   MOVE      *BLANKS       DN(731)                        group code
     C                   MOVE      *BLANKS       DN(732)                        reason code
     C*
     C                   ENDSR
     C*================================================================
     C* UPDLOG - This subroutine updates the log record.
     C*================================================================
     C     UPDLOG        BEGSR
     C*
     C*   - Insurer and Claims administrator info
     C*
     C                   MOVEL     DN(7)         LGINNM
     C                   MOVEL     DN(616)       LGINZP
     C                   MOVEL     DN(6)         LGINFE
     C                   MOVEL     DN(188)       LGCANM
     C                   MOVEL     DN(14)        LGCAZP
     C                   MOVEL     DN(187)       LGCAFE
     C*
     C*   - MCO info
     C*
     C                   MOVEL     DN(209)       LGMCNM
     C                   MOVEL     DN(708)       LGMCA1
     C                   MOVEL     DN(709)       LGMCA2
     C                   MOVEL     DN(710)       LGMCCT
     C                   MOVEL     DN(711)       LGMCST
     C                   MOVEL     DN(712)       LGMCZP
     C                   MOVEL     DN(713)       LGMCCR
     C                   MOVEL     DN(704)       LGMCFE
     C*** BEG CHG *** 08/11/11 ****************************************
     C****************     MOVE *BLANKS   LGMCID
     C                   MOVEL     DN(208)       LGMCID
     C*** END CHG *** 08/11/11 ****************************************
     C*
     C*   - Employer info
     C*
     C                   MOVEL     DN(18)        LGEMNM
     C                   MOVEL     DN(16)        LGEMFE
     C                   MOVEL     DN(19)        LGEMA1
     C                   MOVEL     DN(20)        LGEMA2
     C                   MOVEL     DN(21)        LGEMCT
     C                   MOVEL     DN(22)        LGEMST
     C                   MOVEL     DN(23)        LGEMZP
     C                   MOVEL     DN(159)       XXWK12
     C                   MOVEL     XXWK12        LGEMAR
     C                   MOVE      XXWK12        LGEMPH
     C*** BEG CHG *** 02/18/05 ****************************************
     C                   MOVEL     DN(164)       LGERCT
     C*** END CHG *** 02/18/05 ****************************************
     C*
     C*   - Claimant info
     C*
     C                   MOVEL     DN(5)         LGSCL#
     C                   MOVEL     DN(42)        LGSS#
     C                   MOVEL     CMACDT        LGACDT
     C                   MOVEL     CMACTM        LGACTM
     C                   MOVE      *BLANKS       LGTYPE
     C                   MOVE      *BLANKS       LGCLTY
     C                   MOVE      *BLANKS       LGACLT
     C                   MOVEL     DN(43)        LGCLLN
     C                   MOVEL     DN(44)        LGCLFN
     C                   MOVEL     DN(45)        LGCLMI
     C                   MOVEL     DN(255)       LGCLSX
     C                   MOVEL     DN(153)       LGCLGC
     C                   MOVEL     DN(152)       LGCLVS
     C                   MOVEL     DN(46)        LGCLA1
     C                   MOVEL     DN(47)        LGCLA2
     C                   MOVEL     DN(48)        LGCLCT
     C                   MOVEL     DN(49)        LGCLST
     C                   MOVEL     DN(50)        LGCLZP
     C                   MOVEL     CDDTOB        LGDTOB
     C*** BEG CHG *** 02/18/05 ****************************************
     C******************** MOVELCDSEX     LGSEX
     C                   MOVEL     DN(53)        LGSEX
     C*** END CHG *** 02/18/05 ****************************************
     C                   MOVEL     CDMSTS        LGMSTS
     C*** BEG CHG *** 02/18/05 ****************************************
     C                   MOVEL     DN(155)       LGECTY
     C*** END CHG *** 02/18/05 ****************************************
    C***** Beg Add ***** 12/02/15 ***********************************
     C                   MOVEL     DN(15)        LGCAC#
    C***** End Add ***** 12/02/15 ***********************************
     C*
     C*   - EOMB header info
     C*
     C*** BEG ADD *** 02/18/05 ****************************************
     C*** BEG DEL *** 07/08/06 ****************************************
     C*********
     C*********  EOCSDT    IFEQ 0
     C*********  EOAST     ANDEQ'TX'
     C*********            MOVE EOCRDT    LGCSDT
     C*********            ELSE
     C****END*ADD *** 02/18/05 ****************************************
     C*********            MOVE EOCSDT    LGCSDT
     C****BEG*ADD *** 02/18/05 ****************************************
     C*********            ENDIF
     C*** END DEL *** 07/08/06 ****************************************
     C*** END ADD *** 02/18/05 ****************************************
     C*** BEG ADD *** 07/08/06 ****************************************
     C                   MOVEL     DN(510)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGCSDT                         date of bill
B01  C     EORCN         IFGT      0
     C*** BEG CHG *** 09/12/06 ****************************************
     C******************** Z-ADDESRSDT    LGCSDT
     C                   MOVEL     SRDTO         LGRSDT                         date of bill
     C*** END CHG *** 09/12/06 ****************************************
E01  C                   ENDIF
     C                   MOVE      EOCRDT        LGCRDT
     C*** BEG ADD *** 02/13/02 ****************************
     C                   MOVE      ESRRDT        LGRRDT
     C*** BEG END *** 02/13/02 ****************************
     C                   MOVE      *BLANKS       LGAPDT
     C                   Z-ADD     EAWCPD        SRDTI
     C*** BEGIN CHG *** 02/07/00 **************************************
     C**********           CALL 'SRCVTDAT'DTCYMD
     C                   CALL      'SRCVTDAT'    DTCYM2
     C*** END   CHG *** 02/07/00 **************************************
     C                   MOVEL     SRDTO         LGPDDT
     C*** Begin add *** 08/19/03 **************************************
B01  C     QQCMDT        IFNE      *BLANKS
     C                   MOVE      QQCMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGPDDT
E01  C                   ENDIF
     C*** End add ***** 08/19/03 **************************************
     C*** BEG ADD *** 09/12/06 ****************************************
     C*
     C* When no payment was made then report the EOB Print date
     C*
B01  C     LGPDDT        IFEQ      *ZEROS
     C*
B02  C     EAEPDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     EAEPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         LGPDDT
     C*
X02  C                   ELSE
     C                   Z-ADD     EADT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGPDDT
     C*
E02  C                   ENDIF
     C*
E01  C                   ENDIF
     C*** END ADD *** 09/12/06 ****************************************
     C                   MOVE      EAWCPC        LGPDCK
     C                   MOVEL     DN(502)       LGBLTC
     C                   MOVEL     DN(503)       LGBLFC
     C                   MOVE      *BLANKS       LGHSCD
     C*** BEG CHG *** 02/18/05 ****************************************
     C******************** MOVE *BLANKS   LGFREQ
     C                   MOVEL     DN(505)       LGFREQ
     C*** END CHG *** 02/18/05 ****************************************
     C                   MOVE      *BLANKS       LGBENE
     C                   MOVEL     EOCERT        LGPAUT
     C                   MOVE      EOEXT         LGPAUT
     C                   MOVE      *BLANKS       LGDTRT
     C                   MOVE      EOFRDT        LGFRDT
     C                   MOVE      EOTODT        LGTODT
     C                   MOVE      EOCHRG        LGCHRG
     C                   MOVE      EOMRA         LGMRA
     C                   MOVE      EOOTHR        LGOTHR
     C                   MOVE      EOOTH2        LGOTH2
     C                   MOVE      EONDIS        LGNDIS
     C*** BEG CHG *** 02/20/12 ****************************************
     C*****                MOVELDN,516    LGPAID
     C                   MOVEL     EAPAID        LGPAID
     C*** END CHG *** 02/20/12 ****************************************
     C                   MOVE      *BLANKS       LGNOCV
     C                   MOVEL     DN(624)       LG1PD
     C                   MOVE      *ZEROS        LGPVPD
     C                   MOVE      EOOHIW        LGOHIW
     C                   MOVE      EOOHPI        LGOHPI
     C*** BEG ADD *** 02/18/05 ****************************************
     C                   MOVE      DN(555)       LGPOSC
     C*** END ADD *** 02/18/05 ****************************************
     C*
     C*   - Retrieve the header level EOMB codes and load the group
     C*     and reason codes.
     C*
     C                   Z-ADD     1             X
     C     #HKY          CHAIN     PEE01                              99
B01  C     *IN99         DOWEQ     *OFF
     C*** BEG ADD *** 09/12/06 ****************************************
     C     X             ANDLE     10
     C*** END ADD *** 09/12/06 ****************************************
     C*
     C                   MOVEL(P)  EEEOB         XXKEY
     C*** BEG ADD *** 02/18/05 ****************************************
B02  C     EOAST         IFEQ      'KY'
     C*** END ADD *** 02/18/05 ****************************************
     C     TBKY          CHAIN     PEB01                              99
     C*** BEG ADD *** 02/18/05 ****************************************
X02  C                   ELSE
     C     TBKY          CHAIN     PEB02                              99
E02  C                   ENDIF
     C*** END ADD *** 02/18/05 ****************************************
B02  C     *IN99         IFEQ      *OFF
     C                   MOVEL     EBXREF        GC(X)
     C*** BEG CHG *** 02/18/05 ****************************************
     C********** 3         SUBSTEBDSC2:1  RC,X
     C*** Beg Chg *** 05/01/15 ****************************************
     C*****
B03  C*****EOAST         IFEQ      'KY'
     C*****3             SUBST     EBDSC2:1      RC(X)
X03  C*****              ELSE
     C*****              MOVEL     EBXRF2        RC(X)
E03  C*****              ENDIF
     C                   Eval      RC(X) = EBKEY
     C*** End Chg *** 05/01/15 ****************************************
     C*** END CHG *** 02/18/05 ****************************************
     C                   ADD       1             X
E02  C                   ENDIF
     C*
     C     #HKY          READE     PEE01                                  99
E01  C                   ENDDO
     C*
     C*   - Billing provider info
     C*
     C                   MOVEL     DN(528)       LGBPLN
     C                   MOVEL     DN(529)       LGBPFN
     C                   MOVE      *BLANKS       LGBPMI
     C                   MOVEL     DN(531)       LGBPSX
     C                   MOVEL     DN(629)       LGBPFE
     C                   MOVE      EOMOD         LGBPMD
     C                   MOVEL     DN(537)       LGBPSC
     C                   MOVEL     DN(538)       LGBPA1
     C                   MOVEL     DN(539)       LGBPA2
     C                   MOVEL     DN(540)       LGBPCT
     C                   MOVEL     DN(541)       LGBPST
     C                   MOVEL     DN(542)       LGBPZP
     C*** BEG CHG *** 02/18/05 ****************************************
     C******************** MOVE *BLANKS   LGBPCR
B01  C     P1MCTR        IFEQ      *BLANKS
     C                   MOVEL     'USA'         LGBPCR
X01  C                   ELSE
     C                   MOVEL     P1MCTR        LGBPCR
E01  C                   ENDIF
     C*** END CHG *** 02/18/05 ****************************************
     C                   MOVE      *BLANKS       LGPA#
     C                   MOVE      *BLANKS       LGNAT#
     C                   MOVE      *BLANKS       LGSLIC
     C                   MOVE      *BLANKS       LGPTP#
     C*** BEG ADD *** 11/13/05 ****************************************
     C                   MOVEL     DN(523)       LGBPID
     C*** END ADD *** 11/13/05 ****************************************
     C*
     C*   - Rendering provider info
     C*
     C                   MOVEL     DN(638)       LGRPLN
     C                   MOVEL     DN(639)       LGRPFN
     C                   MOVE      *BLANKS       LGRPMI
     C                   MOVEL     DN(641)       LGRPSX
     C                   MOVEL     DN(642)       LGRPFE
     C                   MOVE      *BLANKS       LGRPMD
     C                   MOVEL     DN(651)       LGRPSC
     C                   MOVEL     DN(652)       LGRPA1
     C                   MOVEL     DN(653)       LGRPA2
     C                   MOVEL     DN(654)       LGRPCT
     C                   MOVEL     DN(655)       LGRPST
     C                   MOVEL     DN(656)       LGRPZP
     C                   MOVE      *BLANKS       LGRPCR
     C*** BEG ADD *** 02/18/05 ****************************************
     C                   MOVEL     DN(506)       LGPSIG
     C*
     C*   - Referring provider info
     C*
     C                   MOVEL     DN(690)       LGFPLN
     C                   MOVEL     DN(691)       LGFPFN
     C                   MOVEL     DN(692)       LGFPMI
     C                   MOVEL     DN(693)       LGFPSX
     C                   MOVEL     DN(694)       LGFPFE
     C                   MOVEL     DN(695)       LGFLIC
     C*** END ADD *** 02/18/05 ****************************************
     C*** BEG ADD *** 11/13/05 ****************************************
     C*
     C*   - Facility info
     C*
     C                   MOVEL     DN(678)       LGFNAM
     C*** END ADD *** 11/13/05 ****************************************
     C*
     C*   - Gate keeper info
     C*
     C                   MOVE      'GP'          LGGTKI
     C                   MOVE      *BLANKS       LGGTK#
     C*
     C                   MOVE      'EX'          LGSTS
     C                   MOVEL     Q1USER        LGUSER
     C                   MOVEL     Q1PGM         LGPGM
     C                   MOVEL     XXEXDT        LGEXDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        LGTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDT
     C                   UPDATE    PLG01
     C*
     C                   ENDSR
     C*================================================================
     C* LD837 - This subroutine loads the 837 extract files.
     C*           The header files are loaded first, then the
     C*           detail files are loaded.
     C*================================================================
     C     LD837         BEGSR
     C****************
     C*  Load header files.
     C****************
     C*
     C*  If this is a dental bill, load the Dental extract files.
     C*
B01  C     EOBTYP        IFEQ      'D'
     C                   CLEAR                   PED02
     C     #HKY          CHAIN     PED01                              99
     C                   Z-ADD     XXCO#         EDCO#
     C                   Z-ADD     LGRCN         EDRCN
     C                   Z-ADD     LGLSEQ        EDLSEQ
     C                   MOVE      'A'           EDSTAT
     C                   MOVEL     Q1USER        EDUSER
     C                   MOVEL     Q1PGM         EDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EDTIME
     C                   Z-ADD     XXDT7         EDDT
     C                   MOVEL     Q1USER        EDIUSR
     C                   MOVEL     Q1PGM         EDIPGM
     C                   Z-ADD     DSTIME        EDITIM
     C                   Z-ADD     XXDT7         EDIDT
     C*** BEG ADD *** 02/18/05 ****************************************
     C*** BEG ADD *** 04/16/15 *********************************************
     C                   If        EDICDIND <> 'Y'
     C*** END ADD *** 04/16/15 *********************************************
     C*** Beg Add *** 07/28/15 *********************************************
     C                   MOVEL(P)  'D'           XXLOST
     C*** End Add *** 07/28/15 *********************************************
     C                   MOVEL(P)  EDDI1         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EDDIA1
     C                   MOVEL(P)  EDDI2         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EDDIA2
     C                   MOVEL(P)  EDDI3         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EDDIA3
     C                   MOVEL(P)  EDDI4         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EDDIA4
     C*** BEG ADD *** 04/16/15 *********************************************
     C                   EndIf
     C*** END ADD *** 04/16/15 *********************************************
     C*** BEG ADD *** 04/16/15 *********************************************
      /Free
       If EDICDIND = 'Y';

         kkInd = 'D';

         kkICD10 = EDDI1001;
         Chain (kkICD10:kkInd) FMICD10L1;
         If %Found(FMICD10L1);
           EDICD1001 = ICFCODE;
         EndIf;

         kkICD10 = EDDI1002;
         Chain (kkICD10:kkInd) FMICD10L1;
         If %Found(FMICD10L1);
           EDICD1002 = ICFCODE;
         EndIf;

         kkICD10 = EDDI1003;
         Chain (kkICD10:kkInd) FMICD10L1;
         If %Found(FMICD10L1);
           EDICD1003 = ICFCODE;
         EndIf;

         kkICD10 = EDDI1004;
         Chain (kkICD10:kkInd) FMICD10L1;
         If %Found(FMICD10L1);
           EDICD1004 = ICFCODE;
         EndIf;

       EndIf;
      /End-Free
     C*** END ADD *** 04/16/15 *********************************************
     C*** END ADD *** 02/18/05 ****************************************
     C*** BEG ADD *** 12/13/08 ****************************************
     C*
     C* Billing provider license number
     C*
     C                   MOVEL     DN(630)       EDFLIC
     C*
     C* Rendering provider license number
     C*
     C                   MOVEL     DN(643)       EDRNLC
     C*
     C*** END ADD *** 12/13/08 ****************************************
     C*** BEG ADD *** 08/11/11 ****************************************
     C                   MOVEL(P)  DN(647)       EDTNPI
     C*** END ADD *** 08/11/11 ****************************************
     C*
     C                   WRITE     PED02
E01  C                   ENDIF
     C*
     C*  If this is a provider bill, load the HCFA extract files.
     C*
B01  C     EOBTYP        IFEQ      'N'
     C                   CLEAR                   PEH02
     C     #HKY          CHAIN     PEH01                              99
     C                   Z-ADD     XXCO#         EHCO#
     C                   Z-ADD     LGRCN         EHRCN
     C                   Z-ADD     LGLSEQ        EHLSEQ
     C*** BEG ADD *** 04/16/15 *********************************************
     C                   If        EHICDIND <> 'Y'
     C*** END ADD *** 04/16/15 *********************************************
     C*** Beg Add *** 07/28/15 *********************************************
     C                   MOVEL(P)  'D'           XXLOST
     C*** End Add *** 07/28/15 *********************************************
     C                   MOVEL(P)  EHDI1         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EHDIA1
     C                   MOVEL(P)  EHDI2         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EHDIA2
     C                   MOVEL(P)  EHDI3         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EHDIA3
     C                   MOVEL(P)  EHDI4         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EHDIA4
     C*** BEG ADD *** 04/16/15 *********************************************
     C                   EndIf
     C*** END ADD *** 04/16/15 *********************************************
     C*** BEG ADD *** 04/16/15 *********************************************
      /Free
       If EHICDIND = 'Y';

         kkInd = 'D';

         kkICD10 = EHDI1001;
         Chain (kkICD10:kkInd) FMICD10L1;
         If %Found(FMICD10L1);
           EHICD1001 = ICFCODE;
         EndIf;

         kkICD10 = EHDI1002;
         Chain (kkICD10:kkInd) FMICD10L1;
         If %Found(FMICD10L1);
           EHICD1002 = ICFCODE;
         EndIf;

         kkICD10 = EHDI1003;
         Chain (kkICD10:kkInd) FMICD10L1;
         If %Found(FMICD10L1);
           EHICD1003 = ICFCODE;
         EndIf;

         kkICD10 = EHDI1004;
         Chain (kkICD10:kkInd) FMICD10L1;
         If %Found(FMICD10L1);
           EHICD1004 = ICFCODE;
         EndIf;

       EndIf;
      /End-Free
     C*** END ADD *** 04/16/15 *********************************************
     C                   MOVE      'A'           EHSTAT
     C                   MOVEL     Q1USER        EHUSER
     C                   MOVEL     Q1PGM         EHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EHTIME
     C                   Z-ADD     XXDT7         EHDT
     C                   MOVEL     Q1USER        EHIUSR
     C                   MOVEL     Q1PGM         EHIPGM
     C                   Z-ADD     DSTIME        EHITIM
     C                   Z-ADD     XXDT7         EHIDT
     C*** BEG ADD *** 12/13/08 ****************************************
     C*
     C* Billing provider license number
     C*
     C                   MOVEL     DN(630)       EHFLIC
     C*
     C* Rendering provider license number
     C*
     C                   MOVEL     DN(643)       EHRNLC
     C*
     C*** END ADD *** 12/13/08 ****************************************
     C*** Beg Add *** 04/03/13 ****************************************
     C                   MOVEL     DN(699)       EHRNPI
     C*** End Add *** 04/03/13 ****************************************
     C*
     C                   WRITE     PEH02
E01  C                   ENDIF
     C*
     C*  If this is a Rx bill, load the Rx extract files.
     C*
B01  C     EOBTYP        IFEQ      'X'
     C                   CLEAR                   PEP02
     C     #HKY          CHAIN     PEP01                              99
     C                   Z-ADD     XXCO#         EPCO#
     C                   Z-ADD     LGRCN         EPRCN
     C                   Z-ADD     LGLSEQ        EPLSEQ
     C                   MOVE      'A'           EPSTAT
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EPTIME
     C                   Z-ADD     XXDT7         EPDT
     C                   MOVEL     Q1USER        EPIUSR
     C                   MOVEL     Q1PGM         EPIPGM
     C                   Z-ADD     DSTIME        EPITIM
     C                   Z-ADD     XXDT7         EPIDT
     C*** BEG ADD *** 12/13/08 ****************************************
     C*
     C* Referring provider license number
     C*
     C                   MOVEL     DN(695)       EPPLIC
     C*
     C*** END ADD *** 12/13/08 ****************************************
     C*
     C*** BEG ADD *** 05/18/09 ****************************************
     C*
     C* Referring provider NPI#
     C*
     C                   MOVEL     DN(699)       EPRFNP
     C*
     C*** END ADD *** 05/18/09 ****************************************
     C*** BEG ADD *** 08/11/11 ****************************************
     C                   MOVEL(P)  DN(647)       EPPHAR
     C*** END ADD *** 08/11/11 ****************************************
     C                   WRITE     PEP02
E01  C                   ENDIF
     C*
     C*  If this is a hospital bill, load the UB extract files.
     C*
B01  C     EOBTYP        IFEQ      'H'
     C                   CLEAR                   PEU02
     C     #HKY          CHAIN     PEU01                              99
     C                   Z-ADD     XXCO#         EUCO#
     C                   Z-ADD     LGRCN         EURCN
     C                   Z-ADD     LGLSEQ        EULSEQ
     C*
     C*** BEG ADD *** 04/16/15 *********************************************
     C                   If        EUICDIND <> 'Y'
     C                   MOVEL(P)  'D'           XXLOST
     C*** END ADD *** 04/16/15 *********************************************
     C                   MOVEL(P)  EUDI1         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EUDIA1
     C*** BEG ADD *** 07/08/06 *************************************************
     C  N99              MOVEL     DIDS1         EUADIA
     C*** END ADD *** 07/08/06 *************************************************
     C                   MOVEL(P)  EUDI2         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EUDIA2
     C                   MOVEL(P)  EUDI3         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EUDIA3
     C                   MOVEL(P)  EUDI4         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EUDIA4
     C                   MOVEL(P)  EUDI5         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EUDIA5
     C                   MOVEL(P)  EUDI6         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EUDIA6
     C                   MOVEL(P)  EUDI7         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EUDIA7
     C                   MOVEL(P)  EUDI8         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EUDIA8
     C                   MOVEL(P)  EUDI9         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EUDIA9
     C*** BEG ADD *** 04/16/15 *********************************************
     C                   EndIf
     C*** END ADD *** 04/16/15 *********************************************
     C*** BEG ADD *** 04/16/15 *********************************************
      /Free
       If EUICDIND = 'Y';

          kkInd = 'D';

          // Diag Codes

          kkICD10 = EUDI1001;
          Chain (kkICD10:kkInd) FMICD10L1;
          If %Found(FMICD10L1);
            EUICD1001 = ICFCODE;
          EndIf;

          kkICD10 = EUDI1002;
          Chain (kkICD10:kkInd) FMICD10L1;
          If %Found(FMICD10L1);
            EUICD1002 = ICFCODE;
          EndIf;

          kkICD10 = EUDI1003;
          Chain (kkICD10:kkInd) FMICD10L1;
          If %Found(FMICD10L1);
            EUICD1003 = ICFCODE;
          EndIf;

          kkICD10 = EUDI1004;
          Chain (kkICD10:kkInd) FMICD10L1;
          If %Found(FMICD10L1);
            EUICD1004 = ICFCODE;
          EndIf;

          kkICD10 = EUDI1005;
          Chain (kkICD10:kkInd) FMICD10L1;
          If %Found(FMICD10L1);
            EUICD1005 = ICFCODE;
          EndIf;

          kkICD10 = EUDI1006;
          Chain (kkICD10:kkInd) FMICD10L1;
          If %Found(FMICD10L1);
            EUICD1006 = ICFCODE;
          EndIf;

          kkICD10 = EUDI1007;
          Chain (kkICD10:kkInd) FMICD10L1;
          If %Found(FMICD10L1);
            EUICD1007 = ICFCODE;
          EndIf;

          kkICD10 = EUDI1008;
          Chain (kkICD10:kkInd) FMICD10L1;
          If %Found(FMICD10L1);
            EUICD1008 = ICFCODE;
          EndIf;

          kkICD10 = EUDI1009;
          Chain (kkICD10:kkInd) FMICD10L1;
          If %Found(FMICD10L1);
            EUICD1009 = ICFCODE;
          EndIf;

          //*** Beg Add *** 10/21/15 **********************************
          kkICD10 = EUDI1010;
          Chain (kkICD10:kkInd) FMICD10L1;
          If %Found(FMICD10L1);
            EUICD1010 = ICFCODE;
          EndIf;
          //*** End Add *** 10/21/15 **********************************

          // Admitting Diag Codes

          kkICD10 = EUAD1001;
          Chain (kkICD10:kkInd) FMICD10L1;
          If %Found(FMICD10L1);
            EUADIA1001 = ICFCODE;
          EndIf;

       EndIf;
      /End-Free
     C*** END ADD *** 04/16/15 *********************************************
     C***** Beg Add *** 05/29/14 **************************************
     C*** load procedure codes

     C*** remove trailing 'D' or 'O'
     C                   eval      EUPI1 = %xlate('DO':'  ':EUPI1:7)
     C                   eval      EUPI2 = %xlate('DO':'  ':EUPI2:7)
     C                   eval      EUPI3 = %xlate('DO':'  ':EUPI3:7)
     C                   eval      EUPI4 = %xlate('DO':'  ':EUPI4:7)
     C                   eval      EUPI5 = %xlate('DO':'  ':EUPI5:7)
     C                   eval      EUPI6 = %xlate('DO':'  ':EUPI6:7)
     C*** BEG ADD *** 04/16/15 *********************************************
     C                   If        EUICDIND <> 'Y' and EUPCM = '9'
     C                   MOVEL(P)  'O'           XXLOST
     C*** END ADD *** 04/16/15 *********************************************
     C                   MOVEL(P)  EUPI1         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EUPRC1
     C                   MOVEL(P)  EUPI2         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EUPRC2
     C                   MOVEL(P)  EUPI3         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EUPRC3
     C                   MOVEL(P)  EUPI4         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EUPRC4
     C                   MOVEL(P)  EUPI5         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EUPRC5
     C                   MOVEL(P)  EUPI6         XXICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDS1         EUPRC6
     C*** BEG ADD *** 04/16/15 *********************************************
     C                   EndIf
     C*** END ADD *** 04/16/15 *********************************************
     C*** BEG ADD *** 04/16/15 *********************************************
      /Free
       If EUICDIND = 'Y';

          // Procedure Codes

          kkInd = 'O';

          kkICD10 = EUPR1001;
          Chain (kkICD10:kkInd) FMICD10L1;
          If %Found(FMICD10L1);
            EUPRC1001 = ICFCODE;
          EndIf;

          kkICD10 = EUPR1002;
          Chain (kkICD10:kkInd) FMICD10L1;
          If %Found(FMICD10L1);
            EUPRC1002 = ICFCODE;
          EndIf;

          kkICD10 = EUPR1003;
          Chain (kkICD10:kkInd) FMICD10L1;
          If %Found(FMICD10L1);
            EUPRC1003 = ICFCODE;
          EndIf;

          kkICD10 = EUPR1004;
          Chain (kkICD10:kkInd) FMICD10L1;
          If %Found(FMICD10L1);
            EUPRC1004 = ICFCODE;
          EndIf;

          kkICD10 = EUPR1005;
          Chain (kkICD10:kkInd) FMICD10L1;
          If %Found(FMICD10L1);
            EUPRC1005 = ICFCODE;
          EndIf;

          kkICD10 = EUPR1006;
          Chain (kkICD10:kkInd) FMICD10L1;
          If %Found(FMICD10L1);
            EUPRC1006 = ICFCODE;
          EndIf;

       EndIf;
      /End-Free
     C*** END ADD *** 04/16/15 *********************************************
     C***** End Add *** 05/29/14 **************************************
     C                   MOVE      'A'           EUSTAT
     C                   MOVEL     Q1USER        EUUSER
     C                   MOVEL     Q1PGM         EUPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EUTIME
     C                   Z-ADD     XXDT7         EUDT
     C                   MOVEL     Q1USER        EUIUSR
     C                   MOVEL     Q1PGM         EUIPGM
     C                   Z-ADD     DSTIME        EUITIM
     C                   Z-ADD     XXDT7         EUIDT
     C*** BEG ADD *** 01/18/07 *************************************************
     C                   MOVEL     EUCTCD        EUCTC2
     C*** END ADD *** 01/18/07 *************************************************
     C*** BEG ADD *** 12/13/08 ****************************************
     C*
     C* Referring provider license number
     C*
     C                   MOVEL     DN(695)       EURLIC
     C*
     C*** END ADD *** 12/13/08 ****************************************
     C*** Beg Add *** 04/03/13 ****************************************
     C                   MOVEL     DN(699)       EUANPI
     C*** End Add *** 04/03/13 ****************************************
     C*
     C                   WRITE     PEU02
E01  C                   ENDIF
     C****************
     C*  Load detail files.
     C****************
     C*
     C* Initial total charges for this recon
     C*
     C*** BEG ADD *** 08/11/11 ****************************************
     C                   Z-ADD     *ZERO         TOTCHG
     C*** END ADD *** 08/11/11 ****************************************
     C*
     C*  Process all of the detail charges.
     C*
     C     #HKY          CHAIN     PCA01                              99
B01  C     *IN99         DOWEQ     *OFF
     C*** BEG ADD *** 10/10/11 ****************************************
     C                   Z-ADD     CACHRG        TOTCHG
     C*** END ADD *** 10/10/11 ****************************************
     C*** BEG ADD *** 08/11/11 ****************************************
     C*
     C* Determine if there are amounts in FDDAP for this recon
     C* If so then continue else dont write this record
     C*
     C     #DAKY         CHAIN     PDA01                              99
     C*
B02  C     *IN99         IFEQ      *OFF
     C*
     C*** BEG DEL *** 10/10/11 ****************************************
     C*********            ADD  CACHRG    TOTCHG
     C*** END DEL *** 10/10/11 ****************************************
     C*** END ADD *** 08/11/11 ****************************************
     C*** BEG ADD *** 10/10/11 ****************************************
     C                   Z-ADD     DAPAID        XXPAID
X02  C                   ELSE
     C                   Z-ADD     0             XXPAID
E02  C                   ENDIF                                                  IN99=OFF PDA01
     C*** END ADD *** 10/10/11 ****************************************
     C*
     C*  Load the EOMB adjustment codes to a hold array, if any.
     C*
     C                   CLEAR                   EOB
     C                   Z-ADD     1             X
     C*** BEG CHG *** 08/11/11 ****************************************
     C********** #DKY      CHAINPDE01                99
     C     #DEKY         CHAIN     PDE01                              99
     C*** END CHG *** 08/11/11 ****************************************
B02  C     *IN99         DOWEQ     *OFF
     C     X             ANDLE     9
     C*
     C********** DERCN     IFEQ LGRCN
     C*** BEG ADD *** 07/08/06 ****************************************
B03  C     DESTAT        IFEQ      'A'
     C*** END ADD *** 07/08/06 ****************************************
     C*
     C                   MOVEL(P)  DEEOB         XXKEY
     C*** BEG ADD *** 02/18/05 ****************************************
B04  C     EOAST         IFEQ      'KY'
     C*** END ADD *** 02/18/05 ****************************************
     C     TBKY          CHAIN     PEB01                              99
     C*** BEG ADD *** 02/18/05 ****************************************
X04  C                   ELSE
     C     TBKY          CHAIN     PEB02                              99
E04  C                   ENDIF
     C*** END ADD *** 02/18/05 ****************************************
B04  C     *IN99         IFEQ      *OFF
B05  C     X             IFEQ      1
     C                   MOVEL     EBXREF        EOB(10)
E05  C                   ENDIF
     C*
     C* KY USES 1ST 3 POS OF THE 2ND DESC FIELD TO HOLD REASON CODE
     C* ALL OTHER STATES WILL USE EBXRF2
     C*
     C*** BEG CHG *** 02/18/05 ****************************************
     C********** 3         SUBSTEBDSC2:1  EOB,X
     C*** Beg Chg *** 05/01/15 ****************************************
     C****
B05  C**** EOAST         IFEQ      'KY'
     C**** 3             SUBST     EBDSC2:1      EOB(X)
X05  C****               ELSE
     C****               MOVEL     EBXRF2        EOB(X)
E05  C****               ENDIF
     C                   Eval      EOB(X) = EBKEY
     C*** End Chg *** 05/01/15 ****************************************
     C*** END CHG *** 02/18/05 ****************************************
     C*** BEG ADD *** 01/25/07 ****************************************
B05  C     EOAST         IFEQ      'TX'
     C     EBSTAT        ANDEQ     'I'
     C*** Beg Chg *** 05/01/15 ****************************************
     C*****************  MOVEL     'W1'          EOB(X)
     C                   MOVEL     'P12'         EOB(X)
     C*** End Chg *** 05/01/15 ****************************************
B06  C     X             IFEQ      1
     C                   MOVEL     'OA'          EOB(10)
E06  C                   ENDIF
E05  C                   ENDIF
     C*** END ADD *** 01/25/07 ****************************************
     C                   ADD       1             X
E04  C                   ENDIF
     C*
     C******************** ENDIF
     C*** BEG ADD *** 07/08/06 ****************************************
E03  C                   ENDIF                                                  DESTAT IFEQ A
     C*** END ADD *** 07/08/06 ****************************************
     C*
     C*** BEG CHG *** 08/11/11 ****************************************
     C*********  #DKY      READEPDE01                    99
     C     #DEKY         READE     PDE01                                  99
     C*** END CHG *** 08/11/11 ****************************************
E02  C                   ENDDO
     C*
     C*  If this is a dental bill, load the Dental extract files.
     C*
B02  C     EOBTYP        IFEQ      'D'
     C                   CLEAR                   PDD02
     C     #DKY          CHAIN     PDD01                              91
     C                   Z-ADD     XXCO#         DDCO#
     C                   Z-ADD     LGRCN         DDRCN
     C                   Z-ADD     LGLSEQ        DDLSEQ
     C                   Z-ADD     CALIN#        DDLINE
     C                   Z-ADD     CAFDAT        DDFRDT
     C                   Z-ADD     CATDAT        DDTODT
     C                   Z-ADD     CACHRG        DDCHRG
     C*** BEG DEL *** 08/11/11 ****************************************
     C****************
     C****************     Z-ADDCAMRA     DDMRA
     C****************     Z-ADDCAOTHR    DDOTHR
     C****************     Z-ADDCAOTH2    DDOTH2
     C****************     Z-ADDCANDIS    DDNTWK
     C****************     Z-ADDCAPAID    DDPAID
     C*** END DEL *** 08/11/11 ****************************************
     C*** BEG ADD *** 08/11/11 ****************************************
     C                   Z-ADD     DAMRA         DDMRA
     C                   Z-ADD     DAOTHR        DDOTHR
     C                   Z-ADD     DAOTH2        DDOTH2
     C                   Z-ADD     DANDIS        DDNTWK
     C*** BEG CHG *** 10/10/11 ****************************************
     C******               Z-ADDDAPAID    DDPAID
     C                   Z-ADD     XXPAID        DDPAID
     C*** END CHG *** 10/10/11 ****************************************
     C*** END ADD *** 08/11/11 ****************************************
     C*** BEG ADD *** 02/18/05 ****************************************
     C                   Z-ADD     CAUNTS        DDUNTS
     C*** END ADD *** 02/18/05 ****************************************
     C                   MOVE      XXEOB1        DDEOB1
     C                   MOVE      XXEOB2        DDEOB2
     C                   MOVE      XXEOB3        DDEOB3
     C                   MOVE      XXEOB4        DDEOB4
     C                   MOVE      XXEOB5        DDEOB5
     C                   MOVE      XXEOB6        DDEOB6
     C                   MOVE      XXEOB7        DDEOB7
     C                   MOVE      XXEOB8        DDEOB8
     C                   MOVE      XXEOB9        DDEOB9
     C                   MOVE      XXEO10        DDEO10
     C                   MOVE      'A'           DDSTAT
     C                   MOVEL     Q1USER        DDUSER
     C                   MOVEL     Q1PGM         DDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DDTIME
     C                   Z-ADD     XXDT7         DDDT
     C                   MOVEL     Q1USER        DDIUSR
     C                   MOVEL     Q1PGM         DDIPGM
     C                   Z-ADD     DSTIME        DDITIM
     C                   Z-ADD     XXDT7         DDIDT
     C*
     C                   WRITE     PDD02
E02  C                   ENDIF
     C*
     C*  If this is a provider bill, load the HCFA extract files.
     C*
B02  C     EOBTYP        IFEQ      'N'
     C                   CLEAR                   PDH02
     C     #DKY          CHAIN     PDH01                              91
     C                   Z-ADD     XXCO#         DHCO#
     C                   Z-ADD     LGRCN         DHRCN
     C                   Z-ADD     LGLSEQ        DHLSEQ
     C                   Z-ADD     CALIN#        DHLINE
     C                   Z-ADD     CAFDAT        DHFRDT
     C                   Z-ADD     CATDAT        DHTODT
     C                   Z-ADD     CACHRG        DHCHRG
     C*** BEG DEL *** 08/11/11 ****************************************
     C****************
     C****************     Z-ADDCAMRA     DHMRA
     C****************     Z-ADDCAOTHR    DHOTHR
     C****************     Z-ADDCAOTH2    DHOTH2
     C****************     Z-ADDCANDIS    DHNTWK
     C****************     Z-ADDCAPAID    DHPAID
     C*** END DEL *** 08/11/11 ****************************************
     C*** BEG ADD *** 08/11/11 ****************************************
     C                   Z-ADD     DAMRA         DHMRA
     C                   Z-ADD     DAOTHR        DHOTHR
     C                   Z-ADD     DAOTH2        DHOTH2
     C                   Z-ADD     DANDIS        DHNTWK
     C*** BEG CHG *** 10/10/11 ****************************************
     C******               Z-ADDDAPAID    DHPAID
     C                   Z-ADD     XXPAID        DHPAID
     C*** END CHG *** 10/10/11 ****************************************
     C*** END ADD *** 08/11/11 ****************************************
     C*** BEG ADD *** 02/18/05 ****************************************
     C**********           Z-ADDCAUNTS    DHUNTS
     C*** END ADD *** 02/18/05 ****************************************
     C                   MOVE      XXEOB1        DHEOB1
     C                   MOVE      XXEOB2        DHEOB2
     C                   MOVE      XXEOB3        DHEOB3
     C                   MOVE      XXEOB4        DHEOB4
     C                   MOVE      XXEOB5        DHEOB5
     C                   MOVE      XXEOB6        DHEOB6
     C                   MOVE      XXEOB7        DHEOB7
     C                   MOVE      XXEOB8        DHEOB8
     C                   MOVE      XXEOB9        DHEOB9
     C                   MOVE      XXEO10        DHEO10
     C                   MOVE      'A'           DHSTAT
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1PGM         DHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     XXDT7         DHDT
     C                   MOVEL     Q1USER        DHIUSR
     C                   MOVEL     Q1PGM         DHIPGM
     C                   Z-ADD     DSTIME        DHITIM
     C                   Z-ADD     XXDT7         DHIDT
     C*
     C                   WRITE     PDH02
E02  C                   ENDIF
     C*
     C*  If this is a Rx bill, load the Rx extract files.
     C*
B02  C     EOBTYP        IFEQ      'X'
     C                   CLEAR                   PDP02
     C     #DKY          CHAIN     PDP01                              91
     C                   Z-ADD     XXCO#         DPCO#
     C                   Z-ADD     LGRCN         DPRCN
     C                   Z-ADD     LGLSEQ        DPLSEQ
     C                   Z-ADD     CALIN#        DPLINE
     C                   Z-ADD     CAFDAT        DPFRDT
     C                   Z-ADD     CATDAT        DPTODT
     C                   Z-ADD     CACHRG        DPCHRG
     C*** BEG DEL *** 08/11/11 ****************************************
     C******************
     C******************   Z-ADDCAMRA     DPMRA
     C******************   Z-ADDCAOTHR    DPOTHR
     C******************   Z-ADDCAOTH2    DPOTH2
     C******************   Z-ADDCANDIS    DPNTWK
     C******************   Z-ADDCAPAID    DPPAID
     C*** END DEL *** 08/11/11 ****************************************
     C*** BEG ADD *** 08/11/11 ****************************************
     C                   Z-ADD     DAMRA         DPMRA
     C                   Z-ADD     DAOTHR        DPOTHR
     C                   Z-ADD     DAOTH2        DPOTH2
     C                   Z-ADD     DANDIS        DPNTWK
     C*** BEG CHG *** 10/10/11 ****************************************
     C******               Z-ADDDAPAID    DPPAID
     C                   Z-ADD     XXPAID        DPPAID
     C*** END CHG *** 10/10/11 ****************************************
     C*** END ADD *** 08/11/11 ****************************************
     C                   MOVE      XXEOB1        DPEOB1
     C                   MOVE      XXEOB2        DPEOB2
     C                   MOVE      XXEOB3        DPEOB3
     C                   MOVE      XXEOB4        DPEOB4
     C                   MOVE      XXEOB5        DPEOB5
     C                   MOVE      XXEOB6        DPEOB6
     C                   MOVE      XXEOB7        DPEOB7
     C                   MOVE      XXEOB8        DPEOB8
     C                   MOVE      XXEOB9        DPEOB9
     C                   MOVE      XXEO10        DPEO10
     C                   MOVE      'A'           DPSTAT
     C                   MOVEL     Q1USER        DPUSER
     C                   MOVEL     Q1PGM         DPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DPTIME
     C                   Z-ADD     XXDT7         DPDT
     C                   MOVEL     Q1USER        DPIUSR
     C                   MOVEL     Q1PGM         DPIPGM
     C                   Z-ADD     DSTIME        DPITIM
     C                   Z-ADD     XXDT7         DPIDT
     C*
     C                   WRITE     PDP02
E02  C                   ENDIF
     C*
     C*  If this is a hospital bill, load the UB extract files.
     C*
B02  C     EOBTYP        IFEQ      'H'
     C                   CLEAR                   PDU02
     C     #DKY          CHAIN     PDU01                              91
     C                   Z-ADD     XXCO#         DUCO#
     C                   Z-ADD     LGRCN         DURCN
     C                   Z-ADD     LGLSEQ        DULSEQ
     C                   Z-ADD     CALIN#        DULINE
     C                   Z-ADD     CAFDAT        DUFRDT
     C                   Z-ADD     CATDAT        DUTODT
     C                   Z-ADD     CACHRG        DUCHRG
     C*** BEG DEL *** 08/11/11 ****************************************
     C********************
     C******************** Z-ADDCAMRA     DUMRA
     C******************** Z-ADDCAOTHR    DUOTHR
     C******************** Z-ADDCAOTH2    DUOTH2
     C******************** Z-ADDCANDIS    DUNTWK
     C******************** Z-ADDCAPAID    DUPAID
     C*** END DEL *** 08/11/11 ****************************************
     C*** BEG DEL *** 10/10/11 ****************************************
     C*** BEG ADD *** 08/11/11 ****************************************
     C*******              Z-ADDCAMRA     DUMRA
     C*******              Z-ADDCAOTHR    DUOTHR
     C*******              Z-ADDCAOTH2    DUOTH2
     C*******              Z-ADDCANDIS    DUNTWK
     C*******              Z-ADDCAPAID    DUPAID
     C*** END ADD *** 08/11/11 ****************************************
     C*** END DEL *** 10/10/11 ****************************************
     C*** BEG ADD *** 10/10/11 ****************************************
     C                   Z-ADD     DAMRA         DUMRA
     C                   Z-ADD     DAOTHR        DUOTHR
     C                   Z-ADD     DAOTH2        DUOTH2
     C                   Z-ADD     DANDIS        DUNTWK
     C                   Z-ADD     XXPAID        DUPAID
     C*** END ADD *** 10/10/11 ****************************************
     C*** BEGIN ADD *** 01/24/01 **************************************
     C*** BEGIN CHG *** 04/07/09 **************************************
     C*******************  MOVE DURVCD    DUBRVC
     C                   MOVE      DUBPRC        DUBRVC
     C*** END   CHG *** 04/07/09 **************************************
     C                   Z-ADD     CAUNTS        DUUNTS
     C*** END ADD ***** 01/24/01 **************************************
     C                   MOVE      XXEOB1        DUEOB1
     C                   MOVE      XXEOB2        DUEOB2
     C                   MOVE      XXEOB3        DUEOB3
     C                   MOVE      XXEOB4        DUEOB4
     C                   MOVE      XXEOB5        DUEOB5
     C                   MOVE      XXEOB6        DUEOB6
     C                   MOVE      XXEOB7        DUEOB7
     C                   MOVE      XXEOB8        DUEOB8
     C                   MOVE      XXEOB9        DUEOB9
     C                   MOVE      XXEO10        DUEO10
     C                   MOVE      'A'           DUSTAT
     C                   MOVEL     Q1USER        DUUSER
     C                   MOVEL     Q1PGM         DUPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DUTIME
     C                   Z-ADD     XXDT7         DUDT
     C                   MOVEL     Q1USER        DUIUSR
     C                   MOVEL     Q1PGM         DUIPGM
     C                   Z-ADD     DSTIME        DUITIM
     C                   Z-ADD     XXDT7         DUIDT
     C*
     C                   WRITE     PDU02
E02  C                   ENDIF
     C*
     C*** BEG DEL *** 10/10/11 ****************************************
     C*** BEG ADD *** 08/11/11 ****************************************
     C*******              ENDIF                           IN99=OFF PDA01
     C*** END ADD *** 08/11/11 ****************************************
     C*** END DEL *** 10/10/11 ****************************************
     C*
     C     #HKY          READE     PCA01                                  99
E01  C                   ENDDO
     C*
     C                   ENDSR
     C*****************************************************************
     C*  CMPDRY - Complete the extract error diary if it exists.
     C*****************************************************************
     C     CMPDRY        BEGSR
     C*
     C*  Call API to build DARTS key
     C*
     C                   MOVE      LGCO#         #1CO#
     C                   MOVE      LGSUB#        #1SUB#
     C                   MOVE      LGFND         #1FND
     C                   MOVE      LGFYR         #1FYR
     C                   MOVE      LGCASE        #1CASE
     C*
     C                   CALL      'FA703R'
     C                   PARM      'K'           ##RTCD            1
     C                   PARM                    ##WCIN            1
     C                   PARM                    ##WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANK        ##KEY            40
     C                   PARM      *BLANK        ##PNAM           10
     C*
     C*  Retrieve the extract diary record and complete it.
     C*
B01  C     LGDS01        IFGT      0
     C                   Z-ADD     LGDS01        XXDSEQ
     C     SDKEY         CHAIN     PCM02                              99
B02  C     *IN99         IFEQ      *OFF
     C     UDATE         MULT      10000.01      CMDCDT
     C                   UPDATE    PCM02
E02  C                   ENDIF
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C*  ERRDRY - Create the extract error diary if it does not exist.
     C*****************************************************************
     C     ERRDRY        BEGSR
     C*
     C*  Determine if the error diary already exists.
     C*
B01  C     LGDS01        IFGT      0
     C*
     C                   Z-ADD     LGDS01        XXDSEQ
     C     SDKEY         CHAIN     PCM02                              99
B02  C     *IN99         IFEQ      *OFF
     C*
B03  C     CMDCDT        IFGT      0
     C                   Z-ADD     0             CMDCDT
     C                   UPDATE    PCM02
E03  C                   ENDIF
     C*
     C                   Z-ADD     LGDS01        SVSEQ#
     C                   GOTO      ENDDRY
E02  C                   ENDIF
E01  C                   ENDIF
     C*
     C*  Call API to build DARTS key
     C*
     C                   MOVE      LGCO#         #1CO#
     C                   MOVE      LGSUB#        #1SUB#
     C                   MOVE      LGFND         #1FND
     C                   MOVE      LGFYR         #1FYR
     C                   MOVE      LGCASE        #1CASE
     C*
     C                   CALL      'FA703R'
     C                   PARM      'K'           ##RTCD            1
     C                   PARM                    ##WCIN            1
     C                   PARM                    ##WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANK        ##KEY            40
     C                   PARM      *BLANK        ##PNAM           10
     C*
     C*  Create the diary if the EDI control field exists.
     C*
B01  C     EXDR01        IFNE      *BLANKS
     C*
B02  C     EX#D01        IFEQ      *ZEROS
     C                   MOVE      DSDATE        ##CDAT
X02  C                   ELSE
     C                   MOVE      DSDATE        SRDATE
     C                   MOVE      EX#D01        SRDAYS
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE            8 0
     C                   PARM                    SRDAYS            5 0
     C                   PARM                    SRERR             1
     C                   MOVE      SRDATE        ##CDAT
E02  C                   ENDIF
     C*
     C*** BEGIN CHG *** 06/30/04 **************************************
     C**********           MOVEL'F0350R'  ##DCID    P
     C                   MOVEL(P)  'F0350C'      ##DCID
     C*** END   CHG *** 06/30/04 **************************************
     C                   MOVE      *ZEROS        ##SECL
     C*** BEGIN CHG *** 06/30/04 **************************************
     C**********           MOVEL##KEY     ##FOLD
     C                   CLEAR                   EOBKEY
     C                   MOVE      LGCO#         CO#
     C                   MOVE      LGSUB#        SUB#
     C                   MOVE      LGFND         FND
     C                   MOVE      LGFYR         FYR
     C                   MOVE      LGCASE        CASE
     C                   MOVE      LGEOB#        EOB#
     C                   MOVEL     EOBKEY        ##FOLD
     C*** END   CHG *** 06/30/04 **************************************
     C                   MOVE      *BLANKS       #1CASE
     C*
     C                   CALL      'DA700R'
     C                   PARM                    ##KEY
     C                   PARM      *BLANKS       ##COMM          256
     C                   PARM                    ##CDAT            8
     C                   PARM                    ##DCID           12
     C                   PARM                    ##PNAM           10
     C                   PARM                    ##SECL            3
     C                   PARM      EOIUSR        ##USER           10
     C                   PARM      Q1PGM         ##PGM            10
     C                   PARM                    ##FOLD           80
     C                   PARM      'Y'           ##DIFL            1
     C                   PARM      Q1PGM         ##IPGM           10
     C                   PARM      'Y'           ##FL01            1
     C                   PARM      'Y'           ##FL02            1
     C                   PARM      EXDR01        ##RMCD            5
     C                   PARM                    ##SEQ#            5
     C*
     C                   MOVE      ##SEQ#        SVSEQ#
     C*
E01  C                   ENDIF
     C*
     C     ENDDRY        ENDSR
     C*****************************************************************
     C*  ERRFIL - This subroutine loads the extract error reason file.
     C*****************************************************************
     C     ERRFIL        BEGSR
     C*
     C                   Z-ADD     XXCO#         E1CO#
     C                   Z-ADD     LGSUB#        E1SUB#
     C                   Z-ADD     LGFND         E1FND
     C                   Z-ADD     LGFYR         E1FYR
     C                   Z-ADD     LGCASE        E1CASE
     C                   Z-ADD     LGEOB#        E1EOB#
     C                   Z-ADD     LGRCN         E1RCN
     C                   MOVEL     LGSTS         E1STS
     C                   MOVEL     LGTSET        E1TSET
     C                   MOVEL     LGJST         E1JST
     C                   MOVEL     LGMTC         E1MTC
     C                   Z-ADD     LGLSEQ        E1LSEQ
     C                   MOVE      'E'           E1ETYP
     C                   MOVE      DNDN#         E1DN#
     C                   MOVEL     DNDESC        E1DND
     C                   MOVE      XXERRS        E1MSG#
     C                   MOVEL     ERDESC        E1MSGD
     C                   MOVE      'A'           E1STAT
     C*
     C                   MOVEL     Q1USER        E1USER
     C                   MOVEL     Q1PGM         E1PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        E1TIME
     C                   Z-ADD     XXDT7         E1DT
     C                   MOVEL     Q1USER        E1IUSR
     C                   MOVEL     Q1PGM         E1IPGM
     C                   Z-ADD     DSTIME        E1ITIM
     C                   Z-ADD     XXDT7         E1IDT
     C*
     C                   WRITE     PE101
     C*
     C                   ENDSR
     C*****************************************************************
     C*  THIS SUBROUTINE LOADS ADDITIONAL INFORMATION FOR THE DETAIL
     C*  REPORT AND PRINTS THE DETAIL LINES.
     C*****************************************************************
     C     RPTDET        BEGSR
     C*
     C*  LOAD THE DETAIL INFORMATION
     C*
     C                   MOVEL     DN(43)        XXCNAM           30
     C     XXCNAM        CAT       ',':0         XXCNAM
     C     XXCNAM        CAT       DN(44):1      XXCNAM
     C                   MOVEL     DN(31)        XXACDT
     C*
     C*  PRINT THE DETAIL LINES
     C*
B01  C     XTEUSR        IFNE      XXEUSR
     C                   EXCEPT    HEAD
     C                   MOVEL     XTEUSR        XXEUSR
     C                   MOVE      '0'           *INOA
E01  C                   END
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
        CDSS# = MaskData('SSN':'CDSS#':CDSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET
      ***** Beg Chg *** 06/09/20 *******************************************
      /Free
        CDSS# = UnMaskData('SSN':'CDSS#':CDSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   MOVE      '1'           *IN91
     C*
     C                   ENDSR
     C*****************************************************************
     C*  THIS SUBROUTINE LOADS ADDITIONAL INFORMATION FOR THE
     C*  ERROR REJECTION REPORT AND HANDLES THE PRINTING OF THE
     C*  DETAIL LINES.
     C*****************************************************************
     C     RPDET1        BEGSR
     C*
     C*  LOAD THE DETAIL INFORMATION
     C*
     C                   MOVEL     DN(43)        XXCNAM           30
     C     XXCNAM        CAT       ',':0         XXCNAM
     C     XXCNAM        CAT       DN(44):1      XXCNAM
     C                   MOVEL     DN(31)        XXACDT
     C*
     C*  GET THE DATA ELEMENT # AND DESCRIPTION
     C*
     C                   Z-ADD     DNDN#         XXDN#
     C                   MOVEL     DNDESC        XXDNDS
     C*
     C*  GET THE ERROR DESCRIPTION.
     C*
     C                   MOVE      XXERRS        XXERR#
     C                   MOVE      *BLANKS       XXEDSC
     C     ERKY          CHAIN     PER01                              99
     C  N99              MOVEL     ERDESC        XXEDSC
     C*
     C*   PRINT THE SUB HEADING IF NECESSARY, THEN PRINT THE DETAIL.
     C*
B01  C     XTEUSR        IFNE      X1EUSR
     C                   EXCEPT    HEAD1
     C                   MOVEL     XTEUSR        X1EUSR
     C                   MOVE      '0'           *INOB
E01  C                   END
B01  C     *INOB         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   MOVE      'N'           XXERR
E01  C                   END
     C                   MOVE      '1'           *IN92
B01  C     XXERR         IFEQ      'N'
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
        CDSS# = MaskData('SSN':'CDSS#':CDSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   EXCEPT    SUBH1
E01  C                   END
      ***** Beg Chg *** 06/09/20 *******************************************
      /Free
        CDSS# = UnMaskData('SSN':'CDSS#':CDSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   EXCEPT    DET1
     C*
     C                   ENDSR
     C*****************************************************************
     C*  This subroutine loads the extract workfile so the report will
     C*  be in EOMB entry user order.
     C*****************************************************************
     C     LDWRKF        BEGSR
     C*
     C*  Read all 837 Log records for the entered jurisdiction ST
     C*
     C     L2KY          CHAIN     PLG02                              99
B01  C     *IN99         DOWEQ     *OFF
     C*
     C*  Check selection criteria
     C*
     C*****      LGFND     CABEQ999       SKIP1            SKIP FUND 999
     C     LGTSET        CABNE     '837'         SKIP1                          ONLY 148 RECORDS
     C     LGSTAT        CABEQ     'H'           SKIP1                          MUST BE RELEASED
     C     LGIDT         CABGT     XXEXDT        SKIP1                          MUST BE LE EXDT
B02  C     LGSTS         IFNE      'EI'                                         MUST NOT HAVE BEEN
     C     LGSTS         ANDNE     'EC'                                          SENT ALREADY
     C                   GOTO      SKIP1
E02  C                   END
     C*
     C*  Retrieve the EOMB entry user's name
     C*
     C     #HKY          CHAIN     PEO01                              99
     C   99              MOVE      *BLANKS       EOIUSR
     C*
     C*  LOAD THE RECORD INTO THE EXTRACT WORKFILE
     C*
     C                   Z-ADD     XXCO#         XTCO#
     C                   Z-ADD     LGSUB#        XTSUB#
     C                   Z-ADD     LGFND         XTFND
     C                   Z-ADD     LGFYR         XTFYR
     C                   Z-ADD     LGCASE        XTCASE
     C                   Z-ADD     LGEOB#        XTEOB#
     C                   Z-ADD     LGLSEQ        XTLSEQ
     C                   MOVEL     LGTSET        XTTSET
     C                   MOVEL     EOIUSR        XTSORT
     C                   MOVEL     EOIUSR        XTEUSR
     C                   Z-ADD     LGDUE         XTDUE
     C                   MOVEL     Q1USER        XTUSER
     C                   MOVEL     Q1PGM         XTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        XTTIME
     C                   Z-ADD     XXDT7         XTDT
     C                   WRITE     PXT01
     C*
     C*  PROCESS NEXT LOG RECORD
     C*
     C     SKIP1         TAG
     C     L2KY          READE     PLG02                                  99
E01  C                   ENDDO
     C*
     C                   ENDSR
     O**************************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                                51 '.1999'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             118
     O                       DSDATE        Y    198
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       XXTITL             129
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD           2
     O                                           19 'JURISDICTION STATE:'
     O                       QQJST               22
     O                                           53 'EXTRACT SUBMISSION DATE:'
     O                       X1EXDT              64 '  /  /    '
     O                                           85 'EOMB ENTRY USER:'
     O                       XTEUSR              96
     O          E            HEAD           0
     O                                            9 'EOB #'
     O                                           27 'CARRIER CLAIM #'
     O                                           49 'CLAIMANT NAME'
     O                                           78 'SOC SEC #'
     O                                           93 'DT OF INJURY'
     O                                          109 'PROVIDER ID #'
     O                                          131 'DATES OF SERVICE'
     O                                          150 'AMOUNT BILLED'
     O                                          165 'AMOUNT PAID'
     O          E            HEAD           2
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
     O**************************************************************************
     O*  EDI 837 ERROR REJECTION REPORT
     OQSYSPRT1  E            ROUTE1         1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       X1TITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE1         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE1         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE1
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                                51 '.1999'
      ***** Beg Chg ***** 08/26/14 *********************************************
     O********* E            ROUTE          1 59
     O          E            ROUTE1         1 59
      ***** End Chg ***** 08/26/14 *********************************************
     O                       #BRND1
      ***** Beg Chg ***** 08/26/14 *********************************************
     O********* E            ROUTE          1
     O          E            ROUTE1         1
      ***** End Chg ***** 08/26/14 *********************************************
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             118
     O                       DSDATE        Y    198
     O          E            HEAD1          2
     O                       Q1PGM               10
     O                       X1TITL             129
     O                                          192 'PAGE:'
     O                       PAGE1         Z    198
     O          E            HEAD1          2
     O                                           19 'JURISDICTION STATE:'
     O                       QQJST               22
     O                                           53 'EXTRACT SUBMISSION DATE:'
     O                       X1EXDT              64 '  /  /    '
     O                                           85 'EOMB ENTRY USER:'
     O                       XTEUSR              96
     O          E            HEAD1          0
     O                                            9 'EOB #'
     O                                           27 'CARRIER CLAIM #'
     O                                           49 'CLAIMANT NAME'
     O                                           78 'SOC SEC #'
     O                                           93 'DT OF INJURY'
     O                                          109 'PROVIDER ID #'
     O                                          131 'DATES OF SERVICE'
     O                                          150 'AMOUNT BILLED'
     O                                          165 'AMOUNT PAID'
     O          E            HEAD1          1
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
     O          E            HEAD1          0
     O                                            7 'DN#'
     O                                           33 'DATA ELEMENT DESCRIPTION'
     O                                           55 'ERR#'
     O                                           74 'ERROR DESCRIPTION'
     O          E            HEAD1          1
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
     O*
     O          E            DET1           1
     O                       XXDN#         4      7
     O                       XXDNDS              49
     O                       XXERR#        4     54
     O                       XXEDSC              87
     O*
     O          E            MSG1        1
     O                                           98 '*** NO RECORDS MEET '
     O                                          119 'SELECTED CRITERIA ***'
     O*
     O**************************************************************************
     O*  EDI 837 SUMMARY REPORT
     OQSYSPRT2  E            ROUTE2         1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE2         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE2         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE2         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE2         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE2         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE2         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE2         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE2         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE2         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE2         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE2         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE2         2
     O                                           19 'REPORT TITLE:'
     O                       X2TITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE2         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE2         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE2
     O****                               23 '(C) COPYRIGHT PROGRESSI'
     O****                               46 'VE DATA SOLUTIONS. INC,'
     O****                               51 '.1999'
      ***** Beg Chg ***** 08/26/14 *********************************************
     O********* E            ROUTE          1 59
     O          E            ROUTE2         1 59
      ***** End Chg ***** 08/26/14 *********************************************
     O                       #BRND1
      ***** Beg Chg ***** 08/26/14 *********************************************
     O********* E            ROUTE          1
     O          E            ROUTE2         1
      ***** End Chg ***** 08/26/14 *********************************************
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O          E            HEAD2          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             118
     O                       DSDATE        Y    198
     O          E            HEAD2          2
     O                       Q1PGM               10
     O                       X2TITL             129
     O                                          192 'PAGE:'
     O                       PAGE2         Z    198
     O          E            HEAD2          2
     O                                           19 'JURISDICTION STATE:'
     O                       QQJST               22
     O                                           53 'EXTRACT SUBMISSION DATE:'
     O                       X1EXDT              64 '  /  /    '
     O*
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
