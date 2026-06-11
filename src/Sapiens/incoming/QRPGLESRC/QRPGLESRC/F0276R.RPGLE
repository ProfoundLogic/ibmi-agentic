     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H****************************************************************
     H* PROGRAM: F0276R                                              *
     H* DESCRIPTION: MEDICAL SERVICES FILE PROGRAM                   *
     H****************************************************************
     H* INDICATORS USAGE:                                            *
     H*                                                              *
     H* GENERAL                                                      *
     H*  98 - GENERAL CHAIN                                          *
     H*  99 - GENERAL CHAIN                                          *
     H*                                                              *
     H* (CHKSSM)                                                     *
     H*  11 - SUBSET MESSEAGE                                        *
     H*                                                              *
     H* (READ & READP)                                               *
     H*  95 - READ INDICATOR                                         *
     H*                                                              *
     H* (SETGT & SETLL)                                              *
     H*  96 - SETLL/SETGT INDICATOR                                  *
     H*                                                              *
     H* SCR03 (ERRCK2)                                               *
     H*  71 - INVALID MEDICAL SERVICE CODE                           *
     H*  72 - INVALID CLOSURE STATUS CODE                            *
     H*  73 - INVALID STAFF CODE                                     *
     H*                                                              *
     H* SCR03 (KJDO)                                                 *
     H*  30 - POSITION TO SSSTS.                                     *
     H*  31 - POSITION TO SSSTAF.                                    *
     H*                                                              *
     H****************************************************************
     H* REVISIONS:
     H*
     H*  06/09/20  C:212 Project : SY420
     H*                  Mask SSN
     H*                  Recompile only
     H*
     H*  08/04/08 C:33  Project: 139MIT
     H*                 Only display for CLT139.
     H*
     H*  01/24/08 C:62  PROJECT: 139MIT
     H*                 ADD BILLING AMOUNT PROCESSING.
     H*
     H*  12/14/01 C:62  PROJECT: MC111 30098
     H*                 -REDEFINE INVOICED 'M' TYPE EOB TO ALSO BE IF
     H*                  EAEPFL = 'P'(BEEN THRU BATCH) IF AUTO CHECK
     H*
     H*  01/26/01  C:62  PROJECT: MCMNT  BASE ONLY
     H*                  COMPILE ONLY
     H*
     H*  05/19/00  C:62  PROJECT: MC172
     H*                  RECON IS NOW IMPORTANT SO ADD TO PARM LIST.
     H*
     H*  09/27/99 C:62  PROJECT:MC212
     H*                 GETTING T/M TO WORK.
     H*
     H*  09/07/99 C:62  PROJECT:MC212
     H*                 GETTING T/M TO WORK.
     H*                                                              *
     H*  06/21/99 C:62  PROJ: 101AX                                  *
     H*                 ADD OPTION 90 OUTCOME/SAVINGS                *
     H*                                                              *
     H*  05/07/99 C:62  PROJ: MC200                                  *
     H*                 ADD F4=DELETE                                *
     H*                                                              *
     H*  12/17/98 C:94  PROJ: MC152                                  *
     H*                 REMOVE ALL UPDATES TO FAE1P AND NAMBP AND    *
     H*                 LET API AA001R DO THE UPDATES INSTEAD        *
     H*                                                              *
     H*  12/03/98 C:53  PROJ: MC098 14996                             *
     H*                 REMOVE UPDATE TO CONTROL EOB # IN THE CONTROL *
     H*                 FILE ON A DELETE ROUTINE                      *
     H*                                                              *
     H* 12/01/98 C:62 PROJECT: MC172                                 *
     H*               - CHANGED ASSIGNMENT OF EOFLG5 FROM ' ' TO 'Y'.*
     H*                                                              *
     H* 10/07/98 C:94 PROJECT: 87MFX                                 *
     H*               CHANGED F12=PREVIOUS TO F12=CANCEL             *
     H*                                                              *
     H* 08/19/98 C:62 PROJECT: MC177                                 *
     H*               VARIOUS CHANGES TO T/M.                        *
     H*                                                              *
     H* 08/06/98 C:94 PROJECT: MCMNT                                 *
     H*               FIXED POSITION TO DATE.                        *
     H*                                                              *
     H* 04/27/98 C:62 PROJECT: MC145                                 *
     H*               ADDED F11 TO INCLUDE VIEW BY BGDT AND ENDT; AND*
     H*               MAKE THAT VIEW THE DEFAULT VIEW.               *
     H*                                                              *
     H* 01/28/98 C:62 PROJECT: MC145                                 *
     H*               1) HEADING CHANGED TO 'WW CASE MAMAGEMENT SER- *
     H*                  VICES'                                      *
     H*               2) COLUMN HEADING CHANGED TO 'CASE MGMT SVC'   *
     H*               3) ZERO SUPRESSED SEQ #.                       *
     H*                                                              *
     H* 01/08/98 C:86 PROJECT: MC145                                 *
     H*               DISPLAY FILE F0275F HAS BEEN CHANGED TO PRINT  *
     H*               A 2 LINE SUB-HEADING. THIS PROGRAM CALLS FA702R*
     H*               TO PRINT IT & USES LIKE DEFINES RO REDEFINE THE*
     H*               OMMITTED SCREEN FIELDS.                        *
     H*                                                              *
     H* 09/26/97 C:59 PROJECT: SUPPORT MC087/#8828                   *
     H*               Calling incorrect program on darts             *
     H*               correct is F0270R NOT F0075R                   *
     H*                                                              *
     H* 02/18/97 C:56  87MAZ                                         *
     H*                CREATED                                       *
     H****************************************************************
     FF0276F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FFAMSP     IF   E             DISK
     F                                     RENAME(PMS01:PFRCDF)
     FFAMSL6    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PMS01:DBRCDF)
     FFAMSL5    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PMS01:DBRCDF2)
     FFMCMP     IF   E             DISK
     F********** BEGIN DELETE ******************* 01/08/97 ************
     F**********FMCDL   IF  E           K        DISK
     F********** END   DELETE ******************* 01/08/97 ************
     FFTMCP     IF   E             DISK
     FFTMCL     IF   E           K DISK
     F                                     RENAME(PMC01:PMC02)
     FSMUPP     IF   E             DISK
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FFTCLP     IF   E             DISK
     FFTCLL     IF   E           K DISK
     F                                     RENAME(PCL01:PCL02)
     F*** BEGIN DEL *** 09/07/99 **************************************
     F*FAEOP   UF  E                    DISK                      A
     F**********                                    KINFDS DBFDSA
     F*** END   DEL *** 09/07/99 **************************************
     FFAEOL4    IF   E           K DISK
     F                                     INFDS(DBFDSB)
     F                                     RENAME(PEO01:PEO02)
     F*** BEGIN END *** 09/27/99 **************************************
     FFAEAL1    IF   E           K DISK
     F*** END   END *** 09/27/99 **************************************
     FFMC1L     UF   E           K DISK
     F*** BEGIN DEL *** 05/07/99 **************************************
     FNDTML6    IF   E           K DISK
     F*** END   DEL *** 05/07/99 **************************************
     F********* BEGIN DELETE  ******************* 01/08/98 ************
     F********* FMF1L   IF  E           K        DISK
     F********* END   DELETE  ******************* 01/08/98 ************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column Headings
     D***  Begin Add  ***  08/04/08  **************************************
     D HD2             S             78    DIM(2) CTDATA PERRCD(1)              Column Headings
     D***  End   Add  ***  08/04/08  **************************************
     D OF              S              1    DIM(15) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(15) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*** BEGIN DEL *** 09/07/99 **************************************
     D**********DBFDSA      DS
     D**********                          B 397 4000DBRRNA
     D*** END   DEL *** 09/07/99 **************************************
     D DBFDSB          DS
     D  DBRRNB               397    400B 0
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  X1DATA                 1     59
     D  X1BGDT                 1     10
     D  X1ENDT                12     21
     D  X1MSCD                25     28
     D  X1SEQ#                31     35
     D*** BEGIN CHG *** 01/24/08 **************************************
     D**********                             40  59 X1DESC
     D  X1DESC                40     54
     D*** END   CHG *** 01/24/08 **************************************
     D                 DS
     D  X2DATA                 1     59
     D  X2MSCD                 1      4
     D  X2SEQ#                 9     13
     D*** BEGIN CHG *** 01/24/08 **************************************
     D**********                             16  35 X2DESC
     D**********                             38  47 X2BGDT
     D**********                             50  59 X2ENDT
     D  X2DESC                16     30
     D  X2BGDT                33     42
     D  X2ENDT                45     54
     D*** END   CHG *** 01/24/08 **************************************
     D*** BEGIN CHG *** 01/24/08 **************************************
     D*** BEG ADD *** 12/16/98 ***************************************
     D*  WRITE/UPDATE DATA STRUCTURE.
     D                 DS
     D  KKWRUP                 1      7
     D  XXFAEO                 1      1
     D  XXFAE1                 2      2
     D  XXFAHB                 3      3
     D  XXNAPB                 4      4
     D  XXNAMB                 5      5
     D  XXNATR                 6      6
     D  XXFAEA                 7      7
     D*** END ADD *** 12/16/98 ***************************************
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D* /COPY required constants
     D*
     D*** BEGIN CHG *** 01/28/98 **************************************
     D**********    'Medical Services'    C         OPTNM
     D OPTNM           C                   CONST('Case Management-
     D                                      Services')
     D*** END   CHG *** 01/28/98 **************************************
     D ACDPGM          C                   CONST('F0276R01')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D SF1PAG          C                   CONST(8)
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D*** BEGIN ADD *** 05/07/99 **************************************
     D DELETE          C                   CONST('*** DELETED ***')
     D*** END   ADD *** 05/07/99 **************************************
     D*** BEGIN ADD *** 06/21/99 **************************************
     D CLRFKY          C                   CONST('                    -
     D                                            NNN')
     D*** END   ADD *** 06/21/99 **************************************
     D*
     D* Program specific constants
     D*
     D** DARTS KEY (SC010R) FOR LINK WITH WCMS
     D*
     D                 DS
     D  DSKEY                  1     40
     D  DSINFO                 1     16  0
     D  DSCO#                  1      3  0
     D  DSFND                  4      6  0
     D  DSFYR                  7      9  0
     D  DSCASE                10     16  0
     D*
     D** DARTS KEY (SC010R) FOR OUTSIDE ACCOUNT
     D*
     D                 DS
     D  DRKEY                  1     40
     D  DRINFO                 1     16  0
     D  DRCO#                  1      3  0
     D  DRSUB#                 4      8  0
     D  DRFND                  9     11  0
     D  DRFYR                 12     14  0
     D  DRCASE                15     21  0

     I*
     I*
     I* Program specific constants
     I*

     C*
     C     *LIKE         DEFINE    MSCO#         KKCO#
     C     *LIKE         DEFINE    MSSUB#        KKSUB#
     C     *LIKE         DEFINE    MSFND         KKFND
     C     *LIKE         DEFINE    MSFYR         KKFYR
     C     *LIKE         DEFINE    MSCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    EOEOB#        HDEOB#
     C     *LIKE         DEFINE    MSMSCD        KKMSCD
     C     *LIKE         DEFINE    MSBGDT        KKBGDT
     C     *LIKE         DEFINE    MCKEY         KKKEY
     C     *LIKE         DEFINE    UPSTAF        KKSTAF
     C     *LIKE         DEFINE    CLKEY         KKCKEY
     C*** BEGIN ADD *** 01/24/08 **************************************
     C     *LIKE         DEFINE    SCAMT         XXAMT
     C*** END   ADD *** 01/24/08 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0          RRN FAMSP
     C                   PARM                    @@RRN2            9 0          RRN FMCMP
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0          RRN FAMSP
     C                   PARM      @@RRN2        ##RRN2            9 0          RRN FMCMP
     C*
     C     ACDPL2        PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKBGDT
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKMSCD
     C*
     C*** KEY LIST FOR MEDICAL SERVICE CODE TABLE FILE
     C*
     C     MCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*** BEGIN ADD *** 05/07/99 **************************************
     C*
     C     TMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    SCMSCD
     C                   KFLD                    SCSEQ#
     C*** END   ADD *** 05/07/99 **************************************
     C*
     C*** KEY LIST FOR USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    KKSTAF
     C*
     C*** KEY LIST FOR CLOSURE CODE TABLE FILE
     C*
     C     CLKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCKEY
     C*** BEGIN ADD *** 09/27/99 **************************************
     C     EAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C     EOKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     EOKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* IF CALLED SEPERATED FROM PROGRAM CHAIN.
     C*
     C                   MOVEL     *OFF          STOP              1
     C*
     C                   SELECT
     C*
     C     @@CALL        WHENEQ    'L'
     C                   MOVEL     '1'           @@CALL
     C                   MOVEL     *ON           STOP
     C*
     C     @@CALL        WHENEQ    'M'
     C                   MOVEL     ' '           @@CALL
     C                   MOVEL     *ON           STOP
     C*
     C     @@CALL        WHENEQ    '2'
     C                   MOVEL     *ON           STOP
     C*
     C                   ENDSL
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSMSCD        IFNE      *BLANKS
     C     SSSTS         ORNE      *BLANKS
     C     SSSTAF        ORNE      *BLANKS
     C*
     C                   MOVE      *ON           *IN11
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** BEGIN DEL *** 09/07/99 **************************************
     C*================================================================
     C* DELEOB - Deletes record from EOMB file FAEOP.
     C*================================================================
     C********** DELEOB    BEGSR
     C*
     C********** DBRRNA    CHAINPEO01                99
     C*
     C**********           DELETPEO01
     C*
     C**********           ENDSR
     C*** END   DEL *** 09/07/99 **************************************
     C*** BEG DEL *** 12/17/98 ***************************************
     C*
     C*** NOTE ***
     C*** THESE SUBROUTINES CAN NOW BE FOUND IN API AA001R
     C*
     C*================================================================
     C* DELEB1 - Deletes record from EOMB auxiliary file FAE1P.
     C*================================================================
     C*================================================================
     C* DELMBT - Deletes record from Medical Bill Trans. File NAMBP.
     C*================================================================
     C*** END DEL *** 12/17/98 ***************************************
     C*
     C*================================================================
     C* DELUPD - Deletes records from FAEOP, FAE1P and NAMBP.
     C*          Updates FMC1P to reset EOB# to original value.
     C*================================================================
     C     DELUPD        BEGSR
     C*
     C* Deletes record from EOMB file (FAEOP).
     C*
     C*** BEGIN DEL *** 09/07/99 **************************************
     C**********           EXSR DELEOB
     C*** END   DEL *** 09/07/99 **************************************
     C*
     C*** BEGIN ADD *** 09/07/99 **************************************
     C*
     C                   MOVEL     *BLANKS       KKWRUP
     C                   Z-ADD     *ZEROS        @@RCN
     C*
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   MOVE      'D'           XXFAEO
     C*** END   ADD *** 09/07/99 **************************************
     C*** BEG ADD *** 12/16/98 ***************************************
     C                   MOVE      'D'           XXFAEA
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   ELSE
     C*** END   ADD *** 09/07/99 **************************************
     C                   MOVE      'D'           XXFAE1
     C                   MOVE      'D'           XXNAMB
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   MOVE      'D'           XXFAEO
     C*
     C                   ENDIF
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C                   CALL      'AA001R'
     C                   PARM      @@RRN2        @@RRN             9 0          RRN OF CASE
     C                   PARM                    @@EOB#            9 0          EOB #
     C                   PARM                    @@RCN             3 0          RECON #
     C                   PARM      Q1PGM         @@PGM            10            NAME OF PGM CALLING
     C                   PARM                    @@BAMT            9 2          BRS AMOUNT
     C                   PARM                    @@NAMT            9 2          NWS AMOUNT
     C                   PARM                    @@BHRF            1            BRS INV H/R FLAG
     C                   PARM                    @@BBLF            1            BRS INV PROC FLAG
     C                   PARM                    @@BINV            9 0          BRS INVOICE #
     C                   PARM                    @@BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    @@NHRF            1            NWS INV H/R FLAG
     C                   PARM                    @@NBLF            1            NWS INV PROC FLAG
     C                   PARM                    @@NINV            9 0          NWS INVOICE #
     C                   PARM                    @@SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        @@WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*** END ADD *** 12/16/98 ***************************************
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*** BEGIN ADD *** 05/07/99 **************************************
     C     SCSEL         IFEQ      '4'
     C*
     C     TMKY          CHAIN     PTM01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 05/07/99 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* SETOF POSITION TO CURSORS 30-32.
     C*
     C                   MOVEA     '000'         *IN(30)
     C*
     C* SUBSET SCREEN F10=SEARCH.
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C     SSMSCD        IFNE      *BLANKS
     C                   MOVEL     SSMSCD        KKKEY
     C     MCKY          CHAIN     PMC02                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        9071
     C     *IN90         CABEQ     '1'           CHKEND
     C                   ENDIF
     C                   ENDIF
     C*
     C     SSSTS         IFNE      *BLANKS
     C                   MOVEL     SSSTS         KKCKEY
     C     CLKY          CHAIN     PCL02                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        9072
     C     *IN90         CABEQ     '1'           CHKEND
     C                   ENDIF
     C                   ENDIF
     C*
     C     SSSTAF        IFNE      *BLANKS
     C                   MOVEL     SSSTAF        KKSTAF
     C     UPKEY         CHAIN     PUP02                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        9073
     C     *IN90         CABEQ     '1'           CHKEND
     C                   ENDIF
     C                   ENDIF
     C     CHKEND        ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C                   MOVE      *BLANKS       SSMSCD
     C                   MOVE      *BLANKS       SSSTS
     C                   MOVE      *BLANKS       SSSTAF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C** GET CLAIM IMFORMATION
     C*
     C     @@RRN2        CHAIN     PCM01                              99
     C*
     C                   Z-ADD     CMCO#         KKCO#
     C                   Z-ADD     CMSUB#        KKSUB#
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C                   MOVEL     *LOVAL        KKMSCD
     C                   Z-ADD     *HIVAL        KKBGDT
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVE      SCPST1        SRDTI
     C   21              CALL      'SRCVTDAT'    DTCYMD
     C   21              MOVEL     SRDTO         KKBGDT
     C*
     C   22              MOVEL     SCPST2        KKMSCD
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C   21              MOVEL     SCBGDT        KKBGDT
     C   22              MOVEL     SCMSCD        KKMSCD
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*** MEDICAL SERVICE CODE SEARCH
     C*
     C     HFLD          IFEQ      'SSMSCD'
     C*
     C                   CALL      'F2425R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        90
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PMC01                              99
     C*
     C                   MOVEL     MCKEY         SSMSCD
     C*
     C                   SETON                                        9030
     C*
     C                   ENDIF                                                  *INKL EQ *ON
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C*** CLOSURE STATUS SEARCH
     C*
     C     HFLD          IFEQ      'SSSTS'
     C*
     C                   CALL      'F2415R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        9030
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PCL01                              99
     C*
     C                   MOVEL     CLKEY         SSSTS
     C                   SETON                                        9031
     C*
     C                   ENDIF                                                  *INKL EQ *ON
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C*** STAFF CODE SEARCH
     C*
     C     HFLD          IFEQ      'SSSTAF'
     C*
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      3             ##VIEW            1 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C                   SETON                                        9031
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PUP01                              99
     C*
     C                   MOVEL     UPSTAF        SSSTAF
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  *INKL EQ *ON
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C                   ENDSR
     C*================================================================
     C* KNDO - F14=DARTS.
     C*================================================================
     C     KNDO          BEGSR
     C*
     C************** BEGIN ADD ***************** 01/08/98 *************
     C*
     C* CALL API TO ACCESS DARTS
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE                   90
     C*
     C                   CALL      'FA703R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    ##WCIN            1
     C                   PARM                    ##WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANK        ##KEY            40
     C*
     C************** END   ADD ***************** 01/08/98 *************
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      *BLANKS       SCMSCD
     C                   Z-ADD     0             SCSEQ#
     C                   Z-ADD     0             SCBGDT
     C                   Z-ADD     0             SCENDT
     C                   MOVE      *BLANKS       SCSTS
     C                   MOVE      *BLANKS       SCSTAF
     C*
     C     MSSTAT        IFNE      'I'
     C*
     C                   MOVEL     MSMSCD        SCMSCD
     C                   MOVEL     MSSEQ#        SCSEQ#
     C                   Z-ADD     MSBGDT        SCBGDT
     C                   Z-ADD     MSENDT        SCENDT
     C*
     C                   MOVEL     MSSTS         SCSTS
     C                   MOVEL     MSSTAF        SCSTAF
     C*
     C   21              MOVEL     MSMSCD        X1MSCD
     C   22              MOVEL     MSMSCD        X2MSCD
     C*
     C                   MOVEL(P)  MSSEQ#        ##NUM
     C*
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM      *BLANKS       ##MASK           10
     C*
     C   21              MOVEL     ##NUM         X1SEQ#
     C   22              MOVEL     ##NUM         X2SEQ#
     C*
     C                   Z-ADD     MSBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXBGDT            8
     C*
     C                   MOVEL(P)  XXBGDT        ##NUM
     C*
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM      'DATE'        ##MASK           10
     C*
     C   21              MOVEL     ##NUM         X1BGDT
     C   22              MOVEL     ##NUM         X2BGDT
     C*
     C                   Z-ADD     MSENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXENDT            8
     C*
     C                   MOVEL(P)  XXENDT        ##NUM
     C*
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM      'DATE'        ##MASK           10
     C*
     C   21              MOVEL     ##NUM         X1ENDT
     C   22              MOVEL     ##NUM         X2ENDT
     C*
     C*** GET MEDICAL SERVICE CODE DESCRIPTION
     C*
     C                   MOVEL     MSMSCD        KKKEY
     C*
     C     MCKY          CHAIN     PMC02                              99
     C*
     C     *IN99         IFEQ      '0'
     C   21              MOVEL     MCDESC        X1DESC
     C   22              MOVEL     MCDESC        X2DESC
     C                   ENDIF
     C*
     C   21              MOVEL     X1DATA        SCDATA
     C   22              MOVEL     X2DATA        SCDATA
     C*
     C*** BEGIN CHG *** 01/24/08 **************************************
     C                   Z-ADD     0             SCAMT
     C*
     C     TMKY          CHAIN     PTM01                              99
     C     *IN99         DOWEQ     *OFF
     C     TMBFLG        IFNE      'N'
     C*
     C     TMHRS         MULT(H)   TMHRRT        XXAMT
     C                   ADD       XXAMT         SCAMT
     C     TMMILE        MULT(H)   TMMIRT        XXAMT
     C                   ADD       XXAMT         SCAMT
     C                   Z-ADD     TMMSCG        XXAMT
     C                   ADD       XXAMT         SCAMT
     C*
     C                   ENDIF
     C     TMKY          READE     PTM01                                  99
     C                   ENDDO
     C*** END   CHG *** 01/24/08 **************************************
     C                   ENDIF                                                  MSSTAT IFEQ
     C*
     C                   ENDSR
     C*================================================================
     C* MVEOBF - Move information to FAEOP fields.
     C*================================================================
     C     MVEOBF        BEGSR
     C*
     C     ##CO#         CHAIN     PC101                              99
     C*
     C                   Z-ADD     C1EOB#        HDEOB#
     C*
     C     C1EOB#        IFEQ      *HIVAL
     C*
     C                   Z-ADD     1             C1EOB#
     C*
     C                   ELSE
     C*
     C                   ADD       1             C1EOB#
     C*
     C                   ENDIF
     C*
     C                   EXSR      UPDC1
     C*
     C                   Z-ADD     C1EOB#        EOCHK#
     C                   Z-ADD     C1EOB#        EOEOB#
     C                   Z-ADD     C1EOB#        XXEOB#
     C                   Z-ADD     ##CO#         EOCO#
     C                   Z-ADD     CMSUB#        EOSUB#
     C                   Z-ADD     CMFND         EOFND
     C                   Z-ADD     CMFYR         EOFYR
     C                   Z-ADD     CMCASE        EOCASE
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOTRDT
     C*
     C                   MOVE      *ZEROS        EOFEIN
     C                   Z-ADD     *ZEROS        EOMOD
     C                   Z-ADD     *ZEROS        EOAMT
     C                   Z-ADD     *ZEROS        EOFRDT
     C                   Z-ADD     *ZEROS        EOTODT
     C                   MOVE      *BLANKS       EOTRCD
     C                   MOVE      *BLANKS       EOPNAM
     C                   MOVE      *BLANKS       EOADR1
     C                   MOVE      *BLANKS       EOADR2
     C                   MOVE      *BLANKS       EOCTY
     C                   MOVE      *BLANKS       EOST
     C                   Z-ADD     *ZEROS        EOZIP
     C                   MOVE      *BLANKS       EOEOB1
     C                   MOVE      *BLANKS       EOEOB2
     C                   MOVE      *BLANKS       EOEOB3
     C                   MOVE      'M'           EOSYS
     C                   MOVE      'X'           EOFLG1
     C                   MOVE      'R'           EOFLG2
     C                   MOVE      *BLANKS       EOFLG3
     C                   MOVE      'Y'           EOFLG4
     C                   MOVE      *BLANKS       EOFLG5
     C                   MOVE      *BLANKS       EOFLG6
     C                   MOVE      *BLANKS       EOFLG7
     C                   MOVE      *BLANKS       EOFLG8
     C                   MOVE      *BLANKS       EOFLG9
     C                   Z-ADD     *ZEROS        EOAMTC
     C                   MOVE      *BLANKS       EOSTAT
     C                   Z-ADD     *ZEROS        EOWCCK
     C                   MOVE      *BLANKS       EODIA1
     C                   MOVE      *BLANKS       EODIA2
     C                   MOVE      *BLANKS       EODIA3
     C                   MOVE      *BLANKS       EODIA4
     C                   MOVE      *BLANKS       EOEMPL
     C                   Z-ADD     *ZEROS        EODFC
     C                   MOVE      *BLANKS       EOFLIC
     C                   MOVE      'N'           EOTAPE
     C                   Z-ADD     1             EO#BLL
     C                   MOVE      *BLANKS       EOCLSF
     C                   MOVE      *BLANKS       EOAST
     C                   MOVE      *BLANKS       EOAUDT
     C                   MOVE      *BLANKS       EOBTFL
     C                   MOVE      'H'           EOHRFL
     C                   MOVE      *BLANKS       EOBLFL
     C                   MOVE      *BLANKS       EONLFL
     C                   MOVE      *BLANKS       EODIFL
     C                   Z-ADD     *ZEROS        EOINV#
     C                   MOVE      *BLANKS       EORMDS
     C                   MOVE      *BLANKS       EORMK1
     C                   MOVE      *BLANKS       EORMK2
     C                   MOVE      *BLANKS       EORMK3
     C                   MOVE      *BLANKS       EOCKCN
     C                   Z-ADD     *ZEROS        EOOTHR
     C                   Z-ADD     *ZEROS        EONDIS
     C                   MOVE      *BLANKS       EOSB01
     C                   MOVE      *BLANKS       EOSB02
     C                   MOVE      *BLANKS       EOSB03
     C                   MOVE      *BLANKS       EOSB04
     C                   MOVE      *BLANKS       EOSB05
     C                   MOVE      *BLANKS       EOSB06
     C                   MOVE      *BLANKS       EOSB07
     C                   MOVE      *BLANKS       EOSB08
     C                   MOVE      *BLANKS       EOSB09
     C                   MOVE      *BLANKS       EOSB10
     C                   Z-ADD     *ZEROS        EOBAMT
     C                   MOVE      *BLANKS       EOZIPA
     C                   MOVE      *BLANKS       EOCERT
     C                   Z-ADD     *ZEROS        EOEXT
     C*
     C                   ENDSR
     C*================================================================
     C* OPT17 - Time/Materials Activity
     C*================================================================
     C     OPT17         BEGSR
     C*
     C     @@CALL        IFEQ      ' '
     C*
     C                   MOVEL     ' '           ##CALL
     C*
     C                   ELSE
     C*
     C                   MOVEL     '1'           ##CALL
     C*
     C                   ENDIF
     C*
     C                   MOVE      *ZEROS        XXEOB#
     C*
     C     EOKY          CHAIN     PEO02                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*** BEGIN ADD *** 09/27/99 **************************************
     C     ##PRVB        IFEQ      'Y'
     C*
     C     EAKY          CHAIN     PEA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     EABINV        IFEQ      *ZEROS
     C     EASYS         ANDEQ     'M'
     C*** BEGIN ADD *** 12/14/01 **************************************
     C     ##ACIS        ANDNE     'Y'
     C     EABINV        OREQ      *ZEROS
     C     EASYS         ANDEQ     'M'
     C     EAEPFL        ANDNE     'P'
     C     ##ACIS        ANDEQ     'Y'
     C*** END   ADD *** 12/14/01 **************************************
     C*
     C                   Z-ADD     EAEOB#        XXEOB#
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C*** BEGIN CHG *** 12/01/98 **************************************
     C********** EOBLFL    IFNE 'P'
     C     EOINV#        IFEQ      *ZEROS
     C*** END   CHG *** 12/01/98 **************************************
     C     EOSYS         ANDEQ     'M'
     C*
     C                   Z-ADD     EOEOB#        XXEOB#
     C                   LEAVE
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 09/27/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C     EOKY          READE     PEO02                                  99
     C*
     C                   ENDDO
     C*
     C     ##WCIF        IFEQ      'Y'
     C*
     C* API to access WARPP to be able to determine if there are any
     C* check requests on this particular EOMB, before allowing T/M.
     C*
     C                   ENDIF
     C*
     C     XXEOB#        IFNE      *ZEROS
     C*
     C                   CALL      'F3040R'
     C                   PARM                    ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      DBRRNB        ##RRN2
     C                   PARM      SCDBRR        ##RRN3            9 0
     C*** BEGIN ADD *** 08/19/98 **************************************
     C                   PARM      *BLANKS       ##CERT           15
     C                   PARM      *ZEROS        ##EXT             3 0
     C*** END   ADD *** 08/19/98 **************************************
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   PARM      *BLANKS       ##EOB             1
     C                   PARM      *ZEROS        ##RCN             3 0
     C*** END   ADD *** 05/19/00 **************************************
     C*** BEGIN ADD *** 09/07/99 **************************************
     C     ##CALL        IFEQ      'F'
     C*
     C                   EXSR      DELUPD
     C*
     C                   ENDIF
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C                   ELSE
     C*
     C                   EXSR      WRITER
     C*** BEGIN ADD *** 09/07/99 **************************************
     C*
     C                   MOVE      CMSUB#        KKSUB#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMFYR         KKFYR
     C                   MOVE(P)   CMCASE        KKCASE
     C                   MOVE      *ZEROS        XXEOB#
     C*
     C     EOKY          CHAIN     PEO02                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*** BEGIN ADD *** 09/27/99 **************************************
     C     ##PRVB        IFEQ      'Y'
     C*
     C     EAKY          CHAIN     PEA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     EABINV        IFEQ      *ZEROS
     C     EASYS         ANDEQ     'M'
     C*** BEGIN ADD *** 12/14/01 **************************************
     C     ##ACIS        ANDNE     'Y'
     C     EABINV        OREQ      *ZEROS
     C     EASYS         ANDEQ     'M'
     C     EAEPFL        ANDNE     'P'
     C     ##ACIS        ANDEQ     'Y'
     C*** END   ADD *** 12/14/01 **************************************
     C*
     C                   Z-ADD     EAEOB#        XXEOB#
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C     EOINV#        IFEQ      *ZEROS
     C     EOSYS         ANDEQ     'M'
     C*
     C                   Z-ADD     EOEOB#        XXEOB#
     C                   LEAVE
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 09/27/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C     EOKY          READE     PEO02                                  99
     C*
     C                   ENDDO
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C*** BEGIN DEL *** 08/19/98 **************************************
     C**********           MOVE CMCO#     #1CO#
     C**********           MOVE CMSUB#    #1SUB#
     C**********           MOVE CMFND     #1FND
     C**********           MOVE CMFYR     #1FYR
     C**********           MOVE CMCASE    #1CASE
     C**********           MOVE CMCASE    #1EOB#
     C*
     C**********           CALL 'N6181R'
     C**********           PARM           #1CO#   3
     C**********           PARM           #1SUB#  5
     C**********           PARM           #1FND   3
     C**********           PARM           #1FYR   3
     C**********           PARM           #1CASE  7
     C**********           PARM           #1EOB#  9
     C*** END   DEL *** 08/19/98 **************************************
     C*
     C                   CALL      'F3040R'
     C                   PARM                    ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C*** BEGIN CHG *** 08/19/98 **************************************
     C**********           PARM DBRRNA    ##RRN2
     C                   PARM      DBRRNB        ##RRN2
     C*** END   CHG *** 08/19/98 **************************************
     C                   PARM      SCDBRR        ##RRN3
     C*** BEGIN ADD *** 08/19/98 **************************************
     C                   PARM      *BLANKS       ##CERT           15
     C                   PARM      *ZEROS        ##EXT             3 0
     C*** END   ADD *** 08/19/98 **************************************
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   PARM      *BLANKS       ##EOB
     C                   PARM      *ZEROS        ##RCN
     C*** END   ADD *** 05/19/00 **************************************
     C*
     C     ##CALL        IFEQ      'F'
     C*
     C                   EXSR      DELUPD
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 06/21/99 **************************************
      *================================================================
      * OPT90 - Outcome/Savings
      *================================================================
     C     OPT90         BEGSR
      *
     C                   CALL      'F0280R'
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      *ZERO         ##RRN             9 0          FDOSP
     C                   PARM      SCDBRR        ##RRN1            9 0          FAMSP
     C                   PARM      *ZERO         ##RRN2            9 0          UMPCP
      *
      *
     C                   ENDSR
     C*** END   ADD *** 06/21/99 **************************************
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                           F10=SEARCH
     C     *INKN         CASEQ     *ON           KNDO                           F14=DARTS
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '17'          OPT17
     C*** BEGIN ADD *** 06/21/99 **************************************
     C     SCSEL         CASEQ     '90'          OPT90
     C*** END   ADD *** 06/21/99 **************************************
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C     STOP          ANDEQ     *OFF
     C*
     C                   CALL      'F0277R'      ACDPL2
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   MOVEL     *ON           RSTPGM
     C                   MOVEL     *OFF          *INKL
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *INLR
     C*
     C                   ENDIF
     C*
     C                   MOVE      '1'           *INLR
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C***  Begin Add  ***  08/04/08  **************************************
     C                   SELECT
     C     CLTID         WHENEQ    '139'
     C                   MOVE      X'20'         AT1                            Normal
     C                   MOVE      X'22'         AT2                            HI
     C                   MOVEA     HD2           HD
     C                   OTHER
     C                   MOVE      X'27'         AT1                            ND
     C                   MOVE      X'27'         AT2                            ND
     C                   ENDSL
     C***  End   Add  ***  08/04/08  **************************************
     C*
     C************* BEGIN ADD ****************** 01/08/98 *************
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
      *
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C*** BEGIN ADD *** 06/21/99 **************************************
10   C                   PARM                    ##RHAB            1            REHAB INST
11   C                   PARM                    ##MSS             1            MS SUPPORT
12   C                   PARM                    ##FSCH            1            FEE SCHEDULE
13   C                   PARM                    ##PRVB            1            PROVIDER BILLS
14   C                   PARM                    ##NPWC            1            NON-PDS WC  LS
15   C                   PARM                    ##OS              1            OUTOME/SAVINGS
     C*** BEGIN ADD *** 12/14/01 **************************************
     C*
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANKS       ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANKS       ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANKS       ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANKS       ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANKS       ##SDTS            1            ALLOW FUTURE SERV DT
     C                   PARM      *BLANKS       ##MCIV            1            MED CASES INIT VIEW
     C                   PARM      *BLANKS       ##PBIV            1            PROV BILLS ININT VIE
     C                   PARM      *BLANKS       ##AINV            1            AUTO INVOICE FLAG
     C                   PARM      *BLANKS       ##WHPG            1            Work History Usage
     C                   PARM      *BLANKS       ##PRIV            1            PROVIDER INIT VIEW
     C                   PARM      *BLANKS       ##TDUP            1            ADJUDICATION DUP CRI
     C                   PARM      *BLANKS       ##NWSL            1            PROV NTWK SELECTION
     C*** END   ADD *** 12/14/01 **************************************
     C*
     C* CHECK TO SEE IF OUTCOME/SAVINGS IS INSTALLED
     C*
     C     ##OS          IFNE      'Y'
     C*
     C                   MOVEL     CLRFKY        OFD(6)
     C*
     C                   ENDIF
     C*** END   ADD *** 06/21/99 **************************************
     C************* END   ADD ****************** 01/08/98 *************
     C*********** BEGIN DELETE ******************* 01/08/98 ***********
     C*********            MOVELCMSS#     XXSS#
     C*********  CDKEY     CHAINPCD01                98
     C*********  *IN98     IFEQ '0'
     C*********            MOVELCDLSN     ##LAST 56
     C*********            MOVELCDFSN     ##FRST 15
     C*********            MOVE '1'       ##TERM  1
     C*********            CALL 'SRNAME'
     C*********            PARM           ##LAST
     C*********            PARM           ##FRST
     C*********            PARM           ##TERM
     C*********            MOVEL##LAST    SCCLNM
     C*********            ELSE
     C*********            MOVEL*BLANKS   SCCLNM
     C*********            ENDIF
     C*********** END   DELETE ******************* 01/08/98 ***********
     C*********** BEGIN ADD ********************** 01/08/98 ***********
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     SCL1HD        PARM      *BLANK        ##L1             78
     C     SCL2HD        PARM      *BLANK        ##L2             78
      *
     C*********** END   ADD ********************** 01/08/98 ***********
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*** BEGIN ADD *** 05/07/99 **************************************
     C                   MOVEL(P)  DELETE        SCDATA
     C*** END   ADD *** 05/07/99 **************************************
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C                   Z-ADD     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSMSCD        IFNE      *BLANKS
     C     SSMSCD        ANDNE     MSMSCD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSSTS         IFNE      *BLANKS
     C     SSSTS         ANDNE     MSSTS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSSTAF        IFNE      *BLANKS
     C     SSSTAF        ANDNE     MSSTAF
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* UPDC1  - Updates Company auxiliary file.
     C*================================================================
     C     UPDC1         BEGSR
     C*
     C                   MOVEL     Q1USER        C1IUSR
     C                   MOVEL     Q1USER        C1USER
     C                   MOVEL     Q1PGM         C1PGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      DSTIME        C1TIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C1IDT
     C*
     C                   MOVEL     SRDTO         C1DT
     C*
     C                   UPDATE    PC101
     C*
     C                   ENDSR
     C*** BEGIN DEL *** 09/07/99 **************************************
     C*================================================================
     C* WRITEE - Writes record to FAEOP.
     C*================================================================
     C********** WRITEE    BEGSR
     C**********
     C**********           CLEARPEO01
     C**********           EXSR MVEOBF
     C**********
     C**********           MOVELQ1USER    EOIUSR
     C**********           MOVELQ1USER    EOUSER
     C**********           MOVELQ1PGM     EOPGM
     C**********
     C**********           TIME           DSDTTM
     C*
     C**********           MOVE DSTIME    EOTIME
     C*
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     EOIDT
     C*
     C**********           MOVELSRDTO     EODT
     C*
     C**********           WRITEPEO01
     C**********           FEOD FAEOP
     C*
     C**********           ENDSR
     C*** END   DEL *** 09/07/99 **************************************
     C*** BEG DEL *** 12/17/98 ***************************************
     C*================================================================
     C* WRITE1 - Writes record to FAE1P.
     C*================================================================
     C*================================================================
     C* WRITEM - Writes record to NAMBP.
     C*================================================================
     C*** END DEL *** 12/17/98 ***************************************
     C*
     C*================================================================
     C* WRITER - Writes record to FAEOP, FAE1P AND NAMBP.
     C*================================================================
     C     WRITER        BEGSR
     C*
     C     SCDBRR        CHAIN     PFRCDF                             99
     C*** BEGIN ADD *** 09/07/99 **************************************
     C*
     C     ##CO#         CHAIN     PC101                              99
     C*
     C                   Z-ADD     C1EOB#        HDEOB#
     C*
     C     C1EOB#        IFEQ      *HIVAL
     C*
     C                   Z-ADD     1             C1EOB#
     C*
     C                   ELSE
     C*
     C                   ADD       1             C1EOB#
     C*
     C                   ENDIF
     C*
     C                   EXSR      UPDC1
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C* Write record to the EOMB file (FAEOP).
     C*
     C*** BEGIN DEL *** 09/07/99 **************************************
     C**********           EXSR WRITEE
     C*** END   DEL *** 09/07/99 **************************************
     C*
     C* Write record to the EOMB auxiliary file (FAE1P).
     C*
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   MOVE      *BLANKS       KKWRUP
     C                   Z-ADD     *ZEROS        @@RCN
     C                   MOVEL     'M'           @@SYS
     C*
     C     ##PRVB        IFEQ      'Y'
     C*** END   ADD *** 09/07/99 **************************************
     C*** BEG CHG *** 12/16/98 ***************************************
     C*** BEGIN DEL *** 09/07/99 **************************************
     C**********           MOVE *BLANKS   KKWRUP
     C**********           MOVE 'W'       XXFAE1
     C*** END   DEL *** 09/07/99 **************************************
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   MOVE      'W'           XXFAEO
     C                   MOVE      'W'           XXFAEA
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C                   CALL      'AA001R'
     C                   PARM      @@RRN2        @@RRN             9 0          RRN OF CASE
     C                   PARM      C1EOB#        @@EOB#            9 0          EOB #
     C                   PARM                    @@RCN             3 0          RECON #
     C                   PARM      Q1PGM         @@PGM            10            NAME OF PGM CALLING
     C                   PARM                    @@BAMT            9 2          BRS AMOUNT
     C                   PARM                    @@NAMT            9 2          NWS AMOUNT
     C*** BEGIN CHG *** 09/07/99 **************************************
     C**********           PARM           @@BHRF  1        BRS INV H/R FLAG
     C                   PARM      'H'           @@BHRF            1            BRS INV H/R FLAG
     C*** END   CHG *** 09/07/99 **************************************
     C                   PARM                    @@BBLF            1            BRS INV PROC FLAG
     C                   PARM                    @@BINV            9 0          BRS INVOICE #
     C                   PARM                    @@BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    @@NHRF            1            NWS INV H/R FLAG
     C                   PARM                    @@NBLF            1            NWS INV PROC FLAG
     C                   PARM                    @@NINV            9 0          NWS INVOICE #
     C*** BEGIN CHG *** 09/07/99 **************************************
     C**********           PARM EOSYS     @@SYS   1        TYPE OF SERVICE
     C                   PARM                    @@SYS             1            TYPE OF SERVICE
     C*** END   CHG *** 09/07/99 **************************************
     C                   PARM      KKWRUP        @@WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C**********           EXSR WRITE1
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   ELSE
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C* Write record to the Medical Mgmt Bill Transfer file (NAMBP).
     C*
     C*
     C*** BEGIN DEL *** 09/07/99 **************************************
     C**********           MOVE *BLANKS   KKWRUP
     C*** END   DEL *** 09/07/99 **************************************
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   MOVE      'W'           XXFAEO
     C                   MOVE      'W'           XXFAE1
     C*** END   ADD *** 09/07/99 **************************************
     C                   MOVE      'W'           XXNAMB
     C*
     C                   CALL      'AA001R'
     C                   PARM      @@RRN2        @@RRN             9 0          RRN OF CASE
     C                   PARM      C1EOB#        @@EOB#            9 0          EOB #
     C                   PARM                    @@RCN             3 0          RECON #
     C                   PARM      Q1PGM         @@PGM            10            NAME OF PGM CALLING
     C                   PARM                    @@BAMT            9 2          BRS AMOUNT
     C                   PARM                    @@NAMT            9 2          NWS AMOUNT
     C                   PARM      'H'           @@BHRF            1            BRS INV H/R FLAG
     C                   PARM                    @@BBLF            1            BRS INV PROC FLAG
     C                   PARM                    @@BINV            9 0          BRS INVOICE #
     C                   PARM                    @@BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    @@NHRF            1            NWS INV H/R FLAG
     C                   PARM                    @@NBLF            1            NWS INV PROC FLAG
     C                   PARM                    @@NINV            9 0          NWS INVOICE #
     C*** BEGIN CHG *** 09/07/99 **************************************
     C**********           PARM EOSYS     @@SYS   1        TYPE OF SERVICE
     C                   PARM                    @@SYS             1            TYPE OF SERVICE
     C*** END   CHG *** 09/07/99 **************************************
     C                   PARM      KKWRUP        @@WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C**********           EXSR WRITEM
     C*** END CHG *** 12/16/98 ***************************************
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C                   ENDSR
** Column headings
Opt    Date        Date     Svc    Seq#    Description     Sts  Code
Opt Svc      Seq#  Description         Date        Date    Sts  Code
** Column headings 2 08/04/08
Opt    Date        Date     Svc    Seq#    Description     Sts  Code      Amt
Opt Svc      Seq#  Description         Date        Date    Sts  Code      Amt
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
11717=T/M Activity          YYN
19090=Outcome/Savings       YYN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               YYY
214F14=DARTS                YYY
217F17=Subset               YYY
3
