     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*=========================================================================
     H*       StoneRiver is a trademark of StoneRiver, Inc.
     H*=========================================================================
     H*
     H*-------------------------------------------------------------------------
     H/TITLE EXTNOT - WC NOTES Extract / NAIC Code 28711
     H*-------------------------------------------------------------------------
     H*   Description:
     H*
     H*=========================================================================
     H*  Called From:  EXTNOTX CL program
     H*  Calls  to:    DA003R
     H*
     H*  Created:   1/06/10    C:58   PHI Providence Holding    Project: 135WNX
     H*
     H*=========================================================================
     H*
     H*  Revision: 06/07/10  C:191  Project: 135WOK - UDS extract
     H*                             Include 63362 and 72096 in extract
     H*                             Change process to include NAIC code in *entr
     H*
     H*=========================================================================
     F* Indicators
     F*
     F*      41  Test numeric
     F*
     F*   46  LOKUP
     F*
     F*   51  Input record ID SDCMP
     F*   53  CHAIN WMCML1
     F*   55  CHAIN, READE SMARL
     F*   57  SETLL READE SXCLL
     F*
     F*        60  General chain/read
     F*
     F*   88  UDS DATA in place to be written
     F*   89  SDCMP data has been written
     F*=================================================================-=======
     F*             FILES
     F*=================================================================-=======
     F*
     F*-----------------------------------------------------------------=-------
     F*      INPUT
     F*-----------------------------------------------------------------=-------
     F*  StoneRiver Note File  Active Notes
     FSDCMLA    IPE  E           K DISK
     F*
     F*  StoneRiver Note Additional Records File
     FSMARL     IF   E           K DISK
     F*
     F*  PHI Auxiliary Fund File    contains NAIC code
     FWDF2L     IF   E           K DISK
     F*
     F*  Claim Master WMCML1        contains JRST code
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     F*
     F*  DARTS CLOB log -- from SMCLP
     FSXCLL     IF   E           K DISK
     F*
     F*  NAIC UDS NOTES Extract File   'F' Record Format
     FUDSFP     O    E             DISK
     F*
     F*    AUDIT REPORT AND FILE TOTALS
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     F*

     D* ------------------------------------------------------------------------
     D*        A r r a y s
     D* ------------------------------------------------------------------------
     D*     Correction file arrays
     D FI              S              7  0 DIM(20)                              Nbr input
     D FU              S              7  0 DIM(20)                              Nbr updated
     D FW              S              7  0 DIM(20)                              Nbr written
     D FE              S              7  0 DIM(20)                              Error counts
     D FN              S             15    DIM(20) CTDATA PERRCD(1)             File names
     D*
     D*       Insolvent Group Indicators
     D IG              S              1    DIM(999)                             X=insolvent
     D*
     D*       NAIC UDS File Text Data
     D*
     D C1              S            250    DIM(128)                             CLOB - 32K Segment
     D*
     D*=========================================================================
     D*      I N P U T   D A T A
     D*=========================================================================
     D CMCOMM          DS                  INZ
     D  COMM1                  1    256
     D  COMM2                257    275
     D*
     D                 DS                  INZ
     D  CMKEY                  1     40
     D*   Define Key Fields
     D  KKCO#                  1      3  0
     D  KKFND                  4      6  0
     D  KKFYR                  7      9  0
     D  KKCASE                10     16  0
     D*
     D*   Define Indexes
     D*     Make SDCMP Claim Key/fund number be IX G
     D  G                      4      6  0
     D*     Define 13 digit case ID
     D  CAS13                  4     16
     D*
     D*     Make WDF2P fund number be IX I
     D*       To load array with insolvent fund numbers
     D                 DS                  INZ
     D  F2FND                  1      3  0
     D  I                      1      3  0
     D*
     D*=========================================================================
     D*      O U T P U T   D A T A        Assembly Area
     D*=========================================================================
     D*     Assemble text for UDS record.
     D                 DS                  INZ
     D  UD                     1   1000
     D                                     DIM(1000)                            Text, NAIC UDS recor
     D  SFTXT1                 1    256
     D  SFTXT2               257    275
     D  SFTXT3               276    500
     D  SFTXT4               501    725
     D  SFTXT5               726    950
     D  SFTXT6               951   1000
     D*     Null CLOB indication
     D  TXT1                   1      1
     D*
     D*     Run Date and Time.
     D                 DS                  INZ
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*  Copy books.
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I* ------------------------------------------------------------------------
     I*        D A T A
     I* ------------------------------------------------------------------------
     I* DARTS PRIMARY   SDCMP
     IPCM01         51
     I                                          CMKEY         L5
     I                                          CMSEQ#        L3

     C*
     C*=========================================================================
     C*      M  A I  N    L  O  G  I  C
     C*=========================================================================
     C*   Got Input DARTS SDCMLA
     C     *IN51         IFEQ      *ON
     C                   ADD       1             FI(1)
     C                   MOVE      *OFF          *IN89                           SDCMP indicator
     C*
     C*   Skip all these DARTS
     C     KKCO#         IFNE      777
     C     CMPNAM        ORNE      'W0070R'                                      Not a claim
     C     KKFND         OREQ      777                                           Test groups
     C     KKFND         OREQ      998                                            test
     C     KKFND         OREQ      999                                            test
     C     KKFND         OREQ      0                                              test
     C                   ADD       1             FE(2)
     C*
     C                   ELSE                                                    X FND = 777 ...
     C                   ADD       1             FI(2)                              Select
     C*
     C*   Record selection: if group in key is 'X' in array IG.
     C     IG(G)         IFEQ      'X'
     C                   ADD       1             FI(3)                              Select
     C*
     C*   Get Claim Master
     C     *INL5         IFEQ      *ON                                           1st this claim
     C     CLMKEY        CHAIN     PCM02                              53
     C*
     C*  Claim Master not found - ERROR - SKIP THIS CLAIM
     C     *IN53         IFEQ      *ON
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF                                                  e-INOF = ON
     C                   EXCEPT    DETL
     C                   ADD       1             FE(6)
     C*
     C                   ELSE                                                   X IN53
     C*  Claim Master found - PROCESS
     C                   ADD       1             FE(5)
     C*
     C*  Build first NAIC UDS record
     C                   EXSR      BLDUDS
     C                   ENDIF                                                  e-IN53 = OFF
     C                   ENDIF                                                  e-INL5 = ON
     C*
     C*  Claim Master found - PROCESS
     C     *IN53         IFEQ      *OFF
     C     *INL3         IFEQ      *ON                                           1st this note
     C                   EXSR      LODNOT
     C                   ENDIF                                                  e-INL3 = ON
     C*
     C*  Get additional records SMARL
     C     SMKEY         CHAIN     PAR01                              55
     C*
     C*  No SMARP, get data from CLOB for this note
     C     *IN55         IFEQ      *ON
     C                   ADD       1             FE(8)
     C                   Z-ADD     0             @ASEQ#                          Set to assign
     C                   EXSR      GETLOB
     C                   ELSE
     C*
     C*   Get additional DARTS SMARP
     C                   EXSR      GETADD
     C                   ENDIF                                                  e-IN55 = ON
     C*
     C                   ENDIF                                                  e-IN53 = OFF
     C                   ENDIF                                                  e-IG,G = 'X'
     C                   ENDIF                                                  e-CO = 777 ...
     C     *IN88         IFEQ      *ON
     C                   EXSR      WRTUDS
     C                   ENDIF                                                  e-IN88 = ON
     C                   ENDIF                                                  e-IN51 = ON
     C*
     C     *IN88         IFEQ      *ON
     C                   EXSR      WRTUDS
     C                   ENDIF                                                  e-IN88 = ON
     C*=========================================================================
     C*     Lx - Level Breaks
     C*=========================================================================
     CLR                 DO
     CL0                 EXSR      ENDER
     CLR                 ENDDO                                                  e LR DO
     CL3                 DO
     CL0   *IN88         IFEQ      *ON
     CL0                 EXSR      WRTUDS
     CL0                 ENDIF                                                  e 88 ON
     CL3                 ENDDO                                                  e LR DO
     C*=
     C*=========================================================================
     C*        Build NAIC UDS Record
     C*=========================================================================
     C     BLDUDS        BEGSR
     C                   CLEAR                   PSF01
     C                   MOVEL     'F'           SFRECT                          Record type
     C*** BEGIN CHG ****** 06/07/10 ************************************
     C***********          Z-ADD28711     SFNAIC            NAIC number
     C                   MOVEL     QQNAIC        SFNAIC                          NAIC number
     C*** END   CHG ****** 06/07/10 ************************************
     C                   MOVEL     CMJRST        SFLST                           Location state
     C                   Z-ADD     11            SFLCD                           Location code
     C                   MOVEL(P)  CAS13         SFICLM                          Insolvent claim #
     C*
     C                   MOVEL(P)  CAS13         SF3CLM                          TPA claim #
     C                   ENDSR
     C*
     C*=
     C*=========================================================================
     C*        LOAD THIS NOTE DATA
     C*=========================================================================
     C     LODNOT        BEGSR
     C*
     C                   Z-ADD     CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SFENDT                          Note date
     C*
     C                   Z-ADD     CMSEQ#        SFNTID                          Note ID #
     C                   Z-ADD     1             SFNLSQ                          Line Seq#
     C                   MOVE      *ON           *IN88                           Write indicator
     C*
     C*  Load CMCOMM text into 1-275
     C     *IN89         IFEQ      *OFF
     C                   MOVEA     COMM1         UD(1)                           Text 1-256
     C                   MOVEA     COMM2         UD(257)                         Text 257-275
     C                   Z-ADD     276           U                 5 0           Set for 1st SMARP
     C                   ENDIF                                                   IN89 OFF
     C                   ENDSR
     C*
     C*=========================================================================
     C*        GET SMARP TEXT
     C*=========================================================================
     C     GETADD        BEGSR
     C     *IN55         DOWEQ     *OFF
     C                   ADD       1             FI(8)
     C*
     C                   MOVEA(P)  ARCM75        UD(U)                          Next 75
     C                   ADD       75            U
     C                   MOVE      *ON           *IN88                           Write indicator
     C*
     C*    Write this record if no room for next 75
     C     U             IFGT      925
     C                   EXSR      WRTUDS
     C                   ENDIF                                                  e-U > 925
     C*
     C     SMKEY         READE     PAR01                                  55
     C                   ENDDO                                                  e-IN55 DOW=OFF
     C                   ENDSR
     C*
     C*=========================================================================
     C*     Write NAIC UDS record
     C*=========================================================================
     C     WRTUDS        BEGSR
     C*
     C     *IN88         IFEQ      *ON
     C                   WRITE     PSF01
     C                   ADD       1             FW(15)
     C                   CLEAR                   UD
     C                   Z-ADD     1             U                              Reset for Next recor
     C*
     C                   ADD       1             SFNLSQ                         Next Line Seq#
     C                   MOVE      *OFF          *IN88                           Write indicator
     C                   ENDIF                                                  e-88 ON
     C*
     C                   ENDSR
     C*
     C*=========================================================================
     C*     Get Character Large Object Text  FOR CURRENT NOTE
     C*=========================================================================
     C     GETLOB        BEGSR
     C                   ADD       1             FI(10)
     C                   Z-ADD     501           U                 5 0           Set for 2 CLOBs
     C*
     C*     Determine existence of CLOB
     C     CLBKEY        SETLL     PXL01
     C     CLBKY2        READE     PXL01                                  57
     C*
     C     *IN57         IFEQ      *OFF
     C                   ADD       1             FI(11)
     C*
     C                   DO        50
     C*
     C                   Z-ADD     1             @POS             15 0
     C                   CALL      'DA003R'      CLOBPL
     C*
     C                   Z-ADD     1             V                 7 0
     C*
     C*  ---- FIRST WRITE THE RECORD WITH SDCMP + 2 C1 ELEMENTS ----------------
     C     *IN89         IFEQ      *OFF
     C*   Move 2 segments to file, and write record
     C                   DO        2
     C*
     C*   Test for and blank out initial quote
     C                   MOVEA     C1(V)         TESTQ             1            Next 250
     C     TESTQ         IFEQ      XXQUOT
     C                   MOVEL     ' '           C1(V)                          Blank out quote
     C                   ENDIF
     C*
     C*   If not blank, move in text
     C     C1(V)         IFNE      *BLANK
     C                   MOVEA(P)  C1(V)         UD(U)                          Next 250
     C                   ADD       250           U                              C1,V ,> Blank
     C                   ENDIF
     C*
     C                   ADD       1             V
     C                   ENDDO                                                  e-DO 2
     C*
     C                   EXSR      WRTUDS
     C                   MOVE      *ON           *IN89                           Write indicator
     C                   ENDIF
     C*  ___________________________________________________________________
     C*  ---- NOW PROCESS REMAINING C1 ELEMENTS 4 AT A  TIME ---------------
     C                   DO        31
     C*
     C*   Move 4 segments to file, and write record
     C                   DO        4
     C*
     C*   Test for and blank out initial quote
     C                   MOVEA     C1(V)         TESTQ             1            Next 250
     C     TESTQ         IFEQ      XXQUOT
     C                   MOVEL     ' '           C1(V)                          Blank out quote
     C                   ENDIF
     C*
     C*   If not blank, move in text
     C     C1(V)         IFNE      *BLANK
     C                   MOVE      *ON           *IN88                          Write indicator
     C                   MOVEA(P)  C1(V)         UD(U)                          Next 250
     C                   ADD       250           U                              C1,V ,> Blank
     C                   ENDIF
     C*
     C                   ADD       1             V
     C*
     C                   ENDDO                                                  e-DO 4
     C*
     C*   Write if not  blank
     C     *IN88         IFEQ      *ON
     C                   EXSR      WRTUDS
     C                   ELSE                                                   X-IN88 = ON
     C                   LEAVE
     C                   ENDIF                                                  e-IN88 = ON
     C                   ENDDO                                                  e-DO 32
     C*
     C     *IN88         IFEQ      *OFF                                          & SUSPENDERS
     C                   LEAVE
     C                   ENDIF                                                  e-IN88 = ON
     C*
     C                   ADD       32000         @POS
     C                   ENDDO                                                  e-DO 50
     C                   ENDIF                                                  e-IN57 = OFF
     C*
     C                   ENDSR
     C*
     C*=========================================================================
     C*      ENDING  SUBROUTINE
     C*=========================================================================
     C     ENDER         BEGSR
     C*
     C*     Print File Totals
     C                   EXCEPT    TOTF
     C                   DO        20            F                 3 0
     C     FI(F)         IFNE      *ZERO
     C     FU(F)         ORNE      *ZERO
     C     FW(F)         ORNE      *ZERO
     C     FE(F)         ORNE      *ZERO
     C                   EXCEPT    DETF
     C                   ENDIF                                                  Fx, <> 0
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C*=========================================================================
     C*        INITIALIZATION
     C*=========================================================================
     C     *INZSR        BEGSR
     C*
     C*** BEGIN ADD ****** 06/07/10 ************************************
     C     *ENTRY        PLIST
     C                   PARM                    QQNAIC           10
     C*
     C                   MOVEL     'Selected'    FN(19)
     C                   CAT       QQNAIC:1      FN(19)
     C*** END   ADD ****** 06/07/10 ************************************
     C*-------------------------------------------------------------------------
     C*          DATE PARAMETERS
     C*-------------------------------------------------------------------------
     C*-------------------------------------------------------------------------
     C*          DATE PARAMETERS
     C*-------------------------------------------------------------------------
     C*
     C     CLOBPL        PLIST
     C                   PARM      CMKEY         @KEY             40
     C                   PARM      CMSEQ#        @SEQ#             5 0
     C                   PARM                    @ASEQ#            5 0
     C                   PARM                    @POS             15 0
     C                   PARM                    @LEN             15 0
     C                   PARM                    C1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTYMD         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YMD'        SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTYYMM        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*-------------------------------------------------------------------------
     C*          KEY LISTS
     C*-------------------------------------------------------------------------
     C*   Additional DARTS - SMARL
     C     SMKEY         KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     C*
     C*   Claim CLOB log SXCLL
     C     CLBKEY        KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     C                   KFLD                    KKASEQ
     C*
     C*   Claim CLOB log SXCLL
     C     CLBKY2        KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     C*
     C*   ClaimMAster WMCML1
     C     CLMKEY        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*
     C*-------------------------------------------------------------------------
     C     *LIKE         DEFINE    CMSEQ#        XXSEQ#
     C     *LIKE         DEFINE    CMKEY         SAVKEY
     C     *LIKE         DEFINE    ARASQ#        XXASQ#
     C     *LIKE         DEFINE    CMDT          XXDT
     C     *LIKE         DEFINE    CLASEQ        KKASEQ
     C*
     C*     Initialize Constants
     C                   Z-ADD     32000         @LEN             15 0
     C                   Z-ADD     *HIVAL        KKASEQ
     C*
     C                   MOVEL     ''''          XXQUOT            1
     C*
     C*     Calculate julian system date -- CYY MM DD
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C*     Load Array IG with Insolvent Fund flags
     C                   CLEAR                   IG
     C                   READ      PF201                                  60
     C     *IN60         DOWEQ     *OFF
     C                   ADD       1             FI(18)
     C*** BEGIN ADD ****** 06/07/10 ************************************
     C     F2NAIC        IFEQ      QQNAIC
     C*** END   ADD ****** 06/07/10 ************************************
     C                   ADD       1             FI(19)
     C                   MOVE      'X'           IG(I)
     C                   ENDIF                                                  e- NAIC = 28711
     C                   READ      PF201                                  60
     C                   ENDDO                                                  e- IN55 DOWEQ OFF
     C*
     C*-------------------------------------------------------------------------
     C*     Skip group 479 per J Motes
     C                   MOVE      ' '           IG(479)
     C*-------------------------------------------------------------------------
     C*
     C*     Print DUMMY
     C   LR
     CANNLR              EXCEPT    DETL
     C*
     C*     Print report headings.
     C                   EXCEPT    HEAD
     C*
     C                   ENDSR
     C*=========================================================================
     C*     PRINTED OUTPUT
     C*=========================================================================
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME               8 '  :  :  '
     O                                           44 'PHI - NAIC UDS Extract'
     O                                           66 ' Notes                '
     O                                           88 '   Project:   135WNX  '
     O                       UDATE         Y    128
     O          E            HEAD           3
     O                       Q1PGM               10
     O                                           44 'Notes with No Claim Fo'
     O                                           66 'und                   '
     O                       PAGE          Z    130
     O                                          125 'PAGE'
     O          E            HEAD           3
     O                                           22 'Fnd  YR    Case       '
     O                                           44 '                      '
     O*
     O          E            DETL        1  1
     O                       KKFND         Z     03
     O                       KKFYR         Z     07
     O                       KKCASE        Z     15
     O                                           37 '** CLAIM not Found **'
     O*
     O          E            TOTF        2  2
     O                                           30 '***** TOTALS ***** '
     O*
     O*
     O* =============
     O          E            TOTF           2
     O                                           22 '*** RECORD  COUNT SUMM'
     O                                           44 'MARY ***              '
     O          E            TOTF        1  2
     O                                           22 'File/Description    In'
     O                                           44 'put read    Rec Update'
     O                                           66 'd    Written out      '
     O                                           88 '   Errors             '
     O          E            DETF           1
     O                       FN(F)               15
     O                       FI(F)         Z     30
     O                       FU(F)         Z     45
     O                       FW(F)         Z     60
     O                       FE(F)         Z     75
     O*
** TABLE      v       FN - File names   30 @ 15
SDCML          <  1
 Claim DARTS   <  2
 Insolvent clm <  3
xxxxxxxxx4     <  4
WMCML1         <  5
 SKIP-no claim <  6
xxxxxxxxx7     <  7
SMARL          <  8
Archived CLOBS <  9
Get CLOB       < 10
SXCLL          < 11
xxxxxxxx12       12
xxxxxxxx13       13
xxxxxxxx14       14
UDSFP            15
xxxxxxxx16       16
xxxxxxxx17       17
FWDF2L           18
 Selected 28711< 19
xxxxxxxx20       20
