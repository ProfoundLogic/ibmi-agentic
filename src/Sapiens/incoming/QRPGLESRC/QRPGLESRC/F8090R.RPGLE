     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H* ========================================================================
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H* ========================================================================
     H*
     H* ------------------------------------------------------------------------
     H/TITLE F8090R - PA Automobile bill adjudication - SIS Import
     H* ------------------------------------------------------------------------
     H* Created:
     H*
     H* 06/09/20 C:295 Project: SY420                                 *
     H*                -Mask SSN                                      *
     H*                                                               *
     H* 06/22/11 C: 58 Project 114MYS
     H*                SIS Claim Import program for SEPTA Auto bills
     H*
     H* ------------------------------------------------------------------------
     H*
     H* Called by: F8090C1
     H* Calls to : None
     H*
     H* ------------------------------------------------------------------------
     H* Revisions:
     H*
     H*    09/20/13  C:062  Support: MC114 83549
     H*                     Use correct state logical
     H*
     H*    01/30/12  C:062  Project: MC114 79017
     H*                     Correct claim number assignment.           PP
     H*
     H*    12/14/11  C:251  Project: 114MBB
     H*                     Add processing as per company number in FASPP
     H*
     H* ------------------------------------------------------------------------
     H*     INDICATOR USAGE
     H* ------------------------------------------------------------------------
     H*      51   FASPP record ID
     H*      66   Claim found, already in file
     H*
     H*      88   Fatal error claim skipped
     H*
     H*      30   General CHAIN
     H* ------------------------------------------------------------------------
     H*
     F*================================================================
     F*      F  I  L  E  S
     F*================================================================
     F*   SIS Claims Import File
     FFASPP     IF   E             DISK
     F*
     F*   MC Claim File
     FFMCMLD    IF   E           K DISK
     F*
     F*   MC Claim File
     FFMCML1    UF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     F*
     F*   MC Claim History File
     FFMCHL1    IF   E           K DISK
     F*
     F*   MC Employer File
     FFMEML     IF   E           K DISK
     F*
     F*   Users File
     FSMUPL     IF   E           K DISK
     F*
     F*   Company file
     FSMCOL     IF   E           K DISK
     F*
     F*  STATE MASTER
     F*** Begin Chg *** 09/20/13 ***************************************
     F**********STSTL3    IF   E           K DISK
     FSTSTL     IF   E           K DISK
     F*** End   Chg *** 09/20/13 ***************************************
     F*** Begin Add *** 01/30/12 **************************************
     F*
     F*  Company Auxiliary File
     FFMC1L     UF   E           K DISK
     F*** End   Add *** 01/30/12 **************************************
     F*
     F*   Statistics / Errors
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     F*
     F*
      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
     D*=========================================================================
     D*        A r r a y s
     D*=========================================================================
     D*     Conversion file arrays
     D FI              S              7  0 DIM(30)                              Nbr input
     D FW              S              7  0 DIM(30)                              Nbr written
     D FE              S              7  0 DIM(30)                              Error counts
     D FN              S             10    DIM(30) CTDATA PERRCD(1)             File names
     D*
     D*       Error message text
     D ERC             S              7  0 DIM(20)                              Errors by type
     D ERR             S             30    DIM(20) CTDATA PERRCD(1)             Error Messages
     D* ------------------------------------------------------------------------
     D*    Date and Time
     D* ------------------------------------------------------------------------
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*    Zip code to numeric
     D                 DS
     D  ZIPX                   1      9
     D  ZIP4X                  6      9
     D*
     D*    Dates YYYYMMDD to numeric
     D                 DS
     D  SPACDT                 1      8
     D  DATACN                 1      8  0
     D  SPCLDT                 9     16
     D  DATCLN                 9     16  0
     D  SPDEDT                17     24
     D  DATDEN                17     24  0
     D  SPDTOB                25     32
     D  DATDBN                25     32  0
     D*
     D* ------------------------------------------------------------------------
     D*    Field definitions for output call parameters
     D* ------------------------------------------------------------------------
     D CMDS          E DS                  EXTNAME(WMCMP)
     D CDDS          E DS                  EXTNAME(WMCDP)
     D CHDS          E DS                  EXTNAME(WMCHP)
     D*
     D* ------------------------------------------------------------------------
     D*    Program Status
     D* ------------------------------------------------------------------------
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D* ------------------------------------------------------------------------
     D*    CONSTANTS
     D* ------------------------------------------------------------------------
     D*        ========  Report Title lines  ======
     D TITL1           C                   CONST('Claim Master File -
     D                                     Conversion  2010  ')
     D TITL2           C                   CONST('SIS - SEPTA Auto B-
     D                                     ills Data Load    ')

     I*__________________________________________________________________
     I*
     I*=========================================================================
     I*      D A T A   S T R U C T U R E S
     I*=========================================================================
     I*
     I*    FMCMLD MC Claims
     IPCM01
     I              CMOSCL                      ZZOSCL
     IPCM02
     I              CMOSCL                      XXOSCL
     I*

     C*=========================================================================
     C*      C  A  L  C  U  L  A  T  I  O  N  S
     C*=========================================================================
     C*     Got Input Record.
     C                   READ      PSP01                                  51
     C*
     C     *IN51         DOWEQ     *OFF
     C*
     C                   ADD       1             INCNT             7 0
     C                   ADD       1             FI(1)
     C*
     C*     See if already in file.
     C                   CLEAR                   CMDS
     C*
     C                   EXSR      CHKCLM
     C*
     C*     Edit data.
     C*
     C                   EXSR      EDTCLM
     C*
     C     *IN88         IFEQ      *ON
     C                   ADD       1             FE(7)                           Skipped
     C*
     C                   ELSE                                                   X IN88
     C*
     C                   EXSR      LODCLM
     C*
     C                   EXSR      MCCLMW
     C                   ADD       1             FW(3)                           Claim master
     C*
     C                   CLEAR                   CDDS
     C*
     C                   EXSR      LODCLT
     C                   EXSR      MCCLMD
     C*** Write TO FMCHP
     C     CMKY1         CHAIN(N)  PCM02                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   EXSR      MCCLMH
     C                   ELSE
     C     CMKY1         SETGT     PCH01                              60
     C     CMKY1         READPE    PCH01                                  60
     C*
     C     *IN60         IFEQ      *ON
     C                   EXSR      MCCLMH
     C                   ELSE
     C     SPSTS         IFNE      CHOCST
     C     SPSTS         IFEQ      'O'
     C     CHOCST        ANDEQ     'C'
     C                   MOVE      'R'           SPSTS
     C                   Z-ADD     XXDT          XXCLDT
     C                   ENDIF                                                  e- SPSTS <> CHOCST
     C                   EXSR      MCCLMH
     C                   ENDIF                                                  e- SPSTS <> CHOCST
     C*
     C                   ENDIF                                                  e- IN60 OFF
     C*
     C                   ENDIF                                                  e- IN60 OFF
     C*
     C                   ENDIF                                                  e- IN88 OFF
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C     CMKY1         CHAIN     PCM02                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EMEMP#        CMEMP#
     C                   Z-ADD     EMDIV         CMDIV
     C                   UPDATE    PCM02
     C                   ENDIF                                                  e- IN60 OFF
     C                   ENDIF                                                  e- IN60 OFF
     C*
     C                   READ      PSP01                                  51
     C*
     C                   ENDDO                                                  e- IN51 OFF
     C*=========================================================================
     C*
     C* = == == == == == == == == == == == == == == == == == == == == == == == =
     C*     LR - End of Program
     C                   EXSR      ENDER
     C                   SETON                                        LR
     C* = == == == == == == == == == == == == == == == == == == == == == == == =
     C*
     C*=========================================================================
     C*      S  U  B  R  O  U  T  I  N  E  S
     C*=========================================================================
     C*     Edit input data.
     C*=========================================================================
     C     EDTCLM        BEGSR
     C*
     C*  Check for FATAL ERRORS -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
     C                   MOVE      *OFF          *IN88                          Fatal
     C*
     C*     State Claim #
     C     SPCLM#        IFEQ      *BLANK                                       not numeric
     C                   MOVE      *ON           *IN88                          Fatal
     C                   Z-ADD     1             E                 5 0          error # 1
     C                   EXSR      ERRPRT
     C                   ADD       1             ERC(E)                         Count by type
     C                   ENDIF                                                  e- CLM# = blank
     C*
     C*     Social Security Number
     C                   MOVE      SPSS#         XXZERO            1
     C                   MOVEL     SPSS#         XXEIGT            8
     C     SPSS#         IFEQ      *BLANK                                       not numeric
     C     XXEIGT        OREQ      '00000000'
     C     XXZERO        ANDEQ     '0'
     C                   MOVE      *ON           *IN88                          Fatal
     C                   Z-ADD     2             E                              error # 2
     C                   EXSR      ERRPRT
     C                   ADD       1             ERC(E)                         Count by type
     C                   ENDIF                                                  e- CLM# = blank
     C*
     C*     Accident Date
     C                   Z-ADD     0             XXACDT
     C                   MOVEL     '00000000'    ACDT8             8 0           8 MDYY
     C                   MOVE      *OFF          *IN41
     C                   MOVE      '0'           SRERR
     C                   TESTN                   SPACDT               41        numeric ?
     C     *IN41         IFEQ      *OFF                                         not numeric
     C                   MOVE      '1'           SRERR
     C                   ELSE                                                     X- IN41 OFF
     C                   Z-ADD     DATACN        SRDTI                          is numeric
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   ENDIF                                                  e- IN41 OFF
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN88                          Fatal
     C                   MOVEL(P)  SPACDT        ERRDTA           20            Date
     C                   Z-ADD     3             E                              error # 3
     C                   EXSR      ERRPRT
     C                   ADD       1             ERC(E)                         Count by type
     C                   ELSE                                                   >ACDT is numeric
     C*
     C                   MOVEL     SRDTO         XXACDT                         7 CYMD
     C                   Z-ADD     XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFNE      '1'
     C                   MOVEL     SRDTO         ACDT8                           8 MDYY
     C                   ENDIF                                                  e- ERR <> 1
     C                   ENDIF                                                  e- ERR = 1
     C*
     C*     Claimant name
     C     SPFSN         IFEQ      *BLANK                                       null
     C     SPLSN         ANDEQ     *BLANK                                        also
     C                   MOVE      *ON           *IN88                          Fatal
     C                   Z-ADD     4             E                              error # 4
     C                   EXSR      ERRPRT
     C                   ADD       1             ERC(E)                         Count by type
     C                   ENDIF                                                  e- CLM# = blank
     C*
     C*     Lost time flag
     C     SPLTFL        IFNE      'M'                                          not numeric
     C     SPLTFL        ANDNE     'I'                                          not numeric
     C     SPLTFL        ANDNE     'R'                                          not numeric
     C                   MOVE      *ON           *IN88                          Fatal
     C                   MOVEL(P)  SPLTFL        ERRDTA
     C                   Z-ADD     5             E                              error # 11
     C                   EXSR      ERRPRT
     C                   ADD       1             ERC(E)                         Count by type
     C                   ENDIF                                                  e- CLM# = blank
     C*
     C*     Status flag
     C     SPSTS         IFNE      'O'                                          not numeric
     C     SPSTS         ANDNE     'C'                                          not numeric
     C                   MOVE      *ON           *IN88                          Fatal
     C                   MOVEL(P)  SPLTFL        ERRDTA
     C                   Z-ADD     6             E                              error # 6
     C                   EXSR      ERRPRT
     C                   ADD       1             ERC(E)                         Count by type
     C                   ENDIF                                                  e- CLM# = blank
     C*
     C*     Jurisdiction state
     C     SPJRST        IFNE      *BLANK                                       not blank
     C                   MOVEL(P)  SPJRST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C                   ENDIF                                                  e- JRST <> blank
     C     *IN60         IFEQ      *ON
     C     SPJRST        OREQ      *BLANK                                       not blank
     C                   MOVE      *ON           *IN88                          Fatal
     C                   MOVEL(P)  SPJRST        ERRDTA
     C                   Z-ADD     7             E                              error # 7
     C                   EXSR      ERRPRT
     C                   ADD       1             ERC(E)                         Count by type
     C                   ENDIF                                                  e- JRST <> blank
     C*
     C*** Beg Add *** 12/14/11 *************************************************
     C* Company Number (SEPTA = 43 , AUTO-CAT = 46)
     C*
     C     SPCO#         IFNE      '43'
     C     SPCO#         ANDNE     '46'
     C                   MOVE      *ON           *IN88                          Fatal
     C                   MOVEL(P)  SPCO#         ERRDTA
     C                   Z-ADD     8             E                 5 0          error # 8
     C                   EXSR      ERRPRT
     C                   ADD       1             ERC(E)                         Count by type
     C                   ENDIF                                                  e- SPCO# <> '43','46
     C*
     C*** End Add *** 12/14/11 *************************************************
     C*  Other Errors -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
     C*
     C*     Closed date
     C                   Z-ADD     0             XXCLDT
     C     SPCLDT        IFNE      '00000000'
     C*
     C*
     C                   MOVEL     '00000000'    CLDT8             8 0           8 MDYY
     C                   MOVE      *OFF          *IN41
     C                   MOVE      '0'           SRERR
     C                   TESTN                   SPCLDT               41        numeric ?
     C     *IN41         IFEQ      *OFF                                         not numeric
     C                   MOVEL     '00000000'    SPCLDT                          8 MDYY
     C                   ELSE                                                     X- IN41 OFF
     C*
     C                   Z-ADD     DATCLN        SRDTI                          is numeric
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   ENDIF                                                  e- IN41 OFF
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  SPCLDT        ERRDTA                         Date
     C                   Z-ADD     18            E                              error # 3
     C                   ADD       1             ERC(E)                         Count by type
     C*
     C                   ELSE
     C                   MOVEL     SRDTO         XXCLDT                         7 CYMD
     C                   Z-ADD     XXCLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFNE      '1'
     C                   MOVEL     SRDTO         CLDT8                           8 MDYY
     C                   ENDIF                                                  e- ERR <> 1
     C                   ENDIF                                                  e- ERR = 1
     C                   ENDIF                                                  SPCLDT = 0
     C*
     C*     Date of death
     C                   Z-ADD     0             XXDEDT
     C     SPDEDT        IFNE      '00000000'
     C*
     C                   MOVEL     '00000000'    DEDT8             8 0           8 MDYY
     C                   MOVE      *OFF          *IN41
     C                   TESTN                   SPDEDT               41        numeric ?
     C     *IN41         IFEQ      *OFF                                         not numeric
     C                   MOVEL     '00000000'    SPDEDT            8             8 MDYY
     C                   ELSE                                                     X- IN41 OFF
     C*
     C                   Z-ADD     DATDEN        SRDTI                          is numeric
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   ENDIF                                                  e- IN41 OFF
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  SPDEDT        ERRDTA                         Date
     C                   Z-ADD     19            E                              error # 3
     C                   ADD       1             FE(15)
     C                   ADD       1             ERC(E)                         Count by type
     C*
     C                   ELSE
     C                   MOVEL     SRDTO         XXDEDT                         7 CYMD
     C                   Z-ADD     XXDEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFNE      '1'
     C                   MOVEL     SRDTO         DEDT8                           8 MDYY
     C                   ENDIF                                                  e- ERR <> 1
     C                   ENDIF                                                  e- ERR = 1
     C                   ENDIF                                                  SPDEDT = 0
     C*
     C*     Date of Birth
     C                   Z-ADD     0             XXDTOB
     C     SPDTOB        IFNE      '00000000'
     C*
     C                   MOVEL     '00000000'    DTOB8             8 0           8 MDYY
     C                   MOVE      *OFF          *IN41
     C                   TESTN                   SPDTOB               41        numeric ?
     C     *IN41         IFEQ      *OFF                                         not numeric
     C                   MOVEL     '00000000'    SPDTOB            8             8 MDYY
     C                   ELSE                                                     X- IN41 OFF
     C*
     C                   Z-ADD     DATDBN        SRDTI                          is numeric
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   ENDIF                                                  e- IN41 OFF
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVEL(P)  SPDTOB        ERRDTA                         Date
     C                   Z-ADD     19            E                              error # 3
     C                   ADD       1             FE(15)
     C                   ADD       1             ERC(E)                         Count by type
     C*
     C                   ELSE
     C                   MOVEL     SRDTO         XXDTOB                         7 CYMD
     C                   Z-ADD     XXDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFNE      '1'
     C                   MOVEL     SRDTO         DTOB8                           8 MDYY
     C                   ENDIF                                                  e- ERR <> 1
     C                   ENDIF                                                  e- ERR = 1
     C                   ENDIF                                                  SPDEDT = 0
     C*
     C*
     C                   ENDSR
     C*
     C*=========================================================================
     C*     Check if Claim already in file
     C*=========================================================================
     C     CHKCLM        BEGSR
     C*
     C                   MOVE      *OFF          *IN68
     C     CLMKY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN68
     C*
     C*    Save pertinent data
     C                   MOVEL     SPCLDT        SVCLDT            8
     C                   MOVE      SPSTS         SVSTS             1
     C*
     C                   ENDIF                                                  e- IN60 = OFF
     C*
     C                   ENDSR
     C*
     C*
     C*================================================================
     C*       - Load Claim Detail Data
     C*================================================================
     C     LODCLM        BEGSR
     C*
     C*    Dates
     C*
     C                   CALL      'FA007R'      COMPL
     C*
     C*** Finding next available claim
     C*
     C                   MOVE      XXACDT        KKACDT            7
     C     3             SUBST     KKACDT        XXFYR             3
     C                   MOVE      XXFYR         KKFYR
     C*** Beg Chg *** 12/14/11 ***********************************************
     C*******              Z-ADD432       KKFND
     C*
     C                   SELECT
     C     SPCO#         WHENEQ    '43'
     C                   Z-ADD     432           KKFND
     C     SPCO#         WHENEQ    '46'
     C                   Z-ADD     461           KKFND
     C                   ENDSL
     C*** End Chg *** 12/14/11 ***********************************************
     C*
     C     *IN68         IFEQ      *OFF
     C*
     C*** Begin Del *** 01/30/12 **************************************
     C********** CMKY      SETGTPCM02                60
     C********** CMKY      REDPEPCM02               N    60
     C********** *IN60     IFEQ *OFF
     C**********           ADD  1         CMCASE
     C**********           ELSE
     C**********           Z-ADD1         CMCASE
     C**********           ENDIF                           e-*in60
     C*** End   Del *** 01/30/12 **************************************
     C*** Begin Add *** 01/30/12 **************************************
     C     C1KY          CHAIN     PC101                              99
     C                   ADD       1             C1LCS#
     C                   UPDATE    PC101
     C                   Z-ADD     C1LCS#        CMCASE
     C*** End   Add *** 01/30/12 **************************************
     C*
     C                   ENDIF                                                  e-*IN68
     C*     Name
     C                   MOVE      ##CO#         CMCO#
     C                   MOVE      ##SUB#        CMSUB#
     C                   MOVE      KKFND         CMFND
     C                   MOVE      KKFYR         CMFYR
     C                   MOVEL(P)  SPLSN         CMLSN                          o Last name
     C                   MOVEL(P)  SPFSN         CMFSN                          o First name
     C                   MOVE      XXACDT        CMACDT
     C                   MOVE      XXDEDT        CMDEDT
     C*
     C*     Other fields
     C                   MOVEL(P)  SPSS#         CMSS#                          o Social security
     C                   MOVEL(P)  SPCLM#        CMSCL#                         o State claim #
     C                   MOVEL(P)  SPACDS        CMACDS                         o Acc description
     C                   MOVEL(P)  SPLTFL        CMLTFL                         o Lost time flag
     C                   MOVEL(P)  SPSTS         CMSTAT                         o Status
     C                   MOVEL(P)  SPDNY         CMDNY                          o Deny flag
     C                   MOVEL(P)  SPCLAD        CMNRSE                         o Case Nurse
     C                   MOVEL(P)  SPJRST        CMAST                           o Jurisdiction ST
     C*
     C*
     C                   ENDSR                                                  e- LODCLM
     C*
     C*================================================================
     C*       - Load Claimant Detail Data
     C*================================================================
     C     LODCLT        BEGSR
     C*
     C*     Social security
     C                   MOVEL     SPSS#         CDSS#                          o Social security
     C*     Name
     C                   MOVEL(P)  SPLSN         CDLSN                          o Last name
     C                   MOVEL(P)  SPFSN         CDFSN                          o First name
     C*     Address
     C                   MOVEL(P)  SPADR1        CDADR1                         o Address
     C                   MOVEL(P)  SPCTY         CDCTY                          o City
     C                   MOVEL(P)  SPST          CDST                           o State
     C                   MOVEL(P)  SPZIP         ZIPX                           o State
     C*     Gender
     C                   MOVEL(P)  SPSEX         CDSEX                          o Gender
     C*
     C*     Date of Birth
     C                   MOVE      XXDTOB        CMDEDT
     C*
     C*     Zip Code
     C                   MOVE      *OFF          *IN41
     C                   TESTN                   ZIP4X                41        numeric ?
     C     *IN41         IFEQ      *OFF                                         not numeric
     C                   MOVE(P)   '0000'        ZIP4X                          fix plus 4
     C                   TESTN                   ZIPX                 41        test all
     C                   ENDIF                                                  e- 41 = OFF
     C*
     C     *IN41         IFEQ      *ON                                          Is numeric now
     C                   MOVE      ZIPX          ZIPN              9 0
     C                   Z-ADD     ZIPN          CDZIP                             use input
     C                   ENDIF                                                  e- 41 = ON
     C*
     C                   ENDSR                                                  SRDET
     C*
     C*=========================================================================
     C*        WRITE MC CLAIM FILE - FMCMP
     C*=========================================================================
     C*
     C     MCCLMW        BEGSR
     C                   MOVE      CMCO#         QMCO#
     C                   MOVE      ##SUB#        QMSUB#
     C                   MOVE      CMFND         QMFND
     C                   MOVE      CMFYR         QMFYR
     C                   MOVE      CMCASE        QMCASE
     C*
     C                   MOVE      CMACDT        QMACDT
     C                   MOVE      CMEMP#        QMEMP#
     C                   MOVE      CMDIV         QMDIV
     C*
     C                   MOVE      CMACTM        QMACTM                         Accidnt time
     C                   MOVE(P)   CMDTRP        QMDTRP                         Reported
     C                   MOVE(P)   CMDTLW        QMDTLW                         Last worked
     C                   MOVE(P)   CMENTD        QMENTD                         ENTERED
     C                   MOVE(P)   CMRCVD        QMRCVD                         Received
     C                   MOVE(P)   CMMMI         QMMMI                          Max med impr
     C                   MOVE(P)   CMDEDT        QMDEDT                         Death date
     C*                                                                *********
     C     CMDTH         IFEQ      'Y'                                          Death flag
     C                   MOVEL     'DI'          QMDEEM
     C                   ELSE
     C                   MOVEL     *BLANKS       QMDEEM
     C                   ENDIF
     C*
     C                   MOVE(P)   CMDTRE        QMDTRE                         Rehabilitation
     C                   MOVE      CM#DEP        QM#DEP                         # dependents
     C                   MOVEL(P)  CMSHFT        QQWKPT                         Shift
     C*
     C                   CALL      'FA003RC'
     C*
     C                   PARM                    QMRTCD            1
     C                   PARM      CMSS#         QMSS#             9
     C                   PARM                    QMACDT            7
     C                   PARM                    QMCO#             3
     C                   PARM                    QMSUB#            5
     C                   PARM                    QMFND             3
     C                   PARM                    QMFYR             3
     C                   PARM                    QMCASE            7
     C                   PARM                    QMEMP#            9
     C                   PARM                    QMDIV             5
     C                   PARM                    QMACTM            6
     C                   PARM                    QMDTRP            7
     C                   PARM                    QMDTLW            7
     C                   PARM      CMACDS        QMACDS           60            Acc description
     C                   PARM      CMMCON        QMMCON            1            Method contact
     C                   PARM                    QMENTD            7
     C                   PARM                    QMRCVD            7
     C                   PARM                    QMMMI             7
     C                   PARM      *BLANKS       QM1A1             1            Unavailable
     C                   PARM      *BLANKS       QM10A1           10            Unavailable
     C                   PARM      CMLTFL        QMLTFL            1            Lost time flag
     C                   PARM      CMSCL#        QMSCL#           25            State claim numbr
     C                   PARM      *BLANKS       QMMCTM            4            Unavailable
     C                   PARM      *BLANKS       QMSTWK            2            Unavailable
     C                   PARM                    QMDEDT            7
     C                   PARM                    QMDEEM            2            Death claim flag
     C                   PARM      CMNRSE        QMCMNU            3            Nurse
     C                   PARM      *BLANKS       QMCMSU            3            Unavailable
     C                   PARM      *BLANKS       QMCLAD            3            Claim examiner
     C                   PARM                    QMRRN             9 0
     C                   PARM      CMLSN         QMLSN            30            Last name
     C                   PARM      CMFSN         QMFSN            15            First name
     C                   PARM      CMMID         QMMID             1            Middle initial
     C                   PARM                    QMDTRE            7
     C                   PARM      CMSUPR        QMCLSU            3            Supervisor
     C                   PARM                    QM#DEP            3
     C                   PARM      CMOCDS        QQOCDS           40            OCDS
     C                   PARM      CMHIDT        QQHIDT            7 0          HIDT
     C                   PARM      CMESTS        QQESTS            2            ESTS
     C                   PARM                    QQWKPT            2            SHFT
     C                   PARM      CMPREX        QQPREX            1            PREX
     C                   PARM      CMACD2        QQACD2           60            ACD2
     C                   PARM      CMCVFL        QQCVFL            1            CVFL
     C                   PARM      CMMCO         QQMCA             1            MCO
     C                   PARM      CMIPCT        QQMMIP            5 2          IPCT
     C                   PARM      CMODIS        QQODIS            1            ODIS
     C                   PARM      CMYRLX        QQYRLX            4 0          YRLX
     C                   PARM      CMDBDT        QQDBDT            7 0          DBDT
     C                   PARM      CMHRSD        QQHRDY            3 0          HRSD
     C                   PARM      CMDAYS        QQDYWK            1 0          DAYS
     C                   PARM      CMFEAD        QQCLFA            3            FEAD
     C                   PARM      CMDNY         QQDNY             1            DENIED
     C                   PARM      CMIMP         QQIMP             1            IMPAIRMENT
     C                   PARM      CMAST         QQJRST            2            JURS STATE
     C                   PARM      CMSTAT        QQSTAT            1            Claim Status ST
     C*
     C                   ENDSR
     C*
     C*=========================================================================
     C*        WRITE MC CLAIMANT HISTORY - FMCHP
     C*=========================================================================
     C     MCCLMH        BEGSR
     C*
     C                   CALL      'FA004RC'
     C*
     C                   PARM      ##CO#         @#CO#             3 0
     C                   PARM      CMFND         @@FND             3 0
     C                   PARM      CMFYR         @@FYR             3 0
     C                   PARM      CMCASE        @@CASE            7 0
     C                   PARM      XXCLDT        @#EFDT            7 0
     C                   PARM      SPSTS         @@OCST            1
     C                   PARM                    @@EXPL           60
     C                   PARM                    @@CLCD            3
     C                   PARM                    @@RTCD            1
     C                   PARM      ##SUB#        @@SUB#            5 0
     C*
     C                   ENDSR
     C*
     C*=========================================================================
     C*        WRITE MC CLAIMANT DEMOGRAPHICS - FMCDP
     C*=========================================================================
     C     MCCLMD        BEGSR
     C*
     C                   MOVE      CDZIP         QQZIP
     C                   MOVE      CDDTOB        QQDTOB
     C                   MOVE      CDAREA        QQAREA
     C                   MOVE      CDPHN#        QQPHN#
     C                   MOVE      CDWARA        QQWARA
     C                   MOVE      CDWPH#        QQWPH#
     C*
     C                   CALL      'FA002RC'
     C*
     C                   PARM                    QQRTCD            1
     C                   PARM      CDSS#         QQSS#             9
     C                   PARM                    QQACDT            7
     C                   PARM      '114'         QQCO#             3
     C                   PARM      *BLANK        QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM      CDLSN         QQLST            30
     C                   PARM      CDFSN         QQNAME           15
     C                   PARM      CDMID         QQMAD1            1
     C                   PARM      CDADR1        QQADR1           40
     C                   PARM      CDADR2        QQADR2           40
     C                   PARM      CDCTY         QQCTY            25
     C                   PARM      CDST          QQST              2
     C                   PARM                    QQZIP             9
     C                   PARM                    QQDTOB            7
     C                   PARM      CDSEX         QQSEX             1
     C                   PARM                    QQAREA            3
     C                   PARM                    QQPHN#            7
     C*
     C                   PARM      CDMSTS        QQMSTS            1
     C                   PARM                    QQWARA            3
     C                   PARM                    QQWPH#            7
     C                   PARM      *BLANKS       QQCTRY            3
     C                   PARM      *BLANKS       QQADR3           40
     C*
     C                   ENDSR
     C*
     C*=========================================================================
     C*     ERROR PRINT
     C*=========================================================================
     C     ERRPRT        BEGSR
     C*
     C                   ADD       1             ERCNT             7 0
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
        SPSS# = MaskData('SSN':'SPSS#':SPSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   EXCEPT    ERR1
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
        SPSS# = UnMaskData('SSN':'SPSS#':SPSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   ENDSR
     C*
     C*=========================================================================
     C*      *INZSR - Initial Logic
     C*=========================================================================
     C     *INZSR        BEGSR
     C* ------------------------------------------------------------------------
     C*  KLISTS
     C* ------------------------------------------------------------------------
     C*** Begin Add *** 01/30/12 **************************************
     C*   KEY LIST FOR WCMC COMPANY AUXILIARY
     C     C1KY          KLIST
     C                   KFLD                    UPCO#
     C*** End   Add *** 01/30/12 **************************************
     C*   KEY LIST FOR MC CLAIM MASTER
     C     CLMKY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SPCLM#
     C*
     C     CMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C*
     C     CMKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    CMCASE
     C*
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    KKFND
     C*
     C*   KEY LIST FOR THE STATE FILE
     C     STKY          KLIST
     C                   KFLD                    XXKEY            10
     C*
     C* ------------------------------------------------------------------------
     C* PLISTS
     C* ------------------------------------------------------------------------
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMMDY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#
     C                   PARM                    ##FYR             3 0
      *
     C* ------------------------------------------------------------------------
     C* BEGIN
     C* ------------------------------------------------------------------------
     C*    Define fields
     C     *LIKE         DEFINE    CMDT          XXDT
     C     *LIKE         DEFINE    CMDT          XXCLDT
     C     *LIKE         DEFINE    CMDT          XXDEDT
     C     *LIKE         DEFINE    CMDT          XXDTOB
     C     *LIKE         DEFINE    CMDT          XXACDT
     C     *LIKE         DEFINE    UPSUB#        ##SUB#
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMFND         KKFND
     C* ------------------------------------------------------------------------
     C*     Get  user and company
     C     Q1USER        CHAIN     PUP01                              60
     C     UPCO#         CHAIN     PCO01                              60
     C*
     C*     Calculate date
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMMDY
     C                   MOVEL     SRDTO         XXDATE            8 0
     C*
     C                   MOVE      *ON           *INOF
     C*
     C                   ENDSR                                                  e- *INZSR
     C*=========================================================================
     C*      ENDER - End of Job
     C*=========================================================================
     C     ENDER         BEGSR
     C*
     C*     Shut down MC files.
     C***                  MOVE '*'       QMRTCD
     C***                  EXSR MCCLMW
     C***                  MOVE '*'       QQRTCD
     C***                  EXSR MCCLMD
     C*
     C*     Print File Totals
     C                   EXCEPT    TOTF
     C                   DO        20            F                 3 0
     C     FI(F)         IFNE      *ZERO
     C     FW(F)         ORNE      *ZERO
     C                   EXCEPT    DETF
     C                   ENDIF                                                  Fx, <> 0
     C                   ENDDO
     C*
     C*________________________________________________________________
     C*     Print Error summary
     C                   XFOOT     ERC           ERCNT             7 0
     C                   EXCEPT    TOTE
     C*
     C                   DO        12            Z                 3 0
     C     ERC(Z)        IFGT      *ZEROS
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF                                                  e OF = On
     C                   EXCEPT    DETE
     C                   ENDIF                                                  e ERC > 0
     C                   ENDDO                                                  e DO 20
     C*
     C                   ENDSR                                                  ENDER
     O*=========================================================================
     O*      R   E   P   O   R   T   S
     O*=========================================================================
     O*
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME               8 '  :  :  '
     O                       TITL1               50
     O                       XXDATE              78 '  /  /    '
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       TITL2               54
     O                       Q1USER             116
     O                       PAGE2         Z    128
     O                                          123 'PAGE'
     O          E            HEAD           2
     O                                           22 '   SS#    Claim #     '
     O                                           44 '              Acc date'
     O                                           66 ' Name                 '
     O                                           88 '         Error        '
     O                                          110 '                      '
     O                                          110 '                      '
     O                                          122 'In Rcd#'
     O*
     O          E            ERR1           1
     O                       SPSS#                9
     O                       SPCLM#              35
     O                       ACDT8               44
     O                       SPLSN               75
     O                       SPFSN               75
     O                       ERR(E)             105
     O               88                         115 'SKIPPED'
     O                       INCNT         Z    122
     O          E            TOTF        1  2
     O                                           44 '***** TOTALS ***** '
     O*
     O          E            TOTF           2
     O                                           22 'Total number of record'
     O                                           44 's read  . . . . . . . '
     O                       INCNT         Z     54
     O*_______________________________________________________________
     O*
     O          E            TOTF        2  1
     O                                           22 '*** RECORD  COUNT SUMM'
     O                                           44 'MARY ***              '
     O          E            TOTF        1  2
     O                                           22 'File name     Input re'
     O                                           44 'ad   Output records   '
     O                                           66 ' Errors               '
     O          E            DETF           1
     O                       FN(F)               10
     O                       FI(F)         Z     22
     O                       FW(F)         Z     37
     O                       FE(F)         Z     52
     O*
     O*_______________________________________________________________
     O          E            TOTE        2  1
     O                                           22 '** ERROR SUMMARY **   '
     O          E            TOTE           2
     O                                           22 '    Number   Descripti'
     O                                           44 'on                    '
     O*
     O          E            DETE           1
     O                       ERC(Z)        Z     10
     O                       ERR(Z)            +  3
     O*________________________________________________________________
     O* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     O*         A R R A Y    D A T A
     O* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
** TABLE v      FN - File names
FASPP        1
xxxxxx2      2
FA002RC      3
FA003RC      4
FA004RC      5
xxxxxx6      6
SKIPPED      7
xxxxxx8      8
xxxxxx9      9
xxxxxx10    10
INV CLM#    11
INV SS#     12
INV ACDT    13
INV NAME    14
INV LTFL    15
INV STAT    16
INV JRST    17
XXXXX18     18
XXXXX19     19
xxxxxx20    20
NO ACDESC   21
NO ADDR     22
NO CITY     23
ST INV      24
NO ZIPC     25
INV CLDT    26
INV DEDT    27
xxxxxx28    28
xxxxxx29    29
xxxxxx30    30
** TABLE   ERR               v   ERROR #  Error descriptions
State Claim# missing                   1
Social Security# missing               2
Invalid accident date                  3
Name missing                           4
Invalid lost time flag                 5
Invalid status                         6
Invalid jurisdiction state             7
Invalid company number                 8
xxxxxxxxxxx09                          9
Rejected for Fatal Error              10
Accident description missing          11
Address missing                       12
City missing                          13
State invalid                         14
Zip code missing                      15
Denied flag blank                     16
Close date invalid                    17
Date of death invalid                 18
xxxxxxxxxxxxxxxx 19                   19
xxxxxxxxxxxxxxxx 20                   20
