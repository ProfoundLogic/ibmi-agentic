     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0070RTN - STATE NOI INFORMATION MAINTENANCE
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION:  This program is used to enter fields   *
     H*    which are used on the TN Notice of Injury form.  Most of   *
     H*    the fields exist in the Claim Master file and can be       *
     H*    updated on this screen if the user chooses.                *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    Work with Claims (determined by the screen sequence in the *
     H*      Group Claim/DARTS control file)                          *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  PROGRAMMER: C:02                                             *
     H*  CREATED:    09/24/98    PROJECT: WC859                       *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     F* 01/28/10  C:249  Project: SY215                               *
     F*                  Retrieve brand Name
     F*                                                               *
     H* 10/22/09 C:189 SEQ#70040                                      *
     H*                Additional screens being displayed which       *
     H*                has not been set up in Group set up.           *
     H*                                                               *
     H* 08/28/09 C:189 Project: 133WHE                                *
     H*                Add the call to Accident Info program          *
     H*                W0070R08 and not call W0190R and SC010R.       *
     H*                                                               *
     H* 05/18/00 C:53  PROJECT: WC962
     H*                Added jurisdiction state to WMNIP chain.
     H*                Load CMJRST to NIST.
     H*                Corrected definition of field XXDT             *
     H*                                                               *
     H* 09/16/99 C:61  PROJECT WC577                                  *
     H*                Add employer cancel date to header info.       *
     H*                                                               *
     H* 12/24/98  C:58  PROJECT: SUPP WC048 Sequence # 15566
     H*                 Override user designated fund name from WA001R
     H*                 with the actual fund of this claim.
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*   N09  -  MAINTENANCE MODE                                    *
     H*    09  -  INQUIRY MODE                                        *
     H*    40  -  CONTROL ADD/UPDATE MODE                             *
     H*    60  -  GENERAL USAGE                                       *
     H*                                                               *
     H*****************************************************************
     H*
     FW0070FTN  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMCXL     IF   E           K DISK
     FWMNIL     UF A E           K DISK
      *
      *
      ***** BEG ADD ******* 09/16/99 **********************************
      *
      *  EMPLOYER'S EFFECTIVE DATES
      *
      *
      *
      /COPY SCOPYMT01D
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
     D*** BEGIN ADD *** 01/28/10 **************************************
     D*
     D***PY SCOPYMT01I
     D*
     D*** END ADD ***** 01/28/10 **************************************
      *
     D                 DS                  INZ
     D  ED                     1     35  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS                  INZ
     D  CD                     1     35  0
     D                                     DIM(5) ASCEND                        CANCEL  DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      *
      ***** END ADD ******* 09/16/99 **********************************
      *
     D*** BEGIN DEL *** 01/28/10 ********************************      ******
     D***         DS
     D***                                     1  140DSDTTM
     D***                                     1   60DSTIME
     D***                                     7  140DSDATE
     D*** END DEL ***** 01/28/10 ********************************      ******
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D                 DS
     D  QCKEY                  1     40
     D  XXCO#                  1      3  0
     D  XXFND                  4      6  0
     D  XXFYR                  7      9  0
     D  XXCASE                10     16  0
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D COMAPI          C                   CONST('WA001R')
     D*** BEGIN DEL *** 01/28/10 ********************************      ******
     D***           '-- Add --'           C         MODEA
     D***           '- Change -'          C         MODEC
     D***           '-Display-'           C         MODED
     D OPTNM           C                   CONST('Claim State Specific-
     D                                      Information')
     D***           'Progressive Data So- C         PDSNAM
     D***           'lutions, Inc.'
     D*** END   DEL *** 01/28/10 ********************************      ******
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     I*** BEGIN DEL *** 01/28/10 ********************************      ******
     I***PSDS      ESDSSZQ1P
     I*** END   DEL *** 01/28/10 ********************************      ******

     C*****************************************************************
     C*  PROGRAM FILE KEY LISTINGS
     C*****************************************************************
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE CLAIM MASTER
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
     C*
     C*   KKey list for the Employer file
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  CLAIM/DARTS CONTROL FILE
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  STATE NOI INFORMATION FILE
     C*
     C     NIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXST                           ADD 05/18/00
     C*
     C*****************************************************************
     C*  *LIKE DEFINES
     C*****************************************************************
     C     *LIKE         DEFINE    CDSS#         XXSS#
     C********** *LIKE     DEFN DSDATE    XXDT             DEL 05/18/00
     C     *LIKE         DEFINE    FNEXDT        XXDT                           ADD 05/18/00
     C     *LIKE         DEFINE    NIST          XXST                           ADD 05/18/00
     C*****************************************************************
     C*  PROGRAM ENTRY PARAMETERS
     C*****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQRTCD            1
      ***** BEG ADD ******* 10/22/09 **********************************
     C                   PARM                    @@CONT            1
      ***** END ADD ******* 10/22/09 **********************************
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     ACDPL3        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQACDT            8
     C                   PARM                    QQRTCD
     C*** BEG ADD ***** 08/28/09 **************************************
     C     ACDPL4        PLIST
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
      ***** BEG CHG ******* 10/22/09 **********************************
     C*************        PARM 'Y'       @@CONT  1
     C                   PARM                    @@CONT            1
      ***** END CHG ******* 10/22/09 **********************************
     C*** END ADD ***** 08/28/09 **************************************
     C*****************************************************************
     C*  PROGRAM START
     C*****************************************************************
     C*
     C*  MOVE THE PARM FIELDS TO WORK FIELDS
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C*
     C*** BEG ADD ***** 08/28/09 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** END ADD ***** 08/28/09 **************************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C                   MOVE      Q1USER        SCUSER
     C*
     C*  Load the program title for display heading
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C*  Load the display mode based on QQCALL.
     C*
     C     QQCALL        IFEQ      'A'
     C                   MOVEL(P)  MODEA         SCMODE
     C                   ENDIF
     C*
     C     QQCALL        IFEQ      'C'
     C     QQCALL        OREQ      'Y'
     C                   MOVEL(P)  MODEA         SCMODE
     C                   ENDIF
     C*
     C     QQCALL        IFEQ      '1'
     C     QQCALL        OREQ      '2'
     C     QQCALL        OREQ      'Z'
     C                   MOVE      *ON           *IN09
     C                   MOVEL(P)  MODED         SCMODE
     C                   ENDIF
     C*
     C*  Only allow F3 to be used if called directly from option 15
     C*  of Work With Claims.
     C*
     C                   MOVE      *ON           *IN08
     C     QQCALL        IFEQ      ' '
     C     QQCALL        OREQ      '1'
     C     QQCALL        OREQ      '2'
     C                   MOVE      *OFF          *IN08
     C                   ENDIF
     C     QQCALL        IFEQ      ' '
     C     QQCALL        OREQ      'C'
     C                   MOVE      *ON           *IN90
     C                   ELSE
     C                   MOVE      *OFF          *IN90
     C                   ENDIF
     C*
     C*  Get the company and fund names.
     C*
     C                   Z-ADD     XXCO#         ##CO#
     C                   Z-ADD     XXFND         ##FND
     C                   CALL      COMAPI        COMPL                  99
     C     *IN99         IFEQ      *ON
     C***  Begin Chg  ***  01/28/10  **************************************
     C**********           MOVELPDSNAM    ##DATA    P
     C                   CALL      'RTVBRND'                            99
     C                   PARM      'I'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C*
     C                   MOVEL(P)  #BRND1        ##DATA
     C***  End   Chg  ***  01/28/10  **************************************
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      40            ##LENG
     C                   MOVEL     ##DATA        ##CONM
     C                   ENDIF
     C                   MOVEL     ##CONM        SCCONM
     C                   MOVEL     ##FNAM        SCFNAM
     C*
     C*  IF FUND IS EXPIRED, THEN DISPLAY IN INQUIRY MODE
     C*
     C                   Z-ADD     XXFND         SCFND
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C     FNEXDT        ANDGT     *ZERO
     C     FNEXDT        ANDLT     XXDT
     C                   MOVE      *ON           *IN09
     C                   MOVEL(P)  MODED         SCMODE
     C                   ENDIF
     C*** BEGIN ADD *** 12/24/98 **************************************
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNNAME        SCFNAM
     C                   ENDIF                                                  . *IN60
     C*** END ADD ***** 12/24/98 **************************************
     C     TOP           TAG
     C*
     C*  Retrieve claimant info
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      CMJRST        XXST                           ADD 05/18/00
     C*
     C     CMSS#         IFNE      *BLANKS
      *****  Begin Chg  ***  06/09/20  **********************
     C*****SS1           CAT(P)    '-':0         SCSS#
     C*****              CAT       SS2:0         SCSS#
     C*****              CAT       '-':0         SCSS#
     C*****              CAT       SS3:0         SCSS#
      /Free
       scSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-Free
      *****  End Chg  ***  06/09/20  **********************
     C*
     C*  CLAIMANT NAME
     C*
     C                   MOVEL     CMSS#         XXSS#
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      *OFF
     C     CDLSN         CAT(P)    ',':0         SCCNAM                         LAST, FIRST M.
     C                   CAT       CDFSN:1       SCCNAM
     C     CDMID         IFNE      *BLANK
     C                   CAT       CDMID:1       SCCNAM
     C                   CAT       '.':0         SCCNAM
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  CMSS# <> BLANK
     C*
     C                   MOVE      *BLANK        SCACDT
     C     CMACDT        IFNE      *ZERO
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCACDT
     C                   ENDIF
     C*
     C                   MOVEL(P)  CMACL#        SCSCL#
     C                   MOVEL(P)  QQFYR         SCYRCS
     C                   CAT       '-':0         SCYRCS
     C                   CAT       QQCASE:0      SCYRCS
     C*
     C*  Get the Emp#, Div, & name
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C                   CALL      'WA005R'
     C                   PARM                    EMEMP#
     C                   PARM                    EMDIV
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    XXEDN            80
      ***** BEG ADD ******* 09/16/99 **********************************
      *
      * Get employer cancel date
      *
     C                   Z-ADD     5             X                 3 0
      *
     C     ED(X)         DOWEQ     *ZERO
     C     X             ANDGT     1
     C                   SUB       1             X
     C                   ENDDO
      *
     C     CD(X)         IFNE      *ZERO
     C                   SETOFF                                       41
     C                   Z-ADD     CD(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCCXDT
      *
     C                   ELSE
      *
     C                   SETON                                        41
     C                   ENDIF                                                  CD,X
      *
     C                   ELSE
      *
     C                   SETON                                        41
      ***** END ADD ******* 09/16/99 **********************************
     C                   ENDIF                                                  Emp record found
     C*
     C                   ENDIF                                                  Claim record found
     C*
     C*  GET THE REQUESTED RECORD, AND LOAD "ADD OR UPDATE" TO
     C*  THE SCREEN.
     C*
     C                   EXSR      SRLOAD
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
     C*
     C*****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*****************************************************************
     C*
B001 C     *IN11         IFEQ      *ON
     C*
     C*******************************
     C*  F3 = Exit
     C*******************************
     C*
     C     *INKC         IFEQ      *ON                                          CMD 1
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF                                                  e- *INKA
     C*
     C* =======================================================
     C*  F11 - EDI LOG HISTORY
     C* =======================================================
     C     *INKK         IFEQ      '1'
      *
     C                   MOVE      SCPRDS        Q1PRDS           40
     C                   MOVE      QQCASE        Q1CASE            7
     C                   MOVE      QQFYR         Q1FYR             3
     C                   MOVE      QQFND         Q1FND             3
     C                   MOVE      QQCO#         Q1CO#             3
     C                   MOVE      *BLANKS       Q1RTCD            1
     C*
     C                   CALL      'W0270R5'
     C                   PARM                    Q1PRDS
     C                   PARM      ' '           Q1CALL            1
     C                   PARM                    Q1CASE
     C                   PARM                    Q1FYR
     C                   PARM                    Q1FND
     C                   PARM                    Q1CO#
     C                   PARM                    Q1RTCD
      *
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   GOTO      TOP
      *
     C                   ENDIF
     C********************************
     C*  F12 = Cancel
     C********************************
     C     *INKL         IFEQ      *ON                                          CMD 2
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN11
     C*
     C**************************************************************
E001 C                   ENDIF                                                  END - CMD KEYS
     C*****************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*****************************************************************
     C*   PROCESS REQUEST SCREEN 1
     C*****************************************************************
     C*
     C     SCR01T        TAG
     C*
     C     *IN09         CABEQ     '1'           INQ1
     C*
     C*  UPDATE OR WRITE THE STATE INFORMATION RECORD
     C*
     C     NIKY          CHAIN     PNI01                              40
     C*
     C                   MOVE      XXCO#         NICO#
     C                   MOVE      XXFND         NIFND
     C                   MOVE      XXFYR         NIFYR
     C                   MOVE      XXCASE        NICASE
     C                   MOVEL(P)  SCACNT        NI10A1
     C                   MOVEL(P)  SCDPCD        NI20A1
     C                   MOVEL(P)  SCHWAG        NI10A2
     C                   MOVEL(P)  SCDWAG        NI10A3
     C                   Z-ADD     SCOWKS        NI3N1
     C                   MOVEL(P)  SCOAMT        NI10A4
     C                   Z-ADD     SCBWKS        NI3N2
     C                   MOVEL(P)  SCBAMT        NI10A5
     C                   MOVEL(P)  SCNREL        NI80A1
     C                   MOVEL(P)  SCDOIN        NI80A2
     C                   MOVEL(P)  SCOCCR        NI80A3
     C                   MOVEL(P)  SCOBJ1        NI80A4
     C                   MOVEL(P)  SCOBJ2        NI80A5
     C                   MOVE      CMJRST        NIST                           ADD 05/18/00
     C                   MOVE      Q1USER        NIUSER
     C                   MOVE      Q1PGM         NIPGM
     C                   Z-ADD     DSTIME        NITIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NIDT
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PNI01
     C                   ELSE
     C                   MOVE      Q1USER        NIIUSR
     C                   Z-ADD     DSTIME        NIITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NIIDT
     C                   WRITE     PNI01
     C                   ENDIF
     C*
     C     INQ1          TAG
     C*
     C*  IF QQCALL = A, GO ON TO THE NEXT PROGRAM IN ADD MODE.
     C*  IF QQCALL = Y, GO ON TO THE NEXT PROGRAM IN CHANGE MODE.
     C*  IF QQCALL = Z, GO ON TO THE NEXT PROGRAM IN DISPLAY MODE.
     C*
     C*** BEG ADD ***** 08/28/09 **************************************
     C     CLTID         IFNE      '133'
     C*** END ADD ***** 08/28/09 **************************************
      *
     C     QQCALL        IFEQ      'Y'
     C     QQCALL        OREQ      'Z'
     C                   MOVE      '1'           QQRTCD
     C                   CALL      'W0190R'      ACDPL3                 99
     C*
     C     QQRTCD        IFEQ      'B'
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      'A'
     C                   GOTO      TERM
     C                   END
     C*
     C                   END                                                     QQCALL = Y,Z
     C*
     C     QQCALL        IFEQ      'A'
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   SELECT
     C     CXDBEN        WHENEQ    'Y'
     C                   MOVE      '1'           QQRTCD
     C                   CALL      'W0190R'      ACDPL3                 99
     C*
     C     CXDART        WHENEQ    'Y'
     C                   CALL      'SC010R'                             99
     C                   PARM      QCKEY         QAKEY            40
     C                   PARM      'W0070R'      QQPGM            10
     C                   ENDSL
     C*
     C     QQRTCD        IFEQ      'B'
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      'A'
     C                   GOTO      TERM
     C                   END
     C*
     C                   END                                                     *IN60
     C*
     C                   END                                                     QQCALL = A
     C*** BEG ADD ***** 08/28/09 **************************************
     C                   ENDIF                                                   CLTID <> 133
      *
     C     CLTID         IFEQ      '133'
     C     QQCALL        ANDNE     *BLANKS
      *
     C     CMKY          CHAIN     PCM01                              99
     C                   Z-ADD     DBRRN         @@RRN
      *
     C                   SELECT
     C     QQCALL        WHENEQ    'A'
     C                   MOVEL     'A'           @@MODE
     C     QQCALL        WHENEQ    'Y'
     C                   MOVEL     'C'           @@MODE
     C     QQCALL        WHENEQ    'Z'
     C                   MOVEL     'D'           @@MODE
     C                   ENDSL
      *
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'W0070R08'    ACDPL4                   99
      *
     C   KL              MOVE      'B'           QQRTCD
     C   KC              MOVE      'A'           QQRTCD
      *
     C     QQRTCD        CABEQ     'B'           STRT
     C     QQRTCD        CABEQ     'A'           TERM
      *
     C                   ENDIF                                                   CLTID = 133
     C*** END ADD ***** 08/28/09 **************************************
      *
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C*
     C************************************************************
     C*  THIS SUBROUTINE LOADS THE FIELDS ON SCR01.              *
     C************************************************************
     C     SRLOAD        BEGSR
     C*
     C     NIKY          CHAIN     PNI01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     NI10A1        SCACNT
     C                   MOVEL     NI20A1        SCDPCD
     C                   MOVEL     NI10A2        SCHWAG
     C                   MOVEL     NI10A3        SCDWAG
     C                   Z-ADD     NI3N1         SCOWKS
     C                   MOVEL     NI10A4        SCOAMT
     C                   Z-ADD     NI3N2         SCBWKS
     C                   MOVEL     NI10A5        SCBAMT
     C                   MOVEL     NI80A1        SCNREL
     C                   MOVEL     NI80A2        SCDOIN
     C                   MOVEL     NI80A3        SCOCCR
     C                   MOVEL     NI80A4        SCOBJ1
     C                   MOVEL     NI80A5        SCOBJ2
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       SCACNT
     C                   MOVE      *BLANKS       SCDPCD
     C                   MOVE      *BLANKS       SCHWAG
     C                   MOVE      *BLANKS       SCDWAG
     C                   Z-ADD     *ZEROS        SCOWKS
     C                   MOVE      *BLANKS       SCOAMT
     C                   Z-ADD     *ZEROS        SCBWKS
     C                   MOVE      *BLANKS       SCBAMT
     C                   MOVE      *BLANKS       SCNREL
     C                   MOVE      *BLANKS       SCDOIN
     C                   MOVE      *BLANKS       SCOCCR
     C                   MOVE      *BLANKS       SCOBJ1
     C                   MOVE      *BLANKS       SCOBJ2
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C**************************************************************************
