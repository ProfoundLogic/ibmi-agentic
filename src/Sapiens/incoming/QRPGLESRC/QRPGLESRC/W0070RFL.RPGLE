     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0070RFL - STATE NOI INFORMATION MAINTENANCE
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION:  This program is used to enter fields   *
     H*    which are used on the FL Notice of Injury form.  Most of   *
     H*    the fields exist in the Claim Master file and can be       *
     H*    updated on this screen if the user chooses.                *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    Work with Claims (determined by the screen sequence in the *
     H*      Group Claim/DARTS control file)                          *
     H*    Request Payment Process - W0080R                           *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  PROGRAMMER: C:15                                             *
     H*  CREATED:    07/09/98    PROJECT: WC577                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/15/22 C:53  Support: 103684
     H*                Added field to global edit.
     H*
     H*  06/09/20 C:318 Project: SY420
     H*                 Mask SSN
     H*
     H*    01/28/10 C:249 PROJECT: SY215
     H*             C:189 CHANGE BRAND NAME.
     H*
     H* 10/22/09 C:189 SEQ#70040                                      *
     H*                Additional screens being displayed which       *
     H*                has not been set up in Group set up.           *
     H*                                                               *
     H* 08/28/09 C:189 Project: 133WHE                                *
     H*                Add the call to Accident Info program          *
     H*                W0070R08 and not call W0190R and SC010R.       *
     H*                                                               *
     H* 12/21/05 C:15  Support: WC048/50338                           *
     H*                Fixed edit on disability type.                 *
     H*                                                               *
     H* 04/01/05 C:15  Project: WC1276                                *
     H*                Added fields for new DWC-1 form.               *
     H*                -Indemnity only denied case.                   *
     H*                -Employee's 8th day of disability.             *
     H*                -Entity's knowledge of 8th day of disability.  *
     H*                -Penalty amount paid in 1st payment.           *
     H*                -Interest amount paid in 1st payment.          *
     H*                Added disability type 'SO' selection.          *
     H*                                                               *
     H* 05/18/00 C:53  PROJECT: WC962
     H*                Added jurisdiction state to WMNIP chain.
     H*                Load CMJRST to NIST.
     H*                                                               *
     H* 09/23/99 C:15  Support: WC048/19562                           *
     H*                Removed edits on protected fields.             *
     H*                                                               *
     H* 09/16/99 C:61  PROJECT WC577                                  *
     H*                Add employer cancel date to header info.       *
     H*                                                               *
     H* 05/27/99  C:33  SEQ#: WC048/16599                             *
     H*                -AVOID RECURSIVE CALLS.                        *
     H*                -CALL F9 & F11 IN CORRECT MODE.                *
     H*                                                               *
     H* 12/24/98  C:58  PROJECT: SUPP WC048 Sequence # 15566
     H*                 Override user designated fund name from WA001R
     H*                 with the actual fund of this claim.
     H*                                                               *
     H* 09/05/98  C:02  PROJECT: 48WLU                                *
     H*                 MODIFICATION TO REMOVE FIELD EDITING FOR THE  *
     H*                 FOLLOWING FIELDS:                             *
     H*                    CASE DENIED                                *
     H*                    FIRST DATE OF DISABILITY                   *
     H*                    FULL SALARY                                *
     H*                    SALARY CONTINUED                           *
     H*                    AWW                                        *
     H*                    COMPENSATION RATE                          *
     H*                 THESE FIELDS HAVE BEEN CHANGED TO DISPLAY ONLY*
     H*                 ADDED F9=CLM MGT INFO AND F11=BENEFIT INFO    *
     H*                                                               *
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
     FW0070FFL  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F*** BEGIN ADD *** 09/05/98 **************************************
     F                                     INFDS(DBFDS)
     F*** END ADD *** 09/05/98 ****************************************
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMCXL     IF   E           K DISK
     FWMNIL     UF A E           K DISK
      /COPY SCOPYMT01D
     D*
      *
      *
      ***** BEG ADD ******* 09/16/99 **********************************
      *
      *  EMPLOYER'S EFFECTIVE DATES
      *
      *
     D*** BEGIN ADD *** 01/28/10 **************************************
     D*
     D***PY SCOPYMT01I
     D*
     D*** END ADD ***** 01/28/10 **************************************
      *
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
     D*** BEGIN ADD *** 09/05/98 **************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*** END ADD *** 09/05/98 ****************************************
     D*
     D COMAPI          C                   CONST('WA001R')
     D*** BEGIN DEL *** 01/28/10 **************************************
     D***           '-- Add --'           C         MODEA
     D***           '- Change -'          C         MODEC
     D***           '-Display-'           C         MODED
     D OPTNM           C                   CONST('Claim State Specific-
     D                                      Information')
     D***           'Progressive Data So- C         PDSNAM
     D***           'lutions, Inc.'
     D*** END DEL *** 01/28/10 ****************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     I*** BEGIN DEL *** 01/28/10 **************************************
     I***PSDS      ESDSSZQ1P
     I*** END DEL   *** 01/28/10 **************************************

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
     C*
     C     *LIKE         DEFINE    CDSS#         XXSS#
     C***  BEGIN CHG  ***  05/27/99  *********************************
     C********** *LIKE     DEFN DSDATE    XXDT
     C     *LIKE         DEFINE    CDDT          XXDT
     C***  END   CHG  ***  05/27/99  *********************************
     C***  BEGIN ADD  ***  05/27/99  *********************************
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    *IN21         XXERR
     C***  END   ADD  ***  05/27/99  *********************************
     C     *LIKE         DEFINE    NIST          XXST                           ADD 05/18/00
     C*
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
     C***********          PARM 'Y'       @@CONT  1
     C                   PARM                    @@CONT            1
      ***** END CHG ******* 10/22/09 **********************************
     C*** END ADD ***** 08/28/09 **************************************
     C*****************************************************************
     C*  PROGRAM START
     C*****************************************************************
     C*
     C***  BEGIN ADD  ***  05/27/99  *********************************
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'W0070R09'    XXPGM
     C     *IN21         PARM                    XXERR
     C*
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'W0070R11'    XXPGM
     C     *IN22         PARM                    XXERR
     C***  END   ADD  ***  05/27/99  *********************************
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
     C*
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
     C     QQCALL        IFEQ      ' '
     C     QQCALL        OREQ      'C'
     C     QQCALL        OREQ      'Y'
     C                   MOVEL(P)  MODEC         SCMODE
     C                   ENDIF
     C*
     C     QQCALL        IFEQ      '1'
     C     QQCALL        OREQ      '2'
     C     QQCALL        OREQ      'Z'
     C                   MOVE      *ON           *IN09
     C                   MOVEL(P)  MODED         SCMODE
     C                   ENDIF
     C*
     C*  Don't allow F3 if called from the Request Payment pgm.
     C*
     C                   MOVE      *OFF          *IN08
     C     QQCALL        IFEQ      'C'
     C                   MOVE      *ON           *IN08
     C                   ENDIF
     C*** BEGIN ADD *** 09/05/98 **************************************
     C     QQCALL        IFEQ      'C'
     C                   MOVE      *ON           *IN90
     C                   ELSE
     C                   MOVE      *OFF          *IN90
     C                   ENDIF
     C*** END ADD *** 09/05/98 ****************************************
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
     C*** BEGIN ADD *** 09/05/98 **************************************
     C     TOP           TAG
     C*** END ADD *** 09/05/98 ****************************************
     C*
     C*  Retrieve claimant info
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      CMJRST        XXST                           ADD 05/18/00
     C*
     C     CMSS#         IFNE      *BLANKS
     C***  Begin Add  ***  06/09/20  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*MASK'       qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'cmss#'       qqname           25
     C     cmss#         parm      cmss#         qqval           100
     C***  End Add  ***  06/09/20  *******************************
     C     SS1           CAT(P)    '-':0         SCSS#
     C                   CAT       SS2:0         SCSS#
     C                   CAT       '-':0         SCSS#
     C                   CAT       SS3:0         SCSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*UNMASK'     qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'cmss#'       qqname           25
     C     cmss#         parm      cmss#         qqval           100
     C***  End Add  ***  06/09/20  *******************************
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
     C*
     C*  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
     C*
     C     CMCVT#        IFNE      *BLANK
     C                   MOVEL     CMCVT#        SCCVT
     C                   MOVE      *ON           *IN64
     C                   ENDIF
     C                   MOVEL(P)  QQFYR         SCYRCS
     C                   CAT       '-':0         SCYRCS
     C                   CAT       QQCASE:0      SCYRCS
     C*
     C*  Get the Emp#, Div, & name
     C*
     C     EMKY          CHAIN     PEM01                              60
      *
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
     C*** BEGIN ADD *** 09/05/98 **************************************
      *
      * =======================================================
      *  F9 - Management info
      * =======================================================
      *
     C     *INKI         IFEQ      '1'
      *
     C                   Z-ADD     DBRRN         ##RRN
     C                   MOVE      Q2RECF        SVRECF           10
     C***  BEGIN ADD  ***  05/27/99  *********************************
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C***  END   ADD  ***  05/27/99  *********************************
      *
     C                   CALL      'W0070R09'
     C     *INKC         PARM      *ZERO         ##KC              1
     C     *INKL         PARM      *ZERO         ##KL              1
     C***  BEGIN CHG  ***  05/27/99  *********************************
     C**********           PARM 'C'       ##MODE  1
     C                   PARM                    ##MODE            1
     C***  END   CHG  ***  05/27/99  *********************************
     C                   PARM                    ##RRN             9 0
      *
      * If F3=Exit is hit, Return to Menu.
      *
     C     *INKC         IFEQ      *ON
     C***  BEGIN ADD  ***  05/27/99  *********************************
     C                   MOVE      'A'           QQRTCD
     C***  END   ADD  ***  05/27/99  *********************************
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVE      SVRECF        Q2RECF
     C                   GOTO      TOP
      *
     C                   ENDIF
     C*
     C* =======================================================
     C*  F11 - Benefit info
     C* =======================================================
     C     *INKK         IFEQ      '1'
      *
     C                   Z-ADD     DBRRN         ##RRN
     C                   MOVE      Q2RECF        SVRECF
     C***  BEGIN ADD  ***  05/27/99  *********************************
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C***  END   ADD  ***  05/27/99  *********************************
      *
     C                   CALL      'W0070R11'                           99
     C     *INKC         PARM      *ZERO         ##KC              1
     C     *INKL         PARM      *ZERO         ##KL              1
     C***  BEGIN CHG  ***  05/27/99  *********************************
     C**********           PARM 'C'       ##MODE  1
     C                   PARM                    ##MODE            1
     C***  END   CHG  ***  05/27/99  *********************************
     C                   PARM                    ##RRN             9 0
      *
     C     *INKC         IFEQ      *ON
     C***  BEGIN ADD  ***  05/27/99  *********************************
     C                   MOVE      'A'           QQRTCD
     C***  END   ADD  ***  05/27/99  *********************************
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVE      SVRECF        Q2RECF
     C                   GOTO      TOP
      *
     C                   ENDIF
     C*** END ADD *** 09/05/98 ****************************************
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
     C*** Begin add *** 04/01/05 **************************************
     C*  The Indemnity only denied case flag must be 'Y' or blank.
     C*
     C     SC1A6         IFNE      *BLANK
     C     SC1A6         ANDNE     'Y'
     C                   MOVE      '1'           *IN71
     C                   GOTO      STRT
     C                   ENDIF
     C*** End add ***** 04/01/05 **************************************
     C*
     C*  MED ONLY TO LOST TIME CASE FLAG MUST BE 'Y' OR BLANK.
     C*
     C     SC1A3         IFNE      *BLANK
     C     SC1A3         ANDNE     'Y'
     C                   MOVE      '1'           *IN72
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*** Begin add *** 04/01/05 **************************************
     C*  The 8th day of disability dates must be valid, if entered.
     C*
     C     SCDT1         IFNE      0
     C                   Z-ADD     SCDT1         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT1
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN77
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCDT2         IFNE      0
     C                   Z-ADD     SCDT2         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT2
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN78
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*** End add ***** 04/01/05 **************************************
     C*
     C*  LOST TIME CASE FLAG MUST BE 'Y' OR BLANK.
     C*
     C     SC1A5         IFNE      *BLANK
     C     SC1A5         ANDNE     'Y'
     C                   MOVE      '1'           *IN73
     C                   GOTO      STRT
     C                   END
     C*
     C*** Begin add *** 04/01/05 **************************************
     C*  The date first payment mailed must be valid, if entered.
     C*
     C     SCDT4         IFNE      0
     C                   Z-ADD     SCDT4         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT4
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN82
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*** End add ***** 04/01/05 **************************************
     C*
     C*  CASE DENIED, MED ONLY TO LT, OR LOST TIME FLAG MUST BE ENTERED.
     C*   IF CALLED FROM ONE OF THE PAYMENTS PGMS.
     C*
     C     *IN08         IFEQ      '1'
     C     SC1A2         ANDNE     'Y'
     C     SC1A3         ANDNE     'Y'
     C     SC1A5         ANDNE     'Y'
     C*** Beg add *** 09/15/22 **************************************
     C     SC1A6         ANDNE     'Y'
     C*** End add *** 09/15/22 **************************************
     C                   MOVE      '1'           *IN74
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  "MED ONLY TO LOST TIME" AND LOST TIME FLAGS ARE MUTUALLY EXCLUSIVE.
     C*
     C     SC1A3         IFEQ      'Y'
     C     SC1A5         ANDEQ     'Y'
     C                   MOVE      '1'           *IN75
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*** Begin dlt *** 04/01/05 **************************************
     C*  CANNOT SELECT CASE DENIED IF ONE OF THESE DATES ARE ENTERED:
     C*  FIRST DISABILITY, FIRST PAYMENT MAILED, SALARY END.
     C*
     C**         SCDT1     IFNE *ZERO                      1ST DISABILITY DATE
     C**         SCDT4     ORNE *ZERO                      1ST PYMT MAILED DATE
     C**         SCDT2     ORNE *ZERO                      SALARY END DATE
     C**         SC1A2     IFEQ 'Y'
     C**                   MOVE '1'       *IN76
     C**                   GOTO STRT
     C**                   ENDIF
     C**                   ENDIF
     C*
     C*  IF EITHER "MED ONLY TO LT" OR "LOST TIME" ENTERED, AND THE FULL
     C*  SALARY FLAG IS NOT 'Y', THE 1ST DATE OF DISABILITY IS REQUIRED.
     C*
     C**         SC1A3     IFEQ 'Y'
     C**         SC1A5     OREQ 'Y'
     C**         SCDT1     IFEQ *ZERO
     C**         SC1A4     ANDNE'Y'
     C*** BEGIN DLT *** 09/23/99 **************************************
     C*******************  MOVE '1'       *IN77
     C*******************  GOTO STRT
     C*** END DLT ***** 09/23/99 **************************************
     C**                   ENDIF
     C**                   ENDIF
     C*
     C*  FIRST DATE OF DISABILITY MUST BE VALID IF ENTERED.
     C*
     C**         SCDT1     IFNE *ZERO
     C*** BEGIN ADD *** 09/05/98 **************************************
     C**         *IN90     OREQ *ON
     C*** END ADD *** 09/05/98 ****************************************
     C**                   Z-ADDSCDT1     SRDTI
     C**                   CALL 'SRCVTDAT'DATCHK
     C**                   MOVELSRDTO     SCDT1
     C**         SRERR     IFEQ '1'
     C*** BEGIN DLT *** 09/23/99 **************************************
     C*******************  MOVE '1'       *IN78
     C*******************  GOTO STRT
     C*** END DLT ***** 09/23/99 **************************************
     C**                   ENDIF
     C**                   ENDIF
     C*
     C*  FULL SALARY FLAG MUST BE 'Y' OR BLANK.
     C*
     C**         SC1A4     IFNE *BLANK
     C**         SC1A4     ANDNE'Y'
     C**         SC1A4     ANDNE'N'
     C*** BEGIN DLT *** 09/23/99 **************************************
     C*******************  MOVE '1'       *IN79
     C*******************  GOTO STRT
     C*** END DLT ***** 09/23/99 **************************************
     C**                   ENDIF
     C*
     C*  IF THE SALARY END DATE IS NOT 0, THE FULL SALARY FLAG MUST BE 'Y'.
     C*
     C**         SCDT2     IFNE 0
     C**         SC1A4     ANDNE'Y'
     C*** BEGIN DLT *** 09/23/99 **************************************
     C*******************  MOVE '1'       *IN80
     C*******************  GOTO STRT
     C*** END DLT ***** 09/23/99 **************************************
     C**                   ENDIF
     C*
     C*  SALARY END DATE MUST BE VALID, IF ENTERED.
     C*
     C**         SCDT2     IFNE 0
     C**                   Z-ADDSCDT2     SRDTI
     C**                   CALL 'SRCVTDAT'DATCHK
     C**                   MOVELSRDTO     SCDT2
     C**         SRERR     IFEQ '1'
     C*** BEGIN DLT *** 09/23/99 **************************************
     C*******************  MOVE '1'       *IN81
     C*******************  GOTO STRT
     C*** END DLT ***** 09/23/99 **************************************
     C**                   ENDIF
     C**                   ENDIF
     C*** End dlt ***** 04/01/05 **************************************
     C*
     C*  IF EITHER "MED ONLY TO LT" OR "LOST TIME" ENTERED, AND THE FULL
     C*  SALARY FLAG IS NOT 'Y', THE 1ST PMT MAILED DATE IS REQUIRED.
     C*
     C     SC1A3         IFEQ      'Y'
     C     SC1A5         OREQ      'Y'
     C*** Begin chg *** 04/01/05 **************************************
     C********   SC1A4     IFNE 'Y'
     C     CMFSAL        IFNE      'Y'
     C*** End chg ***** 04/01/05 **************************************
     C                   Z-ADD     SCDT4         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT4
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN82
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*** BEGIN CHG *** 09/05/98 **************************************
     C* IF CALL  = 'C' THE DATE FIRST COMP MAILED IS REQUIRED
     C*
     C     QQCALL        IFEQ      'C'
     C                   Z-ADD     SCDT4         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT4
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN82
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*** Begin dlt *** 04/01/05 **************************************
     C*  AWW CANNOT BE LESS THAN OR EQUAL TO 0, IF THE MED ONLY TO
     C*  LOST TIME FLAG IS 'Y' OR LOST TIME FLAG IS 'Y'.
     C*
     C**         *IN90     IFEQ *ON
     C**         SC1A3     IFEQ 'Y'
     C**         SC7N1     ANDLE0
     C**         SC1A5     OREQ 'Y'
     C**         SC7N1     ANDLE0
     C*** BEGIN DLT *** 09/23/99 **************************************
     C*******************  MOVE '1'       *IN83
     C*******************  GOTO STRT
     C*** END DLT ***** 09/23/99 **************************************
     C**                   ENDIF
     C*
     C*  COMP RATE CANNOT BE LESS THAN OR EQUAL TO 0, IF THE MED ONLY TO
     C*  LOST TIME FLAG IS 'Y' OR LOST TIME FLAG IS 'Y'.
     C*
     C**         SC1A3     IFEQ 'Y'
     C**         SC7N2     ANDLE0
     C**         SC1A5     OREQ 'Y'
     C**         SC7N2     ANDLE0
     C*** BEGIN DLT *** 09/23/99 **************************************
     C*******************  MOVE '1'       *IN84
     C*******************  GOTO STRT
     C*** END DLT ***** 09/23/99 **************************************
     C**                   ENDIF
     C**                   ENDIF
     C*** END CHG *** 09/05/98 ****************************************
     C*** End dlt ***** 04/01/05 **************************************
     C*
     C*  DISABILITY TYPE MUST BE VALID, IF THE MED ONLY TO
     C*  LOST TIME FLAG IS 'Y' OR LOST TIME FLAG IS 'Y'.
     C*
     C*** Begin chg *** 04/01/05 **************************************
     C********   SC1A4     IFNE 'Y'
     C     CMFSAL        IFNE      'Y'
     C*** End chg ***** 04/01/05 **************************************
     C     SC1A3         IFEQ      'Y'
     C     SC1A5         OREQ      'Y'
     C     SCCD1         IFNE      'TT'
     C     SCCD1         ANDNE     '80'
     C     SCCD1         ANDNE     'TP'
     C     SCCD1         ANDNE     'IB'
     C     SCCD1         ANDNE     'PT'
     C     SCCD1         ANDNE     'DT'
     C*** Begin add *** 12/21/05 **************************************
     C     SCCD1         ANDNE     'SO'
     C*** End add ***** 12/21/05 **************************************
     C                   MOVE      '1'           *IN85
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*  UPDATE OR WRITE THE STATE INFORMATION RECORD
     C*
     C     NIKY          CHAIN     PNI01                              40
     C*
     C                   MOVE      XXCO#         NICO#
     C                   MOVE      XXFND         NIFND
     C                   MOVE      XXFYR         NIFYR
     C                   MOVE      XXCASE        NICASE
     C                   MOVE      SC1A2         NI1A2
     C                   MOVE      SC1A3         NI1A3
     C*** Begin dlt *** 04/01/05 **************************************
     C**                   MOVE SC1A4     NI1A4
     C*** End dlt ***** 04/01/05 **************************************
     C                   MOVE      SC1A5         NI1A5
     C                   Z-ADD     SCDT1         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NIDT1
     C                   Z-ADD     SCDT2         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NIDT2
     C                   Z-ADD     SCDT4         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NIDT4
     C*** Begin dlt *** 04/01/05 **************************************
     C**                   MOVE SC7N1     NI10A2
     C**                   MOVE SC7N2     NI10A3
     C*** End dlt ***** 04/01/05 **************************************
     C                   MOVEL     SCCD1         NI10A1
     C                   MOVEL     SC40A1        NI40A1
     C                   MOVEL     SC40A2        NI40A2
     C                   MOVE      CMJRST        NIST                           ADD 05/18/00
     C                   MOVE      Q1USER        NIUSER
     C                   MOVE      Q1PGM         NIPGM
     C                   Z-ADD     DSTIME        NITIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NIDT
     C*** Begin add *** 04/01/05 **************************************
     C                   MOVE      SC1A6         NI1A6
     C                   Z-ADD     SC9N1         NI9N1
     C                   Z-ADD     SC9N2         NI9N2
     C*** End add ***** 04/01/05 **************************************
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
     C*  Load the entry fields
     C*
     C                   MOVE      CMDNY         SC1A2                          CLAIM DENIED
     C*** Begin dlt *** 04/01/05 **************************************
     C**                   MOVE CMFSAL    SC1A4            FULL SALARY
     C**                   Z-ADDCMDBDT    SRDTI
     C**                   CALL 'SRCVTDAT'DTMDYY
     C**                   MOVELSRDTO     SCDT1            DISABILITY DATE
     C**                   Z-ADDCMFSDT    SRDTI
     C**                   CALL 'SRCVTDAT'DTMDYY
     C**                   MOVELSRDTO     SCDT2            SALARY END DATE
     C**                   Z-ADDCMAWW     SC7N1            AWW
     C**                   Z-ADDCMCRT     SC7N2            COMP RATE
     C*** End dlt ***** 04/01/05 **************************************
     C*
     C     NIKY          CHAIN     PNI01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      NI1A3         SC1A3
     C                   MOVE      NI1A5         SC1A5
     C                   Z-ADD     NIDT4         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT4
     C                   MOVEL     NI10A1        SCCD1
     C                   MOVEL     NI40A1        SC40A1
     C                   MOVEL     NI40A2        SC40A2
     C*** Begin add *** 04/01/05 **************************************
     C                   MOVEL     NI1A6         SC1A6
     C                   Z-ADD     NIDT1         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT1
     C                   Z-ADD     NIDT2         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT2
     C                   Z-ADD     NI9N1         SC9N1
     C                   Z-ADD     NI9N2         SC9N2
     C*** End add ***** 04/01/05 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANK        SC1A2
     C                   MOVE      *BLANK        SC1A5
     C                   MOVE      *ZEROS        SCDT4
     C                   MOVE      *BLANKS       SCCD1                          DISABILITY TYPE
     C                   MOVE      *BLANKS       SC40A1                         REMARKS
     C                   MOVE      *BLANKS       SC40A2                         REMARKS
     C*** Begin add *** 04/01/05 **************************************
     C                   MOVE      *BLANKS       SC1A6
     C                   MOVE      *ZEROS        SCDT1
     C                   MOVE      *ZEROS        SCDT2
     C                   Z-ADD     0             SC9N1
     C                   Z-ADD     0             SC9N2
     C*** End add ***** 04/01/05 **************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C**************************************************************************
