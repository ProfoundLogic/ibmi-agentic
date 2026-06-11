     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE E0030R - Maintenance - Weighting & Ballast Values
     H**
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PROVIDES MAINTENANCE FOR THE WEIGHTING     **
     H**     AND BALLAST VALUES THAT ARE USED IN CALCULATING         **
     H**     EMPLOYER EXPERIENCE MODIFICATION.                       **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     MENU                                                    **
     H**   CALLS TO:                                                 **
     H**     E0530R - Search of Weighting & Ballast Value File       **
     H**   PROGRAMMER: Lori J. Quinn                                 **
     H**   CREATED:    12/20/90     PROJECT: EXP MOD                 **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** MODIFICATIONS:                                              **
     H**                                                             **
     H**  8/12/98  C:90  PROJ:WC853                                  **
     H**                 MODIFIED TO VERSION 3 STANDARDS             **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**      09      Inquiry Only                                   **
     H**      40      General Usage (CHAIN)                          **
     H**      KC      Command 03 - EXIT                              **
     H**      KL      Command 12 - CANCEL                            **
     H**      KE      Command 05 - Search Expected Losses (Screen 1) **
     H**      KM      Command 13 - Change User Parms (Screen 1)      **
     H**  SCR01:                                                     **
     H**      20      Expected Losses < 0 (MSGID = ECR0547)          **
     H**              ECR0547 = Value cannot be less than zero.      **
     H**      21      No Record found in INQUIRY mode (ECR0505)      **
     H**              ECR0505 = Record not found for this information**
     H**  SCR02:                                                     **
     H**      20      Weighting Value < 0 (MSGID = ECR0547)          **
     H**              ECR0547 = Value cannot be less than zero.      **
     H**      21      Weighting Value < 0 (MSGID = ECR0547)          **
     H**              ECR0547 = Value cannot be less than zero.      **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    E0030F:  Screen Display
     H**    SMCOL (PCO01): CO                                        **
     H**    EMUPL (PUP01): USER                                      **
     H**    EMWBL (PWB01): CO/LOSS                                   **
     H*****************************************************************
     H*
     F* Display Screen File
     FE0030F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F* User Parameter Master File
     FEMUPL     IF   E           K DISK
     F* Weighting and Ballast Values Master File
     FEMWBL     UF A E           K DISK
     F*

     D* Table for Update/Add Mode (Displayed on Screen)
     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D*
     D* System Date and Time Data Structure
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* Screen Data Structure
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D* External Program Data Structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Define Work Fields
     C*
     C     *LIKE         DEFINE    COCO#         XXCO#
     C     *LIKE         DEFINE    WBLOSS        XXLOSS
     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C* Key List for the User Parameter Master FIle
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* Key List for the Weighting and Ballast Values Master File
     C*
     C     WBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXLOSS
     C*
     C* Program Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C* Initial and Set-Up
     C*
     C                   TIME                    DSDTTM
     C******************   MOVE Q1USER    SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C* If QQCALL = '1' then this is Inquiry Only
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C* Retrieve the User Parameter Record
     C*
     C     TOP           TAG
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C* Retrieve the Company Master Record
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*
     C* Load the Screen Format for the FIrst Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C* Display All Screen Record Formats
     C*
     C     STRT          TAG
     C*
     C* Execute Screen Format 1
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C* Execute Screen Format 2
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C*  Process All Command Keys
     C*
     C     *IN11         IFEQ      '1'
     C*
     C* COMMAND 03 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C* COMMAND 12 = CANCEL
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C* COMMAND 05 = Seach Expected Losses
     C*
     C     *INKE         IFEQ      '1'
     C                   CALL      'E0530R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCLOSS
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   WRITE     SCR99
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END
     C*
     C* Process Help Requests
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C* COMMAND 13 - Change User Parameters
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'E0006R'
     C                   WRITE     SCR99
     C*
     C* Retrieve User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C* COMMAND 20 = Change Status
     C*
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C                   END
     C*
     C* GOTO Appropiate Screen
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C* Process Requested Screen
     C*
     C     SCR01T        TAG
     C*
     C*  Expected Losses Cannot be < Zero.
     C*
     C     SCLOSS        IFLT      *ZERO
     C                   MOVE      '1'           *IN20                          ECR0547 Err
     C                   GOTO      STRT
     C                   END
     C*
     C* Retrieve Requested Record, Load Screen 2 if Record Exists
     C*
     C                   Z-ADD     SCLOSS        XXLOSS
     C     WBKEY         CHAIN     EMWBL                              40
     C     *IN40         IFEQ      '0'                                          Record Exists
     C*
     C                   MOVEL     UA(2)         SCMODE
     C                   MOVE      WBWEIG        SCWEIG
     C                   MOVE      WBBALL        SCBALL
     C                   MOVEL     WBSTAT        SCSTAT
     C*
     C                   ELSE                                                   New Record
     C*
     C* If no record is found while in Inquiry mode - send error msg
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   Z-ADD     0             SCWEIG
     C                   Z-ADD     0             SCBALL
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
     C* We've passed edit for Screen 1 - Continue to next screen
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C* Weighting value cannot < 0.
     C*
     C     SCWEIG        IFLT      0
     C                   MOVE      '1'           *IN20                          ECR0547 Err
     C                   GOTO      STRT
     C                   END
     C*
     C* Ballast value cannot be < 0.
     C*
     C     SCBALL        IFLT      0
     C                   MOVE      '1'           *IN21                          ECR0547 Err
     C                   GOTO      STRT
     C                   END
     C*
     C* We've Passed all edits on Screen 2: Write/Update Record and
     C*   return to Screen 1.
     C*
     C                   Z-ADD     UPCO#         WBCO#
     C                   Z-ADD     SCLOSS        WBLOSS
     C                   Z-ADD     SCWEIG        WBWEIG
     C                   Z-ADD     SCBALL        WBBALL
     C                   MOVE      SCSTAT        WBSTAT
     C                   MOVE      Q1USER        WBUSER
     C                   MOVE      Q1PGM         WBPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WBDT
     C*
     C                   Z-ADD     DSTIME        WBTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PWB01
     C                   ELSE
     C                   MOVE      Q1USER        WBIUSR                         Initial User
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WBIDT
     C*
     C                   WRITE     PWB01
     C                   END
     C*
     C* Processed Record, Return to Screen 1.
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     0             SCLOSS
     C                   GOTO      STRT
     C*
     C* End the Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  TABLE UA
-- ADD --
- UPDATE -
