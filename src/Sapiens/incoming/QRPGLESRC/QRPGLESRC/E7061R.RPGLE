     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**
     H/TITLE E7061R - EMP EXP MOD - PRINT EXP MOD - PROMPT
     H**
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PROVIDES PROMPT SCREEN FOR PRINTING        **
     H**     EXPERIENCE MODIFIERS FOR EMPLOYERS                      **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     MENU                                                    **
     H**   CALLS TO:                                                 **
     H**     E7061C                                                  **
     H**   PROGRAMMER: Lori J. Quinn                                 **
     H**   CREATED:    03/01/91     PROJECT: EXP MOD                 **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**      40      General Usage (CHAIN)                          **
     H**      KC      Command 03 - EOJ                               **
     H**      KM      Command 13 - Change User Parms (Screen 1)      **
     H**  SCR01:                                                     **
     H**      20      Fund number is invalid (ECR0508)               **
     H**              ECR0508 = Invalid fund number.                 **
     H**      21      No Record found for Employer (ECR0505)         **
     H**              ECR0505 = Record not found for this information**
     H**      22      Effective date is invalid (ECR0500)            **
     H**              ECR0500 = Invalid date in field.               **
     H**      23      Run date is invalid (ECR0500)                  **
     H**              ECR0500 = Invalid date in field.               **
     H**      24      No Record found for Employer #2 (ECR0505)      **
     H**              ECR0505 = Record not found for this information**
     H**      25      Employer numbers must be ascending (ECR0514)   **
     H**              ECR0514 = Employer Numbers must be ascending   **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    E7061F:  Screen Display                                  **
     H**    SMCOL (PCO01): CO                                        **
     H**    EMUPL (PUP01): USER                                      **
     H**    EMFNL (PUP01): CO/FUND                                   **
     H**    EMEML (PEM01): CO/FUND/EMP#                              **
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/29/98  C:35   PROJ: SUPPORT WA071 13936                    *
     H*                  CHANGED SIZE OF EMP# PARAMETER TO VERSION 3  *
     H*                  STANDARD.                                    *
     H*                                                               *
     H* 08/12/98  C:90   PROJ: WC853                                  *
     H*                  MODIFIED TO VERSION 3 STANDARDS              *
     H*                                                               *
     H*****************************************************************
     H*
     F* Display Screen File
     FE7061F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter Master File
     FEMUPL     IF   E           K DISK
     F*
     F* Fund Master File
     FEMFNL     IF   E           K DISK
     F*
     F* Employer Master Maintenance
     FEMEML     IF   E           K DISK

     D*
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
     C     *LIKE         DEFINE    FNFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* Key List for the Fund Master File
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C*
     C* Key List for the Employer Master File
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C* Entry Parameters
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
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* Retrieve the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C* Retrieve the Company Record
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C******************   MOVE Q1USER    SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCRNDT
     C*
     C* Load the Record Format Information for the First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C* Display All Screen Formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C* Process all Command Keys
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C* COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C* COMMAND HELP - PROCESS HELP REQUESTS
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
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C* COMMAND 13 - PROCESS CHANGE OF USER PARAMETERS
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
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C* Goto the Appropriate Screen
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C* Process the Requested Screen
     C*
     C     SCR01T        TAG
     C*
     C* Retrieve the Fund Record, Validate fund record
     C*
     C                   Z-ADD     SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              40
     C     *IN40         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C* Retrieve the Employer Record, Validate emp# if emp is entered
     C*
     C     SCEMP#        IFNE      0
     C                   Z-ADD     SCFND         XXFND
     C                   Z-ADD     SCEMP#        XXEMP#
     C     EMKEY         CHAIN     PEM01                              40
     C     *IN40         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* Retrieve the Emp #2   Record, Validate emp# if emp is entered
     C*
     C     SCEMP2        IFNE      0
     C                   Z-ADD     SCFND         XXFND
     C                   Z-ADD     SCEMP2        XXEMP#
     C     EMKEY         CHAIN     PEM01                              40
     C     *IN40         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* EMPLOYER NUMBERS, IF SELECTED, MUST BE IN ACSENDING SEQUENCE
     C*
     C     SCEMP#        IFNE      0
     C     SCEMP2        ANDNE     0
     C     SCEMP#        IFGT      SCEMP2
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* Validate effective date
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C* Validate run date
     C*
     C                   Z-ADD     SCRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRNDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C* Passed Edits on Call Submit Program
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      *BLANKS       QQBAT1            3
     C                   MOVE      *BLANKS       QQBAT2            3
     C                   MOVE      *BLANKS       QQBAT3            3
     C                   MOVE      *BLANKS       QQBAT4            3
     C                   MOVE      *BLANKS       QQBAT5            3
     C                   MOVE      *BLANKS       QQBAT6            3
     C                   MOVE      *BLANKS       QQBAT7            3
     C                   MOVE      *BLANKS       QQBAT8            3
     C                   MOVE      *BLANKS       QQBAT9            3
     C                   MOVE      SCEFDT        QQEFDT            8
     C                   MOVE      SCRNDT        QQRNDT            8
      **** BEGIN CHG **** 09/29/98 *********************************************
     C******************** MOVE SCEMP2    QQEMP2  5
     C                   MOVE      SCEMP2        QQEMP2            9
      **** END   CHG **** 09/29/98 *********************************************
     C                   CALL      'E7061C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQEMP#
     C                   PARM                    QQBAT1
     C                   PARM                    QQBAT2
     C                   PARM                    QQBAT3
     C                   PARM                    QQBAT4
     C                   PARM                    QQBAT5
     C                   PARM                    QQBAT6
     C                   PARM                    QQBAT7
     C                   PARM                    QQBAT8
     C                   PARM                    QQBAT9
     C                   PARM                    QQEFDT
     C                   PARM                    QQRNDT
     C                   PARM                    UPOUTQ
     C                   PARM                    QQEMP2
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
