     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**
     H/TITLE E7055R - EMP EXP MOD - CALC EXP MOD - PROMPT
     H**
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PROVIDES PROMPT SCREEN FOR CALCULATING     **
     H**     EXPERIENCE MODIFIER FOR EMPLOYER.                       **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     MENU                                                    **
     H**   CALLS TO:                                                 **
     H**     E7055C                                                  **
     H**   PROGRAMMER: Lori J. Quinn                                 **
     H**   CREATED:    02/21/91     PROJECT: EXP MOD                 **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** MODIFICATIONS:                                              **
     H**                                                             **
     H** 02/13/14  C:279 Redmine: 3383                               **
     H**           Modified to correct 'Print after calculation'     **
     H**           checkbox edit.                                    **
     H**                                                             **
     H** 09/02/98  C:90  PROJ: WC853                                 **
     H**           MODIFIED TO VERSION 3 STANDARDS                   **
     H**                                                             **
     H** 08/12/98  C:90  PROJ: WC853                                 **
     H**           MODIFIED TO VERSION 3 STANDARDS                   **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**      40      General Usage (CHAIN)                          **
     H**      KC      Command 01 - EOJ                               **
     H**      KE      Command 05 - Search Exp        (Screen 1)      **
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
     H**      24      Print flag must be Y or N. (ECR0501)           **
     H**              ECR0502 = Value must be "Y" or "N".            **
     H**      25-33   Batch number is not valid for employer (ECR0513)*
     H**              ECR0513 = Batch # is invalid for selected emp. **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    E7055F:  Screen Display                                  **
     H**    SMCOL (PCO01): CO                                        **
     H**    EMUPL (PUP01): USER                                      **
     H**    EMFNL (PUP01): CO/FUND                                   **
     H**    EMEML (PEM01): CO/FUND/EMP#                              **
     H**    EMEBL (PEB01): CO/FUND/EMP#/BATCH #                      **
     H*****************************************************************
     H*
     F* Display Screen File
     FE7055F    CF   E             WORKSTN
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
     F*
     F* Employer Batch Maintenance
     FEMEBL     IF   E           K DISK

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
     C     *LIKE         DEFINE    EBYEAR        XXYEAR
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
     C* Key List for the Employer Batch Master File
     C*
     C     EBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXYEAR
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
      /free
       //******** beg add ******** 02/13/14 *************************
       If scPrnt = ' ';
          scPrnt = 'N';
       Endif;
       //******** end add ******** 02/13/14 *************************
      /end-free
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
     C* COMMAND 10 = SEARCH EXPERIENCE MOD FILE
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C     HFLD          IFEQ      'SCFND'
     C     HFLD          OREQ      'SCEMP#'
     C     HFLD          OREQ      'SCEFDT'
     C                   CALL      'E0555R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQRTC             1
     C                   PARM                    QQCO#             3
     C**** BEGIN CHG **** 09/02/98 ************************************
     C******************** PARM           QQFND   30
     C******************** PARM           QQEMP#  90
     C******************** PARM           QQEFDT  80
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@EFDT            8 0
     C**** END   CHG **** 09/02/98 ************************************
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C**** BEGIN CHG **** 09/02/98 ************************************
     C********** QQFND     OREQ *ZEROS
     C     @@FND         OREQ      *ZEROS
     C**** END   CHG **** 09/02/98 ************************************
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     HFLD          IFEQ      'SCFND'
     C                   MOVE      *ON           *IN34
     C                   ENDIF                                                  HFLD SCFND
     C     HFLD          IFEQ      'SCEMP#'
     C                   MOVE      *ON           *IN35
     C                   ENDIF                                                  HFLD SCEMP#
     C     HFLD          IFEQ      'SCEFDT'
     C                   MOVE      *ON           *IN36
     C                   ENDIF                                                  HFLD SCEFDT
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   WRITE     SCR99
     C     HFLD          IFEQ      'SCFND'
     C                   MOVE      *ON           *IN35
     C                   ENDIF                                                  HFLD SCFND
     C     HFLD          IFEQ      'SCEMP#'
     C                   MOVE      *ON           *IN36
     C                   ENDIF                                                  HFLD SCEMP#
     C     HFLD          IFEQ      'SCEFDT'
     C                   MOVE      *ON           *IN37
     C                   ENDIF                                                  HFLD SCEFDT
     C*
     C                   ENDIF                                                  HFLD
     C*
     C                   MOVE      *ON           *IN90
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  Q2RECF
     C*
     C                   ENDIF                                                  *INKJ
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
     C*
     C* Retrieve the Employer Batch Record, validate batch # if entered
     C*
     C     SCYR1         IFNE      0
     C                   Z-ADD     SCFND         XXFND
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCYR1         XXYEAR
     C     EBKEY         CHAIN     PEB01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C                   END                                                    scyr1 <> 0
     C*
     C     SCYR2         IFNE      0
     C                   Z-ADD     SCFND         XXFND
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCYR2         XXYEAR
     C     EBKEY         CHAIN     PEB01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C                   END                                                    scyr2 <> 0
     C*
     C     SCYR3         IFNE      0
     C                   Z-ADD     SCFND         XXFND
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCYR3         XXYEAR
     C     EBKEY         CHAIN     PEB01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C                   END                                                    scyr3 <> 0
     C*
     C     SCYR4         IFNE      0
     C                   Z-ADD     SCFND         XXFND
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCYR4         XXYEAR
     C     EBKEY         CHAIN     PEB01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C                   END                                                    scyr4 <> 0
     C*
     C     SCYR5         IFNE      0
     C                   Z-ADD     SCFND         XXFND
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCYR5         XXYEAR
     C     EBKEY         CHAIN     PEB01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C                   END                                                    scyr5 <> 0
     C*
     C     SCYR6         IFNE      0
     C                   Z-ADD     SCFND         XXFND
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCYR6         XXYEAR
     C     EBKEY         CHAIN     PEB01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C                   END                                                    scyr6 <> 0
     C*
     C     SCYR7         IFNE      0
     C                   Z-ADD     SCFND         XXFND
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCYR7         XXYEAR
     C     EBKEY         CHAIN     PEB01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C                   END                                                    scyr7 <> 0
     C*
     C     SCYR8         IFNE      0
     C                   Z-ADD     SCFND         XXFND
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCYR8         XXYEAR
     C     EBKEY         CHAIN     PEB01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C                   END                                                    scyr8 <> 0
     C*
     C     SCYR9         IFNE      0
     C                   Z-ADD     SCFND         XXFND
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCYR9         XXYEAR
     C     EBKEY         CHAIN     PEB01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C                   END                                                    scyr9 <> 0
     C*
     C                   END                                                    scemp# <> 0
     C*
     C*  Print Flag must be Y or N.
     C*
     C     SCPRNT        IFNE      'Y'
     C     SCPRNT        ANDNE     'N'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
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
     C**** BEGIN CHG **** 09/02/98 ************************************
     C******************** MOVE SCFND     QQFND   30
     C******************** MOVE SCEMP#    QQEMP#  90
     C******************** MOVE SCYR1     QQBAT1  30
     C******************** MOVE SCYR2     QQBAT2  30
     C******************** MOVE SCYR3     QQBAT3  30
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      SCYR1         QQBAT1            3
     C                   MOVE      SCYR2         QQBAT2            3
     C                   MOVE      SCYR3         QQBAT3            3
     C******************** MOVE SCYR4     QQBAT4  30
     C******************** MOVE SCYR5     QQBAT5  30
     C******************** MOVE SCYR6     QQBAT6  30
     C******************** MOVE SCYR7     QQBAT7  30
     C******************** MOVE SCYR8     QQBAT8  30
     C******************** MOVE SCYR9     QQBAT9  30
     C******************** MOVE SCEFDT    QQEFDT  80
     C******************** MOVE SCRNDT    QQRNDT  80
     C                   MOVE      SCYR4         QQBAT4            3
     C                   MOVE      SCYR5         QQBAT5            3
     C                   MOVE      SCYR6         QQBAT6            3
     C                   MOVE      SCYR7         QQBAT7            3
     C                   MOVE      SCYR8         QQBAT8            3
     C                   MOVE      SCYR9         QQBAT9            3
     C                   MOVE      SCEFDT        QQEFDT            8
     C                   MOVE      SCRNDT        QQRNDT            8
     C**** END   CHG **** 09/02/98 ************************************
     C                   MOVE      SCPRNT        QQPRNT            1
     C                   CALL      'E7055C'
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
     C                   PARM                    QQPRNT
     C                   PARM                    UPOUTQ
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
