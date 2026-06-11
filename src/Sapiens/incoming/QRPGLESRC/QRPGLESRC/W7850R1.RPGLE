     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7850R1-  REOPEN CLAIM HISTORY REPORT - PROMPT PGM
     H*****************************************************************
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM ALLOWS THE USER TO ENTER THE PARTICULAR    **
     H**     DATA: FUND, YR, NUMBER OF MONTHS AFTER CLOSURE          **
     H**     WITHIN A SPECIFIED DATE RANGE AND SELECTED CLAIM TYPE   **
     H**     (LT/NLT/BOTH)                                           **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     MENU                                                    **
     H**   CALLS TO:                                                 **
     H**     W7850C1                                                 **
     H**   PROGRAMMER: C:90                                          **
     H**   CREATED:   08/04/98      PROJECT: WC841                   **
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  05/15/01  C:136 PROJECT: WC577                               *
     H*                  Display location of output file.             *
     H*                                                               *
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      20      WCR0517 - The Fund YR is invalid               **
     H**      21      MCR0502 - # of months after closure-less than 0**
     H**      22      WCR0500 - Invalid Date in field.               **
     H**      23      MCR0500 - Invalid Date in field.               **
     H**      24      MCR0799 - Invalid selection (L/N/B).           **
     H**      26      MCR1183 - Invalid date range                   **
     H**      60      General Usage (CHAIN)                          **
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    SMCOL (PCO01): CO                                        **
     H**    WMUPL (PUP01): USER                                      **
     H**    WMFNL (PFN01): CO/FND                                    **
     H*****************************************************************
     F*
     F* Prompt Screen
     FW7850F1   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter Master File
     FWMUPL     IF   E           K DISK
     F*
     F* Fund Master File
     FWMFNL     IF   E           K DISK
     F*

     D*
     D***  BEGIN ADD  ***  05/15/01  *********************************
     D OPTSEL          C                   CONST(', if selected.')
     D***  END   ADD  ***  05/15/01  *********************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Key List for the Comapnay Master File
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
     C                   KFLD                    XXFND             3 0
     C*
     C* Key List for the Fund Detail File
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR             3 0
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
     C* Get the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*
     C* Get the Company Record
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C* Get the Requested Record, Load Information
     C*
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C* Load the Record Format Info for First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*** BEG ADD ******** 05/15/01 ********************************************
     C*
     C*   DISPLAY LOCATION OF OUTPUT FILE
     C*
     C                   CALL      'SA030R'
     C                   PARM                    QQSUFX            3
     C     QQSUFX        IFEQ      *BLANKS
     C     'PDSRPT'      CAT(P)    OPTSEL        XXRPTL           23
     C                   ELSE
     C     'PDSRPT'      CAT(P)    QQSUFX        XXRPTL
     C                   CAT(P)    OPTSEL:0      XXRPTL
     C                   ENDIF
     C                   MOVE      XXRPTL        SCRPTL
     C*
     C*** END ADD ******** 05/15/01 ********************************************
     C*
     C* Display All Formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C* Process All Command Keys
     C*
     C     *IN11         IFEQ      '1'
     C*
     C* COMMAND 03 = End of Job
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
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
     C* Process Change of User Parameters
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C*
     C* Get the User Parameter Record
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
     C* Process Request Screen
     C*
     C     SCR01T        TAG
     C*
     C*
     C* Validate the Range Date Entered
     C*
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C* Validate the date range  must be ascending.
     C*
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDT             7 0
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT             7 0
     C     XXFDT         IFGT      XXTDT
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C* Validate the # of Months after closure
     C*
     C     SCMCL         IFNE      0
     C     SCMCL         IFLT      0
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* Validate the Claim type selection
     C*
     C     SCROPT        IFNE      'L'
     C     SCROPT        ANDNE     'N'
     C     SCROPT        ANDNE     'B'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE REPORT SELECTION
     C*
     C     SCRPT         IFEQ      *BLANK
     C                   MOVE      *ON           *IN33
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     SCRPT         IFNE      'P'                                          PRINT
     C     SCRPT         ANDNE     'F'                                          FILE
     C     SCRPT         ANDNE     'B'                                          BOTH
     C                   MOVE      *ON           *IN30
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  IFEQ SCRPT
     C*
     C*
     C* Call the CLP to Submit the List
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   MOVE      SCMCL         QQMCL             3
     C                   MOVE      SCROPT        QQROPT            1
     C                   MOVE      SCFDT         QQFDT             8
     C                   MOVE      SCTDT         QQTDT             8
     C                   MOVE      SCRPT         QQRPT             1
     C*
     C                   CALL      'W7850C1'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQOUTQ
     C                   PARM                    QQMCL
     C                   PARM                    QQROPT
     C                   PARM                    QQFDT
     C                   PARM                    QQTDT
     C                   PARM                    QQRPT
     C*
     C*  End the Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
