     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7030R2 - PROMPT FOR CHECK REGISTER DEMAND REPORT
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM ALLOWS THE USER TO REQUEST THE CHECK REG   **
     H**     REPORT FOR A PARTICUALR DATE RANGE, WITH OR WITHOUT     **
     H**     RECOVERIES, AND FOR SELECTION OF TYPE OF OUPUT:         **
     H**     PRINTED ONLY, FILE ONLY, OR BOTH.                       **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     MENU                                                    **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     W7030C2                                                 **
     H**                                                             **
     H**   PROGRAMMER: C:48                                          **
     H**   CREATED:    4/01/96      PROJECT: WC028                   **
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  04/06/04  C:58   PROJECT: WC1148
     H*     Add Group/non-group/Both selection parameter.
     H*
     H*  05/14/01  C:136 PROJECT: WC577                               *
     H*                  Display location of output file.             *
     H*                                                               *
     H*  03/30/00 C:118 PROJECT: WC577                                *
     H*                 MADE V3 DISPLAY FILE CHGS.                    *
     H*                                                               *
     H*  02/16/00 C:61 PROJECT WC577                                  *
     H*                Restrict access upon a combination             *
     H*                of user parm flags and Group                   *
     H*                restrictions.                                  *
     H*                                                               *
     H* 09/23/99 C:61  PROJECT 82WCG                                  *
     H*                Center titles.                                 *
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H*                                                               *
     H* SCR01                                                         *
     H**      20      The Fund is invalid.                           **
     H**   21 - 24    Invalid date or not in ascending order         **
     H**      25      Recoveries flag must be 'Y' or 'N'             **
     H**      26      Report format   must be 'P' or 'F'.            **
     H**      35      Check type must be 'G', 'N', or 'B'
     H**   27 - 29    File format edit.                              **
     H**   31 - 34    Position Indicator                             **
     H**      60      General Usage (CHAIN)                          **
     H*                                                               *
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    WMFNL (PFN01): CO/FND                                    **
     H**    SMCOL (PCO01): CO                                        **
     H**    WMUPL (PUP01): USER                                      **
     H*****************************************************************
     FW7030F2   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK

     D TST             S              1    DIM(10)                              TEST FILE NAME
     D*
     D***  BEGIN ADD  ***  05/14/01  *********************************
     D OPTSEL          C                   CONST(', if selected.')
     D***  END   ADD  ***  05/14/01  *********************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D**
     D XXCKOB          C                   CONST('CHKOBJ REPORT/')
     D**
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND             3 0
     C*
     C     *LIKE         DEFINE    FNDT          XXFRDT
     C     *LIKE         DEFINE    FNDT          XXTODT
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
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C                   MOVE      UPFND         SCFND
     C                   MOVE      'Y'           SCRCOV
     C                   MOVE      'P'           SCOTYP
     C*** BEGIN ADD ****** 04/06/04 ********************************************
     C                   MOVE      'B'           SCCKTY
     C*** END ADD ******** 04/06/04 ********************************************
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
      ***** BEG ADD ******* 02/16/00 **********************************
      *
      *  CHECK FOR USER RESTRICTIONS FUND / EMPLOYER
      *
     C                   EXSR      CKRSTR
      *
      ***** END ADD ******* 02/16/00 **********************************
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
      *
      ***** BEG ADD ******* 09/23/99 **********************************
      *
      * Center Screen Prompt in display field
      *
     C                   MOVEL(P)  SCPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
      *
      ***** END ADD ******* 09/23/99 **********************************
      *
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*** BEG ADD ******** 05/14/01 ********************************************
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
     C*** END ADD ******** 05/14/01 ********************************************
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      UPFND         SCFND
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  EDIT FUND
     C*
     C     SCFND         IFNE      *ZERO
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  EDIT THE FROM DATE
     C*
     C     SCFRDT        IFEQ      *ZERO
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C     SCFRDT        IFNE      *ZERO
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  EDIT THE TO DATE
     C*
     C     SCTODT        IFEQ      *ZERO
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C     SCTODT        IFNE      *ZERO
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  COMPARE THE FROM DATE WITH THE TO DATE.
     C*
     C     SCFRDT        IFNE      *ZERO
     C     SCTODT        ANDNE     *ZERO
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C     XXFRDT        IFGT      XXTODT
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  EDIT RECOVERIES SELECTION
     C*
     C     SCRCOV        IFNE      'Y'
     C     SCRCOV        ANDNE     'N'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*** BEGIN ADD ****** 04/06/04 ********************************************
     C*  CHECK TYPE MUST BE 'G', 'N' OR 'B'.
     C*
     C     SCCKTY        IFNE      'G'                                          PRINT ONLY
     C     SCCKTY        ANDNE     'N'                                          FILE ONLY
     C     SCCKTY        ANDNE     'B'                                          BOTH
     C                   MOVE      *ON           *IN35
     C                   GOTO      STRT
     C                   END
     C*** END ADD ******** 04/06/04 ********************************************
     C*
     C*  THE TYPE OF OUTPUT MUST BE 'P', 'F' OR 'B'.
     C*
     C     SCOTYP        IFNE      'P'                                          PRINT ONLY
     C     SCOTYP        ANDNE     'F'                                          FILE ONLY
     C     SCOTYP        ANDNE     'B'                                          BOTH
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   END
     C****************************************************************
     C****************************************************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         X@CO#
     C                   MOVE      SCFND         X@FND
     C                   MOVE      SCFRDT        X@DAT1            8
     C                   MOVE      SCTODT        X@DAT2            8
     C                   CALL      'W7030C2'
     C                   PARM                    X@CO#             3
     C                   PARM                    X@FND             3
     C                   PARM                    X@DAT1
     C                   PARM                    X@DAT2
     C                   PARM                    SCRCOV
     C*** BEGIN ADD ****** 04/06/04 ********************************************
     C                   PARM                    SCCKTY
     C*** END ADD ******** 04/06/04 ********************************************
     C                   PARM                    SCOTYP
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
      ***** BEG ADD ******* 02/16/00 **********************************
      *
      *
      *
      *****************************************************************
      * CKRSTR - Check if user restricted to Group
      *****************************************************************
     C     CKRSTR        BEGSR
      *
     C     UPXFND        IFEQ      'Y'
     C                   SETON                                        34
     C                   ENDIF
      *
      * IF Fund Restriction records SMAZP exist,
      *   allow entry of Group
      *
     C     *IN34         IFEQ      *ON
      *
     C                   CALL      'SA008R'
     C                   PARM                    UPCO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'W'           ##APPL            1
     C                   PARM      *OFF          ##RCDS            1
     C                   PARM      *BLANK        ##AUTH            1
      *
     C     ##RCDS        IFEQ      *ON
      *
      *    Restriction to Group to take place in report W7031R2
      *
     C                   SETOFF                                       34
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  *IN34
      *
     C                   ENDSR                                                  CKRSTR
      *
      *
      *
      ***** END ADD ******* 02/16/00 **********************************
