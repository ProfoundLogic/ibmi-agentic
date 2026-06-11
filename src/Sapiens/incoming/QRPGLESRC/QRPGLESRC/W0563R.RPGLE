     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE F0509R - SS# MATCHING SEARCH                             *
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H* DESCRIPTION:  THIS PROGRAM DISPLAYS A LIST OF ALL CLAIMS FOR  *
     H*               A SPECIFIC SOCIAL SECURITY NUMBER, WHICH IS     *
     H*               PASSED INTO THIS PROGRAM FROM THE CALLING PGM.  *
     H*                                                               *
     H* CALLED FROM:  W0070R                                          *
     H* CALLS TO:     NONE                                            *
     H* PROJECT:  WC315         C:15         01/11/95                 *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H*  08/22/07 C:33  Project: 114WMF                               *
     H*                 Back to Base.                                 *
     H*       01/05/04  Project: 114WGN                               *
     H*                 Add display for state claim #.                *
     H*                                                               *
     H*  11/29/00  C:15  Project: WC577                               *
     H*                  Fixed the incoming date for positioning the  *
     H*                  search to the correct record.                *
     H*                                                               *
     H*  09/01/00  C:15  Project: WC577                               *
     H*                  Hardcoded the pgm name on the screen.        *
     H*                                                               *
     H*  03/16/98  C:15  Support# 10547                               *
     H*                  Chgd pgm to process selections different to  *
     H*                  handle a blank selection.                    *
     H*                                                               *
     H*  08/15/95  C:15  PROJECT# WC315                               *
     H*                  ADDED CODE TO LOAD '*' INTO QQRTCD WHEN NO   *
     H*                  SELECTION IS MADE.                           *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H*  *IN01  -                                                     *
     H*  *IN04  -  USED TO CLEAR THE SUBFILE                          *
     H*  *IN05  -  END OF FILE                                        *
     H*  *IN06  -                                                     *
     H*  *IN11  -  A COMMAND KEY WAS PRESSED                          *
     H*  *IN20  -  AN INVALID OPTION WAS SELECTED                     *
     H*  *IN32  -  NO RECORDS IN SUBFILE                              *
     H*  *IN50  -  SUBFILE WAS CHANGED                                *
     H*  *IN60  -  GENERAL USAGE                                      *
     H*****************************************************************
     FW0563F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(WDW01S:RRN2)
     F*
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMCMLA    IF   E           K DISK
     FWMCDL     IF   E           K DISK

      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
     D*
     D SS              S              1    DIM(11)
     D*** BEGIN ADD *** 09/01/00 **************************************
     D XXPRDS          C                   CONST('DUPLICATE SS# SEARC-
     D                                     H')
     D*** END ADD ***** 09/01/00 **************************************
     D***  Begin Add  ***  08/22/07  **************************************
     D CHGNUM          C                   CONST('WA003R')
     D***  End   Add  ***  08/22/07  **************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXSS#                  1      9
     D  XXSS#1                 1      3
     D  XXSS#2                 4      5
     D  XXSS#3                 6      9
      *** BEG ADD ***** 01/05/04 *************************************
     D V1              DS
     D  V1EMP#                 1      9
     D  V1DIV                 11     15
     D  V1ACDS                17     36
     D V2              DS
     D  V2ACL#                 1     25
      *** END ADD ***** 01/05/04 *************************************
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE COMPANY MASTER FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXSS#
     C*
     C     CMKY1         KLIST
     C                   KFLD                    XXSS#
     C                   KFLD                    XXACDT
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQSS#             9
     C                   PARM                    QQACDT            8
     C                   PARM                    QQFND             3 0
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQRTCD            1
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
     C***  Begin Add  ***  08/22/07  **************************************
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C***  End   Add  ***  08/22/07  **************************************
     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMACDT        XXACDT
     C*
     C                   MOVE      *BLANKS       QQRTCD
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      QQSS#         XXSS#
     C*** BEG ADD 09/01/00
     C     QQACDT        IFEQ      *HIVAL
     C*** BEGIN CHG *** 11/29/00 **************************************
     C******************** MOVE *HIVAL    X1ACDT
     C                   Z-ADD     12312099      X1ACDT
     C*** END CHG ***** 11/29/00 **************************************
     C                   ELSE
     C*** END ADD 09/01/00
     C                   MOVE      QQACDT        X1ACDT            8 0
     C                   ENDIF                                                  ADD 09/01/00
     C*
     C*** BEGIN ADD *** 11/29/00 **************************************
     C                   Z-ADD     X1ACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         X1ACDT
     C*** END ADD ***** 11/29/00 **************************************
     C*
     C                   Z-ADD     X1ACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C*
     C*** BEGIN ADD *** 09/01/00 **************************************
     C*  Use the hardcoded pgm name and center it for display.
     C*
     C                   MOVEL(P)  XXPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
     C*** END ADD ***** 09/01/00 **************************************
     C*
     C***  Begin Add  ***  08/22/07  **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C     CLTID         COMP      '114'                                  14
     C***  End   Add  ***  08/22/07  **************************************
     C*
     C*  GET THE USER PARAMETER RECORD
     C*
     C                   MOVEL     Q1USER        SCUSER
     C     UPKY          CHAIN     PUP01                              60
     C  N60              Z-ADD     UPCO#         XXCO#
     C*
     C*  GET THE COMPANY NAME
     C*
     C                   MOVE      *BLANKS       SCCONM
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE CLAIMANT'S NAME
     C*
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVEL     CDLSN         XXLAST           56
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        SCCNAM
     C                   END
     C*
     C*  LOAD THE CLAIMANT'S SOCIAL SECURITY NUMBER
     C*
     C                   MOVEA     *BLANKS       SS
     C                   MOVEA     XXSS#1        SS(1)
     C                   MOVE      '-'           SS(4)
     C                   MOVEA     XXSS#2        SS(5)
     C                   MOVE      '-'           SS(7)
     C                   MOVEA     XXSS#3        SS(8)
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              MOVEA     SS            SCSS#
      /Free
       scSS# = DisplaySS#(MaskData('SSN':'XXSS#':XXSS#));
      /End-Free
      *****  End   Chg  ***  06/09/20  **********************
      *===============================================================
      * INITIALIZATION    AREA
      *
     C     TOP           TAG
      *** BEG ADD ***** 01/05/04 ************************************
     C                   Z-ADD     1             VIEW#             1 0
     C                   SETON                                        21
      *** END ADD ***** 01/05/04 ************************************
     C                   EXSR      INZSR
      *
      *===============================================================
      *
     C     MAINLN        TAG
      *
      *===============================================================
      * SCREEN EXECUTION AREA
      *
     C                   EXSR      SCREXC
      *
      *===============================================================
      * HANDLE F KEYS IF PRESSED
      *
     C     *IN11         CASEQ     '1'           FKEYS
     C                   END                                                      END *IN11
      *
      *===============================================================
     C*
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C*
     C     SCDATE        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C*
     C                   GOTO      TOP
     C                   END
     C*
     C                   Z-ADD     RRN2          SVRRN2            5 0
     C*
     C                   READC     WDW01S                                 50
     C*** BEGIN CHG *** 03/16/98 ***********************************************
     C********** *IN50     IFEQ '0'
     C     *IN50         DOWEQ     '0'
     C*** END CHG ***** 03/16/98 ***********************************************
     C*
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
     C     SCSEL         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     SCFND         QQFND
     C                   Z-ADD     SCFYR         QQFYR
     C                   Z-ADD     SCCASE        QQCASE
     C                   GOTO      TERM
     C                   ELSE                                                   ELSE EQ '1'
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   Z-ADD     RRN2          #@$2
     C                   UPDATE    WDW01S
     C                   MOVE      *BLANK        SCSEL
     C                   GOTO      MAINLN
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '1'
     C*** BEGIN CHG *** 03/16/98 ***********************************************
     C                   READC     WDW01S                                 50
     C*** END CHG ***** 03/16/98 ***********************************************
     C                   END                                                    END 0 *IN50
     C                   MOVE      '*'           QQRTCD
     C*
      *===============================================================
      * TERMINATION OF PROGRAM
      *
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
      *====================================================================*
      * * *   INZSR SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INZSR         BEGSR
     C*
     C     CMKY1         SETLL     PCM01                              60
     C*
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     WDW01C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C*
     C                   ENDSR
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        999           X                 5 0
     C     *IN05         CABEQ     '1'           LDOUT
     C*
     C     CMKY          READE     PCM01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   Z-ADD     CMFND         SCFND
     C                   Z-ADD     CMFYR         SCFYR
     C                   Z-ADD     CMCASE        SCCASE
     C                   Z-ADD     CMEMP#        SCEMP#
     C                   Z-ADD     CMDIV         SCDIV
     C                   MOVEL     CMACDS        SCACDS
     C                   MOVE      CMSTAT        SCSTAT
      *** BEG ADD ***** 01/05/04 **********************************
     C                   MOVEL     CMACL#        SCACL#
     C***  Begin Chg  ***  08/22/07  **************************************
     C**********           MOVE SCEMP#    V1EMP#    P
     C**********           MOVE SCDIV     V1DIV     P
     C                   MOVEL(P)  SCEMP#        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         V1EMP#
     C*
     C                   MOVEL(P)  SCDIV         XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         V1DIV
     C***  End   Chg  ***  08/22/07  **************************************
     C                   MOVE(P)   SCACDS        V1ACDS
     C                   MOVEL(P)  SCACL#        V2ACL#
      *
     C                   SELECT
     C     VIEW#         WHENEQ    1
     C                   MOVEL(P)  V1            SCDATA
     C     VIEW#         WHENEQ    2
     C                   MOVEL(P)  V2            SCDATA
     C                   ENDSL
      *** END ADD ***** 01/05/04 **********************************
      *
     C                   ADD       1             RRN2              5 0
     C                   MOVE      '0'           *IN06
     C                   WRITE     WDW01S
     C     NXTRCD        TAG
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INTIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     LDOUT         TAG
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   Z-ADD     0             SCACDT
     C                   Z-ADD     0             SCFND
     C                   Z-ADD     0             SCFYR
     C                   Z-ADD     0             SCCASE
     C                   Z-ADD     0             SCEMP#
     C                   Z-ADD     0             SCDIV
     C                   MOVE      *BLANKS       SCACDS
     C                   MOVE      *BLANKS       SCSTAT
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     WDW01S
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     1             #@$2
     C*
     C                   Z-ADD     0             SCDATE
     C*
     C                   ENDSR
     C/EJECT
      *===============================================================
     C     SCREXC        BEGSR
      *===============================================================
      *
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01C'
     C     Q2RECF        OREQ      'WDW01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     WDW01C
     C                   WRITE     WDW01
     C                   EXFMT     WDW01C
     C                   MOVE      '0'           *IN01
     C     SVRRN2        IFNE      0
     C                   Z-ADD     SVRRN2        RRN2
     C                   Z-ADD     0             SVRRN2
     C                   END
     C                   END
     C*
     C                   ENDSR
      *
     C/EJECT
      *===============================================================
     C     FKEYS         BEGSR
      *===============================================================
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKC
      *
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01S'
     C     Q2RECF        OREQ      'WDW01C'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   END                                                    END *INKL
      *** BEG ADD ***** 01/05/04 *******************************************
     C****************************************
     C*   F11 = CHANGE VIEW                  *
     C****************************************
     C     *INKK         IFEQ      *ON
     C     VIEW#         IFEQ      1
     C                   ADD       1             VIEW#
     C                   SETON                                        22
     C                   SETOFF                                       21
     C                   ELSE
     C                   SUB       1             VIEW#
     C                   SETON                                        21
     C                   SETOFF                                       22
     C                   ENDIF
      *
     C                   EXSR      INZSR
      *
     C                   ENDIF
      *** END ADD ***** 01/05/04 *******************************************
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           MAINLN                         NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      MAINLN
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C                   GOTO      MAINLN
      *
     C                   ENDSR
     C/EJECT
