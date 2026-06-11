     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8102R - PROMPT FOR FUND MASTER/TABLE PURGE
     H****************************************************************
     H*                                                              *
     H*  PROGRAM DESCRIPTION:                                        *
     H*       THIS PROGRAM ALLOWS THE USER TO ENTER THE FUND TO BE   *
     H*       PURGED IN THE FUND MASTER/TABLE PURGE JOB.             *
     H*                                                              *
     H*  CALLED BY: MENU                                             *
     H*                                                              *
     H*  CALLS TO:  NONE                                             *
     H*                                                              *
     H*  PROGRAMMER: C:46                                            *
     H*  CREATED:    03/13/96    PROJECT: WC494                      *
     H****************************************************************
     H*                                                              *
     H* REVISIONS:                                                   *
     H*                                                              *
     H* 06/14/23  C340  Project: 114WRJ (CSI)                        *
     H*                 Add Validation for Group being Purge.        *
     H*                                                              *
     H*  07/31/01 C:135 Project SUPPORT WC577                        *
     H*           Rework the screen to standards. Added a goto at    *
     H*           the end of the function key processing where       *
     H*           the purge program would be called if an            *
     H*           invalid function key was pressed.                  *
     H*           Reinstated the validation for group number.        *
     H*                                                              *
     H*  04/06/01 C:61 PROJECT WC577                                 *
     H*                Chg "Fund" to "Group" on prompt screen.       *
     H*                                                              *
     H*  04/09/98 C:61 PROJECT 60APK                                 *
     H*                Remove edit of Fund Field.                    *
     H*                                                              *
     H*  PLEASE ADD TO TOP                                           *
     H****************************************************************
     H*                                                              *
     H* INDICATOR USAGE:                                             *
     H*  SCR01                                                       *
     H*  21 - INVALID FUND NUMBER                                     *
     H*                                                              *
     H****************************************************************
     FW8102F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      *** BEG UNDEL *** 07/31/01 **************************************
      ***** BEG DEL ******* 04/09/98 **********************************
     FWMFNL     IF   E           K DISK
      ***** END DEL ******* 04/09/98 **********************************
      *** END UNDEL *** 07/31/01 **************************************
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
     FWMCML1    IF   E           K DISK
     FWMEML     IF   E           K DISK
     *** End Add *** 06/14/23 *** 114WRJ ****************************

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
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND             3 0
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
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
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
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
      *
      ***** BEG ADD ******* 04/09/98 **********************************
      *
      * Center Company name in display field
      *
     C                   MOVEL(P)  SCCONM        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCCONM
      *
      * Center Screen Prompt in display field
      *
     C                   MOVEL(P)  SCPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
      *
      ***** END ADD ******* 04/09/98 **********************************
      *
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
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF
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
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
      *** BEG CHG *** 07/31/01 ****************************************
      ********    F5 = FUND # SEARCH
      *           F10 = FUND # SEARCH
     C********   *INKE     IFEQ '1'
     C     *INKJ         IFEQ      '1'
      *** END CHG *** 07/31/01 ****************************************
     C                   CALL      'W0010R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM      1             @@VIEW            1 0
     C*
     C                   MOVE      @@FND         SCFND
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     SCFND         OREQ      *ZERO
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ELSE
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
      *** BEG DEL *** 07/31/01 ****************************************
     C*******
     C*******         F8 = FUND ALPHA SEARCH
     C*******
     C*******    *INKH     IFEQ '1'
     C*******              CALL 'W0010R'
     C*******    *INKC     PARM           @@KC    1
     C*******    *INKL     PARM           @@KL    1
     C*******              PARM '2'       @@CALL  1
     C*******              PARM           @@CO#   3
     C*******              PARM           @@FND   3
     C*******              PARM           @@FYR   3
     C*******              PARM 2         @@VIEW  10
     C*******
     C*******              MOVE @@FND     SCFND
     C*******
     C*******    *INKC     CABEQ*ON       TERM
     C*******
     C*******    *INKL     IFEQ *ON
     C*******    SCFND     OREQ *ZERO
     C*******              WRITESCR99
     C*******              MOVE *BLANK    Q2RECF
     C*******              MOVEL'SCR01'   Q2RECF
     C*******              MOVE '1'       *IN20
     C*******              GOTO STRT
     C*******              ELSE
     C*******
     C*******              WRITESCR99
     C*******              MOVE *BLANK    Q2RECF
     C*******              MOVEL'SCR01'   Q2RECF
     C*******              GOTO STRT
     C*******              END
     C*******              END
      *** END DEL *** 07/31/01 ****************************************
     C*
     C*   F13 = CHG USR PARM
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C*
     C     SCFND         CABEQ     *ZERO         STRT01                   20
     C*
     C                   GOTO      STRT01
     C                   ENDIF
     C*
      ***** BEG ADD ******* 07/31/01 **********************************
     C                   GOTO      STRT
      ***** END ADD ******* 07/31/01 **********************************
     C                   ENDIF
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
      *** BEG UNDEL *** 07/31/01 **************************************
      ***** BEG DEL ******* 04/09/98 **********************************
     C*
     C*  VALIDATE FUND AND GET THE FUND NAME
     C*
     *** Beg Chg *** 06/14/23 *** 114WRJ ****************************
     C**   FNKEY         CHAIN     PFN01                              60
     C**   *IN60         IFEQ      *ON
     C**                 MOVE      *ON           *IN21
     C**                 GOTO      STRT
     C**                 ENDIF

         Chain (UPCO# : SCFND) PFN01;
         If Not %Found(WMFNL);
            erFND = *On;
            msFND = 'WCR0508';
     C                   GOTO      STRT
         EndIf;

     // Check if there are any Claims for the Group

         Chain (UPCO# : SCFND) PCM01;
         If %Found(WMCml1);
            erFND = *On;
            msFND = 'WCR4835';
            Chain (UPCO# : SCFND) PEM01;
            If %Found(WMEml);
               erFND = *On;
               msFND = 'WCR4837';
            EndIf;
     C                   GOTO      STRT
         EndIf;

     // Check if there are any Claims for the Group
         Chain (UPCO# : SCFND) PEM01;
         If %Found(WMEml);
            erFND = *On;
            msFND = 'WCR4836';
     C                   GOTO      STRT
         EndIf;

     *** End Chg *** 06/14/23 *** 114WRJ ****************************
      ***** END DEL ******* 04/09/98 **********************************
      *** END UNDEL *** 07/31/01 **************************************
     C*
     C*   CALL THE CLP TO SUBMIT THE FUND MASTER/TABLE PURGE.
     C*
     C                   MOVE      UPCO#         X@CO#
     C                   MOVE      SCFND         X@FND
     C                   CALL      'W8102C'
     C                   PARM                    X@CO#             3
     C                   PARM                    X@FND             3
     C                   PARM                    UPOUTQ
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
