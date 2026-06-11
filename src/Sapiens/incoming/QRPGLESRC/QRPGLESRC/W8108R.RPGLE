     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8108R - PROMPT FOR CLAIM MASTER FILE PURGE
     H****************************************************************
     H*                                                              *
     H*  PROGRAM DESCRIPTION:                                        *
     H*       THIS PROGRAM ALLOWS THE USER TO ENTER THE FUND TO BE   *
     H*       PURGED IN THE CLAIM MASTER FILE PURGE JOB.             *
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
     H*                 Add CheckBox for Client 114.                 *
     H*                                                              *
     H*  07/31/01 C:135 Project SUPPORT WC577                        *
     H*           Rework the screen to standards. Added a goto at    *
     H*           the end of the function key processing where       *
     H*           the purge program would be called if an            *
     H*           invalid function key was pressed.                  *
     H*           Reinstated the validation for group number.        *
     H*                                                              *
     H*  04/09/98 C:61 PROJECT 60APK                                 *
     H*                Remove Fund field edit.                       *
     H*                                                              *
     H****************************************************************
     H*                                                              *
     H* INDICATOR USAGE:                                             *
     H*  SCR01                                                       *
     H*  21 - INVALID FUND NUMBER                                     *
     H*                                                              *
     H****************************************************************
      *
      *
     FW8108F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      *** BEG CHG *** 07/31/01 ****************************************
      ***** BEG DEL ******* 04/09/98 **********************************
     FWMFNL     IF   E           K DISK
      ***** END DEL ******* 04/09/98 **********************************
      *** END CHG *** 07/31/01 ****************************************
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
      *

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
     D isVisible       S               N   Inz(*Off)
     D ##CltId         S              3    inz('000')
     D X@DelF          S              1A
      *
     D RtvCltid        PR                  EXTPGM('RTVCLTID')
     D ##CltId                        3
     *** End Add *** 06/14/23 *** 114WRJ ****************************
      *
      *  KEY LIST FOR THE COMPANY FILE
      *

     C     COKEY         KLIST
     C                   KFLD                    UPCO#
      *
      *  USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *  FUND MASTER FILE
      *
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND             3 0
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
      *
      *  GET THE COMPANY RECORD
      *
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
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
     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
      // Get Client ID

         RtvCltid(##CltId);
         If ##CltId = '114';
            isVisible = *On;
         Else;
            isVisible = *Off;
         EndIf;

     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
      *
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
      *   DISPLAY ALL FORMATS
      *
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
      *
      *   PROCESS ALL COMMAND KEYS
      *
     C     *IN11         IFEQ      '1'
      *
      *   F3 = END OF JOB
      *
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF
      *
      *   PROCESS HELP REQUESTS
      *
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
      *
     C                   MOVE      @@FND         SCFND
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         IFEQ      *ON
     C     SCFND         OREQ      *ZERO
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ELSE
      *
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
      *** BEG DEL *** 07/31/01 ****************************************
      *******
      *******         F8 = FUND ALPHA SEARCH
      *******
     C*******    *INKH     IFEQ '1'
     C*******              CALL 'W0010R'
     C*******    *INKC     PARM           @@KC    1
     C*******    *INKL     PARM           @@KL    1
     C*******              PARM '2'       @@CALL  1
     C*******              PARM           @@CO#   3
     C*******              PARM           @@FND   3
     C*******              PARM           @@FYR   3
     C*******              PARM 2         @@VIEW  10
      *******
     C*******              MOVE @@FND     SCFND
      *******
     C*******    *INKC     CABEQ*ON       TERM
      *******
     C*******    *INKL     IFEQ *ON
     C*******    SCFND     OREQ *ZERO
     C*******              WRITESCR99
     C*******              MOVE *BLANK    Q2RECF
     C*******              MOVEL'SCR01'   Q2RECF
     C*******              MOVE '1'       *IN20
     C*******              GOTO STRT
     C*******              ELSE
      *******
     C*******              WRITESCR99
     C*******              MOVE *BLANK    Q2RECF
     C*******              MOVEL'SCR01'   Q2RECF
     C*******              GOTO STRT
     C*******              END
     C*******              END
      *** END DEL *** 07/31/01 ****************************************
      *
      *   F13 = CHG USR PARM
      *
      *   PROCESS CHANGE OF USER PARAMETERS
      *
     C     *INKM         IFEQ      '1'
      *
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
      *
     C     SCFND         CABEQ     *ZERO         STRT01                   20
      *
     C                   GOTO      STRT01
     C                   ENDIF
      *
      *** BEG ADD *** 07/31/01 ****************************************
     C                   GOTO      STRT
      *** END ADD *** 07/31/01 ****************************************
     C                   ENDIF
      *
      *   PROCESS REQUEST SCREEN
      *
     C     SCR01T        TAG
      *** BEG CHG *** 07/31/01 ****************************************
      ***** BEG DEL ******* 04/09/98 **********************************
      *
      *  VALIDATE FUND AND GET THE FUND NAME
      *
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF
      ***** END DEL ******* 04/09/98 **********************************
      *** END CHG *** 07/31/01 ****************************************
      *
     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
          If scDelDart = 'Y';
             X@DelF = 'Y' ;
          Else;
             X@DelF = 'N' ;
          EndIf;
     *** End Add *** 06/14/23 *** 114WRJ ****************************
      *   CALL THE CLP TO SUBMIT THE CLAIM MASTER FILE PURGE.
      *
     C                   MOVE      UPCO#         X@CO#
     C                   MOVE      SCFND         X@FND
     C                   CALL      'W8108C'
     C                   PARM                    X@CO#             3
     C                   PARM                    X@FND             3
     C                   PARM                    UPOUTQ
     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
     C                   PARM                    X@DelF
     *** End Add *** 06/14/23 *** 114WRJ ****************************
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
