     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N7721R - AUTO EOB/HBA RELEASE - PROMPT PROGRAM
     H*****************************************************************
     H** REVISIONS:                                                  **
     H**                                                             **
     H*** ADDED 8/29/89 BY CST ( PROJECT A9C )                       **
     H**                                                             **
     H**     ADD DATE FIELD TO F7721F --- RELEASE EOBS PRIOR TO      **
     H**     DATE ENTERED...                                         **
     H**                                                             **
     H**     EDIT DATE IS NOT BLANK AND IS VALID DATE.               **
     H**                                                             **
     H**     PASS DATE AS PARM TO CL FOR SELECTION OF RECORDS        **
     H**                                                             **
     H*** 09/06/89 LJD - CHANGED FROM USER PARM SUBSCRIBER #         **
     H*** PROJ #: A9C    TO ALLOWING USER TO ENTER ONE/ALL SUBSCRIBER**
     H***              - ADDED CMD 5 - SUBSCRIBER SEARCH             **
     H**                                                             **
     H**  02/09/93 C:20  PROJ:                                       **
     H**               - REMOVED DATE FROM ENTRY SCREEN AND EDITS    **
     H**                 PASS DSDATE INSTEAD OF PROMPT DATE          **
     H**               - CHANGED SCREEN WORDING                      **
     H**                                                             **
     H**  04/21/98 C:89 PROJECT: MC181                               **
     H**                CHANGED F6 TO F10 FOR SEARCH                 **
     H**                VERSION 4 STANDARDS                          **
     H**                                                             **
     H**  05/04/98 C:90 PROJECT: MC181                               **
     H**                CHANGED F10 TO CALL F0055R                   **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** INDICATORS   USAGE                                          **
     H** 20 (SCR01) - VALIDATE DATE IS A VALID DATE                  **
     H** 21 (SCR01) - VALIDATE DATE IS NOT BLANK.                    **
     H** 22 (SCR01) - VALIDATE SUBSCRIBER, IF NOT BLANK.             **
     H*****************************************************************
     FN7721F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSML     IF   E           K DISK
      **** BEGIN ADD **** 05/04/98 ************************************
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
      **** END   ADD **** 05/04/98 ************************************

     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D  DSYR                  11     12  0
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND
     D  CONAME                 1     40
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCSUB#
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
     C                   TIME                    DSDTTM                         ADD 02/09/93
     C                   Z-ADD     DSDATE        SCDATE                         ADD 02/09/93
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      UPCO#         XXCO#             3 0
     C*
     C*  GET THE COMPANY RECORDS
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCCONM
     C                   ELSE
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   END
     C*
     C                   TIME                    DSDTTM
     C**** BEGIN DEL **** 04/21/98 ************************************
     C******************** MOVE Q1USER    SCUSER
     C**** END   DEL **** 04/21/98 ************************************
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*****************************************************************
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C**********************************
     C*   COMMAND 03 = EXIT            *
     C**********************************
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C**********************************
     C*   F10 = SEARCH                 *
     C**********************************
      ****BEGIN CHANGE ****04/21/98 ***********************************
     C********** *INKF     IFEQ '1'
     C******************** CALL 'F0545R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCSUB#
     C******************** PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** QQRTC     OREQ '*'
     C********** SCSUB#    OREQ *ZERO
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C******************** END                             END B/*/0
     C******************** END                             END *INKE
     C     *INKJ         IFEQ      '1'
      **** BEGIN CHG **** 05/04/98 ************************************
     C     HFLD          IFEQ      'SCSUB#'
     C                   CALL      'F0055R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQRRNS            9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         CABEQ     *ON           STRT
      *
     C     QQRRNS        CHAIN     PSM02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD SCSUB#
     C******************** CALL 'F0545R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           Q1SUB#  50
     C******************** PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C********** QQRTC     IFEQ 'B'
     C********** Q1SUB#    OREQ *ZERO
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C******************** END                             ENDIF EQ B/0
     C*
     C********** QQRTC     IFEQ '*'
     C******************** MOVE Q1SUB#    SCSUB#
     C******************** GOTO STRT
     C******************** ENDIF                           ENDIF EQ '*'
      **** END   CHG **** 05/04/98 ************************************
     C*
     C                   ENDIF                                                  END *INKJ
      ****END   CHANGE ****04/21/98 ***********************************
     C**********************************
     C*   PROCESS HELP REQUESTS        *
     C**********************************
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
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END                                                    END *IN10
      **** BEGIN DEL **** 05/04/98 ************************************
     C**********************************
     C********* COMMAND 13 = CHANGE USER PARMS *
     C**********************************
     C********** *INKM     IFEQ '1'
     C******************** CALL 'F0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C******************** MOVE *ZERO     UPCO#
     C******************** END                             ENDIF *IN60
     C*
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO TOP
     C******************** END                             ENDIF *INKM
      **** END   DEL **** 05/04/98 ************************************
     C*
     C                   END                                                     END CMDKEY
     C*****************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*****************************************************************
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*** VALIDATE SUBSCRIBER, IF NOT BLANK
     C     SCSUB#        IFNE      *ZEROS
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN22                          INVALID SUB#
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN60
     C                   END                                                    END NE *ZERO
     C*
     C*   CALL THE CLP TO SUBMIT THE AUTO EOB/HBA RELEASE PGM
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      SCSUB#        QQSUB#            5
     C                   MOVE      SCDATE        QQDATE            8
     C                   CALL      'N7721C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQDATE
     C                   PARM                    UPOUTQ
     C                   PARM                    MODE              1
     C*****************************************************************
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
