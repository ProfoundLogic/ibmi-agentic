     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N7081R - PROMPT FOR A/R TRANS LISTING WITHIN DATE RANGE
     H**************************************************************************
     H* 05/12/98 C:90    PROJ:MC181                                            *
     H*                  CHANGED SCREEN TO MEET VERSION 4 STANDARDS            *
     H**************************************************************************
     FN7081F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSML     IF   E           K DISK

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
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    UPDT          XXFRDT
     C     *LIKE         DEFINE    UPDT          XXTODT
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
     C* GET SUBSCRIBER NAME
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         SCSUB
     C                   MOVE      *BLANK        SCSBNM
     C                   ELSE
     C                   MOVEL     UPSUB#        SCSUB
     C                   MOVEL     SMNAME        SCSBNM
     C                   END
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
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
     C****************************
     C*   COMMAND 03 = EXIT      *
     C****************************
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C****************************
     C*   COMMAND 13 = USER PARMS*
     C****************************
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
     C*
     C****************************
     C*  GET THE USER PARAMETER FILE
     C****************************
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END                                                    *IN11
     C*
     C************* END OF COMMAND KEY SECTION ***************************
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE ENTERED FROM DATE
     C*
     C*
      **** BEGIN ADD **** 05/12/98 ************************************
     C     SCFRDT        IFNE      *ZEROS
      **** END   ADD **** 05/12/98 ************************************
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
      **** BEGIN ADD **** 05/12/98 ************************************
     C                   ENDIF                                                  IFNE *ZEROS
      **** END   ADD **** 05/12/98 ************************************
     C*
      **** BEGIN DEL **** 05/12/98 ************************************
     C***********  VALIDATE THE ENTERED TO DATE
     C*
     C********** SCTODT    IFEQ *ZERO
     C******************** MOVE UPPRDT    SCTODT
     C******************** END
      **** END   DEL **** 05/12/98 ************************************
     C*
     C*  VALIDATE THE ENTERED TO DATE
     C*
     C*
      **** BEGIN ADD **** 05/12/98 ************************************
     C     SCTODT        IFNE      *ZEROS
      **** END   ADD **** 05/12/98 ************************************
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
      **** BEGIN ADD **** 05/12/98 ************************************
     C                   ENDIF                                                   IFNE *ZEROS
      **** END   ADD **** 05/12/98 ************************************
     C*
     C*  VALIDATE THE ENTERED DATES FOR ASCENDING ORDER
     C*
      **** BEGIN ADD **** 05/12/98 ************************************
     C     SCFRDT        IFNE      *ZEROS
     C     SCTODT        ANDNE     *ZEROS
      **** END   ADD **** 05/12/98 ************************************
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C     XXFRDT        CABGT     XXTODT        STRT                 24
      **** BEGIN ADD **** 05/12/98 ************************************
     C                   ENDIF                                                   IFNE *ZERO
      **** END   ADD **** 05/12/98 ************************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
      **** BEGIN ADD **** 05/12/98 ************************************
     C     SCFRDT        IFEQ      *ZEROS
     C                   MOVE      '01011940'    QQFRDT            8
     C                   ELSE
      **** END   ADD **** 05/12/98 ************************************
     C                   MOVE      SCFRDT        QQFRDT            8
      **** BEGIN ADD **** 05/12/98 ************************************
     C                   ENDIF                                                  IFEQ SCFRDT
     C     SCTODT        IFEQ      *ZEROS
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         QQTODT            8
     C                   ELSE
      **** END   ADD **** 05/12/98 ************************************
     C                   MOVE      SCTODT        QQTODT            8
      **** BEGIN ADD **** 05/07/98 ************************************
     C                   ENDIF                                                  IFEQ SCTODT
      **** END   ADD **** 05/07/98 ************************************
     C                   CALL      'N7081C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    UPOUTQ
     C                   PARM                    MODE              1
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
