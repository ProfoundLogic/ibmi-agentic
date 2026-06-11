     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W4040R - AGENT LABELS
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  07/24/08 C:61 PROJECT: 46WGC                                 *
     H*                Add parm to pass batch # to submit pgm.        *
     H*                                                               *
     H*  12/05/97 C:85 PROJECT: WC577                                 *
     H*                CHANGE TO POSITION CURSOR AT NEXT FIELD AFTER  *
     H*                SUCCESSFUL AGENT SEARCH.                       *
     H*                DELETED "AMM" INITIALS FROM COMMENTS.          *
     H*                DELETED REFERENCES TO SCR99.                   *
     H*                                                               *
     H*  10/23/97 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 REPORT STANDARDS.                    *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     FW4040F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAAP     IF   E             DISK
     F                                     RENAME(PAA01:PAA02)

     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
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
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#            5 0
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
     C                   MOVE      UPFND         SCFND
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C                   MOVEL     FNPZIP        SCZP              3 0
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   Z-ADD     1             SCNUM
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
     C*   COMMAND F3 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
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
     C*
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   COMMAND F10 = AGENT NUMERIC SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C                   CALL      'W0040R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRN         ##RRN             9 0
     C                   PARM      2             ##VIEW            1 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZERO
     C                   MOVE      '1'           *IN30                          PC @ AGENT#
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PAA02                              99
     C*
     C                   MOVE      AAAGT#        SCAGT#
     C*** BEGIN CHG *** 12/05/97 **************************************
     C                   MOVE      '1'           *IN31                          PC @ COMM MAIL
     C*** END   CHG *** 12/05/97 **************************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
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
     C*   PROCESS REQUEST SCREEN
     C*
     C*
     C*  VALIDATE AGENT NUMBER IF ENTERED
     C*
     C     SCAGT#        IFNE      *ZERO
     C*
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SCAGT#        XXAGT#
     C     AAKEY         CHAIN     PAA01                              40
     C     *IN40         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEGIN ADD *** 10/23/97 **************************************
     C*
     C                   MOVE      'N'           SCALL             1
     C*
     C                   ELSE
     C                   MOVE      'Y'           SCALL             1
     C                   END
     C*
     C*** END   ADD *** 10/23/97 **************************************
     C*
     C*** BEGIN DEL *** 10/23/97 **************************************
     C*
     C*  ALL AGENTS MUST BE "Y" OR "N"
     C*
     C********** SCALL     IFNE 'Y'
     C********** SCALL     ANDNE'N'
     C**********           MOVE '1'       *IN21
     C**********           GOTO STRT
     C**********           END
     C*
     C*** END   DEL *** 10/23/97 **************************************
     C*
     C*
     C*  PAY COMMISSION FLAG MUST BE "Y" OR "N"
     C*
     C     SCCMFL        IFNE      'Y'
     C     SCCMFL        ANDNE     'N'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  MAIL FLAG MUST BE "Y" OR "N"
     C*
     C     SCMLFL        IFNE      'Y'
     C     SCMLFL        ANDNE     'N'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  PROSPECT FLAG MUST BE "Y" OR "N"
     C*
     C     SCPRFL        IFNE      'Y'
     C     SCPRFL        ANDNE     'N'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*  NUMBER OF COPIES MUST BE BETWEEN 1 & 9.
     C*
     C     SCNUM         IFLT      1
     C     SCNUM         ORGT      9
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  LABEL TYPE MUST BE "R" OR "L".
     C*
     C     SCTYPE        IFNE      'R'
     C     SCTYPE        ANDNE     'L'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C* EACH PRODUCER FLAG MUST BE "Y" OR "N"
     C*
     C     SCPDFL        IFNE      'Y'
     C     SCPDFL        ANDNE     'N'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C* RPINT PHONE NUMBER FLAG MUST BE 'Y' OR 'N'
     C*
     C     SCPHON        IFNE      'Y'
     C     SCPHON        ANDNE     'N'
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCAGT#        QQAGT#            5
     C                   MOVE      SCALL         QQALL             1
     C                   MOVE      SCCMFL        QQCMFL            1
     C                   MOVE      SCMLFL        QQMLFL            1
     C                   MOVE      SCPRFL        QQPRFL            1
     C                   MOVE      SCNUM         QQNUM             1
     C                   MOVE      SCTYPE        QQTYPE            1
     C                   MOVE      SCPDFL        QQPDFL            1
     C                   MOVE      SCPHON        QQPHON            1
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C*
     C                   CALL      'W4040C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQAGT#
     C                   PARM                    QQALL
     C                   PARM                    QQCMFL
     C                   PARM                    QQMLFL
     C                   PARM                    QQPRFL
     C                   PARM                    QQNUM
     C                   PARM                    QQTYPE
     C                   PARM                    QQPDFL
     C                   PARM                    QQPHON
     C                   PARM                    QQOUTQ
      ***** beg add **** 07/24/08 *********************************
     C                   PARM      *BLANKS       QQBAT#            9
      ***** beg add **** 07/24/08 *********************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
