     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7340R - PROMPT FOR FIRST 3 BYTES IN ZIP CODE, & LIST TYPE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*  20   WCR0628  LIST TYPE MUST BE 'A' OR 'N'.                  *
     H*  21   WCR1297  REPORT SELECTION MUST BE '1', '2', OR BLANK.   *
     H*  22   WCR0503  FIRST 3 POSITIONS OF ZIP CODE CANNOT BE BLANKS.*
     H*  23   WCR0503  CITY NAME CANNOT BE BLANKS.                    *
     H*  24   WCR0501  ACTIVE AGENT ONLY MUST BE 'Y' OR 'N'           *
     H*****************************************************************
     FW7340F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK

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
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
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
     C                   MOVE      *BLANKS       SCTYPE
     C*
     C*
     C* Center Company name in display field
     C*
     C     ' '           CHECK     SCCONM        N                 3 0
     C                   MOVE      *BLANKS       XXXXNM           40
     C                   SUBST(P)  SCCONM:N      XXXXNM           40
     C     ' '           CHECKR    XXXXNM        N                 3 0
     C     40            SUB       N             N
     C     N             DIV       2             N
     C                   MOVE      *BLANKS       SCCONM
     C                   CAT       XXXXNM:N      SCCONM           40
     C*
     C* Center Screen Prompt in display field
     C*
     C     ' '           CHECK     SCPRDS        N                 3 0
     C                   MOVE      *BLANKS       XXXXNM           40
     C                   SUBST(P)  SCPRDS:N      XXXXNM           40
     C     ' '           CHECKR    XXXXNM        N                 3 0
     C     40            SUB       N             N
     C     N             DIV       2             N
     C                   MOVE      *BLANKS       SCPRDS
     C                   CAT       XXXXNM:N      SCPRDS           40
     C*
     C*
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
     C*   COMMAND 01 = END OF JOB
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
     C     SCR01T        TAG
     C*
     C*   VALIDATE REPORT SELECTION
     C*
     C     SCSEL         IFNE      '1'                                          BY ZIP CODE
     C     SCSEL         ANDNE     '2'                                          BY CITY NAME
     C     SCSEL         ANDNE     *BLANK                                       ALL
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*   MUST ENTER FIRST 3 POSITIONS OF ZIP CODE IF SELECT BY ZIP CODE
     C*
     C     SCSEL         IFEQ      '1'
     C     SCZP          ANDEQ     *ZERO
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*   MUST ENTER CITY NAME IF SELECT BY CITY NAME
     C*
     C     SCSEL         IFEQ      '2'
     C     SCCITY        ANDEQ     *BLANK
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE LIST TYPE
     C*
     C     SCTYPE        IFNE      'A'
     C     SCTYPE        ANDNE     'N'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE LIST TYPE
     C*
     C     SCAAO         IFNE      'A'
     C     SCAAO         ANDNE     'I'
     C     SCAAO         ANDNE     'B'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE CANCELLED AGENT FLAG
     C*
     C     SCCAA         IFNE      'Y'
     C     SCCAA         ANDNE     'N'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCZP          QQZP              3
     C                   MOVE      SCTYPE        QQTYPE            1
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   MOVE      SCSEL         QQSEL             1
     C                   MOVE      SCCITY        QQCITY           25
     C                   MOVE      SCAAO         QQAAO             1
     C                   MOVE      SCCAA         QQCAA             1
     C                   CALL      'W7340C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQZP
     C                   PARM                    QQTYPE
     C                   PARM                    QQOUTQ
     C                   PARM      'D'           MODE              1
     C                   PARM                    QQSEL
     C                   PARM                    QQCITY
     C                   PARM                    QQAAO
     C                   PARM                    QQCAA
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
