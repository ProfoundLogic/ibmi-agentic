     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W2321R - DEPARTMENT/FOLDER/DOCUMENT ID SEARCH            *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FW2321F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F*
     FWMDOL     IF   E           K DISK
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
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    DODPCD        XXDPCD
     C     *LIKE         DEFINE    DOFLDR        XXFLDR
     C     *LIKE         DEFINE    DODCID        XXDCID
     C     *LIKE         DEFINE    DODEPT        XXDEPT
     C*
     C*   KEY LIST FOR THE DEPARMENT/DOC ID MASTER FILE
     C*
     C     DOKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    XXDPCD
     C                   KFLD                    XXFLDR
     C                   KFLD                    XXDCID
     C*
     C     DOKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXDEPT
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
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQDEPT            2
     C                   PARM                    QQDPCD            5
     C                   PARM                    QQFLDR           80
     C                   PARM                    QQDCID           12
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
     C     TOP           TAG
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      QQDEPT        XXDEPT
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  INITIALIZE ROUTINE
     C*
     C                   EXSR      INIT
     C*
     C                   MOVEL     SCDPC2        XXDPCD
     C                   MOVE      *BLANKS       XXFLDR
     C                   MOVEL     SCFLD2        XXFLDR
     C                   MOVEL     SCDCI2        XXDCID
     C     DOKEY         SETLL     PDO01                              60
     C*
     C*  CLEAR SUBFILE
     C*
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C     STRT          TAG
     C*
     C***************      MOVELQ2FILE    SCFILE           SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      '1'
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   END                                                    END *INKL
     C****************************************
     C*   *IN10 -  PROCESS HELP REQUESTS     *
     C****************************************
     C  N10
     CAN 10*IN10         IFEQ      '1'
     C  N10
     CAN 10              MOVE      '1'           *IN10
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
     C                   END                                                    END *IN10
     C*
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C**************************************************************************
     C*
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C*
     C     SCDPC2        IFNE      *BLANKS
     C     SCFLD2        ORNE      *BLANKS
     C     SCDCI2        ORNE      *BLANKS
     C                   GOTO      TOP
     C                   END                                                    END SCDPCD NE BLKS
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C*
     C                   READC     SCR01S                                 50
     C     *IN50         IFEQ      '0'
     C*
     C*  IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*  SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
     C*
     C     SCSEL         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   MOVEL     SCDPCD        QQDPCD
     C                   MOVE      *BLANKS       QQFLDR
     C                   MOVEL     SCFLDR        QQFLDR
     C                   MOVEL     SCDCID        QQDCID
     C                   GOTO      TERM
     C                   ELSE                                                   ELSE EQ '1'
     C*
     C*  IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*  A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C*
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '1'
     C*
     C*  IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
     C*  WITHOUT A SELECTED RECORD
     C*
     C                   ELSE                                                   ELSE 0 *IN50
     C                   GOTO      TERM
     C                   END                                                    END 0 *IN50
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*****************************************************************
     C* INIT SUBROUTINE                ** Subroutine **   I N I T     *
     C*****************************************************************
     C     INIT          BEGSR
     C                   Z-ADD     0             RRN2              5 0
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   ENDSR
     C*****************************************************************
     C* LOAD SUBROUTINE                ** Subroutine **   L O A D     *
     C*****************************************************************
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C*
     C*  LOAD THE SUBFILE SCREEN WITH UPTO 6 LINES
     C*
     C                   DO        6             X                 5 0
     C*
     C     DOKEY1        READE     PDO01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   ADD       1             RRN2
     C                   MOVEL     DODPCD        SCDPCD
     C                   MOVEL     DOFLDR        SCFLDR
     C                   MOVEL     DODCID        SCDCID
     C                   MOVEL     DODCID        SCDCID
     C                   MOVEL     DODESC        SCDESC
     C                   MOVEL     DORMTY        SCRMTY
     C                   MOVEL     DOASRV        SCASRV
     C                   MOVEL     DOSUBA        SCSUBA
     C                   MOVEL     DOSTAT        SCSTS
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C*  INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C*
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVEL     *BLANKS       SCDPCD
     C                   MOVEL     *BLANKS       SCFLDR
     C                   MOVEL     *BLANKS       SCDCID
     C                   MOVEL     *BLANKS       SCDCID
     C                   MOVEL     *BLANKS       SCDESC
     C                   MOVEL     *BLANKS       SCRMTY
     C                   MOVEL     *BLANKS       SCASRV
     C                   MOVEL     *BLANKS       SCSUBA
     C                   MOVEL     *BLANKS       SCSTS
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01S
     C                   END
     C*
     C*  TO POSITION SCREEN AND CURSOR
     C*
     C                   Z-ADD     RRN2          #@$2
     C*
     C                   MOVEL     *BLANK        SCDPC2
     C                   MOVEL     *BLANK        SCFLD2
     C                   MOVEL     *BLANK        SCDCI2
     C*
     C                   ENDSR
