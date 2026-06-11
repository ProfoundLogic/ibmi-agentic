     hcopyright('(c) 2020 Sapiens, Inc. All rights reserved.')
    H option(*nodebugio:*srcstmt)
    H dftactgrp(*no) actgrp(*caller)
     H*
     H* Title F8039R - Upload Process - Assigning Member Nmae
     H*
     H*****************************************************************
     H* Created on 04/21/2004 by c:62 for Project 51MFN               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  05/11/20 C:212  Project: MC437                               *
     H*                  Redesign per CS standards                    *
     H*                  - Call to new CS standard program and return *
     H*****************************************************************
     FF8039F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFXU0L     IF   E           K DISK
     F                                     RENAME(QWHFDML:DBRCDF)
     FFDOCP     IF   E             DISK
     FFPRVP     O    E             DISK    usropn
     D*
     D OC              S              1    DIM(500)
     D*
     D OPTNAM          C                   CONST('Bill Upload -
     D                                     Process : Part 1')
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  MBR1                   1     10
     D  LTRL1                  1      1    INZ('P')
     D  YYYEAR                 2      5  0
     D  YYDAYS                 6      8  0
     D  DATE1                  2      8  0
     D  SEQ1                   9     10  0
     D*
     D                 DS
     D  MBR2                   1     10
     D  LTRL2                  1      1    INZ('P')
     D  DATE2                  2      8  0
     D  SEQ2                   9     10  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D***** Beg Add *** 05/11/20 **************************************
     d PBUpldProc1     pr                  extpgm('MTPBULP1')
     D ppCall                         1
     D***** End Add *** 05/11/20 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
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
     C     DTLJUL        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*JUL'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     U0KY          KLIST
     C                   KFLD                    KKNAME
     C*
     C     *LIKE         DEFINE    MLNAME        KKNAME
     c*
     c***** Beg Add *** 05/11/20 **************************************
     c/free
        Callp PBUpldProc1(@@call);
        *inlr = *on;
        Return;
      /end-free
     c***** End Add *** 05/11/20 **************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C     *IN99         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKY          CHAIN     PCO01                              99
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
     C                   MOVEL(P)  OPTNAM        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C                   SETOFF                                       16
     C                   IF        @@CALL='P'
     C                   SETON                                        16
     C                   ENDIF
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
     C                   ENDIF
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C**************************************************************************
     C*  F3 = EXIT
     C**************************************************************************
     C*
     C*   CALL FOR END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF
     C                   ENDIF
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C                   EXSR      ASSMEM
     C*
     C                   CLEAR                   POC01
     C     1             SETLL     POC01
     C                   READ      POC01                                  99
     C                   IF        NOT *IN99 OR @@CALL<>'P'
     C                   IF        @@CALL='P'
     C
      * Filter out characters that can not be displayed on the AS400
     C                   CLEAR                   OC
     C                   MOVEA     OCDCID        OC
      *
     C     1             DO        500           X                 3 0
     C     OC(X)         IFLT      X'40'
      *
     C     OC(X)         IFNE      X'08'
     C     OC(X)         ANDNE     X'09'
     C                   MOVE      X'40'         OC(X)
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     OC(X)         IFGT      X'F9'
     C                   MOVE      X'40'         OC(X)
     C                   ENDIF
     C                   ENDDO
     C                   MOVEA     OC            OCDCID
     C*
     c                   open      fprvp
     c                   clear                   ppr01
     C                   eval      prdata='H' + ocdcid
     c                   write     ppr01
     c                   close     fprvp
     c                   eval      docid=ocdcid
     c                   else
     c                   eval      docid=*blanks
     c                   endif
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   CALL      'F8039C'
     C                   PARM      MBR1          MEMBER           10
     C                   PARM                    DOCID           500
     C                   ENDIF
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   SETON                                        LR
     C*****************************************************************
     C* ASSMEM - ASSIGN FILE MEMBER
     C*****************************************************************
     C     ASSMEM        BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTLJUL
     C                   MOVEL     SRDTO         DATE5             5 0
     C                   MOVE      DATE5         YYDAYS
     C                   MOVE      DSDATE        YYYEAR
     C*
     C                   Z-ADD     99            SEQ1
     C*
     C                   MOVEL(P)  MBR1          KKNAME
     C     U0KY          SETGT     DBRCDF
     C                   READP     DBRCDF                                 99
     C     *IN99         IFEQ      *OFF
     C     MLNAME        ANDNE     'FPRVP'
     C                   MOVEL     MLNAME        MBR2
     C     DATE1         IFEQ      DATE2
     C     SEQ2          ADD       1             SEQ1
     C                   ELSE
     C                   Z-ADD     0             SEQ1
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     0             SEQ1
     C                   ENDIF
     C*
     C                   ENDSR
