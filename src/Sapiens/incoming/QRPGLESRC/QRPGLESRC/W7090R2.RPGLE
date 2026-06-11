     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE W7090R2 - CLAIM LABELS REPRINT - PROMPT
     H****************************************************************
     H*  PROGRAMMER:  C:79     PROJECT# WC742                        *
     H*                                                              *
     H*  CALLED FROM:                                                *
     H*         MENU                                                 *
     H*  CALLS TO:                                                   *
     H*         W7090C2 - CLAIM LABELS - SUBMITTER                   *
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*      11/07/97 C:79 PROJ: WC742                               *
     H*                    - CHANGE FUND SEARCH TO CALL W0010R       *
     H****************************************************************
     H*  INDICATOR USAGE:                                            *
     H*                                                              *
     H*   10  -  PROCESS HELP REQUEST                                *
     H*   11  -  VALID FUNCTION KEYS                                 *
     H*   20  -  POSITION CURSOR SCFND1                              *
     H*   21  -  POSITION CURSOR SCFND2                              *
     H*   22  -  POSITION CURSOR SCFRYR                              *
     H*   23  -  POSITION CURSOR SCFCAS                              *
     H*   24  -  POSITION CURSOR SCTOYR                              *
     H*   25  -  POSITION CURSOR SCTCAS                              *
     H*   26  -  POSITION CURSOR SCSTAT                              *
     H*   27  -  POSITION CURSOR SCFRDT                              *
     H*   28  -  POSITION CURSOR SCTODT                              *
     H*   41  -  VALIDATE ONLY ONE SET OF FIELDS SELECTED (SCFND1  ) *
     H*   42  -  VALIDATE SCFRDT                          (SCFRDT  ) *
     H*   43  -  VALIDATE SCTODT                          (SCTODT  ) *
     H*   44  -  VALIDATE SCFND1                          (SCFND1  ) *
     H*   45  -  VALIDATE SCFND2                          (SCFND2  ) *
     H*   46  -  VALIDATE "FROM" YEAR NOT > "TO" YEAR     (SCFRYR  ) *
     H*   47  -  VALIDATE SCSTAT                          (SCSTAT  ) *
     H*   48  -  VALIDATE "FROM" DATE ENTERED             (SCFRDT  ) *
     H*   49  -  VALIDATE ONE SET OF FIELDS ENTERED       (SCFRDT  ) *
     H*   50  -  VALIDATE FIELD REQUIRED SCFND2           (SCFND2  ) *
     H*   51  -  VALIDATE FIELD REQUIRED SCFRYR           (SCFRYR  ) *
     H*   52  -  VALIDATE FIELD REQUIRED SCTOYR           (SCTOYR  ) *
     H*   53  -  VALIDATE "FROM" DATE NOT > "TO" DATE     (SCFRDT  ) *
     H*   54  -  VALIDATE "FROM" CASE NOT > "TO" CASE     (SCFCAS  ) *
     H*   55  -  VALIDATE "FROM" CASE ENTERED             (SCFCAS  ) *
     H*   56  -  VALIDATE "TO" CASE ENTERED               (SCTCAS  ) *
     H*   57  -  VALIDATE STATUS ENTERED                  (SCSTAT  ) *
     H*   58  -  VALIDATE "FROM" CASE ENTERED             (SCFCAS  ) *
     H*   59  -  VALIDATE "TO" DATE ENTERED               (SCTODT  ) *
     H*   60  -  GENERAL CHAINING                                    *
     H*   61  -  VALIDATE SCFRDT ENTERED                  (SCFRDT  ) *
     H*   62  -  VALIDATE SCTODT ENTERED                  (SCTODT  ) *
     H*   63  -  INVALID SEARCH FIELD SCFRDT              (SCFRDT  ) *
     H*   64  -  INVALID SEARCH FIELD SCTODT              (SCTODT  ) *
     H*   65  -  INVALID SEARCH FIELD SCFRYR              (SCFRYR  ) *
     H*   66  -  INVALID SEARCH FIELD SCTOYR              (SCTOYR  ) *
     H*   67  -  INVALID SEARCH FIELD SCSTAT              (SCSTAT  ) *
     H*   41  -  VALIDATE ONLY ONE SET OF FIELDS SELECTED (SCFND1  ) *
     H*                                                              *
     H****************************************************************
     FW7090F2   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMCMP     IF   E             DISK
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
      *
      *   KEY LIST FOR THE COMPANY FILE
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
     C                   KFLD                    XXFND
      *
     C     *LIKE         DEFINE    FNDT          XXDAT1
     C     *LIKE         DEFINE    FNDT          XXDAT2
     C     *LIKE         DEFINE    FNFND         XXFND
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
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
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
      *   DISPLAY ALL FORMATS
      *
     C     STRT          TAG
      *
      *  CONVERT THE FROM DATE ENTERED
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCFRDT
     C                   ENDIF                                                  SRERR = '1'
      *
      *  CONVERT THE TO DATE ENTERED
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCTODT
     C                   ENDIF                                                  SRERR = '1'
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
      *
      *   PROCESS ALL COMMAND KEYS
      *
     C     *IN11         IFEQ      *ON
      *
      *   F3=EXIT
      *
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   END
      *
      *   PROCESS HELP REQUESTS
      *
     C     *IN10         IFEQ      *ON
      *
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
      *================================================================
      *   F10=TABLE SEARCH
      *================================================================
     C     *INKJ         IFEQ      *ON
      *
     C     Q2RECF        IFEQ      'SCR01'
      *================================================================
      *  FUND SEARCH
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCFND1'
     C     HFLD          OREQ      'SCFND2'
      *
     ****BEG*DEL*****11/07/97 *****************************************
     C*******************  CALL 'W0510R'
     C*******************  PARM           QQPRDS
     C*******************  PARM           QQCALL
     C*******************  PARM *ZEROS    QQKEY   30
     C*******************  PARM           QQRTCD  1
      *
      *  EXIT THE PROGRAM
      *
     C*****      QQRTCD    CABEQ'A'       TERM
      *
      *  RETURN TO PREVIOUS SCREEN
      *
     C*****      QQRTCD    IFEQQ'B'       TERM
     C*****      QQKEYD    OREQQ*ZEROS    TERM
     *** END DEL *** 11/07/97 *****************************************
      *
     *** BEG ADD *** 11/07/97 *****************************************
     C                   MOVE      UPCO#         ##CO#             3
      *
     C                   CALL      'W0010R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    ##CO#
     C                   PARM      *BLANKS       ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM      1             @@VIEW            1 0
      *
     C     ##FND         IFEQ      *BLANKS
      *
     *** END ADD *** 11/07/97 *****************************************
      *
     C     HFLD          COMP      'SCFND1'                               20
     C     HFLD          COMP      'SCFND2'                               21
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
      *  A VALUE WAS SELECTED
      *
     *** BEG DEL *** 11/07/97 *****************************************
     C********** QQRTCD    IFEQ '*'
     C********** QQKEY     ANDNE*ZEROS
     *** END DEL *** 11/07/97 *****************************************
      *
     *** BEG ADD *** 11/07/97 *****************************************
      *
     C     ##FND         IFNE      *BLANKS
      *
     *** END ADD *** 11/07/97 *****************************************
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCFND1'
      *
     *** BEG CHG *** 11/07/97 *****************************************
     C*******************  MOVE QQKEY     SCFND1
     C                   MOVE      ##FND         SCFND1
     *** END CHG *** 11/07/97 *****************************************
      *
     C                   MOVE      *ON           *IN21
      *
     C     HFLD          WHENEQ    'SCFND2'
      *
     *** BEG CHG *** 11/07/97 *****************************************
     C*******************  MOVE QQKEY     SCFND2
     C                   MOVE      ##FND         SCFND2
     *** END CHG *** 11/07/97 *****************************************
      *
     C                   MOVE      *ON           *IN22
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  ##FND NE BLANK
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
      *================================================================
      *  CASE# SEARCH
      *
     C     HFLD          WHENEQ    'SCFCAS'
     C     HFLD          OREQ      'SCTCAS'
      *
     C                   CALL      'W0070R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '3'           QQMODE            1
     C                   PARM      *ZEROS        QQRRN             9 0
     C                   PARM      'CN'          QQSORT            2
     C                   PARM      *ZEROS        QQRRN2            9 0
     C                   PARM      *ZERO         QQFYR             3 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
      *  *INKL WAS NOT PRESSED. A RECORD WAS SELECTED
      *
     C     *INKL         IFEQ      *OFF
     C     QQRRN         ANDNE     *ZERO
      *
     C     QQRRN         CHAIN     WMCMP                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      CMFND         SCFND2
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCFCAS'
      *
     C                   MOVE      CMCASE        SCFCAS
     C                   MOVE      CMFYR         SCFRYR
     C                   MOVE      *ON           *IN24
      *
     C     HFLD          WHENEQ    'SCTCAS'
      *
     C                   MOVE      CMCASE        SCTCAS
     C                   MOVE      CMFYR         SCTOYR
     C                   MOVE      *ON           *IN26
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
      *  NO RECORDS FOUND SO RETURN AND POSITION TO CURRENT FIELD
      *
     C                   ELSE
      *
     C     HFLD          IFEQ      'SCFCAS'
      *
     C                   MOVE      *ON           *IN23
      *
     C                   ELSE
     C                   MOVE      *ON           *IN25
      *
     C                   ENDIF                                                  HFLD=SCFCAS
      *
     C                   ENDIF                                                  QQRRN IFNE
      *
     C                   GOTO      STRT
      *
      *  INVALID SEARCH FIELD
      *
     C     HFLD          WHENEQ    'SCFRDT'
      *
     C                   MOVE      *ON           *IN63
     C                   GOTO      STRT
      *
      *  INVALID SEARCH FIELD
      *
     C     HFLD          WHENEQ    'SCTODT'
      *
     C                   MOVE      *ON           *IN64
     C                   GOTO      STRT
      *
      *  INVALID SEARCH FIELD
      *
     C     HFLD          WHENEQ    'SCFRYR'
      *
     C                   MOVE      *ON           *IN65
     C                   GOTO      STRT
      *
      *  INVALID SEARCH FIELD
      *
     C     HFLD          WHENEQ    'SCTOYR'
      *
     C                   MOVE      *ON           *IN66
     C                   GOTO      STRT
      *
      *  INVALID SEARCH FIELD
      *
     C     HFLD          WHENEQ    'SCSTAT'
      *
     C                   MOVE      *ON           *IN67
     C                   GOTO      STRT
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  Q2RECF=SCR01
      *
     C                   ENDIF                                                  *INKJ = *ON
      *================================================================
      *   F13=CHG USR PARM
      *================================================================
     C     *INKM         IFEQ      '1'
      *
     C                   CALL      'W0006R'
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT01
     C                   END
      *
     C                   END
      *================================================================
      *   PROCESS REQUEST SCREEN
      *================================================================
     C     SCR01T        TAG
      *================================================================
      *  VALIDATE ONLY ONE SET OF SELECTIONS WAS ENTERED
      *
     C     SCFRDT        IFNE      *ZERO
     C     SCTODT        ORNE      *ZERO
     C     SCFND1        ORNE      *ZERO
      *
     C     SCFND2        IFNE      *ZERO
     C     SCFRYR        ORNE      *ZERO
     C     SCTOYR        ORNE      *ZERO
     C     SCFCAS        ORNE      *ZERO
     C     SCTCAS        ORNE      *ZERO
     C     SCSTAT        ORNE      *BLANK
      *
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCFND2 NE 0
      *
     C                   ENDIF                                                  SCTODT NE 0
      *
      *  VALIDATE AT LEAST ONE SET OF SELECTIONS WAS ENTERED
      *
     C     SCFRDT        IFEQ      *ZERO
     C     SCTODT        ANDEQ     *ZERO
     C     SCFND1        ANDEQ     *ZERO
     C     SCFND2        ANDEQ     *ZERO
     C     SCFRYR        ANDEQ     *ZERO
     C     SCFCAS        ANDEQ     *ZERO
     C     SCTOYR        ANDEQ     *ZERO
     C     SCTCAS        ANDEQ     *ZERO
     C     SCSTAT        ANDEQ     *BLANK
      *
     C                   MOVE      *ON           *IN49
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCTODT NE 0
      *
      *  VALIDATE FIELDS ENTERED FOR GROUP OF CLAIMS
      *
     C     SCFRDT        IFEQ      *ZERO
     C     SCTODT        ANDEQ     *ZERO
     C     SCFND1        ANDEQ     *ZERO
      *
     C     SCFND2        IFNE      *ZERO
     C     SCFRYR        ORNE      *ZERO
     C     SCFCAS        ORNE      *ZERO
     C     SCTOYR        ORNE      *ZERO
     C     SCTCAS        ORNE      *ZERO
     C     SCSTAT        ORNE      *ZERO
      *
     C     SCFND2        IFEQ      *ZERO
      *
     C                   MOVE      *ON           *IN50
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
      *
     C                   ELSE                                                   SCTODT NE 0
      *
     C     SCFRYR        IFEQ      *ZERO
     C                   MOVE      *ON           *IN51
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
      *
     C                   ELSE                                                   SCTODT NE 0
      *
     C     SCFCAS        IFEQ      *ZERO
     C                   MOVE      *ON           *IN58
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
      *
     C                   ELSE
      *
     C     SCTOYR        IFEQ      *ZERO
     C                   MOVE      *ON           *IN52
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
      *
     C                   ELSE
      *
     C     SCTCAS        IFEQ      *ZERO
     C                   MOVE      *ON           *IN56
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
      *
     C                   ELSE
      *
     C     SCSTAT        IFEQ      *BLANK
     C                   MOVE      *ON           *IN57
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCSTAT EQ
      *
     C                   ENDIF                                                  SCTCAS EQ 0
      *
     C                   ENDIF                                                  SCFCAS EQ 0
      *
     C                   ENDIF                                                  SCTOYR EQ 0
      *
     C                   ENDIF                                                  SCFRYR EQ 0
      *
     C                   ENDIF                                                  SCFND2 EQ 0
      *
      *  VALIDATE THE FUND ENTERED FOR GROUP OF CLAIMS RUN
      *
     C     SCFND2        IFNE      *ZEROS
      *
     C                   MOVE      SCFND2        XXFND
      *
     C     FNKEY         CHAIN     PFN01                              60
      *
     C     *IN60         IFEQ      *ON
      *
     C                   MOVE      *ON           *IN45
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *IN60
      *
     C                   ENDIF                                                  SCFND=*ZEROS
      *
      *  VALIDATE FROM YEAR NOT GT TO YEAR
      *
     C     SCFRYR        IFGT      SCTOYR
      *
     C                   MOVE      *ON           *IN46
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
      *  VALIDATE FROM CASE# NOT GT TO CASE#
      *
     C     SCFCAS        IFGT      SCTCAS
      *
     C                   MOVE      *ON           *IN54
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
      *  VALIDATE IF "TO" CASE# ENTERED THEN "FROM" CASE# MUST BE ALSO
      *
     C     SCTCAS        IFNE      *ZERO
     C     SCTOYR        ANDEQ     *ZERO
      *
     C                   MOVE      *ON           *IN55
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
      *  VALIDATE STATUS IS "O", "C", OR "B"
      *
     C     SCSTAT        IFNE      'O'
     C     SCSTAT        ANDNE     'C'
     C     SCSTAT        ANDNE     'B'
      *
     C                   MOVE      *ON           *IN47
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
     C                   ENDIF                                                  SCFND2 NE 0
      *
     C                   ELSE
      *================================================================
      *  VALIDATE FIELDS FOR PREVIOUS DAILY BATCH RUN
      *
     C     SCTODT        IFNE      *ZERO
      *
      *  IF "TO" DATE ENTERED THEN "FROM" DATE MUST BE ENTERED
      *
     C     SCFRDT        IFEQ      *ZEROS
      *
     C                   MOVE      *ON           *IN48
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCTODT NE 0
      *
      *
      *  VALIDATE THE TO DATE ENTERED
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN43
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR = '1'
      *
     C                   ENDIF                                                  SCTODT NE 0
      *
     C     SCFRDT        IFNE      *ZERO
      *
      *  IF "FROM" DATE ENTERED THEN "TO" DATE MUST BE ENTERED
      *
     C     SCTODT        IFEQ      *ZEROS
      *
     C                   MOVE      *ON           *IN28
     C                   MOVE      *ON           *IN59
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCFRDT NE 0
      *
      *  VALIDATE THE FROM DATE ENTERED
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN42
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR = '1'
      *
     C                   ENDIF                                                  SCERR = '1'
      *
      *  DATES MUST BE ASCENDING ORDER
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT2
      *
     C     XXDAT2        IFLT      XXDAT1
     C                   MOVE      *ON           *IN53
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   END
      *
      *  VALIDATE THE FUND ENTERED FOR PREVIOUS DAILY BATCH RUN
      *
     C     SCFND1        IFNE      *ZEROS
      *
     C     SCFRDT        IFEQ      *ZEROS
      *
     C                   MOVE      *ON           *IN27
     C                   MOVE      *ON           *IN61
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
     C     SCTODT        IFEQ      *ZEROS
      *
     C                   MOVE      *ON           *IN28
     C                   MOVE      *ON           *IN62
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
     C                   MOVE      SCFND1        XXFND
      *
     C     FNKEY         CHAIN     PFN01                              60
      *
     C     *IN60         IFEQ      *ON
      *
     C                   MOVE      *ON           *IN44
     C                   MOVE      *ON           *IN20
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *IN60
      *
     C                   ENDIF                                                  SCFND=*ZEROS
      *
     C                   ENDIF                                                  SCFRDT = 0
      *
      *  ALL EDITS HAVE BEEN PASSED, GO ON
      *
      *   CALL THE CLP TO SUBMIT THE LIST.
      *
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND1        QQFND1            3
     C                   MOVE      SCFRDT        QQFRDT            8
     C                   MOVE      SCTODT        QQTODT            8
     C                   MOVE      SCFND2        QQFND2            3
     C                   MOVE      SCFRYR        QQFRYR            3
     C                   MOVE      SCTOYR        QQTOYR            3
     C                   MOVE      SCFCAS        QQFCAS            7
     C                   MOVE      SCTCAS        QQTCAS            7
     C                   MOVE      SCSTAT        QQSTAT            1
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   MOVE      'D'           QQMODE            1
      *
     C                   CALL      'W7090C2'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND1
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    QQFND2
     C                   PARM                    QQFRYR
     C                   PARM                    QQTOYR
     C                   PARM                    QQFCAS
     C                   PARM                    QQTCAS
     C                   PARM                    QQSTAT
     C                   PARM                    QQOUTQ
     C                   PARM                    QQMODE
      *
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
