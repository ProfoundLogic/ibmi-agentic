     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      ******************************************************************
      ******************************************************************
     H/TITLE SC853R - OPTICAL PURGE/DELETE PROMPT PGM
      ******************************************************************
      *    PROGRAM DESCRIPTION:                                        *
      *        PROMPT                                                  *
      *    CALLED FROM: Menu                                           *
      *                                                                *
      *  CALLS TO:                                                     *
      *   SC853C      OPTICAL PURGE/DELETE SUBMITTER                   *
      *    PROGRAMMER: C:167                                           *
      *    CREATED:    12/19/05     PROJECT: 129IMD                    *
      ******************************************************************
      *
      ******************************************************************
      * INDICATOR USAGE                                                *
      *  20            -                                               *
      *  21            -                                               *
      *  22            -                                               *
      *  23            -                                               *
      ******************************************************************
     FSC853F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMUPL     IF   E           K DISK
     FSMOCLA    IF   E           K DISK
     FSMOCP     IF   E             DISK
     F                                     RENAME(POC01:POC02)
      *

     D SELECT          C                   CONST('Selection')
     D COPY            C                   CONST('Copy')
     D DELETE          C                   CONST('Delete')
      *
     D INPROC          C                   CONST('in process. Job star-
     D                                     ted')
      *
     D COMP1           C                   CONST('The')
     D COMP2           C                   CONST('process has complete-
     D                                     d.')
      *
     D MSTWT1          C                   CONST('Must wait until')
     D MSTWT2          C                   CONST('is complete.')
      *
     D PROCRN          C                   CONST('process can now be r-
     D                                     un.')
      *
     D                 DS
     D  WRKDAT                 1     10
     D  WRKMON                 1      2
     D  WRKSP1                 3      3
     D  WRKDAY                 4      5
     D  WRKSP2                 6      6
     D  WRKYR                  7     10
      *
     D                 DS
     D  FLAGS                  1      3
     D  OCSELF                 1      1
     D  OCCPYF                 2      2
     D  OCDELF                 3      3
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
      *  USER PARAMETER FILE
      *

     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *   PARM LIST FOR INPUT DATA.
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCALL            1
      *
     C     OCKY          KLIST
     C                   KFLD                    QQOSYS            1
     C                   KFLD                    QQLIBN           10
      *
     C                   MOVE      'G'           QQOSYS
     C                   MOVEL     'OPTMLB01'    QQLIBN
     C     1             CHAIN     SMOCP                              64
     C     *IN64         IFEQ      *OFF
     C                   MOVEL     OCOSYS        QQOSYS
     C                   MOVEL     OCLIBN        QQLIBN
     C                   ENDIF
      *
     C     VALKY         PLIST
     C                   PARM                    PROCES            1            S,C,D (SEL,CPY,DEL)
     C                   PARM                    RTNCDE            1
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C                   TIME                    DSDTTM
      *
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *BLANKS       UPOUTQ
     C                   END
      *
     C     STRT          TAG
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
      *   DISPLAY ALL FORMATS
      *
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVE      Q2RECF        SCRECF                          FOR DISPLAY
      *
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR02'
     C                   EXFMT     SCR01
     C                   END
      *
      **********************************************************************
      *   PROCESS ALL COMMAND KEYS
      *
     C     *IN11         IFEQ      '1'
      ********************************************
      *   F3 = EXIT                              *
      ********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
      *** Begin Add 06/24/02 ******************************************
      ********************************************
      *   *INKH = F8-Display Optical Purge Status
      ********************************************
     C                   EXSR      GETSTS
     C                   GOTO      STRT
      *** End   Add 06/24/02 ******************************************
      ********************************************
      *   *IN10 = PROCESS HELP REQUESTS          *
      ********************************************
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
      *
     C                   GOTO      STRT
     C                   END                                                    END *IN10
      ********************************************
     C                   END                                                    END CMDKEYS
      *************************************************************************
      *
      *   GOTO THE APPROPRIATE SCREEN
      *
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
      *
      *   PROCESS REQUEST SCREEN
      *
     C     SCR01T        TAG
      *   VALIDATE THE POLICY YEAR FIELD
      *
     C     SCPOLY        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
      *   VALIDATE THE ENTITY FIELD
      *
     C     SCENTY        CABEQ     *BLANKS       STRT                     23
     C     SCENTY        IFNE      'C'
     C     SCENTY        ANDNE     'P'
     C     SCENTY        ANDNE     'B'
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  Cal API to see if this process can be run.
      *
     C                   MOVE      'S'           PROCES
     C                   MOVE      *BLANKS       RTNCDE
     C                   CALL      'SCOCVLR'     VALKY
      *
     C*          RTNCDE    IFNE *BLANK
     C*                    MOVE *ON       *IN22
     C*                    GOTO STRT
     C*                    ENDIF
      *
      * PASSED SCREEN EDITS
      *
     C                   MOVE      SCPOLY        QQPOLY
     C                   MOVE      SCENTY        QQENTY
     C                   CALL      'SC853C'
     C                   PARM                    QQPOLY            3
     C                   PARM                    QQENTY            1
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *** Begin Add 06/24/02 ******************************************
      *************************************************************************
      *  GETSTS - Retrieve the Optical Purge Status
      *************************************************************************
     C     GETSTS        BEGSR
      *
      *
     C     OCKY          CHAIN     POC01                              62
      *
     C     *IN62         IFEQ      *OFF
      *
     C                   Z-ADD     OCPFRM        SRDTI                          Purge from date
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PFRDAT
      *
     C                   Z-ADD     OCPTO         SRDTI                          Purge to date
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PTODAT
      *
     C                   Z-ADD     OCLSEL        SRDTI                          Last Sel date
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SSTDAT
      *
     C                   Z-ADD     OCLCPY        SRDTI                          Last Cpy date
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CSTDAT
      *
     C                   Z-ADD     OCLDEL        SRDTI                          Last Dlt date
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DSTDAT
      *
     C                   MOVE      *BLANKS       SSTAT
     C                   MOVE      *BLANKS       CSTAT
     C                   MOVE      *BLANKS       DSTAT
      *
     C                   SELECT
      *
     C     FLAGS         WHENEQ    'YNN'
     C     SELECT        CAT       INPROC:1      SSTAT                          Selection
     C                   CAT       WRKDAT:1      SSTAT
      *
     C     MSTWT1        CAT       SELECT:1      CSTAT                          Copy
     C                   CAT       MSTWT2:1      CSTAT
      *
     C     MSTWT1        CAT       COPY:1        DSTAT                          Delete
     C                   CAT       MSTWT2:1      DSTAT
      *
     C     FLAGS         WHENEQ    'YNN'
     C     COMP1         CAT       SELECT:1      SSTAT
     C                   CAT       COMP2:1       SSTAT
      *
     C     COPY          CAT       PROCRN:1      CSTAT
      *
     C     MSTWT1        CAT       COPY:1        DSTAT
     C                   CAT       MSTWT2:1      DSTAT
      *
     C     FLAGS         WHENEQ    'YSN'
     C                   MOVE      CSTDAT        WRK08            08
     C                   EXSR      FMTDAT
     C     COMP1         CAT       SELECT:1      SSTAT
     C                   CAT       COMP2:1       SSTAT
      *
     C     COPY          CAT       INPROC:1      CSTAT
     C                   CAT       WRKDAT:1      CSTAT
      *
     C     MSTWT1        CAT       COPY:1        DSTAT                           elete
     C                   CAT       MSTWT2:1      DSTAT
      *
     C     FLAGS         WHENEQ    'YYN'
     C     COMP1         CAT       SELECT:1      SSTAT
     C                   CAT       COMP2:1       SSTAT
      *
     C     COMP1         CAT       COPY:1        CSTAT
     C                   CAT       COMP2:1       CSTAT
      *
     C     DELETE        CAT       PROCRN:1      DSTAT
      *
     C     FLAGS         WHENEQ    'YYS'
     C                   MOVE      DSTDAT        WRK08            08
     C                   EXSR      FMTDAT
     C     COMP1         CAT       SELECT:1      SSTAT
     C                   CAT       COMP2:1       SSTAT
      *
     C     COMP1         CAT       COPY:1        CSTAT
     C                   CAT       COMP2:1       CSTAT
      *
     C     DELETE        CAT       INPROC:1      DSTAT
     C                   CAT       WRKDAT:1      DSTAT
      *
     C     FLAGS         WHENEQ    'YYY'
     C     SELECT        CAT       PROCRN:1      SSTAT
      *
     C     COMP1         CAT       COPY:1        CSTAT
     C                   CAT       COMP2:1       CSTAT
      *
     C     COMP1         CAT       DELETE:1      DSTAT
     C                   CAT       COMP2:1       DSTAT
      *
     C                   ENDSL
      *
      *
     C                   ELSE                                                   NO HIT ON SMOCP
      *** ERROR
     C                   ENDIF                                                  END *IN62 = 0
      *
     C                   MOVE      *BLANKS       SCRECF
     C                   MOVEL     'SCR02'       SCRECF
     C                   EXFMT     SCR02
      *
     C                   ENDSR
      *************************************************************************
      *  FMTDAT - Format the status date.
      *************************************************************************
     C     FMTDAT        BEGSR
      *
     C                   MOVE      WRK08         WRKYR
     C                   MOVEL     WRK08         WRK04             4
     C                   MOVE      WRK04         WRKDAY
     C                   MOVEL     WRK04         WRKMON
     C                   MOVE      '/'           WRKSP1
     C                   MOVE      '/'           WRKSP2
      *
     C                   ENDSR
