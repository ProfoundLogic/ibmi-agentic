     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *****************************************************************
      *TITLE SC825R - OPTICAL PURGE/DELETE COPY PROMPT PGM            *
      *****************************************************************
      *    PROGRAM DESCRIPTION:                                       *
      *        BACKUP AND BATCH - Optical Copy Documents to Optical   *
      *                           Prompt Program                      *
      *                                                               *
      *  CALLED FROM: Menu                                            *
      *                                                               *
      *  CALLS TO: M0006C                                             *
      *            SCOCVLR1                                           *
      *            SC825C                                             *
      *                                                               *
      *  CREATED: 03/31/03    PROGROGRAMMER: C:145    PROJECT: 124IAA *
      *****************************************************************
      ** REVISIONS:                                                   *
      *  CREATED:             PROGRAMMER:             PROJECT:        *
      *                                                               *
      *  03/31/02             C:145                   124IAA          *
      *                Original program for optical purge/delete copy *
      *                prompt program was SC855R.  Program renamed    *
      *                and modified to reflect changes for new        *
      *                batch/backup process.                          *
      ******************************************************************
      * INDICATOR USAGE                                                *
      *                                                                *
      ******************************************************************
      *
     FSC825F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMUPL     IF   E           K DISK
     FSMOTL     UF   E           K DISK
      *

     D COPY            C                   CONST('Copy')
     D DELETE          C                   CONST('Delete')
      *
     D INPROC          C                   CONST('in progress. Job sta-
     D                                     rted')
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
     D NORUN           C                   CONST('The Copy process can-
     D                                      not be run at this -
     D                                     time. Press F8 for P-
     D                                     urge status.')
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
     D  FLAGS                  1      2
     D  OTCPYF                 1      1
     D  OTDELF                 2      2
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
      *  Optical control file
      *

     C     OTKY          KLIST
     C                   KFLD                    QQOSYS
     C                   KFLD                    QQLIBN
      *
     C     VALKY         PLIST
     C                   PARM                    PROCES            1            C,D (COPY,DEL)
     C                   PARM                    RTNCDE            1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *  User parameter file
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *   Parm list for input data
      *
     C                   TIME                    DSDTTM
      *
      *  Get the user parameter file
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *BLANKS       UPOUTQ
     C                   END
      *
      *  Load the record format info for the first time
      *
     C                   MOVE      'G'           QQOSYS            1
     C                   MOVEL     'OPTMLB01'    QQLIBN           10
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      /Free
       scPrDs = 'Purge to Optical Process';
      /End-free
      *
      *  Display all formats
      *
     C     STRT          TAG
      *
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVE      Q2RECF        SCRECF                          FOR DISPLAY
      *
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR02'
     C                   EXFMT     SCR01
     C                   END
      *
     C**********************************************************************
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
     C********************************************
     C*   *INKH = F8-Display Optical Purge Status
     C********************************************
     C                   EXSR      GETSTS
     C                   GOTO      STRT
     C********************************************
     C*   *IN10 = PROCESS HELP REQUESTS          *
     C********************************************
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
      *  Go to the appropriate screen
      *
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
      *
      *  Process request screen
      *
     C     SCR01T        TAG
      *
      *  Cal API to see if this process can be run.
      *
     C                   MOVE      *BLANKS       ERR01
     C                   MOVE      'C'           PROCES
     C                   MOVE      *BLANKS       RTNCDE
     C                   CALL      'SCOCVLR1'    VALKY
      *
     C     RTNCDE        IFNE      *BLANK
     C                   MOVE      *ON           *IN22
     C                   MOVEL     NORUN         ERR01
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  Submit
      *    Make optical control say "in progress"
      *
     C     OTKY          CHAIN     POT01                              62
     C     *IN62         CABEQ     '1'           TERM
     C                   MOVE      'Y'           OTPPFL
     C                   UPDATE    POT01
      *
     C                   CALL      'SC825C'
     C                   PARM                    OTBKUP
      *
      *  End the program
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *************************************************************************
      *  GETSTS - Retrieve the Optical Purge Status
      *************************************************************************
     C     GETSTS        BEGSR
      *
      *
     C     OTKY          CHAIN(N)  POT01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   Z-ADD     OTPFRM        SRDTI                          Purge from date
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PFRDAT
      *
     C                   Z-ADD     OTPTO         SRDTI                          Purge to date
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PTODAT
      *
     C                   Z-ADD     OTLCPY        SRDTI                          Last Cpy date
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CSTDAT
      *
     C                   Z-ADD     OTLDEL        SRDTI                          Last Dlt date
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DSTDAT
      *
     C                   MOVE      *BLANKS       CSTAT
     C                   MOVE      *BLANKS       DSTAT
      *
     C                   SELECT
      *
     C     FLAGS         WHENEQ    'SY'
     C                   MOVE      CSTDAT        WRK08            08
     C                   EXSR      FMTDAT
      *
     C     COPY          CAT       INPROC:1      CSTAT
     C                   CAT       WRKDAT:1      CSTAT
      *
     C     MSTWT1        CAT       COPY:1        DSTAT                           DELETE
     C                   CAT       MSTWT2:1      DSTAT
      *
     C     FLAGS         WHENEQ    'YN'
      *
     C     COMP1         CAT       COPY:1        CSTAT
     C                   CAT       COMP2:1       CSTAT
      *
     C     DELETE        CAT       PROCRN:1      DSTAT
      *
     C     FLAGS         WHENEQ    'YS'
     C                   MOVE      DSTDAT        WRK08            08
     C                   EXSR      FMTDAT
      *
     C     MSTWT1        CAT       DELETE:1      CSTAT                          Copy
     C                   CAT       MSTWT2:1      CSTAT
      *
     C     DELETE        CAT       INPROC:1      DSTAT
     C                   CAT       WRKDAT:1      DSTAT
      *
     C     FLAGS         WHENEQ    'YY'
      *
     C     COPY          CAT       PROCRN:1      CSTAT
      *
     C     COMP1         CAT       DELETE:1      DSTAT
     C                   CAT       COMP2:1       DSTAT
      *
     C                   ENDSL
      *
     C                   ELSE                                                   NO HIT ON SMOTP
     C*** ERROR
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
      *** End   Add 06/24/02 ******************************************
