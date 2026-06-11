     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SC855R - OPTICAL PURGE/DELETE CPY TO OPTICAL
     H*****************************************************************
      ****************************************************************
      **   PROGRAM DESCRIPTION:                                      **
      **       PROMPT                                                **
      **   CALLED FROM: Menu                                         **
      **                                                             **
      *  CALLS TO:                                                    *
      *   SC855C                                                      *
      **   PROGRAMMER: C:12                                          **
      **   CREATED:    05/17/93     PROJECT: OP2                     **
     H*****************************************************************
     H** REVISIONS:
     H*
     H*   11/04/2008  C:220   SUPPORT 62309
     H*                       OPTICAL PURGE PROGRAM MUST GIVE MESSAGE
     H*
     H**  C:02    120IAD   -  MODIFY TO GET THE SMOCP FILE TO RETRIEVE
     H**           01/26/04   THE OPTICAL LIBRARY NAME
     H* 06/24/02 C:128 Project: DR099                                 *
     H*                Added F8 to display status of Purge process.
     H*                Added call to SCOCUPR an API to validate that
     H*                this previous process has completed and that
     H*                this process is safe to run.
     H*
     H******************************************************************
     H******************************************************************
     H* INDICATOR USAGE                                                *
     F**** BEG ADD 11/04/08 *****************************************
     H* 55    TO READ RECORD FORMAT PPR01(SMPRL1)
     F**** END ADD 11/04/08 *****************************************
     H******************************************************************
     H******************************************************************
     FSC855F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMUPL     IF   E           K DISK
     FSMOCL     UF   E           K DISK
     F**** ADD 01/26/04 ***********************************************
     FSMOCP     IF   E             DISK
     F                                     RENAME(POC01:POC02)
     F**** END ADD 01/26/04 *******************************************
      *** Begin Add 06/24/02 ******************************************
     F**** BEG ADD 11/04/2008 *****************************************
     FSMPRL1    IF   E             DISK
     F**** END ADD 11/04/2008 *****************************************
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
     D  FLAGS                  1      3
     D  OCSELF                 1      1
     D  OCCPYF                 2      2
     D  OCDELF                 3      3
      *** End   Add 06/24/02 ******************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*  OPTICAL CONTROL FILE
     C*
     C     OCKY          KLIST
     C                   KFLD                    QQOSYS
     C                   KFLD                    QQLIBN
      *** Begin Add 06/24/02 ******************************************
      *
     C     VALKY         PLIST
     C                   PARM                    PROCES            1            S,C,D (SEL,CPY,DEL)
     C                   PARM                    RTNCDE            1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
      *
      *** End   Add 06/24/02 ******************************************
     C*
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C*
     C*
     C                   TIME                    DSDTTM
     C*
     C*
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *BLANKS       UPOUTQ
     C                   END
      *
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C*** 01/26/04 ** ADD *****************************************8
     C     1             CHAIN     SMOCP                              64
     C     *IN64         IFEQ      *OFF
     C                   MOVEL     OCOSYS        QQOSYS            1
     C                   MOVEL     OCLIBN        QQLIBN           10
     C                   ENDIF
     C*** 01/26/04 ** END ADD *************************************8
     C*
     C*** DEL 1/26/04 ***  MOVE 'G'       QQOSYS  1
     C*** DEL 1/26/04 ***  MOVEL'OPTMLB01'QQLIBN 10
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVE      Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR02'
      *** Begin Add 11/04/08 ******************************************
      *   TO CHECK AT LEAST ONE RECORD SELECTION
      *
     C                   READ      PPR01                                  55
     C     *IN55         DOWEQ     '0'
     C     PRSTAT        IFEQ      'P'
     C                   LEAVE
     C                   ENDIF
     C                   READ      PPR01                                  55
     C                   ENDDO
     C                   MOVE      *OFF          *IN44
     C     PRSTAT        IFNE      'P'
     C                   MOVE      *ON           *IN44
     C                   ENDIF
      *
      *** END   Add 11/04/08 ******************************************
     C                   EXFMT     SCR01
     C                   END
     C*
     C**********************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C********************************************
     C*   F3 = EXIT                              *
     C********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
      *** Begin Add 06/24/02 ******************************************
     C********************************************
     C*   *INKH = F8-Display Optical Purge Status
     C********************************************
     C                   EXSR      GETSTS
     C                   GOTO      STRT
      *** End   Add 06/24/02 ******************************************
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
     C*
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C********************************************
     C                   END                                                    END CMDKEYS
     C*************************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
      *
      *** Begin Add 06/24/02 ******************************************
      *
      *  Cal API to see if this process can be run.
      *
      *
     C                   MOVE      *BLANKS       ERR01
     C                   MOVE      'C'           PROCES
     C                   MOVE      *BLANKS       RTNCDE
     C                   CALL      'SCOCVLR'     VALKY
      *
     C     RTNCDE        IFNE      *BLANK
     C                   MOVE      *ON           *IN22
     C                   MOVEL     NORUN         ERR01
     C                   GOTO      STRT
     C                   ENDIF
      *** End   Add 06/24/02 ******************************************
     C* Submit
     C***** MAKE OPTICAL CONTROL SAY IN PROGRESS ***
     C     OCKY          CHAIN     POC01                              62
     C     *IN62         CABEQ     '1'           TERM
     C                   MOVE      'Y'           OCPPFL
     C                   UPDATE    POC01
      *** BEG   Add 11/04/08 ******************************************
     C     *IN44         IFEQ      *OFF
      *** END   Add 11/40/08 ******************************************
     C*
     C                   CALL      'SC855C'
     C*
      *** BEG   Add 11/04/08 ******************************************
     C                   ENDIF
      *** END   Add 11/04/08 ******************************************
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *** Begin Add 06/24/02 ******************************************
      *************************************************************************
      *  GETSTS - Retrieve the Optical Purge Status
      *************************************************************************
     C     GETSTS        BEGSR
      *
      *
     C     OCKY          CHAIN(N)  POC01                              60
      *
     C     *IN60         IFEQ      *OFF
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
     C     FLAGS         WHENEQ    'SNN'
     C                   MOVE      SSTDAT        WRK08            08
     C                   EXSR      FMTDAT
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
