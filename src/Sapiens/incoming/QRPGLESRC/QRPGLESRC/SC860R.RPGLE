     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SC850R - OPTICAL DELETE PROCESS PROMPT PGM
     H*****************************************************************
      ****************************************************************
      **   PROGRAM DESCRIPTION:                                      **
      **       PROMPT                                                **
      **   CALLED FROM: Menu                                         **
      **                                                             **
      *  CALLS TO:                                                    *
      *   SC850C      OPTICAL DELETE SUBMITTER                        *
      **   PROGRAMMER: C:12                                          **
      **   CREATED:    05/12/93     PROJECT: OP2                     **
     H******************************************************************
     H* INDICATOR USAGE                                                *
     H*  20    SCM0502 - TAPE FLAG MUST BE Y OR N.                     *
     H*  21    SCM0503 - VOLUME ID MUST NOT BE BLANKS.                 *
     H*  22    SCM0503 - TAPE DEVICE MUST NOT BE BLANKS.               *
     H******************************************************************
     H* Value of RTNCD FOR CALL CHKTPC:
     H* 1 Command ended in error
     H* 2 Cannot allocate device
     H* 3 Incorrct volume found on device.
     H* 4 Device not a tape device.
     H* 5 Load failure occurred on device.                                    */
     H* 6 Device not ready.              .
     H* 7 Volume on device cannot be processed
     H* 8 Device not found.
     H* 9 Not authorized to device.
     H******************************************************************
     H*  REVISIONS:                                                   **
     H**  C:02    120IAD   -  MODIFY TO GET THE SMOCP FILE TO RETRIEVE
     H**           01/26/04   THE OPTICAL LIBRARY NAME
     H*                                                               **
     H* 06/24/02 C:128 Project: DR099                                 *
     H*                Added F8 to display status of Purge process.
     H*                Added call to SCOCUPR an API to validate that
     H*                this previous process has completed and that
     H*                this process is safe to run.
     H*
     H*  01/23/94 C:09 PROJECT ATB                                    **
     H*                DEFAULT "SAVE TO TAPE" FLAG TO 'N' PER REQUEST **
     H*                FROM TOM B. AND SUE H.  ALL DOCUMENTS ON       **
     H*                SYSTEM WILL NOW BE SAVED PRIOR TO PURGE USING  **
     H*                A SAVDLO(*ALL) FLR(*ALL).                      **
     H*                                                               **
     H*  09/21/01 C:128 Project FX095 Seq# 29004                      **
     H*                Removed Tape prompt fields from screen so that **
     H*                user would not think that a Tape process was   **
     H*                still supported.                               **
     H*                Replaced with "Press <ENTER> to continue" so   **
     H*                the purge process could continueto Optical.    **
     H*                                                               **
     H******************************************************************
     FSC860F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMUPL     IF   E           K DISK
     FSMDCP     UF   E             DISK
      *** Begin Add 06/24/02 ******************************************
     FSMOCLA    IF   E           K DISK
     F**** ADD 01/26/04 ***********************************************
     FSMOCP     IF   E             DISK
     F                                     RENAME(POC01:POC02)
     F**** END ADD 01/26/04 *******************************************
      *** End   Add 06/24/02 ******************************************
      *

     D DLM             S             68    DIM(9) CTDATA PERRCD(1)              ERROR MESSAGE
      *
      *** Begin Add 06/24/02 ******************************************
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
     D NORUN           C                   CONST('The Delete process c-
     D                                     an not be run at thi-
     D                                     s time. Press F8 for-
     D                                     Purge status.')
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

     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *** Begin Add 06/24/02 ******************************************
      *
     C     OCKY          KLIST
     C                   KFLD                    QQOSYS            1
     C                   KFLD                    QQLIBN           10
      *
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
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCALL            1
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
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
     C                   MOVE      'N'           SCTAPF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVE      Q2RECF        SCRECF                          FOR DISPLAY
     C*
      /Free
       scPrDs = 'Optical Delete Process';
      /End-Free
     C     Q2RECF        IFEQ      'SCR01'
      *** Begin Add 06/24/02 ******************************************
     C     Q2RECF        OREQ      'SCR02'
      *** End   Add 06/24/02 ******************************************
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
     C*   F12= PREVIOUS                          *
     C********************************************
     C     *INKL         IFEQ      '1'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *INKL
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
     C*
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
     C*   EDIT THE TAPE FLAG
     C*
     C     SCTAPF        IFNE      'Y'
     C     SCTAPF        ANDNE     'N'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*   VOLUME ID CANNOT BE BLANKS
     C     SCVOLI        IFEQ      *BLANKS
     C     SCTAPF        ANDEQ     'Y'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*   TAPE DEVICE CANNOT BE BLANKS
     C     SCTDEV        IFEQ      *BLANKS
     C     SCTAPF        ANDEQ     'Y'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     SCTAPF        IFEQ      'Y'
      * EXECUTE VERIFY TAPE DRIVE WINDOW
     C                   MOVE      *BLANKS       SCMSG
     C                   EXFMT     WDW01
     C********************************************
     C*   F12= PREVIOUS                          *
     C********************************************
     C     *IN11         IFEQ      '1'
     C     *INKL         ANDEQ     '1'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  END *INKL
      *
     C                   MOVE      *BLANKS       QQTDEV
     C                   MOVE      *BLANKS       QQVOLI
     C                   MOVE      SCTDEV        QQTDEV
     C                   MOVE      SCVOLI        QQVOLI
     C                   CALL      'CHKTPC'
     C                   PARM                    QQTDEV           10
     C                   PARM                    QQVOLI           10
     C                   PARM      ' '           QQRTCD            1
     C                   ENDIF
     C     QQRTCD        IFNE      '  '
     C                   MOVE      QQRTCD        X                 1 0
     C                   MOVE      *BLANKS       SCMSG
     C                   MOVEL     DLM(X)        SCMSG
      * EXECUTE SCREEN WITH ERROR.
     C                   EXFMT     WDW01
      *
     C                   WRITE     SCR99
      *
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
     C                   GOTO      STRT
     C                   ENDIF
      *** Begin Add 06/24/02 ******************************************
      *
      *  Cal API to see if this process can be run.
      *
     C                   MOVE      *BLANKS       ERR01
     C                   MOVE      'D'           PROCES
     C                   MOVE      *BLANKS       RTNCDE
     C                   CALL      'SCOCVLR'     VALKY
      *
     C     RTNCDE        IFNE      *BLANK
     C                   MOVE      *ON           *IN23
     C                   MOVEL     NORUN         ERR01
     C                   GOTO      STRT
     C                   ENDIF
      *** End   Add 06/24/02 ******************************************
     C*
     C***** MAKE DELETE CONTROL SAY IN PROGRESS ***
     C     1             CHAIN     PDC01                              62
     C     *IN62         CABEQ     '1'           TERM
     C                   MOVE      'Y'           DCDPFL
     C                   UPDATE    PDC01
     C*
     C*
     C* PASSED SCREEN EDITS
     C* Submit
     C                   MOVE      *BLANKS       QQTDEV
     C                   MOVE      *BLANKS       QQVOLI
     C                   MOVE      SCTAPF        QQTAPF
     C                   MOVE      SCTDEV        QQTDEV
     C                   MOVE      SCVOLI        QQVOLI
     C                   CALL      'SC860C'
     C                   PARM                    QQTAPF            1
     C                   PARM                    QQTDEV           10
     C                   PARM                    QQVOLI           10
     C*
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
     C     MSTWT1        CAT       COPY:1        DSTAT
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
     C                   MOVE      DSTDAT        WRK08
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
      /Free
       scPrDs = 'Optical Purge Status';
      /End-Free
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
**
CHKTAP - Command ended in error.
CHKTAP - Cannot allocate device.
CHKTAP - Incorrect volume found on device.
CHKTAP - Device not a tape device.
CHKTAP - Load failure occurred on device.
CHKTAP - Device not ready.
CHKTAP - Volume on device cannot be processed.
CHKTAP - Device not found.
CHKTAP - Not authorized to device.
