     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SC850R - OPTICAL PURGE/DELETE PROMPT PGM
     H*****************************************************************
      **   PROGRAM DESCRIPTION:                                      **
      **       PROMPT                                                **
      **   CALLED FROM: Menu                                         **
      **                                                             **
      *  CALLS TO:                                                    *
      *   SC850C      OPTICAL PURGE/DELETE SUBMITTER                  *
      **   PROGRAMMER: C:12                                          **
      **   CREATED:    05/04/93     PROJECT: OP2                     **
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  09/14/12  C:053  Project: SY195
     H*                            Add edit for IFS install date
     H*
     H**  C:02    120IAD   -  MODIFY TO GET THE SMOCP FILE TO RETRIEVE
     H**    01/26/04          THE OPTICAL LIBRARY NAME
     H**
     H* 06/24/02 C:128 Project: DR099                                 *
     H*                Added F8 to display status of Purge process.
     H*                Added call to SCOCUPR an API to validate that
     H*                this previous process has completed and that
     H*                this process is safe to run.
     H*
     H******************************************************************
     H* INDICATOR USAGE                                                *
     H*  20    SCM0500 - DATE IS INVALID.                              *
     H******************************************************************
     FSC850F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMUPL     IF   E           K DISK
      *** Begin Add 06/24/02 ******************************************
     FSMOCLA    IF   E           K DISK
     F**** ADD 01/26/04 ***********************************************
     FSMOCP     IF   E             DISK
     F                                     RENAME(POC01:POC02)
     F**** END ADD 01/26/04 *******************************************
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
      *** BEG ADD *** 09/14/12 *******************************************
     D date_mdyy       s               d   inz datfmt(*usa)
     D IFSdt           s                   like(updt)
     D xxtodt          s                   like(updt)
     D IFSDATE        uds
     D xxdate                        10a
      *** END ADD *** 09/14/12 *******************************************

     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCALL            1
      *** Begin Add 06/24/02 ******************************************
      *
     C     OCKY          KLIST
     C                   KFLD                    QQOSYS            1
     C                   KFLD                    QQLIBN           10
     C*
     C                   MOVE      'G'           QQOSYS
     C                   MOVEL     'OPTMLB01'    QQLIBN
     C*** 01/26/04 ** ADD *****************************************8
     C     1             CHAIN     SMOCP                              64
     C     *IN64         IFEQ      *OFF
     C                   MOVEL     OCOSYS        QQOSYS
     C                   MOVEL     OCLIBN        QQLIBN
     C                   ENDIF
     C*** 01/26/04 ** END ADD *************************************8
      *
     C     VALKY         PLIST
     C                   PARM                    PROCES            1            S,C,D (SEL,CPY,DEL)
     C                   PARM                    RTNCDE            1
      *
      *** End   Add 06/24/02 ******************************************
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
     C*
     C                   TIME                    DSDTTM
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
     C     STRT          TAG
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVE      Q2RECF        SCRECF                          FOR DISPLAY
     C*
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
     C*   VALIDATE THE DATE FIELD
     C*
     C     SCRDAT        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ELSE
     C*
     C                   Z-ADD     SCRDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRDAT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   END
     C*   VALIDATE THE DATE FIELD
     C*
     C     SCTODT        CABEQ     *ZEROS        STRT                     21
     C*
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         CABEQ     '1'           STRT                     21
      *
      *** BEG ADD *** 09/14/12 *******************************************
     C                   move      xxdate        date_mdyy
     C     *cymd         move      date_mdyy     IFSdt
     C                   move      SCTODT        date_mdyy
     C     *cymd         move      date_mdyy     xxtodt
     C     xxtodt        cabge     IFSdt         STRT                 23  23
      *** END ADD *** 09/14/12 *******************************************
      *
      *** Begin Add 06/24/02 ******************************************
      *
      *  Cal API to see if this process can be run.
      *
     C                   MOVE      'S'           PROCES
     C                   MOVE      *BLANKS       RTNCDE
     C                   CALL      'SCOCVLR'     VALKY
      *
     C     RTNCDE        IFNE      *BLANK
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF
      *** End   Add 06/24/02 ******************************************
     C*
     C* PASSED SCREEN EDITS
     C*
     C                   MOVE      SCRDAT        QQRDAT
     C*
     C                   MOVE      SCTODT        QQTODT
     C                   CALL      'SC850C'
     C                   PARM                    QQRDAT            8
     C                   PARM                    QQTODT            8
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
