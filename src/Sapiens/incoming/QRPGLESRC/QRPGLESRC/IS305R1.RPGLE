     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS305R1 - QUEUE ASSIGNMENT ADD/UPDATE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program allows the user to add, UPDATE A QUEUE ASSINME*
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    Menu                                                       *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  PROGRAMMER:                                                  *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  09    INQUIRY ONLY                                           **
     H* SCR01:                                                        **
     H*  20    QUEUE ASSN. NOT VALID                                  **
     H*  21    FXXXXXXXXXXXXXXXXALREADY EXISTS                        **
     H*  22    XXXXXXXXXXXXXXXXXBE BLANK                              **
     H******************************************************************
     H*  FILE USAGE:                                                  *
     H*    IMQAL   (PQA01): FQUE                                      *
     H*
     H*****************************************************************
     FIS305F1   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMQAL2    UF A E           K DISK
     FIMQAL     IF   E           K DISK
     F                                     RENAME(PQA01:PQA02)
     FIMIQL3    IF   E           K DISK
     F*

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D CD              S             20    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
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
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YMD'        SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8            MDYY
     C                   PARM      '*YYMD'       SRTFMT            8            YYYYMMDD
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFQUE           10
     C                   PARM                    QQTQUE           10
     C                   PARM                    QQFDT             8
     C                   PARM                    QQTDT             8
     C                   PARM                    QQCALL            1
     C*
     C*
     C*   KEY LIST FOR         X FILE
     C     QAKY          KLIST
     C                   KFLD                    XXFQUE           10
     C                   KFLD                    XXTQUE           10
     C                   KFLD                    XXFDT             8 0
     C                   KFLD                    XXTDT             8 0
     C*
     C*
     C*   KEY LIST FOR         X FILE
     C     QAKYD         KLIST
     C                   KFLD                    ##FQUE           10
     C                   KFLD                    ##TQUE           10
     C                   KFLD                    ##FDT             8 0
     C                   KFLD                    ##TDT             8 0
     C*
     C*   KEY LIST FOR         X FILE
     C     QAKY1         KLIST
     C                   KFLD                    ##FQUE
     C                   KFLD                    ##FDT
     C*
     C*   KEY LIST FOR         X FILE
     C     QAKY2         KLIST
     C                   KFLD                    ##FQUE
     C*
     C*
     C*   KEY LIST FOR         X FILE
     C     IQKY1         KLIST
     C                   KFLD                    ##FQUE
     C*
     C*   KEY LIST FOR         X FILE
     C     IQKY2         KLIST
     C                   KFLD                    ##TQUE
     C*
      /Free
       scPrDs = 'Queue Assignment Entry';
      /End-Free
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'ISCVTDT'     DTYYMD
     C                   MOVEL     SRDTO         ##TODY            8 0
     C*
     C     TOP           TAG
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     QQFQUE        IFNE      *BLANKS
     C                   MOVEL     QQFQUE        XXFQUE
     C                   MOVEL     QQTQUE        XXTQUE
     C*** CVT THE FROM/TO DATES
     C*
     C                   MOVE      QQFDT         SRDTI
     C                   CALL      'ISCVTDT'     DTYYMD
     C                   MOVEL     SRDTO         XXFDT
     C*
     C*
     C                   MOVE      QQTDT         SRDTI
     C                   CALL      'ISCVTDT'     DTYYMD
     C                   MOVEL     SRDTO         XXTDT
     C                   ENDIF
     C     QAKY          CHAIN     PQA01                              40
     C     *IN40         IFEQ      '0'
     C*** KEEP LOCKED? *** EXCPTPQA01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     QAFQUE        SCFQUE
     C                   MOVEL     QATQUE        SCTQUE
     C*
     C                   Z-ADD     QAFDT         SRDTI
     C                   CALL      'ISCVTDT'     DTMDYY
     C                   MOVEL     SRDTO         SCFDT
     C*
     C*
     C                   Z-ADD     QATDT         SRDTI
     C                   CALL      'ISCVTDT'     DTMDYY
     C                   MOVEL     SRDTO         SCTDT
     C*
     C                   MOVEL     QAFQUE        SVFQUE           10
     C                   MOVEL     QATQUE        SVTQUE           10
     C                   MOVE      QAFDT         SVFDT             8 0
     C                   MOVE      QATDT         SVTDT             8 0
     C*
     C*
     C                   MOVE      QASTAT        SCSTAT
     C     SCSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN64                          ALLOW DELETING
     C*****              MOVEL     CD(1)         SCCMDD                         |
     C                   ELSE                                                   |
     C                   MOVE      '0'           *IN64                          NO DELETING
     C*****              MOVEL     *BLANKS       SCCMDD                         |
     C                   END                                                    |
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCFQUE
     C                   MOVE      *BLANKS       SCTQUE
     C                   MOVE      *ZEROS        SCFDT
     C                   MOVE      *ZEROS        SCTDT
     C                   MOVE      '0'           *IN64                           NO DELETING
     C*****              MOVEL     *BLANKS       SCCMDD                          |
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
     C*
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVE      Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
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
     C                   MOVEL     'A'           QQCALL
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C********************************************
     C*   F10 = SEARCH                           *
     C********************************************
     C     *INKJ         IFEQ      '1'
     C*
     C     FLD           IFEQ      'SCFQUE  '
     C*** CALL FAX PORT SEARCH PROGRAM
     C                   CALL      'IS120R'
     C                   PARM      SCFQUE        Q@QUE            10
     C                   PARM      *BLANKS       ##DSC            40
     C                   PARM      ' '           ##RTC             1
     C*
     C     ##RTC         CABEQ     'A'           TERM                     LR
     C     ##RTC         CABEQ     'B'           STRT
     C*
     C                   MOVEL     Q@QUE         SCFQUE
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*
     C     FLD           IFEQ      'SCTQUE  '
     C*** CALL FAX PORT SEARCH PROGRAM
     C                   CALL      'IS120R'
     C                   PARM      SCTQUE        Q@QUE            10
     C                   PARM      *BLANKS       ##DSC            40
     C                   PARM      ' '           ##RTC             1
     C*
     C     ##RTC         CABEQ     'A'           TERM                     LR
     C     ##RTC         CABEQ     'B'           STRT
     C*
     C                   MOVEL     Q@QUE         SCTQUE
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C***
     C                   END                                                    END *INKL
     C********************************************
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C                   MOVEL     'B'           QQCALL
     C                   GOTO      TERM
     C                   END                                                    END *INKL
     C********************************************
     C*   *IN10 = PROCESS HELP REQUESTS          *
     C********************************************
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
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C********************************************
     C*   F20 = CHANGE STATUS                    *
     C********************************************
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   MOVE      '1'           *IN64                           ALLOW DELETE
     C*****              MOVEL     CD(1)         SCCMDD                          |
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      '0'           *IN64                           NO DELETING
     C*****              MOVEL     *BLANKS       SCCMDD                          |
     C                   END
     C                   GOTO      STRT
     C                   END                                                     END *INKU
     C********************************************
     C*   F23 = DELETE  RCD        *
     C********************************************
     C     *INKX         IFEQ      '1'
     C     *IN64         ANDEQ     '1'
     C     QAKY          DELETE    PQA01                              60
     C                   MOVE      *BLANK        XXFQUE
     C                   MOVE      *BLANK        XXTQUE
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     ' '           QQCALL
     C                   GOTO      TERM
     C                   END                                                    END *INKX
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
     C*
     C*
     C                   MOVEL     SCFQUE        ##FQUE           10
     C                   MOVEL     SCTQUE        ##TQUE           10
     C*** DATES TO YYYYMMDD FMT.
     C*
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'ISCVTDT'     DTYYMD
     C                   MOVEL     SRDTO         ##FDT             8 0
     C*
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'ISCVTDT'     DTYYMD
     C                   MOVEL     SRDTO         ##TDT             8 0
     C*
     C*** VALID FROM QUEUE
     C     IQKY1         CHAIN     PIQ01                              60
     C     *IN60         CABEQ     *ON           STRT                     21
     C*** VALIDE TO QUEUE
     C     IQKY2         CHAIN     PIQ01                              60
     C     *IN60         CABEQ     *ON           STRT                     22
     C*
     C*** VALID FROM DATE
     C*
     C*
     C     SCFDT         CABEQ     *ZEROS        STRT                     23
     C*
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'ISCVTDT'     DATCHK
     C                   MOVEL     SRDTO         SCFDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     23
     C*
     C*** VALID TO DATE
     C*
     C     SCTDT         CABEQ     *ZEROS        STRT                     24
     C*
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'ISCVTDT'     DATCHK
     C                   MOVEL     SRDTO         SCTDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     24
     C*
     C*** VALID FROM DATE MUST BE LE TO DATE  (25)
     C*
     C     ##FDT         CABGT     ##TDT         STRT                 25
     C*
     C*  DATES CANT ALREADY EXIST IN A RECORD.    (26)
     C*
     C                   MOVE      *OFF          *IN60
     C     QAKY1         SETGT     PQA02
     C     QAKY2         READPE    PQA02                                  60
     C     QAFQUE        IFEQ      SVFQUE
     C     QATQUE        ANDEQ     SVTQUE
     C     QAFDT         ANDEQ     SVFDT
     C     QATDT         ANDEQ     SVTDT
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C   60QAKY1         SETLL     PQA02
     C   60QAKY2         READE     PQA02                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     QAFQUE        IFEQ      SVFQUE
     C     QATQUE        ANDEQ     SVTQUE
     C     QAFDT         ANDEQ     SVFDT
     C     QATDT         ANDEQ     SVTDT
     C                   GOTO      BYPS1
     C                   ENDIF
     C*
     C     QAFDT         CABEQ     ##FDT         STRT                     26
     C*
     C     QAFDT         IFGT      ##FDT
     C     QAFDT         ANDLE     ##TDT
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     ##FDT         IFGT      QAFDT
     C     ##FDT         ANDLE     QATDT
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     QATDT         CABEQ     ##TDT         STRT                     27
     C*
     C     QATDT         IFGE      ##FDT
     C     QATDT         ANDLE     ##TDT
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*
     C     ##TDT         IFGE      QAFDT
     C     ##TDT         ANDLE     QATDT
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     BYPS1         TAG
     C     QAKY2         READE     PQA02                                  60
     C                   ENDDO
     C*
     C*
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C*********  QAKYD     CHAINPQA01                40
     C*
     C                   MOVEL     SCFQUE        QAFQUE
     C                   MOVEL     SCTQUE        QATQUE
     C*** DATES ***
     C                   MOVE      ##FDT         QAFDT
     C                   MOVE      ##TDT         QATDT
     C                   MOVE      SCSTAT        QASTAT
     C                   MOVEL     Q1USER        QALUSR
     C                   MOVEL     Q1PGM         QALPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      QALDT
     C                   Z-ADD     DSTIME        QALTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PQA01
     C                   ELSE
     C*
     C*
     C                   MOVEL     SCFQUE        QAFQUE
     C                   MOVEL     SCTQUE        QATQUE
     C*** DATES ***
     C                   MOVE      ##FDT         QAFDT
     C                   MOVE      ##TDT         QATDT
     C                   MOVE      SCSTAT        QASTAT
     C                   MOVEL     Q1USER        QAIUSR
     C                   MOVEL     Q1PGM         QAIPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      QAIDT
     C                   Z-ADD     DSTIME        QAITIM
     C*
     C                   WRITE     PQA01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     O***********PQA01   E                PQA01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE CD
F23=Delete
