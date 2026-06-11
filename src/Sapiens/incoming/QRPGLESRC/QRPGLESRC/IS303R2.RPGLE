     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS303R2 - FAX ROUTING - PRINT ACTION ENTRY
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program allows the user to add, UPDATE A PRINT ACTION *
     H*         ENTRY
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
     H*  20    FAX PORT OR TSID,  NOT BOTH                            **
     H*  21    FAX PORT OR TSID ALREADY EXISTS                        **
     H*  22    DESCRIPTION CANT BE BLANK                              **
     H******************************************************************
     H*  FILE USAGE:                                                  *
     H*    IMRTAL  (PRT012): TSID/KSEQ                                     *
     H*
     H*****************************************************************
     FIS303F2   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMRTAL    UF A E           K DISK
     FIMFPL     IF   E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D XXACTC          C                   CONST('*TSI-PRINT  ')
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPTSI           20
     C                   PARM                    QQKSEQ            5 0
     C                   PARM                    QQPDSC           40
     C                   PARM                    QQCALL            1
     C*
     C                   MOVEL     QQPTSI        SCTSID
     C                   MOVEL     QQPDSC        SCDSC
     C*
     C*
     C*   KEY LIST FOR DOC INDEX FILE
     C     RTKY          KLIST
     C                   KFLD                    XXTSID           20
     C                   KFLD                    QQKSEQ            5 0
     C*
     C*   KEY LIST FOR
     C     TKY           KLIST
     C                   KFLD                    XXTSID           20
     C*
     C*
     C*   KEY LIST FOR Fax POrt Control File
     C     FPKY          KLIST
     C                   KFLD                    SCIFXP
     C*
     C                   TIME                    DSDTTM
     C*
     C     TOP           TAG
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*
     C*   DISPLAY ALL FORMATS
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     QQPTSI        IFNE      *BLANKS
     C                   MOVEL     QQPTSI        XXTSID
     C                   ENDIF
     C     RTKY          CHAIN     PRT012                             40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PRT01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      RTOUTQ        SCOUTQ
     C                   MOVE      RT#CPY        SC#CPY
     C                   MOVE      RTPDLT        SCPDLT
     C                   MOVEL     RTIFXP        SCIFXP
     C*
     C*
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCOUTQ
     C                   MOVE      *BLANKS       SCIFXP
     C                   MOVE      *ZERO         SC#CPY
     C                   MOVE      *BLANKS       SCPDLT
     C                   MOVE      '0'           *IN64                           NO DELETING
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
     C     FLD           IFEQ      'SCIFXP  '
     C*** CALL FAX PORT SEARCH PROGRAM
     C                   CALL      'IS140R'
     C                   PARM                    SCIFXP
     C                   PARM      *BLANKS       ##DSC            40
     C                   PARM      'S'           ##RTC             1
     C*
     C     ##RTC         CABEQ     'A'           TERM                     LR
     C     ##RTC         CABEQ     'B'           STRT
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*
     C                   GOTO      STRT
     C***
     C                   END                                                    END *INKL
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
     C*
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C                   GOTO      STRT
     C                   ENDIF                                                  END *IN11
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
     C*  OUTQ MUST EXIST
     C*
     C****
     C*
     C     SCOUTQ        CABEQ     *BLANKS       STRT                     21
     C*
     C                   CALL      'SVALOBJ'
     C                   PARM                    SCOUTQ
     C                   PARM      '*OUTQ  '     ##OBTP            7
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     ##RTCD        CABEQ     '1'           STRT                     21
     C****
     C*  VALID # COPIES
     C*
     C     SC#CPY        CABLE     *ZERO         STRT                   2323
     C*
     C*  PRINT THEN DELETE FLAG Y/N
     C*
     C*******    SCPDLT    IFNE 'Y'
     C********   SCPDLT    ANDNE'N'
     C******               MOVE *ON       *IN24
     C******               GOTO STRT
     C******               ENDIF
     C*
     C*  Fax POrt ID must Exist
     C*
     C     SCIFXP        IFNE      '*ALL    '
     C     FPKY          CHAIN     PFP01                              60
     C     *IN60         CABEQ     *ON           STRT                     22
     C                   ENDIF
     C*
     C*
     C*
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     RTKY          CHAIN     PRT012                             40
     C**** ALSO INCLUDE HERE PUTING THE CORRECT FLAGS IN THE RTP FILE
     C**** AND*** PUTTING THE ACTC FIELD, ETC. ETC.
     C                   MOVEL     'A'           RTSTAT
     C*
     C                   MOVEL     XXACTC        RTACTC
     C                   MOVEL     SCOUTQ        RTOUTQ
     C                   MOVE      SCIFXP        RTIFXP
     C                   MOVE      SC#CPY        RT#CPY
     C                   MOVE      SCPDLT        RTPDLT
     C                   MOVEL     Q1USER        RTLUSR
     C                   MOVEL     Q1PGM         RTLPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      RTLDT
     C                   Z-ADD     DSTIME        RTLTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PRT012
     C                   ELSE
     C*** get Next avail SEQ#
     C     TKY           SETGT     IMRTAL
     C     TKY           READPE    IMRTAL                                 60
     C     *IN60         IFEQ      *OFF
     C     RTKSEQ        ADD       1             @@KSEQ            5 0
     C                   ELSE
     C                   Z-ADD     1             @@KSEQ
     C                   ENDIF
     C*
     C* *** WRITE  ... MAKE SURE ALL FIELDS HERE (ACTC) AND IN THE RTP
     C* *** FILE ARE UPDATED (FLAGS) GET NEXT AVAIL SEQ#
     C*
     C                   MOVEL     XXACTC        RTACTC
     C                   Z-ADD     @@KSEQ        RTKSEQ
     C                   MOVEL     XXTSID        RTTSID
     C                   MOVEL     SCOUTQ        RTOUTQ
     C                   MOVE      SCIFXP        RTIFXP
     C                   MOVE      SC#CPY        RT#CPY
     C                   MOVE      SCPDLT        RTPDLT
     C                   MOVEL     Q1USER        RTIUSR
     C                   MOVEL     Q1PGM         RTIPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      RTIDT
     C                   Z-ADD     DSTIME        RTITIM
     C*
     C                   WRITE     PRT012
     C                   END
     C                   END
     C**** Get the IMRTP file to Update the FLAG specifing a QUEUE action
     C****  exists for this TSID.......
      *
      *
      *
      *
      *
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPRT012    E            PRT01E
**  TABLE UA
-- ADD --
- UPDATE -
