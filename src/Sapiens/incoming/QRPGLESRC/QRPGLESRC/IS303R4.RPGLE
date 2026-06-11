     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS303R4 - FAX ROUTING - INDEX  ACTION ENTRY
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program allows the user to add, UPDATE A INDEX  ACTION*
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
     H*    IMRTAL  (PRT014): TSID/KSEQ                                     *
     H*
     H*****************************************************************
     FIS303F4   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMRTAL    UF A E           K DISK
     FIMFPL     IF   E           K DISK
     FSMSLL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FSMKTL     IF   E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D XXACTC          C                   CONST('*TSI-INDEX  ')
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
     C**** DARTS USER FILE
     C*
     C     SLKY          KLIST
     C                   KFLD                    SCDUSR
     C* REMARK CODE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    XXRMCD            5
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
     C     RTKY          CHAIN     PRT014                             40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PRT01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     RTKEY         SCKEY
     C                   MOVEL     RTKYTP        SCKYTP
     C                   MOVEL     RTCOMM        SCCOMM
     C                   MOVEL     RTDUSR        SCDUSR
     C                   MOVEL     RTRMCD        SCRMCD
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
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVEL     *BLANKS       SCKYTP
     C                   MOVEL     *BLANKS       SCCOMM
     C                   MOVEL     *BLANKS       SCDUSR
     C                   MOVEL     *BLANKS       SCRMCD
     C                   MOVE      *BLANKS       SCIFXP
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
     C     FLD           IFEQ      'SCRMCD  '
     C*
     C                   CALL      'SC580R'
     C                   PARM      *BLANKS       SCRMCD
     C                   PARM                    QQRTCD            1
     C*
     C* IF QQRTCD = "A", USER PRESSED CMD1 FROM SEARCH.  TERMINATE
     C* THIS SEARCH
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C* IF THE USER SELECTED A RECORD, OR PRESSED CMD2, REDISPLAY
     C* SCR01 OF SEARCH.
     C*
     C     QQRTCD        IFEQ      'B'
     C*
     C                   WRITE     SCR99
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  END QQRTCD EQ 'B'
     C*
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF
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
     C     FLD           IFEQ      'SCKYTP  '
     C                   CALL      'SC591R'
     C                   PARM                    SCKYTP
     C                   PARM      *BLANKS       QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCKYTP        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C****
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     FLD           IFEQ      'SCDUSR  '
     C                   CALL      'SC530R'
     C                   PARM                    SCDUSR
     C                   PARM                    QQRTCD
     C*
     C* IF QQRTCD = "A", USER PRESSED CMD1 FROM SEARCH.  TERMINATE
     C* THIS SEARCH
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C* IF THE USER SELECTED A RECORD, OR PRESSED CMD2, REDISPLAY
     C* SCR01 OF SEARCH.
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQRTCD        OREQ      '*'
     C*
     C                   WRITE     SCR99
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  END QQRTCD EQ 'B'
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  END SCDUSR
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
     C*  KEY MUST BE ENTERED
     C     SCKEY         CABEQ     *BLANKS       STRT                     23
     C*
     C** KEY TYPE MUST BE VALID *******  24 *******
     C*
     C     SCKYTP        CABEQ     *BLANKS       STRT                     24
     C     SCKYTP        CHAIN     PKT01                              60
     C     *IN60         CABEQ     *ON           STRT                     24
     C*
     C*
     C*  COMMENT MUSTBE ENTERED
     C     SCCOMM        CABEQ     *BLANKS       STRT                     25
     C*
     C*
     C** DIARY USER MUST BE VALID 26 *******
     C*
     C* IF THE DIARY USER FIELD IS ENTERED, THE USER ID MUST BE VALID
     C*
     C     SCDUSR        IFNE      *BLANKS
     C     SLKY          CHAIN     PSL01                              60
     C     *IN60         CABEQ     *ON           STRT                     26
     C                   ENDIF
     C****
     C*  REMARK CODE MSUT BE VALID
     C*
     C     SCRMCD        CABEQ     *BLANKS       STRT                     27
     C*
     C* THE REMARK CODE MUST BE A VALID REMARK CODE, AND ACTIVE
     C*
     C                   MOVEL     SCRMCD        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         CABEQ     *ON           STRT                     27
     C*
     C     CTSTAT        CABEQ     'I'           STRT                     27
     C*
     C*
     C*  Fax PORT ID must Exist
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
     C     RTKY          CHAIN     PRT014                             40
     C**** ALSO INCLUDE HERE PUTING THE CORRECT FLAGS IN THE RTP FILE
     C**** AND*** PUTTING THE ACTC FIELD, ETC. ETC.
     C                   MOVEL     'A'           RTSTAT
     C*
     C                   MOVEL     XXACTC        RTACTC
     C                   MOVEL     SCKEY         RTKEY
     C                   MOVEL     SCKYTP        RTKYTP
     C                   MOVEL     SCCOMM        RTCOMM
     C                   MOVEL     SCDUSR        RTDUSR
     C                   MOVEL     SCRMCD        RTRMCD
     C                   MOVEL     SCIFXP        RTIFXP
     C                   MOVEL     Q1USER        RTLUSR
     C                   MOVEL     Q1PGM         RTLPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      RTLDT
     C                   Z-ADD     DSTIME        RTLTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PRT014
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
     C                   MOVEL     SCKEY         RTKEY
     C                   MOVEL     SCKYTP        RTKYTP
     C                   MOVEL     SCCOMM        RTCOMM
     C                   MOVEL     SCDUSR        RTDUSR
     C                   MOVEL     SCRMCD        RTRMCD
     C                   MOVEL     SCIFXP        RTIFXP
     C                   MOVEL     Q1USER        RTIUSR
     C                   MOVEL     Q1PGM         RTIPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      RTIDT
     C                   Z-ADD     DSTIME        RTITIM
     C*
     C                   WRITE     PRT014
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
     OPRT014    E            PRT01E
**  TABLE UA
-- ADD --
- UPDATE -
