     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS520R - INDEX QUEUE Maintenance
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program allows the user to add, delete, change, or    *
     H*    view INDEX QUEUE Records.                                  *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    Menu                                                       *
     H*  CALLS TO:                                                    *
     H*    IS120R - INDEX QUEUE   Search                              *
     H*    IS191C - USER Search                                       *
     H*    IS100R - DOCUMENT TYPE SEARCH                              *
     H*                                                               *
     H*  PROGRAMMER:                                                  *
     H*  CREATED:    8/8/97               PROJECT:                    *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  09    INQUIRY ONLY                                           **
     H* SCR01:                                                        **
     H*  20    INVALID INDEX QUEUE                                    **
     H*  21    USER Y/N MUST BE VALID                                 **
     H* SCR02:                                                        **
     H*  20    DESCRIPTION REQUIRED                                   **
     H*  21    DEFAULT INDEX QUEUE                                    **
     H*  22    DARTS REMARK CODE                                      **
     H*  64    DELETE NOT ALLOWED                                     **
     H*  40    CHAIN & READ Document Type FILE                        **
     H******************************************************************
     H*  FILE USAGE:                                                  *
     H*    IMIQL   (PIQ01): ID                                        *
     H*
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  04/19/01 - Adding Security level field to screen & updating
     H*             IQSECV with value from screen.
     H*
     H*****************************************************************
     FIS520F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMIQL     UF A E           K DISK
     FIMDTL     IF   E           K DISK
     FIMLBL1    UF   E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D CD              S             20    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPDSC           40
     C                   PARM                    QQCALL            1
     C*
     C*   PARM LIST Document INDEX Search
     C*
     C     PL0120        PLIST
     C                   PARM                    SCID
     C                   PARM                    XXDESC           40
     C                   PARM                    QQRTC             1
     C*
     C*
     C*   PARM LIST USER PROFILE SEARCH
     C*
     C     PL0191        PLIST
     C                   PARM                    SCID
     C                   PARM                    @@RSET            1
     C                   PARM                    QQRTC             1
     C*
     C*   PARM LIST Document Type Search
     C*
     C     PL0100        PLIST
     C                   PARM                    SCDOCT
     C                   PARM                    @@DESC           40
     C                   PARM                    QQRTC             1
     C*
     C*
     C*   PARM LIST RTVUSERC
     C*
     C     PLRTVU        PLIST
     C                   PARM                    SCID
     C                   PARM                    @@DSC            50
     C                   PARM                    @@OUTQ           10
     C                   PARM                    @@ERR             1
     C*
     C*
     C*
     C*   KEY LIST FOR INDEX QUEUE   FILE
     C     IQKY          KLIST
     C                   KFLD                    SCID
     C*
     C*
     C*   KEY LIST FOR LOAD BALANCE FILE
     C     LBKY          KLIST
     C                   KFLD                    IQID
     C*
      /Free
       scPrDs = 'Index Queue Maintenance';
      /End-Free
     C                   TIME                    DSDTTM
     C*
     C     QQCALL        COMP      '1'                                    09
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
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVE      Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      *BLANKS       SCDESC
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
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
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END                                                    END SCR02
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C********************************************
     C*   F5 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKE         IFEQ      '1'
     C********************************************
     C* SCR01 = INDEX QUEUE   SEARCh             *
     C********************************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'IS120R'      PL0120
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCID          OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END                                                    END SCR01
     C                   END                                                    END *INKE
     C********************************************
     C*   F7 = SEARCH                            *
     C********************************************
     C     *INKG         IFEQ      '1'
     C********************************************
     C* SCR01 = USER PROFILE SEARCH              *
     C********************************************
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C     @@RSET        IFEQ      *BLANK
     C                   MOVEL     'Y'           @@RSET
     C                   ELSE
     C                   MOVEL     'N'           @@RSET
     C                   ENDIF
     C*
     C                   CALL      'IS191C'      PL0191
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCID          OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   MOVEL     'Y'           SCFL01
     C*
     C                   CALL      'RTVUSRC'     PLRTVU
     C*
     C     @@ERR         IFEQ      '0'
     C                   MOVEL     @@DSC         SCDESC
     C                   ENDIF
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END                                                    END SCR01
     C                   END                                                    END *INKG
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
     C*
     C     *INKJ         IFEQ      *ON
     C********************************************
     C* SCR02 = DOC TYPE      SEARCh             *
     C********************************************
     C     Q2RECF        IFEQ      'SCR02'
     C                   CALL      'IS100R'      PL0100
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQRTC         OREQ      '*'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END SCR02
     C                   GOTO      STRT
     C                   END                                                    END *INKJ
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
     C*   F23 = DELETE INDEX QUEUE RCD           *
     C********************************************
     C     *INKX         IFEQ      '1'
     C     *IN64         ANDEQ     '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C     IQKY          DELETE    PIQ01                              60
     C                   MOVE      *BLANK        SCID
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *INKX
     C                   END                                                    END CMDKEYS
     C*************************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  INDEX QUEUE   CANNOT BE EQUAL TO BLANKS
     C*
     C     SCID          IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  USER QUEUE MUST BE "Y" OR "N"  (OR BLANKS FOR EXISTING RECORD
     C*
     C     SCFL01        IFNE      *BLANKS
     C     SCFL01        ANDNE     'Y'
     C     SCFL01        ANDNE     'N'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     SCID          CHAIN     PIQ01                              40
     C*** IF NOT EXISTING RECORD, THE VALUE IN THE USER QUEUE FIELD MUST BE
     C*** ENTERED
     C     *IN40         IFEQ      *ON
     C     SCFL01        CABEQ     *BLANKS       STRT                     21
     C                   ENDIF
     C*
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PIQ01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      IQDESC        SCDESC                         DESCRIPTION
     C                   MOVE      IQID          SCID
     C*
     C                   MOVE      IQSTAT        SCSTAT
     C                   MOVE      IQFL01        SCFL01
     C                   MOVE      IQFL02        SCFL02
     C                   MOVE      IQFL03        SCFL03
     C*** BEG ADD *** 04/19/01 ****************************************
     C                   MOVE      IQSECV        SCLVL
     C*** END ADD *** 04/19/01 ****************************************
     C***
     C                   MOVE      IQFL02        ##SV02            1
     C                   MOVE      IQSTAT        ##SVST            1
     C***
     C                   MOVEL     IQDOCT        SCDOCT
     C                   MOVEL     IQMSGQ        SCMSGQ
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
     C******************** MOVE *BLANKS   SCDESC
     C                   MOVE      *BLANKS       SCSTAT
     C*********************MOVE *BLANKS   SCFL01
     C                   MOVE      *BLANKS       SCFL02
     C                   MOVE      *BLANKS       SCFL03
     C*** BEG ADD *** 04/19/01 ****************************************
     C                   MOVE      *BLANKS       SCLVL
     C*** END ADD *** 04/19/01 ****************************************
     C                   MOVEL     *BLANKS       SCDOCT
     C                   MOVEL     *BLANKS       SCMSGQ
     C                   MOVE      '0'           *IN64                           NO DELETING
     C*****              MOVEL     *BLANKS       SCCMDD                          |
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*****************
     C     SCR02T        TAG
     C*
     C*  DESCRIPTION CANNOT BE EQUAL TO BLANKS
     C*
     C     SCDESC        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  DOCTYPE MUST BE VALID  (*IN21)
     C*
     C     SCDOCT        CHAIN     PDT01                              60
     C     *IN60         CABEQ     *ON           STRT                     21
     C*
     C*
     C*  "ACTIVE" MUST BE Y/N (INCLUDE IN ROUTING FLAG) FL02   (*IN22)
     C*
     C     SCFL02        IFNE      'Y'
     C     SCFL02        CABNE     'N'           STRT                 2222
     C                   ENDIF
     C*
     C*  "SHARED" MUST BE Y/N IF USER QUEUE                  (*IN23)
     C*
     C     SCFL01        IFEQ      'Y'
     C     SCFL03        IFNE      'Y'
     C     SCFL03        CABNE     'N'           STRT                 2323
     C                   ENDIF
     C                   ENDIF
     C*** BEG ADD *** 04/19/01 ****************************************
     C     SCLVL         IFEQ      ' '
     C                   MOVEL     '000'         SCLVL
     C                   ENDIF
     C*** END ADD *** 04/19/01 ****************************************
     C*
     C*
     C*  MSGQ MUST EXIST
     C*
     C****
     C*
     C     SCMSGQ        CABEQ     *BLANKS       STRT                     24
     C*
     C                   CALL      'SVALOBJ'
     C                   PARM                    SCMSGQ
     C                   PARM      '*MSGQ  '     ##OBTP            7
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     ##RTCD        CABEQ     '1'           STRT                     24
     C****
     C*
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     IQKY          CHAIN     PIQ01                              40
     C                   MOVEL     SCDESC        IQDESC
     C                   MOVEL     SCID          IQID
     C                   MOVE      SCSTAT        IQSTAT
     C                   MOVEL     Q1USER        IQLUSR
     C                   MOVEL     Q1PGM         IQLPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      IQLDT
     C                   Z-ADD     DSTIME        IQLTIM
     C                   MOVE      SCFL01        IQFL01
     C                   MOVE      SCFL02        IQFL02
     C*
     C     SCFL01        IFEQ      'Y'
     C                   MOVE      SCFL03        IQFL03
     C                   ELSE
     C                   MOVE      *BLANKS       IQFL03
     C                   ENDIF
     C*
     C*** BEG ADD *** 04/19/01 ****************************************
     C                   MOVEL     SCLVL         IQSECV
     C*** END ADD *** 04/19/01 ****************************************
     C                   MOVEL     SCDOCT        IQDOCT
     C                   MOVEL     SCMSGQ        IQMSGQ
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PIQ01
     C*
     C     ##SV02        CASNE     IQFL02        UPDLB
     C     ##SVST        CASNE     IQSTAT        UPDLB
     C                   ENDCS
     C*
     C                   ELSE
     C                   MOVEL     SCID          IQID
     C                   MOVEL     SCDESC        IQDESC
     C                   MOVE      SCSTAT        IQSTAT
     C                   MOVEL     Q1USER        IQIUSR
     C                   MOVEL     Q1PGM         IQIPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      IQIDT
     C                   Z-ADD     DSTIME        IQITIM
     C                   MOVE      SCFL01        IQFL01
     C                   MOVE      SCFL02        IQFL02
     C*
     C     SCFL01        IFEQ      'Y'
     C                   MOVE      SCFL03        IQFL03
     C                   ELSE
     C                   MOVE      *BLANKS       IQFL03
     C                   ENDIF
     C*
     C*** BEG ADD *** 04/19/01 ****************************************
     C                   MOVEL     SCLVL         IQSECV
     C*** END ADD *** 04/19/01 ****************************************
     C                   MOVEL     SCDOCT        IQDOCT
     C                   MOVEL     SCMSGQ        IQMSGQ
     C*
     C*
     C                   WRITE     PIQ01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCID
     C                   MOVE      *BLANKS       SCDESC
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C********************************************************************
     C     UPDLB         BEGSR
     C*
     C*** DISPLAY WINDOW STATING THAT THE LOAD BALANCE RCDS WILL BE
     C***  ACTIVATED/DEACTIVATED , OK? Y/N....
     C***  IF "Y", THEN READ ALL LB RCDS BY QUEUE, AND UPDATE THEM TO
     C*** ACTIVE OR INACTIVE ***
     C*
     C*
     C     IQFL02        IFEQ      'Y'
     C     IQSTAT        ANDEQ     'A'
     C                   MOVE      *ON           *IN31
     C                   ELSE
     C                   MOVE      *OFF          *IN31
     C                   ENDIF
     C*
     C                   EXFMT     WDW01
     C*
     C     SCFLAG        IFEQ      'Y'
     C*** NOW UPDATE THE LB'S
     C     LBKY          CHAIN     PLB01                              66
     C     *IN66         DOWEQ     *OFF
     C     IQFL02        IFEQ      'Y'
     C     IQSTAT        ANDEQ     'A'
     C                   MOVEL     'A'           LBSTAT
     C                   ELSE
     C                   MOVEL     'I'           LBSTAT
     C                   ENDIF
     C                   UPDATE    PLB01
     C     LBKY          READE     PLB01                                  66
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     OPIQ01     E            PIQ01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE CD
F23=Delete
