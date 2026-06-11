     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS530R - WORKSTATION ID FILE MAINTENANCE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program allows the user to add, delete, change, or    *
     H*    view WORKSTATION ID Records.                                *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    Menu                                                       *
     H*  CALLS TO:                                                    *
     H*    IS130R - WORKSTATION ID SEARCH                             *
     H*                                                               *
     H*  PROGRAMMER:                                                  *
     H*  CREATED:    8/8/97               PROJECT:                    *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  09    INQUIRY ONLY                                           **
     H* SCR01:                                                        **
     H*  20    INVALID WORKSTAION ID                                  **
     H* SCR02:                                                        **
     H*  20    JOB NAME    REQUIRED                                   **
     H*  21    XXXXXXXXXXXXXXXXXXX                                    **
     H*  22    XXXXXXXXXXXXXXXXXXX                                    **
     H*  64    DELETE NOT ALLOWED                                     **
     H*  40    CHAIN & READ Document Type FILE                        **
     H******************************************************************
      ** REVISIONS:
      *
      *  06/09/20  C:212 Project : SY420
      *                  Mask SSN
      *                  Recompile only
      *
      * 10/09/03 C:128 Project:
      *                Changed to configure all workstation ID'S
      *                'VW' with the exception of scan stations.
      *                Scan Stations must have a unique workstation ID.
      *                The job name which consists of: LU + A, B, or C
      *                will now be unique in the IMWSP file.
      *
     H*  FILE USAGE:                                                  *
     H*    IMWSL   (PWS02): WSID                                      *
     H*    IMWSL2  (PWS01): JBNM                                      *
     H*
     H*****************************************************************
     FIS530F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
      *** Begin Change 10/09/03 ***************************************
     F*****IMWSL   UF  E           K        DISK                      A
     FIMWSL     IF   E           K DISK
     F                                     RENAME(PWS01:PWS02)
     FIMWSL2    UF A E           K DISK
      *** End   Change 10/09/03 ***************************************

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
     C*   PARM LIST Workstation ID Search
     C*
     C     PL0130        PLIST
     C                   PARM                    SCWSID
     C                   PARM                    QQRTC             1
     C*
     C*
     C*
     C*   KEY LIST FOR INDEX QUEUE   FILE
     C     WSKY          KLIST
      *** Begin Change 10/09/03 ***************************************
     C******               KFLD           SCWSID
     C                   KFLD                    SCJBNM
      *** End   Change 10/09/03 ***************************************
     C*
      /Free
       scPrds = 'Workstation ID Maintenance';
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
      *** Begin Change 10/09/03 ***************************************
     C********************************************
     C* SCR01 = Job Name Search                 *
     C********************************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      '2'           QQRTC
     C                   CALL      'IS130R'
     C                   PARM                    SCJBNM
     C                   PARM                    QQRTC
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCWSID        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
      *
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END                                                    END SCR01
     C********************************************
     C* SCR02 = Workstation ID Search            *
     C********************************************
     C     Q2RECF        IFEQ      'SCR02'
     C                   CALL      'IS130R'      PL0130
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCWSID        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
      *
     C     QQRTC         CABEQ     '*'           SCR02T
     C                   END                                                    END SCR02
      *** End   Change 10/09/03 ***************************************
     C                   END                                                    END *INKE
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
     C*   F23 = DELETE WORKSTATION ID RCD        *
     C********************************************
     C     *INKX         IFEQ      '1'
     C     *IN64         ANDEQ     '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C     WSKY          DELETE    PWS01                              60
     C                   MOVE      *BLANK        SCWSID
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
      *** Begin Change 10/09/03 ***************************************
     C*
     C*  Job Name cannot be equal to blanks
     C*
     C******     SCWSID    IFEQ *BLANKS
     C     SCJBNM        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C******     SCWSID    CHAINPWS01                40
     C     SCJBNM        CHAIN     PWS01                              40
      *** End   Change 10/09/03 ***************************************
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PWS01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      WSJBNM        SCJBNM
      *** Begin Change 10/09/03 ***************************************
     C                   MOVE      *BLANKS       SVWSID            2            SAVE FOR UPDATE
     C                   MOVE      WSWSID        SVWSID
     C                   MOVE      WSWSID        SCWSID
      *** End   Change 10/09/03 ***************************************
     C                   MOVE      WSLUNM        SCLUNM
     C                   MOVE      WSVQUE        SCVQUE
     C                   MOVE      WSVQLB        SCVQLB
     C                   MOVE      WSSRCV        SCSRCV
     C                   MOVE      WSSRLB        SCSRLB
     C                   MOVE      WSSSND        SCSSND
     C                   MOVE      WSSSLB        SCSSLB
     C                   MOVE      WSSCAP        SCSCAP
     C                   MOVE      WSIDRV        SCIDRV
     C**** ADD 8/20/99 - VIEW RCV QUEUE/LIB - VISUAL DARTS (WORKFLOW)
     C                   MOVE      WSVRQ         SCVRQ
     C                   MOVE      WSVRQL        SCVRQL
     C*
     C                   MOVE      WSSTAT        SCSTAT
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
      *** Begin Change 10/09/03 ***************************************
     C*************        MOVE *BLANKS   SCJBNM
      *** Begin Change 10/09/03 ***************************************
     C                   MOVE      *BLANKS       SCLUNM
     C                   MOVE      *BLANKS       SCVQUE
     C                   MOVE      *BLANKS       SCVQLB
     C                   MOVE      *BLANKS       SCSRCV
     C                   MOVE      *BLANKS       SCSRLB
     C                   MOVE      *BLANKS       SCSSND
     C                   MOVE      *BLANKS       SCSSLB
     C                   MOVE      *BLANKS       SCSCAP
     C                   MOVE      *BLANKS       SCIDRV
     C**** ADD 8/20/99 - VIEW RCV QUEUE/LIB - VISUAL DARTS (WORKFLOW)
     C                   MOVE      *BLANKS       SCVRQ
     C                   MOVE      *BLANKS       SCVRQL
     C*
     C                   MOVE      *BLANKS       SCSTAT
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
      *** Begin Change 10/09/03 ***************************************
      * If scan capable = 'Y' then workstation ID must be unique.
     C     SCSCAP        IFEQ      'Y'
     C*
     C*  Workstation connot be blank
     C*
     C     SCWSID        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
      *
     C     SCWSID        CHAIN     PWS02                              41
     C     *IN41         IFEQ      *OFF
     C     SVWSID        ANDNE     SCWSID
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDIF
      *** End   Change 10/09/03 ***************************************
     C*
     C*** VALIDATE LIBS / SCAN CAPABLE Y OR N , AND VALID NAMES?
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     WSKY          CHAIN     PWS01                              40
     C                   MOVEL     SCJBNM        WSJBNM
      *** Begin Change 10/09/03 ***************************************
     C     SCWSID        IFEQ      *BLANKS
     C                   MOVEL     'VW'          WSWSID
     C                   ELSE
     C                   MOVEL     SCWSID        WSWSID
     C                   ENDIF
      *** End   Change 10/09/03 ***************************************
     C                   MOVE      SCLUNM        WSLUNM
     C                   MOVE      SCVQUE        WSVQUE
     C                   MOVE      SCVQLB        WSVQLB
     C                   MOVE      SCSRCV        WSSRCV
     C                   MOVE      SCSRLB        WSSRLB
     C                   MOVE      SCSSND        WSSSND
     C                   MOVE      SCSSLB        WSSSLB
     C                   MOVE      SCSCAP        WSSCAP
     C                   MOVE      SCIDRV        WSIDRV
     C**** ADD 8/20/99 - VIEW RCV QUEUE/LIB - VISUAL DARTS (WORKFLOW)
     C***
     C                   MOVE      SCVRQ         WSVRQ
     C                   MOVE      SCVRQL        WSVRQL
     C**
     C                   MOVE      SCSTAT        WSSTAT
     C                   MOVEL     Q1USER        WSLUSR
     C                   MOVEL     Q1PGM         WSLPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      WSLDT
     C                   Z-ADD     DSTIME        WSLTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PWS01
     C*
     C*** CREATE THE DATA QUEUES IF THEY DONT EXIST ***
     C                   CALL      'ISCRTDQ2'
     C                   PARM                    WSSSND
     C                   PARM                    WSSSLB
     C                   PARM                    WSSRCV
     C                   PARM                    WSSRLB
     C                   PARM                    WSVQUE
     C                   PARM                    WSVQLB
     C**** ADD 8/20/99 - VIEW RCV QUEUE/LIB - VISUAL DARTS (WORKFLOW)
     C                   PARM                    WSVRQ
     C                   PARM                    WSVRQL
     C                   PARM                    RTNCDE            1
     C*
     C                   ELSE
     C                   MOVEL     SCWSID        WSWSID
     C                   MOVEL     SCJBNM        WSJBNM
     C                   MOVE      SCLUNM        WSLUNM
     C                   MOVE      SCVQUE        WSVQUE
     C                   MOVE      SCVQLB        WSVQLB
     C                   MOVE      SCSRCV        WSSRCV
     C                   MOVE      SCSRLB        WSSRLB
     C                   MOVE      SCSSND        WSSSND
     C                   MOVE      SCSSLB        WSSSLB
     C                   MOVE      SCSCAP        WSSCAP
     C                   MOVE      SCIDRV        WSIDRV
     C                   MOVE      SCSTAT        WSSTAT
     C**** ADD 8/20/99 - VIEW RCV QUEUE/LIB - VISUAL DARTS (WORKFLOW)
     C*
     C                   MOVE      SCVRQ         WSVRQ
     C                   MOVE      SCVRQL        WSVRQL
     C*
     C                   MOVEL     Q1USER        WSIUSR
     C                   MOVEL     Q1PGM         WSIPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      WSIDT
     C                   Z-ADD     DSTIME        WSITIM
     C*
     C                   WRITE     PWS01
     C*
     C                   CALL      'ISCRTDQ2'
     C                   PARM                    WSSSND
     C                   PARM                    WSSSLB
     C                   PARM                    WSSRCV
     C                   PARM                    WSSRLB
     C                   PARM                    WSVQUE
     C                   PARM                    WSVQLB
     C**** ADD 8/20/99 - VIEW RCV QUEUE/LIB - VISUAL DARTS (WORKFLOW)
     C                   PARM                    WSVRQ
     C                   PARM                    WSVRQL
     C                   PARM                    RTNCDE            1
     C*
     C                   END
     C                   END
     C*
      *** End   Change 10/09/03 ***************************************
     C     SCSCAP        IFEQ      'Y'
     C                   MOVE      '2'           QQRTC
     C                   CALL      'IS510R'
     C                   PARM                    SCWSID
     C                   PARM                    QQRTC
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCWSID        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
      *
     C     QQRTC         CABEQ     '*'           SCR02T
     C                   GOTO      TERM
     C                   END                                                    END SCR02
      *** End   Change 10/09/03 ***************************************
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCWSID
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPWS01     E            PWS01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE CD
F23=Delete
