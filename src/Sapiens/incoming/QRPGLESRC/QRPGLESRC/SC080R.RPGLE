     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SC080R - REMARK TYPE FILE MAINTENANCE
      *****************************************************************
      * REVISIONS
      *  03/05/92 WLD   - Proj: AJP
      *                   1.  Added logical with 5 byte remark code as  ARPNAM
      *                       key (CTRMCD). Changed size of remark type
      *                       to 5A instead of 2A on Screens 1 and 2.
      *  03/05/92 WLD   - Proj: ANF
      *                   1.  Added Summary Code field to SCR02. This   ARPNAM
      *                       is displayed only if SDRPF/DRFL02 = 'Y'
      *                       Added F6=REMARKS SUMMARY CODE SEARCH
      *  03/05/92 WLD   - Proj: AND
      *                   1.  Added '# of Days to Keep on System Before ARPNAM
      *                       Purge' field.
      *  03/05/92 WLD   - Proj: ANG
      *                   1.  Added Data Entry Program ID field.        ARPNAM
      *  04/15/92 WLD   - Proj:
      *                   1.  Add new column for 'Sub-Area Code'        ARPNAM
      *                       conditioned on SDRPF/DRFL05 = Y.
      *
      *  09/10/92 WLD   - Proj: ANG
      *                   1.  Only display Data Entry program field if  ARPNAM
      *                       DRFL03 = 'Y'.
      *
      *  07/26/93 C:10    PROJ: ATD
      *                   1.  ALLOW USER TO ENTER '# OF DAYS ON SYSTEM' ARPNAM
      *                       AND 'DATA ENTRY PROGRAM ID' AT THE SAME
      *                       TIME.  (REMOVED EDIT)
      *
      *  09/12/95 C:05    PROJ: DR010
      *                   1.  Add new field to allow the user to establish a
      *                       default security level that will be used in
      *                       SC010R on Add-a-Remark.
     H******************************************************************
     H* INDICATOR USAGE                                                *
     H*  09    INQUIRY ONLY                                            *
     H* SCR01:                                                         *
     H*  20    REMARK  TYPE REQUIRED                                   *
     H* SCR02:                                                         *
     H*  20    DESCRIPTION                                             *
     H*  21    Y/N REQUIRED FOR DIARY TYPE FLAG                        *
     H*  64    DELETE NOT ALLOWED                                      *
     H*  91    IF DRFL02 = 'N', DON'T DISPLAY SUMMARY CODE             *
     H* N91    IF DRFL02 = 'Y', SUMMARY CODE IS DISPLAYED              *
     H*  92    IF DRFL05 = 'N', AUTO-FOLDER SUB-AREA CODE IS DISPLAYED *
     H* N92    IF DRFL05 = 'Y', DON'T DISPLAY SUB-AREA CODE            *
     H*  93    IF DRFL03 = 'N', DON'T DISPLAY DATA ENTRY FIELDS        *
     H* N93    IF DRFL03 = 'Y', DATA ENTRY FIELDS ARE DISPLAYED        *
     H******************************************************************
     FSC080F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSTCTL1    UF A E           K DISK
     FSTSML     IF   E           K DISK
     FSSYLF     IF   E           K DISK

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
     C                   PARM                    QQCALL            1
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
     C*   KEY LIST FOR REMARK TYPE MASTER FILE
     C     CTKEY         KLIST
     C                   KFLD                    SCCODE
     C*
     C*   KEY LIST FOR REMARK SUMMARY CODE MASTER FILE
     C*
     C     SMKY1         KLIST
     C                   KFLD                    SCSMCD
     C*
     C*  DARTS CONTROL FILE
     C*
     C     DRKY          KLIST
     C                   KFLD                    XSKEY             2
     C*
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
     C********************************************
     C* SCR01 = REMARK  TYPE SEARCH              *
     C********************************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'SC580R'
     C                   PARM                    SCCODE
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCCODE        OREQ      *BLANK
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
     C*   F6 = REMARK SUMMARY CODE SEARCH        *
     C********************************************
     C     *INKF         IFEQ      '1'
     C********************************************
     C* SCR02 = REMARK SUMMARY CODE SEARCH       *
     C********************************************
     C     Q2RECF        IFEQ      'SCR02'
     C                   CALL      'SC585R'
     C                   PARM                    SCSMCD
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCSMCD        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      '1'           *IN31                          POS CUR SCSMCD
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN32                          POS CUR SCDPRG
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END                                                    END SCR02
     C                   END                                                    END *INKF
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
     C*   F20 = CHANGE STATUS                    *
     C********************************************
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   MOVE      '1'           *IN64                           ALLOW DELETE
     C                   MOVEL     CD(1)         SCCMDD                          |
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      '0'           *IN64                           NO DELETING
     C                   MOVEL     *BLANKS       SCCMDD                          |
     C                   END
     C                   GOTO      STRT
     C                   END                                                     END *INKU
     C********************************************
     C*   F23 = DELETE CODE                      *
     C********************************************
     C     *INKX         IFEQ      '1'
     C     *IN64         ANDEQ     '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C     CTKEY         DELETE    PCT01                              60
     C                   MOVE      *BLANKS       SCCODE
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                     END *INKX
     C*
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
     C*  REMARK TYPE CANNOT BE EQUAL TO BLANKS
     C     SCCODE        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     SCCODE        CHAIN     PCT01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PCT01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      CTDESC        SCDESC
     C                   MOVE      CTSMCD        SCSMCD
     C                   MOVE      CTDPRG        SCDPRG
     C                   MOVE      CTDEPG        SCDEPG
     C                   MOVE      CTTYPE        SCSACD                         ADD 04/15/92
     C                   MOVE      CTFL01        SCFL01
     C                   MOVE      CTDSLV        SCDSLV                         ADD 09/12/95
     C*
     C                   MOVE      CTSTAT        SCSTAT
     C     SCSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN64                          ALLOW DELETING
     C                   MOVEL     CD(1)         SCCMDD                         |
     C                   ELSE                                                   |
     C                   MOVE      '0'           *IN64                          NO DELETING
     C                   MOVEL     *BLANKS       SCCMDD                         |
     C                   END                                                    |
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      'N'           SCFL01
     C                   MOVE      '0'           *IN64                           NO DELETING
     C                   MOVEL     *BLANKS       SCCMDD                          |
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      *BLANKS       SCSMCD
     C                   MOVE      *BLANKS       SCDPRG
     C                   MOVE      *BLANKS       SCDEPG
     C                   MOVE      *BLANKS       SCSACD                         ADD 04/15/92
     C                   MOVE      *ZEROS        SCDSLV                         ADD 09/12/95
     C*
     C                   END
     C*
     C*  IF DRFL02 = 'Y' IN FILE SDRPF, SUMMARY CODE FIELD IS
     C*     DISPLAYED
     C*
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'SC'          XSKEY
     C     DRKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
     C     DRFL02        IFEQ      'Y'
     C                   MOVE      '0'           *IN91
     C                   ELSE
     C                   MOVE      '1'           *IN91
     C                   END
     C*
     C*  IF DRFL05 = 'Y' IN FILE SDRPF, SUB-AREA CODE FIELD IS
     C*     DISPLAYED
     C*
     C     DRFL05        IFEQ      'Y'
     C                   MOVE      '0'           *IN92
     C                   ELSE
     C                   MOVE      '1'           *IN92
     C                   END
     C*
     C*  IF DRFL03 = 'Y' IN FILE SDRPF, DATA ENTRY FIELD IS
     C*     DISPLAYED
     C*
     C     DRFL03        IFEQ      'Y'
     C                   MOVE      '0'           *IN93
     C                   ELSE
     C                   MOVE      '1'           *IN93
     C                   END
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*  DESCRIPTION CANNOT BE EQUAL TO BLANKS
     C     SCDESC        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  DIARY TYPE FLAG MUST BE Y OR N
     C     SCFL01        IFNE      'Y'
     C     SCFL01        ANDNE     'N'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  REMARKS SUMMARY CODE MUST BE VALID IF ENTERED
     C*
     C     SCSMCD        IFNE      *BLANKS
     C     SMKY1         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C     *IN91         ANDEQ     '0'
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     CTKEY         CHAIN     PCT01                              40
     C                   MOVE      SCDESC        CTDESC
     C                   MOVE      SCSTAT        CTSTAT
     C                   MOVE      SCFL01        CTFL01
     C                   MOVE      SCSMCD        CTSMCD
     C                   Z-ADD     SCDPRG        CTDPRG
     C                   MOVE      SCDEPG        CTDEPG
     C                   MOVE      SCSACD        CTTYPE                         ADD 04/15/92
     C                   MOVE      SCDSLV        CTDSLV                         ADD 09/12/95
     C                   MOVE      Q1USER        CTUSER
     C                   MOVE      Q1PGM         CTPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTDT
     C*
     C                   Z-ADD     DSTIME        CTTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PCT01
     C                   ELSE
     C                   MOVEL     SCCODE        CTRMCD
     C                   MOVEL     Q1USER        CTIUSR
     C                   WRITE     PCT01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCCODE
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPCT01     E            PCT01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE CD
F23=Delete
