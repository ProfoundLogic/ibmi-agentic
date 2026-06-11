     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N91D9R - D9 DISCOUNT CODE MAINT - SCRN PGM
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM: N91D9R                                              *
     H*  DESCRIPTION: D9 DISCOUNT CODE - MAINTENANCE PROGRAM          *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  10/03/06 C:62  Project: 71WHI
     H*                 RETURN TO BASE
     H*
     H*****************************************************************
     FN91D9F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FNMBDLD    UF A E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D CD              S             20    DIM(1) CTDATA PERRCD(1)
     D TD              S             70    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DSCOM2                 1     60
     D  DSSRCE                51     51
     D  DSLESS                52     52
     D  DSEXTR                53     60
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  BILLING CODE TABLE FILE
     C*
     C     BDKY1         KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    YMEFDT            7 0
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCODE            3
     C                   PARM                    QQEFDT            8
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
     C                   MOVE      QQEFDT        XXEFDT            8 0
     C*
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         YMEFDT
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*** MOVE IN VALUES FROM PARMS TO SCREEN.
     C*
     C                   MOVE      'D9'          SCTYPE
     C                   MOVEL     TD(1)         SCTDSC
     C                   MOVEL     *BLANKS       SCCMDD
     C*
     C*** GET RECORD IF IT EXISTS, ELSE ITS AN ADD
     C*
     C     BDKY1         CHAIN     PBD01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVE      BDCODE        SCCODE
     C                   Z-ADD     BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   MOVE      BDSTAT        SCSTAT
     C                   MOVE      BDCOM1        SCCOM1
     C                   SUBST(P)  BDCOM2:1      XXCOM2           50
     C                   MOVEL(P)  XXCOM2        SCCOM2
     C                   SUBST(P)  BDCOM2:51     XXSRCE            1
     C                   MOVE      XXSRCE        SCSRCE
     C                   SUBST(P)  BDCOM2:52     XXLESS            1
     C                   MOVE      XXLESS        SCLESS
     C                   MOVEL     BDPC01        SCPC01
     C                   MOVEL     BDPC02        SCPC02
     C                   MOVE      '0'           *IN41                          UPDATE EXISTING
     C                   MOVE      UA(2)         SCMODE                         RECORD
     C     SCSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN64                          ALLOW DELETEING
     C                   MOVEL     CD(1)         SCCMDD                         |           E
     C                   ELSE                                                   |
     C                   MOVE      '0'           *IN64                          NO DELETING ING
     C                   MOVEL     *BLANKS       SCCMDD                         |           E
     C                   END                                                    |
     C*
     C                   ELSE
     C                   MOVE      QQCODE        SCCODE
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C                   Z-ADD     YMEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   MOVE      *ZEROS        SCPC01
     C                   MOVE      *ZEROS        SCPC02
     C                   MOVE      *BLANKS       SCSRCE
     C                   MOVE      *BLANKS       SCLESS
     C*
     C                   MOVE      '1'           *IN41                          ADD RECORD BY
     C                   MOVE      UA(1)         SCMODE                         BRINGING PRIOR
     C                   MOVE      '0'           *IN64                           NO DELETING
     C                   MOVEL     *BLANKS       SCCMDD                          |          E
     C                   MOVE      'A'           SCSTAT                         RCD'S DATA
     C*
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVEL     Q2RECF        SCRECF
     C*
     C                   MOVEL     Q2FILE        SCFILE
     C*
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*
     C                   MOVE      '0'           *IN30
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = PRIOR SCREEN
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*
     C*   COMMAND 20 = CHANGE STATUS
     C*
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   MOVE      '1'           *IN64                           ALLOW DELETE
     C                   MOVEL     CD(1)         SCCMDD                          |          E
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      '0'           *IN64                           NO DELETING
     C                   MOVEL     *BLANKS       SCCMDD                          |          E
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
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
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   COMMAND 23 = DELETE CODE
     C*
     C     *INKX         IFEQ      '1'
     C     *IN64         ANDEQ     '1'
     C                   DELETE    PBD01
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*****************************************************************
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C                   MOVE      QQEFDT        XXEFDT
     C*
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         YMEFDT
     C*
     C*  THE BILLING CODE COMMENTS CANNOT BLANK
     C*
     C     SCCOM1        IFEQ      *BLANK
     C     SCCOM2        ANDEQ     *BLANK
     C*
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*  EDIT SOURCE: (VALID: B - BILLED, F - FEE SCHED, P - PMD)
     C*
     C     SCSRCE        IFNE      'B'
     C     SCSRCE        ANDNE     'F'
     C     SCSRCE        ANDNE     'P'
     C*
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*  EDIT SOURCE = B AND PLUS %
     C*
     C     SCSRCE        IFEQ      'B'
     C     SCPC01        ANDGT     *ZEROS
     C*
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*  EDIT LESSER OF BILLED FLAG (VALID: BLANK, Y OR N)
     C*
     C     SCLESS        IFNE      *BLANK
     C     SCLESS        ANDNE     'Y'
     C     SCLESS        ANDNE     'N'
     C*
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*
     C*  ALL EDITS HAVE BEEN PASSED  *** WRITE/UPDATE FILE!
     C*
     C                   MOVEL     SCCODE        BDCODE
     C                   MOVE      YMEFDT        BDEFDT
     C                   MOVE      SCSTAT        BDSTAT
     C                   MOVE      SCCOM1        BDCOM1
     C                   MOVE      SCCOM2        DSCOM2
     C                   MOVE      SCSRCE        DSSRCE
     C                   MOVE      SCLESS        DSLESS
     C                   MOVE      *BLANKS       DSEXTR
     C                   MOVE      DSCOM2        BDCOM2
     C                   MOVEL     SCPC01        BDPC01
     C                   MOVEL     SCPC02        BDPC02
     C                   MOVE      SCTYPE        BDTYPE
     C                   MOVE      'D'           BDRCDT
     C*
     C                   MOVE      Q1USER        BDUSER
     C                   MOVE      Q1PGM         BDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BDDT
     C                   Z-ADD     DSTIME        BDTIME
     C*
     C     *IN41         IFEQ      '0'
     C                   UPDATE    PBD01
     C                   ELSE
     C                   MOVE      Q1USER        BDIUSR
     C                   WRITE     PBD01
     C                   END
     C*
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE CD
F23=DELETE CODE
**  TABLE TD
NETWORK DISCOUNT: PMD/FS/CHRGS, +/- %, LESSER OF CHRGS VS. PMD/FS
