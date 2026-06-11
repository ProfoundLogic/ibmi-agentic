     H*****************************************************************
     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) ACTGRP(*CALLER)
     H*                                                               *
     H*  Program: N91DSR                                              *
     H*  Description: Medicare network setup                          *
     H*                                                               *
     H*  Programmer: C:198              Project: 114MWS               *
     H*  Created: 08/12/10                                            *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FN91DSF    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FNMBDLD    UF A E           K DISK
     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D TD              S             70    DIM(1) CTDATA PERRCD(1)
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
     C                   KFLD                    YMEFDT
     C*
     C     *LIKE         DEFINE    UPDT          YMEFDT
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
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
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
     C                   MOVE      'DS'          SCTYPE
     C                   MOVEL     TD(1)         SCTDSC
     C*
     C*** GET RECORD IF IT EXISTS, ELSE ITS AN ADD
     C*
     C     BDKY1         CHAIN     PBD01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVE      BDCODE        SCCODE
     C                   MOVE      BDSTAT        SCSTAT
     C                   MOVE      BDCOM1        SCCOM1
     C                   MOVE      BDCOM2        SCCOM2
     C*
     C                   Z-ADD     BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVE      BDPC01        SCPC01
     C                   MOVE      BDPC02        SCPC02
     C                   MOVE      '0'           *IN41                          UPDATE EXISTING
     C                   MOVE      UA(2)         SCMODE                         RECORD
     C*
     C                   ELSE
     C                   MOVE      QQCODE        SCCODE
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C*
     C                   Z-ADD     YMEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVE      *ZEROS        SCPC01
     C                   MOVE      *ZEROS        SCPC02
     C*
     C                   MOVE      '1'           *IN41                          ADD RECORD BY
     C                   MOVE      UA(1)         SCMODE                         BRINGING PRIOR
     C                   MOVE      'A'           SCSTAT                         RCD'S DATA
     C*
     C                   END
     C*
     C     CLTID         IFEQ      '114'
     C                   MOVE      SCCOM1        HDCOM1           60
     C                   MOVE      SCCOM2        HDCOM2           60
     C                   MOVE      SCPC01        HDPC01            4 1
     C                   MOVE      SCPC02        HDPC02            4 1
     C                   ENDIF                                                  CLTID IFEQ 114
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
     C*  THE BILLING CODE CANNOT BLANK
     C*
     C     SCCOM1        IFEQ      *BLANK
     C     SCCOM2        ANDEQ     *BLANK
     C*
     C                   MOVE      '1'           *IN20
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
     C                   MOVE      SCCOM2        BDCOM2
     C                   MOVE      SCPC01        BDPC01
     C                   MOVE      SCPC02        BDPC02
     C                   MOVE      SCTYPE        BDTYPE
     C                   MOVE      'D'           BDRCDT
     C*
     C                   MOVE      Q1USER        BDUSER
     C                   MOVE      Q1PGM         BDPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BDDT
     C*
     C                   Z-ADD     DSTIME        BDTIME
     C*
     C     *IN41         IFEQ      '0'
     C                   UPDATE    PBD01
     C     CLTID         IFEQ      '114'
     C                   MOVE      'U'           @UWFLG            1
     C                   EXSR      WRTTR
     C                   ENDIF                                                  CLTID IFEQ 114
     C                   ELSE
     C                   MOVE      Q1USER        BDIUSR
     C                   WRITE     PBD01
     C     CLTID         IFEQ      '114'
     C                   MOVE      'W'           @UWFLG            1
     C                   EXSR      WRTTR
     C                   ENDIF                                                  CLTID IFEQ 114
     C                   END
     C*
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*================================================================
     C* WRTTR - Write Tracking Record
     C*================================================================
     C     WRTTR         BEGSR
      *
     C                   SELECT
      *
     C     @UWFLG        WHENEQ    'U'
      *
     C     HDCOM1        IFNE      BDCOM1
      *
     C                   MOVEL(P)  'BDCOM1'      @@FLD
     C                   MOVEL(P)  HDCOM1        @@FVAL
     C                   MOVEL(P)  BDCOM1        @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE            3
     C                   PARM      BDTYPE        @@TYPE            2
     C                   PARM      BDRCDT        @@RCDT            1
     C                   PARM      BDEFDT        @@EFDT            7 0
     C                   PARM      *BLANKS       @@CPT4            5
     C                   PARM      *BLANKS       @@PMOD            2
     C                   PARM      *BLANKS       @@10A1           10
     C                   PARM      *BLANKS       @@10A2           10
     C                   PARM      *BLANKS       @@10A3           10
     C                   PARM      *BLANKS       @@10A4           10
     C                   PARM                    @@FLD            10
     C                   PARM                    @@FVAL           80
     C                   PARM                    @@TVAL           80
     C                   ENDIF                                                  HDCOM1 IFNE BDCOM1
      *
     C     HDCOM2        IFNE      BDCOM2
      *
     C                   MOVEL(P)  'BDCOM2'      @@FLD
     C                   MOVEL(P)  HDCOM2        @@FVAL
     C                   MOVEL(P)  BDCOM2        @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @@EFDT
     C                   PARM      *BLANKS       @@CPT4
     C                   PARM      *BLANKS       @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDCOM2 IFNE BDCOM2
      *
     C     HDPC01        IFNE      BDPC01
      *
     C                   MOVEL(P)  'BDPC01'      @@FLD
     C                   MOVEL(P)  HDPC01        @@FVAL
     C                   MOVEL(P)  BDPC01        @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @@EFDT
     C                   PARM      *BLANKS       @@CPT4
     C                   PARM      *BLANKS       @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDAM01 IFNE BDAM01
      *
     C     @UWFLG        WHENEQ    'W'
      *
     C                   MOVEL(P)  'Added'       @@FLD
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @@EFDT
     C                   PARM      *BLANKS       @@CPT4
     C                   PARM      *BLANKS       @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
      *
      *
     C     HDPC02        IFNE      BDPC02
      *
     C                   MOVEL(P)  'BDPC02'      @@FLD
     C                   MOVEL(P)  HDPC02        @@FVAL
     C                   MOVEL(P)  BDPC02        @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @@EFDT
     C                   PARM      *BLANKS       @@CPT4
     C                   PARM      *BLANKS       @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDAM01 IFNE BDAM01
      *
     C     @UWFLG        WHENEQ    'W'
      *
     C                   MOVEL(P)  'Added'       @@FLD
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @@EFDT
     C                   PARM      *BLANKS       @@CPT4
     C                   PARM      *BLANKS       @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  WRTTR
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE TD
Medicare Hospital Fee Schedule discount Algorithm
