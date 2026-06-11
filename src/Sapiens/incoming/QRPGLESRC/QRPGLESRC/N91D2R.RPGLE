     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE N91D2R - TYPE D2 DISCOUNT CODE MAINTENANCE
     H*****************************************************************
     H** REVISIONS:                                                  **
     H**
     H** 09/20/00 C:62  Project: NW068
     H**                -Remove F20=Change status
     H**                -Remove F23=Delete
     H**                                                             **
     H** 7/28/89  CST - ADD 3 MORE RANGES FOR THE CALCULATIONS OF    **
     H**          NETWORK DISCOUNTS USING THE U & C METHOD.          **
     H**          PROJECT : I0014                                    **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** INDICATOR USAGE:                                            **
     H**                                                             **
     H**  *IN20 -- FIRST COMMENT CAN NOT BE BLANK                    **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     FN91D2F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FNMBDLD    UF A E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D*** BEGIN DEL *** 09/20/00 **************************************
     D**********          CD      1   1 20
     D*** END   DEL *** 09/20/00 **************************************
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
     C*  BILILNG CODE TABLE FILE
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
     C                   MOVE      'D2'          SCTYPE
     C                   MOVEL     TD(1)         SCTDSC
     C*** BEGIN DEL *** 09/20/00 **************************************
     C**********           MOVEL*BLANKS   SCCMDD
     C*** END   DEL *** 09/20/00 **************************************
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
     C                   MOVEL     BDPC01        SCPC01
     C                   Z-ADD     BDAM01        SCAM01
     C                   Z-ADD     BDAM02        SCAM02
     C                   Z-ADD     BDAM03        SCAM03
     C                   Z-ADD     BDAM04        SCAM04
     C                   Z-ADD     BDAM05        SCAM05
     C*
     C                   Z-ADD     BDAM06        SCAM06
     C                   Z-ADD     BDAM07        SCAM07
     C                   Z-ADD     BDAM08        SCAM08
     C                   Z-ADD     BDAM09        SCAM09
     C                   Z-ADD     BDAM10        SCAM10
     C                   Z-ADD     BDAM11        SCAM11
     C                   Z-ADD     BDAM25        SCAM25
     C*
     C                   MOVEL     BDPC05        SCPC05
     C                   MOVEL     BDPC06        SCPC06
     C                   MOVEL     BDPC07        SCPC07
     C                   MOVEL     BDPC02        SCPC02
     C                   MOVEL     BDPC03        SCPC03
     C                   MOVEL     BDPC04        SCPC04
     C                   MOVE      '0'           *IN41                          UPDATE EXISTING
     C                   MOVE      UA(2)         SCMODE                         RECORD
     C*** BEGIN DEL *** 09/20/00 **************************************
     C********** SCSTAT    IFEQ 'I'                        LJD 8/5/88
     C**********           MOVE '1'       *IN64            ALLOW DELETEING
     C**********           MOVELCD,1      SCCMDD           |           E
     C**********           ELSE                            |
     C**********           MOVE '0'       *IN64            NO DELETING ING
     C**********           MOVEL*BLANKS   SCCMDD           |           E
     C**********           END                             |
     C*** END   DEL *** 09/20/00 **************************************
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
     C                   MOVE      *ZEROS        SCPC03
     C                   MOVE      *ZEROS        SCPC04
     C                   MOVE      *ZEROS        SCAM01
     C                   MOVE      *ZEROS        SCAM02
     C                   MOVE      *ZEROS        SCAM03
     C                   MOVE      *ZEROS        SCAM04
     C                   MOVE      *ZEROS        SCAM05
     C*
     C*
     C                   MOVE      *ZEROS        SCAM06
     C                   MOVE      *ZEROS        SCAM07
     C                   MOVE      *ZEROS        SCAM08
     C                   MOVE      *ZEROS        SCAM09
     C                   MOVE      *ZEROS        SCAM10
     C                   MOVE      *ZEROS        SCAM11
     C                   MOVE      *ZEROS        SCAM25
     C*
     C                   MOVE      *ZEROS        SCPC05
     C                   MOVE      *ZEROS        SCPC06
     C                   MOVE      *ZEROS        SCPC07
     C*
     C                   MOVE      '1'           *IN41                          ADD RECORD BY
     C                   MOVE      UA(1)         SCMODE                         BRINGING PRIOR
     C*** BEGIN DEL *** 09/20/00 **************************************
     C**********           MOVE '0'       *IN64             NO DELETING
     C**********           MOVEL*BLANKS   SCCMDD            |          E
     C*** END   DEL *** 09/20/00 **************************************
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
     C*** BEGIN DEL *** 09/20/00 **************************************
     C*
     C*   COMMAND 20 = CHANGE STATUS
     C*
     C********** *INKU     IFEQ '1'
     C********** *IN40     ANDEQ'0'
     C********** SCSTAT    IFEQ 'A'
     C**********           MOVE 'I'       SCSTAT
     C**********           MOVE '1'       *IN64             ALLOW DELETE
     C**********           MOVELCD,1      SCCMDD            |          E
     C**********           ELSE
     C**********           MOVE 'A'       SCSTAT
     C**********           MOVE '0'       *IN64             NO DELETING
     C**********           MOVEL*BLANKS   SCCMDD            |          E
     C**********           END
     C**********           GOTO STRT
     C**********           END
     C*** END   DEL *** 09/20/00 **************************************
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
     C*** BEGIN DEL *** 09/20/00 **************************************
     C*
     C*   COMMAND 23 = DELETE CODE
     C*
     C********** *INKX     IFEQ '1'
     C********** *IN64     ANDEQ'1'
     C**********           DELETPBD01
     C**********           MOVE '1'       *INLR
     C**********           GOTO TERM
     C**********           END
     C*** END   DEL *** 09/20/00 **************************************
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
     C***************      MOVE QQEFDT    YMEFDT        DEL 12/01/97
     C*
     C*  THE FIRST COMMENT CANNOT BLANK
     C*
     C     SCCOM1        IFEQ      *BLANK
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
     C                   MOVEL     SCPC01        BDPC01
     C                   MOVEL     SCPC02        BDPC02
     C                   MOVEL     SCPC03        BDPC03
     C                   MOVEL     SCPC04        BDPC04
     C                   Z-ADD     SCAM01        BDAM01
     C                   Z-ADD     SCAM02        BDAM02
     C                   Z-ADD     SCAM03        BDAM03
     C                   Z-ADD     SCAM04        BDAM04
     C                   Z-ADD     SCAM05        BDAM05
     C*
     C                   Z-ADD     SCAM06        BDAM06
     C                   Z-ADD     SCAM07        BDAM07
     C                   Z-ADD     SCAM08        BDAM08
     C                   Z-ADD     SCAM09        BDAM09
     C                   Z-ADD     SCAM10        BDAM10
     C                   Z-ADD     SCAM11        BDAM11
     C                   Z-ADD     SCAM25        BDAM25
     C*
     C                   MOVEL     SCPC05        BDPC05
     C                   MOVEL     SCPC06        BDPC06
     C                   MOVEL     SCPC07        BDPC07
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
**  TABLE TD
NETWORK DISCOUNT: MRA-X%, U&C $ AMT FOR DATE RANGE ,GIVES X,Y,AND Z%
