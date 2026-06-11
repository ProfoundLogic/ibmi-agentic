     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE F0541R - SYSTEM CODE TABLE SEARCH
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    1. TABLE FILE SEARCH FOR SYSTEM CODES                      *
     H*    2. VERIFIY SYSTEM CODES ENTERED IN F0045R                  *
     H*                                                               *
     H*    IF F0045R PASSES A 'V' IN QQRTCD WHEN CALLING THIS PROGRAM *
     H*    THIS PROGRAM WILL ONLY VERIFY THE SYSTEM CODES IT WILL     *
     H*    NOT DISPLAY THE SEARCH ROUTINE.                            *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    SUBSCRIBER MASTER MAINTENANCE - F0045R                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE
     H*                                                               *
     H*  PROGRAMMER: KEN RUTHVEN            PROJECT: 99AAC            *
     H*  CREATED: 07/20/92                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  07/05/95 C:35  REQ/PROJ: NW008                               *
     H*                 1. ADDED M- MEDICAL MANAGEMENT BILLING SYSTEM *
     H*                    TYPE.                                      *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H* SCR01:                                                        *
     H*  18           ROLLUP                                          *
     H*  19           ROLLDOWN                                        *
     H*  60           GENERAL CHAIN & FILL SCREEN                     *
     H*  61           GOTO OVER TAG                                   *
     H*****************************************************************
     FF0541F    CF   E             WORKSTN USROPN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK    USROPN
     FSMUPL     IF   E           K DISK    USROPN
     F*

     D FL              S              1    DIM(14)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEY             S             70    DIM(2)
     D COD             S             10    DIM(999) CTDATA PERRCD(1)            SYSTEM CODE/DESC
     D DESC            S             70    DIM(999) ALT(COD)
     D CDT             S             80    DIM(999) ASCEND                      SORT ARRAY
     D XCD             S             10    DIM(999)                             SYSTEM SRCH CODE
     D XDES            S             70    DIM(999) ASCEND
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  CD                     1     14
     D                                     DIM(14)
     D  SCCD01                 1      1
     D  SCCD02                 2      2
     D  SCCD03                 3      3
     D  SCCD04                 4      4
     D  SCCD05                 5      5
     D  SCCD06                 6      6
     D  SCCD07                 7      7
     D  SCCD08                 8      8
     D  SCCD09                 9      9
     D  SCCD10                10     10
     D  SCCD11                11     11
     D  SCCD12                12     12
     D  SCCD13                13     13
     D  SCCD14                14     14
     D*
     D                 DS
     D  DD                     1    980
     D                                     DIM(14)
     D  SCDD01                 1     70
     D  SCDD02                71    140
     D  SCDD03               141    210
     D  SCDD04               211    280
     D  SCDD05               281    350
     D  SCDD06               351    420
     D  SCDD07               421    490
     D  SCDD08               491    560
     D  SCDD09               561    630
     D  SCDD10               631    700
     D  SCDD11               701    770
     D  SCDD12               771    840
     D  SCDD13               841    910
     D  SCDD14               911    980
     D*
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
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQKEY            10
     C                   PARM                    QQRTCD            1
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
     C* MOVE CODE AND DESC ARRAY TO COMBINATION ARRAY
     C*
     C                   MOVE      COD           CDT
     C                   MOVEL     DESC          CDT
     C*
     C*  SORT THE ARRAY
     C*
     C                   SORTA     CDT
     C*
     C*   MOVE SORTED ARRAY INTO SEARCH ARRAY
     C*
     C                   MOVE      CDT           XCD
     C                   MOVEL     CDT           XDES
     C*
     C*
     C     QQRTCD        IFEQ      'V'
     C     QQKEY         LOOKUP    XCD                                    60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C                   ELSE
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   ELSE
     C                   OPEN      F0541F
     C                   OPEN      SMCOL
     C                   OPEN      SMUPL
     C                   END
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
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
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C                   MOVE      *BLANK        SCTYPE
     C     SCTYPE        COMP      *BLANKS                                21
     C                   GOTO      REPOS
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     XXUP          IFEQ      '1'
     C                   MOVE      *BLANKS       SCUP
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   END
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   END
     C                   EXFMT     SCR01
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*********************************
     C*   F3 = EXIT                   *
     C*********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C**********************************
     C*   F12 = PREVIOUS               *
     C**********************************
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKL
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR01T
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR01T
     C*
     C                   END                                                    END *IN11
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  INVALID ROLL REQUEST
     C*
     C   18XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END                                                    END XXUP
     C*
     C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END                                                    END XXDOWN
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN            1
     C                   MOVEL     KEY(2)        XXDESC
     C                   Z-ADD     1             X                 5 0
     C     XXDESC        LOOKUP    XDES(X)                                60
     C     *IN60         IFEQ      '1'                                          FLIP IND.
     C                   MOVE      '0'           *IN60
     C                   ELSE                                                   *IN60 = 0
     C                   MOVE      '1'           *IN60
     C                   END                                                    END *IN60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    END *IN18
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     KEY(1)        XXDESC
     C                   Z-ADD     1             X
     C     XXDESC        LOOKUP    XDES(X)                                60
     C*
     C                   SUB       13            X
     C     X             IFLT      0
     C                   Z-ADD     1             X
     C     *BLANK        LOOKUP    XDES(X)                            60
     C                   ELSE                                                   X < 0
     C     XDES(X)       IFEQ      *BLANK
     C     *BLANK        LOOKUP    XDES(X)                            60
     C                   END                                                    END X
     C                   END                                                    END *IN19
     C*
     C     *IN60         IFEQ      '1'                                          FLIP IND.
     C                   MOVE      '0'           *IN60
     C                   ELSE                                                   *IN60 = 0
     C                   MOVE      '1'           *IN60
     C                   END                                                    END *IN60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCTYPE        IFNE      *BLANKS
     C     REPOS         TAG
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   MOVE      '0'           *IN21
     C                   MOVE      *BLANK        KEY
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      SCTYPE        XXDESC           70
     C                   MOVE      *BLANK        SCTYPE
     C                   Z-ADD     1             X
     C     XXDESC        IFEQ      *BLANKS                                      FIRST TIME
     C     *BLANK        LOOKUP    XDES(X)                            60
     C                   ELSE                                                   DESC<>BLANK
     C     XXDESC        LOOKUP    XDES(X)                            60  60
     C                   END                                                    END XXDESC
     C* FLIP INDICATOR IN ORDER TO FILL SCREEN
     C     *IN60         IFEQ      '1'
     C                   MOVE      '0'           *IN60
     C                   ELSE                                                   *IN60 = 0
     C                   MOVE      '1'           *IN60
     C                   END                                                    END *IN60
     C*
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       7             I1                3 0
     C*
     C*  VALID CURSOR POSITION - RETURN WITH SELECTION
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     14
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      CD(I1)        QQKEY
     C                   GOTO      TERM
     C                   END                                                     END > 1
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANKS       QQKEY
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       CD
     C                   MOVE      *BLANKS       DD
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVE      XCD(X)        CD(I1)
     C                   MOVE      XDES(X)       DD(I1)
     C                   ENDSR
     C*
     C*  LOAD FORWARD SUBROUTINE
     C*
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C     OVER          TAG
     C*
     C*  END OF ARRAY
     C*
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   ADD       1             I1
     C                   EXSR      FILL
     C                   ADD       1             X
     C     X             COMP      1000                                   60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVEL     DD(1)         KEY(1)
     C                   MOVEL     DD(14)        KEY(2)
     C*
     C                   Z-ADD     1             X
     C     DD(1)         LOOKUP    XDES(X)                                60
     C     X             SUB       1             E                 3 0
     C     E             IFGE      1
     C     XDES(E)       IFEQ      *BLANK
     C                   MOVE      '1'           XXDOWN
     C                   END                                                    END E
     C                   END                                                    END DO
     C                   ENDSR
**
ROLL UP
ROLL DOWN
** BILLING RATE TABLE

         PPROVIDER BILL REVIEW
         HHOSPITAL BILL AUDIT
         NNETWORK
         UUTILIZATION REVIEW
         CCLAIMS EDIT
         DDURABLE MEDICAL EQUIPMENT
         MMEDICAL MANAGEMENT




























































































