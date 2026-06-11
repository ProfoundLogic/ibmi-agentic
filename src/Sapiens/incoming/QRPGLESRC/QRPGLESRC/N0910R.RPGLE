     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*
     H*  REVISIONS
     H*
     H*  08/26/99 C:62 PROJECT: MC145
     H*                IMPLEMENT INQUIRY MODE.
     H*
     H*  07/22/98 C:94 PROJECT: SUPP
     H*                CHANGED TO DISCOUNT CODE VERIFICATION
     H*
     H*  07/20/98 C:94 PROJECT: SUPP (MC095, 12748)
     H*                CHANGED PARM ##TBID TO REFERENCE DSTY NOT DSCT
     H*
     H*****************************************************************
     H/TITLE N0910R - DISCOUNT FILE MAINTENANCE PROMPT
     FN0910F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FNMBDLD    IF   E           K DISK
      **** BEGIN ADD **** 05/12/98 ************************************
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
      **** END   ADD **** 05/12/98 ************************************
     F*** BEG ADD *** 07/22/98 ************************************
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT03)
     F*** END ADD *** 07/22/98 ************************************

     D CMD             S             70    DIM(2) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXCMD                  1    140
     D  XXTYPE                 9     10
     D  XXPRDS                19     58
     D  XXCALL                62     62
     D  XXCODE                72     74
     D  XXEFDT                78     85
     D*
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
     C*
     C*  USER PARAMETER FILE
     C*
     C     BDKEY         KLIST
     C                   KFLD                    SCCODE
     C                   KFLD                    X1EFDT
     C*** BEG ADD *** 07/22/98 *************************************
     C*
     C*** Key list for WCMC generic table file
     C*
     C     GTKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKTKEY
     C*
     C*** END ADD *** 07/22/98 *************************************
     C     *LIKE         DEFINE    UPDT          X1EFDT
     C*** BEG ADD *** 07/22/98 **************************************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKTKEY
     C*** END ADD *** 07/22/98 ****************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C*** BEGIN CHG *** 08/26/99 **************************************
     C**********           PARM           QQCALL  1
     C                   PARM                    @@CALL            1
     C*** END   CHG *** 08/26/99 **************************************
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
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVEL     Q2RECF        SCRECF
     C*
     C                   MOVEL     Q2FILE        SCFILE
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
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C**********************************
     C*   F3 = EXIT                    *
     C**********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
      **** BEGIN CHG **** 05/12/98 ************************************
      *
     C*********************************
     C*     F10 = SEARCH              *
     C*********************************
      *
     C     *INKJ         IFEQ      '1'
      *
     C*********************************
     C*   DISCOUNT CODE SEARCH        *
     C*********************************
      *
     C     HFLD          IFEQ      'SCCODE'
      *
     C                   CALL      'N1410R'
     C                   PARM                    QQPRDS           40
     C                   PARM      '2'           QQCALL            1
     C                   PARM                    ##KEY             3
     C                   PARM                    QQKEY2            8 0
     C                   PARM                    QQRTCD            1
      *
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'B'
     C     ##KEY         OREQ      *BLANK
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      ##KEY         SCCODE
     C                   MOVE      QQKEY2        SCEFDT
     C                   MOVE      *ON           *IN26
     C                   ENDIF                                                  IFEQ '*'
      *
     C                   GOTO      STRT
     C                   END                                                    IFEQ SCCODE
      *
     C*********************************
     C***********   F6 = NET/DISC CODE SEARCH   *
     C*********************************
     C********** *INKF     IFEQ '1'
     C******************** CALL 'N1410R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCCODE
     C******************** PARM           SCEFDT
     C******************** PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C********** QQRTC     IFEQ 'B'
     C********** SCCODE    OREQ *BLANK
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C******************** END
     C*
     C********** QQRTC     CABEQ'*'       SCR01T
     C******************** END
      *
     C*********************************
     C*  DISCOUNT TYPE SEARCH         *
     C*********************************
      *
     C     HFLD          IFEQ      'SCTYPE'
      *
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRNP            9 0
     C*** BEG CHG *** 07/20/98 ***************************************
     C                   PARM      'DSTY'        ##TBID           10
     C*******              PARM 'DSCT'    ##TBID 10
     C*** END CHG *** 07/20/98 ***************************************
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         CABEQ     *ON           STRT                     26
      *
     C     ##RRNP        CHAIN     PGT02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCTYPE
     C                   MOVE      *ON           *IN26
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD SCTYPE
      *
     C                   MOVE      *ON           *IN90
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  IFEQ *INKJ
     C*********************************
     C**********   F10 = TABLE FILE SEARCH     *
     C*********************************
     C********** *INKJ     IFEQ '1'
     C*
     C******************** MOVE *BLANKS   QQKEY  10
     C******************** MOVE SCTYPE    QQKEY
     C******************** MOVE *BLANK    QQRTC   1
     C******************** CALL 'N0915R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           QQKEY
     C******************** PARM           QQRTC
     C*
     C******************** MOVE QQKEY     SCTYPE
     C*
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTC     IFEQ 'B'
     C********** QQKEY     OREQ *BLANK
     C********** QQRTC     OREQ '*'
     C******************** GOTO STRT
     C******************** END
     C*
      **** END   CHG **** 05/12/98 ************************************
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
     C                   MOVEL     Q2RECF        SCRECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
      **** BEGIN DEL **** 05/12/98 ************************************
     C***********   PROCESS CHANGE OF USER PARAMETERS
     C*
     C********** *INKM     IFEQ '1'
     C*
     C******************** CALL 'F0006R'
     C******************** WRITESCR99
     C*
     C**********  GET THE USER PARAMETER FILE
     C*
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C******************** MOVE *ZERO     UPCO#
     C******************** END
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** MOVELQ2RECF    SCRECF
     C******************** GOTO STRT
     C******************** END
     C******************** END
      **** END   DEL **** 05/12/98 ************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1EFDT
     C*
     C*  THE DISCOUNT CODE CANNOT BE BLANK
     C*
     C     SCCODE        CABEQ     *BLANK        STRT                     20
     C*
     C     SCEFDT        CABEQ     *ZERO         STRT                     21
     C*
     C*** VALIDATE THE EFFECTIVE DATE
     C*
     C                   MOVE      *BLANKS       SRDTE             6
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*** A VALID DISCOUNT TYPE MUST BE ENTERED WHEN ADDING DISCOUNT CODE
     C     BDKEY         CHAIN     PBD01                              60
     C     *IN60         IFEQ      '1'
     C*
     C*** TYPE CANT BE BLANK ON ADD
     C     SCTYPE        CABEQ     *BLANK        STRT                     22
     C*
     C*   VALIDATE DISCOUNT TYPE CODE
     C*
     C*** BEG CHG *** 07/22/98 **************************************
     C                   MOVEL     'DSTY'        KKTBID
     C                   MOVEL(P)  SCTYPE        KKTKEY
     C*
     C     GTKY          CHAIN     PGT03                              99
     C*
     C     *IN99         IFEQ      '1'
     C*
     C                   SETON                                        23
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*** END CHG *** 07/22/98 **************************************
     C*******              MOVE *BLANKS   QQKEY  10
     C*                    MOVE SCTYPE    QQKEY
     C*                    MOVE 'V'       QQRTC   1
     C*                    CALL 'N0915R'
     C*                    PARM           QQPRDS
     C*                    PARM           QQCALL
     C*                    PARM           QQKEY
     C*                    PARM           QQRTC
     C*
     C*          QQRTC     IFEQ 'B'
     C*          QQKEY     OREQ *BLANK
     C*                    MOVE '1'       *IN23
     C*                    GOTO STRT
     C*                    END
     C*
     C                   END
     C*
     C*** DETERMNINE THE PROGRAM TO CALL TO PROCESS REQUEST
     C*
     C*
     C** Call QCMDEXC to GET PROGRAM BASED ON TYPE ENTERED.
     C                   Z-ADD     140           XXLEN
     C                   MOVEL     CMD(1)        XXCMD
     C                   MOVE      CMD(2)        XXCMD
     C     *IN60         IFEQ      '0'
     C                   MOVE      BDTYPE        XXTYPE            2
     C                   ELSE
     C                   MOVE      SCTYPE        XXTYPE
     C                   END
     C                   MOVE      SCCODE        XXCODE
     C*
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   MOVEL     QQPRDS        XXPRDS
     C*** BEGIN CHG *** 08/26/99 **************************************
     C**********           MOVELQQCALL    XXCALL
     C                   MOVEL     @@CALL        XXCALL
     C*** END   CHG *** 08/26/99 **************************************
     C*
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM                    XXLEN            15 5
     C*
     C********************************************************************
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVEL     Q2RECF        SCRECF
     C                   MOVE      *BLANK        SCTYPE
     C                   MOVE      *BLANK        SCCODE
     C                   MOVE      *ZERO         SCEFDT
     C                   GOTO      STRT
     C*
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  +... 1 ...+... 2 ...+... 3 ...+... 4 ...+... 5 ...+... 6 ...+... 7
CALL N91XXR PARM('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' 'x'
'xxx' 'xxxxxxxx')
