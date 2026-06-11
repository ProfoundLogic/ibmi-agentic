     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE F9393R - Resubmit EOMBs through CodeReview
     H*****************************************************************
     H*                                                               *
     H* Called From: Menu                                             *
     H* Calls to   : FA735C - API: Bill Review & Repricing submit clp *
     H*                                                               *
     H*****************************************************************
     H* Created on 06/05/2002 by c:62 for Project 114MDC              *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  06/15/12 C:280 PROJECT: 114MER                               *
     H*                 CHANGE FROM CODEREVIEW TO CLAIMCHECK          *
     H*                                                               *
     H*****************************************************************
     FF9393F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFAEOP     UF   E             DISK
     FFAEOL66   IF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     F                                     INFDS(DBFDS)
     FFTBSP     IF   E             DISK
     FFTBSL     IF   E           K DISK
     F                                     RENAME(PBS01:PBS02)
     FFAESL2    IF   E           K DISK

     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D*
     D OPTNM           C                   CONST('Resubmit EOMBs throu-
     D*** BEG CHG *** 06/15/12 ******************************************
     D***           'gh CodeReview'
     D                                     gh ClaimCheck')
     D*
     D*** END CHG *** 06/15/12 ******************************************
     D*
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*****************************************************************
     C*  Parameter Lists
     C*****************************************************************
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
     C*****************************************************************
     C*  Key Lists
     C*****************************************************************
     C*
     C*  Status Code File
     C*
     C     BSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKCODE
     C*
     C*  Company Master File
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*  Case Master File
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  EOMB File
     C*
     C     EOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C*  EOMB History File
     C*
     C     ESKY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C*
     C     ESKY2         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C                   KFLD                    KKIDT
     C                   KFLD                    KKITIM
     C*
     C*  User Parameter File
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*****************************************************************
     C*  *LIKE Defines
     C*****************************************************************
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    BSTYPE        KKTYPE
     C     *LIKE         DEFINE    BSCODE        KKCODE
     C     *LIKE         DEFINE    ESIDT         KKIDT
     C     *LIKE         DEFINE    ESITIM        KKITIM
     C*
     C* GET COMPANY
     C*
     C                   EXSR      GETCOF
     C*
     C     *IN90         DOUEQ     *OFF
     C*
     C                   SETOFF                                       90
     C                   EXFMT     SCR01
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '00000'       *IN(71)
     C*
     C**************************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*****************************************************************
     C     *IN11         IFEQ      *ON
     C     *INKC         CASEQ     *ON           KCDO                           F3=EXIT
     C     *INKJ         CASEQ     *ON           KJDO                           F10=SEARCH
     C     *IN10         CASEQ     *ON           HPDO                           HELP KEY
     C                   ENDCS
     C                   ENDIF
     C*
     C**************************************************************************
     C*   PROCESS REQUEST SCREEN
     C**************************************************************************
     C*
     C     *IN90         IFEQ      *OFF
     C     *INLR         ANDEQ     *OFF
     C*
     C* Validate Pend Status Code
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSUB#        ANDEQ     *ZEROS
     C     SCFND         ANDEQ     *ZEROS
     C     SCFYR         ANDEQ     *ZEROS
     C     SCCASE        ANDEQ     *ZEROS
     C     SCEOB#        ANDEQ     *ZEROS
     C     SCCODE        ANDEQ     *BLANKS
     C     *IN90         OREQ      *OFF
     C     SCCODE        ANDNE     *BLANKS
     C*
     C                   MOVEL(P)  'P'           KKTYPE
     C                   MOVEL(P)  SCCODE        KKCODE
     C     BSKY          CHAIN     PBS02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  BSDESC        SCDESC
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       SCDESC
     C                   SETON                                        7190
     C                   ENDIF
     C                   ENDIF
     C*
     C* Status code must be blank is case/eomb info entered
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCODE        ANDNE     *BLANKS
     C     SCSUB#        IFNE      *ZEROS
     C     SCFND         ORNE      *ZEROS
     C     SCFYR         ORNE      *ZEROS
     C     SCCASE        ORNE      *ZEROS
     C     SCEOB#        ORNE      *ZEROS
     C                   SETON                                        7490
     C                   ENDIF
     C                   ENDIF
     C*
     C* Validate Claim if entered or if EOB# entered
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSUB#        IFNE      *ZEROS
     C     SCFND         ORNE      *ZEROS
     C     SCFYR         ORNE      *ZEROS
     C     SCCASE        ORNE      *ZEROS
     C     SCEOB#        ORNE      *ZEROS
     C                   Z-ADD     SCSUB#        KKSUB#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7290
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C* Validate EOB# if entered
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSUB#        IFNE      *ZEROS
     C     SCFND         ORNE      *ZEROS
     C     SCFYR         ORNE      *ZEROS
     C     SCCASE        ORNE      *ZEROS
     C     SCEOB#        ORNE      *ZEROS
     C                   Z-ADD     SCEOB#        KKEOB#
     C     EOKY          CHAIN     PEO02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7390
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDDO
     C*
     C     *INLR         IFEQ      *OFF
     C*
     C* Let's reprocess these Bills thru CodeReview
     C*
     C     SCEOB#        IFEQ      *ZEROS
     C     KKCO#         CHAIN     PEO02                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     *HIVAL        KKIDT
     C                   Z-ADD     *HIVAL        KKITIM
     C*
     C     ESKY2         SETGT     PES01
     C     ESKY1         READPE    PES01                                  99
     C     *IN99         IFEQ      *OFF
     C     ESSTCD        ANDEQ     SCCODE
     C                   EXSR      UNPEND
     C                   MOVE      DBRRN         $$RRN
     C                   CALL      'FA735C'                             99
     C                   PARM                    $$RRN             9
     C                   PARM                    $$CFMR            1
     C                   ENDIF
     C*
     C     KKCO#         READE     PEO02                                  99
     C                   ENDDO
     C*
     C                   ELSE
     C     EOKY          CHAIN     PEO02                              99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      UNPEND
     C                   MOVE      DBRRN         $$RRN
     C                   CALL      'FA735C'                             99
     C                   PARM                    $$RRN             9
     C                   PARM                    $$CFMR            1
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C                   ENDIF
     C*
     C* End the Program
     C*
     C                   SETON                                        LR
     C*****************************************************************
     C* GETCOF - GET COMAPANY INFO
     C*****************************************************************
     C     GETCOF        BEGSR
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     *ZEROS        KKCO#
     C                   ELSE
     C                   Z-ADD     UPCO#         KKCO#
     C                   ENDIF
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              99
     C                   MOVEL(P)  CONAME        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL(P)  OPTNM         SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS           40
     C*
     C                   ENDSR
     C*****************************************************************
     C* HPDO - HELP KEY
     C*****************************************************************
     C     HPDO          BEGSR
     C*
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      'CR01'        MBRNAM
     C*
     C                   CALL      'M0006C'
     C                   PARM      'QHLP'        FILNAM           10
     C                   PARM      '*LIBL'       LIBNAM           10
     C                   PARM                    MBRNAM           10
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*****************************************************************
     C* KCDO - F3=EXIT
     C*****************************************************************
     C     KCDO          BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*****************************************************************
     C* KJDO - F10=SEARCH
     C*****************************************************************
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C* Pend Status Code Search
     C*
     C     HFLD          WHENEQ    'SCCODE'
     C*
     C                   CALL      'F2435R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZERO         ##KC              1
     C                   PARM      *ZERO         ##KL              1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM      'P'           ##TYPE            1
     C*
     C     ##RRN         IFEQ      *ZEROS
     C                   SETON                                        31
     C                   ELSE
     C     ##RRN         CHAIN     PBS01                              99
     C                   MOVEL(P)  BSCODE        SCCODE
     C                   MOVEL(P)  BSDESC        SCDESC
     C                   SETON                                        32
     C                   ENDIF
     C*
     C* Case Search
     C*
     C     HFLD          WHENEQ    'SCSUB#'
     C     HFLD          OREQ      'SCFND'
     C     HFLD          OREQ      'SCFYR'
     C     HFLD          OREQ      'SCCASE'
     C*
     C                   CALL      'F0269R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##SUB#            5 0
     C                   PARM      *ZEROS        ##FND             3 0
     C                   PARM      *ZEROS        ##FYR             3 0
     C                   PARM      *ZEROS        ##CASE            7 0
     C                   PARM      *BLANKS       ##SS#             9
     C                   PARM      *ZEROS        ##ACDT            8 0
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##SUB#        OREQ      *ZEROS
     C                   SETON                                        9032      pos cursor
     C                   ENDIF
     C*
     C     ##RTCD        IFEQ      '*'
     C     ##SUB#        ANDNE     *ZEROS
     C                   Z-ADD     ##SUB#        SCSUB#
     C                   Z-ADD     ##FND         SCFND
     C                   Z-ADD     ##FYR         SCFYR
     C                   Z-ADD     ##CASE        SCCASE
     C                   SETON                                        9033
     C                   ENDIF                                                  ##RTCD IFEQ *
     C*
     C* EOB Search
     C*
     C     HFLD          WHENEQ    'SCEOB#'
     C*
     C                   CALL      'F0410R'
     C                   PARM      '2'           ##CALL
     C                   PARM      SCSUB#        ##SUB#
     C                   PARM      SCFND         ##FND
     C                   PARM      SCFYR         ##FYR
     C                   PARM      SCCASE        ##CASE
     C                   PARM      *ZEROS        ##EOB#            9 0
     C                   PARM      *BLANKS       ##CERT           15
     C                   PARM      *BLANKS       ##SFDT            8
     C                   PARM      *BLANKS       ##STDT            8
     C                   PARM                    ##FEIN            9
     C                   PARM                    ##MOD             3
     C                   PARM                    ##RCN             3 0
     C                   PARM      'E'           ##ERR             1
     C*
     C     ##EOB#        IFEQ      *ZEROS
     C                   SETON                                        33
     C                   ELSE
     C                   Z-ADD     ##SUB#        SCSUB#
     C                   Z-ADD     ##FND         SCFND
     C                   Z-ADD     ##FYR         SCFYR
     C                   Z-ADD     ##CASE        SCCASE
     C                   Z-ADD     ##EOB#        SCEOB#
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*****************************************************************
     C* UNPEND - UNPENDS BILLS
     C*****************************************************************
     C     UNPEND        BEGSR
     C*
     C     EOBLST        IFEQ      'P'
     C*
     C                   CALL      'F0395R01'
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      'H'           ##MODE            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      DBRRN         ##RRN2            9 0
     C                   PARM      'U'           ##BLST            1
     C                   PARM      *BLANKS       ##STCD            3
     C                   PARM      *BLANKS       ##REAS           60
     C                   PARM      Q1PGM         ##PGM             8
     C                   PARM      *ZEROS        ##LTR#            9 0
     C                   PARM      *BLANKS       ##EDI             1
     C*
     C     DBRRN         CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      'U'           EOBLST
     C*
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EOTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*
     C                   UPDATE    PEO01
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
