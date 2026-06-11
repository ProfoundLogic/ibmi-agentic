     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W1204R - BATCH POSTED SUMMARY SEARCH
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*       THIS PROGRAM ALLOWS THE USER TO VIEW SUMMARIES OF AMT   *
     H*       ENTERED AND AMT POSTED TO A/R OF ALL TRANSFERRED BATCH  *
     H*       NUMBERS.  THE USER COULD ALSO SELECT A BATCH NUMBER AND *
     H*       AMOUNT FROM THIS SEARCH.  INFORMATION DISPLAYED INCLUDE:*
     H*       BATCH#, BATCH AMT, TOTAL AMT ENTERED, AMT POSTED TO     *
     H*       A/R, AND DATE TRANSFERRED.                              *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*       W1200R                                                  *
     H*  CALLS TO:                                                    *
     H*       NONE                                                    *
     H*  PROGRAMMER: MARIE NGUYEN                                     *
     H*  CREATED:    07/12/91                                         *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*       18      ROLL UP REQUEST                                 *
     H*       19      ROLL DOWN REQUEST                               *
     H*       20      INVALID ROLL REQUEST                            *
     H*       21      START AT BEGIN OF FILE                          *
     H*       60      GENERAL CHAINS & READS                          *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/11/93 C:09 PROJECT WC192                                   *
     H*               SEARCH WAS SHOWING INCORRECT VALUES FOR TOTAL   *
     H*               AMOUNT ENTERED AND TOTAL AMOUNT TO BE POSTED TO *
     H*               A/R WHEN A "ROLL UP" WAS PERFORMED.  THE LAST   *
     H*               BATCH NUMBER SHOWN ON THE SCREEN BEFORE THE     *
     H*               ROLL UP WAS BEING REPEATED ON THE SCREEN AFTER  *
     H*               THE ROLL UP AS THE FIRST BATCH NUMBER BUT WITH  *
     H*               INCORRECT AMOUNTS.                              *
     H*                                                               *
     H*****************************************************************
     FW1204F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDEAL     IF   E           K DISK

     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEY             S              9  0 DIM(2)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  BT                     1    126  0
     D                                     DIM(14)
     D  SCBT01                 1      9  0
     D  SCBT02                10     18  0
     D  SCBT03                19     27  0
     D  SCBT04                28     36  0
     D  SCBT05                37     45  0
     D  SCBT06                46     54  0
     D  SCBT07                55     63  0
     D  SCBT08                64     72  0
     D  SCBT09                73     81  0
     D  SCBT10                82     90  0
     D  SCBT11                91     99  0
     D  SCBT12               100    108  0
     D  SCBT13               109    117  0
     D  SCBT14               118    126  0
     D                 DS
     D  BA                     1    126  2
     D                                     DIM(14)
     D  SCBA01                 1      9  2
     D  SCBA02                10     18  2
     D  SCBA03                19     27  2
     D  SCBA04                28     36  2
     D  SCBA05                37     45  2
     D  SCBA06                46     54  2
     D  SCBA07                55     63  2
     D  SCBA08                64     72  2
     D  SCBA09                73     81  2
     D  SCBA10                82     90  2
     D  SCBA11                91     99  2
     D  SCBA12               100    108  2
     D  SCBA13               109    117  2
     D  SCBA14               118    126  2
     D                 DS
     D  TE                     1    126  2
     D                                     DIM(14)
     D  SCTE01                 1      9  2
     D  SCTE02                10     18  2
     D  SCTE03                19     27  2
     D  SCTE04                28     36  2
     D  SCTE05                37     45  2
     D  SCTE06                46     54  2
     D  SCTE07                55     63  2
     D  SCTE08                64     72  2
     D  SCTE09                73     81  2
     D  SCTE10                82     90  2
     D  SCTE11                91     99  2
     D  SCTE12               100    108  2
     D  SCTE13               109    117  2
     D  SCTE14               118    126  2
     D                 DS
     D  PA                     1    126  2
     D                                     DIM(14)
     D  SCPA01                 1      9  2
     D  SCPA02                10     18  2
     D  SCPA03                19     27  2
     D  SCPA04                28     36  2
     D  SCPA05                37     45  2
     D  SCPA06                46     54  2
     D  SCPA07                55     63  2
     D  SCPA08                64     72  2
     D  SCPA09                73     81  2
     D  SCPA10                82     90  2
     D  SCPA11                91     99  2
     D  SCPA12               100    108  2
     D  SCPA13               109    117  2
     D  SCPA14               118    126  2
     D                 DS
     D  DT                     1    112  0
     D                                     DIM(14)
     D  SCDT01               001    008  0
     D  SCDT02               009    016  0
     D  SCDT03               017    024  0
     D  SCDT04               025    032  0
     D  SCDT05               033    040  0
     D  SCDT06               041    048  0
     D  SCDT07               049    056  0
     D  SCDT08               057    064  0
     D  SCDT09               065    072  0
     D  SCDT10               073    080  0
     D  SCDT11               081    088  0
     D  SCDT12               089    096  0
     D  SCDT13               097    104  0
     D  SCDT14               105    112  0
     D                 DS
     D  FL                     1     14
     D                                     DIM(14)
     D  SCFL01                 1      1
     D  SCFL02                 2      2
     D  SCFL03                 3      3
     D  SCFL04                 4      4
     D  SCFL05                 5      5
     D  SCFL06                 6      6
     D  SCFL07                 7      7
     D  SCFL08                 8      8
     D  SCFL09                 9      9
     D  SCFL10                10     10
     D  SCFL11                11     11
     D  SCFL12                12     12
     D  SCFL13                13     13
     D  SCFL14                14     14
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE EMPLOYER A/R BATCH FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSTAT
     C                   KFLD                    XXBAT#
     C*
     C     EAKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSTAT
     C*
     C     *LIKE         DEFINE    EABAT#        XXBAT#
     C     *LIKE         DEFINE    EABAT#        SVBAT#
     C     *LIKE         DEFINE    EASTAT        XXSTAT
     C     *LIKE         DEFINE    EAAMT         XTENTR
     C     *LIKE         DEFINE    EAAMT         XTPOST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQBAT#            9            BATCH #
     C                   PARM                    QQBATA            9            BATCH AMOUNT
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
     C*
     C                   MOVE      QQBAT#        XXBAT#
     C                   Z-ADD     XXBAT#        SCBAT#
     C                   MOVE      'T'           XXSTAT                         TRANSFERRED RECS ONL
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
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C     SCBAT#        COMP      *ZEROS                                 21
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
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *BLANKS       QQBAT#
     C                   MOVE      *BLANKS       QQBATA
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *BLANKS       QQBAT#
     C                   MOVE      *BLANKS       QQBATA
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR01T
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR01T
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
     C                   END
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
     C                   END
     C*
     C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     KEY(2)        XXBAT#
     C     EAKEY         SETGT     PEA01                              60
     C     EAKEY2        READE     PEA01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEY(1)        XXBAT#
     C     EAKEY         CHAIN     PEA01                              60
     C                   READP     PEA01                                  60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCBAT#        IFNE      *ZEROS
     C     REPOS         TAG
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   MOVE      '0'           *IN21
     C                   MOVE      *ZERO         KEY
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     SCBAT#        XXBAT#
     C                   Z-ADD     *ZEROS        SCBAT#
     C     EAKEY         SETLL     PEA01
     C     EAKEY2        READE     PEA01                                  60
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
     C                   MOVE      BT(I1)        QQBAT#
     C                   MOVE      BA(I1)        QQBATA
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANK        QQBAT#
     C                   MOVE      *BLANK        QQBATA
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *ZEROS        BT
     C                   MOVE      *ZEROS        BA
     C                   MOVE      *ZEROS        TE
     C                   MOVE      *ZEROS        PA
     C                   MOVE      *ZEROS        DT
     C                   MOVE      *BLANKS       FL
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*****************************************************************
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   Z-ADD     EABAT#        BT(I1)                         BATCH #
     C                   Z-ADD     EABATA        BA(I1)                         BATCH AMOUNT
     C*
     C                   Z-ADD     EATFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C*
     C                   ENDSR
     C*****************************************************************
     C*
     C*  LOAD FORWARD SUBROUTINE
     C*
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C     OVER          TAG
     C*
     C*  END OF FILE
     C*
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   ADD       1             I1
     C                   EXSR      FILL
     C                   Z-ADD     *ZERO         XTENTR
     C                   Z-ADD     *ZERO         XTPOST
     C                   Z-ADD     EABAT#        SVBAT#
     C*
     C*  FOR THE CURRENT BATCH#, TOTAL AMT ENTERED = ALL POSTED AMOUNTS
     C*  AND AMT POSTED TO A/R = ALL TRANSFERRED AMOUNTS
     C*
     C     *IN60         DOWEQ     '0'
     C     EABAT#        ANDEQ     SVBAT#
     C                   ADD       EAAMT         XTENTR
     C                   ADD       EATFAM        XTPOST
     C     EAKEY2        READE     PEA01                                  60
     C                   END
     C                   Z-ADD     XTENTR        TE(I1)                         TOTAL ENTERED
     C                   Z-ADD     XTPOST        PA(I1)                         TOTAL POSTED TO A/R
     C*
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     BT(1)         KEY(1)
     C                   Z-ADD     BT(14)        KEY(2)
     C                   ENDSR
     C*****************************************************************
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     15            I1
     C     OVERP         TAG
     C*
     C*  BEGINNING OF FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADB
     C                   GOTO      DONEP
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   SUB       1             I1
     C                   EXSR      FILL
     C                   Z-ADD     *ZERO         XTENTR
     C                   Z-ADD     *ZERO         XTPOST
     C                   Z-ADD     EABAT#        SVBAT#
     C*
     C*  FOR THE CURRENT BATCH#, TOTAL AMT ENTERED = ALL POSTED AMOUNTS
     C*  AND AMT POSTED TO A/R = ALL TRANSFERRED AMOUNTS
     C*
     C     *IN60         DOWEQ     '0'
     C     EABAT#        ANDEQ     SVBAT#
     C                   ADD       EAAMT         XTENTR
     C                   ADD       EATFAM        XTPOST
     C                   READP     PEA01                                  60
     C  N60EACO#         COMP      UPCO#                              6060
     C  N60EASTAT        COMP      XXSTAT                             6060
     C                   END
     C                   Z-ADD     XTENTR        TE(I1)                         TOTAL ENTERED
     C                   Z-ADD     XTPOST        PA(I1)                         TOTAL POSTED TO A/R
     C*
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     BT(1)         KEY(1)
     C                   Z-ADD     BT(14)        KEY(2)
     C                   ENDSR
     C*****************************************************************
     C*
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *ZEROS        XXBAT#
     C     EAKEY         SETLL     PEA01
     C     EAKEY2        READE     PEA01                                  60
     C                   EXSR      LOAD
     C     DONEB         ENDSR
**
ROLL UP
ROLL DOWN
