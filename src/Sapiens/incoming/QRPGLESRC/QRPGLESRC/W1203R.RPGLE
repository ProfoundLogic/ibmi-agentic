     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W1203R - TRANSFER MAINTENANCE
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM IS USED TO VIEW ALL A/R BATCHES THAT CURRENTLY   *
     H* EXIST IN THE SYSTEM AND HAVE NOT BEEN TRANSFERRED.            *
     H* INFORMATION FOR EACH BATCH IS ACCUMULATED AND SHOWN ON THE    *
     H* SCREEN.  THIS OPTION IS ALSO USED TO FLAG PARTICULAR BATCHES  *
     H* TO BE TRANSFERRED TO A/R.  IN ORDER FOR A BATCH TO BE         *
     H* TRANSFERRED BY THE TRANSFER PROGRAM (W9206R) BOTH THE         *
     H* TRANSFER FLAG AND THE APPROVED FLAG MUST BE 'Y'.              *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W1200R (COMMAND 05)                                         *
     H* CALLS TO:                                                     *
     H*   M0006C                                                      *
     H*                                                               *
     H* PROGRAMMER: KENNETH KENNEDY                                   *
     H* CREATED: 07/12/91               PROJECT: MD021                *
     H*                                                               *
     H* INDICATORS:                                                   *
     H*   09  INQUIRY ONLY                                            *
     H*   10  HELP REQUEST                                            *
     H*   11  COMMAND KEY WAS PRESSED                                 *
     H*   18  ROLL UP REQUEST                                         *
     H*   19  ROLL DOWN REQUEST                                       *
     H*   20  VALIDATE APPROVED FLAG                                  *
     H*   21  VALIDATE TRANSFER FLAG                                  *
     H*   28  INVALID ROLL REQUEST                                    *
     H*   30 - 43                                                     *
     H*       USED TO REFERENCE DETAIL LINES ONE THROUGH FOURTEEN     *
     H*       ON THE SEARCH SCREEN FOR ERROR MESSAGES                 *
     H*   50  GENERAL CHAIN INDICATOR                                 *
     H*   60  GENERAL CHAIN INDICATOR                                 *
     H*   61  GENERAL CHAIN INDICATOR                                 *
     H*   70  INDICATOR IS SET IF LOADING RECORDS FORWARD, SO READE   *
     H*       IS USED IN FILL SUBROUTINE.  INDICATOR IS RESET IF      *
     H*       LOADING RECORDS BACKWARD, SO READP IS USED IN THE FILL  *
     H*       SUBROUTINE                                              *
     H*   80 - 93                                                     *
     H*       THESE INDICATORS ARE SET ACCORDING TO WHETHER DETAIL    *
     H*       LINES ONE THROUGH FOURTEEN ARE FILLED OR NOT.           *
     H*       CORRESPONDING INDICATORS ARE SET IF THE LINE IS EMPTY.  *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  01/27/94 C:24 PROJECT WC245                                  *
     H*                CORRECT PROBLEM OF INCORRECT AMT ENTERED AND   *
     H*                AMT POSTED ON ROLL DOWN.                       *
     H*****************************************************************
     FW1203F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWTABL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDEAL     UF   E           K DISK

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
     D  AP                     1     14
     D                                     DIM(14)
     D  SCAP01                 1      1
     D  SCAP02                 2      2
     D  SCAP03                 3      3
     D  SCAP04                 4      4
     D  SCAP05                 5      5
     D  SCAP06                 6      6
     D  SCAP07                 7      7
     D  SCAP08                 8      8
     D  SCAP09                 9      9
     D  SCAP10                10     10
     D  SCAP11                11     11
     D  SCAP12                12     12
     D  SCAP13                13     13
     D  SCAP14                14     14
     D                 DS
     D  TF                     1     14
     D                                     DIM(14)
     D  SCTF01                 1      1
     D  SCTF02                 2      2
     D  SCTF03                 3      3
     D  SCTF04                 4      4
     D  SCTF05                 5      5
     D  SCTF06                 6      6
     D  SCTF07                 7      7
     D  SCTF08                 8      8
     D  SCTF09                 9      9
     D  SCTF10                10     10
     D  SCTF11                11     11
     D  SCTF12                12     12
     D  SCTF13                13     13
     D  SCTF14                14     14
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
     C*  KEY LIST FOR THE TABLE FILE
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
     C*  KEY LIST FOR THE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY
     C*
     C     *LIKE         DEFINE    EABAT#        XXBAT#
     C     *LIKE         DEFINE    EASTAT        XXSTAT
     C*          *LIKE     DEFN SYKEY     XXKEY
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQBAT#            9
     C                   PARM                    QQBATA            9
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
     C     QQCALL        COMP      '1'                                    09
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   EXSR      CLR
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
     C                   MOVE      QQBAT#        SCBAT#
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
     C*
     C                   DO        14            I1
     C     BT(I1)        IFEQ      *ZEROS
     C     I1            ADD       79            X                 3 0
     C                   MOVE      '1'           *IN(X)
     C                   END
     C                   END
     C*
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
     C                   MOVE      *ZEROS        QQBAT#
     C                   MOVE      *ZEROS        QQBATA
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *ZEROS        QQBAT#
     C                   MOVE      *ZEROS        QQBATA
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
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   EXSR      UPDAT
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVEL     KEY(2)        XXBAT#
     C     EAKEY         CHAIN     PEA01                              60
     C     EAKEY2        READE     PEA01                                  60
     C  N60              EXCEPT    PEA01E
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   EXSR      UPDAT
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     KEY(1)        XXBAT#
     C     EAKEY         CHAIN     PEA01                              60
     C                   READP     PEA01                                  60
     C  N60EACO#         COMP      UPCO#                              6060
     C  N60EASTAT        COMP      XXSTAT                             6060
     C  N60              EXCEPT    PEA01E
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
     C                   MOVE      *BLANK        KEY
     C                   MOVE      *BLANK        XXUP
     C                   EXSR      UPDAT
     C                   MOVE      '0'           *IN21
     C                   MOVEL     SCBAT#        XXBAT#
     C                   MOVE      *BLANK        SCBAT#
     C     EAKEY         SETLL     PEA01
     C     EAKEY2        READE     PEA01                                  60
     C  N60              EXCEPT    PEA01E
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       7             I1                3 0
     C*
     C                   EXSR      UPDAT
     C*
     C*  VALID CURSOR POSITION - RETURN WITH NO SELECTION
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
     C                   MOVE      *ZEROS        QQBAT#
     C                   MOVE      *ZEROS        QQBATA
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
     C                   MOVE      *BLANKS       AP
     C                   MOVE      *BLANKS       TF
     C                   MOVE      *ZEROS        BT
     C                   MOVE      *ZEROS        BA
     C                   MOVE      *ZEROS        TE
     C                   MOVE      *ZEROS        PA
     C                   MOVE      *BLANKS       FL
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   Z-ADD     EABAT#        XXBAT#
     C                   MOVE      EATFAP        AP(I1)
     C                   MOVE      EATFFL        TF(I1)
     C                   Z-ADD     EABAT#        BT(I1)
     C                   Z-ADD     EABATA        BA(I1)
     C     *IN60         DOWEQ     '0'
     C                   MOVE      *BLANKS       XXKEY            10
     C                   MOVEL     EASYCD        XXKEY
     C     TBKEY         CHAIN     PSY01                              61
     C     *IN61         IFEQ      '0'
     C     SYSTAT        ANDNE     'I'
     C     SYTYPE        IFEQ      'Y'
     C                   ADD       EAAMT         PA(I1)
     C                   END
     C                   END
     C                   ADD       EAAMT         TE(I1)
     C   70EAKEY         READE     PEA01                                  60
     C  N70              READP     PEA01                                  60
     C  N70
     CANN60EACO#         COMP      UPCO#                              6060
     C  N70
     CANN60EASTAT        COMP      XXSTAT                             6060
     C  N70
     CANN60EABAT#        COMP      XXBAT#                             6060
     C  N60              EXCEPT    PEA01E
     C                   END
     C                   MOVE      '0'           *IN60
     C   70EAKEY         SETGT     PEA01
     C  N70EAKEY         SETLL     PEA01
     C                   ENDSR
     C*
     C*  LOAD FORWARD SUBROUTINE
     C*
     C     LOAD          BEGSR
     C                   MOVE      '1'           *IN70
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
     C     EAKEY2        READE     PEA01                                  60
     C  N60              EXCEPT    PEA01E
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVEL     BT(1)         KEY(1)
     C                   MOVEL     BT(14)        KEY(2)
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   MOVE      '0'           *IN70
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
     C                   READP     PEA01                                  60
     C  N60              EXCEPT    PEA01E
     C  N60EACO#         COMP      UPCO#                              6060
     C  N60EASTAT        COMP      XXSTAT                             6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVEL     BT(1)         KEY(1)
     C                   MOVEL     BT(14)        KEY(2)
     C                   ENDSR
     C*
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*
     C     LOADB         BEGSR
     C                   MOVE      '1'           *IN70
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *ZEROS        XXBAT#
     C     EAKEY         SETLL     PEA01
     C     EAKEY2        READE     PEA01                                  60
     C  N60              EXCEPT    PEA01E
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     EAKEY2        READE     PEA01                                  60
     C  N60              EXCEPT    PEA01E
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C*
     C     UPDAT         BEGSR
     C*
     C                   Z-ADD     *ZEROS        XXWK92
     C                   XFOOT     BT            XXWK92            9 2
     C     XXWK92        IFNE      *ZEROS
     C*
     C*  VALIDATE APPROVED FLAG
     C*
     C                   MOVEA     '0000000'     *IN(30)
     C                   MOVEA     '0000000'     *IN(37)
     C                   DO        14            I2                3 0
     C     BT(I2)        IFNE      *ZEROS
     C     AP(I2)        IFNE      'Y'
     C     AP(I2)        ANDNE     'N'
     C     I2            ADD       29            X
     C                   MOVE      '1'           *IN(X)
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END                                                    AP,I2 <> Y,N
     C*
     C*  VALIDATE TRANSFERRED FLAG
     C*
     C                   MOVEA     '0000000'     *IN(30)
     C                   MOVEA     '0000000'     *IN(37)
     C     TF(I2)        IFNE      'Y'
     C     TF(I2)        ANDNE     'N'
     C     I2            ADD       29            X
     C                   MOVE      '1'           *IN(X)
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END                                                    TF,I2 <> Y,N
     C                   END                                                    BT,I2 <> *ZEROS
     C                   END                                                    DO  14  I2
     C*
     C                   DO        14            I2
     C                   Z-ADD     BT(I2)        XXBAT#
     C     XXBAT#        IFNE      *ZEROS
     C     EAKEY         CHAIN     PEA01                              50
     C     *IN50         DOWEQ     '0'
     C                   MOVE      AP(I2)        EATFAP
     C                   MOVE      TF(I2)        EATFFL
     C                   UPDATE    PEA01
     C     EAKEY         READE     PEA01                                  50
     C                   END                                                    DOW *IN50
     C                   END                                                    XXBAT# <> 0
     C                   END                                                    DO  14  I2
     C*
     C                   END                                                    BT <> *ZEROS
     C*
     C                   ENDSR
     OPEA01     E            PEA01E
**
ROLL UP
ROLL DOWN
