     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W1207R - PURGE SEARCH
     H*                                                               *
     H*****************************************************************
     H* 07/26/91 TAA  THIS PGM ALLOWS THE USER TO REVIEW PURGE DATES, *
     H* PROJ: MD021   COMPLETION DATES, AND THE REQUESTER.  PURGE DATE*
     H*               CANNOT BE DELETED IF COMPLETION DATE IS NONZERO.*
     H*****************************************************************
     FW1207F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMBPL     UF   E           K DISK

     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEY             S              8  0 DIM(2)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  PG                     1    112  0
     D                                     DIM(14)
     D  SCPG01               001    008  0
     D  SCPG02               009    016  0
     D  SCPG03               017    024  0
     D  SCPG04               025    032  0
     D  SCPG05               033    040  0
     D  SCPG06               041    048  0
     D  SCPG07               049    056  0
     D  SCPG08               057    064  0
     D  SCPG09               065    072  0
     D  SCPG10               073    080  0
     D  SCPG11               081    088  0
     D  SCPG12               089    096  0
     D  SCPG13               097    104  0
     D  SCPG14               105    112  0
     D                 DS
     D  CM                     1    112  0
     D                                     DIM(14)
     D  SCCM01               001    008  0
     D  SCCM02               009    016  0
     D  SCCM03               017    024  0
     D  SCCM04               025    032  0
     D  SCCM05               033    040  0
     D  SCCM06               041    048  0
     D  SCCM07               049    056  0
     D  SCCM08               057    064  0
     D  SCCM09               065    072  0
     D  SCCM10               073    080  0
     D  SCCM11               081    088  0
     D  SCCM12               089    096  0
     D  SCCM13               097    104  0
     D  SCCM14               105    112  0
     D                 DS
     D  NM                     1    420
     D                                     DIM(14)
     D  SCNM01                 1     30
     D  SCNM02                31     60
     D  SCNM03                61     90
     D  SCNM04                91    120
     D  SCNM05               121    150
     D  SCNM06               151    180
     D  SCNM07               181    210
     D  SCNM08               211    240
     D  SCNM09               241    270
     D  SCNM10               271    300
     D  SCNM11               301    330
     D  SCNM12               331    360
     D  SCNM13               361    390
     D  SCNM14               391    420
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    XXUSER
     C*
     C*  BATCH PURGE FILE
     C*
     C     BPKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXPGDT
     C*
     C*  BATCH PURGE FILE / PARTIAL KEY
     C*
     C     BPKEY1        KLIST
     C                   KFLD                    UPCO#
     C*
     C     *LIKE         DEFINE    SCPGDT        SVPGDT
     C     *LIKE         DEFINE    SCPGDT        X1PGDT
     C     *LIKE         DEFINE    BPDT          XXPGDT
     C     *LIKE         DEFINE    SCPGDT        XXMDY
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    SCPGDT
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
     C                   Z-ADD     SCPGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPGDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*  IF QQCAL IS = 1, THEN PGM IS IN INQUIRY MODE
     C     QQCALL        COMP      '1'                                    09
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C                   MOVE      Q1USER        XXUSER           10
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
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C     SCPGDT        COMP      *ZERO                                  21
     C                   GOTO      REPOS
     C                   MOVE      'N'           XXDEL             1
     C                   Z-ADD     0             SVPGDT
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
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
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
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  COMMAND 23 - DELETE PURGE RECORD
     C*
     C     *INKX         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C     *IN09         ANDEQ     '0'
     C*
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       7             I1                3 0
     C*
     C*  VALID CURSOR POSITION
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     14
     C*
     C                   MOVEL     PG(I1)        X1PGDT
     C                   ELSE
     C*
     C*  INVALID CURSON POSITION
     C*
     C                   MOVE      'N'           XXDEL
     C                   Z-ADD     0             SVPGDT
     C                   GOTO      STRT
     C                   END
     C*
     C     XXDEL         IFEQ      'Y'
     C     SVPGDT        ANDEQ     X1PGDT
     C*
     C                   Z-ADD     X1PGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPGDT
     C*
     C     BPKEY         CHAIN     PBP01                              60
     C*
     C* CANNOT DELETE PURGE RECORD IF COMPLETION DATE N= 0.
     C*
     C     BPCMDT        IFEQ      0
     C  N60              DELETE    PBP01
     C                   ELSE
     C                   Z-ADD     0             X
     C     I1            ADD       29            X                 2 0
     C                   MOVE      '1'           *IN(X)
     C                   SETON                                        23
     C                   END
     C*
     C                   MOVE      'N'           XXDEL
     C                   Z-ADD     0             SVPGDT
     C                   MOVE      PG(1)         SCPGDT
     C                   GOTO      REPOS
     C                   ELSE
     C                   Z-ADD     0             X
     C     I1            ADD       29            X                 2 0
     C                   MOVE      '1'           *IN(X)
     C                   MOVE      '1'           *IN22
     C                   MOVE      'Y'           XXDEL
     C                   Z-ADD     X1PGDT        SVPGDT
     C                   GOTO      STRT
     C                   END
     C                   GOTO      STRT
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
     C*
     C                   Z-ADD     KEY(2)        XXMDY
     C*
     C                   Z-ADD     XXMDY         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPGDT
     C*
     C     BPKEY         CHAIN     PBP01                              60
     C     BPKEY1        READE     PBP01                                  60
     C*
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C*
     C                   Z-ADD     KEY(1)        XXMDY
     C*
     C                   Z-ADD     XXMDY         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPGDT
     C*
     C     BPKEY         CHAIN     PBP01                              60
     C                   READP     PBP01                                  60
     C  N60BPCO#         COMP      UPCO#                              6060
     C*
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCPGDT        IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEY
     C                   MOVE      *BLANK        XXUP
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       21
     C*
     C                   Z-ADD     SCPGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPGDT
     C*
     C                   MOVE      *ZERO         SCPGDT
     C     BPKEY         SETLL     PBP01
     C     BPKEY1        READE     PBP01                                  60
     C*
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C*
     C                   Z-ADD     *ZERO         SCPGDT
     C*
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C     TERM          TAG
     C                   Z-ADD     0             SCPGDT
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C*
     C                   MOVE      *BLANKS       PG
     C                   MOVE      *BLANKS       CM
     C                   MOVE      *BLANKS       NM
     C*
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C*
     C                   Z-ADD     BPPGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PG(I1)
     C*
     C                   Z-ADD     BPCMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CM(I1)
     C*
     C                   EXSR      NAME
     C                   MOVEL     XXNAME        NM(I1)
     C*
     C                   ENDSR
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
     C*
     C     BPKEY1        READE     PBP01                                  60
     C*
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C*
     C                   Z-ADD     PG(1)         KEY(1)
     C                   Z-ADD     PG(14)        KEY(2)
     C*
     C                   ENDSR
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
     C*
     C                   READP     PBP01                                  60
     C  N60BPCO#         COMP      UPCO#                              6060
     C*
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     PG(1)         KEY(1)
     C                   Z-ADD     PG(14)        KEY(2)
     C                   ENDSR
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
     C*
     C                   MOVE      *BLANKS       XXPGDT
     C     BPKEY1        SETLL     PBP01
     C     BPKEY1        READE     PBP01                                  60
     C*
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
     C*
     C     BPKEY1        READE     PBP01                                  60
     C*
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C*
     C*  SUBROUTINE TO GET USER PARM NAME
     C     NAME          BEGSR
     C                   MOVEL     BPUSER        XXUSER
     C     UPKEY         CHAIN     PUP01                              60
     C  N60              MOVEL     UPUSNM        XXNAME           30
     C   60              MOVE      *BLANKS       XXNAME
     C     ENDNAM        ENDSR
**  TABLE UA
ROLL UP
ROLL DOWN
