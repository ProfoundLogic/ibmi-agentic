     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0311R - REINSURANCE POLICY FILE SEARCH
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  08/16/16 C:298 PhaseWare : 42707                             *
     H*                Fixed issue of incorrect cursor position       *
     H*                returned to program W0310R. Increased length   *
     H*                of fields scFl0* on screen.                    *
     H*
     H*  01/28/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  04/03/92 LPK  PROJECT: MD027                                 *
     H*                CHANGED CODE TO USE EMP/DIV INSTEAD OF FYR.    *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     F*****************************************************************
     FW0311F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMRIL1    IF   E           K DISK

     D*
     D*** BEGIN CHG *** 01/28/98 ***********************************************
     D MD              S             10    DIM(4) CTDATA PERRCD(1)              SCMODE TABLE
     D*** END   CHG *** 01/28/98 ***********************************************
     D*
     D FL              S              1    DIM(14)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEYM            S              9  0 DIM(2)
     D KEYD            S              5  0 DIM(2)
     D KEYE            S              7  0 DIM(2)
     D KEYF            S              1    DIM(2)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  EM                     1    126  0
     D                                     DIM(14)
     D  SCEM01                 1      9  0
     D  SCEM02                10     18  0
     D  SCEM03                19     27  0
     D  SCEM04                28     36  0
     D  SCEM05                37     45  0
     D  SCEM06                46     54  0
     D  SCEM07                55     63  0
     D  SCEM08                64     72  0
     D  SCEM09                73     81  0
     D  SCEM10                82     90  0
     D  SCEM11                91     99  0
     D  SCEM12               100    108  0
     D  SCEM13               109    117  0
     D  SCEM14               118    126  0
     D                 DS
     D  DV                     1     70  0
     D                                     DIM(14)
     D  SCDV01                 1      5  0
     D  SCDV02                 6     10  0
     D  SCDV03                11     15  0
     D  SCDV04                16     20  0
     D  SCDV05                21     25  0
     D  SCDV06                26     30  0
     D  SCDV07                31     35  0
     D  SCDV08                36     40  0
     D  SCDV09                41     45  0
     D  SCDV10                46     50  0
     D  SCDV11                51     55  0
     D  SCDV12                56     60  0
     D  SCDV13                61     65  0
     D  SCDV14                66     70  0
     D                 DS
     D  EF                     1    112  0
     D                                     DIM(14)
     D  SCEF01               001    008  0
     D  SCEF02               009    016  0
     D  SCEF03               017    024  0
     D  SCEF04               025    032  0
     D  SCEF05               033    040  0
     D  SCEF06               041    048  0
     D  SCEF07               049    056  0
     D  SCEF08               057    064  0
     D  SCEF09               065    072  0
     D  SCEF10               073    080  0
     D  SCEF11               081    088  0
     D  SCEF12               089    096  0
     D  SCEF13               097    104  0
     D  SCEF14               105    112  0
     D                 DS
     D  AS                     1     14
     D                                     DIM(14)
     D  SCAS01                 1      1
     D  SCAS02                 2      2
     D  SCAS03                 3      3
     D  SCAS04                 4      4
     D  SCAS05                 5      5
     D  SCAS06                 6      6
     D  SCAS07                 7      7
     D  SCAS08                 8      8
     D  SCAS09                 9      9
     D  SCAS10                10     10
     D  SCAS11                11     11
     D  SCAS12                12     12
     D  SCAS13                13     13
     D  SCAS14                14     14
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*                                                               *
     D*** BEGIN ADD *** 01/28/98 ***********************************************
     D OPTNM           C                   CONST('Reinsurance Policy M-
     D                                     aintenance')

     I*** END   ADD *** 01/28/98 ***********************************************
     I*

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#             3 0
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#             3 0
     C                   KFLD                    UPFND             3 0
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE REINSURANCE COMPANY FILE
     C*
     C     RIKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND             3 0
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXASFL            1
     C                   KFLD                    XXEFDT
     C*
     C     RIKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND             3 0
     C*
     C     *LIKE         DEFINE    RIDT          XXEFDT
     C     *LIKE         DEFINE    RIEMP#        XXEMP#
     C     *LIKE         DEFINE    RIDIV         XXDIV
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQEMP#            9 0
     C                   PARM                    QQDIV             5 0
     C                   PARM                    QQEFDT            8 0
     C                   PARM                    QQASFL            1
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C*
     C                   SELECT
     C     QQCALL        WHENEQ    '1'
     C                   MOVE      MD(1)         SCMODE                         DISPLAY MODE
     C*
     C     QQCALL        WHENEQ    '2'
     C                   MOVE      MD(2)         SCMODE                         ADD MODE
     C*
     C     QQCALL        WHENEQ    '3'
     C                   MOVE      MD(3)         SCMODE                         CHANGE MODE
     C*
     C     QQCALL        WHENEQ    '4'
     C                   MOVE      MD(4)         SCMODE                         DELETE MODE
     C                   ENDSL                                                  QQCALL
     C*
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C*
     C**********           MOVE QQPRDS    SCPRDS
     C*
     C*  CENTER PROGRAM NAME
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        SCPRDS
     C                   MOVEL(P)  ##DATA        QQPRDS
     C*
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C                   Z-ADD     QQEMP#        XXEMP#
     C                   Z-ADD     QQDIV         XXDIV
     C*
     C                   Z-ADD     QQEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   MOVE      QQASFL        XXASFL
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
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     UPFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C     QQEMP#        COMP      *ZERO                                  21
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
     C*   F3 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   Z-ADD     *ZERO         QQEMP#
     C                   Z-ADD     *ZERO         QQDIV
     C                   Z-ADD     *ZEROS        QQEFDT
     C                   MOVE      *BLANKS       QQASFL
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   F12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   Z-ADD     *ZERO         QQEMP#
     C                   Z-ADD     *ZERO         QQDIV
     C                   Z-ADD     0             QQEFDT
     C                   MOVE      *BLANKS       QQASFL
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
     C                   Z-ADD     KEYM(2)       XXEMP#
     C                   Z-ADD     KEYD(2)       XXDIV
     C                   Z-ADD     KEYE(2)       XXEFDT
     C                   MOVE      KEYF(2)       XXASFL
     C     RIKEY         CHAIN     PRI01                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYM(1)       XXEMP#
     C                   Z-ADD     KEYD(1)       XXDIV
     C                   Z-ADD     KEYE(1)       XXEFDT
     C                   MOVE      KEYF(1)       XXASFL
     C     RIKEY         CHAIN     PRI01                              60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCEMP#        IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEYM
     C                   MOVE      *BLANK        KEYD
     C                   MOVE      *BLANK        KEYE
     C                   MOVE      *BLANK        KEYF
     C                   MOVE      *BLANK        XXUP
     C   21              MOVE      *BLANK        XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       21
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCDIV         XXDIV
     C                   Z-ADD     0             SCEMP#
     C                   Z-ADD     0             SCDIV
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   Z-ADD     0             SCEFDT
     C                   MOVE      *BLANKS       XXASFL
     C     RIKEY         SETLL     PRI01
     C     RIKEY1        READE     PRI01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       7             I1                3 0
     C*
     C*  VALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     14
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     EM(I1)        QQEMP#
     C                   Z-ADD     DV(I1)        QQDIV
     C                   Z-ADD     EF(I1)        QQEFDT
     C                   MOVE      AS(I1)        QQASFL
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     0             QQEMP#
     C                   Z-ADD     0             QQDIV
     C                   Z-ADD     *ZERO         QQEFDT
     C                   MOVE      *BLANKS       QQASFL
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
     C                   MOVE      *BLANKS       EM
     C                   MOVE      *BLANKS       DV
     C                   MOVE      *BLANKS       EF
     C                   MOVE      *BLANKS       AS
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   Z-ADD     RIEMP#        EM(I1)
     C                   Z-ADD     RIDIV         DV(I1)
     C*
     C                   Z-ADD     RIEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EF(I1)
     C*
     C                   MOVE      RIASFL        AS(I1)
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
     C     RIKEY1        READE     PRI01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     EM(1)         KEYM(1)
     C                   Z-ADD     EM(14)        KEYM(2)
     C                   Z-ADD     DV(1)         KEYD(1)
     C                   Z-ADD     DV(14)        KEYD(2)
     C*
     C                   Z-ADD     EF(1)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KEYE(1)
     C*
     C                   Z-ADD     EF(14)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KEYE(2)
     C*
     C                   MOVE      AS(1)         KEYF(1)
     C                   MOVE      AS(14)        KEYF(2)
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
     C                   READP     PRI01                                  60
     C  N60RICO#         COMP      UPCO#                              6060
     C  N60RIFND         COMP      UPFND                              6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     EM(1)         KEYM(1)
     C                   Z-ADD     EM(14)        KEYM(2)
     C                   Z-ADD     DV(1)         KEYD(1)
     C                   Z-ADD     DV(14)        KEYD(2)
     C*
     C                   Z-ADD     EF(1)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KEYE(1)
     C*
     C                   Z-ADD     EF(14)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KEYE(2)
     C*
     C                   MOVE      AS(1)         KEYF(1)
     C                   MOVE      AS(14)        KEYF(2)
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
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C                   Z-ADD     *ZERO         XXEFDT
     C                   MOVE      *BLANKS       XXASFL
     C     RIKEY         SETLL     PRI01
     C     RIKEY1        READE     PRI01                                  60
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
     C     RIKEY1        READE     PRI01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C*** BEGIN ADD *** 01/28/98 **************************************
     C*   ADDED TABLE MD FOR SCMODE
     C*** END   ADD *** 01/28/98 **************************************
     C*
**  TABLE MD
- Display-
-- Add --
- Change -
- Delete -
**
ROLL UP
ROLL DOWN
