     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE M0002R - MENU PROCESSOR
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  03/15/18 C:33  Project: SY261                                *
     H*                 Do not save adjusted date.                    *
     H*                                                               *
     H*  07/05/13 C:33  Redmine: 3103                                 *
     H*                 Allow for GS access.                          *
    H*                                                               *
     H*  12/21/11 C:279 SUPPORT: 78344                                *
     H*                 Record is being locked for no reason.         *
     H*                                                               *
     H*  10/20/11 C:189 SUPPORT: 78233                                *
     H*                 Wrong indicator usage is causing the cursor   *
     H*                 to go at wrong place                          *
    H*                                                               *
     H*  08/29/11 C:189 PROJECT: SY261 PRISS                          *
     H*                 Days need to scramble before adding to date   *
     H*                                                               *
     H*  07/08/11 C:235 Project: SY261                                *
     H*                 Update WCMS User File.                        *
     H*                                                               *
     H*  01/24/11 C:33  Project: SY261                                *
     H*                 Update WCMS User File.                        *
     H*                                                               *
     H*  08/10/10 C:33  Project: SY215                                *
     H*                 Use rebranding for copyright info.            *
     H*                                                               *
     H*  03/02/09 C:33  Menu copyright update.                        *
     H*                                                               *
     H*  05/07/07 C:33  Project: 89IAI  DT089/58280                   *
     H*                 For client 89, ADD the DTM libraries when     *
     H*                  starting and remove them when ending.        *
     H*                                                               *
     H*  01/06/06 C:33  Seq#: OP135/50631                             *
     H*                 Do not allow a user menu access if they are   *
     H*                  inactive.                                    *
     H*                                                               *
     H*  05/03/91 JJJ   ALLOW F3 EXIT ON MENU                         *
     H*  05/10/91 LJQ   PER JACKIE - MOVE BLANKS TO OPTION # WHEN     *
     H*                 CHANGING MENUS                                *
     H*  06/01/91 LJD   ADDED PARMS FROM M0002C TO DO OVRDBF IN M0002R*
     H*                                                               *
     H*  11/19/92 C:20  CHANGED F13 TO CALL CORRECT PGM BY GETTING    *
     H*                 LAST BYTE OF COMMAND TO EXECUTE FIELD         *
     H*                                                               *
     H*  07/15/94 C:20  MODIFICATION TO CHAIN TO MENU OPTION FILE WHEN*
     H*                 F6 IS PRESSED.  PGM WAS SOMETIMES USING THE   *
     H*                 INCORRECT MENU OPTION FOR CHECKING SECURITY   *
     H*                 THIS ALLOWED THE USER TO GET IN MENU OPTIONS  *
     H*                 THEY DID NOT HAVE SECURITY TO.                *
     H*                                                               *
     H*  12/09/96 C:69  ADDED PARM TO BYPASS LIBL CHANGE              *
     H*****************************************************************
     FM0002F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK    USROPN
     FMMOPL     IF   E           K DISK    USROPN
     FSMUPL     UF   E           K DISK
     FSMWUP     UF A E           K DISK    USROPN
     F                                     RENAME(PWU01:PWU0P)

     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEY             S              4    DIM(2)
     D CM              S              1    DIM(256)
     D QCM             S             20    DIM(1) CTDATA PERRCD(1)              CMD FOR QCMDEXC
     D DAY             S              4    DIM(7) CTDATA PERRCD(1)
     D SCR             S              1    DIM(10) CTDATA PERRCD(1)             SCRAMBLE
     D*
     D ADDDTM          C                   CONST('ADDDTMLIB')
     D RMVDTM          C                   CONST('RMVDTMLIB')
     D CPYRIT          C                   CONST('Copyright (c) -
     D                                     StoneRiver, Inc.  -
     D                                     All Rights Reserved.')
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  OVR                    1    350
     D                                     DIM(5)                               OVRDBF CMDS
     D  QQOVR1                 1     70
     D  QQOVR2                71    140
     D  QQOVR3               141    210
     D  QQOVR4               211    280
     D  QQOVR5               281    350
     D                 DS
     D  OT                     1      6
     D                                     DIM(6)
     D  OPTY01                 1      1
     D  OPTY02                 2      2
     D  OPTY03                 3      3
     D  OPTY04                 4      4
     D  OPTY05                 5      5
     D  OPTY06                 6      6
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
     D                 DS
     D  TX                     1    240
     D                                     DIM(12)
     D  SCTX01                 1     20
     D  SCTX02                21     40
     D  SCTX03                41     60
     D  SCTX04                61     80
     D  SCTX05                81    100
     D  SCTX06               101    120
     D  SCTX07               121    140
     D  SCTX08               141    160
     D  SCTX09               161    180
     D  SCTX10               181    200
     D  SCTX11               201    220
     D  SCTX12               221    240
     D                 DS
     D  OP                     1     56
     D                                     DIM(14)
     D  SCOP01                 1      4
     D  SCOP02                 5      8
     D  SCOP03                 9     12
     D  SCOP04                13     16
     D  SCOP05                17     20
     D  SCOP06                21     24
     D  SCOP07                25     28
     D  SCOP08                29     32
     D  SCOP09                33     36
     D  SCOP10                37     40
     D  SCOP11                41     44
     D  SCOP12                45     48
     D  SCOP13                49     52
     D  SCOP14                53     56
     D                 DS
     D  DS                     1    560
     D                                     DIM(14)
     D  SCDS01                 1     40
     D  SCDS02                41     80
     D  SCDS03                81    120
     D  SCDS04               121    160
     D  SCDS05               161    200
     D  SCDS06               201    240
     D  SCDS07               241    280
     D  SCDS08               281    320
     D  SCDS09               321    360
     D  SCDS10               361    400
     D  SCDS11               401    440
     D  SCDS12               441    480
     D  SCDS13               481    520
     D  SCDS14               521    560
     D                 DS
     D  XXDATE                 1      7  0
     D  XXA                    1      1  0
     D  XXB                    2      2  0
     D  XXC                    3      3  0
     D  XXD                    4      4  0
     D  XXE                    5      5  0
     D  XXF                    6      6  0
     D  XXG                    7      7  0
     D                 DS
     D  SCRMDT                 1      7  0
     D  TMPA                   1      1  0
     D  TMPB                   2      2  0
     D  TMPC                   3      3  0
     D  TMPD                   4      4  0
     D  TMPE                   5      5  0
     D  TMPF                   6      6  0
     D  TMPG                   7      7  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D  PMSGQ            *PROC
     D  MSGID                 40     46

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  PARAMETER LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQOPN             1
     C                   PARM                    QQLIBL            1
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  COMPANY MASTER FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  OPTION FILE
     C*
     C     OPKEY         KLIST
     C                   KFLD                    XXMNAM           10
     C                   KFLD                    XXKEY             4
     C*
     C     OPKEY2        KLIST
     C                   KFLD                    XXMNAM
     C*
     C     *DTAARA       DEFINE    MCOPYRIGHT    SCCYRT
     C                   IN        SCCYRT                               99
     C*
     C                   CALL      'RTVBRND'                            99
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C*
     C                   SELECT
     C     #BRND1        WHENNE    *BLANKS
     C                   MOVEL(P)  #BRND1        SCCYRT
     C     SCCYRT        WHENEQ    *BLANKS
     C                   MOVEL(P)  CPYRIT        SCCYRT
     C                   ENDSL
     C*
     C                   MOVE      *ON           CONTLL            1
B01  C     Q1PRMS        IFGT      1
     C                   MOVE      QQLIBL        CONTLL
E01  C                   ENDIF
     C*
     C                   MOVEL     'MASTER'      XXMNAM
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
     C     *LIKE         DEFINE    UPDT          SYSDT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C*
     C                   OPEN      MMOPL
     C                   OPEN      SMCOL
     C                   MOVE      'Y'           XXOPN             1
     C*
     C     UPKEY         CHAIN     PUP01                              60
B01  C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
E01  C                   END
     C*
     C* If user parm record exists, update WCMS User File
     C     *IN60         IFEQ      *OFF
     C                   OPEN      SMWUP
     C                   EXSR      SRWCMS
     C                   ENDIF
      /Free
       //  Begin Add  ***  07/05/13  **************************************
       xxMNam = %trim(UPiPgm) + '-GS';
       Chain xxMNam pop01;
       If not %found;
          xxMNam = UPiPgm;
       Endif;
       scMnUn = xxMNam;
       //  End   Add  ***  07/05/13  **************************************
      /End-free
     C*
     C***  Begin Del  ***  07/05/13  **************************************
     C**********         MOVEL     UPIPGM        XXMNAM
     C**********         MOVE      UPIPGM        SCMNUN
     C***  End   Del  ***  07/05/13  **************************************
     C*
     C     OPKEY         CHAIN     POP01                              60
B01  C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  OPMNTT        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
E01  C                   ENDIF
     C*
     C*  EXECUTE THE PROGRAM TO CHANGE THE LIBRARY LIST
     C*
     C                   MOVE      *BLANKS       SCMETL
     C                   MOVE      *BLANKS       QQRTCD
     C                   MOVE      *BLANKS       QQOVR1
     C                   MOVE      *BLANKS       QQOVR2
     C                   MOVE      *BLANKS       QQOVR3
     C                   MOVE      *BLANKS       QQOVR4
     C                   MOVE      *BLANKS       QQOVR5
     C*
B01  C     CONTLL        IFEQ      *ON
     C                   CALL      'M0002C'
     C                   PARM                    OPAPP
     C                   PARM                    SCMETL           10
     C                   PARM                    QQOVR1
     C                   PARM                    QQOVR2
     C                   PARM                    QQOVR3
     C                   PARM                    QQOVR4
     C                   PARM                    QQOVR5
     C                   PARM                    QQRTCD            1
E01  C                   ENDIF
     C*
B01  C     QQRTCD        IFEQ      '1'
     C                   GOTO      TERM
E01  C                   END
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C                   MOVE      *OFF          DTMLIB            1
     C     CLTID         IFEQ      '089'
     C                   MOVE      *ON           DTMLIB
     C                   ENDIF
     C*
     C     DTMLIB        IFEQ      *ON
     C                   CALL      ADDDTM
     C                   ENDIF
     C*
     C* * IF OPEN, CLOSE MMOPL
B01  C     XXOPN         IFEQ      'Y'
     C                   CLOSE     MMOPL
     C                   CLOSE     SMCOL
     C                   MOVE      'N'           XXOPN
E01  C                   END
     C*
     C* * DELETE ANY EXISTING OVERRIDES
     C                   Z-ADD     20            LENGTH
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    QCM(1)
     C                   PARM                    LENGTH           15 5
     C                   CALL      'SRRMVMSG'                           99
     C*
     C* * OVERRIDE DATABASE FILE(S) BROUGHT BACK FROM M0002C
     C                   Z-ADD     70            LENGTH
     C                   Z-ADD     1             Z                 5 0
B01  C                   DO        5             Z
B02  C     OVR(Z)        IFNE      *BLANKS
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    OVR(Z)
     C                   PARM                    LENGTH           15 5
E02  C                   END                                                    END NE *BLANKS
E01  C                   END                                                    END DO 5 Z
     C*
     C* * IF CLOSED, OPEN MMOPL
B01  C     XXOPN         IFEQ      'N'
     C                   OPEN      MMOPL
     C                   OPEN      SMCOL
     C                   MOVE      'Y'           XXOPN
E01  C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C     TOP           TAG
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      UPCO#         SCCO#
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVE      *BLANK        SCNAME
B01  C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCNAME
E01  C                   ENDIF
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C                   MOVE      '1'           *IN21
     C                   GOTO      REPOS
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
B01  C     XXUP          IFEQ      '1'
     C                   MOVE      *BLANKS       SCUP
X01  C                   ELSE
     C                   MOVEL     RR(1)         SCUP
E01  C                   END
B01  C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
X01  C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
E01  C                   END
     C*
B01  C     *IN80         IFEQ      '1'
     C                   MOVE      '1'           *IN55
     C                   WRITE     SCMSGC                                                      MG
     C                   MOVE      '0'           *IN55
     C*
     C                   WRITE     SCMSGC                                                      MG
E01  C                   END
     C*
     C                   EXFMT     SCR01
     C                   MOVE      '1'           *IN15
     C                   MOVE      '1'           *IN45
     C                   MOVE      *BLANK        TX
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
B01  C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = END OF JOB
     C*
B02  C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
E02  C                   END
     C*
     C*  FUNCTION-5, PROCESS OUTPUT TYPE
     C*
B02  C     *INKE         IFEQ      '1'
B03  C     *IN70         IFEQ      '1'
     C                   MOVE      '0'           *IN70
X03  C                   ELSE
     C                   MOVE      '1'           *IN70
E03  C                   END
     C                   GOTO      REPOS
E02  C                   END
     C*
     C*  EXECUTE OPTION IF KEYED
     C*
B02  C     *INKF         IFEQ      '1'
B03  C     SCOPTN        IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCOPTN        XXKEY
     C     OPKEY         CHAIN     POP01                              60
B04  C     *IN60         IFEQ      '1'
     C                   GOTO      STRT
E04  C                   END
     C                   MOVE      SCOPTN        XXOPTN            4
     C                   EXSR      SRAUT
     C   55              GOTO      STRT
     C                   EXSR      SREXE
     C                   GOTO      STRT
E03  C                   END
E02  C                   END
     C*
     C*  COMMAND 13 CHANGE USER PARMS
     C*
B02  C     *INKM         IFEQ      '1'
     C*
     C                   MOVEL     SCMNUN        XXMNAM
     C     OPKEY2        CHAIN     POP01                              60
B03  C     *IN60         IFEQ      '0'
     C                   Z-ADD     230           T                 3 0
     C     1             SUBST     OPEXCL:T      XXFLAG            1    65
     C*
B04  C     XXFLAG        IFEQ      'F'
     C                   CALL      'F0006R'
E04  C                   END
     C*
B04  C     XXFLAG        IFEQ      'W'
     C                   CALL      'W0006R'
E04  C                   END
     C*
B04  C     XXFLAG        IFEQ      'R'
     C                   CALL      'R0006R'
E04  C                   END
     C*
B04  C     XXFLAG        IFEQ      'C'
     C                   CALL      'C0006R'
E04  C                   END
     C*
B04  C     XXFLAG        IFEQ      'E'
     C                   CALL      'E0006R'
E04  C                   END
     C*
B04  C     XXFLAG        IFEQ      'S'
     C     XXFLAG        OREQ      ' '
     C                   CALL      'S0006R'
E04  C                   END
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM      'RCLRSC'      ##RCLR            6
     C                   PARM      6             ##LENG           15 5
     C*
E03  C                   END
     C                   WRITE     SCR99
     C*
     C     UPKEY         CHAIN(N)  PUP01                              60
B03  C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
E03  C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C*
E02  C                   END
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
B02  C     *IN10         IFEQ      '1'
     C*
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       6             I1                3 0
     C*
     C*  VALID CURSOR POSITION - RETURN WITH SELECTION
     C*
B03  C     I1            IFGE      1
     C     I1            ANDLE     14
     C*
     C                   EXSR      SRHLP
     C                   ADD       30            I1
     C                   MOVE      '1'           *IN(I1)
     C                   MOVE      '0'           *IN45
     C*
     C                   GOTO      STRT
E03  C                   END
     C*
     C*  INVALID CURSOR POSITION - REDISPLAY THE MENU
     C*
     C                   GOTO      STRT
     C*
     C                   GOTO      STRT
E02  C                   END
     C*
E01  C                   END
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
B01  C   18XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
E01  C                   END
     C*
B01  C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
E01  C                   END
     C*
     C*  ROLL UP REQUEST
     C*
B01  C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVEL     KEY(2)        XXKEY             4
     C     OPKEY         CHAIN     POP01                              60
     C     OPKEY2        READE     POP01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
E01  C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
B01  C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     KEY(1)        XXKEY
     C     OPKEY         CHAIN     POP01                              60
     C                   READP     POP01                                  60
     C                   EXSR      LOADP
     C                   GOTO      STRT
E01  C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
B01  C     SCOPTN        IFNE      *BLANKS
     C     REPOS         TAG
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   MOVE      '0'           *IN21
     C                   MOVE      *BLANK        KEY
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     SCOPTN        XXKEY
     C                   MOVE      *BLANK        SCOPTN
     C     OPKEY         SETLL     POP01
     C     OPKEY2        READE     POP01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
E01  C                   END
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       6             I1                3 0
     C*
     C*  VALID CURSOR POSITION - RETURN WITH SELECTION
     C*
B01  C     I1            IFGE      1
     C     I1            ANDLE     14
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     OP(I1)        XXKEY
     C                   MOVE      *BLANKS       XXOPTN
     C                   MOVEL     OP(I1)        XXOPTN
     C     OPKEY         CHAIN     POP01                              60
B02  C     *IN60         IFEQ      '1'
     C                   GOTO      STRT
E02  C                   END
     C                   EXSR      SREXE
     C*
     C                   ADD       30            I1
     C                   MOVE      '1'           *IN(I1)
     C                   MOVE      '0'           *IN45
     C*
B02  C     *IN56         IFEQ      '1'
     C     *IN55         OREQ      '1'
     C                   GOTO      STRT
E02  C                   END
     C*
     C                   GOTO      STRT
E01  C                   END
     C*
     C*  INVALID CURSOR POSITION - REDISPLAY THE MENU
     C*
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C     DTMLIB        IFEQ      *ON
     C                   CALL      RMVDTM
     C                   ENDIF
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       OP
     C                   MOVE      *BLANKS       DS
     C                   Z-ADD     *ZERO         I1
     C* * *  ADD INDICATORS TO DISPLAY MENU OPTION AS DSPATR(HI)
     C                   MOVEA     '00000000'    *IN(81)
     C                   MOVEA     '000000'      *IN(89)
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVEL     OPOPTN        OP(I1)
B01  C     *IN70         IFEQ      '0'
     C                   MOVEL     OPMNTT        DS(I1)
X01  C                   ELSE
     C                   MOVEL     OPEXCL        DS(I1)
E01  C                   END
     C*
B01  C     OPMYN         IFEQ      'Y'
     C     I1            ADD       80            HI                5 0
     C                   MOVE      '1'           *IN(HI)
E01  C                   END
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
     C*  GOTO THE SUBROUTINE FOR CHECKING TYPES, AUTHORITY
     C*
     C                   EXSR      SRAUT
     C   55              GOTO      NOFIL1
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     NOFIL1        TAG
     C     OPKEY2        READE     POP01                                  60
B01  C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
E01  C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVEL     OP(1)         KEY(1)
     C                   MOVEL     OP(14)        KEY(2)
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
B01  C     *IN60         IFEQ      '1'
     C                   EXSR      LOADB
     C                   GOTO      DONEP
E01  C                   END
     C*
     C*  GOTO THE SUBROUTINE FOR CHECKING TYPES, AUTHORITY
     C*
     C                   EXSR      SRAUT
     C   55              GOTO      NOFIL2
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   SUB       1             I1
     C                   EXSR      FILL
     C     NOFIL2        TAG
     C                   READP     POP01                                  60
     C  N60OPMNAM        COMP      XXMNAM                             6060
B01  C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
E01  C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVEL     OP(1)         KEY(1)
     C                   MOVEL     OP(14)        KEY(2)
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
     C                   MOVE      *BLANKS       XXKEY
     C     OPKEY         SETLL     POP01
     C     OPKEY2        READE     POP01                                  60
B01  C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
E01  C                   END
     C*
     C     OVERB         TAG
     C*
     C*  GOTO THE SUBROUTINE FOR CHECKING TYPES, AUTHORITY
     C*
     C                   EXSR      SRAUT
     C   55              GOTO      NOFIL3
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     NOFIL3        TAG
     C     OPKEY2        READE     POP01                                  60
B01  C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
E01  C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C*
     C*  CHECK RECORD TYPES, AUTHORITY
     C*
     C     SRAUT         BEGSR
     C*
     C                   MOVE      '0'           *IN55
     C*
     C*  IF THE RECORD IS A MENU RECORD, DO NOT DISPLAY
     C*
     C     OPOPTN        COMP      *BLANK                                 55
     C*
     C*  If the user is inactive, do NOT display
     C*
     C  N55UPSTAT        COMP      'I'                                    55
     C*
     C*  IF THE RECORD IS NOT ACTIVE DO NOT DISPLAY
     C*
     C  N55OPSTAT        COMP      'I'                                    55
     C*
     C*  IF THE SECURITY TYPE FROM THE USER FILE IS NOT
     C*  "*" THEN CHECK THE TYPE, AND AUTHORITY
     C*
     C     *IN55         CABEQ     '1'           SRAUTE
     C*
B01  C     UPTYPE        IFNE      '*'
     C     UPTYPE        ANDNE     '#'
     C     UPTYPE        ANDNE     '$'
     C     UPTYPE        ANDNE     '&'
     C     UPTYPE        ANDNE     '%'
     C*
     C*  LOOK FOR THE SECURITY TYPE, FROM THE OPTION
     C*
B02  C     UPTYPE        IFNE      *BLANK
     C     UPTYPE        LOOKUP    OT                                     60
     C  N60              MOVE      '1'           *IN55
B03  C   60              DO
     C*
     C*  THE VALUE FROM THE USER FILE MUST BE GREATER OR EQUAL
     C*
B04  C     UPLVL         IFLT      OPMINS
     C                   MOVE      '1'           *IN55
E04  C                   END
B04  C     UPLVL         IFLT      OPMMSC
     C                   MOVE      '1'           *IN55
E04  C                   END
     C*
E03  C                   END
X02  C                   ELSE
     C                   MOVE      '1'           *IN55
E02  C                   END
X01  C                   ELSE
     C*
     C*  CHECK THE SPECIAL TYPE CODES
     C*
     C                   MOVE      *BLANK        XXSPEC            1
B02  C     UPTYPE        IFNE      '*'
     C*
B03  C     1             DO        6             I2                5 0
B04  C     OT(I2)        IFNE      *BLANK
     C*
B05  C     UPTYPE        IFEQ      '#'
B06  C     OT(I2)        IFGE      'A'
     C     OT(I2)        ANDLE     'M'
     C     OT(I2)        OREQ      '#'
     C                   MOVE      '#'           XXSPEC
E06  C                   END
E05  C                   END
     C*
B05  C     UPTYPE        IFEQ      '$'
B06  C     OT(I2)        IFGE      'N'
     C     OT(I2)        ANDLE     'Z'
     C     OT(I2)        OREQ      '$'
     C                   MOVE      '$'           XXSPEC
E06  C                   END
E05  C                   END
     C*
B05  C     UPTYPE        IFEQ      '&'
B06  C     OT(I2)        IFGE      'A'
     C     OT(I2)        ANDLE     'Z'
     C     OT(I2)        OREQ      '&'
     C     OT(I2)        OREQ      '#'
     C     OT(I2)        OREQ      '$'
     C                   MOVE      '&'           XXSPEC
E06  C                   END
E05  C                   END
     C*
B05  C     UPTYPE        IFEQ      '%'
B06  C     OT(I2)        IFGE      '0'
     C     OT(I2)        ANDLE     '9'
     C                   MOVE      '%'           XXSPEC
E06  C                   END
E05  C                   END
     C*
E04  C                   END
E03  C                   END
     C*
B03  C     XXSPEC        IFEQ      ' '
     C                   MOVE      '1'           *IN55
E03  C                   END
     C*
B03  C     UPLVL         IFLT      OPMINS
     C                   MOVE      '1'           *IN55
E03  C                   END
B03  C     UPLVL         IFLT      OPMMSC
     C                   MOVE      '1'           *IN55
E03  C                   END
     C*
E02  C                   END
     C*
E01  C                   END
     C*
     C     SRAUTE        ENDSR
     C*
     C*  SUBROUTINE TO EXECUTE THE COMMAND
     C*
     C     SREXE         BEGSR
     C*
     C                   MOVEA     '00'          *IN(55)
     C*
B01  C     XXOPTN        IFEQ      *BLANK
B02  C     OP(I1)        IFEQ      *BLANK
     C                   MOVE      '1'           *IN55
     C                   GOTO      SREXEE
E02  C                   END
E01  C                   END
B01  C     XXOPTN        IFNE      *BLANK
     C                   MOVE      XXOPTN        XXKEY
X01  C                   ELSE
     C                   MOVE      OP(I1)        XXKEY
E01  C                   END
     C     OPKEY         CHAIN     POP01                              60
B01  C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN55
     C                   GOTO      SREXEE
E01  C                   END
     C                   MOVE      *BLANK        XXOPTN
     C*
     C*  CHECK FOR ANOTHER MENU OPTION, IF SUCH THEN WE NEED TO
     C*  RE-LOAD
     C*
B01  C     OPMYN         IFEQ      'Y'
     C*
     C*  WE NEED TO CHECK SECURITY
     C*
     C*  IF THE SECURITY TYPE FROM THE USER FILE IS NOT
     C*  "*" THEN CHECK THE TYPE, AND AUTHORITY
     C*
B02  C     UPTYPE        IFNE      '*'
     C     UPTYPE        ANDNE     '#'
     C     UPTYPE        ANDNE     '$'
     C     UPTYPE        ANDNE     '&'
     C     UPTYPE        ANDNE     '%'
     C*
     C*
     C*  LOOK FOR THE SECURITY TYPE, FROM THE OPTION
     C*
B03  C     UPTYPE        IFNE      *BLANK
     C     UPTYPE        LOOKUP    OT                                     60
     C  N60              MOVE      '1'           *IN56
B04  C   60              DO
     C*
     C*  THE VALUE FROM THE USER FILE MUST BE GREATER OR EQUAL
     C*
B05  C     UPLVL         IFLT      OPMMSC
     C                   MOVE      '1'           *IN56
E05  C                   END
     C*
E04  C                   END
X03  C                   ELSE
     C                   MOVE      '1'           *IN56
E03  C                   END
X02  C                   ELSE
     C*
     C*  CHECK THE SPECIAL TYPE CODES
     C*
     C                   MOVE      *BLANK        XXSPEC            1
B03  C     UPTYPE        IFNE      '*'
     C*
B04  C     1             DO        6             I2                5 0
B05  C     OT(I2)        IFNE      *BLANK
     C*
B06  C     UPTYPE        IFEQ      '#'
B07  C     OT(I2)        IFGE      'A'
     C     OT(I2)        ANDLE     'M'
     C     OT(I2)        OREQ      '#'
     C                   MOVE      '#'           XXSPEC
E07  C                   END
E06  C                   END
     C*
B06  C     UPTYPE        IFEQ      '$'
B07  C     OT(I2)        IFGE      'N'
     C     OT(I2)        ANDLE     'Z'
     C     OT(I2)        OREQ      '$'
     C                   MOVE      '$'           XXSPEC
E07  C                   END
E06  C                   END
     C*
B06  C     UPTYPE        IFEQ      '&'
B07  C     OT(I2)        IFGE      'A'
     C     OT(I2)        ANDLE     'Z'
     C     OT(I2)        OREQ      '&'
     C     OT(I2)        OREQ      '#'
     C     OT(I2)        OREQ      '$'
     C                   MOVE      '&'           XXSPEC
E07  C                   END
E06  C                   END
     C*
B06  C     UPTYPE        IFEQ      '%'
B07  C     OT(I2)        IFGE      '0'
     C     OT(I2)        ANDLE     '9'
     C                   MOVE      '%'           XXSPEC
E07  C                   END
E06  C                   END
     C*
E05  C                   END
E04  C                   END
     C*
B04  C     XXSPEC        IFEQ      ' '
     C                   MOVE      '1'           *IN56
E04  C                   END
     C*
     C*  THE VALUE FROM THE USER FILE MUST BE GREATER OR EQUAL
     C*
B04  C     UPLVL         IFLT      OPMMSC
     C                   MOVE      '1'           *IN56
E04  C                   END
     C*
E03  C                   END
E02  C                   END
     C*
     C     *IN56         CABEQ     '1'           SREXEE
     C*
     C                   MOVEL     OPEXCL        XXMNAM
     C                   MOVEL(P)  OPMNTT        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C                   MOVEL     OPEXCL        SCMNUN
     C                   MOVE      *BLANKS       SCOPTN                         LJQ 05-10-91
     C*
     C*  EXECUTE THE PROGRAM TO GOTO A NEW MENU AND CHANGE THE
     C*  LIBRARY LIST
     C*
     C                   MOVE      *BLANKS       SCMETL
     C                   MOVE      *BLANKS       QQRTCD
     C                   MOVE      *BLANKS       QQOVR1
     C                   MOVE      *BLANKS       QQOVR2
     C                   MOVE      *BLANKS       QQOVR3
     C                   MOVE      *BLANKS       QQOVR4
     C                   MOVE      *BLANKS       QQOVR5
     C*
B02  C     CONTLL        IFEQ      *ON
     C                   CALL      'M0002C'
     C                   PARM                    OPAPP
     C                   PARM                    SCMETL           10
     C                   PARM                    QQOVR1
     C                   PARM                    QQOVR2
     C                   PARM                    QQOVR3
     C                   PARM                    QQOVR4
     C                   PARM                    QQOVR5
     C                   PARM                    QQRTCD            1
E02  C                   ENDIF
     C*
B02  C     QQRTCD        IFEQ      '1'
     C                   GOTO      TERM
E02  C                   END
     C*
     C     DTMLIB        IFEQ      *ON
     C                   CALL      ADDDTM
     C                   ENDIF
     C*
     C* * IF OPEN, CLOSE MMOPL
B02  C     XXOPN         IFEQ      'Y'
     C                   CLOSE     MMOPL
     C                   CLOSE     SMCOL
     C                   MOVE      'N'           XXOPN
E02  C                   END
     C*
     C* * DELETE ANY EXISTING OVERRIDES
     C                   Z-ADD     20            LENGTH
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    QCM(1)
     C                   PARM                    LENGTH           15 5
     C                   CALL      'SRRMVMSG'                           99
     C*
     C* * OVERRIDE DATABASE FILE(S) BROUGHT BACK FROM M0002C
     C                   Z-ADD     70            LENGTH
     C                   Z-ADD     1             Z
B02  C                   DO        5             Z
B03  C     OVR(Z)        IFNE      *BLANKS
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    OVR(Z)
     C                   PARM                    LENGTH           15 5
E03  C                   END                                                    END NE *BLANKS
E02  C                   END                                                    END DO 5 Z
     C*
     C* * IF CLOSED, OPEN MMOPL
B02  C     XXOPN         IFEQ      'N'
     C                   OPEN      MMOPL
     C                   OPEN      SMCOL
     C                   MOVE      'Y'           XXOPN
E02  C                   END
     C*
     C                   GOTO      TOP
E01  C                   END
     C*
     C*  TEST THE AUTHORITY OF THE OPTION
     C*
     C*          UPTYPE    IFNE '*'
     C*
     C*  LOOK FOR THE SECURITY TYPE, FROM THE OPTION
     C*
     C*          UPTYPE    IFNE *BLANK
     C*          UPTYPE    LOKUPOT                       60
     C* N60                MOVE '1'       *IN56
     C*  60                DO
     C*
     C*  THE VALUE FROM THE USER FILE MUST BE GREATER OR EQUAL
     C*
     C*          UPLVL     IFLT OPMINS
     C*                    MOVE '1'       *IN56
     C*                    END
     C*
     C*                    END
     C*                    ELSE
     C*                    MOVE '1'       *IN56
     C*                    END
     C*                    END
     C*          *IN56     CABEQ'1'       SREXEE
     C*
     C*  WE HAVE PASSED SECURITY, EXECUTE THE OPTION
     C*
B01  C     OPYNO         IFEQ      'Y'
     C                   MOVEA     OPEXCL        CM
E01  C                   END
B01  C     OPYNO         IFEQ      'N'
     C                   MOVEA     'CALL '       CM
     C                   Z-ADD     6             I2                5 0
     C                   MOVEA     OPEXCL        CM(I2)
E01  C                   END
     C                   MOVEA     CM            CMD             256
     C                   CALL      'M0003C'                             98
     C                   PARM                    CMD
     C                   PARM      256           LENGTH           15 5
     C*
     C                   WRITE     SCR99
     C                   MOVE      '1'           *IN80
     C*
     C     SREXEE        ENDSR
     C*
     C*   HELP SUBROUTINE
     C*
     C     SRHLP         BEGSR
     C                   MOVE      OP(I1)        XXKEY
     C     OPKEY         CHAIN     POP01                              60
B01  C     *IN60         IFEQ      '0'
     C                   MOVE      OPTX01        SCTX01
     C                   MOVE      OPTX02        SCTX02
     C                   MOVE      OPTX03        SCTX03
     C                   MOVE      OPTX04        SCTX04
     C                   MOVE      OPTX05        SCTX05
     C                   MOVE      OPTX06        SCTX06
     C                   MOVE      OPTX07        SCTX07
     C                   MOVE      OPTX08        SCTX08
     C                   MOVE      OPTX09        SCTX09
     C                   MOVE      OPTX10        SCTX10
     C                   MOVE      OPTX11        SCTX11
     C                   MOVE      OPTX12        SCTX12
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN15
     C*
E01  C                   END
     C                   ENDSR
     C*
     C*====================================================================
     C* Update WCMS User File
     C*====================================================================
     C     SRWCMS        BEGSR
     C*
     C     *LIKE         DEFINE    *IN99         SVIN99
     C                   MOVE      *IN99         SVIN99
     C*
     C                   TIME                    DSDTTM
     C*
     C     UPKEY         CHAIN     PWU0P                              99
     C*
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDTE
     C                   MOVE      *BLANK        SRSDAY
     C                   MOVE      *BLANK        SRLDAY
     C                   MOVE      *BLANK        SRERR
     C                   CALL      'SRDAY8'
     C                   PARM                    SRDTE             8 0
     C                   PARM                    SRSDAY            4            Abbreviation
     C                   PARM                    SRLDAY            9            Full Day Name
     C                   PARM                    SRERR                          Error Flag
     C*
     C                   MOVEL     SRSDAY        XXDAY             4
     C*
     C                   Z-ADD     1             X                 1 0
     C     XXDAY         LOOKUP    DAY(X)                                 93
     C                   MOVE      X             ##DAY             1
     C     *IN93         IFEQ      *ON
     C                   MOVEL     SRDTE         ##DT8             8 0
     C                   CALL      'SRPAST8'
     C                   PARM                    ##DT8             8 0
     C                   PARM      X             ##DAYS            5 0
     C                   PARM      *BLANK        ##ERR             1
     C                   Z-ADD     ##DT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         JUMDT             7 0
     C                   MOVE      JUMDT         XXDATE
     C*
     C                   EXSR      SCRAM
     C                   Z-ADD     SCRMDT        JUMDT
     C                   Z-ADD     JUMDT         WUWCDT
     C                   ENDIF
     C*
     C                   CLEAR                   SCRMDT
     C                   CLEAR                   XXDATE
     C*
     C                   MOVEL     SYSDT         XXDATE
     C                   EXSR      SCRAM
     C                   MOVEL     SCRMDT        WUISDT
     C*
     C                   MOVE      SCR(X)        WUDYWK
     C*
     C                   MOVEL(P)  Q1USER        WUUSR
     C                   MOVEL     Q1USER        WUUSER
     C                   MOVEL     Q1PGM         WUPGM
     C                   Z-ADD     SYSDT         WUDT
     C                   Z-ADD     DSTIME        WUTIME
     C*
     C                   SELECT
     C     *IN99         WHENEQ    *ON
     C*
     C                   MOVEL     WUUSER        WUIUSR
     C                   MOVEL     WUPGM         WUIPGM
     C                   Z-ADD     WUDT          WUIDT
     C                   Z-ADD     WUTIME        WUITIM
     C                   WRITE     PWU0P
     C                   OTHER
     C                   UPDATE    PWU0P
     C                   ENDSL
     C*
     C                   MOVE      SVIN99        *IN99
     C*
     C                   CLOSE     SMWUP
     C***  Begin Chg  ***  03/15/18  **************************************
     C**********         Z-ADD     ##DT8         SRDTI
     C**********         CALL      'SRCVTDAT'    DTMDY1
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C***  End   Chg  ***  03/15/18  **************************************
     C                   MOVEL     SRDTO         UPDT1
     C                   UPDATE    PUP01
     C                   ENDSR
     C*====================================================================
     C****Calculation of scramble date
     C*====================================================================
     C     SCRAM         BEGSR
     C* *INTIALIZE
     C                   MOVE      00            X@A               2 0
     C                   MOVE      00            X@B               2 0
     C                   MOVE      00            X@C               2 0
     C                   MOVE      00            X@D               2 0
     C                   MOVE      00            X@E               2 0
     C                   MOVE      00            X@F               2 0
     C                   MOVE      00            X@G               2 0
     C* *
     C                   MOVE      XXA           X@A               2 0
     C                   MOVE      XXB           X@B               2 0
     C                   MOVE      XXC           X@C               2 0
     C                   MOVE      XXD           X@D               2 0
     C                   MOVE      XXE           X@E               2 0
     C                   MOVE      XXF           X@F               2 0
     C                   MOVE      XXG           X@G               2 0
     C     X@A           IFEQ      00
     C                   MOVE      10            X@A
     C                   ENDIF
     C     X@B           IFEQ      00
     C                   MOVE      10            X@B
     C                   ENDIF
     C     X@C           IFEQ      00
     C                   MOVE      10            X@C
     C                   ENDIF
     C     X@D           IFEQ      00
     C                   MOVE      10            X@D
     C                   ENDIF
     C     X@E           IFEQ      00
     C                   MOVE      10            X@E
     C                   ENDIF
     C     X@F           IFEQ      00
     C                   MOVE      10            X@F
     C                   ENDIF
     C     X@G           IFEQ      00
     C                   MOVE      10            X@G
     C                   ENDIF
     C                   MOVE      SCR(X@A)      TMPA
     C                   MOVE      SCR(X@B)      TMPB
     C                   MOVE      SCR(X@C)      TMPC
     C                   MOVE      SCR(X@D)      TMPD
     C                   MOVE      SCR(X@E)      TMPE
     C                   MOVE      SCR(X@F)      TMPF
     C                   MOVE      SCR(X@G)      TMPG
     C                   ENDSR
**
ROLL UP
ROLL DOWN
** QCM - CMD FOR QCMDEXC
DLTOVR FILE(*ALL)
** DAYS OF WEEK FOR ARRAY                                              ***
SUN.
MON.
TUE.
WED.
THU.
FRI.
SAT.
**     SCR
4
7
0
3
6
9
2
5
8
1
