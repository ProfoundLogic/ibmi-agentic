     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0078R - LOCATION PHYSICAL ADDRESS SEARCH
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  THIS SEARCH IS SIMILAR TO THE ENTITY/LOC ALPHA SEARCH EXCEPT *
     H*  THAT THIS SEARCH DISPLAYS THE PHYSICAL ADDRESS OF THE        *
     H*  LOCATIONS OF ONE OR ALL EMPLOYERS.  DISPLAYED INFORMATION    *
     H*  INCLUDE: LOCATION NUMBER, PHYSICAL ADDRESS, EFFECTIVE DATE,  *
     H*  DELETE DATE, AND EMPLOYER/DIV NUMBER.                        *
     H*                                                               *
     H*  CALLED BY: W0050R  - EMPLOYER MASTER MAINTENANCE             *
     H*             W0050R1 - ENDORESEMENT MAINTENANCE                *
     H*             W0077R  - ENTITY/LOCATION MAINTENANCE             *
     H*             W7550R  - ENTITY/LOCATION LABELS                  *
     H*  CALLS TO:  NONE                                              *
     H*                                                               *
     H*  PROGRAMMER: C:24                                             *
     H*  CREATED:    11/01/94          PROJECT: WC378                 *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*   SMCOL   PCO01: CO                                   Company *
     H*   WMUPL   PUP01: USER                               User Parm *
     H*   WMFNL   PFN01: CO/FND                           Fund Master *
     H*   WMEML   PEM01: CO/FND/EMP#/DIV              Employer Master *
     H*   WMALL4  PAL01: CO/FND/EMP#/DIV/PAD1/ELID/ELTP=L  E/L MASTER *
     H*   WMALL   PAL02: CO/FND/EMP#/DIV/ELID              E/L MASTER *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  20   INVALID ROLL REQUEST                                    *
     H*  21   INITIAL DISPLAY OF SEARCH SCREEN                        *
     H*  92   USER HAS SUFFICIENT AUTHORITY TO USE CMD 23-DELETE      *
     H*  98   DELETION REQUESTED - WAITING FOR CONFIRMATION           *
     H*  99   HIGHLIGHT REQUESTED RECORD                              *
     H*****************************************************************
     FW0078F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMALL4    IF   E           K DISK
     FWMALL     UF   E           K DISK
     F                                     RENAME(PAL01:PAL02)

     D FL              S              1    DIM(14)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KY1             S             40    DIM(2)
     D KY2             S             10    DIM(2)
     D MSG             S             29    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  LO                     1    140
     D                                     DIM(14)                              LOCATION ID#
     D  SCLO01                 1     10
     D  SCLO02                11     20
     D  SCLO03                21     30
     D  SCLO04                31     40
     D  SCLO05                41     50
     D  SCLO06                51     60
     D  SCLO07                61     70
     D  SCLO08                71     80
     D  SCLO09                81     90
     D  SCLO10                91    100
     D  SCLO11               101    110
     D  SCLO12               111    120
     D  SCLO13               121    130
     D  SCLO14               131    140
     D                 DS
     D  AD                     1    560
     D                                     DIM(14)                              PHYSICAL ADDRESS
     D  SCAD01                 1     40
     D  SCAD02                41     80
     D  SCAD03                81    120
     D  SCAD04               121    160
     D  SCAD05               161    200
     D  SCAD06               201    240
     D  SCAD07               241    280
     D  SCAD08               281    320
     D  SCAD09               321    360
     D  SCAD10               361    400
     D  SCAD11               401    440
     D  SCAD12               441    480
     D  SCAD13               481    520
     D  SCAD14               521    560
     D                 DS
     D  ED                     1    112  0
     D                                     DIM(14)                              EFF DATE
     D  SCED01               001    008  0
     D  SCED02               009    016  0
     D  SCED03               017    024  0
     D  SCED04               025    032  0
     D  SCED05               033    040  0
     D  SCED06               041    048  0
     D  SCED07               049    056  0
     D  SCED08               057    064  0
     D  SCED09               065    072  0
     D  SCED10               073    080  0
     D  SCED11               081    088  0
     D  SCED12               089    096  0
     D  SCED13               097    104  0
     D  SCED14               105    112  0
     D                 DS
     D  DD                     1    112  0
     D                                     DIM(14)                              DELETE DATE
     D  SCDD01               001    008  0
     D  SCDD02               009    016  0
     D  SCDD03               017    024  0
     D  SCDD04               025    032  0
     D  SCDD05               033    040  0
     D  SCDD06               041    048  0
     D  SCDD07               049    056  0
     D  SCDD08               057    064  0
     D  SCDD09               065    072  0
     D  SCDD10               073    080  0
     D  SCDD11               081    088  0
     D  SCDD12               089    096  0
     D  SCDD13               097    104  0
     D  SCDD14               105    112  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  KEY LIST FOR THE ENTITY/LOCATION FILE
     C*
     C     ALKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXPAD1
     C                   KFLD                    XXELID
     C*
     C     ALKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     ALKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXELID
     C*
     C     *LIKE         DEFINE    ALEMP#        XXEMP#
     C     *LIKE         DEFINE    ALDIV         XXDIV
     C     *LIKE         DEFINE    ALPAD1        XXPAD1
     C     *LIKE         DEFINE    ALELID        XXELID
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQELID           10
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   Z-ADD     XXEMP#        SCEMP#
     C                   Z-ADD     XXDIV         SCDIV
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  USER MUST HAVE PROPER AUTHORITY IN ORDER TO USE CMD 23-DELETE
     C*
     C     UPLVL         IFGE      95
     C                   MOVE      '1'           *IN92
     C                   ELSE
     C                   MOVE      '0'           *IN92
     C                   END
     C*
     C*  GET COMPANY, FUND, AND EMPLOYER NAMES
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     UPFND         SCFND
     C                   MOVEL     FNNAME        SCNAME                         FUND NAME
     C*
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVEL     EMNAME        SCENAM                         EMPLOYER NAME
     C*
     C*  MARK IF THE MODE IS INQUIRY ONLY (*IN09 = ON)
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*  LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   SETON                                        21        INITIAL DISPLAY
     C                   MOVE      *BLANK        XXELID
     C                   GOTO      REPOS
     C*
     C*  DISPLAY ALL FORMATS
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
     C*  PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *BLANKS       QQEMP#
     C                   MOVE      *BLANKS       QQDIV
     C                   MOVE      *BLANKS       QQELID
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *BLANKS       QQEMP#
     C                   MOVE      *BLANKS       QQDIV
     C                   MOVE      *BLANKS       QQELID
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      '0'           *IN98
     C                   MOVE      '0'           *IN99
     C                   GOTO      SCR01T
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      '0'           *IN98
     C                   MOVE      '0'           *IN99
     C                   GOTO      SCR01T
     C                   END
     C*
     C*  PROCESS HELP REQUESTS
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
     C*  COMMAND 23 = DELETE LOCATION
     C*
     C     *INKX         IFEQ      '1'
     C     Q2CPOS        DIV       256           LIN
     C     LIN           SUB       8             X                 3 0
     C     X             IFGE      1
     C     X             ANDLE     14
     C     *IN98         IFEQ      '0'
     C     LO(X)         IFNE      *ZERO
     C     LO(X)         ANDNE     *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVEA     '0000000'     *IN(71)
     C                   MOVEA     '0000000'     *IN(78)
     C     X             ADD       70            I1
     C                   MOVE      '1'           *IN(I1)
     C                   MOVE      '1'           *IN98
     C                   MOVE      '1'           *IN99
     C                   Z-ADD     X             XXSVX             3 0
     C                   MOVE      *BLANKS       SCMSG
     C                   MOVE      MSG(1)        SCMSG
     C                   END
     C                   GOTO      STRT
     C                   ELSE
     C     X             IFEQ      XXSVX
     C                   MOVE      LO(X)         XXELID
     C     ALKY2         CHAIN     PAL02                              60
     C     *IN60         IFEQ      '0'
     C                   DELETE    PAL02
     C                   END
     C                   MOVE      '0'           *IN98
     C                   MOVE      '0'           *IN99
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      KY1(1)        SCPAD1
     C                   MOVE      KY2(1)        XXELID
     C                   GOTO      REPOS
     C                   END                                                    X = SVX
     C                   END                                                    *IN98
     C                   END                                                    X = 1,14
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    *INKX
     C*
     C                   END
     C*
     C*  GOTO THE APPROPRIATE SCREEN
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
     C                   MOVE      *BLANKS       XXPAD1
     C                   MOVE      *BLANKS       XXELID
     C                   MOVEL     KY1(2)        XXPAD1
     C                   MOVEL     KY2(2)        XXELID
     C     ALKY          CHAIN     PAL01                              60
     C     ALKY1         READE     PAL01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANKS       XXPAD1
     C                   MOVE      *BLANKS       XXELID
     C                   MOVEL     KY1(1)        XXPAD1
     C                   MOVEL     KY2(1)        XXELID
     C     ALKY          CHAIN     PAL01                              60
     C                   READP     PAL01                                  60
     C  N60ALCO#         COMP      UPCO#                              6060
     C  N60ALFND         COMP      UPFND                              6060
     C  N60ALEMP#        COMP      XXEMP#                             6060
     C  N60ALDIV         COMP      XXDIV                              6060
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCPAD1        IFNE      *BLANKS
     C     REPOS         TAG
     C                   MOVE      *BLANKS       KY1
     C                   MOVE      *BLANKS       KY2
     C                   MOVEL     SCPAD1        XXPAD1
     C                   MOVE      *BLANKS       SCPAD1
     C                   MOVE      *BLANK        XXUP
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       21
     C     ALKY          SETLL     PAL01
     C     ALKY1         READE     PAL01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       8             I1                3 0
     C*
     C*  VALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     14
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      LO(I1)        QQELID
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANKS       QQELID
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
     C                   MOVE      *BLANKS       LO
     C                   MOVE      *BLANKS       AD
     C                   MOVE      *ZEROS        ED
     C                   MOVE      *ZEROS        DD
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVEL     ALELID        LO(I1)                         LOCATION ID#
     C                   MOVEL     ALPAD1        AD(I1)                         PHYSICAL ADDRESS
     C                   Z-ADD     ALEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ED(I1)
     C                   Z-ADD     ALDLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DD(I1)
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
     C     ALKY1         READE     PAL01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVEL     AD(1)         KY1(1)
     C                   MOVEL     AD(14)        KY1(2)
     C                   MOVEL     LO(1)         KY2(1)
     C                   MOVEL     LO(14)        KY2(2)
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
     C                   READP     PAL01                                  60
     C  N60ALCO#         COMP      UPCO#                              6060
     C  N60ALFND         COMP      UPFND                              6060
     C  N60ALEMP#        COMP      XXEMP#                             6060
     C  N60ALDIV         COMP      XXDIV                              6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVEL     AD(1)         KY1(1)
     C                   MOVEL     AD(14)        KY1(2)
     C                   MOVEL     LO(1)         KY2(1)
     C                   MOVEL     LO(14)        KY2(2)
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
     C                   MOVE      *BLANKS       XXPAD1
     C                   MOVE      *BLANKS       XXELID
     C     ALKY1         SETLL     PAL01
     C     ALKY1         READE     PAL01                                  60
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
     C     ALKY1         READE     PAL01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
**
ROLL UP
ROLL DOWN
**  TABLE MSG
Press CMD 23 again to delete.
