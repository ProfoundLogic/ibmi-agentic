     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0520R - CPT-4 MASTER FILE SEARCH
     H*****************************************************************
     H** REVISIONS:
     H*  06/09/98 C:31  Project: MC145
     H*                 1. Added selection of FMPRP file by state
     H*                                                               *
     H*  07/20/93 C:20   PROJ: AL006                                  *
     H*                  1. ADD IN USE OF PR92N5 FOR DIA AMOUNT       *
     H*                  2. CHANGED HEADINGS IN FDCFP FILE FOR NEW    *
     H*                     SPLTY CODE AMOUNTS                        *
     H*                                                               *
     H*  04/26/93 C:20   PROJ: FS098                                  *
     H*                  1. ADDED CODE TO GET GENERIC HEADINGS FROM   *
     H*                     FDCFP DEPENDING UPON STATE                *
     H*
     H* 12/10/92 C:20 REQ/PROJ:                                       *
     H*               1. ADDED OTHER MRA 2 FIELD TO ACCOMODATE AL     *
     H*                                                               *
     H* 09/16/92 KRR  REQ/PROJ: 10040/FS040                           *
     H*               1. ADDED SCR02 DETAIL INQUIRY                   *
     H*               2. ADDED F5(DET. INQUIRY) TO SCR01              *
     H*****************************************************************
     FF0520F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     F*** BEGIN CHG *** 06/09/98 **************************************
     F********FMPRL   IF  E           K        DISK
     FFMPRL     IF   E           K DISK    USROPN
     F*** END CHG *** 06/09/98 ****************************************
     FSMUPL     IF   E           K DISK
     FFDCFL     IF   E           K DISK
     FSTABL     IF   E           K DISK

     D FL              S              1    DIM(14)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEY             S              5    DIM(2)
     D KEY2            S              8  0 DIM(2)
     D*** BEGIN ADD *** 06/09/98 **************************************
     D QCA             S             56    DIM(2) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*----------------------------------------------------------------
     D*   QCMDEXC CMD STRING MAPPED
     D*----------------------------------------------------------------
     D                 DS
     D  @XCMD                  1     56
     D  @XST                  38     39
     D*** END ADD *** 06/09/98 ****************************************
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  PR                     1     70
     D                                     DIM(14)
     D  SCPR01                 1      5
     D  SCPR02                 6     10
     D  SCPR03                11     15
     D  SCPR04                16     20
     D  SCPR05                21     25
     D  SCPR06                26     30
     D  SCPR07                31     35
     D  SCPR08                36     40
     D  SCPR09                41     45
     D  SCPR10                46     50
     D  SCPR11                51     55
     D  SCPR12                56     60
     D  SCPR13                61     65
     D  SCPR14                66     70
     D                 DS
     D  NM                     1    700
     D                                     DIM(14)
     D  SCNM01                 1     50
     D  SCNM02                51    100
     D  SCNM03               101    150
     D  SCNM04               151    200
     D  SCNM05               201    250
     D  SCNM06               251    300
     D  SCNM07               301    350
     D  SCNM08               351    400
     D  SCNM09               401    450
     D  SCNM10               451    500
     D  SCNM11               501    550
     D  SCNM12               551    600
     D  SCNM13               601    650
     D  SCNM14               651    700
     D                 DS
     D  ST                     1     14
     D                                     DIM(14)
     D  SCST01                 1      1
     D  SCST02                 2      2
     D  SCST03                 3      3
     D  SCST04                 4      4
     D  SCST05                 5      5
     D  SCST06                 6      6
     D  SCST07                 7      7
     D  SCST08                 8      8
     D  SCST09                 9      9
     D  SCST10                10     10
     D  SCST11                11     11
     D  SCST12                12     12
     D  SCST13                13     13
     D  SCST14                14     14
     D                 DS
     D  ED                     1    112  0
     D                                     DIM(14)
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
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    PRCPT4        XXKEY
     C     *LIKE         DEFINE    PREFDT        XXKEY2
     C     *LIKE         DEFINE    STKEY         XXKEY3
     C*
     C*  CPT4 CODE TABLE FILE
     C*
     C     CPKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY
     C                   KFLD                    XXKEY2
     C*
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
     C*  STATE CPT FIELD HEADINGS
     C*
     C     CFKEY         KLIST
     C                   KFLD                    SCST
     C*
     C*  KEY LIST FOR STATE FILE
     C     STKY          KLIST
     C                   KFLD                    XXKEY3
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQKEY             5
     C                   PARM                    QQKEY2            8 0
     C                   PARM                    QQST              2            ADD 04/26/93
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
     C                   MOVE      *BLANK        SCST
     C                   MOVE      QQST          SCST
     C*
     C                   MOVEL     SCST          XXKEY3
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     STNAME        SCSTNM
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
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*** BEGIN ADD *** 06/09/98 **************************************
     C*
     C*          OVERRIDE TO FMPRL
     C*          ------------------------------------------------------
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        @XCMD
     C                   MOVEL     SCST          @XST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @XCMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG            1
     C*
     C*          OPEN FILE FOR PROCESSING
     C*          ------------------------------------------------------
     C                   OPEN      FMPRL
     C*** END ADD *** 06/09/98 ****************************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED
     C*
     C                   MOVEL     QQKEY         SCCPT4
     C                   MOVEL     QQKEY2        SCEFDT
     C                   GOTO      REPOS
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
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
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *BLANK        QQKEY
     C                   MOVE      *ZERO         QQKEY2
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   F5 = DETAIL INQUIRY
     C*
     C     *INKE         IFEQ      '1'
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       7             I1                3 0
     C*
     C*  VALID CURSOR POSITION - GOTO SCR02
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     14
     C                   MOVEL     PR(I1)        XXKEY
     C*
     C                   Z-ADD     ED(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXKEY2
     C*
     C     CPKY1         CHAIN     PPR01                              60
     C                   MOVE      PRDSC1        SCDSC1
     C                   MOVE      PRDSC2        SCDSC2
     C                   MOVE      PRMRA         SCMRA
     C                   MOVE      PRFOLU        SCFOLU
     C                   MOVE      PRANAM        SCANAM
     C                   MOVE      PR1A2         SC1A2                          ADD 04/26/93
     C                   MOVE      PRPROF        SCPROF
     C                   MOVE      PR1A1         SC1A1
     C                   MOVE      PRRPT         SCRPT
     C                   MOVE      PRSURG        SCSURG
     C                   MOVE      PRSUPP        SCSUPP
     C                   MOVE      PRFSR         SCFSR
     C                   Z-ADD     PR92N1        SC92N1
     C                   Z-ADD     PR92N2        SC92N2                         ADD 12/10/92
     C                   Z-ADD     PR92N3        SC92N3
     C                   Z-ADD     PR92N4        SC92N4                         ADD 07/20/93
     C                   Z-ADD     PR92N5        SC92N5                         ADD 07/20/93
     C                   MOVE      PR1A3         SC1A3
     C                   MOVE      PR1A4         SC1A4
     C                   MOVE      PR1A5         SC1A5
     C                   MOVE      PRSTAT        SCSTAT
     C                   MOVEL     PRCPT4        SCCPT4
     C*
     C                   Z-ADD     PREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVEA     '000000'      *IN(31)
     C                   MOVE      '1'           *IN37
     C                   MOVE      '0'           *IN65                          ADD 07/20/93
     C                   MOVE      '0'           *IN66                          ADD 07/20/93
     C     SCST          IFEQ      'AL'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN33                          ADD 07/20/93
     C                   MOVE      '1'           *IN65                          ADD 07/20/93
     C                   MOVE      '1'           *IN66                          ADD 07/20/93
     C                   ELSE
     C     SCST          IFEQ      'FL'
     C                   MOVE      '1'           *IN31
     C                   ELSE
     C     SCST          IFEQ      'GA'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '0'           *IN37
     C                   ELSE
     C     SCST          IFEQ      'LA'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN35
     C                   MOVE      '1'           *IN36
     C                   ELSE
     C     SCST          IFEQ      'NY'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN33
     C                   MOVE      '1'           *IN65                          ADD 07/20/93
     C                   END                                                    END = NY
     C                   END                                                    END = LA
     C                   END                                                    END = GA
     C                   END                                                    END = FL
     C                   END                                                    END = AL
     C*
     C     CFKEY         CHAIN     PCF01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CFHMRA        SCHMRA
     C                   MOVEL     CF92H1        SC92H1
     C                   MOVEL     CF92H2        SC92H2
     C                   MOVEL     CF92H3        SC92H3
     C                   MOVEL     CF92H4        SC92H4                         ADD 07/20/93
     C                   MOVEL     CF92H5        SC92H5                         ADD 07/20/93
     C                   MOVEL     CF1H3         SC1H3
     C                   MOVEL     CF1H4         SC1H4
     C                   MOVEL     CF1H5         SC1H5
     C                   ELSE
     C                   MOVE      *BLANKS       SCHMRA
     C                   MOVE      *BLANKS       SC92H1
     C                   MOVE      *BLANKS       SC92H2
     C                   MOVE      *BLANKS       SC92H3
     C                   MOVE      *BLANKS       SC92H4                         ADD 07/20/93
     C                   MOVE      *BLANKS       SC92H5                         ADD 07/20/93
     C                   MOVE      *BLANKS       SC1H3
     C                   MOVE      *BLANKS       SC1H4
     C                   MOVE      *BLANKS       SC1H5
     C                   END
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*   COMMAND 12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *BLANK        QQKEY
     C                   MOVE      *ZERO         QQKEY2
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      *BLANKS       SCCPT4
     C                   MOVE      *ZEROS        SCEFDT
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
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
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
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
     C                   MOVEL     KEY(2)        XXKEY
     C*
     C                   Z-ADD     KEY2(2)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXKEY2
     C*
     C     CPKY1         CHAIN     PPR01                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     KEY(1)        XXKEY
     C*
     C                   Z-ADD     KEY2(1)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXKEY2
     C*
     C     CPKY1         CHAIN     PPR01                              60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCCPT4        IFNE      *BLANK
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEY
     C                   MOVE      *ZEROS        KEY2
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVEL     SCCPT4        XXKEY
     C                   MOVE      *BLANK        SCCPT4
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXKEY2
     C*
     C                   MOVE      *ZEROS        SCEFDT
     C     CPKY1         SETLL     PPR01
     C     UPCO#         READE     PPR01                                  60
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
     C                   MOVEL     PR(I1)        QQKEY
     C                   MOVE      ED(I1)        QQKEY2
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANK        QQKEY
     C                   MOVE      *ZEROS        QQKEY2
     C                   GOTO      TERM
     C*
     C*   PROCESS REQUEST SCREEN TWO
     C*
     C     SCR02T        TAG
     C*
     C                   MOVE      *BLANKS       SCCPT4
     C                   MOVE      *ZEROS        SCEFDT
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       PR
     C                   MOVE      *BLANKS       NM
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *ZEROS        ED
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVEL     PRCPT4        PR(I1)
     C                   MOVE      PRSTAT        ST(I1)
     C                   MOVEL     PRDSC1        NM(I1)
     C*
     C                   Z-ADD     PREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ED(I1)
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
     C     UPCO#         READE     PPR01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVEL     PR(1)         KEY(1)
     C                   MOVEL     PR(14)        KEY(2)
     C                   MOVE      ED(1)         KEY2(1)
     C                   MOVE      ED(14)        KEY2(2)
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
     C                   READP     PPR01                                  60
     C  N60PRCO#         COMP      UPCO#                              6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVEL     PR(1)         KEY(1)
     C                   MOVEL     PR(14)        KEY(2)
     C                   MOVE      ED(1)         KEY2(1)
     C                   MOVE      ED(14)        KEY2(2)
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
     C                   MOVE      *BLANK        XXKEY
     C     CPKY1         SETLL     PPR01
     C     UPCO#         READE     PPR01                                  60
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
     C     UPCO#         READE     PPR01                                  60
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
**  QCMDEXC TABLE
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
