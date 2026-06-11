     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE SC020R - CUSTOM GROUPING ID MAINTENENCE
      *****************************************************************
      * INDICATOR USAGE
      * SCR02:
      *  20    DESCRIPTION REQUIRED
      *  21    TABLE BACKUP FLAGS MUST BE Y OR N, IF ENTERED
      *  22    MANDATORY ENTRY FLAGS MUST BE Y OR N, IF ENTERED
      *  23    EDIT VALUE FLAGS FOR ALPHA FIELDS NOT ALLOWED - PROTECT/NONDISP
      *  24    EDIT VALUE FLAGS FOR NUMERIC FIELDS MUST BE Y OR N, IF ENTERED
      *  25    EDIT VALUE FLAGS FOR AMOUNT FIELDS MUST BE 1/2/BLK, IF ENTERED
      *  26    SCM0549 - VALID RECORD FORMAT REQUIRED (A OR B)
      *  27    RECORD FORMAT "B" IN USE
      *****************************************************************
      * REVISIONS
      *** 07/19/91 *** LJD *** 9999:90007/D0007
      ***   1.  Added record formats:
      ***       "A" for original 10A/6.0N/9.2N fields
      ***       "B" for new 35A fields
      ***   2.  Indicated separate subroutines for each record format
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
     FSC020F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR02S:RRN2)
     F*
     FSMCGL     UF A E           K DISK
     F*
      *****************************************************************
      * EXCEPTION SPECIFICATIONS * * *
      *****************************************************************

     D*
     D*** Arrays for 10A/6.0N/9.2N fields
     D*
     D FL              S              4    DIM(20) CTDATA PERRCD(20)            FIELD NAMES
     D AT              S              6    DIM(20) CTDATA PERRCD(10)            ATTRIBUTES
     D*
     D FB              S              4    DIM(6) CTDATA PERRCD(6)              FIELD NAMES
     D AB              S              6    DIM(6) CTDATA PERRCD(6)              ATTRIBUTES
     D*
      *****************************************************************
      * INPUT SPECIFICATIONS  * * *
      *****************************************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D                 DS
     D  DS                     1    200
     D                                     DIM(20)                              USR ASGN FLD DSC
     D  CGDA01                 1     10
     D  CGDA02                11     20
     D  CGDA03                21     30
     D  CGDA04                31     40
     D  CGDA05                41     50
     D  CGDA06                51     60
     D  CGDA07                61     70
     D  CGDA08                71     80
     D  CGDA09                81     90
     D  CGDA10                91    100
     D  CGDD01               101    110
     D  CGDD02               111    120
     D  CGDD03               121    130
     D  CGDD04               131    140
     D  CGDD05               141    150
     D  CGDN01               151    160
     D  CGDN02               161    170
     D  CGDN03               171    180
     D  CGDN04               181    190
     D  CGDN05               191    200
     D*
     D*
     D                 DS
     D  TL                     1     20
     D                                     DIM(20)                              TABLE BACKUP FLDS
     D  CGTA01                 1      1
     D  CGTA02                 2      2
     D  CGTA03                 3      3
     D  CGTA04                 4      4
     D  CGTA05                 5      5
     D  CGTA06                 6      6
     D  CGTA07                 7      7
     D  CGTA08                 8      8
     D  CGTA09                 9      9
     D  CGTA10                10     10
     D  CGTD01                11     11
     D  CGTD02                12     12
     D  CGTD03                13     13
     D  CGTD04                14     14
     D  CGTD05                15     15
     D  CGTN01                16     16
     D  CGTN02                17     17
     D  CGTN03                18     18
     D  CGTN04                19     19
     D  CGTN05                20     20
     D*
     D                 DS
     D  SL                     1     60  0
     D                                     DIM(20)                              SECURITY LEVELS
     D  CGSA01                 1      3  0
     D  CGSA02                 4      6  0
     D  CGSA03                 7      9  0
     D  CGSA04                10     12  0
     D  CGSA05                13     15  0
     D  CGSA06                16     18  0
     D  CGSA07                19     21  0
     D  CGSA08                22     24  0
     D  CGSA09                25     27  0
     D  CGSA10                28     30  0
     D  CGSD01                31     33  0
     D  CGSD02                34     36  0
     D  CGSD03                37     39  0
     D  CGSD04                40     42  0
     D  CGSD05                43     45  0
     D  CGSN01                46     48  0
     D  CGSN02                49     51  0
     D  CGSN03                52     54  0
     D  CGSN04                55     57  0
     D  CGSN05                58     60  0
     D*
     D                 DS
     D  ME                     1     20
     D                                     DIM(20)                              MANDATORY ENTRY FIEL
     D  CGMA01                 1      1
     D  CGMA02                 2      2
     D  CGMA03                 3      3
     D  CGMA04                 4      4
     D  CGMA05                 5      5
     D  CGMA06                 6      6
     D  CGMA07                 7      7
     D  CGMA08                 8      8
     D  CGMA09                 9      9
     D  CGMA10                10     10
     D  CGMD01                11     11
     D  CGMD02                12     12
     D  CGMD03                13     13
     D  CGMD04                14     14
     D  CGMD05                15     15
     D  CGMN01                16     16
     D  CGMN02                17     17
     D  CGMN03                18     18
     D  CGMN04                19     19
     D  CGMN05                20     20
     D*
     D                 DS
     D  EV                     1     20
     D                                     DIM(20)                              EDIT VALUES
     D  CGED01                11     11
     D  CGED02                12     12
     D  CGED03                13     13
     D  CGED04                14     14
     D  CGED05                15     15
     D  CGEN01                16     16
     D  CGEN02                17     17
     D  CGEN03                18     18
     D  CGEN04                19     19
     D  CGEN05                20     20
     D*
     D                 DS
     D  EP                     1    200
     D                                     DIM(20)                              EDIT PROGRAMS
     D  CGPA01                 1     10
     D  CGPA02                11     20
     D  CGPA03                21     30
     D  CGPA04                31     40
     D  CGPA05                41     50
     D  CGPA06                51     60
     D  CGPA07                61     70
     D  CGPA08                71     80
     D  CGPA09                81     90
     D  CGPA10                91    100
     D  CGPD01               101    110
     D  CGPD02               111    120
     D  CGPD03               121    130
     D  CGPD04               131    140
     D  CGPD05               141    150
     D  CGPN01               151    160
     D  CGPN02               161    170
     D  CGPN03               171    180
     D  CGPN04               181    190
     D  CGPN05               191    200
     D*
     D*** BEGIN ADD *** 07/19/91 *************************************
     D                 DS
     D  DB                     1     60
     D                                     DIM(6)                               USR ASGN FLD DSC
     D  CGDB01                 1     10
     D  CGDB02                11     20
     D  CGDB03                21     30
     D  CGDB04                31     40
     D  CGDB05                41     50
     D  CGDB06                51     60
     D*
     D                 DS
     D  TB                     1      6
     D                                     DIM(6)                               TABLE BACKUP FLDS
     D  CGTB01                 1      1
     D  CGTB02                 2      2
     D  CGTB03                 3      3
     D  CGTB04                 4      4
     D  CGTB05                 5      5
     D  CGTB06                 6      6
     D*
     D                 DS
     D  SB                     1     18  0
     D                                     DIM(6)                               SECURITY LEVELS
     D  CGSB01                 1      3  0
     D  CGSB02                 4      6  0
     D  CGSB03                 7      9  0
     D  CGSB04                10     12  0
     D  CGSB05                13     15  0
     D  CGSB06                16     18  0
     D*
     D                 DS
     D  MB                     1      6
     D                                     DIM(6)                               MANDATORY ENTRY FIEL
     D  CGMB01                 1      1
     D  CGMB02                 2      2
     D  CGMB03                 3      3
     D  CGMB04                 4      4
     D  CGMB05                 5      5
     D  CGMB06                 6      6
     D*
     D                 DS
     D  PB                     1     60
     D                                     DIM(6)                               EDIT PROGRAMS
     D  CGPB01                 1     10
     D  CGPB02                11     20
     D  CGPB03                21     30
     D  CGPB04                31     40
     D  CGPB05                41     50
     D  CGPB06                51     60
     D*
      * * *  FILE INFORMATION DATA STRUCTURE
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************

     C*
     C*   KEY LIST FOR THE CUSTOM GROUP ID FILE
     C*
     C     CGKY          KLIST
     C                   KFLD                    SCCSID
     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    CGDT          HHDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCALL            1
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
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C*
     C                   EXSR      INIT
     C*
     C*   DISPLAY ALL FORMATS
     C*
      *------------------------*
      *** DISPLAY POINT
      *------------------------*
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR02
     C                   EXFMT     SCR02C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C/EJECT
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      '1'
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F5 = CUSTOM GROUP ID SEARCH        *
     C****************************************
     C     *INKE         IFEQ      '1'
     C                   CALL      'SC520R'
     C                   PARM                    SCCSID
     C                   PARM                    SCRFMT
     C                   PARM      *BLANKS       QQRTCD            1
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCCSID        OREQ      *BLANK
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   GOTO      SCR01T
     C                   END                                                    ENDEQ '*'
     C                   END                                                    END *INKE
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02S'
     C     Q2RECF        OREQ      'SCR02C'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END *INKL
     C****************************************
     C*   *IN10 -  PROCESS HELP REQUESTS     *
     C****************************************
     C  N60
     CAN 60              MOVE      '0'           *IN10                            TEMP * STUFF
     C     *IN10         IFEQ      '1'
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
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C*
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C*
      *****************************************************************
      *   GOTO THE APPROPRIATE SCREEN
      *****************************************************************
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR02S'      SCR02T
     C     Q2RECF        CABEQ     'SCR02C'      SCR02T
     C*
     C*   PROCESS REQUESTED SCREEN
     C*
      *------------------------*
     C     SCR01T        TAG
      *------------------------*
     C*
     C     CGKY          CHAIN     PCG01                              60
     C*
     C*** Validate record format (ADD ONLY)
     C     *IN60         IFEQ      '1'
     C     SCRFMT        IFNE      'A'
     C     SCRFMT        ANDNE     'B'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END                                                    END NE A/B
     C                   ELSE                                                   END EQ '1'
     C                   MOVE      CGRFMT        SCRFMT
     C                   END                                                    END EQ '1'
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02C
     C                   MOVE      '0'           *IN04
     C*
     C     *IN60         IFEQ      '0'
     C*
     C*** Depending upon Record Format, call a different LOAD subroutine
     C     SCRFMT        IFEQ      'B'
     C                   EXSR      LOADB                                        LOAD RCDS FROM FILE
     C                   ELSE
     C                   EXSR      LOADA                                        LOAD RCDS FROM FILE
     C                   END                                                    END EQ 'B'
     C                   ELSE                                                   ELSE 0 *IN60
     C     SCRFMT        IFEQ      'B'
     C                   EXSR      BLANKB                                       LOAD BLANKS
     C                   ELSE
     C                   EXSR      BLANKA                                       LOAD RCDS FROM FILE
     C                   END                                                    END EQ 'B'
     C                   END                                                    END 0 *IN60
     C*
     C*** GOTO SCREEN 2
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C                   GOTO      STRT
     C*
      *------------------------*
     C     SCR02T        TAG
      *------------------------*
     C*
     C*** CUSTOM GROUP ID DESCRIPTION REQUIRED
     C     SCDESC        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END                                                    END EQ *BLANKS
     C*
     C     SCRFMT        IFEQ      'B'
     C                   EXSR      EDITB
     C                   ELSE                                                   ELS EQ 'B'
     C                   EXSR      EDITA
     C                   END                                                    END EQ 'B'
     C*
     C                   GOTO      TOP
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
     C*
      *====================================================================*
      * * *   EDITA SUBROUTINE                                 E D I T A ***
      *====================================================================*
     C     EDITA         BEGSR
     C*
     C*** EDIT TABLE FLAGS Y/N IF HEADING ENTERED
     C                   MOVE      ' '           ERR               1
     C                   MOVE      ' '           ERR#1             1
     C                   DO        20            RRN2
     C     RRN2          CHAIN     SCR02S                             60
     C                   MOVE      '0'           *IN21
     C     SCHEAD        IFNE      *BLANKS
     C     SCTBUP        IFNE      'Y'
     C     SCTBUP        ANDNE     'N'
     C*
     C*** RRN2 USED TO POSITION SCREEN FOR ERROR MESSAGE
     C*** POSITION SCREEN FOR 1ST ERROR OCCURANCE ONLY
     C     ERR#1         IFNE      '1'
     C                   Z-ADD     RRN2          #@$2
     C                   MOVE      '1'           ERR#1
     C                   END                                                    END ERR#1 NE '1'
     C*
     C                   MOVE      '1'           ERR
     C                   MOVE      '1'           *IN21
     C                   END                                                    END NE Y/N
     C                   UPDATE    SCR02S
     C                   END                                                    END NE *BLANKS
     C     ERR           CABEQ     '1'           STRT
     C                   END                                                    END DO
     C*
     C*** EDIT MANDATORY ENTRY FLAGS Y/N IF HEADING ENTERED
     C                   MOVE      ' '           ERR               1
     C                   MOVE      ' '           ERR#1             1
     C                   DO        20            RRN2
     C     RRN2          CHAIN     SCR02S                             60
     C                   MOVE      '0'           *IN22
     C     SCHEAD        IFNE      *BLANKS
     C     SCMANE        IFNE      'Y'
     C     SCMANE        ANDNE     'N'
     C*
     C*** RRN2 USED TO POSITION SCREEN FOR ERROR MESSAGE
     C*** POSITION SCREEN FOR 1ST ERROR OCCURANCE ONLY
     C     ERR#1         IFNE      '1'
     C                   Z-ADD     RRN2          #@$2
     C                   MOVE      '1'           ERR#1
     C                   END                                                    END ERR#1 NE '1'
     C*
     C                   MOVE      '1'           ERR
     C                   MOVE      '1'           *IN22
     C                   END                                                    END NE Y/N
     C                   UPDATE    SCR02S
     C                   END                                                    END NE *BLANKS
     C     ERR           CABEQ     '1'           STRT
     C                   END                                                    END DO
     C*
     C*** EDIT EDIT VALUE FLAGS FOR NUMERIC FIELD MUST BE Y/N IF HEADING ENTERED
     C                   MOVE      ' '           ERR               1
     C                   MOVE      ' '           ERR#1             1
     C                   DO        20            RRN2
     C     RRN2          CHAIN     SCR02S                             60
     C                   MOVE      '0'           *IN24
     C                   MOVEL     SCFLD         XXFLD             2
     C     SCHEAD        IFNE      *BLANKS
     C     XXFLD         IFEQ      'UD'
     C     SCEDTV        IFNE      'Y'
     C     SCEDTV        ANDNE     'N'
     C*
     C*** RRN2 USED TO POSITION SCREEN FOR ERROR MESSAGE
     C*** POSITION SCREEN FOR 1ST ERROR OCCURANCE ONLY
     C     ERR#1         IFNE      '1'
     C                   Z-ADD     RRN2          #@$2
     C                   MOVE      '1'           ERR#1
     C                   END                                                    END ERR#1 NE '1'
     C*
     C                   MOVE      '1'           ERR
     C                   MOVE      '1'           *IN24
     C                   END                                                    END NE Y/N
     C                   END                                                    END EQ 'UD'
     C                   UPDATE    SCR02S
     C                   END                                                    END NE *BLANKS
     C     ERR           CABEQ     '1'           STRT
     C                   END                                                    END DO
     C*
     C*** EDIT EDIT VALUE FLAGS FOR AMOUNT FIELDS MUST BE 1/2/BLANK, IF HEADING
     C                   MOVE      ' '           ERR               1
     C                   MOVE      ' '           ERR#1             1
     C                   DO        20            RRN2
     C     RRN2          CHAIN     SCR02S                             60
     C                   MOVE      '0'           *IN25
     C                   MOVEL     SCFLD         XXFLD             2
     C     SCHEAD        IFNE      *BLANKS
     C     XXFLD         IFEQ      'UN'
     C     SCEDTV        IFNE      '1'
     C     SCEDTV        ANDNE     '2'
     C     SCEDTV        ANDNE     *BLANKS
     C*
     C*** RRN2 USED TO POSITION SCREEN FOR ERROR MESSAGE
     C*** POSITION SCREEN FOR 1ST ERROR OCCURANCE ONLY
     C     ERR#1         IFNE      '1'
     C                   Z-ADD     RRN2          #@$2
     C                   MOVE      '1'           ERR#1
     C                   END                                                    END ERR#1 NE '1'
     C*
     C                   MOVE      '1'           ERR
     C                   MOVE      '1'           *IN25
     C                   END                                                    END NE 1/2/BLANKS
     C                   END                                                    END EQ 'UN'
     C                   UPDATE    SCR02S
     C                   END                                                    END NE *BLANKS
     C     ERR           CABEQ     '1'           STRT
     C                   END                                                    END DO
     C*
     C*** ALL EDITS PASSED, WRITE DATA BASE RECORDS.
     C                   EXSR      WRITEA
     C*
     C                   ENDSR
     C*
      *====================================================================*
      * * *   EDITB SUBROUTINE                                 E D I T B ***
      *====================================================================*
     C     EDITB         BEGSR
     C*
     C*** EDIT TABLE FLAGS Y/N IF HEADING ENTERED
     C                   MOVE      ' '           ERR               1
     C                   MOVE      ' '           ERR#1             1
     C                   DO        6             RRN2
     C     RRN2          CHAIN     SCR02S                             60
     C                   MOVE      '0'           *IN21
     C     SCHEAD        IFNE      *BLANKS
     C     SCTBUP        IFNE      'Y'
     C     SCTBUP        ANDNE     'N'
     C*
     C*** RRN2 USED TO POSITION SCREEN FOR ERROR MESSAGE
     C*** POSITION SCREEN FOR 1ST ERROR OCCURANCE ONLY
     C     ERR#1         IFNE      '1'
     C                   Z-ADD     RRN2          #@$2
     C                   MOVE      '1'           ERR#1
     C                   END                                                    END ERR#1 NE '1'
     C*
     C                   MOVE      '1'           ERR
     C                   MOVE      '1'           *IN21
     C                   END                                                    END NE Y/N
     C                   UPDATE    SCR02S
     C                   END                                                    END NE *BLANKS
     C     ERR           CABEQ     '1'           STRT
     C                   END                                                    END DO
     C*
     C*** EDIT MANDATORY ENTRY FLAGS Y/N IF HEADING ENTERED
     C                   MOVE      ' '           ERR               1
     C                   MOVE      ' '           ERR#1             1
     C                   DO        6             RRN2
     C     RRN2          CHAIN     SCR02S                             60
     C                   MOVE      '0'           *IN22
     C     SCHEAD        IFNE      *BLANKS
     C     SCMANE        IFNE      'Y'
     C     SCMANE        ANDNE     'N'
     C*
     C*** RRN2 USED TO POSITION SCREEN FOR ERROR MESSAGE
     C*** POSITION SCREEN FOR 1ST ERROR OCCURANCE ONLY
     C     ERR#1         IFNE      '1'
     C                   Z-ADD     RRN2          #@$2
     C                   MOVE      '1'           ERR#1
     C                   END                                                    END ERR#1 NE '1'
     C*
     C                   MOVE      '1'           ERR
     C                   MOVE      '1'           *IN22
     C                   END                                                    END NE Y/N
     C                   UPDATE    SCR02S
     C                   END                                                    END NE *BLANKS
     C     ERR           CABEQ     '1'           STRT
     C                   END                                                    END DO
     C*
     C*** ALL EDITS PASSED, WRITE DATA BASE RECORDS.
     C                   EXSR      WRITEB
     C*
     C                   ENDSR
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C*
     C     INIT          BEGSR
     C                   TIME                    DSDTTM
     C                   MOVE      '0'           XXOPN             1
     C                   MOVE      *HIVAL        @@TIME            6 0
     C                   Z-ADD     0             RRN2              5 0
     C                   Z-ADD     0             HLDRRN            5 0
     C                   Z-ADD     0             HLDRR2            5 0
     C                   Z-ADD     0             XXTIME            6 0
     C                   Z-ADD     0             XXDATE            6 0
     C                   Z-ADD     0             XXSEQ#            5 0
     C                   MOVE      *BLANK        SCDESC
     C                   Z-ADD     *ZEROS        SCRCDS
     C                   MOVE      *BLANK        DS
     C                   MOVE      *BLANK        TL
     C                   MOVE      *BLANK        SL
     C                   MOVE      *BLANK        ME
     C                   MOVE      *BLANK        EV
     C                   MOVE      *BLANK        EP
     C*
     C                   MOVE      *BLANK        DB
     C                   MOVE      *BLANK        TB
     C                   MOVE      *BLANK        SB
     C                   MOVE      *BLANK        MB
     C                   MOVE      *BLANK        PB
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      SCCSID        HLCSID            2
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   ENDSR
     C*
     C*====================================================================*
     C* * *   LOADA SUBROUTINE                                    L O A D A ***
     C*====================================================================*
     C     LOADA         BEGSR
     C                   MOVE      '0'           *IN27
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 20 ITEMS
     C                   MOVEL     CGDESC        SCDESC
     C                   Z-ADD     CGRCDS        SCRCDS
     C                   DO        20            RRN2              5 0
     C                   MOVEL     FL(RRN2)      SCFLD
     C                   MOVEL     AT(RRN2)      SCATTR
     C                   MOVEL     DS(RRN2)      SCHEAD
     C                   MOVEL     TL(RRN2)      SCTBUP
     C                   MOVEL     SL(RRN2)      SCSCLV
     C                   MOVEL     ME(RRN2)      SCMANE
     C*
     C*** IF THIS IS AN ALPHA FIELD, PROTECT/NON-DISPLAY EDIT VALUE (W/ *IN23)
     C                   MOVEL     SCFLD         XXFLD
     C     XXFLD         IFEQ      'UA'
     C                   MOVE      *BLANKS       SCEDTV
     C                   MOVE      '1'           *IN23
     C                   ELSE                                                   ELSE XXFLD EQ 'UA'
     C                   MOVEL     EV(RRN2)      SCEDTV
     C                   MOVE      '0'           *IN23
     C                   END                                                    END  XXFLD EQ 'UA'
     C*
     C                   MOVEL     EP(RRN2)      SCEDTP
     C                   WRITE     SCR02S
     C                   END
     C                   Z-ADD     1             #@$2
     C                   ENDSR
     C*
     C*====================================================================*
     C* * *   LOADB SUBROUTINE                                    L O A D B ***
     C*====================================================================*
     C     LOADB         BEGSR
     C                   MOVE      '1'           *IN27
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 6 ITEMS
     C                   MOVEL     CGDESC        SCDESC
     C                   Z-ADD     CGRCDS        SCRCDS
     C                   DO        6             RRN2              5 0
     C                   MOVEL     FB(RRN2)      SCFLD
     C                   MOVEL     AB(RRN2)      SCATTR
     C                   MOVEL     DB(RRN2)      SCHEAD
     C                   MOVEL     TB(RRN2)      SCTBUP
     C                   MOVEL     SB(RRN2)      SCSCLV
     C                   MOVEL     MB(RRN2)      SCMANE
     C                   MOVEL     PB(RRN2)      SCEDTP                         ADD 07/18/96
     C*
     C*** IF THIS IS AN ALPHA FIELD, PROTECT/NON-DISPLAY EDIT VALUE (W/ *IN23)
     C                   MOVEL     SCFLD         XXFLD
     C     XXFLD         IFEQ      'UB'
     C                   MOVE      *BLANKS       SCEDTV
     C                   MOVE      '1'           *IN23
     C                   ELSE                                                   ELSE XXFLD EQ 'UB'
     C                   MOVEL     EV(RRN2)      SCEDTV
     C                   MOVE      '0'           *IN23
     C                   END                                                    END  XXFLD EQ 'UB'
     C*
     C                   WRITE     SCR02S
     C                   END
     C                   Z-ADD     1             #@$2
     C                   ENDSR
     C*====================================================================*
     C* * *   BLANKA SUBROUTINE                                 B L A N K A ***
     C*====================================================================*
     C     BLANKA        BEGSR
     C                   MOVE      '0'           *IN27
     C                   MOVE      *BLANKS       SCDESC
     C                   Z-ADD     *ZEROS        SCRCDS
     C                   DO        20            RRN2              5 0
     C                   MOVEL     FL(RRN2)      SCFLD
     C                   MOVEL     AT(RRN2)      SCATTR
     C                   MOVE      *BLANKS       SCHEAD
     C                   MOVE      *BLANKS       SCTBUP
     C                   MOVE      *BLANKS       SCSCLV
     C                   MOVE      *BLANKS       SCMANE
     C*
     C*** IF THIS IS AN ALPHA FIELD, PROTECT/NON-DISPLAY EDIT VALUE (W/ *IN23)
     C                   MOVEL     SCFLD         XXFLD
     C                   MOVE      *BLANKS       SCEDTV
     C     XXFLD         IFEQ      'UA'
     C                   MOVE      '1'           *IN23
     C                   ELSE                                                   ELSE XXFLD EQ 'UA'
     C                   MOVE      '0'           *IN23
     C                   END                                                    END  XXFLD EQ 'UA'
     C*
     C                   MOVE      *BLANKS       SCEDTP
     C                   WRITE     SCR02S
     C                   END
     C                   Z-ADD     1             #@$2
     C                   ENDSR
     C*====================================================================*
     C* * *   BLANKB SUBROUTINE                                 B L A N K B ***
     C*====================================================================*
     C     BLANKB        BEGSR
     C                   MOVE      '1'           *IN27
     C                   MOVE      *BLANKS       SCDESC
     C                   Z-ADD     *ZEROS        SCRCDS
     C                   DO        6             RRN2              5 0
     C                   MOVEL     FB(RRN2)      SCFLD
     C                   MOVEL     AB(RRN2)      SCATTR
     C                   MOVE      *BLANKS       SCHEAD
     C                   MOVE      *BLANKS       SCTBUP
     C                   MOVE      *BLANKS       SCSCLV
     C                   MOVE      *BLANKS       SCMANE
     C*
     C*** IF THIS IS AN ALPHA FIELD, PROTECT/NON-DISPLAY EDIT VALUE (W/ *IN23)
     C                   MOVEL     SCFLD         XXFLD
     C                   MOVE      *BLANKS       SCEDTV
     C     XXFLD         IFEQ      'UB'
     C                   MOVE      '1'           *IN23
     C                   ELSE                                                   ELSE XXFLD EQ 'UB'
     C                   MOVE      '0'           *IN23
     C                   END                                                    END  XXFLD EQ 'UB'
     C*
     C                   MOVE      *BLANKS       SCEDTP
     C                   WRITE     SCR02S
     C                   END
     C                   Z-ADD     1             #@$2
     C                   ENDSR
      *====================================================================*
      * * *   WRITEA SUBROUTINE                                 W R I T E A ***
      *====================================================================*
     C*
     C     WRITEA        BEGSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C*
     C                   Z-ADD     DSTIME        HHTIME            6 0
     C*
     C     CGKY          CHAIN     PCG01                              60
     C*
     C*
     C                   DO        20            RRN2
     C                   Z-ADD     RRN2          X                 5 0
     C*
     C     RRN2          CHAIN     SCR02S                             50
     C*
     C     *IN50         IFEQ      '0'
     C                   MOVEL     SCHEAD        DS(X)
     C                   MOVEL     SCTBUP        TL(X)
     C                   MOVEL     SCSCLV        SL(X)
     C                   MOVEL     SCMANE        ME(X)
     C                   MOVEL     SCEDTV        EV(X)
     C                   MOVEL     SCEDTP        EP(X)
     C                   END
     C*
     C                   END
     C*
     C                   MOVEL     SCCSID        CGCSID
     C                   MOVEL     SCDESC        CGDESC
     C                   Z-ADD     SCRCDS        CGRCDS
     C                   MOVEL     *BLANK        CGSTAT
     C                   MOVEL     Q1PGM         CGPGM
     C                   MOVEL     Q1USER        CGUSER
     C                   MOVEL     Q1USER        CGIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CGDT
     C*
     C                   Z-ADD     DSTIME        CGTIME
     C                   MOVE      SCRFMT        CGRFMT
     C*
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PCG01
     C                   ELSE
     C                   WRITE     PCG01
     C                   END
     C*
     C*
     C                   ENDSR
     C*
      *====================================================================*
      * * *   WRITEB SUBROUTINE                                 W R I T E B ***
      *====================================================================*
     C*
     C     WRITEB        BEGSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C*
     C                   Z-ADD     DSTIME        HHTIME            6 0
     C*
     C     CGKY          CHAIN     PCG01                              60
     C*
     C                   DO        6             RRN2
     C                   Z-ADD     RRN2          X                 5 0
     C*
     C     RRN2          CHAIN     SCR02S                             50
     C*
     C     *IN50         IFEQ      '0'
     C                   MOVEL     SCHEAD        DB(X)
     C                   MOVEL     SCTBUP        TB(X)
     C                   MOVEL     SCSCLV        SB(X)
     C                   MOVEL     SCMANE        MB(X)
     C                   MOVEL     SCEDTP        PB(X)
     C                   END
     C*
     C                   END
     C*
     C                   MOVEL     SCCSID        CGCSID
     C                   MOVEL     SCDESC        CGDESC
     C                   Z-ADD     SCRCDS        CGRCDS
     C                   MOVEL     *BLANK        CGSTAT
     C                   MOVEL     Q1PGM         CGPGM
     C                   MOVEL     Q1USER        CGUSER
     C                   MOVEL     Q1USER        CGIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CGDT
     C*
     C                   Z-ADD     DSTIME        CGTIME
     C                   MOVE      SCRFMT        CGRFMT
     C*
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PCG01
     C                   ELSE
     C                   WRITE     PCG01
     C                   END
     C*
     C*
     C                   ENDSR
     C*
** FL = FIELD NAMES
UA01UA02UA03UA04UA05UA06UA07UA08UA09UA10UD01UD02UD03UD04UD05UN01UN02UN03UN04UN05
** AT = FIELD ATTRIBUTES
10   A10   A10   A10   A10   A10   A10   A10   A10   A10   A
 6/0 N 6/0 N 6/0 N 6/0 N 6/0 N 9/2 N 9/2 N 9/2 N 9/2 N 9/2 N
** FB = FIELD NAMES FOR 35A
UB01UB02UB03UB04UB05UB06
** AB = FIELD ATTRIBUTES FOR 35A
35   A35   A35   A35   A35   A35   A
