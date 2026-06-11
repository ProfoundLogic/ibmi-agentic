     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SC311R01 - CUSTOM GROUP ID FIELDS -  UPDATE/DISPLAY
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program updates and displays Custom Group ID fields**
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     SC311R - Custom Group ID fields - OPTION 7              **
     H**   CALLS TO:                                                 **
     H**     S2380R - Generic Table Search                           **
     H**                                                             **
     H**                                                             **
     H**   PROGRAMMER: C:86                                          **
     H**   CREATED:   01/28/98      PROJECT: DR048                   **
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 02/01/01 C:33 PROJECT: DR143
     H*               ADD GROUP TO SCREEN.
     H*
     H* 12/28/99 C:62 PROJECT:PR001
     H*               SWAP OUT WA001R WITH SA005R
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      09   Reserved for maintenance/inquiry conditions
     H**      40   Add mode
     H**   50-57   SCR01 SC40002 Error - Blank Custom ID in Path(CSID)
     H**      60   Used for chaining
     H**      68   Used to signify a blank Custom ID Code
     H**   69-78   SCR01 SC40002 Error - Custom Code not valid (CSID)
     H**      79   SCR01 SC40002 Error - Duplicate record (PATH)
     H**      80   SCR01 SCM0543 Error - Required field (FLAG)
     H**      81   SCR01 SCM0502 Error - Required Y or N (FLAG)
     H**      82   SCR01 SCM0502 Error - Required Y or N (FLAG)
     H**      83   SCR01 SCM0501 Error - Required field (FLAG)
     H**      90   Used as general purpose error indicator
     H**      99   Used when chaining to file being updated
     H**
     H*****************************************************************
     FSC311F01  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FSMCGP     UF A E             DISK
     F                                     RENAME(PCG01:DBRCDF)
     FSTGTP     IF   E             DISK
     FSTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F***  BEGIN ADD  ***  02/01/01  *********************************
     FSSYLF     IF   E           K DISK
     F***  END   ADD  ***  02/01/01  *********************************
      /COPY SCOPYMT01D
     D*
     D FN              S              4    DIM(20) CTDATA PERRCD(20)            FIELD NAMES
     D*
     D*** Arrays for 10A/6.0N/9.2N fields
     D*
     D*
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Custom Group ID Fiel-
     D                                     d Information')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(1)
     D*================================================================
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
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DFDTTM                 1     12  0
     D  DFTIME                 1      6  0
     D  DFDATE                 7     12  0
     D*
     D*** PREFIX AND SUFFIX OF FIELD
     D*
     D                 DS
     D  XXFLD                  1      4
     D  XXFLDP                 1      2
     D  XXFLDS                 3      4

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@FLD             4
     C                   PARM                    @@ATTR            6
     C*
     C*  Work With Generic Table Item parameter list
     C*
     C     ACDPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C*** BEGIN DEL *** 12/28/99 **************************************
     C**********           PARM           ##FND   30
     C*** END   DEL *** 12/28/99 **************************************
     C                   PARM                    ##CONM           40
     C*** BEGIN DEL *** 12/28/99 **************************************
     C**********           PARM           ##FNAM 40
     C*** END   DEL *** 12/28/99 **************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKGTKY
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTKEY         KKGTKY
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     *LIKE         DEFINE    DRFL11        USEGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   DO
     C*
     C* CHECK HEADING AND TABLE BACKUP
     C*
     C  N90SCHEDG        IFNE      *BLANKS
     C     SCTBBU        ANDEQ     *BLANKS
     C                   SETON                                        9080
     C                   LEAVE                                                              s
     C                   ENDIF                                                              NKS
     C*
     C* CHECK TABLE BACKUP
     C*
     C  N90SCHEDG        IFNE      *BLANKS
     C     SCTBBU        ANDNE     'Y'
     C     SCTBBU        ANDNE     'N'
     C                   SETON                                        9081
     C                   LEAVE                                                              s
     C                   ENDIF
     C*
     C* CHECK MANDATORY ENTRY
     C*
     C  N90SCSCLV        IFNE      *ZEROS
     C     SCMANE        ANDNE     'Y'
     C     SCMANE        ANDNE     'N'
     C                   SETON                                        9082
     C                   LEAVE                                                              s
     C                   ENDIF
     C*
     C* CHECK EDIT CODE - FIELD TYPE UDXX
     C*
     C  N90SCEDCD        IFNE      *BLANKS
     C     XXFLDP        ANDEQ     'UD'
     C     SCEDCD        ANDNE     'Y'
     C     SCEDCD        ANDNE     'N'
     C                   SETON                                        9083
     C                   LEAVE                                                              s
     C                   ENDIF
     C*
     C* CHECK EDIT CODE - FIELD TYPE UNXX
     C*
     C  N90SCEDCD        IFNE      *BLANKS
     C     XXFLDP        ANDEQ     'UN'
     C     SCEDCD        ANDNE     '1'
     C     SCEDCD        ANDNE     '2'
     C                   SETON                                        9083
     C                   LEAVE                                                              s
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*** BEG ADD *** 06/25/98 **************************************
     C                   SETON                                        10
     C*** END ADD *** 06/25/98 **************************************
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   MOVEL     CGGRP         SCGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   MOVEL     CGCSID        SCCSID
     C                   MOVEL     CGDESC        SCDESC
     C                   MOVEL     CGRFMT        SCFRMT
     C                   MOVE(P)   CGRCDS        SCSELV
     C                   MOVEL     CGSTAT        SCSTAT
     C                   MOVEL     @@FLD         SCFLD
     C                   MOVEL     SCFLD         XXFLD
     C                   MOVEL     @@ATTR        SCATTR
     C                   MOVE      *ON           *IN70
     C     XXFLDP        IFNE      'UN'
     C     XXFLDP        ANDNE     'UD'
     C                   MOVE      *OFF          *IN70
     C                   ENDIF
     C                   TIME                    DFDTTM
     C                   MOVE      *BLANKS       SCHEDG
     C                   MOVE      *BLANKS       SCTBBU
     C                   MOVE      *BLANKS       SCSCLV
     C                   MOVE      *BLANKS       SCMANE
     C                   MOVE      *BLANKS       SCEDPG
     C                   MOVE      *BLANKS       SCEDCD
     C                   MOVE      *BLANKS       SCEDDS
     C*
     C     *IN40         IFEQ      *OFF
     C     XXFLDP        IFEQ      'UA'
     C     XXFLDP        OREQ      'UD'
     C     XXFLDP        OREQ      'UN'
     C                   EXSR      UXSR
     C                   ELSE
     C     XXFLDP        IFEQ      'UB'
     C                   EXSR      UBSR
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* UXSR - UX SUBROUTINE
     C*================================================================
     C******     Set the INDEX       *****
     C     UXSR          BEGSR
     C                   DO        20            S                 2 0
     C     XXFLD         IFEQ      FN(S)
     C                   LEAVE
     C                   ENDIF                                                  CI,A = HFLD
     C                   ENDDO                                                  DO 10 TIMES
     C                   MOVEL     DS(S)         SCHEDG
     C                   MOVEL     TL(S)         SCTBBU
     C                   MOVE(P)   SL(S)         SCSCLV
     C                   MOVEL     ME(S)         SCMANE
     C                   MOVEL     EP(S)         SCEDPG
     C                   MOVEL     EV(S)         SCEDCD
     C     SCEDCD        IFNE      *BLANKS
     C     XXFLDP        IFEQ      'UD'
     C                   MOVEL(P)  'EDTD'        KKTBID
     C                   ENDIF
     C     XXFLDP        IFEQ      'UN'
     C                   MOVEL(P)  'EDTN'        KKTBID
     C                   ENDIF
     C                   MOVEL(P)  EV(S)         KKGTKY
     C     GTKY          CHAIN     PGT02                              60
     C                   MOVEL(P)  GTDESC        SCEDDS
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* UBSR - UB SUBROUTINE
     C*================================================================
     C     UBSR          BEGSR
     C                   MOVE      XXFLDS        T                 1 0
     C                   MOVEL     DB(T)         SCHEDG
     C                   MOVEL     TB(T)         SCTBBU
     C                   MOVE(P)   SB(T)         SCSCLV
     C                   MOVEL     MB(T)         SCMANE
     C                   MOVEL     PB(T)         SCEDPG
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   MOVE      SCGRP         CGGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C     XXFLDP        IFEQ      'UA'
     C     XXFLDP        OREQ      'UD'
     C     XXFLDP        OREQ      'UN'
     C                   EXSR      UXDB
     C                   MOVE      S             SFRRN1            4 0
     C                   ELSE
     C     XXFLDP        IFEQ      'UB'
     C                   EXSR      UBDB
     C                   MOVE      T             SFRRN1
     C                   ENDIF
     C                   ENDIF
     C                   EXSR      MVUPDB
     C                   ENDSR
     C*================================================================
     C* UXDB - UX FIELDS TO DATABASE
     C*================================================================
     C     UXDB          BEGSR
     C                   MOVEL     SCHEDG        DS(S)
     C                   MOVEL     SCTBBU        TL(S)
     C                   MOVEL     SCSCLV        SL(S)
     C                   MOVEL     SCMANE        ME(S)
     C                   MOVEL     SCEDCD        EV(S)
     C                   MOVEL     SCEDPG        EP(S)
     C                   ENDSR
     C*================================================================
     C* UBDB - UB FIELDS TO DATABASE
     C*================================================================
     C     UBDB          BEGSR
     C                   MOVEL     SCHEDG        DB(T)
     C                   MOVEL     SCTBBU        TB(T)
     C                   MOVEL     SCSCLV        SB(T)
     C                   MOVEL     SCMANE        MB(T)
     C                   MOVEL     SCEDPG        PB(T)
     C                   ENDSR
     C*================================================================
     C* MVUPDB - Move update fields to data base fields
     C*================================================================
     C     MVUPDB        BEGSR
     C                   MOVEL     Q1USER        CGUSER
     C                   MOVEL     Q1PGM         CGPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CGDT
     C                   MOVEL     DSTIME        CGTIME
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*          Process Function 10: Search FORMAT
     C*----------------------------------------------------------------
     C     *INKJ         IFEQ      *ON
     C*
     C*** BEG ADD *** 06/25/98 ************************************
     C                   SETOFF                                       111213
     C                   SETOFF                                       141516
     C     HFLD          IFEQ      'SCHEDG'
     C                   SETON                                        1690
     C                   ENDIF
     C     HFLD          IFEQ      'SCTBBU'
     C                   SETON                                        1290
     C***                  SETON                     12
     C                   ENDIF
     C     HFLD          IFEQ      'SCMANE'
     C                   SETON                                        1390
     C***                  SETON                     13
     C                   ENDIF
     C     HFLD          IFEQ      'SCSCLV'
     C                   SETON                                        1490
     C***                  SETON                     14
     C                   ENDIF
     C     HFLD          IFEQ      'SCEDPG'
     C                   SETON                                        1590
     C***                  SETON                     15
     C                   ENDIF
     C*** END ADD *** 06/25/98 ************************************
     C                   MOVE      *ZEROS        ##RRN
     C     HFLD          IFEQ      'SCEDCD'
     C     XXFLDP        ANDEQ     'UD'
     C*
     C                   MOVEL(P)  'EDTD'        ##TBID
     C                   CALL      'S2380R'      ACDPL
     C     ##RRN         CHAIN     PGT01                              60
     C  N60              MOVEL     GTKEY         SCEDCD
     C  N60              MOVEL(P)  GTDESC        SCEDDS
     C                   ELSE
     C*
     C     HFLD          IFEQ      'SCEDCD'
     C     XXFLDP        ANDEQ     'UN'
     C*
     C                   MOVEL(P)  'EDTN'        ##TBID
     C                   CALL      'S2380R'      ACDPL
     C     ##RRN         CHAIN     PGT01                              60
     C  N60              MOVEL     GTKEY         SCEDCD
     C  N60              MOVEL(P)  GTDESC        SCEDDS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  INKJ = On
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09
     C     @@MODE        COMP      '2'                                    09
     C     @@MODE        COMP      '3'                                    09
     C     @@MODE        COMP      'D'                                    09
     C     @@MODE        COMP      'A'                                    40
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   CLEAR                   DRFL11
     C     'SC'          CHAIN     SDR01                              60
     C     DRFL11        COMP      'Y'                                    41
     C                   MOVEL     DRFL11        USEGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        CGIUSR
     C                   MOVEL     Q1PGM         CGIPGM
     C                   MOVEL     DFTIME        CGITIM
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CGIDT
     C                   WRITE     DBRCDF
     C                   FEOD      SMCGP
     C                   ENDSR
** FN = FIELD NAMES
UA01UA02UA03UA04UA05UA06UA07UA08UA09UA10UD01UD02UD03UD04UD05UN01UN02UN03UN04UN05
