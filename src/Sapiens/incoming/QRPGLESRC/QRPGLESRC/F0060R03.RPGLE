     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F0060R03 - WCMC GROUP NETWORK INFORMATION  C:62 12/20/97     *
     H*                                                               *
     H*  CALLED FROM: F0060R - Option 8                               *
     H*               F0060R01 - If A/R is not installed              *
     H*               F0060R02                                        *
     H*  CALLS TO:    F0061R   if EDI installed else                  *
     H*               F0120R01                                        *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  03/03/00 C:62 PROJECT: MCMNT                                 *
     H*                ENHANCED THE WORKFLOW LOGIC.                   *
     H*                                                               *
     H*  10/14/99 C:53 PROJECT: MCMNT                                 *
     H*                CORRECT THE MODE PASSED TO F0120R01 BY NOT     *
     H*                FORCING AN ADD WHEN A DISPLAY OPTION WAS TAKEN *
     H*                                                               *
     H*  04/02/99 C:62 PROJECT: MC095 17425                           *
     H*                MAKE SURE ONE OF TWO PROGRAMS ARE CALLED       *
     H*                F0061R IF EDI='Y' ELSE F0120R01.               *
     H*                                                               *
     H*  12/01/98 C:94  PROJECT: MC145                                *
     H*                 ADDED F14=DARTS TO PROGRAM                    *
     H*                                                               *
     H*  10/13/98 C:110 Project: MC145                                *
     H*               - Allow non-entries for Fee Rate, FEIN, and MOD *
     H*                 if Type = 'N'.                                *
     H*                                                               *
     H*  07/14/98 C:61  Project: 98MAY                                *
     H*               - Restrict access to provider types 1 and 4     *
     H*                 only.                                         *
     H*                                                               *
     H*  03/08/98 C:53  Project: MC145
     H*                 SWAP F0031R TO F0032R (NEW VERSION)
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*  POSITION CURSORS IN KJDO:                                    *
     H*                    DOUBLE INDICATORS:                         *
     H*                                ROW  . . . 71 - 80.            *
     H*                                COLUMN . . 30 - 34, 86.        *
     H*                                                               *
     H*  ERROR INDICATORS IN KJDO: (FOR INVALID SEARCH FLD)           *
     H*                    DOUBLE INDICATORS:                         *
     H*                                ROW  . . . 71 - 80.            *
     H*                                COLUMN . . 87                  *
     H*                                                               *
     H*  ERROR INDICATORS IN ERRCK1:                                  *
     H*                    SINGLE INDICATORS: 89.                     *
     H*                    DOUBLE INDICATORS:                         *
     H*                                ROW  . . . 71 - 80.            *
     H*                                COLUMN . . 35 - 39.            *
     H*                                           61 - 67.            *
     H*                                           81, 82, 84 AND 85.  *
     H*                                                               *
     H*****************************************************************
     H*  MODES USED:                                                  *
     H*                                                               *
     H*  'A' = Add mode - workflow only                               *
     H*  'C' = Change mode - workflow only                            *
     H*  'D' = Display mode - workflow only                           *
     H*  'T' = Change mode - when called from F0060R from opt8        *
     H*  'S' = Display mode - when called from F0060R from opt8       *
     H*  'O' = To indicate that this program is called as part of the *
     H*        workflow.                                              *
     H*                                                               *
     H*  XXMODE is used to hold the mode that the program was called  *
     H*  with so that this program will know whether this program was *
     H*  call as part of the workflow or as an option from F0060R.    *
     H*                                                               *
     H*  This program is called if Network is installed.              *
     H*                                                               *
     H*****************************************************************
     FF0060F03  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMFNP     UF   E             DISK
     F                                     RENAME(PFN01:DBRCDF)
     FFMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FFMF1P     UF   E             DISK
     FFMF1L     IF   E           K DISK
     F                                     RENAME(PF101:PF102)
     F                                     INFDS(DBFDS2)
     FFMSML     IF   E           K DISK
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     FNMNWP     IF   E             DISK
     FNMNWL     IF   E           K DISK
     F                                     RENAME(PNW01:PNW02)
      ***** BEG DEL ******* 07/14/98 **********************************
     F*FMPDL   IF  E           K        DISK
      ***** END DEL ******* 07/14/98 **********************************
      ***** BEG ADD ******* 07/14/98 **********************************
     FFMPDL3    IF   E           K DISK
      ***** END ADD ******* 07/14/98 **********************************
     F*** BEGIN ADD *** 04/02/99 **************************************
     FFMCXL     IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F*** END   ADD *** 04/02/99 **************************************
      /COPY SCOPYMT01D
     D*
     D NWN             S             10    DIM(10) CTDATA PERRCD(1)             NWCD FIELD NM
     D NFN             S             10    DIM(10) CTDATA PERRCD(1)             INY FIELD NM
     D N$N             S             10    DIM(10) CTDATA PERRCD(1)             RATE FIELD NM
     D NTN             S             10    DIM(10) CTDATA PERRCD(1)             TYPE FIELD NM
     D NIN             S             10    DIM(10) CTDATA PERRCD(1)             FEIN FIELD NM
     D NMN             S             10    DIM(10) CTDATA PERRCD(1)             MOD FIELD NM
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*** BEGIN ADD *** 11/17/98 **************************************
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D*** END   ADD *** 11/17/98 **************************************
     D*
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  NWP                    1     50
     D                                     DIM(10)                              NWCD FILE VAL
     D  F1NW01                 1      5
     D  F1NW02                 6     10
     D  F1NW03                11     15
     D  F1NW04                16     20
     D  F1NW05                21     25
     D  F1NW06                26     30
     D  F1NW07                31     35
     D  F1NW08                36     40
     D  F1NW09                41     45
     D  F1NW10                46     50
     D                 DS
     D  NWV                    1     50
     D                                     DIM(10)                              NWCD SCRN VAL
     D  SCNW01                 1      5
     D  SCNW02                 6     10
     D  SCNW03                11     15
     D  SCNW04                16     20
     D  SCNW05                21     25
     D  SCNW06                26     30
     D  SCNW07                31     35
     D  SCNW08                36     40
     D  SCNW09                41     45
     D  SCNW10                46     50
     D                 DS
     D  NFP                    1     10
     D                                     DIM(10)                              INY FILE VAL
     D  F1NF01                 1      1
     D  F1NF02                 2      2
     D  F1NF03                 3      3
     D  F1NF04                 4      4
     D  F1NF05                 5      5
     D  F1NF06                 6      6
     D  F1NF07                 7      7
     D  F1NF08                 8      8
     D  F1NF09                 9      9
     D  F1NF10                10     10
     D                 DS
     D  NFV                    1     10
     D                                     DIM(10)                              INY SCRN VAL
     D  SCNF01                 1      1
     D  SCNF02                 2      2
     D  SCNF03                 3      3
     D  SCNF04                 4      4
     D  SCNF05                 5      5
     D  SCNF06                 6      6
     D  SCNF07                 7      7
     D  SCNF08                 8      8
     D  SCNF09                 9      9
     D  SCNF10                10     10
     D                 DS
     D  N$P                    1     90  2
     D                                     DIM(10)                              RATE FILE VAL
     D  XXN$P                  1     90
     D  F1N$01                 1      9  2
     D  F1N$02                10     18  2
     D  F1N$03                19     27  2
     D  F1N$04                28     36  2
     D  F1N$05                37     45  2
     D  F1N$06                46     54  2
     D  F1N$07                55     63  2
     D  F1N$08                64     72  2
     D  F1N$09                73     81  2
     D  F1N$10                82     90  2
     D                 DS
     D  N$V                    1     90  2
     D                                     DIM(10)                              RATE SCRN VAL
     D  SCN$01                 1      9  2
     D  SCN$02                10     18  2
     D  SCN$03                19     27  2
     D  SCN$04                28     36  2
     D  SCN$05                37     45  2
     D  SCN$06                46     54  2
     D  SCN$07                55     63  2
     D  SCN$08                64     72  2
     D  SCN$09                73     81  2
     D  SCN$10                82     90  2
     D                 DS
     D  NTP                    1     10
     D                                     DIM(10)                              TYPE FILE VAL
     D  F1NT01                 1      1
     D  F1NT02                 2      2
     D  F1NT03                 3      3
     D  F1NT04                 4      4
     D  F1NT05                 5      5
     D  F1NT06                 6      6
     D  F1NT07                 7      7
     D  F1NT08                 8      8
     D  F1NT09                 9      9
     D  F1NT10                10     10
     D                 DS
     D  NTV                    1     10
     D                                     DIM(10)                              TYPE SCRN VAL
     D  SCNT01                 1      1
     D  SCNT02                 2      2
     D  SCNT03                 3      3
     D  SCNT04                 4      4
     D  SCNT05                 5      5
     D  SCNT06                 6      6
     D  SCNT07                 7      7
     D  SCNT08                 8      8
     D  SCNT09                 9      9
     D  SCNT10                10     10
     D                 DS
     D  NIP                    1     90  0
     D                                     DIM(10)                              FEIN FILE VAL
     D  XXNIP                  1     90
     D  F1NI01                 1      9  0
     D  F1NI02                10     18  0
     D  F1NI03                19     27  0
     D  F1NI04                28     36  0
     D  F1NI05                37     45  0
     D  F1NI06                46     54  0
     D  F1NI07                55     63  0
     D  F1NI08                64     72  0
     D  F1NI09                73     81  0
     D  F1NI10                82     90  0
     D                 DS
     D  NIV                    1     90  0
     D                                     DIM(10)                              FEIN SCRN VAL
     D  SCNI01                 1      9  0
     D  SCNI02                10     18  0
     D  SCNI03                19     27  0
     D  SCNI04                28     36  0
     D  SCNI05                37     45  0
     D  SCNI06                46     54  0
     D  SCNI07                55     63  0
     D  SCNI08                64     72  0
     D  SCNI09                73     81  0
     D  SCNI10                82     90  0
     D                 DS
     D  NMP                    1     30  0
     D                                     DIM(10)                              MOD FILE VAL
     D  XXNMP                  1     30
     D  F1NM01                 1      3  0
     D  F1NM02                 4      6  0
     D  F1NM03                 7      9  0
     D  F1NM04                10     12  0
     D  F1NM05                13     15  0
     D  F1NM06                16     18  0
     D  F1NM07                19     21  0
     D  F1NM08                22     24  0
     D  F1NM09                25     27  0
     D  F1NM10                28     30  0
     D                 DS
     D  NMV                    1     30  0
     D                                     DIM(10)                              MOD SCRN VAL
     D  SCNM01                 1      3  0
     D  SCNM02                 4      6  0
     D  SCNM03                 7      9  0
     D  SCNM04                10     12  0
     D  SCNM05                13     15  0
     D  SCNM06                16     18  0
     D  SCNM07                19     21  0
     D  SCNM08                22     24  0
     D  SCNM09                25     27  0
     D  SCNM10                28     30  0
     D                 DS
     D  NNV                    1    200
     D                                     DIM(10)                              NAME SCRN VAL
     D  SCNN01                 1     20
     D  SCNN02                21     40
     D  SCNN03                41     60
     D  SCNN04                61     80
     D  SCNN05                81    100
     D  SCNN06               101    120
     D  SCNN07               121    140
     D  SCNN08               141    160
     D  SCNN09               161    180
     D  SCNN10               181    200
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('WCMC Group Network-
     D                                      Information')
     D COMAPI          C                   CONST('FA007R')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*** BEGIN ADD *** 04/02/99 **************************************
     C                   MOVEL     *BLANKS       XXMODE
     C*** END   ADD *** 04/02/99 **************************************
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'S'
     C                   MOVEL     'S'           XXMODE            1
     C                   MOVEL     'D'           @@MODE
     C*
     C     @@MODE        WHENEQ    'T'
     C                   MOVEL     'T'           XXMODE
     C                   MOVEL     'C'           @@MODE
     C*
     C     @@MODE        WHENEQ    'A'
     C                   MOVEL     'A'           XXMODE            1
     C                   MOVEL     'C'           @@MODE
     C*
     C                   OTHER
     C                   MOVEL     'O'           XXMODE
     C                   ENDSL
     C*
     C* SEARCH PARM LIST
     C*
     C     ACDPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
     C*
     C* SEARCH PARM LIST
     C*
     C     ACDPL1        PLIST
     C                   PARM                    ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C*
     C* SEARCH PARM LIST
     C*
     C     ACDPL2        PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C* SEARCH PARM LIST FOR FEIN AND MOD
     C*
     C     ACDPL3        PLIST
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM      *BLANKS       QQRTCD            1
     C                   PARM      *BLANKS       QQTYPE            1
      ***** BEG ADD ******* 07/14/98 **********************************
     C                   PARM      'Y'           @@1N4             1            TYPE 1 & 4 ONLY
      ***** END ADD ******* 07/14/98 **********************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*** BEGIN ADD *** 04/02/99 **************************************
     C*
     C     CXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    FNSUB#
     C                   KFLD                    FNFND
     C*** END   ADD *** 04/02/99 **************************************
     C*
     C     SMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    FNSUB#
     C*
     C* KEY LIST FOR WCMC GROUP MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    FNSUB#
     C                   KFLD                    KKFND
     C*
     C     F1KY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    FNSUB#
     C                   KFLD                    FNFND
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C     NWKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKNWCD
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    FNCO#         ##CO#
     C     *LIKE         DEFINE    FNFND         KKFND
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    NWNWCD        KKNWCD
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C*
     C                   EXSR      NEXTSC
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   MOVEA     '00000'       *IN(35)
     C                   MOVEA     '00000'       *IN(61)
     C                   MOVEA     '00'          *IN(66)
     C*
     C* NETWORK CODE MUST BE VALID IF NOT BLANK.
     C*
     C     1             DO        10            X
     C*
     C     NWV(X)        IFNE      *BLANKS
     C*
     C                   MOVEL     NWV(X)        KKNWCD
     C*
     C     NWKY          CHAIN     PNW02                              99
     C*
     C     70            ADD       X             W
     C*
     C   99              SETON                                        8190
     C   99              MOVEL     *ON           *IN(W)
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* NETWORK FLAG MUST BE BLANK IF NETWORK CODE IS BLANK.
     C*
     C  N901             DO        10            X
     C*
     C     NFV(X)        IFNE      *BLANKS
     C     NWV(X)        ANDEQ     *BLANKS
     C*
     C     70            ADD       X             W
     C*
     C                   SETON                                        3590
     C                   MOVEL     *ON           *IN(W)
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* FEE RATE MUST BE ZERO IF NETWORK CODE IS BLANK.
     C*
     C  N901             DO        10            X
     C*
     C     N$V(X)        IFNE      *ZEROS
     C     NWV(X)        ANDEQ     *BLANKS
     C*
     C     70            ADD       X             W
     C*
     C                   SETON                                        3690
     C                   MOVEL     *ON           *IN(W)
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* FEE RATE MUST BE ZERO IF NETWORK FLAG NE 'N' OR 'Y'
     C*
     C  N901             DO        10            X
     C*
     C     N$V(X)        IFNE      *ZEROS
     C     NFV(X)        ANDNE     'N'
     C     NFV(X)        ANDNE     'Y'
     C*
     C     70            ADD       X             W
     C*
     C                   SETON                                        6190
     C                   MOVEL     *ON           *IN(W)
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* NETWORK TYPE MUST BE BLANK IF NETWORK CODE IS BLANK.
     C*
     C  N901             DO        10            X
     C*
     C     NTV(X)        IFNE      *BLANKS
     C     NWV(X)        ANDEQ     *BLANKS
     C*
     C     70            ADD       X             W
     C*
     C                   SETON                                        3790
     C                   MOVEL     *ON           *IN(W)
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* FEIN FLAG MUST BE BLANK IF NETWORK CODE IS BLANK.
     C*
     C  N901             DO        10            X
     C*
     C     NIV(X)        IFNE      *ZEROS
     C     NWV(X)        ANDEQ     *BLANKS
     C*
     C     70            ADD       X             W
     C*
     C                   SETON                                        3890
     C                   MOVEL     *ON           *IN(W)
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* NETWORK FLAG MUST BE BLANK IF NETWORK CODE IS BLANK.
     C*
     C  N901             DO        10            X
     C*
     C     NMV(X)        IFNE      *ZEROS
     C     NWV(X)        ANDEQ     *BLANKS
     C*
     C     70            ADD       X             W
     C*
     C                   SETON                                        3990
     C                   MOVEL     *ON           *IN(W)
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* NETWORK FLAG MUST BE VALID IF NETWORK CODE ENTERED
     C*
     C  N901             DO        10            X
     C*
     C     NWV(X)        IFNE      *BLANKS
     C*
     C                   MOVEL     'NWNT'        KKTBID
     C                   MOVEL     NFV(X)        KKKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C     70            ADD       X             W
     C*
     C   99              SETON                                        8290
     C   99              MOVEL     *ON           *IN(W)
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* NETWORK TYPE IS REQUIRED IF NETWORK FLAG = 'N' OR 'Y'
     C*
     C  N901             DO        10            X
     C*
     C     NFV(X)        IFEQ      'N'
     C     NFV(X)        OREQ      'Y'
     C     NTV(X)        IFEQ      *BLANKS
     C*
     C     70            ADD       X             W
     C*
     C                   SETON                                        6290
     C                   MOVEL     *ON           *IN(W)
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* IF NETWORK TYPE = 'B' THEN FLAG = 'N' FEE > *ZEROS
     C*
     C  N901             DO        10            X
     C*
     C     NTV(X)        IFEQ      'B'
     C     NFV(X)        IFNE      'N'
     C     N$V(X)        ORLE      *ZEROS
     C*
     C     70            ADD       X             W
     C*
     C                   SETON                                        6390
     C                   MOVEL     *ON           *IN(W)
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* IF NETWORK TYPE = 'N' THEN FLAG = 'N' FEE = *ZEROS
     C*
     C  N901             DO        10            X
     C*
     C     NTV(X)        IFEQ      'N'
     C     NFV(X)        IFNE      'N'
     C     N$V(X)        ORNE      *ZEROS
     C*
     C     70            ADD       X             W
     C*
     C                   SETON                                        6490
     C                   MOVEL     *ON           *IN(W)
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* IF NETWORK TYPE = 'R' THEN FLAG = 'Y' FEE = *ZEROS
     C*
     C  N901             DO        10            X
     C*
     C     NTV(X)        IFEQ      'R'
     C     NFV(X)        IFNE      'Y'
     C     N$V(X)        ORNE      *ZEROS
     C*
     C     70            ADD       X             W
     C*
     C                   SETON                                        6590
     C                   MOVEL     *ON           *IN(W)
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* IF NETWORK TYPE = 'S' THEN FLAG = 'N' FEE > *ZEROS
     C*
     C  N901             DO        10            X
     C*
     C     NTV(X)        IFEQ      'S'
     C     NFV(X)        IFNE      'N'
     C     N$V(X)        ORLE      *ZEROS
     C*
     C     70            ADD       X             W
     C*
     C                   SETON                                        6690
     C                   MOVEL     *ON           *IN(W)
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* NETWORK TYPE MUST BE VALID IF NOT BLANK.
     C*
     C  N901             DO        10            X
     C*
     C     NTV(X)        IFNE      *BLANKS
     C*
     C                   MOVEL     'NWFT'        KKTBID
     C                   MOVEL     NTV(X)        KKKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C     70            ADD       X             W
     C*
     C   99              SETON                                        8490
     C   99              MOVEL     *ON           *IN(W)
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* IF NETWORK FLAG = 'N' OR 'Y' THEN FEIN CAN'T BE ZERO.
     C*
     C  N901             DO        10            X
     C*
     C     NFV(X)        IFEQ      'N'
     C     NFV(X)        OREQ      'Y'
     C*
     C*** BEG ADD *** 10/13/98 ****************************************
     C     NTV(X)        IFNE      'N'
     C*** END ADD *** 10/13/98 ****************************************
     C*
     C     NIV(X)        IFEQ      *ZEROS
     C*
     C     70            ADD       X             W
     C*
     C                   SETON                                        6790
     C                   MOVEL     *ON           *IN(W)
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** BEG ADD *** 10/13/98 ****************************************
     C                   ENDIF
     C*** END ADD *** 10/13/98 ****************************************
     C*
     C                   ENDDO
     C*
     C* FEIN AND MOD MUST BE VALID IF TYPE IS 'N' OR BLANK.
     C*
     C  N901             DO        10            X
     C*
     C     NTV(X)        IFNE      *BLANKS
     C*** BEG ADD *** 10/13/98 ****************************************
     C*
     C     NTV(X)        IFNE      'N'
     C*
     C*** END ADD *** 10/13/98 ****************************************
     C                   MOVE      NIV(X)        KKFEIN
     C                   MOVE      NMV(X)        KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C*
     C     70            ADD       X             W
     C*
     C   99              SETON                                        8590
     C   99              MOVEL     *ON           *IN(W)
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90F1DT          IFGT      SAVEDT
     C     F1DT          OREQ      SAVEDT
     C     F1TIME        ANDGT     SAVETM
     C                   SETON                                        9089
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Searches.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   MOVEA     '00000'       *IN(30)
     C                   MOVEA     '00000'       *IN(35)
     C                   MOVEA     '00000'       *IN(61)
     C                   MOVEA     '00'          *IN(66)
     C*
     C                   Z-ADD     1             X
     C*
     C* NWCD SEARCH
     C*
     C     HFLD          LOOKUP    NWN(X)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   CALL      'N2325R'      ACDPL2
     C*
     C     X             ADD       70            W                 2 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   MOVEL     *ON           *IN(W)
     C                   MOVEL     *ON           *IN30
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PNW01                              99
     C*
     C                   MOVEL     NWNWCD        NWV(X)
     C                   MOVEL     *ON           *IN(W)
     C                   MOVEL     *ON           *IN31
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* I/N/Y SEARCH
     C*
     C     HFLD          LOOKUP    NFN(X)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL     'NWNT'        ##TBID
     C*
     C                   CALL      'F2380R'      ACDPL
     C*
     C     X             ADD       70            W                 2 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   MOVEL     *ON           *IN(W)
     C                   MOVEL     *ON           *IN31
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTKEY         NFV(X)
     C                   MOVEL     *ON           *IN(W)
     C                   MOVEL     *ON           *IN32
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* TYPE SEARCH
     C*
     C     HFLD          LOOKUP    NTN(X)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL     'NWFT'        ##TBID
     C*
     C                   CALL      'F2380R'      ACDPL
     C*
     C     X             ADD       70            W                 2 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   MOVEL     *ON           *IN(W)
     C                   MOVEL     *ON           *IN33
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTKEY         NTV(X)
     C                   MOVEL     *ON           *IN(W)
     C                   MOVEL     *ON           *IN34
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* FEIN/MOD SEARCH
     C*
     C     HFLD          LOOKUP    NIN(X)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      *ZEROS        QQFEIN
     C                   MOVE      *ZEROS        QQMOD
     C*
     C******************** CALL 'F0031R'  ACDPL3           DEL 03/08/98
     C                   CALL      'F0032R'      ACDPL3                         ADD 03/08/98
     C*
     C     X             ADD       70            W                 2 0
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQFEIN        OREQ      *ZEROS
     C*
     C                   MOVEL     *ON           *IN(W)
     C                   MOVEL     *ON           *IN34
     C*
     C                   ELSE
     C*
     C                   ADD       1             W
     C                   MOVE      QQFEIN        NIV(X)
     C                   MOVE      QQMOD         NMV(X)
     C*
     C                   MOVE      QQFEIN        KKFEIN
     C                   MOVE      QQMOD         KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C*
     C                   MOVEL     PDNAME        NNV(X)
     C                   MOVEL     *ON           *IN(W)
     C                   MOVEL     *ON           *IN30
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* FEIN/MOD SEARCH
     C*
     C     HFLD          LOOKUP    NMN(X)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      *ZEROS        QQFEIN
     C                   MOVE      *ZEROS        QQMOD
     C*
     C******************** CALL 'F0031R'  ACDPL3           DEL 03/08/98
     C                   CALL      'F0032R'      ACDPL3                         ADD 03/08/98
     C*
     C     X             ADD       70            W                 2 0
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQFEIN        OREQ      *ZEROS
     C*
     C                   MOVEL     *ON           *IN(W)
     C                   MOVEL     *ON           *IN86
     C*
     C                   ELSE
     C*
     C                   ADD       1             W
     C                   MOVE      QQFEIN        NIV(X)
     C                   MOVE      QQMOD         NMV(X)
     C*
     C                   MOVE      QQFEIN        KKFEIN
     C                   MOVE      QQMOD         KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C*
     C                   MOVEL     PDNAME        NNV(X)
     C                   MOVEL     *ON           *IN(W)
     C                   MOVEL     *ON           *IN30
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* RATE FIELD TEST
     C*
     C     HFLD          LOOKUP    N$N(X)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C     70            ADD       X             W
     C                   MOVEL     *ON           *IN(W)
     C                   MOVEL     *ON           *IN87
     C*
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** BEG ADD *** 12/01/98 ***************************************
     C*================================================================
     C* F14 = DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C                   MOVE      ##CO#         #@CO#
     C                   MOVE      SCSUB#        #@SUB#
     C                   MOVE      SCFND         #@FND
     C*
     C                   CALL      'FA718R'
     C                   PARM      *BLANKS       #@RTCD            1
     C                   PARM      ##WCIN        #@WCIN            1
     C                   PARM      ##WCDK        #@WDCK            1
     C                   PARM                    #@CO#             3
     C                   PARM                    #@SUB#            5
     C                   PARM                    #@FND             3
     C                   PARM      *BLANKS       #@KEY            40
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** END ADD *** 12/01/98 ***************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     XXMODE        IFEQ      'A'
     C*
     C                   MOVEL(P)  MODEA         SCMODE
     C*
     C                   ENDIF
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C                   MOVE      FNSUB#        SCSUB#
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C                   MOVEL     SMNAME        SCSNAM
     C*
     C     F1KY          CHAIN(N)  PF102                              99
     C*
     C                   MOVE      FNFND         SCFND
     C                   MOVEL     FNNAME        SCGNAM
     C                   MOVEL     FNSTAT        SCSTAT
     C*
     C                   MOVEA     NWP           NWV
     C                   MOVEA     NFP           NFV
     C*
     C     XXN$P         IFEQ      *BLANKS
     C*
     C                   MOVEA     *ZEROS        N$V
     C*
     C                   ELSE
     C*
     C                   MOVEA     N$P           N$V
     C*
     C                   ENDIF
     C*
     C                   MOVEA     NTP           NTV
     C*
     C     XXNIP         IFEQ      *BLANKS
     C*
     C                   MOVEA     *ZEROS        NIV
     C*
     C                   ELSE
     C*
     C                   MOVEA     NIP           NIV
     C*
     C                   ENDIF
     C*
     C     XXNMP         IFEQ      *BLANKS
     C*
     C                   MOVEA     *ZEROS        NMV
     C*
     C                   ELSE
     C*
     C                   MOVEA     NMP           NMV
     C*
     C                   ENDIF
     C*
     C     1             DO        10            X                 2 0
     C*
     C     NIV(X)        IFNE      *ZEROS
     C*
     C                   MOVE      NIV(X)        KKFEIN
     C                   MOVE      NMV(X)        KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C*
     C  N99              MOVEL     PDNAME        NNV(X)
     C   99              MOVE      *BLANKS       NNV(X)
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     1             Y                 2 0
     C*
     C     1             DO        10            X
     C*
     C     NWV(X)        IFNE      *BLANKS
     C*
     C                   MOVEL     NWV(X)        NWP(Y)
     C                   MOVEL     NFV(X)        NFP(Y)
     C                   MOVE      N$V(X)        N$P(Y)
     C                   MOVEL     NTV(X)        NTP(Y)
     C                   MOVE      NIV(X)        NIP(Y)
     C                   MOVE      NMV(X)        NMP(Y)
     C                   ADD       1             Y
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C     Y             IFLE      10
     C*
     C                   MOVEA     *BLANKS       NWV(Y)
     C                   MOVEA     *BLANKS       NFV(Y)
     C                   MOVEA     *ZEROS        N$V(Y)
     C                   MOVEA     *BLANKS       NTV(Y)
     C                   MOVEA     *ZEROS        NIV(Y)
     C                   MOVEA     *ZEROS        NMV(Y)
     C                   MOVEA     *BLANKS       NWP(Y)
     C                   MOVEA     *BLANKS       NFP(Y)
     C                   MOVEA     *ZEROS        N$P(Y)
     C                   MOVEA     *BLANKS       NTP(Y)
     C                   MOVEA     *ZEROS        NIP(Y)
     C                   MOVEA     *ZEROS        NMP(Y)
     C                   MOVEA     *BLANKS       NNV(Y)
     C*
     C                   ENDIF
     C*
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 03/03/00 **************************************
     C*================================================================
     C* NEXTSC - PROCESS NEXT SCREEN - A/R INFORMATION
     C*================================================================
     C********** NEXTSC    BEGSR
     C*
     C********** ##EDI     IFEQ 'Y'
     C*
     C********** XXMODE    IFEQ 'O'
     C********** XXMODE    OREQ 'A'
     C**********           MOVE SCFND     KKFND
     C*
     C********** FNKY      CHAINPFN01               N99
     C**********           MOVE DBRRN     ##RRN
     C*
     C********** @@MODE    IFEQ 'D'
     C**********           MOVE '1'       ##CALL
     C*
     C**********           ELSE                            @@MODE NE 'D'
     C*
     C********** XXMODE    IFEQ 'A'
     C**********           MOVE 'A'       ##CALL
     C**********           ELSE                            XXMODE NE 'A'
     C**********           MOVE ' '       ##CALL
     C**********           ENDIF                           XXMODE 'A'
     C**********           ENDIF                           @@MODE 'D'
     C*
     C**********           CALL 'F0061R  'ACDPL1
     C*
     C********** *INKL     IFEQ *ON
     C**********           SETON                     90
     C**********           EXSR GETDTM
     C**********           ENDIF                           *INKL *ON
     C**********           ENDIF                           XXMODE 'O'
     C*
     C*** BEGIN ADD *** 04/02/99 **************************************
     C*
     C**********           ELSE                            ##EDI NE 'Y'
     C*
     C********** XXMODE    IFNE 'S'
     C********** XXMODE    ANDNE'T'
     C********** XXMODE    ANDNE'A'
     C*
     C**********           MOVEL@@MODE    ##MODE           ADD 10/14/99
     C*
     C********** CXKY      CHAINPCX01                99
     C********** *IN99     IFEQ *OFF
     C**********           MOVEL@@MODE    ##MODE           DEL 10/14/99
     C**********           Z-ADDDBRRN3    ##RRN
     C*
     C**********           ELSE                            *IN99 EQ *ON
     C**********           MOVEL'A'       ##MODE           DEL 10/14/99
     C**********           Z-ADD*ZEROS    ##RRN
     C**********           ENDIF                           *IN99 *OFF
     C*
     C**********           CALL 'F0120R01'
     C**********           PARM           ##CALL  1
     C********** *INKC     PARM '0'       ##KC    1
     C********** *INKL     PARM '0'       ##KL    1
     C**********           PARM           ##MODE  1
     C**********           PARM           ##RRN   90
     C**********           PARM FNSUB#    ##SUB#  50
     C**********           PARM FNFND     ##FND   30
     C*
     C********** *INKL     IFEQ *ON
     C**********           SETON                     90
     C**********           EXSR GETDTM
     C**********           ENDIF                           *INKL *ON
     C**********           ENDIF                           XXMODE ' '
     C**********           ENDIF                           ##EDI 'Y'
     C*** END   ADD *** 04/02/99 **************************************
     C*
     C**********           ENDSR
     C*================================================================
     C* NEXTSC - PROCESS NEXT SCREEN - A/R INFORMATION
     C*================================================================
     C     NEXTSC        BEGSR
     C*
     C     XXMODE        IFEQ      'O'
     C     XXMODE        OREQ      'A'
     C*
     C                   MOVE      SCFND         KKFND
     C*
     C     FNKY          CHAIN(N)  PFN01                              99
     C*
     C                   MOVE      DBRRN2        ##RRN
     C*
     C     @@MODE        IFEQ      'D'
     C                   MOVE      'D'           ##MODE
     C                   MOVE      '1'           ##CALL
     C                   ELSE                                                   @@MODE NE 'D'
     C*
     C                   MOVE      ' '           ##CALL
     C*
     C     XXMODE        IFEQ      'A'
     C*
     C                   MOVE      'A'           ##MODE
     C*
     C                   ELSE                                                   XXMODE 'C'
     C*
     C                   MOVE      'C'           ##MODE
     C*
     C                   ENDIF                                                  XXMODE 'A'
     C                   ENDIF                                                  @@MODE 'D'
     C*
     C                   SELECT
     C*
     C     ##EDI         WHENEQ    'Y'
     C                   CALL      'F0061R'      ACDPL1
     C*
     C                   OTHER
     C     CXKY          CHAIN     PCX01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     @@MODE        ##MODE                         DEL 10/14/99
     C                   Z-ADD     DBRRN3        ##RRN
     C*
     C                   ELSE                                                   *IN99 *ON
     C*
     C                   MOVEL     'A'           ##MODE                         DEL 10/14/99
     C                   Z-ADD     *ZEROS        ##RRN
     C*
     C                   ENDIF                                                  *IN99 *OFF
     C*
     C                   CALL      'F0120R01'
     C                   PARM                    ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      DBRRN3        ##RRN             9 0
     C                   PARM      FNSUB#        ##SUB#            5 0
     C                   PARM      FNFND         ##FND             3 0
     C*
     C                   ENDSL
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SETON                                        90
     C                   EXSR      GETDTM
     C*
     C                   ENDIF                                                  *INKL *ON
     C*
     C                   ENDIF                                                  XXMODE 'O'
     C*
     C                   ENDSR
     C*** END   ADD *** 03/03/00 **************************************
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C*** BEG ADD *** 12/01/98 ***************************************
     C     *INKN         CASEQ     *ON           KNDO
     C*** END ADD *** 12/01/98 ***************************************
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C*** GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*
     C                   Z-ADD     1             #OFSCR            1 0
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C                   EXSR      GETDTM
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update detail record if no record writes detail record
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     F1KY          CHAIN(N)  PF102                              99
     C*
     C  N99DBRRN2        CHAIN     PF101                              99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        F1USER
     C                   MOVEL     Q1PGM         F1PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F1DT
     C*
     C                   MOVEL     DSTIME        F1TIME
     C*
     C                   UPDATE    PF101
     C*
     C                   EXSR      NEXTSC
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C*
     C                   ENDSR
** NWN - NWCD FIELD NAME
SCNW01
SCNW02
SCNW03
SCNW04
SCNW05
SCNW06
SCNW07
SCNW08
SCNW09
SCNW10
** NFN - I/N/Y FIELD NAME
SCNF01
SCNF02
SCNF03
SCNF04
SCNF05
SCNF06
SCNF07
SCNF08
SCNF09
SCNF10
** N$N - RATE FIELD NAME
SCN$01
SCN$02
SCN$03
SCN$04
SCN$05
SCN$06
SCN$07
SCN$08
SCN$09
SCN$10
** NTN - TYPE FIELD NAME
SCNT01
SCNT02
SCNT03
SCNT04
SCNT05
SCNT06
SCNT07
SCNT08
SCNT09
SCNT10
** NIN - FEIN FIELD NAME
SCNI01
SCNI02
SCNI03
SCNI04
SCNI05
SCNI06
SCNI07
SCNI08
SCNI09
SCNI10
** NMN - MOD FIELD NAME
SCNM01
SCNM02
SCNM03
SCNM04
SCNM05
SCNM06
SCNM07
SCNM08
SCNM09
SCNM10
