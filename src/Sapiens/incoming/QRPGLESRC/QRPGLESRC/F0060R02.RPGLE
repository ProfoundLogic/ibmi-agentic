     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F0060R02 - WCMC GROUP MODULE INFORMATION  C:62 12/17/97      *
     H*                                                               *
     H*  CALLED FROM: F0060R01                                        *
     H*               F0060R - Option 7
     H*  CALLS TO:    F0060R03 if Network installed else              *
     H*               F0061R   if EDI installed else                  *
     H*               F0120R01                                        *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  10/31/00 C:62  PROJECT: AR032                                *
     H*                 SWAP OUT OLD BILL RATE CODE SEARCH WITH NEW.  *
     H*                                                               *
     H*  10/18/00 C:62  PROJECT: MCMNT                                *
     H*                 CHANGED EDIT OF A/R RATE CODE TO MATCH SYSTEM.*
     H*                                                               *
     H*  08/25/00 C:62  PROJECT: MC095 24409                          *
     H*                 COMPILE ONLY                                  *
     H*                                                               *
     H*  03/03/00 C:62 PROJECT: MCMNT                                 *
     H*                ENHANCE THE WORKFLOW LOGIC.                    *
     H*                                                               *
     H*  10/14/99 C:53 PROJECT: MCMNT                                 *
     H*           1-   QUALIFY CALL TO F0120R01 ONLY ON WORKFLOW      *
     H*           2-   CORRECT THE MODE PASSED TO F0120R01 BY NOT     *
     H*                FORCING AN ADD WHEN A DISPLAY OPTION WAS TAKEN *
     H*                                                               *
     H*  04/02/99 C:62 PROJECT: MC095 17425                           *
     H*                MAKE SURE ONE OF THREE PROGRAMS ARE CALLED     *
     H*                F0060R03 IF NTWK='Y' ELSE F0061R IF EDI='Y'    *
     H*                ELSE F0120R01.                                 *
     H*                                                               *
     H*  12/01/98 C:94 PROJECT: MC145                                 *
     H*                ADDED F14=DARTS TO PROGRAM                     *
     H*                                                               *
     H*  11/17/98 C:62 PROJECT:MC145                                  *
     H*                ADDED F0120R01 TO THE WORK FLOW.               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*  POSITION CURSORS IN KJDO:  41 - 60.                          *
     H*                                                               *
     H*  ERROR INDICATORS IN KJDO:  31 AND 32 (FOR INVALID SEARCH FLD)*
     H*                                                               *
     H*  ERROR INDICATORS IN ERRCK1:                                  *
     H*                    SINGLE INDICATORS: 71 AND 89.              *
     H*                    DOUBLE INDICATORS:                         *
     H*                                ROW  . . . 72 - 81.            *
     H*                                COLUMN . . 82 - 87.            *
     H*                                                               *
     H*****************************************************************
     H*  MODES USED:                                                  *
     H*                                                               *
     H*  'A' = Add mode - workflow only                               *
     H*  'C' = Change mode - workflow only                            *
     H*  'D' = Display mode - workflow only                           *
     H*  'T' = Change mode - when called from F0060R from opt7        *
     H*  'S' = Display mode - when called from F0060R from opt7       *
     H*  'O' = To indicate that this program is called as part of the *
     H*        workflow.                                              *
     H*                                                               *
     H*  XXMODE is used to hold the mode that the program was called  *
     H*  with so that this program will know whether this program was *
     H*  call as part of the workflow or as an option from F0060R.    *
     H*                                                               *
     H*  This program is called if A/R is installed.                  *
     H*                                                               *
     H*****************************************************************
     FF0060F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMFNP     UF   E             DISK
     F                                     RENAME(PFN01:DBRCDF)
     FFMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F                                     INFDS(DBFDS)
     FNMBDL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     F*** BEGIN ADD *** 11/17/98 **************************************
     FFMCXL     IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F*** END   ADD *** 11/17/98 **************************************
     F*** BEGIN ADD *** 10/31/00 **************************************
     FNMBDP     IF   E             DISK
     F                                     RENAME(PBD01:PBD02)
     F*** END   ADD *** 10/31/00 **************************************
      /COPY SCOPYMT01D
     D*
     D SYF             S             10    DIM(10) CTDATA PERRCD(1)             SYS FIELD
     D ARF             S             10    DIM(10) CTDATA PERRCD(1)             A/R FIELD
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
     D  SYP                    1     10
     D                                     DIM(10)                              SYS FILE VALUE
     D  FNSY01                 1      1
     D  FNSY02                 2      2
     D  FNSY03                 3      3
     D  FNSY04                 4      4
     D  FNSY05                 5      5
     D  FNSY06                 6      6
     D  FNSY07                 7      7
     D  FNSY08                 8      8
     D  FNSY09                 9      9
     D  FNSY10                10     10
     D                 DS
     D  SYV                    1     10
     D                                     DIM(10)                              SYS VALUE
     D  SCSY01                 1      1
     D  SCSY02                 2      2
     D  SCSY03                 3      3
     D  SCSY04                 4      4
     D  SCSY05                 5      5
     D  SCSY06                 6      6
     D  SCSY07                 7      7
     D  SCSY08                 8      8
     D  SCSY09                 9      9
     D  SCSY10                10     10
     D                 DS
     D  SYD                    1    210
     D                                     DIM(10)                              SYS DESC
     D  SCSD01                 1     21
     D  SCSD02                22     42
     D  SCSD03                43     63
     D  SCSD04                64     84
     D  SCSD05                85    105
     D  SCSD06               106    126
     D  SCSD07               127    147
     D  SCSD08               148    168
     D  SCSD09               169    189
     D  SCSD10               190    210
     D                 DS
     D  ARP                    1     30
     D                                     DIM(10)                              A/R FILE VALUE
     D  FNBR01                 1      3
     D  FNBR02                 4      6
     D  FNBR03                 7      9
     D  FNBR04                10     12
     D  FNBR05                13     15
     D  FNBR06                16     18
     D  FNBR07                19     21
     D  FNBR08                22     24
     D  FNBR09                25     27
     D  FNBR10                28     30
     D                 DS
     D  ARV                    1     30
     D                                     DIM(10)                              A/R VALUE
     D  SCBR01                 1      3
     D  SCBR02                 4      6
     D  SCBR03                 7      9
     D  SCBR04                10     12
     D  SCBR05                13     15
     D  SCBR06                16     18
     D  SCBR07                19     21
     D  SCBR08                22     24
     D  SCBR09                25     27
     D  SCBR10                28     30
     D                 DS
     D  ARD                    1    400
     D                                     DIM(10)                              A/R DESC
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
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('WCMC Group Module-
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
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C*
     C*** BEGIN CHG *** 10/31/00 **************************************
     C********** ACDPL2    PLIST
     C**********           PARM OPTNM     QQPRDS 40
     C**********           PARM '2'       QQCALL  1
     C**********           PARM *BLANKS   QQKEY   3
     C**********           PARM *ZEROS    QQKEY2  80
     C**********           PARM *BLANKS   QQRTCD  1
     C     ACDPL2        PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*** END   CHG *** 10/31/00 **************************************
     C*
     C*** BEGIN ADD *** 04/02/99 **************************************
     C     ACDPL3        PLIST
     C                   PARM                    ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C*** END   ADD *** 04/02/99 **************************************
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
     C*  State code search parm list
     C*
     C     STCPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*** BEGIN ADD *** 11/17/98 **************************************
     C*
     C     CXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    FNSUB#
     C                   KFLD                    FNFND
     C*** END   ADD *** 11/17/98 **************************************
     C*
     C     FBKY          KLIST
     C                   KFLD                    KKFKEY            2
     C*
     C     SMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    FNSUB#
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    FNSUB#
     C                   KFLD                    KKFND
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C     BDKY          KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKEFDT
     C*
     C     BDKY1         KLIST
     C                   KFLD                    KKCODE
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    FNCO#         ##CO#
     C     *LIKE         DEFINE    FNFND         KKFND
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    BDCODE        KKCODE
     C     *LIKE         DEFINE    BDEFDT        KKEFDT
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
     C                   MOVEA     '00'          *IN(31)
     C                   MOVEA     '00000'       *IN(41)
     C                   MOVEA     '00000'       *IN(46)
     C                   MOVEA     '00000'       *IN(51)
     C                   MOVEA     '00000'       *IN(56)
     C*
     C* CHECK TO BE SURE AT LEAST ONE VALUE HAS BEEN ENTERED.
     C*
     C     SYV(1)        IFEQ      *BLANKS
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C* IF SYSTEM CODE ENTERED THEN SYSTEM CODE MUST BE VALID.
     C*
     C  N901             DO        10            X                 2 0
     C*
     C     SYV(X)        IFNE      *BLANKS
     C*
     C                   MOVEL     'SYS'         KKTBID
     C                   MOVEL     SYV(X)        KKKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C     71            ADD       X             Z                 2 0
     C*
     C   99              MOVE      *ON           *IN82
     C   99              MOVE      *ON           *IN(Z)
     C   99              SETON                                          90
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* SYSTEM CODE CAN ONLY BE USED ONCE
     C*
     C                   Z-ADD     10            X
     C*
     C  N90X             DOWGE     1
     C*
     C     SYV(X)        IFNE      *BLANKS
     C*
     C                   Z-ADD     1             Y                 2 0
     C*
     C     SYV(X)        LOOKUP    SYV(Y)                                 99
     C*
     C     X             IFNE      Y
     C     *IN99         ANDEQ     *ON
     C*
     C     X             ADD       71            W
     C                   MOVE      *ON           *IN83
     C                   MOVE      *ON           *IN(W)
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   SUB       1             X
     C*
     C                   ENDDO
     C*
     C* IF SYSTEM CODE ENTERED THEN A/R RATE CODE MUST BE ENTERED.
     C*
     C  N90
     CAN 161             DO        10            X                 2 0
     C*
     C     SYV(X)        IFNE      *BLANKS
     C     ARV(X)        ANDEQ     *BLANKS
     C*
     C     71            ADD       X             Z                 2 0
     C*
     C                   MOVE      *ON           *IN84
     C                   MOVE      *ON           *IN(Z)
     C                   SETON                                          90
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* IF SYSTEM CODE IS BLANK THEN A/R RATE CODE MUST BE BLANK
     C*
     C  N90
     CAN 161             DO        10            X                 2 0
     C*
     C     SYV(X)        IFEQ      *BLANKS
     C     ARV(X)        ANDNE     *BLANKS
     C*
     C     71            ADD       X             Z                 2 0
     C*
     C                   MOVE      *ON           *IN85
     C                   MOVE      *ON           *IN(Z)
     C                   SETON                                          90
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* IF A/R RATE CODE ENTERED THEN A/R RATE CODE MUST BE VALID.
     C*
     C  N90
     CAN 161             DO        10            X                 2 0
     C*
     C     ARV(X)        IFNE      *BLANKS
     C*
     C                   MOVEL     ARV(X)        KKCODE
     C*
     C     BDKY1         CHAIN     PBD01                              99
     C*
     C     71            ADD       X             Z                 2 0
     C*
     C   99              MOVE      *ON           *IN86
     C   99              MOVE      *ON           *IN(Z)
     C   99              SETON                                          90
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* IF A/R RATE CODE ENTERED THEN A/R RATE CODE MUST MATCH SYS CD.
     C*
     C  N90
     CAN 161             DO        10            X                 2 0
     C*
     C     ARV(X)        IFNE      *BLANKS
     C*
     C*** BEGIN CHG *** 10/18/00 **************************************
     C**********           MOVELARV,X     XXSYS   1
     C*
     C                   MOVEL     ARV(X)        KKCODE
     C*
     C     BDKY1         CHAIN     PBD01                              99
     C*
     C                   MOVEL     BDTYPE        XXSYS             1
     C*** END   CHG *** 10/18/00 **************************************
     C*
     C     XXSYS         IFNE      SYV(X)
     C*
     C     71            ADD       X             Z                 2 0
     C*
     C                   MOVE      *ON           *IN87
     C                   MOVE      *ON           *IN(Z)
     C                   SETON                                          90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90FNDT          IFGT      SAVEDT
     C     FNDT          OREQ      SAVEDT
     C     FNTIME        ANDGT     SAVETM
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
     C                   MOVEA     '00'          *IN(31)
     C                   MOVEA     '00000'       *IN(41)
     C                   MOVEA     '00000'       *IN(46)
     C                   MOVEA     '00000'       *IN(51)
     C                   MOVEA     '00000'       *IN(56)
     C*
     C                   Z-ADD     1             X
     C                   MOVEL     'SYS'         ##TBID
     C*
     C     HFLD          LOOKUP    SYF(X)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   CALL      'F2380R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C     40            ADD       X             W                 2 0
     C                   MOVE      *ON           *IN(W)
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTKEY         SYV(X)
     C                   MOVEL     GTDESC        SYD(X)
     C*
     C     50            ADD       X             W
     C                   MOVE      *ON           *IN(W)
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     HFLD          LOOKUP    ARF(X)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C*** BEGIN CHG *** 10/31/00 **************************************
     C**********           CALL 'N1420R'  ACDPL2
     C                   CALL      'N2335R'      ACDPL2
     C*** END   CHG *** 10/31/00 **************************************
     C*
     C     *INKL         IFEQ      *ON
     C*** BEGIN CHG *** 10/31/00 **************************************
     C********** QQKEY     OREQ *BLANKS
     C     ##RRN         OREQ      *ZEROS
     C*** END   CHG *** 10/31/00 **************************************
     C*
     C     50            ADD       X             W                 2 0
     C                   MOVE      *ON           *IN(W)
     C*
     C                   ELSE
     C*
     C*** BEGIN CHG *** 10/31/00 **************************************
     C**********           MOVELQQKEY     ARV,X
     C*
     C**********           MOVELQQKEY     KKCODE
     C**********           Z-ADDQQKEY2    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     KKEFDT
     C*
     C********** BDKY      CHAINPBD01                99
     C*
     C**********           MOVELBDCOM1    ARD,X
     C**********
     C     ##RRN         CHAIN     PBD02                              99
     C*
     C                   MOVEL(P)  BDCODE        ARV(X)
     C                   MOVEL(P)  BDCOM1        ARD(X)
     C*** END   CHG *** 10/31/00 **************************************
     C*
     C     X             IFLT      10
     C*
     C     41            ADD       X             W
     C                   MOVE      *ON           *IN(W)
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SC#DAY'
     C*
     C                   SETON                                        31
     C*
     C     HFLD          WHENEQ    'SC#BLL'
     C*
     C                   SETON                                        32
     C*
     C                   ENDSL
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
     C                   MOVE      FNSUB#        SCSUB#
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C                   MOVEL     SMNAME        SCSNAM
     C*
     C                   MOVEL     FNFND         SCFND
     C                   MOVEL     FNNAME        SCGNAM
     C                   MOVEA     SYP           SYV
     C   16              MOVEA     ARP           ARV
     C*
     C     1             DO        10            X
     C*
     C                   MOVEL     'SYS'         KKTBID
     C                   MOVEL     SYV(X)        KKKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C  N99              MOVEL     GTDESC        SYD(X)
     C   99              MOVEL     *BLANKS       SYD(X)
     C*
     C                   ENDDO
     C*
     C     *IN16         IFEQ      *ON
     C*
     C     1             DO        10            X
     C*
     C                   MOVEL     ARV(X)        KKCODE
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C     BDKY          SETGT     PBD01
     C     BDKY1         READPE    PBD01                                  99
     C*
     C  N99              MOVEL     BDCOM1        ARD(X)
     C   99              MOVE      *BLANKS       ARD(X)
     C*
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   MOVE      FN#DAY        SC#DAY
     C                   MOVE      FN#BLL        SC#BLL
     C                   MOVEL     FNSTAT        SCSTAT
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
     C     SYV(X)        IFNE      *BLANKS
     C*
     C                   MOVEL     SYV(X)        SYP(Y)
     C   16              MOVEL     ARV(X)        ARP(Y)
     C                   ADD       1             Y
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C     Y             IFLE      10
     C*
     C                   MOVEA     *BLANKS       SYD(Y)
     C                   MOVEA     *BLANKS       SYP(Y)
     C                   MOVEA     *BLANKS       ARD(Y)
     C                   MOVEA     *BLANKS       ARP(Y)
     C*
     C                   ENDIF
     C*
     C                   MOVE      SC#DAY        FN#DAY
     C                   MOVE      SC#BLL        FN#BLL
     C*
     C                   ENDSR
     C*** BEGIN CHG *** 03/03/00 **************************************
     C*================================================================
     C* NEXTSC - PROCESS NEXT SCREEN - A/R INFORMATION
     C*================================================================
     C********** NEXTSC    BEGSR
     C*
B01  C********** ##NTWK    IFEQ 'Y'
     C*
B02  C********** XXMODE    IFEQ 'O'
     C********** XXMODE    OREQ 'A'
     C*
     C**********           MOVE SCFND     KKFND
     C*
     C********** FNKY      CHAINPFN01               N99
     C*
     C**********           MOVE DBRRN2    ##RRN
     C*
B03  C********** @@MODE    IFEQ 'D'
     C**********           MOVE 'D'       ##MODE
S03  C**********           ELSE                            @@MODE NE 'D'
     C*
B04  C********** XXMODE    IFEQ 'A'
     C**********           MOVE 'A'       ##MODE
S04  C**********           ELSE                            XXMODE 'C'
     C*
     C**********           MOVE 'C'       ##MODE
E04  C**********           ENDIF                           XXMODE 'A'
E03  C**********           ENDIF                           @@MODE 'D'
     C*
     C**********           CALL 'F0060R03'ACDPL1
     C*
B05  C********** *INKL     IFEQ *ON
     C*
     C**********           SETON                     90
     C**********           EXSR GETDTM
     C*
E05  C**********           ENDIF                           *INKL *ON
     C*
E02  C**********           ENDIF                           XXMODE 'O'
     C*** BEGIN ADD *** 11/17/98 **************************************
     C*
S01  C**********           ELSE                            ##NTWK 'Y'
     C*** BEGIN ADD *** 04/02/99 **************************************
B06  C********** ##EDI     IFNE 'Y'
     C*** END   ADD *** 04/02/99 **************************************
     C*** BEG ADD *** 10/14/99 **************************************
     C********** XXMODE    IFEQ 'T'
     C********** @@MODE    ANDEQ'C'
     C**********           ELSE
     C*** END ADD *** 10/14/99 **************************************
     C*
     C**********           MOVEL@@MODE    ##MODE           ADD 10/14/99
     C*
B07  C********** *INKC     IFEQ *OFF
     C********** *INKL     ANDEQ*OFF
     C********** CXKY      CHAINPCX01                99
     C*
B08  C********** *IN99     IFEQ *OFF
     C**********           MOVEL@@MODE    ##MODE           DEL 10/14/99
     C**********           Z-ADDDBRRN3    ##RRN
S08  C**********           ELSE                            *IN99 *ON
     C*
     C******************   MOVEL'A'       ##MODE           DEL 10/14/99
     C**********           Z-ADD*ZEROS    ##RRN
E08  C**********           ENDIF                           *IN99 *OFF
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
B09  C********** *INKL     IFEQ *ON
     C**********           SETON                     90
     C**********           EXSR GETDTM
E09  C**********           ENDIF                           *INKL *ON
E07  C**********           ENDIF                           *INKC *OFF
     C*** BEG ADD *** 10/14/99 **************************************
     C**********           ENDIF                           ??MODE
     C*** END ADD *** 10/14/99 **************************************
     C*
     C*** END   ADD *** 11/17/98 **************************************
     C*** BEGIN ADD *** 04/02/99 **************************************
     C*
S06  C**********           ELSE                            ##EDI EQ 'Y'
B10  C********** XXMODE    IFEQ 'O'
     C********** XXMODE    OREQ 'A'
     C**********           MOVE SCFND     KKFND
     C*
     C********** FNKY      CHAINPFN01               N99
     C**********           MOVE DBRRN     ##RRN
     C*
B11  C********** @@MODE    IFEQ 'D'
     C**********           MOVE '1'       ##CALL
S11  C**********           ELSE                            @@MODE NE 'D'
     C*
B12  C********** XXMODE    IFEQ 'A'
     C**********           MOVE 'A'       ##CALL
S12  C**********           ELSE                            XXMODE NE 'A'
     C*
     C**********           MOVE ' '       ##CALL
E12  C**********           ENDIF                           XXMODE 'A'
E11  C**********           ENDIF                           @@MODE 'D'
     C*
     C**********           CALL 'F0061R  'ACDPL3
     C*
B13  C********** *INKL     IFEQ *ON
     C**********           SETON                     90
     C**********           EXSR GETDTM
E13  C**********           ENDIF                           *INKL *ON
E10  C**********           ENDIF                           XXMODE 'O'
E06  C**********           ENDIF                           ##EDI 'Y'
     C*** END   ADD *** 04/02/99 **************************************
E01  C**********           ENDIF                           ##NTWK 'Y'
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
     C     ##NTWK        WHENEQ    'Y'
     C                   CALL      'F0060R03'    ACDPL1
     C*
     C     ##EDI         WHENEQ    'Y'
     C                   CALL      'F0061R'      ACDPL3
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
     C*** END   CHG *** 03/03/00 **************************************
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
     C*
     C                   Z-ADD     1             #OFSCR            1 0
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
     C     ##AR          IFEQ      'Y'
     C*
     C                   SETON                                        16
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       16
     C*
     C                   ENDIF
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
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        FNUSER
     C                   MOVEL     Q1PGM         FNPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FNDT
     C*
     C                   MOVEL     DSTIME        FNTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   EXSR      NEXTSC
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C*
     C                   ENDSR
** SYF - SYSTEM VALUE FIELD NAME
SCSY01
SCSY02
SCSY03
SCSY04
SCSY05
SCSY06
SCSY07
SCSY08
SCSY09
SCSY10
** ARF - A/R RATE FIELD NAME
SCBR01
SCBR02
SCBR03
SCBR04
SCBR05
SCBR06
SCBR07
SCBR08
SCBR09
SCBR10
