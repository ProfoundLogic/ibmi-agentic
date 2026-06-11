     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H* PROGRAM: F2305RTX - CPT-4 CODE MAINTENANCE: TEXAS
     H*
     H*****************************************************************
     F*  CREATED ON 07/30/2002 BY C:62 FOR PROJECT TX014              *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     F*   08/13/09 C:227 Project: FS349                               *
     F*                  Compilation only.                            *
     F*                                                               *
     H*   11/06/07 C:198 Project: FS308                               *
     H*                  Displayed lowest quartile/51+ miles on the   *
     H*                  CPT-4 Code Maintenance.                      *
     H*                                                               *
     H*  10/11/03 C:79 Project: TX020                                 *
     H*                Add 3RD screen to display mod ind, etc.        *
     H*                Add source field                               *
     H*                                                               *
     H*****************************************************************
     FF2305FTX  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMPRPTX   UF   E             DISK
     F                                     RENAME(PPR01:DBRCDF)
     F*** BEG ADD *** 10/11/03 ****************************************
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     F*** END ADD *** 10/11/03 ****************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('CPT-4 Code Mai-
     D                                     ntenance')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(3)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ST              2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C* PARM LIST FOR CALL TO STATE SPECIFIC PROGRAMS
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM      @@ST          ##ST              2
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*** BEG ADD *** 10/11/03 ****************************************
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*** END ADD *** 10/11/03 ****************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*** BEG ADD *** 10/11/03 ****************************************
     C     *LIKE         DEFINE    GTCO#         KKCO#
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*** END ADD *** 10/11/03 ****************************************
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C     *IN90         IFEQ      *OFF
     C     PRDT          IFGT      SAVEDT
     C     PRDT          OREQ      SAVEDT
     C     PRTIME        ANDGT     SAVETM
     C                   SETON                                        9089
     C                   ENDIF
     C                   ENDIF
     C*
     C* TEXAS DATA SOURCE
     C*
     C                   Z-ADD     ##CO#         KKCO#
     C                   MOVEL     'TXDS'        KKTBID
     C                   MOVEL(P)  SC1A7         KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7190
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C     *IN90         IFEQ      *OFF
     C     PRDT          IFGT      SAVEDT
     C     PRDT          OREQ      SAVEDT
     C     PRTIME        ANDGT     SAVETM
     C                   SETON                                        9089
     C                   ENDIF
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C* PRE, INTRA, AND POST OPERATIVE % MUST TOTAL 100
     C*
     C     SC52N1        IFGT      0
     C     SC52N2        ORGT      0
     C     SC52N3        ORGT      0
     C*
     C     SC52N1        ADD       SC52N2        XXDAYS            3 0
     C                   ADD       SC52N3        XXDAYS
     C*
     C     XXDAYS        IFNE      100
     C                   SETON                                        7190
     C                   GOTO      CK3END
     C                   ENDIF                                                  XX100 NE 0
     C*
     C                   ENDIF                                                  SC5A1 GE
     C*
     C* PC/TC INDICATOR MUST BE 0-9
     C*
     C     SC1AB         IFGE      *BLANK
     C     SC1AB         ANDLE     'Z'
     C                   SETON                                        7290
     C                   GOTO      CK3END
     C                   ENDIF                                                  SC1AB GE ' '
     C*
     C* MULTI-SURG INDICATOR MUST BE 0-3
     C*
     C     SC1AC         IFNE      '0'
     C     SC1AC         ANDNE     '1'
     C     SC1AC         ANDNE     '2'
     C     SC1AC         ANDNE     '3'
     C     SC1AC         ANDNE     '9'
     C                   SETON                                        7390
     C                   GOTO      CK3END
     C                   ENDIF                                                  SC1AC GE ' '
     C*
     C* BILAT-PROC INDICATOR MUST BE 0-3 OR 9
     C*
     C     SC1AD         IFNE      '0'
     C     SC1AD         ANDNE     '1'
     C     SC1AD         ANDNE     '2'
     C     SC1AD         ANDNE     '3'
     C     SC1AD         ANDNE     '9'
     C                   SETON                                        7490
     C                   GOTO      CK3END
     C                   ENDIF                                                  SC1AD GE ' '
     C*
     C* ASST-SURG INDICATOR MUST BE 0-2 OR 9
     C*
     C     SC1A8         IFNE      '0'
     C     SC1A8         ANDNE     '1'
     C     SC1A8         ANDNE     '2'
     C     SC1A8         ANDNE     '9'
     C                   SETON                                        7590
     C                   GOTO      CK3END
     C                   ENDIF                                                  SC1AD GE ' '
     C*
     C* CO-SURG INDICATOR MUST BE 0-2 OR 9
     C*
     C     SC1A9         IFNE      '0'
     C     SC1A9         ANDNE     '1'
     C     SC1A9         ANDNE     '2'
     C     SC1A9         ANDNE     '9'
     C                   SETON                                        7690
     C                   GOTO      CK3END
     C                   ENDIF                                                  SC1A9 GE ' '
     C*
     C* TEAM-SURG INDICATOR MUST BE 0,2 OR 9
     C*
     C     SC1AA         IFNE      '0'
     C     SC1AA         ANDNE     '2'
     C     SC1AA         ANDNE     '9'
     C                   SETON                                        7790
     C                   GOTO      CK3END
     C                   ENDIF                                                  SC1AA GE ' '
     C*
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C     CK3END        ENDSR
     C*================================================================
     C* KJDO - F10 Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SETOFF                                       20
     C*** BEG ADD *** 10/11/03 ****************************************
     C                   SETOFF                                       212223
     C                   SETOFF                                       242526
     C*** END ADD *** 10/11/03 ****************************************
     C*
     C* TEXAS DATA SOURCE
     C*
     C     HFLD          IFEQ      'SC1A7'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      'TXDS'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SC1A7
     C                   MOVEL(P)  GTDESC        SC1A7D
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9020
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*** BEG ADD *** 10/11/03 ****************************************
     C*
     C* PC/TC IND
     C*
     C     HFLD          IFEQ      'SC1AB'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      'MIPC'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SC1AB
     C                   MOVEL(P)  GTDESC        SCPCDS
     C                   SETON                                        9021
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9020
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  HFLD IFEQ SC1AB
     C*
     C* MULT SURG IND
     C*
     C     HFLD          IFEQ      'SC1AC'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      'MI51'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SC1AC
     C                   MOVEL(P)  GTDESC        SCMSDS
     C                   SETON                                        9022
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9021
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  HFLD IFEQ SC1AB
     C*
     C* BILAT SURG IND
     C*
     C     HFLD          IFEQ      'SC1AD'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      'MI50'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SC1AD
     C                   MOVEL(P)  GTDESC        SCBSDS
     C                   SETON                                        9023
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9022
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  HFLD IFEQ SC1AB
     C*
     C* ASST SURG IND
     C*
     C     HFLD          IFEQ      'SC1A8'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      'MI80'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SC1A8
     C                   MOVEL(P)  GTDESC        SCASDS
     C                   SETON                                        9024
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9023
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  HFLD IFEQ SC1AB
     C*
     C* CO SURG IND
     C*
     C     HFLD          IFEQ      'SC1A9'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      'MI62'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SC1A9
     C                   MOVEL(P)  GTDESC        SCCSDS
     C                   SETON                                        9025
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9024
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  HFLD IFEQ SC1AB
     C*
     C* TEAM SURG IND
     C*
     C     HFLD          IFEQ      'SC1AA'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      'MI66'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SC1AA
     C                   MOVEL(P)  GTDESC        SCTSDS
     C                   SETON                                        9026
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9025
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  HFLD IFEQ SC1AB
     C*** END ADD *** 10/11/03 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* LODDES - Load field descriptions
     C*================================================================
     C     LODDES        BEGSR
     C*
     C* PC/TC INDICATOR
     C*
     C                   MOVE      'MIPC'        KKTBID
     C                   MOVEL(P)  SC1AB         KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCPCDS
     C                   ELSE
     C                   CLEAR                   SCPCDS
     C                   ENDIF
     C*
     C* BILAT SURG INDICATOR
     C*
     C                   MOVE      'MI50'        KKTBID
     C                   MOVEL(P)  SC1AD         KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCBSDS
     C                   ELSE
     C                   CLEAR                   SCBSDS
     C                   ENDIF
     C*
     C* MULTI SURG INDICATOR
     C*
     C                   MOVE      'MI51'        KKTBID
     C                   MOVEL(P)  SC1AC         KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCMSDS
     C                   ELSE
     C                   CLEAR                   SCMSDS
     C                   ENDIF
     C*
     C* ASST SURG INDICATOR
     C*
     C                   MOVE      'MI80'        KKTBID
     C                   MOVEL(P)  SC1A8         KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCASDS
     C                   ELSE
     C                   CLEAR                   SCASDS
     C                   ENDIF
     C*
     C* CO SURG INDICATOR
     C*
     C                   MOVE      'MI62'        KKTBID
     C                   MOVEL(P)  SC1A9         KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCSDS
     C                   ELSE
     C                   CLEAR                   SCCSDS
     C                   ENDIF
     C*
     C* TEAM SURG INDICATOR
     C*
     C                   MOVE      'MI66'        KKTBID
     C                   MOVEL(P)  SC1AA         KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCTSDS
     C                   ELSE
     C                   CLEAR                   SCTSDS
     C                   ENDIF
     C*
     C                   MOVE      PR1A7         SC1A7
     C*
     C                   Z-ADD     ##CO#         KKCO#
     C                   MOVEL     'TXDS'        KKTBID
     C                   MOVEL(P)  PR1A7         KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SC1A7D
     C                   ELSE
     C                   MOVEL     *BLANKS       SC1A7D
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*** BEG ADD *** 10/11/03 ****************************************
     C                   SETOFF                                       202122
     C                   SETOFF                                       232425
     C                   SETOFF                                       26
     C*** END ADD *** 10/11/03 ****************************************
     C*
     C                   MOVEL(P)  PRCPT4        SCCPT4
     C*
     C                   Z-ADD     PREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVE      PRMRA         SCMRA
     C                   MOVE      PRANAM        SCANAM
     C                   MOVE      PRPROF        SCPROF
     C*
     C                   MOVE      PR92N1        SC92N1
     C                   MOVE      PR92N6        SC92N6
     C                   MOVE      PR92N7        SC92N7
     C                   MOVE      PR92N8        SC92N8
     C                   MOVE      PR92N9        SC92N9
     C*
     C                   MOVE      PR92NA        SC92NA
     C                   MOVE      PR92NB        SC92NB
     C                   MOVE      PR92NC        SC92NC
     C                   MOVE      PR92ND        SC92ND
     C                   MOVE      PR92NE        SC92NE
     C                   MOVE      PR92NF        SC92NF
     C                   MOVE      PR92NG        SC92NG
     C                   MOVE      PR92NH        SC92NH
     C                   MOVE      PR92NI        SC92NI
     C                   MOVE      PR92NJ        SC92NJ
     C                   MOVE      PR92NK        SC92NK
     C                   MOVE      PR92NL        SC92NL
     C                   MOVE      PR92NM        SC92NM
     C                   MOVE      PR92NN        SC92NN
     C                   MOVE      PR92NO        SC92NO
     C                   MOVE      PR92NP        SC92NP
     C                   MOVE      PR92NQ        SC92NQ
     C                   MOVE      PR92NR        SC92NR
     C                   MOVE      PR92NS        SC92NS
     C                   MOVE      PR92NT        SC92NT
     C                   MOVE      PR92NU        SC92NU
     C                   MOVE      PR92NV        SC92NV
     C                   MOVE      PR92NW        SC92NW
     C                   MOVE      PR92NX        SC92NX
     C                   MOVE      PR92NY        SC92NY
     C                   MOVE      PR92NZ        SC92NZ
     C*
     C                   MOVE      PR92O1        SC92O1
     C                   MOVE      PR92O2        SC92O2
     C                   MOVE      PR92O3        SC92O3
     C                   MOVE      PR92O4        SC92O4
     C                   MOVE      PR92O5        SC92O5
     C                   MOVE      PR92O6        SC92O6
     C                   MOVE      PR92O7        SC92O7
     C                   MOVE      PR92O8        SC92O8
     C                   MOVE      PR92O9        SC92O9
     C*
     C                   MOVE      PR92OA        SC92OA
     C                   MOVE      PR92OB        SC92OB
     C                   MOVE      PR92OC        SC92OC
     C                   MOVE      PR92OD        SC92OD
     C                   MOVE      PR92OE        SC92OE
      *** Beg Add *** 11/06/07 ****************************************
     C                   MOVE      PR92OF        SC92OF
     C                   MOVE      PR92OG        SC92OG
     C                   MOVE      PR92OH        SC92OH
     C                   MOVE      PR92OI        SC92OI
     C                   MOVE      PR92OJ        SC92OJ
     C                   MOVE      PR92OK        SC92OK
     C                   MOVE      PR92OL        SC92OL
     C                   MOVE      PR92OM        SC92OM
      *** End Add *** 11/06/07 ****************************************
     C*
     C                   MOVEL(P)  PRSTAT        SCSTAT
     C*
     C*** BEG ADD *** 10/11/03 ****************************************
     C                   MOVEL(P)  PRDSC1        SCDSC1
     C                   MOVE      PR1A8         SC1A8
     C                   MOVE      PR1A9         SC1A9
     C                   MOVE      PR1AA         SC1AA
     C                   MOVE      PR1AB         SC1AB
     C                   MOVE      PR1AC         SC1AC
     C                   MOVE      PR1AD         SC1AD
     C                   MOVEL     PR5A1         SC5A1
     C                   MOVEL     PR5A2         SC5A2
     C                   Z-ADD     PR52N1        SC52N1
     C                   Z-ADD     PR52N2        SC52N2
     C                   Z-ADD     PR52N3        SC52N3
     C                   EXSR      LODDES
     C*** END ADD *** 10/11/03 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCMRA         PRMRA
     C                   Z-ADD     SCANAM        PRANAM
     C                   Z-ADD     SCPROF        PRPROF
     C*
     C                   Z-ADD     SC92N1        PR92N1
     C                   Z-ADD     SC92N6        PR92N6
     C                   Z-ADD     SC92N7        PR92N7
     C                   Z-ADD     SC92N8        PR92N8
     C                   Z-ADD     SC92N9        PR92N9
     C*
     C                   Z-ADD     SC92NA        PR92NA
     C                   Z-ADD     SC92NB        PR92NB
     C                   Z-ADD     SC92NC        PR92NC
     C                   Z-ADD     SC92ND        PR92ND
     C                   Z-ADD     SC92NE        PR92NE
     C                   Z-ADD     SC92NF        PR92NF
     C                   Z-ADD     SC92NG        PR92NG
     C                   Z-ADD     SC92NH        PR92NH
     C                   Z-ADD     SC92NI        PR92NI
     C                   Z-ADD     SC92NJ        PR92NJ
     C                   Z-ADD     SC92NK        PR92NK
     C                   Z-ADD     SC92NL        PR92NL
     C                   Z-ADD     SC92NM        PR92NM
     C                   Z-ADD     SC92NN        PR92NN
     C                   Z-ADD     SC92NO        PR92NO
     C                   Z-ADD     SC92NP        PR92NP
     C                   Z-ADD     SC92NQ        PR92NQ
     C                   Z-ADD     SC92NR        PR92NR
     C                   Z-ADD     SC92NS        PR92NS
     C                   Z-ADD     SC92NT        PR92NT
     C                   Z-ADD     SC92NU        PR92NU
     C                   Z-ADD     SC92NV        PR92NV
     C                   Z-ADD     SC92NW        PR92NW
     C                   Z-ADD     SC92NX        PR92NX
     C                   Z-ADD     SC92NY        PR92NY
     C                   Z-ADD     SC92NZ        PR92NZ
     C*
     C                   Z-ADD     SC92O1        PR92O1
     C                   Z-ADD     SC92O2        PR92O2
     C                   Z-ADD     SC92O3        PR92O3
     C                   Z-ADD     SC92O4        PR92O4
     C                   Z-ADD     SC92O5        PR92O5
     C                   Z-ADD     SC92O6        PR92O6
     C                   Z-ADD     SC92O7        PR92O7
     C                   Z-ADD     SC92O8        PR92O8
     C                   Z-ADD     SC92O9        PR92O9
     C*
     C                   Z-ADD     SC92OA        PR92OA
     C                   Z-ADD     SC92OB        PR92OB
     C                   Z-ADD     SC92OC        PR92OC
     C                   Z-ADD     SC92OD        PR92OD
     C                   Z-ADD     SC92OE        PR92OE
      *** Beg Add *** 11/06/07 ****************************************
     C                   Z-ADD     SC92OF        PR92OF
     C                   Z-ADD     SC92OG        PR92OG
     C                   Z-ADD     SC92OH        PR92OH
     C                   Z-ADD     SC92OI        PR92OI
     C                   Z-ADD     SC92OJ        PR92OJ
     C                   Z-ADD     SC92OK        PR92OK
     C                   Z-ADD     SC92OL        PR92OL
     C                   Z-ADD     SC92OM        PR92OM
      *** End Add *** 11/06/07 ****************************************
     C*** BEG ADD *** 07/10/03 ****************************************
     C                   MOVE      SC1A7         PR1A7
     C*** END ADD *** 07/10/03 ****************************************
     C*** BEG ADD *** 10/11/03 ****************************************
     C                   MOVE      SC1A8         PR1A8
     C                   MOVE      SC1A9         PR1A9
     C                   MOVE      SC1AA         PR1AA
     C                   MOVE      SC1AB         PR1AB
     C                   MOVE      SC1AC         PR1AC
     C                   MOVE      SC1AD         PR1AD
     C                   MOVEL     SC5A1         PR5A1
     C                   MOVEL     SC5A2         PR5A2
     C                   Z-ADD     SC52N1        PR52N1
     C                   Z-ADD     SC52N2        PR52N2
     C                   Z-ADD     SC52N3        PR52N3
     C*** END ADD *** 10/11/03 ****************************************
     C*
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
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   RETURN
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
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
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        PRUSER
     C                   MOVEL     Q1PGM         PRPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PRDT
     C*
     C                   MOVEL     DSTIME        PRTIME
     C*
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   ENDSR
