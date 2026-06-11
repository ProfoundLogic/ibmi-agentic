     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *
      *****************************************************************
      *  F8621R01 - WW/COVENTRY VALIDATION FILES
      *
      *  DESCRIPTION: THIS PROGRAM WILL DISPLAY THE ERRORS OF THE
      *               BILLS THAT WERE SENT TO COVENTRY.
      *
      *  CREATED BY : C:47   6/02/08  PROJECT: 129MPN
      *
      *  CALLED FROM: F8135R
      *  CALLS TO:    NONE
      *
      ****************************************************************
      *  REVISIONS:
      *
      *****************************************************************
     FF8621F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFIY1P     UF A E             DISK
     F                                     RENAME(PY101:DBRCDF)
     FFAEAL1    UF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('COVENTRY VALIDATION-
     D                                      FILES')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D* ---------------------------------------------------------------
     D*
     D* Record 100
     D V100            DS                  INZ
     D  V0001                  1      3  0
     D  V0003                  4      5
     D  V0004                  6     25
     D  V0005                 26     45
     D  V0006                 46     46
     D  V0007                 47     47
     D  V0098                 48     67  0
     D  V0099                 68     87
     D*
     D* Record 101
     D V101            DS                  INZ
     D  V0101                  1      3  0
     D  V0102                  4      5
     D  V0103                  6     25
     D  V0104                 26     45
     D  V0105                 46     65
     D  V0106                 66     85
     D  V0106A                66     74
     D  V0106B                75     77
     D  V0107                 86    105
     D  CSUB                  86     90  0
     D  CFND                  91     93  0
     D  CFYR                  94     96  0
     D  CCASE                 97    103  0
     D  V0108                106    108
     D  V0109                109    113
     D  V0110                114    183
     D  V0111                184    191
     D  V0112                192    195  0
     D  V0113                196    196
     D  V0198                197    216  0
     D  V0199                217    236
     D*
     D* Record 109
     D V109            DS                  INZ
     D  V0901                  1      3  0
     D  V0904                  4      8  0
     D  V0998                  9     28  0
     D  V0999                 29     68

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     EAKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    EARCN
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C* 101-07 (V0107) MIGHT CONTAIN SUB#, FND, FYR, CASE THAT IS
     C*   NEEDED TO UPDATE FAEAP.
     C*
     C     V0107         IFNE      *BLANK
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     'Y'           Y1PRFL
     C*
     C                   UPDATE    DBRCDF
     C                   MOVEL(P)  Y1DATA        V101
     C                   ENDIF                                                  *IN99
     C*
     C* UPDATE FAEAP IF FIY1P WAS UPDATED.
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C* RESET FEAEP:EANSNT SO THE BILL CAN BE PROCESSED.
     C                   Z-ADD     ##CO#         EACO#
     C                   Z-ADD     CSUB          EASUB#
     C                   Z-ADD     CFND          EAFND
     C                   Z-ADD     CFYR          EAFYR
     C                   Z-ADD     CCASE         EACASE
     C                   MOVE      V0106A        EAEOB#
     C                   MOVE      V0106B        EARCN
     C*
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C***                  MOVEL'P'       EANSNT    P
     C                   MOVE      *BLANK        EANSNT
     C*
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   MOVEL     DSTIME        EATIME
     C*
     C                   UPDATE    PEA01
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  CHGSTS
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
     C*
     C                   CLEAR                   V101
     C                   MOVEL(P)  Y1DATA        V101
     C                   MOVEL(P)  V0102         SC02
     C                   MOVEL(P)  V0103         SC03
     C                   MOVEL(P)  V0104         SC04
     C*
     C                   MOVE      V0105         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         SC05
     C*
     C                   MOVEL(P)  V0106A        SC06A
     C                   MOVEL(P)  V0106B        SC06B
     C                   MOVEL(P)  V0107         SC07
     C*
     C* 101-07 (V0107) MIGHT CONTAIN SUB#, FND, FYR, CASE THAT IS
     C*   NEEDED TO UPDATE FAEAP.
     C*
     C     V0107         IFNE      *BLANK
     C                   MOVE      CSUB          SCS
     C                   MOVE      CFND          SCG
     C                   MOVE      CFYR          SCY
     C                   MOVE      CCASE         SCC
     C                   END
     C*
     C                   MOVE      V0108         SC08
     C                   MOVEL(P)  V0109         SC09
     C                   MOVEL(P)  V0110         SC10
     C                   MOVEL(P)  V0111         SC11
     C                   MOVE      V0112         SC12
     C                   MOVEL(P)  V0113         SC13
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
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
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
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
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FIY1P
     C*
     C                   ENDSR
