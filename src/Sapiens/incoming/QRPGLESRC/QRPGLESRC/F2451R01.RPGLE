     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) ACTGRP(*CALLER)
     H/define CONVERT
     H*
     H*****************************************************************
     H*
     H* Group Outcome/Savings Code Information
     H*
     H*****************************************************************
     H* Created on 12/31/2002 by c:62 for Project MC254
     H*****************************************************************
     H* Revisions:
     H*
     H*****************************************************************
     FF2451F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFTGOP     UF A E             DISK
     F                                     RENAME(PGO01:DBRCDF)
     FFTGOL     IF   E           K DISK
     FFTOTP     IF   E             DISK
     FFTOTL     IF   E           K DISK
     F                                     RENAME(POT01:POT02)
     FFMFNP     IF   E             DISK
     FFMSML     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Group Outcome/Saving-
     D                                     s Codes')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     GOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCODE
     C*
     C     OTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCODE
     C*
     C     SMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    GOCO#         KKCO#
     C     *LIKE         DEFINE    GOSUB#        KKSUB#
     C     *LIKE         DEFINE    GOFND         KKFND
     C     *LIKE         DEFINE    GOCODE        KKCODE
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
     C     *IN40         IFEQ      *ON
     C*
     C* CODE MUST BE VALID
     C*
     C                   MOVEL(P)  SCCODE        KKCODE
     C     OTKY          CHAIN     POT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7190
     C                   ELSE
     C                   MOVEL(P)  OTDESC        SCDESC
     C                   ENDIF
     C*
     C* CODE MUST BE UNIQUE
     C*
     C     GOKY          CHAIN     PGO01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDIF
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
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   CALL      'F2450R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      ' '           ##KC              1
     C     *INKL         PARM      ' '           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0          FTOTP
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     POT01                              99
     C                   MOVEL(P)  OTCODE        SCCODE
     C                   MOVEL(P)  OTDESC        SCDESC
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KNDO - F14=DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C                   MOVE      KKCO#         $$CO#
     C                   MOVE      KKSUB#        $$SUB#
     C                   MOVE      KKFND         $$FND
     C*
     C                   CALL      'FA718R'
     C                   PARM      *BLANKS       $$RTCD            1
     C                   PARM      ##WCIN        $$WCIN            1
     C                   PARM      ##WCDK        $$WDCK            1
     C                   PARM                    $$CO#             3
     C                   PARM                    $$SUB#            5
     C                   PARM                    $$FND             3
     C                   PARM      *BLANKS       $$KEY            40
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C                   MOVEL(P)  GOCODE        SCCODE
     C                   MOVEL(P)  GOCODE        KKCODE
     C     OTKY          CHAIN     POT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  OTDESC        SCDESC
     C                   ELSE
     C                   MOVEL     *BLANKS       SCDESC
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       SCCODE
     C                   MOVEL     *BLANKS       SCDESC
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     KKCO#         GOCO#
     C                   Z-ADD     KKSUB#        GOSUB#
     C                   Z-ADD     KKFND         GOFND
     C                   MOVEL(P)  SCCODE        GOCODE
     C                   MOVEL(P)  'A'           GOSTAT
     C                   MOVEL(P)  Q1USER        GOUSER
     C                   MOVEL(P)  Q1PGM         GOPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        GOTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GODT
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
     C     *INKN         CASEQ     *ON           KNDO
     C                   ENDCS
     C*
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
     C     @@MODE        COMP      'A'                                    40
     C*
     C*** GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANKS       ##WCIF            1            WC INST
     C                   PARM      *BLANKS       ##MCIF            1            MC INST
     C                   PARM      *BLANKS       ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANKS       ##AR              1            MC AR INST
     C                   PARM      *BLANKS       ##NTWK            1            NETWORK INST
     C                   PARM      *BLANKS       ##UR              1            PRECERT INST
     C                   PARM      *BLANKS       ##MED             1            MEDMGMT INST
     C                   PARM      *BLANKS       ##EDI             1            EDI INST
     C                   PARM      *BLANKS       ##WCDK            1            WC DARTS KEY
     C*
     C     @@RRN2        CHAIN     PFN01                              99
     C*
     C                   Z-ADD     FNCO#         KKCO#
     C                   Z-ADD     FNSUB#        KKSUB#
     C                   Z-ADD     FNFND         KKFND
     C                   Z-ADD     FNSUB#        SCSUB#
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL(P)  FNNAME        SCGNAM
     C*
     C     SMKY          CHAIN     PSM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SMNAME        SCSNAM
     C                   ELSE
     C                   MOVEL     *BLANKS       SCSNAM
     C                   ENDIF
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
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL(P)  GOUSER        GOIUSR
     C                   MOVEL(P)  GOPGM         GOIPGM
     C                   Z-ADD     GOTIME        GOITIM
     C                   Z-ADD     GODT          GOIDT
     C                   WRITE     DBRCDF
     C                   FEOD      FTGOP
     C*
     C                   ENDSR
