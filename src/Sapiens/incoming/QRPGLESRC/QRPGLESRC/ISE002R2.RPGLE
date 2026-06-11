     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*  Created by C: 117  Project: 114IBW  04/05/01
     H*****************************************************************
     H*  REVISIONS:
     H*****************************************************************
     FISE002F2  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FIME2P     UF A E             DISK
     F                                     RENAME(PE201:DBRCDF)
     F*  File for Remark Code Description
     FSTCTL1    IF   E           K DISK
     F*  User Parameter File
     FSMUPL     IF   E           K DISK
     F*  Fund Master File
     FWMFNL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Indexing Exit')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(1)

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
     C                   PARM                    ##CONM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  Key list for Remark Code Description
     C*
     C     CTKY          KLIST
     C                   KFLD                    KKRMCD
     C*
     C* key list for user parameter file
     C*
     C     UPKY          KLIST
     C                   KFLD                    KKUSR
     C*
     C*  Key list for Fund Master File
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    E2FND         KKFND
     C     *LIKE         DEFINE    E2RMCD        KKRMCD
     C     *LIKE         DEFINE    UPUSR         KKUSR
     C     *LIKE         DEFINE    UPCO#         KKCO#
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     E2STAT        IFEQ      'I'
     C                   MOVE      'A'           E2STAT
     C                   ELSE
     C                   MOVE      'I'           E2STAT
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        E2USER
     C                   MOVEL     Q1PGM         E2PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E2DT
     C                   MOVEL     DSTIME        E2TIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
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
     C*  SCFND must be validated
     C*
     C                   MOVEL(P)  Q1USER        KKUSR
     C     UPKY          CHAIN     PUP01                              60
     C  N60              MOVEL     UPCO#         KKCO#
     C                   MOVEL     SCFND         KKFND
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C     SCFND         ANDNE     000
     C                   SETON                                        9070
     C                   GOTO      ER1END
     C                   ENDIF
     C*
     C*  SCRMCD must be validated
     C*
     C                   MOVEL     SCRMCD        KKRMCD
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CTDESC        SCDESC
     C                   ELSE
     C                   SETON                                        9072
     C                   GOTO      ER1END
     C                   ENDIF
     C*
     C*  SCAACT can only be 'D', 'R', 'B', or ' '
     C*  If it is 'B', then SCSACT cannot have a value
     C*
     C     SCAACT        IFNE      'D'
     C     SCAACT        ANDNE     'R'
     C     SCAACT        ANDNE     'B'
     C     SCAACT        ANDNE     ' '
     C                   SETON                                        9076
     C                   GOTO      ER1END
     C                   ENDIF
     C*
     C     SCAACT        IFEQ      SCSACT
     C     SCAACT        ANDNE     ' '
     C     SCAACT        ANDNE     'R'
     C                   SETON                                        9073
     C                   GOTO      ER1END
     C                   ENDIF
     C*
     C     SCAACT        IFEQ      'B'
     C     SCSACT        ANDNE     ' '
     C     SCSACT        ANDNE     'R'
     C                   SETON                                        9074
     C                   GOTO      ER1END
     C                   ENDIF
     C*
     C*  SCSACT can only be 'D', 'R', 'B', or ' '
     C*  If it is 'B', then SCAACT cannot have a value
     C*
     C     SCSACT        IFNE      'D'
     C     SCSACT        ANDNE     'R'
     C     SCSACT        ANDNE     'B'
     C     SCSACT        ANDNE     ' '
     C                   SETON                                        9077
     C                   GOTO      ER1END
     C                   ENDIF
     C*
     C     SCSACT        IFEQ      SCAACT
     C     SCSACT        ANDNE     ' '
     C     SCSACT        ANDNE     'R'
     C                   SETON                                        9073
     C                   GOTO      ER1END
     C                   ENDIF
     C*
     C     SCSACT        IFEQ      'B'
     C     SCAACT        ANDNE     ' '
     C     SCAACT        ANDNE     'R'
     C                   SETON                                        9075
     C                   GOTO      ER1END
     C                   ENDIF
     C*
     C     ER1END        ENDSR
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
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C* GROUP ID SEARCH
     C*
     C     HFLD          WHENEQ    'SCFND'
     C                   CALL      'W0010R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '*'           ##CALL            1
     C                   PARM                    @@CO#             3
     C                   PARM      *BLANKS       @@FND             3
     C*
     C     @@FND         IFNE      *BLANKS
     C     @@FND         ANDNE     *ZEROS
     C                   MOVEL     @@FND         SCFND
     C                   SETON                                        31
     C                   ELSE
     C                   SETON                                        30
     C                   ENDIF
     C*
     C* REMARK CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCRMCD'
     C                   MOVE      *BLANKS       QQRTCD            1
     C                   CALL      'SC580R'
     C                   PARM                    SCRMCD
     C                   PARM                    QQRTCD
     C*
     C     QQRTCD        IFEQ      '*'
     C                   SETON                                        32
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQRTCD        OREQ      ' '
     C                   SETON                                        31
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCAACT'
     C                   SETON                                        78
     C*
     C     HFLD          WHENEQ    'SCSACT'
     C                   SETON                                        79
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *ON
     C*
     C* CLEAR FIELDS ON AN ADD
     C*
     C                   CLEAR                   SCFND
     C                   CLEAR                   SCRMCD
     C                   CLEAR                   SCDESC
     C                   CLEAR                   SCAACT
     C                   CLEAR                   SCSACT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     E2FND         SCFND
     C                   MOVEL     E2RMCD        SCRMCD
     C*
     C                   MOVEL     SCRMCD        KKRMCD
     C     CTKY          CHAIN     PCT01                              60
     C  N60              MOVEL     CTDESC        SCDESC
     C*
     C                   MOVEL     E2AACT        SCAACT
     C                   MOVEL     E2SACT        SCSACT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCFND         E2FND
     C                   MOVEL     SCRMCD        E2RMCD
     C                   MOVEL     SCAACT        E2AACT
     C                   MOVEL     SCSACT        E2SACT
     C                   MOVEL     'A'           E2STAT
     C*
     C                   MOVEL     Q1USER        E2USER
     C                   MOVEL     Q1PGM         E2PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E2DT
     C                   MOVEL     DSTIME        E2TIME
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
     C                   SETOFF                                       094041
     C                   SETON                                        LR
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
     C                   MOVEL     Q1USER        E2IUSR
     C                   WRITE     DBRCDF
     C                   FEOD      IME2P
     C                   ENDSR
