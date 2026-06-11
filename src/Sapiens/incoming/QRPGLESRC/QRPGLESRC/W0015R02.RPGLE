     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 04/07/98  C:91  PROJECT: WC577                                *
     H*                 DISPLAY ERROR IF F10 PRESSED ON NON           *
     H*                 SEARCHABLE FIELD                              *
     H*                                                               *
     H*****************************************************************
     FW0015F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWDRCP     UF A E             DISK
     F                                     RENAME(PRC01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMFNP     IF   E             DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRC01)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Reserve Cushion Mast-
     D                                     er Information')
     D COMAPI          C                   CONST('WA001R')
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
     C*  Company file/group retrieve program parameter list
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
     C     RCKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    KKRVCD
     C                   KFLD                    KKEFDT
     C*
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    RTKEY         KKKEY
     C     *LIKE         DEFINE    RCRVCD        KKRVCD
     C     *LIKE         DEFINE    RCRVCD        @@RVCD
     C     *LIKE         DEFINE    RCEFDT        KKEFDT
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     RCSTAT        IFEQ      'I'
     C                   MOVE      'A'           RCSTAT
     C                   ELSE
     C                   MOVE      'I'           RCSTAT
     C                   ENDIF                                                  CPSTAT
     C*
     C                   MOVEL     Q1USER        RCUSER
     C                   MOVEL     Q1PGM         RCPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RCDT
     C*
     C                   MOVEL     DSTIME        RCTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN60
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
     C*
     C*  RESEVE CODE MUST BE VALID
     C*
     C                   MOVEL     SCRVCD        KKKEY
     C*
     C   40
     CANN90TBKY          CHAIN     PRT01                              99
     C*
     C   99              SETON                                        9071
     C*
     C*  EFFECTIVE DATE MUST BE VALID
     C*
     C     *IN40         IFEQ      *ON
     C     *IN90         ANDEQ     *OFF
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        9072
     C*
     C                   ENDIF                                                  SRERR EQ '1'
     C*
     C                   ENDIF                                                  *IN40,90
     C*
     C*  MEDICAL ONLY AMOUNT CANNOT BE LESS THAN ZERO
     C*
     C  N90SCMAMT        IFLT      *ZEROS
     C*
     C                   SETON                                        9073
     C*
     C                   END
     C*
     C*  LOST TIME AMOUNT CANNOT BE LESS THAN ZERO
     C*
     C  N90SCLAMT        IFLT      *ZEROS
     C*
     C                   SETON                                        9074
     C*
     C                   END
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90RCDT          IFGT      SAVEDT
     C     RCDT          OREQ      SAVEDT
     C     RCTIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK1
     C*
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
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C   40              MOVEL     'A'           RCSTAT
     C*
     C                   MOVEL     RCRVCD        SCRVCD
     C*
     C                   MOVEL     RCRVCD        KKKEY
     C*
     C     TBKY          CHAIN     PRT01                              99
     C*
     C  N99              MOVEL     RTDESC        SCDESC
     C   99              MOVE      *BLANKS       SCDESC
     C*
     C                   Z-ADD     RCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   Z-ADD     RCMAMT        SCMAMT
     C                   Z-ADD     RCLAMT        SCLAMT
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     FNCO#         RCCO#
     C                   Z-ADD     SCFND         RCFND
     C                   MOVEL     SCRVCD        RCRVCD
     C                   Z-ADD     SCMAMT        RCMAMT
     C                   Z-ADD     SCLAMT        RCLAMT
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RCEFDT
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
     C     *INKJ         IFEQ      *ON
     C*
      **** BEGIN ADD **** 04/07/98 ************************************
     C     HFLD          IFEQ      'SCRVCD'
      **** END ADD **** 04/07/98 **************************************
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@RVCD
     C*
     C     *INKC         IFEQ      *OFF
     C*
     C     *INKL         IFEQ      *ON
     C     @@RVCD        OREQ      *BLANKS
     C*
     C                   SETON                                        75
     C*
     C                   ELSE
     C*
     C                   SETON                                        76
     C                   MOVEL     @@RVCD        SCRVCD
     C*
     C                   MOVEL     SCRVCD        KKKEY
     C*
     C     TBKY          CHAIN     PRT01                              99
     C*
     C  N99              MOVEL     RTDESC        SCDESC
     C   99              MOVE      *BLANKS       SCDESC
     C*
     C                   ENDIF                                                  *INKL
     C*
     C                   ENDIF                                                  *INKC
      **** BEGIN ADD **** 04/07/98 ************************************
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN77
     C                   ENDIF                                                  HFLD IFEQ 'SCRVCD'
      **** END ADD **** 04/07/98 **************************************
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  *INKJ
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
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C* GET GROUP INFORMATION
     C*
     C     @@RRN2        CHAIN     WMFNP                              60
     C*
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   ENDSR                                                  PGMSTR
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
     C*
     C                   MOVEL     Q1USER        RCUSER
     C                   MOVEL     Q1PGM         RCPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RCDT
     C*
     C                   MOVEL     DSTIME        RCTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF                                                  *IN99
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        RCUSER
     C                   MOVEL     Q1PGM         RCPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RCDT
     C*
     C                   MOVEL     DSTIME        RCTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WDRCP
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  WRITER
