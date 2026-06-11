     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7419R02 - Excluded States Information
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: W7419R - Work With Excluded States                 *
     H*                                                               *
     H* CALLS TO:  SY601R - Search Valid State.                       *
     H*            WA005R - Compress Emp/div/name                     *
     H*                                                               *
     H* CREATED:                                                      *
     H*                                                               *
     H* 11/03/00 C:61  PROJECT 95WFF                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 02/10/04 C:65  PROJECT 114WAK                                 *
     H*                ALLOW PROGRAM TO BE CALLED FROM WW/GROUPS TO   *
     H*                PROVIDE FOR DEFAULT STATES ENTRY.              *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* GENERAL                                                       *
     H*  60       GENERAL READ/CHAIN                                  *
     H*                                                               *
     H* SCR01                                                         *
     H*  71       Invalid State.                                      *
     H*  72       Duplicate record not allowed                        *
     H*                                                               *
     H*****************************************************************
      *
      *
     FW7419F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
      *  Physical file definition (for maintaining record)
     FWDXSP     UF A E             DISK
     F                                     RENAME(PXS01:DBRCDF)
     FWDXSL     IF   E           K DISK
     F                                     RENAME(PXS01:PXS02)
      *
      *  Employer Policy Year
     FWDELP     IF   E             DISK
      *
      *  State
     FSTSTL1    IF   E           K DISK
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
      *
      *  Employer Master
     FWMEML     IF   E           K DISK
      *
      *  Group Master
     FWMFNL     IF   E           K DISK
      *
      *
      *
     D***PY SCOPYMT01I
      *
      *================================================================
      *  /COPY required constants
      *================================================================
      /COPY SCOPYMT01D
     D OPTNM           C                   CONST('Excluded States')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
      *
      *
      *
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          WDXSP
     C                   PARM                    @@RRN1            9 0          WDELP
     C*** BEGIN ADD ***  02/10/04  ************************************
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C*** END ADD *****  02/10/04  ************************************
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
      *
      *  Employer Master
      *
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *
      *  Group Master
      *
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
      *
      *  State
      *
     C     STKY          KLIST
     C                   KFLD                    KKKEY
      *
      *  Excluded States
      *
     C     XSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELFYR
     C                   KFLD                    SCST
      *
      *
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    STKEY         KKKEY
     C     *LIKE         DEFINE    ELEMP#        ##EMP#
     C     *LIKE         DEFINE    ELDIV         ##DIV
     C     *LIKE         DEFINE    EMNAME        ##NAME
      *
      *
      *
     C/COPY SCOPYMT01C
      *
      *
      *
      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
      *
      *
      *
      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
      *
      *
      *
      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
      *
      * State code must be valid
      *
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SCST          KKKEY
     C     STKY          CHAIN     PST01                              60
     C  N60              MOVEL(P)  STNAME        SCSTNM
     C   60              MOVEL(P)  *BLANKS       SCSTNM
     C   60              SETON                                        71  90
     C                   ENDIF
      *
      * State code must unique for employer (no duplicate records)
      *
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C     XSKY          CHAIN     PXS02                              60
     C  N60              SETON                                        72  90
     C                   ENDIF
      *
     C                   ENDSR                                                  ERRCK1
      *
      *
      *
      *================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
      *
      *
      *
      *================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
      *
      *
      *
      *================================================================
      * KJDO - Searches
      *================================================================
     C     KJDO          BEGSR
      *
      * State
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCST'
     C                   CALL      'SY601R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      *ZERO         ##RRN             9 0
      *
     C     ##RRN         IFEQ      *ZERO
      *
     C                   ELSE
      *
     C     ##RRN         CHAIN     PST02                              60
     C                   MOVEL(P)  STKEY         SCST
     C                   MOVEL(P)  STNAME        SCSTNM
     C                   ENDIF                                                  ##RRN
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  KJDO
      *
      *
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVE      XSST          SCST
      *
     C                   MOVEL(P)  SCST          KKKEY
     C     STKY          CHAIN     PST01                              60
     C  N60              MOVEL(P)  STNAME        SCSTNM
     C   60              MOVEL(P)  *BLANKS       SCSTNM
      *
     C                   ENDSR                                                  MVDBSC
      *
      *
      *
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
      *
     C                   Z-ADD     ELCO#         XSCO#
     C                   Z-ADD     ELFND         XSFND
     C                   Z-ADD     ELEMP#        XSEMP#
     C                   Z-ADD     ELFYR         XSFYR
      *
     C                   MOVE      SCST          XSST
      *
     C                   ENDSR                                                  MVSCDB
      *
      *
      *
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
      *
      *
      *
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHFKS
      *
      *
      *
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
      *
      *
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
     C     @@MODE        COMP      'A'                                    40    ADD
     C     @@MODE        COMP      'D'                                    09    INQUIRY
      *
      * Get Employer Policy Year record
      *
     C*** BEGIN ADD ***  02/10/04  ************************************
     C                   MOVE      *OFF          *IN41
     C*
     C     @@CO#         IFNE      '000'
     C                   MOVE      *ON           *IN41
     C                   MOVE      @@CO#         ELCO#
     C                   MOVE      @@FND         ELFND
     C                   Z-ADD     0             ELEMP#
     C                   Z-ADD     0             ELDIV
     C                   Z-ADD     0             ELFYR
     C     FNKY          CHAIN     PFN01                              60
     C                   ELSE
     C*** END ADD *****  02/10/04  ************************************
     C     @@RRN1        CHAIN     PEL01                              60
     C*** BEGIN ADD ***  02/10/04  ************************************
     C                   ENDIF                                                  @@CO# IFNE
     C*** END ADD *****  02/10/04  ************************************
     C   60              SETON                                        LR
     C   60              RETURN
      *
      *  Get Information for Header
      *
     C                   Z-ADD     ELFYR         SCFYR
     C                   Z-ADD     ELFND         SCFND
      *
     C     EMKY          CHAIN     PEM01                              60
      *
      *  COMPRESS EMP#/DIV/NAME
      *
     C                   CALL      'WA005R'
     C                   PARM      ELEMP#        ##EMP#
     C                   PARM      ELDIV         ##DIV
     C                   PARM      EMNAME        ##NAME
     C                   PARM                    ##EDN            80
      *
     C                   MOVEL(P)  ##EDN         SCEDN
      *
      * Get Group name
      *
     C                   MOVEL     *BLANKS       SCNAME
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        SCNAME
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
     C                   ENDSR                                                  PREOTH
      *
      *
      *
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        XSUSER
     C                   MOVEL     Q1PGM         XSPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XSDT
     C                   MOVEL     DSTIME        XSTIME
      *
     C                   UPDATE    DBRCDF
     C                   ENDIF
      *
     C                   ENDSR                                                  UPDATR
      *
      *
      *
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
      *
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        XSIUSR
     C                   MOVEL     Q1USER        XSUSER
     C                   MOVEL     Q1PGM         XSIPGM
     C                   MOVEL     Q1PGM         XSPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XSIDT
     C                   MOVEL     SRDTO         XSDT
     C                   MOVEL     DSTIME        XSITIM
     C                   MOVEL     DSTIME        XSTIME
      *
     C                   WRITE     DBRCDF
     C                   FEOD      WDXSP                                        WRITER
      *
     C                   ENDSR
      *
      *
      *
