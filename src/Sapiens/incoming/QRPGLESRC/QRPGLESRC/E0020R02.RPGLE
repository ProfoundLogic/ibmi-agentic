     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE E0020R02 - Employer Update/Display
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: E0020R                                             *
     H*                                                               *
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/11/13 C:279 Redmine: 1274                                  *
     H*                Corrected checkbox processing for Calculate    *
     H*                exp mod field.                                 *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  71       Invalid Group.                                      *
     H*  72       Employer # can not be blank.                        *
     H*  73       Employer name can not be blank.                     *
     H*  74       State must be valid.                                *
     H*  75       Effective date must be valid.                       *
     H*  76       Status code must be 'A' OR 'I'.                     *
     H*  77       Calculate exp mod must be 'Y' OR 'N'.               *
     H*  78       Position cursor to emp#.
     H*                                                               *
     H*****************************************************************
      *
     FE0020F02  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FEMEMP     UF A E             DISK
     F                                     RENAME(PEM01:DBRCDF)
     F*
     F*  Logical file for validating group and group name
     FEMFNL     IF   E           K DISK
     F*
     F*  Logical file for validating state
     FSTSTL3    IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Employers')
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
      *
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
      *
     C     EMKEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
      *
     C     STKY          KLIST
     C                   KFLD                    KKKEY
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
      *
     C     *LIKE         DEFINE    EMFND         KKFND
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    STKEY         KKKEY
      *
     C/COPY SCOPYMT01C
      *
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C     CHGSTS        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C     EMSTAT        IFEQ      'I'
     C                   MOVE      'A'           EMSTAT
     C                   ELSE
     C                   MOVE      'I'           EMSTAT
     C                   ENDIF                                                  EMSTAT
      *
     C                   MOVEL     Q1USER        EMUSER
     C                   MOVEL     Q1PGM         EMPGM
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
      *
     C                   MOVEL     DSTIME        EMTIME
      *
     C                   UPDATE    DBRCDF
      *
     C                   ENDIF                                                  *IN99
      *
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
     C* Validate fund number
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL     SCFND         KKFND
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        71  90
     C                   ENDIF
     C                   ENDIF
     C*
     C* Employer number cannot be equal to blanks.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCEMP#        ANDEQ     *ZEROS
     C                   SETON                                        72  90
     C                   ENDIF
     C*
     C* Employer name cannot be equal to blanks.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCENAM        ANDEQ     *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF
     C*
     C* Validate state
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SCMST         KKKEY
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        74  90
     C                   ENDIF
     C*
     C* Validate effective date
     C*
     C******** beg del ******** 11/11/13 **********************************************
     C******IN90         IFEQ      *OFF
     C******** end del ******** 11/11/13 **********************************************
     C                   Z-ADD     SCEFD1        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFD1
     C     SRERR         IFEQ      '1'
     C                   SETON                                        75  90
     C                   ENDIF                                                  SRERR
     C******** beg del ******** 11/11/13 **********************************************
     C********           ENDIF
     C******** end del ******** 11/11/13 **********************************************
     C*
     C* Employer Status must be "A" of "I".
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSTAT        IFNE      'A'
     C     SCSTAT        ANDNE     'I'
     C                   SETON                                        76  90
     C                   ENDIF
     C                   ENDIF
      ********************** Beg Add ******************* 11/11/13 *********************
      /Free
       If scMod = ' ';
          scMod = 'N';
       Endif;
      /End-free
      ********************** End Add ******************* 11/11/13 *********************
     C*
     C* Calculate Exp Mod? must be "Y" or "N".
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMOD         IFNE      'Y'
     C     SCMOD         ANDNE     'N'
     C                   SETON                                        77  90
     C                   ENDIF
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
      *===============================================================
      * KJDO - F10 SEARCHES
      *===============================================================
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCFND'
      *
     C                   CALL      'E0015R'
     C                   PARM      '2'           #1CALL            1
     C                   PARM      *BLANK        #1KC              1
     C                   PARM      *BLANK        #1KL              1
     C                   PARM      *BLANKS       #1FND             3
      *
     C     #1FND         IFNE      *BLANKS
     C                   MOVE(P)   #1FND         SCFND
     C                   Z-ADD     SCFND         KKFND
     C                   SETON                                        78
      *
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCFNNM
     C   60              MOVEL(P)  *BLANKS       SCFNNM
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  KJDO
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     EMFND         SCFND
     C*
     C                   MOVEL     EMFND         KKFND
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCFNNM
     C   60              MOVEL(P)  *BLANKS       SCFNNM
     C*
     C                   MOVEL(P)  EMEMP#        SCEMP#
     C                   MOVEL(P)  EMNAME        SCENAM
     C                   MOVEL(P)  EMMAD1        SCMAD1
     C                   MOVEL(P)  EMMCTY        SCMCTY
     C                   MOVEL     EMMST         SCMST
     C                   MOVEL     EMMZIP        SCMZP1
     C                   MOVE      EMMZIP        SCMZP2
     C                   MOVEL(P)  EMSEMP        SCSEMP
     C*
     C                   Z-ADD     EMEFD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFD1
     C*
     C                   MOVEL     EMCNTY        SCCNTY
     C                   MOVEL(P)  EMSTAT        SCSTAT
     C                   MOVEL(P)  EMMOD         SCMOD
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         EMCO#
     C                   Z-ADD     SCFND         EMFND
     C                   Z-ADD     SCEMP#        EMEMP#
     C                   MOVEL     SCENAM        EMNAME
     C*
     C                   Z-ADD     SCEFD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMEFD1
     C*
     C                   MOVEL     SCMAD1        EMMAD1
     C                   MOVEL     SCMCTY        EMMCTY
     C                   MOVEL     SCMST         EMMST
     C                   MOVEL     SCMZP1        EMMZIP
     C                   MOVE      SCMZP2        EMMZIP
     C                   MOVEL(P)  SCSEMP        EMSEMP
     C                   MOVE      SCCNTY        EMCNTY
     C                   MOVE      SCMOD         EMMOD
     C                   MOVE      SCSTAT        EMSTAT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
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
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C     @@MODE        COMP      'A'                                    40
      *
     C     *IN40         IFEQ      *OFF
     C     @@RRN         CHAIN     EMEMP                              60
     C                   Z-ADD     EMFND         KKFND
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL(P)  FNNAME        SCFNNM
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        EMUSER
     C                   MOVEL     Q1PGM         EMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C                   MOVEL     DSTIME        EMTIME
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        EMIUSR
     C                   MOVEL     Q1USER        EMUSER
     C                   MOVEL     Q1PGM         EMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C                   MOVEL     SRDTO         EMIDT
     C                   MOVEL     DSTIME        EMTIME
     C                   WRITE     DBRCDF
     C                   FEOD      EMEMP
     C                   ENDSR
