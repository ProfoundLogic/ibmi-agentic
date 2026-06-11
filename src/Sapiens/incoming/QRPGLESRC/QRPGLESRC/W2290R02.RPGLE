     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*****************************************************************
     H*
     FW2290F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWTACP     UF A E             DISK
     F                                     RENAME(PAC01:DBRCDF)
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Audit Company Inform-
     D                                     ation')
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
     C*  SYSTEM TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY            10
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
      *
     C     CHGSTS        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     *IN99         IFEQ      *OFF
      *
     C     ACSTAT        IFEQ      'I'
     C                   MOVE      'A'           ACSTAT
     C                   ELSE
     C                   MOVE      'I'           ACSTAT
     C                   ENDIF                                                  ACSTAT IFEQ 'I'
      *
     C                   MOVEL     Q1USER        ACUSER
     C                   MOVEL     Q1PGM         ACPGM
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C                   MOVEL     SRDTO         ACDT
     C                   MOVEL     DSTIME        ACTIME
      *
     C                   UPDATE    DBRCDF
      *
     C                   ENDIF                                                  *IN99 IFEQ *OFF
      *
     C                   ENDSR                                                  CHGSTS
      *
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
     C* ERRCK1 - Error checking for screen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
      *
     C     SCKEY         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY  IFEQ *BLANKS
      *
      * THE STATE MUST BE VALID
      *
     C     SCST          IFNE      *BLANK
     C*
     C                   MOVEL(P)  SCST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        72  90
     C*
     C                   ENDIF
      *
      * CHECK TO BE SURE RECORD HAS NOT BEEN UPDATED SINCE ENTRY
      *
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N90ACDT          IFGT      SAVEDT
     C     ACDT          OREQ      SAVEDT
     C     ACTIME        ANDGT     SAVETM
     C                   SETON                                        73  90
     C                   ENDIF                                                  ACDT IFGT SAVEDT
      *
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for screen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for screen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVE      ACKEY         SCKEY
     C                   MOVE      ACNAME        SCNAME
     C                   MOVE      ACADR1        SCADR1
     C                   MOVE      ACADR2        SCADR2
     C                   MOVE      ACCTY         SCCTY
     C                   MOVE      ACST          SCST
     C                   MOVE      ACZIP1        SCZIP1
     C                   MOVE      ACZIP2        SCZIP2
     C                   MOVE      ACPHN1        SCAREA
     C                   MOVEL     ACPHN#        SCPHN1
     C                   MOVE      ACPHN#        SCPHN2
     C                   MOVE      ACFAX1        SCFAXA
     C                   MOVEL     ACFAX#        SCFAX1
     C                   MOVE      ACFAX#        SCFAX2
     C                   MOVE      ACCTNM        SCCTNM
      *
     C     *IN40         IFEQ      *ON
     C                   MOVE      'A'           ACSTAT
     C                   ELSE
     C                   MOVE      ACSTAT        SCSTAT
     C                   ENDIF                                                  *IN40 IFEQ *ON
      *
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
      *
     C                   MOVE      ##CO#         ACCO#
     C                   MOVE      SCKEY         ACKEY
     C                   MOVE      SCNAME        ACNAME
     C                   MOVE      SCADR1        ACADR1
     C                   MOVE      SCADR2        ACADR2
     C                   MOVE      SCCTY         ACCTY
     C                   MOVE      SCST          ACST
     C                   MOVE      SCZIP1        ACZIP1
     C                   MOVE      SCZIP2        ACZIP2
     C                   MOVE      SCAREA        ACPHN1
     C                   MOVEL     SCPHN1        ACPHN#
     C                   MOVE      SCPHN2        ACPHN#
     C                   MOVE      SCFAXA        ACFAX1
     C                   MOVEL     SCFAX1        ACFAX#
     C                   MOVE      SCFAX2        ACFAX#
     C                   MOVE      SCCTNM        ACCTNM
     C                   MOVE      SCSTAT        ACSTAT
      *
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
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
      *
     C     @@MODE        COMP      'A'                                    40    ADD
      *
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
      *
     C                   SELECT
      *
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
      *
     C                   ENDSL                                                  @@MODE WHEQ 'S'
      *
     C                   ENDSR                                                  PREOTH
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        ACUSER
     C                   MOVEL     Q1PGM         ACPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACDT
     C                   MOVEL     DSTIME        ACTIME
      *
     C     *IN40         IFEQ      *ON
     C                   MOVE      ACUSER        ACIUSR
     C                   MOVE      ACPGM         ACIPGM
     C                   MOVE      ACDT          ACIDT
     C                   MOVE      ACTIME        ACITIM
     C                   ENDIF                                                  *IN40 IFEQ *ON
      *
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C                   MOVE      'A'           ACSTAT
     C*
     C                   MOVEL     Q1USER        ACUSER
     C                   MOVEL     Q1PGM         ACPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACDT
     C                   MOVEL     DSTIME        ACTIME
     C*
     C                   MOVE      ACUSER        ACIUSR
     C                   MOVE      ACPGM         ACIPGM
     C                   MOVE      ACDT          ACIDT
     C                   MOVE      ACTIME        ACITIM
      *
     C                   WRITE     DBRCDF
     C                   FEOD      WTACP
     C                   ENDSR
