     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*      W2069R04 - EDI DATA ELEMENT MAINTENANCE
     H*
     H*****************************************************************
     H*  REVISIONS
     H*
     H*  08/06/01  C:15   Support: WC048/28506
     H*                   Added edits to prevent fields from being
     H*                   blank or zero.
     H*
     H*  07/28/00  C: 53  SUPPORT: WC985
     H*                   SETOF ERROR INDICATORS BEFORE EDITING
     H*
     H*  06/25/99  C:118  SUPPORT: WC048/18532
     H*                   Move SCDN# to DCDN# in MVSCDB. Made V3 chgs
     H*                   to display file. Made 'Data element criteria'
     H*                   (SCCRTR) required field as indicated in RPG.
     H*
     H* *** ADD NEW REVISIONS TO THE TOP OF THE LIST ***
     H*****************************************************************
     FW2069F04  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*  Physical file definition (for maintaining record)
     FWTDCP     UF A E             DISK
     F                                     RENAME(PDC01:DBRCDF)
     F*  Logical file definition (for verifying record)
     FWTDCL     IF   E           K DISK
     F*** Begin add *** 08/06/01 **************************************
     FSTSTL3    IF   E           K DISK
     F*** End add ***** 08/06/01 **************************************
     F*
     D***PY SCOPYMT01I
      /COPY SCOPYMT01D
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('EDI Data Element Cr-
     D                                     iteria')
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
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     DCKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##JST
     C                   KFLD                    ##DN#
     C*
     C*** Begin add *** 08/06/01 **************************************
     C*   KEY LIST FOR THE STATE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY            10
     C*** End add ***** 08/06/01 **************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    DCCO#         ##CO#
     C     *LIKE         DEFINE    DCJST         ##JST
     C     *LIKE         DEFINE    DCDN#         ##DN#
     C     *LIKE         DEFINE    DCSET         ##SET
     C     *LIKE         DEFINE    DCMTC         ##MTC
     C     *LIKE         DEFINE    DCCRTR        ##CRTR
     C*
     C     *LIKE         DEFINE    DCJST         @@JST
     C     *LIKE         DEFINE    DCDN#         @@DN#
     C     *LIKE         DEFINE    DCSET         @@SET
     C     *LIKE         DEFINE    DCMTC         @@MTC
     C     *LIKE         DEFINE    DCCRTR        @@CRTR
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     DCSTAT        IFEQ      'I'
     C                   MOVE      'A'           DCSTAT
     C                   ELSE
     C                   MOVE      'I'           DCSTAT
     C                   ENDIF                                                  emSTAT
     C*
     C                   MOVEL     Q1USER        DCUSER
     C                   MOVEL     Q1PGM         DCPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DCDT
     C*
     C                   MOVEL     DSTIME        DCTIME
     C*
     C                   UPDATE    DBRCDF
     C*
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
     C                   MOVEA     '000'         *IN(22)                        ADD 07/28/00
     C*
     C*** Begin add *** 08/06/01 **************************************
     C*  Jurisdiction State can not be blank and it must be valid.
     C*
     C     SCJST         IFEQ      *BLANKS
     C                   SETON                                        7090
     C                   ENDIF
      *
     C                   MOVEL(P)  SCJST         XXKEY
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7190
     C                   END
     C*
     C*  Data element # can not be 0.
     C*
     C     SCDN#         IFEQ      0
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C*  Data element description can not be blank.
     C*
     C     SCDESC        IFEQ      *BLANKS
     C                   SETON                                        7390
     C                   ENDIF
     C*** End add ***** 08/06/01 **************************************
     C*
     C* Error message number is required
     C*
     C     SCCRTR        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN22                          CRITERIA BLANK
     C                   MOVE      *ON           *IN90                                      NK
     C                   ENDIF                                                  SCCRTR=BLANKS
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCRTR        IFNE      'M'                                          Mandatory
     C     SCCRTR        ANDNE     'C'                                          Conditional
     C     SCCRTR        ANDNE     'O'                                          Optional
     C     SCCRTR        ANDNE     'R'                                          Restricted
     C     SCCRTR        ANDNE     'N'                                          Not applicable
     C                   MOVE      *ON           *IN23
     C                   MOVE      *ON           *IN90                                      NK
     C                   ENDIF                                                  SCCRTR
     C                   ENDIF                                                  *IN90 OFF
     C*
     C*   If add, can not be in file
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C                   MOVE      SCJST         ##JST                                      NK
     C                   MOVE      SCDN#         ##DN#                                      NK
     C     DCKY          CHAIN     PDC01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      *ON           *IN90                                      NK
     C                   MOVE      *ON           *IN24                                      NK
     C                   ENDIF                                                  *IN99
     C                   ENDIF                                                  *IN90
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90DCDT          IFGT      SAVEDT
     C     DCDT          OREQ      SAVEDT
     C     DCTIME        ANDGT     SAVETM
     C                   SETON                                        9078
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
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     DCCRTR        SCCRTR
     C                   MOVEL     DCDESC        SCDESC
     C                   MOVEL     DCSTAT        SCSTAT
     C                   MOVE      DCDN#         SCDN#
     C*
     C                   ELSE
     C*
     C                   MOVEL     *ZEROS        SCDN#
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCCRTR
     C                   MOVEL     'A'           SCSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         DCCO#
     C                   MOVEL     SCJST         DCJST
     C***** BEGIN ADD **** 06/25/99 ***********************************
     C                   MOVEL     SCDN#         DCDN#
     C***** END   ADD **** 06/25/99 ***********************************
     C                   MOVEL     *BLANKS       DCSET
     C                   MOVEL     *BLANKS       DCMTC
     C                   MOVEL     SCDESC        DCDESC
     C                   MOVEL     SCCRTR        DCCRTR
     C                   MOVEL     SCSTAT        DCSTAT
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
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C     *IN40         IFEQ      *OFF
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     DCJST         SCJST
     C                   MOVEL     DCDN#         SCDN#
     C                   MOVEL     DCDESC        SCDESC
     C                   MOVEL     DCCRTR        SCCRTR
     C                   MOVEL     DCSTAT        SCSTAT
     C*
     C                   Z-ADD     DCCO#         ##CO#
     C                   MOVEL     DCJST         ##JST
     C                   MOVEL     DCDN#         ##DN#
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      *ON           DSPTYP
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
     C                   MOVEL     Q1USER        DCUSER
     C                   MOVEL     Q1PGM         DCPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DCDT
     C*
     C                   MOVEL     DSTIME        DCTIME
     C*
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
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        DCUSER
     C                   MOVEL     Q1PGM         DCPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DCDT
     C*
     C                   MOVEL     DSTIME        DCTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WTDCP
     C*
     C                   ENDSR
