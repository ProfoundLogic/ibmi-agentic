     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W2295R02 - MT MCO Group Codes
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: W2295R                                             *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED:                                                      *
     H* 10/28/99 C:61  PROJECT WC958                                  *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H* SCR01                                                         *
     H*  71       WC31320 - MCO Group code is required entry.         *
     H*  72       WCR0500 - Invalid effective date.                   *
     H*  73       WCR0500 - Invalid expiration date.                  *
     H*  74       WCR0500 - Dates not in ascending order.             *
     H*  75       WC31321 - Dates overlap with another entry of same  *
     H*                     code.                                     *
     H*  76       WCR0503 - Company Name can not be blank.            *
     H*  77       WCR0503 - First address line can not be blank.      *
     H*  78       WCR0503 - City can not be blank.                    *
     H*  79       WCR0513 - State must be valid.                      *
     H*  80       WCR0503 - City can not be blank.                    *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*****************************************************************
     H*
     H*
     FW2295F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
      *  Physical file definition (for maintaining record)
     FWMMGP     UF A E             DISK
     F                                     RENAME(PMG01:DBRCDF)
     FWMMGL     IF   E           K DISK
     F                                     INFDS(MGFDS)
      *
      *  Fund
     FWMFNP     IF   E             DISK
      *
      *  State Table
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
      *
      *
      /COPY SCOPYMT01D
     D MGFDS           DS
     D  MGRRN                397    400B 0
      *
      *
     D***PY SCOPYMT01I
      *
      *
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('MCO Group Codes')
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
     C                   PARM                    @@RRN             9 0          WMMGP
     C                   PARM                    @@RRN2            9 0          WMFNP
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
      *  Fund master
      *
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
      *
      *  MCO Group code table
      *
     C     MGKY1         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCMCO
      *
      *  State code table
      *
     C     STKY1         KLIST
     C                   KFLD                    KKKEY
      *
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
      *
     C     *LIKE         DEFINE    MGEFDT        XXEFDT
     C     *LIKE         DEFINE    MGEXDT        XXEXDT
     C     *LIKE         DEFINE    STKEY         KKKEY
      *
      *
     C/COPY SCOPYMT01C
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
      * MCO group code required entry
      *
     C  N90SCMCO         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF
      *
      * Effective date must be valid
      *
     C     *IN90         IFEQ      *OFF
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
      *
     C     SRERR         IFEQ      '1'
     C                   SETON                                        72  90
     C                   ENDIF                                                  SRERR
      *
     C                   ENDIF                                                  *IN90
      *
      * Expiration date must be valid, if entered
      *
     C  N90SCEXDT        IFNE      *ZERO
      *
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT
      *
     C     SRERR         IFEQ      '1'
     C                   SETON                                        73  90
     C                   ENDIF                                                  SRERR
      *
     C                   ENDIF                                                  SREXDT
      *
      *  Convert dates entered on screen to CYMD
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
      *
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
      *
      *  If expiration date entered, dates must be in ascending order
      *
     C  N90SCEXDT        IFNE      *ZERO
     C     XXEXDT        ANDLT     XXEFDT
     C                   SETON                                        74  90
     C                   ENDIF                                                  SREXDT
      *
      *
      * New effective date can not overlap with another entry of the
      * same code.
      *
     C     *IN90         IFEQ      *OFF
      *
     C     MGKY1         CHAIN     PMG01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     MGRRN         IFNE      @@RRN
      *
     C                   SELECT
      *
      *
     C     MGEXDT        WHENEQ    *ZERO
     C     XXEXDT        ANDEQ     *ZERO
     C                   SETON                                        90  75
     C                   LEAVE
      *
      *
     C     MGEXDT        WHENEQ    *ZERO
     C     XXEXDT        ANDNE     *ZERO
      *
     C     XXEXDT        IFGE      MGEFDT
     C                   SETON                                        90  75
     C                   LEAVE
     C                   ENDIF
      *
      *
     C     MGEXDT        WHENNE    *ZERO
     C     XXEXDT        ANDEQ     *ZERO
      *
     C     XXEFDT        IFLE      MGEXDT
     C                   SETON                                        90  75
     C                   LEAVE
     C                   ENDIF
      *
      *
     C     MGEXDT        WHENNE    *ZERO
     C     XXEXDT        ANDNE     *ZERO
      *
     C     XXEXDT        IFLT      MGEFDT
     C     XXEFDT        ORGT      MGEXDT
      *
     C                   ELSE
      *
     C                   SETON                                        90  75
     C                   LEAVE
     C                   ENDIF
      *
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  MGRRN
      *
     C     MGKY1         READE     PMG01                                  60
     C                   ENDDO                                                  *IN60
      *
     C                   ENDIF                                                  *IN90
      *
      * Company name required entry
      *
     C  N90SCNAME        IFEQ      *BLANKS
     C                   SETON                                        90  76
     C                   ENDIF                                                  SCNAME
      *
      * Address 1 required entry
      *
     C  N90SCADR1        IFEQ      *BLANKS
     C                   SETON                                        90  77
     C                   ENDIF                                                  SCADR1
      *
      * City required entry
      *
     C  N90SCCTY         IFEQ      *BLANKS
     C                   SETON                                        90  78
     C                   ENDIF                                                  SCCTY
      *
      * State must be valid.
      *
     C  N90              MOVEL(P)  SCST          KKKEY
     C  N90KKKEY         CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        90  79
      *
      * Zip code required entry
      *
     C  N90SCZIP5        IFEQ      *ZERO
     C                   SETON                                        90  80
     C                   ENDIF                                                  SCZIP5
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
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C  N40              MOVEL(P)  MGCO#         SCCO#
     C  N40              MOVEL(P)  MGFND         SCFND
     C                   MOVEL(P)  MGMCO         SCMCO
      *
     C                   Z-ADD     MGEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
      *
     C                   Z-ADD     MGEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
      *
     C                   MOVEL(P)  MGNAME        SCNAME
     C                   MOVEL(P)  MGADR1        SCADR1
     C                   MOVEL(P)  MGADR2        SCADR2
     C                   MOVEL(P)  MGCTY         SCCTY
     C                   MOVEL(P)  MGST          SCST
     C                   MOVEL     MGZIP         SCZIP5
     C                   MOVE      MGZIP         SCZIP4
     C                   MOVEL(P)  MGREP         SCREP
     C                   Z-ADD     MGAREA        SCAREA
     C                   MOVEL     MGPHN#        SCPH1
     C                   MOVE      MGPHN#        SCPH2
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
     C                   MOVEL(P)  SCCO#         MGCO#
     C                   MOVEL(P)  SCFND         MGFND
     C                   MOVEL(P)  SCMCO         MGMCO
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGEFDT
      *
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGEXDT
      *
     C                   MOVEL(P)  SCNAME        MGNAME
     C                   MOVEL(P)  SCADR1        MGADR1
     C                   MOVEL(P)  SCADR2        MGADR2
     C                   MOVEL(P)  SCCTY         MGCTY
     C                   MOVEL(P)  SCST          MGST
     C                   MOVEL(P)  SCZIP5        MGZIP
     C                   MOVE      SCZIP4        MGZIP
     C                   MOVEL(P)  SCREP         MGREP
     C                   Z-ADD     SCAREA        MGAREA
     C                   MOVEL     SCPH1         MGPHN#
     C                   MOVE      SCPH2         MGPHN#
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
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
      *
      *  Get Fund info
      *
     C     @@RRN2        CHAIN     WMFNP                              60
     C  N60              Z-ADD     FNCO#         SCCO#
     C  N60              Z-ADD     FNFND         SCFND
     C  N60              MOVEL(P)  FNNAME        SCFNNM
     C   60              Z-ADD     ##CO#         SCCO#
     C   60              Z-ADD     ##FND         SCFND
     C   60              MOVEL(P)  ##FNAM        SCFNNM
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
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        MGUSER
     C                   MOVEL     Q1PGM         MGPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGDT
     C                   MOVEL     DSTIME        MGTIME
      *
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C                   ENDSR                                                  UPDATR
      *
      *
      *
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        MGIUSR
     C                   MOVEL     Q1USER        MGUSER
     C                   MOVEL     Q1PGM         MGIPGM
     C                   MOVEL     Q1PGM         MGPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGIDT
     C                   MOVEL     SRDTO         MGDT
     C                   MOVEL     DSTIME        MGITIM
     C                   MOVEL     DSTIME        MGTIME
      *
     C                   WRITE     DBRCDF
     C                   FEOD      WMMGP
     C                   ENDSR                                                  WRITER
      *
      *
      *
