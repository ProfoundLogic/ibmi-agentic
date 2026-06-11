     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*================================================================
     H/TITLE W0045R02 - PREMIUM FINANCE COMPANY MAINTENANCE
     H*
     H*  DESCRIPTION:  THIS PROGRAM ALLOWS FOR ADD, CHANGE OR DISPLAY
     H*                OF PREMIUM FINANCE COMPANY INFORMATION.
     H*
     H*  CALLED FROM:  W0045R - WORK WITH PREMIUM FINANCE COMPANIES
     H*                W0051R02 - EMPLOYER DETAIL MAINTENANCE
     H*                W0110R - POST CASH AND ADJUSTMENTS
     H*                W1210R02 - A/R BATCH ENTRY
     H*                W0612R - A/R INQUIRY
     H*                W0610R - A/R DETAIL INQUIRY
     H*                W0165R - WORK WITH CANCELLATIONS/REINSTATEMENTS
     H*                W0162R - WORK WITH CANCELLATION BATCH DETAIL
     H*
     H*  CALLS TO:  NONE
     H*
     H*  CREATED:  09/27/99  PROGRAMMER: C:118  PROJECT: WC939
     H*================================================================
     H*  REVISIONS:
     H*
     H*================================================================
     H*  FILE USAGE:
     H*  WMPFP - PREMIUM FINANCE PHYSICAL FILE
     H*  WMPFL - PREMIUM FINANCE LOGICAL - PFCO#/PFFND/PFFCOD/PFEFDT
     H*  WMFNP - FUND MASTER PHYSICAL FILE
     H*  WTGTL - GENERIC TABLE FILE
     H*
     H*================================================================
     H*  INDICATOR USAGE:
     H*
     H*  09 - DISPLAY MODE
     H* N09 - CHANGE MODE
     H*  40 - ADD MODE
     H*  60 - GENERAL CHAIN INDICATOR
     H*  71 - FINANCE CODE CANNOT BE BLANK
     H*  72 - EFFECTIVE DATE MUST BE VALID
     H*  73 - EXPIRATION DATE MUST BE VALID
     H*  74 - FINANCE COMPANY NAME CANNOT BE BLANK
     H*  75 - ADDRESS LINE 1 CANNOT BE BLANK
     H*  76 - EFFECTIVE DATE MUST BE LESS THAN EXPIRATION DATE
     H*  77 - CITY CANNOT BE BLANK
     H*  78 - STATE CANNOT BE BLANK
     H*  79 - ZIP CODE CANNOT BE BLANK
     H*  81 - RECORD UPDATED BY OTHER USER
     H*  82 - FINANCE COMPANY MUST BE UNIQUE FOR EFFECTIVE DATE
     H*  83 - COMPANY TYPE MUST BE VALID IF ENTERED
     H*  84 - EFFECTIVE DATE CANNOT BE BLANK
     H*  90 - GENERAL ERROR INDICATOR
     H*  99 - ERROR INDICATOR
     H*
     H*================================================================
     H*
     FW0045F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMPFP     UF A E             DISK
     F                                     RENAME(PPF01:DBRCDF)
     F*
     F*  Logical file definition
     FWMPFL     IF   E           K DISK
     F                                     RENAME(PPF01:PPF02)
     F*
     F*  Physical file for group
     FWMFNP     IF   E             DISK
     F*
     F*  Generic table file for type of finance company
     FWTGTL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Premium Finance Comp-
     D                                     anies')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          WMPFP
     C                   PARM                    @@RRN2            9 0          WMFNP
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
     C*  Generic table file key
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*  Finance company code key
     C*
     C     FCKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFCOD
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    PFEFDT        XXEFDT
     C     *LIKE         DEFINE    PFEXDT        XXEXDT
     C     *LIKE         DEFINE    PFZIP         SCZIP
     C     *LIKE         DEFINE    PFPHN#        SCPHN#
     C     *LIKE         DEFINE    PFCO#         SCCO#
     C     *LIKE         DEFINE    GTDESC        XXDESC
     C*================================================================
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*================================================================
     C* ERRCK1 - Error checking for screen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*  Validate finance company code cannot be blank
     C*
     C     SCFCOD        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN71
     C                   ENDIF                                                  SCFCOD=BLNKS
     C*
     C*  Validate eff date and exp date and date range
     C*
     C  N90SCEFDT        IFNE      *ZEROS
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN72
     C                   ENDIF                                                  SRERR=ON
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN84
     C                   ENDIF                                                  SCEFDT<>0
     C*
     C  N90SCEXDT        IFNE      *ZEROS
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN73
     C                   ENDIF                                                  SRERR=ON
     C*
     C                   ENDIF                                                  SCEXDT<>0
     C*
     C*  Convert screen dates to CYMD format for comparison
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
     C*
     C*  Eff date must be less than exp date
     C*
     C  N90XXEFDT        IFNE      *ZERO
     C     XXEXDT        ANDNE     *ZERO
     C*
     C     XXEFDT        IFGT      XXEXDT
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN76
     C                   ENDIF                                                  EFDT>EXDT
     C*
     C                   ENDIF                                                  EFDT<>0
     C*                                                    EXDT<>0
     C*  Finance company code must be unique for eff date
     C*
     C     *IN40         IFEQ      *ON
     C*
     C     FCKY          CHAIN     PPF02                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C  N90XXEFDT        IFEQ      PFEFDT
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN82
     C                   ENDIF                                                  XXEFDT>=PFEF
     C*
     C                   READE     PPF02                                  60
     C                   ENDDO                                                  *IN60DOWEQOF
     C*
     C                   ENDIF                                                  *IN40=ON
     C*
     C*  Finance company name must be entered
     C*
     C  N90SCNAME        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN74
     C                   ENDIF                                                  SCNAME=BLKS
     C*
     C*  Address line 1 must be entered
     C*
     C  N90SCADR1        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN75
     C                   ENDIF                                                  SCADR1=BLKS
     C*
     C*  City, state and zip must be entered
     C*
     C  N90SCCTY         IFEQ      *BLANKS
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN77
     C                   ENDIF                                                  SCCTY=BLKS
     C*
     C  N90SCST          IFEQ      *BLANKS
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN78
     C                   ENDIF                                                  SCST=BLKS
     C*
     C  N90SCZIP1        IFEQ      *ZEROS
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN79
     C                   ENDIF                                                  SCZIP1=0
     C*
     C*  If company type is entered, verify the code is valid
     C*
     C  N90SCTYPE        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  'PFIN'        XXTBID
     C                   MOVEL(P)  SCTYPE        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN83
     C                   ENDIF                                                  *IN60=ON
     C*
     C                   ENDIF                                                  SCTYPE<>BLKS
     C*
     C*  Verify record has not been updated since entry
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90PFDT          IFGT      SAVEDT
     C     PFDT          OREQ      SAVEDT
     C     PFTIME        ANDGT     SAVETM
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN81
     C                   ENDIF                                                  PFDT>=SAVEDT
     C*
     C                   ENDSR                                                  ERRCK1
     C*================================================================
     C* ERRCK2 - Error checking for screen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*================================================================
     C* ERRCK3 - Error checking for screen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
     C*================================================================
     C* KJDO - Subroutine for F10 Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SETOFF                                       60
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCTYPE'
     C*
     C                   MOVEL(P)  'PFIN'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   MOVEL(P)  *BLANKS       SCTYPE
     C                   MOVEL(P)  *BLANKS       SCDESC
     C*
     C                   CALL      'W2493R'
     C                   PARM                    ##DATA
     C                   PARM                    @@MODE
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C*
     C     XXKEY         IFNE      *BLANKS
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     XXKEY         SCTYPE
     C                   MOVEL(P)  GTDESC        SCDESC
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  XXKEY<>BLKS
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      PFFCOD        SCFCOD
     C*
     C*  Convert date for screen display
     C*
     C                   Z-ADD     PFEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   Z-ADD     PFEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C*
     C                   MOVEL     PFNAME        SCNAME
     C                   MOVEL     PFADR1        SCADR1
     C                   MOVEL     PFADR2        SCADR2
     C                   MOVEL     PFCTY         SCCTY
     C                   MOVEL     PFST          SCST
     C                   MOVEL     PFZIP         SCZIP1
     C                   MOVE      PFZIP         SCZIP2
     C                   MOVEL     PFCONT        SCCONT
     C                   Z-ADD     PFAREA        SCAREA
     C                   MOVEL     PFPHN#        SCPHN1
     C                   MOVE      PFPHN#        SCPHN2
     C                   MOVEL     PFTYPE        SCTYPE
     C*
     C*  Get PFTYPE description from table file
     C*
     C     SCTYPE        IFNE      *BLANKS
     C                   MOVEL(P)  'PFIN'        XXTBID
     C                   MOVEL(P)  SCTYPE        XXKEY
     C                   MOVEL(P)  *BLANKS       SCDESC
     C*
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCDESC
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  SCTYPE<>BLKS
     C*
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVE      SCCO#         PFCO#
     C                   MOVE      SCFND         PFFND
     C                   MOVE      SCFCOD        PFFCOD
     C                   MOVE      XXEFDT        PFEFDT
     C                   MOVE      XXEXDT        PFEXDT
     C                   MOVE      SCNAME        PFNAME
     C                   MOVE      SCADR1        PFADR1
     C                   MOVE      SCADR2        PFADR2
     C                   MOVE      SCCTY         PFCTY
     C                   MOVE      SCST          PFST
     C*
     C*  Combine SCZIP1 and SCZIP2 for file
     C*
     C                   MOVEL     SCZIP1        SCZIP
     C                   MOVE      SCZIP2        SCZIP
     C                   MOVE      SCZIP         PFZIP
     C*
     C                   MOVE      SCCONT        PFCONT
     C                   MOVE      SCAREA        PFAREA
     C*
     C*  Combine SCPHN1 and SCPHN2 for file
     C*
     C                   MOVEL     SCPHN1        SCPHN#
     C                   MOVE      SCPHN2        SCPHN#
     C                   MOVE      SCPHN#        PFPHN#
     C*
     C                   MOVEL     SCTYPE        PFTYPE
     C*
     C                   ENDSR                                                  MVSCDB
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C*
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *OFF          *IN40
     C*
     C     @@MODE        IFEQ      'A'
     C                   MOVE      *ON           *IN40
     C                   ENDIF                                                  @@MODE=A
     C*
     C*  Get fund and fund name
     C*
     C     @@RRN2        CHAIN     WMFNP                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     FNCO#         SCCO#
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL(P)  FNNAME        SCFNNM
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR                                                  PREOTH
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1PGM         PFPGM
     C                   MOVEL     Q1USER        PFUSER
     C                   MOVE      DSTIME        PFTIME
     C*
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PFDT
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF                                                  *IN99=OFF
     C*
     C                   ENDSR                                                  UPDATR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1PGM         PFPGM
     C                   MOVEL     Q1PGM         PFIPGM
     C                   MOVEL     Q1USER        PFUSER
     C                   MOVEL     Q1USER        PFIUSR
     C                   MOVE      DSTIME        PFTIME
     C                   MOVE      DSTIME        PFITM
     C*
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PFDT
     C                   MOVEL     SRDTO         PFIDT
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      WMPFP
     C                   ENDSR                                                  WRITER
