     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W2216R02 - ZIP CODE BY COUNTY INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE COUNTY ZIPCODE FILE.     *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 02/28/03  PROGRAMMER: C:132 PROJECT: WC1118         *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  20 - (WCR0504) Invalid State Code   {ADD mode only}          *
     H*  21 - (WCR0503) Code must be entered {ADD mode only}          *
     H*  22 - (WCR0505) State/Code not found in State/County Table    *
     H*  23 - (WCR0503) Zip Code Entry cannot be blank                *
     H*  24 - (WC30005) Zip Code already exists in ZipCode file       *
     H*
     H*****************************************************************
     F*
     FW2216F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMCZP     UF A E             DISK
     F                                     RENAME(PCZ01:DBRCDF)
     F                                     INFDS(DBFDS)
     F*
     F*  Logical file definition (for Checking for Duplicates on add)
     FWMCZL     IF   E           K DISK
     F                                     RENAME(PCZ01:PCZ02)
     F*
     F* Table of State & State County Codes
     FWTCNL1    IF   E           K DISK
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
     D OPTNM           C                   CONST('Zip Codes by County')
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
     C                   PARM                    @@ST              2
     C                   PARM                    @@CNTY            2
     C                   PARM                    @@DESC           30
     C                   PARM                    @@STAT            1
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
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKST
     C                   KFLD                    KKZIP
     C     CNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY
     C*
     C     CZKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCST
     C                   KFLD                    SCZIP
     C*
     C*   KEY LIST FOR STATE FILE
     C*
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CZCO#         KKCO#
     C     *LIKE         DEFINE    CZST          KKST
     C     *LIKE         DEFINE    CZCNTY        KKCNTY
     C     *LIKE         DEFINE    CZZIP         KKZIP
     C     *LIKE         DEFINE    CNKEY         XXKEY
     C*
     C/COPY SCOPYMT01C
     C*
     C*===============================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*===============================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     CZSTAT        IFEQ      'I'
     C                   MOVE      'A'           CZSTAT
     C                   ELSE
     C                   MOVE      'I'           CZSTAT
     C                   ENDIF                                                  CNSTAT=I
     C*
     C                   MOVEL     Q1USER        CZUSER
     C                   MOVEL     Q1PGM         CZPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CZDT
     C*
     C                   MOVEL     DSTIME        CZTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN60=*OFF
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
     C*  Zip Code Entry CANNOT be All Zeros
     C*
B02  C  N90SCZIP         IFEQ      *ZEROS
     C                   SETON                                        23  90
E02  C                   ENDIF                                                  SCCODE=*BLANKS
     C*
     C*  Check to be Sure Add ZIPCODE is not in WMCZP file
     C*
     C   40
B02  CANN90CZKY          CHAIN     PCZ02                              60
     C   40
     CANN90
     CANN60              SETON                                        22  90
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90CZDT          IFGT      SAVEDT
     C     CZDT          OREQ      SAVEDT
     C     CZTIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  CZDT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
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
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *ON
     C                   MOVEL     @@ST          SCST
     C                   MOVEL     @@CNTY        SCCODE
     C                   MOVEL     @@DESC        SCDESC
     C                   MOVEL     @@STAT        SCSTAT
     C                   CLEAR                   SCZIP
     C*
     C                   ELSE
     C*
     C                   MOVEL     CZSTAT        SCSTAT
     C                   MOVE      CZCNTY        SCCODE
     C                   MOVEL     CZST          SCST
     C                   MOVEL(P)  CZST          #WRKA4            4
     C                   MOVE      CZCNTY        #WRKA4
     C                   MOVE      CZCO#         XXCO#             3 0
     C                   MOVEL(P)  #WRKA4        XXKEY
     C     CNKY          CHAIN     PCN01                              61
     C  N61              MOVEL(P)  CNDESC        SCDESC
     C   61              MOVE      *BLANKS       SCDESC
     C                   MOVEL     CZZIP         SCZIP
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVE      ##CO#         CZCO#
     C                   MOVEL     SCST          CZST
     C                   MOVEL     SCZIP         CZZIP
     C                   MOVEL     SCSTAT        CZSTAT
     C                   MOVE      SCCODE        CZCNTY
     C  N40              MOVE      SCSTAT        CZSTAT
     C   40              MOVEL     'A'           CZSTAT
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
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C     @@MODE        WHENEQ    'A'
     C                   MOVEL     @@ST          #STATE            2
     C                   MOVEL     @@CNTY        #CNTY             2
     C                   MOVEL     @@DESC        #DESC            30
     C                   MOVEL     @@STAT        #STAT             1
     C*
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
     C*
     C                   MOVEL     Q1USER        CZUSER
     C                   MOVEL     Q1PGM         CZPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CZDT
     C                   MOVEL     DSTIME        CZTIME
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      WMCZP
     C                   ENDSR
