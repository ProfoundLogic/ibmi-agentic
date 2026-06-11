     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0043R02 - PRODUCER NON-RESIDENT INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE PROD NOT-RES ST FILE.    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0043R - WORK WITH PRODUCER NON-RESIDENT STATES            *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 12/11/97  PROGRAMMER: C:33  PROJECT: WC683          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H*****************************************************************
     FW0043F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMPSP     UF A E             DISK
     F                                     RENAME(PPS01:DBRCDF)
     FWMAPP     IF   E             DISK
     FWMAGL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMPSL     IF   E           K DISK
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
     D*
     D OPTNM           C                   CONST('Producer Non-Residen-
     D                                     t States')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
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
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  NON-RESIDENT STATE FILE
     C*
     C     PSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C                   KFLD                    SCPROD
     C                   KFLD                    SCST
     C*
     C*  SYSTEM TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    APCO#
     C                   KFLD                    APFEIN
     C                   KFLD                    APMOD
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    STKEY         XXKEY
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
     C     PSSTAT        IFEQ      'I'
     C                   MOVE      'A'           PSSTAT
     C                   ELSE
     C                   MOVE      'I'           PSSTAT
     C                   ENDIF                                                  PSSTAT=I
     C*
     C                   MOVEL(P)  Q1USER        PSUSER
     C                   MOVEL(P)  Q1PGM         PSPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PSDT
     C*
     C                   MOVEL     DSTIME        PSTIME
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
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C  N90*IN40         IFEQ      *ON
     C*
     C*  LICENSE STATE MUST BE VALID
     C*
     C                   MOVEL(P)  SCST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        71  90
     C*
     C*  BE SURE THE STATE HAS NOT BEEN PREVIOUSLY BEEN USED
     C*
     C  N90PSKY          CHAIN     PPS01                              60
     C  N90
     CANN60              SETON                                        72  90
     C*
     C*  THE NON-RESIDENT STATE CANNOT BE THE RESIDENT STATE
     C*
     C  N90SCST          IFEQ      APRST
     C                   SETON                                        89  90
     C                   ENDIF                                                  SCST=APRST
     C*
     C                   ENDIF                                                  *IN40=*ON
     C*
     C*  LICENSE FLAG MUST BE 'Y' OR BLANK
     C*
     C  N90SCLFLG        IFNE      *BLANK
     C     SCLFLG        ANDNE     'Y'
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCLFLG<>*BLANK/Y
     C*
     C*  VALIDATE LICENSE RENEWAL DATE
     C*
     C  N90SCRWDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCRWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRWDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        74  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCRWDT<>*ZEROS
     C*
     C*  VALIDATE LICENSE EXPIRATION DATE
     C*
     C  N90SCEXDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        75  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCEXDT<>*ZEROS
     C*
     C*  VALIDATE DATE FILED WITH CARRIER
     C*
     C  N90SCFIDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCFIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFIDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        76  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCFIDT<>*ZEROS
     C*
     C*  VALIDATE DATE MAILED TO STATE
     C*
     C  N90SCMLDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCMLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCMLDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        77  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCMLDT<>*ZEROS
     C*
     C*  VALIDATE CONFIRMED DATE
     C*
     C  N90SCCFDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCCFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCFDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        78  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCCFDT<>*ZEROS
     C*
     C*  VALIDATE DENIED DATE
     C*
     C  N90SCDNDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCDNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDNDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        79  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCDNDT<>*ZEROS
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90APDT          IFGT      SAVEDT
     C     APDT          OREQ      SAVEDT
     C     APTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  APDT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C*
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C   40              MOVE      'A'           PSSTAT
     C*
     C                   MOVE      PSSTAT        SCSTAT
     C                   MOVEL     PSLFLG        SCLFLG
     C                   MOVEL     PSST          SCST
     C                   MOVEL(P)  PSLIC#        SCLIC#
     C                   MOVEL(P)  PSLTYP        SCLTYP
     C                   MOVEL(P)  PSLTP2        SCLTP2
     C                   MOVEL(P)  PSLTP3        SCLTP3
     C                   MOVEL(P)  PSLTP4        SCLTP4
     C                   MOVEL(P)  PSLTP5        SCLTP5
     C*
     C                   Z-ADD     PSEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C*
     C                   Z-ADD     PSRWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRWDT
     C*
     C                   Z-ADD     PSFIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFIDT
     C*
     C                   Z-ADD     PSMLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMLDT
     C*
     C                   Z-ADD     PSCFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCFDT
     C*
     C                   Z-ADD     PSDNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDNDT
     C*
     C                   MOVEL(P)  PSCOM1        SCCOM1
     C                   MOVEL(P)  PSCOM2        SCCOM2
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         PSCO#
     C                   Z-ADD     SCFEIN        PSFEIN
     C                   Z-ADD     SCMOD         PSMOD
     C                   MOVE      SCSTAT        PSSTAT
     C                   Z-ADD     SCPROD        PSPROD
     C                   MOVEL     SCLFLG        PSLFLG
     C                   MOVEL     SCST          PSST
     C                   MOVEL(P)  SCLIC#        PSLIC#
     C                   MOVEL(P)  SCLTYP        PSLTYP
     C                   MOVEL(P)  SCLTP2        PSLTP2
     C                   MOVEL(P)  SCLTP3        PSLTP3
     C                   MOVEL(P)  SCLTP4        PSLTP4
     C                   MOVEL(P)  SCLTP5        PSLTP5
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PSEXDT
     C*
     C                   Z-ADD     SCRWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PSRWDT
     C*
     C                   Z-ADD     SCFIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PSFIDT
     C*
     C                   Z-ADD     SCMLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PSMLDT
     C*
     C                   Z-ADD     SCCFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SCCFDT
     C*
     C                   Z-ADD     SCDNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PSDNDT
     C*
     C                   MOVEL(P)  SCCOM1        PSCOM1
     C                   MOVEL(P)  SCCOM2        PSCOM2
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
     C*
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C     @@RRN2        CHAIN     WMAPP                              60
     C*
     C                   Z-ADD     APCO#         ##CO#
     C                   Z-ADD     APFEIN        SCFEIN
     C                   Z-ADD     APMOD         SCMOD
     C                   Z-ADD     APPROD        SCPROD
     C*
     C     APLSN         CAT(P)    ',':0         SCPNAM
     C     SCPNAM        CAT       APFSN:1       SCPNAM
     C*
     C     AGKY          CHAIN     WMAGL                              60
     C*
     C                   MOVEL(P)  AGNAME        SCANAM
     C*
     C     ##CO#         CHAIN     PCO01                              60
     C*
B02  C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        SCCONM
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C*
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  PGMOTH
     C*
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL(P)  Q1USER        PSIUSR
     C                   MOVEL(P)  Q1PGM         PSIPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PSIDT
     C*
     C                   Z-ADD     DSTIME        PSITM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMPSP
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  WRITER
