     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0400R02 - LOSS CONTROL TRACKING MAINTENANCE
     H*****************************************************************
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0400R - WORK WITH LOSS CONTROL TRACKING                   *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 12/03/98  PROGRAMMER: C:90  PROJECT: 102AE          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/03/05 C:15  Support: WC102/49687                           *
     H*                Added code to delete associated WMVSP records  *
     H*                a WMLCP record is deleted.                     *
     H*                                                               *
     H* 10/26/00 C:61  PROJECT 102BR                                  *
     H*              - Add field 'Number of operations'.              *
     H*              - Default 'Number of operations' - same as most  *
     H*                recent visit.                                  *
     H*                                                               *
     H*  05/14/99 C:113 PROJECT: 102BF                                *
     H*                 Changed MVSCDB to correctly load the date on  *
     H*                 the screen instead of the current date.       *
     H*                                                               *
     H*  04/27/99  C:33  PROJECT: 102AE                               *
     H*                 -TURN ON *IN90 WHEN RECORD IS DUPLICATED      *
     H*                 -ONLY CHECK FOR DUPLICATION IN -ADD- MODE     *
     H*                 -USE *IN40 FOR ADD MODE                       *
     H*                 -CHK FOR RECORD BEING UPDATED BY ANOTHER USER *
     H*                 -REMOVE *IN10 USAGE                           *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - DISPLAY MODE                                            *
     H*                                                               *
     H*  ERROR:                                                       *
     H*  80 - (WCR0500) INVALID DATE ENTERED                          *
     H*  81 - (WCR0505) RECORD NOT FOUND FOR THIS INFORMATION         *
     H*  82 - (WCR0505) RECORD NOT FOUND FOR THIS INFORMATION         *
     H*  83 - (WCR1058) CURSOR NOT ON PROPER FIELD FOR SEARCH         *
     H*  90 - GENERAL INDICATOR                                       *
     H*                                                               *
     H*  POSITION CURSOR:                                             *
     H*  70 - STAFF CODE                                              *
     H*  71 - EMPLOYER #                                              *
     H*  72 - COMMENT 1                                               *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*                                                               *
     H*****************************************************************
     FW0400F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMLCP     UF A E             DISK
     F                                     RENAME(PLC01:DBRCDF)
     F                                     INFDS(DBFDS2)
     FWMLCL2    IF   E           K DISK
     F*
     F*  Employer master file
     FWMEMP     IF   E             DISK
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
     F*
     F*  Workers Comp Parameter File
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP02)
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP03)
     F                                     INFDS(DBFDSF)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F*** Begin add *** 11/03/05 **************************************
     FWMVSL     UF   E           K DISK
     F*** End add ***** 11/03/05 **************************************
      /COPY SCOPYMT01D
     D*
     D DBFDSF          DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D***PY SCOPYMT01I
     D*
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCFND                  1      3  0
     D  QCEMP#                 4     12  0
     D  QCDIV                 13     17  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Loss Control Trackin-
     D                                     g')
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
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXVSDT            7 0
     C                   KFLD                    SCSTF
     C*
     C*  USER PROFILE KEY
     C*
     C     UPKY          KLIST
     C                   KFLD                    SCSTF
     C*
     C*  EMPLOYER MASTER KEY
     C*
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
      *
      ***** BEG ADD ******* 10/26/00 **********************************
      *  Loss Control
      *
     C     LCKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      ***** END ADD ******* 10/26/00 **********************************
      *
     C*** Begin add *** 11/03/05 **************************************
      *  Key list for the Visit File
      *
     C     VSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    LCEMP#
     C                   KFLD                    LCDIV
     C                   KFLD                    LCVSDT
     C                   KFLD                    LCSTF
     C*** End add ***** 11/03/05 **************************************
      *
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
      ***** BEG ADD ******* 10/26/00 **********************************
     C     *LIKE         DEFINE    SCVSDT        SVVSDT
     C     *LIKE         DEFINE    SCSTF         SVSTF
     C     *LIKE         DEFINE    SCEMP#        SVEMP#
     C     *LIKE         DEFINE    SCDIV         SVDIV
      ***** END ADD ******* 10/26/00 **********************************
     C*
     C*
     C*
     C     UPRMPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      3             ##VIEW            1 0
      *
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    ##CO#         SVCO#
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C*
     C*** Begin add *** 11/03/05 **************************************
     C     VSKY          CHAIN     PVS01                              99
     C     *IN99         DOWEQ     '0'
     C                   DELETE    PVS01
     C     VSKY          READE     PVS01                                  99
     C                   ENDDO
     C*** End add ***** 11/03/05 **************************************
     C*
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
     C***  BEGIN ADD  ***  04/27/99  *********************************
     C     *IN40         IFEQ      *ON
     C***  END   ADD  ***  04/27/99  *********************************
      *
      *  EDIT VISIT DATE
      *
     C                   Z-ADD     SCVSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCVSDT
      *
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        80  90
     C                   ENDIF                                                  SRERR=*ON
      *
      *  VALIDATE POLICY SERVICES STAFF CODE, IF ENTERED
      *
      ***** BEG ADD ******* 10/26/00 **********************************
     C     *IN90         IFEQ      *OFF
      ***** END ADD ******* 10/26/00 **********************************
     C     SCSTF         CHAIN     PUP03                              60
     C   60              SETON                                        817090
      ***** BEG ADD ******* 10/26/00 **********************************
     C                   ENDIF
      ***** END ADD ******* 10/26/00 **********************************
     C*
     C*  VALIDATE EMPLOYER / DIVISION
     C*
      ***** BEG ADD ******* 10/26/00 **********************************
     C     *IN90         IFEQ      *OFF
      ***** END ADD ******* 10/26/00 **********************************
     C     EMKY          CHAIN     PEM02                              60
     C   60              SETON                                        827090
      ***** BEG ADD ******* 10/26/00 **********************************
     C                   ENDIF                                                  *IN90 = *OFF
      *
      * Default 'Number of Operations' - same as most recent visit
      *
     C     *IN90         IFEQ      *OFF
     C     SCOPER        ANDEQ     *ZERO
      * ZZZ
     C     SCVSDT        IFNE      SVVSDT
     C     SCSTF         ORNE      SVSTF
     C     SCEMP#        ORNE      SVEMP#
     C     SCDIV         ORNE      SVDIV
      *
     C     LCKY          SETGT     PLC01
     C     LCKY          READPE    PLC01                                  60
     C  N60              Z-ADD     LCOPER        SCOPER
     C                   SETON                                        90  73
      *
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN90 = *OFF
      *
      ***** END ADD ******* 10/26/00 **********************************
     C*
     C*  VALIDATE ENTRY
     C*
      ***** BEG ADD ******* 10/26/00 **********************************
     C     *IN90         IFEQ      *OFF
      ***** END ADD ******* 10/26/00 **********************************
     C                   Z-ADD     SCVSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXVSDT
     C*
     C     KEY1          CHAIN     PLC01                              60
     C     *IN60         IFEQ      *OFF
     C***  BEGIN CHG  ***  04/27/99  *********************************
     C**********           MOVE *ON       *IN84
     C                   SETON                                        84  90
     C***  END   CHG  ***  04/27/99  *********************************
     C                   ENDIF
      ***** BEG ADD ******* 10/26/00 **********************************
     C                   ENDIF
      ***** END ADD ******* 10/26/00 **********************************
     C*
     C***  BEGIN ADD  ***  04/27/99  *********************************
     C                   ENDIF                                                  *IN40=*ON
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90LCDT          IFGT      SAVEDT
     C     LCDT          OREQ      SAVEDT
     C     LCTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  LCDT>SAVEDT
     C***  END   ADD  ***  04/27/99  *********************************
      *
      ***** BEG ADD ******* 10/26/00 **********************************
     C                   Z-ADD     SCVSDT        SVVSDT
     C                   MOVE      SCSTF         SVSTF
     C                   Z-ADD     SCEMP#        SVEMP#
     C                   Z-ADD     SCDIV         SVDIV
      ***** END ADD ******* 10/26/00 **********************************
     C                   ENDSR                                                  ERRCK1
      *
      *
      *
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
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     KJDO          BEGSR
     C*
     C                   SELECT
      ***** BEG DEL ******* 10/26/00 **********************************
      *********
     C*********
     C***EMPLOYER/DIV SEARCH
     C*********
     C*********  HFLD      WHEQ 'SCEMP#'
     C*********  HFLD      OREQ 'SCDIV'
     C*********
     C*****BEGIN ADD  ***  04/27/99  *********************************
     C*********  *IN40     IFEQ *ON
     C*****END*  ADD  ***  04/27/99  *********************************
     C*********
     C*********            CALL 'W0050R'
     C*********  *INKC     PARM           @@KC    1
     C*********  *INKL     PARM           @@KL    1
     C*********            PARM '2'       @@CALL  1
     C*********            PARM           @@RRN   90
     C*********            PARM DBRRN3    @@RRN2  90
     C*********            PARM           @@QTE   1
     C*********            PARM 1         @@VIEW  10
     C*********            PARM           @@RRN3  90
     C*********
     C*********  *INKL     IFEQ *OFF
     C*********  @@RRN     ANDNE*ZEROS
     C*********
     C*********  @@RRN     CHAINPEM01                60
     C*********
     C*********  *IN60     IFEQ *OFF
     C*********            MOVE EMEMP#    SCEMP#
     C*********            MOVE EMDIV     SCDIV
     C*********            MOVE EMNAME    SCEMPN
     C*********            MOVE *ON       *IN72            SCCOM POSITION
     C*********            ENDIF                           *IN60=OFF
     C*********
     C*********            ENDIF                           *INKL=OFF,@@RRN
     C*********
     C*********  *INKL     IFEQ *ON
     C*********  @@RRN     OREQ *ZEROS
     C*********  *IN60     OREQ *ON
     C*********            MOVE *ON       *IN71            SCEMP POSITION
     C*********            ENDIF                           *INKL=ON,@@RRN
     C*********
     C*****BEGIN ADD  ***  04/27/99  *********************************
     C*********            ENDIF                           *IN40=*ON
     C*****END*  ADD  ***  04/27/99  *********************************
      ***** END DEL ******* 10/26/00 **********************************
      ***** BEG ADD ******* 10/26/00 **********************************
      *
      *  EMPLOYER/DIV SEARCH
      *
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C     *IN40         ANDEQ     *ON
      *
     C                   CALL      'W0050R'
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM      DBRRN3        ##RRN2            9 0
     C                   PARM                    ##QTE             1
     C                   PARM      1             ##VIEW            1 0
     C                   PARM                    ##RRN3            9 0
      *
     C     ##RRN         IFEQ      *ZEROS
     C                   MOVE      *ON           *IN71                          SCEMP POSITION
      *
     C                   ELSE
      *
     C     ##RRN         CHAIN     PEM01                              60
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   MOVEL(P)  EMNAME        SCEMPN
     C                   MOVE      *ON           *IN73                          SCCOM POSITION
      *
      * Default 'Number of Operations' - same as most recent visit
      *
     C     LCKY          SETGT     PLC01
     C     LCKY          READPE    PLC01                                  60
      *
     C                   Z-ADD     SCVSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCVSDT
      *
     C  N60              Z-ADD     LCOPER        SCOPER
      *
     C  N60              Z-ADD     SCVSDT        SVVSDT
     C  N60              MOVE      SCSTF         SVSTF
     C  N60              Z-ADD     SCEMP#        SVEMP#
     C  N60              Z-ADD     SCDIV         SVDIV
     C                   ENDIF                                                  ##RRN
      *
      ***** END ADD ******* 10/26/00 **********************************
     C*
     C*
      *  STAFF CODE SEARCH
      *
     C     HFLD          WHENEQ    'SCSTF'
     C*
     C***  BEGIN ADD  ***  04/27/99  *********************************
     C     *IN40         IFEQ      *ON
     C***  END   ADD  ***  04/27/99  *********************************
      *
     C                   CALL      'S0005R'      UPRMPL                 99
      *
     C     ##RRN         IFNE      *ZEROS
      *
     C     ##RRN         CHAIN     PUP02                              99
      *
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     UPSTAF        SCSTF
     C                   MOVEL     UPUSNM        SCSTFN
     C                   MOVE      *ON           *IN71
     C                   ENDIF                                                  *IN99 = OFF
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN70
     C*
     C                   ENDIF                                                  ##STAF<>*BLANKS
     C*
     C***  BEGIN ADD  ***  04/27/99  *********************************
     C                   ENDIF                                                  *IN40=*ON
     C***  END   ADD  ***  04/27/99  *********************************
     C*
     C     HFLD          WHENNE    'SCEMP#'
     C     HFLD          ORNE      'SCDIV'
     C     HFLD          ORNE      'SCSTF'
     C                   MOVE      *ON           *IN83
      *
     C                   ENDSL
     C*
     C                   Z-ADD     SCVSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C***  BEGIN ADD  ***  04/27/99  *********************************
     C     SRERR         IFEQ      *OFF
     C***  END   ADD  ***  04/27/99  *********************************
     C                   MOVEL     SRDTO         SCVSDT
     C***  BEGIN ADD  ***  04/27/99  *********************************
     C                   ENDIF                                                  SRERR=*OFF
     C***  END   ADD  ***  04/27/99  *********************************
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14
     C*===============================================================
     C*
     C*  F14=DARTS
     C*
     C     KNDO          BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   CLEAR                   QCKEY
     C*
     C                   Z-ADD     SCFND         QCFND
     C                   Z-ADD     SCEMP#        QCEMP#
     C                   Z-ADD     SCDIV         QCDIV
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0400R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C***  BEGIN DEL  ***  04/27/99  *********************************
     C********** @@MODE    IFEQ 'C'
     C**********           MOVE *ON       *IN10
     C**********           ENDIF
     C***  END   DEL  ***  04/27/99  *********************************
     C*
     C                   CLEAR                   SCSTFN
     C                   CLEAR                   SCEMPN
     C*
     C                   Z-ADD     LCVSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCVSDT
     C*
     C                   MOVEL     LCSTF         SCSTF
     C     UPKY          CHAIN     PUP03                              60
     C  N60              MOVEL     UPUSNM        SCSTFN
     C*
     C                   MOVEL     LCEMP#        SCEMP#
     C                   MOVEL     LCDIV         SCDIV
     C     EMKY          CHAIN     PEM02                              60
     C  N60              MOVEL     EMNAME        SCEMPN
     C*
      ***** BEG ADD ******* 10/26/00 **********************************
     C                   Z-ADD     LCOPER        SCOPER
      ***** END ADD ******* 10/26/00 **********************************
     C                   MOVEL     LCCOM1        SCCOM1
     C                   MOVEL     LCCOM2        SCCOM2
     C                   MOVEL     LCCOM3        SCCOM3
     C*
     C                   ENDSR                                                  MVDBSC
      *
      *
      *
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     **** BEGIN CHG *** 05/14/99 **************************************
     C************         Z-ADDSCVSDT    SCVSDT
     C                   Z-ADD     SCVSDT        SRDTI
     **** BEGIN CHG *** 05/14/99 **************************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LCVSDT
     C*
     C                   MOVEL     SVCO#         LCCO#
     C                   MOVEL     SCSTF         LCSTF
     C                   MOVEL     SCFND         LCFND
     C                   MOVEL     SCEMP#        LCEMP#
     C                   MOVEL     SCDIV         LCDIV
      ***** BEG ADD ******* 10/26/00 **********************************
     C                   Z-ADD     SCOPER        LCOPER
      ***** END ADD ******* 10/26/00 **********************************
     C                   MOVEL     SCCOM1        LCCOM1
     C                   MOVEL     SCCOM2        LCCOM2
     C                   MOVEL     SCCOM3        LCCOM3
     C*
     C                   MOVE      *BLANKS       SCSTFN
     C                   MOVE      *BLANKS       SCEMPN
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
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKN         CASEQ     *ON           KNDO                     90
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
      ***** BEG ADD ******* 10/26/00 **********************************
     C                   Z-ADD     *ZERO         SVVSDT
     C                   MOVE      *BLANKS       SVSTF
     C                   Z-ADD     *ZERO         SVEMP#
     C                   Z-ADD     *ZERO         SVDIV
      ***** END ADD ******* 10/26/00 **********************************
     C*
     C     @@MODE        IFEQ      'A'
     C     *INKC         ANDEQ     *OFF
     C     *INKL         ANDEQ     *OFF
     C*
     C                   CALL      'W0401R02'                           99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@MODE        ##MODE            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      DBRRN         ##RRN2            9 0
     C                   PARM      Q1PGM         ##PGM            10
     C*
     C                   ENDIF                                                  @@MODE=A
     C*
     C                   ENDSR
      *
      *
      *
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C***  BEGIN ADD  ***  04/27/99  *********************************
     C     @@MODE        COMP      'A'                                    40
     C     @@MODE        COMP      'D'                                    09
     C***  END   ADD  ***  04/27/99  *********************************
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C***  BEGIN DEL  ***  04/27/99  *********************************
     C**********           MOVE *OFF      *IN09
     C**********           MOVE *OFF      *IN10
     C***  END   DEL  ***  04/27/99  *********************************
     C*
     C                   MOVE      ##CO#         SVCO#
     C                   MOVE      ##FND         SCFND
     C                   MOVEL     ##FNAM        SCFNDN
     C***  BEGIN DEL  ***  04/27/99  *********************************
     C**********
     C***DISPLAY MODE
     C**********
     C********** @@MODE    IFEQ 'D'
     C**********           MOVE *ON       *IN09
     C**********           MOVE *ON       *IN10
     C**********           ENDIF
     C***  END   DEL  ***  04/27/99  *********************************
     C*
     C*  ADD MODE
     C*
     C     @@MODE        IFEQ      'A'
     C                   CLEAR                   SCEMP#
     C                   CLEAR                   SCDIV
     C                   CLEAR                   SCEMPN
     C                   CLEAR                   SCSTF
     C                   CLEAR                   SCSTFN
     C                   ENDIF
     C*
      ***** BEG ADD ******* 10/26/00 **********************************
     C                   Z-ADD     *ZERO         SVVSDT
     C                   MOVE      *BLANKS       SVSTF
     C                   Z-ADD     *ZERO         SVEMP#
     C                   Z-ADD     *ZERO         SVDIV
      ***** END ADD ******* 10/26/00 **********************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*
     C*
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        LCUSER
     C                   MOVEL     Q1PGM         LCPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LCDT
     C                   MOVEL     DSTIME        LCTIME
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
     C                   MOVEL     Q1USER        LCUSER
     C                   MOVEL     Q1PGM         LCPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LCDT
     C                   MOVEL     DSTIME        LCTIME
     C*
     C                   MOVE      LCUSER        LCIUSR
     C                   MOVE      LCPGM         LCIPGM
     C                   MOVE      LCDT          LCIDT
     C                   MOVE      LCTIME        LCITM
     C*
     C                   WRITE     DBRCDF
     C*
     C     @@MODE        IFEQ      'A'
     C                   CALL      'W0401R'
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM      DBRRN2        @@RRN             9 0
     C                   ENDIF
     C*
     C                   FEOD      WMLCP
     C                   ENDSR
