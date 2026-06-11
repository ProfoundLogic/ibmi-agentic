     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE W0089R - ENDORS/DARTS CONTROL MAINTENANCE
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW THE USER TO SETUP      *
     H*                 VARIOUS ENDORSEMENT DARTS COMMENT CODES.      *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0010R   - WORK WITH GROUPS                                *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 01/27/98  PROGRAMMER: C:57  PROJECT: CM027          *
     H*****************************************************************
     H*  REVISIONS:
     H*                                                              *
     H*  05/31/07  C:33  Seq#: WA135/58564                           *
     H*                  Change usage of *IN6* for PC to *IN5*       *
     H*                                                              *
     H*  10/11/06  C:33  Seq#: WA099/41801                           *
     H*                  Use SC305R instead of SC580R.               *
     H*
     H*  04/26/06 C:61 PROJECT: 132WFI
     H*                Add Waiver DARTS codes to third screen
     H*                                                               *
     H*  04/10/98 C:90 PROJECT: WC577                                 *
     H*                FIXED CURSOR PLACEMENT ON F10 SEARCH           *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  XX - (WCRXXXX) XXX                                           *
     H*  POSITION CURSOR:                                             *
     H*  SCR02:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*  SCR03:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*****************************************************************
     FW0089F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMDXP     UF A E             DISK
     F                                     RENAME(PDX01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMDXL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     FWMFNP     IF   E             DISK
     FSTCTL1    IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D OPTNM           C                   CONST('Endors/DARTS Contro-
     D                                     l Information')
     D #OFSCR          C                   CONST(3)
     D COMAPI          C                   CONST('WA001R')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*
     C     OFFPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##OFF             5
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  KEY LIST FOR THE ADMIN/DARTS CONTROL FILE
     C*
     C     DXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    ##RRN             9 0
     C*
     C/COPY SCOPYMT01C
     C*
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*  Employer Name Change
     C*
     C     *IN90         IFEQ      *OFF
     C     SCEMNC        ANDNE     *BLANKS
     C     SCEMNC        CHAIN     PCT01                              71
     C*
     C     *IN71         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        71  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Employer Mailing Addr Change
     C*
     C     *IN90         IFEQ      *OFF
     C     SCEMMA        ANDNE     *BLANKS
     C     SCEMMA        CHAIN     PCT01                              72
     C*
     C     *IN72         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        72  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Employer Physical Addr Change
     C*
     C     *IN90         IFEQ      *OFF
     C     SCEMPA        ANDNE     *BLANKS
     C     SCEMPA        CHAIN     PCT01                              73
     C*
     C     *IN73         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        73  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Federal ID Change
     C*
     C     *IN90         IFEQ      *OFF
     C     SCEMFI        ANDNE     *BLANKS
     C     SCEMFI        CHAIN     PCT01                              74
     C*
     C     *IN74         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        74  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Producer (Agent) Name Change
     C*
     C     *IN90         IFEQ      *OFF
     C     SCPANC        ANDNE     *BLANKS
     C     SCPANC        CHAIN     PCT01                              75
     C*
     C     *IN75         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        75  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Legal Status Change
     C*
     C     *IN90         IFEQ      *OFF
     C     SCEMLS        ANDNE     *BLANKS
     C     SCEMLS        CHAIN     PCT01                              76
     C*
     C     *IN76         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        76  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Addition of Division
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDVAD        ANDNE     *BLANKS
     C     SCDVAD        CHAIN     PCT01                              77
     C*
     C     *IN77         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        77  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  DELetion of Division
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDVDL        ANDNE     *BLANKS
     C     SCDVDL        CHAIN     PCT01                              78
     C*
     C     *IN78         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        78  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Experience Modifier Change
     C*
     C     *IN90         IFEQ      *OFF
     C     SCPEXM        ANDNE     *BLANKS
     C     SCPEXM        CHAIN     PCT01                              79
     C*
     C     *IN79         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        79  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Increased Limits Change
     C*
     C     *IN90         IFEQ      *OFF
     C     SCPINL        ANDNE     *BLANKS
     C     SCPINL        CHAIN     PCT01                              80
     C*
     C     *IN80         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        80  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Class Rate / Other Change
     C*
     C     *IN90         IFEQ      *OFF
     C     SCPCRO        ANDNE     *BLANKS
     C     SCPCRO        CHAIN     PCT01                              81
     C*
     C     *IN81         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        81  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Interim Adjust Of Premium
     C*
     C     *IN90         IFEQ      *OFF
     C     SCPIAP        ANDNE     *BLANKS
     C     SCPIAP        CHAIN     PCT01                              82
     C*
     C     *IN82         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        82  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Contingent Modifier Change
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCEXM        ANDNE     *BLANKS
     C     SCCEXM        CHAIN     PCT01                              83
     C*
     C     *IN83         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        83  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90DXDT          IFGT      SAVEDT
     C     DXDT          OREQ      SAVEDT
     C     DXTIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  F2DT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C*  Addition of Entity
     C*
     C     *IN90         IFEQ      *OFF
     C     SCENAD        ANDNE     *BLANKS
     C     SCENAD        CHAIN     PCT01                              71
     C*
     C     *IN71         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        71  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Deletion of Entity
     C*
     C     *IN90         IFEQ      *OFF
     C     SCENDL        ANDNE     *BLANKS
     C     SCENDL        CHAIN     PCT01                              72
     C*
     C     *IN72         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        72  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Entity Name Change
     C*
     C     *IN90         IFEQ      *OFF
     C     SCENNC        ANDNE     *BLANKS
     C     SCENNC        CHAIN     PCT01                              73
     C*
     C     *IN73         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        73  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Entity Mailing Adr Change
     C*
     C     *IN90         IFEQ      *OFF
     C     SCENMA        ANDNE     *BLANKS
     C     SCENMA        CHAIN     PCT01                              74
     C*
     C     *IN74         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        74  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Entity Physical Addr Change
     C*
     C     *IN90         IFEQ      *OFF
     C     SCENPA        ANDNE     *BLANKS
     C     SCENPA        CHAIN     PCT01                              75
     C*
     C     *IN75         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        75  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Addition of Location
     C*
     C     *IN90         IFEQ      *OFF
     C     SCLCAD        ANDNE     *BLANKS
     C     SCLCAD        CHAIN     PCT01                              76
     C*
     C     *IN76         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        76  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Deletion of Location
     C*
     C     *IN90         IFEQ      *OFF
     C     SCLCDL        ANDNE     *BLANKS
     C     SCLCDL        CHAIN     PCT01                              77
     C*
     C     *IN77         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        77  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Location Name Change
     C*
     C     *IN90         IFEQ      *OFF
     C     SCLCNC        ANDNE     *BLANKS
     C     SCLCNC        CHAIN     PCT01                              78
     C*
     C     *IN78         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        78  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Location Mailing Addr Change
     C*
     C     *IN90         IFEQ      *OFF
     C     SCLCMA        ANDNE     *BLANKS
     C     SCLCMA        CHAIN     PCT01                              79
     C*
     C     *IN79         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        79  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Location Physical Addr Change
     C*
     C     *IN90         IFEQ      *OFF
     C     SCLCPA        ANDNE     *BLANKS
     C     SCLCPA        CHAIN     PCT01                              80
     C*
     C     *IN80         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        80  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90DXDT          IFGT      SAVEDT
     C     DXDT          OREQ      SAVEDT
     C     DXTIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  F2DT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
     C*
     C*  Addition of Owner / Officer
     C*
     C     *IN90         IFEQ      *OFF
     C     SCOOAD        ANDNE     *BLANKS
     C     SCOOAD        CHAIN     PCT01                              71
     C*
     C     *IN71         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        71  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Nominal / Ownership Change
     C*
     C     *IN90         IFEQ      *OFF
     C     SCOONC        ANDNE     *BLANKS
     C     SCOONC        CHAIN     PCT01                              72
     C*
     C     *IN72         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        72  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
      ***** BEG ADD ******* 04/26/06 **********************************
      *
      *  ADD of Waiver
      *
     C  N90SCWVAR        IFNE      *BLANKS
     C     SCWVAR        CHAIN     PCT01                              60
      *
     C     *IN60         IFEQ      *ON
     C     CTSTAT        OREQ      'I'
     C                   SETON                                        82  90
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN90 IFEQ
      *
      *  CHG of Waiver
      *
     C  N90SCWVCR        IFNE      *BLANKS
     C     SCWVCR        CHAIN     PCT01                              60
      *
     C     *IN60         IFEQ      *ON
     C     CTSTAT        OREQ      'I'
     C                   SETON                                        83  90
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN90 IFEQ
      *
      *  DLT of Waiver
      *
     C  N90SCWVDR        IFNE      *BLANKS
     C     SCWVDR        CHAIN     PCT01                              60
      *
     C     *IN60         IFEQ      *ON
     C     CTSTAT        OREQ      'I'
     C                   SETON                                        84  90
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN90 IFEQ
      *
      ***** END ADD ******* 04/26/06 **********************************
     C*
     C*  Certificate Of Insurance
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCERT        ANDNE     *BLANKS
     C     SCCERT        CHAIN     PCT01                              73
     C*
     C     *IN73         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        73  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  Miscellaneous
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMISC        ANDNE     *BLANKS
     C     SCMISC        CHAIN     PCT01                              74
     C*
     C     *IN74         IFEQ      *ON
     C                   SETON                                            90
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        74  90
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90DXDT          IFGT      SAVEDT
     C     DXDT          OREQ      SAVEDT
     C     DXTIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  F2DT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK3
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10 = SEARCH
     C*===============================================================
     C*
     C     KJDO          BEGSR
      ***** BEG ADD ******* 04/26/06 **********************************
      *
      *  ADD Waiver
      *
     C     HFLD          IFEQ      'SCWVAR'
     C***  Begin Chg  ***  10/11/06  **************************************
     C**********           CALL 'SC580R'
     C**********           PARM *BLANKS   ##RMCD  5
     C**********           PARM           ##RTC   1
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C***  End   Chg  ***  10/11/06  **************************************
      *
B2   C     ##RMCD        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN79
      *
     C                   ELSE
      *
     C                   MOVEL     ##RMCD        SCWVAR
     C                   MOVE      *ON           *IN80
     C                   ENDIF
      *
     C                   ENDIF
      *
      *  CHG Waiver
      *
     C     HFLD          IFEQ      'SCWVCR'
     C***  Begin Chg  ***  10/11/06  **************************************
     C**********           CALL 'SC580R'
     C**********           PARM *BLANKS   ##RMCD  5
     C**********           PARM           ##RTC   1
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C***  End   Chg  ***  10/11/06  **************************************
      *
B2   C     ##RMCD        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN80
      *
     C                   ELSE
      *
     C                   MOVEL     ##RMCD        SCWVCR
     C                   MOVE      *ON           *IN81
     C                   ENDIF
      *
     C                   ENDIF
      *
      *  DLT Waiver
      *
     C     HFLD          IFEQ      'SCWVDR'
     C***  Begin Chg  ***  10/11/06  **************************************
     C**********           CALL 'SC580R'
     C**********           PARM *BLANKS   ##RMCD  5
     C**********           PARM           ##RTC   1
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C***  End   Chg  ***  10/11/06  **************************************
      *
B2   C     ##RMCD        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN81
      *
     C                   ELSE
      *
     C                   MOVEL     ##RMCD        SCWVDR
     C                   MOVE      *ON           *IN76
     C                   ENDIF
      *
     C                   ENDIF
      *
      ***** END ADD ******* 04/26/06 **********************************
     C*
     C* DARTS REMARK CODE SEARCH
     C*
     C     HFLD          IFEQ      'SCEMNC'
     C     HFLD          OREQ      'SCEMMA'
     C     HFLD          OREQ      'SCEMPA'
     C     HFLD          OREQ      'SCEMFI'
     C     HFLD          OREQ      'SCPANC'
     C     HFLD          OREQ      'SCEMLS'
     C     HFLD          OREQ      'SCDVAD'
     C     HFLD          OREQ      'SCDVDL'
     C     HFLD          OREQ      'SCPEXM'
     C     HFLD          OREQ      'SCPINL'
     C     HFLD          OREQ      'SCPCRO'
     C     HFLD          OREQ      'SCPIAP'
     C     HFLD          OREQ      'SCCEXM'
     C     HFLD          OREQ      'SCENAD'
     C     HFLD          OREQ      'SCENDL'
     C     HFLD          OREQ      'SCENNC'
     C     HFLD          OREQ      'SCENMA'
     C     HFLD          OREQ      'SCENPA'
     C     HFLD          OREQ      'SCLCAD'
     C     HFLD          OREQ      'SCLCDL'
     C     HFLD          OREQ      'SCLCNC'
     C     HFLD          OREQ      'SCLCMA'
     C     HFLD          OREQ      'SCLCPA'
     C     HFLD          OREQ      'SCOOAD'
     C     HFLD          OREQ      'SCOONC'
     C     HFLD          OREQ      'SCCERT'
     C     HFLD          OREQ      'SCMISC'
     C*
     C***  Begin Chg  ***  10/11/06  **************************************
     C**********           CALL 'SC580R'
     C**********           PARM *BLANKS   QQRMCD  5
     C**********           PARM           QQRTC   1
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C     QQRMCD        PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     *LIKE         DEFINE    ##RMCD        QQRMCD
     C***  End   Chg  ***  10/11/06  **************************************
     C*
B2   C     QQRMCD        IFNE      *BLANKS
     C*
B3   C                   SELECT
      **** BEGIN DEL **** 04/10/98 ************************************
     C***********HFLD      WHEQ 'SCEMNC'
     C******************** MOVELQQRMCD    SCEMNC
     C******************** MOVE *ON       *IN84
     C***********HFLD      WHEQ 'SCEMMA'
     C******************** MOVELQQRMCD    SCEMMA
     C******************** MOVE *ON       *IN85
     C***********HFLD      WHEQ 'SCEMPA'
     C******************** MOVELQQRMCD    SCEMPA
     C******************** MOVE *ON       *IN86
     C***********HFLD      WHEQ 'SCEMFI'
     C******************** MOVELQQRMCD    SCEMFI
     C******************** MOVE *ON       *IN87
     C***********HFLD      WHEQ 'SCPANC'
     C******************** MOVELQQRMCD    SCPANC
     C******************** MOVE *ON       *IN88
     C***********HFLD      WHEQ 'SCEMLS'
     C******************** MOVELQQRMCD    SCEMLS
     C******************** MOVE *ON       *IN89
     C***********HFLD      WHEQ 'SCDVAD'
     C******************** MOVELQQRMCD    SCDVAD
     C******************** MOVE *ON       *IN21
      **** END   DEL **** 04/10/98 ************************************
      *
      **** BEGIN ADD **** 04/10/98 ************************************
     C***  Begin Chg  ***  05/31/07  **************************************
     C********** HFLD      WHEQ 'SCEMNC'
     C**********           MOVELQQRMCD    SCEMNC
     C**********           MOVE *ON       *IN60
     C********** HFLD      WHEQ 'SCEMMA'
     C**********           MOVELQQRMCD    SCEMMA
     C**********           MOVE *ON       *IN61
     C********** HFLD      WHEQ 'SCEMPA'
     C**********           MOVELQQRMCD    SCEMPA
     C**********           MOVE *ON       *IN62
     C********** HFLD      WHEQ 'SCEMFI'
     C**********           MOVELQQRMCD    SCEMFI
     C**********           MOVE *ON       *IN63
     C********** HFLD      WHEQ 'SCPANC'
     C**********           MOVELQQRMCD    SCPANC
     C**********           MOVE *ON       *IN64
     C********** HFLD      WHEQ 'SCEMLS'
     C**********           MOVELQQRMCD    SCEMLS
     C**********           MOVE *ON       *IN65
     C********** HFLD      WHEQ 'SCDVAD'
     C**********           MOVELQQRMCD    SCDVAD
     C**********           MOVE *ON       *IN66
     C     HFLD          WHENEQ    'SCEMNC'
     C                   MOVEL     QQRMCD        SCEMNC
     C                   MOVE      *ON           *IN50
     C     HFLD          WHENEQ    'SCEMMA'
     C                   MOVEL     QQRMCD        SCEMMA
     C                   MOVE      *ON           *IN51
     C     HFLD          WHENEQ    'SCEMPA'
     C                   MOVEL     QQRMCD        SCEMPA
     C                   MOVE      *ON           *IN52
     C     HFLD          WHENEQ    'SCEMFI'
     C                   MOVEL     QQRMCD        SCEMFI
     C                   MOVE      *ON           *IN53
     C     HFLD          WHENEQ    'SCPANC'
     C                   MOVEL     QQRMCD        SCPANC
     C                   MOVE      *ON           *IN54
     C     HFLD          WHENEQ    'SCEMLS'
     C                   MOVEL     QQRMCD        SCEMLS
     C                   MOVE      *ON           *IN55
     C     HFLD          WHENEQ    'SCDVAD'
     C                   MOVEL     QQRMCD        SCDVAD
     C                   MOVE      *ON           *IN56
     C***  End   Chg  ***  05/31/07  **************************************
      **** END   ADD **** 04/10/98 ************************************
     C     HFLD          WHENEQ    'SCDVDL'
     C                   MOVEL     QQRMCD        SCDVDL
     C                   MOVE      *ON           *IN22
     C     HFLD          WHENEQ    'SCPEXM'
     C                   MOVEL     QQRMCD        SCPEXM
     C                   MOVE      *ON           *IN23
     C     HFLD          WHENEQ    'SCPINL'
     C                   MOVEL     QQRMCD        SCPINL
     C                   MOVE      *ON           *IN24
     C     HFLD          WHENEQ    'SCPCRO'
     C                   MOVEL     QQRMCD        SCPCRO
     C                   MOVE      *ON           *IN25
     C     HFLD          WHENEQ    'SCPIAP'
     C                   MOVEL     QQRMCD        SCPIAP
     C                   MOVE      *ON           *IN26
     C     HFLD          WHENEQ    'SCCEXM'
     C                   MOVEL     QQRMCD        SCCEXM
     C                   MOVE      *ON           *IN27
     C     HFLD          WHENEQ    'SCENAD'
     C                   MOVEL     QQRMCD        SCENAD
     C                   MOVE      *ON           *IN81
     C     HFLD          WHENEQ    'SCENDL'
     C                   MOVEL     QQRMCD        SCENDL
     C                   MOVE      *ON           *IN82
     C     HFLD          WHENEQ    'SCENNC'
     C                   MOVEL     QQRMCD        SCENNC
     C                   MOVE      *ON           *IN83
     C     HFLD          WHENEQ    'SCENMA'
     C                   MOVEL     QQRMCD        SCENMA
     C                   MOVE      *ON           *IN84
     C     HFLD          WHENEQ    'SCENPA'
     C                   MOVEL     QQRMCD        SCENPA
     C                   MOVE      *ON           *IN85
     C     HFLD          WHENEQ    'SCLCAD'
     C                   MOVEL     QQRMCD        SCLCAD
     C                   MOVE      *ON           *IN86
     C     HFLD          WHENEQ    'SCLCDL'
     C                   MOVEL     QQRMCD        SCLCDL
     C                   MOVE      *ON           *IN87
     C     HFLD          WHENEQ    'SCLCNC'
     C                   MOVEL     QQRMCD        SCLCNC
     C                   MOVE      *ON           *IN88
     C     HFLD          WHENEQ    'SCLCMA'
     C                   MOVEL     QQRMCD        SCLCMA
     C                   MOVE      *ON           *IN89
     C     HFLD          WHENEQ    'SCLCPA'
     C                   MOVEL     QQRMCD        SCLCPA
     C                   MOVE      *ON           *IN21
     C     HFLD          WHENEQ    'SCOOAD'
     C                   MOVEL     QQRMCD        SCOOAD
     C                   MOVE      *ON           *IN75
     C     HFLD          WHENEQ    'SCOONC'
     C                   MOVEL     QQRMCD        SCOONC
      ***** BEG CHG ******* 04/26/06 **********************************
     C*********            MOVE *ON       *IN76
     C                   MOVE      *ON           *IN79
      ***** END CHG ******* 04/26/06 **********************************
     C     HFLD          WHENEQ    'SCCERT'
     C                   MOVEL     QQRMCD        SCCERT
     C                   MOVE      *ON           *IN77
     C     HFLD          WHENEQ    'SCMISC'
     C                   MOVEL     QQRMCD        SCMISC
     C                   MOVE      *ON           *IN78
E3   C                   ENDSL                                                  HFLD WHEQ
     C*
     C                   ELSE
      *
     C     HFLD          COMP      'SCEMNC'                               27
      **** BEGIN DEL **** 04/10/98 ************************************
     C***********HFLD      COMP 'SCEMMA'                 84
     C***********HFLD      COMP 'SCEMPA'                 85
     C***********HFLD      COMP 'SCEMFI'                 86
     C***********HFLD      COMP 'SCPANC'                 87
     C***********HFLD      COMP 'SCEMLS'                 88
     C***********HFLD      COMP 'SCDVAD'                 89
     C***********HFLD      COMP 'SCDVDL'                 21
      **** END   DEL **** 04/10/98 ************************************
      *
      **** BEGIN ADD **** 04/10/98 ************************************
     C***  Begin Chg  ***  05/31/07  **************************************
     C********** HFLD      COMP 'SCEMMA'                 60
     C********** HFLD      COMP 'SCEMPA'                 61
     C********** HFLD      COMP 'SCEMFI'                 62
     C********** HFLD      COMP 'SCPANC'                 63
     C********** HFLD      COMP 'SCEMLS'                 64
     C********** HFLD      COMP 'SCDVAD'                 65
     C********** HFLD      COMP 'SCDVDL'                 66
     C     HFLD          COMP      'SCEMMA'                               50
     C     HFLD          COMP      'SCEMPA'                               51
     C     HFLD          COMP      'SCEMFI'                               52
     C     HFLD          COMP      'SCPANC'                               53
     C     HFLD          COMP      'SCEMLS'                               54
     C     HFLD          COMP      'SCDVAD'                               55
     C     HFLD          COMP      'SCDVDL'                               56
     C***  End   Chg  ***  05/31/07  **************************************
      **** END   ADD **** 04/10/98 ************************************
     C     HFLD          COMP      'SCPEXM'                               22
     C     HFLD          COMP      'SCPINL'                               23
     C     HFLD          COMP      'SCPCRO'                               24
     C     HFLD          COMP      'SCPIAP'                               25
     C     HFLD          COMP      'SCCEXM'                               26
     C     HFLD          COMP      'SCENAD'                               21
     C     HFLD          COMP      'SCENDL'                               81
     C     HFLD          COMP      'SCENNC'                               82
     C     HFLD          COMP      'SCENMA'                               83
     C     HFLD          COMP      'SCENPA'                               84
     C     HFLD          COMP      'SCLCAD'                               85
     C     HFLD          COMP      'SCLCDL'                               86
     C     HFLD          COMP      'SCLCNC'                               87
     C     HFLD          COMP      'SCLCMA'                               88
     C     HFLD          COMP      'SCLCPA'                               89
     C     HFLD          COMP      'SCOOAD'                               78
     C     HFLD          COMP      'SCOONC'                               75
     C     HFLD          COMP      'SCCERT'                               76
     C     HFLD          COMP      'SCMISC'                               77
E2   C                   ENDIF                                                  QQRMCD IFNE
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     DXEMNC        SCEMNC
     C                   MOVEL     DXEMMA        SCEMMA
     C                   MOVEL     DXEMPA        SCEMPA
     C                   MOVEL     DXEMFI        SCEMFI
     C                   MOVEL     DXPANC        SCPANC
     C                   MOVEL     DXEMLS        SCEMLS
     C                   MOVEL     DXDVAD        SCDVAD
     C                   MOVEL     DXDVDL        SCDVDL
     C                   MOVEL     DXPEXM        SCPEXM
     C                   MOVEL     DXPINL        SCPINL
     C                   MOVEL     DXPCRO        SCPCRO
     C                   MOVEL     DXPIAP        SCPIAP
     C                   MOVEL     DXCEXM        SCCEXM
     C                   MOVEL     DXENAD        SCENAD
     C                   MOVEL     DXENDL        SCENDL
     C                   MOVEL     DXENNC        SCENNC
     C                   MOVEL     DXENMA        SCENMA
     C                   MOVEL     DXENPA        SCENPA
     C                   MOVEL     DXLCAD        SCLCAD
     C                   MOVEL     DXLCDL        SCLCDL
     C                   MOVEL     DXLCNC        SCLCNC
     C                   MOVEL     DXLCMA        SCLCMA
     C                   MOVEL     DXLCPA        SCLCPA
     C                   MOVEL     DXOOAD        SCOOAD
     C                   MOVEL     DXOONC        SCOONC
      ***** BEG ADD ******* 04/26/06 **********************************
     C                   MOVEL     DXWVAR        SCWVAR
     C                   MOVEL     DXWVCR        SCWVCR
     C                   MOVEL     DXWVDR        SCWVDR
      ***** END ADD ******* 04/26/06 **********************************
     C                   MOVEL     DXCERT        SCCERT
     C                   MOVEL     DXMISC        SCMISC
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCEMNC        DXEMNC
     C                   MOVEL     SCEMMA        DXEMMA
     C                   MOVEL     SCEMPA        DXEMPA
     C                   MOVEL     SCEMFI        DXEMFI
     C                   MOVEL     SCPANC        DXPANC
     C                   MOVEL     SCEMLS        DXEMLS
     C                   MOVEL     SCDVAD        DXDVAD
     C                   MOVEL     SCDVDL        DXDVDL
     C                   MOVEL     SCPEXM        DXPEXM
     C                   MOVEL     SCPINL        DXPINL
     C                   MOVEL     SCPCRO        DXPCRO
     C                   MOVEL     SCPIAP        DXPIAP
     C                   MOVEL     SCCEXM        DXCEXM
     C                   MOVEL     SCENAD        DXENAD
     C                   MOVEL     SCENDL        DXENDL
     C                   MOVEL     SCENNC        DXENNC
     C                   MOVEL     SCENMA        DXENMA
     C                   MOVEL     SCENPA        DXENPA
     C                   MOVEL     SCLCAD        DXLCAD
     C                   MOVEL     SCLCDL        DXLCDL
     C                   MOVEL     SCLCNC        DXLCNC
     C                   MOVEL     SCLCMA        DXLCMA
     C                   MOVEL     SCLCPA        DXLCPA
     C                   MOVEL     SCOOAD        DXOOAD
     C                   MOVEL     SCOONC        DXOONC
      ***** BEG ADD ******* 04/26/06 **********************************
     C                   MOVEL     SCWVAR        DXWVAR
     C                   MOVEL     SCWVCR        DXWVCR
     C                   MOVEL     SCWVDR        DXWVDR
      ***** END ADD ******* 04/26/06 **********************************
     C                   MOVEL     SCCERT        DXCERT
     C                   MOVEL     SCMISC        DXMISC
      *
     C                   ENDSR                                                  MVSCDB
      *
      *
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C     ##RRN         CHAIN     PFN01                              60
     C  N60              Z-ADD     FNFND         SCFND
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C     DXKY          CHAIN     PDX01                              60
     C  N60              Z-ADD     DBRRN2        @@RRN             9 0
     C   60              MOVEL     'A'           @@MODE
      ***** BEG ADD ******* 04/26/06 **********************************
     C     @@MODE        COMP      'A'                                    40     ADD
      ***** END ADD ******* 04/26/06 **********************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B02  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        DXUSER
     C                   MOVEL     Q1PGM         DXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DXDT
     C                   MOVEL     DSTIME        DXTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E02  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   Z-ADD     FNCO#         DXCO#
     C                   Z-ADD     SCFND         DXFND
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        DXIUSR
     C                   MOVEL     Q1USER        DXUSER
     C                   MOVEL     Q1PGM         DXIPGM
     C                   MOVEL     Q1PGM         DXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DXIDT
     C                   MOVEL     SRDTO         DXDT
     C                   MOVEL     DSTIME        DXITM
     C                   MOVEL     DSTIME        DXTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMDXP
     C*
     C                   MOVEL     'C'           @@MODE
     C*
     C                   ENDSR                                                  WRITER
