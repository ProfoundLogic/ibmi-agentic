     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4150R02 - QUOTE STATUS INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE AGENT QUOTE STATUS FILE. *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W4150R - WORK WITH QUOTE STATUS                            *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 11/06/97  PROGRAMMER: C:33  PROJECT: WC577          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/12/10  C:062 Project: 25WQK                                *
     H*                 Delete WMSHL if Quote type='L'                *
     H*                                                               *
     H* 11/25/09  C:198 Project: 133WHW                               *
     H*                 Added processing for @@mode = 'V'             *
     H*                                                               *
     H* 07/08/09  C:62  Project: 133WHN                               *
     H*                 Add a new quote tracking type 'A' - Duplicate *
     H*                                                               *
     H* 05/28/09  C:33  Project: 133WGV                               *
     H*                 Add a new quote tracking type 'I' - Waiting   *
     H*                                                               *
     H* 10/19/07 C:193 PROJECT: WC1561                                *
     H*                ADD A NEW QUOTE TRACKING TYPE 'S' CLOSED       *
     H*                                                               *
     H* 02/05/07 C:180 SUPPORT WA129 53118                            *
     H*                Write 'X' Record (Transfer-from) before
     H*                'T' Record (Transfer-to) to file WAQTP
     H*                when changing agents.
     H*
     H* 11/18/04 C:33  Project: 132WCB                                *
     H*                Add processing for type 'U'                    *
     H*                                                               *
     H* 03/25/04 C:61  PROJECT 114WHL                                 *
     H*                Update "New Business Tracking" record with     *
     H*                 - date declined                               *
     H*                 - date lost                                   *
     H*                                                               *
     H* 10/10/03 C:33  PROJECT: WC1017                                *
     H*                Add processing for types 'F' & 'G'             *
     H*                                                               *
     H* 12/12/02 C:132 SUPPORT WA099 34803                            *
     H*                In Agent Window, Do not allow F10 Search if    *
     H*                object W0040R exists in the Call Stack         *
     H*                                                               *
     H* 03/14/02 C:61  SUPPORT WA122 31632                            *
     H*                Allow lower case text entry on SCR01.          *
     H*                                                               *
     H* 11/01/00 C:33  PROJECT: WC1008                                *
     H*                WHEN TRANSFERRING, IF LAST STATUS IS 'C' OR 'R'*
     H*                LOAD TO QTXFER                                 *
     H*                                                               *
     H* 03/17/00 C:61  PROJECT WC976                                  *
     H*              - Allow add mode when called from QUOTE menu.    *
     H*              - Provide entry for employer without an          *
     H*                employer number and automatically assign       *
     H*                value 'D' declined.                            *
     H*                                                               *
     H*  08/11/99  C:118 PROJECT: WC577                               *
     H*                  Highlight Orig. Sub Dt., made chg in display *
     H*                  file.                                        *
     H*                                                               *
     H*  08/25/98  C:33  SEQ#: WA091/13385                            *
     H*                  CHANGE WRITER SUBROUTINE TO UPDATE ADD A 'T' *
     H*                  RECORD TO THE TRANSFERRED FROM AGENT         *
     H*                                                               *
     H*  03/03/98  C:33  PROJECT: WC395                               *
     H*                  ADD OPTIONS: BOUND, BOUND W/MONEY & TRANSFER *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  41 - QTSTAT = Q (QUOTE ENV)                                  *
     H* N41 - QTSTAT = W (LIVE ENV)                                   *
     H*  40 - ADD MODE                                                *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  71 - (WCR0500) Type change date invalid                      *
     H*  72 - (WCR0500) Original entry date invalid                   *
     H*  73 - (WCR0500) Faxed date invalid                            *
     H*  74 - (WCR0503) Emplr name can't be blank                     *
     H*  75 - (WCR0500) Effective date invalid                        *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR02:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR03:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*****************************************************************
      *
     FW4150F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWAQTP     UF A E             DISK
     F                                     RENAME(PQT01:DBRCDF)
     FWMFNL     IF   E           K DISK
     F***  BEGIN ADD  ***  03/03/98  *********************************
     F                                     INFDS(FNFDS)
     F***  END   ADD  ***  03/03/98  *********************************
     FWMAAL1    IF   E           K DISK
     F***  BEGIN ADD  ***  03/03/98  *********************************
     FWMAAP     IF   E             DISK
     F                                     RENAME(PAA01:PAA02)
     F***  END   ADD  ***  03/03/98  *********************************
     FWMAGL     IF   E           K DISK
      ***** BEG ADD ******* 03/25/04 **********************************
     F*** BEGIN ADD *** 03/12/10 **************************************
     FWMSHLQ    UF   E           K DISK    USROPN
     F*** END   ADD *** 03/12/10 **************************************
      *
      *  New Business Tracking
     FWMNBL     UF A E           K DISK
      *
      *  System Flag
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
      *
      ***** END ADD ******* 03/25/04 **********************************
      *
      *
      /COPY SCOPYMT01D
     D*** BEGIN ADD *** 03/12/10 **************************************
     D QCA             S             56    DIM(2) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*** END   ADD *** 03/12/10 **************************************
     D*
     D***PY SCOPYMT01I
     D*
     D***  BEGIN ADD  ***  03/03/98  *********************************
     D FNFDS           DS
     D  FNRRN                397    400B 0
     D***  END   ADD  ***  03/03/98  *********************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Quote Status')
     D COMAPI          C                   CONST('WA001R')
     D RTVQTE          C                   CONST('RTVQTELIB')
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
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C                   PARM                    @@QTE             1
     C***  END   ADD  ***  03/03/98  *********************************
      ***** BEG ADD ******* 03/17/00 **********************************
     C                   PARM                    @@RRN2            9 0          WMAAP
      ***** END ADD ******* 03/17/00 **********************************
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
      ***** BEG ADD ******* 03/25/04 **********************************
      *
      *  New Business Tracking
      *
     C     NBKY          KLIST
     C                   KFLD                    QTCO#
     C                   KFLD                    QTFND
     C                   KFLD                    QTEMP#
      *
      ***** END ADD ******* 03/25/04 **********************************
     C*
     C*  GROUP FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    QTCO#
     C                   KFLD                    QTFND
      ***** BEG ADD ******* 03/17/00 **********************************
      *
     C     FNKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      *
      ***** END ADD ******* 03/17/00 **********************************
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    QTCO#
     C                   KFLD                    QTFND
     C                   KFLD                    QTAGT#
     C*
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C     AAKY1         KLIST
     C                   KFLD                    QTCO#
     C                   KFLD                    QTFND
     C                   KFLD                    SCAGT#
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    AACO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*** BEGIN ADD *** 03/12/10 **************************************
     C*
     C*  SUBMISSION HISTORY
     C*
     C     SHKY          KLIST
     C                   KFLD                    QTCO#
     C                   KFLD                    QTFND
     C                   KFLD                    QTEMP#
     C*** END   ADD *** 03/12/10 **************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    QTTYPE        XXTYPE
     C     *LIKE         DEFINE    QTEMP#        XXEMP#
     C     *LIKE         DEFINE    QTDIV         XXDIV
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C     *LIKE         DEFINE    QTAGT#        SVAGT#
     C     *LIKE         DEFINE    @@QTE         XXQTE
     C***  END   ADD  ***  03/03/98  *********************************
      ***** BEG ADD ******* 03/17/00 **********************************
     C     *LIKE         DEFINE    QTCO#         KKCO#
     C     *LIKE         DEFINE    QTFND         KKFND
     C     *LIKE         DEFINE    QTAGT#        KKAGT#
      ***** END ADD ******* 03/17/00 **********************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*===============================================================
     C* CHGQTP - SUBROUTINE FOR CHANGING QUOTE STATUS
     C*===============================================================
     C*
     C     CHGQTP        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C                   SELECT
     C     @@MODE        WHENEQ    'T'
     C                   Z-ADD     SCAGT#        QTAGT#
     C                   MOVE      QTTYPE        XXTYPE
     C                   ENDSL
     C***  END   ADD  ***  03/03/98  *********************************
     C*
     C                   MOVE      XXTYPE        QTTYPE
     C                   MOVEL(P)  Q1USER        QTUSER
     C                   MOVEL(P)  Q1PGM         QTPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QTDT
     C*
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C                   SELECT
     C     @@MODE        WHENEQ    'T'
     C                   OTHER
     C***  END   ADD  ***  03/03/98  *********************************
     C                   MOVEL     SRDTO         QTWKDT
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C                   ENDSL
     C***  END   ADD  ***  03/03/98  *********************************
     C*
     C                   MOVEL     DSTIME        QTTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*** BEGIN ADD *** 03/12/10 **************************************
     C*
     C* END OVERRIDE AND CLOSE FILE
     C*
     C                   EXSR      ENDOVR
     C*** END   ADD *** 03/12/10 **************************************
     C*
     C                   RETURN
     C*
     C                   ENDSR                                                  CHGQTP
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*** BEGIN ADD *** 03/12/10 **************************************
     C*================================================================
     C* DLTSHP - Delete WMSHP records
     C*================================================================
     C     DLTSHP        BEGSR
     C*
     C     SHKY          CHAIN     PSH01                              99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    PSH01
     C     SHKY          READE     PSH01                                  99
     C                   ENDDO
     C*
     C                   ENDSR                                                  DLTSHL
     C*** END   ADD *** 03/12/10 **************************************
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*** BEGIN ADD *** 03/12/10 **************************************
     C*================================================================
     C* ENDOVR - End Override
     C*================================================================
     C     ENDOVR        BEGSR
     C*
     C* END OVERRIDE AND CLOSE FILE
     C*
     C                   CLOSE     WMSHLQ
     C*
     C                   CALL      'QCMDEXC'                              60
     C                   PARM      QCA(2)        CMD              56
     C                   PARM      56            LENGTH           15 5
     C*
     C                   ENDSR                                                  DSPLYR
     C*** END   ADD *** 03/12/10 **************************************
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
      ***** BEG ADD ******* 03/17/00 **********************************
      *
      *  ADD MODE - QUOTE SYSTEM
      *
     C     @@MODE        IFEQ      'A'
     C     @@QTE         ANDEQ     'Q'
      *
      * Employer name can not be blank
      *
     C  N90SCENAM        IFEQ      *BLANKS
     C                   SETON                                        90  74
     C                   ENDIF                                                  SCENAM
      *
      * Effective date must be valid if entered
      *
     C     *IN90         IFEQ      *OFF
     C     SCEFDT        ANDNE     *ZERO
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
      *
     C     SRERR         IFEQ      *ON
     C                   SETON                                        90  75
     C                   ENDIF                                                  SRERR
      *
     C                   ENDIF                                                  SCEFDT
      *
      * Original submission date must be valid
      *
     C     *IN90         IFEQ      *OFF
      *
     C                   Z-ADD     SCORDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCORDT
      *
     C     SRERR         IFEQ      *ON
     C                   SETON                                        90  72
     C                   ENDIF                                                  SRERR
      *
     C                   ENDIF                                                  *IN90
      *
      * Quote Fax date must be valid, if entered
      *
     C  N90SCFXDT        IFNE      *ZERO
      *
     C                   Z-ADD     SCFXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFXDT
      *
     C     SRERR         IFEQ      *ON
     C                   SETON                                        73  90
     C                   ENDIF                                                  SRERR=*ON
      *
     C                   ENDIF                                                  SCFXDT<>*ZERO
      *
     C                   ELSE
      *
      ***** END ADD ******* 03/17/00 **********************************
      *
     C                   Z-ADD     SCWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C  N90              MOVEL     SRDTO         SCWKDT
      *
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        71  90
     C                   ENDIF                                                  SRERR=*ON
      *
     C                   Z-ADD     SCORDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C  N90              MOVEL     SRDTO         SCORDT
      *
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        72  90
     C                   ENDIF                                                  SRERR=*ON
      *
     C  N90SCFXDT        IFNE      *ZERO
      *
     C                   Z-ADD     SCFXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C  N90              MOVEL     SRDTO         SCFXDT
      *
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        73  90
     C                   ENDIF                                                  SRERR=*ON
      *
     C                   ENDIF                                                  SCFXDT<>*ZERO
      *
      *  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
      *
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N90QTDT          IFGT      SAVEDT
     C     QTDT          OREQ      SAVEDT
     C     QTTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  QTDT>SAVEDT
      *
      ***** BEG ADD ******* 03/17/00 **********************************
     C                   ENDIF                                                  @@MODE
      *
      ***** END ADD ******* 03/17/00 **********************************
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
      *
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
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C*================================================================
     C* KJDO - Processing for F10
     C*================================================================
     C*
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCAGT#'
     C*
      ***** BEG ADD **** 12/12/02 ***************************************
     C*
     C* Check for Program in CALL Stack first before Search of Agent
     C* and do not allow F10 Search if it exists in Call Stack
     C*
     C                   MOVEL(P)  'W0040R'      XXPGM
     C*
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM                    XXPGM            10
     C                   PARM                    XXERR             1
     C                   SETOFF                                       23
     C*
     C     XXERR         IFEQ      *ON
     C     XXPGM         ANDNE     *BLANKS
     C                   SETON                                        23  90
     C*
     C                   ENDIF
     C*
     C     *IN23         IFEQ      *OFF
      ***** END ADD **** 12/12/02 ***************************************
     C*
     C                   CALL      'W0040R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      FNRRN         ##RRN             9 0
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     WMAAP                              60
     C*
     C                   Z-ADD     AAAGT#        SCAGT#
     C*
     C                   ENDIF                                                  ##RRN<>*ZEROS
     C*
      ***** BEG ADD **** 12/12/02 ***************************************
     C*
     C                   ENDIF                                                  *IN90 = *OFF
      ***** END ADD **** 12/12/02 ***************************************
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C***  END   ADD  ***  03/03/98  *********************************
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      QTSTAT        SCSTAT
     C                   MOVE      QTTYPE        SCTYPE
     C*
     C                   Z-ADD     QTEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   Z-ADD     QTWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCWKDT
     C*
     C                   Z-ADD     QTORDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCORDT
     C*
     C                   Z-ADD     QTNBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCNBDT
     C*
     C                   Z-ADD     QTFXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFXDT
     C*
     C                   Z-ADD     QTCHAP        SCCHAP
     C                   MOVEL(P)  QTCOMM        SCCOMM
     C                   MOVEL(P)  QTCOM2        SCCOM2
     C                   MOVEL(P)  QTCARR        SCCARR
     C                   Z-ADD     QTPREM        SCPREM
     C*
      ***** BEG ADD ******* 03/17/00 **********************************
      *
      * ADD MODE
      *
     C     @@MODE        IFEQ      'A'
     C     @@QTE         ANDEQ     'Q'
     C                   MOVEL(P)  *BLANKS       SCENAM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SCWKDT
     C                   MOVE      'D'           SCTYPE
     C                   Z-ADD     DSDATE        SCORDT
     C                   Z-ADD     *ZERO         SCNBDT
     C                   MOVE      'Q'           SCSTAT
     C                   ENDIF                                                  @@MODE = A
      *
      ***** END ADD ******* 03/17/00 **********************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
      ***** BEG ADD ******* 03/17/00 **********************************
      *
      * ADD MODE
      *
     C     @@MODE        IFEQ      'A'
     C     @@QTE         ANDEQ     'Q'
     C                   Z-ADD     AACO#         QTCO#
     C                   Z-ADD     AAFND         QTFND
     C                   Z-ADD     AAAGT#        QTAGT#
     C                   MOVEL(P)  SCENAM        QTNAME
     C                   MOVE      'D'           QTTYPE
     C                   MOVE      'Q'           QTSTAT
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QTEFDT
      *
     C                   Z-ADD     SCCHAP        QTCHAP
     C                   ENDIF                                                  @@MODE = A
      *
      ***** END ADD ******* 03/17/00 **********************************
     C*
     C                   Z-ADD     SCWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QTWKDT
     C*
     C                   Z-ADD     SCORDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QTORDT
     C*
     C                   Z-ADD     SCFXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QTFXDT
     C*
     C                   MOVEL(P)  SCCOMM        QTCOMM
     C                   MOVEL(P)  SCCOM2        QTCOM2
     C                   MOVEL(P)  SCCARR        QTCARR
     C   41              Z-ADD     SCPREM        QTPREM
     C                   MOVEL(P)  Q1USER        QTUSER
     C                   MOVEL(P)  Q1PGM         QTPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QTDT
     C*
     C                   MOVEL     DSTIME        QTTIME
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
     C     PGMEND        BEGSR
     C*
     C*** BEGIN ADD *** 03/12/10 **************************************
     C*
     C* END OVERRIDE AND CLOSE FILE
     C*
     C                   EXSR      ENDOVR
     C*** END   ADD *** 03/12/10 **************************************
     C*
     C                   ENDSR                                                  PGMEND
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C     Q1PRMS        IFGE      5
     C                   MOVE      @@QTE         XXQTE
     C                   ELSE
     C                   CLEAR                   XXQTE
     C                   ENDIF                                                  Q1PRMS>=5
     C***  END   ADD  ***  03/03/98  *********************************
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
      ***** BEG ADD ******* 03/17/00 **********************************
      *
     C     @@MODE        COMP      'A'                                    40     ADD
      *
     C     @@RRN         IFNE      *ZERO
      ***** END ADD ******* 03/17/00 **********************************
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             60
     C*
     C     QTSTAT        COMP      'Q'                                    41    QUOTE ENV
     C*
     C                   Z-ADD     QTFND         SCFND
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL(P)  FNNAME        SCNAME
     C*
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C                   Z-ADD     QTAGT#        SVAGT#
     C***  END   ADD  ***  03/03/98  *********************************
     C                   Z-ADD     QTAGT#        SCAGT#
     C     AAKY          CHAIN     PAA01                              60
     C     AGKY          CHAIN     PAG01                              60
     C                   MOVEL(P)  AGNAME        SCANAM
     C*
     C                   Z-ADD     QTEMP#        SCEMP#
     C                   Z-ADD     QTDIV         SCDIV
     C                   MOVEL(P)  QTNAME        XXNAME
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      QTEMP#        XXEMP#
     C                   PARM      QTDIV         XXDIV
     C                   PARM                    XXNAME           40
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
      ***** BEG ADD ******* 03/17/00 **********************************
      *
     C                   ELSE
      *
     C     @@RRN2        CHAIN     PAA02                              60
     C*
     C                   Z-ADD     AAAGT#        SCAGT#
     C                   Z-ADD     AAFND         SCFND
     C                   Z-ADD     AACO#         KKCO#
     C                   Z-ADD     AAFND         KKFND
     C                   Z-ADD     AAAGT#        KKAGT#
      *
     C     FNKY1         CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCNAME
     C   60              MOVEL(P)  *BLANKS       SCNAME
      *
     C     AGKY          CHAIN     PAG01                              60
     C  N60              MOVEL(P)  AGNAME        SCANAM
     C   60              MOVEL(P)  *BLANKS       SCANAM
      *
     C                   MOVE      'Q'           QTSTAT
     C                   SETON                                        41
      *
     C                   ENDIF                                                  @@RRN
      *
      ***** END ADD ******* 03/17/00 **********************************
     C*** BEGIN ADD *** 03/12/10 **************************************
     C*
     C* BEGIN OVERRIDE AND OPEN FILE
     C*
     C* GET QUOTE LIBRARY
     C*
     C                   CALL      RTVQTE
     C                   PARM      '*QTE'        ##ENV             4
     C                   PARM      *BLANKS       ##LIB            10
     C*
     C                   MOVEL     '/WMSHLQ)'    FILE             10
     C*
     C     QCA(1)        CAT(P)    ##LIB:0       CMD
     C                   CAT       FILE:0        CMD
     C*
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    CMD
     C                   PARM      56            LENGTH
     C*
     C                   OPEN      WMSHLQ
     C*** END   ADD *** 03/12/10 **************************************
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
     C     @@MODE        WHENEQ    'Q'
     C                   MOVEL(P)  'Q'           XXTYPE
     C                   EXSR      CHGQTP
     C*
     C     @@MODE        WHENEQ    'N'
     C                   MOVEL(P)  'P'           XXTYPE
     C                   EXSR      CHGQTP
     C*
     C***  Begin Add  ***  07/08/09  **************************************
     C     @@MODE        WHENEQ    'a'
     C                   MOVEL(P)  'A'           XXTYPE
     C                   EXSR      CHGQTP
     C***  End   Add  ***  07/08/09  **************************************
     C***  Begin Add  ***  05/28/09  **************************************
     C     @@MODE        WHENEQ    'i'
     C                   MOVEL(P)  'I'           XXTYPE
     C                   EXSR      CHGQTP
     C***  End   Add  ***  05/28/09  **************************************
     C*
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C     @@MODE        WHENEQ    'B'
     C                   MOVEL(P)  'B'           XXTYPE
     C                   EXSR      CHGQTP
     C*
     C     @@MODE        WHENEQ    'M'
     C                   MOVEL(P)  'M'           XXTYPE
     C                   EXSR      CHGQTP
     C***  END   ADD  ***  03/03/98  *********************************
     C*
     C     @@MODE        WHENEQ    'W'
     C                   MOVEL(P)  'W'           XXTYPE
     C                   EXSR      CHGQTP
     C***  Begin Add  ***  11/18/04  **************************************
     C*
     C     @@MODE        WHENEQ    'U'
     C                   MOVEL(P)  'U'           XXTYPE
     C                   EXSR      CHGQTP
     C***  End   Add  ***  11/18/04  **************************************
     C***  Begin Add  ***  10/10/03  **************************************
     C*
     C     @@MODE        WHENEQ    'F'
     C                   MOVEL(P)  'F'           XXTYPE
     C                   EXSR      CHGQTP
     C*
     C     @@MODE        WHENEQ    'G'
     C                   MOVEL(P)  'G'           XXTYPE
     C                   EXSR      CHGQTP
     C***  End   Add  ***  10/10/03  **************************************
     C*
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C     @@MODE        WHENEQ    'T'
     C                   EXSR      TRNSFR
     C***  END   ADD  ***  03/03/98  *********************************
     C*
     C     @@MODE        WHENEQ    'X'
     C                   MOVEL(P)  'C'           XXTYPE
     C                   EXSR      WRITER
     C*** BEGIN ADD *** 03/12/10 **************************************
     C*
     C* END OVERRIDE AND CLOSE FILE
     C*
     C                   EXSR      ENDOVR
     C*** END   ADD *** 03/12/10 **************************************
     C                   RETURN
     C*
     C     @@MODE        WHENEQ    'R'
     C                   MOVEL(P)  'R'           XXTYPE
     C                   EXSR      WRITER
     C*** BEGIN ADD *** 03/12/10 **************************************
     C*
     C* END OVERRIDE AND CLOSE FILE
     C*
     C                   EXSR      ENDOVR
     C*** END   ADD *** 03/12/10 **************************************
     C                   RETURN
     C*
     C     @@MODE        WHENEQ    'L'
      ***** BEG ADD ******* 03/25/04 **********************************
     C                   EXSR      SRNBTR
      ***** BEG ADD ******* 03/25/04 **********************************
     C                   MOVEL(P)  'L'           XXTYPE
     C                   EXSR      CHGQTP
     C*** BEGIN ADD *** 03/12/10 **************************************
     C                   EXSR      DLTSHP
     C*** END   ADD *** 03/12/10 **************************************
     C*
     C     @@MODE        WHENEQ    'Y'
      ***** BEG ADD ******* 03/25/04 **********************************
     C                   EXSR      SRNBTR
      ***** BEG ADD ******* 03/25/04 **********************************
     C                   MOVEL(P)  'D'           XXTYPE
     C                   EXSR      CHGQTP
     C*
      ***** BEG ADD ******* 10/19/07 **********************************
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      SRNBTR
     C                   MOVEL(P)  'S'           XXTYPE
     C                   EXSR      CHGQTP
      ***** END ADD ******* 10/19/07 **********************************
     C*
     C*** Beg Add *** 11/25/09 ****************************************
     C     @@MODE        WHENEQ    'V'
     C                   MOVEL(P)  'V'           XXTYPE
     C                   EXSR      CHGQTP
     C*** End Add *** 11/25/09 ****************************************
     C                   ENDSL
     C*
     C                   ENDSR                                                  PREOTH
      ***** BEG ADD ******* 03/25/04 **********************************
      *
      *
      *
      *===============================================================
      * SRNBTR - Create New Business Tracking Record
      *===============================================================
      *
     C     SRNBTR        BEGSR
      *
     C                   MOVEL     'WC'          XSKEY             2
     C     XSKEY         CHAIN     SWC01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     WCFL26        ANDEQ     'Y'
      *
     C     @@RRN         CHAIN     DBRCDF                             60
     C  N60NBKY          CHAIN     PNB01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     Q1USER        NBUSER
     C                   MOVEL     Q1PGM         NBPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NBDT
     C                   MOVEL     DSTIME        NBTIME
      *
     C                   SELECT
      *
      *  LOST
     C     @@MODE        WHENEQ    'L'
     C                   Z-ADD     NBDT          NBLSDT
      *
      *  DECLINED
     C     @@MODE        WHENEQ    'Y'
     C                   Z-ADD     NBDT          NBDCDT
      *
     C                   ENDSL
      *
     C                   UPDATE    PNB01
      *
     C                   ELSE
      *
     C                   CLEAR                   PNB01
     C                   Z-ADD     QTCO#         NBCO#
     C                   Z-ADD     QTFND         NBFND
     C                   Z-ADD     QTEMP#        NBEMP#
      *
     C                   MOVEL     Q1USER        NBUSER
     C                   MOVEL     Q1USER        NBIUSR
     C                   MOVEL     Q1PGM         NBPGM
     C                   MOVEL     Q1PGM         NBIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NBDT
     C                   MOVEL     SRDTO         NBIDT
     C                   MOVEL     DSTIME        NBTIME
     C                   MOVEL     DSTIME        NBITM
      *
     C                   SELECT
      *
      *  LOST
     C     @@MODE        WHENEQ    'L'
     C                   Z-ADD     NBDT          NBLSDT
      *
      *  DECLINED
     C     @@MODE        WHENEQ    'Y'
     C                   Z-ADD     NBDT          NBDCDT
      *
     C                   ENDSL
      *
     C                   WRITE     PNB01
     C                   ENDIF                                                  NBKY
      *
     C                   ENDIF                                                  WCFL26
      *
     C                   ENDSR                                                  SRNBTR
      *
      *
      ***** END ADD ******* 03/25/04 **********************************
     C*
     C***  BEGIN ADD  ***  03/03/98  *********************************
     C*================================================================
     C* TRNSFR - Transfer an employer from one agent to another
     C*================================================================
     C*
     C     TRNSFR        BEGSR
     C*
     C     *IN90         DOUEQ     *OFF
     C*
     C                   MOVE      *OFF          *IN90
     C*
     C                   CALL      'SRRMVMSG'                           99
     C*
     C                   EXFMT     WDWAGT#
     C*
     C     *INKL         IFEQ      *ON
     C*** BEGIN ADD *** 03/12/10 **************************************
     C*
     C* END OVERRIDE AND CLOSE FILE
     C*
     C                   EXSR      ENDOVR
     C*** END   ADD *** 03/12/10 **************************************
     C                   RETURN
     C                   ENDIF                                                  *INKL=*ON
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C*  VALIDATE PRIMARY AGENT NUMBER
     C*
     C     AAKY1         CHAIN     PAA01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C     AASTAT        OREQ      'I'
     C                   SETON                                        20  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C*  AGENT # CANNOT BE THE SAME AS PREVIOUS AGENT#
     C*
     C  N90SCAGT#        IFEQ      SVAGT#
     C                   SETON                                        21  90
     C                   ENDIF                                                  SCAGT#=SVAGT#
     C*
     C                   ENDDO                                                  *IN90=*OFF
     C*
     C     XXQTE         IFEQ      'Q'
     C*
     C                   EXSR      CHGQTP
     C*
     C                   ELSE
     C*
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C                   SELECT
     C     QTTYPE        WHENEQ    'C'
     C     QTTYPE        OREQ      'R'
     C                   MOVE      QTTYPE        QTXFER
     C                   ENDSL
     C***  END   ADD  ***  11/01/00  *********************************
     C*
     C***  BEGIN DEL  ***  02/05/07  *********************************
     C*****                MOVEL'T'       XXTYPE    P
     C****                 EXSR WRITER
     C***  END   DEL  ***  02/05/07  *********************************
     C*
     C                   MOVEL(P)  'X'           XXTYPE
     C                   EXSR      WRITER
     C*
     C***  BEGIN ADD  ***  02/05/07  *********************************
     C                   MOVEL(P)  'T'           XXTYPE
     C                   EXSR      WRITER
     C***  END   ADD  ***  02/05/07  *********************************
      *
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C                   CLEAR                   QTXFER
     C***  END   ADD  ***  11/01/00  *********************************
     C*
     C                   ENDIF                                                  XXQTE=Q
     C*
     C*** BEGIN ADD *** 03/12/10 **************************************
     C*
     C* END OVERRIDE AND CLOSE FILE
     C*
     C                   EXSR      ENDOVR
     C*** END   ADD *** 03/12/10 **************************************
     C                   RETURN
     C*
     C                   ENDSR                                                  TRNSFR
     C***  END   ADD  ***  03/03/98  *********************************
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
     C** ***               EXSR MVSCDB
     C*
      ***** BEG ADD ******* 03/17/00 **********************************
      *
     C     @@MODE        IFEQ      'A'
     C     @@QTE         ANDEQ     'Q'
      *
     C                   EXSR      MVSCDB
      *
     C                   ELSE
      *
      ***** END ADD ******* 03/17/00 **********************************
     C     @@RRN         CHAIN(N)  DBRCDF                             60
     C*
     C***  BEGIN CHG  ***  08/25/98  *********************************
     C*****BEGIN ADD  ***  03/03/98  *********************************
     C**********           SELEC
     C********** @@MODE    WHEQ 'T'
     C**********           Z-ADDSCAGT#    QTAGT#
     C********** @@MODE    WHEQ 'X'
     C**********           Z-ADDSVAGT#    QTAGT#
     C**********           ENDSL
     C*****END** ADD  ***  03/03/98  *********************************
     C                   SELECT
     C     XXTYPE        WHENEQ    'T'
     C                   Z-ADD     SCAGT#        QTAGT#
     C     XXTYPE        WHENEQ    'X'
     C                   Z-ADD     SVAGT#        QTAGT#
     C                   ENDSL
     C***  END   CHG  ***  08/25/98  *********************************
     C*
     C                   MOVE      XXTYPE        QTTYPE
      ***** BEG ADD ******* 03/17/00 **********************************
      *
     C                   ENDIF                                                  @@MODE = A
      *
      ***** END ADD ******* 03/17/00 **********************************
     C                   MOVEL(P)  Q1USER        QTUSER
     C                   MOVEL(P)  Q1USER        QTIUSR
     C                   MOVEL(P)  Q1PGM         QTPGM
     C                   MOVEL(P)  Q1PGM         QTIPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QTDT
     C                   MOVEL     SRDTO         QTIDT
     C                   MOVEL     SRDTO         QTWKDT
     C*
     C                   MOVEL     DSTIME        QTTIME
     C                   MOVEL     DSTIME        QTITM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WAQTP
     C*
     C                   ENDSR                                                  WRITER
**  (QCMDEX)
OVRDBF FILE(WMSHLQ) TOFILE(
DLTOVR FILE(WMSHLQ)
