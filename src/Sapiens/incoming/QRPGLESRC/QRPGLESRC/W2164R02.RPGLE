     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W2164R02 - ACTIVITY STATUS CODE INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE ACTIVITY STATUS FILE.    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W2164R02 - WORK WITH ACTIVITY STATUS CODES                 *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 06/04/97  PROGRAMMER: C:33  PROJECT: WC577          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  02/03/98  C:33  PROJECT: WC577                               *
     H*                  RECOMPILED DUE TO ERROR MESSAGE CHANGE       *
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
     H*  20 - (WCR0503) Code cannot be blank {ADD mode only}          *
     H*  21 - (WC30005) Code cannot be used more than once            *
     H*  22 - (WCR0503) Description cannot be blank                   *
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
     FW2164F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWTAVP     UF A E             DISK
     F                                     RENAME(PAV01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWTAVL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D OPTNM           C                   CONST('Activity Status Code-
     D                                      Information')
     D #OFSCR          C                   CONST(1)
     D COMAPI          C                   CONST('WA001R')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*   KEY LIST FOR THE ACTIVITY STATUS TABLE FILE
     C*
     C     AVKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY
     C*
     C     *LIKE         DEFINE    AVKEY         XXKEY
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
     C     AVSTAT        IFEQ      'I'
     C                   MOVE      'A'           AVSTAT
     C                   ELSE
     C                   MOVE      'I'           AVSTAT
     C                   ENDIF                                                  AVSTAT=I
     C*
     C                   MOVEL     Q1USER        AVUSER
     C                   MOVEL     Q1PGM         AVPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AVDT
     C*
     C                   MOVEL     DSTIME        AVTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  CHGSTS
     C*
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C*
     C*
     C                   ENDSR                                                  DLTPRE
     C*
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C*
     C*
     C                   ENDSR                                                  DSPLYR
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*  CODE CANNOT BE BLANK (IF IN ADD MODE)
     C*
     C   40
B02  CANN90SCCODE        IFEQ      *BLANKS
     C                   SETON                                        20  90
E02  C                   ENDIF                                                  SCCODE=*BLANKS
     C*
     C*  CHECK TO BE SURE CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL(P)  SCCODE        XXKEY
     C   40
B02  CANN90AVKY          CHAIN     PAV01                              60
     C   40
     CANN90
     CANN60              SETON                                        21  90
     C*
     C*  REGION DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        22  90
     C                   ENDIF                                                  SCDESC=*BLANKS
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90AVDT          IFGT      SAVEDT
     C     AVDT          OREQ      SAVEDT
     C     AVTIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  AVDT>SAVEDT
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
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK3
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C   40              MOVEL     'A'           AVSTAT
     C*
     C                   MOVEL     AVKEY         SCCODE
     C                   MOVEL     AVDESC        SCDESC
     C                   MOVEL     AVSTAT        SCSTAT
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         AVCO#
     C                   MOVEL(P)  SCCODE        AVKEY
     C                   MOVEL     SCDESC        AVDESC
     C                   MOVEL     SCSTAT        AVSTAT
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C*
     C*
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C*
     C*
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
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
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
     C                   MOVEL     Q1USER        AVUSER
     C                   MOVEL     Q1PGM         AVPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AVDT
     C                   MOVEL     DSTIME        AVTIME
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
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        AVIUSR
     C                   MOVEL     Q1USER        AVUSER
     C                   MOVEL     Q1PGM         AVIPGM
     C                   MOVEL     Q1PGM         AVPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AVIDT
     C                   MOVEL     SRDTO         AVDT
     C                   MOVEL     DSTIME        AVITM
     C                   MOVEL     DSTIME        AVTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WTAVP
     C*
     C                   ENDSR                                                  WRITER
