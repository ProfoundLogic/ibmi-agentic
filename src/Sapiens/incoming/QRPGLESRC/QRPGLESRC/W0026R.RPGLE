     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0026R - ADT TRK/DARTS CONTROL INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0010R   - WORK WITH GROUPS                                *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 03/04/98  PROGRAMMER: C:87  PROJECT: WC630          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  11/27/06 C:136 PROJECT: 71WHL                                *
     H*                 POPULATE AUDIT DARTS AND STAFF CODES          *
     H*                                                               *
     H*  10/11/06  C:33  Seq#: WA099/41801                            *
     H*                  Use SC305R instead of SC580R.                *
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
     H*  29 - (WCR0827) Remark Code not valid                         *
     H*  30 - (SCM0614) Remark Code Inactive                          *
     H*  31 - (WCR0827) Remark Code not valid                         *
     H*  32 - (SCM0614) Remark Code Inactive                          *
     H*  POSITION CURSOR:                                             *
     H*  72 -                                                         *
     H*  73 -                                                         *
     H*  74 -                                                         *
     H*  SCR02:                                                       *
     H*  SCR03:                                                       *
     H*****************************************************************
     FW0026F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMAXP     UF A E             DISK
     F                                     RENAME(PAX01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMAXL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     FWMFNP     IF   E             DISK
     FSTCTL1    IF   E           K DISK
      *** BEG ADD ***** 11/27/06 *******************************
     FSMUPP     IF   E             DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
      *** END ADD ***** 11/27/06 *******************************
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
     D OPTNM           C                   CONST('Adt trk/DARTS Contr-
     D                                     ol Information')
     D #OFSCR          C                   CONST(1)
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
     C*  KEY LIST FOR THE ADT TRK/DARTS CONTROL FILE
     C*
     C     AXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    ##RRN             9 0
     C*
      *** BEG ADD ***** 11/27/06 **********************************
     C     UPRMPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN1            9 0
     C                   PARM      3             ##VIEW            1 0
      *** END ADD ***** 11/27/06 **********************************
      *
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
     C*  AUDIT ATTEMPT REMARK CODE MUST BE VALID
     C*
     C  N90SCATTM        IFNE      *BLANKS
     C  N90SCATTM        CHAIN     PCT01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C*
     C                   SETON                                        31  90
     C*
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCATTM NE BLANK
     C*
     C*  FINAL AUDIT MAILED REMARK CODE MUST BE VALID
     C*
     C  N90SCMAIL        IFNE      *BLANKS
     C  N90SCMAIL        CHAIN     PCT01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C*
     C                   SETON                                        44  90
     C*
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCMAIL NE BLANK
     C*
     C*  AUDIT CONTESTED REMARK CODE MUST BE VALID
     C*
     C  N90SCCNTD        IFNE      *BLANKS
     C  N90SCCNTD        CHAIN     PCT01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C*
     C                   SETON                                        46  90
     C*
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCCNTD NE BLANK
     C*
     C* AUDIT RECEIVED REMARK CODE MUST BE VALID
     C*
     C  N90SCRCVD        IFNE      *BLANKS
     C  N90SCRCVD        CHAIN     PCT01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C*
     C                   SETON                                        48  90
     C*
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C                   ENDIF                                                  SCRCVD NE BLANK
     C*
     C* CLEAR CONTESTED INFO REMARK CODE MUST BE VALID
     C*
     C  N90SCCLR         IFNE      *BLANKS
     C  N90SCCLR         CHAIN     PCT01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C*
     C                   SETON                                        50  90
     C*
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C                   ENDIF                                                  SCCLR  NE BLANK
     C*
     C* REASSIGN AUDIT REPRESENTATIVE REMARK CODE MUST BE VALID
     C*
     C  N90SCRSGN        IFNE      *BLANKS
     C  N90SCRSGN        CHAIN     PCT01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C*
     C                   SETON                                        52  90
     C*
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C                   ENDIF                                                  SCRSGN NE BLANK
      *** BEG ADD ***** 11/27/06 **********************************
     C*
     C* VALIDATE INITIAL AUDIT DARTS REMARK CODE
     C*
     C  N90SCIADC        IFNE      *BLANKS
     C  N90SCIADC        CHAIN     PCT01                              60
     C  N90*IN60         IFEQ      *ON
     C                   SETON                                        85  90
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C                   ENDIF                                                  SCFADC NE BLANK
     C*
     C* VALIDATE REVISED AUDIT DARTS REMARK CODE
     C*
     C  N90SCRADC        IFNE      *BLANKS
     C  N90SCRADC        CHAIN     PCT01                              60
     C  N90*IN60         IFEQ      *ON
     C                   SETON                                        86  90
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C                   ENDIF                                                  SCFADC NE BLANK
     C*
     C* VALIDATE INTERNAL AUDIT DARTS STAFF CODE
     C*
     C  N90SCDASC        IFNE      *BLANKS
     C     SCDASC        CHAIN     PUP02                              60
     C   60              SETON                                        87  90
     C                   ENDIF                                                  SCDASC<>*BLANKS
      *** END ADD ***** 11/27/06 **********************************
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90AXDT          IFGT      SAVEDT
     C     AXDT          OREQ      SAVEDT
     C     AXTIME        ANDGT     SAVETM
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
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     KJDO          BEGSR
     C*
     C                   SELECT
      *
      * DARTS REMARK CODE SEARCH
      *
     C     HFLD          WHENEQ    'SCATTM'
     C     HFLD          OREQ      'SCMAIL'
     C     HFLD          OREQ      'SCCNTD'
     C     HFLD          OREQ      'SCRCVD'
     C     HFLD          OREQ      'SCCLR'
     C     HFLD          OREQ      'SCRSGN'
      *** BEG ADD ***** 11/27/06 *************************************
     C     HFLD          OREQ      'SCIADC'
     C     HFLD          OREQ      'SCRADC'
      *** END ADD ***** 11/27/06 *************************************
      *
     C                   MOVE      *BLANKS       QQRMCD            5
     C***  Begin Chg  ***  10/11/06  **************************************
     C**********           CALL 'SC580R'
     C**********           PARM           QQRMCD
     C**********           PARM           QQRTC   1
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C     QQRMCD        PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C***  End   Chg  ***  10/11/06  **************************************
      *
     C     QQRMCD        IFNE      *BLANKS
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCATTM'
     C                   MOVEL     QQRMCD        SCATTM
     C                   MOVE      *ON           *IN74
      *
     C     HFLD          WHENEQ    'SCMAIL'
     C                   MOVEL     QQRMCD        SCMAIL
     C                   MOVE      *ON           *IN75
      *
     C     HFLD          WHENEQ    'SCCNTD'
     C                   MOVEL     QQRMCD        SCCNTD
     C                   MOVE      *ON           *IN76
      *
     C     HFLD          WHENEQ    'SCRCVD'
     C                   MOVEL     QQRMCD        SCRCVD
     C                   MOVE      *ON           *IN77
      *
     C     HFLD          WHENEQ    'SCCLR'
     C                   MOVEL     QQRMCD        SCCLR
     C                   MOVE      *ON           *IN78
      *
     C     HFLD          WHENEQ    'SCRSGN'
     C                   MOVEL     QQRMCD        SCRSGN
      *** BEG ADD ***** 11/27/06 *************************************
     C                   MOVE      *ON           *IN79
      *
     C     HFLD          WHENEQ    'SCIADC'
     C                   MOVEL     QQRMCD        SCIADC
     C                   MOVE      *ON           *IN80
      *
     C     HFLD          WHENEQ    'SCRADC'
     C                   MOVEL     QQRMCD        SCRADC
     C                   MOVE      *ON           *IN81
      *** END ADD ***** 11/27/06 *************************************
      *
     C                   ENDSL                                                  HFLD WHEQ
      *
     C                   ELSE
      *
     C     HFLD          COMP      'SCATTM'                               73
     C     HFLD          COMP      'SCMAIL'                               74
     C     HFLD          COMP      'SCCNTD'                               75
     C     HFLD          COMP      'SCRCVD'                               76
     C     HFLD          COMP      'SCCLR'                                77
     C     HFLD          COMP      'SCRSGN'                               78
      *** BEG ADD ***** 11/27/06 *************************************
     C     HFLD          COMP      'SCIADC'                               79
     C     HFLD          COMP      'SCRADC'                               80
      *** END ADD ***** 11/27/06 *************************************
      *
     C                   ENDIF                                                  QQRMCD IFNE
      *
      *** BEG ADD ***** 11/27/06 *************************************
     C     HFLD          WHENEQ    'SCDASC'
      *
     C                   CALL      'S0005R'      UPRMPL                 99
     C     ##RRN1        IFNE      *ZEROS
     C     ##RRN1        CHAIN     PUP01                              99
     C                   MOVEL     UPSTAF        SCDASC
     C                   ELSE
     C                   MOVE      *ON           *IN81
     C                   ENDIF
      *
      *** END ADD ***** 11/27/06 *************************************
     C                   OTHER
     C                   MOVE      *ON           *IN82
      *
     C                   ENDSL                                                  HFLD WHEQ
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     AXAACD        SCATTM
     C                   MOVEL     AXFMCD        SCMAIL
     C                   MOVEL     AXACCD        SCCNTD
     C                   MOVEL     AXRCCD        SCRCVD
     C                   MOVEL     AXCCCD        SCCLR
     C                   MOVEL     AXREAR        SCRSGN
      *** BEG ADD ***** 11/27/06 *************************************
     C                   MOVEL     AXIADC        SCIADC
     C                   MOVEL     AXRADC        SCRADC
     C                   MOVEL     AXDASC        SCDASC
      *** END ADD ***** 11/27/06 *************************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCATTM        AXAACD
     C                   MOVEL     SCMAIL        AXFMCD
     C                   MOVEL     SCCNTD        AXACCD
     C                   MOVEL     SCRCVD        AXRCCD
     C                   MOVEL     SCCLR         AXCCCD
     C                   MOVEL     SCRSGN        AXREAR
      *** BEG ADD ***** 11/27/06 *************************************
     C                   MOVEL     SCIADC        AXIADC
     C                   MOVEL     SCRADC        AXRADC
     C                   MOVEL     SCDASC        AXDASC
      *** END ADD ***** 11/27/06 *************************************
     C*
     C                   ENDSR                                                  MVSCDB
     C*
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
     C     ##RRN         CHAIN     PFN01                              60
     C  N60              Z-ADD     FNFND         SCFND
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C     AXKY          CHAIN     PAX01                              60
     C  N60              Z-ADD     DBRRN2        @@RRN             9 0
     C   60              MOVEL     'A'           @@MODE
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
     C                   MOVEL     Q1USER        AXUSER
     C                   MOVEL     Q1PGM         AXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AXDT
     C                   MOVEL     DSTIME        AXTIME
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
     C                   Z-ADD     FNCO#         AXCO#
     C                   Z-ADD     SCFND         AXFND
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        AXIUSR
     C                   MOVEL     Q1USER        AXUSER
     C                   MOVEL     Q1PGM         AXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AXIDT
     C                   MOVEL     SRDTO         AXDT
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMAXP
     C*
     C                   MOVEL     'C'           @@MODE
     C*
     C                   ENDSR                                                  WRITER
