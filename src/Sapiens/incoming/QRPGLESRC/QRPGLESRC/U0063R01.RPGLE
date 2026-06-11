     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      /TITLE U0063R01 - PRE-CERT HISTORY STATUS MAINTENANCE
      * ***************************************************************
      *                                                               *
      *  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A HISTORY RECORD     *
      *                FOR A SINGLE PRE-CERT.                         *
      *                ALSO, IT IS THE FIRST SCREEN TO APPEAR WHEN    *
      *                A PRE-CERT AND/OR RE-CERT IS ADDED.            *
      *                                                               *
      *  CALLED FROM:                                                 *
      *    U0063R   - WORK WITH PRE-CERT STATUS HISTORY (OPT 5)       *
      *    U0060R01 - PRE/RE-CERT MAINT                               *
      *    U0060R   - WORK WITH PRE-CERT (OPT 11)                     *
      *  CALLS TO:                                                    *
      *    NONE                                                       *
      *                                                               *
      *  CREATED:  3/02/98    C:47      PROJECT: 87MCB                *
      * ***************************************************************
      * REVISIONS:                                             *
      *                                                        *
      * 02/09/01 C:62 PROJECT: MCMNT  BASE ONLY                *
      *               POPULATE STATUS DATE, TIME AND USER      *
      *                                                        *
      **********************************************************
      *
     FU0063F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FUMCHP     UF A E             DISK
     F                                     RENAME(PCH01:DBRCDF)
     FUMPCL     UF   E           K DISK
      *  GENERIC TABLE FILE
     FFTGTL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Pre-Cert Status -
     D                                     Maintenance')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D XCOROP          C                   CONST('Original open of -
     D                                     Pre-Cert')
     D XCRCRT          C                   CONST('Re-Certification')
     D XCCLOS          C                   CONST('Close')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQCERT           15
     C                   PARM                    QQEXT             3
     C                   PARM                    QQOCST            1
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
     C* PRE-CERT MASTER
     C*
     C     CHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCERT
     C                   KFLD                    XXEXT
     C*
     C*   KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE PRE-CERT MASTER FILE
     C*
     C     PCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCERT
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CHCO#         XXCO#
     C     *LIKE         DEFINE    CHSUB#        XXSUB#
     C     *LIKE         DEFINE    CHFND         XXFND
     C     *LIKE         DEFINE    CHFYR         XXFYR
     C     *LIKE         DEFINE    CHCASE        XXCASE
     C     *LIKE         DEFINE    CHCERT        XXCERT
     C     *LIKE         DEFINE    CHEXT         XXEXT
     C     *LIKE         DEFINE    CHSUB#        KKSUB#
     C     *LIKE         DEFINE    CHFND         KKFND
     C     *LIKE         DEFINE    CHFYR         KKFYR
     C     *LIKE         DEFINE    CHCASE        KKCASE
     C     *LIKE         DEFINE    CHCERT        KKCERT
     C     *LIKE         DEFINE    CHEXT         KKEXT
     C     *LIKE         DEFINE    CHDT          XXDT
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C*
     C/COPY SCOPYMT01C
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
      *
     C                   DO
      *
      * VALIDATE EFFECTIVE DATE
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN72
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      *  AN EXPLANATION MUST BE ENTERED
      *
     C     SCEXPL        IFEQ      *BLANK
     C                   MOVE      *ON           *IN71
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
     C                   ENDDO
      *
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
     C* KFJO   - F10 = SEARCH
     C*================================================================
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
     C                   OTHER
     C                   MOVE      *ON           *IN20                          INVALID SEARCH
     C                   ENDSL
     C                   ENDSR                                                  KJDO
     C*================================================================
     C* KNJO   - F10 = DARTS
     C*================================================================
     C     KNDO          BEGSR
      *
      * CALL API TO ACCESS DARTS
      *
     C                   MOVE      ##CO#         XACO#
     C                   MOVE      XXSUB#        XASUB#
     C                   MOVE      XXFND         XAFND
     C                   MOVE      XXFYR         XAFYR
     C                   MOVE      XXCASE        XACASE
      *
     C                   CALL      'FA703R'
     C                   PARM      *BLANK        @@RTCD            1
     C                   PARM                    @@WCIN
     C                   PARM                    @@WCDK
     C                   PARM                    XACO#             3
     C                   PARM                    XASUB#            5
     C                   PARM                    XAFND             3
     C                   PARM                    XAFYR             3
     C                   PARM                    XACASE            7
     C                   PARM      *BLANK        ##KEY            40
     C                   ENDSR                                                  KNDO
      * ================================================================
      * INIT SUBROUTINE - INITALIZE ALL FIELDS ON ENTRY
      * ================================================================
     C     INIT          BEGSR
     C*
     C                   MOVE      *BLANK        SCOCST
     C                   MOVE      *BLANK        SCOCSD
     C                   Z-ADD     *ZERO         SCEFDT
     C                   MOVE      *BLANK        SCEXPL
     C                   ENDSR                                                  INIT
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
      *
      *  PASSED IN STATUS (QQOCST) WILL BE:
      *
      *    BLANK - CALLED FROM U0060R: OPT10=U0063R
      *    O     - CALLED FROM U0060R: F6=ADD, U0060R01
      *    R     - CALLED FROM U0060R: OPT 7=RE-CERT, U0062R
      *    C     - CALLED FROM U0060R: OPT 11=CLOSE
      *
     C                   SELECT
     C     QQOCST        WHENEQ    *BLANK
     C                   MOVEL     CHCERT        SCCERT
     C                   Z-ADD     CHEXT         SCEXT             3 0
     C                   MOVEL(P)  CHOCST        SCOCST
      *
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   MOVEL(P)  CHEXPL        SCEXPL
      *
     C                   OTHER
     C                   MOVEL(P)  QQOCST        SCOCST
      *
     C                   Z-ADD     DSDATE        SCEFDT
     C                   ENDSL
      *
     C                   MOVEL(P)  'PCST'        XXTBID
     C                   MOVEL(P)  SCOCST        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCOCSD
     C                   ENDIF
     C*** BEGIN ADD *** 02/09/01 **************************************
     C     *IN40         IFEQ      *OFF
     C                   Z-ADD     CHDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT
     C                   Z-ADD     CHTIME        SCTIME
     C                   MOVEL(P)  CHUSER        SCUSER
     C                   ELSE
     C                   Z-ADD     *ZEROS        SCDT
     C                   Z-ADD     *ZEROS        SCTIME
     C                   MOVEL(P)  *BLANKS       SCUSER
     C                   ENDIF
     C*** END   ADD *** 02/29/01 **************************************
      *
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C     @@MODE        IFEQ      'A'
     C                   Z-ADD     XXCO#         CHCO#
     C                   Z-ADD     XXSUB#        CHSUB#
     C                   Z-ADD     XXFND         CHFND
     C                   Z-ADD     XXFYR         CHFYR
     C                   Z-ADD     XXCASE        CHCASE
     C                   MOVEL(P)  XXCERT        CHCERT
     C*
     C                   Z-ADD     XXEXT         CHEXT
     C     QQOCST        IFEQ      'C'
     C                   Z-ADD     *ZERO         CHEXT
     C                   MOVEL(P)  'C'           CHOCST
     C                   END
     C*
     C                   MOVEL(P)  'A'           CHSTAT
     C                   END                                                    @@MODE = A
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         CHEFDT
     C*
     C                   MOVEL(P)  SCEXPL        CHEXPL
     C                   MOVEL(P)  Q1USER        CHUSER
     C                   MOVEL(P)  Q1PGM         CHPGM
     C                   Z-ADD     XXDT          CHDT
     C                   Z-ADD     DSTIME        CHTIME
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
      *
      * RESET ERROR INDICATORS
     C** ************      MOVEA'000000'  *IN,35           *IN30 - 35
      *
     C     *INKN         CASEQ     *ON           KNDO                     90     F14 = DARTS
     C                   ENDCS
      *
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
      *
      *  CLEAR THE ENTIRE IBM ARRAY (*IN)
      *
     C                   CLEAR                   *IN
      *
     C                   RETURN
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
      *
     C                   CALL      'SA001R'
     C                   PARM                    @@WCIF            1            WC INST
     C                   PARM                    @@MCIF            1            MC INST
     C                   PARM                    @@WCIN            1            WC INTERFACE
     C                   PARM                    @@AR              1            MC AR INST
     C                   PARM                    @@NTWK            1            NETWORK INST
     C                   PARM                    @@UR              1            PRECERT INST
     C                   PARM                    @@MED             1            MEDMGMT INST
     C                   PARM                    @@EDI             1            EDI INST
     C                   PARM                    @@WCDK            1            WC DARTS KEY
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*
     C* INITIALIZE ALL SCREEN FIELDS
     C*
     C                   EXSR      INIT
      *
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVEL     QQCERT        XXCERT
     C                   MOVE      QQEXT         XXEXT
     C                   MOVEL     QQCERT        SCCERT
     C                   MOVE      QQEXT         SCEXT
     C*
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C                   SELECT
     C     @@MODE        WHENEQ    'A'
     C                   MOVE      *ON           *IN40
     C     QQOCST        IFEQ      'O'
     C                   MOVEL(P)  XCOROP        SCEXPL                         OPEN
     C                   MOVE      *BLANK        SCCERT
     C                   MOVE      *BLANK        XXCERT
     C                   MOVE      *BLANK        XXEXT
     C                   MOVE      *BLANK        SCEXT
     C                   END
     C     QQOCST        IFEQ      'R'
     C                   MOVEL(P)  XCRCRT        SCEXPL                         RE-CERT
     C                   END
     C     QQOCST        IFEQ      'C'
     C                   MOVEL(P)  XCCLOS        SCEXPL                         CLOSE
     C                   END
     C*
     C     @@MODE        WHENEQ    '2'
     C                   MOVEL(P)  MODED         SCMODE                         SELECT
     C                   ENDSL
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
      * CALL API TO CREATE A TWO LINE HEADING
      *
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        QQRTCD            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C     SCL1HD        PARM      *BLANK        QQL1             78
     C     SCL2HD        PARM      *BLANK        QQL2             78
      *
     C                   ENDSR
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
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   MOVE      *ON           OUTFLG
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C     QQOCST        IFEQ      'C'
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
      *
      * IF CLOSING A PRE-CERT, UPDATE THE PRE-CERT & ALL RE-CERTS
      *
     C     PCKY          SETLL     PPC01
     C     *IN60         DOUEQ     *ON
     C     PCKY          READE     PPC01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     'C'           PCSTAT
     C                   MOVEL(P)  Q1USER        PCUSER
     C                   MOVEL(P)  Q1PGM         PCPGM
     C                   Z-ADD     XXDT          PCDT
     C                   Z-ADD     DSTIME        PCTIME
     C                   UPDATE    PPC01
     C                   ENDIF
     C                   ENDDO
      *
     C                   FEOD      UMCHP
     C                   ENDIF
     C                   MOVE      *ON           OUTFLG
     C                   ENDSR
