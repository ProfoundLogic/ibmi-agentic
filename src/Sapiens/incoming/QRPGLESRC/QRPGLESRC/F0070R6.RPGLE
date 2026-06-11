     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE F0070R6 - LOG API
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  This program is called from Bill Entry  *
     H*         and creates an 837 log record for the processed EOMB  *
     H*         if the claim is in a state fund and the accident date *
     H*         is on or after 3/1/97.  An 837 will only be sent if   *
     H*         the State Claim # has been assigned.                  *
     H*                                                               *
     H* CALLED BY:  F0070R                                            *
     H* CALLS TO:   None                                              *
     H* CREATED BY: 15       2/24/97       PROJECT: 87MBN             *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/01/97 C:15 PROJECT: 87MAX                                  *
     H*               ADDED CODE TO DELETE LOG RECORDS IF A CLAIM IS  *
     H*               DELETED OR TRANSFERRED.                         *
     H*                                                               *
     H* 08/18/97 C:15 PROJECT: 87MBN                                  *
     H*               ADDED CODE TO PROCESS LOG RECORDS FOR THE 278   *
     H*               TREATMENT PLAN.                                 *
     H*                                                               *
     H* 09/04/97 C:58 PROJECT: 87MBN                                  *
     H*               ADDED CODE TO RESEND AN 837 TRANSMISSION;
     H*               CALLED BY F0350R WITH ACTION FLAG = 'B'.
     H*                                                               *
     H* 09/29/97 C:58 PROJECT: 87MBN                                  *
     H*               Read all log records to qualify to send change
     H*                                                               *
     H* 10/03/97 C:15 PROJECT: 87MBN                                  *
     H*               Added code to clear new fields added to the Log *
     H*               History file.                                   *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*   61   No BWC status found in file
     H*
     H*****************************************************************
     FFMLGL     UF A E           K DISK
     FWDF2L     IF   E           K DISK

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C*  WDF2L - FUND AUXILIARY FILE
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  FMLGL - EDI Log File (Previous key - Transfers)
     C*
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C     *LIKE         DEFINE    LGCO#         XXCO#
     C     *LIKE         DEFINE    LGSUB#        XXSUB#
     C     *LIKE         DEFINE    LGFND         XXFND
     C     *LIKE         DEFINE    LGFYR         XXFYR
     C     *LIKE         DEFINE    LGCASE        XXCASE
     C     *LIKE         DEFINE    LGEOB#        XXEOB#
     C     *LIKE         DEFINE    LGDT          XXDT
     C*** BEGIN ADD *** 08/18/97 ******************************************
     C     *LIKE         DEFINE    LGTSET        XXTSET
     C*** END ADD ***** 08/18/97 ******************************************
     C*** BEGIN ADD *** 09/29/97 ******************************************
     C     *LIKE         DEFINE    *IN63         XXSNDF
     C*** END ADD ***** 09/29/97 ******************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3            COMPANY
     C                   PARM                    QQSUB#            5            SUBSCRIBER
     C                   PARM                    QQFND             3            FUND ID
     C                   PARM                    QQFYR             3            FUND YR
     C                   PARM                    QQCASE            7            CASE/CLAIM
     C                   PARM                    QQEOB#            9            EOB #
     C                   PARM                    QQACTF            1            ACTION FLAG
     C                   PARM                    QQMGID            7            MSG ID
     C                   PARM                    QQRTCD            1
     C*** BEGIN ADD *** 08/18/97 ******************************************
     C                   PARM                    QQTSET            3
     C*** END ADD ***** 08/18/97 ******************************************
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQEOB#        XXEOB#
     C*** BEGIN ADD *** 08/18/97 ******************************************
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      QQTSET        XXTSET
     C                   ELSE
     C                   MOVE      '837'         XXTSET
     C                   END
     C*** END ADD ***** 08/18/97 ******************************************
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C**  Retrieve FUND AUXILIARY Information
     C*   PROCESS ONLY STATE FUNDS
     C*
     C     F2KY          CHAIN     PF201                              60
     C     *IN60         IFEQ      *ON
     C     F2TYPE        ORNE      'ST'
     C                   GOTO      TERM
     C                   END
     C*
     C*  IF QQACTF = A, CREATE A LOG RECORD.
     C*
     C     QQACTF        IFEQ      'A'
     C                   EXSR      SRLOGA                                       Add Log
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 09/04/97 ******************************************
     C*  IF QQACTF = B, CREATE ANOTHER LOG RECORD.
     C*
     C     QQACTF        IFEQ      'B'
     C                   EXSR      SRLOGB                                       Add Log
     C                   ENDIF
     C*** END ADD ***** 09/04/97 ******************************************
     C*  IF QQACTF = D, DELETE THE LOG RECORD.
     C*
     C     QQACTF        IFEQ      'D'
     C                   EXSR      SRLOGD
     C                   END
     C*
     C*  IF QQACTF = R, RETRY
     C*
     C     QQACTF        IFEQ      'R'
     C                   EXSR      SRLOGR
     C                   END
     C*
     C**  End the program.
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C/EJECT
     C* ***************************************************************
     C*  SRLOGA - ADD LOG RECORD.
     C* ***************************************************************
     C*
     CSR   SRLOGA        BEGSR
     C*
     CSR   LGKY          CHAIN     PLG01                              63
     CSR   *IN63         IFEQ      *ON
     CSR   LGSTS         OREQ      'TR'
     C*
     C                   EXSR      SRLOAD
     CSR                 MOVE      '00'          LGMTC
     C*
     CSR                 WRITE     PLG01
     CSR                 ENDIF
     C*
     CSR                 ENDSR
     C*** BEGIN ADD *** 09/04/97 ******************************************
     C* ***************************************************************
     C*  SRLOGB - ADD LOG RECORD.
     C* ***************************************************************
     C     SRLOGB        BEGSR
     C*
     C     LGKY          CHAIN     PLG01                              63
     C*
     C*** BEGIN DLTE ** 09/29/97 ******************************************
     C********** *IN63     IFEQ *ON
     C********** LGSTS     OREQ 'TR'
     C*** END DELETE ** 09/29/97 ******************************************
     C*
     C*** BEGIN ADD *** 09/29/97 ******************************************
     C*
     C*  To qualify, one must have been sent, no other logs exist.
     C                   MOVE      *OFF          XXSNDF                         Send flag
     C     *IN63         DOWEQ     *OFF
     C     LGTSET        IFEQ      '837'
     C     LGSTS         IFEQ      'TR'
     C*** END ADD ***** 09/29/97 ******************************************
     C     LGSTS         OREQ      'TA'
     C     LGSTS         OREQ      'TE'
     C*
     C*** BEGIN ADD *** 09/29/97 ******************************************
     C                   MOVE      *ON           XXSNDF
     C                   ELSE
     C                   MOVE      *OFF          XXSNDF
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C     LGKY          READE     PLG01                                  63
     C                   ENDDO
     C*
     C     XXSNDF        IFEQ      *ON
     C*** END ADD ***** 09/29/97 ******************************************
     C                   EXSR      SRLOAD
     C                   MOVE      '02'          LGMTC
     C*
     C                   WRITE     PLG01
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD ***** 09/04/97 ******************************************
     C/EJECT
     C* ***************************************************************
     C*  SRLOGD- Process LOG Delete
     C* ***************************************************************
     C*
     CSR   SRLOGD        BEGSR
     C*
     C**   If this is a deletion, delete Log records.
     C*
     C*** BEGIN DLT *** 05/01/97 **************************************
     C********** LGKY      CHAINPLG01                63
     C********** *IN63     IFEQ *OFF
     C**********
     C********** LGSTS     IFEQ 'EX'
     C********** LGSTS     OREQ 'SN'
     C********** LGSTS     OREQ 'TA'
     C********** LGSTS     OREQ 'TE'
     C**********           EXSR SRLOAD
     C**********           MOVEL'04'      LGMTC
     C**********           WRITEPLG01
     C**********
     C**********           ELSE
     C**********
     C**********           MOVE *OFF      *IN63
     C********** *IN63     DOUEQ*ON
     C********** LGMTC     IFEQ '00'
     C********** LGSTS     IFEQ 'EC'
     C********** LGSTS     OREQ 'EI'
     C********** LGKY      DELETPLG01                63
     C**********           END
     C**********           END
     C**********           ENDDO
     C**********
     C**********           END
     C**********           END
     C*** END DLT ***** 05/01/97 **************************************
     C*
     C*** BEGIN ADD *** 05/01/97 **************************************
     C                   MOVE      *OFF          *IN60
     C     *IN60         DOUEQ     *ON
     C     LGKY          DELETE    PLG01                              60
     C                   ENDDO
     C*** END ADD ***** 05/01/97 **************************************
     C*
     C                   ENDSR
     C/EJECT
     C* ***************************************************************
     C*  SRLOGR- Process LOG Delete
     C* ***************************************************************
     C*
     CSR   SRLOGR        BEGSR
     C*
     C**   RETRY
     C*
     C     LGKY          CHAIN     PLG01                              63
     C     *IN63         DOWEQ     *OFF
     C*
     C     LGSTS         IFNE      'TR'
     C                   EXSR      SRLOAD
     C                   MOVEL     '00'          LGMTC
     C                   WRITE     PLG01
     C                   LEAVE
     C*
     C                   ELSE
     C                   GOTO      TERM
     C                   END
     C*
     C     LGKY          READE     PLG01                                  63
     C                   ENDDO
     C                   ENDSR
     C/EJECT
     C* ***************************************************************
     C*  SRLOAD - LOAD DATA INTO FIELDS
     C* ***************************************************************
     C*
     CSR   SRLOAD        BEGSR
     C*
     CSR                 MOVE      XXCO#         LGCO#
     CSR                 MOVE      XXSUB#        LGSUB#
     CSR                 MOVE      XXFND         LGFND
     CSR                 MOVE      XXFYR         LGFYR
     CSR                 MOVE      XXCASE        LGCASE
     CSR                 MOVE      XXEOB#        LGEOB#
     C*** BEGIN CHG *** 08/18/97 ******************************************
     C******************** MOVEL'837'     LGTSET
     C                   MOVEL     XXTSET        LGTSET
     C*** END CHG ***** 08/18/97 ******************************************
     CSR                 MOVE      '00'          LGMTC
     CSR                 MOVEL     'OH'          LGJST
     CSR                 Z-ADD     XXDT          LGDUE
     C*
     C*   Load the Submission Status and the record Status
     C*
     CSR                 MOVE      'EC'          LGSTS                          ENTRY STATUS
     CSR                 MOVE      *BLANK        LGSTAT                         RELEASE/HOLD
     C*
     CSR                 MOVE      *ZEROS        LGEXDT
     CSR                 MOVE      *ZEROS        LGMTCD
     CSR                 MOVE      *ZEROS        LGSENT
     CSR                 MOVE      *ZEROS        LGARCV
     CSR                 MOVE      *ZEROS        LGAENT
     CSR                 MOVE      *ZERO         LGDSQA
     CSR                 MOVE      *ZERO         LGDSQB
     CSR                 MOVE      *ZERO         LGDSQC
     CSR                 MOVE      *ZERO         LGDSQD
     CSR                 MOVE      *ZERO         LGDSQE
     CSR                 MOVE      *ZERO         LGDSQF
     CSR                 MOVE      *ZERO         LGDSQG
     CSR                 MOVE      *ZERO         LGDSQH
     CSR                 MOVE      *ZERO         LGDSQI
     CSR                 MOVE      *ZERO         LGDSQJ
     CSR                 MOVE      *ZERO         LGDSQK
     CSR                 MOVE      *ZERO         LGDSQL
     CSR                 MOVE      *ZERO         LGDSQM
     C                   MOVE      *BLANK        LGHCPY
     CSR                 MOVE      *ZEROS        LGSCHP
     CSR                 MOVE      *ZEROS        LGPRSQ
     C*** BEGIN ADD *** 10/03/97 ***********************************************
     C                   MOVE      *BLANK        LGANTH
     C                   MOVE      *BLANKS       LGEFT#
     C*** END ADD ***** 10/03/97 ***********************************************
     C*
     CSR                 MOVEL     Q1USER        LGIUSR
     CSR                 Z-ADD     XXDT          LGIDT
     CSR                 TIME                    LGITM
     CSR                 MOVEL     Q1PGM         LGIPGM
     CSR                 MOVE      Q1USER        LGUSER
     CSR                 Z-ADD     XXDT          LGDT
     CSR                 TIME                    LGTIME
     CSR                 MOVEL     Q1PGM         LGPGM
     C*
     CSR                 ENDSR
