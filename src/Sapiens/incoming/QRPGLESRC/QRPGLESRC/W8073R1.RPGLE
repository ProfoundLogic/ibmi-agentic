     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*                                                               *
     H/TITLE W8073R1 -  Auto Submit - Write Transmission Mgmt File.   *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/18/10  C:53   PROJECT: 139WNN PRISS 83                     *
     H*                  Always write the log record.                 *
     H*                                                               *
     H* 08/11/10  C:220  PROJECT: 139WNN Priss#54                     *
     H*                  Corrected Re transmission flag.              *
     H*                                                               *
     H* 03/24/10  C:232  PROJECT: 139WNN                              *
     H*                  Created.                                     *
     H*                                                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMM2L     UF A E           K DISK
     FSTSTL3    IF   E           K DISK
     FWTGTL     IF   E           K DISK
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE TRANSMISSION MANAGEMENT FILE
     C*
     C     M2KY          KLIST
     C                   KFLD                    @I1CO#
     C                   KFLD                    @I1JRST
     C                   KFLD                    @I1TSID
     C                   KFLD                    XXOTDT
     C                   KFLD                    XXOTTM
     C*
     C*   ENTRY PARM LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @I1CO#            3 0
     C                   PARM                    @I1JRST           2
     C                   PARM                    @SYSDT            8
     C                   PARM                    @SYSTM            6
     C                   PARM                    @I1TSID           3
     C                   PARM                    @I1TSTF           1
     C                   PARM                    @I1EDIF           3
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
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
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    M2OTDT        XXOTDT
     C     *LIKE         DEFINE    M2OTTM        XXOTTM
     C     *LIKE         DEFINE    M2TSET        XXTSET
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C                   MOVE      *BLANK        XXKEY            10
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         UPCO#
     C                   END
     C                   Z-ADD     UPCO#         XXCO#
     C*
     C*  GET THE COMPANY RECORD
     C*
     C                   TIME                    DSDTTM
     C*
     C*   WRITE/UPDATE THE SUBMISSION MANAGEMENT RECORD.
     C*
     C*** BEG DEL *** 10/18/10 *************************************
     C****               Z-ADD     DSDATE        SRDTI
     C****               CALL      'SRCVTDAT'    DTCYMD
     C****               MOVEL     SRDTO         XXOTDT
     C****               MOVE      DSTIME        XXOTTM
     C****               MOVEL     @I1TSID       XXTSET
     C**** M2KY          CHAIN     PM201                              11
     C**** *IN11         IFEQ      *OFF
     C***** Beg Chg ***** 08/11/10 *************************************
     C*****              MOVE      'A'           M2STAT
     C****               MOVE      'R'           M2STAT
     C***** Beg Chg ***** 08/11/10 *************************************
     C****               MOVE      @I1TSTF       M2TSTF
     C****               Z-ADD     UDATE         SRDTI
     C****               CALL      'SRCVTDAT'    DTCYMD
     C****               MOVEL     SRDTO         M2LTDT
     C****               TIME                    M2LTTM
     C**** M2OTDT        IFEQ      *ZEROS
     C****               Z-ADD     DSDATE        SRDTI
     C****               CALL      'SRCVTDAT'    DTCYMD
     C****               MOVEL     SRDTO         M2OTDT
     C****               MOVE      DSTIME        M2OTTM
     C****               MOVE      *DATE         @SYSDT
     C****               MOVE      M2OTTM        @SYSTM
     C****               ENDIF
     C****               UPDATE    PM201
     C****               ELSE
     C*** END DEL *** 10/18/10 *************************************
     C                   MOVE      @I1CO#        M2CO#
     C                   MOVE      @I1JRST       M2JST
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M2OTDT
     C                   MOVE      DSTIME        M2OTTM
     C                   MOVE      *DATE         @SYSDT
     C                   MOVE      M2OTTM        @SYSTM
     C                   MOVE      @I1TSID       M2TSET
     C                   MOVE      *ZEROS        M2MTCD
     C                   MOVE      *ZEROS        M2ARCV
     C                   MOVE      *ZEROS        M2AENT
     C                   MOVEL     'AUTO'        M2IUSR
     C                   MOVEL     Q1PGM         M2IPGM
     C                   MOVE      M2OTDT        M2IDT
     C                   MOVE      M2OTTM        M2ITM
     C                   MOVE      *ZEROS        M2148T
     C                   MOVE      *ZEROS        M2A49T
     C                   MOVE      *ZEROS        M2148S
     C                   MOVE      *ZEROS        M2A49S
     C                   MOVE      M2OTDT        M2LTDT
     C                   MOVE      M2OTTM        M2LTTM
     C                   MOVE      'A'           M2STAT
     C                   MOVE      @I1TSTF       M2TSTF
     C                   MOVE      @I1EDIF       M2EDIF
     C                   WRITE     PM201
     C*** BEG DEL *** 10/18/10 *************************************
     C****               ENDIF
     C*** END DEL *** 10/18/10 *************************************
     C*
     C                   MOVE      '1'           *INLR
