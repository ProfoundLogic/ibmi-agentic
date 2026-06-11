     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W4231R - Undo Check Reconciliation
     F*
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter Master File
     FWMUPL     IF   E           K DISK
     F*
     F* Check Summary Master File
     FWMCSL     UF   E           K DISK
     F*
     F* Check Reconciliation Detail File
     FWDCRL2    UF A E           K DISK
     F*

     D*
     D* System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* External Program Data Structure
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* Key List for the Check Summary Master File
     C*
     C     CSKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXACCT           20
     C*
     C     CSKEY1        KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXACCT           20
     C                   KFLD                    XXTODT
     C*
     C* Key list for the Check Reconciliation Detail File
     C*
     C     CRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXACCT
     C*
     C     *LIKE         DEFINE    CRDT          XXFRDT
     C     *LIKE         DEFINE    CRDT          XXTODT
     C     *LIKE         DEFINE    CRDT          XXRVDT
     C     *LIKE         DEFINE    CRRVTR        XXRVTR
     C*
     C* Entry Parms
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQACCT           20
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
     C* Set-Up and Initialize
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVEL     QQACCT        XXACCT           20
     C*
     C                   TIME                    DSDTTM
     C*
     C*
     C* Get the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C* Get the Company Record
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C* Get the date range for the previous reconciliation period
     C*
     C     CSKEY         SETGT     PCS01                              60
     C                   READP     PCS01                                  60
     C     *IN60         IFEQ      '0'
     C     CSCO#         ANDEQ     XXCO#
     C     CSACCT        ANDEQ     XXACCT
     C                   Z-ADD     CSEDAT        XXTODT
     C                   READP     PCS01                                  60
     C     *IN60         IFEQ      '0'
     C     CSCO#         ANDEQ     XXCO#
     C     CSACCT        ANDEQ     XXACCT
     C                   Z-ADD     CSEDAT        XXFRDT
     C                   ELSE
     C                   Z-ADD     0             XXFRDT
     C                   END
     C                   ELSE
     C                   Z-ADD     0             XXFRDT
     C                   Z-ADD     0             XXTODT
     C                   END
     C*
     C     XXTODT        IFNE      0
     C*
     C* Undo Check Reconciliation.  UPDATE THE PREVIOUS SUMMARY RECORD
     C*
     C     CSKEY1        CHAIN     PCS01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     0             CSEBAL
     C                   MOVE      'I'           CSSTAT
     C                   MOVE      Q1USER        CSUSER
     C                   MOVE      Q1PGM         CSPGM
     C                   Z-ADD     DSTIME        CSTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CSDT
     C*
     C                   UPDATE    PCS01
     C                   END
     C*
     C* IF THE 1ST RECONCILIATION DATE IS BETWEEN THE DATE RANGE AND
     C* IT IS A REVERSED CHECK, THEN UPDATE THE CHECK RECORD SO THAT
     C* APPEARS AS THE VOID OR STOP PAY & WRITE A REVERSAL RECORD.
     C*
     C* IF THE 1ST RECONCILIATION DATE IS BETWEEN THE DATE RANGE AND
     C* IT IS NOT A REVERSAL, THEN BLANK OUT THE 1ST DATE, 2ND DATE
     C* AND STATUS THEN UPDATE THE RECORD.
     C*
     C* IT "ONLY" THE 2ND RECONCILIATION DATE IS BETWEEN THE RANGE,
     C* THEN BLANK OUT THE 2ND DATE AND THE STATUS FIELD AND UPDATE
     C* THE RECORD.
     C*
     C                   Z-ADD     DSTIME        XXTIME            6 0
     C     CRKEY         CHAIN     PCR01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      '0'           *IN50
     C*
     C     CRRDT1        IFEQ      XXTODT
     C*
     C                   Z-ADD     0             CRRDT1
     C                   Z-ADD     0             CRRDT2
     C     CRRVFL        IFEQ      'N'
     C*
     C                   MOVE      '1'           *IN50
     C*
     C                   ELSE
     C*
     C     CRACTP        IFEQ      *BLANKS
     C                   MOVE      CRRVTP        CRACTP
     C                   Z-ADD     CRRVDT        CRACDT
     C                   END
     C                   MOVE      CRRVTP        XXRVTP            1
     C                   Z-ADD     CRRVDT        XXRVDT
     C                   Z-ADD     CRRVTR        XXRVTR
     C                   MOVE      CRSVST        CRSTAT
     C                   Z-ADD     CRSVDT        CRRDT1
     C                   MOVE      'N'           CRRVFL
     C                   MOVE      *BLANK        CRRVTP
     C                   Z-ADD     0             CRRVDT
     C                   MOVE      *BLANK        CRSVST
     C                   Z-ADD     0             CRSVDT
     C                   Z-ADD     0             CRRVTR
     C                   UPDATE    PCR01
     C                   MOVE      XXRVTP        CRRVTP
     C                   Z-ADD     XXRVDT        CRRVDT
     C                   Z-ADD     XXRVTR        CRRVTR
     C                   MOVE      CRSTAT        CRSVST
     C                   Z-ADD     CRRDT1        CRSVDT
     C                   Z-ADD     0             CRRDT1
     C                   MOVE      'X'           CRACTP
     C                   Z-ADD     XXRVTR        CRACDT
     C                   MOVE      *BLANK        CRSTAT
     C                   MOVE      'Z'           CRRTP
     C                   MOVE      *BLANKS       CRKY
     C                   WRITE     PCR01
     C*
     C                   END
     C*
     C                   END
     C*
     C     CRRDT2        IFGT      XXFRDT
     C     CRRDT2        ANDLE     XXTODT
     C                   Z-ADD     0             CRRDT2
     C                   MOVE      '1'           *IN50
     C                   END
     C*
     C     *IN50         IFEQ      '1'
     C                   MOVE      *BLANKS       CRSTAT
     C                   MOVE      Q1USER        CRUSER
     C                   MOVE      Q1PGM         CRPGM
     C                   Z-ADD     XXTIME        CRTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRDT
     C*
     C                   UPDATE    PCR01
     C                   END
     C*
     C     CRKEY         READE     PCR01                                  60
     C                   ADD       1             XXTIME
     C                   END
     C*
     C                   END
     C*
     C                   SETON                                        LR
