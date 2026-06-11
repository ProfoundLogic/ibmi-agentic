     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE CVTLOST - Days Lost - Initialize V3 Claim Master
     H*
     H*     Read all payments for claim; use Compensation payments to
     H*   tally total days lost; update masterfile CMLOST.
     H*
     H*****************************************************************
     H*
     H* CREATED BY:   C58       New for V3
     H* PROJECT:      WC739     09.15.98
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H*****************************************************************
     H* INDICATOR USAGE:
     H*
     H*        60  Payment detail: record  indicator
     H*        62  Claim master: record not found
     H*
     H*
     H*****************************************************************
     F*       Files
     F*________________________________________________________________
     F*      Payment Detail File - by case
     FWDPAL     IPE  E           K DISK
     F*________________________________________________________________
     F*      Claim master file - Co, fund FYR, case
     FWMCML1    UF   E           K DISK
     F*________________________________________________________________
     F*      User parameters
     FWMUPL     IF   E           K DISK
     F*________________________________________________________________
     F*      Printed list
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     F                                     USROPN
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOA)

     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     I*****************************************************************
     I*________________________________________________________________
     I*      Tables and Arrays
     I*
     I*
     IPPA01         60
     I                                          PACO#         L1
     I                                          PAFND         L1
     I                                          PAFYR         L1
     I                                          PACASE        L1
     I*

     C*________________________________________________________________
     C*================================================================
     C*
     C*          MAINLINE
     C*
     C*================================================================
     C*
     C*   Process all the compensation payments
     C     *IN60         IFEQ      *ON                                          Got record
     C                   ADD       1             INPUT             7 0
     C                   Z-ADD     0             XXLOST
     C*
     C*   First payment for new case, get claim master
     C     *INL1         IFEQ      *ON
     C     CMKY          CHAIN     PCM01                              62
     C                   Z-ADD     0             XDLOST
     C                   ENDIF                                                   L1 = *ON
     C*
     C     PARETP        IFEQ      'C'                                          Compensation
     C                   ADD       1             INCMP             7 0
     C*
     C*   Calculate days lost only if master was found
     C     *IN62         IFEQ      *OFF
     C*    Calculate days in weeks
     C     PAWKS         IFNE      *ZERO
     C     CMDAYS        IFNE      *ZERO
     C     PAWKS         MULT      CMDAYS        XXLOST
     C                   ELSE
     C     PAWKS         MULT      7             XXLOST
     C                   ENDIF                                                   CMDAYS ne 0
     C                   ENDIF                                                   PAWKS ne 0
     C*
     C*    Add days
     C                   ADD       PADAYS        XXLOST
     C*
     C*    Post according to Transaction
     C     PATYPE        IFEQ      'A'
     C     PAAMT         ANDGT     *ZERO
     C     PATYPE        OREQ      'P'
     C     PAAMT         ANDGT     *ZERO
     C     PATYPE        OREQ      'M'
     C     PAAMT         ANDGT     *ZERO
     C     PATYPE        OREQ      'Z'
     C     PAAMT         ANDGT     *ZERO
     C     PATYPE        OREQ      'R'
     C     PAAMT         ANDLT     *ZERO
     C     PATYPE        OREQ      'S'
     C     PAAMT         ANDLT     *ZERO
     C     PATYPE        OREQ      'V'
     C     PAAMT         ANDLT     *ZERO
     C                   ADD       XXLOST        XDLOST
     C*
     C                   ELSE                                                   x TYPE = ...
     C*
     C                   SUB       XXLOST        XDLOST
     C*
     C                   ENDIF                                                  PATYPE = ...
     C*
     C                   ENDIF                                                  *IN62 = OFF
     C*
     C                   ENDIF                                                  RETP = C
     C                   ENDIF                                                   60 = *ON
     C*
     C*================================================================
     C*
     C*   End of payments for claim, update  claim master, if any.
     CL1                 EXSR      SRUCLM
     C*
     CLR                 EXCEPT    HEAD
     CLR                 EXCEPT    TOTAL
     C*
     C/EJECT
     C*
     C**************************************************************************
     C*   SRUCLM - Update the claim master.
     C**************************************************************************
     C     SRUCLM        BEGSR
     C*
     C*  Process only if claim had compensation payment(s).
     C     *IN62         IFEQ      *ON
     C     XDLOST        ORNE      CMLOST
     C*
     C*  Print error if master not found
     C     *IN62         IFEQ      *ON
     C*
     C                   ADD       1             CLMNF             7 0
     C     *INOA         IFEQ      *ON
     C                   MOVE      *OFF          *INOA
     C                   EXCEPT    HEAD2
     C                   ENDIF                                                  *INOA = ON
     C                   EXCEPT    DET2
     C*
     C                   ELSE                                                     XIN62
     C*
     C*   Update claim master
     C                   Z-ADD     XDLOST        CMLOST
     C*
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CMTIME
     C                   Z-ADD     XXDT          CMDT
     C                   Z-ADD     XXDT          CMLSDT
     C*
     C                   UPDATE    PCM01
     C                   ADD       1             CLMUP             7 0
     C*
     C*  Print list
     C     *INOF         IFEQ      *ON
     C                   MOVE      *OFF          *INOF
     C                   EXCEPT    HEAD
     C                   ENDIF                                                  *INOF = ON
     C                   EXCEPT    DET
     C*
     C                   ENDIF                                                    *IN62=ON
     C                   ENDIF                                                    CLCMP > 0
     C*
     C     ENUCLM        ENDSR
     C*
     C**************************************************************************
     C*   Program Initialization - *INZSR
     C**************************************************************************
     C     *INZSR        BEGSR
     C*
     C*** Define Parameter Lists
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
     C*________________________________________________________________
     C*   Define Key Lists
     C*________________________________________________________________
     C*          WMUPL - User Parameter File
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*________________________________________________________________
     C*          WMCML - Claim Master File
     C*
     C     CMKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C*________________________________________________________________
     C*   Define work fields
     C*
     C     *LIKE         DEFINE    PADT          XXDT
     C*
     C     *LIKE         DEFINE    CMLOST        XXLOST
     C     *LIKE         DEFINE    CMLOST        XDLOST
     C*
     C*
     C*   Convert batch date to CYYMMDD
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C*   Retrieve the User Parameter File
     C     UPKY          CHAIN     PUP01                              60
     C*
     C                   OPEN      QSYSPRT
     C                   EXCEPT    HEAD
     C*
     C                   EXCEPT    HEAD2
     C*
     C                   ENDSR
     C*________________________________________________________________
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                                           66 '          V3 File Init'
     O                                           88 'ialization            '
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           66 '          Calculate Da'
     O                                           88 'ys Lost               '
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                           75 'Days in'
     O          E            HEAD           2
     O                                            3 'CO#'
     O                                           10 'FND'
     O                                           15 'FYR'
     O                                           24 'CASE'
     O                                           32 'Name '
     O                                           67 'Days Lost'
     O                                           75 ' Week '
     O          E            DET            1
     O                       CMCO#         4      3
     O                       CMFND         4     10
     O                       CMFYR         4     15
     O                       CMCASE        4     24
     O               L1      CMLSN               57
     O               L1      CMFSN               57
     O                       XDLOST        Z     63
     O                       CMDAYS              74
     O          E            TOTAL       2  2
     O                                           63 '  *** TOTALS ***    '
     O          E            TOTAL          1
     O                                           22 'Input records read . .'
     O                                           44 ' . . . . . . . . . . .'
     O                       INPUT         J     53
     O          E            TOTAL          1
     O                                           22 'Compensation records p'
     O                                           44 'rocessed . . . . . . .'
     O                       INCMP         J     53
     O          E            TOTAL          1
     O                                           22 'Claims records updated'
     O                                           44 ' . . . . . . . . . . .'
     O                       CLMUP         J     53
     O          E            TOTAL          1
     O                                           22 'Claims records not fou'
     O                                           44 'nd . . . . . . . . . .'
     O                       CLMNF         J     53
     C*________________________________________________________________
     OQSYSPRT1  E            HEAD2          1 06
     O                       DSTIME              08 '  :  :  '
     O                                           66 '          Calculate Da'
     O                                           88 'ys Lost Errors        '
     O                       DSDATE        Y    132
     O          E            HEAD2          2
     O                       Q1PGM               10
     O                                           66 '          Claim Master'
     O                                           88 ' Not In File          '
     O                                          126 'PAGE:'
     O                       PAGE2         Z    132
     O          E            HEAD2          1
     O                                           56 'Days in'
     O          E            HEAD2          2
     O                                            3 'CO#'
     O                                           10 'FND'
     O                                           15 'FYR'
     O                                           24 'CASE'
     O                                           33 'Pmt Seq#'
     O                                           37 'Typ'
     O                                           54 'Amount'
     O          E            DET2           1
     O                       PACO#         4      3
     O                       PAFND         4     10
     O                       PAFYR         4     15
     O                       PACASE        4     24
     O                       PASEQ#        4     32
     O                       PATYPE              36
     O                       PAAMT         J     55
     O*_______________________________________________________________
     O*
