     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE CHKMST - LIST 10/98 TO 1/99 MULTI-STATE/INDIANA POLICIES
     H*****************************************************************
     H**                                                              *
     H** PROGRAMMER: C:65                                             *
     H** CREATED:    05/23/00         PROJECT: SUPPORT                *
     H**                                                              *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS USED:                                              *
     H*                                                               *
     H* 60 - GENERAL CHAIN                                            *
     H*                                                               *
     H*****************************************************************
     FWDELL2    IF   E           K DISK
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FSMCOL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FQSYSPRT2  O    F  132        PRINTER OFLIND(*INOF)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  ELPFDT                 1      7  0
     D  XXYM                   1      5  0
     D                 DS
     D  XXDT                   1      5  0
     D  XXY                    1      3  0
     D  XXM                    4      5  0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D STTOTS          DS                  OCCURS(50)
     D  TDT                    1      5  0
     D  TOTM                   6     14  0
     D  TOTI                  15     23  0

     C*****************************************************************
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
      *
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
      *
      *  EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXDATE
      *
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     C     ELKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
      *
      *   KEY LIST FOR THE COMPANY FILE
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   FIELD DEFINITIONS
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELPFDT        XXDATE
      *
      *   ENTRY PARAMETERS
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCLT             3
     C                   PARM                    QQCO#             3
     C*
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDY'        SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
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
      *  INITIALIZE FIELDS BASED ON *ENTRY PARMS
      *
     C                   MOVE      QQCO#         XXCO#
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDT             5 0
     C*
     C                   Z-ADD     9810          XXDT              5 0
     C     1             DO        50            W                 2 0
     C     W             OCCUR     STTOTS
     C                   Z-ADD     XXDT          TDT
     C                   Z-ADD     0             TOTM
     C                   Z-ADD     0             TOTI
     C*
     C                   ADD       1             XXM
     C     XXM           IFEQ      13
     C                   Z-ADD     1             XXM
     C                   ADD       1             XXY
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   Z-ADD     981001        XXDATE
     C                   Z-ADD     9810          XXBDT             5 0
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     EMFND         XXFND
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     EMDIV         XXDIV
     C*
     C     EMFND         IFNE      999
     C*
     C     ELKY          SETLL     PEL01
     C     ELKY1         READE     PEL01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     XXYM          IFGE      XXBDT
     C     XXYM          ANDLE     XXCDT
     C*
     C                   MOVE      ' '           XXIND             1
     C                   MOVE      ' '           XXMUL             1
     C*
     C     1             DO        50            W
     C     W             OCCUR     STTOTS
     C     XXYM          IFEQ      TDT
     C                   LEAVE
     C                   ENDIF                                                  TAST IFEQ
     C                   ENDDO                                                  DO W:1-52
     C*
     C                   Z-ADD     ELFYR         XXFYR
     C*
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST              2
     C*
     C     XXST          IFEQ      'IN'
     C                   MOVE      'Y'           XXIND
     C                   ENDIF
     C*
     C     ELKY2         CHAIN     PEL02                              60
     C     ELKY3         READE     PEL02                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     RSKY          CHAIN     PRS01                              60
     C*
     C     RSJRST        IFNE      XXST
     C                   MOVE      'Y'           XXMUL
     C                   ENDIF
     C*
     C     RSJRST        IFEQ      'IN'
     C                   MOVE      'Y'           XXIND
     C                   ENDIF
     C*
     C     ELKY3         READE     PEL02                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     XXMUL         IFEQ      'Y'
     C                   ADD       1             TOTM
     C                   ENDIF
     C*
     C     XXIND         IFEQ      'Y'
     C                   ADD       1             TOTI
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXYM IFGE
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  EMFND IFNE
     C*
     C     EMKY1         SETGT     PEM01
     C     EMKY          READE     PEM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   EXSR      PRINT
     C*
     C                   MOVE      *ON           *INLR
      *
      *****************************************************************
      *
      *  SUBROUTINE TO PRINT THE BILL AND LIST EMPLOYER ON REPORT.
      *
     C     PRINT         BEGSR
     C*
     C                   MOVE      '  /  '       XXHD              5
     C*
     C                   EXCEPT    HEAD
     C*
     C     1             DO        50            W                 2 0
     C     W             OCCUR     STTOTS
     C*
     C     TDT           IFLE      XXCDT
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   Z-ADD     TDT           XXDT
     C                   MOVEL     XXM           XXHD
     C                   MOVE      XXY           XXYEAR            2
     C                   MOVE      XXYEAR        XXHD
     C*
     C                   EXCEPT    DET
     C*
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDSR                                                  PRINT SR
     C*
      *****************************************************************
     C*
     OQSYSPRT2  E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           46 'CLIENT:'
     O                       QQCLT               51
     O                                           66 'COMPANY:'
     O                       QQCO#               71
     O          E            HEAD           2
     O                                           14 'MONTH/YEAR'
     O                                           28 'MULTI-STATE'
     O                                           42 'INDIANA'
     O          E            DET            1
     O                       XXHD                11
     O                       TOTM          1     28
     O                       TOTI          1     42
