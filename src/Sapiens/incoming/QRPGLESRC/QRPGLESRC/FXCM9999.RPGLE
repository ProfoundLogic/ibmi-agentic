     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      /TITLE FXCM9999 - Resequence  SDCMP/SMARP if sequence number is 99999
      *****************************************************************
      * Description:  This program reads all DARTS records & SMARP  *
      *   if the sequence number is 99999 get the previous seq# and   *
      *   add one to it. Change the 99999 seq# to the new number.     *
      *                                                               *
      *****************************************************************
     FSMARL     UF   E           K DISK
     FSDCMLE    UF   E           K DISK
      *
     FSMDLL     UF   E           K DISK
     FSMMDL1    UF   E           K DISK
     FSMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FIMDOLA    UF   E           K DISK
     FIMDOLC    UF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F                                     USROPN
     F*

     D QCM             S             65    DIM(1) CTDATA PERRCD(1)              CMD FOR QCMDEXC
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *=====================================================================
      *
      *  Key list for the DARTS Additional Remarks file.
      *

     C     ARKY          KLIST
     C                   KFLD                    SAVKEY
      *
     C     ARKY9         KLIST
     C                   KFLD                    SAVKEY
     C                   KFLD                    ##SEQ#
      *
     C*  Key list for the User Parameter file
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  Key list for the Company Master file
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
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
     C                   MOVEL     QCM(1)        XXCMD
     C                   Z-ADD     65            LENGTH
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    XXCMD            65
     C                   PARM                    LENGTH           15 5
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   TIME                    DSDTTM
     C                   OPEN      QSYSPRT
     C                   EXCEPT    HEAD
     C                   Z-ADD     0             XXCNT             5 0
     C**************************************************************************
     C**************************************************************************
      *  Begin main loop.
      *
     C                   Z-ADD     99999         ##SEQ#            5 0
      *
     C                   EXSR      AR
     C****
     C*
     C                   EXCEPT    TOT
     C*
     C                   MOVE      *ON           *INLR
     C*****************************************************************8
     C     AR            BEGSR
     C                   READ      PCM01                                  50
b1   C     *IN50         DOWEQ     '0'
      *
      *  If the sequence number is 99999 process this record
      *
b2   C     CMSEQ#        IFEQ      99999
     C                   MOVEL     CMKEY         SAVKEY           40
      ****
     C     ARKY          READPE    PCM01                                  60
b3   C     *IN60         IFEQ      *OFF
      *
b4   C     CMSEQ#        IFGE      *ZERO
     C     CMSEQ#        ADD       1             NEWSEQ            5 0
e4   C                   ENDIF
     C                   ELSE
     C                   Z-ADD     1             NEWSEQ
e3   C                   ENDIF
      *
     C     ARKY9         CHAIN     PCM01                              61
b4   C     *IN61         IFEQ      *OFF
     C                   Z-ADD     NEWSEQ        CMSEQ#
     C                   UPDATE    PCM01
     C                   EXSR      A1
     C                   EXSR      D1
     C                   EXSR      M1
     C                   EXSR      I1
     C*** REPORT
     C                   MOVEL     CMCOMM        XXCOMM           20
     C                   Z-ADD     CMIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXIDT             8 0
     C                   Z-ADD     CMDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT              8 0
     C                   MOVEL     CMKEY         XXKEY            21
     C*
     C                   Z-ADD     CMDCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDCDT            8 0
     C*
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       1             XXCNT
     C*
     **
e4   C                   ENDIF
e2   C                   ENDIF
      *
     C                   READ      PCM01                                  50
e1   C                   ENDDO
     C     ENDAR         ENDSR
     ********************************************************************
     C     A1            BEGSR
     C     ARKY9         CHAIN     PAR01                              51
b1   C     *IN51         DOWEQ     '0'
      *
      *  If the sequence number is 99999 process this record
      *
b2   C     ARSEQ#        IFEQ      99999
     C                   Z-ADD     NEWSEQ        ARSEQ#
     C                   UPDATE    PAR01
e2   C                   ENDIF
      *
     C     ARKY9         READE     PAR01                                  51
e1   C                   ENDDO
     C     ENDA1         ENDSR
     C**************************************************************************
     C     D1            BEGSR
     C     CMFL05        IFEQ      'O'
      *
     C     ARKY9         CHAIN     PDL01                              62
      *
      *
     C                   Z-ADD     NEWSEQ        DLSEQ#
     C                   UPDATE    PDL01
      *
     C                   ENDIF
     C     ENDD1         ENDSR
     ***************************************************************************
     C**************************************************************************
     C     M1            BEGSR
     C     CMQPTY        IFEQ      'Y'
      *
     C     ARKY9         CHAIN     PMD01                              62
      *
     C     *IN62         DOWEQ     *OFF
      *
     C                   Z-ADD     NEWSEQ        MDSEQ#
     C                   UPDATE    PMD01
      *
     C     ARKY9         READE     PMD01                                  62
      *
     C                   ENDDO
      *
     C                   ENDIF
     C     ENDM1         ENDSR
     ***************************************************************************
     C**************************************************************************
     C     I1            BEGSR
     C     CMIMGF        IFNE      ' '
     C     CMIMGF        ANDNE     'W'
      *
     C     ARKY9         CHAIN     PDO01                              62
      *
     C     *IN62         IFEQ      *OFF
     C                   Z-ADD     NEWSEQ        DODSQ#
     C                   UPDATE    PDO01
     C                   ENDIF
      *
     C     ARKY9         CHAIN     PDO011                             62
      *
     C     *IN62         IFEQ      *OFF
     C                   Z-ADD     NEWSEQ        DODSQ#
     C                   UPDATE    PDO011
     C                   ENDIF
      *
     C                   ENDIF
     C     ENDI1         ENDSR
     ***************************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                       DSTIME               8 '  :  :  '
     O                       CONAME              86
     O                       DSDATE        Y    132
     O          E            HEAD           1
     O                       Q1PGM               10
     O                                           55 'THE REPORT LISTS'
     O                                           74 'DARTS RECORDS CHGD'
     O                                           89 'FOR SEQ 99999 '
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                           28 'REMARK'
     O                                           35 'DIARY'
     O                                           44 'DIARY'
     O          E            HEAD           1
     O                                            9 'DARTS KEY'
     O                                           27 'CODE'
     O                                           35 'FLAG'
     O                                           48 'CMPLT DATE'
     O                                           58 'COMMENT'
     O                                           78 'SEQ#'
     O                                           88 'INIT PGM'
     O                                          101 'INIT DATE'
     O                                          114 'INIT USER'
     O                                          123 'CMPNAM'
     O                                          139 'MAINT PGM'
     O                                          151 'MAINT DATE'
     O                                          165 'MAINT USER'
     O          E            DET         1  1
     O                       XXKEY               21
     O                       CMRMCD              28
     O                       CMDIFL              33
     O                       XXDCDT              48 '  /  /    '
     O                       XXCOMM              71
     O                       CMSEQ#        4     78
     O                       CMIPGM              90
     O                       XXIDT              101 '  /  /    '
     O                       CMIUSR             115
     O                       CMPNAM             127
     O                       CMPGM              140
     O                       XXDT               151 '  /  /    '
     O                       CMUSER             165
     O          E            TOT         2  1
     O                                           28 'TOTAL SEQ 99999 RECORDS:'
     O                       XXCNT         4     34
**
OVRPRTF FILE(QSYSPRT) PAGESIZE(88 198) LPI(8) CPI(15) OVRFLW(80)
