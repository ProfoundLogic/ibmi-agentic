     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXDSQ - Fix DARTS records with duplicate sequence #s
     H*****************************************************************
     H* Description:  This program reads all DARTS records in SDCMP   *
     H*   and re-assigns the sequence # if it has been duplicated     *
     H*   within a particular key (CMKEY).                            *
     H*                                                               *
     H*===============================================================*
     H** REVISIONS:
     H*                                                               *
     H* 04/05/02 C:128 Support: VI081/31781                          *
     H*                Changed from SDCMLA logical to SDCMLE. Program *
     H*                will now include deleted records.(CMSTAT="I")  *
     H*
     H* 03/26/02 C:128                                                *
     H*                Changed to include the Multiple Object file
     H*                (SMMDL1). If a duplicate sequence record has
     H*                a object attached to it, change that sequence
     H*                number as well.
     H* 01/14/02 C:128                                                *
     H*                Changed to use location(CMFL05) in SDCMLA
     H*                to determine if the record resides on Optical
     H*                or DASD.
     H*****************************************************************
     F*** Begin Chg *** 04/05/02 **************************************
     F******SDCMLA  IF  E           K        DISK
     FSDCMLE    UF   E           K DISK
     F*** End   Chg *** 04/05/02 **************************************
     FSMARL     UF   E           K DISK
     FSMDLL     UF   E           K DISK
      *** Begin Change 03/26/02 ***************************************
     FSMMDL1    UF   E           K DISK
      *** End   Change 03/26/02 ***************************************
     FSMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
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

     C*=====================================================================
     C*  Key list for the DARTS file.
     C*
     C     CMKY          KLIST
     C                   KFLD                    CMKEY
     C*
     C*  Key list for the DARTS Additional Remarks file.
     C*
     C     ARKY          KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    SVSEQ#
     C*
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
     C     *LIKE         DEFINE    CMSEQ#        ##SEQ#
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
     C*  Begin main loop.
     C*
     C                   READ      PCM01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C                   MOVEL     CMKEY         XXKEY            21
     C*
     C*  Determine if a duplicate exists.
     C*
     C     XXKEY         IFEQ      SVKEY
     C     CMSEQ#        ANDEQ     SVSEQ#
     C*
     C                   Z-ADD     CMDCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDCDT            8 0
     C                   MOVEL     CMCOMM        XXCOMM           20
     C                   Z-ADD     CMIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXIDT             8 0
     C                   Z-ADD     CMDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT              8 0
     C                   EXSR      FIXSEQ
     C   OF              MOVE      '0'           *IN20
     C   OF              EXCEPT    HEAD
     C  N20              EXCEPT    DET
     C                   EXCEPT    DET1
     C                   MOVE      '1'           *IN20
     C                   ADD       1             XXCNT
     C*
     C                   ELSE
     C*
     C                   EXSR      MVDBSV
     C                   MOVE      '0'           *IN20
     C*
     C                   ENDIF
     C*
     C*  Retrieve the next DARTS record.
     C*
     C                   READ      PCM01                                  50
     C                   ENDDO
     C*
     C                   EXCEPT    TOT
     C                   MOVE      '1'           *INLR
     C**************************************************************************
     C*  FIXSEQ - This subroutine gets the next available seq# and assigns
     C*    it to the record with the duplicated seq#.  Then it re-assigns
     C*    the seq# for any Additional Remarks.
     C**************************************************************************
     C     FIXSEQ        BEGSR
     C*
     C                   CALL      'GETNXTSQ'
     C                   PARM                    CMKEY
     C                   PARM      *ZEROS        ##SEQ#
     C*
     C                   Z-ADD     ##SEQ#        CMSEQ#
     C                   UPDATE    PCM01
     C****
     C     ARKY          CHAIN     PAR01                              60
     C     *IN60         DOWEQ     '0'
     C     CMCDAT        IFEQ      ARCDAT
     C     CMRMCD        ANDEQ     ARRMCD
     C                   Z-ADD     CMSEQ#        ARSEQ#
     C                   UPDATE    PAR01
     C                   ENDIF
     C     ARKY          READE     PAR01                                  60
     C                   ENDDO
     C*** Begin Chg 01/14/02 *****************************************
     C******     '*OPTICAL'CAT  '*'       XXWK9   9
     C******     CMDCID    IFEQ XXWK9
     C     CMFL05        IFEQ      'O'
     C*** End   Chg 01/14/02 *****************************************
     C     ARKY          CHAIN     PDL01                              61
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     CMSEQ#        DLSEQ#
     C                   UPDATE    PDL01
     C                   ENDIF
     C                   ENDIF
      *** Begin Change 03/26/02 ***************************************
     C     CMQPTY        IFEQ      'Y'
      *
     C     ARKY          CHAIN     PMD01                              62
      *
     C     *IN62         DOWEQ     *OFF
      *
     C     CMRMCD        IFEQ      MDRMCD
     C                   Z-ADD     CMSEQ#        MDSEQ#
     C                   UPDATE    PMD01
     C                   ENDIF
      *
     C     ARKY          READE     PMD01                                  62
      *
     C                   ENDDO
      *
     C                   ENDIF
      *** End   Change 03/26/02 ***************************************
     C****
     C*
     C                   ENDSR
     C**************************************************************************
     C*  MVDBSV - This subroutine moves the data base fields to hold fields.
     C**************************************************************************
     C     MVDBSV        BEGSR
     C*
     C                   MOVEL     CMKEY         SVKEY            21
     C                   MOVE      CMSEQ#        SVSEQ#            5 0
     C                   MOVE      CMRMCD        SVRMCD            5
     C                   MOVE      CMDIFL        SVDIFL            1
     C                   Z-ADD     CMDCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SVDCDT            8 0
     C                   MOVEL     CMCOMM        SVCOMM           20
     C                   MOVEL     CMIPGM        SVIPGM           10
     C                   Z-ADD     CMIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SVIDT             8 0
     C                   MOVEL     CMIUSR        SVIUSR           10
     C                   MOVEL     CMPNAM        SVPNAM           10
     C                   MOVEL     CMIPGM        SVPGM            10
     C                   Z-ADD     CMDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SVDT              8 0
     C                   MOVEL     CMIUSR        SVUSER           10
     C*
     C                   ENDSR
     C**************************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                       DSTIME               8 '  :  :  '
     O                       CONAME              86
     O                       DSDATE        Y    132
     O          E            HEAD           1
     O                       Q1PGM               10
     O                                           55 'THE REPORT LISTS'
     O                                           74 'DARTS RECORDS WITH'
     O                                           89 'DUPLICATE SEQ#'
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
     O                       SVKEY               21
     O                       SVRMCD              28
     O                       SVDIFL              33
     O                       SVDCDT              48 '  /  /    '
     O                       SVCOMM              71
     O                       CMSEQ#        4     78
     O                       SVIPGM              90
     O                       SVIDT              101 '  /  /    '
     O                       SVIUSR             115
     O                       SVPNAM             127
     O                       SVPGM              140
     O                       SVDT               151 '  /  /    '
     O                       SVUSER             165
     O          E            DET1           1
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
     O                                           28 'TOTAL DUPLICATE RECORDS:'
     O                       XXCNT         4     34
** QCM - CMD FOR QCMDEXC
OVRPRTF FILE(QSYSPRT) PAGESIZE(88 198) LPI(8) CPI(15) OVRFLW(80)
