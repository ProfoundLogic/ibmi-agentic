     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - FA705R - WRITE A T/M ACTIVITY RECORD.                    S DATA
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA705R                                            *
     H*                                                               *
     H*    CREATED: 05/08/98  C:62                                    *
     H*    DESCRIPTION: WRITES A T/M ACTIVITY RECORD.                 *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  05/27/08 C:191 PROJECT: 25MOS WO2                            *
     H*                 REMOVE CHANGES DONE IN 25MOS                  *
     H*                                                               *
     H*  03/04/08 C:62 PROJECT: 25MOS                                 *
     H*                ADD SYS AND EOB# TO PARM LIST                  *
     H*                                                               *
     H*  10/07/99 C:62 PROJECT: MC212                                 *
     H*                ADD RECON TO TMKEY                             *
     H*                                                               *
     H*  02/03/99 C:62 PROJECT: MC098 15830                           *
     H*                *INLR IS NEVER SET ON, SO XXBAMT IS NEVER      *
     H*                CLEARED.                                       *
     H*                                                               *
     H*  07/23/98 C:62 PROJECT: MC172                                 *
     H*                1) WRITE FAE1P AND NAMBP RECORDS               *
     H*                2) EOSTAT='P' AND EOFLG5='Y'                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*    99         GENERAL USAGE (CHAIN)                           *
     H*                                                               *
     H*****************************************************************
     F*
     FNDTMP     O    E             DISK
     FFAEOP     O    E             DISK
     FFAEOLD    IF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     F*** BEGIN ADD *** 07/23/98 **************************************
     FFAE1P     O    E             DISK
     FNAMBP     O    E             DISK
     F*** BEGIN ADD *** 08/13/98 **************************************
     FNAMBL     UF   E           K DISK
     F                                     RENAME(PMB01:PMB02)
     F*** END   ADD *** 08/13/98 **************************************
     FFMCML1    IF   E           K DISK
     FNDTML1    IF   E           K DISK
     F                                     RENAME(PTM01:PTM02)
     F*** END   ADD *** 07/23/98 **************************************
     FFTATL     IF   E           K DISK
     FFMC1L     UF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@STAF            3
     C                   PARM                    @@SVDT            7 0
     C                   PARM                    @@ACT             3
     C                   PARM                    @@FEE             7 2
     C                   PARM                    @@DESC           40
     C                   PARM                    @@USER           10
     C                   PARM                    @@PGM            10
     C                   PARM                    @@MSCD            4
     C                   PARM                    @@MSEQ            5 0
     C*** BEGIN ADD *** 03/04/08 **************************************
     C*** BEG DEL ****** 05/27/08 ************************************
     C*************        PARM           @@SYS   1
     C*************        PARM           @@EOB#  90
     C*** END DEL ****** 05/27/08 ************************************
     C*** END   ADD *** 03/04/08 **************************************
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
     C* ACTIVITY CODE TABLE FILE
     C*
     C     ATKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C* COMPANY AUXILIARY FILE
     C*
     C     C1KY          KLIST
     C                   KFLD                    KKCO#
     C*** BEGIN ADD *** 07/23/98 **************************************
     C*
     C* MEDICAL CASE FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** END   ADD *** 07/23/98 **************************************
     C*
     C* EOMB# FILE
     C*
     C     EOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKSYS
     C                   KFLD                    KKINV#
     C*** BEGIN ADD *** 08/11/98 **************************************
     C*
     C     MBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** END   ADD *** 08/11/98 **************************************
     C*** BEGIN ADD *** 07/23/98 **************************************
     C*
     C     TMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** BEGIN ADD *** 10/07/99 **************************************
     C                   KFLD                    KKRCN
     C*** END   ADD *** 10/07/99 **************************************
     C*
     C     TMKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** BEGIN ADD *** 10/07/99 **************************************
     C                   KFLD                    KKRCN
     C*** END   ADD *** 10/07/99 **************************************
     C                   KFLD                    KKSEQ#
     C*** END   ADD *** 07/23/98 **************************************
     C*
     C*** BEGIN ADD *** 10/07/99 **************************************
     C     *LIKE         DEFINE    EORCN         KKRCN
     C*** END   ADD *** 10/07/99 **************************************
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOSYS         KKSYS
     C     *LIKE         DEFINE    EOINV#        KKINV#
     C     *LIKE         DEFINE    ATKEY         KKKEY
     C*** BEGIN ADD *** 07/23/98 **************************************
     C     *LIKE         DEFINE    TMEOB#        KKEOB#
     C     *LIKE         DEFINE    TMSEQ#        KKSEQ#
     C     *LIKE         DEFINE    TMSEQ#        XXSEQ#
     C*** END   ADD *** 07/23/98 **************************************
     C*** BEGIN ADD *** 08/13/98 **************************************
     C     *LIKE         DEFINE    EOBAMT        XXBAMT
     C*** END   ADD *** 08/13/98 **************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@SUB#        KKSUB#
     C                   Z-ADD     @@FND         KKFND
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C*** BEGIN ADD *** 03/04/08 **************************************
     C*** BEG DEL ****** 05/27/08 ************************************
     C***********Q1PRMS    IFGT 14
     C***********@@EOB#    ANDNE0
     C***********
     C***********          Z-ADD@@EOB#    KKEOB#
     C***********          Z-ADD0         KKRCN
     C***********          MOVE @@SYS     XXSYS   1
     C***********          EXSR TMWRT
     C***********
     C***********          ELSE
     C*** END DEL ****** 05/27/08 ************************************
     C*** END   ADD *** 03/04/08 **************************************
     C                   MOVEL     'M'           KKSYS
     C                   Z-ADD     *ZEROS        KKINV#
     C*
     C     EOKY          CHAIN     PEO02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C*** BEGIN ADD *** 03/04/08 **************************************
     C*** BEG DEL ****** 05/27/08 ************************************
     C***********          Z-ADDEOEOB#    KKEOB#
     C***********          Z-ADDEORCN     KKRCN
     C***********          MOVE 'M'       XXSYS
     C*** END DEL ****** 05/27/08 ************************************
     C*** END   ADD *** 03/04/08 **************************************
     C                   EXSR      TMWRT
     C*** BEGIN ADD *** 08/13/98 **************************************
     C                   EXSR      UPDTMB
     C*** END   ADD *** 08/13/98 **************************************
     C*
     C                   ELSE
     C*
     C                   EXSR      UPDTC1
     C                   EXSR      EOWRT
     C*** BEGIN ADD *** 07/23/98 **************************************
     C                   EXSR      E1WRT
     C*** END   ADD *** 07/23/98 **************************************
     C*** BEGIN ADD *** 03/04/08 **************************************
     C*** BEG DEL ****** 05/27/08 ************************************
     C************         Z-ADDEOEOB#    KKEOB#
     C************         Z-ADDEORCN     KKRCN
     C************         MOVE 'M'       XXSYS
     C*** END DEL ****** 05/27/08 ************************************
     C*** END   ADD *** 03/04/08 **************************************
     C                   EXSR      TMWRT
     C*** BEGIN ADD *** 07/23/98 **************************************
     C                   EXSR      MBWRT
     C*** END   ADD *** 07/23/98 **************************************
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 03/04/08 **************************************
     C*** BEG DEL ****** 05/27/08 ************************************
     C************         ENDIF
     C*** END DEL ****** 05/27/08 ************************************
     C*** END   ADD *** 03/04/08 **************************************
     C*
     C*  END THE PROGRAM
     C*
     C                   RETURN
     C*
     C*** BEGIN ADD *** 08/13/98 **************************************
     C*================================================================
     C* CLCAMT - CALCULATES BILL AMOUNT.
     C*================================================================
     C     CLCAMT        BEGSR
     C*
     C* CALCULATE ORIGINAL AMOUNT.
     C*
     C     TMMILE        MULT      TMMIRT        XXMIRT            9 2
     C*
     C     TMHRS         MULT      TMHRRT        XXHRRT            9 2
     C*
     C                   Z-ADD     TMMSCG        XXMSCG            9 2
     C*
     C     XXMIRT        ADD       XXHRRT        XXOAMT            9 2
     C                   ADD       XXMSCG        XXOAMT
     C*
     C* SUBTRACT ORIGINAL AMOUNT FROM TOTAL AMOUNT
     C*
     C*** BEGIN CHG *** 02/03/99 **************************************
     C**********           ADD  XXOAMT    XXBAMT
     C                   Z-ADD     XXOAMT        XXBAMT
     C*** END   CHG *** 02/03/99 **************************************
     C*
     C                   ENDSR
     C*** END   ADD *** 08/13/98 **************************************
     C*****************************************************************
     C*    SUBROUTINE - EOWRT - WRITES A FAEOP RECORD.
     C*****************************************************************
     C     EOWRT         BEGSR
     C*
     C                   CLEAR                   PEO01
     C*
     C                   EXSR      MVEODB
     C*
     C                   MOVEL     @@USER        EOUSER
     C                   MOVEL     @@USER        EOIUSR
     C                   MOVEL     @@PGM         EOPGM
     C*
     C                   Z-ADD     DSTIME        EOTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   MOVEL     SRDTO         EOIDT
     C*
     C                   WRITE     PEO01
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 07/23/98 **************************************
     C*================================================================
     C* E1WRT - Writes record to FAE1P.
     C*================================================================
     C     E1WRT         BEGSR
     C*
     C                   CLEAR                   PE101
     C*
     C                   EXSR      MVE1DB
     C*
     C                   MOVE      Q1USER        E1USER
     C                   MOVE      Q1PGM         E1PGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E1DT
     C*
     C                   Z-ADD     DSTIME        E1TIME
     C                   MOVEL     Q1USER        E1IUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E1IDT
     C*
     C                   WRITE     PE101
     C                   FEOD      FAE1P
     C*
     C                   ENDSR
     C*================================================================
     C* MBWRT - Writes record to NAMBP.
     C*================================================================
     C     MBWRT         BEGSR
     C*
     C                   CLEAR                   PMB01
     C*
     C                   EXSR      MVMBDB
     C*
     C                   MOVEL     Q1USER        MBUSER
     C                   MOVEL     Q1PGM         MBPGM
     C                   MOVEL     Q1USER        MBIUSR
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MBDT
     C*
     C                   Z-ADD     DSTIME        MBTIME
     C                   Z-ADD     DSTIME        MBITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MBIDT
     C*
     C                   WRITE     PMB01
     C                   FEOD      NAMBP
     C*
     C                   ENDSR
     C*** END   ADD *** 07/23/98 **************************************
     C*****************************************************************
     C*    SUBROUTINE - MVDATA - FILLS DATA BASE FIELDS FOR WRITE.
     C*****************************************************************
     C     MVDATA        BEGSR
     C*
     C*** MOVE PARM VALUES TO DATA BASE FIELDS.
     C*
     C                   Z-ADD     @@CO#         TMCO#
     C                   Z-ADD     @@SUB#        TMSUB#
     C                   Z-ADD     @@FND         TMFND
     C                   Z-ADD     @@FYR         TMFYR
     C                   Z-ADD     @@CASE        TMCASE
     C                   MOVEL(P)  @@STAF        TMSTAF
     C*
     C*** BEGIN CHG *** 03/04/08 **************************************
     C**********           Z-ADDEOEOB#    TMEOB#
     C*** BEG DEL ****** 05/27/08 ************************************
     C**********           Z-ADDKKEOB#    TMEOB#
     C*** END DEL ****** 05/27/08 ************************************
     C*** END   CHG *** 03/04/08 **************************************
     C*** BEGIN ADD *** 03/04/08 **************************************
     C*** BEG DEL ****** 05/27/08 ************************************
     C**********           Z-ADDKKRCN     TMRCN
     C*** END DEL ****** 05/27/08 ************************************
     C*** END   ADD *** 03/04/08 **************************************
     C*
     C     @@SVDT        IFEQ      *ZEROS
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TMSVDT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     @@SVDT        TMSVDT
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  @@ACT         TMACT
     C                   Z-ADD     @@FEE         TMMSCG
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   MOVEL(P)  @@ACT         KKKEY
     C*** BEGIN ADD *** 07/23/98 **************************************
     C                   MOVEL     'H'           TMHOLD
     C                   MOVE      @@MSEQ        TMMSEQ
     C                   MOVE      XXSEQ#        TMSEQ#
     C*** END   ADD *** 07/23/98 **************************************
     C*
     C     ATKY          CHAIN     PAT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  ATTYP1        TMMVFL
     C                   MOVEL(P)  ATTYP2        TMPNFL
     C*
     C     @@DESC        IFEQ      *BLANKS
     C*
     C                   MOVEL(P)  ATDESC        TMDESC
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  @@DESC        TMDESC
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  *BLANKS       TMMVFL
     C                   MOVEL(P)  *BLANKS       TMPNFL
     C                   MOVEL(P)  @@DESC        TMDESC
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  @@MSCD        TMMSCD
     C                   Z-ADD     @@MSEQ        TMMSEQ
     C*
     C* ASSIGN VALUES TO REMAINING DATA BASE FIELDS.
     C*
     C                   MOVEL     'H'           TMHOLD
     C                   MOVEL     'A'           TMSTAT
     C*** BEGIN ADD *** 03/04/08 **************************************
     C*** BEG ADD ****** 05/27/08 ************************************
     C*************        MOVELXXSYS     TMSYS
     C*** END ADD ****** 05/27/08 ************************************
     C*** END   ADD *** 03/04/08 **************************************
     C*
     C                   Z-ADD     *ZEROS        TMHRS
     C                   Z-ADD     *ZEROS        TMHRRT
     C                   Z-ADD     *ZEROS        TMMILE
     C                   Z-ADD     *ZEROS        TMMIRT
     C                   Z-ADD     *ZEROS        TMINV#
     C                   Z-ADD     *ZEROS        TMOIDT
     C                   Z-ADD     *ZEROS        TM92N1
     C                   Z-ADD     *ZEROS        TM92N2
     C                   Z-ADD     *ZEROS        TM92N3
     C                   Z-ADD     *ZEROS        TM92N4
     C                   Z-ADD     *ZEROS        TM92N5
     C                   Z-ADD     *ZEROS        TM60N1
     C                   Z-ADD     *ZEROS        TM60N2
     C                   Z-ADD     *ZEROS        TM60N3
     C                   Z-ADD     *ZEROS        TM60N4
     C                   Z-ADD     *ZEROS        TM60N5
     C*
     C                   MOVEL     *ZEROS        TMBCH#
     C                   MOVEL     *BLANKS       TMPCS
     C                   MOVEL     *BLANKS       TMPRTF
     C                   MOVEL     *BLANKS       TMBNON
     C                   MOVEL     *BLANKS       TMEXPS
     C                   MOVEL     *BLANKS       TMFLG1
     C                   MOVEL     *BLANKS       TMFLG2
     C                   MOVEL     *BLANKS       TMFLG3
     C                   MOVEL     *BLANKS       TMFLG4
     C                   MOVEL     *BLANKS       TMFLG5
     C                   MOVEL     *BLANKS       TMFL06
     C                   MOVEL     *BLANKS       TMFL07
     C                   MOVEL     *BLANKS       TMFL08
     C                   MOVEL     *BLANKS       TMFL09
     C                   MOVEL     *BLANKS       TMFL10
     C                   MOVEL     *BLANKS       TM10A1
     C                   MOVEL     *BLANKS       TM10A2
     C                   MOVEL     *BLANKS       TM10A3
     C                   MOVEL     *BLANKS       TM10A4
     C                   MOVEL     *BLANKS       TM10A5
     C                   MOVEL     *BLANKS       TMTYPC
     C*
     C                   ENDSR
     C*****************************************************************
     C*    SUBROUTINE - MVEODB - FILLS EOMB DATA BASE FIELDS FOR WRITE.
     C*****************************************************************
     C     MVEODB        BEGSR
     C*
     C                   Z-ADD     @@CO#         EOCO#
     C                   Z-ADD     @@SUB#        EOSUB#
     C                   Z-ADD     @@FND         EOFND
     C                   Z-ADD     @@FYR         EOFYR
     C                   Z-ADD     @@CASE        EOCASE
     C                   Z-ADD     C1EOB#        EOEOB#
     C                   Z-ADD     C1EOB#        EOCHK#
     C                   Z-ADD     *ZEROS        EOINV#
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOTRDT
     C*
     C                   MOVE      *ZEROS        EOFEIN
     C                   Z-ADD     *ZEROS        EOMOD
     C                   Z-ADD     *ZEROS        EOAMT
     C                   Z-ADD     *ZEROS        EOFRDT
     C                   Z-ADD     *ZEROS        EOTODT
     C                   MOVE      *BLANKS       EOTRCD
     C                   MOVE      *BLANKS       EOPNAM
     C                   MOVE      *BLANKS       EOADR1
     C                   MOVE      *BLANKS       EOADR2
     C                   MOVE      *BLANKS       EOCTY
     C                   MOVE      *BLANKS       EOST
     C                   Z-ADD     *ZEROS        EOZIP
     C                   MOVE      *BLANKS       EOEOB1
     C                   MOVE      *BLANKS       EOEOB2
     C                   MOVE      *BLANKS       EOEOB3
     C                   MOVE      'M'           EOSYS
     C                   MOVE      'X'           EOFLG1
     C                   MOVE      'R'           EOFLG2
     C                   MOVE      *BLANKS       EOFLG3
     C                   MOVE      'Y'           EOFLG4
     C*** BEGIN CHG *** 07/23/98 **************************************
     C**********           MOVE *BLANKS   EOFLG5
     C                   MOVE      'Y'           EOFLG5
     C*** END   CHG *** 07/23/98 **************************************
     C                   MOVE      *BLANKS       EOFLG6
     C                   MOVE      *BLANKS       EOFLG7
     C                   MOVE      *BLANKS       EOFLG8
     C                   MOVE      *BLANKS       EOFLG9
     C                   Z-ADD     *ZEROS        EOAMTC
     C*** BEGIN CHG *** 07/23/98 **************************************
     C**********           MOVE *BLANKS   EOSTAT
     C                   MOVE      'P'           EOSTAT
     C*** END   CHG *** 07/23/98 **************************************
     C                   Z-ADD     *ZEROS        EOWCCK
     C                   MOVE      *BLANKS       EODIA1
     C                   MOVE      *BLANKS       EODIA2
     C                   MOVE      *BLANKS       EODIA3
     C                   MOVE      *BLANKS       EODIA4
     C                   MOVE      *BLANKS       EOEMPL
     C                   Z-ADD     *ZEROS        EODFC
     C                   MOVE      *BLANKS       EOFLIC
     C                   MOVE      'N'           EOTAPE
     C                   Z-ADD     1             EO#BLL
     C                   MOVE      *BLANKS       EOCLSF
     C                   MOVE      *BLANKS       EOAST
     C                   MOVE      *BLANKS       EOAUDT
     C                   MOVE      *BLANKS       EOBTFL
     C                   MOVE      'H'           EOHRFL
     C                   MOVE      *BLANKS       EOBLFL
     C                   MOVE      *BLANKS       EONLFL
     C                   MOVE      *BLANKS       EODIFL
     C                   Z-ADD     *ZEROS        EOINV#
     C                   MOVE      *BLANKS       EORMDS
     C                   MOVE      *BLANKS       EORMK1
     C                   MOVE      *BLANKS       EORMK2
     C                   MOVE      *BLANKS       EORMK3
     C                   MOVE      *BLANKS       EOCKCN
     C                   Z-ADD     *ZEROS        EOOTHR
     C                   Z-ADD     *ZEROS        EONDIS
     C                   MOVE      *BLANKS       EOSB01
     C                   MOVE      *BLANKS       EOSB02
     C                   MOVE      *BLANKS       EOSB03
     C                   MOVE      *BLANKS       EOSB04
     C                   MOVE      *BLANKS       EOSB05
     C                   MOVE      *BLANKS       EOSB06
     C                   MOVE      *BLANKS       EOSB07
     C                   MOVE      *BLANKS       EOSB08
     C                   MOVE      *BLANKS       EOSB09
     C                   MOVE      *BLANKS       EOSB10
     C                   Z-ADD     *ZEROS        EOBAMT
     C                   MOVE      *BLANKS       EOZIPA
     C                   MOVE      *BLANKS       EOCERT
     C                   Z-ADD     *ZEROS        EOEXT
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 07/23/98 **************************************
     C*================================================================
     C* MVE1DB - Move information to FAE1P fields - EOMB Aux File.
     C*================================================================
     C     MVE1DB        BEGSR
     C*
     C                   Z-ADD     @@CO#         E1CO#
     C                   Z-ADD     @@SUB#        E1SUB#
     C                   Z-ADD     @@FND         E1FND
     C                   Z-ADD     @@FYR         E1FYR
     C                   Z-ADD     @@CASE        E1CASE
     C                   Z-ADD     C1EOB#        E1EOB#
     C                   MOVE      *BLANKS       E1ACCT
     C                   Z-ADD     *ZEROS        E1ENDT
     C                   Z-ADD     *ZEROS        E1BLDT
     C                   Z-ADD     *ZEROS        E1ORDT
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E1CRDT
     C*
     C                   Z-ADD     *ZEROS        E1TRPH
     C                   Z-ADD     *ZEROS        E1TRPM
     C                   MOVE      *BLANKS       E1FLG1
     C                   MOVE      *BLANKS       E1FLG2
     C                   MOVE      *BLANKS       E1FLG3
     C                   MOVE      *BLANKS       E1FLG4
     C                   MOVE      *BLANKS       E1FLG5
     C                   Z-ADD     *ZEROS        E1DT1
     C                   Z-ADD     *ZEROS        E1DT2
     C                   Z-ADD     *ZEROS        E1DT3
     C                   Z-ADD     *ZEROS        E1DT4
     C                   Z-ADD     *ZEROS        E1DT5
     C                   Z-ADD     *ZEROS        E1AMT1
     C                   Z-ADD     *ZEROS        E1AMT2
     C                   Z-ADD     *ZEROS        E1AMT3
     C                   Z-ADD     *ZEROS        E1AMT4
     C                   Z-ADD     *ZEROS        E1AMT5
     C                   MOVE      *BLANKS       E110A1
     C                   MOVE      *BLANKS       E110A2
     C                   MOVE      *BLANKS       E110A3
     C                   MOVE      *BLANKS       E120A1
     C                   MOVE      *BLANKS       E120A2
     C                   MOVE      *BLANKS       E140A1
     C*
     C                   ENDSR
     C*================================================================
     C* MVMBDB - Move information to NAMBP fields - Med Bill Trans File
     C*================================================================
     C     MVMBDB        BEGSR
     C*
     C                   Z-ADD     @@CO#         MBCO#
     C                   Z-ADD     @@SUB#        MBSUB#
     C                   Z-ADD     @@FND         MBFND
     C                   Z-ADD     @@FYR         MBFYR
     C                   Z-ADD     @@CASE        MBCASE
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@SUB#        KKSUB#
     C                   Z-ADD     @@FND         KKFND
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C                   MOVE      CMSS#         MBSS#
     C                   Z-ADD     CMACDT        MBACDT
     C                   Z-ADD     C1EOB#        MBEOB#
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MBTRDT
     C*
     C                   MOVE      *ZEROS        MBFEIN
     C                   Z-ADD     *ZEROS        MBMOD
     C*** BEGIN CHG *** 08/13/98 **************************************
     C                   EXSR      CLCAMT
     C*
     C                   Z-ADD     XXBAMT        MBBAMT
     C**********           Z-ADD*ZEROS    MBBAMT
     C*** END   CHG *** 08/13/98 **************************************
     C                   Z-ADD     *ZEROS        MBCAMT
     C                   Z-ADD     *ZEROS        MBINV#
     C                   MOVE      'H'           MBHRFL
     C                   MOVE      *BLANKS       MBBLFL
     C                   MOVE      *BLANKS       MBFLG1
     C                   MOVE      *BLANKS       MBFLG2
     C                   MOVE      *BLANKS       MBFLG3
     C                   MOVE      *BLANKS       MBFLG4
     C                   MOVE      *BLANKS       MBFLG5
     C                   MOVE      'A'           MBSTAT
     C*
     C                   ENDSR
     C*** END   ADD *** 07/23/98 **************************************
     C*****************************************************************
     C*    SUBROUTINE - TMWRT - T/M WRITE
     C*****************************************************************
     C     TMWRT         BEGSR
     C*** BEGIN ADD *** 07/23/98 **************************************
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@SUB#        KKSUB#
     C                   Z-ADD     @@FND         KKFND
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C*** BEGIN DEL *** 03/04/08 **************************************
     C**********           Z-ADDEOEOB#    KKEOB#
     C*** BEGIN ADD *** 10/07/99 **************************************
     C**********           Z-ADDEORCN     KKRCN
     C*** END   ADD *** 10/07/99 **************************************
     C*** END   DEL *** 03/04/08 **************************************
     C*** BEG ADD ****** 05/27/08 ************************************
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   Z-ADD     EORCN         KKRCN
     C*** END ADD ****** 05/27/08 ************************************
     C                   MOVE      *HIVAL        KKSEQ#
     C*
     C     TMKY2         SETGT     PTM02
     C     TMKY          READPE    PTM02                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     TMSEQ#        ADD       1             XXSEQ#
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             XXSEQ#
     C*
     C                   ENDIF
     C*
     C*** END   ADD *** 07/23/98 **************************************
     C*
     C                   CLEAR                   PTM01
     C*
     C                   EXSR      MVDATA
     C*
     C                   MOVEL     @@USER        TMUSER
     C                   MOVEL     @@USER        TMIUSR
     C                   MOVEL     @@PGM         TMPGM
     C                   MOVEL     @@PGM         TMIPGM
     C*
     C                   Z-ADD     DSTIME        TMTIME
     C                   Z-ADD     DSTIME        TMITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TMDT
     C                   MOVEL     SRDTO         TMIDT
     C*
     C                   WRITE     PTM01
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 08/13/98 **************************************
     C*================================================================
     C* UPDTMB - Updates NAMBL
     C*================================================================
     C     UPDTMB        BEGSR
     C*
     C     MBKY          CHAIN     PMB02                              99
     C*
     C     *IN99         IFEQ      '0'
     C*
     C                   EXSR      CLCAMT
     C*
     C                   ADD       XXBAMT        MBBAMT
     C*
     C                   MOVEL(P)  Q1USER        MBUSER
     C                   MOVEL(P)  Q1PGM         MBPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MBDT
     C*
     C                   Z-ADD     DSTIME        MBTIME
     C*
     C                   UPDATE    PMB02
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 08/13/98 **************************************
     C*****************************************************************
     C*    SUBROUTINE - UPDTC1 - UPDATE FMC1L FILE.
     C*****************************************************************
     C     UPDTC1        BEGSR
     C*
     C     C1KY          CHAIN     PC101                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   ADD       1             C1EOB#
     C*
     C                   MOVEL     @@USER        TMUSER
     C                   MOVEL     @@PGM         TMPGM
     C*
     C                   Z-ADD     DSTIME        C1TIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C1DT
     C*
     C                   UPDATE    PC101
     C*
     C                   ENDIF
     C*
     C                   ENDSR
