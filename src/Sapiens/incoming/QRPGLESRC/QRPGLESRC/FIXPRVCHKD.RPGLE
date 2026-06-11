     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE FIXPRVCHKD - Driver to create Provider Check Requests    *
     H*****************************************************************
     H* PROGRAM DESCRIPTION: Based on F0340R1                         *
     H*                                                               *
     H* CALLED FROM: Command Line                                     *
     H*                                                               *
     H* CALLS TO: FIXPRVCHKD                                          *
     H*                                                               *
     H* PROGRAMMER: C:79                                              *
     H* CREATED:    04/23/25  Support: 118859                         *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  09/22/23 C:062 PROJECT: 146MKP                               *
     H*                 Add other 2 fee processing.                   *
     H*                                                               *
     H*  03/21/19 C:180 PROJECT: 146MIZ                               *
     H*                 Issue separate check for % of fee schedule    *
     H*                 savings.                                      *
     H*                                                               *
     H*  02/12/18 C:180 PROJECT: 114MJC                               *
     H*                 Add 835 export                                *
     H*                                                               *
     H*  08/26/08 C:62  PROJECT: 25MOS                                *
     H*                 DON'T CALC EAOCA1 IF NOT ZERO                 *
     H*                                                               *
     H*  02/07/07 C:62  PROJECT: 144MAP                               *
     H*                 POPULATE EAUSER WITH EOUSER AND EARMDS WITH   *
     H*                 CMCLAD.                                       *
     H*                                                               *
     H*  12/09/04 C:62  PROJECT: NY007                                *
     H*                 POPULATE EAOCA1                               *
     H*                                                               *
     H*  01/21/02 C:62  PROJECT: AR033                                *
     H*                 UPDATE EANHRF WITH 'H'.                       *
     H*                                                               *
     H*  08/15/00 C:62  PROJECT: MC215                                *
     H*                 REPLACE SFBPF REFERENCES WITH APIS.           *
     H*                                                               *
     H*  07/27/99  C:62  PROJECT: MC201                               *
     H*                  ADD UPDATE OF EAAPPF IN FAEAP.               *
     H*                                                               *
     H*  04/22/99  C:94  PROJECT: MC154                               *
     H*                  BASE CALL TO W0081R ON AUTO CHECK ISSUANCE   *
     H*                  FLAG                                         *
     H*                                                               *
     H*  02/16/99  C:94  PROJECT: 25MKL                               *
     H*                  DEL UPDATE OF EAEPFL AND EAAPPF              *
     H*                                                               *
     H*  11/10/98  C:94  PROJECT: 87MBV                               *
     H*                  ADDED ALL UPDATES THAT WERE DONE IN          *
     H*                  F6020R AND F7071R1 (BASE VERSION)            *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60 - GENERAL CHAIN INDICATOR                                  *
     H* 61 - GENERAL CHAIN INDICATOR                                  *
     H*                                                               *
     H*****************************************************************
     F*** EOMB File selected on EOSTAT *NE 'P'
     FFAEAL1    UF   E           K DISK
     F*
     FFAEOL4    UF   E           K DISK
     F*
     FFMCML1    IF   E           K DISK
     F*
     FFMF1L     IF   E           K DISK
     F*
     F***** FROM F6020R *****
     F*
     FFDCALL    UF   E           K DISK
     F*
     FFMFNL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFACLML8   IF   E           K DISK
     d changeWCMCdemo  pr                  ExtPgm('FA002R')

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  CLAIM MASTER FILE
     C*
     C*          CMKY      KLIST
     C*                    KFLD           EOCO#
     C*                    KFLD           EOSUB#
     C*                    KFLD           EOFND
     C*                    KFLD           EOFYR
     C*                    KFLD           EOCASE
     C*
     C*
     C     CMKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  FS BASE CONTROL FILE KEY LIST
     C*
     C*
     C*  EOB FILE KEY LIST
     C*
     C*          EOKY      KLIST
     C*                    KFLD           XXCO#
     C*
     C*  EOB FILE KEY LIST
     C*
     C     EOKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C*
     C     CAKEY         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAFEIN
     C                   KFLD                    EAMOD
     C                   KFLD                    EAEOB#
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
     C* ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQWCPR            1
     C*
     C*  INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    EACO#         XXCO#
     C     *LIKE         DEFINE    EASUB#        XXSUB#
     C     *LIKE         DEFINE    EAFND         XXFND
     C     *LIKE         DEFINE    F1OUTA        XXOUTA
     C     *LIKE         DEFINE    EANAMT        @@NAMT
     C*** Begin Chg *** 09/22/23 *******************************************
     C******LIKE         DEFINE    EANAMT        @@MRA
     C     *LIKE         DEFINE    EAOCA2        @@OCA2
     C*** End   Chg *** 09/22/23 *******************************************
     C     *LIKE         DEFINE    EAUSER        HDUSER
     C     *LIKE         DEFINE    EARMDS        HDRMDS
     C*
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANK        ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANK        ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANK        ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANK        ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANK        ##SDTS            1            ALLOW FUTURE SERV DT
     C*
     C*
     C* Retrieve the client id
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       XXCLT#            3
     C                   MOVE      QQCO#         XXCO#
     C*
     C                   TIME                    DSDTTM
     C*
     C* SET UP FOR READ OF EOB FILE
     C*
     C     EAKY          SETLL     PEA01
     C     EAKY          READE     PEA01                                  50
     C*
     C     *IN50         DOWEQ     *OFF
     C*
     C                   MOVE      Q1PGM         EAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DSTIME        EATIME
     C*
     C     CAKEY         CHAIN     PCA01                              50
     C     *IN50         DOWEQ     '0'
     C                   Z-ADD     EAEOB#        CACHK#
     C*
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C*
     C                   Z-ADD     DSTIME        CATIME
     C*
     C                   UPDATE    PCA01
     C*
     C     CAKEY         READE     PCA01                                  50
     C                   END
      *
     C                   MOVE      'H'           EABHRF
     C                   MOVE      'H'           EANHRF
     C*
     C                   MOVE      EASUB#        ##SUB#
     C                   MOVE      EAFND         ##GRP#
     C                   MOVE      EAEOB#        ##EOB#
     C                   MOVE      EAFYR         ##FYR
     C                   MOVE      EACASE        ##CASE
     C*
     C                   CALL      'FIXPRVCHK'
     C                   PARM      *BLANK        ##CALL            1
     C                   PARM      QQCO#         ##CO#             3
     C                   PARM                    ##SUB#            5
     C                   PARM                    ##GRP#            3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM                    ##EOB#            9
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM      *ZEROS        @@NAMT
     C*** Begin Chg *** 09/22/23 *******************************************
     C**********         PARM      *ZEROS        @@MRA
     C                   PARM      *ZEROS        @@OCA2
     C*** End   Chg *** 09/22/23 *******************************************
     C*
     C     EAOCA1        IFEQ      *ZEROS
     C     EAINPE        ADD       EAOUTE        EAOCA1
     C                   ADD       EAASCE        EAOCA1
     C                   ADD       EACLNE        EAOCA1
     C                   ENDIF
     C*
     C                   UPDATE    PEA01
     C*
     C     EOKY          CHAIN     PEO01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     'P'           EOSTAT
     C*
     C                   MOVE      Q1PGM         EOPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*
     C                   Z-ADD     DSTIME        EOTIME
     C*
     C                   UPDATE    PEO01
     C*
     C                   ENDIF
      *
     C     EAKY          READE     PEA01                                  50
      *
     C                   ENDDO                                                  *IN50 DOWEQ
      *
     C     TERM          TAG
      *
     C                   SETON                                        LR
