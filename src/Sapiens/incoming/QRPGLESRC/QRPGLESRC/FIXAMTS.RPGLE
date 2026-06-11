     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  FIXAMTS - FIX THE AMOUNT FIELDS               C:62 03/20/01  *
     H*                                                MCMNT          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  11/20/01 C:62 PROJECT MC095 29950                            *
     H*                -ADD SECTION TO CREATE WORKFILE                *
     H*                -USE WORKFILE                                  *
     H*                                                               *
     H*****************************************************************
     F*** BEGIN ADD *** 11/20/01 **************************************
     FFXTMP     IF A E             DISK
     F                                     RENAME(PTM01:PTM02)
     FNDTML1    IF   E           K DISK
     F                                     RENAME(PTM01:PTM03)
     F*** END   ADD *** 11/20/01 **************************************
     FNDTML     IF   E           K DISK
     FFAEOLG    UF   E           K DISK
     FFAEAL1    UF   E           K DISK
     FNAMBL1    UF   E           K DISK
     FNAPBL1    UF   E           K DISK
     F*
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     TMKY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     TMKY2         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*** BEGIN ADD *** 11/20/01 **************************************
     C*
     C     TMKY3         KLIST
     C                   KFLD                    TMCO#
     C*
     C     TMKY4         KLIST
     C                   KFLD                    TMCO#
     C                   KFLD                    TMSUB#
     C                   KFLD                    TMFND
     C                   KFLD                    TMFYR
     C                   KFLD                    TMCASE
     C                   KFLD                    TMEOB#
     C                   KFLD                    TMRCN
     C*
     C     EAKY          KLIST
     C                   KFLD                    TMCO#
     C                   KFLD                    TMSUB#
     C                   KFLD                    TMFND
     C                   KFLD                    TMFYR
     C                   KFLD                    TMCASE
     C                   KFLD                    TMEOB#
     C                   KFLD                    TMRCN
     C*
     C     EOKY          KLIST
     C                   KFLD                    TMCO#
     C                   KFLD                    TMSUB#
     C                   KFLD                    TMFND
     C                   KFLD                    TMFYR
     C                   KFLD                    TMCASE
     C                   KFLD                    TMEOB#
     C*** END   ADD *** 11/20/01 **************************************
     C*
     C     MBKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     PBKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     *LIKE         DEFINE    EOBAMT        XXBAMT
     C     *LIKE         DEFINE    EODT          XXDT
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    EABCAM        XXBCAM
     C*** BEGIN ADD *** 11/20/01 **************************************
     C*
     C* BUILD WORK FILE
     C*
     C                   READ      PTM03                                  99
     C     *IN99         DOWEQ     *OFF
     C     TMINV#        IFEQ      *ZEROS
     C     TMSYS         ANDEQ     'M'
     C     TMINV#        OREQ      *ZEROS
     C     TMSYS         ANDEQ     'U'
     C                   WRITE     PTM02
     C                   FEOD      FXTMP
     C                   ENDIF
     C     TMKY4         SETGT     PTM03
     C     TMKY3         READE     PTM03                                  99
     C                   ENDDO
     C*** END   ADD *** 11/20/01 **************************************
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANKS       ##WCIF            1            WC INST
     C                   PARM      *BLANKS       ##MCIF            1            MC INST
     C                   PARM      *BLANKS       ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANKS       ##AR              1            MC AR INST
     C                   PARM      *BLANKS       ##NTWK            1            NETWORK INST
     C                   PARM      *BLANKS       ##UR              1            PRECERT INST
     C                   PARM      *BLANKS       ##MED             1            MEDMGMT INST
     C                   PARM      *BLANKS       ##EDI             1            EDI INST
     C                   PARM      *BLANKS       ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANKS       ##RHAB            1            REHAB INST
     C                   PARM      *BLANKS       ##MSS             1            MS SUPPORT
     C                   PARM      *BLANKS       ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANKS       ##PRVB            1            PROVIDER BILL
     C                   PARM      *BLANKS       ##NPWC            1            NON-PDS WC  LS
     C*
     C     ##PRVB        IFEQ      'Y'
     C                   SETON                                        16
     C                   ELSE
     C                   SETOFF                                       16
     C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        COUNT             5 0
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*
     C     *IN16         IFEQ      *ON
     C*
     C*** BEGIN CHG *** 11/20/01 **************************************
     C**********           READ PEA01                    99
     C     1             CHAIN     PTM02                              99
     C*** END   CHG *** 11/20/01 **************************************
     C*
     C     *IN99         DOWEQ     *OFF
     C*** BEGIN ADD *** 11/20/01 **************************************
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C*** END   ADD *** 11/20/01 **************************************
     C                   EXSR      CLCAMT
     C                   EXSR      UPDTPB
     C*** BEGIN ADD *** 11/20/01 **************************************
     C                   ENDIF
     C*** END   ADD *** 11/20/01 **************************************
     C*** BEGIN CHG *** 11/20/01 **************************************
     C**********           READ PEA01                    99
     C                   READ      PTM02                                  99
     C*** END   CHG *** 11/20/01 **************************************
     C                   ENDDO
     C*
     C                   ELSE
     C*
     C*** BEGIN CHG *** 11/20/01 **************************************
     C**********           READ PEO01                    99
     C     1             CHAIN     PTM02                              99
     C*** END   CHG *** 11/20/01 **************************************
     C*
     C     *IN99         DOWEQ     *OFF
     C*** BEGIN ADD *** 11/20/01 **************************************
     C     EOKY          CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C*** END   ADD *** 11/20/01 **************************************
     C                   EXSR      CLCAMT
     C                   EXSR      UPDTBE
     C*** BEGIN ADD *** 11/20/01 **************************************
     C                   ENDIF
     C*** END   ADD *** 11/20/01 **************************************
     C*** BEGIN CHG *** 11/20/01 **************************************
     C**********           READ PEO01                    99
     C                   READ      PTM02                                  99
     C*** END   CHG *** 11/20/01 **************************************
     C*
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C     *INOA         IFEQ      *ON
     C*
     C                   EXCEPT    HEAD
     C                   SETOFF                                       OA
     C*
     C                   ENDIF
     C*
     C                   EXCEPT    TOT
     C*
     C                   SETON                                        LR
     C*
     C*================================================================
     C* CLCAMT - CALCULATES BILL AMOUNT.
     C*================================================================
     C     CLCAMT        BEGSR
     C*
     C                   Z-ADD     *ZEROS        XXBAMT
     C*
     C     *IN16         IFEQ      *ON
     C     TMKY2         CHAIN     PTM01                              99
     C                   ELSE
     C     TMKY1         CHAIN     PTM01                              99
     C                   ENDIF
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C* CALCULATE ORIGINAL AMOUNT.
     C*
     C     TMMILE        MULT      TMMIRT        XXMIRT            9 2
     C*
     C     TMHRS         MULT      TMHRRT        XXHRRT            9 2
     C*
     C                   Z-ADD     TMMSCG        XXMSCG            9 2
     C*
     C     XXMIRT        ADD       XXHRRT        HDBAMT            9 2
     C                   ADD       XXMSCG        HDBAMT
     C*
     C                   ADD       HDBAMT        XXBAMT
     C*
     C*
     C     *IN16         IFEQ      *ON
     C     TMKY2         READE     PTM01                                  99
     C                   ELSE
     C     TMKY1         READE     PTM01                                  99
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* DETAIL - WRITES DETAIL LINE IN REPORT
     C*================================================================
     C     DETAIL        BEGSR
     C*
     C     *INOA         IFEQ      *ON
     C*
     C                   EXCEPT    HEAD
     C                   SETOFF                                       OA
     C*
     C                   ENDIF
     C*
     C                   EXCEPT    DET
     C*
     C                   ADD       1             COUNT
     C*
     C                   ENDSR
     C*================================================================
     C* UPDTPB - Updates AMOUNTS - PROVIDER BILLS
     C*================================================================
     C     UPDTPB        BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   SELECT
     C**
     C     EASYS         WHENEQ    'U'
     C*
     C                   Z-ADD     EABAMT        XXAMTB            9 2
     C                   Z-ADD     XXBAMT        EABAMT
     C                   ADD       EABCAM        EABAMT
     C                   Z-ADD     EABAMT        XXAMTA            9 2
     C*
     C     XXAMTA        IFNE      XXAMTB
     C*
     C     XXAMTA        SUB       XXAMTB        XXDIFF            9 2
     C*
     C                   MOVEL(P)  Q1USER        EAUSER
     C                   MOVE      'F'           EAPGM
     C*
     C     XXBAMT        IFNE      *ZEROS
     C*
     C                   MOVEL     'Y'           EATMFL
     C*
     C                   ELSE
     C*
     C                   MOVEL     ' '           EATMFL
     C*
     C                   ENDIF
     C*
     C                   UPDATE    PEA01
     C                   EXSR      DETAIL
     C*
     C                   ELSE
     C*
     C                   UPDATE    PEA01
     C*
     C                   ENDIF
     C**
     C     EASYS         WHENEQ    'M'
     C*
     C                   Z-ADD     EABAMT        XXAMTB            9 2
     C                   Z-ADD     XXBAMT        EABAMT
     C                   ADD       EABCAM        EABAMT
     C                   Z-ADD     EABAMT        XXAMTA            9 2
     C*
     C     XXAMTA        IFNE      XXAMTB
     C*
     C     XXAMTA        SUB       XXAMTB        XXDIFF            9 2
     C*
     C                   MOVEL(P)  Q1USER        EAUSER
     C                   MOVE      'F'           EAPGM
     C*
     C     XXBAMT        IFNE      *ZEROS
     C*
     C                   MOVEL     'Y'           EATMFL
     C*
     C                   ELSE
     C*
     C                   MOVEL     ' '           EATMFL
     C*
     C                   ENDIF
     C*
     C                   UPDATE    PEA01
     C                   EXSR      DETAIL
     C*
     C                   ELSE
     C*
     C                   UPDATE    PEA01
     C*
     C                   ENDIF
     C*
     C                   OTHER
     C                   UPDATE    PEA01
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDTBE - Updates AMOUNTS - BILL ENTRY
     C*================================================================
     C     UPDTBE        BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   SELECT
     C**
     C     EOSYS         WHENEQ    'U'
     C     PBKY          CHAIN     PPB01                              99
     C*
     C     *IN99         IFEQ      '0'
     C*
     C                   Z-ADD     PBBAMT        XXAMTB            9 2
     C                   Z-ADD     XXBAMT        PBBAMT
     C                   ADD       PBCAMT        PBBAMT
     C                   Z-ADD     PBBAMT        XXAMTA            9 2
     C*
     C     XXAMTA        IFNE      XXAMTB
     C*
     C     XXAMTA        SUB       XXAMTB        XXDIFF            9 2
     C*
     C                   MOVEL(P)  Q1USER        PBUSER
     C                   MOVE      'F'           PBPGM
     C*
     C                   UPDATE    PPB01
     C                   EXSR      DETAIL
     C*
     C                   ELSE
     C*
     C                   UPDATE    PPB01
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C**
     C     EOSYS         WHENEQ    'M'
     C     MBKY          CHAIN     PMB01                              99
     C*
     C     *IN99         IFEQ      '0'
     C*
     C                   Z-ADD     MBBAMT        XXAMTB            9 2
     C                   Z-ADD     XXBAMT        MBBAMT
     C                   ADD       MBCAMT        PBBAMT
     C                   Z-ADD     MBBAMT        XXAMTA            9 2
     C*
     C     XXAMTA        IFNE      XXAMTB
     C*
     C     XXAMTA        SUB       XXAMTB        XXDIFF            9 2
     C*
     C                   MOVEL(P)  Q1USER        MBUSER
     C                   MOVE      'F'           MBPGM
     C*
     C                   UPDATE    PMB01
     C                   EXSR      DETAIL
     C*
     C                   ELSE
     C*
     C                   UPDATE    PMB01
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     XXBAMT        IFNE      *ZEROS
     C*
     C                   MOVEL     'Y'           EOFLG5
     C*
     C                   ELSE
     C*
     C                   MOVEL     ' '           EOFLG5
     C*
     C                   ENDIF
     C*
     C                   UPDATE    PEO01
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     OQSYSPRT   E            HEAD           1 02
     O                       DSTIME               8 '  :  :  '
     O                                           49 'FIXAMTS2'
     O                       DSDATE             108 '  /  /    '
     O          E            HEAD           1
     O                                           11 'System Type'
     O                                           20 'EOMB#'
     O                                           33 'Invoice #'
     O                                           48 'Before Amount'
     O                                           62 'After Amount'
     O                                           81 'Amount Difference'
     O                                           87 'Sub#'
     O                                           94 'Grp'
     O                                          101 'Year'
     O                                          108 'Case'
     O          E            DET            1
     O              N16      EOSYS                6
     O              N16      EOEOB#        4     22
     O              N16      EOINV#        4     31
     O               16      EASYS                6
     O               16      EAEOB#        4     22
     O               16      EABINV        4     31
     O                       XXAMTB        4     46
     O                       XXAMTA        4     60
     O                       XXDIFF        4     74
     O              N16      EOSUB#        4     88
     O              N16      EOFND         4     94
     O              N16      EOFYR         4    100
     O              N16      EOCASE        4    111
     O               16      EASUB#        4     88
     O               16      EAFND         4     94
     O               16      EAFYR         4    100
     O               16      EACASE        4    111
     O          E            TOT            1
     O                                           22 'Total number of Bills:'
     O                       COUNT         4     29
