     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*                                                               *
     H/TITLE W8073R2 -  Auto Submit - Write Transmission Mgmt File.   *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* 09/28/10  C:220  PROJECT: 139WNN Priss#53                     *
     H*                  Added logic to write WMM1P file same as W8070R
     H*                  have in place.                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 02/08/14 C:180 SUPPORT PRISS 027 - WC2234
     H*                Re-compiled only.
     F*****************************************************************
     FWMM1L     UF A E           K DISK
     FWMLGL3    UF   E           K DISK
     FWXFRL     UF   E           K DISK
     FWXSUL     UF   E           K DISK
     FWXAJL     UF   E           K DISK
     FWXPIL     UF   E           K DISK
     FWXRDL     UF   E           K DISK
     FWXT2L     UF   E           K DISK
     FWXT3L     UF   E           K DISK
     FWMFNL     IF   E           K DISK
     F*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  X1ACLC                 1     25
     D  X1CO#                  1      3  0
     D  X1FND                  4      6  0
     D  X1FYR                  7      9  0
     D  X1CASE                10     16  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C*
     C                   TIME                    DSDTTM
     C*
     C     M1KY          KLIST
     C                   KFLD                    @I1CO#
     C                   KFLD                    @I1JRST
     C                   KFLD                    XXEXDT
     C                   KFLD                    XXTSET
     C                   KFLD                    XXEDIF
     C*
     C*   KEY LIST FOR THE 148 SUBMISSION FLAT FILE
     C*
     C     FRKY          KLIST
     C                   KFLD                    @I1CO#
     C                   KFLD                    @I1JRST
     C                   KFLD                    XXEXDT
     C                   KFLD                    XXTSET
     C                   KFLD                    X1ACLC
     C*
     C*   KEY LIST FOR THE A49 SUBMISSION FLAT FILE
     C*
     C     SUKY          KLIST
     C                   KFLD                    @I1CO#
     C                   KFLD                    @I1JRST
     C                   KFLD                    XXEXDT
     C                   KFLD                    XXTSET
     C                   KFLD                    X1ACLC
     C*
     C*   KEY LIST FOR THE EDI LOG HISTORY FILE
     C*
     C     LGKY          KLIST
     C                   KFLD                    @I1CO#
     C                   KFLD                    @I1JRST
     C                   KFLD                    XXEXDT
     C                   KFLD                    XXTSET
     C*
     C*   FUND MASTER KEY
     C*
     C     FNKY          KLIST
     C                   KFLD                    @I1CO#
     C                   KFLD                    XXFND             3 0
     C*
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
     C*
     C*   AN EXTRACT CANNOT BE RE-SUBMITTED IF THE DATA HAS BEEN DOWNLOADED
     C*   OR THE EXTRACT IS BEING PROCESSED.
     C*
     C*  148 PROCESSING
     C*
     C*
     C                   Z-ADD     @I1CO#        X1CO#
     C                   MOVEL     @I1EDIF       XXEDIF            3
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT            7 0
     C*
     C                   MOVEL     @I1TSID       XXTSID            1
     C     XXTSID        IFEQ      'B'
     C     XXTSID        OREQ      '1'
     C                   MOVEL     '148'         XXTSET            3
     C*
     C*   IF THIS IS A VALID RE-SUBMISSION, FIX VARIOUS FILES WHICH ARE
     C*   UPDATED DURING THIS PROCESS.
     C*
     C     M1KY          CHAIN     PM101                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   FIX RECORDS IN THE EDI LOG HISTORY FILE
     C*
     C     LGKY          CHAIN     PLG01                              62
     C     *IN62         DOWEQ     '0'
     C*
     C                   MOVEL     LGFND         XXFND
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   IF        *IN60 = *OFF
     C                   MOVEL     FNEDIF        X#EDIF            3
     C                   ELSE
     C                   MOVEL     ' '           X#EDIF
     C                   ENDIF
     C*
     C     @I1EDIF       IFEQ      X#EDIF
     C*
     C     LGSTS         IFEQ      'EX'
     C     LGSTS         OREQ      'RE'
     C     LGSTS         OREQ      'EC'
     C     LGSTS         OREQ      'EI'
     C                   MOVE      'EC'          LGSTS
     C                   MOVEL     Q1USER        LGUSER
     C                   MOVEL     Q1PGM         LGPGM
     C                   MOVE      *ZEROS        LGEXDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        LGTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDT
     C                   UPDATE    PLG01
     C*
     C*   DELETE THE EXTRACT RECORD FOR THE ENTERED DATE AND
     C*   ASSOCIATED CLAIM.
     C*
     C                   MOVE      LGFND         X1FND
     C                   MOVE      LGFYR         X1FYR
     C                   MOVE      LGCASE        X1CASE
     C     FRKY          CHAIN     PFR01                              61
     C  N61              DELETE    PFR01
     C                   END
     C                   ENDIF
     C*
     C     LGKY          READE     PLG01                                  62
     C                   ENDDO
     C*
     C*   FIX THE SUBMISSION MANAGEMENT RECORD
     C*
     C                   MOVE      'P'           M1STAT
     C                   MOVEL     Q1USER        M1IUSR
     C                   MOVEL     Q1PGM         M1IPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        M1ITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M1IDT
     C                   UPDATE    PM101
     C*
     C                   ELSE
     C*
     C*   IF THIS IS AN ORIGINAL SUBMISSION, CREATE THE MANAGEMENT RECORD
     C*
     C                   MOVEL     X1CO#         M1CO#
     C                   MOVEL     @I1JRST       M1JST
     C                   Z-ADD     XXEXDT        M1EXDT
     C                   MOVE      '148'         M1TSET
     C                   Z-ADD     0             M1OTDT
     C                   Z-ADD     0             M1MTCD
     C                   Z-ADD     0             M1ARCV
     C                   Z-ADD     0             M1AENT
     C                   Z-ADD     0             M1148T
     C                   Z-ADD     0             M1148S
     C                   Z-ADD     0             M1A49T
     C                   Z-ADD     0             M1A49S
     C                   Z-ADD     0             M1OTTM
     C                   MOVE      'P'           M1STAT
     C                   MOVEL     @I1EDIF       M1EDIF
     C                   MOVEL     Q1USER        M1IUSR
     C                   MOVEL     Q1PGM         M1IPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        M1ITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M1IDT
     C                   WRITE     PM101
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                    148 PROCESSING
     C*
     C*  A49 PROCESSING
     C*
     C     XXTSID        IFEQ      'B'
     C     XXTSID        OREQ      'A'
     C                   MOVEL     'A49'         XXTSET
     C*
     C*** IF THIS IS A VALID RE-SUBMISSION, FIX VARIOUS FILES WHICH ARE
     C*** UPDATED DURING THIS PROCESS.
     C*
     C     M1KY          CHAIN     PM101                              61
     C     *IN61         IFEQ      '0'
     C*
     C*   FIX RECORDS IN THE EDI LOG HISTORY FILE
     C*
     C     LGKY          CHAIN     PLG01                              62
     C     *IN62         DOWEQ     '0'
     C*
     C                   MOVEL     LGFND         XXFND
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   IF        *IN60 = *OFF
     C                   MOVEL     FNEDIF        X#EDIF
     C                   ELSE
     C                   MOVEL     ' '           X#EDIF
     C                   ENDIF
     C*
     C     @I1EDIF       IFEQ      X#EDIF
     C*
     C     LGSTS         IFEQ      'EX'
     C     LGSTS         OREQ      'RE'
     C     LGSTS         OREQ      'EC'
     C     LGSTS         OREQ      'EI'
     C                   MOVE      'EC'          LGSTS
     C                   MOVEL     Q1USER        LGUSER
     C                   MOVEL     Q1PGM         LGPGM
     C                   MOVE      *ZEROS        LGEXDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        LGTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDT
     C                   UPDATE    PLG01
     C*
     C*   DELETE THE EXTRACT RECORDS FOR THE ENTERED DATE AND
     C*   ASSOCIATED CLAIM.
     C*
     C                   MOVE      LGFND         X1FND
     C                   MOVE      LGFYR         X1FYR
     C                   MOVE      LGCASE        X1CASE
     C*
     C     SUKY          CHAIN     PSU01                              61
     C  N61              DELETE    PSU01
     C*
     C     *IN62         DOUEQ     '1'
     C     SUKY          DELETE    PAJ01                              62
     C                   END
     C*
     C     *IN62         DOUEQ     '1'
     C     SUKY          DELETE    PPI01                              62
     C                   END
     C*
     C     *IN62         DOUEQ     '1'
     C     SUKY          DELETE    PRD01                              62
     C                   END
     C*
     C     *IN62         DOUEQ     '1'
     C     SUKY          DELETE    PT201                              62
     C                   END
     C*
     C     *IN62         DOUEQ     '1'
     C     SUKY          DELETE    PT301                              62
     C                   END
     C*
     C                   END
     C                   ENDIF
     C*
     C     LGKY          READE     PLG01                                  62
     C                   ENDDO
     C*
     C*   FIX THE SUBMISSION MANAGEMENT RECORD
     C*
     C                   MOVE      'P'           M1STAT
     C                   MOVEL     Q1USER        M1IUSR
     C                   MOVEL     Q1PGM         M1IPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        M1ITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M1IDT
     C                   UPDATE    PM101
     C*
     C                   ELSE
     C*
     C*   IF THIS IS AN ORIGINAL SUBMISSION, CREATE THE MANAGEMENT RECORD
     C*
     C                   MOVEL     X1CO#         M1CO#
     C                   MOVEL     @I1JRST       M1JST
     C                   Z-ADD     XXEXDT        M1EXDT
     C                   MOVE      'A49'         M1TSET
     C                   Z-ADD     0             M1OTDT
     C                   Z-ADD     0             M1MTCD
     C                   Z-ADD     0             M1ARCV
     C                   Z-ADD     0             M1AENT
     C                   Z-ADD     0             M1148T
     C                   Z-ADD     0             M1148S
     C                   Z-ADD     0             M1A49T
     C                   Z-ADD     0             M1A49S
     C                   Z-ADD     0             M1OTTM
     C                   MOVE      'P'           M1STAT
     C                   MOVEL     @I1EDIF       M1EDIF
     C                   MOVEL     Q1USER        M1IUSR
     C                   MOVEL     Q1PGM         M1IPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        M1ITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M1IDT
     C                   WRITE     PM101
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                   A49 PROCESSI  NG
     C*
     C                   MOVE      '1'           *INLR
     C*
