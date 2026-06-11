     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N7122R - PBR/HBA BILL/RE-BILL - DETERMINE EOMB DATE RANGE
     H***************************************************************
     H* REVISIONS:                                                  *
     H* 07/05/95  C:35  REQ/PROJ: NW008                             *
     H*                 1. ADD MEDICAL MANAGEMENT BILLING FILE      *
     H***************************************************************
     FFAEOL4    IF   E           K DISK
     FFAHBL     IF   E           K DISK
     FNAPBL1    IF   E           K DISK
     F*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     FNAMBL1    IF   E           K DISK
     F*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******

     C*
     C     HBKY          KLIST
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
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C     MBKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C*   PARAMETER LIST FOR CALLED PROGRAM
     C     *ENTRY        PLIST
     C                   PARM                    QQEFDT            6
     C                   PARM                    QQETDT            6
     C*
     C*  INITIALIZATION SECTION
     C     *LIKE         DEFINE    EOTRDT        XXEFDT
     C     *LIKE         DEFINE    EOTRDT        XXETDT
     C*
     C                   Z-ADD     0             XXEFDT
     C                   Z-ADD     0             XXETDT
     C*
     C*  READ EOMB OPEN QUERY FILE
     C     *LOVAL        SETLL     PEO01
     C                   READ      PEO01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C     EOSYS         IFEQ      'P'
     C     EOBAMT        CABLE     *ZEROS        READN
     C                   ELSE                                                   ELSE EQ 'P'
     C     EOSYS         IFEQ      'H'
     C     HBKY          CHAIN     PHB01                              51
     C     *IN51         IFEQ      '0'
     C     HB1AAM        CABLE     *ZEROS        READN
     C                   END                                                    END 0 *IN51
     C                   ELSE                                                   ELSE EQ 'H'
     C     EOSYS         IFEQ      'U'
     C     PBKY          CHAIN     PPB01                              52
     C     *IN52         IFEQ      '0'
     C     PBBAMT        CABLE     *ZEROS        READN
     C                   END                                                    END 0 *IN52
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   ELSE                                                   ELSE EQ 'U'
     C     EOSYS         IFEQ      'M'
     C     MBKY          CHAIN     PMB01                              53
     C     *IN53         IFEQ      '0'
     C     MBBAMT        CABLE     *ZEROS        READN
     C                   END                                                    END 0 *IN53
     C                   END                                                    END EQ 'M'
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   END                                                    END EQ 'U'
     C                   END                                                    END EQ 'H'
     C                   END                                                    END EQ 'P'
     C*
     C* GET "FROM" DATE
     C     EOTRDT        IFLT      XXEFDT
     C     XXEFDT        OREQ      *ZEROS
     C                   Z-ADD     EOTRDT        XXEFDT
     C                   END                                                    END LT XXEFDT
     C*
     C* GET "TO" DATE
     C     EOTRDT        IFGT      XXETDT
     C     XXETDT        OREQ      *ZEROS
     C                   Z-ADD     EOTRDT        XXETDT
     C                   END                                                    END LT XXETDT
     C*
     C     READN         TAG
     C                   READ      PEO01                                  50
     C                   END                                                    END DOWEQ 50
     C*
     C* END OF PROGRAM
     C                   MOVE      XXEFDT        QQEFDT
     C                   MOVE      XXETDT        QQETDT
     C*
     C                   MOVE      '1'           *INLR
