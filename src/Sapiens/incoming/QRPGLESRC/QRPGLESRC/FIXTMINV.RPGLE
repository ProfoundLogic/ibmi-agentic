     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  FIXAMTS - FIX THE AMOUNT FIELDS IN NAPBP, ETC C:62 08/14/98  *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FNDTML     UF   E           K DISK
     FFAEOLG    IF   E           K DISK
     FFAEAL1    IF   E           K DISK
     F*

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
     C     TMKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     EAKY          KLIST
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
     C*
     C                   TIME                    DSDTTM
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C*
     C                   READ      PEO01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     *ZEROS        EABINV
     C*
     C     ##PRVB        IFEQ      'Y'
     C*
     C     EAKY          CHAIN     PEA01                              99
     C*
     C                   ENDIF
     C*
     C*          ##PRVB    IFEQ 'Y'
     C*          EABINV    ANDNE*ZEROS
     C*          ##PRVB    ORNE 'Y'
     C*          EOINV#    ANDNE*ZEROS
     C*
     C                   EXSR      UPDINV
     C*
     C*                    ENDIF
     C*
     C                   READ      PEO01                                  99
     C*
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*
     C*================================================================
     C* UPDINV - UPDATE THE INVOICE NUMBER
     C*================================================================
     C     UPDINV        BEGSR
     C*
     C     TMKY          CHAIN     PTM01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C* UPDATE INVOICE NUMBER
     C*
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   Z-ADD     EABINV        TMINV#
     C*
     C                   ELSE
     C*
     C                   Z-ADD     EOINV#        TMINV#
     C*
     C                   ENDIF
     C*
     C                   UPDATE    PTM01
     C*
     C     TMKY          READE     PTM01                                  99
     C*
     C                   ENDDO
     C*
     C                   ENDSR
