     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - FA733R - Checks for an active network at Employer Level  DATA
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA733R                                            *
     H*    CREATED: 08/09/01  C:062 PROJECT:95MBA                     *
     H*    CALLED FROM: F0350R01                                      *
     H*    CALLS TO: NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*****************************************************************
     FFDENL1    IF   E           K DISK
     FFMS4L1    IF   E           K DISK
     F*

     D NWCD            S              5    DIM(999)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0
     C                   PARM                    @@FEIN            9 0
     C                   PARM                    @@MOD             3 0
     C                   PARM                    @@SYS             1
     C                   PARM                    @@FRDT            8 0
     C                   PARM                    @@TODT            8 0
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@NWCT            7 0
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     ENKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C                   KFLD                    @@EMP#
     C                   KFLD                    @@DIV
     C                   KFLD                    S4NWCD
     C*
     C     S4KY          KLIST
     C                   KFLD                    @@FEIN
     C                   KFLD                    @@MOD
     C*
     C     *LIKE         DEFINE    ENCO#         KKCO#
     C     *LIKE         DEFINE    ENSUB#        KKSUB#
     C     *LIKE         DEFINE    ENFND         KKFND
     C     *LIKE         DEFINE    ENEMP#        KKEMP#
     C     *LIKE         DEFINE    ENDIV         KKDIV
     C     *LIKE         DEFINE    ENNWCD        KKNWCD
     C     *LIKE         DEFINE    ENNWCD        XXNWCD
     C     *LIKE         DEFINE    ENEFDT        XXEFDT
     C     *LIKE         DEFINE    ENEXDT        XXEXDT
     C*
     C                   CLEAR                   NWCD
     C*
     C                   Z-ADD     @@FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   Z-ADD     @@TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
     C*
     C                   MOVEL(P)  'F'           @@RTCD
     C                   Z-ADD     0             XXNWCT            7 0
     C                   MOVE      *BLANKS       XXNWCD
     C*
     C* READ THRU PROVIDER NETWORKS
     C*
     C     S4KY          CHAIN     PS401                              99
     C     *IN99         DOWEQ     *OFF
     C     S4SYS         IFEQ      @@SYS
     C                   EXSR      CHKEND
     C                   ENDIF
     C     S4KY          READE     PS401                                  99
     C                   ENDDO
     C*
     C     XXNWCT        IFGT      *ZEROS
     C                   MOVEL     *BLANKS       @@RTCD
     C                   Z-ADD     XXNWCT        @@NWCT
     C                   ENDIF
     C*
     C* RETURN TO CALLING PROGRAM
     C*
     C                   RETURN
     C*****************************************************************
     C* CHKEND - CHECK EMPLOYER NETWORK CONTRACT DATES
     C*****************************************************************
     C     CHKEND        BEGSR
     C*
     C     ENKY          CHAIN     PEN01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     ENEFDT        IFGE      XXEFDT
     C     ENEFDT        ANDLE     XXEXDT
     C     ENEXDT        ORGE      XXEFDT
     C     ENEXDT        ANDLE     XXEXDT
     C     ENEFDT        ORLE      XXEFDT
     C     ENEXDT        ANDEQ     0
     C     ENEFDT        ORLT      XXEFDT
     C     ENEXDT        ANDGT     XXEXDT
     C                   Z-ADD     1             X                 3 0
     C     ENNWCD        LOOKUP    NWCD(X)                                99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     1             X
     C     '     '       LOOKUP    NWCD(X)                                99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  ENNWCD        NWCD(X)
     C                   ENDIF
     C                   ADD       1             XXNWCT
     C                   ENDIF                                                  *IN99 *OFF
     C                   ENDIF                                                  EFDT >= EFDT
     C*
     C     ENKY          READE     PEN01                                  99
     C                   ENDDO
     C*
     C                   ENDSR
