     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*                                                               *
     H* API - FA755R - Retrieve Allowance for Home Health Bills       *DATA
     H*******************************************************************
     H*                                                               * *
     H*  Called From: F4000NTX                                        * *
     H*  Calls To:    None                                            * *
     H*                                                               * *
     H*******************************************************************
     H* Created on 04/04/2008 by c:62 for  Project TX040              * *
     H*******************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     FFMHHL2    IF   E           K DISK
     F                                     INFDS(DBFHH)
     FFMCDL     IF   E           K DISK

     D DBFHH           DS
     D  HHRRN                397    400B 0

     C*================================================================
     C* Parameter lists
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@SS#
     C                   PARM                    @@PRCD
     C                   PARM                    @@EFDT
     C                   PARM                    @@MOD1
     C                   PARM                    @@MOD2
     C                   PARM                    @@ALOW
     C                   PARM                    @@RPT
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RTCD            1
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
     C*================================================================
     C* Key lists
     C*================================================================
     C*
     C     CDKY          KLIST
     C                   KFLD                    @@SS#
     C*
     C     HHKY1         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@PRCD
     C                   KFLD                    KKFAGE
     C                   KFLD                    KKTAGE
     C                   KFLD                    KKMOD1
     C                   KFLD                    KKMOD2
     C                   KFLD                    KKTOS
     C                   KFLD                    @@EFDT
     C*
     C     HHKY2         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@PRCD
     C                   KFLD                    KKFAGE
     C                   KFLD                    KKTAGE
     C                   KFLD                    KKMOD1
     C                   KFLD                    KKMOD2
     C                   KFLD                    KKTOS
     C*================================================================
     C* Like Defines
     C*================================================================
     C     *LIKE         DEFINE    HHCO#         @@CO#
     C     *LIKE         DEFINE    CDSS#         @@SS#
     C     *LIKE         DEFINE    HHPRCD        @@PRCD
     C     *LIKE         DEFINE    HHEFDT        @@EFDT
     C     *LIKE         DEFINE    HHMOD1        @@MOD1
     C     *LIKE         DEFINE    HHMOD2        @@MOD2
     C     *LIKE         DEFINE    HHALOW        @@ALOW
     C     *LIKE         DEFINE    HHRPT         @@RPT
     C*
     C     *LIKE         DEFINE    HHTOS         KKTOS
     C     *LIKE         DEFINE    HHFAGE        KKFAGE
     C     *LIKE         DEFINE    HHTAGE        KKTAGE
     C     *LIKE         DEFINE    HHMOD1        KKMOD1
     C     *LIKE         DEFINE    HHMOD2        KKMOD2
     C*================================================================
     C* Main Processing
     C*================================================================
     C*
     C                   EXSR      GETAGE
     C*
     C     AGE           IFLE      20
     C                   Z-ADD     0             KKFAGE
     C                   Z-ADD     20            KKTAGE
     C                   ELSE
     C                   Z-ADD     21            KKFAGE
     C                   Z-ADD     999           KKTAGE
     C                   ENDIF
     C*
     C                   MOVEL(P)  @@MOD1        KKMOD1
     C                   MOVEL(P)  @@MOD2        KKMOD2
     C                   MOVEL(P)  '1'           KKTOS
     C     HHKY1         SETGT     PHH01
     C     HHKY2         READPE    PHH01                                  99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     HHRRN         @@RRN
     C                   Z-ADD     HHALOW        @@ALOW
     C                   MOVE      HHRPT         @@RPT
     C                   ELSE
     C                   MOVEL(P)  'C'           KKTOS
     C     HHKY1         SETGT     PHH01
     C     HHKY2         READPE    PHH01                                  99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     HHRRN         @@RRN
     C                   Z-ADD     HHALOW        @@ALOW
     C                   MOVE      HHRPT         @@RPT
     C                   ELSE
     C                   MOVEL     'F'           @@RTCD
     C                   ENDIF
     C                   ENDIF
     C*
     C     @@RTCD        IFEQ      'F'
     C                   MOVEL     ' '           @@RTCD
     C                   MOVEL(P)  @@MOD2        KKMOD1
     C                   MOVEL(P)  @@MOD1        KKMOD2
     C                   MOVEL(P)  '1'           KKTOS
     C     HHKY1         SETGT     PHH01
     C     HHKY2         READPE    PHH01                                  99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     HHRRN         @@RRN
     C                   Z-ADD     HHALOW        @@ALOW
     C                   MOVE      HHRPT         @@RPT
     C                   ELSE
     C                   MOVEL(P)  'C'           KKTOS
     C     HHKY1         SETGT     PHH01
     C     HHKY2         READPE    PHH01                                  99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     HHRRN         @@RRN
     C                   Z-ADD     HHALOW        @@ALOW
     C                   MOVE      HHRPT         @@RPT
     C                   ELSE
     C                   MOVEL     'F'           @@RTCD
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   RETURN
     C*================================================================
     C* GETAGE - Get the Age
     C*================================================================
     C     GETAGE        BEGSR
     C*
     C     CDKY          CHAIN     PCD01                              99
     C     CDDTOB        IFEQ      0
     C     *IN99         OREQ      *ON
     C                   Z-ADD     0             AGE               3 0
     C                   ELSE
     C*
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##DTOB
     C*
     C                   CALL      'SRAGE'
     C                   PARM                    ##DTOB            8
     C                   PARM                    ##AGE             3
     C*
     C                   MOVE      ##AGE         AGE
     C                   ENDIF
     C*
     C                   ENDSR
