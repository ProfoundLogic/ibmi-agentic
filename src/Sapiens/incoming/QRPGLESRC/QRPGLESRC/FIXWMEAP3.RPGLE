     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXWMEAP3 - Copy back future A/R from failed Reinstatement
     H***************************************************************
     H*
     H*   PROGRAM DESCRIPTION:
     H*      This program attempts to copy future A/R billing
     H*      records from WMXAP to WMEAP.
     H*
     H*      This program is created to duplicate processing that would
     H*      normally be done by W7167R.
     H*
     H***************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/11/09 C:61  Seq#: 70656                                    *
     H*                 Created specifically to resolve #70656.
     H*                                                               *
     H***************************************************************
      *
     FWMEAL14   O  A E           K DISK
     FWMXAL     UF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDCXL     IF   E           K DISK
      *
      *** Output File
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
      *
      *
      *
      *

     D                 DS
     D  ED                     1     35  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS
     D  CD                     1     35  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *
      ************************************************************
      * KLISTS
      ************************************************************

     C*
     C* EMPLOYER POLICY YEAR
     C*
     C     EMKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
      *
      *  CANCELLATION DETAIL FILE
      *
     C     CXKY          KLIST
     C                   KFLD                    XXCO#             3 0
     C*
     C* Cancellation A/R detail file
     C*
     C     XAKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXFYR
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C*
     C* A/R detail file
     C*
     C     EAKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    CXFYR
      *
      *
      *===============================================================
      *  *LIKE DEFNS
      *===============================================================
     C*
     C*
     C*
     C**************************************************
     C* PLIST
     C**************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@MODE            1
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
     C***********************************************
     C*   MAIN
     C***********************************************
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT             7 0
      *
     C                   MOVE      @@CO#         XXCO#
     C                   SETON                                        OA
     C   OA              EXCEPT    HEAD
     C   OA              SETOFF                                       OA
      *
      * For each CX detail record with Reinstatement issued 11/06/2009
      * or later.
      *
     C     CXKY          CHAIN     PCX01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     CXRDAT        IFGE      1091106
     C     EMKY          CHAIN     PEM01                              60
      *
      *  Get Employer Effective/Cancellation date
      *
     C                   Z-ADD     5             X                 3 0
      *
     C     ED(X)         DOWEQ     *ZERO
     C     X             ANDGT     1
     C                   SUB       1             X
     C                   ENDDO
      *
     C                   Z-ADD     CD(X)         XXCXDT            7 0
      *
      *  If employer not currently cancelled.
      *
     C     XXCXDT        IFEQ      0
      *
      *  If WMXAP records exist for policy then transfer to WMEAP
      *
     C     XAKY          CHAIN     PXA01                              60
      *
     C     @@MODE        IFEQ      'U'
     C   OA              EXCEPT    HEAD
     C   OA              SETOFF                                       OA
     C  N60              EXCEPT    DET
      *
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      WRTREC
     C                   DELETE    PXA01
     C     XAKY          READE     PXA01                                  60
     C                   ENDDO                                                  *IN60=*OFF
      *
     C                   ELSE
      *
     C   OA              EXCEPT    HEAD
     C   OA              SETOFF                                       OA
     C  N60              EXCEPT    DET
     C                   ENDIF                                                  @@MODE
      *
     C                   ENDIF                                                  CXDT
      *
     C                   ENDIF                                                  RDAT
      *
     C     CXKY          READE     PCX01                                  60
     C                   ENDDO
      *
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
      *
      *
     C*================================================================
     C*  WRTREC - writes record to a/r file exactly as they were when
     C*           they were deleted except for the maintenance fields
     C*           and the transaction date field
     C*================================================================
     C     WRTREC        BEGSR
     C*
     C                   CLEAR                   PEA01
     C                   MOVE      XACO#         EACO#
     C                   MOVE      XACO#         EACO#
     C                   MOVE      XAFND         EAFND
     C                   MOVE      XAFYR         EAFYR
     C                   MOVE      XAEMP#        EAEMP#
     C                   MOVE      XADIV         EADIV
     C                   MOVE      XAARCD        EAARCD
     C                   MOVE      XAARTY        EAARTY
     C                   MOVE      XADESC        EADESC
     C                   MOVE      XAAMT         EAAMT
     C                   MOVE      XAPODT        EAPODT
     C                   MOVE      XAEDIT        EAEDIT
     C                   MOVE      XASTAT        EASTAT
     C                   MOVE      XABUL1        EABUL1
     C                   MOVE      XACHK#        EACHK#
     C                   MOVE      XANINV        EANINV
     C                   MOVE      XALATE        EALATE
     C                   MOVE      XAFL01        EAFL01
     C                   MOVE      XAFL02        EAFL02
     C                   MOVE      XADT01        EADT01
     C                   MOVE      XADT02        EADT02
     C                   MOVE      XACTYP        EACTYP
     C                   MOVE      XARDIV        EARDIV
      *
      *    Update these fields with reinstatement info
      *
     C                   Z-ADD     SYSDT         EATRDT
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   ADD       1             DSTIME
     C                   MOVE      DSTIME        EATIME
     C                   Z-ADD     SYSDT         EADT
      *
     C                   WRITE     PEA01
      *
     C                   ENDSR                                                  WRTREC
      *
      *
     OQSYSPRT   E            HEAD           2 06
     O                       Q1PGM               10
     O                                         + 20 'FIXWMEAP3'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           3
     O                                            6 'CO#'
     O                                         +002 'FND'
     O                                         +002 'FYR'
     O                                         +002 'EMP#      '
     O                                         +002 'DIV  '
      *
     O          E            DET            1
     O                       CXCO#         Z      6
     O                       CXFND         Z   +002
     O                       CXFYR         Z   +002
     O                       CXEMP#        Z   +002
     O                       CXDIV         Z   +002
      *
      *
      *
