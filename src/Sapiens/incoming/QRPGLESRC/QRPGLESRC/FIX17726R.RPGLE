     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIX17726R - Correct cancellation history records
     H*****************************************************************
     H*                                                               *
     H*  Cancellation history records were being written without a    *
     H*  request # because of a previous change.  This program looks  *
     H*  at the canx history file (wdxhp) for each record with xhreq# *
     H*  equal to 0.  The file is updated with the appropriate req#   *
     H*  If the cancellation was reinstated, another canx history rec *
     H*  is written to correspond.                                    *
     H*                                                               *
     H*  Created: C:87                                                *
     H*****************************************************************
     F* Cancellation history file
     FWDXHL14   UF A E           K DISK
     F*
     F* Cancellation detail file
     FWDCXL12   IF   E           K DISK
     F*
     F* Admin/DARTS file
     FWMEXL     IF   E           K DISK
     F*
     F* DARTS file
     FSDCMLD    UF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*IN10)

     D*
     D*
     D                 DS
     D  KKKEY                  1     40
     D  ZZCO#                  1      3  0
     D  ZZFND                  4      6  0
     D  ZZEMP#                 7     15  0
     D  ZZDIV                 16     20  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C*   Cancellation history
     C     XHKY          KLIST
     C                   KFLD                    KKCDAT
     C*
     C*   Cancellation detail
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKCDAT
     C*
     C     CXKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*  Admin/DARTS file
     C     EXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  DARTS file
     C     CMKY          KLIST
     C                   KFLD                    KKKEY
     C                   KFLD                    KKRMCD
     C                   KFLD                    KKDATE
     C*
     C*  Like defines
     C     *LIKE         DEFINE    XHREQ#        KKREQ#
     C     *LIKE         DEFINE    XHREQ#        XXREQ#
     C     *LIKE         DEFINE    XHCO#         KKCO#
     C     *LIKE         DEFINE    XHFND         KKFND
     C     *LIKE         DEFINE    XHFYR         KKFYR
     C     *LIKE         DEFINE    XHEMP#        KKEMP#
     C     *LIKE         DEFINE    XHDIV         KKDIV
     C     *LIKE         DEFINE    XHCDAT        KKCDAT
     C     *LIKE         DEFINE    XHCDAT        KKDATE
     C     *LIKE         DEFINE    EXRECD        KKRMCD
     C*
     C*
     C                   EXCEPT    HEADER
     C*
     C                   Z-ADD     0             KKCDAT
     C*
     C*  MAIN LOOP
     C*    Read canx history for req#=0, find appropriate number and
     C*    update.  If canx is reinstated then write another history rec
     C*
     C     XHKY          SETLL     PXH01
     C                   READ      PXH01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     XHCO#         KKCO#
     C                   Z-ADD     XHFND         KKFND
     C                   Z-ADD     XHFYR         KKFYR
     C                   Z-ADD     XHEMP#        KKEMP#
     C                   Z-ADD     XHDIV         KKDIV
     C                   Z-ADD     XHCDAT        KKCDAT
     C*
     C*   Check canx detail for req#
     C     CXKY          SETLL     PCX01
     C     CXKY2         READE     PCX01                                  61
     C     *IN61         IFEQ      *OFF
     C*
     C                   Z-ADD     CXREQ#        XHREQ#
     C*
     C                   UPDATE    PXH01
     C                   EXCEPT    DETAIL
     C*
     C*     If this is reinstated then write a canx history rec
     C     CXREIN        IFEQ      'Y'
     C                   EXSR      SRREIN
     C*
     C***  BEGIN ADD  ***  05/28/99  **********************************
     C*       Get DARTS remark and update user with CXUSER if different
     C     EXKY          CHAIN     PEX01                              70
     C     *IN70         IFEQ      *OFF
     C*
     C                   MOVE      CXCO#         ZZCO#
     C                   MOVE      CXFND         ZZFND
     C                   MOVE      CXEMP#        ZZEMP#
     C                   MOVE      CXDIV         ZZDIV
     C                   MOVE      EXRECD        KKRMCD
     C                   MOVE      CXRDAT        KKDATE
     C     CMKY          CHAIN     PCM01                              71
     C     *IN71         IFEQ      *OFF
     C     CMUSER        ANDNE     CXUSER
     C*
     C                   MOVE      CXUSER        CMUSER
     C                   MOVE      CXUSER        CMIUSR
     C*
     C                   UPDATE    PCM01
     C*
     C                   ENDIF                                                  *IN71=*OFF
     C*
     C                   ENDIF                                                  *IN70=*OFF
     C***  END   ADD  ***  05/28/99  **********************************
     C*
     C                   ENDIF                                                  CXREIN='Y'
     C*
     C                   ENDIF                                                  *IN61=*OFF
     C*
     C                   READ      PXH01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   MOVE      *ON           *INLR
     C*================================================================
     C*  SRREIN - WRITE RECORD FOR REINSTATEMENT
     C*================================================================
     C     SRREIN        BEGSR
     C*
     C                   MOVE      CXCO#         XHCO#
     C                   MOVE      CXFND         XHFND
     C                   MOVE      CXFYR         XHFYR
     C                   MOVE      CXEMP#        XHEMP#
     C                   MOVE      CXDIV         XHDIV
     C                   MOVE      CXREQ#        XHREQ#
     C                   MOVE      CXCXDT        XHCXDT
     C                   MOVE      CXCDAT        XHCDAT
     C                   MOVE      CXAGT#        XHAGT#
     C                   MOVE      CXRSN         XHRSN
     C                   MOVE      CXREDT        XHREDT
     C                   MOVE      CXRDAT        XHRDAT
     C                   MOVE      CXDT          XHTRDT
     C                   MOVE      *BLANKS       XHRVFL
     C                   MOVE      CXSTAT        XHSTAT
     C                   MOVE      CXUSER        XHUSER
     C                   MOVE      CXPGM         XHPGM
     C                   MOVE      CXTIME        XHTIME
     C                   MOVE      CXDT          XHDT
     C                   MOVE      CXCNC#        XHCNC#
     C                   MOVE      CXERR         XHERR
     C*
     C                   WRITE     PXH01
     C                   EXCEPT    DETAIL
     C*
     C                   ENDSR                                                  SRREIN
     C*================================================================
     OQSYSPRT   E            HEADER         2  4
     O                                           24 'CANCELLATION HISTORY RE'
     O                                           43 'CORD WRITTEN FOR:'
     O          E            HEADER         2
     O                                            8 'CO#'
     O                                           12 'FND'
     O                                           23 'EMPLOYER'
     O                                           30 'DIV'
     O                                           34 'FYR'
     O                                           40 'REQ#'
     O                                           50 'CX ISSUE'
     O                                           57 'PAGE:'
     O                       PAGE          Z     60
     O          E            DETAIL         1
     O                       XHCO#         Z      8
     O                       XHFND         Z     12
     O                       XHEMP#        Z     23
     O                       XHDIV         Z     30
     O                       XHFYR         Z     34
     O                       XHREQ#        Z     40
     O                       XHCDAT              50
