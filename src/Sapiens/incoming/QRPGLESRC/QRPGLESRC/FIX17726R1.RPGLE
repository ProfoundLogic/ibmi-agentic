     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIX17726R1 - Check that canx detail records
     H*****************************************************************
     H*                                                               *
     H*  This pgm will read each canx detail record with a canx issue *
     H*  date > entry parm.  For each record it will determine that it*
     H*  has the minimum # of canx history recs.  If it has been      *
     H*  reinstated it will look for one canx and one rein cx history *
     H*  record, otherwise it will look for one canx history rec.     *
     H*  Those that do not meet the criteria will be printed out.     *
     H*                                                               *
     H*  Created: C:87                                                *
     H*****************************************************************
     F* Cancellation detail file
     FWDCXL13   IF   E           K DISK
     F*
     F* Cancellation history file
     FWDXHL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*IN10)

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C*   Cancellation detail
     C     CXKY          KLIST
     C                   KFLD                    KKCDAT
     C*
     C*   Cancellation history
     C     XHKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKREQ#
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*  Like defines
     C     *LIKE         DEFINE    XHREQ#        KKREQ#
     C     *LIKE         DEFINE    XHCO#         KKCO#
     C     *LIKE         DEFINE    XHFND         KKFND
     C     *LIKE         DEFINE    XHFYR         KKFYR
     C     *LIKE         DEFINE    XHEMP#        KKEMP#
     C     *LIKE         DEFINE    XHDIV         KKDIV
     C     *LIKE         DEFINE    XHCDAT        KKCDAT
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCDAT            8
     C*
     C                   EXCEPT    HEADER
     C*
     C                   MOVE      QQCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         KKCDAT
     C*
     C*  MAIN LOOP
     C*    Read canx detail
     C*
     C     CXKY          SETLL     PCX01
     C                   READ      PCX01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      CXCO#         KKCO#
     C                   MOVE      CXFND         KKFND
     C                   MOVE      CXFYR         KKFYR
     C                   MOVE      CXEMP#        KKEMP#
     C                   MOVE      CXDIV         KKDIV
     C                   MOVE      CXREQ#        KKREQ#
     C*
     C                   SETOFF                                       70  80
     C*
     C     CXREIN        IFEQ      'Y'
     C*
     C*     Check for one cancellation history rec and one
     C*     reinstatement history record
     C     XHKY          CHAIN     PXH01                              61
     C     *IN61         DOWEQ     *OFF
     C*
     C     XHREDT        IFNE      0
     C                   MOVE      *ON           *IN70
     C                   ENDIF                                                  XHREDT<>0
     C*
     C     XHCXDT        IFNE      0
     C     XHREDT        ANDEQ     0
     C                   MOVE      *ON           *IN80
     C                   ENDIF                                                  XHCXDT<>0
     C*
     C     XHKY          READE     PXH01                                  61
     C*
     C                   ENDDO                                                  *IN61=*OFF
     C*
     C                   ELSE
     C*
     C*     Check for one cancellation history record
     C     XHKY          CHAIN     PXH01                              61
     C     *IN61         DOWEQ     *OFF
     C*
     C     XHCXDT        IFNE      0
     C                   SETON                                        70  80
     C                   ENDIF                                                  XHCXDT<>0
     C*
     C     XHKY          READE     PXH01                                  61
     C*
     C                   ENDDO                                                  *IN61=*OFF
     C*
     C                   ENDIF                                                  CXREIN='Y'
     C*
     C*   Print detail line if record did not meet criteria
     C     *IN70         IFEQ      *OFF
     C     *IN80         OREQ      *OFF
     C                   EXCEPT    DETAIL
     C                   ENDIF
     C*
     C                   READ      PCX01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C*
     C                   MOVE      *ON           *INLR
     C*================================================================
     OQSYSPRT   E            HEADER         2  4
     O                                           24 'CORRESPONDING RECORDS I'
     O                                           49 'N CANX HISTORY NOT FOUND'
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
     O                       CXCO#         Z      8
     O                       CXFND         Z     12
     O                       CXEMP#        Z     23
     O                       CXDIV         Z     30
     O                       CXFYR         Z     34
     O                       CXREQ#        Z     40
     O                       CXCDAT              50
