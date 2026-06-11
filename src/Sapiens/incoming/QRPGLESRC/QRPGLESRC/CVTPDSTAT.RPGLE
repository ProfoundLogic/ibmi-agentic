     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTPDSTAT - CONVERT WCMS STATUS TO WCMC STATUS
     FFMPDL     UF   E           K DISK
     FWMPDL     IF   E           K DISK
     F                                     RENAME(PPD01:WPD01)
     F*

     IWPD01
     I              PDFEIN                      WPFEIN
     I              PDMOD                       WPMOD
     I              PDNAME                      WPNAME
     I              PDFSN                       WPFSN
     I              PDTITL                      WPTITL
     I              PDADR1                      WPADR1
     I              PDCTY                       WPCTY
     I              PDST                        WPST
     I              PDZIP                       WPZIP
     I              PDAREA                      WPAREA
     I              PDPHN#                      WPPHN#
     I              PDSHNM                      WPSHNM
     I              PD1099                      WP1099
     I              PDTYPE                      WPTYPE
     I              PDSPCD                      WPSPCD
     I              PDSTAT                      WPSTAT
     I              PDUSER                      WPUSER
     I              PDPGM                       WPPGM
     I              PDTIME                      WPTIME
     I              PDDT                        WPDT
     I              PDFLIC                      WPFLIC
     I              PDVFLG                      WPVFLG

     C*
     C* WCMC PROVIDER FIEL KEY LIST
     C*
     C     PDKEY         KLIST
     C                   KFLD                    WPFEIN
     C                   KFLD                    WPMOD
     C*
     C* READ THE WCMS PROV FIEL AND CHAIN TO WCMC SIDE TO UPDATE STATUS
     C*
     C                   READ      WPD01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      WPSTAT        PDSTAT
     C                   UPDATE    PPD01
     C                   END                                                    END *IN60 EQ '0'
     C*
     C                   READ      WPD01                                  50
     C                   END                                                    ENDDOW 50
     C*
     C                   MOVE      '1'           *INLR
