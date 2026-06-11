     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     F*
      * PRINT THE BARCODE
     F*
     FPRTBARP   O    E             PRINTER OFLIND(*IN01)
     F*

     C*
     C     *ENTRY        PLIST
     C                   PARM                    BARCOD            8
     C*
     C*
     C                   WRITE     PAGEx
     C                   WRITE     PAGE1A
     C*
     C*
     C                   MOVE      '1'           *INLR
