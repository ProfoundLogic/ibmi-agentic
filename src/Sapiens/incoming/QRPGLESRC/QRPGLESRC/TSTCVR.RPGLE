     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     F*
     FDFTCVR    O    E             PRINTER
      *
      *

     C                   MOVEL     '##TITLE#'    TITLE
     C                   MOVEL     '$$TO 1 *'    TO1
     C                   MOVEL     '$$TO 2 *'    TO2
     C                   MOVEL     '$$TO 3 *'    TO3
     C                   MOVEL     '%%FRM1 *'    FROM1
     C                   MOVEL     '%%FRM2 *'    FROM2
     C                   MOVEL     '%%FRM3 *'    FROM3
     C                   MOVEL     '&&CMT **'    CMT
     C                   MOVEL     '11/01/55'    CVPDAT
     C                   MOVEL     '12:00:05'    CVPTIM
     C                   MOVE      005           PAGES
     C*
     C                   WRITE     CVRPAG
     C                   MOVE      '1'           *INLR
