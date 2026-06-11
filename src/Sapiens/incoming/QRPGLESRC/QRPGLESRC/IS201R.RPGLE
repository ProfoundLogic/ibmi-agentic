     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     F*
      * PRINT THE PAGE SEGMENTS AS A SINGLE DOCUMENT
     F*
     FIS201P    O    E             PRINTER OFLIND(*IN01)
     F*

     C*
     C     *ENTRY        PLIST
     C                   PARM                    #PAGES            5 0
     C*
     C                   DO        #PAGES        X                 5 0
     C*
     C                   MOVE      X             $X                5
     C                   MOVE      $X            $X1               1
     C                   MOVE      $X            $X2               2
     C                   MOVE      $X            $X3               3
     C                   MOVE      $X            $X4               4
     C                   MOVE      $X            $X5               5
     C*
     C     X             IFLT      10
     C     'P'           CAT       $X1           DOCID
     C                   ENDIF
     C*
     C*
     C     X             IFLT      100
     C     X             ANDGT     9
     C     'P'           CAT       $X2           DOCID
     C                   ENDIF
     C*
     C*
     C     X             IFLT      1000
     C     X             ANDGT     99
     C     'P'           CAT       $X3           DOCID
     C                   ENDIF
     C*
     C*
     C     X             IFLT      10000
     C     X             ANDGT     999
     C     'P'           CAT       $X4           DOCID
     C                   ENDIF
     C*
     C*
     C     X             IFLT      100000
     C     X             ANDGT     9999
     C     'P'           CAT       $X5           DOCID
     C                   ENDIF
     C*
     C*
     C*
     C*
     C                   WRITE     PAGEX
     C                   WRITE     PAGE1A
     C*
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
