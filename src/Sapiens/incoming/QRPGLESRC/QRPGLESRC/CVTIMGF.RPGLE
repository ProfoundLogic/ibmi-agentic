     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***************************************************************
     H*                                                            **
     H* PROGRAM:     CVTIMGF                                       **
     H* DESCRIPTION: If document ID ends in '.DOC' change the      **
     H*              CMIMGF to 'W' (W = PC document)               **
     H*  THIS IS FOR THE LATEST VERSION OF VDARTS... AS OF 01/24/01**
     H* CREATED BY:  C:128  PROJ:         DATE:  01/18/01          **
     H***************************************************************
     FSDCMP     UF   E             DISK
      *

     D DOC             S              1    DIM(12)
      *

     C*** Read the Darts remark file.
     C                   READ      PCM01                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C*** Clear work fields
     C                   CLEAR                   DOC
     C                   MOVE      *BLANKS       TEST
     C                   MOVE      *BLANKS       EXT
      *
     C*** Check the document ID
     C                   MOVEL     CMDCID        TEST             12
     C                   MOVEA     TEST          DOC
      *
     C                   Z-ADD     1             X                 2 0
     C     *BLANK        LOOKUP    DOC(X)                                 25
      *
     C     *IN25         IFEQ      *ON
      *
     C     X             IFGT      4
     C     X             SUB       4             Y                 2 0
     C                   MOVEA     DOC(Y)        EXT               4
     C                   ENDIF
      *
     C                   ELSE
     C                   MOVEA     DOC(9)        EXT
     C                   ENDIF
      *
     C     EXT           IFEQ      '.DOC'
     C     EXT           OREQ      '.doc'
     C                   MOVE      'W'           CMIMGF
     C                   UPDATE    PCM01
     C                   ENDIF
      *
     C                   READ      PCM01                                  60
     C                   ENDDO                                                  ENDDOW *IN60
     C*
     C*** End the program
     C*
     C                   MOVE      '1'           *INLR
