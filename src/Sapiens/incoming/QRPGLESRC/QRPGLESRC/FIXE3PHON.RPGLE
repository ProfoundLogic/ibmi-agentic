     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title FIXE3PHON - Fix Concurrent Employer phone number
     h dftname(FIXE3PHON)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*CALLER)
     h/define   profoundui
     h bnddir('WTBNDDIR')
     h**********************************************************************
      *===============================================================*
      *  Created   : 06/11/14                                         *
      *  Programmer: C:291                                            *
      *  Project   : RED4175                                          *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fwde3p     uf   e             disk

     D* Definitions
     D x               s              3  0
     D y               s              3  0

     D* Run-time arrays
     D ArrE3Phon       S              1    DIM(15)
     D wkPhon          S              1    DIM(15)

     D* Constants
     D NumberVal       C                   '0123456789'

      * Main program processing

     C                   Read      wde3p                                  LR
     C                   Dow       *inlr = *Off

     C                   Clear                   ArrE3Phon
     C                   Clear                   WkPhon
     C                   MoveA     E3phon        ArrE3Phon
     C                   Eval      x = 1
     C                   Eval      y = 0

      * Check each element for numeric value...
     C                   Do        15
     C     NumberVal     Check     ArrE3phon(x)
     C                   If        not%Found                                    No error fnd
      * If ArrE3Phon element is numeric, then move it to wrok array
     C                   Eval      y = y + 1
     C                   Eval      WkPhon(y) = ArrE3Phon(x)
     C                   Endif

     C                   Eval      x = x + 1
     C                   Enddo
     C                   MoveA     wkphon        E3Phon

     C                   Update    pe301

     C                   Read      wde3p                                  LR
     C                   Enddo

