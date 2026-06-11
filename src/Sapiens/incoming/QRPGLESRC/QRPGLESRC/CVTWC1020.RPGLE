     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* Description:  This pgm updates the menu file to use a new pgm *
     H*               for the WCMS Check Schedule Maint option.       *
     H*                                                               *
     H* Created by:   15     10/02/01      Project: WC1020            *
     H*****************************************************************
     F*
     FMMOPP     UF   E             DISK

     D*
     D EXCL            S             80    DIM(2) CTDATA PERRCD(1)

     C*=========================================================================
     C*
     C                   MOVE      *BLANKS       XXPGM            10
     C                   MOVE      *BLANK        XXGTYP            1
     C*====================================================
     C*  Read all records
     C*
     C                   READ      POP01                                  99
B01  C     *IN99         DOWEQ     '0'
     C*
     C     10            SUBST     OPEXCL:1      XXPGM
B02  C     XXPGM         IFEQ      'W6005R  '
     C*
     C     1             SUBST     OPEXCL:73     XXGTYP
     C     XXGTYP        IFNE      'G'
     C                   MOVEL(P)  EXCL(1)       OPEXCL
     C                   ELSE
     C                   MOVEL(P)  EXCL(2)       OPEXCL
     C                   ENDIF
     C                   UPDATE    POP01
     C*
E02  C                   ENDIF
     C*
     C                   READ      POP01                                  99
E01  C                   ENDDO
     C*
     C*  End of program
     C*
     C                   MOVE      '1'           *INLR
     C*=========================================================================
** NEW MENU COMMAND
W6005C                 (' ' ' ' ' ' x'000000000f' ' ')
W6005C                 (' ' ' ' ' ' x'000000000f' 'G')
