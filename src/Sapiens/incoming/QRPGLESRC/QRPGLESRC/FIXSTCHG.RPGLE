     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXSTCHG - MAKE SURE OWNER/OFFICER STATUS IS BLANK
     H*****************************************************************
     H*                                                               *
     H*  CREATED: 05/07/99  C:09  PROJECT CM044                       *
     H*                                                               *
     H*  THIS PROGRAM WILL CHANGE OWNER/OFFICER STATUS TO BLANK IF IT *
     H*  WAS CHANGED TO 'A', 'C' OR 'X' BY CHANGES MADE FOR MIDTERM   *
     H*  ENDORSEMENTS.  THIS STATUS IS NOT USED BUT SHOULD BE ' '.    *
     H*                                                               *
     H*****************************************************************
     H*
     FWMOOP     UF   E           K DISK

     C                   READ      POO01                                  60
     C     *IN60         DOWEQ     '0'
     C     OOSTAT        IFEQ      'A'
     C     OOSTAT        OREQ      'C'
     C     OOSTAT        OREQ      'X'
     C                   MOVE      *BLANKS       OOSTAT
     C                   UPDATE    POO01
     C                   END
     C                   READ      POO01                                  60
     C                   ENDDO                                                  DOW *IN60
     C*
     C                   MOVE      *ON           *INLR
