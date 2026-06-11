     H*  (C) Copyright Fiserv, Inc., 2008
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      *=========================================================================
     H/TITLE -  Update SQL statements program                                  *
      *                                                                        *
      *  DESCRIPTION: This Program will retrieve SQL statemnts from F0060T03X  *
      *  Txt File and will process the SQL statements                          *
      *                                                                        *
      *                                                                        *
      *   CALLED BY:  F0060C03                                                 *
      *                                                                        *
      *   CALLS TO :                                                           *
      *                                                                        *
      *  CREATED: 01/24/2008  C:189 PROJECT WC1621                             *
      *=========================================================================
      **************************************************************************
      * REVISIONS:
      *
      ***************************************************************************
     FQTXTSRC   UF   E             Disk    RENAME(QTXTSRC:RQTXTSRC)
      *
     D*STMT             S             50A
      *
     C     *Entry        PLIST
     C                   PARM                    @@TOGRP           3
     C                   READ      RQTXTSRC                               60
     C     *IN60         DOWEQ     *OFF
     C                   Eval      SRCDTA = %SUBST(SRCDTA:1:31)  +
     C                              @@TOGRP  + ';'
     C                   UPDATE    RQTXTSRC
     C                   READ      RQTXTSRC                               60
     C                   ENDDO
     C                   EVAL      *INLR = *ON
