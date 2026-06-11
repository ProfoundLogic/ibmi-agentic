     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: CVTLGRPL
     H*****************************************************************
     H*  PROGRAMMER: C:32               PROJECT: MC131                *
     H*  CREATED: 02/23/96                                            *
     H*  THIS PROGRAM LSLGRP FROM LSGRP.  THIS IS FOR CLIENTS WHICH   *
     H*  DO NOT HAVE A NEW LOS DATA FILE.  IT CONVERTS THE EXISTING   *
     H*  LOS (GROUP) TO THE NEW FORMAT.                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  60    GENERAL CHAIN INDICATOR                                *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H**************************************************************************
     H*****************************************************************
     FUMLSP     UF   E           K DISK

     C*
     C*
     C*** UPDATE UMLSP
     C                   READ      PLS01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVEL(P)  LSGRP         LSLGRP
     C*
     C                   UPDATE    PLS01
     C*
     C                   READ      PLS01                                  60
     C                   END                                                    END DOWEQ *IN60
     C*
     C                   MOVE      '1'           *INLR
