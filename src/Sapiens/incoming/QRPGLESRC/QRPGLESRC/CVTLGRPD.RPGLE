     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: CVTLGRPD
     H*****************************************************************
     H*  PROGRAMMER: C:32               PROJECT: MC131                *
     H*  CREATED: 02/23/96                                            *
     H*  THIS PROGRAM LOADS DILGRP FROM DIGRP.  THIS IS FOR CLIENTS   *
     H*  WHICH DO NOT HAVE A NEW HCIA DATA FILE. IT CONVERTS THE      *
     H*  EXISTING FMDIP (GROUP) TO THE NEW FORMAT.                    *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  60    GENERAL CHAIN INDICATOR                                *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H**************************************************************************
     H*****************************************************************
     FFMDIP     UF   E           K DISK

     C*
     C*** UPDATE FMDIP
     C                   READ      PDI01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVEL(P)  DIGRP         DILGRP
     C*
     C                   UPDATE    PDI01
     C*
     C                   READ      PDI01                                  60
     C                   END                                                    END DOWEQ *IN60
     C*
     C*
     C*
     C                   MOVE      '1'           *INLR
