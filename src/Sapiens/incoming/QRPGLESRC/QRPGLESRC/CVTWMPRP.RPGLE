     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE CVTWMPRP
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION                                          *
     H*                                                               *
     H*    Load PREACT based on PRLHCD.                               *
     H*                                                               *
     H*                                                               *
     H*  CREATED:                                                     *
     H*                                                               *
     H*  08/10/98 C:87 PROJECT:WC591                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/09/00  C:87  PROJECT: WC964                                *
     H*                 PREACT should be blank if a value has not     *
     H*                 been assigned.  However, it appears that it   *
     H*                 also can be '0 ' or ' 0' or '00'.  Changed    *
     H*                 to accomodate.                                *
     H*                                                               *
     H* 11/18/99  C:87  PROJECT: WC591                                *
     H*                 Only assign value to PREACT if it does not    *
     H*                 already have a value.                         *
     H*                                                               *
     H*****************************************************************
     F*
     FWMPRP     UF   E             DISK
     F*

     C*  Process all Payroll class records
     C*
     C                   READ      PPR01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C***  BEGIN ADD  ***  11/18/99  **********************************
     C*  Only update if code does not already exist
     C     PREACT        IFEQ      ' '
     C***  BEGIN ADD  ***  03/09/00  **********************************
     C     PREACT        OREQ      '0 '
     C     PREACT        OREQ      ' 0'
     C     PREACT        OREQ      '00'
     C***  END   ADD  ***  03/09/00  **********************************
     C***  END   ADD  ***  11/18/99  **********************************
     C*
     C     PRLHCD        IFNE      'Y'
     C                   MOVE      '01'          PREACT
     C                   ELSE
     C                   MOVE      '02'          PREACT
     C                   ENDIF
     C*
     C                   UPDATE    PPR01
     C*
     C***  BEGIN ADD  ***  11/18/99  **********************************
     C                   ENDIF                                                  PREACT=' '
     C***  END   ADD  ***  11/18/99  **********************************
     C*
     C                   READ      PPR01                                  60
     C*
     C                   ENDDO
     C*
     C* END PROGRAM
     C*
     C                   SETON                                        LR
     C*
