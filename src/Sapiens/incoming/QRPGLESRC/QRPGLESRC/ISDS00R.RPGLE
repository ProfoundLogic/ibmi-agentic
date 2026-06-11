     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***************************************************************
     H*                                                            **
     H* PROGRAM:     ISDS00R                                       **
     H* DESCRIPTION: Driver program for Direct Scan System.        **
     H*              Program gets the Key Type record for the      **
     H*              RRN passed in. It passes out the the ID's     **
     H*              for the Index, and Prompt programs. Also      **
     H*              the Key Type.                                 **
     H*              QQRTCD '1' = No Index Pgm.                    **
     H*                     '2' = No Prompt Pgm.                   **
     H*                     ' ' = All Ok                           **
     H*                                                            **
     H* CREATED BY:  C:128  PROJ:115AAA   DATE:  03/06/01          **
     H***************************************************************
     FSMKTP     IF   E             DISK
      *

     C     *ENTRY        PLIST
     C                   PARM                    QQRRN             9 0
     C                   PARM                    QQIPGM           10
     C                   PARM                    QQPPGM           10
     C                   PARM                    QQRTCD            1
      *
     C*** Chain to the Key Type file.
     C     QQRRN         CHAIN     PKT01                              60
B01  C     *IN60         IFEQ      *OFF
      *
     C*** Check for required parms
B02  C     KTINPG        IFEQ      *BLANK
     C                   MOVE      '1'           QQRTCD
     C                   EXSR      ENDPGM
E02  C                   ENDIF
      *
B02  C     KTSRC9        IFEQ      *BLANK
     C                   MOVE      '2'           QQRTCD
     C                   EXSR      ENDPGM
E02  C                   ENDIF
      *
     C                   MOVEL     KTINPG        QQIPGM
     C                   MOVEL     KTSRC9        QQPPGM
     C                   MOVE      *BLANK        QQRTCD
      *
     C                   ENDIF
      *
     C                   EXSR      ENDPGM
      *================================================================
      *** End the program
      *================================================================
     C     ENDPGM        BEGSR
     C                   MOVE      '1'           *INLR
     C                   RETURN
     C                   ENDSR
