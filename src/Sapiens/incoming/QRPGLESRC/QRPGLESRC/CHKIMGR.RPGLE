     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***************************************************************
     H*                                                            **
     H* PROGRAM:     CHKIMGR                                       **
     H* DESCRIPTION: Check System Control file to see if the       **
     H*              Image application is installed.               **
     H*              Returns: 'I' if installed.                    **
     H* CREATED BY:  C:128  PROJ:         DATE:  07/16/02          **
     H***************************************************************
     FSSYPF     IF   E             DISK
      *

     C     *ENTRY        PLIST
     C                   PARM                    RTNCDE            1
     C*** Read the System Control File
     C                   READ      SSYPF                                  60
     C     *IN60         IFEQ      *OFF
      *
     C     SYIMIF        IFEQ      'Y'
     C                   MOVE      'I'           RTNCDE
     C                   ELSE
     C                   MOVE      *BLANKS       RTNCDE
     C                   ENDIF
      *
     C                   ENDIF                                                  ENDIF *IN60
     C*
     C*** End the program
     C*
     C                   MOVE      '1'           *INLR
