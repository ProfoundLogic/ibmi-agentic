     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA025R - API to update WMUPP from SMUPP
     H*****************************************************************
     H*  Created: 10/01/97  C:69  Project:  WC613                     *
     H*****************************************************************
     H*  Notes:
     H*  WMUPL is user-controlled open and close because the program
     H*  is doing a RETRN and the file is being opened for update.
     H*****************************************************************
     FWMUPL     UF A E           K DISK    USROPN
     FSMUPL     IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@USR
     C                   PARM                    @@DLT             1
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    UPUSR         @@USR
     C*
     C                   OPEN      WMUPL
     C*
     C     @@DLT         IFEQ      *BLANKS
     C                   CLEAR                   PUP01
     C     @@USR         CHAIN     WMUPL                              99
     C     @@USR         CHAIN     SMUPL                              98
     C*
     C     *IN99         IFEQ      *OFF                                         Record exists
     C                   UPDATE    PUP01
     C                   ELSE                                                   Does not exist
     C                   WRITE     PUP01
     C                   ENDIF
     C                   ELSE                                                   @@DLT<>*BLANKS
     C     @@USR         DELETE    WMUPL                              99
     C                   ENDIF
     C*
     C                   CLOSE     WMUPL
     C*
     C                   RETURN
