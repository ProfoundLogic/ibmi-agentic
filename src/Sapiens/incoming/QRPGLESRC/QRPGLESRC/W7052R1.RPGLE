     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W7052R1 - PRINT ENDORSEMENT, WMENP CLEANUP
     H*****************************************************************
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W7051C                                                      *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED: 01/13/98        PROGRAMMER: 57        PROJECT: CM027 *
     H*****************************************************************
     F*
     FWMENL3    UF   E           K DISK

     C*
     C*  WORK FIELD DEFINITIONS
     C*
     C     *LIKE         DEFINE    ENCO#         XXCO#
     C     *LIKE         DEFINE    ENFND         XXFND
     C     *LIKE         DEFINE    ENEMP#        XXEMP#
     C     *LIKE         DEFINE    ENDIV         XXDIV
     C     *LIKE         DEFINE    ENFYR         XXFYR
     C     *LIKE         DEFINE    ENJOB         XXJOB
     C     *LIKE         DEFINE    ENEND#        XXEND#
     C*
     C*  ENDORSEMENT REQUEST WORKFILE
     C*
     C     ENKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXJOB
     C                   KFLD                    XXEND#
     C*
     C*  ENTRY PARAMETER LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQJOB             6
     C                   PARM                    QQEND#            5
     C*
     C*  LOAD THE WORK FIELDS WITH THE INCOMING PARM VALUES
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQJOB         XXJOB
     C                   MOVE      QQEND#        XXEND#
     C*
     C*  READE AND THEN DELETE THE RECORDS FOR CO/FND/EMP#/DIV/FYR/JOB/END#
     C*
     C     ENKY          SETLL     PEN01
     C     ENKY          READE     PEN01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PEN01
     C     ENKY          READE     PEN01                                  60
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
