     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWMAUP - INITIALIZE POLICY TO DATES                     ND   R
     H*****************************************************************
     H*                                                               *
     H*   PROGRAM DESCRIPTION:                                        *
     H*     THIS PROGRAM READS THE AUDIT CONTROL FILE. IT CHAINS TO   *
     H*     THE EMPLOYER DETAIL FILE TO GET THE 'POLICY EFFECTIVE TO  *
     H*     DATE'. IT THEN ASSIGNS ELPTDT TO AUPTDT AND UPDATES WMAUP *
     H*                                                               *
     H*   CALLED FROM:                                                *
     H*     COMMAND ENTRY                                             *
     H*                                                               *
     H*   CALLS TO:                                                   *
     H*     NONE                                                      *
     H*                                                               *
     H*   PROGRAMMER: C:87                                            *
     H*   CREATED:    04/06/98    PROJECT: WC630                      *
     H*                                                               *
     H*****************************************************************
     FWDELL     IF   E           K DISK
     FWMAUP     UF   E             DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*IN10)

     C*
     C     ELKY          KLIST
     C                   KFLD                    AUCO#
     C                   KFLD                    AUFND
     C                   KFLD                    AUEMP#
     C                   KFLD                    AUDIV
     C                   KFLD                    AUFYR
     C*
     C                   READ      PAU01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C     ELKY          CHAIN     PEL01                              50
     C*
     C     *IN50         IFEQ      *OFF
     C*
     C                   Z-ADD     ELPTDT        AUPTDT
     C*
     C                   UPDATE    PAU01
     C*
     C                   ELSE
     C*
     C                   EXCEPT    NOREC
     C*
     C                   ENDIF                                                  *IN50=*OFF
     C*
     C                   READ      PAU01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   MOVE      *ON           *INLR
     C*
     C****************************************************************
     C* *INZSR - INITIALIZATION SUBROUTINE
     C****************************************************************
     C     *INZSR        BEGSR
     C*
     C                   EXCEPT    HEADER
     C*
     C                   ENDSR
     C*
     C****************************************************************
     OQSYSPRT   E            HEADER         2  4
     O                                           24 'AUDIT RECORDS NOT FOUND'
     O                                           43 'IN EMPLOYER DETAIL'
     O          E            HEADER         2
     O                                            8 'CO#'
     O                                           12 'FND'
     O                                           23 'EMPLOYER'
     O                                           30 'DIV'
     O                                           34 'FYR'
     O                                           57 'PAGE:'
     O                       PAGE          Z     60
     O          E            NOREC          1
     O                       AUCO#                8
     O                       AUFND               12
     O                       AUEMP#              23
     O                       AUDIV               30
     O                       AUFYR               34
