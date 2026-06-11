     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWMAUP - INITIALIZE POLICY TO DATES                     ND   R
     H*****************************************************************
     H*                                                               *
     H*   PROGRAM DESCRIPTION:                                        *
     H*     THIS PROGRAM READS THE AUDIT CONTROL FILE FOR INACTIVE    *
     H*     RECORDS. IT THEN LOOKS TO SEE IF THERE IS A CORRESPONDING *
     H*     ACTIVE RECORD. IF NOT, THE INACTIVE RECORD IS ACTIVATED.  *
     H*     (THIS PROGRAM CREATED FOR SUPPORT ITEM WA091 #12011)      *
     H*                                                               *
     H*   CALLED FROM:                                                *
     H*     COMMAND ENTRY                                             *
     H*                                                               *
     H*   CALLS TO:                                                   *
     H*     NONE                                                      *
     H*                                                               *
     H*   PROGRAMMER: C:87                                            *
     H*   CREATED:    06/11/98    PROJECT: WC630                      *
     H*                                                               *
     H*****************************************************************
     FWMAUL6    UF   E           K DISK
     FWMAUL2    IF   E           K DISK
     F                                     RENAME(PAU01:PAU02)
     FQSYSPRT   O    F  132        PRINTER OFLIND(*IN10)
     F*

     IPAU02
     I              AUCO#                       A2CO#
     I              AUFND                       A2FND
     I              AUFYR                       A2FYR
     I              AUEMP#                      A2EMP#
     I              AUDIV                       A2DIV
     I              AUACO                       A2ACO
     I              AURQDT                      A2RQDT
     I              AURCVD                      A2RCVD
     I              AUPRDT                      A2PRDT
     I              AURVFL                      A2RVFL
     I              AUMAIL                      A2MAIL
     I              AUCNTD                      A2CNTD
     I              AUATTM                      A2ATTM
     I              AUSTAF                      A2STAF
     I              AUREP                       A2REP
     I              AUCOM1                      A2COM1
     I              AUCOM2                      A2COM2
     I              AUCOM3                      A2COM3
     I              AUCOM4                      A2COM4
     I              AURSGN                      A2RSGN
     I              AUREFL                      A2REFL
     I              AUSTAT                      A2STAT
     I              AUUSER                      A2USER
     I              AUPGM                       A2PGM
     I              AUTIME                      A2TIME
     I              AUDT                        A2DT
     I              AUCXFL                      A2CXFL
     I              AUPTDT                      A2PTDT
     I              AUATDT                      A2ATDT
     I              AUCNFL                      A2CNFL

     C*
     C*  KEY LISTS
     C     AUKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C     AUKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C     *LIKE         DEFINE    AUCO#         KKCO#
     C     *LIKE         DEFINE    AUFND         KKFND
     C     *LIKE         DEFINE    AUEMP#        KKEMP#
     C     *LIKE         DEFINE    AUDIV         KKDIV
     C     *LIKE         DEFINE    AUFYR         KKFYR
     C*===============================================================
     C*  BEGIN MAIN PROCESSING
     C*===============================================================
     C     AUKY          SETLL     PAU01
     C                   READ      PAU01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C*  IF RECORD INACTIVE LOOK FOR CORRESPONDING ACTIVE REC
     C     AUSTAT        IFEQ      'I'
     C*
     C                   Z-ADD     AUCO#         KKCO#
     C                   Z-ADD     AUFND         KKFND
     C                   Z-ADD     AUEMP#        KKEMP#
     C                   Z-ADD     AUDIV         KKDIV
     C                   Z-ADD     AUFYR         KKFYR
     C*
     C     AUKY          CHAIN     PAU02                              61
     C*
     C*  IF ACTIVE RECORD NOT FOUND, CHG AUSTAT TO 'A' AND UPDATE
     C     *IN61         IFEQ      *ON
     C*
     C                   MOVE      'A'           AUSTAT
     C                   MOVE      'Y'           AUCXFL
     C*
     C                   UPDATE    PAU01
     C*
     C                   ADD       1             TOTAL
     C*
     C     *IN10         IFEQ      *ON
     C                   EXCEPT    HEADER
     C                   MOVE      *OFF          *IN10
     C                   ENDIF
     C*
     C                   EXCEPT    RECDS
     C*
     C                   ENDIF                                                  *IN61=*ON
     C*
     C                   ENDIF                                                  AUSTAT='I'
     C*
     C                   READ      PAU01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   EXCEPT    TOTL
     C*
     C                   MOVE      *ON           *INLR
     C*
     C****************************************************************
     C* *INZSR - INITIALIZATION SUBROUTINE
     C****************************************************************
     C     *INZSR        BEGSR
     C*
     C                   Z-ADD     *LOVAL        KKCO#
     C                   Z-ADD     *LOVAL        KKFND
     C                   Z-ADD     *LOVAL        KKEMP#
     C                   Z-ADD     *LOVAL        KKDIV
     C                   Z-ADD     *LOVAL        KKFYR
     C*
     C                   Z-ADD     *ZERO         TOTAL             5 0
     C*
     C                   EXCEPT    HEADER
     C*
     C                   ENDSR
     C****************************************************************
     OQSYSPRT   E            HEADER         2  4
     O                                           24 'AUDIT RECORDS CHANGED T'
     O                                           42 'O ACTIVE ........:'
     O          E            HEADER         2
     O                                            8 'CO#'
     O                                           12 'FND'
     O                                           23 'EMPLOYER'
     O                                           30 'DIV'
     O                                           34 'FYR'
     O                                           57 'PAGE:'
     O                       PAGE          Z     60
     O          E            RECDS          1
     O                       AUCO#         3      8
     O                       AUFND         3     12
     O                       AUEMP#        3     23
     O                       AUDIV         3     30
     O                       AUFYR         3     34
     O          E            TOTL           1
     O                                           10 'TOTAL:'
     O                       TOTAL         1     20
