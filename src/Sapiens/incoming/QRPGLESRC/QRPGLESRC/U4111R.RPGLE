     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR  dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*Caller)
     H/define CONVERT
     H/TITLE U4111R - COPY SINGLE REQUEST TO FILE IN QTEMP AND DELETE
     H/TITLE          REQUEST FROM FILE UXLCP1 IN THE LIVE ENVIRONMENT
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*     THIS PROGRAM IS USED BY THE PRINT DEPT LETTER JOB TO      *
     H*     COPY A SINGLE REQUEST FROM THE LIVE REQUEST FILE TO THE   *
     H*     FILE SET UP IN QTEMP BY THE PRINT JOB (U4111C).  ONCE     *
     H*     THE REQUEST HAS BEEN COPIED INTO UXLCP1 IN QTEMP IT IS    *
     H*     REMOVED FROM THE LIVE REQUEST FILE.                       *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     U4111C                                                    *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*     NONE                                                      *
     H*                                                               *
     H*  PROGRAMMER: C:62                                             *
     H*  CREATED:    12/09/98               PROJECT: MC184            *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                      GENERAL USAGE                            *
     H*   99   GENERAL READS, CHAINS                                  *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*                                                               *
     H*   UXLCP, UXLCP1 AND UXPOP (PLC01, PLC02 AND PPO01)            *
     H*      NO KEY                                - THIS FILE        *
     H*      IS THE VERSION OF UXLCP1 THAT EXISTS IN QTEMP.           *
     H*                                                               *
     H*   UXLCL, UXLCL11 AND UXPOL (PLC03, PLC04 AND PPO02)           *
     H*      CO#/SUB#/FND/FYR/CASE/CERT/EXT/REQ# - THIS FILE          *
     H*      IS THE VERSION OF UXLCP1 THAT EXISTS IN THE LIVE         *
     H*      ENVIRONMENT.                                             *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 01/23/13  C:287 Seq#: SY266_OVR                               *
     H*                 Added DFTACTGRP and ACTGRP compile options    *
     H*                                                               *
     H*****************************************************************
     FUXLCP     O    E             DISK
     FUXLCP1    O    E             DISK
     F                                     RENAME(PLC01:PLC02)
     FUXPOP     O    E             DISK
     FUXLCL     UF   E           K DISK
     F                                     RENAME(PLC01:PLC03)
     FUXLCL11   UF   E           K DISK
     F                                     RENAME(PLC01:PLC04)
     FUXPOL     UF   E           K DISK
     F                                     RENAME(PPO01:PPO02)
     FUMPCP     IF   E             DISK

     C*
     C     LCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C                   KFLD                    KKEXT
     C                   KFLD                    KKREQ#
     C*
     C     *LIKE         DEFINE    LCCO#         KKCO#
     C     *LIKE         DEFINE    LCSUB#        KKSUB#
     C     *LIKE         DEFINE    LCFND         KKFND
     C     *LIKE         DEFINE    LCFYR         KKFYR
     C     *LIKE         DEFINE    LCCASE        KKCASE
     C     *LIKE         DEFINE    LCCERT        KKCERT
     C     *LIKE         DEFINE    LCEXT         KKEXT
     C     *LIKE         DEFINE    LCREQ#        KKREQ#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN            15 5
     C                   PARM                    @@REQ#           15 5
     C*
     C                   Z-ADD     @@RRN         ##RRN             9 0
     C                   Z-ADD     @@REQ#        ##REQ#            5 0
     C*
     C     ##RRN         CHAIN     PPC01                              99
     C*
     C                   Z-ADD     PCCO#         KKCO#
     C                   Z-ADD     PCSUB#        KKSUB#
     C                   Z-ADD     PCFND         KKFND
     C                   Z-ADD     PCFYR         KKFYR
     C                   Z-ADD     PCCASE        KKCASE
     C                   MOVEL(P)  PCCERT        KKCERT
     C                   Z-ADD     PCEXT         KKEXT
     C                   Z-ADD     ##REQ#        KKREQ#
     C*
     C     LCKY          CHAIN     PLC03                              99
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C                   WRITE     PLC01
     C                   DELETE    PLC03
     C     LCKY          READE     PLC03                                  99
     C*
     C                   ENDDO
     C*
     C     LCKY          CHAIN     PLC04                              99
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C                   WRITE     PLC02
     C                   DELETE    PLC04
     C     LCKY          READE     PLC04                                  99
     C*
     C                   ENDDO
     C*
     C     LCKY          CHAIN     PPO02                              99
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C                   WRITE     PPO01
     C                   DELETE    PPO02
     C     LCKY          READE     PPO02                                  99
     C*
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
