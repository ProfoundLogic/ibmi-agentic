     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR  dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE W4111R - COPY SINGLE REQUEST TO FILE IN QTEMP AND DELETE
     H/TITLE          REQUEST FROM FILE WDCLP IN THE LIVE ENVIRONMENT
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*     THIS PROGRAM IS USED BY THE PRINT DEPT LETTER JOB TO      *
     H*     COPY A SINGLE REQUEST FROM THE LIVE REQUEST FILE TO THE   *
     H*     FILE SET UP IN QTEMP BY THE PRINT JOB (W4111C).  ONCE     *
     H*     THE REQUEST HAS BEEN COPIED INTO WDCLP IN QTEMP IT IS     *
     H*     REMOVED FROM THE LIVE REQUEST FILE.                       *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     W4111C                                                    *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*     NONE                                                      *
     H*                                                               *
     H*  PROGRAMMER: C:30                                             *
     H*  CREATED:    11/28/94               PROJECT: WC376            *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                      GENERAL USAGE                            *
     H*   60   GENERAL READS, CHAINS                                  *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*   WDCLP   (PCL01): NO KEY                      - THIS FILE    *
     H*      IS THE VERSION OF WDCLP THAT EXISTS IN QTEMP.            *
     H*   WDCLL   (PCL02): CO#/FND/REQ#/DPCD/FLDR/DCID - THIS FILE    *
     H*      IS THE VERSION OF WDCLP THAT EXISTS IN THE LIVE          *
     H*      ENVIRONMENT.                                             *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/21/13  C:287 SEQ#: SY266_OVR                              *
     H*                  Add default activation group compiler options*
     H*                                                               *
     H*****************************************************************
     FWDCLP     O    E             DISK
     FWDC2P     O    E             DISK
     FWDCLL     UF   E           K DISK
     F                                     RENAME(PCL01:PCL02)
     FWDC2L     UF   E           K DISK
     F                                     RENAME(PC201:PC202)
      *

     C     CLKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXREQ#
     C                   KFLD                    XXDPCD
     C                   KFLD                    XXFLDR
     C                   KFLD                    XXDCID
     C*
     C     *LIKE         DEFINE    CLCO#         XXCO#
     C     *LIKE         DEFINE    CLFND         XXFND
     C     *LIKE         DEFINE    CLREQ#        XXREQ#
     C     *LIKE         DEFINE    CLDPCD        XXDPCD
     C     *LIKE         DEFINE    CLFLDR        XXFLDR
     C     *LIKE         DEFINE    CLDCID        XXDCID
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQREQ#            5
     C                   PARM                    QQDPCD            5
     C                   PARM                    QQFLDR           80
     C                   PARM                    QQDCID           12
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQREQ#        XXREQ#
     C                   MOVEL     QQDPCD        XXDPCD
     C                   MOVEL     QQFLDR        XXFLDR
     C                   MOVEL     QQDCID        XXDCID
     C*
     C     CLKEY         CHAIN     PCL02                              60
     C     *IN60         DOWEQ     '0'
     C                   WRITE     PCL01
     C                   DELETE    PCL02
     C     CLKEY         READE     PCL02                                  60
     C                   ENDDO
     C*
     C     CLKEY         CHAIN     PC202                              60
     C     *IN60         DOWEQ     '0'
     C                   WRITE     PC201
     C                   DELETE    PC202
     C     CLKEY         READE     PC202                                  60
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
