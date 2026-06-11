     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE W4116R - COPY SINGLE REQUEST TO FILE IN QTEMP AND DELETE REQUEST
     H/TITLE          FROM FILE WDCLP IN THE LIVE ENVIRNMENT
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*     THIS PROGRAM IS USED BY THE PRINT DEPT LETTER JOB TO      *
     H*     COPY A SINGLE REQUEST FROM THE LIVE REQUEST FILE TO THE   *
     H*     FILE SET UP IN QTEMP BY THE PRINT JOB (W4116C).  ONCE     *
     H*     THE REQUEST HAS BEEN COPIED INTO WDCLP IN QTEMP IT IS     *
     H*     REMOVED FROM THE LIVE REQUEST FILE.                       *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     W4116C                                                    *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*     NONE                                                      *
     H*                                                               *
     H*  PROGRAMMER: C:30                                             *
     H*                                                               *
     H*  CREATED:    11/28/93               PROJECT: WC375            *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                      GENERAL USAGE                            *
     H*   60   GENERAL READS, CHAINS                                  *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*   WSDLP   (PDL01): NO KEY                      - THIS FILE    *
     H*      IS THE VERSION OF WSDLP THAT EXISTS IN QTEMP.            *
     H*   WSDLL   (PDL02): CO#/FND/REQ#/DPCD/FLDR/DCID - THIS FILE    *
     H*      IS THE VERSION OF WSDLP THAT EXISTS IN THE LIVE          *
     H*      ENVIRONMENT.                                             *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/21/13  C:287 SEQ#: SY266_OVR                              *
     H*                  Add default activation group compiler options*
     H*                                                               *
     H*****************************************************************
     FWSDLP     O    E             DISK
     FWSDLL     UF   E           K DISK
     F                                     RENAME(PDL01:PDL02)
     F*

     C     DLKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXREQ#
     C                   KFLD                    XXDPCD
     C                   KFLD                    XXFLDR
     C                   KFLD                    XXDCID
     C*
     C     *LIKE         DEFINE    DLCO#         XXCO#
     C     *LIKE         DEFINE    DLFND         XXFND
     C     *LIKE         DEFINE    DLREQ#        XXREQ#
     C     *LIKE         DEFINE    DLDPCD        XXDPCD
     C     *LIKE         DEFINE    DLFLDR        XXFLDR
     C     *LIKE         DEFINE    DLDCID        XXDCID
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
     C                   MOVE      QQDPCD        XXDPCD
     C                   MOVE      QQFLDR        XXFLDR
     C                   MOVE      QQDCID        XXDCID
     C*
     C     DLKEY         CHAIN     PDL02                              60
     C     *IN60         DOWEQ     '0'
     C                   WRITE     PDL01
     C                   DELETE    PDL02
     C     DLKEY         READE     PDL02                                  60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
