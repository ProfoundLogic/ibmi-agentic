     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE W7413R4 - STORE/FIND SPOOL FILE LOCATION FOR AGENT COPY.
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM WILL WRITE/RETRIEVE A RECORD TO THE        **
     H**     SPLCPYA FILE FOR THE LOCATION OF THE EMPLOYER COPY      **
     H**     SPOOL FILE.  THIS LOCATION WILL BE USED TO DO A CPYSPLF **
     H**     FOR THE AGENT COPY.                                     **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     W7413C4 - POLICY PAGE PRINT CL                          **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**                                                             **
     H**   PROGRAMMER: C:65                                          **
     H**   CREATED:    02/17/04     PROJECT: SY059                   **
     H**                                                             **
     H*****************************************************************
     H** REVISIONS:                                                  **
     H**                                                             **
     H*****************************************************************
     FSPLCPYAL  IF A E           K DISK

     C*
     C     SPKY          KLIST
     C                   KFLD                    XXDOC            12
     C                   KFLD                    XXFOLD           40
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDOC            12
     C                   PARM                    QQFOLD           80
     C                   PARM                    QQJOBN           10
     C                   PARM                    QQJOBU           10
     C                   PARM                    QQJOB#            6
     C                   PARM                    QQSPL#            4 0
     C*
     C     QQSPL#        IFEQ      *ZERO
     C                   MOVEL(P)  QQDOC         XXDOC
     C                   MOVEL(P)  QQFOLD        XXFOLD
     C     SPKY          CHAIN     SPL0A                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  SPJOBN        QQJOBN
     C                   MOVEL(P)  SPJOBU        QQJOBU
     C                   MOVEL(P)  SPJOB#        QQJOB#
     C                   Z-ADD     SPSPL#        QQSPL#
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  QQDOC         SPDOC
     C                   MOVEL(P)  QQFOLD        SPFOLD
     C                   MOVEL(P)  QQJOBN        SPJOBN
     C                   MOVEL(P)  QQJOBU        SPJOBU
     C                   MOVEL(P)  QQJOB#        SPJOB#
     C                   Z-ADD     QQSPL#        SPSPL#
     C                   WRITE     SPL0A
     C                   ENDIF                                                  QQSPL# IFEQ
     C*
     C                   MOVE      '1'           *INLR
