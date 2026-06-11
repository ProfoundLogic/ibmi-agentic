     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  FIXAMTS - FIX THE AMOUNT FIELDS               C:62 03/20/01  *
     H*                                                MCMNT          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  11/20/01 C:62 PROJECT MC095 29950                            *
     H*                -ADD SECTION TO CREATE WORKFILE                *
     H*                -USE WORKFILE                                  *
     H*                                                               *
     H*****************************************************************
     FFAESP     UF   E           K DISK

     C*
     C                   READ      PES01                                  99
     C     *IN99         DOWEQ     *OFF
     C     ESBRDT        IFLT      0
     C                   Z-ADD     *ZEROS        ESBRDT
     C                   ENDIF
     C                   UPDATE    PES01
     C                   READ      PES01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
