     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWMAUP - INITIALIZE POLICY TO DATES                     ND   R
     H*****************************************************************
     H*                                                               *
     H*   PROGRAM DESCRIPTION:                                        *
     H*     CHANGE SUBMISSIN HISTORY FILE WITH STAT OF '4' OR 'E' AND *
     H*      SHIDT OF 0980201 OR GREATER TO HAVE A SHSTAT OF '3'      *
     H*                                                               *
     H*   CALLED FROM:                                                *
     H*     COMMAND ENTRY                                             *
     H*                                                               *
     H*   CALLS TO:                                                   *
     H*     NONE                                                      *
     H*                                                               *
     H*   PROGRAMMER: C:87                                            *
     H*   CREATED:    08/24/98    PROJECT: SUPP WA091 #13352          *
     H*                                                               *
     H*****************************************************************
     FWMSHL11   UF   E           K DISK

     C*
     C*
     C*
     C                   READ      PSH01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   DELETE    PSH01
     C*
     C                   READ      PSH01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C*
     C                   MOVE      *ON           *INLR
     C*
