     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H***************************************************************
     H* Created on 08/21/2007 by c:62 for Project SY157
     H***************************************************************
     H* REVISIONS
     H*
     H***************************************************************
     FSTABL     IF   E             DISK
     F*

     D CLCALL          C                   CONST('FIXFTABLC')

     I*

     C                   CALL      CLCALL
     C                   PARM      'FTABL'       ##FILE           10
     C*
     C                   READ      PST01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     'FTABL'       CAT(P)    STKEY:0       FILE             10
     C                   CALL      CLCALL
     C                   PARM      FILE          ##FILE           10
     C*
     C                   READ      PST01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
