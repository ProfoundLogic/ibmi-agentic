     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE CVTWATNPB
     H*****************************************************************
     H*  DESCRIPTION: CONVERT TNPTYR FROM 2 TO 4 DIGITS               *
     H*                                                               *
     H*  CREATED: 12/01/98   C:33  PROJECT: WC871                     *
     H*****************************************************************
     FWATNPB    UF   E             DISK

     C*_______________________________________________________________
     C*
     C     *LIKE         DEFINE    TNPTYR        X4PTYR
     C*_______________________________________________________________
     C*
     C                   READ      PTN02                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C*  PAYMENT YEAR
     C*
     C                   MOVE      TNPTYR        X2PTYR            2
     C*
     C     X2PTYR        IFEQ      '  '
     C*
     C                   MOVEL     TNPTYR        X2PTYR
     C     '19'          CAT(P)    X2PTYR:0      X4PTYR
     C                   MOVEL     X4PTYR        TNPTYR
     C*
     C                   ENDIF                                                  X2PTYR='  '
     C*
     C*  ZIP CODE
     C*
     C                   MOVE(P)   TNPZIP        ZIP2              4
     C*
     C     ZIP2          IFEQ      '0000'
     C                   MOVE      '    '        TNPZIP
     C                   ENDIF                                                  ZIP2='0000'
     C*
     C                   UPDATE    PTN02
     C*
     C                   READ      PTN02                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*_______________________________________________________________
     C*
     C*  END PROGRAM
     C*
     C                   SETON                                        LR
     C*
