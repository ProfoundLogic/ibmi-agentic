      *  (C) Copyright Progressive Data Solutions, Inc., 2001
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      *
      *  W8127R1 - CLARENDON - BUILD CALENDER EARNED/UNEARNED FILE
      *  FOR HISTORICAL
      *
      *****************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
      *                                                               *
      *****************************************************************
     FWxpgl     IF   E           K DISK
      *
     D QQco#           S              3
     D QQfdt           S              8
     D QQtdt           S              6
     D QQtdt8          S              8
     D QQfnd           S              8
     D QQque           S             10
     D XXco#           S              3  0
     D Datusa          S               D   DATFMT(*USA)
      *
      *  GROUP CONTROL FILE
      *
     C     Pgky          Klist
     C                   Kfld                    XXco#
      *
      *
      *   ENTRY PARAMETERS
      *
     C     *Entry        Plist
     C                   Parm                    QQco#
     C                   Parm                    QQfdt
     C                   Parm                    QQtdt
     C                   Parm                    QQque
      *
     C     *Mdy0         Move      QQtdt         Datusa
     C     *Usa0         Move      Datusa        QQtdt8
     C                   Move      QQco#         XXco#
      *
     C     Pgky          Setll     Xpg01
      *
     C                   Dou       %Eof
     C     Pgky          Reade     Xpg01
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   If        PgFlag = 'I'
      *
     C                   Movel     Pggrp         QQfnd
      *
     C                   Call      'W4902R'
     C                   Parm                    QQco#
     C                   Parm                    QQfnd
     C                   Parm      '01011980'    QQfdt
     C                   Parm                    QQtdt8
     C                   Parm                    QQtdt8
     C                   Parm                    QQque
     C                   Parm      'P'           QQdtr             1
     C                   Parm      'S'           QQsum             1
      *
     C                   Endif                                                  PGFLAG IFEQ
      *
      *
     C                   Enddo
      *
     C                   Eval      *Inlr = *on
