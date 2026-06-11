      * (C) Copyright Progressive Data Solutions, Inc., 2003
      *
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      *****************************************************************
      * CREATED 02/07/2003 C:142 PROJECT: WC1100                      *
      *   CREATE FILE QFEUP                                           *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 01/22/13 C:287 Seq#: SY266_OVR                                *
      *                   Added Activation group compile options      *
      *                                                               *
      * 12/06/04 C: 09    PROJECT: 93WEH/Seq # 44496                  *
      *                   Compile ONLY to add new fields in QFEUP.    *
      *                                                               *
      * 08/03/03 C: 132   PROJECT: WC970                              *
      *                   Compile ONLY to add new fields in QFEUP.    *
      *                                                               *
      *****************************************************************
      *                                                               *
     FWfeup     IF   E             Disk
     FQfeup     O  a E             Disk    Rename(Peu01:Qeu01)
     *
     D Datusa          S               D   DATFMT(*USA)
      *
     D                 DS
     D  Dsdttm                 1     12  0
     D  Dstime                 1      6  0
     D  Dsdate                 7     12  0
      *
     D                 DS
     D  Cyymm                  1      5  0
     D  C                      1      1  0
     D  YY                     2      3  0
     D  MM                     4      5  0
      *
     D PSDS          ESDS                  Extname(Szq1p)
      *
     C                   Eval      *Inlr = *Off
      /eject
      *
      *
      *  Read work file input
      *
     C                   Dou       %Eof(Wfeup)
     C                   Read      Wfeup
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   Eval      Eurcym  = Cyymm
     C                   Write     Qeu01
      *
     C                   Enddo
      *
     C                   Eval      *Inlr = *On
      *
      /eject
      *****************************************************************
      *  INITIALIZATION                ** SUBROUTINE **   S R 1 S T   *
      *****************************************************************
      *
     C     *Inzsr        Begsr
      *
     C     *Entry        Plist
     C                   Parm                    QQdate            8
      *
     C                   Movel     QQdate        MM
     C                   Move      QQdate        YY
     C                   Move      QQdate        H4                4 0
     C                   If        H4 > 1999
     C                   Move      1             C
     C                   Else
     C                   Move      0             C
     C                   Endif
      *
     C                   Endsr
