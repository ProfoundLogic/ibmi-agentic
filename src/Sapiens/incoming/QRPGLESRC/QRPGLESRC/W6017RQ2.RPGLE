      *****************************************************************
      * (C) Copyright Progressive Data Solutions, Inc., 2003 - 2004
      *****************************************************************
      *   Delete all QFEUP records that match the input year & month  *
      *   QFEUP records are used for Calendar Earned/Unearned Report  *
      *                                                               *
      * CREATED 09/05/2003 C:132 PROJECT: 129WCU                      *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      *  04/01/04  C: 132   Project: WC1100                           *
      *              - Add to Base to delete in situations where EOM  *
      *                has been run twice for same monthend           *
      *                                                               *
      *****************************************************************
      *                                                               *
     FQfe2p     UF   E             Disk
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
     C                   Dou       %Eof(Qfe2p)
     C                   Read      Qfe2p
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   IF        E2rcym  = Cyymm
     C                   Delete    PE201
     C                   EndIf
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
