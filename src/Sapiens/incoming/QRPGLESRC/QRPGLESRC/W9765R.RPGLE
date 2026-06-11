      * (C) Copyright Progressive Data Solutions, Inc., 2003
      *
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      *****************************************************************
      * CREATED 02/16/2003 C:15  PROJECT: WC1063                      *
      *   CREATE FILE QFLJP                                           *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 01/22/13 C:287 Seq#: SY266_OVR                                *
      *                   Added Activation group compile options      *
      *                                                               *
      *****************************************************************
      *                                                               *
     FWfljp     IF   E             Disk
     FQfljp     O  A E             Disk    Rename(Plj01:Qlj01)
     *
     D Datusa          S               D   DATFMT(*USA)
     D Datiso          S               D   DATFMT(*iso)
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
     Iplj01
     I              ljasof                      pljasof
     I              ljrndt                      pljrndt
      *
     C                   Eval      *Inlr = *Off
      *
      *  Read work file input
      *
     C                   Read      Wfljp
     C                   Dow       not%Eof(Wfljp)
      *
      *  Convert all dates to *USA.
      *
     C     *usa          move      pljasof       ljasof
     C     *usa          move      pljrndt       ljrndt
     C                   Eval      ljrcym  = Cyymm
      *
     C                   Write     Qlj01
      *
     C                   Read      Wfljp
     C                   Enddo
      *
     C                   Eval      *Inlr = *On
      *
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
