     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      *
      *****************************************************************
      * CREATED 02/07/2003 C:142 PROJECT: WC1100                      *
      *   CREATE FILE QFE2P                                           *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 01/22/13 C:287 Seq#: SY266_OVR                                *
      *                   Added Activation group compile options      *
      *                                                               *
      * 12/01/10 C: 235   PROJECT: 133UKG                             *
      *                   Compile ONLY to add new deductible fields   *
      *                   in WFE2P, E2ACD2                            *
      *                                                               *
      * 01/29/07 C: 132   PROJECT: 135WIK                             *
      *                   Compile ONLY to add new deductible fields   *
      *                   in WFE2P, QFE2P                             *
      *                                                               *
      * 12/29/05 C: 168   PROJECT: 129UIB                             *
      *                   Compile ONLY to add new fields in WFE2P,QFE2P
      *                                                               *
      * 08/03/03 C: 132   PROJECT: WC970                              *
      *                   Compile ONLY to add new fields in QFEUP.    *
      *                                                               *
      *****************************************************************
      *                                                               *
     FWfe2p     IF   E             Disk
     FQfe2p     O  A E             Disk    Rename(Pe201:Qe201)
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
     C                   Dou       %Eof(Wfe2p)
     C                   Read      Wfe2p
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   Eval      E2rcym  = Cyymm
     C                   Write     Qe201
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
