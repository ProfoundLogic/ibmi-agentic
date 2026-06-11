      *****************************************************************
      * (C) Copyright Progressive Data Solutions, Inc., 2003 - 2004
      *****************************************************************
      *   Delete all QFEUP records that match the input year & month  *
      *   QFEUP records are used for Calendar Earned/Unearned Report  *
      *                                                               *
      * CREATED 09/05/2003 C:132 PROJECT: 129WCU                      *
      *****************************************************************
      * REVISIONS:                                                    *
      *  02/13/08  C:180   SUPPORT 62143
      *                    Per mini-specs,replace QFE2P with QFE2L,
      *                    Delete only records for period processed.
      *
      *
      *                                                               *
      *****************************************************************
      *                                                               *
      ******* Begin Add  02/13/08 ******************************************
     FSMCOL     IF   E           K DISK
     FQFE2L     UF   E           K DISK
      ******* End Add    02/13/08 ******************************************
      **** Begin Delete 02/13/08 **************************************
     F***Qfe2p     UF   E             Disk
      **** End Delete   02/13/08 **************************************
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
      ******* Begin Add  02/13/08 ******************************************
     C     FEUKY         KLIST
     C                   KFLD                    COCO#
     C                   KFLD                    Cyymm
      ******* End Add    02/13/08 ******************************************
     C                   Eval      *Inlr = *Off
      /eject
      *
      *
      *  Read work file input
      *
      ******* Begin Change 02/13/08 ****************************************
     C*****              Dou       %Eof(Qfe2p)
     C*****              Read      Qfe2p
      *****
     C*****              If        %Eof
     C*****              Leave
     C*****              Endif
      *****
     C*****              IF        E2rcym  = Cyymm
     C*****              Delete    PE201
     C*****              EndIf
      *****
     C*****              Enddo
      *
     C     *LOVAL        SETLL     SMCOL
     C                   READ      SMCOL
      *
     C                   DOW       NOT %EOF(SMCOL)
     C                   DOU       NOT %FOUND
     C     FEUKY         DELETE    QFE2L
     C                   ENDDO
     C                   READ      SMCOL
     C                   ENDDO
      ******* End  Change 02/13/08 ****************************************
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
