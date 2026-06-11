      *****************************************************************
      * (C) Copyright Progressive Data Solutions, Inc., 2005
      *****************************************************************
      *   Delete all QFLJP records that match the input year & month  *
      *   QFLJP records are used for Loss Ratio by Job Class Report.  *
      *                                                               *
      * CREATED 02/16/2005 C:15  PROJECT: WC1063                      *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      *  02/13/08  C:180   SUPPORT 62143
      *                    Per mini-specs,replace QFLJP with QFLJL,
      *                    Delete only records for period processed.
      *
      *
      *****************************************************************
      *                                                               *
      ******* Begin Add  02/13/08 ******************************************
     FSMCOL     IF   E           K DISK
     FQFLJL     UF   E           K DISK
      ******* End Add    02/13/08 ******************************************
      **** Begin Delete 02/13/08 **************************************
     F**Qfljp     UF   E             Disk
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
     C*****              Dou       %Eof(Qfljp)
     C*****              Read      Qfljp
      *****
     C*****              If        %Eof
     C*****              Leave
     C*****              Endif
      *****
     C*****              IF        ljrcym  = Cyymm
     C*****              Delete    PLJ01
     C*****              EndIf
      *****
     C*****              Enddo
      *
      *
     C     *LOVAL        SETLL     SMCOL
     C                   READ      SMCOL
      *
     C                   DOW       NOT %EOF(SMCOL)
     C                   DOU       NOT %FOUND
     C     FEUKY         DELETE    QFLJL
     C                   ENDDO
     C                   READ      SMCOL
     C                   ENDDO
      ******* End  Change 02/13/08 ****************************************
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
