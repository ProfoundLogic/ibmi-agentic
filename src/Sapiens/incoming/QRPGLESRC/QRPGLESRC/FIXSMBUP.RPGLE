    H*****************************************************************
    H*  REVISIONS:                                                   *
    H*****************************************************************
     FSmbup     uf a e             disk
    C*
     C                   Dou       %Eof
     C                   Read      Smbup
     C                   If        %Eof
     C                   Leave
     C                   Endif
     C                   Eval      Bucrpt = 'N'
     C                   Eval      Bumrpt = 'N'
     C                   Movel(P)  'QPGMR'       Bucque
     C                   Movel(P)  'QPGMR'       Bumque
     C                   Update    Pbu01
     C                   Enddo
     C                   Eval      *Inlr = *On
      /EJECT
