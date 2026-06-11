     FWPOSTD    IP   E             DISK    RENAME(WPOSTD:WPOSTR) PREFIX(x)
     FWMEAP     IF   E             DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     D PODT            s               d   INZ
     D TRDT            s               d   INZ
     D RECCNT          S              5  0 INZ
     D RRN             S              9  0 INZ
     C                   MOVE      xSEL0001      RRN
     C     RRN           CHAIN     WMEAP
     C                   IF        %FOUND(WMEAP)
     C     *CYMD         MOVE      EAPODT        PODT
     C     *CYMD         MOVE      EATRDT        TRDT
     C                   EVAL      RECCNT = RECCNT + 1
     C   OF              EXCEPT    HDG
     C                   EXCEPT    DET
     C                   ENDIF
     CLR                 EXCEPT    TOT
     C     *INZSR        BEGSR
     C                   EXCEPT    HDG
     C                   ENDSR
     OQSYSPRT   E            HDG           02 01
     O                                              'PGM:RPT001R'
     O                                          +30 'A/R RECORDS FIXED'
     O                       UDATE         Y   +001
     O          E            DET        01
     O                       RRN           Z
     O                       EAFND             +001
     O                       EAFYR             +001
     O                       EAEMP#        Z   +001
     O                       EADIV         Z   +001
     O                       TRDT              +001
     O                       EAAMT         K   +001
     O                       PODT              +001
     O          E            TOT        02
     O                                              'REC COUNT:'
     O                       RECCNT        Z   +001
