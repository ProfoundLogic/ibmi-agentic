     FFAEOP     UP   E             DISK    INFDS(EOPFDS)
     FFALTL7    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D EOPFDS          ds
     D dbrrn                 397    400b 0
     D UPDFLG          S              1
     D RRN             S              9  0
     D TIMSTP          S              6  0

     C     *ENTRY        PLIST
     C                   PARM                    UPDFLG

     C     KEYALT        KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#

     C                   MOVE      *OFF          *IN90

     C                   IF        EOCRPE = 'T'
     C     KEYALT        SETLL     FALTL7

     C                   IF        %EQUAL

     C     KEYALT        READE     FALTL7
     C                   DOW       NOT %EOF(FALTL7)
     C                   IF        LTFLG2 = 'T'
     C                   MOVE      *OFF          *IN90
     C                   LEAVE
     C                   ELSE
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C     KEYALT        READE     FALTL7
     C                   ENDDO
      *
     C                   ELSE
     C                   MOVE      *ON           *IN90
     C                   ENDIF

     C                   IF        *IN90

     C                   IF        *INOF
     C                   EXCEPT    HDG
     C                   ENDIF
     C                   MOVE      dbrrn         RRN
     C                   EXCEPT    DET

     C                   IF        UPDFLG = 'Y'
     C                   MOVE      *BLANKS       EOCRPE
     C                   UPDATE    PEO01
     C                   ELSE
     C                   MOVE      *BLANKS       EOCRPE
     C                   ENDIF

     C                   IF        *INOF
     C                   EXCEPT    HDG
     C                   ENDIF
     C                   EXCEPT    DET
     C                   IF        *INOF
     C                   EXCEPT    HDG
     C                   ENDIF
     C                   EXCEPT    SPACE

     C                   ENDIF
     C                   ENDIF

     C     *INZSR        BEGSR
     C                   TIME                    TIMSTP
     C                   IF        UPDFLG = 'Y'
     C                   MOVE      *ON           *IN77
     C                   ELSE
     C                   MOVE      *OFF          *IN77
     C                   ENDIF
     C                   EXCEPT    HDG
     C                   ENDSR
     OQSYSPRT   E            HDG            2 01
     O                                              'PGM:FIXDTA001R'
     O                       UDATE         Y   +001
     O                       TIMSTP            +001 '  :  :  '
     O                                           70 'SUPPORT # 59884 - FAEOP'
     O                                        +0001 'FIX'
     O               77                       +0001 '*** UPDATE MODE ***'
     O                                          125 'PAGE:'
     O                       PAGE          Z   +001
     O          E            HDG         1  1
     O                                              'KEY'
     O                                           60 'BATCH PROC'
     O                                          100 'RRN'
     O          E            DET         1
     O                       EOCO#
     O                       EOSUB#            +002
     O                       EOFND             +002
     O                       EOFYR             +002
     O                       EOCASE            +002
     O                       EOEOB#            +002
     O                       EOCRPE              55
     O                       RRN           Z    100
     O          E            SPACE       1
