     FWPOSTD    IP   E             DISK    RENAME(WPOSTD:WPOSTR) PREFIX(x)
     FWMEAP     UF   E             DISK
     D RECCNT          S              5  0 INZ
     D RRN             S              9  0 INZ
     D RRNA            S              9    INZ
     D RRNB            S              9    INZ
     D TRDTY4          S              4  0 INZ
     D PODT_ISO        s               d   INZ
     D PODT2_ISO       s               d   INZ
     D TRDT_ISO        s               d   INZ
     D JAN_1980        s               d   INZ (D'1980-01-01')
     D DEC_2013        s               d   INZ (D'2013-12-31')
     D NEWPODT         s               d   INZ

     D TRDTDS          DS
     D TRDTCN                         1  0 INZ(1)
     D TRDTYY                         2  0
     D TRDTMM                         2  0
     D TRDTDD                         2  0

     C                   MOVE      xSEL0001      RRN
     C     RRN           CHAIN     WMEAP
     C                   IF        %FOUND(WMEAP) and EAPODT = xEAPODT and
     C                             EAAMT = xEAAMT and EATRDT = xEATRDT and
     C                             EAEMP# = xEAEMP#
     C                   IF        EAPODT = *ZERO
     C                   EVAL      EAPODT = 0600101
     C                   ENDIF
     C     *CYMD         MOVE      EAPODT        PODT_ISO
     C     *CYMD         MOVE      EATRDT        TRDT_ISO
     C                   IF        PODT_ISO < JAN_1980 or
     C                             PODT_ISO > DEC_2013
     c     TRDT_ISO      ADDDUR    1:*DAYS       PODT2_ISO
     C                   EXTRCT    PODT2_ISO:*M  trdtmm
     C                   EXTRCT    PODT2_ISO:*D  trdtDD
     C                   EXTRCT    PODT2_ISO:*Y  trdtY4
     C                   MOVE      TRDTY4        TRDTYY
     C                   MOVE      TRDTDS        EAPODT
     C                   MOVE      RRN           RRNA
     C                   MOVE      RRN           RRNB
     C                   CALL      'CPYF001L'
     C                   PARM                    RRNA
     C                   PARM                    RRNB
     C                   UPDATE    PEA01
     C                   EVAL      RECCNT = RECCNT + 1
     C                   ENDIF
     C                   ENDIF
