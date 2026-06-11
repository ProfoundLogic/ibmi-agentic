      *
      * FIND POLICIES WITH ONE YEAR TERMS THAT RENEWED WITH TERM SHORTER
      * THAN A YEAR
      *
     FWMEMP     IP   E             DISK
     FWDELL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     D CANDAT          DS
     D  EMCAD1
     D  EMCAD2
     D  EMCAD3
     D  EMCAD4
     D  EMCAD5
     D FRTERM          S               D   INZ
     D TOTERM          S               D   INZ
     D XXFTERM         S               D   INZ
     D XXTTERM         S               D   INZ
     D xxdays          S              5  0 INZ
     D cnt1            S              5  0 INZ
     D cnt2            S              5  0 INZ
     D DAYS            S              5  0 INZ
     D POLYR           S              3  0 INZ

     C     KYDET         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV

     c                   eval      cnt1   = *zeros
     c                   eval      cnt2   = *zeros
     C                   IF        CANDAT = *ZEROS
     C     KYDET         SETLL     WDELL
     C     KYDET         READE     WDELL
     C                   DOW       NOT %EOF(WDELL)
     C     *CYMD         MOVE      ELPFDT        xxFTERM
     C     *CYMD         MOVE      ELPTDT        xxTTERM
     C     xxTTERM       SUBDUR    xxFTERM       xxdays:*D
     C                   SELECT
     C                   WHEN      xxdays < 365 AND xxdays > 0 and cnt2 > 0
     c                   eval      cnt1   = cnt1   + 1
     C                   IF        cnt1   = 1
     C                   MOVE      ELFYR         POLYR
     C                   MOVE      xxdays        DAYS
     C                   MOVE      xxFTERM       FRTERM
     C                   MOVE      xxTTERM       TOTERM
     C                   ENDIF
     C                   WHEN      xxdays >=365
     c                   eval      cnt2   = cnt2   + 1
     C                   ENDSL
     C     KYDET         READE     WDELL
     C                   ENDDO
     C                   ENDIF

     c                   if        cnt1 > *zeros AND cnt2 > *zeros
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HDG
     C                   SETOFF                                       OF
     C                   ENDIF
     C                   EXCEPT    DET
     C                   ENDIF
     C     *INZSR        BEGSR
     C                   MOVE      *ON           *INOF
     C                   ENDSR
     OQSYSPRT   E            HDG            2  2
     O                                              'POLICIES WITH TERMS'
     O                                        +0001 'SHORTER THAN 1 YR'
     O          E            HDG         1
     O                                            5 'FUND'
     O                                           20 'POLICY NUMBER'
     O                                           28 'YR'
     O                                           34 'TERM'
     O                                           58 'DAYS'
     O          E            DET         1
     O                       EMCO#                5
     O                       EMFND             +001
     O                       EMEMP#        Z   +001
     O                       EMDIV             +001
     O                       POLYR          B  +001
     O                       FRTERM            +001
     O                       TOTERM            +001
     O                       DAYS          ZB  +001
