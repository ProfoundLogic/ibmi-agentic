      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD007D - Report discrepancies in outstanding reserves
      *****************************************************************
      *
      * DESCPRIPTION:  This pgm reports discrepancies between claim
      *               and employer detail outstanding reserves
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
     FBADPV3    IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWDELL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D CMOUTS          S             11  2 INZ
     D EMOUTS          S             11  2 INZ
     D DIFF            S             11  2 INZ
     D TOTCNT          S              5  0 INZ
     D TOTCNT2         S              5  0 INZ
     D TOTCLM          S             11  2 INZ
     D TOTEMP          S             11  2 INZ
     D XXDIV           S                   LIKE(CMDIV)INZ
     D XXEMP#          S                   LIKE(CMEMP#)INZ
     D XXCO            S                   LIKE(P3CO)INZ
     D XXFND           S                   LIKE(P3FND)INZ
     D XXFYR           S                   LIKE(P3FYR)INZ
     D J               S              5  0 INZ
     D X               S              5  0 INZ
     D Z               S              5  0 INZ
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     D @EMP            S             23    DIM(1000)
     D @OUT            S             11  2 DIM(1000)

     D EMPNBR          DS
     D   XACO#                        3
     D   XAFND                        3
     D   XAEMP#                       9
     D   XADIV                        5
     D   XAFYR                        3
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2

      *
     C     CMKY          KLIST
     C                   KFLD                    P3CO
     C                   KFLD                    P3FND
     C                   KFLD                    P3FYR
     C                   KFLD                    P3CASE

     C     ELKY1         KLIST
     C                   KFLD                    XXCO
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR

      *
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD

      * Process payments

     C                   READ      BADPV3
     C                   DOW       NOT %EOF(BADPV3 )
     C     CMKY          CHAIN     WMCML1

     C                   IF        %FOUND(WMCML1)
     C                   XFOOT     RR            CMOUTS
     C                   MOVE      CMCO#         XACO#
     C                   MOVE      CMFND         XAFND
     C                   MOVE      CMEMP#        XAEMP#
     C                   MOVE      CMDIV         XADIV
     C                   MOVE      CMFYR         XAFYR
     C                   Z-ADD     1             X
     C     EMPNBR        LOOKUP    @EMP(X)                                50
     C                   IF        %EQUAL
     C                   ADD       CMOUTS        @OUT(X)
     C                   ELSE
     C                   ADD       1             Z
     C                   MOVE      EMPNBR        @EMP(Z)
     C                   Z-ADD     CMOUTS        @OUT(Z)
     C                   ADD       1             TOTCNT2
     C                   ENDIF
     C                   ENDIF

     C                   READ      BADPV3
     C                   ENDDO

     C                   DO        Z             J
     C                   MOVE      @EMP(J)       EMPNBR
     C                   Z-ADD     @OUT(J)       CMOUTS
     C                   ADD       @OUT(J)       TOTCLM
     C                   EXSR      CHKOUTS
     C                   ENDDO

     C   OF              EXCEPT    HEAD
     C                   EXCEPT    TOT1
     C                   EVAL      *INLR = *ON
     C**********************************************************************
     C* CHECK OUTSTANDING RESERVES
     C**********************************************************************
     C     CHKOUTS       BEGSR
     C                   MOVE      XACO#         XXCO
     C                   MOVE      XAFND         XXFND
     C                   MOVE      XAEMP#        XXEMP#
     C                   MOVE      XADIV         XXDIV
     C                   MOVE      XAFYR         XXFYR
     C                   MOVE      *ZEROS        EMOUTS
     C                   MOVE      *ZEROS        DIFF
     C     ELKY1         CHAIN     WDELL
     C                   IF        %FOUND(WDELL)
     C                   MOVE      ELOUTS        EMOUTS
     C                   ADD       ELOUTS        TOTEMP
     C                   ENDIF

     C                   IF        EMOUTS <> CMOUTS
     C                   ADD       1             TOTCNT
     C     CMOUTS        SUB       EMOUTS        DIFF
     C                   ENDIF

     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ENDSR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 02
     O                       DSTIME              08 '  :  :  '
     O                                           51 'Discrepancies between CLA'
     O                                         +000 'IM HIST and EMP DET'
     O                                         +001 'outstanding reserves.'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           10 'WTFIXD007D'
     O                                          125 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           2
     O                                              'Employer #'
     O                                           46 'Hist Out Rsv '
     O                                           63 'Empl Out Rsv  '
     O                                           83 'Diff          '
     O          E            DET            1
     O                       XXFND
     O                                         +000 '-'
     O                       XXEMP#
     O                                         +000 '-'
     O                       XXDIV
     O                                         +000 '-'
     O                       XXFYR
     O                       CMOUTS        J     44
     O                       ELOUTS        J     60
     O                       DIFF          J     83
     O          E            TOT1           1
     O                                              'COUNT:'
     O                       TOTCNT        J   +001
     O                                         +001 'OF'
     O                       TOTCNT2       J   +001
     O                       TOTCLM        J     44
     O                       TOTEMP        J     60
