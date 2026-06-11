      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD006F - Fix closed claims with outstanding expense
      *                   reserves.
      *****************************************************************
      *
      * DESCRIPTION:  This pgm fixes closed claims with outstanding
      *               expense reserves by removing the outstanding
      *               reserves.
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
     FWARPL     IF   E           K DISK
     FWMCML1    UF   E           K DISK    USROPN
     FWMRHP     O    E           K DISK    USROPN
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D CMOUTS          S             11  2 INZ
     D TOTCNT          S              5  0 INZ
     D TOTCNT2         S              5  0 INZ
     D DIFF            S             11  2 INZ
     D TOTRR           S             11  2 INZ
     D TOTSU           S             11  2 INZ
     D XXCO            S                   LIKE(CMCO#)INZ(777)
     D XXFND           S                   LIKE(CMFND)
     D XXFYR           S                   LIKE(CMFYR)
     D XXCASE          S                   LIKE(CMCASE)
     D X               S              5  0 INZ
     D @1ST            S               N   INZ(*ON)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
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

     D                 DS
     D  RC                     1     20
     D                                     DIM(10)
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
      *
     D                 DS
     D  SU                     1     90  2
     D                                     DIM(10)
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1

     C     CMKY          KLIST
     C                   KFLD                    XXCO
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE

     C     RPKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE

     C     *ENTRY        PLIST
     C                   PARM                    @CO               3
     C                   PARM                    @FND              3
     C                   PARM                    @FYR              3
     C                   PARM                    @CASE             7
     C                   PARM                    QQUPD             1
     C                   PARM                    QQPRT             1
     C                   PARM                    QQEXIT            1
      *
     C                   SELECT
     C                   WHEN      QQEXIT = *ON
     C                   EVAL      *INLR = *ON

     C                   OTHER
     C                   OPEN      WMCML1
     C                   OPEN      WMRHP
     C                   MOVE      @CO           XXCO
     C                   MOVE      @FND          XXFND
     C                   MOVE      @FYR          XXFYR
     C                   MOVE      @CASE         XXCASE
     C
     C                   IF        QQUPD = 'Y'
     C                   MOVE      *ON           *IN75
     C                   ELSE
     C                   MOVE      *OFF          *IN75
     C                   ENDIF

     C                   TIME                    DSDTTM

      * Process payments

     C     CMKY          CHAIN     WMCML1

     C                   MOVE      *ZEROS        CMOUTS
     C*
     C                   IF        %FOUND(WMCML1) and CMSTAT = 'C'

     C*
     C*  Check if payment requests
     C*
     C                   MOVE      *OFF          *IN78
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C     RPSTAT        IFEQ      *BLANK
     c                   SETON                                        78
     C                   ENDIF

     C     RPKY          READE     PRP01                                  60
     C                   ENDDO

     C                   IF        *IN78 = *OFF
     C                   DO        10            X
     C                   IF        RR(X)  > *ZERO
     C                   EXSR      CLROUTS
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF

     C                   ENDIF

     C                   CLOSE     WMCML1
     C                   CLOSE     WMRHP
     C                   RETURN
     C                   ENDSL
     C**********************************************************************
     C* CLEAR OUTSTANDING RESERVES
     C**********************************************************************
     C     CLROUTS       BEGSR

     C                   MOVE      XXCO          RHCO#
     C                   MOVE      XXFND         RHFND
     C                   Z-ADD     XXFYR         RHFYR
     C                   MOVE      XXCASE        RHCASE

     C                   CLEAR                   RC
     C                   CLEAR                   SU

     C                   MOVE      RT(X)         RC(X)
     C                   Z-SUB     RR(X)         SU(X)
     C                   MOVEL(P)  'CLAIM CLOSED'RHEXPL
     C                   MOVE      *BLANKS       RHSTAT
     C                   MOVE      *BLANKS       RHCLST
     C                   MOVE      Q1USER        RHUSER
     C                   MOVE      Q1PGM         RHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RHTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RHEFDT
     C                   MOVEL     SRDTO         RHDT
     C                   MOVEL     SRDTO         RHIDT
     C                   MOVE      Q1USER        RHIUSR
     C                   MOVE      Q1PGM         RHIPGM
     C                   Z-ADD     DSTIME        RHITIM
     C     RR(X)         ADD       SU(X)         DIFF
     C                   ADD       RR(X)         TOTRR
     C                   ADD       SU(X)         TOTSU
     C                   EVAL      TOTCNT = TOTCNT + 1
     C                   IF        QQPRT ='Y'
     C                   IF        @1ST or *INOF
     C                   MOVE      *OFF          @1ST
     C                   EXCEPT    HEAD
     C                   ENDIF
     C                   EXCEPT    DET
     C                   CLEAR                   RR
     C                   ENDIF
     C                   IF        QQUPD = 'Y'
     C                   WRITE     PRH01
     C                   UPDATE    PCM01
     C                   ENDIF
     C                   ENDSR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 02
     O                       DSTIME              08 '  :  :  '
     O                                           75 'Closed claims with'
     O                                         +001 'OUTSTANDING expense'
     O                                         +001 'reserves'
     O               75                        +002 '- UPDATE -'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           10 'WTFIXD006F'
     O                                          125 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           2
     O                                              'Claim    #'
     O                                           46 'Hist Out Rsv '
     O                                           46 'Clr  Out Rsv '
     O                                           83 'Diff          '
     O          E            DET            1
     O                       XXCO
     O                                         +000 '-'
     O                       XXFND
     O                                         +000 '-'
     O                       XXFYR
     O                                         +000 '-'
     O                       XXCASE
     O                       RR(X)         JB    44
     O                       SU(X)         J     60
     O                       DIFF          J     83
