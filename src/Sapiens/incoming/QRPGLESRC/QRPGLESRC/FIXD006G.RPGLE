      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD006G - Fix negative reserves
      *****************************************************************
      *
      * DESCRIPTION:  This pgm fixes the negative reserves
      *               in the claim master as a result of the blank reserve
      *               code.
      *
      *               Input file BADPV4 contains the records that
      *               need fixing.
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
      *
     FBADPV4    IF   E           K DISK
     FBADPV3    IF   E           K DISK
     FWMCML1    UF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMRHL     UF   E           K DISK

     D @UPD            S              1A
     D XXCO#           S              3  0 INZ(777)
     D Z               S              3  0 INZ
     D X               S              3  0 INZ
     D J               S              3  0 INZ
     D @ICC            S               N   INZ(*OFF)
     D                 DS
     D  RC                     1     20
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
     D  RS                     1     20
     D                                     DIM(10)
     D  FNRS01                 1      2
     D  FNRS02                 3      4
     D  FNRS03                 5      6
     D  FNRS04                 7      8
     D  FNRS05                 9     10
     D  FNRS06                11     12
     D  FNRS07                13     14
     D  FNRS08                15     16
     D  FNRS09                17     18
     D  FNRS10                19     20
     D                 DS
     D  RA                     1     90  2
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
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
     D                 DS
     D  RT                     1     20
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

     C     *ENTRY        PLIST
     C                   PARM                    @UPD

      *
     C     FNKY          KLIST
     C                   KFLD                    P4CO
     C                   KFLD                    P4FND

     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    P4FND
     C                   KFLD                    P4FYR
     C                   KFLD                    P4CASE

     C     RHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    P4FND
     C                   KFLD                    P4FYR
     C                   KFLD                    P4CASE

     C                   READ      BADPV4
     C                   DOW       NOT %EOF(BADPV4)

     C     CMKY          CHAIN     BADPV3
     C                   IF        %FOUND(BADPV3)

     C                   IF        @UPD = 'Y'
     C     CMKY          CHAIN     WMCML1
     C                   ELSE
     C     CMKY          CHAIN(N)  WMCML1
     C                   ENDIF

     C                   IF        %FOUND(WMCML1) and @UPD = 'Y'

     C                   EXSR      UPDRSV
     C                   EXSR      UPDRHI

     C                   ENDIF
     C                   ENDIF

     C                   READ      BADPV4
     C                   ENDDO

     C                   EVAL      *INLR = *ON
     C**********************************************************************
     C* UPDATE RESERVES
     C**********************************************************************
     C     UPDRSV        BEGSR
      *
      * Find entry in error
      *
     C                   Z-ADD     1             Z
     C     *BLANKS       LOOKUP    RC(Z)                                  62
     C                   IF        *IN62 and (RR(Z) < *ZERO or RA(Z) <> *ZERO)
      *
      * Find correct entry
      *
     C                   Z-ADD     1             X
     C     P4RECD        LOOKUP    RC(X)                                  61
     C                   IF        *IN61
      *
      * Fix reserves
      *
     C                   SELECT
     C                   WHEN      RR(Z)< *ZERO AND RR(X) >= *ZERO and
     C                             (RR(Z) + RR(X)) < *ZERO
      *                  omit do not cause a negative reserve
     C                   OTHER
     C                   ADD       RR(Z)         RR(X)
     C                   ENDSL

     C                   ADD       RA(Z)         RA(X)
     c                   MOVE      *ZEROS        RR(Z)
     c                   MOVE      *ZEROS        RA(Z)

     C                   IF        @UPD = 'Y'
     C                   UPDATE    PCM01
     C                   ENDIF

     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C**********************************************************************
     C* UPDATE RESERVE HISTORY
     C**********************************************************************
     C     UPDRHI        BEGSR
     C     RHKY          SETLL     WMRHL
     C     RHKY          READE     WMRHL

     C                   DOW       NOT %EOF(WMRHL)

     C                   IF        RHRT06 = *BLANKS and RHSU06 <> *ZEROS
     C                   Z-ADD     1             J
     C     P4RECD        LOOKUP    RT(J)                                  63

     C                   IF        *IN63
     C                   ADD       SU(6)         SU(J)
     C                   MOVE      *ZEROS        RHSU06
     C                   ELSE

     C     FNKY          CHAIN     WMFNL

     C                   IF        %FOUND(WMFNL)
     C                   Z-ADD     1             J
     C     P4RECD        LOOKUP    RS(J)                                  64

     C                   IF        *IN64 and RT(J) = *BLANKS and SU(J) = *ZEROS
     C                   MOVE      P4RECD        RT(J)
     C                   Z-ADD     SU(6)         SU(J)
     C                   MOVE      *ZEROS        RHSU06
     C                   ELSE
     C                   MOVE      P4RECD        RHRT06
     C                   ENDIF

     C                   ELSE
     C                   MOVE      P4RECD        RHRT06
     C                   ENDIF
     C                   ENDIF

     C                   UPDATE    PRH01
     C                   ENDIF

     C     RHKY          READE     WMRHL
     C                   ENDDO
     C                   ENDSR
