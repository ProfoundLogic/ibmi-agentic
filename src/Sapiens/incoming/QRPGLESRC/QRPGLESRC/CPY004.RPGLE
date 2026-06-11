      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE CPY004  - Write test env files
      *****************************************************************
      *
      * DESCRIPTION:  This pgm copies affected data for testing purposes.
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
     FBADPV3    IF   E             DISK
     FWMCML1    IF   E           K DISK
     FWMRHL     IF   E           K DISK
     FWDPAL     IF   E           K DISK
     FWARPL     IF   E           K DISK
     FXWMCMP    O    E           K DISK    RENAME(PCM01:XPCM01)
     FXWMRHP    O    E           K DISK    RENAME(PRH01:XPRH01)
     FXWDPAP    O    E           K DISK    RENAME(PPA01:XPPA01)
     FXWARPP    O    E           K DISK    RENAME(PRP01:XPRP01)
     D PVFND           S                   LIKE(PAFND)
     D PVFYR           S                   LIKE(PAFYR)
     D PVCASE          S                   LIKE(PACASE)
     C     CMKY          KLIST
     C                   KFLD                    P3CO
     C                   KFLD                    P3FND
     C                   KFLD                    P3FYR
     C                   KFLD                    P3CASE

     C                   READ      BADPV3
     C                   DOW       NOT %EOF(BADPV3 )
     C                   MOVE      P3FND         PVFND
     C                   MOVE      P3FYR         PVFYR
     C                   MOVE      P3CASE        PVCASE
     C                   EXSR      WRTCLM
     C                   EXSR      WRTPAY
     C                   EXSR      WRTREQ
     C                   EXSR      WRTHIS
     C                   DOU       PVFND  <> P3FND  or PVFYR <> P3FYR  or
     C                             PVCASE <> P3CASE  or %EOF(BADPV3 )
     C                   READ      BADPV3
     C                   ENDDO
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C**********************************************************************
     C*             Write Claim Master
     C**********************************************************************
     C     WRTCLM        BEGSR
     C                   CLEAR                   XPCM01
     C     CMKY          SETLL     WMCML1
     C     CMKY          READE     WMCML1
     C                   DOW       NOT %EOF(WMCML1)
     C                   WRITE     XPCM01
     C     CMKY          READE     WMCML1
     C                   ENDDO
     C                   ENDSR
     C**********************************************************************
     C*             Write Payments
     C**********************************************************************
     C     WRTPAY        BEGSR
     C                   CLEAR                   XPPA01
     C     CMKY          SETLL     WDPAL
     C     CMKY          READE     WDPAL
     C                   DOW       NOT %EOF(WDPAL)
     C                   WRITE     XPPA01
     C     CMKY          READE     WDPAL
     C                   ENDDO
     C                   ENDSR
     C**********************************************************************
     C*             Write Reserve History
     C**********************************************************************
     C     WRTHIS        BEGSR
     C                   CLEAR                   XPRH01
     C     CMKY          SETLL     WMRHL
     C     CMKY          READE     WMRHL
     C                   DOW       NOT %EOF(WMRHL)
     C                   WRITE     XPRH01
     C     CMKY          READE     WMRHL
     C                   ENDDO
     C                   ENDSR
     C**********************************************************************
     C*             Write Payment requests
     C**********************************************************************
     C     WRTREQ        BEGSR
     C                   CLEAR                   XPRP01
     C     CMKY          SETLL     WARPL
     C     CMKY          READE     WARPL
     C                   DOW       NOT %EOF(WARPL)
     C                   WRITE     XPRP01
     C     CMKY          READE     WARPL
     C                   ENDDO
     C                   ENDSR
