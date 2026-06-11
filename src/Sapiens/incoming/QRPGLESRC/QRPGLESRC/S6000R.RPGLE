     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE S6000R - ASSIGN SYSTEM VAL TO FLAGS FOR SELECT PROCESSING
     H*****************************************************************
     H*  Description:                                                 *
     H*                                                               *
     H*    Access System Control file record 'SY' to obtain values for*
     H*    select processing in daily batch.                          *
     H*    SSYPF                                                      *
     H*      SYFL02 - 'Y' WCMC Installed -> @@WCMC = 'Y'              *
     H*      SYFL03 - 'Y' Check rec Installed -> @@CHK = 'Y'          *
     H*      SYBCIF - 'Y' Bar code Installed -> @@BAR = 'Y'           *
     H*    SWCPF                                                      *
     H*      WCFL06 - 'Y' Deductible Module -> @@DED = 'Y'            *
     H*      WCFL08 - ' ','A' Admin Processing -> @@ADMN = 'Y'        *
     H*      WCFL08 - ' ','C' Claim Processing -> @@CLM = 'Y'         *
     H*      WCFL09 - 'Y' EDI Module -> @@EDI = 'Y'                   *
     H*      WCFL10 - 'Y' MICR Checks -> @@MICR = 'Y'                 *
     H*                                                               *
     H*  Called From:                                                 *
     H*    W6000C - DAILY BATCH                                       *
     H*                                                               *
     H*  Calls To:                                                    *
     H*                                                               *
     H*    None                                                       *
     H*                                                               *
     H*  Created:  10/16/96  C:33  Project:  WC577                    *
     H*                                                               *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  Indicators:                                                  *
     H*                                                               *
     H*    60 - General File Access                                   *
     H*****************************************************************
     F*
     F* System Control file
     FSSYLF     IF   E           K DISK

     C*
     C     SYKY          KLIST
     C                   KFLD                    XSKEY
     C*
     C     *LIKE         DEFINE    SYKEY         XSKEY
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@WCMC            1
     C                   PARM                    @@CHK             1
     C                   PARM                    @@BAR             1
     C                   PARM                    @@DED             1
     C                   PARM                    @@ADMN            1
     C                   PARM                    @@CLM             1
     C                   PARM                    @@EDI             1
     C                   PARM                    @@MICR            1
     C*
     C                   CLEAR                   @@WCMC
     C                   CLEAR                   @@CHK
     C                   CLEAR                   @@BAR
     C                   CLEAR                   @@DED
     C                   CLEAR                   @@ADMN
     C                   CLEAR                   @@CLM
     C                   CLEAR                   @@EDI
     C                   CLEAR                   @@MICR
     C*
     C*  Access System Control file 'SY' record
     C*
     C                   MOVEL(P)  'SY'          XSKEY
     C*
     C     SYKY          CHAIN     SSY01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     SYFL02        @@WCMC
     C                   MOVEL     SYFL03        @@CHK
     C                   MOVEL     SYBCIF        @@BAR
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  Access System Control file 'WC' record
     C*
     C                   MOVEL(P)  'WC'          XSKEY
     C*
     C     SYKY          CHAIN     SWC01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     WCFL06        @@DED
     C*
     C     WCFL08        IFNE      *BLANK
     C*
     C     WCFL08        IFEQ      'A'
     C                   MOVEL     'Y'           @@ADMN
     C                   ENDIF                                                  WCFL08=A
     C*
     C     WCFL08        IFEQ      'C'
     C                   MOVEL     'Y'           @@CLM
     C                   ENDIF                                                  WCFL08=C
     C*
     C                   ELSE
     C*
     C                   MOVEL     'Y'           @@ADMN
     C                   MOVEL     'Y'           @@CLM
     C*
     C                   ENDIF                                                  WCFL08<>*BLANK
     C*
     C                   MOVEL     WCFL09        @@EDI
     C                   MOVEL     WCFL10        @@MICR
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   MOVE      *ON           *INLR
