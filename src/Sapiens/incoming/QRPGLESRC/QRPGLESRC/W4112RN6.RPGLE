     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *
     H/TITLE  W4112RN6   -  PRINT PROGRAM FOR NC FORM 63
      *****************************************************************
      * CALLED FROM: W4112CN6   - OVERRIDE PGM
      *****************************************************************
      * REVISIONS:
      *
      * 08/19/09  C:180 FIX DISABILITY DATE
      *
      * 12/19/08  C:61  PROJECT 81WCN
      *                 Created.
      *
      *****************************************************************
     FWMEML     IF   E           K DISK
     FWMRPL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWDCLL     IF   E           K DISK
     FWDC2L     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FW4112PN6  O    E             PRINTER OFLIND(*IN01)

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXLDT1                 1      8  0
     D  XXL1DT                 1      2  0
     D  XXL2DT                 3      4  0
     D  XXL3DT                 5      8  0
     D                 DS
     D  XXSS#                  1      9
     D  XXSS1                  1      3
     D  XXSS2                  4      6
     D  XXSS3                  7      9
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *================================================================
      * KLIST
      *================================================================
      *
      *  CARRIER POLICY
      *

     C     RPKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
      *
      *  CLAIMANT DEMOGRAPHICS
      *
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
      *
      *  GROUP AUXILIARY
      *
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  CLAIM MASTER FILE
      *
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      *
      *   COMPANY FILE
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      *================================================================
      * PLIST
      *================================================================
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDY6        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *================================================================
      * *LIKE
      *================================================================
      *
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
      *
      *================================================================
      * BEGIN
      *================================================================
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
     C                   TIME                    DSDTTM
      *
     C                   READ      PCL01                                  60
     C                   READ      PC201                                  60
      *
     C                   MOVE      CLCO#         XXCO#
     C                   MOVE      CLFND         XXFND
     C                   MOVE      CLFYR         XXFYR
     C                   MOVE      CLFCLM        XXCASE
      *
     C     COKY          CHAIN     PCO01                              60
     C     CMKY          CHAIN     PCM01                              60
     C  N60RPKY          CHAIN     PRP01                              60
     C  N60              MOVEL(P)  RPCAP#        PRCAP#
      *
     C                   MOVE      DSDATE        XXRPDT            8 0
      *
     C                   MOVEL(P)  CMACL#        PRACL#
      *
      *  CARRIER FILE #
      *
     C                   MOVE      CMFND         XXT31             3
     C                   MOVE      CMFYR         XXT32             3
     C                   MOVE      CMCASE        XXT71             7
     C     XXT31         CAT       '-':0         XXCAR#           15
     C                   CAT       XXT32:0       XXCAR#           15
     C                   CAT       '-':0         XXCAR#           15
     C                   CAT       XXT71:0       XXCAR#           15
      *
      *  GET THE EMPLOYER INFORMATION
      *
     C     EMKY          CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      '0'
     C     EMPAD1        ANDEQ     *BLANKS
     C                   MOVE      EMMAD1        EMPAD1
     C                   MOVE      EMMAD2        EMPAD2
     C                   MOVE      EMMCTY        EMPCTY
     C                   MOVE      EMMST         EMPST
     C                   Z-ADD     EMMZIP        EMPZIP
     C                   ENDIF
      *
     C                   MOVE      EMSEMP        PRSEMP
     C     F2KY          CHAIN     PF201                              60
     C  N60              MOVE      F2CRCD        PRCRCD
      *
     C                   Z-ADD     EMFEIN        PRFEIN
     C                   MOVEL(P)  CLCLNM        PRCLNM
     C                   MOVEL(P)  CLLAD1        PRLAD1
     C                   MOVEL(P)  CLLAD2        PRLAD2
      *
     C     PRLAD2        IFEQ      *BLANK
     C                   MOVEL(P)  PRLAD1        PRLAD2
     C                   MOVE      *BLANKS       PRLAD1
     C                   ENDIF
      *
     C                   MOVEL(P)  CLLCSZ        PRLCSZ
      *
     C                   Z-ADD     CLAREA        PRCARA
     C                   MOVEL     CLPHN#        PRCPH1
     C                   MOVE      CLPHN#        PRCPH2
      *
     C                   Z-ADD     CLWARA        PRWARA
     C                   MOVEL     CLWPH#        PRWPH1
     C                   MOVE      CLWPH#        PRWPH2
      * SSN
     C                   MOVEL     CLSS#         PRCSS1
     C                   MOVEL     CLSS#         XXTMP1            5
     C                   MOVE      XXTMP1        PRCSS2
     C                   MOVE      CLSS#         PRCSS3
     C     CDKY          CHAIN     PCD01                              60
      *
     C     CDSEX         IFEQ      'M'
     C                   MOVE      'X'           PRMSEX            1
     C                   ENDIF
      *
     C     CDSEX         IFEQ      'F'
     C                   MOVE      'X'           PRFSEX            1
     C                   ENDIF
      * DATE OF BIRTH
     C                   MOVEL     CLDTOB        PRDOBD
     C                   MOVEL     CLDTOB        XXTMP2            4
     C                   MOVE      XXTMP2        PRDOBM
     C                   MOVE      CLDTOB        PRDOBY
      * EMPLOYER
     C                   MOVEL(P)  CLENME        PRENME
      *
     C                   Z-ADD     CLEARE        PREARA
     C                   MOVEL     CLEPH#        PREPH1
     C                   MOVE      CLEPH#        PREPH2
      *
     C                   MOVEL(P)  CLEMA1        PREMA1
     C                   MOVEL(P)  CLEMA2        PREMA2
      *
     C     PREMA2        IFEQ      *BLANK
     C                   MOVEL(P)  PREMA1        PREMA2
     C                   MOVE      *BLANKS       PREMA1
     C                   ENDIF
      *
     C                   MOVEL(P)  CLEMCS        PREMCS
      *
     C                   MOVEL(P)  CLCNME        PRCNME
     C                   MOVEL(P)  CLCAD1        PRCAD1
     C                   MOVEL(P)  CLCAD2        PRCAD2
      *
     C     PRCAD2        IFEQ      *BLANK
     C                   MOVEL(P)  PRCAD1        PRCAD2
     C                   MOVE      *BLANKS       PRCAD1
     C                   ENDIF
      *
     C                   MOVEL(P)  CLCCSZ        PRCCSZ
      *
      * CARRIER PH#
      *
     C                   Z-ADD     COAREA        PRAARA
     C                   MOVEL     COPHN#        PRAPH1
     C                   MOVE      COPHN#        PRAPH2
      *
      * CARRIER FAX
      *
     C                   Z-ADD     COFARA        PRFARA
     C                   MOVEL     COFAX#        PRFPH1
     C                   MOVE      COFAX#        PRFPH2
      *
      * INJURY ON
      *
     C     CL60A5        IFNE      *BLANKS
     C     CL60A6        ORNE      *BLANKS
     C                   MOVE      'X'           PR1A1
     C                   MOVEL     CLACDT        PRAD1M
     C                   MOVEL     CLACDT        XXTMP2            4
     C                   MOVE      XXTMP2        PRAD1D
     C                   MOVE      CLACDT        PRAD1Y
      *
     C                   MOVEL(P)  CL60A5        PR60A5
     C                   MOVEL(P)  CL60A6        PR60A6
     C                   ENDIF
      *
      * OCCUPATIONAL DISEASE
      *
     C     CL60A3        IFNE      *BLANKS
     C     CL60A4        ORNE      *BLANKS
     C                   MOVE      'X'           PR1A2
     C                   MOVEL     CLACDT        PRAD2M
     C                   MOVEL     CLACDT        XXTMP2            4
     C                   MOVE      XXTMP2        PRAD2D
     C                   MOVE      CLACDT        PRAD2Y
      *
     C                   MOVEL(P)  CL60A3        PR60A3
     C                   MOVEL(P)  CL60A4        PR60A4
     C                   ENDIF
      *
      * DEATH ON DATE
      *
     C     CLDOD         IFNE      *ZERO
     C                   MOVE      'X'           PR1A3
     C                   MOVEL     CLDOD         PRAD3M
     C                   MOVEL     CLDOD         XXTMP2            4
     C                   MOVE      XXTMP2        PRAD3D
     C                   MOVE      CLDOD         PRAD3Y
     C                   ENDIF
      *
      * SECTION 1
      *
     C     CL1A4         IFNE      *BLANK
     C                   MOVE      'X'           PR1A4
     C                   MOVEL     CLDT1         PRAD4M
     C                   MOVEL     CLDT1         XXTMP2            4
     C                   MOVE      XXTMP2        PRAD4D
     C                   MOVE      CLDT1         PRAD4Y
      *
      ****** BEGIN CHANGE ****** 08/19/09 ****************************
     C****                 MOVELCLACDT    PRAD5M
     C****                 MOVELCLACDT    XXTMP2  4
     C****                 MOVE XXTMP2    PRAD5D
     C****                 MOVE CLACDT    PRAD5Y
      *
     C                   MOVEL     CLDBDT        PRAD5M
     C                   MOVEL     CLDBDT        XXTMP2            4
     C                   MOVE      XXTMP2        PRAD5D
     C                   MOVE      CLDBDT        PRAD5Y
      ****** END CHANGE   ****** 08/19/09 ****************************
      *
     C                   MOVEL     CLDT2         PRAD6M
     C                   MOVEL     CLDT2         XXTMP2            4
     C                   MOVE      XXTMP2        PRAD6D
     C                   MOVE      CLDT2         PRAD6Y
      *
     C                   Z-ADD     CLCRT         PRCRT
     C                   Z-ADD     CLAWW         PRAWW
     C                   ENDIF
      *
      * SECTION 2
      *
     C     CL1A5         IFNE      *BLANK
     C                   MOVE      'X'           PR1A5
     C                   MOVEL     CLDT3         PRAD7M
     C                   MOVEL     CLDT3         XXTMP2            4
     C                   MOVE      XXTMP2        PRAD7D
     C                   MOVE      CLDT3         PRAD7Y
     C                   ENDIF
      *
      * SIGNATURE
      *
     C                   MOVEL(P)  CLCXNM        PRCXNM
     C                   MOVEL(P)  CLTITL        PRTITL
      *
     C                   MOVEL     DSDATE        PRAD8M
     C                   MOVEL     DSDATE        XXTMP2            4
     C                   MOVE      XXTMP2        PRAD8D
     C                   MOVE      DSDATE        PRAD8Y
      *
     C                   WRITE     FRM63R1
     C                   WRITE     FRM63R2
      *
      *  END OF PROGRAM
      *
     C                   MOVE      '1'           *INLR
      *
      *
