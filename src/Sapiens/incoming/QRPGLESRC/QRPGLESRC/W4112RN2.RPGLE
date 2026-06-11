     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *
     H/TITLE  W4112RN2   -  PRINT PROGRAM FOR NC FORM 26A
      *****************************************************************
      * CALLED FROM: W4112CN2   - OVERRIDE PGM
      *****************************************************************
      * REVISIONS:
      *
      * 12/19/08  C:61  PROJECT 81WCN
      *                 Created.
      *
      *****************************************************************
     FSMUPL1    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWDCLL     IF   E           K DISK
     FWDC2L     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FW4112PN2  O    E             PRINTER OFLIND(*IN01)

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
     D DT1PAY          C                   CONST('Date of first payme-
     D                                     nt')
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *================================================================
      * KLIST
      *================================================================
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
      *
     C                   MOVE      DSDATE        XXRPDT            8 0
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
      * EMPLOYEE
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
     C                   MOVE      CLPHN#        PRCPH
      *
     C                   Z-ADD     CLWARA        PRWARA
     C                   MOVE      CLWPH#        PRWPH
      * SSN
     C                   MOVE      CLSS#         PRCSS
      *
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
     C                   MOVEL(P)  CLCNME        PRCNM1
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
     C                   MOVE      COPHN#        PRAPH
      *
      * CARRIER FAX
      *
     C                   Z-ADD     COFARA        PRFARA
     C                   MOVE      COFAX#        PRFPH
      *
      * ACCIDENT DATE
      *
     C     CLACDT        IFNE      *ZERO
     C                   MOVEL     CLACDT        XXMTH             2
     C                   MOVEL     CLACDT        XXTMP2            4
     C                   MOVE      XXTMP2        XXDAY             2
     C                   MOVE      CLACDT        XXYR              2
     C     XXMTH         CAT       '/':0         PRACDT
     C                   CAT       XXDAY:0       PRACDT
     C                   CAT       '/':0         PRACDT
     C                   CAT       XXYR:0        PRACDT
     C                   ENDIF
      *
      * #3 INJURIES
      *
     C                   MOVEL(P)  CL60A1        PRINJ1
     C                   MOVEL(P)  CL60A2        PRINJ2
      *
      * #4 - EMPLOYEE PAID FOR 7 DAY WAITING PERIOD
      *
     C     CL1A1         IFNE      *BLANK
     C                   MOVE      'X'           PR1A1
     C                   ENDIF
      *
     C     CL1A2         IFNE      *BLANK
     C                   MOVE      'X'           PR1A2
     C                   ENDIF
      *
      *  WAS SALARY CONTINUED?
      *
     C     CLFSAL        IFEQ      'Y'
     C                   MOVE      'X'           PRFSAY
     C                   ELSE
     C                   MOVE      'X'           PRFSAN
     C                   ENDIF
      *
      *  WAS EMPLOYEE PAID FOR THE DATE OF INJURY?
      *
     C     CLPOI         IFEQ      'Y'
     C                   MOVE      'X'           PRPOIY
     C                   ELSE
     C                   MOVE      'X'           PRPOIN
     C                   ENDIF
      *
      *  AVERAGE WEEKLY WAGE
      *
     C                   Z-ADD     CLAWW         PRAWW
     C                   Z-ADD     CLCRT         PRCRT
      *
      * #6
      *
      *
     C     CL1A3         IFNE      *BLANK
     C                   MOVE      'X'           PR1A3
     C                   ENDIF
      *
     C     CL1A4         IFNE      *BLANK
     C                   MOVE      'X'           PR1A4
     C                   ENDIF
      *
     C     CLDT1         IFNE      *ZERO
     C                   MOVEL     CLDT1         XXMTH             2
     C                   MOVEL     CLDT1         XXTMP2            4
     C                   MOVE      XXTMP2        XXDAY             2
     C                   MOVE      CLDT1         XXYR              2
     C     XXMTH         CAT       '/':0         PRDT1
     C                   CAT       XXDAY:0       PRDT1
     C                   CAT       '/':0         PRDT1
     C                   CAT       XXYR:0        PRDT1
     C                   ENDIF
      *
     C                   MOVEL(P)  CL20A1        PR20A1
     C                   MOVE      CL10A1        PR10A1
      *
      * #7
      *
     C     CL1A5         IFNE      *BLANK
     C                   MOVE      'X'           PR1A5
     C                   ENDIF
      *
     C     CL1A6         IFNE      *BLANK
     C                   MOVE      'X'           PR1A6
     C                   ENDIF
      *
      * #8
      *
     C                   Z-ADD     CL5N1         PR5N1
     C                   Z-ADD     CL5N3         PR5N3
     C                   Z-ADD     CL5N5         PR5N5
      *
     C                   Z-ADD     CL5N2         PR5N2
     C                   Z-ADD     CL5N4         PR5N4
     C                   Z-ADD     CL5N6         PR5N6
      *
     C                   MOVE      CL10A2        PR10A2
     C                   MOVE      CL10A3        PR10A3
     C                   MOVE      CL10A4        PR10A4
      *
     C                   MOVEL(P)  CL20A2        PR20A2
     C                   MOVEL(P)  CL20A3        PR20A3
     C                   MOVEL(P)  CL20A4        PR20A4
      * ZZZ
     C                   MOVE      CL10A5        PR10A5
     C                   MOVEL(P)  DT1PAY        PR1DAT
      *
     C     CLDT2         IFNE      *ZERO
     C                   MOVEL     CLDT2         XXMTH             2
     C                   MOVEL     CLDT2         XXTMP2            4
     C                   MOVE      XXTMP2        XXDAY             2
     C                   MOVE      CLDT2         XXYR              2
     C     XXMTH         CAT       '/':0         PRDT2
     C                   CAT       XXDAY:0       PRDT2
     C                   CAT       '/':0         PRDT2
     C                   CAT       XXYR:0        PRDT2
     C                   ENDIF
      *
      * #9
      *
     C                   MOVEL(P)  CL60A3        PR60A3
     C                   MOVEL(P)  CL60A4        PR60A4
      *
      * #10
      *
     C                   MOVE      CL10A6        PR10A6
     C     CL60A5        CAT       CL60A6        PROCLC
      *
     C     CL1A9         IFNE      *BLANK
     C                   MOVE      'X'           PR1A9
     C                   ENDIF
      *
     C     CL1A10        IFNE      *BLANK
     C                   MOVE      'X'           PR1A10
     C                   ENDIF
      *
      * #11
      *
     C                   MOVE      CL10A7        PR10A7
      *
     C     CL1A7         IFNE      *BLANK
     C                   MOVE      'X'           PR1A7
     C                   ENDIF
      *
     C     CL1A8         IFNE      *BLANK
     C                   MOVE      'X'           PR1A8
     C                   ENDIF
      *
     C                   MOVEL(P)  CLENME        PRENM1
     C                   MOVEL(P)  CLCXNM        PRCXNM
      *
     C     CLCLEX        CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     UPPHNA        ANDNE     *ZERO
     C     UPPHN#        ANDNE     *ZERO
     C                   MOVE      UPPHNA        XXTMP3            3
     C     '('           CAT(P)    XXTMP3:0      PRCXAR
     C                   CAT(P)    ')':0         PRCXAR
     C                   Z-ADD     UPPHN#        PRCXPH
     C                   ENDIF
      *
     C                   MOVEL(P)  CLTITL        PRTITL
      *
     C                   MOVEL     DSDATE        XXMTH             2
     C                   MOVEL     DSDATE        XXTMP2            4
     C                   MOVE      XXTMP2        XXDAY             2
     C                   MOVE      DSDATE        XXYR              2
     C     XXMTH         CAT       '/':0         PRCLXD
     C                   CAT       XXDAY:0       PRCLXD
     C                   CAT       '/':0         PRCLXD
     C                   CAT       XXYR:0        PRCLXD
      *
     C                   WRITE     F26P1R1
     C                   WRITE     F26P1L1
     C                   WRITE     F26P1R2
      *
     C                   WRITE     F26P2R1
     C                   WRITE     F26P2R2
      *
     C                   WRITE     F26P3R1
     C                   WRITE     F26P3R2
      *
      *  END OF PROGRAM
      *
     C                   MOVE      '1'           *INLR
      *
