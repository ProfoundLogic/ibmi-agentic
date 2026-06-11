     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE  W4112RLA02 -  PRINT PROGRAM FOR FORM LUBA-WC-1002
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  THIS PROGRAM IS USED TO PRINT THE       *
     H*  LUBA  SPECIFIC WC-1002 FORM.                                 *
     H*                                                               *
     H* CALLED FROM: W4112CLA02 - OVERRIDE PGM FOR FORM LUBA-WC-1002  *
     H* CALLS TO:  NONE                                               *
     H* PROJECT: 133WET         C:190          01/14/08               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*****************************************************************
     FWMUPL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWDCLL     IF   E           K DISK
     FWDC2L     IF   E           K DISK
     FW4112PL2  O    E             PRINTER OFLIND(*IN01)

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXACDT                 1      8  0
     D  XXACD1                 1      2  0
     D  XXACD2                 3      4  0
     D  XXACD3                 5      8  0
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
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST - USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
      *  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
      *  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
      *  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
      *   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
      *   KEY LIST FOR THE TABLE FILES
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CDAREA        XXCLAR
     C     *LIKE         DEFINE    CDPHN#        XXCLPH
     C     *LIKE         DEFINE    CDADR1        XXCLA1
     C     *LIKE         DEFINE    CDADR2        XXCLA2
     C     *LIKE         DEFINE    CDCTY         XXCLCY
     C     *LIKE         DEFINE    CDST          XXCLST
     C     *LIKE         DEFINE    CDZIP         XXCLZP
     C     *LIKE         DEFINE    UPUSNM        XXUSNM
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    @@CLT#
     C*
     C                   TIME                    DSDTTM
     C*
     C                   READ      PCL01                                  60
     C                   READ      PC201                                  60
     C                   MOVE      CLCO#         XXCO#
     C                   MOVE      CLFND         XXFND
     C                   MOVE      CLFYR         XXFYR
     C                   MOVE      CLFCLM        XXCASE
     C*
      *  CALL SYSTEM API TO GET VARIOUS SYSTEM VALUES.
     C*
     C                   CALL      'SA001R'
     C                   PARM                    @@WCIF            1
     C                   PARM                    @@MCIF            1
     C                   PARM                    @@WCIN            1
     C                   PARM                    @@AR              1
     C                   PARM                    @@NTWK            1
     C                   PARM                    @@UR              1
     C                   PARM                    @@MED             1
     C                   PARM                    @@EDI             1
     C                   PARM                    @@WCDK            1
     C                   PARM                    @@RHAB            1
     C                   PARM                    @@MSS             1
     C                   PARM                    @@FSCH            1
     C                   PARM                    @@PRVB            1
     C                   PARM                    @@NPWC            1
     C                   PARM                    @@OS              1
     C                   PARM                    @@ARLS            1
     C                   PARM                    @@CCOD            1
     C                   PARM                    @@CRVW            1
     C                   PARM                    @@PROM            1
     C                   PARM                    @@MCRV            1
     C                   PARM                    @@FREQ            1
     C                   PARM                    @@CRAC            1
     C                   PARM                    @@PREO            1
     C                   PARM                    @@PARN            1
     C                   PARM                    @@MGRI            1
     C                   PARM                    @@KCIF            1
     C                   PARM                    @@CURS            1
     C                   PARM                    @@PCIB            1
     C                   PARM                    @@CLT#            3
     C                   PARM                    @@UCPF            1
     C                   PARM                    @@DCF             1
     C                   PARM                    @@AUTB            1
     C*
      *  INDICATE REPORT DATE
     C*
     C                   MOVE      DSDATE        XXRPDT            8 0
     C*
      *  LOAD CLAIMANT INFORMATION
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     CDKY          CHAIN     PCD01                              60
     C*
      *  LOAD CLAIMANT SOCIAL SECURITY NUMBER AND NAME
     C*
     C     CMSS#         IFNE      *BLANKS
     C                   MOVE      CMSS#         XXSS#
     C                   MOVEL     XXSS1         XXCSS#           11
     C     XXCSS#        CAT       '-':0         XXCSS#
     C     XXCSS#        CAT       XXSS2:0       XXCSS#
     C     XXCSS#        CAT       '-':0         XXCSS#
     C                   MOVE      XXSS3         XXCSS#
     C                   ELSE
     C                   MOVE      *BLANKS       XXSS#
     C                   MOVE      *BLANKS       XXCSS#
     C                   ENDIF
     C*
     C                   MOVEL     CLDT1         XXLDT1
     C*
     C                   Z-ADD     CLDT2         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXLDT2            8 0
     C*
     C*  LOAD PREPARER'S NAME.
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     UPUSNM        XXUSNM
     C                   ENDIF
     C*
     C*  LOAD EMPLOYER/INSURER, ADDRESS, PHONE.
     C*
     C     COKY          CHAIN     PCO01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CONAME        XXONM            40
     C                   MOVEL(P)  COADR1        XXOADR           40
     C                   MOVEL(P)  COADR2        X2OADR           40
     C                   MOVEL(P)  COCTY         XXOCTY           25
     C                   MOVEL(P)  COST          XXOST             2
     C                   MOVE      COZIP         XXOZIP            9 0
     C                   MOVE      COAREA        XXAREA            3 0
     C                   MOVEL(P)  COPHN#        XXPHN#            7
     C                   ENDIF
     C*
     C*  GET THE EMPLOYER INFORMATION
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  EMNAME        XXENAM           40
     C*
     C     EMPAD1        IFEQ      *BLANKS
     C                   MOVE      EMMAD1        EMPAD1
     C                   MOVE      EMMAD2        EMPAD2
     C                   MOVE      EMMCTY        EMPCTY
     C                   MOVE      EMMST         EMPST
     C                   Z-ADD     EMMZIP        EMPZIP
     C                   END
     C*
     C                   END
     C*
      *  LOAD  PHONE NUMBER
     C*
     C     EMAREA        IFNE      *ZERO
     C                   MOVE      EMAREA        XXCLAR
     C                   ELSE
     C                   MOVE      *ZERO         XXCLAR
     C                   ENDIF
     C     EMPHN#        IFNE      *ZERO
     C                   MOVE      EMPHN#        XXCLPH
     C****                 ELSE
     C****                 MOVE *ZERO     XXCLAR
     C****                 MOVE *ZERO     XXCLPH
     C                   ENDIF
     C*
     C*
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL(P)  CMNATR        XXKEY
     C*
     C     TBKY          CHAIN     PNA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  NADESC        XXNADS           30
     C                   ELSE
     C                   MOVE(P)   *BLANK        XXNADS
     C                   ENDIF
     C*
      *  RETRIEVE THE PART DESCRIPTION.
     C*
     C                   MOVEL(P)  CMPART        XXKEY
     C     TBKY          CHAIN     PPA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PADESC        XXBPRT           30
     C                   ELSE
     C                   MOVE(P)   *BLANK        XXBPRT
     C                   ENDIF
     C*
      *   RETRIEVE THE CAUSE DESCRIPTION.
     C*
     C                   MOVEL(P)  CMCAUS        XXKEY
     C     TBKY          CHAIN     PCA01                              60
     C  N60              MOVEL     CADESC        XXCAUS           30
     C*
     C*  CONVERT ALL DATES WHICH ARE PRINTED.
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXACDT
     C*
     C                   EXSR      @@MOVE
     C                   WRITE     LUBAREC
     C*
     C*  END OF PROGRAM
     C*
     C                   MOVE      '1'           *INLR
     C*
     C***************************************************************
     C*SUBROUTINE TO MOVE VARIABLE VALUES TO PRINTER FILE.
     C***************************************************************
     C     @@MOVE        BEGSR
     C                   MOVE      XXSS1         PRCS1#
     C                   MOVE      XXSS2         PRCS2#
     C                   MOVE      XXSS3         PRCS3#
     C                   MOVEL(P)  CMLSN         PRCLNM
     C                   CAT       ',':0         PRCLNM
     C                   CAT       CMFSN:1       PRCLNM
     C                   CAT       CMMID:1       PRCLNM
     C                   MOVEL(P)  PRCLNM        PRCNM2
     C                   MOVE      CL1A1         PRL1A1
     C                   MOVE      CL1A2         PRL1A2
     C                   MOVE      CL1A3         PRL1A3
     C                   MOVE      CL1A4         PRL1A4
     C                   MOVE      CL1A5         PRL1A5
     C                   MOVE      CL1A6         PRL1A6
     C                   MOVE      CL1A7         PRL1A7
     C                   MOVE      CL1A8         PRL1A8
     C                   MOVE      CL1A9         PRL1A9
     C                   MOVE      CL1A10        PRL110
     C                   MOVE      CL1A11        PRL111
     C                   MOVE      CL1A12        PRL112
     C                   MOVE      CL1A13        PRL113
     C                   MOVE      CL1A14        PRL114
     C                   MOVE      CL1A15        PRL115
     C                   MOVE      CL1A16        PRL116
     C                   MOVE      CL1A17        PRL117
     C                   MOVE      CL1A18        PRL118
     C                   MOVE      CL1A19        PRL119
     C                   Z-ADD     CL5N1         PRL5N1
     C                   Z-ADD     CL5N2         PRL5N2
     C                   Z-ADD     CL9N1         PRL9N1
     C                   Z-ADD     CL9N2         PRL9N2
     C                   Z-ADD     CL9N3         PRL9N3
     C                   Z-ADD     CL9N4         PRL9N4
     C                   Z-ADD     CL9N5         PRL9N5
     C                   Z-ADD     CL9N6         PRL9N6
     C                   MOVEL     CL40A1        PR40A1
     C                   MOVEL     CL40A2        PR40A2
     C                   MOVE      XXL1DT        PRL1DT
     C                   MOVE      XXL2DT        PRL2DT
     C                   MOVE      XXL3DT        PRL3DT
     C                   MOVE      XXLDT2        PRLDT2
     C                   MOVEL     XXBPRT        PRBPRT
     C                   MOVEL     XXNADS        PRNADS
     C                   MOVEL     XXONM         PRCONM
     C                   MOVEL     XXOADR        PROADR
     C********             MOVELCOADR2    PROAD2
     C                   MOVEL     XXOCTY        PROCTY
     C                   MOVEL     XXOST         PROST
     C                   MOVE      XXOZIP        PROZIP
     C                   MOVEL     XXENAM        PRENAM
     C                   MOVEL     EMPAD1        PRMAD1
     C********             MOVELEMPAD2    PRMAD2
     C                   MOVEL     EMPCTY        PRMCTY
     C                   MOVEL     EMPST         PRMST
     C                   MOVEL     EMPZIP        PRMZIP
     C                   MOVE      XXCLAR        PRCLAR
     C                   MOVE      XXCLPH        PRCLPH
     C                   MOVE      XXACD1        PRACD1
     C                   MOVE      XXACD2        PRACD2
     C                   MOVE      XXACD3        PRACD3
     C                   Z-ADD     CMAWW         PRCMRT
     C                   MOVEL     XXUSNM        PRUSNM                         PREPARER NAME
     C                   MOVEL     XXAREA        PRAREA                         SMCOP/COAREA
     C                   MOVEL     XXPHN#        PRPHN#                         SMCOP/COPHN#
     C*
     C                   ENDSR
     C**************************************************************************
