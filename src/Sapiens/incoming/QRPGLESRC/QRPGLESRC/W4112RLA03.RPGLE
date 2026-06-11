     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE  W4112RLA03 -  PRINT PROGRAM FOR FORM LDOL-WC-1003
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  THIS PROGRAM IS USED TO PRINT THE       *
     H*  LDOL  SPECIFIC WC-1003 FORM.                                 *
     H*                                                               *
     H* CALLED FROM: W4112CLA03 - OVERRIDE PGM FOR FORM LDOL-WC-1003  *
     H* CALLS TO:  NONE                                               *
     H* PROJECT: 133WET         C:190          01/14/08               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 12/20/13 C:282 Project: 133WOK                                *
     H*                Update the programs to populate                *
     H*                the Date of Birth, Changes to the Length       *
     H*                of Disability value is determined,             *
     H*                'Total settlement amount' will be populated    *
     H*                from 'Comp days paid to date'.                 *
     H*                                                               *
     H* 04/10/09 C:190 SUPPORT: 66187                                 *
     H*                Corrected the issue:"The Date Compensation     *
     H*                Paid Through" field does not reflect the       *
     H*                correct date".                                 *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*****************************************************************
     FWDCLL     IF   E           K DISK
     FWDC2L     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDPAL5    IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FW4112PL3  O    E             PRINTER OFLIND(*IN01)

     D LR              S              7  0 DIM(198)                             ALL WHDATE DATES
     D L               S              7  0 DIM(99)                              LOOSING TIME
     D R               S              7  0 DIM(99)                              RETURNED TO WORK
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRPDT                 1      8  0
     D  XXR1DT                 1      2  0
     D  XXR2DT                 3      4  0
     D  XXR3DT                 5      8  0
     D                 DS
     D  XATODT                 1      8  0
     D  XAT1DT                 1      2  0
     D  XAT2DT                 3      4  0
     D  XAT3DT                 5      8  0
     D                 DS
     D  XXSS#                  1      9
     D  XXSS1                  1      3
     D  XXSS2                  4      6
     D  XXSS3                  7      9
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST - USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR WDC2P
     C*
     C     C2KEY         KLIST
     C                   KFLD                    XXCO#
     C*
      *  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
      *  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
      *  PROVDER MASTER
     C*
     C     PDKY          KLIST
     C                   KFLD                    CMHOSF
     C                   KFLD                    CMHOSM
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
     C                   KFLD                    XXKEY
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
     C     *LIKE         DEFINE    NAKEY         XXKEY
     C     *LIKE         DEFINE    NADESC        XXNATR
     C     *LIKE         DEFINE    PADESC        XXPART
     C     *LIKE         DEFINE    CADESC        XXCAUS
     C     *LIKE         DEFINE    CDAREA        XXCLAR
     C     *LIKE         DEFINE    CDPHN#        XXCLPH
     C     *LIKE         DEFINE    CDADR1        XXCLA1
     C     *LIKE         DEFINE    CDADR2        XXCLA2
     C     *LIKE         DEFINE    CDCTY         XXCLCY
     C     *LIKE         DEFINE    CDST          XXCLST
     C     *LIKE         DEFINE    CDZIP         XXCLZP
     C     *LIKE         DEFINE    UPUSNM        XXUSNM
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
      *   RETRIEVE COMPANY MASTER INFO
     C*
     C     COKY          CHAIN     PCO01                              60
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
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   ELSE
     C                   MOVE      *BLANKS       XXSS#
     C                   ENDIF
     C*
      *  LOAD BALANCE OF UNUSED RESERVES
     C*
     C                   Z-ADD     *ZEROS        XXRR10            9 2
     C                   ADD       CMRR01        XXRR10
     C                   ADD       CMRR02        XXRR10
     C                   ADD       CMRR03        XXRR10
     C                   ADD       CMRR04        XXRR10
     C                   ADD       CMRR05        XXRR10
     C                   ADD       CMRR06        XXRR10
     C                   ADD       CMRR07        XXRR10
     C                   ADD       CMRR08        XXRR10
     C                   ADD       CMRR09        XXRR10
     C                   ADD       CMRR10        XXRR10
     C*
     C*  LOAD DATE COMP. PAID THROUGH,
     C*
     C                   Z-ADD     0             XATODT
     C     CMKY          SETGT     PPA02
     C     *IN60         DOUEQ     *ON
     C     CMKY          READPE    PPA02                                  60
     C     *IN60         IFEQ      *OFF
     C     PARETP        IFEQ      'C'
     C                   Z-ADD     PATODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XATODT
      *** BEG CHG ***** 04/10/09 ******************************************
     C                   LEAVE
      *** END CHG ***** 04/10/09 ******************************************
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C*
     C*  LOAD COST INCURRED FOR THIS CASE
     C*
     C     CMKY          CHAIN     PPA02                              60
     C                   Z-ADD     0             XXAMTT
     C                   Z-ADD     0             XXAMWL
     C                   Z-ADD     0             XXAMPI
     C                   Z-ADD     0             XXAMPT
     C                   Z-ADD     0             XXAMDB
     C                   Z-ADD     0             XXAMFR
     C     *IN60         DOWEQ     '0'
     C*
     C     PABR2         IFEQ      'TT'
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   SUB       PAAMT         XXAMTT            9 2
     C                   ELSE
     C                   ADD       PAAMT         XXAMTT            9 2
     C                   ENDIF                                                  PABR2 = 'TT'
     C                   ENDIF                                                  PATYPE = 'R,S,V'
     C*
     C     PABR2         IFEQ      'WL'
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   SUB       PAAMT         XXAMWL            9 2
     C                   ELSE
     C                   ADD       PAAMT         XXAMWL            9 2
     C                   ENDIF                                                  PABR2 = 'WL'
     C                   ENDIF                                                  PATYPE = 'R,S,V'
     C*
     C     PABR2         IFEQ      'PI'
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   SUB       PAAMT         XXAMPI            9 2
     C                   ELSE
     C                   ADD       PAAMT         XXAMPI            9 2
     C                   ENDIF                                                  PABR2 = 'PI'
     C                   ENDIF                                                  PATYPE = 'R,S,V'
     C*
     C     PABR2         IFEQ      'PT'
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   SUB       PAAMT         XXAMPT            9 2
     C                   ELSE
     C                   ADD       PAAMT         XXAMPT            9 2
     C                   ENDIF                                                  PABR2 = 'PT'
     C                   ENDIF                                                  PATYPE = 'R,S,V'
     C*
     C     PABR2         IFEQ      'DB'
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   SUB       PAAMT         XXAMDB            9 2
     C                   ELSE
     C                   ADD       PAAMT         XXAMDB            9 2
     C                   ENDIF                                                  PABR2 = 'DB'
     C                   ENDIF                                                  PATYPE = 'R,S,V'
     C*
     C     PABR2         IFEQ      'FR'
     C     PABR2         ANDEQ     'CA'
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   SUB       PAAMT         XXAMFR            9 2
     C                   ELSE
     C                   ADD       PAAMT         XXAMFR            9 2
     C                   ENDIF                                                  PABR2 = 'FR,CA'
     C                   ENDIF                                                  PATYPE = 'R,S,V'
     C*
     C     PABR2         IFEQ      'MA'
     C     PABR2         OREQ      'SA'
     C     PABR2         OREQ      'SE'
     C     PABR2         OREQ      'SM'
      *** BEG ADD ***** 12/20/13 ******************************************
     C     PABR2         OREQ      'SB'
     C     PABR2         OREQ      'SC'
     C     PABR2         OREQ      'SF'
     C     PABR2         OREQ      'SI'
     C     PABR2         OREQ      'SP'
     C     PABR2         OREQ      'ST'
     C     PABR2         OREQ      'SU'
     C     PABR2         OREQ      'SS'
     C     PABR2         OREQ      'EL'
      *** END ADD ***** 12/20/13 ******************************************
     C                   ADD       PAAMT         XXAMT1            9 2
     C                   ENDIF                                                  PABR2 = 'MA,SA,SE,SM
     C*
     C     PABR2         IFEQ      'HP'
     C     PABR2         OREQ      'HO'
     C     PABR2         OREQ      'MD'
     C     PABR2         OREQ      'DT'
     C     PABR2         OREQ      'DG'
     C     PABR2         OREQ      'MT'
     C     PABR2         OREQ      'IE'
     C     PABR2         OREQ      'DM'
     C     PABR2         OREQ      'MM'
     C     PABR2         OREQ      'TR'
     C     PABR2         OREQ      'SO'
     C     PABR2         OREQ      'AM'
     C     PABR2         OREQ      'AC'
     C     PABR2         OREQ      'CH'
     C     PABR2         OREQ      'PH'
     C                   ADD       PAAMT         XXAMT2            9 2
     C                   ENDIF                                                  PABR2 = 'HP,HO,MD...
     C*
     C     PABR2         IFEQ      'RH'
     C                   ADD       PAAMT         XXAMT3            9 2
     C                   ENDIF                                                  PABR2 = 'MA,SA,SE,SM
     C*
     C     XXAMTT        ADD       XXAMWL        XXAMT7            9 2
     C     XXAMPI        ADD       XXAMWL        XXAMT8            9 2
     C     XXAMDB        ADD       XXAMFR        XXAMT9            9 2
     C     XXAMT7        ADD       XXAMT8        XXAM10            9 2
     C     XXAMT9        ADD       XXAM10        XXAM11            9 2
     C*
     C*  LOAD LEGAL EXPENSES
     C*
     C     PARECD        IFEQ      'LE'
     C                   ADD       PAAMT         XXAMT4            9 2
     C                   ENDIF                                                  PARECD= 'LE'
     C*
     C     PARECD        IFEQ      'EX'
     C                   ADD       PAAMT         XXAMT5            9 2
     C                   ENDIF                                                  PARECD= 'LE'
     C     XXAMT4        ADD       XXAMT5        XXAMT6            9 2
     C*
     C*
     C     CMKY          READE     PPA02                                  60
     C                   ENDDO
     C*
     C*  TOTAL WORKER COMPENSATION COSTS
     C                   Z-ADD     *ZEROS        XXTWCC            9 2
     C     XXAMTT        ADD       XXTWCC        XXTWCC
     C     XXAMT1        ADD       XXTWCC        XXTWCC
     C     XXAMT2        ADD       XXTWCC        XXTWCC
     C     XXAMT3        ADD       XXTWCC        XXTWCC
     C     XXAMT6        ADD       XXTWCC        XXTWCC
     C*
     C*  LOAD PREPARER'S NAME.
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     UPUSNM        XXUSNM
     C                   ENDIF
     C*
     C*  LOAD EMPLOYER/INSURER, ADDRESS, PHONE.
     C*
     C                   MOVEL(P)  CONAME        XXONM            40
     C                   MOVEL(P)  COADR1        XXOADR           40
     C                   MOVEL(P)  COADR2        X2OADR           40
     C                   MOVEL(P)  COCTY         XXOCTY           25
     C                   MOVEL(P)  COST          XXOST             2
     C                   MOVE      COZIP         XXOZIP            9 0
     C                   MOVE      COAREA        XXAREA            3 0
     C                   MOVEL(P)  COPHN#        XXPHN#            7
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
     C                   END
     C*
      *  DATE OF DISABILITY
     C                   Z-ADD     CMDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDBDT            8 0
     C*
     C                   Z-ADD     CMRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXRTW             8 0
     C                   EXSR      SRDAYS
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
     C                   ELSE
     C                   MOVE      *ZERO         XXCLAR
     C                   MOVE      *ZERO         XXCLPH
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
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
     C*  CONVERT ALL DATES WHICH ARE PRINTED.
     C*
     C*  DATE OF INJURY/ILLNESS
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXACDT            8 0
     C*
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXDTOB            8 0
     C*
     C                   EXSR      @@MOVE
     C                   WRITE     LUBAREC
     C*
     C*
     C*  END OF PROGRAM
     C*
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C*SUBROUTINE TO MOVE VARIABLE VALUES TO PRINTER FILE.
     C***************************************************************
     C     @@MOVE        BEGSR
     C                   MOVE      XXSS1         PRCS1#                         SSN1
     C                   MOVE      XXSS2         PRCS2#                         SSN2
     C                   MOVE      XXSS3         PRCS3#                         SSN3
     C                   MOVE      XXACDT        PRACDT                         INJURY DATE
     C                   MOVEL(P)  CMLSN         PRCLNM
     C                   CAT       ',':0         PRCLNM
     C                   CAT       CMFSN:1       PRCLNM
     C                   CAT       CMMID:1       PRCLNM
     C                   MOVEL(P)  PRCLNM        PRCNM2
      *** BEG ADD ***** 12/20/13 ******************************************
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PRDTOB                         Date of Birth
      *** END ADD ***** 12/20/13 ******************************************
     C                   MOVEL(P)  XXR1DT        PRR1DT                         NOTICE DATE
     C                   MOVEL(P)  XXR2DT        PRR2DT                         NOTICE DATE
     C                   MOVEL(P)  XXR3DT        PRR3DT                         NOTICE DATE
     C                   MOVEL(P)  XXBPRT        PRBPRT                         PART OF BODY
     C                   MOVE      CL1A1         PRL1A1                         PUR OF FORM '1'
     C                   MOVE      CL1A2         PRL1A2                         PUR OF FORM '2'
     C                   MOVE      CL1A3         PRL1A3                         PUR OF FORM '3'
     C                   MOVE      CL1A4         PRL1A4                         PUR OF FORM '4'
     C                   MOVE      CL1A5         PRL1A5                         PUR OF FORM '5'
     C                   MOVE      CL1A6         PRL1A6                         PUR OF FORM '6'
     C                   MOVE      CL1A7         PRL1A7                         OTHER
     C                   MOVE      CL40A1        PR40A1                         OTHER DES
     C                   MOVE      CL20A1        PR20A1                         ICD-9
     C                   MOVE      CL20A2        PR20A2                         CPT PROCEDURE
     C                   MOVEL     XXUSNM        PRUSNM                         PREPARER NAME
     C                   MOVEL     XXONM         PRCONM                         EMPLOYER
     C                   MOVEL     XXOADR        PROADR                         EMP ADDR
     C******               MOVELCOADR2    PROAD2           EMP ADDR
     C                   MOVEL     XXOCTY        PROCTY                         EMP CITY
     C                   MOVEL     XXOST         PROST                          EMP STATE
     C                   MOVE      XXOZIP        PROZIP                         EMP ZIP CODE
     C                   MOVE      XXAREA        PRAREA                         EMP AREA CODE
     C                   MOVE      XXPHN#        PRPHN#                         EMP PHONE
     C                   MOVEL     XXENAM        PRENAM                         EMPLOYEE NAME
     C                   MOVEL(P)  EMPAD1        PRMAD1                         EMPLOYEE NAME
     C******               MOVELEMPAD2    PRMAD2    P      EMPLOYEE ADDR
     C                   MOVEL(P)  EMPCTY        PRMCTY                         EMPLOYEE CITY
     C                   MOVEL(P)  EMPST         PRMST                          EMPLOYEE STATE
     C                   MOVEL(P)  EMPZIP        PRMZIP                         EMPLOYEE ZIP
     C                   MOVE      XXCLAR        PRCLAR                         EMPLOYEE AREA
     C                   MOVE      XXCLPH        PRCLPH                         EMPLOYEE PHN
     C                   MOVEL     XAT1DT        PRT1DT                         DATE COMP.
     C                   MOVEL     XAT2DT        PRT2DT                         DATE COMP.
     C                   MOVEL     XAT3DT        PRT3DT                         DATE COMP.
     C                   MOVE      XXAMTT        PRAMTT                         TEMP. TOTAL
     C                   MOVE      XXAMWL        PRAMWL                         SUPP. EARNING
     C                   MOVE      XXAMPI        PRAMPI                         PER. PARTIAL
     C                   MOVE      XXAMPT        PRAMPT                         PER. TOTAL
     C                   MOVE      XXAMDB        PRAMDB                         DEATH BENEFITS
     C                   MOVE      XXAMFR        PRAMFR                         OTHER BENEFITS
     C                   MOVE      XXAMT1        PRAMT1                         SETTLEMENT AMOUNT
     C                   MOVE      XXAMT2        PRAMT2                         TOT MED EXPENSES
     C                   MOVE      XXAMT3        PRAMT3                         TOT REHAB EXPENSES
     C                   MOVE      XXAM11        PRAM11                         TOT INDEM BENEFITS
     C                   MOVE      XXAMT4        PRAMT4                         ATTORNEY FEE
     C                   MOVE      XXAMT5        PRAMT5                         ADMIN/OTHER COST
     C                   MOVE      XXAMT6        PRAMT6                         TOT LAGAL EXPENSES
     C                   MOVE      XXRR10        PRRR10                         BALANCE OF UNUSED RE
     C                   MOVE      XXWKS         PRWKS                          DISABILITY WEEKS
     C                   MOVE      XDAYS         PDAYS                          DISABILITY DAYS
     C                   MOVE      XXTWCC        PTWCC                          TOTAL WORKER CO
     C************************************************
     C                   ENDSR
      * ================================================================
     C     SRDAYS        BEGSR
      *
     C                   Z-ADD     1             Y
     C                   Z-ADD     1             Z                 3 0
     C     LR(Y)         DOUEQ     *ZERO
     C                   Z-ADD     LR(Y)         L(Z)
     C                   ADD       1             Y
     C                   Z-ADD     LR(Y)         R(Z)
     C                   ADD       1             Y
     C                   ADD       1             Z
     C                   ENDDO
      *
     C                   Z-ADD     1             Y                 3 0
     C                   Z-ADD     *ZERO         XXTADL            5 0
      *
     C     L(Y)          DOUEQ     *ZERO
     C                   Z-ADD     L(Y)          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         7,0 --> 8,8
     C                   MOVEL(P)  SRDTO         @@FROM            8 0
      *
     C                   Z-ADD     R(Y)          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         @@TO              8 0
      *
     C     @@FROM        IFNE      *ZERO
     C     @@TO          ANDNE     *ZERO
     C                   CALL      'SRDDIF8'
     C                   PARM                    @@FROM
     C                   PARM                    @@TO
     C                   PARM      *ZERO         @@DAYS            5 0
     C                   PARM      *BLANK        @@ERR             1
      *
     C                   ADD       @@DAYS        XXTADL
     C                   ADD       1             Y
     C                   ELSE
     C                   LEAVE
     C                   END
     C                   ENDDO
      *
      *** BEG CHG ***** 12/20/13 ******************************************
      *****   NOW ADD DAYS BETWEEN DATE OF DISABILITY & RTW DATE
      *****
     C*****              Z-ADD     XXDBDT        @@FROM
     C*****              Z-ADD     XXRTW         @@TO
      *****
     C*****              CALL      'SRDDIF8'
     C*****              PARM                    @@FROM
     C*****              PARM                    @@TO
     C*****              PARM      *ZERO         @@DAYS            5 0
     C*****              PARM      *BLANK        @@ERR
      *****
     C*****              ADD       @@DAYS        XXTADL
     C*****XXTADL        DIV       7             XXWKS             5 0
     C*****              MVR                     XDAYS             5 0
     C     CMLOST        DIV       7             XXWKS             5 0
     C                   MVR                     XDAYS             5 0
      *** END CHG ***** 12/20/13 ******************************************
     C                   ENDSR
      * ================================================================
     C**************************************************************************
