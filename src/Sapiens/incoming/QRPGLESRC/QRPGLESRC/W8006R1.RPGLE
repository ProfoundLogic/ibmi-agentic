     H* (c) 2011 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE W8006R1 - FEE INVOICES
     H* ***************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*   THIS PROGRAM WILL PRINT FEEE INVOICES BASED ON              *
     H*   RECORDS CREATED BY THE "CALCULATE  BILL AMOUNTS"            *
     H*   PROGRAM (W8006R) FOR THE INVOICE DATE SPECIFIED.            *
     H* CALLED FROM:                                                  *
     H*   W8006C                                                      *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H* PROGRAMMER: C:189                                             *
     H* CREATED: 01/25/10               PROJECT: WC1851               *
     H* ***************************************************************
     H* INDICATORS:                                                   *
     H*   *IN60 - GENERAL CHAIN, DOWEQ                                *
     H* ***************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/20/11 C:65  WC1851                                         *
     H*                Change the current vs prior billed check to    *
     H*                use the posting date passed in vs the posting  *
     H*                date within the fee detail record itself.      *
     H*                                                               *
     H* 02/15/11 C:061 WC1851                                         *
     H*               -Changes not commented.                         *
     H*               -Change to use SPRDATE routines.                *
     H*               -Remove SRFND# routine (use SRFND instead)      *
     H*               -Remove GOTOs.                                  *
     H*                                                               *
     H* 05/20/10 C:190 WC1851                                         *
     H*                Corrected 'Call' parameters for SRFUTR8         *
     H*                Removed Reference of Previous Balance date.
     H*                                                               *
     H* ***************************************************************
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMEML     IF   E           K DISK
      *
      * Employer Contact
     FWMECL1    IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PSY01)
     FWMEAL     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWDELL2    IF   E           K DISK
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL03)
     FWMA3L     IF   E           K DISK
     FWMOFL     IF   E           K DISK
     FWMRPL     IF   E           K DISK
      * Batch Detail Fund
     FWDEAL5    IF   E           K DISK
     F                                     RENAME(PEA01:PEA05)
     FWMEXL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
      * Application System Control
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDFCL1    IF   E           K DISK
     FWXGSL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     FW8006R1O  O    E             PRINTER OFLIND(*IN01)
     F                                     USROPN
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D OCL             S              1    DIM(32)                              SCANLINE
     D WTL             S              1    DIM(46) CTDATA PERRCD(46)            WEIGHT
     D MN              S              9    DIM(12) CTDATA PERRCD(1)             MONTH NAME

     d x2podt          s              7  0
     d x2lsdt          s              7  0
     d x2pbdt          s              7  0
     d xxdate          s              7  0
     d sysDate8        s              8  0
     d xxcndt          s              8  0
     d q#cdat          s              8
     d ##days          s              5  0
     d rpdate          s              8  0
     DsingleEmp        S              1n

     D                 DS
     D  EMN                    1    400
     D                                     DIM(8)
     D  XXEMN1                 1     50
     D  XXEMN2                51    100
     D  XXEMN3               101    150
     D  XXEMN4               151    200
     D  XXEMN5               201    250
     D  XXEMN6               251    300
     D  XXEMN7               301    350
     D  XXEMN8               351    400
     D                 DS
     D  ATT                    1     40
     D                                     DIM(8)
     D  XXATT1                 1      5
     D  XXATT2                 6     10
     D  XXATT3                11     15
     D  XXATT4                16     20
     D  XXATT5                21     25
     D  XXATT6                26     30
     D  XXATT7                31     35
     D  XXATT8                36     40
     D                 DS
     D  ARY                    1     33  0
     D                                     DIM(11)
     D  ARY01                  1      3  0
     D  ARY02                  4      6  0
     D  ARY03                  7      9  0
     D  ARY04                 10     12  0
     D  ARY05                 13     15  0
     D  ARY06                 16     18  0
     D  ARY07                 19     21  0
     D  ARY08                 22     24  0
     D  ARY09                 25     27  0
     D  ARY10                 28     30  0
     D  ARY11                 31     33  0
     D                 DS
     D  ARA                    1    132  2
     D                                     DIM(12)
     D  ARA01                  1     11  2
     D  ARA02                 12     22  2
     D  ARA03                 23     33  2
     D  ARA04                 34     44  2
     D  ARA05                 45     55  2
     D  ARA06                 56     66  2
     D  ARA07                 67     77  2
     D  ARA08                 78     88  2
     D  ARA09                 89     99  2
     D  ARA10                100    110  2
     D  ARA11                111    121  2
     D  ARA12                122    132  2
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D                 DS
     D  OFAPHN                 1      7  0
     D  XXPHN1                 1      3  0
     D  XXPHN2                 4      7  0
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS
     D  XXOCR                  1     47
     D  ##TYPE                 1      1
     D  #1FND                  2      4  0
     D  #1EMP#                 5     13  0
     D  #1DIV                 14     18  0
     D  ##FYR                 19     21  0
     D  XXAMT1                22     32  2
     D  XXCKDG                33     33  0
     D                 DS
     D  XQDT                   1      8  0
     D  XXMTH                  1      2  0
     D  XXDAY                  3      4
     D  XXYR                   5      8
     D*
     D CARPOL          C                   CONST('Carrier Policy #: ')
     D POL             C                   CONST('Policy #:')
     D FILE            C                   CONST('File #:')
     D LOSSH           C                   CONST('Losses as of:')
     D CANDH           C                   CONST('Cancellation Date:')
     D DEDHDR          C                   CONST('FEE  INVOICE')
     D SUMHDR          C                   CONST('SUMMARY FEE-
     D                                      INVOICE')
     D BREAKH          C                   CONST('Break Out of Current-
     D                                      Billed by Policy-
     D                                      Year:')
     D SEEH            C                   CONST('See Itemization Atta-
     D                                     ched for Detail')
     D BATH            C                   CONST('Internal Use: Pendin-
     D                                     g Payment Batch#')
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D CHGNUM          C                   CONST('SA020R')
     D DA              C                   CONST('FEE INVOICE  .RUN -
     D                                     ON:')
     D D71             C                   CONST('FEE   INVOICE -
     D                                      RUN ON:')
     D DA2             C                   CONST('FOR  A/R POSTING DAT-
     D                                     E:')
     D                 DS
     D  ZZKEY                  1     40
     D  ZZCO#                  1      3  0
     D  ZZFND                  4      6  0
     D  ZZEMP#                 7     15  0
     D  ZZDIV                 16     20  0

     d/copy sPrDate


     IPEA05
     I              EAACHF                      $AACHF
     I              EAARCD                      $AARCD
     I              EAAMT                       $AAMT
     I              EAPODT                      $APODT
     I              EAFYR                       $AFYR
      *
      *
      *===============================================================
      * KLIST
      *===============================================================
      *
      *  Employer Contact
      *
     C     ECKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKPCNT            1
      *
     C*
     C*   KEY LIST FOR THE COMPANY MASTER FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    xxCo#
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    xxCo#
     C                   KFLD                    FCFND
     C*
     C*   KEY LIST FOR THE OFFICE MASTER FILE
     C*
     C     OFKY          KLIST
     C                   KFLD                    xxCo#
     C                   KFLD                    EMOFF#
     C*
     C*   KEY LIST FOR THE FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    xxCo#
     C                   KFLD                    FCFND
     C                   KFLD                    XXBFYR
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    xxCo#
     C                   KFLD                    FCFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C*
     C*  KEY LIST FOR THE FEE DETAIL FILE
     C*
     C     keyWDFCL1     KLIST
     C                   KFLD                    xxCo#
     C                   KFLD                    xxFnd
     C*
     C     key1WDFCL1    KLIST
     C                   KFLD                    xxCo#
     C                   KFLD                    xxFnd
     C                   KFLD                    xxEmp#
     C                   KFLD                    xxDiv
     C*
     C*  KEY LIST FOR THE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    xxCo#
     C                   KFLD                    XXKEY
     C*
     C*  KEY LIST FOR THE EMPLOYER A/R FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    xxCo#
     C                   KFLD                    FCFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY1         KLIST
     C                   KFLD                    xxCo#
     C                   KFLD                    FCFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    X2PODT
     C*
     C     ELKY2         KLIST
     C                   KFLD                    xxCo#
     C                   KFLD                    FCFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C*
     C     ELKY3         KLIST
     C                   KFLD                    xxCo#
     C                   KFLD                    FCFND
     C                   KFLD                    FCEMP#
     C                   KFLD                    FCDIV
     C                   KFLD                    FCFYR
      *
      * KEY LIST FOR THE CARRIER/STATE REPORTING FILE
      *
     C     RPKY          KLIST
     C                   KFLD                    xxCo#
     C                   KFLD                    FCFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    XXBFYR
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    xxCo#
     C                   KFLD                    FCFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    XXBFYR
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    xxCo#
     C                   KFLD                    FCFND
     C                   KFLD                    XXAGT#            5 0
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    xxCo#
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C*
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    xxCo#
     C                   KFLD                    XXRTCD            3
      *
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
      *
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
     C     FNKY          KLIST
     C                   KFLD                    xxCo#

      *=================================================================
      * *LIKE
      *=================================================================
     C     *LIKE         DEFINE    FCCO#         xxCo#
     C     *LIKE         DEFINE    FCPODT        X2PODT
     C     *LIKE         DEFINE    BCKEY         XXKEY
     C     *LIKE         DEFINE    EMMAD2        XXCSZ
     C     *LIKE         DEFINE    EMMCTY        XXCTY
     C     *LIKE         DEFINE    EMMST         XXST

      *=================================================================
      * PLIST
      *=================================================================
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQPBDT            8            PREV BAL DATE
     C                   PARM                    QQLSDT            8            LOSS DATE
     C                   PARM                    QQPODT            8            POST DATE
     C                   PARM                    QQPRDT            8            PRINT DATE
     C                   PARM                    QQCOM1            2            REMARK 1
     C                   PARM                    QQCOM2            2                   2
     C                   PARM                    QQCOM3            2                   3
     C                   PARM                    QQCOM4            2                   4
     C                   PARM                    QQOUTQ           10
     C                   PARM                    QQDFLG            1
     C                   PARM                    QQDRMK            5
     C                   PARM                    QQEXCL            1

     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10


      *=================================================================
      * MAIN
      *=================================================================
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQCO#         xxCo#
     C                   MOVE      QQEMP#        xxEmp#            9 0
     C                   MOVE      QQDIV         xxDiv             5 0
     C                   MOVE      QQFND         xxFnd
     C                   MOVE      QQDFLG        XXDFLG            1
     C                   MOVE      QQDRMK        XXDRMK            5
     C                   MOVE      QQPODT        XXPODT            8 0
     c                   eval      x2podt = dtcymd(xxpodt)
     C                   MOVE      QQLSDT        X1LSDT            8 0
     c                   eval      x2lsdt = dtcymd(x1lsdt)
     C                   MOVE      QQPBDT        XXPBDT            8 0
     c                   eval      x2pbdt = dtcymd(xxpbdt)
     c                   eval      xxdate = dtcymd(dsdate)
     C                   MOVEL     X1LSDT        XXLSDT            8 0
     C                   MOVE      *ZEROS        ED
     C                   MOVE      *ZEROS        CD
     C                   MOVE      QQPRDT        X1PRDT            8 0
     C                   MOVEL     X1PRDT        XXPRDT            8 0
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*
     C     COKEY         CHAIN     PCO01                              60
     c                   eval      rpdate = dtCheck(dsdate)
     **
     C                   MOVE      QQLSDT        RSDATE            8 0
     C                   MOVE      QQPODT        RSDAT2            8 0
     **
     C     ORKEY         CHAIN     POR01                              60
     C*
     C* PULL THE HOLD STATUS FOR NEW FORMAT BILLS FROM REPORT #0.
     C*
     C                   MOVE      '*YES'        X1HOLD            4
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ORHOLD        X1HOLD
     C                   ENDIF                                                  *IN60 IFEQ
      *
      *  Get Office Install Flag
      *
     C                   MOVEL     'SY'          XXSYK
     C     SYKY          CHAIN     SSY01                              60
     C     UPKEY         CHAIN     PUP01                              60
      *
      *  Process Groups
      *
     C                   IF        xxEmp# = 0
      *
     C*  Read through file to make sure at least one group is included
      *
     C     FNKY          CHAIN     PGS01                              71
     C                   DOW       *IN71=*OFF
      *
     C                   IF        GSIEFL =  'I'
     c                   eval      singleEmp = *off
     c                   eval      xxFnd = GSFND
     C                   EXSR      SRFND
     C                   ENDIF
      *
     C     FNKY          READE     PGS01                                  71
     C                   ENDDO

     C                   ELSE

     c                   eval      singleEmp = *on
     C                   EXSR      SRFND
     C                   ENDIF                                              QQEMP# = ' '
      *
     C                   MOVE      *ON           *INLR
      *
      *
      *=============================================================
      * SRFND
      *=============================================================
     C     SRFND         BEGSR
     C                   CLEAR                   SZ
     C                   CLEAR                   ED
     C                   CLEAR                   CD
     C                   CLEAR                   ARA
     C                   CLEAR                   ARY
     C                   CLEAR                   EMN
     C                   CLEAR                   ATT
     C                   MOVE      *ALL'0'       *IN
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     FNKEY         CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     EXAHDR        IFEQ      *BLANKS
     C                   MOVEL(P)  'Agent'       EXAHDR
     C                   ENDIF
     c                   if        xxpodt = 0
     c                   eval      ##days = EXDFID
     c                   eval      x2podt = dtFuture(sysdate8:##days)

     c                   else

     c                   eval      x2podt = dtcymd(xxpodt)
     c                   endif

     c                   if        qqdrmk = *blanks
     c                   eval      xxdrmk = EXFIRM
     c                   endif

     C                   ELSE
     C                   MOVEL(P)  'Agent'       EXAHDR
     c                   if        xxpodt = 0
     c                   eval      x2podt = xxdate
     c                   endif
     C                   ENDIF
     C*
     C                   MOVE      *OFF          *IN66
     C     FNCGFL        IFEQ      'Y'
     C                   MOVE      *ON           *IN66
     C                   ENDIF                                                  FNCGFL IFEQ
     C*
     C*  LOAD PRINT FIELD FOR REMITTANCE CITY/ST/ZIP
     C*
     C                   MOVEL     FNNAME        XNNAME           40
     C     FNMAD2        COMP      *BLANKS                                31
     C                   MOVE      FNMST         XXST
     C                   Z-ADD     FNMZIP        XXZIP
     C                   MOVEL     FNMCTY        XXCTY
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXST          XXST13           13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     **
      *  Check for Lock box address in Group Auxiliary file
      *  If exists use lock box address instead of Accounting Ofc addr
     C     FNKEY         CHAIN     PF201                              60
     C                   MOVE      ' '           XXLKBX            1
      *
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        FNCSZ            40
     C*
     C     QQCOM1        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     QQCOM1        XXKEY
     C     TBKEY         CHAIN     PBC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXCOM1
     C                   MOVEL     BCDESC        XXCOM1
     C                   END
     C                   END
     C     QQCOM2        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     QQCOM2        XXKEY
     C     TBKEY         CHAIN     PBC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     BCDESC        XXCOM2
     C                   END
     C                   END
     C     QQCOM3        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     QQCOM3        XXKEY
     C     TBKEY         CHAIN     PBC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXCOM3
     C                   MOVEL     BCDESC        XXCOM3
     C                   END
     C                   END
     C     QQCOM4        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     QQCOM4        XXKEY
     C     TBKEY         CHAIN     PBC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXCOM4
     C                   MOVEL     BCDESC        XXCOM4
     C                   END
     C                   END
     C*
     C* TOTAL ALL AMOUNT FIELDS FOR SUMMARY INVOICE FOR PEO POLICIES.
     C*
     C                   Z-ADD     0             SVAMT             9 2
     C                   Z-ADD     0             SVBILL            9 2
     C                   Z-ADD     0             SVPAID            9 2
     C                   Z-ADD     0             SVPTAX            9 2
     C                   Z-ADD     0             SVBTAX            9 2
     C                   Z-ADD     0             SVCTAX            9 2
     C*
     C*  READ ALL CHARGE FILE DETAIL RECORDS
     C*
     C                   Z-ADD     *ZEROS        XXAMT             9 2
     C                   Z-ADD     *ZEROS        XXBILL            9 2
     C                   Z-ADD     *ZEROS        XXCMCB            9 2
     C                   Z-ADD     *ZEROS        XXCMPB            9 2
     C                   Z-ADD     *ZEROS        XXCLCB            9 2
     C                   Z-ADD     *ZEROS        XXCLPB            9 2
     C                   Z-ADD     FCEMP#        SVEMP#
     C                   Z-ADD     FCDIV         SVDIV
     C                   Z-ADD     0             SVFYR             3 0
     C                   Z-ADD     0             X                 3 0
     C                   Z-ADD     70            IX                3 0
     C                   CLEAR                   ARA
     C                   CLEAR                   ARY
     C                   MOVEA     '00000000'    *IN(71)
     C                   MOVEA     '0000'        *IN(79)
     C
     C                   select
     C                   when      singleEmp
     C     key1WDFCL1    setll     pfc01
     C                   other
     C     keyWDFCL1     setll     pfc01
     C                   endsl
     C*
     C                   dou       %eof(wdfcl1)
     C*
     C                   select
     C                   when      singleEmp
     C     key1WDFCL1    reade     pfc01
     C                   other
     C     keyWDFCL1     reade     pfc01
     C                   endsl
     C*
     C                   if        not %eof(wdfcl1)
     C*
     C     FCEMP#        IFNE      SVEMP#
     C     FCDIV         ORNE      SVDIV
     C*
     C                   EXSR      SREBRK
     C** Check Activity based on Losses as of date
     C                   EVAL      @@ACTVTY = *BLANKS
     C                   IF        EATRDT  >   X2LSDT
     C                   MOVE      'Y'           @@ACTVTY          1
     C                   ENDIF
     C                   MOVE      'Y'           @@ACTVTY          1

     C*  If Exclude flag is set to 'Y' and employer have NO activity with
     c*  no prior balance, exclude it.
     C                   EVAL      @@PRBAL = *BLANKS
     C                   IF        XPRBAL <> 0 OR XPRBAL1 <>  0
     C                   MOVE      'Y'           @@PRBAL           1
     C                   ENDIF
     C                   IF        @@PRBAL <> 'Y'
     C                   IF        QQEXCL = 'Y' AND @@ACTVTY <> 'Y'
     C                   iter
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     0             SVFYR             3 0
     C                   Z-ADD     0             X                 3 0
     C                   Z-ADD     70            IX                3 0
     C                   CLEAR                   ARA
     C                   CLEAR                   ARY
     C                   MOVEA     '00000000'    *IN(71)
     C                   MOVEA     '0000'        *IN(79)
     C*
     C                   ENDIF                                                  DDEMP# <> SVEMP#
     C*
     c* Previous balance would be based on the Posting Date entered on the
     c* prompt screen based on the posting date in A/R for already
     c* billed/paid items.
     c*
     c*** begin chg ***  04/20/11  ********************************************************
     C*****EAKEY         SETGT     PEA01
     C*****EAKEY         READPE    PEA01                                  60
     C*****EAPODT        IFGE      X2PODT
     C     FCPODT        IFGE      X2PODT
     c*** end chg *****  04/20/11  ********************************************************
     C                   ADD       FCCFCB        XXCMCB                         CURR BILLED
     C                   ADD       FCCOFB        XXCLCB                         CURR BILLED
     C                   ELSE
     c*** begin chg ***  04/20/11  ********************************************************
     C*****EAPODT        IFLT      X2PODT
     C     FCPODT        IFLT      X2PODT
     c*** end chg *****  04/20/11  ********************************************************
     C                   ADD       FCCFCB        XXCMPB                         PRV BILLED
     C                   ADD       FCCOFB        XXCLPB                         PRV BILLED
     C                   END
     C                   END

     C                   endif
     C                   enddo                                                  DOWEQ *IN50 '0'
     C*
     C                   EXSR      SREBRK
     C     FNPEOF        IFNE      *BLANK
     C                   EXSR      SUMINV
     C                   ENDIF
     C*
     C                   ENDSR
      *
      *===============================================================
      * SRCONT - Get Employer Contact Name
      *===============================================================
      *
     C     SRCONT        BEGSR
     C                   CLEAR                   XXCNT
      *
      * Get Contact Name
      *
     C     ECKY          CHAIN     PEC01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     ECFSN         IFNE      *BLANKS
     C     ECFSN         CAT       ECLSN:1       XXCNT            50
      *
     C                   ELSE
      *
     C     ECTITL        CAT       ECLSN:1       XXCNT
     C                   ENDIF
      *
     C                   ENDIF                                                  ECKY
      *
     C                   ENDSR                                                  SRCONT
      *
      *
      * =================================================================
     C* SREBRK SUBROUTINE              ** SUBROUTINE **   S R E B R K *
      * =================================================================
     C     SREBRK        BEGSR
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXCHEC           40
     C*
     C     OFKY          CHAIN     POF01                              62
     C     *IN62         IFEQ      *OFF
     C*
     C*  LOAD PRINT FIELD FOR REMITTANCE CITY/ST/ZIP
     C*
     C                   MOVEL     OFCKNM        XXCHEC
     C   66              MOVEL(P)  OFACNM        XNNAME           40
     C  N66              MOVEL(P)  FNNAME        XNNAME
     C     OFAAD2        COMP      *BLANKS                                31
     C                   MOVE      OFAST         XXST
     C                   MOVE      OFAZIP        XXZIP
     C                   MOVEL     OFACTY        XXCTY
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXST          XXST13           13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XACSZ            40
     C                   MOVEL(P)  OFACNM        LBACNM
     C                   MOVEL(P)  OFAAD1        LBAAD1
     C                   MOVEL(P)  OFAAD2        LBAAD2
     C                   MOVEL     XACSZ         LBACSZ
      *  If EXISTS use LOCK BOX address instead of Accounting Ofc addr
      *  by Moving LOCK BOX address fields into accunting Ofc addr flds
     C     XXLKBX        IFEQ      'Y'
     C     F2LBNM        IFNE      *BLANKS
     C                   MOVEL(P)  F2LBNM        LBACNM           40
     C                   ENDIF
     C                   MOVEL(P)  F2LAD1        LBAAD1           40
     C                   MOVEL(P)  F2LAD2        LBAAD2           40
     C     F2LCTY        CAT(P)    ',':0         LBACSZ           40
     C     LBACSZ        CAT       F2LST:1       LBACSZ
     C                   MOVE      F2LZIP        XXZIP
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVE      XXZPM2        XXZP2
     C                   CAT       XXZP1:1       LBACSZ
     C     XXZP2         IFNE      *ZEROS
     C                   CAT       '-':0         LBACSZ
     C                   CAT       XXZP2:0       LBACSZ
     C                   ENDIF
     C     F2LAD2        IFEQ      *BLANK
     C                   MOVE      *ON           *IN31
     C                   ELSE
     C                   MOVE      *OFF          *IN31
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  *IN62 IFEQ
     C*
     C     EMKEY         CHAIN     PA301                              61
     C     *IN61         IFEQ      *OFF
     C     A3STAT        ANDEQ     'A'
     C     A3ADR1        ANDNE     *BLANKS
     C     A3CONT        COMP      *BLANKS                                28
     C     A3ADR2        COMP      *BLANKS                                30
     C                   MOVEL     A3ADR1        XXADR1
     C                   MOVEL     A3ADR2        XXADR2
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      A3ST          XXST
     C                   MOVEL     A3ZIP1        XXZIP
     C                   MOVE      A3ZIP2        XXZIP
     C                   MOVEL     A3CTY         XXCTY
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXST          XXST13           13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ
     C                   MOVEL     XXCSZ         XXECSZ
     C                   MOVEL(P)  A3CONT        XXCONT           40
     C*
     C                   ELSE
     C*
      *
      * Get Employer Contact Name
      *
     C                   MOVE      '1'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         EMCONT
      *
     C                   MOVEL(P)  EMCONT        XXCONT           40
     C     EMCONT        COMP      *BLANKS                                28
     C     EMMPFL        IFEQ      'M'
     C     EMMAD2        COMP      *BLANKS                                30
     C                   MOVEL     EMMAD1        XXADR1
     C                   MOVEL     EMMAD2        XXADR2
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      EMMST         XXST
     C                   Z-ADD     EMMZIP        XXZIP
     C                   MOVEL     EMMCTY        XXCTY
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXST          XXST13           13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ
     C                   MOVEL     XXCSZ         XXECSZ
     C                   ELSE
     C*
     C     EMPAD2        COMP      *BLANKS                                30
     C                   MOVEL     EMPAD1        XXADR1
     C                   MOVEL     EMPAD2        XXADR2
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      EMPST         XXST
     C                   Z-ADD     EMPZIP        XXZIP
     C                   MOVEL     EMPCTY        XXCTY
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXST          XXST13           13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ
     C                   MOVEL     XXCSZ         XXECSZ
     C                   ENDIF                                                  EMMPFL IFEQ
     C*
     C                   ENDIF                                                  *IN61 IFEQ
     C                   ELSE
     C                   MOVE      *BLANKS       XXADR1
     C                   MOVE      *BLANKS       XXADR2
     C                   MOVE      *BLANKS       XXECSZ
     C                   END
     C                   MOVE      xxCo#         ##CO#             3
     C                   MOVE      FCFND         ##FND             3
     C                   MOVE      SVEMP#        ##EMP#            9
     C                   MOVE      SVDIV         ##DIV             5
     C                   MOVE      'W'           ##ODR             1
     C                   MOVE      ' '           ##NODR            1
     C                   MOVE      ' '           ##AODR            1
     C                   MOVE      *BLANKS       ##EMN1           50
     C                   MOVE      *BLANKS       ##EMN2           50
     C                   MOVE      *BLANKS       ##EMN3           50
     C                   MOVE      *BLANKS       ##EMN4           50
     C                   MOVE      *BLANKS       ##EMN5           50
     C                   MOVE      *BLANKS       ##EMN6           50
     C                   MOVE      *BLANKS       ##EMN7           50
     C                   CALL      'SRADDR'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##ODR
     C                   PARM                    ##NODR
     C                   PARM                    ##AODR
     C                   PARM                    ##EMN1
     C                   PARM                    ##EMN2
     C                   PARM                    ##EMN3
     C                   PARM                    ##EMN4
     C                   PARM                    ##EMN5
     C                   PARM                    ##EMN6
     C                   PARM                    ##EMN7
     C*
     C                   MOVEA     *BLANKS       EMN
     C                   MOVEA     *BLANKS       ATT
     C                   Z-ADD     8             A                 2 0
     C*
     C                   MOVE      ##EMN1        XXEMN2           50
     C                   MOVE      ##EMN2        XXEMN3           50
     C                   MOVE      ##EMN3        XXEMN4           50
     C                   MOVEL     ##EMN4        XXEMN5           50
     C                   MOVEL     ##EMN5        XXEMN6           50
     C                   MOVEL     ##EMN6        XXEMN7           50
     C                   MOVEL     ##EMN7        XXEMN8           50
     C*
     C     *IN28         IFEQ      *OFF
     C*
     C     A             DOWGT     0
     C     EMN(A)        IFEQ      *BLANKS
     C                   MOVEL     XXCONT        EMN(A)
     C                   MOVEL     'ATTN:'       ATT(A)
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             A
     C                   ENDDO
     C*
     C                   ENDIF                                                  *IN28 IFEQ
     C*
     C*
     C*  CHECK FOR EMPLOYER CANCELLATION DATE
     C*
     C                   Z-ADD     *ZERO         XXCNDT
     C                   MOVE      *OFF          *IN32
     C*
     C                   Z-ADD     1             Y                 1 0
     C     *ZERO         LOOKUP    ED(Y)                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   END
     C     Y             DOWNE     0
     C     X2PODT        COMP      ED(Y)                              60  60
     C  N60              SUB       1             Y
     C  N60              END
     C*
     C     Y             IFNE      *ZERO
     C     CD(Y)         ANDNE     *ZERO
     C     CD(Y)         ANDLT     X2PODT
     c                   eval      xxcndt = dtmdyy(cd(Y))
     C                   MOVE      *ON           *IN32
     C                   END
     C*
     C*  DETERMINE THE BILLING FUND YEAR FOR THE EMPLOYER
     C*
     C     ELKY1         SETGT     PEL01
     C*
     C     *IN60         DOUEQ     *ON
     C     ELKY2         READPE    PEL01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     ELPFDT        IFNE      ELPTDT
     C                   Z-ADD     ELFYR         XXBFYR            3 0
     C                   MOVE      XXBFYR        XXFYR2            2 0
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  ELPFDT <> ELPTDT
     C*
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDDO                                                  *IN60 DOU *ON
     C*
     C     F1KEY         CHAIN     PF101                              60
      *
      * CHECK FOR A CARRIER/STATE REPORTING RECORD
      *
     C                   MOVE      *OFF          *IN33
     C     RPKY          CHAIN     PRP01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     RPCAP#        IFNE      *BLANKS
     C                   MOVE      *ON           *IN33
     C                   ENDIF                                                  RPCAP# IFNE *BLANKS
     C                   ENDIF                                                  *IN60 IFEQ *OFFANKS
      *
     C*  CALCULATE PRIOR BILLED AND PRIOR  PAID
     C*
     C                   Z-ADD     *ZEROS        XXPAID            9 2
     C                   Z-ADD     *ZEROS        XXPTAX            9 2
     C                   Z-ADD     *ZEROS        XXBTAX            9 2
     C                   Z-ADD     *ZEROS        XXCTAX            9 2
     C                   Z-ADD     *ZEROS        XXCMPP           11 2       CLM FEE PRV PAID
     C                   Z-ADD     *ZEROS        XXCLPP           11 2       COL FEE PRV PAID
     C                   Z-ADD     *ZEROS        XXSVYR            3 0
     C*
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     EAFYR         IFNE      XXSVYR
     C                   Z-ADD     EAFYR         XXBFYR
     C*
     C     F1KEY         CHAIN     PF101                              60
     C   60              MOVE      *BLANKS       F1CFTA
     C   60              MOVE      *BLANKS       F1ACFT
     C   60              MOVE      *BLANKS       F1COTA
     C   60              MOVE      *BLANKS       F1ACOT
     C                   ENDIF
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C*
     C*
     C* ACCUMULATE PAID CLAIM FEE RECORDS
     C*
     C     EAARCD        IFEQ      F1CFTA
     C     EAARCD        OREQ      F1ACFT
     C                   ADD       EAAMT         XXCMPP
     C                   ENDIF
     C*
     C* ACCUMULATE PAID COL FEE RECORDS
     C*
     C     EAARCD        IFEQ      F1COTA
     C     EAARCD        OREQ      F1ACOT
     C                   ADD       EAAMT         XXCLPP
     C                   ENDIF
     C*
     C     NEXT1         TAG
     C                   Z-ADD     EAFYR         XXSVYR            3 0
     C     EAKEY         READE     PEA01                                  60
     C                   ENDDO
     C*
     C*
     C* CALCULATE PRIOR BILLED TOTAL, PRIOR PAID TOTAL, AND CURRENT BILLED TOTAL
     C*
     C* PRIOR BALANCE
     C                   EVAL      XPRBAL = XXCMPB  - XXCMPP
     C                   EVAL      XPRTOT = XPRBAL
     C                   EVAL      XPRBAL1= XXCLPB  - XXCLPP
     C                   EVAL      XPRTOT1= XPRBAL1
     C* Current Billed
     C                   EVAL      XXAMT   = XXCMCB
     C                   EVAL      XXCTOT = XXAMT
     C
     C                   EVAL      XXAMT11 = XXCLCB
     C                   EVAL      XXCTOT1= XXAMT11
     C
     C* CALCULATE BALANCE DUE
     C*
     C                   EVAL      XXTOTL = XPRTOT+XXCTOT+XPRTOT1+XXCTOT1
     C*
     C     XXTOTL        IFNE      *ZEROS
     C*
     C                   MOVEL(P)  EMNAME        XXILN1
     C                   MOVEL(P)  XXADR1        XXILN2
     C                   MOVEL(P)  XXADR2        XXILN3
     C                   MOVEL(P)  XXECSZ        XXILN4
     C*
     C                   EXSR      FILLI
     C*
     C                   MOVEL(P)  LBACNM        XXRLN1
     C                   MOVEL(P)  LBAAD1        XXRLN2
     C                   MOVEL(P)  LBAAD2        XXRLN3
     C                   MOVEL(P)  LBACSZ        XXRLN4
     C*
     C                   EXSR      FILLR
     C*
     C                   Z-ADD     FCFND         @XFND
     C                   Z-ADD     SVEMP#        @VEMP#
     C                   Z-ADD     SVDIV         @VDIV
     C                   Z-ADD     XXBFYR        @XBFYR
     C                   Z-ADD     XXTOTL        @XTOTL
     C*
     C                   MOVE      '3'           ##TYPE
     C                   Z-ADD     FCFND         #1FND
     C                   Z-ADD     SVEMP#        #1EMP#
     C                   Z-ADD     SVDIV         #1DIV
     C                   Z-ADD     XXBFYR        ##FYR
     C                   Z-ADD     XXTOTL        XXAMT1
     C*
     C* Check Digit
     C                   MOVEL     XXOCR         XXOCR2           32
     C                   MOVEA     XXOCR2        OCL
     C                   Z-ADD     *ZEROS        XXSUM
     C     1             DO        32            A                 2 0
     C                   MOVE      OCL(A)        XXLN              1 0
     C                   MOVE      WTL(A)        XXRN              1 0
     C     XXLN          MULT      XXRN          XX2N              2 0
     C                   MOVEL     XX2N          XXLN
     C                   MOVE      XX2N          XXRN
     C     XXLN          ADD       XXRN          XXTTL             2 0
     C                   ADD       XXTTL         XXSUM             5 0
     C                   ENDDO
     C     XXSUM         DIV       10            XXWRK             2 0
     C                   MVR                     XXREM             1 0
     C     10            SUB       XXREM         XXCKDG
     C*
     C                   if        not %open(W8006R1O)
     C                   OPEN      W8006R1O
     C                   endif
     C                   WRITE     INVOIC1
     C                   if        %open(W8006R1O)
     C                   CLOSE     W8006R1O
     C                   ENDIF
     C*
     C     QQDFLG        IFEQ      'Y'
     C                   EXSR      DARTS
     C                   ENDIF
     C*
     C                   END
     C*
     C*
     C* SAVE AMOUNT FIELDS FOR SUMMARY INVOICE FOR PEO POLICIES.
     C*
     C                   ADD       XXAMT         SVAMT
     C                   ADD       XXBILL        SVBILL
     C                   ADD       XXPAID        SVPAID
     C                   ADD       XXPTAX        SVPTAX
     C                   ADD       XXBTAX        SVBTAX
     C                   ADD       XXCTAX        SVCTAX
     C                   Z-ADD     FCEMP#        SVEMP#
     C                   Z-ADD     FCDIV         SVDIV
     C                   Z-ADD     *ZEROS        XXAMT
     C                   Z-ADD     *ZEROS        XXBILL
     C                   Z-ADD     *ZEROS        XXCMCB
     C                   Z-ADD     *ZEROS        XXCLCB
     C                   Z-ADD     *ZEROS        XXCMPB
     C                   Z-ADD     *ZEROS        XXCLPB
     C*
     C                   ENDSR                                                  SREBRK
      *
      * =================================================================
     C*
      * =================================================================
     C* SUBROUTINE TO PRINT SUMMARY INVOICE FOR PEO POLICY
      * =================================================================
     C*
     C     SUMINV        BEGSR
     C*
     C                   Z-ADD     1             SVEMP#
     C                   Z-ADD     0             SVDIV
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXCHEC           40
     C*
     C     OFKY          CHAIN     POF01                              62
     C     *IN62         IFEQ      *OFF
     C*
     C*  LOAD PRINT FIELD FOR REMITTANCE CITY/ST/ZIP
     C*
     C                   MOVEL     OFCKNM        XXCHEC
     C                   MOVEL     OFACNM        XNNAME           40
     C     OFAAD2        COMP      *BLANKS                                31
     C                   MOVE      OFAST         XXST
     C                   MOVE      OFAZIP        XXZIP
     C                   MOVEL     OFACTY        XXCTY
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXST          XXST13           13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XACSZ            40
     C                   ENDIF                                                  *IN62 IFEQ
     C*
     C     EMKEY         CHAIN     PA301                              61
     C     *IN61         IFEQ      *OFF
     C     A3STAT        ANDEQ     'A'
     C     A3ADR1        ANDNE     *BLANKS
     C     A3CONT        COMP      *BLANKS                                28
     C     A3ADR2        COMP      *BLANKS                                30
     C                   MOVEL     A3ADR1        XXADR1
     C                   MOVEL     A3ADR2        XXADR2
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      A3ST          XXST
     C                   MOVEL     A3ZIP1        XXZIP
     C                   MOVE      A3ZIP2        XXZIP
     C                   MOVEL     A3CTY         XXCTY
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXST          XXST13           13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ
     C                   MOVEL     XXCSZ         XXECSZ
     C                   MOVEL(P)  A3CONT        XXCONT           40
     C*
     C                   ELSE
      *
      * Get Employer Contact Name
      *
     C                   MOVE      '1'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         EMCONT
      *
     C                   MOVEL(P)  EMCONT        XXCONT           40
     C     EMCONT        COMP      *BLANKS                                28
     C     EMMPFL        IFEQ      'M'
     C     EMMAD2        COMP      *BLANKS                                30
     C                   MOVEL     EMMAD1        XXADR1
     C                   MOVEL     EMMAD2        XXADR2
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      EMMST         XXST
     C                   Z-ADD     EMMZIP        XXZIP
     C                   MOVEL     EMMCTY        XXCTY
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXST          XXST13           13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ
     C                   MOVEL     XXCSZ         XXECSZ
     C*
     C                   ELSE
     C*
     C     EMPAD2        COMP      *BLANKS                                30
     C                   MOVEL     EMPAD1        XXADR1
     C                   MOVEL     EMPAD2        XXADR2
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      EMPST         XXST
     C                   Z-ADD     EMPZIP        XXZIP
     C                   MOVEL     EMPCTY        XXCTY
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXST          XXST13           13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ
     C                   MOVEL     XXCSZ         XXECSZ
     C                   ENDIF                                                  EMMPFL IFEQ
     C*
     C                   ENDIF                                                  *IN61 IFEQ
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       XXADR1
     C                   MOVE      *BLANKS       XXADR2
     C                   MOVE      *BLANKS       XXECSZ
     C                   END
     C                   MOVE      xxCo#         ##CO#             3
     C                   MOVE      FCFND         ##FND             3
     C                   MOVE      SVEMP#        ##EMP#            9
     C                   MOVE      SVDIV         ##DIV             5
     C                   MOVE      'W'           ##ODR             1
     C                   MOVE      ' '           ##NODR            1
     C                   MOVE      ' '           ##AODR            1
     C                   MOVE      *BLANKS       ##EMN1           50
     C                   MOVE      *BLANKS       ##EMN2           50
     C                   MOVE      *BLANKS       ##EMN3           50
     C                   MOVE      *BLANKS       ##EMN4           50
     C                   MOVE      *BLANKS       ##EMN5           50
     C                   MOVE      *BLANKS       ##EMN6           50
     C                   MOVE      *BLANKS       ##EMN7           50
     C                   CALL      'SRADDR'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##ODR
     C                   PARM                    ##NODR
     C                   PARM                    ##AODR
     C                   PARM                    ##EMN1
     C                   PARM                    ##EMN2
     C                   PARM                    ##EMN3
     C                   PARM                    ##EMN4
     C                   PARM                    ##EMN5
     C                   PARM                    ##EMN6
     C                   PARM                    ##EMN7
     C*
     C                   MOVEA     *BLANKS       EMN
     C                   MOVEA     *BLANKS       ATT
     C                   Z-ADD     8             A                 2 0
     C*
     C                   MOVE      ##EMN1        XXEMN2           50
     C                   MOVE      ##EMN2        XXEMN3           50
     C                   MOVE      ##EMN3        XXEMN4           50
     C                   MOVEL     ##EMN4        XXEMN5           50
     C                   MOVEL     ##EMN5        XXEMN6           50
     C                   MOVEL     ##EMN6        XXEMN7           50
     C                   MOVEL     ##EMN7        XXEMN8           50
     C*
     C     *IN28         IFEQ      *OFF
     C*
     C     A             DOWGT     0
     C     EMN(A)        IFEQ      *BLANKS
     C                   MOVEL     XXCONT        EMN(A)
     C                   MOVEL     'ATTN:'       ATT(A)
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             A
     C                   ENDDO
     C*
     C                   ENDIF                                                  *IN28 IFEQ
     C*
     C*
     C*  CHECK FOR EMPLOYER CANCELLATION DATE
     C*
     C                   Z-ADD     *ZERO         XXCNDT
     C                   MOVE      *OFF          *IN32
     C*
     C                   Z-ADD     1             Y                 1 0
     C     *ZERO         LOOKUP    ED(Y)                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   END
     C     Y             DOWNE     0
     C     X2PODT        COMP      ED(Y)                              60  60
     C  N60              SUB       1             Y
     C  N60              END
     C*
     C     Y             IFNE      *ZERO
     C     CD(Y)         ANDNE     *ZERO
     C     CD(Y)         ANDLT     X2PODT
     c                   eval      xxcndt = dtmdyy(cd(y))
     C                   MOVE      *ON           *IN32
     C                   END
     C*
     C*  DETERMINE THE BILLING FUND YEAR FOR THE EMPLOYER
     C*
     C     ELKY1         SETGT     PEL01
     C*
     C     *IN60         DOUEQ     *ON
     C     ELKY2         READPE    PEL01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     ELPFDT        IFNE      ELPTDT
     C                   Z-ADD     ELFYR         XXBFYR            3 0
     C                   MOVE      XXBFYR        XXFYR2            2 0
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  ELPFDT <> ELPTDT
     C*
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDDO                                                  *IN60 DOU *ON
     C*
     C     F1KEY         CHAIN     PF101                              60
      *
      * CHECK FOR A CARRIER/STATE REPORTING RECORD
      *
     C                   MOVE      *OFF          *IN33
     C     RPKY          CHAIN     PRP01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     RPCAP#        IFNE      *BLANKS
     C                   MOVE      *ON           *IN33
     C                   ENDIF                                                  RPCAP# IFNE *BLANKS
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFFANKS
     C*
     C*  GET AGENT NAME VIA AGENT HISTORY AND AGENT MASTER FILES
     C*
     C     AHKY          SETGT     PAH01
     C     AHKY          READPE    PAH01                                  50
     C     *IN50         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C     AHKY          READPE    PAH01                                  50
     C                   END
     C*
     C     *IN50         IFEQ      '0'
     C                   MOVEL     AHAGT#        XXAGT#
     C*
     C     AAKY          CHAIN     PAA01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     AAFEIN        XXFEIN
     C                   MOVEL     AAMOD         XXMOD
     C                   ELSE
     C                   MOVE      *ZEROS        XXFEIN
     C                   MOVE      *ZEROS        XXMOD
     C                   END
     C*
     C     AGKY          CHAIN     PAG01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVE      *BLANKS       XXZIP4
     C     AGMAD1        IFNE      *BLANKS
     C                   MOVEL     AGMAD1        XXAAD1           40
     C                   MOVEL     AGMCTY        XXSHRT           19
     C                   MOVEL     XXSHRT        XXLAST           56
     C                   MOVEL     AGMST         XXSTZP           13
     C                   MOVE      AGMZIP        XXSTZP
     C                   ELSE
     C                   MOVEL     AGPAD1        XXAAD1
     C                   MOVEL     AGPCTY        XXSHRT
     C                   MOVEL     XXSHRT        XXLAST
     C                   MOVEL     AGPST         XXSTZP
     C                   MOVE      AGPZIP        XXSTZP
     C                   ENDIF
     C                   MOVE      XXSTZP        XXZIP4            4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5               5
     C     AGMAD1        IFNE      *BLANKS
     C                   MOVEL     AGMZIP        XXST10           10
     C                   ELSE
     C                   MOVEL     AGPZIP        XXST10           10
     C                   ENDIF
     C                   MOVE      XX5           XXST10           10
     C                   MOVE      XXST10        XXSTZP
     C                   ENDIF
     C*
     C                   MOVEL     XXSTZP        XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXACSZ           30
     C                   ELSE
     C                   MOVE      *BLANKS       AGNAME
     C                   MOVE      *BLANKS       XXAAD1
     C                   MOVE      *BLANKS       XXACSZ
     C                   MOVE      *ZEROS        AGARE1
     C                   MOVE      *ZEROS        AGPHN1
     C                   END
     C*
     C                   ELSE
     C                   MOVE      *BLANK        AHAGT#
     C                   MOVE      *BLANK        AGNAME
     C                   MOVE      *BLANKS       XXAAD1
     C                   MOVE      *BLANKS       XXACSZ
     C                   MOVE      *ZEROS        AGARE1
     C                   MOVE      *ZEROS        AGPHN1
     C                   END
     C*
     C     AGPHN1        IFEQ      *ZEROS
     C                   MOVE      *OFF          *IN69
     C                   ELSE
     C                   MOVE      *ON           *IN69
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       XXRDPY            2
     C*
     C* RETRIEVE PAYMENT BATCH#
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      'R'
     C     TTTYPE        ANDEQ     'Q'
     C                   MOVEL     TTKEY         XXRDPY
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   Z-ADD     0             XPBAT#            9 0
     C*
     C     FNKEY         SETGT     PEA05
     C     FNKEY         READPE    PEA05                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C* * *$AARCD        ANDNE     XXRDPY
     C     $AACHF        andne     'F'
     C     $APODT        ANDNE     X2PODT
     C     FNKEY         READPE    PEA05                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EABAT#        XPBAT#
     C                   ENDIF
     C*
     C                   Z-ADD     SVAMT         XXAMT
     C                   Z-ADD     SVBILL        XXBILL
     C                   Z-ADD     SVPAID        XXPAID
     C                   Z-ADD     SVPTAX        XXPTAX
     C                   Z-ADD     SVBTAX        XXBTAX
     C                   Z-ADD     SVCTAX        XXCTAX
     C*
     C* CALCULATE PRIOR BILLED TOTAL, PRIOR PAID TOTAL, AND CURRENT BILLED TOTAL
     C*
     C     XXBILL        ADD       XXBTAX        XXBTOT
     C     XXPAID        ADD       XXPTAX        XXPTOT
     C     XXAMT         ADD       XXCTAX        XXCTOT
     C*
     C* CALCULATE BALANCE DUE
     C*
     C     XXBTOT        ADD       XXCTOT        XXTOTL
     C                   ADD       XXPTOT        XXTOTL
     C*
     C     XXTOTL        IFNE      *ZEROS
     C*
     C* PRINT NEW FORMAT PREMIUM NOTICE.
      *
     C                   MOVE      *ON           *IN63
     C                   OPEN      W8006R1O
     C                   EXSR      NEWF
     C                   CLOSE     W8006R1O
     C                   END
     C*
     C                   ENDSR                                                  SUMINV
      *
     C*================================================================
     C* Adjust for blank 'Insured' address lines
     C*================================================================
     C     FILLI         BEGSR
     C*
     C                   DO        3
     C*
     C     XXILN1        IFEQ      *BLANKS
     C                   MOVEL(P)  XXILN2        XXILN1
     C                   MOVEL(P)  XXILN3        XXILN2
     C                   MOVEL(P)  XXILN4        XXILN3
     C                   CLEAR                   XXILN4
     C                   ENDIF
     C*
     C     XXILN2        IFEQ      *BLANKS
     C                   MOVEL(P)  XXILN3        XXILN2
     C                   MOVEL(P)  XXILN4        XXILN3
     C                   CLEAR                   XXILN4
     C                   ENDIF
     C*
     C     XXILN3        IFEQ      *BLANKS
     C                   MOVEL(P)  XXILN4        XXILN3
     C                   CLEAR                   XXILN4
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* Adjust for blank 'Remit to' address lines
     C*================================================================
     C     FILLR         BEGSR
     C*
     C                   DO        3
     C*
     C     XXRLN1        IFEQ      *BLANKS
     C                   MOVEL(P)  XXRLN2        XXRLN1
     C                   MOVEL(P)  XXRLN3        XXRLN2
     C                   MOVEL(P)  XXRLN4        XXRLN3
     C                   CLEAR                   XXRLN4
     C                   ENDIF
     C*
     C     XXRLN2        IFEQ      *BLANKS
     C                   MOVEL(P)  XXRLN3        XXRLN2
     C                   MOVEL(P)  XXRLN4        XXRLN3
     C                   CLEAR                   XXRLN4
     C                   ENDIF
     C*
     C     XXRLN3        IFEQ      *BLANKS
     C                   MOVEL(P)  XXRLN4        XXRLN3
     C                   CLEAR                   XXRLN4
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C*
     C* NEW FORMAT OUTPUT SUBROUTINE.
     C*
     C     NEWF          BEGSR
     C*
     C* POLICY INFO SECTION
     C*
     C     RPKY          CHAIN     PEL03                              60
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        XQST
     C                   ELSE
     C                   MOVE      *BLANKS       XQST
     C                   ENDIF
     C*
     C                   MOVEL(P)  LOSSH         XQLOSH
     C*
     C                   MOVEL(P)  DEDHDR        XXHDR
     C                   MOVEL(P)  SUMHDR        X1HDR
     C*
     C                   MOVEL(P)  OFAAD1        XXAAD1
     C                   MOVEL(P)  OFAAD2        XXAAD2
     C*
     C     OFAAD2        IFNE      *BLANKS
     C                   MOVEL(P)  XACSZ         XOACSZ
     C                   ELSE
     C                   MOVEL(P)  XACSZ         XXAAD2
     C                   ENDIF
     C*
     C     1             DO        8             A
     C     ATT(A)        IFNE      *BLANKS
     C                   MOVEL(P)  'Attn:'       ATT(A)
     C                   ENDIF
     C                   ENDDO
     C*
     C                   Z-ADD     XXPRDT        XQDT
     C*
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MN(X)         XQPRDT
     C                   CAT       XXDAY:1       XQPRDT
     C                   CAT       ',':0         XQPRDT
     C                   CAT       XXYR:1        XQPRDT
     C*
     C                   MOVEL(P)  'Division'    XQDIVH           20
     C                   CAT       ':':0         XQDIVH
     C*
     C                   MOVEL(P)  'Rating S'    XQRSTH           20
     C                   CAT       'tate:':0     XQRSTH
     C*
     C*
     C                   MOVEL(P)  'Policy'      XQPROF           20
     C                   CAT       'For:':1      XQPROF
     C*
     C                   MOVEL(P)  'Print'       XQPROD           20
     C                   CAT       'Date:':1     XQPROD
     C*
     C                   MOVEL(P)  'For Peri'    XQPERD           20
     C                   CAT       'od:':0       XQPERD
     c                   eval      xqdt = dtmdyy(elpfdt)
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MN(X)         XQPFDT           20
     C                   CAT       XXDAY:1       XQPFDT
     C                   CAT       ',':0         XQPFDT
     C                   CAT       XXYR:1        XQPFDT
     c                   eval      xqdt = dtmdyy(elptdt)
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MN(X)         XQPTDT           20
     C                   CAT       XXDAY:1       XQPTDT
     C                   CAT       ',':0         XQPTDT
     C                   CAT       XXYR:1        XQPTDT
     C*
     C                   MOVEL(P)  XQPFDT        XQPOLP           45
     C                   CAT       ' to':1       XQPOLP
     C                   CAT       XQPTDT:1      XQPOLP
     C*
     C                   MOVEL(P)  CARPOL        XQCAPH
     C*
     C     *IN33         IFEQ      *ON
     C                   MOVEL     FILE          @@PLCY
     C                   ELSE
     C                   MOVEL     POL           @1PLCY
     C                   ENDIF
     C*
     C                   MOVE      FCFND         XAFND             3
     C                   MOVE      SVEMP#        XAEMP             9
     C                   MOVE      XXBFYR        XAFYR             3
     C*
     C                   MOVEL(P)  XAFND         XXPLCY
     C                   CAT       XAEMP:0       XXPLCY
     C                   CAT       XAFYR:0       XXPLCY
     C*
     C                   MOVEL(P)  BREAKH        XXBRK
     C*
     C                   MOVEL(P)  SEEH          XXSEE
     C*
     C                   MOVEL(P)  BATH          XPBATH
     C*
     C                   MOVEL(P)  CANDH         XQCNDH
     C*
     C* AGENT INFO SECTION
     C*
     C                   MOVE      *BLANKS       XXAGHD
     C     EXAHDR        CAT       ':':0         XXAGHD
     C*
     C                   MOVE      *BLANKS       XXAGPH
     C*
     C     AGPHN1        IFNE      *ZERO
     C                   MOVE      AGARE1        X1AREA            3
     C                   MOVEL     AGPHN1        X1AGN1            3
     C                   MOVE      AGPHN1        X1AGN2            4
     C*
     C                   MOVEL     '('           XXAGPH
     C                   CAT       X1AREA:0      XXAGPH
     C                   CAT       ')':0         XXAGPH
     C                   CAT       X1AGN1:1      XXAGPH
     C                   CAT       '-':1         XXAGPH
     C                   CAT       X1AGN2:1      XXAGPH
     C                   ENDIF                                                  AGPHN1 IFNE
     C*
     C* INFO BOX SECTION
     C*
     C                   MOVE      *BLANKS       XXCHKH
     C                   MOVE      *BLANKS       XXREMH
     C                   MOVE      *BLANKS       XXSERH
     C                   MOVE      *BLANKS       XXDIRH
     C                   MOVE      *BLANKS       XXOFAD
     C                   MOVE      *BLANKS       XXSERN
     C                   MOVE      *BLANKS       XXDIRN
     C*
     C                   MOVEL     'Please'      XXCHKH
     C                   CAT       'make':1      XXCHKH
     C                   CAT       'checks':1    XXCHKH
     C                   CAT       'payabl':1    XXCHKH
     C                   CAT       'e to:':0     XXCHKH
     C*
     C                   MOVEL     'Remit'       XXREMH
     C                   CAT       'to:':1       XXREMH
     C*
     C                   MOVEL     'Serviced'    XXSERH
     C                   CAT       'by:':1       XXSERH
     C*
     C                   MOVEL     'Direct I'    XXDIRH
     C                   CAT       'nquire':0    XXDIRH
     C                   CAT       's to:':0     XXDIRH
     C*
     C                   MOVEL     LBAAD1        XXOFAD
     C                   CAT       LBAAD2:1      XXOFAD
     C                   CAT       ',':0         XXOFAD
     C                   CAT       LBACSZ:1      XXOFAD
     C*
     C                   MOVEL     LBACNM        XXSERN
     C*
     C                   MOVEL     OFPRNM        XXDIRN
     C*
     C     XXPHN1        IFNE      *ZERO
     C                   MOVE      OFAARA        X1AREA            3
     C                   MOVE      XXPHN1        X1AGN1            3
     C                   MOVE      XXPHN2        X1AGN2            4
     C*
     C                   CAT       '(':1         XXDIRN
     C                   CAT       X1AREA:0      XXDIRN
     C                   CAT       ')':0         XXDIRN
     C                   CAT       X1AGN1:1      XXDIRN
     C                   CAT       '-':1         XXDIRN
     C                   CAT       X1AGN2:1      XXDIRN
     C                   ENDIF                                                  XXPHN1 IFNE
     C*
     C                   SELECT
     C     XXCLT#        WHENEQ    '046'
     C     F2BLOG        ANDEQ     'TIMBER'
     C                   Z-ADD     0.5           LOGHRZ
     C                   Z-ADD     6.3           LOGVRT
     C                   OTHER
     C                   Z-ADD     0.6           LOGHRZ
     C                   Z-ADD     5.2           LOGVRT
     C                   ENDSL
     C*
     C     CLTID         IFEQ      '025'
     C                   Z-ADD     5.36          STMHV
     C     'State'       CAT(P)    'ment #':0    XQSTMH
     C* * *                MOVELEANINV    XQSTMD
     C                   MOVEL     EAINV#        XQSTMD
     C                   ENDIF
     C*
     C                   WRITE     QHEAD
     C                   WRITE     QHEAD1
     C                   WRITE     QLINE1
     C                   WRITE     QHEAD2
     C                   WRITE     QHEAD3
     C                   WRITE     QDET
     C  N63              WRITE     QBREAK
     C   63              WRITE     QAGT
     C                   WRITE     QINFO
     C                   WRITE     QBOT
     C*
     C                   ENDSR                                                  NEWF SUBR
      *====================================================================
      *  DARTS SUBROUTINE  -  CREATE DARTS RECORD
      *====================================================================
     C     DARTS         BEGSR
      *
      *
      *
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   MOVEL(P)  *BLANKS       Q#DCID           12
      *
      *  If office is installed, resolve output for DARTS
      *
     C     SYOFIF        IFEQ      'Y'
     C     XXDFLG        ANDEQ     'Y'
      *
     C                   MOVE      FCFND         ##FND
      *
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       ##FLR            80
     C                   PARM      *BLANKS       ##DCID           12
     C                   PARM      'FE'          ##PRE             2
     C                   PARM      QQCO#         ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM      'P'           ##RTCD            1
      *
     C                   MOVE      SVEMP#        ##EMP#
      *
     C                   CALL      'SRESOLVE'
     C                   PARM      'W8006R1O'    ##SplF           10
     C                   PARM                    ##Flr            80
     C                   PARM                    ##DcId           12
     C                   PARM      qqOutQ        ##OutQ           10
      *
     C                   MOVEL(P)  ##FLR         Q#FOLD           80
     C                   MOVEL(P)  ##DCID        Q#DCID           12
     C                   ENDIF
      *
      *
      *   ESTABLISH KEY
      *
     C                   MOVE      *BLANKS       ZZKEY
     C                   MOVE      xxCo#         ZZCO#
     C                   MOVE      FCFND         ZZFND
     C                   MOVE      SVEMP#        ZZEMP#
     C                   MOVE      SVDIV         ZZDIV
     C*
     C     XXDRMK        IFNE      ' '
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  ZZKEY         Q#KEY            40
     C     CLTID         IFEQ      '071'
     C                   MOVEL     D71           DACOMM          255
     C                   ELSE
     C                   MOVEL     DA            DACOMM          255
     C                   ENDIF
     C                   MOVEL(P)  RPDATE        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK           10
     C                   CALL      CHGNUM        CHGPL
     C                   CAT       XXNUM:1       DACOMM
     C                   CAT       DA2:1         DACOMM
     C                   MOVEL     RSDAT2        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK           10
     C                   CALL      CHGNUM        CHGPL
     C                   CAT       XXNUM:1       DACOMM
     C                   CAT       '.':0         DACOMM
     C                   CAT       'BY:':1       DACOMM
     C                   CAT       UPUSNM:1      DACOMM
     C                   CAT       '.':0         DACOMM
     C                   CAT       'AMOUNT':1    DACOMM
     C                   CAT       'DUE:':1      DACOMM
     C                   MOVEL(P)  XXTOTL        XXNUM
     C                   MOVEL     'X,XXX.XX'    EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   CAT       XXNUM:0       DACOMM
     C                   CAT       '.':0         DACOMM
     C                   MOVEL(P)  DACOMM        Q#COMM          256
     C                   MOVE      *BLANKS       Q#COM2           19
     c                   eval      Q#cdat = %char(dtCheck(dsdate))
     C                   MOVEL(P)  'W8006R1'     Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  'N'           Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVEL(P)  XXDRMK        Q#RMCD            5
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  'O'           Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
     C                   MOVE      *BLANKS       DACOMM
      *
     C                   ENDIF
      *
     C                   ENDSR
      *
** WTL {Weight}
3713713713713713713713713713713713713713713713
** MN
January
February
March
April
May
June
July
August
September
October
November
December
