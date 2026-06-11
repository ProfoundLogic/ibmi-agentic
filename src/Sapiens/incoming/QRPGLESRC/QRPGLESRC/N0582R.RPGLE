     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE N0582R - PROVIDER/DISCOUNT INFORMATION - SCREEN PROGRAM
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  06/09/20  C:212  Project : SY420
     H*                   Mask SSN
     H*
     H*  06/10/19  C:180  Support: 80032
     H*                   Fix tab names.
     H*
     H*  06/05/19  C:180  Project: MC432
     H*                   Replace pgm N91D7R with FPINETDISC.
     H*
     H*  08/12/10  C:251  Project: 114MWS
     H*                   Create 'DR','DS' Discount
     H*                   1. Add SCR13 for 'DR' code output
     H*                   1. Add SCR14 for 'DS' code output
     H*
     H*  12/14/09  C:227  PROJECT: MC337
     H*                   CHANGED LOGIC FOR DISCOUNT TYPE DM
     H*
     H*  12/04/09  C:198  SUPPORT: 70760
     H*                   CHANGED FILE FDCALB TO FDCALF TO AVOID
     H*                   NULLL WHEN TAKING 7 OPTION ON A BILL.
     H*
     H*  03/03/09  C:212  SUPPORT: 65839
     H*                   ADD ADDITIONAL PARAMETER WHILE CALLING
     H*                   FA747R
     H*
     H*  04/11/07  C:62   REQ/PROJ: 25MOD
     H*                   ADD SCR12 FOR DISCOUNT TYPE DM.
     H*
     H*  10/03/06  C:62   REQ/PROJ: 71WHI
     H*                   ADD SCR08 FOR DISCOUNT TYPE D9.
     H*                   BACK TO BASE
     H*
     H*  07/23/99 C:31  Project: NW010
     H*                 WC Summary File
     H*                 1. Add F8=Network Info
     H*                 2. Replace FADCP with FAEOP for provider bills
     H*
     H*  05/18/99 C:31  Project: SY031
     H*                 1999 Users conference
     H*                 1. Compile only
     H*
     H*  04/08/99 C:56  Support: MC095 #17568
     H*                 Add CO#/SUB#/FND as parms passed to program.
     H*                 Use these parms instead of user parms.
     H*
     H*  03/09/99 C:31  Support: 17000
     H*                 N0582R call issue
     H*                 1. Remove test for RTCD 'P'
     H*
     H*  02/19/99 C:31  Project: 95MAR
     H*                 DC discount changes
     H*                 1. Compile only
     H*
     H*  08/20/98 C:31  Project: 48MBR
     H*                 DB Discount Alogrithm
     H*                 1. 'DB' Code support added
     H*
     H*  05/29/98 C:31  Project: 48MAY
     H*                 Create 'DC' Discount
     H*                 1. Add SCR09 for 'DC' code output
     H*
     H*  05/21/98 C:31  Project: 51MCT
     H*                 CorVel Network
     H*                 1. Add SCR11 for 'DF' code output
     H*
     H*  04/30/98 C:31  Project: 51MCM
     H*                 P.E.N Network
     H*                 1. Add SCR10 for 'DD' code output
     H*
     H*  03/06/98 C:31  Project: MC145
     H*                 Yr 2000, RISC Developemnt
     H*                 1. Changed call to F0031R1 to F0032R01.
     H*                 2. If S4ENDT = *ZEROS, then make it 9999999.
     H*
     H*  02/18/98 C:62  Project: MC159
     H*                 ** CHANGED NMPHL & NMHSL WITH FMS4L2/L3.
     H*
     H*  01/15/98 C:31  Project: 60MBS
     H*                 Anthem Network
     H*                 1. Add 'DE' support
     H*
     H*  12/31/96  C:30   Project:  87MBB
     H*            F6 call changed from F0031R to F0031R1.  API
     H*            FA006R added as requirement for F0031R1.
     H*
     H*  03/07/95  C:20   REQ/PROJ: NW007
     H*                   1. ADD SCR07 FOR DISCOUNT TYPE 7.  ADD F5=DE
     H*                      TAIL INQUIRY INTO TYPE 7 CARVE OUT PGM
     H*
     H*  04/26/94  C:20   REQ/PROJ: 128/FS127
     H*                   1. ADD F6=PROV MASTER INQUIRY TO ALLOW USER
     H*                      TO  GO TO PROV MASTER IN INQUIRY MODE SO
     H*                      THEY CAN VIEW THE PROV INFORMATION
     H*
     H*  03/22/94  C:20   REQ/PROJ: 98/NW001
     H*                   1. CHANGED CODE TO CORRECTLY GET DISCOUNT
     H*                      INFORMATION FOR EACH PROVIDER CONTRACT
     H*                      DATE RANGE.
     H*
     H*****************************************************************
     H BNDDIR ('FTBNDDIR')
     FN0582F    CF   E             WORKSTN USROPN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     F*** BEG DEL *** 04/08/99 ****************************************
     F**********SMUPL   IF  E           K        DISK
     F*** END DEL *** 04/08/99 ****************************************
     F*** BEGIN CHG *** 07/23/99 **************************************
     F********FDCAL1  UF  E           K        DISK
     F*** BEGIN ADD *** 04/11/07 **************************************
     F**********FDCALB  IF  E           K        DISK
     F*** BEG CHG *** 12/04/09 ****************************************
     F**********FDCALB  IF  E           K        DISK         KINFDS DBFDS
     FFDCALF    IF   E           K DISK    INFDS(DBFDS)
     F*** END CHG *** 12/04/09 ****************************************
     F*** END   ADD *** 04/11/07 **************************************
     F*** END CHG *** 07/23/99 ****************************************
     FFMCML1    IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     F*** BEGIN CHG *** 02/18/98 **************************************
     F********** NMPHL   IF  E           K        DISK
     F********** NMHSL   IF  E           K        DISK
     FFMS4L2    IF   E           K DISK
     FFMS4L3    IF   E           K DISK
     F                                     RENAME(PS401:PS402)
     F*** END   CHG *** 02/18/98 **************************************
     FNMBDLD    IF   E           K DISK
     F*** BEGIN CHG *** 07/23/99 **************************************
     F*** BEGIN ADD *** 03/22/94 ***********************************************
     FFADCL     IF   E           K DISK    USROPN
     FFAEOL4    IF   E           K DISK    USROPN
     F*** END ADD *** 03/22/94 *************************************************
     F*** END CHG *** 07/23/99 ****************************************
     F*** BEGIN ADD *** 04/11/07 **************************************
     FNDCOL3    IF   E           K DISK
     F                                     RENAME(PCO01:PCO02)
     F*** END   ADD *** 04/11/07 **************************************
     F*

     D*** BEGIN ADD *** 04/26/94 ***********************************************
     D PTL             S             40    DIM(1) CTDATA PERRCD(1)              CALLED PGM TITLES
     D*** END ADD *** 04/26/94 *************************************************
     D***** Beg Add *** 06/09/20 *******************************************
     D kkSS#           s              9    inz                                  CALLED PGM TITLES
     D***** End Add *** 06/09/20 *******************************************
     D*** BEGIN ADD *** 04/11/07 **************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*** END   ADD *** 04/11/07 **************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*** BEGIN ADD *** 10/03/06 **************************************
     D                 DS
     D  DSCOM2                 1     60
     D  DSCOM                  1     50
     D  DSSRCE                51     51
     D  DSLESS                52     52
     D  DSEXTR                53     60
     D*** END   ADD *** 10/03/06 **************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** Begin Add *** 06/05/19 **************************************
     d networkDisc     pr
     d                                2    const
     d                               40    const
     d                                1    const
     d                                3    const
     d                                8    const
     d                                9    const options(*nopass)
     d                                5    const options(*nopass)
      *
     d QQefdt          S              8A   INZ
      *** End   Add *** 06/05/19 **************************************

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
     C*
     C*   KEY LIST FOR THE CASE DETAIL FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCSEQ#
     C*** BEGIN ADD *** 04/11/07 **************************************
     C*
     C* Carve-out transaction file
     C*
     C     COKY1         KLIST
     C                   KFLD                    SCCODE
     C                   KFLD                    BDEFDT
     C                   KFLD                    KK10A1
     C*
     C     COKY2         KLIST
     C                   KFLD                    SCCODE
     C                   KFLD                    BDEFDT
     C*** END   ADD *** 04/11/07 **************************************
     C*** BEGIN ADD *** 03/22/94 ***********************************************
     C*
     C*   KEY LIST FOR THE DIAGNOSIS TEMP FILE
     C*
     C     DCKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C                   KFLD                    CADCS#
     C*** BEGIN ADD *** 07/23/99 **************************************
     C     EOKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    CAEOB#
     C*** END ADD *** 07/23/99 ****************************************
     C*** END ADD *** 03/22/94 *************************************************
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    SCCO#
     C*
     C*   KEY LIST FOR SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB
     C*
     C*  USER PARAMETER FILE
     C*
     C*** BEG DEL *** 04/08/99 ****************************************
     C********** UPKEY     KLIST
     C**********           KFLD           Q1USER
     C*** END DEL *** 04/08/99 ****************************************
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB
     C                   KFLD                    SCFND
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C***** Beg Chg *** 06/09/20 *******************************************
     C                   KFLD                    kkSS#
     C***                KFLD                    SCSS#
     C***** End Chg *** 06/09/20 *******************************************
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*** BEGIN DEL *** 02/18/98 **************************************
     C*
     C*  PHYSICIAN MASTER FILE
     C*
     C********** PHKEY     KLIST
     C**********           KFLD           SCFEIN
     C**********           KFLD           SCMOD
     C*** BEGIN ADD *** 03/22/94 ***********************************************
     C**********           KFLD           XXNWCD
     C**********           KFLD           XXFDAT
     C*** END ADD *** 03/22/94 *************************************************
     C*
     C*  HOSPITAL MASTER FILE
     C*
     C********** HSKEY     KLIST
     C**********           KFLD           SCFEIN
     C**********           KFLD           SCMOD
     C*** BEGIN ADD *** 03/22/94 ***********************************************
     C**********           KFLD           XXNWCD
     C**********           KFLD           XXFDAT
     C*** END ADD *** 03/22/94 *************************************************
     C*** END   DEL *** 02/18/98 **************************************
     C*** BEGIN ADD *** 02/18/98 **************************************
     C*
     C*  HOSPITAL MASTER FILE
     C*
     C     S4KEY         KLIST
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C                   KFLD                    XXNWCD
     C                   KFLD                    XXFDAT
     C*** END   ADD *** 02/18/98 **************************************
     C*
     C*  BILLING CODE TABLE FILE
     C*
     C     BDKEY         KLIST
     C                   KFLD                    SCCODE
     C                   KFLD                    XXFDAT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQSEQ#            5 0
     C                   PARM                    QQRTCD            1
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQSUB             5 0
     C                   PARM                    QQFND             3 0
     C*** END ADD *** 04/08/99 ****************************************
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
     C*** BEGIN DEL *** 03/09/99 **************************************
     C********   QQRTCD    CABEQ'P'       TERM             PRE-OPEN FILES
     C*** END DEL *** 03/09/99 ****************************************
     C*
     C                   OPEN      N0582F
     C                   MOVE      '1'           *IN99
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      QQCASE        SCCASE
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQFYR         SCYR
     C                   MOVE      QQSEQ#        SCSEQ#
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   Z-ADD     QQCO#         SCCO#             3 0          NOT ON SCREEN
     C                   Z-ADD     QQSUB         SCSUB
     C                   Z-ADD     QQFND         SCFND
     C*** END ADD *** 04/08/99 ****************************************
     C     *LIKE         DEFINE    CAFYR         XXFYR
     C     *LIKE         DEFINE    CAFDAT        XXFDAT
     C*** BEGIN ADD *** 04/11/07 **************************************
     C     *LIKE         DEFINE    BDCODE        KKCODE
     C     *LIKE         DEFINE    BDEFDT        KKEFDT
     C     *LIKE         DEFINE    CO10A1        KK10A1
     C*** END   ADD *** 04/11/07 **************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C*** BEG DEL *** 04/08/99 ****************************************
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C**********           MOVE *ZERO     UPCO#
     C**********           MOVE *ZERO     UPFND
     C**********           MOVE *ZERO     UPSUB#
     C**********           END
     C*** END DEL *** 04/08/99 ****************************************
     C*
     C*  GET COMPANY NAME
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCCONM
     C                   ELSE
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   END
     C*
     C* GET SUBSCRIBER NAME
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         SCSUB
     C                   MOVE      *BLANK        SCSBNM
     C                   ELSE
     C**********           MOVELUPSUB#    SCSUB
     C                   MOVEL     SMNAME        SCSBNM
     C                   END
     C*
     C* GET GROUP NAME
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         SCFND
     C                   MOVE      *BLANK        SCFNNM
     C                   ELSE
     C**********           MOVELUPFND     SCFND
     C                   MOVEL     FNNAME        SCFNNM
     C                   END
     C*
     C*   GET CLAIMENTS SS# AND NAME
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      CMSS#         SCSS#
      /free
        kkSS# = CMss#;
        scSS# = MaskData('SSN':'CMss#':CMss#);
      /end-free
     C***** End Chg *** 06/09/20 *******************************************
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCCNAM
     C                   ELSE
     C                   MOVEL     *BLANKS       SCCNAM
     C                   END
     C*** BEGIN CHG *** 07/23/99 **************************************
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C     ##PRVB        IFEQ      'Y'
     C                   OPEN      FAEOL4
     C                   ELSE                                                   ##PRVB = Y
     C                   OPEN      FADCL
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 07/23/99 ****************************************
     C*
     C*   CHAIN OUT TO THE CASE DETAIL FILE AND GET CHOOSEN RECORD
     C*
     C     CAKEY         CHAIN     PCA01                              50
     C     *IN50         IFEQ      '0'
     C*** BEGIN DEL *** 07/23/99 **************************************
     C********             EXCPTRLSCAE
     C*** END DEL *** 07/23/99 ****************************************
     C                   MOVE      CAFEIN        SCFEIN
     C                   MOVE      CAMOD         SCMOD
     C                   MOVE      CAFDAT        XXFDAT
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFDAT
     C*** BEGIN ADD *** 03/22/94 ***********************************************
     C*** BEGIN CHG *** 07/23/99 **************************************
     C     ##PRVB        IFEQ      'Y'
     C     EOKY          CHAIN     PEO01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  EONTWK        XXNWCD            5
     C                   ELSE
     C                   MOVE      *BLANKS       XXNWCD
     C                   END                                                    END *IN60 EQ '0'
     C                   ELSE                                                   ##PRVB = Y
     C     DCKEY         CHAIN     PDC01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             T                 2 0
     C     5             SUBST     DC10A1:T      XXNWCD            5    65
     C*** END CHG *** 07/23/99 ****************************************
     C                   ELSE
     C                   MOVE      *BLANKS       XXNWCD
     C                   END                                                    END *IN60 EQ '0'
     C*** END ADD *** 03/22/94 *************************************************
     C*** BEGIN ADD *** 07/23/99 **************************************
     C                   ENDIF                                                  ##PRVB = Y
     C*** END ADD *** 07/23/99 ****************************************
     C*
     C*
     C*   GET PROVIDER INFORMATION
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PDNAME        QQLAST           56
     C                   MOVEL     PDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCPNAM
     C                   MOVEL     PDADR1        SCADR1
     C                   MOVEL     PDCTY         SCCTY
     C                   MOVEL     PDST          SCST
     C                   MOVEL     PDZIP         SCZP1
     C                   ELSE
     C                   MOVEL     *BLANKS       SCCNAM
     C                   MOVEL     *BLANKS       SCADR1
     C                   MOVEL     *BLANKS       SCADR2
     C                   MOVEL     *BLANKS       SCCTY
     C                   MOVEL     *BLANKS       SCST
     C                   MOVE      *ZEROS        SCZP1
     C                   END
     C*
     C*  GET THE EFFECTIVE DATE FOR PROVIDER
     C*
     C     CAPRTY        IFEQ      '4'
     C*
     C*** BEGIN CHG *** 03/22/94 ***********************************************
     C********** HSKEY     CHAINPHS01                60
     C*** BEGIN CHG *** 02/18/98 ***********************************************
     C********** HSKEY     SETGTPHS01
     C     S4KEY         SETGT     PS401
     C     NXTHOS        TAG
     C**********           READPPHS01                    60
     C                   READP     PS401                                  60
     C     *IN60         IFEQ      '0'
     C********** HSNWCD    ANDEQXXNWCD
     C********** HSFEIN    ANDEQCAFEIN
     C********** HSFMOD    ANDEQCAMOD
     C     S4NWCD        ANDEQ     XXNWCD
     C     S4FEIN        ANDEQ     CAFEIN
     C     S4MOD         ANDEQ     CAMOD
     C*** BEGIN ADD *** 03/06/98 **************************************
     C                   Z-ADD     S4ENDT        XXWN80            8 0
     C     S4ENDT        IFEQ      0
     C                   Z-ADD     99999999      XXWN80
     C                   ENDIF                                                  ENDT = 0
     C*** END ADD *** 03/06/98 ****************************************
     C* THE SERVICE DATE MUST BE BETWEEN THE PROVIDERS CONTRACT DATES
     C*
     C********** CAFDAT    IFGT HSTMDT
     C*** BEGIN CHG *** 03/06/98 **************************************
     C********   CAFDAT    IFGT S4ENDT
     C     CAFDAT        IFGT      XXWN80
     C*** END CHG *** 03/06/98 ****************************************
     C                   GOTO      NXTHOS
     C                   END
     C*
     C***DEL     HSNWCD    IFNE XXNWCD
     C*** 8/26/96 *** DEL**GOTO NXTHOS
     C***                  END
     C**********           MOVE HSDTCD    SCCODE
     C**********           Z-ADDHSCEDT    SCCEDT
     C                   MOVE      S4DISC        SCCODE
     C                   Z-ADD     S4BGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCEDT
     C**********           Z-ADDHSTMDT    SCTMDT
     C*** END   CHG *** 02/18/98 ***********************************************
     C                   Z-ADD     S4ENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTMDT
     C                   ELSE
     C                   MOVE      *BLANKS       SCCODE
     C                   Z-ADD     *ZEROS        SCCEDT
     C                   Z-ADD     *ZEROS        SCTMDT
     C                   END                                                    *IN60 OFF
     C*
     C                   ELSE
     C*
     C*** BEGIN CHG *** 02/18/98 ***********************************************
     C*
     C********** PHKEY     CHAINPPH01                60
     C********** PHKEY     SETGTPPH01
     C     S4KEY         SETGT     PS402
     C     NXTPHY        TAG
     C**********           READPPPH01                    60
     C                   READP     PS402                                  60
     C     *IN60         IFEQ      '0'
     C********** PHNWCD    ANDEQXXNWCD
     C********** PHFEIN    ANDEQCAFEIN
     C********** PHFMOD    ANDEQCAMOD
     C     S4NWCD        ANDEQ     XXNWCD
     C     S4FEIN        ANDEQ     CAFEIN
     C     S4MOD         ANDEQ     CAMOD
     C*** BEGIN ADD *** 03/06/98 **************************************
     C                   Z-ADD     S4ENDT        XXWN80
     C     S4ENDT        IFEQ      0
     C                   Z-ADD     99999999      XXWN80
     C                   ENDIF                                                  ENDT = 0
     C*** END ADD *** 03/06/98 ****************************************
     C* THE SERVICE DATE MUST BE BETWEEN THE PROVIDERS CONTRACT DATES
     C*
     C********** CAFDAT    IFGT PHNXDT
     C*** BEGIN CHG *** 03/06/98 **************************************
     C********   CAFDAT    IFGT S4ENDT
     C     CAFDAT        IFGT      XXWN80
     C*** END CHG *** 03/06/98 ****************************************
     C                   GOTO      NXTPHY
     C                   END
     C*
     C*****      PHNWCD    IFNE XXNWCD
     C*** DEL *** 8/26/96 *GOTO NXTPHY
     C***                  END
     C**********           MOVE PHDTCD    SCCODE
     C**********           Z-ADDPHNEDT    SCCEDT
     C                   MOVE      S4DISC        SCCODE
     C                   Z-ADD     S4BGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCEDT
     C**********           Z-ADDPHNXDT    SCTMDT
     C*** END   CHG *** 02/18/98 ***********************************************
     C                   Z-ADD     S4ENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTMDT
     C                   ELSE
     C                   MOVE      *BLANKS       SCCODE
     C                   Z-ADD     *ZEROS        SCCEDT
     C                   Z-ADD     *ZEROS        SCTMDT
     C                   END                                                    *IN60 OFF
     C*** END CHG *** 03/22/94 *************************************************
     C*
     C                   END
     C*
     C*  GET THE DISCOUNT MASTER RECORD
     C*
     C     BDKEY         SETGT     PBD01
     C     NXTBD         TAG
     C                   READP     PBD01                                  60
     C*
     C*** VERIFY DISCOUNT CODE AND STATUS
     C*
     C     *IN60         IFEQ      '0'
     C     BDCODE        IFNE      SCCODE
     C     BDSTAT        OREQ      'I'
     C*
     C                   GOTO      NXTBD
     C                   ELSE
     C                   MOVE      BDTYPE        SCTYPE
     C                   Z-ADD     BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCODT
     C                   MOVE      BDCOM1        SCCOM1
     C                   MOVE      BDCOM2        SCCOM2
     C*** BEGIN ADD *** 10/03/06 **************************************
     C     BDTYPE        IFEQ      'D9'
     C                   MOVE      BDCOM2        DSCOM2
     C                   MOVEL(P)  DSCOM         SCCOM2
     C                   MOVE      DSSRCE        SCSRCE            1
     C                   MOVE      DSLESS        SCLESS            1
     C                   END
     C*** END   ADD *** 10/03/06 **************************************
     C                   MOVEL     BDPC01        SCPC01
     C                   MOVEL     BDPC02        SCPC02
     C                   MOVEL     BDPC03        SCPC03
     C*** BEGIN ADD *** 04/30/98 **************************************
     C                   MOVE      'N'           SCFL03
     C     BDPC03        IFEQ      1
     C                   MOVE      'Y'           SCFL03
     C                   ENDIF                                                  PC03 = 1
     C*** END ADD *** 04/30/98 ****************************************
     C                   MOVEL     BDPC04        SCPC04
     C                   MOVEL     BDPC05        SCPC05
     C                   MOVEL     BDPC06        SCPC06
     C                   MOVEL     BDPC07        SCPC07
     C                   Z-ADD     BDAM01        SCAM01
     C                   Z-ADD     BDAM02        SCAM02
     C                   Z-ADD     BDAM03        SCAM03
     C                   Z-ADD     BDAM04        SCAM04
     C                   Z-ADD     BDAM05        SCAM05
     C                   Z-ADD     BDAM06        SCAM06
     C                   Z-ADD     BDAM07        SCAM07
     C                   Z-ADD     BDAM08        SCAM08
     C                   Z-ADD     BDAM09        SCAM09
     C                   Z-ADD     BDAM10        SCAM10
     C                   Z-ADD     BDAM11        SCAM11
     C                   Z-ADD     BDAM25        SCAM25
     C*** BEGIN ADD *** 04/11/07 **************************************
     C     BDTYPE        IFEQ      'DM'
     C*
     C*** Beg Add *** 02/26/10 ****************************************
     C     *LIKE         DEFINE    BDCODE        @@CODE
     C                   MOVE      QQCO#         @@CO#
     C                   MOVE      QQSUB         @@SUB
     C                   MOVE      QQFND         @@FND
     C                   MOVE      QQFYR         @@FYR
     C                   MOVE      QQCASE        @@CASE
     C                   MOVE      CADSEQ        @@DSEQ
     C                   MOVE      CAFEIN        @@FEIN
     C                   MOVE      CAMOD         @@MOD
     C                   MOVE      CAFDAT        @@FDAT
     C                   CALL      'N96DMR'
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB             5
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@FEIN            9
     C                   PARM                    @@MOD             3
     C                   PARM                    @@FDAT            8
     C                   PARM                    @@DSEQ            5
     C                   PARM      BDCODE        @@CODE
     C                   PARM      *ZEROS        @@CEDT            8
     C                   PARM      *BLANKS       @@SYS             1
     C                   PARM      *BLANKS       @@FL1             1
     C                   PARM      *BLANKS       @@FL2             1
     C                   PARM      *BLANKS       @@FL3             1
     C                   PARM      *BLANKS       @@FL4             1
     C                   PARM      *BLANKS       @@FL5             1
     C                   PARM      *BLANKS       @@DS1            10
     C                   PARM      *BLANKS       @@DS2            10
     C                   PARM      *BLANKS       @@DS3            10
     C                   PARM      *BLANKS       @@DS4            10
     C                   PARM      *BLANKS       @@DS5            10
     C                   PARM      *BLANKS       @@RTCD            1
     C                   PARM      'F'           @@MODE            1
     C                   PARM      *ZEROS        @@ALOW            9 2
     C*
     C                   Z-ADD     @@ALOW        SCMRA
     C*
     C                   CALL      'N96DMR'
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB             5
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@FEIN            9
     C                   PARM                    @@MOD             3
     C                   PARM                    @@FDAT            8
     C                   PARM                    @@DSEQ            5
     C                   PARM      BDCODE        @@CODE
     C                   PARM      *ZEROS        @@CEDT            8
     C                   PARM      *BLANKS       @@SYS             1
     C                   PARM      *BLANKS       @@FL1             1
     C                   PARM      *BLANKS       @@FL2             1
     C                   PARM      *BLANKS       @@FL3             1
     C                   PARM      *BLANKS       @@FL4             1
     C                   PARM      *BLANKS       @@FL5             1
     C                   PARM      *BLANKS       @@DS1            10
     C                   PARM      *BLANKS       @@DS2            10
     C                   PARM      *BLANKS       @@DS3            10
     C                   PARM      *BLANKS       @@DS4            10
     C                   PARM      *BLANKS       @@DS5            10
     C                   PARM      *BLANKS       @@RTCD            1
     C                   PARM      'P'           @@MODE            1
     C                   PARM      *ZEROS        @@ALOW            9 2
     C*
     C                   Z-ADD     @@ALOW        SCPROF
     C*
     C                   CALL      'N96DMR'
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB             5
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@FEIN            9
     C                   PARM                    @@MOD             3
     C                   PARM                    @@FDAT            8
     C                   PARM                    @@DSEQ            5
     C                   PARM      BDCODE        @@CODE
     C                   PARM      *ZEROS        @@CEDT            8
     C                   PARM      *BLANKS       @@SYS             1
     C                   PARM      *BLANKS       @@FL1             1
     C                   PARM      *BLANKS       @@FL2             1
     C                   PARM      *BLANKS       @@FL3             1
     C                   PARM      *BLANKS       @@FL4             1
     C                   PARM      *BLANKS       @@FL5             1
     C                   PARM      *BLANKS       @@DS1            10
     C                   PARM      *BLANKS       @@DS2            10
     C                   PARM      *BLANKS       @@DS3            10
     C                   PARM      *BLANKS       @@DS4            10
     C                   PARM      *BLANKS       @@DS5            10
     C                   PARM      *BLANKS       @@RTCD            1
     C                   PARM      'T'           @@MODE            1
     C                   PARM      *ZEROS        @@ALOW            9 2
     C*
     C                   Z-ADD     @@ALOW        SCTECH
     C*** End Add *** 02/26/10 ****************************************
     C* Retrieve % of CMS based upon the procedure code range.
     C*
     C                   Z-ADD     *ZEROS        COFEE
     C                   MOVEL(P)  CACPT4        KK10A1
     C     COKY1         SETGT     PCO02
     C     COKY2         READPE    PCO02                                  99
     C     *IN99         DOUEQ     *ON
     C     KK10A1        IFGE      CO10A1
     C     KK10A1        ANDLE     CO10A2
     C     COSTAT        ANDNE     'I'
     C                   LEAVE
     C                   ELSE
     C     COKY2         READPE    PCO02                                  99
     C                   ENDIF
     C                   ENDDO
     C*
     C     COFEE         DIV(H)    100           FEEPCT            9 4
     C*
     C* Retrieve allowance values
     C*
     C                   CALL      'FA747R'
     C                   PARM      DBRRN         ##RRN             9 0
     C                   PARM      *ZEROS        ##MRA             9 2
     C                   PARM      *ZEROS        ##PROF            9 2
     C                   PARM      *ZEROS        ##TECH            9 2
     C*** BEG ADD *** 03/03/09 ****************************************
     C                   PARM      *BLANKS       ##1A7             1
     C*** END ADD *** 03/03/09 ****************************************
     C*** Beg Add *** 02/26/10 ****************************************
     C                   PARM      'B'           ##CALL            1
     C*** End Add *** 02/26/10 ****************************************
     C*
     C*** Beg Chg *** 02/26/10 ****************************************
     C**********           DIV  1.25      ##MRA     H
     C**********           DIV  1.25      ##PROF    H
     C**********           DIV  1.25      ##TECH    H
     C*
     C********** ##MRA     MULT FEEPCT    SCMRA     H
     C********** ##PROF    MULT FEEPCT    SCPROF    H
     C********** ##TECH    MULT FEEPCT    SCTECH    H
     C                   Z-ADD     ##MRA         SCFSTO
     C                   Z-ADD     ##PROF        SCFSPC
     C                   Z-ADD     ##TECH        SCFSTC
     C*** End Chg *** 02/26/10 ****************************************
     C*
     C                   ENDIF
     C*** END   ADD *** 04/11/07 **************************************
     C                   END                                                    BDCODE/BDSTAT
     C                   ELSE
     C                   MOVE      *BLANKS       SCTYPE
     C                   Z-ADD     *ZEROS        SCCODT
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C                   Z-ADD     *ZEROS        SCPC01
     C                   Z-ADD     *ZEROS        SCPC02
     C                   Z-ADD     *ZEROS        SCPC03
     C*** BEGIN ADD *** 04/30/98 **************************************
     C                   MOVE      *BLANK        SCFL03
     C*** END ADD *** 04/30/98 ****************************************
     C                   Z-ADD     *ZEROS        SCPC04
     C                   Z-ADD     *ZEROS        SCPC05
     C                   Z-ADD     *ZEROS        SCPC06
     C                   Z-ADD     *ZEROS        SCPC07
     C                   Z-ADD     *ZEROS        SCAM01
     C                   Z-ADD     *ZEROS        SCAM02
     C                   Z-ADD     *ZEROS        SCAM03
     C                   Z-ADD     *ZEROS        SCAM04
     C                   Z-ADD     *ZEROS        SCAM05
     C                   Z-ADD     *ZEROS        SCAM06
     C                   Z-ADD     *ZEROS        SCAM07
     C                   Z-ADD     *ZEROS        SCAM08
     C                   Z-ADD     *ZEROS        SCAM09
     C                   Z-ADD     *ZEROS        SCAM10
     C                   Z-ADD     *ZEROS        SCAM11
     C                   Z-ADD     *ZEROS        SCAM25
     C*** BEGIN ADD *** 10/03/06 **************************************
     C                   MOVE      *BLANKS       SCSRCE
     C                   MOVE      *BLANKS       SCLESS
     C*** END   ADD *** 10/03/06 **************************************
     C                   END                                                    *IN60 OFF
     C*
     C                   END                                                    *IN50
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
      /Free
       //*** Begin Add *** 06/10/19 *******************************************
       SCPRDS = 'Network Information';
       //*** End   Add *** 06/10/19 *******************************************
      /End-Free
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   WRITE     SCR01
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR APPROPRIATE DISCOUNT TYPE
     C*
     C*** BEGIN DEL *** 05/29/98 **************************************
     C********   SCTYPE    IFEQ 'D1'
     C********             EXFMTSCR02
     C********             ELSE
     C********   SCTYPE    IFEQ 'D2'
     C********             EXFMTSCR03
     C********             ELSE
     C********   SCTYPE    IFEQ 'D3'
     C********             EXFMTSCR04
     C********             ELSE
     C********   SCTYPE    IFEQ 'D4'
     C********             EXFMTSCR05
     C********             ELSE
     C********   SCTYPE    IFEQ 'D5'
     C********             EXFMTSCR06
     C****BEGIN ADD *** 03/07/95 ***********************************************
     C********             ELSE
     C********   SCTYPE    IFEQ 'D7'
     C****BEGIN ADD *** 01/15/98 **************************************
     C********   SCTYPE    OREQ 'DE'
     C****END*ADD *** 01/15/98 ****************************************
     C********             EXFMTSCR07
     C****END*ADD *** 03/07/95 *************************************************
     C********             ELSE
     C****BEGIN ADD *** 04/30/98 **************************************
     C********   SCTYPE    IFEQ 'DD'
     C********             EXFMTSCR10
     C********             ELSE
     C****END*ADD *** 04/30/98 ****************************************
     C********             EXFMTSCRERR
     C****BEGIN ADD *** 04/30/98 **************************************
     C********             END
     C****END*ADD *** 04/30/98 ****************************************
     C********             END
     C********             END
     C********             END
     C********             END
     C********             END
     C********             END
     C*** END DEL *** 05/29/98 ****************************************
     C*** BEGIN ADD *** 05/29/98 **************************************
     C                   DO
     C*** BEGIN ADD *** 08/20/98 **************************************
     C     SCTYPE        COMP      'DB'                                   60
     C   60              EXFMT     SCR07
     C   60              LEAVE
     C*** END ADD *** 08/20/98 ****************************************
     C*** BEGIN ADD *** 10/03/06 ***********************************************
     C     SCTYPE        COMP      'D9'                                   60
     C   60              EXFMT     SCR08
     C   60              LEAVE
     C*** END ADD   *** 10/03/06 ***********************************************
     C     SCTYPE        COMP      'DC'                                   60
     C   60              EXFMT     SCR09
     C   60              LEAVE
     C     SCTYPE        COMP      'DD'                                   60
     C   60              EXFMT     SCR10
     C   60              LEAVE
     C*** BEGIN ADD *** 05/21/98 **************************************
     C     SCTYPE        COMP      'DF'                                   60
     C   60              EXFMT     SCR11
     C   60              LEAVE
     C*** END ADD *** 05/21/98 ****************************************
     C     SCTYPE        COMP      'D1'                                   60
     C   60              EXFMT     SCR02
     C   60              LEAVE
     C     SCTYPE        COMP      'D2'                                   60
     C   60              EXFMT     SCR03
     C   60              LEAVE
     C     SCTYPE        COMP      'D3'                                   60
     C   60              EXFMT     SCR04
     C   60              LEAVE
     C     SCTYPE        COMP      'D4'                                   60
     C   60              EXFMT     SCR05
     C   60              LEAVE
     C     SCTYPE        COMP      'D5'                                   60
     C   60              EXFMT     SCR06
     C   60              LEAVE
     C     SCTYPE        COMP      'D7'                                   60
     C  N60SCTYPE        COMP      'DE'                                   60
     C   60              EXFMT     SCR07
     C   60              LEAVE
     C*** BEGIN ADD *** 04/11/07 **************************************
     C     SCTYPE        COMP      'DM'                                   60
     C     *IN60         IFEQ      *ON
     C                   EXFMT     SCR12
     C                   LEAVE
     C                   ENDIF
     C*** END   ADD *** 04/11/07 **************************************
     C*
     C*** BEG ADD *** 08/12/10 ****************************************
     C     SCTYPE        COMP      'DR'                                   60
     C     *IN60         IFEQ      *ON
     C                   EXFMT     SCR13
     C                   LEAVE
     C                   ENDIF
     C*
     C     SCTYPE        COMP      'DS'                                   60
     C     *IN60         IFEQ      *ON
     C                   EXFMT     SCR14
     C                   LEAVE
     C                   ENDIF
     C*** END ADD *** 08/12/10 ****************************************
     C*
     C                   EXFMT     SCRERR
     C                   ENDDO                                                  DO Loop
     C*** END ADD *** 10/21/97 ****************************************
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*
     C*   F3 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*** BEGIN ADD *** 03/07/95 ***********************************************
     C*
     C*   F5 = DETAIL INQUIRY(CARVE OUT)
     C*
     C     *INKE         IFEQ      '1'
     C*
     C                   MOVE      BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQFDAT            8
     C*
     C*** BEGIN ADD *** 01/15/98 **************************************
     C*** END ADD *** 01/15/98 ****************************************
      *
      *** Begin Change *** 06/05/19 ***********************************
     C***     BDTYPE        IFEQ      'D7'
     C***                   CALL      'N91D7R'
     C***                   PARM                    QQPRDS
     C***                   PARM      '1'           QQCALL            1
     C***                   PARM                    SCCODE
     C***                   PARM                    QQFDAT
      /Free
       If BDtype = 'D7';
           QQcall = '1';
           QQefdt = %CHAR(%date(BDEFDT:*CYMD):*ISO0);
           NetworkDisc(BDtype:QQprds:QQcall:SCcode:
            QQefdt);
       Endif;
      /End-Free
     C*** BEGIN ADD *** 01/15/98 **************************************
     C***                  ENDIF                                                  TYPE = 'D7'
      *
      *** End   Change *** 06/05/19 ***********************************
     C     BDTYPE        IFEQ      'DE'
     C                   CALL      'N91DER'
     C                   PARM                    QQPRDS
     C                   PARM      '1'           QQCALL            1
     C                   PARM                    SCCODE
     C                   PARM                    QQFDAT
     C                   ENDIF                                                  TYPE = 'DE'
     C*** END ADD *** 01/15/98 ****************************************
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
     C*** END ADD *** 03/07/95 *************************************************
     C*** BEGIN ADD *** 04/26/94 ***********************************************
     C*
     C*   F6 = PROV MASTER INQUIRY
     C*
     C     *INKF         IFEQ      '1'
     C*
     C                   MOVE      *ZEROS        Q1FEIN            9
     C                   MOVE      *ZEROS        Q1MOD             3
     C                   MOVE      *BLANKS       Q1PRDS           40
     C                   MOVE      SCFEIN        Q1FEIN
     C                   MOVE      SCMOD         Q1MOD
     C                   MOVEL     PTL(1)        Q1PRDS
      *** BEGIN CHG *** 12/31/96 ***************************************
     C*********************CALL 'F0031R'
     C*********************PARM           Q1PRDS
     C*********************PARM '4'       Q1CALL  1
     C*********************PARM           Q1FEIN
     C*********************PARM           Q1MOD
      *
      *  CALL API TO OBTAIN FMPDP RRN
      *
     C                   CALL      'FA006R'
     C                   PARM                    Q1FEIN
     C                   PARM                    Q1MOD
     C                   PARM                    @@RRN             9 0          FMPDP RRN
      *
      *  CALL PROVIDER MAINTENANCE IN DISPLAY MODE
      *
     C*** BEGIN CHG *** 03/06/98 **************************************
     C********             CALL 'F0031R1'
     C                   CALL      'F0032R01'
     C*** END CHG *** 03/06/98 ****************************************
     C     *INKC         PARM      *INKC         @@KC              1
     C     *INKL         PARM      *INKL         @@KL              1
     C                   PARM      'D'           @@MODE            1
     C                   PARM                    @@RRN
      *
      *** END   CHG *** 12/31/96 ***************************************
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
     C*** END ADD *** 04/26/94 *************************************************
     C*
     C*   F12 = PREVIOUS
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C   99              CLOSE     N0582F
     C   99              MOVE      '0'           *IN99
     C*
     C     ##PRVB        IFEQ      'Y'
     C                   CLOSE     FAEOL4
     C                   ELSE                                                   ##PRVB = Y
     C                   CLOSE     FADCL
     C                   ENDIF                                                  ##PRVB = Y
     C*                    RETRN
     C                   MOVE      '1'           *INLR
     C*
     O*** BEGIN DEL *** 07/23/99 **************************************
     O********PCA01   E                RLSCAE
     O*** END DEL *** 07/23/99 ****************************************
**  PTL - CALLED PGM TITLES
      PROVIDER MASTER MAINTENANCE
