     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H*
     H/TITLE W8608R - CLAIM AUTO CLOSURE REPORT
     H*****************************************************************
     H*
     H*    Reads all DARTS/Claim control records, processing all the
     H*    claims for every group record for the company.  The control
     H*    file parameters for auto claim closure are used to qualify
     H*    claims for automatic closing, based on the batch date.
     H*
     H*    Every claim selected is reported as a closure account or as
     H*    an exception if some conditions prevent closing.  Closure
     H*    accounts are either diaried to the adjustor; or if a remark
     H*    code is not in the control file, they are automatically
     H*    closed.
     H*
     H*    Monetary totals for the closings are printed by group;
     H*    final run totals are separated by diaried versus automatic-
     H*    ally closed claims.
     H*****************************************************************
     H*
     H*  08/18/97 C:58           PROJECT WC675    NEW FOR V3
     H*****************************************************************
     H*
     H*  CALLED FROM:
     H*      DAILY BATCH - W6000C
     H*
     H*  CALLS TO:
     H*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 08/15/17 C:33  Project: SY394                                 *
     H*                Update initial reserve flag                    *
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 12/01/14 C:279 Project: 114WLF                                *
     H*                Delete pending reserve worksheet on closing a  *
     H*                claim.                                         *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 08/10/11 C:062 Support: WC102 77046                           *
     H*                Don't write to WMRHP if record only claim.     *
     H*
     H* 03/03/10 C:232 Project:  139WNH                               *
     H*                Update all of the initial maintenance fields.  *
     H*                Populating RHIDT and RHDT with the current     *
     H*                system date.                                   *
     H*
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 09/11/09  C:062 Project: 133WGP
     H*                 Don't allow closure on indemnity claim if no   s
     H*                 compensation payments.
     H*                                                               *
     H* 11/03/05 C:15  Support: WC102/49689                           *
     H*                Added code to load the batch date into CHEFDT  *
     H*                and hardcode '235500' into CHTIME.  See changes*
     H*                on 08/17/01 and 06/12/00.                      *
     H*                                                               *
     H* 08/17/01 C:15  Support: WC102/28654                           *
     H*                Reversed changes made on 6/12/00 which loads   *
     H*                the batch dt instead of the system dt because  *
     H*                this causes a problem on claims that are closed*
     H*                on the same day they were opened.  See support *
     H*                item for details.                              *
     H*                                                               *
     H* 08/10/01 C:62  Support: WC048/28589                           *
     H*                Rename claim file fields so that they don't    *
     H*                affect DARTS file.                             *
     H*                                                               *
     H* 06/12/00 C:15  Support: WC081/23082                           *
     H*                Subtracted 1 from the '# of days with no       *
     H*                activity' so claims are closed on the correct  *
     H*                day.  Also, chgd pgm to load the batch date to *
     H*                CHEFDT, RHEFDT and CMCDAT to indicate the      *
     H*                correct date.                                  *
     H*                                                               *
     H*  04/26/00 C:61  Project: DR097                                *
     H*                 Chg ALL PDS pgms                              *
     H*                 1. Replace write to SDCMP with API DA700R     *
     H*                                                               *
     H* 04/10/00 C:118 PROJECT: WC987                                 *
     H*                LOAD ALTERNATE COMPANY NAME FROM FUND MASTER   *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H* 05/21/98 C:15 Support: WC065/11696                            *
     H*              -Added code to clear the DARTS record before     *
     H*               loading the fields.                             *
     H*              -Added code to only write the DARTS diary if one *
     H*               does exist or it is older than the claim reopen *
     H*               date.                                           *
     H*                                                               *
     H* 04/20/98 C:15 Project: WC577                                  *
     H*               Chgd the pgm to check the initial entry date    *
     H*               instead of the accident date when determining   *
     H*               whether or not to close the claim due to        *
     H*               inactivity.                                     *
     H*                                                               *
     H* 03/17/98 C:15 Project: WC577                                  *
     H*               Added a line to clear XXSELF before processing  *
     H*               each claim, to prevent claims from being closed *
     H*               inadvertently.                                  *
     H*                                                               *
     H*  02/23/98 C:85 Project: WC577                                 *
     H*                CHANGED THE PROGRAM MOVED TO CMPNAM            *
     H*                FROM W8608R TO W0070R.                         *
     H*                CHANGED THE FIELD MOVED TO CMSMCD              *
     H*                FROM *BLANKS TO CTSMCD.                        *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H* INDICATOR USAGE:
     H*
     H*
     H*        45  No claims closed
     H*        46  No claims diaried
     H*        47  No exceptions found
     H*
     H*        60  General chain indicator
     H*
     H*        61  Claim/DARTS: EOF and loop control
     H*        62  Claim Master: EOF and loop control
     H*        65  Request Payments: EOF and loop control
     H*****************************************************************
     H*
     F*================================================================
     F*     FILE DEFINITIONS
     F*================================================================
     F*________________________________________________________________
     F*    Claim master  -- Open claims only
     FWMCMLH    UF   E           K DISK
     F*________________________________________________________________
     F*    Claim/DARTS Control file
     FWMCXL     IF   E           K DISK
     F*________________________________________________________________
     F*     Reserve History File
     FWMRHL     IF A E           K DISK
     F*________________________________________________________________
     F*     Request Payment File
     FWARPL9    IF   E           K DISK
     F*________________________________________________________________
     F*     Claims Check Payment File
     FWDPAL5    IF   E           K DISK
     F*________________________________________________________________
     F*     Claim Status Change History File
     FWMCHP     O    E             DISK
     F*________________________________________________________________
     F*     Claimant Demographics File
     FWMCDL     IF   E           K DISK
     F*________________________________________________________________
     F*     DARTS Remark Types
     FSTCTL1    IF   E           K DISK
     F*________________________________________________________________
      ****** BEG DEL *** 04/26/00 *************************************
     F******* DARTS Diary Remarks file
     F*******SDCML   O   E           K        DISK
     F*******            PCM01                             KRENAMEPCM02
     F*________________________________________________________________
     F*******     DARTS Diary Remarks file
     F*******SDCMLI  IF  E           K        DISK
     F*******            PCM01                             KRENAMEPCM03
      ****** END DEL *** 04/26/00 *************************************
     F*________________________________________________________________
     F*     Group Master File
     FWMFNL     IF   E           K DISK
     F*________________________________________________________________
     F*     Table File
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)
      ******* beg add ******** 12/01/14 *******************************
     fwdrwl     uf   e           k disk    rename(prw01:prw00)
      ******* beg add ******** 12/01/14 *******************************
     F*________________________________________________________________
     F*    Company File
     FSMCOL     IF   E           K DISK
     F*________________________________________________________________
     F*    User parameters
     FWMUPL1    IF   E           K DISK
     F*________________________________________________________________
     F*    Output Routing
     FSMORL     IF   E           K DISK
     F*** BEGIN ADD *** 05/21/98 **************************************
     F*    Claim History
     FWMCHL     IF   E           K DISK
     F                                     RENAME(PCH01:PCH02)
     F*________________________________________________________________
     F*     DARTS Diary Remarks file
     FSDCMLF    IF   E           K DISK
     F                                     RENAME(PCM01:PCM04)
     F*** END ADD ***** 05/21/98 **************************************
     F*________________________________________________________________
     F*     Auto Close Report
     FQSYSPRT   O    F  196        PRINTER OFLIND(*INOF)
     F*________________________________________________________________
     F*     Exception Report
     FQSYSPRT1  O    F  196        PRINTER OFLIND(*INOA)
     F*________________________________________________________________
     F*     Eligible to Close Diaries List
     FQSYSPRT2  O    F  196        PRINTER OFLIND(*INOB)

     D*
     D*================================================================
     D*     TABLES AND ARRAYS
     D*================================================================
     D*
     D CP              S              9  2 DIM(4)                               Claim payments
     D CR              S              9  2 DIM(4)                               Claim reserves
     D*  Auto close totals
     D CFP             S             11  2 DIM(4)                               Grp pmt-close
     D CFR             S             11  2 DIM(4)                               Grp res-close
     D CTP             S             11  2 DIM(4)                               Tot pmt-close
     D CTR             S             11  2 DIM(4)                               Tot res-close
     D*  Diaries totals
     D DFP             S             11  2 DIM(4)                               Grp pmt-Diary
     D DFR             S             11  2 DIM(4)                               Grp res-Diary
     D DTP             S             11  2 DIM(4)                               Total payments
     D DTR             S             11  2 DIM(4)                               Total reserves
     D*
     D TOT             S             11  2 DIM(4)                               Total incurred
     D*** END   ADD *** 08/10/01 **************************************
     D*
     D*================================================================
     D*     DATA STRUCTURES
     D*================================================================
     D*________________________________________________________________
     D*    Diary Key
     D                 DS                  INZ
     D  XCKEY                  1     40
     D  XCCO#                  1      3  0
     D  XCFND                  4      6  0
     D  XCFYR                  7      9  0
     D  XCCASE                10     16  0
     D*________________________________________________________________
     D*    Reserve Types
     D                 DS
     D  CMT                    1     20
     D                                     DIM(10)                              RESERVE TYPES
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
     D*________________________________________________________________
     D*    Payment Amounts
     D                 DS
     D  CMA                    1     90  2
     D                                     DIM(10)                              PMT AMOUNT
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
     D*________________________________________________________________
     D*    Reserve Amounts
     D                 DS
     D  CMR                    1     90  2
     D                                     DIM(10)                              RESERVE AMOUNT
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
     D*________________________________________________________________
     D*    Reserve Types
     D                 DS
     D  RMT                    1     20
     D                                     DIM(10)                              RESERVE TYPES
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
     D*________________________________________________________________
     D*    Reserve Amounts
     D                 DS
     D  RMR                    1     90  2
     D                                     DIM(10)                              RESERVE AMOUNT
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
     D*________________________________________________________________
     D*    Date and time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*________________________________________________________________
     D MSG             C                   CONST('Claim automatically -
     D                                     closed due to inacti-
     D                                     vity.')
     D*
      //***  Begin Add  ***  08/15/17  ************************************
      /copy wPrClmRsv
      //***  End   Add  ***  08/15/17  ************************************
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     I*================================================================
     I*     DATABASE FIELDS
     I*================================================================
     I*** BEGIN ADD *** 08/10/01 **************************************
     IPCM04
     I              CMSTAT                      C2STAT
     I              CMUSER                      C2USER
     I              CMPGM                       C2PGM
     I              CMTIME                      C2TIME
     I              CMDT                        C2DT
     I              CMIUSR                      C2IUSR
     I              CMIPGM                      C2IPGM
     I              CMIDT                       C2IDT

     C*****************************************************************
     C*
     C*================================================================
     C*     MAINLINE
     C*================================================================
     C*________________________________________________________________
     C*   Start reading Claim/DARTS control: process all groups for CO#
     C     CXKY          CHAIN     PCX01                              61
     C     *IN61         DOWEQ     *OFF
     C*
     C*________________________________________________________________
     C*   Process group if an auto closing flag is on:
     C     CXMOC         IFEQ      'Y'                                            -medical
     C     CXINC         OREQ      'Y'                                            -indemnity
     C                   EXSR      SRDATE                                         calc dates
     C*
     C*   Print totals for prior group
     C     XXFND         IFNE      *ZERO                                          not 1st ti
     C                   EXSR      SRFPRT                                        TOTAL PRINT
     C                   ENDIF                                                    FND ne ZER
     C*
     C                   MOVE      CXFND         XXFND
     C     FNKY          CHAIN     PFN01                              60
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C*________________________________________________________________
     C*   Position claim master to process all claims for group
     C     CMKY          CHAIN     PCM01                              62          = Co/Grp
     C     *IN62         DOWEQ     *OFF
     C                   MOVE      'N'           XXCONT                          clear
     C                   Z-ADD     *ZEROS        XXSTDT                            work
     C                   MOVE      *BLANK        XXRMK                              fields
     C*** BEGIN ADD *** 03/17/98 ***********************************************
     C                   MOVE      *OFF          XXSELF
     C*** END ADD ***** 03/17/98 ***********************************************
     C*
     C                   MOVE      CMFYR         XXFYR
     C                   MOVE      CMCASE        XXCASE
     C                   MOVE      CMSS#         XXSS#
     C*
     C*________________________________________________________________
     C*   Select claim master records to process
     C*      o medical only claims
     C     CXMOC         IFEQ      'Y'                                          med flag=Y &
     C     CMLTFL        ANDEQ     'M'                                          -medical clm
     C*                                                         or
     C     CXMOC         OREQ      'Y'                                          med flag=Y &
     C     CMLTFL        ANDEQ     'R'                                          -record only
     C                   MOVE      'Y'           XXCONT                         yes, process
     C                   Z-ADD     CXNFAM        XXDAYS                         med days
     C                   Z-ADD     XXMEDT        XXSTDT                         start date
     C*
     C*      o set up remark code if not actually closing
     C     CXACCM        IFEQ      'N'                                              no close
     C                   MOVE      CXADRM        XXRMK                          Adj diary cd
     C                   ENDIF                                                  e ACCM = Y
     C*
     C                   ELSE                                                   x MOC/LTFL=Y
     C*
     C*      o indemnity claims
     C     CXINC         IFEQ      'Y'                                          ind flag=Y
     C     CMLTFL        ANDEQ     'I'                                          -indemnity
     C                   MOVE      'Y'           XXCONT                         continue flg
     C                   Z-ADD     CXNFAI        XXDAYS                         indem days
     C                   Z-ADD     XXINDT        XXSTDT                         start date
     C*
     C*      o set up remark code if not actually closing
     C     CXACCI        IFEQ      'N'                                              no close
     C                   MOVE      CXADRI        XXRMK                          Adj diary cd
     C                   ENDIF                                                  e ACCI = Y
     C*
     C                   ENDIF                                                  e INC/LTFL=I
     C                   ENDIF                                                  e MOC/LTFL=Y
     C*
     C*________________________________________________________________
     C*   Select for closing action
     C     XXCONT        IFEQ      'Y'                                          Select claim
     C*** BEGIN DLT *** 04/20/98 ***********************************************
     C********** CMACDT    CASLEXXSTDT    SRCSEL
     C*** END DLT ***** 04/20/98 ***********************************************
     C*** BEGIN ADD *** 04/20/98 ***********************************************
     C     CMIDT         CASLE     XXSTDT        SRCSEL
     C*** END ADD ***** 04/20/98 ***********************************************
     C                   ENDCS
     C                   ENDIF                                                  e XXCONT = Y
     C*
     C*________________________________________________________________
     C*================================================================
     C*   Take closing actions
     C     XXSELF        IFEQ      *ON                                          Selected
     C*
     C     XXPARQ        IFEQ      *ON                                          Is req pmt
     C                   EXSR      SRXCPT                                       Except report
     C*
     C                   ELSE                                                   x XXPARQ= ON
     C     XXRMK         IFNE      *BLANK                                       Remark due
     C                   EXSR      SRDIRY                                       Write diary
     C*
     C                   ELSE                                                   x XXRMK =
     C                   EXSR      SRCLOS                                       CLOSE IT
     C                   ENDIF                                                  e XXRMK = ''
     C*
     C                   ENDIF                                                  e XXPARQ =
     C                   ENDIF                                                  e XXSELF=ON
     C*================================================================
     C*
     C*________________________________________________________________
     C*   Get next claim
     C     CMKY          READE     PCM01                                  62     EOF group
     C                   ENDDO                                                  e *IN62=OFF
     C*
     C                   ENDIF                                                  e MOC/INC =Y
     C*
     C*________________________________________________________________
     C*   Get next control record
     C     CXKY          READE     PCX01                                  61     EOF group
     C                   ENDDO                                                  e *IN61=OFF
     C*________________________________________________________________
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*________________________________________________________________
     C*
     C*________________________________________________________________
     C*  PRINT FINAL TOTALS
     CLR                 EXSR      SRFTOT
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKy POR01;
       If %found;
          Except Route;
       Endif;
       xxPrt = '1';
       Chain orKy POR01;
       If %found;
          Except Rout1;
       Endif;
       xxPrt = '2';
       Chain orKy POR01;
       If %found;
          Except Rout2;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*****************************************************************
     C*    SRCSEL - Select the Claim for Closing
     C*****************************************************************
     C     SRCSEL        BEGSR
     C                   MOVE      *OFF          *IN60
     C                   MOVE      *OFF          XXSELF                            Sel. flag
     C*________________________________________________________________
     C*  CHECK FOR RECENT ACTIVITY IN THE RESERVE HISTORY FILE
     C*
     C     TRKY          SETLL     PRH01
     C     TRKY2         READE     PRH01                                  60
     C     *IN60         CABEQ     *OFF          ENDSEL                            Found one
     C                   MOVE      *OFF          *IN60
     C*
     C*________________________________________________________________
     C*  CHECK FOR RECENT ACTIVITY IN THE REQUEST PAYMENT FILE
     C*
     C     TRKY          SETLL     PRP01
     C     TRKY2         READE     PRP01                                  60
     C     *IN60         CABEQ     *OFF          ENDSEL                            Found one
     C                   MOVE      *OFF          *IN60
     C*
     C*________________________________________________________________
     C*  CHECK FOR RECENT ACTIVITY IN THE CLAIMS CHECK PAYMENT FILE
     C     TRKY          SETLL     PPA01
     C     TRKY2         READE     PPA01                                  60
     C     *IN60         CABEQ     *OFF          ENDSEL                            Found one
     C                   MOVE      *OFF          *IN60
     C*** BEGIN ADD *** 09/11/09 **************************************
     C*
     C*________________________________________________________________
     C*  IF THIS IS AN INDEMNITY CLAIM CHECK TO SEE IF THERE ARE ANY
     C*  COMPENSATION PAYMENTS, IF NOT THEN KEEP FROM CLOSING.
     C     CMLTFL        IFEQ      'I'
     C     CLTID         ANDEQ     '133'
     C                   CALL      'WA723R'
     C                   PARM      CMCO#         $$CO#             3 0
     C                   PARM      CMFND         $$FND             3 0
     C                   PARM      CMFYR         $$FYR             3 0
     C                   PARM      CMCASE        $$CASE            7 0
     C                   PARM      *BLANKS       $$RTCD            1
     C     $$RTCD        CABEQ     'N'           ENDSEL                            Found none
     C                   ENDIF
     C*** END   ADD *** 09/11/09 **************************************
     C*
     C*================================================================
     C*  This Claim has been Selected
     C                   MOVE      *ON           XXSELF                            Sel. flag
     C*================================================================
     C*
     C*________________________________________________________________
     C*  GET CLAIMANT DEMOGRAPHICS FILE
     C     CDKY          CHAIN     PCD01                              60
     C*
     C                   MOVEL     CDFSN         WKNAME            1
     C     CDLSN         CAT(P)    ',':0         CNAME            15
     C     CNAME         CAT       WKNAME:1      CNAME
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        CNAME
     C                   ENDIF                                                    *IN60 = 1
     C*________________________________________________________________
     C*  CHECK FOR A PAYMENT REQUEST WITH STATUS P OR ' '
     C     PAKY          CHAIN     PRP01                              65
     C                   MOVE      *OFF          XXPARQ                            Clear
     C     *IN65         DOWEQ     *OFF
     C*
     C     RPSTAT        IFEQ      'P'
     C     RPSTAT        OREQ      *BLANK
     C                   MOVE      *ON           XXPARQ                            RQ Exists
     C                   LEAVE
     C                   ENDIF                                                    e STAT=P,b
     C     PAKY          READE     PRP01                                  65
     C                   ENDDO                                                    *IN65 OFF
     C*
     C     ENDSEL        ENDSR
     C*
     C*****************************************************************
     C*    SRCLOSE - Close the claim
     C*****************************************************************
     C     SRCLOS        BEGSR
     C*
     C*________________________________________________________________
     C*  Check for Active DARTS Remarks
     C                   EXSR      SRACTD
     C*________________________________________________________________
     C*  Accumulate Claim Totals and Print Detail
     C                   EXSR      SRACPR
     C*________________________________________________________________
     C*________________________________________________________________
     C*  MOVE FIELDS FOR RESERVE HISTORY AND WRITE THE RECORD
     C                   MOVE      XXCO#         RHCO#
     C                   MOVE      XXFND         RHFND
     C                   Z-ADD     XXFYR         RHFYR
     C                   MOVE      XXCASE        RHCASE
     C*** BEGIN CHG *** 06/12/00 **************************************
     C******************** Z-ADDXXCUR7    RHEFDT
     C*** Begin dlt *** 08/17/01 **************************************
     C***************      Z-ADDXXDATE    RHEFDT
     C*** End dlt ***** 08/17/01 **************************************
     C*** END CHG ***** 06/12/00 **************************************
     C*** Begin add *** 08/17/01 **************************************
     C*** Begin dlt *** 11/03/05 **************************************
     C*******************  Z-ADDXXCUR7    RHEFDT
     C*** End dlt ***** 11/03/05 **************************************
     C*** End add ***** 08/17/01 **************************************
     C*** Begin add *** 11/03/05 **************************************
     C                   Z-ADD     XXDATE        RHEFDT
     C                   Z-ADD     235500        RHTIME
     C*** End add ***** 11/03/05 **************************************
     C                   MOVEL     MSG           RHEXPL
     C                   MOVE      *BLANKS       RHSTAT
     C                   MOVE      *BLANKS       RHCLST
     C                   MOVE      Q1USER        RHUSER
     C                   MOVE      Q1PGM         RHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXCUR7        RHDT                           Today CYYMMDD
     C*** Begin dlt *** 11/03/05 **************************************
     C*******************  Z-ADDDSTIME    RHTIME
     C*** End dlt ***** 11/03/05 **************************************
     C*
     C*** BEG ADD ***** 03/03/10 **************************************
     C                   MOVEL     XXCUR7        RHIDT
      *
     C                   MOVE      Q1USER        RHIUSR
     C                   MOVE      Q1PGM         RHIPGM
      *
     C                   Z-ADD     DSTIME        RHITIM
     C*** END ADD ***** 03/03/10 **************************************
      //***  Begin Add  ***  08/15/17  ************************************
      /Free
       RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);
      /End-free
      //***  End   Add  ***  08/15/17  ************************************
     C*
     C*** BEGIN ADD *** 08/10/11 **************************************
     C*
     C* Don't write reserve history record if this is a record only clm
     C*
     C     CMLTFL        IFNE      'R'
     C*** END   ADD *** 08/10/11 **************************************
     C                   WRITE     PRH01
     C*** BEGIN ADD *** 08/10/11 **************************************
     C                   ENDIF
     C*** END   ADD *** 08/10/11 **************************************
     C*________________________________________________________________
     C*  MOVE FIELDS FOR STATUS CHANGE HISTORY AND WRITE THE RECORD
     C                   MOVE      XXCO#         CHCO#
     C                   MOVE      XXFND         CHFND
     C                   Z-ADD     XXFYR         CHFYR
     C                   MOVE      XXCASE        CHCASE
     C*** BEGIN CHG *** 06/12/00 **************************************
     C******************** Z-ADDXXCUR7    CHEFDT
     C*** Begin dlt *** 08/17/01 **************************************
     C***************      Z-ADDXXDATE    CHEFDT
     C*** End dlt ***** 08/17/01 **************************************
     C*** END CHG ***** 06/12/00 **************************************
     C*** Begin add *** 08/17/01 **************************************
     C*** Begin dlt *** 11/03/05 **************************************
     C*******************  Z-ADDXXCUR7    CHEFDT
     C*** End dlt ***** 11/03/05 **************************************
     C*** End add ***** 08/17/01 **************************************
     C*** Begin add *** 11/03/05 **************************************
     C                   Z-ADD     XXDATE        CHEFDT
     C                   Z-ADD     235500        CHTIME
     C*** End add ***** 11/03/05 **************************************
     C                   MOVEL     MSG           CHEXPL
     C                   MOVE      'C'           CHOCST
     C                   MOVE      *BLANK        CHSTAT
     C                   MOVE      Q1USER        CHUSER
     C                   MOVE      Q1PGM         CHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXCUR7        CHDT                           Today CYYMMDD
     C*** Begin dlt *** 11/03/05 **************************************
     C*******************  Z-ADDDSTIME    CHTIME
     C*** End dlt ***** 11/03/05 **************************************
     C*
     C                   WRITE     PCH01
     C*________________________________________________________________
     C*  UPDATE THE CLAIM STATUS TO CLOSED
     C                   MOVE      'C'           CMSTAT
     C                   MOVE      Q1USER        CMUSER
     C                   MOVE      Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C*** Begin chg *** 11/03/05 **************************************
     C******************** Z-ADDXXCUR7    CMDT             Today CYYMMDD
     C                   Z-ADD     XXDATE        CMDT
     C*** End chg ***** 11/03/05 **************************************
     C*
     C                   UPDATE    PCM01
      /free
       //******* beg add ******** 12/01/14 *********************************
       // When a claim is closed, delete the pending claim reserve worksheet
       // if any exists.
       Chain (CMco#:CMfnd:CMfyr:CMcase) prw00;
       If %found and RWstat = 'P';
          Delete prw00;
       Endif;
       Unlock WDRWL;
       //******* end add ******** 12/01/14 *********************************
      /end-free
     C*
     C     ENDCLO        ENDSR
     C*
     C*****************************************************************
     C*    SRXCPT - Print Exception Report
     C*****************************************************************
     C     SRXCPT        BEGSR
     C*
     C*________________________________________________________________
     C*  Check for Active DARTS Remarks
     C                   EXSR      SRACTD
     C*________________________________________________________________
     C*  FORMAT PRINT OUTPUT
     C*
     C                   ADD       1             FNCASX            5 0
     C                   ADD       1             LRCASX            5 0
     C*
     C                   EXSR      SRMOVP
     C*________________________________________________________________
     C*  PRINT THE RECORD
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD1
     C                   MOVE      *OFF          *INOA
     C                   ENDIF                                                    *INOA = ON
     C                   EXCEPT    DET01
     C*
     C*
     C     ENDXCP        ENDSR
     C*
     C*****************************************************************
     C*    SRDIRY - Write Adjuster Diary
     C*****************************************************************
     C     SRDIRY        BEGSR
     C*
     C*________________________________________________________________
     C*       Accumulate totals and Print detail
     C                   EXSR      SRACPD
     C*________________________________________________________________
     C*       Build Key field
     C                   MOVE      *BLANKS       XCKEY
     C                   Z-ADD     XXCO#         XCCO#
     C                   Z-ADD     XXFND         XCFND
     C                   Z-ADD     XXFYR         XCFYR
     C                   Z-ADD     XXCASE        XCCASE
     C*
     C*** BEGIN ADD *** 05/21/98 ***********************************************
     C*________________________________________________________________
     C*  Determine if the diary has already been sent.
     C*
     C     PAKY          SETGT     PCH02
     C     PAKY          READPE    PCH02                                  60
     C   60              Z-ADD     0             CHEFDT
     C*
     C*  If a diary already exists, do not create another one.
     C*
     C     C4KY          CHAIN     PCM04                              60
     C     *IN60         IFEQ      *OFF
     C     CMCDAT        ANDGE     CHEFDT
     C                   GOTO      ENDIRY
     C                   ENDIF
     C*
      ****** BEG DEL *** 04/26/00 *************************************
     C*********            CLEARPCM02
     C****END*ADD ***** 05/21/98 ***********************************************
     C*********            MOVELXCKEY     CMKEY
     C*********            MOVELXXRMK     CMRMCD
     C*________________________________________________________________
     C********Retrieve Adjuster User ID
     C*********            MOVE *BLANKS   CMUSER
     C*********  CMCLEX    CHAINPUP01                60
     C****BEGIN ADD *** 05/21/98 **************************************
     C***60****            GOTO ENDIRY
     C****END*ADD ***** 05/21/98 **************************************
     C*********            MOVELUPUSR     CMUSER
     C*________________________________________________________________
     C********Retrieve DARTS Remark Type
     C*********  CMRMCD    CHAINPCT01                60
     C****BEGIN ADD *** 05/21/98 **************************************
     C***60****            GOTO ENDIRY
     C****END*ADD ***** 05/21/98 **************************************
     C*********  *IN60     IFEQ *OFF
     C*********
     C****BEGIN ADD *** 02/23/98 ***********************************************
     C*********            MOVE CTSMCD    CMSMCD
     C****END** ADD *** 02/23/98 ***********************************************
     C*********
     C*********            MOVELCTDESC    CMCOMM
     C*********            MOVELCTDEPG    CMDCID
     C*********  CMDCID    IFNE *BLANKS
     C*********            MOVELCMKEY     CMFOLD
     C*********            MOVE 'Y'       CMFL01
     C*********            ENDIF
     C*********  CTDPRG    IFGT *ZEROS
     C*********            MOVE 'Y'       CMFL02
     C*********            ENDIF
     C*********            MOVE CTFL01    CMDIFL
     C*********            MOVE CTFL02    CMFL03
     C*********            ENDIF
     C*________________________________________________________________
     C********Move data to record fields
     C*********            MOVE *BLANKS   CMCSID
     C*********            TIME           CMTIME
     C****BEGIN CHG *** 06/12/00 **************************************
     C******************** Z-ADDXXCUR7    CMCDAT
     C*********            Z-ADDXXDATE    CMCDAT
     C****END*CHG ***** 06/12/00 **************************************
     C*********            MOVE *BLANKS   CMCODE
     C*********
     C****BEGIN CHG *** 02/23/98 *******************************************
     C**********           MOVEL'W8608R ' CMPNAM
     C*********            MOVEL'W0070R'  CMPNAM
     C****END** CHG *** 02/23/98 *******************************************
     C*********
     C*********            Z-ADDXXCUR7    CMTRDT           Today CYYMMDD
     C*********            TIME           CMTRTM
     C*********            MOVE *ZEROS    CMSECL
     C*********            MOVE 'N'       CMSBYL
     C*********            MOVE 'A'       CMSTAT
     C*********
     C*********            MOVELQ1PGM     CMPGM
     C*********            Z-ADD1         CMTIME
     C*********            Z-ADDXXCUR7    CMDT
     C*********            MOVE Q1USER    CMIUSR
     C*********            MOVELQ1PGM     CMIPGM
     C*********            Z-ADDXXCUR7    CMIDT
     C****BEGIN DLT *** 05/21/98 ***********************************************
     C******************** MOVE *BLANKS   CMFOLD
     C******************** MOVE *ZEROS    CMDCDT
     C******************** MOVE *BLANKS   CMAPID
     C******************** MOVE *BLANKS   CMIDOC
     C******************** MOVE *BLANKS   CMIQUE
     C******************** MOVE *BLANKS   CMIMGF
     C******************** MOVE *BLANKS   CMIPTH
     C******************** MOVE *ZEROS    CMPIDX
     C******************** MOVE *BLANKS   CMQPTY
     C******************** MOVE *BLANKS   CMDCTP
     C******************** MOVE *BLANKS   CMFL04
     C******************** MOVE *BLANKS   CMFL05
     C****END*DLT ***** 05/21/98 ***********************************************
     C*********
     C****BEGIN DEL *** 02/23/98 ***********************************************
     C**********           MOVE *BLANKS   CMSMCD
     C****END** DEL *** 02/23/98 ***********************************************
     C*********
     C*________________________________________________________________
     C***GET*THE NEXT AVAILABLE SEQ# AND WRITE THE RECORD
     C*********  XCKEY     SETGTPCM03
     C*********  XCKEY     REDPEPCM03                    50
     C*********  *IN50     IFEQ *OFF
     C*********            ADD  1         CMSEQ#
     C*********            ELSE
     C*********            Z-ADD1         CMSEQ#
     C*********            ENDIF
     C*********
     C*********            WRITEPCM02
     C******BEGIN ADD **** 07/01/99 ***********************************
     C*********            FEOD SDCML
     C******END   ADD **** 07/01/99 ***********************************
      *********
      ****** END DEL *** 04/26/00 *************************************
      ****** BEG ADD *** 04/26/00 *************************************
      *
      *  ESTABLISH KEY
      *
     C                   MOVEL(P)  XCKEY         Q#KEY            40
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      '000'         Q#SECL            3
     C                   MOVEL(P)  'N'           Q#SBYL            1
      *
      *       Retrieve Adjuster User ID
      *
     C                   MOVE      *BLANKS       Q#USER
     C     CMCLEX        CHAIN     PUP01                              60
     C   60              GOTO      ENDIRY
     C                   MOVEL(P)  UPUSR         Q#USER           10
      *
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZERO         Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
     C                   MOVEL(P)  XXRMK         Q#RMCD            5
     C                   MOVEL(P)  *BLANKS       Q#DCID           12
     C                   MOVE      *BLANKS       Q#FOLD           80
     C                   MOVE      *BLANK        Q#DIFL            1
     C                   MOVE      *BLANKS       Q#FL01            1
     C                   MOVE      *BLANKS       Q#FL02            1
     C                   MOVE(P)   QQDATE        Q#CDAT            8
     C                   MOVE(P)   *BLANKS       Q#COM2           19
     C                   MOVEL(P)  *BLANKS       Q#COMM          256
      *
      *       Retrieve DARTS Remark Type
      *
     C     Q#RMCD        CHAIN     PCT01                              60
     C   60              GOTO      ENDIRY
     C                   MOVEL(P)  CTDEPG        Q#DCID
      *
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  XCKEY         Q#FOLD
     C                   MOVE      'Y'           Q#FL01
     C                   ENDIF
      *
     C     CTDPRG        IFGT      *ZEROS
     C                   MOVE      'Y'           Q#FL02
     C                   ENDIF
      *
     C                   MOVE      CTFL01        Q#DIFL
      *
      * ZZZ
      *
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
      *
      ****** END ADD *** 04/26/00 *************************************
     C*
     C*** BEGIN ADD *** 05/21/98 **************************************
     C     ENDIRY        TAG
     C*** END ADD ***** 05/21/98 **************************************
     C*
     C                   ENDSR
     C*****************************************************************
     C*  CALCULATE CLAIM TOTALS AND PRINT DETAIL -- CLOSURES
     C*****************************************************************
     C     SRACPR        BEGSR
     C*================================================================
     C*
     C*       Accumulate payments and remaining reserves of the claim
     C*   into arrays CP and CR elements 1-3: compensation, medical
     C*   and other, respectively.  Crossfoot each array into its last
     C*   element for a line total.  Add both arrays to the array TOT;
     C*   Each array is 1 print line of financial data on the claim:
     C*   "Total incurred," "Payments," and "Remaining."
     C*
     C*       Add the two arrays to CFP and CFR, and CTP and CTR,
     C*   respectively, to accumulate group and report totals.  Use the
     C*   same TOT array for "Total incurred" when printing the totals.
     C*
     C*________________________________________________________________
     C*  INITIALIZE CLAIM TOTALS AND RESERVE HISTORY ARRAYS
     C                   Z-ADD     *ZERO         CP
     C                   Z-ADD     *ZERO         CR
     C                   Z-ADD     *ZERO         TOT
     C                   CLEAR                   RMT
     C                   CLEAR                   RMR
     C*________________________________________________________________
     C*  Accumulate Claim Reserves and Payments
     C*________________________________________________________________
     C                   DO        10            R                 2 0
     C                   MOVEL     CMT(R)        XXRES
     C*
     C*  Get Reserve Type code -- set array pointer S
     C     TBKY          CHAIN     PRT01                              60
     C*
     C                   SELECT
     C     RTTYPE        WHENEQ    'C'
     C                   Z-ADD     1             S                 2 0
     C     RTTYPE        WHENEQ    'M'
     C                   Z-ADD     2             S
     C                   OTHER
     C                   Z-ADD     3             S
     C                   ENDSL
     C*________________________________________________________________
     C*  Assumulate Claim Payment and Reserves
     C                   ADD       CMA(R)        CP(S)
     C                   ADD       CMR(R)        CR(S)
     C*________________________________________________________________
     C*   Load Reserve History arrays; zero reserves.
     C                   MOVE      CMT(R)        RMT(R)
     C                   Z-SUB     CMR(R)        RMR(R)
     C                   Z-ADD     *ZERO         CMR(R)
     C*
     C                   ENDDO                                                     e DO 10
     C*
     C*  Calculate Line Totals
     C                   XFOOT     CP            CP(4)
     C                   XFOOT     CR            CR(4)
     C*
     C*  Calculate Total Incurred
     C                   ADD       CP            TOT
     C                   ADD       CR            TOT
     C*
     C*    Tally the count
     C                   ADD       1             FNCASE            5 0
     C                   ADD       1             LRCASE            5 0
     C*
     C*________________________________________________________________
     C*  Accumulate Group and Final Totals
     C                   ADD       CP            CFP
     C                   ADD       CR            CFR
     C*
     C                   ADD       CP            CTP                             Automatic
     C                   ADD       CR            CTR                                close
     C*________________________________________________________________
     C*  FORMAT PRINT OUTPUT
     C*
     C                   EXSR      SRMOVP
     C*________________________________________________________________
     C*  PRINT THE RECORD
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF                                                    *INOF = ON
     C                   EXCEPT    DETAIL
     C*
     C     ENACPR        ENDSR
     C*
     C*****************************************************************
     C*  CALCULATE CLAIM TOTALS AND PRINT DETAIL -- DIARIES
     C*****************************************************************
     C     SRACPD        BEGSR
     C*================================================================
     C*
     C*       Accumulate payments and remaining reserves of the claim
     C*   into arrays DCP and DCR elements 1-3: compensation, medical
     C*   and other, respectively.  Crossfoot each array into its last
     C*   element for a line total.  Add both arrays to the array TOT.
     C*
     C*   Each array is 1 print line of financial data on the claim:
     C*   "Total Incurred," "Payments,"  and "Reserve Remaining."
     C*
     C*       Add the two arrays to DFP and DFR, and DTP and DTR,
     C*   respectively, to accumulate group and report totals.  (Re-use
     C*   same TOT array for Total Incurred when printing the totals).
     C*
     C*________________________________________________________________
     C*  INITIALIZE CLAIM TOTALS AND RESERVE HISTORY ARRAYS
     C                   Z-ADD     *ZERO         CP
     C                   Z-ADD     *ZERO         CR
     C                   Z-ADD     *ZERO         TOT
     C                   CLEAR                   RMT
     C                   CLEAR                   RMR
     C*________________________________________________________________
     C*  Accumulate Claim Reserves and Payments
     C*________________________________________________________________
     C                   DO        10            R                 2 0
     C                   MOVEL     CMT(R)        XXRES
     C*
     C*  Get Reserve Type code -- set array pointer S
     C     TBKY          CHAIN     PRT01                              60
     C*
     C                   SELECT
     C     RTTYPE        WHENEQ    'C'
     C                   Z-ADD     1             S                 2 0
     C     RTTYPE        WHENEQ    'M'
     C                   Z-ADD     2             S
     C                   OTHER
     C                   Z-ADD     3             S
     C                   ENDSL
     C*________________________________________________________________
     C*  Assumulate Claim Payment and Reserves
     C                   ADD       CMA(R)        CP(S)
     C                   ADD       CMR(R)        CR(S)
     C*
     C                   ENDDO                                                     e DO 10
     C*
     C*  Calculate Line Totals
     C                   XFOOT     CP            CP(4)
     C                   XFOOT     CR            CR(4)
     C*
     C*  Calculate Total Incurred
     C                   ADD       CP            TOT
     C                   ADD       CR            TOT
     C*
     C*    Tally the count
     C                   ADD       1             FNCASD            5 0
     C                   ADD       1             LRCASD            5 0
     C*
     C*________________________________________________________________
     C*  Accumulate Group and Final Totals
     C                   ADD       CP            DFP
     C                   ADD       CR            DFR
     C*
     C                   ADD       CP            DTP                             Diaries to
     C                   ADD       CR            DTR                                close
     C*________________________________________________________________
     C*  FORMAT PRINT OUTPUT
     C*
     C                   EXSR      SRMOVP
     C*________________________________________________________________
     C*  PRINT THE RECORD
     C*
     C     *INOB         IFEQ      *ON
     C                   EXCEPT    HEAD2
     C                   MOVE      *OFF          *INOB
     C                   ENDIF                                                    *INOB = ON
     C                   EXCEPT    DET02
     C*
     C     ENACPD        ENDSR
     C*
     C*****************************************************************
     C*    SRMOVP - Move fields to  print
     C*****************************************************************
     C     SRMOVP        BEGSR
     C*________________________________________________________________
     C*  FORMAT PRINT OUTPUT
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ACCDTE
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         RECDTE
     C*
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   MOVEL     XXSS#         SS4               4
     C                   MOVE      '-'           SS4
     C                   MOVE      XXSS#         SS6               6
     C                   MOVEL     SS6           SS2               2
     C                   MOVE      SS6           SS5               5
     C                   MOVEL     '-'           SS5
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C*
     C     ENMOVP        ENDSR
     C*
     C*****************************************************************
     C*    SRACTD - Discover if active DARTS remark exists on claim
     C*****************************************************************
     C     SRACTD        BEGSR
     C*________________________________________________________________
     C*       Build Key field
     C*________________________________________________________________
     C                   MOVE      *BLANK        XXDART
     C                   MOVE      *BLANKS       XCKEY
     C                   Z-ADD     XXCO#         XCCO#
     C                   Z-ADD     XXFND         XCFND
     C                   Z-ADD     XXFYR         XCFYR
     C                   Z-ADD     XXCASE        XCCASE
     C*
     C     XCKEY         CHAIN     PCM04                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     CMDCDT        IFEQ      0
     C                   MOVE      '*'           XXDART
     C                   LEAVE
     C                   END
     C*
     C     XCKEY         READE     PCM04                                  60
     C                   ENDDO
     C*
     C*
     C     ENDACT        ENDSR
     C*
     C*****************************************************************
     C*    SRDATE - Calculate inactivity start dates for group
     C*****************************************************************
     C     SRDATE        BEGSR
     C                   Z-ADD     *ZEROS        XXMEDT                           clear
     C                   Z-ADD     *ZEROS        XXINDT                            work
     C                   Z-ADD     *ZEROS        XXSTDT                             fields
     C*________________________________________________________________
     C*   Inactivity start for medical only -- XXMEDT
     C     CXMOC         IFEQ      'Y'
     C     CXNFAM        ANDGT     *ZERO
     C*** BEGIN CHG *** 06/12/00 **************************************
     C******************** Z-ADDCXNFAM    SRPST
     C     CXNFAM        SUB       1             SRPST
     C*** END CHG ***** 06/12/00 **************************************
     C                   MOVE      X1DATE        SRDTE
     C                   MOVE      *BLANK        SRERR
     C                   MOVE      *BLANK        SRTERM
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE             8 0
     C                   PARM                    SRPST             5 0
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
     C                   MOVEL     SRDTE         X1MEDT
     C*** BEGIN DLT *** 06/12/00 **************************************
     C******************** MOVELSRDTE     XXDATE
     C******************** Z-ADDXXDATE    SRDTI
     C*** END DLT ***** 06/12/00 **************************************
     C*** BEGIN ADD *** 06/12/00 **************************************
     C                   Z-ADD     X1MEDT        SRDTI
     C*** END ADD ***** 06/12/00 **************************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXMEDT
     C                   ENDIF
     C*________________________________________________________________
     C*   Inactivity start for indemnity -- XXINDT
     C     CXINC         IFEQ      'Y'
     C     CXNFAI        ANDGT     *ZERO
     C                   Z-ADD     CXNFAI        SRPST
     C*** BEGIN CHG *** 06/12/00 **************************************
     C******************** Z-ADDCXNFAI    SRPST
     C     CXNFAI        SUB       1             SRPST
     C*** END CHG ***** 06/12/00 **************************************
     C                   MOVE      X1DATE        SRDTE
     C                   MOVE      *BLANK        SRERR
     C                   MOVE      *BLANK        SRTERM
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C                   MOVEL     SRDTE         X1INDT
     C*** BEGIN DLT *** 06/12/00 **************************************
     C******************** MOVELSRDTE     XXDATE
     C******************** Z-ADDXXDATE    SRDTI
     C*** END DLT ***** 06/12/00 **************************************
     C*** BEGIN ADD *** 06/12/00 **************************************
     C                   Z-ADD     X1INDT        SRDTI
     C*** END ADD ***** 06/12/00 **************************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXINDT
     C                   ENDIF
     C*
     C     ENDDAT        ENDSR
     C*
     C*****************************************************************
     C*    SRFPRT - Print group totals
     C*****************************************************************
     C     SRFPRT        BEGSR
     C*
     C*
     C*      Closure totals
     C     FNCASE        IFNE      *ZERO                                          Got some
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   ENDIF
     C*
     C*      Calculate Group Total Incurred
     C                   Z-ADD     *ZEROS        TOT
     C                   ADD       CFP           TOT
     C                   ADD       CFR           TOT
     C*      Print
     C                   EXCEPT    FNDTOT
     C*
     C*      Clear Group Totals
     C                   Z-ADD     *ZEROS        CFP
     C                   Z-ADD     *ZEROS        CFR
     C                   Z-ADD     *ZEROS        FNCASE
     C                   MOVE      *ON           *INOF
     C                   ENDIF                                                    FNCASE n 0
     C*
     C*      Diary totals
     C     FNCASD        IFNE      *ZERO                                          Got some
     C*
     C*      Calculate Fund Total Incurred
     C                   Z-ADD     *ZEROS        TOT
     C                   ADD       DFP           TOT
     C                   ADD       DFR           TOT
     C*
     C     *INOB         IFEQ      *ON
     C                   EXCEPT    HEAD2
     C                   ENDIF
     C*
     C*      Print
     C                   EXCEPT    FNDTO2
     C*
     C*      Clear accumulators
     C                   Z-ADD     *ZEROS        DFP
     C                   Z-ADD     *ZEROS        DFR
     C*
     C                   Z-ADD     *ZEROS        FNCASD
     C                   MOVE      *ON           *INOB
     C                   ENDIF                                                    FNCASD n 0
     C*
     C*      Exception totals
     C     FNCASX        IFNE      *ZERO                                          Got some
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD1
     C                   ENDIF                                                    *INOA = ON
     C*
     C                   EXCEPT    FNDTO1
     C                   Z-ADD     *ZEROS        FNCASX
     C                   MOVE      *ON           *INOA
     C                   ENDIF                                                    FNCASX n 0
     C*
     C     ENDFPR        ENDSR
     C*
     C*****************************************************************
     C*    SRFTOT - Print final totals
     C*****************************************************************
     C     SRFTOT        BEGSR
     C*
     C*     First Print Residual Group Totals
     C                   EXSR      SRFPRT
     C*
     C*     Print Closure Totals
     C     LRCASE        IFNE      *ZERO
     C*
     C*      Calculate Total Incurred and print Auto Closed
     C                   Z-ADD     *ZEROS        TOT
     C                   ADD       CTP           TOT
     C                   ADD       CTR           TOT
     C                   EXCEPT    TOTALS
     C*
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN45
     C                   EXCEPT    HEAD
     C                   EXCEPT    NOREC
     C*
     C                   ENDIF                                                   LRCASE ne 0
     C*
     C*     Print final totals for diaries
     C     LRCASD        IFNE      *ZERO
     C     *INOB         IFEQ      *ON
     C                   EXCEPT    HEAD2
     C                   MOVE      *OFF          *INOB
     C                   ENDIF                                                   *INOB = ON
     C*
     C*      Calculate Total Incurred and print Diaried
     C                   Z-ADD     *ZEROS        TOT
     C                   ADD       DTP           TOT
     C                   ADD       DTR           TOT
     C                   EXCEPT    TOTAL2
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN47
     C                   EXCEPT    HEAD2
     C                   EXCEPT    NOREC2
     C*
     C                   ENDIF                                                   LRCASD ne 0
     C*
     C*     Print final totals for exceptions
     C     LRCASX        IFNE      *ZERO
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD1
     C                   MOVE      *OFF          *INOA
     C                   ENDIF                                                   *INOA = ON
     C                   EXCEPT    TOTAL1
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN46
     C                   EXCEPT    HEAD1
     C                   EXCEPT    NOREC1
     C*
     C                   ENDIF                                                   LRCASX ne 0
     C*
     C     ENDTOT        ENDSR
     C*
     C*****************************************************************
     C*    *INZSR - Program Initialization
     C*****************************************************************
     C     *INZSR        BEGSR
     C*
     C*================================================================
     C*     PARAMETER LISTS
     C*================================================================
     C*________________________________________________________________
     C     *ENTRY        PLIST
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
     C                   PARM                    QQRTN             1
     C*
     C*================================================================
     C*________________________________________________________________
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*________________________________________________________________
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*________________________________________________________________
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*================================================================
     C*     KEY LISTS
     C*================================================================
     C*________________________________________________________________
     C*  CLAIM/DARTS CONTROLS
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C*________________________________________________________________
     C*   COMPANY FILE
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*________________________________________________________________
     C*   GROUP MASTER
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*________________________________________________________________
     C*  CLAIM MASTER
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*________________________________________________________________
     C*  TRANSACTION ACTIVITY FILES SET
     C     TRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXSTDT
     C*________________________________________________________________
     C*  TRANSACTION ACTIVITY FILES READ
     C     TRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*________________________________________________________________
     C*  PAYMENT ACTIVITY FILE
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*________________________________________________________________
     C*   CLAIMANT DEMOGRAPHIC FILE
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
     C*________________________________________________________________
     C*   TABLE FILE
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRES            10
     C*________________________________________________________________
     C*   OVERRIDE FILE
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*** BEGIN ADD *** 05/21/98 **************************************
     C*________________________________________________________________
     C*  DARTS file (SDCMLF)
     C*
     C     C4KY          KLIST
     C                   KFLD                    XCKEY
     C                   KFLD                    XXRMK
     C*** END ADD ***** 05/21/98 **************************************
     C*
     C*================================================================
     C*     FIELD DEFINITIONS
     C*================================================================
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C*
     C     *LIKE         DEFINE    CMACDT        XXMEDT
     C     *LIKE         DEFINE    CMACDT        XXINDT
     C     *LIKE         DEFINE    CMACDT        XXSTDT
     C*
     C     *LIKE         DEFINE    CXADRM        XXRMK
     C     *LIKE         DEFINE    CXNFAM        XXDAYS
     C*
     C     *LIKE         DEFINE    CMACDT        XXCUR7                         Today CYYMMDD
     C     *LIKE         DEFINE    *IN60         XXSELF                         ON = selectd
     C     *LIKE         DEFINE    *IN60         XXPARQ                         ON = exists
     C*
     C     *LIKE         DEFINE    ORTITL        XXTITL                                        SRINI
     C     *LIKE         DEFINE    ORTITL        X1TITL                                        SRINI
     C     *LIKE         DEFINE    ORTITL        X2TITL                                        SRINI
     C*
     C                   Z-ADD     *ZEROS        ACCDTE            8 0
     C                   Z-ADD     *ZEROS        RECDTE            8 0
     C*
     C                   Z-ADD     *ZEROS        X1MEDT            8 0
     C                   Z-ADD     *ZEROS        X1INDT            8 0
     C                   Z-ADD     *ZEROS        X1DATE            8 0
     C*** BEGIN CHG *** 06/12/00 **************************************
     C******************** Z-ADD*ZEROS    XXDATE  80
     C                   Z-ADD     *ZEROS        XXDATE            7 0
     C*** END CHG ***** 06/12/00 **************************************
     C                   MOVE      'N'           XXCONT            1
     C                   MOVE      *BLANK        XXDART            1
     C*
     C                   MOVE      *ZEROS        XXFND
     C*================================================================
     C*     PREPARATION
     C*================================================================
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQDATE        X1DATE
     C*________________________________________________________________
     C*  SET DATES
     C                   TIME                    DSDTTM
     C*________________________________________________________________
     C*   Calculate 7-digit Julian for today's date  -- CYYMMDD
     C                   Z-ADD     UDATE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCUR7
     C*
     C*   Calculate 7-digit Julian for batch date  -- CYYMMDD
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*** BEGIN ADD *** 09/11/09 **************************************
     C*________________________________________________________________
     C*  GET CLIENT ID
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       CLTID             3
     C*** END   ADD *** 09/11/09 **************************************
     C*________________________________________________________________
     C*  GET THE COMPANY RECORD
     C     COKY          CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*________________________________________________________________
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     ORTITL        XXTITL                                        SRINI
      ***** Beg Del ***** 08/26/14 *************************************************
     C*********          EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *************************************************
     C                   ENDIF
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     ORTITL        X1TITL                                        SRINI
      ***** Beg Del ***** 08/26/14 *************************************************
     C**********         EXCEPT    ROUT1
      ***** End Del ***** 08/26/14 *************************************************
     C                   ENDIF
     C*
     C                   MOVE      '2'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     ORTITL        X2TITL                                        SRINI
      ***** Beg Del ***** 08/26/14 *************************************************
     C**********         EXCEPT    ROUT2
      ***** End Del ***** 08/26/14 *************************************************
     C                   ENDIF
     C*
     C                   CLEAR                   RMT
     C                   CLEAR                   RMR
     C                   ENDSR
     C*
     C*________________________________________________________________
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 *************************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
     O**********E            ROUTE          2
     O**********                                 37 'COPIES:'
     O**********             ORCOPY        Z     41
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
      ***** End Del ***** 08/26/14 *************************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS, INC.'
     O***                                52 ', 1997'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       X1DATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       XXTITL             101
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                              'GROUP:'
     O                 N45   XXFND         Z   +  1
     O                 N45   FNNAME            +  1
     O          E            HEAD           2
     O                                           38 'INACTIVE CLAIMS --'
     O                                           57 'MEDICAL SINCE:'
     O                       X1MEDT        Y     68
     O                                           89 'INDEMNITY SINCE:'
     O                       X1INDT        Y     99
     O                                          130 '* = Active DARTS remarks'
     O          E            HEAD           1
     O                                           14 'GRP YR    CASE'
     O                                           21 'D/A'
     O                                           33 'SS#'
     O                                           46 'CLAIMANT'
     O                                           60 'LT FLAG'
     O          E            HEAD           2
     O                                           83 'COMPENSATION'
     O                                           99 'MEDICAL'
     O                                          115 'OTHER'
     O                                          131 'TOTAL'
     O          E            DETAIL         1
     O                       CMFND         3      3
     O                       CMFYR         3      6
     O                       CMCASE              14
     O                                            4 '-'
     O                                            7 '-'
     O                       ACCDTE              25 '  /  /    '
     O                       SS4                 30
     O                       SS2                 32
     O                       SS5                 37
     O                       CNAME               53
     O                       CMLTFL              57
     O                       XXDART         B   107
     O*
     O          E            DETAIL         1
     O                                           68 'INCURRED:'
     O                       TOT(1)        J     84
     O                       TOT(2)        J    100
     O                       TOT(3)        J    116
     O                       TOT(4)        J    132
     O          E            DETAIL         1
     O                                           68 'PAYMENTS:'
     O                       CP(1)         J     84
     O                       CP(2)         J    100
     O                       CP(3)         J    116
     O                       CP(4)         J    132
     O          E            DETAIL         3
     O                                           68 'REMAINING:'
     O                       CR(1)         J     84
     O                       CR(2)         J    100
     O                       CR(3)         J    116
     O                       CR(4)         J    132
     O          E            FNDTOT      2  1
     O                                           15 'GROUP'
     O                       XXFND         Z     19
     O                                           38 'TOTAL CLAIM COUNT:'
     O                       FNCASE        1     44
     O                                           68 'TOTAL INCURRED:'
     O                       TOT(1)        J     84
     O                       TOT(2)        J    100
     O                       TOT(3)        J    116
     O                       TOT(4)        J    132
     O          E            FNDTOT         1
     O                                           68 'TOTAL PAYMENTS:'
     O                       CFP(1)        J     84
     O                       CFP(2)        J    100
     O                       CFP(3)        J    116
     O                       CFP(4)        J    132
     O          E            FNDTOT         2
     O                                           68 'TOTAL REMAINING:'
     O                       CFR(1)        J     84
     O                       CFR(2)        J    100
     O                       CFR(3)        J    116
     O                       CFR(4)        J    132
     O          E            TOTALS      2  1
     O                                           38 'TOTAL CLAIM COUNT:'
     O                       LRCASE        1     44
     O                                           68 'TOTAL INCURRED:'
     O                       TOT(1)        J     84
     O                       TOT(2)        J    100
     O                       TOT(3)        J    116
     O                       TOT(4)        J    132
     O          E            TOTALS         1
     O                                           68 'TOTAL PAYMENTS:'
     O                       CTP(1)        J     84
     O                       CTP(2)        J    100
     O                       CTP(3)        J    116
     O                       CTP(4)        J    132
     O          E            TOTALS         2
     O                                           68 'TOTAL REMAINING:'
     O                       CTR(1)        J     84
     O                       CTR(2)        J    100
     O                       CTR(3)        J    116
     O                       CTR(4)        J    132
     O          E            NOREC       2  3
     O                                           24 '** NO CLAIMS WERE SELECT'
     O                                           48 'ED FOR AUTO CLOSING **  '
     OQSYSPRT1  E            ROUT1          1 06
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUT1          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUT1          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUT1          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUT1          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUT1          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUT1          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUT1          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUT1          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUT1          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUT1          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUT1          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUT1          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              60
     O**********E            ROUT1          1
     O**********             ORDIS2              60
     O**********E            ROUT1          1
     O**********             ORDIS3              60
     O**********E            ROUT1          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 *************************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUT1          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUT1          1
     O                       ORDIS2              60
     O          E            ROUT1          1
     O                       ORDIS3              60
     O          E            ROUT1          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUT1          2
     O                                           19 'REPORT TITLE:'
     O                       X1TITL              80
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********E            ROUT1          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUT1          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *************************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUT1
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS, INC.'
     O***                                52 ', 1997'
     O          E            ROUT1          1 59
     O                       #BRND1
     O          E            ROUT1          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       X1DATE        Y    132
     O          E            HEAD1          2
     O                       Q1PGM               10
     O                       X1TITL              96
     O                                          126 'PAGE:'
     O                       PAGE1         Z    132
     O          E            HEAD1          2
     O                                              'GROUP:'
     O                 N46   XXFND         Z   +  1
     O                 N46   FNNAME            +  1
     O          E            HEAD1          3
     O                                           38 'INACTIVE CLAIMS --'
     O                                           57 'MEDICAL SINCE:'
     O                       X1MEDT        Y     68
     O                                           89 'INDEMNITY SINCE:'
     O                       X1INDT        Y     99
     O                                          130 '* = Active DARTS remarks'
     O          E            HEAD1          2
     O*
     O                                           14 'GRP YR    CASE'
     O                                           27 'D/A'
     O                                           47 'SS#'
     O                                           67 'CLAIMANT'
     O                                           91 'LT FLAG'
     O          E            DET01          1
     O                       CMFND         3      3
     O                       CMFYR         3      6
     O                       CMCASE              14
     O                                            4 '-'
     O                                            7 '-'
     O                       ACCDTE              31 '  /  /    '
     O                       SS4                 44
     O                       SS2                 46
     O                       SS5                 51
     O                       CNAME               74
     O                       CMLTFL              88
     O                       XXDART         B   107
     O*
     O          E            NOREC1      2  3
     O                                           24 '** NO CLAIMS WERE FOUND '
     O                                           48 'WITH PAYMENT REQUESTS **'
     O          E            FNDTO1      2  1
     O                                           15 'GROUP'
     O                       XXFND         Z     19
     O                                           38 'TOTAL CLAIM COUNT:'
     O                       FNCASX        1     44
     O          E            TOTAL1      2  1
     O                                           38 'TOTAL CLAIM COUNT:'
     O                       LRCASX        1     44
     C*________________________________________________________________
     OQSYSPRT2  E            ROUT2          1 06
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUT2          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUT2          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUT2          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUT2          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUT2          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUT2          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUT2          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUT2          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUT2          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUT2          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUT2          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUT2          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              60
     O**********E            ROUT2          1
     O**********             ORDIS2              60
     O**********E            ROUT2          1
     O**********             ORDIS3              60
     O**********E            ROUT2          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 *************************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUT2          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUT2          1
     O                       ORDIS2              60
     O          E            ROUT2          1
     O                       ORDIS3              60
     O          E            ROUT2          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUT2          2
     O                                           19 'REPORT TITLE:'
     O                       X2TITL              80
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********E            ROUT2          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUT2          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *************************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUT2
     O****                               23 '(C) COPYRIGHT PROGRESSI'
     O****                               46 'VE DATA SOLUTIONS, INC.'
     O****                               52 ', 1997'
     O          E            ROUT2          1 59
     O                       #BRND1
     O          E            ROUT2          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD2          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       X1DATE        Y    132
     O          E            HEAD2          2
     O                       Q1PGM               10
     O                       X2TITL             101
     O                                          126 'PAGE:'
     O                       PAGE2         Z    132
     O          E            HEAD2          2
     O                                              'GROUP:'
     O                 N47   XXFND         Z   +  1
     O                 N47   FNNAME            +  1
     O          E            HEAD2          3
     O                                           38 'INACTIVE CLAIMS --'
     O                                           57 'MEDICAL SINCE:'
     O                       X1MEDT        Y     68
     O                                           89 'INDEMNITY SINCE:'
     O                       X1INDT        Y     99
     O                                          130 '* = Active DARTS remarks'
     O          E            HEAD2          1
     O*
     O                                           14 'GRP YR    CASE'
     O                                           21 'D/A'
     O                                           33 'SS#'
     O                                           46 'CLAIMANT'
     O                                           60 'LT FLAG'
     O          E            HEAD2          2
     O                                           83 'COMPENSATION'
     O                                           99 'MEDICAL'
     O                                          115 'OTHER'
     O                                          131 'TOTAL'
     O          E            DET02          1
     O                       CMFND         3      3
     O                       CMFYR         3      6
     O                       CMCASE              14
     O                                            4 '-'
     O                                            7 '-'
     O                       ACCDTE              25 '  /  /    '
     O                       SS4                 30
     O                       SS2                 32
     O                       SS5                 37
     O                       CNAME               53
     O                       CMLTFL              57
     O                       XXDART         B   107
     O*
     O          E            DET02          1
     O                                           68 'INCURRED:'
     O                       TOT(1)        J     84
     O                       TOT(2)        J    100
     O                       TOT(3)        J    116
     O                       TOT(4)        J    132
     O          E            DET02          1
     O                                           68 'PAYMENTS:'
     O                       CP(1)         J     84
     O                       CP(2)         J    100
     O                       CP(3)         J    116
     O                       CP(4)         J    132
     O          E            DET02          3
     O                                           68 'REMAINING:'
     O                       CR(1)         J     84
     O                       CR(2)         J    100
     O                       CR(3)         J    116
     O                       CR(4)         J    132
     O          E            FNDTO2      2  1
     O                                           15 'GROUP'
     O                       XXFND         Z     19
     O                                           38 'TOTAL CLAIM COUNT:'
     O                       FNCASD        1     44
     O                                           68 'TOTAL INCURRED:'
     O                       TOT(1)        J     84
     O                       TOT(2)        J    100
     O                       TOT(3)        J    116
     O                       TOT(4)        J    132
     O          E            FNDTO2         1
     O                                           68 'TOTAL PAYMENTS:'
     O                       DFP(1)        J     84
     O                       DFP(2)        J    100
     O                       DFP(3)        J    116
     O                       DFP(4)        J    132
     O          E            FNDTO2         2
     O                                           68 'TOTAL REMAINING:'
     O                       DFR(1)        J     84
     O                       DFR(2)        J    100
     O                       DFR(3)        J    116
     O                       DFR(4)        J    132
     O          E            TOTAL2      2  1
     O                                           38 'TOTAL CLAIM COUNT:'
     O                       LRCASD        1     44
     O                                           68 'TOTAL INCURRED:'
     O                       TOT(1)        J     84
     O                       TOT(2)        J    100
     O                       TOT(3)        J    116
     O                       TOT(4)        J    132
     O          E            TOTAL2         1
     O                                           68 'TOTAL PAYMENTS:'
     O                       DTP(1)        J     84
     O                       DTP(2)        J    100
     O                       DTP(3)        J    116
     O                       DTP(4)        J    132
     O          E            TOTAL2         2
     O                                           68 'TOTAL REMAINING:'
     O                       DTR(1)        J     84
     O                       DTR(2)        J    100
     O                       DTR(3)        J    116
     O                       DTR(4)        J    132
     O          E            NOREC2      2  3
     O                                           24 '** NO CLAIMS WERE SELECT'
     O                                           46 'ED TO BE DIARIED  **  '
