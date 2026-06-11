     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H* ***************************************************************
     H/TITLE WA009R - FINANCIAL SUMMARY OF CLAIMANT
     H Bnddir('WTBNDDIR')
     H* ***************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A WINDOW CONTAINING  *
     H*                FINANCIAL INFO ABOUT A SPECIFIC CLAIM:         *
     H*            1.  RESERVE TYPE, OUTSTANDING RESERVES, PAID TO    *
     H*                DATE, & TOTAL INCURRED.                        *
     H*            2.  LAST COMP PAID INFO                            *
     H*            3.  SPECIFIC RETENTION (EXCESS REINSURANCE)        *
     H*            4.  DEDUCTIBLE INFO                                *
     H*            5.  F8 = DEDUCTIBLE HISTORY                        *
     H*                                                               *
     H*  CALLED FROM: W0070R06, 07, 08, 09, 10, 11, 12                *
     H*                                                               *
     H*  CALLS TO: WA010R - GET SPECIFIC RETENTION & FLAG             *
     H*                                                               *
     H*  CREATED: 06/17/97  PROGRAMMER: C:47  PROJECT: 60ANC          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*
     H* 11/25/13 C:282 PROJECT 129WSC                                 *
     H*                Replace legal literal with control field       *
     H*                for cltid 129 only.                            *
     H*                                                               *
     H* 05/28/13 C:287 Support: WA099 80909                           *
     H*                Add Large Deductible logic                     *
     H*                                                               *
     F* 01/28/10  C:249  Project: SY215                               *
     F*           C:189  Retrieve brand Name
     F*                                                               *
     H* 10/11/07 C:190 SUPPORT WA099 54504                            *
     H*                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE      *
     H*                DISCOUNT CODE HAVING INACTIVE RECORDS.         *
     H*                                                               *
     H*  05/25/07 C:191 PROJECT: WC1515                               *
     H*                 Check claims authority                        *
     H*                                                               *
     H*  04/10/06 C:65 PROJECT SUPP#51293                             *
     H*                CHANGE DEDUCTIBLE DISCOUNT TO PULL BASED ON    *
     H*                ANNIV RATING DATE INSTEAD OF POLICY EFF DATE.  *
     H*                                                               *
     H* 04/07/05 C:156PROJECT: 126WDK                                 *
     H*          C:33 PROJECT: 126WCD                                 *
     H*               Multiple Deductible discounts per Year.         *
     H*                                                               *
     H* 10/27/99 C:15 Project: WC577                                  *
     H*               Changed the last comp pmt process to include    *
     H*               adjustments.                                    *
     H*                                                               *
     H* 08/25/99 C:90 PROJECT: WC902                                  *
     H*               PRINT 'PER OCC' OR 'PER CLM' BASED ON THE VALUE *
     H*               OF DDVERB IN WTDDP.                             *
     H*                                                               *
     H* 04/01/99 C:15 Project: WC577                                  *
     H*               Fixed pgm to allow more than 10 reserve codes   *
     H*               to be setup in the reserve code table file.     *
     H*****************************************************************
     FWA009F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMCMP     IF   E             DISK
     F                                     RENAME(PCM01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMFNL     IF   E           K DISK
     FWMEML     IF   E           K DISK
      *  CLAIMANT DEMOGRAPHICS
     FWMCDL     IF   E           K DISK
      *  PAYMENT DETAIL FILE
     FWDPAL     IF   E           K DISK
      *
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PPA01)
      *
      *  DEDUCTIBLE DISCOUNT CODE FILE
     FWTDDL2    IF   E           K DISK
     F***Begin Add ***** 05/28/13 *******************************************
     Fwmldl1    if   e           k disk
     F***End Add ******* 05/28/13 *******************************************
      *  EMPLOYER DETAIL FILE
     FWDELL     IF   E           K DISK
      *  RATE SET CODE FILE
     FWMRSL     IF   E           K DISK
      *  DEDUCTIBLE DETAIL FILE
     FWDDDL     IF   E           K DISK
     F                                     RENAME(PDD01:PDD02)
      *** Beg Add ***  11/25/13  ******************************************************
     FWMCXL     IF   E           K DISK
      *** End Add ***  11/25/13  ***********************************************HEADING
      *
      /COPY SCOPYMT01D
      ***  Begin Add  ***  06/09/20  ****************************
      /copy sPrMask
      /copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
     D*** BEGIN DLT *** 04/10/99 **************************************
     D******************* TR         10  1               TABLE RSV TYPE
     D******************* TRT        10  2               TABL RSV TYPE
     D******************* TIN        10  3 0 TDS    15   RSV TYPE/DESC
     D*** END DLT ***** 04/10/99 **************************************
      *
     D NM1             S              1    DIM(80)
     D NM2             S              1    DIM(80)
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*** BEGIN ADD *** 01/28/10 **************************************
     D***PY SCOPYMT01I
     D*
     D*** END ADD ***** 01/28/10 **************************************
     D*** BEGIN DEL *** 01/28/10 **************************************
     D****PSDS      ESDSSZQ1P
     D*** END DEL   *** 01/28/10 **************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D                 DS                  INZ
     D  RR                     1     90  2
     D                                     DIM(10)                              CM RMAIN RSV
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
     D                 DS                  INZ
     D  AR                     1     90  2
     D                                     DIM(10)                              CM PAYMENTS
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
     D                 DS                  INZ
     D  RT                     1     20
     D                                     DIM(10)                              CM RSV TYPE
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
     D                 DS                  INZ
     D  SCTP                   1     60
     D                                     DIM(4)                               SCREEN RSV DESC
     D  SCTP01                 1     15
     D  SCTP02                16     30
     D  SCTP03                31     45
     D  SCTP04                46     60
     D                 DS                  INZ
     D  SCRR                   1     36  2
     D                                     DIM(4)                               SCREEN REMAIN RSV
     D  SCRR01                 1      9  2
     D  SCRR02                10     18  2
     D  SCRR03                19     27  2
     D  SCRR04                28     36  2
     D                 DS                  INZ
     D  SCAR                   1     36  2
     D                                     DIM(4)                               SCREEN PAYMENTS
     D  SCAR01                 1      9  2
     D  SCAR02                10     18  2
     D  SCAR03                19     27  2
     D  SCAR04                28     36  2
     D                 DS                  INZ
     D  SCTI                   1     36  2
     D                                     DIM(4)                               SCREEN TOT INCURR
     D  SCTI01                 1      9  2
     D  SCTI02                10     18  2
     D  SCTI03                19     27  2
     D  SCTI04                28     36  2
     D                 DS                  INZ
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D                 DS                  INZ
     D  XATRDT                 1     10
     D  XATRDM                 1      2
     D  XATRDD                 3      4
     D  XATRDY                 5      8
     D                 DS                  INZ
     D  XAFRDT                 1     10
     D  XAFRDM                 1      2
     D  XAFRDD                 3      4
     D  XAFRDY                 5      8
     D                 DS                  INZ
     D  XATODT                 1      8
     D  XATODM                 1      2
     D  XATODD                 3      4
     D  XATODY                 5      8
      *
     D OPTNM           C                   CONST('Financial Summary')
     D XCEXRE          C                   CONST('EXCS REIN ')
     D*** BEGIN DEL *** 01/28/10 **************************************
     D***           '-Display- '          C         MODED
     D***           'Progressive Data So- C         PDSNAM
     D***           'lutions, Inc.'
     D*** END DELETE*** 01/28/10 **************************************
     D COMAPI          C                   CONST('WA001R')
     D XCCOMP          C                   CONST('Compensation')
     D XCMED           C                   CONST('Medical')
     D XCEXP           C                   CONST('Expense')
     D XCLGAL          C                   CONST('Legal')
     D XCLCP           C                   CONST('Last compensation -
     D                                     paid was $')
     D XCON            C                   CONST('on')
     D XCFOR           C                   CONST('for')
     D XCFROM          C                   CONST('from')
     D XCTHRO          C                   CONST('through')
     D XCP             C                   CONST('.')
      *** Beg Add ***  11/25/13  ******************************************************
     D charLGRH        s             10                                         LEGAL RESERVE HEAD
     D CltId           s              3
      *
     d RtvCltID        pr                  ExtPgm('RTVCLTID')
     d cltID                          3    const
      *** End Add ***  11/25/13  ***********************************************HEADING
      *

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FOR WMCMP
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    #@CO#             3
     C                   PARM                    #@FND             3
     C                   PARM                    #@FYR             3
     C                   PARM                    #@CASE            7
     C                   PARM      Q1USER        #@USR            10
     C                   PARM                    #@RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  KEY LIST FOR THE DEDUCTIBLE DISCOUNT CODE FILE (WTDDL2)
     C*
     C     DDKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    RSJRST
     C***  BEGIN CHG  ***  04/07/05 *************************************
     C**********           KFLD           ELDCDE
     C                   KFLD                    XXDDCD
     C***  END   CHG  ***  04/07/05 *************************************
     C*
     C     DDKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    RSJRST
     C***  BEGIN CHG  ***  04/07/05 *************************************
     C**********           KFLD           ELDCDE
     C**********           KFLD           ELPFDT
     C                   KFLD                    XXDDCD
     C                   KFLD                    XXDDEF
     C***  END   CHG  ***  04/07/05 *************************************
     C*
     C*  KEY LIST FOR THE DEDUCTIBLE DETAIL FILE (WDDDL)
     C*
     C     D2KY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C*
     C*   Key List for PAYMENT DETAIL file
     C*
     C     PAKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*   KEY LIST FOR THE RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    ELRTCD
     C*
     C*  KEY LIST FOR THE TABLE FILE
     C*
     C     RTKY          KLIST
     C                   KFLD                    CMCO#
     C*
     C*  TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    XXKEY            10
     C*
      *** Beg Add ***  11/25/13  ******************************************************
     C*   Claim/DARTS Control Definition File
     C*
     C     keyWMcxl      KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C*** End Add ***  11/25/13  ******************************************************
     C*================================================================
     C*  Normal Programming Area
     C*================================================================
     C*** Beg Add ***  11/25/13  ******************************************************
      /Free
       RtvCltID(CltId);
      /End-Free
     C*** End Add ***  11/25/13  ******************************************************
     C*
     C                   EXSR      GETCOF
     C                   EXSR      GETPGT
     C                   MOVEL(P)  MODED         SCMODE
      *
     C     @@RRN         CHAIN     DBRCDF                             60
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         #@CO#
     C                   MOVE      CMFND         #@FND
     C                   MOVE      CMFYR         #@FYR
     C                   MOVE      CMCASE        #@CASE
     C                   MOVE      *BLANKS       #@RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     #@RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
      * EXCESS REINSURANCE
      *
     C                   MOVE      *OFF          *IN69
     C     CMEXCS        IFEQ      'Y'
     C                   MOVEL(P)  XCEXRE        SCMSG1
     C                   MOVE      *ON           *IN69
     C                   ELSE
     C                   CALL      'WA010R'                                     EXCESS REINSUR
     C                   PARM                    @@RRN             9 0          FOR WMCMP
     C                   PARM                    XXFNDR           13 2
     C                   PARM                    XXEXRE            1
     C     XXEXRE        IFEQ      'Y'
     C                   MOVEL(P)  XCEXRE        SCMSG1
     C                   MOVE      *ON           *IN69
     C                   END
     C                   END
      *
     C                   Z-ADD     CMFND         SCFND
     C                   MOVE      *BLANK        SCFNAM
      *
      *  GET FUND NAME & IF FUND IS EXPIRED, THEN DISPLAY IN INQUIRY MODE
      *
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  FNNAME        SCFNAM
     C                   END
     C*** Beg Add ***  11/25/13  ******************************************************
      /Free
       charLGRH = *Blanks;
       If CltId = '129';
          Chain keyWMcxl pcx01;
          If %Found(wmcxl);
             charLGRH = CXlgrh;
          Endif;
       EndIf;
      /End-Free
     C*** End Add ***  11/25/13  ******************************************************
      *
     C*******              Z-ADDCMEMP#    SCEMP#
     C*******              Z-ADDCMDIV     SCDIV
      *
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C*******              MOVELEMNAME    SCENAM    P
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'
     C                   PARM                    CMEMP#
     C                   PARM                    CMDIV
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    XXEDN            80
     C                   END                                                    WMEMP
      *
      *  CLAIMANT SS#
      *
     C     CMSS#         IFNE      *BLANK
      ******  Begin Chg  ***  06/09/20  **********************
     C*****SS1           CAT(P)    '-':0         SCSS#
     C*****SCSS#         CAT       SS2:0         SCSS#
     C*****SCSS#         CAT       '-':0         SCSS#
     C*****SCSS#         CAT       SS3:0         SCSS#
      /Free
       scSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-free
      ******  End Chg  ***  06/09/20  **********************
     C                   END
      *
      *
      *  CLAIMANT NAME
      *
     C                   MOVE      *BLANK        SCCNAM
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      *OFF
     C     CDLSN         CAT(P)    ',':0         SCCNAM                         LAST, FIRST M.
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
     C     CDMID         IFNE      *BLANK
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C     SCCNAM        CAT       '.':0         SCCNAM
     C                   END
     C                   END
      *  ACCIDENT DATE
      *
     C     CMACDT        IFNE      *ZERO
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL(P)  SRDTO         SCACDT
     C                   ELSE
     C                   MOVE      *BLANK        SCACDT
     C                   END
      *
      *  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
      *
     C     CMCVT#        IFNE      *BLANK
     C                   MOVEL     CMCVT#        SCCVT
     C                   MOVE      *ON           *IN64
     C                   END
      *  FYR-CASE
     C                   MOVE(P)   CMFYR         XAFYR             3
     C                   MOVE(P)   CMCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCS
     C     SCYRCS        CAT       XACASE:0      SCYRCS
      *
      *  AGENGY CLAIM# (OR, SATE CLAIM#)
      *
     C                   MOVEL(P)  CMACL#        SCSCL#
      *
      * ---------------------------------------------------------------
      *  RESERVE SUMMARY
      *
     C*** BEGIN DLT *** 04/01/99 **************************************
      *  GET THE RESERVE TYPE CODES
      *
     C********** RTKY      SETLLPRT01
     C********** *IN51     DOUEQ*ON
     C********** RTKY      READEPRT01                    51
     C********** *IN51     IFEQ *OFF
      *
     C********** RTSTAT    IFEQ 'A'
     C**********           ADD  1         N
     C**********           MOVELRTKEY     TRT,N
     C**********           MOVELRTTYPE    TR,N
      *
     C**********           SELEC**********
     C********** RTTYPE    WHEQ 'C'
     C**********           Z-ADD1         TIN,N
     C**********           MOVELXCCOMP    TDS,N
     C********** RTTYPE    WHEQ 'M'
     C**********           Z-ADD2         TIN,N
     C**********           MOVELXCMED     TDS,N
     C********** RTTYPE    WHEQ 'E'
     C**********           Z-ADD3         TIN,N
     C**********           MOVELXCEXP     TDS,N
     C********** RTTYPE    WHEQ 'L'
     C**********           Z-ADD4         TIN,N
     C**********           MOVELXCLGAL    TDS,N
     C**********           ENDSL
      *
     C**********           END
     C**********           END
     C**********           ENDDO
      *
     C********** 1         DO   10        N       30
      *
     C********** RT,N      IFEQ *BLANK
     C**********           ITER
     C**********           END
      *
     C**********           Z-ADD1         I       30
     C********** RT,N      LOKUPTRT,I                    40
     C********** *IN40     IFEQ *ON
     C**********           Z-ADDTIN,I     J       30
     C**                   MOVELTR,I      SCTP,J             RSV TYPE
     C**********           MOVELTDS,I     SCTP,J             RSV DESC**********
     C**********           ADD  RR,N      SCRR,J             REMAIN RSV
     C**********           ADD  AR,N      SCAR,J             PAYMENTS
     C**********           ADD  AR,N      SCTI,J             TOT INCURRED
     C**********           ADD  RR,N      SCTI,J
     C**********           END
      *
     C**********           ENDDO
     C*** END DLT ***** 04/01/99 **************************************
     C*** BEGIN ADD *** 04/01/99 **************************************
     C*  Load the reserve descriptions.
     C*
     C                   MOVEL     XCCOMP        SCTP01
     C                   MOVEL     XCMED         SCTP02
     C                   MOVEL     XCEXP         SCTP03
     C*** Beg Chg ***  11/25/13  ******************************************************
      /Free
       If Cltid = '129';
          If charLGRH <> *Blanks;
             scTP04 = charLGRH;
          Else ;
             scTP04 = xcLGAL;
          Endif;
       Else;
      /End-Free
     C*** End Chg ***  11/25/13  ******************************************************
     C                   MOVEL     XCLGAL        SCTP04
     C*** Beg Chg ***  11/25/13  ******************************************************
      /Free
       EndIf;
      /End-Free
     C*** End Chg ***  11/25/13  ******************************************************
     C*
     C                   DO        10            N                 3 0
     C*
     C                   MOVEL     RT(N)         XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     RTTYPE        IFEQ      'C'
     C                   ADD       RR(N)         SCRR(1)                          REMAIN RSV
     C                   ADD       AR(N)         SCAR(1)                          PAYMENTS
     C                   ADD       AR(N)         SCTI(1)                          TOT INCURRED
     C                   ADD       RR(N)         SCTI(1)
     C                   ENDIF
     C     RTTYPE        IFEQ      'M'
     C                   ADD       RR(N)         SCRR(2)                          REMAIN RSV
     C                   ADD       AR(N)         SCAR(2)                          PAYMENTS
     C                   ADD       AR(N)         SCTI(2)                          TOT INCURRED
     C                   ADD       RR(N)         SCTI(2)
     C                   ENDIF
     C     RTTYPE        IFEQ      'E'
     C                   ADD       RR(N)         SCRR(3)                          REMAIN RSV
     C                   ADD       AR(N)         SCAR(3)                          PAYMENTS
     C                   ADD       AR(N)         SCTI(3)                          TOT INCURRED
     C                   ADD       RR(N)         SCTI(3)
     C                   ENDIF
     C     RTTYPE        IFEQ      'L'
     C                   ADD       RR(N)         SCRR(4)                          REMAIN RSV
     C                   ADD       AR(N)         SCAR(4)                          PAYMENTS
     C                   ADD       AR(N)         SCTI(4)                          TOT INCURRED
     C                   ADD       RR(N)         SCTI(4)
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*** END ADD ***** 04/01/99 **************************************
      *
      * TOTAL LINE
     C                   XFOOT     RR            SCRRTO                         REMAIN RESV
     C                   XFOOT     AR            SCARTO                         PAYMENTS
     C     SCRRTO        ADD       SCARTO        SCTITO
      *
      * ---------------------------------------------------------------
      *  GET THE LAST COMP PAYMENT INFO
      *
     C                   Z-ADD     *ZERO         SCAMT             9 2
     C                   MOVE      *BLANK        SCTRDT            8 0
     C                   Z-ADD     *ZERO         XXDAYS
     C                   MOVE      *BLANK        SCBR2             2
     C                   MOVE      *BLANK        SCBR2D           20
     C                   MOVE      *BLANK        SCFRDT            8 0
     C                   MOVE      *BLANK        SCTODT            8 0
     C                   MOVE      *BLANK        XXLEAV
      *
     C     PAKY          SETGT     PPA01
     C     *IN60         DOUEQ     *ON
     C     PAKY          READPE    PPA01                                  60
     C     *IN60         IFEQ      *OFF
     C     PARETP        IFEQ      'C'
     C     PAADST        ANDEQ     *BLANK
     C*
     C***  BEGIN ADD  ***  04/23/98  *********************************
     C                   MOVEL(P)  PATRCD        XXKEY
     C*
     C     TBKY          CHAIN     PPC01                              60
     C*
     C*  DO NOT DISPLAY ADJUSTMENTS, RECOVERIES, STOP PYMTS OR VOIDS
     C*  AS LAST COMP PAYMENT
     C*
     C*** BEGIN DLT *** 10/27/99 **************************************
     C********** PCTRTP    IFEQ 'A'
     C********** PCTRTP    OREQ 'R'
     C*** END DLT ***** 10/27/99 **************************************
     C*** BEGIN ADD *** 10/27/99 **************************************
     C     PCTRTP        IFEQ      'R'
     C*** END ADD ***** 10/27/99 **************************************
     C     PCTRTP        OREQ      'S'
     C     PCTRTP        OREQ      'V'
     C                   ITER
     C                   ENDIF                                                  PCTRTP=A/R/S/V
     C***  END   ADD  ***  04/23/98  *********************************
     C*
     C                   MOVE      *ON           *IN57                          DISPLAY
     C                   Z-ADD     PAAMT         SCAMT
      *
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C                   MOVEL     SRDTO         XATRDT
      * CALC DAYS LOST
     C     PAWKS         IFNE      *ZERO
     C     CMDAYS        IFNE      *ZERO                                        DAYS WRK PER WEEK
     C     PAWKS         MULT      CMDAYS        XXDAYS            5 0
     C                   ELSE
     C     PAWKS         MULT      5             XXDAYS
     C                   END
     C                   END
     C                   ADD       PADAYS        XXDAYS
      *
      *  GET ONLY ACTIVE RECORD THAT IS NOT INTEREST OR PENALTY
      *
     C                   MOVEL(P)  PABR2         SCBR2
     C                   MOVEL(P)  PABR2         XXKEY
     C     TBKY          CHAIN     PB201                              60
     C     *IN60         IFEQ      *OFF
     C     B2STAT        ANDNE     'I'                                          ACTIVE RECORD
      *
     C     B2PLCD        IFEQ      'A'                                          INTEREST
     C     B2PLCD        OREQ      'S'                                          PENALTIES
     C                   MOVE(P)   'N'           XXLEAV            1
      *
     C                   ELSE                                                   GOOD RECORD
     C                   MOVEL(P)  B2DESC        SCBR2D
     C                   MOVE(P)   'Y'           XXLEAV
     C                   END
     C                   END                                                    *IN60
      *
     C                   Z-ADD     PAFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFRDT            8 0
     C                   MOVEL     SRDTO         XAFRDT
      *
     C                   Z-ADD     PATODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT            8 0
     C                   MOVEL     SRDTO         XATODT
      *
     C     XXLEAV        IFEQ      'Y'
     C                   LEAVE
     C                   END
      *
     C                   END
     C                   END
     C                   ENDDO
      *
     C*********            Z-ADDXXDAYS    SCLOST
      *
      * IF THERE IS A COMP RECORD, BUILD 2 STRINGS TO SHOW IT NEATLY.
      *
     C     *IN57         IFEQ      *ON
      * LINE 1
      * AMOUNT
     C                   MOVEL(P)  SCAMT         XXNUM            80
     C                   EXSR      CHGNUM
     C                   MOVEL     XXNUM         XAAMT            13
      * LAST COMP PAID WAS $ AMT ON
     C     XCLCP         CAT(P)    XAAMT:1       SCLCP1
     C                   CAT       XCON:1        SCLCP1
      * TRANS DATE
     C                   CAT       XATRDM:1      SCLCP1
     C                   CAT       '/':0         SCLCP1
     C                   CAT       XATRDD:0      SCLCP1
     C                   CAT       '/':0         SCLCP1
     C                   CAT       XATRDY:0      SCLCP1
      * LINE 2
      * FOR SCBR2 SCBR2D
     C     XCFOR         CAT(P)    SCBR2:1       SCLCP2
     C                   CAT       SCBR2D:1      SCLCP2
     C                   CAT       XCFROM:1      SCLCP2
      * FROM DATE
     C                   CAT       XAFRDM:1      SCLCP2
     C                   CAT       '/':0         SCLCP2
     C                   CAT       XAFRDD:0      SCLCP2
     C                   CAT       '/':0         SCLCP2
     C                   CAT       XAFRDY:0      SCLCP2
      * THROUGH
     C                   CAT       XCTHRO:1      SCLCP2
      * FROM DATE
     C                   CAT       XATODM:1      SCLCP2
     C                   CAT       '/':0         SCLCP2
     C                   CAT       XATODD:0      SCLCP2
     C                   CAT       '/':0         SCLCP2
     C                   CAT       XATODY:0      SCLCP2
      * .
     C                   CAT       XCP:0         SCLCP2
     C                   END
      *
      * ---------------------------------------------------------------
      *  LOAD THE SPECIFIC RETENTION FIELD
      *
     C                   Z-ADD     *ZERO         SCSPRE
     C                   CALL      'WA010R'
     C                   PARM                    @@RRN             9 0          FOR WMCMP
     C                   PARM                    XXFNDR           13 2
     C                   PARM                    XXEXRE            1
      *
     C                   Z-ADD     XXFNDR        SCSPRE
      *
     C     SCSPRE        IFNE      *ZERO
     C                   MOVE      *ON           *IN56                          DISPLAY
     C                   END
      *
      * ---------------------------------------------------------------
      *  LOAD THE DEDUCTIBLE TYPE DISPLAY FIELD
      *
     C                   MOVE      *BLANK        SCDETY
     C                   MOVE      *BLANK        SCDEFD
     C                   MOVE      *BLANK        SCAGGR
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C***  BEGIN ADD  ***  04/07/05 *************************************
     C     *LIKE         DEFINE    ELDCDE        XXDDCD
     C     *LIKE         DEFINE    ELDCEF        XXDDEF
     C     *LIKE         DEFINE    ELDCEX        XXDCEX
     C     *LIKE         DEFINE    EL2DEX        XX2DEX
     C*
     C                   Z-ADD     ELDCEX        XXDCEX
     C                   Z-ADD     EL2DEX        XX2DEX
     C*
     C     ELDCEX        IFEQ      *ZERO
     C                   Z-ADD     *HIVAL        XXDCEX
     C                   ENDIF
     C*
     C     EL2DEX        IFEQ      *ZERO
     C                   Z-ADD     *HIVAL        XX2DEX
     C                   ENDIF
     C*
     C                   SELECT
     C     ELDCD2        WHENNE    *BLANK
     C     EL2DEF        ANDLE     CMACDT
     C     XX2DEX        ANDGE     CMACDT
     C                   MOVE      ELDCD2        XXDDCD
     C                   MOVE      EL2DEF        XXDDEF
     C     ELDCDE        WHENNE    *BLANK
     C     ELDCEF        ANDLE     CMACDT
     C     XXDCEX        ANDGE     CMACDT
     C                   MOVE      ELDCDE        XXDDCD
     C                   MOVE      ELDCEF        XXDDEF
     C                   ENDSL
     C***  END   ADD  ***  04/07/05 *************************************
     C*** BEGIN CHG ***  04/10/06  ************************************
     C                   MOVE      XXDDEF        QQDATE
     C                   MOVE      ELCO#         QQCO#
     C                   MOVE      ELFND         QQFND
     C                   MOVE      ELFYR         QQFYR
     C                   MOVE      ELEMP#        QQEMP#
     C                   MOVE      ELDIV         QQDIV
     C*
     C                   CALL      'SREFFD'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQDATE            7
     C*
     C                   MOVE      QQDATE        XXDDEF
     C*** END CHG *****  04/10/06  ************************************
     C*
     C     DDKY1         SETGT     PDD01
     C     DDKY          READPE    PDD01                                  60
     C     *IN60         IFEQ      *OFF
     C*** BEG ADD *****  10/11/07  ************************************
     C     DDSTAT        ANDEQ     'A'
     C*** END ADD *****  10/11/07  ************************************
     C                   MOVE      *ON           *IN58                          DISPLAY
     C                   Z-ADD     DDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL(P)  SRDTO         SCDEFD
     C                   Z-ADD     DDAMPO        SCOCUR
     C                   Z-ADD     DDAMAG        SCAGGR
      **** BEGIN ADD **** 08/25/99 ************************************
     C     DDVERB        IFEQ      'C'
     C                   MOVE      *ON           *IN50
     C                   ELSE
     C                   MOVE      *OFF          *IN50
     C                   ENDIF
      **** END   ADD **** 08/25/99 ************************************
     C     DDTYPE        IFEQ      'G'
     C                   MOVEL(P)  'Gross'       SCDETY
     C                   ELSE
     C                   MOVEL(P)  'Net'         SCDETY
     C                   END
     C                   END
      *
     C                   END
     C                   END
      *
      * ---------------------------------------------------------------
      *  GET DEDUCTIBLE BILLED AMOUNT
      *
     C                   Z-ADD     *ZERO         SCAMTB
     C     D2KY          SETLL     PDD02
     C     *IN61         DOUEQ     *ON
     C     D2KY          READE     PDD02                                  61
     C     *IN61         IFEQ      *OFF
     C                   MOVE      *ON           *IN59                          DISPLAY
     C                   ADD       DDAMTB        SCAMTB
     C                   END
     C                   ENDDO
      *
      * ---------------------------------------------------------------
      *  DISPLAY SCREEN
      *
     C     *INKH         DOUEQ     *OFF
     C                   EXFMT     SCR01
      *
      *  F8 = DEDUCTIBLE HISTORY
      *
     C     *INKH         IFEQ      *ON
     C                   MOVE      CMFND         QQFND
     C                   MOVE      CMEMP#        QQEMP#
     C                   MOVE      CMDIV         QQDIV
     C                   MOVE      CMFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
     C                   CALL      'W0585R'
     C                   PARM      *BLANK        QQPRDS           40
     C                   PARM      *BLANK        QQCALL            1
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM      *BLANK        QQRTCD            1
     C                   END
      *
     C     QQRTCD        IFEQ      'A'
     C                   LEAVE
     C                   END
     C*
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C*================================================================
     C*  GETCOF - Gets company and fund names if possible; otherwise
     C*           get PDS name for company name
     C*================================================================
     C     GETCOF        BEGSR
     C                   CALL      COMAPI        COMPL                  99
B01  C     *IN99         IFEQ      *ON
     C***  Begin Chg  ***  01/28/10  **************************************
     C**********           MOVELPDSNAM    ##DATA    P
     C                   CALL      'RTVBRND'                            99
     C                   PARM      'I'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C*
     C                   MOVEL(P)  #BRND1        ##DATA
     C***  End   Chg  ***  01/28/10  **************************************
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        ##CONM
E01  C                   ENDIF
     C                   MOVEL     ##CONM        SCCONM
     C                   ENDSR
     C*================================================================
     C*  GETPGT - Get program title for display heading
     C*================================================================
     C     GETPGT        BEGSR
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   ENDSR
     C*===============================================================
     C* CHGNUM - SUBROUTINE TO CHANGE NUMBERS TO ALPHA WITHOUT ZEROS
     C*===============================================================
     C*
     C     CHGNUM        BEGSR
     C*
     C                   CLEAR                   NM1
     C*
     C     '0'           CHECK     XXNUM         XXZERO            3 0
     C     ' '           CHECKR    XXNUM         XXLEN             3 0
     C     XXZERO        SUB       1             XXBLNK            3 0
     C                   MOVEA     XXNUM         NM1
     C*
     C                   DO        XXBLNK        X                 3 0
     C                   MOVE      ' '           NM1(X)
     C                   ENDDO                                                  X->XXBLNK
     C*
     C                   CLEAR                   NM2
     C**
     C**         EDTMSK    WHEQ 'X,XXX.XX'
     C*
     C                   Z-ADD     XXLEN         W                 3 0
     C     XXLEN         DIV       3             Y                 3 0
     C                   ADD       Y             XXLEN
     C                   ADD       1             XXLEN
     C                   Z-ADD     XXLEN         Z                 3 0
     C*
     C                   DO        XXLEN         X
     C*
     C     X             IFEQ      1
     C*
     C                   MOVEL     *BLANK        NM2(Z)
     C*
     C                   ELSE
     C*
     C     NM1(W)        IFNE      *BLANK
     C*
     C     X             DIV       4             Y
     C                   MVR                     Y
     C*
     C     Y             IFEQ      *ZERO
     C*
     C     X             IFEQ      4
     C                   MOVEA     '.'           NM2(Z)
     C                   ELSE
     C                   MOVEA     ','           NM2(Z)
     C                   ENDIF                                                  Z=4
     C*
     C                   ELSE
     C*
     C                   MOVE      NM1(W)        NM2(Z)
     C                   SUB       1             W
     C*
     C                   ENDIF                                                  Y=*ZERO
     C*
     C                   ENDIF                                                  NM1,W<>*BLANK
     C*
     C                   ENDIF                                                  X=1
     C*
     C                   SUB       1             Z
      *
     C                   ENDDO                                                  X->XXLEN
      *
      * LEFT ADJUST AMOUNT
      *
     C                   MOVEA     NM2           XXNUM
     C     ' '           CHECK     XXNUM         S                 3 0
     C                   SUBST(P)  XXNUM:S       XXNUM
      *
     C                   ENDSR                                                  CHGNUM
