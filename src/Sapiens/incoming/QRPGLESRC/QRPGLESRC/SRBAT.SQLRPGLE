     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE SRBAT - API FOR CREATING A/R BATCH RECORDS
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                       *
     H**                                                              *
     H**      THIS PROGRAM IS USED TO CREATE A/R BATCH RECORDS,       *
     H**      BUILDING A NEW BATCH # IF NECESSARY.                    *
     H**                                                              *
     H**      IF CALLED WITH A MODE OF 'O' FOR ONLINE PROCESSING, IT  *
     H**      WILL ATTEMPT TO FIND AN ONLINE BATCH FOR THE DAY ITS    *
     H**      PROCESSING (BILLING OR PAYMENT) AND USE IT IF FOUND.    *
     H*       THE ONLINE PAYMENT BATCH IS ALSO FLAGGED AS ACH.        *
     H*                                                               *
     H*       Mode = 'P' for 3rd Party Processing                     *
     H*        EAACHF = 'A' for S/R Online EFT Payments               *
     H*        EAACHF = 'B' for S/R Online Billings                   *
     H*        EAACHF = 'T' for 3rd Party Payments                    *
     H*        EAACHF = 'U' for 3rd Party Billings                    *
     H*                                                               *
     H* PROGRAMMER: C:65                                              *
     H* CREATED:    03/28/08            PROJECT: CI019                *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *                            *
     H* 06/24/24 C:079 Project: 146WKR                                *
     H*                Collateral Processing - Recomile               *
     H*                                                               *
     H* 01/25/24 C:344 Project 133WTD                                 *
     H*               -Update SRONLB subroutine to build or access    *
     H*                different third party batch when writing the   *
     H*                A/R batch record for a third party payment.    *
     H*                                                               *
     H* 10/23/23 C:062 Ticket: 114363(099)                            *
     H*                Assign inv# like IVC.                          *
     H*                                                               *
     H* 05/12/21 C:062 Project WC2663                                 *
     H*                Add invoice cloud processing                   *
     H*                                                               *
     H* 11/18/20 C:180 Project WC2757                                 *
     H*                Recompiled only.                               *
     H*                                                               *
     H* 10/27/20 C:136 Project SY424                                  *
     H*                Add invoice number to entry parms so that      *
     H*                SRONLI can pass it back.                       *
     H*                                                               *
     H* 11/04/19 C:136 Project: SY407 (DE573)                         *
     H*                Add processing for 3rd Party transactions      *
     H*                                                               *
     H* 10/21/13 C:136 Support: WA146 83726                           *
     H*                Flag A/R batch record as dissoved when         *
     H*                written for non state rpt div on summarized    *
     H*                policy to prevent A/R Batch edit requiring     *
     H*                all records be dissolved from invoking         *
     H*                on S/R Online Batches                          *
     H*                                                               *
     H* 08/28/13 C:136 Support: WA146 83053                           *
     H*                Don't wash payments to non state reporting     *
     H*                divisions with summarized A/R through          *
     H*                Cash Dissolve                                  *
     H*                                                               *
     H* 06/16/11 C:191 Project: WC2048                                *
     H*                Auto Cash/Dissolve/Pmts AR Batch               *
     H*                                                               *
     H* 12/27/11 C:136 SUPPORT: WA146 78754                           *
     H*                ADD A/R SUMMARY PROCESSING                     *
     H*                                                               *
     H* 01/11/11 C:180 SUPPORT 75397
     H*                CREATE NEW BATCH DURING ONLI-PROCESSING WHEN
     H*                THE BATCH HAS ALREADY BEEN APPROVED OR
     H*                TRANSFERRED.
     H*
     H* 04/19/10 C:65  Project: SUPPORT                               *
     H*                RETURN THE BATCH # TO THE CALLING PROGRAM.     *
     H*                CORRECT THE BATCH TOTAL FOR NON-ONLINE.        *
     H*                                                               *
     H* 11/19/09 C:33  Project: 25WQI                                 *
     H*               -For 025, load the invoice # when writing       *
     H*                 billing records                               *
     H*****************************************************************
     FWTABL     IF   E           K DISK
     FWDEAL     IF   E           K DISK
     F                                     RENAME(PEA01:PEA01B)
     FWDEAL4    UF A E           K DISK
     F                                     RENAME(PEA01:PEA02B)
     F                                     INFDS(DBFDS)
     FWDF2L     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     F*** Begin Add *** 05/12/21 *******************************************
     FWmeap     o    E             DISK    Rename(pea01:pea0p)
     FWmexl     if   E           k DISK    Rename(pex01:pex00)
     F*** End   Add *** 05/12/21 *******************************************

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDS           DS
     D  DBRRN                397    400B 0
     d xxAcfl          s               n   inz(*off)
     d GetAutoCash     pr              n
     d  inCo#                              like(ELco#)
     d  inFnd                              like(ELfnd)
     d  inEmp#                             like(ELemp#)
     d  inDiv                              like(ELdiv)
     d  inFyr                              like(ELfyr)
     d  outDoNotDiss                   n
     d doNotDissFlg    s              1
      *** beg add ***** 11/04/19 **************************
     d xxAchF          s              1
     d*** Begin Add *** 10/23/23 *******************************************
     d inKey           s             20
     d*** End   Add *** 10/23/23 *******************************************
     d*** Begin Add *** 01/25/24 *** 133WTD ********************************
     d strLiveLib      s             10a   Varying Inz
     d strSql          s           5000a   Varying Inz
     d flg3pyGrp       s              1a   Inz
     D/copy wPrEMplr
     d*** End   Add *** 01/25/24 *** 133WTD ********************************
     d intNextInv#     s             10  0
     d intRRN          s              9  0
     d*** Begin Add *** 05/12/21 *******************************************
     d chrMode         s             25
     d isWritingLiveAR...
     d                 s               n   Inz
     d xxRrn           s              9  0 Inz(0)
     d*** End   Add *** 05/12/21 *******************************************
     d CashDissolve    pr                  ExtPgm('WA055R')
     d  mode                          1    Const
     d  rrn                           9  0
      /copy wprAr
      *** end add ***** 11/04/19 **************************
     C*
     C*  TABLE FILE KEY LIST
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  EMPLOYER A/R BATCH FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSTAT
     C*
     C     EAKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBAT#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     EAKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBAT#
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     *LIKE         DEFINE    EACO#         XXCO#
     C     *LIKE         DEFINE    EAFND         XXFND
     C     *LIKE         DEFINE    EAFYR         XXFYR
     C     *LIKE         DEFINE    EAEMP#        XXEMP#
     C     *LIKE         DEFINE    EADIV         XXDIV
     C     *LIKE         DEFINE    BSKEY         XXKEY
     C     *LIKE         DEFINE    EAARCD        XXARCD
     C     *LIKE         DEFINE    EAPODT        XXPODT
     C     *LIKE         DEFINE    EAPODT        XXTRDT
     C     *LIKE         DEFINE    EABAT#        XXBAT#
     C     *LIKE         DEFINE    EASYCD        XXSYCD
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
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQPODT            8
     C                   PARM                    QQARCD            2
     C                   PARM                    QQAMT            11
     C                   PARM                    QQBAT#            9
     C                   PARM                    QQMODE            1
      *** beg add ***** 10/27/20 ***************************************
     C                   PARM                    qqInv#           10 0
      *** end add ***** 10/27/20 ***************************************
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQAMT         XXAMT            11 2
     C                   MOVE      QQARCD        XXARCD
     C                   MOVE      QQPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C*
     C                   Z-ADD     0             XXTOTB           11 2
     C                   MOVE      *BLANKS       XXSYCD
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*
      /free
       xxACfl = GetAutoCash(xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:doNotDissFlg);
      *** beg add ***** 10/27/20 ***************************
       If q1Prms >= 11;
          intNextInv# = qqInv#;
       Endif;
      *** end add ***** 10/27/20 ***************************
       //*** Begin Add *** 05/12/21 ****************************************
       // Are we dealing with Pay to Go  Wrte to live A/R
       Chain (xxCo#:xxFnd) pex00;
       If %Found(wmexl);
          If EXicP2Gar = 'Y';
             isWritingLiveAR = *Off;
          EndIf;
       EndIf;
       // Set mode field to be more descriptive.
       Select;
       When qqMode = 'O';
          chrMode = 'S/R Online';
       When qqMode = 'T';
          chrMode = 'Pay As You Go EFT';
       When qqMode = 'P';
          chrMode = 'Pay As You Go 3rd Party';
       EndSl;
       //*** End   Add *** 05/12/21 ****************************************
      /end-free
     C*
     C     QQBAT#        IFNE      *BLANKS
     C                   MOVE      QQBAT#        XXBAT#
     C*
     C     EAKEY2        CHAIN(N)  PEA02B                             60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     EASYCD        XXSYCD
     C                   ELSE
     C                   Z-ADD     0             XXBAT#
     C                   ENDIF
     C*
     C                   ELSE
     C                   Z-ADD     0             XXBAT#
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C*
     C                   MOVEL(P)  XXARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C*
     C                   MOVE      TTTYPE        SVTYPE            1
     C                   MOVE      TTDESC        SVDESC           30
     C                   MOVE      TTCTYP        SVCTYP            1
     C*
     C* FIND ONLINE PROCESSING BATCH WHEN CALLED IN ONLINE MODE.
     C*
      *** beg chg ***** 11/04/19 *******************************
     C***     QQMODE        IFEQ      'O'
     C***     XXBAT#        ANDEQ     *ZERO
     C***                   EXSR      SRONLB
     C***                   ELSE
     C****
     C***     XXBAT#        IFNE      *ZERO
     C***     EAKEY2        CHAIN(N)  PEA02B                             60
     C***     *IN60         DOWEQ     '0'
     C***                   ADD       EAAMT         XXTOTB
     C***     EAKEY2        READE(N)  PEA02B                                 60
     C***                   ENDDO                                                  *IN60 DOWEQ
     C***                   ENDIF                                                  XXBAT# IFNE
     C****
     C***                   ENDIF                                                  QQMODE IFEQ

      /Free
       Select;
       When xxBat# <> 0;
          Setll EAkey2 pea02b;
          Dou %Eof(WDEAL4);
             Reade EAkey2 pea02b;
             If %Eof;
                Leave;
             EndIf;
             xxTotB += EAamt;
          Enddo;
       //*** Begin Chg *** 05/12/21 ***********************************
       //*** When qqMode = 'O' Or qqMode = 'P';
       When qqMode = 'O' Or qqMode = 'P' Or qqMode = 'T';
       //*** End   Chg *** 05/12/21 ***********************************
          Exsr SrOnlB;
       EndSl;
      /End-Free
      *** end chg ***** 11/04/19 *******************************

     C*
     C* CREATE NEW A/R BATCH # IF NEEDED.
     C*
     C     XXBAT#        IFEQ      *ZERO
     C                   EXSR      SRBAT
     C                   ENDIF
     C*
     C* CREAT A/R BATCH RECORD.
     C*
     C     XXBAT#        IFNE      *ZERO
     C                   EXSR      SRWRTB
     C                   ENDIF                                                  XXBAT# IFNE
     C*
     C* UPDATE BATCH TOTAL AND DELETE DUMMY BATCH IF FOUND.
     C*
     C     XXBAT#        IFNE      *ZERO
     C                   EXSR      SRDELB
     C                   ENDIF                                                  XXBAT# IFNE
     C*
     C                   MOVE      XXBAT#        QQBAT#
     C                   MOVE      *ON           *INLR
     C*====================================================================
     C* Get the invoice # for billing records being created
     C*====================================================================
     C     GETINV        BEGSR
     C*
     C     *LIKE         DEFINE    EACO#         #ICO#
     C     *LIKE         DEFINE    EAFND         #IFND
     C     *LIKE         DEFINE    EAEMP#        #IEMP#
     C     *LIKE         DEFINE    EADIV         #IDIV
     C     *LIKE         DEFINE    EAFYR         #IFYR
     C     *LIKE         DEFINE    EAPODT        #IDT
     C     *LIKE         DEFINE    EAINV#        #IINV#
     C*
     C     XXCLT#        IFEQ      '025'
     C*
     C     SVTYPE        IFEQ      'A'
     C     SVTYPE        OREQ      'B'
     C     SVTYPE        OREQ      'D'
     C     SVTYPE        OREQ      'F'
     C     SVTYPE        OREQ      'H'
     C*
     C                   CALL      'SRINV#'
     C                   PARM      EACO#         #ICO#
     C                   PARM      EAFND         #IFND
     C                   PARM      EAEMP#        #IEMP#
     C                   PARM      EADIV         #IDIV
     C                   PARM      EAFYR         #IFYR
     C                   PARM      EAPODT        #IDT
     C     EAINV#        PARM                    #IINV#
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*
     C*  FIND ONLINE BATCH # AND STORE TOTAL BATCH AMOUNT.
     C*
     C     SRONLB        BEGSR
     C*
      *** beg chg ***** 11/04/19 **************************
     C***                   MOVE      *BLANK        XXACHF            1
     C***                   MOVE      'B'           XXACHF            1
     C***     SVTYPE        IFEQ      'P'
     C***     SVTYPE        OREQ      'Q'
     C***                   MOVE      'A'           XXACHF
     C***                   ENDIF

      /Free
       //*** Begin Chg *** 05/12/21 *********************************
       //***Select;
       //***When qqMode = 'O';
       //***   If svType = 'P' or svType = 'Q';
       //***      xxAchf = 'A';
       //***   Else;
       //***      xxAchF = 'B';
       //***   Endif;
       //***When qqMode = 'P';
       //***   If svType = 'P' or svType = 'Q';
       //***      xxAchf = 'T';
       //***   Else;
       //***      xxAchF = 'U';
       //***   Endif;
       //***EndSl;

       //*** Beg Add *** 01/25/24 *** 133WTD *******************************
       // Get live library name
       strLiveLib = RtvQuoteLib('*LIV');
       Chain(n) (xxCo#:xxFnd) pex00;
       Clear flg3pyGrp;
       If %found(WMEXL);
          flg3pyGrp = EXiCpsGrp;
       Endif;
       //*** End Add *** 01/25/24 *** 133WTD *******************************

       // Leave if writing directly to live A/R
       If isWritingLiveAR;
          LeaveSr;
       Endif;

       // Set ACH Flag
       Select;
       When chrMode = 'S/R Online' Or chrMode = 'Pay As You Go EFT';
          If svType = 'P' or svType = 'Q';
             xxAchf = 'A';
          Else;
             xxAchF = 'B';
          Endif;
       When chrMode = 'Pay As You Go 3rd Party';
          If svType = 'P' or svType = 'Q';
             xxAchf = 'T';
          Else;
             xxAchF = 'U';
          Endif;
       EndSl;
       //*** End   Chg *** 05/12/21 *********************************
      /End-Free
      *** end chg ***** 11/04/19 **************************
     C*
     C                   MOVE      *BLANKS       XXSTAT            1
       //*** Beg Del *** 01/25/24 *** 133WTD **************************
     C***     EAKEY         SETGT     PEA01B
     C***     EAKEY         READPE    PEA01B                              60
     C***
     C***     *IN60         DOWEQ     *OFF
     C***
     C***     EAONLI        IFNE      'Y'
     C***     EAACHF        ORNE      XXACHF
     C***     EAIDT         ORNE      XXTRDT
     C***     EAKEY         READPE    PEA01B                                 60
     C***                   ELSE
     C***                   LEAVE
     C***                   ENDIF
     C***                   ENDDO
       //*** End Del *** 01/25/24 *** 133WTD **************************
       //*** Beg Add *** 01/25/24 *** 133WTD **************************
       // Is there a non-approved 3rd party batch in WDEAP for the
       // current system date.
       strSql = 'Select EAsycd, EAbat# ' +
        'From ' + %Trim(strLIVELIB) + '/WDEAP ' +
        'Where EAonli = ''Y'' And EAachf = '''  + %Trim(xxACHF) +
        ''' And EAidt = ' + %Editc(xxTRDT:'3') + ' And ' +
        'EAtfap <> ''Y'' And EAtffl <> ''Y''';
        Select;
        When flg3pyGrp = 'Y';
           strSql += ' And EAfnd = ' + %Editc(EXfnd:'X');
        Other;
           strSql += ' And EAfnd In (Select EXfnd ' +
           'From ' + %Trim(strLivelib) + '/WMEXP ' +
           'Where EXicpsgrp <> ''Y'')';
        Endsl;
       strSql += ' Limit 1';
       Exec SQL Prepare e1 From :strSQL;
       Exec SQL Declare e1Cursor Cursor For e1;
       Exec SQL Open e1Cursor;
       Exec SQL Fetch Next From e1Cursor
        Into :xxSYCD, :xxBAT#;
       Exec SQL Close e1Cursor;
       //*** End Add *** 01/25/24 *** 133WTD **************************
     C*
     C* ONLINE BATCH HAS BEEN FOUND.
     C*
       //*** Beg Del *** 01/25/24 *** 133WTD **************************
     C***     *IN60         IFEQ      *OFF
     C***     EATFAP        ANDNE     'Y'
     C***     EATFFL        ANDNE     'Y'
     C***                   Z-ADD     EABAT#        XXBAT#
     C***                   MOVEL     EASYCD        XXSYCD
       //*** End Del *** 01/25/24 *** 133WTD **************************
     C*
     C     EAKEY2        CHAIN(N)  PEA02B                             60
     C     *IN60         DOWEQ     '0'
     C                   ADD       EAAMT         XXTOTB
     C     EAKEY2        READE(N)  PEA02B                                 60
     C                   ENDDO                                                  *IN60 DOWEQ
       //*** Beg Del *** 01/25/24 *** 133WTD **************************
     C***                   ENDIF                                               *IN60 IFEQ
       //*** End Del *** 01/25/24 *** 133WTD **************************
     C*
     C                   ENDSR                                                  sRONLB SUBR
     C*****************************************************************
     C*
     C*  RESERVE NEXT AVAILABLE BATCH NUMBER FOR A/R BATCH.
     C*
     C     SRBAT         BEGSR


       //*** Begin Add *** 05/12/21 ***********************************
       // Leave if writing directly to live A/R
       If isWritingLiveAR;
          LeaveSr;
       Endif;
       //*** End   Add *** 05/12/21 ***********************************
     C*
     C*  FIND A VALID SYSTEM TABLE CODE
     C*
     C     XXCO#         SETLL     PSY01
     C     *IN60         DOUEQ     *ON
     C     XXCO#         READE     PSY01                                  60
     C     SYTYPE        IFEQ      'Y'
     C     SYSTAT        ANDNE     'I'
     C                   MOVEL     SYKEY         XXSYCD
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C                   ENDDO
     C*
     C                   MOVE      *BLANKS       XXSTAT            1
     C     EAKEY         SETGT     PEA01B
     C                   READP     PEA01B                                 60
     C     *IN60         IFEQ      '0'
     C     EACO#         ANDEQ     XXCO#
     C     1             ADD       EABAT#        XXVAL1            9 0
     C                   END
     C*
     C                   MOVE      'T'           XXSTAT
     C     EAKEY         SETGT     PEA01B
     C                   READP     PEA01B                                 60
     C     *IN60         IFEQ      '0'
     C     EACO#         ANDEQ     XXCO#
     C     1             ADD       EABAT#        XXVAL2            9 0
     C                   END
     C*
     C     XXVAL1        IFGT      XXVAL2
     C                   Z-ADD     XXVAL1        XXBAT#
     C                   ELSE
     C                   Z-ADD     XXVAL2        XXBAT#
     C                   END
      *
     C     XXBAT#        IFEQ      *ZEROS
     C                   Z-ADD     1             XXBAT#
     C                   END
      *
      *  Write an A/R batch dummy record, to hold the bat#.
      *
     C                   Z-ADD     XXCO#         EACO#
     C                   Z-ADD     XXBAT#        EABAT#
     C                   Z-ADD     0             EABATA
     C                   MOVE      *HIVAL        EAFND
     C                   MOVE      *HIVAL        EAFYR
     C                   MOVE      *HIVAL        EAEMP#
     C                   MOVE      *HIVAL        EADIV
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EACTYP
     C                   MOVE      *BLANKS       EADESC
     C                   Z-ADD     *ZEROS        EAPODT
     C                   Z-ADD     *ZEROS        EACHK#
     C                   Z-ADD     *ZEROS        EACKAM
     C                   Z-ADD     *ZEROS        EAAMT
     C                   MOVE      *BLANKS       EASYCD
     C                   MOVE      'N'           EATFAP
     C                   MOVE      'N'           EATFFL
     C                   Z-ADD     *ZEROS        EATFDT
     C                   Z-ADD     *ZEROS        EATFAM
     C                   MOVE      *BLANKS       EASTAT
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   Z-ADD     DSTIME        EATIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   MOVEL     SRDTO         EAIDT
     C                   MOVE      Q1USER        EAIUSR
     C                   MOVE      Q1PGM         EAIPGM
     C                   Z-ADD     *ZEROS        EAINV#
     C                   Z-ADD     *ZEROS        EATRDT
     C                   MOVE      *BLANKS       EALATE
      *
     C                   WRITE     PEA02B
     C*
     C                   ENDSR                                                  SRBAT SUBR
     C*****************************************************************
     C*
     C* WRITE BATCH RECORD.
     C*
     C     SRWRTB        BEGSR
     C*
     C                   CLEAR                   PEA02B
     C*
     C                   Z-ADD     XXCO#         EACO#
     C                   Z-ADD     XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   Z-ADD     XXEMP#        EAEMP#
     C     ELKY          CHAIN     PEL01                              60
     C     ELARSM        IFNE      *BLANK
      *
     C     EMKY          SETLL     PEM01
     C     EMKY          READE     PEM01                                  60
     C     *IN60         DOWEQ     *OFF
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   LEAVE
     C                   ENDIF
     C     EMKY          READE     PEM01                                  60
     C                   ENDDO
      *
     C                   ELSE
      *
     C                   Z-ADD     XXDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C*
     C                   Z-ADD     XXBAT#        EABAT#
     C                   Z-ADD     *ZEROS        EACHK#
     C                   Z-ADD     *ZEROS        EABATA
     C                   MOVE      XXSYCD        EASYCD
     C                   MOVE      'N'           EATFAP
     C                   MOVE      'N'           EATFFL
     C                   Z-ADD     *ZEROS        EATFDT
     C                   Z-ADD     *ZEROS        EATFAM
     C*
      *** Beg Chg ***** 11/04/19 ***************************
     C***     QQMODE        IFEQ      'O'
     C****
     C***     SVTYPE        IFEQ      'P'
     C***     SVTYPE        OREQ      'Q'
     C***                   MOVE      'A'           EAACHF
     C***                   ENDIF
     C****
     C***                   ENDIF                                                  QQMODE IFEQ
     C****
     C***     QQMODE        IFEQ      'O'
     C***                   MOVE      'Y'           EAONLI
     C***                   ENDIF
      ***
      /Free
       //*** Begin Chg *** 05/12/21 ****************************************
       //**********Select;
       //**********When qqMode = 'O';  // S/R Online
       //**********   EAonli = 'Y';
       //**********   If svType = 'P' or svType = 'Q';
       //**********      EAAchf = 'A';
       //**********   Else;
       //**********      EAAchF = 'B';
       //**********   Endif;
       //*********When qqMode = 'P'; // S/R Online with 3rd Party Processing
         //*** beg add ***** 10/27/20 **************************
       //**********   If intNextInv# = 0;
         //*** end add ***** 10/27/20 **************************
       //**********      intNextInv# =
       //**********       GetNext3rdPartySeq#(
       //**********       %EditC(EAco#:'X')  +
       //**********       %EditC(EAfnd:'X')  +
       //**********       %EditC(EAemp#:'X') +
       //**********       %EditC(xxDiv:'X')
       //**********       );
         //*** beg add ***** 10/27/20 **************************
       //**********   Endif;
       //**********   If Q1prms >= 11;
       //**********      qqInv# = intNextInv#;
       //**********   Endif;
         //*** end add ***** 10/27/20 **************************
       //**********   EAonli = 'Y';
       //**********   If svType = 'P' or svType = 'Q';
       //**********      EAAchf = 'T';
       //**********   Else;
       //**********      EAAchF = 'U';
       //**********   Endif;
       //**********EndSl;
       //*** Begin Add *** 10/23/23 ****************************************
       //* Get the invoice#
       //* Load invoice from entry parameter
       If q1Prms >= 11;
          intNextInv# = qqInv#;
       Endif;

       // No invoice# so lets find one or create one
       If intNextInv# = 0;

          // Find invoice# by looking at billing records
          Exec Sql Select EAninv Into :intNextInv# From WMEAP
                   Where EAco#=:EAco# And EAfnd=:EAfnd And
                   EAemp#=:EAemp# And EAdiv=:xxDiv And EAfyr=:EAfyr
                   And EACTyp<> 'C' And EACTyp <> 'R' And
                   EApoDt=:xxPoDt And EAarTy in ('A','B','D','F','H');

          // No billing records had an invoice# so look in AR batch file
          If intNextInv# = 0;

             Exec Sql Select EAinv# Into :intNextInv#
                      From WDEAP join WTTTP on EAarCd = TTkey
                      Where EAco#=:EAco# And EAfnd=:EAfnd And
                      EAemp#=:EAemp# And EAdiv=:xxDiv And EAfyr=:EAfyr
                      And EACTyp<> 'C' And EACTyp <> 'R' And
                      EApoDt=:xxPoDt And TTtype in ('A','B','D','F','H');

             If intNextInv# = 0;
                inKey       = %EditC(EAco#:'X') + %EditC(EAfnd:'X') +
                              %EditC(EAemp#:'X') + %EditC(xxDiv:'X');
                intNextInv# = GetNext3rdPartySeq#(inKey);
             Endif;
          Endif;
       Endif;

       // Return the invoice#
       If Q1prms >= 11;
          qqInv# = intNextInv#;
       Endif;
       //*** End   Add *** 10/23/23 ****************************************
       Select;
       When chrMode = 'S/R Online' Or chrMode = 'Pay As You Go EFT';
          EAonli = 'Y';
          If svType = 'P' or svType = 'Q';
             EAAchf = 'A';
          Else;
             EAAchF = 'B';
          Endif;
       When chrMode = 'Pay As You Go 3rd Party';
          //*** Begin Del *** 10/23/23 *************************************
          //***If intNextInv# = 0;
          //***   intNextInv# =
          //***    GetNext3rdPartySeq#(
          //***    %EditC(EAco#:'X')  +
          //***    %EditC(EAfnd:'X')  +
          //***    %EditC(EAemp#:'X') +
          //***    %EditC(xxDiv:'X')
          //***    );
          //***Endif;
          //***If Q1prms >= 11;
          //***   qqInv# = intNextInv#;
          //***Endif;
          //*** End   Del *** 10/23/23 *************************************
          EAonli = 'Y';
          If svType = 'P' or svType = 'Q';
             EAAchf = 'T';
          Else;
             EAAchF = 'U';
          Endif;
       EndSl;
       //*** End   Chg *** 05/12/21 ****************************************
      /End-Free
      *** End Chg ***** 11/04/19 ***************************
     C*
     C                   MOVE      XXARCD        EAARCD
     C                   MOVEL     SVDESC        EADESC
     C                   MOVE      SVCTYP        EACTYP
     C*
     C                   Z-ADD     XXAMT         EAAMT
     C                   Z-ADD     XXAMT         EACKAM
     C                   ADD       EAAMT         XXTOTB
     C*
     C                   MOVE      XXPODT        EAPODT
     C*
      *** Beg Chg ***** 11/04/19 ***************************
     C***  EAACHF        IFNE      *BLANK
     C     EAACHF        IFEQ      'A'
     C     EAACHF        OREQ      'T'
      *** End Chg ***** 11/04/19 ***************************
     C                   MOVE      XXTRDT        EATRDT
     C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        EAINV#
     C                   MOVE      *BLANKS       EALATE
     C*
     C                   MOVE      *BLANKS       EASTAT
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   MOVEL     SRDTO         EAIDT
     C                   MOVE      Q1USER        EAIUSR
     C                   MOVE      Q1PGM         EAIPGM
      *** Beg Chg ***** 11/04/19 **********************
     C***                   EXSR      GETINV
      /Free
       EAinv# = intNextInv#;
      *** End Chg ***** 11/04/19 **********************
       If xxAcFl = *on and doNotDissFlg = *on and svType = 'P';
          EAcash = 'D';
          xxAcfl = *off;
       Endif;
      /end-free
     C*** Begin Chg *** 05/12/21 ****************************************
     C***                WRITE     PEA02B
       Select;
       When chrMode = 'Pay As You Go EFT' And
            svType = 'B' And isWritingLiveAR = *On;
          EAnInv = EAinv#;
          EAarTy = GetARType(EAarCd:xxRrn:xxCo#);
          EAtrDt = xxTrDt;
          Write pea0p;
       Other;
          Write PEA02B;
       EndSl;
     c*** End   Chg *** 05/12/21 ****************************************
     C*
      *** Beg Chg ***** 11/04/19 ***************************
     C***     XXACFL        IFEQ      '1'
     C***     SVTYPE        ANDEQ     'P'
     C***                   CALL      'WA055R'
     C***                   PARM      'S'           ##MODE            1
     C***                   PARM      DBRRN         ##RRN             9 0
     C***                   ENDIF
      /Free
       // Run S/R Online Payments thru Cash Dissolve
       //  if not 3rd Party Processing
       //*** Begin Chg *** 05/12/21 ****************************************
       //***If xxAcFl = *On
       //*** And svType = 'P'
       //*** And qqMode = 'O';
       //***   intRRN = dbRRN;
       //***   CashDissolve('S':intRRN);
       //***Endif;
       intRRN = dbRRN;
       Select;
       When xxAcFl = *Off;
       When svType <> 'P';
       When chrMode = 'S/R Online';
         CashDissolve('S':intRRN);
       When chrMode = 'Pay As You Go EFT';
         CashDissolve('T':intRRN);
       Endsl;
       //*** End   Chg *** 05/12/21 ****************************************
      /End-Free
      *** End Chg ***** 11/04/19 ***************************
     C*
     C                   ENDSR                                                  SRWRTB SUBR
     C*****************************************************************
     C*
     C* UPDATE BATCH TOTAL AND REMOVE DUMMY PLACEHOLDER BATCH.
     C*
     C     SRDELB        BEGSR
      *
      *  UPDATE ALL BATCH RECORDS CREATED WITH THE TOTAL BATCH AMOUNT
      *
     C     EAKEY2        CHAIN     PEA02B                             60
      *
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     XXTOTB        EABATA
     C                   UPDATE    PEA02B
     C     EAKEY2        READE     PEA02B                                 60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
      *  Delete the A/R batch dummy record.
      *
     C                   MOVE      *HIVAL        XXFND
     C                   MOVE      *HIVAL        XXEMP#
     C                   MOVE      *HIVAL        XXDIV
     C                   MOVE      *HIVAL        XXFYR
     C     EAKEY1        CHAIN     PEA02B                             60
     C  N60              DELETE    PEA02B
     C*
     C                   ENDSR                                                  SRDELB SUBR
     C*****************************************************************
      *===============================================================*
      * GetAutoCash - Ensure Auto Cash Dissolve is set up
      *===============================================================*
     p GetAutoCash     b
     d GetAutoCash     pi              n
     d  inCo#                              like(ELco#)
     d  inFnd                              like(ELfnd)
     d  inEmp#                             like(ELemp#)
     d  inDiv                              like(ELdiv)
     d  inFyr                              like(ELfyr)
     d  outDissFlg                     n
      /free
       // Don't wash non state rpt div with summarized A/R
       // through auto cash dissolve
       Chain (inCo#:inFnd:inEmp#:inDiv:inFyr) pel01;
       Chain (inCo#:inFnd:inEmp#:inDiv) pem01;
       If ELarsm = 'Y' and EMstrp <> 'Y';
          outDissFlg = *on;
       Endif;
       Chain (inCo#:inFnd) pf201;
       If %found and F2ccyf = 'Y' and F2cpmt = *blanks;
          Return *on;
       Endif;
       Return *off;
      /end-free
     p GetAutoCash     e
