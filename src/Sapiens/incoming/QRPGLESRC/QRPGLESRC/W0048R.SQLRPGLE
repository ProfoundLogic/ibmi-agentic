     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H*****************************************************************
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0048R - BILLING OPTIONS MAINTENANCE
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW THE USER TO SETUP      *
     H*                 VARIOUS OPTIONS AND CRITERIA FOR BILLING.     *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0010R   - WORK WITH GROUPS                                *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 11/13/03  PROGRAMMER: C:65  PROJECT: WC1167         *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/25/24 C:344 Project: 133WTD                                *
     H*                Load and store to the new field EXICPSGRP      *
     H*                'IVC Separate 3PY Batch by Group' in the WMEXP *
     H*                file.                                          *
     H*                                                               *
     H* 11/30/23 C:053 Support 114682                                 *
     H*                Load value to screen field.                    *
      //                                                               *
      //  04/28/22 C:33  Project: WC2670                               *
      //                 Disable the generate premium notice checkboxes*
      //                  if spool file output is not being used       *
     H*
     H* 08/10/21 c:079 Project WC2663
     H*                Made changes to Invoice Cloud billing options
     H*                Add WDF2P for access to fields needed for edit
     H*
     H* 08/02/21 c:318 Project WC2820
     H*                Added New fields for scoring vendors.
     H*
     H* 03/11/21 C:062 Project: WC2663
     H*                Add Invoice Cloud billing options.
     H*
     H* 05/12/20 C:136 Project: 150WEU
     H*                Add checkbox field for 'Exclude policy from
     H*                A/R change by Est/Rev Billing.
     H*
     H* 12/16/19 C:180 Project: 150WEH                                *
     H*                add SR payroll as Audit Payrolls remark code   *
     H*                                                               *
     H* 04/30/19 C:062 Project: 152WAB                                *
     H*                Apply Optacomp Mods.                           *
     H*                                                               *
     H* 08/07/17 C:65  PROJECT: PHW#48224                             *
     H*                Expand description field for deleted a/r msg.  *
     H*                                                               *
     H* 09/14/15 C:65  PROJECT: WC2375                                *
     H*                Add checkboxs for deferring prior audits on    *
     H*                a credit/debit transaction.                    *
     H*                                                               *
     H* 11/22/13 C:190 PROJECT: 129WSB                                *
     H*                Add checkbox for suppression of  prior year    *
     H*                balance on premium notice.                     *
     H*                                                               *
     H* 04/03/13 C:282 PROJECT: 147WBB                                *
     H*                Third Party Billing option will now be         *
     H*                accessible to client = '114'.                  *
     H*                                                               *
     H* 02/08/13 C:190 PROJECT: 140WAU                                *
     H*                Added prompt for estimating adjustment A/R     *
     H*                code.                                          *
     H*                                                               *
     H* 10/07/11 C:279 PROJECT: 133WLM                                *
     H*                 Include deposit in prior year balance for     *
     H*                 S/R online                                    *
     H*                                                               *
     H* 08/17/11 C:191 PROJECT: WC2048                                *
     H*                 Include prior year balance for S/R online     *
     H*                                                               *
     H* 04/27/11 C: 58 PROJECT: 114WYZ                                *
     H*                Add Flag: Due date P=Posting D=Due on receipt  *
     H*                                                               *
     H* 02/11/11 C:33  Project: WC1851                                *
     H*                Remove A/R codes from SCR02.                   *
     H*                                                               *
     H* 01/17/11 C:235 SY255 PRISS:-099                               *
     H*                F10 option is missing from SCR04               *
     H*                                                               *
     H* 09/29/10 C:220 PROJECT:WC1775                                 *
     H*                - Added new field SCARRI/EXARRI                *
     H*                - Validate values from Table file to be stored *
     H*                in EXARRI field.                               *
     H*                                                               *
     F* 09/07/10 C:255 PROJECT:WC1929                                 *
     F*                Added handling for new 'Disable 10/01/08 DE    *
     F*                Rate flag.' at scr04. 2nd Field at SCR04 will  *
     F*                be displayed conditionally instead whole screen*
     F*                for client 071.                                *
     F*                                                               *
     F* 05/24/10 C:61  PROJECT:WC1851                                 *
     F*                Added Error message "Updated by another user". *
     H*                to screen.                                     *
     H*                                                               *
     H* 05/18/10 C:136 PROJECT: WC1806                                *
     H*                REMOVE ACH INTERIM DAYS. IT MOVED TO BANK      *
     H*                ACCOUNT MASTER.                                *
     H*                                                               *
     H* 01/25/10 C:232 PROJECT: WC1851                                *
     H*                MODIFIED FOR THE ADMINISTRATIVE FEES FOR       *
     H*                COLLATERAL AND CLAIMS.                         *
     H*                                                               *
     F* 10/16/09 C:235 PROJECT:139WMD
     F*                NAME/ADDRESS ON TOP OF QUOTES & BILLINGS
     F*                                                               *
     F* 03/02/09 C:220 SUPPORT: 67388                                 *
     F*               -Billing Options Maintenance - Display underline*
     F*                in DSIPLAY ONLY mode.                          *
     F*                                                               *
     H* 12/31/08 C:146 PROJECT0 146WBW                                *
     H*                ADD NEW PROMPT FOR 3RD PARTY BILLING           *
     H*                                                               *
     H* 06/25/08 C:65  PROJECT: 114WRH                                *
     H*                ADD NEW FLAG TO PROCESS RETRO ACROSS GROUP AND *
     H*                FLAG ALL RETRO DEFAULTS ON MODULE FLAG.        *
     H*                                                               *
     H* 07/16/07 C:183 Project: 45WFO                                 *
     H*               -Added a new field to designate whether or not  *
     H*                policy years are to be included in the         *
     H*                calculation of the monthly interim prior bal.  *
     H*                                                               *
     H* 03/06/07 C:61  PROJECT 71WIF                                  *
     H*                Add prompt for "Spread revised billing         *
     H*                adjustments in the future default (Y/ )".      *
     H*                                                               *
     H* 12/22/06 C:132 PROJECT: 129WPH                                *
     H*                ADD field for "TAX BILLED AT AUDIT" for Client *
     H*                129 ONLY, and New field is SEARCHABLE.         *
     H*                                                               *
     H*  10/11/06  C:33  Seq#: WA099/41801                            *
     H*                  Use SC305R instead of SC580R.                *
     H*                                                               *
     H* 09/20/06 C:136 PROJECT: 71WHE (BACK TO BASE)                  *
     H*               -POPULATE LOCKBOX A/R CODES                     *
     H*               -POPULATE 'GENERATE PREMIUM NOTICE WITH' FLAGS  *
     H*                                                               *
     H* 02/16/06 C:132 PROJECT: 132WDP                                *
     H*                ADD field that will control Rounting of the    *
     H*                Installments, to the nearest $1.00.            *
     H*                                                               *
     H* 01/03/05 C:136 PROJECT: 132WBN                                *
     H*                ADD FIELD THAT WILL CONTROL OPTION TO LUMP     *
     H*                ADJUSTMENTS INTO ONE IN REVISED BILL.          *
     H*                                                               *
     H* 11/30/04 C:65  PROJECT: 132WBW                                *
     H*                ADD NEW FIELDS FOR ACCT. CURRENT INTERIM DAYS. *
     H*                                                               *
     H* 07/01/04 C:65  PROJECT: WC1167                                *
     H*                REMOVE REQUIREMENT ON QUOTE # DAYS FIELD.      *
     H*                                                               *
     H* 06/24/04 C:65  PROJECT: 114WHO                                *
     H*                ADD SCREEN FIELDS FOR RETRO PROGRAM DEFAULTS.  *
     H*                                                               *
     H* 06/03/04 C:65  PROJECT: WC1087                                *
     H*                ADD FLAG FOR BILLING ASSESSMENTS TO DIVISION   *
     H*                WITH HIGHEST MINIMUM PREMIUM.                  *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*                                                               *
     H*****************************************************************
     FW0048F    CF   E             WORKSTN
     F                                     Handler('PROFOUNDUI(HANDLER)')
     F                                     INFDS(FIDS01)
     FWMEXP     UF A E             DISK
     F                                     RENAME(PEX01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMEXL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     FWMFNP     IF   E             DISK
     FWTABL     IF   E           K DISK
     FWDF2L     if   e           k Disk
      *
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     FSTCTL1    IF   E           K DISK
     FSTSTL3    IF   E           K DISK
     F*  Generic table file logical
     FWTGTL     IF   E           K DISK
      /Copy SPRMODULE
      ***  Begin Add  ***  04/28/22  *****************
      /copy sPrXML
      ***  End   Add  ***  04/28/22  *****************
      /Copy wPrPolErr
      /COPY SCOPYMT01D
     d strP2GoHdr      s             40
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D OPTNM           C                   CONST('Billing Options Mai-
     D                                     ntenance')
     D COMAPI          C                   CONST('WA001R')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     d charMsg         s            100
      ***  Begin Add  ***  04/28/22  *****************
     dtipPrmNotDis     c                   'This option is disabled as -
     d                                     spooled file output is no longer -
     d                                     being generated.'
     disDisPrmNotAudit...
     d                 s              1n   inz
     disDisPrmNotEstimated...
     d                 s              1n   inz
     disDisPrmNotRevised...
     d                 s              1n   inz
      ***  End   Add  ***  04/28/22  *****************

     C*
     C     OFFPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##OFF             5
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Program parm list for call to W2493R
     C*
     C     SYSTPL        PLIST
     C                   PARM                    ##PRDS           40
     C                   PARM                    ##CALL            1
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY1
     C                   PARM                    ##RTCD            1
     C     Dtmdyyp       Plist
     C                   Parm                    srDti
     C                   Parm      '*CYMD'       srFfmt
     C                   Parm      *BLANKS       srTfmt
     C                   Parm                    srDto
     C                   Parm                    srErr
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY            10
     C*
     C*  KEY LIST FOR THE ADMIN/DARTS CONTROL FILE
     C*
     C     EXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*  KEY LIST FOR THE EMP A/C RECEIVABLE (WMEAL26)
     C     ARKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    ARCODE            2
      *
     C*    Generic table file key list
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY1
     C*
      *
      * *LIKE DEFINES
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY1
      *
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    ##RRN             9 0
     C*
     C/include SCOPYMT05C
     C*
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*  Various flags must be 'Y' or blank
     C*
     C  N90SCDMSG        IFNE      'Y'
     C     SCDMSG        ANDNE     *BLANK
     C                   SETON                                        21  90
     C                   ENDIF                                                  SCDMSG='Y'
     C*
     C  N90SCCMSG        IFNE      'Y'
     C     SCCMSG        ANDNE     *BLANK
     C                   SETON                                        22  90
     C                   ENDIF                                                  SCCMSG='Y'
     C*
     C  N90SCBEXP        IFNE      'Y'
     C     SCBEXP        ANDNE     *BLANK
     C                   SETON                                        23  90
     C                   ENDIF                                                  SCBEXP='Y'
     C*
     C  N90SCBEST        IFNE      'Y'
     C     SCBEST        ANDNE     *BLANK
     C                   SETON                                        24  90
     C                   ENDIF                                                  SCBEST='Y'
     C*
     C  N90SCBASM        IFNE      'Y'
     C     SCBASM        ANDNE     *BLANK
     C                   SETON                                        30  90
     C                   ENDIF                                                  SCBASM='Y'
     C*
     C  N90SCBTYP        IFNE      'Y'
     C     SCBTYP        ANDNE     *BLANK
     C                   SETON                                        29  90
     C                   ENDIF                                                  SCBTYP='Y'
     C*
     C  N90SCLUMP        IFNE      'Y'
     C     SCLUMP        ANDNE     *BLANK
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCLUMP<>'Y"
     C*
     C  N90SCRNDI        IFNE      'Y'
     C     SCRNDI        ANDNE     *BLANK
     C                   SETON                                        33  90
     C                   ENDIF                                                  SCRNDI<>'Y"
     C*
     C*  # days of valid quote cannot be less than or equal to zero.
     C*
     C  N90SC#DYS        IFLT      *ZERO
     C                   SETON                                        25  90
     C                   ENDIF
      *
      * "Spread Revised Billing Estimates in the future (Y/ )"
      *
     C  N90SCAJFL        IFNE      'Y'
     C     SCAJFL        ANDNE     *BLANK
     C                   SETON                                        72  90
     C                   ENDIF                                                  SCAJFL<>'Y"
      *
      *
     C* MOVED TO SCREEN 4
      *
     C  N90SCSGRA        IFNE      'Y'
     C     SCSGRA        ANDNE     *BLANK
     C                   SETON                                        75  90
     C                   ENDIF                                                  SCSGRA<>'Y"
     C  N90SC3RDB        IFNE      'Y'
     C     SC3RDB        ANDNE     *BLANK
     C                   SETON                                        74  90
     C                   ENDIF                                                  SC3RDB<>'Y'
     C*
     C*  INTERIM DAYS FOR AUDIT BILLING
     C*
     C  N90SCAUID        IFLT      *ZERO
     C                   SETON                                        26  90
     C                   ENDIF
     C*
     C*  INTERIM DAYS FOR ACCOUNT CURRENT BILLING - AUDIT
     C*
     C  N90SCAAID        IFLT      *ZERO
     C                   SETON                                        31  90
     C                   ENDIF
     C*
     C*  INTERIM DAYS FOR REVISED BILLING
     C*
     C  N90SCRVID        IFLT      *ZERO
     C                   SETON                                        27  90
     C                   ENDIF
     C*
     C*  INTERIM DAYS FOR ACCOUNT CURRENT BILLING - ORIGINAL/REVISED
     C*
     C  N90SCAOID        IFLT      *ZERO
     C                   SETON                                        32  90
     C                   ENDIF
     C*
     C* POP UP OVERRIDE WINDOW IF VISUAL DARTS IS NOT LOADED AND
     C* USER IS ATTEMPTING TO CHOOSE ENHANCED LAYOUT BILLS.
     C*
     C  N90SCBTYP        IFEQ      'Y'
     C     *IN95         ANDEQ     *OFF
     C*
     C                   MOVE      'N'           SCEOVR
     C                   EXFMT     WDWMSG
      *
     C     SCEOVR        IFEQ      'N'
     C                   MOVE      *BLANK        SCBTYP
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCBTYP IFEQ
     C*
     C*
     C*  INTERIM DAYS FOR FEE PROCESSING BILLING - DIRECT/CURRENT
     C*
     C     WCFL44        IFEQ      'Y'
     C*
     C  N90SCDFID        IFLT      *ZERO
     C                   SETON                                        66  90
     C                   ENDIF
     C*
     C  N90SCAFID        IFLT      *ZERO
     C                   SETON                                        67  90
     C                   ENDIF
     C*
     C                   ENDIF                                                  WCFL44 = 'Y'
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C*  ESTIMATING A/R CODE MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCESCD        IFNE      *BLANKS
     C                   MOVEL(P)  SCESCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              31
     C     *IN31         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C*
     C     *IN90         IFEQ      *OFF
     C     TTSTAT        ANDEQ     'I'
     C                   MOVE      *ON           *IN31
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C     *IN90         IFEQ      *OFF
     C     TTTYPE        ANDNE     'B'
     C                   MOVE      *ON           *IN32
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C                   ENDIF                                                  SCESCD IFNE
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  S/R A/R CODE MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSRCD        IFNE      *BLANKS
     C                   MOVEL(P)  SCSRCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              33
     C*
     C     *IN33         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     *IN90         IFEQ      *OFF
     C     TTSTAT        ANDEQ     'I'
     C                   MOVE      *ON           *IN33
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C     *IN90         IFEQ      *OFF
     C     TTTYPE        ANDNE     'H'
     C                   MOVE      *ON           *IN34
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C                   ENDIF                                                  SCESCD IFNE
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  DA A/R CODE MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDACD        IFNE      *BLANKS
     C                   MOVEL(P)  SCDACD        XXKEY
     C     TBKEY         CHAIN     PTT01                              35
     C*
     C     *IN35         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     *IN90         IFEQ      *OFF
     C     TTSTAT        ANDEQ     'I'
     C                   MOVE      *ON           *IN35
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C     *IN90         IFEQ      *OFF
     C     TTTYPE        ANDNE     'H'
     C                   MOVE      *ON           *IN36
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C                   ENDIF                                                  SCESCD IFNE
     C*
     C                   ENDIF                                                  *IN90 IFEQ
      *
      * AUDIT ADJUSTMENT A/R CODE MUST BE VALID
      *
     C     *IN90         IFEQ      *OFF
     C     SCAACD        IFNE      *BLANKS
     C                   MOVEL(P)  SCAACD        XXKEY
     C     TBKEY         CHAIN     PTT01                              37
      *
     C     *IN37         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN37 IFEQ
      *
     C     *IN90         IFEQ      *OFF
     C     TTTYPE        ANDNE     'D'
     C                   MOVE      *ON           *IN38
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C                   ENDIF                                                  SCAACD IFNE
     C*
     C                   ENDIF                                                  *IN90 IFEQ
      /free
       //*** Estimating adjustment A/R code must be valid.
       If *in90 = '0 ';
          If scEstAdj  <> ' ';
             xxKey = scEstAdj;
             Chain TBKEY PTT01;
             If not %Found;
                errEstAdj1 = *On;
                *in90 = *On;
             Endif;

             If *in90 = *Off and TTstat = 'I';
                errEstAdj2 = *On;
                *in90 = *On;
             Endif;

       //* Category type must be 'P' for Estimating adjustment code.
       //* Type must be 'A' for Estimating adjustment code.
             If *in90 = *OFF and (TTCTYP <> 'P'or TTTYPE <> 'A');
                errEstAdj3 = *On;
                *in90 = *ON;
             Endif;
          Endif;
       Endif;
      /end-free
      *
      * TAX BILLED AT AUDIT A/R CODE MUST BE VALID
      *
     C     *IN90         IFEQ      *OFF
     C     SCTBAC        IFNE      *BLANKS
     C                   MOVEL(P)  SCTBAC        XXKEY
     C     TBKEY         CHAIN     PTT01                              63
      *
     C     *IN63         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN63 IFEQ
      *
     C     *IN90         IFEQ      *OFF
     C     TTTYPE        ANDNE     'F'
     C                   MOVE      *ON           *IN61
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C                   ENDIF                                                  SCTBAC IFNE
     C*
     C                   ENDIF                                                  *IN90 IFEQ
      *
      *  QUOTE BILL REMARK CODE MUST BE VALID
      *
     C     *IN90         IFEQ      *OFF
     C     SCQUOT        IFNE      *BLANKS
     C     SCQUOT        CHAIN     PCT01                              41
     C*
     C     *IN41         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN41 IFEQ
     C*
     C     *IN90         IFEQ      *OFF
     C     CTSTAT        ANDEQ     'I'
     C                   MOVE      *ON           *IN42
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C                   ENDIF                                                  SCORG IFNE
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  ORIGINAL BILL REMARK CODE MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCORG         IFNE      *BLANKS
     C     SCORG         CHAIN     PCT01                              43
     C*
     C     *IN43         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN43 IFEQ
     C*
     C     *IN90         IFEQ      *OFF
     C     CTSTAT        ANDEQ     'I'
     C                   MOVE      *ON           *IN44
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C                   ENDIF                                                  SCORG IFNE
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  REVISED BILL REMARK CODE MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCREV         IFNE      *BLANKS
     C     SCREV         CHAIN     PCT01                              45
     C*
     C     *IN45         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN45 IFEQ
     C*
     C     *IN90         IFEQ      *OFF
     C     CTSTAT        ANDEQ     'I'
     C                   MOVE      *ON           *IN46
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C                   ENDIF                                                  SCREV IFNE
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  AUDIT BILL REMARK CODE MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCAUD         IFNE      *BLANKS
     C     SCAUD         CHAIN     PCT01                              47
     C*
     C     *IN47         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN47 IFEQ
     C*
     C     *IN90         IFEQ      *OFF
     C     CTSTAT        ANDEQ     'I'
     C                   MOVE      *ON           *IN48
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C                   ENDIF                                                  SCAUD IFNE
     C*
     C                   ENDIF                                                  *IN90 IFEQ
      *
      /Free
       // SR PAYROLL AUDIT

         If *in90 = *Off and SCaudSr <> *blanks;
            Chain SCaudsr PCT01;
            If not %found(STCTL1);
               *in80 = *On;
               *in90 = *On;
            Else;
               If CTstat = 'I';
                  *in80 = *On;
                  *in90 = *On;
               Endif;
            Endif;
         Endif;

      /End-Free

     C*
     C*  REVISED FINAL AUDIT BILL REMARK CODE MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRFAF        IFNE      *BLANKS
     C     SCRFAF        CHAIN     PCT01                              49
     C*
     C     *IN49         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN49 IFEQ
     C*
     C     *IN90         IFEQ      *OFF
     C     CTSTAT        ANDEQ     'I'
     C                   MOVE      *ON           *IN50
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C                   ENDIF                                                  SCAUD IFNE
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  SETTLEMENT PROCESS REMARK CODE MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSPCD        IFNE      *BLANKS
     C     SCSPCD        CHAIN     PCT01                              51
     C*
     C     *IN51         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN51 IFEQ
     C*
     C     *IN90         IFEQ      *OFF
     C     CTSTAT        ANDEQ     'I'
     C                   MOVE      *ON           *IN52
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C                   ENDIF                                                  SCSPCD IFNE
     C*
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C*  LOSS LIMITATION CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCSRTN        IFLT      *ZERO
     C                   SETON                                        53  90
     C                   ENDIF
     C*
     C*  EXCESS LOSS PREMIUM FACTOR CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCELP         IFLT      *ZERO
     C                   SETON                                        54  90
     C                   ENDIF
     C*
     C*  EXCESS LOSS PREM FACTOR - F CLASSES CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCELPF        IFLT      *ZERO
     C                   SETON                                        55  90
     C                   ENDIF
     C*
     C*  RETRO DEVELOPMENT FACTOR #1 CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCRDF1        IFLT      *ZERO
     C                   SETON                                        56  90
     C                   ENDIF
     C*
     C*  RETRO DEVELOPMENT FACTOR #2 CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCRDF2        IFLT      *ZERO
     C                   SETON                                        57  90
     C                   ENDIF
     C*
     C*  RETRO DEVELOPMENT FACTOR #3 CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCRDF3        IFLT      *ZERO
     C                   SETON                                        58  90
     C                   ENDIF
     C*
     C*  PROCESS RETRO ACROSS GROUP FLAG MUST BE VALID.
     C*
     C  N90SCRETG        IFNE      'Y'
     C     SCRETG        ANDNE     ' '
     C                   SETON                                        64  90
     C                   ENDIF
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90EXDT          IFGT      SAVEDT
     C     EXDT          OREQ      SAVEDT
     C     EXTIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  F2DT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C     ERRCK3        BEGSR
     C*
     C     SCOBFL        IFNE      'Y'
     C     SCOBFL        ANDNE     ' '
     C                   SETON                                        7190
     C                   ENDIF
     C*
     C  N90SCRBFL        IFNE      'Y'
     C     SCRBFL        ANDNE     ' '
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C  N90SCABFL        IFNE      'Y'
     C     SCABFL        ANDNE     ' '
     C                   SETON                                        7390
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCBLAR        ANDNE     *BLANK
     C                   MOVEL(P)  SCBLAR        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        7490
     C                   ELSE
     C     TTCTYP        IFNE      'P'
     C     TTTYPE        ORNE      'P'
     C     TTSTAT        ORNE      'A'
     C                   SETON                                        7490
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCATAR        ANDNE     *BLANK
     C                   MOVEL(P)  SCATAR        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        7590
     C                   ELSE
     C     TTCTYP        IFNE      'P'
     C     TTTYPE        ORNE      'P'
     C     TTSTAT        ORNE      'A'
     C                   SETON                                        7590
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDDAR        ANDNE     *BLANK
     C                   MOVEL(P)  SCDDAR        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        7690
     C                   ELSE
     C     TTCTYP        IFNE      'R'
     C     TTTYPE        ORNE      'Q'
     C     TTSTAT        ORNE      'A'
     C                   SETON                                        7690
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90EXDT          IFGT      SAVEDT
     C     EXDT          OREQ      SAVEDT
     C     EXTIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  F2DT>SAVEDT
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK3
     C*===============================================================
     C* ERRCK4 - SUBROUTINE FOR ERROR CHECKING SCR04
     C*===============================================================
     C     ERRCK4        BEGSR
     C*
     C* Validate from table file values to be stored in EXARRI
     C*
     C     SCARRI        IFNE      *BLANK
     C*
     C                   MOVEL     'DARE'        XXTBID
     C                   MOVEL(P)  SCARRI        XXKEY1
     C     GTKY          CHAIN     PGT01                              60
     C                   Clear                   scarrd
     C  N60              MOVEL     XXKEY1        SCARRI
     C  N60              MOVEL     GTDESC        SCARRD
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANK        SCARRI
     C                   MOVEL     *BLANKS       SCARRD
     C                   SETON                                        7590
     C                   ENDIF
     C                   ENDIF
     C*
      * "DISABLE 10/01/08 DE RATE SPLIT (Y/ )"
      *
     C  N90SCDE08        IFNE      'Y'
     C     SCDE08        ANDNE     *BLANK
     C                   SETON                                        31  90
     C                   ENDIF                                                  SCDE08<>'Y"
     C* VERIFY QUOTE ADDRESS IF ENTERED.
     C*
     C     SCQAD1        IFNE      *BLANKS
     C*
     C* CITY CANNOT BE BLANK.
     C*
     C  N90SCQCTY        IFEQ      *BLANKS
     C                   SETON                                        9071
     C                   ENDIF
     C*
     C* STATE MUST BE VALID.
     C*
     C                   MOVEL     SCQST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        9072
     C*
     C* ZIP CODE CANNOT BE ZERO.
     C*
     C  N90SCQZP1        IFEQ      *ZERO
     C                   SETON                                        9073
     C                   ENDIF                                                  SCMZP1=*ZERO
     C*
     C                   ENDIF                                                  SCQAD1 IFNE
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90EXDT          IFGT      SAVEDT
     C     EXDT          OREQ      SAVEDT
     C     EXTIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF
     C  N90SCPODU        IFEQ      *BLANKS
     C                   SETON                                        9078
     C                   ENDIF
     C  N90SCPODU        IFNE      'P'
     C     SCPODU        ANDNE     'D'
     C                   SETON                                        9079
     C                   ENDIF
     C*
     C  N90SCAYFL        IFNE      'Y'
     C     SCAYFL        ANDNE     *BLANK
     C                   SETON                                        76  90
     C                   ENDIF                                                  SCAYFL<>'Y"
     C*
     C  N90SCPYBS        IFNE      *BLANKS
     C     SCPYBS        ANDNE     'Y'
     C                   SETON                                        9077
     C                   ENDIF
     C  N90SCINDP        IFNE      *BLANKS
     C     SCINDP        ANDNE     'Y'
     C                   SETON                                        9081
     C                   ENDIF
      /Free
       erScrVndEx = *off;
       erScrVndEf = *off;
       msScrVndEx = *blanks;
       msScrVndEf = *blanks;
       msScrVnd   = *blanks;
       erScrVnd   = *off;
       // Validation Check for Effective Date
       If scScrVndEf <> *Zeros;
          erScrVndEf = ValidDate(scScrVndEf:charMsg);
          If erScrVndEf = *on;
             msScrVndEf = charMsg;
             *in90 = *on;
          Endif;
       Endif;
       If (scScrVnd <> *Blanks and scScrVndEf = *Zeros) or
          (scScrVnd =  *Blanks and scScrVndEf <> *Zeros);
          msScrVnd = 'WCR0544';
          erScrVnd = *on;
          msScrVndEf = 'WCR0544';
          erScrVndEf = *on;
          *in90 = *On;
       Endif;
       //Scoring Vendor  effective date and expiry date are mutually inclusive
       If (scScrVndEf = *zeros and scScrVndEx <> *Zeros);
          msScrVndEx = 'WCR0544';
          erScrVndEx = *on;
          msScrVndEf = 'WCR0544';
          erScrVndEf = *on;
          *in90 = *On;
       Endif;
       // Validation Check for Expiration Date
       If  scScrVndEx <> *Zeros;
           erScrVndEx = ValidDate(scScrVndEx:charMsg);
           If erScrVndEx = *on;
              msScrVndEx = charMsg;
              *in90 = *On;
           Endif;
       Endif;
       // Dates must be in Ascending order
       If scScrVndEf <> *zeros and scScrVndEx <> *zeros
          and  erScrVndEf = *off and erScrVndEx = *off;
          erScrVndEx = AscendDates(scScrVndEf:scScrVndEx:charMsg);
          If erScrVndEx = *on;
             erScrVndEf = *on;
             msScrVndEf = charMsg;
             msScrVndEx = charMsg;
             *in90 = *On;
          Endif;
       Endif;
      /end-free

     C*
      *
     C                   ENDSR                                                  ERRCK4
     C*===============================================================
     C* ERRCK5 - SUBROUTINE FOR ERROR CHECKING SCR05
     C*===============================================================
     C     ERRCK5        BEGSR
     C                   ENDSR                                                  ERRCK5
      *===============================================================
      * KJDO - SUBROUTINE FOR F10
      *===============================================================
      *
      *  F10=SEARCH
      *
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
      * A/R/ TRANSACTION CODE SEARCH
      *
     C     HFLD          WHENEQ    'SCESCD'
     C     HFLD          OREQ      'SCSRCD'
     C     HFLD          OREQ      'SCDACD'
     C     HFLD          OREQ      'SCAACD'
     C     HFLD          OREQ      'SCBLAR'
     C     HFLD          OREQ      'SCATAR'
     C     HFLD          OREQ      'SCDDAR'
     C     HFLD          OREQ      'SCTBAC'
      *
     C     HFLD          OREQ      'SCCFBA'
     C     HFLD          OREQ      'SCCOBA'
     C     HFLD          OREQ      'SCESTADJ'
      *
     C                   MOVE      *BLANKS       SCKEY            10
      *
     C                   CALL      'W2161R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
      *
     C     *INKL         IFNE      *ON
     C     SCKEY         ANDNE     *BLANKS
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCESCD'
     C                   MOVEL     SCKEY         SCESCD
     C                   MOVE      *ON           *IN22
      *
     C     HFLD          WHENEQ    'SCSRCD'
     C                   MOVEL     SCKEY         SCSRCD
     C                   MOVE      *ON           *IN23
      *
     C     HFLD          WHENEQ    'SCDACD'
     C                   MOVEL     SCKEY         SCDACD
     C                   MOVE      *ON           *IN24
      /free
       When HFLD = 'SCESTADJ';
           scEstAdj = scKey;
           If *In59 = *On;
              *IN62 = '1';
           Else;
              *IN25 = '1';
           Endif;
      /end-free
      *
     C     HFLD          WHENEQ    'SCAACD'
     C                   MOVEL     SCKEY         SCAACD
      /free
       If *in91 = '0';
           *in92 = *On;
       Endif;
      /end-free
     C   91              MOVE      *ON           *IN68
      *
     C     HFLD          WHENEQ    'SCBLAR'
     C                   MOVEL     SCKEY         SCBLAR
     C                   MOVE      *ON           *IN22
      *
     C     HFLD          WHENEQ    'SCATAR'
     C                   MOVEL     SCKEY         SCATAR
     C                   MOVE      *ON           *IN23
      *
     C     HFLD          WHENEQ    'SCDDAR'
     C                   MOVEL     SCKEY         SCDDAR
     C                   MOVE      *ON           *IN24
      *
     C     HFLD          WHENEQ    'SCAACD'
     C                   MOVEL     SCKEY         SCAACD
      /free
       If *in91 = '0';
           *in92 = *On;
       Endif;
      /end-free
     C   91              MOVE      *ON           *IN68
      *
     C     HFLD          WHENEQ    'SCTBAC'
     C                   MOVEL     SCKEY         SCTBAC
     C  N91              MOVE      *ON           *IN25
     C   91              MOVE      *ON           *IN69
      *
      *
     C                   ENDSL                                                  HFLD WHEQ
      *
     C                   ELSE
      *
     C     HFLD          COMP      'SCESCD'                               21
     C     HFLD          COMP      'SCSRCD'                               22
     C     HFLD          COMP      'SCDACD'                               23
     C     HFLD          COMP      'SCAACD'                               24
     C     HFLD          COMP      'SCBLAR'                               21
     C     HFLD          COMP      'SCATAR'                               22
     C     HFLD          COMP      'SCDDAR'                               23
     C     HFLD          COMP      'SCTBAC'                               62
     C     HFLD          COMP      'SCCFBA'                               68
     C     HFLD          COMP      'SCCOBA'                               69
     C     HFLD          COMP      'SCESTADJ'                             92
      *
     C                   ENDIF                                                  SCKEY IFNE
      *
      * DARTS REMARK CODE SEARCH
      *
     C     HFLD          WHENEQ    'SCORG'
     C     HFLD          OREQ      'SCREV'
     C     HFLD          OREQ      'SCAUD'
     C     HFLD          OREQ      'SCRFAF'
     C     HFLD          OREQ      'SCQUOT'
     C     HFLD          OREQ      'SCSPCD'
     C     HFLD          OREQ      'SCAUDSR'
      *
     C                   MOVE      *BLANKS       QQRMCD            5
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C     QQRMCD        PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     QQRMCD        IFNE      *BLANKS
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCQUOT'
     C                   MOVEL     QQRMCD        SCQUOT
     C                   MOVE      *ON           *IN26
      *
     C     HFLD          WHENEQ    'SCORG'
     C                   MOVEL     QQRMCD        SCORG
     C                   MOVE      *ON           *IN27
      *
     C     HFLD          WHENEQ    'SCREV'
     C                   MOVEL     QQRMCD        SCREV
     C                   MOVE      *ON           *IN28
      *
     C     HFLD          WHENEQ    'SCAUD'
     C                   MOVEL     QQRMCD        SCAUD
     C                   MOVE      *ON           *IN29

     C     HFLD          WHENEQ    'SCRFAF'
     C                   MOVEL     QQRMCD        SCRFAF
     C                   MOVE      *ON           *IN82
      *
     C     HFLD          WHENEQ    'SCAUDSR'
     C                   MOVEL     QQRMCD        SCAUDSR
     C                   MOVE      *ON           *IN30
      *
     C     HFLD          WHENEQ    'SCSPCD'
     C                   MOVEL     QQRMCD        SCSPCD
     C                   MOVE      *ON           *IN39
      *
     C                   ENDSL                                                  HFLD WHEQ
      *
     C                   ELSE
      *
     C     HFLD          COMP      'SCQUOT'                               25
     C     HFLD          COMP      'SCORG'                                26
     C     HFLD          COMP      'SCREV'                                27
     C     HFLD          COMP      'SCAUD'                                28
     C     HFLD          COMP      'SCAUDSR'                              82
     C     HFLD          COMP      'SCRFAF'                               29
     C     HFLD          COMP      'SCSPCD'                               30
      *
     C                   ENDIF                                                  QQRMCD IFNE
     C     HFLD          WHENEQ    'SCARRI'
      *
     C                   MOVEL     'DARE'        XXTBID
     C*
     C                   CALL      'W2493R'      SYSTPL
     C*
     C                   MOVEL     *BLANKS       SCARRD
     C     ##RTCD        IFEQ      '*'
     C*
     C     GTKY          CHAIN     PGT01                              61
     C                   MOVEL     XXKEY1        SCARRI
     C  N61              MOVEL     GTDESC        SCARRD
     C                   SETON                                        46
     C                   ELSE
     C                   SETON                                        45
     C                   ENDIF                                                  ##RTCD=*
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      EXCMSG        SCCMSG
     C                   MOVE      EXDMSG        SCDMSG
     C                   MOVE      EXBEXP        SCBEXP
     C                   MOVE      EXBEST        SCBEST
     C                   MOVE      EXBASM        SCBASM
     C                   MOVE      EXBTYP        SCBTYP
     C                   MOVE      EX#DYS        SC#DYS
     C                   MOVE      EXAJFL        SCAJFL
     C                   MOVE      EX3RDB        SC3RDB
     C                   MOVE      EXAYFL        SCAYFL
     C                   MOVE      EXSGRA        SCSGRA
     C                   MOVE      EXAUID        SCAUID
     C                   MOVE      EXRVID        SCRVID
     C                   MOVE      EXAAID        SCAAID
     C                   MOVE      EXAOID        SCAOID
     C*
     C                   MOVEL     EXBTXT        SCBTX1
     C                   MOVEL     EXBTXT        XXTEMP          160
     C                   MOVE      XXTEMP        SCBTX2
     C                   MOVE      EXBTXT        SCBTX3
     C*
     C                   MOVE      EXESCD        SCESCD
     C                   MOVE      EXSRCD        SCSRCD
     C                   MOVE      EXDACD        SCDACD
     C                   MOVE      EXAUCD        SCAACD
     C                   MOVE      EXTBAC        SCTBAC
     C                   MOVE      EXQTE         SCQUOT
     C                   MOVE      EXORG         SCORG
     C                   MOVE      EXREV         SCREV
     C                   MOVE      EXAUD         SCAUD
     C                   MOVE      EXAUDSR       SCAUDSR
     C                   MOVE      EXRFAF        SCRFAF
     C                   MOVE      EXSPCD        SCSPCD
     C                   Z-ADD     EXSRTN        SCSRTN
     C                   Z-ADD     EXELP         SCELP
     C                   Z-ADD     EXELPF        SCELPF
     C                   Z-ADD     EXRDF1        SCRDF1
     C                   Z-ADD     EXRDF2        SCRDF2
     C                   Z-ADD     EXRDF3        SCRDF3
     C                   MOVE      EXLUMP        SCLUMP
        // Load Invoice Cloud fields
        scICGrp   = EXicGrp;
        scICQte   = EXicQte;
        scICPayAR = EXicPayAR;
        scICPayAR = EXicPayAR;
        scICP2GAR = EXicP2GAR;
        scICAmpAR = EXicAmpAR;
        //*** Begin Add *** 11/30/23 *******************************
        scICCXInv = EXicCxInv;
        //*** End Add *** 11/30/23 *******************************
        //*** Begin Add *** 01/25/24 *** 133WTD ****************************
        scICpsGrp = EXicPsGrp;
        //*** End   Add *** 01/25/24 *** 133WTD ****************************
      *
     C                   MOVE      EXRNDI        SCRNDI
     C                   MOVE      EXOBFL        SCOBFL
     C                   MOVE      EXRBFL        SCRBFL
     C                   MOVE      EXABFL        SCABFL
     C                   MOVE      EXBLAR        SCBLAR
     C                   MOVE      EXATAR        SCATAR
     C                   MOVE      EXDDAR        SCDDAR
     C                   MOVEL     EXQAD1        SCQAD1
     C                   MOVEL     EXQAD2        SCQAD2
     C                   MOVEL     EXQCTY        SCQCTY
     C                   MOVEL     EXQST         SCQST
     C                   MOVEL     EXQZIP        SCQZP1
     C                   MOVE      EXQZIP        SCQZP2
     C                   MOVE      EXRETG        SCRETG
     C                   MOVE      EXDPAC        SCDPAC
     C                   MOVE      EXDPAD        SCDPAD
     C*
     C                   MOVE      EXDFID        SCDFID
     C                   MOVE      EXAFID        SCAFID
     C                   MOVEL     EXDE08        SCDE08
     C                   MOVEL     EXARRI        SCARRI
     C                   MOVEL     'DARE'        XXTBID
     C                   MOVEL(P)  SCARRI        XXKEY1
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     XXKEY1        SCARRI
     C  N60              MOVEL     GTDESC        SCARRD
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANK        SCARRI
     C                   MOVEL     *BLANKS       SCARRD
     C                   ENDIF
     C                   MOVE      EXPODU        SCPODU
     C                   MOVE      EXPYBS        SCPYBS
     C                   MOVE      EXINDP        SCINDP
     C                   Eval      scScrVnd = EXscrVnd
     C                   Z-Add     EXscrVndEf    srDti
     C                   Call      'SRCVTDAT'    Dtmdyyp
     C                   MoveL     srDto         scScrVndEf
     C                   Z-Add     EXscrVndEx    srDti
     C                   Call      'SRCVTDAT'    Dtmdyyp
     C                   MoveL     srDto         scScrVndEx
      /free
       scEstAdj = EXeacd;
       scSupprBal = EXspyb;
       scExcArBl = EXexcArBl;
      /end-free
       // Load hidden screen fields for IC Edits on Check boxes
       Chain (EXco#:EXfnd) pf201;
       If %Found(WDF2L);
          scCpYf = F2cPyf;
          scCcYf = F2cCyf;
          scCrYf = F2cRyf;
          //*** Begin Chg *** 03/16/22 *******************************
          //*** scNpCd = F2npCd;
          //*** scNxCd = F2nxCd;
          scNpCd = 'N';
          scNxCd = 'N';
          If F2npCd <> ' ';
             scNpCd = 'Y';
          EndIf;
          If F2nxCd <> ' ';
             scNxCd = 'Y';
          EndIf;
          //*** End   Chg *** 03/16/22 *******************************
       Endif;
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVE      SCCMSG        EXCMSG
     C                   MOVE      SCDMSG        EXDMSG
     C                   MOVE      SCBEXP        EXBEXP
     C                   MOVE      SCBEST        EXBEST
     C                   MOVE      SCBASM        EXBASM
     C                   MOVE      SCBTYP        EXBTYP
     C                   MOVE      SC#DYS        EX#DYS
     C                   MOVE      SCAJFL        EXAJFL
     C                   MOVE      SC3RDB        EX3RDB
     C                   MOVE      SCAYFL        EXAYFL
     C                   MOVE      SCSGRA        EXSGRA
     C                   MOVE      SCAUID        EXAUID
     C                   MOVE      SCRVID        EXRVID
     C                   MOVE      SCAAID        EXAAID
     C                   MOVE      SCAOID        EXAOID
     C*
     C                   MOVEL(P)  SCBTX1        XXTEMP
     C                   MOVE      SCBTX2        XXTEMP
     C                   MOVEL(P)  XXTEMP        EXBTXT
     C                   MOVE      SCBTX3        EXBTXT
     C*
     C                   MOVE      SCESCD        EXESCD
     C                   MOVE      SCSRCD        EXSRCD
     C                   MOVE      SCDACD        EXDACD
     C                   MOVE      SCAACD        EXAUCD
     C                   MOVE      SCTBAC        EXTBAC
     C                   MOVE      SCQUOT        EXQTE
     C                   MOVE      SCORG         EXORG
     C                   MOVE      SCREV         EXREV
     C                   MOVE      SCAUD         EXAUD
     C                   MOVE      SCAUDSR       EXAUDSR
     C                   MOVE      SCRFAF        EXRFAF
     C                   MOVE      SCSPCD        EXSPCD
     C                   Z-ADD     SCSRTN        EXSRTN
     C                   Z-ADD     SCELP         EXELP
     C                   Z-ADD     SCELPF        EXELPF
     C                   Z-ADD     SCRDF1        EXRDF1
     C                   Z-ADD     SCRDF2        EXRDF2
     C                   Z-ADD     SCRDF3        EXRDF3
     C                   MOVE      SCLUMP        EXLUMP
        // Update Invoice Cloud fields
        EXicGrp   = scICGrp;
        EXicQte   = scICQte;
        EXicPayAR = scICPayAR;
        EXicCxInv = scICCXInv;
        EXicP2GAR = scICP2GAR;
        EXicAmpAR = scICAmpAR;
        //*** Begin Add *** 11/30/23 *******************************
        scICCXInv = EXicCxInv;
        //*** End Add *** 11/30/23 *******************************
        //*** Begin Add *** 01/25/24 *** 133WTD ****************************
        EXicPsGrp = scICpsGrp;
        //*** Begin Add *** 01/25/24 *** 133WTD ****************************
      *
     C                   MOVE      SCRNDI        EXRNDI
     C                   MOVE      SCOBFL        EXOBFL
     C                   MOVE      SCRBFL        EXRBFL
     C                   MOVE      SCABFL        EXABFL
     C                   MOVE      SCBLAR        EXBLAR
     C                   MOVE      SCATAR        EXATAR
     C                   MOVE      SCDDAR        EXDDAR
     C                   MOVEL     SCQAD1        EXQAD1
     C                   MOVEL     SCQAD2        EXQAD2
     C                   MOVEL     SCQCTY        EXQCTY
     C                   MOVEL     SCQST         EXQST
     C                   MOVEL     SCQZP1        EXQZIP
     C                   MOVE      SCQZP2        EXQZIP
     C                   MOVE      SCRETG        EXRETG
     C                   MOVE      SCDPAC        EXDPAC
     C                   MOVE      SCDPAD        EXDPAD
     C*
     C                   MOVEL     SCDFID        EXDFID
     C                   MOVEL     SCAFID        EXAFID
     C                   MOVEL     SCDE08        EXDE08
     C                   MOVEL     SCARRI        EXARRI
     C                   MOVE      SCPODU        EXPODU
     C                   MOVE      SCPYBS        EXPYBS
     C                   MOVE      SCINDP        EXINDP
     C                   Eval      EXscrVnd = scScrVnd
     C                   Z-ADD     scScrVndEf    srDti
     C                   CALL      'SRCVTDAT'    Dtcymd
     C                   MOVEL     srDto         EXscrVndEf
     C                   Z-ADD     scScrVndEx    srDti
     C                   CALL      'SRCVTDAT'    Dtcymd
     C                   MOVEL     srDto         EXscrVndEx
      /free
       EXeacd = scEstAdj;
       EXspyb = scSupprBal;
       EXexcArBl = scExcArBl;
      /end-free
     C*
     C                   ENDSR                                                  MVSCDB
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   Eval      isWC2820 = '0'
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C     ##RRN         CHAIN     PFN01                              60
     C  N60              Z-ADD     FNFND         SCFND
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C     EXKY          CHAIN     PEX01                              60
     C  N60              Z-ADD     DBRRN2        @@RRN             9 0
     C   60
     CANN09              MOVEL     'A'           @@MODE
     C*
     C     'WC'          CHAIN     SWC01                              60
     C  N60WCFL24        COMP      'Y'                                    94
     C  N60WCFL11        COMP      'Y'                                    96
     C*
     C     'SC'          CHAIN     SDR01                              60
     C  N60DRFL10        COMP      'Y'                                    95
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C     CLTID         IFEQ      '071'
     C                   SETON                                        97
     C                   ELSE
     C                   SETOFF                                       97
     C                   ENDIF
     C                   Z-ADD     4             #OFSCR            1 0
      *
     C     CLTID         IFEQ      '129'
     C                   MOVE      *ON           *IN59
     C                   ENDIF
     C*
     C                   MOVE      X'A7'         AT3P                           Non-Dsp Prot
     C                   MOVE      X'A7'         AT3F                           Non-Dsp Prot
      /Free
       If CLTID = '146' Or CLTID = '114' Or CLTID = '152';
      /End-Free
     C                   MOVE      X'20'         AT3P                           Normal
     C     *IN09         IFEQ      *ON                                          Underline
     C                   MOVE      X'A2'         AT3F                           Underline
     C                   ELSE                                                   Underline
     C                   MOVE      X'24'         AT3F                           Underline
     C                   ENDIF                                                  Underline
      /Free
       EndIf;
      /End-Free
     C*
     C* CHECK FLAG WCFL44 FOR FEE INFORMATION.....
      *
     C     WCFL44        IFEQ      'Y'
     C                   SETON                                        91
     C                   ELSE
     C                   SETOFF                                       91
     C                   ENDIF
      *
     C*
     C*
     C* FIELD WILL ONLY DISPLAY IF THE S/R ONLINE MODULE IS INSTALLED
     C     WCFL38        IFEQ      'Y'
     C                   SETON                                        94
     C                   ELSE
     C                   SETOFF                                       94
     C                   ENDIF
       // Let's see if Invoice Cloud is Installed
       isInvCloud = isModEnabled('Invoice Cloud');
       // Let's get the Invoice Cloud Header
       scICHeader = GetModuleProperty('Invoice Cloud':'Name');
       // Let's get the S/R Online Header and build the flexible headings for Invoice Cloud
       strP2GoHdr = GetModuleProperty('S/R Online':'Name');
       lblICgrp = 'Include Group for ' + %Trim(scICHeader);
       lblICqte = 'Include Quote Bills for ' + %Trim(scICHeader);
       lblICPayAR = %Trim(scICHeader) + ' Payment Direct to A/R';
       lblICp2GAR = %Trim(strP2GoHdr) + ' Payment Direct to A/R';
       lblICcxInv = %Trim(scICHeader) + ' Inactivate Invoices after CX';
       //***  Begin Add  ***  01/25/24  ************************************
       lblICpsGrp = 'Separate 3PY Batches by Group';
       //***  End   Add  ***  01/25/24  ************************************

       //***  Begin Add  ***  04/28/22  *****************
       // Disable premium notice flags?
       isDisPrmNotAudit = not UseSpoolOut('Audit Bill');
       isDisPrmNotEstimated = not UseSpoolOut('Estimated Bill');
       isDisPrmNotRevised = not UseSpoolOut('Revised Bill');
       Clear tipABFl;
       Clear tipOBFl;
       Clear tipRBFl;
       disABFl = isDisPrmNotAudit;
       disOBFl = isDisPrmNotEstimated;
       disRBFl = isDisPrmNotRevised;
       Select;
       When *in09; // Inquiry
          If isDisPrmNotAudit;
             tipABFl = tipPrmNotDis;
          Endif;
          If isDisPrmNotEstimated;
             tipOBFl = tipPrmNotDis;
          Endif;
          If isDisPrmNotRevised;
             tipRBFl = tipPrmNotDis;
          Endif;
       Endsl;
       //***  End   Add  ***  04/28/22  *****************
     C*
     C                   ENDSR                                                  PGMSTR
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B02  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        EXUSER
     C                   MOVEL     Q1PGM         EXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EXDT
     C                   MOVEL     DSTIME        EXTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E02  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   Z-ADD     FNCO#         EXCO#
     C                   Z-ADD     SCFND         EXFND
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        EXIUSR
     C                   MOVEL     Q1USER        EXUSER
     C                   MOVEL     Q1PGM         EXIPGM
     C                   MOVEL     Q1PGM         EXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EXIDT
     C                   MOVEL     SRDTO         EXDT
     C                   MOVEL     DSTIME        EXITM
     C                   MOVEL     DSTIME        EXTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMEXP
     C*
     C                   MOVEL     'C'           @@MODE
     C*
     C                   ENDSR                                                  WRITER
