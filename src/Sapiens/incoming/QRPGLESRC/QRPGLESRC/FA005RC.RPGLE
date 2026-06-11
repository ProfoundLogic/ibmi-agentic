     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*******************************************************************
     H/TITLE FA005R - API to Access/Update/Write FMPDP & FMP1P
     H*******************************************************************
     H*  Description:                                                   *
     H*                                                                 *
     H*    Using @@MODE, execute the proper file functions:             *
     H*    For 'C' (Update) or 'D' (Display), Access FMPDP and FMP1P    *
     H*    using FEIN/MOD to obtain the corresponding RRNs.             *
     H*    For 'C' (Update) or 'A' (Write), Assign FMPDP and FMP1P      *
     H*    field values from the WCMS WMPDP access.                     *
     H*    For 'C' (Update), update FMPDP and FMP1P.                    *
     H*    For 'A' (Write), write FMPDP and FMP1P.                      *
     H*    Return RRN values for both files to pass to F0031R1.         *
     H*                                                                 *
     H*  Called From:                                                   *
     H*    W0030R1                                                      *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*    None                                                         *
     H*                                                                 *
     H*  Created:  11/06/96  C:30  Project:  68MDI                      *
     H*                                                                 *
     H*******************************************************************
     H*  Revisions:                                                     *
     H*                                                                 *
     H* 06/19/14 C:280 Support: 82449
     H*                Add code to populate FMP1P fields
     H*                Add new fields to parm list and qualify use
     H*                by using q1prms
     H*                                                                 *
     H* 12/10/07 C:198 Project: FL064                                 *
     H*                Expanded license# field to 13 characters.      *
     H*                                                               *
     H*  04/10/01 C:62 Project: MC244                                   *
     H*                CHANGE PROCESSING.                               *
     H*                                                                 *
     H*  07/30/98 C:62 Project: WC082 12658  MC033 12687                *
     H*                PHYSICAL ADDRESS BEING ASSIGNED PREVIOUS ENTRY   *
     H*                PHYSICAL ADDRESS.                                *
     H*                                                                 *
     H*  02/13/98 C:53 Project: 60MBS                                   *
     H*           P1PARN NOW BEING UPDATED WITH A BLANK                 *
     H*                                                                 *
     H*  03/26/97 C:30 Project: 87MBQ                                   *
     H*           Fields PDWCDT & PDBWC# added.                         *
     H*                                                                 *
     H*  07/03/97 C:31  Project: 87MCD
     H*                 Loading BWC Provider Types
     H*                 1. Add provider type (PDBWCT).
     H*                                                                 *
     H*******************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*    40 *OFF - FMPDP rec found.  Update record.                   *
     H*    40 *ON  - FMPDP rec not found.  Write record.                *
     H*    41 *OFF - FMP1P rec found.  Update record.                   *
     H*    41 *ON  - FMP1P rec not found.  Write record.                *
     H*    60 - General File Access                                     *
     H*******************************************************************
      *
      * MCMS Provider file
     FFMPDL     UF A E           K DISK
     F                                     INFDS(DBFDS1)
      *
      * MCMS Provider Auxiliary file
     FFMP1L     UF A E           K DISK
     F                                     INFDS(DBFDS2)
      *

     D DBFDS1          DS
     D  DBRRN1               397    400B 0
      *
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*** BEGIN ADD *** 04/10/01 **************************************
     D NAME            C                   CONST('Prov Type chgd in W-
     D                                     CMS to non-Medical-
     D                                      type.')

     I*** END   ADD *** 04/10/01 **************************************
      *

     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
      *
     C     P1KY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
      *
     C     *LIKE         DEFINE    PDFEIN        XXFEIN
     C     *LIKE         DEFINE    PDMOD         XXMOD
     C     *LIKE         DEFINE    PDFEIN        WPFEIN
     C     *LIKE         DEFINE    PDMOD         WPMOD
     C     *LIKE         DEFINE    PDNAME        WPNAME
     C     *LIKE         DEFINE    PDFSN         WPFSN
     C     *LIKE         DEFINE    PDTITL        WPTITL
     C     *LIKE         DEFINE    PDADR1        WPADR1
     C     *LIKE         DEFINE    PDCTY         WPCTY
     C     *LIKE         DEFINE    PDST          WPST
     C     *LIKE         DEFINE    PDZIP         WPZIP
     C     *LIKE         DEFINE    PDAREA        WPAREA
     C     *LIKE         DEFINE    PDPHN#        WPPHN#
     C     *LIKE         DEFINE    PDSHNM        WPSHNM
     C     *LIKE         DEFINE    PD1099        WP1099
     C     *LIKE         DEFINE    PDTYPE        WPTYPE
     C     *LIKE         DEFINE    PDSPCD        WPSPCD
     A*** Begin Chg *** 12/10/07 **************************************
     C********** *LIKE     DEFN PDFLIC    WPFLIC
     C     *LIKE         DEFINE    PDLIC#        WPLIC#
     A*** End   Chg *** 12/10/07 **************************************
     C     *LIKE         DEFINE    PDVFLG        WPVFLG
     C     *LIKE         DEFINE    PDADR2        WPADR2
     C     *LIKE         DEFINE    PDDBA1        WPDBA1
     C     *LIKE         DEFINE    PDFXAR        WPFXAR
     C     *LIKE         DEFINE    PDFAX#        WPFAX#
     C     *LIKE         DEFINE    PDWCCE        WPWCCE
     C     *LIKE         DEFINE    PDSTAT        WPSTAT
     C     *LIKE         DEFINE    PDWCDT        WPWCDT
      *** BEGIN ADD *** 03/26/97 ***********************************************
     C     *LIKE         DEFINE    PDBWC#        WPBWC#
      *** END   ADD *** 03/26/97 ***********************************************
     C*** BEGIN ADD ** 07/03/97 ************************************************
     C     *LIKE         DEFINE    PDBWCT        WPBWCT
     C*** END ADD ** 07/03/97 **************************************************
     C*
     C*** Beg add ***** 06/19/14 ***********************************************
     C*
     C     *LIKE         DEFINE    P1NAME        WP1NAME
     C     *LIKE         DEFINE    P1FSN         WP1FSN
     C     *LIKE         DEFINE    P1ADR1        WP1ADR1
     C     *LIKE         DEFINE    P1ADR2        WP1ADR2
     C     *LIKE         DEFINE    P1CTY         WP1CTY
     C     *LIKE         DEFINE    P1PST         WP1PST
     C     *LIKE         DEFINE    P1ZIP         WP1ZIP
     C     *LIKE         DEFINE    P1AREA        WP1AREA
     C     *LIKE         DEFINE    P1PHN#        WP1PHN#
     C*
     C*** End add ***** 06/19/14 ***********************************************
     C*
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *          M A I N     L O G I C
      *
     C     *ENTRY        PLIST
01   C                   PARM                    @@MODE            1
 2   C                   PARM                    QQMCPD            9 0          FMPDP RRN
 3   C                   PARM                    QQMCP1            9 0          FMP1P RRN
 4   C                   PARM                    QQRTCD            1            Return Code
 5   C                   PARM                    WPFEIN
 6   C                   PARM                    WPMOD
 7   C                   PARM                    WPNAME
 8   C                   PARM                    WPFSN
 9   C                   PARM                    WPTITL
10   C                   PARM                    WPADR1
 1   C                   PARM                    WPCTY
 2   C                   PARM                    WPST
 3   C                   PARM                    WPZIP
 4   C                   PARM                    WPAREA
 5   C                   PARM                    WPPHN#
 6   C                   PARM                    WPSHNM
 7   C                   PARM                    WP1099
 8   C                   PARM                    WPTYPE
 9   C                   PARM                    WPSPCD
20   A*** Begin Chg *** 12/10/07 **************************************
     C*****************    PARM           WPFLIC
 1   C                   PARM                    WPLIC#
     A*** End   Chg *** 12/10/07 **************************************
 2   C                   PARM                    WPVFLG
 3   C                   PARM                    WPADR2
 4   C                   PARM                    WPDBA1
 5   C                   PARM                    WPFXAR
 6   C                   PARM                    WPFAX#
 7   C                   PARM                    WPWCCE
 8   C                   PARM                    WPSTAT
      *** BEGIN ADD *** 03/26/97 ***********************************************
 9   C                   PARM                    WPWCDT
30   C                   PARM                    WPBWC#
      *** END   ADD *** 03/26/97 ***********************************************
     C*** BEGIN ADD ** 07/03/97 ************************************************
 1   C                   PARM                    WPBWCT
     C*** END ADD ** 07/03/97 **************************************************
     C*
     C*** Beg add ***** 06/19/14 ***********************************************
     C*
32   C                   PARM                    WP1NAME
 3   C                   PARM                    WP1FSN
 4   C                   PARM                    WP1ADR1
 5   C                   PARM                    WP1ADR2
 6   C                   PARM                    WP1CTY
 7   C                   PARM                    WP1PST
 8   C                   PARM                    WP1ZIP
 9   C                   PARM                    WP1AREA
40   C                   PARM                    WP1PHN#
     C*
     C*
     C                   CLEAR                   PPD01
     C                   CLEAR                   PP101
      *
      *  Access WCMC Provider and Provider Auxiliary file using FEIN/MOD
      *
     C                   Z-ADD     WPFEIN        XXFEIN
     C                   Z-ADD     WPMOD         XXMOD
      *
     C     PDKY          CHAIN     PPD01                              40
      *
     C     P1KY          CHAIN     PP101                              41
      *
      *  Execute specific tasks based on function requested (@@MODE).
      *
     C     @@MODE        IFEQ      'D'                                          Display
     C     @@MODE        OREQ      'M'                                          WCMC Info
      *
      *  For 'Display' or 'WCMC Info':
      *  Return RRN values for FMPDP & FMP1P if both files accessed.
      *  Otherwise, return Error Code.
      *
     C                   SELECT
     C     *IN40         WHENEQ    *ON
     C     *IN41         ANDEQ     *ON
     C                   MOVEL     '2'           QQRTCD
     C                   Z-ADD     *ZERO         QQMCPD
     C                   Z-ADD     *ZERO         QQMCP1
      *
     C     *IN40         WHENEQ    *ON
     C                   MOVEL     '3'           QQRTCD
     C                   Z-ADD     *ZERO         QQMCPD
     C                   Z-ADD     DBRRN2        QQMCP1
      *
     C     *IN41         WHENEQ    *ON
     C                   MOVEL     '4'           QQRTCD
     C                   Z-ADD     DBRRN1        QQMCPD
     C                   Z-ADD     *ZERO         QQMCP1
      *
     C                   OTHER
     C                   MOVE      *BLANK        QQRTCD
     C                   Z-ADD     DBRRN1        QQMCPD
     C                   Z-ADD     DBRRN2        QQMCP1
     C                   ENDSL
      *
     C  N40              EXCEPT    RPD01
     C  N41              EXCEPT    RP101
      *
     C                   ELSE
      *
      *  For all functions except 5=Display & 8=WCMC Info, assign all
      *  WCMC Provider and Provider Auxiliary fields from WCMS Payee values.
      *  Then Update/Write FMPDP & FMP1P depending on *IN40 & *IN41.
      *
     C                   EXSR      ASSIGN
      *
     C     *IN40         IFEQ      *OFF
     C*** BEGIN ADD *** 04/10/01 **************************************
     C     WPTYPE        IFNE      '1'
     C     WPTYPE        ANDNE     '4'
     C                   MOVEL(P)  'I'           PDSTAT
     C                   MOVEL(P)  NAME          PDNAME
     C                   ENDIF
     C*** END   ADD *** 04/10/01 **************************************
     C                   UPDATE    PPD01
     C                   ELSE
     C*** BEGIN ADD *** 04/10/01 **************************************
     C     WPTYPE        IFEQ      '1'
     C     WPTYPE        OREQ      '4'
     C*** END   ADD *** 04/10/01 **************************************
     C                   WRITE     PPD01
     C*** BEGIN ADD *** 04/10/01 **************************************
     C                   ENDIF
     C*** END   ADD *** 04/10/01 **************************************
     C                   ENDIF
      *
     C     *IN41         IFEQ      *OFF
     C                   UPDATE    PP101
     C                   ELSE
     C                   MOVE      Q1USER        P1IUSR
     C                   Z-ADD     P1DT          P1IDT
     C                   WRITE     PP101
     C                   ENDIF
      *
     C                   MOVE      *BLANK        QQRTCD
     C                   Z-ADD     DBRRN1        QQMCPD
     C                   Z-ADD     DBRRN2        QQMCP1
     C                   MOVEL     'C'           @@MODE
      *
     C                   ENDIF
     C*** BEGIN ADD *** 07/30/98 **************************************
     C*** BEG DEL *** 09/29/00 **************************************
     C*******************  SETON                     LR
     C*** END DEL *** 09/29/00 **************************************
     C*** END   ADD *** 07/30/98 **************************************
      *
     C                   RETURN
      *
      *************************************************************************
      *  ASSIGN - Subroutine to assign WC Payee fields to MC Provider fields
      *************************************************************************
     C     ASSIGN        BEGSR
      *
     C                   Z-ADD     WPFEIN        PDFEIN
     C                   Z-ADD     WPMOD         PDMOD
     C                   MOVEL(P)  WPNAME        PDNAME
     C                   MOVEL(P)  WPFSN         PDFSN
     C                   MOVEL(P)  WPTITL        PDTITL
     C                   MOVEL(P)  WPADR1        PDADR1
     C                   MOVEL(P)  WPCTY         PDCTY
     C                   MOVE      WPST          PDST
     C                   Z-ADD     WPZIP         PDZIP
     C                   Z-ADD     WPAREA        PDAREA
     C                   Z-ADD     WPPHN#        PDPHN#
     C                   MOVE      WPSHNM        PDSHNM
     C                   MOVE      WP1099        PD1099
     C                   MOVE      WPTYPE        PDTYPE
     C                   MOVE      WPSPCD        PDSPCD
     C                   MOVE      WPADR2        PDADR2
     C                   MOVE      WPDBA1        PDDBA1
     C                   MOVE      WPFXAR        PDFXAR
     C                   MOVE      WPFAX#        PDFAX#
     C                   MOVE      WPWCCE        PDWCCE
      *** BEGIN ADD *** 03/26/97 ***********************************************
     C*
     C*****                Z-ADDWPWCDT    SRDTI
     C*****                CALL 'SRCVTDAT'DTCYMD
     C*****                MOVELSRDTO     PDWCDT
     C                   Z-ADD     WPWCDT        PDWCDT
     C*
     C                   MOVE      WPBWC#        PDBWC#
      *** END   ADD *** 03/26/97 ***********************************************
     C*** BEGIN ADD ** 07/03/97 ************************************************
     C     Q1PRMS        IFGT      29
     C                   MOVE      WPBWCT        PDBWCT
     C                   ENDIF
     C*** END ADD ** 07/03/97 **************************************************
     C                   MOVE      WPSTAT        PDSTAT
     A*** Begin Chg *** 12/10/07 **************************************
     C****************     MOVE WPFLIC    PDFLIC
     C                   MOVE      WPLIC#        PDLIC#
     A*** End   Chg *** 12/10/07 **************************************
     C                   MOVE      Q1USER        PDUSER
     C                   MOVEL(P)  Q1PGM         PDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        PDTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PDDT
      *
     C     SRERR         IFEQ      *ON
     C                   Z-ADD     *ZERO         PDDT
     C                   ENDIF
      *
      *  Assign Provider Auxiliary fields
      *
     C     *IN41         IFEQ      *ON
     C                   Z-ADD     PDDT          P1IDT
     C                   MOVEL     PDUSER        P1IUSR
     C*** BEG CHG *** 06/19/14 *****************************
     C**********         MOVEL(P)  WPNAME        P1NAME
     C**********         MOVEL(P)  WPFSN         P1FSN
     C*** END CHG *** 06/19/14 *****************************
     C*** BEG CHG *** 02/13/98 *****************************
     C***********          MOVE 'N'       P1PARN
     C                   MOVE      ' '           P1PARN
     C*** BEG CHG *** 02/13/98 *****************************
     C                   ENDIF
     C*
     C*** BEG ADD *** 06/19/14 *****************************
     C*
     C     Q1PRMS        IFGT      32
     C*
     C                   EVAL      P1NAME = WP1NAME
     C                   EVAL      P1FSN  = WP1FSN
     C                   EVAL      P1ADR1 = WP1ADR1
     C                   EVAL      P1ADR2 = WP1ADR2
     C                   EVAL      P1CTY  = WP1CTY
     C                   EVAL      P1PST  = WP1PST
     C                   EVAL      P1ZIP  = WP1ZIP
     C                   EVAL      P1AREA = WP1AREA
     C                   EVAL      P1PHN# = WP1PHN#
     C*
     C                   ENDIF
     C*
     C*** END ADD *** 06/19/14 *****************************
     C*
      *
     C                   Z-ADD     WPFEIN        P1FEIN
     C                   Z-ADD     WPMOD         P1MOD
     C                   MOVE      Q1USER        P1USER
     C                   MOVEL(P)  Q1PGM         P1PGM
     C                   Z-ADD     DSTIME        P1TIME
     C                   MOVE      WPSTAT        P1STAT
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P1DT
     C*
     C     SRERR         IFEQ      *ON
     C                   Z-ADD     *ZERO         P1DT
     C                   ENDIF
      *
     C                   ENDSR
     OPPD01     E            RPD01
     OPP101     E            RP101
