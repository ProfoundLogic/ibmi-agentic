     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W470OR21 - AUDIT STAUS REPORT
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*       THIS PROGRAM WILL DISPLAY A SCREEN CONTAINING THE INFO  *
     H*    FROM THE LAST AUDIT TRACKING DETAIL REPORT. AFTER EACH     *
     H*    DETAIL REPORT, THE INFORMATION IS STORED IN WMA2P, WITH    *
     H*    THE DATE THAT THE REPORT WAS RUN. THE SCREEN VALUES WILL   *
     H*    BE TAKEN FROM THIS FILE.                                   *
     H*                                                               *
     H*  CALLED FROM: W4700R                                          *
     H*  CALLS TO:    W4700C21 (PRINT REPORT <OPT6>)                  *
     H*               W4708R   (VIEW REPORT <OPT8>)                   *
     H*                                                               *
     H*                                                               *
     H*  CREATED: 03/04/98  PROGRAMMER: C:87  PROJECT: WC630          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  12/09/99  C:87   PROJECT: SUPP Y2k testing                   *
     H*                   'Premium with expense constant' field was   *
     H*                   being filled with incorrect value.          *
     H*                                                               *
     H*  04/26/99  C:118  PROJECT: WC630                              *
     H*                   CHANGED 'F5=Reprint' TO 'F6=Reprint' AND    *
     H*                   ADDED 'F5=Refresh'.  ADDED A POP-UP MESSAGE *
     H*                   WINDOW WHEN 'F6=Reprint' IS SELECTED TO     *
     H*                   INDICATE THAT REPORT IS BEING SENT TO SPOOL *
     H*                   FILE. ADDED 2 SUBROUTINES TO LEFT ALIGN     *
     H*                   SELECTIONS.                                 *
     H*                                                               *
     H*  04/19/99  C:118  PROJECT: WC630                              *
     H*                   CHANGE 'FUND' TO 'GROUP', DATE FORMAT       *
     H*                   MODIFIED TO MM/DD/YY FORMAT. ROUTING        *
     H*                   PAGE MODIFIED.                              *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H*     OA -  OVERFLOW FOR PRINTER                                *
     H*     50 -  MAIN LOOP - DO WHILE                                *
     H*     60 -  GENERAL CHAIN INDICATOR                             *
     H*     98 -  LOKUP OPERATIONS                                    *
     H*                                                               *
     H*****************************************************************
     F*
     FW4700F21  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     FWMA2L     UF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOA)

     D*
     D*
     D TITLE           C                   CONST('AUDIT STATUS DETA-
     D                                     IL REPORT')
     D SUBPRT          C                   CONST('WA007R')
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C****************************************************************
     C*  KEY LISTINGS
     C****************************************************************
     C*
     C*  Audit detail file key list
     C     A2KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  User parameter key list
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  Company Master key list
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*  Key list for the Overide file
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C****************************************************************
     C*  *LIKE DEFINES
     C****************************************************************
     C*
     C*
     C     *LIKE         DEFINE    A2CO#         XXCO#
     C     *LIKE         DEFINE    A2FND         XXFND
     C     *LIKE         DEFINE    A2FNNM        XXFNNM
     C     *LIKE         DEFINE    A2FRDT        XXFRDT
     C     *LIKE         DEFINE    A2TODT        XXTODT
     C     *LIKE         DEFINE    A2RNDT        XXRNDT
     C*
     C*  Percentages
     C     *LIKE         DEFINE    A2RPCT        XXRPCT
     C     *LIKE         DEFINE    A2CPCT        XXCPCT
     C     *LIKE         DEFINE    A2OPCT        XXOPCT
     C*
     C*  Counter fields
     C     *LIKE         DEFINE    A2TOTM        XXACNT
     C     *LIKE         DEFINE    A2TOTM        XXCCNT
     C     *LIKE         DEFINE    A2TOTM        XXTCNT
     C     *LIKE         DEFINE    A2RQS#        XXRQST
     C     *LIKE         DEFINE    A2RCVD        XXRCVD
     C     *LIKE         DEFINE    A2OUTS        XXOUTS
     C     *LIKE         DEFINE    A2COMP        XXCOMP
     C     *LIKE         DEFINE    A2ATTM        XXATTM
     C     *LIKE         DEFINE    A2RVSD        XXRVSD
     C     *LIKE         DEFINE    A2CNTD        XXCNTD
     C     *LIKE         DEFINE    A2INHS        XXINHS
     C     *LIKE         DEFINE    A2#AUD        XX#AUD
     C*
     C*  Premiums work fields
     C     *LIKE         DEFINE    A2PRM1        X1PRM1
     C     *LIKE         DEFINE    A2PRM2        X1PRM2
     C     *LIKE         DEFINE    A2APRM        X1APRM
     C     *LIKE         DEFINE    A2PPDT        QQPPDT
     C     *LIKE         DEFINE    A2PPLA        QQPPLA
     C*
     C****************************************************************
     C*  *ENTRY PARAMETERS
     C****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         QQKC              1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*
     C     OPT6PL        PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##SEL             1
     C                   PARM                    ##QUE            10
     C                   PARM                    ##MODE            1
     C*
     C     OPT8PL        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##SEL             1
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
     C****************************************************************
     C*   SET UP AND INITIALIZE                                 "
     C****************************************************************
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQCO#         KKCO#             3 0
     C                   MOVE      QQFND         KKFND             3 0
     C*
     C*  Chain to company file
     C     COKY          CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*  Center company name in field
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM           40
     C*
     C*  Chain to the user parameter file
     C     UPKY          CHAIN     PUP01                              60
     C*
     C*  Center title
     C***** BEGIN CHG **** 04/19/99 ***********************************
     C**********           MOVELTITLE     SRDFLD    P
     C                   MOVEL(P)  ORTITL        SRDFLD
     C***** END CHG ****** 04/19/99 ***********************************
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS           40
     C*
     C*  MAIN LOOP - Read Audit Detail file and output the results
     C*              from the last Audit Detail report
     C*
     C     A2KY          CHAIN(N)  PA201                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   EXSR      SRSCR1
     C*
     C                   ELSE
     C                   EXSR      SRSCR2
     C                   ENDIF
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C                   RETURN
     C*
     C*================================================================
     C* ERRCK - Check for errors on screen 1
     C*================================================================
     C     ERRCK         BEGSR
     C*
     C                   MOVE      *OFF          *IN90
     C*
     C     SCPPDT        IFNE      *ZERO
     C*
     C                   Z-ADD     SCPPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPPDT
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  Check for errors in Opt column
     C*
     C***** BEGIN ADD **** 04/26/99 ***********************************
     C                   EXSR      CHKOPT
     C***** END ADD ****** 04/26/99 ***********************************
     C*
     C  N90SCSEL1        IFNE      ' '
     C     SCSEL1        ANDNE     '6'
     C     SCSEL1        ANDNE     '8'
     C                   SETON                                        71  90
     C                   ENDIF
     C*
     C  N90SCSEL2        IFNE      ' '
     C     SCSEL2        ANDNE     '6'
     C     SCSEL2        ANDNE     '8'
     C                   SETON                                        72  90
     C                   ENDIF
     C*
     C  N90SCSEL3        IFNE      ' '
     C     SCSEL3        ANDNE     '6'
     C     SCSEL3        ANDNE     '8'
     C                   SETON                                        73  90
     C                   ENDIF
     C*
     C  N90SCSEL4        IFNE      ' '
     C     SCSEL4        ANDNE     '6'
     C     SCSEL4        ANDNE     '8'
     C                   SETON                                        74  90
     C                   ENDIF
     C*
     C  N90SCSEL5        IFNE      ' '
     C     SCSEL5        ANDNE     '6'
     C     SCSEL5        ANDNE     '8'
     C                   SETON                                        75  90
     C                   ENDIF
     C*
     C  N90SCSEL6        IFNE      ' '
     C     SCSEL6        ANDNE     '6'
     C     SCSEL6        ANDNE     '8'
     C                   SETON                                        76  90
     C                   ENDIF
     C*
     C  N90SCSEL7        IFNE      ' '
     C     SCSEL7        ANDNE     '6'
     C     SCSEL7        ANDNE     '8'
     C                   SETON                                        77  90
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* *INZSR - Initialization subroutine
     C*================================================================
     C     *INZSR        BEGSR
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        SAVETM            6 0
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SAVEDT            7 0
     C*
     C                   ENDSR
     C*================================================================
     C* LODPRT - Load variables for print output
     C*================================================================
     C     LODPRT        BEGSR
     C*
     C                   Z-ADD     A2FND         XXFND
     C                   MOVEL     A2FNNM        XXFNNM
     C*
     C                   Z-ADD     A2FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C***** BEGIN CHG **** 04/19/99 ***********************************
     C**********           MOVELSRDTO     XXFRDT
     C                   MOVEL     SRDTO         XSFRDT            8 0
     C***** END CHG ****** 04/19/99 ***********************************
     C*
     C                   Z-ADD     A2TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C***** BEGIN CHG **** 04/19/99 ***********************************
     C**********           MOVELSRDTO     XXTODT
     C                   MOVEL     SRDTO         XSTODT            8 0
     C***** END CHG ****** 04/19/99 ***********************************
     C*
     C                   Z-ADD     A2RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE            8 0
     C*
     C                   Z-ADD     A2TOTM        XXTCNT
     C                   Z-ADD     A2TOTA        XXACNT
     C                   Z-ADD     A2TOTC        XXCCNT
     C*
     C                   Z-ADD     A2RQS#        XXRQST
     C                   Z-ADD     A2PRM1        X1PRM1
     C***  BEGIN ADD  ***  12/09/99  **********************************
     C*******************  Z-ADDA2PRM1    X1PRM2
     C                   Z-ADD     A2PRM2        X1PRM2
     C***  END   ADD  ***  12/09/99  **********************************
     C                   Z-ADD     A2APRM        X1APRM
     C                   Z-ADD     A2#AUD        XX#AUD
     C*
     C                   Z-ADD     A2RCVD        XXRCVD
     C                   Z-ADD     A2RPCT        XXRPCT
     C                   Z-ADD     A2COMP        XXCOMP
     C                   Z-ADD     A2CPCT        XXCPCT
     C                   Z-ADD     A2OUTS        XXOUTS
     C                   Z-ADD     A2OPCT        XXOPCT
     C*
     C                   Z-ADD     A2INHS        XXINHS
     C                   Z-ADD     A2ATTM        XXATTM
     C                   Z-ADD     A2RVSD        XXRVSD
     C                   Z-ADD     A2CNTD        XXCNTD
     C*
     C                   ENDSR
     C*================================================================
     C* LODSCR - Load variables for output to screen
     C*================================================================
     C     LODSCR        BEGSR
     C*
     C                   Z-ADD     A2FND         SCFND
     C                   MOVEL     A2FNNM        SCFNNM
     C*
     C                   Z-ADD     A2FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C                   Z-ADD     A2TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     A2TODT        IFEQ      *ZERO
     C                   MOVE      *ON           *IN79
     C                   ENDIF
     C*
     C                   Z-ADD     A2RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCLRDT
     C*
     C                   Z-ADD     A2PPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPPDT
     C                   Z-ADD     SCPPDT        QQPPDT
     C*
     C                   Z-ADD     A2PPLA        SCPPLA
     C                   Z-ADD     SCPPLA        QQPPLA
     C*
     C                   Z-ADD     A2TOTM        SCTOTM
     C                   Z-ADD     A2TOTA        SCTOTA
     C                   Z-ADD     A2TOTC        SCTOTC
     C*
     C                   Z-ADD     A2RQS#        SCRQS#
     C                   Z-ADD     A2PRM1        SCPRXA
     C                   Z-ADD     A2PRM2        SCPRWA
     C*
     C                   Z-ADD     A2RCVD        SCAMT1
     C                   Z-ADD     A2RPCT        SCPCT1
     C                   Z-ADD     A2COMP        SCAMT2
     C                   Z-ADD     A2CPCT        SCPCT2
     C                   Z-ADD     A2OUTS        SCAMT3
     C                   Z-ADD     A2OPCT        SCPCT3
     C*
     C                   Z-ADD     A2INHS        SCAMT4
     C                   Z-ADD     A2ATTM        SCAMT5
     C                   Z-ADD     A2RVSD        SCAMT6
     C                   Z-ADD     A2CNTD        SCAMT7
     C*
     C                   ENDSR
     C*================================================================
     C* PRTDTL - Print audit status detail report
     C*================================================================
     C     PRTDTL        BEGSR
     C*
     C                   EXSR      LODPRT
     C*
     C*  Get the distribution Information
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C***** BEGIN ADD **** 04/19/99 ***********************************
     C                   MOVE      *BLANKS       XXTITL           60
     C*
     C*   LEFT JUSTIFY TITLE IN FIELD
     C*
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      XXTITL
     C                   ENDIF
     C                   ENDIF
     C***** END ADD ****** 04/19/99 ***********************************
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   ENDIF
     C*
     C***** BEGIN ADD **** 04/19/99 ***********************************
     C*   CENTER TEXT IN FIELD
     C*
     C                   MOVEL     ORTITL        ##DATA
     C                   CALL      'SRCENTER'                             99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL     ##DATA        XXTITL
     C***** END ADD ****** 04/19/99 ***********************************
     C*
     C*   Print detail report
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C                   ENDSR
     C*================================================================
     C* SRSCR1 - Output screen and process
     C*================================================================
     C     SRSCR1        BEGSR
     C*
     C                   EXSR      LODSCR
     C*
     C     *IN90         DOUEQ     *OFF
     C     *INKC         ANDEQ     *ON
     C     *IN90         OREQ      *OFF
     C     *INKL         ANDEQ     *ON
     C*
     C                   EXFMT     SCR01
     C*
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C*
     C***** BEGIN ADD **** 04/26/99 ***********************************
     C*  F5=Refresh
     C                   MOVE      *OFF          *IN90
     C     *INKE         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   MOVE      *BLANK        SCSEL1
     C                   MOVE      *BLANK        SCSEL2
     C                   MOVE      *BLANK        SCSEL3
     C                   MOVE      *BLANK        SCSEL4
     C                   MOVE      *BLANK        SCSEL5
     C                   MOVE      *BLANK        SCSEL6
     C                   MOVE      *BLANK        SCSEL7
     C*
     C                   ENDIF
     C***** END ADD ****** 04/26/99 ***********************************
     C*
     ***** BEGIN CHG ***** 04/26/99 ***********************************
     C**********           EXSR ERRCK
     C  N90              EXSR      ERRCK
     C***** END CHG ****** 04/26/99 ***********************************
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   SELECT
     C*
     C     QQPPDT        WHENNE    SCPPDT
     C     QQPPLA        ORNE      SCPPLA
     C                   EXSR      UPDTR
     C*
     C***** BEGIN CHG **** 04/26/99 ***********************************
     C********** *INKE     WHEQ *ON
     C     *INKF         WHENEQ    *ON
     C***** END CHG ****** 04/26/99 ***********************************
     C*
     C                   EXSR      PRTDTL
     C*
     C***** BEGIN ADD **** 04/26/99 ***********************************
     C* Pop-up window indicating report going to spool file
     C*
     C                   EXFMT     SCR04
     C***** END ADD ****** 04/26/99 ***********************************
     C*
     C     SCSEL1        WHENNE    *BLANK
     C     SCSEL2        ORNE      *BLANK
     C     SCSEL3        ORNE      *BLANK
     C     SCSEL4        ORNE      *BLANK
     C     SCSEL5        ORNE      *BLANK
     C     SCSEL6        ORNE      *BLANK
     C     SCSEL7        ORNE      *BLANK
     C*
     C                   MOVE      A2CO#         ##CO#
     C                   MOVE      A2FND         ##FND
     C                   MOVE      UPOUTQ        ##QUE
     C                   MOVE      'D'           ##MODE
     C*
     C     SCSEL1        IFEQ      '6'
     C                   MOVE      '1'           ##SEL
     C                   CALL      'W4700C21'    OPT6PL
     C                   CALL      SUBPRT
     C                   SETON                                        81
     C                   ELSE
     C     SCSEL1        IFEQ      '8'
     C                   MOVE      '1'           ##SEL
     C                   CALL      'W4708R'      OPT8PL
     C                   SETON                                        81
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCSEL2        IFEQ      '6'
     C                   MOVE      '2'           ##SEL
     C                   CALL      'W4700C21'    OPT6PL
     C                   CALL      SUBPRT
     C                   SETON                                        82
     C                   ELSE
     C     SCSEL2        IFEQ      '8'
     C                   MOVE      '2'           ##SEL
     C                   CALL      'W4708R'      OPT8PL
     C                   SETON                                        82
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCSEL3        IFEQ      '6'
     C                   MOVE      '3'           ##SEL
     C                   CALL      'W4700C21'    OPT6PL
     C                   CALL      SUBPRT
     C                   SETON                                        83
     C                   ELSE
     C     SCSEL3        IFEQ      '8'
     C                   MOVE      '3'           ##SEL
     C                   CALL      'W4708R'      OPT8PL
     C                   SETON                                        83
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCSEL4        IFEQ      '6'
     C                   MOVE      '4'           ##SEL
     C                   CALL      'W4700C21'    OPT6PL
     C                   CALL      SUBPRT
     C                   SETON                                        84
     C                   ELSE
     C     SCSEL4        IFEQ      '8'
     C                   MOVE      '4'           ##SEL
     C                   CALL      'W4708R'      OPT8PL
     C                   SETON                                        84
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCSEL5        IFEQ      '6'
     C                   MOVE      '5'           ##SEL
     C                   CALL      'W4700C21'    OPT6PL
     C                   CALL      SUBPRT
     C                   SETON                                        85
     C                   ELSE
     C     SCSEL5        IFEQ      '8'
     C                   MOVE      '5'           ##SEL
     C                   CALL      'W4708R'      OPT8PL
     C                   SETON                                        85
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCSEL6        IFEQ      '6'
     C                   MOVE      '6'           ##SEL
     C                   CALL      'W4700C21'    OPT6PL
     C                   CALL      SUBPRT
     C                   SETON                                        86
     C                   ELSE
     C     SCSEL6        IFEQ      '8'
     C                   MOVE      '6'           ##SEL
     C                   CALL      'W4708R'      OPT8PL
     C                   SETON                                        86
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCSEL7        IFEQ      '6'
     C                   MOVE      '7'           ##SEL
     C                   CALL      'W4700C21'    OPT6PL
     C                   CALL      SUBPRT
     C                   SETON                                        87
     C                   ELSE
     C     SCSEL7        IFEQ      '8'
     C                   MOVE      '7'           ##SEL
     C                   CALL      'W4708R'      OPT8PL
     C                   SETON                                        87
     C                   ENDIF
     C                   ENDIF
     C*
     C*  If F12=*ON is returned from CALL, set it *OFF
     C     *INKL         IFEQ      *ON
     C                   MOVE      *OFF          *INKL
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* SRSCR2 - Output screen2 and process
     C*================================================================
     C     SRSCR2        BEGSR
     C*
     C                   EXFMT     SCR02
     C*
     C                   ENDSR
     C*================================================================
     C* UPDTR - Update the record
     C*================================================================
     C     UPDTR         BEGSR
     C*
     C     A2KY          CHAIN     PA201                              60
     C*
     C     A2DT          IFGT      SAVEDT
     C     A2DT          OREQ      SAVEDT
     C     A2TIME        ANDGT     SAVETM
     C                   SETON                                        78  90
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SCPPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         A2PPDT
     C*
     C                   Z-ADD     SCPPLA        A2PPLA
     C*
     C                   MOVEL     Q1USER        A2USER
     C                   MOVEL     Q1PGM         A2PGM
     C                   Z-ADD     DSTIME        A2TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         A2DT
     C*
     C                   UPDATE    PA201
     C*
     C                   Z-ADD     SCPPDT        QQPPDT
     C                   Z-ADD     SCPPLA        QQPPLA
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C***** BEGIN ADD **** 04/26/99 ***********************************
     C*================================================================
     C* CHKOPT - Left align selection options
     C*================================================================
     C     CHKOPT        BEGSR
     C*
     C     SCSEL1        IFNE      *BLANKS
     C                   MOVE(P)   SCSEL1        XXSEL             2
     C                   EXSR      CHKSEL
     C                   MOVE      XXSEL         SCSEL1
     C                   ENDIF
     C*
     C     SCSEL2        IFNE      *BLANKS
     C                   MOVE(P)   SCSEL2        XXSEL             2
     C                   EXSR      CHKSEL
     C                   MOVE      XXSEL         SCSEL2
     C                   ENDIF
     C*
     C     SCSEL3        IFNE      *BLANKS
     C                   MOVE(P)   SCSEL3        XXSEL             2
     C                   EXSR      CHKSEL
     C                   MOVE      XXSEL         SCSEL3
     C                   ENDIF
     C*
     C     SCSEL4        IFNE      *BLANKS
     C                   MOVE(P)   SCSEL4        XXSEL             2
     C                   EXSR      CHKSEL
     C                   MOVE      XXSEL         SCSEL4
     C                   ENDIF
     C*
     C     SCSEL5        IFNE      *BLANKS
     C                   MOVE(P)   SCSEL5        XXSEL             2
     C                   EXSR      CHKSEL
     C                   MOVE      XXSEL         SCSEL5
     C                   ENDIF
     C*
     C     SCSEL6        IFNE      *BLANKS
     C                   MOVE(P)   SCSEL6        XXSEL             2
     C                   EXSR      CHKSEL
     C                   MOVE      XXSEL         SCSEL6
     C                   ENDIF
     C*
     C     SCSEL7        IFNE      *BLANKS
     C                   MOVE(P)   SCSEL7        XXSEL             2
     C                   EXSR      CHKSEL
     C                   MOVE      XXSEL         SCSEL7
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* CHKSEL - Move option selection left in field
     C*================================================================
     C     CHKSEL        BEGSR
     C*
     C     ' '           CHECK     XXSEL         L                 1 0
     C                   SUBST(P)  XXSEL:L       XXSEL
     C*
     C                   ENDSR
     C***** END ADD ****** 04/26/99 ***********************************
     C*================================================================
     C*CBegin Output Specs
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
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
     O***** BEGIN CHG ******** 04/19/99 *******************************
     O******* E  1             ROUTE
     O**********E            ROUTE          2
     O***** END CHG ********** 04/19/99 *******************************
     O**********                                 15 'ROUTE TO:'
     O**********             UPUSNM              67
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           15 'ROUTE TO:'
     O                       UPUSNM              67
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O***** BEGIN CHG ******** 04/19/99 *******************************
     O                       XXTITL              80
     O**********               ORTITL    80
     O***** END CHG ********** 04/19/99 *******************************
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE          2 34
     O***** BEGIN ADD ******** 04/19/99 *******************************
     O          E            ROUTE          2
     O                                           24 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O***** END ADD ********** 04/19/99 *******************************
     O***** BEGIN CHG ******** 04/19/99 *******************************
     O**********                          8 'COMPANY:'
     O                                           18 'COMPANY:'
     O***** END CHG ********** 04/19/99 *******************************
     O                       XXCO#         Z     26
     O          E            ROUTE          2
     O***** BEGIN CHG ******** 04/19/99 *******************************
     O**********                          5 'FUND:'
     O                                           16 'GROUP:'
     O***** END CHG ********** 04/19/99 *******************************
     O                       XXFND         Z     26
     O***** BEGIN CHG ******** 04/19/99 *******************************
     O********E  170           ROUTE
     O**********                         24 '(C) PROGRESSIVE DATA'
     O**********                         45 'SOLUTIONS, INC. 1991'
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O****                               23 '(C) COPYRIGHT PROGRESSI'
     O****                             +000 'VE DATA SOLUTIONS, INC.,'
     O                                         +000 '(C)'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O***** END CHG ********** 04/19/99 *******************************
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              60
     O                       DSDATE        Y     80
     O          E            HEAD           1
     O                       Q1PGM               10
     O***** BEGIN CHG ******** 04/19/99 *******************************
     O**********               TITLE     56
     O                       XXTITL              70
     O***** END CHG ********** 04/19/99 *******************************
     O                                           75 'PAGE:'
     O                       PAGE          Z     80
     O          E            HEAD           1
     O***** BEGIN CHG ******** 04/19/99 *******************************
     O**********                         37 'AS OF:'
     O**********               XXDATE    46 '  /  /    '
     O******* E  1             HEAD
     O**********                         23 'POLICY EXPIRATION DATE'
     O**********                         30 'RANGE:'
     O**********               XXFRDT    41 '  /  /    '
     O**********               XXTODT    53 '  /  /    '
     O*
     O                                           36 'AS OF:'
     O                       XXDATE              48 '  /  /    '
     O          E            HEAD           1
     O                                           22 'POLICY EXPIRATION DATE'
     O                                           29 'RANGE:'
     O                       XSFRDT              41 '  /  /    '
     O                       XSTODT              54 '  /  /    '
     O***** END CHG ********** 04/19/99 *******************************
     O          E            HEAD           1
     O***** BEGIN CHG ******** 04/19/99 *******************************
     O**********                          5 'FUND:'
     O                                            6 'GROUP:'
     O**********               XXFND Z   09
     O                       XXFND         Z     11
     O**********               XXFNNM    70
     O                       XXFNNM              72
     O***** END CHG ********** 04/19/99 *******************************
     O*
     O          E            DET         1  1
     O                                           25 'TOTAL NUMBER OF'
     O***** BEGIN CHG ******** 04/19/99 *******************************
     O**********                         34 'MEMBERS:'
     O                                           35 'POLICIES:'
     O***** END CHG ********** 04/19/99 *******************************
     O                       XXTCNT        J     70
     O          E            DET            2
     O                                           21 'ACTIVE:'
     O                       XXACNT        J     32
     O                                           44 'CANCELLED:'
     O                       XXCCNT        J     55
     O          E            DET         1  2
     O                                           27 'AUDITS REQUESTED:'
     O                       XXRQST        J     70
     O          E            DET         1  2
     O                                           25 'AUDITS RECEIVED'
     O                                           41 'AND PERCENTAGE:'
     O                       XXRPCT        J     56
     O                                           58 '%'
     O                       XXRCVD        J     70
     O          E            DET         1  2
     O                                           26 'AUDITS COMPLETED'
     O                                           42 'AND PERCENTAGE:'
     O                       XXCPCT        J     56
     O                                           58 '%'
     O                       XXCOMP        J     70
     O          E            DET         1  2
     O                                           28 'AUDITS OUTSTANDING'
     O                                           44 'AND PERCENTAGE:'
     O                       XXOPCT        J     56
     O                                           58 '%'
     O                       XXOUTS        J     70
     O          E            DET         1  2
     O                                           28 'AUDITS IN HOUSE TO'
     O                                           37 'PROCESS:'
     O                       XXINHS        J     70
     O          E            DET         1  0
     O                                           27 'PROJECTED PREMIUM'
     O                                           44 'FROM LOSS REPORT'
     O          E            DET         1  2
     O                                           45 'AS OF __/__/__'
     O                                           70 '______________'
     O          E            DET         1  0
     O                                           31 'NON-PRODUCTIVE STATUS'
     O                                           43 'REPORT FROM'
     O          E            DET         1  2
     O                                           44 'AUDIT COMPANIES:'
     O                       XXATTM        J     70
     O          E            DET         1  2
     O                                           20 'RE-AUDITS:'
     O                       XXRVSD        J     70
     O          E            DET         1  2
     O                                           27 'CONTESTED AUDITS:'
     O                       XXCNTD        J     70
     O          E            DET         1  0
     O                                           26 'AUDITED PREMIUM:'
     O                       X1APRM        J     70
     O          E            DET         1  2
     O                                           31 'NUMBER OF AUDITED'
     O                                           41 'ACCOUNTS:'
     O                       XX#AUD        J     70
     O          E            DET         1  0
     O                                           27 'AUDITED/ESTIMATED'
     O                                           35 'PREMIUM'
     O          E            DET         1  1
     O                                           33 'WITHOUT EXPENSE'
     O                                           43 'CONSTANT:'
     O                       X1PRM1        J     70
     O          E            DET         1  1
     O                                           33 'WITH    EXPENSE'
     O                                           43 'CONSTANT:'
     O                       X1PRM2        J     70
