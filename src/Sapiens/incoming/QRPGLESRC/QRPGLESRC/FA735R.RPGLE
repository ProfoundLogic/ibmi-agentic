     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE FA735R - API FOR BILL REVIEW AND REPRICING
     H* *****************************************************************
     H*  Description:                                                   *
     H*                                                                 *
     H*  THIS API WILL CALL STATE ADJUDICATION, CODE REVIEW, PRE-CERT   *
     H*  NETWORK DISCOUNT, FSMRA PROCESSING, AND COMPLETE BILL PGM      *
     H*******************************************************************
     H*                                                                 *
     H*  Called From:                                                   *
     H*    FA735C: Bill Review and Repricing API submit CLP             *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*    F4000N: State Adjudication                                   *
     H*    F1391R: CodeReview Integration Pgm                           *
     H*    U0066R: Pre-Cert Editing Process                             *
     H*    N9600R: Network Discount Process                             *
     H*    F4100R: FSMRA Process                                        *
     H*    F4200R: Apportionment Calculation Program                    *
     H*    F1393R: Complete bill pgm                                    *
     H*                                                                 *
     H*  Created:  07/21/02  C:79  Project:  114MDC                     *
     H*                                                                 *
     H*******************************************************************
     H*  REVISIONS:
     H*
     H*  04/30/19 C:062 Project: 152WAB
     H*                 Apply Optacomp Mods.
     H*
     H*  04/04/16 C:212 Project: OK022
     H*                 Replace call to F4100R with F5100R and other changes
     H*
     H*  01/15/16 C:079 Support: 114/39337
     H*                 Call to N9601R takes priority over call to N9600R
     H*
     H*  01/15/16 C:079 Support: 114/39337
     H*                 Call to N9601R takes priority over call to N9600R
     H*
     H*  11/25/13 C:282 Project: 129WSC
     H*                 Call to the apportionment program was already added
     H*                 in Base under the project "03/25/03 Project: 129MBH".
     H*                 Only added the condition to call the apportionment
     H*                 program for 129 only.
     H*
     H*  04/30/13 C:284 Project: 147MAS
     H*                 Add call to N9601R for clt 114.
     H*
     H*  06/11/12 C:251 Project: 114MER
     H*                 Replace Code Review with ClaimCheck
     H*           C:079 Call COBOL Intermediary program
     H*
     H*  03/25/03 C:62  Project: 129MBH
     H*                 Add call to the apportionment program.
     H*                                                                 *
     H*  11/25/02 C:62 PROJECT 114MDC                                   *
     H*                CHANGE CALL TO F0078R SO IT DOES NOT REMOVE      *
     H*                CODEREVIEW MESSAGES                              *
     H*                                                                 *
     H*******************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*    99 - General File Access                                     *
     H*******************************************************************
     F*
     F* EOMB Transaction File
     FFAEOP     IF   E             DISK
     F*
     F* Provider Master File
     FFMPDL     IF   E           K DISK
     F*
     F* Provider Auxiliary File
     FFMP1L     IF   E           K DISK
     F*
     F* Provider Network Contract File
     FFMS4L5    IF   E           K DISK
     FFMS4L4    IF   E           K DISK
     F                                     RENAME(PS401:PS402)
     F*
     F* WCMC Detail Charge File
     FFDCAL13   UF   E           K DISK
     F*
     F* WCMC Detail Amount File
     FFDDAL2    UF A E           K DISK
     F*
     F* State FS Rules/PGM Cross Reference File
     FFDRUL1    IF   E           K DISK
     FFMF1L     IF   E           K DISK
     F* State override table file logical
     FFTABL     IF   E           K DISK
     F* Bill type code file logical
     FFTBSL     IF   E           K DISK
     F*** Beg Add *** 04/30/13 ****************************************
     FFTLPL1    IF   E           K DISK
     FFAEUL     IF   E           K DISK
     FFAEHL     IF   E           K DISK
     F***** Beg Add *** 04/04/16 *******************************************
     FSTSTL     IF   E           K DISK
     F***** End Add *** 04/04/16 *******************************************
      *
     DProcdtlchrg      pr                  extpgm('N9601R')
     D##co#                           3
     D##sub#                          5
     D##fnd                           3
     D##1fyr                          3
     D##1case                         7
     D#1fein                          9
     D#1mod                           3
     D#1eob#                          9
     D##nedt                          6
     D##nwcd                          5
     D##sys                           1
     D##fl1                           1
     D##fl2                           1
     D##fl3                           1
     D##fl4                           1
     D##fl5                           1
     D##ds1                          10
     D##ds2                          10
     D##ds3                          10
     D##ds4                          10
     D##ds5                          10
     Dkkbtyp                          1
     D##rtcd                          1
     D*
     Dkklic#           s                   like(lplic#)
     Dkkbtyp           s                   like(eobtyp)
     F*** End ADD *** 04/30/13 ****************************************
     D*
     D APGM            S              6    DIM(10)                              ADJUDICATION PROGRAM
     D QCA             S             56    DIM(14) CTDATA PERRCD(1)             CMD EXEC/QCMDEXC
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  GN                     1     50
     D                                     DIM(10)                              GROUP NETWORK CODE
     D  F1NW01                 1      5
     D  F1NW02                 6     10
     D  F1NW03                11     15
     D  F1NW04                16     20
     D  F1NW05                21     25
     D  F1NW06                26     30
     D  F1NW07                31     35
     D  F1NW08                36     40
     D  F1NW09                41     45
     D  F1NW10                46     50
     D                 DS
     D  GF                     1     10
     D                                     DIM(10)                              GROUP NETWORK FLAGS
     D  F1NF01                 1      1
     D  F1NF02                 2      2
     D  F1NF03                 3      3
     D  F1NF04                 4      4
     D  F1NF05                 5      5
     D  F1NF06                 6      6
     D  F1NF07                 7      7
     D  F1NF08                 8      8
     D  F1NF09                 9      9
     D  F1NF10                10     10
     D* QCMDEXC CMD STRING MAPPED
     D                 DS
     D  XXCMD                  1     56
     D  XXST                  38     39
     D* QCMDEXC CMD STRING MAPPED
     D                 DS
     D  X1CMD                  1     56
     D  X1ST                  40     41
     D*** Beg Add *** 06/11/12 ****************************************
     DRtvPrcCd         pr                  extpgm('FA777R')
     D inRRN                          9
     D inRRN           s              9
     D*** End Add *** 06/11/12 ****************************************
     D*** Beg Add *** 01/15/16 ****************************************
     DRtvCltId         pr                  extpgm('RTVCLTID')
     D cltId                          3
     D cltId           s              3
     D isN9601R        s              1
     D*** End Add *** 01/15/16 ****************************************
     D***** Beg Add *** 04/04/16 *******************************************
     D kkkey           s                   like(STkey) inz
     D***** End Add *** 04/04/16 *******************************************
     D* End Standalone Fields
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9            FAEOP
     C                   PARM                    @@CFMR            1            Create FSMRA
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*MDY'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
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
     C*  KEY LISTS
     C*
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     P1KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     S4KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKNWCD
     C*
     C     BSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKCODE
     C*
     C     CAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     CAKY2         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C*
     C     DAKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C                   KFLD                    CARCN
     C*
     C     RUKY          KLIST
     C                   KFLD                    KKAST
     C                   KFLD                    KKATYP
     C                   KFLD                    KKFDAT
     C*
     C*   KEY LIST FOR THE FUND AUXILLARY FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C*  Adjudication code Table File
     C     TCSKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    TABKEY           10
     C*
     C* FIELD DEFINITIONS
     C*
     C     *LIKE         DEFINE    EOEOB#        ##EOB#
     C     *LIKE         DEFINE    EOFEIN        KKFEIN
     C     *LIKE         DEFINE    EOMOD         KKMOD
     C     *LIKE         DEFINE    EONTWK        KKNWCD
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    CAEOB#        KKEOB#
     C     *LIKE         DEFINE    CARCN         KKRCN
     C     *LIKE         DEFINE    RUST          KKAST
     C     *LIKE         DEFINE    RUATYP        KKATYP
     C     *LIKE         DEFINE    RUEFDT        KKFDAT
     C     *LIKE         DEFINE    BSTYPE        KKTYPE
     C     *LIKE         DEFINE    BSCODE        KKCODE
     C     *LIKE         DEFINE    EOCO#         #1CO#
     C     *LIKE         DEFINE    EOSUB#        #1SUB#
     C     *LIKE         DEFINE    EOFND         #1FND
     C*
     C*****************************************************************
     C**                      MAIN PROGRAM                           **
     C*****************************************************************
     C*
     C                   MOVE      @@RRN         #1RRN             9 0
     C*
     C     #1RRN         CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C* INITIALIZE FIELDS, CALL SYSTEM API
     C*
     C                   EXSR      SETUP
     C*
     C* PERFORM FILE OVERRIDES TO THE ADJUDICATION STATE OF THE BILL
     C*
     C                   EXSR      OVRRID
     C*
     C* LOAD ADJUDICATION PROGRAM ARRAY
     C*
     C                   EXSR      LODPGM
     C*** Beg Add *** 06/11/12 ******************************************
      /free
        exsr rstBill;
      /end-free
     C*** End Add *** 06/11/12 ******************************************
     C*
     C* CALL ADJUDICATION PROGRAM(S)
     C*
     C                   EXSR      CADJUD
     C*
     C* UPDATE/WRITE FDDAP
     C*
     C                   EXSR      WRUDAP
     C*
     C*** Beg Chg *** 06/11/12 ******************************************
     C**CALL*CodeReview*INTEGRATION*PROGRAM*************
     C*
     C********           CALL      'F1391R'
     C* Call Claimcheck integration program
     C                   CALL      'FA779R'
     C*** End Chg *** 06/11/12 ******************************************
     C                   PARM      #1RRN         ##RRN             9 0
     C                   PARM      *BLANKS       ##CRPE            1
     C                   PARM      *BLANKS       ##PCOD            3
     C                   PARM      *BLANKS       ##CHG             1            Adj Needed
     C*** Beg Del *** 06/11/12 ******************************************
     C********           PARM      *BLANKS       ##CRKB            2
     C********           PARM      *BLANKS       ##PMKB            2
     C*** End Del *** 06/11/12 ******************************************
     C*
     C* IF CODEREVIEW MADE CHANGES CALL ADJUDICATION AGAIN BUT DON'T
     C* DELETE MESSAGES AND EOB CODES (F0078R)
     C*
     C     ##CHG         IFEQ      'Y'
     C                   EXSR      CADJUD
     C                   EXSR      WRUDAP
     C                   ENDIF
     C*
     C* CALL PRE-CERT EDITING PROGRAM IF NECESSARY
     C*
     C                   EXSR      PRECRT
     C*
     C* CALL NETWORK DISCOUNTING PROGAM IF NECESSARY
     C*
     C                   EXSR      NETWRK
     C*
     C* CALL THE FSMRA ADJUSTMENT PROGRAM
     C*
     C***** Beg Add *** 04/04/16 *******************************************
      /free
         kkkey = EOast;
         Chain(n) kkkey pst01;
      /end-free
     C***** Beg Add *** 04/04/16 *******************************************
     C*
     C     EOAST         IFEQ      'OK'
     C     QQCFMR        ANDNE     'N'
     C***** Beg Add *** 04/04/16 *******************************************
     C     STbs03        ANDEQ     'Y'
     C***** Beg Add *** 04/04/16 *******************************************
     C     EOAST         OREQ      'AL'
     C     EOBTYP        ANDEQ     'H'
     C     QQCFMR        ANDNE     'N'
     C*
     C***** Beg Chg *** 04/04/16 *******************************************
     C*****              CALL      'F4100R'
     C                   CALL      'F5100R'
     C***** End Chg *** 04/04/16 *******************************************
     C                   PARM      #1RRN         ##RRN
     C*
     C                   ENDIF                                                  EOAST = OK
     C*** BEGIN ADD *** 03/25/03 **************************************
     C*
     C* CALL THE APPORTIONMENT CALCULATION PROGRAM
     C*
     C*** BEG ADD *** 11/25/13 ****************************************
      /Free
       If CltId = '129';
      /End-Free
     C*** END ADD *** 11/25/13 ****************************************
     C                   CALL      'F4200R'                             99
     C                   PARM      #1RRN         ##RRN
     C*** BEG ADD *** 11/25/13 ****************************************
      /Free
       EndIf;
      /End-Free
     C*** END ADD *** 11/25/13 ****************************************
     C*
     C*** END   ADD *** 03/25/03 **************************************
     C*
     C* CALL THE PROGRAM TO COMPLETE THE BILL
     C*
     C                   EXSR      CMPEOB
     C*
     C* DELETE FILE OVERRIDES
     C*
     C                   EXSR      DLTOVR
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   RETURN
     C*
     C*** Beg Add *** 06/11/12 **********************************************
     C*****************************************************************
     C* RstBill - Restore bill to original if already processed       *
     C*****************************************************************
      /free
       Begsr   RstBill;
         if eocrne = 'Y' and eocrdo <> ' ';
          //Restore bill to originl status
           inRrn = @@rrn;
           Callp RtvPrcCd(inRrn);
         endif;
       Endsr;
      /end-free
     C*** End Add *** 06/11/12 **********************************************
     C*****************************************************************
     C* CADJUD - Calls the adjudication program                       *
     C*****************************************************************
     C     CADJUD        BEGSR
     C*
     C* CALL ADJUDICATION PROGRAMS LISTED IN ARRAY 'APGM'
     C*
     C*
     C* DELETE ANY EXISTING ERROR MESSAGE PRIOR TO ADJUDICATION
     C* UNLESS CALLING AFTER CODEREVIEW MADE CHANGES (##CHG=Y)
     C*
     C     ##CHG         IFNE      'Y'
     C*
     C                   MOVE      EOCO#         #2CO#             3
     C                   MOVE      EOSUB#        #2SUB#            5
     C                   MOVE      EOFND         #2FND             3
     C                   MOVE      EOFYR         #2FYR             3
     C                   MOVE      EOCASE        #2CASE            7
     C                   MOVE      EOEOB#        #2EOB#            9
     C                   MOVE      EORCN         #2RCN             3
     C                   MOVE      'S'           #2DEL1            1            STATE ADJ
     C                   MOVE      'P'           #2DEL2            1            PRE-CERT
     C                   MOVE      'N'           #2DEL3            1            NETWORK
     C                   MOVE      'C'           #2DEL4            1            CORRECT CODING
     C*** BEGIN CHG *** 11/25/02 **************************************
     C**********           MOVE 'R'       #2DEL5  1        CODE REVIEW
     C                   MOVE      ' '           #2DEL5            1            CODE REVIEW
     C*** END   CHG *** 11/25/02 **************************************
     C*
     C                   CALL      'F0078R'
     C                   PARM                    #2CO#
     C                   PARM                    #2SUB#
     C                   PARM                    #2FND
     C                   PARM                    #2FYR
     C                   PARM                    #2CASE
     C                   PARM                    #2EOB#
     C                   PARM                    #2RCN
     C                   PARM                    #2DEL1                         STATE ADJUDICAT
     C                   PARM                    #2DEL2                         PRE-CERT
     C                   PARM                    #2DEL3                         NETWORK
     C                   PARM                    #2DEL4                         CORRECT CODING
     C                   PARM                    #2DEL5                         CODE REVIEW
     C*
     C                   ENDIF                                                  ##CHG IFNE Y
     C*
     C                   Z-ADD     1             K                 2 0
     C*
     C                   DO        10            K
     C     APGM(K)       IFNE      *BLANKS
     C                   MOVEL(P)  APGM(K)       ADJPGM            8
     C*
     C* LOAD ADJUDICATION STATE IF NOT F4000N - NEW FEE SCHEDULE PROCESS
     C*
     C     ADJPGM        IFNE      'F4000N'
     C                   MOVE      KKAST         ADJPGM
     C                   ENDIF                                                  ADJPGM NE F4000R
     C*
     C     ADJPGM        IFEQ      'F4000N'
     C                   CALL      ADJPGM
     C                   PARM                    ##FYR             3 0
     C                   PARM                    ##CASE            7 0
     C                   PARM                    ##FDTS          150
     C                   PARM                    ##FDT2          150
     C                   PARM                    ##SEQS          150
     C                   PARM                    ##ER#S           60
     C                   PARM                    ##RTCD            1
     C                   PARM                    ##SS#             9 0
     C                   PARM                    ##ACDT            8 0
     C                   PARM                    ##SPCD            3
     C                   PARM                    ##FEIN            9 0
     C                   PARM                    ##MOD             3 0
     C                   PARM                    ##DCS#            5
     C                   PARM                    ##AST             2
     C                   PARM                    ##EOB#
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C*
     C                   ELSE
     C*
     C                   CALL      ADJPGM
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM                    ##RTCD
     C                   PARM                    ##SS#
     C                   PARM                    ##ACDT
     C                   PARM                    ##SPCD
     C                   PARM                    ##EOB#
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C*
     C                   ENDIF                                                  ADJPGM = F4000R
     C*
     C                   ENDIF                                                  ADJPGM NE K
     C                   ENDDO                                                  DO 10
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* CLRFLD - Clear payment fields in FDCAP for Adjudication       *
     C*****************************************************************
     C     CLRFLD        BEGSR
     C*
     C* Get the adjudication code
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C*
     C* Only update charges for current recon and not override type
     C     EORCN         IFEQ      CARCN
     C     CSOVRD        ANDNE     'Y'
     C*
     C                   Z-ADD     *ZERO         CAPAID
     C                   Z-ADD     *ZERO         CAMRA
     C                   Z-ADD     *ZERO         CAOTHR
     C                   Z-ADD     *ZERO         CAOTH2
     C                   Z-ADD     *ZERO         CANDIS
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C*
     C                   MOVE      Q1USER        CAUSER
     C                   MOVE      Q1PGM         CAPGM
     C                   MOVE      DSTIME        CATIME
     C*
     C                   UPDATE    PCA01
     C*
     C                   ELSE                                                               CN
     C                   EXCEPT    RCA01
     C*
     C                   ENDIF                                                  EORCN EQ CARCN
     C*
     C                   ENDSR
     C*****************************************************************
     C* CMPEOB - Call program to complete/submit the bill             *
     C*****************************************************************
     C     CMPEOB        BEGSR
     C*
     C     ##PCOD        IFNE      *BLANKS
     C                   MOVEL(P)  'P'           KKTYPE
     C                   MOVEL(P)  ##PCOD        KKCODE
     C     BSKY          CHAIN     PBS01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  BSDESC        $$REAS
     C                   ELSE
     C                   MOVEL     *BLANKS       $$REAS
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       $$REAS
     C                   ENDIF
     C*
     C                   CALL      'F1393R'
     C                   PARM      #1RRN         $$RRN             9 0
     C                   PARM      ##CHG         $$CHG             1            From F1391R
     C                   PARM      ##CRPE        $$CRPE            1            From F1391R
     C                   PARM      ##PCOD        $$PCOD            3            From F1391R
     C                   PARM                    $$REAS           60
     C*** Beg Del *** 06/11/12 ******************************************
     C*******            PARM      ##CRKB        $$CRKB            2            From F1391R
     C*******            PARM      ##PMKB        $$PMKB            2            From F1391R
     C*******            PARM      *BLANKS       $$FQKB            2
     C*** End Del *** 06/11/12 ******************************************
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* SUBROUTINE  ****** D L T O V R ********************************
     C*****************************************************************
     C     DLTOVR        BEGSR
     C*
     C* DELETE OVERRIDE TO FTABL
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(8)        XXCMD
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM                    LENGTH           15 5
     C*
     C* DELETE OVERRIDE TO FMPRL
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(9)        XXCMD
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM                    LENGTH           15 5
     C*
     C* DELETE OVERRIDE TO FTPSL
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(10)       XXCMD
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM                    LENGTH           15 5
     C*
     C* DELETE OVERRIDE TO FMSVP
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(11)       XXCMD
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM                    LENGTH           15 5
     C*
     C* DELETE OVERRIDE TO FMPRP
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(12)       XXCMD
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM                    LENGTH           15 5
     C*
     C* DELETE OVERRIDE TO FMSVL1
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(13)       X1CMD
     C                   CALL      'QCMDEXC'
     C                   PARM                    X1CMD
     C                   PARM                    LENGTH           15 5
     C*
     C* DELETE OVERRIDE TO FMSVL2
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(14)       X1CMD
     C                   CALL      'QCMDEXC'
     C                   PARM                    X1CMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   ENDSR
     C*****************************************************************
     C* LODPGM - Loads the adjudication program array                 *
     C*****************************************************************
     C     LODPGM        BEGSR
     C*
     C* CYCLE THROUGH DETAIL CHARGE FILE FOR EOB# AND LOAD
     C* NECESSARY ADJUDICATION PROGRAMS IN ARRAY 'APGM'
     C*
     C     CAKY1         CHAIN     PCA01                              96
     C     *IN96         DOWEQ     *OFF
     C                   EXSR      CLRFLD
     C                   Z-ADD     CAFDAT        KKFDAT
     C     RUKY          SETGT     PRU01
     C                   READP     PRU01                                  60
     C     *IN60         IFEQ      *OFF
     C     RUST          ANDEQ     KKAST
     C     RUATYP        ANDEQ     KKATYP
     C                   Z-ADD     1             J                 2 0
     C     RUAPGM        LOOKUP    APGM(J)                                61
     C     *IN61         IFEQ      *OFF
     C     *BLANKS       LOOKUP    APGM(J)                                61
     C   61              MOVEL     RUAPGM        APGM(J)
     C                   ENDIF                                                  *IN61 = *0FF
     C                   ENDIF                                                  *IN90 = *OFF
     C     CAKY1         READE     PCA01                                  96
     C                   ENDDO                                                  *IN96 = *OFF
     C*
     C                   ENDSR                                                  *IN96 = *OFF
     C*****************************************************************
     C* NETWRK - Calls the Network program(s)                         *
     C*****************************************************************
     C     NETWRK        BEGSR
     C*
     C                   Z-ADD     EOFEIN        KKFEIN
     C                   Z-ADD     EOMOD         KKMOD
     C*
     C     P1KY          CHAIN     PP101                              99
     C                   Z-ADD     1             T                 2 0
     C*
     C* DETEMINE IF THIS IS AN OUTSIDE ACCOUNT AND IF BILL HAS A
     C* NETWORK
     C*
     C                   MOVE      EOCO#         ##CO#
     C                   MOVE      EOSUB#        ##SUB#
     C                   MOVE      EOFND         ##FND
     C*
     C                   CALL      'FA701R'
     C                   PARM      ' '           ##RTCD
     C                   PARM                    ##CO#
     C                   PARM                    ##SUB#
     C                   PARM                    ##FND
     C                   PARM                    ##OUTS            1
     C*
     C     EONTWK        IFNE      *BLANKS
     C*** Beg Add *** 01/15/16 ****************************************
      /Free
       rtvCltID(cltid);
       isN9601R=' ';
       //*** Begin Add *** 04/30/19 ***********************************
       //*** If cltid = '114' and eontwk <> *blanks;
       If (cltid = '114' Or cltid = '152')and eontwk <> *blanks;
       //*** End   Add *** 04/30/19 ***********************************
          If eobtyp = 'N';
             chain caky1 peh01;
             If %found(faehl);
                kklic# = ehflic;
             Endif;
          Elseif eobtyp = 'H';
             chain caky1 peu01;
             If %found(faeul);
                kklic# = euflic;
             Endif;
          Endif;
          kkbtyp = eobtyp;

          chain kklic# plp01;
          If %found(ftlpl1);
             isN9601R='Y';
          Endif;
       Endif;
      /End-free
     C*** End Add *** 01/15/16 ****************************************
     C*
     C     F1KY          CHAIN     PF101                              99
     C*
     C* DETERMINE IF BILL NETWORK IS ONE OF GROUP'S NETWORKS
     C* AND IF OUTSIDE NETWORK IS SET
     C*
     C     EONTWK        LOOKUP    GN(T)                                  98
     C     *IN98         IFEQ      '1'
     C     GF(T)         IFEQ      'Y'
     C                   MOVE      'Y'           XXNTWK            1
     C                   ELSE
     C                   MOVE      'N'           XXNTWK
     C                   ENDIF                                                  GF,T = 'Y'
     C                   ELSE
     C                   MOVE      'N'           XXNTWK
     C                   ENDIF                                                  *IN98 = '1'
     C*
     C     ##NTWK        IFEQ      'Y'
     C     XXNTWK        ANDEQ     'N'
     C     EONTWK        ANDGT     *BLANKS
     C*
     C     PDKY          CHAIN     PPD01                              97
     C     *IN97         IFEQ      *OFF
     C     PDPARN        ANDEQ     'P'
     C*
     C* GET PROVIDER NETWORK CONTRACT
     C*
     C                   MOVE      EONTWK        KKNWCD
     C     PDTYPE        IFEQ      '1'
     C     S4KY          CHAIN     PS401                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     S4BGDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ##FRDT            6 0
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  PDTYPE = 'P'
     C*
     C     PDTYPE        IFEQ      '4'
     C     S4KY          CHAIN     PS402                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     S4BGDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ##FRDT            6 0
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  PDTYPE = 'P'
     C*
     C* CALL THE PROGRAM TO PROCESS THE DISCOUNTING PROGRAMS
     C*
     C                   MOVE      EOCO#         ##CO#             3
     C                   MOVE      EOSUB#        ##SUB#            5
     C                   MOVE      EOFND         ##FND             3
     C                   MOVE      EOFYR         #1FYR             3
     C                   MOVE      EOCASE        #1CASE            7
     C                   MOVE      EOFEIN        #1FEIN            9
     C                   MOVE      EOMOD         #1MOD             3
     C                   MOVE(P)   EOEOB#        #1EOB#            9
     C                   MOVE      ##FRDT        ##NEDT            6
     C                   MOVE      EONTWK        ##NWCD            5
     C                   MOVE      'N'           ##RTCD
     C                   MOVE      EOSYS         ##SYS             1
     C                   MOVE      *BLANKS       ##FL1             1
     C                   MOVE      *BLANKS       ##FL2             1
     C                   MOVE      *BLANKS       ##FL3             1
     C                   MOVE      *BLANKS       ##FL4             1
     C                   MOVE      *BLANKS       ##FL5             1
     C                   MOVE      *BLANKS       ##DS1            10
     C                   MOVE      *BLANKS       ##DS2            10
     C                   MOVE      *BLANKS       ##DS3            10
     C                   MOVE      *BLANKS       ##DS4            10
     C                   MOVE      *BLANKS       ##DS5            10
     C*
     C*** Beg Add *** 01/15/16 ****************************************
     C** N9601R takes priority over N9600R call
     C                   If        isN9601R <> 'Y'
     C*** End Add *** 01/15/16 ****************************************
     C                   CALL      'N9600R'
     C                   PARM                    ##CO#
     C                   PARM                    ##SUB#
     C                   PARM                    ##FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C                   PARM                    #1FEIN
     C                   PARM                    #1MOD
     C                   PARM                    #1EOB#
     C                   PARM                    ##NEDT
     C                   PARM                    ##NWCD
     C                   PARM                    ##SYS
     C                   PARM                    ##FL1
     C                   PARM                    ##FL2
     C                   PARM                    ##FL3
     C                   PARM                    ##FL4
     C                   PARM                    ##FL5
     C                   PARM                    ##DS1
     C                   PARM                    ##DS2
     C                   PARM                    ##DS3
     C                   PARM                    ##DS4
     C                   PARM                    ##DS5
     C                   PARM                    ##FDTS
     C                   PARM                    ##FDT2
     C                   PARM                    ##RTCD
     C                   ENDIF                                                  *IN97 = 97
     C                   ENDIF                                                  ##NTWK = 'Y'
     C*** Beg Add *** 01/15/16 ****************************************
     C                   Endif
     C*** End Add *** 01/15/16 ****************************************
     C*
     C*** Beg Add *** 04/30/13 ****************************************
     C*** Beg Del *** 01/15/16 ****************************************
     C**************
     C**************     CALL      'RTVCLTID'
     C**************     PARM      *BLANKS       cltid             3
     C**************
      /free
       //**** If cltid = '114' and eontwk <> *blanks;
       //**** If eobtyp = 'N';
       //****      chain caky1 peh01;
       //****      If %found(faehl);
       //****         kklic# = ehflic;
       //****      Endif;
       //****   Elseif eobtyp = 'H';
       //****      chain caky1 peu01;
       //****      If %found(faeul);
       //****         kklic# = euflic;
       //****      Endif;
       //****   Endif;
       //****   kkbtyp = eobtyp;

       //****   chain kklic# plp01;
       //****   If %found(ftlpl1);
       //** End Del *** 01/15/16 ****************************************
       //** Beg Add *** 01/15/16 ****************************************
          If        isN9601R = 'Y';
       //** End Add *** 01/15/16 ****************************************
            evalr ##co# = %char(eoco#);
            evalr ##sub# = %char(eosub#);
            evalr ##fnd = %char(eofnd);
            evalr #1fyr = %char(eofyr);
            evalr #1case = %char(eocase);
            evalr #1fein = %char(eofein);
            evalr #1mod = %char(eomod);
            evalr #1eob# = %char(eoeob#);
            evalr ##nedt = %char(##frdt);
            ##nwcd = eontwk;
            ##rtcd = 'N';
            ##sys = eosys;
            ##fl1 = *blanks;
            ##fl2 = *blanks;
            ##fl3 = *blanks;
            ##fl4 = *blanks;
            ##fl5 = *blanks;
            ##ds1 = *blanks;
            ##ds2 = *blanks;
            ##ds3 = *blanks;
            ##ds4 = *blanks;
            ##ds5 = *blanks;

            Procdtlchrg(##co#:##sub#:##fnd:#1fyr:#1case:#1fein:
                        #1mod:#1eob#:##nedt:##nwcd:##sys:##fl1:
                        ##fl2:##fl3:##fl4:##fl5:##ds1:##ds2:
                        ##ds3:##ds4:##ds5:kkbtyp:##rtcd);
       //** Beg Del *** 01/15/16 ****************************************
       //****   Endif;
       //** End Del *** 01/15/16 ****************************************
          Endif;
      /End-Free
     C*** End Del *** 01/15/16 ****************************************
     C*** End Add *** 04/30/13 ****************************************
     C*
     C                   ENDIF                                                  NTWK <> blanks
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* SUBROUTINE  ****** O V R R I D ********************************
     C*****************************************************************
     C     OVRRID        BEGSR
     C*
     C* OVERRIDE DATABASE FILE FTABL TO STATE SPECIFIC FILE            LLS
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(1)        XXCMD
     C                   MOVE      EOAST         XXST
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM                    LENGTH           15 5
     C*
     C* OVERRIDE DATABASE FILE FMPRL TO STATE SPECIFIC FILE            LLS
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        XXCMD
     C                   MOVE      EOAST         XXST
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM                    LENGTH           15 5
     C*
     C* OVERRIDE DATABASE FILE FTPSL TO STATE SPECIFIC FILE            LLS
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(3)        XXCMD
     C                   MOVE      EOAST         XXST
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM                    LENGTH           15 5
     C*
     C* OVERRIDE DATABASE FILE FMSVP TO STATE SPECIFIC FILE            LLS
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(4)        XXCMD
     C                   MOVE      EOAST         XXST
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM                    LENGTH           15 5
     C*
     C* OVERRIDE DATABASE FILE FMPRP TO STATE SPECIFIC FILE            LLS
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(5)        XXCMD
     C                   MOVE      EOAST         XXST
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM                    LENGTH           15 5
     C*
     C* OVERRIDE DATABASE FILE FMSVL1 TO STATE SPECIFIC FILE           LLS
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(6)        X1CMD
     C                   MOVE      EOAST         X1ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    X1CMD
     C                   PARM                    LENGTH           15 5
     C*
     C* OVERRIDE DATABASE FILE FMSVL2 TO STATE SPECIFIC FILE           LLS
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(7)        X1CMD
     C                   MOVE      EOAST         X1ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    X1CMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   ENDSR
     C*****************************************************************
     C* PRECRT - Calls the pre-cert program                           *
     C*****************************************************************
     C     PRECRT        BEGSR
     C*
     C     EOCERT        IFNE      *BLANKS
     C*
     C                   Z-ADD     EOFYR         ##FYR
     C                   Z-ADD     EOCASE        ##CASE
     C                   MOVEL     EOSS#         ##SS#
     C                   Z-ADD     EOACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         #1ACDT
     C                   MOVEL(P)  EOCERT        ##CERT
     C                   MOVEL     EOEXT         ##EXT
     C                   Z-ADD     EOEOB#        ##EOB#
     C*
     C                   CALL      'U0066R'
     C                   PARM                    ##FYR             3 0
     C                   PARM                    ##CASE            7 0
     C                   PARM      ' '           ##RTCD            1
     C                   PARM                    ##SS#             9 0
     C                   PARM                    #1ACDT            8 0
     C                   PARM                    ##CERT           15
     C                   PARM                    ##EXT             3
     C                   PARM                    ##EOB#
     C*
     C                   ENDIF                                                  EOCERT NE *BLANKS
     C*
     C                   ENDSR
     C*****************************************************************
     C* SUBROUTINE  ****** S E T U P **********************************
     C*****************************************************************
     C     SETUP         BEGSR
     C*
     C* SET KEY FIELDS
     C*
     C                   MOVE      @@CFMR        QQCFMR            1
     C*
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   Z-ADD     EORCN         KKRCN
     C*
     C                   Z-ADD     EOCO#         #1CO#
     C                   Z-ADD     EOSUB#        #1SUB#
     C                   Z-ADD     EOFND         #1FND
     C*
     C                   MOVE      @@CFMR        QQCFMR
     C*
     C* SET BILL TYPE AND ADJ STATE FOR ADJUDICATION PROGRAM LOOK-UP
     C*
     C     EOBTYP        IFEQ      'N'
     C                   MOVE      'P'           KKATYP
     C                   ELSE
     C                   MOVE      EOBTYP        KKATYP
     C                   ENDIF                                                  EOBTYP = 'N'
     C*
     C                   MOVE      EOAST         KKAST
     C*
     C*
     C* CALL API FOR SYSTEM FLAGS
     C*
     C                   CALL      'SA001R'
     C                   PARM      ' '           ##WCIF            1
     C                   PARM      ' '           ##MCIF            1
     C                   PARM      ' '           ##WCIN            1
     C                   PARM      ' '           ##AR              1
     C                   PARM      ' '           ##NTWK            1
     C                   PARM      ' '           ##UR              1
     C                   PARM      ' '           ##MED             1
     C                   PARM      ' '           ##EDI             1
     C                   PARM      ' '           ##WCDK            1
     C                   PARM      ' '           ##RHAB            1
     C                   PARM      ' '           ##MSS             1
     C                   PARM      ' '           ##FSCH            1
     C                   PARM      ' '           ##PRVB            1
     C                   PARM      ' '           ##NPWC            1
     C                   PARM      ' '           ##OS              1
     C                   PARM      ' '           ##ARLS            1
     C                   PARM      ' '           ##CCOD            1
     C                   PARM      ' '           ##CRVW            1
     C                   PARM      ' '           ##PROM            1
     C                   PARM      ' '           ##MCRV            1
     C                   PARM      ' '           ##FREQ            1
     C                   PARM      ' '           ##CRAC            1
     C*
     C* SET ADJUDICATION PROGRAM PARMS
     C*
     C                   Z-ADD     EOFYR         ##FYR
     C                   Z-ADD     EOCASE        ##CASE
     C                   MOVE      *BLANKS       ##FDTS
     C                   MOVE      *BLANKS       ##FDT2
     C                   MOVE      *BLANKS       ##SEQS
     C                   MOVE      *BLANKS       ##ER#S
     C                   MOVE      '*'           ##RTCD
     C                   MOVEL     EOSS#         ##SS#
     C*
     C                   Z-ADD     EOACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##ACDT
     C*
     C                   MOVEL     EOSPCD        ##SPCD
     C                   Z-ADD     EOFEIN        ##FEIN
     C                   Z-ADD     EOMOD         ##MOD
     C                   MOVE      *ZEROS        ##DCS#
     C                   Z-ADD     EOEOB#        ##EOB#
     C                   MOVEL     EOAST         ##AST
     C*
     C                   ENDSR
     C*****************************************************************
     C* WRUDAP - WRITES OR UPDATES FDDAP.                             *
     C*****************************************************************
     C     WRUDAP        BEGSR
     C*
     C     CAKY2         CHAIN(N)  PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     DAKY          CHAIN     PDA01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     CACHRG        DACHRG
     C                   Z-ADD     CAMRA         DAMRA
     C                   Z-ADD     CANDIS        DANDIS
     C                   Z-ADD     CAOTHR        DAOTHR
     C                   Z-ADD     CAOTH2        DAOTH2
     C                   Z-ADD     CAPAID        DAPAID
     C*
     C********* MAINTENANCE FIELDS ************************************
     C*
     C                   MOVEL     Q1USER        DAUSER
     C                   MOVEL     Q1PGM         DAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DADT
     C                   UPDATE    PDA01
     C*
     C                   ELSE
     C*
     C                   CLEAR                   PDA01
     C                   Z-ADD     CACO#         DACO#
     C                   Z-ADD     CASUB#        DASUB#
     C                   Z-ADD     CAFND         DAFND
     C                   Z-ADD     CAFYR         DAFYR
     C                   Z-ADD     CACASE        DACASE
     C                   Z-ADD     CAEOB#        DAEOB#
     C                   Z-ADD     CARCN         DARCN#
     C                   MOVEL     CASS#         DASS#
     C                   Z-ADD     CAACDT        DAACDT
     C                   Z-ADD     CADSEQ        DADSEQ
     C                   Z-ADD     CARDAT        DATRDT
     C                   Z-ADD     CACHRG        DACHRG
     C                   Z-ADD     CAMRA         DAMRA
     C                   Z-ADD     CANDIS        DANDIS
     C                   Z-ADD     CAOTHR        DAOTHR
     C                   Z-ADD     CAOTH2        DAOTH2
     C                   Z-ADD     CAPAID        DAPAID
     C*
     C********* MAINTENANCE FIELDS ************************************
     C*
     C                   MOVEL     'A'           DASTAT
     C                   MOVEL     Q1USER        DAUSER
     C                   MOVEL     Q1PGM         DAPGM
     C                   MOVEL     Q1USER        DAIUSR
     C                   MOVEL     Q1PGM         DAIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DATIME
     C                   Z-ADD     DSTIME        DAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DADT
     C                   MOVEL     SRDTO         DAIDT
     C                   WRITE     PDA01
     C                   ENDIF
     C     CAKY2         READE(N)  PCA01                                  99
     C                   ENDDO
     C*
     C                   ENDSR
     C*****************************************************************
     OPCA01     E            RCA01
**  (QCMDEX)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
OVRDBF FILE(FTPSL) TOFILE(*LIBL/FTPSL  )
OVRDBF FILE(FMSVP) TOFILE(*LIBL/FMSVP  )
OVRDBF FILE(FMPRP) TOFILE(*LIBL/FMPRP  )
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
OVRDBF FILE(FMSVL2) TOFILE(*LIBL/FMSVL2  )
DLTOVR FILE(FTABL)
DLTOVR FILE(FMPRL)
DLTOVR FILE(FTPSL)
DLTOVR FILE(FMSVP)
DLTOVR FILE(FMPRP)
DLTOVR FILE(FMSVL1)
DLTOVR FILE(FMSVL2)
