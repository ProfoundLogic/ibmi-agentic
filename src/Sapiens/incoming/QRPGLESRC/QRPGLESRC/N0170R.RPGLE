     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N0170R - A/R CASH POSTING
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  PROGRAMMER: C:35               REQ/PROJ: 25MBK               *
     H*  CREATED: 08/22/95                                            *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*  01 - DISPLAY SUBFILE                                         *
     H*  04 - CLEAR SUBFILE                                           *
     H*  40 - INVALID SUB#/FND                                        *
     H*  41 - INVALID TRANSACTION CODE                                *
     H*  42 - EOB AUXILIARY FILE READ - LOGICAL FAE1L1                *
     H*  43 - EOB AUXILIARY FILE READ - LOGICAL FAE1L2                *
     H*  44 - INVALID CHECK AMOUNT                                    *
     H*  45 - INVALID POSTING DATE                                    *
     H*  46 - DESCRIPTION IS BLANK                                    *
     H*  47 - INVALID CHECK #                                         *
     H*  48 - CHECK AMOUNT GREATER THAN INVOICE AMOUNT                *
     H*  49 - POSITION CURSOR AT SCFND                                *
     H*  50 - READ SUBFILE                                            *
     H*  51 - SUBFILE CHAIN                                           *
     H*  52 - POSITION CURSOR AT SCTRCD                               *
     H*  53 - POSITION CURSOR AT SCPODT                               *
     H*  54 - POSITION CURSOR AT SCCAMT                               *
     H*  55 - GROUP NOT LINKED WITH WCMS                              *
     H*  56 - INVALID SUBSCRIBER                                      *
     H*  57 - NO RECORDS FOUND                                        *
     H*  58 - CHECK HAS ALREADY BEEN POSTED                           *
     H*  59 - CHECK AMOUNT LESS THAN INVOICE AMOUNT                   *
     H*  60 - GENERAL READ/CHAIN INDICATOR                            *
     H*  61 - GENERAL READ/CHAIN INDICATOR                            *
     H*  62 - GENERAL READ/CHAIN INDICATOR                            *
     H*** BEGIN ADD *** 03/29/96 ***********************************************
     H*  64/65 - PRINT CONTROL USAGE                                  *
     H*  70 - ERROR MESSAGE CONTROL FOR OUTPUT                        *
     H*  71 - ERROR MESSAGE CONTROL FOR OUTPUT                        *
     H*  72 - ERROR MESSAGE CONTROL FOR OUTPUT                        *
     H*  72 - ERROR MESSAGE CONTROL FOR OUTPUT                        *
     H*** END ADD *** 03/29/96 *************************************************
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 12/18/24 C:342 Project : MC447 Phase 2                        *
     H*                1.F10 Search disabled on the Group Field       *
     H*                DSPF Changes:                                  *
     H*                1.Changed Existing Group field into Typeahead  *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H*   08/26/14 C:307 Project: SY354                               *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*   01/28/10 C:249 Project: SY215                               *
     H*            C:189 Retrieve brand Name                          *
     H*                                                               *
     H*   12/23/03 C:62  PROJECT: MC025 40355
     H*                  DON'T POST CASH IF THE ACTIVITY ASSOCIATED
     H*                  WITH A CHECK HAS NOT BEEN INVOICED.
     H*                                                               *
     H*   01/25/02 C:62  PROJECT: AR033
     H*                  POPULATE SYSTEM WITH 'N' IF NETWORK CHECK
     H*
     H*   06/26/00 C:62  PROJECT: MC025 23044
     H*                  ADD RECON PROCESSING.
     H*
     H*   02/18/00 C:53  PROJECT: SUP21911
     H*                  USED EAWCPD INSTEAD OF EATRDT TO COMPARE
     H*
     H*   01/20/00 C:53  PROJECT: SUP21467
     H*                  OMIT EOMBS MORE THAT A YEAR OLD DUE TO THE
     H*                  CLIENTS RE-USING CHECK NUMBERS
     H*
     H*   05/11/99 C:94  PROJECT: SUP17993
     H*                  CHANGE HOW *IN42 IS TURNED OFF AND ON
     H*
     H*   05/05/99 C:94  PROJECT: SUP17345                            *
     H*                  ADD CODE TO TURN ON *IN32 IF NO MORE RECORDS *
     H*                                                               *
     H*   02/22/99  C:94  PROJECT: 25MKL                              *
     H*                   CHANGE PGM TO USE FAEAP INSTEAD OF FAE1P    *
     H*                                                               *
     H*   12/18/98  C:94  PROJECT: MC152                              *
     H*                   1.  CHG PGM TO USE FAEAP INSTEAD OF NATRP   *
     H*                                                               *
     H*   11/13/95 *** C:41 *** _N/A_ ***                             *
     H*            1. CHG PGM TO ALLOW IF CHECK AMOUNT ENTERED IS     *
     H*               GREATER THAN CALC AMOUNT POST THE DIFFERENCE    *
     H*                                                               *
     H*            2. CHG PGM TO ALLOW UNPOSTED RECORDS TO BE         *
     H*               DISPLAYED AND POSTED.                           *
     H*                                                               *
     H*   11/27/95 C:20 PROJ: ARMNT                                   *
     H*            1. ADD CODE TO ZERO OUT WORKFILEDS USED TO ACCUMLATE
     H*               TOTALS DISPLAYED ON SCREEN.                     *
     H*                                                               *
     H*   02/16/96 C:20 PROJ: ARMNT                                   *
     H*            1. IF E110A1 IS THE CHECK NUMBER USED GET THE INV# *
     H*               FROM NATRP.  WE NEED THE NETWORK INVOICE FOR    *
     H*               THESE NOT THE BILL REVIEW INVOICE.              *
     H*                                                               *
     H*   03/19/96 C:20 PROJ: ARMNT                                   *
     H*            1. ADD CODE TO ZERO OUT WORKFILEDS USED TO ACCUMLATE
     H*               TOTALS DISPLAYED ON SCREEN.                     *
     H*                                                               *
     H*   03/29/96 C:53 PROJ: AR015                                   *
     H*            1. ADD CODE TO CHECK FOR EOMB ITEMS NOT INCLUDED   *
     H*               IN THE CHECK THEN CREATE A REPORT WITH THIS     *
     H*               INFORMATION.                                    *
     H*            2. ADD CODE FOR W/C CROSSOVER CHAIN                *
     H*                                                               *
     H*   08/06/96 C:53 PROJ: MC025 # 3361                            *
     H*            1. ADD CODE TO TAKE A SECOND PASS THROUGH THE EOB  *
     H*               AUX FILE TO PICK UP NETWORK CHECKS ALSO         *
     H*****************************************************************
     FN0170F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR02S:RRN2)
     FFMSML     IF   E           K DISK
     F                                     rename(psm01:psm00)
     F                                     infds(smfds)
     FFMSMP     IF   E             DISK
     F                                     rename(psm01:psm0p)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMFNP     IF   E             DISK
     F                                     rename(pfn01:pfn0p)
     FFMF1L     IF   E           K DISK
     FFAEOL4    IF   E           K DISK
     FFAEAL6    IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FFAEAL7    IF   E           K DISK
     F                                     RENAME(PEA01:PEA03)
     FFAE1L1    IF   E           K DISK
     FFAE1L2    IF   E           K DISK
     F                                     RENAME(PE101:PE102)
     FNMARL8    IF   E           K DISK
     F                                     RENAME(PAR01:PAR02)
     FNMARLA    IF   E           K DISK
     F                                     RENAME(PAR01:PAR03)
     FFDE1L2    IF   E           K DISK
     F                                     RENAME(PE101:PE10D)
     FFDE1L3    IF   E           K DISK
     F                                     RENAME(PE101:PE13D)
     FSMORL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     F                                     USROPN
     FNMARP     O    E           K DISK
     FWDPAL8    IF   E           K DISK
     FFTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)
     FFAEAL1    IF   E           K DISK
     FNATRL     IF   E           K DISK

     D smfds           ds
     D  smrrn                397    400b 0
     Drrn              s              9  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXOCMD                 1    150
     D  XXOUTQ                31     40
     D  ORFORM                52     61
     D  XXCOPY                71     72
     D  ORHOLD                80     83
     D  ORSAVE                91     94
     D*** NAMED CONSTANTS ***
     D OVR             C                   CONST('OVRPRTF FILE(QSYSPRT-
     D                                        ) OUTQ(          -
     D                                     ) FORMTYPE(         -
     D                                      ) COPIES(  ) HOLD( -
     D                                        ) SAVE(    )')
     D*
     D HDGA            C                   CONST('A/R AMOUNT PREVIOUSL-
     D                                     Y PAID OR ADJUSTED. -
     D                                      AMOUNTS THAT WILL N-
     D                                     OT POST:')
     D*
     D HDGB            C                   CONST('CHECK DOES NOT RELAT-
     D                                     E TO A/R OR ACTIVE E-
     D                                     OB/SVC.  AMOUNTS THA-
     D                                     T WILL NOT POST:')
     D*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     DF0055R           pr                  ExtPgm('F0055R')
     D ##call                         1    const
     D ##kc                           1n
     D ##kl                           1n
     D ##rrn                          9  0
     DF0060R           pr                  ExtPgm('F0060R')
     D ##call                         1    const
     D ##kc                           1n
     D ##kl                           1n
     D ##rrn                          9  0
      *****************************************************************

     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    UPFYR         XXFYR
     C     *LIKE         DEFINE    Q1USER        XXUSER
     C     *LIKE         DEFINE    EOCASE        XXCASE
     C     *LIKE         DEFINE    ARCK#         XXCK#
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    E110A1        XXE1CK
     C*** BEG ADD *** 02/22/99 ***************************************
     C     *LIKE         DEFINE    EAWCNC        XXEACK
     C*** END ADD *** 02/22/99 ***************************************
     C     *LIKE         DEFINE    PAAMT         XXIAMT
     C     *LIKE         DEFINE    PAAMT         XXPAMT
     C     *LIKE         DEFINE    ARAMT         XXAMTI                         ADD 11/10/95
     C     *LIKE         DEFINE    ARAMT         XXAMTP                         ADD 11/10/95
     C*** BEG CHG *** 12/18/98 ***************************************
     C     *LIKE         DEFINE    EANINV        XXINV#
     C********** *LIKE     DEFN EOINV#    XXINV#
     C*** END CHG *** 12/18/98 ***************************************
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C*** BEGIN ADD *** 03/29/96 ***********************************************
     C     *LIKE         DEFINE    SCCASE        XOCASE
     C     *LIKE         DEFINE    SCINV#        XOINV#
     C     *LIKE         DEFINE    SCEOB#        XOEOB#
     C     *LIKE         DEFINE    SCPAMT        XOPAMT
     C     *LIKE         DEFINE    SCIAMT        XOIAMT
     C     *LIKE         DEFINE    SCDESC        XODESC
     C     *LIKE         DEFINE    SCSYS         XOSYS
     C     *LIKE         DEFINE    ARTRTP        XOTRTP
     C     *LIKE         DEFINE    SCSYS         XXPRNT
     C     *LIKE         DEFINE    SCSYS         XXPOVR
     C     *LIKE         DEFINE    SCSYS         XXPRTD
     C     *LIKE         DEFINE    SCFYR         XOFYR
     C     *LIKE         DEFINE    SCCAMT        TTNPST
     C     *LIKE         DEFINE    SCCAMT        TTNPS2
     C     *LIKE         DEFINE    SCCAMT        TTNPSF
     C     *LIKE         DEFINE    SCCAMT        XXAFOR
     C     *LIKE         DEFINE    SCCASE        X2CASE
     C     *LIKE         DEFINE    SCEOB#        X2EOB#
     C     *LIKE         DEFINE    FNWCF#        XXWCF#
     C     *LIKE         DEFINE    XXPRNT        XXFSTB
     C*** BEG CHG *** 02/22/99 ***************************************
     C     *LIKE         DEFINE    EAUSER        XOUSER
     C********** *LIKE     DEFN E1USER    XOUSER
     C*** END CHG *** 02/22/99 ***************************************
     C*** END ADD *** 03/29/96 *************************************************
     C     *LIKE         DEFINE    EAWCPD        XXPRDT                         ADD 01/20/00
     C*** BEGIN ADD *** 06/26/00 **************************************
     C     *LIKE         DEFINE    EARCN         XXRCN
     C*** END   ADD *** 06/26/00 **************************************
      *
      *****************************************************************
      *
      *   KEY LIST FOR THE COMPANY FILE
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *****************************************************************
      *
      * KEY LIST FOR THE  USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    XXUSER
      *****************************************************************
      *
      * KEY LIST FOR THE FUND MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
      *****************************************************************
      *
      * KEY LIST FOR THE SUBCRIBER FILE
      *
     C     SMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
      *****************************************************************
      *
      * KEY LIST FOR THE FUND AUXILIARY FILE
      *
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
      *****************************************************************
      *
     C*** BEG ADD *** 02/22/99 ***************************************
      * KEY LIST FOR THE EOB AUXILIARY FILE
      *
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEACK
     C*** END ADD *** 02/22/99 ***************************************
      * KEY LIST FOR THE EOB AUXILIARY FILE
      *
     C     E1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXE1CK
      *****************************************************************
      *
      * KEY LIST FOR THE EOMB FILE
      *
     C     EOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
      *****************************************************************
      *
      * KEY LIST FOR THE A/R TRANSACTION FILE
      *
     C     ARKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXINV#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
      *
     C*** BEGIN ADD *** 03/29/96 ***********************************************
      *
      * PARTIAL KEY LIST FOR THE A/R TX FILE WITHOUT INV#
     C     ARKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
      *
      * KEY LIST FOR THE PAYMENT DETAIL FILE PARTIAL KEY
      *
     C     PAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWCF#
     C                   KFLD                    XXCK#
     C*** BEG ADD *** 02/22/99 **************************************
     C     EAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEACK
     C                   KFLD                    X2CASE
     C                   KFLD                    X2EOB#
     C*** END ADD *** 02/22/99 **************************************
      *
      * FULL KEY LIST FOR THE EOB AUXILIARY FILE
      *
     C     E1KY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXE1CK
     C                   KFLD                    X2CASE
     C                   KFLD                    X2EOB#
     C*
     C*** END ADD *** 03/29/96 *************************************************
      *****************************************************************
      *
      * KEY LIST FOR THE PAYMENT DETAIL FILE
      *
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C*** BEGIN CHG *** 03/29/96 ***********************************************
     C*******************  KFLD           XXFND
     C                   KFLD                    XXWCF#
     C*** END CHG *** 03/29/96 *************************************************
     C                   KFLD                    XXCK#
     C                   KFLD                    XXEOB#
      *****************************************************************
     C*
     C*  KEY LIST FOR THE TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
      *****************************************************************
     C*** BEGIN ADD *** 02/16/96 ***********************************************
     C*
     C* KEY LIST FOR THE EOMB FILE
     C*
     C     TRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*** END ADD *** 02/16/96 *************************************************
     C*** BEG ADD *** 12/18/98 ****************************************
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*** BEGIN ADD *** 06/26/00 **************************************
     C                   KFLD                    XXRCN
     C*** END   ADD *** 06/26/00 **************************************
     C*** END ADD *** 12/18/98 ****************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** BEG ADD *** 02/22/00 ***************************************
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** END ADD *** 02/22/00 ***************************************
     C*** BEG ADD *** 12/18/98 ***************************************
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
     C*
     C*** END ADD *** 12/18/98 ***************************************
      *
      *  GET THE USER PARAMETER FILE
      *
     C                   MOVE      Q1USER        XXUSER
      *
     C     UPKY          CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   MOVE      *ZERO         UPFND
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPSUB#        SCSUB#
     C                   MOVE      UPFND         SCFND
     C*** BEGIN CHG *** 03/29/96 ***********************************************
     C                   MOVEL     OVR           XXOCMD
     C                   MOVE      UPOUTQ        XXOUTQ
     C*** END CHG *** 03/29/96 *************************************************
      *
     C     SCSUB#        IFNE      0
     C     SCFND         ANDNE     0
     C                   MOVE      '1'           *IN54
     C                   ENDIF
      *
      *  GET THE COMPANY RECORD
      *
     C     COKY          CHAIN     PCO01                              60
      *
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
      *
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
      *   DISPLAY ALL FORMATS
      *
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   Z-ADD     *ZEROS        SCITTL
     C                   Z-ADD     *ZEROS        SCPTTL
     C*** BEGIN ADD *** 03/19/96 ***********************************************
     C                   Z-ADD     0             XXAMTI
     C                   Z-ADD     0             XXIAMT
     C                   Z-ADD     0             XXPAMT
     C                   Z-ADD     0             XXAMTP
     C*** END ADD *** 03/19/96 *************************************************
     C*** BEGIN ADD *** 03/29/96 ***********************************************
     C                   Z-ADD     0             TTNPST
     C                   Z-ADD     0             TTNPS2
     C                   Z-ADD     0             TTNPSF
     C*** END ADD *** 03/29/96 *************************************************
     C                   EXFMT     SCR01
     C                   ENDIF                                                  Q2RECF IFEQ 'SC
      *
      *   PROCESS ALL COMMAND KEYS
      *
     C     *IN11         IFEQ      '1'
      *
      **********************************
      *   F3 = EXIT                    *
      **********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC IFEQ
      *
      **********************************
      *   COMMAND 06 = SUBSC SEARCH    *
      **********************************
     C     *INKF         IFEQ      '1'
      *
     C     Q2RECF        IFEQ      'SCR01'
      /Free
       Callp F0055R('2':*inkc:*inkl:rrn);
       Clear SMsub#;
       Chain rrn psm0p;
      /End-free
      *
     C     *inkc         CABEQ     *on           TERM
      *
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
     C     *inkl         IFEQ      *on
     C     SMsub#        OREQ      *ZERO
     C                   GOTO      STRT
     C                   ENDIF                                                  QQRTCD IFEQ
      *
     C                   MOVE      SMsub#        SCSUB#
     C                   MOVE      '1'           *IN49
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  Q2RECF IFEQ
      *
     C                   ENDIF                                                  INKF IFEQ
      *
      *********************************
      *   F7 = GROUP SEARCH           *
      *********************************
     C     *INKG         IFEQ      '1'
      *
     C     Q2RECF        IFEQ      'SCR01'
      *
     C     SCSUB#        IFEQ      *ZEROS
     C                   MOVE      UPSUB#        QQSUB#            5 0
     C                   ELSE
     C                   MOVE      SCSUB#        QQSUB#
     C                   ENDIF                                                  SCSUB# IFEQ
      /Free
       //***** Beg Del ***** 12/18/24 ***************************
       //***Chain (xxCo#:qqSub#) psm00;
       //***rrn = smrrn;
       //***Callp F0060R('2':*inkc:*inkl:rrn);
       //***Clear FNfnd;
       //**Chain rrn pfn0p;
      /End-free
      ******
     C******inkc         CABEQ     *on           TERM
      *****
     C******inkl         IFEQ      *on
     C*****FNfnd         OREQ      *ZERO
     C*****              WRITE     SCR99
     C*****              MOVE      *BLANK        Q2RECF
     C*****              MOVEL     'SCR01'       Q2RECF
     C*****              MOVE      '1'           *IN49
     C*****              GOTO      STRT
     C*****              ENDIF                                                  QQRTC IFEQ
     C*****
     C*****              MOVE      FNfnd         SCFND
     C*****              MOVE      '1'           *IN54
     C*****              GOTO      STRT
     C******** End Del ***** 12/18/24 ***************************
     C*
     C                   ENDIF                                                  Q2RECF IFEQ
      *
     C                   ENDIF                                                  *INKG IFEQ
      *
      ****************************************************************
      *  F10 - TABLE FILE SEARCH FOR A/R TRANSACTION CODE            *
      ****************************************************************
     C     *INKJ         IFEQ      '1'
      *
     C                   MOVEL     SCTRCD        XX2               2
      *
     C     XX2           IFEQ      '? '
      *
     C                   MOVE      *BLANK        QQKEY            10
     C                   CALL      'F2181R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD            1
      *
     C     QQRTCD        CABEQ     'A'           TERM
      *
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      '1'           *IN52
     C                   ELSE
     C                   MOVEL     QQKEY         SCTRCD
     C                   MOVE      '1'           *IN53
     C                   ENDIF                                                  QQRTCD IFEQ
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  XX2 IFEQ
      *
     C                   ENDIF                                                  *INKJ IFEQ
      *
      **********************************
      * CHANGE USER PARMS              *
      **********************************
     C     *INKM         IFEQ      '1'
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKY          CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   MOVE      UPCO#         XXCO#
      *
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKM IFEQ
      *
     C                   ENDIF                                                  *IN11 IFEQ  1'
      *
      *   GOTO THE APPROPRIATE SCREEN
      *
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
      *
      *   PROCESS REQUEST SCREEN
      *
     C     SCR01T        TAG
      *
      * VALIDATE SUBSCRIBER
      *
     C                   MOVE      SCSUB#        XXSUB#
     C     SMKY          CHAIN     Psm00                              56
      *
     C     *IN56         CABEQ     *ON           STRT
      *
     C                   MOVEL     SMNAME        SCSBNM
      *
      * VALIDATE FUND
      *
     C                   MOVE      SCSUB#        XXSUB#
     C                   MOVE      SCFND         XXFND
     C     FNKY          CHAIN     PFN01                              40
      *
     C     *IN40         CABEQ     *ON           STRT
     C*** BEGIN ADD *** 03/29/96 ***********************************************
     C                   Z-ADD     FNWCF#        XXWCF#
     C*** END ADD *** 03/29/96 *************************************************
      *
     C     F1KY          CHAIN     PF101                              60
     C     *IN60         IFEQ      *OFF
     C     F1OUTA        ANDNE     'N'
     C                   MOVE      '1'           *IN55
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   MOVEL     FNNAME        SCFNNM
      *
      *   VALIDATE TRANSACTION CODE
      *
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCTRCD        XXKEY
      *
     C     TBKY          CHAIN     PAC01                              41
      *
     C     *IN41         CABEQ     *ON           STRT
      *
     C     ACTYPE        IFNE      'P'
     C                   MOVE      '1'           *IN41                          INVALID CODE
     C                   GOTO      STRT
     C                   ENDIF                                                  ACTYPE IFNE
      *
      *   VALIDATE CHECK NUMBER
      *
     C     SCCK#         IFLE      *ZERO
     C                   MOVE      '1'           *IN47                          INVALID CHK#
     C                   GOTO      STRT
     C                   ENDIF                                                  SCCK# IFLE
     C                   MOVEL     SCCK#         XXE1CK
     C*** BEG ADD *** 02/22/99 **************************************
     C                   MOVEL     SCCK#         XXEACK
     C*** END ADD *** 02/22/99 **************************************
     C*** BEG CHG *** 02/22/99 **************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKY1         SETLL     PEA02
     C     EAKY1         READE     PEA02                                  42
     C   42EAKY1         SETLL     PEA03
     C   42EAKY1         READE     PEA03                                  43
     C                   ELSE
     C     E1KY          SETLL     PE101
     C     E1KY          READE     PE101                                  42
     C   42E1KY          SETLL     PE102
     C   42E1KY          READE     PE102                                  43
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 02/22/99 **************************************
     C     *IN43         CABEQ     *ON           STRT
     C*** BEGIN ADD *** 12/23/03 **************************************
     C*
     C* HAS ACTIVITY BEEN INVOICED?
     C*
     C     *IN42         IFEQ      *OFF
     C     EANINV        IFEQ      *ZEROS
     C                   SETON                                        61
     C                   GOTO      STRT
     C                   ENDIF
     C                   ELSE
     C     EABINV        IFEQ      *ZEROS
     C                   SETON                                        61
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 12/23/03 **************************************
      *
      *   VALIDATE CHECK AMOUNT
      *
     C     SCCAMT        IFEQ      *ZERO
     C                   MOVE      '1'           *IN44                          INVALID AMT
     C                   GOTO      STRT
     C                   ENDIF                                                  SCCAMT IFLE
      *
      *   VALIDATE POST DATE
      *
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
      *
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN45                          INVALID DATE
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR IFEQ
     C*** BEG ADD *** 01/20/00 *****************************
     C*** CALCULATE DATE 1 YEAR IN THE PAST TO OMIT OLD DATA.
     C*
     C                   MOVE      SCPODT        SRDTE             8 0
     C                   Z-ADD     365           SRPST             5 0
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C*
     C                   Z-ADD     SRDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         XXPRDT
     C*** END ADD *** 01/20/00 *****************************
      *
      *  ALL EDITS HAVE BEEN PASSED
      *
     C*===============================================================
     C* INITIALIZATION    AREA
     C*
     C     TOP           TAG
     C                   EXSR      INZSR
     C*
     C*===============================================================
     C*
     C     MAINLN        TAG
     C*
     C*===============================================================
     C* SCREEN EXECUTION AREA
     C*
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C                   EXSR      SCREXC
     C*
     C*===============================================================
     C* HANDLE F KEYS IF PRESSED
     C*
     C     *IN11         CASEQ     '1'           FKEYS
     C                   END                                                    *IN11 CASEQ
     C*
     C                   Z-ADD     RRN2          SVRRN2            5 0
     C*
     C                   READC     SCR02S                                 50
      *
     C                   EXSR      EDITDC
     C*
     C**N48****************EXSR WRITDC                     DEL 11/09/95
     C                   EXSR      WRITDC                                       ADD 11/09/95
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*** BEGIN ADD *** 01/25/02 **************************************
     C                   Z-ADD     *ZEROS        SCCAMT
     C                   Z-ADD     *ZEROS        SCCK#
     C                   MOVEL     *BLANKS       SCTRCD
     C                   Z-ADD     *ZEROS        SCPODT
     C*** END   ADD *** 01/25/02 **************************************
     C*** BEGIN ADD *** 03/29/96 ***********************************************
     C     XXPRNT        IFEQ      'X'
     C                   CLOSE     QSYSPRT
     C                   MOVE      ' '           XXPRNT
     C                   ENDIF
     C*** END ADD *** 03/29/96 *************************************************
     C                   GOTO      STRT
     C*
     C*===============================================================
     C* TERMINATION OF PROGRAM
     C*
     C     TERM          TAG
     C*** BEGIN ADD *** 03/29/96 ***********************************************
     C     XXPRNT        IFEQ      'X'
     C                   CLOSE     QSYSPRT
     C                   MOVE      ' '           XXPRNT
     C                   ENDIF
     C*** END ADD *** 03/29/96 *************************************************
     C*
     C                   MOVE      '1'           *INLR
      *
     C/EJECT
      *====================================================================*
      * * *   EDITDC SUBROUTINE                               E D I T D C ***
      *====================================================================*
     C     EDITDC        BEGSR
      *
     C                   MOVE      RRN2          SRRN2             5 0
     C                   Z-ADD     *ZEROS        K                 5 0
     C     NXTDC2        TAG
     C                   ADD       1             K
     C     K             CHAIN     SCR02S                             51
      *
     C     *IN51         IFEQ      '0'
      *
     C     SCCASE        IFNE      0
      *
      * DESCRIPTION CANNOT BE BLANK
      *
     C     SCDESC        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN46
     C                   UPDATE    SCR02S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      MAINLN
      *
     C                   ELSE
     C                   MOVE      '0'           *IN46
     C                   ENDIF                                                  SCDESC IFEQ
      *
     C     *IN46         IFEQ      '0'
     C                   UPDATE    SCR02S
     C                   ENDIF                                                  *IN46 IFEQ
      *
     C                   GOTO      NXTDC2
      *
     C                   ENDIF                                                  SCCASEIFEQ
      *
     C                   ENDIF                                                  *IN51 IFEQ
      *
     C                   MOVE      SRRN2         RRN2
      *
     C                   ENDSR                                                  EDITDC
     C/EJECT
      *====================================================================*
      * * *   INZSR SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INZSR         BEGSR
      *
     C*** BEG CHG *** 02/22/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C  N42EAKY1         SETLL     PEA02                              60
     C   42EAKY1         SETLL     PEA03                              60
     C                   ELSE
     C  N42E1KY          SETLL     PE101                              60
     C   42E1KY          SETLL     PE102                              60
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 02/22/99 ***************************************
     C*** BEGIN ADD *** 03/29/96 ***********************************************
     C                   MOVE      '0'           *IN70
     C*** END ADD *** 03/29/96 *************************************************
      *
      *** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02C
     C                   MOVE      '0'           *IN04
      *
     C                   Z-ADD     0             RRN2              5 0          CHG 11/10/95
      *
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
      *
      *
     C                   ENDSR                                                  INZSR
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
      *
     C*** BEG DEL *** 05/11/99 ***************************************
     C**********           MOVE '0'       *IN42            ADD 08/06/96
     C*** END DEL *** 05/11/99 ***************************************
      *
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   MOVE      '0'           *IN08
      *
     C     REREAD        TAG                                                    ADD 08/06/96
      *
     C*** BEG CHG *** 02/22/99 ****************************************
     C     ##PRVB        IFEQ      'Y'
     C  N42EAKY1         SETLL     PEA02
     C   42EAKY1         SETLL     PEA03
      *
     C  N42EAKY1         READE     PEA02                                  60
     C   42EAKY1         READE     PEA03                                  60
     C                   ELSE
     C  N42E1KY          SETLL     PE101
     C   42E1KY          SETLL     PE102
     C*
     C  N42E1KY          READE     PE101                                  60
     C   42E1KY          READE     PE102                                  60
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 02/22/99 ****************************************
     C*** BEGIN ADD *** 06/26/00 **************************************
     C                   Z-ADD     EARCN         XXRCN
     C*** END   ADD *** 06/26/00 **************************************
     C*
     C*** BEGIN ADD *** 08/06/96 **************************
     C  N42*IN60         IFEQ      '1'
     C**********           MOVE '1'       *IN42            DEL 05/11/99
     C                   GOTO      REREAD
     C                   ENDIF
     C*** END   ADD *** 08/06/96 **************************
     C*
     C     *IN60         IFEQ      *ON
     C                   ADD       1             RRN2
     C                   Z-ADD     0             SCFYR
     C                   Z-ADD     0             SCCASE
     C                   Z-ADD     0             SCINV#
     C                   Z-ADD     0             SCEOB#
     C*** BEGIN ADD *** 06/26/00 **************************************
     C                   Z-ADD     0             SCRCN
     C*** END   ADD *** 06/26/00 **************************************
     C                   MOVE      *BLANK        SCSYS
     C                   Z-ADD     0             SCIAMT
     C                   Z-ADD     0             SCPAMT
     C                   Z-ADD     0             XXIAMT                         ADD 11/27/95
     C                   Z-ADD     0             XXPAMT                         ADD 11/27/95
     C                   MOVE      *BLANKS       SCDESC
      *
     C                   WRITE     SCR02S
      *
     C                   MOVE      '1'           *IN57
      *
     C                   ELSE
     C*** BEG ADD *** 01/20/00 ****************************************
     C     ##PRVB        IFEQ      'Y'
     C********** EATRDT    CABLTXXPRDT    REREAD           DEL 02/18/00
     C     EAWCPD        CABLT     XXPRDT        REREAD                         ADD 02/18/00
     C                   ELSE
     C     EOTRDT        CABLT     XXPRDT        REREAD
     C                   ENDIF
     C*** END ADD *** 01/20/00 ****************************************
     C                   MOVE      '0'           *IN57
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C     *IN60         DOWEQ     *OFF
      *
      *
     C*** BEG CHG *** 02/22/99 ****************************************
     C     ##PRVB        IFEQ      'Y'
     C                   Z-ADD     EAFYR         SCFYR
     C                   MOVE      EACASE        SCCASE
     C                   MOVE      EAEOB#        SCEOB#
     C*** BEGIN ADD *** 06/26/00 **************************************
     C                   Z-ADD     EARCN         SCRCN
     C                   Z-ADD     EARCN         XXRCN
     C*** END   ADD *** 06/26/00 **************************************
     C                   ELSE
     C                   Z-ADD     E1FYR         SCFYR
     C                   MOVE      E1CASE        SCCASE
     C                   MOVE      E1EOB#        SCEOB#
     C                   ENDIF                                                  ##PRVB = Y
     C*** BEG CHG *** 02/22/99 ****************************************
      *
      * READ EOMB# FILE
      *
     C*** BEG CHG *** 02/22/99 ****************************************
     C     ##PRVB        IFEQ      'Y'
     C                   MOVE      EACO#         XXCO#
     C                   MOVE      EASUB#        XXSUB#
     C                   MOVE      EAFND         XXFND
     C                   MOVE      EAFYR         XXFYR
     C                   MOVE      EACASE        XXCASE
     C                   MOVE      EAEOB#        XXEOB#
     C                   Z-ADD     EADT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDT              6 0
     C                   ELSE
     C*                    MOVE E1CO#     XXCO#
     C                   MOVE      E1SUB#        XXSUB#
     C                   MOVE      E1FND         XXFND
     C                   MOVE      E1FYR         XXFYR
     C                   MOVE      E1CASE        XXCASE
     C                   MOVE      E1EOB#        XXEOB#
     C                   Z-ADD     E1DT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDT              6 0
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 02/22/99 ****************************************
      *
     C     EOKY          CHAIN     PEO01                              61
     C*
      *
     C     *IN61         IFEQ      *OFF
     C*** BEGIN ADD *** 01/25/02 **************************************
     C     *IN42         IFEQ      *OFF
     C                   MOVEL     'N'           SCSYS
     C                   ELSE
     C*** END   ADD *** 01/25/02 **************************************
     C                   MOVE      EOSYS         SCSYS
     C*** BEGIN ADD *** 01/25/02 **************************************
     C                   ENDIF
     C*** END   ADD *** 01/25/02 **************************************
     C**** BEGIN ADD *** 02/16/96 **********************************************
     C*** BEG CHG *** 12/18/98 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKY          CHAIN     PEA01                              61
     C                   ELSE
     C     TRKY          CHAIN     PTR01                              61
     C                   ENDIF
     C*** END CHG *** 12/18/98 ***************************************
     C     *IN61         IFEQ      '1'
     C                   Z-ADD     *ZEROS        ARINV#
     C                   END
     C*** END ADD *** 02/16/96 *************************************************
      *
      * READ A/R MASTER FILE
      *
     C                   Z-ADD     0             SCIAMT
     C                   Z-ADD     0             XXAMTI                         ADD 11/10/95
     C                   Z-ADD     0             XXAMTP                         ADD 11/10/95
     C                   Z-ADD     0             SCITTL                         ADD 11/10/95
     C                   Z-ADD     0             SCPTTL                         ADD 11/10/95
      *
     C                   MOVE      EOCO#         XXCO#
     C                   MOVE      EOSUB#        XXSUB#
     C*** BEGIN CHG *** 02/16/96 ***********************************************
     C*** BEG CHG *** 12/18/98 ****************************************
     C     ##PRVB        IFEQ      'Y'
     C   42              MOVE      EABINV        XXINV#
     C  N42              MOVE      EANINV        XXINV#
     C                   ELSE
     C   42              MOVE      EOINV#        XXINV#
     C  N42              MOVE      TRINV#        XXINV#
     C                   ENDIF
     C*** END CHG *** 12/18/98 ****************************************
     C*** END CHG *** 02/16/96 *************************************************
     C                   MOVE      EOFND         XXFND
     C                   MOVE      EOFYR         XXFYR
     C                   MOVE      EOCASE        XXCASE
     C                   MOVE      EOEOB#        XXEOB#
      *
     C*** BEGIN ADD *** 03/29/96 ***********************************************
     C     XXINV#        IFEQ      *ZEROS
      *
     C     ARKY2         SETLL     PAR03
     C     ARKY2         READE     PAR03                                  61
      *
     C     *IN61         DOWEQ     *OFF
      *
      * READ PYMT DETAIL
     C                   MOVE      EOCO#         XXCO#
     C                   MOVE      EOFND         XXFND
     C                   MOVE      SCCK#         XXCK#
     C                   MOVE      EOEOB#        XXEOB#
      *
     C     PAKY          SETLL     PPA01
     C     PAKY          READE     PPA01                                  62
      *
     C     *IN62         DOWEQ     *OFF
     C*** BEGIN ADD *** 06/26/00 **************************************
     C     XXRCN         IFEQ      PAEOBS
     C*** END   ADD *** 06/26/00 **************************************
      * PRINT FIELDS
      *
     C     XXPRNT        IFEQ      ' '
     C                   MOVE      'X'           XXPRNT
     C*   OVERRIDE
     C*
     C     XXPOVR        IFEQ      ' '
     C                   MOVE      'X'           XXPOVR
     C*
     C*   GET AND PRINT THE DISTRIBUTION INFORMATION
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              64
     C*
     C                   Z-ADD     150           LENGTH
     C                   MOVE      ORCOPY        XXCOPY
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXOCMD          150
     C                   PARM                    LENGTH           15 5
     C                   ENDIF
     C*
     C                   OPEN      QSYSPRT
      ***** Beg Del ***** 08/26/14 ************************************
     C**N64
     C**********ANN65              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   MOVE      '1'           *IN65
     C*
     C*   PRINT HEADINGS
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADA
     C                   EXCEPT    HEADL
     C*
     C                   ENDIF
      *
     C                   MOVE      EOFYR         XOFYR
     C                   MOVE      EOCASE        XOCASE
     C                   MOVE      EOEOB#        XOEOB#
     C                   MOVE      EOSYS         XOSYS
     C                   MOVE      ARINV#        XOINV#
     C                   Z-ADD     ARAMT         XOIAMT
     C                   MOVEL     ARDESC        XODESC
     C                   MOVEL     ARTRTP        XOTRTP
     C*
     C     XOIAMT        IFGT      *ZERO
     C                   Z-ADD     PAAMT         XOPAMT
     C                   ADD       XOPAMT        TTNPST
     C*** BEG CHG *** 02/22/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C                   Z-ADD     EADT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDT
     C                   MOVE      EAUSER        XOUSER
     C                   ELSE
     C                   Z-ADD     E1DT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDT
     C                   MOVE      E1USER        XOUSER
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 02/22/99 ***************************************
     C                   ELSE
     C                   Z-ADD     ARDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDT
     C                   MOVE      ARUSER        XOUSER
     C                   ENDIF
     C*
     C*   PRINT THE DETAIL
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADA
     C                   EXCEPT    HEADL
     C                   MOVE      '0'           *INOA
     C                   END
     C                   MOVE      '1'           *IN73
     C                   EXCEPT    DETEA
     C*** BEGIN ADD *** 06/26/00 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/26/00 **************************************
      *
     C     PAKY          READE     PPA01                                  62
      *
     C                   ENDDO                                                  *IN62 DOWEQ
      *
     C     ARKY2         READE     PAR03                                  61
      *
     C                   ENDDO                                                  *IN61 DOWEQ
      *
     C                   MOVE      EOEOB#        XXEOB#                         RESET
     C                   MOVE      '0'           *IN63                          SET AS UNPOSTED
     C                   MOVE      '0'           *IN73
     C*** INV# NOT = 0
     C                   ELSE
     C*** END ADD *** 03/29/96 *************************************************
      *
     C     ARKY          SETLL     PAR02
     C     ARKY          READE     PAR02                                  62
      *
     C     *IN62         DOWEQ     *OFF
     C*** BEGIN ADD *** 06/26/00 **************************************
     C     XXRCN         IFEQ      ARRCN
     C*** END   ADD *** 06/26/00 **************************************
      *
     C     ARTRTP        IFEQ      'P'
     C*****************    MOVE '1'       *IN58
     C*****************    MOVE *BLANKS   Q2RECF
     C*****************    MOVEL'SCR01'   Q2RECF
     C*****************    GOTO STRT
     C                   ADD       ARAMT         XXAMTP                         ADD 11/10/95
     C                   ENDIF                                                  ARTRTP IFEQ
      *
     C     ARTRTP        IFEQ      'I'
     C     ARTRTP        OREQ      'A'                                          ADD 11/10/95
     C                   ADD       ARAMT         SCIAMT
     C                   ADD       ARAMT         XXIAMT
     C                   ADD       ARAMT         XXAMTI                         ADD 11/10/95
     C                   MOVE      ARINV#        SCINV#
     C                   MOVEL     ARDESC        SCDESC
     C                   ENDIF                                                  ARTRTP IFEQ
     C*** BEGIN ADD *** 06/26/00 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/26/00 **************************************
      *
     C     ARKY          READE     PAR02                                  62
      *
     C                   ENDDO                                                  *IN62 DOWEQ
      *
     C     XXAMTP        IFEQ      XXAMTI
     C                   MOVE      '0'           *IN63                          RECORD POST
     C                   ELSE
     C                   MOVE      '1'           *IN63                          UNPOSTED REC
     C                   ENDIF
      *
     C*** BEGIN ADD *** 03/29/96 ***********************************************
     C                   ENDIF                                                  XXINV# = 0
     C*** END ADD *** 03/29/96 *************************************************
     C                   ELSE
      *
     C                   Z-ADD     0             SCIAMT
     C                   MOVE      *BLANK        SCSYS
     C                   MOVE      *BLANKS       SCDESC
      *
     C                   ENDIF                                                  *IN61 IFEQ
      *
      * READ PAYMENT DETAIL FILE
      *
     C     *IN63         IFEQ      '1'
      *
     C                   Z-ADD     0             SCPAMT
      *
     C                   MOVE      EOCO#         XXCO#
     C                   MOVE      EOFND         XXFND
     C                   MOVE      SCCK#         XXCK#
     C                   MOVE      EOEOB#        XXEOB#
      *
     C     PAKY          SETLL     PPA01
     C     PAKY          READE     PPA01                                  62
      *
     C     *IN62         DOWEQ     *OFF
     C*** BEGIN ADD *** 06/26/00 **************************************
     C     XXRCN         IFEQ      PAEOBS
     C*** END   ADD *** 06/26/00 **************************************
     C                   ADD       PAAMT         SCPAMT
     C                   ADD       PAAMT         XXPAMT
     C*** BEGIN ADD *** 06/26/00 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/26/00 **************************************
     C     PAKY          READE     PPA01                                  62
      *
     C                   ENDDO                                                  *IN62 DOWEQ
      *
     C                   ADD       1             RRN2
     C                   WRITE     SCR02S
      *
     C                   ENDIF                                                  *IN63 = '1'
      *
     C     RREAD2        TAG                                                    ADD 01/20/00
     C*
     C*** BEG CHG *** 02/22/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C  N42EAKY1         READE     PEA02                                  60
     C   42EAKY1         READE     PEA03                                  60
     C                   ELSE
     C  N42E1KY          READE     PE101                                  60
     C   42E1KY          READE     PE102                                  60
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 02/22/99 ***************************************
     C*** BEG ADD *** 05/05/99 ***************************************
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           *IN32
     C*** BEG ADD *** 01/20/00 ****************************************
     C                   ELSE
     C     ##PRVB        IFEQ      'Y'
     C********** EATRDT    CABLTXXPRDT    RREAD2           DEL 02/18/00
     C     EAWCPD        CABLT     XXPRDT        RREAD2                         ADD 02/18/00
     C                   ELSE
     C     EOTRDT        CABLT     XXPRDT        RREAD2
     C                   ENDIF
     C*** END ADD *** 01/20/00 ****************************************
     C                   ENDIF
     C*** END ADD *** 05/05/99 ***************************************
     C*
     C*** BEGIN ADD *** 08/06/96 **************************
     C*** RESET FILE AND MAKE A 2ND PASS
     C  N42*IN60         IFEQ      '1'
     C*** BEG DEL *** 05/11/99 ****************************************
     C**********           MOVE '1'       *IN42
     C**********           MOVE '0'       *IN60
     C*** END DEL *** 05/11/99 ****************************************
     C*** BEG CHG *** 02/22/99 ****************************************
     C     ##PRVB        IFEQ      'Y'
     C********** EAKY1     SETLLPEA03                      DEL 05/11/99
     C                   ELSE
     C     E1KY          SETLL     PE102
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 02/22/99 ****************************************
     C                   ENDIF
     C*** END   ADD *** 08/06/96 **************************
     C*
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C     RRN2          IFEQ      *ZEROS
     C                   MOVE      '1'           *IN58
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  RRN2 = ZEROS
      *
     C     *IN57         IFEQ      *OFF
      *
     C     SCCAMT        IFGT      XXIAMT
      *
     C                   MOVE      '1'           *IN48
     C     SCCAMT        SUB       XXIAMT        SCOAMT
      *
     C                   ELSE
      *
     C                   MOVE      '0'           *IN48
      *
     C     SCCAMT        IFLT      XXIAMT
     C                   MOVE      '1'           *IN59
     C*** BEGIN ADD *** 03/29/96 ***********************************************
     C     XXPRNT        IFEQ      'X'
     C                   EXCEPT    TOTL
     C                   ENDIF
      * DOES ACCOUNTED FOR MONEY ADD UP TO CHECK AMOUNT
     C     XXIAMT        ADD       TTNPST        XXAFOR
     C     SCCAMT        IFNE      XXAFOR
     C                   EXSR      SRCHEK
     C                   ELSE
     C                   Z-ADD     TTNPST        TTNPSF
     C     XXPRNT        IFEQ      'X'
     C                   EXCEPT    TOTLF
     C                   ENDIF
     C                   ENDIF
     C     XXPRNT        IFEQ      'X'
      ***** Beg Add ***** 08/26/14 ************************************
     C                   Except    Route
      ***** End Add ***** 08/26/14 ************************************
     C                   CLOSE     QSYSPRT
     C                   MOVE      ' '           XXPRNT
     C                   MOVE      ' '           XXFSTB
     C                   ENDIF
     C*** END ADD *** 03/29/96 *************************************************
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  SCCAMT IFLT
      *
     C                   ENDIF                                                  SCCAMT IFGT
      *
     C*** BEGIN ADD *** 03/29/96 ***********************************************
     C     XXPRNT        IFEQ      'X'
     C                   EXCEPT    TOTL
     C                   ENDIF
      * DOES ACCOUNTED FOR MONEY ADD UP TO CHECK AMOUNT
     C     XXIAMT        ADD       TTNPST        XXAFOR
     C     SCCAMT        IFNE      XXAFOR
     C                   EXSR      SRCHEK
     C                   ELSE
     C                   Z-ADD     TTNPST        TTNPSF
     C     XXPRNT        IFEQ      'X'
     C                   EXCEPT    TOTLF
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 03/29/96 *************************************************
     C                   ENDIF                                                  *IN57 IFEQ
      *
     C                   Z-ADD     XXIAMT        SCITTL
     C                   Z-ADD     XXPAMT        SCPTTL
     C                   Z-ADD     *ZEROS        XXIAMT
     C                   Z-ADD     *ZEROS        XXPAMT
      *
      *** TO POSITION SCREEN AND CURSOR
      *
     C                   Z-ADD     1             #@$2
      *
     C                   ENDSR                                                  LOAD
     C*** BEGIN ADD *** 03/29/96 ***********************************************
     C/EJECT
      *===============================================================
     C     SRCHEK        BEGSR
      *===============================================================
      *
     C     PAKY2         SETLL     PPA01
     C     PAKY2         READE     PPA01                                  60
      *
     C     *IN60         DOUEQ     '1'
     C                   MOVEA     '000'         *IN(70)
      *
      * READ EOMB# FILE
     C                   MOVE      *BLANKS       XXE1CK
     C                   MOVEL     PACHK#        XXE1CK
     C*** BEG ADD *** 02/22/99 ***************************************
     C                   MOVE      *BLANKS       XXEACK
     C                   MOVEL     PACHK#        XXEACK
     C*** END ADD *** 02/22/99 ***************************************
     C                   MOVE      PACASE        X2CASE
     C                   MOVE      PAEOB#        X2EOB#
      *
     C     PAEOB#        IFNE      0
     C     *IN42         IFEQ      '1'
     C*** BEG CHG *** 02/22/99 ****************************************
     C     ##PRVB        IFEQ      'Y'
     C*** BEGIN CHG *** 06/26/00 **************************************
     C********** EAKY2     CHAINPEA03                61
     C***61***** E1KY2     CHAINPE10D                62
     C     EOKY          CHAIN     PEO01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     EOBLST        IFEQ      'D'
     C*
     C                   SETON                                        61
     C                   SETOFF                                       62
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   CHG *** 06/26/00 **************************************
     C                   ELSE
     C     E1KY2         CHAIN     PE102                              61
     C   61E1KY2         CHAIN     PE10D                              62
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 02/22/99 ****************************************
     C   61
     CANN62              MOVE      '1'           *IN70
     C                   ELSE
     C*** BEG CHG *** 02/22/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKY2         CHAIN     PEA02                              61
     C   61E1KY2         CHAIN     PE13D                              62
     C                   ELSE
     C     E1KY2         CHAIN     PE101                              61
     C   61E1KY2         CHAIN     PE13D                              62
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 02/22/99 ***************************************
     C   61
     CANN62              MOVE      '1'           *IN70
     C                   ENDIF
     C                   ELSE
     C                   MOVE      '1'           *IN71
     C                   MOVE      '1'           *IN61
     C                   ENDIF
      *
      * PRINT
     C     *IN61         IFEQ      '1'
      *
     C                   MOVE      PAFYR         XOFYR
     C                   MOVE      PACASE        XOCASE
     C                   MOVE      PAEOB#        XOEOB#
     C                   Z-ADD     PAAMT         XOIAMT
     C                   MOVEL     PADESC        XODESC
     C*** BEG CHG *** 02/22/99 ****************************************
     C     ##PRVB        IFEQ      'Y'
     C                   Z-ADD     EADT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDT
     C                   ELSE
     C                   Z-ADD     E1DT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDT
     C                   ENDIF
     C*** END CHG *** 02/22/99 ****************************************
     C  N70
     CANN71              MOVE      '1'           *IN72
     C*
     C     XOIAMT        IFGT      *ZERO
     C                   Z-ADD     PAAMT         XOPAMT
     C                   ADD       XOPAMT        TTNPS2
     C                   ENDIF
     C*
     C*   PRINT THE DETAIL
     C     XXPRNT        IFEQ      ' '
     C                   MOVE      'X'           XXPRNT
     C*   OVERRIDE
     C*
     C     XXPOVR        IFEQ      ' '
     C                   MOVE      'X'           XXPOVR
     C*
     C*   GET AND PRINT THE DISTRIBUTION INFORMATION
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              64
     C*
     C                   Z-ADD     150           LENGTH
     C                   MOVE      ORCOPY        XXCOPY
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXOCMD          150
     C                   PARM                    LENGTH           15 5
     C                   ENDIF
     C*
     C                   OPEN      QSYSPRT
      ***** Beg Del ***** 08/26/14 ************************************
     C**N64
     ***********CANN65              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   MOVE      '1'           *IN65
     C*
     C*   PRINT HEADINGS
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADB1
     C                   EXCEPT    HEADL
     C                   MOVE      'X'           XXFSTB
     C*
     C                   ENDIF
      *
     C     XXPRNT        IFEQ      'X'
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADB1
     C                   EXCEPT    HEADL
     C                   MOVE      'X'           XXFSTB
     C                   MOVE      '0'           *INOA
     C                   END
     C     XXFSTB        IFEQ      ' '
     C                   EXCEPT    HEADB
     C                   MOVE      'X'           XXFSTB
     C                   ENDIF
     C                   EXCEPT    DETEA
     C                   ENDIF
     C                   ENDIF
      *
     C     PAKY2         READE     PPA01                                  60
     C                   ENDDO                                                  *IN60
      *
     C     XXPRNT        IFEQ      'X'
     C                   EXCEPT    TOTLB
     C     TTNPST        ADD       TTNPS2        TTNPSF
     C                   EXCEPT    TOTLF
     C                   MOVE      *BLANKS       XXE1CK
     C*** BEG ADD *** 02/22/99 ***************************************
     C                   MOVE      *BLANKS       XXEACK
     C*** END ADD *** 02/22/99 ***************************************
     C                   ENDIF
      *
     C                   ENDSR                                                  SRCHEK
     C*** END ADD *** 03/29/96 *************************************************
     C/EJECT
      *===============================================================
     C     SCREXC        BEGSR
      *===============================================================
      *
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR02
     C                   EXFMT     SCR02C
     C                   MOVE      '0'           *IN01
      *
     C     SVRRN2        IFNE      0
     C                   Z-ADD     SVRRN2        RRN2
     C                   Z-ADD     0             SVRRN2
     C                   ENDIF                                                  SVRRN2 IFNE
      *
     C                   ENDIF                                                  Q2RECF IFEQ
      *
      *
     C                   ENDSR                                                  SCREXC
     C*
      /EJECT
      *====================================================================*
      * * *   WRITDC SUBROUTINE                              W R I T D C ***
      *====================================================================*
     C     WRITDC        BEGSR
      *
     C                   Z-ADD     *ZEROS        K                 5 0
     C     NXTDC3        TAG
     C                   ADD       1             K
     C     K             CHAIN     SCR02S                             51
      *
     C     *IN51         IFEQ      '0'
      *
     C     SCCASE        IFEQ      0
     C                   GOTO      NXTDC3
      *
     C                   ELSE
     C                   MOVE      'P'           ARTRTP                         TRAN TYPE
     C*
     C*  MOVE SCREEN FIELDS TO FILE FIELDS FOR NMARP
     C*
     C                   MOVE      XXCO#         ARCO#
     C                   MOVE      SCSUB#        ARSUB#                                     G
     C                   MOVEL     SCFND         ARFND
     C                   MOVE      SCFYR         ARFYR
     C                   MOVE      SCCASE        ARCASE
     C                   MOVEL     SCTRCD        ARTRCD
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARPODT
     C                   MOVE      SCCK#         ARCK#
     C                   MOVE      Q1USER        ARUSER
     C                   MOVE      Q1PGM         ARPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C                   Z-ADD     DSTIME        ARTIME
     C                   MOVE      SCINV#        ARINV#
     C                   Z-ADD     SCEOB#        AREOB#
     C*** BEGIN ADD *** 06/26/00 **************************************
     C                   Z-ADD     SCRCN         ARRCN
     C*** END   ADD *** 06/26/00 **************************************
     C                   MOVE      *BLANKS       ARSTAT
     C                   MOVE      SCSYS         ARSYS
     C                   MOVE      SCDESC        ARDESC
     C                   MOVE      SCIAMT        ARAMT
     C                   WRITE     PAR01
      *
     C                   GOTO      NXTDC3
      *
     C                   ENDIF                                                  SCCASE IFEQ
      *
     C                   ENDIF                                                  *IN51 IFEQ
      *
     C                   ENDSR                                                  WRITDC
      *
     C/EJECT
      *===============================================================
     C     FKEYS         BEGSR
      *===============================================================
      *
      ****************************************
      *   F3 = EXIT                          *
      ****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC IFEQ
      *
      ****************************************
      *   F5 = REFRESH                       *
      ****************************************
     C     *INKE         IFEQ      '1'
      *
      *   Clear subfile
      *
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02C
     C                   MOVE      '0'           *IN04
      *
     C                   Z-ADD     0             RRN2
     C                   Z-ADD     0             XXRRN2            5 0
      *
     C*** BEG CHG *** 02/22/99 ****************************************
     C     ##PRVB        IFEQ      'Y'
     C  N42EAKY1         SETLL     PEA02                              60
     C   42EAKY1         SETLL     PEA03                              60
     C                   ELSE
     C  N42E1KY          SETLL     PE101                              60
     C   42E1KY          SETLL     PE102                              60
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 02/22/99 ****************************************
     C*** BEGIN ADD *** 03/29/96 ***********************************************
     C                   MOVE      '0'           *IN70
     C*** END ADD *** 03/29/96 *************************************************
      *
     C                   EXSR      LOAD
      *
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      MAINLN
      *
     C                   ENDIF                                                  *INKE IFEQ
      *
      ****************************************
      *   F12 = PREVIOUS                     *
      ****************************************
     C     *INKL         IFEQ      '1'
      *
     C*** BEGIN ADD *** 03/29/96 ***********************************************
     C     XXPRNT        IFEQ      'X'
      ***** Beg Add ***** 08/26/14 ************************************
     C                   Except    Route
      ***** End Add ***** 08/26/14 ************************************
     C                   CLOSE     QSYSPRT
     C                   MOVE      ' '           XXPRNT
     C                   MOVE      ' '           XXFSTB
     C                   ENDIF
     C*** END ADD *** 03/29/96 *************************************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *INKL IFEQ
      *
      *
      ****************************************
      *   *IN18 -  ROLLUP REQUEST            *
      ****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
      *
     C     *IN32         CABEQ     '1'           MAINLN                         NO ROLL IF EOF
      *
     C                   GOTO      MAINLN
      *
     C                   ENDIF                                                  *IN18 IFEQ  ROLLUP
      *
     C                   GOTO      MAINLN
      *
     C                   ENDSR                                                  FKEYS
     C*
     C*** BEGIN ADD *** 03/29/96 ***********************************************
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
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             UPUSNM              67
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       UPUSNM              67
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE       3  2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O          E            ROUTE          2
      ***** Beg Del ***** 08/26/14 ************************************
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
     O**********             XXOUTQ              30
     O**********          09   QQQUE     30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               63 'INC. 1990, 1991, 1992'
     O****                               84 ', 1993, 1994, 1995, 1'
     O****                               87 '996'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              90
     O                       DSDATE        Y    127
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          121 'PAGE:'
     O                       PAGE          Z    127
     O          E            HEAD        1  1
     O                                           11 'SUBSCRIBER:'
     O                       SCSUB#        3     17
     O                                           19 '-'
     O                       SCSBNM              60
     O                                           67 'GROUP:'
     O                       SCFND         3     71
     O                                           73 '-'
     O                       SCFNNM             114
     O          E            HEAD           1
     O                                           11 'CHECK #:'
     O                       SCCK#         L     20
     O                                           35 'CHECK AMOUNT:'
     O                       SCCAMT        J     49
     O                                           67 'TRANS CODE:'
     O                       SCTRCD              70
     O                                           88 'POSTING DATE:'
     O                       SCPODT        Y     97
     O          E            HEADL       1  2
     O                                           10 'CASE #'
     O                                           20 'INV #'
     O                                           30 'EOMB #'
     O                                           43 'A/R AMT'
     O                                           54 'A/R TYPE'
     O                                           63 'PAY AMT'
     O                                           77 'DESCRIPTION'
     O                                          104 'INFORMATION'
     O          E            HEADA       1  1
     O                       HDGA                68
     O          E            HEADB       1  2
     O                       HDGB                76
     O          E            HEADB1      1  1
     O                       HDGB                76
     O          E            DETEA          1
     O                       XOFYR                3
     O                       XOCASE              12
     O              N70N71N72XOINV#              22
     O                       XOEOB#        L     33
     O                       XOIAMT        L     45
     O              N70N71N72XOTRTP              50
     O                       XOPAMT        LB    63
     O                       XODESC              96
     O               70                         101 'DELETED:'
     O*** BEG CHG *** 02/22/99 **************************************
     O               70      XOUSER             112
     O**********       70      E1USER   112
     O*** END CHG *** 02/22/99 **************************************
     O               70      XXDT          YB   121
     O               71                         111 'ZERO EOMB # IN A/R'
     O               73                         102 'ADJUSTED:'
     O               73      XOUSER             113
     O               73      XXDT          Y    122
     O          E            TOTL        1  3
     O                                           33 '                 AMOUNT '
     O                                           52 'THAT WILL NOT POST:'
     O                       TTNPST        L     63
     O*
     O          E            TOTLB       1  1
     O                                           33 '           AMOUNT '
     O                                           52 'THAT WILL NOT POST:'
     O                       TTNPS2        L     63
     O          E            TOTLF       1  1
     O                                           33 '     TOTAL AMOUNT '
     O                                           52 'THAT WILL NOT POST:'
     O                       TTNPSF        L     63
     O          E            TOTLF       1  1
     O                                           33 '               AMO'
     O                                           52 'UNT THAT WILL POST:'
     O                       XXIAMT        L     63
     C*** END ADD *** 03/29/96 *************************************************
