     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*******************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H* *****************************************************************
     H/TITLE FA713R - API TO CALL ADJUDICATION PROGRAMS
     H* *****************************************************************
     H*  Description:                                                   *
     H*                                                                 *
     H*  THIS API WILL CALL THE APPROPRIATE ADJUDICATION PROGRAMS AS    *
     H*  WELL AS THE NETWORK AND PRE-CERT PROCESSING PROGRAMS           *
     H*******************************************************************
     H*                                                                 *
     H*  Called From:                                                   *
     H*    F0351R: Physician (HCFA) Header Infor update/display         *
     H*    F0353R: Physician (HCFA) Detail Information                  *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*    F4100R: FSMRA Program                                        *
     H*    F4200R: Apportionment Calculation Program                    *
     H*    FA768R: API to update EOB codes/descriptions                 *
     H*                                                                 *
     H*  Created:  08/17/98  C:56  Project:  MC152                      *
     H*                                                                 *
     H* *****************************************************************
     H** REVISIONS:
     H*
     H*  04/30/19 C:062 Project: 152WAB
     H*                 Apply Optacomp Mods.
     H*
     H*  05/02/17 C:079 Project: GA038
     H*                 Add logic for GA to call F5100R
     H*
     H*  04/04/16 C:212 Project: OK022
     H*                 Replace call to F4100R with F5100R and other changes
     H*
     H*  02/19/16 C:079 Support: 114/39926
     H*                 Redo fix for 39337
     H*
     H*  01/15/16 C:079 Support: 114/39337
     H*                 Call to N9601R takes priority over call to N9600R
     H*
     H*  06/27/14 C:079 Redmine: 4440
     H*                 Call rtvcltid at program start so its available
     H*                 throughout the program                              .
     H*
     H*  11/25/13 C:282 Project: 129WSC
     H*                 Call to the apportionment program was already added
     H*                 in Base under the project "03/25/03 Project: 129MBH".
     H*                 Only added the condition to call the apportionment
     H*                 program for 129 only.
     H*
     H*  09/17/13 C:079 Project: SEQ83514
     H*                 Add logic for CAMJIF back in
     H*
     H*  09/17/13 C:079 Project: SEQ83514
     H*                 Correct logic for CAMJIF
     H*
     H*  04/30/13 C:284 Project: 147MAS
     H*                 Add call to N9601R for clt 114.
     H*
     H*  02/08/13 C:280 Project: 114MEA
     H*                 import of re-priced CAMDEN JIF BILLS
     H*
     H*  06/22/12 C:280 PROJECT : 114MER
     H*                 OUTPATIENT HOSPITAL BILLS AN ASC'S
     H*
     H*  05/23/11 C:251 PROJECT : FS389
     H*                 Call to FA768R for 'FL' to update EOB/description
     H*
     H*  04/07/08 C:189 PROJECT : TX039
     H*                 2008 TX Hospital Fee Guidelines - IP/OP

     H*  01/29/04 C:79  Support: 41090
     H*                 For subtype = 'ASC' set KKATYP = 'A'
     H*
     H*  03/25/03 C:62  Project: 129MBH
     H*                 Add call to the apportionment program.
     H*
     H*  08/21/02 C:79  Project: 114MDC
     H*                 1. Add call to new FSMRA program F4100R.
     H*
     H*  06/07/02 C:62  Project: 114MDC
     H*                 Check to see if CodeReview installed and update
     H*                 EOCRNE.
     H*
     H*  04/05/02 C:79  Project: 114MAO
     H*                 Clear correct coding and code review messages
     H*
     H*  11/23/99 C:79  Support: MC044/20789
     H*                 Clear payment and reduction fields FDCAP before
     H*                 adjudication
     H*
     H*  11/10/99 C:31  Project: NW011
     H*                 Discount Calculation changes
     H*                 1. Change FDCAP logic to EOB# instead of DCS#
     H*
     H*  11/09/99 C:31  Support: 20483
     H*                 Network D7 issue.
     H*                 1. Remove outside account requirement
     H*                                                                 *
     H*  05/20/99 C:79  PROJECT: MO002
     H*                 Change QQACDT to 8 numeric
     H*                                                                 *
     H*  04/22/99 C:56  PROJECT: NW011
     H*                 CHANGE NETWORK PROGRAM CALLED
     H*
     H*  04/08/99 C:56  Support: MC095 #17568
     H*             1.  Add CO#/SUB#/FND as parms to call of
     H*                 adjudication programs
     H*                                                                 *
     H* *****************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*    99 - General File Access                                     *
     H* *****************************************************************
     H*
     FFA713F    CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     INFDS(FIDS01)
     F*
     F* EOMB Transaction File
     F*** BEGIN CHG *** 06/07/02 **************************************
     F**********FAEOP   IF  E                    DISK
     FFAEOP     UF   E             DISK
     F*** END   CHG *** 06/07/02 **************************************
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
     F*** BEG CHG *** 11/23/99 ****************************************
     F***********  FDCAL13 IF  E           K        DISK
     FFDCAL13   UF   E           K DISK
     F*** END CHG *** 11/23/99 ****************************************
     F*
     F* State FS Rules/PGM Cross Reference File
     FFDRUL1    IF   E           K DISK
     FFMF1L     IF   E           K DISK
     F*** BEG ADD *** 11/23/99 ****************************************
     F* State override table file logical
     FFTABL     IF   E           K DISK
     F*** END ADD *** 11/23/99 ****************************************
     F*** BEGIN ADD *** 06/07/02 **************************************
     FFMCXL     IF   E           K DISK
     F*** END   ADD *** 06/07/02 **************************************
     F*** BEG ADD *** 04/07/08 ****************************************
     FFAEUL     IF   E           K DISK
     F*** END   ADD * 04/07/08 ****************************************
     F*** Beg Add *** 04/30/13 ****************************************
     FFTLPL1    IF   E           K DISK
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

     D JSTP            S             40    DIM(6) CTDATA PERRCD(1)              JOB STEP DESCRIPTION
     D JST             S              1    DIM(40)                              JOB STEP
     D APGM            S              6    DIM(10)                              ADJUDICATION PROGRAM
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
     D*** Beg Add *** 01/15/16 ****************************************
     DRtvCltId         pr                  extpgm('RTVCLTID')
     D cltId                          3
     D cltId           s              3
     D isN9601R        s              1
     D*** End Add *** 01/15/16 ****************************************
     D***** Beg Add *** 04/04/16 *******************************************
     D kkkey           s                   like(STkey) inz
     D***** End Add *** 04/04/16 *******************************************

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@RRN             9 0          FAEOP
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*MDY'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
      *** BEG ADD *** 05/20/99 ****************************************
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *** END ADD *** 05/20/99 ****************************************
      *** BEG ADD *** 11/23/99 ****************************************
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *** END ADD *** 11/23/99 ****************************************
     C*
     C*  KEY LISTS
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
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
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
      *
     C*** BEG ADD *** 11/23/99 ****************************************
      *  Adjudication code Table File
     C     TCSKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    TABKEY           10
     C*** END ADD *** 11/23/99 ****************************************
     C*** BEGIN ADD *** 06/07/02 **************************************
     C*
     C*   Group Control File
     C*
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*** END   ADD *** 06/07/02 **************************************
      *
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
     C     *LIKE         DEFINE    RUST          KKAST
     C     *LIKE         DEFINE    RUATYP        KKATYP
     C     *LIKE         DEFINE    RUEFDT        KKFDAT
     C*** BEG ADD *** 04/08/99 ****************************************
     C     *LIKE         DEFINE    EOCO#         #1CO#
     C     *LIKE         DEFINE    EOSUB#        #1SUB#
     C     *LIKE         DEFINE    EOFND         #1FND
     C*** END ADD *** 04/08/99 ****************************************
     C*** Beg Add *** 06/27/14 ****************************************
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       cltid             3
     C*** End Add *** 06/27/14 ****************************************
     C*
     C                   SETOFF                                       71
     C   71              EXFMT     SCR99
     C*
     C     @@RRN         CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C* SET KEY FIELDS
     C*
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C*
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   Z-ADD     EOCO#         #1CO#
     C                   Z-ADD     EOSUB#        #1SUB#
     C                   Z-ADD     EOFND         #1FND
     C*** END ADD *** 04/08/99 ****************************************
     C*
     C* SET BILL TYPE AND ADJ STATE FOR ADJUDICATION PROGRAM LOOK-UP
     C* ASC BILLS NEED TO USE ADJ TYPE = 'A'
     C*
     C     EOBTYP        IFEQ      'N'
     C                   MOVE      'P'           KKATYP
     C                   ELSE
     C                   MOVE      EOBTYP        KKATYP
     C                   ENDIF                                                  EOBTYP = 'N'
     C*** BEG ADD *** 01/29/04 ****************************************
     C*
     C     EOSTYP        IFEQ      'ASC'
     C                   MOVE      'A'           KKATYP
     C                   ENDIF                                                  EOSTYP = ASC
     C*** END ADD *** 01/29/04 ****************************************
     C*
     C                   MOVE      EOAST         KKAST
     C*** BEGIN DEL *** 06/07/02 **************************************
     C*
     C* DISPLAY ADJUDICATION STEP
     C*
     C**********           MOVE *BLANKS   SCSTP1
     C**********           MOVE *BLANKS   SCSTP2
     C**********           MOVELJSTP,1    SCSTP1
     C**********           MOVEAJSTP,2    JST
     C**********           MOVEAEOAST     JST,9
     C**********           MOVEAJST01     SCSTP2
     C**********           WRITEWDW01
     C*** END   DEL *** 06/07/02 **************************************
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
     C*** BEGIN ADD *** 06/07/02 **************************************
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
     C*** BEG ADD *** 06/22/12 **************************************
     C     EOBTYP        IFEQ      'H'
     C*
     C     CAKY          CHAIN     PEU01                              99
     C*
     C                   ENDIF
     C*** END ADD *** 06/22/12 **************************************
     C*
     C     EOBTYP        IFEQ      'N'
     C*
     C*** BEG ADD *** 06/22/12 **************************************
     C*
     C     EOBTYP        OREQ      'H'
     C     EUBLTY        ANDGE     '130'
     C     EUBLTY        ANDLE     '139'
     C*
     C     EOBTYP        OREQ      'H'
     C     EUBLTY        ANDGE     '830'
     C     EUBLTY        ANDLE     '839'
     C*
     C*** END ADD *** 06/22/12 **************************************
     C*
     C     ##CRVW        IFEQ      'Y'
     C*** Beg Add *** 02/08/13 **************************************
     C*** Beg Del *** 09/17/13 **************************************
     C**** EOCRNE        ANDNE     'N'
     C*** End Del *** 09/17/13 **************************************
     C*** Beg Add *** 09/17/13 **************************************
     C     EOCRNE        ANDNE     'N'
     C*** End Add *** 09/17/13 **************************************
     C*** End Add *** 02/08/13 **************************************
     C*
     C     CXKY          CHAIN     PCX01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     CXCRUS        ANDEQ     'Y'
     C                   MOVEL(P)  'Y'           EOCRNE
     C                   ELSE
     C                   MOVEL(P)  'N'           EOCRNE
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVEL(P)  'N'           EOCRNE
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVEL(P)  'N'           EOCRNE
     C                   ENDIF
     C*
     C                   UPDATE    PEO01
     C*
     C* Don't process if bill is going to pass thru CodeReview.
     C*
     C     EOCRNE        IFNE      'Y'
     C*
     C* Display Adjudication Step
     C*
     C                   MOVE      *BLANKS       SCSTP1
     C                   MOVE      *BLANKS       SCSTP2
     C                   MOVEL     JSTP(1)       SCSTP1
     C                   MOVEA     JSTP(2)       JST
     C                   MOVEA     EOAST         JST(9)
     C                   MOVEA     JST           SCSTP2
     C                   WRITE     WDW01
     C*** END   ADD *** 06/07/02 **************************************
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
      *** BEG CHG *** 05/20/99 ****************************************
     C******************** CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
      *** END CHG *** 05/20/99 ****************************************
     C                   MOVEL     SRDTO         ##ACDT
     C*
     C                   MOVEL     EOSPCD        ##SPCD
     C                   Z-ADD     EOFEIN        ##FEIN
     C                   Z-ADD     EOMOD         ##MOD
     C                   MOVE      *ZEROS        ##DCS#
     C                   Z-ADD     EOEOB#        ##EOB#
     C                   MOVEL     EOAST         ##AST
     C*
     C* CYCLE THROUGH DETAIL CHARGE FILE FOR EOB# AND LOAD
     C* NECESSARY ADJUDICATION PROGRAMS IN ARRAY 'APGM'
     C*
     C     CAKY          CHAIN     PCA01                              96
     C     *IN96         DOWEQ     *OFF
     C*** BEG ADD *** 11/23/99 ****************************************
     C                   EXSR      CLRFLD
     C*** END ADD *** 11/23/99 ****************************************
     C                   Z-ADD     CAFDAT        KKFDAT
     C*** BEG ADD *** 04/07/08 ****************************************
     C                   EXSR      CHKEUD
     C*** END ADD *** 04/07/08 ****************************************
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
     C     CAKY          READE     PCA01                                  96
     C                   ENDDO                                                  *IN96 = *OFF
     C*
     C* CALL ADJUDICATION PROGRAMS LISTED IN ARRAY 'APGM'
     C*
     C*
     C* DELETE ANY EXISTING ERROR MESSAGE PRIOR TO ADJUDICATION
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
     C                   MOVE      'N'           #2DEL3            1
     C*** BEG CHG *** 04/05/02 ****************************************
     C******************** MOVE *BLANKS   #2DEL4  1
     C******************** MOVE *BLANKS   #2DEL5  1
     C                   MOVE      'C'           #2DEL4            1
     C                   MOVE      'R'           #2DEL5            1
     C*** END CHG *** 04/05/02 ****************************************
     C                   CALL      'F0078R'
     C                   PARM                    #2CO#
     C                   PARM                    #2SUB#
     C                   PARM                    #2FND
     C                   PARM                    #2FYR
     C                   PARM                    #2CASE
     C                   PARM                    #2EOB#
     C                   PARM                    #2RCN
     C                   PARM                    #2DEL1
     C                   PARM                    #2DEL2
     C                   PARM                    #2DEL3
     C                   PARM                    #2DEL4
     C                   PARM                    #2DEL5
     C*
     C                   Z-ADD     1             K                 2 0
     C                   DO        10            K
     C     APGM(K)       IFNE      *BLANKS
     C                   MOVEL(P)  APGM(K)       ADJPGM            8
     C*
     C* LOAD ADJUDICATION STATE IF NOT F4000R - NEW FEE SCHEDULE PROCESS
     C*
     C*****      ADJPGM    IFNE 'F4000R'
TEMP C*****      ADJPGM    IFNE 'F400XR'
     C     ADJPGM        IFNE      'F4000N'
     C                   MOVE      KKAST         ADJPGM
     C                   ENDIF                                                  ADJPGM NE F4000R
     C*
     C*****      ADJPGM    IFEQ 'F4000R'
TEMP C*****      ADJPGM    IFEQ 'F400XR'
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
      *** BEG CHG *** 05/20/99 ****************************************
     C******************** PARM           ##ACDT  60
     C                   PARM                    ##ACDT            8 0
      *** END CHG *** 05/20/99 ****************************************
     C                   PARM                    ##SPCD            3
     C                   PARM                    ##FEIN            9 0
     C                   PARM                    ##MOD             3 0
     C                   PARM                    ##DCS#            5
     C                   PARM                    ##AST             2
     C                   PARM                    ##EOB#
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C*** END ADD *** 04/08/99 ****************************************
     C*
     C                   ELSE
     C*
     C                   CALL      ADJPGM
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C**********           PARM           ##FDTS           (5 DATES)
     C**********           PARM           ##FDT2
     C**********           PARM           ##SEQS           (30 SEQ#S)
     C**********           PARM           ##ER#S           (30 ERRS)
     C                   PARM                    ##RTCD
     C                   PARM                    ##SS#
     C                   PARM                    ##ACDT
     C                   PARM                    ##SPCD
     C                   PARM                    ##EOB#
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C*** END ADD *** 04/08/99 ****************************************
     C*
     C                   ENDIF                                                  ADJPGM = F4000R
     C*
     C                   ENDIF                                                  ADJPGM NE K
     C                   ENDDO                                                  DO 10
     C*
     C*** BEG ADD *** 08/21/02 ****************************************
     C*
     C* CALL FSMRA PROCESSING PROGRAM IF NECESSARY
     C*
     C***** Beg Add *** 04/04/16 *******************************************
      /free
         kkkey = EOast;
         Chain(n) kkkey pst01;
      /end-free
     C***** Beg Add *** 04/04/16 *******************************************
     C***** Beg Add *** 05/02/17 *******************************************
     C                   if        STbs03 = 'Y'
     C***** End Add *** 05/02/17 *******************************************
     C*
     C     EOAST         IFEQ      'OK'
     C***** Beg Add *** 04/04/16 *******************************************
     C***** Beg Del *** 05/02/17 *******************************************
     C**** STbs03        ANDEQ     'Y'
     C***** End Del *** 05/02/17 *******************************************
     C***** End Add *** 04/04/16 *******************************************
     C     EOAST         OREQ      'AL'
     C     EOBTYP        ANDEQ     'H'
     C***** Beg Add *** 05/02/17 *******************************************
     C     EOast         oreq      'GA'
     C     EUblty        andge     '131'
     C     EUblty        andle     '139'
     C*
     C     EOast         oreq      'GA'
     C     EUblty        andge     '831'
     C     EUblty        andle     '839'
     C*
     C***** Beg Add *** 05/02/17 *******************************************
     C                   endif
     C***** End Add *** 05/02/17 *******************************************
     C*
     C***** Beg Chg *** 04/04/16 *******************************************
     C*****              CALL      'F4100R'
     C                   CALL      'F5100R'
     C***** End Chg *** 04/04/16 *******************************************
     C                   PARM                    @@RRN
     C*
     C                   ENDIF                                                  EOAST IFEQ OK
     C*** END ADD *** 08/21/02 ****************************************
     C*
     C                   ENDIF                                                  EOCRN IFEQ N
     C*
     C*** BEGIN ADD *** 06/07/02 **************************************
     C                   ENDIF
     C*
     C* Don't process if bill is going to pass thru CodeReview.
     C*
     C     EOCRNE        IFNE      'Y'
     C*** END   ADD *** 06/07/02 **************************************
     C*
     C* CALL PRE-CERT EDITING PROGRAM IF NECESSARY
     C*
     C     EOCERT        IFNE      *BLANKS
     C*
     C*               DISPLAY PRE-CERT EDITING STEP
     C*
     C                   MOVE      *BLANKS       SCSTP1
     C                   MOVE      *BLANKS       SCSTP2
     C                   MOVEL     JSTP(3)       SCSTP1
     C                   MOVEA     JSTP(4)       SCSTP2
     C                   WRITE     WDW01
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
     C                   CALL      'U0066R'
     C                   PARM                    ##FYR             3 0
     C                   PARM                    ##CASE            7 0
     C                   PARM      ' '           ##RTCD            1
     C                   PARM                    ##SS#             9 0
     C                   PARM                    #1ACDT            8 0
     C                   PARM                    ##CERT           15
     C                   PARM                    ##EXT             3
     C                   PARM                    ##EOB#
     C                   ENDIF                                                  EOCERT NE *BLANKS
     C*
     C* CALL NETWORK DISCOUNTING PROGAM IF NECESSARY
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
     C                   CALL      'FA701R'
     C                   PARM      ' '           ##RTCD
     C                   PARM                    ##CO#
     C                   PARM                    ##SUB#
     C                   PARM                    ##FND
     C                   PARM                    ##OUTS            1
     C*
     C*** BEGIN CHG *** 11/09/99 **************************************
     C********   ##OUTS    IFEQ 'N'
     C********   EONTWK    ANDNE*BLANKS
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
             chain caky  peh01;
             If %found(faehl);
                kklic# = ehflic;
             Endif;
          Elseif eobtyp = 'H';
             chain caky  peu01;
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
     C*** END CHG *** 11/09/99 ****************************************
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
     C*               DISPLAY NETWORK PROCESSING STEP
     C*
     C                   MOVE      *BLANKS       SCSTP1
     C                   MOVE      *BLANKS       SCSTP2
     C                   MOVEL     JSTP(5)       SCSTP1
     C                   MOVEA     JSTP(6)       SCSTP2
     C                   WRITE     WDW01
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
     C*** BEGIN CHG *** 11/10/99 **************************************
     C********             MOVE *BLANKS   ##DCS#  5 P
     C                   MOVE(P)   EOEOB#        #1EOB#            9
     C*** END CHG *** 11/10/99 ****************************************
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
     C*** BEG CHG *** 04/22/99 ****************************************
     C**********           CALL 'N9500R'
     C*** Beg Add *** 01/15/16 ****************************************
     C** N9601R takes priority over N9600R call
     C                   If        isN9601R <> 'Y'
     C*** End Add *** 01/15/16 ****************************************
     C                   CALL      'N9600R'
     C*** END CHG *** 04/22/99 ****************************************
     C                   PARM                    ##CO#
     C                   PARM                    ##SUB#
     C                   PARM                    ##FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C                   PARM                    #1FEIN
     C                   PARM                    #1MOD
     C*** BEGIN CHG *** 11/10/99 **************************************
     C********             PARM           ##DCS#
     C                   PARM                    #1EOB#
     C*** END CHG *** 11/10/99 ****************************************
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
      /Free
       //**** If cltid = '114' and eontwk <> *blanks;
       //**** If eobtyp = 'N';
       //****      chain caky  peh01;
       //****      If %found(faehl);
       //****         kklic# = ehflic;
       //****      Endif;
       //****   Elseif eobtyp = 'H';
       //****      chain caky  peu01;
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
          //*** Endif;
       //** End Del *** 01/15/16 ****************************************
          Endif;
      /End-free
     C*** End Add *** 04/30/13 ****************************************
     C*
     C*** BEGIN CHG *** 11/09/99 **************************************
     C********             ENDIF                           ##OUTS = N
     C                   ENDIF                                                  NTWK <> blanks
     C*** END CHG *** 11/09/99 ****************************************
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
     C                   PARM                    @@RRN
     C*** BEG ADD *** 11/25/13 ****************************************
      /Free
       EndIf;
      /End-Free
     C*** END ADD *** 11/25/13 ****************************************
     C*
     C*** END   ADD *** 03/25/03 **************************************
     C*** BEGIN ADD *** 06/07/02 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/07/02 **************************************
     C*** BEG ADD *** 05/23/11 ****************************************
     C     EOAST         IFEQ      'FL'
     C                   CALL      'FA768R'
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN
     C                   PARM                    @@RTCD            1
     C                   ENDIF
     C*** END ADD *** 05/23/11 ****************************************
     C*
     C                   MOVE      *ON           *INLR
      *** BEG ADD *** 11/23/99 ****************************************
     C*****************************************************************
     C* SUBROUTINE  ****** C L R F L D ********************************
     C*****************************************************************
     C     CLRFLD        BEGSR
      *
      * Get the adjudication code
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C*
      * Only update charges for current recon and not override type
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
     C*** BEG  ADD *** 04/07/08 ****************************************
     C*****************************************************************
     C* SUBROUTINE  ****** CHKEUD      ********************************
     C*****************************************************************
     C     CHKEUD        BEGSR
      *
     C     CAKY          CHAIN     PEU01                              99
     C     EOBTYP        IFEQ      'H'
     C     EOAST         ANDEQ     'TX'
     C     EUICN         IFEQ      'IM'
     C     EUICN         OREQ      'IP'
     C     EUICN         OREQ      'IR'
     C     EUICN         OREQ      'IS'
      *
     C     EUADDT        ANDGE     1080301
     C                   Z-ADD     EUADDT        KKFDAT
      *
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END  ADD *** 04/07/08 ****************************************
     OPCA01     E            RCA01
      *** END ADD *** 11/23/99 ****************************************
     C*****************************************************************
**  JOBSTEP - STEP IN BILL PROCESS DISPLAYED TO USER
The entered bill is now being processed
against   's state reimbursement rules.
The entered bill is now being processed
against the selected Pre/Re-Cert.
The entered bill is now being processed
thru the PPO/Network Discounting.
