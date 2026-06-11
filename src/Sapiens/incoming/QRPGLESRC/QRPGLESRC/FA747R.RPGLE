     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*                                                               *
     H* API - FA747R - Texas: Retrieve Procedure Code Fees            *
     H*******************************************************************
     H*                                                               * *
     H*  Called From: N96DMR - DM Network Algorithm                   * *
     H*               N0582R - Prov/Disc Information                  * *
     H*               N4052NTX - ADJUDICATION (08/01/97 RULES)        * *
     H*  Calls To:    None                                            * *
     H*                                                               * *
     H*******************************************************************
     H* Created on 04/10/2007 by c:62 for  Project 25MOD              * *
     H*******************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*  02/15/16 C:079 Support 25/388986                             *
     H*                 Get locality using 9 digit zip code           *
     H*                                                               *
     H*  10/15/10 C:062 Project: MC025 74537                          *
     H*                 Add parm checking.                            *
     H*                                                               *
     H*  02/26/10 C:198 Project: MC337                                *
     H*                 Alliance Network Contract Modifications       *
     H*                                                               *
     H*  12/22/08 C:198 Support: 65839                                *
     H*                 Added another parameter to *entry fields      *
     H*                                                               *
     H*  07/02/07 C:182 Support: 57703                                *
     H*                 Added condition to execute GETAMT subroutine  *
     H*                 other than CPT4 99445 & 99456.                *
     H*****************************************************************
     FFDCAP     IF   E             DISK
     FFAEOL4    IF   E           K DISK
     FFDDHL     IF   E           K DISK
     FFMPRLTX   IF   E           K DISK
     FFMZRLTX   IF   E           K DISK
     FFMP1L     IF   E           K DISK
     FFAAGLTX   IF   E           K DISK
     FFTCCL1    IF   E           K DISK
     F*** Beg Add *** 02/26/10 ****************************************
     FFMDMLTX   IF   E           K DISK
     F*** End Add *** 02/26/10 ****************************************
     D*** Beg Add *** 02/15/16 ********************************************
     D* prototype definitions
     D/copy FPRZIPLOC
     D*** End Add *** 02/15/16 ********************************************

     D*================================================================
     D* Arrays
     D*================================================================
     D MOD             S              2    DIM(4) ASCEND
     D*** BEGIN ADD *** 10/15/10 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** END   ADD *** 10/15/10 **************************************
     D*** Beg Add *** 02/15/16 ********************************************
     D*================================================================
     D* standalone variables
     D*================================================================
     Dx1loc#           s              3  0 inz
     D*** End Add *** 02/15/16 ********************************************

     C*================================================================
     C* Parameter lists
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@MRA
     C                   PARM                    @@PROF
     C                   PARM                    @@TECH
     C*** BEG ADD *** 12/22/08 ****************************************
     C     PR1A7         PARM                    @@1A7             1
     C*** END ADD *** 12/22/08 ****************************************
     C*** Beg Add *** 02/26/10 ****************************************
     C*** ADDED TO DECIDE WHERE IT IS CALLED FROM - A=N96DMR, B=N0582R
     C                   PARM                    @@CALL            1
     C*** Beg Add *** 02/26/10 ****************************************
     C*================================================================
     C* Key lists
     C*================================================================
     C*
     C     AGKY1         KLIST
     C                   KFLD                    KKGRP
     C                   KFLD                    KKRGN
     C                   KFLD                    CAFDAT
     C*
     C     AGKY2         KLIST
     C                   KFLD                    KKGRP
     C*
     C     CCKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    KKKEY
     C*
     C     CAKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*
     C     DHKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*
     C     EOKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C*
     C     P1KY          KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C*
     C     PRKY1         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    KKCPT4
     C                   KFLD                    CAFDAT
     C*
     C     PRKY2         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    KKCPT4
     C*
     C     ZRKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    XXZIP
     C                   KFLD                    CAFDAT
     C*** Beg Add *** 02/26/10 ****************************************
     C     DMKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    XXCPT4            5
     C                   KFLD                    XXEFDT            7 0
     C                   KFLD                    XXMOD1            2
     C                   KFLD                    XXMOD2            2
     C*** End Add *** 02/26/10 ****************************************
     C*================================================================
     C* Like Defines
     C*================================================================
     C     *LIKE         DEFINE    CAMRA         @@MRA
     C     *LIKE         DEFINE    CAMRA         @@PROF
     C     *LIKE         DEFINE    CAMRA         @@TECH
     C     *LIKE         DEFINE    PRCPT4        KKCPT4
     C     *LIKE         DEFINE    ZRFZCD        XXZIP
     C     *LIKE         DEFINE    ZRRGN#        XXLOC#
     C     *LIKE         DEFINE    CCKEY         KKKEY
     C     *LIKE         DEFINE    AGRGN         KKRGN
     C     *LIKE         DEFINE    AGGRP         KKGRP
     C*** Beg Add *** 02/26/10 ****************************************
     C     *LIKE         DEFINE    DHMOD1        MOD1
     C     *LIKE         DEFINE    DHMOD1        MOD2
     C     *LIKE         DEFINE    DHMOD1        MOD3
     C     *LIKE         DEFINE    DHMOD1        MOD4
     C     *LIKE         DEFINE    DHMOD1        MODI
     C     *LIKE         DEFINE    DHMOD1        MODJ
     C*** End Add *** 02/26/10 ****************************************
     C*================================================================
     C* Main Processing
     C*================================================================
     C*
     C     @@RRN         CHAIN     PCA01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CACPT4        KKCPT4
     C*
     C     DHKY          CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C     CACPT4        IFEQ      '99455'
     C     CACPT4        OREQ      '99456'
     C                   MOVE      DHMOD1        MOD(1)
     C                   MOVE      DHMOD2        MOD(2)
     C                   MOVE      DHMOD3        MOD(3)
     C                   MOVE      DHMOD4        MOD(4)
     C*
     C     'V1'          LOOKUP    MOD                                    99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  '99211'       KKCPT4
     C                   ELSE
     C     'V2'          LOOKUP    MOD                                    99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  '99212'       KKCPT4
     C                   ELSE
     C     'V3'          LOOKUP    MOD                                    99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  '99213'       KKCPT4
     C                   ELSE
     C     'V4'          LOOKUP    MOD                                    99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  '99214'       KKCPT4
     C                   ELSE
     C     'V5'          LOOKUP    MOD                                    99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  '99215'       KKCPT4
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   EXSR      GETAMT
     C*** Beg Add *** 02/26/10 *************************************************
     C     CACPT4        IFGE      '00100'
     C     CACPT4        ANDLE     '01999'
     C                   Z-ADD     CAMRAA        @@MRA
     C                   ENDIF
     C*** End Add *** 02/26/10 *************************************************
     C*** Beg Add *** 02/26/10 *************************************************
     C*** BEGIN ADD *** 10/15/10 **************************************
     C     Q1PRMS        IFGT      5
     C*** END   ADD *** 10/15/10 **************************************
     C     @@CALL        IFEQ      'B'
     C     CACPT4        IFEQ      '99455'
     C     CACPT4        OREQ      '99456'
     C     CACPT4        OREQ      '99080'
     C     CACPT4        OREQ      '97545'
     C     CACPT4        OREQ      '97750'
     C     CACPT4        OREQ      '97799'
|||  C                   Z-ADD     CAMRAA        @@MRA
||-  C                   ENDIF
||-  C                   ENDIF
     C*** BEGIN ADD *** 10/15/10 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/15/10 **************************************
     C*** End Add *** 02/26/10 *************************************************
     C*** BEG ADD ***** 07/02/07 ***********************************************
     C                   ELSE
     C                   MOVEL(P)  CACPT4        KKCPT4
     C                   EXSR      GETAMT
     C*** END ADD ***** 07/02/07 ***********************************************
     C*** Beg Add *** 02/26/10 *************************************************
     C     CACPT4        IFGE      '00100'
     C     CACPT4        ANDLE     '01999'
     C                   Z-ADD     CAMRAA        @@MRA
     C                   ENDIF
     C*** End Add *** 02/26/10 *************************************************
     C*** Beg Add *** 02/26/10 *************************************************
     C*** BEGIN ADD *** 10/15/10 **************************************
     C     Q1PRMS        IFGT      5
     C*** END   ADD *** 10/15/10 **************************************
     C     @@CALL        IFEQ      'B'
     C     CACPT4        IFEQ      '99455'
     C     CACPT4        OREQ      '99456'
     C     CACPT4        OREQ      '99080'
     C     CACPT4        OREQ      '97545'
     C     CACPT4        OREQ      '97750'
     C     CACPT4        OREQ      '97799'
     C     CAPAID        IFLT      CACHRG
     C     @@MRA         IFNE      CAPAID
     C                   MOVE      *ON           RULE              1
     C                   ENDIF
     C                   ENDIF
||-  C     RULE          IFEQ      *ON
|||  C                   Z-ADD     CAMRAA        @@MRA
||-  C                   ENDIF
||-  C                   ENDIF
||-  C                   ENDIF
     C*** BEGIN ADD *** 10/15/10 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/15/10 **************************************
     C*** End Add *** 02/26/10 *************************************************
     C                   ENDIF
     C                   ENDIF
     C*
     C                   RETURN
     C*================================================================
     C* GETAMT - Get the amounts
     C*================================================================
     C     GETAMT        BEGSR
     C*
     C                   CLEAR                   PPR01
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C*
     C*** Beg Add *** 02/26/10 *****************************
     C                   MOVEL     PR1A7         @@1A7
     C*** End Add *** 02/26/10 *****************************
     C                   CLEAR                   PEO01
     C     EOKY          CHAIN     PEO01                              99
     C*
     C                   CLEAR                   PP101
     C     P1KY          CHAIN     PP101                              99
     C*
     C     EOFLIC        IFNE      *BLANKS
     C                   MOVEL     EOFLIC        XXZIP
     C                   ELSE
     C                   MOVEL     P1ZIP         XXZIP
     C                   ENDIF
     C*
     C* Get the locality for 5 digit zip code
     C*
     C*
     C* Default to locality 99
     C*
     C                   Z-ADD     99            XXLOC#
     C*
     C     ZRKY          SETGT     PZR01
     C                   READP     PZR01                                  99
     C     *IN99         IFEQ      *OFF
     C     XXZIP         ANDGE     ZRFZCD
     C     XXZIP         ANDLE     ZRTZCD
     C                   Z-ADD     ZRRGN#        XXLOC#
     C                   ENDIF
     C*** Beg Add *** 02/15/16 ********************************************
     C*
     C* Get the locality for 9 digit zip code
     C*
     C                   if        %subst(EOflic:6:4) > '0000'
     C                   eval      x1loc# = getZip9Loc(EOflic:EOast:CAfDat)
     C*
     C                   if        x1loc# <> 0
     C                   eval      xxloc# = x1loc#
     C                   Endif
     C*
     C                   Endif
     C*** End Add *** 02/15/16 ********************************************
     C*                                                                *****
     C                   MOVEL     'N'           XXFAC             1
     C     DHPOS         IFNE      ' 03'
     C     DHPOS         ANDNE     ' 04'
     C     DHPOS         ANDNE     ' 11'
     C     DHPOS         ANDNE     ' 12'
     C     DHPOS         ANDNE     ' 15'
     C     DHPOS         ANDNE     ' 20'
     C     DHPOS         ANDNE     ' 25'
     C     DHPOS         ANDNE     ' 32'
     C     DHPOS         ANDNE     ' 33'
     C     DHPOS         ANDNE     ' 50'
     C     DHPOS         ANDNE     ' 54'
     C     DHPOS         ANDNE     ' 55'
     C     DHPOS         ANDNE     ' 60'
     C     DHPOS         ANDNE     ' 62'
     C     DHPOS         ANDNE     ' 71'
     C     DHPOS         ANDNE     ' 72'
     C     DHPOS         ANDNE     ' 81'
     C                   MOVEL     'Y'           XXFAC
     C                   ENDIF
     C*
     C*** Beg Add *** 02/26/10 **************************************
     C***        Modify to get amount when code is DMEPOS
     C                   MOVE      DHMOD1        MOD(1)
     C                   MOVE      DHMOD2        MOD(2)
     C                   MOVE      DHMOD3        MOD(3)
     C                   MOVE      DHMOD4        MOD(4)
     C*
     C                   MOVEL     PRCPT4        XXCPT4
     C                   MOVEL     PREFDT        XXEFDT
     C*
     C                   MOVEL     ' '           XXDME             1
     C                   Z-ADD     0             I                 1 0
     C                   MOVEL     ' '           XXOUT
     C*
     C     I             DOWLT     5
     C*
     C                   ADD       1             I
     C     I             IFLT      5
     C     MOD(I)        DOWEQ     '  '
     C                   ADD       1             I
     C     I             IFGT      4
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ENDIF                                                  I < 5
     C*
     C     I             IFGE      5
     C                   MOVEL     'Y'           XXOUT             1
     C                   ENDIF
     C*
     C     XXOUT         IFNE      'Y'
     C*
     C                   Z-ADD     0             J                 1 0
     C     J             ADD       1             L                 1 0
     C*
     C     J             DOWNE     5
     C     L             ANDNE     5
     C                   ADD       1             J
     C     J             ADD       1             L
     C     MOD(J)        DOWEQ     '  '
     C                   ADD       1             J
     C     J             ADD       1             L
     C     J             IFGT      4
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     J             IFNE      I
     C*
     C     J             IFNE      5
     C     I             ANDNE     5
     C                   MOVEL     MOD(I)        MODI
     C                   MOVEL     MOD(J)        MODJ
     C*
     C     MODI          IFNE      ' '
     C     MODJ          ANDNE     ' '
     C                   MOVEL     MOD(I)        XXMOD1
     C                   MOVEL     MOD(J)        XXMOD2
     C                   ENDIF
     C*
     C     DMKY          CHAIN     PDM01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DMMRA         @@MRA
     C                   MOVEL     '  '          XXMOD2
     C                   MOVEL     'Y'           XXDME
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF                                                  I <> 5 & J <> 5
     C*
     C                   ENDIF                                                  I <> J
     C*
     C                   ENDDO                                                  J <> 5 & L <> 5
     C*
     C                   ENDIF                                                  XXOUT <> 'Y'
     C*
     C                   ENDDO                                                  I < 5
     C*
     C     XXDME         IFNE      'Y'
     C*
     C                   MOVEL     *BLANKS       XXMOD1
     C                   MOVEL     MOD(1)        MOD1
     C                   MOVEL     MOD(2)        MOD2
     C                   MOVEL     MOD(3)        MOD3
     C                   MOVEL     MOD(4)        MOD4
     C                   SELECT
     C     MOD1          WHENNE    ' '
     C     MOD2          ANDEQ     ' '
     C     MOD3          ANDEQ     ' '
     C     MOD4          ANDEQ     ' '
     C                   MOVEL     MOD1          XXMOD1
     C     MOD1          WHENEQ    ' '
     C     MOD2          ANDNE     ' '
     C     MOD3          ANDEQ     ' '
     C     MOD4          ANDEQ     ' '
     C                   MOVEL     MOD2          XXMOD1
     C     MOD1          WHENEQ    ' '
     C     MOD2          ANDEQ     ' '
     C     MOD3          ANDNE     ' '
     C     MOD4          ANDEQ     ' '
     C                   MOVEL     MOD3          XXMOD1
     C     MOD1          WHENEQ    ' '
     C     MOD2          ANDEQ     ' '
     C     MOD3          ANDEQ     ' '
     C     MOD4          ANDNE     ' '
     C                   MOVEL     MOD4          XXMOD1
     C                   ENDSL                                                  SELECT STMT
     C*
     C                   MOVEL     '  '          XXMOD2
     C*
     C     DMKY          CHAIN     PDM01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DMMRA         @@MRA
     C                   MOVEL     '  '          XXMOD2
     C                   MOVEL     'Y'           XXDME
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXDME <> 'Y'
     C* If no amount was loaded from FMDMPTX then load from FMPRPTX
     C*
     C     @@MRA         IFEQ      0
     C*
     C* Default to load from MRA
     C*
     C                   Z-ADD     PRMRA         @@MRA
     C*
     C     'RR'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PRANAM        @@MRA
     C                   ENDIF
    C*
     C*
     C***        Modify to have amount for when from Clinical lab
     C*** End Add *** 02/26/10 **************************************
     C* Load amount fields based on region
     C*
     C                   SELECT
     C*----------------------------------------------------------------
     C* Location 9
     C*----------------------------------------------------------------
     C     XXLOC#        WHENEQ    9
     C                   Z-ADD     PRMRA         @@MRA
     C*
     C* Facility allowance
     C*
     C     XXFAC         IFEQ      'Y'
     C                   Z-ADD     PR92N6        @@MRA
     C                   ENDIF
     C*
     C* Modifiers
     C*
     C     'KM'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PRANAM        @@MRA
     C                   ENDIF
     C*
     C     'UE'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92N7        @@MRA
     C                   ENDIF
     C*
     C     'KN'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92N7        @@MRA
     C                   ENDIF
     C*
     C* Prof, tech and anes
     C*
     C                   Z-ADD     PRPROF        @@PROF
     C                   Z-ADD     PR92N1        @@TECH
     C*----------------------------------------------------------------
     C* Location 11
     C*----------------------------------------------------------------
     C     XXLOC#        WHENEQ    11
     C                   Z-ADD     PR92N8        @@MRA
     C*
     C* Facility allowance
     C*
     C     XXFAC         IFEQ      'Y'
     C                   Z-ADD     PR92N9        @@MRA
     C                   ENDIF
     C*
     C* Modifiers
     C*
     C     'KM'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92NA        @@MRA
     C                   ENDIF
     C*
     C     'UE'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92ND        @@MRA
     C                   ENDIF
     C*
     C     'KN'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92ND        @@MRA
     C                   ENDIF
     C*
     C* Prof, tech and anes
     C*
     C                   Z-ADD     PR92NB        @@PROF
     C                   Z-ADD     PR92NC        @@TECH
     C*----------------------------------------------------------------
     C* Location 15
     C*----------------------------------------------------------------
     C     XXLOC#        WHENEQ    15
     C                   Z-ADD     PR92NE        @@MRA
     C*
     C* Facility allowance
     C*
     C     XXFAC         IFEQ      'Y'
     C                   Z-ADD     PR92NF        @@MRA
     C                   ENDIF
     C*
     C* Modifiers
     C*
     C     'KM'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92NG        @@MRA
     C                   ENDIF
     C*
     C     'UE'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92NJ        @@MRA
     C                   ENDIF
     C*
     C     'KN'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92NJ        @@MRA
     C                   ENDIF
     C*
     C* Prof, tech and anes
     C*
     C                   Z-ADD     PR92NH        @@PROF
     C                   Z-ADD     PR92NI        @@TECH
     C*----------------------------------------------------------------
     C* Location 18
     C*----------------------------------------------------------------
     C     XXLOC#        WHENEQ    18
     C                   Z-ADD     PR92NK        @@MRA
     C*
     C* Facility allowance
     C*
     C     XXFAC         IFEQ      'Y'
     C                   Z-ADD     PR92NL        @@MRA
     C                   ENDIF
     C*
     C* Modifiers
     C*
     C     'KM'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92NM        @@MRA
     C                   ENDIF
     C*
     C     'UE'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92NP        @@MRA
     C                   ENDIF
     C*
     C     'KN'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92NP        @@MRA
     C                   ENDIF
     C*
     C* Prof, tech and anes
     C*
     C                   Z-ADD     PR92NN        @@PROF
     C                   Z-ADD     PR92NO        @@TECH
     C*----------------------------------------------------------------
     C* Location 20
     C*----------------------------------------------------------------
     C     XXLOC#        WHENEQ    20
     C                   Z-ADD     PR92NQ        @@MRA
     C*
     C* Facility allowance
     C*
     C     XXFAC         IFEQ      'Y'
     C                   Z-ADD     PR92NR        @@MRA
     C                   ENDIF
     C*
     C* Modifiers
     C*
     C     'KM'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92NS        @@MRA
     C                   ENDIF
     C*
     C     'UE'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92NV        @@MRA
     C                   ENDIF
     C*
     C     'KN'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92NV        @@MRA
     C                   ENDIF
     C*
     C* Prof, tech and anes
     C*
     C                   Z-ADD     PR92NT        @@PROF
     C                   Z-ADD     PR92NU        @@TECH
     C*----------------------------------------------------------------
     C* Location 28
     C*----------------------------------------------------------------
     C     XXLOC#        WHENEQ    28
     C                   Z-ADD     PR92NW        @@MRA
     C*
     C* Facility allowance
     C*
     C     XXFAC         IFEQ      'Y'
     C                   Z-ADD     PR92NX        @@MRA
     C                   ENDIF
     C*
     C* Modifiers
     C*
     C     'KM'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92NY        @@MRA
     C                   ENDIF
     C*
     C     'UE'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92O2        @@MRA
     C                   ENDIF
     C*
     C     'KN'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92O2        @@MRA
     C                   ENDIF
     C*
     C* Prof, tech and anes
     C*
     C                   Z-ADD     PR92NZ        @@PROF
     C                   Z-ADD     PR92O1        @@TECH
     C*----------------------------------------------------------------
     C* Location 31
     C*----------------------------------------------------------------
     C     XXLOC#        WHENEQ    31
     C                   Z-ADD     PR92O3        @@MRA
     C*
     C* Facility allowance
     C*
     C     XXFAC         IFEQ      'Y'
     C                   Z-ADD     PR92O4        @@MRA
     C                   ENDIF
     C*
     C* Modifiers
     C*
     C     'KM'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92O5        @@MRA
     C                   ENDIF
     C*
     C     'UE'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92O8        @@MRA
     C                   ENDIF
     C*
     C     'KN'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92O8        @@MRA
     C                   ENDIF
     C*
     C* Prof, tech and anes
     C*
     C                   Z-ADD     PR92O6        @@PROF
     C                   Z-ADD     PR92O7        @@TECH
     C*----------------------------------------------------------------
     C* Location 99
     C*----------------------------------------------------------------
     C     XXLOC#        WHENEQ    99
     C                   Z-ADD     PR92O9        @@MRA
     C*
     C* Facility allowance
     C*
     C     XXFAC         IFEQ      'Y'
     C                   Z-ADD     PR92OA        @@MRA
     C                   ENDIF
     C*
     C* Modifiers
     C*
     C     'KM'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92OB        @@MRA
     C                   ENDIF
     C*
     C     'UE'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92OE        @@MRA
     C                   ENDIF
     C*
     C     'KN'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PR92OE        @@MRA
     C                   ENDIF
     C*
     C* Prof, tech and anes
     C*
     C                   Z-ADD     PR92OC        @@PROF
     C                   Z-ADD     PR92OD        @@TECH
     C*
     C                   ENDSL
     C*
     C* Load the ASC allowance if applicable
     C*
     C     'SG'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
     C     CAFDAT        ANDGE     1040901
     C*** Beg Add *** 02/26/10 *******************************************
     C     CAFDAT        ANDLT     1080901
     C*** End Add *** 02/26/10 *******************************************
     C     DHPRCD        ANDGE     '10000'
     C     DHPRCD        ANDLE     '69999'
     C                   Z-ADD     0             @@MRA
     C*
     C     3             SUBST     P1CNTY:6      ENDKEY            3
     C     'TX'          CAT(P)    ENDKEY:0      KKKEY
     C     CCKY          CHAIN     PCC01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CC10A1        KKRGN
     C                   MOVE      PR1AI         KKGRP
     C     AGKY1         SETGT     PAG01
     C     AGKY2         READPE    PAG01                                  99
     C     *IN99         IFEQ      *OFF
     C     AGSTAT        ANDEQ     'A'
     C                   Z-ADD     AGALOW        @@MRA
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     F*** Beg Add *** 02/26/10 ****************************************
     C                   ENDIF                                                  @@MRA = 0
     C*
     C**** IF CODE IS FROM CLINICAL LAB
     C     PR1A7         IFEQ      'L'
     C                   Z-ADD     PR92N1        @@TECH
     C                   Z-ADD     0             @@PROF
     C                   Z-ADD     0             @@MRA
     C     @@TECH        MULT(H)   0.45          @@PROF
     C     @@TECH        ADD       @@PROF        @@MRA
     C                   ENDIF
     F*** End Add *** 02/26/10 ****************************************
     C*
     C                   ENDSR
