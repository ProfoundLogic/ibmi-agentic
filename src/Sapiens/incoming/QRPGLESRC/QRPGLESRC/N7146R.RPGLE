     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE N7146R - RE-BILL (RE-INVOICE)
     H*
     H*****************************************************************
     H*  INDICATORS:                                                  *
     H* SCR01                                                         *
     H*                                                               *
     H*  29 - POSITION CURSOR AT YEAR                                 *
     H*  30 - POSITION CURSOR AT SUBCRIBER                            *
     H*  31 - POSITION CURSOR AT GROUP                                *
     H*  32 - POSITION CURSOR AT INV#                                 *
     H*  59 - POSITION CURSOR AT CASE                                 *
     H*  77 - ERROR MESSAGE FOR SUB#  VALIDATION                      *
     H*  78 - ERROR MESSAGE FOR GROUP VALIDATION                      *
     H*                                                               *
     H*****************************************************************
     H*                 R E V I S I O N S                             *
     H*                                                               *
     H* 12/18/24 C:342 Project : MC447 Phase 2                        *
     H*                1.F10 Search disabled on the Group Field       *
     H*                DSPF Changes:                                  *
     H*                1.Changed Existing Group field into Typeahead  *
     H*                                                               *
     H* 04/17/02 C:62 PROJECT: MC099 32105                            *
     H*               INITIALIZE Q2RECF TO 'SCR01'                    *
     H*                                                               *
     H* 04/10/02 C:62 PROJECT: AR036    BASE ONLY                     *
     H*               CHANGE F24 TO F2.                               *
     H*                                                               *
     H* 01/22/02 C:62 PROJECT: AR033                                  *
     H*               MODIFY TO HANDLE NETWORK INVOICES.              *
     H*                                                               *
     H* 05/19/00 C:62 PROJECT: MC172                                  *
     H*               ADDED ADDITIONAL PARM TO CALL TO F3040R.        *
     H*                                                               *
     H* 04/25/00 C:62 PROJECT:AR001                                   *
     H*               -CHANGE SO THAT THIS PGM CAN BE CALL FROM WORK  *
     H*                WITH INVOICES.                                 *
     H*               -CHANGE OUT OLD SEARCHS WITH NEW SEARCHS        *
     H*                                                               *
     H* 10/07/99 C:62 PROJECT:MC212                                   *
     H*               ADD OPTION 17 TO ACCESS T/M ACTIVITY            *
     H*                                                              *
     H****************************************************************
     H*  FILES:                                                      *
     H*  FWEAP:  PBR/HBA REBILL WORK FILE (CO/SUB/FND/FYR/CASE/INV)  *
     H*          MEMBER CREATED IN F7126C                            *
     H*          RECORDS ADDED IN F7126C1 (CALLED FROM THIS RPG)     *
     H*  FNEOP:  NETWORK REBILL WORK FILE (CO/SUB/EOB#)              *
     H*          MEMBER CREATED IN F7126C                            *
     H*          RECORDS ADDED IN F7126C1 (CALLED FROM THIS RPG)     *
     H****************************************************************
     H*  DESCRIPTION OF THE CALCULATED REVIEWER FEE CHANGE:          *
     H*                                                              *
     H*    THERE ARE NOW TWO AMOUNT FIELDS:                          *
     H*       1) T/M AMOUNT - CONSISTS OF ONLY THE SUM OF ALL T/M    *
     H*                       ACTIVITY LINE ITEMS.                   *
     H*       2) REVIEWER FEE - CONSISTS OF ONLY THE AMOUNT CALC-    *
     H*                         ULATED FROM N6180R OR IN OTHER WORDS *
     H*                         THE TOTAL AMOUNT - SUM(T/M LINE ITEMS*
     H*                                                 AMOUNT).     *
     H****************************************************************
     FN7146F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F*** BEGIN ADD *** 04/25/00 **************************************
     F                                     INFDS(DBFDSA)
     FNMIMP     IF   E             DISK
     F*** END   ADD *** 04/25/00 **************************************
     FFMCDL     IF   E           K DISK
     FNMARL6    IF   E           K DISK
     F**********FAEOL8  IF  E           K        DISK
     F**********FAEOL7  IF  E           K        DISK
     F**********  PEO01                             KRENAMEPEO02
     F**********FAEOL3  IF  E           K        DISK
     F**********  PEO01                             KRENAMEPEO03
     F**********FAHBL   IF  E           K        DISK
     F**********NAPBL1  IF  E           K        DISK
     F**********NAMBL1  IF  E           K        DISK
     F**********NATRL   IF  E           K        DISK
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     FFMSMP     IF   E             DISK
     FFMFNL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     F                                     RENAME(PSM01:PSM02)
     FFWEAP     UF A E           K DISK
     F                                     RENAME(PEA01:PEW01)
     FFNEAP     UF A E           K DISK
     F                                     RENAME(PEA01:PEN01)
     F*
     F*** BEGIN ADD *** 10/07/99 **************************************
     FFAEOLG    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F*** END   ADD *** 10/07/99 **************************************
     F**********FAEOLG  IF  E           K        DISK
     F**********  PEO01                             KRENAMEPEO04
     FFAEAL10   IF   E           K DISK
     FFAEAL11   IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FFAEAL12   IF   E           K DISK
     F                                     RENAME(PEA01:PEA03)
     F*

     D*
     D*** BEGIN CHG *** 10/07/10 **************************************
     D**********          FL         12  1               FLAG
     D*** END   CHG *** 10/07/10 **************************************
     D*** BEGIN CHG *** 10/07/10 **************************************
     D**********          DS         12 30               DESCRIPTION
     D*** END   CHG *** 10/07/10 **************************************
     D*** BEGIN ADD *** 10/07/10 **************************************
     D*** END   ADD *** 10/07/10 **************************************
     D KEY             S              9  0 DIM(2)                               EOB KEY
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D*** BEGIN ADD *** 10/07/99 **************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*** END   ADD *** 10/07/99 **************************************
     D*** BEGIN ADD *** 04/25/00 **************************************
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D*** END   ADD *** 04/25/00 **************************************
     D*                                                                *********
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*** BEGIN CHG *** 10/07/10 **************************************
     D**********  DS
     D**********                              1  12 FL
     D**********                              1   1 SCFL01
     D**********                              2   2 SCFL02
     D**********                              3   3 SCFL03
     D**********                              4   4 SCFL04
     D**********                              5   5 SCFL05
     D**********                              6   6 SCFL06
     D**********                              7   7 SCFL07
     D**********                              8   8 SCFL08
     D**********                              9   9 SCFL09
     D**********                             10  10 SCFL10
     D**********                             11  11 SCFL11
     D**********                             12  12 SCFL12
     D                 DS
     D  FL                     1     24
     D                                     DIM(12)                              FLAG
     D  SCFL01                 1      2
     D  SCFL02                 3      4
     D  SCFL03                 5      6
     D  SCFL04                 7      8
     D  SCFL05                 9     10
     D  SCFL06                11     12
     D  SCFL07                13     14
     D  SCFL08                15     16
     D  SCFL09                17     18
     D  SCFL10                19     20
     D  SCFL11                21     22
     D  SCFL12                23     24
     D*** END   CHG *** 10/07/10 **************************************
     D                 DS
     D  EO                     1    108  0
     D                                     DIM(12)                              EOB #
     D  SCEO01                 1      9  0
     D  SCEO02                10     18  0
     D  SCEO03                19     27  0
     D  SCEO04                28     36  0
     D  SCEO05                37     45  0
     D  SCEO06                46     54  0
     D  SCEO07                55     63  0
     D  SCEO08                64     72  0
     D  SCEO09                73     81  0
     D  SCEO10                82     90  0
     D  SCEO11                91     99  0
     D  SCEO12               100    108  0
     D                 DS
     D  RC                     1     36  0
     D                                     DIM(12)                              RECON #
     D  SCRC01                 1      3  0
     D  SCRC02                 4      6  0
     D  SCRC03                 7      9  0
     D  SCRC04                10     12  0
     D  SCRC05                13     15  0
     D  SCRC06                16     18  0
     D  SCRC07                19     21  0
     D  SCRC08                22     24  0
     D  SCRC09                25     27  0
     D  SCRC10                28     30  0
     D  SCRC11                31     33  0
     D  SCRC12                34     36  0
     D                 DS
     D  SY                     1     12
     D                                     DIM(12)                              SYSTEM ID
     D  SCSY01                 1      1
     D  SCSY02                 2      2
     D  SCSY03                 3      3
     D  SCSY04                 4      4
     D  SCSY05                 5      5
     D  SCSY06                 6      6
     D  SCSY07                 7      7
     D  SCSY08                 8      8
     D  SCSY09                 9      9
     D  SCSY10                10     10
     D  SCSY11                11     11
     D  SCSY12                12     12
     D                 DS
     D  DT                     1    096  0
     D                                     DIM(12)                              TRANS DATE
     D  SCDT01               001    008  0
     D  SCDT02               009    016  0
     D  SCDT03               017    024  0
     D  SCDT04               025    032  0
     D  SCDT05               033    040  0
     D  SCDT06               041    048  0
     D  SCDT07               049    056  0
     D  SCDT08               057    064  0
     D  SCDT09               065    072  0
     D  SCDT10               073    080  0
     D  SCDT11               081    088  0
     D  SCDT12               089    096  0
     D*** BEGIN CHG *** 10/07/10 **************************************
     D**********  DS
     D**********                              1 360 DS
     D**********                              1  30 SCDS01
     D**********                             31  60 SCDS02
     D**********                             61  90 SCDS03
     D**********                             91 120 SCDS04
     D**********                            121 150 SCDS05
     D**********                            151 180 SCDS06
     D**********                            181 210 SCDS07
     D**********                            211 240 SCDS08
     D**********                            241 270 SCDS09
     D**********                            271 300 SCDS10
     D**********                            301 330 SCDS11
     D**********                            331 360 SCDS12
     D                 DS
     D  DS                     1    264
     D                                     DIM(12)                              DESCRIPTION
     D  SCDS01                 1     22
     D  SCDS02                23     44
     D  SCDS03                45     66
     D  SCDS04                67     88
     D  SCDS05                89    110
     D  SCDS06               111    132
     D  SCDS07               133    154
     D  SCDS08               155    176
     D  SCDS09               177    198
     D  SCDS10               199    220
     D  SCDS11               221    242
     D  SCDS12               243    264
     D*** END   CHG *** 10/07/10 **************************************
     D                 DS
     D  AM                     1    108  2
     D                                     DIM(12)                              AMOUNT
     D  SCAM01                 1      9  2
     D  SCAM02                10     18  2
     D  SCAM03                19     27  2
     D  SCAM04                28     36  2
     D  SCAM05                37     45  2
     D  SCAM06                46     54  2
     D  SCAM07                55     63  2
     D  SCAM08                64     72  2
     D  SCAM09                73     81  2
     D  SCAM10                82     90  2
     D  SCAM11                91     99  2
     D  SCAM12               100    108  2
     D*** BEGIN ADD *** 10/07/10 **************************************
     D                 DS
     D  TA                     1    108  2
     D                                     DIM(12)                              AMOUNT
     D  SCTA01                 1      9  2
     D  SCTA02                10     18  2
     D  SCTA03                19     27  2
     D  SCTA04                28     36  2
     D  SCTA05                37     45  2
     D  SCTA06                46     54  2
     D  SCTA07                55     63  2
     D  SCTA08                64     72  2
     D  SCTA09                73     81  2
     D  SCTA10                82     90  2
     D  SCTA11                91     99  2
     D  SCTA12               100    108  2
     D                 DS
     D  TAH                    1    108  2
     D                                     DIM(12)                              AMOUNT
     D  XHTA01                 1      9  2
     D  XHTA02                10     18  2
     D  XHTA03                19     27  2
     D  XHTA04                28     36  2
     D  XHTA05                37     45  2
     D  XHTA06                46     54  2
     D  XHTA07                55     63  2
     D  XHTA08                64     72  2
     D  XHTA09                73     81  2
     D  XHTA10                82     90  2
     D  XHTA11                91     99  2
     D  XHTA12               100    108  2
     D                 DS
     D  TM                     1     12
     D                                     DIM(12)                              T/M FLAG
     D  SCTM01                 1      1
     D  SCTM02                 2      2
     D  SCTM03                 3      3
     D  SCTM04                 4      4
     D  SCTM05                 5      5
     D  SCTM06                 6      6
     D  SCTM07                 7      7
     D  SCTM08                 8      8
     D  SCTM09                 9      9
     D  SCTM10                10     10
     D  SCTM11                11     11
     D  SCTM12                12     12
     D                 DS
     D  RN                     1    108  0
     D                                     DIM(12)                              FAEOP RRN
     D  SCRR01               001    009  0
     D  SCRR02               010    018  0
     D  SCRR03               019    027  0
     D  SCRR04               028    036  0
     D  SCRR05               037    045  0
     D  SCRR06               046    054  0
     D  SCRR07               055    063  0
     D  SCRR08               064    072  0
     D  SCRR09               073    081  0
     D  SCRR10               082    090  0
     D  SCRR11               091    099  0
     D  SCRR12               100    108  0
     D                 DS
     D  CT                     1    180
     D                                     DIM(12)                              CERT #
     D  SCCT01               001    015
     D  SCCT02               016    030
     D  SCCT03               031    045
     D  SCCT04               046    060
     D  SCCT05               061    075
     D  SCCT06               076    090
     D  SCCT07               091    105
     D  SCCT08               106    120
     D  SCCT09               121    135
     D  SCCT10               136    150
     D  SCCT11               151    165
     D  SCCT12               166    180
     D                 DS
     D  EX                     1     36  0
     D                                     DIM(12)                              CERT EXT
     D  SCEX01               001    003  0
     D  SCEX02               004    006  0
     D  SCEX03               007    009  0
     D  SCEX04               010    012  0
     D  SCEX05               013    015  0
     D  SCEX06               016    018  0
     D  SCEX07               019    021  0
     D  SCEX08               022    024  0
     D  SCEX09               025    027  0
     D  SCEX10               028    030  0
     D  SCEX11               031    033  0
     D  SCEX12               034    036  0
     D*** END   ADD *** 10/07/10 **************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*                                                                *********
     IPEW01
     I              EACHIN                      EWCHIN
     IPEN01
     I              EACHIN                      ENCHIN

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    KKCO#
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*
     C* Key List for the FUND Master File
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C* Key List for the SUB# Master File
     C     SMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    XXSUB#
     C*
     C*
     C*   KEY LIST FOR THE CASE MASTER FILE (FMCML1 - PCM01)
     C*
     C     CMKEY1        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C*** BEGIN ADD *** 04/25/00 **************************************
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*** END   ADD *** 04/25/00 **************************************
     C*
     C*   KEY LIST FOR THE EOB MASTER FILE
     C*
     C     EOKEY1        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCINV#
     C*
     C     EOKEY2        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCINV#
     C                   KFLD                    XXEOB#
     C*
     C     EOKEY3        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    AREOB#
     C     EOKEY4        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCINV#
     C                   KFLD                    XXEOB#
     C*
     C     EWKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCINV#
     C     EWKEY2        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCINV#
     C*
     C     EWKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCINV#
     C                   KFLD                    XXEOB#
     C*
     C     ENKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCINV#
     C                   KFLD                    XXEOB#
     C**
     C     ENKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCINV#
     C**********
     C********** TRKEY     KLIST
     C**********           KFLD           EOCO#
     C**********           KFLD           EOSUB#
     C**********           KFLD           EOFND
     C**********           KFLD           EOFYR
     C**********           KFLD           EOCASE
     C**********           KFLD           EOEOB#
     C**********
     C********** HBKEY     KLIST
     C**********           KFLD           KKCO#
     C**********           KFLD           EOSUB#
     C**********           KFLD           EOFND
     C**********           KFLD           EOFYR
     C**********           KFLD           EOCASE
     C**********           KFLD           EOEOB#
     C**********
     C**PRE-CERT BILLING FILE
     C**********
     C********** PBKEY     KLIST
     C**********           KFLD           KKCO#
     C**********           KFLD           EOSUB#
     C**********           KFLD           EOFND
     C**********           KFLD           EOFYR
     C**********           KFLD           EOCASE
     C**********           KFLD           EOEOB#
     C**********
     C**MED*MGMT BILLING FILE
     C**********
     C********** MBKEY     KLIST
     C**********           KFLD           KKCO#
     C**********           KFLD           EOSUB#
     C**********           KFLD           EOFND
     C**********           KFLD           EOFYR
     C**********           KFLD           EOCASE
     C**********           KFLD           EOEOB#
     C*
     C     EAKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    SCINV#
     C*
     C     EAKEY2        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCINV#
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXRCN
     C*
     C     EAKEY3        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    AREOB#
     C                   KFLD                    ARRCN
     C*
     C     EOKEY5        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    XXEOB#
     C*** BEGIN ADD *** 10/07/99 **************************************
     C*
     C     EOKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*** END   ADD *** 10/07/99 **************************************
     C*
     C*   KEY LIST FOR THE A/R MASTER FILE
     C*
     C     ARKEY1        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCINV#
     C*
     C     ARKEY2        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCINV#
     C                   KFLD                    XXEOB#
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMOGRAPHIC FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   PARAMETER LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO              3
     C                   PARM                    QQSUB             5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQINV#            9
     C                   PARM                    QQDATE            8
     C                   PARM                    QQTYPE            1             SYSTEM TYPE
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQMBR            10
     C*** BEGIN ADD *** 04/25/00 **************************************
     C                   PARM                    @1CO              3 0
     C                   PARM                    @1SUB             5 0
     C                   PARM                    @1FND             3 0
     C                   PARM                    @1FYR             3 0
     C                   PARM                    @1CASE            7 0
     C                   PARM                    @1INV#            9 0
     C*
     C*** END   ADD *** 04/25/00 **************************************
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
     C*  INITIALIZATION SECTION
     C     *LIKE         DEFINE    EAEOB#        XXEOB#
     C*
     C*  HOLDS SUM OF EOMB# FOR THE SAME CASE -- DISPLAYING 0 IF
     C*  AMOUNT HAS BEEN CLEARED.
     C*
     C     *LIKE         DEFINE    ARAMT         XXAMT
     C     *LIKE         DEFINE    UPCO#         KKCO#
     C     *LIKE         DEFINE    EARCN         XXRCN
     C*** BEGIN ADD *** 10/07/99 **************************************
     C     *LIKE         DEFINE    EANAMT        XXNAMT
     C*** END   ADD *** 10/07/99 **************************************
     C*
     C*
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
     C*   LOAD THE OUTPUT CONSTANTS FROM THE PARMS
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*** INITIALIZE FIELDS
     C*
     C                   EXSR      INIT
     C*
     C*   GET THE USER PARAMETER FILE
     C*   GET THE BASE USER DEFAULT VALUES
     C*
     C     TOP           TAG
     C*
     C*
     C     QQCASE        IFGT      *BLANKS
     C                   MOVE      QQFND         SCFND
     C                   MOVE      QQCO          KKCO#
     C                   MOVE      QQSUB         SCSUB#
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQCASE        SCCASE
     C                   MOVE      QQINV#        SCINV#
     C                   ELSE
     C*** BEGIN ADD *** 04/25/00 **************************************
     C     Q1PRMS        IFGT      12
     C*
     C                   Z-ADD     @1CO          KKCO#
     C                   Z-ADD     @1SUB         SCSUB#
     C                   Z-ADD     @1FND         SCFND
     C                   Z-ADD     @1FYR         SCFYR
     C                   Z-ADD     @1CASE        SCCASE
     C                   Z-ADD     @1INV#        SCINV#
     C*
     C                   ELSE
     C*** END   ADD *** 04/25/00 **************************************
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *BLANK        SCFND
     C                   MOVE      *ZERO         UPSUB#
     C                   END
     C                   MOVE      UPCO#         KKCO#
     C                   MOVE      UPFND         SCFND
     C                   Z-ADD     UPSUB#        SCSUB#
     C*** BEGIN ADD *** 04/25/00 **************************************
     C                   ENDIF
     C*** END   ADD *** 04/25/00 **************************************
     C                   ENDIF
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*
     C                   MOVEL(P)  SCPRDS        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*** BEGIN ADD *** 04/25/00 **************************************
     C     Q1PRMS        IFGT      12
     C*
     C*** BEGIN ADD *** 04/17/02 **************************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*** END   ADD *** 04/17/02 **************************************
     C                   GOTO      SCR01T
     C*
     C                   ENDIF
     C*** END   ADD *** 04/25/00 **************************************
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      KKCO#         @@CO#             3
     C                   MOVE      SCSUB#        @@SUB#            5
     C                   MOVE      SCFND         @@FND             3
     C                   MOVE      SCFYR         @@FYR             3
     C                   MOVE      SCCASE        @@CASE            7
     C*
     C                   CALL      'FA702R'
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@L1             78
     C                   PARM                    @@L2             78
     C*
     C                   MOVE      @@L1          SCL1
     C                   MOVE      @@L2          SCL2
     C*
     C     XXUP          IFEQ      '1'
     C                   MOVE      *BLANKS       SCUP
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   END
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   END
     C                   EXFMT     SCR02
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C**
     C**
     C****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C**********************************
     C*   COMMAND 03 = END OF JOB      *
     C**********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   MOVE      ' '           QQRTC
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC
     C*
     C************************
     C*   F10 = SEARCH       *
     C************************
     C*
     C*
      **** BEGIN ADD **** 04/20/98 ************************************
     C*
     C     *INKJ         IFEQ      *ON
     C     Q2RECF        IFEQ      'SCR01'
     C*
      *********************
      * SUBSCRIBER SEARCH *
      *********************
     C*
     C     HFLD          IFEQ      'SCSUB#'
     C*
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     30     PC AT SUB#
     C*
     C     ##RRN         CHAIN     PSM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVE      *ON           *IN31                          PC AT SCFND
     C                   ELSE
     C                   MOVE      *ON           *IN30                          PC AT SCSUB#
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCSUB#
     C*
      ****************
      * GROUP SEARCH *
      ****************
     C******** Beg Del ***** 12/18/24 ***************************
     C     HFLD          IFEQ      'SCFND'
     C*****
     C*****              MOVE      ##RRN         ##RRN2
     C*****
     C*****              CALL      'F0060R'
     C*****              PARM      '2'           ##CALL            1
     C******INKC         PARM                    ##KC              1
     C******INKL         PARM                    ##KL              1
     C*****              PARM                    ##RRN2            9 0
     C*****
     C******INKC         CABEQ     *ON           TERM
     C******INKL         CABEQ     *ON           STRT                     31     PC AT SCFND
     C*****
     C*****##RRN2        CHAIN     PFN02                              60
     C*****
     C******IN60         IFEQ      *OFF
     C*****              MOVE      FNFND         SCFND
     C*****              MOVE      *ON           *IN29                          PC AT SCFYR
     C*****              ELSE
     C*****              MOVE      *ON           *IN31
     C*****              ENDIF                                                  *IN60 IFEQ
     C*****
     C                   GOTO      STRT
     C
     C                   ENDIF                                                  SCFND
     C******** End Del ***** 12/18/24 ***************************
     C*
      **********************
      * YEAR & CASE SEARCH *
      **********************
     C     HFLD          IFEQ      'SCFYR'
     C     HFLD          OREQ      'SCCASE'
     C*
     C                   MOVE      SCSUB#        Q1SUB#
     C                   MOVE      SCFND         Q1FND
     C*
     C                   CALL      'F0269R'
     C                   PARM      '2'           Q1CALL            1
     C                   PARM                    Q1SUB#            5 0
     C                   PARM                    Q1FND             3 0
     C                   PARM      0             Q1FYR             3 0
     C                   PARM      0             Q1CASE            7 0
     C                   PARM                    Q1SS#             9
     C                   PARM                    Q1ACDT            8 0
     C                   PARM                    QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     Q1CASE        OREQ      *ZERO
     C     HFLD          CABEQ     'SCFYR'       STRT                     29      PC AT SCFYR
     C     HFLD          CABEQ     'SCCASE'      STRT                     59      PC AT SCCASE
     C                   ENDIF                                                  QQRTC EQ B
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      Q1SUB#        SCSUB#
     C                   MOVE      Q1FND         SCFND
     C                   MOVE      Q1FYR         SCFYR
     C                   MOVE      Q1CASE        SCCASE
     C                   MOVE      *ON           *IN32                          PC AT SCINV#
     C                   GOTO      STRT
     C                   ENDIF                                                  QQRTC EQ *
     C*
     C                   ENDIF                                                  SCFYR SCCASE
     C*
     C*
      ********************
      * INVOICE # SEARCH *
      ********************
     C*
     C     HFLD          IFEQ      'SCINV#'
     C*
     C*** BEGIN CHG *** 04/25/00 **************************************
     C**********           MOVE SCFND     QQFND   3
     C**********           MOVE SCFYR     QQFYR   3
     C**********           MOVE SCCASE    QQCASE  7
     C**********           MOVE SCINV#    QQINV#  9
     C**********           CALL 'N0720R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQFND
     C**********           PARM           QQFYR
     C**********           PARM           QQCASE
     C**********           PARM           QQINV#
     C**********           PARM           QQRTC   1
     C*
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTC     IFEQ 'B'
     C********** SCFND     OREQ *ZEROS
     C**********           MOVE '1'       *IN32            PC AT SCINV#
     C**********           GOTO STRT
     C**********           ENDIF                           QQRTC EQ B
     C*
     C********** QQRTC     IFEQ '*'
     C**********           MOVE QQFND     SCFND
     C**********           MOVE QQFYR     SCFYR
     C**********           MOVE QQCASE    SCCASE
     C**********           MOVE QQINV#    SCINV#
     C**********           GOTO STRT
     C**********           ENDIF                           QQRTC EQ *
     C*
     C     SCCASE        IFNE      *ZEROS
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'N0420R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0          NMIMP
     C                   PARM      DBRRNA        ##RRN2            9 0          FMCMP
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        32
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PIM01                              99
     C*
     C                   Z-ADD     IMSUB#        SCSUB#
     C                   Z-ADD     IMFND         SCFND
     C                   Z-ADD     IMFYR         SCFYR
     C                   Z-ADD     IMCASE        SCCASE
     C                   Z-ADD     IMINV#        SCINV#
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   SETON                                        20
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   CALL      'N0419R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0          NMIMP
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        32
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PIM01                              99
     C*
     C                   Z-ADD     IMSUB#        SCSUB#
     C                   Z-ADD     IMFND         SCFND
     C                   Z-ADD     IMFYR         SCFYR
     C                   Z-ADD     IMCASE        SCCASE
     C                   Z-ADD     IMINV#        SCINV#
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C*** END   CHG *** 04/25/00 **************************************
     C                   ENDIF                                                  HFLD SCINV#
     C*
     C                   ENDIF                                                  SCR01
     C                   ENDIF                                                  *INKJ
     C*
     C**********************************
     C*   COMMAND 12 = PRIOR SCREEN    *
     C**********************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C*** BEGIN ADD *** 04/25/00 **************************************
     C     Q1PRMS        IFLE      12
     C*** END   ADD *** 04/25/00 **************************************
     C                   Z-ADD     *ZEROS        XXEOB#
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*** BEGIN ADD *** 04/25/00 **************************************
     C                   ELSE
     C*
     C                   GOTO      TERM
     C*
     C                   ENDIF
     C*** END   ADD *** 04/25/00 **************************************
     C                   ENDIF                                                  SCR02
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   Z-ADD     *ZEROS        XXEOB#
     C*** BEGIN DEL *** 01/22/02 **************************************
     C********** ARSYS     IFEQ 'N'
     C**********           MOVEL'SCR02'   Q2RECF
     C**********           ELSE
     C*** END   DEL *** 01/22/02 **************************************
     C                   MOVEL     'SCR02'       Q2RECF
     C*** BEGIN DEL *** 01/22/02 **************************************
     C**********           ENDIF                           ARSYS = N
     C*** END   DEL *** 01/22/02 **************************************
     C*
     C                   GOTO      STRT
     C                   ENDIF                                                  SCR03
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C                   Z-ADD     *ZEROS        XXEOB#
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  SCR04
     C                   ENDIF                                                  *INKL
     C*
     C**********************************
     C*   ROLL UP REQUEST              *
     C**********************************
     C     *IN18         CABEQ     '1'           SCR02T
     C**********************************
     C*   ROLL DOWN REQUEST            *
     C**********************************
     C     *IN19         CABEQ     '1'           SCR02T
     C**********************************
     C*   PROCESS HELP REQUESTS        *
     C**********************************
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDEQ *IN10
     C*
     C**********************************
     C*   COMMAND 2  SUBMIT RE-BILL   *
     C**********************************
     C*** BEGIN CHG *** 04/10/02 **************************************
     C********** *INKY     IFEQ '1'
     C     *INKB         IFEQ      *ON
     C*** END   CHG *** 04/10/02 **************************************
     C*
     C     ARSYS         IFEQ      'N'
     C                   EXSR      RITEN
     C                   ELSE
     C                   EXSR      RITE
     C                   END                                                    ARSYS EQ N
     C*
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDEQ *INKY
     C*
     C                   END                                                    END CMD KEYS
     C*****************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C******************************************
     C*   PROCESS REQUEST SCREEN - SCREEN 01   *
     C******************************************
     C     SCR01T        TAG
     C*
     C* Validate the SUB#, if entered.
     C*
     C                   Z-ADD     SCSUB#        XXSUB#            5 0
     C     SMKY          CHAIN     PSM02                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *ON           *IN77
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60 IFEQ 1
     C*
     C* Validate the FUND, if entered.
     C*
     C                   Z-ADD     SCFND         XXFND             3 0
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *ON           *IN78
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60 IFEQ 1
     C*
     C*
     C***  VALIDATE CASE NUMBER ENTERED
     C*
     C                   MOVE      SCFYR         XXFYR             3 0
     C     CMKEY1        CHAIN     PCM01                              20
     C     *IN20         CABEQ     '1'           STRT                           INVALID CASE
     C*
     C***  VALIDATE INVOICE # ENTERED
     C*
     C     ARKEY1        CHAIN     PAR01                              21
     C     *IN21         IFEQ      '1'                                          INVALID INV#
     C                   GOTO      STRT
     C                   ELSE
     C                   Z-ADD     ARTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDATE
     C                   END
     C**
     C*
     C* * * *  LAST,FIRST NAME SUBPROGRAM FOR CLAIMANT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANK        SRLNAM           56
     C                   MOVEL     CDLSN         SRLNAM
     C                   MOVEL     CDFSN         SRFNAM           15
     C                   MOVEL     ' '           SRTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SRTERM
     C                   MOVEL     SRLNAM        SCCLNM
     C                   ELSE
     C                   MOVE      *BLANKS       SCCLNM
     C                   END                                                    ENDIF *IN60
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVE      *BLANK        KEY
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVE      *ZEROS        SCEOB#
     C     ARKEY2        SETLL     PAR01                                        INV/EOB
     C     ARKEY1        READE     PAR01                                  60    INV
     C     ARSYS         IFNE      'N'
     C                   EXSR      LOAD
     C                   ELSE
     C                   MOVE      'N'           QQTYPE
     C                   EXSR      LOADN
     C                   END
     C**
     C*
     C                   MOVE      *BLANKS       QQCO
     C                   MOVE      *BLANKS       QQSUB
     C                   MOVE      *BLANKS       QQFND
     C                   MOVE      *BLANKS       QQFYR
     C                   MOVE      *BLANKS       QQCASE
     C                   MOVE      *BLANKS       QQINV#
     C                   MOVE      *BLANKS       QQTYPE
     C*
     C                   MOVE      KKCO#         QQCO
     C                   MOVE      SCSUB#        QQSUB
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      SCINV#        QQINV#
     C*
     C**
     C**
     C*** BEGIN DEL *** 01/22/02 **************************************
     C********** ARSYS     IFEQ 'N'
     C**********           MOVEL'SCR02'   Q2RECF
     C**
     C**********           ELSE
     C*** END   DEL *** 01/22/02 **************************************
     C                   MOVEL     'SCR02'       Q2RECF
     C*** BEGIN DEL *** 01/22/02 **************************************
     C**********           END
     C*** END   DEL *** 01/22/02 **************************************
     C**
     C**
     C                   GOTO      STRT
     C*
     C*****************************************************************
     C*   PROCESS EOB DETAIL SCREEN - SCREEN 02 -- USED FOR PBR/HBA'S *
     C*****************************************************************
     C     SCR02T        TAG
     C*
     C*  INVALID ROLL REQUEST
     C*
     C   18XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C     ARSYS         IFNE      'N'
     C                   EXSR      RITE
     C                   ELSE
     C                   EXSR      RITEN
     C                   END
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVE      KEY(2)        XXEOB#
     C     ARKEY2        CHAIN     PAR01                              60        INV/EOB
     C     ARSYS         IFNE      'N'
     C                   EXSR      LOAD
     C                   ELSE
     C                   EXSR      LOADN
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C     ARSYS         IFNE      'N'
     C                   EXSR      RITE
     C                   ELSE
     C                   EXSR      RITEN
     C                   END
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      KEY(1)        XXEOB#
     C     ARKEY2        CHAIN     PAR01                              60        INV/EOB
     C     ARSYS         IFNE      'N'
     C                   EXSR      LOADP
     C                   ELSE
     C                   EXSR      LOADNP
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCEOB#        IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVE      SCEOB#        XXEOB#
     C                   MOVE      *BLANK        SCEOB#
     C     ARKEY2        SETLL     PAR01                                        INV/EOB
     C     ARKEY1        READE     PAR01                                  60    INV
     C     ARSYS         IFNE      'N'
     C                   EXSR      LOAD
     C                   ELSE
     C                   EXSR      LOADN
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C     ARSYS         IFNE      'N'
     C                   EXSR      RITE
     C                   ELSE
     C                   EXSR      RITEN
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C**************************************************
     C* PROCESS SUBMIT REBILL SCREEN - SCREEN 03       *
     C**************************************************
     C     SCR03T        TAG
     C*
     C*** VALIDATE INVOICE DATE
     C*
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C*
     C     SRERR         CABEQ     '1'           STRT                     20    INVALID DATE
     C*
     C                   MOVE      KKCO#         QQCO#             3
     C                   MOVE      SCSUB#        QQSUB#            5
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCCASE        QQCASE            7
     C                   MOVE      SCINV#        QQINV#            9
     C                   MOVE      SCDATE        QQDATE
     C                   MOVE      '*'           QQRTCD            1
     C*
     C     ARSYS         IFEQ      'N'
     C                   MOVE      'N'           QQTYPE
     C                   END
     C*
     C*
     C*** ... AFTER RE-BILL SUBMITTED, RETURN TO MENU
     C*
     C                   GOTO      TERM
     C*
     C**
     C***********************
     C*  END THE PROGRAM    *
     C***********************
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C**************************************************
     C*  RITEN - UPDATE WORK FILE WITH SCIFEE          *
     C**************************************************
     C     RITEN         BEGSR
     C*
     C* VALIDATE OPTION 4 OR BLANKS
     C*
     C                   DO        12            Y                 5 0
     C     Y             ADD       44            Y1                2 0
     C     FL(Y)         IFNE      *BLANK
     C     FL(Y)         ANDNE     '4'
     C*** BEGIN ADD *** 10/07/99 **************************************
     C     FL(Y)         ANDNE     '17'
     C*** END   ADD *** 10/07/99 **************************************
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*** UPDATE WORK FILE WITH REVIEWER AMT
     C*
     C                   DO        12            V                 2 0
     C     EO(V)         IFGT      *ZERO
     C                   Z-ADD     EO(V)         XXEOB#
     C     ENKY1         CHAIN     PEN01                              70        INV/EOB
     C     *IN70         IFEQ      '0'
     C     EO(V)         ANDEQ     EAEOB#
     C     RC(V)         ANDEQ     EARCN
     C**********           Z-ADDAM,V      EANAMT
     C                   Z-ADD     AM(V)         EANCAM
     C*** BEGIN CHG *** 01/22/02 **************************************
     C********** TA,V      ADD  AM,V      EANAMT
     C                   Z-ADD     AM(V)         EANAMT
     C*** END   CHG *** 01/22/02 **************************************
     C*
     C     TA(V)         IFNE      TAH(V)
     C*
     C                   MOVEL     'Y'           ENCHIN
     C*
     C                   ELSE
     C*
     C                   MOVEL     ' '           ENCHIN
     C*
     C                   ENDIF
     C     FL(V)         IFEQ      '4'
     C                   MOVE      'Y'           EANDIF
     C                   ELSE
     C                   MOVE      *BLANK        EANDIF
     C                   ENDIF
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   Z-ADD     DSTIME        EATIME
     C                   UPDATE    PEN01
     C                   END                                                    ENDEQ *IN70
     C                   END                                                    ENDGT *ZERO
     C                   END                                                    END DO
     C                   ENDSR
     C**************************************************
     C*  INITIALIZE ARRAYS AND SCREEN 1 FIELDS         *
     C**************************************************
     CSR   INIT          BEGSR
     C                   Z-ADD     *ZEROS        SCSUB#
     C                   MOVE      *BLANK        SCFND
     C                   MOVE      *ZERO         SCFYR
     C                   MOVE      *ZERO         SCCASE
     C                   MOVE      *ZERO         SCINV#
     CSR                 ENDSR
     C**************************************************
     C*  CLEAR SCREEN ARRAYS - CLR                     *
     C**************************************************
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       FL
     C                   Z-ADD     *ZEROS        EO
     C                   Z-ADD     *ZEROS        RC
     C                   MOVE      *BLANKS       SY
     C                   Z-ADD     *ZERO         DT
     C                   MOVE      *BLANKS       DS
     C                   Z-ADD     *ZERO         AM
     C                   Z-ADD     *ZERO         I1                3 0
     C                   Z-ADD     *ZERO         Z1                3 0
     C*** BEGIN ADD *** 10/07/99 **************************************
     C                   Z-ADD     *ZERO         RN
     C                   MOVE      *BLANKS       TM
     C                   Z-ADD     *ZERO         EX
     C                   Z-ADD     *ZERO         TA
     C                   Z-ADD     *ZERO         TAH
     C                   MOVE      *BLANKS       CT
     C*** END   ADD *** 10/07/99 **************************************
     C                   ENDSR
     C**************************************************
     C*  FILL SCREEN ARRAYS - FILL                     *
     C**************************************************
     C     FILL          BEGSR
     C                   MOVE      EAEOB#        EO(I1)
     C                   MOVE      EARCN         RC(I1)
     C                   MOVE      EASYS         SY(I1)
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C                   MOVEL     ARDESC        DS(I1)
     C*** BEGIN ADD *** 10/07/99 **************************************
     C     EOKY          CHAIN     PEO01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     DBRRN         RN(I1)
     C                   MOVEL     EOCERT        CT(I1)
     C                   Z-ADD     EOEXT         EX(I1)
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        RN(I1)
     C                   MOVEL     *BLANKS       CT(I1)
     C                   Z-ADD     *ZEROS        EX(I1)
     C*
     C                   ENDIF
     C*
     C     EATMFL        IFEQ      'Y'
     C*
     C                   MOVEL     '*'           TM(I1)
     C*
     C                   ELSE
     C*
     C                   MOVEL     ' '           TM(I1)
     C*
     C                   ENDIF
     C*** END   ADD *** 10/07/99 **************************************
     C*
     C*** BEGIN DEL *** 01/22/02 **************************************
     C********** EASYS     IFEQ 'P'
     C********** EASYS     OREQ 'H'
     C********** EASYS     OREQ 'U'
     C********** EASYS     OREQ 'M'
     C*** END   DEL *** 01/22/02 **************************************
     C*** BEGIN CHG *** 10/07/99 **************************************
     C**********           MOVE EABAMT    AM,I1
     C                   Z-ADD     EABCAM        AM(I1)
     C     EABAMT        SUB       EABCAM        TA(I1)
     C     ARAMT         SUB       EABCAM        TAH(I1)
     C*** END   CHG *** 10/07/99 **************************************
     C*** BEGIN DEL *** 01/22/02 **************************************
     C**********           ELSE
     C********** EASYS     IFEQ 'N'
     C*** BEGIN CHG *** 10/07/99 **************************************
     C**********           MOVE EANAMT    AM,I1
     C**********           Z-ADDEANCAM    AM,I1
     C********** EANAMT    SUB  EANCAM    TA,I1
     C********** ARAMT     SUB  EANCAM    TAH,I1
     C*** END   CHG *** 10/07/99 **************************************
     C**********           ENDIF
     C**********           ENDIF
     C*** END   DEL *** 01/22/02 **************************************
     C*
     C     EWKY1         CHAIN     PEW01                              60
     C     *IN60         IFEQ      '0'
     C     EABDIF        IFEQ      'Y'
     C                   MOVE      4             XXBDIF            1
     C*
     C     XXBDIF        IFNE      FL(I1)
     C                   MOVEL     XXBDIF        FL(I1)
     C                   END
     C*** BEGIN DEL *** 01/22/02 **************************************
     C********** EASYS     IFEQ 'P'
     C*** END   DEL *** 01/22/02 **************************************
     C     EABAMT        IFNE      AM(I1)
     C*** BEGIN CHG *** 10/07/99 **************************************
     C**********           Z-ADDEABAMT    AM,I1
     C                   Z-ADD     EABCAM        AM(I1)
     C     EABAMT        SUB       EABCAM        TA(I1)
     C     ARAMT         SUB       EABCAM        TAH(I1)
     C*** END   CHG *** 10/07/99 **************************************
     C                   END
     C*** BEGIN DEL *** 01/22/02 **************************************
     C*** BEGIN DEL *** 01/22/02 **************************************
     C**********           ELSE
     C********** EABAMT    IFNE *ZEROS
     C********** EABAMT    ANDNEAM,I1
     C*** BEGIN CHG *** 10/07/99 **************************************
     C**********           Z-ADDEABAMT    AM,I1
     C**********           Z-ADDEANCAM    AM,I1
     C********** EANAMT    SUB  EANCAM    TA,I1
     C********** ARAMT     SUB  EANCAM    TAH,I1
     C*** END   CHG *** 10/07/99 **************************************
     C**********           END
     C**********           END
     C*** END   DEL *** 01/22/02 **************************************
     C                   END
     C                   ENDIF
     C*
     C                   MOVE      '1'           *IN(Z1)                        COLM SEPARATORS
     C                   ENDSR
     C**************************************************
     C*  LOAD FORWARD - LOAD                           *
     C**************************************************
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C     OVER          TAG
     C*
     C*  END OF FILE
     C*
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C*****************************************************************
     C*  This edit will add up the offset and deletes for an eomb
     C*  returning a value of zero to be displayed.  If user clears
     C*  amount without using option 4 to delete EOMB the record will
     C*  stay in the file and will not allow the user to delete the
     C*  EOMB for EOMB delete.  Here we are forcing the user to see
     C*  even though they cleared out the field, it is not deleted.
     C*  Option '4' to delete is required.
     C*
     C*  Without this edit, the program will not display the deleted
     C*  EOMB#.  It displays the line once for 'O' offset,'D' delete
     C*  instead of a line for each item.
     C*
     C                   Z-ADD     *ZEROS        XXAMT
     C     *IN87         DOUEQ     '1'
     C*
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDNE     'B'
     C                   ADD       ARAMT         XXAMT
     C                   END
     C                   Z-ADD     AREOB#        XXEOB#
     C                   Z-ADD     ARRCN         XXRCN
     C*
     C     ARSTAT        IFEQ      'B'
     C                   LEAVE
     C                   ENDIF
     C     ARKEY2        READE     PAR01                                  87
     C                   ENDDO
     C*
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDEQ     'B'
     C*
     C* This will allow the EOMBs that have been cleared out by the
     C* user.
     C     ARTRTP        OREQ      'I'
     C     ARSTAT        ANDEQ     'D'
     C                   Z-ADD     AREOB#        XXEOB#
     C     SCCASE        IFEQ      *ZEROS
     C*
     C     EAKEY2        CHAIN     PEA02                              65
     C                   ELSE
     C     EAKEY         CHAIN     PEA01                              65
     C*
     C                   ENDIF
     C     *IN65         IFEQ      '0'
     C*****************************************************************
     C*  This allows us to put in the accumulated total if it was
     C*  adjusted.  After it offsets the original EOMB it creates a
     C*  delete record.  We will display the delete record with the
     C*  sum of the two records ('O' + 'D') = $0
     C*
     C*          ARSTAT    IFEQ 'D'
     C*                    Z-ADDXXAMT     EABAMT
     C*                    ELSE
     C*                    Z-ADDARAMT     EABAMT           ADD 07/14/93
     C*                    ENDIF
     C*
     C                   MOVE      *BLANKS       EWCHIN                         ADD 03/20/96
     C*
     C  N16              WRITE     PEW01                                        ADD 07/14/93
     C*
     C                   ADD       1             I1
     C     I1            ADD       30            Z1
     C                   EXSR      FILL
     C                   END                                                    ENDEQ *IN65
     C                   END                                                    ENDEQ 'I'
     C*
     C     ARKEY2        SETGT     PAR01                                        ADD 06/12/97
     C     ARKEY1        READE     PAR01                                  60    INV
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      12                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVE      EO(1)         KEY(1)
     C                   MOVE      EO(12)        KEY(2)
     C                   ENDSR
     C**************************************************
     C*  LOAD BACKWARD - LOADP                         *
     C**************************************************
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     12            I1
     C     OVERP         TAG
     C*
     C*  BEGINNING OF FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADB
     C                   GOTO      DONEP
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C*****************************************************************
     C*  IF STATIS IS EQUAL TO 'O', AMOUNT HAS BEEN PREVIOUSLY REMOVED
     C*  BUT THE EOMB HAS NOT BEEN DELETED. INV# STILL CONTAINS THE
     C*  INVOICE NUMBER.
     C*
     C     *IN87         DOUEQ     '1'
     C*
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDNE     'B'
     C                   ADD       ARAMT         EABAMT
     C                   END
     C                   Z-ADD     AREOB#        XXEOB#
     C*
     C     ARSTAT        IFEQ      'B'
     C                   LEAVE
     C                   ENDIF
     C     ARKEY2        READE     PAR01                                  87
     C                   ENDDO
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDEQ     'B'
     C     ARTRTP        OREQ      'I'                                          ADD 06/12/97
     C     ARSTAT        ANDEQ     'D'                                          ADD 06/12/97
     C                   Z-ADD     AREOB#        XXEOB#
     C     SCCASE        IFEQ      *ZEROS
     C*
     C     EAKEY2        CHAIN     PEA02                              65
     C*
     C                   ELSE
     C*
     C     EAKEY         CHAIN     PEA01                              65
     C*
     C                   ENDIF
     C     *IN65         IFEQ      '0'
     C                   SUB       1             I1
     C     I1            ADD       30            Z1
     C                   EXSR      FILL
     C                   END                                                    ENDEQ *IN65
     C                   END                                                    ENDEQ 'I'
     C*
     C                   READP     PAR01                                  60
     C  N60ARCO#         COMP      KKCO#                              6060
     C  N60ARSUB#        COMP      SCSUB#                             6060
     C  N60ARFND         COMP      SCFND                              6060
     C  N60ARFYR         COMP      XXFYR                              6060
     C  N60ARCASE        COMP      SCCASE                             6060
     C  N60ARINV#        COMP      SCINV#                             6060      ADD 07/06/93
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVE      EO(1)         KEY(1)
     C                   MOVE      EO(12)        KEY(2)
     C                   ENDSR
     C**************************************************
     C*  LOAD FROM BEGINNING OF FILE  - LOADB          *
     C**************************************************
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANKS       XXEOB#
     C     ARKEY2        SETLL     PAR01                                        INV/EOB
     C     ARKEY1        READE     PAR01                                  60    INV
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C*
     C*****************************************************************
     C*  IF STATIS IS EQUAL TO 'O', AMOUNT HAS BEEN PREVIOUSLY REMOVED
     C*  BUT THE EOMB HAS NOT BEEN DELETED. INV# STILL CONTAINS THE
     C*  INVOICE NUMBER.
     C*
     C     *IN87         DOUEQ     '1'
     C*
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDNE     'B'
     C                   ADD       ARAMT         EABAMT
     C                   END
     C                   Z-ADD     AREOB#        XXEOB#
     C*
     C     ARSTAT        IFEQ      'B'
     C                   LEAVE
     C                   ENDIF
     C     ARKEY2        READE     PAR01                                  87
     C                   ENDDO
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDEQ     'B'
     C     ARTRTP        OREQ      'I'                                          ADD 06/12/97
     C     ARSTAT        ANDEQ     'D'                                          ADD 06/12/97
     C                   Z-ADD     AREOB#        XXEOB#
     C     SCCASE        IFEQ      *ZEROS
     C*
     C     EAKEY2        CHAIN     PEA02                              65
     C*
     C                   ELSE
     C*
     C     EAKEY         CHAIN     PEA01                              65
     C*
     C                   ENDIF
     C     *IN65         IFEQ      '0'
     C                   ADD       1             I1
     C     I1            ADD       30            Z1
     C                   EXSR      FILL
     C                   END                                                    ENDEQ *IN65
     C                   END                                                    ENDEQ 'I'
     C*
     C     ARKEY2        SETGT     PAR01                                        ADD 06/12/97
     C     ARKEY1        READE     PAR01                                  60    INV
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      12                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C******************************************************
     C*  UPDATE FWEOP FILE W/ DELETE FLAG (EODIFL) - RITE  *
     C******************************************************
     CSR   RITE          BEGSR
     C*
     C*  ONLY PERFORM THIS SUBROUTINE IF THIS IS NOT A NETWORK INVOICE
     C     ARSYS         IFNE      'N'
     C*
     C* VALIDATE OPTION 4 OR BLANKS
     C*
     C                   DO        12            Y                 5 0
     C     Y             ADD       44            Y1                2 0
     C     FL(Y)         IFNE      *BLANK
     C     FL(Y)         ANDNE     '4'
     C*** BEGIN ADD *** 10/07/99 **************************************
     C     FL(Y)         ANDNE     '17'
     C*** END   ADD *** 10/07/99 **************************************
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*
     C                   DO        12            V                 2 0
     C     EO(V)         IFGT      *ZERO
     C                   Z-ADD     EO(V)         XXEOB#
     C     EWKY1         CHAIN     PEW01                              70        INV/EOB
     C     *IN70         IFEQ      '0'
     C     EO(V)         ANDEQ     EAEOB#
     C*** BEGIN CHG *** 10/07/99 **************************************
     C**********           Z-ADDAM,V      EABAMT
     C                   Z-ADD     AM(V)         EABCAM
     C     TA(V)         ADD       AM(V)         EABAMT
     C*
     C     TA(V)         IFNE      TAH(V)
     C*
     C                   MOVEL     'Y'           EWCHIN
     C*
     C                   ELSE
     C*
     C                   MOVEL     ' '           EWCHIN
     C*
     C                   ENDIF
     C*** END   CHG *** 10/07/99 **************************************
     C     FL(V)         IFEQ      '4'
     C                   MOVE      'Y'           EABDIF
     C                   ELSE
     C                   MOVE      *BLANK        EABDIF
     C                   ENDIF
     C                   UPDATE    PEW01
     C                   END                                                    ENDEQ *IN70
     C                   END                                                    ENDGT *ZERO
     C*** BEGIN ADD *** 10/07/99 **************************************
     C*
     C* CALL WORK WITH T/M ACTIVITYS
     C*
     C     FL(V)         IFEQ      '17'
     C*
     C     TM(V)         IFEQ      '*'
     C*
     C                   CALL      'F3040R'
     C                   PARM      ' '           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      RN(V)         ##RRN2            9 0
     C                   PARM      *ZEROS        ##RRN3            9 0
     C                   PARM      CT(V)         ##CERT           15
     C                   PARM      EX(V)         ##EXT             3 0
     C                   PARM      'C'           ##EOB             1
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   PARM      *ZEROS        ##RCN             3 0
     C*** END   ADD *** 05/19/00 **************************************
     C*
     C                   SETON                                        16
     C                   Z-ADD     *ZEROS        XXEOB#
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      SCR01T
     C*
     C                   ELSE
     C*
     C     V             ADD       70            Y1                2 0
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 10/07/99 **************************************
     C                   END                                                    END DO
     C                   END                                                    END NE 'N'
     CSR                 ENDSR
     C*
     C**************************************************
     C*  FILL SCREEN ARRAYS - FILLN - NETWORK ONLY     *
     C**************************************************
     C     FILLN         BEGSR
     C     EANDIF        IFEQ      'Y'
     C                   MOVE      4             FL(I1)
     C                   ENDIF
     C                   MOVE      EAEOB#        EO(I1)
     C*** BEGIN ADD *** 01/22/02 **************************************
     C                   MOVE      EARCN         RC(I1)
     C*** END   ADD *** 01/22/02 **************************************
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C                   MOVEL     ARDESC        DS(I1)
     C*** BEGIN ADD *** 10/07/99 **************************************
     C     EOKY          CHAIN     PEO01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     DBRRN         RN(I1)
     C                   MOVEL     EOCERT        CT(I1)
     C                   Z-ADD     EOEXT         EX(I1)
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        RN(I1)
     C                   MOVEL     *BLANKS       CT(I1)
     C                   Z-ADD     *ZEROS        EX(I1)
     C*
     C                   ENDIF
     C*
     C*** BEGIN DEL *** 01/22/02 **************************************
     C********** EATMFL    IFEQ 'Y'
     C*
     C**********           MOVEL'*'       TM,I1
     C*
     C**********           ELSE
     C*
     C**********           MOVEL' '       TM,I1
     C*
     C**********           ENDIF
     C*** END   DEL *** 01/22/02 **************************************
     C*** END   ADD *** 10/07/99 **************************************
     C*
     C*** BEGIN CHG *** 01/22/02 **************************************
     C**********           MOVE EASYS     SY,I1
     C                   MOVE      'N'           SY(I1)
     C*** END   CHG *** 01/22/02 **************************************
     C*** BEGIN CHG *** 10/07/99 **************************************
     C**********           Z-ADDEANAMT    AM,I1            REVIEWR FEE
     C                   Z-ADD     EANCAM        AM(I1)                         REVIEWR FEE
     C*** BEGIN CHG *** 01/22/02 **************************************
     C********** EANAMT    SUB  EANCAM    TA,I1            REVIEWR FEE
     C********** ARAMT     SUB  EANCAM    TAH,I1
     C                   Z-ADD     *ZEROS        TA(I1)                         REVIEWR FEE
     C                   Z-ADD     *ZEROS        TAH(I1)
     C*** END   CHG *** 01/22/02 **************************************
     C*** END   CHG *** 10/07/99 **************************************
     C     ENKY1         CHAIN     PEN01                              60
     C     *IN60         IFEQ      '0'
     C     EANDIF        IFEQ      'Y'
     C                   MOVE      4             XXNDIF            1
     C     XXNDIF        IFNE      FL(I1)
     C                   MOVE      XXNDIF        FL(I1)
     C                   END
     C*** BEGIN DEL *** 01/22/02 **************************************
     C*** BEGIN ADD *** 10/07/99 **************************************
     C********** EANAMT    SUB  EANCAM    XXNAMT
     C*** END   ADD *** 10/07/99 **************************************
     C********** EANAMT    IFNE *ZEROS
     C*** BEGIN CHG *** 10/07/99 **************************************
     C********** EANAMT    ANDNEAM,I1
     C**********           Z-ADDEANAMT    AM,I1
     C********** XXNAMT    ANDNETA,I1
     C**********           Z-ADDEANAMT    TA,I1
     C********** ARAMT     SUB  EANAMT    TAH,I1
     C*** END   CHG *** 10/07/99 **************************************
     C**********           END
     C*** BEGIN ADD *** 10/07/99 **************************************
     C********** EANCAM    IFNE *ZEROS
     C********** EANCAM    ANDNEAM,I1
     C**********           Z-ADDEANCAM    AM,I1
     C**********           END
     C*** END   ADD *** 10/07/99 **************************************
     C*** END   DEL *** 01/22/02 **************************************
     C                   END
     C                   ENDIF
     C*
     C                   MOVE      '1'           *IN(Z1)                        COLM SEPARATORS
     C                   ENDSR
     C**************************************************
     C*  LOAD FORWARD - LOADN - NETWORK ONLY           *
     C**************************************************
     C     LOADN         BEGSR
     C                   EXSR      CLR
     C     OVERN         TAG
     C*
     C*  END OF FILE
     C*
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONEN
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C*  IF STATIS IS EQUAL TO 'O', AMOUNT HAS BEEN PREVIOUSLY REMOVED
     C*  BUT THE EOMB HAS NOT BEEN DELETED. INV# STILL CONTAINS THE
     C*  INVOICE NUMBER.
     C*
     C     *IN87         DOUEQ     '1'
     C*
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDNE     'B'
     C                   ADD       ARAMT         EANAMT
     C                   END
     C                   Z-ADD     AREOB#        XXEOB#
     C*
     C     ARSTAT        IFEQ      'B'
     C                   LEAVE
     C                   ENDIF
     C     ARKEY2        READE     PAR01                                  87
     C                   ENDDO
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDEQ     'B'
     C     ARTRTP        OREQ      'I'                                          ADD 06/12/97
     C     ARSTAT        ANDEQ     'D'                                          ADD 06/12/97
     C                   Z-ADD     AREOB#        XXEOB#
     C     EAKEY3        CHAIN     PEA03                              65
     C     *IN65         IFEQ      '0'
     C                   Z-ADD     ARINV#        EANINV
     C     ARSTAT        IFEQ      'D'
     C                   Z-ADD     XXAMT         EANAMT
     C                   ELSE
     C                   Z-ADD     ARAMT         EANAMT                         ADD 07/14/93
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       ENCHIN
     C*
     C  N16              WRITE     PEN01
     C                   ADD       1             I1
     C     I1            ADD       30            Z1
     C                   EXSR      FILLN
     C                   END                                                    ENDEQ *IN65
     C                   END                                                    ENDEQ 'I'
     C*
     C     ARKEY2        SETGT     PAR01                                        ADD 06/12/97
     C     ARKEY1        READE     PAR01                                  60    INV
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEN
     C                   END
     C     I1            COMP      12                                   61
     C   61              GOTO      OVERN
     C     DONEN         TAG
     C                   MOVE      EO(1)         KEY(1)
     C                   MOVE      EO(12)        KEY(2)
     C                   ENDSR
     C**************************************************
     C*  LOAD BACKWARD - LOADNP - NETWORK ONLY         *
     C**************************************************
     C     LOADNP        BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     12            I1
     C     OVERNP        TAG
     C*
     C*  BEGINNING OF FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADNB
     C                   GOTO      DONENP
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C*  IF STATIS IS EQUAL TO 'O', AMOUNT HAS BEEN PREVIOUSLY REMOVED
     C*  BUT THE EOMB HAS NOT BEEN DELETED. INV# STILL CONTAINS THE
     C*  INVOICE NUMBER.
     C*
     C     *IN87         DOUEQ     '1'
     C*
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDNE     'B'
     C                   ADD       ARAMT         EANAMT
     C                   END
     C                   Z-ADD     AREOB#        XXEOB#
     C*
     C     ARSTAT        IFEQ      'B'
     C                   LEAVE
     C                   ENDIF
     C     ARKEY2        READE     PAR01                                  87
     C                   ENDDO
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDEQ     'B'
     C     ARTRTP        OREQ      'I'                                          ADD 06/12/97
     C     ARSTAT        ANDEQ     'D'                                          ADD 06/12/97
     C                   Z-ADD     AREOB#        XXEOB#
     C     EAKEY3        CHAIN     PEA03                              65
     C     *IN65         IFEQ      '0'
     C                   SUB       1             I1
     C     I1            ADD       30            Z1
     C                   EXSR      FILLN
     C                   END                                                    ENDEQ *IN65
     C                   END                                                    ENDEQ 'I'
     C*
     C                   READP     PAR01                                  60
     C  N60ARCO#         COMP      KKCO#                              6060
     C  N60ARSUB#        COMP      SCSUB#                             6060
     C  N60ARFND         COMP      SCFND                              6060
     C  N60ARFYR         COMP      XXFYR                              6060
     C  N60ARCASE        COMP      SCCASE                             6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERNP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERNP
     C     DONENP        TAG
     C                   MOVE      EO(1)         KEY(1)
     C                   MOVE      EO(12)        KEY(2)
     C                   ENDSR
     C**************************************************
     C*  LOAD FROM BEGINNING OF FILE  - LOADNB         *
     C**************************************************
     C     LOADNB        BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANKS       XXEOB#
     C     ARKEY2        SETLL     PAR01                                        INV/EOB
     C     ARKEY1        READE     PAR01                                  60    INV
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONENB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERNB        TAG
     C*
     C*  IF STATIS IS EQUAL TO 'O', AMOUNT HAS BEEN PREVIOUSLY REMOVED
     C*  BUT THE EOMB HAS NOT BEEN DELETED. INV# STILL CONTAINS THE
     C*  INVOICE NUMBER.
     C*
     C     *IN87         DOUEQ     '1'
     C*
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDNE     'B'
     C                   ADD       ARAMT         EANAMT
     C                   END
     C                   Z-ADD     AREOB#        XXEOB#
     C*
     C     ARSTAT        IFEQ      'B'
     C                   LEAVE
     C                   ENDIF
     C     ARKEY2        READE     PAR01                                  87
     C                   ENDDO
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDEQ     'B'
     C     ARTRTP        OREQ      'I'                                          ADD 06/12/97
     C     ARSTAT        ANDEQ     'D'                                          ADD 06/12/97
     C                   Z-ADD     AREOB#        XXEOB#
     C     EAKEY3        CHAIN     PEA03                              65
     C     *IN65         IFEQ      '0'
     C                   ADD       1             I1
     C     I1            ADD       30            Z1
     C                   EXSR      FILLN
     C                   END                                                    ENDEQ *IN65
     C                   END                                                    ENDEQ 'I'
     C*
     C     ARKEY2        SETGT     PAR01                                        ADD 06/12/97
     C     ARKEY1        READE     PAR01                                  60    INV
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONENB
     C                   END
     C     I1            COMP      12                                   61
     C   61              GOTO      OVERNB
     C     DONENB        ENDSR
     C*****************************************************************
**
ROLL UP
ROLL DOWN
