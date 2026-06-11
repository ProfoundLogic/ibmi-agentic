     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H*        SPECIAL NOTE:                                         **
     H*  T/M CHARGES LISTING IS RESTRICTED TO SYSTEM TYPES U,P,H     **
     H*  IF THIS CHANGES THE FOLLOWING PROGRAMS WILL NEED TO BE      **
     H*  CHANGED: N9300R                                             **
     H*           N7000R                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** PROGRAM:   N7129R - TRIAL INVOICES                          **
     H** DESCRIPTION:  PRINT PROGRAM - ORIGINAL/ REBILLING           **
     H**                                                             **
     H** FOR ORIGINAL BILLING:                                       **
     H**  EOB File selected on EOHRFL *NE 'H' &  EOBLFL *NE 'P'      **
     H*****************************************************************
     H** REVISIONS:
     H*                                                               *
     H* 06/09/20 C:212 Project : SY420                                *
     H*                Mask SSN                                       *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*   01/28/10 C:249 Project: SY215                               *
     H*            C:189 Retrieve brand Name                          *
     H*                                                               *
     H*  04/10/07 C:58  PROJECT: 139MGK
     H*                 o Suppress printing of T&M pages on invoice
     H*                     for client 139.
     H*
     H*  08/07/06 C:62  PROJECT: 139MEM
     H*                 ONLY INCLUDE T/M WHERE BILLING FLAG = Y
     H*
     H*  08/03/06 C:62  PROJECT: 139MEJ
     H*                 -CHANGE SS# TO CONSTANT XXX-XX-XXXX
     H*                 -CHANGE FROM 30 DAYS IN FUTURE TO 5.
     H*
     H*  03/10/05 C:62  PROJECT: MC260
     H*                 -USE ALTERNATIVE FEIN IF ENTERED
     H*
     H*  03/10/05 C:62  PROJECT: MC260
     H*                 -USE ALTERNATIVE FEIN IF ENTERED
     H*
     H*  08/24/04 C:62  PROJECT: MC090 43570
     H*                 -USE ALTERNATIVE PHONE#
     H*
     H*  04/13/04 C:62  PROJECT: MC090 42134
     H*                 FIX ZIP
     H*
     H*  03/09/00 C:62  PROJECT: MCMNT
     H*                 EXPAND DATES TO 8.0 AND FYR TO 3.0.
     H*
     H*  11/17/99 C:53  PROJECT: AR029
     H*                 1. CORRECT DETAIL CHARGES TO SEQUENCE BY DATE
     H*                 2. ADD ALTERNATE ADDRESS PROCESSING
     H*                 3. CORRECTED ZIP CODE ISSUE
     H*                 4. REMOVED TABLE LOOKUP OF CM SERVICES TO
     H*                    DEFAULT TO THE TM RECORD.
     H*
     H*  11/17/99 C:53  PROJECT: Y2K LOG # 18
     H*                 CORRECT DATE ISSUES FROM MDY TO MDCY
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**                                                             **
     H**  21       21 ON MEANS AT LEAST ONE INVOICE AMT IS GREATER   **
     H**           THAN 0 AND AT LEAST ONE INVOICE WILL BE PRODUCED. **
     H**           THIS PREVENTS THE EXCPT ON HEAD AND TOT FROM      **
     H**           SRCASE WHICH WOULD PRODUCE A BLANK INVOICE.       **
     H**                                                             **
     H**  95       CHAIN TO HBA FILE TO CHECK INVOICE AMT            **
     H**  96       PROVIDER MODIFIER EXISTS                          **
     H** N96       NO PROVIDER MODIFIER                              **
     H**  97       RE-BILL                                           **
     H** N97       ORIGINAL BILL                                     **
     H**  98       CLAIM NUMBER EXISTS                               **
     H** N98       NO CLAIM NUMBER                                   **
     H**  99       HBA BILL                                          **
     H** N99       PBA BILL                                          **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     F*  XF#    = FILE # (GROUP-YEAR-CASE)                            *
     F*  XXACDT = ACCIDENT DATE                                       *
     F*  XXAMT  = BILLING AMOUNT (DETAIL LINE ON BILL)                *
     F*  XXCCLM = CLAIM #                                             *
     F*  XXCHBA = BILLED AMOUNT (CASE TOTAL ON RPT)                   *
     F*  XXCONM = COMPANY NAME                                        *
     F*  XXCPBR = BILLED AMOUNT (CASE TOTAL ON RPT)                   *
     F*  XXCTOT = BILLING AMOUNT (TOTAL LINE ON BILL)                 *
     F*  XXDESC = SERVICES DESCRIPTION                                *
     F*  XXFND  = GROUP                                               *
     F*  XXFNNM = GROUP NAME                                          *
     F*  XXFRDT = FROM DATE                                           *
     F*  XXEOB# = EOB #                                               *
     F*  XXGHBA = BILLED AMOUNT (GRP TOTAL ON RPT)                    *
     F*  XXGPBR = BILLED AMOUNT (GRP TOTAL ON RPT)                    *
     F*  XXINV# = CASE INVOICE #                                      *
     F*  XXMOD  = PROVIDER MODIFIER                                   *
     F*  XXPNAM = PATIENT NAME                                        *
     F*  XXPRNM = PROVIDER NAME                                       *
     F*  XXPRV# = PROVIDER #                                          *
     F*  XXSS#  = SOC SEC #                                           *
     F*  XXTHBA = BILLED AMOUNT (ALL GRP TOTAL ON RPT)                *
     F*  XXTPBR = BILLED AMOUNT (ALL GRP TOTAL ON RPT)                *
     F*  XXTRDT = TRANSACTION DATE                                    *
     F*  XXTODT = TO DATE                                             *
     F*  XX#CHB = # BILLS HBA (CASE TOTAL ON RPT)                     *
     F*  XX#CPB = # BILLS PBR (CASE TOTAL ON RPT)                     *
     F*  XX#GHB = # BILLS HBA (GRP TOTAL ON RPT)                      *
     F*  XX#GPB = # BILLS PBR (GRP TOTAL ON RPT)                      *
     F*  XX#THB = # BILLS HBA (ALL GRP TOTAL ON RPT)                  *
     F*  XX#TPB = # BILLS PBR (ALL GRP TOTAL ON RPT)                  *
     F*****************************************************************
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFAEOL4    UF   E           K DISK
     F*** BEG CHG *** 03/19/99 ***************************************
     F*** BEG CHG *** 01/08/99 ***************************************
     F*** BEGIN CHG *** 09/23/99 ************************************
     F**********FAEAL1  UF  E           K        DISK
     FFAEAL1    IF   E           K DISK
     F*** END   CHG *** 09/23/99 ************************************
     F**********FAEAL1  IF  E           K        DISK
     F*** END CHG *** 03/19/99 ***************************************
     F*** BEGIN ADD *** 09/23/99 ************************************
     FFAEAL14   IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     F*** END   ADD *** 09/23/99 ************************************
     FFAHBL     IF   E           K DISK
     FNAPBL1    IF   E           K DISK
     FNAMBL1    IF   E           K DISK
     F**********FAHBL   UF  E           K        DISK
     F**********NAPBL1  UF  E           K        DISK
     F*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     F**********NAMBL1  UF  E           K        DISK
     F*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     F*** END CHG *** 01/08/99 ***************************************
     FSMCOL     IF   E           K DISK
     FFMC1L     UF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F*** BEG ADD *** 01/08/99 ***************************************
     F                                     INFDS(DBFDS)
     F*** END ADD *** 01/08/99 ***************************************
     FFMPDL     IF   E           K DISK
     FNMARP     O    E             DISK
     FNMARL8    IF   E           K DISK
     F                                     RENAME(PAR01:PAR02)
     F*** BEG CHG *** 01/08/99 ***************************************
     FNDTML3    UF   E           K DISK
     F**********NDTML1  IF  E           K        DISK
     F*** END CHG *** 01/08/99 ***************************************
     F*** BEG DEL *** 11/17/99 ***************************************
     F*****FTABL***IF  E           K        DISK
     F*** END DEL *** 11/17/99 ***************************************
     F*** BEG ADD *** 11/17/99 ***************************************
     FFMF1L     IF   E           K DISK
     F*** END ADD *** 11/17/99 ***************************************
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     F*** BEG CHG *** 01/14/99 ***************************************
     FFAEOLG    IF   E           K DISK
     F**********FAEOL3  IF  E           K        DISK
     F*** END CHG *** 01/14/99 ***************************************
     F                                     RENAME(PEO01:PEO02)
     FFMSML     IF   E           K DISK
     FFWEOP     IF   E           K DISK
     F                                     RENAME(PEO01:PEW01)
     F*
     FQSYSPRT   O    F  127        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)
     FQSYSPRT1  O    F  198        PRINTER INFDS(PR1DS)
     F                                     OFLIND(*INOA)
     FQSYSPRT2  O    F  127        PRINTER OFLIND(*INOB)
     F*

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      /copy sPrSecure
      ***** End Add *** 06/09/20 *******************************************

     D*** BEGIN CHG ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     D********************DS      1   5 30               BILLING DESC.
     D DS              S             30    DIM(6) CTDATA PERRCD(1)              BILLING DESC.
     D*** END   CHG ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D*** FILE ARRAYS
     D EOB             S              9  0 DIM(999)                             EOB# PER CASE
     D*** BEGIN ADD *** 01/15/99 **************************************
     D RCN             S              3  0 DIM(999)                             RCN# PER CASE
     D*** END   ADD *** 09/23/99 *************************************
     D*
     D*** BEG ADD *** 01/08/99 ***************************************
     D*  WRITE/UPDATE DATA STRUCTURE.
     D                 DS
     D  KKWRUP                 1      7
     D  XXFAEO                 1      1
     D  XXFAE1                 2      2
     D  XXFAHB                 3      3
     D  XXNAPB                 4      4
     D  XXNAMB                 5      5
     D  XXNATR                 6      6
     D  XXFAEA                 7      7
     D*** END ADD *** 01/08/99 ***************************************
     D                 DS
     D  XSZIP                  1      9  0
     D  S1ZPP1                 1      5  0
     D  S1ZPP2                 6      9  0
     D*** BEG ADD *** 11/17/99 ****************************
     D                 DS
     D  XXZIP                  1     10
     D  XXZIP1                 1      5
     D  XXZIPD                 6      6
     D  XXZIP2                 7     10
     D*** END ADD *** 11/17/99 ****************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND
     D  CONAME                 1     40
     D                 DS
     D  XCNM                   1     40
     D                                     DIM(40) ASCEND
     D  XSNAME                 1     40
     D                 DS
     D  FNNM                   1     60
     D                                     DIM(60) ASCEND
     D  FNNAME                 1     60
     D***
     D                 DS
     D*** BEGIN CHG *** 03/09/00 **************************************
     D**********                              1  20 XF#
     D**********                              1   50XXSUB#
     D**********                              6   6 XFHYP1
     D**********                              7   90XXFND
     D**********                             10  10 XFHYP2
     D**********                             11  120XXYR
     D**********                             13  13 XFHYP3
     D**********                             14  200XXCASE
     D  XF#                    1     21
     D  XXSUB#                 1      5  0
     D  XFHYP1                 6      6
     D  XXFND                  7      9  0
     D  XFHYP2                10     10
     D  XXYR                  11     13  0
     D  XFHYP3                14     14
     D  XXCASE                15     21  0
     D*** END   CHG *** 03/09/00 **************************************
     D***
     D                 DS
     D  XCPHN#                 1     14
     D  XXAREA                 2      4  0
     D  XXCPRN                 5      5
     D  XXPH1                  7      9  0
     D  XXHP1                 10     10
     D  XXPH2                 11     14  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D PR1DS           DS
     D  FILL3                  1    200
     D  FILL4                201    366
     D  X1LINE               367    368B 0
     D  X1PAGE               369    372B 0
     D                 DS
     D  FSY                    1     10
     D                                     DIM(10)                              FILE SYSTEM CODE
     D  FNSY01                 1      1
     D  FNSY02                 2      2
     D  FNSY03                 3      3
     D  FNSY04                 4      4
     D  FNSY05                 5      5
     D  FNSY06                 6      6
     D  FNSY07                 7      7
     D  FNSY08                 8      8
     D  FNSY09                 9      9
     D  FNSY10                10     10
     D                 DS
     D  FBR                    1     30
     D                                     DIM(10)                              FILE BILL/RATE CD
     D  FNBR01                 1      3
     D  FNBR02                 4      6
     D  FNBR03                 7      9
     D  FNBR04                10     12
     D  FNBR05                13     15
     D  FNBR06                16     18
     D  FNBR07                19     21
     D  FNBR08                22     24
     D  FNBR09                25     27
     D  FNBR10                28     30
     D                 DS
     D  ESB                    1     10
     D                                     DIM(10)                              SYSTEMS BILLED
     D  EOSB01                 1      1
     D  EOSB02                 2      2
     D  EOSB03                 3      3
     D  EOSB04                 4      4
     D  EOSB05                 5      5
     D  EOSB06                 6      6
     D  EOSB07                 7      7
     D  EOSB08                 8      8
     D  EOSB09                 9      9
     D  EOSB10                10     10
     D                 DS
     D  XZPGM                  1      8
     D  XZTYPE                 4      5
     D*** BEGIN ADD *** 08/03/2006 ************************************
     D SS#             C                   CONST('XXX-XX-XXXX')
     D*** END   ADD *** 08/03/2006 ************************************
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** BEG ADD *** 01/08/99 ***************************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0

      ***** Beg Add *** 06/09/20 *******************************************
     d xxss#           s             11    inz
      ***** End Add *** 06/09/20 *******************************************

     I*** END   ADD *** 01/15/99 **************************************
     I*
     IPEO02
     I              EOCO#                       X2CO#
     I              EOFND                       X2FND
     I              EOFYR                       X2FYR
     I              EOCASE                      X2CASE
     I              EOTRDT                      X2TRDT
     I              EOFEIN                      X2FEIN
     I              EOMOD                       X2MOD
     I              EOAMT                       X2AMT
     I              EOFRDT                      X2FRDT
     I              EOTODT                      X2TODT
     I              EOTRCD                      X2TRCD
     I              EOPNAM                      X2PNAM
     I              EOADR1                      X2ADR1
     I              EOADR2                      X2ADR2
     I              EOCTY                       X2CTY
     I              EOST                        X2ST
     I              EOZIP                       X2ZIP
     I              EOEOB1                      X2EOB1
     I              EOEOB2                      X2EOB2
     I              EOEOB3                      X2EOB3
     I              EOCHK#                      X2CHK#
     I              EOEOB#                      X2EOB#
     I              EOSYS                       X2SYS
     I              EOFLG1                      X2FLG1
     I              EOFLG2                      X2FLG2
     I              EOFLG3                      X2FLG3
     I              EOFLG4                      X2FLG4
     I              EOFLG5                      X2FLG5
     I              EOFLG6                      X2FLG6
     I              EOFLG7                      X2FLG7
     I              EOFLG8                      X2FLG8
     I              EOFLG9                      X2FLG9
     I              EOAMTC                      X2AMTC
     I              EOSTAT                      X2STAT
     I              EOUSER                      X2USER
     I              EOPGM                       X2PGM
     I              EOTIME                      X2TIME
     I              EODT                        X2DT
     I              EOIUSR                      X2IUSR
     I              EOIDT                       X2IDT
     I              EOWCCK                      X2WCCK
     I              EODIA1                      X2DIA1
     I              EODIA2                      X2DIA2
     I              EODIA3                      X2DIA3
     I              EODIA4                      X2DIA4
     I              EOEMPL                      X2EMPL
     I              EODFC                       X2DFC
     I              EOFLIC                      X2FLIC
     I              EOTAPE                      X2TAPE
     I              EO#BLL                      X2#BLL
     I              EOCLSF                      X2CLSF
     I              EOAST                       X2AST
     I              EOSUB#                      X2SUB#
     I              EOAUDT                      X2AUDT
     I              EOBTFL                      X2BTFL
     I              EOHRFL                      X2HRFL
     I              EOBLFL                      X2BLFL
     I              EONLFL                      X2NLFL
     I              EODIFL                      X2DIFL
     I              EOINV#                      X2INV#
     I              EORMDS                      X2RMDS
     I              EORMK1                      X2RMK1
     I              EORMK2                      X2RMK2
     I              EORMK3                      X2RMK3
     I              EOCKCN                      X2CKCN
     I              EOOTHR                      X2OTHR
     I              EOSB01                      X2SB01
     I              EOSB02                      X2SB02
     I              EOSB03                      X2SB03
     I              EOSB04                      X2SB04
     I              EOSB05                      X2SB05
     I              EOSB06                      X2SB06
     I              EOSB07                      X2SB07
     I              EOSB08                      X2SB08
     I              EOSB09                      X2SB09
     I              EOSB10                      X2SB10
     I              EOBAMT                      X2BAMT
     I              EOZIPA                      X2ZIPA
     I              EOCERT                      X2CERT
     I              EOEXT                       X2EXT
     IPEW01
     I              EOCO#                       XWCO#
     I              EOFND                       XWFND
     I              EOFYR                       XWFYR
     I              EOCASE                      XWCASE
     I              EOTRDT                      XWTRDT
     I              EOFEIN                      XWFEIN
     I              EOMOD                       XWMOD
     I              EOAMT                       XWAMT
     I              EOFRDT                      XWFRDT
     I              EOTODT                      XWTODT
     I              EOTRCD                      XWTRCD
     I              EOPNAM                      XWPNAM
     I              EOADR1                      XWADR1
     I              EOADR2                      XWADR2
     I              EOCTY                       XWCTY
     I              EOST                        XWST
     I              EOZIP                       XWZIP
     I              EOEOB1                      XWEOB1
     I              EOEOB2                      XWEOB2
     I              EOEOB3                      XWEOB3
     I              EOCHK#                      XWCHK#
     I              EOEOB#                      XWEOB#
     I              EOSYS                       XWSYS
     I              EOFLG1                      XWFLG1
     I              EOFLG2                      XWFLG2
     I              EOFLG3                      XWFLG3
     I              EOFLG4                      XWFLG4
     I              EOFLG5                      XWFLG5
     I              EOFLG6                      XWFLG6
     I              EOFLG7                      XWFLG7
     I              EOFLG8                      XWFLG8
     I              EOFLG9                      XWFLG9
     I              EOAMTC                      XWAMTC
     I              EOSTAT                      XWSTAT
     I              EOUSER                      XWUSER
     I              EOPGM                       XWPGM
     I              EOTIME                      XWTIME
     I              EODT                        XWDT
     I              EOIUSR                      XWIUSR
     I              EOIDT                       XWIDT
     I              EOWCCK                      XWWCCK
     I              EODIA1                      XWDIA1
     I              EODIA2                      XWDIA2
     I              EODIA3                      XWDIA3
     I              EODIA4                      XWDIA4
     I              EOEMPL                      XWEMPL
     I              EODFC                       XWDFC
     I              EOFLIC                      XWFLIC
     I              EOTAPE                      XWTAPE
     I              EO#BLL                      XW#BLL
     I              EOCLSF                      XWCLSF
     I              EOAST                       XWAST
     I              EOSUB#                      XWSUB#
     I              EOAUDT                      XWAUDT
     I              EOBTFL                      XWBTFL
     I              EOHRFL                      XWHRFL
     I              EOBLFL                      XWBLFL
     I              EONLFL                      XWNLFL
     I              EODIFL                      XWDIFL
     I              EOINV#                      XWINV#
     I              EORMDS                      XWRMDS
     I              EORMK1                      XWRMK1
     I              EORMK2                      XWRMK2
     I              EORMK3                      XWRMK3
     I              EOCKCN                      XWCKCN
     I              EOOTHR                      XWOTHR
     I              EOSB01                      XWSB01
     I              EOSB02                      XWSB02
     I              EOSB03                      XWSB03
     I              EOSB04                      XWSB04
     I              EOSB05                      XWSB05
     I              EOSB06                      XWSB06
     I              EOSB07                      XWSB07
     I              EOSB08                      XWSB08
     I              EOSB09                      XWSB09
     I              EOSB10                      XWSB10
     I              EOBAMT                      XWBAMT
     I              EOZIPA                      XWZIPA
     I              EOCERT                      XWCERT
     I              EOEXT                       XWEXT
     I*** BEGIN ADD *** 09/23/99 *************************************
     IPEA02
     I              EACO#                       XACO#
     I              EAFND                       XAFND
     I              EAFYR                       XAFYR
     I              EACASE                      XACASE
     I              EASS#                       XASS#
     I              EAACDT                      XAACDT
     I              EAEOB#                      XAEOB#
     I              EARCN                       XARCN
     I              EATRDT                      XATRDT
     I              EACHRG                      XACHRG
     I              EAMRA                       XAMRA
     I              EAOTHR                      XAOTHR
     I              EAOTH2                      XAOTH2
     I              EANDIS                      XANDIS
     I              EAPAID                      XAPAID
     I              EASTAT                      XASTAT
     I              EAUSER                      XAUSER
     I              EAPGM                       XAPGM
     I              EATIME                      XATIME
     I              EADT                        XADT
     I              EAIUSR                      XAIUSR
     I              EAIPGM                      XAIPGM
     I              EAITIM                      XAITIM
     I              EAIDT                       XAIDT
     I              EAWCPC                      XAWCPC
     I              EAWCPD                      XAWCPD
     I              EAWCBC                      XAWCBC
     I              EAWCBD                      XAWCBD
     I              EAWCNC                      XAWCNC
     I              EAWCND                      XAWCND
     I              EABAMT                      XABAMT
     I              EANAMT                      XANAMT
     I              EARMDS                      XARMDS
     I              EARMK1                      XARMK1
     I              EARMK2                      XARMK2
     I              EARMK3                      XARMK3
     I              EACKST                      XACKST
     I              EATRCD                      XATRCD
     I              EAPBRC                      XAPBRC
     I              EABHRF                      XABHRF
     I              EABBLF                      XABBLF
     I              EABINV                      XABINV
     I              EABCAM                      XABCAM
     I              EANHRF                      XANHRF
     I              EANBLF                      XANBLF
     I              EANINV                      XANINV
     I              EANCAM                      XANCAM
     I              EAESDT                      XAESDT
     I              EAEDI                       XAEDI
     I              EAEPFL                      XAEPFL
     I              EABPFL                      XABPFL
     I              EASYS                       XASYS
     I              EAFEIN                      XAFEIN
     I              EAMOD                       XAMOD
     I              EAPLNK                      XAPLNK
     I              EABIDT                      XABIDT
     I              EANIDT                      XANIDT
     I              EAAPPF                      XAAPPF
     I              EABDIF                      XABDIF
     I              EANDIF                      XANDIF
     I              EABR2                       XABR2
     I              EATMFL                      XATMFL
     I              EACNTL                      XACNTL
     I*** END ADD *** 01/08/99 ***************************************
     I*

     C*
     C*  COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*  FUND MASTER
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE COMPANY AUXILIARY FILE
     C*
     C     C1KEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  PAYEE DEMOGRAPHICS
     C*
     C     PDKEY         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C*** BEG ADD *** 03/19/99 **************************************
     C     PDKEY1        KLIST
     C                   KFLD                    EAFEIN
     C                   KFLD                    EAMOD
     C*** END ADD *** 03/19/99 **************************************
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*** BEG ADD *** 01/08/99 ***************************************
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*** BEGIN ADD *** 01/15/99 **************************************
     C                   KFLD                    XXRCN
     C*** END   ADD *** 01/15/99 **************************************
     C*
     C*
     C     EAKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    EOB(X)
     C*** BEGIN ADD *** 01/15/99 **************************************
     C                   KFLD                    RCN(X)
     C*** END   ADD *** 01/15/99 **************************************
     C*** BEGIN ADD *** 09/23/99 **************************************
     C     EAKEY3        KLIST
     C                   KFLD                    TMCO#
     C                   KFLD                    TMEOB#
     C                   KFLD                    TMRCN
     C                   KFLD                    TMSUB#
     C                   KFLD                    TMFND
     C                   KFLD                    TMFYR
     C                   KFLD                    TMCASE
     C*** END   ADD *** 09/23/99 **************************************
     C*** END ADD *** 01/08/99 ***************************************
     C*
     C*  PRE/RE-CERT TRANSACTION FILE
     C*
     C     PBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*  PRE/RE-CERT TRANS FILE -- KEY FOR SRAMT SUBR
     C*
     C     PBKEY1        KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     PBKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    EOB(X)
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C*  MEDICAL MANAGEMENT TRANSACTION FILE
     C*
     C     MBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C     MBKEY1        KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     MBKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    EOB(X)
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C*  TIME/MATERIALS BILLING FILE
     C*
     C     TMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    EOB(X)
     C*** BEGIN ADD *** 01/15/99 **************************************
     C                   KFLD                    RCN(X)
     C*** END   ADD *** 01/15/99 **************************************
     C*
     C*** BEG ADD *** 01/08/99 ***************************************
     C*
     C     TMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*** BEGIN ADD *** 01/15/99 **************************************
     C                   KFLD                    XXRCN
     C*** END   ADD *** 01/15/99 **************************************
     C*
     C*** END ADD *** 01/08/99 ***************************************
     C*  EOMB FILE
     C*
     C*** BEG CHG *** 01/14/99 ***************************************
     C     EOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    EOB(X)
     C*
     C********** EOKEY     KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXSUB#
     C**********           KFLD           EOB,X
     C*
     C*** END CHG *** 01/14/99 ***************************************
     C*** BEG ADD *** 03/19/99 ***************************************
     C     EOKEY1        KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C*** END ADD *** 03/19/99 ***************************************
     C*  TABLE FIEL KEY LIST
     C*
     C*** BEG DEL *** 11/17/99 **************************************
     C********** TBLKEY    KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXKEY  10
     C*** END DEL *** 11/17/99 **************************************
     C*
     C*  USER PARMETER FILE
     C*
     C     UPKY1         KLIST
     C                   KFLD                    XXSTAF
     C*
     C*  HBA TRANS FILE
     C*
     C     HBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*  HBA TRANS FILE -- KEY FOR SRAMT SUBR
     C*
     C     HBKEY1        KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     HBKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    EOB(X)
     C*
     C*  CASE DETAIL FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C                   KFLD                    EOTRCD
     C                   KFLD                    EOEOB#
     C*
     C*  USER PARMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  OUTPUT ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*** KEY LIST FOR BD FILE NMBDLB
     C*
     C     BDKEY         KLIST
     C                   KFLD                    XXCODE
     C                   KFLD                    XYDATE            7 0
     C*
     C* KEY LIST FOR THE A/R TRANSACTION FILE
     C*
     C     ARKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#                         08/18/93 MOVED
     C                   KFLD                    XXINV#                         SUB# BEFORE INV
     C*
     C*** BEG ADD *** 11/17/99 ***************************************
     C*
     C     ARKEY1        KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#                                     ED
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C*
     C* KEY LIST FOR FUND AUXILIARY FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C*
     C*** END ADD *** 11/17/99 ***************************************
     C*
     C*
     C* KEY LIST RE-BILL WORK FILE
     C*
     C     EWKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    XXINV#
     C                   KFLD                    EOEOB#
     C*** BEG ADD *** 03/19/99 ***************************************
     C     EWKEY1        KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    XXINV#
     C                   KFLD                    EAEOB#
     C*** END ADD *** 03/19/99 ***************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQTYPE            1                REBILL ?
     C*** BEGIN CHG *** 01/04/99 **************************************
     C********             PARM           QQINV#  5        9/22/89 ADD
     C                   PARM                    QQINV#            9
     C*** END CHG *** 01/04/99 ****************************************
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* INITIALIZATION SECTION
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C*** BEGIN ADD *** 01/15/99 **************************************
     C     *LIKE         DEFINE    EORCN         XXRCN
     C*** END   ADD *** 01/15/99 **************************************
     C*** BEG ADD *** 01/06/99 ***************************************
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
     C**** Begin Add ***** 04/10/07 ***************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C**** End Add ******* 04/10/07 ***************************************
     C*** BEGIN ADD *** 08/03/06 **************************************
     C                   SETOFF                                       71
     C     CLTID         IFEQ      '139'
     C                   SETON                                        71
     C                   ENDIF
     C*** END   ADD *** 08/03/06 **************************************
     C*
     C*** END ADD *** 01/06/99 ***************************************
     C                   Z-ADD     *ZEROS        XHSYST           10 2          ADD 03/26/93
     C                   Z-ADD     *ZEROS        XPSYST           10 2          ADD 03/26/93
     C                   Z-ADD     *ZEROS        XUSYST           10 2          ADD 03/26/93
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   Z-ADD     *ZEROS        XMSYST           10 2
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   Z-ADD     *ZEROS        XASYST           10 2          ADD 03/26/93
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C*** BEGIN CHG *** 01/04/99 **************************************
     C********             MOVE QQINV#    XXINV#  50       9/22/89 ADD
     C                   MOVE      QQINV#        XXINV#            9 0
     C*** END CHG *** 01/04/99 ****************************************
     C                   MOVE      QQMODE        XXMODE            1
     C*
     C* IS THIS BEING CALLED IN PRINT MODE ONLY
     C*
     C     QQCALL        IFEQ      'P'
     C*
     C                   SETON                                        16
     C                   Z-ADD     0             XXINV#
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       16
     C*
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XYDATE
     C*** BEG ADD *** 11/17/99 *********************************
     C                   Z-ADD     XYDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XSDATE            8 0
     C*** END ADD *** 11/17/99 *********************************
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*  CHAIN TO USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*** GET ROUTING INFORMATION FOR REPORT
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C********           MOVE      '1'           XXPRT
     C********     ORKEY         CHAIN     POR01                              60
     C********     *IN60         IFEQ      '0'
     C********           EXCEPT    ROUTE1
     C********           END                                                    END IFEQ '0'
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C* * * GET COMPANY RECORD (CARRIER INFO)
     C*
     C     COKEY         CHAIN     PCO01                              51
     C     *IN51         IFEQ      '0'
     C*                    -----***REMOVE LEADING BLANKS FROM COMPANY NAME
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    CONM(Z)                            60
     C                   MOVEA     CONM(Z)       XXCONM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXCONM
     C                   END
     C*
     C*   GET THE SUBSCRIBER INFORMATION
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       XXSBNM           35
     C*** BEG ADD *** 11/17/99 **************************************
     C                   MOVE      *BLANKS       XXSAD1           35
     C                   MOVE      *BLANKS       XXSAD2           35
     C                   MOVE      *BLANKS       XXSCTZ           35
     C*** END ADD *** 11/17/99 **************************************
     C                   ELSE
     C                   MOVEL     SMNAME        XXSBNM           35
     C*** BEG ADD *** 11/17/99 **************************************
     C                   MOVEL     SMADR1        XXSAD1           35
     C                   MOVEL     SMADR2        XXSAD2           35
     C     XXSAD2        COMP      *BLANKS                                70
     C                   MOVEL     SMZIP         XXZIP1
     C                   MOVE      SMZIP         XXZIP2
     C     XXZIP2        IFEQ      '0000'
     C                   MOVE      ' '           XXZIPD
     C                   MOVE      *BLANKS       XXZIP2
     C                   ELSE
     C                   MOVE      '-'           XXZIPD
     C                   ENDIF
     C     SMCTY         CAT(P)    ',':0         XXSCTZ           35
     C                   CAT(P)    SMST:1        XXSCTZ
     C                   CAT(P)    XXZIP:2       XXSCTZ
     C*** END ADD *** 11/17/99 **************************************
     C     SMAR01        IFEQ      'A'
     C                   MOVE      '1'           *IN90
     C                   ELSE
     C     SMAR01        IFEQ      'B'
     C                   MOVE      '1'           *IN91
     C     SMAR02        IFEQ      'B'
     C                   MOVE      '1'           *IN93
     C                   ELSE
     C                   MOVE      '0'           *IN93
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C*** DETERMINE TYPE OF INVOICE - REBILL OR REGULAR INVOICING ...
     C*
     C     QQTYPE        IFEQ      'R'                                          REBILL 1 INV
     C                   MOVE      '1'           *IN97
     C                   ELSE                                                   ELSE EQ 'R'
     C                   MOVE      '0'           *IN97
     C                   END                                                    END EQ 'R'
     C*
     C*
     C*  DETERMINE IF THERE ARE ANY BILLS TO PROCESS
     C*
     C*** BEG ADD *** 03/19/99 ***************************************
     C*** IF PROVIDER BILLS IS INSTALLED THEN THE PROGRAM WILL USE
     C*** FAEAP AS THE MAIN FILE OTHER WISE IT WILL CONTINUE TO USE
     C*** FAEOP AS THE MAIN FILE.
     C****************************************************************
     C***************************************************************
     C*  PROVIDER BILLS PROCESS BEGINS HERE                  PBP
     C***************************************************************
     C     ##PRVB        IFEQ      'Y'                                          PROV. BILL INSTALLED
     C*
     C***************************************************************
     C*
     C                   READ(N)   PEA01                                  50
     C     *IN50         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C     QQTYPE        IFNE      'R'
     C                   EXCEPT    MSG
     C                   END
     C                   MOVE      '1'           *INLR
     C*
     C     QQTYPE        IFEQ      'R'
     C                   Z-ADD     *ZEROS        XXCRAM
     C     ARKEY         CHAIN     PAR02                              40
     C                   Z-ADD     ARFND         XXFND
     C                   Z-ADD     ARFYR         XXFYR
     C                   Z-ADD     ARFYR         XXYR
     C                   Z-ADD     ARCASE        XXCASE
     C                   EXSR      SRHEAD
     C                   EXCEPT    MSGHD
     C     *IN40         DOWEQ     '0'
     C     ARSTAT        IFEQ      'D'
     C     AREOB#        IFNE      XXEOB#
     C                   ADD       ARAMT         XXCRAM            9 2
     C                   Z-ADD     AREOB#        XXEOB#
     C*** BEGIN ADD *** 01/15/99 **************************************
     C                   Z-ADD     ARRCN         XXRCN
     C*** END   ADD *** 01/15/99 **************************************
     C                   EXCEPT    MSGDT
     C                   END
     C                   END
     C     ARKEY         READE     PAR02                                  40
     C                   END
     C                   EXCEPT    MSG2
     C                   END
     C*
     C*
     C                   GOTO      TERM2
     C                   END
     C*
     C*  SET-UP REMITTANCE ADDRESS
     C*
     C     C1KEY         CHAIN(N)  PC101                              60
     C     *IN60         IFEQ      '0'
     C*** BEGIN DEL *** 10/18/99 **************************************
     C**********           EXCPTRLSC1
     C*** END   DEL *** 10/18/99 **************************************
     C                   MOVE      C1ZIP         XSZIP
     C                   MOVE      C1NAME        XSNAME
     C*
     C*** BEG ADD *** 11/17/99 **************************************
     C                   MOVEL     C1NAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM      C1NAME        ##DATA           80
     C                   PARM      40            ##LGTH            3 0
     C                   MOVEL     ##DATA        XPNAM            40
     C*** END ADD *** 11/17/99 **************************************
     C*
     C                   MOVEL     '('           XCPHN#
     C                   MOVE      C1AREA        XXAREA
     C                   MOVE      ')'           XXCPRN
     C                   MOVEL     C1PHN#        XXPH1
     C                   MOVE      C1PHN#        XXPH2
     C                   MOVE      C1FEIN        XXFEIN            9 0
     C                   MOVE      '-'           XXHP1
     C                   MOVE      '-'           XFHYP1
     C                   MOVE      '-'           XFHYP2
     C                   MOVE      '-'           XFHYP3
     C  N97              MOVE      C1LIN#        XXINV#                         LAST INV# USED
     C   97              MOVE      EABINV        XXINV#                         USE SAME INV#
     C*** BEG ADD *** 11/17/99 ****************************************
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C*
     C     F1KY          CHAIN     PF101                              98
     C*
     C     *IN98         IFEQ      *OFF
     C     F1FLG1        ANDEQ     'Y'
     C                   MOVEL(P)  F1ANME        XCNAME
     C                   MOVEL(P)  F1AAD1        XCADR1
     C                   MOVEL     F1AAD1        XCADR1           40
     C                   MOVEL(P)  F1AAD2        XCADR2
     C*
     C                   MOVEL     F1AST         XXST             13
     C                   MOVEA     XXST          SZ
     C*** BEGIN CHG *** 04/13/04 **************************************
     C**********           MOVE F1AZIP    XXZP1   5
     C                   MOVEL     F1AZIP        XXZP1             5
     C*** END   CHG *** 04/13/04 **************************************
     C                   MOVEA     XXZP1         SZ(4)
     C***        S1ZPP2    IFNE 0
     C***                  MOVE '-'       SZ,9
     C***                  MOVE S1ZPP2    XXZP2   4
     C***                  MOVEAXXZP2     SZ,10
     C***                  END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     F1ACTY        XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C*
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XCCSZ            40
     C*** BEGIN ADD *** 08/24/04 **************************************
     C                   MOVEL     '('           XCPHN#
     C                   MOVE      F1AARA        XXAREA
     C                   MOVE      ')'           XXCPRN
     C                   MOVEL     F1APHN        XXPH1
     C                   MOVE      F1APHN        XXPH2
     C                   MOVE      '-'           XXHP1
     C*** END   ADD *** 08/24/04 **************************************
     C*** BEGIN ADD *** 03/10/05 **************************************
     C     F1AFEI        IFNE      *ZEROS
     C                   Z-ADD     F1AFEI        XXFEIN
     C                   ENDIF
     C*** END   ADD *** 03/10/05 **************************************
     C*
     C***                  MOVELF1AARA    XXAREA
     C***                  MOVELF1APHN    XXPHN1  3
     C***                  MOVE F1APHN    XXPHN2  4
     C                   ELSE                                                   F1FLG1
     C*** END ADD *** 11/17/99 ****************************************
     C*
     C*** REMOVE LEADING BLANKS FROM REMIT TO NAME
     C*
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    XCNM(Z)                            60
     C     *IN60         IFEQ      '1'
     C                   MOVEA     XCNM(Z)       XCNAME           40
     C                   ELSE
     C                   MOVE      *BLANKS       XCNAME
     C                   END
     C*
     C                   MOVEL     C1ADR1        XCADR1           40
     C**** BEG CHG *** 11/17/99 ************************
     C                   MOVEL     C1ADR2        XCADR2           40
     C*
     C                   MOVEL     C1ST          XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      S1ZPP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     S1ZPP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      S1ZPP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C**** END CHG *** 11/17/99 ************************
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     C1ST          XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      S1ZPP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     S1ZPP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      S1ZPP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     C1CTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XCCSZ            40
     C                   MOVEL     'FEE SCHD'    XX13A            13
     C                   MOVE      'USER'        XX13A
     C                   ENDIF                                                  ADD 11/17/99
     C                   END
     C*
     C*  DETERMINE DUE DATE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXDATE            8 0
     C                   MOVE      XXDATE        SRDATE            8 0
     C*** BEGIN CHG *** 08/03/2006 ************************************
     C**********           Z-ADD30        SRFUTR  50
     C     *IN71         IFEQ      *ON
     C                   Z-ADD     5             SRFUTR            5 0
     C                   ELSE
     C                   Z-ADD     30            SRFUTR            5 0
     C                   ENDIF
     C*** END   CHG *** 08/03/2006 ************************************
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD            1
     C*
     C                   Z-ADD     SRDATE        XDUEDT            8 0          ADD 11/17/99
     C*** BEG DEL *** 11/17/99 **************************************
     C**********           Z-ADDSRDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XDUEDT  60
     C*** END DEL *** 11/17/99 **************************************
     C*
     C*** GET ROUTING INFORMATION FOR BILLS
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C*******     *IN91         IFEQ      '1'
     C*******     *IN93         IFEQ      '1'
     C*******            MOVE      '0'           XXPRT
     C*******     ORKEY         CHAIN     POR01                              60
     C*******     *IN60         IFEQ      '0'
     C*******                   EXCEPT    ROUTE
     C*******                   END                                                    END IFEQ '0'
     C*******            MOVE      '2'           XXPRT
     C*******     ORKEY         CHAIN     POR01                              60
     C*******     *IN60         IFEQ      '0'
     C*******            EXCEPT    ROUTE2
     C*******            END                                                    END IN60=0
     C*******            ELSE                                                   ELSE 93 =0
     C*******            MOVE      '2'           XXPRT
     C*******     ORKEY         CHAIN     POR01                              60
     C*******    *IN60         IFEQ      '0'
     C*******            EXCEPT    ROUTE2
     C*******            END                                                    END IN60=0
     C*******            END                                                    END *IN93
     C*******            ELSE                                                   ELSE IN91=0
     C*******            MOVE      '0'           XXPRT
     C*******     ORKEY         CHAIN     POR01                              60
     C*******     *IN60         IFEQ      '0'
     C*******            EXCEPT    ROUTE
     C*******            END                                                    END IFEQ '0'
     C*******            END                                                    END IN91
      ***** End Del ***** 08/26/14 *********************************************
     C                   MOVE      *BLANKS       XXFFLG            1
     C*
     C*  PRINT REPORT HEADING
     C*
     C                   EXCEPT    HEAD1
     C*
     C*** READ EOB FILE
     C*
     C     *LOVAL        SETLL     PEA01
     C                   READ      PEA01                                  50
     C*
     C     *IN50         IFEQ      '0'
     C                   Z-ADD     EACO#         XXCO#             3 0
     C                   Z-ADD     EASUB#        XXSUB#            5 0
     C                   Z-ADD     EAFND         XXFND             3 0
     C                   Z-ADD     EAFYR         XXFYR             3 0
     C                   Z-ADD     EAFYR         XXYR
     C                   Z-ADD     EACASE        XXCASE            7 0
     C*
     C                   EXSR      SRHEAD                                       ADD 02/03/95
     C*
     C*** VERIFY INVOICE AMT GREATER THAN ZERO
     C                   EXSR      SRAMT
     C*
     C     *IN91         IFEQ      '1'
     C     XXFFLG        IFEQ      *BLANKS
     C  N97              ADD       1             XXINV#
     C                   MOVE      'N'           XXFFLG
     C                   END
     C                   ELSE
     C  N97              ADD       1             XXINV#
     C                   END
     C     *IN91         IFEQ      '1'
     C     *IN93         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   Z-ADD     0             PAGE
     C                   EXCEPT    HEADS
     C                   ELSE
     C                   EXCEPT    HEADS
     C                   END
     C                   ELSE
     C                   EXCEPT    HEAD
     C                   END
     C                   EXCEPT    HEAD3
     C                   END
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C*
     C*** VERIFY INVOICE AMT GREATER THAN ZERO
     C                   EXSR      SRAMT
     C*
     C*
     C                   Z-ADD     EAEOB#        XXEOB#
     C                   Z-ADD     EARCN         XXRCN
     C     EACASE        IFEQ      XXCASE
     C*** BEGIN ADD *** 09/23/99 **************************************
     C     EATMFL        ANDEQ     'Y'
     C*** END   ADD *** 09/23/99 **************************************
???  C* * * * * *EOFLG5 * *ANDEQ'Y' * * * * * * * * * * * * * * * * *
     C                   Z-ADD     1             Q                 3 0
     C     *ZEROS        LOOKUP    EOB(Q)                                 60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     EAEOB#        EOB(Q)
     C*** BEGIN ADD *** 01/15/99 **************************************
     C                   Z-ADD     EARCN         RCN(Q)
     C*** END   ADD *** 01/15/99 **************************************
     C                   END
     C                   END
     C*
     C*** IF CASE CHANGES, PRINT CASE TOTALS
     C*
     C     EACASE        IFNE      XXCASE
     C     XXCASE        ANDNE     *ZEROS
     C                   EXSR      SRCASE
     C                   Z-ADD     EAFYR         XXFYR
     C                   EXSR      SRHEAD
     C     *IN91         IFEQ      '1'
     C     *IN93         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   END
     C                   ELSE
     C                   EXCEPT    HEAD
     C                   END
     C*** BEGIN ADD *** 09/23/99 **************************************
     C     EATMFL        IFEQ      'Y'
     C*** END   ADD *** 09/23/99 **************************************
???  C* * * * * *EOFLG5 * *IFEQ 'Y' * * * * * * * * * * * * * * * * *
     C                   Z-ADD     1             Q                 3 0
     C     *ZEROS        LOOKUP    EOB(Q)                                 60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     EAEOB#        EOB(Q)
     C*** BEGIN ADD *** 01/15/99***************************************
     C                   Z-ADD     EARCN         RCN(Q)
     C*** END * ADD *** 01/75/99***************************************
     C                   END
     C                   END
     C                   END                                                    ENDNE XXCASE
     C*
     C*
     C*** IF GROUP CHANGES, PRINT GROUP TOTALS, PAGE BREAK HEADINGS
     C*
     C     EAFND         IFNE      XXFND
     C     XXFND         ANDNE     *ZEROS
     C                   EXSR      SRGRP
     C                   EXCEPT    HEAD1
     C                   EXSR      SRHEAD
     C     *IN91         IFEQ      '1'
     C     *IN93         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   END
     C                   ELSE
     C                   EXCEPT    HEAD
     C                   END
     C                   EXCEPT    HEAD3
     C                   END                                                    ENDNE XXFND
     C*
     C*** DETERMINE IF CASE IS HBA OR PBR
     C*
     C     EASYS         IFEQ      'H'
     C                   MOVE      '1'           *IN99                          HBA
     C                   MOVE      '0'           *IN94                          CERT
     C                   ELSE
     C     EASYS         IFEQ      'P'
     C                   MOVE      '0'           *IN99                          PBR
     C                   MOVE      '0'           *IN94                          CERT
     C                   ELSE
     C     EASYS         IFEQ      'U'
     C                   MOVE      '1'           *IN94                          CERT
     C                   MOVE      '0'           *IN99                          PBR/HBA
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   ELSE
     C     EASYS         IFEQ      'M'
     C                   MOVE      '1'           *IN94
     C                   MOVE      '1'           *IN99
     C                   END
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   END
     C                   END
     C                   END
     C***
     C     EOKEY1        CHAIN(N)  PEO01                              60
     C***
     C*
     C     EASYS         IFEQ      'P'
     C*
     C     EOKEY1        CHAIN(N)  PEO01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EO#BLL        XX#BLL
     C                   Z-ADD     EOFRDT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXFRDT
     C                   Z-ADD     EOTODT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXTODT
     C                   ENDIF                                                  *IN60 = 0
     C*
     C                   Z-ADD     EATRDT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXTRDT
     C                   MOVE      *BLANKS       XXDESC                         PBR DESC
     C                   MOVEL     DS(2)         XXDESC                         PBR DESC
     C                   ADD       1             XX#CPB            4 0          CASE # PBR
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     ##RHAB        IFEQ      'Y'
     C*
     C                   EXSR      TMCALC
     C*
     C                   ELSE
     C*
     C*** END   ADD *** 10/25/99 **************************************
     C                   Z-ADD     EABAMT        XXAMT
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*
     C                   ADD       XXAMT         XXCPBR           10 2          CASE BILLD
     C*
     C                   ENDIF                                                  EOSYS = P
     C*
     C     EASYS         IFEQ      'H'
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     ##RHAB        IFEQ      'Y'
     C*
     C                   EXSR      TMCALC
     C*
     C                   ELSE
     C*
     C*** END   ADD *** 10/25/99 **************************************
     C                   Z-ADD     EABAMT        XXAMT             9 2          DETL BILLD
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*** BEGIN CHG *** 10/25/99 **************************************
     C**********           ADD  EABAMT    XXCHBA 102       CASE BILLD
     C                   ADD       XXAMT         XXCHBA           10 2          CASE BILLD
     C*** END   CHG *** 10/25/99 **************************************
     C                   Z-ADD     EOFRDT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C******************   MOVELSRDTO     XXFRDT  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDT            8 0
     C*** END CHG *** 11/17/99 ***************************
     C                   Z-ADD     EOTODT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C******************   MOVELSRDTO     XXTODT  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODT            8 0
     C*** END CHG *** 11/17/99 ***************************
     C                   Z-ADD     EATRDT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C******************   MOVELSRDTO     XXTRDT  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*** END CHG *** 11/17/99 ***************************
     C*
     C*** BEG CHG *** 04/13/99 ***************************************
     C                   MOVE      *BLANKS       XXDESC           20            PBR DESC
     C**********           MOVE *BLANKS   XXDESC 30        PBR DESC
     C*** END CHG *** 04/13/99 ***************************************
     C                   MOVEL     DS(1)         XXDESC                         HBA DESC
     C                   ADD       1             XX#CHB            4 0          CASE # HBA
     C*
     C                   Z-ADD     *ZEROS        XX#BLL            3 0          # OF BILLS
     C*
     C                   ENDIF                                                  EASYS = H
     C*
     C     EASYS         IFEQ      'U'
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     ##RHAB        IFEQ      'Y'
     C*
     C                   EXSR      TMCALC
     C*
     C                   ELSE
     C*
     C*** END   ADD *** 10/25/99 **************************************
     C                   Z-ADD     EABAMT        XXAMT             9 2          DETL BILLD
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*** BEGIN CHG *** 10/25/99 **************************************
     C**********           ADD  EABAMT    XXCCRT 102       CASE BILLD
     C                   ADD       XXAMT         XXCCRT           10 2          CASE BILLD
     C*** END   CHG *** 10/25/99 **************************************
     C                   Z-ADD     EOFRDT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXFRDT
     C                   Z-ADD     EOTODT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXTODT
     C                   Z-ADD     EATRDT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXTRDT
     C*
     C                   MOVE      *BLANKS       XXDESC                         PBR DESC
     C                   MOVEL     DS(5)         XXDESC                         HBA DESC
     C                   ADD       1             XX#CCT            4 0          CASE # HBA
     C*
     C                   Z-ADD     *ZEROS        XX#BLL            3 0          # OF BILLS
     C*
     C                   ENDIF                                                  EASYS = U
     C*
     C     EASYS         IFEQ      'M'
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     ##RHAB        IFEQ      'Y'
     C*
     C                   EXSR      TMCALC
     C*
     C                   ELSE
     C*
     C*** END   ADD *** 10/25/99 **************************************
     C                   Z-ADD     EABAMT        XXAMT             9 2          DETL BILLD
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*** BEGIN CHG *** 10/25/99 **************************************
     C**********           ADD  EABAMT    XXCMED 102       CASE BILLD
     C                   ADD       XXAMT         XXCMED           10 2          CASE BILLD
     C*** END   CHG *** 10/25/99 **************************************
     C                   Z-ADD     EOFRDT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXFRDT
     C                   Z-ADD     EOTODT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXTODT
     C                   Z-ADD     EATRDT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXTRDT
     C*
     C                   MOVE      *BLANKS       XXDESC                         PBR DESC
     C                   MOVEL     DS(6)         XXDESC                         HBA DESC
     C                   ADD       1             XX#CMD            4 0          CASE # HBA
     C*
     C                   Z-ADD     *ZEROS        XX#BLL            3 0          # OF BILLS
     C*
     C                   ENDIF                                                  EASYS = M
     C*
     C*** COUNT NUMBER OF EOBS PER CASE AND GROUP ...
     C*
     C                   ADD       1             XX#EOB            3 0          # OF EOBS/CASE
     C                   ADD       1             XXGEOB            4 0          # OF EOBS/GROUP
     C*
     C*
     C* * * GET PROVIDER INFORMATION.
     C*
     C     PDKEY1        CHAIN     PPD01                              50
     C     *IN50         IFEQ      '0'
     C*
     C*
     C*** BEG CHG *** 04/13/99 ***************************************
     C                   MOVEL     PDNAME        XXPRNM           17
     C**********           MOVELPDNAME    XXPRNM 18
     C*** END CHG *** 04/13/99 ***************************************
     C                   MOVE      EAFEIN        XXPRV#            9 0
     C                   MOVE      EAMOD         XXMOD             3 0
     C                   ELSE
     C                   MOVE      *BLANKS       XXPRNM
     C                   MOVE      EAFEIN        XXPRV#
     C                   MOVE      EAMOD         XXMOD
     C                   END
     C*
     C     XXMOD         IFNE      *ZEROS
     C                   MOVE      '1'           *IN96                          NOT BLANK
     C                   ELSE
     C                   MOVE      '0'           *IN96                          BLANK
     C                   END
     C*
     C     EOKEY1        CHAIN(N)  PEO01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     EOPNAM        XXPRNM                         PROVIDER NAME  A
     C                   ENDIF
     C*
     C***  OUTPUT BILL - DETAIL LINE
     C*
     C     *IN91         IFEQ      '1'
     C     *IN93         IFEQ      '1'
     C     XXLINE        IFGT      40
     C                   EXCEPT    HEAD
     C                   END
     C                   EXCEPT    DET
     C                   END
     C                   ELSE
     C     XXLINE        IFGT      40
     C                   EXCEPT    HEAD
     C                   END
     C                   EXCEPT    DET
     C                   END
     C*
     C***  OUTPUT REPORT - DETAIL LINE
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    DETL1
     C*
     C*** IF BILLING IS PERFORMED, CREATE A/R RECORD.
     C*
     C                   EXSR      SRAR
     C*
     C*** BEGIN DEL *** 09/23/99 **************************************
     C**********           MOVE 'P'       EABBLF
     C**********           Z-ADDXXINV#    EABINV
     C**********           MOVE Q1PGM     EAPGM
     C**********           MOVE Q1USER    EAUSER
     C**********           MOVE Q1PGM     EAPGM
     C**********           Z-ADDDSTIME    EATIME
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     EADT
     C**********           MOVELSRDTO     EABIDT
     C*** END   DEL *** 09/23/99 **************************************
     C*** BEGIN ADD *** 09/23/99 **************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBIDT            7 0
     C*
     C                   MOVEL     *BLANKS       KKWRUP
     C                   MOVEL     'U'           XXFAEA
     C*
     C     CMKEY         CHAIN(N)  PCM01                              55
     C*
     C     *IN55         IFEQ      *OFF
     C*
     C                   Z-ADD     DBRRN         ##RRN
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        ##RRN
     C*
     C                   ENDIF                                                  *IN55 = OFF
     C*
     C  N16              CALL      'AA001R'
     C                   PARM                    ##RRN             9 0          RRN OF CASE
     C                   PARM      XXEOB#        ##EOB#            9 0          EOB #
     C                   PARM      XXRCN         ##RCN             3 0          RECON #
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM                    ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM      'P'           ##BBLF            1            BRS INV PROC FLAG
     C                   PARM      XXINV#        ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EASYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C                   PARM      *BLANKS       ##CERT           15            PRE-CERT
     C                   PARM      *ZEROS        ##EXT             3 0          EXTENSION
     C                   PARM      *ZEROS        ##FEIN            9 0          PROVIDER FEIN
     C                   PARM      *ZEROS        ##MOD             3 0          PROVIDER MOD
     C                   PARM      XXBIDT        ##BIDT            7 0          BRS INV DT
     C*** END   ADD *** 09/23/99 **************************************
     C*
     C*** UPDATE NDTMP WITH INV#
     C*
     C     *IN16         IFEQ      *OFF
     C*
     C     TMKY1         CHAIN     PTM01                              55
     C     *IN55         DOWEQ     *OFF
     C                   MOVE      Q1PGM         TMPGM
     C                   MOVE      Q1USER        TMUSER
     C                   MOVE      Q1PGM         TMPGM
     C                   Z-ADD     DSTIME        TMTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TMDT
     C                   Z-ADD     XXINV#        TMINV#
     C                   UPDATE    PTM01
     C                   ENDDO                                                  *IN55 = OFF
     C*
     C                   ENDIF
     C*
     C*** UPDATE EOB TRANS RECORD
     C*
     C                   Z-ADD     1             T                 5 0
     C     ' '           LOOKUP    ESB(T)                                 60
     C     *IN60         IFEQ      '1'
     C   99              MOVE      'H'           ESB(T)
     C  N99              MOVE      'P'           ESB(T)
     C                   ELSE
     C                   MOVE      *BLANK        ESB(T)
     C                   END
     C***
     C*
     C*** BEGIN DEL *** 09/23/99 **************************************
     C**********           UPDATPEA01
     C*** END   DEL *** 09/23/99 **************************************
     C     READN1        TAG
     C                   READ      PEA01                                  50
     C*
     C                   ENDDO                                                  END DOWEQ 50
     C***************************************************************
     C*  BILL ENTRY PROCESS BEGINS HERE                  BEP
     C***************************************************************
     C                   ELSE                                                   PROV. BILL NOT INST.
     C*
     C  N16              READ      PEO01                                  50
     C   16              READ(N)   PEO01                                  50
     C     *IN50         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C     QQTYPE        IFNE      'R'
     C                   EXCEPT    MSG
     C                   END
     C                   MOVE      '1'           *INLR
     C*
     C     QQTYPE        IFEQ      'R'
     C                   Z-ADD     *ZEROS        XXCRAM
     C     ARKEY         CHAIN     PAR02                              40
     C                   Z-ADD     ARFND         XXFND
     C                   Z-ADD     ARFYR         XXFYR
     C                   Z-ADD     ARFYR         XXYR
     C                   Z-ADD     ARCASE        XXCASE
     C                   EXSR      SRHEAD
     C                   EXCEPT    MSGHD
     C     *IN40         DOWEQ     '0'
     C     ARSTAT        IFEQ      'D'
     C     AREOB#        IFNE      XXEOB#
     C                   ADD       ARAMT         XXCRAM            9 2
     C                   Z-ADD     AREOB#        XXEOB#
     C*** BEGIN ADD *** 01/15/99 **************************************
     C                   Z-ADD     ARRCN         XXRCN
     C*** END   ADD *** 01/15/99 **************************************
     C                   EXCEPT    MSGDT
     C                   END
     C                   END
     C     ARKEY         READE     PAR02                                  40
     C                   END
     C                   EXCEPT    MSG2
     C                   END
     C*
     C*
     C                   GOTO      TERM2
     C                   END
     C*
     C*  SET-UP REMITTANCE ADDRESS
     C*
     C*
     C     C1KEY         CHAIN(N)  PC101                              60
     C     *IN60         IFEQ      '0'
     C*** BEGIN DEL *** 10/18/99 **************************************
     C**********           EXCPTRLSC1
     C*** END   DEL *** 10/18/99 **************************************
     C                   MOVE      C1ZIP         XSZIP
     C                   MOVE      C1NAME        XSNAME
     C                   MOVEL     '('           XCPHN#
     C                   MOVE      C1AREA        XXAREA
     C                   MOVE      ')'           XXCPRN
     C                   MOVEL     C1PHN#        XXPH1
     C                   MOVE      C1PHN#        XXPH2
     C                   MOVE      C1FEIN        XXFEIN            9 0
     C                   MOVE      '-'           XXHP1
     C                   MOVE      '-'           XFHYP1
     C                   MOVE      '-'           XFHYP2
     C                   MOVE      '-'           XFHYP3
     C*** BEGIN CHG *** 01/04/99 **************************************
     C**N97***             MOVE C1LIN#    XXINV#  50       LAST INV# USED
     C  N97
     CANN16              MOVE      C1LIN#        XXINV#                         LAST INV# USED
     C   16              ADD       1             XXINV#
     C*** END CHG *** 01/04/99 ****************************************
     C   97              MOVE      EOINV#        XXINV#                         USE SAME INV#
     C*
     C*
     C*** REMOVE LEADING BLANKS FROM REMIT TO NAME
     C*
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    XCNM(Z)                            60
     C     *IN60         IFEQ      '1'
     C                   MOVEA     XCNM(Z)       XCNAME           40
     C                   ELSE
     C                   MOVE      *BLANKS       XCNAME
     C                   END
     C*
     C                   MOVEL     C1ADR1        XCADR1           40
     C                   MOVEL     C1ADR2        XCADR2           40
     C*** BEG ADD *** 11/17/99 **************************************
     C     XCADR1        CAT(P)    ',':0         XCADRC           50
     C                   CAT(P)    XCADR2:1      XCADRC
     C*** END ADD *** 11/17/99 **************************************
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     C1ST          XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      S1ZPP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     S1ZPP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      S1ZPP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     C1CTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XCCSZ            40
     C                   MOVEL     'FEE SCHD'    XX13A            13
     C                   MOVE      'USER'        XX13A
     C                   END
     C*
     C*  DETERMINE DUE DATE
     C*
     C****                 MOVE DSDATE    XXDATE  60       DEL 11/11/97
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXDATE            8 0
     C                   MOVE      XXDATE        SRDATE            8 0
     C*** BEGIN CHG *** 08/03/2006 ************************************
     C**********           Z-ADD30        SRFUTR  50
     C     *IN71         IFEQ      *ON
     C                   Z-ADD     5             SRFUTR            5 0
     C                   ELSE
     C                   Z-ADD     30            SRFUTR            5 0
     C                   ENDIF
     C*** END   CHG *** 08/03/2006 ************************************
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD            1
     C*
     C                   Z-ADD     SRDATE        XDUEDT                         ADD 11/17/99
     C*** BEG DEL *** 11/17/99 **************************************
     C**********           Z-ADDSRDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XDUEDT  60
     C*** END DEL *** 11/17/99 **************************************
     C*
     C*** GET ROUTING INFORMATION FOR BILLS
     C*
     C     *IN91         IFEQ      '1'
     C     *IN93         IFEQ      '1'
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    ROUTE
     C                   END                                                    END IFEQ '0'
     C                   MOVE      '2'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    ROUTE2
     C                   END                                                    END IN60=0
     C                   ELSE                                                   ELSE 93 =0
     C                   MOVE      '2'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    ROUTE2
     C                   END                                                    END IN60=0
     C                   END                                                    END *IN93
     C                   ELSE                                                   ELSE IN91=0
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    ROUTE
     C                   END                                                    END IFEQ '0'
     C                   END                                                    END IN91
     C                   MOVE      *BLANKS       XXFFLG            1
     C*
     C*  PRINT REPORT HEADING
     C*
     C                   EXCEPT    HEAD1
     C*
     C*** READ EOB FILE
     C*
     C     *LOVAL        SETLL     PEO01
     C  N16              READ      PEO01                                  50
     C   16              READ(N)   PEO01                                  50
     C*
     C     *IN50         IFEQ      '0'
     C                   Z-ADD     EOCO#         XXCO#             3 0
     C                   Z-ADD     EOSUB#        XXSUB#            5 0
     C                   Z-ADD     EOFND         XXFND             3 0
     C                   Z-ADD     EOFYR         XXFYR             3 0
     C                   Z-ADD     EOFYR         XXYR
     C                   Z-ADD     EOCASE        XXCASE            7 0
     C*
     C                   EXSR      SRHEAD                                       ADD 02/03/95
     C*
     C*** VERIFY INVOICE AMT GREATER THAN ZERO
     C                   EXSR      SRAMT
     C*
     C     *IN91         IFEQ      '1'
     C     XXFFLG        IFEQ      *BLANKS
     C  N97              ADD       1             XXINV#
     C                   MOVE      'N'           XXFFLG
     C                   END
     C                   ELSE
     C  N97              ADD       1             XXINV#
     C                   END
     C     *IN91         IFEQ      '1'
     C     *IN93         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   Z-ADD     0             PAGE
     C                   EXCEPT    HEADS
     C                   ELSE
     C                   EXCEPT    HEADS
     C                   END
     C                   ELSE
     C                   EXCEPT    HEAD
     C                   END
     C                   EXCEPT    HEAD3
     C                   END
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C*
     C*** VERIFY INVOICE AMT GREATER THAN ZERO
     C                   EXSR      SRAMT
     C*
     C*
     C                   Z-ADD     EOEOB#        XXEOB#
     C*** BEGIN ADD *** 01/15/99 **************************************
     C                   Z-ADD     EORCN         XXRCN
     C*** END   ADD *** 01/15/99 **************************************
     C     EOCASE        IFEQ      XXCASE
     C     EOFLG5        ANDEQ     'Y'
     C                   Z-ADD     1             Q                 3 0
     C     *ZEROS        LOOKUP    EOB(Q)                                 60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     EOEOB#        EOB(Q)
     C*** BEGIN ADD *** 01/15/99 **************************************
     C                   Z-ADD     EORCN         RCN(Q)
     C*** END   ADD *** 01/15/99 **************************************
     C                   END
     C                   END
     C*
     C*** IF CASE CHANGES, PRINT CASE TOTALS
     C*
     C     EOCASE        IFNE      XXCASE
     C     XXCASE        ANDNE     *ZEROS
     C                   EXSR      SRCASE
     C                   Z-ADD     EOFYR         XXFYR
     C                   EXSR      SRHEAD
     C     *IN91         IFEQ      '1'
     C     *IN93         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   END
     C                   ELSE
     C                   EXCEPT    HEAD
     C                   END
     C     EOFLG5        IFEQ      'Y'
     C                   Z-ADD     1             Q                 3 0
     C     *ZEROS        LOOKUP    EOB(Q)                                 60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     EOEOB#        EOB(Q)
     C*** BEGIN ADD *** 01/15/99 **************************************
     C                   Z-ADD     EORCN         RCN(Q)
     C*** END   ADD *** 01/15/99 **************************************
     C                   END
     C                   END
     C                   END                                                    ENDNE XXCASE
     C*
     C*** IF GROUP CHANGES, PRINT GROUP TOTALS, PAGE BREAK HEADINGS
     C*
     C     EOFND         IFNE      XXFND
     C     XXFND         ANDNE     *ZEROS
     C                   EXSR      SRGRP
     C                   EXCEPT    HEAD1
     C                   EXSR      SRHEAD
     C     *IN91         IFEQ      '1'
     C     *IN93         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   END
     C                   ELSE
     C                   EXCEPT    HEAD
     C                   END
     C                   EXCEPT    HEAD3
     C                   END                                                    ENDNE XXFND
     C*
     C*** DETERMINE IF CASE IS HBA OR PBR
     C*
     C     EOSYS         IFEQ      'H'
     C                   MOVE      '1'           *IN99                          HBA
     C                   MOVE      '0'           *IN94                          CERT
     C                   ELSE
     C     EOSYS         IFEQ      'P'
     C                   MOVE      '0'           *IN99                          PBR
     C                   MOVE      '0'           *IN94                          CERT
     C                   ELSE
     C     EOSYS         IFEQ      'U'
     C                   MOVE      '1'           *IN94                          CERT
     C                   MOVE      '0'           *IN99                          PBR/HBA
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   ELSE
     C     EOSYS         IFEQ      'M'
     C                   MOVE      '1'           *IN94
     C                   MOVE      '1'           *IN99
     C                   END
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   END
     C                   END
     C                   END
     C*
     C     EOSYS         IFEQ      'P'
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXFRDT
     C                   Z-ADD     EOTODT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXTODT
     C                   Z-ADD     EOTRDT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXTRDT
     C                   MOVE      *BLANKS       XXDESC                         PBR DESC
     C                   MOVEL     DS(2)         XXDESC                         PBR DESC
     C                   ADD       1             XX#CPB            4 0          CASE # PBR
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     ##RHAB        IFEQ      'Y'
     C*
     C                   EXSR      TMCALC
     C*
     C                   ELSE
     C*
     C*** END   ADD *** 10/25/99 **************************************
     C                   Z-ADD     EOBAMT        XXAMT
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*
     C                   Z-ADD     EO#BLL        XX#BLL                         # OF BILLS
     C*
     C                   ADD       XXAMT         XXCPBR           10 2          CASE BILLD
     C*
     C                   ENDIF                                                  EOSYS = P
     C*
     C     *IN99         IFEQ      '1'
     C     *IN94         ANDEQ     '0'                                          ADD 12/08/92
     C     HBKEY         CHAIN     PHB01                              55
     C*
     C     *IN55         IFEQ      '0'
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     ##RHAB        IFEQ      'Y'
     C*
     C                   EXSR      TMCALC
     C*
     C                   ELSE
     C*
     C*** END   ADD *** 10/25/99 **************************************
     C                   Z-ADD     HB1AAM        XXAMT             9 2          DETL BILLD
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*** BEGIN CHG *** 10/25/99 **************************************
     C**********           ADD  HB1AAM    XXCHBA 102       CASE BILLD
     C                   ADD       XXAMT         XXCHBA           10 2          CASE BILLD
     C*** END   CHG *** 10/25/99 **************************************
     C                   Z-ADD     HBFRDT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXFRDT
     C                   Z-ADD     HBTODT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXTODT
     C                   Z-ADD     HBTRDT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXTRDT
     C                   ELSE                                                   ELS IFEQ '0'
     C                   Z-ADD     0             XXAMT                          DETL BILLD
     C                   Z-ADD     0             XXFRDT
     C                   Z-ADD     0             XXTODT
     C                   END                                                    END IFEQ '0'
     C*
     C                   MOVE      *BLANKS       XXDESC                         PBR DESC
     C                   MOVEL     DS(1)         XXDESC                         HBA DESC
     C                   ADD       1             XX#CHB            4 0          CASE # HBA
     C*
     C*
     C                   Z-ADD     *ZEROS        XX#BLL            3 0          # OF BILLS
     C*
     C                   ELSE                                                   ELSE *IN99
     C     *IN99         IFEQ      '0'
     C     *IN94         ANDEQ     '0'
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXFRDT
     C                   Z-ADD     EOTODT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXTODT
     C                   Z-ADD     EOTRDT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXTRDT
     C                   MOVE      *BLANKS       XXDESC                         PBR DESC
     C                   MOVEL     DS(2)         XXDESC                         PBR DESC
     C                   ADD       1             XX#CPB            4 0          CASE # PBR
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     ##RHAB        IFEQ      'Y'
     C*
     C                   EXSR      TMCALC
     C*
     C                   ELSE
     C*
     C*** END   ADD *** 10/25/99 **************************************
     C                   Z-ADD     EOBAMT        XXAMT
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*
     C*
     C                   Z-ADD     EO#BLL        XX#BLL                         # OF BILLS
     C*
     C                   ADD       XXAMT         XXCPBR           10 2          CASE BILLD
     C*
     C*
     C                   ELSE
     C*
     C*** IF PRE/RE-CERT CASE, GET PRE/RE-CERT RECORD
     C*
     C     *IN94         IFEQ      '1'
     C     *IN99         ANDEQ     '0'                                          ADD 12/08/92
     C     PBKEY         CHAIN     PPB01                              55
     C*
     C     *IN55         IFEQ      '0'
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     ##RHAB        IFEQ      'Y'
     C*
     C                   EXSR      TMCALC
     C*
     C                   ELSE
     C*
     C*** END   ADD *** 10/25/99 **************************************
     C                   Z-ADD     PBBAMT        XXAMT             9 2          DETL BILLD
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*** BEGIN CHG *** 10/25/99 **************************************
     C**********           ADD  PBBAMT    XXCCRT 102       CASE BILLD
     C                   ADD       XXAMT         XXCCRT           10 2          CASE BILLD
     C*** END   CHG *** 10/25/99 **************************************
     C                   Z-ADD     EOFRDT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXFRDT
     C                   Z-ADD     EOTODT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXTODT
     C                   Z-ADD     PBTRDT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXTRDT
     C                   ELSE                                                   ELS IFEQ '0'
     C                   Z-ADD     0             XXAMT                          DETL BILLD
     C                   Z-ADD     0             XXFRDT
     C                   Z-ADD     0             XXTODT
     C                   END                                                    END IFEQ '0'
     C*
     C                   MOVE      *BLANKS       XXDESC                         PBR DESC
     C                   MOVEL     DS(5)         XXDESC                         CRT DESC
     C                   ADD       1             XX#CCT            4 0          CASE # CRT
     C*
     C*
     C                   Z-ADD     *ZEROS        XX#BLL            3 0          # OF BILLS
     C*
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   ELSE
     C*
     C*** IF MED MANAGEMENT
     C*
     C     *IN94         IFEQ      '1'
     C     *IN99         ANDEQ     '1'
     C     MBKEY         CHAIN     PMB01                              55
     C*
     C     *IN55         IFEQ      '0'
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     ##RHAB        IFEQ      'Y'
     C*
     C                   EXSR      TMCALC
     C*
     C                   ELSE
     C*
     C*** END   ADD *** 10/25/99 **************************************
     C                   Z-ADD     MBBAMT        XXAMT             9 2          DETL BILLD
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*** BEGIN CHG *** 10/25/99 **************************************
     C**********           ADD  MBBAMT    XXCMED 102       CASE BILLD
     C                   ADD       XXAMT         XXCMED           10 2          CASE BILLD
     C*** END   CHG *** 10/25/99 **************************************
     C                   Z-ADD     EOFRDT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXFRDT
     C                   Z-ADD     EOTODT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXTODT
     C                   Z-ADD     MBTRDT        SRDTI
     C*** BEG CHG *** 11/17/99 ***************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** END CHG *** 11/17/99 ***************************
     C                   MOVEL     SRDTO         XXTRDT
     C                   ELSE                                                   ELS IFEQ '0'
     C                   Z-ADD     0             XXAMT                          DETL BILLD
     C                   Z-ADD     0             XXFRDT
     C                   Z-ADD     0             XXTODT
     C                   END                                                    END IFEQ '0'
     C*
     C                   MOVE      *BLANKS       XXDESC                         DESC
     C                   MOVEL     DS(6)         XXDESC                         MED DESC
     C                   ADD       1             XX#CMD            4 0          CASE # MED
     C*
     C                   Z-ADD     *ZEROS        XX#BLL            3 0          # OF BILLS
     C*
     C                   END
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   END                                                    END *IN99
     C                   END                                                    ADD 12/08/92
     C                   END                                                    ADD 12/08/92
     C*
     C*** COUNT NUMBER OF EOBS PER CASE AND GROUP ...
     C*
     C                   ADD       1             XX#EOB            3 0          # OF EOBS/CASE
     C                   ADD       1             XXGEOB            4 0          # OF EOBS/GROUP
     C*
     C*
     C* * * GET PROVIDER INFORMATION.
     C*
     C     PDKEY         CHAIN     PPD01                              50
     C     *IN50         IFEQ      '0'
     C*
     C*
     C                   MOVEL     PDNAME        XXPRNM
     C                   MOVE      EOFEIN        XXPRV#            9 0
     C                   MOVE      EOMOD         XXMOD             3 0
     C                   ELSE
     C                   MOVE      *BLANKS       XXPRNM
     C                   MOVE      EOFEIN        XXPRV#
     C                   MOVE      EOMOD         XXMOD
     C                   END
     C*
     C     XXMOD         IFNE      *ZEROS
     C                   MOVE      '1'           *IN96                          NOT BLANK
     C                   ELSE
     C                   MOVE      '0'           *IN96                          BLANK
     C                   END
     C*
     C                   MOVEL     EOPNAM        XXPRNM                         PROVIDER NAME  A
     C*
     C***  OUTPUT BILL - DETAIL LINE
     C*
     C     *IN91         IFEQ      '1'
     C     *IN93         IFEQ      '1'
     C     XXLINE        IFGT      40
     C                   EXCEPT    HEAD
     C                   END
     C                   EXCEPT    DET
     C                   END
     C                   ELSE
     C     XXLINE        IFGT      40
     C                   EXCEPT    HEAD
     C                   END
     C                   EXCEPT    DET
     C                   END
     C*
     C***  OUTPUT REPORT - DETAIL LINE
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    DETL1
     C*
     C*** IF BILLING IS PERFORMED, CREATE A/R RECORD.
     C*
     C                   EXSR      SRAR
     C*
     C*** BEG CHG *** 01/08/99 ***************************************
     C*
     C     CMKEY         CHAIN(N)  PCM01                              55
     C     *IN55         IFEQ      *OFF
     C                   Z-ADD     DBRRN         ##RRN
     C                   ENDIF                                                  *IN55 = OFF
     C*
     C*** BEG ADD *** 03/18/99 **************************************
     C     EOSYS         IFEQ      'P'
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           MOVE *BLANKS   KKWRUP
     C**********           MOVE 'U'       XXFAEO
     C                   Z-ADD     XXINV#        EOINV#
     C                   MOVEL     'P'           EOBLFL
     C*** END   CHG *** 09/23/99 **************************************
     C                   ENDIF
     C*** END ADD *** 03/18/99 **************************************
     C     EOSYS         IFEQ      'H'
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXFAHB
     C                   ENDIF
     C*
     C     EOSYS         IFEQ      'U'
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXNAPB
     C                   ENDIF
     C*
     C     EOSYS         IFEQ      'M'
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXNAMB
     C                   ENDIF
     C*** BEGIN ADD *** 09/23/99 **************************************
     C     EOSYS         IFNE      'P'
     C*** END   ADD *** 09/23/99 **************************************
     C*
     C  N16              CALL      'AA001R'
     C                   PARM                    ##RRN             9 0          RRN OF CASE
     C                   PARM      XXEOB#        ##EOB#            9 0          EOB #
     C*** BEGIN CHG *** 01/15/99 **************************************
     C**********           PARM           ##RCN   30       RECON #
     C                   PARM      XXRCN         ##RCN             3 0          RECON #
     C*** END   CHG *** 01/15/99 **************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM                    ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM      'P'           ##BBLF            1            BRS INV PROC FLAG
     C                   PARM      XXINV#        ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*** BEGIN ADD *** 09/23/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 09/23/99 **************************************
     C*
     C*** UPDATE NDTMP WITH INV#
     C*
     C     *IN16         IFEQ      *OFF
     C*
     C     TMKY1         CHAIN     PTM01                              55
     C     *IN55         DOWEQ     *OFF
     C                   MOVE      Q1PGM         TMPGM
     C                   MOVE      Q1USER        TMUSER
     C                   MOVE      Q1PGM         TMPGM
     C                   Z-ADD     DSTIME        TMTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TMDT
     C                   Z-ADD     XXINV#        TMINV#
     C                   UPDATE    PTM01
     C                   ENDDO                                                  *IN55 = OFF
     C*
     C                   ENDIF
     C*
     C*** END CHG *** 01/08/99 ***************************************
     C*** UPDATE EOB TRANS RECORD
     C*
     C                   MOVE      Q1PGM         EOPGM
     C                   MOVE      'P'           EOBLFL
     C                   MOVE      Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   Z-ADD     XXINV#        EOINV#
     C****
     C                   Z-ADD     1             T                 5 0
     C     ' '           LOOKUP    ESB(T)                                 60
     C     *IN60         IFEQ      '1'
     C   99              MOVE      'H'           ESB(T)
     C  N99              MOVE      'P'           ESB(T)
     C                   ELSE
     C                   MOVE      *BLANK        ESB(T)
     C                   END
     C***
     C*
     C  N16              UPDATE    PEO01
     C     READN         TAG
     C  N16              READ      PEO01                                  50
     C   16              READ(N)   PEO01                                  50
     C*
     C                   END                                                    END DOWEQ 50
     C*** BEG ADD *** 03/19/99 ****************************************
     C                   ENDIF                                                  PROV.BILL INSTALLED
     C*** END ADD *** 03/19/99 ****************************************
     C     *IN91         IFEQ      '1'
     C     XHSYST        COMP      0                                  30
     C     XPSYST        COMP      0                                  31
     C     XUSYST        COMP      0                                  32
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C     XMSYST        COMP      0                                  33
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   EXCEPT    DETS
     C     XHSYST        ADD       XPSYST        XASYST
     C     XASYST        ADD       XUSYST        XASYST
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C     XASYST        ADD       XMSYST        XASYST
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   EXCEPT    TOTS
     C                   END
     C*
     C*** END OF PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*
     C     *IN21         IFEQ      '0'
     C     *INLR         ANDEQ     '1'
     C                   EXCEPT    MSG
     C                   END
     C*
     C     *IN21         CABEQ     '0'           TERM2
     C*
     C                   EXSR      SRCASE
     C                   EXSR      SRGRP
     C*
     C*** OUTPUT FINAL TOTALS
     C*
     C*  ADD TOTAL HBA TO TOTAL PBR TO PRINT TOTAL AMT INVOICED
     C*
     C     XXTPBR        ADD       XXTHBA        XXTALL           10 2
     C                   ADD       XXTCRT        XXTALL                         ADD 12/09/92
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C     XXTALL        ADD       XXTMED        XXTALL
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD1C
     C                   EXCEPT    HEAD4
     C                   EXCEPT    TOTAL1
     C*
     C*  UPDATE COMPANY AUXILARY FILE WITH LAST INVOICE # USED
     C*
     C  N16C1KEY         CHAIN     PC101                              60
     C   16C1KEY         CHAIN(N)  PC101                              60
     C     *IN60         IFEQ      '0'
     C  N97              Z-ADD     XXINV#        C1LIN#
     C  N16              UPDATE    PC101
     C                   END
     C*
     C     TERM2         TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '1';
       Chain Orkey Por01;
       If %found;
          Except Route1;
       Endif;
       If ##prvb = 'Y';
          If *in91 = '1';
             If *in93 = '1';
                xxprt = '0';
                Chain Orkey Por01;
                If %found;
                   Except Route;
                Endif;
                xxprt = '2';
                Chain Orkey Por01;
                If %found;
                   Except Route2;
                Endif;
             Else;
                xxprt = '2';
                Chain Orkey Por01;
                If %found;
                   Except Route2;
                Endif;
             Endif;
          Else;
             xxprt = '0';
             Chain Orkey Por01;
             If %found;
                Except Route;
             Endif;
          Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C*****************************************************************--*
     C*  CREATE A/R RECORD
     C*****************************************************************--*
     C*
     C     SRAR          BEGSR
     C*
     C     *IN99         IFEQ      '1'
     C     *IN94         ANDEQ     '0'                                          ADD 12/08/92
     C                   MOVE      'H'           ARSYS
     C                   ADD       XXAMT         XHSYST                         ADD 03/26/93
     C                   ELSE
     C     *IN99         IFEQ      '0'                                          ADD 12/08/92
     C     *IN94         ANDEQ     '0'                                          ADD 12/08/92
     C                   MOVE      'P'           ARSYS
     C                   ADD       XXAMT         XPSYST                         ADD 03/26/93
     C                   ELSE                                                   ADD 12/08/92
     C     *IN94         IFEQ      '1'                                          ADD 12/08/92
     C     *IN99         ANDEQ     '0'                                          ADD 12/08/92
     C                   MOVE      'U'           ARSYS                          ADD 12/08/92
     C                   ADD       XXAMT         XUSYST                         ADD 03/26/93
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   ELSE
     C     *IN94         IFEQ      '1'
     C     *IN99         ANDEQ     '1'
     C                   MOVE      'M'           ARSYS
     C                   ADD       XXAMT         XMSYST
     C                   END
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   END
     C                   END                                                    ADD 12/08/92
     C                   END                                                    ADD 12/08/92
     C     *IN97         IFEQ      '1'
     C*** BEG ADD *** 03/19/99 ****************************************
     C     ##PRVB        IFEQ      'Y'
     C     EWKEY1        CHAIN     PEW01                              65
     C                   ELSE
     C*** END ADD *** 03/19/99 ***************************************
     C     EWKEY         CHAIN     PEW01                              65
     C*** BEG ADD *** 03/19/99 ****************************************
     C                   ENDIF
     C*** END ADD *** 03/19/99 ****************************************
     C     *IN65         IFEQ      '0'
     C     XWFLG1        IFEQ      *BLANKS
     C     XWDIFL        ANDEQ     *BLANKS
     C                   GOTO      NXTAR
     C                   END
     C                   ELSE                                                   ADD 07/14/93
     C                   GOTO      NXTAR                                        ADD 07/14/93
     C                   END
     C                   END
     C*
     C*** BEG ADD *** 03/19/99 ***************************************
     C*
     C     *IN16         IFEQ      *OFF
     C*****************************************************************
     C* PROVIDER BILLS PROCESS FOR WRITING A/R RECORD          PBP
     C*****************************************************************
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   MOVE      EACO#         ARCO#
     C                   MOVE      EASUB#        ARSUB#
     C                   MOVE      EAFND         ARFND
     C     *IN90         IFEQ      '1'
     C                   MOVE      EAFYR         ARFYR
     C                   MOVE      EACASE        ARCASE
     C                   ELSE
     C     *IN91         IFEQ      '1'
     C                   Z-ADD     *ZEROS        ARFYR
     C                   Z-ADD     *ZEROS        ARCASE
     C                   END
     C                   END
     C                   MOVE      *BLANK        ARTRCD
     C                   MOVE      'I'           ARTRTP
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARPODT
     C                   MOVEL     XXDESC        ARDESC
     C                   Z-ADD     XXAMT         ARAMT
     C                   MOVE      EAEOB#        AREOB#
     C                   MOVE      *ZERO         ARCK#
     C                   MOVE      'B'           ARSTAT
     C                   MOVE      Q1USER        ARUSER
     C                   MOVE      Q1PGM         ARPGM
     C                   Z-ADD     DSTIME        ARTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C                   Z-ADD     XXINV#        ARINV#
     C                   MOVEL     EARCN         ARRCN
     C                   WRITE     PAR01
     C*
     C*****************************************************************
     C* BILL ENTRY PROCESS FOR WRITING A/R RECORD          BEP
     C*****************************************************************
     C                   ELSE                                                   PROV. BILL INSTAL
     C*** END ADD *** 03/19/99 ***************************************
     C                   MOVE      EOCO#         ARCO#
     C                   MOVE      EOSUB#        ARSUB#
     C                   MOVE      EOFND         ARFND
     C     *IN90         IFEQ      '1'
     C                   MOVE      EOFYR         ARFYR
     C                   MOVE      EOCASE        ARCASE
     C                   ELSE
     C     *IN91         IFEQ      '1'
     C                   Z-ADD     *ZEROS        ARFYR
     C                   Z-ADD     *ZEROS        ARCASE
     C                   END
     C                   END
     C                   MOVE      *BLANK        ARTRCD
     C                   MOVE      'I'           ARTRTP
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARPODT
     C                   MOVEL     XXDESC        ARDESC
     C                   Z-ADD     XXAMT         ARAMT
     C                   MOVE      EOEOB#        AREOB#
     C                   MOVE      *ZERO         ARCK#
     C                   MOVE      'B'           ARSTAT
     C                   MOVE      Q1USER        ARUSER
     C                   MOVE      Q1PGM         ARPGM
     C                   Z-ADD     DSTIME        ARTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C                   Z-ADD     XXINV#        ARINV#
     C*** BEG ADD *** 03/05/99 **************************************
     C                   MOVEL     EORCN         ARRCN
     C*** END ADD *** 03/05/99 **************************************
     C                   WRITE     PAR01
     C*** BEG ADD *** 03/19/99 **************************************
     C                   ENDIF                                                  PROV. BILL
     C*** END ADD *** 03/19/99 **************************************
     C                   ENDIF
     C*
     C     NXTAR         TAG
     C*
     C                   ENDSR
     C*
     C*****************************************************************--*
     C*  GET HEADING INFORMATION FOR BILLS
     C*****************************************************************--*
     C*
     C     SRHEAD        BEGSR
     C*
     C* * * GET FUND(GROUP) INFORMATION.
     C*
     C     FNKEY         CHAIN     PFN01                              51
     C     *IN51         IFEQ      '0'
     C                   Z-ADD     1             Z                 5 0
     C*                    -----***REMOVE LEADING BLANKS FROM GROUP NAME
     C     *BLANK        LOOKUP    FNNM(Z)                            60
     C                   MOVEA     FNNM(Z)       XXFNNM           35
     C                   Z-ADD     1             X                 5 0
     C     'P'           LOOKUP    FSY(X)                                 60
     C     *IN60         IFEQ      '1'
     C*
     C                   MOVEL     FBR(X)        XXCODE            3
     C                   ELSE
     C                   MOVEL     *BLANK        XXCODE            3
     C                   END
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C*
     C* * * GET CLAIM MASTER FILE
     C*
     C     CMKEY         CHAIN     PCM01                              51
     C     *IN51         IFEQ      '0'
     C                   Z-ADD     CMACDT        SRDTI
     C*** BEG CHG *** 11/17/99 **************************
     C******************   CALL 'SRCVTDAT'DTMDY
     C*****************    MOVELSRDTO     XXACDT  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*** END CHG *** 11/17/99 **************************
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      CMSS#         XXSS#             9 0
      /Free
       xxSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C                   MOVE      CMCCL#        XXCCLM           13
     C     XXCCLM        COMP      *BLANK                             9898
     C     CMSS#         CHAIN     PCD01                              52
     C     *IN52         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST           56
     C     CDFSN         IFNE      *BLANK
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END
     C*** BEG CHG *** 01/08/99 ***************************************
     C                   MOVEL     XXLAST        XXPNAM           27
     C**********           MOVELXXLAST    XXPNAM 30
     C*** END CHG *** 01/08/99 ***************************************
     C                   ELSE
     C*** BEG CHG *** 01/08/99 ***************************************
     C                   MOVEL     *BLANKS       XXPNAM           27
     C**********           MOVEL*BLANKS   XXPNAM 30
     C*** END CHG *** 01/08/99 ***************************************
     C                   END
     C*
     C                   ELSE
     C                   Z-ADD     0             XXACDT
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      *ZERO         XXSS#             9 0
     C                   MOVE      *BLANKS       XXSS#
      ***** End Chg *** 06/09/20 *******************************************
     C                   MOVE      *BLANKS       XXCCLM           13
     C                   MOVEL     *BLANKS       XXPNAM
     C                   END
     C*
     C  N91              Z-ADD     0             PAGE
     C*
     C*** DETERMINE IF 2ND ADDRESS IS BLANK
     C*
     C     XCADR2        COMP      *BLANKS                            9494
     C*
     C                   ENDSR
     C*
     C*****************************************************************--*
     C*  PERFORM CASE TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRCASE        BEGSR
     C*
     C                   ADD       XX#CHB        XX#GHB            5 0          GROUP # HBA
     C                   ADD       XX#CPB        XX#GPB            5 0          GROUP # PBR
     C                   ADD       XX#CCT        XX#GCT            5 0          GROUP # CERT
     C                   ADD       XX#CMD        XX#GME            5 0          GROUP # CERT
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   ADD       XX#CMD        XX#GMD            5 0          GROUP # MED
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   ADD       XXCHBA        XXGHBA           10 2          GROUP HBA BLLED
     C                   ADD       XXCPBR        XXGPBR           10 2          GROUP PBR BLLED
     C                   ADD       XXCCRT        XXGCRT           10 2          GROUP CRT BLLED
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   ADD       XXCMED        XXGMED           10 2          GROUP MED BLLED
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C     XXCHBA        ADD       XXCPBR        XXCAMT           10 2          CASE TOTAL  LED
     C                   ADD       XXCCRT        XXCAMT           10 2          CASE TOTAL  LED
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   ADD       XXCMED        XXCAMT
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C                   ADD       1             XX#INV            6 0          # INVOICES
     C*
     C     *IN91         IFNE      '1'
     C     XXLINE        IFGT      40
     C                   EXCEPT    HEAD
     C                   END
     C                   EXCEPT    TOT
     C                   ELSE
     C     *IN93         IFEQ      '1'
     C     XXLINE        IFGT      40
     C                   EXCEPT    HEAD
     C                   END
     C                   EXCEPT    TOT1
     C                   END
     C                   END
     C*
     C**** Begin Add ***** 04/10/07 ***************************************
     C     CLTID         IFNE      '139'                                        NO T&M for 139
     C**** End Add ******* 04/10/07 ***************************************
     C*
     C* LOKUP EOB ARRAY TO SEE IF ANY VALUES WERE STORED
     C*
     C                   Z-ADD     1             Q
     C     *ZEROS        LOOKUP    EOB(Q)                                 60
     C     *IN60         IFEQ      '1'
     C     Q             ANDGT     0
     C     Q             SUB       1             Q
     C                   DO        Q             X                 5 0
     C*
     C* CHAIN TO T/M DETAIL CHARGE FILE AND EXCPT HEADING
     C*
     C     TMKEY         CHAIN(N)  PTM01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    HEADTM
     C*
     C* READ ALL THE DETAIL CHARGES FOR THIS CASE
     C*
     C                   Z-ADD     *ZEROS        TOTEBH            7 2
     C                   Z-ADD     *ZEROS        TOTEBM            4 0
     C                   Z-ADD     *ZEROS        TOTEBC            7 2
     C                   Z-ADD     *ZEROS        TOTHCH
     C                   Z-ADD     *ZEROS        TOTMCH
     C     *IN60         DOWEQ     '0'
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     TMHOLD        IFNE      'H'
     C*** BEGIN ADD *** 08/07/06 **************************************
     C     TMBFLG        ANDNE     'N'
     C*** END   ADD *** 08/07/06 **************************************
     C     ##RHAB        ORNE      'Y'
     C*** END   ADD *** 10/25/99 **************************************
     C*** BEGIN ADD *** 08/07/06 **************************************
     C     TMBFLG        ANDNE     'N'
     C*** END   ADD *** 08/07/06 **************************************
     C                   Z-ADD     TMSVDT        SRDTI
     C*** BEG CHG *** 11/17/99 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXSVDT  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXSVDT            8 0
     C*** END CHG *** 11/17/99 **************************************
     C                   Z-ADD     TMHRS         XXHOUR            7 2
     C     XXHOUR        MULT      TMHRRT        XXTHCH            7 2
     C                   ADD       XXTHCH        TOTHCH            7 2
     C                   ADD       TMHRS         TOTEBH
     C                   Z-ADD     TMMILE        XXMILE            4 0
     C     XXMILE        MULT      TMMIRT        XXTMCH            7 2
     C                   ADD       XXTMCH        TOTMCH            7 2
     C                   ADD       TMMILE        TOTEBM
     C                   Z-ADD     TMMSCG        XXMISC            7 2
     C                   ADD       TMMSCG        TOTEBC
     C*
     C                   MOVEL     TMDESC        XXACDS           20            ADD 11/17/99
     C                   MOVEL     TMACT         XXACT             3            ADD 11/17/99
     C*
     C*** BEG DEL *** 11/17/99 **************************************
     C**GET*ACTIVITY CODE DESCRIPTION FROM THE ACTIVITY CODE TABLE FILE
     C**********
     C**********           MOVE *BLANKS   XXKEY
     C**********           MOVE *BLANKS   XXACDS
     C**********           MOVELTMACT     XXKEY
     C********** TBLKEY    CHAINPAT01                60
     C********** *IN60     IFEQ '0'
     C****BEGIN*CHG *** 10/25/99 **************************************
     C**********           MOVELATDESC    XXACDS 20
     C**********           MOVELATDESC    XXACDS 15
     C****END***CHG *** 10/25/99 **************************************
     C**********           END
     C*** END DEL *** 11/17/99 **************************************
     C*
     C* GET THE INTERVIEWER NAME FROM THE USER PARM FILE
     C*
     C                   MOVE      *BLANKS       XXINTR           20
     C                   MOVE      *BLANKS       XXSTAF            3
     C                   MOVEL     TMSTAF        XXSTAF
     C     UPKY1         CHAIN     PUP02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXINTR
     C                   END
     C*
     C* PRINT THE DETAIL LINE
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEADTM
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    DETTM
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*
     C* GET THE NEXT RECORD
     C*
     C     TMKEY         READE(N)  PTM01                                  60
     C                   END                                                    END DO
     C*
     C* CALCULATE THE REVIEWER FEE AND PRINT THE TOTALS
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEADTM
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C*** BEG ADD *** 03/19/99 ***************************************
     C*****************************************************************
     C* PROVIDER BILLS PROCESS                                PBP
     C*****************************************************************
     C     ##PRVB        IFEQ      'Y'
     C*** BEGIN CHG *** 09/23/99 *************************************
     C********** EAKEY2    CHAINPEA01               N60
     C     EAKEY3        CHAIN     PEA02                              60
     C*** END   CHG *** 09/23/99 *************************************
     C     *IN60         IFEQ      *OFF
     C     XASYS         IFEQ      'P'
     C     XASYS         OREQ      'H'
     C     XASYS         OREQ      'U'
     C     XASYS         OREQ      'M'
     C*
     C*** BEGIN CHG *** 09/23/99 *************************************
     C**********           Z-ADDEABAMT    XXBAMT
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     ##RHAB        IFEQ      'Y'
     C*
     C                   EXSR      TMCALC
     C                   Z-ADD     XXAMT         XXBAMT
     C*
     C                   ELSE
     C*
     C*** END   ADD *** 10/25/99 **************************************
     C                   Z-ADD     XABAMT        XXBAMT
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*** END   CHG *** 09/23/99 *************************************
     C*** BEGIN ADD *** 09/23/99 *************************************
     C                   Z-ADD     XABCAM        XXRFEE            7 2
     C*** END   ADD *** 09/23/99 *************************************
     C                   ENDIF
     C                   ENDIF
     C*****************************************************************
     C* BILL ENTRY PROCESS                                    BEP
     C*****************************************************************
     C                   ELSE
     C*** END ADD *** 03/19/99 ***************************************
     C     EOKEY         CHAIN     PEO02                              60
     C     *IN60         IFEQ      '0'
     C     X2SYS         IFEQ      'P'
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     ##RHAB        IFEQ      'Y'
     C*
     C                   EXSR      TMCALC
     C                   Z-ADD     XXAMT         XXBAMT
     C*
     C                   ELSE
     C*
     C*** END   ADD *** 10/25/99 **************************************
     C                   Z-ADD     X2BAMT        XXBAMT            9 2
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*** BEGIN ADD *** 09/23/99 *************************************
     C                   Z-ADD     X2BAMT        XXRFEE
     C*** END   ADD *** 09/23/99 *************************************
     C                   ELSE
     C     X2SYS         IFEQ      'U'
     C     PBKEY2        CHAIN     PPB01                              60
     C     *IN60         IFEQ      '0'
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     ##RHAB        IFEQ      'Y'
     C*
     C                   EXSR      TMCALC
     C                   Z-ADD     XXAMT         XXBAMT
     C*
     C                   ELSE
     C*
     C*** END   ADD *** 10/25/99 **************************************
     C                   Z-ADD     PBBAMT        XXBAMT
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*** BEGIN ADD *** 09/23/99 *************************************
     C                   Z-ADD     PBCAMT        XXRFEE
     C*** END   ADD *** 09/23/99 *************************************
     C                   END
     C                   ELSE
     C     X2SYS         IFEQ      'H'
     C     HBKEY2        CHAIN     PHB01                              60
     C     *IN60         IFEQ      '0'
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     ##RHAB        IFEQ      'Y'
     C*
     C                   EXSR      TMCALC
     C                   Z-ADD     XXAMT         XXBAMT
     C*
     C                   ELSE
     C*
     C*** END   ADD *** 10/25/99 **************************************
     C                   Z-ADD     HB1AAM        XXBAMT
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*** BEGIN ADD *** 09/23/99 *************************************
     C                   Z-ADD     HB1CAM        XXRFEE
     C*** END   ADD *** 09/23/99 *************************************
     C                   END
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   ELSE
     C     X2SYS         IFEQ      'M'
     C     MBKEY2        CHAIN     PMB01                              60
     C     *IN60         IFEQ      '0'
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     ##RHAB        IFEQ      'Y'
     C*
     C                   EXSR      TMCALC
     C                   Z-ADD     XXAMT         XXBAMT
     C*
     C                   ELSE
     C*
     C*** END   ADD *** 10/25/99 **************************************
     C                   Z-ADD     MBBAMT        XXBAMT
     C*** BEGIN ADD *** 10/25/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*** BEGIN ADD *** 09/23/99 *************************************
     C                   Z-ADD     MBCAMT        XXRFEE
     C*** END   ADD *** 09/23/99 *************************************
     C                   END
     C                   END
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   END
     C                   END
     C                   END
     C                   END
     C*** BEG ADD *** 03/19/99 ***************************************
     C                   ENDIF                                                  PRVB = Y
     C*** END ADD *** 03/19/99 ****************************************
     C*
     C*** BEGIN DEL *** 09/23/99 **************************************
     C********** XXBAMT    SUB  TOTHCH    XXRFEE  72
     C********** XXRFEE    SUB  TOTMCH    XXRFEE
     C********** XXRFEE    SUB  TOTEBC    XXRFEE
     C*** END   DEL *** 09/23/99 **************************************
     C                   EXCEPT    TOTTM
     C                   END
     C                   END                                                    END DO
     C                   END                                                    ENDIF
     C**** Begin Add ***** 04/10/07 ***************************************
     C                   ENDIF                                                  end CLT 139
     C**** End Add ******* 04/10/07 ***************************************
     C*
     C* CLEAR THE EOB ARRAY READY FOR THE NEXT CASE
     C*
     C*** BEGIN ADD *** 03/19/99 *************************************
     C**********           MOVE *ZEROS    EOB
     C                   MOVEA     *ZEROS        EOB
     C*** END   ADD *** 03/19/99 *************************************
     C*
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    CASTOT
     C*
     C                   Z-ADD     0             XXCAMT
     C*** BEG ADD *** 03/19/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C                   Z-ADD     EAFYR         XXFYR
     C                   Z-ADD     EAFYR         XXYR
     C                   Z-ADD     EACASE        XXCASE
     C                   ELSE
     C*** END ADD *** 03/19/99 ***************************************
     C                   Z-ADD     EOFYR         XXFYR
     C                   Z-ADD     EOFYR         XXYR
     C                   Z-ADD     EOCASE        XXCASE
     C*** BEG ADD *** 03/19/99 **************************************
     C                   ENDIF
     C*** END ADD *** 03/19/99 **************************************
     C*
     C     *INLR         IFEQ      '0'
     C     *IN91         IFNE      '1'                                          ADD 03/26/93
     C  N97              ADD       1             XXINV#
     C                   END                                                    ADD 03/26/93
     C                   END
     C*
     C*** BEG ADD *** 03/19/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     *INLR         IFEQ      '0'
     C     EAFND         ANDNE     XXFND
     C     XXFND         ANDNE     *ZEROS
     C                   EXSR      SRGRP
     C                   EXCEPT    HEAD1
     C                   EXSR      SRHEAD
     C                   EXCEPT    HEAD3
     C                   END
     C                   ELSE
     C*** END ADD *** 03/19/99 ***************************************
     C     *INLR         IFEQ      '0'
     C     EOFND         ANDNE     XXFND
     C     XXFND         ANDNE     *ZEROS
     C                   EXSR      SRGRP
     C                   EXCEPT    HEAD1
     C                   EXSR      SRHEAD
     C                   EXCEPT    HEAD3
     C                   END
     C*** BEG ADD *** 03/19/99 ***************************************
     C                   ENDIF
     C*** END ADD *** 03/19/99 ****************************************
     C*
     C                   ENDSR
     C*
     C*****************************************************************--*
     C*  PERFORM GROUP TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRGRP         BEGSR
     C*
     C                   ADD       XX#GHB        XX#THB            5 0          TOTAL # HBA
     C                   ADD       XX#GPB        XX#TPB            5 0          TOTAL # PBR
     C                   ADD       XX#GCT        XX#TCT            5 0          TOTAL # CRT
     C                   ADD       XX#GMD        XX#TMD            5 0          TOTAL # CRT
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C     XX#THB        ADD       XX#TPB        XX#TTO            6 0          TOTAL #
     C                   ADD       XX#TCT        XX#TTO
     C                   ADD       XX#TMD        XX#TTO
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   ADD       XXGHBA        XXTHBA           10 2          TOTAL HBA BLLED
     C                   ADD       XXGPBR        XXTPBR           10 2          TOTAL PBR BLLED
     C                   ADD       XXGCRT        XXTCRT           10 2          TOTAL CRT BLLED
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   ADD       XXGMED        XXTMED           10 2          TOTAL MED BLLED
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    GRPTOT
     C*
     C*** BEG ADD *** 03/19/99 **************************************
     C     ##PRVB        IFEQ      'Y'
     C                   Z-ADD     EAFND         XXFND
     C                   ELSE
     C*** END ADD *** 03/19/99 ***************************************
     C                   Z-ADD     EOFND         XXFND
     C*** BEG ADD *** 03/19/99 ***************************************
     C                   ENDIF
     C*** END ADD *** 03/19/99 ***************************************
     C*
     C                   ENDSR
     C*
     C*
     C*
     C*****************************************************************--*
     C*  VERIFY BILLING AMT GREATER THAN 0 BEFORE PRODUCING INVOICE ...
     C*****************************************************************--*
     C*
     C     SRAMT         BEGSR
     C*
     C*** BEG ADD *** 03/19/99 ***************************************
     C*****************************************************************
     C* PROVIDER BILLS PROCESS FOR DETERMINING SKIP        PBP
     C*****************************************************************
     C     ##PRVB        IFEQ      'Y'
     C     EASYS         IFEQ      'P'
     C     EASYS         OREQ      'H'
     C     EASYS         OREQ      'U'
     C     EASYS         OREQ      'M'
     C*
     C     EABAMT        CABLE     *ZEROS        READN1
     C*
     C     *IN21         IFEQ      '0'
     C                   MOVE      '1'           *IN21
     C                   END
     C*
     C                   ENDIF
     C*****************************************************************
     C* BILL ENTRY PROCESS FOR DETERMINING SKIP        BEP
     C*****************************************************************
     C                   ELSE
     C*** END ADD *** 03/19/99 ***************************************
     C     EOSYS         IFEQ      'P'
     C     EOBAMT        CABLE     *ZEROS        READN
     C*
     C     *IN21         IFEQ      '0'
     C                   MOVE      '1'           *IN21
     C                   END
     C*
     C                   ELSE
     C*
     C     EOSYS         IFEQ      'H'                                          ADD 12/08/92
     C*
     C     HBKEY1        CHAIN     PHB01                              95
     C     *IN95         IFEQ      '0'
     C     HB1AAM        CABLE     *ZEROS        READN
     C*
     C     *IN21         IFEQ      '0'
     C                   MOVE      '1'           *IN21
     C                   END
     C*
     C                   END
     C*
     C                   ELSE                                                   ADD 12/08/92
     C*
     C     EOSYS         IFEQ      'U'                                          ADD 12/08/92
     C*
     C     PBKEY1        CHAIN     PPB01                              95
     C     *IN95         IFEQ      '0'
     C     PBBAMT        CABLE     *ZEROS        READN
     C*
     C     *IN21         IFEQ      '0'
     C                   MOVE      '1'           *IN21
     C                   END
     C*
     C                   END
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C                   ELSE
     C*
     C     EOSYS         IFEQ      'M'
     C*
     C     MBKEY1        CHAIN     PMB01                              95
     C     *IN95         IFEQ      '0'
     C     MBBAMT        CABLE     *ZEROS        READN
     C*
     C     *IN21         IFEQ      '0'
     C                   MOVE      '1'           *IN21
     C                   END
     C                   END
     C                   END
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   END
     C                   END                                                    ADD 12/08/92
     C                   END                                                    ADD 12/08/92
     C*** BEG ADD *** 03/19/99 ****************************************
     C                   ENDIF
     C*** END ADD *** 03/19/99 ****************************************
     C*
     C                   ENDSR
     C*
     C****
     C*** BEG ADD *** 10/25/99 ****************************************
     C*****************************************************************
     C* TMCALC - CALCULATES T/M AMOUNTS
     C*****************************************************************
     C     TMCALC        BEGSR
     C*
     C                   Z-ADD     *ZEROS        XXTMAT
     C*
     C     TMKY1         CHAIN(N)  PTM01                              55
     C*
     C     *IN55         DOWEQ     *OFF
     C*
     C     TMHOLD        IFNE      'H'
     C*
     C     TMHRS         MULT      TMHRRT        XXTMHR            7 2
     C     TMMILE        MULT      TMMIRT        XXTMMR            7 2
     C     XXTMHR        ADD       XXTMMR        XXTMA             9 2
     C                   ADD       TMMSCG        XXTMA
     C*
     C                   ADD       XXTMA         XXTMAT           11 2
     C*
     C                   ENDIF
     C*
     C     TMKY1         READE(N)  PTM01                                  55
     C*
     C                   ENDDO                                                  *IN55 = OFF
     C*
     C                   Z-ADD     XXTMAT        XXAMT
     C*
     C                   ENDSR
     C*** END ADD *** 10/25/99 ****************************************
     O/EJECT
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
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
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                 N97   ORTITL              80
     O                  97                       35 'PBR/HBA RE-BILL'
      ***** Beg Del ***** 08/26/14 *********************************************
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
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               57 'INC. 2000     '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT   E            HEAD           1 01
     O*
     O*
     O*** BEG CHG *** 11/17/99 **************************************
     O************************          112 'INVOICE #:'
     O************************ XXINV#Z  122
     O                                          111 'INVOICE #:'
     O                                          122 '**TRIAL**'
     O*** END CHG *** 11/17/99 **************************************
     O          E            HEAD           1
     O*** BEG CHG *** 11/17/99 **************************************
     O******************************    112 'PAGE:'
     O                                          111 'PAGE:'
     O*** END CHG *** 11/17/99 **************************************
     O                       PAGE               122
     O          E            HEAD           1
     O*** BEG CHG *** 11/17/99 **************************************
     O********************************  112 'DATE:'
     O                                          111 'DATE:'
     O*** END CHG *** 11/17/99 **************************************
     O*** BEG CHG *** 11/17/99 ************************
     O********************     DSDATEY  122
     O                       XSDATE        Y    122
     O*** END CHG *** 11/17/99 ************************
     O*
     O*
     O          E            HEAD           1 08
     O                                           70 '* PRINT ONLY *'
     O          E            HEAD           2
     O              N97                          70 '*** INVOICE ***'
     O               97                          70 'ADJUSTED INVOICE'
     O*** BEG CHG *** 04/13/99 ****************************************
     O          E            HEAD           1
     O                                           14 'SUBSC  :'
     O                       XXSUB#        Z     20
     O                                           23 '-'
     O                       XXSBNM              58
     O******* E  1             HEAD
     O*******                             9 'SUBSC  :'
     O*******                  XXSUB#    15
     O*******                            17 '-'
     O*******                  XXSBNM    53
     O*** END CHG *** 04/13/99 ****************************************
     O*
     O                                           79 'PATIENT:'
     O*** BEG CHG *** 11/17/99 ****************************************
     O**********************   XXPNAM   115
     O                       XXPNAM             107
     O*** END CHG *** 11/17/99 ****************************************
     O*** BEG CHG *** 04/13/99 ****************************************
     O          E            HEAD           1
     O                                           14 'GROUP  :'
     O                       XXFND         Z     20
     O                                           23 '-'
     O                       XXFNNM              58
     O******* E  1             HEAD
     O*******                             9 'GROUP  :'
     O*******                  XXFND Z   15
     O*******                            17 '-'
     O*******                  XXFNNM    53
     O*** END CHG *** 04/13/99 ****************************************
     O                                           79 'SOC SEC:'
     O*** BEGIN CHG *** 08/03/2006 ************************************
     O**********               XXSS#     91 '0   -  -    '
      ***** Beg Chg *** 06/09/20 *******************************************
     O***           N71      XXSS#               91 '0   -  -    '
     O              N71      XXSS#               91
      ***** End Chg *** 06/09/20 *******************************************
     O               71      SS#                 91
     O*** END   CHG *** 08/03/2006 ************************************
     O          E            HEAD           1
     O                                           79 'ACC  DT:'
     O                       XXACDT        Y     90
     O*** BEG CHG *** 04/13/99 ***************************************
     O          E            HEAD           2
     O               98                          14 'CLAIM #:'
     O               98      XXCCLM              28
     O******* E  2             HEAD
     O*******          98                 9 'CLAIM #:'
     O*******          98      XXCCLM    23
     O*** END CHG *** 04/13/99 ***************************************
     O                                           79 'FILE # :'
     O*** BEGIN CHG *** 03/09/00 **************************************
     O**********               XF#      100
     O                       XF#                101
     O*** END   CHG *** 03/09/00 **************************************
     O          E            HEAD           2
     O*
     O*
     O*** BEG CHG *** 04/13/99 ***************************************
     O*** BEG CHG *** 11/17/99 **************************************
     O***********************            13 'DATE'
     O                                           14 'DATE'
     O*** END CHG *** 11/17/99 **************************************
     O                                           28 'EOB#/SVC#'
     O*** BEG CHG *** 11/17/99 **************************************
     O*******************************    33 'RCN'
     O                                           32 'RCN'
     O*** END CHG *** 11/17/99 **************************************
     O                                           50 'SERVICE DATES'
     O                                           63 'SERVICES'
     O                                           85 'PROVIDER'
     O                                          105 'PROVIDER NAME'
     O                                          122 'REVIEWER FEE'
     O*****
     O*****                               7 'DATE'
     O*****                              19 'EOB#/SVC#'
     O****BEGIN CHG *** 01/15/99 **************************************
     O***********                        37 'SERVICE DATES'
     O***********                        50 'SERVICES'
     O***********                        82 'PROVIDER'
     O***********                       102 'PROVIDER NAME'
     O***********                       124 'REVIEWER FEE'
     O*****                              23 'RCN'
     O*****                              42 'SERVICE DATES'
     O*****                              55 'SERVICES'
     O*****                              87 'PROVIDER'
     O*****                             107 'PROVIDER NAME'
     O*****                             127 'REVIEWER FEE'
     O*** END   CHG *** 01/15/99 **************************************
     O*** END CHG *** 04/13/99 ***************************************
     O*** BEG CHG *** 04/13/99 ***************************************
     O          E            DET            1
     O*** BEG CHG *** 11/17/99 **************************************
     O**********************   XXTRDT    15 '  /  /  '
     O                       XXTRDT              16 '  /  /    '
     O*** END CHG *** 11/17/99 **************************************
     O                       XXEOB#        Z     28
     O*** BEG CHG *** 11/17/99 **************************************
     O***********************  XXRCN Z   33
     O***********************  XXFRDT    42 '  /  /  '
     O                       XXRCN         Z     32
     O                       XXFRDT              43 '  /  /    '
     O*** END CHG *** 11/17/99 **************************************
     O                                           44 '-'
     O*** BEG CHG *** 11/17/99 **************************************
     O***********************  XXTODT    53 '  /  /  '
     O                       XXTODT              54 '  /  /    '
     O*** END CHG *** 11/17/99 **************************************
     O                       XXDESC              75
     O                       XXPRV#        Z     86
     O               96                          87 '-'
     O               96      XXMOD         Z     90
     O                       XXPRNM             109
     O                       XXAMT         4    122
     O*****
     O*****   E  1             DET
     O*****                    XXTRDT     9 '  /  /  '
     O*****                    XXEOB#Z   19
     O****BEGIN CHG *** 01/15/99 **************************************
     O**********               XXFRDT    29 '  /  /  '
     O**********                         31 '-'
     O**********               XXTODT    40 '  /  /  '
     O**********               XXDESC    72
     O**********               XXPRV#Z   83
     O**********       96                84 '-'
     O**********       96      XXMOD Z   87
     O**********               XXPRNM   109
     O**********               XXAMT 4  124
     O*****                    XXRCN Z   23
     O*****                    XXFRDT    34 '  /  /  '
     O*****                              36 '-'
     O*****                    XXTODT    45 '  /  /  '
     O*****                    XXDESC    77
     O*****                    XXPRV#Z   88
     O*****            96                89 '-'
     O*****            96      XXMOD Z   92
     O*****                    XXPRNM   112
     O*****                    XXAMT 4  127
     O*** END   CHG *** 01/15/99 **************************************
     O*** END CHG *** 04/13/99 ***************************************
     O          E            TOT1           1
     O                                          124 '==========='
     O          E            TOT1           1
     O                       XXCAMT        4    124
     O*
     O*
     O          E            TOT            1 43
     O                                           53 'PLEASE PAY THIS AMOUNT:'
     O                       XXCAMT        4B    67 '$'
     O                                           80 'ON OR BEFORE'
     O                       XDUEDT        Y     91
     O          E            TOT            1
     O                                           67 '============'
     O          E            TOT            1 48
     O                                           52 'PLEASE MAIL REMITTANCE'
     O                                           56 'TO:'
     O                       XCNAME             100
     O          E            TOT            1
     O                       XCADR1             100
     O          E    94      TOT            1
     O                       XCADR2             100
     O          E            TOT            1
     O                       XCCSZ              100
     O          E            TOT            2
     O                       XCPHN#              74
     O          E            TOT            1
     O                                           56 'TAX ID #:'
     O                       XXFEIN        Z     69
     O          E            HEADTM         1 01
     O                                          110 'INVOICE #:'
     O*** BEG CHG *** 11/17/99 **************************************
     O************************ XXINV#Z  122
     O                                          122 '**TRIAL**'
     O*** END CHG *** 11/17/99 **************************************
     O          E            HEADTM         1
     O                                          110 'PAGE:'
     O                       PAGE               122
     O          E            HEADTM         1
     O                                          110 'DATE:'
     O*** BEG CHG *** 11/17/99 ************************
     O******************       DSDATEY  122
     O                       XSDATE        Y    122
     O*** END CHG *** 11/17/99 ************************
     O          E            HEADTM         1
     O                                          110 'EOB#/SVC#:'
     O                       TMEOB#        Z    122
     O*** BEGIN ADD *** 01/15/99 **************************************
     O                       TMRCN         Z    127
     O*** END   ADD *** 01/15/99 **************************************
     O          E            HEADTM         2 09
     O                                           53 'TIME/MATERIALS'
     O                                           71 'CHARGES WORKSHEET'
     O          E            HEADTM         1
     O                                            9 'SUBSC  :'
     O                       XXSUB#        Z     15
     O                                           17 '-'
     O                       XXSBNM              53
     O*
     O                                           79 'PATIENT:'
     O*** BEG CHG *** 11/17/99 ****************************************
     O**********************   XXPNAM   115
     O                       XXPNAM             107
     O*** END CHG *** 11/17/99 ****************************************
     O          E            HEADTM         1
     O                                            9 'GROUP  :'
     O                       XXFND         Z     15
     O                                           17 '-'
     O                       XXFNNM              53
     O                                           79 'SOC SEC:'
     O*** BEGIN CHG *** 08/03/2006 ************************************
     O**********               XXSS#     91 '0   -  -    '
      ***** Beg Chg *** 06/09/20 *******************************************
     O***           N71      XXSS#               91 '0   -  -    '
     O              N71      XXSS#               91
      ***** End Chg *** 06/09/20 *******************************************
     O               71      SS#                 91
     O*** END   CHG *** 08/03/2006 ************************************
     O          E            HEADTM         1
     O                                           79 'ACC  DT:'
     O                       XXACDT        Y     90
     O          E            HEADTM         2
     O               98                           9 'CLAIM #:'
     O               98      XXCCLM              23
     O                                           79 'FILE # :'
     O*** BEGIN CHG *** 03/09/00 **************************************
     O**********               XF#      100
     O                       XF#                101
     O*** END   CHG *** 03/09/00 **************************************
     O          E            HEADTM         1
     O                                           52 'ACTIVITY'
     O                                          101 'MISC'
     O          E            HEADTM         2
     O                                           18 'DATE'
     O                                           37 'INTERVIEWER'
     O                                           50 'CODE'
     O                                           75 'PROFESSIONAL SERVICES'
     O                                           82 'TIME'
     O                                           93 'MILES'
     O                                          103 'CHARGES'
     O          E            DETTM          1
     O*** BEG CHG *** 11/17/99 **************************************
     O**********************   XXSVDT    20 '  /  /  '
     O                       XXSVDT              20 '0 /  /    '
     O*** END CHG *** 11/17/99 **************************************
     O                       XXINTR              42
     O*** BEG CHG *** 11/17/99 **************************************
     O**********               XXACT     50
     O**********               XXACDS    74
     O                       XXACT               49
     O                       XXACDS              74
     O*** END CHG *** 11/17/99 **************************************
     O                       XXHOUR        M     82
     O                       XXMILE        Z     92
     O                       XXMISC        M    103
     O          E            TOTTM       2  1
     O                                           65 'TOTAL TIME/MILES:'
     O                       TOTEBH        M     82
     O                       TOTEBM        Z     92
     O          E            TOTTM          1
     O                                           65 'TIME/MATERIALS CHARGES:'
     O                       TOTHCH        M     82
     O                       TOTMCH        M     93
     O                       TOTEBC        M    103
     O          E            TOTTM          1
     O                                           65 'CALCULATED REVIEWER FEE:'
     O                       XXRFEE        M     82
     O          E            TOTTM          1
     O                                           79 '======'
     O                                          103 '========================'
     O          E            TOTTM          1
     O                                           65 'TOTAL INVOICED PER EOMB:'
     O                       XXBAMT        M     82
     O**
     OQSYSPRT1  E            ROUTE1         1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE1         1
     O**********     08      ORDIS2              60
     O**********E            ROUTE1         1
     O**********     08      ORDIS3              60
     O**********E            ROUTE1         2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE1         1
     O               08      ORDIS2              60
     O          E            ROUTE1         1
     O               08      ORDIS3              60
     O          E            ROUTE1         2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                 N97   ORTITL              80
     O                  97                       35 'PBR/HBA RE-BILL'
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE1         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE1         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE1
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               57 'INC. 2000     '
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT1  E            HEAD1          1 06
     O*** BEG CHG *** 11/17/99 ************************
     O*********************    DSDATE   172 '0  /  /  '
     O                       XSDATE             172 '0  /  /    '
     O*** END CHG *** 11/17/99 ************************
     O                       XXCONM             113
     O                       DSTIME               8 '  :  :  '
     O          E            HEAD1          1
     O                                           70 '* PRINT ONLY *'
     O          E            HEAD1          1
     O                       ORPNAM              10
     O                 N97   ORTITL             118
     O                  97                      111 'PBR/HBA RE-BILL'
     O                       PAGE1              172
     O                                          166 'PAGE:'
     O          E            HEAD1C         2
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#        Z     18
     O                                           20 '-'
     O                       XXSBNM              62
     O          E            HEAD1C         3
     O                                           70 'FINAL TOTALS'
     O          E            HEAD3          1
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#        Z     18
     O                                           20 '-'
     O                       XXSBNM              62
     O          E            HEAD3          2
     O                                            6 'GROUP:'
     O                       XXFND               18
     O                                           20 '-'
     O                       XXFNNM              62
     O          E            HEAD3          1
     O                                           69 'ACCIDENT'
     O                                          121 'HBA'
     O                                          133 'PBR'
     O                                          148 'PRE/RE-CERT'
     O*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O                                          160 'CASE'
     O*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O*** BEGIN CHG ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O**********************************148 '# BILLS'
     O                                          172 '# BILLS'
     O*** BEGIN CHG ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O          E            HEAD3          2
     O                                            9 'FILE #'
     O                                           27 'INV #'
     O                                           41 'EOB#/SVC#'
     O*** BEGIN CHG *** 01/15/99 **************************************
     O**********                         54 'SOC. SEC. #'
     O**********                         62 'DATE'
     O**********                         77 'PATIENT NAME'
     O**********                        105 'BILL AMT'
     O**********                        119 'BILL AMT'
     O**********                        135 'BILL AMT'
     O*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O**********                        151 'BILL AMT'
     O*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O*** BEGIN CHG ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O**********************************148 'PER EOB'
     O**********                        164 'PER EOB'
     O*** BEGIN CHG ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O                                           45 'RCN'
     O                                           59 'SOC. SEC. #'
     O                                           67 'DATE'
     O*** BEG CHG *** 11/17/99 ****************************************
     O******************************     82 'PATIENT NAME'
     O                                           83 'PATIENT NAME'
     O*** END CHG *** 11/17/99 ****************************************
     O                                          124 'BILL AMT'
     O                                          136 'BILL AMT'
     O                                          149 'BILL AMT'
     O                                          162 'BILL AMT'
     O                                          172 'PER EOB'
     O*** END   CHG *** 01/15/99 **************************************
     O          E            HEAD4       1  2
     O                                           70 'HBA'
     O                                           83 'PBR'
     O                                           96 'PRE/RE-CERT'
     O                                          109 'CASE'
     O                                          122 'TOTAL'
     O          E            DETL1          1
     O*** BEGIN CHG *** 03/09/00 **************************************
     O**********               XF#       20
     O*** BEG CHG *** 11/17/99 **************************************
     O**********               XXINV#Z   30
     O**********                         30 '**TRIAL**'
     O                       XF#                 21
     O                                           31 '**TRIAL**'
     O*** END CHG *** 11/17/99 **************************************
     O*** END   CHG *** 03/09/00 **************************************
     O                       XXEOB#        Z     41
     O*** BEGIN CHG *** 01/15/99 **************************************
     O**********               XXSS#     54 '   -  -    '
     O**********               XXACDT    64 '  /  /  '
     O**********               XXPNAM    92
     O**********       99N94   XXAMT K  106
     O**********      N99N94   XXAMT K  120
     O**********       94N99   XXAMT K  136
     O*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O**********       94 99   XXAMT K  152
     O*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O*
     O*** BEGIN CHG ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O*************************XX#BLLZ  148
     O**********               XX#BLLZ  164
     O*** END   CHG ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O                       XXRCN               45
     O*** BEGIN CHG *** 08/03/2006 ************************************
     O**********               XXSS#     59 '0   -  -    '
      ***** Beg Chg *** 06/09/20 *******************************************
     O***           N71      XXSS#               59 '0   -  -    '
     O              N71      XXSS#               59
      ***** End Chg *** 06/09/20 *******************************************
     O               71      SS#                 59
     O*** END   CHG *** 08/03/2006 ************************************
     O*** BEG CHG *** 11/17/99 ****************************************
     O**********************   XXACDT    70 '0  /  /  '
     O**********************   XXPNAM    97
     O                       XXACDT              70 '0  /  /    '
     O                       XXPNAM              98
     O*** END CHG *** 11/17/99 ****************************************
     O               99N94   XXAMT              123 '      0 .  '
     O              N99N94   XXAMT              136 '      0 .  '
     O               94N99   XXAMT              149 '      0 .  '
     O               94 99   XXAMT              162 '      0 .  '
     O                       XX#BLL             172 ' 0 '
     O*** END   CHG *** 01/15/99 **************************************
     O*** CASE TOTALS
     O          E            CASTOT      1  2
     O                                           12 'CASE TOTALS:'
     O                                           31 '# HBA BILLS:'
     O                       XX#CHB         B    36 '  0 '
     O                                           50 '# PBR BILLS:'
     O                       XX#CPB         B    55 '  0 '
     O                                           70 '# CERT BILLS:'
     O                       XX#CCT         B    75 '  0 '
     O                                           90 '# CASE BILLS:'
     O                       XX#CMD         B    95 '  0 '
     O                                          104 '# EOBS:'
     O                       XX#EOB         B   110 ' 0 '
     O                       XXCHBA         B   123 '      0 .  '
     O                       XXCPBR         B   136 '      0 .  '
     O                       XXCCRT         B   149 '      0 .  '
     O*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O                       XXCMED         B   162 '      0 .  '
     O*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O*
     O*** GROUP TOTALS
     O          E            GRPTOT      1  2
     O                                            5 'GROUP'
     O                       XXFND                9
     O                                           17 'TOTALS:'
     O                                           31 '# HBA BILLS:'
     O                       XX#GHB         B    36 '   0 '
     O                                           50 '# PBR BILLS:'
     O                       XX#GPB         B    55 '   0 '
     O                                           70 '# CERT BILLS:'
     O                       XX#GCT         B    75 '   0 '
     O*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O                                           89 '# MED BILLS'
     O                       XX#GMD         B    94 '   0 '
     O*
     O                                          103 '# EOBS:'
     O                       XXGEOB         B   109 '  0 '
     O*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O*** BEGIN CHG ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O*************************XXGHBAKB 106
     O*************************XXGPBRKB 120
     O*************************XXGCRTKB 136
     O                       XXGHBA         B   122 '      0 .  '
     O                       XXGPBR         B   135 '      0 .  '
     O                       XXGCRT         B   148 '      0 .  '
     O*** END   CHG ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O                       XXGMED         B   161 '      0 .  '
     O*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O*
     O          E            TOTAL1         2
     O                                           57 'TOTAL NUMBER BILLED:'
     O                       XX#THB              70 '   0 '
     O                       XX#TPB              83 '   0 '
     O                       XX#TCT              96 '   0 '
     O                       XX#TMD             109 '   0 '
     O*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O                       XX#TTO             122 '    0 '
     O*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O*
     O          E            TOTAL1         2
     O                                           57 'TOTAL AMOUNT BILLED:'
     O                       XXTHBA              70 '      0 .  '
     O                       XXTPBR              83 '      0 .  '
     O                       XXTCRT              96 '      0 .  '
     O*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O                       XXTMED             109 '      0 .  '
     O*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O*** BEGIN CHG ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O*************************XXTALLJ  128
     O                       XXTALL             122 '      0 .  '
     O*** END   CHG ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O*
     O          E            TOTAL1         2
     O                                           57 'NUMBER OF INVOICES :'
     O                       XX#INV             122 '    0 '
     O*
     O          E            MSG            2
     O                                           21 '*** NO INVOICES TO BE'
     O                                           35 'TO BE PROCESSED FOR'
     O                                           47 'SUBSCRIBER:'
     O                       XXSUB#        Z     53
     O                                           55 '-'
     O                       XXSBNM              91
     O          E            MSGHD          2
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#        Z     18
     O                                           20 '-'
     O                       XXSBNM              62
     O          E            MSGHD          2
     O                                            6 'GROUP:'
     O                       XXFND               18
     O                                           20 '-'
     O                       XXFNNM              62
     O          E            MSGHD          1
     O                                           69 'ACCIDENT'
     O          E            MSGHD          2
     O                                            9 'FILE #'
     O                                           27 'INV #'
     O                                           41 'EOB#/SVC#'
     O*** BEGIN CHG *** 01/15/99 **************************************
     O**********                         54 'SOC. SEC. #'
     O**********                         62 'DATE'
     O**********                         77 'PATIENT NAME'
     O                                           45 'RCN'
     O                                           59 'SOC. SEC. #'
     O                                           67 'DATE'
     O*** BEG CHG *** 11/17/99 ****************************************
     O********************************   82 'PATIENT NAME'
     O                                           83 'PATIENT NAME'
     O*** END CHG *** 11/17/99 ****************************************
     O*** END   CHG *** 01/15/99 **************************************
     O          E            MSGDT          1
     O*** BEGIN CHG *** 03/09/00 **************************************
     O***********              XF#       20
     O*** BEG CHG *** 11/17/99 **************************************
     O***********              XXINV#Z   30
     O***********                        30 '**TRIAL**'
     O*** END CHG *** 11/17/99 **************************************
     O                       XF#                 21
     O                                           31 '**TRIAL**'
     O*** END   CHG *** 03/09/00 **************************************
     O                       XXEOB#        Z     41
     O*** BEGIN CHG *** 01/15/99 **************************************
     O**********               XXSS#     54 '   -  -    '
     O**********               XXACDT    64 '  /  /  '
     O**********               XXPNAM    92
     O                       XXRCN               45
     O*** BEGIN CHG *** 08/03/2006 ************************************
     O**********               XXSS#     59 '0   -  -    '
      ***** Beg Chg *** 06/09/20 *******************************************
     O***           N71      XXSS#               59 '0   -  -    '
     O              N71      XXSS#               59
      ***** End Chg *** 06/09/20 *******************************************
     O               71      SS#                 59
     O*** END   CHG *** 08/03/2006 ************************************
     O                       XXACDT              70 '  /  /    '
     O*** BEG CHG *** 11/17/99 ****************************************
     O*******************      XXPNAM    97
     O                       XXPNAM              98
     O*** END CHG *** 11/17/99 ****************************************
     O*** END   CHG *** 01/15/99 **************************************
     O          E            MSG2        3
     O                                           60 'ALL EOBS FOR THIS INVOIC'
     O                                           80 'E HAVE BEEN DELETED.'
     O          E            MSG2        1
     O                                           56 'INVOICE #:'
     O*** BEG CHG *** 11/17/99 **************************************
     O*********************    XXINV#Z   68
     O                                           68 '**TRIAL**'
     O*** END CHG *** 11/17/99 **************************************
     O          E            MSG2        1
     O                                           60 'CREDIT AMOUNT:'
     O                       XXCRAM        4     76
     OQSYSPRT2  E            ROUTE2         1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE2         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE2         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE2         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE2         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE2         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE2         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE2         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE2         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE2         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE2         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE2         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE2         1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE2         1
     O**********     08      ORDIS2              60
     O**********E            ROUTE2         1
     O**********     08      ORDIS3              60
     O**********E            ROUTE2         2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE2         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE2         1
     O               08      ORDIS2              60
     O          E            ROUTE2         1
     O               08      ORDIS3              60
     O          E            ROUTE2         2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE2         2
     O                                           19 'REPORT TITLE:'
     O                 N97   ORTITL              80
     O                  97                       35 'PBR/HBA RE-BILL'
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE2         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE2         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE2
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               57 'INC. 2000     '
     O          E            ROUTE2         1 59
     O                       #BRND1
     O          E            ROUTE2         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT2  E            HEADS          1 01
     O*
     O*
     O                                          112 'INVOICE #:'
     O*** BEG CHG *** 11/17/99 **************************************
     O************************ XXINV#Z  122
     O                                          122 '**TRIAL**'
     O*** END CHG *** 11/17/99 **************************************
     O          E            HEADS          1
     O                                          112 'PAGE:'
     O                       PAGE               122
     O          E            HEADS          1
     O                                          112 'DATE:'
     O*** BEG CHG *** 11/17/99 ************************
     O**********************   DSDATEY  122
     O                       XSDATE        Y    122
     O*** END CHG *** 11/17/99 ************************
     O*
     O*
     O          E            HEADS          1 08
     O              N97                          70 '* PRINT ONLY *'
     O          E            HEADS          2
     O              N97                          70 '*** INVOICE ***'
     O               97                          70 'ADJUSTED INVOICE'
     O          E            HEADS          1
     O                                            9 'SUBSC  :'
     O                       XXSUB#        Z     15
     O                                           17 '-'
     O                       XXSBNM              53
     O          E            HEADS          3
     O                                            9 'GROUP  :'
     O                       XXFND         Z     15
     O                                           17 '-'
     O                       XXFNNM              53
     O          E    30      DETS           1
     O                                           25 'TOTAL H SERVICES:'
     O                       XHSYST        M     40
     O          E    31      DETS           1
     O                                           25 'TOTAL P SERVICES:'
     O                       XPSYST        M     40
     O          E    32      DETS           1
     O                                           25 'TOTAL U SERVICES:'
     O                       XUSYST        M     40
     O*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O          E    33      DETS           1
     O                                           25 'TOTAL M SERVICES:'
     O                       XMSYST        M     40
     O*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     O          E            DETS           1
     O                                           40 '============'
     O          E            TOTS           2
     O                                           25 'PLEASE PAY THIS AMOUNT'
     O                       XASYST        4B    40 '$'
     O                                           54 'ON OR BEFORE'
     O                       XDUEDT        Y     65
     O          E            TOTS        3  1
     O                                           52 'PLEASE MAIL REMITTANCE'
     O                                           56 'TO:'
     O                       XCNAME             100
     O          E            TOTS           1
     O                       XCADR1             100
     O          E    94      TOTS           1
     O                       XCADR2             100
     O          E            TOTS           1
     O                       XCCSZ              100
     O          E            TOTS           2
     O                       XCPHN#              74
     O          E            TOTS           1
     O                                           56 'TAX ID #:'
     O                       XXFEIN        Z     69
     O**
     O*********PC101   E                RLSC1
** (DS)
LEVEL I - HOSPITAL BILL AUDIT
PROVIDER BILL REVIEW
ORIGINAL BILLING HBA
ORIGINAL BILLING PBR
PRE/RE-CERT REVIEW
CASE MANAGEMENT
