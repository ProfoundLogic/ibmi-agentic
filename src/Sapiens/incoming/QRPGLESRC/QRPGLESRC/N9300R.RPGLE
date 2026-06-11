     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H*****************************************************************
    H* (C) 2010 StoneRiver, Inc. All rights Reserved.                *
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H** PROGRAM:   N9300R - PRINT BILLS/RE-BILLS                    **
     H** DESCRIPTION:  PRINT PROGRAM - ORIGINAL/ REBILLING           **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** REVISIONS:
     H*
     H* 06/09/20  C:212 Project : SY420                               *
     H*                 Mask SSN                                      *
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10 C:249 Project: SY215                                 *
     H*          C:189 Retrieve brand Name                            *
     H*                                                               *
     H*  04/28/00 C:62  PROJECT: AR001
     H*                 WRITE NMIMP RECORD.
     H*
     H*  11/12/99  C:62  PROJECT: MCMNT
     H*                  PASS MODE PARM TO AA001R.
     H*
     H*  03/06/99  C:94  PROJECT: MC152
     H*                  ADD RECON NUMBER
     H*
     H*  01/11/99  C:94  PROJECT: MC152
     H*                  CHANGE PROGRAM TO USE API TO UPDATE NATRP,
     H*                  AND TO USE FAEAP IF PROVIDER BILLS IS
     H*                  INSTALLED.
     H*
     H*  01/04/99 C:31  Project: MC152
     H*                 1. Change size of invoice number
     H*
     H** 9/05/89  CONNIE TERRY - CREATE PGM TO PRINT BILLS IN A      **
     H**          BATCH MODE, PRINT THE EDIT REPORT TO SUMMERIZE,    **
     H**          AND UPDATE THE FILES WITH INVOICE INFO.            **
     H**          PROJECT : A9C                                      **
     H**                                                             **
     H** 9/22/89  C:05- ADDED PARM &INV BECAUSE ITS NEEDED IF A      **
     H** PROJ #: A9C    RE-BILL IS DONE WHERE ALL EOBS HAVE BEEN     **
     H**                DELETED                                      **
     H**                                                             **
     H** 10/5/89  CST - ADDED CODE TO VERIFY INVOICE AMT IS GREATER  **
     H** PROJ #: A9C    THAN ZERO BEFORE PRINTING AN INVOICE.        **
     H**                                                             **
     H** 10/13/89 C:05- MOVED TRFRDT/TRTODT CODE BECAUSE IT WAS ONLY **
     H** PROJ #: A9C    BEING EXECUTED ON THE FIRST RECORD READ.     **
     H**                EVERY RECORD AFTER THAT WAS USING THE SAME   **
     H**                FROM/TO SERVICE DATES.                       **
     H**                                                             **
     H*  08/26/92 C:05  PROJ: 99AAC                                  **
     H*                 Changed all instances of TRFLG4 to TRBLFL    **
     H*                                                              **
     H*  12/11/92 C:20  PROJ: 99AAI                                  **
     H*                 ADDED PRINTING OF CREDIT AMOUNT IF ALL EOMBS **
     H*                 FOR AN INVOICE ARE DELETED                   **
     H*                                                              **
     H*  12/21/92 C:20  PROJ: 99AAE                                  **
     H*                 SUPPRESS ZERO ON PRINTED INVOICE NUMBER      **
     H*                                                              **
     H*  03/26/93 C:20  PROJ: 58/MC018                               **
     H*                 1.ADD PROCESS OF SUMMARY INVOICE             **
     H*                   DEPENDING ON SUBSCRIBER A/R ROUTINE TYPE   **
     H*                   AND A/R ROUTINE TYPE PRINT THEY WILL GET   **
     H*                   STANDARD INVOICE PER CASE OR A SUMMARY     **
     H*                   INVOICE AND DETAIL INVOICES OR ONLY A      **
     H*                   SUMMARY INVOICE                            **
     H**                                                             **
     H*  08/18/93 C:20   REQ/PROJ: ARMNT                             **
     H*                  1. CHANGED KEY FOR NMARL8 TO BE:            **
     H*                     CO/SUB/INV                   FROM        **
     H*                     CO/INV/SUB                               **
     H*                                                              **
     H*  08/30/93 C:20  PROJ: ARMNT                                  **
     H*                 SRAR SR - ADDED CODE IF NO HIT WHEN CHAINING **
     H**                TO WORKFILE DO NOT WRITE A/R RECORD          **
     H**                                                             **
     H*  11/06/93 C:20   REQ/PROJ: ARMNT                             **
     H*                  1. IF NO RECORDS ARE SELECTED TO PRINT      **
     H*                     CHANGED MESSAGE TO PRINT NO INVOICES FOR **
     H*                     THIS SUBSCRIBER RATHER THAN THIS COMPANY **
     H*                                                              **
     H*  02/03/95 C:20   REQ/PROJ: ARMNT                             **
     H*                  1. MODIFICATION SO THAT IF DOING A REINVOICE**
     H*                     AND FIRST RECORD NO BILLING AMOUNT STILL **
     H*                     GET CLAIMANT HEADING INFORMATION         **
     H*                                                              **
     H*  10/26/95 C:20   REQ/PROJ: ARMNT                             **
     H*                  1. MOVE REVIEWER FEE FIELD IN TWO PLACES    **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** INDICATOR USAGE:                                            **
     H**                                                             **
     H**  21       21 ON MEANS AT LEAST ONE INVOICE AMT IS GREATER   **
     H**           THAN 0 AND AT LEAST ONE INVOICE WILL BE PRODUCED. **
     H**           THIS PREVENTS THE EXCPT ON HEAD AND TOT FROM      **
     H**           SRCASE WHICH WOULD PRODUCE A BLANK INVOICE.       **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     F*** BEG CHG *** 01/11/99 ***************************************
     FNATRL     IF   E           K DISK
     F**********NATRL   UF  E           K        DISK
     F*** END CHG *** 01/11/99 ***************************************
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFAEOL4    IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FFMC1L     UF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F*** BEG ADD *** 01/11/99 *************************************
     F                                     INFDS(DBFDS)
     F*** END ADD *** 01/11/99 *************************************
     FFMPDL     IF   E           K DISK
     FNMARP     O    E             DISK
     FNMARL8    IF   E           K DISK
     F                                     RENAME(PAR01:PAR02)
     F*** BEGIN ADD *** 04/28/00 **************************************
     FNMIMP     O    E             DISK
     F*** END   ADD *** 04/28/00 **************************************
     FFMSML     IF   E           K DISK
     F*
     FFNEOP     IF   E           K DISK
     F                                     RENAME(PEO01:PEN01)
     F*** BEG ADD *** 01/11/99 ***************************************
     FFAEAL1    IF   E           K DISK
     F*** END ADD *** 01/11/99 ***************************************
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

     D DS              S             30    DIM(5) CTDATA PERRCD(1)              BILLING DESC.
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D*** FILE ARRAYS
     D*
     D                 DS
     D  XSZIP                  1      9  0
     D  S1ZPP1                 1      5  0
     D  S1ZPP2                 6      9  0
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
     D  XF#                    1     20
     D  XXSUB#                 1      5  0
     D  XFHYP1                 6      6
     D  XXFND                  7      9  0
     D  XFHYP2                10     10
     D  XXYR                  11     12  0
     D  XFHYP3                13     13
     D  XXCASE                14     20  0
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
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** BEG ADD *** 01/11/99 ***************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
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

     d***** Beg Add *** 06/09/20 *******************************************
     d xxss#           s             11    inz
     d***** End Add *** 06/09/20 *******************************************

     I*
     IPEN01
     I              EOCO#                       XNCO#
     I              EOFND                       XNFND
     I              EOFYR                       XNFYR
     I              EOCASE                      XNCASE
     I              EOTRDT                      XNTRDT
     I              EOFEIN                      XNFEIN
     I              EOMOD                       XNMOD
     I              EOAMT                       XNAMT
     I              EOFRDT                      XNFRDT
     I              EOTODT                      XNTODT
     I              EOTRCD                      XNTRCD
     I              EOPNAM                      XNPNAM
     I              EOADR1                      XNADR1
     I              EOADR2                      XNADR2
     I              EOCTY                       XNCTY
     I              EOST                        XNST
     I              EOZIP                       XNZIP
     I              EOEOB1                      XNEOB1
     I              EOEOB2                      XNEOB2
     I              EOEOB3                      XNEOB3
     I              EOCHK#                      XNCHK#
     I              EOEOB#                      XNEOB#
     I              EOSYS                       XNSYS
     I              EOFLG1                      XNFLG1
     I              EOFLG2                      XNFLG2
     I              EOFLG3                      XNFLG3
     I              EOFLG4                      XNFLG4
     I              EOFLG5                      XNFLG5
     I              EOFLG6                      XNFLG6
     I              EOFLG7                      XNFLG7
     I              EOFLG8                      XNFLG8
     I              EOFLG9                      XNFLG9
     I              EOAMTC                      XNAMTC
     I              EOSTAT                      XNSTAT
     I              EOUSER                      XNUSER
     I              EOPGM                       XNPGM
     I              EOTIME                      XNTIME
     I              EODT                        XNDT
     I              EOIUSR                      XNIUSR
     I              EOIDT                       XNIDT
     I              EOWCCK                      XNWCCK
     I              EODIA1                      XNDIA1
     I              EODIA2                      XNDIA2
     I              EODIA3                      XNDIA3
     I              EODIA4                      XNDIA4
     I              EOEMPL                      XNEMPL
     I              EODFC                       XNDFC
     I              EOFLIC                      XNFLIC
     I              EOTAPE                      XNTAPE
     I              EO#BLL                      XN#BLL
     I              EOCLSF                      XNCLSF
     I              EOAST                       XNAST
     I              EOSUB#                      XNSUB#
     I              EOAUDT                      XNAUDT
     I              EOBTFL                      XNBTFL
     I              EOHRFL                      XNHRFL
     I              EOBLFL                      XNBLFL
     I              EONLFL                      XNNLFL
     I              EODIFL                      XNDIFL
     I              EOINV#                      XNINV#
     I              EORMDS                      XNRMDS
     I              EORMK1                      XNRMK1
     I              EORMK2                      XNRMK2
     I              EORMK3                      XNRMK3
     I              EOCKCN                      XNCKCN
     I              EOOTHR                      XNOTHR
     I              EOSB01                      XNSB01
     I              EOSB02                      XNSB02
     I              EOSB03                      XNSB03
     I              EOSB04                      XNSB04
     I              EOSB05                      XNSB05
     I              EOSB06                      XNSB06
     I              EOSB07                      XNSB07
     I              EOSB08                      XNSB08
     I              EOSB09                      XNSB09
     I              EOSB10                      XNSB10
     I              EOBAMT                      XNBAMT
     I              EOZIPA                      XNZIPA
     I              EOCERT                      XNCERT
     I              EOEXT                       XNEXT
     I*
     I*** END ADD *** 01/11/99 ***************************************
     I*
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
     C                   KFLD                    TRFEIN
     C                   KFLD                    TRMOD
     C*
     C*** BEG ADD *** 01/11/99 ***************************************
     C*
     C     PDKEY1        KLIST
     C                   KFLD                    EAFEIN
     C                   KFLD                    EAMOD
     C*
     C*** END ADD *** 01/11/99 ***************************************
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
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
     C* KEY LIST FOR THE A/R TRANSACTION FILE
     C*
     C     ARKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#                         08/18/93 MOVED
     C                   KFLD                    XXINV#                         SUB BEFORE INV
     C*
     C*  EOB FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    TRCO#
     C                   KFLD                    TRSUB#
     C                   KFLD                    TRFND
     C                   KFLD                    TRFYR
     C                   KFLD                    TRCASE
     C                   KFLD                    TREOB#
     C*
     C*** BEG ADD *** 01/11/99 ***************************************
     C*
     C     EOKEY1        KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C     ENKEY1        KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    XXINV#
     C                   KFLD                    EAEOB#
     C*
     C*** END ADD *** 01/11/99 ***************************************
     C* KEY LIST RE-BILL WORK FILE
     C*
     C     ENKEY         KLIST
     C                   KFLD                    TRCO#
     C                   KFLD                    TRSUB#
     C                   KFLD                    TRFND
     C                   KFLD                    XXINV#
     C                   KFLD                    TREOB#
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
     C     *LIKE         DEFINE    EONDIS        XXNDIS
     C     *LIKE         DEFINE    EOAMTC        XXAMTC
     C                   Z-ADD     *ZEROS        XNSYST           10 2          ADD 03/26/93
     C*
     C*** BEG ADD *** 01/11/99 ***************************************
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
     C*** END ADD *** 01/11/99 ***************************************
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C*** BEGIN CHG *** 01/04/99 **************************************
     C********             MOVE QQINV#    XXINV#  50       9/22/89 ADD
     C                   MOVE      QQINV#        XXINV#            9 0
     C*** END CHG *** 01/04/99 ****************************************
     C                   MOVE      QQMODE        XXMODE            1
     C*
     C                   TIME                    DSDTTM
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
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C***********        MOVE      '1'           XXPRT
     C***********     ORKEY         CHAIN     POR01                              60
     C***********     *IN60         IFEQ      '0'
     C***********        EXCEPT    ROUTE1
     C***********        END                                                    END IFEQ '0'
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
     C                   ELSE
     C                   MOVEL     SMNAME        XXSBNM           35
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
     C*  DETERMINE IF THERE ARE ANY BILLS TO PROCESS
     C*
     C*** BEG CHG *** 01/11/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C                   READ      PEA01                                  50
     C                   ELSE
     C                   READ      PTR01                                  50
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/11/99 ***************************************
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
     C*** BEG ADD *** 03/06/99 ***************************************
     C                   Z-ADD     ARRCN         XXRCN
     C*** END ADD *** 03/06/99 ***************************************
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
     C     C1KEY         CHAIN     PC101                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    RLSC1
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
     C  N97              MOVE      C1LIN#        XXINV#            9 0
     C*** END CHG *** 01/04/99 ****************************************
     C*** BEG CHG *** 01/11/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C   97              MOVE      EANINV        XXINV#                         USE SAME INV#
     C                   ELSE
     C   97              MOVE      TRINV#        XXINV#                         USE SAME INV#
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/11/99 ***************************************
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
     C*** GET ROUTING INFORMATION FOR BILLS
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C*********     *IN91         IFEQ      '1'
     C*********     *IN93         IFEQ      '1'
     C*********          MOVE      '0'           XXPRT
     C*********     ORKEY         CHAIN     POR01                              60
     C*********     *IN60         IFEQ      '0'
     C*********          EXCEPT    ROUTE
     C*********          END                                                    END IFEQ '0'
     C*********          MOVE      '2'           XXPRT
     C*********     ORKEY         CHAIN     POR01                              60
     C*********     *IN60         IFEQ      '0'
     C*********          EXCEPT    ROUTE2
     C*********          END                                                    END IN60=0
     C*********          ELSE                                                   ELSE 93 =0
     C*********          MOVE      '2'           XXPRT
     C*********     ORKEY         CHAIN     POR01                              60
     C*********     *IN60         IFEQ      '0'
     C*********          EXCEPT    ROUTE2
     C*********          END                                                    END IN60=0
     C*********          END                                                    END *IN93
     C*********          ELSE                                                   ELSE IN91=0
     C*********          MOVE      '0'           XXPRT
     C*********     ORKEY         CHAIN     POR01                              60
     C*********     *IN60         IFEQ      '0'
     C*********          EXCEPT    ROUTE
     C*********          END                                                    END IFEQ '0'
     C*********          END                                                    END IN91
      ***** End Del ***** 08/26/14 *********************************************
     C                   MOVE      *BLANKS       XXFFLG            1
     C*
     C*  PRINT REPORT HEADING
     C*
     C                   EXCEPT    HEAD1
     C*
     C*** READ EOB FILE
     C*
     C*** BEG CHG *** 01/11/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     *LOVAL        SETLL     PEA01
     C                   READ      PEA01                                  50
     C                   ELSE                                                   ##PRVB = Y
     C     *LOVAL        SETLL     PTR01
     C                   READ      PTR01                                  50
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/11/99 ***************************************
     C*
     C     *IN50         IFEQ      '0'
     C*** BEG CHG *** 01/11/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C                   Z-ADD     EACO#         XXCO#             3 0
     C                   Z-ADD     EASUB#        XXSUB#            5 0
     C                   Z-ADD     EAFND         XXFND             3 0
     C                   Z-ADD     EAFYR         XXFYR             3 0
     C                   Z-ADD     EAFYR         XXYR
     C                   Z-ADD     EACASE        XXCASE            7 0
     C                   ELSE                                                   ##PRVB = Y
     C                   Z-ADD     TRCO#         XXCO#             3 0
     C                   Z-ADD     TRSUB#        XXSUB#            5 0
     C                   Z-ADD     TRFND         XXFND             3 0
     C                   Z-ADD     TRFYR         XXFYR             3 0
     C                   Z-ADD     TRFYR         XXYR
     C                   Z-ADD     TRCASE        XXCASE            7 0
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/11/99 ***************************************
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
     C*** VERIFY INVOICE AMT GREATER THAN ZERO
     C*
     C                   EXSR      SRAMT
     C*
     C*** BEG CHG *** 01/11/99 ***************************************
     C*
     C     ##PRVB        IFEQ      'Y'
     C                   Z-ADD     EAEOB#        XXEOB#
     C*** BEG ADD *** 03/06/99 ***************************************
     C                   Z-ADD     EARCN         XXRCN             3 0
     C*** END ADD *** 03/06/99 ***************************************
     C                   ELSE                                                   #PRVB = Y
     C                   Z-ADD     TREOB#        XXEOB#
     C                   ENDIF                                                  #PRVB = Y
     C*** END CHG *** 01/11/99 ***************************************
     C*
     C*** IF CASE CHANGES, PRINT CASE TOTALS
     C*
     C*** BEG CHG *** 01/11/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
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
     C                   END                                                    ENDNE XXCASE
     C                   ELSE                                                   ##PRVB = Y
     C     TRCASE        IFNE      XXCASE
     C     XXCASE        ANDNE     *ZEROS
     C                   EXSR      SRCASE
     C                   Z-ADD     TRFYR         XXFYR
     C                   EXSR      SRHEAD
     C     *IN91         IFEQ      '1'
     C     *IN93         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   END
     C                   ELSE
     C                   EXCEPT    HEAD
     C                   END
     C                   END                                                    ENDNE XXCASE
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/11/99 ***************************************
     C*
     C*** IF GROUP CHANGES, PRINT GROUP TOTALS, PAGE BREAK HEADINGS
     C*
     C*** BEG CHG *** 01/11/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
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
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C*
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTODT            6 0
     C*
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTRDT            6 0
     C*
     C                   MOVE      *BLANKS       XXDESC            3 0          NTW DESC
     C                   MOVEL     DS(1)         XXDESC                         NTW DESC
     C                   ADD       1             XX#CNT            4 0          CASE # PBR
     C                   Z-ADD     EANAMT        XXAMT             9 2
     C*
     C                   ADD       XXAMT         XXCNTW           10 2          CASE BILLD
     C*
     C                   ADD       1             XX#EOB            3 0          # OF EOBS/CASE
     C                   ADD       1             XXGEOB            4 0          # OF EOBS/GROUP
     C*
     C                   ELSE                                                   ##PRVB = Y
     C*
     C     TRFND         IFNE      XXFND
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
     C*
     C                   Z-ADD     TRFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C*
     C                   Z-ADD     TRTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTODT            6 0
     C*
     C                   Z-ADD     TRTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTRDT            6 0
     C*
     C                   MOVE      *BLANKS       XXDESC            3 0          NTW DESC
     C                   MOVEL     DS(1)         XXDESC                         NTW DESC
     C                   ADD       1             XX#CNT            4 0          CASE # PBR
     C                   Z-ADD     TRBAMT        XXAMT             9 2
     C*
     C                   ADD       XXAMT         XXCNTW           10 2          CASE BILLD
     C*
     C                   ADD       1             XX#EOB            3 0          # OF EOBS/CASE
     C                   ADD       1             XXGEOB            4 0          # OF EOBS/GROUP
     C*
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/11/99 ***************************************
     C*
     C* * * GET PROVIDER INFORMATION.
     C*
     C*** BEG CHG *** XX/XX/XX ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     PDKEY1        CHAIN     PPD01                              50
     C     *IN50         IFEQ      '0'
     C*
     C                   MOVEL     PDNAME        XXPRNM           20
     C                   MOVE      EAFEIN        XXPRV#            9 0
     C                   MOVE      EAMOD         XXMOD             3 0
     C                   ELSE
     C                   MOVE      *BLANKS       XXPRNM
     C                   MOVE      EAFEIN        XXPRV#
     C                   MOVE      EAMOD         XXMOD
     C                   END
     C                   ELSE                                                   ##PRVB = Y
     C     PDKEY         CHAIN     PPD01                              50
     C     *IN50         IFEQ      '0'
     C*
     C                   MOVEL     PDNAME        XXPRNM           20
     C                   MOVE      TRFEIN        XXPRV#            9 0
     C                   MOVE      TRMOD         XXMOD             3 0
     C                   ELSE
     C                   MOVE      *BLANKS       XXPRNM
     C                   MOVE      TRFEIN        XXPRV#
     C                   MOVE      TRMOD         XXMOD
     C                   END
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/11/99 ***************************************
     C*
     C     XXMOD         IFNE      *ZEROS
     C                   MOVE      '1'           *IN96                          NOT BLANK
     C                   ELSE
     C                   MOVE      '0'           *IN96                          BLANK
     C                   END
     C*
     C*** GET EOB RECORD FOR NETWORK DISCOUNT ...
     C*
     C*** BEG CHG *** 01/11/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EOKEY1        CHAIN     PEO01                              51
     C*
     C     *IN51         IFEQ      '0'
     C                   Z-ADD     EONDIS        XXNDIS                         DISCOUNT
     C                   Z-ADD     EOAMTC        XXAMTC                         AMT CHARGED
     C                   END                                                    *51 = 0
     C                   ELSE                                                   ##PRVB = Y
     C     EOKEY         CHAIN     PEO01                              51
     C*
     C     *IN51         IFEQ      '0'
     C                   Z-ADD     EONDIS        XXNDIS                         DISCOUNT
     C                   Z-ADD     EOAMTC        XXAMTC                         AMT CHARGED
     C                   END                                                    *51 = 0
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/11/99 ***************************************
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
     C*** UPDATE NET TRANS RECORD
     C*** BEG CHG *** 01/11/99 ***************************************
     C*
     C                   MOVE      *BLANKS       KKWRUP
     C*** BEGIN ADD *** 11/12/99 **************************************
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   MOVE      'U'           XXFAEA
     C*
     C                   ELSE
     C*** END   ADD *** 11/12/99 **************************************
     C                   MOVE      'U'           XXNATR
     C*** BEGIN ADD *** 11/12/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 11/12/99 **************************************
     C*
     C                   CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      XXEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/06/99 **************************************
     C                   PARM      XXRCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/06/99 **************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM                    ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM      'P'           ##NBLF            1            NWS INV PROC FLAG
     C                   PARM      XXINV#        ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*** BEGIN ADD *** 11/12/99 **************************************
     C                   PARM      *BLANKS       ##BPFL            1                        ER
     C                   PARM      QQMODE        ##MODE            1                        ER
     C*** END   ADD *** 11/12/99 **************************************
     C*
     C*** END CHG *** 01/11/99 ***************************************
     C*** BEG CHG *** XX/XX/XX ***************************************
     C     READN         TAG
     C     ##PRVB        IFEQ      'Y'
     C                   READ      PEA01                                  50
     C                   ELSE
     C                   READ      PTR01                                  50
     C                   ENDIF
     C*** END CHG *** XX/XX/XX ***************************************
     C*
     C                   END                                                    END DOWEQ 50
     C     *IN91         IFEQ      '1'
     C                   EXCEPT    DETS
     C                   EXCEPT    TOTS
     C                   END
     C*
     C*** END OF PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
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
     C                   ADD       XXTNTW        XXTALL           10 2
     C*
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD1C
     C                   EXCEPT    HEAD4
     C                   EXCEPT    TOTAL1
     C*
     C*  UPDATE COMPANY AUXILARY FILE WITH LAST INVOICE # USED
     C*
     C     C1KEY         CHAIN     PC101                              60
     C     *IN60         IFEQ      '0'
     C  N97              Z-ADD     XXINV#        C1LIN#
     C                   UPDATE    PC101
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
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C*****************************************************************--*
     C*  CREATE A/R RECORD
     C*****************************************************************--*
     C*
     C     SRAR          BEGSR
     C*
     C                   MOVE      'N'           ARSYS                          ADD 12/08/92
     C                   ADD       XXAMT         XNSYST                         ADD 03/26/93
     C     *IN97         IFEQ      '1'
     C*** BEG CHG *** 01/11/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     ENKEY1        CHAIN     PEN01                              65
     C                   ELSE
     C     ENKEY         CHAIN     PEN01                              65
     C                   ENDIF
     C*** END CHG *** 01/11/99 ***************************************
     C     *IN65         IFEQ      '0'
     C     XNFLG1        IFEQ      *BLANKS
     C     XNDIFL        ANDEQ     *BLANKS
     C                   GOTO      NXTAR
     C                   END
     C                   ELSE                                                   ADD 08/30/93
     C                   GOTO      NXTAR                                        ADD 08/30/93
     C                   END
     C                   END
     C*
     C*** BEG CHG *** 01/11/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C                   MOVE      EACO#         ARCO#
     C                   MOVE      EASUB#        ARSUB#
     C                   MOVE      EAFND         ARFND
     C                   ELSE                                                   ##PRVB = Y
     C                   MOVE      TRCO#         ARCO#
     C                   MOVE      TRSUB#        ARSUB#
     C                   MOVE      TRFND         ARFND
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/11/99 ***************************************
     C     *IN90         IFEQ      '1'
     C*** BEG CHG *** 01/11/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C                   MOVE      EAFYR         ARFYR
     C                   MOVE      EACASE        ARCASE
     C                   ELSE                                                   ##PRVB = Y
     C                   MOVE      TRFYR         ARFYR
     C                   MOVE      TRCASE        ARCASE
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/11/99 ***************************************
     C                   ELSE
     C     *IN91         IFEQ      '1'
     C                   Z-ADD     *ZEROS        ARFYR
     C                   Z-ADD     *ZEROS        ARCASE
     C                   END
     C                   END
     C                   MOVE      *BLANK        ARTRCD
     C                   MOVE      'I'           ARTRTP
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARPODT
     C*
     C                   MOVEL     DS(1)         ARDESC
     C                   Z-ADD     XXAMT         ARAMT
     C*** BEG CHG *** 01/11/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C                   MOVE      EAEOB#        AREOB#
     C                   ELSE
     C                   MOVE      TREOB#        AREOB#
     C                   ENDIF
     C*** END CHG *** 01/11/99 ***************************************
     C                   MOVE      *ZERO         ARCK#
     C                   MOVE      'B'           ARSTAT
     C                   MOVE      Q1USER        ARUSER
     C                   MOVE      Q1PGM         ARPGM
     C                   Z-ADD     DSTIME        ARTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   Z-ADD     XXINV#        ARINV#
     C                   WRITE     PAR01
     C*** BEGIN ADD *** 04/28/00 **************************************
     C  N97              EXSR      WRITEI
     C*** END   ADD *** 04/28/00 **************************************
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
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
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
     C                   MOVEL     XXLAST        XXPNAM           30
     C                   ELSE
     C                   MOVEL     *BLANKS       XXPNAM           30
     C                   END
     C*
     C                   ELSE
     C                   Z-ADD     0             XXACDT            6 0
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      *ZERO         XXSS#             9 0
     C                   MOVE      *BLANKS       XXSS#
      ***** End Chg *** 06/09/20 *******************************************
     C                   MOVE      *BLANKS       XXCCLM           13
     C                   MOVEL     *BLANKS       XXPNAM           30
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
     C                   ADD       XX#CNT        XX#GNT            5 0          GROUP # NTWK
     C                   ADD       XXCNTW        XXGNTW           10 2          GROUP NTW BLLED
     C                   ADD       XXCNTW        XXCAMT           10 2          CASE TOTAL  LED
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
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    CASTOT
     C*
     C                   Z-ADD     0             XXCAMT
     C*** BEG CHG *** 01/11/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C                   Z-ADD     EAFYR         XXFYR
     C                   Z-ADD     EAFYR         XXYR
     C                   Z-ADD     EACASE        XXCASE
     C                   ELSE
     C                   Z-ADD     TRFYR         XXFYR
     C                   Z-ADD     TRFYR         XXYR
     C                   Z-ADD     TRCASE        XXCASE
     C                   ENDIF
     C*** END CHG *** 01/11/99 ***************************************
     C*
     C     *INLR         IFEQ      '0'
     C     *IN91         IFNE      '1'                                          ADD 03/26/93
     C  N97              ADD       1             XXINV#
     C                   END                                                    ADD 03/26/93
     C                   END
     C*
     C*** BEG CHG *** 01/11/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     *INLR         IFEQ      '0'
     C     EAFND         ANDNE     XXFND
     C     XXFND         ANDNE     *ZEROS
     C                   EXSR      SRGRP
     C                   EXCEPT    HEAD1
     C                   EXSR      SRHEAD
     C                   EXCEPT    HEAD3
     C                   END
     C*
     C                   ELSE
     C     *INLR         IFEQ      '0'
     C     TRFND         ANDNE     XXFND
     C     XXFND         ANDNE     *ZEROS
     C                   EXSR      SRGRP
     C                   EXCEPT    HEAD1
     C                   EXSR      SRHEAD
     C                   EXCEPT    HEAD3
     C                   END
     C*
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/11/99 ***************************************
     C                   ENDSR
     C*
     C*****************************************************************--*
     C*  PERFORM GROUP TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRGRP         BEGSR
     C*
     C                   ADD       XX#GNT        XX#TNT            5 0          TOTAL # NTW
     C                   ADD       XXGNTW        XXTNTW           10 2          TOTAL NTW BLLED
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    GRPTOT
     C*
     C                   Z-ADD     TRFND         XXFND
     C*
     C                   ENDSR
     C*****************************************************************--*
     C*  VERIFY BILLING AMT GREATER THAN 0 BEFORE PRODUCING INVOICE ...
     C*****************************************************************--*
     C*
     C     SRAMT         BEGSR
     C*
     C*** BEG CHG *** 01/11/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EANAMT        CABLE     *ZEROS        READN
     C                   ELSE                                                   ##PRVB = Y
     C     TRBAMT        CABLE     *ZEROS        READN
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/11/99 ***************************************
     C*
     C     *IN21         IFEQ      '0'
     C                   MOVE      '1'           *IN21
     C                   END
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 04/28/00 **************************************
     C*****************************************************************--*
     C*  WRITEI - WRITE NMIMP RECORD
     C*****************************************************************--*
     C     WRITEI        BEGSR
     C*
     C                   CLEAR                   PIM01
     C*
     C                   Z-ADD     ARCO#         IMCO#
     C                   Z-ADD     ARSUB#        IMSUB#
     C                   Z-ADD     ARFND         IMFND
     C                   Z-ADD     ARFYR         IMFYR
     C                   Z-ADD     ARCASE        IMCASE
     C                   Z-ADD     ARINV#        IMINV#
     C                   Z-ADD     ARTRDT        IMTRDT
     C*
     C                   MOVEL(P)  'A'           IMSTAT
     C                   MOVEL(P)  ARUSER        IMUSER
     C                   MOVEL(P)  ARUSER        IMIUSR
     C                   MOVEL(P)  ARPGM         IMPGM
     C                   MOVEL(P)  ARPGM         IMIPGM
     C*
     C                   Z-ADD     ARTIME        IMTIME
     C                   Z-ADD     ARTIME        IMITIM
     C                   Z-ADD     ARDT          IMDT
     C                   Z-ADD     ARDT          IMIDT
     C*
     C                   WRITE     PIM01
     C*
     C                   ENDSR
     C*** END   ADD *** 04/28/00 **************************************
     C*
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
     O****                               57 'INC. 1997     '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT   E            HEAD           1 01
     O*
     O                                          113 'INVOICE #:'
     O                       XXINV#        Z    122
     O          E            HEAD           1
     O                                          113 'PAGE:'
     O                       PAGE               122
     O          E            HEAD           1
     O                                          113 'DATE:'
     O                       DSDATE        Y    122
     O*
     O          E            HEAD           2 09
     O              N97                          70 '*** NETWORK INVOICE ***'
     O               97                          70 'ADJUSTED NETWORK INVOICE'
     O          E            HEAD           1
     O                                            9 'SUBSC  :'
     O                       XXSUB#              15
     O                                           17 '-'
     O                       XXSBNM              53
     O*
     O                                           79 'PATIENT:'
     O                       XXPNAM             115
     O          E            HEAD           1
     O                                            9 'GROUP  :'
     O                       XXFND         Z     15
     O                                           17 '-'
     O                       XXFNNM              53
     O                                           79 'SOC SEC:'
      ***** Beg Chg *** 06/09/20 *******************************************
     O***                    XXSS#               91 '   -  -    '
     O                       XXSS#               91
      ***** End Chg *** 06/09/20 *******************************************
     O          E            HEAD           1
     O                                           79 'ACC  DT:'
     O                       XXACDT        Y     88
     O          E            HEAD           2
     O               98                           9 'CLAIM #:'
     O               98      XXCCLM              23
     O                                           79 'FILE # :'
     O                       XF#                100
     O          E            HEAD           1
     O                                           91 'PROVIDER'
     O                                          105 'NETWORK'
     O          E            HEAD           2
     O*
     O                                            7 'DATE'
     O                                           19 'EOB#/SVC#'
     O                                           37 'SERVICE DATES'
     O                                           52 'PROVIDER'
     O                                           71 'PROVIDER NAME'
     O                                           90 'CHARGES'
     O                                          105 'SAVINGS'
     O                                          122 'REVIEWER FEE'
     O          E            DET            1
     O                       XXTRDT               9 '  /  /  '
     O                       XXEOB#        Z     19
     O                       XXFRDT              29 '  /  /  '
     O                                           31 '-'
     O                       XXTODT              40 '  /  /  '
     O                       XXPRV#        Z     53
     O               96                          54 '-'
     O               96      XXMOD         Z     57
     O                       XXPRNM              79
     O                       XXAMTC        4     90
     O                       XXNDIS        4    105
     O                       XXAMT         4    122
     O          E            TOT1           1
     O                                          122 '==========='
     O          E            TOT1           1
     O                       XXCAMT        4    122
     O*
     O          E            TOT            1 43
     O                                           53 'PLEASE PAY THIS AMOUNT:'
     O                       XXCAMT        4B    67 '$'
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
     O                  97                       35 'NETWORK RE-BILL'
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
     O****                               57 'INC. 1997     '
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT1  E            HEAD1          1 06
     O                       DSDATE             127 '  /  /  '
     O                       XXCONM              84
     O                       DSTIME               8 '  :  :  '
     O          E            HEAD1          1
     O                       ORPNAM              10
     O                 N97   ORTITL              94
     O                  97                       70 'NETWORK RE-BILL'
     O                       PAGE1              127
     O                                          121 'PAGE:'
     O          E            HEAD1C         2
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#              18
     O                                           20 '-'
     O                       XXSBNM              62
     O          E            HEAD1C         3
     O                                           70 'FINAL TOTALS'
     O          E            HEAD3          1
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#              18
     O                                           20 '-'
     O                       XXSBNM              62
     O          E            HEAD3          2
     O                                            6 'GROUP:'
     O                       XXFND               18
     O                                           20 '-'
     O                       XXFNNM              62
     O          E            HEAD3          1
     O                                           60 'ACCIDENT'
     O                                          105 'NETWORK'
     O          E            HEAD3          2
     O                                            9 'FILE #'
     O                                           27 'INV #'
     O                                           37 'EOB#/SVC#'
     O                                           50 'SOC. SEC. #'
     O                                           58 'DATE'
     O                                           74 'PATIENT NAME'
     O                                          105 'BILL AMT'
     O          E            HEAD4       1  2
     O                                           71 'NETWORK'
     O          E            DETL1          1
     O                       XF#                 20
     O                       XXINV#        Z     27
     O                       XXEOB#        Z     37
      ***** Beg Chg *** 06/09/20 *******************************************
     O***                    XXSS#               50 '   -  -    '
     O                       XXSS#               50
      ***** End Chg *** 06/09/20 *******************************************
     O                       XXACDT              60 '  /  /  '
     O                       XXPNAM              92
     O                       XXAMT         K    106
     O*
     O*** CASE TOTALS
     O          E            CASTOT      1  2
     O                                           12 'CASE TOTALS:'
     O                                           36 '# NETWORK BILLS:'
     O                       XX#CNT        ZB    41
     O                       XXCNTW        KB   106
     O*
     O                                           50 '# EOBS:'
     O                       XX#EOB        ZB    58
     O*
     O*** GROUP TOTALS
     O          E            GRPTOT      1  2
     O                                            5 'GROUP'
     O                       XXFND                9
     O                                           17 'TOTALS:'
     O                                           36 '# NETWORK BILLS:'
     O                       XX#GNT        ZB    41
     O                       XXGNTW        KB   106
     O*
     O                                           50 '# EOBS:'
     O                       XXGEOB        ZB    58
     O*
     O          E            TOTAL1         2
     O                                           57 'TOTAL NUMBER BILLED:'
     O                       XX#TNT        Z     71
     O*
     O          E            TOTAL1         2
     O                                           57 'TOTAL AMOUNT BILLED:'
     O                       XXTNTW        J     72
     O*
     O          E            TOTAL1         2
     O                                           57 'NUMBER OF INVOICES:'
     O                       XX#INV        Z     71
     O*
     O          E            MSG            2
     O                                           21 '*** NO INVOICES TO BE'
     O                                           35 'TO BE PROCESSED FOR'
     O                                           47 'SUBSCRIBER:'
     O                       XXSUB#              53
     O                                           55 '-'
     O                       XXSBNM              91
     O          E            MSGHD          2
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#              18
     O                                           20 '-'
     O                       XXSBNM              62
     O          E            MSGHD          2
     O                                            6 'GROUP:'
     O                       XXFND               18
     O                                           20 '-'
     O                       XXFNNM              62
     O          E            MSGHD          1
     O                                           57 'ACCIDENT'
     O          E            MSGHD          2
     O                                            9 'FILE #'
     O                                           27 'INV #'
     O                                           37 'EOB#/SVC#'
     O                                           50 'SOC. SEC. #'
     O                                           58 'DATE'
     O                                           74 'PATIENT NAME'
     O          E            MSGDT          1
     O                       XF#                 20
     O                       XXINV#        Z     27
     O                       XXEOB#        Z     37
      ***** Beg Chg *** 06/09/20 *******************************************
     O***                    XXSS#               50 '   -  -    '
     O                       XXSS#               50
      ***** End Chg *** 06/09/20 *******************************************
     O                       XXACDT              60 '  /  /  '
     O                       XXPNAM              92
     O          E            MSG2        3
     O                                           60 'ALL EOBS FOR THIS INVOIC'
     O                                           80 'E HAVE BEEN DELETED.'
     O          E            MSG2        1
     O                                           60 'INVOICE #:'
     O                       XXINV#        Z     68
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
     O****    E   60           ROUTE2
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               57 'INC. 1997     '
     O          E            ROUTE2         1 59
     O                       #BRND1
     O          E            ROUTE2         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT2  E            HEADS          1 01
     O*
     O*
     O                                          113 'INVOICE #:'
     O                       XXINV#        Z    122
     O          E            HEADS          1
     O                                          113 'PAGE:'
     O                       PAGE               122
     O          E            HEADS          1
     O                                          113 'DATE:'
     O                       DSDATE        Y    122
     O*
     O*
     O          E            HEADS          2 09
     O              N97                          70 '*** NETWORK INVOICE ***'
     O               97                          70 'ADJUSTED NETWORK INVOICE'
     O          E            HEADS          1
     O                                            9 'SUBSC  :'
     O                       XXSUB#              15
     O                                           17 '-'
     O                       XXSBNM              53
     O          E            HEADS          3
     O                                            9 'GROUP  :'
     O                       XXFND         Z     15
     O                                           17 '-'
     O                       XXFNNM              53
     O          E            DETS           1
     O                                           25 'TOTAL N SERVICES:'
     O                       XNSYST        M     40
     O          E            DETS           1
     O                                           40 '============'
     O          E            TOTS           2
     O                                           25 'PLEASE PAY THIS AMOUNT'
     O                       XNSYST        4B    40 '$'
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
     OPC101     E            RLSC1
** (DS)
NETWORK INVOICE
LEVEL I - HOSPITAL BILL AUDIT
PROVIDER BILL REVIEW
ORIGINAL BILLING HBA
ORIGINAL BILLING PBR
