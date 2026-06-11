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
     H*                                                              **
     H*  PROGRAM:   N9310R - NETWORK PRINT BILLS/RE-BILLS            **
     H*  DESCRIPTION:  PRINT PROGRAM - ORIGINAL/ REBILLING           **
     H*                                                              **
     H*****************************************************************
     H*  CREATED ON 01/21/2002 BY C:62 FOR PROJECT AR033
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 06/09/20  C:212 Project : SY420                               *
     H*                 Mask SSN                                      *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10 C:249 Project: SY215                                 *
     H*          C:189 Retrieve brand Name                            *
     H*                                                               *
     H*  08/03/06 C:62  PROJECT: 139MEJ
     H*                 -CHANGE SS# TO CONSTANT XXX-XX-XXXX
     H*                 -CHANGE FROM 30 DAYS IN FUTURE TO 5.
     H*
     H*  03/10/05 C:62  PROJECT: MC260
     H*                 -USE ALTERNATIVE FEIN IF ENTERED
     H*
     H*  08/24/04 C:62  PROJECT: MC090 43570
     H*                 -USE ALTERNATIVE PHONE#
     H*
     H*  11/27/02 C:62  PROJECT: MC025 35026
     H*                 DON'T UPDATE EANIDT IF A/R NOT WRITTEN.
     H*
     F*****************************************************************
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFAEOL4    IF   E           K DISK
     FFAEAL1    IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FFMC1L     UF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     FFMPDL     IF   E           K DISK
     FNMARP     O    E             DISK
     FNMIMP     O    E             DISK
     FNMARL8    IF   E           K DISK
     F                                     RENAME(PAR01:PAR02)
     FFMF1L     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFNEAP     IF   E           K DISK
     F                                     RENAME(PEA01:PEN01)
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

     D DS              S             30    DIM(1) CTDATA PERRCD(1)              BILLING DESC.
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D EOB             S              9  0 DIM(999)                             EOB# PER CASE
     D RCN             S              3  0 DIM(999)                             RCN# PER CASE
     D*
     D*  WRITE/UPDATE DATA STRUCTURE.
     D*
     D                 DS
     D  KKWRUP                 1      7
     D  XXFAEO                 1      1
     D  XXFAE1                 2      2
     D  XXFAHB                 3      3
     D  XXNAPB                 4      4
     D  XXNAMB                 5      5
     D  XXNATR                 6      6
     D  XXFAEA                 7      7
     D                 DS
     D  XSZIP                  1      9  0
     D  S1ZPP1                 1      5  0
     D  S1ZPP2                 6      9  0
     D                 DS
     D  XXZIP                  1     10
     D  XXZIP1                 1      5
     D  XXZIPD                 6      6
     D  XXZIP2                 7     10
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
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
     D  XF#                    1     21
     D  XXSUB#                 1      5  0
     D  XFHYP1                 6      6
     D  XXFND                  7      9  0
     D  XFHYP2                10     10
     D  XXYR                  11     13  0
     D  XFHYP3                14     14
     D  XXCASE                15     21  0
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
     D  XZPGM                  1      8
     D  XZTYPE                 4      5
     D*** BEGIN ADD *** 08/03/2006 ************************************
     D SS#             C                   CONST('XXX-XX-XXXX')
     D*** END   ADD *** 08/03/2006 ************************************
     D*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0

     d***** Beg Add *** 06/09/20 *******************************************
     d xxss#           s             11    inz
     d***** End Add *** 06/09/20 *******************************************

     I*
     IPEN01
     I              EACO#                       XPCO#
     I              EAFND                       XPFND
     I              EAFYR                       XPFYR
     I              EACASE                      XPCASE
     I              EASS#                       XPSS#
     I              EAACDT                      XPACDT
     I              EAEOB#                      XPEOB#
     I              EARCN                       XPRCN
     I              EATRDT                      XPTRDT
     I              EACHRG                      XPCHRG
     I              EAMRA                       XPMRA
     I              EAOTHR                      XPOTHR
     I              EAOTH2                      XPOTH2
     I              EANDIS                      XPNDIS
     I              EAPAID                      XPPAID
     I              EASTAT                      XPSTAT
     I              EAUSER                      XPUSER
     I              EAPGM                       XPPGM
     I              EATIME                      XPTIME
     I              EADT                        XPDT
     I              EAIUSR                      XPIUSR
     I              EAIPGM                      XPIPGM
     I              EAITIM                      XPITIM
     I              EAIDT                       XPIDT
     I              EAWCPC                      XPWCPC
     I              EAWCPD                      XPWCPD
     I              EAWCBC                      XPWCBC
     I              EAWCBD                      XPWCBD
     I              EAWCNC                      XPWCNC
     I              EAWCND                      XPWCND
     I              EABAMT                      XPBAMT
     I              EANAMT                      XPNAMT
     I              EARMDS                      XPRMDS
     I              EARMK1                      XPRMK1
     I              EARMK2                      XPRMK2
     I              EARMK3                      XPRMK3
     I              EACKST                      XPCKST
     I              EATRCD                      XPTRCD
     I              EAPBRC                      XPPBRC
     I              EABHRF                      XPBHRF
     I              EABBLF                      XPBBLF
     I              EABINV                      XPBINV
     I              EABCAM                      XPBCAM
     I              EANHRF                      XPNHRF
     I              EANBLF                      XPNBLF
     I              EANINV                      XPNINV
     I              EANCAM                      XPNCAM
     I              EAESDT                      XPESDT
     I              EAEDI                       XPEDI
     I              EAEPFL                      XPEPFL
     I              EABPFL                      XPBPFL
     I              EASYS                       XPSYS
     I              EAFEIN                      XPFEIN
     I              EAMOD                       XPMOD
     I              EAPLNK                      XPPLNK
     I              EABIDT                      XPBIDT
     I              EANIDT                      XPNIDT
     I              EAAPPF                      XPAPPF
     I              EABDIF                      XPDIFL
     I              EANDIF                      XPNDIF
     I              EABR2                       XPBR2
     I              EATMFL                      XPTMFL
     I              EACNTL                      XPCNTL
     I              EACHIN                      XPCHIN

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQTYPE            1                REBILL ?
     C                   PARM                    QQINV#            9
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
     C*  COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  SUBSCRIBER FILE
     C*
     C     SMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*  FUND MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE COMPANY AUXILIARY FILE
     C*
     C     C1KY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  PAYEE DEMOGRAPHICS
     C*
     C     PDKY          KLIST
     C                   KFLD                    EAFEIN
     C                   KFLD                    EAMOD
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  EOMB FILE
     C*
     C     EOKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C*  USER PARMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  OUTPUT ROUTING FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C* KEY LIST FOR THE A/R TRANSACTION FILE
     C*
     C     ARKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#                         08/18/93 MOVED
     C                   KFLD                    XXINV#                         SUB# BEFORE INV
     C*
     C     ARKY1         KLIST
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
     C* KEY LIST RE-BILL WORK FILE
     C*
     C     ENKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    XXINV#
     C                   KFLD                    EAEOB#
     C*
     C* INITIALIZATION SECTION
     C*
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EARCN         XXRCN
     C*
     C*** BEGIN ADD *** 08/03/06 **************************************
     C                   SETOFF                                       71
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       XXCLT#            3
     C     XXCLT#        IFEQ      '139'
     C                   SETON                                        71
     C                   ENDIF
     C*** END   ADD *** 08/03/06 **************************************
     C                   Z-ADD     *ZEROS        XNSYST           10 2
     C                   Z-ADD     *ZEROS        XASYST           10 2
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQINV#        XXINV#            9 0
     C                   MOVE      QQMODE        XXMODE            1
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        XSDATE            8 0
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'B'
     C                   SETON                                        08        BATCH MODE
     C                   ELSE
     C                   SETON                                        09        DEMAND MODE
     C                   ENDIF
     C*
     C*  CHAIN TO USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              99
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
     C*********          MOVE      '1'           XXPRT
     C*********     ORKY          CHAIN     POR01                              99
     C*********     *IN99         IFEQ      '0'
     C*********          EXCEPT    ROUTE1
     C*********          ENDIF                                                  END IFEQ '0'
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C*** GET COMPANY RECORD (CARRIER INFO)
     C*
     C     COKY          CHAIN     PCO01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    CONM(Z)                            60
     C                   MOVEA     CONM(Z)       XXCONM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXCONM
     C                   ENDIF
     C*
     C*   GET THE SUBSCRIBER INFORMATION
     C*
     C     SMKY          CHAIN     PSM01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVE      *BLANKS       XXSBNM           35
     C                   MOVE      *BLANKS       XXSAD1           35
     C                   MOVE      *BLANKS       XXSAD2           35
     C                   MOVE      *BLANKS       XXSCTZ           35
     C                   ELSE
     C                   MOVEL     SMNAME        XXSBNM           35
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
     C*
     C     SMAR01        IFEQ      'A'
     C                   MOVE      '1'           *IN90
     C                   ELSE
     C     SMAR01        IFEQ      'B'
     C                   MOVE      '1'           *IN91
     C     SMAR02        IFEQ      'B'
     C                   MOVE      '1'           *IN93
     C                   ELSE
     C                   MOVE      '0'           *IN93
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*** DETERMINE TYPE OF INVOICE - REBILL OR REGULAR INVOICING ...
     C*
     C     QQTYPE        IFEQ      'R'                                          REBILL 1 INV
     C                   SETON                                        97
     C                   ELSE                                                   ELSE EQ 'R'
     C                   SETOFF                                       97
     C                   ENDIF                                                  END EQ 'R'
     C*
     C*  DETERMINE IF THERE ARE ANY BILLS TO PROCESS
     C*
     C                   READ(N)   PEA01                                  99
     C     *IN99         IFEQ      *ON
     C                   EXCEPT    HEAD1
     C     QQTYPE        IFNE      'R'
     C                   EXCEPT    MSG
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C*
     C     QQTYPE        IFEQ      'R'
     C                   Z-ADD     *ZEROS        XXCRAM
     C     ARKY          CHAIN     PAR02                              99
     C                   Z-ADD     ARFND         XXFND
     C                   Z-ADD     ARFYR         XXFYR
     C                   Z-ADD     ARFYR         XXYR
     C                   Z-ADD     ARCASE        XXCASE
     C                   EXSR      SRHEAD
     C                   EXCEPT    MSGHD
     C     *IN99         DOWEQ     *OFF
     C     ARSTAT        IFEQ      'D'
     C     AREOB#        IFNE      XXEOB#
     C                   ADD       ARAMT         XXCRAM            9 2
     C                   Z-ADD     AREOB#        XXEOB#
     C                   Z-ADD     ARRCN         XXRCN
     C                   EXCEPT    MSGDT
     C                   ENDIF
     C                   ENDIF
     C     ARKY          READE     PAR02                                  99
     C                   ENDDO
     C                   EXCEPT    MSG2
     C                   ENDIF
     C                   GOTO      TERM2
     C                   ENDIF
     C*
     C*  SET-UP REMITTANCE ADDRESS
     C*
     C     C1KY          CHAIN     PC101                              99
     C     *IN99         IFEQ      *OFF
     C                   EXCEPT    RLSC1
     C                   MOVE      C1ZIP         XSZIP
     C                   MOVE      C1NAME        XSNAME
     C*
     C                   MOVEL     C1NAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM      C1NAME        ##DATA           80
     C                   PARM      40            ##LGTH            3 0
     C                   MOVEL     ##DATA        XPNAM            40
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
     C   97              MOVE      EANINV        XXINV#                         USE SAME INV#
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C*
     C     F1KY          CHAIN     PF101                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     F1FLG1        ANDEQ     'Y'
     C                   MOVEL(P)  F1ANME        XCNAME
     C                   MOVEL(P)  F1AAD1        XCADR1
     C                   MOVEL     F1AAD1        XCADR1           40
     C                   MOVEL(P)  F1AAD2        XCADR2
     C                   MOVEL     F1AST         XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      F1AZIP        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     F1ACTY        XXLAST
     C                   MOVEL     XXSZ          XXFRST
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
     C                   ELSE                                                   F1FLG1
     C*
     C*** REMOVE LEADING BLANKS FROM REMIT TO NAME
     C*
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    XCNM(Z)                            60
     C     *IN60         IFEQ      '1'
     C                   MOVEA     XCNM(Z)       XCNAME           40
     C                   ELSE
     C                   MOVE      *BLANKS       XCNAME
     C                   ENDIF
     C*
     C                   MOVEL     C1ADR1        XCADR1           40
     C                   MOVEL     C1ADR2        XCADR2           40
     C                   MOVEL     C1ST          XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      S1ZPP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C*
     C     S1ZPP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      S1ZPP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     C1ST          XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      S1ZPP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C*
     C     S1ZPP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      S1ZPP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   ENDIF
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
     C                   ENDIF
     C*
     C*  DETERMINE DUE DATE
     C*
     C*** BEGIN ADD *** 08/03/2006 ************************************
     C     *IN71         IFEQ      *ON
     C                   Z-ADD     5             SRFUTR
     C                   ELSE
     C                   Z-ADD     30            SRFUTR
     C                   ENDIF
     C*** END   ADD *** 08/03/2006 ************************************
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        SRDATE            8 0
     C*** BEGIN CHG *** 08/03/2006 ************************************
     C**********           PARM 30        SRFUTR  50
     C                   PARM                    SRFUTR            5 0
     C*** END   CHG *** 08/03/2006 ************************************
     C                   PARM      *BLANKS       SRRTCD            1
     C*
     C                   Z-ADD     SRDATE        XDUEDT            8 0          ADD 11/17/99
     C*
     C*** GET ROUTING INFORMATION FOR BILLS
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C*******     *IN91         IFEQ      *ON
     C*******     *IN93         IFEQ      *ON
     C*******            MOVE      '0'           XXPRT
     C*******     ORKY          CHAIN     POR01                              99
     C*******     *IN99         IFEQ      *OFF
     C*******            EXCEPT    ROUTE
     C*******            ENDIF                                                  END IFEQ '0'
     C*******            MOVE      '2'           XXPRT
     C*******     ORKY          CHAIN     POR01                              99
     C*******     *IN99         IFEQ      *OFF
     C*******            EXCEPT    ROUTE2
     C*******            ENDIF                                                  END IN99=0
     C*******            ELSE                                                   ELSE 93 =0
     C*******            MOVE      '2'           XXPRT
     C*******     ORKY          CHAIN     POR01                              99
     C*******     *IN99         IFEQ      *OFF
     C*******            EXCEPT    ROUTE2
     C*******                   ENDIF                                                  END IN99=0
     C*******            ENDIF                                                  END *IN93
     C*******            ELSE                                                   ELSE IN91=0
     C*******            MOVE      '0'           XXPRT
     C*******     ORKY          CHAIN     POR01                              99
     C*******     *IN99         IFEQ      *OFF
     C*******            EXCEPT    ROUTE
     C*******            ENDIF                                                  END IFEQ '0'
     C*******            ENDIF                                                  END IN91
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
     C                   READ      PEA01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     EACO#         XXCO#             3 0
     C                   Z-ADD     EASUB#        XXSUB#            5 0
     C                   Z-ADD     EAFND         XXFND             3 0
     C                   Z-ADD     EAFYR         XXFYR             3 0
     C                   Z-ADD     EAFYR         XXYR
     C                   Z-ADD     EACASE        XXCASE            7 0
     C                   MOVEL     *ON           DOIT              1
     C*
     C                   EXSR      SRHEAD                                       ADD 02/03/95
     C*
     C*** VERIFY INVOICE AMT GREATER THAN ZERO
     C*
     C                   EXSR      SRAMT
     C*
     C     *IN91         IFEQ      '1'
     C     XXFFLG        IFEQ      *BLANKS
     C     *IN97         IFEQ      *OFF
     C                   ADD       1             XXINV#
     C                   ENDIF
     C                   MOVE      'N'           XXFFLG
     C                   ENDIF
     C                   ELSE
     C     *IN97         IFEQ      *OFF
     C                   ADD       1             XXINV#
     C                   ENDIF
     C                   ENDIF
     C     *IN91         IFEQ      '1'
     C     *IN93         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   Z-ADD     0             PAGE
     C                   EXCEPT    HEADS
     C                   ELSE
     C                   EXCEPT    HEADS
     C                   ENDIF
     C                   ELSE
     C                   EXCEPT    HEAD
     C                   ENDIF
     C                   EXCEPT    HEAD3
     C                   ENDIF
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C*** VERIFY INVOICE AMT GREATER THAN ZERO
     C*
     C                   EXSR      SRAMT
     C*
     C                   Z-ADD     EAEOB#        XXEOB#
     C                   Z-ADD     EARCN         XXRCN
     C*
     C*** IF CASE CHANGES, PRINT CASE TOTALS
     C*
     C     EACASE        IFNE      XXCASE
     C     XXCASE        ANDNE     *ZEROS
     C                   EXSR      SRCASE
     C                   Z-ADD     EAFYR         XXFYR
     C                   EXSR      SRHEAD
     C     *IN91         IFEQ      *ON
     C     *IN93         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   ENDIF
     C                   ELSE
     C                   EXCEPT    HEAD
     C                   ENDIF
     C                   ENDIF                                                  ENDNE XXCASE
     C*
     C*
     C*** IF GROUP CHANGES, PRINT GROUP TOTALS, PAGE BREAK HEADINGS
     C*
     C     EAFND         IFNE      XXFND
     C     XXFND         ANDNE     *ZEROS
     C                   EXSR      SRGRP
     C                   EXCEPT    HEAD1
     C                   EXSR      SRHEAD
     C     *IN91         IFEQ      *ON
     C     *IN93         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   ENDIF
     C                   ELSE
     C                   EXCEPT    HEAD
     C                   ENDIF
     C                   EXCEPT    HEAD3
     C                   ENDIF                                                  ENDNE XXFND
     C*
     C     EOKY          CHAIN     PEO01                              99
     C                   MOVEL     EOPNAM        XXPRNM                         PROVIDER NAME  A
     C*
     C                   Z-ADD     EANAMT        XXAMT             9 2          DETL BILLD
     C                   ADD       XXAMT         XXCNET           10 2          CASE BILLD
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDT            8 0
     C*
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODT            8 0
     C*
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
     C                   MOVE      *BLANKS       XXDESC           20            PBR DESC
     C                   MOVEL(P)  DS(1)         XXDESC           20            HBA DESC
     C                   ADD       1             XX#CNT            5 0          CASE # HBA
     C                   Z-ADD     *ZEROS        XX#BLL            3 0          # OF BILLS
     C*
     C*** COUNT NUMBER OF EOBS PER CASE AND GROUP ...
     C*
     C                   ADD       1             XX#EOB            4 0          # OF EOBS/CASE
     C                   ADD       1             XXGEOB            4 0          # OF EOBS/GROUP
     C*
     C*
     C* * * GET PROVIDER INFORMATION.
     C*
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     PDNAME        XXPRNM           17
     C                   MOVE      EAFEIN        XXPRV#            9 0
     C                   MOVE      EAMOD         XXMOD             3 0
     C                   ELSE
     C                   MOVE      *BLANKS       XXPRNM
     C                   MOVE      EAFEIN        XXPRV#
     C                   MOVE      EAMOD         XXMOD
     C                   ENDIF
     C*
     C     XXMOD         IFNE      *ZEROS
     C                   SETON                                        96
     C                   ELSE
     C                   SETOFF                                       96
     C                   ENDIF
     C*
     C***  OUTPUT BILL - DETAIL LINE
     C*
     C     *IN91         IFEQ      *ON
     C     *IN93         IFEQ      *ON
     C     XXLINE        IFGT      40
     C                   EXCEPT    HEAD
     C                   ENDIF
     C                   EXCEPT    DET
     C                   ENDIF
     C                   ELSE
     C     XXLINE        IFGT      40
     C                   EXCEPT    HEAD
     C                   ENDIF
     C                   EXCEPT    DET
     C                   ENDIF
     C*
     C***  OUTPUT REPORT - DETAIL LINE
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C                   EXCEPT    DETL1
     C*
     C*** IF BILLING IS PERFORMED, CREATE A/R RECORD.
     C*
     C                   EXSR      SRAR
     C*
     C*** BEGIN ADD *** 11/27/02 **************************************
     C                   Z-ADD     *ZEROS        XXNIDT
     C     UPDATE        IFEQ      *ON
     C*** END   ADD *** 11/27/02 **************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXNIDT            7 0
     C*** BEGIN ADD *** 11/27/02 **************************************
     C                   ENDIF
     C*** END   ADD *** 11/27/02 **************************************
     C*
     C                   MOVEL     *BLANKS       KKWRUP
     C                   MOVEL     'U'           XXFAEA
     C*
     C     CMKY          CHAIN(N)  PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN         ##RRN
     C                   ELSE
     C                   Z-ADD     *ZEROS        ##RRN
     C                   ENDIF                                                  *IN99 = OFF
     C*
     C                   CALL      'AA001R'
     C                   PARM                    ##RRN             9 0          RRN OF CASE
     C                   PARM      XXEOB#        ##EOB#            9 0          EOB #
     C                   PARM      XXRCN         ##RCN             3 0          RECON #
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM      *ZEROS        ##BAMT            9 2          BRS AMOUNT
     C                   PARM      *ZEROS        ##NAMT            9 2          NWS AMOUNT
     C                   PARM      *BLANKS       ##BHRF            1            BRS INV H/R FLAG
     C                   PARM      *BLANKS       ##BBLF            1            BRS INV PROC FLAG
     C                   PARM      *ZEROS        ##BINV            9 0          BRS INVOICE #
     C                   PARM      *ZEROS        ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM      *BLANKS       ##NHRF            1            NWS INV H/R FLAG
     C                   PARM      'P'           ##NBLF            1            NWS INV PROC FLAG
     C                   PARM      XXINV#        ##NINV            9 0          NWS INVOICE #
     C                   PARM      EASYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM      *ZEROS        ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM      *ZEROS        ##TMAA            9 2          TIME/MAT AFTER
     C                   PARM      *BLANKS       ##CERT           15            PRE-CERT
     C                   PARM      *ZEROS        ##EXT             3 0          EXTENSION
     C                   PARM      *ZEROS        ##FEIN            9 0          PROVIDER FEIN
     C                   PARM      *ZEROS        ##MOD             3 0          PROVIDER MOD
     C                   PARM      *ZEROS        ##BIDT            7 0          BRS INV DT
     C                   PARM      ' '           ##BPFL            1            BRS INV DT
     C                   PARM      QQMODE        ##MODE            1            BRS INV DT
     C                   PARM      XXNIDT        ##NIDT            7 0          NET INV DT
     C*
     C     READN1        TAG
     C                   READ      PEA01                                  99
     C                   ENDDO                                                  END DOWEQ 99
     C*
     C     *IN91         IFEQ      *ON
     C     XNSYST        COMP      *ZEROS                             30
     C                   EXCEPT    DETS
     C                   Z-ADD     XNSYST        XASYST
     C                   EXCEPT    TOTS
     C                   ENDIF
     C*
     C*** END OF PROGRAM
     C*
     C     TERM          TAG
     C                   SETON                                        LR
     C*
     C     *IN21         IFEQ      *OFF
     C     *INLR         ANDEQ     *ON
     C                   EXCEPT    MSG
     C                   ENDIF
     C*
     C     *IN21         CABEQ     *OFF          TERM2
     C*
     C                   EXSR      SRCASE
     C                   EXSR      SRGRP
     C*
     C*** OUTPUT FINAL TOTALS
     C*
     C*  TOTAL NETWORK
     C*
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD1C
     C                   EXCEPT    HEAD4
     C                   EXCEPT    TOTAL1
     C*
     C*  UPDATE COMPANY AUXILARY FILE WITH LAST INVOICE # USED
     C*
     C     C1KY          CHAIN     PC101                              99
     C     *IN99         IFEQ      *OFF
     C     *IN97         IFEQ      *OFF
     C                   Z-ADD     XXINV#        C1LIN#
     C                   ENDIF
     C                   UPDATE    PC101
     C                   ENDIF
     C*
     C     TERM2         TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '1';
       Chain Orky Por01;
       If %found;
          Except Route1;
       Endif;
       If *in91 = '1';
          If *in93 = '1';
             xxprt = '0';
             Chain Orky Por01;
             If %found;
                Except Route;
             Endif;
             xxprt = '2';
             Chain Orky Por01;
             If %found;
                Except Route2;
             Endif;
          Else;
             xxprt = '2';
             Chain Orky Por01;
             If %found;
                Except Route2;
             Endif;
          Endif;
       Else;
          xxprt = '0';
          Chain Orky Por01;
          If %found;
             Except Route;
          Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*****************************************************************--*
     C*  CREATE A/R RECORD
     C*****************************************************************--*
     C     SRAR          BEGSR
     C*
     C*** BEGIN ADD *** 11/27/02 **************************************
     C                   MOVE      *OFF          UPDATE            1
     C*** END   ADD *** 11/27/02 **************************************
     C                   MOVE      'N'           ARSYS
     C                   ADD       XXAMT         XNSYST                         ADD 03/26/93
     C*
     C     *IN97         IFEQ      *ON
     C     ENKY          CHAIN     PEN01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     XPDIFL        IFEQ      *BLANKS
     C     XPCHIN        ANDEQ     *BLANKS
     C                   GOTO      NXTARP
     C                   ENDIF
     C                   ELSE
     C                   GOTO      NXTARP                                       ADD 07/14/93
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      EACO#         ARCO#
     C                   MOVE      EASUB#        ARSUB#
     C                   MOVE      EAFND         ARFND
     C*
     C     *IN90         IFEQ      *ON
     C                   MOVE      EAFYR         ARFYR
     C                   MOVE      EACASE        ARCASE
     C                   ELSE
     C     *IN91         IFEQ      *ON
     C                   Z-ADD     *ZEROS        ARFYR
     C                   Z-ADD     *ZEROS        ARCASE
     C                   ENDIF
     C                   ENDIF
     C*
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
     C*** BEGIN ADD *** 11/27/02 **************************************
     C                   MOVE      *ON           UPDATE
     C*** END   ADD *** 11/27/02 **************************************
     C*
     C                   WRITE     PAR01
     C*
     C     DOIT          IFEQ      *ON
     C     *IN97         IFEQ      *OFF
     C                   EXSR      WRITEI
     C                   MOVEL     *OFF          DOIT
     C                   ENDIF
     C                   ENDIF
     C*
     C     NXTARP        TAG
     C*
     C                   ENDSR
     C*****************************************************************--*
     C*  GET HEADING INFORMATION FOR BILLS
     C*****************************************************************--*
     C     SRHEAD        BEGSR
     C*
     C*** GET FUND(GROUP) INFORMATION.
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    FNNM(Z)                            99
     C                   MOVEA     FNNM(Z)       XXFNNM           35
     C                   Z-ADD     1             X                 5 0
     C     'P'           LOOKUP    FSY(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     FBR(X)        XXCODE            3
     C                   ELSE
     C                   MOVEL     *BLANK        XXCODE            3
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   ENDIF
     C*
     C*** GET CLAIM MASTER FILE
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      CMSS#         XXSS#             9 0
      /Free
       xxSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   MOVE      CMCCL#        XXCCLM           13
     C     XXCCLM        COMP      *BLANK                             9898
     C     CMSS#         CHAIN     PCD01                              99
     C     *IN99         IFEQ      *OFF
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
     C                   ENDIF
     C                   MOVEL     XXLAST        XXPNAM           27
     C                   ELSE
     C                   MOVEL     *BLANKS       XXPNAM           27
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     0             XXACDT
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      *ZERO         XXSS#             9 0
     C                   MOVE      *BLANKS       XXSS#
      ***** End Chg *** 06/09/20 *******************************************
     C                   MOVE      *BLANKS       XXCCLM           13
     C                   MOVEL     *BLANKS       XXPNAM
     C                   ENDIF
     C*
     C     *IN91         IFEQ      *OFF
     C                   Z-ADD     0             PAGE
     C                   ENDIF
     C*
     C*** DETERMINE IF 2ND ADDRESS IS BLANK
     C*
     C     XCADR2        COMP      *BLANKS                            9494
     C*
     C                   ENDSR
     C*****************************************************************--*
     C*  PERFORM CASE TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C     SRCASE        BEGSR
     C*
     C                   MOVEL     *ON           DOIT
     C                   ADD       XX#CNT        XX#GNT            5 0          GROUP # NET
     C                   ADD       XXCNET        XXGNET           10 2          GROUP HBA BLLED
     C                   Z-ADD     XXCNET        XXCAMT           10 2          CASE TOTAL  LED
     C                   ADD       1             XX#INV            6 0          # INVOICES
     C*
     C     *IN91         IFNE      *ON
     C     XXLINE        IFGT      40
     C                   EXCEPT    HEAD
     C                   ENDIF
     C                   EXCEPT    TOT
     C                   ELSE
     C     *IN93         IFEQ      *ON
     C     XXLINE        IFGT      40
     C                   EXCEPT    HEAD
     C                   ENDIF
     C                   EXCEPT    TOT1
     C                   ENDIF
     C                   ENDIF
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C                   SETOFF                                       OA
     C                   ENDIF
     C*
     C                   EXCEPT    CASTOT
     C*
     C                   Z-ADD     0             XXCAMT
     C                   Z-ADD     EAFYR         XXFYR
     C                   Z-ADD     EAFYR         XXYR
     C                   Z-ADD     EACASE        XXCASE
     C*
     C     *INLR         IFEQ      *OFF
     C     *IN91         IFNE      *ON
     C     *IN97         IFEQ      *OFF
     C                   ADD       1             XXINV#
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     *INLR         IFEQ      '0'
     C     EAFND         ANDNE     XXFND
     C     XXFND         ANDNE     *ZEROS
     C                   EXSR      SRGRP
     C                   EXCEPT    HEAD1
     C                   EXSR      SRHEAD
     C                   EXCEPT    HEAD3
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************--*
     C*  PERFORM GROUP TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C     SRGRP         BEGSR
     C*
     C                   ADD       XX#GNT        XX#TNT            6 0          TOTAL # NET
     C                   ADD       XXGNET        XXTNET           10 2          TOTAL HBA BLLED
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C                   SETOFF                                       OA
     C                   ENDIF
     C*
     C                   EXCEPT    GRPTOT
     C*
     C                   Z-ADD     EAFND         XXFND
     C*
     C                   ENDSR
     C*****************************************************************--*
     C*  VERIFY BILLING AMT GREATER THAN 0 BEFORE PRODUCING INVOICE ...
     C*****************************************************************--*
     C     SRAMT         BEGSR
     C*
     C     EANAMT        CABLE     *ZEROS        READN1
     C*
     C     *IN21         IFEQ      *OFF
     C                   SETON                                        21
     C                   ENDIF
     C*
     C                   ENDSR
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
     O*****************************************************************
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
     O**********E    08      ROUTE          1
     O**********             ORDIS2              60
     O**********E    08      ROUTE          1
     O**********             ORDIS3              60
     O**********E    08      ROUTE          2
     O**********             ORDIS4              60
     O**********E    09      ROUTE          1
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E    08      ROUTE          1
     O                       ORDIS2              60
     O          E    08      ROUTE          1
     O                       ORDIS3              60
     O          E    08      ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                 N97   ORTITL              80
     O                  97                       35 'NETWORK RE-BILL'
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
     O***     E   60           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                57 'INC. 2002     '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      *
     OQSYSPRT   E            HEAD           1 01
     O*
     O                                          111 'INVOICE #:'
     O                       XXINV#        Z    122
     O          E            HEAD           1
     O                                          111 'PAGE:'
     O                       PAGE               122
     O          E            HEAD           1
     O                                          111 'DATE:'
     O                       XSDATE        Y    122
     O*
     O          E            HEAD           2 09
     O              N97                          70 '*** INVOICE ***'
     O               97                          70 'ADJUSTED INVOICE'
     O          E            HEAD           1
     O                                           14 'SUBSC  :'
     O                       XXSUB#        Z     20
     O                                           23 '-'
     O                       XXSBNM              58
     O*
     O                                           79 'PATIENT:'
     O                       XXPNAM             107
     O          E            HEAD           1
     O                                           14 'GROUP  :'
     O                       XXFND         Z     20
     O                                           23 '-'
     O                       XXFNNM              58
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
     O          E            HEAD           2
     O               98                          14 'CLAIM #:'
     O               98      XXCCLM              28
     O                                           79 'FILE # :'
     O                       XF#                101
     O          E            HEAD           2
     O                                           13 'DATE'
     O                                           28 'EOB#/SVC#'
     O                                           32 'RCN'
     O                                           50 'SERVICE DATES'
     O                                           63 'SERVICES'
     O                                           85 'PROVIDER'
     O                                          105 'PROVIDER NAME'
     O                                          122 'REVIEWER FEE'
     O          E            DET            1
     O                       XXTRDT              16 '  /  /    '
     O                       XXEOB#        Z     28
     O                       XXRCN         Z     32
     O                       XXFRDT              43 '  /  /    '
     O                                           44 '-'
     O                       XXTODT              54 '  /  /    '
     O                       XXDESC              75
     O                       XXPRV#        Z     86
     O               96                          87 '-'
     O               96      XXMOD         Z     90
     O                       XXPRNM             109
     O                       XXAMT         4    122
     O          E            TOT1           1
     O                                          122 '==========='
     O          E            TOT1           1
     O                       XXCAMT        4    122
     O          E            TOT            1 43
     O                                           53 'PLEASE PAY THIS AMOUNT:'
     O                       XXCAMT        4B    67 '$'
     O                                           80 'ON OR BEFORE'
     O                       XDUEDT        Y   +001
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
     O**********E    08      ROUTE1         1
     O**********             ORDIS2              60
     O**********E    08      ROUTE1         1
     O**********             ORDIS3              60
     O**********E    08      ROUTE1         2
     O**********             ORDIS4              60
     O**********E    09      ROUTE1         1
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E    08      ROUTE1         1
     O                       ORDIS2              60
     O          E    08      ROUTE1         1
     O                       ORDIS3              60
     O          E    08      ROUTE1         2
     O                       ORDIS4              60
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
     O***     E   60           ROUTE1
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               57 'INC. 2002     '
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT1  E            HEAD1          1 06
     O                       XSDATE             172 '0  /  /    '
     O                       XXCONM             113
     O                       DSTIME               8 '  :  :  '
     O          E            HEAD1          1
     O                       ORPNAM              10
     O                 N97   ORTITL             143
     O                  97                       99 'NETWORK RE-BILL'
     O                       PAGE1              172
     O                                          166 'PAGE:'
     O          E            HEAD1C         2
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#        Z     18
     O                                           20 '-'
     O                       XXSBNM              62
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
     O                                          120 '# BILLS'
     O          E            HEAD3          2
     O                                            9 'FILE #'
     O                                           27 'INV #'
     O                                           41 'EOB#/SVC#'
     O                                           45 'RCN'
     O                                           59 'SOC. SEC. #'
     O                                           67 'DATE'
     O                                           83 'PATIENT NAME'
     O                                          111 'BILLING AMT'
     O                                          120 'PER EOB'
     O          E            DETL1          1
     O                       XF#                 21
     O                       XXINV#        Z     31
     O                       XXEOB#        Z     41
     O                       XXRCN               45
     O*** BEGIN CHG *** 08/03/2006 ************************************
     O**********               XXSS#     59 '0   -  -    '
      ***** Beg Chg *** 06/09/20 *******************************************
     O***           N71      XXSS#               59 '0   -  -    '
     O              N71      XXSS#               59
      ***** End Chg *** 06/09/20 *******************************************
     O               71      SS#                 59
     O*** END   CHG *** 08/03/2006 ************************************
     O                       XXACDT              70 '0  /  /    '
     O                       XXPNAM              98
     O                       XXAMT              111 '      0 .  '
     O                       XX#BLL             118 ' 0 '
     O*** CASE TOTALS
     O          E            CASTOT      1  1
     O                                           12 'CASE TOTALS:'
     O                                         +008 '# NET BILLS:'
     O                       XX#CNT         B  +002 '   0 '
     O                                         +002 '# EOBS:'
     O                       XX#EOB         B  +002 '  0 '
     O                       XXCNET         B  +046 '      0 .  '
     O*
     O*** GROUP TOTALS
     O          E            GRPTOT      1  2
     O                                            5 'GROUP'
     O                       XXFND             +001
     O                                         +002 'TOTALS:'
     O                                         +002 '# NET BILLS:'
     O                       XX#GNT         B  +002 '   0 '
     O                                         +002 '# EOBS:'
     O                       XXGEOB         B  +002 '  0 '
     O                       XXGNET         B  +046 '      0 .  '
     O*
     O          E            HEAD1C         1
     O                                           97 'FINAL TOTALS'
     O*
     O          E            HEAD4       1  2
     O                                           70 '   '
     O                                          122 '     '
     O*
     O          E            TOTAL1         2
     O                                           93 'TOTAL NUMBER BILLED:'
     O                       XX#TNT             106 '    0 '
     O*
     O          E            TOTAL1         2
     O                                           93 'TOTAL AMOUNT BILLED:'
     O                       XXTNET             106 '      0 .  '
     O*
     O          E            TOTAL1         2
     O                                           93 'NUMBER OF INVOICES :'
     O                       XX#INV             106 '    0 '
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
     O                                           45 'RCN'
     O                                           59 'SOC. SEC. #'
     O                                           67 'DATE'
     O                                           83 'PATIENT NAME'
     O          E            MSGDT          1
     O                       XF#                 21
     O                       XXINV#        Z     31
     O                       XXEOB#        Z     41
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
     O                       XXPNAM              98
     O                       XXPNAM              97
     O          E            MSG2        3
     O                                           60 'ALL EOBS FOR THIS INVOIC'
     O                                           80 'E HAVE BEEN DELETED.'
     O          E            MSG2        1
     O                                           56 'INVOICE #:'
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
     O**********E    08      ROUTE2         1
     O**********             ORDIS2              60
     O**********E    08      ROUTE2         1
     O**********             ORDIS3              60
     O**********E    08      ROUTE2         2
     O**********             ORDIS4              60
     O**********E    09      ROUTE2         1
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE2         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E    08      ROUTE2         1
     O                       ORDIS2              60
     O          E    08      ROUTE2         1
     O                       ORDIS3              60
     O          E    08      ROUTE2         2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE2         2
     O                                           19 'REPORT TITLE:'
     O                 N97   ORTITL              80
     O                  97                       35 'NETWORK RE-BILL'
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
     O****                               57 'INC. 2002     '
     O          E            ROUTE2         1 59
     O                       #BRND1
     O          E            ROUTE2         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT2  E            HEADS          1 01
     O*
     O*
     O                                          112 'INVOICE #:'
     O                       XXINV#        Z    122
     O          E            HEADS          1
     O                                          112 'PAGE:'
     O                       PAGE               122
     O          E            HEADS          1
     O                                          112 'DATE:'
     O                       XSDATE        Y    122
     O*
     O*
     O          E            HEADS          2 09
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
     O                                           25 'TOTAL N SERVICES:'
     O                       XNSYST        M     40
     O          E            DETS           1
     O                                           40 '============'
     O          E            TOTS           2
     O                                           25 'PLEASE PAY THIS AMOUNT'
     O                       XASYST        4B    40 '$'
     O                                           54 'ON OR BEFORE'
     O                       XDUEDT        Y   +001
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
ORIGINAL BILLING NET
