     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H/TITLE N7128R - CREDIT MEMO                                     *
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    PROGRAM TO PROCESS THE WRITE OF A REFUND A/R RECORD FOR A  *
     H*    RECON/AUDIT EOMB AND GENERATES A CREDIT MEMO               *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    F0086R - EOMB REQUEST                                      *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  PROGRAMMER: C:20               REQ/PROJ: 25MAE               *
     H*  CREATED: 08/16/95                                            *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/09/20  C:212 Project : SY420                               *
     H*                  Mask SSN                                     *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*   01/28/10 C:249 Project: SY215                               *
     H*            C:189 Retrieve brand Name                          *
     H*                                                               *
     H*  03/18/99 C:94  PROJECT: 25MKL
     H*                 ADD RECON
     H*
     H*  03/09/99 C:94  PROJECT: MC152
     H*                 CHANGE FDCAL2 TO FDCALL AND CHANGE KLIST
     H*
     H*  01/15/99 C:62  PROJECT: MC152
     H*                 ADD RECON NUMBER
     H*
     H*  01/09/99  C:94  PROJECT: MC152
     H*                  CHANGE PROGRAM TO USE API(AA001R) TO UPDATE
     H*                  NAMBP, FAHBP, AND FAEAP.  ALSO CHANGE THE
     H*                  LENGTH OF INV# FROM 5 TO 9.
     H*
     H*****************************************************************
     FFAEOL4    UF A E           K DISK
     FNDTMP     O    E           K DISK
     FNDTML1    IF   E           K DISK
     F                                     RENAME(PTM01:PTM02)
     F*** BEG CHG *** 03/09/99 ************************************
     FFDCALL    IF   E           K DISK
     F**********FDCAL2  IF  E           K        DISK
     F*** END CHG *** 03/09/99 ************************************
     FFMFNL     IF   E           K DISK
     FNMARL     IF   E           K DISK
     FNMARP     O    E           K DISK
     F                                     RENAME(PAR01:PAR02)
     FNMBDLB    IF   E           K DISK
     FSMCOL     IF   E           K DISK
     F*** BEG CHG *** 01/09/99 *************************************
     FFAHBL     IF   E           K DISK
     F********** FAHBL   UF  E           K        DISK
     F*** END CHG *** 01/09/99 *************************************
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F*** BEG ADD *** 01/09/99 *************************************
     F                                     INFDS(DBFDS)
     F*** END ADD *** 01/09/99 *************************************
     FFMC1L     UF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     F*** BEG CHG *** 01/09/99 ***************************************
     FNAMBL1    IF   E           K DISK
     F****NAMBL1  UF  E           K        DISK                      A
     F*** END CHG *** 01/09/99 ***************************************
     FFAMSL4    IF   E           K DISK
     F                                     RENAME(PMS01:PMS03)
     FFAMSL3    IF   E           K DISK
     F                                     RENAME(PMS01:PMS02)
     FFAMSP     O    E           K DISK
     F*** BEG ADD *** 01/09/99 *************************************
     FFAEAL1    IF   E           K DISK
     F*** END ADD *** 01/09/99 *************************************
     FQSYSPRT   O    F  127        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      /copy sPrSecure
      ***** End Add *** 06/09/20 *******************************************

     D*
     D DS              S             30    DIM(3) CTDATA PERRCD(1)              BILLING DESC.
     D TMDSC           S             30    DIM(1) CTDATA PERRCD(1)              T/M BILLING DESC.
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  FSY                    1     10
     D                                     DIM(10)                              GROUP SYS CODE
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
     D                                     DIM(10)                              GROUP BILL RATE CD
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
     D  XXPGM                  1      8
     D  XXTYPE                 4      5
     D                 DS
     D  XSZIP                  1      9  0
     D  S1ZPP1                 1      5  0
     D  S1ZPP2                 6      9  0
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND                       COMPANY NAME
     D  CONAME                 1     40
     D                 DS
     D  XCNM                   1     40
     D                                     DIM(40) ASCEND                       COMPANY NAME
     D  XSNAME                 1     40
     D                 DS
     D  FNNM                   1     60
     D                                     DIM(60) ASCEND                       FUND NAME
     D  FNNAME                 1     60
     D                 DS
     D  XF#                    1     20
     D  XXSUB#                 1      5  0
     D  XFHYP1                 6      6
     D  XXFND                  7      9  0
     D  XFHYP2                10     10
     D  XXYR                  11     12  0
     D  XFHYP3                13     13
     D  XXCASE                14     20  0
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
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** BEG ADD *** 01/09/99 ***************************************
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

     I*
     IPTM02
     I              TMCO#                       T2CO#
     I              TMSUB#                      T2SUB#
     I              TMFND                       T2FND
     I              TMFYR                       T2FYR
     I              TMCASE                      T2CASE
     I              TMEOB#                      T2EOB#
     I              TMSTAF                      T2STAF
     I              TMSVDT                      T2SVDT
     I              TMACT                       T2ACT
     I              TMPNFL                      T2PNFL
     I              TMHRS                       T2HRS
     I              TMHRRT                      T2HRRT
     I              TMMILE                      T2MILE
     I              TMMIRT                      T2MIRT
     I              TMMSCG                      T2MSCG
     I              TMDESC                      T2DESC
     I              TMSEQ#                      T2SEQ#
     I              TMPCS                       T2PCS
     I              TMSTAT                      T2STAT
     I              TMMSCD                      T2MSCD
     I              TMMSEQ                      T2MSEQ
     I              TM92N1                      T292N1
     IPMS02
     I              MSCO#                       M2CO#
     I              MSSUB#                      M2SUB#
     I              MSFND                       M2FND
     I              MSFYR                       M2FYR
     I              MSCASE                      M2CASE
     I              MSSEQ#                      M2SEQ#
     IPMS03
     I              MSCO#                       M3CO#
     I              MSSUB#                      M3SUB#
     I              MSFND                       M3FND
     I              MSFYR                       M3FYR
     I              MSCASE                      M3CASE
     I              MSSEQ#                      M3SEQ#
     I              MSICD9                      M3ICD9
     I              MSLOST                      M3LOST
     I              MSDPR#                      M3DPR#
     I              MSNAME                      M3NAME
     I              MSBGDT                      M3BGDT
     I              MSENDT                      M3ENDT
     I              MSSTS                       M3STS
     I              MSSTAF                      M3STAF
     I              MSCOMM                      M3COMM
     I              MSSTAT                      M3STAT
     I              MSUSER                      M3USER
     I              MSPGM                       M3PGM
     I              MSTIME                      M3TIME
     I              MSDT                        M3DT
     I              MSIUSR                      M3IUSR
     I              MSIDT                       M3IDT
     I              MSITIM                      M3ITIM
     I              MSFL01                      M3FL01
     I              MSFL02                      M3FL02
     I              MSFL03                      M3FL03
     I              MSFL04                      M3FL04
     I              MSFL05                      M3FL05
     I              MSFL06                      M3FL06
     I              MSFL07                      M3FL07
     I              MSFL08                      M3FL08
     I              MSFL09                      M3FL09
     I              MSFL10                      M3FL10
     I              MS10A1                      M310A1
     I              MS10A2                      M310A2
     I              MS10A3                      M310A3
     I              MS10A4                      M310A4
     I              MS10A5                      M310A5
     I              MS40A1                      M340A1
     I              MS40A2                      M340A2
     I              MS60N1                      M360N1
     I              MS60N2                      M360N2
     I*
     I*** END ADD *** 01/09/99 ***************************************
     I*

     C*
     C* COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* COMPANY AUXILIARY FILE
     C*
     C     C1KEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C* PAYEE DEMOGRAPHICS
     C*
     C     PDKEY         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C*
     C* CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C* OUTPUT ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C* USER PARMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* KEY LIST FOR THE EOMB TRANSACTION FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C     EOKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*** BEG ADD *** 01/09/99 **************************************
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    EOEOB#
     C*
     C*** END ADD *** 01/09/99 **************************************
     C* KEY LIST FOR THE HBA TRANSACTION FILE
     C*
     C     HBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    EOEOB#
     C*
     C* KEY LIST FOR THE DETAIL CHARGE FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C**********           KFLD           EOTRCD           DEL 03/09/99
     C                   KFLD                    XXEOB#
     C*
     C* KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C* KEY LIST FOR THE A/R TRANSACTION FILE
     C*
     C     ARKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXSYS
     C                   KFLD                    XXEOB#
     C*
     C* BILLING CODE TABLE FILE
     C*
     C     BDKEY         KLIST
     C                   KFLD                    XXCODE
     C                   KFLD                    XXDATE            7 0
     C*
     C* KEY LIST FOR THE TIME AND MATERIALS DETAIL CHARGE FILE
     C*
     C     TMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    X1EOB#
     C*
     C     TMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    X1EOB#
     C*** BEGIN ADD *** 01/15/99 **************************************
     C                   KFLD                    XXRCN
     C*** END   ADD *** 01/15/99 **************************************
     C                   KFLD                    XXSEQ#
     C*
     C* KEY LIST FOR THE MEDICAL MANAGEMENT BILLING FILE
     C*
     C     MBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    X1EOB#
     C*
     C* KEY LIST FOR WCMC CLAIM MEDICAL SERVICES TRANSACTION FILE
     C*
     C     MSKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXMSCD
     C*
     C     MSKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    X1SEQ#
     C*
     C* PARAMETER LIST FOR CALLED PROGRAM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQEOB#            9
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
     C* LIKE DEFINE SECTION
     C*
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOFYR         XXFYR
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    EOEOB#        X1EOB#
     C     *LIKE         DEFINE    EOEOB#        X2EOB#
     C     *LIKE         DEFINE    EOSYS         XXSYS
     C     *LIKE         DEFINE    EOBAMT        XXBAMT
     C     *LIKE         DEFINE    EOBAMT        X1BAMT
     C     *LIKE         DEFINE    EOBAMT        XMBAMT
     C     *LIKE         DEFINE    EOBAMT        XOBAMT
     C     *LIKE         DEFINE    EOBAMT        L1BAMT
     C     *LIKE         DEFINE    EOBAMT        L2BAMT
     C     *LIKE         DEFINE    EOFEIN        XXPRV#
     C     *LIKE         DEFINE    EOFEIN        XXFEIN
     C     *LIKE         DEFINE    EOMOD         XXMOD
     C     *LIKE         DEFINE    ARAMT         XARAMT
     C     *LIKE         DEFINE    ARINV#        XXINV#
     C     *LIKE         DEFINE    CMACDT        XXACDT
     C     *LIKE         DEFINE    UPSTAF        XXSTAF
     C     *LIKE         DEFINE    MSMSCD        XXMSCD
     C*** BEGIN ADD *** 01/15/99 **************************************
     C     *LIKE         DEFINE    EORCN         XXRCN
     C*** END   ADD *** 01/15/99 **************************************
     C*** BEG ADD *** 01/09/99 ***************************************
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
     C*** END ADD *** 01/09/99 ***************************************
     C*** BEGIN ADD *** 01/15/99 **************************************
     C                   Z-ADD     *ZEROS        XXRCN
     C*** END   ADD *** 01/15/99 **************************************
     C                   Z-ADD     *ZEROS        XXDISA            9 2
     C                   Z-ADD     *ZEROS        Z                 5 0
     C                   Z-ADD     *ZEROS        #LIN              5 0
     C                   Z-ADD     *ZEROS        XXFRDT            6 0
     C                   Z-ADD     *ZEROS        XXTODT            6 0
     C                   Z-ADD     *ZEROS        XXTRDT            6 0
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                Z-ADD     *ZEROS        XXSS#             9 0
     C                   MOVE      *Blanks       XXSS#            11
      ***** End Chg *** 06/09/20 *******************************************
     C                   Z-ADD     *ZEROS        XXSEQ#            5 0
     C                   Z-ADD     *ZEROS        X1SEQ#            5 0
     C                   Z-ADD     *ZEROS        X2SEQ#            5 0
     C                   Z-ADD     *ZEROS        XXADDT            9 2
     C                   MOVE      *BLANKS       XXCONM           40
     C                   MOVE      *BLANKS       XXSBNM           35
     C                   MOVE      *BLANKS       XXFNNM           35
     C                   MOVE      *BLANKS       XXPNAM           30
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVE      *BLANKS       XCNAME           40
     C                   MOVE      *BLANKS       XXPRNM           20
     C                   MOVE      *BLANKS       XCADR1           40
     C                   MOVE      *BLANKS       XCADR2           40
     C                   MOVE      *BLANKS       XXDESC           30
     C                   MOVE      *BLANKS       XCCSZ            40
     C                   MOVE      *BLANKS       XX13A            13
     C                   MOVE      *BLANKS       XXST             13
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVE      *BLANKS       XXTERM            1
     C                   MOVE      *BLANKS       XXSUMM            1
     C                   MOVE      *BLANKS       XXMODE            1
     C                   MOVEL     'BRVW'        XXMSCD
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQFYR         XXYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQEOB#        XXEOB#
     C                   TIME                    DSDTTM
     C*
     C* SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C* GET USER PARAMETER RECORD
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPSTAF        XXSTAF
     C                   ELSE
     C                   MOVE      *BLANKS       XXSTAF
     C                   END
     C*
     C* CHAIN TO THE EOMB FILE
     C*
     C     EOKEY         CHAIN     PEO01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   EXSR      WRTAR
     C*
     C                   ENDIF                                                  END *IN60 EQ '0'
     C*
     C* END PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C/EJECT
     C**************************************************************************
     C* PROCESS PAPER CREDIT MEMO                                  S R C R E D *
     C**************************************************************************
     C     SRCRED        BEGSR
     C*
     C* GET COMPANY RECORD (CARRIER INFO)
     C*
     C     COKEY         CHAIN     PCO01                              51
     C     *IN51         IFEQ      '0'
     C                   Z-ADD     1             Z
     C     *BLANK        LOOKUP    CONM(Z)                            60
     C                   MOVEA     CONM(Z)       XXCONM
     C                   ELSE
     C                   MOVE      *BLANKS       XXCONM
     C                   END
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C* GET THE SUBSCRIBER INFORMATION
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       XXSBNM
     C                   ELSE
     C                   MOVEL     SMNAME        XXSBNM
     C                   END
     C*
     C* GET GROUP MASTER FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             Z
     C     *BLANK        LOOKUP    FNNM(Z)                            60
     C                   MOVEA     FNNM(Z)       XXFNNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C*
     C* GET CLAIM MASTER FILE
     C*
     C     CMKEY         CHAIN     PCM01                              51
     C     *IN51         IFEQ      '0'
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      CMSS#         XXSS#
      /Free
       xxSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C     CMSS#         CHAIN     PCD01                              52
     C     *IN52         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVEL     CDLSN         XXLAST
     C     CDFSN         IFNE      *BLANK
     C                   MOVEL     CDFSN         XXFRST
     C                   MOVE      '1'           XXTERM
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END
     C                   MOVEL     XXLAST        XXPNAM
     C                   ELSE
     C                   MOVEL     *BLANKS       XXPNAM
     C                   END
     C*
     C                   ELSE
     C                   Z-ADD     0             XXACDT
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      *ZERO         XXSS#
     C                   MOVE      *blanks       XXSS#
      ***** End Chg *** 06/09/20 *******************************************
     C                   MOVEL     *BLANKS       XXPNAM
     C                   END
     C*
     C* GET PROVIDER INFORMATION.
     C*
     C                   MOVE      EOFEIN        XXPRV#
     C                   MOVE      EOMOD         XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     XXMOD         IFNE      *ZEROS
     C                   MOVE      '1'           *IN96                          NOT BLANK
     C                   ELSE
     C                   MOVE      '0'           *IN96                          BLANK
     C                   END
     C*
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVEL     PDNAME        XXLAST
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST
     C                   MOVE      '1'           XXTERM
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END                                                    END NE *BLANK
     C                   MOVEL     XXLAST        XXPRNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXPRNM
     C                   END
     C*
     C* SET-UP REMITTANCE ADDRESS
     C*
     C     C1KEY         CHAIN     PC101                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    PC101E
     C                   MOVE      C1ZIP         XSZIP
     C                   MOVE      C1NAME        XSNAME
     C                   MOVEL     '('           XCPHN#
     C                   MOVE      C1AREA        XXAREA
     C                   MOVE      ')'           XXCPRN
     C                   MOVEL     C1PHN#        XXPH1
     C                   MOVE      C1PHN#        XXPH2
     C                   MOVE      C1FEIN        XXFEIN
     C                   MOVE      '-'           XXHP1
     C                   MOVE      '-'           XFHYP1
     C                   MOVE      '-'           XFHYP2
     C                   MOVE      '-'           XFHYP3
     C*
     C* REMOVE LEADING BLANKS FROM REMIT TO NAME
     C*
     C                   Z-ADD     1             Z
     C     *BLANK        LOOKUP    XCNM(Z)                            60
     C     *IN60         IFEQ      '1'
     C                   MOVEA     XCNM(Z)       XCNAME
     C                   ELSE
     C                   MOVE      *BLANKS       XCNAME
     C                   END
     C*
     C                   MOVEL     C1ADR1        XCADR1
     C                   MOVEL     C1ADR2        XCADR2
     C     XCADR2        COMP      *BLANKS                            9494
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     C1ST          XXST
     C                   MOVEA     XXST          SZ
     C                   MOVE      S1ZPP1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     S1ZPP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      S1ZPP2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVEL     C1CTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XCCSZ
     C                   MOVEL     'FEE SCHD'    XX13A
     C                   MOVE      'USER'        XX13A
     C                   END
     C*
     C* GET ROUTING INFORMATION FOR REFUNDS
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C*******            MOVE      '0'           XXPRT
     C*******     ORKEY         CHAIN     POR01                              60
     C*******     *IN60         IFEQ      '0'
     C*******            EXCEPT    ROUTE
     C*******            END                                                    END IFEQ '0'
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C* PRINT REPORT HEADING
     C*
     C                   EXCEPT    HEAD
     C*
     C* DETERMINE SYSTEM TYPE
     C*
     C                   MOVE      *BLANKS       XXDESC                         PBR DESC
     C     EOSYS         IFEQ      'H'
     C                   MOVEL     DS(1)         XXDESC                         HBA DESC
     C*** BEG CHG *** 01/09/99 ************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDT
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODT
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT
     C*
     C* UPDATE THE FILE WITH THE NEW BILLING AMOUNT
     C*
     C  N87EABAMT        SUB       XXBAMT        EABAMT
     C*
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXFAHB
     C*
     C  N87              CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/18/99 **************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/18/99 **************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM      EABAMT        ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXFRDT
     C                   Z-ADD     *ZEROS        XXTODT
     C                   Z-ADD     *ZEROS        XXTRDT
     C                   END
     C*
     C                   ELSE                                                   ##PRVB = Y
     C*
     C     HBKEY         CHAIN     PHB01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     HBFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDT
     C                   Z-ADD     HBTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODT
     C                   Z-ADD     HBTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT
     C*
     C* UPDATE THE FILE WITH THE NEW BILLING AMOUNT
     C*
     C  N87HB1AAM        SUB       XXBAMT        HB1AAM
     C*
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXFAHB
     C*
     C  N87              CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      EOEOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/18/99 **************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/18/99 **************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM      HB1AAM        ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXFRDT
     C                   Z-ADD     *ZEROS        XXTODT
     C                   Z-ADD     *ZEROS        XXTRDT
     C                   END
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/09/99 **************************************
     C                   ELSE
     C     EOSYS         IFEQ      'P'
     C                   MOVEL     DS(2)         XXDESC                         PBR DESC
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDT
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODT
     C                   Z-ADD     EOTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT
     C*
     C* UPDATE THE FILE WITH THE NEW BILLING AMOUNT
     C*
     C  N87EOBAMT        SUB       XXBAMT        EOBAMT
     C  N87              UPDATE    PEO01
     C                   END
     C                   END
     C*
     C* OUTPUT THE DETAIL LINE
     C*
     C                   EXCEPT    DET
     C*
     C* OUTPUT TOTAL
     C*
     C                   EXCEPT    TOTL
     C*
     C                   ENDSR
     C/EJECT
     C**************************************************************************
     C* PROCESS H1 BILLING TYPE                                        S R H 1 *
     C**************************************************************************
     C     SRH1          BEGSR
     C*
     C     EOAMTC        SUB       EOAMT         XXDISA
     C     XXDISA        SUB       EOOTHR        XXDISA
     C     XXDISA        MULT      BDPC01        XMBAMT
     C*
     C* CHECK TO SEE IF CALCULATED BILL AMOUNT EXCEEDS THE GREATEST ALLOWABLE
     C* BILL AMOUNT ESTABLISHED
     C*
     C     XMBAMT        IFGT      BDAM01
     C                   Z-ADD     BDAM01        XMBAMT
     C                   END
     C*
     C     EOOTHR        MULT      BDPC02        XOBAMT
     C     XMBAMT        ADD       XOBAMT        XXBAMT
     C*
     C     EOSYS         IFEQ      'H'
     C*** BEG CHG *** 01/09/99 *************************************
     C     ##PRVB        IFEQ      'Y'
     C*
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         IFEQ      *OFF
     C     XXBAMT        IFGT      EABAMT
     C     XXBAMT        SUB       EABAMT        XXBAMT
     C                   MOVE      '1'           *IN87
     C                   MOVE      '1'           *IN99
     C                   EXSR      SRTM
     C                   GOTO      SRH1E
     C                   ENDIF                                                  XXBAMT>EABAMT
     C                   ENDIF                                                  *IN60 = O
     C*
     C                   ELSE                                                   ##PRVB = Y
     C*
     C     HBKEY         CHAIN     PHB01                              60
     C     *IN60         IFEQ      '0'
     C     XXBAMT        IFGT      HB1AAM
     C     XXBAMT        SUB       HB1AAM        XXBAMT
     C                   MOVE      '1'           *IN87
     C                   MOVE      '1'           *IN99
     C                   EXSR      SRTM
     C                   GOTO      SRH1E
     C                   END                                                    END XXBAMT GT HB1AAM
     C                   END                                                    END *IN60 EQ '0'
     C*
     C                   ENDIF                                                  ##PRVB = Y
     C*
     C*** END CHG *** 01/09/99 ***************************************
     C*
     C                   END                                                    END EOSYS EQ 'H'
     C*
     C* READ THRU A/R FILE TO SEE IF ANY PRIOR RECON ADJUSTMENTS HAVE
     C* BEEN POSTED TO THIS EOMB.  IF THEY HAVE VERIFY THIS ONE WILL NOT
     C* TAKE BILLING AMOUNT BELOW THE MINIMUM BILLING AMOUNT
     C*
     C     *IN86         IFEQ      '1'
     C                   Z-ADD     *ZEROS        XARAMT
     C                   ADD       ARAMT         XARAMT
     C     ARKEY         CHAIN     PAR01                              55
     C     *IN55         DOWEQ     '0'
     C*
     C     ARTRCD        IFEQ      'RC'
     C     ARTRTP        ANDEQ     'A'
     C                   ADD       ARAMT         XARAMT
     C                   END                                                    END ARTRCD
     C*
     C     ARKEY         READE     PAR01                                  55
     C                   END                                                    END *IN55 A/R
     C                   ELSE                                                   ELSE *IN86 EQ '1'
     C                   Z-ADD     HB1AAM        XARAMT
     C                   END                                                    END *IN86 EQ '1'
     C*
     C* IF THE BILL AMOUNT HAS NOT CHANGED, PRINT CREDIT MEMO INDICATING THIS
     C*
     C     XXBAMT        IFEQ      XARAMT
     C                   MOVE      '1'           *IN89
     C                   MOVE      '1'           *IN99
     C                   GOTO      SRH1E
     C                   END
     C*
     C* VERIFY THAT THE REFUND WILL NOT TAKE BILL AMOUNT BELOW MINIMUM
     C* BILLING AMOUNT
     C* IF REFUND WILL TAKE BILLING AMOUNT BELOW MINIMUM APPLY AS MUCH
     C* REFUND AMOUNT AS POSSIBLE
     C* IF BILLING AMOUNT IS ALREADY AT MINIMUM BILLING AMOUNT ZERO
     C* OUT REFUND AMOUNT
     C*
     C     XARAMT        IFNE      BDAM02
     C     XARAMT        SUB       XXBAMT        X1BAMT
     C     X1BAMT        IFLT      BDAM02
     C     ARAMT         SUB       BDAM02        XXBAMT
     C                   ELSE
     C                   Z-ADD     X1BAMT        XXBAMT
     C                   END                                                    END X1BAMT
     C                   ELSE                                                   ELSE ARAMT
     C                   Z-ADD     *ZEROS        XXBAMT
     C                   MOVE      '1'           *IN97
     C                   MOVE      '1'           *IN99
     C                   END                                                    END ARAMT
     C*
     C     SRH1E         ENDSR
     C/EJECT
     C**************************************************************************
     C* PROCESS H2 BILLING TYPE                                        S R H 2 *
     C**************************************************************************
     C     SRH2          BEGSR
     C*
     C* CHAIN TO DETAIL CHARGE FILE
     C*
     C                   Z-ADD     *ZEROS        #LIN
     C     CAKEY         CHAIN     PCA01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C* ACCUMULATE THE NUMBER OF DETAIL LINES
     C*
     C                   ADD       1             #LIN              5 0
     C*
     C* READ THE NEXT CASE DETAIL RECORD FOR THIS CLAIM.
     C*
     C     CAKEY         READE     PCA01                                  50
     C*
     C                   ENDDO                                                  END DOWEQ 50
     C*
     C     #LIN          IFLE      0
     C                   Z-ADD     1             #LIN
     C                   END
     C*
     C                   Z-ADD     BDAM01        L1BAMT
     C                   SUB       1             #LIN
     C*
     C     #LIN          IFGT      *ZERO
     C     #LIN          MULT      BDAM02        L2BAMT
     C                   ELSE
     C                   Z-ADD     *ZERO         L2BAMT
     C                   END
     C     L1BAMT        ADD       L2BAMT        XXBAMT
     C*
     C     EOSYS         IFEQ      'H'
     C*** BEG CHG *** 01/09/99 **************************************
     C     ##PRVB        IFEQ      'Y'
     C*
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         IFEQ      '0'
     C     XXBAMT        IFGT      EABAMT
     C     XXBAMT        SUB       EABAMT        XXBAMT
     C                   MOVE      '1'           *IN87
     C                   MOVE      '1'           *IN99
     C                   EXSR      SRTM
     C                   GOTO      SRH2E
     C                   END                                                    END XXBAMT GT HB1AAM
     C                   END                                                    END *IN60 EQ '0'
     C*
     C                   ELSE                                                   ##PRVB = Y
     C*
     C     HBKEY         CHAIN     PHB01                              60
     C     *IN60         IFEQ      '0'
     C     XXBAMT        IFGT      HB1AAM
     C     XXBAMT        SUB       HB1AAM        XXBAMT
     C                   MOVE      '1'           *IN87
     C                   MOVE      '1'           *IN99
     C                   EXSR      SRTM
     C                   GOTO      SRH2E
     C                   END                                                    END XXBAMT GT HB1AAM
     C                   END                                                    END *IN60 EQ '0'
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/09/99 ***************************************
     C                   END                                                    END EOSYS EQ 'H'
     C*
     C* READ THRU A/R FILE TO SEE IF ANY PRIOR RECON ADJUSTMENTS HAVE
     C* BEEN POSTED TO THIS EOMB.  IF THEY HAVE VERIFY THIS ONE WILL NOT
     C* TAKE BILLING AMOUNT BELOW THE MINIMUM BILLING AMOUNT
     C*
     C     *IN86         IFEQ      '1'
     C                   Z-ADD     *ZEROS        XARAMT
     C                   ADD       ARAMT         XARAMT
     C     ARKEY         CHAIN     PAR01                              55
     C     *IN55         DOWEQ     '0'
     C*
     C     ARTRCD        IFEQ      'RC'
     C     ARTRTP        ANDEQ     'A'
     C                   ADD       ARAMT         XARAMT
     C                   END                                                    END ARTRCD
     C*
     C     ARKEY         READE     PAR01                                  55
     C                   END                                                    END *IN55 A/R
     C                   ELSE                                                   ELSE *IN86 EQ '1'
     C                   Z-ADD     HB1AAM        XARAMT
     C                   END                                                    END *IN86 EQ '1'
     C*
     C* IF THE BILL AMOUNT HAS NOT CHANGED, PRINT CREDIT MEMO INDICATING THIS
     C*
     C     XXBAMT        IFEQ      XARAMT
     C                   MOVE      '1'           *IN89
     C                   MOVE      '1'           *IN99
     C                   GOTO      SRH2E
     C                   END
     C*
     C* VERIFY THAT THE REFUND WILL NOT TAKE BILL AMOUNT BELOW MINIMUM
     C* BILLING AMOUNT
     C* IF REFUND WILL TAKE BILLING AMOUNT BELOW MINIMUM APPLY AS MUCH
     C* REFUND AMOUNT AS POSSIBLE
     C* IF BILLING AMOUNT IS ALREADY AT MINIMUM BILLING AMOUNT ZERO
     C* OUT REFUND AMOUNT
     C*
     C     XARAMT        IFNE      XXBAMT
     C     XARAMT        SUB       XXBAMT        XXBAMT
     C                   ELSE                                                   ELSE ARAMT
     C                   Z-ADD     *ZEROS        XXBAMT
     C                   MOVE      '1'           *IN89
     C                   MOVE      '1'           *IN99
     C                   END                                                    END ARAMT
     C*
     C     SRH2E         ENDSR
     C/EJECT
     C**************************************************************************
     C* PROCESS P2 BILLING TYPE                                        S R P 2 *
     C**************************************************************************
     C     SRP2          BEGSR
     C*
     C* CHAIN TO DETAIL CHARGE FILE
     C*
     C                   Z-ADD     *ZEROS        #LIN
     C     CAKEY         CHAIN     PCA01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C* ACCUMULATE THE NUMBER OF DETAIL LINES
     C*
     C                   ADD       1             #LIN              5 0
     C*
     C* READ THE NEXT CASE DETAIL RECORD FOR THIS CLAIM.
     C*
     C     CAKEY         READE     PCA01                                  50
     C*
     C                   ENDDO                                                  END DOWEQ 50
     C*
     C     #LIN          IFLE      0
     C                   Z-ADD     1             #LIN
     C                   END
     C*
     C                   Z-ADD     BDAM01        L1BAMT
     C                   SUB       1             #LIN
     C*
     C     #LIN          IFGT      *ZERO
     C     #LIN          MULT      BDAM02        L2BAMT
     C                   ELSE
     C                   Z-ADD     *ZERO         L2BAMT
     C                   END
     C     L1BAMT        ADD       L2BAMT        XXBAMT
     C*
     C     XXBAMT        IFGT      EOBAMT
     C     XXBAMT        SUB       EOBAMT        XXBAMT
     C                   MOVE      '1'           *IN87
     C                   MOVE      '1'           *IN99
     C                   EXSR      SRTM
     C                   GOTO      SRP2E
     C                   END                                                    END XXBAMT GT HB1AAM
     C*
     C* READ THRU A/R FILE TO SEE IF ANY PRIOR RECON ADJUSTMENTS HAVE
     C* BEEN POSTED TO THIS EOMB.  IF THEY HAVE VERIFY THIS ONE WILL NOT
     C* TAKE BILLING AMOUNT BELOW THE MINIMUM BILLING AMOUNT
     C*
     C     *IN86         IFEQ      '1'
     C                   Z-ADD     *ZEROS        XARAMT
     C                   ADD       ARAMT         XARAMT
     C     ARKEY         CHAIN     PAR01                              55
     C     *IN55         DOWEQ     '0'
     C*
     C     ARTRCD        IFEQ      'RC'
     C     ARTRTP        ANDEQ     'A'
     C                   ADD       ARAMT         XARAMT
     C                   END                                                    END ARTRCD
     C*
     C     ARKEY         READE     PAR01                                  55
     C                   END                                                    END *IN55 A/R
     C                   ELSE                                                   ELSE *IN86 EQ '1'
     C                   Z-ADD     EOBAMT        XARAMT
     C                   END                                                    END *IN86 EQ '1'
     C*
     C* IF THE BILL AMOUNT HAS NOT CHANGED, PRINT CREDIT MEMO INDICATING THIS
     C*
     C     XXBAMT        IFEQ      XARAMT
     C                   MOVE      '1'           *IN89
     C                   MOVE      '1'           *IN99
     C                   GOTO      SRP2E
     C                   END
     C*
     C* VERIFY THAT THE REFUND WILL NOT TAKE BILL AMOUNT BELOW MINIMUM
     C* BILLING AMOUNT
     C* IF REFUND WILL TAKE BILLING AMOUNT BELOW MINIMUM APPLY AS MUCH
     C* REFUND AMOUNT AS POSSIBLE
     C* IF BILLING AMOUNT IS ALREADY AT MINIMUM BILLING AMOUNT ZERO
     C* OUT REFUND AMOUNT
     C*
     C     XARAMT        IFNE      XXBAMT
     C     XARAMT        SUB       XXBAMT        XXBAMT
     C                   ELSE                                                   ELSE ARAMT
     C                   Z-ADD     *ZEROS        XXBAMT
     C                   MOVE      '1'           *IN89
     C                   MOVE      '1'           *IN99
     C                   END                                                    END ARAMT
     C*
     C     SRP2E         ENDSR
     C/EJECT
     C**************************************************************************
     C* PROCESS P3 BILLING TYPE                                        S R P 3 *
     C**************************************************************************
     C     SRP3          BEGSR
     C*
     C     EOAMTC        SUB       EOAMT         XXDISA
     C     XXDISA        SUB       EOOTHR        XXDISA
     C     XXDISA        MULT      BDPC01        XMBAMT
     C*
     C* CHECK TO SEE IF CALCULATED BILL AMOUNT EXCEEDS THE GREATEST ALLOWABLE
     C* BILL AMOUNT ESTABLISHED
     C*
     C     XMBAMT        IFGT      BDAM01
     C                   Z-ADD     BDAM01        XMBAMT
     C                   END
     C*
     C     EOOTHR        MULT      BDPC02        XOBAMT
     C     XMBAMT        ADD       XOBAMT        XXBAMT
     C*
     C     XXBAMT        IFGT      EOBAMT
     C     XXBAMT        SUB       EOBAMT        XXBAMT
     C                   MOVE      '1'           *IN87
     C                   MOVE      '1'           *IN99
     C                   EXSR      SRTM
     C                   GOTO      SRP3E
     C                   END                                                    END XXBAMT GT HB1AAM
     C*
     C* READ THRU A/R FILE TO SEE IF ANY PRIOR RECON ADJUSTMENTS HAVE
     C* BEEN POSTED TO THIS EOMB.  IF THEY HAVE VERIFY THIS ONE WILL NOT
     C* TAKE BILLING AMOUNT BELOW THE MINIMUM BILLING AMOUNT
     C*
     C     *IN86         IFEQ      '1'
     C                   Z-ADD     *ZEROS        XARAMT
     C                   ADD       ARAMT         XARAMT
     C     ARKEY         CHAIN     PAR01                              55
     C     *IN55         DOWEQ     '0'
     C*
     C     ARTRCD        IFEQ      'RC'
     C     ARTRTP        ANDEQ     'A'
     C                   ADD       ARAMT         XARAMT
     C                   END                                                    END ARTRCD
     C*
     C     ARKEY         READE     PAR01                                  55
     C                   END                                                    END *IN55 A/R
     C                   ELSE                                                   ELSE *IN86 EQ '1'
     C                   Z-ADD     EOBAMT        XARAMT
     C                   END                                                    END *IN86 EQ '1'
     C*
     C* IF THE BILL AMOUNT HAS NOT CHANGED, PRINT CREDIT MEMO INDICATING THIS
     C*
     C     XXBAMT        IFEQ      XARAMT
     C                   MOVE      '1'           *IN89
     C                   MOVE      '1'           *IN99
     C                   GOTO      SRP3E
     C                   ELSE
     C                   END
     C*
     C* VERIFY THAT THE REFUND WILL NOT TAKE BILL AMOUNT BELOW MINIMUM
     C* BILLING AMOUNT
     C* IF REFUND WILL TAKE BILLING AMOUNT BELOW MINIMUM APPLY AS MUCH
     C* REFUND AMOUNT AS POSSIBLE
     C* IF BILLING AMOUNT IS ALREADY AT MINIMUM BILLING AMOUNT ZERO
     C* OUT REFUND AMOUNT
     C*
     C     XARAMT        IFNE      BDAM02
     C     XARAMT        SUB       XXBAMT        X1BAMT
     C     X1BAMT        IFLT      BDAM02
     C     ARAMT         SUB       BDAM02        XXBAMT
     C                   ELSE
     C                   Z-ADD     X1BAMT        XXBAMT
     C                   END                                                    END X1BAMT
     C                   ELSE                                                   ELSE ARAMT
     C                   Z-ADD     *ZEROS        XXBAMT
     C                   MOVE      '1'           *IN97
     C                   MOVE      '1'           *IN99
     C                   END                                                    END ARAMT
     C*
     C     SRP3E         ENDSR
     C/EJECT
     C**************************************************************************
     C* WRITE T/M RECORD FOR ADDITIONAL BILLING                        S R T M *
     C**************************************************************************
     C     SRTM          BEGSR
     C*
     C* FIND THE NON-INVOICED 'M' SYSTEM TYPE IN FAEOP, FOR THIS CLAIM
     C*
     C                   Z-ADD     *ZEROS        X1EOB#
     C     EOKY1         CHAIN     PEO01                              50
     C     *IN50         DOWEQ     '0'
     C                   EXCEPT    PEO01E
     C     EOBLFL        IFNE      'P'
     C     EOSYS         ANDEQ     'M'
     C                   Z-ADD     EOEOB#        X1EOB#
     C                   END                                                    END EOBLFL NE 'P'
     C     EOKY1         READE     PEO01                                  50
     C                   END                                                    END DOWEQ '0'
     C*
     C* IF NO NON-INVOICED 'M' TYPE FAEOP RECORD IS NOT FOUND, CREATE ONE
     C*
     C     X1EOB#        IFEQ      *ZEROS
     C*
     C* MOVE FIELDS FOR WRITE TO FAEOP FILE
     C*
     C* GET THE NEXT AVAILABLE EOB# FROM CO AUXILLARY FILE
     C*
     C     C1KEY         CHAIN     PC101                              60
     C     *IN60         IFEQ      '0'
     C     C1EOB#        IFEQ      *HIVAL
     C                   Z-ADD     1             C1EOB#
     C                   ELSE
     C                   ADD       1             C1EOB#
     C                   END
     C                   UPDATE    PC101
     C                   ELSE
     C                   Z-ADD     *HIVAL        C1EOB#
     C                   END
     C*
     C                   Z-ADD     C1EOB#        EOCHK#
     C                   Z-ADD     C1EOB#        EOEOB#
     C                   Z-ADD     C1EOB#        X1EOB#
     C*
     C                   Z-ADD     XXCO#         EOCO#
     C                   Z-ADD     XXSUB#        EOSUB#
     C                   Z-ADD     XXFND         EOFND
     C                   Z-ADD     XXFYR         EOFYR
     C                   Z-ADD     XXCASE        EOCASE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOTRDT
     C                   MOVE      *ZEROS        EOFEIN
     C                   Z-ADD     *ZEROS        EOMOD
     C                   Z-ADD     *ZEROS        EOAMT
     C                   Z-ADD     *ZEROS        EOFRDT
     C                   Z-ADD     *ZEROS        EOTODT
     C                   MOVE      *BLANKS       EOTRCD
     C                   MOVE      *BLANKS       EOPNAM
     C                   MOVE      *BLANKS       EOADR1
     C                   MOVE      *BLANKS       EOADR2
     C                   MOVE      *BLANKS       EOCTY
     C                   MOVE      *BLANKS       EOST
     C                   Z-ADD     *ZEROS        EOZIP
     C                   MOVE      *BLANKS       EOEOB1
     C                   MOVE      *BLANKS       EOEOB2
     C                   MOVE      *BLANKS       EOEOB3
     C                   MOVE      'M'           EOSYS
     C                   MOVE      *BLANKS       EOFLG1
     C                   MOVE      *BLANKS       EOFLG2
     C                   MOVE      *BLANKS       EOFLG3
     C                   MOVE      *BLANKS       EOFLG4
     C                   MOVE      *BLANKS       EOFLG5
     C                   MOVE      *BLANKS       EOFLG6
     C                   MOVE      *BLANKS       EOFLG7
     C                   MOVE      *BLANKS       EOFLG8
     C                   MOVE      *BLANKS       EOFLG9
     C                   Z-ADD     *ZEROS        EOAMTC
     C                   MOVE      *BLANKS       EOSTAT
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     Q1USER        EOIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOIDT
     C                   Z-ADD     *ZEROS        EOWCCK
     C                   MOVE      *BLANKS       EODIA1
     C                   MOVE      *BLANKS       EODIA2
     C                   MOVE      *BLANKS       EODIA3
     C                   MOVE      *BLANKS       EODIA4
     C                   MOVE      *BLANKS       EOEMPL
     C                   Z-ADD     *ZEROS        EODFC
     C                   MOVE      *BLANKS       EOFLIC
     C                   MOVE      'N'           EOTAPE
     C                   Z-ADD     1             EO#BLL
     C                   MOVE      *BLANKS       EOCLSF
     C                   MOVE      *BLANKS       EOAST
     C                   MOVE      *BLANKS       EOAUDT
     C                   MOVE      *BLANKS       EOBTFL
     C                   MOVE      'H'           EOHRFL
     C                   MOVE      *BLANKS       EOBLFL
     C                   MOVE      *BLANKS       EONLFL
     C                   MOVE      *BLANKS       EODIFL
     C                   Z-ADD     *ZEROS        EOINV#
     C                   MOVE      *BLANKS       EORMDS
     C                   MOVE      *BLANKS       EORMK1
     C                   MOVE      *BLANKS       EORMK2
     C                   MOVE      *BLANKS       EORMK3
     C                   MOVE      *BLANKS       EOCKCN
     C                   Z-ADD     *ZEROS        EOOTHR
     C                   Z-ADD     *ZEROS        EONDIS
     C                   MOVE      *BLANKS       EOSB01
     C                   MOVE      *BLANKS       EOSB02
     C                   MOVE      *BLANKS       EOSB03
     C                   MOVE      *BLANKS       EOSB04
     C                   MOVE      *BLANKS       EOSB05
     C                   MOVE      *BLANKS       EOSB06
     C                   MOVE      *BLANKS       EOSB07
     C                   MOVE      *BLANKS       EOSB08
     C                   MOVE      *BLANKS       EOSB09
     C                   MOVE      *BLANKS       EOSB10
     C                   Z-ADD     *ZEROS        EOBAMT
     C                   MOVE      *BLANKS       EOZIPA
     C                   MOVE      *BLANKS       EOCERT
     C                   Z-ADD     *ZEROS        EOEXT
     C                   WRITE     PEO01
     C*
     C*** BEG CHG *** 01/09/99 ***************************************
     C*  WRITE  THE EOMB AUXILARY FILE
     C*
     C     CMKEY         CHAIN     PCM01                              51
     C     *IN51         IFEQ      *OFF
     C                   Z-ADD     DBRRN         ##RRN
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'W'           XXFAE1
     C*
     C                   CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      C1EOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/18/99 **************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/18/99 **************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM                    ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C* MOVE FIELDS FOR WRITE TO NAMBP FILE
     C*
     C     CMKEY         CHAIN     PCM01                              51
     C     *IN51         IFEQ      *OFF
     C                   Z-ADD     DBRRN         ##RRN
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'W'           XXNAMB
     C*
     C                   CALL      'AA001R'
     C                   PARM      DBRRN         ##RRN             9 0          RRN OF CASE
     C                   PARM      C1EOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/18/99 **************************************
     C                   PARM      EORCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/18/99 **************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM                    ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM      'H'           ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C*** END CHG *** 01/09/99 ***************************************
     C*
     C* CALL THE BILLING PROCESS TO CREATE MEDICAL MANGEMTN SET-UP BILLING
     C*
     C                   MOVE      XXCO#         Q2CO#             3
     C                   MOVE      XXSUB#        Q2SUB#            5
     C                   MOVE      XXFND         Q2FND             3
     C                   MOVE      XXFYR         Q2FYR             3
     C                   MOVE      XXCASE        Q2CASE            7
     C                   MOVE      C1EOB#        Q2EOB#            9
     C                   CALL      'N6181R'
     C                   PARM                    Q2CO#
     C                   PARM                    Q2SUB#
     C                   PARM                    Q2FND
     C                   PARM                    Q2FYR
     C                   PARM                    Q2CASE
     C                   PARM                    Q2EOB#
     C*
     C                   END                                                    END X1EOB# EQ *ZEROS
     C*
     C* CHAIN TO THE MEDICAL SERVICES FIEL TO SEE IF BRVW ALREADY EXISTS FOR
     C* THE CLAIM
     C*
     C     MSKEY         CHAIN     PMS03                              60
     C     *IN60         IFEQ      '1'
     C*
     C* GET NEXT SEQUENCE NUMBER
     C*
     C                   Z-ADD     *HIVAL        X1SEQ#
     C     MSKEY1        SETGT     PMS02
     C                   READP     PMS02                                  60
     C     *IN60         IFEQ      '0'
     C     M2CO#         ANDEQ     XXCO#
     C     M2SUB#        ANDEQ     XXSUB#
     C     M2FND         ANDEQ     XXFND
     C     M2FYR         ANDEQ     XXFYR
     C     M2CASE        ANDEQ     XXCASE
     C     M2SEQ#        ADD       1             X2SEQ#
     C                   ELSE
     C                   Z-ADD     1             X2SEQ#
     C                   END
     C*
     C                   Z-ADD     XXCO#         MSCO#
     C                   Z-ADD     XXSUB#        MSSUB#
     C                   Z-ADD     XXFND         MSFND
     C                   Z-ADD     XXFYR         MSFYR
     C                   Z-ADD     XXCASE        MSCASE
     C                   MOVEL     'BRVW'        MSMSCD
     C                   Z-ADD     X2SEQ#        MSSEQ#
     C                   MOVE      *BLANKS       MSICD9
     C                   MOVE      *BLANKS       MSLOST
     C                   MOVE      *BLANKS       MSDPR#
     C                   MOVE      *BLANKS       MSNAME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MSBGDT
     C                   Z-ADD     *ZEROS        MSENDT
     C                   MOVE      *BLANKS       MSSTS
     C                   MOVEL     XXSTAF        MSSTAF
     C                   MOVE      *BLANKS       MSCOMM
     C                   MOVE      *BLANKS       MSSTAT
     C                   MOVEL     Q1USER        MSUSER
     C                   MOVEL     Q1PGM         MSPGM
     C                   Z-ADD     DSTIME        MSTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MSDT
     C                   MOVEL     Q1USER        MSIUSR
     C                   Z-ADD     DSTIME        MSITIM
     C                   MOVE      *BLANKS       MSFL01
     C                   MOVE      *BLANKS       MSFL02
     C                   MOVE      *BLANKS       MSFL03
     C                   MOVE      *BLANKS       MSFL04
     C                   MOVE      *BLANKS       MSFL05
     C                   MOVE      *BLANKS       MSFL06
     C                   MOVE      *BLANKS       MSFL07
     C                   MOVE      *BLANKS       MSFL08
     C                   MOVE      *BLANKS       MSFL09
     C                   MOVE      *BLANKS       MSFL10
     C                   MOVE      *BLANKS       MS10A1
     C                   MOVE      *BLANKS       MS10A2
     C                   MOVE      *BLANKS       MS10A3
     C                   MOVE      *BLANKS       MS10A4
     C                   MOVE      *BLANKS       MS10A5
     C                   MOVE      *BLANKS       MS40A1
     C                   MOVE      *BLANKS       MS40A2
     C                   Z-ADD     *ZEROS        MS60N1
     C                   Z-ADD     *ZEROS        MS60N2
     C                   WRITE     PMS01
     C*
     C                   ELSE                                                   ELSE *IN60 EQ '1'
     C*
     C                   Z-ADD     *HIVAL        X1SEQ#
     C     MSKEY1        SETGT     PMS02
     C                   READP     PMS02                                  60
     C     *IN60         IFEQ      '0'
     C     M2CO#         ANDEQ     XXCO#
     C     M2SUB#        ANDEQ     XXSUB#
     C     M2FND         ANDEQ     XXFND
     C     M2FYR         ANDEQ     XXFYR
     C     M2CASE        ANDEQ     XXCASE
     C     M2SEQ#        ADD       1             X2SEQ#
     C                   ELSE
     C                   Z-ADD     1             X2SEQ#
     C                   END
     C*
     C                   END                                                    END *IN60 EQ '1'
     C*
     C* EXECUTE THE SUBROUTINE TO DETERMINE ANY PRIOR ADDITIONAL AMOUNT BILLED
     C*
     C                   EXSR      SRTM1
     C*
     C                   MOVE      *HIVAL        XXSEQ#
     C     TMKY1         SETGT     PTM02
     C                   READP     PTM02                                  60
     C     *IN60         IFEQ      '0'
     C     T2CO#         ANDEQ     XXCO#
     C     T2SUB#        ANDEQ     XXSUB#
     C     T2FND         ANDEQ     XXFND
     C     T2FYR         ANDEQ     XXFYR
     C     T2CASE        ANDEQ     XXCASE
     C     T2EOB#        ANDEQ     X1EOB#
     C                   Z-ADD     T2SEQ#        XXSEQ#
     C                   ELSE
     C                   Z-ADD     *ZERO         XXSEQ#
     C                   END
     C*
     C                   ADD       1             XXSEQ#
     C*
     C                   Z-ADD     XXCO#         TMCO#
     C                   Z-ADD     XXSUB#        TMSUB#
     C                   Z-ADD     XXFND         TMFND
     C                   Z-ADD     XXFYR         TMFYR
     C                   Z-ADD     XXCASE        TMCASE
     C                   Z-ADD     X1EOB#        TMEOB#
     C                   MOVEL     XXSTAF        TMSTAF
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TMSVDT
     C                   MOVEL     'ABR'         TMACT
     C                   MOVE      *BLANKS       TMMVFL
     C                   MOVEL     'N'           TMPNFL
     C                   Z-ADD     *ZEROS        TMHRS
     C                   Z-ADD     *ZEROS        TMHRRT
     C                   Z-ADD     *ZEROS        TMMILE
     C                   Z-ADD     *ZEROS        TMMIRT
     C                   Z-ADD     XXBAMT        TMMSCG
     C                   MOVEL     TMDSC(1)      TMDESC
     C                   MOVE      XXSEQ#        TMSEQ#
     C                   MOVEL     'BRVW'        TMMSCD
     C                   Z-ADD     X2SEQ#        TMMSEQ
     C                   MOVE      *BLANKS       TMMVFL
     C                   Z-ADD     *ZEROS        TMINV#
     C                   MOVE      'H'           TMHOLD
     C                   MOVE      *BLANKS       TMPCS
     C                   MOVE      *BLANKS       TMPRTF
     C                   MOVE      *BLANKS       TMBNON
     C                   MOVE      *BLANKS       TMEXPS
     C                   MOVE      *BLANKS       TMFLG1
     C                   MOVE      *BLANKS       TMFLG2
     C                   MOVE      *BLANKS       TMFLG3
     C                   MOVE      *BLANKS       TMFLG4
     C                   MOVE      *BLANKS       TMFLG5
     C                   Z-ADD     *ZEROS        TMOIDT
     C                   MOVE      *BLANKS       TMTYPC
     C                   MOVE      *BLANKS       TMBCH#
     C                   MOVE      *BLANKS       TMSTAT
     C                   MOVE      *BLANKS       TMFL06
     C                   MOVE      *BLANKS       TMFL07
     C                   MOVE      *BLANKS       TMFL08
     C                   MOVE      *BLANKS       TMFL09
     C                   MOVE      *BLANKS       TMFL10
     C                   MOVE      *BLANKS       TM10A1
     C                   MOVE      *BLANKS       TM10A2
     C                   MOVE      *BLANKS       TM10A3
     C                   MOVE      *BLANKS       TM10A4
     C                   MOVE      *BLANKS       TM10A5
     C                   MOVE      XXEOB#        TM92N1
     C                   Z-ADD     *ZEROS        TM92N2
     C                   Z-ADD     *ZEROS        TM92N3
     C                   Z-ADD     *ZEROS        TM92N4
     C                   Z-ADD     *ZEROS        TM92N5
     C                   Z-ADD     *ZEROS        TM60N1
     C                   Z-ADD     *ZEROS        TM60N2
     C                   Z-ADD     *ZEROS        TM60N3
     C                   Z-ADD     *ZEROS        TM60N4
     C                   Z-ADD     *ZEROS        TM60N5
     C                   MOVEL     Q1USER        TMIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TMIDT
     C                   Z-ADD     DSTIME        TMITIM
     C                   MOVEL     Q1USER        TMUSER
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TMDT
     C                   WRITE     PTM01
     C*
     C* CHAIN TO THE MEDICAL SERVICE FILE AND UPDATE THE BILL AMOUNT
     C*
     C*** BEG CHG *** 01/09/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         IFEQ      '0'
     C     EABAMT        ADD       XXBAMT        EABAMT
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXNAMB
     C     CMKEY         CHAIN     PCM01                              61
     C     *IN61         IFEQ      *OFF
     C                   Z-ADD     DBRRN         ##RRN
     C                   ENDIF
     C*
     C                   CALL      'AA001R'
     C                   PARM                    ##RRN             9 0          RRN OF CASE
     C                   PARM      X1EOB#        ##EOB#            9 0          EOB #
     C*** BEG CHG *** 03/18/99 ***************************************
     C                   PARM      EARCN         ##RCN             3 0          RECON #
     C**********           PARM           ##RCN   30       RECON #
     C*** END CHG *** 03/18/99 ***************************************
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM      EABAMT        ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C                   ENDIF                                                  *IN60 = OFF
     C*
     C                   ELSE
     C     MBKEY         CHAIN     PMB01                              60
     C     *IN60         IFEQ      '0'
     C     MBBAMT        ADD       XXBAMT        MBBAMT
     C                   MOVE      *BLANKS       KKWRUP
     C                   MOVE      'U'           XXNAMB
     C     CMKEY         CHAIN     PCM01                              61
     C     *IN61         IFEQ      *OFF
     C                   Z-ADD     DBRRN         ##RRN
     C                   ENDIF
     C*
     C                   CALL      'AA001R'
     C                   PARM                    ##RRN             9 0          RRN OF CASE
     C                   PARM      X1EOB#        ##EOB#            9 0          EOB #
     C                   PARM                    ##RCN             3 0          RECON #
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM      MBBAMT        ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM                    ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      EOSYS         ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*
     C                   END                                                    END *IN60 MBKEY
     C*
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/09/99 ***************************************
     C                   ENDSR
     C/EJECT
     C**************************************************************************
     C* CALCULATE ALL PRIOR ADDITIONAL BILLINGS                      S R T M 1 *
     C**************************************************************************
     C     SRTM1         BEGSR
     C*
     C* READ T/M FILE TO CALCULATE ALL PRIOR ADDITIONAL BILLINGS
     C*
     C                   Z-ADD     *ZEROS        XXADDT
     C     TMKEY         CHAIN     PTM02                              55
     C     *IN55         DOWEQ     '0'
     C                   MOVE      T292N1        X2EOB#
     C*
     C     T2ACT         IFEQ      'ABR'
     C     X2EOB#        ANDEQ     XXEOB#
     C*
     C     XXADDT        ADD       T2MSCG        XXADDT
     C*
     C                   END
     C*
     C     TMKEY         READE     PTM02                                  55
     C                   END
     C*
     C     XXBAMT        SUB       XXADDT        XXBAMT
     C*
     C                   ENDSR
     C/EJECT
     C**************************************************************************
     C* WRITE A/R OFFSET INFORMATION                                 W R T A R *
     C**************************************************************************
     C     WRTAR         BEGSR
     C*
     C                   Z-ADD     *ZEROS        XXBAMT
     C                   Z-ADD     *ZEROS        XMBAMT
     C                   Z-ADD     *ZEROS        XOBAMT
     C*
     C                   MOVE      EOSYS         XXSYS
     C                   Z-ADD     EOINV#        XXINV#
     C*
     C* NOW WE CAN CHAIN TO THE A/R FILE TO SEE IF ANY A/R RECORDS
     C* EXIST FOR THIS EOMB
     C*
     C                   MOVE      '0'           *IN86
     C     ARKEY         CHAIN     PAR01                              60
     C     CNTAR         TAG
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN86
     C*
     C* WE MUST VEFIY THAT WE HAVE THE ORIGNIAL INVOICE SO WE GET THE
     C* CORRECT A/R AMOUNT FOR THIS EOMB
     C*
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDEQ     'B'
     C*
     C* WE MUST VERIFY WHICH TYPE OF INVOICE THIS EOMB WAS ON(I.E. SUMMARY
     C* OR SINGLE CASE)
     C*
     C     ARCASE        IFEQ      *ZEROS
     C                   MOVE      'Y'           XXSUMM
     C                   ELSE                                                   ELSE ARCASE
     C                   MOVE      'N'           XXSUMM
     C                   END                                                    END ARCASE
     C*
     C* IF WE GET A HIT FOR THIS EOMB IN THE A/R FILE THEN CHAIN TO THE
     C* GROUP MASTER FILE AND GET THE BILL RATE CODE FOR THIS SYSTEM
     C* TYPE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    FNNM(Z)                            60
     C                   MOVEA     FNNM(Z)       XXFNNM           35
     C*
     C* GET THE BILL RATE CODE FOR HTIS SYSTEM TYPE FOR THIS GROUP
     C*
     C                   Z-ADD     1             X                 5 0
     C     EOSYS         LOOKUP    FSY(X)                                 62
     C     *IN62         IFEQ      '1'
     C                   MOVEL     FBR(X)        XXCODE            3
     C                   ELSE                                                   ELSE *IN62
     C                   MOVE      *BLANKS       XXCODE
     C                   END                                                    END *IN62
     C*
     C* NOW READ THE BILL RATE CODE MASTER FILE AND GET THE AMOUNTS
     C*
     C                   Z-ADD     UDATE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C     BDKEY         SETGT     PBD01
     C     NXTBDP        TAG
     C                   READP     PBD01                                  64
     C*
     C* VERIFY BILLING CODE AND STATUS
     C*
     C     *IN64         IFEQ      '0'
     C     BDCODE        IFNE      XXCODE
     C     BDSTAT        OREQ      'I'
     C                   GOTO      NXTBDP
     C                   END                                                    END CODE/STAT
     C                   END                                                    END *IN64
     C*
     C     *IN64         IFEQ      '0'
     C                   MOVEL     BDTYPE        XBTYPE            2
     C                   END                                                    END *IN64
     C*
     C* EXSR BILLING FORMULA SUBROUTINE TO CALCULATE THE AMOUNT OF REFUND
     C* DEPENDING ON WHAT BILL TYPE
     C*
     C     XBTYPE        IFEQ      'H1'
     C                   EXSR      SRH1
     C                   ELSE                                                   ELSE XBTYPE
     C     XBTYPE        IFEQ      'H2'
     C                   EXSR      SRH2
     C                   ELSE                                                   ELSE XBTYPE
     C     XBTYPE        IFEQ      'P2'
     C                   EXSR      SRP2
     C                   ELSE                                                   ELSE XBTYPE
     C     XBTYPE        IFEQ      'P3'
     C                   EXSR      SRP3
     C                   ELSE                                                   ELSE XBTYPE
     C     XBTYPE        IFEQ      'H3'
     C     XBTYPE        OREQ      'P1'
     C                   MOVE      '1'           *IN88
     C*
     C* EXECUTE THE SUBROUTINE TO PRINT THE CREDIT MEMO
     C*
     C                   EXSR      SRCRED
     C                   GOTO      NOAR
     C                   END                                                    END EQ 'H3'
     C                   END                                                    END EQ 'P3'
     C                   END                                                    END EQ 'P2'
     C                   END                                                    END EQ 'H2'
     C                   END                                                    END EQ 'H1'
     C*
     C* ONLY PROCESS THE WRITE OF THE REFUND A/R RECORD IF XXBAMT IS NOT
     C* ZEROS
     C     XXBAMT        IFNE      *ZEROS
     C     *IN89         ANDEQ     '0'
     C     *IN87         ANDEQ     '0'
     C*
     C* NOW WE ARE READY TO WRITE THE OFFSETTING A/R RECORD MOVE FIELDS
     C* ACCORDINGLY
     C*
     C                   Z-ADD     XXCO#         ARCO#
     C                   Z-ADD     XXSUB#        ARSUB#
     C                   Z-ADD     XXFND         ARFND
     C*
     C     XXSUMM        IFEQ      'Y'
     C                   Z-ADD     *ZEROS        ARFYR
     C                   Z-ADD     *ZEROS        ARCASE
     C                   ELSE                                                   ELSE XXSUMM
     C                   Z-ADD     XXFYR         ARFYR
     C                   Z-ADD     XXCASE        ARCASE
     C                   END                                                    END XXSUMM
     C                   MOVE      XXSYS         ARSYS
     C                   MOVE      'RC'          ARTRCD
     C                   MOVE      'A'           ARTRTP
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARPODT
     C                   MOVE      *BLANKS       ARDESC
     C                   MOVEL     'REFUND'      ARDESC
     C*
     C                   Z-SUB     XXBAMT        ARAMT
     C                   Z-ADD     XXEOB#        AREOB#
     C                   MOVE      *BLANKS       ARSTAT
     C                   MOVE      Q1USER        ARUSER
     C                   MOVE      Q1PGM         ARPGM
     C                   Z-ADD     DSTIME        ARTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C                   MOVE      *ZERO         ARCK#
     C                   Z-ADD     XXINV#        ARINV#
     C                   WRITE     PAR02
     C*
     C                   END                                                    END XXBAMT
     C*
     C* EXECUTE THE SUBROUTINE TO PRINT THE CREDIT MEMO
     C*
     C                   EXSR      SRCRED
     C*
     C                   END                                                    END *IN60 FND
     C*
     C                   ELSE                                                   ELSE ARTRTP/ARSTAT
     C*
     C     ARKEY         READE     PAR01                                  60
     C                   GOTO      CNTAR
     C*
     C                   END                                                    END ARTRTP/ARSTAT
     C*
     C                   ELSE                                                   ELSE *IN60 A/R
     C*
     C* GROUP MASTER FILE AND GET THE BILL RATE CODE FOR THIS SYSTEM
     C* TYPE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    FNNM(Z)                            60
     C                   MOVEA     FNNM(Z)       XXFNNM           35
     C*
     C* GET THE BILL RATE CODE FOR HTIS SYSTEM TYPE FOR THIS GROUP
     C*
     C                   Z-ADD     1             X                 5 0
     C     EOSYS         LOOKUP    FSY(X)                                 62
     C     *IN62         IFEQ      '1'
     C                   MOVEL     FBR(X)        XXCODE            3
     C                   ELSE                                                   ELSE *IN62
     C                   MOVE      *BLANKS       XXCODE
     C                   END                                                    END *IN62
     C*
     C* NOW READ THE BILL RATE CODE MASTER FILE AND GET THE AMOUNTS
     C*
     C                   Z-ADD     UDATE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C     BDKEY         SETGT     PBD01
     C     NXTBD1        TAG
     C                   READP     PBD01                                  64
     C*
     C* VERIFY BILLING CODE AND STATUS
     C*
     C     *IN64         IFEQ      '0'
     C     BDCODE        IFNE      XXCODE
     C     BDSTAT        OREQ      'I'
     C                   GOTO      NXTBD1
     C                   END                                                    END CODE/STAT
     C                   END                                                    END *IN64
     C*
     C     *IN64         IFEQ      '0'
     C                   MOVEL     BDTYPE        XBTYPE            2
     C                   END                                                    END *IN64
     C*
     C* EXSR BILLING FORMULA SUBROUTINE TO CALCULATE THE AMOUNT OF REFUND
     C* DEPENDING ON WHAT BILL TYPE
     C*
     C     XBTYPE        IFEQ      'H1'
     C                   EXSR      SRH1
     C                   ELSE                                                   ELSE XBTYPE
     C     XBTYPE        IFEQ      'H2'
     C                   EXSR      SRH2
     C                   ELSE                                                   ELSE XBTYPE
     C     XBTYPE        IFEQ      'P2'
     C                   EXSR      SRP2
     C                   ELSE                                                   ELSE XBTYPE
     C     XBTYPE        IFEQ      'P3'
     C                   EXSR      SRP3
     C                   ELSE                                                   ELSE XBTYPE
     C     XBTYPE        IFEQ      'H3'
     C     XBTYPE        OREQ      'P1'
     C                   MOVE      '1'           *IN88
     C*
     C* EXECUTE THE SUBROUTINE TO PRINT THE CREDIT MEMO
     C*
     C                   EXSR      SRCRED
     C                   GOTO      NOAR
     C                   END                                                    END EQ 'H3'
     C                   END                                                    END EQ 'P3'
     C                   END                                                    END EQ 'P2'
     C                   END                                                    END EQ 'H2'
     C                   END                                                    END EQ 'H1'
     C*
     C                   END                                                    END *IN60 FND
     C*
     C                   MOVE      '1'           *IN90
     C                   MOVE      '1'           *IN99
     C*
     C* EXECUTE THE SUBROUTINE TO PRINT THE CREDIT MEMO
     C*
     C                   EXSR      SRCRED
     C*
     C                   END                                                    END *IN60 A/R
     C*
     C     NOAR          TAG
     C*
     C                   ENDSR
     C**************************************************************************
     C/EJECT
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
     O                       ORTITL              80
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
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               57 'INC. 1994,1995'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      *
     OQSYSPRT   E            HEAD           1 04
     O*
     O                                          111 'ADJUSTED EOMB:'
     O                       XXEOB#        Z    122
     O          E            HEAD           1
     O                                          111 'PAGE:'
     O                       PAGE               122
     O          E            HEAD           1
     O                                          111 'DATE:'
     O                       DSDATE        Y    122
     O*
     O          E            HEAD           2 09
     O                                           73 '***** CREDIT MEMO ****'
     O          E            HEAD           1
     O                                           12 'SUBSC   :'
     O                       XXSUB#              19
     O                                           21 '-'
     O                       XXSBNM              57
     O*
     O                                           79 'PATIENT:'
     O                       XXPNAM             110
     O          E            HEAD           1
     O                                           12 'GROUP   :'
     O                       XXFND         Z     19
     O                                           21 '-'
     O                       XXFNNM              57
     O                                           79 'SOC SEC:'
      ***** Beg Chg *** 06/09/20 *******************************************
     O***                    XXSS#               91 '   -  -    '
     O                       XXSS#               91
      ***** End Chg *** 06/09/20 *******************************************
     O          E            HEAD           1
     O                                           12 'PROVIDER:'
     O                       XXPRV#        Z     23
     O               96                          25 '-'
     O               96      XXMOD         Z     29
     O                       XXPRNM              50
     O                                           79 'ACC  DT:'
     O                       XXACDT        Y     88
     O          E            HEAD           2
     O                                           79 'FILE # :'
     O                       XF#                100
     O          E            HEAD           1
     O                                           29 'ORIGINAL'
     O                                           97 'REFUND'
     O          E            HEAD           2
     O*
     O                                           10 'DATE'
     O                                           22 'INVOICE #'
     O                                           32 'EOB#/SVC#'
     O                                           49 'SERVICE DATES'
     O                                           61 'SERVICES'
     O                                           97 'AMOUNT'
     O          E            DET            1
     O                       XXTRDT              12 '  /  /  '
     O                       EOINV#        Z     22
     O                       EOEOB#        Z     32
     O                       XXFRDT              41 '  /  /  '
     O                                           43 '-'
     O                       XXTODT              52 '  /  /  '
     O                       XXDESC              83
     O              N99      XXBAMT        4     97
     O               88 99                       97 '***'
     O               89 99                       97 '****'
     O               90 99                       97 '**'
     O               97 99                       97 '*'
     O          E    87 99   DET            1
     O               87 99                       97 '*****'
     O          E            TOTL           1 43
     O                                           83 'REFUNDED TO CLAIM:'
     O              N99      XXBAMT        4B    97 '$'
     O          E            TOTL           1
     O                                           97 '============'
     O          E    97      TOTL           1
     O                                           26 '** MINIMUM CHARGE CRITE'
     O                                           49 'RIA MET - NO REFUND ISS'
     O                                           52 'UED'
     O          E    90      TOTL           1
     O                                           26 'ORIGINAL EOMB WAS NOT I'
     O                                           49 'NVOICED - NO REFUND ISS'
     O                                           52 'UED'
     O          E    88      TOTL           1
     O                                           26 'ORIGINAL EOMB PROCESSED'
     O                                           49 ' FOR A FLAT RATE - NO R'
     O                                           61 'EFUND ISSUED'
     O          E    89      TOTL           1
     O                                           26 'ORIGINAL BILL AMOUNT HA'
     O                                           49 'S NOT BEEN CHANGED - NO'
     O                                           63 'REFUND ISSUED'
     O          E    87      TOTL           1
     O                                           26 'BILLING AMOUNT HAS INCR'
     O                                           50 'EASED - NO REFUND ISSUED'
     O          E            TOTL           1 48
     O                                           53 'REFUNDED FROM:'
     O                       XCNAME              95
     O          E            TOTL           1
     O                       XCADR1              95
     O          E    94      TOTL           1
     O                       XCADR2              95
     O          E            TOTL           1
     O                       XCCSZ               95
     O          E            TOTL           2
     O                       XCPHN#              69
     O          E            TOTL           1
     O                                           53 'TAX ID #:'
     O                       XXFEIN        Z     64
     OPEO01     E            PEO01E
     OPC101     E            PC101E
** (DS)
LEVEL I - HOSPITAL BILL AUDIT
PROVIDER BILL REVIEW
PRE/RE-CERTIFICATION REVIEW
** (T/M DESCRIPTION)
ADDITIONAL BILL REVIEW
