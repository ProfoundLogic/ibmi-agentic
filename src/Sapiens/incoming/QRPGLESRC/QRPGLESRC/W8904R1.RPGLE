     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8904R1-WH- EXTRACT PDS CLAIMS - WMCMP
     H*                    TO WAREHOUSE - WHCLP
     H*****************************************************************
     H*   Description:
     H*
     H*   o Reads PDS claim files, WMCMP WMCHP WMCDP WDPAP
     H*         Omit group 999
     H*
     H*   o Creates warehouse extract, WHCLP
     H*
     H*================================================================
     H*  Called From:  Command Line.
     H*
     H*  Calls  to:
     H*
     H*  Created:  3.30.00     C:58   Project: WC924
     H*================================================================
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/09/05 C: 132   CI099 - 48925                               *
     H*             Change to create correct Policy number field      *
     H*                                                               *
     H*  07/22/05       C: 58      Project: 135WEH
     H*        For extracted policy#, use WMRPP/carrier number:
     H*          For: client 135;
     H*                                                               *
     H* 05/13/05 C:132  129DKP
     H*             If staff assignment code starts with "#", do not  *
     H*             extract name, leave blank. (ORIGINALLY WAS IN     *
     H*             CLT129 with 05/06/05 Date, under C: 156)          *
     H*                                                               *
     H* 11/15/04 C: 132 CI021                                         *
     H*             Add check on WDEHP file when a policy start date  *
     H*             in WDELL is in the future. If no WDEHP record     *
     H*             for future start dates, do not write the record.  *
     H*     NOTE: NO CHANGE TO THIS PGM, FOR INFO ONLY, IF HAVE A     *
     H*             CLAIM, WANT TO INCLUDE THE RECORD  ** ==== **     *
     H*                                                               *
     H* 11/05/04 C: 132 Support CI114 44461                           *
     H*             Change to use PDS codes for Part, Nature & cause, *
     H*             instead of doing table lookup for the EDI codes.  *
     H*                                                               *
     H* 08/11/04 C: 132 91DLK                                         *
     H*             Add Exclude Employers file WHXPP, Check file      *
     H*             before Write WHCLP record, if exists in WHXPP     *
     H*             file then do not write the record.                *
     H*                                                               *
     H* 07/23/04 C: 132 93DDP                                         *
     H*           - Add ten fields to WHCLP for various Recovery      *
     H*             Amounts and add calculations to load these new    *
     H*             fields.                                           *
     H*           - THESE FIELDS ARE NOT POPULATED IN THIS PROGRAM    *
     H*             AT THIS TIME, ONLY IN CLIENT 093                  *
     H*                                                               *
     H* 03/25/04 C: 132 CI010   and CI015                             *
     H*           - Add start Date PARM for Job Date, so each file    *
     H*             has same ASOF DATE, for next incremetal.  CI010   *
     H*                                                               *
     H* 03/10/04 C: 58  CI015
     H*             Add start fund year PARM; extract only therefrom.
     H*             Also, initialize numerics in WMCDP1
     H*                                                               *
     H* 02/27/04 C:132 SUPPORT CI099 40909                            *
     H*                Check for certain hex characters in output     *
     H*                alpha fields and replaced if exists. Fields    *
     H*                from tables and static files are not checked   *
     H*                                                               *
     H* 01/05/04 C:132 SUPPORT: FQ091/40219                           *
     H*                - Move code for as of date field to end of the *
     H*                  Subr BLDCMP so always is output to WHCLP     *
     H*                - Change Lost time days if negative & over     *
     H*                  999 days to minus 999 - TEMP FIX             *
     H*                                                               *
     H*****************************************************************
     H* 08/11/04 UNCOMMENTED --> W/CHANGES FOR 93DDP/91DLK  C:132     *
     H*                                                               *
     H***** WC909 COMMENTED OUT UNTIL CIC CENTER IS READY FOR CHANGES *
     H* 10/20/03 C:53 Project: WC909                                  *
     H*               Added country info.                             *
     H***** WC909 COMMENTED OUT UNTIL CIC CENTER IS READY FOR CHANGES *
     H*****************************************************************
     H*                                                               *
     H* 07/17/03 C:53 Support: WC065/38013                            *
     H*               Correct weeks/days calc.  When a neg recovery   *
     H*               is processed, the weeks/days should be added.   *
     H*                                                               *
     H* 05/30/02 C:132 PROJECT WC924                                  *
     H*                Add Adjuster ID, Phone, FAX and Email Fields   *
     H*                to WHCLP file & load fields from WMUPP         *
     H*                                                               *
     H* 04/22/02  C:132 PROJECT SEQ: 32154                            *
     H*               - Change: READ to READE on Paymnets file WDPAP  *
     H*                                                               *
     H* 10/02/01  C:132 PROJECT WC924                                 *
     H*               - Change: Add a check new FNCICF flag in WMFNP  *
     H*                 for a 'Y' to include in Extract = Else Bypass *
     H*
     H* 08/17/01 C:03  PROJECT WC924
     H*                Change THE CLEAR TO RSV TABLES
     H*
     H* 08/16/01 C:132 PROJECT WC924
     H*                Change PDS Reserve Array RT, element 10 to
     H*                correct Field name of CMRT10
     H*
     H* 07/30/01 C:132 PROJECT WC924
     H*                Added Code to Check Claim File Maintenance Date
     H*                Against Extact Parm Date. Bypass Records if File
     H*                Maint Date is <= Input Parm Date. Also Changed
     H*                to use Run Date for A/R paments & as KFLD in
     H*                key for Payroll File.
     H*
     H* 04/10/01 C:03  PROJECT WC924
     H*                ADDED CODE TO LOAD NEW FIELD CLICLM
     H*                 THIS PROGRMA WILL NEED TO BE MODIFIED FOR
     H*                 ANY CLIENT REQUIRING THEIR OWN CLM# TO BE
     H*                 LOADED TO CLCASE. THIS COULD BE LOADED FROM
     H*                 PDS BY USING CMCVT# OR CMACL# DEPENDING ON
     H*                 HOW/WHERE EACH CLT LOADED THEIR EXTERNALLY
     H*                 OR PRE-PDS CLM#
     H*                                                               *
     H* 03/21/01 C:61  PROJECT WC924                                  *
     H*                Adjust left/right field alignment.             *
     H*                                                               *
     H* 02/06/01 C:61  PROJECT WC924                                  *
     H*                Load Class Code Description                    *
     H*                                                               *
     H* 12/13/00 C:61  PROJECT WC924                                  *
     H*                Chg to accept mmddccyy date parm.              *
     H*                                                               *
     H* 09/05/00  C:58 PROJECT WC924
     H*                 Remove journal and error list.
     H*
     H* 06/10/00  C:58 PROJECT WC993
     H*                 Select only non-test groups.
     H*
     H* 05/15/00  C:58 PROJECT WC924
     H*                 ADD field: CLAODT - Data AS OF date.
     H*                                                               *
     F****************************************************************
     F* Indicators
     F*
     F*        43  LOKUP work indicator
     F*
     F*        51  Input record PDS claims WMCMP
     F*
     F*        55  Read, control WDPAP input
     F*        56  Chain claim status WMCHL
     F*        57  Chain claimant demographics WMCDL
     F*        58  Chain claim legal file WMLLL
     F*
     F*        60  General chain, read
     F*        62  Chain carrier WMRPP
     F*
     F*================================================================
     F*             FILES
     F*================================================================
     F*
     F*      INPUT
     F*
     F*    PDS CLAIM MASTER
     FWMCML2    IPE  E           K DISK
     F*
     F*    Claim Payment History
     FWDPAL     IF   E           K DISK
     F*
     F*    Claimant Demographics
     FWMCDL     IF   E           K DISK
     F*
     F*    Claim Status History
     FWMCHL     IF   E           K DISK
     F*
     F*    Group Master File - WMFNL
     FWMFNL     IF   E           K DISK
     F*
     F*    Legal File - WMLLL
     FWMLLL     IF   E           K DISK
     F*
     F*    Multi-claim file
     FWAMCL     IF   E           K DISK
     F*
     F*    User Parameter File.
     FWMUPL1    IF   E           K DISK
     F*
     F*    Adjustor State License File
     FSMALL     IF   E           K DISK
     F*
     F*    Generic Table File.
     FWTGTL     IF   E           K DISK
      *
      *    Employer Payroll
     FWMEPL1    IF   E           K DISK
      *
      *    Payroll Classification
     FWMPRL1    IF   E           K DISK
      *
      *    Rate Set Code Table
     FWMRSL     IF   E           K DISK
      *
      *    Payroll Description Extension
     FWMPXL1    IF   E           K DISK
      *
      *    Employer Policy Year
     FWDELL     IF   E           K DISK
      *
      ****** BEG ADD ***** 08/11/04 ************************************
     FWHXPL     IF   E           K DISK
      ****** END ADD ***** 08/11/04 ************************************
     F**** Begin Add ****  07/22/05  **************************************
     F*   Employer Carrier File
     FWMRPL     IF   E           K DISK
     F**** End Add ******  07/22/05  **************************************
     F*
     F*    Worker's Comp Table
     FWTABL     IF   E           K DISK
     F*    PPA01         WTPAP
     F*    PRT01         WTRTP
     F                                     RENAME(PPA01:PPA02)
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PB201)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PBS01)
     F                                     IGNORE(PTT01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PVS01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)
     F*
     F*----------------------------------------------------------------
     F*      OUTPUT
     F*----------------------------------------------------------------
     F*
     F*    Claim Master
     FWHCLP     O    E             DISK
     F*

     D*----------------------------------------------------------------
     D*      ARRAYS
     D*----------------------------------------------------------------
     D*
     D*   Date work arrays
     D*     o Date field arrays
     D*
     D*     o PDS input dates CYMD
     D*
     D*     o Output converted dates MM/DD/YYYY
     D*
     D*       RESERVES
     D*   Standard Types for type Codes used
     D RCD             S              2    DIM(10)                              Reserve code
     D RTP             S              1    DIM(10)                              Reserve type
     D*
     D*     o Claim Record Reserve Arrays
     D*
     D TI              S              9  2 DIM(10)                              Total Incurred
     D*
     D*     o OUTPUT Reserves by Reserve Type C,M,E,L
      *
      ***** BEG ADD ***** 02/27/04 ************************************
     D HX              S              1    DIM(50)                              HEX CNVT ARRY
     D*
     D*    Policy number assembly
     D                 DS
     D  XXPOL#                 1     18
     D  CMFND                  1      3  0
     D  CMEMP#                 4     12  0
     D  CMFYR                 13     15  0
     D*
     D*    Multi-claim case
     D                 DS
     D  KKCASE                 1     14
     D  XCCASE                 1     14  0
     D*
      ***** BEG ADD **** 05/30/02 ****************************************
     D*    Adjuster Phone Number disassembly
     D                 DS
     D  ##EPHN                 1     10  0
     D  UPPHNA                 1      3  0
     D  UPPHN#                 4     10  0
     D*    Adjuster FAX Number disassembly
     D                 DS
     D  ##EFAX                 1     10  0
     D  UPFAXA                 1      3  0
     D  UPFAX#                 4     10  0
      ***** END ADD **** 05/30/02 ****************************************
     D*
     D*    Zip code disassembly
     D***** BEG CHG ******* 03/10/04 **********************************
     D*********   DS  *********
     D                 DS                  INZ
     D***** END CHG ******* 03/10/04 **********************************
     D  CDZIP                  1      9  0
     D  IZIP1                  1      5  0
     D  IZIP2                  6      9  0
     D                 DS
     D  CLZIP                  1     10
     D  OZIP1                  1      5  0
     D  ODASH                  6      6
     D  OZIP2                  7     10  0
     D*
     D*    Input dates - from WMCMP/WMCDP/WMCHP
     D***** BEG CHG ******* 03/10/04 **********************************
     D*********   DS  *********
     D                 DS                  INZ
     D***** END CHG ******* 03/10/04 **********************************
     D  ID                     1     56  0
     D                                     DIM(8)
     D  CMACDT                 1      7  0
     D  CDDTOB                 8     14  0
     D  CMHIDT                15     21  0
     D  CMDOD                 22     28  0
     D  CMRCVD                29     35  0
     D  CMDTRP                36     42  0
     D  CHEFDT                43     49  0
     D  CMDTLW                50     56  0
     D*
     D*    8 and 10 digit, numeric and character
     D                 DS
     D  DN                     1      8
     D                                     DIM(8)                               8 digit num
     D  D8X                    1      8
     D  D8N                    1      8  0
     D                 DS
     D  DX                     1     10
     D                                     DIM(10)                              10 dig edited
     D  D10X                   1     10
     D  D10N                   1     10  0
     D*
     D*    Output dates - to WHCLP
     D                 DS
     D  OD                     1     80
     D                                     DIM(8)
     D  CLACDT                 1     10
     D  CLDTOB                11     20
     D  CLHIDT                21     30
     D  CLDOD                 31     40
     D  CLDTRP                41     50
     D  CLRPTD                51     60
     D  CLCLDT                61     70
     D  CLDTLW                71     80
     D*
     D*    ACT/LOSS codes
     D                 DS
     D  GTCF01                 1     10
     D  XXACT                  1      2
     D  XXLOSS                 3      4
     D*
     D*----------------------------------------------------------------
     D*     PDS Reserve arrays.
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)                              type Codes
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)                              PMT AGAINST RR
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)                              REMAINING
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D*
     D*    OUTPUT Record Reserve arrays--by type,
     D*       in the following order: C, M, E, L.
     D*        Paid
     D                 DS
     D  TAR                    1     64  2
     D                                     DIM(4)                               PAID
     D  CLPDCO                 1     16  2
     D  CLPDME                17     32  2
     D  CLPDLE                33     48  2
     D  CLPDLG                49     64  2
     D*        Remaining Reserve
     D                 DS
     D  TRR                    1     64  2
     D                                     DIM(4)                               REMAINING
     D  CLOSCO                 1     16  2
     D  CLOSME                17     32  2
     D  CLOSLE                33     48  2
     D  CLOSLG                49     64  2
     D*        Total Incurred
     D                 DS
     D  TTI                    1     64  2
     D                                     DIM(4)                               Total Incurred
     D  CLRUCO                 1     16  2
     D  CLRUME                17     32  2
     D  CLALAE                33     48  2
     D  CLINLG                49     64  2
     D*
     D* System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*----------------------------------------------------------------
     D*        TEXT
     D*----------------------------------------------------------------
     D*
     D NULL99          C                   CONST('999999999')
     D CLREAS          C                   CONST('CVT  . . ..  xxxxx-
     D                                     ctd,')
      ***** END ADD ***** 02/27/04 ************************************

     I*
     I*================================================================
     I*             DATA
     I*================================================================
     IPCM01         51
     I                                          CMCO#         L3
     I                                          CMFND         L2
     I                                          CMSS#         L1
     I*

     C*================================================================
     C*             MAIN PROCESSING
     C*================================================================
     C*   Got WMCMP record.
     C     *IN51         IFEQ      *ON
     C                   ADD       1             INREC             7 0
     C                   CLEAR                   PCL01
     C*
     C*----------------------------------------------------------------
     C*    Load Reserve type/code table if new company.
     C     *INL3         IFEQ      *ON
     C                   EXSR      TYPLOD
     C                   ENDIF                                                  .L2
     C*----------------------------------------------------------------
     C*    New Group -- get Master.
     C     *INL2         IFEQ      *ON
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PFN01
     C                   MOVE      'Y'           FNTEST                         Force bypass
     C*************        CLEARRCD                        Dont Clear if
     C*************        CLEARRTP                        No Valid Group
     C                   ENDIF
     C                   ENDIF
     C*
     C*         Bypass test groups.
     C* If not a Test Group, must have CiCenter Flag set to continue
     C     FNTEST        IFNE      'Y'
     C     FNCICF        ANDEQ     'Y'
     C*
     C*   Get other required files.
     C*----------------------------------------------------------------
     C*      WMCDP - CLAIMANT DEMOGRAPHICS - when SS# changes
     C     *INL1         IFEQ      *ON
     C     CMSS#         ANDNE     *BLANK
     C     CDKEY         CHAIN     PCD01                              57
     C                   ENDIF
     C     *IN57         IFEQ      *ON
     C                   CLEAR                   PCD01
     C                   ENDIF
      *
      ****** BEG ADD ***** 08/11/04 ************************************
      *  If GET hit on excluded Employer, then bypass the write of record
     C     XPKY          CHAIN     PXP01                              65
     C     *IN65         IFEQ      *ON
      ****** END ADD ***** 08/11/04 ************************************
     C*
     C*----------------------------------------------------------------
     C*  Check Claim Maint Date > Input Parm EXTRACT Date, BYPASS IF <=
     C     CMDT          IFLE      XXENDT
     C                   GOTO      NEXT
     C                   ENDIF
     C***** BEG ADD ******* 03/10/04 **********************************
     C*     Within requested time span.
     C     CMFYR         IFGE      @@STYR
     C***** END ADD ******* 03/10/04 **********************************
     C*
     C*----------------------------------------------------------------
     C*        CLAIM STATUS HISTORY WMCHP
     C     CHKY          SETGT     PCH01
     C     CHKY2         READPE    PCH01                                  56
     C     *IN56         IFEQ      *ON
     C                   CLEAR                   PCH01
     C                   CLEAR                   CHEFDT
     C                   ENDIF
     C*
     C*----------------------------------------------------------------
     C*        CLAIM LEGAL FILE  WMLLP
     C     CHKY2         CHAIN     PLL01                              58
     C     *IN58         IFEQ      *ON
     C                   CLEAR                   PLL01
     C                   ENDIF
     C*
     C*----------------------------------------------------------------
     C*   Convert all dates.
     C                   EXSR      CVTDAT
     C     CHOCST        IFNE      'C'
     C                   MOVE      *BLANKS       OD(7)                          Close date
     C                   ENDIF
     C*
     C*   Calculate Total Incurred Reserves
     C     AR            ADD       RR            TI
     C*
     C*----------------------------------------------------------------
     C*        WMCMP - CLAIM MASTER
     C*----------------------------------------------------------------
     C*
     C                   EXSR      BLDCMP
     C*
     C                   WRITE     PCL01
     C                   ADD       1             CLMCT             7 0
     C*
     C* Bypass Label if Claim Maint Date Less than or Equal Extact Date
     C     NEXT          TAG
     C*
     C***** BEG ADD ******* 03/10/04 **********************************
     C*     Within requested time span.
     C                   ENDIF
     C***** END ADD ******* 03/10/04 **********************************
      *
      ****** BEG ADD ***** 08/11/04 ************************************
     C                   ENDIF
      ****** END ADD ***** 08/11/04 ************************************
     C*
     C                   ENDIF                                                  .Not TESTgrp
     C                   ENDIF                                                  .IN51 ON
     C*
     C*================================================================
     C*  BLDCMP - Claim Master
     C*================================================================
     C     BLDCMP        BEGSR
     C*
     C*     Identifiers
     C**** Begin Add ****  07/22/05  **************************************
     C*
     C*     Policy number
     C     CLTID         IFEQ      '135'
     C     RPKEY         CHAIN     PRP01                              62
     C     *IN62         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANK
     C                   MOVEL(P)  RPCAP#        CLPONR
     C                   ELSE
     C                   MOVEL     XXPOL#        CLPONR
     C                   ENDIF                                                  e 62 OFF...&
      ***** BEG ADD ***** 09/09/05 ***************************************
     C                   ELSE
     C                   MOVEL     XXPOL#        CLPONR
      ***** END ADD ***** 09/09/05 ***************************************
     C                   ENDIF                                                  e CLTID=135
     C**** End Add ******  07/22/05  **************************************
     C**** Begin delete *  07/22/05  **************************************
     C*******************  MOVELXXPOL#    CLPONR
     C**** End delete ***  07/22/05  **************************************
     C                   MOVEL     CMCASE        CLCASE
     C                   MOVEL     CMCASE        CLICLM
     C*
     C*     Loss coverage - act & loss
     C     CMJRST        CAT       'LC'          XXTBID
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     CMLSCV        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *OFF          *IN60
     C                   MOVE      '  LC'        XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C                   ENDIF
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  XXACT         CLACT
     C                   MOVEL(P)  XXLOSS        CLLSCV
     C                   ENDIF
     C*
     C*     Catastrophe code
     C                   Z-ADD     CMCATA        CLCATA
     C*
     C*     Body part
      *
      ***** BEG ADD ***** 11/05/04 ********************************
      * Change to use PDS code from WMCMP instead of table EDI code
     C                   MOVEL     CMPART        CLPART
      ***** END ADD ***** 11/05/04 ********************************
      ***** BEG DEL ***** 11/05/04 ********************************
     C*********            MOVE *BLANKS   XXKEY
     C*********            MOVELCMPART    XXKEY
     C*********  TBKY      CHAINPPA02                60
     C*********  *IN60     IFEQ *OFF
     C*********            MOVELPAEDIP    CLPART
     C*********            ENDIF
      ***** END DEL ***** 11/05/04 ********************************
     C*
     C*     Cause code.
      *
      ***** BEG ADD ***** 11/05/04 ********************************
      * Change to use PDS code from WMCMP instead of table EDI code
     C                   MOVEL     CMCAUS        CLCAUS
      ***** END ADD ***** 11/05/04 ********************************
      ***** BEG DEL ***** 11/05/04 ********************************
     C**********           MOVE *BLANKS   XXKEY
     C**********           MOVELCMCAUS    XXKEY
     C********** TBKY      CHAINPCA01                60
     C********** *IN60     IFEQ *OFF
     C**********           MOVELCAEDIC    CLCAUS
     C**********           ENDIF
      ***** END DEL ***** 11/05/04 ********************************
     C*
     C*     Class code.
     C                   MOVEL(P)  CMCLAS        CLCLAS
     C*
     C*     Jurisdiction state.
     C                   MOVEL     CMJRST        CLJRST
     C*
     C*     Accident description.
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     CMACDS        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            CMACDS
      *
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     CMACD2        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            CMACD2
      *
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     CMACD3        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            CMACD3
      ***** END ADD ***** 02/27/04 ************************************
     C     CMACDS        CAT       CMACD2        CLACDS
     C     CLACDS        CAT       CMACD3        CLACDS
     C*
     C*     MCO type.
     C                   MOVEL     CMMCTP        CLMCTP
     C*
     C*     Nature code.
      *
      ***** BEG ADD ***** 11/05/04 ********************************
      * Change to use PDS code from WMCMP instead of table EDI code
     C                   MOVEL     CMNATR        CLNATR
      ***** END ADD ***** 11/05/04 ********************************
      ***** BEG DEL ***** 11/05/04 ********************************
     C**********           MOVE *BLANKS   XXKEY
     C**********           MOVELCMNATR    XXKEY
     C********** TBKY      CHAINPNA01                60
     C********** *IN60     IFEQ *OFF
     C**********           MOVELNAEDIN    CLNATR
     C**********           ENDIF
      ***** END DEL ***** 11/05/04 ********************************
     C*
     C*     LC recovery type.
     C                   MOVEL     CMLSRV        CLLSRV
     C*
     C*     LC coverage type.
     C                   MOVEL     CMCOVT        CLCOVT
     C*
     C     *IN58         IFEQ      *OFF
     C                   MOVEL(P)  LLMTST        CLLSST
     C                   ENDIF
     C*
     C*    Status
     C     CHOCST        IFEQ      'C'                                           Closed
     C                   Z-ADD     1             CLSTAT
     C                   ELSE
     C     CHKY2         READPE    PCH01                                  56
     C     *IN56         IFEQ      *OFF
     C                   Z-ADD     2             CLSTAT                          Reopened
     C                   ENDIF
     C                   ENDIF
     C*
     C*----------------------------------------------------------------
     C*     Reserve amounts.
     C*        Get Types for the reserve Codes on this claim.
     C                   DO        10            R                 2 0           type Codes
     C     RT(R)         IFNE      *BLANK
     C*
     C                   MOVE      *OFF          *IN43
     C                   Z-ADD     1             N                 2 0
     C     RT(R)         LOOKUP    RCD(N)                                 43
     C     *IN43         IFEQ      *ON
     C                   EXSR      ADDRES
     C                   ENDIF                                                  .IN43 ON
     C*
     C                   ENDIF                                                  .RT <> ' '
     C                   ENDDO                                                  .DO 10
     C*
     C*----------------------------------------------------------------
     C*     Paid Reserves - from payment history file
     C                   EXSR      ADDPAP
     C*
     C*----------------------------------------------------------------
     C*
     C*    NCCI Injury type
     C                   MOVEL     CMNCCI        CLINJT
     C*    Lump sum flag
     C                   MOVEL     LLLUMP        CLLUSF
     C     CLLUSF        IFEQ      *BLANK
     C                   MOVEL     'N'           CLLUSF
     C                   ENDIF                                                  .LUSF= ' '
     C*    Something
     C                   MOVEL(P)  CMOFF#        CLTEPD
     C*    Vocational Rehab
     C                   MOVEL     CMVORC        CLVORC
     C     CLVORC        IFEQ      *BLANK
     C                   MOVEL     'N'           CLVORC
     C                   ENDIF                                                  .LUSF= ' '
     C*
     C*----------------------------------------------------------------
     C*     Claimant Demographics Data - from WMCDP
     C     *IN57         IFEQ      *OFF
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters in alpha fields
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     CDLSN         HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            CDLSN
      *
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     CDFSN         HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            CDFSN
      *
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     CDADR1        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            CDADR1
      *
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     CDCTY         HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            CDCTY
      ***** END ADD ***** 02/27/04 ************************************
      *
     C                   MOVEL     CDLSN         CLLSN
     C                   MOVEL     CDFSN         CLFSN
     C                   MOVEL     CDMID         CLMID
     C                   MOVEL     CDADR1        CLADR1
     C                   MOVEL     CDADR2        CLADR2
     C                   MOVEL     CDCTY         CLCTY
     C                   MOVEL     CDST          CLST
     C                   Z-ADD     IZIP1         OZIP1
     C                   Z-ADD     IZIP2         OZIP2
     C                   MOVEL     '-'           ODASH
     C*** BEG ADD *** 10/20/03 ****************************
     C                   MOVEL     CDCTRY        CLCTRY
     C                   MOVEL     CDADR3        CLADR3
     C*** END ADD *** 10/20/03 ****************************
     C*
     C                   MOVEL     CDSEX         CLSEX
     C     CDMSTS        IFEQ      'M'
     C                   MOVEL     'Y'           CLMSTS                           Married
     C                   ELSE                                                   .
     C                   MOVEL     'N'           CLMSTS                           Not
     C                   ENDIF                                                  . MSTS = M
     C*
     C                   ENDIF                                                  . *IN57
     C*----------------------------------------------------------------
     C*
     C*    Adjustor data.
     C     CMCLEX        IFNE      *BLANK
     C     CMCLEX        CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PUP01
     C                   ELSE                                                   .*IN60
     C* ***  BEGIN ADD  ***  05/13/05 ***********************************
     C                   MOVEL     CMCLEX        TEST#             1            If has "#", clr
     C     TEST#         IFEQ      '#'
     C                   CLEAR                   CLEXID
     C                   CLEAR                   CLEXN
     C                   CLEAR                   CLEXA1
     C                   CLEAR                   CLEXA2
     C                   CLEAR                   CLEXCY
     C                   CLEAR                   CLEXST
     C                   CLEAR                   CLEXZP
     C                   CLEAR                   CLEPHN
     C                   CLEAR                   CLEFAX
     C                   CLEAR                   CLEMAL
     C                   CLEAR                   CLEXFE
     C                   ELSE
     C* ***  END   ADD  ***  05/13/05 ***********************************
     C                   MOVEL     UPUSNM        CLEXN
      ***** BEG ADD **** 05/30/02 ****************************************
     C                   MOVEL     CMCLEX        CLEXID                         Adjuster ID
     C                   MOVEL(P)  UPADR1        CLEXA1                         Addr line 1
     C                   MOVEL(P)  UPADR2        CLEXA2                         Addr line 2
     C                   MOVEL(P)  UPCTY         CLEXCY                         Addr line 1
     C                   MOVEL(P)  UPST          CLEXST                         Addr line 2
     C     UPZIP         IFGT      *ZEROS
     C                   MOVEL(P)  UPZIP         CLEXZP                         Adjuster ZIP
     C                   ENDIF
     C     ##EPHN        IFGT      *ZEROS
     C                   MOVEL(P)  ##EPHN        CLEPHN                         Adjuster Phone
     C                   ENDIF
     C     ##EFAX        IFGT      *ZEROS
     C                   MOVEL(P)  ##EFAX        CLEFAX                         Adjuster FAX #
     C                   ENDIF
     C                   MOVEL     UPMAIL        CLEMAL                         Adjuster Email
      ***** END ADD **** 05/30/02 ****************************************
     C     EXKY          CHAIN     PAL01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     ALLIC         CLEXFE
     C                   ENDIF                                                  .*IN60
     C                   ENDIF                                                  .*IN60
     C* ***  BEGIN ADD  ***  05/13/05 ***********************************
     C                   ENDIF                                                  .CLEX <> '#'
     C* ***  END   ADD  ***  05/13/05 ***********************************
     C                   ENDIF                                                  .CLEX <> ' '
     C*
     C*    SS#
     C                   MOVEL     CMSS#         CLSS#
     C*
     C*    Claim type
     C                   SELECT
     C     CMLTFL        WHENEQ    'I'
     C                   MOVEL     'IND'         CLTYPE                           Indemnity
     C     CMLTFL        WHENEQ    'M'
     C                   MOVEL     'MO'          CLTYPE                           Medical
     C     CMLTFL        WHENEQ    'R'
     C                   MOVEL     'RO'          CLTYPE                           Report
     C                   ENDSL
     C*
     C*      Occupation description
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters in alpha fields
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     CMOCCD        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            CMOCCD
      *
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     CMOCDS        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            CMOCDS
      ***** END ADD ***** 02/27/04 ************************************
     C     CMOCCD        CAT       CMOCDS:1      CLOCDS
     C*
     C*    Litigation flag
     C                   MOVEL     LLLITG        CLLITF
     C     CLLITF        IFEQ      *BLANK
     C                   MOVEL     'N'           CLLITF
     C                   ENDIF
     C*
     C*      Department, Div, Loc, Co
     C                   MOVEL     CMDEPT        CLDEPT
     C                   Z-ADD     CMDIV         CLDIV
     C                   MOVEL     CMCTG1        CLLOC
     C                   Z-ADD     CMCO#         CLCO#
     C*
     C*      Flags
     C                   MOVEL     CMCVFL        CLCVFL
     C     CLCVFL        IFEQ      *BLANK
     C                   MOVEL     'N'           CLCVFL
     C                   ENDIF
     C                   MOVEL     CMSUBR        CLSUBR
     C     CLSUBR        IFEQ      *BLANK
     C                   MOVEL     'N'           CLSUBR
     C                   ENDIF
     C                   MOVEL     LLFRAU        CLFRAU
     C                   MOVEL     CMDNY         CLDNY
     C     CLDNY         IFEQ      *BLANK
     C                   MOVEL     'N'           CLDNY
     C                   ENDIF
     C*
     C*    Multiple claim #.
     C                   Z-ADD     CMCASE        XCCASE
     C     MCKY          CHAIN     PMC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     MCCLM#        CLMULT
     C                   ENDIF                                                  .*IN60
     C*
     C*    Average weekly wage.
     C                   Z-ADD     CMAWW         CLAWW
     C*
     C*    Supervisor name
     C     CMSUPR        IFNE      *BLANK
     C     CMSUPR        CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     UPUSNM        CLSUN
     C                   ENDIF                                                  .*IN60
      *
      ***** BEG DEL ***** 01/05/04 ***********************************
      *      As of date
     C**********           MOVEL*BLANKS   D10X
     C**********           TIME           DSDTTM
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY2
     C********** SRERR     IFEQ '0'
     C**********           MOVELSRDTO     D8N
     C**********           EXSR EDDAT
     C**********           ENDIF
     C**********           MOVE D10X      CLAODT
      ***** END DEL ***** 01/05/04 ***********************************
      *
     C                   ENDIF                                                  .SUPR <> ' '
     C*
     C*----------------------------------------------------------------
      *
      *  Get Class Description
      *
     C     EPKY          SETGT     PEP01
     C     EPKY1         READPE    PEP01                                  60
     C  N60ELKY          CHAIN     PEL01                              60
     C  N60RSKY          CHAIN     PRS01                              61
     C  N61PXKY          CHAIN     PPX01                              61
      *
     C     *IN60         IFEQ      *OFF
     C     *IN61         ANDEQ     *OFF
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     PXDESC        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            PXDESC
      ***** END ADD ***** 02/27/04 ************************************
     C                   MOVEL(P)  PXDESC        CLCLDS
      *
     C                   ELSE
      *
     C     *IN60         IFEQ      *OFF
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters
     C     *IN60         IFEQ      *OFF
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     PRDESC        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            PRDESC
     C                   ENDIF
      ***** END ADD ***** 02/27/04 ************************************
     C  N60              MOVEL(P)  PRDESC        CLCLDS
     C                   ENDIF
      *
     C                   ENDIF
      *
      ***** BEG ADD ***** 01/05/04 ***********************************
      *      As of date
     C                   MOVEL     *BLANKS       D10X
     C                   TIME                    DSDTTM
      *
      ***** BEG CHG ***** 03/25/04 ************************************
     C*************        Z-ADDDSDATE    SRDTI
     C                   MOVEL     @@JBDT        ZZRNDT            8 0
     C                   Z-ADD     ZZRNDT        SRDTI
      ***** END CHG ***** 03/25/04 ************************************
     C                   CALL      'SRCVTDAT'    DTMDY2
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         D8N
     C                   EXSR      EDDAT
      ***** BEG ADD ***** 03/25/04 ************************************
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         D8N
     C                   EXSR      EDDAT
      ***** END ADD ***** 03/25/04 ************************************
     C                   ENDIF
     C                   MOVE      D10X          CLAODT
      ***** END ADD ***** 01/05/04 ***********************************
      *
     C                   ENDSR                                                  BLDCMP
      *
     C*================================================================
     C*      ADDRES  Accumulate Reserves by Reserve Type
     C*================================================================
     C     ADDRES        BEGSR
     C                   Z-ADD     3             T                 2 0          DEFAULT 'E'
     C                   SELECT
     C     RTP(N)        WHENEQ    'C'
     C                   Z-ADD     1             T
     C     RTP(N)        WHENEQ    'M'
     C                   Z-ADD     2             T
     C     RTP(N)        WHENEQ    'E'
     C                   Z-ADD     3             T
     C     RTP(N)        WHENEQ    'L'
     C                   Z-ADD     4             T
     C                   ENDSL
     C*
     C                   ADD       AR(R)         TAR(T)
     C                   ADD       RR(R)         TRR(T)
     C                   ADD       TI(R)         TTI(T)
     C*
     C                   ENDSR
     C*================================================================
     C*      ADDPAP  Accumulate Payment Totals
     C*================================================================
     C     ADDPAP        BEGSR
     C*
     C                   Z-ADD     0             XDLOST
     C*
     C*     Read all the payments for this claim.
     C     PAKY          SETLL     PPA01
      ***** BEG CHG ***** 04/22/02 ************************************
     C*************        READ PPA01                    55
     C     PAKY          READE     PPA01                                  55
      ***** END CHG ***** 04/22/02 ************************************
     C     *IN55         DOWEQ     *OFF
     C*
     C********   PATRDT    IFLE XXENDT                     Parm Date
     C     PATRDT        IFLE      XXDT                                         Run Date
     C*
     C*----------------------------------------------------------------
     C*     Accumulate Comp Lost Time.
     C     PARETP        IFEQ      'C'                                          Compensation
     C                   Z-ADD     0             XXLOST
     C*
     C*    Calculate days in weeks
     C     PAWKS         IFNE      *ZERO
     C     CMDAYS        IFNE      *ZERO
     C     PAWKS         MULT      CMDAYS        XXLOST
     C                   ELSE
     C     PAWKS         MULT      7             XXLOST
     C                   ENDIF                                                   CMDAYS ne 0
     C                   ENDIF                                                   PAWKS ne 0
     C*
     C*    Add days
     C                   ADD       PADAYS        XXLOST
     C*
     C*    Post according to Transaction
     C     PATYPE        IFEQ      'A'
     C     PAAMT         ANDGT     *ZERO
     C     PATYPE        OREQ      'P'
     C     PAAMT         ANDGT     *ZERO
     C     PATYPE        OREQ      'M'
     C     PAAMT         ANDGT     *ZERO
     C     PATYPE        OREQ      'Z'
     C     PAAMT         ANDGT     *ZERO
     C*** BEG ADD *** 07/17/03 **************************************
     C     PATYPE        OREQ      'R'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'S'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'V'
     C     PAAMT         ANDLT     0
     C*** END ADD *** 07/17/03 **************************************
     C                   ADD       XXLOST        XDLOST
     C*
     C                   ELSE                                                   x TYPE = ...
     C*
     C                   SUB       XXLOST        XDLOST
     C*
     C                   ENDIF                                                  PATYPE = ...
     C*
     C                   ENDIF                                                  RETP = C
      *
      ***** BEG CHG ***** 01/05/04 ***********************************
      * TEMP FIX to override Negative days greater than -999
     C**************       Z-ADDXDLOST    CLLOST
     C     XDLOST        IFLT      -999
     C                   Z-ADD     -999          CLLOST
     C                   ELSE
     C                   Z-ADD     XDLOST        CLLOST
     C                   ENDIF
      ***** END CHG ***** 01/05/04 ***********************************
     C*
     C*----------------------------------------------------------------
     C*      Process Recoveries
     C*----------------------------------------------------------------
     C     PATYPE        IFEQ      'R'
     C*
     C*   By Reserve Type - Indemnity
     C     PARETP        IFEQ      'C'
     C*
     C*   Determine if Deductible
     C     PATRCD        IFEQ      FNCOTR
     C     PABR2         ANDEQ     FNCOP1
     C                   ADD       PAAMT         CLDRIN
     C                   ELSE                                                   <TR-PV1vsFNP
     C                   ADD       PAAMT         CLRVCO
     C                   ENDIF                                                  .TR-PV1vsFNP
     C                   ENDIF                                                  .RETP = C
     C*
     C*   By Reserve Type - Medical
     C     PARETP        IFEQ      'M'
     C*
     C*   Determine if Deductible
     C     PATRCD        IFEQ      FNMETR
     C     PABR2         ANDEQ     FNMEP1
     C                   ADD       PAAMT         CLDEDR
     C                   ELSE                                                   <TR-PV1vsFNP
     C                   ADD       PAAMT         CLRVME
     C                   ENDIF                                                  .TR-PV1vsFNP
     C                   ENDIF                                                  .RETP = M
     C*
     C*   By Reserve Type - Expense
     C     PARETP        IFEQ      'E'
     C*
     C*   Determine if Deductible
     C     PATRCD        IFEQ      FNEXTR
     C     PABR2         ANDEQ     FNEXP1
     C                   ELSE                                                   <TR-PV1vsFNP
     C                   ADD       PAAMT         CLRVLE
     C                   ENDIF                                                  .TR-PV1vsFNP
     C                   ENDIF                                                  .RETP = E
     C*
     C*   By Reserve Type - Legal
     C     PARETP        IFEQ      'L'
     C*
     C*   Determine if Deductible
     C     PATRCD        IFEQ      FNLETR
     C     PABR2         ANDEQ     FNLEP1
     C                   ELSE                                                   <TR-PV1vsFNP
     C                   ADD       PAAMT         CLRVLG
     C                   ENDIF                                                  .TR-PV1vsFNP
     C                   ENDIF                                                  .RETP = L
     C*
     C                   ENDIF                                                  PATYPE = R
     C*----------------------------------------------------------------
     C                   ENDIF                                                  .TR <= ENDT
     C*
     C     PAKY          READE     PPA01                                  55
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C*      TYPLOD     Load Reserve type tables RCD & RTP
     C*================================================================
     C     TYPLOD        BEGSR
     C                   CLEAR                   RCD
     C                   CLEAR                   RTP
     C                   MOVE      *BLANKS       XXKEY
     C                   Z-ADD     1             RE                2 0
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         DOWEQ     *OFF
     C     RE            ANDLE     10
     C                   MOVEL     RTKEY         RCD(RE)
     C                   MOVEL     RTTYPE        RTP(RE)
     C                   ADD       1             RE
     C     RTKY          READE     PRT01                                  60
     C                   ENDDO                                                  .DOW IN60
     C*
     C                   ENDSR
     C*================================================================
     C*      CVTDAT     Convert CYMD dates to MMDDYYYY & edit.
     C*                 Dates from array ID; to array OD.
     C*================================================================
     C     CVTDAT        BEGSR
     C*
     C                   DO        8             D                 2 0
     C                   MOVEL     *BLANKS       D10X
     C                   Z-ADD     ID(D)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         D8N
     C                   EXSR      EDDAT
     C                   ENDIF
     C                   MOVEA     D10X          OD(D)
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C*      EDDAT      EDIT DATE  8 digit to MM/DD/YYYY
     C*================================================================
     C     EDDAT         BEGSR
     C*
     C                   Z-ADD     1             W                 2 0
     C                   DO        10            X                 2 0
     C*
     C     X             IFEQ      3
     C     X             OREQ      6
     C                   MOVEL     '/'           DX(X)
     C                   ELSE
     C                   MOVEL     DN(W)         DX(X)
     C                   ADD       1             W
     C                   ENDIF                                                  .X=3/6
     C*
     C                   ENDDO                                                  .DO 10
     C*
     C                   ENDSR
     C*
     C*================================================================
     C*  ERRSR  - Error processing
     C*================================================================
     C     ERRSR         BEGSR
     C*
     C*      Print
     C                   ADD       1             ERRCT             7 0
     C*
     C                   ENDSR
     C*================================================================
     C*  *INZSR - Initialization
     C*================================================================
     C     *INZSR        BEGSR
     C*
     C*----------------------------------------------------------
     C*       PARAMETER LISTS
     C*----------------------------------------------------------
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDATE            8            mmddccyy
      ***** BEG ADD ***** 03/25/04 ************************************
     C                   PARM                    @@JBDT            8
      ***** END ADD ***** 03/25/04 ************************************
     C***** BEG ADD ******* 03/10/04 **********************************
     C                   PARM                    @@STYR            3 0
     C***** END ADD ******* 03/10/04 **********************************
     C*
     C*----------------------------------------------------------
     C*          DATE PARAMETERS
     C*----------------------------------------------------------
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDY'        SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*----------------------------------------------------------
     C*          KEY LISTS
     C*----------------------------------------------------------
     C*
     C*  Key for Claimant Demographics -- WMCDL
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C     GTKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*    WTABL - Work Comp Table File
     C     TBKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    XXKEY
     C*
     C     TBKY2         KLIST
     C                   KFLD                    CMCO#
     C*
     C*    WTRTP - Reserve Types for Reserve Codes
     C     RTKY          KLIST
     C                   KFLD                    CMCO#
     C*
     C*    SMALP - Adjustor License File
     C     EXKY          KLIST
     C                   KFLD                    UPUSR
     C                   KFLD                    CMJRST
     C*
     C*    WAMCL - Multi-claim file
     C     MCKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    KKCASE
     C*
     C*    WMCHL - Work Comp Status File
     C     CHKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    XXDT
     C*
     C     CHKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*    WDPAL - Work Comp Payments
     C     PAKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*    WMLLL - Work Comp Legal File
     C     LLKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*    WMFNL - Group Master
     C     FNKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
      *
      *    Employer Payroll
      *
     C     EPKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMCLAS
     C                   KFLD                    CMSPEC
     C***********          KFLD           XXENDT           Extract Date
     C                   KFLD                    XXDT                           Run date
      *
     C     EPKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMCLAS
     C                   KFLD                    CMSPEC
      *
      *    Employer Policy Year
      *
     C     ELKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
      *
      *    Rate Set Code Table
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      *
      *    Payroll Extension Description
      *
     C     PXKY          KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    RSJRST
      *
      *    Payroll Classification
      *
     C     PRKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEFDT
      *
     C     PRKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    EPCLAS
      *
      ****** BEG ADD ***** 08/11/04 ************************************
      *
      *  Employer Excluded file
      *
     C     XPKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
      ****** END ADD ***** 08/11/04 ************************************
     C**** Begin Add ****  07/22/05  **************************************
     C     RPKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C**** End Add ******  07/22/05  **************************************
     C*
     C*----------------------------------------------------------
     C*      DATA DEFINITIONS
     C*----------------------------------------------------------
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C*
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMACDT        XXACDT
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C*
     C     *LIKE         DEFINE    CMLOST        XXLOST
     C     *LIKE         DEFINE    CMLOST        XDLOST
     C*
     C                   MOVEL     *BLANK        WKNAME           25
     C*----------------------------------------------------------
     C*      PRE PROCESS
     C*----------------------------------------------------------
     C*
     C*     Calculate julian system date -- C YY MM DD
     C     *LIKE         DEFINE    CMDT          XXDT
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C*     Calculate julian ending date -- C YY MM DD
     C     *LIKE         DEFINE    CMDT          XXENDT
     C*
     C                   MOVE      QQDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXENDT
     C*
     C     XXENDT        IFEQ      *ZERO
     C                   Z-ADD     XXDT          XXENDT
     C                   ENDIF
     C*
     C                   Z-ADD     1             Z                 7 0
     C*
     C                   ENDSR
     C* ==================================================================== *
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      *================================================================
      * SRHEX - Check Alpha input field for Invald Hex characters
      *         The Alpha field is loaded into the arrary w/MOVEA
      *         then Loop thru array & replace w/blank if out of
      *         is not a valid HEX Character. After exiting subr
      *         the array is moved back to the alpha field w/MOVEA
      *================================================================
     C     SRHEX         BEGSR
      *
     C                   DO        50            K                 3 0
      *
     C     HX(K)         IFLT      X'40'
     C     HX(K)         OREQ      X'41'
     C     HX(K)         ORGT      X'F9'
     C                   MOVE      X'40'         HX(K)
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   ENDSR                                                  SROUT
      *================================================================
      ***** END ADD ***** 02/27/04 ************************************
