     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE N96DMR - Type DM Discount Code Calculations
     H*****************************************************************
     H* This pgm is called by N9600R to calcualte the network discount*
     H* amt based on the providers discount code. The discount will   *
     H* only be calculated for the signle sequence number passed to   *
     H* this program.                                                 *
     H*                                                               *
     H*  Called from:                                                 *
     H*    N9600R                                                     *
     H*                                                               *
     H*****************************************************************
     H* Created on 04/09/2007 by c:62 for project 25MOD               *
     H*****************************************************************
     H* Revisions:
     H*
     H* 07/18/23   C:342 Project: NY060                               *
     H*                  NY Compliance: CARC/RARC Update              *
     H*
     H* 02/15/16   C:079 Support 25/388986                            *
     H*                  Get locality using 9 digit zip code          *
     H*
     H* 05/28/15   C:212 Support PHW 32016                            *
     H*                  System not paying alliance rate in some case,*
     H*                  fixed that                                   *
     H*
     H* 01/21/15   C:079 Support: MC025 32840                         *
     H*                  Change override scope                        *
     H*
     H* 08/04/14   C:280 Support: 86312                               *
     H*                  add code for p12
     H*
     H* 02/09/13   C:079 Project: TX080                               *
     H*                  For 1/1/13 and greater use fully implemented *
     H*                  Practice expense RVU's                       *
     H*
     H* 07/08/13   C:295 Support: 81116 MC025                         *
     H*                  Provider Bill Detail issue                   *
     H*                                                               *
     H* 08/29/12   C:277 Support: 80361 MC025                         *
     H*                  Don't assign EOB code 45 for older than 100D *
     H*                                                               *
     H* 07/03/12   C:079 Support: 80223 MC025                         *
     H*                  Back out part of the change for 79419 to fix *
     H*                  HPSA overpayment and 95 day rule             *
     H*                                                               *
     H* 05/14/12   C:251 Support: 79419 MC025                         *
     H*                  Changed HPSA to not pay when hitting 95 day  *
     H*                  denial rule.                                 *
     H*                                                               *
     H* 07/20/11   C:198 Support: 77345                               *
     H*                  Changed XXAMT1/2/3 to 11,4 from 11,3.        *
     H*                                                               *
     H* 07/19/11   C:198 Support: 77344                               *
     H*                  fixed issue where efdt from NMBDP was not    *
     H*                  found in NDCOP.                              *
     H*                                                               *
     H* 03/07/11   C:198 Support: 76103                               *
     H*                  Made POS to pay non-facility.                *
     H*                                                               *
     H* 12/02/10    C:227    Project: MC351
     H*                      Flat fee to Calculate Additional Units
     H*
     H* 09/09/10    C:198    Support: 73884
     H*                      Code would be verified whether it is from clinical
     H*                      lab or not from FTLBP.
     H*
     H* 07/13/10    C:198    Project: 144MBS Priss 10.
     H*                      Incorporated alliance processing for anesthesia
     H*                      codes.
     H*
     H* 07/01/10    C:198    Project: 144MBS Priss 10.
     H*                      Fixed the where GPCI's have been changed
     H*                      resulting into change of FS allowance.
     H*
     H* 06/29/10    C:198    Support:73024
     H*                      Fixed the issue where error messages
     H*                      were getting removed.
     H*
     H* 06/04/10    C:198    Support:73024
     H*                      Modified to remove the 97750 to behave as
     H*                      commission code.
     H*
     H* 02/26/10    C:198    Project:MC337
     H*                      Modified to improve Alliance n/w contract bill
     H*                      processing.
     H*
     H* 01/12/10    C:079    SUPPORT 71281
     H*                      Use new Medicare and TDI rates for TX064  plied
     H*
     H* 03/24/09    C:079    SUPPORT 67785
     H*                      Use new Medicare and TDI rates for TX055  plied
     H*
     H* 12/22/08    C:198    SUPPORT 65839
     H*                      Medicare drug update codes are still multiplied
     H*                      with 1.25.
     H*
     H* 09/10/08    C:062    SUPPORT 65107
     H*                      Correct DIV by zero issue.
     H*
     H* 09/08/08    C:062    PROJECT 25MPA
     H*                      Add Modifier Processing.
     H*
     H* 07/07/08    C:198    SUPPORT 63919
     H*                      Included functionality to retain messages
     H*                      and EOB code when EOB code is not W1 and
     H*                      delete it otherwise.
     H*
     H* 04/11/08    C:198    SUPPORT 63008
     H*                      Corrected alliance n/w to incorporate
     H*                      new Texas multipliers.
     H*
     H* 02/12/08-B  C:180    SUPPORT 62100
     H*                      Use correct EOB code description.
     H*
     H* 02/06/08 C:191 PROJECT: 25MOY
     H*               OUTPUT CHANGES ON EOB FOR ALLIANCE NETWORK
     H*
     H* 01/15/08 C:62 PROJECT MC025 61683
     H*               ALLOW FOR POSITIVE ADJUSTMENTS
     H*
     H* 12/07/07 C:79 SUPPORT 60398
     H*               ADJUST LINE ITEMS THAT ARE HPSA ELIGIBLE
     H*
     H* 10/10/07 C:62 PROJECT 60505
     H*               HALF ADJUST
     H*
     H*****************************************************************
     FNMBDLD    IF   E           K DISK
     FFDCALF    UF   E           K DISK    INFDS(DBFDS)
     FNDCOL3    IF   E           K DISK
     F*** Beg Chg *** 02/26/10 ****************************************
     F*******FTABL   IF  E           K        DISK
     FFTABL     IF   E           K DISK    USROPN
     F*** End Chg *** 02/26/10 ****************************************
     FFAEOLG    IF   E           K DISK
     FFDDEL3    UF A E           K DISK
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     F*** BEGIN CHG *** 09/08/08 **************************************
     F**********FDMGL6  UF  E           K        DISK
     F*** Beg Chg *** 06/29/10 ****************************************
     F*****FDMGL6  UF  E           K        DISK                      A
     FFDMGL6    O    E             DISK
     F*** End Chg *** 06/29/10 ****************************************
     F*** END   CHG *** 09/08/08 **************************************
     FFDDHL     IF   E           K DISK
      ****** BEGIN ADD **************  02/12/08-B *********************
     FFTEBLTX   IF   E           K DISK
     F                                     RENAME(PEB01:PEB01TX)
      ****** END   ADD **************  02/12/08-B *********************
     F*** BEG ADD *** 12/07/07 ****************************************
     FFACAL1    IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     F*** BEG ADD *** 12/07/07 ****************************************
     F*** Beg Add *** 07/01/10 ****************************************
     FFTRVL1    IF   E           K DISK
     FFTGPL3    IF   E           K DISK
     FFMZRL     IF   E           K DISK    USROPN
     F*** End Add *** 07/01/10 ****************************************
     F*** Beg Add *** 02/26/10 ****************************************
     FFTRTL2    IF   E           K DISK
     F*** Beg Add *** 09/09/10 ****************************************
     FFTLBL1    IF   E           K DISK
     F*** End Add *** 09/09/10 ****************************************
     F*** Beg Add *** 05/14/12 ****************************************
     FFDFRL0    IF   E           K DISK
     F*** End Add *** 05/14/12 ****************************************
     D*** Beg Add *** 02/15/16 ********************************************
     D*===================================================================
     D* prototype definitions
     D*===================================================================
     D/copy FPRZIPLOC
     D*** End Add *** 02/15/16 ********************************************
     D*
     D*** Beg Chg *** 07/01/10 ****************************************
     D*** Beg Chg *** 01/21/15 ****************************************
     D*********           QCA     1   2 56
     D**** QCA             S             56    DIM(4) CTDATA PERRCD(1)
     D QCA             S             61    DIM(4) CTDATA PERRCD(1)
     D*** End Chg *** 01/21/15 ****************************************
     D*** End Chg *** 07/01/10 ****************************************
     D*** End Add *** 02/26/10 ****************************************
     D*** Beg Add *** 07/01/10 ****************************************
     D MOD             S              2    DIM(4)
     D*** END ADD *** 12/07/07 ****************************************
     D*** Beg Add *** 02/15/16 ********************************************
     D xxloc           S              2  0
     D*** End Add *** 02/15/16 ********************************************
     D*================================================================
     D* Data Structures
     D*================================================================
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*** Beg Add *** 02/26/10 ****************************************
     D                 DS
     D*** Begin Chg *** 01/21/15 **************************************
     D**QQCMD                  1     56
     D  QQCMD                  1     61
     D*** End   Chg *** 01/21/15 **************************************
     D  QQST                  40     41
     D*** End Add *** 02/26/10 ****************************************
     D*================================================================
     D* Constants
     D*================================================================
     D********** BEGIN CHANGE ********   02/12/08-B *******************
     D***           'Network Discount Ap- C         XXNOT1
     D***           'plied'
     D***           'Charges exceed your- C         XXNOT2
     D***           ' contracted/legisla-
     D***           'ted fee arrangement.'
     D XXNOTC          C                   CONST('Network Discount Ap-
     D                                     plied')

     I*** End Add *** 07/01/10 ****************************************
     I*
     IPCA02
     I              CACO#                       XRCO#
     I              CASUB#                      XRSUB#
     I              CAFND                       XRFND
     I              CAFYR                       XRFYR
     I              CACASE                      XRCASE
     I              CACSCD                      XRCSCD
     I              CAFDAT                      XRFDAT
     I              CATDAT                      XRTDAT
     I              CARDAT                      XRRDAT
     I              CAIPOP                      XRIPOP
     I              CAFEIN                      XRFEIN
     I              CAMOD                       XRMOD
     I              CAPRTY                      XRPRTY
     I              CAPRT                       XRPRT
     I              CATRCD                      XRTRCD
     I              CASVCD                      XRSVCD
     I              CAPMOD                      XRPMOD
     I              CACPT4                      XRCPT4
     I              CAICD9                      XRICD9
     I              CAANMN                      XRANMN
     I              CAANES                      XRANES
     I              CAUNTS                      XRUNTS
     I              CATOTC                      XRTOTC
     I              CATOTA                      XRTOTA
     I              CATOTD                      XRTOTD
     I              CAEOMB                      XREOMB
     I              CATOTP                      XRTOTP
     I              CADSEQ                      XRDSEQ
     I              CADELF                      XRDELF
     I              CAFLAG                      XRFLAG
     I              CACHK#                      XRCHK#
     I              CAEOB#                      XREOB#
     I              CAPART                      XRPART
     I              CAP1                        XRP1
     I              CASS#                       XRSS#
     I              CAACDT                      XRACDT
     I              CASTAT                      XRSTAT
     I              CAUSER                      XRUSER
     I              CAPGM                       XRPGM
     I              CATIME                      XRTIME
     I              CADT                        XRDT
     I              CAIUSR                      XRIUSR
     I              CAIDT                       XRIDT
     I              CADXR                       XRDXR
     I              CADCS#                      XRDCS#
     I              CALTR#                      XRLTR#
     I              CARCFL                      XRRCFL
     I              CAOTHR                      XROTHR
     I              CANDIS                      XRNDIS
     I              CARDTP                      XRRDTP
     I              CABRSV                      XRBRSV
     I              CABRPM                      XRBRPM
     I              CABRPC                      XRBRPC
     I              CABRAM                      XRBRAM
     I              CABRUN                      XRBRUN
     I              CABRTC                      XRBRTC
     I              CAZIPA                      XRZIPA
     I              CAAST                       XRAST
     I              CADIFF                      XRDIFF
     I              CABRPT                      XRBRPT
     I              CABAST                      XRBAST
     I              CADAFL                      XRDAFL
     I              CAMRA                       XRMRA
     I              CABREC                      XRBREC
     I              CABRP                       XRBRP
     I              CATYPE                      XRTYPE
     I              CARPT                       XRRPT
     I              CAFSR                       XRFSR
     I              CAFACF                      XRFACF
     I              CAPLAC                      XRPLAC
     I              CANDC                       XRNDC
     I              CAMOD1                      XRMOD1
     I              CAPROF                      XRPROF
     I              CABFHE                      XRBFHE
     I********** END CHANGE **********   02/12/08-B *******************
     I***** Beg Add *** 05/28/15 **************************************
     IPCA01
     I              CAUNCOMP                    CAUNCO
     IPRT01
     I              RTLIPPOW                    RTPPOW
     I***** End Add *** 05/28/15 **************************************

     C*================================================================
     C* Parameter Lists
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@FEIN            9
     C                   PARM                    @@MOD             3
     C                   PARM                    @@FDAT            8
     C                   PARM                    @@DSEQ            5
     C                   PARM                    @@CODE
     C                   PARM                    @@CEDT            8
     C                   PARM                    @@SYS             1
     C                   PARM                    @@FL1             1
     C                   PARM                    @@FL2             1
     C                   PARM                    @@FL3             1
     C                   PARM                    @@FL4             1
     C                   PARM                    @@FL5             1
     C                   PARM                    @@DS1            10
     C                   PARM                    @@DS2            10
     C                   PARM                    @@DS3            10
     C                   PARM                    @@DS4            10
     C                   PARM                    @@DS5            10
     C                   PARM                    @@RTCD            1
     C*** Beg Add *** 02/26/10 *******************************************
     C                   PARM                    @@MODE            1
     C                   PARM                    @@ALOW
     C*** End Add *** 02/26/10 *******************************************
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
     C*** Beg Add *** 07/13/10 ****************************************
     C     ANESPL        PLIST
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CALL            1
     C                   PARM      *BLANKS       @@EOB             3
     C                   PARM      *BLANKS       @@MSG             7
     C                   PARM                    @@MODR            1
     C                   PARM                    @@RND             1
     C                   PARM                    @@ALW1            9 2
     C                   PARM                    @@ALW2            9 2
     C                   PARM                    @@ALW3            9 2
     C                   PARM                    @@RTCD            1
     C*** End Add *** 07/13/10 ****************************************
     C*================================================================
     C* Key Lists
     C*================================================================
     C*
     C* Billing code table file
     C*
     C     BDKY1         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKEFDT
     C*
     C     BDKY2         KLIST
     C                   KFLD                    KKCODE
     C*
     C* Case detail file
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKDSEQ
     C*** BEG ADD *** 12/07/07 ****************************************
     C*
     C* Case detail file
     C*
     C     CAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*** END ADD *** 12/07/07 ****************************************
     C*
     C* Carve-out transaction file
     C*
     C     COKY1         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKEFDT
     C                   KFLD                    KK10A1
     C*
     C     COKY2         KLIST
     C                   KFLD                    KKCODE
     C*** Beg Del *** 07/19/11 ***********************************
     C*******              KFLD           KKEFDT
     C*** End Del *** 07/19/11 ***********************************
     C*
     C* Physician detail file.
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
     C* Eomb Header File
     C*
     C     EOKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C*
     C* Claim status code table file
     C*
     C     TCSKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    TABKEY           10
     C*
     C* Eomb detail eob/notes file.
     C*
     C     DEKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    EORCN
     C*
     C     DEKY1         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C                   KFLD                    KKSEQ#
     C*
     C     DEKY2         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*
     C* Message file
     C*
     C     MGKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    EORCN
     C                   KFLD                    CADSEQ
      ******** BEGIN ADD ************ 02/12/08-B **********************
     C* EOMB Descriptions table file
     C*
     C     TEBKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    TEBKEY           10
      ******** END ADD ************** 02/12/08-B **********************
     C*** Beg Add *** 02/26/10 ****************************************
     C     TBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C     RTKY1         KLIST
     C                   KFLD                    QQST
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKEFDT
     C*
     C     RTKY2         KLIST
     C                   KFLD                    QQST
     C                   KFLD                    KKTYPE
     C*** End Add *** 02/26/10 ****************************************
     C*** Beg Add *** 07/01/10 ****************************************
     C     GPKY1         KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKLOC
     C                   KFLD                    KKEFDT
     C*
     C     GPKY2         KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKLOC
     C*
     C     RVKY1         KLIST
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEFDT
     C*
     C     RVKY2         KLIST
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKMOD
     C*
     C     ZRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFZCD
     C                   KFLD                    KKEFDT
     C*
     C*** End Add *** 07/01/10 ****************************************
     C*** Beg Add *** 09/09/10 ****************************************
     C     LBKY1         KLIST
     C                   KFLD                    CACPT4
     C                   KFLD                    XXMOD             2
     C                   KFLD                    CAFDAT
     C*
     C     LBKY2         KLIST
     C                   KFLD                    CACPT4
     C                   KFLD                    XXMOD
     C*
     C*** End Add *** 09/09/10 ****************************************
     C*** Beg Add *** 05/14/12 ****************************************
     C     FRKY1         KLIST
     C                   KFLD                    CAAST
     C                   KFLD                    CARULE
     C                   KFLD                    CAFDAT
     C*
     C     FRKY2         KLIST
     C                   KFLD                    CAAST
     C                   KFLD                    CARULE
     C*** End Add *** 05/14/12 ****************************************
     C*================================================================
     C* Like Defines
     C*================================================================
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    CAEOB#        KKEOB#
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    BDCODE        @@CODE
     C     *LIKE         DEFINE    BDCODE        KKCODE
     C     *LIKE         DEFINE    BDEFDT        KKEFDT
     C     *LIKE         DEFINE    CO10A1        KK10A1
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C     *LIKE         DEFINE    CAMRA         HDMRA
     C     *LIKE         DEFINE    CAMRA         HDPROF
     C     *LIKE         DEFINE    CAMRA         HDTECH
     C**** Beg Chg *** 07/18/23 **************************************
     C**** *LIKE         DEFINE    DEEOB         XXEOB
     C     *LIKE         DEFINE    DEEOBCD       XXEOB
     C**** End Chg *** 07/18/23 **************************************
     C*** Beg Add *** 02/26/10 ****************************************
     C     *LIKE         DEFINE    XXMRAB        SVMRAB
     C     *LIKE         DEFINE    XXMRAB        XMMRAB
     C     *LIKE         DEFINE    XXMRAB        @@ALOW
     C     *LIKE         DEFINE    DHMOD1        SVMOD1
     C     *LIKE         DEFINE    DHMOD2        SVMOD2
     C     *LIKE         DEFINE    DHMOD3        SVMOD3
     C     *LIKE         DEFINE    DHMOD4        SVMOD4
     C     *LIKE         DEFINE    PMKEY         KKKEY
     C     *LIKE         DEFINE    RTTYPE        KKTYPE
     C*** End Add *** 02/26/10 ****************************************
     C*** Beg Add *** 07/01/10 ****************************************
     C     *LIKE         DEFINE    RVPRCD        KKPRCD
     C     *LIKE         DEFINE    ZRRGN#        KKLOC
     C     *LIKE         DEFINE    DHMOD1        KKMOD
     C     *LIKE         DEFINE    ZRFZCD        KKFZCD
     C     *LIKE         DEFINE    ZREFDT        Z1EFDT
     C     *LIKE         DEFINE    EOAST         KKST
     C*** End Add *** 07/01/10 ****************************************
     C*================================================================
     C* Main Processing
     C*================================================================
     C*
     C** Beg Add *** 02/26/10 *****************************************
     C                   MOVEL(P)  QCA(1)        QQCMD
     C                   MOVEL     'TX'          QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C*** Begin Chg *** 01/21/15 ***************************************
     C****************   PARM      56            LENGTH           15 5
     C                   PARM      61            LENGTH           15 5
     C*** End   Chg *** 01/21/15 ***************************************
     C*
     C                   OPEN      FTABL
     C*
     C** End Add *** 02/26/10 *****************************************
     C* Initialize key values.
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@SUB#        KKSUB#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
     C                   MOVE      @@DSEQ        KKDSEQ
     C                   MOVEL(P)  @@CODE        KKCODE
     C*
     C     CAKY          CHAIN     PCA01                              99
B01  C     *IN99         IFEQ      *OFF
     C*** Beg Add *** 08/29/12 ****************************************
     C     FRKY1         SETGT     PFR01
     C     FRKY2         READPE    PFR01                                  99
     C*** End Add *** 08/29/12 ****************************************
     C*
     C** Beg Add *** 07/01/10 *****************************************
     C     EOKY          CHAIN(N)  PEO01                              99
     C                   MOVEL(P)  QCA(3)        QQCMD
     C  N99              MOVEL     EOAST         QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C*** Begin Chg *** 01/21/15 ***************************************
     C**************     PARM      56            LENGTH           15 5
     C                   PARM      61            LENGTH           15 5
     C*** End   Chg *** 01/21/15 ***************************************
     C*
     C                   OPEN      FMZRL
     C*
     C** End Add *** 07/01/10 *****************************************
     C*** BEG ADD *** 12/07/07 ****************************************
     C                   Z-ADD     CAEOB#        KKEOB#
     C*** END ADD *** 12/07/07 ****************************************
     C*
     C* Retrieve allownaces
     C*
     C                   CALL      'FA747R'
     C                   PARM      DBRRN         ##RRN             9 0
     C     HDMRA         PARM      *ZEROS        ##MRA             9 2
     C     HDPROF        PARM      *ZEROS        ##PROF            9 2
     C     HDTECH        PARM      *ZEROS        ##TECH            9 2
     C*** BEG ADD *** 12/22/08 ***********************************
     C*** value in ##1a7 is being received from FA747R
     C                   PARM      *BLANK        ##1A7             1
     C*** END ADD *** 12/22/08 ***********************************
     C*** Beg Add *** 02/26/10 *****************************************
     C                   PARM      'A'           ##CALL            1
     C*** End Add *** 02/26/10 *****************************************
     C*** Beg Add *** 09/09/10 *****************************************
     C                   MOVE      ' '           XXMOD
     C     LBKY1         SETGT     PLB01                              99
     C     LBKY2         READPE    PLB01                                  99
     C     *IN99         DOWEQ     *OFF
     C     LBSTAT        IFEQ      'I'
     C     LBSTAT        ANDEQ     *OFF
     C     LBKY2         READPE    PLB01                                  99
     C                   ELSE
     C     LBSTAT        IFEQ      'A'
     C                   LEAVE
     C                   ENDIF                                                  LBSTAT=A
     C                   ENDIF                                                  LBSTAT=I
     C                   ENDDO                                                  *IN99='0'
     C     *IN99         IFEQ      *OFF
     C                   MOVE      'L'           ##1A7
     C                   ENDIF                                                  *IN99='0'
     C*** End Add *** 09/09/10 *****************************************
     C*
     C* Do not process if one of the following procedure codes or
     C* if the allowance is zero.
     C*
     C*** BEGIN DEL *** 09/08/08 **************************************
B02  C********** CACPT4    IFNE '97545'
     C********** CACPT4    ANDNE'97546'
     C********** CACPT4    ANDNE'97750'
     C********** CACPT4    ANDNE'97799'
     C*** END   DEL *** 09/08/08 **************************************
     C********** HDMRA     ANDNE0
     C*
     C* Retrieve the eomb header record.
     C*
     C     EOKY          CHAIN     PEO01                              99
     C*
     C* Retrieve the physician detail record.
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C  N99DHKY          CHAIN     PDH01                              99
     C                   ENDSL                                                  End select
     C*
     C* Be sure claim status code is not "C" for cap on EC/MD
     C*
     C                   MOVE      *BLANKS       TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              99
B03  C     *IN99         IFEQ      '0'
     C     CSDISC        ANDNE     'C'
     C*
     C* For each detail record read, find the discount master record
     C* that applys to that date of service.
     C*
     C                   Z-ADD     CAFDAT        KKEFDT
     C*
     C     BDKY1         SETGT     PBD01
     C     BDKY2         READPE    PBD01                                  99
     C     *IN99         DOUEQ     *ON
     C     BDSTAT        IFNE      'I'
     C                   LEAVE
     C                   ELSE
     C     BDKY2         READPE    PBD01                                  99
     C                   ENDIF                                                  BDSTAT
     C                   ENDDO                                                  *IN99
     C*
     C* An active discount code has been found
     C*
B04  C     *IN99         IFEQ      *OFF
     C*
     C* Retrieve % of CMS based upon the procedure code range.
     C*
     C*** BEGIN ADD *** 09/08/08 **************************************
     C                   MOVE      *OFF          GOOD1             1
     C                   MOVE      *OFF          GOOD2             1
     C                   MOVE      *OFF          GOOD3             1
     C*** END   ADD *** 09/08/08 **************************************
     C                   Z-ADD     BDEFDT        KKEFDT
     C                   MOVEL(P)  CACPT4        KK10A1
     C     COKY1         SETGT     PCO01
     C     COKY2         READPE    PCO01                                  99
     C     *IN99         DOUEQ     *ON
     C     KK10A1        IFGE      CO10A1
     C     KK10A1        ANDLE     CO10A2
     C     COSTAT        ANDNE     'I'
     C*** Beg Add *** 02/26/10 *****************************************
     C                   EXSR      PMTMOD
     C*** End Add *** 02/26/10 *****************************************
     C*** BEGIN ADD *** 09/08/08 **************************************
     C                   EXSR      CHKMOD
     C*** Beg Add *** 02/26/10 *****************************************
     C                   MOVEL     SVMOD1        DHMOD1
     C                   MOVEL     SVMOD2        DHMOD2
     C                   MOVEL     SVMOD3        DHMOD3
     C*** End Add *** 02/26/10 *****************************************
     C     FOUND         IFEQ      *ON
     C*** END   ADD *** 09/08/08 **************************************
     C                   LEAVE
     C*** BEGIN ADD *** 09/08/08 **************************************
     C                   ENDIF
     C*** END   ADD *** 09/08/08 **************************************
     C*** BEGIN CHG *** 09/08/08 **************************************
     C**********           ELSE
     C                   ENDIF
     C*** END   CHG *** 09/08/08 **************************************
     C     COKY2         READPE    PCO01                                  99
     C*** BEGIN DEL *** 09/08/08 **************************************
     C**********           ENDIF
     C*** END   DEL *** 09/08/08 **************************************
     C                   ENDDO
     C*
     C* If a valid discount record is not found, zero the discount
     C*
B05  C     *IN99         IFEQ      *OFF
     C*
     C* Calculate the discount                                         LE
     C*
     C* Initialize values
     C*
     C                   EXSR      INZVAL
     C*
     C* This is a UCR bill.
     C*
-    C     CARCFL        IFEQ      'Y'
|    C     HDMRA         OREQ      0
     C*** Beg Add *** 02/26/10 *******************************************
|-   C     CORATE        ANDEQ     *ZEROS
|    C     CACPT4        OREQ      '97545'
|-   C     CORATE        ANDEQ     *ZEROS
|-   C     HDMRA         ANDEQ     *ZEROS
|    C     CACPT4        OREQ      '97546'
|-   C     CORATE        ANDEQ     *ZEROS
     C*** Beg Del *** 06/04/10 *******************************************
|-   C*****      HDMRA     ANDEQ*ZEROS
|    C*****      CACPT4    OREQ '97750'
     C*** End Del *** 06/04/10 *******************************************
|-   C     CORATE        ANDEQ     *ZEROS
|-   C     HDMRA         ANDEQ     *ZEROS
|    C     CACPT4        OREQ      '97799'
|-   C     CORATE        ANDEQ     *ZEROS
|-   C     HDMRA         ANDEQ     *ZEROS
|    C     CACPT4        OREQ      '99455'
|-   C     CORATE        ANDEQ     *ZEROS
|-   C     HDMRA         ANDEQ     *ZEROS
|    C     CACPT4        OREQ      '99456'
|-   C     CORATE        ANDEQ     *ZEROS
|-   C     HDMRA         ANDEQ     *ZEROS
     C     Q1PRMS        IFLE      23
     C*** End Add *** 02/26/10 *******************************************
|    C     CAPAID        MULT      XXFEE         CAPAID
     C*** Beg Add *** 02/26/10 *******************************************
     C                   ENDIF
     C                   Z-ADD     CAPAID        XXMRAA
     C*** End Add *** 02/26/10 *******************************************
     C*** Beg Del *** 02/26/10 *******************************************
|-   C*********  CAPAID    IFGT CACHRG
||   C*********            Z-ADDCACHRG    CAPAID
|-   C*********            ENDIF
     C*** End Del *** 02/26/10 *******************************************
|    C     CACHRG        SUB       CAPAID        CANDIS
|    C*** BEG CHG ****** 02/06/08 ****************************************
|    C**********           SUB  CAMRA     CANDIS
|    C**********           SUB  CAOTHR    CANDIS
|    C**********           SUB  CAOTH2    CANDIS
|    C                   CLEAR                   CAMRA
|    C                   CLEAR                   CAOTHR
|    C                   CLEAR                   CAOTH2
|    C*** END CHG ****** 02/06/08 ****************************************
|    C*
|    C* This not a UCR bill
|    C*
-    C                   ELSE
|    C*** BEGIN ADD *** 09/08/08 **************************************
|-   C     CORATE        IFNE      *ZEROS
     C*** Beg Add *** 05/14/12 ****************************************
     C** Logic to deny payment when a rule is hit
     C*
     C     FRKY1         SETGT     PFR01
     C     FRKY2         READPE    PFR01                                  99
     C     *IN99         IFEQ      *OFF
     C     FRDIS         IFEQ      'Y'
     C                   Z-ADD     0             CORATE
     C                   ENDIF
     C                   ENDIF
     C*** End Add *** 05/14/12 ****************************************
||   C*
     C*** BEG CHG *** 12/02/10 ****************************************
     C***********          Z-ADDCORATE    CAPAID
     C     CAUNTS        MULT(H)   CORATE        CAPAID
     C*** END CHG *** 12/02/10 ****************************************
||   C     CACHRG        SUB       CAPAID        CANDIS
     C*** Beg Del *** 02/26/10 *******************************************
||   C**************       SUB  CAMRA     CANDIS
||   C**************       SUB  CAOTHR    CANDIS
||   C**************       SUB  CAOTH2    CANDIS
     C*** End Del *** 02/26/10 *******************************************
     C*** Beg Add *** 02/26/10 *******************************************
|    C                   CLEAR                   CAMRA
|    C                   CLEAR                   CAOTHR
|    C                   CLEAR                   CAOTH2
     C*** BEG CHG *** 12/02/10 ****************************************
     C***********          Z-ADDCORATE    XXMRAA
     C     CAUNTS        MULT(H)   CORATE        XXMRAA
     C*** END CHG *** 12/02/10 ****************************************
     C*** End Add *** 02/26/10 *******************************************
||   C*
|-   C                   ELSE
||   C*** END   ADD *** 09/08/08 **************************************
||   C*
||   C* Does state fee schedule rules apply?
||   C*
||   C                   EXSR      CHKMRA
||   C*** BEG ADD *** 12/07/07 ****************************************
||   C*
||   C* Calculate the contract allowable for HPSA
||   C*
     C*** Beg Add *** 07/03/12 ****************************************
     C     CAPAID        IFGT      0
     C*** Beg Add *** 07/08/13 ****************************************
     C     CSOVRD        ANDNE     'Y'
     C*** End Add *** 07/08/13 ****************************************
     C*** End Add *** 07/03/12 ****************************************
||   C                   EXSR      HPSA
     C*** Beg Add *** 07/03/12 ****************************************
     C                   ENDIF
     C*** End Add *** 07/03/12 ****************************************
||   C*** END ADD *** 12/07/07 ****************************************
||   C*
||   C* State fee schedule rule applies.
||   C*
||-  C     RULE          IFEQ      *ON
|||  C*** BEGIN CHG *** 02/12/08 **************************************
|||  C********** XXMRAA    MULT ALLOWP    CAPAID
|||  C     XXMRAA        MULT(H)   ALLOWP        CAPAID
|||  C*** END   CHG *** 02/12/08 **************************************
||-  C                   ELSE
     C***** Beg Del *** 05/28/15 **************************************
|||  C*****************  Z-ADD     XXMRAA        CAPAID
     C***** End Del *** 05/28/15 **************************************
     C***** Beg Add *** 05/28/15 **************************************
     C     CAUNTS        IFLE      1
     C                   Z-ADD     XXMRAA        CAPAID
     C                   ELSE
     C     XXMRAA        MULT(H)   CAUNTS        CAPAID
     C                   ENDIF
     C***** End Add *** 05/28/15 **************************************
||-  C                   ENDIF
||   C*
||   C     CAPAID        IFGT      CACHRG
||   C     GTCHRG        ANDEQ     *OFF
||   C                   Z-ADD     CACHRG        CAPAID
||   C                   ENDIF
||   C*
||   C     CACHRG        SUB       CAPAID        CANDIS
||   C*** BEG CHG ****** 02/06/08 ****************************************
||   C**********           SUB  CAMRA     CANDIS
||   C**********           SUB  CAOTHR    CANDIS
||   C**********           SUB  CAOTH2    CANDIS
||   C                   CLEAR                   CAMRA
||   C                   CLEAR                   CAOTHR
||   C                   CLEAR                   CAOTH2
||   C*** END CHG ****** 02/06/08 ****************************************
||   C*** BEGIN ADD *** 01/15/08 **************************************
||-  C     CANDIS        IFEQ      0.01
|||  C     CANDIS        OREQ      -0.01
|||  C                   ADD       CANDIS        CAPAID
|||  C                   Z-ADD     0             CANDIS
||-  C                   ENDIF
||   C*** END   ADD *** 01/15/08 **************************************
||   C*** BEGIN ADD *** 09/08/08 **************************************
|-   C                   ENDIF
|    C*** END   ADD *** 09/08/08 **************************************
-    C                   ENDIF
     C*** BEG ADD ****** 02/06/08 ****************************************
     C                   ELSE
     C*** Beg Chg *** 02/26/10 ****************************************
     C*********            Z-ADDCAMRA     CANDIS
     C*********            ADD  CAOTHR    CANDIS
     C*********            ADD  CAOTH2    CANDIS
     C                   Z-ADD     CACHRG        CANDIS
     C                   CLEAR                   CAPAID
     C*** End Chg *** 02/26/10 ****************************************
     C                   CLEAR                   CAMRA
     C                   CLEAR                   CAOTHR
     C                   CLEAR                   CAOTH2
     C*
     C* What eob code do we use?
     C*
     C     CAAST         IFEQ      'TX'
     C                   MOVEL(P)  '45'          XXEOB
     C                   ELSE
     C                   MOVEL(P)  '398'         XXEOB
     C                   ENDIF
     C*** BEGIN ADD *** 09/08/08 **************************************
     C*
     C* WRITE MESSAGE THAT SAYS NO MATCH FOUND
     C*
     C*** Beg Add *** 02/26/10 ****************************************
     C     Q1PRMS        IFGT      23
     C     @@MODE        ANDEQ     'F'
     C     Q1PRMS        ORGT      23
     C     @@MODE        ANDEQ     'P'
     C     Q1PRMS        ORGT      23
     C     @@MODE        ANDEQ     'T'
     C                   ELSE
     C*** End Add *** 02/26/10 ****************************************
     C                   Z-ADD     CACO#         MGCO#
     C                   Z-ADD     CASUB#        MGSUB#
     C                   Z-ADD     CAFND         MGFND
     C                   Z-ADD     CAFYR         MGFYR
     C                   Z-ADD     CACASE        MGCASE
     C                   Z-ADD     CADCS#        MGDCS#
     C                   Z-ADD     CADSEQ        MGDSEQ
     C                   Z-ADD     1             MGMSEQ
     C                   MOVE      CAEOB#        MGEOB#
     C                   MOVE      'NW00004'     MGMSG#
     C                   MOVE      *BLANKS       MGMSFX
     C                   MOVE      *BLANKS       MGCSI
     C                   MOVE      'N'           MGSRC
     C                   MOVE      'A'           MGSTAT
     C                   MOVE      CAUSER        MGUSER
     C                   MOVE      CAPGM         MGPGM
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        MGTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGDT
     C                   MOVE      MGUSER        MGIUSR
     C                   MOVE      MGPGM         MGIPGM
     C                   Z-ADD     MGTIME        MGITIM
     C                   Z-ADD     MGDT          MGIDT
     C                   MOVE      CARCN         MGRCN
     C                   WRITE     PMG01
     C*** Beg Add *** 02/26/10 ****************************************
     C                   ENDIF                                                  Q1PRMS > 23
     C*** End Add *** 02/26/10 ****************************************
     C*** END   ADD *** 09/08/08 **************************************
E05  C                   ENDIF                                                   NDCOP *IN99
     C*** END ADD ****** 02/06/08 ****************************************
     C*
     C* If CANDIS has an amount then update detail
     C*
     C*** Beg Del *** 02/26/10 ****************************************
B06  C********** CANDIS    IFNE *ZEROS
     C*** Beg Del *** 02/26/10 ****************************************
     C*
     C* Remove system messages and notes.
     C*** Beg Add *** 02/26/10 ****************************************
     C     Q1PRMS        IFGT      23
     C     @@MODE        ANDEQ     'F'
     C     Q1PRMS        ORGT      23
     C     @@MODE        ANDEQ     'P'
     C     Q1PRMS        ORGT      23
     C     @@MODE        ANDEQ     'T'
     C*** Beg Add *** 08/29/12 ****************************************
     C     FRDIS         OREQ      'Y'
     C     CAPAID        ANDEQ     0
     C*** End Add *** 08/29/12 ****************************************
     C                   ELSE
     C*** End Add *** 02/26/10 ****************************************
     C*
     C                   EXSR      REMMSG
     C*
     C* Get the next available sequence number
     C*
     C                   Z-ADD     *HIVAL        KKSEQ#
     C     DEKY1         SETGT     PDE02
     C     DEKY2         READPE    PDE02                                  99
     C     *IN99         IFEQ      *OFF
     C     DESEQ#        ADD       1             XXSEQ#
     C                   ELSE
     C                   Z-ADD     1             XXSEQ#
     C                   ENDIF
     C*
     C* Write the eob note.
     C*
     C                   CLEAR                   PDE01
     C                   MOVE      CACO#         DECO#
     C                   MOVE      CASUB#        DESUB#
     C                   MOVE      CAFND         DEFND
     C                   MOVE      CAFYR         DEFYR
     C                   MOVE      CACASE        DECASE
     C                   MOVE      CASS#         DESS#
     C                   MOVE      CAACDT        DEACDT
     C                   MOVE      CAEOB#        DEEOB#
     C                   MOVE      CADSEQ        DEDSEQ
     C                   Z-ADD     XXSEQ#        DESEQ#
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      XXEOB         DEEOB
     C                   MOVEL     XXEOB         DEEOBCD
     C**** End Chg *** 07/18/23 **************************************
     C****** BEGIN CHANGE ****************  02/12/08-B ****************
     C*****      XXEOB     IFEQ '398'
     C*****                MOVELXXNOT1    DENOT1    P
     C*****                ELSE
     C*****                MOVELXXNOT2    DENOT1    P
     C*****                ENDIF
     C                   MOVEL     XXNOTC        XXNOT1           70
     C                   MOVE      *BLANKS       XXNOT2           70
      *
     C     CAAST         IFEQ      'TX'
     C                   MOVEL(P)  XXEOB         TEBKEY
     C     TEBKY         CHAIN     PEB01TX                            60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  EBDESC        XXNOT1
     C                   MOVEL(P)  EBDSC2        XXNOT2
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVEL(P)  XXNOT1        DENOT1
      ****** END ADD ****************  02/12/08-B *********************
      ****** BEGIN CHANGE ***********  02/12/08-B *********************
     C*****                MOVE *BLANKS   DENOT2
     C                   MOVEL     XXNOT2        DENOT2
      ****** END CHANGE *************  02/12/08-B *********************
     C                   MOVE      'A'           DESTAT
     C                   MOVE      'N'           DEESRC
     C                   Z-ADD     CARCN         DERCN
     C                   MOVE      Q1USER        DEUSER
     C                   MOVE      Q1PGM         DEPGM
     C                   MOVE      Q1USER        DEIUSR
     C                   MOVE      Q1PGM         DEIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DEDT
     C                   MOVEL     SRDTO         DEIDT
     C                   Z-ADD     DSTIME        DETIME
     C                   Z-ADD     DSTIME        DEITIM
     C                   WRITE     PDE01
     C*** Beg Add *** 02/26/10 ****************************************
     C                   ENDIF                                                  Q1PRMS > 23
     C*** End Add *** 02/26/10 ****************************************
     C*** Beg Del *** 02/26/10 ****************************************
E06  C**********           ENDIF                            CANDIS <> 0
     C*** End Del *** 02/26/10 ****************************************
     C*
     C* Update the detail record.
     C*** Beg Add *** 02/26/10 ****************************************
     C     Q1PRMS        IFGT      23
     C     @@MODE        ANDEQ     'F'
     C     Q1PRMS        ORGT      23
     C     @@MODE        ANDEQ     'P'
     C     Q1PRMS        ORGT      23
     C     @@MODE        ANDEQ     'T'
     C*** Release the record lock on FDCAP
     C                   UNLOCK    FDCALF
     C                   ELSE
     C*** End Add *** 02/26/10 ****************************************
     C*
     C                   UPDATE    PCA01
     C*** Beg Add *** 02/26/10 ****************************************
     C                   ENDIF                                                  Q1PRMS > 23
     C*** End Add *** 02/26/10 ****************************************
     C*
     C*** BEG DEL ****** 02/06/08 ****************************************
E05  C**********           ENDIF                            NDCOP *IN99
     C*** END DEL ****** 02/06/08 ****************************************
E04  C                   ENDIF                                                   NMBDP *IN99
E03  C                   ENDIF                                                   CLM STS CD NE 'C'
     C*** BEGIN DEL *** 09/08/08 **************************************
E02  C**********           ENDIF                            CACPT4 *IN99
     C*** END   DEL *** 09/08/08 **************************************
E01  C                   ENDIF                                                   FDCALF *IN99
     C*
     C*** Beg Add *** 02/26/10 ****************************************
     C     Q1PRMS        IFGT      23
     C                   Z-ADD     XXMRAA        @@ALOW
     C                   ENDIF
     C*
     C*** Begin Chg *** 01/21/15 ***************************************
     C***********        Z-ADD     56            LENGTH
     C                   Z-ADD     61            LENGTH
     C*** End   Chg *** 01/21/15 ***************************************
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*
     C                   CLOSE     FTABL
     C*** End Add *** 02/26/10 ****************************************
     C*** Beg Add *** 07/01/10 ****************************************
     C*** Begin Chg *** 01/21/15 ***************************************
     C****************** Z-ADD     56            LENGTH
     C                   Z-ADD     61            LENGTH
     C*** End   Chg *** 01/21/15 ***************************************
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*
     C                   CLOSE     FMZRL
     C*** End Add *** 07/01/10 ****************************************
     C                   SETON                                        LR
     C*================================================================***
     C* CHKMOD - Check modifier                                        N T
     C*================================================================***
     C     CHKMOD        BEGSR
     C*
     C                   MOVE      *OFF          GOOD1             1
     C                   MOVE      *OFF          GOOD2             1
     C                   MOVE      *OFF          GOOD3             1
     C                   MOVE      *OFF          GOOD4             1
     C                   MOVE      *OFF          GOOD5             1
     C                   MOVE      *OFF          GOOD6             1
     C                   MOVE      *OFF          FOUND             1
     C*
     C     DHMOD1        IFEQ      *BLANKS
     C     DHMOD2        ANDEQ     *BLANKS
     C     DHMOD3        ANDEQ     *BLANKS
     C     COPMOD        IFEQ      *BLANKS
     C     COPMD2        ANDEQ     *BLANKS
     C     COPMD3        ANDEQ     *BLANKS
     C                   MOVE      *ON           GOOD1
     C                   MOVE      *ON           GOOD2
     C                   MOVE      *ON           GOOD3
     C                   MOVE      *ON           GOOD4
     C                   MOVE      *ON           GOOD5
     C                   MOVE      *ON           GOOD6
     C                   ENDIF
     C                   ENDIF
     C*
     C     DHMOD1        IFNE      *BLANKS
     C     DHMOD1        IFEQ      COPMOD
     C     DHMOD1        OREQ      COPMD2
     C     DHMOD1        OREQ      COPMD3
     C                   MOVE      *ON           GOOD1
     C                   ENDIF
     C                   ELSE
     C     COPMOD        IFEQ      *BLANKS
     C     COPMD2        OREQ      *BLANKS
     C     COPMD3        OREQ      *BLANKS
     C                   MOVE      *ON           GOOD1
     C                   ENDIF
     C                   ENDIF
     C*
     C     DHMOD2        IFNE      *BLANKS
     C     DHMOD2        IFEQ      COPMOD
     C     DHMOD2        OREQ      COPMD2
     C     DHMOD2        OREQ      COPMD3
     C                   MOVE      *ON           GOOD2
     C                   ENDIF
     C                   ELSE
     C     COPMOD        IFEQ      *BLANKS
     C     COPMD2        OREQ      *BLANKS
     C     COPMD3        OREQ      *BLANKS
     C                   MOVE      *ON           GOOD2
     C                   ENDIF
     C                   ENDIF
     C*
     C     DHMOD3        IFNE      *BLANKS
     C     DHMOD3        IFEQ      COPMOD
     C     DHMOD3        OREQ      COPMD2
     C     DHMOD3        OREQ      COPMD3
     C                   MOVE      *ON           GOOD3
     C                   ENDIF
     C                   ELSE
     C     COPMOD        IFEQ      *BLANKS
     C     COPMD2        OREQ      *BLANKS
     C     COPMD3        OREQ      *BLANKS
     C                   MOVE      *ON           GOOD3
     C                   ENDIF
     C                   ENDIF
     C*
     C     COPMOD        IFNE      *BLANKS
     C     COPMOD        IFEQ      DHMOD1
     C     COPMOD        OREQ      DHMOD2
     C     COPMOD        OREQ      DHMOD3
     C                   MOVE      *ON           GOOD4
     C                   ENDIF
     C                   ELSE
     C     DHMOD1        IFEQ      *BLANKS
     C     DHMOD2        OREQ      *BLANKS
     C     DHMOD3        OREQ      *BLANKS
     C                   MOVE      *ON           GOOD4
     C                   ENDIF
     C                   ENDIF
     C*
     C     COPMD2        IFNE      *BLANKS
     C     COPMD2        IFEQ      DHMOD1
     C     COPMD2        OREQ      DHMOD2
     C     COPMD2        OREQ      DHMOD3
     C                   MOVE      *ON           GOOD5
     C                   ENDIF
     C                   ELSE
     C     DHMOD1        IFEQ      *BLANKS
     C     DHMOD2        OREQ      *BLANKS
     C     DHMOD3        OREQ      *BLANKS
     C                   MOVE      *ON           GOOD5
     C                   ENDIF
     C                   ENDIF
     C*
     C     COPMD3        IFNE      *BLANKS
     C     COPMD3        IFEQ      DHMOD1
     C     COPMD3        OREQ      DHMOD2
     C     COPMD3        OREQ      DHMOD3
     C                   MOVE      *ON           GOOD6
     C                   ENDIF
     C                   ELSE
     C     DHMOD1        IFEQ      *BLANKS
     C     DHMOD2        OREQ      *BLANKS
     C     DHMOD3        OREQ      *BLANKS
     C                   MOVE      *ON           GOOD6
     C                   ENDIF
     C                   ENDIF
     C*
     C     GOOD1         IFEQ      *ON
     C     GOOD2         ANDEQ     *ON
     C     GOOD3         ANDEQ     *ON
     C     GOOD4         ANDEQ     *ON
     C     GOOD5         ANDEQ     *ON
     C     GOOD6         ANDEQ     *ON
     C                   MOVE      *ON           FOUND
     C                   ENDIF
     C*
     C                   ENDSR
     C*** Beg Add *** 02/26/10 *****************************************
     C*================================================================***
     C* PMTMOD - Check to see it is payment affecting modifier
     C     PMTMOD        BEGSR
     C                   MOVEL     DHMOD1        SVMOD1
     C                   MOVEL     DHMOD2        SVMOD2
     C                   MOVEL     DHMOD3        SVMOD3
     C*** Beg Add *** 02/26/10 *****************************************
     C     Q1PRMS        IFGT      23
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'F'
     C*
     C     DHMOD1        IFEQ      'TC'
     C     DHMOD1        OREQ      '26'
     C                   MOVE      *BLANKS       DHMOD1
     C                   ENDIF
     C*
     C     DHMOD2        IFEQ      'TC'
     C     DHMOD2        OREQ      '26'
     C                   MOVE      *BLANKS       DHMOD2
     C                   ENDIF
     C*
     C     DHMOD3        IFEQ      'TC'
     C     DHMOD3        OREQ      '26'
     C                   MOVE      *BLANKS       DHMOD3
     C                   ENDIF
     C*
     C     @@MODE        WHENEQ    'P'
     C*
     C*****      DHMOD1    IFEQ ' '
     C                   MOVEL     '26'          DHMOD1
     C                   MOVE      *BLANKS       DHMOD2
     C                   MOVE      *BLANKS       DHMOD3
     C*****                ELSE
     C*****      DHMOD2    IFEQ ' '
     C*****                MOVEL'26'      DHMOD2
     C*****                ELSE
     C*****      DHMOD3    IFEQ ' '
     C*****                MOVEL'26'      DHMOD3
     C*****                ENDIF
     C*****                ENDIF
     C*****                ENDIF
     C*
     C     @@MODE        WHENEQ    'T'
     C*
     C*****      DHMOD1    IFEQ '26'
     C*****                MOVEL' '       DHMOD1
     C*****                ENDIF
     C*
     C*****      DHMOD2    IFEQ '26'
     C*****                MOVEL' '       DHMOD2
     C*****                ENDIF
     C*
     C*****      DHMOD3    IFEQ '26'
     C*****                MOVEL' '       DHMOD3
     C*****                ENDIF
     C*
     C*****      DHMOD1    IFEQ ' '
     C                   MOVEL     'TC'          DHMOD1
     C                   MOVE      *BLANKS       DHMOD2
     C                   MOVE      *BLANKS       DHMOD3
     C*****                ELSE
     C*****      DHMOD2    IFEQ ' '
     C*****                MOVEL'TC'      DHMOD2
     C*****                ELSE
     C*****      DHMOD3    IFEQ ' '
     C*****                MOVEL'TC'      DHMOD3
     C*****                ENDIF
     C*****                ENDIF
     C*****                ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  Q1PRMS >= 23
     C*
     C*** End Add *** 02/26/10 *****************************************
     C                   MOVEL     DHMOD1        KKKEY
     C     TBKY          CHAIN     PPM01                              60
     C     *IN60         IFEQ      *OFF
     C     PMAFPY        ANDEQ     ' '
     C                   MOVE      *BLANKS       DHMOD1
     C                   ENDIF
     C                   MOVEL     DHMOD2        KKKEY
     C     TBKY          CHAIN     PPM01                              60
     C     *IN60         IFEQ      *OFF
     C     PMAFPY        ANDEQ     ' '
     C                   MOVE      *BLANKS       DHMOD2
     C                   ENDIF
     C                   MOVEL     DHMOD3        KKKEY
     C     TBKY          CHAIN     PPM01                              60
     C     *IN60         IFEQ      *OFF
     C     PMAFPY        ANDEQ     ' '
     C                   MOVE      *BLANKS       DHMOD3
     C                   ENDIF
     C                   ENDSR
     C*================================================================***
     C*** End Add *** 02/26/10 *****************************************
     C*================================================================***
     C* CHKMRA - Check to see if rule applies and get %                N T
     C*================================================================***
     C     CHKMRA        BEGSR
     C*
     C* Initialize to mra
     C*
     C                   Z-ADD     HDMRA         XXMRAB
     C*** Beg Add *** 02/26/10 ****************************************
     C     CACPT4        IFEQ      '97545'
     C*** Beg Del *** 06/04/10 ****************************************
     C******     CACPT4    OREQ '97750'
     C*** End Del *** 06/04/10 ****************************************
     C     CACPT4        OREQ      '97799'
     C     CACPT4        OREQ      '97546'
     C                   Z-ADD     CAMRAA        XXMRAB
     C                   ENDIF
     C*** End Add *** 02/26/10 ****************************************
     C*
     C*
     C* Should we use professional component?
     C*
     C     DHMOD1        IFEQ      '26'
     C     DHMOD2        OREQ      '26'
     C     DHMOD3        OREQ      '26'
     C     DHMOD4        OREQ      '26'
     C                   Z-ADD     HDPROF        XXMRAB
     C                   ENDIF
     C*
     C* Should we use technical component?
     C*
     C     DHMOD1        IFEQ      'TC'
     C     DHMOD2        OREQ      'TC'
     C     DHMOD3        OREQ      'TC'
     C     DHMOD4        OREQ      'TC'
     C                   Z-ADD     HDTECH        XXMRAB
     C                   ENDIF
     C*
     C* Should this bill pay more than charged
     C*
     C                   MOVEL     *OFF          GTCHRG            1
     C*** BEG DEL *** 12/07/07 ****************************************
     C*********  DHMOD1    IFEQ 'AQ'
     C*********  DHMOD2    OREQ 'AQ'
     C*********  DHMOD3    OREQ 'AQ'
     C*********  DHMOD4    OREQ 'AQ'
     C*********            MOVE *ON       GTCHRG
     C*********            ENDIF
     C*** END DEL *** 12/07/07 ****************************************
     C*
     C* Does a fee schedule rule apply?
     C*
     C     CAPAID        IFLT      CACHRG
     C*** BEG DEL *** 12/07/07 ****************************************
     C********** GTCHRG    OREQ *ON
     C*** END DEL *** 12/07/07 ****************************************
     C     XXMRAB        IFNE      CAPAID
     C*** BEGIN ADD *** 09/10/08 **************************************
     C     XXMRAB        ANDNE     *ZEROS
     C*** END   ADD *** 09/10/08 **************************************
     C     CAPAID        DIV(H)    XXMRAB        ALLOWP
     C                   MOVE      *ON           RULE
     C                   ENDIF
     C                   ENDIF
     C*
     C*** Beg Add *** 02/26/10 *****************************************
     C     Q1PRMS        IFGT      23
     C                   SELECT
     C     @@MODE        WHENEQ    'P'
     C                   Z-ADD     HDPROF        XXMRAB
     C     @@MODE        WHENEQ    'T'
     C                   Z-ADD     HDTECH        XXMRAB
     C     @@MODE        WHENEQ    'F'
     C                   Z-ADD     HDMRA         XXMRAB
     C*** Beg Add *** 02/26/10 ****************************************
     C     CACPT4        IFEQ      '97545'
     C*** Beg Del *** 06/04/10 ****************************************
     C*******    CACPT4    OREQ '97750'
     C*** End Del *** 06/04/10 ****************************************
     C     CACPT4        OREQ      '97799'
     C     CACPT4        OREQ      '97546'
     C                   Z-ADD     CAMRAA        XXMRAB
     C                   ENDIF
     C*** End Add *** 02/26/10 ****************************************
     C                   ENDSL
     C                   ENDIF
     C*
     C     Q1PRMS        IFGT      23
     C*
     C                   MOVEL     DHMOD1        SVMOD1
     C                   MOVEL     DHMOD2        SVMOD2
     C                   MOVEL     DHMOD3        SVMOD3
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'F'
     C*
     C     DHMOD1        IFEQ      'TC'
     C     DHMOD1        OREQ      '26'
     C                   MOVE      *BLANKS       DHMOD1
     C                   ENDIF
     C*
     C     DHMOD2        IFEQ      'TC'
     C     DHMOD2        OREQ      '26'
     C                   MOVE      *BLANKS       DHMOD2
     C                   ENDIF
     C*
     C     DHMOD3        IFEQ      'TC'
     C     DHMOD3        OREQ      '26'
     C                   MOVE      *BLANKS       DHMOD3
     C                   ENDIF
     C*
     C     @@MODE        WHENEQ    'P'
     C*
     C*****      DHMOD1    IFEQ ' '
     C                   MOVEL     '26'          DHMOD1
     C                   MOVE      *BLANKS       DHMOD2
     C                   MOVE      *BLANKS       DHMOD3
     C*****                ELSE
     C*****      DHMOD2    IFEQ ' '
     C*****                MOVEL'26'      DHMOD2
     C*****                ELSE
     C*****      DHMOD3    IFEQ ' '
     C*****                MOVEL'26'      DHMOD3
     C*****                ENDIF
     C*****                ENDIF
     C*****                ENDIF
     C*
     C     @@MODE        WHENEQ    'T'
     C*
     C*****      DHMOD1    IFEQ '26'
     C*****                MOVEL' '       DHMOD1
     C*****                ENDIF
     C*
     C*****      DHMOD2    IFEQ '26'
     C*****                MOVEL' '       DHMOD2
     C*****                ENDIF
     C*
     C*****      DHMOD3    IFEQ '26'
     C*****                MOVEL' '       DHMOD3
     C*****                ENDIF
     C*
     C*****      DHMOD1    IFEQ ' '
     C                   MOVEL     'TC'          DHMOD1
     C                   MOVE      *BLANKS       DHMOD2
     C                   MOVE      *BLANKS       DHMOD3
     C*****                ELSE
     C*****      DHMOD2    IFEQ ' '
     C*****                MOVEL'TC'      DHMOD2
     C*****                ELSE
     C*****      DHMOD3    IFEQ ' '
     C*****                MOVEL'TC'      DHMOD3
     C*****                ENDIF
     C*****                ENDIF
     C*****                ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  Q1PRMS >= 23
     C*
     C     ##1A7         IFEQ      'L'
     C                   Z-ADD     HDTECH        XXMRAB
     C                   ENDIF
     C*
     C                   Z-ADD     XXMRAB        SVMRAB
     C                   Z-ADD     XXMRAB        XMMRAB
     C*** End Add *** 02/26/10 *****************************************
     C* Calculate the contract allowable.
     C*
     C*** Beg Add *************04/11/08 **********************************
     C****If service date is less than 03/01/08, divide by 1.25
     C****else call a subroutine to calculate the base Medicare Allowance.
     C     CAFDAT        IFLT      1080301
     C*** BEG ADD ************ 12/22/08 **********************************
     C     ##1A7         OREQ      'D'
     C*** END ADD ************ 12/22/08 **********************************
     C*** Beg Add *** 02/26/10 ****************************************
     C     ##1A7         OREQ      'M'
     C*** code is from clinical lab FS
     C     ##1A7         OREQ      'L'
     C*** code is dmepos
     C     CACPT4        ORGE      'A4200'
     C     CACPT4        ANDLE     'A8900'
     C*
     C     CACPT4        ORGE      'E0100'
     C     CACPT4        ANDLE     'E9999'
     C*
     C     CACPT4        ORGE      'K0001'
     C     CACPT4        ANDLE     'K9999'
     C*
     C     CACPT4        ORGE      'L0001'
     C     CACPT4        ANDLE     'L9999'
     C*
     C     CACPT4        ORGE      'V2001'
     C     CACPT4        ANDLE     'V2786'
     C*
     C*** Beg Del *** 07/13/10 *******************************************
     C*****      CACPT4    ORGE '00100'
     C*****      CACPT4    ANDLE'01999'
     C*** End Del *** 07/13/10 *******************************************
     C*
     C*** End Add *** 02/26/10 ****************************************
     C*** End Add *************04/11/08 **********************************
     C                   DIV       1.25          XXMRAB
     C*** Beg Add *************04/11/08 **********************************
     C                   ELSE
     C                   EXSR      CALMED
     C                   ENDIF
     C*** End Add *************04/11/08 **********************************
     C*** Beg Add *** 02/26/10 ****************************************
     C     ##1A7         IFEQ      'L'
     C*
     C     DHMOD1        IFEQ      '26'
     C     DHMOD2        OREQ      '26'
     C     DHMOD3        OREQ      '26'
     C     DHMOD4        OREQ      '26'
     C     XXMRAB        MULT(H)   .45           XXMRAB
     C                   MOVE      'Y'           XXFND             1
     C                   ENDIF
     C*
     C     XXFND         IFNE      'Y'
     C     DHMOD1        IFEQ      'TC'
     C     DHMOD2        OREQ      'TC'
     C     DHMOD3        OREQ      'TC'
     C     DHMOD4        OREQ      'TC'
     C                   MOVE      'Y'           XXFND             1
     C                   ENDIF
     C                   ENDIF
     C*
     C     Q1PRMS        IFGT      23
     C     @@MODE        ANDEQ     'F'
     C     XXFND         ORNE      'Y'
     C*
     C     DHMOD1        IFEQ      ' '
     C     DHMOD2        OREQ      ' '
     C     DHMOD3        OREQ      ' '
     C     DHMOD4        OREQ      ' '
     C                   Z-ADD     XXMRAB        SVMRAB
     C     XXMRAB        MULT(H)   .45           XXMRAB
     C     SVMRAB        ADD       XXMRAB        XXMRAB
     C                   ENDIF
     C*
     C                   ENDIF                                                  @@MODE = 'F'
     C*
     C                   ENDIF                                                  ##1A7 = 'L'
     C*
     C                   MOVEL     SVMOD1        DHMOD1
     C                   MOVEL     SVMOD2        DHMOD2
     C                   MOVEL     SVMOD3        DHMOD3
     C     CACPT4        IFEQ      '97545'
     C     CACPT4        OREQ      '97546'
     C*** Beg Del ***  06/04/10 ******************************************
     C*******    CACPT4    OREQ '97750'
     C*** End Del ***  06/04/10 ******************************************
     C     CACPT4        OREQ      '97799'
     C                   Z-ADD     XMMRAB        XXMRAB
     C                   ENDIF
     C*** End Add *** 02/26/10 ****************************************
     C     XXMRAB        MULT(H)   XXFEE         XXMRAA
     C*
     C                   ENDSR
     C*** BEG ADD *** 12/07/07 ****************************************
     C*================================================================***
     C* HPSA - Calculate the HPSA base rate                            N T
     C*================================================================***
     C     HPSA          BEGSR
     C*
     C* Calculate the HPSA BASE rate
     C*
     C     CAKY1         CHAIN     PCA02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     XRBFHE        ANDEQ     'Y'
     C*
     C                   MOVE      *ON           GTCHRG
     C                   MOVE      *OFF          RULE
     C*
     C     XRPROF        IFNE      0
     C     XRPROF        MULT      .10           AMT1              9 2
     C                   ADD       CACHRG        AMT1
     C     XRPROF        MULT      .10           AMT2              9 2
     C                   ADD       XXMRAB        AMT2
     C                   ELSE
     C     XXMRAB        MULT      .10           AMT1
     C                   ADD       CACHRG        AMT1
     C     XXMRAB        MULT      .10           AMT2
     C                   ADD       XXMRAB        AMT2
     C                   ENDIF
     C*
     C     AMT1          IFLT      AMT2
     C                   Z-ADD     AMT1          XXMRAB
     C                   ELSE
     C                   Z-ADD     AMT2          XXMRAB
     C                   ENDIF
     C*
     C     XXMRAB        MULT(H)   XXFEE         XXMRAA
     C*** Beg Add *** 05/14/12 ****************************************
     C*** Beg Del *** 07/03/12 ****************************************
     C** Logic to deny payment when a rule is hit
     C**********
     C********** CAPAID    IFLT CACHRG
     C********** GTCHRG    OREQ *ON
     C**********
     C********** XXMRAB    IFNE CAPAID
     C********** XXMRAB    ANDNE*ZEROS
     C**********
     C********** CAPAID    DIV  XXMRAB    ALLOWP    H
     C**********           MOVE *ON       RULE
     C**********
     C**********           ENDIF
     C**********           ENDIF
     C*** End Del *** 07/03/12 ****************************************
     C*** End Add *** 05/14/12 ****************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*** END ADD *** 12/07/07 ****************************************
     C*================================================================***
     C* INZVAL - Initialize values                                     N T
     C*================================================================***
     C     INZVAL        BEGSR
     C*
     C* Initialize work fields
     C*
     C                   MOVE      *OFF          RULE              1
     C                   Z-ADD     0             ALLOWP            5 3
     C                   Z-ADD     0             XXMRAB            9 2
     C                   Z-ADD     0             XXMRAA            9 2
     C                   Z-ADD     0             XXDIFF            9 2
     C*** Beg Add ******* 04/11/08 **********************************
     C                   Z-ADD     0             XXMRAH           11 4
     C*** End Add ******* 04/11/08 **********************************
     C*
     C* Calcualte the % of CMS
     C*
     C     COFEE         DIV(H)    100           XXFEE             9 4
     C*
     C* What eob code do we use?
     C*
     C     CAAST         IFEQ      'TX'
     C                   MOVEL(P)  '45'          XXEOB
     C                   ELSE
     C                   MOVEL(P)  '398'         XXEOB
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================***
     C* REMMSG - Remove adjudication messages/EOMB Codes               N T
     C*================================================================***
     C     REMMSG        BEGSR
     C*
     C* Remove adjudication messages from Prov bills errors
     C*
     C*** Beg Del *** 02/26/10 ****************************************
     C********** MGKY      CHAINPMG01                99
     C********** *IN99     DOWEQ*OFF
     C********** MGSRC     IFEQ 'S'
     C**********           MOVE MGMSG#    XXMSG#  5
     C********** XXMSG#    IFNE '00003'
     C*** End Del *** 02/26/10 ****************************************
     C*** BEG ADD ******** 07/07/08 ********************************
     C*** Beg Del *** 06/29/10 ****************************************
     C*********  DEKY      CHAINPDE01                99
     C*** Beg Chg *** 02/26/10 ****************************************
     C********** *IN99     IFEQ *OFF
     C*********  *IN99     DOWEQ*OFF
     C*** End Chg *** 02/26/10 ****************************************
     C*********  DEESRC    IFEQ 'A'
     C*********  DEEOB     ANDEQ'W1'
     C*** END ADD ******** 07/07/08 ********************************
     C*** Beg Add *** 02/26/10 ****************************************
     C*********  MGKY      CHAINPMG01                99
     C*********  *IN99     DOWEQ*OFF
     C*********  MGSRC     IFEQ 'S'
     C*********            MOVE MGMSG#    XXMSG#  5
     C*********  XXMSG#    IFNE '00003'
     C*********  MGDSEQ    ANDEQDEDSEQ
     C*** End Add *** 02/26/10 ****************************************
     C*********            DELETPMG01
     C*********            ENDIF                           SRC = 'S'
     C*********            ENDIF                           MSG# <> '00003'
     C*********  MGKY      READEPMG01                    99
     C*********            ENDDO                           *IN99 = '0'
     C*** BEG ADD ******** 07/07/08 ********************************
     C*********            ENDIF                           DEESRC = 'A'
     C*** Beg Chg *** 02/26/10 ****************************************
     C**********           ENDIF                           *IN99 = '0'
     C*********  DEKY      READEPDE01                    99
     C*********            ENDDO                           *IN99 = '0'
     C*** End Del *** 06/29/10 ****************************************
     C*** End Chg *** 02/26/10 ****************************************
     C*** END ADD ******** 07/07/08 ********************************
     C*** Beg Del *** 02/26/10 ****************************************
     C**********           ENDIF
     C**********           ENDIF                           SRC = 'S'
     C********** MGKY      READEPMG01                    99
     C**********           ENDDO                           *IN99 = Off
     C*** End Del *** 02/26/10 ****************************************
     C*
     C* Remove adjudication EOMB codes from Detail EOB/NOTES
     C*
     C     DEKY          CHAIN     PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C     DEESRC        IFEQ      'A'
     C     DEDSEQ        ANDEQ     CADSEQ
     C*** BEG ADD ******** 07/07/08 ********************************
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         ANDEQ     'W1'
     C     DEEOBCD       ANDEQ     'W1'
     C**** End Chg *** 07/18/23 **************************************
     C*** END ADD ******** 07/07/08 ********************************
     C*
     C     DEESRC        orEQ      'A'
     C     DEDSEQ        ANDEQ     CADSEQ
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         ANDEQ     'P12'
     C     DEEOBCD       ANDEQ     'P12'
     C**** End Chg *** 07/18/23 **************************************
     C*
     C                   DELETE    PDE01
     C                   ENDIF                                                  ESRC = 'A'
     C     DEKY          READE     PDE01                                  99
     C                   ENDDO                                                  *IN99 = Off
     C*
     C                   ENDSR
     C*** Beg Add ******* 04/11/08 *******************************************
     C************************************************************************
     C*** CALMED - CALCULATE MEDICARE ALLOWANCE BASED ON NEW TEXAS MULTIPLIERS
     C************************************************************************
     C     CALMED        BEGSR
     C*** BEG ADD *** 03/24/09 ****************************************
     C                   SELECT
     C*
     C     CAFDAT        WHENLT    1090101
     C                   Z-ADD     38.087        MEDCON            6 4
     C                   Z-ADD     52.83         TDICV1            4 2
     C                   Z-ADD     66.32         TDICV2            4 2
     C*
     C     CAFDAT        WHENLT    1100101
     C                   Z-ADD     36.0666       MEDCON
     C                   Z-ADD     53.68         TDICV1
     C                   Z-ADD     67.38         TDICV2
     C*** BEG ADD *** 01/12/10 ****************************************
     C*
     C     CAFDAT        WHENLT    1110101
     C                   Z-ADD     36.0846       MEDCON
     C                   Z-ADD     54.32         TDICV1
     C                   Z-ADD     68.19         TDICV2
     C*** END ADD *** 01/12/10 ****************************************
     C*
     C                   ENDSL
     C*** END ADD *** 03/24/09 ****************************************
     C*** Beg Add *** 02/26/10 ****************************************
     C     CAFDAT        IFGE      1100101
     C                   Z-ADD     CAFDAT        KKEFDT
     C                   MOVEL     'M'           KKTYPE
     C*** Beg Chg *** 07/01/10 ****************************************
     C*********  RTKY1     CHAINPRT01                99
     C*********  RTKY2     CHAINPRT01                99
     C     RTKY1         SETGT     PRT01                              99
     C     RTKY2         READPE    PRT01                                  99
     C*** End Chg *** 07/01/10 ****************************************
     C     *IN99         DOUEQ     *ON
     C     RTSTAT        IFNE      'I'
     C                   Z-ADD     RTCONV        MEDCON
     C                   Z-ADD     RTMUL1        TDICV1
     C                   Z-ADD     RTMUL2        TDICV2
     C                   LEAVE
     C                   ELSE
     C     RTKY2         READPE    PRT01                                  99
     C                   ENDIF                                                  STSTAT <> 'I'
     C                   ENDDO                                                  *IN99 <> '1'
     C                   ENDIF                                                  CAFDAT  >= 1100101
     C*** End Add *** 02/26/10 ****************************************
     C*
     C*** Beg Add *** 07/13/10 ****************************************
     C     CACPT4        IFGE      '00100'
     C     CACPT4        ANDLE     '01999'
     C     CAFDAT        ANDGE     1100101
     C                   MOVE      DBRRN         @@RRN
     C                   MOVE      'A'           @@CALL
     C                   MOVE      *ZEROS        @@ALW1
     C                   MOVE      *ZEROS        @@ALW2
     C                   MOVE      *ZEROS        @@ALW3
     C                   MOVE      *BLANKS       @@RTCD
     C                   MOVEL     'M'           @@RND
     C                   MOVE      'Y'           @@MODR
     C                   CALL      'FA763R'      ANESPL
     C     @@RTCD        IFEQ      ' '
     C                   MOVE      @@ALW1        XXMRAB
     C                   ENDIF
     C                   ELSE
     C*** End Add *** 07/13/10 ****************************************
     C*** BEG CHG *** 03/24/09 ****************************************
     C*********  XXMRAB    MULT 38.087    XXMRAH    H
     C     XXMRAB        MULT(H)   MEDCON        XXMRAH
     C*** END CHG *** 03/24/09 ****************************************
     C*
     C     DHKY          CHAIN     PDH01                              99
     C*** Beg Del *** 07/01/10 ****************************************
     C******     CACPT4    IFGE '10000'
     C******     CACPT4    ANDLE'69999'
     C******     DHPOS     IFEQ ' 11'
     C*** End Del *** 07/01/10 ****************************************
     C*** Beg Add *** 07/01/10 ****************************************
     C                   Z-ADD     0             XXMRAB
     C                   MOVEL     DHMOD1        MOD(1)
     C                   MOVEL     DHMOD2        MOD(2)
     C                   MOVEL     DHMOD3        MOD(3)
     C                   MOVEL     DHMOD4        MOD(4)
     C*** calculating XXMRAB based on FTRVP(prcd/mod/efdt) and FTGPP
     C*** (st/loc/efdt) + place of service.
     C*** Locality and state would be retrieved from EOFLIC.
     C                   MOVEL     CACPT4        KKPRCD
     C*
     C     CACPT4        IFEQ      '99455'
     C     CACPT4        OREQ      '99456'
     C*
     C     'V1'          LOOKUP    MOD                                    99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  '99211'       KKPRCD
     C                   ELSE
     C     'V2'          LOOKUP    MOD                                    99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  '99212'       KKPRCD
     C                   ELSE
     C     'V3'          LOOKUP    MOD                                    99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  '99213'       KKPRCD
     C                   ELSE
     C     'V4'          LOOKUP    MOD                                    99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  '99214'       KKPRCD
     C                   ELSE
     C     'V5'          LOOKUP    MOD                                    99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  '99215'       KKPRCD
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  CACPT4 =99455/456
     C*** Lookup for non blank modifier
     C                   MOVE      '  '          KKMOD
     C     'TC'          LOOKUP    MOD                                    99
     C   99              MOVE      'TC'          KKMOD
     C     '26'          LOOKUP    MOD                                    99
     C   99              MOVE      '26'          KKMOD
     C*
     C* Get the locality for 5 digit zip code
     C*
     C                   Z-ADD     CAFDAT        KKEFDT
     C                   MOVEL     EOFLIC        KKFZCD
     C                   MOVE      ' 99'         KKLOC
     C     ZRKY          SETGT     PZR01                              99
     C                   READP     PZR01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     ZREFDT        Z1EFDT
     C*
     C     KKFZCD        IFGE      ZRFZCD
     C     KKFZCD        ANDLE     ZRTZCD
     C     ZREFDT        ANDEQ     Z1EFDT
     C*
     C                   MOVE      ZRRGN#        KKLOC
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C                   READP     PZR01                                  99
     C*
     C                   ENDDO
     C*** Beg Add *** 02/15/16 ********************************************
     C*
     C* Get the locality for 9 digit zip code
     C*
     C                   if        %subst(EOflic:6:4) > '0000'
     C*
     C                   eval      xxloc = getZip9Loc(EOflic:EOast:CAfDat)
     C*
     C                   if        xxloc <> 0
     C                   eval      kkloc = xxloc
     C                   Endif
     C*
     C                   Endif
     C*** End Add *** 02/15/16 ********************************************
     C*
     C                   MOVEL     EOAST         KKST
     C*
     C     GPKY1         SETGT     PGP01
     C     GPKY2         READPE    PGP01                                  99
     C     *IN99         IFEQ      *OFF
     C     RVKY1         SETGT     PRV01
     C     RVKY2         READPE    PRV01                                  99
     C     *IN99         IFEQ      *OFF
     C*** Beg Chg *** 07/20/11 ****************************************
     C********   GPWKGP    MULT RVWKRV    XXAMT1 113H
     C     GPWKGP        MULT(H)   RVWKRV        XXAMT1           11 4
     C*** End Chg *** 07/20/11 ****************************************
     C*** Beg Add *** 03/07/11 ****************************************
     C*** Calling SR to decide whether POS should pay facility or non fac
     C                   EXSR      SRFAC
     C*** End Add *** 03/07/11 ****************************************
     C*** Beg Chg *** 03/07/11 ****************************************
     C*******    DHPOS     IFEQ ' 11'
     C     XXFAC         IFEQ      'N'
     C*** End Chg *** 03/07/11 ****************************************
     C*** Beg Chg *** 07/20/11 ****************************************
     C********   GPPEGP    MULT RVTNPE    XXAMT2 113H
     C     GPPEGP        MULT(H)   RVTNPE        XXAMT2           11 4
     C*** End Chg *** 07/20/11 ****************************************
     C*** Beg Add *** 02/09/13 ****************************************
     C     CAFDAT        IFGE      1130101
     C     GPPEGP        MULT(H)   RVFNPE        XXAMT2           11 4
     C                   ENDIF
     C*** End Add *** 02/09/13 ****************************************
     C                   ELSE
     C     GPPEGP        MULT(H)   RVTFPE        XXAMT2
     C*** Beg Add *** 02/09/13 ****************************************
     C     CAFDAT        IFGE      1130101
     C     GPPEGP        MULT(H)   RVFFPE        XXAMT2           11 4
     C                   ENDIF
     C*** End Add *** 02/09/13 ****************************************
     C                   ENDIF                                                  dhpos = ' 11'
     C*** Beg Chg *** 07/20/11 ****************************************
     C********   GPMPGP    MULT RVMPRV    XXAMT3 113H
     C     GPMPGP        MULT(H)   RVMPRV        XXAMT3           11 4
     C*** End Chg *** 07/20/11 ****************************************
     C     XXAMT1        ADD(H)    XXAMT2        XXMRA4           11 4
     C     XXAMT3        ADD(H)    XXMRA4        XXMRA4
     C     XXMRA4        MULT(H)   RVCONV        XXMRAB
     C                   ENDIF                                                  *in99 = '0'
     C                   ENDIF                                                  *in99 = '0'
     C*** End Add *** 07/01/10 ****************************************
     C*** Beg Del *** 07/01/10 ****************************************
     C*** BEG CHG *** 03/24/09 ****************************************
     C********   XXMRAH    DIV  52.83     XXMRAB    H
     C*********  XXMRAH    DIV  TDICV1    XXMRAB    H
     C*** END CHG *** 03/24/09 ****************************************
     C*********            ELSE
     C*** BEG CHG *** 03/24/09 ****************************************
     C********   XXMRAH    DIV  66.32     XXMRAB    H
     C*********  XXMRAH    DIV  TDICV2    XXMRAB    H
     C*** END CHG *** 03/24/09 ****************************************
     C*********            ENDIF
     C*********            ELSE
     C*** BEG CHG *** 03/24/09 ****************************************
     C*********  XXMRAH    DIV  52.83     XXMRAB    H
     C*********  XXMRAH    DIV  TDICV1    XXMRAB    H
     C*** END CHG *** 03/24/09 ****************************************
     C*********            ENDIF
     C*** End Del *** 07/01/10 ****************************************
     C*** Beg Add *** 07/13/10 ****************************************
     C                   ENDIF
     C*** End Add *** 07/13/10 ****************************************
     C                   ENDSR
     C*** End Add ******* 04/11/08 *******************************************
     C*** Beg Add *** 03/07/11 ****************************************
     C*** Calling SR to decide whether POS should pay facility or non fac
     C     SRFAC         BEGSR
     C                   MOVEL     'N'           XXFAC             1
     C     DHPOS         IFNE      ' 01'
     C     DHPOS         ANDNE     ' 03'
     C     DHPOS         ANDNE     ' 04'
     C     DHPOS         ANDNE     ' 09'
     C     DHPOS         ANDNE     ' 11'
     C     DHPOS         ANDNE     ' 12'
     C     DHPOS         ANDNE     ' 13'
     C     DHPOS         ANDNE     ' 14'
     C     DHPOS         ANDNE     ' 15'
     C     DHPOS         ANDNE     ' 16'
     C     DHPOS         ANDNE     ' 17'
     C     DHPOS         ANDNE     ' 20'
     C     DHPOS         ANDNE     ' 25'
     C     DHPOS         ANDNE     ' 32'
     C     DHPOS         ANDNE     ' 33'
     C     DHPOS         ANDNE     ' 49'
     C     DHPOS         ANDNE     ' 50'
     C     DHPOS         ANDNE     ' 54'
     C     DHPOS         ANDNE     ' 55'
     C     DHPOS         ANDNE     ' 57'
     C     DHPOS         ANDNE     ' 60'
     C     DHPOS         ANDNE     ' 62'
     C     DHPOS         ANDNE     ' 65'
     C     DHPOS         ANDNE     ' 71'
     C     DHPOS         ANDNE     ' 72'
     C     DHPOS         ANDNE     ' 81'
     C     DHPOS         ANDNE     ' 99'
     C                   MOVEL     'Y'           XXFAC             1
     C                   ENDIF                                                  XXFAC = 'Y'
     C                   ENDSR
     C*** End Add *** 03/07/11 ****************************************
**  TABLE QCA
OVRDBF FILE(FTABL)   TOFILE(*LIBL/FTABL  ) OVRSCOPE(*CALLLVL)
DLTOVR FILE(FTABL) LVL(*)
OVRDBF FILE(FMZRL)   TOFILE(*LIBL/FMZRL  ) OVRSCOPE(*CALLLVL)
DLTOVR FILE(FMZRL) LVL(*)
