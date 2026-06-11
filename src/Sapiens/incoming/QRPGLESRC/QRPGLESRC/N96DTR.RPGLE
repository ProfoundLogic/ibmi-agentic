     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE N96DTR - Network Discount Type DT Processing Program
     H*****************************************************************
     H* This pgm is called by N9600R to calcualte the network discount*
     H* amt based on the providers discount code. The discount will   *
     H* only be calculated for the signle sequence number passed to   *
     H* this program.                                                 *
     H*                                                               *
     H*  Called from:                                                 *
     H*    N9600R                                                     *
     H*                                                               *
     H*  Calls  To:                                                   *
     H*    FA763R, FA783R                                             *
     H*                                                               *
     H*****************************************************************
     H* Created on 04/30/2013 by C:277 for Project 147MAS             *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*  07/18/23 C:342 Project: NY060                                *
     H*                 NY Compliance: CARC/RARC Update               *
     H*                                                               *
     H*  05/15/18 C:180 Project: 114MJR                               *
     H*                 Add logic to pay MRA even if > billed amount  *
     H*                                                               *
     H* 01/16/18 C: 079 Support: 114/45847                            *
     H*                 When paying a percent of Fee Schedule or CMS  *
     H*                 allow a match even if the modifier does not   *
     H*                 match.  Consider blank to be all modifiers.   *
     H*                                                               *
     H* 08/21/17 C: 079 Support: 114/45847                            *
     H*                 Remove adjudication EOB code and messages     *
     H*                 Set print flag to Y for network EOB code      *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FNMBDLD    IF   E           K DISK
     FFDCALF    UF   E           K DISK    INFDS(DBFDS)
     FNDCOL3    IF   E           K DISK
     FFTABL     IF   E           K DISK
     FFAEOLG    IF   E           K DISK
     FFDDEL3    UF A E           K DISK
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     F*** Beg Chg *** 08/21/17 ***********************************
     F***FDMGL6    O    E             DISK
     FFDMGL6    UF A E           K DISK
     F*** End Chg *** 08/21/17 ***********************************
     FFDDHL     IF   E           K DISK
     FFACAL1    IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     FFTRTL2    IF   E           K DISK
     FFDFRL0    IF   E           K DISK
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
     D*================================================================
     D* Constants
     D*================================================================
     D XXNOTC          C                   CONST('Network Discount Ap-
     D                                     plied')
     D*================================================================
     D* Stand Alone variables
     D*================================================================
     D @@DSC1          s             70
     D @@DSC2          s             70
     D @@DSC3          s             70
     D @@DSC4          s             70
     D @@TEXT          s             70
     D QQST            s              2
     d*** Beg Add *** 01/16/18 **************************************
     d isFoundCode     s              1
     d*** End Add *** 01/16/18 **************************************
      **** Begin Add *** 05/15/18 *************************************
     D CltId           S              3A   INZ
     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d                                3
      **** End   Add *** 05/15/18 *************************************
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
     C                   PARM                    @@MODE            1
     C                   PARM                    @@ALOW
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
     C*
     C     RTKY1         KLIST
     C                   KFLD                    QQST
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKEFDT
     C*
     C     RTKY2         KLIST
     C                   KFLD                    QQST
     C                   KFLD                    KKTYPE
     C*
     C*
     C     FRKY1         KLIST
     C                   KFLD                    CAAST
     C                   KFLD                    CARULE
     C                   KFLD                    CAFDAT
     C*
     C     FRKY2         KLIST
     C                   KFLD                    CAAST
     C                   KFLD                    CARULE
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
     C     *LIKE         DEFINE    XXMRAB        SVMRAB
     C     *LIKE         DEFINE    XXMRAB        XMMRAB
     C     *LIKE         DEFINE    XXMRAB        @@ALOW
     C     *LIKE         DEFINE    DHMOD1        SVMOD1
     C     *LIKE         DEFINE    DHMOD2        SVMOD2
     C     *LIKE         DEFINE    DHMOD3        SVMOD3
     C     *LIKE         DEFINE    DHMOD4        SVMOD4
     C     *LIKE         DEFINE    DHMOD1        KKMOD
     C     *LIKE         DEFINE    EOAST         KKST
     C     *LIKE         DEFINE    RTTYPE        KKTYPE
     C*** Beg Add 01/17/18 *********************************
     C     *LIKE         DEFINE    CAPAID        XXPAID
     C*** End Add 01/17/18 *********************************
     C*================================================================
     C* Main Processing
     C*================================================================
     C*
     C* Initialize key values.
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@SUB#        KKSUB#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
     C                   EVAL      KKDSEQ = %Dec(@@DSEQ:5:0)
     C                   MOVEL(P)  @@CODE        KKCODE
      /Free
       //**** Begin Add *** 05/15/18 ***************************************
       GetClientId(CltId);
       //**** End   Add *** 05/15/18 ***************************************
      /End-Free
     C*
     C     CAKY          CHAIN     PCA01                              99
B01  C     *IN99         IFEQ      *OFF
     C*** Beg Add *** 08/21/17 ***************************************
B01  C     CApaid        AndGt     0
     C*** End Add *** 08/21/17 ***************************************
     C     FRKY1         SETGT     PFR01
     C     FRKY2         READPE    PFR01                                  99
     C*
     C     EOKY          CHAIN(N)  PEO01                              99
     C                   Z-ADD     CAEOB#        KKEOB#
     C*
     C* Do not process if one of the following procedure codes or
     C* if the allowance is zero.
     C*
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
     C                   MOVE      *OFF          GOOD1             1
     C                   MOVE      *OFF          GOOD2             1
     C                   MOVE      *OFF          GOOD3             1
     C                   Z-ADD     BDEFDT        KKEFDT
     C                   MOVEL(P)  CACPT4        KK10A1
     C     COKY1         SETGT     PCO01
     C     COKY2         READPE    PCO01                                  99
     C     *IN99         DOUEQ     *ON
     C     KK10A1        IFGE      CO10A1
     C     KK10A1        ANDLE     CO10A2
     C     COSTAT        ANDNE     'I'
     C                   EXSR      CHKMOD
     C                   MOVEL     SVMOD1        DHMOD1
     C                   MOVEL     SVMOD2        DHMOD2
     C                   MOVEL     SVMOD3        DHMOD3
     C     FOUND         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C     COKY2         READPE    PCO01                                  99
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
     C                   SELECT
     C                   WHEN      COPCT1 <> 0
     C*** Beg Chg *** 01/17/18 *********************************************
     C***************    EVAL      CAPAID = (CAPAID * COPCT1)/100
     C                   EVAL      XXPAID = (CAPAID * COPCT1)/100
     C*** End Chg *** 01/17/18 *********************************************
     C*
     C                   WHEN      COPCT2 <> 0
     C*** Beg Chg *** 01/17/18 *********************************************
     C***************    EVAL      CAPAID = (CAPAID * COPCT2)/100
     C                   EVAL      XXPAID = (CAPAID * COPCT2)/100
     C*** End Chg *** 01/17/18 *********************************************
     C*
     C                   OTHER
     C                   EXSR      CALMED
     C*
     C                   ENDSL
      /Free
       //**** Begin Change *** 05/15/18 ************************************
       Select;
       When CltID = '114' and EOast = 'FL';
         // Do nothing
       Other;
       //**** End   Change *** 05/15/18 ************************************
      /End-Free
||   C*
||   C     CAPAID        IFGT      CACHRG
     C*** Beg Chg *** 01/17/18 ************************************
||   C*************      Z-ADD     CACHRG        CAPAID
||   C                   Z-ADD     CACHRG        XXPAID
     C*** End Chg *** 01/17/18 ************************************
     C*** Beg Add *** 01/17/18 ************************************
||   C                   CLEAR                   CAMRA
||   C                   CLEAR                   CANDIS
||   C                   CLEAR                   CAOTHR
||   C                   CLEAR                   CAOTH2
     C*** End Add *** 01/17/18 ************************************
||   C                   ENDIF
      /Free
       //**** Begin Change *** 05/15/18 ************************************
       Endsl;
       //**** End   Change *** 05/15/18 ************************************
      /End-Free
||   C*
     C*** Beg Chg *** 01/17/18 **************************************
||   C**** CACHRG        SUB       CAPAID        CANDIS
||   C****               CLEAR                   CAMRA
||   C****               CLEAR                   CAOTHR
||   C****               CLEAR                   CAOTH2
     C                   If        COPCT2 = 100
||   C     CACHRG        SUB       XXPAID        CAMRA
||   C                   CLEAR                   CANDIS
||   C                   CLEAR                   CAOTHR
||   C                   CLEAR                   CAOTH2
     C                   ELSE
||   C     CAPAID        SUB       XXPAID        CANDIS
||   C                   EVAL      CAPAID = XXPAID
     C                   ENDIF
     C*** End Chg *** 01/17/18 **************************************
     C                   ELSE
     C*** Beg Del *** 01/16/18 ***********************************
     C**************
     C**************     Z-ADD     CACHRG        CANDIS
     C**************     CLEAR                   CAPAID
     C**************     CLEAR                   CAMRA
     C**************     CLEAR                   CAOTHR
     C**************     CLEAR                   CAOTH2
     C*** End Del *** 01/16/18 ***********************************
     C*** Beg Add *** 01/16/18 ***********************************
     C*
     C* If the code is found then apply the discount even when the code and
     C* modifier combination were not found
     C                   EVAL      *IN99 = *OFF
     C                   EVAL      ISFOUNDCODE = 'N'
     C*
     C     COKY1         SETGT     PCO01
     C     COKY2         READPE    PCO01                                  99
     C*
     C     *IN99         DOUEQ     *ON
     C*
     C     KK10A1        IFGE      CO10A1
     C     KK10A1        ANDLE     CO10A2
     C     COSTAT        ANDNE     'I'
     C*
     C                   EVAL      ISFOUNDCODE = 'Y'
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C     COKY2         READPE    PCO01                                  99
     C*
     C                   ENDDO
     C*
     C                   IF        ISFOUNDCODE = 'Y'
     C*
     C* Initialize values
     C*
     C                   EXSR      INZVAL
     C*
     C                   SELECT
     C                   WHEN      COPCT1 <> 0
     C                   EVAL      XXPAID = (CAPAID * COPCT1)/100
     C*
     C                   WHEN      COPCT2 <> 0
     C                   EVAL      XXPAID = (CAPAID * COPCT2)/100
     C*
     C                   OTHER
     C                   EXSR      CALMED
     C*
     C                   ENDSL
      /Free
       //**** Begin Change *** 05/15/18 ************************************
       Select;
       When CltID = '114' and EOast = 'FL';
         // Do nothing
       Other;
       //**** End   Change *** 05/15/18 ************************************
      /End-Free
||   C*
||   C     XXPAID        IFGT      CACHRG
||   C                   Z-ADD     CACHRG        XXPAID
||   C                   CLEAR                   CAMRA
||   C                   CLEAR                   CANDIS
||   C                   CLEAR                   CAOTHR
||   C                   CLEAR                   CAOTH2
||   C                   ENDIF
      /Free
       //**** Begin Change *** 05/15/18 ************************************
       Endsl;
       //**** End   Change *** 05/15/18 ************************************
      /End-Free
||   C*
     C                   If        COPCT2 = 100
||   C     CACHRG        SUB       XXPAID        CAMRA
||   C                   CLEAR                   CANDIS
||   C                   CLEAR                   CAOTHR
||   C                   CLEAR                   CAOTH2
     C                   ELSE
||   C     CAPAID        SUB       XXPAID        CANDIS
||   C                   EVAL      CAPAID = XXPAID
     C                   ENDIF
     C*
     C*
     C* What eob code do we use?
     C*
     C     CAAST         IFEQ      'FL'
     C                   MOVEL(P)  ' 93'         XXEOB
     C                   ELSE
     C                   MOVEL(P)  '398'         XXEOB
     C                   ENDIF
     C                   ENDIF
     C*** End Add *** 01/16/18 ***********************************
     C*
     C* WRITE MESSAGE THAT SAYS NO MATCH FOUND
     C*
     C     Q1PRMS        IFGT      23
     C     @@MODE        ANDEQ     'F'
     C     Q1PRMS        ORGT      23
     C     @@MODE        ANDEQ     'P'
     C     Q1PRMS        ORGT      23
     C     @@MODE        ANDEQ     'T'
     C                   ELSE
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
     C                   ENDIF                                                  Q1PRMS > 23
E05  C                   ENDIF                                                   NDCOP *IN99
     C*
     C* Remove system messages and notes.
     C*
     C*** Beg Del *** 08/21/17 *********************************************
     C****
     C**** Q1PRMS        IFGT      23
     C**** @@MODE        ANDEQ     'F'
     C**** Q1PRMS        ORGT      23
     C**** @@MODE        ANDEQ     'P'
     C**** Q1PRMS        ORGT      23
     C**** @@MODE        ANDEQ     'T'
     C**** FRDIS         OREQ      'Y'
     C**** CAPAID        ANDEQ     0
     C****               ELSE
     C****
     C****               EXSR      REMMSG
     C*** End Del *** 08/21/17 *********************************************
     C*** Beg Add *** 08/21/17 *********************************************
     C*** Beg Chg *** 01/17/18 *********************************************
     C***                if        CAnDis > 0
     C                   if        CAnDis > 0 or isFoundCode = 'Y'
     C                             or Found = *on
     C*** End Chg *** 01/17/18 *********************************************
     C                   Exsr      remMsg
     C*** End Add *** 08/21/17 *********************************************
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
     C                   MOVEL     XXNOTC        XXNOT1           70
     C                   MOVE      *BLANKS       XXNOT2           70
      *
     C     CAAST         IFEQ      'FL'
     C*
     C                   MOVE      CACO#         @@CO#
     C                   CALL      'FA783R'
     C                   PARM                    CACO#
     C                   PARM      XXEOB         @@EOB
     C                   PARM      BDCOM1        @@TEXT
     C                   PARM      *BLANKS       @@DSC1
     C                   PARM      *BLANKS       @@DSC2
     C                   PARM      *BLANKS       @@DSC3
     C                   PARM      *BLANKS       @@DSC4
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  @@DSC1        DENOT1
     C                   MOVEL     @@DSC2        DENOT2
     C                   MOVEL     @@DSC3        DENOT3
     C                   MOVEL     @@DSC4        DENOT4
     C                   MOVE      'A'           DESTAT
     C                   MOVE      'N'           DEESRC
     C*** Beg Add *** 08/21/17 ***************************************
     C                   Eval      DEprnt = 'Y'
     C*** End Add *** 08/21/17 ***************************************
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
     C                   ENDIF                                                  Q1PRMS > 23
     C*
     C* Update the detail record.
     C     Q1PRMS        IFGT      23
     C     @@MODE        ANDEQ     'F'
     C     Q1PRMS        ORGT      23
     C     @@MODE        ANDEQ     'P'
     C     Q1PRMS        ORGT      23
     C     @@MODE        ANDEQ     'T'
     C*** Release the record lock on FDCAP
     C                   UNLOCK    FDCALF
     C                   ELSE
     C*
     C                   UPDATE    PCA01
     C                   ENDIF                                                  Q1PRMS > 23
     C*
E04  C                   ENDIF                                                   NMBDP *IN99
E03  C                   ENDIF                                                   CLM STS CD NE 'C'
E01  C                   ENDIF                                                   FDCALF *IN99
     C*
     C     Q1PRMS        IFGT      23
     C                   Z-ADD     XXMRAA        @@ALOW
     C                   ENDIF
     C                   SETON                                        LR
     C*****************************************************************
     C* CHKMOD - Check modifier                                        N T
     C*****************************************************************
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
     C*================================================================***
     C* INZVAL - Initialize values                                     N T
     C*================================================================***
     C     INZVAL        BEGSR
     C*
     C* Initialize work fields
     C*
     C                   MOVE      'FL'          QQST
     C                   MOVE      *OFF          RULE              1
     C                   Z-ADD     0             ALLOWP            5 3
     C                   Z-ADD     0             XXMRAB            9 2
     C                   Z-ADD     0             XXMRAA            9 2
     C                   Z-ADD     0             XXDIFF            9 2
     C                   Z-ADD     0             XXMRAH           11 4
     C*
     C* Calcualte the % of CMS
     C*
     C     COFEE         DIV(H)    100           XXFEE             9 4
     C*
     C* What eob code do we use?
     C*
     C     CAAST         IFEQ      'FL'
     C                   MOVEL(P)  ' 93'         XXEOB
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================***
     C* REMMSG - Remove adjudication messages/EOMB Codes               N T
     C*================================================================***
     C     REMMSG        BEGSR
     C*
     C* Remove adjudication messages from Prov bills errors
     C**** Beg Add *** 08/21/17 *************************************
     C     MGKY          CHAIN     PMG01                              99
     C     *IN99         DOWEQ     *OFF
     C     MGSRC         IFEQ      'S'
     C                   MOVE      MGMSG#        XXMSG#            5
     C     XXMSG#        IFNE      '00003'
     C                   DELETE    PMG01
     C                   ENDIF
     C                   ENDIF                                                  SRC = 'S'
     C     MGKY          READE     PMG01                                  99
     C                   ENDDO                                                  *IN99 = Off
     C**** End Add *** 08/21/17 *************************************
     C*
     C* Remove adjudication EOMB codes from Detail EOB/NOTES
     C*
     C     DEKY          CHAIN     PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C     DEESRC        IFEQ      'A'
     C     DEDSEQ        ANDEQ     CADSEQ
     C*** Beg Del *** 08/21/17 ******************************************
     C***  DEEOB         ANDEQ     'W1'
     C*** End Del *** 08/21/17 ******************************************
     C                   DELETE    PDE01
     C                   ENDIF                                                  ESRC = 'A'
     C     DEKY          READE     PDE01                                  99
     C                   ENDDO                                                  *IN99 = Off
     C*
     C                   ENDSR
     C*****************************************************************
     C* CALMED -  Calculate the medicare
     C*****************************************************************
     C     CALMED        BEGSR
     C*
|-   C     CORATE        IFNE      *ZEROS
||   C*
     C*** Beg Chg *** 01/17/18 *************************************
     C***  CAUNTS        MULT(H)   CORATE        CAPAID
||   C***  CACHRG        SUB       CAPAID        CANDIS
     C     CAUNTS        MULT(H)   CORATE        XXPAID
||   C     CACHRG        SUB       XXPAID        CANDIS
     C*** End Chg *** 01/17/18 *************************************
     C*** Beg Del *** 01/17/18 *************************************
|    C****               CLEAR                   CAMRA
|    C****               CLEAR                   CAOTHR
|    C****               CLEAR                   CAOTH2
     C*** End Del *** 01/17/18 *************************************
     C     CAUNTS        MULT(H)   CORATE        XXMRAA
||   C*
|-   C                   ELSE
||   C*
     C     CACPT4        IFEQ      '97545'
     C     CACPT4        OREQ      '97799'
     C     CACPT4        OREQ      '97546'
     C                   Z-ADD     CAMRAA        XXMRAB
     C                   ENDIF
     C****If service date is less than 03/01/08, divide by 1.25
     C****else call a subroutine to calculate the base Medicare Allowance.
     C     CACPT4        IFGE      'A4200'
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
     C                   DIV       1.25          XXMRAB
     C                   ELSE
     C*
     C     CACPT4        IFGE      '00100'
     C     CACPT4        ANDLE     '01999'
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
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C     XXMRAB        MULT(H)   XXFEE         XXMRAA
     C*
     C                   ENDSR
