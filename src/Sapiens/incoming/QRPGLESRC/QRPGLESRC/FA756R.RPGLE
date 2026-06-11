     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H*****************************************************************
     H*  CREATED BY                                                   *
     H*                                                               *
     H*  04/07/08  C:189 Project: TX039                               *
     H*                                                               *
     H*  This program will calculate the DRG allownece and will       *
     H*  return 'F' if the records in files is not found and  will    *
     H*  return a '*' if the data is insuffeciant to calculate the    *
     H*  the allowence                                                *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/30/24 C:079 Project: FS680                                 *
     H*                Correct EHR Reduction Logic                    *
     H*                                                               *
     H* 10/26/23 C:333 Project: FS661                                 *
     H*                Medicare 2023 Q4 update (Recompile)            *
     H*                                                               *
     H* 07/18/23 C:342 Project: NY060                                 *
     H*                NY Compliance: CARC/RARC Update                *
     H*                                                               *
     H* 03/30/23 C:342 Project: FS658                                 *
     H*                Medicare 2023 Q1 update                        *
     H*                                                               *
     H* 12/01/22 C:342 Project: FS626                                 *
     H*                Medicare 2022 Q4 update                        *
     H*                                                               *
     H* 11/09/20 C:295 Project: FS577                                 *
     H*                Medicare 2020 Q4 update                        *
     H*                                                               *
     H* 02/22/19 C:079 Project: WV018                                 *
     H*                Initialize Return Code                         *
     H*                                                               *
     H* 11/05/18 C:079 Project: FS547                                 *
     H*                Load service date from admission date          *
     H*                                                               *
     H* 10/31/18 C:274 Project: FS535                                 *
     H*                Add HAC Adjustment.                            *
     H*                C:079 - Add Low volume percent to calc         *
     H*                                                               *
     H* 01/04/18 C:180 Project: FS514                                 *
     H*                Change Low Volume Adjustment to 25%            *
     H*                                                               *
     H* 12/14/17 C:180 Project: FS514                                 *
     H*                Change Low Volume Adjustment formula in        *
     H*                LoVolume subroutine                            *
     H*
     H* 05/05/17 C:079 Project: FS511                                 *
     H*                Use table to pay low volume adjustment         *
     H*
     H* 04/06/17 C:079 Project: FS490                                 *
     H*                Change EHR conditions                          *
     H*                Change Capital DSH condition                   *
     H*
     H* 10/26/16 C:309 Project: FS490                                 *
     H*                Load new field values in Labor/Non-labor       *
     H*                percentage                                     *
     H*
     H* 05/18/15 C:212 Project: FS466                                 *
     H*                 Load rate from provider specific file rather  *
     H*                 uncomp. care file in some scenario            *
     H*
     H* 11/13/14 C:212 Project: FS448                                 *
     H*                 DSH changes                                   *
     H*                 Uncompensated Care payment changes            *
     H*
     H* 04/14/14 C:079 Support: MC099 85317                           *
     H*                 Change HSP to be correct                      *
     H*                 Change DSH to be correct                      *
     H*
     H* 03/07/14 C:274 Project: FS427                                 *
     H*                 Medicare 2013 Q4 Update.                      *
     H*                                                               *
     H*                                                               *
     H* 01/30/14 C:079 Project: FS426                                 *
     H*                 Change out logical used for prov file         *
     H*                 Change VBP logic
     H*                 Change Low Volume Logic
     H*
     H* 01/02/14 C:296 Project: FS426                                 *
     H*                 Adding four neew fields and polulating value. *
     H*                 1) New tech amt (CANTEC)                      *
     H*                 2) Value Based Purchase Adjustment (CAVBPA)   *
     H*                 3) Readmit Adjustment (CAHRRA)                *
     H*                 4) Low volume Adjustment (CALVOL)             *
     H*                                                               *
     H* 11/19/12 C:277 Project: FS407                                 *
     H*                 MDH program expires to the end of 2012, so    *
     H*                 no need to update HSP amount in PSF for MDH.  *
     H*                                                               *
     H* 06/21/12 C:277 Project: 114MFI                                *
     H*                 Add 100 Line when state is HL                 *
     H*                                                               *
     H* 02/25/12 C:251 Project: FS404                                 *
     H*                 Medicare 2012 Q1 Update. Use straight         *
     H*                 multipliers instead of dividing by 100        *
     H*                                                               *
     H* 11/28/11 C:280 Support: 78620                                 *
     H*                fields not rounding up causing miss match on   *
     H*                totals between programs                        *
     H*                                                               *
     H* 11/07/11 C:277 Project: FS385                                 *
     H*                Medicare Q4 updates to use FTRTP to get values *
     H*                                                               *
     H* 11/18/10 C:227 Project: FS366                                 *
     H*                Medicare Q4 updates to use FTRTP to get values *
     H*                                                               *
     H* 10/01/10 C:198 Project: WV009                                 *
     H*                Changed to check for medicare release level    *
     H*                                                               *
     H* 10/12/10 C:189 Support: 74497
     H*                fixed decimal data error
     H*                                                                                    *
     H* 09/06/10 C:220 Project: FS365                                                      *
     H*                Don't use PFWIND for the wage index for discharge    file FTOPP.    *
     H*                date on or after 10/1/2010                                          *
     H*                                                                                    *
     H* 08/12/10 C:198 Proj : 114MWS                                  *
     H*                Adjust to process Medicare PPO calculations.  *
     H*                                                               *
     H* 03/03/10 C:079 Proj : FS363                                   *
     H*                Adjust Capital IME Calculations                *
     H*                                                               *
     H* 03/03/10 C:220 Proj : FS363                                   *
     H*                Criteria 1,2,3 and 4 calculation correction    *
     H*                                                               *
     H* 11/09/09 C:220 Proj : FS345                                   *
     H*                Medicare 2009 Q2 Update                        *
     H*                                                               *
     H* 01/15/09 C:189 Proj : FS329                                   *
     H*                CMS IPPS/OPPS Update                           *
     H*                                                               *
     H*                                                               *
     H* 11/21/08 C:079 Support: 65778                                 *
     H*                Correct the Fed Oper Payment with IME/DSH      *
     H*                                                               *
     H* 11/14/08 C:189 Support: 65565                                 *
     H*                Allow DRG payment to be calculated if          *
     H*                EUADDT <= Termination Date  when Termination   *
     H*                Date is > 0                                    *
     H*                                                               *
     H* 11/10/08 C:079 Support: 65387                                 *
     H*                Load Rev Code 100 into DUBPRC                  *
     H*                                                               *
     H* 10/21/08 C:189 Support: 65387                                 *
     H*                EOCHRG is not loaded when the bill is added so *
     H*                we cant rely on this value, it must be calced  *
     H*                                                               *
     H*                                                               *
     H* 10/17/08 C:079 Support: 65280                                 *
     H*                EOCHRG is not loaded when the bill is added so *
     H*                we cant rely on this value, it must be calced  *
     H*                                                               *
     H* 10/15/08 C:189 Support: 65280                                 *
     H*                Remove all Capital rates from calculation if   *
     H*                its a new hospital                             *
     H*                                                               *
     H* 09/23/08 C:189 Support: 65151                                 *
     H*                Implants 'I' will be multiplied by 143%        *
     H*                                                               *
     H* 09/24/08 C:189 Support: 65195                                 *
     H*                New adjudication code 'DRG' handling added     *
     H*                                                               *
     H* 09/12/08 C:198 PROJECT: TX039                                 *
     H*                Eliminated the case where it might divide by   *
     H*                by zero.                                       *
     H*                                                               *
     H* 08/08/08 C:079 PROJECT: TX039                                 *
     H*                Uncomment Outlier Changes                      *
     H*                                                               *
     H* 08/05/08 C:189 PROJECT: TX039                                 *
     H*                Use PFFFDT instead of PFEFDT                   *
     H*****************************************************************
     Fftpfl2    if   e           k disk
     Fftdgl     if   e           k disk
     Ffaeop     uf   e             disk
     Ffdcal     if   e           k disk
     Ffddal     if   e           k disk
     Ffddul3    Uf A e           k disk
     Ffaeul     if   e           k disk
     Ffdcalf    IF A E           K DISK
     F                                     RENAME(PCA01:PCA0F)
     Ffdcal19   IF   E           K DISK
     F                                     RENAME(PCA01:PCA19)
     Ffdcal24   UF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     Fftrtl2    IF   E           K DISK
     Fftabl     if   e           k disk    usropn
     Fftmil     if   e           k disk
     FFDMGL6    UF A E           K DISK
     FFDDEL3    UF A E           K DISK
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     Fnmbdld    if   e           k disk
     F* Add wage files
     FFTI2L1    IF   E           k DISK
     FFTA4L1    IF   E           k DISK
     FFTB4L1    IF   E           k DISK
     FFAIP14L1  IF   E           k Disk
     Fftuncl    if   e           k disk
     D*
     D                 ds
     D qqcmd                         56
     D  qqst                          2    overlay(qqcmd:38)
     D*
     D qca             s             56    dim(2) ctdata perrcd(1)
     *
     D psds          esds                  extname(szq1p)
     *
     D                 ds
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     *
     Dxxbasrt          s              9  2
     Dxxadjbrt         s              9  2
     Dmed_ad           s             11  4
     DAvg_diem         s             14  4
     Dxxttlpmt         s             14  4
     DTrns_pmt         s             14  4
     Dpr_dm_pmt        s             14  4
     D*Total_Payment    s             14  4
     DTotal_Payment    s             14  2
     DIMECAP           s             14  4
     DIMEOPER          s             14  4
     DDSHPER           s             14  4
     Dxxcode           s             11
     D@@adt            s                   like(dgefdt)
     DCOLA             s                   like(PFCOLA)
     D@@car            s                   like(pfprov)
     D@@drg#           s                   like(dgdrg#)
     Dxxchrg           s                   like(cachrg)
     D@@chrg           s                   like(cachrg)
     Dxxdsh            s             14  4
     Dxxdshc           s             14  4
     Dxxdsho           s             14  4
     DO_P_Rate         s             14  4
     DC_P_Rate         s             14  4
     DState_Mult_1     s                   like(RTMUL1)
     DState_Mult_2     s                   like(RTMUL2)
     DOutl_flg         s                   like(RTOTOF)
     Dxxneut1          s                   like(RTNEUT)                          Nutrality Adjustor
     Dxxneut2          s                   like(RTNEUT)                          Nutrality Adjustor
     Dxxneut3          s                   like(RTNEUT)                          Nutrality Adjustor
     Dxxudfa1          s                   like(RTUDFA)                          Quality Ind
     Dxxudfa2          s                   like(RTUDFA)                          Quality Ind
     Dxxudfa3          s                   like(RTUDFA)                          Quality Ind
     Dxxneut4          s                   like(RTNEUT)                          Nutrality Adjustor
     Dxxudfa4          s                   like(RTUDFA)                          Quality Ind
     Dxxneut5          s                   like(RTNEUT)                          Nutrality Adjustor
     Dxxudfa5          s                   like(RTUDFA)                          Quality Ind
     Dxxefdt           s                   like(RTEFDT)                          Nutrality Adjustor
     Dxxhosup          s             14  4                                       HOS update factor
     Dxxhosrt          s             14  4                                       HOS rate
     Dxxfedrt          s             14  4                                       FED rate
     Dxxhbrt           s             14  4                                       HOS bonus rate
     Dlabor            s             14  4
     DNonLabor         s             14  4
     DL_U_add_on       s              3  2
     DF_L_thsh         s              6  0
     DM_C_Ftr          s              3  2
     Dfopimedsh        s             14  4
     Dfcpimedsh        s             14  4
     DO_Cost           s             14  4
     DC_Cost           s             14  4
     DO_Ccr            s             14  4
     DC_Ccr            s             14  4
     DO_O_thsh         s             14  5
     DC_O_thsh         s             14  5
     DO_pmnt           s             14  4
     Dc_pmnt           s             14  4
     DT_O_pmnt         s             14  4
     Df_o_p            s             14  4
     Df_c_p            s             14  4
     Drural_urban      s              1
     D@@len            s              5  0
     DTotalDPP         s             14  4
     DDSH_OPER_PAF     s             14  4
     DDSH_CAP_PAF      s             14  4
     D##TRAN           s             14  4
     Doper_lab         s             14  4
     Doper_nlab        s             14  4
     Dsmchrg           s             11  2
     Dimeper           s              8  4
     D XXCMIX          s                   like(RTCMIX)
     D XXNEUT6         s                   like(RTNEUT)
     D XXUDFA6         s                   like(RTUDFA)
     DNewTech          s              9S 2 inz(0)
     DValueBasedPart   s              9S 2 inz(0)
     DReAdmitAdj       s              9S 2 inz(0)
     DLowVolumeAdj     s              9S 2 inz(0)
     DxxHACadj         s              9S 2 inz(0)
     DPercent          s              7S 4 inz(0.25)
     DxxUdfa2013       s                   like(RTUdfa)
     D xxEPClmAmt      s                   like(UNEPCLM) inz
     D EPClmAmtInd     s               n   inz(*off)
     D decBudNeuFy19   s                   like(RTneut)
     D decBudNeuFy20   s                   like(RTneut)
     D decBudNeuFy21   s                   like(RTneut)
     D decUpdFacFy19   s                   like(RTudFa)
     D decUpdFacFy20   s                   like(RTudFa)
     D decUpdFacFy21   s                   like(RTudFa)
      //*** Begin Add *** 12/01/22 **********************************
     D decBudNeuFy23   s                   like(RTneut)
     D decUpdFacFy23   s                   like(RTudFa)
      //*** End   Add *** 12/01/22 **********************************

     C     *entry        plist
     C                   parm                    @@rrn             9 0           RRN of FAEOP
     C                   parm                    @@rtcd            1
     C                   parm                    @@allow           9 2
     C                   parm                    @@call            1
     C                   parm                    @@eob
     C                   parm                    @@msg
     C                   parm                    @@code
     *
     C     dtcymd1       plist
     C                   parm                    srdti1            8 0
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C
     C     ifky          klist
     C                   kfld                    @@car
     C                   kfld                    @@bgdt
     *
     C     ifky1         klist
     C                   kfld                    @@car
     *
     C     dgky          klist
     C                   kfld                    @@drg#
     C                   kfld                    @@efdt
     *
     C     duky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrvcd
     C                   kfld                    kkrate
     *
     C     duky#         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrvcd
     *
     C     CAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     CAKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKDSEQ
     C*
     C     CAKY3         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*
     C     RTKY          KLIST
     C                   KFLD                    @@st              2
     C                   KFLD                    @@type            1
     C                   KFLD                    @@efdt            7 0
     C*
     C     RTKY1         KLIST
     C                   KFLD                    @@st              2
     C                   KFLD                    @@type            1
     C     CAKY4         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKLIN#
     C*
     C*
     C     CAKY#         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     BDKY1         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKEFDT
     C*
     C     BDKY2         KLIST
     C                   KFLD                    KKCODE
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
     C     ebky          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    kkkey
     C*
     C     mgky          KLIST
     C                   KFLD                    kkmsg#
     C*
     C     l4ky          klist
     C                   kfld                    @@CBSA
     C                   kfld                    @@efdt            7 0
     C*
     C     l2ky          klist
     C                   kfld                    @@prov#          10
     C                   kfld                    @@efdt
      *
     C     l2ky1         klist
     C                   kfld                    @@prov#          10
      *
     C     l4ky1         klist
     C                   kfld                    @@CBSA
     C     ipky          klist
     C                   kfld                    PFPROV
     C                   kfld                    @@efdt
     C     faky          klist
     C                   kfld                    PFPROV
     C                   kfld                    k1efdt
     C     faky1         klist
     C                   kfld                    PFPROV
      *
     C*
     C     *LIKE         DEFINE    BDCODE        KKCODE
     C     *LIKE         DEFINE    BDCODE        @@CODE
     C     *LIKE         DEFINE    BDEFDT        KKEFDT
     C     *LIKE         DEFINE    EBKEY         KKKEY
     C     *LIKE         DEFINE    EBDESC        XXNOT1
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C**** Beg Chg *** 07/18/23 **************************************
     C**** *LIKE         DEFINE    DEEOB         @@EOB
     C     *LIKE         DEFINE    DEEOBCD       @@EOB
     C**** End Chg *** 07/18/23 **************************************
     C     *LIKE         DEFINE    Mgmsg#        @@msg
     C     *LIKE         DEFINE    Mgmsg#        kkmsg#
     C**** Beg Chg *** 07/18/23 **************************************
     C**** *LIKE         DEFINE    DEEOB         ##eob
     C     *LIKE         DEFINE    DEEOBCD       ##eob
     C**** End Chg *** 07/18/23 **************************************
     C     *LIKE         DEFINE    Mgmsg#        ##msg
     C     *LIKE         DEFINE    BDCODE        ##code
     C     *LIKE         DEFINE    CALIN#        X1LIN#
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    CALIN#        KKLIN#
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *like         define    durvcd        kkrvcd
     C     *like         define    durate        kkrate
     C     *like         define    eoco#         kkco#
     C     *like         define    eosub#        kksub#
     C     *like         define    eofnd         kkfnd
     C     *like         define    eofyr         kkfyr
     C     *like         define    eocase        kkcase
     C     *like         define    eoeob#        kkeob#
     C     *like         define    pfbgdt        @@bgdt
     C     *like         define    pfwcbs        @@cbsa
     C     *like         define    IP14efdt      k1efdt
     *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C     DTCYMD2       PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*Set the pointer for the FAEOP from the RRN
     C*
     C                   movel     *blanks       @@RtCd
     C                   movel     *blanks       ##call
     C                   movel     *blanks       ##eob
     C                   movel     *blanks       ##msg
     C                   movel     *blanks       ##code
     C                   if        %parms > 3
     C                   movel     @@call        ##call            1
     C                   movel     @@eob         ##eob
     C                   movel     @@msg         ##msg
     C                   movel     @@code        ##code
     C                   endif
     C     @@rrn         Chain     peo01                              99
B001 C                   if        *in99 = *on
     C                   Move      'F'           @@rtcd
     C                   else
     C*
     C                   Z-add     eoco#         kkco#
     C                   Z-add     eosub#        kksub#
     C                   Z-add     eofnd         kkfnd
     C                   Z-add     eocase        kkcase
     C                   Z-add     eoeob#        kkeob#
     C                   Z-add     eofyr         kkfyr
     C                   exsr      opnOverride
     C*
     C* Calculate total charged
     C*
     C                   exsr      calc_tot_chrg
     C*
     C*Set the pointer for the FAEUL from the values of FAEOP
     C*
     C     caky1         Chain     peu01                              99
     C                   endif
     C**************************************************************************
     C* MAIN CODE STARTS HERE
     C*
     C                   eval      @@car = eumcar
     C                   Movel     eoast         @@st
     C                   Movel     'I'           @@type
     C                   Movel     euaddt        @@efdt
     C                   eval      k1efdt = euaddt
     C*
     C     rtky          setgt     prt01                              99
     C     rtky1         readpe    prt01                                  99
     C*
     C                   dow       *in99 = *off
     C                   if        rtstat = 'I'
     C     rtky1         readpe    prt01                                  99
     C                   else
     C                   leave
     C                   endif
     C                   enddo
     C                   if        *in99 = *off
     C                   Eval      State_Mult_1 = RTMUL1
     C                   Eval      State_Mult_2 = RTMUL2
     C                   z-add     rtefdt        xxefdt
     C                   if        rtmrel = '1'
     C                   z-add     rtefdt        srdti1
     C                   else
     C                   Z-Add     euaddt        srdti1
     C                   endif                                                  rtmrel = '1'
     C                   endif                                                  *in90 = '0'
     C                   call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         @@bgdt
     C*
     C     ifky          setgt     ptp01                              99
     C     ifky1         readpe    ptp01                                  99
     C                   If        *in99= *off or EOAST = 'HL'
     C**Get the fields EOGAF and EOWIND
     C*
     C                   if        rtmrel = '1'
     C                   z-add     rtefdt        @@efdt
     C                   else
     C                   Movel     euaddt        @@efdt
     C                   endif                                                  rtmrel = '1'
     C*
     C                   movel     PFPROV        @@PROV#
     C                   movel     PFACBS        @@CBSA
     C*
     C                   if        EUDCDT < 1091001
     C*
     C                   z-add     PFSPWI        EOWIND
     C                   Eval(h)   EOGAF = EOWIND ** .6848
     C                   Else
     C*
     C*//*** Begin Chg *** 03/30/23 **********************************
     C******             if        PFSPPY = 'Y' or PFSPPY = '2'
     C                   if        PFSPPY = 'Y' or PFSPPY = '2' or PFSPPY = 'D'
     C*//*** End   Chg *** 03/30/23 **********************************
     C     l2ky          setgt     PI201
     C     l2ky1         readpe    PI201                                  60
     C                   If        *in60 = *off
     C                   z-add     I2CRWI        EOWIND
     C                   endif
     C*
     C                   Eval(h)   EOGAF = EOWIND ** .6848
     C*
     C                   Else
a    C     l4ky1         chain     PA401                              60
a    C                   If        *in60 = *off
     C     l4ky          setgt     PA401
     C     l4ky1         readpe    PA401                                  60
     C                   If        *in60 = *off
     C                   z-add     A4CRWI        EOWIND
     C                   Eval(h)   EOGAF = EOWIND ** .6848
     C                   endif
     C                   Else
     C     l4ky          setgt     PB401
     C     l4ky1         readpe    PB401                                  60
     C                   If        *in60 = *off
     C                   z-add     B4CRWI        EOWIND
     C                   Eval(h)   EOGAF = EOWIND ** .6848
     C                   endif
     C                   endif
     C*
     C                   endif
     C                   endif
     C*Set the pointer for the FTRTP to get the rate values
     C*
     C*
     C*
     C                   if        State_Mult_1 = 0
     C                   if        xxefdt < 1120101
     C                   Z-add     100           State_Mult_1
     C                   else
     C                   Z-add     1             State_Mult_1
     C                   endif
     C                   endif
     C                   if        State_Mult_2 = 0
     C                   if        xxefdt < 1120101
     C                   Z-add     100           State_Mult_2
     C                   else
     C                   Z-add     1             State_Mult_2
     C                   endif
     C                   endif
     C*
     C                   Movel     'MC'          @@st
     C                   Movel     'I'           @@type
     C                   Z-add     euaddt        @@efdt
     C*
     C     rtky          setgt     prt01                              99
     C     rtky1         readpe    prt01
B001 C                   if        *in99 = *off
     C                   Eval      O_P_Rate   = RTOPBR
     C                   Eval      C_P_Rate   = RTCPBR
     C                   Eval      F_L_thsh   = RTOUTL
     C                   Eval      M_C_Ftr    = RTMGFA
     C                   Eval      L_U_add_on = RTURAD
     C*
     C                   endif                                                   record found
     C*
     C* If the value = 0 from the record then default outlier offset to = 1
     C*
     C                   if        RTOTOF = 0
     C*
     C                   Eval      Outl_flg = 1
     C                   else
     C                   Eval      Outl_flg = RTOTOF
     C*
     C                   endif                                              Load Outlier flag
     *
     C                   eval      @@drg#  = eudrg#
 001 C     dgky          setgt     pdg01                              99
 001 C     @@drg#        readpe    pdg01
 001 C
B002 C                   if        *in99 = *off
     *
     * A '0' in these fields repersents insufficeint data
     *
 002 C                   if        dgrelw <> 0 and dgglos <> 0 and dgalos <> 0
     *
     * Check the wage index  for calculating LABOUR PERCENTAGE
     *
     *
 002 C                   if        eowind >= 1.0
 002 C                   eval(h)   labor  = RTOPLB/100
 002 C                   else
 002 C                   eval(h)   labor  = RTOPLW/100
     C                   endif
     *
     * NON LABOUR PERCENTAGE = 100 - lABOUR%
     *
 002 C                   eval(h)   NonLabor  = (1 - labor)
     C*** Beg Del *** 10/30/2024 *******************************************
     C***************
     C***************    If        PFehr <> 'Y' or EUdcDt < 1161001
 002 C***************    If        eowind >= 1.0   and PFHOQU = '1'
     C***************    eval      oper_lab  = RTOPL1
     C***************    eval      oper_nlab = RTOPN1
     C***************    endif
     ***************
 002 C***************    if        eowind <= 1.0   and PFHOQU = '1'
     C***************    eval      oper_lab  = RTOPL2
     C***************    eval      oper_nlab = RTOPN2
     C***************    endif
     ***************
 002 C***************    if        eowind >  1.0   and PFHOQU <> '1'
     C***************    eval      oper_lab  = RTOPL3
     C***************    eval      oper_nlab = RTOPN3
     C***************    endif
     ***************
 002 C***************    if        eowind <= 1.0   and PFHOQU <> '1'
     C***************    eval      oper_lab  = RTOPL4
     C***************    eval      oper_nlab = RTOPN4
     C***************    endif
     C***************    EndIf
     C***************
     C***************
     C***************    If        PFehr = 'Y' and EUdcDt >= 1161001
     C***************
     C***************    If        EOwind >= 1.0   and PFhoqu =  '1'
     C***************    eval      oper_lab  = RTOLNE1
     C***************    eval      oper_nlab = RTONLNE1
     C***************    endif
     C***************
     C***************    If        EOwind >  1.0   and PFhoqu <> '1'
     C***************    eval      oper_lab  = RTOLNE2
     C***************    eval      oper_nlab = RTONLNE2
     C***************    endif
     C***************
     C***************    If        EOwind <= 1.0   and PFhoqu =  '1'
     C***************    eval      oper_lab  = RTOLNE3
     C***************    eval      oper_nlab = RTONLNE3
     C***************    endif
     C***************
     C***************    If        EOwind <= 1.0   and PFhoqu <> '1'
     C***************    eval      oper_lab  = RTOLNE4
     C***************    eval      oper_nlab = RTONLNE4
     C***************    endif
     C***************    endif
     C*** End Del *** 10/30/2024 *******************************************
       //*** Beg Add *** 10/30/2024 ****************************************
       Select;
       // Wage index > 1.  Note PFehr = 'Y' means the provider did not have
       // meaningful Electronic Health Records (EHR)
       When EOwind > 1;
          If PFhoqu = '1' and PFehr <> 'Y';
             oper_Lab  = RTopL1;
             oper_Nlab = RTopN1;
          Endif;
          If PFhoqu = '1' and PFehr = 'Y';
             oper_lab  = RToLnE1;
             oper_nlab = RToNlNe1;
          Endif;
          If PFhoqu <> '1' and PFehr <> 'Y';
             oper_lab  = RTopL3;
             oper_nlab = RTopN3;
          Endif;
          If PFhoqu <> '1' and PFehr = 'Y';
             oper_lab  = RToLnE2;
             oper_nlab = RToNlNe2;
          Endif;
       // Wage index <= 1
       When EOwind <= 1;
          If PFhoqu = '1' and PFehr <> 'Y';
             oper_Lab  = RTopL2;
             oper_Nlab = RTopN2;
          Endif;
          If PFhoqu = '1' and PFehr = 'Y';
             oper_lab  = RToLnE3;
             oper_nlab = RToNlNe3;
          Endif;
          If PFhoqu <> '1' and PFehr <> 'Y';
             oper_lab  = RTopL4;
             oper_nlab = RTopN4;
          Endif;
          If PFhoqu <> '1' and PFehr = 'Y';
             oper_lab  = RToLnE4;
             oper_nlab = RToNlNe4;
          Endif;
       Endsl;
       //*** End Add *** 10/30/2024 ****************************************
     *
 002 C                   eval(h)   COLA      = (.3152 * (Pfcola -1)  +1)
     * Calculate  Federal Operating Payment (Base rate) f_o_p
     *
 002 C                   eval(h)   f_o_p   =  DGRELW * ((oper_lab    *
 002 C                                       eowind) + (oper_nlab    *
 002 C                                       pfcola))
     *
     * Calculate  Federal Capitol Payment (Base rate) f_c_p
     *
 002 C                   eval      f_c_p   = (DGRELW * C_P_rate * COLA *EOGAF
 002 C                                       * L_U_add_on)
     *
     C                   if        pfnew = 'Y'
 002 C                   eval      f_c_p   = 0
     C                   endif
     **************************************************************
     *Now take the Base Rate and multiply with MS-DRG relative wt.*
     *TO get the ADJUSTED BASE RATE                               *
     **************************************************************
 002 C                   eval(h)   xxadjbrt = f_o_p + f_c_p
     *
     *Now calculate the DSH *

     C
     *DSH - Disproportionate Share Add-on = Operating + Capitol
      ///Free
     C                   exsr      caldsh1
      /free
        EPClmAmtInd = *off;
        clear xxEPClmAmt;
        If EUDCDT >= 1131001;
          exsr CalUnCompAmt;
        EndIf;
      /end-free

     *
     **************************************************
     *Now Calculate the IME
     **************************************************
     *
     C                   eval(h)   IMEPER =(1.35*((1+ pfibed)**.405-1))
     C                   eval(h)   IMEOPER =f_o_p*(1.35*((1+ pfibed)**.405- 1))
     C                   eval(h)   IMECAP  =((2.718281828 **(.2822*PFCIME)-1) *
     C                             f_c_p )
     *
     C                   if        EUADDT >= 1081001
     C                             and EUADDT <= 1090930
     C                   eval(h)   IMECAP  = IMECAP/2
     C                   endif
     C
     C                   if        pfnew = 'Y'
     C                   eval      IMECAP  = 0
     C                   endif
     *
     C                   Eval      med_ad =  IMEOPER + IMECAP
     C                   If        med_ad <  0
     C                   Eval      med_ad =  0
     C                   endif
     ****************************************
     *Calculation of Total Outlier payement *
     ****************************************
     *
     * Step 1) to 7) done in D spec inialization
     *
     * 8)  XXCHRG = SMCHRG - any implants that were billed
     C
     C                   Z-add     278           kkrvcd
     C
     C     duky#         setll     pdu01
     C     duky#         reade     pdu01                                  60
     C                   Dow       *in60 = *off
     C                   Eval      kkdseq = dudseq
     C     caky2         chain     PCA0F                              99
     C                   if        *in99 = *off
     C                   eval      @@chrg    = @@chrg +Cachrg
     C                   endif
     C     duky#         reade     pdu01                                  60
     C                   enddo
     C
     C                   eval      XXCHRG = SMCHRG - @@chrg
     *
      * 9)  Federal Operating Payment with IME and DSH
     C                   eval(h)   fopimedsh = DGRELW *(((oper_lab        *
     C                                         eowind) + (oper_nlab       *
     C                                         PFCOLA)) * (1+IMEPER
     C                                          +DSH_OPER_PAF))
     *
      *10)  Federal Capitol Payment with IME an DSH
     C                   eval(h)   FCPIMEDSH =(DGRELW * C_P_Rate * L_U_add_on
     C                                        *EOGAF  *COLA)
     C                                        * (1 + IMECAP + DSH_CAP_PAF)
     *
      *11)  Operating_Cost
     C                   eval(h)   O_Cost    =  XXCHRG * PFOCCR
     *
      *12)  Capital_Cost
     C                   eval(h)   C_Cost    =  XXCHRG * PFCCCR
     *
      *13)  Oper_to total_CCR
     C                   if        C_cost + O_cost <> 0
     C                   eval(h)   O_CCR     =  O_Cost /(C_Cost +O_Cost)
     *
      *14)  Cap_to total_CCR
     *
     C                   eval(h)   C_CCR     =  C_Cost /(C_Cost +O_Cost)
     *
     C                   endif
     *
      *15)  Operating_outlier_Thresh
     *
     C                   Eval(h)   O_O_thsh  =  (F_L_Thsh * ((Labor *eowind) +
     C                             nonlabor*PFCOLA)) * O_CCR +  fopimedsh
     *
      *16)  Capital_Outlier_Thresh
     *
     C                   eval(h)   C_O_thsh  =(F_L_Thsh * EOGAF  * L_U_add_on *
     C                             C_CCR  *COLA)  + fcpimedsh
     *
      *17)  If  (operating_Cost + Capital Costs) > (operating_outlier_Thresh +
     *     Capital_outlier_Thresh then we will have an outlier payment
     *
 002 C                   if        (O_cost + C_cost) > (O_O_thsh +C_O_thsh)
     *
     *18) Operating Payment
     C                   eval(h)   O_pmnt    = ( O_Cost - O_O_thsh) * M_C_Ftr
     *
     *19) Capital_Payment
     C                   Eval      C_pmnt    = (C_Cost - C_O_thsh)*  M_C_Ftr
     C                   If        c_pmnt    < 0
     C                   eval(h)   c_pmnt    = 0
     C                   endif
     *
     C                   if        pfnew = 'Y'
     C                   eval      c_pmnt  = 0
     C                   endif
     *
     *20) Total_Outlier_Payment
     C                   eval(h)   T_O_pmnt  =  O_pmnt + c_pmnt
     C                   endif
     *
     **************************************************
     *Now Check the eligibilty of   TRANSFER PAYEMENT *
     **************************************************
     *
     C                   movel     euscpt        srdate1           7 0
     C                   movel     euscpf        srdate2           7 0
     C                   movel     *off          *in40
     *
     C                   call      'SRDDIF7'
     C                   parm                    srdate2
     C                   parm                    srdate1
     C                   parm      *Zeros        @los              5 0
     C                   parm      *Blanks       srerr             1
    C*
     C                   Eval      @los  = @los + 1
    C*
    C*
     C                   If        (@los < dgglos  and eutrst ='01')  or
     C                             (@los<dgglos and eutrst ='02'and DGPSAC ='Y')
     C                   movel     *on           *in40                          Transfer pmt ind
    C*
     * Now we will have per diem payement
    C*
     C                   eval(h)   Avg_diem = xxadjbrt / dgglos
     C                   If        dgspec <> 'Y'
     C                   eval(h)   pr_dm_pmt= 2 * (Avg_diem) +avg_diem*(@los -1)
     C                   else
     C                   eval(h)   pr_dm_pmt= (.5 * xxadjbrt) + (.5 * Avg_diem) +
     *                                       + .5 ((@los-1) * Avg_diem)
     C                   endif
     *
     C                   endif
     C
     C                   If        *in40 = *on
     C                   If        pr_dm_pmt  >  xxadjbrt
 002 C                   eval      pr_dm_pmt = xxadjbrt
     C                   endif
     C
 002 C                   eval(h)   Total_Payment = pr_dm_pmt + xxdsh + T_O_pmnt
 002 C                             + med_ad  + PFTPAS
     C                   Z-ADD     Total_Payment ##TRAN
 002 C*
     C                   else
 002 C                   eval(h)   Total_Payment = xxadjbrt  + xxdsh + T_O_pmnt
 002 C                             + med_ad  + PFTPAS
 002 C*
     c                   endif

     c** Add Estimated per claim amount to total payment

     c                   eval(h)   total_payment = total_payment + xxEPClmAmt
     *
     C*
     C* Operating Payment HSP Operating Bonus for MDH and SCH
     C*
 002 C                   if        (pftype = '14' or pftype = '15'  or
 002 C                             pftype = '16' or pftype = '17')
 002 C                             and euaddt < 1121001
     C                   exsr      HSP_Op_Bonus
     c                   endif
      /Free
         //*** Operating Payment HSP Operating Bonus for 10/1/2012

         If (PFtype = '16' or PFtype = '17')
            and EuadDt >= 1121001 and EUadDt < 1181001;
         Exsr HSP_Op_Bonus_2013;
         Endif;
         If (PfType = '16' or PfType = '17')
            and EUadDt >= 1181001;
            Exsr HspOpBonus;
         Endif;

      /End-Free
      /Free
           exsr New_Tech ;
           exsr ValueBasedParticipant ;
           exsr ReAdmit ;
           If PFREL = 'Y' and EUDCDT >= 1101001 and F_O_P > 0 ;
              exsr LowVolume ;
           endIf ;
           If PFhred = 'Y';
              exsr HACadj;
           endIf ;
      /End-Free
     C*
 002 C                   if        euimst =  'N' or euimst = 'P' or euimst = 'I'
     C                   if        xxefdt < 1120101
     C                   eval(h)   Total_Payment = (Total_Payment *
     C                             State_Mult_1)/100
     C                   else
     C                   eval(h)   Total_Payment = (Total_Payment *
     C                             State_Mult_1)
     C                   endif
     C                   endif
     *
 002 C                   if        euimst = 'S'
     C                   if        xxefdt < 1120101
     C                   eval(h)   Total_Payment = (Total_Payment *
     C                             State_Mult_2)/100
     C                   else
     C                   eval(h)   Total_Payment = (Total_Payment *
     C                             State_Mult_2)
     C                   endif
     C                   endif
     *
     C                   if        %parms > 3
     C                   movel     @@code        kkcode
     C                   eval      kkefdt = cafdat
     C*
     C     BDKY1         SETGT     PBD01
     C     BDKY2         READPE    PBD01                                  99
     C                   dow       *in99 = *off
     C                   if        bdstat = 'I'  and *in99 = *off
     C     bdky2         readpe    pbd01                                  99
     C                   elseif    bdstat = 'A'
     C                   leave
     C                   endif
     C                   enddo
     C                   if        ##call = 'N' and
     C                             *in99 = *off
     C                   eval      total_payment = total_payment * bdpc01
     C                   z-add     total_payment @@allow
     C                   endif
     C*
     C                   endif                                                  %parms
 002 C                   if        @@rtcd = ' '
     C                   exsr      lodrcd
     C                   endif
     C                   if        %parms > 3
     C                   if        @@call = 'N'
     C                   exsr      wrteob
     C                   exsr      wrtmsg
     C                   endif
     C                   endif                                                  %parms
     C                   exsr      clsOverride
     *
     C                   else
     C                   Move      '*'           @@rtcd
     C                   endif
     *
     C                   else
     C                   Move      'F'           @@rtcd
     C                   endif
     *
     C                   else
     C                   Move      'F'           @@rtcd
     C                   endif
     *
 002 C                   if        @@rtcd =  ' '
     C                   Z-add     Total_Payment @@allow
     C                   endif
     *
     C*
     C* Upadte the FAEOP file to store the EOWIND and EOGAF values
     C*
     C                   TIME                    DSDTTM
     C                   Movel     q1user        eouser
     C                   Movel     q1pgm         eopgm
     C                   Z-add     dstime        eotime
     C                   Z-add     dsdate        srdti
     C                   Call      'SRCVTDAT'    dtcymd
     C                   Movel     srdto         eodt
     C*
     C                   update    peo01
     C*
     C                   seton                                        LR
     C                   return
    C********************************************************************
    C*
    C*  Calculate total charged for this bill
    C*
    C*****************************************************************
     C     calc_tot_chrg begsr
     C*
     C                   eval      smchrg = 0
     C*
     C     caky1         chain(N)  PCA19                              99
     C*
     C                   dow       *in99 = *off
     C*
     C                   eval      smchrg    = cachrg + smchrg
     C*
     C     caky1         reade(N)  pca19                                  99
     C*
     C                   enddo
     C*
     C                   endsr
    C********************************************************************
    C*  Start loading the records in FDDUP & FDCAP
    C*
    C*****************************************************************
     C     lodrcd        begsr
     C                   Movel     '   '         kkrate
     C                   Z-add     100           kkrvcd
     C     duky1         chain     pdu01                              60
     C                   if        *in60 = *off
     *
     C                   Z-add     dudseq        kkdseq
     C     caky#         chain     pca02                              60
     C                   if        *in60 = *off
ks01 C                   if        cacscd  <> 'DRG'
     C                   exsr      updpd
     C                   endif
     C                   endif
     C                   else
     C                   exsr      wrtpd
     C                   endif
     C     lodrcde       endsr
    C********************************************************************
    C*  Write the records in FDCAP and FDDUP
    C*
    C*****************************************************************
     C     wrtpd         begsr
     C*
     C* Get the next available seq# & line#
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *HIVAL        KKDSEQ
     C*
     C     caky2         SETGT     PCA0F
     C     caky3         READPE(N) PCA0F                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     CADSEQ        ADD       1             XXDSEQ
     C                   ELSE
     C                   Z-ADD     1             XXDSEQ
     C                   ENDIF
     C*
     C                   MOVE      *HIVAL        KKLIN#
     C*
     C     CAKY4         SETGT     PCA19
     C     CAKY1         READPE(N) PCA19                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     CALIN#        ADD       1             X1LIN#
     C                   ELSE
     C                   Z-ADD     1             X1LIN#
     C                   ENDIF
     C*
     C                   Z-ADD     EOCO#         CACO#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C*
     C     ##call        IFEQ      'N'
     C     ##call        OREQ      ' '
     C                   MOVEL     '2  '         CACSCD
     C*
     C                   else
     C                   MOVEL     '1  '         CACSCD
     C*
     C                   endif
      /Free
                         If EUadDt <> 0;
                            CAfDat = EUadDt;
                         Else;
                            CAfDat = EOfrDt;
                         Endif;

                         If EUdcDt <> 0;
                            CAtDat = EUdcDt;
                         Else;
                            CAtDat = EOtoDt;
                         Endif;
      /End-Free
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CARDAT
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C                   MOVEL     '     '       CACPT4
     C                   MOVEL     *BLANKS       CAFLAG
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   MOVEL     EOSS#         CASS#
     C                   Z-ADD     EOACDT        CAACDT
     C                   MOVEL     'A'           CASTAT
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     *BLANKS       CADXR
     C                   MOVEL     'D'           CARCFL
     C                   MOVEL     EOSUB#        CASUB#
     C                   Z-ADD     *ZERO         CAOTHR
     C                   Z-ADD     *ZERO         CAOTH2
     C                   Z-ADD     *ZERO         CANDIS
     C                   If        PFTRDT = 0  or  EUADDT <=  PFTRDT
     C                   Z-ADD     Total_Payment CAPAID
     C                   endif
     C                   Z-ADD     *ZEROS        CACHRG
     C                   if        %parms > 3 and @@code <> *blanks
     C     CACHRG        SUB       CAPAID        CANDIS
     C                   z-add     0             camra
     C                   else
     C     CACHRG        SUB       CAPAID        CAMRA
     C                   endif
     C                   MOVEL     EORCN         CARCN
     C                   MOVEL     *BLANKS       CARULE
     C                   MOVEL     *BLANKS       CAOT1C
     C                   MOVEL     *BLANKS       CAOT2C
     C                   If        PFTRDT = 0  or  EUADDT <=  PFTRDT
     C                   Z-ADD     Total_Payment CAMRAA
     C                   endif
     C                   Z-ADD     *ZEROS        CAMNAA
     C                   MOVEL     *BLANKS       CAADJ
     C                   MOVEL     *BLANKS       CABPRC
     C                   Z-ADD     X1LIN#        CALIN#
     C                   Z-ADD     XXDSEQ        CADSEQ
     C                   MOVEL     Q1USER        CAIUSR
     C                   MOVEL     Q1PGM         CAIPGM
     C                   Z-ADD     DSTIME        CAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CAIDT
     C                   Z-ADD     *ZERO         CADIFF
     C*
     C*
     C                   MOVE      'X'           CADELF
     C                   eval      xxcode = '100        '
     C                   Movel     xxcode        cacode
     C                   If        PFTRDT = 0  or  EUADDT <=  PFTRDT
     C*
     C*
     C*                  Z-ADD     f_o_p         CABOPR
     C*                  Z-ADD     f_c_p         CABCAP
     C*                  Z-ADD     IMEOPER       CAOIME
     C*                  Z-ADD     IMECAP        CACIME
     C*                  Z-ADD     xxdsho        CAODSH
     C*                  Z-ADD     xxdshc        CACDSH
     C*                  Z-ADD     T_O_pmnt      CAOUTL
     C*                  Z-ADD     ##TRAN        CATRAN
     C*
     C                   eval(h)   CABOPR = f_o_p
     C                   eval(h)   CABCAP = f_c_p
     C                   eval(h)   CAOIME = IMEOPER
     C                   eval(h)   CACIME = IMECAP
     C                   eval(h)   CAODSH = xxdsho
     C                   eval(h)   CACDSH = xxdshc
     C                   eval(h)   CAOUTL = T_O_pmnt
     C                   eval(h)   CATRAN = ##TRAN
     C                   eval(h)   CAUNCOMP = xxEPClmAmt
     C*
     C*
     C                   endif
     C                   eval(h)    cahspb = xxhbrt
      /Free
        CANTEC = NewTech ;
        CAVBPA = ValueBasedPart ;
        CAHRRA = ReAdmitAdj ;
        CALVOL = LowVolumeAdj ;
        CAhac = xxHACadj;
      /End-Free
     C*
     C                   WRITE     PCA0F
     C*
     C* WRITE RECORD IN FDDUP
     C*
     C                   Z-ADD     EOCO#         DUCO#
     C                   Z-ADD     EOSUB#        DUSUB#
     C                   Z-ADD     EOFND         DUFND
     C                   Z-ADD     EOFYR         DUFYR
     C                   Z-ADD     EOCASE        DUCASE
     C                   MOVEL     CASS#         DUSS#
     C                   Z-ADD     EOACDT        DUACDT
     C                   Z-ADD     CADSEQ        DUDSEQ
     C                   Z-ADD     EOEOB#        DUEOB#
     C                   MOVE      '21'          DUPOS
     C                   Z-ADD     100           DURVCD
     C                   MOVEL     *blanks       DURATE
     C                   MOVEL     EUDGDS        DUDESC
     C                   Z-ADD     1             DUUNTS
     C                   MOVEL     'A'           DUSTAT
     C                   MOVEL     Q1USER        DUUSER
     C                   MOVEL     Q1USER        DUIUSR
     C                   MOVEL     Q1PGM         DUPGM
     C                   MOVEL     Q1PGM         DUIPGM
     C                   Z-ADD     DSTIME        DUTIME
     C                   Z-ADD     DSTIME        DUITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUIDT
     C                   MOVEL     *BLANKS       DUBRAT
     C                   Z-ADD     100           DUBPRC
     C                   MOVEL     EUDGDS        DUBPRD
     C*
     C                   WRITE     PDU01
     C*
     C     wrtpde        endsr
    C********************************************************************
    C*  Update the records in FDCAP and FDDUP
    C********************************************************************
     C     updpd         begsr
     C                   TIME                    DSDTTM
    C*
    C*Update the FDCAP
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     *BLANKS       CADXR
     C                   MOVEL     'D'           CARCFL
     C                   MOVEL     EOSUB#        CASUB#
     C                   Z-ADD     *ZERO         CAOTHR
     C                   Z-ADD     *ZERO         CAOTH2
     C                   Z-ADD     *ZERO         CANDIS
     C                   If        PFTRDT = 0  or  EUADDT <=  PFTRDT
     C                   Z-ADD     Total_Payment CAPAID
     C                   endif
     C                   if        %parms > 3 and @@code <> *blanks
     C     CACHRG        SUB       CAPAID        CANDIS
     C                   z-add     0             camra
     C                   else
     C     CACHRG        SUB       CAPAID        CAMRA
     C                   endif
     C                   If        PFTRDT = 0  or  EUADDT <=  PFTRDT
     C                   Z-ADD     Total_Payment CAMRAA
     C                   Z-ADD     *ZEROS        CAMNAA
     C*
     C*
     C*                  Z-ADD     f_o_p         CABOPR
     C*                  Z-ADD     f_c_p         CABCAP
     C*                  Z-ADD     IMEOPER       CAOIME
     C*                  Z-ADD     IMECAP        CACIME
     C*                  Z-ADD     xxdsho        CAODSH
     C*                  Z-ADD     xxdshc        CACDSH
     C*                  Z-ADD     T_O_pmnt      CAOUTL
     C*                  Z-ADD     ##TRAN        CATRAN
     C*
     C                   eval(h)   CABOPR = f_o_p
     C                   eval(h)   CABCAP = f_c_p
     C                   eval(h)   CAOIME = IMEOPER
     C                   eval(h)   CACIME = IMECAP
     C                   eval(h)   CAODSH = xxdsho
     C                   eval(h)   CACDSH = xxdshc
     C                   eval(h)   CAOUTL = T_O_pmnt
     C                   eval(h)   CATRAN = ##TRAN
     C                   eval(h)   CAUNCOMP = xxEPClmAmt
     C*
     C*
     C                   eval(h)    cahspb = xxhbrt
     C                   endif
      /Free
        CANTEC = NewTech ;
        CAVBPA = ValueBasedPart ;
        CAHRRA = ReAdmitAdj ;
        CALVOL = LowVolumeAdj ;
        CAhac = xxHACadj;
      /End-Free
     C                   Update    PCA02
    C*
     C                   MOVEL     'A'           DUSTAT
     C                   MOVEL     Q1USER        DUUSER
     C                   MOVEL     Q1USER        DUIUSR
     C                   MOVEL     Q1PGM         DUPGM
     C                   MOVEL     Q1PGM         DUIPGM
     C                   Z-ADD     DSTIME        DUTIME
     C                   Z-ADD     DSTIME        DUITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUIDT
     C*
     C                   update    PDU01
     C     updpde        endsr
    C*****************************************************************
    C* Calculate DSH
    C********************************************************************
     C     CALDSH        begsr
     C*   e. Criteria 5 ( 2):
     C
     C                   If        PFODSH = .35
     C                   eval       DSH_OPER_PAF = PFODSH
     C                   endif
     C*
     C*First calculate the DSH for Operating costs
     C                   if        PFsalo <> *blanks
     C                   Eval      @@len = %scan(' ': PFsalo :1)
     C                   Else
     C                   Eval      @@len = %scan(' ': PFACBS :1)
     C                   Endif
     C*
B002 C                   if        @@len <> 1
     C                   Eval      rural_urban  = 'U'
     C                   else
     C                   Eval      rural_urban  = 'R'
     C                   endif
     C*
     C                   Eval      TotalDPP = PFSSI + PFMRAT
    C*
     C*****************************************************
      *a. Criteria 1 ((c)(1)(i):
     C
     C                   If        (TotalDPP >=.15 and Rural_Urban = 'U' and
     C                             PFBEDS >= 100) or (Rural_Urban = 'R' and
     C                             TotalDPP >=.15 and PFBEDS >=  500)
     C
    C*               If the hospitals TotalDPP is >= 20.2%
     C*
     C                   If        TotalDPP >= .202
     C                   Eval      DSH_OPER_PAF=.0588+.825*(TotalDPP-.202)
     C                   endif
     C*
     C                   If        TotalDPP <  .202
     C                   Eval      DSH_OPER_PAF=.025 +.65*(TotalDPP-.15)
     C                   endif
     C*
     C                   endif
     C
     C*****************************************************
     C* b. Criteria 2 (c) (1) (ii):
     C
     C                   If        (TotalDPP >=.15 and Rural_Urban = 'R' and
     C                             PFBEDS > 100 and  PFBEDS < 500) or
     C                             (TotalDPP >=.15 and(PFTYPE = '16' or
     C                             PFTYPE = '17'))
     C
     C                   If        (PFTYPE ='07'or PFTYPE = '16'or PFTYPE ='17')
     C
     C                   If        TotalDPP <= .202
     C                   Eval      DSH_OPER_PAF=.025 +.65 *(TotalDPP-.15 )
     C                   endif
     C
     C                   If        TotalDPP >  .202
     C                   Eval      DSH_OPER_PAF=.0588+.825*(TotalDPP-.202)
     C                   endif
     C
     C                   endif
     C
     C                   If        rural_urban = 'R'  and  PFBEDS >= 100  and
     C                              PFTYPE <> '07'and PFTYPE <> '16'
     C                             or PFTYPE ='17'  or PFTYPE ='22'
     C
     C
     C                   If        TotalDPP <= .202
     C                   Eval      DSH_OPER_PAF=.025 +.65 *(TotalDPP-.15)
     C                   endif
     C
     C                   If        TotalDPP >  .202
     C                   Eval      DSH_OPER_PAF=.0588 +.825  *(TotalDPP-.202)
     C                   endif
     C
     C                   If        DSH_OPER_PAF  > .12
     C                   Eval      DSH_OPER_PAF=.12
     C                   endif
     C                   endif
     C                   endif
     C
     C*****************************************************
     C* c. Criteria 3 ( c ) (1) (iii):
     C
     C
     C                   If        (TotalDPP >= .15 and Rural_Urban = 'U' and
     C                             PFBEDS < 100)
     C
     C                   If        TotalDPP <= .202
     C                   Eval      DSH_OPER_PAF=.025 +.65 *(TotalDPP-.15)
     C                   endif
     C
     C                   If        TotalDPP >  .202
     C                   Eval      DSH_OPER_PAF=.0588 +.825  *(TotalDPP-.202)
     C                   endif
     C
     C                   If        DSH_OPER_PAF  > .12
     C                   Eval      DSH_OPER_PAF=.12
     C                   endif
     C
     C                   endif
     C
     C
     C*****************************************************
     C*  d. Criteria 4 ( c ) (1) (iv):
     C
     C                   If        (TotalDPP >= .15 and Rural_Urban = 'R' and
     C                             PFBEDS <= 100)
    C*
     C
     C                   If        TotalDPP <= .202
     C                   Eval      DSH_OPER_PAF=.025 +.65 *(TotalDPP-.15)
     C                   endif
     C
     C                   If        TotalDPP >  .202
     C                   If        pftype = '14' OR pftype = '16'
     C                   Eval      DSH_OPER_PAF = .0588
     C                   Else
     C                   Eval      DSH_OPER_PAF=.0588 +.825  *(TotalDPP-.202)
     C                   endif                                                   Pftype = 14 or 15
     C                   endif
     C
     C                   If        DSH_OPER_PAF  > .12  and PFTYPE  <> '14' and
     C                             PFTYPE  <> '15'
     C                   Eval      DSH_OPER_PAF=.12
     C                   endif
     C
     C                   endif
     C
     C
     C
     C*Finally we have the OPERATING DSH
     C                   eval(h)    xxdsho = DSH_OPER_PAF * f_o_p
     C
     C*Now calculate the DSH for Capital costs
     C                   If        (TotalDPP >= 0   and Rural_Urban = 'U' and
     C                             PFBEDS >= 100)
     C                   eval      DSH_CAP_PAF=(2.718281828**(.2025*TotalDPP)-1)
     C                   endif
     C
     C                   If        PFODSH = .35
     C                   eval      DSH_CAP_PAF= DSH_OPER_PAF
     C                   endif
     C
     C                   eval(h)    xxdshc = DSH_CAP_PAF  * f_c_p
     C
     C* Check the Discharge date as it will depend on the Discharge date
     C
     C*
     C
     C                   if        pfnew = 'Y'
     C                   eval      xxdshc  = 0
     C                   endif
     C
     C*Now calculate the DSH
     C                   eval(h)    xxdsh  = xxdsho + xxdshc
     C                   endsr
     C*
     C* Calculate DSH
     C     CALDSH1       begsr
     C
     C                   If        PFODSH = .35
     C                   eval       DSH_OPER_PAF = PFODSH
     C                   endif
     C*
     C*First calculate the DSH for Operating costs
     C                   if        PFsalo <> *blanks
     C                   Eval      @@len = %scan(' ': PFsalo :1)
     C                   Else
     C                   Eval      @@len = %scan(' ': PFACBS :1)
     C                   Endif
     C*
B002 C                   if        @@len <> 1
     C                   Eval      rural_urban  = 'U'
     C                   else
     C                   Eval      rural_urban  = 'R'
     C                   endif
     C*
     C                   Eval      TotalDPP = PFSSI + PFMRAT
     C*
     C                   Select
     C*
     C* Urban hospitals that are not MDH
     C*
     C                   when      Rural_Urban = 'U' and pftype <> '14'
     C                             and pftype <> '15'
     C*
     C                   if        pfbeds >= 0 and pfbeds <= 99
     C*
     C                   if        TotalDPP >= .15 and TotalDPP <= .202
     C                   Eval      DSH_OPER_PAF=.025 +.65 *(TotalDPP-.15)
     C                   endif
     C*
     C                   if        TotalDPP > .202
     C                   Eval      DSH_OPER_PAF=.0588 +.825 *(TotalDPP-.202)
     C                   endif
     C*
     C                   If        DSH_OPER_PAF  > .12
     C                   Eval      DSH_OPER_PAF=.12
     C                   endif
     C*
     C                   endif
     C*
     C                   if        pfbeds >= 100
     C*
     C                   if        TotalDPP >= .15 and TotalDPP <= .202
     C                   Eval      DSH_OPER_PAF=.025 +.65 *(TotalDPP-.15)
     C                   endif
     C*
     C                   if        TotalDPP > .202
     C                   Eval      DSH_OPER_PAF=.0588 +.825 *(TotalDPP-.202)
     C                   endif
     C*
     C                   endif
     C*
     C* Rural Referral Centers that are not MDH
     C*
     C                   when      Rural_Urban = 'R' and
     C                             (pftype = '07' or pftype = '17' or
     C                              pftype = '22')
     C*
     C                   if        TotalDPP >= .15 and TotalDPP <= .202
     C                   Eval      DSH_OPER_PAF=.025 +.65 *(TotalDPP-.15)
     C                   endif
     C*
     C                   if        TotalDPP > .202
     C                   Eval      DSH_OPER_PAF=.0588 +.825 *(TotalDPP-.202)
     C                   endif
     C*
     C* Medicare Dependent Hospitals (MDH)
     C*
     C                   when      pftype = '14' or pftype = '15'
     C*
     C                   if        TotalDPP >= .15 and TotalDPP <= .202
     C                   Eval      DSH_OPER_PAF=.025 +.65 *(TotalDPP-.15)
     C                   endif
     C*
     C                   if        TotalDPP > .202
     C                   Eval      DSH_OPER_PAF=.0588 +.825 *(TotalDPP-.202)
     C                   endif
     C*
     C                   other
     C*
     C* Other Rural Hospitals
     C*
     C                   if        Rural_Urban = 'R'
     C*
     C                   if        pfbeds >= 0 and pfbeds <= 499
     C*
     C                   if        TotalDPP >= .15 and TotalDPP <= .202
     C                   Eval      DSH_OPER_PAF=.025 +.65 *(TotalDPP-.15)
     C                   endif
     C*
     C                   if        TotalDPP > .202
     C                   Eval      DSH_OPER_PAF=.0588 +.825 *(TotalDPP-.202)
     C                   endif
     C*
     C                   If        DSH_OPER_PAF  > .12
     C                   Eval      DSH_OPER_PAF=.12
     C                   endif
     C*
     C                   endif
     C*
     C                   if        pfbeds >= 500
     C*
     C                   if        TotalDPP >= .15 and TotalDPP <= .202
     C                   Eval      DSH_OPER_PAF=.025 +.65 *(TotalDPP-.15)
     C                   endif
     C*
     C                   if        TotalDPP > .202
     C                   Eval      DSH_OPER_PAF=.0588 +.825 *(TotalDPP-.202)
     C                   endif
     C*
     C                   endif
     C*
     C                   endif
     C
     C                   endsl
     C
     C*Finally we have the OPERATING DSH
     C                   eval(h)    xxdsho = DSH_OPER_PAF * f_o_p

     C*Now calculate the DSH for Capital costs
     C                   If        (TotalDPP >= 0   and Rural_Urban = 'U' and
     C                             PFBEDS >= 100)
     C                   eval      DSH_CAP_PAF=(2.718281828**(.2025*TotalDPP)-1)
     C                   endif
     C
     C                   If        PFODSH = .35
     C                   eval      DSH_CAP_PAF= DSH_OPER_PAF
     C                   endif
     C
     C                   eval(h)    xxdshc = DSH_CAP_PAF  * f_c_p
     C
     C* Check the Discharge date as it will depend on the Discharge date
     C
     C*
     C
     C                   if        pfnew = 'Y'
     C                   eval      xxdshc  = 0
     C                   endif
     C*
      /free

        // Adjust DSH payment for Discharge date >= 10/01/2013

        If EUDCDT >= 1131001;
           eval(h) xxdsho = xxdsho * 0.25;
           eval(h) xxdshc = xxdshc * 0.25;
        EndIf;

      /end-free
     C
     C*Now calculate the DSH
     C                   eval(h)    xxdsh  = xxdsho + xxdshc
     C*
     C                   endsr
     C*
     C*
     C*** Beg Add *** 11/19/12 **************************************
    C*  Calculate the Hospital Specific Rate (HSP)
     C     HSP_Op_Bonus  begsr
     C*
     C                   Movel     @@st          @@st1             2
     C                   Movel     'MC'          @@st
     C*
     C* Get the FY 2008 budget neutrality adjuster
     C*
     C                   Z-add     1071001       @@efdt
     C*
     C     rtky          setgt     prt01                              99
     C     rtky1         readpe    prt01                                  99
     C*
     C                   if        *in99 = *off
     C                   Eval      xxneut1 = rtneut
     C                   Eval      xxudfa1 = rtudfa
     C                   endif
     C*
     C* Get the FY 2009 budget neutrality adjuster
     C*
     C                   Z-add     1081001       @@efdt
     C*
     C     rtky          setgt     prt01                              99
     C     rtky1         readpe    prt01                                  99
     C*
     C                   if        *in99 = *off
     C                   Eval      xxneut2 = rtneut
     C                   if        pfhoqu = '1'
     C                   Eval      xxudfa2 = rtudfa
     C                   else
     C                   Eval      xxudfa2 = rtudf1
     C                   endif
     C                   endif
     C*
     C* Get the FY 2010 budget neutrality adjuster
     C*
     C                   Z-add     1091001       @@efdt
     C*
     C*
     C* Get the FY 04/01/2010 budget neutrality adjuster
     C*
     C                   if        euaddt >= 1100401
     C                   Z-add     1100401       @@efdt
     C                   endif
     C*
     C     rtky          setgt     prt01                              99
     C     rtky1         readpe    prt01                                  99
     C*
     C                   if        *in99 = *off
     C                   Eval      xxneut3 = rtneut
     C*
     C                   if        pfhoqu = '1'
     C                   Eval      xxudfa3 = rtudfa
     C                   else
     C                   Eval      xxudfa3 = rtudf1
     C                   endif
     C                   endif
     C*
     C* Get the FY 10/01/2010 budget neutrality adjuster
     C*
     C                   if        euaddt >= 1101001
     C                   Z-add     1101001       @@efdt
     C                   endif
     C*
     C     rtky          setgt     prt01                              99
     C     rtky1         readpe    prt01                                  99
     C*
     C                   if        *in99 = *off
     C                   Eval      xxneut4 = rtneut
     C*
     C                   if        pfhoqu = '1'
     C                   Eval      xxudfa4 = rtudfa
     C                   else
     C                   Eval      xxudfa4 = rtudf1
     C                   endif
     C                   endif
     C*
     C*
     C* Get the FY 10/01/2011 budget neutrality adjuster
     C*
     C                   if        euaddt >= 1111001
     C                   Z-add     1111001       @@efdt
     C                   endif
     C*
     C     rtky          setgt     prt01                              99
     C     rtky1         readpe    prt01                                  99
     C*
     C                   if        *in99 = *off
     C                   Eval      xxneut5 = rtneut
     C*
     C                   if        pfhoqu = '1'
     C                   Eval      xxudfa5 = rtudfa
     C                   else
     C                   Eval      xxudfa5 = rtudf1
     C                   endif
     C                   endif
     C*
     C*
     C* Compute the Hospital Update Factor
     C*
     C                   eval(h)    xxhosup  = xxudfa1 * xxudfa2 * xxudfa3
     C                              * xxudfa4 * xxudfa5
     C                              * xxneut1 * xxneut2 * xxneut3 * xxneut4
     C                              * xxneut5
     C*
     C* Hospital Specific Rate
     C*
     C                   eval(h)    xxhosrt  = xxhosup * pfmix
     C
     C*
     C* Calculate the Federal Rate
     C*
     C                   eval(h)   xxfedrt =
     C                             (1 * (Oper_lab * EOWIND + Oper_nlab * PFCOLA
     C                             )) * (( 1 + imeoper + xxdsho) / Outl_flg )
     C*
     C* Hospital Specific Rate vs Federal Rate
     C*
     C                   if        xxhosrt > xxfedrt
     C                   eval(h)    xxhbrt = (xxhosrt - xxfedrt) * dgrelw
     C*
     C                   if        pftype = '14' or pftype = '15'
     C                   eval(h)    xxhbrt = (xxhosrt - xxfedrt) * dgrelw * .75
     C*
     C                   endif                                                   PFTYPE
     C*
     C                   else
     C                   eval(h)    xxhbrt = 0
     C                   endif                                                   HOS > FED
     C*
     C* Add the HSP Operating Bonus to total_payment and save HSP Operating Bonus
     C*
     C                   eval(h)    total_payment = total_payment + xxhbrt
     C*
     C*
     C                   Movel     @@st1         @@st
     C                   endsr
      /Free
        //===================================================================
        // HSP_Op_Bonus_2013 - Hospital Specific Rate
        //===================================================================
       Begsr HSP_Op_Bonus_2013;


       @@st1 = @@st;
       @@st = 'MC';

        If euaddt >= 1121001;
          @@efdt = 1121001;
        Endif;

       Setgt rtky prt01;
       Readpe rtky1 prt01;
       If not %eof(ftrtl2);
          xxneut6 = rtneut;
          xxcmix = rtcmix;
          If pfhoqu = '1';
             xxudfa6 = rtudfa * xxNeut6 * xxCmix;
             xxudfa2013 = rtudfa;
          Else;
             xxudfa6 = rtudf1 * xxNeut6 * xxCmix;
             xxudfa2013 = rtudf1;
          Endif;
       Endif;
       If euaddt >= 1131001;
         @@efdt = 1131001;

         Setgt rtky prt01;
         Readpe rtky1 prt01;
         If not %eof(ftrtl2);
            xxneut6 = xxneut6 * rtneut;
            xxcmix = RTcmix;
            If pfhoqu = '1';
               xxudfa6 = xxudfa2013 * rtudfa * xxneut6 * xxCmix * RTssad;
            Else;
               xxudfa6 = xxudfa2013 * rtudf1 * xxneut6 * xxCmix * RTssad;
            Endif;
         Endif;

       Endif;

        // Compute the Hospital Update Factor
       xxhosup  = pfmix * xxudfa6;

        // Hospital Specific Rate
       xxhosrt = xxhosup * dgrelw;

        // Hospital Specific Rate vs Federal Rate
       If xxhosrt > xxfedrt;
          xxhbrt = (xxhosrt - F_O_P);
       Else;
          xxhbrt = 0;
       Endif;

        // Add the HSP Operating Bonus to total_payment and save HSP Operating
       total_payment = total_payment + xxhbrt;
       @@st = @@st1;

       EndSr;
      /End-Free
        //===================================================================
        // HspOpBonus
        //===================================================================
        Begsr HspOpBonus;
        // Get the FY 2019 values
           Setgt ('MC':'I':1181001) prt01;
           Readpe ('MC':'I') prt01;
           If not %eof(ftrtl2);
              decBudNeuFy19 = RTneut;
              If PFhoqu = '1';
                decUpdFacFy19 = RTudFa;
              Else;
                decUpdFacFy19 = RTudF1;
              Endif;
              xxHosUp  = decBudNeuFy19 * decUpdFacFy19;
           Endif;

          // Get the FY 2020 values
              If EUadDt >= 1191001;
                @@efdt = 1191001;
                Setgt ('MC':'I':1191001) prt01;
                Readpe ('MC':'I')  prt01;
                If not %eof(ftrtl2);
                  decBudNeuFy20 = RTneut;
                  If PFhoqu = '1';
                    decUpdFacFy20 = RTudFa;
                  Else;
                    decUpdFacFy20 = RTudF1;
                  Endif;
                Endif;
                xxHosUp = xxHosUp *(decBudNeuFy20 * decUpdFacFy20);
              Endif;

           // Get the FY 2021 values
              If EUadDt >= 1201001;
                @@efdt = 1201001;
                Setgt ('MC':'I':1201001) prt01;
                Readpe ('MC':'I')  prt01;
                If not %eof(ftrtl2);
                  decBudNeuFy21 = RTneut;
                  If pfhoqu = '1';
                    decUpdFacFy21 = RTudFa;
                  Else;
                    decUpdFacFy21 = RTudF1;
                  Endif;
                Endif;
                xxHosUp = xxHosUp *(decBudNeuFy21 * decUpdFacFy21);
              Endif;

           //*** Begin Add *** 12/01/22 **********************************
           // Get the FY 2023 values
              If EUadDt >= 1221001;
                @@efdt = 1221001;
                Setgt ('MC':'I':1221001) prt01;
                Readpe ('MC':'I')  prt01;
                If not %eof(ftrtl2);
                  decBudNeuFy23 = RTneut;
                  If pfhoqu = '1';
                    decUpdFacFy23 = RTudFa;
                  Else;
                    decUpdFacFy23 = RTudF1;
                  Endif;
                Endif;
                xxHosUp = xxHosUp *(decBudNeuFy23 * decUpdFacFy23);
              Endif;
           //*** End   Add *** 12/01/22 **********************************
           // Determine the Hospital Update Factor by comparison
           Select;
           When EUadDt >= 1201001 and (1.031584 * decUpdFacFy21) > xxHosUp;
                xxHosUp = 1.031584 * decUpdFacFy21;
           //*** Begin Add *** 12/01/22 **********************************
           When EUadDt >= 1221001 and (1.031584 * decUpdFacFy23) > xxHosUp;
                xxHosUp = 1.031584 * decUpdFacFy23;
           //*** End   Add *** 12/01/22 **********************************
           Endsl;
           // Hospital Specific Rate
           //*** Beg Chg *** 02/16/21 ***************************************
           //***xxHosRt = xxHosUp * dgrelw;
                xxHosRt = xxHosUp * PFmix * dgrelw;
           //*** End Chg *** 02/16/21 ***************************************

           // Hospital Specific Rate vs Federal Rate
           //*** Beg Chg *** 02/16/21 ***************************************
           //***   If xxHosRt > xxFedRt;
           //***      xxHbRt = (xxHosRt - F_O_P);
              If xxHosRt > (xxAdjBrt + T_O_pmnt + xxEPClmAmt);
                 xxHbRt = xxHosRt - (xxAdjBrt  + T_O_pmnt + xxEPClmAmt);
           //*** End Chg *** 02/16/21 ***************************************
              Else;
                 xxHbRt = 0;
              Endif;

           // Add the HSP Operating Bonus to total_payment and save HSP Op
              total_payment = total_payment + xxhbrt;
       Endsr;
     C*
    C*================================================================
    C*  clsOverride - Close and Delter Override FTABL
    C*================================================================
     C     clsOverride   begsr
     C*
     C                   close     ftabl
     C                   call      'QCMDEXC'                              99
     C                   parm                    qca(2)
     C                   parm      56            length
     C*
     C                   endsr
    C*================================================================
    C*  opnOverride - Open and Override FTABL
    C*================================================================
     C     opnOverride   begsr
     C*
     C                   eval      length = 56
     C                   movel(p)  qca(1)        qqcmd
     C                   movel(p)  eoast         qqst
     c*
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm      56            length           15 5
     C                   open      ftabl
     C*
     C                   endsr
    C*================================================================
    C*  wrteob      - Write EOB codes to FDDEL3
    C*================================================================
     C     wrteob        begsr
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
     C* load denot1 from ftebpmc else form ftabl
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      ##EOB         DEEOB
     C                   MOVE      ##EOB         DEEOBCD
     C**** End Chg *** 07/18/23 **************************************
     C                   movel     ##eob         kkkey
     C     ebky          chain     peb01
     C                   if        *in99 = *off
     C                   MOVEL(P)  ebdesc        DENOT1
     C                   endif
     C                   MOVE      *BLANKS       DENOT2
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
     C*
     C                   endsr
    C*================================================================
    C*  wrtmsg      - Write error message to FDMGL3
    C*================================================================
     C     wrtmsg        begsr
     C*
     C                   if        ##msg <>  ' '
     C*
     C                   Z-ADD     CACO#         MGCO#
     C                   Z-ADD     CASUB#        MGSUB#
     C                   Z-ADD     CAFND         MGFND
     C                   Z-ADD     CAFYR         MGFYR
     C                   Z-ADD     CACASE        MGCASE
     C                   Z-ADD     CADCS#        MGDCS#
     C                   Z-ADD     CADSEQ        MGDSEQ
     C                   Z-ADD     1             MGMSEQ
     C                   MOVE      CAEOB#        MGEOB#
     C*
     C                   MOVE      ##msg         MGMSG#
     C*
     C                   movel     ##msg         kkmsg#
     C     mgky          chain     pmi01
     C                   if        *in99 = *off
     C                   MOVEL(P)  midesc        mgdesc
     C                   endif
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
     C*
     C                   endif
     C*
     C                   endsr
      /Free
        //===================================================================
        // New_Tech - New tech amt
        //===================================================================
       Begsr New_Tech ;
            NewTech = 0 ;
            Total_Payment = Total_Payment + NewTech;
       Endsr ;

        //===================================================================
        // ValueBasedParticipant - Value Based Purchase Adjustment
        //===================================================================
       Begsr ValueBasedParticipant ;
        If PFVBPP = 'Y';

          ValueBasedPart = (PFVBPA) * (F_O_P + NewTech) - F_O_P ;
        Else;
          ValueBasedPart = 0;
        EndIf;
          Total_Payment = Total_Payment + ValueBasedPart;

       Endsr ;

        //===================================================================
        // ReAdmit - Readmit Adjustment
        //===================================================================
       Begsr ReAdmit ;

          If EUDCDT > 1121001 and PFHRRI = '1' ;
          ReAdmitAdj = (PFHRRA) * (F_O_P + NewTech) - (F_O_P + NewTech) ;
          Total_Payment = Total_Payment + ReAdmitAdj;
          EndIf ;

       Endsr ;

        //===================================================================
        // LowVolume - Low volume Adjustment
        //===================================================================
       Begsr LowVolume ;

          setgt faky PFA01 ;
          readpe faky1 PFA01 ;
          if %found(FAIP14L1) and EUdcDt <= 1170930;
           if EUDCDT >= 1131001;
            LowVolumeAdj = IP14LVPA * Total_Payment;
            Total_Payment = Total_Payment + LowVolumeAdj;
           else  ;
            LowVolumeAdj = Percent * Total_Payment;
            Total_Payment = Total_Payment + LowVolumeAdj;
           endif ;
          endif ;

        LowVolumeAdj = PFlVol * Total_Payment;
        Total_Payment = Total_Payment + LowVolumeAdj;
       Endsr ;

        //===================================================================
        // HACadj - HAC Adjustment
        //===================================================================
       Begsr HACadj;

           xxHACadj = (-.01) * (Total_Payment - PFtPas) ;
           Total_Payment = Total_Payment + xxHACadj ;

       Endsr ;


        //===================================================================
        // CalUnCompAmt - Calculate Uncompensated Care amount
        //===================================================================
       Begsr CalUnCompAmt;

         // get estimated per claim amount
         EPClmAmtInd = *off;
         clear xxEPClmAmt;


         If EUDCDT >= 1150401;

           xxEPClmAmt = PFuncm;
           EPClmAmtInd = *on;

         Else;

           Setgt (pfprov) pun01;
           Readpe (pfprov) pun01;
           Dow not %eof(ftuncl);
             if eudcdt >= uneffdt;
               xxEPClmAmt = UNEPClm;
               EPClmAmtInd = *on;
               leave;
             endif;
             Readpe (pfprov) pun01;
           Enddo;

         EndIf;

       Endsr ;
        //===================================================================
      /End-Free
**  (QCMDEX)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
DLTOVR FILE(FTABL)
