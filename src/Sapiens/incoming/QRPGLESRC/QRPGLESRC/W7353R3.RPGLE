     H Copyright('(c) 2025 Sapiens. All Rights Reserved.')                            *
     H/TITLE W7353R3 - WCSTAT Transfer Data                                           *
     H dftactgrp(*no) actgrp(*caller)                                                 *
     H option(*nodebugio:*srcstmt)                                                    *
     H*********************************************************************************
     H*  Description:  Transfer extraction data to WCSTAT file.                       *
     H*                                                                               *
     H*  Called from: W7353C3                                                         *
     H*  Calls to:                                                                    *
     H*                                                                               *
     H*                                                                               *
     H*  Created: 08/19/05    Programmer C:15    Project:WC591                        *
     H*********************************************************************************
     H*  Revisions:                                                                   *
     H*                                                                               *
     H* 08/22/25 C:333 Support: 119618 (AmWINS)                                       *
     H*                WCSTAT CDX dropping USRs due to 9740 mod errorr                *
     H*                - Changes for RT4 (47-50), reserved for future use.            *
     H*                                                                               *
     H* 08/18/25 C:65  Project: Phw#119560    (Sunz/WC2952)                           *
     H*                Decrease rate to 7.3 size as the exposure record layout for    *
     H*                WCSTAT did not change - it should have stayed 7.3 size.        *
     H*                                                                               *
     H* 09/20/24 C:344 Project: WC2952                                                *
     H*                Increase rate to 8.3 zoned.                                    *
     H*                                                                               *
     H* 03/11/22 C:340 Project: WC2843                                                *
     H*                Use Carrier specific Data provider and FEIN                    *
     H*                                                                               *
     H* 01/02/18 C:267 Project: C150WCI                                               *
     H*                Added Business Segment ID# field in the header DS              *
     H*                                                                               *
     H* 08/08/17 C:65  Project: Phw# 48723 (OIC)                                      *
     H*                Update loss record format and populate a zero for recovery dt. *
     H*                                                                               *
     H* 09/01/16 C:65  Project: Phw# 42692 (LUBA)                                     *
     H*                Populate a 'N' into the certified health care network field.   *
     H*                Program variable to be replaced if we add a database field     *
     H*                for the network flag.                                          *
     H*                                                                               *
     H* 12/03/15 C:65  Project: 45WKP                                                 *
     H*                Process old carrier # data for LCTA when indicated by new flag *
     H*                passed into program.                                           *
     H*
     H*  10/23/13  C:65  Project: Supp# WA147/83828                                   *
     H*                  Expand size of CAP array to ensure nothing is lost when      *
     H*                  building the hyphen free policy #.                           *
     H*                                                                               *
     H*  10/22/09  C:232 Support: WC139/70373                                         *
     H*                  Fixed the issue of 'Submission of Individual units'.         *
     H*                                                                               *
     H*  06/10/09  C:65  Project: Supp# 66722                                         *
     H*                  If reporting to NJ then update all exposure with a split     *
     H*                  code of 00 to use the ARD date for the rate and mod dates.   *
     H*                                                                               *
     H*  05/11/09  C:61  Project: 93WHW                                               *
     H*                  Added experience rating selection criteria
     H*                                                                               *
     H*  03/13/09  C:65  Project: Supp#67575                                          *
     H*                  Remove the hard coding of certain non-NCCI states when       *
     H*                  creating a NCCI submission file.  Instead verify the state's *
     H*                  NCCI status by checking the WCST table file.                 *
     H*                                                                               *
     H*  01/12/09  C:65  Project: 132WAZ                                              *
     H*                  Use carrier # 12985 for any NCCI submissions for Compwest    *
     H*                  for group 5.                                                 *
     H*                                                                               *
     H*  05/02/08  C:15  Project: 114WNI                                              *
     H*                 -Added code to load the claimant's gender in the ICR record.  *
     H*                 -Added code to process "ICR only" resubmissions.              *
     H*                                                                               *
     H*  10/16/07  C:15  Support: WC136/60300                                         *
     H*                  Added code to prevent the manipulation of the policy# for    *
     H*                  106 or greater policy years for clt136.                      *
     H*                                                                               *
     H*  08/21/07  C:53  Support: 59466                                               *
     H*                  Added code to remove hyphens from the claim #.               *
     H*                                                                               *
     H*  02/22/07  C:15  Support: WC139/56691                                         *
     H*                  Added code to remove hyphens from the claim #.               *
     H*                                                                               *
     H*  01/17/07  C:185 Project: 114WNI                                              *
     H*                  Rpt ICR Data (Individual Case Report) NJ                     *
     H*                                                                               *
     H*  07/03/06  C:15  Support: WC135/53079                                         *
     H*                  Added code to remove hyphens from the policy #.              *
     H*                                                                               *
     H*  07/02/06  C:15  Support: WC132/53428                                         *
     H*                  Added code to load the TPA FEIN from the company master file.*
     H*                                                                               *
     H*  06/27/06  C:15  Support: WC136/53002                                         *
     H*                  Added code for the expanded format fields for CA.            *
     H*                                                                               *
     H*  05/24/06  C:65  Support: #52868                                              *
     H*                  Add new coding to copy positions 5-12 for Carewest carrier   *
     H*                  policy numbers.                                              *
     H*                                                                               *
     H*********************************************************************************
     H*  Indicators:                                                                  *
     H*                                                                               *
     H*  20 - Indicates ASWG submission                                               *
     H*                                                                               *
     H*********************************************************************************
     F* Link, header, name, address file
     FWMSRP3L1  UF   E           K DISK
     F*
     F* Exposure file
     FWDSEL     IF   E           K DISK
     F*
     F* Loss file
     FWDSLL     IF   E           K DISK
     F*** Begin add *** 07/02/06 *******************************************
     Fsmcol     if   e           k disk
     F*** End add ***** 07/02/06 *******************************************
     F***  Begin Add  ***  01/17/07  **************************************
     FWDSIL1    IF   E           K DISK
     F***  End add ******  01/17/07  **************************************
     F*
     F* Unit record file
     FWMSQL     IF   E           K DISK
     F*
     FWMEML     IF   E           K DISK
     F*
     FWMRPL     IF   E           K DISK
     F*
     FSSYLF     IF   E           K DISK    include(swc01)
     F*** Begin add *** 03/13/09 *******************************************
     FWTGTL     if   e           k disk                                         generic table file
     FSTSTL1    if   e           k disk                                         state table file
     F*** End add ***** 03/13/09 *******************************************
     f*** begin add ***  06/10/09  **********************************************
     Fwdell     UF   E           K DISK
     f*** begin add ***  06/10/09  **********************************************
     F*
     FWCSTATX   IF A E             DISK
     F*

     D*  Key field definitions
     D kkdiv           S                   Like(EMDIV)
     D kkfyr           S                   Like(SRFYR)
     D kkey            S                   Like(WCKEY)
     D kkstcd          S                   Like(sestcd)
     D xxstcd          S                   Like(srstcd)
     c*** begin add ***  06/10/09  **********************************************
     D xxandt          S                   Like(semddt)
     D xxmddt          S                   Like(semddt)
     D xxrtdt          S                   Like(semddt)
     c*** end add *****  06/10/09  **********************************************
     D xxzip           S              9A
     D recFound        s              1
     D svstcd          s                   Like(srstcd)
     D x               s              3  0
     d*** begin add ***  03/13/09  *****************************************
     D kktbid          s                   like(gttbid)
     D xxncci          s              1
     D NCCIst          s              2  0 dim(99)
     d*** end add *****  03/13/09  *****************************************
     D*** Begin add *** 07/03/06 *******************************************
     D*** Begin chg *** 10/23/13 *******************************************
     D*****CAP              s              1    dim(18)
     DCAP              s              1    dim(25)
     D*** End chg ***** 10/23/13 *******************************************
     DCAP2             s              1    dim(18)
     D*** End add ***** 07/03/06 *******************************************
     D*** Begin add *** 02/22/07 *******************************************
     DClm              s              1    dim(12)
     DClm2             s              1    dim(12)
     D*** End add ***** 02/22/07 *******************************************

     D*  Date/time data structure
     D                 DS
     D dsdttm                        12  0
     D  dstime                        6  0 Overlay(dsdttm)
     D  dsdate                        6  0 Overlay(dsdttm:7)

     D xxjuldt         s              5  0
     D xxisodt         s              8  0
     D JulDt           s               d   datfmt(*JUL)
     D IsoDt           s               d   datfmt(*ISO)
     D*----------------------------
     D*  Transfer data structures
     D*----------------------------
     D ETR             DS
     D  Label                  1     14    INZ('$!+WORKCOMP+!$')
     D  Email                 15     45
     D  Rec_typ               46     47    INZ('  ')
     D  Data_typ              48     50    INZ('WCS')
     D  Data_rcv              51     55
     D  Version               56     63
     D  Subm_typ              64     64    INZ('S')
     D  Subm_rpl_ind          65     72
     D  Data_prov             73     77
     D  Contact               78    102
     D  Rcpt_cd              103    103    INZ('E')
     D  Phone#               104    113
     D  Phone_ext            114    119
     D  Fax#                 120    129
     D  Proc_dt              130    137
     D  Prov_adr             138    238
     D  Prov_ind             239    239    INZ('T')
     D  TPA_FEIN             240    248    INZ('251686685')
     D Link            DS
     D  SRCARR                 1      5  0
     D  Policy#                6     23
     D     FND                 6      8  0
     D     EMP                 9     17  0
     D     FYR                18     20  0
     D  SRCRT#                24     30
     D  SRSTCD                31     32  0
     D  Pol_Efdt              33     38  0
     D  SRRPT#                39     39
     D  SRCORR                40     40
     D*
     D Header          DS
     D  Hdr_type               1      1  0 INZ(1)
     D  SRPCON                 2     13  0
     D*  reserved
     D  Pol_Exp               15     20  0
     D  SRRSK#                21     29
     D*  reserved
     D  SRPND#                31     40
     D  SRTERM                41     41  0
     D  SRURRI                42     42
     D  SRPTYP                43     44  0
     D*  reserved
     D  SRRRPT                65     65
     D*** begin add ***  01/02/18  *********************************************
     D  SRBSI#                66     72  0
     D*** end   add ***  01/02/18  *********************************************
     D*  reserved
     D  SRCTYP                82     82
     D  State_eff             83     88  0
     D  SRFEIN                89     97  0
     D*  reserved
     D  SRPOLC               106    112
     D*** begin add ***  09/01/16  *********************************************
     D  xxCHCN               113    113
     D*** end add *****  09/01/16  *********************************************
     D*  reserved
     D  SRPID#               117    122  0
     D  SRDTYP               125    128  0
     D  SRDPCT               129    130  0
     D  SRDAMP               131    139  0
     D  SRDAGG               140    148  0
     D  SRPRP#               149    150  0
     D*  reserved
     D  SRPCOR               152    152
     D  SRPCAR               153    157  0
     D  Prev_pol#            158    175
     D  Prev_effdt           176    181  0
     D  SRPEXP               182    183  0
     D  SRPCER               184    190
     D*  reserved
     D  SRASWG               210    210
     D*
     D Name            DS
     D  Nme_type               1      1  0 INZ(2)
     D  SRNAME                 2     80
     D*  reserved
     D* note: NJ only uses pos 2 - 51 for insured's name
     D*
     D Address         DS
     D  Add_type               1      1  0 INZ(3)
     D  SRADDR                 2     80
     D*  reserved
     D* note: NJ only uses pos 2 - 51 for insured's address
     D*
     D Exposure        DS
     D  Exp_type               1      1  0 INZ(4)
     D*  reserved
     D  Class_code             3      6  0
     D*  reserved
      *** Beg Del ***** 08/22/25 *******************************************
     D***SEPRVR                 8      8  0
     D***SEEXP#                 9     10  0
      *** End Del ***** 08/22/25 *******************************************
     D  Exp_mod               11     14  3
     D  Mod_eff_dt            15     20  0
     D  Rat_eff_dt            21     26  0
     D  SEEXPA                27     36  0
     D  SEPRMA                37     45  0
     D*** begin chg *** 09/20/24 *******************************************
     D***SEMNRT                46     52  3
     D*** begin chg *** 08/18/25 *******************************************
     D**SEMNRT                46     53  3
     D***SESPLT                53     53  0
     D**SESPLT                54     54  0
     D  exp_Rate              46     52  3
     D  SESPLT                53     53  0
     D*** end chg ***** 08/18/25 *******************************************
     D*** end   chg *** 09/20/24 *******************************************
     D*  reserved
     D  SETIER                71     72  0
     D*  reserved
     D  SEUPDT                81     81
     D*  reserved
     D  SEEXPO                83     84  0
     D*  reserved
     D*
     D Loss            DS
     D  Los_type               1      1  0 INZ(5)
     D*  reserved
     D  ClassCode              3      6  0
     D*  reserved
     D  SLPRVR                 8      8  0
     D  SLLOS#                 9     10  0
     D  SL#CLM                11     14  0
     D  Acc_date              15     20  0
     D  SLCLM#                21     32
     D  SLCLST                33     33  0
     D*** begin add *** 06/27/06 *******************************************
     D  SLAWW                 34     38  0
     D*** end add ***** 06/27/06 *******************************************
     D  SLINJ#                39     40  0
     D  SLCAT#                41     42  0
     D  SLINDM                43     51  0
     D  SLMEDI                52     60  0
     D*** begin chg *** 08/08/17 *******************************************
     D*****  SLSS#                 61     69  0
     D******  reserved
     D  SLRCVD                61     68  0
     D*  reserved
     D  SLCAS#                72     80
     D*** end chg ***** 08/08/17 *******************************************
     D  SLUPDT                81     81
     D*  reserved
     D  SLLC1                 83     84  0
     D  SLLC2                 85     86  0
     D  SLLC3                 87     88  0
     D  SLLC4                 89     90  0
     D  SLLC5                 91     92  0
     D*** begin add *** 06/27/06 *******************************************
     D  SLVRHB                93     99  0
     D*** end add ***** 06/27/06 *******************************************
     D*  reserved
     D  SLJRST               100    101  0
     D  SLMCTP               102    103  0
     D  SLPART               104    105  0
     D  SLNATR               106    107  0
     D  SLCAUS               108    109  0
     D  SLOCCU               110    127
     D  SLVOCR               128    128
     D  SLLUMP               129    129
     D  SLFRAU               130    131  0
     D*  reserved
     D  SLPDIN               134    142  0
     D  SLPDMD               143    151  0
     D  SLCATF               152    160  0
     D  SLEATF               161    169  0
     D  SLDEDR               170    178  0
     D*** begin add *** 06/27/06 *******************************************
     D  SLTGI                179    187  0
     D*** end add ***** 06/27/06 *******************************************
     D*  reserved
     D  SLALAP               190    198  0
     D  SLALAI               199    207  0
     D*** begin add *** 06/27/06 *******************************************
     D  SLIPCT               208    210  0
     D*** end add ***** 06/27/06 *******************************************
     D*  reserved
     D*
     D Unit            DS
     D  Unt_type               1      1  0 INZ(6)
     D  SQEPAY                 2     12  0
     D  SQEOTH                13     22  1
     D  SQSUBP                23     32  0
     D  SQSTDP                33     43  0
     D  SQ#CLT                44     48  0
     D  SQTIND                49     58  0
     D  SQTMED                59     68  0
     D  SQ#RCD                69     73  0
     D  SQPRPT                74     74  0
     D*  reserved
     D  SQPDIN                83     92  0
     D  SQPDMD                93    102  0
     D  SQCLAF               103    112  0
     D  SQEMAF               113    122  0
     D  SQTALP               123    132  0
     D  SQTALI               133    142  0
     D*  reserved
     D*
     D Submission      DS
     D  Sub_type               1      1  0 INZ(9)
     D  Dtl_rec#               2      9  0 INZ(0)
     D  Ttl_units             10     16  0
     D*  not required
     D  #ICRs                 23     30  0 INZ(0)
     D*  reserved
     D  Sub_ASWG             210    210

     D***  Begin Add  ***  01/17/07  ***************************************
      *
     D ICR7A           DS
     D  Rec_Type_A             1      1  0
     D  Sub_Rec_TypA           2      2
     D  Claim_Nbr_A            3     14
     D  Res_Typ_Code          15     16  0 INZ(0)
     D  Yr_last_Exp           17     20  0 INZ(0)
     D**RESERVED**************21     28
     D  Class_Cd_ICR          29     32  0 INZ(0)
     D**RESERVED**************33     33
     D  Injury_Code           34     35  0 INZ(0)
     D  Loss_Cov_Cd           36     37  0 INZ(0)
     D  Tran_Typ_Cd           38     38  0 INZ(0)
     D*** Begin add *** 05/02/08 ******************************************
     D  Gender_Cd             39     39
     D*** End add ***** 05/02/08 ******************************************
     D  Accdt_Date            40     45  0 INZ(0)
     D  Deced_Date            46     51  0 INZ(0)
     D  Reprt_Date            52     57  0 INZ(0)
     D  Birth_Date            58     63  0 INZ(0)
     D**RESERVED**************64     69
     D  Close_Date            70     73  0 INZ(0)
     D**RESERVED**************74     78
     D  Status_Code           79     79  0 INZ(0)
     D  Mth_Stl_Cd            80     80  0 INZ(0)
     D**RESERVED**************81     82
     D  Los_Cnd_Cd            83     92  0 INZ(0)
     D Los_Cnd_Cd_Act         83     84  0 INZ(0)
     D Los_Cnd_Cd_Typ         85     86  0 INZ(0)
     D Los_Cnd_Cd_Rec         87     88  0 INZ(0)
     D Los_Cnd_Cd_Cov         89     90  0 INZ(0)
     D Los_Cnd_Cd_Stl         91     92  0 INZ(0)
     D**RESERVED**************93     99
     D  Jur_St_Cd            100    101  0 INZ(0)
     D  Mc_Org_Typ_Cd        102    103  0 INZ(0)
     D  Lum_Sum_Ind          104    104
     D  Frd_Clm_Cd           105    106  0 INZ(0)
     D**RESERVED*************107    209
     D  ASWG_Sub_Cd          210    210


     D ICR7B           DS
     D  Rec_Type_B             1      1  0
     D  Sub_Rec_TypB           2      2
     D  Claim_Nbr_B            3     14
     D  Avg_WkWg_Amt          15     19  0 INZ(0)
     D  Inj_Bdy_Prt           20     21  0 INZ(0)
     D  Inj_Nat_Cd            22     23  0 INZ(0)
     D  Inj_Cos_Cd            24     25  0 INZ(0)
     D  Inc_Cst_Tot           26     34  0 INZ(0)
     D  SI_Pct_Dis1           35     37  0 INZ(0)
     D  SI_Mbr_Cd1            38     39  0 INZ(0)
     D  SI_Num_Wk1            40     43  0 INZ(0)
     D  SI_Los_Tot1           44     52  0 INZ(0)
     D  Num_Wk_TBen           53     56  0 INZ(0)
     D*RESERVED***************57     62
     D  SI_Pct_Dis2           63     65  0 INZ(0)
     D  SI_Mbr_Cd2            66     67  0 INZ(0)
     D  SI_Num_Wk2            68     71  0 INZ(0)
     D  SI_Los_Tot2           72     80  0 INZ(0)
     D*RESERVED***************81    210


     D ICR7C           DS
     D  Rec_Type_C             1      1  0
     D  Sub_Rec_TypC           2      2
     D  Claim_Nbr_C            3     14
     D  NI_Dis_Pct            15     17  0 INZ(0)
     D  NI_Inc_LsToT          18     26  0 INZ(0)
     D  Emp_Lia_Inc           27     35  0 INZ(0)
     D  Voc_Rehab_Tot         36     44  0 INZ(0)
     D  PIB_Pd_ValDt          45     53  0 INZ(0)
     D  PVFI_Pmt_Amt          54     62  0 INZ(0)
     D  Fnr_Alw_Amt           63     71  0 INZ(0)
     D  LS_RM_Amt             72     80  0 INZ(0)
     D*RESERVED***************81    210


     D ICR7D           DS
     D  Rec_Type_D             1      1  0
     D  Sub_Rec_TypD           2      2
     D  Claim_Nbr_D            3     14
     D  Ind_Inc_Tot           15     24  0 INZ(0)
     D  Med_Inc_Tot           25     34  0 INZ(0)
     D  PdTot_Ind2Val         35     44  0 INZ(0)
     D  PdTot_Med2Val         45     54  0 INZ(0)
     D  SS_Off_Amt            55     63  0 INZ(0)
     D  PIA_PvRs_NPd          64     73  0 INZ(0)
     D*RESERVED***************74    210  0


     D ICR7E           DS
     D  Rec_Type_E             1      1  0
     D  Sub_Rec_TypE           2      2
     D  Claim_Nbr_E            3     14
     D  Ben_Cd_Rel1           15     15  0 INZ(0)
     D  Ben_Cd_Dep1           16     16
     D  Ben_Bth_Dt1           17     22  0 INZ(0)
     D  Ben_Cd_Rel2           23     23  0 INZ(0)
     D  Ben_Cd_Dep2           24     24
     D  Ben_Bth_Dt2           25     30  0 INZ(0)
     D  Ben_Cd_Rel3           31     31  0 INZ(0)
     D  Ben_Cd_Dep3           32     32
     D  Ben_Bth_Dt3           33     38  0 INZ(0)
     D  Ben_Cd_Rel4           39     39  0 INZ(0)
     D  Ben_Cd_Dep4           40     40
     D  Ben_Bth_Dt4           41     46  0 INZ(0)
     D  Ben_Cd_Rel5           47     47  0 INZ(0)
     D  Ben_Cd_Dep5           48     48
     D  Ben_Bth_Dt5           49     54  0 INZ(0)
     D*RESERVED***************55     56
     D  Insured_Name          57     80
     D*RESERVED***************81    210


     D ICR7F           DS
     D  Rec_Type_F             1      1  0
     D  Sub_Rec_TypF           2      2
     D  Claim_Nbr_F            3     14
     D  Carrier_Name          15     32
     D  Wrkr_Lst_Nam          33     50
     D  Wrkr_Occptn           51     68
     D  SSN_Nbr               69     77  0 INZ(0)
     D*RESERVED***************78    210


     D ICR7G           DS
     D  Rec_Type_G             1      1  0
     D  Sub_Rec_TypG           2      2
     D  Claim_Nbr_G            3     14
     D  PdHosCos2Val          15     23  0 INZ(0)
     D  LE_Def_Amt            24     32  0 INZ(0)
     D  LE_Clm_Amt            33     41  0 INZ(0)
     D  Tot_Grs_Inc           42     50  0 INZ(0)
     D  VocReh_IndAmt         51     59  0 INZ(0)
     D  VocReh_TrnAmt         60     68  0 INZ(0)
     D  VocReh_ValAmt         69     77  0 INZ(0)
     D*RESERVED***************78    210


     D ICR7H           DS
     D  Rec_Type_H             1      1  0
     D  Sub_Rec_TypH           2      2
     D  Claim_Nbr_H            3     14
     D  Hire_date             15     20  0 INZ(0)
     D  Emp_Sts_Cd            21     22  0 INZ(0)
     D  PTDB_ValDt            23     32  0 INZ(0)
     D  PPPB_ValDt            33     42  0 INZ(0)
     D  PVRB_ValDt            43     52  0 INZ(0)
     D  PPTB_ValDt            53     62  0 INZ(0)
     D  PDBt_ValDt            63     72  0 INZ(0)
     D*RESERVED***************73    210


     D ICR7I           DS
     D  Rec_Type_I             1      1  0
     D  Sub_Rec_TypI           2      2
     D  Claim_Nbr_I            3     14
     D  PStlAmt_ValDt         15     24  0 INZ(0)
     D  PPC_ValDt             25     34  0 INZ(0)
     D  PAMV_ValDt            35     44  0 INZ(0)
     D  PDMV_ValDt            45     54  0 INZ(0)
     D  PIME_ValDt            55     64  0 INZ(0)
     D  Srg_Cd                65     65  0 INZ(0)
     D  Att_Rep_Cd            66     66  0 INZ(0)
     D  Att_DFR_Dt            67     72  0 INZ(0)
     D*RESERVED***************73    210


     D ICR7J           DS
     D  Rec_Type_J             1      1  0
     D  Sub_Rec_TypJ           2      2
     D  Claim_Nbr_J            3     14
     D  Ant_Prc_Amt           15     24  0 INZ(0)
     D*RESERVED***************25     34
     D  Sgl_Sum_Pdt           35     40  0 INZ(0)
     D*RESERVED***************41    210


     D SubmissionICR   DS
     D  Rec_Type_ICR           1      1  0 INZ(0)
     D  Dtl_Rec#_ICR           2      9  0 INZ(0)
     D  Ttl_Unts_ICR          10     16  0 INZ(0)
     D  Pri_Eff_MMYY          17     22  0 INZ(0)
     D  Ttl_ICR               23     30  0 INZ(0)
     D*RESERVED***************31    209
     D  ASWG_Sbm_Cd          210    210


     D***  End   Add  ***  01/17/07  ***************************************



     D*  Filler constant for submission record
     D Filler          C                   Const('99999999999999999999999999999+
     D                                            99999999999')
     D*

     DPSDS           ESDS                  Extname(SZQ1P)

     C*---------------
     C*  Key list
     C*---------------
     C*    Keys for extracted data files
     C*
     C     key1          klist
     C                   kfld                    srco#
     C                   kfld                    srfnd
     C                   kfld                    srrpmy
     C                   kfld                    sremp#
     C                   kfld                    srfyr
     C                   kfld                    srstcd
     C*** begin add *** 03/13/09 *******************************************
     C*   generic table id file
     C     gtky          klist
     C                   kfld                    kkco#
     C                   kfld                    kktbid
     C*** End add ***** 03/13/09 *******************************************
     C*
     C*  Key for employer master file
     C     emky          klist
     C                   kfld                    srco#
     C                   kfld                    srfnd
     C                   kfld                    sremp#
     C*
     C*  Key for carrier/state reporting master file
     C     rpky          klist
     C                   kfld                    srco#
     C                   kfld                    srfnd
     C                   kfld                    sremp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C*
     C*** Begin add *** 07/02/06 *******************************************
     C*  Company Master file key list.
     C*
     C     coky          klist
     C                   kfld                    srco#
     C*** End add ***** 07/02/06 *******************************************

     C     dtmdyy        plist
     C                   parm                    srdat_in          8 0
     C                   parm      '*CYMD'       srfr_fmt          8
     C                   parm      *blanks       srto_fmt          8
     C                   parm                    srdat_out        10
     C                   parm                    srerror           1

     C     *entry        plist
     C                   parm                    @@stcd            2
     C                   parm                    @@mail           40
     C                   parm                    @@name           25
     C                   parm                    @@phn            10
     C                   parm                    @@ext             4
     C                   parm                    @@fax            10
     C                   parm                    @@adr           101
     C*** beg add *** 05/11/09 ***************************************
     C                   parm                    @@er              1
     C*** end add *** 05/11/09 ***************************************
     C*** begin add ***  12/03/15  ***********************************************
     C                   Parm                    QQOLDCAR          1
     C*** end add *****  12/03/15  ***********************************************
     C*** Begin add ***  03/11/22  ***********************************************
     C                   Parm                    qqDtaPvdr         1
     C                   Parm                    qqFein            9
     C*** End add *****  03/11/22  ***********************************************

     C*** begin add ***  05/24/06  *******************************************************
     C                   call      'RTVCLTID'
     C                   parm                    cltid             3
     C*** end add *****  05/24/06  *******************************************************

     C                   time                    dsdttm
     C*
     C                   z-add     0             Dtl_rec#
     C                   z-add     0             Ttl_units
     C                   move(p)   @@stcd        xxstcd
     C*

     C*--------------------
     C*  Main processing
     C*--------------------
     C
     C                   eval      recFound = *off
     C                   read      PSR01                                  99
     C*** Begin add *** 03/13/09 *******************************************
     c                   z-add     srco#         kkco#             3 0
     C                   if        @@stcd = *blanks
     C                   exsr      NCCI_st
     C                   endif
     C*** End add ***** 03/13/09 *******************************************
     C*
     C                   if        @@stcd <> *blanks and @@mail <> *blanks       PA, DE, NJ
     C                   exsr      ETR_sr
     C                   endif
     C*
     C                   dow       *in99 = *off
     c*** begin add ***  06/10/09  **********************************************
     c                   clear                   xxandt
     c*** end add *****  06/10/09  **********************************************
     c*** begin add ***  01/12/09  *****************************************
     c                   z-add     0             svcarr            5 0
     c                   if        cltid = '132' and @@stcd = *blanks
     c                             and srfnd = 5
     c                   z-add     srcarr        svcarr
     c                   z-add     12985         srcarr
     c                   endif
     c*** end add *****  01/12/09  *****************************************
     C*
      *** beg add *** 05/11/09 ***************************************
     c                   movel(p)  'NO '         xxskip            3
      *
      *  If clt093 then check experience rating parm
      *
     c                   if        (cltid = '093') and
     c                             (@@er <> *blank)
     c     emky          chain     PEM01                              60
      *
     c                   dow       (*in60 = *off) and (EMSTRP <> 'Y')
     c     emky          reade     PEM01                                  60
     c                   enddo                                                  *in60=*off
      *
     c                   if        (*in60 = *off) and
     c                             (EMSTRP = 'Y') and
     c                             (EMEXRT = @@ER)
     c                   movel(p)  'NO '         xxskip            3
     c                   else
     c                   movel(p)  'YES'         xxskip            3
     c                   endif
      *
     c                   endif
     C*** begin add ***  12/03/15  ***********************************************
     C
     C                   If        cltid = '045' and QQOldCar = 'Y'
     C                             and SREFDT >= 1160101
     c                   movel(p)  'YES'         xxskip
     C                   Endif
     C
     C                   If        cltid = '045' and QQOldCar <> 'Y'
     C                             and SREFDT < 1160101
     c                   movel(p)  'YES'         xxskip
     C                   Endif
     C
     C                   If        cltid = '045' and QQOldCar = 'Y'
     C                             and SREFDT < 1160101
     c                   z-add     33057         srcarr
     C                   Endif
     C*** end add *****  12/03/15  ***********************************************
      *
     c                   if        xxskip = 'NO '
      *** end add *** 05/11/09 ***************************************
     C                   exsr      LnkHdrNmAd
      *** beg add *** 05/11/09 ***************************************
     c                   endif
      *** end add *** 05/11/09 ***************************************
     C*
     c*** begin add ***  01/12/09  *****************************************
     c                   if        svcarr <> 0
     c                   z-add     svcarr        srcarr
     c                   endif
     c*** end add *****  01/12/09  *****************************************
     C                   eval      srstat = 'P'
     C                   update    psr01
     C*
     C                   read      PSR01                                  99
     C                   enddo
     C*
     C                   exsr      Sr_Submit
     C*
     C*  Call CL to transfer WCSTATX to save member.
     C*
     C                   call      'W7353CA3'
     C                   parm                    @@stcd
     C*
     C                   eval      *inlr = *on
     C                   return
     C*

     C*==========================================================================
     C*  LnkHdrNmAd  - subroutine to transfer WMSRP files to WCSTAT file
     C*==========================================================================
     C     LnkHdrNmAd    begsr
     C*
     C*  Verify that the policies rating state matches the state being
     C*  reported to.
     C*
     c*** begin add ***  03/13/09  *****************************************
     c*** begin chg ***  10/22/09  *****************************************
     C**** srstcd        lookup    NCCIst                                 99
     C     srstcd        lookup    NCCIst                                 97
     c*** end chg *****  10/22/09  *****************************************
     c*** end add *****  03/13/09  *****************************************
     C                   if        @@stcd = *blanks
     c*** begin chg ***  03/13/09  *****************************************
     C*********                    and srstcd <> 04 and srstcd <> 07
     C*********                    and srstcd <> 21 and srstcd <> 29
     C*********                    and srstcd <> 37
     c*** begin chg ***  10/22/09  *****************************************
     C*********                    and *in99 = *on
     C                             and *in97 = *on
     c*** end chg *****  10/22/09  *****************************************
     c*** end chg *****  03/13/09  *****************************************
     C                             or xxstcd = srstcd
     C                             or xxstcd = 07 and srstcd = 37               07=DE & 37=PA
     C                             or xxstcd = 37 and srstcd = 07               07=DE & 37=PA

     C*    set flag that at least one record is found on
     C                   eval      recFound = *on
     C*
     C*    Assign data structure values
     C                   z-add     SREFDT        Pol_Efdt
     C*
     C                   z-add     SRCNDT        Pol_Exp
     C                   z-add     SRSTED        State_Eff
     C                   movel(p)  SRPPOL        Prev_pol#
     C                   z-add     SRPEFD        Prev_effdt
     C*
     C*
     C*    Determine state rptg div and check for carrier policy#
     C     emky          chain     PEM01                              60
     C                   dow       (*in60 = *off) and (EMSTRP <> 'Y')
     C     emky          reade     PEM01                                  60
     C                   enddo                                                  *in60=*off
     C*
     C                   if        *in60=*off
     C*
     C                   eval      kkdiv = EMDIV
     C                   eval      kkfyr = SRFYR
     c*** begin add ***  06/10/09  **********************************************
     c*
     c* Check for ARD for reporting exposures.
     c*
     c     rpky          chain     pel01                              60
     c                   if        *in60 = *off and elandt <> 0
     c                   z-add     elandt        xxandt
     c                   endif
     c*** end add *****  06/10/09  **********************************************
     C     rpky          chain     PRP01                              60
     C                   if        (*in60=*off) and (RPCAP# <> *blank)
     C*** Begin chg *** 10/23/13 *******************************************
     C*********          movel(p)  RPCAP#        Policy#
     C                   movel(p)  RPCAP#        xxPol25          25
     C*** End chg ***** 10/23/13 *******************************************
     C*** begin add ***  05/24/06  *******************************************************
     c                   if        cltid = '136'
     c*** begin add *** 10/16/07 *********************************************************
     c                   movel(p)  rpcap#        xx1a              1
     c                   if        xx1a = 'C'
     c*** end add ***** 10/16/07 *********************************************************
     c                   movel(p)  rpcap#        xx12             12
     c                   move (p)  xx12          xx8               8
     C*** Begin chg *** 10/23/13 *******************************************
     C*********          movel(p)  xx8           Policy#
     C                   movel(p)  xx8           xxPol25
     C*** End chg ***** 10/23/13 *******************************************
     c*** begin add *** 10/16/07 *********************************************************
     c                   endif
     c*** end add ***** 10/16/07 *********************************************************
     c                   endif                                                  cltid='136'
     C*** end add *****  05/24/06  *******************************************************
     C*
     C*** Begin add *** 07/03/06 *******************************************
     C*  Remove hyphens from the policy #.
     C*
     C                   clear                   cap
     C                   clear                   cap2
     C*****              movea     rpcap#        cap
     C*** Begin chg *** 10/23/13 *******************************************
     C*********          movea     policy#       cap
     C                   movea     xxPol25       cap
     C*** End chg ***** 10/23/13 *******************************************
     C                   z-add     1             t                 2 0
     C                   z-add     1             u                 2 0
     C*
     C*** Begin chg *** 10/23/13 *******************************************
     C*****t***          dowle     18
     C     t             dowle     25
     C*** End chg ***** 10/23/13 *******************************************
     C*
     C     cap(t)        ifne      '-'
     C*** Begin add *** 10/23/13 *******************************************
     C     u             andle     18
     C*** End add ***** 10/23/13 *******************************************
     C                   move      cap(t)        cap2(u)
     C                   add       1             u
     C                   endif
     C*
     C                   add       1             t
     C                   enddo
     C*
     C                   movea     cap2          Policy#
     C*** End add ***** 07/03/06 *******************************************
     C*
     C                   else
     c                   move      srfnd         FND
     c                   move      sremp#        EMP
     c                   move      srfyr         FYR
     C                   endif                                                  *in60=*off
     C*
     C                   endif                                                  *in60=*off
     C*** begin add ***  09/01/16  *********************************************
     C                   Eval      xxCHCN = 'N'
     C*** end add *****  09/01/16  *********************************************

     C*** Begin add *** 05/02/08 ******************************************
     C*  For an "ICR only" resubmission (srstat = 'I'), only create the
     C*  ICR records (skip all other records).
     C*
     C                   if        srstat <> 'I'
     C*** End add ***** 05/02/08 ******************************************
     C*
     C*  Load RDATA and call subroutine to write to WCSTATX
     C*                Header
     C                   eval      RDATA = Link + Header
     C                   exsr      Writer
     C*
     C*                Name
     C                   eval      RDATA = Link + Name
     C                   exsr      Writer
     C*
     C*                Address
     C                   eval      RDATA = Link + Address
     C                   exsr      Writer

     C*
     C*    Call subroutine to write all exposure recs associated with this emp#/fyr
     C                   exsr      Sr_Expo

     C*
     C*    Call subroutine to write all loss recs associated with this emp#/fyr
     C                   exsr      Sr_Loss

     C*
     C*    Call subroutine to write unit record associated with this emp#/fyr
     C                   exsr      Sr_Unit

     C*** Begin add *** 05/02/08 ******************************************
     C                   endif
     C*** End add ***** 05/02/08 ******************************************

     C***  Begin Add  ***  01/17/07  ***************************************
     C*** Call subroutine to write ICR record associated with this emp#/fy
     C                   exsr      Sr_Icr
     C***  End   Add  ***  01/17/07  ***************************************

     C*
     C*    Indicate submission as ASWG if unit ASWG flag is 'A'
     C                   if        SRASWG = 'A'
     C                   seton                                        20
     C                   endif
     C
     C                   endif
     C*
     C                   endsr                                                  LnkHdrNmAd

     C*==========================================================================
     C*  Sr_Expo  - subroutine to transfer WDSEP files to WCSTAT file
     C*==========================================================================
     C     Sr_Expo       begsr
     C*
     c*** begin add ***  06/10/09  **********************************************
     c*
     c* if reporting to NJ then find the earliest rate/mod dates for the 1st split
     c* period, which will be the ARD date.
     c*
     c                   move      *hival        xxmddt
     c                   move      *hival        xxrtdt
     c
     c                   if        @@stcd = '29'
     c
     C     key1          chain     PSE01                              60
     C                   dow       *in60 = *off
     c
     c                   if        sesplt = 0
     c
     c                   if        sertdt < xxrtdt
     c                   z-add     semddt        xxmddt
     c                   z-add     sertdt        xxrtdt
     c                   endif
     c
     c                   endif                                                  sesplt = 0
     c
     C     key1          reade     PSE01                                  60
     C                   enddo
     c
     c
     c                   endif                                                  @@stcd = '29'
     c
     c                   if        xxrtdt = *hival or xxandt = 0
     c                   z-add     0             xxmddt
     c                   z-add     0             xxrtdt
     c                   endif
     c*** end add *****  06/10/09  **********************************************
     C     key1          chain     PSE01                              60
     C                   dow       *in60 = *off
     C*
     C*  Assign data structure values
     C                   move      SECLAS        Class_code
     C                   z-add     SEEXMD        Exp_mod
     C                   z-add     SEMDDT        Mod_eff_dt
     C                   z-add     SERTDT        Rat_eff_dt
     c*** begin add ***  06/10/09  **********************************************
     c                   if        sesplt = 0 and xxrtdt <> 0
     c                   z-add     xxmddt        Mod_eff_dt
     c                   z-add     xxrtdt        Rat_eff_dt
     c                   endif
     c*** end add *****  06/10/09  **********************************************
     C*** begin add *** 08/18/25 *******************************************
     C                   z-add     SEMNRT        exp_Rate
     C*** end add ***** 08/18/25 *******************************************

     C*
     C*  Load RDATA and call subroutine to write to WCSTATX
     C                   eval      RDATA = Link + Exposure
     C                   exsr      Writer

     C*
     C     key1          reade     PSE01                                  60
     C                   enddo
     C*
     C                   endsr                                                  Sr_Expo
     C*
     C*==========================================================================
     C*  Sr_Loss - subroutine to transfer WDSLP files to WCSTAT file
     C*==========================================================================
     C     Sr_Loss       begsr
     C*
     C     key1          chain     PSL01                              60
     C                   dow       *in60 = *off
     C*
     C*** Begin add *** 02/22/07 *******************************************
     C*  Remove hyphens from the claim #.
     C*
     C                   clear                   clm
     C                   clear                   clm2
     C                   movea     slclm#        clm
     C*** beg chg *** 08/21/07 ************************
     C*******            z-add     1             t                 2 0
     C*******            z-add     1             u                 2 0
     C*******
     C*****t*            dowle     12
     C                   z-add     12            t                 2 0
     C                   z-add     12            u                 2 0
     C*
     C                   do        12
     C*** end chg *** 08/21/07 ************************
     C*
     C     clm(t)        ifne      '-'
     C                   move      clm(t)        clm2(u)
     C*** beg chg *** 08/21/07 ************************
     C*******            add       1             u
     C                   sub       1             u
     C*** end chg *** 08/21/07 ************************
     C                   endif
     C*
     C*** beg chg *** 08/21/07 ************************
     C*******            add       1             t
     C                   sub       1             t
     C*** end chg *** 08/21/07 ************************
     C                   enddo
     C*
     C                   movea     clm2          slclm#
     C*** End add ***** 02/22/07 *******************************************
     C*
     C*  Assign data structure values
     C                   move      SLCLAS        ClassCode
     C                   z-add     SLACDT        Acc_date
     C*** begin add *** 08/08/17 *******************************************
     C                   z-add     0             slrcvd
     C                   move      *blanks       slcas#
     C*** end add ***** 08/08/17 *******************************************

     C*
     C*  Load RDATA and call subroutine to write to WCSTATX
     C                   eval      RDATA = Link + Loss
     C                   exsr      Writer

     C*
     C     key1          reade     PSL01                                  60
     C                   enddo
     C*
     C                   endsr                                                  Sr_Loss
     C*
     C*==========================================================================
     C*  Sr_Unit - subroutine to transfer WMSQP files to WCSTAT file
     C*==========================================================================
     C     Sr_Unit       begsr
     C*
     C     key1          chain     PSQ01                              60
     C                   if        *in60 = *off
     C*
     C                   if        sqrpt# <> '1'
     C                   eval      sqepay = 0
     C                   eval      sqeoth = 0
     C                   eval      sqsubp = 0
     C                   eval      sqstdp = 0
     C                   endif

     C*
     C*  Load RDATA and call subroutine to write to WCSTATX
     C                   eval      RDATA = Link + Unit
     C                   exsr      Writer
     C                   add       1             Ttl_units
     C*
     C                   endif
     C*
     C                   endsr                                                  Sr_Unit
     C*
     C*==========================================================================
     C*  Sr_Submit  - subroutine to transfer submission record to WCSTAT file
     C*==========================================================================
     c     Sr_Submit     begsr
     C*
     C                   if        *in20 = *on
     C                   movel(p)  'WC'          kkey
     C     kkey          chain     SWC01                              60
     C*
     C                   if        WCFL13 = 'Y'
     C                   eval      Sub_ASWG = 'T'
     C                   else
     C                   eval      Sub_ASWG = 'P'
     C                   endif
     C*
     C                   endif                                                  *in20=*on

     C
     C                   if        recFound = *on

     C*  Load RDATA and call subroutine to write to WCSTATX
     C                   eval      RDATA = Filler + Submission
     C                   exsr      Writer
     C
     C                   endif                                                  recFound=*on

     C*
     C                   endsr                                                  Sr_Submit
     C*
     C*==========================================================================
     C*  Writer - subroutine to write a record to the WCSTAT file
     C*==========================================================================
     C     Writer        begsr
     C*
     C                   write     RSTAT
     C                   add       1             Dtl_rec#
     C*
     C                   endsr                                                  Writer
     C*
     C*==========================================================================
     C*  ETR (Electronic Transmittal Record) - This subroutine writes the ETR record.
     C*==========================================================================
     C     ETR_sr        begsr
     C*
     C                   eval      x = 1
     C*
     C                   eval      Data_rcv = '000' + @@stcd
     C*
     C     *MDY          move      dsdate        JulDt
     C                   move      JulDt         xxjuldt
     C                   movel     xxjuldt       version
     C                   cat       'V01':0       version
     C  n99              movel     srcarr        Data_prov
     C*
     C     *MDY          move      dsdate        IsoDt
     C                   move      IsoDt         xxisodt
     C                   move      xxisodt       Proc_dt
     C                   movel     @@mail        Email
     C                   movel     @@name        Contact
     C                   move      @@phn         Phone#
     C                   move      @@ext         Phone_ext
     C                   move      @@fax         Fax#
     C                   movel     @@adr         Prov_adr
     C*** Begin add *** 07/02/06 *******************************************
     C*  Retrieve the company FEIN.
     C*
     C*** Beg   add *** 03/11/22 *******************************************
     C***  coky          chain     pco01                              99
     C**  N99              movel     cofein        tpa_fein
                         prov_ind = qqDtaPvdr;
                         tpa_fein = %EDITC(%Dec(qqFein:9:0):'X')  ;
     C*** End   add *** 03/11/22 *******************************************
     C*** End add ***** 07/02/06 *******************************************
     C*
     C                   eval      RDATA = ETR
     C                   exsr      Writer
     C*
     C                   endsr                                                  Writer

     C*** Begin add *** 01/17/07 *******************************************
     C*==========================================================================
     C*  Sr_ICR - This subroutine writes the ICR record.
     C*==========================================================================
     C     Sr_Icr        BegSr

     C* Read WDSIP File to populate ICR Data Structure
     C     key1          chain     PSI01                              60
     C                   dow       *in60 = *off

     C* Retrieve the current loss record for the claim being processed.
     C     key1          chain     PSL01                              99
     C                   Dow       *in99 = *off and slclm# <> siclm#
     C                               and slupdt <> 'R'
     C     key1          reade     PSL01                                  99
     C                   Enddo

     C****ICR7A Data Begins*
     C                   move      '7'           Rec_Type_A
     C                   move      'A'           Sub_Rec_TypA
     C                   move      SICLM#        Claim_Nbr_A
     C                   move      SIRTYP        Res_Typ_Code
     C                   move      SIYREX        Yr_last_Exp
     C                   move      SLCLAS        Class_Cd_ICR                   WDSLP
     C                   move      SLINJ#        Injury_Code                    WDSLP
     C                   move      SITTYP        Tran_Typ_Cd
     C*** Begin add *** 05/02/08 ******************************************
     C                   move      SISEX         Gender_Cd
     C*** End add ***** 05/02/08 ******************************************
     C                   move      SLACDT        Accdt_Date                     WDSLP
     C                   move      SLCLST        Status_Code
     C                   move      SIDOD         Deced_Date
     C                   move      SIDTRP        Reprt_Date
     C                   move      SIDOB         Birth_Date
     C                   move      SIDTCL        Close_Date
     C                   move      SIMTST        Mth_Stl_Cd
     C                   move      SLLC1         Los_Cnd_Cd_Act                 WDSLP
     C                   move      SLLC2         Los_Cnd_Cd_Typ                 WDSLP
     C                   move      SLLC3         Los_Cnd_Cd_Rec                 WDSLP
     C                   move      SLLC4         Los_Cnd_Cd_Cov                 WDSLP
     C                   move      SLLC5         Los_Cnd_Cd_Stl                 WDSLP
     C                   move      SLJRST        Jur_St_Cd                      WDSLP
     C                   move      SLMCTP        MC_Org_Typ_Cd                  WDSLP
     C                   move      SLLUMP        Lum_Sum_Ind                    WDSLP
     C                   move      SLFRAU        Frd_Clm_Cd                     WDSLP
     C                   move      'A'           ASWG_Sub_Cd
     C****ICR7A Data Ends***

     C*Write ICR7A Data Structure to Flat File
     C* Load RDATA and call subroutine to write to WCSTATX
     C                   eval      RDATA = Link + ICR7A
     C                   exsr      Writer


     C****ICR7B Data Begins*
     C                   move      '7'           Rec_Type_B
     C                   move      'B'           Sub_Rec_TypB
     C                   move      SICLM#        Claim_Nbr_B
     C                   move      SLAWW         Avg_WkWg_Amt                   WDSLP
     C                   move      SLPART        Inj_Bdy_Prt                    WDSLP
     C                   move      SLNATR        Inj_Nat_Cd                     WDSLP
     C                   move      SLCAUS        Inj_Cos_Cd                     WDSLP
     C                   move      SIINCT        Inc_Cst_Tot
     C                   move      SISDS1        SI_Pct_Dis1
     C                   move      SISBM1        SI_Mbr_Cd1                     WDSLP
     C                   move      SISWK1        SI_Num_Wk1
     C                   move      SISLS1        SI_Los_Tot1
     C                   move      SIWKTB        Num_Wk_TBen
     C                   move      SISDS2        SI_Pct_Dis2
     C                   move      SISBM2        SI_Mbr_Cd2
     C                   move      SISWK2        SI_Num_Wk2
     C                   move      SISLS2        SI_Los_Tot2
     C****ICR7B Data Ends***

     C*Write ICR7B Data Structure to Flat File
     C* Load RDATA and call subroutine to write to WCSTATX
     C                   eval      RDATA = Link + ICR7B
     C                   exsr      Writer


     C****ICR7C Data Begins*
     C                   move      '7'           Rec_Type_C
     C                   move      'C'           Sub_Rec_TypC
     C                   move      SICLM#        Claim_Nbr_C
     C                   move      SINSDS        NI_Dis_Pct
     C                   move      SINSLS        NI_Inc_LsToT
     C                   move      SILIAB        Emp_Lia_Inc
     C                   move      SIVRHB        Voc_Rehab_Tot
     C                   move      SIPIB         PIB_Pd_ValDt
     C                   move      SIPVFP        PVFI_Pmt_Amt
     C                   move      SIFNRL        Fnr_Alw_Amt
     C                   move      SIRMRG        LS_RM_Amt
     C****ICR7C Data Ends***

     C*Write ICR7C Data Structure to Flat File
     C* Load RDATA and call subroutine to write to WCSTATX
     C                   eval      RDATA = Link + ICR7C
     C                   exsr      Writer


     C****ICR7D Data Begins*
     C                   move      '7'           Rec_Type_D
     C                   move      'D'           Sub_Rec_TypD
     C                   move      SICLM#        Claim_Nbr_D
     C                   move      SIINCI        Ind_Inc_Tot
     C                   move      SIINCM        Med_Inc_Tot
     C                   move      SIPDIN        PdTot_Ind2Val
     C                   move      SIPDMD        PdTot_Med2Val
     C                   move      SISSAM        SS_Off_Amt
     C                   move      SIPPR         PIA_PvRs_NPd
     C****ICR7D Data Ends***

     C*Write ICR7D Data Structure to Flat File
     C* Load RDATA and call subroutine to write to WCSTATX
     C                   eval      RDATA = Link + ICR7D
     C                   exsr      Writer


     C****ICR7E Data Begins*
     C                   move      '7'           Rec_Type_E
     C                   move      'E'           Sub_Rec_TypE
     C                   move      SICLM#        Claim_Nbr_E
     C                   move      SIBCR1        Ben_Cd_Rel1
     C                   move      SIBCD1        Ben_Cd_Dep1
     C                   move      SIBBD1        Ben_Bth_Dt1
     C                   move      SIBCR2        Ben_Cd_Rel2
     C                   move      SIBCD2        Ben_Cd_Dep2
     C                   move      SIBBD2        Ben_Bth_Dt2
     C                   move      SIBCR3        Ben_Cd_Rel3
     C                   move      SIBCD3        Ben_Cd_Dep3
     C                   move      SIBBD3        Ben_Bth_Dt3
     C                   move      SIBCR4        Ben_Cd_Rel4
     C                   move      SIBCD4        Ben_Cd_Dep4
     C                   move      SIBBD4        Ben_Bth_Dt4
     C                   move      SIBCR5        Ben_Cd_Rel2
     C                   move      SIBCD5        Ben_Cd_Dep2
     C                   move      SIBBD5        Ben_Bth_Dt2
     C                   move      SINMIN        Insured_Name
     C****ICR7E Data Ends***

     C*Write ICR7E Data Structure to Flat File
     C* Load RDATA and call subroutine to write to WCSTATX
     C                   eval      RDATA = Link + ICR7E
     C                   exsr      Writer


     C****ICR7F Data Begins*
     C                   move      '7'           Rec_Type_F
     C                   move      'F'           Sub_Rec_TypF
     C                   move      SICLM#        Claim_Nbr_F
     C                   move      SINMCR        Carrier_Name
     C                   move      SILSN         Wrkr_Lst_Nam
     C                   move      SIOCCP        Wrkr_Occptn
     C                   move      SISS#         SSN_Nbr
     C****ICR7F Data Ends***

     C*Write ICR7F Data Structure to Flat File
     C* Load RDATA and call subroutine to write to WCSTATX
     C                   eval      RDATA = Link + ICR7F
     C                   exsr      Writer


     C****ICR7G Data Begins*
     C                   move      '7'           Rec_Type_G
     C                   move      'G'           Sub_Rec_TypG
     C                   move      SICLM#        Claim_Nbr_G
     C                   move      SIHOSP        PdHosCos2Val
     C                   move      SILEGD        LE_Def_Amt
     C                   move      SILEGC        LE_Clm_Amt
     C                   move      SIGRSS        Tot_Grs_Inc
     C                   move      SIVIND        VocReh_IndAmt
     C                   move      SIVTRN        VocReh_TrnAmt
     C                   move      SIVEVL        VocReh_ValAmt
     C****ICR7G Data Ends***

     C*Write ICR7G Data Structure to Flat File
     C* Load RDATA and call subroutine to write to WCSTATX
     C                   eval      RDATA = Link + ICR7G
     C                   exsr      Writer


     C****ICR7H Data Begins*
     C                   move      '7'           Rec_Type_H
     C                   move      'H'           Sub_Rec_TypH
     C                   move      SICLM#        Claim_Nbr_H
     C                   move      SIDOH         Hire_date
     C                   move      SIEMST        Emp_Sts_Cd
     C                   move      SIDISP        PTDB_ValDt
     C                   move      SIPTPD        PPPB_ValDt
     C                   move      SIRHBP        PVRB_ValDt
     C                   move      SIPRMP        PPTB_ValDt
     C                   move      SIDTHP        PDBt_ValDt
     C****ICR7H Data Ends***

     C*Write ICR7H Data Structure to Flat File
     C* Load RDATA and call subroutine to write to WCSTATX
     C                   eval      RDATA = Link + ICR7H
     C                   exsr      Writer


     C****ICR7I Data Begins*
     C                   move      '7'           Rec_Type_I
     C                   move      'I'           Sub_Rec_TypI
     C                   move      SICLM#        Claim_Nbr_I
     C                   move      SISTLA        PStlAmt_ValDt
     C                   move      SIPHPD        PPC_ValDt
     C                   move      SIMETD        PAMV_ValDt
     C                   move      SIDMDP        PDMV_ValDt
     C                   move      SIIMDP        PIME_ValDt
     C                   move      SISURG        Srg_Cd
     C                   move      SIATTY        Att_Rep_Cd
     C                   move      SIATDT        Att_DFR_Dt
     C****ICR7I Data Ends***

     C*Write ICR7I Data Structure to Flat File
     C* Load RDATA and call subroutine to write to WCSTATX
     C                   eval      RDATA = Link + ICR7I
     C                   exsr      Writer


     C****ICR7J Data Begins*
     C                   move      '7'           Rec_Type_J
     C                   move      'J'           Sub_Rec_TypJ
     C                   move      SICLM#        Claim_Nbr_J
     C                   move      SIANNU        Ant_Prc_Amt
     C                   move      SISUMD        Sgl_Sum_Pdt
     C****ICR7J Data Ends***

     C*Write ICR7J Data Structure to Flat File
     C* Load RDATA and call subroutine to write to WCSTATX
     C                   eval      RDATA = Link + ICR7J
     C                   exsr      Writer

     C                   eval      #ICRs = #ICRs + 1


     C     key1          reade     PSI01                                  60
     C                   enddo

     C                   EndSr
     C*** End   add *** 01/17/07 *******************************************
     C*** begin add *** 03/13/09 *******************************************
     C*==========================================================================
     C*  NCCI states - This subroutine loads the NCCI states to an array.
     C*==========================================================================
     C     NCCI_st       begsr
     C*
     C                   clear                   NCCIst
     C                   eval      x = 1
     C                   eval      kktbid = 'WCST'
     C*
     C*** begin chg *** 10/22/09 *******************************************
     C**** gtky          chain     pgt01                              99
     C****               dow       *in99 = *off
     C     gtky          chain     pgt01                              98
     C                   dow       *in98 = *off
     C*** end   chg *** 10/22/09 *******************************************
     C*
     C                   if        gtstat = 'A'
     C*** begin chg *** 10/22/09 *******************************************
     C**** gtkey         chain     pst01                              99
     C**** n99              move      stnst         NCCIst(x)
     C**** n99              eval      x = x + 1
     C     gtkey         chain     pst01                              98
     C  n98              move      stnst         NCCIst(x)
     C  n98              eval      x = x + 1
     C*** end   chg *** 10/22/09 *******************************************
     C                   endif
     C*
     C*** begin chg *** 10/22/09 *******************************************
     C***  gtky          reade     pgt01                                  99
     C     gtky          reade     pgt01                                  98
     C*** end   chg *** 10/22/09 *******************************************
     C                   enddo
     C*
     C                   endsr                                                  Writer
     C*** end add ***** 03/13/09 *******************************************
