     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*
    H/TITLE F1491R - ClaimCheck Interface Program
    H**********************************************************************
    H*                                                                    *
    H* Called From: FA735R  - API: Bill Review & Repricing                *
    H* Calls to   : HPRBLSA - CodeReview Shell Program                    *
    H*                                                                    *
    H**********************************************************************
    H* Created on 06/11/2012 by C:251 for Project 114MER                  *
    H**********************************************************************
    H* REVISIONS:                                                         *
    H*                                                                    *
     H* 03/13/19 C:079 Support 099/77880                                   *
     H*                Zero out CAMRA on replaced lines                    *
    H*                                                                    *
     H* 10/29/17 C:079 Support 114/49807                                   *
     H*                Only process lines for the current recon            *
    H*                                                                    *
     H* 08/24/15 C:079 Support MC114 36895                                 *
     H*                Update CAMSEQ when writing FDCAP                    *
    H*                                                                    *
     H* 09/10/14 C:062 Support MC114 86683                                 *
     H*                Swap out CACPT4 usage.                              *
    H*                                                                    *
     H* 05/05/14 C:079 Support MC114 82109                                 *
     H*                Do not update FAEAP. That will be done in F1393R    *
     H*                                                                    *
    H* 02/27/14 C:079 Support MC114 84802                                 *
    H*                Fix how CAOTH2 is being populated                   *
    H*                                                                    *
    H* 02/26/14 C:079 Support MC114 82109                                 *
    H*                Load changes to amounts correctly                   *
    H*                                                                    *
    H* 04/16/13 C:079 Support MC114 82097                                 *
    H*                Do not delete possible duplicate messages           *
    H*                                                                    *
    H* 01/08/13 C:277 support 81393                                       *
    H*                Change message id to Left Justified                 *
    H*                                                                    *
    H* 12/06/12 c:280 support 81222                                       *
    H*                ensure eob print flag set                           *
    H*                                                                    *
    H*                                                                    *
    H* 09/04/12 C: 079 Project: 114MER                                    *
    H*                 1. Correct History Filtering logic                 *
    H*                 2. Correct how charges are loaded to the proc area *
    H*                                                                    *
    H**********************************************************************
     Ffaeal1    uf   e           k disk
     Ffaeep     o    e             disk
     Ffaeel     if   e           k disk    rename(pee01:pee02)
     Ffaehl     if   e           k disk
     Ffaeop     uf   e             disk
     Ffaeol4    if   e           k disk    rename(peo01:peo02) prefix(tm)
     Ffdcal13   uf   e           k disk
     Ffdcall    if   e           k disk    rename(pca01:pca02)
     Ffdcal19   uf a e           k disk    rename(pca01:pca03)
     Ffdcalf    if   e           k disk    rename(pca01:pca04)
     Ffddhl1    if a e           k disk
     Ffmcdl     if   e           k disk
     Ffmpdl     if   e           k disk
     Ffmprl     if   e           k disk
     Fftmil     if   e           k disk
     Ffddep     o    e             disk
     Ffddel     uf   e           k disk    rename(pde01:pde02)
     Ffddel1    if   e           k disk    rename(pde01:pde03)
     Ffdmgp     o    e             disk
     Ffdmgl1    uf   e           k disk    rename(pmg01:pmg02)
     Ffdmgl2    if   e           k disk    rename(pmg01:pmg03)
     Ffdchp     o    e             disk
     Ffteblck   if   e           k disk    rename(peb01:peb02)
     Ffddap     o    e             disk
     Ffddal4    uf   e           k disk    rename(pda01:pda02)
     Fftabl     if   e           k disk
     Fftgtl     if   e           k disk
     Ffddul     if a e           k disk
     Ffaeul     if   e           k disk
     Ffaesl3    if   e           k disk
     Ffdkhp     if a e             disk
     F*
     D dbfds           ds
     D dbrrn                 397    400b 0
    D*
     D optnm           c                   const('Resubmit EOMBs throu-
     D                                     gh ClaimCheck')
     D xxloc           s              3  0
     D xxprcd          s              7a   inz(*blanks)
     D xxmcod          s              7a   inz(*blanks)
    D*
     D xx_integ_code   ds
     D  xxcode                        5a
     D  xxind                         1a
     D  xxsts                         1a
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D*
     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Constants
    D*---------------------------------------------------------------------
     D cce_error       c                   'ClaimCheck encountered a -
     D                                     fatal error. Contact your -
     D                                     IT Department.'
     D che_warn        c                   'ClaimCheck encountered a -
     D                                     warning.'
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D kkco#           s                   like(caco#)
     D kksub#          s                   like(casub#)
     D kkfnd           s                   like(cafnd)
     D kkfyr           s                   like(cafyr)
     D kkcase          s                   like(cacase)
     D kkfein          s                   like(cafein)
     D kkmod           s                   like(camod)
     D kkeob#          s                   like(eoeob#)
     D kkrcn           s                   like(eorcn)
     D kkblst          s                   like(esblst) inz('P')
     D*
     D $$eob#          s                   like(eoeob#)
     D $$rcn           s                   like(eorcn)
     D*
     D kkcpt4          s                   like(prcpt4)
     D kkefdt          s                   like(prefdt)
     D kkefd1          s                   like(prefdt)
     D kkefd2          s                   like(prefdt)
     D kkss#           s                   like(cdss#)
     D kkdseq          s                   like(cadseq)
     D kkmsg#          s                   like(mimsg#)
     D kkmseq          s                   like(mimseq) inz(*zeros)
     D kklin#          s                   like(calin#)
     D kkseq#          s                   like(deseq#) inz(*hival)
     D hdseq#          s                   like(deseq#)
     D hdmseq          s                   like(mgmseq)
     D kkkey           s                   like(cskey)
     D kkkey1          s                   like(gtkey)
     D kktbid          s                   like(gttbid)
     D*
     D kkeob           s                   like(deeob)
     D kkeob1          s                   like(ebkey)
     D*
     D from_date       s                   like(eofrdt)
     D to_date         s                   like(eotodt)
     D from_date1      s                   like(eofrdt)
     D to_date1        s                   like(eotodt)
     D hdeob#          s                   like(eoeob#)
     D hdrcn           s                   like(eorcn)
     D chg_chrg        s                   like(cachrg)
     D chg_mra         s                   like(camra)
     D chg_paid        s                   like(capaid)
     D chg_oth2        s                   like(caoth2)
     D xxpay           s              1  0

     D ##ar            s              1
     D ##arls          s              1
     D ##ccod          s              1
     D ##crac          s              1
     D ##crvw          s              1
     D ##edi           s              1
     D ##freq          s              1
     D ##fsch          s              1
     D ##mcif          s              1
     D ##mcrv          s              1
     D ##med           s              1
     D ##mss           s              1
     D ##npwc          s              1
     D ##ntwk          s              1
     D ##os            s              1
     D ##prom          s              1
     D ##prvb          s              1
     D ##rhab          s              1
     D ##ur            s              1
     D ##wcdk          s              1
     D ##wcif          s              1
     D ##wcin          s              1
     D @@rrn           s              9  0
     D @@ckpe          s              1
     D @@pcod          s              3
     D @@chg           s              1
     D srdfld          s             80
     D srdlen          s              3  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D x               s              3  0 inz(1)
     D y               s              3  0 inz(1)
     D z               s              3  0 inz(1)
     D i               s              2  0 inz(0)
     D line_ret        s              3  0 inz(1)
     D len             s              3  0
     D code            s              1
     D first_pass      s              1
     D*
     D line            s              3  0 inz(0)
     D chartest        s              1
     D date_test1      s               d   inz datfmt(*usa)
     D date_test2      s              7  0
     D date_test3      s               d   inz datfmt(*iso)
     D char_fnd        s              3
     D char_fyr        s              3
     D char_case       s              7
     D char_eob#       s              9
     D char_rcn#       s              3
     D char_fein       s              9
     D char_mod        s              3
     D ckchg           s              1    inz(*blanks)
     D ckpend          s              1    inz(*blanks)
     D current         s              1
     D flag            s              1
     D assign_direct   s              1
     D flag_age        s              1
     D deny_mod        s              1
     D deny_orig       s              1    inz(*blanks)
     D xxstrdt         s              8s 0
     D xxmodcnt        s              1  0
     D cmp_proc        s              7
     D Hld_Dt_of_Serv  s              8s 0
     D Hld_chrg        s                   like(rs_chrg)
     D*** Beg Add *** 09/04/12 *********************************************
     D rs_chrg112      s             11  2
     D hld_chrg110     s                   like(rs_chrg)
     D*** End Add *** 09/04/12 *********************************************
     D Hld_hst         s              1
     D hold_dseq       s                   like(cadseq)
     D hold_lin#       s                   like(calin#)
     D hld_lin_ord     s                   like(rs_code_order)
     D*xxmsg#          s                   like(mgmsg#)
     D xx_eob_code     s                   like(deeob)
     D mult_flag       s              1    inz('N')
     D xxmod1          s                   like(dhmod1)
     D xxmod2          s                   like(dhmod1)
     D xxmod3          s                   like(dhmod1)
     D xxmod4          s                   like(dhmod1)
     D*
     D bill_number     s                   like(eoeob#) dim(100) inz(*zeros)
     D recon           s                   like(eorcn ) dim(100) inz(*zeros)
     D sequence        s                   like(cadseq) dim(100) inz(*zeros)
     D*** Begin Add *** 09/10/2014 **************************************************
     D cpt4            s                   like(cacpt4) dim(100) inz(*zeros)
     D*** End   Add *** 09/10/2014 **************************************************
     D*
     D dates           s                   like(cafdat) dim(100)
     D orders          s             18    dim(100)
     D*
     D integ_arr       s              8    dim(69) ctdata perrcd(1)
     D  integ_arr_eob  s              3    dim(69) alt(integ_arr)
     D*
     D bill_numbers    s             39
     D xxcmp           s             18
     D*
     D xx_Err_Lin      ds
     D err_sub                        3  0 inz(*zeros)
     D err_pos3_4                     2    inz(*blanks)
     D err_px_lin                     1  0 inz(*zeros)
     D err_clm_dx                     2  0 inz(*zeros)
     D*
     D err_line        ds
     D err_lin1                       8
     D err_lin2                       8
     D err_lin3                       8
     D err_lin4                       8
     D err_lin5                       8

     D Ca_Diag_Inf     ds
     D Ca_Diag_code                   7    inz(*blanks)
     D Ca_Diag_Sys                    1    inz('1')
     D Ca_Filler01                    5    inz(*blanks)
     D Ca_Diag_Desc                  28
     D Ca_Filler02                    1s 0
     D Ca_Diag_Stat                   2    inz(*blanks)
     D*
     D Proc_area       s            372    dim(125)

     DClaim_area       ds
     D Ca_Gmis_Claim                 26s 0 inz(*zeros)
     D Ca_Claim_Num                  30a
     D Ca_User_Id                     8a   inz('CCBATCH')
     D Ca_Cap_Sts                     1a   inz('3')
     D Ca_Return_Pgm                  8a   inz('CCB0100')
     D Ca_Sum_Scr_Opt                 1a   inz('Y')
     D Ca_Entry_From                  1a   inz('I')
     D Ca_Dt_of_Serv                  8s 0 inz(*zeros)
     D Ca_Dt_of_Birth                 8s 0
     D Ca_Sex                         1a   inz('U')
     D Ca_Clm_Diag                   44a   dim(12)
     D Ca_Provider                   16a   inz(*blanks)
     D Ca_Speciality                 10a   inz(*blanks)
     D Ca_Opt_Data                   14a   inz(*blanks)
     D Ca_Proc_Cnt                    3s 0 inz(*zeros)
     D Ca_Source_Pgm                  1a   inz(*blanks)
     D Ca_Claim_Sav                  13s 0 inz(*zeros)
     D Ca_Err_Lin                     8a   dim(5)
     D Ca_Sort_Field                 14a   inz(*blanks)
     D Ca_Acc_Id                     14a   inz('GMISDEFAULT')
     D Ca_Ovr_File_SW                 1a   inz(*blanks)
     D Ca_Shr_Cust_Ac                14a   inz(*blanks)
     D Ca_Pay_Pct_SW                  1a   inz(*blanks)
     D Ca_Ast_Surg_Sw                 1a   inz(*blanks)
     D Ca_Clm_Sws                     1a   inz('Y')
     D Ca_Rule_Err_Nm                10a   inz(*blanks)
     D Ca_SS_Clm_Sts                  1a   inz(*blanks)
     D Ca_SS_Match                  290a   inz(*blanks)
     D Ca_SS_More_Mch                 1a   inz(*blanks)
     D Ca_Filler03                   38a   inz(*blanks)
     D Ca_Ast_Surg_80                 1a   inz(*blanks)
     D Ca_Filler04                    8a   inz(*blanks)
     D Ca_SS_App_Aud                  1a   inz(*blanks)
     D Ca_Filler05                   14a   inz(*blanks)
     D Ca_Scr_Ind                     2s 0 inz(00)
     D Ca_Filler05A                  12a   inz(*blanks)
     D Ca_Shrd_Cs_Acc                14a   inz(*blanks)
     D Ca_Clm_Ovr_Fil                 1a   inz(*blanks)
     D Ca_Filler06                    3a   inz(*blanks)
     D Ca_Exp_Err_Pro                 1a   inz(*blanks)
     D Ca_Rec_Typ_Tot                 3s 0 inz(*zeros)
     D Ca_Cflt_Err_Sw                 1a   inz(*blanks)
     D Ca_Err_Level                   1a   inz(*blanks)
     D Ca_Syn_Err_Cnt                 8s 0 inz(*zeros)
     D Ca_Rul_Edt_Cnt                 8s 0 inz(*zeros)
     D Ca_Not_Edt_Cnt                 8s 0 inz(*zeros)
     D Ca_Dft_Mem_Num                30a   inz(*blanks)
     D Ca_Dft_Pos                     3a   inz(*blanks)
     D Ca_SS_Shrd_Acc                14a
     D Ca_Ast_Surg_81                 1a   inz(*blanks)
     D Ca_Ast_Surg_82                 1a   inz(*blanks)
     D Ca_Filler07                   12a   inz(*blanks)
     D*
     D In_Area         Ds
     D Pr_Med_Code                    7a   inz(*blanks)
     D Pr_Med_Code_Md                 2a   dim(4) inz(*blanks)
     D Pr_Dt_of_Serv                  8s 0 inz(*zeros)
     D Pr_Age                         3s 0 inz(*zeros)
     D Pr_Infant_Ind                  1a   inz(*blanks)
     D Pr_DX_Tbl                      8a   dim(4) inz(*blanks)
     D Pr_Sce_Err1                    2s 0 inz(*zeros)
     D Pr_Sce_Err2                    2s 0 inz(*zeros)
     D Pr_Sce_Age_Sts                 2s 0 inz(*zeros)
     D Pr_Sce_Ast_Sts                 1s 0 inz(*zeros)
     D Pr_Sce_Exp_Sts                 1s 0 inz(*zeros)
     D Pr_Sce_Obs_sts                 1s 0 inz(*zeros)
     D Pr_Sce_RVU_Sts                 1a   inz(*blanks)
     D Pr_Sce_Pay_Sts                 1a   inz(*blanks)
     D Pr_Code_Orig                   1a   inz(*blanks)
     D Pr_UandC_Val                  11s 0 inz(*zeros)
     D Pr_Cpt_Mod_Id                  1a   inz(*blanks)
     D Pr_Short_Desc                 28a   inz(*blanks)
     D Pr_Filler08                    1a   inz(*blanks)
     D Pr_Code_Order                  3a   inz(*blanks)
     D Pr_Pay_Pcnt                    3s 0 inz(*zeros)
     D Pr_Chrg                       11s 0 inz(*zeros)
     D Pr_Redis_Chg                  11s 0 inz(*zeros)
     D Pr_Alw_Amt                    11s 0 inz(*zeros)
     D Pr_Code_Sts                    1a   inz(*blanks)
     D Pr_Usr_Area                   10a   inz(*blanks)
     D Pr_Dupl_Flg                    1a   inz(*blanks)
     D Pr_Rvu                         5s 0 inz(*zeros)
     D Pr_Drp_Day_Rng                 3s 0 inz(*zeros)
     D Pr_Msg_code                    7a   dim(4) inz(*blanks)
     D Pr_Msg_Mod                     2a   dim(4) inz(*blanks)
     D Pr_Filler13                   24a   inz(*blanks)
     D Pr_Mod_Wrn_Ind                 2a   inz(*blanks)
     D Pr_Dup_Msg_Typ                 2a   inz(*blanks)
     D Pr_Max_Tim_Dup                 2s 0 inz(*zeros)
     D Pr_Filler09                    5a   inz(*blanks)
     D Pr_Hist_Ind                    1a   inz(*blanks)
     D Pr_Provider                   16a   inz(*blanks)
     D Pr_Specialty                  10a   inz(*blanks)
     D Pr_Tpldx_Stat                  2a   dim(4) inz(*blanks)
     D Pr_Tplpx_Stat                  2a   inz(*blanks)
     D Pr_Mcbdcd_Proc                 2a   inz(*blanks)
     D Pr_Ios_Proc                    1a   inz(*blanks)
     D Pr_Idxpx_Proc                  1a   inz(*blanks)
     D Pr_Ipxdx_Proc                  1a   inz(*blanks)
     D Pr_Filler10                   18a   inz(*blanks)
     D Pr_Px_Pos                      3a   inz(*blanks)
     D Pr_Px_Clm_Num                 30a   inz(*blanks)
     D Pr_Ast_at_Surg                 1a   inz(*blanks)
     D Pr_Orig_Order                  5s 0 inz(*zeros)
     D Pr_Splt_Mod_In                 1a   inz(*zeros)
     D Pr_Org_NVF_Vst                 7a   inz(*blanks)
     D Pr_MSR_Exmp_In                 1a   inz(*blanks)
     D Pr_Mue_Orig_Ln                 3a   inz(*blanks)
     D Pr_Filler11                    1a   inz(*blanks)
     D Pr_Filler12                   18a   inz(*blanks)
     D*
     D Rs_Area         Ds                  occurs(125)
     D Rs_Med_Code                    7a   inz(*blanks)
     D Rs_Med_Code_Md                 2a   dim(4)
     D Rs_Dt_of_Serv                  8s 0
     D Rs_Age                         3s 0
     D Rs_Infant_Ind                  1a
     D Rs_DX_Tbl                      8a   dim(4)
     D Rs_Sce_Err1                    2a
     D Rs_Sce_Err2                    2a
     D Rs_Sce_Age_Sts                 2a
     D Rs_Sce_Ast_Sts                 1a
     D Rs_Sce_Exp_Sts                 1a
     D Rs_Sce_Obs_sts                 1a
     D Rs_Sce_RVU_Sts                 1a
     D Rs_Sce_Pay_Sts                 1a
     D Rs_Code_Orig                   1a
     D Rs_UandC_Val                  11s 0
     D Rs_Cpt_Mod_Id                  1a
     D Rs_Short_Desc                 28a
     D Rs_Filler08                    1a
     D Rs_Code_Order                  3a
     D Rs_Pay_Pcnt                    3s 0
     D Rs_Chrg                       11s 0
     D Rs_Redis_Chg                  11s 0
     D Rs_Alw_Amt                    11s 0
     D Rs_Code_Sts                    1a
     D Rs_Usr_Area                   10a
     D Rs_Dupl_Flg                    1a
     D Rs_Rvu                         5s 0
     D Rs_Drp_Day_Rng                 3s 0
     D Rs_Msg_code                    7a   dim(4)
     D Rs_Msg_mod                     2a   dim(4)
     D Rs_Filler13                   24a   inz(*blanks)
     D Rs_Mod_Wrn_Ind                 2a
     D Rs_Dup_Msg_Typ                 2a
     D Rs_Max_Tim_Dup                 2s 0
     D Rs_Filler09                    5a
     D Rs_Hist_Ind                    1a
     D Rs_Provider                   16a
     D Rs_Specialty                  10a
     D Rs_Tpldx_Stat                  2a   dim(4)
     D Rs_Tplpx_Stat                  2a
     D Rs_Mcbdcd_Proc                 2a
     D Rs_Ios_Proc                    1a
     D Rs_Idxpx_Proc                  1a
     D Rs_Ipxdx_Proc                  1a
     D Rs_Filler10                   18a
     D Rs_Px_Pos                      3a
     D Rs_Px_Clm_Num                 30a
     D Rs_Ast_at_Surg                 1a
     D Rs_Orig_Order                  5a
     D Rs_Splt_Mod_In                 1a
     D Rs_Org_NVF_Vst                 7a
     D Rs_MSR_Exmp_In                 1a
     D Rs_Mue_Orig_Ln                 3a
     D Rs_Filler11                    1a
     D Rs_Filler12                   18a
     D Rs_Integ_Code                  8a   inz('CKNOPRC')
     D Rs_Integ_Code2                 8a   inz('*blanks')
     D Rs_eob_code                    3a
     D Rs_line_sts                    4a   inz(*blanks)
     D Rs_add_line                    1a   inz(*blanks)
     D Rs_edit_eob#                  30a
     D Rs_edit_lin#                   3a
     D*
     D R1_Area         Ds                  occurs(125)
     D R1_Med_Code                    7a   inz(*blanks)
     D R1_Med_Code_Md                 2a   dim(4)
     D R1_Dt_of_Serv                  8s 0
     D R1_Age                         3s 0
     D R1_Infant_Ind                  1a
     D R1_DX_Tbl                      8a   dim(4)
     D R1_Sce_Err1                    2a
     D R1_Sce_Err2                    2a
     D R1_Sce_Age_Sts                 2a
     D R1_Sce_Ast_Sts                 1a
     D R1_Sce_Exp_Sts                 1a
     D R1_Sce_Obs_sts                 1a
     D R1_Sce_RVU_Sts                 1a
     D R1_Sce_Pay_Sts                 1a
     D R1_Code_Orig                   1a
     D R1_UandC_Val                  11s 0
     D R1_Cpt_Mod_Id                  1a
     D R1_Short_Desc                 28a
     D R1_Filler08                    1a
     D R1_Code_Order                  3a
     D R1_Pay_Pcnt                    3s 0
     D R1_Chrg                       11s 0
     D R1_Redis_Chg                  11s 0
     D R1_Alw_Amt                    11s 0
     D R1_Code_Sts                    1a
     D R1_Usr_Area                   10a
     D R1_Dupl_Flg                    1a
     D R1_Rvu                         5s 0
     D R1_Drp_Day_Rng                 3s 0
     D R1_Msg_code                    7a   dim(4)
     D R1_Msg_mod                     2a   dim(4)
     D R1_Filler13                   24a
     D R1_Mod_Wrn_Ind                 2a
     D R1_Dup_Msg_Typ                 2a
     D R1_Max_Tim_Dup                 2s 0
     D R1_Filler09                    5a
     D R1_Hist_Ind                    1a
     D R1_Provider                   16a
     D R1_Specialty                  10a
     D R1_Tpldx_Stat                  2a   dim(4)
     D R1_Tplpx_Stat                  2a
     D R1_Mcbdcd_Proc                 2a
     D R1_Ios_Proc                    1a
     D R1_Idxpx_Proc                  1a
     D R1_Ipxdx_Proc                  1a
     D R1_Filler10                   18a
     D R1_Px_Pos                      3a
     D R1_Px_Clm_Num                 30a
     D R1_Ast_at_Surg                 1a
     D R1_Orig_Order                  5a
     D R1_Splt_Mod_In                 1a
     D R1_Org_NVF_Vst                 7a
     D R1_MSR_Exmp_In                 1a
     D R1_Mue_Orig_Ln                 3a
     D R1_Filler11                    1a
     D R1_Filler12                   18a
     D R1_Integ_Code                  8a
     D R1_Integ_Code2                 8a   inz('*blanks')
     D R1_eob_code                    3a
     D R1_line_sts                    4a   inz(*blanks)
     D R1_add_line                    1a   inz(*blanks)
     D R1_edit_eob#                  30a
     D R1_edit_lin#                   3a

     D mod_flag        s              1a

    C*****************************************************************
    C*  Parameter Lists
    C*****************************************************************
     C     *entry        plist
     C                   parm                    @@rrn
     C                   parm                    @@ckpe
     C                   parm                    @@pcod
     C                   parm                    @@chg
    C*****************************************************************
    C*  Key Lists
    C*****************************************************************
    C*
     C     caky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
    C*
     C     caky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkfein
     C                   kfld                    kkmod
    C*
     C     caky3         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
     C                   kfld                    kkdseq
    C*
     C     caky4         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kklin#
    C*
     C     caky5         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkdseq
    C*
     C     caky6         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
    C*
     C     caky7         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
    C*
     C     cdky          klist
     C                   kfld                    kkss#
    C*
    C*
     C     csky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey
    C*
     C     daky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
     C                   kfld                    kkdseq
    C*
     C     deky1         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    kkdseq
     C                   kfld                    kkseq#
    C*
     C     deky2         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    kkdseq
    C*
     C     deky3         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    kkdseq
     C                   kfld                    kkeob
    C*
     C     dhky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkdseq
    C*
     C     duky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkdseq
    C*
     C     eaky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
     C*
     C     eaky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    $$eob#
     C                   kfld                    $$rcn
     C*
     C     esky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    $$eob#
     C                   kfld                    $$rcn
     C                   kfld                    kkblst
    C*
     C     ebky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkeob1
    C*
     C     eeky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
    C*
     C     eeky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkseq#
    C*
     C     ehky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
    C*
     C     euky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
    C*
     C     mgky1         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    kkdseq
     C                   kfld                    kkseq#
    C*
     C     mgky2         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    kkdseq
     C*
     C     mgky3         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    kkdseq
     C                   kfld                    kkmsg#
    C*
     C     miky          klist
     C                   kfld                    kkmsg#
     C                   kfld                    kkmseq
    C*
     C     pdky          klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
    C*
     C     prky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkcpt4
     C                   kfld                    kkefdt
    C*
     C     prky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kkcpt4
    C*
     C     gtky          klist
     C                   kfld                    kkco#
     C                   kfld                    kktbid
     C                   kfld                    kkkey1
     C*
     C     eoky          klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
    C*
    C*****************************************************************
    C*  *LIKE Defines
    C*****************************************************************
     C                   setoff                                       90
    C*
    C* Call API for system flags
    C*
     C                   call      'SA001R'
     C                   parm      *blanks       ##wcif
     C                   parm      *blanks       ##mcif
     C                   parm      *blanks       ##wcin
     C                   parm      *blanks       ##ar
     C                   parm      *blanks       ##ntwk
     C                   parm      *blanks       ##ur
     C                   parm      *blanks       ##med
     C                   parm      *blanks       ##edi
     C                   parm      *blanks       ##wcdk
     C                   parm      *blanks       ##rhab
     C                   parm      *blanks       ##mss
     C                   parm      *blanks       ##fsch
     C                   parm      *blanks       ##prvb
     C                   parm      *blanks       ##npwc
     C                   parm      *blanks       ##os
     C                   parm      *blanks       ##arls
     C                   parm      *blanks       ##ccod
     C                   parm      *blanks       ##crvw
     C                   parm      *blanks       ##prom
     C                   parm      *blanks       ##mcrv
     C                   parm      *blanks       ##freq
     C                   parm      *blanks       ##crac
     C*
     C* Section 1 - Filtering - Determining what lines are going to be
     C*                         sent into the Claimcheck process.
     C*
     C                   exsr      filtering
     C*
     C* Here we will only continue with the ClaimCheck process, if there
     C* is at least one current line item eligible for ClaimCheck
     C* processing,
     C*
     C                   if        *in90 = *off
     C*
     C* Section 2 - Formatting - Populating the ClaimCheck Copybook
     C*
     C                   exsr      formatting
     C*
     C* Section 3 - Calling ClaimCheck
    C*
     C                   setoff                                       91
     C*
     C                   dou       *in90 = *off
     C*
     C                   call      'CCB0100I'
     C                   parm                    Claim_area
     C                   parm                    Proc_area
     C*
     C* Section 4 - Process Errors - Checking for various ClaimCheck errors
    C*
     C                   exsr      process_errors
     C*
     C                   enddo
     C*
     C* We don't need to do any further processing if a critical
     C* Error has been encountered.
     C*
     C                   if        *in91 = *off
     C*
     C* Section 5 - Reviewing and Interpreting the Results
     C*
     C                   if        ca_cap_sts <> '0'
     C                   exsr      result
     C                   endif
     C*
     C* Section 6 - Program end processing
     C*
     C                   exsr      program_end
     C*
     C                   endif
     C                   endif
     C*
     C* End the Program
     C*
     C                   eval      *inlr = *on
     C**********************************************************************
     C* Check_dates - Check Pre and Post op dates.
     C**********************************************************************
     C     Check_dates   begsr
     C*
     C* If code is within 10000-69999 or 99201-99499
     C*
     C                   if        cacpt4>='10000' and cacpt4<='69999'
     C                             or cacpt4>='99201' and cacpt4<='99499'
     C*
     C                   if        cafdat < from_date or cafdat > to_date
     C                   seton                                        91
     C                   endif
     C*
     C                   else
     C*
     C* If this is not the Same day code
     C*
     C     cafdat        lookup    dates(1)                               99
     C                   if        *in99 = *off
     C                   seton                                        91
     C                   endif
     C*
     C                   endif
     C*
     C                   endsr
     C**********************************************************************
     C* Check_range - Check code range and dates for history bill
     C**********************************************************************
     C     Check_range   begsr
     C*
     C* If code is within 10000-69999 or 99201-99499 ignore the line. This was selected
     C* by Sr Filter_history
     C*
     C                   if        cacpt4>='10000' and cacpt4<='69999'
     C                             or cacpt4>='99201' and cacpt4<='99499'
     C*
     C                   seton                                        91
     C*
     C                   else
     C*
     C* Ignore same day code . This was already selected by Sr Filter_history
     C*
     C     cafdat        lookup    dates(1)                               99
     C                   if        *in99 = *on
     C                   seton                                        91
     C*
     C* Ignore lines if not in range of 90 days back to 90 days fwd
     C*
     C                   elseif    cafdat < from_date1 or cafdat > to_date1
     C                   seton                                        91
     C                   endif
     C*
     C                   endif
     C*
     C                   endsr
     C**********************************************************************
     C* Filter_Current - Filter line items from current bill
     C**********************************************************************
     C     filter_currentbegsr
    C*
     C     @@rrn         chain(n)  peo01                              99
     C                   seton                                        90
     C                   if        *in99 = *off
     C*
     C* Initialize work fields
     C*
     C                   eval      kkco#=eoco#
     C                   eval      kksub#=eosub#
     C                   eval      kkfnd=eofnd
     C                   eval      kkfyr=eofyr
     C                   eval      kkcase=eocase
     C                   eval      kkeob#=eoeob#
     C                   eval      kkrcn=eorcn
     C                   eval      kkfein=eofein
     C                   eval      kkmod=eomod
     C                   eval      from_date = *zeros
     C                   eval      to_date = *zeros
     C                   clear                   dates
     C                   setoff                                       92
     C*
     C* Here we are reading thru the current bills line items
     C*
     C     caky1         chain     pca01
     C                   dow       not %eof(fdcal13)
     C*** Begin Add *** 09/10/2014 **********************************************
     C*
     C* Retrieve the procedure code if hospital bill
     C*
     C                   Exsr      GetProcCode
     C*** End   Chg *** 09/10/2014 **********************************************
     C*
     C* First lets see if the line can be passed into ClaimCheck.
     C*
     C                   eval      current = *on
     C                   exsr      qualify_line
     C                   if        *in91 = *off
     C*
     C* Setting off 90 to communicate that we will be calling ClaimCheck.
     C*
     C                   setoff                                       90
     C*
     C* Here we are keeping track of the number of lines, because we can't
     C* send more than 100 lines into ClaimCheck.
     C*
     C                   eval      line = line + 1
     C*
     C* This line is ok so let's 1) Come up with the date range we will
     C* use to filter the history line items or 2) Load dates into an
     C* array that we will use for same day line items.
     C*
     C                   select
     C                   when      cacpt4>='10000' and cacpt4<='69999'
     C*
     C* Assigning from date
     C*
     C     *cymd         move      cafdat        date_test1
     C                   subdur    1:*d          date_test1
     C     *cymd         move      date_test1    date_test2
     C*
     C                   if        from_date = 0 or date_test2 < from_date
     C                   eval      from_date = date_test2
     C                   endif
     C*
     C* Assigning to date
     C*
     C     *cymd         move      cafdat        date_test1
     C                   adddur    90:*d         date_test1
     C     *cymd         move      date_test1    date_test2
     C*
     C                   if        date_test2 > to_date
     C                   eval      to_date = date_test2
     C                   endif
     C*
     C                   when      cacpt4>='99201' and cacpt4<='99499'
     C*
     C* Assigning from date
     C*
     C     *cymd         move      cafdat        date_test1
     C                   subdur    90:*d         date_test1
     C     *cymd         move      date_test1    date_test2
     C*
     C                   if        from_date = 0 or date_test2 < from_date
     C                   eval      from_date = date_test2
     C                   endif
     C*
     C* Assigning to date
     C*
     C     *cymd         move      cafdat        date_test1
     C                   adddur    1:*d          date_test1
     C     *cymd         move      date_test1    date_test2
     C*
     C                   if        date_test2 > to_date
     C                   eval      to_date = date_test2
     C                   endif
     C*
     C                   other
     C*
     C                   endsl
     C* Populate date array for same day codes
     C*
     C                   eval      dates(x) = cafdat
     C                   eval      x = x + 1
     C*
     C* For all other codes assigning 1-90 back and 1-90 forward
     C*
     C** Get dates for comparing from to for all other codes in SR Filter_hist
     C*
     C     *cymd         move      cafdat        date_test1
     C                   subdur    90:*d         date_test1
     C     *cymd         move      date_test1    date_test2
     C*
     C                   if        from_date1= 0 or date_test2 < from_date1
     C                   eval      from_date1= date_test2
     C                   endif
     C*
     C* Assigning to date
     C*
     C     *cymd         move      cafdat        date_test1
     C                   adddur    90:*d         date_test1
     C     *cymd         move      date_test1    date_test2
     C*
     C                   if        date_test2 > to_date1
     C                   eval      to_date1= date_test2
     C                   endif
     C*
     C* Here we are adding the accepted line to the three arrays.
     C* These arrays will be used when loading the input_area and
     C* hist_bills (holds the historical bill numbers).
     C*
     C                   eval      bill_number(line) = caeob#
     C                   eval      recon(line) = carcn
     C                   eval      sequence(line) = cadseq
     C*** Begin Add *** 09/10/2014 **********************************************
     C                   eval      cpt4(line) = CAcpt4
     C*** End   Add *** 09/10/2014 **********************************************
     C*
     C* Stop process if we already have 100 lines
     C*
     C                   if        line = 100
     C                   seton                                        92
     C                   leave
     C                   endif
     C*
     C                   endif
     C*
     C     caky1         reade     pca01
     C                   enddo
     C                   endif
    C*
     C                   endsr
     C**********************************************************************
     C* Filter_History - Filter line items from history bills
     C**********************************************************************
     C     filter_historybegsr
     C*
     C                   eval      hdeob# = 0
     C                   eval      hdrcn = 0
    C*
     C     caky2         chain     pca02                              99
     C                   dow       *in99 = *off
     C*
     C* We want to make sure that no current line items are picked up.
     C* Current line items were looked at in filter_current.
     C*
     C                   if        caeob# <> eoeob# or carcn <> eorcn
     C*** Begin Add *** 09/10/2014 **********************************************
     C*
     C* Retrieve the procedure code if hospital bill
     C*
     C                   Exsr      GetProcCode
     C*** End   Chg *** 09/10/2014 **********************************************
     C*
     C* First lets see if the line can be passed into ClaimCheck.
     C*
     C                   eval      current = *off
     C                   exsr      qualify_line
     C*
     C* History bill line items must be checked against date range.
     C*
     C                   if        *in91 = *off
     C                   exsr      check_dates
     C                   endif
     C*
     C                   if        *in91 = *off
     C*
     C* Check to see if we are dealing with a new bill.
     C*
     C                   if        hdeob# <> caeob# or hdrcn <> carcn
     C                   eval      hdeob# = caeob#
     C                   eval      hdrcn = carcn
     C                   endif
     C*
     C* Here we are keeping track of the number of lines, because we can't
     C* send more than 100 lines into CodeReview.
     C*
     C                   eval      line = line + 1
     C*
     C* Here we are adding the accepted line to the three arrays.
     C* These arrays will be used when loading the input_area and
     C* hist_bills (holds the historical bill numbers).
     C*
     C                   eval      bill_number(line) = caeob#
     C                   eval      recon(line) = carcn
     C                   eval      sequence(line) = cadseq
     C*** Begin Add *** 09/10/2014 **********************************************
     C                   eval      cpt4(line) = CAcpt4
     C*** End   Add *** 09/10/2014 **********************************************
     C*
     C* Stop process if we already have 100 lines
     C*
     C                   if        line = 100
     C                   seton                                        92
     C                   leave
     C                   endif
     C*
     C                   endif
     C                   endif
     C*
     C     caky2         reade     pca02                                  99
     C                   enddo
    C*
     C                   endsr
     C**********************************************************************
     C* Filter_Hist - Filter line items from history bills for all other codes
     C**********************************************************************
     C     filter_hist   begsr
     C*
     C                   eval      hdeob# = 0
     C                   eval      hdrcn = 0
    C*
     C     caky2         chain     pca02                              99
     C                   dow       *in99 = *off
     C*
     C* We want to make sure that no current line items are picked up.
     C* Current line items were looked at in filter_current.
     C*
     C                   if        caeob# <> eoeob# or carcn <> eorcn
     C*** Begin Add *** 09/10/2014 **********************************************
     C*
     C* Retrieve the procedure code if hospital bill
     C*
     C                   Exsr      GetProcCode
     C*** End   Chg *** 09/10/2014 **********************************************
     C*
     C                   eval      current = *off
     C*
     C* History bill line items must be checked against date and code range.
     C* This is to remove lines already selected by previous history selection
     C                   exsr      check_range
     C*
     C* First lets see if the line can be passed into ClaimCheck.
     C*
     C                   if        *in91 = *off
     C                   exsr      qualify_line
     C                   endif
     C*
     C                   if        *in91 = *off
     C*
     C* Check to see if we are dealing with a new bill.
     C*
     C                   if        hdeob# <> caeob# or hdrcn <> carcn
     C                   eval      hdeob# = caeob#
     C                   eval      hdrcn = carcn
     C                   endif
     C*
     C* Here we are keeping track of the number of lines, because we can't
     C* send more than 100 lines into CodeReview.
     C*
     C                   eval      line = line + 1
     C*
     C* Here we are adding the accepted line to the three arrays.
     C* These arrays will be used when loading the input_area and
     C* hist_bills (holds the historical bill numbers).
     C*
     C                   eval      bill_number(line) = caeob#
     C                   eval      recon(line) = carcn
     C                   eval      sequence(line) = cadseq
     C*** Begin Add *** 09/10/2014 **********************************************
     C                   eval      cpt4(line) = CAcpt4
     C*** End   Add *** 09/10/2014 **********************************************
     C*
     C* Stop process if we already have 100 lines
     C*
     C                   if        line = 100
     C                   leave
     C                   endif
     C*
     C                   endif
     C                   endif
     C*
     C     caky2         reade     pca02                                  99
     C                   enddo
    C*
     C                   endsr
    C**********************************************************************
     C*                   Section   1
    C* Filtering - This subroutine selects line items to pass to ClaimCheck
    C**********************************************************************
     C     filtering     begsr
    C*
     C                   exsr      filter_current
     C*
     C* If *in90 is on then, there is no sense in loading history if there
     C* are no lines in the current bill that are going to go thru CLaimCheck
     C*
     C* If *in92 is on then, there is no sense in loading history since the
     C* ClaimCheck line limit has been reached.
     C*
     C                   if        *in90=*off and *in92=*off
     C                   exsr      filter_history
     C                   endif
     C*
     C* Executing the SR for filtering history for all codes other than what have
     C* been selected above . This is to capture the case to have codes from
    C* 1-90 days fwd and 1-90 days backward. Same day history codes were
     C* captured in previous SR
     C*
     C                   if        *in90=*off and *in92=*off
     C                   exsr      filter_hist
     C                   endif
     C*
     C                   endsr
    C**********************************************************************
     C*                   Section   2
    C* Formatting - Populate ClaimCheck CopyBook and Calling ClaimCheck
    C**********************************************************************
     C     formatting    begsr
     C*
     C*---------------------------------------------------------------------
    C* Lets populate the copybook before calling ClaimCheck.
     C*---------------------------------------------------------------------
     C* Here we are populating the ClaimCheck interface area for claim level
     C*
     C                   exsr      Sr_load_Head
     C*
     C* Send Procedure level fields to claim check
     C*
     C* Now we are going to load current bill
     C                   eval      y = 1
     C     1             do        100           x
     C                   reset                   In_area
     C*
     C* We are jumping out if an eob# is zero
     C                   if        bill_number(x) = 0
     C                   leave
     C                   endif
     C*
     C* We only want to load the current bill now.
     C*
     C                   if        bill_number(x) = eoeob# and
     C                             recon(x) = eorcn
     C                   exsr      Sr_load_Proc
     C                   endif
     C*
     C                   enddo
     C*
     C* Now we are going to load history bill
     C     1             do        100           x
     C                   reset                   In_area
     C*
     C* We are jumping out if an eob# is zero
     C*
     C                   if        bill_number(x) = 0
     C                   leave
     C                   endif
     C*
     C* We only want to load the history bill now.
     C*
     C                   if        bill_number(x) <> eoeob# or
     C                             recon(x) <> eorcn
     C                   exsr      Sr_load_Proc
     C                   endif
     C*
     C                   enddo
     C*
     C* initialize occurences from y to 125
     C*
     C     y             do        125           x
     C                   reset                   In_area
     C                   move      in_area       proc_area(x)
     C                   enddo
     C*
     C                   endsr
     C*** Begin Add *** 09/10/2014 *****************************************
     C**********************************************************************
     C* GetProcCode - Get the procedure code if hospital bill
     C**********************************************************************
     C     GetProcCode   begsr
     C*
     C                   Eval      kkEob# = CAeob#
     C                   Eval      kkDseq  = CAdSeq
     C     duky          chain     pdu01                              99
     C                   If        *in99 = *Off
     C                   movel     DUrate        CAcpt4
     C                   EndIf
     C*
     C                   endsr
     C*** End   Add *** 09/10/2014 *****************************************
    C**********************************************************************
     C*                   Section   2
    C* Sr_Load_Head - load claim level copybook
    C**********************************************************************
     C     Sr_load_head  begsr
     C*
     C* Send Claim number
     C*
     C                   move      eoeob#        char_eob#
     C                   move      eorcn         char_rcn#
     C     char_eob#     cat(p)    '-':0         ca_claim_num
     C                   cat       char_rcn#:0   ca_claim_num
     C*
     C* Send Date of birth and sex
     C                   eval      kkss# = eoss#
     C     cdky          chain     pcd01                              99
     C                   if        *in99 = *off
     C*
     C                   eval      Flag_Age = 'N'
     C*
     C                   if        cddtob <> *zeros
     C     *cymd         move      cddtob        date_test3
     C     *iso          move      date_test3    Ca_Dt_of_Birth
     C                   else
     C     *cymd         move      eofrdt        date_test3
     C                   subdur    18:*y         date_test3
     C     *iso          move      date_test3    Ca_Dt_of_Birth
     C*
     C** Flagging this claim so that age edits by claimcheck can be overriden
     C*
     C                   eval      Flag_Age = 'Y'
     C                   endif
     C*
     C                   if        cdsex = 'M' or cdsex = 'F'
     C                   eval      Ca_Sex = cdsex
     C                   else
     C                   eval      Ca_Sex = 'U'
     C                   endif
     C*
     C                   else
     C                   eval      Ca_Dt_of_Birth =  *zeros
     C                   eval      Ca_Sex = 'U'
     C                   endif
     C*
     C* Send diagnosis code
     C                   if        eobtyp = 'N'
     C     ehky          chain     peh01                              99
     C                   if        *in99 = *off
     C     1             do        4             x
     C                   select
     C                   when      x = 1
     C                   movel     ehdia1        ca_diag_code
     C                   when      x = 2
     C                   movel     ehdia2        ca_diag_code
     C                   when      x = 3
     C                   movel     ehdia3        ca_diag_code
     C                   when      x = 4
     C                   movel     ehdia4        ca_diag_code
     C                   endsl
     C                   move      Ca_Diag_Inf   Ca_clm_diag(x)
     C                   enddo
     C                   endif
     C*
     C                   else
     C     euky          chain     peu01
     C                   if        %found(faeul)
     C     1             do        4             x
     C                   select
     C                   when      x = 1
     C                   movel     eudia1        ca_diag_code
     C                   when      x = 2
     C                   movel     eudia2        ca_diag_code
     C                   when      x = 3
     C                   movel     eudia3        ca_diag_code
     C                   when      x = 4
     C                   movel     eudia4        ca_diag_code
     C                   endsl
     C                   move      Ca_Diag_Inf   Ca_clm_diag(x)
     C                   enddo
     C                   endif
     C*
     C                   endif
     C*
     C* Populate rest of the occurances
     C*
     C     5             do        12            x
     C                   move      Ca_Diag_Inf   Ca_clm_diag(x)
     C                   enddo
     C*
     C* Populate error line with defaults
     C*
     C     1             do        5             x
     C                   move      xx_err_lin    Ca_Err_Lin(x)
     C                   enddo
     C*
     C* Send provider id
     C*
     C                   move      eofein        char_fein
     C                   move      eomod         char_mod
     C     char_fein     cat(p)    '-':0         Ca_Provider
     C                   cat       char_mod:0    Ca_provider
     C*
     C** Send Procedure Count
     C                   z-add     line          Ca_Proc_Cnt
     C*
     C** Send Account id
     C                   eval      kkco# =  eoco#
     C                   eval      kktbid = 'CCAC'
     C
     C                   select
     C                   when      eobtyp = 'N'
     C                   eval      kkkey1 = 'P'
     C
     C                   when      eobtyp = 'H'
     C                   eval      kkkey1 = 'F'
     C                   endsl
     C     gtky          chain     pgt01
     C                   if        %found(ftgtl)
     C                   eval      Ca_Acc_ID = gtcf01
     C                   endif
     C*
     C** Send Default member name (claim number )-> fnd-year-case
     C*
     C                   move (p)  eofnd         char_fnd
     C                   move      eofyr         char_fyr
     C                   move      eocase        char_case
     C*
     C     char_fnd      cat(p)    '-':0         Ca_Dft_Mem_Num
     C                   cat       char_fyr:0    Ca_Dft_Mem_Num
     C                   cat       '-':0         Ca_Dft_Mem_Num
     C                   cat       char_case:0   Ca_Dft_Mem_Num
     C                   endsr
    C**********************************************************************
     C*
    C* Sr_Load_Proc - load procedure level copybook
    C**********************************************************************
     C     Sr_load_proc  begsr
     C*
     C*
     C                   eval      kkeob# = bill_number(x)
     C                   eval      kkrcn = recon(x)
     C                   eval      kkdseq = sequence(x)
     C*
     C     caky3         chain(n)  pca01
     C                   if        %found(fdcal13)
     C* Send procedure code
     C*** Begin Chg *** 09/10/2014 **********************************************
     C**********         movel     cacpt4        pr_med_code
     C                   movel     cpt4(x)       pr_med_code
     C*** End   Chg *** 09/10/2014 **********************************************
     C*
     C* Send Proc code modifier
     C*
     C                   move      calin#        pr_code_order
     C*** Beg Chg *** 09/04/12 *********************************************
     C**************     eval      pr_chrg = cachrg
     C                   move      cachrg        pr_chrg
     C*** End Chg *** 09/04/12 *********************************************
     C*
     C                   eval      char_fein = *blanks
     C                   eval      char_mod = *blanks
     C*
     C                   move      cafein        char_fein
     C                   move      camod         char_mod
     C     char_fein     cat(p)    '-':0         pr_provider
     C                   cat       char_mod:0    pr_provider
     C*
     C                   movel     caeob#        pr_px_clm_num
     C*
     C                   if        cafdat <> 0
     C     *cymd         move      cafdat        date_test3
     C     *iso          move      date_test3    pr_dt_of_serv
     C                   else
     C                   move      '0000/00/00'  pr_dt_of_serv
     C                   endif
     C*
     C                   if        caeob# = eoeob#
     C*** Beg Add *** 10/29/17 **************************************
     C                   if        carcn = eorcn
     C*** End Add *** 10/29/17 **************************************
     C                   eval      pr_hist_ind = '0'
     C*** Beg Add *** 10/29/17 **************************************
     C                   else
     C                   eval      pr_hist_ind = '1'
     C                   endif
     C*** End Add *** 10/29/17 **************************************
     C                   eval      pr_specialty = eospcd
     C                   else
     C                   eval      pr_hist_ind = '1'
     C** Get specaility code from faeop for the history bill
     C     eoky          chain     peo02
     C                   if        %found(faeol4)
     C                   eval      pr_specialty = tmeospcd
     C                   endif
     C                   endif
     C*
     C                   endif
     C*
     C                   if        eobtyp = 'N'
     C     dhky          chain     pdh01
     C                   eval      Pr_Med_Code_Md(1) = dhmod1
     C                   eval      Pr_Med_Code_Md(2) = dhmod2
     C                   eval      Pr_Med_Code_Md(3) = dhmod3
     C                   eval      Pr_Med_Code_Md(4) = dhmod4
     C*
     C                   movel     ehdia1        pr_dx_tbl(1)
     C                   movel     ehdia2        pr_dx_tbl(2)
     C                   movel     ehdia3        pr_dx_tbl(3)
     C                   movel     ehdia4        pr_dx_tbl(4)
     C*
     C                   eval      pr_px_pos = dhpos
     C*
     C                   else
     C     duky          chain     pdu01
     C                   eval      Pr_Med_Code_Md(1) = dupmod
     C                   eval      Pr_Med_Code_Md(2) = dumod2
     C                   eval      Pr_Med_Code_Md(3) = dumod3
     C                   eval      Pr_Med_Code_Md(4) = dumod4
     C*
     C                   movel     eudia1        pr_dx_tbl(1)
     C                   movel     eudia2        pr_dx_tbl(2)
     C                   movel     eudia3        pr_dx_tbl(3)
     C                   movel     eudia4        pr_dx_tbl(4)
     C*
     C                   eval      pr_px_pos = dupos
     C                   endif
    C*
     C                   move      in_area       proc_area(y)
     C                   eval      y = y + 1
    C*
     C                   endsr
    C**********************************************************************
     C*                   P A S S   1
    C* Rs_Populate - Populate Results DS
    C**********************************************************************
     C     Rs_Populate   begsr
    C*
     C                   reset                   rs_area
     C                   reset                   r1_area
     C                   reset                   in_area
     C*
     C** We are loading current lines, then newly added lines followed by history lines
     C** in the results array
     C*
     C                   eval      x = 1
     C                   eval      y = 1
     C     y             occur     Rs_area
     C*
     C** Populate current EOB lines
     C     1             do        125           x
     C                   eval      in_area = proc_area(x)
     C                   if        Pr_Hist_ind = '0' and Pr_Code_orig = '0'
     C                   eval      Rs_area  = Proc_area(x)
     C                   eval      y = y + 1
     C     y             occur     Rs_area
     C                   endif
     C                   enddo
     C*
     C** Populate new lines
     C     1             do        125           x
     C                   eval      in_area = proc_area(x)
     C                   if        Pr_Hist_ind = '0' and Pr_Code_orig <> '0'
     C                   eval      Rs_area = Proc_area(x)
     C* Write the new lines to FDCAP
     C*
     C                   exsr      write_FDCAP
     C                   move      calin#        rs_code_order
     C                   eval      y = y + 1
     C     y             occur     Rs_area
     C                   endif
     C                   enddo
    C** Populate history lines
     C     1             do        125           x
     C                   eval      in_area = proc_area(x)
     C                   if        Pr_Hist_ind = '1'
     C                   eval      Rs_area = Proc_area(x)
     C                   eval      y = y + 1
     C     y             occur     Rs_area
     C                   endif
     C                   enddo
    C*
     C                   eval      line_ret = y-1
     C*
     C     1             do        line_ret      x
     C     x             occur     rs_area
     C     x             occur     r1_area
     C                   movel     rs_area       r1_area
     C                   enddo
     C*
     C                   endsr
    C**********************************************************************
     C*                   P A S S   2
    C* Rs_Process - Assign sample integration code to rs ds
    C**********************************************************************
     C     Rs_Process    begsr
     C*
     C* Read Results DS
     C*
     C     1             do        line_ret      x
     C     x             occur     rs_area
     C* Check payment status when between A-Z
     C                   select
     C                   when      rs_sce_pay_sts = 'A'
     C                   eval      xxcode = 'CKAGE'
     C*
     C                   when      rs_sce_pay_sts = 'D'
     C                   eval      xxcode = 'CKAST'
     C*
     C                   when      rs_sce_pay_sts = 'E'
     C                   eval      xxcode = 'CKAAS'
     C*
     C                   when      rs_sce_pay_sts = 'F'
     C                   eval      xxcode = 'CKNVF'
     C*
     C                   when      rs_sce_pay_sts = 'G'
     C                   eval      xxcode = 'CKCCI'
     C*
     C                   when      rs_sce_pay_sts = 'H'
     C                   eval      xxcode = 'CKCCM'
     C*
     C                   when      rs_sce_pay_sts = 'I'
     C                   eval      xxcode = 'CKINC'
     C*
     C                   when      rs_sce_pay_sts = 'J'
     C                   eval      xxcode = 'CKAGE'
     C*
     C                   when      rs_sce_pay_sts = 'M'
     C                   eval      xxcode = ' CKME'
     C*
     C                   when      rs_sce_pay_sts = 'O'
     C                   eval      xxcode = 'CKPST'
     C*
     C                   when      rs_sce_pay_sts = 'P'
     C                   eval      xxcode = 'CKPRE'
     C*
     C                   when      rs_sce_pay_sts = 'R'
     C                   eval      xxcode  = 'CKREB'
     C*
     C                   when      rs_sce_pay_sts = 'S' OR rs_sce_pay_sts = 'T'
     C                   eval      xxcode = 'CKSEX'
     C*
     C                   when      rs_sce_pay_sts = 'U'
     C                   select
     C                   when      rs_dup_msg_typ='01'
     C                   eval      xxcode = 'CKUBD'
     C                   when      rs_dup_msg_typ='02'
     C                   eval      xxcode = 'CKBCD'
     C                   when      rs_dup_msg_typ='03'
     C                   eval      xxcode = 'CKMAL'
     C                   when      rs_dup_msg_typ='04'
     C                   eval      xxcode = 'CKMDO'
     C                   endsl
     C*
     C                   when      rs_sce_pay_sts = 'V'
     C                   eval      xxcode = 'CKVIS'
     C*
     C                   when      rs_sce_pay_sts = 'W'
     C                   eval      xxcode = 'CKPXD'
     C*
     C                   when      rs_sce_pay_sts = 'X'
     C                   eval      xxcode = 'CKDXP'
     C*
     C                   when      rs_sce_pay_sts = 'Y'
     C                   eval      xxcode = 'CKMUE'
     C*
     C** Interpret other claimcheck results
     C                   when      rs_sce_pay_sts = '1' OR
     C                             rs_sce_pay_sts = '2' OR
     C                             rs_sce_pay_sts = '3' OR
     C                             rs_sce_pay_sts = '4' OR
     C                             rs_sce_pay_sts = '5' OR
     C                             rs_sce_pay_sts = '9'
     C                   move      rs_sce_pay_stsxxpay
     C                   select
     C*
     C                   when      rs_sce_exp_sts = '1'
     C                   eval      rs_line_sts = 'DENY'
     C                   eval      rs_integ_code = 'CKEXPFC'
     C*
     C                   when      rs_sce_age_sts <> '00'  and flag_age ='N'
     C                             and rs_hist_ind = '0'
     C                   eval      rs_line_sts = 'DENY'
     C                   select
     C                   when      rs_sce_age_sts <> '05'
     C                   eval      rs_integ_code = 'CKAG1CC'
     C                   when      rs_sce_age_sts <> '06'
     C                   eval      rs_integ_code = 'CKAG2CC'
     C                   when      rs_sce_age_sts <> '07'
     C                   eval      rs_integ_code = 'CKAG3CC'
     C                   when      rs_sce_age_sts <> '08'
     C                   eval      rs_integ_code = 'CKAG4CC'
     C                   endsl
     C*
     C                   when      rs_sce_err1 = '01' and rs_hist_ind ='0'
     C                   eval      rs_line_sts = 'DENY'
     C                   eval      rs_integ_code = 'CKSX1CC'
     C*
     C                   when      rs_sce_err1 = '02' and rs_hist_ind ='0'
     C                   eval      rs_line_sts = 'DENY'
     C                   eval      rs_integ_code = 'CKSX2CC'
     C*
     C                   when      rs_sce_err2 = '03' and
     C                              rs_hist_ind = '0'
     C                   eval      rs_line_sts = 'PEND'
     C                   eval      rs_integ_code = 'CKCS1FC'
     C*
     C                   when      rs_sce_err2 = '05' and
     C                              rs_hist_ind = '0'
     C                   eval      rs_line_sts = 'PEND'
     C                   eval      rs_integ_code = 'CKCS2FC'
     C*
     C                   when      rs_sce_err2 = '04' and
     C                             rs_hist_ind = '0'
     C                   eval      rs_line_sts = 'PEND'
     C                   eval      rs_integ_code = 'CKUNLFC'
     C*
     C                   when      rs_sce_ast_sts = 'S' and rs_hist_ind = '0'
     C                   eval      rs_line_sts = 'DENY'
     C                   eval      rs_integ_code = 'CKASTFC'
     C*
     C                   when      rs_ast_at_surg = 'S' and rs_hist_ind = '0'
     C                   eval      rs_line_sts = 'DENY'
     C                   eval      rs_integ_code = 'CKAASFC'
     C*
     C**No other results returned
     C                   when      xxpay >= 1  and xxpay <= 9
     C                   if        rs_hist_ind = '0'
     C                   eval      rs_integ_code = 'CKNONCN'
     C                   eval      rs_line_sts = 'PAY'
     C*                  endsl
     C*
     C                   else
     C                   eval      rs_integ_code = 'CKNONHN'
     C                   eval      rs_line_sts = 'PAY'
     C                   endif
     C                   endsl
     C* Continue processing as code has already been assigned to this line
     C                   Iter
     C*
     C                   other
     C* Continue processing if no edit
     C                   Iter
     C*
     C                   endsl
     C*
     C* - Check messages on each line
    C*
     C                   exsr      Rs_Chk_Msg
     C*  Interpret the lines to assign complete itegration code
    C*
     C                   exsr      Rs_Interpret
     C*
     C* Set ckchg flag to be sent to FA735C
     C                   if        rs_line_sts ='PEND' or rs_line_sts = 'DENY'
     C                   eval      ckchg = 'Y'
     C                   endif
    C*
     C                   enddo
     C                   endsr
    C**********************************************************************
     C*
    C* Check_Msg - Check message on results DS
    C**********************************************************************
     C     Rs_Chk_Msg    Begsr
     C*
     C** Check message code
     C*
     C                   eval      assign_direct = 'N'
     C*
     C                   if        Rs_msg_code(1) <> *blanks or
     C                             (Rs_Dup_Msg_Typ <> *blanks and
     C                             Rs_Sce_Pay_Sts = 'U')
     C** Check if line is history or current
     C*
     C                   if        rs_hist_ind = '0'
     C                   eval      xxind = 'C'
     C                   else
     C                   eval      xxind = 'H'
     C                   endif
     C*
     C** Check which code to use for comparison
     C                   if        rs_sce_pay_sts <> 'U'
     C                   eval      cmp_proc = rs_msg_code(1)
     C                   else
     C                   eval      cmp_proc = rs_med_code
     C                   endif
     C*
     C                   else                                                   not dupl.blank msg
     C*
     C* Hard code assignments to the line  e.g CKASTCC
     C*
     C                   eval      assign_direct = 'Y'
     C                   endif
     C                   endsr
     C*
    C**********************************************************************
     C*
    C* GetMsgInf - Get FDCAP Information for the code that caused edit
    C**********************************************************************
     C     GetMsgInf     Begsr
     C*
     C                   if        rs_edit_eob# = *blanks
     C                   eval      khmlin = calin#
     C                   eval      khmcds = cadseq
     C                   eval      khmceb = caeob#
     C                   else
     C*
     C                   movel     rs_edit_eob#  kkeob#
     C                   move(p)   rs_edit_lin#  kklin#
     C*
     C     caky4         chain(n)  pca03
     C                   if        %found(fdcal19)
     C                   eval      khmlin = calin#
     C                   eval      khmcds = cadseq
     C                   eval      khmceb = caeob#
     C                   endif
     C                   endif
     C*
     C                   endsr
    C**********************************************************************
     C*
    C* Rs_Interpret - Interpret each line of results Ds
    C**********************************************************************
     C     Rs_interpret  Begsr
     C*
     C** Interpret results with message code
     C*
     C** Check if multicode edit
     C                   if        assign_direct = 'N'
     C     1             do        line_ret      y
     C                   if        y <> x
     C     y             occur     r1_area
     C*
     C*** Include the check for pre-op and post op DOS validation
     C                   select
     C*
     C                   when      rs_sce_pay_sts = 'P'
     C* DOS should be one day greater than proc DOS else scan next line
     C                   if        r1_dt_of_serv - rs_dt_of_serv <> 1
     C                   iter
     C                   endif
     C*
     C                   when      rs_sce_pay_sts = 'O'
     C* DOS should be within 90 range of  proc DOS else scan next line
     C                   move      rs_dt_of_serv xxstrdt
     C     *iso          move      xxstrdt       date_test3
     C                   subdur    90:*d         date_test3
     C     *iso          move      date_test3    xxstrdt
     C*
     C                   if        r1_dt_of_serv < xxstrdt or
     C                             r1_dt_of_serv >= rs_dt_of_serv
     C                   iter
     C                   endif
     C*
     C                   other
     C* DOS should same as proc DOS else scan next line
     C                   if        r1_dt_of_serv <> rs_dt_of_serv
     C                   iter
     C                   endif
     C*
     C                   endsl
     C*
     C                   if        r1_med_code= cmp_proc
     C                   select
     C*
     C* This is a current line affecting rs_area line
     C                   when      r1_code_orig = '0' and r1_hist_ind = '0'
     C                   eval      xxsts = 'C'
     C                   eval      rs_line_sts= 'DENY'
     C*
     C* Get eob#/line for the line that caused the edit
     C                   eval      rs_edit_eob# = r1_px_clm_num
     C                   eval      rs_edit_lin# = r1_code_order
     C*
     C* This is a history line affecting rs_area line
     C                   when      r1_hist_ind = '1'
     C                   eval      xxsts = 'H'
     C                   eval      rs_line_sts= 'DENY'
     C                   eval      rs_edit_eob# = r1_px_clm_num
     C                   eval      rs_edit_lin# = r1_code_order
     C*
     C* This is a new line affecting rs_area line
     C                   when      r1_code_orig <> '0'
     C*
     C*
     C                   eval      xxsts= 'N'
     C                   eval      rs_line_sts= 'DENY'
     C                   eval      rs_edit_eob# = r1_px_clm_num
     C                   eval      rs_edit_lin# = r1_code_order
     C*
     C* Zero out charges of initial line if not history and add to the new line

     C                   eval      hld_chrg = rs_chrg
     C                   eval      hld_hst = rs_hist_ind
     C                   eval      hld_dt_of_serv =  rs_dt_of_serv
     C*
     C                   if        hld_hst = '0'
     C                   eval      rs_chrg = 0
     C                   endif
     C*
     C* Get the Y instance and add charges
     C     y             occur     rs_area
     C                   if        hld_hst = '0'
     C*** Beg Chg *** 09/04/12 *********************************************
     C****************** eval      rs_chrg =  rs_chrg + hld_chrg
     C                   move      hld_chrg      hld_chrg110
     C                   eval      rs_chrg =  rs_chrg + hld_chrg110
     C*** End Chg *** 09/04/12 *********************************************
     C                   eval      rs_add_line =  *blanks
     C                   eval      rs_dt_of_serv = hld_dt_of_serv
     C*
     C* This is to have the integration code of ADD for the new lines
     C*
     C                   select
     C                   when      r1_code_orig = '1'
     C                   eval      rs_integ_code2= 'CKRBADD'
     C                   when      r1_code_orig = '2'
     C                   eval      rs_integ_code2= 'CKSXADD'
     C                   when      r1_code_orig = '3'
     C                   eval      rs_integ_code2= 'CKAGADD'
     C                   endsl
     C*
     C*
     C* This is to handle ex 6 of rebundling i.e not to add the new line if a history line
     C* has been rebundled to a new line
     C*
     C                   else
     C                   eval      rs_add_line =  'N'
     C                   exsr      del_FDCAP
     C                   endif
     C*
     C* Get the (original) X instance for further processing
     C     x             occur     rs_area
     C*
     C                   endsl
     C                   eval      rs_integ_code = xx_integ_code
     C*
     C                   select
     C*
     C                   when      rs_code_orig = '0' and
     C                             rs_integ_code = 'CKAGECN'
     C                   eval      rs_integ_code = 'CKAGERC'
     C*
     C                   when      rs_code_orig = '0' and
     C                             rs_integ_code = 'CKSEXCN'
     C                   eval      rs_integ_code = 'CKSEXRC'
     C*
     C                   when      rs_code_orig = '3'
     C                   eval      rs_integ_code2= 'CKAGADD'
     C*
     C                   when      rs_code_orig = '2'
     C                   eval      rs_integ_code2= 'CKSXADD'
     C                   endsl
     C*
     C                   endif
     C                   endif
     C                   enddo
     C*
     C                   else
     C** Assign directly
     C                   select
     C                   when      rs_sce_pay_sts = 'D' and rs_hist_ind = '0'
     C                   eval      rs_integ_code = 'CKASTCC'
     C                   eval      rs_line_sts = 'DENY'
     C*
     C                   when      rs_sce_pay_sts = 'E' and rs_hist_ind = '0'
     C                   eval      rs_integ_code = 'CKAASCC'
     C                   eval      rs_line_sts = 'DENY'
     C*
     C                   when      rs_sce_pay_sts = 'A' and rs_hist_ind = '0'
     C                             and rs_code_orig = '0'
     C                   eval      rs_integ_code = 'CKAGERC'
     C                   eval      rs_line_sts = 'DENY'
     C*
     C                   endsl
     C*
     C                   endif
     C*
     C                   endsr
    C**********************************************************************
     C*                   P A S S   3
    C* Rs_Mult_Mods - If multiple site mod expansion has occured
    C**********************************************************************
     C     Rs_Mult_Mod   Begsr
     C*
     C                   clear                   orders
     C                   eval      z = 1
     C*
     C* First re-populate r1_area for final comparing for multi mods
     C*
     C     1             do        line_ret      x
     C     x             occur     rs_area
     C     x             occur     r1_area
     C                   eval      r1_area = rs_area
     C                   enddo
     C*
     C* Next compare whole of r1_area taking one line at time from rs_area
     C*
     C     1             do        line_ret      x
     C     x             occur     rs_area
     C*
     C                   eval      deny_orig = ' '
     C*
     C     1             do        line_ret      y
     C     y             occur     r1_area
     C*
     C                   if        y <> x and
     C                             (rs_code_order = r1_code_order  and
     C                              rs_px_clm_num = r1_px_clm_num )
     C*
     C                   select
     C                   when      rs_integ_code <> r1_integ_code
     C                   eval      deny_orig = 'Y'
     C                   if        rs_hist_ind = '0'
     C                   eval      rs_integ_code2 = 'CKMDSPL'
     C                   eval      rs_add_line  = *blanks
     C                   exsr      write_FDCAP
     C                   endif
     C                   eval      hld_lin_ord = rs_code_order
     C                   move      calin#        rs_code_order
     C                   leave
     C                   when      rs_integ_code = r1_integ_code
     C                   eval      rs_add_line = 'N'
     C                   eval      deny_orig = 'N'
     C                   eval      hld_lin_ord = rs_code_order
     C                   iter
     C*
     C                   endsl
     C                   endif
     C                   enddo
     C*
     C** check if original line is being touched for the first time. This is to avoid repetition
     C** of messages in files and eob codes when expansion occurs
     C*
     C                   movel     Rs_Px_Clm_Num kkeob#
     C                   move(p)   hld_lin_ord   kklin#
     C*
     C                   eval      xxcmp = Rs_Px_Clm_Num + hld_lin_ord
     C     xxcmp         lookup    orders                                 99
     C*
     C                   if        deny_orig <> ' ' and  *in99 = *off
     C*
     C                   eval      orders(z) = xxcmp
     C                   eval      z = z + 1
     C*
     C                   select
     C                   when      deny_orig = 'Y' and rs_hist_ind = '0'
     C*
     C* deny the original line in FDCAP and write to all related files
     C*
     C     caky4         chain     pca03
     C                   if        %found(fdcal19)
     C                   eval      kkdseq = cadseq
     C*
     C                   eval      chg_paid = chg_paid - capaid
     C*** Beg Del *** 02/26/14 *********************************************
     C****************** eval      chg_oth2 = chg_oth2 - caoth2
     C*** End Del *** 02/26/14 *********************************************
     C*** Beg Chg *** 02/27/14 *********************************************
     C*****************  eval      caoth2 = cachrg
     C                   eval      caoth2 = capaid
     C*** End Chg *** 02/27/14 *********************************************
     C                   eval      capaid = 0
     C                   eval      chg_oth2 = chg_oth2 + caoth2
     C                   eval      caot2c = 'CCK'
     C                   eval      cachrg = 0
     C                   eval      cacscd = '2'
     C                   eval      cacchk = 'N'
     C                   update    pca03
     C*
     C                   exsr      del_fddep
     C                   exsr      del_fdmgp
     C*
     C                   eval      kkmsg# = 'CKSPLCC'
     C                   eval      kkeob = 'D33'
     C*
     C                   endif
     C*
     C                   exsr      write_fddep
     C                   exsr      write_fdmgp
     C*
     C** Deny original = 'N'
     C** Don't deny the original line and assign the code and update fdcap as per integ code
     C*
     C                   when      deny_orig = 'N' and rs_hist_ind = '0'
     C*
     C                   exsr      upd_fdcap
     C*
     C                   eval      kkdseq = cadseq
     C*
     C                   eval      kkmsg# = rs_integ_code
     C*
     C                   if        rs_integ_code <> 'CKNOPRC'
     C                             and rs_integ_code <> *blanks
     C** Assign EOB code
     C                   eval      i = %lookup(rs_integ_code:integ_arr)
     C                   if        i <> 0
     C                   eval      kkeob = integ_arr_eob(i)
     C                   endif
     C                   endif
     C*
     C                   exsr      write_fddep
     C                   exsr      write_fdmgp
     C*
     C                   endsl
     C*
     C** Report denied line to FDKHP (history file) irrespective of it being a history or
     C*  cuurent line.
     C                   if        deny_orig = 'Y'
     C                   exsr      wrt_hist_mult
     C                   endif
     C*
     C                   endif
     C*
     C                   enddo
     C                   endsr
    C**********************************************************************
     C*                   P A S S   4
    C* Rs_UpdFil - Update EOB codes/files etc
    C**********************************************************************
     C     Rs_UpdFil     Begsr
     C*
     C* SR update results area with apt EOB code and report to fdkhp if required
     C*
     C* Read results area and update EOB code from compile time array
     C*
     C     1             do        line_ret      x
     C     x             occur     rs_area
     C*
     C                   if        rs_integ_code <> 'CKNOPRC'
     C                             and rs_integ_code <> *blanks
     C*
     C** Assign EOB code to RS_area
     C*
     C                   eval      i = %lookup(rs_integ_code:integ_arr)
     C                   if        i <> 0
     C                   eval      rs_eob_code = integ_arr_eob(i)
     C                   endif
     C*
     C* Get detail sequence number from fdcap
     C*
     C                   if        rs_add_line = *blanks
     C                   movel     Rs_Px_Clm_Num kkeob#
     C                   move(p)   Rs_Code_Order kklin#
     C     caky4         chain(n)  pca03
     C                   if        %found(fdcal19)
     C                   eval      kkdseq = cadseq
     C                   else
     C                   eval      kkdseq = 0
     C                   endif
     C                   endif
     C*
     C*
     C** Remove fee schedule Eob codes/messages for lines denied and not history/new line
     C                   if        rs_line_sts = 'DENY' and rs_hist_ind = '0'
     C                             and rs_code_orig = '0'
     C*
     C                   exsr      del_fdmgp
     C                   exsr      del_fddep
     C                   endif
     C*
     C** Update/Write FDCAP for new/current lines
     C                   if        rs_hist_ind = '0' and rs_add_line <> 'N'
     C                   exsr      SrLine
     C                   endif
     C*
     C** Report every changed line to FDKHP (history file)
     C                   if        rs_line_sts = 'DENY' or rs_code_orig <> '0'
     C                             or (rs_integ_code <> 'CKNONCN' and
     C                             rs_integ_code <> 'CKNONHN')
     C*
     C                   exsr      Write_FDKHP
     C                   endif
     C*
     C                   endif
     C*
     C                   enddo
     C                   endsr
    C**********************************************************************
     C*
    C* Delete_FDMGP - Delete Eob Codes for denied line item
     C*
    C**********************************************************************
     C     del_fdmgp     begsr
     C*
     C* Delete unless its is the possible duplicate message xx00003
     C*
     C     mgky2         chain     pmg02
     C                   if        %found(fdmgl1)
     C* delete all messages from file
     C     mgky1         setgt     pmg02
     C     mgky2         readpe    pmg02
     C                   dow       not%eof(fdmgl1)
     C                   if        mgsrc <> 'K'
     C*** Beg Add *** 04/16/13 *********************************************
     C                             and %subst(mgmsg#:3:5) <> '00003'
     C*** End Add *** 04/16/13 *********************************************
     C                   delete    pmg02
     C                   endif
     C     mgky2         readpe    pmg02
     C                   enddo
     C                   endif
     C                   endsr
    C**********************************************************************
     C*
    C* Delete_FDDEP - Delete Eob Codes for denied line item
     C*
    C**********************************************************************
     C     del_fddep     begsr
     C*
     C     deky2         chain     pde02
     C                   if        %found(fddel)
     C*
     C     deky1         setgt     pde02
     C     deky2         readpe    pde02
     C                   dow       not %eof(fddel)
     C                   if        deesrc <> 'K'
     C                   delete    pde02
     C                   endif
     C     deky2         readpe    pde02
     C                   enddo
     C                   endif
     C*
     C                   endsr
    C**********************************************************************
     C*
    C* Write_FDKHP - Write to history file information of all changed lines
     C*               (whether it be history or current or new)
    C**********************************************************************
     C     Write_FDKHP   Begsr
     C*
     C                   clear                   pkh01
     C*
     C                   eval      khco# = kkco#
     C                   eval      khsub# = kksub#
     C                   eval      khfnd = kkfnd
     C                   eval      khfyr = kkfyr
     C                   eval      khcase = kkcase
     C                   eval      kheob# = kkeob#
     C                   eval      khrcn = carcn
     C                   eval      khdseq = kkdseq
     C                   eval      khlin# = kklin#
     C                   eval      khpeob# = eoeob#
     C                   eval      khprcd = rs_med_code
     C                   eval      khdos =%dec(%date(rs_dt_of_serv:*ISO):*CYMD)
     C                   eval      khpaid = capaid
     C                   eval      khckrf = 'C'
     C                   eval      khckhs = rs_hist_ind
     C                   eval      khckps = *blanks
     C                   eval      khckco = rs_code_orig
     C                   eval      khmcod = rs_msg_code(1)
     C                   eval      khmesg = rs_integ_code
     C                   eval      kheob  = rs_eob_code
     C*
     C     khmesg        chain     pmi01
     C                   if        rs_hist_ind = '0'
     C                   eval      khckac = mickca
     C                   else
     C                   eval      khckac = mickha
     C                   endif
     C*
     C                   exsr      GetMsgInf
     C*
     C                   eval      khstat = 'A'
     C                   eval      khprdt = 0
     C                   eval      khpgm  = q1pgm
     C                   eval      khuser = q1user
     C                   eval      khipgm  = q1pgm
     C                   eval      khiusr = q1user
     C*
     C                   time                    dsdttm
     C                   move      dsdate        date_test1
     C     *cymd         move      date_test1    khdt
     C     *cymd         move      date_test1    khidt
     C                   eval      khtime = dstime
     C                   eval      khitim = dstime
     C*
     C                   write     pkh01
     C*
     C                   endsr
    C**********************************************************************
     C*
    C* Wrt_hist_mult - Write tio history denied original line in case of multi site
     C*                 mods expansion
    C**********************************************************************
     C     Wrt_hist_mult Begsr
     C*
     C                   clear                   pkh01
     C*
     C                   eval      khco# = caco#
     C                   eval      khsub# = casub#
     C                   eval      khfnd = cafnd
     C                   eval      khfyr = cafyr
     C                   eval      khcase = cacase
     C                   eval      kheob# = caeob#
     C                   eval      khrcn = carcn
     C                   eval      khdseq = cadseq
     C                   eval      khlin# = calin#
     C                   eval      khpeob# = caeob#
     C                   eval      khprcd = cacpt4
     C                   eval      khdos = cafdat
     C                   eval      khpaid = capaid
     C                   eval      khckrf = 'C'
     C*
     C                   eval      khckps = *blanks
     C                   eval      khckco = '0'
     C                   eval      khmcod = *blanks
     C                   eval      khmesg = kkmsg#
     C                   eval      kheob  = kkeob
     C*
     C     khmesg        chain     pmi01
     C                   if        caeob# = eoeob#
     C                   eval      khckhs = '0'
     C                   eval      khckac = mickca
     C                   else
     C                   eval      khckhs = '1'
     C                   eval      khckac = mickha
     C                   endif
     C*
     C                   exsr      GetMsgInf
     C*
     C                   eval      khstat = 'A'
     C                   eval      khprdt = *zeros
     C                   eval      khpgm  = q1pgm
     C                   eval      khuser = q1user
     C                   eval      khipgm  = q1pgm
     C                   eval      khiusr = q1user
     C*
     C                   time                    dsdttm
     C                   move      dsdate        date_test1
     C     *cymd         move      date_test1    khdt
     C     *cymd         move      date_test1    khidt
     C                   eval      khtime = dstime
     C                   eval      khitim = dstime
     C*
     C                   write     pkh01
     C*
     C                   endsr
    C**********************************************************************
     C*                   Section   4
    C* Process_Errors - Pre-processing errors
    C**********************************************************************
     C     process_errorsbegsr
     C*
     C                   setoff                                         9091
     C*
     C                   do
     C
     C* Check to see if Fatal error
     C*
     C                   if        *in91 = *off and Ca_Cap_Sts = '8'
     C                   eval      @@ckpe = 'E'
     C                   eval      @@pcod = 'CCE'
     C                   exsr      write_faeep
     C                   seton                                        91
     C                   leave
     C                   endif
     C*
     C* Check to see if Warning
     C*
     C                   if        *in91 = *off and Ca_Cap_Sts= '9'
     C                   eval      @@pcod = 'CHE'
     C                   exsr      write_faeep
     C                   eval      @@pcod = *blanks
     C                   endif
    C*
     C                   enddo
    C*
     C                   endsr
     C**********************************************************************
     C* Program_end - Program end processing.
     C**********************************************************************
     C     Program_end   begsr
    C*
     C                   eval      @@chg  = ckchg
     C*
     C                   if        ckchg = 'Y'
     C                   eval      @@ckpe = 'Y'
     C                   eval      @@pcod = 'CRP'
     C                   endif
     C*
     C                   endsr
    C**********************************************************************
    C* Qualify_line - Sees if line should be passed into ClaimCheck
    C**********************************************************************
     C     Qualify_line  begsr
     C*
     C                   setoff                                       91
     C                   eval      deny_mod = 'N'
     C*
     C                   do
     C*
     C* For current bills only, check to see if line is overriden.
     C*
     C                   if        current = *on
     C                   movel(p)  cacscd        kkkey
     C     csky          CHAIN     pcs01                              99
     C                   if        csovrd = 'Y' or *in99 = *on
     C                   seton                                        91
     C                   leave
     C                   endif
     C                   endif
     C*
     C* Paid must be greater than 0
     C*
     C                   if        capaid = *zeros
     C                   seton                                        91
     C                   leave
     C                   endif
     C*
     C*
     C* We want to omit deleted bills and cancelled bills
     C*
     C                   eval      $$eob# = caeob#
     C                   eval      $$rcn  = carcn
     C     eaky2         chain(n)  pea01                              99
     C                   if        *in99 = *off
     C                   if        eaepfl = 'D' or eaepfl = 'X'
     C                   seton                                        91
     C                   leave
     C                   endif
     C                   endif
     C*
     C* We only want to include pended lines if pended to 'CRP'
     C*
     C                   if        current = *off
     C*** Beg Add *** 09/04/12 *********************************************
     C                             and caflag = 'P'
     C*** End Add *** 09/04/12 *********************************************
     C     esky          setgt     pes01
     C     esky          readpe(n) pes01                                  99
     C                   if        *in99 = *off  and esstcd <> 'CRP'
     C                   seton                                        91
     C                   leave
     C                   endif
     C                   endif
     C*
     C* We only want Physician or Hospital Line items
     C*
     C                   eval      kkeob# = caeob#
     C                   eval      kkdseq = cadseq
     C     dhky          chain     pdh01                              99
     C                   if        *in99 = *on
     C     duky          chain     pdu01                              99
     C                   if        *in99 = *on
     C                   seton                                        91
     C                   leave
     C*
     C                   else
     C* Durate should not be blank for Outpatient hospital bills
     C*
     C                   if        durate = *blanks
     C                   seton                                        91
     C                   leave
     C                   endif
     C*
     C                   endif
     C                   endif
     C*
     C* Skip if user defined code or flag is set to skip ClaimCheck for physician bill
     C*
     C                   movel(p)  cacode        kkcpt4
     C                   eval      kkefdt = cafdat
     C     prky1         setgt     ppr01
     C     prky2         readpe    ppr01                                  99
     C                   if        *in99 = *off and pr1ag = 'Y'
     C                             and eobtyp <> 'N'
     C                   seton                                        91
     C                   leave
     C                   endif
     C*
     C* Exclude dental codes
     C*
     C                   movel     cacode        chartest
     C                   if        chartest = 'D'
     C                   seton                                        91
     C                   leave
     C                   endif
     C*
     C* Exclude lines where number of units <> valid site mods
     C*
     C                   eval      xxmodcnt = 0
     C                   eval      xxmod1 = *blanks
     C                   eval      xxmod2 = *blanks
     C                   eval      xxmod3 = *blanks
     C                   eval      xxmod4 = *blanks
     C*
     C                   eval      kkeob# = caeob#
     C                   eval      kkdseq = cadseq
     C*
     C                   if        eobtyp = 'N'
     C     dhky          chain     pdh01
     C                   if        %found(fddhl1)
     C                   eval      xxmod1 = dhmod1
     C                   eval      xxmod2 = dhmod2
     C                   eval      xxmod3 = dhmod3
     C                   eval      xxmod4 = dhmod4
     C                   endif
     C*
     C                   elseif    eobtyp = 'H'
     C     duky          chain     pdu01
     C                   if        %found(fddul)
     C                   eval      xxmod1 = dupmod
     C                   eval      xxmod2 = dumod2
     C                   eval      xxmod3 = dumod3
     C                   eval      xxmod4 = dumod4
     C                   endif
     C*
     C                   endif
     C*
     C                   if        xxmod1 <> *blanks
     C                   eval      kktbid = 'SSMD'
     C* Check whether this is a valid site mod or not
     C                   eval      kkkey1 = xxmod1
     C     gtky          chain     pgt01
     C                   if        %found(ftgtl)
     C                   eval      xxmodcnt = xxmodcnt + 1
     C                   endif
     C                   endif
     C*
     C                   if        xxmod2 <> *blanks
     C                   eval      kkkey1 = xxmod2
     C     gtky          chain     pgt01
     C                   if        %found(ftgtl)
     C                   eval      xxmodcnt = xxmodcnt + 1
     C                   endif
     C                   endif
     C*
     C                   if        xxmod3 <> *blanks
     C                   eval      kkkey1 = xxmod3
     C     gtky          chain     pgt01
     C                   if        %found(ftgtl)
     C                   eval      xxmodcnt = xxmodcnt + 1
     C                   endif
     C                   endif
     C*
     C                   if        xxmod4 <> *blanks
     C                   eval      kkkey1 = xxmod4
     C     gtky          chain     pgt01
     C                   if        %found(ftgtl)
     C                   eval      xxmodcnt = xxmodcnt + 1
     C                   endif
     C                   endif
     C*
     C* Filter out line if site mods > 1 and and units > 1 and  site mods <> units
     C                   if        xxmodcnt > 1
     C                   if        caunts > 1 and xxmodcnt <> caunts
     C*
     C                   seton                                        91
     C* Set the line with eob, integration code, update files and deny the line
     C                   eval      deny_mod = 'Y'
     C                   eval      kkmsg# = 'CKSTMD'
     C                   eval      kkeob = 'D35'
     C                   eval      kkdseq = cadseq
     C*
     C                   exsr      wrt_hist_mult
     C*
     C                   if        caeob# = eoeob#                                     **current eob
     C*
     C                   exsr      del_fdmgp
     C                   exsr      del_fddep
     C                   exsr      write_fdmgp
     C                   exsr      write_fddep
     C*
     C                   eval      chg_paid = chg_paid - capaid
     C*** Beg Del *** 02/27/14 *********************************************
     C***************    eval      chg_oth2 = chg_oth2 - caoth2
     C*** End Del *** 02/27/14 *********************************************
     C*
     C                   eval      caochg = cachrg
     C*** Beg Chg *** 02/27/14 *********************************************
     C*****************  eval      caoth2 = cachrg
     C                   eval      caoth2 = capaid
     C*** End Chg *** 02/27/14 *********************************************
     C*
     C                   eval      chg_oth2 = chg_oth2 + caoth2
     C                   eval      capaid = 0
     C                   eval      caot2c = 'CCK'
     C                   update    pca01
     C*
     C                   endif
     C                   leave
     C                   else
     C* Set flag to indicate that multi site mod expansion will occur
     C                   eval      mod_flag = 'Y'
     C                   endif
     C*
     C                   endif
     C*
     C* Update CAOCHG with origianl charges.This will help us in restoring charges
     C* back if required
     C*
     C                   if        current = *on
     C                   eval      caochg = cachrg
     C                   update    pca01
     C                   endif
     C*
     C                   enddo
    C*
     C                   endsr
    C**********************************************************************
     C*                   Section   5
    C* Result  - Recieving and interpeting CodeReview Results
    C**********************************************************************
     C     Result        begsr
     C*
     C* Pass 1 - Populate the results DS in proper sequence i.e current lines first
     C*          followed by new lines and then history lines
    C*
     C                   exsr      Rs_populate
     C*
     C* Pass 2 - Process and assign each line partial integration code
    C*
     C                   exsr      Rs_Process
     C*
     C* Pass 3 - If multi site mod expansion
     C                   if        mod_flag = 'Y'
     C                   exsr      Rs_mult_Mod
     C                   endif
    C*
     C* Pass 4 - Update the EOB on each line, delete EOB/messages, write to
     C*          related files(FDCAP, FDKHP,FDMGP,FDDEP etc)
     C                   exsr      Rs_UpdFil
    C*
     C* Pass 5 - Update other header files for amount details
     C                   exsr      update_header
     C                   endsr
     C**********************************************************************
     C* Update_header  - Updates amount files like FAEOP and FAEAP
     C**********************************************************************
     C     Update_header begsr
     C*
     C* Update the Bill Header Transaction Amount File
     C*
     C*** Beg Del *** 05/05/14 *********************************************
     C************
     C*****eaky***       chain     pea01                              99
     C************       if        *in99 = *off
     C************       eval      eachrg = eachrg + chg_chrg
     C************       eval      eamra  = eamra  + chg_mra
     C************       eval      eapaid = eapaid + chg_paid
     C************       eval      eaoth2 = eaoth2 + chg_oth2
     C************       update    pea01
     C************       endif
     C*** End Del *** 05/05/14 *********************************************
     C*
     C* Update the Bill Header File
     C*
     C     @@RRN         chain     peo01                              99
     C                   if        *in99 = *off
     C                   eval      eochrg = eochrg + chg_chrg
     C                   eval      eomra  = eomra  + chg_mra
     C                   eval      eopaid = eopaid + chg_paid
     C                   eval      eooth2 = eooth2 + chg_oth2
     C                   update    peo01
     C                   endif
     C*
     C                   endsr
     C**********************************************************************
     C* Write_FDCAP - Write FDCAP record.
     C**********************************************************************
     C     Write_FDCAP   begsr
    C*
     C                   eval      kklin# = *hival
     C                   eval      kkeob# = eoeob#
    C*
     C     caky4         setgt     pca03
     C     caky7         readpe(n) pca03
    C*
     C                   if        %found(fdcal19)
     C                   eval      hold_lin# = calin# + 1
     C                   else
     C                   eval      hold_lin# = 1
     C                   endif
    C*
     C                   eval      kkdseq = *hival
     C     caky5         setgt     pca04
     C     caky6         readpe(n) pca04
    C*
     C                   if        %found(fdcalf)
     C                   eval      hold_dseq = cadseq + 1
     C                   else
     C                   eval      hold_dseq = 1
     C                   endif
     C*
     C                   clear                   pca03
    C*
     C                   eval      caco# = kkco#
     C                   eval      cafnd = kkfnd
     C                   eval      cafyr = kkfyr
     C                   eval      cacase = kkcase
     C                   eval      cacscd = '1'
     C                   eval      caunts =  1
     C                   eval      cafdat=%dec(%date(rs_dt_of_serv:*ISO):*CYMD)
     C                   eval      catdat = cafdat
     C                   eval      cardat = cafdat
    C*
     C                   eval      cafein = eofein
     C                   eval      camod = eomod
    C*
     C     pdky          chain     ppd01                              99
     C                   if        *in99 = *off
     C                   eval      caprty = pdtype
     C                   else
     C                   eval      caprty = *blanks
     C                   endif
    C*
     C                   eval      cadseq = hold_dseq
     C                   eval      caeob# = eoeob#
     C                   eval      cass# = eoss#
     C                   eval      caacdt = eoacdt
     C*
     C                   eval      carcfl = 'Y'
     C                   movel(p)  rs_med_code   kkcpt4
     C                   eval      kkefdt = cafdat
     C     prky1         setgt     ppr01
     C     prky2         readpe    ppr01                                  99
     C                   if        *in99 = *off
     C                   eval      carcfl = 'C'
     C                   endif
     C                   eval      casub# = kksub#
     C                   eval      cazipa = eozipa
     C                   eval      caast  = eoast
     C*
     C                   if        rs_line_sts= 'DENY'                          ** New line denied
     C                   eval      chg_chrg = chg_chrg - cachrg
     C                   eval      chg_oth2 = chg_oth2 - caoth2
     C                   eval      chg_mra = chg_mra - camra
     C                   eval      cachrg = 0
     C                   eval      camra  = 0
     C                   eval      caoth2 = 0
     C                   eval      caot2c = *blanks
     C                   eval      cacscd = '2'
     C*
     C                   else
     C*** Beg Chg *** 09/04/12 *********************************************
     C*****************  eval      cachrg = rs_chrg
     C*****************  eval      chg_chrg = chg_chrg - cachrg
     C                   move      rs_chrg       rs_chrg112
     C                   eval      cachrg = rs_chrg112
     C                   eval      chg_chrg = chg_chrg - cachrg
     C*** Beg Chg *** 09/04/12 *********************************************
     C
     C                   endif
     C*
     C                   eval      carcn = eorcn
     C                   eval      calin# = hold_lin#
     C*** Beg Add 08/24/15 *********************************************
     C                   eval      CAmSeq = CAlin#
     C*** End Add 08/24/15 *********************************************
     C                   eval      cacpt4 = rs_med_code
     C                   eval      cabprc = rs_med_code
     C                   eval      cacode = rs_med_code
     C                   eval      capmod = rs_med_code_md(1)
     C                   eval      caicd9 = rs_dx_tbl(1)
     C*
     C                   eval      cacchk = 'Y'
    C*
    C********* MAINTENANCE FIELDS ************************************
    C*
     C                   eval      castat = 'A'
     C                   eval      causer = q1user
     C                   eval      capgm = q1pgm
     C                   eval      caiusr = q1user
     C                   eval      caipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      catime = dstime
     C                   eval      caitim = dstime
     C                   move      dsdate        date_test1
     C     *cymd         move      date_test1    cadt
     C     *cymd         move      date_test1    caidt
    C*
     C                   write     pca03
     C*
     C                   endsr
     C*
     C**********************************************************************
     C* Del_FDCAP - delete FDCAP record where new line is not to be added
     C**********************************************************************
     C     Del_FDCAP     begsr
     C*
     C                   eval      kkeob#  =  eoeob#
     C                   move(p)   rs_code_order kklin#
     C*
     C     caky4         chain     pca03
     C                   if        %found(fdcal19)
     C                   delete    pca03
     C                   endif
     C*
     C                   endsr
     C**********************************************************************
     C* Upd_FDCAP - Update FDCAP record.
     C**********************************************************************
     C     Upd_FDCAP     begsr
     C*
     C     caky4         chain     pca03
     C                   if        %found(fdcal19)
     C*
     C                   if        rs_code_orig <> '0'
     C                   eval      cafdat=%dec(%date(rs_dt_of_serv:*ISO):*CYMD)
     C                   eval      catdat = cafdat
     C                   eval      cardat = cafdat
     C                   endif
     C*
     C                   if        rs_line_sts= 'DENY'
     C*
     C                   if        cacchk = 'Y'
     C                   eval      cacchk = 'D'
     C                   else
     C                   eval      cacchk = 'N'
     C                   endif
     C*
     C                   eval      chg_oth2 = chg_oth2 - caoth2
     C*
     C* if this line resulted in addition of a new line
     C                   eval      len = %len(%trim(rs_integ_code))
     C                   eval      code = %subst(rs_integ_code:len:1)
     C*
     C                   if        code = 'N'
     C                   eval      caoth2= 0
     C                   else
     C* else show savings
     C*** Beg Chg *** 09/04/12 *********************************************
     C*****************  eval      caoth2 = rs_chrg
     C                   move      rs_chrg       rs_chrg112
     C*** Beg Chg *** 02/27/14 *********************************************
     C****************   eval      caoth2 = rs_chrg112
     C                   eval      caoth2 = capaid
     C*** End Chg *** 02/27/14 *********************************************
     C*** End Chg *** 09/04/12 *********************************************
     C                   eval      chg_oth2 = chg_oth2 + caoth2
     C                   endif
     C*
     C                   eval      chg_paid = chg_paid - capaid
     C                   eval      chg_chrg = chg_chrg - cachrg
     C                   eval      chg_mra = chg_mra - camra
     C*** Beg Del *** 02/27/14 *********************************************
     C****************   eval      camra  = 0
     C*** End Del *** 02/27/14 *********************************************
     C                   eval      capaid = 0
     C                   eval      caot2c = 'CCK'
     C                   eval      cacscd = '2'
     C*** Beg Add *** 03/13/19 *********************************************
      /Free
                         If rs_chrg = 0 and CApaid = 0;
                            CAmra = 0;
                         Endif;
      /End-Free
     C*** End Add *** 03/13/19 *********************************************
     C                   else
     C*** Beg Chg *** 09/04/12 *********************************************
     C***************    eval      chg_chrg = chg_chrg - cachrg
     C***************    eval      chg_chrg = chg_chrg + rs_chrg
     C                   eval      chg_chrg = chg_chrg - cachrg
     C                   move      rs_chrg       rs_chrg112
     C                   eval      chg_chrg = chg_chrg + rs_chrg112
     C*** End Chg *** 09/04/12 *********************************************
     C                   endif
     C* If this is a multi mod line being updated, don't touch the charges
     C                   if        deny_orig = ' '
     C*** Beg Chg *** 09/04/12 *********************************************
     C*****************  eval      cachrg = rs_chrg
     C                   move      rs_chrg       rs_chrg112
     C                   eval      cachrg = rs_chrg112
     C*** End Chg *** 09/04/12 *********************************************
     C                   endif
     C*
     C                   time                    dsdttm
     C                   eval      catime = dstime
     C                   move      dsdate        date_test1
     C     *cymd         move      date_test1    cadt
     C*
     C                   eval      causer = q1user
     C                   eval      capgm = q1pgm
     C                   update    pca03
     C*
     C                   endif
     C*
     C                   endsr
     C**********************************************************************
     C* Upd_FDDAP - Update FDDAP record.
     C**********************************************************************
     C     Upd_Fddap     begsr
     C*
     C* Update the Bill Detail Transaction Amount File
     C*
     C     daky          chain     pda02
     C                   if        %found(fddal4)
     C                   eval      dachrg = cachrg
     C                   eval      damra  = camra
     C                   eval      daothr = caothr
     C                   eval      daoth2 = caoth2
     C                   eval      dandis = candis
     C                   eval      dapaid = capaid
     C*
     C                   update    pda02
     C                   endif
     C*
     C                   endsr
     C**********************************************************************
     C* Write_FAEEP - Write FAEEP record.
     C**********************************************************************
     C     Write_FAEEP   begsr
     C*
     C     eeky2         setgt     pee02
     C     eeky1         readpe    pee02                                  99
     C                   if        *in99 = *off
     C                   eval      hdseq# = eeseq# + 1
     C                   else
     C                   eval      hdseq# = 1
     C                   endif
     C*
     C                   clear                   pee01
     C*
     C                   eval      eeco#  = eoco#
     C                   eval      eesub# = eosub#
     C                   eval      eefnd  = eofnd
     C                   eval      eefyr  = eofyr
     C                   eval      eecase = eocase
     C                   eval      eess#  = eoss#
     C                   eval      eeacdt = eoacdt
     C                   eval      eeeob# = eoeob#
     C                   eval      eeseq# = hdseq#
     C*
     C** Move error line value to err_lin data structure
     C*
     C                   eval      err_lin1 = ca_err_lin(1)
     C                   eval      err_lin2 = ca_err_lin(2)
     C                   eval      err_lin3 = ca_err_lin(3)
     C                   eval      err_lin4 = ca_err_lin(4)
     C                   eval      err_lin5 = ca_err_lin(5)
     C*
     C                   select
     C                   when      @@PCOD = 'CCE'
     C                   eval      eenot1 = cce_error
     C                   eval      eenot2 = 'CA1000-ERROR-LINE = ' + err_line
     C                   when      @@PCOD = 'CHE'
     C                   eval      eenot1 = che_warn
     C                   eval      eenot2 = 'CA1000-ERROR-LINE = ' + err_line
     C                   endsl
     C*
     C                   eval      eercn  = eorcn
     C                   eval      eeprnt = 'N'
    C*
    C********* MAINTENANCE FIELDS ************************************
    C*
     C                   eval      eestat = 'A'
     C                   eval      eeuser = q1user
     C                   eval      eepgm = q1pgm
     C                   eval      eeiusr = q1user
     C                   eval      eeipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      eetime = dstime
     C                   eval      eeitim = dstime
     C                   move      dsdate        date_test1
     C     *cymd         move      date_test1    eedt
     C     *cymd         move      date_test1    eeidt
    C*
     C                   write     pee01
     C*
     C                   endsr
     C**********************************************************************
     C* Write_FDDAP - Write FDDAP record.
     C**********************************************************************
     C     Write_FDDAP   begsr
     C*
     C                   clear                   pda01
     C*
     C                   eval      daco#  = kkco#
     C                   eval      dasub# = kksub#
     C                   eval      dafnd  = kkfnd
     C                   eval      dafyr  = kkfyr
     C                   eval      dacase = kkcase
     C                   eval      daeob# = kkeob#
     C                   eval      darcn# = eorcn
     C                   eval      dass#  = eoss#
     C                   eval      daacdt = eoacdt
     C                   eval      dadseq = cadseq
     C                   eval      datrdt = cardat
     C                   eval      dachrg = cachrg
     C                   eval      damra  = camra
     C                   eval      dandis = candis
     C                   eval      daothr = caothr
     C                   eval      daoth2 = caoth2
     C                   eval      dapaid = capaid
    C*
    C********* MAINTENANCE FIELDS ************************************
    C*
     C                   eval      dastat = 'A'
     C                   eval      dauser = q1user
     C                   eval      dapgm = q1pgm
     C                   eval      daiusr = q1user
     C                   eval      daipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      datime = dstime
     C                   eval      daitim = dstime
     C                   move      dsdate        date_test1
     C     *cymd         move      date_test1    dadt
     C     *cymd         move      date_test1    daidt
    C*
     C                   write     pda01
     C*
     C                   endsr
     C**********************************************************************
     C* Write_FDDEP - Write Detail EOB note.
     C**********************************************************************
     C     Write_FDDEP   begsr
     C*
     C                   if        kkeob <> *blanks
     C*
     C     deky3         chain     pde03
     C                   if        not %found(fddel1)
     C*
     C     deky1         setgt     pde02
     C     deky2         readpe    pde02
     C                   if        %found(fddel)
     C                   eval      hdseq# = deseq# + 1
     C                   else
     C                   eval      hdseq# = 1
     C                   endif
     C*
     C                   clear                   pde01
     C*
     C                   eval      deco# = caco#
     C                   eval      desub# = casub#
     C                   eval      defnd = cafnd
     C                   eval      defyr = cafyr
     C                   eval      decase = cacase
     C                   eval      dess# = cass#
     C                   eval      deacdt = caacdt
     C                   eval      deeob# = caeob#
     C                   eval      dedseq = cadseq
     C                   eval      deseq# = hdseq#
     C*
     C                   eval      deeob = kkeob
     C                   eval      kkeob1= deeob
     C* Get the description from FTEBP for the code
     C     ebky          chain(n)  peb02
     C                   if        %found(fteblck)
     C                   eval      denot1 = ebdesc
     C                   eval      denot2 = ebdsc2
     C                   eval      denot3 = ebdsc3
     C                   eval      denot4 = ebdsc4
     C                   endif
     C*
     C                   eval      deesrc = 'K'
     C                   eval      dercn = carcn
    C*
    C********* MAINTENANCE FIELDS ************************************
    C*
     C                   eval      destat = 'A'
     C                   eval      deuser = q1user
     C                   eval      depgm = q1pgm
     C                   eval      deiusr = q1user
     C                   eval      deipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      detime = dstime
     C                   eval      deitim = dstime
     C                   move      dsdate        date_test1
     C     *cymd         move      date_test1    dedt
     C     *cymd         move      date_test1    deidt
     C*
     C*** BEGIN CHG *** 12/06/12 *******************************************
     C*
     C                   eval      deprnt = 'Y'
     C*
     C*** END   CHG *** 12/06/12 *******************************************
     C*
     C                   write     pde01
     C                   endif
     C                   endif
     C*
     C                   endsr
     C**********************************************************************
     C* Write_FDDHP - Write FDDHP record.
     C**********************************************************************
     C     Write_FDDHP   begsr
     C*
     C                   clear                   pdh01
    C*
     C                   eval      dhco# = kkco#
     C                   eval      dhsub# = kksub#
     C                   eval      dhfnd = kkfnd
     C                   eval      dhfyr = kkfyr
     C                   eval      dhcase = kkcase
     C                   eval      dheob# = kkeob#
     C                   eval      dhss#  = eoss#
     C                   eval      dhacdt = eoacdt
     C                   eval      dhdseq = cadseq
     C*                  move(p)   rs_px_pos     dhpos
     C                   eval      dhpos =  rs_px_pos
     C                   eval      dhprcd = rs_med_code
     C                   eval      dhmod1 = rs_Med_Code_Md(1)
     C                   eval      dhmod2 = rs_Med_Code_Md(2)
     C                   eval      dhmod3 = rs_Med_Code_Md(3)
     C                   eval      dhmod4 = rs_Med_Code_Md(4)
     C                   eval      dhrcfl = 'C'
     C                   eval      dhbprc = rs_med_code
     C                   eval      dhdxr1 = 'X'
     C                   eval      dhunts =  1
     C*
     C                   movel(p)  dhprcd        kkcpt4
     C                   eval      kkefdt = cafdat
     C     prky1         setgt     ppr01
     C     prky2         readpe    ppr01                                  99
     C                   if        *in99 = *off
     C                   eval      dhprds = prdsc1
     C                   eval      dhbprd = prdsc1
     C                   else
     C                   eval      dhprds = *blanks
     C                   eval      dhbprd = *blanks
     C                   endif
    C*
    C********* MAINTENANCE FIELDS ************************************
    C*
     C                   eval      dhstat = 'A'
     C                   eval      dhuser = q1user
     C                   eval      dhpgm = q1pgm
     C                   eval      dhiusr = q1user
     C                   eval      dhipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      dhtime = dstime
     C                   eval      dhitim = dstime
     C                   move      dsdate        date_test1
     C     *cymd         move      date_test1    dhdt
     C     *cymd         move      date_test1    dhidt
    C*
     C                   write     pdh01
     C*
     C                   endsr
     C**********************************************************************
     C* Write_FDDUP - Write FDDUP record.
     C**********************************************************************
     C     Write_FDDUP   begsr
     C*
     C                   clear                   pdu01
    C*
     C                   eval      duco# = kkco#
     C                   eval      dusub# = kksub#
     C                   eval      dufnd = kkfnd
     C                   eval      dufyr = kkfyr
     C                   eval      ducase = kkcase
     C                   eval      dueob# = kkeob#
     C                   eval      duss#  = eoss#
     C                   eval      duacdt = eoacdt
     C                   eval      dudseq = cadseq
     C                   eval      duunts =  1
     C                   eval      dupos  = rs_px_pos
     C                   eval      durate = rs_med_code
     C                   eval      dupmod = rs_Med_Code_Md(1)
     C                   eval      dumod2 = rs_Med_Code_Md(2)
     C                   eval      dumod3 = rs_Med_Code_Md(3)
     C                   eval      dumod4 = rs_Med_Code_Md(4)
     C                   eval      dubrat = rs_med_code
     C*
     C                   movel(p)  durate        kkcpt4
     C                   eval      kkefdt = cafdat
     C     prky1         setgt     ppr01
     C     prky2         readpe    ppr01                                  99
     C                   if        *in99 = *off
     C                   eval      dudesc = prdsc1
     C                   eval      dubprd = prdsc1
     C                   else
     C                   eval      dudesc = *blanks
     C                   eval      dubprd = *blanks
     C                   endif
    C*
    C********* MAINTENANCE FIELDS ************************************
    C*
     C                   eval      dustat = 'A'
     C                   eval      duuser = q1user
     C                   eval      dupgm = q1pgm
     C                   eval      duiusr = q1user
     C                   eval      duipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      dutime = dstime
     C                   eval      duitim = dstime
     C                   move      dsdate        date_test1
     C     *cymd         move      date_test1    dudt
     C     *cymd         move      date_test1    duidt
    C*
     C                   write     pdu01
     C*
     C                   endsr
     C**********************************************************************
     C* Write_FDMGP - Write to Message File.
     C**********************************************************************
     C     Write_FDMGP   begsr
     C*
     C     mgky3         chain     pmg03
     C                   if        not %found(fdmgl2)
     C*
     C     mgky1         setgt     pmg02
     C     mgky2         readpe    pmg02
     C                   if        %found(fdmgl2)
     C                   eval      hdmseq = mgmseq + 1
     C                   else
     C                   eval      hdmseq = 1
     C                   endif
     C*
     C                   clear                   pmg01
     C*
     C                   eval      mgco# = kkco#
     C                   eval      mgsub# = kksub#
     C                   eval      mgfnd = kkfnd
     C                   eval      mgfyr = kkfyr
     C                   eval      mgcase = kkcase
     C                   eval      mgdcs# = *zeros
     C                   eval      mgdseq = kkdseq
     C                   eval      mgmseq = hdmseq
     C                   if        rs_px_clm_num =  *blanks
     C                   eval      mgeob# =  caeob#
     C                   else
     C                   movel     rs_px_clm_num mgeob#
     C                   endif
     C*
     C*** Beg Chg *** 01/08/13 ***************************************
     C*****              eval      mgmsg# = kkmsg#
     C                   eval      mgmsg# = %trim(kkmsg#)
     C                   Movel     mgmsg#        mgmsg#
     C*** End Chg *** 01/08/13 ***************************************
     C*
     C                   eval      mgrcn = carcn
     C                   eval      mgsrc = 'K'
     C* Get the description from FTMIP for the message code
     C*
     C     kkmsg#        chain     pmi01
     C                   if        %found(ftmil)
     C                   eval      mgdesc = midesc
     C*
      /Free
             xxloc = 0;
             xxloc = %scan('XXXXX':mgdesc);
                If xxloc <> 0;
                  xxprcd = rs_med_code;
                  mgdesc = %replace(%trim(xxprcd):mgdesc: xxloc);
                Endif;
             xxloc = 0;
             xxloc = %scan('YYYYY':mgdesc);
                If xxloc <> 0;
                  xxmcod = rs_msg_code(1);
                  mgdesc = %replace(%trim(xxmcod):mgdesc: xxloc);
                Endif;
      /End-Free
     C*
     C                   else
     C                   eval      mgdesc = *blanks
     C                   endif
    C*
    C** Maintenance Fields
    C*
     C                   eval      mgstat = 'A'
     C                   eval      mguser = q1user
     C                   eval      mgpgm = q1pgm
     C                   eval      mgiusr = q1user
     C                   eval      mgipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      mgtime = dstime
     C                   eval      mgitim = dstime
     C                   move      dsdate        date_test1
     C     *cymd         move      date_test1    mgdt
     C     *cymd         move      date_test1    mgidt
     C*
     C                   write     pmg01
     C                   endif
     C*
     C                   endsr
     C**********************************************************************
     C* SrLine - Write Detail Line.
     C**********************************************************************
     C     SrLine        begsr
     C*
     C* If new line or line added due to mult mod expansion
     C*
     C                   if        rs_code_orig <> '0'
     C                             or rs_integ_code2 = 'CKMDSPL'
     C                   exsr      Write_Fddap
     C*
     C                   if        eobtyp = 'N'
     C                   exsr      Write_Fddhp
     C                   else
     C                   exsr      Write_Fddup
     C                   endif
     C*
     C                   exsr      Upd_Fdcap
     C* For current line
     C                   else
     C                   exsr      Upd_Fdcap
     C                   exsr      Upd_Fddap
     C*
     C                   endif
     C*
     C                   eval      kkeob = rs_eob_code
     C                   exsr      write_fddep
     C*
     C                   eval      kkmsg# = rs_integ_code
     C                   exsr      write_fdmgp
     C*
     C* This is for cases where we can have more than 1 integration codes e.g split mod case
     C*
     C                   if        rs_integ_code2 <> *blanks
     C                   eval      kkmsg# = rs_integ_code2
     C                   exsr      write_fdmgp
     C*
     C                   eval      i = %lookup(rs_integ_code2:integ_arr)
     C                   if        i <> 0
     C                   eval      xx_eob_code = integ_arr_eob(i)
     C                   endif
     C                   eval      kkeob = xx_eob_code
     C                   exsr      write_fddep
     C                   endif
     C*
     C                   endsr
     C*********************************************************************
**
CKSTMD  D35
CKREBCC D01
CKREBCH D02
CKREBCN D03
CKREBHC D02
CKREBHN D04
CKRBADD A01
CKUBDCC D05
CKUBDCH D06
CKUBDHC D06
CKBCDCC D05
CKBCDCH D06
CKBCDHC D06
CKMALCC D07
CKMALCH D08
CKMALHC D08
CKMDOCC D07
CKMDOCH D08
CKMDOHC D08
CKINCCC D11
CKINCCH D12
CKINCHC D12
 CKMECC D13
 CKMECH D14
CKMEHC  D14
CKCCICC D15
CKCCICH D16
CKCCIHC D16
CKCCMCC D17
CKCCMCH D18
CKCCMHC D18
CKASTFC D19
CKASTCC D19
CKAASFC D19
CKAASCC D19
CKAG1CC D21
CKAG2CC D21
CKAG3CC D21
CKAG4CC D21
CKAGERC D21
CKAGADD A03
CKSX1CC D22
CKSX2CC D22
CKSEXRC D22
CKSXADD A02
CKCS1FC P23
CKCS2FC P23
CKUNLFC P24
CKEXPFC D25
CKPRECC D27
CKPRECH D28
CKPREHC D28
CKPSTCC D29
CKPSTCH D30
CKPSTHC D30
CKVISCC D31
CKVISCH D32
CKVISHC D32
CKSPLCC D33
CKMDSPL A33
