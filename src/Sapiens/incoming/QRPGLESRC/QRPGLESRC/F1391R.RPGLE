    h* (C) Copyright Progressive Data Solutions, Inc. 2002
    H*
    H/TITLE F1391R - CodeReview Interface Program
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H*                                                                    *
    H* Called From: FA735R  - API: Bill Review & Repricing                *
    H* Calls to   : HPRBLSA - CodeReview Shell Program                    *
    H*                                                                    *
    H**********************************************************************
    H* Created on 07/03/2002 by c:62 for Project 114MDC                   *
    H**********************************************************************
    H* REVISIONS:                                                         *
    H*                                                                    *
    H* 12/06/12 c:280 support 81222                                       *
    H*                ensure eob print flag set                           *
    H*                                                                    *
    H* 04/20/09 c:62 Project 114MTC                                       *
    H*               Copy original units to new line                      *
    H*                                                                    *
    H* 11/02/03 c:79 Project FS265                                        *
    H*               Skip CodeReview if PR1AH = 'R' or 'B'                *
    H*                                                                    *
    H* 04/15/03 c:62 Project 114MFH                                       *
    H*               Added Promatch Processing                            *
    H*                                                                    *
    H* 02/04/03 c:62 Project 114MDC                                       *
    H*               Don't accept line items if they are delete/cancel    *
    H*                                                                    *
    H* 11/26/02 c:62 Project 114MDC                                       *
    H*               Keep from adding eob if it already exists.           *
    H*                                                                    *
    H* 11/25/02 c:62 Project 114MDC                                       *
    H*               Keep from adding a message if it already exists.     *
    H*                                                                    *
    H**********************************************************************
     Fsmcol     if   e           k disk
     Ffaeal1    uf   e           k disk
     Ffaeep     o    e             disk
     Ffaeel     if   e           k disk    rename(pee01:pee02)
     Ffaehl     if   e           k disk
     Ffaeop     uf   e             disk
     Ffdcal13   if   e           k disk
     Ffdcall    if   e           k disk    rename(pca01:pca02)
     Ffdcal19   uf a e           k disk    rename(pca01:pca03)
     Ffdcalf    if   e           k disk    rename(pca01:pca04)
     Ffddhl1    if a e           k disk
     Ffmcdl     if   e           k disk
     Ffmdil     if   e           k disk
     Ffmfnl     if   e           k disk
     Ffmpdl     if   e           k disk
     Ffmprl     if   e           k disk
     Ffmsml     if   e           k disk
     Fftmil     if   e           k disk
     Ffddep     o    e             disk
     Ffddel     if   e           k disk    rename(pde01:pde02)
     F*** BEGIN ADD *** 11/25/02 *******************************************
     Ffddel1    if   e           k disk    rename(pde01:pde03)
     F*** END   ADD *** 11/25/02 *******************************************
     Ffdmgp     o    e             disk
     Ffdmgl1    if   e           k disk    rename(pmg01:pmg02)
     F*** BEGIN ADD *** 11/25/02 *******************************************
     Ffdmgl2    if   e           k disk    rename(pmg01:pmg03)
     F*** END   ADD *** 11/25/02 *******************************************
     Ffdchp     o    e             disk
     Ffdchl2    uf a e           k disk    rename(pch01:pch02) prefix(c2:2)
     F*** BEGIN ADD *** 04/15/03 *******************************************
     Ffdchl1    uf a e           k disk    rename(pch01:pch03)
     F*** END   ADD *** 04/15/03 *******************************************
     Fftebl3cr  if   e           k disk    rename(peb01:peb02)
     Ffddap     o    e             disk
     Ffddal4    uf   e           k disk    rename(pda01:pda02)
     Ffdgxl1    if   e           k disk
     Ffdsxl1    if   e           k disk
     Fftabl     if   e           k disk
     F*** BEGIN ADD *** 04/15/03 *******************************************
     Fftpul1    if   e           k disk
     Fftpul2    if   e           k disk    rename(ppu01:ppu02)
     Fftpul3    if   e           k disk    rename(ppu01:ppu03)
     Fftpul4    if   e           k disk    rename(ppu01:ppu04)
     F*** END   ADD *** 04/15/03 *******************************************
     D dbfds           ds
     D dbrrn                 397    400b 0
    D*
     D optnm           c                   const('Resubmit EOMBs throu-
     D                                     gh CodeReview')
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

     Deob_recon1       ds
     D eob#1                          9s 0
     D hyphen                         1a
     D recon1                         3s 0

     Deob_recon        ds
     D co#                            3s 0
     D hyphen1                        1a
     D sub#                           5s 0
     D hyphen2                        1a
     D fnd                            3s 0
     D hyphen3                        1a
     D fyr                            3s 0
     D hyphen4                        1a
     D case                           7s 0
     D hyphen5                        1a
     D eob#                           9s 0
     D hypen6                         1
     D recons                         3s 0

    
     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Constants
    D*---------------------------------------------------------------------
     D cce_error       c                   'Program Stop Error, -
     D                                     Contact PDS'
     D che_error       c                   'Control Record 001 Error, -
     D                                     Contact PDS'
     D cbe_error       c                   'Bill Stop Error, -
     D                                     Contact PDS'
     D cee_error       c                   'Additional Edit Error, -
     D                                     Contact PDS'
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D k1co#           s                   like(chco#) inz(0)
     D kkco#           s                   like(caco#)
     D kksub#          s                   like(casub#)
     D kkfnd           s                   like(cafnd)
     D kkfyr           s                   like(cafyr)
     D kkcase          s                   like(cacase)
     D kkfein          s                   like(cafein)
     D kkmod           s                   like(camod)
     D kkeob#          s                   like(eoeob#)
     D kkrcn           s                   like(eorcn)
     D*** BEGIN ADD *** 02/04/03 *******************************************
     D $$eob#          s                   like(eoeob#)
     D $$rcn           s                   like(eorcn)
     D*** END   ADD *** 02/04/03 *******************************************
     D kkcpt4          s                   like(prcpt4)
     D kkefdt          s                   like(prefdt)
     D kkefd1          s                   like(prefdt)
     D kkefd2          s                   like(prefdt)
     D kkss#           s                   like(cdss#)
     D kkicd9          s                   like(diicd9)
     D kklost          s                   like(dilost) inz('D')
     D kkdseq          s                   like(cadseq)
     D kkmsg#          s                   like(mimsg#)
     D kkmseq          s                   like(mimseq) inz(*zeros)
     D kkcrst          s                   like(ebcrst)
     D kkcret          s                   like(ebcret)
     D kklin#          s                   like(calin#)
     D kkseq#          s                   like(deseq#) inz(*hival)
     D hdseq#          s                   like(deseq#)
     D hdmseq          s                   like(mgmseq)
     D kkcrvw          s                   like(gxcrvw)
     D kkst            s                   like(sxst)
     D kkkey           s                   like(cskey)
     D*** BEGIN ADD *** 11/26/02 *******************************************
     D kkeob           s                   like(deeob)
     D*** END       *** 11/26/02 *******************************************
     D from_date       s                   like(eofrdt)
     D to_date         s                   like(eotodt)
     D cut_off_date    s                   like(cafdat)
     D hdeob#          s                   like(eoeob#)
     D hdrcn           s                   like(eorcn)
     D hold_charge     s                   like(camra)
     D hold_mra        s                   like(camra)
     D hold_paid       s                   like(capaid)
     D hold_code       s                   like(cacpt4)
     D hold_dseq       s                   like(cadseq)
     D hold_lin#       s                   like(calin#)
     D chg_chrg        s                   like(cachrg)
     D chg_mra         s                   like(camra)
     D chg_paid        s                   like(capaid)
     D chg_oth2        s                   like(caoth2)
     D hold_crb#       s                   like(chcrb#)
     D hold_outp       s                   like(line_outp)
     D hold_fdat       s                   like(cafdat)
     D hold_tdat       s                   like(catdat)
     D hold_pos        s                   like(dhpos)
     D*** BEGIN ADD *** 04/20/09 *******************************************
     D hold_units      s                   like(dhunts)
     D*** END   ADD *** 04/20/09 *******************************************
     D*** BEGIN ADD *** 04/15/03 *******************************************
     D kkpatn          s                   like(pupatn)
     D kkpmst          s                   like(pupmst)
     D kkast           s                   like(puast)
     D kknot1          s                   like(denot1)
     D kkcrb#          s                   like(chcrb#) inz(*hival)
     D*** END   ADD *** 04/15/03 *******************************************

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
     D @@crpe          s              1
     D @@pcod          s              3
     D @@chg           s              1
     D @@crkb          s              2
     D @@pmkb          s              2
     D srdfld          s             80
     D srdlen          s              3  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D x               s              3  0 inz(1)
     D xx              s              3  0 inz(1)
     D y               s              3  0 inz(1)
     D*** BEGIN ADD *** 04/15/03 *******************************************
     D yy              s              3  0 inz(1)
     D*** END   ADD *** 04/15/03 *******************************************
     D z               s              2  0 inz(1)
     D line            s              3  0 inz(0)
     D bills           s              3  0 inz(0)
     D length          s             15  5 inz(56)
     D chartest        s              1
     D date_test1      s               d   inz datfmt(*usa)
     D date_test2      s              7  0
     D char_co#        s              3
     D char_sub#       s              5
     D char_fnd        s              3
     D char_fyr        s              3
     D char_case       s              7
     D char_eob#       s              9
     D char_rcn#       s              3
     D char_fein       s              9
     D char_mod        s              3
     D char_zip        s              9
     D char_zipa       s              5
     D char_zipb       s              4
     D char_claim      s             26
     D eight_long      s              8
     D six_long        s              6
     D two_long        s              2
     D output_lines    s              3s 0 inz(*zeros)
     D crchg           s              1    inz(*blanks)
     D crpend          s              1    inz(*blanks)
     D cramtf          s              1
     D primary         s              1
     D current         s              1
     D added           s              1
     D code2           s              3
     D code3           s              3
     D flag            s              1
     D*** BEGIN ADD *** 04/15/03 *******************************************
     D dx_ptr_out      s              1s 0
     D pm_usage_hit    s              1
     D*** END   ADD *** 04/15/03 *******************************************

     D bill_number     s                   like(eoeob#) dim(100) inz(*zeros)
     D recon           s                   like(eorcn ) dim(100) inz(*zeros)
     D sequence        s                   like(cadseq) dim(100) inz(*zeros)

     D dates           s                   like(cafdat) dim(100)
     D bill_numbers    s             39

     Dinterface_area   ds
     D edit_error_msg                30a   inz(*blanks)
     D program_stop                   1a   inz('N')
     D claim_stop                     1a   inz('N')
     D abend_pgm_id                   8a   inz(*blanks)
     D abend_para_id                  4a   inz(*blanks)
     D abend_file_id                  7a   inz(*blanks)
     D filler1                        9a   inz(*blanks)
     D abend_file_sts                 2a   inz('00')
     D filler2                        2a   inz(*blanks)
     D abend_file_fnc                12a   inz(*blanks)
     D abend_rec_key                 40a   inz(*blanks)
     D init_program                   8a
     D know_base_id                   2a
     D promatch_kb_id                 2a
     D num_codes_pssd                 3s 0
     D screen_mode                    7a   inz('ADD')
     D term_id                        4a   inz(*blanks)
     D trans_id                       4a   inz(*blanks)
     D user_info_area                50a   inz(*blanks)
     D cpt_line_limit                 5s 0 inz(100)
     D pgm_vers_epa                   3a   inz(*blanks)
     D pgm_vers_epc                   3a   inz(*blanks)
     D pgm_vers_epd                   3a   inz(*blanks)
     D pgm_vers_ex1                   3a   inz(*blanks)
     D pgm_vers_iia                   3a   inz(*blanks)
     D pgm_vers_uep                   3a   inz(*blanks)
     D pgm_vers_uop                   3a   inz(*blanks)
     D pgm_vers_oia                   3a   inz(*blanks)
     D pgm_vers_rsa                   3a   inz(*blanks)
     D pgm_vers_sea                   3a   inz(*blanks)
     D pgm_vers_sla                   3a   inz(*blanks)
     D date_sense                     1a   inz(*blanks)
     D num_of_units                   1a   inz(*blanks)
     D add_mod_51                     1a   inz(*blanks)
     D user_lin_lim                   3s 0 inz(*zeros)
     D write_log                      1a   inz(*blanks)
     D write_good                     1a   inz(*blanks)
     D dump_interface                 1a   inz('Y')
     D dump_int_type                  1a   inz('N')
     D write_unchg                    1a   inz('N')
     D write_chg                      1a   inz('N')
     D write_quest                    1a   inz('N')
     D filler3                        2a   inz(*blanks)
     D bilat_format                   1a   inz('N')
     D distinct_mod                   1a   inz(*blanks)
     D override_mod26                 1a   inz(*blanks)
     D batch_close                    1a   inz('N')
     D call_user_edit                 1a   inz('N')
     D call_user_outp                 1a   inz('N')
     D update_claim                   1a   inz('N')
     D update_ex1                     1a   inz('N')
     D update_xref                    1a   inz('N')
     D pend_flag                      1a   inz('N')
     D process_method                 1a
     D process_pmatch                 1a
     D claim_changed                  1a   inz('N')
     D claim_question                 1a   inz('N')
     D claim_supersed                 1a   inz('N')
     D hist_changed                   1a   inz('N')
     D hist_question                  1a   inz('N')
     D filler4                        1a   inz(*blanks)
     D pmatch_reduced                 1a   inz('N')
     D pmatch_failed                  1a   inz('N')
     D curr_aff_curr                  1a   inz('N')
     D hist_aff_curr                  1a   inz('N')
     D hist_aff_chg                   1a   inz('N')
     D hist_aff_quest                 1a   inz('N')
     D edit_err                       1a   inz('N')
     D cntl_001_err                   1a   inz('N')
     D limit_err                      1a   inz('N')
     D kb_id_err                      1a   inz('N')
     D units_serv_err                 1a   inz('N')
     D user_limit_err                 1a   inz('N')
     D method_err                     1a   inz('N')
     D pmatch_err                     1a   inz('N')
     D filler5                        7a   inz(*blanks)
     D claim_num_err                  1a   inz('N')
     D pat_id_err                     1a   inz('N')
     D pat_dob_err                    1a   inz('N')
     D pat_sex_err                    1a   inz('N')
     D pat_age_err                    1a   inz('N')
     D prov_id_err                    1a   inz('N')
     D dx_err                         1a   inz('N')
     D company_number                 6a
     D company_name                  30a
     D sub_number                     8a
     D sub_name                      30a
     D group_number                  10a
     D group_name                    30a
     D claim_id                      10a   inz(*blanks)
     D claim_number                  42a
     D seq_number                     2a   inz(*blanks)
     D policy_id                     20a   inz(*blanks)
     D subscriber_id                 12a   inz(*blanks)
     D patient_id                    20a
     D patient_dob                   10a
     D patient_sex                    1a
     D relationship                   2a   inz(*blanks)
     D patient_age                    3a
     D provider_id                   16a
     D prov_zip_code                 10a
     D claim_adjustor                20a   inz(*blanks)
     D upin                           6a   inz(*blanks)
     D filler6                        3a   inz(*blanks)
     D diagnosis_area                15a   dim(4)
     D filler7                        6a   inz(*blanks)
     D hist_bills                    42a   dim(25)
     D input_area                   131a   dim(100)
     D output_area                  110a   dim(100)
     D program_area    s             28a   dim(100)
     D*** BEGIN CHG *** 04/15/03 *******************************************
     D*lit_area        s            257a   dim(200)
     D lit_area        s            173a   dim(200)
     D*** END   CHG *** 04/15/03 *******************************************

     Ddx_area          ds
     D dx_code                        6a
     D dx_code_result                 3a   inz(*blanks)
     D dx_pattern_id                  5a   inz(*blanks)
     D dx_filler                      1a   inz(*blanks)

     Din_area          ds
     D line_data_in                   3a
     D code_stat_in                   1a   inz(*blanks)
     D disp_stat_in                   1a   inz(*blanks)
     D pm_preauth_in                  1a   inz('N')
     D code_error_in                  1a   inz('N')
     D mod1_error_in                  1a   inz('N')
     D mod2_error_in                  1a   inz('N')
     D frdt_error_in                  1a   inz('N')
     D todt_error_in                  1a   inz('N')
     D pos_error_in                   1a   inz('N')
     D chrg_error_in                  1a   inz('N')
     D allow_error_in                 1a   inz('N')
     D unit_error_in                  1a   inz('N')
     D serv_quest_in                  1a   inz('N')
     D over_error_in                  1a   inz('N')
     D clm_ptr_err_in                 1a   inz('N')
     D rvu_in                         6a   inz(*blanks)
     D serv_units_in                  3s 0 inz(001)
     D code_in                        5a
     D mod_1_in                       2a
     D mod_2_in                       2a
     D from_date_in                  10a   inz('00/00/0000')
     D to_date_in                    10a   inz('00/00/0000')
     D pos_in                         2a
     D charge_amt_in                  9a   inz('000000.00')
     D allowed_amt_in                 9a   inz('000000.00')
     D override_in                    1a   inz(*blanks)
     D rule_exec_in                   2a   inz(*blanks)
     D rule_type_in                   1a   inz(*blanks)
     D group_no_in                    3s 0 inz(*zeros)
     D output_ptr_in                  3s 0 inz(*zeros)
     D expand_ptr_in                  3s 0 inz(*zeros)
     D msg1_code_in                   5a   inz(*blanks)
     D msg1_mod_in                    2a   inz(*blanks)
     D msg1_key_in                    5a   inz(*blanks)
     D msg2_code_in                   5a   inz(*blanks)
     D msg2_mod_in                    2a   inz(*blanks)
     D msg2_key_in                    5a   inz(*blanks)
     D msg2_hist_ptr                  2s 0 inz(*zeros)
     D msg2_hist_dtl                  3a   inz(*blanks)
     D diag_ptr_in                    1a   dim(4) inz(*blanks)
     D hist_ptr_in                    2s 0 inz(*zeros)
     D hist_dtl_in                    3a
     D filler_in                      4a   inz(*blanks)

     Dout_area         ds
     D code_stat_out                  1a
     D pm_result_out                  3a
     D pm_rfm_out                     1a
     D filler_out_1                   3a
     D pm_preauth_out                 1a
     D pm_dx_ptr_out                  1a
     D rvu_out                        6a
     D serv_unit_out                  3s 0
     D code_out                       5a
     D pm_red_cod_out                 5a
     D mod_1_out                      2a
     D mod_2_out                      2a
     D from_date_out                 10a
     D to_date_out                   10a
     D pos_out                        2a
     D chrg_amt_out                   9a
     D payment_out                    9a
     D rule_type_out                  1a
     D pm_msg_cod_out                 5a
     D pm_msg_mod_out                 2a
     D pm_msg_key_out                 5a
     D diag_table_out                 4a
     D group_no_out                   3s 0
     D override_out                   1a
     D filler_out_2                   4a

     Ddx_table         ds
     D dx_ptr                         1s 0
     D dx_result_out                  3a

     Dprogram_table    ds
     D pgm_input_ptr                  3  0 inz(0)
     D pgm_bill_type                  1a
     D pgm_eob#                       9  0 inz(0)
     D pgm_rcn                        3  0 inz(0)
     D pgm_lin#                       3  0 inz(0)
     D pgm_rtyp                       1a
     D pgm_msg#                       7a   inz('CR     ')
     D pgm_mod_flag                   1a

     DLIT_table        ds
     D line_outp_to                   3  0
     D line_outp                      3  0
     D line_type                      1a
     D line_eob#                      9  0
     D line_rcn                       3  0
     D line_lin#                      9  0
     D line_code                      5a
     D line_mod1                      2a
     D line_mod2                      2a
     D line_charge                   11  2
     D line_change                    1a
     D line_action                   10a
     D line_pend                      1a
     D line_rcode                     5a
     D line_rmod1                     2a
     D line_rmod2                     2a
     D line_eob_code                  3a
     D line_eob_desc                 70a
     D line_msg#                      7a
     D line_err_code                  5a
     D line_status                    3a
     D*** BEGIN DEL *** 04/15/03 *******************************************
     D*line_pm_change                 1a
     D*** END   DEL *** 04/15/03 *******************************************
     D line_edit_type                10a
     D*** BEGIN DEL *** 04/15/03 *******************************************
     D*line_pm_msg#                   7a
     D*line_pm_status                 3a
     D*line_pm_eob                    3a
     D*line_pm_eobd                  70a
     D*** END   DEL *** 04/15/03 *******************************************
     D line_dseq                      5  0
     D line_prfl                      1a
    C*****************************************************************
    C*  Parameter Lists
    C*****************************************************************
     C     *entry        plist
     C                   parm                    @@rrn
     C                   parm                    @@crpe
     C                   parm                    @@pcod
     C                   parm                    @@chg
     C                   parm                    @@crkb
     C                   parm                    @@pmkb
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
     C     chky          klist
     C                   kfld                    k1co#
     C*** BEGIN ADD *** 04/15/03 *******************************************
    C*
     C     chky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkdseq
    C*
     C     chky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkdseq
     C                   kfld                    kkcrb#
     C*** END   ADD *** 04/15/03 *******************************************
    C*
     C     coky          klist
     C                   kfld                    kkco#
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
     C                   kfld                    cadseq
     C                   kfld                    kkseq#
    C*
     C     deky2         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C*** BEGIN ADD *** 11/26/02 *******************************************
    C*
     C     deky3         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C                   kfld                    kkeob
     C*** END   ADD *** 11/26/02 *******************************************
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
     C     diky          klist
     C                   kfld                    kkicd9
     C                   kfld                    kklost
    C*
     C     eaky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
     C*** BEGIN ADD *** 02/04/03 *******************************************
     C     eaky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    $$eob#
     C                   kfld                    $$rcn
     C*** END   ADD *** 02/04/03 *******************************************
    C*
     C     ebky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkcrst
     C                   kfld                    kkcret
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
     C     gxky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkcrvw
     C                   kfld                    kkefd1
    C*
     C     gxky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkcrvw
    C*
     C     fnky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
    C*
     C     mgky1         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C                   kfld                    kkseq#
    C*
     C     mgky2         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C*** BEGIN ADD *** 11/25/02 *******************************************
    C*
     C     mgky3         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C                   kfld                    kkmsg#
     C*** END   ADD *** 11/25/02 *******************************************
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
     C*** BEGIN ADD *** 04/15/03 *******************************************
    C*
     C     puky1         klist
     C                   kfld                    kkpatn
     C                   kfld                    kkpmst
     C                   kfld                    kkast
     C                   kfld                    kkfnd
    C*
     C     puky2         klist
     C                   kfld                    kkpatn
     C                   kfld                    kkpmst
     C                   kfld                    kkast
    C*
     C     puky3         klist
     C                   kfld                    kkpatn
     C                   kfld                    kkpmst
     C                   kfld                    kkfnd
    C*
     C     puky4         klist
     C                   kfld                    kkpatn
     C                   kfld                    kkpmst
    C*
     C     puky5         klist
     C                   kfld                    kkpatn
     C                   kfld                    kkast
     C                   kfld                    kkfnd
    C*
     C     puky6         klist
     C                   kfld                    kkpatn
     C                   kfld                    kkast
    C*
     C     puky7         klist
     C                   kfld                    kkpatn
     C                   kfld                    kkfnd
    C*
     C     puky8         klist
     C                   kfld                    kkpatn
     C*** END   ADD *** 04/15/03 *******************************************
    C*
     C     smky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
    C*
     C     sxky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkcrvw
     C                   kfld                    kkefd2
     C                   kfld                    kkst
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
     C*                         sent into the CodeReview process.
     C*
     C                   exsr      filtering
     C*
     C* Here we will only continue with the CodeReview process, if there
     C* is at least one current line item eligible for CodeReview
     C* processing,
     C*
     C                   if        *in90 = *off
     C*
     C* Section 2 - Formatting - Populating the CodeReview Copybook
     C*
     C                   exsr      formatting
     C*
     C* Section 3 - Calling CodeReview
    C*
     C                   setoff                                       91
     C*
     C                   dou       *in90 = *off
     C*
     C                   call      'HPRBSLA'
     C                   parm                    interface_area
     C*
     C* Section 4 - Process Errors - Checking for various CodeReview errors
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
     C* Let's assign a batch number
     C*
     C                   exsr      get_batch#
     C*
     C* Section 5 - Reviewing and Interpreting the Results
     C*
     C                   exsr      results
     C*
     C* Section 6 - Program end processing
     C*
     C                   exsr      program_end
     C*
     C                   endif
     C                   endif
     C*
     C                   eval      batch_close = 'Y'
     C                   call      'HPRBSLA'
     C                   parm                    interface_area
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
     C* Same day code
     C*
     C     cafdat        lookup    dates(1)                               99
     C                   if        *in99 = *off
     C                   seton                                        91
     C                   endif
     C*
     C                   endif
     C*
     C                   endsr
     C*** BEGIN ADD *** 04/15/03 ******************************************
     C**********************************************************************
     C* Deny_line - Deny line
     C**********************************************************************
     C     Deny_line     begsr
     C*
     C* Let's deny this line
     C*
     C     caky4         chain     pca03                              99
     C                   if        *in99 = *off
     C*
     C                   eval      caoth2 = capaid
     C*
     C                   if        caoth2 > 0
     C                   eval      caot2c = 'CRW'
     C                   endif
     C*
     C                   eval      capaid = 0
     C                   eval      caccod = pm_red_cod_out
     C*
     C                   update    pca03
     C                   endif
     C*
     C                   endsr
     C**********************************************************************
     C* Deny_replace - Deny/Replace Line
     C**********************************************************************
     C     Deny_replace  begsr
    C*
    C* Let's get the next available line number and detail sequence number
    C*
     C                   eval      kklin# = *hival
     C     caky4         setgt     pca03
     C     caky7         readpe(n) pca03                                  99
    C*
     C                   if        *in99 = *off
     C                   eval      hold_lin# = calin# + 1
     C                   else
     C                   eval      hold_lin# = 1
     C                   endif
    C*
     C                   eval      kkdseq = *hival
     C     caky5         setgt     pca04
     C     caky6         readpe    pca04                                  99
    C*
     C                   if        *in99 = *off
     C                   eval      hold_dseq = cadseq + 1
     C                   else
     C                   eval      hold_dseq = 1
     C                   endif
     C*
     C* Let's make a copy of the denied line
     C*
     C     caky4         chain(n)  pca03                              99
     C                   if        *in99 = *off
     C                   exsr      write_line
     C                   endif
     C*
     C* Let's deny the denied line
     C*
     C                   exsr      deny_line
     C*
     C                   endsr
     C*** END   ADD *** 04/15/03 ******************************************
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
     C     caky1         chain     pca01                              99
     C                   dow       *in99 = *off
     C*
     C* First lets see if the line can be passed into CodeReview.
     C*
     C                   eval      current = *on
     C                   exsr      qualify_line
     C                   if        *in91 = *off
     C*
     C* Setting off 90 to communicate that we will be calling CodeReview.
     C*
     C                   setoff                                       90
     C*
     C* Here we are keeping track of the number of lines, because we can't
     C* send more than 100 lines into CodeReview.
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
     C                   adddur    45:*d         date_test1
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
     C                   subdur    45:*d         date_test1
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
     C* Populate date array for same day codes
     C*
     C                   eval      dates(x) = cafdat
     C                   eval      x = x + 1
     C*
     C                   endsl
     C*
     C* Here we are adding the accepted line to the three arrays.
     C* These arrays will be used when loading the input_area and
     C* hist_bills (holds the historical bill numbers).
     C*
     C                   eval      bill_number(line) = caeob#
     C                   eval      recon(line) = carcn
     C                   eval      sequence(line) = cadseq
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
     C     caky1         reade     pca01                                  99
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
     C*
     C* First lets see if the line can be passed into CodeReview.
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
     C*
     C* Here we are keeping track of the number of bills, because we can't
     C* send more than 25 bills into CodeReview.
     C*
     C                   eval      hdeob# = caeob#
     C                   eval      hdrcn = carcn
     C                   eval      bills = bills + 1
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
     C*
     C* Stop process if we already have 25 bills
     C*
     C                   if        bills = 25
     C                   leave
     C                   endif
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
    C* Filtering - This subroutine selects line items to pass to CodeReview
    C**********************************************************************
     C     filtering     begsr
    C*
     C                   exsr      filter_current
     C*
     C* If *in90 is on then, there is no sense in loading history if there
     C* are no lines in the current bill that are going to go thru CodeRev.
     C*
     C* If *in92 is on then, there is no sense in loading history since the
     C* CodeReview line limit has been reached.
     C*
     C                   if        *in90=*off and *in92=*off
     C                   exsr      filter_history
     C                   endif
    C*
     C                   endsr
    C**********************************************************************
     C*                   Section   2
    C* Formatting - Populate CodeReview CopyBook and Calling CodeReview
    C**********************************************************************
     C     formatting    begsr
     C*
     C*---------------------------------------------------------------------
    C* Lets populate the copybook before calling CodeReview.
     C*---------------------------------------------------------------------
     C*
     C* Here we are populating the CodeReview area of the copybook.
     C*
     C                   movel(p)  q1pgm         init_program
     C*
     C* Here we are getting the CodeReview Knowledge base ID
     C*
     C                   eval      know_base_id = '99'
     C*
     C                   eval      kkcrvw = 'CR'
     C                   time                    dsdttm
     C                   move      dsdate        date_test1
     C     *cymd         move      date_test1    kkefd1
     C*
     C     gxky1         setgt     pgx01
     C     gxky2         readpe    pgx01                                  99
     C                   dow       *in99 = *off
     C                   if        gxstat <> 'I'
     C                   if        gxexdt = *zeros or kkefd1 <= gxexdt
     C                   eval      know_base_id = gxkbid
     C                   eval      kkefd2 = gxefdt
     C                   eval      kkst = eoast
     C     sxky          chain     psx01                              99
     C                   if        *in99 = *off
     C                   if        sxexdt = *zeros or  kkefd1 <= sxexdt
     C                   eval      know_base_id = sxkbid
     C                   endif
     C                   endif
     C                   leave
     C                   endif
     C                   endif
     C     gxky2         readpe    pgx01                                  99
     C                   enddo
     C*
     C* Here we are getting the Promatch Knowledge base ID
     C*
     C                   eval      promatch_kb_id = '99'
     C*
     C                   eval      kkcrvw = 'PM'
     C                   time                    dsdttm
     C                   move      dsdate        date_test1
     C     *cymd         move      date_test1    kkefd1
     C*
     C     gxky1         setgt     pgx01
     C     gxky2         readpe    pgx01                                  99
     C                   dow       *in99 = *off
     C                   if        gxstat <> 'I'
     C                   if        gxexdt = *zeros or kkefd1 <= gxexdt
     C                   eval      promatch_kb_id = gxkbid
     C                   eval      kkefd2 = gxefdt
     C                   eval      kkst = eoast
     C     sxky          chain     psx01                              99
     C                   if        *in99 = *off
     C                   if        sxexdt = *zeros or  kkefd1 <= sxexdt
     C                   eval      promatch_kb_id = sxkbid
     C                   endif
     C                   endif
     C                   leave
     C                   endif
     C                   endif
     C     gxky2         readpe    pgx01                                  99
     C                   enddo
     C*
     C                   z-add     line          num_codes_pssd
     C*
     C                   select
     C                   when      ##mcrv = 'Y' and ##crvw = 'Y'
     C                   eval      process_method = 'B'
     C                   when      ##mcrv = 'Y'
     C                   eval      process_method = 'M'
     C                   other
     C                   eval      process_method = 'C'
     C                   endsl
     C*
     C                   if        ##prom = 'Y'
     C                   eval      process_pmatch = 'Y'
     C                   else
     C                   eval      process_pmatch = 'N'
     C                   endif
     C*
     C* Here we are populating the header area of the copybook.
     C*
     C                   move (p)  eoco#         company_number
     C*
     C     coky          chain     pco01                              99
     C                   if        *in99 = *off
     C                   movel(p)  coname        company_name
     C                   else
     C                   movel     *blanks       company_name
     C                   endif
     C*
     C                   move (p)  eosub#        sub_number
     C*
     C     smky          chain     psm01                              99
     C                   if        *in99 = *off
     C                   movel(p)  smname        sub_name
     C                   else
     C                   movel     *blanks       sub_name
     C                   endif
     C*
     C                   move (p)  eofnd         group_number
     C*
     C     fnky          chain     pfn01                              99
     C                   if        *in99 = *off
     C                   movel(p)  fnname        group_name
     C                   else
     C                   movel     *blanks       group_name
     C                   endif
     C*
     C                   move      eoeob#        char_eob#
     C                   move      eorcn         char_rcn#
     C     char_eob#     cat(p)    '-':0         claim_number
     C                   cat       char_rcn#:0   claim_number
     C*
     C                   move      eofyr         char_fyr
     C                   move      eocase        char_case
     C     char_fyr      cat(p)    '-':0         patient_id
     C                   cat       char_case:0   patient_id
     C*
     C                   eval      kkss# = eoss#
     C     cdky          chain     pcd01                              99
     C                   if        *in99 = *off
     C*
     C                   if        cdsex = 'M' or cdsex = 'F'
     C                   eval      patient_sex = cdsex
     C                   else
     C                   eval      patient_sex = 'U'
     C                   endif
     C*
     C                   if        cddtob <> 0
     C     *cymd         move      cddtob        date_test1
     C                   move      date_test1    patient_dob
     C                   eval      patient_age = *blanks
     C                   else
     C                   eval      patient_dob = *blanks
     C                   eval      patient_age = '035'
     C                   endif
     C*
     C                   else
     C                   eval      patient_dob = *blanks
     C                   eval      patient_sex = 'U'
     C                   eval      patient_age = '035'
     C                   endif
     C*
     C                   move      eofein        char_fein
     C                   move      eomod         char_mod
     C     char_fein     cat(p)    '-':0         provider_id
     C                   cat       char_mod:0    provider_id
     C*
     C                   move      eozip         char_zip
     C     5             subst(p)  char_zip:1    char_zipa
     C     4             subst(p)  char_zip:6    char_zipb
     C     char_zipa     cat(p)    '-':0         prov_zip_code
     C                   cat       char_zipb:0   prov_zip_code
     C*
     C                   clear                   diagnosis_area
     C                   eval      kkeob#=eoeob#
     C*
     C                   if        ##prom = 'Y'
     C     ehky          chain     peh01                              99
     C                   if        *in99 = *off
     C     1             do        4             x
     C*
     C                   select
     C                   when      x = 1
     C                   eval      kkicd9 = ehdia1
     C                   when      x = 2
     C                   eval      kkicd9 = ehdia2
     C                   when      x = 3
     C                   eval      kkicd9 = ehdia3
     C                   when      x = 4
     C                   eval      kkicd9 = ehdia4
     C                   endsl
     C*
     C     diky          chain     pdi01                              99
     C                   if        *in99 = *off
     C                   movel     dids1         dx_code
     C                   movel     dx_code       flag
     C                   if        flag='E'
     C                   move      dx_code       code2
     C                   if        code2=*blanks
     C                   move      '. '          dx_code
     C                   endif
     C                   else
     C                   move      dx_code       code3
     C                   if        code3=*blanks
     C                   move      '.  '         dx_code
     C                   endif
     C                   endif
     C                   else
     C                   movel     *blanks       dx_code
     C                   endif
     C*
     C                   eval      diagnosis_area(x) = dx_area
     C                   enddo
     C                   endif
     C                   endif
     C*
     C* Here we are populating the hist_bills part of the copybook.
     C*
     C                   clear                   hist_bills
     C                   move      kkco#         char_co#
     C                   move      kksub#        char_sub#
     C                   move      kkfnd         char_fnd
     C                   move      kkfyr         char_fyr
     C                   move      kkcase        char_case
     C     char_co#      cat(p)    '-':0         char_claim
     C                   cat       char_sub#:0   char_claim
     C                   cat       '-':0         char_claim
     C                   cat       char_fnd:0    char_claim
     C                   cat       '-':0         char_claim
     C                   cat       char_fyr:0    char_claim
     C                   cat       '-':0         char_claim
     C                   cat       char_case:0   char_claim
     C                   cat       '-':0         char_claim
     C                   eval      hdeob# = 0
     C                   eval      hdrcn = 0
     C                   eval      y = 1
     C*
     C     1             do        100           x
     C*
     C* We are jumping out if an eob# is zero
     C*
     C                   if        bill_number(x) = 0
     C                   leave
     C                   endif
     C*
     C* We don't want to load the current bill.
     C*
     C                   if        bill_number(x) <> eoeob# or
     C                             recon(x) <> eorcn
     C*
     C* Here we are looking for the next eob
     C*
     C                   if        hdeob# <> bill_number(x) or
     C                             hdrcn <> recon(x)
     C*
     C                   eval      hdeob# = bill_number(x)
     C                   eval      hdrcn = recon(x)
     C                   move      bill_number(x)char_eob#
     C                   move      recon(x)      char_rcn#
     C     char_claim    cat(p)    char_eob#:0   hist_bills(y)
     C                   cat       '-':0         hist_bills(y)
     C                   cat       char_rcn#:0   hist_bills(y)
     C                   eval      y = y + 1
     C*
     C                   endif
     C                   endif
     C                   enddo
     C*
     C* Here we are populating the input_area part of the copybook.
     C*
     C                   clear                   input_area
     C*
     C* First we are going to load history
     C*
     C                   eval      y = 1
     C                   eval      z = 0
     C                   eval      hdeob# = 0
     C                   eval      hdrcn = 0
     C*
     C     1             do        100           x
     C*
     C                   reset                   in_area
     C*
     C* We are jumping out if an eob# is zero
     C*
     C                   if        bill_number(x) = 0
     C                   leave
     C                   endif
     C*
     C* We don't want to load the current bill yet.
     C*
     C                   if        bill_number(x) <> eoeob# or
     C                             recon(x) <> eorcn
     C*
     C* We need to keep track what the history pointer is.
     C*
     C                   if        hdeob# <> bill_number(x) or
     C                             hdrcn <> recon(x)
     C                   eval      hdeob# = bill_number(x)
     C                   eval      hdrcn  = recon(x)
     C                   eval      z = z + 1
     C                   endif
     C*
     C                   eval      kkeob# = bill_number(x)
     C                   eval      kkrcn = recon(x)
     C                   eval      kkdseq = sequence(x)
     C*
     C     caky3         chain     pca01                              99
     C                   if        *in99 = *off
     C                   move      calin#        line_data_in
     C*
     C                   if        cafdat <> 0
     C     *cymd         move      cafdat        date_test1
     C                   move      date_test1    from_date_in
     C                   else
     C                   move      '00/00/0000'  from_date_in
     C                   endif
     C*
     C                   if        catdat <> 0
     C     *cymd         move      catdat        date_test1
     C                   move      date_test1    to_date_in
     C                   else
     C                   move      '00/00/0000'  to_date_in
     C                   endif
     C*
     C                   move      cadseq        hist_dtl_in
     C                   move      cachrg        eight_long
     C                   movel     eight_long    six_long
     C                   move      eight_long    two_long
     C                   movel     six_long      charge_amt_in
     C                   move      two_long      charge_amt_in
     C*
     C                   move      capaid        eight_long
     C                   movel     eight_long    six_long
     C                   move      eight_long    two_long
     C                   movel     six_long      allowed_amt_in
     C                   move      two_long      allowed_amt_in
     C*
     C                   endif
     C*
     C     dhky          chain     pdh01                              99
     C                   if        *in99 = *off
     C                   eval      code_in = dhprcd
     C                   eval      mod_1_in = dhmod1
     C                   eval      mod_2_in = dhmod2
     C                   move      dhpos         pos_in
     C                   endif
     C*
     C                   z-add     z             hist_ptr_in
     C*
     C                   move      in_area       input_area(y)
     C                   eval      y = y + 1
     C*
     C                   endif
     C*
     C                   enddo
     C*
     C* Now we are going to load current bill
     C*
     C     1             do        100           x
     C*
     C                   reset                   in_area
     C*
     C* We are jumping out if an eob# is zero
     C*
     C                   if        bill_number(x) <> 0
     C*
     C* We only want to load the current bill now.
     C*
     C                   if        bill_number(x) = eoeob# and
     C                             recon(x) = eorcn
     C*
     C                   eval      kkeob# = bill_number(x)
     C                   eval      kkrcn = recon(x)
     C                   eval      kkdseq = sequence(x)
     C*
     C     caky3         chain     pca01                              99
     C                   if        *in99 = *off
     C                   move      calin#        line_data_in
     C*
     C                   if        cafdat <> 0
     C     *cymd         move      cafdat        date_test1
     C                   move      date_test1    from_date_in
     C                   else
     C                   move      '00/00/0000'  from_date_in
     C                   endif
     C*
     C                   if        catdat <> 0
     C     *cymd         move      catdat        date_test1
     C                   move      date_test1    to_date_in
     C                   else
     C                   move      '00/00/0000'  to_date_in
     C                   endif
     C*
     C                   move      *blanks       hist_dtl_in
     C                   move      cachrg        eight_long
     C                   movel     eight_long    six_long
     C                   move      eight_long    two_long
     C                   movel     six_long      charge_amt_in
     C                   move      two_long      charge_amt_in
     C*
     C                   move      capaid        eight_long
     C                   movel     eight_long    six_long
     C                   move      eight_long    two_long
     C                   movel     six_long      allowed_amt_in
     C                   move      two_long      allowed_amt_in
     C*
     C                   endif
     C*
     C     dhky          chain     pdh01                              99
     C                   if        *in99 = *off
     C                   eval      code_in = dhprcd
     C                   eval      mod_1_in = dhmod1
     C                   eval      mod_2_in = dhmod2
     C                   move      dhpos         pos_in
     C                   endif
     C*
     C                   z-add     *zeros        hist_ptr_in
     C*
     C                   move      in_area       input_area(y)
     C                   eval      y = y + 1
     C*
     C                   endif
     C                   else
     C                   clear                   in_area
     C                   move      in_area       input_area(y)
     C                   eval      y = y + 1
     C                   endif
     C*
     C                   enddo
     C*
     C* Here we are initializing output_area part of the copybook.
     C*
     C                   clear                   output_area
    C*
     C                   endsr
     C**********************************************************************
     C* Get_Batch# - Get the batch number.
     C**********************************************************************
     C     Get_batch#    begsr
     C*
     C     chky          chain     pch02                              99
     C                   if        *in99 = *off
    C*
     C                   eval      c2crb# = c2crb# + 1
    C*
     C                   eval      c2user = q1user
     C                   eval      c2pgm = q1pgm
     C                   time                    dsdttm
     C                   eval      c2time = dstime
     C                   move      dsdate        date_test1
     C     *cymd         move      date_test1    c2dt
     C*
     C                   update    pch02
     C                   else
     C                   clear                   pch02
    C*
     C                   eval      c2crb# = 1
    C*
     C                   eval      c2stat = 'A'
     C                   eval      c2user = q1user
     C                   eval      c2pgm = q1pgm
     C                   eval      c2iusr = q1user
     C                   eval      c2ipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      c2time = dstime
     C                   eval      c2itim = dstime
     C                   move      dsdate        date_test1
     C     *cymd         move      date_test1    c2dt
     C     *cymd         move      date_test1    c2idt
     C*
     C                   write     pch02
     C*
     C                   endif
     C*
     C                   endsr
    C**********************************************************************
     C*                   P A S S   2
    C* Populate_LIT_E - Populate Line Item Table (LI) for existing lines
    C**********************************************************************
     C     populate_LIT_Ebegsr
    C*
     C                   clear                   LIT_area
    C*
     C     1             do        line          x
     C*
     C                   clear                   LIT_table
     C                   eval      in_area = input_area(x)
     C*
     C                   eval      line_outp_to = output_ptr_in
     C                   eval      line_outp = *zeros
     C*
     C                   if        hist_ptr_in = 0
     C                   eval      line_type = 'C'
     C                   movel(p)  claim_number  eob_recon1
     C                   eval      line_eob# = eob#1
     C                   eval      line_rcn = recon1
     C                   else
     C                   eval      line_type = 'H'
     C                   eval      bill_numbers = hist_bills(hist_ptr_in)
     C                   movel(p)  bill_numbers  eob_recon
     C                   eval      line_eob# = eob#
     C                   eval      line_rcn = recons
     C                   endif
     C*
     C                   move      line_data_in  line_lin#
     C                   eval      line_code = code_in
     C                   eval      line_mod1 = mod_1_in
     C                   eval      line_mod2 = mod_2_in
     C                   eval      line_status = code_stat_in
     C*** BEGIN DEL *** 04/15/03 *******************************************
     C**********         eval      line_pm_status = *blanks
     C**********         eval      line_pm_msg#   = *blanks
     C*** END   DEL *** 04/15/03 *******************************************
     C*
     C                   movel     charge_amt_in six_long
     C                   move      charge_amt_in two_long
     C                   movel     six_long      eight_long
     C                   move      two_long      two_long
     C                   move      eight_long    line_charge
     C*
     C                   if        output_ptr_in <> 0
     C                   eval      out_area = output_area(output_ptr_in)
     C                   eval      line_rcode = code_out
     C                   eval      line_rmod1 = mod_1_out
     C                   eval      line_rmod2 = mod_2_out
     C                   else
     C                   eval      line_rcode = *blanks
     C                   eval      line_rmod1 = *blanks
     C                   eval      line_rmod2 = *blanks
     C                   endif
     C*
     C                   eval      line_err_code = msg1_code_in
     C                   eval      line_msg# = 'CR' + msg1_key_in
     C                   eval      kkmsg# = line_msg#
     C*
     C     miky          chain     pmi01                              99
     C                   if        *in99 = *off
     C                   eval      line_edit_type = micret
     C                   eval      kkcrst = micrst
     C                   eval      kkcret = micret
     C     ebky          chain     peb02                              99
     C                   if        *in99 = *off
     C                   movel(p)  ebkey         line_eob_code
     C                   eval      line_eob_desc = ebdesc
     C                   if        line_type = 'H'
     C                   eval      line_change = 'N'
     C                   eval      line_action = ebcrha
     C                   eval      line_pend = *blanks
     C                   else
     C                   if        output_ptr_in <> 0
     C                   eval      program_table = program_area(output_ptr_in)
     C                   if        kkcret = 'FRAGMENT' and pgm_rtyp = 'B'
     C                   eval      line_change = 'N'
     C                   eval      line_action = ebcrha
     C                   eval      line_pend = *blanks
     C                   else
     C                   eval      line_change = 'Y'
     C                   eval      line_action = ebcrca
     C                   eval      line_pend = ebcrpf
     C                   endif
     C                   else
     C                   eval      line_change = 'Y'
     C                   eval      line_action = ebcrca
     C                   eval      line_pend = ebcrpf
     C                   endif
     C                   endif
     C                   endif
     C                   endif
     C*
     C                   eval      LIT_area(x) = LIT_table
     C*
     C                   enddo
    C*
     C                   endsr
    C**********************************************************************
     C*                   P A S S   3
    C* Populate_LIT_N - Populate Line Item Table (LI) for Added lines
    C**********************************************************************
     C     populate_LIT_Nbegsr
    C*
     C     1             do        output_lines  x
     C*
     C                   eval      program_table = program_area(x)
     C                   eval      out_area = output_area(x)
     C*
     C* Is this a line that needs to be added
     C* When pgm_bill_type = *blanks, this indicates a replacement
     C* When pgm_mod_flag = 'X', this indicates a modifier change
     C*
     C                   if        pgm_bill_type = *blanks or
     C                             pgm_mod_flag = 'X'
     C*
     C                   eval      line = line + 1
     C*
     C                   clear                   LIT_table
     C*
     C                   eval      line_outp = x
     C*
     C                   eval      line_type = pgm_rtyp
     C                   eval      line_lin# = 0
     C*
     C                   if        pgm_rtyp = 'C'
     C                   movel(p)  claim_number  eob_recon1
     C                   eval      line_eob# = eob#1
     C                   eval      line_rcn = recon1
     C                   else
     C                   eval      line_eob# = 0
     C                   eval      line_rcn = 0
     C                   endif
     C*
     C                   eval      line_code = code_out
     C                   eval      line_mod1 = mod_1_out
     C                   eval      line_mod2 = mod_2_out
     C                   eval      line_status = code_stat_out
     C*** BEGIN DEL *** 04/15/03 *******************************************
     C**********         eval      line_pm_status = pm_result_out
     C**********         eval      line_pm_msg#   = 'CR' + pm_msg_key_out
     C*** END   DEL *** 04/15/03 *******************************************
     C*
     C                   movel     chrg_amt_out  six_long
     C                   move      chrg_amt_out  two_long
     C                   movel     six_long      eight_long
     C                   move      two_long      two_long
     C                   move      eight_long    line_charge
     C*
     C                   eval      line_rcode = *blanks
     C                   eval      line_rmod1 = *blanks
     C                   eval      line_rmod2 = *blanks
     C*
     C                   eval      line_msg# = *blanks
     C                   eval      kkmsg# = pgm_msg#
     C*
     C     miky          chain     pmi01                              99
     C                   if        *in99 = *off
     C                   eval      kkcrst = 'A'
     C                   eval      line_edit_type = micret
     C                   eval      kkcret = micret
     C     ebky          chain     peb02                              99
     C                   if        *in99 = *off
     C                   movel(p)  ebkey         line_eob_code
     C                   eval      line_eob_desc = ebdesc
     C                   if        line_type = 'C'
     C                   eval      line_change = 'Y'
     C                   eval      line_action = ebcrca
     C                   eval      line_pend = ebcrpf
     C                   else
     C                   eval      line_change = 'N'
     C                   eval      line_action = ebcrha
     C                   eval      line_pend = *blanks
     C                   endif
     C                   endif
     C                   endif
     C*
     C                   eval      LIT_area(line) = LIT_table
     C*
     C                   else
     C*
     C                   eval      LIT_table = LIT_area(pgm_input_ptr)
     C                   eval      line_outp = x
     C*
     C*** BEGIN DEL *** 04/15/03 *******************************************
     C**********         if        ##prom = 'Y'
     C**********         eval      line_pm_status = pm_result_out
     C**********         eval      line_pm_msg#   = 'CR' + pm_msg_key_out
     C**********         eval      line_pm_eob    = *blanks
     C**********         eval      line_pm_eobd   = *blanks
     C**********         else
     C**********         eval      line_pm_status = *blanks
     C**********         eval      line_pm_msg#   = *blanks
     C**********         eval      line_pm_eob    = *blanks
     C**********         eval      line_pm_eobd   = *blanks
     C**********         endif
     C*** END   DEL *** 04/15/03 *******************************************
     C*
     C                   if        pgm_mod_flag = 'P' and
     C                             line_type = 'C'
     C                   eval      line_change = 'N'
     C                   eval      line_action = 'NONE'
     C                   eval      line_pend = 'Y'
     C                   endif
     C*
     C                   eval      LIT_area(pgm_input_ptr) = LIT_table
     C*
     C                   endif
     C*
     C                   enddo
     C*** BEGIN DEL *** 04/15/03 *******************************************
     C*
     C* Pro-match Processing
     C*
     C**********         if        ##prom = 'Y'
     C*****1****         do        line          x
     C*
     C**********         eval      lit_table = lit_area(x)
     C*
     C**********         if        line_outp <> 0
     C**********         eval      line_pm_change = 'N'
     C*
     C*  We need to find out what actions Promatch suggests
     C*
     C**********         eval      kkcrst = line_pm_status
     C**********         eval      kkcret = 'PROMATCH'
     C*****ebky*         chain     peb02                              99
     C*
     C**********         if        *in99 = *off
     C*
     C**********         if        ebcrpf = 'Y' and
     C**********                   line_type = 'C'
     C**********         movel(p)  ebkey         line_pm_eob
     C**********         eval      line_pm_eobd = ebdesc
     C**********         eval      line_pm_change = 'Y'
     C**********         endif
     C*
     C**********         if        ebcrca <> 'NONE'
     C*
     C**********         movel(p)  ebkey         line_pm_eob
     C**********         eval      line_pm_eobd = ebdesc
     C*
     C**********         if        line_type = 'C'
     C**********         eval      line_change = 'Y'
     C**********         eval      line_action = ebcrca
     C**********         eval      line_pend = ebcrpf
     C**********         eval      line_pm_change = 'Y'
     C**********         endif
     C**********         endif
     C**********         endif
     C**********         eval      lit_area(x) = lit_table
     C**********         endif
     C**********         enddo
     C**********         endif
     C*** END   DEL *** 04/15/03 *******************************************
    C*
     C                   endsr
    C**********************************************************************
     C*                   P A S S   1
    C* Populate_PT - Populate Program Table (PT)
    C**********************************************************************
     C     populate_PT   begsr
    C*
     C                   clear                   program_area
     C                   reset                   program_table
     C*
     C     1             do        100           x
     C                   eval      program_area(x) = program_table
     C                   enddo
    C*
     C     1             do        line          x
     C*
     C                   eval      in_area = input_area(x)
     C*
     C* Does this line point to something in the output area, if equal
     C* to zero then this line has been denied
     C*
b01  C                   if        output_ptr_in <> 0
     C                   eval      program_table = program_area(output_ptr_in)
     C                   eval      out_area = output_area(output_ptr_in)
     C*
     C* I want to know the number of lines are in the output area.
     C*
b02  C                   if        output_ptr_in > output_lines
     C                   eval      output_lines = output_ptr_in
e02  C                   endif
     C*
     C* Has the code been accepted or is there a modifier change.
     C*
b03  C                   if        code_in = code_out
     C*
     C                   if        code_stat_in = 'A' or code_stat_in = 'Q'
     C                             or code_stat_in = 'K'
     C                             or pgm_input_ptr = 0
     C                   eval      pgm_input_ptr = x
     C*
     C* I want to know if I am dealing with a current or history line.
     C* I need to know this, so I know how to populate the eob#/recon.
     C*
b04  C                   if        hist_ptr_in = 0
     C                   eval      pgm_bill_type = 'C'
     C                   movel(p)  claim_number  eob_recon1
     C                   eval      pgm_eob# = eob#1
     C                   eval      pgm_rcn = recon1
s04  C                   else
     C                   eval      pgm_bill_type = 'H'
     C                   eval      bill_numbers = hist_bills(hist_ptr_in)
     C                   movel(p)  bill_numbers  eob_recon
     C                   eval      pgm_eob# = eob#
     C                   eval      pgm_rcn = recons
e04  C                   endif
     C*
     C                   move      line_data_in  pgm_lin#
     C                   eval      pgm_rtyp = *blanks
     C*
     C                   movel(p)  'CR'          pgm_msg#
     C                   move      msg1_key_in   pgm_msg#
     C*
     C* Need to know what kind of modifier change if any was made.
     C* If CodeReview returns a '99' we don't know what the change
     C* is, so we want to communicate that the bill is to be pended.
     C* We are also checking for any modifier change and tagging these
     C* so we can add/deny later.
     C*
b05  C                   if        mod_2_out = '99'
     C                   eval      pgm_mod_flag = 'P'
s05  C                   else
     C                   if        mod_1_in <> mod_2_in
     C                   if        (mod_1_in <> mod_1_out and
     C                             mod_1_in <> mod_2_out) or
     C                             (mod_2_in <> mod_1_out and
     C                             mod_2_in <> mod_2_out)
     C                   eval      pgm_mod_flag = 'X'
     C                   if        hist_ptr_in = 0
     C                   eval      pgm_rtyp = 'C'
     C                   else
     C                   eval      pgm_rtyp = 'H'
     C                   endif
     C                   else
     C                   eval      pgm_mod_flag = *blanks
     C                   endif
     c                   else
     C                   if        (mod_1_out <> mod_1_in and
     C                             mod_1_out <> mod_2_in) or
     C                             (mod_2_out <> mod_1_in and
     C                             mod_2_out <> mod_2_in)
     C                   eval      pgm_mod_flag = 'X'
     C                   if        hist_ptr_in = 0
     C                   eval      pgm_rtyp = 'C'
     C                   else
     C                   eval      pgm_rtyp = 'H'
     C                   endif
     C                   else
     C                   eval      pgm_mod_flag = *blanks
     C                   endif
     C                   endif
e05  C                   endif
     C*
     C                   endif
     C*
s03  C                   else
     C*
     C* Here we are dealing with replacements
     C*
     C                   if        pgm_input_ptr = 0
     C                   eval      pgm_input_ptr = x
     C                   endif
     C*
     C                   if        pgm_eob# = *zeros
     C                   eval      pgm_eob# = *zeros
     C                   eval      pgm_rcn = *zeros
     C                   eval      pgm_lin# = *zeros
     C                   endif
     C*
     C* We want to know how this line relates to the other lines it
     C* replaces.  Is it pure current, is it pure history or is
     C* there a mix of current and history.
     C*
     C                   if        pgm_rtyp = *blanks
     C                   if        hist_ptr_in = 0
     C                   eval      pgm_rtyp = 'C'
     C                   else
     C                   eval      pgm_rtyp = 'H'
     C                   endif
     C                   else
     C                   if        pgm_rtyp = 'C'
     C                   if        hist_ptr_in <> 0
     C                   eval      pgm_rtyp = 'B'
     C                   endif
     C                   else
     C                   if        hist_ptr_in = 0
     C                   eval      pgm_rtyp = 'B'
     C                   endif
     C                   endif
     C                   endif
     C*
     C                   if        pgm_msg# = 'CR     '
     C                   movel     'CR'          pgm_msg#
     C                   move      msg1_key_in   pgm_msg#
     C                   endif
     C*
     C                   eval      pgm_mod_flag = *blanks
     C*
     C                   endif
     C*
     C* Load program table array
     C*
     C                   eval      program_area(output_ptr_in)= program_table
     C*
     C                   endif
     C*
     C                   enddo
    C*
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
     C*
     C* Check to see if Program_stop error is 'Y'
     C*
     C                   if        *in91 = *off and program_stop = 'Y'
     C                   eval      @@crpe = 'E'
     C                   eval      @@pcod = 'CCE'
     C                   exsr      write_faeep
     C                   seton                                        91
     C                   leave
     C                   endif
     C*
     C* Check to see if Control record error is 'Y'
     C*
     C                   if        *in91 = *off and cntl_001_err = 'Y'
     C                   eval      @@crpe = 'E'
     C                   eval      @@pcod = 'CHE'
     C                   exsr      write_faeep
     C                   seton                                        91
     C                   leave
     C                   endif
     C*
     C* Check to see if Bill error is 'Y'
     C*
     C                   if        *in91 = *off and claim_stop = 'Y'
     C                   eval      @@crpe = 'E'
     C                   eval      @@pcod = 'CBE'
     C                   exsr      write_faeep
     C                   seton                                        91
     C                   leave
     C                   endif
     C*
     C* Check to see if any additional edit errors.
     C*
     C                   if        *in91 = *off and edit_err = 'Y'
     C                   if        limit_err = 'Y' or
     C                             kb_id_err = 'Y' or
     C                             units_serv_err = 'Y' or
     C                             user_limit_err = 'Y' or
     C                             method_err = 'Y' or
     C                             pmatch_err = 'Y' or
     C                             claim_num_err = 'Y' or
     C                             pat_id_err = 'Y' or
     C                             pat_dob_err = 'Y' or
     C                             pat_sex_err = 'Y' or
     C                             pat_age_err = 'Y' or
     C                             prov_id_err = 'Y' or
     C                             dx_err = 'Y'
     C                   eval      @@crpe = 'E'
     C                   eval      @@pcod = 'CEE'
     C                   exsr      write_faeep
     C                   seton                                        91
     C                   leave
     C                   endif
     C*
     C* Check to see if any line item errors.
     C*
     C                   if        *in91 = *off
     C     1             do        100           x
     C                   eval      in_area = input_area(x)
     C                   if        code_stat_in = 'E'
     C                   eval      override_in = 'K'
     C                   eval      input_area(x) = in_area
     C                   seton                                        90
     C                   endif
     C                   enddo
     C                   endif
     C                   endif
     C*
     C                   enddo
    C*
     C                   endsr
     C**********************************************************************
     C* Program_end - Program end processing.
     C**********************************************************************
     C     Program_end   begsr
    C*
     C                   eval      @@chg  = crchg
     C*
     C                   if        crpend = 'Y'
     C                   eval      @@crpe = 'Y'
     C                   eval      @@pcod = 'CRP'
     C                   endif
     C*
     C                   eval      @@crkb = know_base_id
     C                   eval      @@pmkb = promatch_kb_id
    C*
     C                   endsr
     C*** BEGIN ADD *** 04/15/03 *******************************************
    C**********************************************************************
     C*                   P A S S   5
    C* Promatch_proc - Promatch Processing
    C**********************************************************************
     C     Promatch_proc begsr
     C*
     C                   eval      xx = 2
     C*
     C* Promatch only processes lines that were accepted by CodeReview
     C*
     C     1             do        output_lines  x
     C*
     C                   eval      program_table = program_area(x)
     C                   eval      out_area = output_area(x)
     C*
     C* Get the Line info
     C*
     C     1             do        line          y
     C                   eval      lit_table = lit_area(y)
     C                   if        line_outp = x
     C                   eval      kkeob# = line_eob#
     C                   eval      kklin# = line_lin#
     C                   eval      kkdseq = line_dseq
     C                   leave
     C                   endif
     C                   if        y = line
     C                   clear                   lit_table
     C                   endif
     C                   enddo
     C*
     C* We only want to process current lines
     C*
     C                   if        pgm_bill_type = 'C' or
     C                             pgm_bill_type = ' ' and
     C                             pgm_rtyp = 'C'
     C*
     C* Only process if line has gone thru Promatch and did not pass.
     C*
     C                   if        pm_result_out <> *blanks and
     C                             pm_result_out <> 'PAS'
     C*
     C* There has to be a dx pointer
     C*
     C                   if        pm_dx_ptr_out <> *blanks and
     C                             pm_dx_ptr_out <> '0'
     C*
     C* Let's get the pattern
     C*
     C                   move      pm_dx_ptr_out dx_ptr_out
     C                   eval      dx_area = diagnosis_area(dx_ptr_out)
     C                   eval      kkpmst = pm_result_out
     C                   eval      kkast  = eoast
     C                   eval      pm_usage_hit = 'N'
     C*
     C* Let's check out the Promatch Usage Table
     C*
     C                   do
     C*
     C* Let's start with the sub-pattern
     C*
     C                   eval      kkpatn = dx_pattern_id
     C*
     C* Checking PM Status/Adj State/Group
     C*
     C     puky1         chain     ppu01                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking PM Status/Adj State
     C*
     C     puky2         chain     ppu01                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking PM Status/Group
     C*
     C     puky3         chain     ppu02                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking PM Status
     C*
     C     puky4         chain     ppu01                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking Adj State/Group
     C*
     C     puky5         chain     ppu03                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking Adj State
     C*
     C     puky6         chain     ppu03                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking Group
     C*
     C     puky7         chain     ppu04                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking just the pattern
     C*
     C     puky8         chain     ppu03                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C*
     C* Let's look at the major-pattern
     C*
     C                   eval      kkpatn = %subst(dx_pattern_id:1:3)
     C*
     C* Checking PM Status/Adj State/Group
     C*
     C     puky1         chain     ppu01                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking PM Status/Adj State
     C*
     C     puky2         chain     ppu01                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking PM Status/Group
     C*
     C     puky3         chain     ppu02                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking PM Status
     C*
     C     puky4         chain     ppu01                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking Adj State/Group
     C*
     C     puky5         chain     ppu03                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking Adj State
     C*
     C     puky6         chain     ppu03                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking Group
     C*
     C     puky7         chain     ppu04                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking just the pattern
     C*
     C     puky8         chain     ppu03                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Let's look at the master-pattern
     C*
     C                   eval      kkpatn = %subst(dx_pattern_id:1:2)
     C*
     C* Checking PM Status/Adj State/Group
     C*
     C     puky1         chain     ppu01                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking PM Status/Adj State
     C*
     C     puky2         chain     ppu01                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking PM Status/Group
     C*
     C     puky3         chain     ppu02                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking PM Status
     C*
     C     puky4         chain     ppu01                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking Adj State/Group
     C*
     C     puky5         chain     ppu03                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking Adj State
     C*
     C     puky6         chain     ppu03                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking Group
     C*
     C     puky7         chain     ppu04                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C*
     C* Checking just the pattern
     C*
     C     puky8         chain     ppu03                              99
     C                   if        *in99 = *off
     C                   eval      pm_usage_hit = 'Y'
     C                   leave
     C                   endif
     C                   enddo
     C*
     C* If no hit in the ProMatch Usage file then skip line
     C*
     C                   if        pm_usage_hit = 'Y'
     C*
     C* Must be an include record
     C*
     C                   if        puie = 'I'
     C*
     C* Now we are set to do some stuff
     C*
     C                   select
     C                   when      puactn = 'DNY'
     C                   exsr      deny_line
     C                   eval      crchg = 'Y'
     C                   when      puactn = 'DRP'
     C                   exsr      deny_replace
     C                   eval      crchg = 'Y'
     C                   when      puactn = 'RPT'
     C                   eval      crpend = 'Y'
     C                   endsl
     C*
     C* Let's create the eob and message
     C*
     C                   if        puactn = 'DNY' or
     C                             puactn = 'DRP' or
     C                             puactn = 'RPT'
     C                   exsr      write_fddep
     C                   exsr      write_fdmgp
     C                   endif
     C*
     C* Let's update history
     C*
     C                   exsr      update_fdchp
     C*
     C                   endif
     C                   endif
     C                   endif
     C                   endif
     C                   endif
     C                   enddo
     C*
     C                   endsr
     C*** END   ADD *** 04/15/03 *******************************************
    C**********************************************************************
    C* Qualify_line - Sees if line should be passed into CodeReview
    C**********************************************************************
     C     Qualify_line  begsr
     C*
     C                   setoff                                       91
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
     C* We only want Physician line items
     C*
     C*** BEGIN ADD *** 02/04/03 *******************************************
     C                   eval      $$eob# = caeob#
     C                   eval      $$rcn  = carcn
     C     eaky2         chain(n)  pea01                              99
     C                   if        *in99 = *off
     C                   if        eaepfl = 'D' or eaepfl = 'X'
     C                   seton                                        91
     C                   leave
     C                   endif
     C                   endif
     C*** END   ADD *** 02/04/03 *******************************************
     C*
     C* We want to omit deleted bills and cancelled bills
     C*
     C                   eval      kkeob# = caeob#
     C                   eval      kkdseq = cadseq
     C     dhky          chain     pdh01                              99
     C                   if        *in99 = *on
     C                   seton                                        91
     C                   leave
     C                   endif
     C*
     C* Service date must be greater than the current date - 3yrs.
     C*
     C                   time                    dsdttm
     C                   move      dsdate        date_test1
     C                   subdur    3:*y          date_test1
     C     *cymd         move      date_test1    cut_off_date
     C*
     C                   if        cafdat < cut_off_date
     C                   seton                                        91
     C                   leave
     C                   endif
     C*
     C* Skip if user defined code or flag is set to skip CodeReview
     C*
     C                   movel(p)  cacode        kkcpt4
     C                   eval      kkefdt = cafdat
     C     prky1         setgt     ppr01
     C     prky2         readpe    ppr01                                  99
     C                   if        *in99 = *off and pr1ag = 'Y'
     C*** BEG ADD *** 11/02/03 *********************************************
     C                             or *in99 = *off and pr1ah = 'R'
     C                             or *in99 = *off and pr1ah = 'B'
     C*** END ADD *** 11/02/03 *********************************************
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
     C* Exclude routine vision codes.
     C*
     C                   movel     cacode        hold_code
     C                   if        hold_code >= 'V2020' and
     C                             hold_code <= 'V2999'
     C                   seton                                        91
     C                   leave
     C                   endif
     C*
     C                   enddo
    C*
     C                   endsr
    C**********************************************************************
     C*                   P A S S   4
    C* Reading_LIT - Reading Line Item Table
    C**********************************************************************
     C     reading_LIT   begsr
     C*
     C                   eval      crchg = 'N'
     C                   eval      crpend = 'N'
     C*** BEGIN ADD *** 04/15/03 *******************************************
     C                   eval      xx = 1
     C*** END   ADD *** 04/15/03 *******************************************
     C*=====================================================================
     C* Let's work with the denied lines first
     C*=====================================================================
     C     1             do        line          x
     C*
     C                   eval      LIT_table = LIT_area(x)
     C*
     C                   if        line_action = 'DENY' and
     C                             line_prfl = *blanks
     C*
     C                   eval      crchg = 'Y'
     C                   eval      cramtf = 'Y'
     C                   eval      chg_chrg = 0
     C                   eval      chg_mra = 0
     C                   eval      chg_paid = 0
     C                   eval      chg_oth2 = 0
     C                   eval      hold_charge = 0
     C                   eval      hold_mra = 0
     C                   eval      hold_paid = 0
     C*ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
     C* This line was denied due to replacement, defragmentation or
     C* bundling.
     C*ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
     C                   if        line_code <> line_rcode and
     C                             line_rcode <> *blanks
     C*
     C                   eval      kkeob# = line_eob#
     C                   eval      kklin# = line_lin#
     C*
     C                   eval      hold_outp = line_outp_to
     C*
     C     caky4         chain     pca03                              99
     C                   if        *in99 = *off
     C*
     C                   eval      kkdseq = cadseq
     C     dhky          chain     pdh01                              99
     C                   if        *in99 = *off
     C                   eval      hold_pos = dhpos
     C*** BEGIN ADD *** 04/20/09 *******************************************
     C                   eval      hold_units = dhunts
     C*** END   ADD *** 04/20/09 *******************************************
     C                   else
     C                   eval      hold_units = 1
     C*** BEGIN ADD *** 04/20/09 *******************************************
     C*** END   ADD *** 04/20/09 *******************************************
     C                   endif
     C*
     C                   eval      hold_charge = cachrg
     C                   eval      hold_mra = camra
     C                   eval      hold_paid = capaid
     C                   eval      hold_code = line_rcode
     C*
     C* Updating the line item table with '1' so we can update this record
     C* with information about the replacement line.
     C*
     C                   eval      line_dseq = cadseq
     C                   eval      line_prfl = '1'
     C                   eval      LIT_area(x) = LIT_table
     C*
     C* Write History, EOB code and Error Message
     C*
     C                   exsr      write_fdchp
     C                   exsr      write_fddep
     C                   exsr      write_fdmgp
     C*
     C* Updating Detail record
     C*
     C                   eval      caoth2 = capaid
     C                   eval      chg_oth2 = chg_oth2 + caoth2
     C                   if        caoth2 > 0
     C                   eval      caot2c = 'CRW'
     C                   endif
     C                   eval      chg_paid = chg_paid - capaid
     C                   eval      capaid = 0
     C                   eval      hold_fdat = cafdat
     C                   eval      hold_tdat = catdat
     C                   eval      caccod = line_err_code
     C*
     C                   update    pca03
     C                   endif
     C*---------------------------------------------------------------------
     C* Let's look for other lines that have been replaced by this code
     C*---------------------------------------------------------------------
     C     1             do        line          y
     C*
     C                   eval      LIT_table = LIT_area(y)
     C*
     C                   if        line_outp_to = hold_outp and
     C                             line_outp <> hold_outp and
     C                             line_prfl = *blanks
     C*
     C                   if        line_change = 'Y'
     C*
     C                   eval      kkeob# = line_eob#
     C                   eval      kklin# = line_lin#
     C*
     C     caky4         chain     pca03                              99
     C                   if        *in99 = *off
     C*
     C                   eval      hold_charge = hold_charge + cachrg
     C                   eval      hold_mra = hold_mra + camra
     C                   eval      hold_paid = hold_paid + capaid
     C*
     C* Updating the line item table
     C*
     C                   eval      line_dseq = cadseq
     C                   eval      line_prfl = '1'
     C                   eval      LIT_area(y) = LIT_table
     C*
     C* Write History, EOB code and Error Message
     C*
     C                   exsr      write_fdchp
     C                   exsr      write_fddep
     C                   exsr      write_fdmgp
     C*
     C* Updating Detail record
     C*
     C                   eval      caoth2 = capaid
     C                   eval      chg_oth2 = chg_oth2 + caoth2
     C                   if        caoth2 > 0
     C                   eval      caot2c = 'CRW'
     C                   endif
     C                   eval      chg_paid = chg_paid - capaid
     C                   eval      capaid = 0
     C                   eval      caccod = line_err_code
     C*
     C                   if        cafdat < hold_fdat
     C                   eval      hold_fdat = cafdat
     C                   endif
     C*
     C                   if        catdat > hold_tdat
     C                   eval      hold_tdat = catdat
     C                   endif
     C*
     C                   update    pca03
     C                   endif
     C                   else
     C*
     C* Updating the line item table
     C*
     C                   eval      kkeob# = line_eob#
     C                   eval      kklin# = line_lin#
     C*
     C     caky4         chain(n)  pca03                              99
     C                   if        *in99 = *off
     C                   eval      line_dseq = cadseq
     C                   else
     C                   eval      line_dseq = *zeros
     C                   endif
     C                   eval      line_prfl = 'P'
     C                   eval      LIT_area(y) = LIT_table
     C*
     C* Write History record
     C*
     C                   exsr      write_fdchp
     C*
     C* Here we are communicating that we don't want to apply any
     C* amount changes to the replacment code.
     C*
     C                   eval      cramtf = 'N'
     C                   endif
     C                   endif
     C*
     C                   enddo
     C*---------------------------------------------------------------------
     C* Let's look for the replacement code
     C*---------------------------------------------------------------------
     C     1             do        line          y
     C*
     C                   eval      LIT_table = LIT_area(y)
     C*
     C                   if        hold_outp = line_outp
     C*ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
     C* Unbundling - Major code reported along with minor codes
     C*ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
     C                   if        line_outp_to = line_outp
     C                   if        line_change = 'Y'
     C*
     C                   eval      kkeob# = line_eob#
     C                   eval      kklin# = line_lin#
     C*
     C     caky4         chain(n)  pca03                              99
     C                   if        *in99 = *off
     C*
     C* Updating the line item table
     C*
     C                   eval      line_dseq = cadseq
     C                   eval      line_prfl = 'P'
     C                   eval      LIT_area(y) = LIT_table
     C                   eval      cramtf = 'N'
     C*
     C* Write History, EOB code and Error Message
     C*
     C                   exsr      write_fdchp
     C                   exsr      write_fddep
     C                   exsr      write_fdmgp
     C*
     C                   endif
     C*
     C                   else
     C*
     C* Updating the line item table
     C*
     C                   eval      kkeob# = line_eob#
     C                   eval      kklin# = line_lin#
     C*
     C     caky4         chain(n)  pca03                              99
     C                   if        *in99 = *off
     C                   eval      line_dseq = cadseq
     C                   else
     C                   eval      line_dseq = *zeros
     C                   endif
     C                   eval      line_prfl = 'P'
     C                   eval      LIT_area(y) = LIT_table
     C                   eval      cramtf = 'N'
     C*
     C* Write History record
     C*
     C                   exsr      write_fdchp
     C*
     C                   endif
     C*ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
     C* Fragmentation/Replacement - This will require a line to be added
     C*ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
     C                   else
     C*
     C                   if        line_change = 'Y'
     C*
     C* Write Detail Lines
     C*
     C                   exsr      write_line
     C*
     C* Updating the line item table
     C*
     C                   eval      line_dseq = cadseq
     C*** BEGIN ADD *** 04/15/03 *******************************************
     C                   eval      line_lin# = calin#
     C*** END   ADD *** 04/15/03 *******************************************
     C                   eval      line_prfl = 'P'
     C                   eval      LIT_area(y) = LIT_table
     C*
     C* Write History, EOB code and Error Message
     C*
     C                   exsr      write_fdchp
     C                   exsr      write_fddep
     C*
     C                   else
     C*
     C* Updating the line item table
     C*
     C                   eval      kkeob# = line_eob#
     C                   eval      kklin# = line_lin#
     C*
     C     caky4         chain(n)  pca03                              99
     C                   if        *in99 = *off
     C                   eval      line_dseq = cadseq
     C                   else
     C                   eval      line_dseq = *zeros
     C                   endif
     C                   eval      line_prfl = 'P'
     C                   eval      LIT_area(y) = LIT_table
     C                   eval      cramtf = 'N'
     C*
     C* Write History record
     C*
     C                   exsr      write_fdchp
     C*
     C                   endif
     C                   endif
     C                   endif
     C*
     C                   enddo
     C*---------------------------------------------------------------------
     C* Now we need to go back to the denied lines and update them with
     C* CodeReview status and The replacement lines line number.
     C*---------------------------------------------------------------------
     C     1             do        line          y
     C*
     C                   eval      LIT_table = LIT_area(y)
     C*
     C                   if        line_prfl = '1'
     C*
     C                   eval      kkeob# = line_eob#
     C                   eval      kkrcn  = line_rcn
     C                   eval      kkdseq = line_dseq
     C                   eval      kklin# = line_lin#
     C*
     C* Let's zero out the charged amount of the denied lines.
     C     caky4         chain     pca03                              99
     C                   if        *in99 = *off
     C                   if        cramtf = 'Y'
     C*
     C                   eval      chg_mra = chg_mra - camra
     C                   eval      chg_chrg = chg_chrg - cachrg
     C                   eval      chg_oth2 = chg_oth2 - caoth2
     C                   eval      cachrg = 0
     C                   eval      camra  = 0
     C                   eval      caoth2 = 0
     C                   eval      caot2c = *blanks
     C                   else
     C                   eval      cacscd = '2'
     C                   endif
     C*
     C                   update    pca03
     C*
     C                   eval      primary = *on
     C                   exsr      update_fddap
     C*
     C                   endif
     C*
     C* Updating the line item table
     C*
     C                   eval      line_prfl = 'P'
     C                   eval      LIT_area(y) = LIT_table
     C*
     C                   endif
     C*
     C                   enddo
     C*ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
     C* line_code = line_rcode or line_rcode = *blanks
     C* These were denied due to a modifier change or a flat denial
     C* or Replacements by the same code.
     C*ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
     C                   else
     C*ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
     C* Let's deal with the modifier change or Denials
     C*ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
     C                   if        line_code = line_rcode
     C*
     C                   eval      kkeob# = line_eob#
     C                   eval      kklin# = line_lin#
     C                   eval      hold_outp = line_outp_to
     C*
     C     caky4         chain     pca03                              99
     C                   if        *in99 = *off
     C*
     C                   eval      kkdseq = cadseq
     C     dhky          chain     pdh01                              99
     C                   if        *in99 = *off
     C                   eval      hold_pos = dhpos
     C                   else
     C                   eval      hold_pos = ' 11'
     C                   endif
     C*
     C                   eval      hold_charge = cachrg
     C                   eval      hold_mra = camra
     C                   eval      hold_paid = capaid
     C                   eval      hold_code = line_rcode
     C*
     C* Updating the line item table with '1' so we can update this record
     C* with information about the replacement line.
     C*
     C                   eval      line_dseq = cadseq
     C                   eval      line_prfl = '1'
     C                   eval      LIT_area(x) = LIT_table
     C*
     C* Write History, EOB code and Error Message
     C*
     C                   exsr      write_fdchp
     C                   exsr      write_fddep
     C                   exsr      write_fdmgp
     C*
     C* Updating Detail record
     C*
     C                   eval      caoth2 = capaid
     C                   eval      chg_oth2 = chg_oth2 + caoth2
     C                   if        caoth2 > 0
     C                   eval      caot2c = 'CRW'
     C                   endif
     C                   eval      chg_paid = chg_paid - capaid
     C                   eval      capaid = 0
     C                   eval      hold_fdat = cafdat
     C                   eval      hold_tdat = catdat
     C*
     C* Update the Bill Detail Transaction Amount File
     C*
     C                   eval      primary = *off
     C                   exsr      update_fddap
     C*
     C                   update    pca03
     C                   endif
     C*---------------------------------------------------------------------
     C* Now we need to add the replacement
     C*---------------------------------------------------------------------
     C     1             do        line          y
     C*
     C                   eval      LIT_table = LIT_area(y)
     C*
     C                   if        hold_outp = line_outp and line_prfl <> 'P'
     C                   if        line_change = 'Y'
     C                   if        line_action = 'ADD       '
     C*
     C* Write Detail Lines
     C*
     C                   exsr      write_line
     C*
     C* Updating the line item table
     C*
     C                   eval      line_dseq = cadseq
     C*** BEGIN ADD *** 04/15/03 *******************************************
     C                   eval      line_lin# = calin#
     C*** END   ADD *** 04/15/03 *******************************************
     C                   eval      line_prfl = 'P'
     C                   eval      LIT_area(y) = LIT_table
     C*
     C* Write History, EOB code and Error Message
     C*
     C                   exsr      write_fdchp
     C                   exsr      write_fddep
     C*
     C                   eval      added = *on
     C                   else
     C                   eval      added = *off
     C                   endif
     C*
     C                   else
     C*
     C* Updating the line item table
     C*
     C                   eval      kkeob# = line_eob#
     C                   eval      kklin# = line_lin#
     C*
     C     caky4         chain(n)  pca03                              99
     C                   if        *in99 = *off
     C                   eval      line_dseq = cadseq
     C                   else
     C                   eval      line_dseq = *zeros
     C                   endif
     C                   eval      line_prfl = 'P'
     C                   eval      LIT_area(y) = LIT_table
     C*
     C* Write History record
     C*
     C                   exsr      write_fdchp
     C*
     C                   endif
     C                   endif
     C*
     C                   enddo
     C*---------------------------------------------------------------------
     C* Now we need to go back to the denied lines and update them with
     C* CodeReview status and The replacement lines line number.
     C*---------------------------------------------------------------------
     C     1             do        line          y
     C*
     C                   eval      LIT_table = LIT_area(y)
     C*
     C                   if        line_prfl = '1'
     C*
     C                   eval      kkeob# = line_eob#
     C                   eval      kkrcn  = line_rcn
     C                   eval      kkdseq = line_dseq
     C                   eval      kklin# = line_lin#
     C*
     C     caky4         chain     pca03                              99
     C                   if        *in99 = *off
     C                   if        added = *on
     C                   eval      chg_chrg = chg_chrg - cachrg
     C                   eval      chg_mra = chg_mra - camra
     C                   eval      chg_oth2 = chg_oth2 - caoth2
     C                   eval      cachrg = 0
     C                   eval      camra = 0
     C                   eval      caoth2 = 0
     C                   eval      caot2c = *blanks
     C                   else
     C                   eval      cacscd = '2'
     C                   endif
     C                   eval      primary = *off
     C                   exsr      update_fddap
     C                   update    pca03
     C                   endif
     C*
     C* Updating the line item table
     C*
     C                   eval      line_prfl = 'P'
     C                   eval      LIT_area(y) = LIT_table
     C*
     C                   endif
     C                   enddo
     C                   endif
     C*ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
     C* Here we are dealing with a line that was flat denied,
     C*ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
     C                   if        line_rcode = *blanks
     C*
     C                   eval      kkeob# = line_eob#
     C                   eval      kklin# = line_lin#
     C*
     C     caky4         chain     pca03                              99
     C                   if        *in99 = *off
     C*
     C* Updating the line item table with 'P'
     C*
     C                   eval      line_dseq = cadseq
     C                   eval      line_prfl = 'P'
     C                   eval      LIT_area(x) = LIT_table
     C*
     C* Write History, EOB code and Error Message
     C*
     C                   exsr      write_fdchp
     C                   exsr      write_fddep
     C                   exsr      write_fdmgp
     C*
     C* Updating Detail record
     C*
     C                   eval      cacscd = '2'
     C                   eval      chg_oth2 = chg_oth2 + capaid
     C                   eval      caoth2 = capaid
     C                   if        caoth2 > 0
     C                   eval      caot2c = 'CRW'
     C                   endif
     C                   eval      chg_paid = chg_paid - capaid
     C                   eval      capaid = 0
     C*
     C* Update the Bill Detail Transaction Amount File
     C*
     C                   eval      primary = *off
     C                   exsr      update_fddap
     C*
     C                   update    pca03
     C                   endif
     C                   endif
     C                   endif
     C                   endif
     C*
     C                   enddo
     C*
     C* We must update other amount files like FAEOP and FAEAP.
     C*
     C                   exsr      update_header
     C*
     C*=====================================================================
     C* Now let's work with the lines with no change
     C*=====================================================================
     C     1             do        line          x
     C*
     C                   eval      LIT_table = LIT_area(x)
     C*
     C                   if        line_action = 'NONE' and
     C                             line_prfl = *blanks
     C*
     C                   if        line_pend = 'Y'
     C*
     C                   eval      kkeob# = line_eob#
     C                   eval      kklin# = line_lin#
     C*
     C     caky4         chain     pca03                              99
     C                   if        *in99 = *off
     C*
     C* Write History, EOB code and Error Message
     C*
     C                   eval      line_dseq = cadseq
     C                   exsr      write_fdchp
     C                   exsr      write_fddep
     C                   exsr      write_fdmgp
     C*
     C                   eval      crpend = 'Y'
     C*
     C                   update    pca03
     C                   endif
     C                   endif
     C*
     C                   eval      line_prfl = 'P'
     C                   eval      LIT_area(x) = LIT_table
     C*
     C                   endif
     C*
     C                   enddo
     C*=====================================================================
     C* Now let's work with the lines that are going to report.
     C*=====================================================================
     C     1             do        line          x
     C*
     C                   eval      LIT_table = LIT_area(x)
     C*
     C                   if        line_action = 'REPORT' and
     C                             line_prfl = *blanks
     C*
     C                   eval      kkeob# = line_eob#
     C                   eval      kklin# = line_lin#
     C*
     C     caky4         chain     pca03                              99
     C                   if        *in99 = *off
     C*
     C* Write History
     C*
     C                   eval      line_dseq = cadseq
     C                   exsr      write_fdchp
     C*
     C                   update    pca03
     C                   endif
     C*
     C                   eval      line_prfl = 'P'
     C                   eval      LIT_area(x) = LIT_table
     C*
     C                   endif
     C*
     C                   enddo
     C*=====================================================================
     C* Now let's look for pended lines.
     C*=====================================================================
     C     1             do        line          x
     C*
     C                   eval      LIT_table = LIT_area(x)
     C*
     C                   if        line_pend = 'Y'
     C                   eval      crpend = 'Y'
     C                   leave
     C                   endif
     C*
     C                   enddo
     C*
     C                   endsr
    C**********************************************************************
     C*                   Section   5
    C* Results - Recieving and interpeting CodeReview Results
    C**********************************************************************
     C     Results       begsr
     C*
     C* Pass 1 - Populate Program Table
    C*
     C                   exsr      populate_PT
     C*
     C* Pass 2 - Populate Line Table for Existing Lines
    C*
     C                   exsr      populate_LIT_E
     C*
     C* Pass 3 - Populate Line Table for New Lines
    C*
     C                   exsr      populate_LIT_N
     C*
     C* Pass 4 - Reading Line Item Table
    C*
     C                   exsr      reading_LIT
     C*** BEGIN ADD *** 04/15/03 *******************************************
     C*
     C* Pass 5 - ProMatch Processing
    C*
     C                   if        ##prom = 'Y'
    C*
     C                   exsr      Promatch_proc
    C*
     C                   endif
     C*** END   ADD *** 04/15/03 *******************************************
    C*
     C                   endsr
     C*** BEGIN ADD *** 04/15/03 *******************************************
     C**********************************************************************
     C* Update_FDCHP - Update History File.
     C**********************************************************************
     C     Update_FDCHP  begsr
     C*
     C     chky2         setgt     pch03
     C     chky1         readpe    pch03                                  99
     C                   if        *in99 = *off
     C*
     C                   move      pm_msg_key_outchpmm#
     C                   eval      chpmst = pm_result_out
     C                   eval      chpmet = 'PROMATCH'
     C*
    C********* MAINTENANCE FIELDS ************************************
    C*
     C                   eval      chuser = q1user
     C                   eval      chpgm = q1pgm
     C                   time                    dsdttm
     C                   eval      chtime = dstime
     C                   move      dsdate        date_test1
     C     *cymd         move      date_test1    chdt
     C*
     C                   update    pch03
     C                   endif
     C*
     C                   endsr
     C*** END   ADD *** 04/15/03 *******************************************
     C**********************************************************************
     C* Update_header  - Updates amount files like FAEOP and FAEAP
     C**********************************************************************
     C     Update_header begsr
     C*
     C* Update the Bill Header Transaction Amount File
     C*
     C     eaky          chain     pea01                              99
     C                   if        *in99 = *off
     C                   eval      eachrg = eachrg + chg_chrg
     C                   eval      eamra  = eamra  + chg_mra
     C                   eval      eapaid = eapaid + chg_paid
     C                   eval      eaoth2 = eaoth2 + chg_oth2
     C                   update    pea01
     C                   endif
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
     C     caky4         setgt     pca03
     C     caky7         readpe(n) pca03                                  99
    C*
     C                   if        *in99 = *off
     C                   eval      hold_lin# = calin# + 1
     C                   else
     C                   eval      hold_lin# = 1
     C                   endif
    C*
     C                   eval      kkdseq = *hival
     C     caky5         setgt     pca04
     C     caky6         readpe    pca04                                  99
    C*
     C                   if        *in99 = *off
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
     C                   eval      cafdat = hold_fdat
     C                   eval      catdat = hold_tdat
     C                   eval      cardat = hold_fdat
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
     C                   eval      caeob# = kkeob#
     C                   eval      cass# = eoss#
     C                   eval      caacdt = eoacdt
     C                   move      hold_mra      caltr#
     C*
     C*** BEGIN ADD *** 02/04/03 *******************************************
     C**********         eval      carcfl = 'C'
     C                   eval      carcfl = 'Y'
     C                   movel(p)  line_code     kkcpt4
     C                   eval      kkefdt = cafdat
     C     prky1         setgt     ppr01
     C     prky2         readpe    ppr01                                  99
     C                   if        *in99 = *off
     C                   eval      carcfl = 'C'
     C                   endif
     C*** END   ADD *** 02/04/03 *******************************************
     C                   eval      casub# = kksub#
     C                   eval      cazipa = eozipa
     C                   eval      caast  = eoast
     C                   eval      cachrg = hold_charge
     C                   eval      camra = hold_mra
     C                   eval      capaid = hold_paid
     C                   eval      chg_mra = chg_mra + camra
     C                   eval      chg_paid = chg_paid + capaid
     C                   eval      chg_chrg = chg_chrg + cachrg
     C                   eval      carcn = eorcn
     C                   eval      calin# = hold_lin#
     C                   eval      cacpt4 = line_code
     C                   eval      cabprc = line_code
     C                   eval      cacode = line_code
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
     C*** BEGIN ADD *** 04/15/03 ******************************************
     C**********************************************************************
     C* Write_FDCAPPM - Write FDCAP record for ProMatch
     C**********************************************************************
     C     Write_FDCAPPM begsr
     C*
     C                   eval      cadseq = hold_dseq
     C                   move      camra         caltr#
     C*
     C                   eval      carcfl = 'Y'
     C                   movel(p)  pm_red_cod_outkkcpt4
     C                   eval      kkefdt = cafdat
     C     prky1         setgt     ppr01
     C     prky2         readpe    ppr01                                  99
     C                   if        *in99 = *off
     C                   eval      carcfl = 'C'
     C                   endif
     C*                                                                    *
     C                   eval      calin# = hold_lin#
     C                   eval      cacpt4 = kkcpt4
     C                   eval      cabprc = kkcpt4
     C                   eval      cacode = kkcpt4
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
     C*** END   ADD *** 04/15/03 ******************************************
     C**********************************************************************
     C* Update_FDDAP - Update FDDAP record.
     C**********************************************************************
     C     Update_FDDAP  begsr
     C*
     C* Update the Bill Detail Transaction Amount File
     C*
     C     daky          chain     pda02                              99
     C                   if        *in99 = *off
     C                   eval      dachrg = cachrg
     C                   eval      damra  = camra
     C                   eval      daothr = caothr
     C                   eval      daoth2 = caoth2
     C                   eval      dandis = candis
     C                   eval      dapaid = capaid
     C*
     C                   if        primary = *on
     C                   eval      dacrst = line_status
     C                   eval      dacrl# = line_dseq
     C                   endif
     C*
     C                   update    pda02
     C*
     C                   else
     C*
     C                   exsr      write_fddap
     C*
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
     C                   select
     C                   when      @@PCOD = 'CCE'
     C                   if        edit_error_msg = *blanks
     C                   eval      eenot1 = cce_error
     C                   else
     C                   eval      eenot1 = edit_error_msg
     C                   endif
     C                   when      @@PCOD = 'CHE'
     C                   if        edit_error_msg = *blanks
     C                   eval      eenot1 = che_error
     C                   else
     C                   eval      eenot1 = edit_error_msg
     C                   endif
     C                   when      @@PCOD = 'CBE'
     C                   if        edit_error_msg = *blanks
     C                   eval      eenot1 = cbe_error
     C                   else
     C                   eval      eenot1 = edit_error_msg
     C                   endif
     C                   when      @@PCOD = 'CEE'
     C                   if        edit_error_msg = *blanks
     C                   eval      eenot1 = cee_error
     C                   else
     C                   eval      eenot1 = edit_error_msg
     C                   endif
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
     C* Write_FDCHP - Write to History File.
     C**********************************************************************
     C     Write_FDCHP   begsr
     C*
     C                   clear                   pch01
     C*
     C                   eval      chco# = kkco#
     C                   eval      chsub# = kksub#
     C                   eval      chfnd = kkfnd
     C                   eval      chfyr = kkfyr
     C                   eval      chcase = kkcase
     C                   eval      cheob# = line_eob#
     C                   eval      chrcn = line_rcn
     C                   eval      chdseq = line_dseq
     C*
     C                   if        line_type = 'C'
     C                   eval      chtype = 'C'
     C                   else
     C                   eval      chtype = 'H'
     C                   endif
     C*
     C                   eval      chcrb# = c2crb#
     C                   eval      chcrl# = line_lin#
     C                   move      line_msg#     chcrm#
     C                   eval      chcrst = line_status
     C                   eval      chcret = line_edit_type
     C                   eval      chcrpc = line_rcode
     C                   eval      chcrm1 = line_rmod1
     C                   eval      chcrm2 = line_rmod2
     C*** BEGIN DEL *** 04/15/03 *******************************************
     C**********         eval      chpmst = line_pm_status
     C*
     C**********         if        chpmst <> *blanks
     C**********         move      line_pm_msg#  chpmm#
     C**********         eval      chpmet = 'PROMATCH'
     C**********         endif
     C*** END   DEL *** 04/15/03 *******************************************
     C*
    C********* MAINTENANCE FIELDS ************************************
    C*
     C                   eval      chstat = 'A'
     C                   eval      chuser = q1user
     C                   eval      chpgm = q1pgm
     C                   eval      chiusr = q1user
     C                   eval      chipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      chtime = dstime
     C                   eval      chitim = dstime
     C                   move      dsdate        date_test1
     C     *cymd         move      date_test1    chdt
     C     *cymd         move      date_test1    chidt
     C*
     C                   write     pch01
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
     C*** BEGIN DEL *** 04/15/03 *******************************************
     C*****1****         do        2             xx
     C*** END   DEL *** 04/15/03 *******************************************
     C*
     C*** BEGIN CHG *** 04/15/03 *******************************************
     C**********         if        (xx = 1 and line_eob_code <> *blanks and
     C**********                   line_action <> 'NONE') or
     C**********                   (xx = 1 and line_eob_code <> *blanks and
     C**********                   line_status = 'Q') or
     C**********                   (xx = 2 and line_pm_change = 'Y')
     C                   if        (xx = 1 and line_eob_code <> *blanks and
     C                             line_action <> 'NONE') or
     C                             (xx = 1 and line_eob_code <> *blanks and
     C                             line_status = 'Q') or
     C                             (xx = 2)
     C*** END   CHG *** 04/15/03 *******************************************
     C*** BEGIN ADD *** 11/26/02 *******************************************
     C*
     C                   select
     C                   when      xx = 1
     C                   eval      kkeob = line_eob_code
     C                   when      xx = 2
     C*** BEGIN CHG *** 04/15/03 *******************************************
     C**********         eval      kkeob = line_pm_eob
     C                   eval      kkcrst = pm_result_out
     C                   eval      kkcret = 'PROMATCH'
     C     ebky          chain     peb02                              99
     C                   if        *in99 = *off
     C                   movel(p)  ebkey         kkeob
     C                   movel(p)  ebdesc        kknot1
     C                   else
     C                   eval      kkeob = *blanks
     C                   eval      kknot1 = *blanks
     C                   endif
     C*** END   CHG *** 04/15/03 *******************************************
     C                   endsl
     C*
     C     deky3         chain     pde03                              99
     C                   if        *in99 = *on
     C*** END   ADD *** 11/26/02 *******************************************
     C*
     C     deky1         setgt     pde02
     C     deky2         readpe    pde02                                  99
     C                   if        *in99 = *off
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
     C                   select
     C                   when      xx = 1
     C                   eval      deeob = line_eob_code
     C                   eval      denot1 = line_eob_desc
     C                   when      xx = 2
     C*** BEGIN CHG *** 04/15/03 *******************************************
     C**********         eval      deeob = line_pm_eob
     C**********         eval      denot1 = line_pm_eobd
     C                   eval      deeob = kkeob
     C                   eval      denot1 = kknot1
     C*** END   CHG *** 04/15/03 *******************************************
     C                   endsl
     C*
     C                   eval      deesrc = 'R'
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
     C*** BEGIN ADD *** 11/26/02 *******************************************
     C                   endif
     C*** END   ADD *** 11/26/02 *******************************************
     C*
     C                   endif
     C*** BEGIN DEL *** 04/15/03 *******************************************
     C**********         enddo
     C*** END   DEL *** 04/15/03 *******************************************
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
     C                   eval      dhpos  = hold_pos
     C                   eval      dhprcd = line_code
     C                   eval      dhmod1 = line_mod1
     C                   eval      dhmod2 = line_mod2
     C*** BEGIN CHG *** 04/20/09 *******************************************
     C**********         eval      dhunts = 1
     C                   eval      dhunts = hold_units
     C*** END   CHG *** 04/20/09 *******************************************
     C                   eval      dhrcfl = 'C'
     C                   eval      dhbprc = line_code
     C                   eval      dhdxr1 = 'X'
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
     C*** BEGIN ADD *** 04/15/03 ******************************************
     C**********************************************************************
     C* Write_FDDHPPM - Write FDDHP record for ProMatch
     C**********************************************************************
     C     Write_FDDHPPM begsr
     C*
     C     dhky          chain     pdh01                              99
     C                   if        *in99 = *off
    C*
     C                   eval      dhdseq = cadseq
     C                   eval      dhprcd = pm_red_cod_out
     C                   eval      dhbprc = pm_red_cod_out
     C*
     C                   movel(p)  dhprcd        kkcpt4
     C                   eval      dhrcfl = 'C'
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
     C                   endif
     C*
     C                   endsr
     C*** END   ADD *** 04/15/03 ******************************************
     C**********************************************************************
     C* Write_FDMGP - Write to Message File.
     C**********************************************************************
     C     Write_FDMGP   begsr
     C*
     C*** BEGIN DEL *** 04/15/03 *******************************************
     C*****1****         do        2             xx
     C*** END   DEL *** 04/15/03 *******************************************
     C*
     C*** BEGIN CHG *** 04/15/03 *******************************************
     C**********         if        (xx = 1 and line_msg# <> 'CR     ' and
     C**********                   line_status <> 'A') or
     C**********                   (xx = 2 and line_pm_change = 'Y')
     C                   if        (xx = 1 and line_msg# <> 'CR     ' and
     C                             line_status <> 'A') or
     C                             (xx = 2)
     C*** END   CHG *** 04/15/03 *******************************************
     C*** BEGIN ADD *** 11/25/02 *******************************************
     C*
     C                   select
     C                   when      xx = 1
     C                   eval      kkmsg# = line_msg#
     C                   when      xx = 2
     C*** BEGIN CHG *** 04/15/03 *******************************************
     C**********         eval      kkmsg# = line_pm_msg#
     C                   eval      kkmsg# = 'CR' + pm_msg_key_out
     C*** END   CHG *** 04/15/03 *******************************************
     C                   endsl
     C*
     C     mgky3         chain     pmg03                              99
     C                   if        *in99 = *on
     C*** END   ADD *** 11/25/02 *******************************************
     C*
     C     mgky1         setgt     pmg02
     C     mgky2         readpe    pmg02                                  99
     C                   if        *in99 = *off
     C                   eval      hdmseq = mgmseq + 1
     C                   else
     C                   eval      hdmseq = 1
     C                   endif
     C*
     C                   clear                   pmg01
     C*
     C                   eval      mgco# = caco#
     C                   eval      mgsub# = casub#
     C                   eval      mgfnd = cafnd
     C                   eval      mgfyr = cafyr
     C                   eval      mgcase = cacase
     C                   eval      mgdcs# = *zeros
     C                   eval      mgdseq = cadseq
     C                   eval      mgmseq = hdmseq
     C                   eval      mgeob# = caeob#
     C*
     C                   select
     C                   when      xx = 1
     C                   eval      mgmsg# = line_msg#
     C                   when      xx = 2
     C*** BEGIN CHG *** 04/15/03 *******************************************
     C**********         eval      mgmsg# = line_pm_msg#
     C                   eval      mgmsg# = 'CR' + pm_msg_key_out
     C*** END   CHG *** 04/15/03 *******************************************
     C                   endsl
     C*
     C                   eval      mgrcn = carcn
     C                   eval      mgsrc = 'R'
    C*
    C********* MAINTENANCE FIELDS ************************************
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
     C*** BEGIN ADD *** 11/25/02 *******************************************
     C                   ENDIF
     C*** END   ADD *** 11/25/02 *******************************************
     C*
     C                   endif
     C*** BEGIN DEL *** 04/15/03 *******************************************
     C**********         enddo
     C*** END   DEL *** 04/15/03 *******************************************
     C*
     C                   endsr
     C**********************************************************************
     C* Write_Line - Write Detail Line.
     C**********************************************************************
     C     Write_line    begsr
     C*
     C*** BEGIN ADD *** 04/15/03 *******************************************
     C                   select
     C                   when      xx = 1
     C*** END   ADD *** 04/15/03 *******************************************
     C                   exsr      write_fdcap
     C                   exsr      write_fddap
     C                   exsr      write_fddhp
     C*** BEGIN ADD *** 04/15/03 *******************************************
     C                   when      xx = 2
     C                   exsr      write_fdcappm
     C                   exsr      write_fddap
     C                   exsr      write_fddhppm
     C                   endsl
     C*** END   ADD *** 04/15/03 *******************************************
     C                   endsr
     C*********************************************************************
