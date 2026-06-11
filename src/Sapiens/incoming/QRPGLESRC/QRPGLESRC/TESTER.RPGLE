    H* (C) Copyright Progressive Data Solutions, Inc. 2002
    H*
    H/TITLE F1391R - CodeReview Interface Program
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
    H**********************************************************************
     D date_test1      s               d   datfmt(*usa)
     D date_test2      s              7  0 inz(0641117)
     D date_test3      s             10
     D from            s              7  0 inz(1020312)
     D to              s              7  0 inz(*zeros)
     D x               s              7  0
     D y               s              7  0 inz(*zeros)
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
     D from_date_in                  10a
     D to_date_in                    10a
     D pos_in                         2a
     D charge_amt_in                  9a
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
     C*
     C     *cymd         move      date_test2    date_test1
     C                   move      date_test1    date_test3
     C     *cymd         move      from          date_test1
     C                   move      date_test1    from_date_in
     C*
     C                   reset                   in_area
     C*
     C                   eval      y = 1
     C     1             do        4             x
     C                   eval      y = y + 2
     C                   enddo
     C*
     C                   do
     C                   eval      y = y - 1
     C                   if        y = 0
     C                   leave
     C                   endif
     C                   enddo
     C*
     C*End the Program
     C*
     C                   eval      *inlr = *on
