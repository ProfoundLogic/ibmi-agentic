     H Copyright('(c) 2020 Sapiens. All Rights Reserved.')
     h/TITLE W1054R04 - Delete "Additional Information" endorsement records
     h option(*nodebugio:*srcstmt)
     h**************************************************************************
     h*  DESCRIPTION:  This program deletes "addtional information" data
     h*                records associated with a single "manual attach"
     h*                policy page endorsement for a policy.
     h*
     h*  Called from:  W1054R02 - Endorsement information
     h*  Calls      :  None
     h*
     h**************************************************************************
     h* Revisions:
     h*
     h* 02/21/20 C:136 Support: 80310
      *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
      *!!! ALL THE OBJECTS FOR THIS SUPPORT ITEM MUST GO TOGETHER  !!!*
      *!!! AND FIX80310 MUST BE RUN.                               !!!*
      *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
     h*                Delete detail per waiver form. i.e. Don't delete
     h*                TX waivers if deleting UT waivers.
     h*
     h* 11/08/10 C:220 project 114wxf
     h*                Add section to delete WLFCP file records
     h*                if matching key: co#/fnd/fyr/emp# for form WC999903.
     h*
     h* 07/14/08 C:61  project WC1505
     h*                Add section for WPBSL.
     h*
     h* 06/09/08 C:61  project 136wds
     h*                Add section for WLL1P and WLL2P.
     h*
     h* 06/06/08 C:197 project 114WQG
     h*                Add section for WLRPP.
     h*
     h* 05/30/08 C:61  project 136wdp
     h*                Add section for WLLRP.
     h*
     h* 04/16/02 C:61  support wa099 44141
     h*                Created.
     h*
     h**************************************************************************


     fwxfml12   uf   e           k disk    usropn
     fwdael     uf   e           k disk    usropn
     fwmewl     uf   e           k disk    usropn
     fwmwvl6    uf   e           k disk    usropn
     fwdl2l     uf   e           k disk    usropn
     fwdlcl     uf   e           k disk    usropn
     fwniel     uf   e           k disk    usropn
     fwmgel     uf   e           k disk    usropn
     fwmiel     uf   e           k disk    usropn
     f                                     rename(pie01:pie02)
     fwlpel     uf   e           k disk    usropn
     fwlpol     uf   e           k disk    usropn
      ***** beg add ****** 05/30/08 ********************************************
     fwllrl     uf   e           k disk    usropn
      ***** end add ****** 05/30/08 ********************************************
      ***** beg add ****** 06/09/08 ********************************************
     fwll1l     uf   e           k disk    usropn
     fwll2l     uf   e           k disk    usropn
     f                                     rename(pl201:pl202)
     f                                     prefix(en_)
      ***** end add ****** 06/09/08 ********************************************
     fwmfrl     uf   e           k disk    usropn
      ***** beg add ****** 06/06/08 ********************************************
     fwlrpl     uf   e           k disk    usropn
      ***** end add ****** 06/06/08 ********************************************
      ***** beg add ****** 07/14/08 ********************************************
     fwpbsl     uf   e           k disk    usropn
      ***** end add ****** 07/14/08 ********************************************
      ***** beg add ****** 11/08/10 ********************************************
     fwlfcl     uf   e           k disk    usropn
      ***** end add ****** 11/08/10 ********************************************
      *** beg add ***** 02/21/20 ***********************************************
     fwtenl     if   e           k disk    rename(pen01:pen00)
      *** end add ***** 02/21/20 ***********************************************

      *=========================================================================
      * KLISTS
      *=========================================================================

     c     wxfml12_key   klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@emp#
     c                   kfld                    @@fyr
     c                   kfld                    @@form
     c                   kfld                    kkend#            5 0

     c     wdael_key     klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@emp#

     c     wmewl_key     klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@emp#

     c     wmwvl6_key    klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@emp#
     c                   kfld                    @@fyr
     c                   kfld                    kkend#            5 0

     c     wdl2l_key     klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@emp#

     c     wdlcl_key     klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@emp#

     c     wniel_key     klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@emp#

     c     wmgel_key     klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@emp#

     c     wmiel_key     klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@emp#

     c     wlpel_key     klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@emp#

     c     wlpol_key     klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@emp#
      ***** beg add ****** 05/30/08 ********************************************
      ***** beg add ****** 06/09/08 ********************************************

     c     wll1l_key     klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@emp#

     c     wll2l_key     klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@emp#
      ***** end add ****** 06/09/08 ********************************************

     c     wllrl_key     klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@emp#
      ***** end add ****** 05/30/08 ********************************************

     c     wmfrl_key     klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@emp#
     c                   kfld                    @@fyr
      ***** beg add ****** 06/06/08 ********************************************

     c     wlrpl_key     klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@emp#
      ***** end add ****** 06/06/08 ********************************************
      ***** beg add ****** 07/14/08 ********************************************

     c     wpbsl_key     klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@emp#
      ***** end add ****** 07/14/08 ********************************************
      ***** beg add ****** 11/08/10 ********************************************
     c     wlfcl_key     klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@emp#
      ***** end add ****** 11/08/10 ********************************************


      *=========================================================================
      * PLISTS
      *=========================================================================
     c     *entry        plist
     c                   parm                    @@co#             3 0
     c                   parm                    @@fnd             3 0
     c                   parm                    @@emp#            9 0
     c                   parm                    @@fyr             3 0
     c                   parm                    @@form           15

     c*=========================================================================
     c* Main Processing
     c*=========================================================================


     c                   z-add     0             kkend#
     c                   movel     @@form        xxform            8

      *  Additional information file

     c                   if        @@form <> *blanks
     c                   open      wxfml12
     c     wxfml12_key   delete    pfm01
     c                   close     wxfml12
     c                   endif

      *  Alternate Employer

     c                   if        xxform = 'WC000301'
     c                   open      wdael
     c     wdael_key     delete    pae01
     c                   close     wdael
     c                   endif

      *  Excluded Work Place

     c                   if        xxform = 'WC000302'
     c                   open      wmewl
     c     wmewl_key     delete    pew01
     c                   close     wmewl
     c                   endif

      *** beg chg ***** 02/21/20 ********************************
     c***                   if        (xxform = 'WC000313')
     c***                             or (xxform = 'WC040306')
     c***                             or (xxform = 'WC430305')
     c***                   open      wmwvl6
     c***     wmwvl6_key    setll     pwv01
     c***     wmwvl6_key    reade     pwv01
      ***
     c***                   dow       not %eof
      ***
     c***                   if        (wvend# = 0)
      ***
     c***                   if        (xxform = 'WC000313')
     c***                             and (wvemst <> 'CA')
     c***                             and (wvemst <> 'UT')
     c***                   delete    pwv01
     c***                   endif
      ***
     c***                   if        (xxform = 'WC040306')
     c***                             and (wvemst = 'CA')
     c***                   delete    pwv01
     c***                   endif
      ***
     c***                   if        (xxform = 'WC430305')
     c***                             and (wvemst = 'UT')
     c***                   delete    pwv01
     c***                   endif
      ***
     c***                   endif
      ***
     c***     wmwvl6_key    reade     pwv01
     c***                   enddo
      ***
     c***                   close     wmwvl6
     c***                   endif

      *  Waiver of Subrogation

      /Free
       Chain (@@Co#:@@Fnd:@@Form) pen00;
       If %Found And ENtype = 'Waiver    ';
          Open WMWVL6;
          Setll (@@Co#:@@Fnd:@@Emp#:@@Fyr:0) pwv01;
          Dou %Eof(WMWVL6);
             Reade (@@Co#:@@Fnd:@@Emp#:@@Fyr:0) pwv01;
             If %Eof;
                Leave;
             EndIf;
             If WVenm = @@Form;
                Delete pwv01;
             Endif;
          Enddo;
          Close WMWVL6;
       Endif;
      /End-Free
      *** end chg ***** 02/21/20 ********************************

      *  Labor Contractor

     c                   if        xxform = 'WC000320'
     c                   open      wdl2l
     c     wdl2l_key     delete    pl201
     c                   close     wdl2l
     c                   endif

      *  Labor Contractor Exclusion

     c                   if        xxform = 'WC000321'
     c                   open      wdlcl
     c     wdlcl_key     delete    plc01
     c                   close     wdlcl
     c                   endif

      *  Employee Leasing

     c                   if        xxform = 'WC020301'
     c                   open      wniel
     c     wniel_key     delete    pie01
     c                   close     wniel
     c                   endif

      *  GA Employee Leasing

     c                   if        xxform = 'WC100301'
     c                   open      wmgel
     c     wmgel_key     delete    pge01
     c                   close     wmgel
     c                   endif

      *  IL Employee Leasing

     c                   if        xxform = 'WC120303'
     c                   open      wmiel
     c     wmiel_key     delete    pie02
     c                   close     wmiel
     c                   endif

      *  LA Professional Employment Organization (PEO)

     c                   if        xxform = 'WC170304'
     c                   open      wlpel
     c     wlpel_key     delete    plp01
     c                   close     wlpel
     c                   endif

      *  LA PEO Exclusion

     c                   if        xxform = 'WC170305'
     c                   open      wlpol
     c     wlpol_key     delete    ple01
     c                   close     wlpol
     c                   endif
      ***** beg add ****** 05/30/08 ********************************************

      *  clt136 WC890712 WLLRP

     c                   if        xxform = 'WC990712'
     c                   open      wllrl
     c                   dou       not %found
     c     wllrl_key     delete    plr01
     c                   enddo
     c                   close     wllrl
     c                   endif
      ***** end add ****** 05/30/08 ********************************************
      ***** beg add ****** 06/09/08 ********************************************

      *  clt136 WC040314 WLL1P

     c                   if        xxform = 'WC040314'
     c                   open      wll1l
     c                   dou       not %found
     c     wll1l_key     delete    pl101
     c                   enddo
     c                   close     wll1l
     c                   endif

      *  clt136 WC040316 WLL2P

     c                   if        xxform = 'WC040316'
     c                   open      wll2l
     c                   dou       not %found
     c     wll2l_key     delete    pl202
     c                   enddo
     c                   close     wll2l
     c                   endif
      ***** end add ****** 06/09/08 ********************************************
      ***** beg add ****** 07/14/08 ********************************************

      *         WC140602 WPBSL

     c                   if        xxform = 'WC140602'
     c                   open      wpbsl
     c                   dou       not %found
     c     wpbsl_key     delete    pbs01
     c                   enddo
     c                   close     wpbsl
     c                   endif
      ***** end add ****** 07/14/08 ********************************************
      ***** beg add ****** 11/08/10 ********************************************

      *  clt114 WC999903 WLFCP

     c                   if        xxform = 'WC999903'
     c                   open      wlfcl
     c                   dou       not %found
     c     wlfcl_key     delete    pfc01
     c                   enddo
     c                   close     wlfcl
     c                   endif
      ***** end add ****** 11/08/10 ********************************************


      *  CA Approved Form 10

     c                   if        xxform = 'WC99FR10'
     c                   open      wmfrl
     c     wmfrl_key     delete    pfr01
     c                   close     wmfrl
     c                   endif


     c                   eval      *inlr = *on
      ***** beg add ****** 06/06/08 ********************************************

      *  clt114 WC000509A WLRPP

     c                   if        xxform = 'WC000509'
     c                   open      wlrpl
     c                   dou       not %found
     c     wlrpl_key     delete    prp01
     c                   enddo
     c                   close     wlrpl
     c                   endif
      ***** end add ****** 06/06/08 ********************************************

