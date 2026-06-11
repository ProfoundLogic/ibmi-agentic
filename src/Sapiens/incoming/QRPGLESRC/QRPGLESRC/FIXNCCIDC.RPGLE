     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title FIXNCCIDC- Fix Program to Clear Group/State Flags.
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*CALLER)
     h**********************************************************************
     h*===============================================================*
     h*  Created   : 11/24/14                                         *
     h*  Programmer: C:296                                            *
     h*  Project   : 114MQA                                           *
     h*===============================================================*
     h*  Revisions:                                                   *
     H*
     H* 06/08/15 C:280 Phaseware: 34144 inhouse
     H*                Change to user open on wcms only
     h*                                                               *
     h*                                                               *
     h* ------------------------------------------------------------- *
     h*  NOTE: Please add comments to the top of Revisions.           *
     h* ------------------------------------------------------------- *
     h*===============================================================*
     f*
     f***** Beg Chg 06/08/15 ******************************************
     f*****wmcxp     uf a e             disk
     f*
     fwmcxp     uf a e             disk    usropn
     f*
     f***** End Chg 06/08/15 ******************************************
     f*
     fststl     uf a e             disk

     d* Stand alone variables.
     d*
     dpsds           esds                  extname(szQ1p)
     d*---------------------------------------------------
     d GetSysflg       pr                  extpgm('SA001R')
     d ##wcif                         1A
     d ##mcif                         1A
     d ##wcin                         1A
     d ##ar                           1A
     d ##ntwk                         1A
     d ##ur                           1A
     d ##med                          1A
     d ##edi                          1A
     d ##wckd                         1A
     d ##rhab                         1A
     d ##mss                          1A
     d ##fsch                         1A
     d ##prvb                         1A
     d ##npwc                         1A
     d ##os                           1A
     d ##arls                         1A
     d ##ccod                         1A
     d ##cchk                         1A
     d ##prom                         1A
     d ##mcrv                         1A
     d ##freq                         1A
     d ##crac                         1A
     d ##preo                         1A
     d ##parn                         1A
     d ##mgri                         1A
     d ##kcif                         1A
     d ##curs                         1A
     d ##pcib                         1A
     d ##clt#                         1A
     d ##ucpf                         1A
     d ##dcf                          1A
     d ##autb                         1A
     d ##deif                         1A
     d ##aure                         1A
     d ##cred                         1A
     d ##idrt                         1A
     d ##cprv                         1A
     d ##eft                          1A
     d ##govr                         1A
     d ##ncdc                         1A
     d*---------------------------------------------------
     d ##wcif          s              1A   inz(*blanks)
     d ##mcif          s              1A   inz(*blanks)
     d ##wcin          s              1A   inz(*blanks)
     d ##ar            s              1A   inz(*blanks)
     d ##ntwk          s              1A   inz(*blanks)
     d ##ur            s              1A   inz(*blanks)
     d ##med           s              1A   inz(*blanks)
     d ##edi           s              1A   inz(*blanks)
     d ##wckd          s              1A   inz(*blanks)
     d ##rhab          s              1A   inz(*blanks)
     d ##mss           s              1A   inz(*blanks)
     d ##fsch          s              1A   inz(*blanks)
     d ##prvb          s              1A   inz(*blanks)
     d ##npwc          s              1A   inz(*blanks)
     d ##os            s              1A   inz(*blanks)
     d ##arls          s              1A   inz(*blanks)
     d ##ccod          s              1A   inz(*blanks)
     d ##cchk          s              1A   inz(*blanks)
     d ##prom          s              1A   inz(*blanks)
     d ##mcrv          s              1A   inz(*blanks)
     d ##freq          s              1A   inz(*blanks)
     d ##crac          s              1A   inz(*blanks)
     d ##preo          s              1A   inz(*blanks)
     d ##parn          s              1A   inz(*blanks)
     d ##mgri          s              1A   inz(*blanks)
     d ##kcif          s              1A   inz(*blanks)
     d ##curs          s              1A   inz(*blanks)
     d ##pcib          s              1A   inz(*blanks)
     d ##clt#          s              1A   inz(*blanks)
     d ##ucpf          s              1A   inz(*blanks)
     d ##dcf           s              1A   inz(*blanks)
     d ##autb          s              1A   inz(*blanks)
     d ##deif          s              1A   inz(*blanks)
     d ##aure          s              1A   inz(*blanks)
     d ##cred          s              1A   inz(*blanks)
     d ##idrt          s              1A   inz(*blanks)
     d ##cprv          s              1A   inz(*blanks)
     d ##eft           s              1A   inz(*blanks)
     d ##govr          s              1A   inz(*blanks)
     d ##ncdc          s              1A   inz(*blanks)
     C*
     C* Main program processing
      /Free
       exsr SrMain;

       begsr SrMain;
         GetSysflg (##wcif :##mcif :##wcin :##ar
                    :##ntwk :##ur :##med :##edi :##wckd
                    :##rhab :##mss :##fsch :##prvb :##npwc
                    :##os :##arls :##ccod :##cchk :##prom
                    :##mcrv :##freq :##crac :##preo :##parn
                    :##mgri :##kcif :##curs :##pcib :##clt#
                    :##ucpf :##dcf :##autb :##deif :##aure
                    :##cred :##idrt :##cprv :##eft :##govr
                    :##ncdc);

       // *** Beg Chg ** 06/08/15 *************************************

       // **** if ##ncdc = 'Y'; **************************************

            if ##ncdc = 'Y' and ##wcif = 'Y';

              open wmcxp;

       // *** End Chg ** 06/08/15 *************************************

              exsr SrUpdWmcxp;
              exsr SrUpdStstl;

       // *** Beg Chg ** 06/08/15 *************************************

              close wmcxp;

       // *** End Chg ** 06/08/15 *************************************

           endif;

         *inlr = *on;
       endsr;

       // Subroutine to update "NOI/CLAIM INTAKE DARTS" file.
       begsr SrUpdWmcxp;
         setll *start wmcxp;
         read pcx01;

         dow not %eof(wmcxp);

           // if this group was excluded from NCCI medical data  call
           // Reportin. Then make this flag as blank.
           if CXexNm = 'Y';
              CXexNm = *blanks;
              CXuser = Q1user;
              CXpgm  = Q1Pgm;
              CXtime = %Dec(%Time():*hms);
              CXdt   = %Dec(%Date():*cymd);
                update pcx01;
           endif;
          read pcx01;
         enddo;
       endSr;

       // Subroutine to update "STATE CODE FILE" file.
       begsr SrUpdStstl;
         setll *start ststl;
         read pst01;

         dow not %eof(ststl);

           // if this was NCCI MEDICAL DATA CALL then
           // make flag stncdc as blank.
           if STncDc = 'Y';
              STncDc = *blanks;
              STuser = Q1user;
              STpgm  = Q1Pgm;
              STtime = %Dec(%Time():*hms);
              STdt   = %Dec(%Date():*cymd);
                update pst01;
           endif;
          read pst01;
         enddo;
       endSr;
      /End-Free

