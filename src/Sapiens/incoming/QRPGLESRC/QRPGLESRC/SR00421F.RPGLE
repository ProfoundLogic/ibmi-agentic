     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR00421F - Foreign Terrorism Premium

      //===============================================================*
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //===============================================================*
      // CREATED:                                                      *
      //                                                               *
      // 01/31/22 C: 079 Project: WC2853                               *
      //                                                               *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      //                For single print endorsements provide specific *
      //                group and employer so that correct sig is found*
      //                                                               *
      // 01/27/23 C:291 Project: WC2863                                *
      //                State Include/Exclude Endorsement Print Control*
      //                                                               *
      // 10/26/22 C:333 Project: WC2834                                *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 08/16/22 C:65  Project: Phw#103318                            *
      //                Correct an infinite loop in OTHINF - thre is   *
      //                an Iter statement but the loop is not built to *
      //                support one, instead it never ends.            *
      //                                                               *
      //===============================================================*
      // Notes:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      //                                                               *
      //  QQMODE = 1 - Called to determine if form should be attached. *
      //  QQMODE = 2 - Called to print endorsement.                    *
      //  QQMODE = 3 - Called for additional input.                    *
      //  QQMODE = 4 - Called for mid-term processing.                 *
      //===============================================================*
      // Copybook F specs
     F/COPY SCOPYEN01F
      //* Statistical Reporting/Rating File
     Fwdrrl     if   e           k disk
      //* Statistical Reporting/Rating File
     Fwxfmp3    o    e             disk    usropn
      // Copybook D specs
     D/COPY SCOPYEN01D

     D                 DS
     D  ST                                 like(fmcd01)
     D                                     DIM(52) ASCEND

     D                 DS
     D  RT                                 like(fmrt01)
     D                                     DIM(52) ASCEND

     D                 DS
     D  PR                                 like(fmam01)
     D                                     DIM(52) ASCEND

     D  FM1ds          ds
     dfmc401
     dfmc402
     dfmc403
     dfmc404
     dfmc405
     dfmc406
     dfmc407
     dfmc408
     dfmc409
     dfmc410
     dfmc411
     dfmc412
     dfmc413
     dfmc414
     dfmc415
     dfmc416
     dfmc417
     dfmc418
     dfmc419
     dfmc420
     dfmc421
     dfmc422
     dfmc423
     dfmc424
     dfmc425
     dfmc426
     dfmc427
     dfmc428
     dfmc429
     dfmc430
     dfmc431
     dfmc432
     dfmc433
     dfmc434
     dfmc435
     dfmc436
     dfmc437
     dfmc438
     dfmc439
     dfmc440
     dfmc441
     dfmc442
     D fm1                                 OVERLAY(fm1ds)
     D                                     LIKE(fmc401) DIM(42)

     D  FM2ds          ds
     dfmnm01
     dfmnm02
     dfmnm03
     dfmnm04
     dfmnm05
     dfmnm06
     dfmnm07
     dfmnm08
     dfmnm09
     dfmnm10
     D fm2                                 OVERLAY(fm2DS)
     D                                     LIKE(fmnm01) DIM(10)

     d x1              s              3  0

     d stsav           s                   like(fmcd01)
     d rtsav           s                   like(fmrt01)
     d prsav           s                   like(fmam01)

     d sttmp           s                   like(fmcd01)
     d rttmp           s                   like(fmrt01)
     d prtmp           s                   like(fmam01)

     d #1co#           s              3    inz(*blanks)
     d #1fnd           s              3    inz(*blanks)
     d ##rtcd          s              3    inz(*blanks)
     d ##clas          s              5    inz(*blanks)
     d ##efdt          s              7    inz(*blanks)
     d ##lcm           s              5    inz(*blanks)
     d xxlcm           s              5  4
     d temp            s              5  0

     d kkefdt          s                   like(elpfdt)
     d intCxDate       s                   like(elpfdt)

     D qqForm          s             15A   Inz('WC000421F ')

     D AddPgm          c                   const('SRXXXXXXXX')

     D SR00421F        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00421F        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     D GetLossCostM    pr                  extpgm('SRLCM')
     D  qqCo#                         3
     D  qqFmd                         3
     D  ELrtcd                        3
     D  ##clas                        5
     D  ##efdt                        7
     D  ##Lcm                         5

     d GetEffDat       pr                  extPgm('SREFFD')
     d  qqCO#                         3
     d  qqFND                         3
     d  qqFyr                         3
     d  qqEmp#                        9
     d  qqDiv                         5
     d  ##efdt                        7

      /copy wprendorse
      //***** Beg Del *** 10/26/22 ***********************************
      ***/copy wprEmplr
      //***** End Del *** 10/26/22 ***********************************
      /copy sCopyEn01c
      /Free
       //=======================================================================
       //  OTHINF - Subroutine to load form specific information & write record
       //=======================================================================
       Begsr OthInf;
          // open work file
          Open WXFMP3;
          //    initialize
          Clear pfm03;
          // Get state / rate information
          FMform = xxForm;

          Setll (xxCo#:xxFnd:xxFyr:xxEmp#) WDELL1;
          Reade (xxCo#:xxFnd:xxFyr:xxEmp#) WDELL1;

          Dow Not %EOF(WDELL1);
          //***** Beg Add *** 10/26/22 ******************************
             xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
             If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
          //***** End Add *** 10/26/22 ******************************
                Chain (xxCo#:ELrtcd) WMRSL;

                kkefdt = svStRptEfDt;
                //*** Beg Chg ***** 01/27/23 *******************************
                //***If %Found(WMRSL) and isIncludedState(ELco#:qqForm:RSjrSt:kkEfDt)
                //***   = *On;
                If %Found(WMRSL);
                   Exsr ChkApprExclSt;
                   If isApprExclSt = *On;
                //*** End Chg ***** 01/27/23 *******************************
                      Setgt (ELco#:ELfnd:RSjrst:kkEfdt) WDRRL;
                      Readpe (ELco#:ELfnd:RSjrst) WDRRL;

                   // Exclude flat cancalled policies
                      intCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
                      If ELpfDt = ELptDt or (ELpfDt >= intCxDate and
                      intCxDate <> 0);
                   //*** begin add ***  08/16/22  ******************************
                         Reade (xxCo#:xxFnd:xxFyr:xxEmp#) WDELL1;
                   //*** end add *****  08/16/22  ******************************
                         Iter;
                      Endif;
                      If Not %EOF(WDRRL) And RRdtrc <> 0;
                         ##clas = %Editc(RRdtrc:'X');
                         ##efdt = %Editc(svStRptEfDt:'X');
                         GetLossCostM(qqCo#:qqFnd:ELrtcd:##clas:##efdt:##Lcm);
                         temp = %Dec(##Lcm:5:0);
                         xxLcm = Temp / 10000;
                         Eval(h) rtsav  = RRdtrr * xxlcm;
                         stSav = RSjrst;
                         prSav = ELedtr;
                         Exsr SrIns;
                      EndIf;
                   //*** Beg Add ***** 01/27/23 ******************************
                   EndIf;
                   //*** End Add ***** 01/27/23 ******************************

                EndIf;
           //***** Beg Add *** 10/26/22 ******************************
             Endif;
           //***** End Add *** 10/26/22 ******************************

             Reade (xxCo#:xxFnd:xxFyr:xxEmp#) WDELL1;
          EndDo;

          // Write the output record
          Exsr SrOut;
          Write pfm03;
          //  Close the endorsement form workfile
          Close WXFMP3;

       Endsr;
       //=======================================================================
       //  PGMSTR - Beginning of program processing
       //=======================================================================
       Begsr PgmStr;
       Endsr;
       //=======================================================================
       //  PGMEND - End of program processing
       //=======================================================================
       Begsr PgmEnd;
       Endsr;

       //=======================================================================
       //  srins - Insertion sort
       //=======================================================================
       Begsr SrIns;

          x1 = 1;
          Dow x1 <= 52;

             Select;

             When st(x1) = *blanks;
                st(x1) = stSav;
                rt(x1) = rtSav;
                pr(x1) = prSav;
                Leave;

             When stSav > st(x1);
                x1 = x1 + 1;
                Iter;

             When stSav = st(x1);
                If rtSav > rt(x1);
                   rt(x1) = rtSav;
                Endif;

                If prSav <> 0;
                   pr(x1) += prSav;
                EndIf;
                Leave;
                //
             Other;
                stTmp = st(x1);
                rtTmp = rt(x1);
                prTmp = pr(x1);
                st(x1) = stSav;
                rt(x1) = rtSav;
                pr(x1) = prSav;
                stSav = stTmp;
                rtSav = rtTmp;
                prSav = prTmp;
                x1 = x1 + 1;

                Iter;

             Endsl;

          EndDo;

       Endsr;
       //=======================================================================
       // SROUT - Move data from arrays to output fields
       //=======================================================================
       Begsr SrOut;

          x1 = 1;

          FMc313 = EMnam2;
          FMc314 = EMnam2;
          FMc315 = EMfdn2;
          FMc316 = EMfdn2;

          Dow x1 <= 26;

             If st(x1) = *blank;
                Leave;
             Endif;

             fm1(x1) = '    ' +  st(x1) + '   ' +
                 %Editc(rt(x1):'4') + '   ' + %Editc(pr(x1):'K':'$');

             x1 = x1 + 1;

          Enddo;

          x1 = 27;

          Dow x1 <= 42;

             If st(x1) = *blank;
                Leave;
             Endif;

             fm1(x1) = '      ' + st(x1) + '   ' +
                 %Editc(rt(x1):'4') + '   ' + %Editc(pr(x1):'K':'$');

             x1 = x1 + 1;
          Enddo;

          x1 = 43;

          Dow x1 <= 52;

             If st(x1) = *blank;
                Leave;
             Endif;

             fm2(x1-42) = '      ' + st(x1) + '   ' +
                 %Editc(rt(x1):'4') + '   ' + %Editc(pr(x1):'K':'$');

             x1 = x1 +1;

          Enddo;

       Endsr;
       //=======================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=======================================================================
       Begsr Valid;

          kkefdt = svStRptEfDt;
          Setgt (ELco#:ELfnd:RSjrst:kkEfdt) WDRRL;
          Readpe (ELco#:ELfnd:RSjrst) WDRRL;
          //  Attach endorsement if non-zero domestic code for jurisdicti   on s
          If Not %EOF And RRdtrc <> *zero;
             crtFrm = *on;
          Endif;

       Endsr;
      /End-Free
