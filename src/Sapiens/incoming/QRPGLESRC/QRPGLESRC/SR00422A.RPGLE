     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

     H/TITLE SR00422A - TERRORISM RISK INSURANCE PROGRAM              *
      //                 REAUTHORIZATION ACT DISCLOSURE ENDORSEMENT    *
      //===============================================================*
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 01/17/17 C:180 Support:40988                                  *
      //                Change delimeters to |                         *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //
      // 02/17/15 C:302 Support 33331
      //              - Not valid in FL
      //                                                               *
      // 07/08/14 C:242 Support:85985                                  *
      //                Do NOT attach Endorsment Form for Deleted Div  *
      //                                                               *
      // 08/01/08  C:190 PROJECT WC1744                                *
      //           C:61  Created.                                      *
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

      /COPY SCOPYEN01F

      //* Statistical Reporting/Rating File
     Fwdrrl     if   e           k disk

      //* Statistical Reporting/Rating File
     Fwxfmp3    o    e             disk    usropn

      /COPY SCOPYEN01D

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

     D efdt            ds
     D  emefd1
     D  emefd2
     D  emefd3
     D  emefd4
     D  emefd5
     D ef                                  OVERLAY(efdt)
     D                                     LIKE(emefd1) DIM(5)
     D                                     inz

     D cxdt            ds
     D  emcad1
     D  emcad2
     D  emcad3
     D  emcad4
     D  emcad5
     D cx                                  OVERLAY(cxdt)
     D                                     LIKE(emcad1) DIM(5)
     D                                     inz

     D xxefdt          s                   like(emefd1)
     D xxcxdt          s                   like(emcad1)
     D xxsydt          s                   like(emcad1)

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

     D qqForm          s             15A   Inz('WC000422A ')

     D addpgm          c                   const('SRXXXXXXXX')

     D SR00422A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00422A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      //========================================================================
      //  Prototypes
      //========================================================================
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

      /Free

      /COPY SCOPYEN01C

       //=========================================================================
       //  GetCXDT - Get Employer Effective/Cancellation date
       //=========================================================================
       Begsr GetCXDT;

          chain (elco#:elfnd:elemp#:eldiv) pem01;
          x1 = 5;

          Dow  ef(x1) = 0 and (x1 > 1);
             x1 = x1 - 1;
          enddo;
          xxefdt = ef(x1);
          xxcxdt = cx(x1);

       Endsr;

       //=========================================================================
       //  OTHINF - Subroutine to load form specific information & write record
       //=========================================================================
       begsr othinf;

          // open work file
          open wxfmp3;

          // initialize
          clear pfm03;

          FMform = xxForm;
          //  Get state / rate information
          Setll (xxCo#:xxFnd:xxFyr:xxEmp#) WDELL1;
          Reade (xxCo#:xxFnd:xxFyr:xxEmp#) WDELL1;
          Dow Not %EOF(WDELL1);

             Exsr getcxdt;
             If (xxcxdt > elpfdt) or (xxcxdt = 0);
                Chain (xxCo#:ELrtcd) WMRSL;
                If %Found(WMRSL);
                   kkEfdt = ELpfdt;
                   Setgt (ELco#:ELfnd:RSjrst:kkEfdt) WDRRL;
                   Readpe (ELco#:ELfnd:RSjrst) WDRRL;

                   If Not %EOF(wdrrl) And RRterc <> 0;
                      If RSjrst <> 'FL';
                         ##clas = %EditC(RRterc:'X');
                         ##efdt = %EditC(ELpfdt:'X');
                         GetLossCostM(qqCo#:qqFnd:ELrtcd:##clas:##efdt:##Lcm);
                         temp = %dec(##Lcm:5:0);
                         xxLcm = Temp / 10000;
                         eval(h) rtsav  = RRTERR * xxlcm;
                         stSav = RSjrst;
                         prsav  = eleter;
                         exsr srins;
                      Endif;
                   EndIf;
                EndIf;
             Endif;
             Reade (xxCo#:xxFnd:xxFyr:xxEmp#) WDELL1;
          EndDo;

          //* Write the output record

          Exsr srout;

          Write pfm03;
          //
          //* Close the endorsement form workfile
          Close WXFMP3;

       Endsr;

       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr pgmstr;
       Endsr;

       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       begsr pgmend;
       endsr;

       //=========================================================================
       //  srins - Insertion sort
       //=========================================================================
       begsr srins;

          x1 = 1;

          Dow x1 <= 52;

             Select;
                //
             When st(x1) = *blanks;
                st(x1) = stSav;
                rt(x1) = rtSav;
                pr(x1) = prSav;
                Leave;
             When stSav > st(x1);
                x1 = x1 + 1;
                iter;

             When stSav = st(x1);
                If rtSav > rt(x1);
                   rt(x1) = rtSav;
                Endif;
                pr(x1) = prSav;
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

       //=========================================================================
       // SROUT - Move data from arrays to output fields
       //=========================================================================
       begsr srout;

          x1 = 1;

          FMc313 = EMnam2;
          FMc314 = EMnam2;
          FMc315 = EMfdn2;
          FMc316 = EMfdn2;

          Dow x1 <= 26;

             If st(x1) = *blank;
                Leave;
             Endif;
             //
             fm1(x1) = '|' + st(x1) + '|' +
                 %editc(rt(x1):'4') + '|' + %editc(pr(x1):'K':'$');
             x1 = x1 + 1;

          Enddo;

          x1 = 27;

          Dow x1 <= 42;

             If st(x1) = *blank;
                Leave;
             Endif;

             fm1(x1) = '|' + st(x1) + '|' +
                 %editc(rt(x1):'4') + '   ' + %editc(pr(x1):'K':'$');
             x1 = x1 + 1;
          Enddo;

          x1 = 43;

          Dow x1 <= 52;

             If st(x1) = *blank;
                Leave;
             Endif;

             fm2(x1-42) = '|' + st(x1) + '|' +
                 %editc(rt(x1):'4') + '|' + %editc(pr(x1):'K':'$');

             x1 = x1 + 1;
          Enddo;

       Endsr;

       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr valid;

          Exsr getcxdt;
          If (xxCxdt > xxSydt) or (xxCxdt = 0);
             Chain (xxCo#:ELrtcd) WMRSL;
             If %Found;

                kkefdt = ELpfdt;
                Setgt (ELco#:ELfnd:RSjrst:kkEfdt) WDRRL;
                Readpe (ELco#:ELfnd:RSjrst) WDRRL;
                //  Attach endorsement if non-zero domestic code for jurisdiction state
                If Not %EOF And RRterc <> *zero;
                   If RSjrst <> 'FL';
                      crtfrm = *on;
                   EndIf;
                Endif;
             Endif;
          Endif;

       Endsr;
      /End-Free
