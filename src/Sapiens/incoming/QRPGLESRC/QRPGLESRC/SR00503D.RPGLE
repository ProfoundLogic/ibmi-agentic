     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR00503D - Retrospective Rating Plan Premium Endorsment
      //                Print/Attach program.
      //===============================================================*
      // WC000503D - Retrospective Rating Plan Premium Endorsement.    *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      // Created   : 02/20/20                                          *
      // Programmer: C:079                                             *
      // Project   : WC2729                                            *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 10/27/22 C:333 Project: WC2834                                *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 02/20/20 C:079 Project: WC2729                                *
      //                Remove references to WMFMP3                    *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //
      //  04/01/15 C:302 Support item 32953                           *
      //                 Add standards premium from all retro         *
      //                 divisions.   Calculate the 50% and           *
      //                 the 150% amounts.                            *
      //                                                              *
      // ==============================================================*
      // Notes:                                                        *
      //                                                               *
      // Utilizes the endorsement copybook (SCOPYEN01F/I/C).           *
      //                                                               *
      // QQMODE = 1 - Called to determine if form should be attached.  *
      // QQMODE = 2 - Called to print endorsement.                     *
      // QQMODE = 3 - Called for additional input.                     *
      // QQMODE = 4 - Called for mid-term processing.                  *
      //==============================================================*
      /copy scopyen01f

     fwtabl     if   e           k disk    Include(pbs01)

     f*** Beg Del *** 02/20/20 *********************************************
     f****wxfmp3    o    e             disk    Usropn
     f*** End Del *** 02/20/20 *********************************************
     fwxpel     uf a e           k disk
     fwxpel1    if   e           k disk
     f                                     Rename(ppe01:ppe02)
     fststl3    if   e           k disk
     fwmepl1    if   e           k disk
     fwmprl1    if   e           k disk
     fwmexl     if   e           k disk
     f
      /copy scopyen01d
      /copy wwcpoldat4

     d fact            ds                  Occurs(60)
     d  x1St                          2a
     d  x1Elp                         7  4
     d  x1Elpf                        7  4
     d  x1Tax                         7  4
     d  x1Taxf                        7  4
     d  x1Rdf1                        7  4
     d  x1Rdf2                        7  4
     d  x1Rdf3                        7  4
     d  x1Num                         5  0
      * payroll array
     d xpAra           ds                  Occurs(255)
     d  xpClas                        5  0
     d  xpPrem                       15  2
     d  xpHG                          1

     d x1              s              2  0
     d z               s              3  0
     d xxUnid          s                   Like(peunid)
     d x1End#          s                   Like(peend#)
     d #1Pgm           s             10
     d #1Flag          s              1
     d x1Type          s              1
     d svHG            s              1
     d xxKey           s             10
     d svCode          s              5  0
     d svPrem          s             15  2
     d xxDt            s              7  0
     d xxRtcd          s              3
     d xxClas          s              5  0
     d xxEfdt          s              7  0
     d qqForm          s             15A   INZ('WC000503D ')
     d addPgm          c                   Const('SRXXXXXXXX')

     d ChkProgInSt     pr                       ExtPgm('SRCHKPGM')
     d  #1Pgm                        10
     d  #1Flag                        1

     d CvtDatYMD       pr                       ExtPgm('SRCVTDAT')
     d  srDti                         8  0
     d                               10    Const
     d                                8    Const
     d  srDto                         8
     d  srErr                         1

     d RetroPremEnd    pr                       ExtPgm('SR00503ZD')
     d  qqCo#                         3
     d  qqFnd                         3
     d  qqEmp#                        9
     d  qqDiv                         5
     d  qqFyr                         3
     d  qqMode                        1
     d  qqRrn                         9  0
     d  qqMod2                        1

     D SR00503D        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00503D        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      * Employer Master
     c     keyWMEML      Klist
     c                   Kfld                    xxCo#
     c                   Kfld                    xxFnd
     c                   Kfld                    xxEmp#
      * Billing Schedule
     c     keyWTABL      Klist
     c                   Kfld                    xxCo#
     c                   Kfld                    xxKey

     c     keyWXPEL1     Klist
     c                   Kfld                    xxCo#
     c                   Kfld                    xxFnd
     c                   Kfld                    xxEmp#
     c                   Kfld                    xxFyr
     c                   Kfld                    xxDt

     c     key1WXPEL1    KList
     c                   KFld                    xxCo#
     c                   KFld                    xxFnd
     c                   KFld                    xxEmp#
     c                   KFld                    xxFyr

     c     keyWXPEL      KList
     c                   KFld                    xxCo#
     c                   KFld                    xxFnd
     c                   KFld                    xxEmp#
     c                   KFld                    xxFyr
     c                   KFld                    xxUnid
     c                   KFld                    x1End#

     c     keySTstl3     KList
     c                   KFld                    xxKey

     c     keyWMEPL1     KList
     c                   KFld                    xxCo#
     c                   KFld                    xxFnd
     c                   KFld                    xxFyr
     c                   KFld                    xxEmp#

     c     keyWMPRL1     KList
     c                   KFld                    xxCo#
     c                   KFld                    xxRtcd
     c                   KFld                    xxClas
     c                   KFld                    xxEfdt

     c     key1WMPRL1    KList
     c                   KFld                    xxCo#
     c                   KFld                    xxRtcd
     c                   KFld                    xxClas

     c     keyWMEXL      KList
     c                   KFld                    xxCo#
     c                   KFld                    xxFnd

      /Copy scopyen01c

      /Free

       //=========================================================================
       //  AddFact - Add factors by state
       //=========================================================================
       Begsr AddFact;

          x1 = 1;
          Dow x1 <= 60;
             %Occur(fact) = x1;
             Select;
             When x1St = *Blanks;
                x1St   = RSjrSt;
                x1Elp  = ELelp;
                x1Elpf = ELelpf;
                x1Tax  = ELtax;
                x1Taxf = ELtaxf;
                x1Rdf1 = ELrdf1;
                x1Rdf2 = ELrdf2;
                x1Rdf3 = ELrdf3;
                x1Num  = 1;
                Leave;
             When RSjrSt  = x1St;
                Leave;
             Other;
                x1 += 1;
                Iter;
             EndSl;
          EndDo;
       EndSr;
       //=========================================================================
       //  Othinf - Subroutine to load form specific information & write record
       //=========================================================================
       Begsr Othinf;

       // Open work file

        //*** Beg Del *** 02/20/20 *****************************************
        //***  Open  wxfmp3;
        //*** End Del *** 02/20/20 *****************************************
          Chain keyWMEXL pex01;

          If %Found(wmexl);
             FMfl01 = Exretg;
          EndIf;

       // Initialize
        //*** Beg Del *** 02/20/20 *****************************************
        //***  Clear pfm03;
        //*** End Del *** 02/20/20 *****************************************
          FMform = xxform;

       // Get info from state reporting division
          Setll keyWMEML pem01;
          Reade keyWMEML pem01;
          Dow Not %EOF(wmeml) and (emstrp <> 'Y');
             Reade keyWMEML pem01;
          EndDo;

          If %Found;
             FMam01 = ELretn;
             FMa541 = ELlcvf;
             FMa542 = ELmnfc;
             FMa543 = ELmxfc;
             FMa544 = ELbsfc;
          EndIf;

       // Get factors for each state
          Clear *all fact;

          Setll  ELky2  pel02;
          Reade  ELky2  pel02;
          Dow Not %EOF(Wdell1);
          //***** Beg Add *** 10/27/22 ******************************
             xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
             If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
          //***** End Add *** 10/27/22 ******************************

                xxKey = %EditC(ELbscd:'X');
                Chain keyWTABL pbs01;

                If %Found(wtabl) And ((BSlPgm = 'P') or (BSlPgm = 'I'));

                   FMam02 = FMam02 + ELstpr;
                   Chain RSky prs01;
                   If %Found(wmrsl);
                       Exsr AddFact;
                   EndIf;
                EndIf;
           //***** Beg Add *** 10/27/22 ******************************
             Endif;
           //***** End Add *** 10/27/22 ******************************
             ReadE ELky2 pel02;
          EndDo;

          //  Calculate the 50% and the 150%
           FMam03 = FMam02 * .5;
           FMam04 = FMam02 * 1.5;
          // Write factors to output work file
          Exsr WrtFact;

          // Write the output record
        //*** Beg Del *** 02/20/20 *****************************************
        //***  Write pfm03;
        //*** End Del *** 02/20/20 *****************************************

          // Close the endorsement form workfile
        //*** Beg Del *** 02/20/20 *****************************************
        //***  Close wxfmp3;
        //*** End Del *** 02/20/20 *****************************************

          // Write records to the Policy Page Endorsement Transaction File
          Exsr TransWrt;

       EndSr;
       //==========================================================================
       //   PayHg - Subroutine to determine hazard group for policy.
       //==========================================================================
       Begsr PayHg;
          svDiv  = xxDiv;
          xxEfDt = ELpfdt;
          svCode = 0;
          svPrem = 0;

          For z = 1 to 255;
             %Occur(xpAra) = z;
             Clear xpAra;
          EndFor;

          // Check All Payroll Records For Pplicy Year.
          Setll keyWMEPL1 pep01;
          Reade keyWMEPL1 pep01;
          Dow Not %Eof(wmepl1);
             If EPstat <> 'I';
                xxDiv = EPdiv;

                Chain ELky pel01;
                If %Found(wdell);
                   xxRtcd = ELrtcd;
                Endif;

               // Set Multi Occr DS to Matching Class Element Or Next Available.
                For z = 1 to 255;
                   %Occur(xpAra) = z;

                   // Next Available
                   If xpClas = *Zero;
                      xpClas = EPclas;
                      Leave;
                   Endif;
                   // Matching Class
                   IF xpClas = EPclas;
                      Leave;
                   EndIf;
                EndFor;

                // Total Payroll Field.
                xpPrem += EPempr;
                xxClas = xpClas;

                SetGt  keyWMPRL1  ppr01;
                ReadPe key1WMPRL1 ppr01;
                If Not %EOF(wmprl1);
                   xpHG = PRhzgr;
                EndIf;

             EndIf;
             Reade keyWMEPL1 pep01;
          EndDo;

          // Check For Class With Highest Premium Total.
          For z = 1 to 255;
             %Occur(xpAra) = z;
             If xpPrem >= svPrem And xpClas <> *Zero;
                svCode = xpClas;
                svPrem = xpPrem;
                svHg   = xpHG;
             EndIf;
          EndFor;

          xxDiv  = svDiv;
          Chain ELKY pel01;
       Endsr;
       //=========================================================================
       //  PgmEnd - End of program processing
       //=========================================================================
       BegSr PgmEnd;
          If  (qqMode = '2' and EXretg = 'Y');
              RetroPremEnd(qqCo#:qqFnd:qqEmp#:qqDiv:qqFyr:qqMode:qqRrn:qqMod2);
          EndIf;
       EndSr;
       //=========================================================================
       //  PgmStr - Beginning of program processing
       //=========================================================================
       Begsr PgmStr;
       Endsr;
       //=========================================================================
       //  TransWrt - Write records to Policy Page End Transaction File
       //=========================================================================
       BegSr TransWrt;

       // If printing a policy page.  (Not called from W1054R)
          #1Pgm  = 'W1054R   ';
          #1Flag = *Blank;
          ChkProgInSt(#1Pgm:#1Flag);

          If #1Flag = '0';
       // Update the endorsement reporting file with the proper data.
             x1Type  = 'P';
             xxUnid  = 0;
             xxDt    =  *Hival;

             SetGt  keyWXPEL1  ppe02;
             Readpe key1WXPEL1 ppe02;

             If Not %EOF(wxpel1);
                xxUnid = PEunid;
             EndIf;

             x1End# = xxForm;
             Chain keyWXPEL ppe01;

             If %Found(wxpel);
                //Copy data into the record.
                svHG = *Blank;
                Exsr PayHg;

                I = 1;
                %Occur(fact) = I;

                If x1Num <> 0;
                   xxKEY = x1St;
                   Chain keySTstl3 pst01;
                   If %Found(ststl3);
                      PEst = %Int(STnst);
                   EndIf;

                   E00503 = *Zeros;
                   xxLlim = FMam01;
                   xxLcon = FMa541;
                   xxHG   = svHG;
                   xxLfst = x1elp;
                   XXLffe = x1Elpf;
                   XXRdf1 = x1Rdf1;
                   XXRdf2 = x1Rdf2;
                   XXRdf3 = x1Rdf3;
                   xxTxst  =  x1Tax;
                   xxTxfe  =  x1Taxf;
                   xxM050  =  0;
                   xxM100  =  FMa542;
                   xxM150  =  0;
                   xxX050  =  0;
                   xxX100  =  FMa543;
                   xxX150  =  0;
                   xxB050  =  0;
                   xxB100  =  FMa544;
                   xxB150  =  0;
                   xxE050  =  0;
                   xxE100  =  FMam02;
                   xxE150  = 0;
                   PEdata  = *Blanks;
                   PEdata  = E00503;
                   PEuser = q1User;
                   PEpgm  = q1Pgm;
                   PEtime = dsTime;
                   srDti  = dsDate;
                   CvtDatYMD(srDti:*Blanks:'*CYMD':srDto:srErr);
                   PEdt   = %Int(srDto);
                   Update ppe01;
                EndIf;

                I +=1;

                Dow I <= 5;
                   %Occur(fact) = I;
                   If x1Num <> 0;
                      xxKey = x1St;
                      Chain keySTstl3 pst01;
                      If %Found(STstl3);
                         PEst = %Int(STnst);
                      EndIf;

                      E00503 = *Zeros;
                      xxLlim = FMam01;
                      xxLcon = FMa541;
                      xxHG   = svHG;
                      xxLfst = x1elp;
                      XXLffe = x1Elpf;
                      XXRdf1 = x1Rdf1;
                      XXRdf2 = x1Rdf2;
                      XXRdf3 = x1Rdf3;
                      xxTxst  =  x1Tax;
                      xxTxfe  =  x1Taxf;
                      xxM050  =  0;
                      xxM100  =  FMa542;
                      xxM150  =  0;
                      xxX050  =  0;
                      xxX100  =  FMa543;
                      xxX150  =  0;
                      xxB050  =  0;
                      xxB100  =  FMa544;
                      xxB150  =  0;
                      xxE050  =  0;
                      xxE100  =  FMam02;
                      xxE150  = 0;
                      PEdata  = *Blanks;
                      PEdata  = E00503;
                      PEiusr =  q1User;
                      PEipgm =  q1Pgm;
                      PEitim =  q1Time;
                      srDti = dsDate;
                      CvtDatYMD(srDti:*Blanks:'*CYMD':srDto:srErr);
                      PEidt = %Int(srDto);
                      Write ppe01;
                   EndIf;
                   I += 1;
                EndDo;
             Endif;
          Endif;
       Endsr;
       //=========================================================================
       //  Valid - Subroutine to attach this form to the policy
       //=========================================================================
       BegSr Valid;
          xxKey = %EditC(ELbscd:'X');
          Chain keyWTABL pbs01;
          If %Found(wtabl) and ((BSlPgm = 'P') or (BSlPgm = 'I'));
             Chain RSky prs01;
             If %Found(wmrsl) and (RSjrSt <> 'CA');
             // Found a division with a matching jurisdiction state
                CrtFrm = *on;
             Endif;
          Endif;
       Endsr;
       //=========================================================================
       //  WrtFact - Write Factors to output work file
       //=========================================================================
       BegSr WrtFact;
          %Occur(fact) = 1;
          FMcd01 = x1St;
          FMrt01 = x1Elp;
          FMrt02 = x1Elpf;
          FMrt03 = x1Tax;
          FMrt04 = x1Taxf;
          FMrt05 = x1Rdf1;
          FMrt06 = x1Rdf2;
          FMrt07 = x1Rdf3;

          %Occur(fact) = 2;
          FMcd11 = x1St;
          FMrt11 = x1Elp;
          FMrt12 = x1Elpf;
          FMrt13 = x1Tax;
          FMrt14 = x1Taxf;
          FMrt15 = x1Rdf1;
          FMrt16 = x1Rdf2;
          FMrt17 = x1Rdf3;

          %Occur(fact) = 3;
          FMcd21 = x1St;
          FMrt21 = x1Elp;
          FMrt22 = x1Elpf;
          FMrt23 = x1Tax;
          FMrt24 = x1Taxf;
          FMrt25 = x1Rdf1;
          FMrt26 = x1Rdf2;
          FMrt27 = x1Rdf3;

          %Occur(fact) = 4;
          FMcd31 = x1St;
          FMrt31 = x1Elp;
          FMrt32 = x1Elpf;
          FMrt33 = x1Tax;
          FMrt34 = x1Taxf;
          FMrt35 = x1Rdf1;
          FMrt36 = x1Rdf2;
          FMrt37 = x1Rdf3;

          %Occur(fact) = 5;
          FMcd41 = x1St;
          FMrt41 = x1Elp;
          FMrt42 = x1Elpf;
          FMrt43 = x1Tax;
          FMrt44 = x1Taxf;
          FMrt45 = x1Rdf1;
          FMrt46 = x1Rdf2;
          FMrt47 = x1Rdf3;
       Endsr;

      /End-Free
