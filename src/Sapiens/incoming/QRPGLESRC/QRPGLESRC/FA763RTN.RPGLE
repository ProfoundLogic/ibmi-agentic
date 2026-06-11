     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*****************************************************************
     H* This program will be used to calculate Medicare Allowances    *
     H* for physician bills.                                          *
     H*****************************************************************
     H*  CREATED BY:                                                  *
     H*  07/26/12  C:277 Project: TN016                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     Ffdcap     if   e             disk
     Ffaeolg    if   e           k disk
     Ffddhl     if   e           k disk
     Ffddul     if   e           k disk
     Ffmzrl     if   e           k disk    usropn
     Ffmacl1    if   e           k disk
     Fftlbl1    if   e           k disk
     Fftrvl1    if   e           k disk
     Ffmrcl     if   e           k disk
     Fftgpl3    if   e           k disk
     Ffmdel1    if   e           k disk
     Ffmdfl1    if   e           k disk
     Ffmaml1    if   e           k disk
     Fftuml     if   e           k disk
     Fftrtl2    if   e           k disk
     F*
     D psds          esds                  extname(szq1p)
     *
     D                 ds
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0
     D                 ds
     D  qqcmd                  1     56
     D  qqst                  40     41
     D*
     DRunCommand       pr                  extPgm('QCMDEXC')
     D inCmd                         56    const
     D inCmdLen                      15  5 const
     D*
     D qca             s             56    dim(2) ctdata perrcd(1)
     D mod             s              2    dim(4)
     D kkzip           s                   like(zrfzcd)
     D kkefdt          s                   like(zrefdt)
     D k1efdt          s                   like(zrefdt)
     D kkloc           s                   like(zrrgn#)
     D kkmod           s                   like(dhmod1)
     D kkst            s                   like(eoast)
     D kktype          s                   like(rttype)
     D kkcpt4          s                   like(cacpt4)
     D xxunts          s              5  1
     D xxamt1          s              9  2
     D xxamt2          s              9  2
     D xxdme           s              1    inz('N')
     D xxmod1          s              2
     D xxmod2          s              2
     D i               s              1  0
     D j               s              1  0
     D k               s              1  0
     D inCmd           s             56
     D inCmdLen        s             15  5
     D xxIdx           s              3  0
     D xxtech          s                   like(camra)
     D xxprof          s                   like(camra)
     D xxmd26          s                   like(umod26)
     D xxmd27          s                   like(umod27)
     D xxrate          s              9  2
     C*
     C     *entry        plist
     C                   parm                    @@rrn             9 0
     C                   parm                    @@call            1
     C                   parm                    @@eob             3
     C                   parm                    @@msg             7
     C                   parm                    @@modr            1
     C                   parm                    @@rnd             1
     C                   parm                    @@alw1            9 2
     C                   parm                    @@alw2            9 2
     C                   parm                    @@alw3            9 2
     C                   parm                    @@rtcd            1
     *
     C     dtcymd1       plist
     C                   parm                    srdti1            8 0
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *BLANKS       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     *
     C     eoky          klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C*
     C     dhky          klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C*
     C     zrky          klist
     C                   kfld                    caco#
     C                   kfld                    kkzip
     C                   kfld                    kkefdt
     C*
     C     rvky1         klist
     C                   kfld                    kkcpt4
     C                   kfld                    kkmod
     C                   kfld                    kkefdt
     C*
     C     rvky2         klist
     C                   kfld                    kkcpt4
     C                   kfld                    kkmod
     C*
     C     acky1         klist
     C                   kfld                    kkst
     C                   kfld                    kkloc
     C                   kfld                    kkefdt
     C*
     C     acky2         klist
     C                   kfld                    kkst
     C                   kfld                    kkloc
     C*
     C     rcky1         klist
     C                   kfld                    caco#
     C                   kfld                    cazipa
     C                   kfld                    cacpt4
     C                   kfld                    kkefdt
     c*
     C     rcky2         klist
     C                   kfld                    caco#
     C                   kfld                    cazipa
     C                   kfld                    cacpt4
     C*
     C     amky1         klist
     C                   kfld                    kkst
     C                   kfld                    kkloc
     C                   kfld                    cacpt4
     C                   kfld                    kkefdt
     c*
     C     amky2         klist
     C                   kfld                    kkst
     C                   kfld                    kkloc
     C                   kfld                    cacpt4
     C*
     C     deky1         klist
     C                   kfld                    cacpt4
     C                   kfld                    xxmod1
     C                   kfld                    xxmod2
     C                   kfld                    kkefdt
     c*
     C     deky2         klist
     C                   kfld                    cacpt4
     C                   kfld                    xxmod1
     C                   kfld                    xxmod2
     C*
     C     dfky1         klist
     C                   kfld                    cacpt4
     C                   kfld                    kkefdt
     c*
     C     dfky2         klist
     C                   kfld                    cacpt4
     c*
     C     gpky1         klist
     C                   kfld                    kkst
     C                   kfld                    kkloc
     C                   kfld                    kkefdt
     C*
     C     gpky2         klist
     C                   kfld                    kkst
     C                   kfld                    kkloc
     C*
     C     umky1         klist
     C                   kfld                    caco#
     C                   kfld                    cacpt4
     C                   kfld                    kkefdt
     C*
     C     umky2         klist
     C                   kfld                    caco#
     C                   kfld                    cacpt4
     C*
     C     rtky1         klist
     C                   kfld                    kkst
     C                   kfld                    kktype
     C                   kfld                    cafdat
     C*
     C     rtky2         klist
     C                   kfld                    kkst
     C                   kfld                    kktype
     C*
      /free
          //**********************************************************
          //   M A I N    P R O C E S S I N G
          //**********************************************************

          //Set the pointer for the FDCAP from the RRN

          @@rtcd = *blanks;
           Chain @@rrn pca01;
              If not %found(fdcap);
                 @@rtcd = 'F';
                 return;

              Else;

                 Chain eoky peo01;
                   If %found(faeolg);

           //open up FMZRPxx to calculate the locality
                      qqcmd = qca(1);
                      qqst  = eoast;
                      inCmd = qqcmd;
                      inCmdLen = 56;

                      callp RunCommand(inCmd:inCmdLen);

                      open fmzrl;
                      kkloc = 99;
                      kkefdt= cafdat;
                      kktype = 'M';
                      kkst  = eoast;

                      Setgt rtky1 prt01;
                      Readpe rtky2 prt01;

                         Dow not %Eof(ftrtl2);
                            If rtstat = 'I';
                               Readpe rtky2 prt01;
                            Else;
                               Leave;
                            Endif;
                         Enddo;

                      If not %Eof(ftrtl2) and rtmrel = '1';
                         kkefdt = rtefdt;
                      Else;
                         kkefdt = cafdat;
                      Endif;

                      Select;
                      When eobtyp = 'N';
                      Chain dhky pdh01;
                      When eobtyp = 'H';
                      Chain dhky pdu01;
                      Endsl;
          //Initialising
                      @@alw1 = 0;
                      @@alw2 = 0;
                      @@alw3 = 0;
                      xxamt1 = 0;
                      xxamt2 = 0;
                      mod = *blanks;
          //Anes
                      Select;

                      When cacpt4 >= '00100' and cacpt4 <= '01999' and
                           %subst(cacpt4:5:1) <> 'F' and
                           %subst(cacpt4:5:1) <> 'T'
                           and (@@call = 'A'
                           or casrc = 'R')
                           or cacpt4 = '99100'
                           or cacpt4 = '99116'
                           or cacpt4 = '99135'
                           or cacpt4 = '99140';
                      exsr anes;

          //Clinical Lab
                      When casrc = 'L';
                      exsr clab;

                      When casrc = 'F';
                      exsr drgf;

                      When casrc = 'A';
                      exsr amb;

                      When casrc = 'D';
                      exsr dme;

                      When casrc = 'R' and
                           ((cacpt4 >= 'A0001' and cacpt4 <= 'Z9999')
                           or (cacpt4 >= '10000' and cacpt4 <= '99999')
                           or %subst(cacpt4:5:1) =  'F'  or
                            %subst(cacpt4:5:1) = 'T');
                      exsr phy;

                      Endsl;

                   Endif;                                                                //FAEOL
          //Closing the FMZRLxx
                    qqcmd = qca(2);
                    inCmd = qqcmd;
                    inCmdLen = 56;

                    callp RunCommand(inCmd:inCmdLen);

                    Close fmzrl;

              Endif;                                                                       //FDCAP

              Return;
         //***************************************************************
         //  Anes - Calculating Amesthesia amount
         //***************************************************************
          Begsr Anes;

             Select;
             When eobtyp = 'N';
             mod(1) = dhmod1;
             mod(2) = dhmod2;
             mod(3) = dhmod3;
             mod(4) = dhmod4;

             When eobtyp = 'H';
             mod(1) = dupmod;
             mod(2) = dumod2;
             mod(3) = dumod3;
             mod(4) = dumod4;

             Endsl;

             kkmod = *blanks;

             Select;
             When mod(1) <> *blanks;
             kkmod = mod(1);
             When mod(2) <> *blanks;
             kkmod = mod(2);
             When mod(3) <> *blanks;
             kkmod = mod(3);
             When mod(4) <> *blanks;
             kkmod = mod(4);
             Endsl;

             kkcpt4 = cacpt4;
             kkst   = eoast;

             If kkmod = 'QX' or kkmod = 'QY' or
                kkmod = 'QK' or kkmod = 'AA' or
                kkmod = 'P1' or kkmod = 'P2' or
                kkmod = 'P3' or kkmod = 'P4' or
                kkmod = 'P5' or
                kkmod = 'QZ' or kkmod = 'AD';
                kkmod = *blanks;
             Endif;

          //read from ftrvl1
             Setgt rvky1 prv01;
             Readpe rvky2 prv01;
                Dow not %Eof(ftrtl2);
                   If rvstat = 'I';
                      Readpe rvky2 prv01;
                   Else;
                      Leave;
                   Endif;
                Enddo;

             @@alw2 = rvwkrv;

          Endsr;
         //***********************************************************
         //  Phy  - Calculating Physician amount
         //***********************************************************
          Begsr Phy;

             Select;
             When eobtyp = 'N';
             mod(1) = dhmod1;
             mod(2) = dhmod2;
             mod(3) = dhmod3;
             mod(4) = dhmod4;

             When eobtyp = 'H';
             mod(1) = dupmod;
             mod(2) = dumod2;
             mod(3) = dumod3;
             mod(4) = dumod4;

             Endsl;
             kkmod  = *blanks;
             kkcpt4 = cacpt4;
             kkst   = eoast;
          //to find gpci's involed in final calculations
             kkloc = 35;
             Setgt gpky1 pgp01;
             Readpe gpky2 pgp01;
                Dow not %Eof(ftgpl3);
                   If gpstat = 'I';
                      Readpe gpky2 pgp01;
                   Else;
                      Leave;
                   Endif;
                Enddo;

             If not %Eof(ftgpl3);
          //read from ftrvl1
          //look up into modifier
                xxIdx = %lookup('26':mod);
                If xxIdx <> 0;
                  kkmod = '26';
                Else ;
                  xxIdx = %lookup('53':mod);
                  If xxIdx <> 0;
                    kkmod = '53';
                  Else ;
                    xxIdx = %lookup('TC':mod);
                    If xxIdx <> 0;
                      kkmod = 'TC';
                    Endif;
                  Endif;
                Endif;

                If kkmod = *blanks;

                Select;
                When mod(1) <> *blanks;
                kkmod = mod(1);

                When mod(2) <> *blanks;
                kkmod = mod(2);

                When mod(3) <> *blanks;
                kkmod = mod(3);

                When mod(4) <> *blanks;
                kkmod = mod(4);
                EndSl;

                If kkmod = '50' or kkmod = '51' or
                   kkmod = '62' or kkmod = '80' or
                   kkmod = '81' or kkmod = '82';
                  kkmod = *blanks;
                Endif;

                Endif;                                                             //kkmod = ' '

                Setgt rvky1 prv01;
                Readpe rvky2 prv01;
                   Dow not %Eof(ftrvl1);
                      If rvstat = 'I';
                         Readpe rvky2 prv01;
                      Else;
                         Leave;
                      Endif;
                   Enddo;

                kktype = 'M';
                Setgt rtky1 prt01;
                Readpe rtky2 prt01;
                   Dow not %Eof(ftrtl2);
                      If rtstat = 'I';
                         Readpe rtky2 prt01;
                      Else;
                         Leave;
                      Endif;
                   Enddo;

                If not %Eof(ftrtl2);

                   Select;

                   When eobtyp = 'N';

                      Select;
                      When dhpos =  ' 11';
                         xxamt1 = ((rvwkrv * gpwkgp) +
                                  (rvtnpe * gppegp) +
                                  (rvmprv * gpmpgp)) * rtconv;
                      When dhpos <> ' 11';
                         xxamt1 = ((rvwkrv * gpwkgp) +
                                  (rvtfpe * gppegp) +
                                  (rvmprv * gpmpgp)) * rtconv;
                      Endsl;

                   When eobtyp = 'H';

                      Select;
                      When dupos =  ' 11';
                         xxamt1 = ((rvwkrv * gpwkgp) +
                                  (rvtnpe * gppegp) +
                                  (rvmprv * gpmpgp)) * rtconv;
                      When dupos <> ' 11';
                         xxamt1 = ((rvwkrv * gpwkgp) +
                                  (rvtfpe * gppegp) +
                                  (rvmprv * gpmpgp)) * rtconv;
                      Endsl;

                   Endsl;                                                          //eobty = 'N'/'H'


                   @@alw1 = xxamt1 * 1;

                Endif;

             Endif;                                                                  //ftgpl3

          Endsr;
          //****************************************************************
          //Clab - Calculating Clinical lab amount
          //****************************************************************
          Begsr Clab;

             Select;
             When eobtyp = 'N';
             mod(1) = dhmod1;
             mod(2) = dhmod2;
             mod(3) = dhmod3;
             mod(4) = dhmod4;

             mod(1) = dupmod;
             mod(2) = dumod2;
             mod(3) = dumod3;
             mod(4) = dumod4;
             Endsl;

             kkmod  = *blanks;
             kkcpt4 = cacpt4;
             kkst   = eoast;

             Select;
             When mod(1) <> *blanks;
             kkmod = mod(1);
             When mod(2) <> *blanks;
             kkmod = mod(2);
             When mod(3) <> *blanks;
             kkmod = mod(3);
             When mod(4) <> *blanks;
             kkmod = mod(4);
             Endsl;

           //read from ftlbl1
              Setgt rvky1 plb01;
              Readpe rvky2 plb01;
                 Dow not %Eof(ftlbl1);
                    If lbstat = 'I';
                       Readpe rvky2 prv01;
                    Else;
                       Leave;
                    Endif;
                 Enddo;

              If not %Eof(ftlbl1);

                 Select;
                 When kkst = 'AR';
                 xxamt1 = lbar;

                 When kkst = 'DC';
                 xxamt1 = lbdc;

                 When kkst = 'TN';
                 xxamt1 = lbtn;

                 When kkst = 'TX';
                 xxamt1 = lbtx;

                 When kkst = 'NJ';
                 xxamt1 = lbnj;

                 When kkst = 'WV';
                 xxamt1 = lbwv;

                 When kkst = 'PA';
                 xxamt1 = lbpa;
                 Endsl;

                 If @@alw1 = 0;
                    @@alw1 = xxamt1;
                 Endif;                                                                 //@@alw1 = 0

              Endif;                                                                    //FTLBL1

         Endsr;
         //**************************************************************
         //Amb  - Calculating Ambulance amount
         //**************************************************************
         Begsr Amb;

            kkst   = eoast;
            kkcpt4 = cacpt4;
           kkzip = %int(eoflic);
           Setgt zrky pzr01;
           Readp pzr01;
             Dow not %Eof(fmzrl);
               k1efdt = zrefdt;
               If kkzip >= zrfzcd and kkzip <= zrtzcd
                  and zrefdt = k1efdt;
                 kkloc = zrrgn#;
                 leave;
               Endif;
             Readp pzr01;
             Enddo;

            If kkst = 'KY' and kkloc = 99;
               kkloc = 0;
            Endif;
          //read from ftrvl1
            Setgt amky1 pam01;
            Readpe amky2 pam01;
               Dow not %Eof(fmaml1);
                  If amstat = 'I';
                     Readpe amky2 pam01;
                  Else;
                     Leave;
                  Endif;
               Enddo;

            If not %Eof(fmaml1);

               If cacpt4 = 'A0425';
                  Select;
                  When zrarea = 'U';
                  xxamt1 = amurbn * caunts;

                  When caunts <= 17 and zrarea = 'R'
                       and caunts >= 1;
                  xxamt1 = amrgrd * caunts;

                  When caunts > 17 and zrarea = 'R';
                  xxamt1=(amrurl*1.5*17) + (amrurl*(caunts-17));

                  When caunts > 17 and zrarea = 'S';                             //Super Rural>17mil
                     If amlqsr > 0;
                        xxrate = amlqsr;
                     Else;
                        xxrate = amrgrd;
                     Endif;
                  xxamt1=(amrgrd*17) + ((caunts-17)*xxrate);

                  When caunts <= 17 and zrarea = 'S';                           //Super Rural<=17mil
                     If amlqsr > 0;
                        xxrate = amlqsr;
                     Else;
                        xxrate = amrgrd;
                     Endif;
                  xxamt1 = caunts * xxrate;

                  When zrarea = *blanks;
                  xxamt1 = amurbn;

                  Endsl;

               Endif;                                                           //ground ambulan mil

               If cacpt4 >='A0430' and cacpt4 <= 'A0431'
                  or cacpt4 >= 'A0435' and cacpt4 <= 'A0436';

                  Select;
                  When zrarea = 'U';
                  xxamt1 = amurbn * caunts;

                  When zrarea = 'R';
                  xxamt1 = amurbn * 1.5 *caunts;

                  When zrarea = 'S';                                             //Super Rural
                     If amlqsr > 0;
                        xxrate = amlqsr;
                     Else;
                        xxrate = amurbn;
                     Endif;
                  xxamt1 = xxrate * 1.5 * caunts;

                  When zrarea = *blanks;
                  xxamt1 = amurbn;

                  Endsl;

               Endif;                                                           //air ambulance mil

               If cacpt4 >='A0426' and cacpt4 <= 'A0429';
                  Select;

                  When zrarea = 'U';
                  xxamt1 = amurbn * caunts;

                  When zrarea = 'R';
                  xxamt1 = amrurl * caunts;

                  When zrarea = 'S';
                  xxamt1 = amlqsr * caunts;

                  Endsl;
               Endif;                                                           //air ambulance mil

               If cacpt4 >='A0432' and cacpt4 <= 'A0434';
                  Select;

                  When zrarea = 'U';
                  xxamt1 = amurbn * caunts;

                  When zrarea = 'R';
                  xxamt1 = amrurl * caunts;

                  When zrarea = 'S';
                  xxamt1 = amlqsr * caunts;

                  Endsl;
               Endif;                                                           //air ambulance mil

               @@alw1 = xxamt1;

            Endif;                                                                    //FMAML1

         Endsr;
         //***********************************************************
        //Dme  - Calculating DMEPOS amount
        //************************************************************
         Begsr Dme;

            Select;

            When eobtyp = 'N';
            mod(1) = dhmod1;
            mod(2) = dhmod2;
            mod(3) = dhmod3;
            mod(4) = dhmod4;

            When eobtyp = 'H';
            mod(1) = dupmod;
            mod(2) = dumod2;
            mod(3) = dumod3;
            mod(4) = dumod4;
            Endsl;

            kkst   = eoast;
            kkcpt4 = cacpt4;
            xxdme = *blanks;
            i = 0;

               Dou i < 5;
                  i = i + 1;

                  If i  <  5;
                     Dow mod(i) = *blanks;
                         i = i + 1;
                         If i > 4;
                            Leave;
                         Endif;
                     Enddo;                                                       //mod(i) = ' '
                  Endif;                                                          //i < 5
                  If i >=5;
                     Leave;
                  Endif;                                                          //   i >= 5

                  j = 0;

                  Dow j <> 5 and j + 1 <> 5;
                      j = j + 1;

                      Dow mod(j) = *blanks;
                          j = j + 1;
                          If j > 4;
                             Leave;
                          Endif;
                      Enddo;

                      If j <> i;

                         If i <> 5  and j <> 5;

                            If mod(i) <> ' ' and mod(j) <>  ' ';
                               xxmod1 = mod(i);
                               xxmod2 = mod(j);

                               Setgt deky1 pde01;
                               Readpe deky2 pde01;
                                  If not %Eof(fmdel1);
                                     xxamt1 = detn;
                                     xxmod2 = *blanks;
                                     xxdme = 'Y';
                                     Leave;
                                  Endif;                                          //if *in99 = *off
                            Endif;                                                //mod(i) & mod(j

                         Endif;                                                   //i<>5 & j<>5

                      Endif;                                                      //j <> i

                  Enddo;                                                           //j <> 5

               Enddo;                                                             //dou

            If xxdme <> 'Y';
               xxmod1 = *blanks;
               Select;

               When mod(1) <> ' ' and mod(2) = ' ' and mod(3) = ' '
                    and mod(4) = ' ';
               xxmod1 = mod(1);

               When mod(1) = ' ' and mod(2) <> ' ' and mod(3) = ' '
                    and mod(4) = ' ';
               xxmod1 = mod(2);

               When mod(1) = ' ' and mod(2) = ' ' and mod(3) <> ' '
                    and mod(4) = ' ';
               xxmod1 = mod(3);

               When mod(1) = ' ' and mod(2) = ' ' and mod(3) = ' '
                    and mod(4) <> ' ';
               xxmod1 = mod(4);
               Endsl;
               xxmod2 = *blanks;
               Setgt deky1 pde01;
               Readpe deky2 pde01;
                  If not %Eof(fmdel1);
                     xxamt1 = detn;
                     xxdme = 'Y';
                  Endif;                                                        //if fmdel1
            Endif;                                                              //if xxdme <> 'Y'

            @@alw1 = xxamt1;

         Endsr;
        //**************************************************************
        //Drgf - Calculating Drug fee amount
        //**************************************************************
         Begsr Drgf;

            kkcpt4 = cacpt4;
           //** read from ftrvl1
            Setgt dfky1 pdf01;
            Readpe dfky2 pdf01;
               Dow not %Eof(fmdfl1);
                  If dfstat = 'I';
                     Readpe dfky2 pdf01;
                  Else;
                     Leave;
                  Endif;
               Enddo;

            If not %Eof(fmdfl1);

               xxamt1 = dfrate;
               @@alw1 = xxamt1;

            Endif;                                                              //FTLBL1\

         Endsr;
      /End-free
**  TABLE QCA
OVRDBF FILE(FMZRL)   TOFILE(*LIBL/FMZRL  )
DLTOVR FILE(FMZRL)
