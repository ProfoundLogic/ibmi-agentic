     Hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H*****************************************************************
     H* This program will be used to calculate Medicare Allowances    *
     H* for physician bills.                                          *
     H*****************************************************************
     H*  CREATED BY:                                                  *
     H*  06/21/12  C:277 Project: 114MFI                              *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  10/13/25  C:333 Project 114MIB                               *
     H*                  New 2025 OWCP USH&L Fee Schedules            *
     H*                                                               *
     H*  07/18/25  C:342 Project MC451                                *
     H*                  NCCI Med Data Call Enhancements              *
     H*                                                               *
     H*  07/15/24  C:342/079 Project: FS688                           *
     H*                  2024 USH&L Fee Schedule                      *
     H*                                                               *
     H*  09/14/22  C:333 Project: FS648                               *
     H*                  FS648 : 2022 USH&L FS                        *
     H*                                                               *
     H*  11/05/18  C:079 Project: FS547                               *
     H*                  Pay ASC using office allowance               *
     H*                                                               *
     H*  09/16/18  C:212 Project: FS473                               *
     H*                  Corrected to use hlcf or rtconv based on hlcf*
     H*                                                               *
     H*  11/09/13  C:296 Project: FS433                               *
     H*                  Corrected the logic when hlcf <> 0          *
     H*                                                               *
     H*  02/12/13  C:279 Project: SY266 Transform                     *
     H*                  Corrected condition for status check.        *
     H*                                                               *
     H*  10/30/12  C:280 Support: 80935                               *
     H*                  change how modifier is used                  *
     H*                                                               *
     H*****************************************************************
     Ffdcap     if   e             disk
     Ffaeolg    if   e           k disk
     Ffddhl     if   e           k disk
     Ffddul     if   e           k disk
     Ffmzrl     if   e           k disk    usropn
     Fftlbl1    if   e           k disk
     Ffmrcl     if   e           k disk
     Fftgpl3    if   e           k disk
     Ffmdel1    if   e           k disk
     Ffmdfl1    if   e           k disk
     Ffmaml1    if   e           k disk
     Fftuml     if   e           k disk
     Fftrtl2    if   e           k disk
     Ffmhll1    if   e           k disk
     Ffthal2    if   e           k disk
     Ffmp1l     if   e           k disk
     Ffmcml1    if   e           k disk
     Fststl     if   e           k disk
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
     Dqca              s             56    dim(2) ctdata perrcd(1)
     Dmod              s              2    dim(4)
     D*
     DRunCommand       pr                  extPgm('QCMDEXC')
     D inCmd                         56    const
     D inCmdLen                      15  5 const
     D kkzip           s                   like(zrfzcd)
     D kkefdt          s                   like(zrefdt)
     D k1efdt          s                   like(zrefdt)
     D kkloc           s                   like(zrrgn#)
     D kkmod           s                   like(dhmod1)
     D kkst            s                   like(eoast)
     D kkcpt4          s                   like(cacpt4)
     D kkfein          s                   like(eofein)
     D kkmod1          s                   like(eomod)
     D kkkey           s                   like(stkey)
     D inCmd           s             56
     D inCmdLen        s             15  5
     D xxIdx           s              3  0
     D*
     D @@rrn           s              9  0
     D @@call          s              1
     D @@eob           s              3
     D @@msg           s              7
     D @@modr          s              1
     D @@rnd           s              1
     D @@alw1          s              9  2
     D @@alw2          s              9  2
     D @@alw3          s              9  2
     D @@rtcd          s              1
     D xxtype          s                   like(rttype)
     D xxst            s                   like(rtst)
     D x1st            s                   like(rtst)
     D zzst            s                   like(rtst) inz('HL')
     D*
     D*** Beg Chg 10/30/12 ***********************
     D*
     D Found           s              1
     D*
     D*** End Chg 10/30/12 ***********************
     D***** Beg Add *** 09/16/15 *******************************************
     D xxconv          s                   like(rtconv) inz
     D***** End Add *** 09/16/15 *******************************************
     D*
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
     *
     C     zrky          klist
     C                   kfld                    caco#
     C                   kfld                    kkzip
     C                   kfld                    kkefdt
     *
     C     hlky1         klist
     C                   kfld                    kkcpt4
     C                   kfld                    kkmod
     C                   kfld                    kkefdt
     *
     C     hlky2         klist
     C                   kfld                    kkcpt4
     C                   kfld                    kkmod
     *
     C     haky1         klist
     C                   kfld                    kkzip
     C                   kfld                    kkefdt
     *
     C     haky2         klist
     C                   kfld                    kkzip
     *
     C     rcky1         klist
     C                   kfld                    caco#
     C                   kfld                    cazipa
     C                   kfld                    cacpt4
     C                   kfld                    kkefdt
     *
     C     rcky2         klist
     C                   kfld                    caco#
     C                   kfld                    cazipa
     C                   kfld                    cacpt4
     *
     C     gpky1         klist
     C                   kfld                    kkst
     C                   kfld                    kkloc
     C                   kfld                    kkefdt
     *
     C     gpky2         klist
     C                   kfld                    kkst
     C                   kfld                    kkloc
     *
     C     rtky1         klist
     C                   kfld                    zzst
     C                   kfld                    xxtype
     C                   kfld                    cafdat
     *
     C     rtky2         klist
     C                   kfld                    zzst
     C                   kfld                    xxtype
     *
     C
     C     cmky          klist
     C                   kfld                    eoco#
     C                   Kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     *
     C     stky          klist
     C                   kfld                    kkkey
     *
     C     p1ky          klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod1
      /Free
         //Main Processing
         //Set the pointer for the FDCAP from the RRN
       @@rtcd = *blanks;
       Chain @@rrn pca01;
         If not %Found(fdcap);
           @@rtcd = 'F';

            *inlr = *on;
           Return;

         Else;

         Chain eoky peo01;
           If %found(faeolg);

         //Retrieve the claim adjudication state from FMCMP
             Chain cmky pcm01;
             If %Found(fmcml1);

         //Check this state is found in State table or not
               kkkey = eoast;
               Chain stky pst01;
               If %Found(ststl);
                 xxst = cmast;
                 x1st = cmast;
               Endif;

             Else;

         //Retrive state from file FMP1P
             kkfein = eofein;
             kkmod1 = eomod;
             Chain p1ky pp101;
               If %Found(FMP1L);
                 xxst = p1st;
               Endif;
             Endif;

         //If state is PA the use P1 as the state so the file will be FMZRLP1
           If xxst = 'PA';
             x1st = 'P1';
           Endif;

         //Open up FMZRPxx to calculate the locality
           qqcmd = qca(1);
           qqst  = x1st;
           inCmd = qqcmd;
           inCmdLen = 56;

           callp RunCommand(inCmd:inCmdLen);

           open fmzrl;
           kkloc = 99;
           kkefdt = cafdat;
           qqst = xxst;
           xxtype = 'M';

           Setgt rtky1 prt01;
           Readpe rtky2 prt01;

             Dow not %Eof(FTRTL2);
               If rtstat = 'I';
                 Readpe rtky2 prt01;
               Else;
                 Leave;
               Endif;
             Enddo;

           If rtmrel = '1';
             kkefdt = rtefdt;
           Else;
             kkefdt = cafdat;
           Endif;

           //*** Beg Chg ***** 07/18/25 *************************************
           //*** kkzip = %int(eoflic);
           kkzip = %int(%subst(eoflic:1:5));
           //*** End Chg ***** 07/18/25 *************************************
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

           Select;

           When eobtyp = 'N';
           Chain dhky  pdh01;

           When eobtyp = 'H';
           Chain dhky pdu01;

           Endsl;

       //Initialising
           @@alw1 = 0;
           @@alw2 = 0;
           @@alw3 = 0;
           mod = *blanks;

       //Calling of subroutine Anes and Phy

           Select;
           When cacpt4 >= '00100' and cacpt4 <= '01999' and
                %subst(cacpt4:5:1) <> 'F' and
                %subst(cacpt4:5:1) <> 'T'
                or cacpt4 = '99100' or cacpt4 = '99116'
                or cacpt4 = '99135' or cacpt4 = '99140';
           Exsr anes;
           @@eob = '395';

           When (cacpt4 >= 'A0001' and cacpt4 <= 'Z9999')
                or (cacpt4 >= '10000' and cacpt4 <= '99999')
                or (cacpt4 >= 'AAA00' and cacpt4 <= 'ZZZ99')
                or %subst(cacpt4:5:1) =  'F'  or
       //**** Beg Chg *** 09/14/22 *********************************
                //*** %subst(cacpt4:5:1) = 'T';
       //**** Beg Chg *** 10/13/25 *********************************
                //*** %subst(cacpt4:5:1) = 'T' or %subst(cacpt4:5:1) = 'A';
                 %subst(cacpt4:5:1) = 'T' or %subst(cacpt4:5:1) = 'A'
                 or %subst(cacpt4:5:1) = 'U';
       //**** End Chg *** 10/13/25 *********************************
       //**** End Chg *** 09/14/22 *********************************

           Exsr phy;

           Endsl;

         Endif;

       //Closing the FMZRLxx
       qqcmd = *blanks;
       qqcmd = qca(2);
       inCmd = qqcmd;
       inCmdLen = 56;

         callp RunCommand(inCmd:inCmdLen);

       Close FMZRL;

       //Program End here

       Endif;

       *inlr = *on;
       Return;
    
       //Anes - Calculating Amesthesia amount
    
       Begsr Anes;

       Select;

       When Eobtyp = 'N';
       mod(1) = dhmod1;
       mod(2) = dhmod2;
       mod(3) = dhmod3;
       mod(4) = dhmod4;

       When Eobtyp = 'H';
       mod(1) = dupmod;
       mod(2) = dumod2;
       mod(3) = dumod3;
       mod(4) = dumod4;
       EndSl;

       kkmod = *blanks;
       kkcpt4 = cacpt4;

       //Read from FMHLL1

       Setgt hlky1  phl01;
       Readpe hlky2 phl01;
         Dow not %Eof(fmhll1);
           If hlstat = 'I';
             Readpe hlky2 phl01;
           Else;
             //***** Beg Chg *** 02/12/13 ***********************
             // If hlstat = 'A';
             // leave;
             // Endif;
             leave;
             //***** End Chg *** 02/12/13 ***********************
           Endif;
         Enddo;

       If not %Eof(FMHLL1);

       //Rread from FTHAL2 to find out the conversion factor
         Setgt haky1 pha01;
         Readpe haky2 pha01;
           Dow not %Eof(fthal2);
             If hastat = 'I';
               Readpe haky2 pha01;
             Else;
               If hastat = 'A';
                 leave;
               EndIf;
             EndIf;
           Enddo;

       If not %Eof(fthal2);

         @@alw1 = hacf;
         @@alw2 = hlwkrv;

       Endif;                                                                       //FTHAL2
       Endif;                                                                       //FMHLL1

       Endsr;

       //Phy  - Calculating Physician amount
       Begsr Phy;

       @@eob = '395';

       Select;

       When Eobtyp = 'N';
       mod(1) = dhmod1;
       mod(2) = dhmod2;
       mod(3) = dhmod3;
       mod(4) = dhmod4;

       When Eobtyp = 'H';
       mod(1) = dupmod;
       mod(2) = dumod2;
       mod(3) = dumod3;
       mod(4) = dumod4;
       EndSl;

       kkmod = *blanks;
       kkst = xxst;
       kkcpt4 = cacpt4;

       //find gpci's involed in final calculations

       Setgt gpky1 pgp01;
       Readpe gpky2 pgp01;
         Dow not %Eof(ftgpl3);
           If gpstat = 'I';
             Readpe gpky2 pgp01;
           Else;
             If gpstat = 'A';
               leave;
             Endif;
           Endif;
         Enddo;

       If not %Eof(ftgpl3) ;

       //Read from FMHLL1
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
            kkmod = '81' or kkmod = '82'
            or kkmod = '25'
            or eospcd= 'ASC';
           kkmod = *blanks;
         Endif;

       Endif;

       //*** Beg Chg 10/30/12 ***********************

       Eval Found  = *off;

       Dou Found = *On;

       //*** End Chg 10/30/12 ***********************

       Setgt hlky1 phl01;
       Readpe hlky2 phl01;
         Dow not %Eof(fmhll1);
           If hlstat = 'I';
             Readpe hlky2 phl01;
           Else;
             //***** Beg Del *** 02/12/13 ***********************
             // If hlstat= 'A';
             //***** End Del *** 02/12/13 ***********************
       //*** Beg Chg 10/30/12 ***********************
                 Eval Found  = *on;
       //*** End Chg 10/30/12 ***********************
               leave;
             //***** Beg Del *** 02/12/13 ***********************
             // Endif;
             //***** End Del *** 02/12/13 ***********************
           Endif;
         Enddo;

       //*** Beg Chg 10/30/12 ***********************

         If Found = *off or %eof(fmhll1);
            If kkmod = 'NU' or kkmod = 'RR' or kkmod = 'UE';
               If kkcpt4 >= 'A0001' and kkcpt4 <= 'Z9999';
                  kkmod = *blanks;
               else;
                  Leave;
               Endif;
            else;
               Leave;
            Endif;
         Endif;

       EndDo;

       //*** End Chg 10/30/12 ***********************

       If not %Eof(fmhll1);

       Select;

       When eobtyp = 'N';

         Select;
       //*** Beg Chg *** 11/05/18 ****************************************
       //***When dhpos =  ' 11';
         When dhpos =  ' 11' or (DHpos <> ' 11' and EOspCd = 'ASC' and
                     CAfDat >= 1181015);
       //*** End Chg *** 11/05/18 ****************************************
       //*** BEG CHG 11/09/13 ***********************
       //If hlnfpe = 0 and hlfape = 0 and hlmlrv = 0;
         //@@alw1 = hlwkrv;
         If hlnfpe = 0 and hlfape = 0 and hlmlrv = 0 and hlcf = 0;
           @@alw1 = hlwkrv;
         ElseIf hlnfpe = 0 and hlfape = 0 and hlmlrv = 0 and hlcf <> 0;
           //*** Beg Chg *** 07/15/24 *********************************
           //***@@alw1 = hlwkrv*hlcf;
           @@alw1 = hlwkrv * GPwkGp * hlcf;
           //*** End Chg *** 07/15/24 *********************************
       //*** END CHG 11/09/13 ***********************
         Else;
       //**** Beg Add *** 09/16/15 ***************************************
           if HLcf <> 0;
             xxconv = HLcf;
           else;
             xxconv = RTconv;
           endif;

       //**** End Add *** 09/16/15 ***************************************
       //**** Beg Chg *** 09/16/15 ***************************************

           @@alw1 = ((hlwkrv * gpwkgp) +
                    (hlnfpe * gppegp) +
                    (hlmlrv * gpmpgp)) * xxconv;

         //  @@alw1 = ((hlwkrv * gpwkgp) +
         //           (hlnfpe * gppegp) +
         //           (hlmlrv * gpmpgp)) * rtconv;
       //**** End Chg *** 09/16/15 ***************************************
         Endif;

         When dhpos <> ' 11';
       //*** BEG CHG 11/09/13 ***********************
       //If hlnfpe = 0 and hlfape = 0 and hlmlrv = 0;
         //@@alw1 = hlwkrv;
         If hlnfpe = 0 and hlfape = 0 and hlmlrv = 0 and hlcf = 0;
           @@alw1 = hlwkrv;
         ElseIf hlnfpe = 0 and hlfape = 0 and hlmlrv = 0 and hlcf <> 0;
           //*** Beg Chg *** 07/15/24 *********************************
           //***@@alw1 = hlwkrv*hlcf;
           @@alw1 = hlwkrv * GPwkGp * hlcf;
           //*** End Chg *** 07/15/24 *********************************
       //*** END CHG 11/09/13 ***********************
         Else;
       //**** Beg Add *** 09/16/15 ***************************************
           if HLcf <> 0;
             xxconv = HLcf;
           else;
             xxconv = RTconv;
           endif;

       //**** End Add *** 09/16/15 ***************************************
       //**** Beg Chg *** 09/16/15 ***************************************

           @@alw1 = ((hlwkrv * gpwkgp) +
                    (hlfape * gppegp) +
                    (hlmlrv * gpmpgp)) * xxconv;

           // @@alw1 = ((hlwkrv * gpwkgp) +
           //         (hlfape * gppegp) +
           //         (hlmlrv * gpmpgp)) * rtconv;
       //**** End Chg *** 09/16/15 ***************************************
         Endif;
         Endsl;

       When eobtyp = 'H';

         Select;
         When dupos =  ' 11';
       //*** BEG CHG 11/09/13 ***********************
       //If hlnfpe = 0 and hlfape = 0 and hlmlrv = 0;
         //@@alw1 = hlwkrv;
         If hlnfpe = 0 and hlfape = 0 and hlmlrv = 0 and hlcf = 0;
           @@alw1 = hlwkrv;
         ElseIf hlnfpe = 0 and hlfape = 0 and hlmlrv = 0 and hlcf <> 0;
           //*** Beg Chg *** 07/15/24 ********************************
           //***@@alw1 = hlwkrv*hlcf;
           @@alw1 = hlwkrv * GPwkGp * hlcf;
           //*** End Chg *** 07/15/24 *********************************
       //*** END CHG 11/09/13 ***********************
         Else;
       //**** Beg Add *** 09/16/15 ***************************************
           if HLcf <> 0;
             xxconv = HLcf;
           else;
             xxconv = RTconv;
           endif;

       //**** End Add *** 09/16/15 ***************************************
       //**** Beg Chg *** 09/16/15 ***************************************

           @@alw1 = ((hlwkrv * gpwkgp) +
                    (hlnfpe * gppegp) +
                    (hlmlrv * gpmpgp)) * xxconv;

           // @@alw1 = ((hlwkrv * gpwkgp) +
           //         (hlnfpe * gppegp) +
           //         (hlmlrv * gpmpgp)) * rtconv;
       //**** End Chg *** 09/16/15 ***************************************
         Endif;

         When dupos <> ' 11';
       //*** BEG CHG 11/09/13 ***********************
       //If hlnfpe = 0 and hlfape = 0 and hlmlrv = 0;
         //@@alw1 = hlwkrv;
         If hlnfpe = 0 and hlfape = 0 and hlmlrv = 0 and hlcf = 0;
           @@alw1 = hlwkrv;
         ElseIf hlnfpe = 0 and hlfape = 0 and hlmlrv = 0 and hlcf <> 0;
           //*** Beg Chg *** 07/15/24 *********************************
           //***@@alw1 = hlwkrv*hlcf;
           @@alw1 = hlwkrv * GPwkGp * hlcf;
           //*** End Chg *** 07/15/24 *********************************
       //*** END CHG 11/09/13 ***********************
         Else;
       //**** Beg Add *** 09/16/15 ***************************************
           if HLcf <> 0;
             xxconv = HLcf;
           else;
             xxconv = RTconv;
           endif;

       //**** End Add *** 09/16/15 ***************************************
       //**** Beg Chg *** 09/16/15 ***************************************

            @@alw1 = ((hlwkrv * gpwkgp) +
                    (hlfape * gppegp) +
                    (hlmlrv * gpmpgp)) * xxconv;

           // @@alw1 = ((hlwkrv * gpwkgp) +
           //          (hlfape * gppegp) +
           //          (hlmlrv * gpmpgp)) * rtconv;
       //**** End Chg *** 09/16/15 ***************************************
         Endif;
         Endsl;

       Endsl;
       Endif;

       Endif;

       Endsr;
     /End-Free
**  TABLE QCA
OVRDBF FILE(FMZRL)   TOFILE(*LIBL/FMZRL  )
DLTOVR FILE(FMZRL)
