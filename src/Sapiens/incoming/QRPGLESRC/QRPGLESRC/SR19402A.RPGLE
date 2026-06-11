     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) BndDir('WTBNDDIR')
     H/TITLE SR19402A - MD Alcohol-And-Drug-Free Workplace Premium Credit
      //===============================================================*
      // WC190402A                                                     *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      // Created   : 08/25/17                                          *
      // Programmer: C:180                                             *
      // Project   : WC2518                                            *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 10/19/22 C: 342 Project: WC2834                               *
      //                 Endorsement Recognition of Cancellation       *
      //                                                               *
      //                                                               *
      //                                                               *
      //
      //
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
     fwdrrl     if   e           k disk

      //* Copybook F specs
     f/copy scopyen01f

      //* Copybook D specs
     d/copy scopyen01d
      //***** Beg Del *** 10/19/22 **********************************
     d*****/copy wPrEmplr
      //***** End Del *** 10/19/22 **********************************


     d SR19402A        PR
     d                                3
     d                                3
     d                                9
     d                                5
     d                                3
     d                                1
     d                                9  0
     d                                1

     d SR19402A        PI
     d  qqCo#                         3
     d  qqFnd                         3
     d  qqEmp#                        9
     d  qqDiv                         5
     d  qqFyr                         3
     d  qqMode                        1
     d  qqRrn                         9  0
     d  qqMod2                        1
    
     D StrDiv          s                   Like(ELdiv) Inz
     D StrFlg          s              1    Inz
     D EMrrn           s              9  0 Inz
     d qqFORM          s             15a   inz('WC190402A')
     d xxRatSt         s              2    inz
     d intPremCR       s              5  2 inz
     d intStRptDiv     s              5  0 inz
     d intStRptEffDt   s              7  0 inz

      //* Additional entry program
     d addpgm          c                   const('SRXXXXXXX ')

      //=============================================================
      //  Main
      //=============================================================

      //* Copybook C specs (KLISTS and mainline)
     c/copy scopyen01c

      //=============================================================
      //  OTHINF - Subroutine to load form specific information
      //=============================================================
      /Free
       Begsr Othinf;

       StrDiv = GetStRptDv(xxCo#:xxfnd:xxemp#:emrrn:StrFlg);
       Chain (xxCo#:xxfnd:xxfyr:xxemp#:StRDiv) wdell1;
       intStRptEffdt = ELpfdt;

       Setll (xxCo#:xxfnd:xxfyr:xxemp#) wdell1;
       Reade (xxCo#:xxfnd:xxfyr:xxemp#) wdell1;
       Dow not(%eof);
          //***** Beg Add *** 10/19/22 ******************************
          xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
          //***** End Add *** 10/19/22 ******************************
          xxRatSt = GetRatingState(xxco#:xxfnd:xxemp#:ELdiv);
          Setgt (ELco#:ELfnd:xxRatSt:intStRptEffdt) prr01;
          Readpe (ELco#:ELfnd:xxRatSt) prr01;
          If not %eof;
             IntPremCR = *Zeros;
             Select;
             When RRcccd = 9846 and ELasp1 <> *Zero;  //Contractor's CR
                IntPremCR = ELasp1;
             When RRdv1c = 9846 and ELasp2 <> *Zero;  //Dev1
                IntPremCR = ELasp2;
             When RRdv2c = 9846 and ELdev2 <> *Zero;  //Dev2
                IntPremCR = ELdev2;
             When RR1acd = 9846 and ELaspm <> *Zero;  //Subj1
                IntPremCR = ELaspm;
             When RR1bcd = 9846 and ELasps <> *Zero;  //Subj2
                IntPremCR = ELasps;
             Endsl;

          // Populate schedule fields

             If IntPremCR <> *Zeros;
                FMa521 = IntPremCR;
                Exsr WriteWorkFile;
             Endif;

          Endif;
          //***** Beg Add *** 10/19/22 ******************************
          Endif;
          //***** End Add *** 10/19/22 ******************************
          Reade (xxCo#:xxfnd:xxfyr:xxEmp#) wdell1;
       Enddo;

       Endsr;
       //============================================================
       //  PGMSTR - Beginning of program processing
       //============================================================
       Begsr Pgmstr;
       WriteInCopyBook = *off;
       Endsr;
       //============================================================
       //  PGMEND - End of program processing
       //============================================================
       Begsr Pgmend;
       Endsr;
       //============================================================
       //  VALID - Subroutine to attach this form to the policy
       //============================================================
       Begsr Valid;
        Setgt (ELco#:ELfnd:RSjrst:ELpfdt) prr01;
        Readpe (ELco#:ELfnd:RSjrst) prr01;
        If Not(%Eof);
           Select;
           When RRcccd = 9846 and ELasp1 <> *Zero;  //Contractor's Credit
              crtFrm = *on;
           When RRdv1c = 9846 and ELasp2 <> *Zero;  //Dev1
              If ELdvef > 0 and ELdvex > 0;
                 If ELdvef >=ELpfdt and ELdvef < ELptdt and ELdvex > Sysdat;
                    CrtFrm = *on;
                 Endif;
              Endif;

              If ELdvef > 0 and ELdvex = 0;
                 If ELdvef >=ELpfdt and ELdvef < ELptdt;
                    CrtFrm = *on;
                 Endif;
              Endif;

              If ELdvef = 0 and ELdvex > 0;
                 If ELdvex > Sysdat;
                    CrtFrm = *on;
                 Endif;
              Endif;

           When RRdv2c = 9846 and ELdev2 <> *Zero;  //Dev2
              If ELd2ef > 0 and ELd2ex > 0;
                 If ELd2ef >=ELpfdt and ELd2ef < ELptdt and ELd2ex > Sysdat;
                    CrtFrm = *on;
                 Endif;
              Endif;

              If ELd2ef > 0 and ELd2ex = 0;
                 If ELd2ef >=ELpfdt and ELd2ef < ELptdt;
                    CrtFrm = *on;
                 Endif;
              Endif;

              If ELd2ef = 0 and ELd2ex > 0;
                 If ELd2ex > Sysdat;
                    CrtFrm = *on;
                 Endif;
              Endif;
           When RR1acd = 9846 and ELaspm <> *Zero;  //Subj1
              If ELmcef > 0 and ELmcex > 0;
                 If ELmcef >=ELpfdt and ELmcef < ELptdt and ELmcex > Sysdat;
                    CrtFrm = *on;
                 Endif;
              Endif;

              If ELmcef > 0 and ELmcex = 0;
                 If ELmcef >=ELpfdt and ELmcef < ELptdt;
                    CrtFrm = *on;
                 Endif;
              Endif;

              If ELmcef = 0 and ELmcex > 0;
                 If ELmcex > Sysdat;
                    CrtFrm = *on;
                 Endif;
              Endif;
           When RR1bcd = 9846 and ELasps <> *Zero;  //Subj2
              If ELscef > 0 and ELscex > 0;
                 If ELscef >=ELpfdt and ELscef < ELptdt and ELscex > Sysdat;
                    CrtFrm = *on;
                 Endif;
              Endif;

              If ELscef > 0 and ELscex = 0;
                 If ELscef >=ELpfdt and ELscef < ELptdt;
                    CrtFrm = *on;
                 Endif;
              Endif;

              If ELscef = 0 and ELscex > 0;
                 If ELscex > Sysdat;
                    CrtFrm = *on;
                 Endif;
              Endif;
           Endsl;
        Endif;
       Endsr;
      /End-free
