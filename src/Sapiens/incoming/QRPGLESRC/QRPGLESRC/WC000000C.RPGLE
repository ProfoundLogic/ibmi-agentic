     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     h DFTACTGRP(*no) ACTGRP(*CALLER)
      //********************************************************************
      /TITLE WC000000C - Create endorsement WC000000.C
      //********************************************************************
      // Created: 09/09/14    Programmer: C:302   Project: WC2304          *
      //********************************************************************
     FWC000000C o    e             printer oflind(*in98)
     Fstopp     if   e             disk
     Fwdf2l     if   e           k disk
     Fstopl5    if   e           k disk    rename(pop01:pop05)
     Fstopl6    if   e           k disk    rename(pop01:pop06)

     D Main            pr                  ExtPgm('WC000000C')
     D    Company                     3
     D    Fund                        3

     D Main            pi
     D    qqCo#                       3
     D    qqFnd                       3

     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d    ClientId                    3
     d

     D fids01        e ds                  extname(szq4p)
     D psds          esds                  extname(szq1p)

      // Work variables

     D xcplb           c                   const('*LIBL')
     D xcvrz           c                   const(00.200)
     D xchrz           c                   const(00.200)
     D xxpag           s              8a   INZ('WCC000P')
     D Y               s              1s 0
     D First           s              1    Inz('Y')

     d    @@ovps       s              8
     d    @@lib        s             10
     d    @@vpos       s              5  3
     d    @@hpos       s              5  3
     d    @@wide       s              5  3
     d    @@high       s              5  3
     d    @@rota       s              3  0
     d    @@rtcd       s              1

     D*--------------------------------------------------
     D* Procedure name: LoadPageSegment
     D* Purpose:        Load page segment
     D* Returns:
     D*--------------------------------------------------
     DLoadPageSegment  pr

     D*--------------------------------------------------
     D* Procedure name: PrintForm
     D* Purpose:        Print form from file settings
     D* Returns:
     D*--------------------------------------------------
     D PrintForm       PR


      //*****************************************************************
      // Main Line program
      //*****************************************************************

      /Free

       // Get the client id
       GetClientID(opclt#);

       OPco# = %Dec(qqCo#:3:0);
       OPFnd = %Dec(qqFnd:3:0);
       OPForm = 'WC000000C';

       //  Try by group
       Setll (opclt#:OPco#:OPform:OPfnd) stopl5;
       Reade (opclt#:OPco#:OPform:OPfnd) stopl5;

       If %EOF;

          // Try by Carrier
          chain (OPco#:OPFnd) wdf2l;
          OPFnd  = *Zeros;
          Setll (opclt#:OPco#:OPform:F2Car#) stopl6;
          Reade (opclt#:OPco#:OPform:F2Car#) stopl6;

          If %EOF;

             //  Use defaults
             For Y = 1 to 6;

                xpNam = %trim(xxpag) + %Trim(%Editc(Y:'X'));
                xpplb = xcplb;
                xpvrz = xcvrz;
                xphrz = xchrz;

                write wc0000001;

                If Y < 6;
                   write newPage;
                Endif;

             EndFor;

          Else;

            Dou %EOF;
               PrintForm();
               Reade (opclt#:OPco#:OPform:F2Car#) stopl6;
            EndDo;

          EndIf;

       Else;

         Dou %EOF;
            PrintForm();
            Reade (opclt#:OPco#:OPform:OPfnd) stopl5;
         EndDo;

       Endif;

       *inlr = *on;

      /End-Free

     P*--------------------------------------------------
     P* Procedure name: PrintForm
     P* Purpose:        Print form from file settings
     P* Returns:
     P*--------------------------------------------------
     P PrintForm       B
     D PrintForm       PI

      /Free

            xpNam = OPovps;
            xpplb = OPlib;
            xpvrz = OPvpos;
            xphrz = OPhpos;

            If xpplb = *Blanks;
               xpplb = xcplb;
            Endif;

            If First = 'Y';
               First = 'N';
               write wc0000001;
            Else;
               write newPage;
               write wc0000001;
            EndIf;

      /End-Free

     P PrintForm       E
