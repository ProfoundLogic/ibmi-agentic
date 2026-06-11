     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H*  FA784R - API to Retrieve FL Fee Schedule Allowance           *
     H*           This program will be used to Retrieve FL            *
     H*           Fee Schedule Allowance.                             *
     H*                                                               *
     H*  Called From: N96DVR                                          *
     H*                                                               *
     H*  Calls To:    None                                            *
     H*                                                               *
     H*****************************************************************
     H* Created on 04/30/2013 by C:277   for  Project 147MAS          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     Ffmprlfl   if   e           k disk
     F*
     Ffaeol31   if   e           k disk
     F*
     Ffmzrlfl   if   e           k disk
     F*
     Ffaehl     if   e           k disk
     F*
     Ffddhl     if   e           k disk
     F*
     D psds          esds                  extname(szq1p)
     *
     D                 ds
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0
     D*
     D @@eob#          s              9  0
     D @@code          s              5
     D @@mod           s              2
     D @@efdt          s              7  0
     D @@st            s              2
     D ##rtcd          s              1
     D kkco#           s              3  0 inz(777)
     D kkzip           s                   like(zrfzcd)
     D xxloc           s                   like(zrrgn#)
     D xxmramed        s                   like(prmra)
     D XXMRAMED26      s                   like(prmra)
     D XXMRAMEDTC      s                   like(prmra)
     D ##alow          s                   like(prmra)
     C*
     C     *entry        plist
     C                   parm                    @@eob#
     C                   parm                    @@code
     C                   parm                    @@mod
     C                   parm                    @@efdt
     C                   parm                    @@st
     C                   parm                    ##alow
     C                   parm                    ##rtcd
     C*
     C     fmprlflKey    klist
     C                   kfld                    kkco#
     C                   kfld                    @@code
     C                   kfld                    @@efdt
     C*
     C     fmprlflKey1   klist
     C                   kfld                    kkco#
     C                   kfld                    @@code
     C*
     C     faeol31Key    klist
     C                   kfld                    kkco#
     C                   kfld                    @@eob#
     C*
     C     faehlKey      klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     C                   kfld                    eoeob#
     C*
     C     fddhlKey      klist
     C                   kfld                    eoco#
     C                   kfld                    eosub#
     C                   kfld                    eofnd
     C                   kfld                    eofyr
     C                   kfld                    eocase
     C                   kfld                    eoeob#
     C*
     C     fmzrlflKey    klist
     C                   kfld                    kkco#
     C                   kfld                    kkzip
     C                   kfld                    @@efdt
     C*
     C     fmzrlflKey1   klist
     C                   kfld                    kkco#
     C                   kfld                    kkzip
      /Free
           // Main Processing
           //Get Record from FAEOP

           Chain faeol31Key peo01;

              If %found(faeol31);
              Chain faehlKey peh01;
              Chain fddhlKey pdh01;

                 If @@efdt < 1050509
                    and (%subst(ehflic:1:2) = 'ME'
                    or %subst(ehflic:1:2) = 'OS')
                    or %subst(ehflic:1:3) = 'MFC'
                    or @@efdt >= 1050509 and
                    %subst(ehflic:1:2) <> 'ZZ' and
                    %subst(ehflic:1:2) <> 'XX';

           // Find the locality

                    kkzip = %int(%subst(eoflic:1 :5));
                    Setgt fmzrlflKey pzr01;
                    Readpe fmzrlflKey1 pzr01;

                       If %found(fmzrlfl);
                          xxloc = zrrgn#;
                       Else;
                          xxloc = 99;
                       Endif;

           // Load the correct rate

                    xxmramed = 0;
                    xxmramed26 = 0;
                    xxmramedTC = 0;

           // Read FMPRPFL to get the highest Florida Fee Schedule allowance

                    Setgt fmprlflKey ppr01;
                    Readpe fmprlflKey1 ppr01;

                    Select;

            // Locality 99 (01/02)

                    When xxloc = 99;
                    xxmramed26 = pr92n5;
                    xxmramedTC = pr92n6;

                       If dhpos = ' 11';
                          xxmramed = pr92n3;
                       Else;
                          xxmramed = pr92n4;
                       Endif;

            //Locality 03

                    When xxloc = 03;
                    xxmramed26 = pr92n9;
                    xxmramedTC = pr92na;

                       If dhpos = ' 11';
                          xxmramed = pr92n7;
                       Else;
                          xxmramed = pr92n8;
                       Endif;

            // Locality 04

                    When xxloc = 04;
                    xxmramed26 = pr92nd;
                    xxmramedTC = pr92ne;

                       If dhpos = ' 11';
                          xxmramed = pr92nb;
                       Else;
                          xxmramed = pr92nc;
                       Endif;

                    Endsl;

                 Endif;
              Endif;
           Chain faehlKey peh01;
           Chain fddhlKey pdh01;
           // Read FMPRPFL to get the highest Florida Fee Schedule allowance
           Setgt fmprlflKey ppr01;
           Readpe fmprlflKey1 ppr01;

              If @@efdt >= 1090209
                 and (%subst(ehflic:1:2) = 'ZZ'
                 or %subst(ehflic:1:2) = 'XX');

                 If dhpos = ' 11';
                    xxmramed = pr92nf;
                 Else;
                    xxmramed = pr92ng;
                 Endif;

                 If @@mod = '26';
                    xxmramed26 = pr92nh;
                 Endif;
    
                 If @@mod = 'TC';
                   xxmramedTC = pr92ni;
                 Endif;
              Endif;
         // Retrun highest accurate allowance

              Select;
              When @@mod = '26';
              ##Alow = xxmramed26;
              ##rtcd = 'P';

              When @@mod = 'TC';
              ##Alow = xxmramedTC;
              ##rtcd = 'P';

              Other;
              ##Alow = xxmramed;
              ##rtcd = 'P';
              Endsl;

              If ##Alow = 0;
                 ##rtcd = 'F';
              Endif;

         *inlr = *on;
         return;
      /End-Free
