     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR00503ZC - Retrospective Premium Endorsement Attached Schd
     *===============================================================*
     * WC000503ZD - Retrospective Rating Plan Premium Endorsement.   *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 02/20/20                                          *
     * Programmer: C:079                                             *
     * Project   : WC2729                                            *
     *===============================================================*
     * Revisions:                                                    *
      *                                                               *
     * 02/20/20 C: 079 Project: WC2729                               *
     *                 Remove pfm01 and turn write in copybook off   *
     * ------------------------------------------------------------- *
     *  Note: Please add comments to the top of revisions.           *
     * ------------------------------------------------------------- *
     *===============================================================*
     * Notes:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     *                                                               *
     *  QQMODE = 1 - Called to determine if form should be attached. *
     *  QQMODE = 2 - Called to print endorsement.                    *
     *  QQMODE = 3 - Called for additional input.                    *
     *  QQMODE = 4 - Called for mid-term processing.                 *
     *===============================================================*
    
     ** Copybook F specs
     f/copy scopyen01f
    
     * Billing Schedule
     fwtabl     if   e           k disk    Include(pbs01)
    
     * Admin/Darts Control File
     fwmexl     if   e           k disk
    
      ** Additional Work File
     fwxfmp3    o    e             disk    Usropn
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     d/copy scopyen01d
    
     *=========================================================================
     *  Stand alone
     *=========================================================================
     d x1              s              2  0
     d xxSvemp#        s              9  0
     d x               s              4  0
     d x1Carr          s             20
     d xpCarr          s             20
     d xx30            s             30
     d xx26            s             26
     d xx14            s             14
     d xx10            s             10
     d xx6             s              6
     d xx1st           s              1
     d xxLine          s              2  0
     d xxSpot          s              2  0
     d xaEmp#          s              9
    d xaFyr           s              3
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqForm          c                   Const('WC000503ZD')
    
     ** Additional entry program
     D AddPgm          c                   Const('SRXXXXXXXX')
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
     *
      *=========================================================================
      * Prototype Declaration
      *=========================================================================
     d CvtDatMDY       pr                       Extpgm('SRCVTDAT')
     d  srdti                         8  0
     d                               10    Const
    d                                8    Const
    d  srdto                         8
    d  srerr                         1
      *
     ** Entry parameters
     C     *Entry        Plist
     C                   Parm                    qqCo#             3
     C                   Parm                    qqFnd             3
     C                   Parm                    qqEmp#            9
     C                   Parm                    qqDiv             5
     C                   Parm                    qqFyr             3
     C                   Parm                    qqMode            1
     C                   Parm                    qqRRn             9 0
     C                   Parm                    qqMod2            1
     *=========================================================================
     *  KLIST definitions
     *=========================================================================

      * Admin/Darts Control File

     c     keyWmexp      Klist
     c                   Kfld                    xxCo#
     c                   Kfld                    xxFnd

      * Employer Detail

     c     keyWdell      Klist
     c                   Kfld                    xxCo#
     c                   Kfld                    xxFnd
     c                   Kfld                    xxFyr

      * Carrier Policy # file

     c     keyWmrpl      Klist
     c                   Kfld                    xxCo#
     c                   Kfld                    xxFnd
     c                   Kfld                    elEmp#
     c                   Kfld                    elDiv
     c                   Kfld                    xxFyr

      * Billing Schedule

     c     keyWtabl      Klist
     c                   Kfld                    xxCo#
     c                   Kfld                    xxKey            10
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     c/Copy scopyen01c
     *=========================================================================
     *  CLRDAT - Clear data fields for next page.
     *=========================================================================
      /Free
       Begsr Clrdat;

         Clear FMnm01;
         Clear FMnm02;
         Clear FMnm03;
         Clear FMnm04;
         Clear FMnm05;
         Clear FMnm06;
         Clear FMnm07;
         Clear FMnm08;
         Clear FMnm09;
         Clear FMnm10;
         Clear FMnm11;
         Clear FMnm12;
         Clear FMnm13;
         Clear FMnm14;

         Clear FMc101;
         Clear FMc102;
         Clear FMc103;
         Clear FMc104;
         Clear FMc105;
         Clear FMc106;
         Clear FMc107;
         Clear FMc108;
         Clear FMc109;
         Clear FMc110;
         Clear FMc111;
         Clear FMc112;
         Clear FMc113;
         Clear FMc114;
         Clear FMc115;
         Clear FMc116;

         Clear FMc201;
         Clear FMc202;
         Clear FMc203;
         Clear FMc204;
         Clear FMc205;
         Clear FMc206;
         Clear FMc207;
         Clear FMc208;
         Clear FMc209;
         Clear FMc210;
         Clear FMc211;
         Clear FMc212;
         Clear FMc213;
         Clear FMc214;

         Clear FMc301;
         Clear FMc302;
         Clear FMc303;
         Clear FMc304;
         Clear FMc305;
         Clear FMc306;
         Clear FMc307;
         Clear FMc308;
         Clear FMc309;
         Clear FMc310;
         Clear FMc311;
         Clear FMc312;
         Clear FMc313;
         Clear FMc314;
         Clear FMc315;
         Clear FMc316;

         Clear FMc401;
         Clear FMc402;
         Clear FMc403;
         Clear FMc404;
         Clear FMc405;
         Clear FMc406;
         Clear FMc407;
         Clear FMc408;
         Clear FMc409;
         Clear FMc410;
         Clear FMc411;
         Clear FMc412;
         Clear FMc413;
         Clear FMc414;
         Clear FMc415;
         Clear FMc416;
         Clear FMc417;
         Clear FMc418;
         Clear FMc419;
         Clear FMc420;
         Clear FMc421;
         Clear FMc422;
         Clear FMc423;
         Clear FMc424;

       Endsr;
    
      //=========================================================================
      //OTHINF - Subroutine to load form specific information & write record
      //=========================================================================
       Begsr Othinf;
          Open wmrpl;

         //Open work file
          Open wxfmp3;
    
         //Initialize
          Clear pfm03;
          fmForm = xxForm;
          srdti = elPFdt;
          CvtDatMDY(srdti:'*CYMD':'*MDYY':srdto:srerr);
          fmEedt = %Int(srdto);
         xxSVemp# = 0;
          x = 1;
          xx1st = *blanks;
          xxLine = 1;
          xxSpot = 1;

          // Loop through all employers in group to list their policy #'s.
          Setll keyWdell pel02;
          Reade keyWdell pel02;
         Dow Not %Eof(Wdell1) and x < 9999;
            If ELemp# <> xxemp# and ELemp#  <> xxSvEmp#;
                Chain keyWmrpl prp01;
                If %found(Wmrpl) and RPcap# <> *blanks;
                   x1Carr = RPcap#;
                Else;
                   xaEmp# = %EditC(ELemp#:'X');
                   xaFyr  = %EditC(xxFyr:'X');
                   x1Carr = %EditC(xxFnd:'X');
                   x1Carr = %trim(x1Carr) + %trim(xaEmp#) + %trim(xaFyr);
                Endif;
                Exsr Setcar;
                x = x+1;
                xxSvemp# = ELemp#;
             Endif;
          Reade keyWdell pel02;
          Enddo;

         // Write the output record
       //*** Beg Chg *** 02/20/20 ******************************************
       //*Write pfm01;
       //*If xx1st = *blank;
       //*** Write pfm03;
       //*** xx1st = 'Y';
       //*Endif;
          Exsr WriteWorkFile;
       //*** End Chg *** 02/20/20 ******************************************
    
         // Close the endorsement form workfile
          Close wxFmp3;
          Close wmRpl;
       Endsr;
    
      //=========================================================================
      //  PGMSTR - Beginning of program processing
      //=========================================================================
       Begsr PgmStr;
       //*** Beg Add *** 02/20/20 ******************************************
          WriteinCopyBook = *off;
       //*** End Add *** 02/20/20 ******************************************
       Endsr;
    
      //=========================================================================
      //  PGMEND - End of program processing
      //=========================================================================
       Begsr PgmEnd;
       Endsr;
    
      //=========================================================================
      //  SETCAR - Set the carrier policy# into data field.
      //=========================================================================
       Begsr Setcar;
          Select;
             When xxLine = 1;
                Select;
                When xxSpot = 1;
                   FMnm01 = x1Carr;
                When xxSpot = 2;
                   xx26 = x1Carr;
                   %subst(FMnm01:21:26) = xx26;
                When xxSpot = 3;
                   %subst(FMnm01:41:6) = %subst(x1Carr:1:6);
                   FMc201 =  %subst(x1Carr:7:14);
                Endsl;

             When xxLine = 2;
                Select;
                When xxSpot = 1;
                   FMnm02 = x1Carr;
                When xxSpot = 2;
                   xx26 = x1Carr;
                   %subst(FMnm02:21:26) = xx26;
                When xxSpot = 3;
                   %subst(FMnm02:41:6) = %subst(x1Carr:1:6);
                   FMc202 =  %subst(x1Carr:7:14);
                Endsl;

             When xxLine = 3;
                Select;
                When xxSpot = 1;
                   FMnm03 = x1Carr;
                When xxSpot = 2;
                   xx26 =  x1Carr;
                   %subst(FMnm03:21:26) = xx26;
                When xxSpot = 3;
                   %subst(FMnm03:41:6) = %subst(x1Carr:1:6);
                   FMc203 =  %subst(x1Carr:7:14);
                Endsl;

             When xxLine = 4;
                Select;
                when xxSpot = 1;
                   FMnm04 = x1Carr;
                When xxSpot = 2;
                   xx26 = x1Carr;
                   %subst(FMnm04:21:26) = xx26;
                When xxSpot = 3;
                   %subst(FMnm04:41:6) = %subst(x1Carr:1:6);
                   FMc204 =  %subst(x1Carr:7:14);
                Endsl;

             When xxLine = 5;
                Select;
                When xxSpot = 1;
                   FMnm05 = x1Carr;
                When xxSpot = 2;
                   xx26 = x1Carr;
                   %subst(FMnm05:21:26) = xx26;
                When xxSpot = 3;
                   %subst(FMnm05:41:6) = %subst(x1Carr:1:6);
                   FMc205 =  %subst(x1Carr:7:14);
                Endsl;

             When xxLine = 6;
                Select;
                When xxSpot = 1;
                   FMnm06 = x1Carr;
                When xxSpot = 2;
                   xx26 = x1Carr;
                   %subst(FMnm06:21:26) = xx26;
                When xxSpot = 3;
                   %subst(FMnm06:41:6) = %subst(x1Carr:1:6);
                   FMc206 =  %subst(x1Carr:7:14);
                Endsl;

             When xxLine = 7;
                Select;
                When xxSpot = 1;
                   FMnm07 = x1Carr;
                When xxSpot = 2;
                   xx26 = x1Carr;
                   %subst(FMnm07:21:26) = xx26;
                When xxSpot = 3;
                   %subst(FMnm07:41:6) = %subst(x1Carr:1:6);
                   FMc207 =  %subst(x1Carr:7:14);
                Endsl;

             When xxLine = 8;
                Select;
                When xxSpot = 1;
                   FMnm08 = x1Carr;
                When xxSpot = 2;
                   xx26 = x1Carr;
                   %subst(FMnm08:21:26) = xx26;
                When xxSpot = 3;
                   %subst(FMnm08:41:6) = %subst(x1Carr:1:6);
                   FMc208 =  %subst(x1Carr:7:14);
                Endsl;

             When xxLine = 9;
                Select;
                When xxSpot = 1;
                   FMnm09 = x1Carr;
                When xxSpot = 2;
                   xx26 = x1Carr;
                   %subst(FMnm09:21:26) = xx26;
                When xxSpot = 3;
                   %subst(FMnm09:41:6) = %subst(x1Carr:1:6);
                   FMc209 =  %subst(x1Carr:7:14);
                Endsl;

             When xxLine = 10;
                Select;
                When xxSpot = 1;
                   FMnm10 = x1Carr;
                When xxSpot = 2;
                   xx26 = x1Carr;
                   %subst(FMnm10:21:26) = xx26;
                When xxSpot = 3;
                   %subst(FMnm10:41:6) = %subst(x1Carr:1:6);
                   FMc210 =  %subst(x1Carr:7:14);
                Endsl;

             When xxLine = 11;
                Select;
                When xxSpot = 1;
                   FMnm11 = x1Carr;
                When xxSpot = 2;
                   xx26 = x1Carr;
                   %subst(FMnm11:21:26) = xx26;
                When xxSpot = 3;
                   %subst(FMnm11:41:6) = %subst(x1Carr:1:6);
                   FMc211 =  %subst(x1Carr:7:14);
                Endsl;

             When xxLine = 12;
                Select;
                When xxSpot = 1;
                   FMnm11 = x1Carr;
                When xxSpot = 2;
                   xx26 = x1Carr;
                   %subst(FMnm12:21:26) = xx26;
                When xxSpot = 3;
                   %subst(FMnm12:41:6) = %subst(x1Carr:1:6);
                   FMc212 =  %subst(x1Carr:7:14);
                Endsl;

             When xxLine = 13;
                Select;
                When xxSpot = 1;
                   FMnm13 = x1Carr;
                When xxSpot = 2;
                   xx26 = x1Carr;
                   %subst(FMnm13:21:26) = xx26;
                When xxSpot = 3;
                   %subst(FMnm13:41:6) = %subst(x1Carr:1:6);
                   FMc213 =  %subst(x1Carr:7:14);
                Endsl;

             When xxLine = 14;
                Select;
                When xxSpot = 1;
                   FMnm14 = x1Carr;
                When xxSpot = 2;
                   xx26 = x1Carr;
                   %subst(FMnm14:21:26) = xx26;
                When xxSpot = 3;
                   %subst(FMnm14:41:6) = %subst(x1Carr:1:6);
                   Fmc214 =  %subst(x1Carr:7:14);
                Endsl;

             When xxLine = 15;
                Select;
                When xxSpot = 1;
                   FMc301 = x1Carr;
                When xxSpot = 2;
                   %subst(FMc301:21:10) = %subst(x1Carr:1:10);
                   fmC401 = %subst( x1Carr:11:10);
                When xxSpot = 3;
                   xx30  = x1Carr;
                   %subst(Fmc401:11:30) = xx30;
                Endsl;

             When xxLine = 16;
                Select;
                When xxSpot = 1;
                   FMc302 = x1Carr;
                When xxSpot = 2;
                   %subst(FMc302:21:10) = %subst(x1Carr:1:10);
                   fmC402 = %subst(x1Carr:11:10);
                When xxSpot = 3;
                   xx30  = x1Carr;
                   %subst(FMc402:11:30) = xx30;
                Endsl;

             When xxLine = 17;
                Select;
                When xxSpot = 1;
                   FMc303 = x1Carr;
                When xxSpot = 2;
                   %subst(FMc303:21:10) = %subst(x1Carr:1:10);
                   FMc403 = %subst(x1Carr:11:10);
                When xxSpot = 3;
                   xx30  = x1Carr;
                   %subst(FMc403:11:30) = xx30;
                Endsl;

             When xxLine = 18;
                Select;
                When xxSpot = 1;
                   FMc304 = x1Carr;
                When xxSpot = 2;
                   %subst(FMc304:21:10) = %subst(x1Carr:1:10);
                   FMc404 = %subst(x1Carr:11:10);
                When xxSpot = 3;
                   xx30  = x1Carr;
                   %subst(FMc404:11:30) = xx30;
                Endsl;

             When xxLine = 19;
                Select;
                When xxSpot = 1;
                   FMc305 = x1Carr;
                When xxSpot = 2;
                   %subst(FMc305:21:10) = %subst(x1Carr:1:10);
                   FMc405 = %subst(x1Carr:11:10);
                When xxSpot = 3;
                   xx30  = x1Carr;
                   %subst(FMc405:11:30) = xx30;
                Endsl;

             When xxLine = 20;
                Select;
                When xxSpot = 1;
                   FMc306 = x1Carr;
                When xxSpot = 2;
                   %subst(FMc306:21:10) = %subst(x1Carr:1:10);
                   FMc406 = %subst(x1Carr:11:10);
                When xxSpot = 3;
                   xx30  = x1Carr;
                   %subst(FMc406:11:30) = xx30;
                Endsl;

             When xxLine = 21;
                Select;
                When xxSpot = 1;
                   FMc307 = x1Carr;
                When xxSpot = 2;
                   %subst(FMc307:21:10) = %subst(x1Carr:1:10);
                   FMc407 = %subst(x1Carr:11:10);
                When xxSpot = 3;
                   xx30  = x1Carr;
                   %subst(FMc407:11:30) = xx30;
                Endsl;

             When xxLine = 22;
                Select;
                When xxSpot = 1;
                   FMc308 = x1Carr;
                When xxSpot = 2;
                   %subst(FMc308:21:10) = %subst(x1Carr:1:10);
                   FMc408 = %subst(x1Carr:11:10);
                When xxSpot = 3;
                   xx30  = x1Carr;
                   %subst(FMc408:11:30) = xx30;
                Endsl;

             When xxLine = 23;
                Select;
                When xxSpot = 1;
                   FMc309 = x1Carr;
                When xxSpot = 2;
                   %subst(FMc309:21:10) = %subst(x1Carr:1:10);
                   FMc409 = %subst(x1Carr:11:10);
                When xxSpot = 3;
                   xx30  = x1Carr;
                   %subst(FMc409:11:30) = xx30;
                Endsl;

             When xxLine = 24;
                Select;
                When xxSpot = 1;
                   FMc310 = x1Carr;
                When xxSpot = 2;
                   %subst(FMc310:21:10) = %subst(x1Carr:1:10);
                   FMc410 = %subst(x1Carr:11:10);
                When xxSpot = 3;
                   xx30  = x1Carr;
                   %subst(FMc410:11:30) = xx30;
                Endsl;

             When xxLine = 25;
                Select;
                When xxSpot = 1;
                   FMc311 = x1Carr;
                When xxSpot = 2;
                   %subst(FMc311:21:10) = %subst(x1Carr:1:10);
                   FMc411 = %subst(x1Carr:11:10);
                When xxSpot = 3;
                   xx30  = x1Carr;
                   %subst(FMc411:11:30) = xx30;
                Endsl;

             When xxLine = 26;
                Select;
                When xxSpot = 1;
                   FMc312 = x1Carr;
                When xxSpot = 2;
                   %subst(FMc312:21:10) = %subst(x1Carr:1:10);
                   FMc412 = %subst(x1Carr:11:10);
                When xxSpot = 3;
                   xx30  = x1Carr;
                   %subst(FMc412:11:30) = xx30;
                Endsl;

             When xxLine = 27;
                Select;
                When xxSpot = 1;
                   FMc313 = x1Carr;
                When xxSpot = 2;
                   %subst(FMc313:21:10) = %subst(x1Carr:1:10);
                   FMc413 = %subst(x1Carr:11:10);
                When xxSpot = 3;
                   xx30  = x1Carr;
                   %subst(FMc413:11:30) = xx30;
                Endsl;

             When xxLine = 28;
                Select;
                When xxSpot = 1;
                   FMc314 = x1Carr;
                When xxSpot = 2;
                   %subst(FMc314:21:10) = %subst(x1Carr:1:10);
                   FMc414 = %subst(x1Carr:11:10);
                When xxSpot = 3;
                   xx30  = x1Carr;
                   %subst(FMc414:11:30) = xx30;
                Endsl;

             When xxLine = 29;
                Select;
                When xxSpot = 1;
                   FMc315 = x1Carr;
                When xxSpot = 2;
                   %subst(FMc315:21:10) = %subst(x1Carr:1:10);
                   FMc415 = %subst(x1Carr:11:10);
                When xxSpot = 3;
                   xx30  = x1Carr;
                   %subst(FMc415:11:30) = xx30;
                Endsl;

             When xxLine = 30;
                Select;
                When xxSpot = 1;
                   FMc316 = x1Carr;
                When xxSpot = 2;
                   %subst(FMc316:21:10) = %subst(x1Carr:1:10);
                   FMc416 = %subst(x1Carr:11:10);
                When xxSpot = 3;
                   xx30  = x1Carr;
                   %subst(FMc416:11:30) = xx30;
                Endsl;

             When xxLine = 31;
                Select;
                When xxSpot = 1;
                   FMc101 = x1Carr;
                   FMc417 = %subst(x1Carr:11:10);
                When xxSpot = 2;
                   xx30  = x1Carr;
                   %subst(FMc417:11:30) = xx30;
                When xxSpot = 3;
                   %subst(FMc417:31:10) = %subst(x1Carr:1:10);
                   FMc102= %subst(x1Carr:11:10);
                Endsl;

             When xxLine = 32;
                Select;
                When xxSpot = 1;
                   FMc103 = x1Carr;
                   FMc418 = %subst(x1Carr:11:10);
                When xxSpot = 2;
                   xx30  = x1Carr;
                   %subst(FMc418:11:30) = xx30;
                When xxSpot = 3;
                   %subst(FMc418:31:10) = %subst(x1Carr:1:10);
                   FMc104= %subst(x1Carr:11:10);
                Endsl;

             When xxLine = 33;
                Select;
                When xxSpot = 1;
                   FMc105 = x1Carr;
                   FMc419 = %subst(x1Carr:11:10);
                When xxSpot = 2;
                   xx30  = x1Carr;
                   %subst(FMc419:11:30) = xx30;
                When xxSpot = 3;
                   %subst(FMc419:31:10) = %subst(x1Carr:1:10);
                   FMc106= %subst(x1Carr:11:10);
                Endsl;

             When xxLine = 34;
                Select;
                When xxSpot = 1;
                   FMc107 = x1Carr;
                   FMc420 = %subst(x1Carr:11:10);
                When xxSpot = 2;
                   xx30  = x1Carr;
                   %subst(FMc420:11:30) = xx30;
                When xxSpot = 3;
                   %subst(FMc420:31:10) = %subst(x1Carr:1:10);
                   FMc108= %subst(x1Carr:11:10);
                Endsl;

             When xxLine = 35;
                Select;
                When xxSpot = 1;
                   FMc109 = x1Carr;
                   FMc421 = %subst(x1Carr:11:10);
                When xxSpot = 2;
                   xx30  = x1Carr;
                   %subst(FMc421:11:30) = xx30;
                When xxSpot = 3;
                   %subst(FMc421:31:10) = %subst(x1Carr:1:10);
                   FMc110= %subst(x1Carr:11:10);
                Endsl;

             When xxLine = 36;
                Select;
                When xxSpot = 1;
                   FMc111 = x1Carr;
                   FMc422 = %subst(x1Carr:11:10);
                When xxSpot = 2;
                   xx30  = x1Carr;
                   %subst(FMc422:11:30) = xx30;
                When xxSpot = 3;
                   %subst(FMc422:31:10) = %subst(x1Carr:1:10);
                   FMc112= %subst(x1Carr:11:10);
                Endsl;

             When xxLine = 37;
                Select;
                When xxSpot = 1;
                   FMc113 = x1Carr;
                   FMc423 = %subst(x1Carr:11:10);
                When xxSpot = 2;
                   xx30  = x1Carr;
                   %subst(FMc423:11:30) = xx30;
                When xxSpot = 3;
                   %subst(FMc423:31:10) = %subst(x1Carr:1:10);
                   FMc114= %subst(x1Carr:11:10);
                Endsl;

             When xxLine = 38;
                Select;
                When xxSpot = 1;
                   FMc115 = x1Carr;
                   FMc424 = %subst(x1Carr:11:10);
                When xxSpot = 2;
                   xx30  = x1Carr;
                   %subst(FMc424:11:30) = xx30;
                When xxSpot = 3;
                   %subst(FMc424:31:10) = %subst(x1Carr:1:10);
                   FMc116= %subst(x1Carr:11:10);
                Endsl;

           Endsl;

       // Increment line and spot counters.Write record if page is full.
       If xxLine = 38 and xxSpot = 3;
       //*** Beg Chg *** 02/20/20 ******************************************
       //*Write pfm01;
       //*If xx1st = *blank;
       //*** Write pfm03;
       //*** xx1st = 'Y';
       //*Endif;
          Exsr WriteWorkFile;
       //*** End Chg *** 02/20/20 ******************************************

          Exsr Clrdat;
          xxLine = 1;
          xxSpot = 1;
       Else;
          xxSpot = xxSpot+1;
          If xxSpot > 3;
             xxLine = xxLine+1;
             xxSpot = 1;
          Endif;
       Endif;
       evalr xpCarr = x1Carr;

       Endsr;
    
      //=========================================================================
      // VALID - Subroutine to attach this form to the policy
      //=========================================================================
       Begsr Valid;
      Chain keyWMEXP pex01;
       If %Found(wtabl);
          xxKey = %EditC(ELbscd:'X');
          Chain keyWtabl pbs01;
          If %Found(wtabl) and ((BSlpgm = 'P') or (BSlpgm = 'I'))
                           and (EXretg = 'Y');
          Crtfrm = *on;
          Endif;
       Endif;
       Endsr;
      //=========================================================================
