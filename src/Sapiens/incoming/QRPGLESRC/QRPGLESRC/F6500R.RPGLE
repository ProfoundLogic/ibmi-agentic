     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*
     H****************************************************************
     H*  F6500R - Build Member Name
     H*
     H*  Description: This pgm will build and return member name
     H*
     H*  Created    : 04/12/2016  Programmer: 212  Project: C114MXO
     H*
     H*  Called By  : F6500C
     H*  Calls To   :
     H*
     H****************************************************************
     H*  Revisions:
     H****************************************************************
    F*
    f* Log Record File
     ffak9l1    if a e           k disk
     f*
    D*
     d** Procedure definitions
     d*
     d*** spbnddir/spifiles
     d bldMember       pr            10
     D inprefix                       1
     D informat                       1
     D insuffix                       1

     d** Data Structures
     d*
    D*
     D psds          esds                  extname(szq1p)
     d*
     d                 ds
     d DTime                         20  0
     d CDateIso                       8  0 overlay(DTime)
     d CTimeHms                       6  0 overlay(DTime:*next)
     d*
     d                 ds
     d xxnewmbr                      10    inz
     d xxnewprfx                      1    overlay(xxnewmbr:1)
     d xxnewfmt                       7    overlay(xxnewmbr:2)
     d xxnewsfx                       2    overlay(xxnewmbr:9)
     d xxnewsfx#                      2s 0 overlay(xxnewsfx)
     d*
     D** Standalone variables
     D*
     D @@newmbr        s             10

     D inprfx          s              1    inz
     D infrmt          s              1    inz
     D insfx           s              1    inz
     d*
    C*
     c     *entry        plist
     c                   parm                    @@newmbr
      /free
        //================================================================
        //mainline
        //================================================================

        // initialisation routine
         exsr srinit;

        // call proc to build member name
         xxnewmbr = bldmember(inprfx:infrmt:insfx);
         if xxnewmbr <> *blanks;

           // verify if the member name is already in use
           Setll xxnewmbr pk901;
           dow %equal (fak9l1);
             // increment suffix to create new name
             if xxnewsfx# = 99;
                xxnewsfx# = 01;
             else;
                xxnewsfx# += 1;
             endif;
             Setll xxnewmbr pk901;
           enddo;

           @@newmbr = xxnewmbr;

           // create FAK9P record with current suffix
           K9mbr = xxnewmbr;
           K9user = q1user;
           K9pgm = q1pgm;
           K9dt = %dec(%char(%date():*cymd0):7:0);
           K9time = %dec(%char(%time():*iso0):6:0);
           K9iusr = q1user;
           K9ipgm = q1pgm;
           K9idt = K9dt;
           K9itim = K9time;

           write pk901;

         endif;

         *inlr = *on;

        //==================================================================
        // end of mainline
        //==================================================================
        // SrInit - initialization subroutine
        //================================================================
         begsr srinit;

           dtime = %dec(%timestamp());

         endsr;
        //===============================================================
      /end-free
     P*=====================================================================
     P*** Beg Del *** 07/29/16 ********************************************'
     P* bldMember - returns name for a member
     P*  Parameter 1 - prefix                  (1a)
     P*  Parameter 2 - format                  (1a)
     P*  Parameter 3 - suffix                  (1a)
     P*  Returns - a member name of 10 char
     P*  P = prefix, M = middle, S = suffix
     P* example : PMMMMMMMSS
     P*=====================================================================
     P*****bldMember        B                   Export
     D*****
     D*****bldMember        PI            10
     D***** prefix                         1
     D***** format                         1
     D***** suffix                         1

     D***** mbrname         s             10    inz

     D***** prfx            s              1    inz
     D***** frmt            s              1    inz
     D***** sfx             s              1    inz

     D***** xxprfx          s              1    inz
     D***** xxfrmt          s              7    inz
     D***** xxsfx           s              2    inz
         //********
         //***** /free
         //**if*%parms >= 3 and suffix <> *blanks;
         //****sfx = suffix;
         //**else;
         //****sfx = *blanks;
         //**endif;
         //*****
         //**if*%parms >= 2 and format <> *blanks;
         //****frmt = format;
         //**else;
         //****frmt = *blanks;
         //**endif;
         //*****
         //**if*%parms >= 1 and prefix <> *blanks;
         //****prfx = prefix;
         //**else;
         //****prfx = *blanks;
         //**endif;
         //*****
         //**//*default values in case they are not passed
         //*****
         //**if*sfx = *blanks;
         //****xxsfx = '00';
         //**endif;
         //*****
         //**if*prfx = *blanks;
         //****xxprfx = 'B';
         //**endif;
         //*****
         //**if*frmt = *blanks;
         //****frmt = 'J'; // julian
         //**endif;
         //*****
         //**//*build format
         //**select;
         //*****
         //***when frmt = 'J';
         //****xxfrmt = %char(%Date() : *LongJul0);
         //*****
         //**other;
         //**endsl;
         //*****
         //**mbrname = xxprfx + xxfrmt + xxsfx;
         //*****
         //**return mbrname;
         //*** /end-free

         //***Pbldmember        E
     P*** End Del *** 07/29/16 ********************************************'
