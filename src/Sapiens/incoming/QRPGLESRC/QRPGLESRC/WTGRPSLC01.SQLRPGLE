     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve file RRE from member in WXGSP
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTGRPSLCT
     h*  Calls to:
     h*
     h*  Created on 06/24/15   by C:33  for 35519
     h**********************************************************************
     h* Revisions:
     h*
     h* 09/11/15 C:295 PROJECT: WC2362
     h*                Include Transaction ID to Group Selection
     h*
     h*
     h**********************************************************************
     fwxgsl     if   e           k Disk    ReName(pgs01:pgs00)
      /copy sPrAtrium
      /copy sPrString
      * *ENTRY
     DWTGRPSLC01       pr                  ExtPgm('WTGRPSLC01')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DWTGRPSLC01       pi
     D inRrn                          9  0
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     D decRRE#         s                   like(GSRRE#)
     D startPos        s              5  0 inz
     D endPos          s              5  0 inz
     D len             s              5  0 inz
     D value           s             10    inz
     D member          s             10    inz
     D***** Beg Chg **** 09/11/15 ******************************
     D**** choices         s          10000    inz
     D RREchoices      s          10000    inz
     D TCCchoices      s                   like(RREchoices)
     D strTCC          s                   like(GSTCC)
     D***** End Chg **** 09/11/15 ******************************
     D sqlStmt         s           1000    inz
     D alias           s             50    inz
      /Free

       //                          1   5  8  9
       // input data is in format: ,member,

       Exsr ProcessGenParm;
       If RtnCode = 'X';
          RtnCode = ' ';
          BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
          Return;
       Endif;
       member = upper(member);
       //**** Beg Chg **** 09/11/15 ******************************
       //****** choices = *blanks;
       RREchoices = *blanks;
       //**** End Chg **** 09/11/15 ******************************

       alias = 'QTEMP/WXGSP_MEMBER';

       Monitor;
          sqlStmt = 'CREATE ALIAS ' + %trim(alias) +
           ' FOR WXGSP(' + %trim(member) + ')';
          Exec Sql Execute IMMEDIATE :SqlStmt;

          sqlStmt = 'DECLARE CURSOR cursor1 cursor as ' +
           '(select distinct(GSRRE#) from ' + %trim(alias) +
           ' order by GSRRE#';
          Exec Sql DECLARE cursor1 cursor for
           select distinct(GSRRE#) from QTEMP/WXGSP_MEMBER
           order by GSRRE#;

          Exec Sql open cursor1;
          Dou SqlCod <> *zero;
             Exec Sql Fetch cursor1 into :decRRE#;
             If SqlCod = *zero;
       //**** Beg Chg **** 09/11/15 ******************************
       //****   choices = %trimr(choices) + ',' + %editc(decRRE#:'X');
                RREchoices = %trimr(RREchoices) + ',' + %editc(decRRE#:'X');
       //**** End Chg **** 09/11/15 ******************************
             Endif;
          Enddo;

          Exec Sql close cursor1;

       //**** Beg Add **** 09/11/15 ******************************
       sqlStmt = 'DECLARE CURSOR cursor2 cursor as ' +
        '(select distinct(GSTCC) from ' + %trim(alias) +
        ' order by GSTCC';

       Exec Sql DECLARE cursor2 cursor for
        select distinct(GSTCC) from QTEMP/WXGSP_MEMBER
        order by GSTCC;

        Exec Sql open cursor2;
        Dou SqlCod <> *zero;
           Exec Sql Fetch cursor1 into :strTCC;
           If SqlCod = *zero;
              TCCchoices = %trimr(TCCchoices) + ',' + strTCC;
           Endif;
        Enddo;

        Exec Sql close cursor2 ;
       //**** End Add **** 09/11/15 ******************************

          sqlStmt = 'DROP ALIAS' + %trim(alias);
          Exec Sql Execute IMMEDIATE :SqlStmt;

       //**** Beg Chg **** 09/11/15 ******************************
       //******* BuildJSON(outJSON:'choices':%trim(choices));
           BuildJSON(outJSON:'RREchoices':%trim(RREchoices));
           BuildJSON(outJSON:'TCCchoices':%trim(TCCchoices));
       //**** End Chg **** 09/11/15 ******************************
          RtnCode = ' ';

       On-Error;

          BuildJSON(outJSON :'RtnCode' :'RRE list not retrieved');

       EndMon;

       Return;
       //**************************************************************
       // Process genparm
       //**************************************************************
       BegSr ProcessGenParm;
        startPos = 0;
        endPos = 0;
        Monitor;
        // extract member
           startPos = %Scan(',':inGenParm)+1;
           If startPos <= 1;
              LeaveSr;
           Endif;
           Exsr ExtractValue;
           member = %trim(value);
           Rtncode = ' ';
        On-Error;
           Rtncode = 'X';
        EndMon;
       EndSr;
       //*********************************************************************
       // extract values from genparm
       //*********************************************************************
       BegSr ExtractValue;
        value = *blanks;
        If startPos > 0;
           endPos = %Scan(',':inGenParm:startPos);
           If endPos > 0;
              len = endPos-startPos;
              If len > 0;
                 value = %Subst(inGenParm:startPos:len);
              Endif;
           Endif;
        Endif;
       EndSr;
      /End-Free
