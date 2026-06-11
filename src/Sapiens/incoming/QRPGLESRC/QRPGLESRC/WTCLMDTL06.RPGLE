     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h*
     h*  Description: Response program to populate the address lines
     h*               Accident Location Information.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTCLMDTL
     h*  Calls to:
     h*
     h*  Created on 08/04/2014 by C:283 for Support WC146 84781
     h**********************************************************************
     h* Revisions:
     h*
     h* 12/08/17 c:62 Ticket: 51357(146)
     h*               Fix county, should be passing back coce not description
     h*
     h**********************************************************************
     fwmall     if   e           k Disk    ReName(pal01:pal00)
     fwmeml     if   e           k Disk    ReName(pem01:pem00)
     fwtcnl1    if   e           k Disk    ReName(pcn01:pcn01)

      /copy sPrAtrium

      * *Entry
     dDefaultPoints    pr                  extpgm('WTCLMDTL06')
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     dDefaultPoints    pi
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     dExtractValue     pr            20
     d inStart                        5  0 CONST
     d inGenParm                    512    CONST

     d inStart         s              5  0 inz
     d intCO#          s              3  0 inz
     d intFND#         s              3  0 inz
     d intEMP#         s              9  0 inz
     d intDIV          s              5  0 inz
     d intLOC          s             10  0 inz
     d xxCO#           s              3    inz
     d xxFND#          s              3    inz
     d xxEMP#          s              9    inz
     d xxDIV           s              5    inz
     d xxLOC           s             10    inz
     d xxNAME          s             40
     d xxADR1          s             40
     d xxCTY           s             25
     d xxST            s              2
     d xxKEY           s             10
     d xxCNTY          s             30
     d startPos        s              5  0 inz
     d endPos          s              5  0 inz
     d len             s              5  0 inz
     d strDtlZip1      s             10    inz(*blanks)
     d strDtlphn#      s             30    inz(*blanks)
     d outValue        s             20

     d                 Ds
     d  intzip                 1      9  0
     d  intzip1                1      5  0
     d  intzip2                6      9  0

      /free
       // input data is in format: ,ccc,ggg,eeeeeeeee,ddddd,llllllllll
       // ccc: Company #
       // ggg: Group #
       // eeeeeeeee: Employer #
       // ddddd: Division #
       // llllllllll: Location ID

       outJSON  = ' ';

       Exsr ProcessGenParm;
       If RtnCode = 'X';
          RtnCode = ' ';
          BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
          Return;
       Endif;

       xxKEY = *Blanks;
       Select;
       When xxLOC <> '0000000000';
          Chain(n) (intCO# : intFND# : intEMP# : intDIV : xxLOC) wmall;
          If %found(wmall);
             xxNAME = ALname;
             If ALpad1 <> *Blanks;
                xxADR1 = ALpad1;
                xxCTY = ALpcty;
                xxST = ALpst;
                intzip = ALpzip;
                strDtlZip1 = %editc(intZip1:'X') + '-'+ %editc(intZip2:'X');
                xxKEY = ALpst + ALcnty;
             Else;
                xxADR1 = ALmad1;
                xxCTY = ALmcty;
                xxST = ALmst;
                intzip = ALmzip;
                strDtlZip1 = %editc(intZip1:'X') + '-'+ %editc(intZip2:'X');
                xxKEY = ALmst + ALcnty;
             Endif;
          Endif;

       When xxLOC = '0000000000';
          Chain(N) (intCO# : intFND# : intEMP# : intDIV) wmeml;
          If %Found(wmeml);
             xxNAME = EMname;
             If EMpad1 <> *Blanks;
                xxADR1 = EMpad1;
                xxCTY = EMpcty;
                xxST = EMpst;
                intzip = EMpzip;
                strDtlZip1 = %editc(intZip1:'X') + '-'+ %editc(intZip2:'X');
                xxKEY = EMpst + EMcnty;
             Else;
                xxADR1 = EMmad1;
                xxCTY = EMmcty;
                xxST = EMmst;
                intzip = EMmzip;
                strDtlZip1 = %editc(intZip1:'X') + '-'+ %editc(intZip2:'X');
                xxKEY = EMmst + EMcnty;
             Endif;
          Endif;

       Endsl;

       Chain(N) (intCO# : xxKEY) wtcnl1;
       If %Found(wtcnl1);
          //*** Begin Chg *** 12/08/17 *********************
          //*** xxCNTY = CNdesc;
          xxCNTY = xxKey;
          //*** End   Chg *** 12/08/17 *********************
       Else;
          xxCNTY = *Blanks;
       Endif;

       BuildJSON(outJSON :'Points' :%trim(xxNAME));
       BuildJSON(outJSON :'Point1' :%trim(xxADR1));
       BuildJSON(outJSON :'Point2' :%trim(xxCTY));
       BuildJSON(outJSON :'Point3' :%trim(xxST));
       BuildJSON(outJSON :'Point4' :%trim(strDtlZip1));
       BuildJSON(outJSON :'Point5' :%trim(xxCNTY));

       RtnCode = ' ';
       Return;
       //**************************************************************
       // Process genparm
       //**************************************************************
       BegSr ProcessGenParm;
       startPos = 0;
       endPos = 0;
       Rtncode = 'X';
       Monitor;

          // Extract Company Number (1st Parameter)
          startPos = %Scan(',':inGenParm)+1;
          If startPos <= 1;
             LeaveSr;
          Endif;
          xxCO# = ExtractValue(startPos : inGenParm);

          // Extract Group Number (2nd Parameter)
          startPos = %Scan(',' : inGenParm : startPos) + 1;
          xxFND# = ExtractValue(startPos : inGenParm);

          // Extract Employer Number (3rd Parameter)
          startPos = %Scan(',' : inGenParm : startPos) + 1;
          xxEMP# = ExtractValue(startPos : inGenParm);

          // Extract Division (4th Parameter)
          startPos = %Scan(',' : inGenParm : startPos) + 1;
          xxDIV = ExtractValue(startPos : inGenParm);
          If xxDIV = *Blanks;
             xxDIV = '00000';
          Endif;

          // Extract Location (5th Parameter)
          startPos = %Scan(',' : inGenParm : startPos) + 1;
          xxLOC = ExtractValue(startPos : inGenParm);
          If xxLOC = *Blanks;
             xxLOC = '0000000000';
          Endif;

          Monitor;
            intCO# = %Int(%trim(xxCO#));
            intFND# = %Int(%trim(xxFND#));
            intEMP# = %Int(%trim(xxEMP#));
            intDIV = %Int(%trim(xxDIV));
            intLOC = %Int(%trim(xxLOC));
          On-Error;
             intCO# = 0;
             intFND# = 0;
             intEMP# = 0;
             intDIV = 0;
             intLOC = 0;
          EndMon;

          Rtncode = ' ';
       On-Error;
          Rtncode = 'X';
       EndMon;
       EndSr;
      /End-Free

     p*------------------------------------------------------
     p* Procedure name: ExtractValue
     p* Purpose:        Extract the Value of the Gen Params
     p* Returns:
     p*------------------------------------------------------
     pExtractValue     B
     dExtractValue     pi            20
     d inStart                        5  0 CONST
     d inGenParm                    512    CONST

     d outValue        s             20
      /Free
       outValue = *blanks;
       If inStart > 0;
         endPos = %Scan(',':inGenParm:inStart);
         If endPos > 0;
           len = endPos-inStart;
           If len > 0;
             outValue = %Trim(%Subst(inGenParm:inStart:len));
           Endif;
         Endif;
       Endif;
       Return outValue;
      /End-Free
     pExtractValue     E
