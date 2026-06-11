     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title S4411R - ISO Navigator XMLPREP Write Program
     h/define CONVERT
     h bnddir('STBNDDIR')
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     H*****************************************************************
     H/TITLE S4411R - ISO Navigator XMLPREP Write Program
     h*****************************************************************
     h* Revisions:                                                    *
     h*                                                               *
     h* 06/01/17 C: 180 Support: 47321                                *
     h*                 Concatenate time to XPXML.                    *
     h*                                                               *
     h* 09/13/16 C: 308 Project: 114WXY                               *
     h*                 Remove Docroot on folder path                 *
     h*                                                               *
     h* 08/18/16 C: 079 Project: 114WXY                               *
     h*                 Append docroot on folder path                 *
     h*                                                               *
     h*****************************************************************
     fSsisp     if   e           k disk    Rename(pis01:pis00)
     fXmlprel1  if a e           k disk

     dpsds           esds                  extname(szq1p)
      //*** Beg Add *** 08/18/16 **************************
       //*** Beg Del *** 09/13/16 ****************************
     D*xxRoot          s             10
       //*** End Del *** 09/13/16 ****************************
     D xxSufx          s              3
     D xxFlrP          s             80
      //*** End Add *** 08/18/16 **************************

     d/copy sPrDate
      * ISO Navigator XMLPREP Write Program
     d ISONavXMLWrt    pr                  Extpgm('S4411R')
     d inRunType                      1A
     d inXPXMLC                     100A
     d inXPXMLM                     100A

     d ISONavXMLWrt    pi
     d inRunType                      1A
     d inXPXMLC                     100A
     d inXPXMLM                     100A

       //*** Beg Add *** 08/18/16 ****************************
       //*** Beg Del *** 09/13/16 ****************************
      * Retrieve doc root and suffix
     d*DocRoot         pr                  ExtPgm('RTVDOCROOT')
     d*  xxRoot_                     10
       //*** End Del *** 09/13/16 ****************************

     d SA030R          pr                  ExtPgm('SA030R')
     d   xxSuffix                     3
     d   xxFlrp                      80
       //*** End Add *** 08/18/16 ****************************

      *** Main Processing ***
      /Free
       //*** Beg Add *** 08/18/16 ****************************
       //*** Beg Del *** 09/13/16 ****************************
       // Get Doc Root

        //DocRoot(xxRoot);
       //*** End Del *** 09/13/16 ****************************
          SA030R(xxSufx:xxFlrP);
          If xxFlrP <> ' ';
             xxFlrP = '/' + %trim(xxFlrP);
          Endif;
       //*** End Add *** 08/18/16 ****************************
       Read pis00;
       If inRunType = 'E';
       //**** Write First record to XMLPREP **//
       //*** Beg Chg *** 06/01/17 ****************************
       //***   XPxml = %trim(ISofil)+'_'+%char(%Int( %Char( %date():*ISO0)));
          XPxml = %trim(ISofil)+'_'+ %char(%date():*iso0) +
                  %char(%time():*iso0);
       //*** End Chg *** 06/01/17 ****************************
       //*** Beg Chg *** 08/18/16 ****************************
       //***** XPxmlf = ISofld;
       //*** Beg Chg *** 09/13/16 ****************************
        //XPxmlf = '/' + %trim(xxRoot) + %trim(xxFlrP) + '/' +
        //         %trim(ISofld);
          XPxmlf =  %trim(xxFlrP) + '/' + %trim(ISofld);
       //*** End Chg *** 09/13/16 ****************************

       //*** End Chg *** 08/18/16 ****************************
          XPmail = ISemad;
          XPtype = 'ISO Claim Export';
          inXPXMLC = XPxml;
          XPPgm  = Q1PGM;
          XPUser = Q1USER;
          XPTime = Timeto6(SystemTime());
          XPDt   = Dateto7(SystemDate());

          Write xp100;

       //**** Write second record to XMLPREP **//
       //*** Beg Chg *** 06/01/17 ***************************************
       //***   XPxml = %trim(ISmfil)+'_'+%char(%Int( %Char( %date():*ISO0)));
          XPxml = %trim(ISmfil)+'_'+%char(%date():*iso0) +
                  %char(%time():*iso0);
       //*** End Chg *** 06/01/17 ***************************************
       //*** Beg Chg *** 08/18/16 ****************************
       //***** XPxmlf = ISofld;
       //*** Beg Chg *** 09/13/16 ****************************
        //XPxmlf = '/' + %trim(xxRoot) + %trim(xxFlrP) + '/' +
        //         %trim(ISofld);
          XPxmlf = %trim(xxFlrP) + '/' + %trim(ISofld);
       //*** End Chg *** 09/13/16 ****************************

       //*** End Chg *** 08/18/16 ****************************
          XPmail = ISemad;
          XPtype = 'ISO Medical Export';
          inXPXMLM = XPxml;
          XPpgm  = Q1PGM;
          XPuser = Q1USER;
          XPtime = Timeto6(SystemTime());
          XPdt   = Dateto7(SystemDate());

          Write xp100;
       Endif;

       *inlr = *on;
      /End-free
