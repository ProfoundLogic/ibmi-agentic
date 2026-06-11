     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     H**********************************************************
     H*
     H* PROGRAM:     SRDIV
     H* DESCRIPTION: Build JSON string with # of divs on policy
     H* Programmer:  C:65
     H* Date:        03/12/13
     H* Project:     WC2116 - Policy Transform
     H*
     H**********************************************************
     H*  REVISIONS
     H*
     H* 05/06/14 C:279 Redmine: 2222
     H*                Modifed to have 3 entry parameters and
     H*                to receive employer detail RRN.
     H*
     H*
     H**********************************************************
      ***** beg add ******* 05/06/14 ***************************
     Fwdelp     if   e             disk    rename(pel01:pel0p)
      ***** end add ******* 05/06/14 ***************************
     Fwmemp     if   e             disk    rename(pem01:pem0p)
     Fwmeml     if   e           k disk    rename(pem01:pem00)

      /copy sPrDate
      /copy sPrAtrium
     D*
     d numEdtCde       c                   const('X')
     D xxCount         s              5  0 inz
     D xxRTCD          s              3    inz
     D*
      * *ENTRY
     DDivCount         pr                  ExtPgm('SRDIV')
     D                                9  0
      ******** beg del ****** 05/06/14 *************************
     D********                      512
      ******** end del ****** 05/06/14 *************************
     D                            32767
     D                                1
      *
     DDivCount         pi
     D inRRN                          9  0
      ******** beg del ****** 05/06/14 *************************
     D*inData***                    512
      ******** end del ****** 05/06/14 *************************
     D outJSON                    32767
     D rtnCode                        1
     D*
      /Free

       //***** beg chg ****** 05/06/14 *************************
       // Chain(n) inRRN PEM0P;
       Chain(n) inRRN PEL0P;
       //***** end chg ****** 05/06/14 *************************
       xxRTCD  = ' ';
       outJSON = ' ';

       xxCount = 0;
       //***** beg chg ****** 05/06/14 *************************
       // Setll (EMco#:EMfnd:EMemp#) pem00;
       // Reade (EMco#:EMfnd:EMemp#) pem00;

       Setll (ELco#:ELfnd:ELemp#) pem00;
       Reade (ELco#:ELfnd:ELemp#) pem00;
       //***** end chg ****** 05/06/14 *************************
       Dow not %eof(WMEML);
          xxCount = xxCount + 1;
          //***** beg chg ****** 05/06/14 *************************
          // Reade (EMco#:EMfnd:EMemp#) pem00;
          Reade (ELco#:ELfnd:ELemp#) pem00;
          //***** end chg ****** 05/06/14 *************************
       Enddo;

       BuildJSON(outJSON :'divCount' : %editc(xxCount:numEdtCde));
       rtnCode = ' ';
       Return;

      /End-free
     C*
