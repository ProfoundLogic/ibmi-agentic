     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve List of secured areas on display file
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: App toolbar
     h*  Calls to:
     h*
     h*  Created on 10/31/2012 by C:191 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h* 05/20/13 C:136 Project: SY307
     h*                Modify key used used to access SDASP from
     h*                CO#/FILE/RCDFMT/AREA to CO#/AREA
     h**********************************************************************
     fsmasl     if   e           k disk    rename(pas01:pas00)
     fsdasl     if   e           k disk    rename(pas01:pds00)
     f                                     prefix(d)

      /copy sPrAtrium
      /copy sprUsrPrms
      * *ENTRY
     DListSecAreas     pr                  ExtPgm('STSECAREAS')
     D                              512
     D                            32767
     D                                1
      *
     DListSecAreas     pi
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     D psds          esds                  extname(szq1p)

     D strDspf         s             10    inz
     D strFormat       s             10    inz
     D strFields       s          32767a   inz varying
     D intCo#          s              3  0 inz
      /Free

       outJSON = ' ';

       //Extract params from GenParm format; ddddddddddFFFFFFFFFF
       strDspf = %Trim(%Subst(inGenParm:1:10));
       strFormat = %Trim(%Subst(inGenParm:11:10));

       If strDspf = *Blanks;
         BuildJSON(outJSON :'RtnCode' :'Blank display file name');
         RtnCode = 'X';
         Return;
       Endif;
       If strFormat = *Blanks;
         BuildJSON(outJSON :'RtnCode' :'Blank format name');
         RtnCode = 'X';
         Return;
       Endif;

       intCo# = GetUserInfo(Q1user);
       outJSON = '"areas":';
       StartJSONarray(outJSON);
       //Process request
       Setll (intCo#:strDspf:strFormat) pas00;
       DoU %Eof(smasl);
         Reade (intCo#:strDspf:strFormat) pas00;
         If %Eof(smasl);
           Leave;
         Endif;

         StartJSONelemnt(outJSON);
         BuildJSON(outJSON :'areaid' :ASarea);
         outJSON = %Trim(outJSON) + ', "fields":';
         StartJSONarray(outJSON);
         strFields = *Blanks;

       //*** beg chg ***** 05/20/13 **********************************
       //*** Setll (ASco#:ASdspf:ASfrmt:ASarea) pds00;
       //*** DoU %Eof(sdasl);
       //***   Reade (ASco#:ASdspf:ASfrmt:ASarea) pds00;
         Setll (ASco#:ASarea) pds00;
         DoU %Eof(sdasl);
           Reade (ASco#:ASarea) pds00;
       //*** end chg ***** 05/20/13 **********************************
           If %Eof(sdasl);
             Leave;
           Endif;
           If strFields <> *Blanks;
             strFields = %Trim(strFields) + ', ';
           EndiF;
           strFields = %Trim(strFields) + '"' + %Trim(DASfld) +'"';
         Enddo;
         outJSON = %Trim(outJSON) + strFields;

         EndJSONarray(outJSON);
         EndJSONelemnt(outJSON);

       Enddo;
       EndJSONarray(outJSON);

       RtnCode = ' ';
       Return;
