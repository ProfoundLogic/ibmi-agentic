     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Owner/Office detail box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLLMST
     h*  Calls to:
     h*
     h*  Created on 07/24/2012 by C:191 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h* 06/09/20  C:298 Project: SY420
     h*                 Mask SSN
     h*
     h**********************************************************************
     Fwmoop     if   e             disk    rename(poo01:poo0p)
     Fwmeml     if   e           k disk    rename(pem01:pem00)
     Fwmall     if   e           k disk    rename(pal01:pal00)
     Fwmprl     if   e           k disk    rename(ppr01:ppr00)
     Fwmprl1    if   e           k disk    rename(ppr01:ppr01)
     Fwdelp     if   e             disk    rename(pel01:pel0p)

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrString
      /copy wPrEmplr
      ***  Begin Add  ***  06/09/20  ****************************
      /copy sPrMask
      /copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
      * *ENTRY
     DOwnerOffDtls     pr                  ExtPgm('WTPOLLMS01')
     D                                9  0
     D                            32767
     D                                1
      *
     DOwnerOffDtls     pi
     D inRRN                          9  0
     D outJSON                    32767
     D RtnCode                        1
      *
     D numEdtCde       c                   const('N')
      *
     D AdL1            s             40    inz
     D AdL2            s             40    inz
     D AdL3            s             40    inz
     D ss#             s              9s 0 inz
     D xxELrrn         s              9  0 inz
      *
      /Free

       Chain(n) inRRN poo0p;
        // set key values

       outJSON = ' ';

       BuildJSON(outJSON :'DtlOutODOB' :%EditW(dtMDYY(OOdob):'  /  /    '));
       //***  Begin Chg  ***  06/09/20  ***********************
       //Monitor;
         //ss# = %Int(OOoss#);
       //On-Error;
         //ss# = *Zeros;
       //Endmon;
       //BuildJSON(outJSON :'DtlOutOSSN' :%Editw(ss#:'0   -  -    '));
       BuildJSON(outJSON :'DtlOutOSSN'
        :DisplaySS#(MaskData('SSN':'DtlOutOSSN':OOoss#)));
       //***  End   Chg  ***  06/09/20  ***********************

       PRdesc = *Blanks;
       ELrtcd = *Blanks;
       If OOclas <> 0;
         xxELrrn = GetEmpYrRR(0 :OOemp# :OOdiv :OOfnd :OOco# :' ');
         Chain(n) xxELrrn pel0p;
         If ELrtcd <> *Blanks;
           Chain(n) (OOco#:ELrtcd:OOclas) ppr01;
         Else;
           Chain(n) (OOco#:OOclas) ppr00;
         Endif;
         BuildJSON(outJSON :'DtlOutOClass' :
          %Editc(OOclas:'X') + ' - ' + %Trim(PRdesc));
       Else;
         BuildJSON(outJSON :'DtlOutOClass' : ' ');
       Endif;

        //Division address
       Chain (OOco#:OOfnd:OOemp#:OOdiv) pem00;
       FormatAddress(EMpad1:EMpad2:EMpcty:EMpst:EMpzip:AdL1:AdL2:AdL3);
       BuildJSON(outJSON :'DtlOutOPAdd1' :%Trim(AdL1));
       BuildJSON(outJSON :'DtlOutOPAdd2' :%Trim(AdL2));
       BuildJSON(outJSON :'DtlOutOPAdd3' :%Trim(AdL3));

       FormatAddress(EMmad1:EMmad2:EMmcty:EMmst:EMmzip:AdL1:AdL2:AdL3);
       BuildJSON(outJSON :'DtlOutOMAdd1' :%Trim(AdL1));
       BuildJSON(outJSON :'DtlOutOMAdd2' :%Trim(AdL2));
       BuildJSON(outJSON :'DtlOutOMAdd3' :%Trim(AdL3));

        //Entity address
       If OOelid <> *Blanks;
         Chain (OOco#:OOfnd:OOemp#:OOdiv:OOelid) pal00;
         FormatAddress(ALpad1:ALpad2:ALpcty:ALpst:ALpzip:AdL1:AdL2:AdL3);
         BuildJSON(outJSON :'DtlOutOPEAdd1' :%Trim(AdL1));
         BuildJSON(outJSON :'DtlOutOPEAdd2' :%Trim(AdL2));
         BuildJSON(outJSON :'DtlOutOPEAdd3' :%Trim(AdL3));

         FormatAddress(ALmad1:ALmad2:ALmcty:ALmst:ALmzip:AdL1:AdL2:AdL3);
         BuildJSON(outJSON :'DtlOutOMEAdd1' :%Trim(AdL1));
         BuildJSON(outJSON :'DtlOutOMEAdd2' :%Trim(AdL2));
         BuildJSON(outJSON :'DtlOutOMEAdd3' :%Trim(AdL3));
       Else;
         Adl1 = *Blanks;
         Adl2 = *Blanks;
         Adl3 = *Blanks;
         BuildJSON(outJSON :'DtlOutOPEAdd1' :%Trim(AdL1));
         BuildJSON(outJSON :'DtlOutOPEAdd2' :%Trim(AdL2));
         BuildJSON(outJSON :'DtlOutOPEAdd3' :%Trim(AdL3));
         BuildJSON(outJSON :'DtlOutOMEAdd1' :%Trim(AdL1));
         BuildJSON(outJSON :'DtlOutOMEAdd2' :%Trim(AdL2));
         BuildJSON(outJSON :'DtlOutOMEAdd3' :%Trim(AdL3));
       Endif;

       RtnCode = ' ';
       Return;
      /End-Free
