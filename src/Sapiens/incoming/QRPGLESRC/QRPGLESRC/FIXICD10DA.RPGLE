     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('STBNDDIR')
     H*
     H****************************************************************
     H*  FIXICD10DA - Update ICDDTA10P data area
     H*
     H*  Description: This pgm will update ICDDTA10P data area
     H*
     H*  Created    : 04/28/2015  Programmer: 212  Project: MC377
     H*
     H*  Called By  :
     H*  Calls To   :
     H*
     H****************************************************************
     H*  Revisions:
     H****************************************************************
    F*
    D*
     d** Procedure definitions
     d*
     d**Entry
     d FIXICD10DA      pr                  ExtPgm('FIXICD10DA')

     d FIXICD10DA      pi

     d CheckObj        pr                  extPgm('SRCHKOBJ')
     d outObject                     10    const
     d outObjectType                  7    const
     d inError                        1

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3
     D*
     d* data structures
     d*
     d psds          esds                  extname(szq1p)
     d*
     d ICD10DTA        ds            10    dtaara(ICD10DTA)
     d*
     d** Standalone variables
     d*
     d clientID        s              3    inz('000')
     d isLibNtExt      s              1n   inz
     d strHldLib       s             10    inz
     d xxPdninst       s               n   inz(*off)
     C*
     c** Copybook declarations
     c*
     c/copy sPrPrprty
     c*
     c/free

       // get client ID
       GetClientID(clientID);

       // get instance library name
       strHldLib = SysProperty('InstanceName');

       isLibNtExt = *off;
       If strHldLib <> *blanks;
          // verify if library exist
          CheckObj(strHldLib:'*LIB':isLibNtExt);
       Endif;

       // check if QA env
       If isLibNtExt or strHldLib = ' ';
          isLibNtExt = *off;
          strHldLib = 'PF' + clientID + 'QA';

          // verify if library exist
          CheckObj(strHldLib:'*LIB':isLibNtExt);

          // check if PDN env
          If isLibNtExt;
             isLibNtExt = *off;
             strHldLib = 'PF' + clientID + 'PN';

             // verify if library exist
             CheckObj(strHldLib:'*LIB':isLibNtExt);

             If isLibNtExt;
                strHldLib = ' ';
             Endif;
          Endif;
       Endif;

       If strHldLib = ' ';
          Return;
       Endif;

       If %subst(strHldLib:6:2) = 'PN';
          xxPDNinst = *on;
       Else;
          xxPDNinst = *off;
       Endif;

       // get data area
       IN *Lock ICD10DTA;

       If xxPDNinst = *on;
         ICD10DTA = '10012015';            // ICD-10 eff. date
       Else;
         ICD10DTA = %char(%date():*usa0);   // system date
       Endif;

       // update data area
       OUT ICD10DTA;

       *inlr = *on;

      /end-free
