     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H DFTACTGRP(*no) ACTGRP(*caller)
     H BNDDIR('WTBNDDIR':'YAJL')
    H*********************************************************************
    H/TITLE CIAGTCHG - Transfer Agency changes to CiCenter
    H*********************************************************************
    H* Program desription:                                               *
    H*  This program will gather information from the Agency and create  *
    H*   and XML script to be used by CiCenter.  This script will be     *
    H*   transfered to a data queue for the CiCenter to retrieve and     *
    H*   process.                                                        *
    H*                                                                   *
    H* Use the following from a command line to clear the data queue:    *
    H*  CALL PGM(QCLRDTAQ) PARM(CIAGTDTAQ *LIBL)                         *
    H* This should only be used during testing.                          *
    H*                                                                   *
    H* Created: 07/14/14   C:295   Project: CI103                        *
    H*********************************************************************
    H* Revisions:                                                        *
     H*                                                                   *
     H* 01/24/19  C:33  Project: SY406                                    *
     H*                 Include Portal with change information            *
    H*********************************************************************
     Fwmagp     if   e             disk    rename(pag01:pag0p)
     Fssylf     if   e           k disk    rename(ssy01:psy00)
     F                                     Ignore(spc01)
     F                                     Ignore(sfb01)
     F                                     Ignore(src01)
     F                                     Ignore(sdr01)
     F                                     Ignore(sbc01)
     F                                     Ignore(scs01)
     F                                     Ignore(sgl01)
     F                                     Ignore(sp201)

     D*entry
     DciAgtChg         pr                  extpgm('CIAGTCHG')
     D @@Rrn                          9  0
     D @@Type                        10
     D @@OFein                        9  0
     D @@OMod                         3  0
     D @@OName                      100

     DciAgtChg         pi
     D @@Rrn                          9  0
     D @@Type                        10
     D @@OFein                        9  0
     D @@OMod                         3  0
     D @@OName                      100

     D CheckLib        pr                  extpgm('SRCHKLIB')
     D ##Lib                         10    const
     D ##Type                         4    const
     D ##AppendSfx                    1    const
     D ##Flag                         1
     D*
     D Ceedod          pr
     D parmNum                       10I 0 const
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               12A   options(*omit)
     D*
     D ChgDate         pr           240
     D*
     D ChgUser         pr           240
     D*
     D DbaChgFtr       pr           240
     D*
     D DbaChgHdr       pr           540
     D*
     D GetClientID     pr                  extpgm('RTVCLTID')
     D ##cltid                        3
     D*
     D LoadDataQ       pr                  extpgm('QSNDDTAQ')
     D ##DataQ                       10    const
     D ##DataQLib                    10    const
     D ##DataQLen                     5  0 const
     D ##Data                      2820    const
     D*
     D NewName         pr           240
     D*
     D NewFEIN         pr           240
     D*
     D NewMod          pr           240
     D*
     D OldName         pr           240
     D*
     D OldFein         pr           240
     D*
     D OldMod          pr           240
     D*
     DRmvXMLExcptChar  pr         32766    opdesc
     D ##inField                  32766    options(*varsize) const
     D ##incSize                      5  0 options(*nopass)
     D*
     D StandardHdr     pr           120
     D*
      * Constants
     D #ofExcept       c                   5
     D***  Begin Add  ***  01/24/19  **************************************
     D CRLF            C                   x'0D25'
     D***  End   Add  ***  01/24/19  **************************************
      * StandAlones
     D excChar         s              1    dim(#ofExcept) ctdata perrcd(1)
     D replace         s             10    dim(#ofExcept) alt(excChar)
     D* parameters passed to Ceedod
     D descType        s             10I 0
     D dataType        s             10I 0
     D descInfo1       s             10I 0
     D descInfo2       s             10I 0
     D inLen           s             10I 0
     D*
     D CharupSize      s              5  0
     D Data            s           2820
     D***  Begin Add  ***  01/24/19  **************************************
     D isUseCiCenter   s              1n   inz(*off)
     D***  End   Add  ***  01/24/19  **************************************
     D isProcess       s              1n   inz(*on)
     D***  Begin Add  ***  01/24/19  **************************************
     D isUsePortal     s              1n   inz(*off)
     D***  End   Add  ***  01/24/19  **************************************
     D isquoteEnv      s              1n   inz(*off)
     D Chardtaq        s                   Like(SYfl14)
     D***  Begin Add  ***  01/24/19  **************************************
     D strJson         s           5000a   varying
     D***  End   Add  ***  01/24/19  **************************************
     D ##cltid         s              3    inz('000')

     D***  Begin Add  ***  01/24/19  **************************************
     D/copy sPrDate
     D/copy sPrJson
     D/copy sPrModule

     D/Include yajl_h
     D***  End   Add  ***  01/24/19  **************************************

      /Free
       // Main Line

       // Get client id
       GetClientID(##cltid);
       //***  Begin Chg  ***  01/24/19  ***********************************
       //****Clear WCfl19;

       //*If*CiCenter is not installed, don't process
       //****Chain(n) 'WC' swc01;
       //****If WCfl19 <> 'Y';
       isUseCiCenter = IsModEnabled('CiCenter');
       isUsePortal = IsModEnabled('Portal Module');

       If not isUseCiCenter and
        not isUsePortal;
       //***  End   Chg  ***  01/24/19  ***********************************
          isProcess = *off;
          Exsr PgmEnd;
       Else;
          // Retrieve data queue name
          Chain(n) 'SY' psy00;
             If %Found;
                Chardtaq = SYfl14;
             Endif;

          Chain(n) @@RRN pag0p;
          If not %found;
             Exsr PgmEnd;
          Endif;

          Select ;
          When @@Type = 'FEIN';
          Exsr CrtFeinXML;

          When @@Type = 'DBA';
          Exsr CrtDbaXML ;
          Endsl;

       //***  Begin Add  ***  01/24/19  ********************************
          Exsr CreateJson;
       //***  End   Add  ***  01/24/19  ********************************

       //***  Begin Chg  ***  01/24/19  ********************************
       //*LoadDataQ (Chardtaq :'*LIBL':%len(%trim(Data)):Data);
          If isUseCiCenter;
             LoadDataQ (Chardtaq :'*LIBL':%len(%trim(Data)):Data);
          Endif;
       //***  End   Chg  ***  01/24/19  ***********************************

       Endif;
       *inlr = *on;
       //==================================================================
       //  Program End
       //==================================================================
       BegSr PgmEnd;
          *inLr = *on;
          Return;
       EndSr;

       //***  Begin Add  ***  01/24/19  ********************************
       //==================================================================
       // Create the Json for the changes
       //==================================================================
       Begsr CreateJson;
        //strJson += 'Content-Type: text/plain' + CRLF + CRLF;
        strJson += '{"agentChange":{' +
         '"name":{' +
          BuildJsonNode('old':%trim(@@OName)) + ',' +
          BuildJsonNode('new':%trim(AGname)) +
          '},' +
         '"fein":{' +
          BuildJsonNode('old':%editc(@@OFein:'X')) + ',' +
          BuildJsonNode('new':%editc(AGfein:'X')) +
         '},' +
         '"modifier":{' +
          BuildJsonNode('old':%editc(@@OMod:'X')) + ',' +
          BuildJsonNode('new':%editc(AGmod:'X')) +
         '},' +
         '"changeInfo":{' +
          BuildJsonNode('user':%trim(AGuser)) + ',' +
          BuildJsonNode('date':%char(DtMDYY(AGdt)):'date') +
         '}' +
         '}}';
       Endsr;
       //***  End   Add  ***  01/24/19  ********************************

       //==================================================================
       // Create the XML for Agent DBA name changes
       //==================================================================
       BegSr CrtDbaXML;

       //This first 'data' is used for debugging.  Using the DSPDTAQ command
       // each line will appear evenly in the data queue.  Size of each
       // field used is divisable by 60 for this reason.

         data = StandardHdr +
         DbaChgHdr +
         OldName +
         OldFEIN +
         NewName +
         NewFEIN +
         ChgUser +
         ChgDate +
         OldMOD  +
         NewMOD  +
         DbaChgFtr;

         data = %trim(StandardHdr) + ' ' +
         %trim(DbaChgHdr) + ' ' +
         %trim(OldName) + ' ' +
         %trim(OldFein) + ' ' +
         %trim(NewName) + ' ' +
         %trim(NewFein) + ' ' +
         %trim(ChgUser) + ' ' +
         %trim(ChgDate) + ' ' +
         %trim(OldMod ) + ' ' +
         %trim(NewMod ) + ' ' +
         %trim(DbaChgFtr) + ' ' ;
       Endsr;

       //====================================================================
       // Create the XML for Agent FEIN changes
       //====================================================================
       begsr CrtFeinXML;
       // The FEIN and DBA XML scripts are currently the same, call DBA script
       exsr CrtDbaXML;
       endsr;
      /end-free
      //====================================================================
     P*====================================================================
     P* Create Requsting Date XML
     P*====================================================================
     P ChgDate         b
     D ChgDate         pi           240

     D data            s            240
     D date            s               d   datfmt(*iso)
     D intDate         s              8  0
     C     *cymd         test(d)                 AGdt                   99
     C  n99*cymd         move      AGdt          date
     C                   eval      data = '<requestedDate>'
     C                                  + %char(date)
     C                                  + '</requestedDate>'
     C                   return    data
     PChgDate          e
     P*====================================================================
     P* Create Requsting User XML
     P*====================================================================
     PChgUser          b
     D ChgUser         pi           240

     D Data            s            240

      /free
       Data = '<requestedBy>'
        + %trim(RmvXMLExcptChar(AGUSER))
        + '</requestedBy>';
       return Data;
      /end-free

     PChgUser          e
     P*====================================================================
     PDbaChgHdr        b
     D DbaChgHdr       pi           540

     D data            s            540

     D xsi             C                   'http://www.w3.org/2001-
     D                                     /XMLSchema-instance'
     D xmlns           C                   'http://www.stoneriver.com/comp-
     D                                     suite/namespace'
     D*schemaLocation  C                   'http://www.stoneriver.com/comp-
     D*                                    suite/namespaceCompanyName-
     D*                                    FeinChage.xsd'
     D schemaLocation  C                   'CompSuiteCompanyChangeRequest.xsd'
      /free
        data =
          '<CompSuiteCompanyChangeRequest '
             //+ ' xmlns:xsi="' + xsi + '"'
            // + ' xsi:"' + xsi + '"'
             + ' xsi:noNamespaceSchemaLocation="' +
               schemaLocation + '"' +  ' xmlns:xsi="' + xsi + '"' + '> '
             + '<AgencyChangeRequest>' ;
        return data;
      /end-free

     PDbaChgHdr        e
     P*====================================================================
     PDbaChgFtr        b
     D DbaChgFtr       pi           240

     D data            s            240
      /free
         data = '</AgencyChangeRequest> '
         + '</CompSuiteCompanyChangeRequest>';
          return data;
      /end-free
     PDbaChgFtr        e
     P*====================================================================
     P* Create New Name XML header
     P*====================================================================
     PNewName          b
     D NewName         pi           240

     D data            s            240
      /free
       data = '<newCompanyName>'
        + %trim(%subst(RmvXMLExcptChar(AGNAME:CharupSize)
                       :1:%size(AGNAME)+CharupSize))
        + '</newCompanyName>';
       return data;
      /end-free

     PNewName          e
     P*====================================================================
     P* Create New Fein XML header
     P*====================================================================
     PNewFein          b
     D NewFein         pi           240

     D data            s            240
      /free
       data = '<newFEIN>'
        + %trim(RmvXMLExcptChar(%editc(AGFEIN : 'X')))
        + '</newFEIN>';
       return data;
      /end-free

     PNewFein          e
     P*====================================================================
     P* Create New Mod XML header
     P*====================================================================
     PNewMod           b
     D NewMod          pi           240

     D data            s            240
      /free
       data = '<newModifier>'
        + %trim(RmvXMLExcptChar(%editc(AGMOD : 'X')))
        + '</newModifier>';
       return data;
      /end-free
     PNewMOD           e
     P*====================================================================
      * Create old Name XML header
     P*====================================================================
     POldName          b
     D OldName         pi           240

     D data            s            240
     D name            s                   like(@@OName)
      /free
       name = @@OName;
       data = '<oldCompanyName>'
        + %trim(RmvXMLExcptChar(@@OName) + ' ')
        + '</oldCompanyName>';
       return data;
      /end-free

     POldName          e
     P*====================================================================
     P* Create old FEIN XML header
     P*====================================================================
     POldFein          b
     D OldFein         pi           240

     D data            s            240
      /free
       data = '<oldFEIN>'
        + %trim(RmvXMLExcptChar(%editc(@@OFEIN : 'X')))
        + '</oldFEIN>';
       return data;
      /end-free
     POldFEIN          e

     P*====================================================================
     P* Create old Mod XML header
     P*====================================================================
     POldMod           b
     D OldMod          pi           240

     D data            s            240
      /free
       data = '<oldModifier>'
        + %trim(RmvXMLExcptChar(%editc(@@OMod : 'X')))
        + '</oldModifier>';
       return data;
      /end-free
     POldMOD           e

     P*====================================================================
     P* Remove XML Exception Characters
     P*====================================================================
     PRmvXMLExcptChar  b
     DRmvXMLExcptChar  pi         32766    opdesc
     D ##inField                  32766    options(*varsize) const
     D ##incSize                      5  0 options(*nopass)

     D xxIncSize       s                   like(##incSize) inz
     D f1              s              3  0
     D pos             s              5  0
     D returnFld       s                   like(##inField)

      /free
       callp Ceedod(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       returnFld = %subst(##inField:1:inLen);
       for f1 = 1 to #ofExcept;
          pos = 0;
          dou pos = 0;
             pos = %scan(excChar(f1):returnFld:pos+1);
             if pos <> 0;
                returnFld = %replace(%trim(replace(f1)):returnFld:pos:1);
                xxIncSize += %len(%trim(replace(f1))) - 1;
             endif;
          enddo;
       endfor;
       if %parms >= 2;
          ##IncSize = xxIncSize;
       endif;
       return returnFld;
      /end-free

     PRmvXMLExcptChar  e
     P*====================================================================
     PStandardHdr      b
     D StandardHdr     pi           120

     D data            s            120
      /Free
        data = '<?xml version="1.0"'
             + ' encoding="UTF-8"?>';
        Return data;
      /End-free
     PStandardHdr      e
** Exception Characters & Replacements
&&amp;                    <<<==== MUST BE FIRST
<&lt;
>&gt;
'&apos;
"&quot;
